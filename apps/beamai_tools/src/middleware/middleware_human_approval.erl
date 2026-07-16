%%%-------------------------------------------------------------------
%%% @doc 人工审批中间件（around 模型）
%%%
%%% 在工具执行前暂停并等待人工确认。
%%% around_tool 包裹工具执行：前置检查是否需审批 → 获批则调 Next → 拒绝则短路。
%%%
%%% 支持模式：all（全部需审批）/ selective（选择性）/ custom（自定义）/ none（无需审批）
%%%
%%% **缺省 mode 为 all（fail-closed）**，与 beamai_middleware_presets:human_approval/1
%%% 一致。审批网关若在未配置时默认放行，等于静默失效——加了中间件却毫无审批，
%%% 且不报错。所以未配置时宁可全部拦下（无 approval_handler 时按 reject 处理），
%%% 现象明显而非静默。真不需要审批就别挂这个中间件，或显式传 mode => none。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_human_approval).

-behaviour(beamai_middleware).

-export([init/1, around_tool/3]).
-export([requires_approval/3]).

-export_type([approval_mode/0]).

%%====================================================================
%% 类型定义
%%%-------------------------------------------------------------------

-type approval_mode() :: all | selective | custom | none.

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    #{
        mode => maps:get(mode, Opts, all),
        tools_requiring_approval => maps:get(tools_requiring_approval, Opts, []),
        approval_fn => maps:get(approval_fn, Opts, undefined),
        approval_handler => maps:get(approval_handler, Opts, undefined),
        timeout => maps:get(timeout, Opts, 60000),
        timeout_action => maps:get(timeout_action, Opts, reject)
    }.

%% @doc 包裹工具执行：前置检查审批，获批才调 Next。
-spec around_tool(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_tool(Req, #{mode := none} = State, Next) ->
    %% 无需审批，直接调用
    Resp = Next(Req),
    {Resp, State};
around_tool(Req, State, Next) ->
    ToolName = tool_name(Req),
    Args = maps:get(args, Req, #{}),

    case requires_approval([ToolName], Req, State) of
        false ->
            Resp = Next(Req),
            {Resp, State};
        true ->
            case handle_approval(ToolName, Args, State) of
                confirm ->
                    Resp = Next(Req),
                    {Resp, State};
                reject ->
                    %% 审批拒绝：短路返回一个**已编码的** JSON 结果回灌模型。
                    %%
                    %% 与上游 beamai_filters:approval_filter 保持一致：人拒绝不是
                    %% 系统故障，而是一个正当结果，让模型看见并改道即可，不该走错误通道。
                    %%
                    %% 关键是必须自己 encode。以前直接把裸元组
                    %% `{error, {approval_rejected, rejected_by_user}}' 塞进 result，
                    %% kernel 的 `{ok, #{result := V}} -> {ok, V, W}' 把它当成功值透出，
                    %% 最后由 beamai_agent_utils 的 encode_result 兜底成
                    %% `io_lib:format("~p")' —— 模型收到的是一串 Erlang 项式
                    %% （`<<"..">>'、`#{k => v}'），不是它认识的 JSON。
                    Ctx = maps:get(context, Req),
                    Rejection = beamai_tool:encode_result(
                        #{error => #{type => approval_rejected,
                                     message => <<"已拒绝执行（未获人工批准）"/utf8>>,
                                     tool => ToolName}}),
                    Resp = #{result => Rejection, writes => #{}, context => Ctx},
                    {Resp, State}
            end
    end.

%%====================================================================
%% 公共辅助函数
%%%-------------------------------------------------------------------

-spec requires_approval([binary()], map(), map()) -> boolean().
requires_approval(_Names, _Ctx, #{mode := none}) -> false;
requires_approval(_Names, _Ctx, #{mode := all}) -> true;
requires_approval(Names, _Ctx, #{mode := selective, tools_requiring_approval := RequiredTools}) ->
    lists:any(fun(Name) -> lists:member(Name, RequiredTools) end, Names);
requires_approval(Names, Ctx, #{mode := custom, approval_fn := ApprovalFn}) when is_function(ApprovalFn, 2) ->
    try ApprovalFn(Names, Ctx) catch _:_ -> false end;
requires_approval(_, _, _) -> false.

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

%% @private around_tool 的 Req 里 tool 是 tool_spec（map），不是名字。
%% 早先直接拿 maps:get(tool, Req) 当名字用，导致 selective 模式
%% `lists:member(ToolSpecMap, [<<"names">>])` 永远为 false（审批从不触发），
%% 且 approval_handler 收到的是整个 spec map。
tool_name(#{tool := ToolSpec}) when is_map(ToolSpec) ->
    beamai_tool:get_name(ToolSpec);
tool_name(#{tool := Name}) -> Name;
tool_name(_) -> <<"unknown">>.

handle_approval(ToolName, Args, State) ->
    #{approval_handler := Handler, timeout := Timeout, timeout_action := TimeoutAction} = State,

    ApprovalRequest = #{
        type => tool_approval,
        function => ToolName,
        args => Args,
        timestamp => erlang:system_time(millisecond)
    },

    case Handler of
        undefined ->
            %% 未配置处理器，默认拒绝
            reject;
        Handler when is_function(Handler, 2) ->
            try
                case Timeout of
                    0 -> Handler(ApprovalRequest, #{});
                    _ -> call_with_timeout(Handler, ApprovalRequest, Timeout, TimeoutAction)
                end
            catch
                _:Reason ->
                    logger:warning("Approval handler error: ~p, using timeout action: ~p", [Reason, TimeoutAction]),
                    TimeoutAction
            end
    end.

%% @private 在独立进程里跑审批处理器，带超时。
%%
%% 必须 spawn_monitor：处理器在**另一个进程**里崩溃时不会发消息回来，
%% 只 spawn 的话这里会干等满 Timeout（缺省 60s）才返回 timeout_action，
%% 而 handle_approval/3 的 try/catch 根本捕不到跨进程的崩溃。
%% 收到 'DOWN' 立刻按 timeout_action 收场。
call_with_timeout(Handler, Request, Timeout, TimeoutAction) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() -> Parent ! {Ref, Handler(Request, #{})} end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Reason} ->
            logger:warning("Approval handler crashed: ~p, using timeout action: ~p",
                           [Reason, TimeoutAction]),
            TimeoutAction
    after Timeout ->
        exit(Pid, kill),
        erlang:demonitor(MRef, [flush]),
        TimeoutAction
    end.
