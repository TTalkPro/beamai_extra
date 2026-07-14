%%%-------------------------------------------------------------------
%%% @doc 人工审批中间件（around 模型）
%%%
%%% 在工具执行前暂停并等待人工确认。
%%% around_tool 包裹工具执行：前置检查是否需审批 → 获批则调 Next → 拒绝则短路。
%%%
%%% 支持模式：all（全部需审批）/ selective（选择性）/ custom（自定义）/ none（无需审批）
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
        mode => maps:get(mode, Opts, none),
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
    ToolName = maps:get(tool, Req, <<"unknown">>),
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
                    %% 审批拒绝，短路返回错误
                    Ctx = maps:get(context, Req),
                    Resp = #{result => {error, {approval_rejected, rejected_by_user}}, context => Ctx},
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

call_with_timeout(Handler, Request, Timeout, TimeoutAction) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() -> Parent ! {Ref, Handler(Request, #{})} end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        TimeoutAction
    end.
