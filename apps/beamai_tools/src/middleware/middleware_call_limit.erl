%%%-------------------------------------------------------------------
%%% @doc 调用限制中间件（around 模型）
%%%
%%% 限制模型调用次数：around_chat 检查并递增模型调用计数。
%%% 超限时根据 on_limit_exceeded 配置决定中止（halt）或警告继续（warn_and_continue）。
%%%
%%% == 工具调用限额请用 agent 的 max_tool_calls，不在这里 ==
%%%
%%% 本中间件曾提供 `max_tool_calls` / `max_tool_calls_per_turn'（走 around_tool），
%%% 但它们**从未生效**，且在 filter 层不可能生效——已删除。
%%%
%%% 原因是执行模型而非实现疏漏：批内每个工具拿到的 context 是**同一份只读快照**
%%% （见 beamai_agent_utils:execute_sequential/4，以及上游
%%% design/context_split_parallel_tools.md §4.1「快照 + 屏障折叠，一个 API 只有
%%% 一种状态语义，不引入批内穿线」）。于是一批并发 5 个工具各自读到同样的计数、
%%% 各自算出 count+1，折叠完计数只前进 1——filter 里的计数器天生累加不起来。
%%% 让 invoke_tool 回传 context 也救不了这一点。
%%%
%%% 正确的位置是 agent 的 tool loop：那一层是串行的，
%%% `length(ToolCallsMade)' 无歧义。已在上游实现：
%%%
%%% ```erlang
%%% beamai_agent:new(#{
%%%     llm => LlmConfig,
%%%     max_tool_calls => 30,        %% 整轮 run 的工具调用总数上限（缺省 infinity）
%%%     max_tool_iterations => 10    %% 正交：限"来回几轮"
%%% })
%%% '''
%%%
%%% 超限返回 `{error, {max_tool_calls, CallsMade}}'，与既有的
%%% `{error, {max_tool_iterations, _}}' 对称。
%%%
%%% 另注：`max_iterations` 与 `max_model_calls` 在 around_chat 里是同步递增的，
%%% 两者实为同一个计数，谁小谁先触发。真正的"迭代"应在 around_turn 计，但 turn
%%% 链的响应是 tuple 而非带 context 的 map，filter 状态回写路径不通
%%% （见 beamai_middleware_runner:wrap_hook/2 的说明）。要限迭代请用 agent 的
%%% `max_tool_iterations'。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_call_limit).

-behaviour(beamai_middleware).

-export([init/1, around_chat/3]).

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    #{
        max_model_calls => maps:get(max_model_calls, Opts, 20),
        max_iterations => maps:get(max_iterations, Opts, 15),
        on_limit_exceeded => maps:get(on_limit_exceeded, Opts, halt),
        model_call_count => 0,
        iteration_count => 0
    }.

%% @doc 包裹 LLM 调用：前置检查模型调用/迭代限制，后置递增计数器。
-spec around_chat(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_chat(Req, State, Next) ->
    #{max_model_calls := MaxCalls,
      max_iterations := MaxIters,
      model_call_count := ModelCount,
      iteration_count := IterCount,
      on_limit_exceeded := Action} = State,

    case check_limits([{model_calls, ModelCount, MaxCalls},
                       {iterations, IterCount, MaxIters}]) of
        ok ->
            Resp = Next(Req),
            NewState = State#{model_call_count => ModelCount + 1,
                              iteration_count => IterCount + 1},
            {Resp, NewState};
        {exceeded, Type, Count, Max} ->
            case Action of
                halt ->
                    halt_with(Type, Count, Max);
                warn_and_continue ->
                    logger:warning("~s", [format_limit_message(Type, Count, Max)]),
                    Resp = Next(Req),
                    NewState = State#{model_call_count => ModelCount + 1,
                                      iteration_count => IterCount + 1},
                    {Resp, NewState}
            end
    end.

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

%% @private halt：抛出，让 beamai_filter_chain:run/4 在最外层收敛成 {error, Reason}。
%%
%% 必须 throw，不能返回 `#{response => Error}' / `#{result => Error}'——
%% kernel 对这两条链的成功路径是 `{ok, #{response := R}} -> {ok, R, Ctx}' 和
%% `{ok, #{result := V}} -> {ok, V, W}'，返回错误值等于**报告成功**：
%% 实测 halt 时 invoke_chat 返回 `{ok, {limit_exceeded, ...}, Ctx}'，agent 拿它
%% 当 LLM 响应用，content 解出 null、tool_calls 解出 []——于是"halt"变成了
%% 静默返回一个空答案，限额信息谁也看不到，更没有任何东西被 halt。
%%
%% throw 之后：chat 链 -> invoke_chat 返回 {error, {limit_exceeded, _}}，
%% 整轮带原因失败（这才是 halt）；tool 链 -> invoke_tool 返回 {error, _}，
%% 该次工具调用带原因失败并归一成错误结果回灌模型（工具 filter 停不了整个循环，
%% 这是能做到的最好结果）。
halt_with(Type, Count, Max) ->
    throw({limit_exceeded, #{
        type => Type, count => Count, max => Max,
        message => format_limit_message(Type, Count, Max)
    }}).

check_limits([]) -> ok;
check_limits([{Type, Count, Max} | Rest]) ->
    case Count >= Max of
        true -> {exceeded, Type, Count, Max};
        false -> check_limits(Rest)
    end.

format_limit_message(model_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("Model call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(iterations, Count, Max) ->
    iolist_to_binary(io_lib:format("Iteration limit exceeded (~p/~p)", [Count, Max])).
