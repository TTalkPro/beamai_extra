%%%-------------------------------------------------------------------
%%% @doc 调用限制中间件（around 模型）
%%%
%%% 在执行过程中限制各类调用次数：
%%% - around_chat: 检查并递增模型调用计数
%%% - around_tool: 检查并递增工具调用计数
%%%
%%% 超限时根据 on_limit_exceeded 配置决定中止（halt）或警告继续（warn_and_continue）。
%%%
%%% == 已知限制：工具侧计数无法跨调用累加（上游架构所致，本仓库修不了）==
%%%
%%% filter 私有状态存在 context 里，进出各一次；但 beamai_kernel:invoke_tool/4
%%% 返回的是 `{ok, Value, Writes}`——**不回传 context**（上游明确设计：context 是
%%% 每次工具调用的只读环境快照，见 beamai_agent_utils:run_one_tool/3）。
%%% 于是 around_tool 递增的 tool_call_count / current_turn_tool_calls 在本次调用
%%% 结束后即被丢弃，下次调用又从 init 值起算。实测：max_tool_calls => 2 时连调
%%% 5 次全部放行。
%%%
%%% 结论：`max_tool_calls`、`max_tool_calls_per_turn` 目前**不生效**。
%%% around_chat 侧不同——invoke_chat 返回 `{ok, Resp, Context}` 带 context，
%%% 调用方回穿即可累加，故 `max_model_calls` / `max_iterations` 可用。
%%%
%%% 想真正修好需要上游改动（invoke_tool 回传 context，或给 filter 一个显式的
%%% 跨调用状态通道）；在本仓库内绕过只能用 ETS/counters 之类的可变副信道，
%%% 那会把"限额作用域"从 run 级悄悄变成 kernel 生命周期级，语义更糟。
%%%
%%% 另注：`max_iterations` 与 `max_model_calls` 在 around_chat 里是同步递增的，
%%% 两者实为同一个计数，谁小谁先触发。真正的"迭代"应在 around_turn 计，但 turn
%%% 链的响应是 tuple 而非带 context 的 map，filter 状态回写路径不通
%%% （见 beamai_middleware_runner:wrap_hook/2 的说明）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_call_limit).

-behaviour(beamai_middleware).

-export([init/1, around_chat/3, around_tool/3]).

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    #{
        max_model_calls => maps:get(max_model_calls, Opts, 20),
        max_tool_calls => maps:get(max_tool_calls, Opts, 50),
        max_tool_calls_per_turn => maps:get(max_tool_calls_per_turn, Opts, 10),
        max_iterations => maps:get(max_iterations, Opts, 15),
        on_limit_exceeded => maps:get(on_limit_exceeded, Opts, halt),
        model_call_count => 0,
        tool_call_count => 0,
        iteration_count => 0,
        current_turn_tool_calls => 0
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

%% @doc 包裹工具调用：前置检查工具调用限制，后置递增计数器。
-spec around_tool(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_tool(Req, State, Next) ->
    #{max_tool_calls := MaxToolCalls,
      max_tool_calls_per_turn := MaxPerTurn,
      tool_call_count := TotalCount,
      current_turn_tool_calls := TurnCount,
      on_limit_exceeded := Action} = State,

    case check_limits([{tool_calls_per_turn, TurnCount, MaxPerTurn},
                       {tool_calls, TotalCount, MaxToolCalls}]) of
        ok ->
            Resp = Next(Req),
            NewState = State#{tool_call_count => TotalCount + 1,
                              current_turn_tool_calls => TurnCount + 1},
            {Resp, NewState};
        {exceeded, Type, Count, Max} ->
            case Action of
                halt ->
                    halt_with(Type, Count, Max);
                warn_and_continue ->
                    logger:warning("~s", [format_limit_message(Type, Count, Max)]),
                    Resp = Next(Req),
                    NewState = State#{tool_call_count => TotalCount + 1,
                                      current_turn_tool_calls => TurnCount + 1},
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
format_limit_message(tool_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("Tool call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(tool_calls_per_turn, Count, Max) ->
    iolist_to_binary(io_lib:format("Per-turn tool call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(iterations, Count, Max) ->
    iolist_to_binary(io_lib:format("Iteration limit exceeded (~p/~p)", [Count, Max])).
