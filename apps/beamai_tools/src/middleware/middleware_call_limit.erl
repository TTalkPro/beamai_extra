%%%-------------------------------------------------------------------
%%% @doc 调用限制中间件（around 模型）
%%%
%%% 在执行过程中限制各类调用次数：
%%% - around_chat: 检查并递增模型调用计数
%%% - around_tool: 检查并递增工具调用计数
%%%
%%% 超限时根据 on_limit_exceeded 配置决定中止（halt）或警告继续（warn_and_continue）。
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
                    Error = {limit_exceeded, #{
                        type => Type, count => Count, max => Max,
                        message => format_limit_message(Type, Count, Max)
                    }},
                    Ctx = maps:get(context, Req),
                    #{response => Error, context => Ctx};
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
                    Error = {limit_exceeded, #{
                        type => Type, count => Count, max => Max,
                        message => format_limit_message(Type, Count, Max)
                    }},
                    Ctx = maps:get(context, Req),
                    #{result => Error, context => Ctx};
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
