%%%-------------------------------------------------------------------
%%% @doc 中间件行为定义（around 模型，与 beamai_filter 对齐）
%%%
%%% 每个中间件回调是一个 around 闭包，同时承担「前置 → 调内层 → 后置」：
%%% - `around_chat/3`：包裹一次 LLM 调用
%%% - `around_tool/3`：包裹一次工具执行
%%% - `around_turn/3`：包裹整个工具循环
%%%
%%% 签名与 beamai_filter 的 around_fun 完全对齐：
%%%   `(Request, State, Next) -> Response | {Response, NewState}`
%%% - 前置：检查/改写 Request
%%% - 调内层：`Next(Request1)` 拿 Response（不调即短路，多调即重试）
%%% - 后置：检查/改写 Response
%%% - 返回 Response（状态不变）或 {Response, NewState}（更新状态）
%%%
%%% Request / Response：
%%% - chat：Request `#{messages, context, opts}` → Response `#{response, context}`
%%% - tool：Request `#{tool, args, context}`     → Response `#{result, context}`
%%% - turn：Request `#{messages, context, resume}` → Response = 工具循环结果 tuple
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% -module(my_middleware).
%%% -behaviour(beamai_middleware).
%%% -export([init/1, around_chat/3]).
%%%
%%% init(Opts) ->
%%%     #{max_calls => maps:get(max_calls, Opts, 10), count => 0}.
%%%
%%% around_chat(Req, #{max_calls := Max, count := Count} = State, Next) ->
%%%     case Count >= Max of
%%%         true ->
%%%             %% 短路：不调 Next，直接返回错误响应
%%%             #{response => #{error => limit_exceeded},
%%%               context => maps:get(context, Req)};
%%%         false ->
%%%             Resp = Next(Req),
%%%             {Resp, State#{count => Count + 1}}
%%%     end.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware).

-export_type([
    middleware/0,
    middleware_state/0,
    hook_name/0,
    around_fun/0,
    request/0,
    response/0,
    next/0
]).

%%====================================================================
%% 类型定义
%%%-------------------------------------------------------------------

-type middleware() :: #{
    module := module(),
    state := middleware_state(),
    priority => integer()
}.

-type middleware_state() :: map().

-type hook_name() :: around_chat | around_tool | around_turn.

-type request() :: map().
-type response() :: map() | tuple().
-type next() :: fun((request()) -> response()).
-type around_fun() :: fun((request(), middleware_state(), next()) ->
    response() | {response(), middleware_state()}).

%%====================================================================
%% 回调函数定义
%%%-------------------------------------------------------------------

%% @doc 初始化中间件状态。
-callback init(Opts :: map()) -> middleware_state().

%% @doc 包裹一次 LLM 调用（chat 链）。
%% Request: `#{messages, context, opts}` → Response: `#{response, context}`
-callback around_chat(Request :: request(), State :: middleware_state(), Next :: next()) ->
    response() | {response(), middleware_state()}.

%% @doc 包裹一次工具执行（tool 链）。
%% Request: `#{tool, args, context}` → Response: `#{result, context}`
-callback around_tool(Request :: request(), State :: middleware_state(), Next :: next()) ->
    response() | {response(), middleware_state()}.

%% @doc 包裹整个工具循环（turn 链）。
%% Request: `#{messages, context, resume}` → Response: 工具循环结果 tuple
-callback around_turn(Request :: request(), State :: middleware_state(), Next :: next()) ->
    response() | {response(), middleware_state()}.

-optional_callbacks([
    init/1,
    around_chat/3,
    around_tool/3,
    around_turn/3
]).
