%%%-------------------------------------------------------------------
%%% @doc AG-UI HTTP 请求处理（框架无关）
%%%
%%% 只负责：解析/校验请求、定位 agent 工厂、给出响应头。
%%% 不负责：HTTP 服务器启动、路由、TLS、**SSE 帧怎么写出去**——最后这条是
%%% 刻意的：流式写回各框架姿势不同（cowboy 的 stream_body、其他框架的
%%% chunked writer），把它留给适配器，这一层才能真的框架无关。
%%%
%%% == 端点 ==
%%%
%%% - `POST /agui/run`  —— RunAgentInput → SSE 事件流
%%% - `GET  /agui/info` —— 已注册的 agent 列表（对标 getRuntimeInfo）
%%%
%%% == 用法 ==
%%%
%%% ```erlang
%%% Config = beamai_agui_http_handler:init(#{agent => <<"my-agent">>}),
%%% case beamai_agui_http_handler:handle_run(Body, Headers, Config) of
%%%     {stream, Input, Factory, RespHeaders} ->
%%%         %% 200 + RespHeaders，然后逐帧写 beamai_agui_event:to_sse/1
%%%         ok;
%%%     {error, Status, Json, RespHeaders} ->
%%%         send_response(Status, RespHeaders, Json)
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_http_handler).

-export([init/1]).
-export([handle_run/3, handle_info/1]).
-export([sse_headers/0, json_headers/0, cors_headers/0]).

-type config() :: #{
    %% 该端点固定服务的 agent 名；不给则从 forwardedProps.agent 取
    agent => binary()
}.

-export_type([config/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 归一化配置
-spec init(map()) -> config().
init(Config) when is_map(Config) ->
    Config.

%% @doc 处理一次 run 请求
%%
%% @returns {stream, Input, Factory, RespHeaders} —— 校验通过，可以开始流式响应
%% @returns {error, Status, JsonBody, RespHeaders} —— 校验失败
-spec handle_run(binary() | map(), [{binary(), binary()}], config()) ->
    {stream, beamai_agui_types:run_input(), beamai_agui_registry:factory(),
     [{binary(), binary()}]} |
    {error, pos_integer(), binary(), [{binary(), binary()}]}.
handle_run(Body, _Headers, Config) ->
    case beamai_agui_types:parse_run_input(Body) of
        {error, Reason} ->
            error_response(400, <<"invalid_request">>, Reason);
        {ok, Input} ->
            case resolve_agent(Input, Config) of
                {error, Reason} ->
                    error_response(404, <<"agent_not_found">>, Reason);
                {ok, Factory} ->
                    {stream, Input, Factory, sse_headers()}
            end
    end.

%% @doc 运行时信息：已注册的 agent 列表
-spec handle_info(config()) -> {ok, binary(), [{binary(), binary()}]}.
handle_info(_Config) ->
    Body = beamai_utils:encode_json(#{
        <<"agents">> => beamai_agui_registry:list(),
        <<"runtime">> => <<"beamai_agui">>,
        <<"version">> => <<"1.0.0">>
    }),
    {ok, Body, json_headers()}.

%%====================================================================
%% 响应头
%%====================================================================

%% @doc SSE 响应头
%%
%% `x-accel-buffering: no` 是给 nginx 的：默认它会缓冲上游响应，
%% 一缓冲流式就变成了"最后一次性吐出来"，前端看不到任何增量。
-spec sse_headers() -> [{binary(), binary()}].
sse_headers() ->
    [{<<"content-type">>, <<"text/event-stream">>},
     {<<"cache-control">>, <<"no-cache">>},
     {<<"connection">>, <<"keep-alive">>},
     {<<"x-accel-buffering">>, <<"no">>}] ++ cors_headers().

%% @doc JSON 响应头
-spec json_headers() -> [{binary(), binary()}].
json_headers() ->
    [{<<"content-type">>, <<"application/json">>}] ++ cors_headers().

%% @doc CORS 头（AG-UI 前端通常与 runtime 不同源）
-spec cors_headers() -> [{binary(), binary()}].
cors_headers() ->
    [{<<"access-control-allow-origin">>, <<"*">>},
     {<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>},
     {<<"access-control-allow-headers">>, <<"content-type, authorization">>}].

%%====================================================================
%% 内部
%%====================================================================

%% @private 定位 agent 工厂：端点配置优先，其次 forwardedProps.agent
resolve_agent(Input, Config) ->
    case agent_name(Input, Config) of
        undefined ->
            {error, no_agent_specified};
        Name ->
            case beamai_agui_registry:lookup(Name) of
                {ok, Factory} -> {ok, Factory};
                {error, not_found} -> {error, {unknown_agent, Name}}
            end
    end.

agent_name(Input, Config) ->
    case maps:get(agent, Config, undefined) of
        undefined -> beamai_agui_types:agent_name(Input);
        Name -> Name
    end.

error_response(Status, Code, Reason) ->
    Body = beamai_utils:encode_json(#{
        <<"error">> => Code,
        <<"detail">> => iolist_to_binary(io_lib:format("~p", [Reason]))
    }),
    {error, Status, Body, json_headers()}.
