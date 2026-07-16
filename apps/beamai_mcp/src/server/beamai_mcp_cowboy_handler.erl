%%%-------------------------------------------------------------------
%%% @doc MCP Cowboy Handler 适配器
%%%
%%% 将 beamai_mcp_handler 适配到 Cowboy HTTP 服务器。
%%% 这是一个可选模块，用户可以选择使用或自行实现适配器。
%%%
%%% == 支持的端点 ==
%%%
%%% - POST /mcp : Streamable HTTP 端点
%%% - GET /mcp/sse : SSE 流端点（可选）
%%% - POST /mcp/sse/message : SSE 模式消息端点（可选）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 在 Cowboy 路由配置中
%%% McpConfig = #{
%%%     tools => [
%%%         beamai_mcp_types:make_tool(
%%%             <<"echo">>,
%%%             <<"Echo the input">>,
%%%             #{type => object, properties => #{text => #{type => string}}},
%%%             fun(#{<<"text">> := Text}) -> {ok, Text} end
%%%         )
%%%     ],
%%%     server_info => #{<<"name">> => <<"my-mcp-server">>, <<"version">> => <<"1.0">>}
%%% },
%%%
%%% Dispatch = cowboy_router:compile([
%%%     {'_', [
%%%         %% Streamable HTTP 端点
%%%         {"/mcp", beamai_mcp_cowboy_handler, McpConfig},
%%%         %% SSE 端点（可选）
%%%         {"/mcp/sse", beamai_mcp_cowboy_handler, McpConfig#{mode => sse_init}},
%%%         {"/mcp/sse/message", beamai_mcp_cowboy_handler, McpConfig#{mode => sse_message}}
%%%     ]}
%%% ]),
%%%
%%% {ok, _} = cowboy:start_clear(mcp_listener, [{port, 8080}],
%%%                               #{env => #{dispatch => Dispatch}}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_cowboy_handler).

-include("beamai_mcp.hrl").

%%====================================================================
%% Cowboy Handler 回调
%%====================================================================

-export([init/2]).

%% 用于 SSE 流模式
-export([info/3]).

%% cowboy_loop 清理回调
-export([terminate/3]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(sse_state, {
    server_pid :: pid() | undefined,
    session_id :: binary() | undefined,
    config :: map()
}).

%%====================================================================
%% Cowboy Handler 实现
%%====================================================================

%% @doc 初始化请求处理
%%
%% 根据请求方法和配置的 mode 分发到不同的处理逻辑。
init(Req, Config) ->
    Method = cowboy_req:method(Req),
    Mode = maps:get(mode, Config, streamable_http),
    handle_by_mode(Method, Mode, Req, Config).

%% @private 根据模式处理请求
handle_by_mode(<<"POST">>, streamable_http, Req, Config) ->
    handle_streamable_http(Req, Config);
handle_by_mode(<<"DELETE">>, streamable_http, Req, Config) ->
    handle_session_delete(Req, Config);
handle_by_mode(<<"POST">>, sse_message, Req, Config) ->
    handle_sse_message(Req, Config);
handle_by_mode(<<"GET">>, sse_init, Req, Config) ->
    handle_sse_init(Req, Config);
handle_by_mode(<<"OPTIONS">>, _, Req, Config) ->
    handle_options(Req, Config);
handle_by_mode(Method, Mode, Req, _Config) ->
    Req2 = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>,
        <<"allow">> => allowed_methods(Mode)
    }, jsx:encode(#{
        <<"error">> => <<"Method not allowed">>,
        <<"method">> => Method
    }), Req),
    {ok, Req2, undefined}.

%%====================================================================
%% Streamable HTTP 处理
%%====================================================================

%% @private 处理 Streamable HTTP POST 请求
%%
%% MCP 是有状态会话协议，server 进程必须**跨请求存活**。历史实现每个请求
%% `beamai_mcp_handler:init(Config)'（新建 server）→ 处理 → `close'（停掉），
%% session id 读了也回显了但从没用它查过 session——于是 initialize 之后的
%% 一切方法都打到全新 server 上，一律 -32600 "Server not initialized"。
%%
%% 现在的流程（对照 MCP Streamable HTTP 规范）：
%%   - 无 mcp-session-id 头 + initialize 请求 → 经 server_sup 起会话级 server，
%%     分发后从响应态取回 server 发的 session id，登记进 registry，
%%     响应头带 mcp-session-id；
%%   - 带 mcp-session-id 头 → registry 找回同一个 server 分发；
%%     查无此会话 → 404（规范语义：客户端须重新 initialize）；
%%   - 无头且不是 initialize → 400；
%%   - DELETE + mcp-session-id → 终止会话（见 handle_session_delete/2）。
handle_streamable_http(Req, Config) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case cowboy_req:header(<<"mcp-session-id">>, Req2) of
        undefined ->
            case is_initialize_request(Body) of
                true -> start_session(Body, Req2, Config);
                false -> reply_json(400, beamai_mcp_jsonrpc:invalid_request(null), Req2)
            end;
        SessionId ->
            case beamai_mcp_session_registry:lookup(SessionId) of
                {ok, ServerPid} ->
                    dispatch_to_session(ServerPid, Body, Req2, Config);
                {error, not_found} ->
                    %% 规范：未知/过期会话回 404，客户端据此重新 initialize
                    Req3 = cowboy_req:reply(404, #{}, <<>>, Req2),
                    {ok, Req3, undefined}
            end
    end.

%% @private initialize：起会话级 server，登记，响应头发 session id
start_session(Body, Req, Config) ->
    {ok, ServerPid} = beamai_mcp_server_sup:start_server(Config),
    HandlerState = beamai_mcp_handler:init_with_server(ServerPid, Config),
    Headers = maps:to_list(cowboy_req:headers(Req)),
    {Type, Response, NewState} =
        beamai_mcp_handler:handle_post(Body, Headers, HandlerState),
    case beamai_mcp_handler:get_session_id(NewState) of
        undefined ->
            %% initialize 没走通（版本不合、参数非法……）：会话没建立，
            %% 不登记、把刚起的 server 停掉，错误照常返回
            catch beamai_mcp_server_sup:stop_server(ServerPid),
            reply_dispatched(Type, Response, undefined, Req);
        SessionId ->
            ok = beamai_mcp_session_registry:register(SessionId, ServerPid),
            reply_dispatched(Type, Response, SessionId, Req)
    end.

%% @private 后续请求：分发到会话既有的 server；**不许 close**（server 要活过本请求）
dispatch_to_session(ServerPid, Body, Req, Config) ->
    HandlerState = beamai_mcp_handler:init_with_server(ServerPid, Config),
    Headers = maps:to_list(cowboy_req:headers(Req)),
    {Type, Response, NewState} =
        beamai_mcp_handler:handle_post(Body, Headers, HandlerState),
    reply_dispatched(Type, Response, beamai_mcp_handler:get_session_id(NewState), Req).

%% @private 按 handler 判定的类型（json/sse）回复
reply_dispatched(json, Response, SessionId, Req) ->
    RespHeaders = build_response_headers(json, SessionId),
    Req2 = cowboy_req:reply(200, RespHeaders, Response, Req),
    {ok, Req2, undefined};
reply_dispatched(sse, Response, SessionId, Req) ->
    RespHeaders = build_response_headers(sse, SessionId),
    Req2 = cowboy_req:stream_reply(200, RespHeaders, Req),
    cowboy_req:stream_body(iolist_to_binary(Response), fin, Req2),
    {ok, Req2, undefined}.

%% @private DELETE：客户端显式终止会话（规范允许；204 成功，404 查无此会话）
handle_session_delete(Req, _Config) ->
    case cowboy_req:header(<<"mcp-session-id">>, Req) of
        undefined ->
            reply_json(400, beamai_mcp_jsonrpc:invalid_request(null), Req);
        SessionId ->
            case beamai_mcp_session_registry:delete(SessionId) of
                ok ->
                    Req2 = cowboy_req:reply(204, #{}, <<>>, Req),
                    {ok, Req2, undefined};
                {error, not_found} ->
                    Req2 = cowboy_req:reply(404, #{}, <<>>, Req),
                    {ok, Req2, undefined}
            end
    end.

%% @private 请求体是否为 initialize（决定能否免 session id 头）
is_initialize_request(Body) ->
    try jsx:decode(Body, [return_maps]) of
        #{<<"method">> := <<"initialize">>} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

%% @private 以 JSON-RPC 错误体回复
reply_json(Status, ErrorBody, Req) ->
    Req2 = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, ErrorBody, Req),
    {ok, Req2, undefined}.

%%====================================================================
%% SSE 模式处理
%%====================================================================

%% @private 处理 SSE 初始化（GET 请求）
%%
%% SSE 是双端点会话：GET 建立事件流并返回 endpoint 事件（内含 POST 端点），
%% 客户端此后 POST 请求到该端点、响应经本流回推。因此 GET 这一步必须：
%%   - 起一个会话级 server（跨 POST 存活）；
%%   - 把 **session id 编进 endpoint URL**（POST 请求靠它自报身份）；
%%   - 登记 {session_id → server, 本 cowboy_loop 进程} 进 registry。
%%
%% 历史实现每次 GET `beamai_mcp_handler:init'（新建 server）却从不登记、
%% endpoint URL 也不带 session id——POST 进来根本无从定位会话，
%% 那个 handle_sse_message 只能干回 202、请求响应永远回不来。
handle_sse_init(Req, Config) ->
    %% 会话级 server（不能用嵌入式 init/1，那条路用完即停）
    {ok, ServerPid} = beamai_mcp_server_sup:start_server(Config),
    SessionId = generate_session_id(),

    %% POST 端点 = <当前路径>/message?session_id=<id>
    BasePath = maps:get(sse_message_path, Config,
                        <<(cowboy_req:path(Req))/binary, "/message">>),
    Endpoint = <<BasePath/binary, "?session_id=", SessionId/binary>>,
    EndpointEvent = beamai_mcp_handler:format_sse_response(
                      <<"endpoint">>, jsx:encode(#{<<"uri">> => Endpoint})),

    RespHeaders = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    Req2 = cowboy_req:stream_reply(200, RespHeaders, Req),
    cowboy_req:stream_body(iolist_to_binary(EndpointEvent), nofin, Req2),

    ok = beamai_mcp_session_registry:register_sse(SessionId, ServerPid, self()),
    State = #sse_state{server_pid = ServerPid, session_id = SessionId, config = Config},
    {cowboy_loop, Req2, State}.

%% @private crypto 强随机 session id（与 handler/server 同款）
generate_session_id() ->
    Hex = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"mcp-", Hex/binary>>.

%% @private 处理 SSE 模式下的消息 POST（`/message?session_id=X'）
%%
%% 用 session_id 从 registry 找到该会话的 server 与 SSE loop 进程：
%% 请求分发到 server，响应 push 给 loop（由 loop 写进 SSE 流），
%% 本 POST 自身回 202 Accepted——这正是 SSE 传输模式的规范形态
%% （POST 只投递，真正的响应从流出来）。
handle_sse_message(Req, _Config) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    case cowboy_req:match_qs([{session_id, [], undefined}], Req2) of
        #{session_id := undefined} ->
            reply_json(400, beamai_mcp_jsonrpc:invalid_request(null), Req2);
        #{session_id := SessionId} ->
            case beamai_mcp_session_registry:lookup_sse(SessionId) of
                {ok, ServerPid, LoopPid} ->
                    Response = dispatch_sse_request(ServerPid, Body),
                    LoopPid ! {send_event, <<"message">>, Response},
                    Req3 = cowboy_req:reply(202, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{<<"status">> => <<"accepted">>}), Req2),
                    {ok, Req3, undefined};
                {error, not_found} ->
                    Req3 = cowboy_req:reply(404, #{}, <<>>, Req2),
                    {ok, Req3, undefined}
            end
    end.

%% @private 分发单条 SSE 消息，返回已编码的 JSON-RPC 响应 binary
dispatch_sse_request(ServerPid, Body) ->
    case beamai_mcp_jsonrpc:decode(Body) of
        {ok, Request} ->
            case beamai_mcp_server:handle_request(ServerPid, Request) of
                {ok, Response} -> Response;
                {error, _} ->
                    beamai_mcp_jsonrpc:internal_error(request_id(Request))
            end;
        {error, _} ->
            beamai_mcp_jsonrpc:parse_error(null)
    end.

request_id(Request) when is_map(Request) -> maps:get(<<"id">>, Request, null);
request_id(_) -> null.

%% @doc 处理 SSE 流中的消息
%%
%% 用于 cowboy_loop 模式
info({send_event, EventType, Data}, Req, State) ->
    %% 把会话响应写进 SSE 流（Data 已是编码好的 JSON-RPC 响应 binary）
    EventData = beamai_mcp_handler:format_sse_response(EventType, Data),
    cowboy_req:stream_body(iolist_to_binary(EventData), nofin, Req),
    {ok, Req, State};

info(close, Req, State) ->
    %% registry 已负责停 server（stop_session_pids），这里只收流。
    {stop, Req, State};

info(_Info, Req, State) ->
    {ok, Req, State}.

%% cowboy_loop 进程终止（客户端断连等）：registry 的 monitor 会摘掉会话并停 server
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% CORS 和 OPTIONS 处理
%%====================================================================

%% @private 处理 OPTIONS 请求（CORS 预检）
handle_options(Req, Config) ->
    Mode = maps:get(mode, Config, streamable_http),
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => allowed_methods(Mode),
        <<"access-control-allow-headers">> => <<"content-type, mcp-session-id, authorization">>,
        <<"access-control-max-age">> => <<"86400">>
    }, <<>>, Req),
    {ok, Req2, undefined}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建响应头
-spec build_response_headers(json | sse, binary() | undefined) -> map().
build_response_headers(json, SessionId) ->
    Base = #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    add_session_header(Base, SessionId);
build_response_headers(sse, SessionId) ->
    Base = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    add_session_header(Base, SessionId).

%% @private 添加会话 ID 头
-spec add_session_header(map(), binary() | undefined) -> map().
add_session_header(Headers, undefined) ->
    Headers;
add_session_header(Headers, SessionId) ->
    Headers#{<<"mcp-session-id">> => SessionId}.

%% @private 获取允许的方法
-spec allowed_methods(atom()) -> binary().
allowed_methods(streamable_http) -> <<"POST, DELETE, OPTIONS">>;
allowed_methods(sse_init) -> <<"GET, OPTIONS">>;
allowed_methods(sse_message) -> <<"POST, OPTIONS">>;
allowed_methods(_) -> <<"POST, GET, OPTIONS">>.
