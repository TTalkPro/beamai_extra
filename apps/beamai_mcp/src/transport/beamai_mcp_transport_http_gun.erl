%%%-------------------------------------------------------------------
%%% @doc MCP Streamable HTTP 传输实现（Gun 后端）
%%%
%%% 使用 Gun 作为 HTTP 客户端的 MCP Streamable HTTP 传输。
%%% 支持 HTTP/2，提供更好的性能和多路复用。
%%%
%%% == 配置参数 ==
%%%
%%% ```erlang
%%% #{
%%%     transport => http,
%%%     backend => gun,                              %% 使用 Gun 后端
%%%     url => <<"https://example.com/mcp">>,        %% 必填：服务器端点
%%%     headers => [{<<"Authorization">>, <<"Bearer token">>}], %% 可选
%%%     timeout => 30000,                            %% 可选：请求超时
%%%     session_id => undefined                      %% 可选：会话 ID
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_http_gun).

-behaviour(beamai_mcp_transport).

%% 行为回调导出
-export([
    connect/1,
    send/2,
    recv/2,
    close/1,
    is_connected/1,
    set_protocol_version/2
]).

%% 额外 API
-export([
    request/2,
    request/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-define(MCP_HTTP_MAX_BODY, 16 * 1024 * 1024).

-record(http_gun_state, {
    %% Gun owner 进程。它把 gun 事件原样转发给 MCP client 进程，
    %% 避免这些消息与 gen_statem 的 info 事件混在同一 mailbox 中。
    worker :: pid() | undefined,
    worker_monitor :: reference() | undefined,
    %% 服务器信息
    host :: binary(),
    port :: pos_integer(),
    path :: binary(),
    transport :: tcp | tls,
    %% 请求头
    headers = [] :: [{binary(), binary()}],
    timeout :: pos_integer(),
    session_id :: binary() | undefined,
    protocol_version :: binary() | undefined,
    %% 当前请求的流引用与请求级截止时间
    stream_ref :: reference() | undefined,
    deadline :: integer() | undefined,
    %% SSE 解析缓冲区
    buffer = <<>> :: binary(),
    response_is_sse = false :: boolean(),
    nonblocking = false :: boolean(),
    %% 消息队列
    message_queue = [] :: [binary()],
    %% 响应体保护上限
    max_body = ?MCP_HTTP_MAX_BODY :: pos_integer(),
    error_response :: {pos_integer(), binary()} | undefined,
    %% 是否已连接
    connected = false :: boolean()
}).

-type state() :: #http_gun_state{}.

%%====================================================================
%% 行为回调实现
%%====================================================================

%% @doc 创建 HTTP 传输并建立连接
-spec connect(map()) -> {ok, state()} | {error, term()}.
connect(#{url := Url} = Config) ->
    Headers = maps:get(headers, Config, []),
    Timeout = maps:get(timeout, Config, 30000),
    SessionId = maps:get(session_id, Config, undefined),
    ProtocolVersion = maps:get(protocol_version, Config, undefined),
    MaxBody = maps:get(max_body, Config, ?MCP_HTTP_MAX_BODY),

    %% 解析 URL
    case parse_url(Url) of
        {ok, Host, Port, Path, Transport} ->
            %% 建立 Gun 连接。Gun 要求 owner 处理连接事件；这个专用 owner
            %% 进程让 MCP client 的 gen_statem 不需要“轮询 + 抢收”自己的邮箱。
            GunOpts = #{
                connect_timeout => Timeout,
                transport => Transport,
                protocols => protocols(Transport),
                tls_opts => tls_opts(Host, Transport, Config)
            },
            case open_connection(Host, Port, GunOpts, Timeout) of
                {ok, Worker, Monitor} ->
                    State = #http_gun_state{
                        worker = Worker,
                        worker_monitor = Monitor,
                        host = Host,
                        port = Port,
                        path = Path,
                        transport = Transport,
                        headers = Headers,
                        timeout = Timeout,
                        session_id = SessionId,
                        protocol_version = ProtocolVersion,
                        max_body = MaxBody,
                        connected = true
                    },
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
connect(_) ->
    {error, missing_url}.

%% @doc 发送请求
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(Message, #http_gun_state{worker = Worker, path = Path,
                              headers = Headers, session_id = SessionId,
                              protocol_version = ProtocolVersion,
                              worker_monitor = Monitor,
                              timeout = Timeout} = State) ->
    %% 构建请求头
    RequestHeaders = build_request_headers(Headers, SessionId,
                                           ProtocolVersion),
    Tag = make_ref(),
    Worker ! {beamai_mcp_http, self(), Tag, {post, Path, RequestHeaders, Message}},
    receive
        {beamai_mcp_http, Worker, Tag, {sent, StreamRef}} ->
            Deadline = erlang:monotonic_time(millisecond) + Timeout,
            SentState = State#http_gun_state{
                stream_ref = StreamRef,
                deadline = Deadline,
                buffer = <<>>,
                message_queue = []
            },
            case is_jsonrpc_notification(Message) of
                true ->
                    %% Streamable HTTP 的通知由 HTTP 状态码确认。在返回前排空
                    %% 202/空响应，避免 client 立刻复用连接时和下一个请求竞争。
                    drain_notification_response(Timeout, SentState);
                false ->
                    {ok, SentState}
            end;
        {beamai_mcp_http, Worker, Tag, {error, Reason}} ->
            {error, Reason};
        {'DOWN', Monitor, process, Worker, Reason} ->
            {error, {worker_down, Reason}}
    after Timeout ->
        {error, send_timeout}
    end.

%% @doc 接收响应
%%
%% `{wait, NewState}' = 这一轮没有完整消息，但 NewState 里可能已经存着
%% 本轮到达的 body 分块／半个 SSE 事件，调用方必须穿着它继续轮询。
-spec recv(timeout(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
recv(_Timeout, #http_gun_state{message_queue = [Msg | Rest]} = State) ->
    {ok, Msg, State#http_gun_state{message_queue = Rest}};
recv(_Timeout, #http_gun_state{stream_ref = undefined}) ->
    {error, no_pending_request};
recv(_Timeout, #http_gun_state{worker_monitor = Monitor,
                               connected = false})
  when Monitor =/= undefined ->
    receive
        {'DOWN', Monitor, process, _Worker, Reason} ->
            {error, {worker_down, Reason}}
    after 0 ->
        {error, not_connected}
    end;
recv(Timeout, #http_gun_state{stream_ref = StreamRef, deadline = Deadline} = State)
  when is_reference(StreamRef), Deadline =/= undefined ->
    Now = erlang:monotonic_time(millisecond),
    Remaining = max(0, Deadline - Now),
    %% 重点：timeout=0 只是“现在没数据就返回”。这个调用绝不能取消在途请求；
    %% 请求只有在自身截止时间到了之后才允许 cancel。
    Wait = case Timeout of
        0 -> 0;
        _ -> min(Timeout, Remaining)
    end,
    WaitState = State#http_gun_state{nonblocking = Wait =:= 0},
    case {Remaining, Wait} of
        {0, _} ->
            cancel_request(State),
            {error, timeout};
        {_, 0} ->
            %% 非阻塞：receive_response 内部会把 worker 队列里已经躺着的
            %% 事件一次读干，只有队列真空了才交回 {wait, State}。
            receive_response(0, WaitState);
        _ ->
            receive_response(Wait, WaitState)
    end.

%% @doc 关闭连接
-spec close(state()) -> ok | {error, term()}.
close(#http_gun_state{worker = Worker, worker_monitor = Monitor}) ->
    case Worker of
        undefined -> ok;
        _ ->
            Worker ! {beamai_mcp_http, close},
            case Monitor of
                undefined -> ok;
                _ ->
                    erlang:demonitor(Monitor, [flush])
            end
    end,
    ok.

%% @doc 检查连接状态
-spec is_connected(state()) -> boolean().
is_connected(#http_gun_state{connected = Connected, worker = Worker}) ->
    Connected andalso is_pid(Worker) andalso is_process_alive(Worker).

%% @doc 记录 initialize 协商出的协议版本，供后续 HTTP 请求头使用。
-spec set_protocol_version(binary(), state()) -> state().
set_protocol_version(Version, State) ->
    State#http_gun_state{protocol_version = Version}.

%%====================================================================
%% 额外 API
%%====================================================================

%% @doc 发送请求并同步等待响应
-spec request(binary(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
request(Message, State) ->
    request(Message, State#http_gun_state.timeout, State).

%% @doc 发送请求并同步等待响应（指定超时）
-spec request(binary(), timeout(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
request(Message, Timeout, State) ->
    {ok, SentState} = send(Message, State#http_gun_state{timeout = Timeout}),
    recv(Timeout, SentState).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析 URL
-spec parse_url(binary()) -> {ok, binary(), pos_integer(), binary(), tcp | tls} | {error, term()}.
parse_url(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parsed ->
            Port = maps:get(port, Parsed, default_port(Scheme)),
            RawPath = maps:get(path, Parsed, <<"/">>),
            %% uri_string:parse 对无路径的 URL（如 <<"http://host">>）返回 path => <<>>，
            %% maps:get/3 的默认值仅在键不存在时生效，故需手动兜底。
            Path = case RawPath of <<>> -> <<"/">>; _ -> RawPath end,
            Query = maps:get(query, Parsed, <<>>),
            FullPath = case Query of
                <<>> -> ensure_binary(Path);
                _ -> <<(ensure_binary(Path))/binary, "?", (ensure_binary(Query))/binary>>
            end,
            Transport = case Scheme of
                <<"https">> -> tls;
                "https" -> tls;
                _ -> tcp
            end,
            {ok, ensure_binary(Host), Port, FullPath, Transport};
        _ ->
            {error, {invalid_url, Url}}
    end.

%% @private 默认端口
default_port(<<"https">>) -> 443;
default_port("https") -> 443;
default_port(_) -> 80.

%% @private 确保是二进制
ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V).

%% @private 构建请求头
-spec build_request_headers([{binary(), binary()}], binary() | undefined,
                            binary() | undefined) ->
    [{binary(), binary()}].
build_request_headers(Headers, SessionId, ProtocolVersion) ->
    BaseHeaders = [
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json, text/event-stream">>}
        | Headers
    ],
    HeadersWithSession = case SessionId of
        undefined -> BaseHeaders;
        _ -> [{<<"mcp-session-id">>, SessionId} | BaseHeaders]
    end,
    case ProtocolVersion of
        undefined -> HeadersWithSession;
        _ -> HeadersWithSession ++ [{<<"mcp-protocol-version">>, ProtocolVersion}]
    end.

%% @private 启动 Gun owner 并等待连接就绪
-spec open_connection(binary(), pos_integer(), map(), pos_integer()) ->
    {ok, pid(), reference()} | {error, term()}.
open_connection(Host, Port, GunOpts, Timeout) ->
    Parent = self(),
    {Worker, Monitor} = spawn_monitor(fun() ->
        worker_init(Parent, Host, Port, GunOpts, Timeout)
    end),
    receive
        {beamai_mcp_http, Worker, ready, {ok, _Protocol}} ->
            {ok, Worker, Monitor};
        {beamai_mcp_http, Worker, ready, {error, Reason}} ->
            erlang:demonitor(Monitor, [flush]),
            {error, Reason};
        {'DOWN', Monitor, process, Worker, Reason} ->
            {error, {worker_down, Reason}}
    after Timeout + 100 ->
        exit(Worker, kill),
        erlang:demonitor(Monitor, [flush]),
        {error, connection_timeout}
    end.

%% @private Cleartext HTTP/2 requires prior knowledge; Gun cannot negotiate
%% this list over TCP. TLS uses ALPN and can fall back to HTTP/1.1.
-spec protocols(tcp | tls) -> [http2 | http].
protocols(tls) -> [http2, http];
protocols(tcp) -> [http].

%% @private Gun owner 初始化
worker_init(Parent, Host, Port, GunOpts, Timeout) ->
    ParentMonitor = erlang:monitor(process, Parent),
    worker_wait_up(Parent, ParentMonitor,
                   gun:open(binary_to_list(Host), Port, GunOpts), Timeout).

worker_wait_up(Parent, ParentMonitor, {ok, ConnPid}, Timeout) ->
    case gun:await_up(ConnPid, Timeout) of
        {ok, Protocol} ->
            Parent ! {beamai_mcp_http, self(), ready, {ok, Protocol}},
            worker_loop(Parent, ParentMonitor, ConnPid, queue:new());
        {error, Reason} ->
            gun:close(ConnPid),
            Parent ! {beamai_mcp_http, self(), ready,
                      {error, {connection_failed, Reason}}},
            worker_wait_parent_down(ParentMonitor)
    end;
worker_wait_up(Parent, ParentMonitor, {error, Reason}, _Timeout) ->
    Parent ! {beamai_mcp_http, self(), ready, {error, {open_failed, Reason}}},
    worker_wait_parent_down(ParentMonitor).

worker_wait_parent_down(Monitor) ->
    receive
        {'DOWN', Monitor, process, _, _} -> ok
    end.

%% @private Gun owner 主循环
worker_loop(Parent, ParentMonitor, ConnPid, EventQueue) ->
    receive
        {'DOWN', ParentMonitor, process, _, _} ->
            gun:close(ConnPid);
        {beamai_mcp_http, close} ->
            gun:close(ConnPid);
        {beamai_mcp_http, Parent, Tag, {post, Path, Headers, Body}} ->
            StreamRef = gun:post(ConnPid, Path, Headers, Body),
            Parent ! {beamai_mcp_http, self(), Tag, {sent, StreamRef}},
            worker_loop(Parent, ParentMonitor, ConnPid, EventQueue);
        {beamai_mcp_http, cancel, StreamRef} ->
            gun:cancel(ConnPid, StreamRef),
            worker_loop(Parent, ParentMonitor, ConnPid, EventQueue);
        {beamai_mcp_http, Parent, Tag, {recv, Wait}} ->
            case queue:out(EventQueue) of
                {{value, Event}, RestQueue} ->
                    Parent ! {beamai_mcp_http, self(), Tag, {event, Event}},
                    worker_loop(Parent, ParentMonitor, ConnPid, RestQueue);
                {empty, _} ->
                    receive
                        {'DOWN', ParentMonitor, process, _, _} ->
                            gun:close(ConnPid);
                        {beamai_mcp_http, close} ->
                            gun:close(ConnPid);
                        GunEvent when element(1, GunEvent) =:= gun_response;
                                      element(1, GunEvent) =:= gun_data;
                                      element(1, GunEvent) =:= gun_error;
                                      element(1, GunEvent) =:= gun_down ->
                            Parent ! {beamai_mcp_http, self(), Tag,
                                      {event, GunEvent}},
                            worker_loop(Parent, ParentMonitor, ConnPid,
                                        EventQueue)
                    after Wait ->
                        Parent ! {beamai_mcp_http, self(), Tag, timeout},
                        worker_loop(Parent, ParentMonitor, ConnPid, EventQueue)
                    end
            end;
        Message when element(1, Message) =:= gun_response;
                     element(1, Message) =:= gun_data;
                     element(1, Message) =:= gun_error;
                     element(1, Message) =:= gun_down ->
            worker_loop(Parent, ParentMonitor, ConnPid,
                        queue:in(Message, EventQueue))
    end.

%% @private 读取由 worker 转发来的当前请求事件
-spec receive_response(timeout(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
receive_response(Timeout, State) ->
    case receive_gun_event(Timeout, State) of
        {ok, Event, NewState} ->
            handle_gun_event(Event, NewState);
        %% 本轮没有新事件。这不一定是失败：非阻塞轮询下“已经收了半截、
        %% 还没收完”是正常状态，必须把 state 交回调用方，否则本轮
        %% 已并入 buffer 的分块（以及 session_id 等）随错误返回一起作废。
        {error, timeout} -> {wait, State};
        {error, _} = Error -> Error
    end.

-spec receive_gun_event(timeout(), state()) ->
    {ok, tuple(), state()} | {error, term()}.
receive_gun_event(Timeout, #http_gun_state{worker = Worker,
                                           worker_monitor = Monitor,
                                           stream_ref = StreamRef} = State) ->
    Tag = make_ref(),
    Worker ! {beamai_mcp_http, self(), Tag, {recv, Timeout}},
    receive
        %% 上一个请求迟到的事件：不属于当前 stream_ref 的直接丢弃，
        %% 继续等当前请求的下一个事件（连接级事件没有 StreamRef，照常放行）。
        {beamai_mcp_http, Worker, Tag, {event, Event}} ->
            case is_stale_event(Event, StreamRef) of
                true -> receive_gun_event(Timeout, State);
                false -> {ok, Event, State}
            end;
        {beamai_mcp_http, Worker, Tag, timeout} ->
            {error, timeout};
        {'DOWN', Monitor, process, Worker, Reason} ->
            {error, {worker_down, Reason}}
    after Timeout + 1 ->
        {error, timeout}
    end.

%% @private 事件是否属于别的请求。连接级事件（{gun_error, Conn, Reason}、
%% gun_down）的第 3 元不是 stream reference，照常交给 handle_gun_event；
%% 没有在途请求（stream_ref=undefined）时一律放行，维持原有的收尾行为。
-spec is_stale_event(tuple(), reference() | undefined) -> boolean().
is_stale_event(_Event, undefined) -> false;
is_stale_event(Event, StreamRef) when tuple_size(Event) >= 3 ->
    case element(3, Event) of
        Ref when is_reference(Ref) -> Ref =/= StreamRef;
        _ -> false
    end;
is_stale_event(_Event, _StreamRef) -> false.

-spec handle_gun_event(tuple(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
handle_gun_event({gun_response, _Worker, StreamRef, fin, Status, _Headers}, State)
  when Status >= 200, Status < 300 ->
    _ = StreamRef,
    {ok, <<>>, clear_request(State)};
handle_gun_event({gun_response, _Worker, _StreamRef, nofin, Status, Headers}, State)
  when Status >= 200, Status < 300 ->
    SessionId = beamai_mcp_types:get_header(Headers, <<"mcp-session-id">>),
    BaseState = case SessionId of
        undefined -> State;
        _ -> State#http_gun_state{session_id = SessionId}
    end,
    ContentType = beamai_mcp_types:get_header(Headers, <<"content-type">>),
    NewState = BaseState#http_gun_state{
        response_is_sse = is_sse_content_type(ContentType)
    },
    receive_response(remaining_timeout(NewState), NewState);
handle_gun_event({gun_response, _Worker, _StreamRef, fin, Status, _Headers}, State) ->
    _ = cancel_request(State),
    {error, {http_error, Status, <<>>}};
handle_gun_event({gun_response, _Worker, _StreamRef, nofin, Status, _Headers}, State) ->
    NewState = State#http_gun_state{
        error_response = {Status, <<>>},
        response_is_sse = false
    },
    receive_response(remaining_timeout(NewState), NewState);
handle_gun_event({gun_data, _Worker, _StreamRef, fin, Data}, State) ->
    case State#http_gun_state.error_response of
        {Status, Buffer} ->
            _ = cancel_request(State),
            {error, {http_error, Status, <<Buffer/binary, Data/binary>>}};
        undefined ->
            append_data(Data, clear_request(State))
    end;
handle_gun_event({gun_data, _Worker, _StreamRef, nofin, Data}, State) ->
    case State#http_gun_state.error_response of
        {Status, Buffer} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case byte_size(NewBuffer) > State#http_gun_state.max_body of
                true ->
                    cancel_request(State),
                    {error, response_too_large};
                false ->
                    NewState = State#http_gun_state{
                        error_response = {Status, NewBuffer}
                    },
                    receive_response(remaining_timeout(NewState), NewState)
            end;
        undefined ->
            append_data(Data, State)
    end;
handle_gun_event({gun_error, _Worker, _StreamRef, Reason}, State) ->
    clear_request(State),
    {error, {gun_error, Reason}};
handle_gun_event({gun_error, _Worker, Reason}, State) ->
    clear_request(State),
    {error, {gun_error, Reason}};
handle_gun_event({gun_down, _Worker, _StreamRef, Reason}, State) ->
    clear_request(State),
    {error, {connection_closed, Reason}}.


append_data(Data, #http_gun_state{buffer = Buffer, max_body = MaxBody} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    case byte_size(NewBuffer) > MaxBody of
        true ->
            cancel_request(State),
            {error, response_too_large};
        false ->
            receive_sse_data(NewBuffer, State#http_gun_state{buffer = NewBuffer})
    end.

%% @private JSON 与 SSE 响应体都经这里累积；只有 SSE 会被提前分帧。
-spec receive_sse_data(binary(), state()) ->
    {ok, binary(), state()} | {wait, state()} | {error, term()}.
receive_sse_data(Buffer, #http_gun_state{response_is_sse = false} = State) ->
    case State#http_gun_state.stream_ref of
        undefined -> {ok, Buffer, State};
        _ -> receive_response(remaining_timeout(State), State)
    end;
receive_sse_data(Buffer, State) ->
    {Remaining, Events} = parse_sse(Buffer),
    Messages = extract_messages(Events),
    case Messages of
        [Msg | Rest] ->
            {ok, Msg, State#http_gun_state{
                buffer = Remaining,
                message_queue = State#http_gun_state.message_queue ++ Rest
            }};
        [] when State#http_gun_state.stream_ref =:= undefined ->
            case Buffer of
                <<>> -> {error, no_pending_request};
                _ -> {error, no_message}
            end;
        [] ->
            NewState = State#http_gun_state{buffer = Remaining},
            receive_response(remaining_timeout(NewState), NewState)
    end.

clear_request(State) ->
    State#http_gun_state{stream_ref = undefined, deadline = undefined,
                         error_response = undefined}.

cancel_request(#http_gun_state{worker = Worker, stream_ref = StreamRef}) ->
    case is_pid(Worker) andalso is_reference(StreamRef) of
        true -> Worker ! {beamai_mcp_http, cancel, StreamRef};
        false -> ok
    end.

remaining_timeout(#http_gun_state{deadline = undefined}) -> 0;
remaining_timeout(#http_gun_state{nonblocking = true}) -> 0;
remaining_timeout(#http_gun_state{deadline = Deadline}) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

%% @private 显式 TLS 默认值；用户传入 tls_opts 时原样透传。
-spec tls_opts(binary(), tcp | tls, map()) -> [term()].
tls_opts(_Host, tcp, _Config) -> [];
tls_opts(Host, tls, Config) ->
    case maps:get(tls_opts, Config, undefined) of
        Opts when is_list(Opts) -> Opts;
        undefined -> default_tls_opts(Host)
    end.

default_tls_opts(Host) ->
    [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {server_name_indication, binary_to_list(Host)},
        {customize_hostname_check,
         [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
    ].

%% @private 获取响应头

%% @private 检查是否为 SSE Content-Type
-spec is_sse_content_type(binary() | undefined) -> boolean().
is_sse_content_type(undefined) -> false;
is_sse_content_type(ContentType) ->
    binary:match(ContentType, <<"text/event-stream">>) =/= nomatch.

%% @private JSON-RPC notification does not carry an id; Streamable HTTP
%% returns an HTTP acknowledgement which must be consumed before reusing
%% the HTTP/2 connection for another request.
-spec is_jsonrpc_notification(binary()) -> boolean().
is_jsonrpc_notification(Message) ->
    case beamai_mcp_jsonrpc:decode(Message) of
        {ok, #{<<"method">> := _Method} = Decoded} ->
            not is_map_key(<<"id">>, Decoded);
        _ ->
            false
    end.

-spec drain_notification_response(timeout(), state()) ->
    {ok, state()} | {error, term()}.
drain_notification_response(Timeout, State) ->
    case receive_response(Timeout, State) of
        {ok, _Response, DoneState} ->
            {ok, DoneState};
        %% 通知发出去就算数。本轮没等到确认也要把推进后的 state 交回去
        %% （session_id 之类可能就在已到达的响应头里）；剩下的确认留给
        %% 下一次 recv，由 stream_ref 过滤掉。
        {wait, WaitState} ->
            {ok, WaitState};
        {error, no_pending_request} ->
            %% 上一个请求可能留下尾部 fin；继续等真正的 HTTP 通知确认。
            drain_notification_response(Timeout, State);
        {error, Reason} ->
            {error, Reason}
    end.

%% beamai_sse:parse/1 目前按 LF 分帧；远端服务器普遍使用 CRLF。
%% HTTP transport 有独立的最小解析器，避免把整条未完成事件永久留在 buffer。
-spec parse_sse(binary()) -> {binary(), [map()]}.
parse_sse(Data) ->
    Candidates = [{binary:match(Data, <<"\r\n\r\n">>), 4},
                  {binary:match(Data, <<"\n\n">>), 2}],
    case lists:filtermap(fun({M, _}) when M =:= undefined; M =:= nomatch -> false;
                            ({{Pos, _}, Len}) -> {true, {Pos, Len}}
                         end, Candidates) of
        [] -> {Data, []};
        Matches ->
            {Pos, Len} = lists:min(Matches),
            Block = binary:part(Data, 0, Pos),
            Rest = binary:part(Data, Pos + Len, byte_size(Data) - Pos - Len),
            {Rest2, Events} = parse_sse(Rest),
            {Rest2, [parse_sse_event(Block) | Events]}
    end.

parse_sse_event(Block) ->
    Lines = binary:split(Block, <<"\n">>, [global]),
    lists:foldl(fun(Line0, Acc) ->
                    Line = string:trim(Line0, trailing, "\r"),
                    case Line of
                        <<"event:", Value/binary>> ->
                            Acc#{event => string:trim(Value)};
                        <<"data:", Value/binary>> ->
                            Data = string:trim(Value),
                            case Acc of
                                #{data := Old} -> Acc#{data => <<Old/binary, "\n", Data/binary>>};
                                _ -> Acc#{data => Data}
                            end;
                        _ ->
                            Acc
                    end
                end, #{}, Lines).

%% @private 从 SSE 事件中提取消息
-spec extract_messages([map()]) -> [binary()].
extract_messages(Events) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{event := <<"message">>, data := Data} ->
                {true, Data};
            #{data := Data} when not is_map_key(event, Event) ->
                {true, Data};
            _ ->
                false
        end
    end, Events).
