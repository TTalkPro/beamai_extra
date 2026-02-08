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
    is_connected/1
]).

%% 额外 API
-export([
    request/2,
    request/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(http_gun_state, {
    %% Gun 连接
    conn_pid :: pid() | undefined,
    %% 服务器信息
    host :: binary(),
    port :: pos_integer(),
    path :: binary(),
    transport :: tcp | tls,
    %% 请求头
    headers = [] :: [{binary(), binary()}],
    timeout :: pos_integer(),
    session_id :: binary() | undefined,
    %% 当前请求的流引用
    stream_ref :: reference() | undefined,
    %% SSE 解析缓冲区
    buffer = <<>> :: binary(),
    %% 消息队列
    message_queue = [] :: [binary()],
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

    %% 解析 URL
    case parse_url(Url) of
        {ok, Host, Port, Path, Transport} ->
            %% 建立 Gun 连接
            GunOpts = #{
                connect_timeout => Timeout,
                transport => Transport,
                protocols => [http2, http]
            },
            case gun:open(binary_to_list(Host), Port, GunOpts) of
                {ok, ConnPid} ->
                    %% 等待连接就绪
                    case gun:await_up(ConnPid, Timeout) of
                        {ok, _Protocol} ->
                            State = #http_gun_state{
                                conn_pid = ConnPid,
                                host = Host,
                                port = Port,
                                path = Path,
                                transport = Transport,
                                headers = Headers,
                                timeout = Timeout,
                                session_id = SessionId,
                                connected = true
                            },
                            {ok, State};
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, {connection_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {open_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
connect(_) ->
    {error, missing_url}.

%% @doc 发送请求
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(Message, #http_gun_state{conn_pid = ConnPid, path = Path,
                               headers = Headers, session_id = SessionId} = State) ->
    %% 构建请求头
    RequestHeaders = build_request_headers(Headers, SessionId),

    %% 发送 POST 请求
    StreamRef = gun:post(ConnPid, Path, RequestHeaders, Message),
    {ok, State#http_gun_state{stream_ref = StreamRef, buffer = <<>>, message_queue = []}}.

%% @doc 接收响应
-spec recv(timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
recv(_Timeout, #http_gun_state{message_queue = [Msg | Rest]} = State) ->
    %% 队列中有消息
    {ok, Msg, State#http_gun_state{message_queue = Rest}};
recv(_Timeout, #http_gun_state{stream_ref = undefined}) ->
    %% 没有活跃的请求
    {error, no_pending_request};
recv(Timeout, #http_gun_state{conn_pid = ConnPid, stream_ref = StreamRef} = State) ->
    receive_response(ConnPid, StreamRef, Timeout, State).

%% @doc 关闭连接
-spec close(state()) -> ok | {error, term()}.
close(#http_gun_state{conn_pid = ConnPid}) when ConnPid =/= undefined ->
    gun:close(ConnPid),
    ok;
close(_) ->
    ok.

%% @doc 检查连接状态
-spec is_connected(state()) -> boolean().
is_connected(#http_gun_state{connected = Connected, conn_pid = ConnPid}) ->
    Connected andalso is_process_alive(ConnPid).

%%====================================================================
%% 额外 API
%%====================================================================

%% @doc 发送请求并同步等待响应
-spec request(binary(), state()) -> {ok, binary(), state()} | {error, term()}.
request(Message, State) ->
    request(Message, State#http_gun_state.timeout, State).

%% @doc 发送请求并同步等待响应（指定超时）
-spec request(binary(), timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
request(Message, Timeout, State) ->
    case send(Message, State) of
        {ok, NewState} ->
            recv(Timeout, NewState);
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析 URL
-spec parse_url(binary()) -> {ok, binary(), pos_integer(), binary(), tcp | tls} | {error, term()}.
parse_url(Url) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} = Parsed ->
            Port = maps:get(port, Parsed, default_port(Scheme)),
            Path = maps:get(path, Parsed, <<"/">>),
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
-spec build_request_headers([{binary(), binary()}], binary() | undefined) ->
    [{binary(), binary()}].
build_request_headers(Headers, SessionId) ->
    BaseHeaders = [
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json, text/event-stream">>}
        | Headers
    ],
    case SessionId of
        undefined -> BaseHeaders;
        _ -> [{<<"mcp-session-id">>, SessionId} | BaseHeaders]
    end.

%% @private 接收响应
-spec receive_response(pid(), reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_response(ConnPid, StreamRef, Timeout, State) ->
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            %% 无 body 的成功响应
            {ok, <<>>, State#http_gun_state{stream_ref = undefined}};
        {gun_response, ConnPid, StreamRef, fin, Status, _Headers} ->
            {error, {http_error, Status}};

        {gun_response, ConnPid, StreamRef, nofin, Status, Headers}
          when Status >= 200, Status < 300 ->
            %% 检查 Content-Type 和 Session-Id
            ContentType = get_header(Headers, <<"content-type">>),
            NewSessionId = get_header(Headers, <<"mcp-session-id">>),
            NewState = case NewSessionId of
                undefined -> State;
                _ -> State#http_gun_state{session_id = NewSessionId}
            end,
            %% 根据 Content-Type 处理响应
            case is_sse_content_type(ContentType) of
                true ->
                    receive_sse_response(ConnPid, StreamRef, Timeout, NewState);
                false ->
                    receive_json_response(ConnPid, StreamRef, Timeout, NewState)
            end;
        {gun_response, ConnPid, StreamRef, nofin, Status, _Headers} ->
            {error, {http_error, Status}};

        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}};
        {gun_error, ConnPid, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(ConnPid, StreamRef),
        {error, timeout}
    end.

%% @private 接收 JSON 响应
-spec receive_json_response(pid(), reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_json_response(ConnPid, StreamRef, Timeout, State) ->
    receive_json_body(ConnPid, StreamRef, <<>>, Timeout, State).

-spec receive_json_body(pid(), reference(), binary(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_json_body(ConnPid, StreamRef, Acc, Timeout, State) ->
    receive
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            {ok, <<Acc/binary, Data/binary>>, State#http_gun_state{stream_ref = undefined}};
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_json_body(ConnPid, StreamRef, <<Acc/binary, Data/binary>>, Timeout, State);
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(ConnPid, StreamRef),
        {error, timeout}
    end.

%% @private 接收 SSE 响应
-spec receive_sse_response(pid(), reference(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_sse_response(ConnPid, StreamRef, Timeout, #http_gun_state{buffer = Buffer} = State) ->
    receive
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            %% 处理剩余数据
            FinalBuffer = <<Buffer/binary, Data/binary>>,
            {_Remaining, Events} = beamai_sse:parse(FinalBuffer),
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    {ok, Msg, State#http_gun_state{
                        stream_ref = undefined,
                        buffer = <<>>,
                        message_queue = Rest
                    }};
                [] ->
                    {error, no_message}
            end;
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Remaining, Events} = beamai_sse:parse(NewBuffer),
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    {ok, Msg, State#http_gun_state{
                        buffer = Remaining,
                        message_queue = Rest
                    }};
                [] ->
                    receive_sse_response(ConnPid, StreamRef, Timeout,
                                         State#http_gun_state{buffer = Remaining})
            end;
        {gun_error, ConnPid, StreamRef, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(ConnPid, StreamRef),
        {error, timeout}
    end.

%% @private 获取响应头
-spec get_header([{binary(), binary()}], binary()) -> binary() | undefined.
get_header(Headers, Name) ->
    LowerName = string:lowercase(Name),
    case lists:keyfind(LowerName, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, Value} -> Value;
        false -> undefined
    end.

%% @private 检查是否为 SSE Content-Type
-spec is_sse_content_type(binary() | undefined) -> boolean().
is_sse_content_type(undefined) -> false;
is_sse_content_type(ContentType) ->
    binary:match(ContentType, <<"text/event-stream">>) =/= nomatch.

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
