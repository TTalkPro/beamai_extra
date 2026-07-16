%%%-------------------------------------------------------------------
%%% @doc MCP SSE 传输实现（Gun 后端）
%%%
%%% 使用 Gun 作为 HTTP 客户端的 MCP SSE 传输。
%%% 支持 HTTP/2，提供更好的流控制。
%%%
%%% == 配置参数 ==
%%%
%%% ```erlang
%%% #{
%%%     transport => sse,
%%%     backend => gun,                              %% 使用 Gun 后端
%%%     url => <<"https://example.com/mcp/sse">>,    %% 必填：SSE 端点
%%%     headers => [{<<"Authorization">>, <<"Bearer token">>}], %% 可选
%%%     timeout => 30000                             %% 可选：连接超时
%%% }
%%% ```
%%%
%%% == SSE 协议 ==
%%%
%%% SSE 连接流程：
%%% 1. 客户端发起 GET 请求到 SSE 端点
%%% 2. 服务器返回 endpoint 事件，包含 POST 端点 URL
%%% 3. 客户端向 POST 端点发送 JSON-RPC 请求
%%% 4. 服务器通过 SSE 流返回响应
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_sse_gun).

-behaviour(beamai_mcp_transport).

%% 行为回调导出
-export([
    connect/1,
    send/2,
    recv/2,
    close/1,
    is_connected/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(sse_gun_state, {
    %% SSE 连接
    sse_conn :: pid() | undefined,
    sse_stream :: reference() | undefined,
    sse_url :: binary(),
    %% POST 端点信息
    post_conn :: pid() | undefined,
    post_url :: binary() | undefined,
    post_host :: binary() | undefined,
    post_port :: pos_integer() | undefined,
    post_path :: binary() | undefined,
    post_transport :: tcp | tls | undefined,
    %% 消息队列
    message_queue = [] :: [binary()],
    %% SSE 解析缓冲区
    buffer = <<>> :: binary(),
    %% 请求头
    headers = [] :: [{binary(), binary()}],
    %% 超时配置
    timeout :: pos_integer()
}).

-type state() :: #sse_gun_state{}.

%%====================================================================
%% 行为回调实现
%%====================================================================

%% @doc 建立 SSE 连接
-spec connect(map()) -> {ok, state()} | {error, term()}.
connect(#{url := Url} = Config) ->
    Headers = maps:get(headers, Config, []),
    Timeout = maps:get(timeout, Config, 30000),

    %% 解析 URL
    case parse_url(Url) of
        {ok, Host, Port, Path, Transport} ->
            %% 建立 Gun 连接
            GunOpts = #{
                connect_timeout => Timeout,
                transport => Transport,
                protocols => [http]
            },
            case gun:open(binary_to_list(Host), Port, GunOpts) of
                {ok, ConnPid} ->
                    case gun:await_up(ConnPid, Timeout) of
                        {ok, _Protocol} ->
                            %% 发起 SSE 请求
                            RequestHeaders = [
                                {<<"accept">>, <<"text/event-stream">>},
                                {<<"cache-control">>, <<"no-cache">>}
                                | Headers
                            ],
                            StreamRef = gun:get(ConnPid, Path, RequestHeaders),

                            State = #sse_gun_state{
                                sse_conn = ConnPid,
                                sse_stream = StreamRef,
                                sse_url = Url,
                                headers = Headers,
                                timeout = Timeout
                            },
                            %% 等待 endpoint 事件
                            wait_for_endpoint(State, Timeout);
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

%% @doc 发送消息
-spec send(binary(), state()) -> {ok, state()} | {error, term()}.
send(_Message, #sse_gun_state{post_url = undefined}) ->
    {error, no_post_endpoint};
send(Message, #sse_gun_state{post_conn = _PostConn, post_path = _PostPath,
                              headers = Headers, timeout = Timeout} = State) ->
    %% 确保 POST 连接存在
    case ensure_post_connection(State) of
        {ok, NewState} ->
            #sse_gun_state{post_conn = Conn, post_path = Path} = NewState,
            RequestHeaders = [
                {<<"content-type">>, <<"application/json">>}
                | Headers
            ],
            StreamRef = gun:post(Conn, Path, RequestHeaders, Message),
            %% 等待响应
            case wait_post_response(Conn, StreamRef, Timeout) of
                ok -> {ok, NewState};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 接收消息
-spec recv(timeout(), state()) -> {ok, binary(), state()} | {error, term()}.
recv(_Timeout, #sse_gun_state{message_queue = [Msg | Rest]} = State) ->
    {ok, Msg, State#sse_gun_state{message_queue = Rest}};
recv(Timeout, #sse_gun_state{sse_conn = Conn, sse_stream = Stream,
                              buffer = Buffer} = State)
  when Conn =/= undefined, Stream =/= undefined ->
    receive_sse_events(Conn, Stream, Buffer, Timeout, State);
recv(_Timeout, _State) ->
    {error, not_connected}.

%% @doc 关闭连接
-spec close(state()) -> ok | {error, term()}.
close(#sse_gun_state{sse_conn = SseConn, post_conn = PostConn}) ->
    case SseConn of
        undefined -> ok;
        _ -> gun:close(SseConn)
    end,
    case PostConn of
        undefined -> ok;
        _ when PostConn =/= SseConn -> gun:close(PostConn);
        _ -> ok
    end,
    ok.

%% @doc 检查连接状态
-spec is_connected(state()) -> boolean().
is_connected(#sse_gun_state{sse_conn = Conn, post_url = PostUrl}) ->
    Conn =/= undefined andalso PostUrl =/= undefined andalso is_process_alive(Conn).

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

%% @private 等待 endpoint 事件
-spec wait_for_endpoint(state(), timeout()) -> {ok, state()} | {error, term()}.
wait_for_endpoint(#sse_gun_state{sse_conn = Conn, sse_stream = Stream,
                                  buffer = Buffer} = State, Timeout) ->
    receive
        {gun_response, Conn, Stream, nofin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            wait_for_endpoint(State, Timeout);
        {gun_response, Conn, Stream, nofin, Status, _Headers} ->
            gun:close(Conn),
            {error, {http_error, Status}};
        {gun_response, Conn, Stream, fin, Status, _Headers} ->
            gun:close(Conn),
            {error, {http_error, Status}};

        {gun_data, Conn, Stream, nofin, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case parse_endpoint_event(NewBuffer) of
                {ok, PostUrl, Rest} ->
                    %% 解析 POST URL：若为相对 URI，基于 SSE URL 解析为绝对 URI
                    AbsoluteUrl = resolve_uri(PostUrl, State#sse_gun_state.sse_url),
                    case parse_url(AbsoluteUrl) of
                        {ok, Host, Port, Path, Transport} ->
                            {ok, State#sse_gun_state{
                                post_url = PostUrl,
                                post_host = Host,
                                post_port = Port,
                                post_path = Path,
                                post_transport = Transport,
                                buffer = Rest
                            }};
                        {error, Reason} ->
                            {error, {invalid_post_url, Reason}}
                    end;
                {need_more, Rest} ->
                    wait_for_endpoint(State#sse_gun_state{buffer = Rest}, Timeout)
            end;
        {gun_data, Conn, Stream, fin, _Data} ->
            {error, connection_closed};

        {gun_error, Conn, Stream, Reason} ->
            {error, {gun_error, Reason}};
        {gun_error, Conn, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:close(Conn),
        {error, timeout}
    end.

%% @private 解析 endpoint 事件
-spec parse_endpoint_event(binary()) -> {ok, binary(), binary()} | {need_more, binary()}.
parse_endpoint_event(Buffer) ->
    {Remaining, Events} = beamai_sse:parse(Buffer),
    case find_endpoint_event(Events) of
        {ok, PostUrl} ->
            {ok, PostUrl, Remaining};
        not_found ->
            {need_more, Remaining}
    end.

%% @private 查找 endpoint 事件
%%
%% endpoint 事件数据可能是：
%% - JSON 格式 {"uri":"..."} / {"url":"..."}（自家服务端）
%% - 原始 URI 字符串（MCP 规范 compliant 服务端，如 /message?sessionId=xxx）
-spec find_endpoint_event([map()]) -> {ok, binary()} | not_found.
find_endpoint_event([]) ->
    not_found;
find_endpoint_event([#{event := <<"endpoint">>, data := Data} | _Rest]) ->
    parse_endpoint_data(Data);
find_endpoint_event([_ | Rest]) ->
    find_endpoint_event(Rest).

%% @private 解析 endpoint 事件数据
%%
%% 先尝试 JSON（自家服务端格式），失败则作为原始 URI 字符串处理（MCP 规范格式）。
-spec parse_endpoint_data(binary()) -> {ok, binary()} | not_found.
parse_endpoint_data(Data) ->
    try jsx:decode(Data, [return_maps]) of
        #{<<"uri">> := Uri} when is_binary(Uri) -> {ok, Uri};
        #{<<"url">> := Url} when is_binary(Url) -> {ok, Url};
        _ -> try_raw_uri(Data)
    catch
        _:_ ->
            %% JSON 解析失败——Data 是原始 URI 字符串（MCP 规范）
            try_raw_uri(Data)
    end.

%% @private 尝试将数据视为原始 URI 字符串
-spec try_raw_uri(binary()) -> {ok, binary()} | not_found.
try_raw_uri(Data) ->
    Trimmed = string:trim(Data),
    case Trimmed of
        <<>> -> not_found;
        _ -> {ok, Trimmed}
    end.

%% @private 解析相对 URI
%%
%% 如果 Uri 是绝对 URI（含 scheme），直接返回。
%% 否则基于 BaseUrl 解析为绝对 URI（RFC 3986）。
-spec resolve_uri(binary(), binary()) -> binary().
resolve_uri(Uri, BaseUrl) ->
    case uri_string:parse(Uri) of
        #{scheme := _} ->
            Uri;
        _ ->
            try
                %% 注意：Erlang 的 uri_string:resolve/2 第一参数为相对 URI，第二参数为 base
                Resolved = uri_string:resolve(Uri, BaseUrl),
                ensure_binary(Resolved)
            catch
                _:_ -> Uri
            end
    end.

%% @private 确保 POST 连接存在
-spec ensure_post_connection(state()) -> {ok, state()} | {error, term()}.
ensure_post_connection(#sse_gun_state{post_conn = Conn} = State)
  when Conn =/= undefined, is_pid(Conn) ->
    case is_process_alive(Conn) of
        true -> {ok, State};
        false -> create_post_connection(State)
    end;
ensure_post_connection(State) ->
    create_post_connection(State).

%% @private 创建 POST 连接
-spec create_post_connection(state()) -> {ok, state()} | {error, term()}.
create_post_connection(#sse_gun_state{post_host = Host, post_port = Port,
                                       post_transport = Transport,
                                       timeout = Timeout} = State) ->
    GunOpts = #{
        connect_timeout => Timeout,
        transport => Transport,
        protocols => [http2, http]
    },
    case gun:open(binary_to_list(Host), Port, GunOpts) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid, Timeout) of
                {ok, _} ->
                    {ok, State#sse_gun_state{post_conn = ConnPid}};
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, {post_connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {post_open_failed, Reason}}
    end.

%% @private 等待 POST 响应
-spec wait_post_response(pid(), reference(), timeout()) -> ok | {error, term()}.
wait_post_response(Conn, StreamRef, Timeout) ->
    receive
        {gun_response, Conn, StreamRef, fin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            ok;
        {gun_response, Conn, StreamRef, fin, Status, _Headers} ->
            {error, {http_error, Status}};
        {gun_response, Conn, StreamRef, nofin, Status, _Headers}
          when Status >= 200, Status < 300 ->
            %% 需要读取 body
            drain_response_body(Conn, StreamRef, Timeout);
        {gun_response, Conn, StreamRef, nofin, Status, _Headers} ->
            drain_response_body(Conn, StreamRef, Timeout),
            {error, {http_error, Status}};
        {gun_error, Conn, StreamRef, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        gun:cancel(Conn, StreamRef),
        {error, timeout}
    end.

%% @private 排空响应 body
-spec drain_response_body(pid(), reference(), timeout()) -> ok.
drain_response_body(Conn, StreamRef, Timeout) ->
    receive
        {gun_data, Conn, StreamRef, fin, _Data} ->
            ok;
        {gun_data, Conn, StreamRef, nofin, _Data} ->
            drain_response_body(Conn, StreamRef, Timeout)
    after Timeout ->
        gun:cancel(Conn, StreamRef),
        ok
    end.

%% @private 接收 SSE 事件
-spec receive_sse_events(pid(), reference(), binary(), timeout(), state()) ->
    {ok, binary(), state()} | {error, term()}.
receive_sse_events(Conn, Stream, Buffer, Timeout, State) ->
    receive
        {gun_data, Conn, Stream, fin, _Data} ->
            {error, connection_closed};
        {gun_data, Conn, Stream, nofin, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Remaining, Events} = beamai_sse:parse(NewBuffer),
            Messages = extract_messages(Events),
            case Messages of
                [Msg | Rest] ->
                    NewQueue = State#sse_gun_state.message_queue ++ Rest,
                    {ok, Msg, State#sse_gun_state{buffer = Remaining, message_queue = NewQueue}};
                [] ->
                    receive_sse_events(Conn, Stream, Remaining, Timeout, State)
            end;
        {gun_error, Conn, Stream, Reason} ->
            {error, {gun_error, Reason}};
        {gun_error, Conn, Reason} ->
            {error, {gun_error, Reason}}
    after Timeout ->
        {error, timeout}
    end.

%% @private 从事件中提取消息
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
