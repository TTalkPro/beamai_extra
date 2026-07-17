%%%-------------------------------------------------------------------
%%% @doc SSE MCP Server 模式端到端测试（真 cowboy + 真 HTTP + 真 SSE 流）
%%%
%%% 历史缺陷：SSE 是双端点会话（GET 建流拿 endpoint URL，POST 发消息、响应
%%% 经流回推），但服务端从不登记会话、endpoint URL 也不带 session id——
%%% handle_sse_message 是个 "TODO: locate SSE session" 存根，只干回 202，
%%% 请求的响应永远回不到客户端。SSE Server 模式等于不工作。
%%%
%%% 现在 GET 起会话级 server + 登记进 registry + endpoint 带 session_id；
%%% POST /message?session_id=X 找回会话、分发、把响应 push 进 SSE 流。
%%% 这里用 gun 建真实 SSE 流、用 httpc POST 真实消息，验证响应确实从流回来。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_sse_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, mcp_sse_test_listener).

sse_server_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     {with, [
        fun endpoint_event_carries_session_id/1,
        fun request_response_flows_over_sse/1,
        fun post_to_unknown_session_is_404/1,
        fun post_without_session_id_is_400/1
     ]}}.

setup() ->
    {ok, _} = application:ensure_all_started(beamai_mcp),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(inets),
    Tools = [beamai_mcp_types:make_tool(
        <<"echo">>, <<"Echo">>,
        #{type => object, properties => #{text => #{type => string}}},
        fun(#{<<"text">> := T}) -> {ok, T} end)],
    Config = #{tools => Tools,
               server_info => #{<<"name">> => <<"sse-test">>, <<"version">> => <<"1">>}},
    Dispatch = cowboy_router:compile([{'_', [
        {"/mcp/sse", beamai_mcp_cowboy_handler, Config#{mode => sse_init}},
        {"/mcp/sse/message", beamai_mcp_cowboy_handler, Config#{mode => sse_message}}
    ]}]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    ranch:get_port(?LISTENER).

cleanup(_Port) ->
    cowboy:stop_listener(?LISTENER),
    ok.

%%====================================================================
%% 用例
%%====================================================================

%% 回归测试：endpoint 事件必须带 session_id，否则 POST 无从自报身份
endpoint_event_carries_session_id(Port) ->
    {Conn, StreamRef} = open_sse(Port),
    {ok, Endpoint, _Buf} = recv_endpoint(Conn, StreamRef, 3000),
    ?assertNotEqual(nomatch, binary:match(Endpoint, <<"session_id=">>)),
    ?assertNotEqual(nomatch, binary:match(Endpoint, <<"/mcp/sse/message">>)),
    gun:close(Conn).

%% 回归测试：完整往返——POST 请求，响应从 SSE 流回来。
%% 会话是有状态的：和真实客户端一样，先 initialize 再调工具。
%%
%% 用累积缓冲区 + 已消费事件数在多次交互间前进，避免把上一条消息当成这一条。
request_response_flows_over_sse(Port) ->
    {Conn, StreamRef} = open_sse(Port),
    {ok, Endpoint, Buf0} = recv_endpoint(Conn, StreamRef, 3000),
    Sid = session_id_of(Endpoint),
    Url = message_url(Port, Sid),

    %% 已消费 1 个事件（endpoint）
    post(Url, rpc(1, <<"initialize">>,
                  #{<<"protocolVersion">> => <<"2025-03-26">>,
                    <<"capabilities">> => #{},
                    <<"clientInfo">> => #{<<"name">> => <<"c">>, <<"version">> => <<"1">>}})),
    {ok, InitResp, Buf1, N1} = recv_nth_message(Conn, StreamRef, Buf0, 1, 3000),
    ?assertMatch(#{<<"result">> := #{<<"serverInfo">> := _}},
                 json:decode(InitResp)),

    post(Url, rpc(2, <<"tools/call">>,
                  #{<<"name">> => <<"echo">>,
                    <<"arguments">> => #{<<"text">> => <<"hi over sse">>}})),
    {ok, Response, _Buf2, _N2} = recv_nth_message(Conn, StreamRef, Buf1, N1, 3000),
    #{<<"result">> := #{<<"content">> := [C], <<"isError">> := false}} =
        json:decode(Response),
    ?assertEqual(<<"hi over sse">>, maps:get(<<"text">>, C)),
    gun:close(Conn).

post_to_unknown_session_is_404(Port) ->
    Url = message_url(Port, <<"mcp-nope">>),
    Msg = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                       <<"method">> => <<"tools/list">>, <<"params">> => #{}}),
    {ok, {{_, Status, _}, _, _}} =
        httpc:request(post, {Url, [], "application/json", Msg}, [], []),
    ?assertEqual(404, Status).

post_without_session_id_is_400(Port) ->
    Url = lists:flatten(io_lib:format("http://127.0.0.1:~p/mcp/sse/message", [Port])),
    Msg = beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                       <<"method">> => <<"tools/list">>, <<"params">> => #{}}),
    {ok, {{_, Status, _}, _, _}} =
        httpc:request(post, {Url, [], "application/json", Msg}, [], []),
    ?assertEqual(400, Status).

%%====================================================================
%% 辅助（gun 客户端驱动 SSE 流）
%%====================================================================

open_sse(Port) ->
    {ok, Conn} = gun:open("127.0.0.1", Port),
    {ok, _} = gun:await_up(Conn, 3000),
    StreamRef = gun:get(Conn, "/mcp/sse", [{<<"accept">>, <<"text/event-stream">>}]),
    {Conn, StreamRef}.

%% 收流直到第 1 个 endpoint 事件；返回 {ok, Uri, 累积缓冲}
recv_endpoint(Conn, StreamRef, Timeout) ->
    case recv_nth(Conn, StreamRef, <<>>, 0, Timeout) of
        {ok, #{event := <<"endpoint">>, data := Data}, Buf, _N} ->
            #{<<"uri">> := Uri} = json:decode(Data),
            {ok, Uri, Buf};
        Other -> Other
    end.

%% 取第 Skip 个之后的下一个 message 事件；返回 {ok, Data, 缓冲, 新已消费数}
recv_nth_message(Conn, StreamRef, Buffer, Skip, Timeout) ->
    case recv_nth(Conn, StreamRef, Buffer, Skip, Timeout) of
        {ok, #{event := <<"message">>, data := Data}, Buf, N} ->
            {ok, Data, Buf, N};
        {ok, _Other, Buf, N} ->
            %% 跳过非 message 事件，继续
            recv_nth_message(Conn, StreamRef, Buf, N, Timeout);
        Other -> Other
    end.

%% 累积 gun data，用 beamai_sse:parse 全量解析，返回第 (Skip+1) 个事件。
%% Skip 是"已消费事件数"——多次交互间靠它前进，避免重读旧事件。
recv_nth(Conn, StreamRef, Buffer, Skip, Timeout) ->
    {_Remaining, Events} = beamai_sse:parse(Buffer),
    case length(Events) > Skip of
        true ->
            {ok, lists:nth(Skip + 1, Events), Buffer, Skip + 1};
        false ->
            receive
                {gun_response, Conn, StreamRef, nofin, _S, _H} ->
                    recv_nth(Conn, StreamRef, Buffer, Skip, Timeout);
                {gun_data, Conn, StreamRef, _Fin, Data} ->
                    recv_nth(Conn, StreamRef, <<Buffer/binary, Data/binary>>, Skip, Timeout)
            after Timeout ->
                {error, {timeout_waiting_for_event, Skip + 1}}
            end
    end.

session_id_of(Endpoint) ->
    Params = uri_string:dissect_query(query_of(Endpoint)),
    {_, Sid} = lists:keyfind(<<"session_id">>, 1, Params),
    Sid.

query_of(Uri) ->
    case binary:split(Uri, <<"?">>) of
        [_, Q] -> Q;
        [_] -> <<>>
    end.

message_url(Port, Sid) ->
    lists:flatten(io_lib:format("http://127.0.0.1:~p/mcp/sse/message?session_id=~s",
                                [Port, binary_to_list(Sid)])).

rpc(Id, Method, Params) ->
    beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                 <<"method">> => Method, <<"params">> => Params}).

%% POST 一条消息到 message 端点，断言 202 Accepted
post(Url, Body) ->
    {ok, {{_, 202, _}, _, _}} =
        httpc:request(post, {Url, [], "application/json", Body}, [], []),
    ok.
