%%%-------------------------------------------------------------------
%%% @doc Streamable HTTP MCP Server 端到端测试（真 cowboy + 真 HTTP）
%%%
%%% 历史缺陷：cowboy 适配层每个 POST 新建一个 beamai_mcp_server、用完即停，
%%% mcp-session-id 读了也回显了但从没用它查过 session——initialize 之后的
%%% 一切方法都打到全新 server 上，一律 -32600 "Server not initialized"。
%%% 服务端一辈子只能回答一个方法，MCP Server 能力等于不存在。
%%%
%%% 现在 session 经 beamai_mcp_session_registry 跨请求存活。这里起真实
%%% cowboy 监听、用 httpc 发真实 HTTP，锁住完整会话流程与规范语义
%%% （404 未知会话 / 400 无会话非 initialize / DELETE 终止）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_streamable_http_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, mcp_test_listener).

streamable_http_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     {with, [
        fun full_session_round_trip/1,
        fun unknown_session_is_404/1,
        fun missing_session_non_initialize_is_400/1,
        fun delete_ends_session/1,
        fun sessions_are_isolated/1,
        fun bad_tool_return_does_not_kill_session/1,
        fun subscribe_without_uri_does_not_kill_session/1
     ]}}.

setup() ->
    {ok, _} = application:ensure_all_started(beamai_mcp),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    Tools = [
        beamai_mcp_types:make_tool(
            <<"echo">>, <<"Echo the input">>,
            #{type => object, properties => #{text => #{type => string}}},
            fun(#{<<"text">> := Text}) -> {ok, Text} end),
        %% 返回裸值的坏工具：回归 try_clause 崩 server 的缺陷
        beamai_mcp_types:make_tool(
            <<"bad_return">>, <<"Returns a bare value">>,
            #{type => object, properties => #{}},
            fun(_) -> not_a_tagged_tuple end)
    ],
    Config = #{tools => Tools,
               server_info => #{<<"name">> => <<"test-server">>, <<"version">> => <<"1">>}},
    Dispatch = cowboy_router:compile([{'_', [{"/mcp", beamai_mcp_cowboy_handler, Config}]}]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    Port = ranch:get_port(?LISTENER),
    lists:flatten(io_lib:format("http://127.0.0.1:~p/mcp", [Port])).

cleanup(_Url) ->
    cowboy:stop_listener(?LISTENER),
    ok.

%%====================================================================
%% 用例
%%====================================================================

%% 回归测试：以前第二个请求必然 -32600 "Server not initialized"
full_session_round_trip(Url) ->
    {200, Hdrs, Body} = post(Url, [], initialize_body()),
    Sid = proplists:get_value("mcp-session-id", Hdrs),
    ?assertNotEqual(undefined, Sid),
    ?assertMatch(#{<<"result">> := #{<<"serverInfo">> := _}}, json:decode(Body)),

    %% 同一会话：tools/list
    {200, _, Body2} = post(Url, [{"mcp-session-id", Sid}], rpc(2, <<"tools/list">>, #{})),
    #{<<"result">> := #{<<"tools">> := ToolList}} = json:decode(Body2),
    ?assertEqual([<<"bad_return">>, <<"echo">>],
                 lists:sort([maps:get(<<"name">>, T) || T <- ToolList])),

    %% 同一会话：tools/call 真的执行
    {200, _, Body3} = post(Url, [{"mcp-session-id", Sid}],
                           rpc(3, <<"tools/call">>,
                               #{<<"name">> => <<"echo">>,
                                 <<"arguments">> => #{<<"text">> => <<"你好"/utf8>>}})),
    #{<<"result">> := #{<<"content">> := [C], <<"isError">> := false}} =
        json:decode(Body3),
    ?assertEqual(<<"你好"/utf8>>, maps:get(<<"text">>, C)).

%% 规范：未知会话回 404，客户端据此重新 initialize
unknown_session_is_404(Url) ->
    {Status, _, _} = post(Url, [{"mcp-session-id", "mcp-deadbeef"}],
                          rpc(1, <<"tools/list">>, #{})),
    ?assertEqual(404, Status).

%% 无 session 头且不是 initialize：400
missing_session_non_initialize_is_400(Url) ->
    {Status, _, _} = post(Url, [], rpc(1, <<"tools/list">>, #{})),
    ?assertEqual(400, Status).

%% DELETE 终止会话；之后同一 id 就是 404
delete_ends_session(Url) ->
    {200, Hdrs, _} = post(Url, [], initialize_body()),
    Sid = proplists:get_value("mcp-session-id", Hdrs),
    {ok, {{_, 204, _}, _, _}} =
        httpc:request(delete, {Url, [{"mcp-session-id", Sid}]}, [], []),
    {Status, _, _} = post(Url, [{"mcp-session-id", Sid}], rpc(9, <<"tools/list">>, #{})),
    ?assertEqual(404, Status).

%% 两个并发会话互不串线：删掉一个，另一个照常工作
sessions_are_isolated(Url) ->
    {200, H1, _} = post(Url, [], initialize_body()),
    {200, H2, _} = post(Url, [], initialize_body()),
    Sid1 = proplists:get_value("mcp-session-id", H1),
    Sid2 = proplists:get_value("mcp-session-id", H2),
    ?assertNotEqual(Sid1, Sid2),
    {ok, {{_, 204, _}, _, _}} =
        httpc:request(delete, {Url, [{"mcp-session-id", Sid1}]}, [], []),
    {Status, _, Body} = post(Url, [{"mcp-session-id", Sid2}], rpc(5, <<"tools/list">>, #{})),
    ?assertEqual(200, Status),
    ?assertMatch(#{<<"result">> := _}, json:decode(Body)).

%% 回归测试：工具返回裸值曾致 try_clause 崩掉 server（catch 接不住 of 分支）。
%% 现在应归一为 isError:true，且会话继续可用。
bad_tool_return_does_not_kill_session(Url) ->
    {200, Hdrs, _} = post(Url, [], initialize_body()),
    Sid = proplists:get_value("mcp-session-id", Hdrs),
    {200, _, Body} = post(Url, [{"mcp-session-id", Sid}],
                          rpc(2, <<"tools/call">>,
                              #{<<"name">> => <<"bad_return">>, <<"arguments">> => #{}})),
    #{<<"result">> := #{<<"isError">> := IsErr}} = json:decode(Body),
    ?assertEqual(true, IsErr),
    %% 会话没死
    {Status, _, _} = post(Url, [{"mcp-session-id", Sid}], rpc(3, <<"tools/list">>, #{})),
    ?assertEqual(200, Status).

%% 回归测试：resources/subscribe 缺 uri 曾 function_clause 崩掉 server + Cowboy 进程。
%% 现在应回 JSON-RPC invalid params，且会话继续可用。
subscribe_without_uri_does_not_kill_session(Url) ->
    {200, Hdrs, _} = post(Url, [], initialize_body()),
    Sid = proplists:get_value("mcp-session-id", Hdrs),
    {200, _, Body} = post(Url, [{"mcp-session-id", Sid}],
                          rpc(2, <<"resources/subscribe">>, #{})),
    #{<<"error">> := #{<<"code">> := Code}} = json:decode(Body),
    ?assertEqual(-32602, Code),
    {Status, _, _} = post(Url, [{"mcp-session-id", Sid}], rpc(3, <<"tools/list">>, #{})),
    ?assertEqual(200, Status).

%%====================================================================
%% 辅助
%%====================================================================

initialize_body() ->
    rpc(1, <<"initialize">>,
        #{<<"protocolVersion">> => <<"2025-03-26">>,
          <<"capabilities">> => #{},
          <<"clientInfo">> => #{<<"name">> => <<"test-client">>, <<"version">> => <<"1">>}}).

rpc(Id, Method, Params) ->
    beamai_utils:encode_json(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
                 <<"method">> => Method, <<"params">> => Params}).

post(Url, Headers, Body) ->
    {ok, {{_, Status, _}, RespHeaders, RespBody}} =
        httpc:request(post, {Url, Headers, "application/json", Body},
                      [], [{body_format, binary}]),
    {Status, RespHeaders, RespBody}.
