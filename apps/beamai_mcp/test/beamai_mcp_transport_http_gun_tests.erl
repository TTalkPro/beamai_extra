%%%-------------------------------------------------------------------
%%% @doc Streamable HTTP 传输层回归测试（真 cowboy + 真 Gun）。
%%%
%%% 锁定 2026-10-26 下游反馈：非 2xx body 被丢弃、超过 max_body 的响应
%%% 只慢不报、initialize 后缺少 mcp-protocol-version 请求头。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_transport_http_gun_tests).

-include_lib("eunit/include/eunit.hrl").

http_transport_test_() ->
    {setup, fun() -> start(raw_unauthorized) end, fun stop_listener/1,
     {with, [fun http_error_includes_body/1]}}.

large_response_test_() ->
    {setup, fun() -> start(raw_large) end, fun stop_listener/1,
     {with, [fun oversized_response_fails_fast/1]}}.

protocol_header_test_() ->
    {setup, fun() -> start(raw_protocol) end, fun stop_listener/1,
     {with, [fun requests_after_initialize_carry_protocol_version/1]}}.

http_error_includes_body({_Mode, Url}) ->
    {ok, Pid} = client(Url),
    ?assertEqual(
       {error, {connection_failed, {http_error, 401, <<"unauthorized-body">>}}},
       beamai_mcp_client:await_connected(Pid, 3000)),
    beamai_mcp_client:stop(Pid).

oversized_response_fails_fast({_Mode, Url}) ->
    {ok, Pid} = client(Url),
    ?assertEqual({error, {connection_failed, response_too_large}},
                 beamai_mcp_client:await_connected(Pid, 3000)),
    beamai_mcp_client:stop(Pid).

requests_after_initialize_carry_protocol_version({_Mode, Url}) ->
    {ok, Pid} = client(Url),
    ok = beamai_mcp_client:await_connected(Pid, 3000),
    ?assertMatch({ok, #{<<"tools">> := [_]}},
                 beamai_mcp_client:list_tools(Pid)),
    beamai_mcp_client:stop(Pid).

start(Mode) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    Name = listener_name(Mode),
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/mcp", beamai_mcp_http_probe_handler, Mode}]}]),
    {ok, _} = cowboy:start_clear(Name, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    Port = ranch:get_port(Name),
    Url = unicode:characters_to_binary(
            io_lib:format("http://127.0.0.1:~p/mcp", [Port])),
    {Mode, Url}.

stop_listener({Mode, _Url}) ->
    cowboy:stop_listener(listener_name(Mode)),
    ok.

listener_name(raw_unauthorized) -> mcp_http_unauthorized_listener;
listener_name(raw_large) -> mcp_http_large_listener;
listener_name(raw_protocol) -> mcp_http_protocol_listener.

client(Url) ->
    beamai_mcp_client:start_link(#{
        transport => http,
        url => Url,
        timeout => 2000,
        init_timeout => 2000,
        max_body => 1024
    }).
