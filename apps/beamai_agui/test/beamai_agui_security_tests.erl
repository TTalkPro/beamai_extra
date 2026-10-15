%%%-------------------------------------------------------------------
%%% @doc Authentication / rate limiting: middleware behaviour and its Cowboy
%%% integration, including CORS on rejected requests.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_security_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, beamai_agui_security_listener).

security_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Ctx) ->
        [{"disabled gates let requests through", fun() -> disabled_bypass() end},
         {"enabled auth rejects a missing key", fun() -> missing_key() end},
         {"enabled auth rejects an unknown key", fun() -> invalid_key() end},
         {"bearer and X-API-Key both authenticate", fun() -> valid_schemes() end},
         {"expired key is rejected", fun() -> expired_key() end},
         {"rate limit is per authenticated key",
          fun() -> per_key_rate_limit() end},
         {"anonymous clients are identified by proxy headers",
          fun() -> anonymous_rate_limit() end},
         {"Cowboy rejects without a key and still returns CORS",
          fun() -> cowboy_missing_key(Ctx) end},
         {"Cowboy accepts a valid key", fun() -> cowboy_valid_key(Ctx) end}]
     end}.

setup() ->
    {ok, _} = ensure_started(beamai_agui_auth, #{}),
    {ok, _} = ensure_started(beamai_agui_rate_limit, #{}),
    ok = beamai_agui_auth:set_config(#{enabled => true}),
    {ok, _} = beamai_agui_auth:add_key(<<"key-a">>,
                                       #{name => <<"A">>, permissions => [read]}),
    {ok, _} = beamai_agui_auth:add_key(<<"key-b">>, #{}),
    {ok, _} = beamai_agui_auth:add_key(<<"expired">>,
                                       #{expires_at => 1}),
    ok = beamai_agui_rate_limit:set_config(#{enabled => true,
                                             requests_per_minute => 1,
                                             requests_per_hour => 100}),
    ok = beamai_agui_rate_limit:reset_all(),

    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, Registry} = beamai_agui_registry:start_link(),
    ok = beamai_agui_registry:register(<<"echo">>, factory()),
    Dispatch = cowboy_router:compile([
        {'_', [{"/agui/info", beamai_agui_cowboy_handler, #{mode => info}}]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    #{registry => Registry, port => ranch:get_port(?LISTENER)}.

cleanup(#{registry := Registry}) ->
    ok = cowboy:stop_listener(?LISTENER),
    unlink(Registry),
    MRef = erlang:monitor(process, Registry),
    exit(Registry, shutdown),
    receive {'DOWN', MRef, process, Registry, _} -> ok
    after 1000 -> erlang:demonitor(MRef, [flush]) end,
    _ = beamai_agui_auth:remove_key(<<"key-a">>),
    _ = beamai_agui_auth:remove_key(<<"key-b">>),
    _ = beamai_agui_auth:remove_key(<<"expired">>),
    _ = beamai_agui_auth:set_config(#{enabled => false}),
    _ = beamai_agui_rate_limit:remove_rule(<<"ip:203.0.113.9">>),
    _ = beamai_agui_rate_limit:set_config(#{enabled => false}),
    _ = beamai_agui_rate_limit:reset_all(),
    ok.

ensure_started(Module, Config) ->
    case whereis(Module) of
        undefined -> Module:start_link(Config);
        Pid -> {ok, Pid}
    end.

disabled_bypass() ->
    set_gates(false, false),
    {ok, Security} = beamai_agui_middleware:check([]),
    ?assertEqual(false, maps:get(authentication, Security)),
    ?assertEqual(false, maps:get(rate_limit, Security)).

missing_key() ->
    set_gates(true, false),
    {error, 401, <<"auth_required">>, _, []} =
        beamai_agui_middleware:check([]).

invalid_key() ->
    set_gates(true, false),
    Headers = [{<<"authorization">>, <<"Bearer no-such-key">>}],
    {error, 401, <<"auth_failed">>, _, []} =
        beamai_agui_middleware:check(Headers).

valid_schemes() ->
    set_gates(true, false),
    {ok, Bearer} = beamai_agui_middleware:check(
                     [{<<"authorization">>, <<"Bearer key-a">>}]),
    {ok, HeaderKey} = beamai_agui_middleware:check(
                        [{<<"x-api-key">>, <<"key-a">>}]),
    ?assertEqual(true, maps:get(authentication, Bearer)),
    ?assertEqual(true, maps:get(authentication, HeaderKey)).

expired_key() ->
    set_gates(true, false),
    Headers = [{<<"authorization">>, <<"Bearer expired">>}],
    {error, 401, <<"auth_expired">>, _, []} =
        beamai_agui_middleware:check(Headers).

per_key_rate_limit() ->
    set_gates(true, true),
    {ok, _} = beamai_agui_middleware:check(
                 [{<<"authorization">>, <<"Bearer key-a">>}]),
    {error, 429, <<"rate_limited">>, _, Headers} =
        beamai_agui_middleware:check(
          [{<<"authorization">>, <<"Bearer key-a">>}]),
    ?assertMatch({_, _}, lists:keyfind(<<"retry-after">>, 1, Headers)),
    %% 另一个 key 有自己的桶，不会因 key-a 已用尽而被牵连
    {ok, _} = beamai_agui_middleware:check(
                 [{<<"authorization">>, <<"Bearer key-b">>}]).

anonymous_rate_limit() ->
    set_gates(false, true),
    Headers = [{<<"x-forwarded-for">>, <<"203.0.113.9, 70.41.3.18">>}],
    {ok, _} = beamai_agui_middleware:check(Headers),
    {error, 429, <<"rate_limited">>, _, _} = beamai_agui_middleware:check(Headers).

cowboy_missing_key(#{port := Port}) ->
    set_gates(true, false),
    {401, Headers, Data} = request(Port, <<"GET">>, <<"/agui/info">>, [], <<>>),
    ?assertEqual(<<"*">>, proplists:get_value(<<"access-control-allow-origin">>, Headers)),
    Json = beamai_utils:parse_json(Data),
    ?assertEqual(<<"auth_required">>, maps:get(<<"error">>, Json)).

cowboy_valid_key(#{port := Port}) ->
    set_gates(true, true),
    ok = beamai_agui_rate_limit:reset_all(),
    Headers = [{<<"authorization">>, <<"Bearer key-b">>}],
    {200, _RespHeaders, Data} = request(Port, <<"GET">>, <<"/agui/info">>, Headers, <<>>),
    ?assertEqual(<<"beamai_agui">>,
                 maps:get(<<"runtime">>, beamai_utils:parse_json(Data))).

set_gates(Auth, Rate) ->
    ok = beamai_agui_auth:set_config(#{enabled => Auth}),
    ok = beamai_agui_rate_limit:set_config(#{enabled => Rate}).

factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

request(Port, Method, Path, Headers, Body) ->
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    try
        Ref = gun:request(Conn, Method, Path, Headers, Body),
        case gun:await(Conn, Ref, 10000) of
            {response, fin, Status, RespHeaders} ->
                {Status, RespHeaders, <<>>};
            {response, nofin, Status, RespHeaders} ->
                {ok, Data} = gun:await_body(Conn, Ref, 10000),
                {Status, RespHeaders, Data}
        end
    after
        gun:close(Conn)
    end.
