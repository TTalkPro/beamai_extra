%%%-------------------------------------------------------------------
%%% @doc HTTP 层集成测试：真起 cowboy、真发请求、真读 SSE 帧
%%%
%%% 其余用例都直接调 beamai_agui_runner:run/3，绕开了适配器 —— 这个模块是唯一
%%% 会执行到 stream_reply / stream_body / forward 循环的地方。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_cowboy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, beamai_agui_test_listener).

%%====================================================================
%% 夹具
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, Registry} = beamai_agui_registry:start_link(),
    ok = beamai_agui_registry:register(<<"echo">>, factory()),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/agui/run", beamai_agui_cowboy_handler, #{agent => <<"echo">>}},
            {"/agui/open", beamai_agui_cowboy_handler, #{}},
            %% 按 agent 寻址那条：标准客户端照 `/info` 的字典拼出来的就是它
            {"/agui/agent/:agent/run", beamai_agui_cowboy_handler, #{}},
            {"/agui/info", beamai_agui_cowboy_handler, #{mode => info}},
            %% 允许列表模式（缺省是 `*`，见 cors_* 用例）
            {"/agui/scoped", beamai_agui_cowboy_handler,
             #{agent => <<"echo">>,
               cors_origins => [<<"http://localhost:3002">>, <<"http://localhost:3100">>]}}
        ]}
    ]),
    %% 端口 0：由内核挑一个空闲端口，避免测试之间抢端口
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    #{registry => Registry, port => ranch:get_port(?LISTENER)}.

cleanup(#{registry := Registry}) ->
    ok = cowboy:stop_listener(?LISTENER),
    unlink(Registry),
    MRef = erlang:monitor(process, Registry),
    exit(Registry, shutdown),
    receive {'DOWN', MRef, process, Registry, _} -> ok
    after 1000 -> erlang:demonitor(MRef, [flush]), ok
    end.

cowboy_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Ctx) ->
        [{"SSE 流式响应", fun() -> sse_stream(Ctx) end},
         {"info 端点", fun() -> info_endpoint(Ctx) end},
         {"未知 agent → 404，候选摊成字段", fun() -> unknown_agent(Ctx) end},
         {"`/agent/default/run` → 落到唯一那个 agent", fun() -> default_agent(Ctx) end},
         {"两个 agent 时 `default` → 404（不猜）", fun() -> default_ambiguous(Ctx) end},
         {"手抖的名字不回落 → 404", fun() -> typo_not_aliased(Ctx) end},
         {"畸形请求体 → 400", fun() -> bad_body(Ctx) end},
         {"方法不允许 → 405", fun() -> method_not_allowed(Ctx) end},
         {"OPTIONS 预检 → 204 + CORS", fun() -> options_preflight(Ctx) end},
         {"缺省 CORS 是 `*`，不带凭据位", fun() -> cors_wildcard(Ctx) end},
         {"允许列表命中 → 回显 Origin + 凭据位", fun() -> cors_allowlist_hit(Ctx) end},
         {"允许列表没命中 → 不发 allow-origin", fun() -> cors_allowlist_miss(Ctx) end}]
     end}.

%%====================================================================
%% 被测 agent
%%====================================================================

factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

mock_llm() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            _ = [TokenCb(T, #{}) || T <- [<<"he">>, <<"llo">>]],
            {ok, beamai_chat_response:new(
                #{content => <<"hello">>, finish_reason => <<"stop">>})}
        end),
    ok.

%%====================================================================
%% 用例
%%====================================================================

%% 完整往返：200 + text/event-stream + 逐帧的 AG-UI 事件
sse_stream(#{port := Port}) ->
    ok = mock_llm(),
    try
        Body = beamai_utils:encode_json(
                 #{<<"threadId">> => <<"t-http">>,
                   <<"messages">> => [#{<<"role">> => <<"user">>,
                                        <<"content">> => <<"hi">>}]}),
        {Status, Headers, Data} = post(Port, "/agui/run", Body),
        ?assertEqual(200, Status),
        ?assertEqual(<<"text/event-stream">>, header(<<"content-type">>, Headers)),
        %% nginx 会缓冲上游响应，缓冲了就不是流式了
        ?assertEqual(<<"no">>, header(<<"x-accel-buffering">>, Headers)),
        Events = parse_sse(Data),
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"STEP_STARTED">>,
                      <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                      <<"TEXT_MESSAGE_CONTENT">>,
                      <<"TEXT_MESSAGE_END">>, <<"STEP_FINISHED">>,
                      <<"RUN_FINISHED">>],
                     [maps:get(<<"type">>, E) || E <- Events]),
        %% threadId 原样带回，前端据此续上后续请求
        [Started | _] = Events,
        ?assertEqual(<<"t-http">>, maps:get(<<"threadId">>, Started))
    after
        meck:unload(beamai_chat_model)
    end.

info_endpoint(#{port := Port}) ->
    {Status, Headers, Data} = get(Port, "/agui/info"),
    ?assertEqual(200, Status),
    ?assertEqual(<<"application/json">>, header(<<"content-type">>, Headers)),
    Json = beamai_utils:parse_json(Data),
    ?assertEqual(<<"beamai_agui">>, maps:get(<<"runtime">>, Json)),
    %% agents 是**以 id 为键的字典**，不是数组 —— 给数组前端会把下标当 agent id
    Agents = maps:get(<<"agents">>, Json),
    ?assert(is_map(Agents)),
    ?assertEqual([<<"echo">>], maps:keys(Agents)),
    Echo = maps:get(<<"echo">>, Agents),
    ?assertEqual(<<"echo">>, maps:get(<<"name">>, Echo)),
    %% 能力位：走 AG-UI 的 interrupt 协议、逐 token 出、认前端工具、有共享状态
    Caps = maps:get(<<"capabilities">>, Echo),
    ?assertEqual(true, maps:get(<<"interrupts">>, maps:get(<<"humanInTheLoop">>, Caps))),
    ?assertEqual(true, maps:get(<<"streaming">>, maps:get(<<"transport">>, Caps))),
    ?assertEqual(true, maps:get(<<"clientProvided">>, maps:get(<<"tools">>, Caps))),
    ?assertEqual(true, maps:get(<<"deltas">>, maps:get(<<"state">>, Caps))),
    %% 不谎报：三个插件位缺省全 false
    ?assertEqual(false, maps:get(<<"suggestions">>, Json)),
    ?assertEqual(false, maps:get(<<"openGenerativeUIEnabled">>, Json)),
    ?assertEqual(false, maps:get(<<"a2uiEnabled">>, Json)),
    %% threadEndpoints **无论开关开关这个键都发**：省略与"明确说没有"在客户端
    %% 那儿是两码事（本例没起会话层，故如实全报 false）
    Endpoints = maps:get(<<"threadEndpoints">>, Json),
    ?assertEqual(false, maps:get(<<"list">>, Endpoints)),
    ?assertEqual(false, maps:get(<<"realtimeMetadata">>, Endpoints)).

%% 端点没绑 agent、请求也没指定 → 404（不是 500）
unknown_agent(#{port := Port}) ->
    Body = beamai_utils:encode_json(
             #{<<"messages">> => [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}],
               <<"forwardedProps">> => #{<<"agent">> => <<"nope">>}}),
    {Status, _Headers, Data} = post(Port, "/agui/open", Body),
    ?assertEqual(404, Status),
    Json = beamai_utils:parse_json(Data),
    ?assertEqual(<<"agent_not_found">>, maps:get(<<"error">>, Json)),
    %% 候选是**数据**而不是一行 `~p` 出来的 Erlang 项式 —— 客户端换个 id 就能自救
    ?assertEqual(<<"nope">>, maps:get(<<"requested">>, Json)),
    ?assertEqual([<<"echo">>], maps:get(<<"available">>, Json)),
    ?assertEqual(<<"no agent registered as 'nope'; available: echo">>,
                 maps:get(<<"detail">>, Json)).

%% `default` 不是名字，是「就这一台」的惯用写法：注册表里唯一那个接下来
default_agent(#{port := Port}) ->
    ok = mock_llm(),
    try
        Body = beamai_utils:encode_json(
                 #{<<"threadId">> => <<"t-default">>,
                   <<"messages">> => [#{<<"role">> => <<"user">>,
                                        <<"content">> => <<"hi">>}]}),
        {Status, Headers, Data} = post(Port, "/agui/agent/default/run", Body),
        ?assertEqual(200, Status),
        ?assertEqual(<<"text/event-stream">>, header(<<"content-type">>, Headers)),
        ?assertMatch([#{<<"type">> := <<"RUN_STARTED">>} | _], parse_sse(Data))
    after
        meck:unload(beamai_chat_model)
    end.

%% 两个候选时 `default` 指谁只能靠猜 —— 猜错是静悄悄的，所以宁可 404
default_ambiguous(#{port := Port}) ->
    ok = beamai_agui_registry:register(<<"echo-2">>, factory()),
    try
        Body = beamai_utils:encode_json(
                 #{<<"messages">> => [#{<<"role">> => <<"user">>,
                                        <<"content">> => <<"hi">>}]}),
        {Status, _Headers, Data} = post(Port, "/agui/agent/default/run", Body),
        ?assertEqual(404, Status),
        Json = beamai_utils:parse_json(Data),
        ?assertEqual(<<"default">>, maps:get(<<"requested">>, Json)),
        ?assertEqual([<<"echo">>, <<"echo-2">>],
                     lists:sort(maps:get(<<"available">>, Json)))
    after
        beamai_agui_registry:unregister(<<"echo-2">>)
    end.

%% 只有字面的 `default` 回落：手抖的名字喂给唯一那个 agent，就成了
%% 「能跑但跑的不是你以为那个」——比 404 难查得多
typo_not_aliased(#{port := Port}) ->
    Body = beamai_utils:encode_json(
             #{<<"messages">> => [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}]}),
    {Status, _Headers, Data} = post(Port, "/agui/agent/ech0/run", Body),
    ?assertEqual(404, Status),
    ?assertEqual(<<"ech0">>,
                 maps:get(<<"requested">>, beamai_utils:parse_json(Data))).

bad_body(#{port := Port}) ->
    {Status, _Headers, Data} = post(Port, "/agui/run", <<"not json">>),
    ?assertEqual(400, Status),
    ?assertEqual(<<"invalid_request">>,
                 maps:get(<<"error">>, beamai_utils:parse_json(Data))).

method_not_allowed(#{port := Port}) ->
    {Status, Headers, _Data} = get(Port, "/agui/run"),
    ?assertEqual(405, Status),
    ?assertEqual(<<"POST, OPTIONS">>, header(<<"allow">>, Headers)).

options_preflight(#{port := Port}) ->
    {Status, Headers, _Data} = request(Port, <<"OPTIONS">>, "/agui/run", [], <<>>),
    ?assertEqual(204, Status),
    ?assertEqual(<<"*">>, header(<<"access-control-allow-origin">>, Headers)),
    %% 断线重连靠 `Last-Event-ID` 把水位带回来 —— 不放行它，重连就只能从头来
    ?assertMatch({_, _}, binary:match(header(<<"access-control-allow-headers">>, Headers),
                                      <<"last-event-id">>)),
    %% 不缓存预检的话，每个 POST 前面都多一次 OPTIONS 往返
    ?assertEqual(<<"600">>, header(<<"access-control-max-age">>, Headers)).

%%====================================================================
%% CORS
%%
%% 浏览器**不允许 `*` 与凭据并存**：前端一旦 `credentials: 'include'`，请求当场被
%% 浏览器拦掉，而且错误只在控制台，服务端日志干干净净 —— 这类问题最难查，所以两种
%% 模式各有用例守着。
%%====================================================================

cors_wildcard(#{port := Port}) ->
    {200, Headers, _} = get_with(Port, "/agui/info",
                                 [{<<"origin">>, <<"http://localhost:3002">>}]),
    ?assertEqual(<<"*">>, header(<<"access-control-allow-origin">>, Headers)),
    %% `*` 模式下**不能**发凭据位：两者并存浏览器直接拒
    ?assertEqual(undefined, header(<<"access-control-allow-credentials">>, Headers)).

cors_allowlist_hit(#{port := Port}) ->
    Origin = <<"http://localhost:3100">>,
    {Status, Headers, _} = request(Port, <<"OPTIONS">>, "/agui/scoped",
                                   [{<<"origin">>, Origin}], <<>>),
    ?assertEqual(204, Status),
    %% 命中就**原样回显**（不是 `*`），并带上凭据位
    ?assertEqual(Origin, header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(<<"true">>, header(<<"access-control-allow-credentials">>, Headers)),
    %% 响应随 Origin 而变，不告诉缓存这件事，CDN 会把 A 源的响应喂给 B 源
    ?assertEqual(<<"origin">>, header(<<"vary">>, Headers)).

cors_allowlist_miss(#{port := Port}) ->
    {Status, Headers, _} = request(Port, <<"OPTIONS">>, "/agui/scoped",
                                   [{<<"origin">>, <<"http://evil.example">>}], <<>>),
    ?assertEqual(204, Status),
    %% 没命中就**一个字都不发** —— 让浏览器如实拦掉，而不是回个 `*` 假装放行
    ?assertEqual(undefined, header(<<"access-control-allow-origin">>, Headers)),
    ?assertEqual(undefined, header(<<"access-control-allow-credentials">>, Headers)).

%%====================================================================
%% 迷你 HTTP 客户端（gun）
%%====================================================================

get(Port, Path) ->
    request(Port, <<"GET">>, Path, [], <<>>).

get_with(Port, Path, Headers) ->
    request(Port, <<"GET">>, Path, Headers, <<>>).

post(Port, Path, Body) ->
    request(Port, <<"POST">>, Path, [{<<"content-type">>, <<"application/json">>}], Body).

%% @private 发一次请求，把整个响应体读完（SSE 也一样——流完了就结束）
request(Port, Method, Path, Headers, Body) ->
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    try
        Ref = gun:request(Conn, Method, list_to_binary(Path), Headers, Body),
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

header(Name, Headers) ->
    proplists:get_value(Name, Headers).

%% @private SSE 帧 → 事件 map 列表
parse_sse(Data) ->
    {_Rest, Frames} = beamai_sse:parse(Data),
    [beamai_utils:parse_json(maps:get(data, F)) || F <- Frames].
