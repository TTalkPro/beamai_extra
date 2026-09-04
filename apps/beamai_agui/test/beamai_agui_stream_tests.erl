%%%-------------------------------------------------------------------
%%% @doc HTTP 层的会话能力：断线重连、跨请求 stop、只读面
%%%
%%% 与 `beamai_agui_cowboy_tests` 的分工：那个模块**不起会话层**，走的是
%%% "run 绑在请求上"的回落路径；这个模块起了会话层，验的是 run 脱离请求之后
%%% 才有的那三件事。
%%%
%%% 关键在于**真的断开连接**再接回来 —— 只在同一个连接里读读停停证明不了任何
%%% 事情。所以这里的重连用例是：读几帧 → `gun:close` → 另开一个连接
%%% `GET /threads/:id/stream`，带上刚才最后那个 `id:`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_stream_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, beamai_agui_stream_test_listener).

%%====================================================================
%% 夹具
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, Registry} = beamai_agui_registry:start_link(),
    {ok, SessionSup} = beamai_agui_session_sup:start_link(),
    {ok, Sessions} = beamai_agui_sessions:start_link(),
    ok = beamai_agui_registry:register(<<"echo">>, factory()),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/agui/run", beamai_agui_cowboy_handler, #{agent => <<"echo">>}},
            {"/agui/info", beamai_agui_cowboy_handler, #{mode => info}},
            {"/agui/threads", beamai_agui_cowboy_handler, #{mode => threads}},
            {"/agui/threads/:thread", beamai_agui_cowboy_handler, #{mode => thread}},
            {"/agui/threads/:thread/:sub", beamai_agui_cowboy_handler, #{mode => thread}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    #{registry => Registry, sessions => Sessions, session_sup => SessionSup,
      port => ranch:get_port(?LISTENER)}.

cleanup(#{registry := Registry, sessions := Sessions, session_sup := SessionSup}) ->
    ok = cowboy:stop_listener(?LISTENER),
    _ = [stop_proc(P) || P <- [Sessions, SessionSup, Registry]],
    ok.

stop_proc(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 2000 -> erlang:demonitor(MRef, [flush]), ok
    end.

stream_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(Ctx) ->
        [{"SSE 帧带 id:（Last-Event-ID 的原料）", fun() -> frames_carry_id(Ctx) end},
         {"断线重连：另开连接只补缺口", fun() -> reconnect_resumes(Ctx) end},
         {"跨请求 stop", fun() -> stop_endpoint(Ctx) end},
         {"threads 只读面", fun() -> threads_readonly(Ctx) end},
         {"会话层起着时 info 如实报 threadEndpoints",
          fun() -> info_reports_threads(Ctx) end},
         {"未知 thread → 404", fun() -> unknown_thread(Ctx) end}]
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

mock_fast() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            _ = [TokenCb(T, #{}) || T <- [<<"he">>, <<"llo">>]],
            {ok, beamai_chat_response:new(
                #{content => <<"hello">>, finish_reason => <<"stop">>})}
        end),
    ok.

%% 逐 token 慢慢吐：这样才有"读到一半断开"这回事
mock_slow() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            lists:foreach(fun(T) -> TokenCb(T, #{}), timer:sleep(150) end,
                          [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>]),
            {ok, beamai_chat_response:new(
                #{content => <<"abcde">>, finish_reason => <<"stop">>})}
        end),
    ok.

%% 一直不返回：用来验"停一个还在跑的 run"
mock_hang() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            TokenCb = maps:get(on_llm_new_token, Opts),
            TokenCb(<<"thinking">>, #{}),
            timer:sleep(60000),
            {ok, beamai_chat_response:new(#{content => <<"never">>,
                                            finish_reason => <<"stop">>})}
        end),
    ok.

run_body(ThreadId) ->
    beamai_utils:encode_json(
      #{<<"threadId">> => ThreadId,
        <<"messages">> => [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}]}).

%%====================================================================
%% 用例
%%====================================================================

%% 每一帧都带 `id:` —— 浏览器 `EventSource` 断线时把它放进 Last-Event-ID，
%% 服务端凭它只补缺口。没有这行，重连就只能从头来（或者干脆丢一段）。
frames_carry_id(#{port := Port}) ->
    ok = mock_fast(),
    try
        {200, Headers, Data} = post(Port, "/agui/run", run_body(<<"t-id">>)),
        %% run id 也回在头上：另一个请求要停这条 run 得先知道它是谁
        ?assert(is_binary(header(<<"x-agui-run-id">>, Headers))),
        {_Rest, Frames} = beamai_sse:parse(Data),
        Ids = [maps:get(id, F, undefined) || F <- Frames],
        ?assertEqual([], [I || I <- Ids, I =:= undefined]),
        %% 号从 0 起、连续无洞
        ?assertEqual(lists:seq(0, length(Ids) - 1),
                     [binary_to_integer(I) || I <- Ids])
    after
        meck:unload(beamai_chat_model)
    end.

%% **真的断开**再接回来：读两帧就 close，另开连接带着水位续上
reconnect_resumes(#{port := Port}) ->
    ok = mock_slow(),
    try
        {Conn, Ref} = open_post(Port, "/agui/run", run_body(<<"t-reconnect">>)),
        {response, nofin, 200, _} = gun:await(Conn, Ref, 5000),
        {Seen, Last} = read_frames(Conn, Ref, 3),
        ok = gun:close(Conn),                    %% ← 连接没了，但 run 还在跑
        ?assert(Last =/= undefined),

        %% 另一个连接接回来。中间掉的那几帧从环形缓冲补出来，一条不漏
        Path = "/agui/threads/t-reconnect/stream?since=" ++ integer_to_list(Last),
        {200, _, Data} = get(Port, Path),
        {_Rest, Frames} = beamai_sse:parse(Data),
        Resumed = [binary_to_integer(maps:get(id, F)) || F <- Frames],
        %% 一条不重（都在水位之后），也一条不漏（接着水位往下）
        ?assert(lists:all(fun(Seq) -> Seq > Last end, Resumed)),
        ?assertEqual(Last + 1, hd(Resumed)),
        %% 终态照样收到 —— 断线那一下没把这轮的输出弄丢
        Types = [maps:get(<<"type">>, beamai_utils:parse_json(maps:get(data, F)))
                 || F <- Frames],
        ?assertEqual(<<"RUN_FINISHED">>, lists:last(Types)),
        ?assert(length(Seen) >= 3)
    after
        meck:unload(beamai_chat_model)
    end.

%% 跨请求 stop：调用方手上只有 threadId
stop_endpoint(#{port := Port}) ->
    ok = mock_hang(),
    try
        {Conn, Ref} = open_post(Port, "/agui/run", run_body(<<"t-stop">>)),
        {response, nofin, 200, _} = gun:await(Conn, Ref, 5000),
        {_Seen, Last} = read_frames(Conn, Ref, 3),
        ?assert(Last =/= undefined),

        %% **另一个 HTTP 请求**把它停掉
        {200, _, StopBody} = post(Port, "/agui/threads/t-stop/stop", <<>>),
        ?assertEqual(true, maps:get(<<"stopped">>, beamai_utils:parse_json(StopBody))),

        %% 停稳的信号是终态事件，它到了还开着的那条流上
        Types = drain_types(Conn, Ref, 5000),
        ?assertEqual(<<"RUN_FINISHED">>, lists:last(Types)),
        ok = gun:close(Conn),

        %% 缓冲里那条终态如实说了"是取消，不是跑完了"
        {200, _, EvBody} = get(Port, "/agui/threads/t-stop/events"),
        Events = maps:get(<<"events">>, beamai_utils:parse_json(EvBody)),
        #{<<"event">> := LastEv} = lists:last(Events),
        ?assertEqual(#{<<"status">> => <<"cancelled">>}, maps:get(<<"result">>, LastEv))
    after
        meck:unload(beamai_chat_model)
    end.

%% 只读面：列表 / 事件 / 状态。**没有第二套存储** —— 全是会话里那份
threads_readonly(#{port := Port}) ->
    ok = mock_fast(),
    try
        Body = beamai_utils:encode_json(
                 #{<<"threadId">> => <<"t-ro">>,
                   <<"state">> => #{<<"count">> => 2},
                   <<"messages">> => [#{<<"role">> => <<"user">>,
                                        <<"content">> => <<"hi">>}]}),
        {200, _, _} = post(Port, "/agui/run", Body),

        {200, _, ListBody} = get(Port, "/agui/threads"),
        Threads = maps:get(<<"threads">>, beamai_utils:parse_json(ListBody)),
        [Ours] = [T || T <- Threads, maps:get(<<"threadId">>, T) =:= <<"t-ro">>],
        ?assertEqual(<<"idle">>, maps:get(<<"state">>, Ours)),

        {200, _, StateBody} = get(Port, "/agui/threads/t-ro/state"),
        ?assertEqual(#{<<"count">> => 2},
                     maps:get(<<"state">>, beamai_utils:parse_json(StateBody))),

        %% events 也认 since：与 SSE 那条路同一个水位语义
        {200, _, All} = get(Port, "/agui/threads/t-ro/events"),
        {200, _, Tail} = get(Port, "/agui/threads/t-ro/events?since=1"),
        NAll = length(maps:get(<<"events">>, beamai_utils:parse_json(All))),
        NTail = length(maps:get(<<"events">>, beamai_utils:parse_json(Tail))),
        ?assertEqual(NAll - 2, NTail),

        %% DELETE 把会话摘掉（不动历史与未决暂停）
        {200, _, _} = request(Port, <<"DELETE">>, "/agui/threads/t-ro", [], <<>>),
        {200, _, ListBody2} = get(Port, "/agui/threads"),
        ?assertEqual([], [T || T <- maps:get(<<"threads">>,
                                             beamai_utils:parse_json(ListBody2)),
                               maps:get(<<"threadId">>, T) =:= <<"t-ro">>])
    after
        meck:unload(beamai_chat_model)
    end.

%% 会话层起着 → threadEndpoints 报 true（`beamai_agui_cowboy_tests` 里没起，报 false）
info_reports_threads(#{port := Port}) ->
    {200, _, Data} = get(Port, "/agui/info"),
    Endpoints = maps:get(<<"threadEndpoints">>, beamai_utils:parse_json(Data)),
    ?assertEqual(true, maps:get(<<"list">>, Endpoints)),
    ?assertEqual(true, maps:get(<<"inspect">>, Endpoints)),
    ?assertEqual(true, maps:get(<<"mutations">>, Endpoints)),
    %% 恒 false：实时元数据那条路我们没有，报 true 会让客户端等一条不来的流
    ?assertEqual(false, maps:get(<<"realtimeMetadata">>, Endpoints)).

unknown_thread(#{port := Port}) ->
    {404, _, Data} = get(Port, "/agui/threads/nope/state"),
    ?assertEqual(<<"thread_not_found">>,
                 maps:get(<<"error">>, beamai_utils:parse_json(Data))).

%%====================================================================
%% 迷你 HTTP 客户端（gun）
%%====================================================================

get(Port, Path) ->
    request(Port, <<"GET">>, Path, [], <<>>).

post(Port, Path, Body) ->
    request(Port, <<"POST">>, Path, [{<<"content-type">>, <<"application/json">>}], Body).

request(Port, Method, Path, Headers, Body) ->
    {Conn, Ref} = open(Port, Method, Path, Headers, Body),
    try
        case gun:await(Conn, Ref, 15000) of
            {response, fin, Status, RespHeaders} ->
                {Status, RespHeaders, <<>>};
            {response, nofin, Status, RespHeaders} ->
                {ok, Data} = gun:await_body(Conn, Ref, 15000),
                {Status, RespHeaders, Data}
        end
    after
        gun:close(Conn)
    end.

open_post(Port, Path, Body) ->
    open(Port, <<"POST">>, Path, [{<<"content-type">>, <<"application/json">>}], Body).

open(Port, Method, Path, Headers, Body) ->
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    {Conn, gun:request(Conn, Method, list_to_binary(Path), Headers, Body)}.

%% @private 读 N 帧就停手，返回读到的帧与最后那个 `id:`
read_frames(Conn, Ref, N) ->
    read_frames(Conn, Ref, N, <<>>, [], undefined).

read_frames(_Conn, _Ref, 0, _Buf, Acc, Last) ->
    {lists:reverse(Acc), Last};
read_frames(Conn, Ref, N, Buf, Acc, Last) ->
    case gun:await(Conn, Ref, 5000) of
        {data, _, Chunk} ->
            {Rest, Frames} = beamai_sse:parse(<<Buf/binary, Chunk/binary>>),
            case Frames of
                [] ->
                    read_frames(Conn, Ref, N, Rest, Acc, Last);
                _ ->
                    Ids = [binary_to_integer(maps:get(id, F)) || F <- Frames],
                    read_frames(Conn, Ref, max(0, N - length(Frames)), Rest,
                                lists:reverse(Frames) ++ Acc, lists:last(Ids))
            end;
        _ ->
            {lists:reverse(Acc), Last}
    end.

%% @private 把剩下的读完，返回事件类型序列
drain_types(Conn, Ref, Timeout) ->
    drain_types(Conn, Ref, Timeout, <<>>, []).

drain_types(Conn, Ref, Timeout, Buf, Acc) ->
    case gun:await(Conn, Ref, Timeout) of
        {data, nofin, Chunk} ->
            {Rest, Frames} = beamai_sse:parse(<<Buf/binary, Chunk/binary>>),
            Types = [maps:get(<<"type">>, beamai_utils:parse_json(maps:get(data, F)))
                     || F <- Frames],
            case lists:member(<<"RUN_FINISHED">>, Types)
                 orelse lists:member(<<"RUN_ERROR">>, Types) of
                true -> Acc ++ Types;
                false -> drain_types(Conn, Ref, Timeout, Rest, Acc ++ Types)
            end;
        {data, fin, Chunk} ->
            {_Rest, Frames} = beamai_sse:parse(<<Buf/binary, Chunk/binary>>),
            Acc ++ [maps:get(<<"type">>, beamai_utils:parse_json(maps:get(data, F)))
                    || F <- Frames];
        _ ->
            Acc
    end.

header(Name, Headers) ->
    proplists:get_value(Name, Headers).
