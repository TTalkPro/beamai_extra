%%%-------------------------------------------------------------------
%%% @doc 会话层：run 的寿命脱离调用方
%%%
%%% 这个模块守着三条契约，它们是"断线重连不丢输出"与"跨请求 stop"的全部依据：
%%%
%%% 1. **`seq` 在一个会话内单调无洞，跨 run 连续** —— 否则重连时"补 3 号之后的"
%%%    会把上一轮的事件也补一遍；
%%% 2. **一条 run 恰好一个终态事件，且它是最后一条** —— 崩溃、被杀也不例外
%%%    （会话替它补），否则客户端的流永远关不掉；
%%% 3. **订阅者带 `since` 只补缺口** —— 一条不漏也一条不重。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 夹具
%%====================================================================

setup() ->
    {ok, SessionSup} = beamai_agui_session_sup:start_link(),
    {ok, Sessions} = beamai_agui_sessions:start_link(),
    #{session_sup => SessionSup, sessions => Sessions}.

cleanup(#{session_sup := SessionSup, sessions := Sessions}) ->
    ok = stop_proc(Sessions),
    ok = stop_proc(SessionSup),
    ok.

stop_proc(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 2000 -> erlang:demonitor(MRef, [flush]), ok
    end.

session_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"起 run → 订阅补齐 → 终态收口", fun started_run_streams_to_subscriber/0},
      {"seq 跨 run 连续、无洞", fun seq_is_monotonic_across_runs/0},
      {"since 只补缺口，不重不漏", fun since_replays_only_the_gap/0},
      {"缓冲越界 → 报缺口", fun buffer_overrun_reports_gap/0},
      {"跨请求 stop：另一个调用方凭 threadId 停得掉", fun stop_from_elsewhere/0},
      {"被杀的 run 也有终态事件", fun killed_run_still_gets_terminal/0},
      {"一会话一 run：reject 缺省", fun second_run_is_rejected/0},
      {"supersede：新的顶掉旧的", fun supersede_replaces_running/0},
      {"STATE_SNAPSHOT + DELTA 折出当前 state", fun state_is_folded/0},
      {"消息账本：从事件流折出 AG-UI 消息", fun messages_are_folded/0},
      {"订阅者进程没了不影响 run", fun dead_subscriber_is_dropped/0}]}.

%%====================================================================
%% 被测 agent
%%====================================================================

%% 一句话就跑完的 agent
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

%% 一直不返回的 agent —— 用来验"停一个还在跑的 run"
mock_slow() ->
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

factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

input(ThreadId, RunId) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go">>}]}),
    Input.

%% @doc 一直收到终态事件为止（会话层的契约就是它一定来）
collect_until_terminal(Ref) ->
    collect_until_terminal(Ref, []).

collect_until_terminal(Ref, Acc) ->
    receive
        {agui_event, Ref, Seq, Event} ->
            case beamai_agui_event:is_terminal(Event) of
                true -> lists:reverse([{Seq, Event} | Acc]);
                false -> collect_until_terminal(Ref, [{Seq, Event} | Acc])
            end;
        {agui_closed, Ref, _} ->
            lists:reverse(Acc)
    after 5000 ->
        lists:reverse(Acc)
    end.

types([{_Seq, E} | Rest]) -> [beamai_agui_event:type_of(E) | types(Rest)];
types([]) -> [].

seqs(Events) -> [Seq || {Seq, _} <- Events].

%%====================================================================
%% 用例
%%====================================================================

started_run_streams_to_subscriber() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-basic">>),
        {started, RunId, Since} =
            beamai_agui_session:start_run(S, input(<<"s-basic">>, <<"r1">>), factory()),
        ?assertEqual(<<"r1">>, RunId),
        %% run 一进来就发了 RUN_STARTED，我们是在那之后才订阅的 —— 水位补齐了它
        ?assertEqual(-1, Since),
        {ok, Ref, Replay, undefined} = beamai_agui_session:subscribe(S, Since),
        Events = Replay ++ collect_until_terminal(Ref),
        Types = types(Events),
        ?assertEqual(<<"RUN_STARTED">>, hd(Types)),
        ?assertEqual(<<"RUN_FINISHED">>, lists:last(Types)),
        %% 恰好一个终态，且它是最后一条
        ?assertEqual(1, length([T || T <- Types,
                                     lists:member(T, [<<"RUN_FINISHED">>,
                                                      <<"RUN_ERROR">>])])),
        %% 号连续无洞
        ?assertEqual(lists:seq(0, length(Events) - 1), seqs(Events))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-basic">>)
    end.

seq_is_monotonic_across_runs() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-seq">>),
        {started, _, Since1} =
            beamai_agui_session:start_run(S, input(<<"s-seq">>, <<"r1">>), factory()),
        {ok, Ref1, R1, _} = beamai_agui_session:subscribe(S, Since1),
        First = R1 ++ collect_until_terminal(Ref1),
        ok = beamai_agui_session:unsubscribe(S, Ref1),

        %% 第二轮：号**接着上一轮往下走**，不重置
        {started, _, Since2} =
            beamai_agui_session:start_run(S, input(<<"s-seq">>, <<"r2">>), factory()),
        ?assertEqual(lists:last(seqs(First)), Since2),
        {ok, Ref2, R2, _} = beamai_agui_session:subscribe(S, Since2),
        Second = R2 ++ collect_until_terminal(Ref2),
        ?assertEqual(Since2 + 1, hd(seqs(Second)))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-seq">>)
    end.

since_replays_only_the_gap() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-since">>),
        {started, _, Since} =
            beamai_agui_session:start_run(S, input(<<"s-since">>, <<"r1">>), factory()),
        {ok, Ref, Replay, _} = beamai_agui_session:subscribe(S, Since),
        All = Replay ++ collect_until_terminal(Ref),
        ok = beamai_agui_session:unsubscribe(S, Ref),

        %% 断线重连：手上最后收到的是第 2 号
        {ok, Ref2, Gap, undefined} = beamai_agui_session:subscribe(S, 2),
        ok = beamai_agui_session:unsubscribe(S, Ref2),
        ?assertEqual([Seq || Seq <- seqs(All), Seq > 2], seqs(Gap)),
        %% 一条不重
        ?assert(lists:all(fun(Seq) -> Seq > 2 end, seqs(Gap)))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-since">>)
    end.

buffer_overrun_reports_gap() ->
    ok = application:set_env(beamai_agui, buffer_size, 3),
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-overrun">>),
        {started, _, Since} =
            beamai_agui_session:start_run(S, input(<<"s-overrun">>, <<"r1">>), factory()),
        {ok, Ref, Replay, _} = beamai_agui_session:subscribe(S, Since),
        _ = Replay ++ collect_until_terminal(Ref),
        ok = beamai_agui_session:unsubscribe(S, Ref),
        %% 缓冲只剩最后 3 条，从 0 号补起 → 中间那段永远补不回来了
        {ok, Ref2, _, Resync} = beamai_agui_session:subscribe(S, 0),
        ok = beamai_agui_session:unsubscribe(S, Ref2),
        %% 缺口给的是一份消息快照（`MESSAGES_SNAPSHOT` 的载荷），不是一声「你漏了」
        ?assert(is_list(Resync)),
        ?assertMatch([#{<<"role">> := <<"assistant">>, <<"content">> := <<"hello">>}],
                     Resync)
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-overrun">>),
        application:unset_env(beamai_agui, buffer_size)
    end.

stop_from_elsewhere() ->
    ok = mock_slow(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-stop">>),
        {started, RunId, Since} =
            beamai_agui_session:start_run(S, input(<<"s-stop">>, <<"r1">>), factory()),
        {ok, Ref, _Replay, _} = beamai_agui_session:subscribe(S, Since),
        %% 等 run 真的跑起来（第一个 token 出来了）
        ?assert(wait_for_type(Ref, <<"TEXT_MESSAGE_CONTENT">>, 3000)),

        %% **另一个调用方**手上只有 threadId —— 这正是跨请求 stop 的形状
        ?assert(beamai_agui_session:stop_run(<<"s-stop">>, RunId)),

        %% 停稳的信号是终态事件，不是 stop_run 的返回值
        Rest = collect_until_terminal(Ref),
        {_, Last} = lists:last(Rest),
        ?assertEqual(<<"RUN_FINISHED">>, beamai_agui_event:type_of(Last)),
        ?assertEqual(#{<<"status">> => <<"cancelled">>}, maps:get(<<"result">>, Last)),
        %% 停完会话回到 idle，下一轮照常起得来
        ?assertEqual(idle, maps:get(state, beamai_agui_session:status(S)))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-stop">>)
    end.

killed_run_still_gets_terminal() ->
    ok = mock_slow(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-kill">>),
        {started, _RunId, Since} =
            beamai_agui_session:start_run(S, input(<<"s-kill">>, <<"r1">>), factory()),
        {ok, Ref, _Replay, _} = beamai_agui_session:subscribe(S, Since),
        ?assert(wait_for_type(Ref, <<"TEXT_MESSAGE_CONTENT">>, 3000)),

        %% 不走 stop_run，直接把 run 进程打死：模拟它自己崩了
        #{run_id := _} = beamai_agui_session:status(S),
        RunPid = run_pid(S),
        exit(RunPid, kill),

        %% 契约不因为崩溃而破：终态事件照样有，且如实说是错误不是"跑完了"
        Rest = collect_until_terminal(Ref),
        {_, Last} = lists:last(Rest),
        ?assertEqual(<<"RUN_ERROR">>, beamai_agui_event:type_of(Last)),
        ?assertEqual(<<"runner_died">>, maps:get(<<"code">>, Last))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-kill">>)
    end.

second_run_is_rejected() ->
    ok = mock_slow(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-busy">>),
        {started, RunId, _} =
            beamai_agui_session:start_run(S, input(<<"s-busy">>, <<"r1">>), factory()),
        %% 缺省 reject：一会话至多一个 in-flight run，第二个如实碰壁
        ?assertEqual({busy, RunId},
                     beamai_agui_session:start_run(S, input(<<"s-busy">>, <<"r2">>),
                                                   factory())),
        ?assert(beamai_agui_session:stop_run(S))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-busy">>)
    end.

supersede_replaces_running() ->
    ok = mock_slow(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-super">>),
        {started, _Old, Since} =
            beamai_agui_session:start_run(S, input(<<"s-super">>, <<"r1">>), factory()),
        {ok, Ref, _, _} = beamai_agui_session:subscribe(S, Since),
        ?assert(wait_for_type(Ref, <<"TEXT_MESSAGE_CONTENT">>, 3000)),

        %% 聊天 UX：用户又发了一句，新的顶掉上一条
        {started, New, _} =
            beamai_agui_session:start_run(S, input(<<"s-super">>, <<"r2">>), factory(),
                                          #{on_concurrent => supersede}),
        ?assertEqual(<<"r2">>, New),
        %% 旧的收口了（cancelled），新的在跑
        ?assertEqual(<<"r2">>, maps:get(run_id, beamai_agui_session:status(S))),
        ?assert(beamai_agui_session:stop_run(S))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-super">>)
    end.

state_is_folded() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-state">>),
        {ok, Input} = beamai_agui_types:parse_run_input(
                        #{<<"threadId">> => <<"s-state">>, <<"runId">> => <<"r1">>,
                          <<"state">> => #{<<"count">> => 1},
                          <<"messages">> => [#{<<"role">> => <<"user">>,
                                               <<"content">> => <<"go">>}]}),
        {started, _, Since} = beamai_agui_session:start_run(S, Input, factory()),
        {ok, Ref, Replay, _} = beamai_agui_session:subscribe(S, Since),
        _ = Replay ++ collect_until_terminal(Ref),
        %% 基线从流上的 STATE_SNAPSHOT 折出来 —— `/threads/:id/state` 读的就是它
        ?assertEqual(#{<<"count">> => 1}, beamai_agui_session:state(S))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-state">>)
    end.

dead_subscriber_is_dropped() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-sub">>),
        Self = self(),
        Dead = spawn(fun() ->
            {ok, Ref, _, _} = beamai_agui_session:subscribe(S, undefined),
            Self ! {subscribed, Ref}
        end),
        receive {subscribed, _} -> ok after 2000 -> ?assert(false) end,
        wait_gone(Dead),
        %% 订阅者进程没了，会话该把它摘掉，run 照跑
        {started, _, Since} =
            beamai_agui_session:start_run(S, input(<<"s-sub">>, <<"r1">>), factory()),
        {ok, Ref2, Replay, _} = beamai_agui_session:subscribe(S, Since),
        Events = Replay ++ collect_until_terminal(Ref2),
        ?assertEqual(<<"RUN_FINISHED">>, lists:last(types(Events))),
        ?assertEqual(1, maps:get(subscribers, beamai_agui_session:status(S)))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-sub">>)
    end.

%%====================================================================
%% 辅助
%%====================================================================

wait_for_type(Ref, Type, Timeout) ->
    receive
        {agui_event, Ref, _Seq, Event} ->
            case beamai_agui_event:type_of(Event) of
                Type -> true;
                _ -> wait_for_type(Ref, Type, Timeout)
            end
    after Timeout ->
        false
    end.

wait_gone(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 2000 -> erlang:demonitor(MRef, [flush]), ok
    end.

%% run 进程是会话 link 出去的那个（会话自己不 link 别人）
run_pid(Session) ->
    {links, Links} = erlang:process_info(Session, links),
    [Pid] = [P || P <- Links, P =/= self(),
                  erlang:process_info(P, registered_name) =:= []],
    Pid.

%% 消息账本是**这条流自己的投影** —— 用来回答「你漏掉的那几条事件里说了什么」，
%% 不是第二个对话历史（那在 chat memory 里）。
messages_are_folded() ->
    ok = mock_fast(),
    try
        {ok, S} = beamai_agui_sessions:ensure(<<"s-ledger">>),
        {started, _, Since} =
            beamai_agui_session:start_run(S, input(<<"s-ledger">>, <<"r1">>), factory()),
        {ok, Ref, Replay, _} = beamai_agui_session:subscribe(S, Since),
        _ = Replay ++ collect_until_terminal(Ref),
        [Message] = beamai_agui_session:messages(S),
        ?assertEqual(<<"assistant">>, maps:get(<<"role">>, Message)),
        ?assertEqual(<<"hello">>, maps:get(<<"content">>, Message)),
        ?assert(is_binary(maps:get(<<"id">>, Message)))
    after
        meck:unload(beamai_chat_model),
        beamai_agui_sessions:forget(<<"s-ledger">>)
    end.
