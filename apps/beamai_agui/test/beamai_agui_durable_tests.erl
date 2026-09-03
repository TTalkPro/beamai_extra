%%%-------------------------------------------------------------------
%%% @doc 持久化 pause_store：后端选择 + 跨"节点重启"的 HITL
%%%
%%% beamai_agui_resume_tests 证明的是「两个请求之间只隔着 store」，这里再往前
%%% 推一步：**store 进程在两个请求之间死过一次**。ETS 后端到这一步就断了，
%%% DETS 后端不会。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_durable_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NAME, beamai_agui_durable_test_store).

%%====================================================================
%% 后端选择
%%====================================================================

backend_selection_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> unset_all() end,
     [fun() ->
        unset_all(),
        %% 什么都不配 → ETS（开发期够用，也不在磁盘上留文件）
        ?assertMatch({ets, beamai_agui_pause_store}, beamai_agui_thread:backend())
      end,
      fun() ->
        unset_all(),
        application:set_env(beamai_agui, pause_store_file, "/tmp/x.dets"),
        ?assertMatch({dets, beamai_agui_pause_store, "/tmp/x.dets"},
                     beamai_agui_thread:backend()),
        ?assertEqual({beamai_pause_store_dets, beamai_agui_pause_store},
                     beamai_agui_thread:store())
      end,
      fun() ->
        unset_all(),
        %% 宿主自带的 store 压过一切，runtime 不再起任何东西
        application:set_env(beamai_agui, pause_store_file, "/tmp/x.dets"),
        application:set_env(beamai_agui, pause_store, {my_store, ref1}),
        ?assertEqual({external, {my_store, ref1}}, beamai_agui_thread:backend()),
        ?assertEqual({ok, {my_store, ref1}}, beamai_agui_thread:available_store())
      end,
      fun() ->
        unset_all(),
        application:set_env(beamai_agui, pause_store_file, "/tmp/x.dets"),
        %% 配了 DETS 但进程没起来 → 退化成「没有 HITL 持久化」，而不是把死句柄
        %% 交给 agent（上游每个终态都会 pause_clear，那会让正常 run 也崩在收尾上）
        ?assertEqual(none, beamai_agui_thread:available_store())
      end]}.

unset_all() ->
    application:unset_env(beamai_agui, pause_store),
    application:unset_env(beamai_agui, pause_store_file),
    application:unset_env(beamai_agui, pause_store_name),
    ok.

%%====================================================================
%% 跨 store 重启的 HITL
%%====================================================================

durable_hitl_test_() ->
    {setup, fun setup_dets/0, fun cleanup_dets/1,
     fun(Ctx) -> [{"暂停跨 store 重启后仍能续跑", fun() -> round_trip(Ctx) end}] end}.

setup_dets() ->
    unset_all(),
    Unique = erlang:unique_integer([positive]),
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "beamai_agui_durable_tests"),
    ok = filelib:ensure_path(Dir),
    File = filename:join(Dir, lists:concat(["agui_", Unique, ".dets"])),
    application:set_env(beamai_agui, pause_store_name, ?NAME),
    application:set_env(beamai_agui, pause_store_file, File),
    {ok, _} = beamai_pause_store_dets:start_link(?NAME, #{file => File}),
    #{file => File}.

cleanup_dets(#{file := File}) ->
    catch beamai_pause_store_dets:stop(?NAME),
    file:delete(File),
    unset_all().

%%====================================================================
%% 被测 agent
%%====================================================================

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

mock_llm() ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_C, _M, _RawCb, Opts) ->
            counters:add(CC, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CC, 1) of
                1 -> {ok, beamai_chat_response:new(
                        #{content => null,
                          tool_calls => [tc(<<"ask1">>, <<"ask_human">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"done">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"done">>, finish_reason => <<"stop">>})}
            end
        end),
    ok.

factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(
            #{chat_client => K, memory => false,
              interrupt_tools => [#{name => <<"ask_human">>,
                                    description => <<"ask">>, parameters => #{}}]},
            RuntimeCfg))
    end.

input(Thread, Messages) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => Thread, <<"messages">> => Messages}),
    Input.

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

run(Input) ->
    Self = self(),
    ok = beamai_agui_runner:run(Input, factory(), fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

types(Events) -> [maps:get(<<"type">>, E) || E <- Events].

%%====================================================================
%% 用例
%%====================================================================

round_trip(#{file := File}) ->
    Thread = <<"t-durable">>,
    ok = mock_llm(),
    try
        %% 请求一：停在人身上，快照落到磁盘
        Ev1 = run(input(Thread, [#{<<"role">> => <<"user">>,
                                   <<"content">> => <<"go">>}])),
        ?assert(lists:member(<<"CUSTOM">>, types(Ev1))),
        ?assertMatch({ok, _}, beamai_agui_thread:pending(Thread)),

        %% 制造断点：store 进程停掉，再从同一个文件开起来（≈ 节点重启）
        ok = beamai_pause_store_dets:stop(?NAME),
        ?assertEqual(none, beamai_agui_thread:available_store()),
        {ok, _} = beamai_pause_store_dets:start_link(?NAME, #{file => File}),

        %% 快照还在
        {ok, Snapshot} = beamai_agui_thread:pending(Thread),
        ?assertEqual(<<"ask1">>, beamai_agui_thread:pending_tool_call_id(Snapshot)),

        %% 请求二：全新进程、全新 agent，从磁盘接上接着跑
        Reply = #{<<"role">> => <<"tool">>, <<"toolCallId">> => <<"ask1">>,
                  <<"content">> => <<"yes">>},
        Ev2 = run(input(Thread, [#{<<"role">> => <<"user">>, <<"content">> => <<"go">>},
                                 Reply])),
        ?assert(lists:member(<<"TOOL_CALL_RESULT">>, types(Ev2))),
        ?assert(lists:member(<<"TEXT_MESSAGE_CONTENT">>, types(Ev2))),
        ?assert(lists:member(<<"RUN_FINISHED">>, types(Ev2))),
        %% 跑到终态，未决暂停被清掉（且清除也落了盘）
        ?assertEqual(none, beamai_agui_thread:pending(Thread)),
        ok = beamai_pause_store_dets:stop(?NAME),
        {ok, _} = beamai_pause_store_dets:start_link(?NAME, #{file => File}),
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        meck:unload(beamai_chat_model)
    end.
