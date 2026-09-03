%%%-------------------------------------------------------------------
%%% @doc 跨请求 HITL：一次 run 暂停在人身上，下一次请求把它接着跑完
%%%
%%% 两次 `beamai_agui_runner:run/3` 之间**没有任何共享的进程或状态**——中间只
%%% 隔着 pause store。这正是真实部署里两个 HTTP 请求之间的样子。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_resume_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 夹具
%%====================================================================

setup() ->
    {ok, Pid} = beamai_pause_store_ets:start_link(beamai_agui_thread:store_name()),
    Pid.

cleanup(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 1000 -> erlang:demonitor(MRef, [flush]), ok
    end.

resume_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [fun hitl_round_trip/0,
      fun resume_without_input_errors/0,
      fun rejected_decision_emits_result/0,
      fun approved_decision_emits_real_tool_result/0,
      fun store_down_degrades_to_no_hitl/0]}.

%%====================================================================
%% 辅助
%%====================================================================

tc(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.

%% mock：首轮请求人类介入，之后给最终答案
mock_llm() ->
    mock_llm(<<"ask_human">>, <<"ask1">>).

mock_llm(ToolName, ToolCallId) ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            counters:add(CC, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CC, 1) of
                1 ->
                    {ok, beamai_chat_response:new(
                        #{content => null,
                          tool_calls => [tc(ToolCallId, ToolName,
                                            <<"{\"q\":\"ok?\"}">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"done">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"done">>, finish_reason => <<"stop">>})}
            end
        end),
    CC.

%% 工厂：被中断的工具是**真工具**（注册进 chat client），approved 时会真执行
approval_factory() ->
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(
              K0, #{name => <<"dangerous">>, parameters => #{},
                    handler => fun(_A, _C) -> {ok, <<"EXECUTED">>} end}),
        beamai_agent:new(maps:merge(
            #{chat_client => K,
              memory => false,
              interrupt_tools => [#{name => <<"dangerous">>,
                                    description => <<"needs approval">>,
                                    parameters => #{}}]},
            RuntimeCfg))
    end.

%% 工厂：带一个中断工具的 agent；RuntimeCfg 整个 merge 进去
factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(
            #{chat_client => K,
              memory => false,
              interrupt_tools => [#{name => <<"ask_human">>,
                                    description => <<"ask the human">>,
                                    parameters => #{}}]},
            RuntimeCfg))
    end.

input(ThreadId, Messages) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => ThreadId,
                      <<"messages">> => Messages}),
    Input.

user_msg(Text) -> #{<<"role">> => <<"user">>, <<"content">> => Text}.

tool_msg(Id, Content) ->
    #{<<"role">> => <<"tool">>, <<"toolCallId">> => Id, <<"content">> => Content}.

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

run(Input) ->
    run(Input, factory()).

run(Input, Factory) ->
    Self = self(),
    ok = beamai_agui_runner:run(Input, Factory, fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

types(Events) -> [maps:get(<<"type">>, E) || E <- Events].

%%====================================================================
%% 用例
%%====================================================================

%% 请求一暂停、请求二续跑：两次之间只隔着 pause store
hitl_round_trip() ->
    Thread = <<"t-hitl">>,
    _CC = mock_llm(),
    try
        %% ---- 请求一：模型要求人类介入 ----
        Ev1 = run(input(Thread, [user_msg(<<"go">>)])),
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>,
                      <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
                      <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                      <<"CUSTOM">>, <<"RUN_FINISHED">>], types(Ev1)),
        %% 前端看到的是一个**没有结果**的工具调用——那就是"等你回话"
        ?assertEqual([], [E || E <- Ev1,
                              maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>]),
        %% CUSTOM 告诉前端在等哪次调用
        [Custom] = [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"CUSTOM">>],
        ?assertEqual(<<"interrupt">>, maps:get(<<"name">>, Custom)),
        ?assertEqual(<<"ask1">>, maps:get(<<"toolCallId">>, maps:get(<<"value">>, Custom))),

        %% ---- 暂停确实落库了 ----
        {ok, Snapshot} = beamai_agui_thread:pending(Thread),
        ?assertEqual(<<"ask1">>, beamai_agui_thread:pending_tool_call_id(Snapshot)),

        %% ---- 请求二：前端把工具结果作为 tool 消息回传 ----
        Ev2 = run(input(Thread, [user_msg(<<"go">>), tool_msg(<<"ask1">>, <<"yes">>)])),
        %% 文本三件套夹在 STEP_STARTED / STEP_FINISHED 之间 —— 续跑走的是
        %% beamai_agent:stream_resume/3，token 是实时流出来的（不是 on_message_end
        %% 时整块补的）。这正是给上游加流式 resume 入口要拿到的东西。
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"TOOL_CALL_RESULT">>,
                      <<"STEP_STARTED">>,
                      <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                      <<"STEP_FINISHED">>, <<"TEXT_MESSAGE_END">>,
                      <<"RUN_FINISHED">>], types(Ev2)),
        %% 补上的结果对回了那次调用，内容是人给的
        [Result] = [E || E <- Ev2, maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(<<"ask1">>, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"yes">>, maps:get(<<"content">>, Result)),

        %% ---- 跑到终态，未决暂停被清掉（上游在终态自己清）----
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        meck:unload(beamai_chat_model)
    end.

%% thread 卡在暂停上、请求却没带任何输入 → RUN_ERROR，且不去动 agent
resume_without_input_errors() ->
    Thread = <<"t-noinput">>,
    _CC = mock_llm(),
    try
        _ = run(input(Thread, [user_msg(<<"go">>)])),
        ?assertMatch({ok, _}, beamai_agui_thread:pending(Thread)),
        %% 空 messages：既没有 tool 结果也没有 user 文本
        Ev = run(input(Thread, [])),
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"RUN_ERROR">>], types(Ev)),
        %% 暂停仍在，没被这次失败的请求弄丢
        ?assertMatch({ok, _}, beamai_agui_thread:pending(Thread))
    after
        beamai_agui_thread:clear(Thread),
        meck:unload(beamai_chat_model)
    end.

%% 显式决策：forwardedProps.resume 里的 rejected 也会补出 TOOL_CALL_RESULT
rejected_decision_emits_result() ->
    Thread = <<"t-reject">>,
    _CC = mock_llm(),
    try
        _ = run(input(Thread, [user_msg(<<"go">>)])),
        {ok, Input} = beamai_agui_types:parse_run_input(
                        #{<<"threadId">> => Thread,
                          <<"messages">> => [user_msg(<<"go">>)],
                          <<"forwardedProps">> =>
                              #{<<"resume">> =>
                                    #{<<"decision">> => <<"rejected">>,
                                      <<"payload">> => #{<<"message">> => <<"nope">>}}}}),
        Self = self(),
        ok = beamai_agui_runner:run(Input, factory(),
                                    fun(Ev) -> Self ! {ev, Ev}, ok end),
        Events = collect(),
        [Result] = [E || E <- Events,
                         maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(<<"ask1">>, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"已拒绝执行：nope"/utf8>>, maps:get(<<"content">>, Result)),
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        beamai_agui_thread:clear(Thread),
        meck:unload(beamai_chat_model)
    end.

%% approved：服务端真去执行那个工具，TOOL_CALL_RESULT 带的是工具的真实输出
%%
%% 这条结果**不是** runtime 补的（`emit_resume_result` 对 approved 什么都不发），
%% 而是上游 resume 执行工具时触发 on_tool_result 走 emitter 出来的。断言只有
%% 一条 TOOL_CALL_RESULT，顺带守住"别两边都发"。
approved_decision_emits_real_tool_result() ->
    Thread = <<"t-approve">>,
    _CC = mock_llm(<<"dangerous">>, <<"call_1">>),
    try
        Ev1 = run(input(Thread, [user_msg(<<"go">>)]), approval_factory()),
        ?assert(lists:member(<<"CUSTOM">>, types(Ev1))),
        %% 暂停时那次调用还没有结果
        ?assertEqual([], [E || E <- Ev1,
                              maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>]),

        {ok, Input} = beamai_agui_types:parse_run_input(
                        #{<<"threadId">> => Thread,
                          <<"messages">> => [user_msg(<<"go">>)],
                          <<"forwardedProps">> =>
                              #{<<"resume">> => #{<<"decision">> => <<"approved">>}}}),
        Ev2 = run(Input, approval_factory()),
        [Result] = [E || E <- Ev2,
                         maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(<<"call_1">>, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"EXECUTED">>, maps:get(<<"content">>, Result)),
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        beamai_agui_thread:clear(Thread),
        meck:unload(beamai_chat_model)
    end.

%% store 没起来：退化成「没有 HITL 持久化」，而不是每个请求都炸
%%
%% 上游在**每个终态**都会 pause_clear，把一个指向死进程的句柄交给 agent
%% 会让正常的 run 也崩在收尾上——所以 runtime_config 必须先看 store 活着没有。
store_down_degrades_to_no_hitl() ->
    %% 把 store 名指到一个不存在的进程上，而不是去杀夹具起的那个——
    %% 那个进程 link 在夹具进程上，杀它会把整组用例一起带走
    application:set_env(beamai_agui, pause_store_name, no_such_pause_store),
    _CC = mock_llm(),
    try
        ?assertEqual(none, beamai_agui_thread:available_store()),
        Ev = run(input(<<"t-nostore">>, [user_msg(<<"go">>)])),
        %% 照常跑到中断（只是这次中断没地方存，下个请求接不上）
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>,
                      <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
                      <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                      <<"CUSTOM">>, <<"RUN_FINISHED">>], types(Ev)),
        ?assertEqual(none, beamai_agui_thread:pending(<<"t-nostore">>))
    after
        meck:unload(beamai_chat_model),
        application:unset_env(beamai_agui, pause_store_name)
    end.
