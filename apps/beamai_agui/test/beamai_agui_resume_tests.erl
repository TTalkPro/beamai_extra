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
      fun store_down_degrades_to_no_hitl/0]}.

%%====================================================================
%% 辅助
%%====================================================================

tc(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.

%% mock：首轮请求人类介入，之后给最终答案
mock_llm() ->
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
                          tool_calls => [tc(<<"ask1">>, <<"ask_human">>,
                                            <<"{\"q\":\"ok?\"}">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"done">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"done">>, finish_reason => <<"stop">>})}
            end
        end),
    CC.

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
    Self = self(),
    ok = beamai_agui_runner:run(Input, factory(), fun(Ev) -> Self ! {ev, Ev}, ok end),
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
        ?assertEqual([<<"RUN_STARTED">>,
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
        %% 注意文本三件套在 STEP_FINISHED **之后**才整块出现：上游的
        %% beamai_agent:resume/3 没有流式版本（不设 stream_token_handler），
        %% 续跑那一轮走的是非流式 invoke_chat，一个 token 都不产生。
        %% emitter 的非流式回落把整条文本在 on_message_end 时一次性补齐——
        %% 事件流仍然合法，只是 HITL 之后的答案没有打字机效果。
        %% 这条缺口记在 README（需要上游给 resume 加流式入口）。
        ?assertEqual([<<"RUN_STARTED">>, <<"TOOL_CALL_RESULT">>,
                      <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
                      <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                      <<"TEXT_MESSAGE_END">>,
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
        ?assertEqual([<<"RUN_STARTED">>, <<"RUN_ERROR">>], types(Ev)),
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
        ?assertEqual([<<"RUN_STARTED">>,
                      <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
                      <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                      <<"CUSTOM">>, <<"RUN_FINISHED">>], types(Ev)),
        ?assertEqual(none, beamai_agui_thread:pending(<<"t-nostore">>))
    after
        meck:unload(beamai_chat_model),
        application:unset_env(beamai_agui, pause_store_name)
    end.
