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
      fun store_down_degrades_to_no_hitl/0,
      fun interrupt_uses_agui_outcome/0,
      fun protocol_resume_round_trip/0,
      fun approved_and_rejected_differ/0,
      fun status_is_not_a_decision/0,
      fun frontend_tool_asks_for_a_result/0,
      fun frontend_tool_result_is_not_fabricated/0]}.

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
        %% 收口只有 `RUN_FINISHED + outcome:"interrupt"` 这一条 —— 曾经还并发一条
        %% `CUSTOM(interrupt)`，同一件事两份迟早不同步，已经去掉
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>,
                      <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
                      <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                      <<"RUN_FINISHED">>], types(Ev1)),
        %% 前端看到的是一个**没有结果**的工具调用——那就是"等你回话"
        ?assertEqual([], [E || E <- Ev1,
                              maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>]),
        %% outcome 告诉前端在等哪次调用
        [Finished1] = [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [Int1] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, Finished1)),
        ?assertEqual(<<"ask1">>, maps:get(<<"toolCallId">>, Int1)),

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
        ?assertMatch([_], [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>]),
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
                      <<"RUN_FINISHED">>], types(Ev)),
        ?assertEqual(none, beamai_agui_thread:pending(<<"t-nostore">>))
    after
        meck:unload(beamai_chat_model),
        application:unset_env(beamai_agui, pause_store_name)
    end.

%% AG-UI 的 interrupt 协议：**收口与告知是同一条事件**
%%
%% `RUN_FINISHED` 让流关得掉（run 必须有终态），`outcome` 又说清了「不是跑完了，
%% 是停在这儿等人」。通用客户端（`useInterrupt` / `useHumanInTheLoop`）认的是
%% 这一条 —— `CUSTOM(interrupt)` 是我们先于协议实现的那条，只有自家前端认。
interrupt_uses_agui_outcome() ->
    Thread = <<"t-outcome">>,
    _CC = mock_llm(),
    try
        Events = run(input(Thread, [user_msg(<<"go">>)])),
        [Finished] = [E || E <- Events, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        Outcome = maps:get(<<"outcome">>, Finished),
        ?assertEqual(<<"interrupt">>, maps:get(<<"type">>, Outcome)),
        [Interrupt] = maps:get(<<"interrupts">>, Outcome),
        %% id 取被中断调用的 tool-call id：客户端把它原样放回 resume[].interruptId，
        %% 服务端据此认得出"回的是不是当前这一条"
        ?assertEqual(<<"ask1">>, maps:get(<<"id">>, Interrupt)),
        ?assertEqual(<<"ask1">>, maps:get(<<"toolCallId">>, Interrupt)),
        %% responseSchema 让通用客户端知道该渲染什么控件
        Schema = maps:get(<<"responseSchema">>, Interrupt),
        ?assertEqual([<<"decision">>], maps:get(<<"required">>, Schema)),
        %% metadata 带着被中断的工具是谁、参数是什么
        Pending = maps:get(<<"pendingTool">>, maps:get(<<"metadata">>, Interrupt)),
        ?assertEqual(<<"ask_human">>, maps:get(<<"name">>, Pending))
    after
        meck:unload(beamai_chat_model)
    end.

%% 走协议正路续跑：客户端把决定放在**下一次 run 的 `resume[]`** 里带回来
protocol_resume_round_trip() ->
    Thread = <<"t-protocol">>,
    _CC = mock_llm(),
    try
        Ev1 = run(input(Thread, [user_msg(<<"go">>)])),
        [Finished] = [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [Interrupt] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, Finished)),
        InterruptId = maps:get(<<"id">>, Interrupt),

        %% 请求二：只带 resume[]，连 user 消息都不用有
        {ok, Input2} = beamai_agui_types:parse_run_input(
                         #{<<"threadId">> => Thread,
                           <<"messages">> => [user_msg(<<"go">>)],
                           <<"resume">> => [#{<<"interruptId">> => InterruptId,
                                              <<"status">> => <<"reply">>,
                                              <<"payload">> => #{<<"message">> => <<"yes">>}}]}),
        Ev2 = run(Input2, factory()),
        [Result] = [E || E <- Ev2, maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(InterruptId, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"yes">>, maps:get(<<"content">>, Result)),
        %% 跑到终态，暂停清掉
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 决策：`approved` 与 `rejected` 必须**不一样**
%%
%% 这一族守的是下游报上来的那条账：只把 `decision` 换个词、其余逐字相同时，
%% 两次的 `TOOL_CALL_RESULT` 曾经**一模一样**（都是 `"resolved"` —— 协议字段
%% `status` 的值），于是界面上两颗钮点哪个都一样，而且一个字都不报。
%%====================================================================

resume_body(Thread, Entry) ->
    #{<<"threadId">> => Thread,
      <<"messages">> => [user_msg(<<"go">>)],
      <<"resume">> => [Entry]}.

%% @private 重新装一遍剧本 —— mock 的轮次计数器是**整个 mock 一份**的，
%% 一个用例里跑两轮暂停就得从头来，否则第二轮拿到的是"最终答案"那一幕
remock() ->
    catch meck:unload(beamai_chat_model),
    mock_llm().

%% @private 跑一轮到暂停，返回那条 interrupt
pause_at(Thread) ->
    Ev = run(input(Thread, [user_msg(<<"go">>)])),
    [Finished] = [E || E <- Ev, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
    [Interrupt] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, Finished)),
    {Interrupt, Ev}.

result_content(Events) ->
    case [E || E <- Events, maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>] of
        [R | _] -> maps:get(<<"content">>, R);
        [] -> undefined
    end.

approved_and_rejected_differ() ->
    _CC = mock_llm(),
    try
        %% 批准：服务端去执行那个工具，**这里不该有人造的结果**
        {I1, _} = pause_at(<<"t-approved">>),
        _ = remock(),
        {ok, In1} = beamai_agui_types:parse_run_input(
                      resume_body(<<"t-approved">>,
                                  #{<<"interruptId">> => maps:get(<<"id">>, I1),
                                    <<"status">> => <<"resolved">>,
                                    <<"payload">> => #{<<"decision">> => <<"approved">>}})),
        Approved = run(In1, factory()),
        _ = remock(),

        %% 拒绝：模型必须知道这一步**没做成**
        {I2, _} = pause_at(<<"t-rejected">>),
        _ = remock(),
        {ok, In2} = beamai_agui_types:parse_run_input(
                      resume_body(<<"t-rejected">>,
                                  #{<<"interruptId">> => maps:get(<<"id">>, I2),
                                    <<"status">> => <<"resolved">>,
                                    <<"payload">> => #{<<"decision">> => <<"rejected">>}})),
        Rejected = run(In2, factory()),

        ?assertNotEqual(result_content(Approved), result_content(Rejected)),
        ?assertMatch({_, _}, binary:match(result_content(Rejected), <<"拒绝"/utf8>>))
    after
        meck:unload(beamai_chat_model)
    end.

%% `status` 是**协议层**的「这条 interrupt 答完了」，不是人做的决定 ——
%% 把它当成工具的返回值发给模型，是在编造一个没发生过的结果
status_is_not_a_decision() ->
    _CC = mock_llm(),
    try
        {I, _} = pause_at(<<"t-status">>),
        {ok, In} = beamai_agui_types:parse_run_input(
                     resume_body(<<"t-status">>,
                                 #{<<"interruptId">> => maps:get(<<"id">>, I),
                                   <<"status">> => <<"resolved">>,
                                   <<"payload">> => #{<<"decision">> => <<"rejected">>}})),
        Content = result_content(run(In, factory())),
        ?assertNotEqual(<<"resolved">>, Content)
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% 前端工具：要的是**结果**，不是决定
%%====================================================================

%% @private 前端声明的工具（服务端没有实现），随 `RunAgentInput.tools` 上来
frontend_input(Thread, Extra) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    maps:merge(#{<<"threadId">> => Thread,
                                 <<"messages">> => [user_msg(<<"go">>)],
                                 <<"tools">> => [#{<<"name">> => <<"ask_human">>,
                                                   <<"description">> => <<"say hi">>,
                                                   <<"parameters">> => #{}}]},
                               Extra)),
    Input.

%% 前端工具的 interrupt 要的是这次调用**返回了什么** —— 拿审批那张 schema 给它，
%% 客户端照着填只能填出个 `{decision}`，填不出结果来
frontend_tool_asks_for_a_result() ->
    _CC = mock_llm(),
    try
        Ev = run(frontend_input(<<"t-fe-schema">>, #{})),
        [Finished] = [E || E <- Ev, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [Interrupt] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, Finished)),
        ?assertEqual([<<"result">>], maps:get(<<"required">>,
                                              maps:get(<<"responseSchema">>, Interrupt))),
        Meta = maps:get(<<"metadata">>, Interrupt),
        ?assertEqual(<<"frontend-tool">>, maps:get(<<"kind">>, Meta)),
        ?assertMatch({_, _}, binary:match(maps:get(<<"reason">>, Interrupt),
                                          <<"回传结果"/utf8>>))
    after
        meck:unload(beamai_chat_model)
    end.

%% 客户端回传了结果就用它；**没回传就如实说没拿到** —— 服务端替客户端工具伪造
%% 一条成功的结果，模型据此宣布"干完了"，而客户端从头到尾没被调用过
frontend_tool_result_is_not_fabricated() ->
    _CC = mock_llm(),
    try
        %% ① 有结果：原样进 TOOL_CALL_RESULT
        Ev1 = run(frontend_input(<<"t-fe-ok">>, #{})),
        [F1] = [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [I1] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, F1)),
        In1 = frontend_input(<<"t-fe-ok">>,
                             #{<<"resume">> =>
                                   [#{<<"interruptId">> => maps:get(<<"id">>, I1),
                                      <<"status">> => <<"resolved">>,
                                      <<"payload">> =>
                                          #{<<"result">> => <<"已经打过招呼了"/utf8>>}}]}),
        _ = remock(),
        ?assertEqual(<<"已经打过招呼了"/utf8>>, result_content(run(In1, factory()))),
        _ = remock(),

        %% ② 只回了个决定、没给结果：不编造成功
        Ev2 = run(frontend_input(<<"t-fe-missing">>, #{})),
        _ = remock(),
        [F2] = [E || E <- Ev2, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [I2] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, F2)),
        In2 = frontend_input(<<"t-fe-missing">>,
                             #{<<"resume">> =>
                                   [#{<<"interruptId">> => maps:get(<<"id">>, I2),
                                      <<"status">> => <<"resolved">>,
                                      <<"payload">> =>
                                          #{<<"decision">> => <<"approved">>}}]}),
        Content = result_content(run(In2, factory())),
        ?assertMatch({_, _}, binary:match(Content, <<"没有回传"/utf8>>))
    after
        meck:unload(beamai_chat_model)
    end.
