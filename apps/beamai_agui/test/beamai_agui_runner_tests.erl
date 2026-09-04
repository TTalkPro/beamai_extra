%%%-------------------------------------------------------------------
%%% @doc 端到端：真 agent 跑一轮工具循环，看吐出来的 AG-UI 事件流
%%%
%%% 这个用例的价值在于**证明翻译层对上游回调的假设成立**：message_id 确实
%%% 在 on_token 的 Meta 里、tool_call_id 确实在 on_tool_result 的 Info 里、
%%% 边界确实成对。emitter 的单测是喂假回调，这里喂的是 beamai_agent 真发的。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_runner_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

%% mock：首轮发 tool_call，次轮逐 token 吐文本
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
                        #{content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"he">>, #{}),
                    TokenCb(<<"llo">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"hello">>, finish_reason => <<"stop">>})}
            end
        end),
    CC.

factory() ->
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(K0, #{name => <<"t1">>, parameters => #{},
                                              handler => fun(_) -> {ok, <<"TOOL-OUT">>} end}),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

input() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>, <<"runId">> => <<"r1">>,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go">>}]}),
    Input.

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

run_and_collect(Factory) ->
    Self = self(),
    ok = beamai_agui_runner:run(input(), Factory, fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

%%====================================================================
%% 用例
%%====================================================================

%% 一轮工具循环的完整事件流
full_run_event_sequence_test() ->
    _CC = mock_llm(),
    try
        Events = run_and_collect(factory()),
        Types = [maps:get(<<"type">>, E) || E <- Events],
        ?assertEqual([
            <<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>,
            %% 第一轮：模型要调工具。**STEP_FINISHED 排在工具调用闭合之后** ——
            %% 步骤是容器，容器最后关
            <<"STEP_STARTED">>,
            <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
            <<"STEP_FINISHED">>,
            <<"TOOL_CALL_RESULT">>,
            %% 第二轮：逐 token 吐最终答案
            <<"STEP_STARTED">>,
            <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
            <<"TEXT_MESSAGE_CONTENT">>,
            <<"TEXT_MESSAGE_END">>, <<"STEP_FINISHED">>,
            <<"RUN_FINISHED">>
        ], Types)
    after
        meck:unload(beamai_chat_model)
    end.

%% 工具调用与它的结果由同一个 toolCallId 串起来
tool_call_correlates_with_result_test() ->
    _CC = mock_llm(),
    try
        Events = run_and_collect(factory()),
        [Start] = [E || E <- Events,
                        maps:get(<<"type">>, E) =:= <<"TOOL_CALL_START">>],
        [Result] = [E || E <- Events,
                         maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(<<"c1">>, maps:get(<<"toolCallId">>, Start)),
        ?assertEqual(<<"c1">>, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"t1">>, maps:get(<<"toolCallName">>, Start)),
        ?assertEqual(<<"TOOL-OUT">>, maps:get(<<"content">>, Result))
    after
        meck:unload(beamai_chat_model)
    end.

%% 文本事件三件套共用同一个 messageId，且它就是发起工具调用那条消息之外的另一条
text_events_share_message_id_test() ->
    _CC = mock_llm(),
    try
        Events = run_and_collect(factory()),
        TextIds = [maps:get(<<"messageId">>, E) || E <- Events,
                   lists:member(maps:get(<<"type">>, E),
                                [<<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                                 <<"TEXT_MESSAGE_END">>])],
        ?assertEqual(4, length(TextIds)),
        ?assertEqual(1, length(lists:usort(TextIds))),
        %% 工具调用挂在**另一条** assistant 消息下
        [Start] = [E || E <- Events,
                        maps:get(<<"type">>, E) =:= <<"TOOL_CALL_START">>],
        ?assertNotEqual(hd(TextIds), maps:get(<<"parentMessageId">>, Start))
    after
        meck:unload(beamai_chat_model)
    end.

%% 没有 user 消息 → RUN_ERROR，不去建 agent
no_user_message_yields_run_error_test() ->
    {ok, Empty} = beamai_agui_types:parse_run_input(#{<<"messages">> => []}),
    Self = self(),
    Never = fun(_I, _Cfg) -> Self ! factory_called, {error, should_not_happen} end,
    ok = beamai_agui_runner:run(Empty, Never, fun(Ev) -> Self ! {ev, Ev}, ok end),
    Types = [maps:get(<<"type">>, E) || E <- collect()],
    ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"RUN_ERROR">>], Types),
    receive factory_called -> ?assert(false) after 0 -> ok end.

%% 工厂返回 error → RUN_ERROR
factory_error_yields_run_error_test() ->
    Self = self(),
    Bad = fun(_I, _Cfg) -> {error, no_model} end,
    ok = beamai_agui_runner:run(input(), Bad, fun(Ev) -> Self ! {ev, Ev}, ok end),
    Types = [maps:get(<<"type">>, E) || E <- collect()],
    ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"RUN_ERROR">>], Types).

%% 工厂抛异常也不会把 run 进程带走，照样收到 RUN_ERROR
factory_crash_is_contained_test() ->
    Self = self(),
    Crash = fun(_I, _Cfg) -> error(boom) end,
    ok = beamai_agui_runner:run(input(), Crash, fun(Ev) -> Self ! {ev, Ev}, ok end),
    Types = [maps:get(<<"type">>, E) || E <- collect()],
    ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>, <<"RUN_ERROR">>], Types).

%%====================================================================
%% `RunAgentInput.context`：前端每轮重发的页面状态
%%
%% 它得**进 system prompt** 才有用 —— state 是给工具读的，模型看不见；context
%% 是说给模型听的。追加而不是覆盖：工厂那份人设是常量配置，不该被一次请求改掉。
%%====================================================================

%% 捕获真正发给模型的消息，看 context 有没有到位
capture_llm(Ref) ->
    Self = self(),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, Messages, _RawCb, _Opts) ->
            Self ! {Ref, Messages},
            {ok, beamai_chat_response:new(#{content => <<"ok">>,
                                            finish_reason => <<"stop">>})}
        end),
    ok.

simple_factory(Prompt) ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        Base = #{chat_client => K, memory => false},
        beamai_agent:new(maps:merge(
            case Prompt of
                undefined -> Base;
                _ -> Base#{system_prompt => Prompt}
            end, RuntimeCfg))
    end.

context_input(Context) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t-ctx">>, <<"runId">> => <<"r1">>,
                      <<"context">> => Context,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go">>}]}),
    Input.

system_text(Messages) ->
    iolist_to_binary([maps:get(content, M, <<>>)
                      || M <- Messages, maps:get(role, M, undefined) =:= system]).

context_is_appended_to_system_prompt_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        ok = beamai_agui_runner:run(
               context_input([#{<<"description">> => <<"current page">>,
                                <<"value">> => <<"settings">>}]),
               simple_factory(<<"You are Bob.">>), fun(_) -> ok end),
        Messages = receive {Ref, M} -> M after 2000 -> [] end,
        Text = system_text(Messages),
        %% 人设还在（追加不覆盖）
        ?assertMatch({_, _}, binary:match(Text, <<"You are Bob.">>)),
        %% 页面状态到了模型眼前
        ?assertMatch({_, _}, binary:match(Text, <<"- current page settings">>)),
        %% 抬头说清了出处：不写它这几行会被当成用户指令
        ?assertMatch({_, _}, binary:match(Text, <<"AG-UI context">>))
    after
        meck:unload(beamai_chat_model)
    end.

%% 工厂没设人设时，context 自己就是那份 system prompt
context_without_system_prompt_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        ok = beamai_agui_runner:run(
               context_input([#{<<"value">> => <<"page: home">>}]),
               simple_factory(undefined), fun(_) -> ok end),
        Messages = receive {Ref, M} -> M after 2000 -> [] end,
        ?assertMatch({_, _}, binary:match(system_text(Messages), <<"- page: home">>))
    after
        meck:unload(beamai_chat_model)
    end.

%% 这轮没 context → 一个字都不加（不拿空抬头去占 system prompt）
no_context_leaves_prompt_alone_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        ok = beamai_agui_runner:run(context_input([]), simple_factory(<<"You are Bob.">>),
                                    fun(_) -> ok end),
        Messages = receive {Ref, M} -> M after 2000 -> [] end,
        ?assertEqual(<<"You are Bob.">>, system_text(Messages))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% parentRunId：原样带着走
%%====================================================================

%% 只有 RUN_STARTED 有这一位（RUN_FINISHED / RUN_ERROR 都没有），
%% 所以父链的声明就这一次机会
parent_run_id_rides_on_run_started_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        {ok, Input} = beamai_agui_types:parse_run_input(
                        #{<<"threadId">> => <<"t-parent">>, <<"runId">> => <<"r1">>,
                          <<"parentRunId">> => <<"p1">>,
                          <<"messages">> => [#{<<"role">> => <<"user">>,
                                               <<"content">> => <<"go">>}]}),
        Self = self(),
        ok = beamai_agui_runner:run(Input, simple_factory(undefined),
                                    fun(Ev) -> Self ! {ev, Ev}, ok end),
        Events = collect(),
        [Started] = [E || E <- Events, maps:get(<<"type">>, E) =:= <<"RUN_STARTED">>],
        ?assertEqual(<<"p1">>, maps:get(<<"parentRunId">>, Started)),
        [Finished] = [E || E <- Events, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        ?assertNot(maps:is_key(<<"parentRunId">>, Finished))
    after
        meck:unload(beamai_chat_model)
    end.

%%====================================================================
%% event_transform：事件流插件的通用挂点
%%
%% A2UI / 生成式 UI 那类插件住在这个位置 —— 它们把工具调用翻成活动事件，
%% 而 runner / emitter 一个字都不用改。
%%====================================================================

transform_run(Transform) ->
    Self = self(),
    ok = beamai_agui_runner:run(context_input([]), simple_factory(undefined),
                                fun(Ev) -> Self ! {ev, Ev}, ok end,
                                #{event_transform => Transform}),
    collect().

%% 改写一条
event_transform_rewrites_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        Events = transform_run(fun(E) -> E#{<<"tagged">> => true} end),
        ?assert(lists:all(fun(E) -> maps:get(<<"tagged">>, E, false) end, Events))
    after
        meck:unload(beamai_chat_model)
    end.

%% 吞掉一条 / 在一条前后插事件
event_transform_drops_and_expands_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        Events = transform_run(fun(E) ->
            case maps:get(<<"type">>, E) of
                <<"STATE_SNAPSHOT">> -> drop;
                <<"RUN_STARTED">> -> [E, beamai_agui_event:custom(<<"hello">>, 1)];
                _ -> E
            end
        end),
        Types = [maps:get(<<"type">>, E) || E <- Events],
        ?assertEqual([], [T || T <- Types, T =:= <<"STATE_SNAPSHOT">>]),
        ?assertEqual([<<"RUN_STARTED">>, <<"CUSTOM">>], lists:sublist(Types, 2))
    after
        meck:unload(beamai_chat_model)
    end.

%% 插件自己炸了不该把 run 带走：它是旁路增强，出事就退回原事件
event_transform_crash_is_contained_test() ->
    Ref = make_ref(),
    ok = capture_llm(Ref),
    try
        Events = transform_run(fun(_) -> error(boom) end),
        Types = [maps:get(<<"type">>, E) || E <- Events],
        ?assertEqual(<<"RUN_STARTED">>, hd(Types)),
        ?assertEqual(<<"RUN_FINISHED">>, lists:last(Types))
    after
        meck:unload(beamai_chat_model)
    end.
