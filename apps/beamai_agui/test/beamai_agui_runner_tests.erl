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
            %% 第一轮：模型要调工具
            <<"STEP_STARTED">>, <<"STEP_FINISHED">>,
            <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
            <<"TOOL_CALL_RESULT">>,
            %% 第二轮：逐 token 吐最终答案
            <<"STEP_STARTED">>,
            <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
            <<"TEXT_MESSAGE_CONTENT">>,
            <<"STEP_FINISHED">>, <<"TEXT_MESSAGE_END">>,
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
