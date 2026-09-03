%%%-------------------------------------------------------------------
%%% @doc 翻译层测试：直接驱动回调，断言事件序列
%%%
%%% 这里不碰 agent、不碰 LLM——只喂回调、看吐出来的事件对不对。
%%% 端到端的接线由 beamai_agui_runner_tests 覆盖。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_emitter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

new_emitter() ->
    Self = self(),
    beamai_agui_emitter:new(#{thread_id => <<"t1">>, run_id => <<"r1">>,
                              sink => fun(Ev) -> Self ! {ev, Ev}, ok end}).

%% 收集已发出的事件（按序）
drain() ->
    receive {ev, Ev} -> [Ev | drain()] after 0 -> [] end.

types() ->
    [maps:get(<<"type">>, Ev) || Ev <- drain()].

%%====================================================================
%% 文本消息
%%====================================================================

%% 流式：START 在第一个 token 时惰性发出，END 由 on_message_end 闭合
streaming_text_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_token := OT, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    OT(<<"he">>, #{message_id => <<"m1">>}),
    OT(<<"llo">>, #{message_id => <<"m1">>}),
    ME(#{role => assistant, content => <<"hello">>}, #{message_id => <<"m1">>}),
    ?assertEqual([<<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                  <<"TEXT_MESSAGE_CONTENT">>, <<"TEXT_MESSAGE_END">>], types()),
    beamai_agui_emitter:close(E).

%% 纯 tool_calls 的回合一个字都没有：不该冒出空的 START/END 对
tool_calls_only_emits_no_text_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    ME(#{role => assistant, content => null, tool_calls => [tc(<<"c1">>, <<"t1">>)]},
       #{message_id => <<"m1">>}),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>],
                 types()),
    beamai_agui_emitter:close(E).

%% 非流式（run/2，没有 token）：on_message_end 一次性补齐三件套
non_streaming_text_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    ME(#{role => assistant, content => <<"whole">>}, #{message_id => <<"m1">>}),
    ?assertEqual([<<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                  <<"TEXT_MESSAGE_END">>], types()),
    beamai_agui_emitter:close(E).

%% 消息落定但没有内容（LLM 出错）：什么都不发，交给 RUN_ERROR 收尾
empty_message_end_emits_nothing_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    ME(undefined, #{message_id => <<"m1">>}),
    ?assertEqual([], types()),
    beamai_agui_emitter:close(E).

%% 文本流开着时出错：run_error 先闭合它，不留不成对的 START
run_error_closes_open_text_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_token := OT} = beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    OT(<<"partial">>, #{message_id => <<"m1">>}),
    ok = beamai_agui_emitter:run_error(E, boom),
    ?assertEqual([<<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>,
                  <<"TEXT_MESSAGE_END">>, <<"RUN_ERROR">>], types()),
    beamai_agui_emitter:close(E).

%% 两条消息各自成对，token 按 message_id 归到正确的那条
two_messages_dont_bleed_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_token := OT, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    OT(<<"a">>, #{message_id => <<"m1">>}),
    ME(#{role => assistant, content => <<"a">>}, #{message_id => <<"m1">>}),
    MS(<<"m2">>, #{}),
    OT(<<"b">>, #{message_id => <<"m2">>}),
    ME(#{role => assistant, content => <<"b">>}, #{message_id => <<"m2">>}),
    Ids = [maps:get(<<"messageId">>, Ev) || Ev <- drain()],
    ?assertEqual([<<"m1">>, <<"m1">>, <<"m1">>, <<"m2">>, <<"m2">>, <<"m2">>], Ids),
    beamai_agui_emitter:close(E).

%%====================================================================
%% 工具
%%====================================================================

%% 工具结果靠 tool_call_id 配回调用（并发下顺序不定，工具名配不出来）
tool_result_uses_tool_call_id_test() ->
    E = new_emitter(),
    #{on_tool_result := TR} = beamai_agui_emitter:callbacks(E),
    TR(<<"t1">>, <<"out">>, #{tool_call_id => <<"c1">>}),
    [Ev] = drain(),
    ?assertEqual(<<"TOOL_CALL_RESULT">>, maps:get(<<"type">>, Ev)),
    ?assertEqual(<<"c1">>, maps:get(<<"toolCallId">>, Ev)),
    ?assertEqual(<<"out">>, maps:get(<<"content">>, Ev)),
    beamai_agui_emitter:close(E).

%% 旧签名（Fun/2，Info 里没有 tool_call_id）配不出来，只能丢——不能瞎猜
tool_result_without_id_dropped_test() ->
    E = new_emitter(),
    #{on_tool_result := TR} = beamai_agui_emitter:callbacks(E),
    TR(<<"t1">>, <<"out">>, #{}),
    ?assertEqual([], types()),
    beamai_agui_emitter:close(E).

%% TOOL_CALL_ARGS 的 delta 必须是 JSON 文本，不是 map
tool_call_args_is_json_text_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_message_end := ME} =
        beamai_agui_emitter:callbacks(E),
    MS(<<"m1">>, #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c1">>, <<"t1">>, <<"{\"city\":\"SH\"}">>)]},
       #{message_id => <<"m1">>}),
    [_Start, Args, _End] = drain(),
    Delta = maps:get(<<"delta">>, Args),
    ?assert(is_binary(Delta)),
    ?assertEqual(#{<<"city">> => <<"SH">>}, beamai_utils:parse_json(Delta)),
    beamai_agui_emitter:close(E).

%%====================================================================
%% 步骤
%%====================================================================

%% STEP_STARTED / STEP_FINISHED 用同一个序号，逐轮递增
step_pairs_test() ->
    E = new_emitter(),
    #{on_llm_call := LC, on_llm_result := LR} = beamai_agui_emitter:callbacks(E),
    LC([], #{}), LR(#{}, #{}),
    LC([], #{}), LR(#{}, #{}),
    Names = [maps:get(<<"stepName">>, Ev) || Ev <- drain()],
    ?assertEqual([<<"step-1">>, <<"step-1">>, <<"step-2">>, <<"step-2">>], Names),
    beamai_agui_emitter:close(E).

%%====================================================================
%% 辅助
%%====================================================================

tc(Id, Name) -> tc(Id, Name, <<"{}">>).

tc(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.
