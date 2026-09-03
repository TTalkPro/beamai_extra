%%%-------------------------------------------------------------------
%%% @doc 增量翻译：provider 原始 chunk → TOOL_CALL_ARGS / REASONING_*
%%%
%%% 直接喂 chunk、看吐出来的事件。真 provider 的形状在这里以合成 chunk 复现，
%%% 两家（OpenAI 系 / Anthropic 系）各一组。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_raw_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

new_emitter() ->
    Self = self(),
    beamai_agui_emitter:new(#{thread_id => <<"t1">>, run_id => <<"r1">>,
                              sink => fun(Ev) -> Self ! {ev, Ev}, ok end,
                              raw_translator => beamai_agui_raw:translator()}).

drain() ->
    receive {ev, Ev} -> [Ev | drain()] after 0 -> [] end.

types() ->
    [maps:get(<<"type">>, Ev) || Ev <- drain()].

cbs(E) -> beamai_agui_emitter:callbacks(E).

%% OpenAI 系 chunk
oa(Delta) -> #{<<"choices">> => [#{<<"delta">> => Delta}]}.

oa_tool(Index, Id, Name, Args) ->
    Fun0 = case Name of
        undefined -> #{};
        _ -> #{<<"name">> => Name}
    end,
    Fun = case Args of
        undefined -> Fun0;
        _ -> Fun0#{<<"arguments">> => Args}
    end,
    TC0 = #{<<"index">> => Index, <<"function">> => Fun},
    TC = case Id of
        undefined -> TC0;
        _ -> TC0#{<<"id">> => Id}
    end,
    oa(#{<<"tool_calls">> => [TC]}).

%% Anthropic 系 chunk
ant_start(Index, Id, Name) ->
    #{<<"type">> => <<"content_block_start">>, <<"index">> => Index,
      <<"content_block">> => #{<<"type">> => <<"tool_use">>,
                               <<"id">> => Id, <<"name">> => Name}}.

ant_args(Index, Partial) ->
    #{<<"type">> => <<"content_block_delta">>, <<"index">> => Index,
      <<"delta">> => #{<<"type">> => <<"input_json_delta">>,
                       <<"partial_json">> => Partial}}.

ant_thinking(Text) ->
    #{<<"type">> => <<"content_block_delta">>, <<"index">> => 0,
      <<"delta">> => #{<<"type">> => <<"thinking_delta">>, <<"thinking">> => Text}}.

tc(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.

%%====================================================================
%% 工具参数增量
%%====================================================================

%% OpenAI：首片带 id/name，后续只有 index + arguments 片段
openai_tool_args_streamed_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(oa_tool(0, <<"c1">>, <<"weather">>, <<"{\"ci">>), #{}),
    RE(oa_tool(0, undefined, undefined, <<"ty\":\"SH\"}">>), #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c1">>, <<"weather">>, <<"{\"city\":\"SH\"}">>)]},
       #{message_id => <<"m1">>}),
    Events = drain(),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>],
                 [maps:get(<<"type">>, Ev) || Ev <- Events]),
    %% 参数是**两段拼起来**的，不是 on_message_end 补的那一整块
    Deltas = [maps:get(<<"delta">>, Ev) || Ev <- Events,
              maps:get(<<"type">>, Ev) =:= <<"TOOL_CALL_ARGS">>],
    ?assertEqual([<<"{\"ci">>, <<"ty\":\"SH\"}">>], Deltas),
    %% START 挂在当前这条 assistant 消息下
    [Start | _] = Events,
    ?assertEqual(<<"m1">>, maps:get(<<"parentMessageId">>, Start)),
    beamai_agui_emitter:close(E).

%% Anthropic：content_block_start 开调用，input_json_delta 送片段
anthropic_tool_args_streamed_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(ant_start(1, <<"c1">>, <<"weather">>), #{}),
    RE(ant_args(1, <<"{\"ci">>), #{}),
    RE(ant_args(1, <<"ty\":\"SH\"}">>), #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c1">>, <<"weather">>, <<"{}">>)]},
       #{message_id => <<"m1">>}),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>], types()),
    beamai_agui_emitter:close(E).

%% 没流过的调用仍走一次性回落（两条路不会都发 START）
unstreamed_tool_falls_back_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c9">>, <<"other">>, <<"{}">>)]},
       #{message_id => <<"m1">>}),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>],
                 types()),
    beamai_agui_emitter:close(E).

%% 同一批里一个流过、一个没流过：各走各的，都只有一条 START
mixed_streamed_and_not_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(oa_tool(0, <<"c1">>, <<"a">>, <<"{}">>), #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c1">>, <<"a">>, <<"{}">>),
                        tc(<<"c2">>, <<"b">>, <<"{}">>)]},
       #{message_id => <<"m1">>}),
    Events = drain(),
    Starts = [maps:get(<<"toolCallId">>, Ev) || Ev <- Events,
              maps:get(<<"type">>, Ev) =:= <<"TOOL_CALL_START">>],
    ?assertEqual([<<"c1">>, <<"c2">>], Starts),
    Ends = [maps:get(<<"toolCallId">>, Ev) || Ev <- Events,
            maps:get(<<"type">>, Ev) =:= <<"TOOL_CALL_END">>],
    ?assertEqual([<<"c1">>, <<"c2">>], Ends),
    beamai_agui_emitter:close(E).

%% index 是每次 LLM 调用内的序号：下一条消息从头记，不能串到上一条的 id 上
tool_index_resets_per_message_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(oa_tool(0, <<"c1">>, <<"a">>, <<"{}">>), #{}),
    ME(#{role => assistant, content => null, tool_calls => [tc(<<"c1">>, <<"a">>, <<"{}">>)]},
       #{message_id => <<"m1">>}),
    _ = drain(),
    %% 第二条消息的 index 0 还没开头就来了片段 → 丢掉，不该错记到 c1 上
    MS(<<"m2">>, #{}),
    RE(oa_tool(0, undefined, undefined, <<"orphan">>), #{}),
    ?assertEqual([], types()),
    beamai_agui_emitter:close(E).

%%====================================================================
%% 思维链
%%====================================================================

%% OpenAI 系的 reasoning_content：第一片补 START + MESSAGE_START
openai_reasoning_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_token := OT} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(oa(#{<<"reasoning_content">> => <<"think">>}), #{}),
    RE(oa(#{<<"reasoning_content">> => <<"more">>}), #{}),
    %% 正文一开口，推理段就收掉（MESSAGE_END 排在 END 前）
    OT(<<"answer">>, #{message_id => <<"m1">>}),
    ?assertEqual([<<"REASONING_START">>, <<"REASONING_MESSAGE_START">>,
                  <<"REASONING_MESSAGE_CONTENT">>, <<"REASONING_MESSAGE_CONTENT">>,
                  <<"REASONING_MESSAGE_END">>, <<"REASONING_END">>,
                  <<"TEXT_MESSAGE_START">>, <<"TEXT_MESSAGE_CONTENT">>], types()),
    beamai_agui_emitter:close(E).

%% Anthropic 的 thinking_delta 同理；没有正文时由 on_message_end 收尾
anthropic_thinking_closed_by_message_end_test() ->
    E = new_emitter(),
    #{on_message_start := MS, on_llm_event := RE, on_message_end := ME} = cbs(E),
    MS(<<"m1">>, #{}),
    RE(ant_thinking(<<"hmm">>), #{}),
    ME(#{role => assistant, content => null,
         tool_calls => [tc(<<"c1">>, <<"a">>, <<"{}">>)]},
       #{message_id => <<"m1">>}),
    ?assertEqual([<<"REASONING_START">>, <<"REASONING_MESSAGE_START">>,
                  <<"REASONING_MESSAGE_CONTENT">>,
                  <<"REASONING_MESSAGE_END">>, <<"REASONING_END">>,
                  <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>],
                 types()),
    beamai_agui_emitter:close(E).

%% 出错时推理段也要收干净，不留不成对的 START
run_error_closes_reasoning_test() ->
    E = new_emitter(),
    #{on_llm_event := RE} = cbs(E),
    RE(oa(#{<<"reasoning_content">> => <<"think">>}), #{}),
    ok = beamai_agui_emitter:run_error(E, boom),
    ?assertEqual([<<"REASONING_START">>, <<"REASONING_MESSAGE_START">>,
                  <<"REASONING_MESSAGE_CONTENT">>,
                  <<"REASONING_MESSAGE_END">>, <<"REASONING_END">>,
                  <<"RUN_ERROR">>], types()),
    beamai_agui_emitter:close(E).

%%====================================================================
%% 认不出的形状
%%====================================================================

%% 陌生 chunk 安静丢弃：这条通道是旁路增强，不该让主流程有感知
unknown_chunks_are_dropped_test() ->
    E = new_emitter(),
    #{on_llm_event := RE} = cbs(E),
    RE(#{<<"type">> => <<"message_start">>, <<"message">> => #{<<"content">> => []}}, #{}),
    RE(#{<<"choices">> => [#{<<"delta">> => #{<<"content">> => <<"text">>}}]}, #{}),
    RE(#{<<"whatever">> => 1}, #{}),
    RE(<<"not even a map">>, #{}),
    ?assertEqual([], types()),
    beamai_agui_emitter:close(E).
