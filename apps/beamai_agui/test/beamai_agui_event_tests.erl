%%%-------------------------------------------------------------------
%%% @doc AG-UI 事件构造与编码测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_event_tests).

-include_lib("eunit/include/eunit.hrl").

%% 每个事件都带 type 与 timestamp（BaseEvent 约定）
base_fields_test() ->
    Ev = beamai_agui_event:run_started(<<"t1">>, <<"r1">>),
    ?assertEqual(<<"RUN_STARTED">>, maps:get(<<"type">>, Ev)),
    ?assert(is_integer(maps:get(<<"timestamp">>, Ev))),
    ?assertEqual(<<"t1">>, maps:get(<<"threadId">>, Ev)),
    ?assertEqual(<<"r1">>, maps:get(<<"runId">>, Ev)).

%% parentMessageId 可选：不给就不该出现这个键（而不是给个 null）
tool_call_start_optional_parent_test() ->
    Bare = beamai_agui_event:tool_call_start(<<"c1">>, <<"t">>, undefined),
    ?assertNot(maps:is_key(<<"parentMessageId">>, Bare)),
    WithParent = beamai_agui_event:tool_call_start(<<"c1">>, <<"t">>, <<"m1">>),
    ?assertEqual(<<"m1">>, maps:get(<<"parentMessageId">>, WithParent)).

%% 代理层可能只能转发不带边界的 token 流；chunk 构造器保留给这类宿主。
text_message_chunk_test() ->
    Ev = beamai_agui_event:text_message_chunk(<<"m1">>, <<"hel">>),
    ?assertEqual(<<"TEXT_MESSAGE_CHUNK">>, maps:get(<<"type">>, Ev)),
    ?assertEqual(<<"hel">>, maps:get(<<"delta">>, Ev)),
    ?assertEqual(<<"assistant">>, maps:get(<<"role">>, Ev)),
    WithRole = beamai_agui_event:text_message_chunk(<<"m1">>, <<"hel">>, <<"reasoning">>),
    ?assertEqual(<<"reasoning">>, maps:get(<<"role">>, WithRole)).

tool_call_chunk_test() ->
    Ev = beamai_agui_event:tool_call_chunk(<<"c1">>, <<"lookup">>, <<"{">>, <<"m1">>),
    ?assertEqual(<<"TOOL_CALL_CHUNK">>, maps:get(<<"type">>, Ev)),
    ?assertEqual(<<"c1">>, maps:get(<<"toolCallId">>, Ev)),
    ?assertEqual(<<"lookup">>, maps:get(<<"toolCallName">>, Ev)),
    ?assertEqual(<<"{">>, maps:get(<<"delta">>, Ev)),
    ?assertEqual(<<"m1">>, maps:get(<<"parentMessageId">>, Ev)).

%% SSE 帧只有 data 一行、以空行收尾——AG-UI 不用具名事件
sse_frame_shape_test() ->
    Frame = beamai_agui_event:to_sse(
              beamai_agui_event:text_message_content(<<"m1">>, <<"hi">>)),
    ?assertMatch(<<"data: ", _/binary>>, Frame),
    ?assertEqual(nomatch, binary:match(Frame, <<"event:">>)),
    Size = byte_size(Frame),
    ?assertEqual(<<"\n\n">>, binary:part(Frame, Size - 2, 2)).

%% 帧里的 JSON 能原样解回事件
sse_frame_roundtrip_test() ->
    Ev = beamai_agui_event:text_message_content(<<"m1">>, <<"hi">>),
    Frame = beamai_agui_event:to_sse(Ev),
    Json = binary:part(Frame, 6, byte_size(Frame) - 8),
    ?assertEqual(Ev, beamai_utils:parse_json(Json)).

%% 工具结果事件的 role 固定是 tool
tool_call_result_role_test() ->
    Ev = beamai_agui_event:tool_call_result(<<"m2">>, <<"c1">>, <<"out">>),
    ?assertEqual(<<"tool">>, maps:get(<<"role">>, Ev)),
    ?assertEqual(<<"c1">>, maps:get(<<"toolCallId">>, Ev)).
