%%%-------------------------------------------------------------------
%%% @doc RunAgentInput 解析测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_types_tests).

-include_lib("eunit/include/eunit.hrl").

body() ->
    beamai_utils:encode_json(#{
        <<"threadId">> => <<"t1">>,
        <<"runId">> => <<"r1">>,
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => <<"first">>},
            #{<<"role">> => <<"assistant">>, <<"content">> => <<"reply">>},
            #{<<"role">> => <<"user">>, <<"content">> => <<"second">>}
        ],
        <<"tools">> => [],
        <<"state">> => #{<<"k">> => 1},
        <<"forwardedProps">> => #{<<"agent">> => <<"my-agent">>}
    }).

parse_camel_case_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(body()),
    ?assertEqual(<<"t1">>, beamai_agui_types:thread_id(Input)),
    ?assertEqual(<<"r1">>, beamai_agui_types:run_id(Input)),
    ?assertEqual(#{<<"k">> => 1}, beamai_agui_types:state(Input)),
    ?assertEqual(<<"my-agent">>, beamai_agui_types:agent_name(Input)).

%% 缺 threadId / runId 时自动生成（首轮对话本来就没有 thread）
generates_missing_ids_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    beamai_utils:encode_json(#{<<"messages">> => []})),
    ?assertMatch(<<"thread_", _/binary>>, beamai_agui_types:thread_id(Input)),
    ?assertMatch(<<"run_", _/binary>>, beamai_agui_types:run_id(Input)).

%% 只取最后一条 user 消息：跨轮历史归 memory filter 管，全量回灌会重复计入
last_user_message_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(body()),
    ?assertEqual({ok, <<"second">>}, beamai_agui_types:last_user_message(Input)).

no_user_message_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    beamai_utils:encode_json(
                      #{<<"messages">> => [#{<<"role">> => <<"assistant">>,
                                             <<"content">> => <<"x">>}]})),
    ?assertEqual({error, no_user_message},
                 beamai_agui_types:last_user_message(Input)).

invalid_body_test() ->
    ?assertEqual({error, invalid_body}, beamai_agui_types:parse_run_input(<<"nonsense">>)),
    ?assertEqual({error, invalid_body}, beamai_agui_types:parse_run_input(<<>>)).

%% 字段类型不对时退回缺省，而不是让整个请求崩掉
tolerates_wrong_types_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"messages">> => <<"not-a-list">>,
                      <<"forwardedProps">> => <<"not-a-map">>}),
    ?assertEqual([], beamai_agui_types:messages(Input)),
    ?assertEqual(#{}, beamai_agui_types:forwarded_props(Input)),
    ?assertEqual(undefined, beamai_agui_types:agent_name(Input)).

%%====================================================================
%% resume 输入解析
%%====================================================================

resume_body(Extra) ->
    maps:merge(#{<<"threadId">> => <<"t1">>,
                 <<"messages">> => [#{<<"role">> => <<"user">>,
                                      <<"content">> => <<"yes please">>},
                                    #{<<"role">> => <<"tool">>,
                                      <<"toolCallId">> => <<"c1">>,
                                      <<"content">> => <<"tool-said">>}]},
               Extra).

%% 优先级一：forwardedProps.resume 显式决策压过其它一切
explicit_resume_wins_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body(#{<<"forwardedProps">> =>
                                      #{<<"resume">> =>
                                            #{<<"decision">> => <<"approved">>}}})),
    ?assertEqual({ok, <<"approved">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% payload 的 binary 键转成上游要的 atom 键（只认 message / args）
explicit_resume_payload_keys_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body(#{<<"forwardedProps">> =>
                                      #{<<"resume">> =>
                                            #{<<"decision">> => <<"reply">>,
                                              <<"payload">> =>
                                                  #{<<"message">> => <<"m">>,
                                                    <<"args">> => #{<<"a">> => 1},
                                                    <<"junk">> => <<"x">>}}}})),
    ?assertEqual({ok, <<"reply">>, #{message => <<"m">>, args => #{<<"a">> => 1}}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% 优先级二：对得上 id 的 tool 消息 → reply 语义（AG-UI 原生路径）
tool_message_resume_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(resume_body(#{})),
    ?assertEqual({ok, <<"reply">>, #{message => <<"tool-said">>}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% id 对不上就不能当成这次暂停的答复，退到 user 文本
tool_message_id_must_match_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(resume_body(#{})),
    ?assertEqual({ok, <<"yes please">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"other-id">>)).

%% 优先级三：人直接回一句话（聊天式 HITL）
user_text_resume_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go on">>}]}),
    ?assertEqual({ok, <<"go on">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% 什么都没有 → none（卡在暂停上的 thread 推不动）
no_resume_input_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>, <<"messages">> => []}),
    ?assertEqual(none, beamai_agui_types:resume_input(Input, <<"c1">>)).
