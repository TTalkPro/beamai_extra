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

%%====================================================================
%% `resume[]`：AG-UI interrupt 协议的答复数组
%%
%% 上一条 run 以 `RUN_FINISHED + outcome:"interrupt"` 收口，客户端把人的决定
%% 放在**下一次 run 的请求体**里带回来 —— 协议里没有单独的审批端点。
%%====================================================================

resume_body_with(Resume) ->
    #{<<"threadId">> => <<"t1">>,
      <<"resume">> => Resume,
      <<"messages">> => [#{<<"role">> => <<"user">>, <<"content">> => <<"yes please">>}]}.

%% interruptId 对得上被中断的调用 → 采纳
protocol_resume_matches_interrupt_id_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body_with([#{<<"interruptId">> => <<"c1">>,
                                        <<"status">> => <<"approved">>}])),
    ?assertEqual({ok, <<"approved">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% 措辞归一到上游认的关键词（accept/deny/respond 都是客户端的写法）
protocol_resume_normalizes_decision_test() ->
    Decide = fun(Status) ->
        {ok, Input} = beamai_agui_types:parse_run_input(
                        resume_body_with([#{<<"interruptId">> => <<"c1">>,
                                            <<"status">> => Status}])),
        {ok, D, _} = beamai_agui_types:resume_input(Input, <<"c1">>),
        D
    end,
    ?assertEqual(<<"approved">>, Decide(<<"accept">>)),
    ?assertEqual(<<"approved">>, Decide(<<"Approved">>)),
    ?assertEqual(<<"rejected">>, Decide(<<"deny">>)),
    ?assertEqual(<<"reply">>, Decide(<<"respond">>)).

%% 只有一条且不带 id 时宽容采纳（不少客户端只发 `[{status: "approved"}]`）
protocol_resume_single_entry_without_id_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body_with([#{<<"status">> => <<"rejected">>}])),
    ?assertEqual({ok, <<"rejected">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% **id 对不上就不认**：拿别的 interrupt 的答复去恢复这一个，是把人的意思安到别处
protocol_resume_stale_id_falls_through_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body_with([#{<<"interruptId">> => <<"old">>,
                                        <<"status">> => <<"approved">>}])),
    %% 退到"人直接回了一句话"那条路，而不是误采纳
    ?assertEqual({ok, <<"yes please">>, #{}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%% payload 是字符串时当作 message（`payload: "ok"`）
protocol_resume_string_payload_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    resume_body_with([#{<<"interruptId">> => <<"c1">>,
                                        <<"status">> => <<"reply">>,
                                        <<"payload">> => <<"done">>}])),
    ?assertEqual({ok, <<"reply">>, #{message => <<"done">>}},
                 beamai_agui_types:resume_input(Input, <<"c1">>)).

%%====================================================================
%% `context`：前端每轮重发的页面状态
%%====================================================================

context_prompt_renders_lines_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"context">> => [#{<<"description">> => <<"当前用户"/utf8>>,
                                          <<"value">> => <<"张三"/utf8>>},
                                        #{<<"value">> => <<"页面：设置"/utf8>>}]}),
    Prompt = beamai_agui_types:context_prompt(Input),
    ?assert(is_binary(Prompt)),
    %% 抬头是给模型的**出处说明**：不写这几行会被当成用户指令
    ?assertMatch({_, _}, binary:match(Prompt, <<"AG-UI context">>)),
    ?assertMatch({_, _}, binary:match(Prompt, <<"- 当前用户 张三"/utf8>>)),
    ?assertMatch({_, _}, binary:match(Prompt, <<"- 页面：设置"/utf8>>)).

%% 非字符串的 value 按 JSON 打平（路上谁 stringify 由客户端版本决定）
context_prompt_json_value_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"context">> => [#{<<"description">> => <<"选中"/utf8>>,
                                          <<"value">> => #{<<"id">> => 7}}]}),
    ?assertMatch({_, _}, binary:match(beamai_agui_types:context_prompt(Input),
                                      <<"{\"id\":7}">>)).

%% 这轮没上下文 → undefined（不发一段空抬头去占 system prompt）
context_prompt_empty_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(#{<<"threadId">> => <<"t1">>}),
    ?assertEqual(undefined, beamai_agui_types:context_prompt(Input)),
    {ok, Blank} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"context">> => [#{<<"description">> => <<"空"/utf8>>,
                                          <<"value">> => <<>>}]}),
    ?assertEqual(undefined, beamai_agui_types:context_prompt(Blank)).

%%====================================================================
%% 多模态 content：客户端可能发一个部件数组
%%====================================================================

%% 不翻的话下游拿到的是一坨结构体，内容全丢**且不报错**
multimodal_content_is_flattened_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> =>
                                               [#{<<"type">> => <<"text">>,
                                                  <<"text">> => <<"看这张图："/utf8>>},
                                                #{<<"type">> => <<"image">>,
                                                  <<"url">> => <<"http://x/y.png">>},
                                                #{<<"type">> => <<"text">>,
                                                  <<"text">> => <<"是什么？"/utf8>>}]}]}),
    ?assertEqual({ok, <<"看这张图：是什么？"/utf8>>},
                 beamai_agui_types:last_user_message(Input)).

content_to_text_shapes_test() ->
    ?assertEqual(<<"plain">>, beamai_agui_types:content_to_text(<<"plain">>)),
    ?assertEqual(<<>>, beamai_agui_types:content_to_text(null)),
    ?assertEqual(<<"ab">>, beamai_agui_types:content_to_text([<<"a">>, <<"b">>])).

%%====================================================================
%% parentRunId：原样带着走，我们不解释它
%%====================================================================

parent_run_id_test() ->
    {ok, With} = beamai_agui_types:parse_run_input(
                   #{<<"threadId">> => <<"t1">>, <<"parentRunId">> => <<"p1">>}),
    ?assertEqual(<<"p1">>, beamai_agui_types:parent_run_id(With)),
    {ok, Without} = beamai_agui_types:parse_run_input(#{<<"threadId">> => <<"t1">>}),
    ?assertEqual(undefined, beamai_agui_types:parent_run_id(Without)).
