%%%-------------------------------------------------------------------
%%% @doc 子 agent lane：委派出去那一段也变成可渲染的事件
%%%
%%% 这个模块要证明两件事：
%%%
%%% 1. **不改上游就接得上** —— 靠包住 `beamai_agent_delegate` 的 `subagent` fn
%%%    （它返回一份 agent 配置，配置里可以有 callbacks）与工具 handler；
%%% 2. **lane 的每条事件都带 `subagentRunId`** —— 给既有事件多一个字段是安全的
%%%    （AG-UI 的事件 schema 是 passthrough），所以只有 `SUBAGENT_*` 三个新类型
%%%    需要开关，字段不需要。
%%%
%%% 还要证明**关着开关时一行事件都不多** —— 「不塞就什么都不会发生」。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_subagent_tests).

-include_lib("eunit/include/eunit.hrl").

subagent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [{"开着：lane 开合 + 每条事件带 subagentRunId", fun lane_events/0},
      {"关着：一行事件都不多", fun disabled_emits_nothing/0},
      {"委派失败 → SUBAGENT_ERROR，不是静静地没有收口", fun failure_closes_lane/0}]}.

setup() ->
    _ = beamai_subagent_manager:ensure_started(),
    ok.

cleanup(_) ->
    application:unset_env(beamai_agui, subagent_events),
    ok.

%%====================================================================
%% 被测 agent
%%====================================================================

tc(Id, Name, Args) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => Args}}.

%% 父 agent 首轮调委派工具，次轮给答案；子 agent 一句话就完
mock_llm() ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    Respond = fun() ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, beamai_chat_response:new(
                        #{content => null,
                          tool_calls => [tc(<<"d1">>, <<"research">>,
                                            <<"{\"task\":\"dig\"}">>)],
                          finish_reason => <<"tool_calls">>})};
            _ -> {ok, beamai_chat_response:new(
                        #{content => <<"done">>, finish_reason => <<"stop">>})}
        end
    end,
    %% 父走流式（stream/2），子 agent 走非流式（manager 的 do_run 用 run/2）
    meck:expect(beamai_chat_model, stream_chat,
                fun(_C, _M, _R, _O) -> Respond() end),
    meck:expect(beamai_chat_model, chat,
                fun(_C, _M, _O) ->
                    {ok, beamai_chat_response:new(
                           #{content => <<"sub-answer">>, finish_reason => <<"stop">>})}
                end),
    ok.

%% @private 工厂：委派工具由 `beamai_agui_subagent:delegate_tool/2` 包出来，
%% observer 取自 runtime 交下来的配置（开关关着时它是 undefined）
factory() ->
    fun(_Input, RuntimeCfg) ->
        Observer = maps:get(subagent_observer, RuntimeCfg, undefined),
        Delegate = beamai_agui_subagent:delegate_tool(
                     #{name => <<"research">>,
                       subagent => fun(_Args, _Ctx) ->
                           K = beamai_chat_client:add_chat_model(
                                 beamai_chat_client:new(),
                                 beamai_chat_model:create(mock, #{})),
                           #{chat_client => K, memory => false}
                       end}, Observer),
        K = beamai_chat_client:add_tool(
              beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                                beamai_chat_model:create(mock, #{})),
              Delegate),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

input() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t-sub">>, <<"runId">> => <<"r1">>,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go">>}]}),
    Input.

run() ->
    Self = self(),
    ok = beamai_agui_runner:run(input(), factory(), fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

types(Events) -> [beamai_agui_event:type_of(E) || E <- Events].

pick(Type, Events) -> [E || E <- Events, beamai_agui_event:type_of(E) =:= Type].

lane_of(Event) -> maps:get(<<"subagentRunId">>, Event, undefined).

%%====================================================================
%% 用例
%%====================================================================

lane_events() ->
    application:set_env(beamai_agui, subagent_events, true),
    ok = mock_llm(),
    try
        Events = run(),
        [Started] = pick(<<"SUBAGENT_STARTED">>, Events),
        [Finished] = pick(<<"SUBAGENT_FINISHED">>, Events),
        LaneId = maps:get(<<"subagentRunId">>, Started),
        ?assertEqual(<<"research">>, maps:get(<<"name">>, Started)),
        %% 任务原文带上，前端那张卡片要显示「在干什么」
        ?assertEqual(<<"dig">>, maps:get(<<"description">>, Started)),
        ?assertEqual(LaneId, maps:get(<<"subagentRunId">>, Finished)),
        ?assertEqual(#{<<"type">> => <<"success">>}, maps:get(<<"outcome">>, Finished)),

        %% lane 里子 agent 说的话确实出来了，且**每一条都带 subagentRunId**
        Lane = [E || E <- Events, lane_of(E) =:= LaneId,
                     not lists:member(beamai_agui_event:type_of(E),
                                      [<<"SUBAGENT_STARTED">>, <<"SUBAGENT_FINISHED">>])],
        ?assert(length(Lane) > 0),
        ?assert(lists:member(<<"TEXT_MESSAGE_CONTENT">>, types(Lane))),
        [Content] = pick(<<"TEXT_MESSAGE_CONTENT">>, Lane),
        ?assertEqual(<<"sub-answer">>, maps:get(<<"delta">>, Content)),

        %% 主线的事件**不带**这个字段 —— 否则前端把父说的话也画进子卡片里
        [Main] = [E || E <- pick(<<"TEXT_MESSAGE_CONTENT">>, Events),
                       lane_of(E) =:= undefined],
        ?assertEqual(<<"done">>, maps:get(<<"delta">>, Main)),

        %% 开合成对：发了 STARTED 就一定有 FINISHED，否则前端那张卡片永远转圈
        ?assertEqual(length(pick(<<"SUBAGENT_STARTED">>, Events)),
                     length(pick(<<"SUBAGENT_FINISHED">>, Events)))
    after
        meck:unload(beamai_chat_model)
    end.

disabled_emits_nothing() ->
    application:set_env(beamai_agui, subagent_events, false),
    ok = mock_llm(),
    try
        Events = run(),
        ?assertEqual([], pick(<<"SUBAGENT_STARTED">>, Events)),
        ?assertEqual([], pick(<<"SUBAGENT_FINISHED">>, Events)),
        %% 连字段都不多一个
        ?assertEqual([], [E || E <- Events, lane_of(E) =/= undefined]),
        %% 委派本身照跑（工具结果回来了）
        ?assertMatch([_ | _], pick(<<"TOOL_CALL_RESULT">>, Events))
    after
        meck:unload(beamai_chat_model)
    end.

%% 子 agent 起不来时 lane 也要收口 —— `SUBAGENT_STARTED` 发了却没有对应的
%% 收尾事件，前端那张卡片会永远转圈
failure_closes_lane() ->
    application:set_env(beamai_agui, subagent_events, true),
    ok = mock_llm(),
    %% 子 agent 的 LLM 调用炸掉
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) -> {error, boom} end),
    try
        Events = run(),
        ?assertEqual(1, length(pick(<<"SUBAGENT_STARTED">>, Events))),
        Closed = pick(<<"SUBAGENT_FINISHED">>, Events) ++ pick(<<"SUBAGENT_ERROR">>, Events),
        ?assertEqual(1, length(Closed))
    after
        meck:unload(beamai_chat_model)
    end.
