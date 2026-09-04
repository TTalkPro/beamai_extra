%%%-------------------------------------------------------------------
%%% @doc 前端工具：RunAgentInput.tools → interrupt_tools → HITL 管道
%%%
%%% 前端定义、前端执行的工具，服务端只负责「告诉模型有这个东西」和「模型调到
%%% 就停下来把球交出去」。走的是与人工审批完全相同的暂停/续跑路径。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_frontend_tools_tests).

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

frontend_tools_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [fun advertised_to_model/0,
      fun full_round_trip/0,
      fun server_side_tool_wins_on_name_clash/0,
      fun malformed_entries_ignored/0]}.

%%====================================================================
%% 辅助
%%====================================================================

%% mock：首轮调前端工具，之后给最终答案；顺带把下发的 tools 交回测试进程
mock_llm(ToolName) ->
    %% 每个用例会触发多次 LLM 调用，上一个用例的 tools_sent 会留在信箱里串味
    ok = flush_tools(),
    Self = self(),
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_Config, _Messages, _RawCb, Opts) ->
            counters:add(CC, 1, 1),
            Self ! {tools_sent, maps:get(tools, Opts, [])},
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CC, 1) of
                1 ->
                    {ok, beamai_chat_response:new(
                        #{content => null,
                          tool_calls => [#{id => <<"fc1">>, type => <<"function">>,
                                           function => #{name => ToolName,
                                                         arguments => <<"{}">>}}],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"ok">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"ok">>, finish_reason => <<"stop">>})}
            end
        end),
    ok.

%% 工厂：什么中断工具都不声明（前端工具全靠 runtime 挂上去）
plain_factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

%% 工厂：声明了一个同名的服务端侧中断工具
clashing_factory() ->
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                              beamai_chat_model:create(mock, #{})),
        beamai_agent:new(maps:merge(
            #{chat_client => K, memory => false,
              interrupt_tools => [#{name => <<"confirm">>,
                                    description => <<"server side">>,
                                    parameters => #{}}]},
            RuntimeCfg))
    end.

fe_tool(Name) ->
    #{<<"name">> => Name,
      <<"description">> => <<"runs in the browser">>,
      <<"parameters">> => #{<<"type">> => <<"object">>,
                            <<"properties">> => #{}}}.

input(Thread, Messages, Tools) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => Thread,
                      <<"messages">> => Messages,
                      <<"tools">> => Tools}),
    Input.

user_msg(T) -> #{<<"role">> => <<"user">>, <<"content">> => T}.

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

run(Input, Factory) ->
    Self = self(),
    ok = beamai_agui_runner:run(Input, Factory, fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

types(Events) -> [maps:get(<<"type">>, E) || E <- Events].

recv_tools() ->
    receive {tools_sent, Tools} -> Tools after 500 -> timeout end.

flush_tools() ->
    receive {tools_sent, _} -> flush_tools() after 0 -> ok end.

spec_names(Specs) ->
    [maps:get(name, F) || #{function := F} <- Specs].

%%====================================================================
%% 用例
%%====================================================================

%% 前端工具必须出现在下发给模型的 tools 里——看不见就永远不会被调
advertised_to_model() ->
    ok = mock_llm(<<"pick_file">>),
    try
        _ = run(input(<<"t-fe-ad">>, [user_msg(<<"go">>)], [fe_tool(<<"pick_file">>)]),
                plain_factory()),
        ?assertEqual([<<"pick_file">>], spec_names(recv_tools()))
    after
        beamai_agui_thread:clear(<<"t-fe-ad">>),
        meck:unload(beamai_chat_model)
    end.

%% 完整往返：模型调前端工具 → 暂停（无结果的 TOOL_CALL_*）→ 前端回传结果 → 续跑
full_round_trip() ->
    Thread = <<"t-fe">>,
    Tools = [fe_tool(<<"pick_file">>)],
    ok = mock_llm(<<"pick_file">>),
    try
        Ev1 = run(input(Thread, [user_msg(<<"go">>)], Tools), plain_factory()),
        ?assertEqual([<<"RUN_STARTED">>, <<"STATE_SNAPSHOT">>,
                      <<"STEP_STARTED">>,
                      <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                      <<"STEP_FINISHED">>, <<"RUN_FINISHED">>], types(Ev1)),
        %% 服务端没有实现，当然也就没有结果——等前端给
        ?assertEqual([], [E || E <- Ev1,
                              maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>]),
        %% 收口那条 `RUN_FINISHED` 说清了在等哪次调用；**前端工具那一支要的是
        %% 「结果」而不是「决定」** —— 拿审批那张 schema 给它，客户端照着填只能
        %% 填出个 {decision}，填不出结果来
        [Finished] = [E || E <- Ev1, maps:get(<<"type">>, E) =:= <<"RUN_FINISHED">>],
        [Interrupt] = maps:get(<<"interrupts">>, maps:get(<<"outcome">>, Finished)),
        Meta = maps:get(<<"metadata">>, Interrupt),
        ?assertEqual(<<"frontend-tool">>, maps:get(<<"kind">>, Meta)),
        ?assertEqual(<<"pick_file">>,
                     maps:get(<<"name">>, maps:get(<<"pendingTool">>, Meta))),
        ?assertEqual([<<"result">>],
                     maps:get(<<"required">>, maps:get(<<"responseSchema">>, Interrupt))),

        %% 前端执行完，把结果当 tool 消息送回来
        Reply = #{<<"role">> => <<"tool">>, <<"toolCallId">> => <<"fc1">>,
                  <<"content">> => <<"/tmp/a.txt">>},
        Ev2 = run(input(Thread, [user_msg(<<"go">>), Reply], Tools), plain_factory()),
        [Result] = [E || E <- Ev2,
                         maps:get(<<"type">>, E) =:= <<"TOOL_CALL_RESULT">>],
        ?assertEqual(<<"fc1">>, maps:get(<<"toolCallId">>, Result)),
        ?assertEqual(<<"/tmp/a.txt">>, maps:get(<<"content">>, Result)),
        ?assert(lists:member(<<"TEXT_MESSAGE_CONTENT">>, types(Ev2))),
        ?assertEqual(none, beamai_agui_thread:pending(Thread))
    after
        beamai_agui_thread:clear(Thread),
        meck:unload(beamai_chat_model)
    end.

%% 重名时服务端那个赢：它有真实现，前端那个只是个声明
server_side_tool_wins_on_name_clash() ->
    ok = mock_llm(<<"confirm">>),
    try
        _ = run(input(<<"t-fe-clash">>, [user_msg(<<"go">>)], [fe_tool(<<"confirm">>)]),
                clashing_factory()),
        Specs = recv_tools(),
        ?assertEqual([<<"confirm">>], spec_names(Specs)),
        %% 留下的是工厂那份（description 为 server side）
        [#{function := F}] = Specs,
        ?assertEqual(<<"server side">>, maps:get(description, F))
    after
        beamai_agui_thread:clear(<<"t-fe-clash">>),
        meck:unload(beamai_chat_model)
    end.

%% 没名字的条目直接丢：它会变成一个 name 为空的 spec，把整个 LLM 请求带坏
malformed_entries_ignored() ->
    ok = mock_llm(<<"pick_file">>),
    Junk = [#{<<"description">> => <<"no name">>},
            #{<<"name">> => <<>>},
            <<"not even a map">>,
            fe_tool(<<"pick_file">>)],
    try
        _ = run(input(<<"t-fe-junk">>, [user_msg(<<"go">>)], Junk), plain_factory()),
        ?assertEqual([<<"pick_file">>], spec_names(recv_tools()))
    after
        beamai_agui_thread:clear(<<"t-fe-junk">>),
        meck:unload(beamai_chat_model)
    end.
