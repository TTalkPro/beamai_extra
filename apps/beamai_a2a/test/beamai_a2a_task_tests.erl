%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_task 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_task_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Task 创建测试
%%====================================================================

create_task_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),
    ?assert(is_pid(Pid)),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assert(is_map(Task)),
    ?assertEqual(submitted, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

create_task_with_message_test() ->
    Message = #{
        role => user,
        parts => [#{kind => text, text => <<"Hello">>}]
    },
    {ok, Pid} = beamai_a2a_task:start(#{message => Message}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Messages = maps:get(messages, Task),
    ?assertEqual(1, length(Messages)),

    beamai_a2a_task:stop(Pid).

create_task_with_context_id_test() ->
    ContextId = <<"ctx-123">>,
    {ok, Pid} = beamai_a2a_task:start(#{context_id => ContextId}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(ContextId, maps:get(context_id, Task)),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% Task ID 测试
%%====================================================================

get_task_id_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    {ok, TaskId} = beamai_a2a_task:get_id(Pid),
    ?assert(is_binary(TaskId)),
    ?assertMatch(<<"task_", _/binary>>, TaskId),

    beamai_a2a_task:stop(Pid).

custom_task_id_test() ->
    CustomId = <<"my-custom-task-id">>,
    {ok, Pid} = beamai_a2a_task:start(#{task_id => CustomId}),

    {ok, TaskId} = beamai_a2a_task:get_id(Pid),
    ?assertEqual(CustomId, TaskId),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 状态转换测试
%%====================================================================

update_status_to_working_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(working, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

update_status_to_completed_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    %% 先转到 working
    ok = beamai_a2a_task:update_status(Pid, working),
    %% 再转到 completed
    ok = beamai_a2a_task:update_status(Pid, completed),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(completed, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

update_status_to_input_required_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, input_required),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(input_required, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

invalid_transition_from_submitted_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    %% submitted 不能直接转到 completed
    Result = beamai_a2a_task:update_status(Pid, completed),
    ?assertMatch({error, {invalid_transition, submitted, completed}}, Result),

    beamai_a2a_task:stop(Pid).

invalid_transition_from_terminal_state_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, completed),

    %% 终态不能再转换
    Result = beamai_a2a_task:update_status(Pid, working),
    ?assertMatch({error, {invalid_transition, completed, working}}, Result),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 消息和产出物测试
%%====================================================================

add_message_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    Message = #{
        role => agent,
        parts => [#{kind => text, text => <<"Response">>}]
    },
    ok = beamai_a2a_task:add_message(Pid, Message),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Messages = maps:get(messages, Task),
    ?assertEqual(1, length(Messages)),

    beamai_a2a_task:stop(Pid).

add_artifact_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    Artifact = #{
        name => <<"result">>,
        parts => [#{kind => data, data => #{<<"key">> => <<"value">>}}]
    },
    ok = beamai_a2a_task:add_artifact(Pid, Artifact),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Artifacts = maps:get(artifacts, Task),
    ?assertEqual(1, length(Artifacts)),

    %% 应该自动生成 artifact_id
    [A] = Artifacts,
    ?assert(maps:is_key(artifact_id, A)),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 顺序不变量（守 O(n²)→O(1) 重构：内部 newest-first 存储 + 读时 reverse）
%%
%% 上面 add_message/add_artifact 只加**一条**，翻转 bug 检不出来；这里多次
%% 追加并断言读出的是**时间序**（oldest-first）。
%%====================================================================

add_messages_preserve_order_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),
    Msg = fun(N) -> #{role => agent, parts => [#{kind => text, text => N}]} end,
    ok = beamai_a2a_task:add_message(Pid, Msg(<<"m1">>)),
    ok = beamai_a2a_task:add_message(Pid, Msg(<<"m2">>)),
    ok = beamai_a2a_task:add_message(Pid, Msg(<<"m3">>)),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Texts = [hd(maps:get(parts, M)) || M <- maps:get(messages, Task)],
    ?assertEqual([<<"m1">>, <<"m2">>, <<"m3">>],
                 [maps:get(text, P) || P <- Texts]),

    beamai_a2a_task:stop(Pid).

%% 初始消息（最旧）在多次追加后仍排最前
init_message_stays_first_test() ->
    Init = #{role => user, parts => [#{kind => text, text => <<"init">>}]},
    {ok, Pid} = beamai_a2a_task:start(#{message => Init}),
    ok = beamai_a2a_task:add_message(Pid,
            #{role => agent, parts => [#{kind => text, text => <<"reply">>}]}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    [First, Second] = maps:get(messages, Task),
    ?assertEqual(<<"init">>, maps:get(text, hd(maps:get(parts, First)))),
    ?assertEqual(<<"reply">>, maps:get(text, hd(maps:get(parts, Second)))),

    beamai_a2a_task:stop(Pid).

add_artifacts_preserve_order_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),
    Art = fun(N) -> #{name => N, parts => [#{kind => text, text => N}]} end,
    ok = beamai_a2a_task:add_artifact(Pid, Art(<<"a1">>)),
    ok = beamai_a2a_task:add_artifact(Pid, Art(<<"a2">>)),
    ok = beamai_a2a_task:add_artifact(Pid, Art(<<"a3">>)),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual([<<"a1">>, <<"a2">>, <<"a3">>],
                 [maps:get(name, A) || A <- maps:get(artifacts, Task)]),

    beamai_a2a_task:stop(Pid).

%% update_status 的 {State, Msg, [Artifacts]} 批量产出物路径：批内保序，
%% 且接在先前单条 add_artifact 之后（守 `lists:reverse(Batch) ++ Existing`）
add_artifacts_batch_via_status_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),
    ok = beamai_a2a_task:add_artifact(Pid,
            #{name => <<"a0">>, parts => [#{kind => text, text => <<"a0">>}]}),
    Batch = [#{name => <<"b1">>, parts => []},
             #{name => <<"b2">>, parts => []}],
    ok = beamai_a2a_task:update_status(Pid, {working, undefined, Batch}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual([<<"a0">>, <<"b1">>, <<"b2">>],
                 [maps:get(name, A) || A <- maps:get(artifacts, Task)]),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 取消测试
%%====================================================================

cancel_task_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:cancel(Pid),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(canceled, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 历史记录测试
%%====================================================================

status_history_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, input_required),
    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, completed),

    {ok, Task} = beamai_a2a_task:get(Pid),
    History = maps:get(history, Task),

    %% 历史应该有 4 条记录（每次状态变化前的状态）
    ?assertEqual(4, length(History)),

    %% 验证历史顺序
    [H1, H2, H3, H4] = History,
    ?assertEqual(submitted, maps:get(state, H1)),
    ?assertEqual(working, maps:get(state, H2)),
    ?assertEqual(input_required, maps:get(state, H3)),
    ?assertEqual(working, maps:get(state, H4)),

    beamai_a2a_task:stop(Pid).
