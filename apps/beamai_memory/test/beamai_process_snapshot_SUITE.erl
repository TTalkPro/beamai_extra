%%%-------------------------------------------------------------------
%%% @doc Process Snapshot 测试套件
%%%
%%% 测试 Layer 2 Process Snapshot 功能。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_snapshot_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_process_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% CT 回调
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% 测试用例
-export([
    test_new/1,
    test_save_and_load/1,
    test_save_from_state/1,
    test_get_latest/1,
    test_time_travel_go_back/1,
    test_time_travel_go_forward/1,
    test_time_travel_goto/1,
    test_undo_redo/1,
    test_get_current_position/1,
    test_fork_from/1,
    test_branch_management/1,
    test_get_history/1,
    test_get_lineage/1,
    test_snapshot_accessors/1
]).

%%====================================================================
%% CT 回调
%%====================================================================

all() ->
    [
        test_new,
        test_save_and_load,
        test_save_from_state,
        test_get_latest,
        test_time_travel_go_back,
        test_time_travel_go_forward,
        test_time_travel_goto,
        test_undo_redo,
        test_get_current_position,
        test_fork_from,
        test_branch_management,
        test_get_history,
        test_get_lineage,
        test_snapshot_accessors
    ].

init_per_suite(Config) ->
    application:ensure_all_started(beamai_memory),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% 为每个测试创建新的环境
    StoreName = list_to_atom("snapshot_store_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, _} = beamai_store_ets:start_link(StoreName, #{}),
    Backend = {beamai_store_ets, StoreName},
    StateStore = beamai_state_store:new(Backend, #{namespace => <<"snapshot_test">>}),
    Mgr = beamai_process_snapshot:new(StateStore, #{max_entries => 50, auto_prune => false}),
    [{mgr, Mgr}, {store_name, StoreName} | Config].

end_per_testcase(_TestCase, Config) ->
    StoreName = ?config(store_name, Config),
    catch gen_server:stop(StoreName),
    ok.

%%====================================================================
%% 测试用例
%%====================================================================

%% @doc 测试创建 Snapshot Manager
test_new(Config) ->
    Mgr = ?config(mgr, Config),
    ?assertMatch(#{module := beamai_process_snapshot, state_store := _}, Mgr).

%% @doc 测试保存和加载快照
test_save_and_load(Config) ->
    Mgr = ?config(mgr, Config),
    ThreadId = <<"thread_1">>,

    %% 创建快照
    Snapshot = beamai_process_snapshot:create_snapshot(ThreadId, #{
        process_spec => #{name => test_process},
        fsm_state => running,
        steps_state => #{},
        event_queue => []
    }, #{snapshot_type => initial}),

    %% 保存
    {ok, SavedSnapshot, Mgr2} = beamai_process_snapshot:save(Mgr, ThreadId, Snapshot),
    ?assertNotEqual(undefined, SavedSnapshot#process_snapshot.id),
    ?assertEqual(ThreadId, SavedSnapshot#process_snapshot.thread_id),

    %% 加载
    SnapshotId = SavedSnapshot#process_snapshot.id,
    {ok, LoadedSnapshot} = beamai_process_snapshot:load(Mgr2, SnapshotId),
    ?assertEqual(SnapshotId, LoadedSnapshot#process_snapshot.id),
    ?assertEqual(running, LoadedSnapshot#process_snapshot.fsm_state).

%% @doc 测试从状态保存快照
test_save_from_state(Config) ->
    Mgr = ?config(mgr, Config),
    ThreadId = <<"thread_2">>,

    ProcessState = #{
        process_spec => #{name => my_process},
        fsm_state => paused,
        steps_state => #{step1 => #{state => done, collected_inputs => #{}, activation_count => 1}},
        event_queue => [#{type => continue}],
        paused_step => step1,
        pause_reason => user_interrupt
    },

    {ok, Snapshot, _Mgr2} = beamai_process_snapshot:save_from_state(Mgr, ThreadId, ProcessState, #{
        snapshot_type => paused,
        step_id => step1
    }),

    ?assertEqual(paused, Snapshot#process_snapshot.fsm_state),
    ?assertEqual(step1, Snapshot#process_snapshot.paused_step),
    ?assertEqual([#{type => continue}], Snapshot#process_snapshot.event_queue).

%% @doc 测试获取最新快照
test_get_latest(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_3">>,

    %% 保存多个快照
    {ok, _S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{
        fsm_state => idle
    }, #{snapshot_type => initial}),

    {ok, _S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{
        fsm_state => running
    }, #{snapshot_type => step_completed}),

    {ok, S3, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{
        fsm_state => completed
    }, #{snapshot_type => completed}),

    %% 获取最新
    {ok, Latest} = beamai_process_snapshot:get_latest(Mgr3, ThreadId),
    ?assertEqual(S3#process_snapshot.id, Latest#process_snapshot.id),
    ?assertEqual(completed, Latest#process_snapshot.fsm_state).

%% @doc 测试时间旅行 - 回退
test_time_travel_go_back(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_4">>,

    %% 创建 3 个快照
    {ok, S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, _S3, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => completed}, #{}),

    %% 回退 2 步
    {ok, BackSnapshot, _Mgr4} = beamai_process_snapshot:go_back(Mgr3, ThreadId, 2),
    ?assertEqual(S1#process_snapshot.id, BackSnapshot#process_snapshot.id),
    ?assertEqual(idle, BackSnapshot#process_snapshot.fsm_state).

%% @doc 测试时间旅行 - 前进
test_time_travel_go_forward(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_5">>,

    %% 创建 3 个快照
    {ok, _S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, S3, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => completed}, #{}),

    %% 先回退
    {ok, _, Mgr4} = beamai_process_snapshot:go_back(Mgr3, ThreadId, 2),

    %% 再前进
    {ok, ForwardSnapshot, _Mgr5} = beamai_process_snapshot:go_forward(Mgr4, ThreadId, 2),
    ?assertEqual(S3#process_snapshot.id, ForwardSnapshot#process_snapshot.id),
    ?assertEqual(completed, ForwardSnapshot#process_snapshot.fsm_state).

%% @doc 测试时间旅行 - 跳转
test_time_travel_goto(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_6">>,

    %% 创建 3 个快照
    {ok, _S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, _S3, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => completed}, #{}),

    %% 跳转到第 2 个快照
    {ok, GotoSnapshot, _Mgr4} = beamai_process_snapshot:goto(Mgr3, ThreadId, S2#process_snapshot.id),
    ?assertEqual(S2#process_snapshot.id, GotoSnapshot#process_snapshot.id),
    ?assertEqual(running, GotoSnapshot#process_snapshot.fsm_state).

%% @doc 测试撤销/重做
test_undo_redo(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_7">>,

    %% 创建 2 个快照
    {ok, S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),

    %% 撤销
    {ok, UndoSnapshot, Mgr3} = beamai_process_snapshot:undo(Mgr2, ThreadId),
    ?assertEqual(S1#process_snapshot.id, UndoSnapshot#process_snapshot.id),

    %% 重做
    {ok, RedoSnapshot, _Mgr4} = beamai_process_snapshot:redo(Mgr3, ThreadId),
    ?assertEqual(S2#process_snapshot.id, RedoSnapshot#process_snapshot.id).

%% @doc 测试获取当前位置
test_get_current_position(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_8">>,

    %% 创建 3 个快照
    {ok, _, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, _, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => completed}, #{}),

    %% 获取位置（应该在最后）
    {ok, Pos1} = beamai_process_snapshot:get_current_position(Mgr3, ThreadId),
    ?assertEqual(2, maps:get(current, Pos1)),  %% 0-indexed
    ?assertEqual(3, maps:get(total, Pos1)),

    %% 回退后获取位置
    {ok, _, Mgr4} = beamai_process_snapshot:go_back(Mgr3, ThreadId, 1),
    {ok, Pos2} = beamai_process_snapshot:get_current_position(Mgr4, ThreadId),
    ?assertEqual(1, maps:get(current, Pos2)).

%% @doc 测试分支创建
test_fork_from(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_9">>,

    %% 创建快照
    {ok, S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),

    %% 从 S1 创建分支
    {ok, ForkedSnapshot, Mgr3} = beamai_process_snapshot:fork_from(Mgr2, S1#process_snapshot.id, <<"experiment">>, ThreadId),

    %% 验证分支
    ?assertNotEqual(<<"main">>, ForkedSnapshot#process_snapshot.branch_id),
    ?assertEqual(S1#process_snapshot.id, ForkedSnapshot#process_snapshot.parent_id),

    %% 验证分支列表
    Branches = beamai_process_snapshot:list_branches(Mgr3),
    ?assertEqual(2, length(Branches)).

%% @doc 测试分支管理
test_branch_management(Config) ->
    Mgr0 = ?config(mgr, Config),

    %% 初始只有 main 分支
    Branches0 = beamai_process_snapshot:list_branches(Mgr0),
    ?assertEqual(1, length(Branches0)),

    %% 创建分支（通过 fork）
    ThreadId = <<"thread_10">>,
    {ok, S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _, Mgr2} = beamai_process_snapshot:fork_from(Mgr1, S1#process_snapshot.id, <<"feature">>, ThreadId),

    Branches1 = beamai_process_snapshot:list_branches(Mgr2),
    ?assertEqual(2, length(Branches1)),

    %% 切换回 main 分支
    {ok, Mgr3} = beamai_process_snapshot:switch_branch(Mgr2, <<"main">>),
    ?assertMatch(#{current_branch := <<"main">>}, Mgr3).

%% @doc 测试获取历史
test_get_history(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_11">>,

    %% 创建多个快照
    {ok, _, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, _, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, _, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => paused}, #{}),
    {ok, _, Mgr4} = beamai_process_snapshot:save_from_state(Mgr3, ThreadId, #{fsm_state => completed}, #{}),

    %% 获取历史
    {ok, History} = beamai_process_snapshot:get_history(Mgr4, ThreadId),
    ?assertEqual(4, length(History)),

    %% 验证顺序（版本号递增）
    Versions = [S#process_snapshot.version || S <- History],
    ?assertEqual([1, 2, 3, 4], Versions).

%% @doc 测试获取血统
test_get_lineage(Config) ->
    Mgr0 = ?config(mgr, Config),
    ThreadId = <<"thread_12">>,

    %% 创建快照链
    {ok, S1, Mgr1} = beamai_process_snapshot:save_from_state(Mgr0, ThreadId, #{fsm_state => idle}, #{}),
    {ok, S2, Mgr2} = beamai_process_snapshot:save_from_state(Mgr1, ThreadId, #{fsm_state => running}, #{}),
    {ok, S3, Mgr3} = beamai_process_snapshot:save_from_state(Mgr2, ThreadId, #{fsm_state => completed}, #{}),

    %% 获取 S3 的血统
    {ok, Lineage} = beamai_process_snapshot:get_lineage(Mgr3, S3#process_snapshot.id),

    %% 验证血统（从根到当前）
    ?assertEqual(3, length(Lineage)),
    [L1, L2, L3] = Lineage,
    ?assertEqual(S1#process_snapshot.id, L1#process_snapshot.id),
    ?assertEqual(S2#process_snapshot.id, L2#process_snapshot.id),
    ?assertEqual(S3#process_snapshot.id, L3#process_snapshot.id).

%% @doc 测试快照访问器函数
test_snapshot_accessors(Config) ->
    Mgr = ?config(mgr, Config),
    ThreadId = <<"thread_13">>,

    ProcessState = #{
        process_spec => #{name => test},
        fsm_state => paused,
        steps_state => #{
            step1 => #{state => done, collected_inputs => #{a => 1}, activation_count => 2}
        },
        event_queue => [#{type => resume}, #{type => input}],
        paused_step => step1,
        pause_reason => waiting_input
    },

    {ok, Snapshot, _} = beamai_process_snapshot:save_from_state(Mgr, ThreadId, ProcessState, #{
        snapshot_type => paused,
        step_id => step1
    }),

    %% 测试步骤状态访问
    {ok, StepState} = beamai_process_snapshot:get_step_state(Snapshot, step1),
    ?assertEqual(#{state => done, collected_inputs => #{a => 1}, activation_count => 2}, StepState),

    %% 测试所有步骤状态
    StepsState = beamai_process_snapshot:get_steps_state(Snapshot),
    ?assertEqual(1, maps:size(StepsState)),

    %% 测试事件队列
    EventQueue = beamai_process_snapshot:get_event_queue(Snapshot),
    ?assertEqual(2, length(EventQueue)),

    %% 测试暂停检查
    ?assertEqual(true, beamai_process_snapshot:is_paused(Snapshot)),

    %% 测试暂停信息
    {ok, PauseInfo} = beamai_process_snapshot:get_pause_info(Snapshot),
    ?assertEqual(step1, maps:get(step, PauseInfo)),
    ?assertEqual(waiting_input, maps:get(reason, PauseInfo)).
