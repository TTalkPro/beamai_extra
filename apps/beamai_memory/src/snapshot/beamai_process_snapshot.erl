%%%-------------------------------------------------------------------
%%% @doc Process Snapshot 模块 - Process Framework 专用
%%%
%%% 为 Process Framework 提供状态快照功能：
%%% - 保存/恢复流程状态
%%% - 时间旅行（回退/前进）
%%% - 分支管理
%%% - 血统追踪
%%%
%%% == 实现 beamai_snapshot 行为 ==
%%%
%%% 本模块实现 beamai_snapshot 行为，提供 Process 特定的
%%% 条目访问器、修改器和工厂函数。
%%%
%%% == 时间旅行模型 ==
%%%
%%% Process Framework 的时间线由事件驱动：
%%% ```
%%% event_1 → event_2 → event_3 → event_4 → event_5
%%%    │         │         │         │         │
%%%   sn_1      sn_2      sn_3      sn_4      sn_5
%%%              ↑                    ↑
%%%              └── go_back(2) ──────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_snapshot).

-behaviour(beamai_snapshot).

-include_lib("beamai_memory/include/beamai_process_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 类型导出
-export_type([
    snapshot/0,
    manager/0,
    snapshot_type/0
]).

%% 构造函数
-export([
    new/1,
    new/2
]).

%% 核心操作（委托给 snapshot）
-export([
    save/3,
    load/2,
    delete/2,
    get_latest/2
]).

%% 时间旅行（委托给 snapshot）
-export([
    go_back/3,
    go_forward/3,
    goto/3,
    undo/2,
    redo/2,
    get_current_position/2
]).

%% 分支管理（委托给 snapshot）
-export([
    fork_from/4,
    fork_from/5,
    list_branches/1,
    switch_branch/2
]).

%% 历史查询（委托给 snapshot）
-export([
    get_history/2,
    get_history/3,
    get_lineage/2,
    compare/3
]).

%% Process 专用操作
-export([
    get_id/1,
    save_from_state/3,
    save_from_state/4,
    create_snapshot/2,
    create_snapshot/3,
    get_step_state/2,
    get_steps_state/1,
    get_event_queue/1,
    is_paused/1,
    get_pause_info/1
]).

%% Snapshot 行为回调
-export([
    entry_id/1,
    entry_owner_id/1,
    entry_parent_id/1,
    entry_version/1,
    entry_branch_id/1,
    entry_created_at/1,
    entry_state/1,
    set_entry_id/2,
    set_entry_parent_id/2,
    set_entry_version/2,
    set_entry_branch_id/2,
    new_entry/3,
    entry_to_state_entry/1,
    state_entry_to_entry/1,
    namespace/0,
    id_prefix/0,
    entry_type/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type snapshot() :: #process_snapshot{}.
-type manager() :: beamai_snapshot:manager().

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Snapshot 管理器
-spec new(beamai_state_store:store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

%% @doc 创建 Snapshot 管理器（带选项）
-spec new(beamai_state_store:store(), map()) -> manager().
new(StateStore, Opts) ->
    beamai_snapshot:new(?MODULE, StateStore, Opts).

%%====================================================================
%% 核心操作
%%====================================================================

%% @doc 保存快照
-spec save(manager(), binary(), snapshot()) ->
    {ok, snapshot(), manager()} | {error, term()}.
save(Mgr, ThreadId, Snapshot) ->
    beamai_snapshot:save(Mgr, ThreadId, Snapshot).

%% @doc 加载快照
-spec load(manager(), binary()) -> {ok, snapshot()} | {error, term()}.
load(Mgr, SnapshotId) ->
    beamai_snapshot:load(Mgr, SnapshotId).

%% @doc 删除快照
-spec delete(manager(), binary()) -> {ok, manager()} | {error, term()}.
delete(Mgr, SnapshotId) ->
    beamai_snapshot:delete(Mgr, SnapshotId).

%% @doc 获取最新快照
-spec get_latest(manager(), binary()) -> {ok, snapshot()} | {error, term()}.
get_latest(Mgr, ThreadId) ->
    beamai_snapshot:get_latest(Mgr, ThreadId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个版本
-spec go_back(manager(), binary(), pos_integer()) ->
    {ok, snapshot(), manager()} | {error, term()}.
go_back(Mgr, ThreadId, Steps) ->
    beamai_snapshot:go_back(Mgr, ThreadId, Steps).

%% @doc 前进 N 个版本
-spec go_forward(manager(), binary(), pos_integer()) ->
    {ok, snapshot(), manager()} | {error, term()}.
go_forward(Mgr, ThreadId, Steps) ->
    beamai_snapshot:go_forward(Mgr, ThreadId, Steps).

%% @doc 跳转到指定快照
-spec goto(manager(), binary(), binary()) ->
    {ok, snapshot(), manager()} | {error, term()}.
goto(Mgr, ThreadId, SnapshotId) ->
    beamai_snapshot:goto(Mgr, ThreadId, SnapshotId).

%% @doc 撤销
-spec undo(manager(), binary()) -> {ok, snapshot(), manager()} | {error, term()}.
undo(Mgr, ThreadId) ->
    beamai_snapshot:undo(Mgr, ThreadId).

%% @doc 重做
-spec redo(manager(), binary()) -> {ok, snapshot(), manager()} | {error, term()}.
redo(Mgr, ThreadId) ->
    beamai_snapshot:redo(Mgr, ThreadId).

%% @doc 获取当前位置
-spec get_current_position(manager(), binary()) ->
    {ok, beamai_snapshot:position()} | {error, term()}.
get_current_position(Mgr, ThreadId) ->
    beamai_snapshot:get_current_position(Mgr, ThreadId).

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 从指定快照创建分支
-spec fork_from(manager(), binary(), binary(), binary()) ->
    {ok, snapshot(), manager()} | {error, term()}.
fork_from(Mgr, SnapshotId, NewBranchName, ThreadId) ->
    beamai_snapshot:fork_from(Mgr, SnapshotId, NewBranchName, ThreadId).

-spec fork_from(manager(), binary(), binary(), binary(), map()) ->
    {ok, snapshot(), manager()} | {error, term()}.
fork_from(Mgr, SnapshotId, NewBranchName, ThreadId, Opts) ->
    beamai_snapshot:fork_from(Mgr, SnapshotId, NewBranchName, ThreadId, Opts).

%% @doc 列出所有分支
-spec list_branches(manager()) -> [beamai_snapshot:branch()].
list_branches(Mgr) ->
    beamai_snapshot:list_branches(Mgr).

%% @doc 切换分支
-spec switch_branch(manager(), binary()) -> {ok, manager()} | {error, term()}.
switch_branch(Mgr, BranchId) ->
    beamai_snapshot:switch_branch(Mgr, BranchId).

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 获取历史记录
-spec get_history(manager(), binary()) -> {ok, [snapshot()]} | {error, term()}.
get_history(Mgr, ThreadId) ->
    beamai_snapshot:get_history(Mgr, ThreadId).

-spec get_history(manager(), binary(), map()) -> {ok, [snapshot()]} | {error, term()}.
get_history(Mgr, ThreadId, Opts) ->
    beamai_snapshot:get_history(Mgr, ThreadId, Opts).

%% @doc 获取血统
-spec get_lineage(manager(), binary()) -> {ok, [snapshot()]} | {error, term()}.
get_lineage(Mgr, SnapshotId) ->
    beamai_snapshot:get_lineage(Mgr, SnapshotId).

%% @doc 比较两个快照
-spec compare(manager(), binary(), binary()) ->
    {ok, #{from := snapshot(), to := snapshot(), version_diff := integer()}} |
    {error, term()}.
compare(Mgr, SnapshotId1, SnapshotId2) ->
    beamai_snapshot:compare(Mgr, SnapshotId1, SnapshotId2).

%%====================================================================
%% Process 专用操作
%%====================================================================

%% @doc 获取快照 ID
-spec get_id(snapshot()) -> binary().
get_id(#process_snapshot{id = Id}) -> Id.

%% @doc 从 Process 状态创建并保存快照
-spec save_from_state(manager(), binary(), map()) ->
    {ok, snapshot(), manager()} | {error, term()}.
save_from_state(Mgr, ThreadId, ProcessState) ->
    save_from_state(Mgr, ThreadId, ProcessState, #{}).

-spec save_from_state(manager(), binary(), map(), map()) ->
    {ok, snapshot(), manager()} | {error, term()}.
save_from_state(Mgr, ThreadId, ProcessState, Opts) ->
    Snapshot = create_snapshot(ThreadId, ProcessState, Opts),
    save(Mgr, ThreadId, Snapshot).

%% @doc 创建快照（不保存）
-spec create_snapshot(binary(), map()) -> snapshot().
create_snapshot(ThreadId, ProcessState) ->
    create_snapshot(ThreadId, ProcessState, #{}).

-spec create_snapshot(binary(), map(), map()) -> snapshot().
create_snapshot(ThreadId, ProcessState, Opts) ->
    Now = erlang:system_time(millisecond),

    #process_snapshot{
        id = undefined,  %% 由 snapshot 分配
        thread_id = ThreadId,
        parent_id = undefined,  %% 由 snapshot 设置
        branch_id = maps:get(branch_id, Opts, ?DEFAULT_BRANCH),
        version = 0,  %% 由 snapshot 设置
        created_at = Now,

        %% Process 状态
        process_spec = maps:get(process_spec, ProcessState, #{}),
        fsm_state = maps:get(fsm_state, ProcessState, idle),
        steps_state = maps:get(steps_state, ProcessState, #{}),
        event_queue = maps:get(event_queue, ProcessState, []),
        paused_step = maps:get(paused_step, ProcessState, undefined),
        pause_reason = maps:get(pause_reason, ProcessState, undefined),

        %% 快照类型和上下文
        snapshot_type = maps:get(snapshot_type, Opts, manual),
        step_id = maps:get(step_id, Opts, undefined),
        run_id = maps:get(run_id, Opts, undefined),
        agent_id = maps:get(agent_id, Opts, undefined),
        agent_name = maps:get(agent_name, Opts, undefined),

        %% 元数据
        metadata = maps:get(metadata, Opts, #{})
    }.

%% @doc 获取指定步骤的状态
-spec get_step_state(snapshot(), atom()) -> {ok, step_snapshot()} | {error, not_found}.
get_step_state(#process_snapshot{steps_state = StepsState}, StepId) ->
    case maps:find(StepId, StepsState) of
        {ok, State} -> {ok, State};
        error -> {error, not_found}
    end.

%% @doc 获取所有步骤状态
-spec get_steps_state(snapshot()) -> #{atom() => step_snapshot()}.
get_steps_state(#process_snapshot{steps_state = StepsState}) ->
    StepsState.

%% @doc 获取事件队列
-spec get_event_queue(snapshot()) -> [map()].
get_event_queue(#process_snapshot{event_queue = Queue}) ->
    Queue.

%% @doc 检查是否暂停
-spec is_paused(snapshot()) -> boolean().
is_paused(#process_snapshot{fsm_state = paused}) -> true;
is_paused(_) -> false.

%% @doc 获取暂停信息
-spec get_pause_info(snapshot()) ->
    {ok, #{step := atom(), reason := term()}} | {error, not_paused}.
get_pause_info(#process_snapshot{fsm_state = paused, paused_step = Step, pause_reason = Reason}) ->
    {ok, #{step => Step, reason => Reason}};
get_pause_info(_) ->
    {error, not_paused}.

%%====================================================================
%% Snapshot 行为回调 - 条目访问器
%%====================================================================

%% @private
entry_id(#process_snapshot{id = Id}) -> Id.

%% @private
entry_owner_id(#process_snapshot{thread_id = ThreadId}) -> ThreadId.

%% @private
entry_parent_id(#process_snapshot{parent_id = ParentId}) -> ParentId.

%% @private
entry_version(#process_snapshot{version = Version}) -> Version.

%% @private
entry_branch_id(#process_snapshot{branch_id = BranchId}) -> BranchId.

%% @private
entry_created_at(#process_snapshot{created_at = CreatedAt}) -> CreatedAt.

%% @private
entry_state(#process_snapshot{} = Snapshot) ->
    #{
        process_spec => Snapshot#process_snapshot.process_spec,
        fsm_state => Snapshot#process_snapshot.fsm_state,
        steps_state => Snapshot#process_snapshot.steps_state,
        event_queue => Snapshot#process_snapshot.event_queue,
        paused_step => Snapshot#process_snapshot.paused_step,
        pause_reason => Snapshot#process_snapshot.pause_reason,
        snapshot_type => Snapshot#process_snapshot.snapshot_type,
        step_id => Snapshot#process_snapshot.step_id,
        run_id => Snapshot#process_snapshot.run_id,
        agent_id => Snapshot#process_snapshot.agent_id,
        agent_name => Snapshot#process_snapshot.agent_name,
        metadata => Snapshot#process_snapshot.metadata
    }.

%%====================================================================
%% Snapshot 行为回调 - 条目修改器
%%====================================================================

%% @private
set_entry_id(Snapshot, Id) ->
    Snapshot#process_snapshot{id = Id}.

%% @private
set_entry_parent_id(Snapshot, ParentId) ->
    Snapshot#process_snapshot{parent_id = ParentId}.

%% @private
set_entry_version(Snapshot, Version) ->
    Snapshot#process_snapshot{version = Version}.

%% @private
set_entry_branch_id(Snapshot, BranchId) ->
    Snapshot#process_snapshot{branch_id = BranchId}.

%%====================================================================
%% Snapshot 行为回调 - 工厂和转换
%%====================================================================

%% @private
new_entry(ThreadId, State, Opts) ->
    create_snapshot(ThreadId, State, Opts).

%% @private
entry_to_state_entry(#process_snapshot{} = Snapshot) ->
    #state_entry{
        id = Snapshot#process_snapshot.id,
        owner_id = Snapshot#process_snapshot.thread_id,
        parent_id = Snapshot#process_snapshot.parent_id,
        branch_id = Snapshot#process_snapshot.branch_id,
        version = Snapshot#process_snapshot.version,
        state = snapshot_to_map(Snapshot),
        entry_type = process_snapshot,
        created_at = Snapshot#process_snapshot.created_at,
        metadata = Snapshot#process_snapshot.metadata
    }.

%% @private
state_entry_to_entry(#state_entry{} = Entry) ->
    map_to_snapshot(Entry#state_entry.state, Entry).

%% @private
namespace() -> ?NS_PROCESS_SNAPSHOTS.

%% @private
id_prefix() -> ?SNAPSHOT_ID_PREFIX.

%% @private
entry_type() -> process_snapshot.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 快照转 Map（用于存储）
-spec snapshot_to_map(snapshot()) -> map().
snapshot_to_map(#process_snapshot{} = S) ->
    #{
        <<"process_spec">> => S#process_snapshot.process_spec,
        <<"fsm_state">> => atom_to_binary(S#process_snapshot.fsm_state, utf8),
        <<"steps_state">> => encode_steps_state(S#process_snapshot.steps_state),
        <<"event_queue">> => S#process_snapshot.event_queue,
        <<"paused_step">> => maybe_atom_to_binary(S#process_snapshot.paused_step),
        <<"pause_reason">> => S#process_snapshot.pause_reason,
        <<"snapshot_type">> => atom_to_binary(S#process_snapshot.snapshot_type, utf8),
        <<"step_id">> => maybe_atom_to_binary(S#process_snapshot.step_id),
        <<"run_id">> => S#process_snapshot.run_id,
        <<"agent_id">> => S#process_snapshot.agent_id,
        <<"agent_name">> => S#process_snapshot.agent_name
    }.

%% @private Map 转快照（从存储恢复）
-spec map_to_snapshot(map(), #state_entry{}) -> snapshot().
map_to_snapshot(State, Entry) ->
    #process_snapshot{
        id = Entry#state_entry.id,
        thread_id = Entry#state_entry.owner_id,
        parent_id = Entry#state_entry.parent_id,
        branch_id = Entry#state_entry.branch_id,
        version = Entry#state_entry.version,
        created_at = Entry#state_entry.created_at,

        process_spec = maps:get(<<"process_spec">>, State, #{}),
        fsm_state = binary_to_existing_atom(maps:get(<<"fsm_state">>, State, <<"idle">>), utf8),
        steps_state = decode_steps_state(maps:get(<<"steps_state">>, State, #{})),
        event_queue = maps:get(<<"event_queue">>, State, []),
        paused_step = maybe_binary_to_atom(maps:get(<<"paused_step">>, State, undefined)),
        pause_reason = maps:get(<<"pause_reason">>, State, undefined),

        snapshot_type = binary_to_existing_atom(
            maps:get(<<"snapshot_type">>, State, <<"manual">>), utf8),
        step_id = maybe_binary_to_atom(maps:get(<<"step_id">>, State, undefined)),
        run_id = maps:get(<<"run_id">>, State, undefined),
        agent_id = maps:get(<<"agent_id">>, State, undefined),
        agent_name = maps:get(<<"agent_name">>, State, undefined),

        metadata = Entry#state_entry.metadata
    }.

%% @private 编码步骤状态
-spec encode_steps_state(#{atom() => step_snapshot()}) -> map().
encode_steps_state(StepsState) ->
    maps:fold(
        fun(StepId, StepState, Acc) ->
            Key = atom_to_binary(StepId, utf8),
            maps:put(Key, StepState, Acc)
        end,
        #{},
        StepsState
    ).

%% @private 解码步骤状态
-spec decode_steps_state(map()) -> #{atom() => step_snapshot()}.
decode_steps_state(EncodedState) ->
    maps:fold(
        fun(Key, StepState, Acc) ->
            StepId = binary_to_existing_atom(Key, utf8),
            maps:put(StepId, StepState, Acc)
        end,
        #{},
        EncodedState
    ).

%% @private 安全的原子转二进制
-spec maybe_atom_to_binary(atom() | undefined) -> binary() | undefined.
maybe_atom_to_binary(undefined) -> undefined;
maybe_atom_to_binary(Atom) -> atom_to_binary(Atom, utf8).

%% @private 安全的二进制转原子
-spec maybe_binary_to_atom(binary() | undefined) -> atom() | undefined.
maybe_binary_to_atom(undefined) -> undefined;
maybe_binary_to_atom(<<>>) -> undefined;
maybe_binary_to_atom(Bin) -> binary_to_existing_atom(Bin, utf8).
