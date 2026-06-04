%%%-------------------------------------------------------------------
%%% @doc Process Store 实现 - 基于 beamai_process_snapshot 后端
%%%
%%% 实现 beamai_process_store_behaviour，将流程快照存储到
%%% beamai_process_snapshot Manager 中。
%%%
%%% == Store Ref 格式 ==
%%%
%%% store_ref() = {beamai_process_snapshot:manager(), #{thread_id := binary()}}
%%%
%%% == 格式转换 ==
%%%
%%% 保存时：process snapshot map 直接作为 snapshot 的 values 存储
%%% 加载时：从 snapshot 的 values 提取 process snapshot map
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Process Snapshot Manager
%%% Mgr = beamai_process_snapshot:new(StateStore, #{}),
%%%
%%% %% 配置 store ref
%%% StoreRef = {Mgr, #{thread_id => <<"process-thread-1">>}},
%%%
%%% %% 在 runtime 启动时使用
%%% {ok, Pid} = beamai_process:start(ProcessSpec, #{
%%%     store => {beamai_process_memory_store, StoreRef}
%%% }).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_memory_store).

-behaviour(beamai_process_store_behaviour).

%% Behaviour 回调
-export([
    save_snapshot/3,
    load_snapshot/3,
    delete_snapshot/2,
    list_snapshots/2
]).

%% 分支 API（非 behaviour 回调）
-export([
    branch/3,
    load_branch/3,
    list_branches/2,
    get_lineage/2,
    diff/3
]).

%% 时间旅行 API（非 behaviour 回调）
-export([
    go_back/2,
    go_forward/2,
    goto/2,
    list_history/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type store_ref() :: {beamai_process_snapshot:manager(), thread_config()}.
-type thread_config() :: #{thread_id := binary(), _ => _}.

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc 保存流程快照到 beamai_process_snapshot
%%
%% Process snapshot 直接作为 snapshot 的 values 存储。
%% 从 SaveOpts 中提取元数据用于 config。
-spec save_snapshot(store_ref(), beamai_process_state:snapshot(),
                    beamai_process_store_behaviour:save_opts()) ->
    {ok, binary()} | {error, term()}.
save_snapshot({Mgr, ThreadConfig}, ProcessSnapshot, _SaveOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),

    %% Process snapshot 直接作为 values 存储
    State = ProcessSnapshot,

    case beamai_process_snapshot:save_from_state(Mgr, ThreadId, State, #{}) of
        {ok, Snapshot, _NewMgr} ->
            {ok, Snapshot};
        {error, _} = Error ->
            Error
    end.

%% @doc 从 beamai_process_snapshot 加载流程快照
%%
%% 支持指定 SnapshotId 或 latest 加载最新快照。
%% 返回的 values 即为 process snapshot map。
-spec load_snapshot(store_ref(), binary() | latest,
                    beamai_process_store_behaviour:load_opts()) ->
    {ok, beamai_process_state:snapshot()} | {error, not_found | term()}.
load_snapshot({Mgr, ThreadConfig}, latest, _LoadOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    case beamai_process_snapshot:get_latest(Mgr, ThreadId) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, _} = Error ->
            Error
    end;
load_snapshot({Mgr, _ThreadConfig}, SnapshotId, _LoadOpts) ->
    case beamai_process_snapshot:load(Mgr, SnapshotId) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除流程快照
-spec delete_snapshot(store_ref(), binary()) -> ok | {error, term()}.
delete_snapshot({Mgr, _ThreadConfig}, SnapshotId) ->
    case beamai_process_snapshot:delete(Mgr, SnapshotId) of
        {ok, _NewMgr} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc 列出流程快照
%%
%% 从 beamai_process_snapshot 获取快照列表，转换为 snapshot_info() 格式。
-spec list_snapshots(store_ref(), beamai_process_store_behaviour:list_opts()) ->
    {ok, [beamai_process_store_behaviour:snapshot_info()]} | {error, term()}.
list_snapshots({Mgr, ThreadConfig}, ListOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),

    Opts = case maps:get(limit, ListOpts, undefined) of
        undefined -> #{};
        Limit -> #{limit => Limit}
    end,
    Opts2 = case maps:get(offset, ListOpts, undefined) of
        undefined -> Opts;
        Offset -> Opts#{offset => Offset}
    end,

    case beamai_process_snapshot:get_history(Mgr, ThreadId, Opts2) of
        {ok, Snapshots} ->
            Infos = [snapshot_to_info(S) || S <- Snapshots],
            {ok, Infos};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支 API
%%====================================================================

%% @doc 从当前快照创建分支
%%
%% 在指定 thread 的最新快照上创建分支。
-spec branch(store_ref(), binary(), map()) ->
    {ok, #{branch_thread_id := binary(), snapshot_id := binary()}} | {error, term()}.
branch({Mgr, ThreadConfig}, BranchName, BranchOpts) ->
    ThreadId = maps:get(thread_id, ThreadConfig),

    %% 获取最新快照以 fork from
    case beamai_process_snapshot:get_latest(Mgr, ThreadId) of
        {ok, Snapshot} ->
            SnapshotId = beamai_process_snapshot:get_id(Snapshot),
            BranchThreadId = maps:get(thread_id, BranchOpts,
                                      generate_branch_thread_id(ThreadId, BranchName)),
            case beamai_process_snapshot:fork_from(Mgr, SnapshotId, BranchName, BranchThreadId) of
                {ok, ForkedSnapshot, _NewMgr} ->
                    ForkedId = beamai_process_snapshot:get_id(ForkedSnapshot),
                    {ok, #{branch_thread_id => BranchThreadId, snapshot_id => ForkedId}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 从分支加载最新快照
%%
%% 加载分支 thread 的最新 process snapshot。
-spec load_branch(store_ref(), binary(), map()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
load_branch({Mgr, _ThreadConfig}, BranchThreadId, _Opts) ->
    beamai_process_snapshot:get_latest(Mgr, BranchThreadId).

%% @doc 列出指定快照的所有分支
-spec list_branches(store_ref(), map()) ->
    {ok, [map()]} | {error, term()}.
list_branches({Mgr, _ThreadConfig}, _Opts) ->
    {ok, beamai_process_snapshot:list_branches(Mgr)}.

%% @doc 获取执行谱系（从当前快照回溯到根）
-spec get_lineage(store_ref(), map()) ->
    {ok, [map()]} | {error, term()}.
get_lineage({Mgr, ThreadConfig}, Opts) ->
    %% 获取最新快照的 ID 用于追溯
    ThreadId = maps:get(thread_id, Opts, maps:get(thread_id, ThreadConfig)),
    case beamai_process_snapshot:get_latest(Mgr, ThreadId) of
        {ok, Snapshot} ->
            SnapshotId = beamai_process_snapshot:get_id(Snapshot),
            beamai_process_snapshot:get_lineage(Mgr, SnapshotId);
        {error, _} = Error ->
            Error
    end.

%% @doc 比较两个快照（不同 thread）的差异
-spec diff(store_ref(), map(), map()) ->
    {ok, map()} | {error, term()}.
diff({Mgr, _ThreadConfig}, Config1, Config2) ->
    ThreadId1 = maps:get(thread_id, Config1),
    ThreadId2 = maps:get(thread_id, Config2),
    case {beamai_process_snapshot:get_latest(Mgr, ThreadId1),
          beamai_process_snapshot:get_latest(Mgr, ThreadId2)} of
        {{ok, Snapshot1}, {ok, Snapshot2}} ->
            beamai_process_snapshot:compare(Mgr,
                beamai_process_snapshot:get_id(Snapshot1),
                beamai_process_snapshot:get_id(Snapshot2));
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

%%====================================================================
%% 时间旅行 API
%%====================================================================

%% @doc 回退 N 步
%%
%% 在当前 thread 中回退 N 个快照，返回过去的 process snapshot。
-spec go_back(store_ref(), pos_integer()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
go_back({Mgr, ThreadConfig}, Steps) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    case beamai_process_snapshot:go_back(Mgr, ThreadId, Steps) of
        {ok, Snapshot, _NewMgr} -> {ok, Snapshot};
        {error, _} = Error -> Error
    end.

%% @doc 前进 N 步
%%
%% 在当前 thread 中前进 N 个快照（从当前位置向未来方向）。
-spec go_forward(store_ref(), pos_integer()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
go_forward({Mgr, ThreadConfig}, Steps) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    case beamai_process_snapshot:go_forward(Mgr, ThreadId, Steps) of
        {ok, Snapshot, _NewMgr} -> {ok, Snapshot};
        {error, _} = Error -> Error
    end.

%% @doc 跳转到指定快照
%%
%% 跳转到指定 SnapshotId，返回该快照的 process snapshot。
-spec goto(store_ref(), binary()) ->
    {ok, beamai_process_state:snapshot()} | {error, term()}.
goto({Mgr, ThreadConfig}, SnapshotId) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    case beamai_process_snapshot:goto(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot, _NewMgr} -> {ok, Snapshot};
        {error, _} = Error -> Error
    end.

%% @doc 列出执行历史
%%
%% 返回当前 thread 的快照历史列表。
-spec list_history(store_ref()) ->
    {ok, [map()]} | {error, term()}.
list_history({Mgr, ThreadConfig}) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    beamai_process_snapshot:get_history(Mgr, ThreadId).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将 process snapshot 转换为 snapshot_info
-spec snapshot_to_info(term()) -> beamai_process_store_behaviour:snapshot_info().
snapshot_to_info(Snapshot) ->
    #{
        id => beamai_process_snapshot:get_id(Snapshot),
        timestamp => 0,
        trigger => manual,
        process_name => undefined,
        step_id => undefined
    }.

%% @private 生成分支 thread_id
-spec generate_branch_thread_id(binary(), binary()) -> binary().
generate_branch_thread_id(ParentThreadId, BranchName) ->
    beamai_id:gen_id(<<ParentThreadId/binary, "_branch_", BranchName/binary>>).
