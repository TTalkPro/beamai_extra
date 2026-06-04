%%%-------------------------------------------------------------------
%%% @doc Snapshot - 通用快照引擎
%%%
%%% 提供通用的时间旅行和分支管理功能。
%%% 通过 behaviour 回调与具体的领域层（Process Snapshot/Graph Snapshot）集成。
%%%
%%% == 核心概念 ==
%%%
%%% - Entry: 时间线上的一个状态点
%%% - Version: 条目在时间线中的位置
%%% - Branch: 时间线分支，支持分叉和并行演化
%%% - Owner: 时间线所属者（thread_id 或 run_id）
%%%
%%% == 时间旅行模型 ==
%%%
%%% ```
%%% v0 ──→ v1 ──→ v2 ──→ v3 ──→ v4  (main branch)
%%%               │
%%%               └──→ v2' ──→ v3'   (forked branch)
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_snapshot).

-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 行为回调 (通过 -callback 声明)

%% 管理器操作
-export([
    new/2,
    new/3
]).

%% 条目操作
-export([
    save/3,
    load/2,
    delete/2,
    get_latest/2
]).

%% 时间旅行
-export([
    go_back/3,
    go_forward/3,
    goto/3,
    undo/2,
    redo/2,
    get_current_position/2
]).

%% 分支管理
-export([
    fork_from/4,
    fork_from/5,
    list_branches/1,
    get_branch/2,
    switch_branch/2,
    delete_branch/2
]).

%% 历史查询
-export([
    get_history/2,
    get_history/3,
    get_lineage/2,
    compare/3
]).

%% 类型导出
-export_type([
    manager/0,
    branch/0,
    position/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type manager() :: #{
    module := module(),
    state_store := beamai_state_store:store(),
    current_branch := binary(),
    branches := #{binary() => branch()},
    positions := #{binary() => non_neg_integer()},
    max_entries := pos_integer(),
    auto_prune := boolean()
}.

-type branch() :: #{
    id := binary(),
    name := binary(),
    head_id := binary() | undefined,
    entry_count := non_neg_integer(),
    parent_branch_id := binary() | undefined,
    forked_from_id := binary() | undefined,
    created_at := integer()
}.

-type position() :: #{
    current := non_neg_integer(),
    total := non_neg_integer(),
    entry_id := binary(),
    branch_id := binary()
}.

%%====================================================================
%% 行为回调
%%====================================================================

%% 条目访问器
-callback entry_id(Entry :: term()) -> binary() | undefined.
-callback entry_owner_id(Entry :: term()) -> binary().
-callback entry_parent_id(Entry :: term()) -> binary() | undefined.
-callback entry_version(Entry :: term()) -> non_neg_integer().
-callback entry_branch_id(Entry :: term()) -> binary().
-callback entry_created_at(Entry :: term()) -> integer().
-callback entry_state(Entry :: term()) -> map().

%% 条目修改器
-callback set_entry_id(Entry :: term(), Id :: binary()) -> term().
-callback set_entry_parent_id(Entry :: term(), ParentId :: binary() | undefined) -> term().
-callback set_entry_version(Entry :: term(), Version :: non_neg_integer()) -> term().
-callback set_entry_branch_id(Entry :: term(), BranchId :: binary()) -> term().

%% 工厂函数
-callback new_entry(OwnerId :: binary(), State :: map(), Config :: map()) -> term().

%% 转换函数
-callback entry_to_state_entry(Entry :: term()) -> #state_entry{}.
-callback state_entry_to_entry(StateEntry :: #state_entry{}) -> term().

%% 配置
-callback namespace() -> binary().
-callback id_prefix() -> binary().
-callback entry_type() -> atom().

%%====================================================================
%% 管理器操作
%%====================================================================

%% @doc 创建快照管理器
-spec new(module(), beamai_state_store:store()) -> manager().
new(Module, StateStore) ->
    new(Module, StateStore, #{}).

%% @doc 创建快照管理器（带选项）
-spec new(module(), beamai_state_store:store(), map()) -> manager().
new(Module, StateStore, Opts) ->
    MainBranch = create_branch(<<"main">>, <<"main">>, undefined, undefined),
    #{
        module => Module,
        state_store => StateStore,
        current_branch => <<"main">>,
        branches => #{<<"main">> => MainBranch},
        positions => #{},
        max_entries => maps:get(max_entries, Opts, 100),
        auto_prune => maps:get(auto_prune, Opts, true)
    }.

%%====================================================================
%% 条目操作
%%====================================================================

%% @doc 保存条目
-spec save(manager(), binary(), term()) -> {ok, term(), manager()} | {error, term()}.
save(#{module := Module, state_store := Store, current_branch := BranchId,
       branches := Branches, positions := Positions} = Mgr, OwnerId, Entry0) ->

    %% 获取当前版本
    CurrentVersion = maps:get(OwnerId, Positions, 0),
    NewVersion = CurrentVersion + 1,

    %% 获取父条目 ID（如果条目已设置 parent_id 则保留）
    ExistingParentId = Module:entry_parent_id(Entry0),
    ParentId = case ExistingParentId of
        undefined -> get_head_id(Mgr, OwnerId, BranchId);
        _ -> ExistingParentId
    end,

    %% 生成新 ID
    Prefix = Module:id_prefix(),
    NewId = beamai_state_store:generate_id(Prefix),

    %% 更新条目
    Entry1 = Module:set_entry_id(Entry0, NewId),
    Entry2 = Module:set_entry_parent_id(Entry1, ParentId),
    Entry3 = Module:set_entry_version(Entry2, NewVersion),
    Entry4 = Module:set_entry_branch_id(Entry3, BranchId),

    %% 转换为存储条目
    StateEntry = Module:entry_to_state_entry(Entry4),

    %% 保存
    case beamai_state_store:save(Store, StateEntry) of
        {ok, _} ->
            %% 更新分支头
            Branch = maps:get(BranchId, Branches),
            NewBranch = Branch#{
                head_id => NewId,
                entry_count => maps:get(entry_count, Branch, 0) + 1
            },
            NewBranches = maps:put(BranchId, NewBranch, Branches),

            %% 更新位置
            NewPositions = maps:put(OwnerId, NewVersion, Positions),

            NewMgr = Mgr#{
                branches => NewBranches,
                positions => NewPositions
            },

            %% 自动清理
            NewMgr2 = maybe_prune(NewMgr, OwnerId),

            {ok, Entry4, NewMgr2};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载条目
-spec load(manager(), binary()) -> {ok, term()} | {error, term()}.
load(#{module := Module, state_store := Store}, EntryId) ->
    case beamai_state_store:load(Store, EntryId) of
        {ok, StateEntry} ->
            {ok, Module:state_entry_to_entry(StateEntry)};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除条目
-spec delete(manager(), binary()) -> {ok, manager()} | {error, term()}.
delete(#{state_store := Store} = Mgr, EntryId) ->
    case beamai_state_store:delete(Store, EntryId) of
        ok ->
            {ok, Mgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取最新条目
-spec get_latest(manager(), binary()) -> {ok, term()} | {error, term()}.
get_latest(#{current_branch := BranchId} = Mgr, OwnerId) ->
    case get_history(Mgr, OwnerId, #{branch_id => BranchId, limit => 1, order => desc}) of
        {ok, [Entry | _]} ->
            {ok, Entry};
        {ok, []} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个版本
-spec go_back(manager(), binary(), pos_integer()) ->
    {ok, term(), manager()} | {error, term()}.
go_back(Mgr, OwnerId, Steps) ->
    case get_history(Mgr, OwnerId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_position_in_history(Mgr, OwnerId, History),
            TargetPos = max(0, CurrentPos - Steps),
            goto_position(Mgr, OwnerId, History, TargetPos);
        {ok, []} ->
            {error, no_history};
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 个版本
-spec go_forward(manager(), binary(), pos_integer()) ->
    {ok, term(), manager()} | {error, term()}.
go_forward(Mgr, OwnerId, Steps) ->
    case get_history(Mgr, OwnerId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_position_in_history(Mgr, OwnerId, History),
            MaxPos = length(History) - 1,
            TargetPos = min(MaxPos, CurrentPos + Steps),
            goto_position(Mgr, OwnerId, History, TargetPos);
        {ok, []} ->
            {error, no_history};
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定条目
-spec goto(manager(), binary(), binary()) ->
    {ok, term(), manager()} | {error, term()}.
goto(#{module := Module, positions := Positions} = Mgr, OwnerId, EntryId) ->
    case load(Mgr, EntryId) of
        {ok, Entry} ->
            Version = Module:entry_version(Entry),
            NewPositions = maps:put(OwnerId, Version, Positions),
            NewMgr = Mgr#{positions => NewPositions},
            {ok, Entry, NewMgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销（回退一步）
-spec undo(manager(), binary()) -> {ok, term(), manager()} | {error, term()}.
undo(Mgr, OwnerId) ->
    go_back(Mgr, OwnerId, 1).

%% @doc 重做（前进一步）
-spec redo(manager(), binary()) -> {ok, term(), manager()} | {error, term()}.
redo(Mgr, OwnerId) ->
    go_forward(Mgr, OwnerId, 1).

%% @doc 获取当前位置信息
-spec get_current_position(manager(), binary()) -> {ok, position()} | {error, term()}.
get_current_position(#{module := Module, current_branch := BranchId} = Mgr, OwnerId) ->
    case get_history(Mgr, OwnerId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_position_in_history(Mgr, OwnerId, History),
            Entry = lists:nth(CurrentPos + 1, History),
            {ok, #{
                current => CurrentPos,
                total => length(History),
                entry_id => Module:entry_id(Entry),
                branch_id => BranchId
            }};
        {ok, []} ->
            {error, no_history};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 从指定条目创建分支
-spec fork_from(manager(), binary(), binary(), binary()) ->
    {ok, term(), manager()} | {error, term()}.
fork_from(Mgr, EntryId, NewBranchName, OwnerId) ->
    fork_from(Mgr, EntryId, NewBranchName, OwnerId, #{}).

-spec fork_from(manager(), binary(), binary(), binary(), map()) ->
    {ok, term(), manager()} | {error, term()}.
fork_from(#{module := Module, branches := Branches, current_branch := CurrentBranch} = Mgr,
          EntryId, NewBranchName, OwnerId, Opts) ->
    case load(Mgr, EntryId) of
        {ok, SourceEntry} ->
            %% 生成新分支 ID
            NewBranchId = beamai_state_store:generate_id(<<"branch_">>),

            %% 创建新分支记录
            NewBranch = create_branch(NewBranchId, NewBranchName, CurrentBranch, EntryId),
            NewBranches = maps:put(NewBranchId, NewBranch, Branches),

            %% 创建新条目（复制源条目状态）
            SourceState = Module:entry_state(SourceEntry),
            NewEntry0 = Module:new_entry(OwnerId, SourceState, Opts),
            NewEntry1 = Module:set_entry_parent_id(NewEntry0, EntryId),
            NewEntry2 = Module:set_entry_branch_id(NewEntry1, NewBranchId),

            %% 切换到新分支并保存
            Mgr1 = Mgr#{
                branches => NewBranches,
                current_branch => NewBranchId
            },

            save(Mgr1, OwnerId, NewEntry2);
        {error, _} = Error ->
            Error
    end.

%% @doc 列出所有分支
-spec list_branches(manager()) -> [branch()].
list_branches(#{branches := Branches}) ->
    maps:values(Branches).

%% @doc 获取指定分支
-spec get_branch(manager(), binary()) -> {ok, branch()} | {error, not_found}.
get_branch(#{branches := Branches}, BranchId) ->
    case maps:find(BranchId, Branches) of
        {ok, Branch} -> {ok, Branch};
        error -> {error, not_found}
    end.

%% @doc 切换当前分支
-spec switch_branch(manager(), binary()) -> {ok, manager()} | {error, not_found}.
switch_branch(#{branches := Branches} = Mgr, BranchId) ->
    case maps:is_key(BranchId, Branches) of
        true ->
            {ok, Mgr#{current_branch => BranchId}};
        false ->
            {error, not_found}
    end.

%% @doc 删除分支
-spec delete_branch(manager(), binary()) -> {ok, manager()} | {error, term()}.
delete_branch(#{current_branch := CurrentBranch}, BranchId)
  when BranchId =:= CurrentBranch ->
    {error, cannot_delete_current_branch};
delete_branch(#{branches := Branches} = Mgr, BranchId) ->
    case maps:is_key(BranchId, Branches) of
        true ->
            %% TODO: 删除分支下的所有条目
            NewBranches = maps:remove(BranchId, Branches),
            {ok, Mgr#{branches => NewBranches}};
        false ->
            {error, not_found}
    end.

%%====================================================================
%% 历史查询
%%====================================================================

%% @doc 获取历史记录
-spec get_history(manager(), binary()) -> {ok, [term()]} | {error, term()}.
get_history(Mgr, OwnerId) ->
    get_history(Mgr, OwnerId, #{}).

-spec get_history(manager(), binary(), map()) -> {ok, [term()]} | {error, term()}.
get_history(#{module := Module, state_store := Store, current_branch := DefaultBranch},
            OwnerId, Opts) ->
    BranchId = maps:get(branch_id, Opts, DefaultBranch),
    Limit = maps:get(limit, Opts, 1000),
    Order = maps:get(order, Opts, asc),

    ListOpts = #{
        owner_id => OwnerId,
        branch_id => BranchId,
        limit => Limit,
        order => Order
    },

    case beamai_state_store:list(Store, ListOpts) of
        {ok, StateEntries} ->
            Entries = [Module:state_entry_to_entry(SE) || SE <- StateEntries],
            {ok, Entries};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取血统（从当前条目追溯到根）
-spec get_lineage(manager(), binary()) -> {ok, [term()]} | {error, term()}.
get_lineage(Mgr, EntryId) ->
    get_lineage_acc(Mgr, EntryId, []).

%% @doc 比较两个条目
-spec compare(manager(), binary(), binary()) ->
    {ok, #{from := term(), to := term(), version_diff := integer()}} | {error, term()}.
compare(#{module := Module} = Mgr, EntryId1, EntryId2) ->
    case {load(Mgr, EntryId1), load(Mgr, EntryId2)} of
        {{ok, Entry1}, {ok, Entry2}} ->
            V1 = Module:entry_version(Entry1),
            V2 = Module:entry_version(Entry2),
            {ok, #{
                from => Entry1,
                to => Entry2,
                version_diff => V2 - V1
            }};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 创建分支记录
-spec create_branch(binary(), binary(), binary() | undefined, binary() | undefined) -> branch().
create_branch(Id, Name, ParentBranchId, ForkedFromId) ->
    #{
        id => Id,
        name => Name,
        head_id => undefined,
        entry_count => 0,
        parent_branch_id => ParentBranchId,
        forked_from_id => ForkedFromId,
        created_at => erlang:system_time(millisecond)
    }.

%% @private 获取分支头条目 ID
-spec get_head_id(manager(), binary(), binary()) -> binary() | undefined.
get_head_id(#{module := Module} = Mgr, OwnerId, BranchId) ->
    case get_history(Mgr, OwnerId, #{branch_id => BranchId, limit => 1, order => desc}) of
        {ok, [Entry | _]} ->
            Module:entry_id(Entry);
        _ ->
            undefined
    end.

%% @private 获取当前在历史中的位置
-spec get_position_in_history(manager(), binary(), [term()]) -> non_neg_integer().
get_position_in_history(#{module := Module, positions := Positions}, OwnerId, History) ->
    CurrentVersion = maps:get(OwnerId, Positions, 0),
    case CurrentVersion of
        0 ->
            %% 没有记录位置，默认在最后
            length(History) - 1;
        _ ->
            %% 查找对应版本的位置
            case lists:search(
                fun(E) -> Module:entry_version(E) =:= CurrentVersion end,
                History
            ) of
                {value, _} ->
                    find_index(
                        fun(E) -> Module:entry_version(E) =:= CurrentVersion end,
                        History,
                        0
                    );
                false ->
                    %% 版本不存在，返回最近的较小版本位置
                    Filtered = lists:filter(
                        fun(E) -> Module:entry_version(E) < CurrentVersion end,
                        History
                    ),
                    length(Filtered) - 1
            end
    end.

%% @private 查找索引
-spec find_index(fun((term()) -> boolean()), [term()], non_neg_integer()) -> non_neg_integer().
find_index(_Pred, [], Idx) ->
    Idx;
find_index(Pred, [H | T], Idx) ->
    case Pred(H) of
        true -> Idx;
        false -> find_index(Pred, T, Idx + 1)
    end.

%% @private 跳转到指定位置
-spec goto_position(manager(), binary(), [term()], non_neg_integer()) ->
    {ok, term(), manager()} | {error, term()}.
goto_position(#{module := Module, positions := Positions} = Mgr, OwnerId, History, Position) ->
    Entry = lists:nth(Position + 1, History),
    Version = Module:entry_version(Entry),
    NewPositions = maps:put(OwnerId, Version, Positions),
    NewMgr = Mgr#{positions => NewPositions},
    {ok, Entry, NewMgr}.

%% @private 递归获取血统
%% 返回从根到当前条目的列表 [root, ..., current]
-spec get_lineage_acc(manager(), binary() | undefined, [term()]) ->
    {ok, [term()]} | {error, term()}.
get_lineage_acc(_Mgr, undefined, Acc) ->
    %% Acc 已经是 [root, ..., current] 顺序，无需反转
    {ok, Acc};
get_lineage_acc(#{module := Module} = Mgr, EntryId, Acc) ->
    case load(Mgr, EntryId) of
        {ok, Entry} ->
            ParentId = Module:entry_parent_id(Entry),
            %% 先递归获取祖先，再追加当前条目
            get_lineage_acc(Mgr, ParentId, [Entry | Acc]);
        {error, _} = Error ->
            Error
    end.

%% @private 自动清理旧条目
-spec maybe_prune(manager(), binary()) -> manager().
maybe_prune(#{auto_prune := false} = Mgr, _OwnerId) ->
    Mgr;
maybe_prune(#{auto_prune := true, max_entries := MaxEntries, state_store := Store} = Mgr, OwnerId) ->
    case beamai_state_store:count_by_owner(Store, OwnerId) of
        {ok, Count} when Count > MaxEntries ->
            %% 删除最旧的条目
            ToDelete = Count - MaxEntries,
            case beamai_state_store:list_by_owner(Store, OwnerId, #{limit => ToDelete, order => asc}) of
                {ok, OldEntries} ->
                    Ids = [E#state_entry.id || E <- OldEntries],
                    beamai_state_store:batch_delete(Store, Ids),
                    Mgr;
                _ ->
                    Mgr
            end;
        _ ->
            Mgr
    end.
