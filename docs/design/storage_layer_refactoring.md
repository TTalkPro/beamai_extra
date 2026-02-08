# beamai_memory 存储分层重构方案

## 1. 整体架构

```
┌─────────────────────────────────────────────────────────────────────┐
│                        beamai_memory (API)                          │
│           save_snapshot/3, load_checkpoint/2, restore/2             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│   ┌─────────────────────────────────────────────────────────────┐   │
│   │              Layer 2: MetaInfo Layer (元信息层)              │   │
│   │                                                             │   │
│   │  ┌─────────────────────┐    ┌─────────────────────────────┐ │   │
│   │  │  beamai_checkpoint  │    │     beamai_snapshot         │ │   │
│   │  │    (轻量级检查点)    │    │      (完整快照)             │ │   │
│   │  │                     │    │                             │ │   │
│   │  │ - step_id           │    │ - parent_id                 │ │   │
│   │  │ - step_index        │    │ - branch_id                 │ │   │
│   │  │ - process_state     │    │ - snapshot_type             │ │   │
│   │  │ - activation_count  │    │ - time_travel               │ │   │
│   │  │ - recoverable       │    │ - lineage                   │ │   │
│   │  │ - retry_count       │    │ - diff                      │ │   │
│   │  └──────────┬──────────┘    └──────────────┬──────────────┘ │   │
│   │             │                              │                │   │
│   │             └──────────────┬───────────────┘                │   │
│   │                            │                                │   │
│   └────────────────────────────┼────────────────────────────────┘   │
│                                │                                    │
│   ┌────────────────────────────┼────────────────────────────────┐   │
│   │              Layer 1: State Store (通用状态存储层)           │   │
│   │                            │                                │   │
│   │                 ┌──────────▼──────────┐                     │   │
│   │                 │  beamai_state_store │                     │   │
│   │                 │                     │                     │   │
│   │                 │  - save/3           │                     │   │
│   │                 │  - load/2           │                     │   │
│   │                 │  - delete/2         │                     │   │
│   │                 │  - list/1           │                     │   │
│   │                 │  - exists/2         │                     │   │
│   │                 │  - batch_save/2     │                     │   │
│   │                 │  - batch_load/2     │                     │   │
│   │                 └──────────┬──────────┘                     │   │
│   │                            │                                │   │
│   └────────────────────────────┼────────────────────────────────┘   │
│                                │                                    │
├────────────────────────────────┼────────────────────────────────────┤
│                    Backend Layer (后端层)                           │
│                                │                                    │
│            ┌───────────────────┴───────────────────┐                │
│            │                                       │                │
│   ┌────────▼────────┐                   ┌──────────▼──────────┐     │
│   │ beamai_store_ets│                   │ beamai_store_sqlite │     │
│   │    (内存后端)    │                   │    (持久化后端)      │     │
│   └─────────────────┘                   └─────────────────────┘     │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## 2. Layer 1: 通用状态存储层

### 2.1 头文件定义

**文件**: `include/beamai_state_store.hrl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc 通用状态存储层 - 头文件
%%%
%%% 定义纯粹的状态存储相关记录，不包含任何业务语义。
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_STATE_STORE_HRL).
-define(BEAMAI_STATE_STORE_HRL, true).

%%====================================================================
%% 存储的状态记录
%%====================================================================

%% 存储的状态 - 纯数据，无业务语义
-record(stored_state, {
    %% 唯一标识（由存储层生成或调用方提供）
    id :: binary(),

    %% 所属的逻辑分组（如 thread_id）
    group_id :: binary(),

    %% 状态数据（JSON 可序列化）
    data :: map(),

    %% 版本号（用于乐观锁和冲突检测）
    version = 1 :: pos_integer(),

    %% 时间戳
    created_at :: integer(),
    updated_at :: integer()
}).

%%====================================================================
%% 存储选项
%%====================================================================

%% 保存选项
-record(save_opts, {
    %% 自定义 ID（如果不提供则自动生成）
    id :: binary() | undefined,

    %% 是否覆盖已存在的状态
    overwrite = true :: boolean(),

    %% 期望的版本号（用于乐观锁）
    expected_version :: pos_integer() | undefined,

    %% TTL（秒），undefined 表示永不过期
    ttl :: pos_integer() | undefined
}).

%% 加载选项
-record(load_opts, {
    %% 是否包含已过期的状态
    include_expired = false :: boolean()
}).

%% 列表选项
-record(list_opts_v2, {
    %% 时间范围
    from_time :: integer() | undefined,
    to_time :: integer() | undefined,

    %% 排序
    order = desc :: asc | desc,

    %% 分页
    offset = 0 :: non_neg_integer(),
    limit = 100 :: pos_integer()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 默认命名空间前缀
-define(NS_STATES, <<"states">>).

%% 版本冲突错误
-define(ERR_VERSION_CONFLICT, version_conflict).

%% 状态不存在错误
-define(ERR_NOT_FOUND, not_found).

-endif.
```

### 2.2 通用状态存储模块

**文件**: `src/store/beamai_state_store.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc 通用状态存储层
%%%
%%% 提供纯粹的状态存储和恢复功能，不包含任何业务语义。
%%% 作为 Layer 1，为上层（checkpoint、snapshot）提供基础存储能力。
%%%
%%% == 设计原则 ==
%%%
%%% 1. 纯粹性：只负责状态的存储和恢复，不包含任何业务逻辑
%%% 2. 无状态：不维护任何内部状态，所有状态存储在后端
%%% 3. 可组合：可被上层模块自由组合使用
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建状态存储
%%% StateStore = beamai_state_store:new(Store, #{
%%%     namespace_prefix => [<<"my_app">>]
%%% }),
%%%
%%% %% 保存状态
%%% {ok, Id} = beamai_state_store:save(StateStore, <<"group-1">>, #{
%%%     messages => [],
%%%     context => #{}
%%% }),
%%%
%%% %% 加载状态
%%% {ok, State} = beamai_state_store:load(StateStore, <<"group-1">>, Id).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_store).

-include_lib("beamai_memory/include/beamai_state_store.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 核心 CRUD
-export([
    save/3, save/4,
    load/3, load/4,
    delete/3,
    exists/3
]).

%% 列表和查询
-export([
    list/2, list/3,
    count/2
]).

%% 批量操作
-export([
    batch_save/3,
    batch_load/3,
    batch_delete/3
]).

%% 工具函数
-export([
    generate_id/0,
    get_store/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(state_store, {
    %% 底层存储
    store :: beamai_store:store(),

    %% 命名空间前缀
    namespace_prefix :: [binary()],

    %% ID 生成器（可自定义）
    id_generator :: fun(() -> binary())
}).

-opaque state_store() :: #state_store{}.
-type group_id() :: binary().
-type state_id() :: binary().
-type state_data() :: map().

-export_type([state_store/0, group_id/0, state_id/0, state_data/0]).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建状态存储
-spec new(beamai_store:store()) -> state_store().
new(Store) ->
    new(Store, #{}).

%% @doc 创建状态存储（带选项）
%%
%% Opts:
%% - namespace_prefix => [binary()] - 命名空间前缀
%% - id_generator => fun(() -> binary()) - 自定义 ID 生成器
-spec new(beamai_store:store(), map()) -> state_store().
new(Store, Opts) ->
    #state_store{
        store = Store,
        namespace_prefix = maps:get(namespace_prefix, Opts, [?NS_STATES]),
        id_generator = maps:get(id_generator, Opts, fun generate_id/0)
    }.

%%====================================================================
%% 核心 CRUD
%%====================================================================

%% @doc 保存状态
%%
%% 将状态数据保存到指定分组下，返回生成的状态 ID。
-spec save(state_store(), group_id(), state_data()) ->
    {ok, state_id()} | {error, term()}.
save(StateStore, GroupId, Data) ->
    save(StateStore, GroupId, Data, #save_opts{}).

%% @doc 保存状态（带选项）
-spec save(state_store(), group_id(), state_data(), #save_opts{}) ->
    {ok, state_id()} | {error, term()}.
save(#state_store{store = Store, namespace_prefix = Prefix, id_generator = IdGen},
     GroupId, Data, #save_opts{id = CustomId, overwrite = Overwrite, expected_version = ExpectedVersion}) ->

    %% 生成或使用自定义 ID
    StateId = case CustomId of
        undefined -> IdGen();
        _ -> CustomId
    end,

    Namespace = Prefix ++ [GroupId],
    Now = erlang:system_time(millisecond),

    %% 检查是否存在（如果需要版本控制）
    case ExpectedVersion of
        undefined ->
            do_save(Store, Namespace, StateId, Data, Now, Overwrite);
        Version ->
            do_save_with_version(Store, Namespace, StateId, Data, Now, Version)
    end.

%% @doc 加载状态
-spec load(state_store(), group_id(), state_id()) ->
    {ok, #stored_state{}} | {error, not_found | term()}.
load(StateStore, GroupId, StateId) ->
    load(StateStore, GroupId, StateId, #load_opts{}).

%% @doc 加载状态（带选项）
-spec load(state_store(), group_id(), state_id(), #load_opts{}) ->
    {ok, #stored_state{}} | {error, not_found | term()}.
load(#state_store{store = Store, namespace_prefix = Prefix},
     GroupId, StateId, _LoadOpts) ->

    Namespace = Prefix ++ [GroupId],
    case beamai_store:get(Store, Namespace, StateId) of
        {ok, #store_item{value = Value}} ->
            {ok, map_to_stored_state(Value)};
        {error, not_found} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除状态
-spec delete(state_store(), group_id(), state_id()) -> ok | {error, term()}.
delete(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateId) ->
    Namespace = Prefix ++ [GroupId],
    beamai_store:delete(Store, Namespace, StateId).

%% @doc 检查状态是否存在
-spec exists(state_store(), group_id(), state_id()) -> boolean().
exists(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateId) ->
    Namespace = Prefix ++ [GroupId],
    case beamai_store:get(Store, Namespace, StateId) of
        {ok, _} -> true;
        {error, not_found} -> false;
        {error, _} -> false
    end.

%%====================================================================
%% 列表和查询
%%====================================================================

%% @doc 列出分组下的所有状态
-spec list(state_store(), group_id()) -> {ok, [#stored_state{}]} | {error, term()}.
list(StateStore, GroupId) ->
    list(StateStore, GroupId, #list_opts_v2{}).

%% @doc 列出分组下的状态（带选项）
-spec list(state_store(), group_id(), #list_opts_v2{}) ->
    {ok, [#stored_state{}]} | {error, term()}.
list(#state_store{store = Store, namespace_prefix = Prefix}, GroupId,
     #list_opts_v2{order = Order, offset = Offset, limit = Limit}) ->

    Namespace = Prefix ++ [GroupId],
    SearchOpts = #{limit => Limit, offset => Offset},

    case beamai_store:search(Store, Namespace, SearchOpts) of
        {ok, Results} ->
            States = [map_to_stored_state(Item#store_item.value)
                      || #search_result{item = Item} <- Results],

            %% 按时间戳排序
            Sorted = case Order of
                desc -> lists:sort(fun(A, B) -> A#stored_state.updated_at > B#stored_state.updated_at end, States);
                asc -> lists:sort(fun(A, B) -> A#stored_state.updated_at < B#stored_state.updated_at end, States)
            end,
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

%% @doc 统计分组下的状态数量
-spec count(state_store(), group_id()) -> non_neg_integer().
count(#state_store{store = Store, namespace_prefix = Prefix}, GroupId) ->
    Namespace = Prefix ++ [GroupId],
    case beamai_store:search(Store, Namespace, #{limit => 100000}) of
        {ok, Results} -> length(Results);
        {error, _} -> 0
    end.

%%====================================================================
%% 批量操作
%%====================================================================

%% @doc 批量保存状态
-spec batch_save(state_store(), group_id(), [{state_id() | undefined, state_data()}]) ->
    {ok, [state_id()]} | {error, term()}.
batch_save(#state_store{store = Store, namespace_prefix = Prefix, id_generator = IdGen},
           GroupId, Items) ->

    Namespace = Prefix ++ [GroupId],
    Now = erlang:system_time(millisecond),

    %% 构建批量操作
    {Ops, Ids} = lists:foldl(fun({MaybeId, Data}, {AccOps, AccIds}) ->
        StateId = case MaybeId of
            undefined -> IdGen();
            _ -> MaybeId
        end,
        Value = stored_state_to_map(StateId, GroupId, Data, Now),
        Op = {put, Namespace, StateId, Value},
        {[Op | AccOps], [StateId | AccIds]}
    end, {[], []}, Items),

    case beamai_store:batch(Store, lists:reverse(Ops)) of
        {ok, _} -> {ok, lists:reverse(Ids)};
        {error, _} = Error -> Error
    end.

%% @doc 批量加载状态
-spec batch_load(state_store(), group_id(), [state_id()]) ->
    {ok, [{state_id(), #stored_state{} | not_found}]} | {error, term()}.
batch_load(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateIds) ->
    Namespace = Prefix ++ [GroupId],

    Ops = [{get, Namespace, StateId} || StateId <- StateIds],

    case beamai_store:batch(Store, Ops) of
        {ok, Results} ->
            Pairs = lists:zipwith(fun(StateId, Result) ->
                case Result of
                    {ok, #store_item{value = Value}} ->
                        {StateId, map_to_stored_state(Value)};
                    {error, not_found} ->
                        {StateId, not_found};
                    _ ->
                        {StateId, not_found}
                end
            end, StateIds, Results),
            {ok, Pairs};
        {error, _} = Error ->
            Error
    end.

%% @doc 批量删除状态
-spec batch_delete(state_store(), group_id(), [state_id()]) -> ok | {error, term()}.
batch_delete(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateIds) ->
    Namespace = Prefix ++ [GroupId],
    Ops = [{delete, Namespace, StateId} || StateId <- StateIds],

    case beamai_store:batch(Store, Ops) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 生成状态 ID
%%
%% 格式: st_<timestamp_hex>_<random_hex>
-spec generate_id() -> state_id().
generate_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("st_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @doc 获取底层 Store
-spec get_store(state_store()) -> beamai_store:store().
get_store(#state_store{store = Store}) ->
    Store.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行保存
-spec do_save(beamai_store:store(), [binary()], state_id(), map(), integer(), boolean()) ->
    {ok, state_id()} | {error, term()}.
do_save(Store, Namespace, StateId, Data, Now, _Overwrite) ->
    Value = stored_state_to_map(StateId, lists:last(Namespace), Data, Now),
    case beamai_store:put(Store, Namespace, StateId, Value) of
        ok -> {ok, StateId};
        {error, _} = Error -> Error
    end.

%% @private 带版本检查的保存
-spec do_save_with_version(beamai_store:store(), [binary()], state_id(), map(), integer(), pos_integer()) ->
    {ok, state_id()} | {error, term()}.
do_save_with_version(Store, Namespace, StateId, Data, Now, ExpectedVersion) ->
    case beamai_store:get(Store, Namespace, StateId) of
        {ok, #store_item{value = ExistingValue}} ->
            CurrentVersion = maps:get(<<"version">>, ExistingValue, 1),
            if
                CurrentVersion =:= ExpectedVersion ->
                    NewVersion = CurrentVersion + 1,
                    Value = stored_state_to_map_with_version(StateId, lists:last(Namespace), Data, Now, NewVersion),
                    case beamai_store:put(Store, Namespace, StateId, Value) of
                        ok -> {ok, StateId};
                        {error, _} = Error -> Error
                    end;
                true ->
                    {error, ?ERR_VERSION_CONFLICT}
            end;
        {error, not_found} when ExpectedVersion =:= 0 ->
            %% 新建时期望版本为 0
            Value = stored_state_to_map(StateId, lists:last(Namespace), Data, Now),
            case beamai_store:put(Store, Namespace, StateId, Value) of
                ok -> {ok, StateId};
                {error, _} = Error -> Error
            end;
        {error, not_found} ->
            {error, ?ERR_NOT_FOUND};
        {error, _} = Error ->
            Error
    end.

%% @private 状态记录转 Map
-spec stored_state_to_map(state_id(), group_id(), map(), integer()) -> map().
stored_state_to_map(StateId, GroupId, Data, Now) ->
    #{
        <<"id">> => StateId,
        <<"group_id">> => GroupId,
        <<"data">> => Data,
        <<"version">> => 1,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    }.

%% @private 带版本的状态记录转 Map
-spec stored_state_to_map_with_version(state_id(), group_id(), map(), integer(), pos_integer()) -> map().
stored_state_to_map_with_version(StateId, GroupId, Data, Now, Version) ->
    #{
        <<"id">> => StateId,
        <<"group_id">> => GroupId,
        <<"data">> => Data,
        <<"version">> => Version,
        <<"created_at">> => Now,  % 保持原创建时间会更好，这里简化处理
        <<"updated_at">> => Now
    }.

%% @private Map 转状态记录
-spec map_to_stored_state(map()) -> #stored_state{}.
map_to_stored_state(Map) ->
    #stored_state{
        id = maps:get(<<"id">>, Map),
        group_id = maps:get(<<"group_id">>, Map),
        data = maps:get(<<"data">>, Map, #{}),
        version = maps:get(<<"version">>, Map, 1),
        created_at = maps:get(<<"created_at">>, Map, 0),
        updated_at = maps:get(<<"updated_at">>, Map, 0)
    }.
```

## 3. Layer 2: MetaInfo Layer

### 3.1 Checkpoint 模块（轻量级检查点）

**文件**: `include/beamai_checkpoint.hrl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Checkpoint 头文件 - 轻量级检查点
%%%
%%% 专注于流程恢复场景，提供：
%%% - 步骤执行状态
%%% - 错误恢复信息
%%% - 激活计数
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_CHECKPOINT_HRL).
-define(BEAMAI_CHECKPOINT_HRL, true).

%%====================================================================
%% Checkpoint MetaInfo
%%====================================================================

%% Checkpoint 元信息 - 专注于恢复场景
-record(checkpoint_meta, {
    %%--------------------------------------------------------------------
    %% 步骤执行信息
    %%--------------------------------------------------------------------

    %% 当前步骤 ID
    step_id :: atom() | undefined,

    %% 步骤在流程中的索引
    step_index :: non_neg_integer() | undefined,

    %% 流程状态
    process_state :: idle | running | paused | failed | completed,

    %% 各步骤激活计数
    step_activations = #{} :: #{atom() => non_neg_integer()},

    %%--------------------------------------------------------------------
    %% 恢复信息
    %%--------------------------------------------------------------------

    %% 是否可恢复
    recoverable = true :: boolean(),

    %% 重试次数
    retry_count = 0 :: non_neg_integer(),

    %% 最大重试次数
    max_retries = 3 :: non_neg_integer(),

    %% 最后一次错误
    last_error :: binary() | undefined,

    %% 错误发生时间
    error_at :: integer() | undefined,

    %%--------------------------------------------------------------------
    %% 执行标识
    %%--------------------------------------------------------------------

    %% 流程名称
    process_name :: atom() | undefined,

    %% 执行 ID
    run_id :: binary() | undefined,

    %% Agent 标识
    agent_id :: binary() | undefined
}).

%% Checkpoint - 轻量级检查点
-record(checkpoint, {
    %% 检查点 ID
    id :: binary(),

    %% 线程 ID
    thread_id :: binary(),

    %% 状态数据（委托给 state_store）
    state :: map(),

    %% 元信息
    meta :: #checkpoint_meta{},

    %% 时间戳
    created_at :: integer()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间
-define(NS_CHECKPOINTS, <<"checkpoints">>).

%% 默认最大检查点数
-define(DEFAULT_MAX_CHECKPOINTS, 10).

%% ID 前缀
-define(CHECKPOINT_ID_PREFIX, <<"cp_">>).

-endif.
```

**文件**: `src/checkpoint/beamai_checkpoint.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Checkpoint 模块 - 轻量级检查点
%%%
%%% 基于 state_store 实现的轻量级检查点，专注于流程恢复场景。
%%% 相比 snapshot，checkpoint 更轻量，主要记录：
%%% - 步骤执行状态
%%% - 错误恢复信息
%%% - 激活计数
%%%
%%% == 使用场景 ==
%%%
%%% - 步骤执行失败后的自动恢复
%%% - 流程暂停/继续
%%% - 调试和监控
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 checkpoint 管理器
%%% CpMgr = beamai_checkpoint:new(StateStore, #{
%%%     max_checkpoints => 10
%%% }),
%%%
%%% %% 创建检查点
%%% Meta = #checkpoint_meta{
%%%     step_id = step_1,
%%%     step_index = 0,
%%%     process_state = running
%%% },
%%% {ok, CpId} = beamai_checkpoint:create(CpMgr, <<"thread-1">>, State, Meta),
%%%
%%% %% 恢复检查点
%%% {ok, {State, Meta}} = beamai_checkpoint:restore(CpMgr, <<"thread-1">>, CpId).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint).

-include_lib("beamai_memory/include/beamai_checkpoint.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 核心操作
-export([
    create/4,
    restore/3,
    get_latest/2,
    delete/3
]).

%% 恢复相关
-export([
    mark_failed/3,
    mark_recovered/2,
    can_retry/2,
    get_recovery_info/2
]).

%% 查询
-export([
    list/2, list/3,
    count/2
]).

%% 清理
-export([
    prune/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(checkpoint_manager, {
    %% 底层状态存储
    state_store :: beamai_state_store:state_store(),

    %% 最大检查点数量（每线程）
    max_checkpoints :: pos_integer(),

    %% 自动清理
    auto_prune :: boolean()
}).

-opaque manager() :: #checkpoint_manager{}.
-type thread_id() :: binary().
-type checkpoint_id() :: binary().

-export_type([manager/0]).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Checkpoint 管理器
-spec new(beamai_state_store:state_store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

%% @doc 创建 Checkpoint 管理器（带选项）
-spec new(beamai_state_store:state_store(), map()) -> manager().
new(StateStore, Opts) ->
    #checkpoint_manager{
        state_store = StateStore,
        max_checkpoints = maps:get(max_checkpoints, Opts, ?DEFAULT_MAX_CHECKPOINTS),
        auto_prune = maps:get(auto_prune, Opts, true)
    }.

%%====================================================================
%% 核心操作
%%====================================================================

%% @doc 创建检查点
-spec create(manager(), thread_id(), map(), #checkpoint_meta{}) ->
    {ok, checkpoint_id()} | {error, term()}.
create(#checkpoint_manager{state_store = StateStore, auto_prune = AutoPrune, max_checkpoints = Max} = Mgr,
       ThreadId, State, Meta) ->

    %% 自动清理
    case AutoPrune of
        true -> maybe_prune(Mgr, ThreadId, Max);
        false -> ok
    end,

    %% 构建检查点数据
    CpId = generate_checkpoint_id(),
    Now = erlang:system_time(millisecond),

    Data = #{
        <<"state">> => State,
        <<"meta">> => checkpoint_meta_to_map(Meta),
        <<"created_at">> => Now
    },

    %% 使用 state_store 保存
    SaveOpts = #save_opts{id = CpId},
    case beamai_state_store:save(StateStore, ThreadId, Data, SaveOpts) of
        {ok, CpId} ->
            %% 更新 latest 指针
            update_latest_index(StateStore, ThreadId, CpId),
            {ok, CpId};
        {error, _} = Error ->
            Error
    end.

%% @doc 恢复检查点
-spec restore(manager(), thread_id(), checkpoint_id()) ->
    {ok, {map(), #checkpoint_meta{}}} | {error, not_found | term()}.
restore(#checkpoint_manager{state_store = StateStore}, ThreadId, CpId) ->
    case beamai_state_store:load(StateStore, ThreadId, CpId) of
        {ok, #stored_state{data = Data}} ->
            State = maps:get(<<"state">>, Data, #{}),
            MetaMap = maps:get(<<"meta">>, Data, #{}),
            Meta = map_to_checkpoint_meta(MetaMap),
            {ok, {State, Meta}};
        {error, _} = Error ->
            Error
    end.

%% @doc 获取最新检查点
-spec get_latest(manager(), thread_id()) ->
    {ok, {checkpoint_id(), map(), #checkpoint_meta{}}} | {error, not_found}.
get_latest(#checkpoint_manager{state_store = StateStore}, ThreadId) ->
    %% 从索引获取最新 ID
    IndexKey = <<"_latest">>,
    case beamai_state_store:load(StateStore, <<ThreadId/binary, "_index">>, IndexKey) of
        {ok, #stored_state{data = #{<<"checkpoint_id">> := LatestId}}} ->
            case beamai_state_store:load(StateStore, ThreadId, LatestId) of
                {ok, #stored_state{data = Data}} ->
                    State = maps:get(<<"state">>, Data, #{}),
                    MetaMap = maps:get(<<"meta">>, Data, #{}),
                    Meta = map_to_checkpoint_meta(MetaMap),
                    {ok, {LatestId, State, Meta}};
                {error, _} = Error ->
                    Error
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除检查点
-spec delete(manager(), thread_id(), checkpoint_id()) -> ok | {error, term()}.
delete(#checkpoint_manager{state_store = StateStore}, ThreadId, CpId) ->
    beamai_state_store:delete(StateStore, ThreadId, CpId).

%%====================================================================
%% 恢复相关
%%====================================================================

%% @doc 标记检查点失败
-spec mark_failed(manager(), thread_id(), binary()) -> ok | {error, term()}.
mark_failed(Mgr, ThreadId, ErrorMsg) ->
    case get_latest(Mgr, ThreadId) of
        {ok, {CpId, State, Meta}} ->
            NewMeta = Meta#checkpoint_meta{
                process_state = failed,
                retry_count = Meta#checkpoint_meta.retry_count + 1,
                last_error = ErrorMsg,
                error_at = erlang:system_time(millisecond)
            },
            %% 更新检查点
            #checkpoint_manager{state_store = StateStore} = Mgr,
            Data = #{
                <<"state">> => State,
                <<"meta">> => checkpoint_meta_to_map(NewMeta),
                <<"created_at">> => erlang:system_time(millisecond)
            },
            case beamai_state_store:save(StateStore, ThreadId, Data, #save_opts{id = CpId}) of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 标记检查点已恢复
-spec mark_recovered(manager(), thread_id()) -> ok | {error, term()}.
mark_recovered(Mgr, ThreadId) ->
    case get_latest(Mgr, ThreadId) of
        {ok, {CpId, State, Meta}} ->
            NewMeta = Meta#checkpoint_meta{
                process_state = running,
                last_error = undefined,
                error_at = undefined
            },
            #checkpoint_manager{state_store = StateStore} = Mgr,
            Data = #{
                <<"state">> => State,
                <<"meta">> => checkpoint_meta_to_map(NewMeta),
                <<"created_at">> => erlang:system_time(millisecond)
            },
            case beamai_state_store:save(StateStore, ThreadId, Data, #save_opts{id = CpId}) of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 检查是否可以重试
-spec can_retry(manager(), thread_id()) -> boolean().
can_retry(Mgr, ThreadId) ->
    case get_latest(Mgr, ThreadId) of
        {ok, {_, _, Meta}} ->
            Meta#checkpoint_meta.recoverable andalso
            Meta#checkpoint_meta.retry_count < Meta#checkpoint_meta.max_retries;
        {error, _} ->
            false
    end.

%% @doc 获取恢复信息
-spec get_recovery_info(manager(), thread_id()) -> {ok, map()} | {error, not_found}.
get_recovery_info(Mgr, ThreadId) ->
    case get_latest(Mgr, ThreadId) of
        {ok, {CpId, _, Meta}} ->
            {ok, #{
                checkpoint_id => CpId,
                step_id => Meta#checkpoint_meta.step_id,
                process_state => Meta#checkpoint_meta.process_state,
                retry_count => Meta#checkpoint_meta.retry_count,
                max_retries => Meta#checkpoint_meta.max_retries,
                can_retry => can_retry(Mgr, ThreadId),
                last_error => Meta#checkpoint_meta.last_error,
                error_at => Meta#checkpoint_meta.error_at
            }};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 查询
%%====================================================================

%% @doc 列出检查点
-spec list(manager(), thread_id()) -> {ok, [#checkpoint{}]} | {error, term()}.
list(Mgr, ThreadId) ->
    list(Mgr, ThreadId, #{}).

%% @doc 列出检查点（带选项）
-spec list(manager(), thread_id(), map()) -> {ok, [#checkpoint{}]} | {error, term()}.
list(#checkpoint_manager{state_store = StateStore}, ThreadId, Opts) ->
    ListOpts = #list_opts_v2{
        limit = maps:get(limit, Opts, 100),
        offset = maps:get(offset, Opts, 0),
        order = maps:get(order, Opts, desc)
    },
    case beamai_state_store:list(StateStore, ThreadId, ListOpts) of
        {ok, States} ->
            Checkpoints = lists:filtermap(fun(#stored_state{id = Id, data = Data, created_at = CreatedAt}) ->
                case maps:get(<<"state">>, Data, undefined) of
                    undefined -> false;
                    State ->
                        MetaMap = maps:get(<<"meta">>, Data, #{}),
                        {true, #checkpoint{
                            id = Id,
                            thread_id = ThreadId,
                            state = State,
                            meta = map_to_checkpoint_meta(MetaMap),
                            created_at = CreatedAt
                        }}
                end
            end, States),
            {ok, Checkpoints};
        {error, _} = Error ->
            Error
    end.

%% @doc 统计检查点数量
-spec count(manager(), thread_id()) -> non_neg_integer().
count(#checkpoint_manager{state_store = StateStore}, ThreadId) ->
    beamai_state_store:count(StateStore, ThreadId).

%%====================================================================
%% 清理
%%====================================================================

%% @doc 清理旧检查点
-spec prune(manager(), thread_id(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
prune(#checkpoint_manager{state_store = StateStore}, ThreadId, KeepCount) ->
    case beamai_state_store:list(StateStore, ThreadId, #list_opts_v2{limit => 10000, order = desc}) of
        {ok, States} ->
            TotalCount = length(States),
            case TotalCount > KeepCount of
                true ->
                    ToDelete = lists:sublist(States, KeepCount + 1, TotalCount - KeepCount),
                    IdsToDelete = [S#stored_state.id || S <- ToDelete],
                    case beamai_state_store:batch_delete(StateStore, ThreadId, IdsToDelete) of
                        ok -> {ok, length(IdsToDelete)};
                        {error, _} = Error -> Error
                    end;
                false ->
                    {ok, 0}
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成检查点 ID
-spec generate_checkpoint_id() -> checkpoint_id().
generate_checkpoint_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("cp_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @private 可能需要清理
-spec maybe_prune(manager(), thread_id(), pos_integer()) -> ok.
maybe_prune(Mgr, ThreadId, MaxCount) ->
    CurrentCount = count(Mgr, ThreadId),
    case CurrentCount >= MaxCount of
        true ->
            prune(Mgr, ThreadId, MaxCount - 1),
            ok;
        false ->
            ok
    end.

%% @private 更新最新索引
-spec update_latest_index(beamai_state_store:state_store(), thread_id(), checkpoint_id()) -> ok.
update_latest_index(StateStore, ThreadId, CpId) ->
    IndexGroupId = <<ThreadId/binary, "_index">>,
    Data = #{<<"checkpoint_id">> => CpId, <<"updated_at">> => erlang:system_time(millisecond)},
    beamai_state_store:save(StateStore, IndexGroupId, Data, #save_opts{id = <<"_latest">>}),
    ok.

%% @private Meta 转 Map
-spec checkpoint_meta_to_map(#checkpoint_meta{}) -> map().
checkpoint_meta_to_map(Meta) ->
    #{
        <<"step_id">> => Meta#checkpoint_meta.step_id,
        <<"step_index">> => Meta#checkpoint_meta.step_index,
        <<"process_state">> => Meta#checkpoint_meta.process_state,
        <<"step_activations">> => Meta#checkpoint_meta.step_activations,
        <<"recoverable">> => Meta#checkpoint_meta.recoverable,
        <<"retry_count">> => Meta#checkpoint_meta.retry_count,
        <<"max_retries">> => Meta#checkpoint_meta.max_retries,
        <<"last_error">> => Meta#checkpoint_meta.last_error,
        <<"error_at">> => Meta#checkpoint_meta.error_at,
        <<"process_name">> => Meta#checkpoint_meta.process_name,
        <<"run_id">> => Meta#checkpoint_meta.run_id,
        <<"agent_id">> => Meta#checkpoint_meta.agent_id
    }.

%% @private Map 转 Meta
-spec map_to_checkpoint_meta(map()) -> #checkpoint_meta{}.
map_to_checkpoint_meta(Map) ->
    #checkpoint_meta{
        step_id = maps:get(<<"step_id">>, Map, undefined),
        step_index = maps:get(<<"step_index">>, Map, undefined),
        process_state = binary_to_existing_atom_safe(maps:get(<<"process_state">>, Map, <<"idle">>)),
        step_activations = maps:get(<<"step_activations">>, Map, #{}),
        recoverable = maps:get(<<"recoverable">>, Map, true),
        retry_count = maps:get(<<"retry_count">>, Map, 0),
        max_retries = maps:get(<<"max_retries">>, Map, 3),
        last_error = maps:get(<<"last_error">>, Map, undefined),
        error_at = maps:get(<<"error_at">>, Map, undefined),
        process_name = maps:get(<<"process_name">>, Map, undefined),
        run_id = maps:get(<<"run_id">>, Map, undefined),
        agent_id = maps:get(<<"agent_id">>, Map, undefined)
    }.

%% @private 安全的 binary 转 atom
-spec binary_to_existing_atom_safe(binary() | atom()) -> atom().
binary_to_existing_atom_safe(Bin) when is_binary(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> idle
    end;
binary_to_existing_atom_safe(Atom) when is_atom(Atom) ->
    Atom.
```

### 3.2 重构后的 Snapshot 模块

**文件**: `include/beamai_snapshot_v2.hrl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Snapshot V2 头文件 - 完整快照
%%%
%%% 基于 state_store 重构，专注于：
%%% - 分支管理
%%% - 时间旅行
%%% - 完整的血统追踪
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_SNAPSHOT_V2_HRL).
-define(BEAMAI_SNAPSHOT_V2_HRL, true).

%%====================================================================
%% Snapshot MetaInfo
%%====================================================================

%% Snapshot 元信息 - 专注于分支和历史
-record(snapshot_meta, {
    %%--------------------------------------------------------------------
    %% 血统信息
    %%--------------------------------------------------------------------

    %% 父快照 ID
    parent_id :: binary() | undefined,

    %% 分支 ID
    branch_id :: binary(),

    %% 从哪个快照分支出来的（用于合并）
    branched_from :: binary() | undefined,

    %%--------------------------------------------------------------------
    %% 快照类型
    %%--------------------------------------------------------------------

    %% 快照类型
    snapshot_type :: initial | step_completed | paused | completed | error | manual | branch,

    %%--------------------------------------------------------------------
    %% 执行上下文
    %%--------------------------------------------------------------------

    %% 流程名称
    process_name :: atom() | undefined,

    %% 执行 ID
    run_id :: binary() | undefined,

    %% Agent 信息
    agent_id :: binary() | undefined,
    agent_name :: binary() | undefined,

    %%--------------------------------------------------------------------
    %% 扩展信息
    %%--------------------------------------------------------------------

    %% 标签
    tags = [] :: [binary()],

    %% 描述
    description :: binary() | undefined,

    %% 自定义元数据
    custom = #{} :: map()
}).

%% Snapshot - 完整快照
-record(snapshot_v2, {
    %% 快照 ID
    id :: binary(),

    %% 线程 ID
    thread_id :: binary(),

    %% 状态数据（委托给 state_store）
    state :: map(),

    %% 元信息
    meta :: #snapshot_meta{},

    %% 时间戳
    created_at :: integer()
}).

%%====================================================================
%% 分支信息
%%====================================================================

-record(branch_info, {
    %% 分支 ID
    id :: binary(),

    %% 分支名称
    name :: binary(),

    %% 头快照 ID
    head_snapshot_id :: binary() | undefined,

    %% 快照数量
    snapshot_count = 0 :: non_neg_integer(),

    %% 创建时间
    created_at :: integer(),

    %% 最后更新时间
    updated_at :: integer()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间
-define(NS_SNAPSHOTS_V2, <<"snapshots_v2">>).
-define(NS_BRANCHES, <<"branches">>).

%% 默认分支
-define(DEFAULT_BRANCH, <<"main">>).

%% 默认最大快照数
-define(DEFAULT_MAX_SNAPSHOTS_V2, 100).

%% ID 前缀
-define(SNAPSHOT_ID_PREFIX, <<"sn_">>).

-endif.
```

**文件**: `src/snapshot/beamai_snapshot_v2.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Snapshot V2 模块 - 完整快照
%%%
%%% 基于 state_store 实现的完整快照系统，提供：
%%% - 分支管理
%%% - 时间旅行
%%% - 血统追踪
%%% - 快照比较
%%%
%%% == 与 Checkpoint 的区别 ==
%%%
%%% | 特性 | Checkpoint | Snapshot |
%%% |------|------------|----------|
%%% | 用途 | 流程恢复 | 完整历史 |
%%% | 分支 | 不支持 | 支持 |
%%% | 时间旅行 | 不支持 | 支持 |
%%% | 血统追踪 | 不支持 | 支持 |
%%% | 存储量 | 轻量 | 完整 |
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 snapshot 管理器
%%% SnMgr = beamai_snapshot_v2:new(StateStore, #{
%%%     max_snapshots => 100
%%% }),
%%%
%%% %% 保存快照
%%% Meta = #snapshot_meta{
%%%     snapshot_type = step_completed,
%%%     branch_id = <<"main">>
%%% },
%%% {ok, SnId} = beamai_snapshot_v2:save(SnMgr, <<"thread-1">>, State, Meta),
%%%
%%% %% 时间旅行
%%% {ok, Snapshot} = beamai_snapshot_v2:go_back(SnMgr, <<"thread-1">>, 3).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_snapshot_v2).

-include_lib("beamai_memory/include/beamai_snapshot_v2.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 核心操作
-export([
    save/4,
    load/3,
    load_latest/2,
    delete/3
]).

%% 时间旅行
-export([
    go_back/3,
    go_forward/3,
    goto/3,
    undo/2,
    redo/2
]).

%% 分支管理
-export([
    create_branch/4,
    switch_branch/3,
    list_branches/2,
    merge_branch/4,
    delete_branch/3
]).

%% 血统和历史
-export([
    get_lineage/3,
    get_history/2,
    diff/4
]).

%% 查询
-export([
    list/2, list/3,
    count/2,
    search/3
]).

%% 清理
-export([
    prune/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(snapshot_manager, {
    %% 底层状态存储
    state_store :: beamai_state_store:state_store(),

    %% 当前分支
    current_branch :: binary(),

    %% 分支缓存
    branches :: #{binary() => #branch_info{}},

    %% 最大快照数量（每线程）
    max_snapshots :: pos_integer(),

    %% 自动清理
    auto_prune :: boolean()
}).

-opaque manager() :: #snapshot_manager{}.
-type thread_id() :: binary().
-type snapshot_id() :: binary().
-type branch_id() :: binary().

-export_type([manager/0]).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 Snapshot 管理器
-spec new(beamai_state_store:state_store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

%% @doc 创建 Snapshot 管理器（带选项）
-spec new(beamai_state_store:state_store(), map()) -> manager().
new(StateStore, Opts) ->
    InitialBranch = maps:get(initial_branch, Opts, ?DEFAULT_BRANCH),

    #snapshot_manager{
        state_store = StateStore,
        current_branch = InitialBranch,
        branches = #{InitialBranch => #branch_info{
            id = InitialBranch,
            name = InitialBranch,
            snapshot_count = 0,
            created_at = erlang:system_time(millisecond),
            updated_at = erlang:system_time(millisecond)
        }},
        max_snapshots = maps:get(max_snapshots, Opts, ?DEFAULT_MAX_SNAPSHOTS_V2),
        auto_prune = maps:get(auto_prune, Opts, true)
    }.

%%====================================================================
%% 核心操作
%%====================================================================

%% @doc 保存快照
-spec save(manager(), thread_id(), map(), #snapshot_meta{}) ->
    {ok, snapshot_id(), manager()} | {error, term()}.
save(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch,
                       auto_prune = AutoPrune, max_snapshots = Max, branches = Branches} = Mgr,
     ThreadId, State, Meta0) ->

    %% 确保分支 ID
    Meta = case Meta0#snapshot_meta.branch_id of
        undefined -> Meta0#snapshot_meta{branch_id = CurrentBranch};
        _ -> Meta0
    end,

    %% 自动清理
    case AutoPrune of
        true -> maybe_prune(Mgr, ThreadId, Max);
        false -> ok
    end,

    %% 生成快照 ID
    SnId = generate_snapshot_id(),
    Now = erlang:system_time(millisecond),

    %% 获取父快照 ID（当前分支的 head）
    BranchId = Meta#snapshot_meta.branch_id,
    BranchInfo = maps:get(BranchId, Branches, #branch_info{
        id = BranchId,
        name = BranchId,
        created_at = Now,
        updated_at = Now
    }),
    ParentId = case Meta#snapshot_meta.parent_id of
        undefined -> BranchInfo#branch_info.head_snapshot_id;
        Pid -> Pid
    end,

    %% 更新 Meta 的 parent_id
    FinalMeta = Meta#snapshot_meta{parent_id = ParentId},

    %% 构建快照数据
    Data = #{
        <<"state">> => State,
        <<"meta">> => snapshot_meta_to_map(FinalMeta),
        <<"created_at">> => Now
    },

    %% 使用 state_store 保存
    GroupId = make_group_id(ThreadId, BranchId),
    SaveOpts = #save_opts{id = SnId},
    case beamai_state_store:save(StateStore, GroupId, Data, SaveOpts) of
        {ok, SnId} ->
            %% 更新分支信息
            NewBranchInfo = BranchInfo#branch_info{
                head_snapshot_id = SnId,
                snapshot_count = BranchInfo#branch_info.snapshot_count + 1,
                updated_at = Now
            },
            NewBranches = maps:put(BranchId, NewBranchInfo, Branches),
            NewMgr = Mgr#snapshot_manager{branches = NewBranches},

            %% 更新索引
            update_indexes(StateStore, ThreadId, BranchId, SnId),

            {ok, SnId, NewMgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载快照
-spec load(manager(), thread_id(), snapshot_id()) ->
    {ok, #snapshot_v2{}} | {error, not_found | term()}.
load(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
     ThreadId, SnId) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    case beamai_state_store:load(StateStore, GroupId, SnId) of
        {ok, #stored_state{data = Data}} ->
            {ok, data_to_snapshot(SnId, ThreadId, Data)};
        {error, not_found} ->
            %% 尝试在其他分支查找
            search_in_all_branches(StateStore, ThreadId, SnId);
        {error, _} = Error ->
            Error
    end.

%% @doc 加载最新快照
-spec load_latest(manager(), thread_id()) ->
    {ok, #snapshot_v2{}} | {error, not_found}.
load_latest(#snapshot_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
            ThreadId) ->

    BranchInfo = maps:get(CurrentBranch, Branches, undefined),
    case BranchInfo of
        undefined -> {error, not_found};
        #branch_info{head_snapshot_id = undefined} -> {error, not_found};
        #branch_info{head_snapshot_id = HeadId} -> load(Mgr, ThreadId, HeadId)
    end.

%% @doc 删除快照
-spec delete(manager(), thread_id(), snapshot_id()) -> ok | {error, term()}.
delete(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
       ThreadId, SnId) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    beamai_state_store:delete(StateStore, GroupId, SnId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 步
-spec go_back(manager(), thread_id(), pos_integer()) ->
    {ok, #snapshot_v2{}, manager()} | {error, term()}.
go_back(Mgr, ThreadId, Steps) ->
    case get_history(Mgr, ThreadId) of
        {ok, History} ->
            CurrentIndex = find_current_index(Mgr, History),
            TargetIndex = min(CurrentIndex + Steps, length(History) - 1),
            case lists:nth(TargetIndex + 1, History) of
                Snapshot ->
                    %% 更新分支 head
                    NewMgr = update_branch_head(Mgr, Snapshot#snapshot_v2.id),
                    {ok, Snapshot, NewMgr}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 步
-spec go_forward(manager(), thread_id(), pos_integer()) ->
    {ok, #snapshot_v2{}, manager()} | {error, term()}.
go_forward(Mgr, ThreadId, Steps) ->
    case get_history(Mgr, ThreadId) of
        {ok, History} ->
            CurrentIndex = find_current_index(Mgr, History),
            TargetIndex = max(CurrentIndex - Steps, 0),
            case lists:nth(TargetIndex + 1, History) of
                Snapshot ->
                    NewMgr = update_branch_head(Mgr, Snapshot#snapshot_v2.id),
                    {ok, Snapshot, NewMgr}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定快照
-spec goto(manager(), thread_id(), snapshot_id()) ->
    {ok, #snapshot_v2{}, manager()} | {error, term()}.
goto(Mgr, ThreadId, SnId) ->
    case load(Mgr, ThreadId, SnId) of
        {ok, Snapshot} ->
            NewMgr = update_branch_head(Mgr, SnId),
            {ok, Snapshot, NewMgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销（回退 1 步）
-spec undo(manager(), thread_id()) ->
    {ok, #snapshot_v2{}, manager()} | {error, term()}.
undo(Mgr, ThreadId) ->
    go_back(Mgr, ThreadId, 1).

%% @doc 重做（前进 1 步）
-spec redo(manager(), thread_id()) ->
    {ok, #snapshot_v2{}, manager()} | {error, term()}.
redo(Mgr, ThreadId) ->
    go_forward(Mgr, ThreadId, 1).

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 创建分支
-spec create_branch(manager(), thread_id(), binary(), map()) ->
    {ok, branch_id(), manager()} | {error, term()}.
create_branch(#snapshot_manager{state_store = StateStore, branches = Branches} = Mgr,
              ThreadId, BranchName, Opts) ->

    %% 检查分支是否已存在
    case maps:is_key(BranchName, Branches) of
        true ->
            {error, branch_exists};
        false ->
            %% 获取当前快照作为分支起点
            case load_latest(Mgr, ThreadId) of
                {ok, CurrentSnapshot} ->
                    Now = erlang:system_time(millisecond),

                    %% 创建新分支的初始快照
                    NewMeta = #snapshot_meta{
                        parent_id = CurrentSnapshot#snapshot_v2.id,
                        branch_id = BranchName,
                        branched_from = CurrentSnapshot#snapshot_v2.id,
                        snapshot_type = branch,
                        description = maps:get(description, Opts, undefined)
                    },

                    %% 创建分支信息
                    BranchInfo = #branch_info{
                        id = BranchName,
                        name = BranchName,
                        snapshot_count = 0,
                        created_at = Now,
                        updated_at = Now
                    },
                    NewBranches = maps:put(BranchName, BranchInfo, Branches),

                    %% 保存新分支的初始快照
                    TempMgr = Mgr#snapshot_manager{
                        branches = NewBranches,
                        current_branch = BranchName
                    },
                    case save(TempMgr, ThreadId, CurrentSnapshot#snapshot_v2.state, NewMeta) of
                        {ok, _, FinalMgr} ->
                            {ok, BranchName, FinalMgr};
                        {error, _} = Error ->
                            Error
                    end;
                {error, not_found} ->
                    %% 没有当前快照，创建空分支
                    Now = erlang:system_time(millisecond),
                    BranchInfo = #branch_info{
                        id = BranchName,
                        name = BranchName,
                        snapshot_count = 0,
                        created_at = Now,
                        updated_at = Now
                    },
                    NewBranches = maps:put(BranchName, BranchInfo, Branches),
                    NewMgr = Mgr#snapshot_manager{branches = NewBranches},
                    {ok, BranchName, NewMgr};
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc 切换分支
-spec switch_branch(manager(), thread_id(), branch_id()) ->
    {ok, #snapshot_v2{} | undefined, manager()} | {error, branch_not_found}.
switch_branch(#snapshot_manager{branches = Branches} = Mgr, ThreadId, BranchId) ->
    case maps:is_key(BranchId, Branches) of
        true ->
            NewMgr = Mgr#snapshot_manager{current_branch = BranchId},
            case load_latest(NewMgr, ThreadId) of
                {ok, Snapshot} -> {ok, Snapshot, NewMgr};
                {error, not_found} -> {ok, undefined, NewMgr}
            end;
        false ->
            {error, branch_not_found}
    end.

%% @doc 列出分支
-spec list_branches(manager(), thread_id()) -> {ok, [#branch_info{}]}.
list_branches(#snapshot_manager{branches = Branches}, _ThreadId) ->
    {ok, maps:values(Branches)}.

%% @doc 合并分支
-spec merge_branch(manager(), thread_id(), branch_id(), branch_id()) ->
    {ok, snapshot_id(), manager()} | {error, term()}.
merge_branch(#snapshot_manager{} = Mgr, ThreadId, SourceBranch, TargetBranch) ->
    %% 简单实现：将源分支的最新状态复制到目标分支
    TempMgr = Mgr#snapshot_manager{current_branch = SourceBranch},
    case load_latest(TempMgr, ThreadId) of
        {ok, SourceSnapshot} ->
            %% 切换到目标分支并保存
            TargetMgr = Mgr#snapshot_manager{current_branch = TargetBranch},
            Meta = #snapshot_meta{
                snapshot_type = branch,
                branch_id = TargetBranch,
                branched_from = SourceSnapshot#snapshot_v2.id,
                description = <<"Merged from ", SourceBranch/binary>>
            },
            save(TargetMgr, ThreadId, SourceSnapshot#snapshot_v2.state, Meta);
        {error, _} = Error ->
            Error
    end.

%% @doc 删除分支
-spec delete_branch(manager(), thread_id(), branch_id()) ->
    {ok, manager()} | {error, term()}.
delete_branch(#snapshot_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
              _ThreadId, BranchId) ->
    case BranchId of
        CurrentBranch ->
            {error, cannot_delete_current_branch};
        ?DEFAULT_BRANCH ->
            {error, cannot_delete_main_branch};
        _ ->
            case maps:is_key(BranchId, Branches) of
                true ->
                    NewBranches = maps:remove(BranchId, Branches),
                    {ok, Mgr#snapshot_manager{branches = NewBranches}};
                false ->
                    {error, branch_not_found}
            end
    end.

%%====================================================================
%% 血统和历史
%%====================================================================

%% @doc 获取血统链
-spec get_lineage(manager(), thread_id(), snapshot_id()) ->
    {ok, [#snapshot_v2{}]} | {error, term()}.
get_lineage(Mgr, ThreadId, SnId) ->
    get_lineage_recursive(Mgr, ThreadId, SnId, []).

%% @doc 获取历史
-spec get_history(manager(), thread_id()) ->
    {ok, [#snapshot_v2{}]} | {error, term()}.
get_history(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
            ThreadId) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    ListOpts = #list_opts_v2{limit => 1000, order = desc},
    case beamai_state_store:list(StateStore, GroupId, ListOpts) of
        {ok, States} ->
            Snapshots = [data_to_snapshot(S#stored_state.id, ThreadId, S#stored_state.data)
                         || S <- States],
            {ok, Snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 比较两个快照
-spec diff(manager(), thread_id(), snapshot_id(), snapshot_id()) ->
    {ok, map()} | {error, term()}.
diff(Mgr, ThreadId, SnId1, SnId2) ->
    case {load(Mgr, ThreadId, SnId1), load(Mgr, ThreadId, SnId2)} of
        {{ok, Sn1}, {ok, Sn2}} ->
            State1 = Sn1#snapshot_v2.state,
            State2 = Sn2#snapshot_v2.state,

            Keys1 = maps:keys(State1),
            Keys2 = maps:keys(State2),

            Added = [K || K <- Keys2, not maps:is_key(K, State1)],
            Removed = [K || K <- Keys1, not maps:is_key(K, State2)],
            Changed = [K || K <- Keys1,
                            maps:is_key(K, State2),
                            maps:get(K, State1) =/= maps:get(K, State2)],

            {ok, #{
                added => [{K, maps:get(K, State2)} || K <- Added],
                removed => [{K, maps:get(K, State1)} || K <- Removed],
                changed => [{K, #{old => maps:get(K, State1), new => maps:get(K, State2)}}
                            || K <- Changed],
                snapshot1 => #{id => Sn1#snapshot_v2.id, created_at => Sn1#snapshot_v2.created_at},
                snapshot2 => #{id => Sn2#snapshot_v2.id, created_at => Sn2#snapshot_v2.created_at}
            }};
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

%%====================================================================
%% 查询
%%====================================================================

%% @doc 列出快照
-spec list(manager(), thread_id()) -> {ok, [#snapshot_v2{}]} | {error, term()}.
list(Mgr, ThreadId) ->
    list(Mgr, ThreadId, #{}).

%% @doc 列出快照（带选项）
-spec list(manager(), thread_id(), map()) -> {ok, [#snapshot_v2{}]} | {error, term()}.
list(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
     ThreadId, Opts) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    ListOpts = #list_opts_v2{
        limit => maps:get(limit, Opts, 100),
        offset => maps:get(offset, Opts, 0),
        order => maps:get(order, Opts, desc)
    },
    case beamai_state_store:list(StateStore, GroupId, ListOpts) of
        {ok, States} ->
            Snapshots = [data_to_snapshot(S#stored_state.id, ThreadId, S#stored_state.data)
                         || S <- States],
            {ok, Snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 统计快照数量
-spec count(manager(), thread_id()) -> non_neg_integer().
count(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
      ThreadId) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    beamai_state_store:count(StateStore, GroupId).

%% @doc 搜索快照
-spec search(manager(), thread_id(), map()) -> {ok, [#snapshot_v2{}]} | {error, term()}.
search(Mgr, ThreadId, Query) ->
    %% 简单实现：按标签过滤
    case list(Mgr, ThreadId, #{limit => 1000}) of
        {ok, Snapshots} ->
            FilteredSnapshots = filter_snapshots(Snapshots, Query),
            {ok, FilteredSnapshots};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 清理
%%====================================================================

%% @doc 清理旧快照
-spec prune(manager(), thread_id(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
prune(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
      ThreadId, KeepCount) ->

    GroupId = make_group_id(ThreadId, CurrentBranch),
    case beamai_state_store:list(StateStore, GroupId, #list_opts_v2{limit => 10000, order = desc}) of
        {ok, States} ->
            TotalCount = length(States),
            case TotalCount > KeepCount of
                true ->
                    ToDelete = lists:sublist(States, KeepCount + 1, TotalCount - KeepCount),
                    IdsToDelete = [S#stored_state.id || S <- ToDelete],
                    case beamai_state_store:batch_delete(StateStore, GroupId, IdsToDelete) of
                        ok -> {ok, length(IdsToDelete)};
                        {error, _} = Error -> Error
                    end;
                false ->
                    {ok, 0}
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成快照 ID
-spec generate_snapshot_id() -> snapshot_id().
generate_snapshot_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("sn_~16.16.0b_~4.16.0b", [Ts, Rand])).

%% @private 构建分组 ID
-spec make_group_id(thread_id(), branch_id()) -> binary().
make_group_id(ThreadId, BranchId) ->
    <<ThreadId/binary, "_", BranchId/binary>>.

%% @private 可能需要清理
-spec maybe_prune(manager(), thread_id(), pos_integer()) -> ok.
maybe_prune(Mgr, ThreadId, MaxCount) ->
    CurrentCount = count(Mgr, ThreadId),
    case CurrentCount >= MaxCount of
        true ->
            prune(Mgr, ThreadId, MaxCount - 1),
            ok;
        false ->
            ok
    end.

%% @private 更新索引
-spec update_indexes(beamai_state_store:state_store(), thread_id(), branch_id(), snapshot_id()) -> ok.
update_indexes(StateStore, ThreadId, BranchId, SnId) ->
    IndexGroupId = <<ThreadId/binary, "_", BranchId/binary, "_index">>,
    Data = #{<<"snapshot_id">> => SnId, <<"updated_at">> => erlang:system_time(millisecond)},
    beamai_state_store:save(StateStore, IndexGroupId, Data, #save_opts{id = <<"_latest">>}),
    ok.

%% @private 更新分支 head
-spec update_branch_head(manager(), snapshot_id()) -> manager().
update_branch_head(#snapshot_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
                   SnId) ->

    BranchInfo = maps:get(CurrentBranch, Branches),
    NewBranchInfo = BranchInfo#branch_info{
        head_snapshot_id = SnId,
        updated_at = erlang:system_time(millisecond)
    },
    NewBranches = maps:put(CurrentBranch, NewBranchInfo, Branches),
    Mgr#snapshot_manager{branches = NewBranches}.

%% @private 查找当前索引
-spec find_current_index(manager(), [#snapshot_v2{}]) -> non_neg_integer().
find_current_index(#snapshot_manager{current_branch = CurrentBranch, branches = Branches},
                   History) ->

    BranchInfo = maps:get(CurrentBranch, Branches, undefined),
    case BranchInfo of
        undefined -> 0;
        #branch_info{head_snapshot_id = undefined} -> 0;
        #branch_info{head_snapshot_id = HeadId} ->
            find_index_by_id(History, HeadId, 0)
    end.

%% @private 按 ID 查找索引
-spec find_index_by_id([#snapshot_v2{}], snapshot_id(), non_neg_integer()) -> non_neg_integer().
find_index_by_id([], _SnId, _Index) ->
    0;
find_index_by_id([#snapshot_v2{id = SnId} | _], SnId, Index) ->
    Index;
find_index_by_id([_ | Rest], SnId, Index) ->
    find_index_by_id(Rest, SnId, Index + 1).

%% @private 递归获取血统
-spec get_lineage_recursive(manager(), thread_id(), snapshot_id(), [#snapshot_v2{}]) ->
    {ok, [#snapshot_v2{}]} | {error, term()}.
get_lineage_recursive(Mgr, ThreadId, SnId, Acc) ->
    case load(Mgr, ThreadId, SnId) of
        {ok, Snapshot} ->
            NewAcc = [Snapshot | Acc],
            case Snapshot#snapshot_v2.meta#snapshot_meta.parent_id of
                undefined ->
                    {ok, lists:reverse(NewAcc)};
                ParentId ->
                    get_lineage_recursive(Mgr, ThreadId, ParentId, NewAcc)
            end;
        {error, not_found} when Acc =/= [] ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

%% @private 在所有分支中搜索
-spec search_in_all_branches(beamai_state_store:state_store(), thread_id(), snapshot_id()) ->
    {ok, #snapshot_v2{}} | {error, not_found}.
search_in_all_branches(_StateStore, _ThreadId, _SnId) ->
    %% TODO: 实现跨分支搜索
    {error, not_found}.

%% @private 过滤快照
-spec filter_snapshots([#snapshot_v2{}], map()) -> [#snapshot_v2{}].
filter_snapshots(Snapshots, Query) ->
    Tags = maps:get(tags, Query, []),
    SnapshotType = maps:get(snapshot_type, Query, undefined),

    lists:filter(fun(#snapshot_v2{meta = Meta}) ->
        TagMatch = case Tags of
            [] -> true;
            _ -> lists:any(fun(T) -> lists:member(T, Meta#snapshot_meta.tags) end, Tags)
        end,
        TypeMatch = case SnapshotType of
            undefined -> true;
            _ -> Meta#snapshot_meta.snapshot_type =:= SnapshotType
        end,
        TagMatch andalso TypeMatch
    end, Snapshots).

%% @private Data 转 Snapshot
-spec data_to_snapshot(snapshot_id(), thread_id(), map()) -> #snapshot_v2{}.
data_to_snapshot(SnId, ThreadId, Data) ->
    #snapshot_v2{
        id = SnId,
        thread_id = ThreadId,
        state = maps:get(<<"state">>, Data, #{}),
        meta = map_to_snapshot_meta(maps:get(<<"meta">>, Data, #{})),
        created_at = maps:get(<<"created_at">>, Data, 0)
    }.

%% @private Meta 转 Map
-spec snapshot_meta_to_map(#snapshot_meta{}) -> map().
snapshot_meta_to_map(Meta) ->
    #{
        <<"parent_id">> => Meta#snapshot_meta.parent_id,
        <<"branch_id">> => Meta#snapshot_meta.branch_id,
        <<"branched_from">> => Meta#snapshot_meta.branched_from,
        <<"snapshot_type">> => Meta#snapshot_meta.snapshot_type,
        <<"process_name">> => Meta#snapshot_meta.process_name,
        <<"run_id">> => Meta#snapshot_meta.run_id,
        <<"agent_id">> => Meta#snapshot_meta.agent_id,
        <<"agent_name">> => Meta#snapshot_meta.agent_name,
        <<"tags">> => Meta#snapshot_meta.tags,
        <<"description">> => Meta#snapshot_meta.description,
        <<"custom">> => Meta#snapshot_meta.custom
    }.

%% @private Map 转 Meta
-spec map_to_snapshot_meta(map()) -> #snapshot_meta{}.
map_to_snapshot_meta(Map) ->
    #snapshot_meta{
        parent_id = maps:get(<<"parent_id">>, Map, undefined),
        branch_id = maps:get(<<"branch_id">>, Map, ?DEFAULT_BRANCH),
        branched_from = maps:get(<<"branched_from">>, Map, undefined),
        snapshot_type = binary_to_existing_atom_safe(maps:get(<<"snapshot_type">>, Map, <<"manual">>)),
        process_name = maps:get(<<"process_name">>, Map, undefined),
        run_id = maps:get(<<"run_id">>, Map, undefined),
        agent_id = maps:get(<<"agent_id">>, Map, undefined),
        agent_name = maps:get(<<"agent_name">>, Map, undefined),
        tags = maps:get(<<"tags">>, Map, []),
        description = maps:get(<<"description">>, Map, undefined),
        custom = maps:get(<<"custom">>, Map, #{})
    }.

%% @private 安全的 binary 转 atom
-spec binary_to_existing_atom_safe(binary() | atom()) -> atom().
binary_to_existing_atom_safe(Bin) when is_binary(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> manual
    end;
binary_to_existing_atom_safe(Atom) when is_atom(Atom) ->
    Atom.
```

## 4. 迁移策略

### 4.1 分阶段迁移

```
Phase 1: 创建新模块（并行运行）
├── 创建 beamai_state_store
├── 创建 beamai_checkpoint
├── 创建 beamai_snapshot_v2
└── 保持旧模块不变

Phase 2: 适配层
├── 在 beamai_memory API 层添加适配
├── 新功能使用新模块
└── 旧功能继续使用旧模块

Phase 3: 数据迁移
├── 提供迁移脚本
├── 将旧快照迁移到新格式
└── 验证数据完整性

Phase 4: 清理
├── 废弃旧模块
├── 更新所有调用方
└── 删除旧代码
```

### 4.2 API 兼容层

```erlang
%% 在 beamai_memory.erl 中添加兼容层

%% 旧 API -> 新实现
save_snapshot(Manager, Snapshot, Config) ->
    %% 转换为新格式
    State = Snapshot#snapshot.values,
    Meta = #snapshot_meta{
        parent_id = Snapshot#snapshot.parent_id,
        branch_id = ?DEFAULT_BRANCH,
        snapshot_type = step_completed
    },
    ThreadId = Snapshot#snapshot.thread_id,

    %% 使用新实现
    case beamai_snapshot_v2:save(get_snapshot_mgr(Manager), ThreadId, State, Meta) of
        {ok, SnId, _NewMgr} ->
            {ok, SnId};
        {error, _} = Error ->
            Error
    end.
```

## 5. 总结

### 5.1 分层职责

| 层级 | 模块 | 职责 |
|------|------|------|
| **Layer 1** | beamai_state_store | 纯粹的状态存储/恢复，无业务语义 |
| **Layer 2** | beamai_checkpoint | 轻量级检查点，专注流程恢复 |
| **Layer 2** | beamai_snapshot_v2 | 完整快照，支持分支和时间旅行 |

### 5.2 选择指南

| 场景 | 推荐模块 | 原因 |
|------|---------|------|
| 步骤执行失败恢复 | checkpoint | 轻量、快速、专注恢复 |
| 流程暂停/继续 | checkpoint | 简单的状态保存 |
| 完整历史记录 | snapshot_v2 | 支持血统追踪 |
| 多分支实验 | snapshot_v2 | 支持分支管理 |
| 回溯调试 | snapshot_v2 | 支持时间旅行 |
| 自定义存储需求 | state_store | 可直接使用基础层 |

### 5.3 文件清单

```
apps/beamai_memory/
├── include/
│   ├── beamai_state_store.hrl      # 新增
│   ├── beamai_checkpoint.hrl       # 新增
│   ├── beamai_snapshot_v2.hrl      # 新增
│   └── beamai_snapshot.hrl         # 保留（兼容）
├── src/
│   ├── store/
│   │   ├── beamai_state_store.erl  # 新增
│   │   ├── beamai_store.erl        # 保留
│   │   └── ...
│   ├── checkpoint/
│   │   └── beamai_checkpoint.erl   # 新增
│   └── snapshot/
│       ├── beamai_snapshot_v2.erl  # 新增
│       └── beamai_snapshot_manager.erl  # 保留（兼容）
└── docs/
    └── design/
        └── storage_layer_refactoring.md  # 本文档
```
