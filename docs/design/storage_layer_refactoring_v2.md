# beamai_memory 存储分层重构方案 V2

## 1. 核心概念澄清

### 1.1 Snapshot vs Checkpoint 的本质区别

| 维度 | Snapshot (Process) | Checkpoint (Graph) |
|------|-------------------|-------------------|
| **服务框架** | Process Framework | Graph Engine (Pregel) |
| **执行模型** | gen_statem 事件驱动状态机 | BSP 批同步并行计算 |
| **时间单位** | 事件 (event) | 超步 (superstep) |
| **核心单元** | Step (事件触发激活) | Node/Vertex (超步计算) |
| **并发模型** | poolboy 工作池 | Pregel 超步同步 |

### 1.2 两者都支持时间旅行

**关键认识**：两者都形成线性时间线，都可以支持时间旅行。

```
Process Framework (事件驱动时间线):
═══════════════════════════════════════════════════════════════════════
event_1 ───→ event_2 ───→ event_3 ───→ event_4 ───→ event_5
    │            │            │            │            │
   sn_1         sn_2         sn_3         sn_4         sn_5
    ↑                         ↑
    └───── go_back(2) ────────┘

Graph Engine (超步驱动时间线):
═══════════════════════════════════════════════════════════════════════
superstep_0 ───→ superstep_1 ───→ superstep_2 ───→ superstep_3
      │               │               │               │
     cp_0            cp_1            cp_2            cp_3
      ↑                               ↑
      └────── go_back(2) ─────────────┘
```

### 1.3 时间旅行语义对比

| 操作 | Snapshot (Process) | Checkpoint (Graph) |
|------|-------------------|-------------------|
| **go_back(N)** | 回退 N 个事件处理点 | 回退 N 个超步 |
| **go_forward(N)** | 前进 N 个事件处理点 | 前进 N 个超步 |
| **goto(id)** | 跳转到某个快照 | 跳转到某个检查点 |
| **branch/fork** | 从快照创建分支，可触发不同事件 | 从检查点分叉，可注入不同 resume_data |
| **回退后重执行** | 事件队列可能产生不同路径 | 条件边 + 不同输入可产生不同路径 |

### 1.4 状态内容对比

**Process Snapshot 状态内容**:
```erlang
#{
    process_spec := ProcessSpec,      %% 流程定义
    current_state := atom(),          %% 状态机状态: idle|running|paused|completed|failed
    steps_state := #{                 %% 各步骤的运行时状态
        step_id => #{
            state := term(),          %% 步骤内部状态
            collected_inputs := #{},  %% 已收集的输入
            activation_count := N     %% 激活次数
        }
    },
    event_queue := [Event],           %% 待处理事件队列
    paused_step := atom() | undefined,%% 暂停的步骤
    pause_reason := term()            %% 暂停原因
}
```

**Graph Checkpoint 状态内容**:
```erlang
#{
    pregel_snapshot := #{
        superstep := N,               %% 当前超步
        vertices := #{                %% 顶点状态
            vertex_id => #{
                value := term(),      %% 顶点值(包含节点函数和状态)
                active := boolean(),  %% 是否活跃
                messages := [msg()]   %% 待处理消息
            }
        },
        pending_activations := [id]   %% 待激活顶点
    },
    global_state := State,            %% 全局共享状态
    iteration := N,                   %% 执行迭代次数
    run_id := binary(),               %% 执行ID
    active_vertices := [id],          %% 活跃顶点列表
    completed_vertices := [id],       %% 已完成顶点列表
    type := initial|step|error|interrupt|final  %% 检查点类型
}
```

## 2. 分层架构

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         beamai_memory (API)                             │
│              save_snapshot/3, save_checkpoint/3, restore/3              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                  Layer 2: Domain Layer (领域层)                  │   │
│   │                                                                 │   │
│   │  ┌─────────────────────────┐    ┌─────────────────────────────┐ │   │
│   │  │   beamai_snapshot       │    │    beamai_checkpoint        │ │   │
│   │  │   (Process Framework)   │    │    (Graph Engine)           │ │   │
│   │  │                         │    │                             │ │   │
│   │  │ 时间单位: 事件          │    │ 时间单位: 超步              │ │   │
│   │  │                         │    │                             │ │   │
│   │  │ 核心状态:               │    │ 核心状态:                   │ │   │
│   │  │ • steps_state           │    │ • superstep                 │ │   │
│   │  │ • event_queue           │    │ • vertices                  │ │   │
│   │  │ • fsm_state             │    │ • pending_activations       │ │   │
│   │  │ • paused_step           │    │ • global_state              │ │   │
│   │  │                         │    │                             │ │   │
│   │  │ 共同能力:               │    │ 共同能力:                   │ │   │
│   │  │ • 时间旅行              │    │ • 时间旅行                  │ │   │
│   │  │ • 分支/分叉             │    │ • 分支/分叉                 │ │   │
│   │  │ • 血统追踪              │    │ • 执行路径追踪              │ │   │
│   │  │                         │    │                             │ │   │
│   │  │ 特有能力:               │    │ 特有能力:                   │ │   │
│   │  │ • 事件队列回放          │    │ • 顶点重试 (retry)          │ │   │
│   │  │ • 步骤激活恢复          │    │ • resume_data 注入          │ │   │
│   │  └──────────┬──────────────┘    └──────────────┬──────────────┘ │   │
│   │             │                                  │                │   │
│   │             └──────────────┬───────────────────┘                │   │
│   │                            │                                    │   │
│   └────────────────────────────┼────────────────────────────────────┘   │
│                                │                                        │
│   ┌────────────────────────────┼────────────────────────────────────┐   │
│   │            Layer 1: State Store (通用状态存储层)                 │   │
│   │                            │                                    │   │
│   │                 ┌──────────▼──────────┐                         │   │
│   │                 │  beamai_state_store │                         │   │
│   │                 │                     │                         │   │
│   │                 │  纯粹的 CRUD:       │                         │   │
│   │                 │  • save/3, load/3   │                         │   │
│   │                 │  • delete/3         │                         │   │
│   │                 │  • list/2           │                         │   │
│   │                 │  • batch_*/3        │                         │   │
│   │                 │                     │                         │   │
│   │                 │  无业务语义         │                         │   │
│   │                 └──────────┬──────────┘                         │   │
│   │                            │                                    │   │
│   └────────────────────────────┼────────────────────────────────────┘   │
│                                │                                        │
├────────────────────────────────┼────────────────────────────────────────┤
│                     Backend Layer (后端层)                              │
│                                │                                        │
│            ┌───────────────────┴───────────────────┐                    │
│            │                                       │                    │
│   ┌────────▼────────┐                   ┌──────────▼──────────┐         │
│   │ beamai_store_ets│                   │ beamai_store_sqlite │         │
│   │    (内存后端)    │                   │    (持久化后端)      │         │
│   └─────────────────┘                   └─────────────────────┘         │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## 3. Layer 1: 通用状态存储层

### 3.1 头文件

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

    %% 所属的逻辑分组（如 thread_id 或 run_id）
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

-define(NS_STATES, <<"states">>).
-define(ERR_VERSION_CONFLICT, version_conflict).
-define(ERR_NOT_FOUND, not_found).

-endif.
```

### 3.2 模块实现

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
    load/3,
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
    store :: beamai_store:store(),
    namespace_prefix :: [binary()],
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

-spec new(beamai_store:store()) -> state_store().
new(Store) ->
    new(Store, #{}).

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

-spec save(state_store(), group_id(), state_data()) ->
    {ok, state_id()} | {error, term()}.
save(StateStore, GroupId, Data) ->
    save(StateStore, GroupId, Data, #save_opts{}).

-spec save(state_store(), group_id(), state_data(), #save_opts{}) ->
    {ok, state_id()} | {error, term()}.
save(#state_store{store = Store, namespace_prefix = Prefix, id_generator = IdGen},
     GroupId, Data, #save_opts{id = CustomId}) ->

    StateId = case CustomId of
        undefined -> IdGen();
        _ -> CustomId
    end,

    Namespace = Prefix ++ [GroupId],
    Now = erlang:system_time(millisecond),

    Value = #{
        <<"id">> => StateId,
        <<"group_id">> => GroupId,
        <<"data">> => Data,
        <<"version">> => 1,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },

    case beamai_store:put(Store, Namespace, StateId, Value) of
        ok -> {ok, StateId};
        {error, _} = Error -> Error
    end.

-spec load(state_store(), group_id(), state_id()) ->
    {ok, #stored_state{}} | {error, not_found | term()}.
load(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateId) ->
    Namespace = Prefix ++ [GroupId],
    case beamai_store:get(Store, Namespace, StateId) of
        {ok, #store_item{value = Value}} ->
            {ok, map_to_stored_state(Value)};
        {error, not_found} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

-spec delete(state_store(), group_id(), state_id()) -> ok | {error, term()}.
delete(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateId) ->
    Namespace = Prefix ++ [GroupId],
    beamai_store:delete(Store, Namespace, StateId).

-spec exists(state_store(), group_id(), state_id()) -> boolean().
exists(#state_store{store = Store, namespace_prefix = Prefix}, GroupId, StateId) ->
    Namespace = Prefix ++ [GroupId],
    case beamai_store:get(Store, Namespace, StateId) of
        {ok, _} -> true;
        _ -> false
    end.

%%====================================================================
%% 列表和查询
%%====================================================================

-spec list(state_store(), group_id()) -> {ok, [#stored_state{}]} | {error, term()}.
list(StateStore, GroupId) ->
    list(StateStore, GroupId, #list_opts_v2{}).

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
            Sorted = case Order of
                desc -> lists:sort(fun(A, B) -> A#stored_state.updated_at > B#stored_state.updated_at end, States);
                asc -> lists:sort(fun(A, B) -> A#stored_state.updated_at < B#stored_state.updated_at end, States)
            end,
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

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

-spec batch_save(state_store(), group_id(), [{state_id() | undefined, state_data()}]) ->
    {ok, [state_id()]} | {error, term()}.
batch_save(#state_store{store = Store, namespace_prefix = Prefix, id_generator = IdGen},
           GroupId, Items) ->

    Namespace = Prefix ++ [GroupId],
    Now = erlang:system_time(millisecond),

    {Ops, Ids} = lists:foldl(fun({MaybeId, Data}, {AccOps, AccIds}) ->
        StateId = case MaybeId of
            undefined -> IdGen();
            _ -> MaybeId
        end,
        Value = #{
            <<"id">> => StateId,
            <<"group_id">> => GroupId,
            <<"data">> => Data,
            <<"version">> => 1,
            <<"created_at">> => Now,
            <<"updated_at">> => Now
        },
        Op = {put, Namespace, StateId, Value},
        {[Op | AccOps], [StateId | AccIds]}
    end, {[], []}, Items),

    case beamai_store:batch(Store, lists:reverse(Ops)) of
        {ok, _} -> {ok, lists:reverse(Ids)};
        {error, _} = Error -> Error
    end.

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
                    _ ->
                        {StateId, not_found}
                end
            end, StateIds, Results),
            {ok, Pairs};
        {error, _} = Error ->
            Error
    end.

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

-spec generate_id() -> state_id().
generate_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("st_~16.16.0b_~4.16.0b", [Ts, Rand])).

-spec get_store(state_store()) -> beamai_store:store().
get_store(#state_store{store = Store}) ->
    Store.

%%====================================================================
%% 内部函数
%%====================================================================

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

## 4. Layer 2: Snapshot (Process Framework)

### 4.1 头文件

**文件**: `include/beamai_snapshot.hrl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Snapshot 头文件 - Process Framework 专用
%%%
%%% 面向 Process Framework 的状态快照，记录：
%%% - 状态机状态 (gen_statem)
%%% - 步骤运行时状态
%%% - 事件队列
%%% - 暂停信息
%%%
%%% 支持时间旅行：go_back, go_forward, goto, branch
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_SNAPSHOT_HRL).
-define(BEAMAI_SNAPSHOT_HRL, true).

%%====================================================================
%% Snapshot 核心记录
%%====================================================================

%% Process Snapshot - 面向 Process Framework
-record(process_snapshot, {
    %% 唯一标识
    id :: binary(),

    %% 线程标识
    thread_id :: binary(),

    %%--------------------------------------------------------------------
    %% Process Framework 特有状态
    %%--------------------------------------------------------------------

    %% 流程定义引用（或完整定义）
    process_spec :: map() | atom(),

    %% 状态机当前状态
    fsm_state :: idle | running | paused | completed | failed,

    %% 各步骤的运行时状态
    steps_state :: #{atom() => step_snapshot()},

    %% 待处理的事件队列
    event_queue :: [map()],

    %% 暂停相关
    paused_step :: atom() | undefined,
    pause_reason :: term() | undefined,

    %%--------------------------------------------------------------------
    %% 时间旅行元信息
    %%--------------------------------------------------------------------

    %% 父快照（用于分支和回溯）
    parent_id :: binary() | undefined,

    %% 分支标识
    branch_id :: binary(),

    %% 版本号（用于时间旅行定位）
    version :: non_neg_integer(),

    %%--------------------------------------------------------------------
    %% 快照类型和执行上下文
    %%--------------------------------------------------------------------

    %% 快照类型
    snapshot_type :: initial | step_completed | paused | completed | error | manual | branch,

    %% 执行标识
    run_id :: binary() | undefined,
    agent_id :: binary() | undefined,
    agent_name :: binary() | undefined,

    %% 时间戳
    created_at :: integer(),

    %% 自定义元数据
    metadata :: map()
}).

%% 步骤快照
-type step_snapshot() :: #{
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%%====================================================================
%% 分支信息
%%====================================================================

-record(snapshot_branch, {
    id :: binary(),
    name :: binary(),
    head_snapshot_id :: binary() | undefined,
    snapshot_count :: non_neg_integer(),
    parent_branch_id :: binary() | undefined,
    forked_from_snapshot_id :: binary() | undefined,
    created_at :: integer()
}).

%%====================================================================
%% 常量
%%====================================================================

-define(NS_PROCESS_SNAPSHOTS, <<"process_snapshots">>).
-define(SNAPSHOT_ID_PREFIX, <<"psn_">>).
-define(DEFAULT_MAX_SNAPSHOTS, 100).
-define(DEFAULT_BRANCH, <<"main">>).

-endif.
```

### 4.2 模块实现

**文件**: `src/snapshot/beamai_snapshot.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Snapshot 模块 - Process Framework 专用
%%%
%%% 为 Process Framework 提供状态快照功能：
%%% - 保存/恢复流程状态
%%% - 时间旅行（回退/前进）
%%% - 分支管理
%%% - 血统追踪
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
-module(beamai_snapshot).

-include_lib("beamai_memory/include/beamai_snapshot.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 核心操作 - Process Runtime 集成
-export([
    save_from_runtime/4,
    restore_to_runtime/3,
    restore_to_runtime/4
]).

%% 通用操作
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
    redo/2,
    get_current_position/2
]).

%% 分支管理
-export([
    create_branch/4,
    switch_branch/3,
    list_branches/2,
    merge_branch/4,
    delete_branch/3,
    fork_from/4
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
    count/2
]).

%% 清理
-export([
    prune/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(snapshot_manager, {
    state_store :: beamai_state_store:state_store(),
    current_branch :: binary(),
    branches :: #{binary() => #snapshot_branch{}},
    %% 当前位置（用于时间旅行）
    current_position :: #{binary() => non_neg_integer()},  %% thread_id => version
    max_snapshots :: pos_integer(),
    auto_prune :: boolean()
}).

-opaque manager() :: #snapshot_manager{}.
-type thread_id() :: binary().
-type snapshot_id() :: binary().

-export_type([manager/0]).

%%====================================================================
%% 构造函数
%%====================================================================

-spec new(beamai_state_store:state_store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

-spec new(beamai_state_store:state_store(), map()) -> manager().
new(StateStore, Opts) ->
    InitialBranch = maps:get(initial_branch, Opts, ?DEFAULT_BRANCH),
    #snapshot_manager{
        state_store = StateStore,
        current_branch = InitialBranch,
        branches = #{InitialBranch => #snapshot_branch{
            id = InitialBranch,
            name = InitialBranch,
            snapshot_count = 0,
            created_at = erlang:system_time(millisecond)
        }},
        current_position = #{},
        max_snapshots = maps:get(max_snapshots, Opts, ?DEFAULT_MAX_SNAPSHOTS),
        auto_prune = maps:get(auto_prune, Opts, true)
    }.

%%====================================================================
%% Process Runtime 集成
%%====================================================================

%% @doc 从 Process 运行时状态创建快照
-spec save_from_runtime(manager(), thread_id(), map(), map()) ->
    {ok, snapshot_id(), manager()} | {error, term()}.
save_from_runtime(Mgr, ThreadId, RuntimeState, Opts) ->
    %% 获取当前序列号
    CurrentVersion = get_thread_version(Mgr, ThreadId),
    NewVersion = CurrentVersion + 1,

    Snapshot = #process_snapshot{
        id = undefined,
        thread_id = ThreadId,
        process_spec = maps:get(process_spec, RuntimeState),
        fsm_state = maps:get(current_state, RuntimeState, idle),
        steps_state = snapshot_steps_state(maps:get(steps_state, RuntimeState, #{})),
        event_queue = maps:get(event_queue, RuntimeState, []),
        paused_step = maps:get(paused_step, RuntimeState),
        pause_reason = maps:get(pause_reason, RuntimeState),
        parent_id = undefined,
        branch_id = Mgr#snapshot_manager.current_branch,
        version = NewVersion,
        snapshot_type = maps:get(snapshot_type, Opts, manual),
        run_id = maps:get(run_id, Opts),
        agent_id = maps:get(agent_id, Opts),
        agent_name = maps:get(agent_name, Opts),
        created_at = erlang:system_time(millisecond),
        metadata = maps:get(metadata, Opts, #{})
    },

    save(Mgr, ThreadId, Snapshot, Opts).

%% @doc 从快照恢复为 Process 运行时状态
-spec restore_to_runtime(manager(), thread_id(), snapshot_id()) ->
    {ok, map()} | {error, term()}.
restore_to_runtime(Mgr, ThreadId, SnapshotId) ->
    restore_to_runtime(Mgr, ThreadId, SnapshotId, #{}).

-spec restore_to_runtime(manager(), thread_id(), snapshot_id(), map()) ->
    {ok, map()} | {error, term()}.
restore_to_runtime(Mgr, ThreadId, SnapshotId, _Opts) ->
    case load(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot} ->
            RuntimeState = #{
                process_spec => Snapshot#process_snapshot.process_spec,
                current_state => Snapshot#process_snapshot.fsm_state,
                steps_state => restore_steps_state(
                    Snapshot#process_snapshot.steps_state,
                    maps:get(steps, Snapshot#process_snapshot.process_spec, #{})
                ),
                event_queue => Snapshot#process_snapshot.event_queue,
                paused_step => Snapshot#process_snapshot.paused_step,
                pause_reason => Snapshot#process_snapshot.pause_reason
            },
            {ok, RuntimeState};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 通用操作
%%====================================================================

-spec save(manager(), thread_id(), #process_snapshot{}, map()) ->
    {ok, snapshot_id(), manager()} | {error, term()}.
save(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch,
                       branches = Branches, current_position = Positions,
                       auto_prune = AutoPrune, max_snapshots = MaxSnapshots} = Mgr,
     ThreadId, Snapshot0, _Opts) ->

    case AutoPrune of
        true -> maybe_prune(Mgr, ThreadId, MaxSnapshots);
        false -> ok
    end,

    SnapshotId = generate_snapshot_id(),
    Now = erlang:system_time(millisecond),

    BranchInfo = maps:get(CurrentBranch, Branches, #snapshot_branch{
        id = CurrentBranch, name = CurrentBranch, snapshot_count = 0, created_at = Now
    }),
    ParentId = BranchInfo#snapshot_branch.head_snapshot_id,

    %% 确定序列号
    Version = case Snapshot0#process_snapshot.version of
        undefined -> get_thread_version(Mgr, ThreadId) + 1;
        N -> N
    end,

    Snapshot = Snapshot0#process_snapshot{
        id = SnapshotId,
        thread_id = ThreadId,
        parent_id = ParentId,
        branch_id = CurrentBranch,
        version = Version,
        created_at = Now
    },

    Data = process_snapshot_to_map(Snapshot),
    GroupId = make_group_id(ThreadId, CurrentBranch),
    SaveOpts = #save_opts{id = SnapshotId},

    case beamai_state_store:save(StateStore, GroupId, Data, SaveOpts) of
        {ok, SnapshotId} ->
            NewBranchInfo = BranchInfo#snapshot_branch{
                head_snapshot_id = SnapshotId,
                snapshot_count = BranchInfo#snapshot_branch.snapshot_count + 1
            },
            NewBranches = maps:put(CurrentBranch, NewBranchInfo, Branches),
            NewPositions = maps:put(ThreadId, Version, Positions),
            NewMgr = Mgr#snapshot_manager{
                branches = NewBranches,
                current_position = NewPositions
            },
            {ok, SnapshotId, NewMgr};
        {error, _} = Error ->
            Error
    end.

-spec load(manager(), thread_id(), snapshot_id()) ->
    {ok, #process_snapshot{}} | {error, not_found | term()}.
load(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
     ThreadId, SnapshotId) ->
    GroupId = make_group_id(ThreadId, CurrentBranch),
    case beamai_state_store:load(StateStore, GroupId, SnapshotId) of
        {ok, #stored_state{data = Data}} ->
            {ok, map_to_process_snapshot(Data)};
        {error, _} = Error ->
            Error
    end.

-spec load_latest(manager(), thread_id()) ->
    {ok, #process_snapshot{}} | {error, not_found}.
load_latest(#snapshot_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
            ThreadId) ->
    case maps:get(CurrentBranch, Branches, undefined) of
        undefined -> {error, not_found};
        #snapshot_branch{head_snapshot_id = undefined} -> {error, not_found};
        #snapshot_branch{head_snapshot_id = HeadId} -> load(Mgr, ThreadId, HeadId)
    end.

-spec delete(manager(), thread_id(), snapshot_id()) -> ok | {error, term()}.
delete(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
       ThreadId, SnapshotId) ->
    GroupId = make_group_id(ThreadId, CurrentBranch),
    beamai_state_store:delete(StateStore, GroupId, SnapshotId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个快照
%%
%% 在事件时间线上回退 N 步。
-spec go_back(manager(), thread_id(), pos_integer()) ->
    {ok, #process_snapshot{}, manager()} | {error, term()}.
go_back(Mgr, ThreadId, Steps) ->
    case get_history(Mgr, ThreadId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, ThreadId, History),
            TargetPos = max(0, CurrentPos - Steps),
            move_to_position(Mgr, ThreadId, History, TargetPos);
        {ok, []} ->
            {error, no_snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 个快照
%%
%% 在事件时间线上前进 N 步（仅在回退后有效）。
-spec go_forward(manager(), thread_id(), pos_integer()) ->
    {ok, #process_snapshot{}, manager()} | {error, term()}.
go_forward(Mgr, ThreadId, Steps) ->
    case get_history(Mgr, ThreadId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, ThreadId, History),
            MaxPos = length(History) - 1,
            TargetPos = min(MaxPos, CurrentPos + Steps),
            move_to_position(Mgr, ThreadId, History, TargetPos);
        {ok, []} ->
            {error, no_snapshots};
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定快照
-spec goto(manager(), thread_id(), snapshot_id()) ->
    {ok, #process_snapshot{}, manager()} | {error, term()}.
goto(Mgr, ThreadId, SnapshotId) ->
    case load(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot} ->
            NewPositions = maps:put(ThreadId, Snapshot#process_snapshot.version,
                                    Mgr#snapshot_manager.current_position),
            NewMgr = Mgr#snapshot_manager{current_position = NewPositions},
            {ok, Snapshot, NewMgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销（回退 1 步）
-spec undo(manager(), thread_id()) ->
    {ok, #process_snapshot{}, manager()} | {error, term()}.
undo(Mgr, ThreadId) ->
    go_back(Mgr, ThreadId, 1).

%% @doc 重做（前进 1 步）
-spec redo(manager(), thread_id()) ->
    {ok, #process_snapshot{}, manager()} | {error, term()}.
redo(Mgr, ThreadId) ->
    go_forward(Mgr, ThreadId, 1).

%% @doc 获取当前在时间线中的位置
-spec get_current_position(manager(), thread_id()) ->
    {ok, #{current := non_neg_integer(), total := non_neg_integer(), snapshot_id := binary()}} |
    {error, term()}.
get_current_position(Mgr, ThreadId) ->
    case get_history(Mgr, ThreadId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, ThreadId, History),
            Snapshot = lists:nth(CurrentPos + 1, History),
            {ok, #{
                current => CurrentPos,
                total => length(History),
                snapshot_id => Snapshot#process_snapshot.id
            }};
        {ok, []} ->
            {error, no_snapshots};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分支管理
%%====================================================================

%% @doc 创建分支
-spec create_branch(manager(), thread_id(), binary(), map()) ->
    {ok, binary(), manager()} | {error, term()}.
create_branch(#snapshot_manager{branches = Branches, current_branch = CurrentBranch} = Mgr,
              ThreadId, BranchName, Opts) ->
    case maps:is_key(BranchName, Branches) of
        true ->
            {error, branch_exists};
        false ->
            Now = erlang:system_time(millisecond),
            CurrentBranchInfo = maps:get(CurrentBranch, Branches),

            NewBranch = #snapshot_branch{
                id = BranchName,
                name = BranchName,
                head_snapshot_id = CurrentBranchInfo#snapshot_branch.head_snapshot_id,
                snapshot_count = 0,
                parent_branch_id = CurrentBranch,
                forked_from_snapshot_id = CurrentBranchInfo#snapshot_branch.head_snapshot_id,
                created_at = Now
            },

            NewBranches = maps:put(BranchName, NewBranch, Branches),
            NewMgr = Mgr#snapshot_manager{branches = NewBranches},
            {ok, BranchName, NewMgr}
    end.

%% @doc 从指定快照分叉创建新执行
%%
%% 用于从历史中的某个点开始新的执行路径。
-spec fork_from(manager(), thread_id(), snapshot_id(), map()) ->
    {ok, binary(), manager()} | {error, term()}.
fork_from(Mgr, ThreadId, SnapshotId, Opts) ->
    case load(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot} ->
            ForkName = maps:get(branch_name, Opts,
                generate_fork_name(Snapshot#process_snapshot.branch_id)),
            Now = erlang:system_time(millisecond),

            NewBranch = #snapshot_branch{
                id = ForkName,
                name = ForkName,
                head_snapshot_id = SnapshotId,
                snapshot_count = 0,
                parent_branch_id = Snapshot#process_snapshot.branch_id,
                forked_from_snapshot_id = SnapshotId,
                created_at = Now
            },

            NewBranches = maps:put(ForkName, NewBranch, Mgr#snapshot_manager.branches),
            NewMgr = Mgr#snapshot_manager{
                branches = NewBranches,
                current_branch = ForkName
            },
            {ok, ForkName, NewMgr};
        {error, _} = Error ->
            Error
    end.

-spec switch_branch(manager(), thread_id(), binary()) ->
    {ok, #process_snapshot{} | undefined, manager()} | {error, branch_not_found}.
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

-spec list_branches(manager(), thread_id()) -> {ok, [#snapshot_branch{}]}.
list_branches(#snapshot_manager{branches = Branches}, _ThreadId) ->
    {ok, maps:values(Branches)}.

-spec merge_branch(manager(), thread_id(), binary(), binary()) ->
    {ok, snapshot_id(), manager()} | {error, term()}.
merge_branch(Mgr, ThreadId, SourceBranch, TargetBranch) ->
    TempMgr = Mgr#snapshot_manager{current_branch = SourceBranch},
    case load_latest(TempMgr, ThreadId) of
        {ok, SourceSnapshot} ->
            TargetMgr = Mgr#snapshot_manager{current_branch = TargetBranch},
            MergeSnapshot = SourceSnapshot#process_snapshot{
                snapshot_type = branch,
                metadata = (SourceSnapshot#process_snapshot.metadata)#{
                    <<"merged_from">> => SourceBranch
                }
            },
            save(TargetMgr, ThreadId, MergeSnapshot, #{});
        {error, _} = Error ->
            Error
    end.

-spec delete_branch(manager(), thread_id(), binary()) ->
    {ok, manager()} | {error, term()}.
delete_branch(#snapshot_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
              _ThreadId, BranchId) ->
    case BranchId of
        CurrentBranch -> {error, cannot_delete_current_branch};
        ?DEFAULT_BRANCH -> {error, cannot_delete_main_branch};
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

-spec get_lineage(manager(), thread_id(), snapshot_id()) ->
    {ok, [#process_snapshot{}]} | {error, term()}.
get_lineage(Mgr, ThreadId, SnapshotId) ->
    get_lineage_recursive(Mgr, ThreadId, SnapshotId, []).

-spec get_history(manager(), thread_id()) ->
    {ok, [#process_snapshot{}]} | {error, term()}.
get_history(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
            ThreadId) ->
    GroupId = make_group_id(ThreadId, CurrentBranch),
    case beamai_state_store:list(StateStore, GroupId, #list_opts_v2{limit => 10000, order => asc}) of
        {ok, States} ->
            Snapshots = [map_to_process_snapshot(S#stored_state.data) || S <- States],
            %% 按序列号排序
            Sorted = lists:sort(fun(A, B) ->
                A#process_snapshot.version =< B#process_snapshot.version
            end, Snapshots),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

-spec diff(manager(), thread_id(), snapshot_id(), snapshot_id()) ->
    {ok, map()} | {error, term()}.
diff(Mgr, ThreadId, SnId1, SnId2) ->
    case {load(Mgr, ThreadId, SnId1), load(Mgr, ThreadId, SnId2)} of
        {{ok, Sn1}, {ok, Sn2}} ->
            {ok, #{
                fsm_state_changed => Sn1#process_snapshot.fsm_state =/= Sn2#process_snapshot.fsm_state,
                steps_diff => diff_steps(Sn1#process_snapshot.steps_state, Sn2#process_snapshot.steps_state),
                event_queue_diff => #{
                    count1 => length(Sn1#process_snapshot.event_queue),
                    count2 => length(Sn2#process_snapshot.event_queue)
                },
                version_diff => Sn2#process_snapshot.version - Sn1#process_snapshot.version
            }};
        {Error, _} when element(1, Error) =:= error -> Error;
        {_, Error} -> Error
    end.

%%====================================================================
%% 查询和清理
%%====================================================================

-spec list(manager(), thread_id()) -> {ok, [#process_snapshot{}]} | {error, term()}.
list(Mgr, ThreadId) -> list(Mgr, ThreadId, #{}).

-spec list(manager(), thread_id(), map()) -> {ok, [#process_snapshot{}]} | {error, term()}.
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
            Snapshots = [map_to_process_snapshot(S#stored_state.data) || S <- States],
            {ok, Snapshots};
        Error -> Error
    end.

-spec count(manager(), thread_id()) -> non_neg_integer().
count(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch}, ThreadId) ->
    GroupId = make_group_id(ThreadId, CurrentBranch),
    beamai_state_store:count(StateStore, GroupId).

-spec prune(manager(), thread_id(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
prune(#snapshot_manager{state_store = StateStore, current_branch = CurrentBranch},
      ThreadId, KeepCount) ->
    GroupId = make_group_id(ThreadId, CurrentBranch),
    case beamai_state_store:list(StateStore, GroupId, #list_opts_v2{limit => 10000, order => desc}) of
        {ok, States} when length(States) > KeepCount ->
            ToDelete = lists:sublist(States, KeepCount + 1, length(States) - KeepCount),
            IdsToDelete = [S#stored_state.id || S <- ToDelete],
            beamai_state_store:batch_delete(StateStore, GroupId, IdsToDelete),
            {ok, length(IdsToDelete)};
        _ ->
            {ok, 0}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

generate_snapshot_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("psn_~16.16.0b_~4.16.0b", [Ts, Rand])).

generate_fork_name(ParentBranch) ->
    Rand = rand:uniform(16#FFFF),
    <<ParentBranch/binary, "_fork_", (integer_to_binary(Rand))/binary>>.

make_group_id(ThreadId, BranchId) ->
    <<ThreadId/binary, "_", BranchId/binary>>.

get_thread_version(#snapshot_manager{current_position = Positions}, ThreadId) ->
    maps:get(ThreadId, Positions, 0).

maybe_prune(Mgr, ThreadId, MaxCount) ->
    case count(Mgr, ThreadId) >= MaxCount of
        true -> prune(Mgr, ThreadId, MaxCount - 1);
        false -> ok
    end.

get_current_position_internal(#snapshot_manager{current_position = Positions}, ThreadId, History) ->
    CurrentVersion = maps:get(ThreadId, Positions, 0),
    %% 找到当前序列号在历史中的位置
    case CurrentVersion of
        0 -> length(History) - 1;  %% 默认在最新位置
        _ ->
            case lists:search(fun(Sn) -> Sn#process_snapshot.version =:= CurrentVersion end, History) of
                {value, _} ->
                    length(lists:takewhile(
                        fun(Sn) -> Sn#process_snapshot.version < CurrentVersion end,
                        History
                    ));
                false ->
                    length(History) - 1
            end
    end.

move_to_position(Mgr, ThreadId, History, TargetPos) ->
    Snapshot = lists:nth(TargetPos + 1, History),
    NewPositions = maps:put(ThreadId, Snapshot#process_snapshot.version,
                            Mgr#snapshot_manager.current_position),
    NewMgr = Mgr#snapshot_manager{current_position = NewPositions},
    {ok, Snapshot, NewMgr}.

snapshot_steps_state(StepsState) ->
    maps:map(
        fun(_StepId, #{state := State, collected_inputs := Inputs, activation_count := Count}) ->
            #{state => State, collected_inputs => Inputs, activation_count => Count}
        end,
        StepsState
    ).

restore_steps_state(StepsSnapshots, StepSpecs) ->
    maps:map(
        fun(StepId, #{state := State, collected_inputs := Inputs, activation_count := Count}) ->
            StepSpec = maps:get(StepId, StepSpecs, #{}),
            #{
                '__step_runtime__' => true,
                step_spec => StepSpec,
                state => State,
                collected_inputs => Inputs,
                activation_count => Count
            }
        end,
        StepsSnapshots
    ).

process_snapshot_to_map(Snapshot) ->
    #{
        <<"id">> => Snapshot#process_snapshot.id,
        <<"thread_id">> => Snapshot#process_snapshot.thread_id,
        <<"process_spec">> => Snapshot#process_snapshot.process_spec,
        <<"fsm_state">> => Snapshot#process_snapshot.fsm_state,
        <<"steps_state">> => Snapshot#process_snapshot.steps_state,
        <<"event_queue">> => Snapshot#process_snapshot.event_queue,
        <<"paused_step">> => Snapshot#process_snapshot.paused_step,
        <<"pause_reason">> => Snapshot#process_snapshot.pause_reason,
        <<"parent_id">> => Snapshot#process_snapshot.parent_id,
        <<"branch_id">> => Snapshot#process_snapshot.branch_id,
        <<"version">> => Snapshot#process_snapshot.version,
        <<"snapshot_type">> => Snapshot#process_snapshot.snapshot_type,
        <<"run_id">> => Snapshot#process_snapshot.run_id,
        <<"agent_id">> => Snapshot#process_snapshot.agent_id,
        <<"agent_name">> => Snapshot#process_snapshot.agent_name,
        <<"created_at">> => Snapshot#process_snapshot.created_at,
        <<"metadata">> => Snapshot#process_snapshot.metadata
    }.

map_to_process_snapshot(Map) ->
    #process_snapshot{
        id = maps:get(<<"id">>, Map),
        thread_id = maps:get(<<"thread_id">>, Map),
        process_spec = maps:get(<<"process_spec">>, Map),
        fsm_state = safe_to_atom(maps:get(<<"fsm_state">>, Map, idle)),
        steps_state = maps:get(<<"steps_state">>, Map, #{}),
        event_queue = maps:get(<<"event_queue">>, Map, []),
        paused_step = maps:get(<<"paused_step">>, Map),
        pause_reason = maps:get(<<"pause_reason">>, Map),
        parent_id = maps:get(<<"parent_id">>, Map),
        branch_id = maps:get(<<"branch_id">>, Map, ?DEFAULT_BRANCH),
        version = maps:get(<<"version">>, Map, 0),
        snapshot_type = safe_to_atom(maps:get(<<"snapshot_type">>, Map, manual)),
        run_id = maps:get(<<"run_id">>, Map),
        agent_id = maps:get(<<"agent_id">>, Map),
        agent_name = maps:get(<<"agent_name">>, Map),
        created_at = maps:get(<<"created_at">>, Map, 0),
        metadata = maps:get(<<"metadata">>, Map, #{})
    }.

get_lineage_recursive(Mgr, ThreadId, SnapshotId, Acc) ->
    case load(Mgr, ThreadId, SnapshotId) of
        {ok, Snapshot} ->
            NewAcc = [Snapshot | Acc],
            case Snapshot#process_snapshot.parent_id of
                undefined -> {ok, lists:reverse(NewAcc)};
                ParentId -> get_lineage_recursive(Mgr, ThreadId, ParentId, NewAcc)
            end;
        {error, not_found} when Acc =/= [] -> {ok, lists:reverse(Acc)};
        Error -> Error
    end.

diff_steps(Steps1, Steps2) ->
    #{
        added => maps:keys(maps:without(maps:keys(Steps1), Steps2)),
        removed => maps:keys(maps:without(maps:keys(Steps2), Steps1)),
        changed => [K || K <- maps:keys(Steps1),
                        maps:is_key(K, Steps2),
                        maps:get(K, Steps1) =/= maps:get(K, Steps2)]
    }.

safe_to_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8)
    catch error:badarg -> binary_to_atom(Bin, utf8)
    end;
safe_to_atom(Atom) when is_atom(Atom) -> Atom.
```

## 5. Layer 2: Checkpoint (Graph Engine)

### 5.1 头文件

**文件**: `include/beamai_checkpoint.hrl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Checkpoint 头文件 - Graph Engine 专用
%%%
%%% 面向 Graph Engine (Pregel) 的执行检查点，记录：
%%% - 超步 (superstep) 状态
%%% - 顶点状态和消息
%%% - 执行路径
%%% - 中断/错误恢复信息
%%%
%%% 支持时间旅行：go_back, go_forward, goto, fork
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_CHECKPOINT_HRL).
-define(BEAMAI_CHECKPOINT_HRL, true).

%%====================================================================
%% Checkpoint 核心记录
%%====================================================================

%% Graph Checkpoint - 面向 Graph Engine (Pregel)
-record(graph_checkpoint, {
    %% 唯一标识
    id :: binary(),

    %% 执行标识
    run_id :: binary(),

    %%--------------------------------------------------------------------
    %% Pregel 引擎状态
    %%--------------------------------------------------------------------

    %% 当前超步
    superstep :: non_neg_integer(),

    %% 迭代次数
    iteration :: non_neg_integer(),

    %% 版本号（用于时间旅行定位）
    version :: non_neg_integer(),

    %% 顶点状态
    vertices :: #{atom() => vertex_state()},

    %% 待激活顶点列表
    pending_activations :: [atom()],

    %% 全局共享状态
    global_state :: map(),

    %%--------------------------------------------------------------------
    %% 执行分类
    %%--------------------------------------------------------------------

    %% 活跃顶点列表
    active_vertices :: [atom()],

    %% 已完成顶点列表
    completed_vertices :: [atom()],

    %% 失败顶点（用于重试）
    failed_vertices :: [atom()],

    %% 中断顶点（等待用户输入）
    interrupted_vertices :: [atom()],

    %%--------------------------------------------------------------------
    %% 检查点类型
    %%--------------------------------------------------------------------

    %% 类型: initial | step | error | interrupt | final
    checkpoint_type :: checkpoint_type(),

    %%--------------------------------------------------------------------
    %% 恢复信息
    %%--------------------------------------------------------------------

    %% 是否可恢复
    resumable :: boolean(),

    %% 恢复时注入的数据
    resume_data :: #{atom() => term()},

    %% 重试计数
    retry_count :: non_neg_integer(),

    %%--------------------------------------------------------------------
    %% 分支信息（用于分叉执行）
    %%--------------------------------------------------------------------

    %% 父检查点
    parent_id :: binary() | undefined,

    %% 分支标识
    branch_id :: binary(),

    %%--------------------------------------------------------------------
    %% 元信息
    %%--------------------------------------------------------------------

    %% 时间戳
    created_at :: integer(),

    %% 自定义元数据
    metadata :: map()
}).

%% 顶点状态
-type vertex_state() :: #{
    value := term(),
    active := boolean(),
    messages := [term()],
    halt_voted := boolean()
}.

%% 检查点类型
-type checkpoint_type() ::
    initial |
    step |
    error |
    interrupt |
    final.

%%====================================================================
%% 分支信息
%%====================================================================

-record(checkpoint_branch, {
    id :: binary(),
    name :: binary(),
    head_checkpoint_id :: binary() | undefined,
    checkpoint_count :: non_neg_integer(),
    parent_branch_id :: binary() | undefined,
    forked_from_checkpoint_id :: binary() | undefined,
    created_at :: integer()
}).

%%====================================================================
%% 常量
%%====================================================================

-define(NS_GRAPH_CHECKPOINTS, <<"graph_checkpoints">>).
-define(CHECKPOINT_ID_PREFIX, <<"gcp_">>).
-define(DEFAULT_MAX_CHECKPOINTS, 50).
-define(DEFAULT_CHECKPOINT_BRANCH, <<"main">>).

-endif.
```

### 5.2 模块实现

**文件**: `src/checkpoint/beamai_checkpoint.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Checkpoint 模块 - Graph Engine 专用
%%%
%%% 为 Graph Engine (Pregel) 提供执行检查点功能：
%%% - 保存/恢复图执行状态
%%% - 时间旅行（回退/前进）
%%% - 分叉执行
%%% - 顶点失败重试
%%%
%%% == 时间旅行模型 ==
%%%
%%% Graph Engine 的时间线由超步驱动：
%%% ```
%%% superstep_0 → superstep_1 → superstep_2 → superstep_3
%%%      │             │             │             │
%%%     cp_0          cp_1          cp_2          cp_3
%%%                    ↑                           ↑
%%%                    └──── go_back(2) ───────────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_checkpoint).

-include_lib("beamai_memory/include/beamai_checkpoint.hrl").
-include_lib("beamai_memory/include/beamai_state_store.hrl").

%% 构造函数
-export([new/1, new/2]).

%% 核心操作 - Pregel 集成
-export([
    save_from_pregel/3,
    prepare_restore/4
]).

%% 通用操作
-export([
    save/3,
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
    redo/2,
    get_current_position/2
]).

%% 分叉执行
-export([
    fork_from/4,
    list_branches/2,
    switch_branch/3,
    delete_branch/3
]).

%% 重试和恢复
-export([
    get_failed_vertices/2,
    get_interrupted_vertices/2,
    can_retry/2,
    get_resume_options/3
]).

%% 执行路径
-export([
    get_execution_path/2,
    get_vertex_history/3
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
    state_store :: beamai_state_store:state_store(),
    current_branch :: binary(),
    branches :: #{binary() => #checkpoint_branch{}},
    %% 当前位置（用于时间旅行）
    current_position :: #{binary() => non_neg_integer()},  %% run_id => version
    max_checkpoints :: pos_integer(),
    auto_prune :: boolean()
}).

-opaque manager() :: #checkpoint_manager{}.
-type run_id() :: binary().
-type checkpoint_id() :: binary().

-export_type([manager/0]).

%%====================================================================
%% 构造函数
%%====================================================================

-spec new(beamai_state_store:state_store()) -> manager().
new(StateStore) ->
    new(StateStore, #{}).

-spec new(beamai_state_store:state_store(), map()) -> manager().
new(StateStore, Opts) ->
    InitialBranch = maps:get(initial_branch, Opts, ?DEFAULT_CHECKPOINT_BRANCH),
    #checkpoint_manager{
        state_store = StateStore,
        current_branch = InitialBranch,
        branches = #{InitialBranch => #checkpoint_branch{
            id = InitialBranch,
            name = InitialBranch,
            checkpoint_count = 0,
            created_at = erlang:system_time(millisecond)
        }},
        current_position = #{},
        max_checkpoints = maps:get(max_checkpoints, Opts, ?DEFAULT_MAX_CHECKPOINTS),
        auto_prune = maps:get(auto_prune, Opts, true)
    }.

%%====================================================================
%% Pregel 集成
%%====================================================================

%% @doc 从 Pregel snapshot_data 创建检查点
-spec save_from_pregel(manager(), run_id(), map()) ->
    {ok, checkpoint_id(), manager()} | {error, term()}.
save_from_pregel(Mgr, RunId, SnapshotData) ->
    PregelSnapshot = maps:get(pregel_snapshot, SnapshotData, #{}),
    Vertices = maps:get(vertices, PregelSnapshot, #{}),
    {FailedVertices, InterruptedVertices} = classify_problem_vertices(Vertices),

    %% 获取当前序列号
    CurrentVersion = get_run_version(Mgr, RunId),
    NewVersion = CurrentVersion + 1,

    Checkpoint = #graph_checkpoint{
        id = undefined,
        run_id = RunId,
        superstep = maps:get(superstep, PregelSnapshot, 0),
        iteration = maps:get(iteration, SnapshotData, 0),
        version = NewVersion,
        vertices = Vertices,
        pending_activations = maps:get(pending_activations, PregelSnapshot, []),
        global_state = maps:get(global_state, SnapshotData, #{}),
        active_vertices = maps:get(active_vertices, SnapshotData, []),
        completed_vertices = maps:get(completed_vertices, SnapshotData, []),
        failed_vertices = FailedVertices,
        interrupted_vertices = InterruptedVertices,
        checkpoint_type = maps:get(type, SnapshotData, step),
        resumable = true,
        resume_data = #{},
        retry_count = 0,
        parent_id = undefined,
        branch_id = Mgr#checkpoint_manager.current_branch,
        created_at = erlang:system_time(millisecond),
        metadata = #{}
    },

    save(Mgr, RunId, Checkpoint).

%% @doc 准备恢复选项
%%
%% 返回 graph_runner 可用的 restore_from 格式。
-spec prepare_restore(manager(), run_id(), checkpoint_id(), map()) ->
    {ok, map()} | {error, term()}.
prepare_restore(Mgr, RunId, CheckpointId, Opts) ->
    case load(Mgr, RunId, CheckpointId) of
        {ok, Checkpoint} ->
            ResumeData = maps:get(resume_data, Opts, #{}),
            RestoreOpts = #{
                pregel_snapshot => #{
                    superstep => Checkpoint#graph_checkpoint.superstep,
                    vertices => Checkpoint#graph_checkpoint.vertices,
                    pending_activations => Checkpoint#graph_checkpoint.pending_activations
                },
                global_state => Checkpoint#graph_checkpoint.global_state,
                iteration => Checkpoint#graph_checkpoint.iteration,
                resume_data => ResumeData
            },
            {ok, RestoreOpts};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 通用操作
%%====================================================================

-spec save(manager(), run_id(), #graph_checkpoint{}) ->
    {ok, checkpoint_id(), manager()} | {error, term()}.
save(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch,
                         branches = Branches, current_position = Positions,
                         auto_prune = AutoPrune, max_checkpoints = MaxCheckpoints} = Mgr,
     RunId, Checkpoint0) ->

    case AutoPrune of
        true -> maybe_prune(Mgr, RunId, MaxCheckpoints);
        false -> ok
    end,

    CheckpointId = generate_checkpoint_id(),
    Now = erlang:system_time(millisecond),

    BranchInfo = maps:get(CurrentBranch, Branches, #checkpoint_branch{
        id = CurrentBranch, name = CurrentBranch, checkpoint_count = 0, created_at = Now
    }),
    ParentId = BranchInfo#checkpoint_branch.head_checkpoint_id,

    Version = case Checkpoint0#graph_checkpoint.version of
        undefined -> get_run_version(Mgr, RunId) + 1;
        N -> N
    end,

    Checkpoint = Checkpoint0#graph_checkpoint{
        id = CheckpointId,
        run_id = RunId,
        parent_id = ParentId,
        branch_id = CurrentBranch,
        version = Version,
        created_at = Now
    },

    Data = graph_checkpoint_to_map(Checkpoint),
    GroupId = make_group_id(RunId, CurrentBranch),
    SaveOpts = #save_opts{id = CheckpointId},

    case beamai_state_store:save(StateStore, GroupId, Data, SaveOpts) of
        {ok, CheckpointId} ->
            NewBranchInfo = BranchInfo#checkpoint_branch{
                head_checkpoint_id = CheckpointId,
                checkpoint_count = BranchInfo#checkpoint_branch.checkpoint_count + 1
            },
            NewBranches = maps:put(CurrentBranch, NewBranchInfo, Branches),
            NewPositions = maps:put(RunId, Version, Positions),
            NewMgr = Mgr#checkpoint_manager{
                branches = NewBranches,
                current_position = NewPositions
            },
            {ok, CheckpointId, NewMgr};
        {error, _} = Error ->
            Error
    end.

-spec load(manager(), run_id(), checkpoint_id()) ->
    {ok, #graph_checkpoint{}} | {error, not_found | term()}.
load(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch},
     RunId, CheckpointId) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    case beamai_state_store:load(StateStore, GroupId, CheckpointId) of
        {ok, #stored_state{data = Data}} ->
            {ok, map_to_graph_checkpoint(Data)};
        {error, _} = Error ->
            Error
    end.

-spec load_latest(manager(), run_id()) ->
    {ok, #graph_checkpoint{}} | {error, not_found}.
load_latest(#checkpoint_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
            RunId) ->
    case maps:get(CurrentBranch, Branches, undefined) of
        undefined -> {error, not_found};
        #checkpoint_branch{head_checkpoint_id = undefined} -> {error, not_found};
        #checkpoint_branch{head_checkpoint_id = HeadId} -> load(Mgr, RunId, HeadId)
    end.

-spec delete(manager(), run_id(), checkpoint_id()) -> ok | {error, term()}.
delete(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch},
       RunId, CheckpointId) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    beamai_state_store:delete(StateStore, GroupId, CheckpointId).

%%====================================================================
%% 时间旅行
%%====================================================================

%% @doc 回退 N 个超步
%%
%% 在超步时间线上回退 N 步。
-spec go_back(manager(), run_id(), pos_integer()) ->
    {ok, #graph_checkpoint{}, manager()} | {error, term()}.
go_back(Mgr, RunId, Steps) ->
    case get_execution_path(Mgr, RunId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, RunId, History),
            TargetPos = max(0, CurrentPos - Steps),
            move_to_position(Mgr, RunId, History, TargetPos);
        {ok, []} ->
            {error, no_checkpoints};
        {error, _} = Error ->
            Error
    end.

%% @doc 前进 N 个超步
%%
%% 在超步时间线上前进 N 步（仅在回退后有效）。
-spec go_forward(manager(), run_id(), pos_integer()) ->
    {ok, #graph_checkpoint{}, manager()} | {error, term()}.
go_forward(Mgr, RunId, Steps) ->
    case get_execution_path(Mgr, RunId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, RunId, History),
            MaxPos = length(History) - 1,
            TargetPos = min(MaxPos, CurrentPos + Steps),
            move_to_position(Mgr, RunId, History, TargetPos);
        {ok, []} ->
            {error, no_checkpoints};
        {error, _} = Error ->
            Error
    end.

%% @doc 跳转到指定检查点
-spec goto(manager(), run_id(), checkpoint_id()) ->
    {ok, #graph_checkpoint{}, manager()} | {error, term()}.
goto(Mgr, RunId, CheckpointId) ->
    case load(Mgr, RunId, CheckpointId) of
        {ok, Checkpoint} ->
            NewPositions = maps:put(RunId, Checkpoint#graph_checkpoint.version,
                                    Mgr#checkpoint_manager.current_position),
            NewMgr = Mgr#checkpoint_manager{current_position = NewPositions},
            {ok, Checkpoint, NewMgr};
        {error, _} = Error ->
            Error
    end.

%% @doc 撤销（回退 1 个超步）
-spec undo(manager(), run_id()) ->
    {ok, #graph_checkpoint{}, manager()} | {error, term()}.
undo(Mgr, RunId) ->
    go_back(Mgr, RunId, 1).

%% @doc 重做（前进 1 个超步）
-spec redo(manager(), run_id()) ->
    {ok, #graph_checkpoint{}, manager()} | {error, term()}.
redo(Mgr, RunId) ->
    go_forward(Mgr, RunId, 1).

%% @doc 获取当前在时间线中的位置
-spec get_current_position(manager(), run_id()) ->
    {ok, #{current := non_neg_integer(), total := non_neg_integer(),
           checkpoint_id := binary(), superstep := non_neg_integer()}} |
    {error, term()}.
get_current_position(Mgr, RunId) ->
    case get_execution_path(Mgr, RunId) of
        {ok, History} when length(History) > 0 ->
            CurrentPos = get_current_position_internal(Mgr, RunId, History),
            Checkpoint = lists:nth(CurrentPos + 1, History),
            {ok, #{
                current => CurrentPos,
                total => length(History),
                checkpoint_id => Checkpoint#graph_checkpoint.id,
                superstep => Checkpoint#graph_checkpoint.superstep
            }};
        {ok, []} ->
            {error, no_checkpoints};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 分叉执行
%%====================================================================

%% @doc 从指定检查点分叉创建新执行
%%
%% 用于从超步历史中的某个点开始新的执行路径。
%% 可以注入不同的 resume_data 来测试不同的执行分支。
-spec fork_from(manager(), run_id(), checkpoint_id(), map()) ->
    {ok, binary(), map(), manager()} | {error, term()}.
fork_from(Mgr, RunId, CheckpointId, Opts) ->
    case load(Mgr, RunId, CheckpointId) of
        {ok, Checkpoint} ->
            ForkName = maps:get(branch_name, Opts,
                generate_fork_name(Checkpoint#graph_checkpoint.branch_id)),
            ResumeData = maps:get(resume_data, Opts, #{}),
            Now = erlang:system_time(millisecond),

            NewBranch = #checkpoint_branch{
                id = ForkName,
                name = ForkName,
                head_checkpoint_id = CheckpointId,
                checkpoint_count = 0,
                parent_branch_id = Checkpoint#graph_checkpoint.branch_id,
                forked_from_checkpoint_id = CheckpointId,
                created_at = Now
            },

            NewBranches = maps:put(ForkName, NewBranch, Mgr#checkpoint_manager.branches),
            NewMgr = Mgr#checkpoint_manager{
                branches = NewBranches,
                current_branch = ForkName
            },

            %% 准备恢复选项
            RestoreOpts = #{
                pregel_snapshot => #{
                    superstep => Checkpoint#graph_checkpoint.superstep,
                    vertices => Checkpoint#graph_checkpoint.vertices,
                    pending_activations => Checkpoint#graph_checkpoint.pending_activations
                },
                global_state => Checkpoint#graph_checkpoint.global_state,
                iteration => Checkpoint#graph_checkpoint.iteration,
                resume_data => ResumeData
            },

            {ok, ForkName, RestoreOpts, NewMgr};
        {error, _} = Error ->
            Error
    end.

-spec list_branches(manager(), run_id()) -> {ok, [#checkpoint_branch{}]}.
list_branches(#checkpoint_manager{branches = Branches}, _RunId) ->
    {ok, maps:values(Branches)}.

-spec switch_branch(manager(), run_id(), binary()) ->
    {ok, #graph_checkpoint{} | undefined, manager()} | {error, branch_not_found}.
switch_branch(#checkpoint_manager{branches = Branches} = Mgr, RunId, BranchId) ->
    case maps:is_key(BranchId, Branches) of
        true ->
            NewMgr = Mgr#checkpoint_manager{current_branch = BranchId},
            case load_latest(NewMgr, RunId) of
                {ok, Checkpoint} -> {ok, Checkpoint, NewMgr};
                {error, not_found} -> {ok, undefined, NewMgr}
            end;
        false ->
            {error, branch_not_found}
    end.

-spec delete_branch(manager(), run_id(), binary()) ->
    {ok, manager()} | {error, term()}.
delete_branch(#checkpoint_manager{current_branch = CurrentBranch, branches = Branches} = Mgr,
              _RunId, BranchId) ->
    case BranchId of
        CurrentBranch -> {error, cannot_delete_current_branch};
        ?DEFAULT_CHECKPOINT_BRANCH -> {error, cannot_delete_main_branch};
        _ ->
            case maps:is_key(BranchId, Branches) of
                true ->
                    NewBranches = maps:remove(BranchId, Branches),
                    {ok, Mgr#checkpoint_manager{branches = NewBranches}};
                false ->
                    {error, branch_not_found}
            end
    end.

%%====================================================================
%% 重试和恢复
%%====================================================================

-spec get_failed_vertices(manager(), run_id()) ->
    {ok, [atom()]} | {error, term()}.
get_failed_vertices(Mgr, RunId) ->
    case load_latest(Mgr, RunId) of
        {ok, Checkpoint} ->
            {ok, Checkpoint#graph_checkpoint.failed_vertices};
        {error, _} = Error ->
            Error
    end.

-spec get_interrupted_vertices(manager(), run_id()) ->
    {ok, [atom()]} | {error, term()}.
get_interrupted_vertices(Mgr, RunId) ->
    case load_latest(Mgr, RunId) of
        {ok, Checkpoint} ->
            {ok, Checkpoint#graph_checkpoint.interrupted_vertices};
        {error, _} = Error ->
            Error
    end.

-spec can_retry(manager(), run_id()) -> boolean().
can_retry(Mgr, RunId) ->
    case load_latest(Mgr, RunId) of
        {ok, Checkpoint} ->
            Checkpoint#graph_checkpoint.resumable andalso
            (Checkpoint#graph_checkpoint.failed_vertices =/= [] orelse
             Checkpoint#graph_checkpoint.interrupted_vertices =/= []);
        {error, _} ->
            false
    end.

-spec get_resume_options(manager(), run_id(), map()) ->
    {ok, map()} | {error, term()}.
get_resume_options(Mgr, RunId, ResumeData) ->
    case load_latest(Mgr, RunId) of
        {ok, Checkpoint} ->
            prepare_restore(Mgr, RunId, Checkpoint#graph_checkpoint.id, #{
                resume_data => ResumeData
            });
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 执行路径
%%====================================================================

-spec get_execution_path(manager(), run_id()) ->
    {ok, [#graph_checkpoint{}]} | {error, term()}.
get_execution_path(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch},
                   RunId) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    case beamai_state_store:list(StateStore, GroupId, #list_opts_v2{limit => 10000, order => asc}) of
        {ok, States} ->
            Checkpoints = [map_to_graph_checkpoint(S#stored_state.data) || S <- States],
            Sorted = lists:sort(fun(A, B) ->
                A#graph_checkpoint.version =< B#graph_checkpoint.version
            end, Checkpoints),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

-spec get_vertex_history(manager(), run_id(), atom()) ->
    {ok, [map()]} | {error, term()}.
get_vertex_history(Mgr, RunId, VertexId) ->
    case get_execution_path(Mgr, RunId) of
        {ok, Checkpoints} ->
            History = lists:filtermap(fun(Cp) ->
                case maps:get(VertexId, Cp#graph_checkpoint.vertices, undefined) of
                    undefined -> false;
                    VertexState ->
                        {true, #{
                            checkpoint_id => Cp#graph_checkpoint.id,
                            superstep => Cp#graph_checkpoint.superstep,
                            version => Cp#graph_checkpoint.version,
                            vertex_state => VertexState,
                            checkpoint_type => Cp#graph_checkpoint.checkpoint_type
                        }}
                end
            end, Checkpoints),
            {ok, History};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 查询和清理
%%====================================================================

-spec list(manager(), run_id()) -> {ok, [#graph_checkpoint{}]} | {error, term()}.
list(Mgr, RunId) -> list(Mgr, RunId, #{}).

-spec list(manager(), run_id(), map()) -> {ok, [#graph_checkpoint{}]} | {error, term()}.
list(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch},
     RunId, Opts) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    ListOpts = #list_opts_v2{
        limit => maps:get(limit, Opts, 100),
        offset => maps:get(offset, Opts, 0),
        order => maps:get(order, Opts, desc)
    },
    case beamai_state_store:list(StateStore, GroupId, ListOpts) of
        {ok, States} ->
            Checkpoints = [map_to_graph_checkpoint(S#stored_state.data) || S <- States],
            {ok, Checkpoints};
        Error -> Error
    end.

-spec count(manager(), run_id()) -> non_neg_integer().
count(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch}, RunId) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    beamai_state_store:count(StateStore, GroupId).

-spec prune(manager(), run_id(), pos_integer()) -> {ok, non_neg_integer()} | {error, term()}.
prune(#checkpoint_manager{state_store = StateStore, current_branch = CurrentBranch},
      RunId, KeepCount) ->
    GroupId = make_group_id(RunId, CurrentBranch),
    case beamai_state_store:list(StateStore, GroupId, #list_opts_v2{limit => 10000, order => desc}) of
        {ok, States} when length(States) > KeepCount ->
            ToDelete = lists:sublist(States, KeepCount + 1, length(States) - KeepCount),
            IdsToDelete = [S#stored_state.id || S <- ToDelete],
            beamai_state_store:batch_delete(StateStore, GroupId, IdsToDelete),
            {ok, length(IdsToDelete)};
        _ ->
            {ok, 0}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

generate_checkpoint_id() ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("gcp_~16.16.0b_~4.16.0b", [Ts, Rand])).

generate_fork_name(ParentBranch) ->
    Rand = rand:uniform(16#FFFF),
    <<ParentBranch/binary, "_fork_", (integer_to_binary(Rand))/binary>>.

make_group_id(RunId, BranchId) ->
    <<RunId/binary, "_", BranchId/binary>>.

get_run_version(#checkpoint_manager{current_position = Positions}, RunId) ->
    maps:get(RunId, Positions, 0).

maybe_prune(Mgr, RunId, MaxCount) ->
    case count(Mgr, RunId) >= MaxCount of
        true -> prune(Mgr, RunId, MaxCount - 1);
        false -> ok
    end.

get_current_position_internal(#checkpoint_manager{current_position = Positions}, RunId, History) ->
    CurrentVersion = maps:get(RunId, Positions, 0),
    case CurrentVersion of
        0 -> length(History) - 1;
        _ ->
            case lists:search(fun(Cp) -> Cp#graph_checkpoint.version =:= CurrentVersion end, History) of
                {value, _} ->
                    length(lists:takewhile(
                        fun(Cp) -> Cp#graph_checkpoint.version < CurrentVersion end,
                        History
                    ));
                false ->
                    length(History) - 1
            end
    end.

move_to_position(Mgr, RunId, History, TargetPos) ->
    Checkpoint = lists:nth(TargetPos + 1, History),
    NewPositions = maps:put(RunId, Checkpoint#graph_checkpoint.version,
                            Mgr#checkpoint_manager.current_position),
    NewMgr = Mgr#checkpoint_manager{current_position = NewPositions},
    {ok, Checkpoint, NewMgr}.

classify_problem_vertices(Vertices) ->
    maps:fold(fun(VertexId, VertexState, {Failed, Interrupted}) ->
        Value = maps:get(value, VertexState, #{}),
        case maps:get(status, Value, undefined) of
            error -> {[VertexId | Failed], Interrupted};
            interrupted -> {Failed, [VertexId | Interrupted]};
            _ -> {Failed, Interrupted}
        end
    end, {[], []}, Vertices).

graph_checkpoint_to_map(Cp) ->
    #{
        <<"id">> => Cp#graph_checkpoint.id,
        <<"run_id">> => Cp#graph_checkpoint.run_id,
        <<"superstep">> => Cp#graph_checkpoint.superstep,
        <<"iteration">> => Cp#graph_checkpoint.iteration,
        <<"version">> => Cp#graph_checkpoint.version,
        <<"vertices">> => Cp#graph_checkpoint.vertices,
        <<"pending_activations">> => Cp#graph_checkpoint.pending_activations,
        <<"global_state">> => Cp#graph_checkpoint.global_state,
        <<"active_vertices">> => Cp#graph_checkpoint.active_vertices,
        <<"completed_vertices">> => Cp#graph_checkpoint.completed_vertices,
        <<"failed_vertices">> => Cp#graph_checkpoint.failed_vertices,
        <<"interrupted_vertices">> => Cp#graph_checkpoint.interrupted_vertices,
        <<"checkpoint_type">> => Cp#graph_checkpoint.checkpoint_type,
        <<"resumable">> => Cp#graph_checkpoint.resumable,
        <<"resume_data">> => Cp#graph_checkpoint.resume_data,
        <<"retry_count">> => Cp#graph_checkpoint.retry_count,
        <<"parent_id">> => Cp#graph_checkpoint.parent_id,
        <<"branch_id">> => Cp#graph_checkpoint.branch_id,
        <<"created_at">> => Cp#graph_checkpoint.created_at,
        <<"metadata">> => Cp#graph_checkpoint.metadata
    }.

map_to_graph_checkpoint(Map) ->
    #graph_checkpoint{
        id = maps:get(<<"id">>, Map),
        run_id = maps:get(<<"run_id">>, Map),
        superstep = maps:get(<<"superstep">>, Map, 0),
        iteration = maps:get(<<"iteration">>, Map, 0),
        version = maps:get(<<"version">>, Map, 0),
        vertices = maps:get(<<"vertices">>, Map, #{}),
        pending_activations = maps:get(<<"pending_activations">>, Map, []),
        global_state = maps:get(<<"global_state">>, Map, #{}),
        active_vertices = maps:get(<<"active_vertices">>, Map, []),
        completed_vertices = maps:get(<<"completed_vertices">>, Map, []),
        failed_vertices = maps:get(<<"failed_vertices">>, Map, []),
        interrupted_vertices = maps:get(<<"interrupted_vertices">>, Map, []),
        checkpoint_type = safe_to_atom(maps:get(<<"checkpoint_type">>, Map, step)),
        resumable = maps:get(<<"resumable">>, Map, true),
        resume_data = maps:get(<<"resume_data">>, Map, #{}),
        retry_count = maps:get(<<"retry_count">>, Map, 0),
        parent_id = maps:get(<<"parent_id">>, Map),
        branch_id = maps:get(<<"branch_id">>, Map, ?DEFAULT_CHECKPOINT_BRANCH),
        created_at = maps:get(<<"created_at">>, Map, 0),
        metadata = maps:get(<<"metadata">>, Map, #{})
    }.

safe_to_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8)
    catch error:badarg -> binary_to_atom(Bin, utf8)
    end;
safe_to_atom(Atom) when is_atom(Atom) -> Atom.
```

## 6. 功能对比总结

### 6.1 共同能力

| 能力 | Snapshot (Process) | Checkpoint (Graph) |
|------|-------------------|-------------------|
| **保存/恢复** | save_from_runtime / restore_to_runtime | save_from_pregel / prepare_restore |
| **时间旅行** | go_back / go_forward / goto | go_back / go_forward / goto |
| **撤销/重做** | undo / redo | undo / redo |
| **分支/分叉** | create_branch / fork_from | fork_from |
| **历史查询** | get_history / get_lineage | get_execution_path / get_vertex_history |
| **位置追踪** | get_current_position | get_current_position |

### 6.2 特有能力

| 模块 | 特有能力 | 说明 |
|------|---------|------|
| **Snapshot** | merge_branch | 合并分支（事件流合并） |
| **Snapshot** | event_queue 恢复 | 恢复待处理事件队列 |
| **Checkpoint** | retry_vertices | 重试失败的顶点 |
| **Checkpoint** | resume_data 注入 | 恢复时注入用户数据到顶点 |
| **Checkpoint** | get_failed_vertices | 获取失败的顶点列表 |
| **Checkpoint** | get_interrupted_vertices | 获取中断的顶点列表 |

### 6.3 时间单位对比

```
Snapshot (Process) 时间线:
══════════════════════════════════════════════════════
event_1 → event_2 → event_3 → event_4 → event_5
   │         │         │         │         │
 seq=1     seq=2     seq=3     seq=4     seq=5
   │                   │
   └── go_back(2) ─────┘

Checkpoint (Graph) 时间线:
══════════════════════════════════════════════════════
superstep_0 → superstep_1 → superstep_2 → superstep_3
     │             │             │             │
   seq=1         seq=2         seq=3         seq=4
     │                           │
     └──── go_back(2) ───────────┘
```

## 7. 文件清单

```
apps/beamai_memory/
├── include/
│   ├── beamai_state_store.hrl      # Layer 1: 通用状态存储
│   ├── beamai_snapshot.hrl         # Layer 2: Process Framework
│   └── beamai_checkpoint.hrl       # Layer 2: Graph Engine
├── src/
│   ├── store/
│   │   └── beamai_state_store.erl  # Layer 1 实现
│   ├── snapshot/
│   │   └── beamai_snapshot.erl     # Layer 2 - Process 实现
│   └── checkpoint/
│       └── beamai_checkpoint.erl   # Layer 2 - Graph 实现
└── docs/
    └── design/
        └── storage_layer_refactoring_v2.md  # 本文档
```
