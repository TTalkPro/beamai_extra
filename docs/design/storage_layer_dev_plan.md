# beamai_memory 存储分层重构 - 开发计划

## 1. 深度分析

### 1.1 共同点提取

通过对比 `process_snapshot` 和 `graph_checkpoint` 记录，发现以下共同结构：

```
┌─────────────────────────────────────────────────────────────────────┐
│                         共同基础结构                                 │
├─────────────────────────────────────────────────────────────────────┤
│  id           :: binary()              %% 唯一标识                  │
│  owner_id     :: binary()              %% 所属者 (thread/run)       │
│  parent_id    :: binary() | undefined  %% 父节点 (链表结构)         │
│  branch_id    :: binary()              %% 分支标识                  │
│  version      :: non_neg_integer()     %% 版本号                    │
│  created_at   :: integer()             %% 创建时间戳                │
│  metadata     :: map()                 %% 自定义元数据              │
│  state        :: map()                 %% 核心状态数据              │
├─────────────────────────────────────────────────────────────────────┤
│                         共同操作                                    │
├─────────────────────────────────────────────────────────────────────┤
│  save/3, load/2, delete/2, list/2     %% CRUD                       │
│  go_back/3, go_forward/3, goto/3      %% 时间旅行                   │
│  undo/2, redo/2                       %% 撤销/重做                  │
│  get_current_position/2               %% 位置查询                   │
│  fork_from/4                          %% 分支创建                   │
│  get_history/2, get_lineage/2         %% 历史查询                   │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 差异点分析

```
┌───────────────────────────────────────┬───────────────────────────────────────┐
│         Snapshot (Process)            │         Checkpoint (Graph)            │
├───────────────────────────────────────┼───────────────────────────────────────┤
│ 所属者字段: thread_id                 │ 所属者字段: run_id                    │
├───────────────────────────────────────┼───────────────────────────────────────┤
│ 类型枚举:                             │ 类型枚举:                             │
│ - initial (流程初始化)                │ - initial (图初始化)                  │
│ - step_completed (步骤完成)           │ - step (超步完成)                     │
│ - paused (流程暂停)                   │ - error (执行出错)                    │
│ - completed (流程完成)                │ - interrupt (等待输入)                │
│ - error (执行出错)                    │ - final (图执行完成)                  │
│ - manual (手动创建)                   │                                       │
│ - branch (分支创建)                   │                                       │
├───────────────────────────────────────┼───────────────────────────────────────┤
│ 领域状态:                             │ 领域状态:                             │
│ - process_spec (流程定义)             │ - superstep (当前超步)                │
│ - fsm_state (状态机状态)              │ - iteration (迭代次数)                │
│ - steps_state (步骤状态)              │ - vertices (顶点状态)                 │
│ - event_queue (事件队列)              │ - pending_activations (待激活)        │
│ - paused_step (暂停的步骤)            │ - global_state (全局状态)             │
│ - pause_reason (暂停原因)             │ - active_vertices (活跃顶点)          │
│                                       │ - completed_vertices (完成顶点)       │
│                                       │ - failed_vertices (失败顶点)          │
│                                       │ - interrupted_vertices (中断顶点)     │
├───────────────────────────────────────┼───────────────────────────────────────┤
│ 执行上下文:                           │ 恢复信息:                             │
│ - run_id (执行ID)                     │ - resumable (是否可恢复)              │
│ - agent_id (Agent ID)                 │ - resume_data (恢复注入数据)          │
│ - agent_name (Agent 名称)             │ - retry_count (重试计数)              │
├───────────────────────────────────────┼───────────────────────────────────────┤
│ 特有操作:                             │ 特有操作:                             │
│ - replay_events (事件回放)            │ - retry_vertex (顶点重试)             │
│ - merge_branch (分支合并)             │ - inject_resume_data (注入数据)       │
│ - get_step_state (步骤状态查询)       │ - get_vertex_history (顶点历史)       │
└───────────────────────────────────────┴───────────────────────────────────────┘
```

### 1.3 分层架构设计

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              应用层                                          │
│                                                                             │
│     Process Runtime                          Graph Runner                   │
│     (beamai_process_runtime)                 (graph_runner)                 │
│              │                                      │                       │
│              ▼                                      ▼                       │
├─────────────────────────────────────────────────────────────────────────────┤
│                           Layer 2: 领域层                                    │
│                                                                             │
│  ┌─────────────────────────────┐      ┌─────────────────────────────┐      │
│  │     beamai_snapshot         │      │     beamai_checkpoint       │      │
│  │     (Process 专用)          │      │     (Graph 专用)            │      │
│  │                             │      │                             │      │
│  │  职责:                      │      │  职责:                      │      │
│  │  • 添加 Process 元信息      │      │  • 添加 Graph 元信息        │      │
│  │  • 步骤状态管理             │      │  • 顶点状态管理             │      │
│  │  • 事件队列处理             │      │  • 超步追踪                 │      │
│  │  • Agent 上下文             │      │  • 恢复数据注入             │      │
│  └──────────────┬──────────────┘      └──────────────┬──────────────┘      │
│                 │                                    │                      │
│                 │    实现 timeline 行为              │                      │
│                 └──────────────┬─────────────────────┘                      │
│                                │                                            │
├────────────────────────────────┼────────────────────────────────────────────┤
│                                ▼                                            │
│                    Layer 1.5: 时间线抽象层                                   │
│                                                                             │
│                 ┌─────────────────────────────┐                             │
│                 │      beamai_timeline        │                             │
│                 │      (behaviour)            │                             │
│                 │                             │                             │
│                 │  定义:                      │                             │
│                 │  • 时间旅行回调             │                             │
│                 │  • 分支管理回调             │                             │
│                 │  • 版本控制逻辑             │                             │
│                 │                             │                             │
│                 │  提供:                      │                             │
│                 │  • 通用时间旅行实现         │                             │
│                 │  • 通用分支管理实现         │                             │
│                 │  • 历史查询实现             │                             │
│                 └──────────────┬──────────────┘                             │
│                                │                                            │
├────────────────────────────────┼────────────────────────────────────────────┤
│                                ▼                                            │
│                      Layer 1: 存储层                                        │
│                                                                             │
│                 ┌─────────────────────────────┐                             │
│                 │     beamai_state_store      │                             │
│                 │                             │                             │
│                 │  纯 CRUD:                   │                             │
│                 │  • save/3, load/3           │                             │
│                 │  • delete/3, list/2         │                             │
│                 │  • batch_save/2             │                             │
│                 │  • search/2                 │                             │
│                 └──────────────┬──────────────┘                             │
│                                │                                            │
├────────────────────────────────┼────────────────────────────────────────────┤
│                                ▼                                            │
│                        Backend Layer                                        │
│                                                                             │
│            ┌───────────────────┴───────────────────┐                        │
│            │                                       │                        │
│     beamai_store_ets                      beamai_store_sqlite               │
│     (内存/测试)                            (持久化/生产)                     │
└─────────────────────────────────────────────────────────────────────────────┘
```

## 2. 开发阶段

### Phase 1: 基础存储层 (Layer 1)

**目标**: 实现纯粹的 CRUD 存储层，无业务语义

#### Task 1.1: 定义 state_store 头文件
- 文件: `apps/beamai_memory/include/beamai_state_store.hrl`
- 内容:
  - `#state_entry{}` 记录定义
  - `#store_opts{}` 选项记录
  - 命名空间常量

#### Task 1.2: 实现 state_store 模块
- 文件: `apps/beamai_memory/src/store/beamai_state_store.erl`
- API:
  ```erlang
  -export([
      new/1, new/2,
      save/3, load/3, delete/3,
      list/2, search/2,
      batch_save/2, batch_delete/2
  ]).
  ```
- 依赖: `beamai_store` (现有后端抽象)

#### Task 1.3: 单元测试
- 文件: `apps/beamai_memory/test/beamai_state_store_SUITE.erl`
- 覆盖: 所有 CRUD 操作、边界条件、并发场景

---

### Phase 2: 时间线抽象层 (Layer 1.5)

**目标**: 抽取共同的时间旅行和分支管理逻辑

#### Task 2.1: 定义 timeline 行为
- 文件: `apps/beamai_memory/src/timeline/beamai_timeline.erl`
- 定义回调:
  ```erlang
  %% 必须实现的回调
  -callback entry_id(Entry :: term()) -> binary().
  -callback entry_owner_id(Entry :: term()) -> binary().
  -callback entry_parent_id(Entry :: term()) -> binary() | undefined.
  -callback entry_version(Entry :: term()) -> non_neg_integer().
  -callback entry_branch_id(Entry :: term()) -> binary().
  -callback entry_created_at(Entry :: term()) -> integer().

  -callback set_entry_id(Entry :: term(), Id :: binary()) -> term().
  -callback set_entry_parent_id(Entry :: term(), ParentId :: binary() | undefined) -> term().
  -callback set_entry_version(Entry :: term(), Version :: non_neg_integer()) -> term().
  -callback set_entry_branch_id(Entry :: term(), BranchId :: binary()) -> term().

  -callback new_entry(OwnerId :: binary(), State :: map(), Opts :: map()) -> term().
  -callback entry_to_state_entry(Entry :: term()) -> #state_entry{}.
  -callback state_entry_to_entry(StateEntry :: #state_entry{}) -> term().

  %% 可选回调（有默认实现）
  -callback namespace() -> binary().
  -callback id_prefix() -> binary().
  ```

#### Task 2.2: 实现通用时间旅行函数
- 同一文件: `beamai_timeline.erl`
- 通用函数:
  ```erlang
  -export([
      %% 时间旅行
      go_back/4,      %% (Module, Manager, OwnerId, Steps)
      go_forward/4,
      goto/4,
      undo/3,
      redo/3,
      get_current_position/3,

      %% 分支管理
      fork_from/5,    %% (Module, Manager, EntryId, NewBranchName, Opts)
      list_branches/2,
      switch_branch/3,

      %% 历史查询
      get_history/3,
      get_lineage/3,
      compare/4
  ]).
  ```

#### Task 2.3: 定义 timeline_manager 记录
- 通用管理器状态:
  ```erlang
  -record(timeline_manager, {
      state_store :: beamai_state_store:store(),
      module :: module(),                          %% 实现模块
      current_branch :: binary(),
      branches :: #{binary() => branch_info()},
      current_position :: #{binary() => non_neg_integer()},
      max_entries :: pos_integer(),
      auto_prune :: boolean()
  }).
  ```

#### Task 2.4: 单元测试
- 文件: `apps/beamai_memory/test/beamai_timeline_SUITE.erl`
- 使用 mock 模块测试通用逻辑

---

### Phase 3: Snapshot 领域层 (Layer 2 - Process)

**目标**: 实现 Process Framework 专用的 Snapshot 模块

#### Task 3.1: 更新 snapshot 头文件
- 文件: `apps/beamai_memory/include/beamai_snapshot.hrl`
- 更新 `#process_snapshot{}` 记录
- 添加 `#snapshot_metadata{}` 用于 Process 特定元信息:
  ```erlang
  -record(snapshot_metadata, {
      %% 快照类型
      snapshot_type :: snapshot_type(),

      %% Process 信息
      process_name :: atom() | undefined,
      process_spec :: map() | atom(),
      fsm_state :: idle | running | paused | completed | failed,

      %% 步骤信息
      step_id :: atom() | undefined,
      steps_state :: #{atom() => step_snapshot()},

      %% 事件队列
      event_queue :: [map()],

      %% 暂停信息
      paused_step :: atom() | undefined,
      pause_reason :: term() | undefined,

      %% 执行上下文
      run_id :: binary() | undefined,
      agent_id :: binary() | undefined,
      agent_name :: binary() | undefined
  }).
  ```

#### Task 3.2: 实现 beamai_snapshot 模块
- 文件: `apps/beamai_memory/src/snapshot/beamai_snapshot.erl`
- 实现 `beamai_timeline` 行为
- 添加 Process 特定 API:
  ```erlang
  -export([
      %% 基础操作 (委托给 timeline)
      new/1, new/2,
      save/3, load/2, delete/2, list/2,

      %% 时间旅行 (委托给 timeline)
      go_back/3, go_forward/3, goto/3,
      undo/2, redo/2, get_current_position/2,

      %% 分支管理 (委托给 timeline)
      fork_from/4, list_branches/1, switch_branch/2,

      %% Process 专用
      save_from_runtime/4,          %% 从 Process Runtime 保存
      restore_to_runtime/2,         %% 恢复到 Process Runtime
      get_step_state/3,             %% 获取步骤状态
      replay_events/3,              %% 回放事件
      merge_branch/3                %% 合并分支
  ]).
  ```

#### Task 3.3: 集成测试
- 文件: `apps/beamai_memory/test/beamai_snapshot_SUITE.erl`
- 测试与 Process Runtime 的集成

---

### Phase 4: Checkpoint 领域层 (Layer 2 - Graph)

**目标**: 实现 Graph Engine 专用的 Checkpoint 模块

#### Task 4.1: 创建 checkpoint 头文件
- 文件: `apps/beamai_memory/include/beamai_checkpoint.hrl`
- 定义 `#graph_checkpoint{}` 记录
- 添加 `#checkpoint_metadata{}` 用于 Graph 特定元信息:
  ```erlang
  -record(checkpoint_metadata, {
      %% 检查点类型
      checkpoint_type :: checkpoint_type(),

      %% Pregel 状态
      superstep :: non_neg_integer(),
      iteration :: non_neg_integer(),

      %% 顶点分类
      vertices :: #{atom() => vertex_state()},
      pending_activations :: [atom()],
      active_vertices :: [atom()],
      completed_vertices :: [atom()],
      failed_vertices :: [atom()],
      interrupted_vertices :: [atom()],

      %% 全局状态
      global_state :: map(),

      %% 恢复信息
      resumable :: boolean(),
      resume_data :: #{atom() => term()},
      retry_count :: non_neg_integer()
  }).
  ```

#### Task 4.2: 实现 beamai_checkpoint 模块
- 文件: `apps/beamai_memory/src/checkpoint/beamai_checkpoint.erl`
- 实现 `beamai_timeline` 行为
- 添加 Graph 特定 API:
  ```erlang
  -export([
      %% 基础操作 (委托给 timeline)
      new/1, new/2,
      save/3, load/2, delete/2, list/2,

      %% 时间旅行 (委托给 timeline)
      go_back/3, go_forward/3, goto/3,
      undo/2, redo/2, get_current_position/2,

      %% 分支管理 (委托给 timeline)
      fork_from/4, list_branches/1, switch_branch/2,

      %% Graph 专用
      save_from_runner/4,           %% 从 Graph Runner 保存
      restore_to_runner/3,          %% 恢复到 Graph Runner (含 resume_data)
      retry_vertex/4,               %% 重试失败顶点
      inject_resume_data/3,         %% 注入恢复数据
      get_vertex_history/3,         %% 获取顶点历史
      get_execution_path/2          %% 获取执行路径
  ]).
  ```

#### Task 4.3: 集成测试
- 文件: `apps/beamai_memory/test/beamai_checkpoint_SUITE.erl`
- 测试与 Graph Runner 的集成

---

### Phase 5: 迁移和集成

**目标**: 迁移现有代码，集成到框架中

#### Task 5.1: 迁移 beamai_snapshot_manager
- 将现有 `beamai_snapshot_manager.erl` 功能迁移到新架构
- 保持向后兼容的 API wrapper

#### Task 5.2: 迁移 graph_snapshot
- 将 `graph_snapshot.erl` 功能迁移到 `beamai_checkpoint`
- 更新 `graph_runner.erl` 的调用

#### Task 5.3: 更新 Process Runtime 集成
- 更新 `beamai_process_runtime.erl` 使用新的 `beamai_snapshot`

#### Task 5.4: 端到端测试
- 完整流程测试
- 性能基准测试

---

## 3. 文件清单

### 新增文件

| 文件路径 | 描述 |
|----------|------|
| `include/beamai_state_store.hrl` | 状态存储记录定义 |
| `include/beamai_checkpoint.hrl` | Graph Checkpoint 记录定义 |
| `src/store/beamai_state_store.erl` | 通用状态存储模块 |
| `src/timeline/beamai_timeline.erl` | 时间线行为和通用实现 |
| `src/checkpoint/beamai_checkpoint.erl` | Graph Checkpoint 模块 |
| `test/beamai_state_store_SUITE.erl` | 状态存储测试 |
| `test/beamai_timeline_SUITE.erl` | 时间线测试 |
| `test/beamai_checkpoint_SUITE.erl` | Checkpoint 测试 |

### 修改文件

| 文件路径 | 修改内容 |
|----------|----------|
| `include/beamai_snapshot.hrl` | 更新记录结构，添加 timeline 兼容字段 |
| `src/snapshot/beamai_snapshot.erl` | 重构为实现 timeline 行为 |
| `src/snapshot/beamai_snapshot_manager.erl` | 简化为向后兼容 wrapper |

### 可能废弃文件

| 文件路径 | 说明 |
|----------|------|
| `beamai_core/src/graph/runner/graph_snapshot.erl` | 功能迁移到 beamai_checkpoint 后废弃 |

---

## 4. 里程碑

| 里程碑 | 完成标准 |
|--------|----------|
| M1: 基础存储 | beamai_state_store 通过所有单元测试 |
| M2: 时间线抽象 | beamai_timeline 行为定义完成，通用函数可用 |
| M3: Snapshot 重构 | beamai_snapshot 实现 timeline 行为，Process 集成正常 |
| M4: Checkpoint 实现 | beamai_checkpoint 实现 timeline 行为，Graph 集成正常 |
| M5: 迁移完成 | 现有功能迁移完成，向后兼容，端到端测试通过 |

---

## 5. 风险和缓解

| 风险 | 缓解措施 |
|------|----------|
| 向后兼容性破坏 | 保留 wrapper 模块，分阶段迁移 |
| 性能退化 | 每个阶段进行性能基准测试 |
| 接口设计不合理 | Phase 2 完成后评审，必要时调整 |
| 集成问题 | 充分的集成测试，灰度发布 |

---

## 6. 依赖关系图

```
Phase 1                    Phase 2                    Phase 3 & 4              Phase 5
─────────────────────────────────────────────────────────────────────────────────────────

beamai_state_store    ───►  beamai_timeline     ───►  beamai_snapshot    ───►  迁移
     │                           │                         │                     │
     │                           │                    beamai_checkpoint ───►  集成
     │                           │                         │
     ▼                           ▼                         ▼
 单元测试                    单元测试                  集成测试
```
