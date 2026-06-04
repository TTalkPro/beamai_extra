%%%-------------------------------------------------------------------
%%% @doc Process Snapshot 记录和常量定义
%%%
%%% 定义 Process Framework 专用的 Snapshot 记录。
%%% 用于保存和恢复流程执行状态。
%%%
%%% == 核心概念 ==
%%%
%%% - Thread: 单个执行会话，由 thread_id 标识
%%% - Snapshot: 某一时刻的流程状态快照
%%% - Version: 快照在时间线中的版本号
%%%
%%% == 时间旅行模型 ==
%%%
%%% ```
%%% event_1 ──→ event_2 ──→ event_3 ──→ event_4
%%%    │           │           │           │
%%%   v1          v2          v3          v4
%%%    │                       │
%%%    └─── go_back(2) ────────┘
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_PROCESS_SNAPSHOT_HRL).
-define(BEAMAI_PROCESS_SNAPSHOT_HRL, true).

%%====================================================================
%% Process Snapshot 记录
%%====================================================================

%% Process Snapshot - 面向 Process Framework
-record(process_snapshot, {
    %%--------------------------------------------------------------------
    %% 基础标识（时间线通用字段）
    %%--------------------------------------------------------------------

    %% 唯一标识（创建时为 undefined，由 snapshot 引擎分配）
    id :: binary() | undefined,

    %% 所属线程
    thread_id :: binary(),

    %% 父快照 ID（用于链式追溯）
    parent_id :: binary() | undefined,

    %% 分支标识
    branch_id :: binary(),

    %% 版本号（用于时间旅行定位）
    version :: non_neg_integer(),

    %% 创建时间戳
    created_at :: integer(),

    %%--------------------------------------------------------------------
    %% Process Framework 状态
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
    %% 快照类型和执行上下文
    %%--------------------------------------------------------------------

    %% 快照类型
    snapshot_type :: snapshot_type(),

    %% 触发此快照的步骤 ID
    step_id :: atom() | undefined,

    %% 执行标识
    run_id :: binary() | undefined,
    agent_id :: binary() | undefined,
    agent_name :: binary() | undefined,

    %%--------------------------------------------------------------------
    %% 扩展信息
    %%--------------------------------------------------------------------

    %% 自定义元数据
    metadata = #{} :: map()
}).

%% 步骤快照
-type step_snapshot() :: #{
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%% 快照类型
-type snapshot_type() ::
    initial |           %% 流程初始化
    step_completed |    %% 步骤执行完成
    paused |            %% 流程暂停
    completed |         %% 流程完成
    error |             %% 执行出错
    manual |            %% 手动创建
    branch.             %% 分支创建

%%====================================================================
%% 快照配置
%%====================================================================

-record(snapshot_config, {
    %% 线程 ID（必需）
    thread_id :: binary(),

    %% 最大快照数量
    max_snapshots :: pos_integer(),

    %% 自动清理
    auto_prune :: boolean(),

    %% 自定义配置
    options = #{} :: map()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 命名空间
-define(NS_PROCESS_SNAPSHOTS, <<"process_snapshots">>).

%% ID 前缀
-define(SNAPSHOT_ID_PREFIX, <<"psn_">>).

%% 默认值
-define(DEFAULT_MAX_SNAPSHOTS, 100).
-define(DEFAULT_BRANCH, <<"main">>).

%% 状态机状态
-define(FSM_IDLE, idle).
-define(FSM_RUNNING, running).
-define(FSM_PAUSED, paused).
-define(FSM_COMPLETED, completed).
-define(FSM_FAILED, failed).

-endif.
