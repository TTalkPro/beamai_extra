%%%-------------------------------------------------------------------
%%% @doc State Store 记录和常量定义
%%%
%%% 定义通用状态存储层的记录和常量。
%%% 这是最底层的存储抽象，提供纯 CRUD 操作，无业务语义。
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_STATE_STORE_HRL).
-define(BEAMAI_STATE_STORE_HRL, true).

%%====================================================================
%% 状态条目
%%====================================================================

%% 状态条目 - 通用的状态存储单元
-record(state_entry, {
    %% 唯一标识
    id :: binary(),

    %% 所属者标识 (thread_id 或 run_id)
    owner_id :: binary(),

    %% 父条目 ID（用于链式结构）
    parent_id :: binary() | undefined,

    %% 分支标识
    branch_id :: binary(),

    %% 版本号
    version :: non_neg_integer(),

    %% 状态数据
    state :: map(),

    %% 条目类型（由上层定义语义）
    entry_type :: atom(),

    %% 创建时间戳
    created_at :: integer(),

    %% 自定义元数据
    metadata = #{} :: map()
}).

%%====================================================================
%% 存储选项
%%====================================================================

%% 存储配置选项
-record(state_store_opts, {
    %% 后端类型
    backend :: ets | sqlite | undefined,

    %% 后端配置
    backend_opts = #{} :: map(),

    %% 命名空间
    namespace :: binary(),

    %% 最大条目数
    max_entries :: pos_integer() | undefined,

    %% TTL（秒）
    ttl :: pos_integer() | undefined
}).

%%====================================================================
%% 查询选项
%%====================================================================

%% 列表查询选项
-record(list_entries_opts, {
    %% 所属者过滤
    owner_id :: binary() | undefined,

    %% 分支过滤
    branch_id :: binary() | undefined,

    %% 类型过滤
    entry_type :: atom() | undefined,

    %% 时间范围
    from_time :: integer() | undefined,
    to_time :: integer() | undefined,

    %% 版本范围
    from_version :: non_neg_integer() | undefined,
    to_version :: non_neg_integer() | undefined,

    %% 分页
    offset = 0 :: non_neg_integer(),
    limit = 100 :: pos_integer(),

    %% 排序
    order = asc :: asc | desc
}).

%% 搜索选项
-record(search_entries_opts, {
    %% 所属者
    owner_id :: binary() | undefined,

    %% 分支
    branch_id :: binary() | undefined,

    %% 元数据过滤
    metadata_filter :: map() | undefined,

    %% 分页
    offset = 0 :: non_neg_integer(),
    limit = 100 :: pos_integer()
}).

%%====================================================================
%% 常量
%%====================================================================

%% 默认值
-define(STATE_STORE_DEFAULT_MAX_ENTRIES, 10000).
-define(STATE_STORE_DEFAULT_LIST_LIMIT, 100).
-define(STATE_STORE_DEFAULT_BRANCH, <<"main">>).

%% 命名空间前缀
-define(NS_STATE_STORE, <<"state_store">>).

-endif.
