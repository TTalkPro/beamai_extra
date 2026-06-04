%%%-------------------------------------------------------------------
%%% @doc beamai_memory 公共工具模块
%%%
%%% 提供 beamai_memory 应用各模块通用的工具函数：
%%%
%%% == 功能分类 ==
%%%
%%% 1. **参数验证**
%%%    - 用户 ID 验证
%%%    - 数据结构验证
%%%    - 配置选项验证
%%%
%%% 2. **命名空间处理**
%%%    - 标准命名空间生成
%%%    - 命名路径转换
%%%    - 命名空间校验
%%%
%%% 3. **数据转换**
%%%    - 二进制转换
%%%    - 时间戳处理
%%%    - 列表/Map 转换
%%%
%%% 4. **查询构建**
%%%    - 搜索选项构建
%%%    - 过滤器构建
%%%    - 分页选项构建
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 验证用户 ID
%%% {ok, UserId} = beamai_memory_helpers:validate_user_id(<<"user_123">>).
%%%
%%% %% 生成偏好命名空间
%%% Namespace = beamai_memory_helpers:preference_namespace(<<"user_123">>).
%%%
%%% %% 构建搜索选项
%%% Opts = beamai_memory_helpers:build_search_opts(#{limit => 10, offset => 0}).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_helpers).

-include_lib("beamai_memory/include/beamai_store.hrl").

%%====================================================================
%%% API 导出 - 参数验证
%%====================================================================

-export([
    validate_user_id/1,
    validate_agent_id/1,
    validate_thread_id/1,
    validate_id/2,
    validate_positive_integer/1,
    validate_non_neg_integer/1,
    validate_binary/1
]).

%%====================================================================
%%% API 导出 - 命名空间处理
%%====================================================================

-export([
    %% 通用命名空间构建
    build_namespace/2,
    build_namespace/3
]).

%%====================================================================
%%% API 导出 - 数据转换
%%====================================================================

-export([
    safe_binary_to_atom/1,
    safe_atom_to_binary/1,
    safe_list_to_binary/1,
    timestamp_to_datetime/1,
    datetime_to_timestamp/1,
    ensure_binary/1,
    ensure_list/1
]).

%%====================================================================
%%% API 导出 - 查询构建
%%====================================================================

-export([
    build_search_opts/1,
    build_put_opts/1,
    normalize_limit/1,
    normalize_offset/1
]).

%%====================================================================
%%% 类型定义
%%====================================================================

-type user_id() :: binary().
-type agent_id() :: binary().
-type thread_id() :: binary().
-type namespace() :: [binary()].
-type validation_result() :: {ok, term()} | {error, term()}.

%%====================================================================
%%% 参数验证函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 验证用户 ID
%%
%% 验证用户 ID 是否为非空二进制字符串。
%%
%% 参数：
%% - UserId: 用户 ID
%%
%% 返回：{ok, UserId} | {error, invalid_user_id}
%%
%% 示例：
%% ```
%% {ok, <<"user_123">>} = beamai_memory_helpers:validate_user_id(<<"user_123">>),
%% {error, invalid_user_id} = beamai_memory_helpers:validate_user_id(<<"">>).
%% '''
%% @end
%%--------------------------------------------------------------------
-spec validate_user_id(term()) -> {ok, user_id()} | {error, invalid_user_id}.
validate_user_id(UserId) when is_binary(UserId), byte_size(UserId) > 0 ->
    {ok, UserId};
validate_user_id(_) ->
    {error, invalid_user_id}.

%%--------------------------------------------------------------------
%% @doc 验证 Agent ID
%%
%% 验证 Agent ID 是否为有效的二进制字符串。
%% @end
%%--------------------------------------------------------------------
-spec validate_agent_id(term()) -> {ok, agent_id()} | {error, invalid_agent_id}.
validate_agent_id(AgentId) when is_binary(AgentId), byte_size(AgentId) > 0 ->
    {ok, AgentId};
validate_agent_id(_) ->
    {error, invalid_agent_id}.

%%--------------------------------------------------------------------
%% @doc 验证 Thread ID
%%
%% 验证 Thread ID 是否为有效的二进制字符串。
%% @end
%%--------------------------------------------------------------------
-spec validate_thread_id(term()) -> {ok, thread_id()} | {error, invalid_thread_id}.
validate_thread_id(ThreadId) when is_binary(ThreadId), byte_size(ThreadId) > 0 ->
    {ok, ThreadId};
validate_thread_id(_) ->
    {error, invalid_thread_id}.

%%--------------------------------------------------------------------
%% @doc 通用 ID 验证
%%
%% 根据类型验证 ID（user_id | agent_id | thread_id）。
%% @end
%%--------------------------------------------------------------------
-spec validate_id(term(), atom()) -> validation_result().
validate_id(Id, user_id) -> validate_user_id(Id);
validate_id(Id, agent_id) -> validate_agent_id(Id);
validate_id(Id, thread_id) -> validate_thread_id(Id);
validate_id(_, _) -> {error, invalid_id_type}.

%%--------------------------------------------------------------------
%% @doc 验证正整数
%%
%% 确保值大于 0 的整数。
%% @end
%%--------------------------------------------------------------------
-spec validate_positive_integer(term()) -> {ok, pos_integer()} | {error, not_positive_integer}.
validate_positive_integer(N) when is_integer(N), N > 0 ->
    {ok, N};
validate_positive_integer(_) ->
    {error, not_positive_integer}.

%%--------------------------------------------------------------------
%% @doc 验证非负整数
%%
%% 确保值大于等于 0 的整数。
%% @end
%%--------------------------------------------------------------------
-spec validate_non_neg_integer(term()) -> {ok, non_neg_integer()} | {error, not_non_neg_integer}.
validate_non_neg_integer(N) when is_integer(N), N >= 0 ->
    {ok, N};
validate_non_neg_integer(_) ->
    {error, not_non_neg_integer}.

%%--------------------------------------------------------------------
%% @doc 验证二进制字符串
%%
%% 确保值是非空的二进制字符串。
%% @end
%%--------------------------------------------------------------------
-spec validate_binary(term()) -> {ok, binary()} | {error, not_binary}.
validate_binary(Binary) when is_binary(Binary), byte_size(Binary) > 0 ->
    {ok, Binary};
validate_binary(_) ->
    {error, not_binary}.

%%====================================================================
%%% 命名空间处理函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 构建两段式命名空间
%%
%% 通用的命名空间构建函数。
%%
%% 示例：
%% ```
%% Namespace = beamai_memory_helpers:build_namespace(<<"semantic">>, <<"user_123">>),
%% %% 结果：[<<"semantic">>, <<"user_123">>]
%% '''
%% @end
%%--------------------------------------------------------------------
-spec build_namespace(binary(), binary()) -> namespace().
build_namespace(Prefix, Suffix) ->
    [Prefix, Suffix].

%%--------------------------------------------------------------------
%% @doc 构建三段式命名空间
%%
%% 通用的命名空间构建函数。
%%
%% 示例：
%% ```
%% Namespace = beamai_memory_helpers:build_namespace(
%%     <<"semantic">>,
%%     <<"user_123">>,
%%     <<"entities">>
%% ),
%% %% 结果：[<<"semantic">>, <<"user_123">>, <<"entities">>]
%% '''
%% @end
%%--------------------------------------------------------------------
-spec build_namespace(binary(), binary(), binary()) -> namespace().
build_namespace(Prefix, Middle, Suffix) ->
    [Prefix, Middle, Suffix].

%%====================================================================
%%% 数据转换函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 安全地将二进制转换为原子
%%
%% 如果二进制对应的原子不存在，则返回 undefined 而不是抛出异常。
%%
%% 用途：处理可能来自外部的不可信数据。
%% @end
%%--------------------------------------------------------------------
-spec safe_binary_to_atom(binary()) -> atom() | undefined.
safe_binary_to_atom(Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        error:badarg ->
            undefined
    end;
safe_binary_to_atom(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc 安全地将原子转换为二进制
%%
%% 不会抛出异常的原子转二进制函数。
%% @end
%%--------------------------------------------------------------------
-spec safe_atom_to_binary(atom()) -> binary().
safe_atom_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

%%--------------------------------------------------------------------
%% @doc 安全地将列表转换为二进制
%%
%% 支持字符串列表和 IO 列表。
%% @end
%%--------------------------------------------------------------------
-spec safe_list_to_binary(string() | iolist()) -> binary().
safe_list_to_binary(String) when is_list(String) ->
    try
        iolist_to_binary(String)
    catch
        _:_ ->
            list_to_binary(String)
    end;
safe_list_to_binary(Binary) when is_binary(Binary) ->
    Binary;
safe_list_to_binary(Term) ->
    term_to_binary(Term).

%%--------------------------------------------------------------------
%% @doc 将时间戳转换为日期时间元组
%%
%% 输入：毫秒时间戳
%% 输出：{{Year, Month, Day}, {Hour, Minute, Second}}
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_datetime(integer()) -> calendar:datetime().
timestamp_to_datetime(TimestampMs) when is_integer(TimestampMs) ->
    TimestampSec = TimestampMs div 1000,
    calendar:gregorian_seconds_to_datetime(TimestampSec).

%%--------------------------------------------------------------------
%% @doc 将日期时间元组转换为时间戳
%%
%% 输入：{{Year, Month, Day}, {Hour, Minute, Second}}
%% 输出：毫秒时间戳
%% @end
%%--------------------------------------------------------------------
-spec datetime_to_timestamp(calendar:datetime()) -> integer().
datetime_to_timestamp(DateTime) ->
    TimestampSec = calendar:datetime_to_gregorian_seconds(DateTime),
    TimestampSec * 1000.

%%--------------------------------------------------------------------
%% @doc 确保值为二进制类型
%%
%% 如果输入是列表，则转换为二进制；否则直接返回。
%%
%% 示例：
%% ```
%% <<"hello">> = beamai_memory_helpers:ensure_binary(<<"hello">>),
%% <<"hello">> = beamai_memory_helpers:ensure_binary("hello").
%% '''
%% @end
%%--------------------------------------------------------------------
-spec ensure_binary(binary() | string() | atom()) -> binary().
ensure_binary(Binary) when is_binary(Binary) -> Binary;
ensure_binary(String) when is_list(String) -> list_to_binary(String);
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

%%--------------------------------------------------------------------
%% @doc 确保值为列表类型
%%
%% 如果输入不是列表，则包装为单元素列表。
%% @end
%%--------------------------------------------------------------------
-spec ensure_list(term()) -> list().
ensure_list(List) when is_list(List) -> List;
ensure_list(Term) -> [Term].

%%====================================================================
%%% 查询构建函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 构建搜索选项 Map
%%
%% 从用户提供的选项构建标准化的搜索选项。
%%
%% 支持的选项：
%% - limit: 返回结果数量限制（默认 10）
%% - offset: 结果偏移量（默认 0）
%% - filter: 过滤条件 Map
%% - sort_by: 排序字段
%% - sort_order: 排序顺序（asc | desc）
%%
%% 示例：
%% ```
%% Opts = beamai_memory_helpers:build_search_opts(#{limit => 20}),
%% %% 结果：#{limit => 20, offset => 0}
%% '''
%% @end
%%--------------------------------------------------------------------
-spec build_search_opts(map()) -> map().
build_search_opts(UserOpts) when is_map(UserOpts) ->
    %% 提取 limit
    Limit = case maps:get(limit, UserOpts, undefined) of
        undefined -> ?DEFAULT_SEARCH_LIMIT;
        L -> normalize_limit(L)
    end,

    %% 提取 offset
    Offset = case maps:get(offset, UserOpts, undefined) of
        undefined -> 0;
        O -> normalize_offset(O)
    end,

    %% 构建基础选项
    BaseOpts = #{
        limit => Limit,
        offset => Offset
    },

    %% 添加可选的 filter
    Opts1 = case maps:get(filter, UserOpts, undefined) of
        undefined -> BaseOpts;
        Filter -> BaseOpts#{filter => Filter}
    end,

    %% 添加可选的排序选项
    Opts2 = case maps:get(sort_by, UserOpts, undefined) of
        undefined -> Opts1;
        SortBy -> Opts1#{sort_by => SortBy}
    end,

    %% 添加可选的排序顺序
    Opts3 = case maps:get(sort_order, UserOpts, undefined) of
        undefined -> Opts2;
        Order when Order =:= asc orelse Order =:= desc ->
            Opts2#{sort_order => Order};
        _ -> Opts2
    end,

    Opts3.

%%--------------------------------------------------------------------
%% @doc 构建存储选项 Map
%%
%% 从用户提供的选项构建标准化的存储选项。
%%
%% 支持的选项：
%% - ttl: 过期时间（秒）
%% - embedding: 向量嵌入
%% - metadata: 元数据 Map
%% - update: 是否更新现有记录
%%
%% 示例：
%% ```
%% Opts = beamai_memory_helpers:build_put_opts(#{
%%     ttl => 3600,
%%     metadata => #{source => <<"user">>}
%% }).
%% '''
%% @end
%%--------------------------------------------------------------------
-spec build_put_opts(map()) -> map().
build_put_opts(UserOpts) when is_map(UserOpts) ->
    %% 构建基础选项
    BaseOpts = #{},

    %% 添加可选的 TTL
    Opts1 = case maps:get(ttl, UserOpts, undefined) of
        undefined -> BaseOpts;
        Ttl when is_integer(Ttl), Ttl > 0 ->
            BaseOpts#{ttl => Ttl};
        _ -> BaseOpts
    end,

    %% 添加可选的 embedding
    Opts2 = case maps:get(embedding, UserOpts, undefined) of
        undefined -> Opts1;
        Embedding when is_list(Embedding) ->
            Opts1#{embedding => Embedding};
        _ -> Opts1
    end,

    %% 添加可选的 metadata
    Opts3 = case maps:get(metadata, UserOpts, undefined) of
        undefined -> Opts2;
        Meta when is_map(Meta) ->
            Opts2#{metadata => Meta};
        _ -> Opts2
    end,

    %% 添加可选的 update 标志
    Opts4 = case maps:get(update, UserOpts, undefined) of
        undefined -> Opts3;
        Update when is_boolean(Update) ->
            Opts3#{update => Update};
        _ -> Opts3
    end,

    Opts4.

%%--------------------------------------------------------------------
%% @doc 规范化 limit 值
%%
%% 确保 limit 在合理范围内 [1, MAX_LIMIT]。
%% @end
%%--------------------------------------------------------------------
-spec normalize_limit(term()) -> pos_integer().
normalize_limit(Limit) when is_integer(Limit), Limit > 0 ->
    min(Limit, ?DEFAULT_SEARCH_LIMIT);
normalize_limit(_) ->
    ?DEFAULT_SEARCH_LIMIT.

%%--------------------------------------------------------------------
%% @doc 规范化 offset 值
%%
%% 确保 offset 为非负整数。
%% @end
%%--------------------------------------------------------------------
-spec normalize_offset(term()) -> non_neg_integer().
normalize_offset(Offset) when is_integer(Offset), Offset >= 0 ->
    Offset;
normalize_offset(_) ->
    0.
