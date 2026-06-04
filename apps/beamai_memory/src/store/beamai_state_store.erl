%%%-------------------------------------------------------------------
%%% @doc State Store - 通用状态存储层
%%%
%%% 提供纯粹的 CRUD 操作，无业务语义。
%%% 作为 Timeline 层和领域层的基础存储后端。
%%%
%%% == 设计原则 ==
%%%
%%% 1. 纯存储：只负责数据的存取，不理解数据的业务含义
%%% 2. 命名空间隔离：通过 namespace 隔离不同类型的数据
%%% 3. 可插拔后端：支持 ETS、SQLite 等后端
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_store).

-include_lib("beamai_memory/include/beamai_state_store.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%% 类型导出
-export_type([
    store/0,
    entry/0,
    list_opts/0,
    search_opts/0
]).

%% 构造函数
-export([
    new/1,
    new/2
]).

%% 核心 CRUD
-export([
    save/2,
    load/2,
    delete/2,
    exists/2
]).

%% 批量操作
-export([
    batch_save/2,
    batch_load/2,
    batch_delete/2
]).

%% 查询操作
-export([
    list/2,
    list_by_owner/2,
    list_by_owner/3,
    list_by_branch/3,
    search/2,
    count/1,
    count_by_owner/2
]).

%% 辅助函数
-export([
    generate_id/1,
    get_namespace/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type store() :: #{
    backend := beamai_store:store(),
    namespace := binary(),
    max_entries := pos_integer() | undefined
}.

-type entry() :: #state_entry{}.
-type list_opts() :: #list_entries_opts{} | map().
-type search_opts() :: #search_entries_opts{} | map().

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建 State Store（使用现有后端）
-spec new(beamai_store:store()) -> store().
new(Backend) ->
    new(Backend, #{}).

%% @doc 创建 State Store（带选项）
-spec new(beamai_store:store(), map()) -> store().
new(Backend, Opts) ->
    #{
        backend => Backend,
        namespace => maps:get(namespace, Opts, ?NS_STATE_STORE),
        max_entries => maps:get(max_entries, Opts, undefined)
    }.

%%====================================================================
%% 核心 CRUD
%%====================================================================

%% @doc 保存状态条目
-spec save(store(), entry()) -> {ok, entry()} | {error, term()}.
save(#{backend := Backend, namespace := Namespace}, Entry) ->
    #state_entry{
        id = Id,
        owner_id = OwnerId,
        parent_id = ParentId,
        branch_id = BranchId,
        version = Version,
        state = State,
        entry_type = EntryType,
        created_at = CreatedAt,
        metadata = Metadata
    } = Entry,

    %% 构建存储值
    Value = #{
        <<"id">> => Id,
        <<"owner_id">> => OwnerId,
        <<"parent_id">> => ParentId,
        <<"branch_id">> => BranchId,
        <<"version">> => Version,
        <<"state">> => State,
        <<"entry_type">> => atom_to_binary(EntryType, utf8),
        <<"created_at">> => CreatedAt,
        <<"metadata">> => Metadata
    },

    %% 使用 owner_id 作为命名空间的一部分
    FullNamespace = [Namespace, OwnerId],

    case beamai_store:put(Backend, FullNamespace, Id, Value) of
        ok ->
            {ok, Entry};
        {error, _} = Error ->
            Error
    end.

%% @doc 加载状态条目
-spec load(store(), binary()) -> {ok, entry()} | {error, not_found | term()}.
load(#{backend := Backend, namespace := Namespace}, Id) ->
    %% 需要搜索所有 owner 下的条目
    SearchOpts = #{
        filter => #{<<"id">> => Id},
        limit => 1
    },
    case beamai_store:search(Backend, [Namespace], SearchOpts) of
        {ok, [#search_result{item = Item}]} ->
            {ok, value_to_entry(Item#store_item.value)};
        {ok, []} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%% @doc 删除状态条目
-spec delete(store(), binary()) -> ok | {error, term()}.
delete(Store, Id) ->
    case load(Store, Id) of
        {ok, #state_entry{owner_id = OwnerId}} ->
            #{backend := Backend, namespace := Namespace} = Store,
            beamai_store:delete(Backend, [Namespace, OwnerId], Id);
        {error, not_found} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%% @doc 检查条目是否存在
-spec exists(store(), binary()) -> boolean().
exists(Store, Id) ->
    case load(Store, Id) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%====================================================================
%% 批量操作
%%====================================================================

%% @doc 批量保存
-spec batch_save(store(), [entry()]) -> {ok, [entry()]} | {error, term()}.
batch_save(Store, Entries) ->
    Results = lists:map(fun(Entry) -> save(Store, Entry) end, Entries),
    case lists:partition(fun({ok, _}) -> true; (_) -> false end, Results) of
        {Oks, []} ->
            {ok, [E || {ok, E} <- Oks]};
        {_, [Error | _]} ->
            Error
    end.

%% @doc 批量加载
-spec batch_load(store(), [binary()]) -> {ok, [entry()]} | {error, term()}.
batch_load(Store, Ids) ->
    Results = lists:filtermap(
        fun(Id) ->
            case load(Store, Id) of
                {ok, Entry} -> {true, Entry};
                {error, _} -> false
            end
        end,
        Ids
    ),
    {ok, Results}.

%% @doc 批量删除
-spec batch_delete(store(), [binary()]) -> ok | {error, term()}.
batch_delete(Store, Ids) ->
    lists:foreach(fun(Id) -> delete(Store, Id) end, Ids),
    ok.

%%====================================================================
%% 查询操作
%%====================================================================

%% @doc 列出所有条目
-spec list(store(), list_opts()) -> {ok, [entry()]} | {error, term()}.
list(#{backend := Backend, namespace := Namespace}, Opts) ->
    ListOpts = normalize_list_opts(Opts),
    #list_entries_opts{
        owner_id = OwnerId,
        branch_id = BranchId,
        entry_type = EntryType,
        from_time = FromTime,
        to_time = ToTime,
        from_version = FromVersion,
        to_version = ToVersion,
        offset = Offset,
        limit = Limit,
        order = Order
    } = ListOpts,

    %% 构建过滤条件
    Filter0 = #{},
    Filter1 = maybe_add_filter(Filter0, <<"branch_id">>, BranchId),
    Filter2 = maybe_add_filter(Filter1, <<"entry_type">>,
                               maybe_atom_to_binary(EntryType)),
    Filter3 = maybe_add_range_filter(Filter2, <<"created_at">>, FromTime, ToTime),
    Filter4 = maybe_add_range_filter(Filter3, <<"version">>, FromVersion, ToVersion),

    SearchNs = case OwnerId of
        undefined -> [Namespace];
        _ -> [Namespace, OwnerId]
    end,

    %% 不向后端传递 limit，因为需要先按 version 排序再分页
    SearchOpts = #{
        filter => Filter4
    },

    case beamai_store:search(Backend, SearchNs, SearchOpts) of
        {ok, Results} ->
            Entries = [value_to_entry(R#search_result.item#store_item.value) || R <- Results],
            Sorted = sort_entries(Entries, Order),
            %% 在排序后应用分页
            Paginated = apply_pagination(Sorted, Offset, Limit),
            {ok, Paginated};
        {error, _} = Error ->
            Error
    end.

%% @doc 按所属者列出条目
-spec list_by_owner(store(), binary()) -> {ok, [entry()]} | {error, term()}.
list_by_owner(Store, OwnerId) ->
    list_by_owner(Store, OwnerId, #{}).

-spec list_by_owner(store(), binary(), map()) -> {ok, [entry()]} | {error, term()}.
list_by_owner(Store, OwnerId, Opts) ->
    list(Store, Opts#{owner_id => OwnerId}).

%% @doc 按分支列出条目
-spec list_by_branch(store(), binary(), binary()) -> {ok, [entry()]} | {error, term()}.
list_by_branch(Store, OwnerId, BranchId) ->
    list(Store, #{owner_id => OwnerId, branch_id => BranchId}).

%% @doc 搜索条目
-spec search(store(), search_opts()) -> {ok, [entry()]} | {error, term()}.
search(#{backend := Backend, namespace := Namespace}, Opts) ->
    SearchOpts = normalize_search_opts(Opts),
    #search_entries_opts{
        owner_id = OwnerId,
        branch_id = BranchId,
        metadata_filter = MetaFilter,
        offset = Offset,
        limit = Limit
    } = SearchOpts,

    Filter0 = #{},
    Filter1 = maybe_add_filter(Filter0, <<"branch_id">>, BranchId),
    Filter2 = case MetaFilter of
        undefined -> Filter1;
        _ -> maps:merge(Filter1, maps:fold(
            fun(K, V, Acc) ->
                maps:put(<<"metadata.", K/binary>>, V, Acc)
            end,
            #{},
            MetaFilter
        ))
    end,

    SearchNs = case OwnerId of
        undefined -> [Namespace];
        _ -> [Namespace, OwnerId]
    end,

    StoreOpts = #{
        filter => Filter2,
        offset => Offset,
        limit => Limit
    },

    case beamai_store:search(Backend, SearchNs, StoreOpts) of
        {ok, Results} ->
            Entries = [value_to_entry(R#search_result.item#store_item.value) || R <- Results],
            {ok, Entries};
        {error, _} = Error ->
            Error
    end.

%% @doc 统计条目数量
-spec count(store()) -> {ok, non_neg_integer()} | {error, term()}.
count(#{backend := Backend, namespace := Namespace}) ->
    case beamai_store:search(Backend, [Namespace], #{limit => 1000000}) of
        {ok, Results} ->
            {ok, length(Results)};
        {error, _} = Error ->
            Error
    end.

%% @doc 按所属者统计
-spec count_by_owner(store(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_by_owner(#{backend := Backend, namespace := Namespace}, OwnerId) ->
    case beamai_store:search(Backend, [Namespace, OwnerId], #{limit => 1000000}) of
        {ok, Results} ->
            {ok, length(Results)};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc 生成唯一 ID（委托给 beamai_id）
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    beamai_id:gen_id(Prefix).

%% @doc 获取命名空间
-spec get_namespace(store()) -> binary().
get_namespace(#{namespace := Namespace}) ->
    Namespace.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将存储值转换为条目记录
-spec value_to_entry(map()) -> entry().
value_to_entry(Value) ->
    #state_entry{
        id = maps:get(<<"id">>, Value),
        owner_id = maps:get(<<"owner_id">>, Value),
        parent_id = maps:get(<<"parent_id">>, Value, undefined),
        branch_id = maps:get(<<"branch_id">>, Value, ?STATE_STORE_DEFAULT_BRANCH),
        version = maps:get(<<"version">>, Value, 0),
        state = maps:get(<<"state">>, Value, #{}),
        entry_type = binary_to_existing_atom(maps:get(<<"entry_type">>, Value, <<"unknown">>), utf8),
        created_at = maps:get(<<"created_at">>, Value, 0),
        metadata = maps:get(<<"metadata">>, Value, #{})
    }.

%% @private 标准化列表选项
-spec normalize_list_opts(list_opts()) -> #list_entries_opts{}.
normalize_list_opts(#list_entries_opts{} = Opts) ->
    Opts;
normalize_list_opts(Opts) when is_map(Opts) ->
    #list_entries_opts{
        owner_id = maps:get(owner_id, Opts, undefined),
        branch_id = maps:get(branch_id, Opts, undefined),
        entry_type = maps:get(entry_type, Opts, undefined),
        from_time = maps:get(from_time, Opts, undefined),
        to_time = maps:get(to_time, Opts, undefined),
        from_version = maps:get(from_version, Opts, undefined),
        to_version = maps:get(to_version, Opts, undefined),
        offset = maps:get(offset, Opts, 0),
        limit = maps:get(limit, Opts, ?STATE_STORE_DEFAULT_LIST_LIMIT),
        order = maps:get(order, Opts, asc)
    }.

%% @private 标准化搜索选项
-spec normalize_search_opts(search_opts()) -> #search_entries_opts{}.
normalize_search_opts(#search_entries_opts{} = Opts) ->
    Opts;
normalize_search_opts(Opts) when is_map(Opts) ->
    #search_entries_opts{
        owner_id = maps:get(owner_id, Opts, undefined),
        branch_id = maps:get(branch_id, Opts, undefined),
        metadata_filter = maps:get(metadata_filter, Opts, undefined),
        offset = maps:get(offset, Opts, 0),
        limit = maps:get(limit, Opts, ?STATE_STORE_DEFAULT_LIST_LIMIT)
    }.

%% @private 条件添加过滤条件
-spec maybe_add_filter(map(), binary(), term()) -> map().
maybe_add_filter(Filter, _Key, undefined) ->
    Filter;
maybe_add_filter(Filter, Key, Value) ->
    maps:put(Key, Value, Filter).

%% @private 条件添加范围过滤
-spec maybe_add_range_filter(map(), binary(), term(), term()) -> map().
maybe_add_range_filter(Filter, _Key, undefined, undefined) ->
    Filter;
maybe_add_range_filter(Filter, Key, From, To) ->
    Range = #{},
    Range1 = case From of
        undefined -> Range;
        _ -> maps:put(<<"$gte">>, From, Range)
    end,
    Range2 = case To of
        undefined -> Range1;
        _ -> maps:put(<<"$lte">>, To, Range1)
    end,
    case maps:size(Range2) of
        0 -> Filter;
        _ -> maps:put(Key, Range2, Filter)
    end.

%% @private 原子转二进制（如果非 undefined）
-spec maybe_atom_to_binary(atom() | undefined) -> binary() | undefined.
maybe_atom_to_binary(undefined) -> undefined;
maybe_atom_to_binary(Atom) -> atom_to_binary(Atom, utf8).

%% @private 排序条目
-spec sort_entries([entry()], asc | desc) -> [entry()].
sort_entries(Entries, asc) ->
    lists:sort(
        fun(A, B) ->
            A#state_entry.version =< B#state_entry.version
        end,
        Entries
    );
sort_entries(Entries, desc) ->
    lists:sort(
        fun(A, B) ->
            A#state_entry.version >= B#state_entry.version
        end,
        Entries
    ).

%% @private 应用分页
-spec apply_pagination([entry()], non_neg_integer(), pos_integer()) -> [entry()].
apply_pagination(Entries, Offset, Limit) ->
    Dropped = case Offset >= length(Entries) of
        true -> [];
        false when Offset =:= 0 -> Entries;
        false -> lists:nthtail(Offset, Entries)
    end,
    lists:sublist(Dropped, Limit).
