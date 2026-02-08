%%%-------------------------------------------------------------------
%%% @doc RAG 公共工具模块
%%%
%%% 提供 RAG 相关模块共享的工具函数：
%%% - 类型转换
%%% - 向量运算
%%% - 安全操作
%%%
%%% 注意：ID 生成已迁移至 beamai_id 模块
%%%
%%% 设计原则：
%%% - 纯函数，无副作用
%%% - 可复用于所有 RAG 模块
%%% - 简化常见操作
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_utils).

%%====================================================================
%% 导出 API
%%====================================================================

%% 类型转换
-export([
    ensure_binary/1,
    to_float/1
]).

%% 向量运算
-export([
    dot_product/2,
    vector_norm/1,
    safe_divide/2
]).

%% Map 操作
-export([
    get_opt/3
]).

%% 列表操作
-export([
    zip_with_index/1,
    take/2
]).

%%====================================================================
%% 类型转换
%%====================================================================

%% @doc 确保值为二进制
%% 委托给 beamai_utils:ensure_binary/1
-spec ensure_binary(term()) -> binary().
ensure_binary(V) ->
    beamai_utils:ensure_binary(V).

%% @doc 转换为浮点数
%%
%% 支持转换：float, integer, binary
-spec to_float(term()) -> float().
to_float(F) when is_float(F) -> F;
to_float(I) when is_integer(I) -> float(I);
to_float(B) when is_binary(B) ->
    try binary_to_float(B)
    catch _:_ ->
        try float(binary_to_integer(B))
        catch _:_ -> 0.0
        end
    end;
to_float(_) -> 0.0.

%%====================================================================
%% 向量运算
%%====================================================================

%% @doc 计算两个向量的点积
%%
%% 使用 lists:zipwith 进行元素级乘法后求和。
-spec dot_product([float()], [float()]) -> float().
dot_product(Vec1, Vec2) ->
    lists:sum(lists:zipwith(fun(A, B) -> A * B end, Vec1, Vec2)).

%% @doc 计算向量的模（L2 范数）
%%
%% 返回向量元素平方和的平方根。
-spec vector_norm([float()]) -> float().
vector_norm(Vec) ->
    math:sqrt(lists:sum([X * X || X <- Vec])).

%% @doc 安全除法
%%
%% 当除数为零时返回默认值 0.0，避免除零错误。
-spec safe_divide(number(), number()) -> float().
safe_divide(_Numerator, Denominator) when Denominator == 0 -> 0.0;
safe_divide(Numerator, Denominator) -> Numerator / Denominator.

%%====================================================================
%% Map 操作
%%====================================================================

%% @doc 从 Map 获取值，支持默认值
%%
%% 简化 maps:get/3 的调用。
-spec get_opt(map(), term(), term()) -> term().
get_opt(Map, Key, Default) ->
    maps:get(Key, Map, Default).

%%====================================================================
%% 列表操作
%%====================================================================

%% @doc 为列表元素添加索引
%%
%% 返回 [{Index, Element}, ...] 格式，索引从 0 开始。
-spec zip_with_index([term()]) -> [{non_neg_integer(), term()}].
zip_with_index(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).

%% @doc 取列表前 N 个元素
%%
%% 当列表长度小于 N 时返回整个列表。
-spec take([term()], non_neg_integer()) -> [term()].
take(List, N) ->
    lists:sublist(List, N).
