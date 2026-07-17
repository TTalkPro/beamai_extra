%%%-------------------------------------------------------------------
%%% @doc beamai_rag_utils 单元测试
%%%
%%% 此前零覆盖。全是纯函数——类型转换、向量运算、map/列表工具。
%%% 重点钉住几个易被"好心改坏"的边界：
%%%   - safe_divide 的 `== 0' 同时接住 0 与 0.0；
%%%   - zip_with_index([]) 靠 lists:seq(0,-1)=[] 正确产出 []；
%%%   - to_float(<<"42">>) 走 binary_to_integer 兜底。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 类型转换
%%====================================================================

ensure_binary_test() ->
    ?assertEqual(<<"abc">>, beamai_rag_utils:ensure_binary(<<"abc">>)),
    ?assertEqual(<<"abc">>, beamai_rag_utils:ensure_binary(abc)),
    ?assert(is_binary(beamai_rag_utils:ensure_binary(123))).

to_float_test() ->
    ?assertEqual(3.14, beamai_rag_utils:to_float(3.14)),
    ?assertEqual(5.0, beamai_rag_utils:to_float(5)),
    ?assertEqual(3.14, beamai_rag_utils:to_float(<<"3.14">>)),
    %% 整数形式的 binary：binary_to_float 失败 → binary_to_integer 兜底
    ?assertEqual(42.0, beamai_rag_utils:to_float(<<"42">>)),
    %% 非数字 → 0.0
    ?assertEqual(0.0, beamai_rag_utils:to_float(<<"nope">>)),
    ?assertEqual(0.0, beamai_rag_utils:to_float(some_atom)).

%%====================================================================
%% 向量运算
%%====================================================================

dot_product_test() ->
    ?assertEqual(32.0, beamai_rag_utils:dot_product([1.0, 2.0, 3.0], [4.0, 5.0, 6.0])),
    %% 正交向量点积为 0
    ?assertEqual(0.0, beamai_rag_utils:dot_product([1.0, 0.0], [0.0, 1.0])),
    ?assertEqual(0, beamai_rag_utils:dot_product([], [])).

vector_norm_test() ->
    ?assertEqual(5.0, beamai_rag_utils:vector_norm([3.0, 4.0])),
    ?assertEqual(0.0, beamai_rag_utils:vector_norm([0.0, 0.0])),
    ?assertEqual(0.0, beamai_rag_utils:vector_norm([])).

%% 关键：`== 0' 是非严格比较，同时接住整数 0 与浮点 0.0
safe_divide_test() ->
    ?assertEqual(2.5, beamai_rag_utils:safe_divide(5, 2)),
    ?assertEqual(0.0, beamai_rag_utils:safe_divide(5, 0)),
    ?assertEqual(0.0, beamai_rag_utils:safe_divide(5, 0.0)),
    ?assertEqual(0.0, beamai_rag_utils:safe_divide(0, 5)).

%%====================================================================
%% Map / 列表工具
%%====================================================================

get_opt_test() ->
    ?assertEqual(v, beamai_rag_utils:get_opt(#{k => v}, k, default)),
    ?assertEqual(default, beamai_rag_utils:get_opt(#{}, k, default)).

zip_with_index_test() ->
    ?assertEqual([{0, a}, {1, b}, {2, c}],
                 beamai_rag_utils:zip_with_index([a, b, c])),
    %% 空列表：lists:seq(0, -1) = [] —— 不能崩
    ?assertEqual([], beamai_rag_utils:zip_with_index([])),
    ?assertEqual([{0, x}], beamai_rag_utils:zip_with_index([x])).

take_test() ->
    ?assertEqual([a, b], beamai_rag_utils:take([a, b, c, d], 2)),
    %% N 超过长度：返回整个列表
    ?assertEqual([a, b], beamai_rag_utils:take([a, b], 5)),
    ?assertEqual([], beamai_rag_utils:take([a, b], 0)),
    ?assertEqual([], beamai_rag_utils:take([], 3)).
