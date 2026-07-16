%%%-------------------------------------------------------------------
%%% @doc beamai_embeddings 单元测试
%%%
%%% 此前该模块零测试。重点回归：
%%%
%%% 1. cosine_similarity/2、euclidean_distance/2 只有一条带 guard 的子句，
%%%    向量不等长时（含空 embedding 的文档参与搜索）直接 function_clause 崩溃。
%%%    已补兜底子句。
%%%
%%% 2. generate_hash_embedding/2 原用 rand:seed/2 + rand:uniform/0，会修改
%%%    调用者进程字典中的随机种子——对宿主进程的随机行为造成隐蔽副作用。
%%%    已改用 rand:seed_s/2 + rand:uniform_s/1（纯函数式，不动进程字典）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embeddings_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% cosine_similarity 回归
%%====================================================================

%% 向量不等长 → 旧代码 function_clause 崩溃；修复后返回 0.0
cosine_unequal_length_no_crash_test() ->
    ?assertEqual(0.0, beamai_embeddings:cosine_similarity([1.0, 2.0, 3.0], [1.0, 2.0])),
    ?assertEqual(0.0, beamai_embeddings:cosine_similarity([0.1, 0.2], [])),
    ?assertEqual(0.0, beamai_embeddings:cosine_similarity([], [0.1, 0.2])).

%% 相同向量 → 余弦相似度 = 1.0
cosine_identical_vectors_test() ->
    ?assertEqual(1.0, beamai_embeddings:cosine_similarity([1.0, 0.0], [1.0, 0.0])),
    ?assertEqual(1.0, beamai_embeddings:cosine_similarity([3.0, 4.0], [3.0, 4.0])).

%% 正交向量 → 0.0
cosine_orthogonal_vectors_test() ->
    ?assertEqual(0.0, beamai_embeddings:cosine_similarity([1.0, 0.0], [0.0, 1.0])).

%% 反向向量 → -1.0
cosine_opposite_vectors_test() ->
    ?assertEqual(-1.0, beamai_embeddings:cosine_similarity([1.0, 0.0], [-1.0, 0.0])).

%% 两个空向量 → 0.0（safe_divide 对零分母兜底）
cosine_both_empty_test() ->
    ?assertEqual(0.0, beamai_embeddings:cosine_similarity([], [])).

%%====================================================================
%% euclidean_distance 回归
%%====================================================================

%% 向量不等长 → 旧代码 function_clause 崩溃；修复后返回极大距离
euclidean_unequal_length_no_crash_test() ->
    Result = beamai_embeddings:euclidean_distance([1.0, 2.0, 3.0], [1.0, 2.0]),
    ?assert(Result > 1.0e300),
    Result2 = beamai_embeddings:euclidean_distance([0.1], []),
    ?assert(Result2 > 1.0e300).

%% 相同向量 → 距离 0.0
euclidean_identical_vectors_test() ->
    ?assertEqual(0.0, beamai_embeddings:euclidean_distance([1.0, 2.0, 3.0], [1.0, 2.0, 3.0])).

%% 已知距离验证：[0,0] 与 [3,4] 距离 = 5
euclidean_known_distance_test() ->
    ?assertEqual(5.0, beamai_embeddings:euclidean_distance([0.0, 0.0], [3.0, 4.0])).

%%====================================================================
%% rand:seed 不污染进程字典（回归）
%%====================================================================

%% generate_hash_embedding 使用 rand:seed_s/rand:uniform_s（纯函数式），
%% 不应修改调用者进程字典中的随机状态。
%% 验证：同一种子下，embed_text 调用前后 uniform() 的输出序列不变。
rand_seed_not_polluted_test() ->
    rand:seed(exsss, {99, 99, 99}),
    Expected = rand:uniform(),
    rand:seed(exsss, {99, 99, 99}),
    {ok, _} = beamai_embeddings:embed_text(beamai_embeddings:new_local(), <<"pollution check">>),
    Actual = rand:uniform(),
    ?assertEqual(Expected, Actual).

%% 连续多次调用也不应累积污染
rand_seed_multiple_calls_not_polluted_test() ->
    rand:seed(exsss, {7, 7, 7}),
    _Expected1 = rand:uniform(),
    Expected2 = rand:uniform(),
    rand:seed(exsss, {7, 7, 7}),
    _ = rand:uniform(),  %% 消耗一个，对齐 Expected1 的位置
    Model = beamai_embeddings:new_local(),
    {ok, _} = beamai_embeddings:embed_text(Model, <<"first">>),
    {ok, _} = beamai_embeddings:embed_text(Model, <<"second">>),
    {ok, _} = beamai_embeddings:embed_batch(Model, [<<"a">>, <<"b">>, <<"c">>]),
    Actual2 = rand:uniform(),
    ?assertEqual(Expected2, Actual2).

%%====================================================================
%% 本地嵌入确定性与维度
%%====================================================================

%% 同一文本应产生相同的嵌入向量
local_embedding_is_deterministic_test() ->
    Model = beamai_embeddings:new_local(),
    {ok, V1} = beamai_embeddings:embed_text(Model, <<"hello world">>),
    {ok, V2} = beamai_embeddings:embed_text(Model, <<"hello world">>),
    ?assertEqual(V1, V2).

%% 不同文本应产生不同的嵌入向量
local_embedding_different_text_test() ->
    Model = beamai_embeddings:new_local(),
    {ok, V1} = beamai_embeddings:embed_text(Model, <<"hello">>),
    {ok, V2} = beamai_embeddings:embed_text(Model, <<"world">>),
    ?assert(V1 =/= V2).

%% 自定义维度
local_embedding_dimension_test() ->
    Model = beamai_embeddings:new_local(#{dimension => 128}),
    {ok, V} = beamai_embeddings:embed_text(Model, <<"test">>),
    ?assertEqual(128, length(V)).

%% 默认维度 384
local_embedding_default_dimension_test() ->
    Model = beamai_embeddings:new_local(),
    {ok, V} = beamai_embeddings:embed_text(Model, <<"test">>),
    ?assertEqual(384, length(V)).

%% 批量嵌入数量正确
local_batch_embedding_test() ->
    Model = beamai_embeddings:new_local(#{dimension => 16}),
    {ok, Vectors} = beamai_embeddings:embed_batch(Model, [<<"a">>, <<"b">>, <<"c">>]),
    ?assertEqual(3, length(Vectors)),
    [?assertEqual(16, length(V)) || V <- Vectors].

%%====================================================================
%% normalize
%%====================================================================

%% 归一化后向量模为 1
normalize_unit_norm_test() ->
    Normalized = beamai_embeddings:normalize([3.0, 4.0]),
    Norm = math:sqrt(lists:sum([X * X || X <- Normalized])),
    ?assert(abs(Norm - 1.0) < 0.0001).

%% 零向量归一化不崩（保持 0）
normalize_zero_vector_test() ->
    ?assertEqual([0.0, 0.0], beamai_embeddings:normalize([0.0, 0.0])).
