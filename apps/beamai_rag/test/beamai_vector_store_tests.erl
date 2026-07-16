%%%-------------------------------------------------------------------
%%% @doc beamai_vector_store 单元测试
%%%
%%% 此前该模块零测试。重点回归：
%%%
%%% 1. add_document 默认 embedding => []（空列表）。旧代码 search →
%%%    compute_similarity → cosine_similarity(QueryVec, []) 时向量不等长，
%%%    直接 function_clause 崩溃。已修：score_single_document 跳过空 embedding。
%%%
%%% 2. 正常搜索排序、top_k 截取、空存储等基本路径覆盖。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_vector_store_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 空 embedding 文档不崩搜索（核心回归）
%%====================================================================

%% 文档默认 embedding 为 []，search 不应 function_clause 崩溃
search_with_empty_embedding_no_crash_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _DocId, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"no embedding doc">>
    }),
    {ok, Results} = beamai_vector_store:search(Store1, [0.1, 0.2, 0.3]),
    %% 空 embedding 文档被过滤掉
    ?assertEqual([], Results).

%% 混合：有 embedding 和无 embedding 的文档，只返回有 embedding 的
search_mixed_embeddings_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"no embedding">>
    }),
    {ok, _, Store2} = beamai_vector_store:add_document(Store1, #{
        content => <<"has embedding">>,
        embedding => [1.0, 0.0, 0.0]
    }),
    {ok, Results} = beamai_vector_store:search(Store2, [1.0, 0.0, 0.0]),
    ?assertEqual(1, length(Results)),
    [#{document := Doc}] = Results,
    ?assertEqual(<<"has embedding">>, maps:get(content, Doc)).

%% 空 embedding 显式传入 <<>> 也不崩
search_with_binary_empty_embedding_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"binary empty">>,
        embedding => <<>>
    }),
    {ok, Results} = beamai_vector_store:search(Store1, [1.0]),
    ?assertEqual([], Results).

%%====================================================================
%% 正常搜索
%%====================================================================

%% 相似度排序正确
search_ranking_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"doc A">>, embedding => [1.0, 0.0]
    }),
    {ok, _, Store2} = beamai_vector_store:add_document(Store1, #{
        content => <<"doc B">>, embedding => [0.0, 1.0]
    }),
    {ok, Results} = beamai_vector_store:search(Store2, [0.9, 0.1]),
    [First | _] = Results,
    ?assertEqual(<<"doc A">>, maps:get(content, maps:get(document, First))).

%% top_k 截取
search_top_k_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_documents(Store0, [
        #{content => <<"a">>, embedding => [1.0]},
        #{content => <<"b">>, embedding => [0.5]},
        #{content => <<"c">>, embedding => [0.1]}
    ]),
    {ok, Results} = beamai_vector_store:search(Store1, [1.0], #{top_k => 2}),
    ?assertEqual(2, length(Results)).

%% min_score 过滤
search_min_score_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_documents(Store0, [
        #{content => <<"identical">>, embedding => [1.0, 0.0]},
        #{content => <<"orthogonal">>, embedding => [0.0, 1.0]}
    ]),
    {ok, Results} = beamai_vector_store:search(Store1, [1.0, 0.0], #{min_score => 0.5}),
    ?assertEqual(1, length(Results)),
    [#{document := Doc}] = Results,
    ?assertEqual(<<"identical">>, maps:get(content, Doc)).

%% 空存储搜索
search_empty_store_test() ->
    Store = beamai_vector_store:new(),
    {ok, Results} = beamai_vector_store:search(Store, [1.0, 2.0]),
    ?assertEqual([], Results).

%% 自定义 filter
search_with_filter_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, _, Store1} = beamai_vector_store:add_documents(Store0, [
        #{content => <<"red">>, embedding => [1.0], metadata => #{color => red}},
        #{content => <<"blue">>, embedding => [1.0], metadata => #{color => blue}}
    ]),
    Filter = fun(#{metadata := M}) -> maps:get(color, M) =:= red end,
    {ok, Results} = beamai_vector_store:search(Store1, [1.0], #{filter => Filter}),
    ?assertEqual(1, length(Results)).

%%====================================================================
%% 文档操作
%%====================================================================

%% 文档计数
count_test() ->
    Store0 = beamai_vector_store:new(),
    ?assertEqual(0, beamai_vector_store:count(Store0)),
    {ok, _, Store1} = beamai_vector_store:add_document(Store0, #{content => <<"a">>}),
    ?assertEqual(1, beamai_vector_store:count(Store1)).

%% 获取文档
get_document_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, DocId, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"hello">>, embedding => [1.0]
    }),
    {ok, Doc} = beamai_vector_store:get_document(Store1, DocId),
    ?assertEqual(<<"hello">>, maps:get(content, Doc)).

%% 删除文档
delete_document_test() ->
    Store0 = beamai_vector_store:new(),
    {ok, DocId, Store1} = beamai_vector_store:add_document(Store0, #{
        content => <<"bye">>, embedding => [1.0]
    }),
    {ok, Store2} = beamai_vector_store:delete_document(Store1, DocId),
    ?assertEqual({error, not_found}, beamai_vector_store:get_document(Store2, DocId)).
