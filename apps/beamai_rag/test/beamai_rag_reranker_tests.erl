%%%-------------------------------------------------------------------
%%% @doc RAG 重排序适配单元测试
%%%
%%% 覆盖：
%%%   - 重排序器构造（关闭 / provider 选项 / 现成 beamai_rerank 配置）
%%%   - 粗排候选数计算（两阶段检索的第一阶段放大）
%%%   - 结果合并（得分覆盖、vector_score 保留、top_n 截断）
%%%   - 接入 beamai_rag:retrieve/3 后的端到端行为与失败降级
%%%
%%% 用 beamai_llm 的 mock 重排 Provider（词重叠打分，无网络），
%%% 失败路径用测试模块自身实现的假 Provider 注入。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_reranker_tests).

-include_lib("eunit/include/eunit.hrl").

%% 假 Rerank Provider 回调（供 {custom, ?MODULE} 使用）
-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

%% mock 重排 Provider 按词重叠打分，构造可预期的相关性差异
-define(DOCS, [
    <<"cats and dogs are pets">>,
    <<"erlang otp supervision trees">>,
    <<"otp release handling">>,
    <<"cooking pasta at home">>
]).

%%====================================================================
%% 构造
%%====================================================================

new_disabled_test() ->
    ?assertEqual(disabled, beamai_rag_reranker:new(undefined)),
    ?assertEqual(disabled, beamai_rag_reranker:new(false)),
    ?assertEqual(disabled, beamai_rag_reranker:new(#{})),
    ?assertNot(beamai_rag_reranker:enabled(disabled)).

new_from_provider_opts_test() ->
    Reranker = beamai_rag_reranker:new(#{provider => mock}),
    ?assert(beamai_rag_reranker:enabled(Reranker)),
    ?assertEqual(fallback, beamai_rag_reranker:on_error(Reranker)),
    %% provider / 策略字段不会漏进 beamai_rerank 配置
    Config = maps:get(config, Reranker),
    ?assertEqual(mock, maps:get(provider, Config)),
    ?assertNot(maps:is_key(candidate_multiplier, Config)),
    ?assertNot(maps:is_key(on_error, Config)).

new_from_ready_config_test() ->
    Config = beamai_rerank:create(mock, #{}),
    Reranker = beamai_rag_reranker:new(Config),
    ?assert(beamai_rag_reranker:enabled(Reranker)),
    ?assertEqual(Config, maps:get(config, Reranker)).

on_error_policy_test() ->
    ?assertEqual(fail, beamai_rag_reranker:on_error(
        beamai_rag_reranker:new(#{provider => mock, on_error => fail}))),
    %% 非法取值退回默认 fallback
    ?assertEqual(fallback, beamai_rag_reranker:on_error(
        beamai_rag_reranker:new(#{provider => mock, on_error => whatever}))).

%%====================================================================
%% 粗排候选数
%%====================================================================

candidate_top_k_test() ->
    %% 未启用重排：召回多少就返回多少
    ?assertEqual(5, beamai_rag_reranker:candidate_top_k(disabled, 5, #{})),
    %% 启用后默认放大 4 倍
    R = beamai_rag_reranker:new(#{provider => mock}),
    ?assertEqual(20, beamai_rag_reranker:candidate_top_k(R, 5, #{})),
    %% 自定义倍数
    R2 = beamai_rag_reranker:new(#{provider => mock, candidate_multiplier => 2}),
    ?assertEqual(10, beamai_rag_reranker:candidate_top_k(R2, 5, #{})),
    %% 调用级显式指定，且不会小于 top_k
    ?assertEqual(30, beamai_rag_reranker:candidate_top_k(R, 5, #{candidate_top_k => 30})),
    ?assertEqual(5, beamai_rag_reranker:candidate_top_k(R, 5, #{candidate_top_k => 2})).

%%====================================================================
%% 结果合并
%%====================================================================

rerank_reorders_and_scores_test() ->
    Reranker = beamai_rag_reranker:new(#{provider => mock}),
    Candidates = fake_results(?DOCS),
    {ok, Results} = beamai_rag_reranker:rerank(Reranker, <<"erlang otp">>, Candidates),
    ?assertEqual(4, length(Results)),
    %% 相关文档排到最前
    [#{document := Doc} | _] = Results,
    ?assertEqual(<<"erlang otp supervision trees">>, maps:get(content, Doc)),
    %% 得分字段：score 被覆盖为重排得分，原相似度保留
    [First | _] = Results,
    ?assertEqual(maps:get(score, First), maps:get(rerank_score, First)),
    ?assert(maps:is_key(vector_score, First)),
    %% 降序
    Scores = [S || #{score := S} <- Results],
    ?assertEqual(lists:reverse(lists:sort(Scores)), Scores).

rerank_top_n_test() ->
    Reranker = beamai_rag_reranker:new(#{provider => mock}),
    {ok, Results} = beamai_rag_reranker:rerank(
        Reranker, <<"otp">>, fake_results(?DOCS), #{top_n => 2}),
    ?assertEqual(2, length(Results)).

rerank_empty_candidates_test() ->
    Reranker = beamai_rag_reranker:new(#{provider => mock}),
    ?assertEqual({ok, []}, beamai_rag_reranker:rerank(Reranker, <<"q">>, [])).

rerank_disabled_passthrough_test() ->
    Candidates = fake_results(?DOCS),
    ?assertEqual({ok, Candidates}, beamai_rag_reranker:rerank(disabled, <<"q">>, Candidates)),
    {ok, Truncated} = beamai_rag_reranker:rerank(disabled, <<"q">>, Candidates, #{top_n => 2}),
    ?assertEqual(2, length(Truncated)).

rerank_error_propagates_test() ->
    reset_fake(),
    put(fake_error, {http_error, 500, <<"boom">>}),
    Reranker = beamai_rag_reranker:new(#{provider => {custom, ?MODULE}}),
    ?assertEqual({error, {http_error, 500, <<"boom">>}},
                 beamai_rag_reranker:rerank(Reranker, <<"q">>, fake_results(?DOCS),
                                            #{max_retries => 0})).

%%====================================================================
%% 接入 beamai_rag:retrieve/3
%%====================================================================

retrieve_with_rerank_test() ->
    Pipeline = indexed_pipeline(#{provider => mock}),
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>, #{top_k => 2, min_score => -1.0}),
    ?assertEqual(2, length(Results)),
    ?assert(lists:all(fun(R) -> maps:is_key(rerank_score, R) end, Results)),
    [#{document := Doc} | _] = Results,
    ?assertEqual(<<"erlang otp supervision trees">>, maps:get(content, Doc)).

retrieve_without_rerank_keeps_vector_results_test() ->
    Pipeline = indexed_pipeline(undefined),
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>, #{top_k => 2, min_score => -1.0}),
    ?assertEqual(2, length(Results)),
    ?assertNot(lists:any(fun(R) -> maps:is_key(rerank_score, R) end, Results)).

retrieve_can_disable_rerank_per_call_test() ->
    Pipeline = indexed_pipeline(#{provider => mock}),
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>,
                                        #{top_k => 2, min_score => -1.0, reranker => false}),
    ?assertNot(lists:any(fun(R) -> maps:is_key(rerank_score, R) end, Results)).

retrieve_rerank_callbacks_test() ->
    Self = self(),
    Callbacks = #{
        on_rerank_start => fun(_Query, Candidates, _Meta) ->
            Self ! {rerank_start, length(Candidates)}, ok
        end,
        on_rerank_end => fun(Results, _Meta) ->
            Self ! {rerank_end, length(Results)}, ok
        end
    },
    Pipeline = indexed_pipeline(#{provider => mock}),
    {ok, _} = beamai_rag:retrieve(Pipeline, <<"otp">>,
                                  #{top_k => 1, min_score => -1.0, callbacks => Callbacks}),
    receive {rerank_start, N} -> ?assert(N >= 1)
    after 1000 -> ?assert(false)
    end,
    receive {rerank_end, 1} -> ok
    after 1000 -> ?assert(false)
    end.

retrieve_rerank_failure_falls_back_test() ->
    reset_fake(),
    put(fake_error, {http_error, 503, <<"unavailable">>}),
    Pipeline = indexed_pipeline(#{provider => {custom, ?MODULE}, on_error => fallback,
                                  max_retries => 0}),
    %% 降级后返回的仍是向量检索的真实结果，只是未经重排
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>,
                                        #{top_k => 2, min_score => -1.0}),
    ?assertEqual(2, length(Results)),
    ?assertNot(lists:any(fun(R) -> maps:is_key(rerank_score, R) end, Results)).

retrieve_rerank_failure_can_fail_hard_test() ->
    reset_fake(),
    put(fake_error, {http_error, 503, <<"unavailable">>}),
    Pipeline = indexed_pipeline(#{provider => {custom, ?MODULE}, on_error => fail,
                                  max_retries => 0}),
    ?assertMatch({error, {rerank_error, _}},
                 beamai_rag:retrieve(Pipeline, <<"erlang otp">>, #{top_k => 2, min_score => -1.0})).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 构造伪检索结果（向量得分随位置递减）
fake_results(Docs) ->
    [#{document => #{id => integer_to_binary(I),
                     content => Doc,
                     embedding => [0.0],
                     metadata => #{}},
       score => 1.0 / (I + 1)}
     || {I, Doc} <- lists:zip(lists:seq(0, length(Docs) - 1), Docs)].

%% @private 建好并索引文档的管道（本地伪嵌入，无网络）
indexed_pipeline(RerankerOpts) ->
    Pipeline = beamai_rag:new(#{
        embeddings => beamai_embeddings:new_local(),
        reranker => RerankerOpts
    }),
    lists:foldl(fun(Doc, Acc) ->
        {ok, _Id, Next} = beamai_rag:index_document(Acc, Doc, #{}),
        Next
    end, Pipeline, ?DOCS).

%%====================================================================
%% 假 Rerank Provider（失败路径注入）
%%====================================================================

reset_fake() ->
    erase(fake_error),
    ok.

name() -> <<"Fake RAG Rerank">>.

default_config() -> #{model => <<"fake">>}.

validate_config(_Config) -> ok.

max_documents() -> 100.

rerank(Config, Request) ->
    case get(fake_error) of
        undefined ->
            Documents = maps:get(documents, Request, []),
            Results = [#{index => I, score => 1.0 / (I + 1)}
                       || I <- lists:seq(0, length(Documents) - 1)],
            {ok, beamai_rerank_common:new_response(#{
                model => maps:get(model, Config, <<"fake">>),
                provider => fake,
                results => Results,
                usage => #{input_tokens => 0, total_tokens => 0},
                raw => #{}
            })};
        Error ->
            {error, Error}
    end.
