%%%-------------------------------------------------------------------
%%% @doc beamai_embeddings 的 provider 形态单元测试
%%%
%%% 覆盖 RAG 嵌入层接入 beamai_llm 的 beamai_embedding 之后的行为：
%%%   - new/1 多形态构造（本地 / 已构造模型 / 现成配置 / provider 选项）
%%%   - embed_text 与 embed_batch 委托 beamai_embedding
%%%   - 检索语义标注：查询 query、待索引文档 document
%%%   - 调用选项透传（batch_size / 重试）与错误传播
%%%   - 接入 beamai_rag 管道后的端到端索引与检索
%%%
%%% 用 beamai_llm 的 mock 嵌入 Provider（哈希伪向量，无网络），
%%% 需要观测请求参数时用测试模块自身实现的假 Provider。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_embeddings_provider_tests).

-include_lib("eunit/include/eunit.hrl").

%% 假 Embedding Provider 回调（供 {custom, ?MODULE} 使用）
-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

%%====================================================================
%% new/1 多形态构造
%%====================================================================

new_defaults_to_local_test() ->
    ?assertMatch(#{type := local}, beamai_embeddings:new(undefined)),
    ?assertMatch(#{type := local}, beamai_embeddings:new(<<"garbage">>)).

new_passes_through_built_model_test() ->
    Local = beamai_embeddings:new_local(),
    ?assertEqual(Local, beamai_embeddings:new(Local)),
    Provider = beamai_embeddings:new_provider(mock, #{}),
    ?assertEqual(Provider, beamai_embeddings:new(Provider)).

new_from_provider_opts_test() ->
    Model = beamai_embeddings:new(#{provider => mock, dimensions => 16}),
    ?assertMatch(#{type := provider}, Model),
    %% dimensions 同步到模型的 dimension 字段
    ?assertEqual(16, maps:get(dimension, Model)),
    Config = maps:get(config, Model),
    ?assertEqual(mock, maps:get(provider, Config)),
    %% 本模块消费的键不会漏进 beamai_embedding 配置
    ?assertNot(maps:is_key(text_type, Config)),
    ?assertNot(maps:is_key(batch_size, Config)).

new_from_ready_config_test() ->
    Config = beamai_embedding:create(mock, #{dimensions => 8}),
    Model = beamai_embeddings:new(Config),
    ?assertMatch(#{type := provider}, Model),
    ?assertEqual(Config, maps:get(config, Model)).

%%====================================================================
%% 委托 beamai_embedding
%%====================================================================

embed_text_via_provider_test() ->
    Model = beamai_embeddings:new_provider(mock, #{dimensions => 16}),
    {ok, Vector} = beamai_embeddings:embed_text(Model, <<"你好"/utf8>>),
    ?assertEqual(16, length(Vector)).

embed_batch_via_provider_test() ->
    Model = beamai_embeddings:new_provider(mock, #{dimensions => 4}),
    {ok, Vectors} = beamai_embeddings:embed_batch(Model, [<<"a">>, <<"b">>, <<"c">>]),
    ?assertEqual(3, length(Vectors)),
    ?assert(lists:all(fun(V) -> length(V) =:= 4 end, Vectors)).

embed_batch_empty_test() ->
    Model = beamai_embeddings:new_provider(mock, #{}),
    ?assertEqual({ok, []}, beamai_embeddings:embed_batch(Model, [])).

deterministic_across_calls_test() ->
    Model = beamai_embeddings:new_provider(mock, #{}),
    {ok, V1} = beamai_embeddings:embed_text(Model, <<"same">>),
    {ok, V2} = beamai_embeddings:embed_text(Model, <<"same">>),
    ?assertEqual(V1, V2).

%%====================================================================
%% 检索语义标注（text_type）
%%====================================================================

query_marked_as_query_test() ->
    reset_fake(),
    Model = beamai_embeddings:new_provider({custom, ?MODULE}, #{}),
    {ok, _} = beamai_embeddings:embed_text(Model, <<"查询"/utf8>>),
    [Request] = get(fake_calls),
    ?assertEqual(<<"query">>, maps:get(text_type, Request)).

documents_marked_as_document_test() ->
    reset_fake(),
    Model = beamai_embeddings:new_provider({custom, ?MODULE}, #{}),
    {ok, _} = beamai_embeddings:embed_batch(Model, [<<"文档"/utf8>>]),
    [Request] = get(fake_calls),
    ?assertEqual(<<"document">>, maps:get(text_type, Request)).

fixed_text_type_overrides_auto_test() ->
    reset_fake(),
    Model = beamai_embeddings:new_provider({custom, ?MODULE},
                                           #{text_type => <<"document">>}),
    {ok, _} = beamai_embeddings:embed_text(Model, <<"查询"/utf8>>),
    [Request] = get(fake_calls),
    ?assertEqual(<<"document">>, maps:get(text_type, Request)).

%%====================================================================
%% 调用选项与错误
%%====================================================================

batch_size_forwarded_test() ->
    reset_fake(),
    %% 假 Provider 单批上限 4，选项再收紧到 2 → 5 条文本分 3 批
    Model = beamai_embeddings:new_provider({custom, ?MODULE}, #{batch_size => 2}),
    {ok, Vectors} = beamai_embeddings:embed_batch(
        Model, [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>]),
    ?assertEqual(5, length(Vectors)),
    ?assertEqual(3, length(get(fake_calls))).

error_propagates_test() ->
    reset_fake(),
    put(fake_error, {http_error, 401, <<"unauthorized">>}),
    Model = beamai_embeddings:new_provider({custom, ?MODULE}, #{max_retries => 0}),
    ?assertEqual({error, {http_error, 401, <<"unauthorized">>}},
                 beamai_embeddings:embed_text(Model, <<"q">>)).

%%====================================================================
%% 接入 beamai_rag 管道
%%====================================================================

pipeline_index_and_retrieve_test() ->
    Pipeline0 = beamai_rag:new(#{embeddings => #{provider => mock, dimensions => 32}}),
    ?assertMatch(#{type := provider}, maps:get(embeddings, Pipeline0)),
    Pipeline = lists:foldl(fun(Doc, Acc) ->
        {ok, _Id, Next} = beamai_rag:index_document(Acc, Doc, #{}),
        Next
    end, Pipeline0, [<<"erlang otp supervision">>, <<"cooking pasta">>]),
    ?assertEqual(2, beamai_rag:get_document_count(Pipeline)),
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>,
                                        #{top_k => 2, min_score => -1.0}),
    ?assertEqual(2, length(Results)).

pipeline_with_provider_embeddings_and_rerank_test() ->
    %% 嵌入与重排都走 beamai_llm 的 mock Provider，全链路无网络
    Pipeline0 = beamai_rag:new(#{
        embeddings => #{provider => mock, dimensions => 32},
        reranker => #{provider => mock}
    }),
    Pipeline = lists:foldl(fun(Doc, Acc) ->
        {ok, _Id, Next} = beamai_rag:index_document(Acc, Doc, #{}),
        Next
    end, Pipeline0, [<<"erlang otp supervision trees">>,
                     <<"cooking pasta at home">>,
                     <<"otp release handling">>]),
    {ok, Results} = beamai_rag:retrieve(Pipeline, <<"erlang otp">>,
                                        #{top_k => 2, min_score => -1.0}),
    ?assertEqual(2, length(Results)),
    ?assert(lists:all(fun(R) -> maps:is_key(rerank_score, R) end, Results)),
    [#{document := Doc} | _] = Results,
    ?assertEqual(<<"erlang otp supervision trees">>, maps:get(content, Doc)).

%%====================================================================
%% 假 Embedding Provider
%%====================================================================

reset_fake() ->
    erase(fake_calls),
    erase(fake_error),
    ok.

name() -> <<"Fake RAG Embeddings">>.

default_config() -> #{model => <<"fake">>}.

validate_config(_Config) -> ok.

max_batch_size() -> 4.

supports_dimensions() -> true.

%% @private 记录请求并按文本长度生成可预期的向量
embed(Config, Request) ->
    put(fake_calls, existing_calls() ++ [Request]),
    case get(fake_error) of
        undefined ->
            Input = maps:get(input, Request, []),
            {ok, beamai_embedding_common:new_response(#{
                model => maps:get(model, Config, <<"fake">>),
                provider => fake,
                embeddings => [[float(byte_size(T))] || T <- Input],
                usage => #{input_tokens => length(Input), total_tokens => length(Input)},
                raw => #{}
            })};
        Error ->
            {error, Error}
    end.

existing_calls() ->
    case get(fake_calls) of
        undefined -> [];
        Calls -> Calls
    end.
