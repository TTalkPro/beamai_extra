%%%-------------------------------------------------------------------
%%% @doc RAG 重排序适配模块
%%%
%%% 把 beamai_llm 的 beamai_rerank（Cross-Encoder 重排序）接进 RAG 检索链路，
%%% 承担两件事：
%%%
%%%   1. 形态转换：向量检索结果（`#{document, score}'）与 rerank 接口的
%%%      纯文本列表之间来回映射，重排后把得分挂回原结果
%%%   2. 策略承载：粗排候选数、失败降级策略、beamai_llm 缺席时的兜底
%%%
%%% == 两阶段检索 ==
%%%
%%% 向量召回负责「广」，重排序负责「准」：先取 `top_k * candidate_multiplier'
%%% 条候选，再由重排模型打分截到 `top_k'。
%%%
%%% == 配置 ==
%%%
%%% ```erlang
%%% Pipeline = beamai_rag:new(#{
%%%     reranker => #{
%%%         provider => siliconflow,          %% beamai_rerank 支持的 provider
%%%         api_key => ApiKey,
%%%         model => <<"BAAI/bge-reranker-v2-m3">>,
%%%         candidate_multiplier => 4,        %% 粗排候选倍数（默认 4）
%%%         on_error => fallback              %% fallback（默认）| fail
%%%     }
%%% }).
%%% ```
%%%
%%% 也可直接传已建好的 beamai_rerank 配置：
%%% `#{reranker => beamai_rerank:create(cohere, #{api_key => Key})}'。
%%%
%%% == 失败策略 ==
%%%
%%%   - `fallback'（默认）：重排失败时记 warning，退回向量检索原始顺序
%%%     （返回的仍是真实召回结果，只是没经过重排）
%%%   - `fail'：把错误往上抛，由调用方决定
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_reranker).

%% 构造与查询
-export([new/1, enabled/1, on_error/1, candidate_top_k/3]).

%% 重排序
-export([rerank/3, rerank/4]).

-type search_result() :: beamai_vector_store:search_result().

-type reranker() :: disabled | #{
    config := map(),                        %% beamai_rerank 配置
    call_opts := map(),                     %% 每次调用的重试选项
    candidate_multiplier := pos_integer(),
    on_error := fallback | fail
}.

-export_type([reranker/0]).

%% 粗排候选倍数：向量召回 top_k * 4 条交给重排模型
-define(DEFAULT_CANDIDATE_MULTIPLIER, 4).

%% 本模块自己消费、不应混进 beamai_rerank 配置的键
-define(ADAPTER_KEYS, [candidate_multiplier, on_error | ?RETRY_KEYS]).

%% 重试选项：构造时给出即成为默认值，调用时可覆盖
-define(RETRY_KEYS, [max_retries, retry_delay, on_retry]).

%%====================================================================
%% 构造与查询
%%====================================================================

%% @doc 构造重排序器
%%
%% 接受三种入参：
%%   - `undefined' / `false'：不启用（默认）
%%   - beamai_rerank 配置（含 `'__rerank_config__''）：直接使用
%%   - 选项 map（含 `provider'）：由本模块创建 beamai_rerank 配置
-spec new(undefined | false | map()) -> reranker().
new(undefined) -> disabled;
new(false) -> disabled;
new(#{'__rerank_config__' := true} = Config) ->
    wrap(Config, #{});
new(#{provider := Provider} = Opts) ->
    ProviderOpts = maps:without([provider | ?ADAPTER_KEYS], Opts),
    wrap(beamai_rerank:create(Provider, ProviderOpts), Opts);
new(_Other) ->
    disabled.

%% @doc 是否启用重排序
-spec enabled(reranker()) -> boolean().
enabled(disabled) -> false;
enabled(#{config := _}) -> true.

%% @doc 失败策略
-spec on_error(reranker()) -> fallback | fail.
on_error(disabled) -> fallback;
on_error(#{on_error := Policy}) -> Policy.

%% @doc 计算向量召回阶段应取的候选条数
%%
%% 未启用重排时即为 TopK；启用时放大到 `TopK * candidate_multiplier'，
%% 可由调用方通过 Opts 的 `candidate_top_k' 直接指定。
-spec candidate_top_k(reranker(), pos_integer(), map()) -> pos_integer().
candidate_top_k(disabled, TopK, _Opts) ->
    TopK;
candidate_top_k(#{candidate_multiplier := Multiplier}, TopK, Opts) ->
    case maps:get(candidate_top_k, Opts, undefined) of
        N when is_integer(N), N > 0 -> max(N, TopK);
        _ -> TopK * Multiplier
    end.

%%====================================================================
%% 重排序
%%====================================================================

%% @doc 对向量检索结果重排序
-spec rerank(reranker(), binary(), [search_result()]) ->
    {ok, [search_result()]} | {error, term()}.
rerank(Reranker, Query, Results) ->
    rerank(Reranker, Query, Results, #{}).

%% @doc 对向量检索结果重排序（带选项）
%%
%% 选项：
%%   top_n - 重排后保留条数（默认全部保留）
%%   max_retries / retry_delay / on_retry - 覆盖构造时给出的重试策略
%%
%% 返回结果按重排得分降序，每条追加：
%%   `rerank_score' - 重排模型给出的相关性得分
%%   `vector_score' - 向量检索阶段的原始相似度
%% 并把 `score' 覆盖为重排得分，便于下游沿用统一字段。
-spec rerank(reranker(), binary(), [search_result()], map()) ->
    {ok, [search_result()]} | {error, term()}.
rerank(disabled, _Query, Results, Opts) ->
    {ok, take(Results, maps:get(top_n, Opts, undefined))};
rerank(_Reranker, _Query, [], _Opts) ->
    {ok, []};
rerank(#{config := Config} = Reranker, Query, Results, Opts) ->
    Documents = [content_of(R) || R <- Results],
    RerankOpts = rerank_opts(Reranker, Opts),
    case safe_rerank(Config, Query, Documents, RerankOpts) of
        {ok, Ranking} -> {ok, merge_ranking(Ranking, Results)};
        {error, _} = Error -> Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 组装 reranker 结构
wrap(Config, Opts) ->
    #{
        config => Config,
        call_opts => maps:with(?RETRY_KEYS, Opts),
        candidate_multiplier => maps:get(candidate_multiplier, Opts,
                                         ?DEFAULT_CANDIDATE_MULTIPLIER),
        on_error => normalize_policy(maps:get(on_error, Opts, fallback))
    }.

normalize_policy(fail) -> fail;
normalize_policy(_) -> fallback.

%% @private 只透传 beamai_rerank 认识的调用选项
%% 构造时给出的重试选项作为默认值，单次调用可覆盖。
rerank_opts(Reranker, Opts) ->
    maps:merge(maps:get(call_opts, Reranker, #{}),
               maps:with([top_n | ?RETRY_KEYS], Opts)).

%% @private 调用 beamai_rerank
%% beamai_llm 对 beamai_rag 是可选依赖（optional_applications），
%% 未随发布带上时给出明确错误，而不是让 undef 冒到调用方。
safe_rerank(Config, Query, Documents, Opts) ->
    try beamai_rerank:rerank(Config, Query, Documents, Opts)
    catch
        error:undef ->
            {error, {rerank_unavailable, beamai_rerank}};
        Class:Reason:Stack ->
            logger:warning("RAG rerank call failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {rerank_call_failed, Class, Reason}}
    end.

%% @private 取检索结果的文本内容
content_of(#{document := Doc}) ->
    maps:get(content, Doc, <<>>).

%% @private 把重排名次挂回原检索结果
%% 重排返回的 index 指向传入文档列表的下标（从 0 开始）。
merge_ranking(Ranking, Results) ->
    Indexed = list_to_tuple(Results),
    Size = tuple_size(Indexed),
    lists:filtermap(fun(#{index := Index, score := Score}) when Index >= 0, Index < Size ->
                            {true, apply_score(element(Index + 1, Indexed), Score)};
                       (_) ->
                            %% 越界 index（异常响应）直接丢弃，不猜测对应文档
                            false
                    end, Ranking).

%% @private 覆盖 score 并保留向量阶段得分
apply_score(#{score := VectorScore} = Result, RerankScore) ->
    Result#{
        score => RerankScore,
        rerank_score => RerankScore,
        vector_score => VectorScore
    }.

%% @private 截断到前 N 条
take(Results, undefined) -> Results;
take(Results, N) when is_integer(N), N > 0 -> lists:sublist(Results, N);
take(Results, _) -> Results.
