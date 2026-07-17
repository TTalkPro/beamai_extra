%%%-------------------------------------------------------------------
%%% @doc Embedding live 测试 —— 真连本地 Qwen3-Embedding 服务。
%%%
%%% 端到端验证 beamai_rag embedding 路径：请求体 json 编码（迁移后的
%%% beamai_utils:encode_json）+ gun HTTP + 响应 json 解析 + 向量运算。
%%%
%%% 服务：http://192.168.186.1:8080（llama.cpp，OpenAI 兼容，1024 维，无需 key）。
%%% @end
%%%-------------------------------------------------------------------
-module(live_embedding).

-export([run/0]).

-define(BASE_URL, <<"http://192.168.186.1:8080/v1/embeddings">>).
-define(MODEL, <<"Qwen3-Embedding-0.6B-Q8_0.gguf">>).
-define(DIM, 1024).

run() ->
    {ok, _} = application:ensure_all_started(beamai_core),
    io:format("~n========== Embedding Live Test ==========~n"),
    Model = beamai_embeddings:new_openai(#{
        api_key => <<"none">>,
        model => ?MODEL,
        base_url => ?BASE_URL,
        dimension => ?DIM
    }),
    R1 = t_single(Model),
    R2 = t_batch(Model),
    R3 = t_similarity(Model),
    io:format("~n========== 结果 ==========~n"),
    io:format("1. 单条 embed_text  : ~ts~n", [tag(R1)]),
    io:format("2. 批量 embed_batch : ~ts~n", [tag(R2)]),
    io:format("3. 余弦相似度语义   : ~ts~n", [tag(R3)]),
    case lists:all(fun(X) -> X =:= ok end, [R1, R2, R3]) of
        true  -> io:format("~n全部通过 ✓~n"), ok;
        false -> io:format("~n有失败 ✗~n"), error
    end.

%%====================================================================

%% 1. 单条：embed_text → 1024 维 float 向量
t_single(Model) ->
    try beamai_embeddings:embed_text(Model, <<"你好，世界"/utf8>>) of
        {ok, Vec} ->
            io:format("  [single] dim=~p head=~p~n",
                      [length(Vec), lists:sublist(Vec, 3)]),
            case length(Vec) =:= ?DIM andalso lists:all(fun is_number/1, Vec) of
                true -> ok; false -> {error, {bad_vector, length(Vec)}}
            end;
        {error, R} -> io:format("  [single] error: ~p~n", [R]), {error, R}
    catch C:E:S -> io:format("  [single] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}} end.

%% 2. 批量：embed_batch → N 个向量
t_batch(Model) ->
    Texts = [<<"苹果"/utf8>>, <<"香蕉"/utf8>>, <<"汽车"/utf8>>],
    try beamai_embeddings:embed_batch(Model, Texts) of
        {ok, Vecs} ->
            io:format("  [batch] n=~p dims=~p~n",
                      [length(Vecs), [length(V) || V <- Vecs]]),
            case length(Vecs) =:= 3 andalso lists:all(fun(V) -> length(V) =:= ?DIM end, Vecs) of
                true -> ok; false -> {error, {bad_batch, length(Vecs)}}
            end;
        {error, R} -> io:format("  [batch] error: ~p~n", [R]), {error, R}
    catch C:E:S -> io:format("  [batch] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}} end.

%% 3. 语义：相关文本余弦相似度应高于不相关（验证真实 embedding + 向量数学）
t_similarity(Model) ->
    try
        {ok, VCat} = beamai_embeddings:embed_text(Model, <<"猫是一种宠物"/utf8>>),
        {ok, VDog} = beamai_embeddings:embed_text(Model, <<"狗是一种宠物"/utf8>>),
        {ok, VStock} = beamai_embeddings:embed_text(Model, <<"今天股市大涨"/utf8>>),
        SimRelated = beamai_embeddings:cosine_similarity(VCat, VDog),
        SimUnrelated = beamai_embeddings:cosine_similarity(VCat, VStock),
        io:format("  [sim] 猫-狗=~.4f  猫-股市=~.4f~n", [SimRelated, SimUnrelated]),
        case SimRelated > SimUnrelated of
            true -> ok;
            false -> {error, {similarity_not_ordered, SimRelated, SimUnrelated}}
        end
    catch C:E:S -> io:format("  [sim] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}} end.

tag(ok) -> "✓ PASS";
tag({error, R}) -> lists:flatten(io_lib:format("✗ FAIL (~p)", [R])).
