%%%-------------------------------------------------------------------
%%% @doc beamai_rag 的 LLM 错误传播测试
%%%
%%% 历史缺陷：call_llm/2 的 catch 是 `_:_ -> {ok, <<"[LLM unavailable] ...">>}'，
%%% 把任何异常吞成一个**带 {ok,_} 标签的假答案**——没配 API key 时，
%%% query/2 会把提示词原样回显当作模型的回答返回。调用方无从分辨真答案与占位符，
%%% 这类静默错误答案比直接报错危险得多。
%%%
%%% 这里走公开的 query/3 路径（call_llm/2 是私有的）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_llm_error_tests).

-include_lib("eunit/include/eunit.hrl").

llm_error_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun llm_crash_is_reported_as_error/0,
      fun llm_error_return_is_propagated/0,
      fun llm_success_still_works/0,
      fun failure_never_echoes_the_question_back/0]}.

setup() ->
    catch meck:unload(beamai_chat_completion),
    catch meck:unload(beamai_embeddings),
    meck:new(beamai_chat_completion, [passthrough, unstick]),
    %% 检索链路要有 embedding 才能跑到 LLM 那一步
    meck:new(beamai_embeddings, [passthrough, unstick]),
    meck:expect(beamai_embeddings, embed_text, fun(_, _) -> {ok, [1.0, 0.0, 0.0]} end),
    meck:expect(beamai_embeddings, embed_batch, fun(_, Ts) -> {ok, [[1.0, 0.0, 0.0] || _ <- Ts]} end),
    ok.

cleanup(_) ->
    catch meck:unload(beamai_chat_completion),
    catch meck:unload(beamai_embeddings),
    ok.

%% 回归测试：异常必须变成 {error,_}，不能变成 {ok, 假答案}
llm_crash_is_reported_as_error() ->
    meck:expect(beamai_chat_completion, create, fun(_, _) -> error(no_api_key) end),
    ?assertMatch({error, {llm_call_failed, _, _}}, query(<<"问题是什么"/utf8>>)).

%% {error,_} 返回值照常上抛，不被吞
llm_error_return_is_propagated() ->
    meck:expect(beamai_chat_completion, create, fun(_, C) -> C#{'__llm_config__' => true} end),
    meck:expect(beamai_chat_completion, chat, fun(_, _) -> {error, rate_limited} end),
    ?assertEqual({error, {llm_error, rate_limited}}, query(<<"问题是什么"/utf8>>)).

llm_success_still_works() ->
    meck:expect(beamai_chat_completion, create, fun(_, C) -> C#{'__llm_config__' => true} end),
    meck:expect(beamai_chat_completion, chat,
                fun(_, _) -> {ok, #{content => <<"真答案"/utf8>>}} end),
    ?assertEqual({ok, <<"真答案"/utf8>>}, query(<<"问题是什么"/utf8>>)).

%% 失败时绝不能把问题/提示词当答案回显
failure_never_echoes_the_question_back() ->
    meck:expect(beamai_chat_completion, create, fun(_, _) -> error(boom) end),
    Question = <<"这个问题不该被当成答案"/utf8>>,
    case query(Question) of
        {ok, Answer} ->
            ?assertEqual(nomatch, binary:match(Answer, Question)),
            ?assert(false);   %% 失败时根本就不该返回 {ok,_}
        {error, _} ->
            ok
    end.

%%====================================================================
%% 辅助
%%====================================================================

%% 建一个含单篇文档的 pipeline，然后 query —— 检索命中后才会调 LLM
query(Question) ->
    P0 = beamai_rag:new(#{llm => #{provider => openai, model => <<"m">>}}),
    {ok, _Id, P1} = beamai_rag:index_document(P0, <<"一些上下文内容"/utf8>>, #{}),
    case beamai_rag:query(P1, Question) of
        {ok, Answer, _P2} -> {ok, Answer};
        {error, Reason} -> {error, Reason}
    end.
