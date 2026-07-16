%%%-------------------------------------------------------------------
%%% @doc middleware_model_fallback 单元测试
%%%
%%% 与 middleware_model_retry 同源的历史缺陷：should_trigger_fallback/2
%%% 只匹配 2 元组和裸原子，真实 LLM 错误（3/4 元组）一律不触发降级。
%%%
%%% 降级触发面比"可重试"更宽：换模型能救的都触发（限流/5xx/超时/鉴权/模型不存在），
%%% 换模型救不了的不触发（如 400 请求本身有问题）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_fallback_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ERR_429,     {http_error, 429, <<"{}">>}).
-define(ERR_503,     {http_error, 503, <<"{}">>}).
-define(ERR_TIMEOUT, {request_failed, timeout}).
-define(ERR_401,     {http_error, 401, <<"{}">>}).
-define(ERR_404,     {http_error, 404, <<"model not found">>}).
-define(ERR_400,     {http_error, 400, <<"bad request">>}).

-define(MODELS, [<<"backup-1">>, <<"backup-2">>]).

%%====================================================================
%% 触发条件
%%====================================================================

%% 回归测试：这些以前一律不触发降级
triggers_on_real_errors_test_() ->
    [{lists:flatten(io_lib:format("~p 触发降级", [E])),
      ?_assertEqual(2, calls_for(E))}
     || E <- [?ERR_429, ?ERR_503, ?ERR_TIMEOUT]].

%% 鉴权失败/模型不存在：换模型（可能换 provider）有救，应触发
triggers_on_auth_and_model_not_found_test_() ->
    [{lists:flatten(io_lib:format("~p 触发降级", [E])),
      ?_assertEqual(2, calls_for(E))}
     || E <- [?ERR_401, ?ERR_404]].

%% 400：请求本身有问题，换模型也救不了
does_not_trigger_on_bad_request_test() ->
    ?assertEqual(1, calls_for(?ERR_400)).

legacy_atom_errors_still_trigger_test() ->
    ?assertEqual(2, calls_for(timeout)),
    ?assertEqual(2, calls_for(quota_exceeded)),
    ?assertEqual(1, calls_for(some_unknown_error)).

legacy_binary_errors_still_trigger_test() ->
    ?assertEqual(2, calls_for(<<"Rate limit exceeded">>)),
    ?assertEqual(1, calls_for(<<"totally unrelated">>)).

classification_never_crashes_test() ->
    ?assertEqual(1, calls_for({weird, #{}, [1,2,3], self()})),
    ?assertEqual(1, calls_for(make_ref())).

%%====================================================================
%% 降级链行为
%%====================================================================

success_passes_through_test() ->
    {Resp, Calls, _} = run_chat(#{fallback_models => ?MODELS}, [ok_resp()]),
    ?assertEqual(#{response => #{content => <<"hi">>}}, Resp),
    ?assertEqual(1, Calls).

%% 首个降级模型成功即停
stops_at_first_working_model_test() ->
    {Resp, Calls, Models} = run_chat(#{fallback_models => ?MODELS},
                                     [err_resp(?ERR_503), ok_resp()]),
    ?assertEqual(#{response => #{content => <<"hi">>}}, Resp),
    ?assertEqual(2, Calls),
    %% 第二次调用用的是第一个降级模型
    ?assertEqual([undefined, <<"backup-1">>], Models).

%% 逐个降级，用尽后返回最后的错误
exhausts_all_models_then_returns_error_test() ->
    {Resp, Calls, Models} = run_chat(#{fallback_models => ?MODELS},
                                     lists:duplicate(5, err_resp(?ERR_503))),
    ?assertEqual(#{response => {error, ?ERR_503}}, Resp),
    %% 主模型 + 2 个降级 = 3 次
    ?assertEqual(3, Calls),
    ?assertEqual([undefined, <<"backup-1">>, <<"backup-2">>], Models).

no_fallback_models_means_single_call_test() ->
    {_Resp, Calls, _} = run_chat(#{fallback_models => []},
                                 lists:duplicate(3, err_resp(?ERR_503))),
    ?assertEqual(1, Calls).

on_fallback_callback_invoked_test() ->
    Self = self(),
    Opts = #{fallback_models => ?MODELS,
             on_fallback => fun(From, To, E) -> Self ! {fb, From, To, E} end},
    run_chat(Opts, [err_resp(?ERR_503), ok_resp()]),
    receive {fb, From, To, E} ->
        ?assertEqual(undefined, From),
        ?assertEqual(<<"backup-1">>, To),
        ?assertEqual(?ERR_503, E)
    after 100 -> ?assert(false)
    end.

custom_trigger_errors_list_test() ->
    ?assertEqual(2, calls_for(my_error, #{trigger_errors => [my_error]})),
    ?assertEqual(1, calls_for(timeout, #{trigger_errors => [my_error]})),
    %% 上游可识别的错误不受自定义列表影响
    ?assertEqual(2, calls_for(?ERR_429, #{trigger_errors => [my_error]})).

%%====================================================================
%% 辅助
%%====================================================================

ok_resp() -> #{response => #{content => <<"hi">>}}.
err_resp(E) -> #{response => {error, E}}.

%% 该错误会导致多少次 Next 调用：1 = 未触发降级，2 = 触发了一次
calls_for(Error) -> calls_for(Error, #{}).

calls_for(Error, Extra) ->
    Opts = maps:merge(#{fallback_models => [<<"backup-1">>]}, Extra),
    {_Resp, Calls, _} = run_chat(Opts, [err_resp(Error), ok_resp()]),
    Calls.

%% 驱动 around_chat，返回 {最终响应, Next 调用次数, 每次调用所用的 model}
run_chat(Opts, Responses) ->
    Ref = make_ref(),
    put(Ref, {Responses, []}),
    Next = fun(Req) ->
        Model = maps:get(model, maps:get(opts, Req, #{}), undefined),
        {Rs, Ms} = get(Ref),
        {R, Rest} = case Rs of
            [Last] -> {Last, [Last]};        % 用尽后重复最后一个
            [H | T] -> {H, T}
        end,
        put(Ref, {Rest, Ms ++ [Model]}),
        R
    end,
    St = middleware_model_fallback:init(Opts),
    Result = middleware_model_fallback:around_chat(#{messages => []}, St, Next),
    Resp = case Result of
        {R2, _NewSt} -> R2;
        R2 -> R2
    end,
    {_, Models} = get(Ref),
    erase(Ref),
    {Resp, length(Models), Models}.
