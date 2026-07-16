%%%-------------------------------------------------------------------
%%% @doc middleware_model_retry 单元测试
%%%
%%% 重点：真实 LLM 错误形状必须被识别为可重试。历史上 is_retryable_default/2
%%% 只匹配 2 元组和裸原子，而 beamai_llm 实际抛出的是
%%% {http_error, Code, Body} / {http_error, Code, Body, Meta} / {request_failed, timeout}，
%%% 导致 429、5xx、超时全部不重试——中间件形同虚设。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%% 真实错误形状（取自 beamai_llm_http_client / beamai_llm_error:classify/2）
-define(ERR_429,        {http_error, 429, <<"{}">>}).
-define(ERR_429_RA(Ms), {http_error, 429, <<"{}">>, #{retry_after_ms => Ms}}).
-define(ERR_503,        {http_error, 503, <<"{}">>}).
-define(ERR_500,        {http_error, 500, <<"{}">>}).
-define(ERR_TIMEOUT,    {request_failed, timeout}).
-define(ERR_CLOSED,     {request_failed, {closed, tcp}}).
-define(ERR_401,        {http_error, 401, <<"{}">>}).
-define(ERR_400,        {http_error, 400, <<"{}">>}).

%%====================================================================
%% 分类：真实 LLM 错误
%%====================================================================

%% 回归测试：这些以前全部返回 false
retryable_real_errors_test_() ->
    St = middleware_model_retry:init(#{}),
    [{lists:flatten(io_lib:format("~p 可重试", [E])),
      ?_assert(middleware_model_retry:is_retryable(E, St))}
     || E <- [?ERR_429, ?ERR_429_RA(1000), ?ERR_503, ?ERR_500, ?ERR_TIMEOUT, ?ERR_CLOSED]].

non_retryable_real_errors_test_() ->
    St = middleware_model_retry:init(#{}),
    [{lists:flatten(io_lib:format("~p 不可重试", [E])),
      ?_assertNot(middleware_model_retry:is_retryable(E, St))}
     || E <- [?ERR_401, ?ERR_400, {http_error, 403, <<"{}">>}]].

%% 兜底列表仍需生效：自定义原子/文本错误上游认不出来
legacy_atom_errors_still_retryable_test() ->
    St = middleware_model_retry:init(#{}),
    ?assert(middleware_model_retry:is_retryable(timeout, St)),
    ?assert(middleware_model_retry:is_retryable(rate_limit, St)),
    ?assert(middleware_model_retry:is_retryable({connection_error, detail}, St)),
    ?assertNot(middleware_model_retry:is_retryable(some_unknown_error, St)).

legacy_binary_errors_still_retryable_test() ->
    St = middleware_model_retry:init(#{}),
    ?assert(middleware_model_retry:is_retryable(<<"Rate limit exceeded">>, St)),
    ?assertNot(middleware_model_retry:is_retryable(<<"invalid request body">>, St)).

custom_retryable_errors_list_test() ->
    St = middleware_model_retry:init(#{retryable_errors => [my_error]}),
    ?assert(middleware_model_retry:is_retryable(my_error, St)),
    ?assertNot(middleware_model_retry:is_retryable(timeout, St)),
    %% 上游可识别的错误不受自定义列表影响
    ?assert(middleware_model_retry:is_retryable(?ERR_429, St)).

classification_never_crashes_test() ->
    St = middleware_model_retry:init(#{}),
    ?assertNot(middleware_model_retry:is_retryable({weird, #{}, [1,2,3], self()}, St)),
    ?assertNot(middleware_model_retry:is_retryable(make_ref(), St)).

%%====================================================================
%% retry_fn 覆盖
%%====================================================================

retry_fn_overrides_default_test() ->
    St = middleware_model_retry:init(#{retry_fn => fun(_E, _A) -> false end}),
    %% 429 默认可重试，retry_fn 说不行就不行
    ?assertNot(middleware_model_retry:is_retryable(?ERR_429, St)).

retry_fn_receives_attempt_number_test() ->
    Self = self(),
    St = middleware_model_retry:init(#{
        retry_fn => fun(_E, Attempt) -> Self ! {attempt, Attempt}, false end
    }),
    middleware_model_retry:is_retryable(?ERR_429, St, 2),
    receive {attempt, A} -> ?assertEqual(2, A)
    after 100 -> ?assert(false)
    end.

retry_fn_crash_is_not_retryable_test() ->
    St = middleware_model_retry:init(#{retry_fn => fun(_E, _A) -> error(boom) end}),
    ?assertNot(middleware_model_retry:is_retryable(?ERR_429, St)).

%%====================================================================
%% around_chat 重试行为
%%====================================================================

success_passes_through_without_retry_test() ->
    {Resp, Calls} = run_chat(#{}, [ok_resp()]),
    ?assertEqual(#{response => #{content => <<"hi">>}}, Resp),
    ?assertEqual(1, Calls).

%% 回归测试：429 以前一次都不重试
retries_429_until_success_test() ->
    {Resp, Calls} = run_chat(fast_backoff(#{}),
                             [err_resp(?ERR_429), err_resp(?ERR_429), ok_resp()]),
    ?assertEqual(#{response => #{content => <<"hi">>}}, Resp),
    ?assertEqual(3, Calls).

does_not_retry_auth_error_test() ->
    {_Resp, Calls} = run_chat(fast_backoff(#{}), [err_resp(?ERR_401), ok_resp()]),
    ?assertEqual(1, Calls).

stops_at_max_retries_test() ->
    %% max_retries=2 → 首次 + 2 次重试 = 3 次调用，最终仍返回错误
    {Resp, Calls} = run_chat(fast_backoff(#{max_retries => 2}),
                             lists:duplicate(5, err_resp(?ERR_503))),
    ?assertEqual(#{response => {error, ?ERR_503}}, Resp),
    ?assertEqual(3, Calls).

on_retry_callback_invoked_test() ->
    Self = self(),
    Opts = fast_backoff(#{max_retries => 1,
                          on_retry => fun(E, N, D) -> Self ! {retried, E, N, D} end}),
    run_chat(Opts, [err_resp(?ERR_503), ok_resp()]),
    receive {retried, E, N, D} ->
        ?assertEqual(?ERR_503, E),
        ?assertEqual(1, N),
        ?assert(is_integer(D))
    after 100 -> ?assert(false)
    end.

%%====================================================================
%% Retry-After
%%====================================================================

%% 服务端 Retry-After 优先于退避策略
honors_server_retry_after_test() ->
    Opts = #{max_retries => 1,
             backoff => #{type => constant, initial_delay => 5000,
                          max_delay => 30000, jitter => false}},
    {Elapsed, _} = timer:tc(fun() ->
        run_chat(Opts, [err_resp(?ERR_429_RA(120)), ok_resp()])
    end),
    ElapsedMs = Elapsed div 1000,
    %% 用了 Retry-After 的 120ms，而不是退避的 5000ms
    ?assert(ElapsedMs >= 100),
    ?assert(ElapsedMs < 1500).

%% Retry-After 受 max_delay 上限约束，防止被服务端拖死
clamps_retry_after_to_max_delay_test() ->
    Opts = #{max_retries => 1,
             backoff => #{type => constant, initial_delay => 1,
                          max_delay => 100, jitter => false}},
    {Elapsed, _} = timer:tc(fun() ->
        run_chat(Opts, [err_resp(?ERR_429_RA(60000)), ok_resp()])
    end),
    ?assert(Elapsed div 1000 < 1500).

respect_retry_after_can_be_disabled_test() ->
    Opts = #{max_retries => 1, respect_retry_after => false,
             backoff => #{type => constant, initial_delay => 1,
                          max_delay => 100, jitter => false}},
    {Elapsed, _} = timer:tc(fun() ->
        run_chat(Opts, [err_resp(?ERR_429_RA(60000)), ok_resp()])
    end),
    %% 忽略 Retry-After，走 1ms 退避
    ?assert(Elapsed div 1000 < 1000).

%%====================================================================
%% calculate_delay
%%====================================================================

calculate_delay_test() ->
    Cfg = #{type => constant, initial_delay => 100, max_delay => 1000},
    ?assertEqual(100, middleware_model_retry:calculate_delay(1, Cfg)),
    Lin = #{type => linear, initial_delay => 100, max_delay => 1000},
    ?assertEqual(300, middleware_model_retry:calculate_delay(3, Lin)),
    Exp = #{type => exponential, initial_delay => 100, max_delay => 1000, multiplier => 2},
    ?assertEqual(100, middleware_model_retry:calculate_delay(1, Exp)),
    ?assertEqual(400, middleware_model_retry:calculate_delay(3, Exp)).

calculate_delay_caps_at_max_test() ->
    Exp = #{type => exponential, initial_delay => 100, max_delay => 250, multiplier => 2},
    ?assertEqual(250, middleware_model_retry:calculate_delay(10, Exp)).

%%====================================================================
%% 辅助
%%====================================================================

ok_resp() -> #{response => #{content => <<"hi">>}}.
err_resp(E) -> #{response => {error, E}}.

%% 退避压到 1ms，避免测试真的睡几秒
fast_backoff(Opts) ->
    Opts#{backoff => #{type => constant, initial_delay => 1,
                       max_delay => 10, jitter => false}}.

%% 用一串预设响应驱动 around_chat，返回 {最终响应, Next 调用次数}
run_chat(Opts, Responses) ->
    Ref = make_ref(),
    put(Ref, Responses),
    Next = fun(_Req) ->
        case get(Ref) of
            [R] -> put(Ref, [R]), R;          % 用尽后重复最后一个
            [R | Rest] -> put(Ref, Rest), R
        end
    end,
    Counter = counters:new(1, []),
    CountingNext = fun(Req) -> counters:add(Counter, 1, 1), Next(Req) end,
    St = middleware_model_retry:init(Opts),
    Result = middleware_model_retry:around_chat(#{messages => []}, St, CountingNext),
    Resp = case Result of
        {R, _NewSt} -> R;
        R -> R
    end,
    erase(Ref),
    {Resp, counters:get(Counter, 1)}.
