%%%-------------------------------------------------------------------
%%% @doc 模型重试中间件（around 模型）
%%%
%%% 在 LLM 调用失败时自动重试，支持退避策略和随机抖动。
%%% around_chat 包裹 LLM 调用：调 Next → 检测错误 → 可重试则等待后重调 Next。
%%%
%%% 错误分类以 beamai_llm_error 为准：真实 LLM 错误形如
%%% `{http_error, 429, Body}`、`{http_error, 503, Body, #{retry_after_ms => Ms}}`、
%%% `{request_failed, timeout}`，由 beamai_llm_error:from_reason/1 归一化后
%%% 用 retryable/1 判定。retryable_errors 原子列表仅作为兜底，匹配上游
%%% 无法识别的自定义错误（如测试 mock）。
%%%
%%% 服务端给出 Retry-After 时优先采用该值（上限 backoff 的 max_delay），
%%% 否则按退避策略计算。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_retry).

-behaviour(beamai_middleware).

-export([init/1, around_chat/3]).
-export([is_retryable/2, is_retryable/3, calculate_delay/2]).

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    DefaultBackoff = #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2,
        jitter => true
    },
    #{
        max_retries => maps:get(max_retries, Opts, 3),
        backoff => maps:merge(DefaultBackoff, maps:get(backoff, Opts, #{})),
        retryable_errors => maps:get(retryable_errors, Opts, default_retryable_errors()),
        retry_fn => maps:get(retry_fn, Opts, undefined),
        on_retry => maps:get(on_retry, Opts, undefined),
        respect_retry_after => maps:get(respect_retry_after, Opts, true),
        retry_count => 0
    }.

%% @doc 包裹 LLM 调用：失败时按退避策略重试。
-spec around_chat(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_chat(Req, State, Next) ->
    do_retry(Req, State, Next, 0).

%%====================================================================
%% 公共辅助函数
%%%-------------------------------------------------------------------

-spec is_retryable(term(), map()) -> boolean().
is_retryable(Error, State) ->
    is_retryable(Error, State, 0).

%% @doc 判定错误是否可重试。Attempt 为已完成的重试次数，透传给自定义 retry_fn。
-spec is_retryable(term(), map(), non_neg_integer()) -> boolean().
is_retryable(Error, #{retryable_errors := RetryableErrors, retry_fn := RetryFn}, Attempt) ->
    case RetryFn of
        undefined -> is_retryable_default(Error, RetryableErrors);
        Fn when is_function(Fn, 2) ->
            try Fn(Error, Attempt) catch _:_ -> false end
    end.

-spec calculate_delay(pos_integer(), map()) -> pos_integer().
calculate_delay(RetryCount, #{type := Type, initial_delay := InitialDelay, max_delay := MaxDelay} = Config) ->
    BaseDelay = case Type of
        constant -> InitialDelay;
        linear -> InitialDelay * RetryCount;
        exponential ->
            Multiplier = maps:get(multiplier, Config, 2),
            round(InitialDelay * math:pow(Multiplier, RetryCount - 1))
    end,
    DelayWithJitter = case maps:get(jitter, Config, false) of
        true ->
            Jitter = BaseDelay * 0.25,
            round(BaseDelay + (rand:uniform() * 2 - 1) * Jitter);
        false ->
            BaseDelay
    end,
    min(max(DelayWithJitter, 0), MaxDelay).

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

default_retryable_errors() ->
    [timeout, connection_error, connection_closed, rate_limit,
     rate_limited, server_error, service_unavailable,
     internal_server_error, bad_gateway, gateway_timeout,
     econnrefused, econnreset, etimedout].

do_retry(Req, State, Next, Attempt) ->
    #{max_retries := MaxRetries,
      backoff := Backoff,
      on_retry := OnRetry} = State,

    Resp = Next(Req),

    case extract_error(Resp) of
        undefined ->
            {Resp, State#{retry_count => Attempt}};
        Error when Attempt < MaxRetries ->
            case is_retryable(Error, State, Attempt) of
                true ->
                    NextAttempt = Attempt + 1,
                    Delay = retry_delay(Error, NextAttempt, Backoff, State),
                    maybe_call_on_retry(OnRetry, Error, NextAttempt, Delay),
                    timer:sleep(Delay),
                    do_retry(Req, State, Next, NextAttempt);
                false ->
                    {Resp, State#{retry_count => Attempt}}
            end;
        _Error ->
            {Resp, State#{retry_count => Attempt}}
    end.

%% @private 从 chat 响应中提取错误
extract_error(#{response := {error, Error}}) -> Error;
extract_error(#{response := #{error := Error}}) -> Error;
extract_error(#{response := Error}) when is_atom(Error), Error =/= ok -> Error;
extract_error(_) -> undefined.

%% @private 先问上游分类器，识别不了再走兜底列表
is_retryable_default(Error, RetryableErrors) ->
    case classify(Error) of
        undefined -> matches_legacy(Error, RetryableErrors);
        LlmError ->
            case beamai_llm_error:retryable(LlmError) of
                true -> true;
                false -> matches_legacy(Error, RetryableErrors)
            end
    end.

%% @private 归一化为 beamai_llm_error；无法分类时返回 undefined（中间件不应因分类崩溃）
classify(Error) ->
    try beamai_llm_error:from_reason(Error)
    catch _:_ -> undefined
    end.

%% @private 兜底：匹配自定义原子/文本错误（上游 LLM 错误已在上面判完）
matches_legacy({ErrorType, _}, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
matches_legacy(ErrorType, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
matches_legacy(ErrorBin, _) when is_binary(ErrorBin) ->
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error", "503", "502", "504"]);
matches_legacy(_, _) -> false.

%% @private 服务端 Retry-After 优先，上限 max_delay；否则按退避策略
retry_delay(Error, Attempt, Backoff, #{respect_retry_after := true}) ->
    case server_retry_after_ms(Error) of
        undefined -> calculate_delay(Attempt, Backoff);
        Ms -> min(Ms, maps:get(max_delay, Backoff, Ms))
    end;
retry_delay(_Error, Attempt, Backoff, _State) ->
    calculate_delay(Attempt, Backoff).

%% @private 从错误中取服务端 Retry-After（毫秒）
server_retry_after_ms(Error) ->
    case classify(Error) of
        undefined -> undefined;
        LlmError -> beamai_llm_error:retry_after_ms(LlmError)
    end.

maybe_call_on_retry(undefined, _, _, _) -> ok;
maybe_call_on_retry(OnRetry, Error, RetryCount, Delay) when is_function(OnRetry, 3) ->
    try OnRetry(Error, RetryCount, Delay) catch _:R -> logger:warning("Retry callback error: ~p", [R]) end.
