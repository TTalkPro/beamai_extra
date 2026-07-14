%%%-------------------------------------------------------------------
%%% @doc 工具重试中间件（around 模型）
%%%
%%% 在工具调用失败时自动重试，支持指数/线性/固定退避策略。
%%% around_tool 包裹工具执行：调 Next → 检测错误 → 可重试则等待后重调 Next。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_retry).

-behaviour(beamai_middleware).

-export([init/1, around_tool/3]).
-export([is_retryable/2, calculate_delay/2]).

%%====================================================================
%% 中间件回调
%%%-------------------------------------------------------------------

-spec init(map()) -> map().
init(Opts) ->
    DefaultBackoff = #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2
    },
    #{
        max_retries => maps:get(max_retries, Opts, 3),
        backoff => maps:merge(DefaultBackoff, maps:get(backoff, Opts, #{})),
        retryable_errors => maps:get(retryable_errors, Opts, all),
        retry_fn => maps:get(retry_fn, Opts, undefined),
        on_retry => maps:get(on_retry, Opts, undefined),
        enable_delay => maps:get(enable_delay, Opts, true),
        retry_count => 0
    }.

%% @doc 包裹工具执行：失败时按退避策略重试。
-spec around_tool(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_tool(Req, State, Next) ->
    Result = do_retry(Req, State, Next, 0),
    Result.

%%====================================================================
%% 公共辅助函数
%%%-------------------------------------------------------------------

-spec is_retryable(term(), map()) -> boolean().
is_retryable(Error, #{retryable_errors := RetryableErrors, retry_fn := RetryFn}) ->
    case RetryFn of
        undefined -> is_retryable_default(Error, RetryableErrors);
        Fn when is_function(Fn, 2) ->
            try Fn(Error, 0) catch _:_ -> false end
    end.

-spec calculate_delay(pos_integer(), map()) -> pos_integer().
calculate_delay(RetryCount, #{type := Type, initial_delay := InitialDelay, max_delay := MaxDelay} = Config) ->
    Delay = case Type of
        constant -> InitialDelay;
        linear -> InitialDelay * RetryCount;
        exponential ->
            Multiplier = maps:get(multiplier, Config, 2),
            round(InitialDelay * math:pow(Multiplier, RetryCount - 1))
    end,
    min(Delay, MaxDelay).

%%====================================================================
%% 内部函数
%%%-------------------------------------------------------------------

%% @private 递归重试
do_retry(Req, State, Next, Attempt) ->
    #{max_retries := MaxRetries,
      backoff := Backoff,
      on_retry := OnRetry,
      enable_delay := EnableDelay} = State,

    Resp = Next(Req),

    case extract_error(Resp) of
        undefined ->
            {Resp, State#{retry_count => Attempt}};
        Error when Attempt < MaxRetries ->
            case is_retryable(Error, State) of
                true ->
                    NextAttempt = Attempt + 1,
                    Delay = calculate_delay(NextAttempt, Backoff),
                    maybe_call_on_retry(OnRetry, maps:get(tool, Req, <<"unknown">>), Error, NextAttempt, Delay),
                    case EnableDelay andalso Delay > 0 of
                        true -> timer:sleep(Delay);
                        false -> ok
                    end,
                    do_retry(Req, State, Next, NextAttempt);
                false ->
                    {Resp, State#{retry_count => Attempt}}
            end;
        _Error ->
            %% 不可重试或已耗尽，返回原始错误响应
            {Resp, State#{retry_count => Attempt}}
    end.

%% @private 从 tool 响应中提取错误
extract_error(#{result := {error, Error}}) -> Error;
extract_error(#{result := Error}) when is_atom(Error), Error =/= ok -> Error;
extract_error(_) -> undefined.

is_retryable_default(_Error, all) -> true;
is_retryable_default({ErrorType, _}, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorType, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(_, _) -> false.

maybe_call_on_retry(undefined, _, _, _, _) -> ok;
maybe_call_on_retry(OnRetry, ToolName, Error, RetryCount, Delay) when is_function(OnRetry, 4) ->
    try OnRetry(ToolName, Error, RetryCount, Delay)
    catch _:Reason -> logger:warning("Retry callback error: ~p", [Reason])
    end.
