%%%-------------------------------------------------------------------
%%% @doc 工具重试中间件（around 模型）
%%%
%%% 在工具调用失败时自动重试，支持指数/线性/固定退避策略。
%%% around_tool 包裹工具执行：调 Next → 检测错误 → 可重试则等待后重调 Next。
%%%
%%% **工具失败走的是 throw，不是返回值**：beamai_kernel 的 tool_terminal 里
%%% `{error, Reason} -> throw(Reason)`，由 beamai_filter_chain:run/4 在**最外层**
%%% 统一 catch 成 `{error, Reason}`。也就是说 Next(Req) 失败时直接抛异常穿透本
%%% 中间件，绝不会返回 `#{result => {error, _}}`。因此这里必须 catch throw 才
%%% 拿得到错误；不重试时要原样 re-throw，保持 kernel 的错误语义。
%%%
%%% 错误分类以 beamai_tool_error:classify/1 为准，默认只重试 transient
%%% （与上游 beamai_tool:invoke 的 retry 语义一致）——semantic 错误（如参数非法）
%%% 重试多少次都一样失败，只会白白放大副作用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_retry).

-behaviour(beamai_middleware).

-export([init/1, around_tool/3]).
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
        multiplier => 2
    },
    #{
        max_retries => maps:get(max_retries, Opts, 3),
        backoff => maps:merge(DefaultBackoff, maps:get(backoff, Opts, #{})),
        retryable_errors => maps:get(retryable_errors, Opts, auto),
        retry_fn => maps:get(retry_fn, Opts, undefined),
        on_retry => maps:get(on_retry, Opts, undefined),
        enable_delay => maps:get(enable_delay, Opts, true),
        retry_count => 0
    }.

%% @doc 包裹工具执行：失败时按退避策略重试。
-spec around_tool(beamai_middleware:request(), map(), beamai_middleware:next()) ->
    beamai_middleware:response() | {beamai_middleware:response(), map()}.
around_tool(Req, State, Next) ->
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
%%
%% 两条失败路径都要管：
%%   1. throw —— kernel tool_terminal 抛出的真实工具错误（主路径）
%%   2. 返回值里带 error —— 内层 filter 短路时可能这么返回（兜底）
do_retry(Req, State, Next, Attempt) ->
    try Next(Req) of
        Resp ->
            case extract_error(Resp) of
                undefined -> {Resp, State#{retry_count => Attempt}};
                Error -> maybe_retry(Error, Req, State, Next, Attempt,
                                     fun() -> {Resp, State#{retry_count => Attempt}} end)
            end
    catch
        throw:Reason ->
            %% 放弃重试时必须原样抛回，让 filter_chain 收敛成 {error, Reason}
            maybe_retry(Reason, Req, State, Next, Attempt,
                        fun() -> throw(Reason) end)
    end.

%% @private 决定重试还是收场；GiveUp 由调用方决定（返回响应 or re-throw）
maybe_retry(Error, Req, State, Next, Attempt, GiveUp) ->
    #{max_retries := MaxRetries,
      backoff := Backoff,
      on_retry := OnRetry,
      enable_delay := EnableDelay} = State,
    case Attempt < MaxRetries andalso is_retryable(Error, State, Attempt) of
        true ->
            NextAttempt = Attempt + 1,
            Delay = calculate_delay(NextAttempt, Backoff),
            maybe_call_on_retry(OnRetry, tool_name(Req), Error, NextAttempt, Delay),
            case EnableDelay andalso Delay > 0 of
                true -> timer:sleep(Delay);
                false -> ok
            end,
            do_retry(Req, State, Next, NextAttempt);
        false ->
            GiveUp()
    end.

%% @private around_tool 的 Req 里 tool 是 tool_spec，不是名字
tool_name(#{tool := ToolSpec}) when is_map(ToolSpec) ->
    beamai_tool:get_name(ToolSpec);
tool_name(#{tool := Name}) -> Name;
tool_name(_) -> <<"unknown">>.

%% @private 从 tool 响应中提取错误
extract_error(#{result := {error, Error}}) -> Error;
extract_error(#{result := Error}) when is_atom(Error), Error =/= ok -> Error;
extract_error(_) -> undefined.

%% auto（缺省）：交给上游分类器，只重试 transient
is_retryable_default(Error, auto) ->
    classify(Error) =:= transient;
%% all：无条件重试（仅在确认工具幂等时使用）
is_retryable_default(_Error, all) -> true;
%% 显式原子列表：匹配自定义错误
is_retryable_default({ErrorType, _}, RetryableErrors) when is_atom(ErrorType), is_list(RetryableErrors) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorType, RetryableErrors) when is_atom(ErrorType), is_list(RetryableErrors) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(_, _) -> false.

%% @private 归一化工具错误类别；分类器异常时按 semantic（不重试）处理
classify(Error) ->
    try beamai_tool_error:classify(Error)
    catch _:_ -> semantic
    end.

maybe_call_on_retry(undefined, _, _, _, _) -> ok;
maybe_call_on_retry(OnRetry, ToolName, Error, RetryCount, Delay) when is_function(OnRetry, 4) ->
    try OnRetry(ToolName, Error, RetryCount, Delay)
    catch _:Reason -> logger:warning("Retry callback error: ~p", [Reason])
    end.
