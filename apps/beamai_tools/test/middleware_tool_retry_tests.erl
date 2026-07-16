%%%-------------------------------------------------------------------
%%% @doc middleware_tool_retry 单元测试
%%%
%%% 历史缺陷：工具失败走的是 **throw**（beamai_kernel tool_terminal 里
%%% `{error,Reason} -> throw(Reason)`），而 do_retry 只看 Next(Req) 的**返回值**，
%%% 异常直接穿透中间件——max_retries 配多少都只调用一次，重试从未发生。
%%%
%%% 这里大量用真实 kernel 跑：throw 路径只有真 kernel 才复现得出来，
%%% 用假 Next 会把 bug 测没了。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 真实 kernel 下的重试行为（回归测试）
%%====================================================================

%% 以前恒为 1：throw 穿透，从不重试
retries_transient_error_test() ->
    {Result, Calls} = run_tool(timeout, #{max_retries => 3}),
    ?assertEqual({error, timeout}, Result),
    ?assertEqual(4, Calls).

%% 放弃重试时必须 re-throw，保持 kernel 的 {error, Reason} 语义
preserves_error_semantics_after_exhaustion_test() ->
    {Result, _} = run_tool({request_failed, econnrefused}, #{max_retries => 1}),
    ?assertEqual({error, {request_failed, econnrefused}}, Result).

%% semantic 错误重试多少次都一样，auto 下不该重试
does_not_retry_semantic_error_test() ->
    {Result, Calls} = run_tool(invalid_argument, #{max_retries => 3}),
    ?assertEqual({error, invalid_argument}, Result),
    ?assertEqual(1, Calls).

%% 成功不受影响，且不吞掉结果
success_is_not_retried_test() ->
    Counter = counters:new(1, []),
    Handler = fun(_) -> counters:add(Counter, 1, 1), {ok, <<"ok">>} end,
    Result = invoke(Handler, #{max_retries => 3}),
    ?assertEqual({ok, <<"ok">>, #{}}, Result),
    ?assertEqual(1, counters:get(Counter, 1)).

%% 先失败后成功：重试真的救回来了
recovers_after_transient_failure_test() ->
    Counter = counters:new(1, []),
    Handler = fun(_) ->
        case counters:get(Counter, 1) of
            N when N < 2 -> counters:add(Counter, 1, 1), {error, timeout};
            _ -> counters:add(Counter, 1, 1), {ok, <<"recovered">>}
        end
    end,
    Result = invoke(Handler, #{max_retries => 3}),
    ?assertEqual({ok, <<"recovered">>, #{}}, Result),
    ?assertEqual(3, counters:get(Counter, 1)).

all_mode_retries_semantic_errors_test() ->
    {_, Calls} = run_tool(invalid_argument, #{max_retries => 2, retryable_errors => all}),
    ?assertEqual(3, Calls).

explicit_error_class_is_honored_test() ->
    {_, Transient} = run_tool(#{error_class => transient}, #{max_retries => 2}),
    ?assertEqual(3, Transient),
    {_, Semantic} = run_tool(#{error_class => semantic}, #{max_retries => 2}),
    ?assertEqual(1, Semantic).

%%====================================================================
%% 分类
%%====================================================================

auto_classification_test_() ->
    St = middleware_tool_retry:init(#{}),
    Transient = [timeout, tool_timeout, tool_worker_crash,
                 {request_failed, econnrefused}, {timeout, x}, {closed, tcp}],
    Semantic = [invalid_argument, {badmatch, x}, some_random_error],
    [{"transient 可重试", [?_assert(middleware_tool_retry:is_retryable(E, St)) || E <- Transient]},
     {"semantic 不重试", [?_assertNot(middleware_tool_retry:is_retryable(E, St)) || E <- Semantic]}].

all_mode_retries_everything_test() ->
    St = middleware_tool_retry:init(#{retryable_errors => all}),
    ?assert(middleware_tool_retry:is_retryable(invalid_argument, St)),
    ?assert(middleware_tool_retry:is_retryable(anything_at_all, St)).

explicit_list_mode_test() ->
    St = middleware_tool_retry:init(#{retryable_errors => [my_error]}),
    ?assert(middleware_tool_retry:is_retryable(my_error, St)),
    ?assert(middleware_tool_retry:is_retryable({my_error, detail}, St)),
    ?assertNot(middleware_tool_retry:is_retryable(timeout, St)).

retry_fn_receives_attempt_number_test() ->
    Self = self(),
    St = middleware_tool_retry:init(#{
        retry_fn => fun(_E, Attempt) -> Self ! {attempt, Attempt}, false end
    }),
    middleware_tool_retry:is_retryable(timeout, St, 3),
    receive {attempt, A} -> ?assertEqual(3, A)
    after 100 -> ?assert(false)
    end.

retry_fn_crash_is_not_retryable_test() ->
    St = middleware_tool_retry:init(#{retry_fn => fun(_, _) -> error(boom) end}),
    ?assertNot(middleware_tool_retry:is_retryable(timeout, St)).

classification_never_crashes_test() ->
    St = middleware_tool_retry:init(#{}),
    ?assertNot(middleware_tool_retry:is_retryable(make_ref(), St)).

%%====================================================================
%% on_retry 回调
%%====================================================================

%% 回调拿到的必须是工具名，不是整个 tool_spec map
on_retry_receives_tool_name_test() ->
    Self = self(),
    Opts = #{max_retries => 1, enable_delay => false,
             on_retry => fun(Name, E, N, D) -> Self ! {retried, Name, E, N, D} end},
    run_tool(timeout, Opts),
    receive {retried, Name, E, N, _D} ->
        ?assertEqual(<<"flaky">>, Name),
        ?assertEqual(timeout, E),
        ?assertEqual(1, N)
    after 200 -> ?assert(false)
    end.

%%====================================================================
%% 辅助
%%====================================================================

%% 跑一个恒定失败的工具，返回 {invoke_tool 结果, handler 实际调用次数}
run_tool(Error, Opts) ->
    Counter = counters:new(1, []),
    Handler = fun(_) -> counters:add(Counter, 1, 1), {error, Error} end,
    Result = invoke(Handler, Opts),
    {Result, counters:get(Counter, 1)}.

%% 用真实 kernel + tool_retry filter 调用工具
invoke(Handler, Opts0) ->
    Opts = maps:merge(#{enable_delay => false}, Opts0),
    Tool = beamai_tool:new(<<"flaky">>, Handler, #{description => <<"test tool">>}),
    Chain = beamai_middleware_runner:init([{middleware_tool_retry, Opts}]),
    Kernel = beamai_kernel:add_tool(
               beamai_kernel:new(#{}, beamai_middleware_runner:to_filters(Chain)), Tool),
    beamai_kernel:invoke_tool(Kernel, <<"flaky">>, #{}, beamai_context:new()).
