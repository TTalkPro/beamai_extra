%%%-------------------------------------------------------------------
%%% @doc 工具超时声明的契约测试
%%%
%%% 上游 680a395 起，beamai_tool:invoke/3 把 handler 跑在受监控子进程里并到点
%%% 强杀；c8dca82 之后**缺省不限时**——只有声明了 timeout（tool_spec 或
%%% manager 注入）才真的限时，不声明就无限等待。
%%%
%%% 于是「声明」成了唯一的护栏，这里逐个锁住：
%%%   - file/todo：毫秒级操作，必须声明（不声明 = 卡死时永远挂着）
%%%   - shell_execute：显式 infinity（长命令要跑到底，由 handler 按次管 deadline）
%%%   - 声明了的必须真的生效，且 tool_spec 优先于 manager 缺省
%%%
%%% 另锁超时错误与 tool_retry 的交互（tool_timeout 归 transient，会被重试）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 每个工具的 timeout 声明
%%====================================================================

%% 人机交互工具已改为 interrupt tool，不再注册进 kernel，也就不存在"每工具超时"
%% 这回事——拦截发生在执行之前。这里守住它不会悄悄变回 kernel 工具。
human_tools_are_not_kernel_tools_test() ->
    code:ensure_loaded(beamai_tool_human),
    ?assertNot(lists:member(beamai_tool_human, beamai_tools:available())),
    ?assertNot(erlang:function_exported(beamai_tool_human, tools, 0)),
    ?assert(erlang:function_exported(beamai_tool_human, interrupt_tools, 0)).

%% shell 自己按次管 deadline，故让出上游的每工具上限
shell_declares_infinity_test() ->
    ?assertEqual(infinity, timeout_of(beamai_tool_shell, <<"shell_execute">>)).

%% 其余工具必须显式声明——缺省已是 infinity，不声明就是无限等待，没有兜底
all_other_tools_declare_explicit_timeout_test_() ->
    Specs = beamai_tool_file:tools() ++ beamai_tool_todo:tools(),
    [{binary_to_list(maps:get(name, S)) ++ " 显式声明了 timeout",
      ?_assert(is_integer(maps:get(timeout, S, undefined)))}
     || S <- Specs].

%% 遍历类要比单文件操作宽
scan_tools_get_longer_budget_test() ->
    Read = timeout_of(beamai_tool_file, <<"file_read">>),
    Grep = timeout_of(beamai_tool_file, <<"file_grep">>),
    Glob = timeout_of(beamai_tool_file, <<"file_glob">>),
    ?assert(Grep > Read),
    ?assert(Glob > Read).

%%====================================================================
%% 声明真的生效（跑真 kernel）
%%====================================================================

%% infinity 不被任何东西管辖（现在也是缺省；这里验证显式声明形式同样生效）
infinity_is_not_capped_test() ->
    Spec = beamai_tool:new(<<"slow">>, fun(_) -> timer:sleep(200), {ok, done} end,
                           #{description => <<"d">>, timeout => infinity}),
    ?assertEqual({ok, done}, beamai_tool:invoke(Spec, #{}, beamai_context:new())).

%% 声明的 timeout 是硬上限，到点强杀
declared_timeout_is_enforced_test() ->
    Spec = beamai_tool:new(<<"slow">>, fun(_) -> timer:sleep(500), {ok, done} end,
                           #{description => <<"d">>, timeout => 50}),
    ?assertMatch({error, #{reason := tool_timeout, timeout_ms := 50}},
                 beamai_tool:invoke(Spec, #{}, beamai_context:new())).

%% tool_spec 声明优先于 manager 经 context 注入的缺省
declared_timeout_beats_manager_default_test() ->
    Ctx = beamai_context:with_default_tool_timeout(beamai_context:new(), 50),
    Spec = beamai_tool:new(<<"t">>, fun(_) -> timer:sleep(200), {ok, done} end,
                           #{description => <<"d">>, timeout => 5000}),
    ?assertEqual({ok, done}, beamai_tool:invoke(Spec, #{}, Ctx)).

%% 未声明时才回落到 manager 缺省
manager_default_applies_when_undeclared_test() ->
    Ctx = beamai_context:with_default_tool_timeout(beamai_context:new(), 50),
    Spec = beamai_tool:new(<<"t">>, fun(_) -> timer:sleep(300), {ok, done} end,
                           #{description => <<"d">>}),
    ?assertMatch({error, #{reason := tool_timeout}}, beamai_tool:invoke(Spec, #{}, Ctx)).

%%====================================================================
%% 超时 × tool_retry 中间件
%%====================================================================

%% 上游的超时错误是 map（#{class,reason,timeout_ms,stacktrace}），不是裸原子；
%% classify/1 递归解包后应为 transient，故 auto 模式会重试
timeout_error_is_transient_test() ->
    Err = #{class => timeout, reason => tool_timeout, timeout_ms => 100, stacktrace => []},
    ?assertEqual(transient, beamai_tool_error:classify(Err)),
    ?assert(middleware_tool_retry:is_retryable(Err, middleware_tool_retry:init(#{}))).

%% 端到端：超时工具经 tool_retry 会被重试
timeout_is_retried_by_middleware_test() ->
    Counter = counters:new(1, []),
    Handler = fun(_) -> counters:add(Counter, 1, 1), timer:sleep(300), {ok, never} end,
    Tool = beamai_tool:new(<<"slow">>, Handler, #{description => <<"d">>, timeout => 50}),
    Chain = beamai_middleware_runner:init([
        {middleware_tool_retry, #{max_retries => 2, enable_delay => false}}
    ]),
    K = beamai_kernel:add_tool(
          beamai_kernel:new(#{}, beamai_middleware_runner:to_filters(Chain)), Tool),
    ?assertMatch({error, _}, beamai_kernel:invoke_tool(K, <<"slow">>, #{}, beamai_context:new())),
    ?assertEqual(3, counters:get(Counter, 1)).

%%====================================================================
%% shell：deadline 由 handler 按次管
%%====================================================================

%% LLM 自愿设的短 timeout 真的生效（以前 collect_output 写死 60s，会拖到 60s）
shell_honors_short_timeout_test() ->
    {Elapsed, Result} = timer:tc(fun() ->
        beamai_tool_shell:handle_execute(
          #{<<"command">> => <<"sleep 10">>, <<"timeout">> => 300}, #{})
    end),
    ?assertMatch({error, {timeout, 300}}, Result),
    ?assert(Elapsed div 1000 < 3000).

%% 不传 timeout = 不设限，命令照常跑完（不会被我们自己造的死线截断）
shell_without_timeout_runs_to_completion_test() ->
    ?assertMatch({ok, _},
                 beamai_tool_shell:handle_execute(#{<<"command">> => <<"sleep 0.3">>}, #{})).

%% 非法 timeout 当没给 = 不设限，不崩也不凭空造死线
shell_invalid_timeout_means_no_limit_test() ->
    ?assertMatch({ok, _},
                 beamai_tool_shell:handle_execute(
                   #{<<"command">> => <<"echo hi">>, <<"timeout">> => <<"bogus">>}, #{})),
    ?assertMatch({ok, _},
                 beamai_tool_shell:handle_execute(
                   #{<<"command">> => <<"echo hi">>, <<"timeout">> => -5}, #{})).

%% LLM 传的大 timeout 不该被我们钳掉——命令 0.3s 就结束，验证的是不报 timeout 错
shell_does_not_impose_own_ceiling_test() ->
    ?assertMatch({ok, _},
                 beamai_tool_shell:handle_execute(
                   #{<<"command">> => <<"sleep 0.3">>, <<"timeout">> => 999999999}, #{})).

%% 回归测试：超时**不能杀死调用者**。
%% 旧写法用 spawn_link + exit(Pid,kill)，killed 顺着链传回调用者，
%% 调用进程直接暴毙——本测试进程会被一起带走。
shell_timeout_does_not_kill_caller_test() ->
    Self = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        R = beamai_tool_shell:handle_execute(
              #{<<"command">> => <<"sleep 10">>, <<"timeout">> => 200}, #{}),
        Self ! {result, R}
    end),
    receive
        {result, R} ->
            erlang:demonitor(MRef, [flush]),
            ?assertMatch({error, {timeout, 200}}, R);
        {'DOWN', MRef, process, Pid, Reason} ->
            ?assertEqual(caller_should_have_survived, {caller_killed, Reason})
    after 5000 ->
        ?assert(false)
    end.

%%====================================================================
%% 辅助
%%====================================================================

timeout_of(Module, Name) ->
    [Spec] = [S || S <- Module:tools(), maps:get(name, S) =:= Name],
    maps:get(timeout, Spec, undefined).
