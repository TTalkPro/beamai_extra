%%%-------------------------------------------------------------------
%%% @doc beamai_middleware_runner 单元测试
%%%
%%% 该模块是 beamai_extra 中间件接到上游 beamai_filter 洋葱的唯一桥梁，
%%% 此前无任何测试覆盖。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_runner_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% init/1 规格解析
%%====================================================================

init_accepts_bare_module_test() ->
    [MW] = beamai_middleware_runner:init([mw_stub_noop]),
    ?assertEqual(mw_stub_noop, maps:get(module, MW)),
    ?assertEqual(100, maps:get(priority, MW)),
    %% init/1 被调用过
    ?assertEqual(noop, maps:get(stub, maps:get(state, MW))).

init_accepts_module_with_opts_test() ->
    [MW] = beamai_middleware_runner:init([{mw_stub_noop, #{a => 1}}]),
    ?assertEqual(100, maps:get(priority, MW)),
    ?assertEqual(1, maps:get(a, maps:get(state, MW))).

init_accepts_module_with_opts_and_priority_test() ->
    [MW] = beamai_middleware_runner:init([{mw_stub_noop, #{}, 42}]),
    ?assertEqual(42, maps:get(priority, MW)).

%% 未导出 init/1 的模块直接拿 Opts 当状态
init_without_init_callback_uses_opts_as_state_test() ->
    [MW] = beamai_middleware_runner:init([{lists, #{raw => opts}}]),
    ?assertEqual(#{raw => opts}, maps:get(state, MW)).

%% 优先级升序 = 洋葱外层在前
init_sorts_by_priority_ascending_test() ->
    Chain = beamai_middleware_runner:init([
        {mw_stub_noop, #{}, 90},
        {mw_stub_hooks, #{}, 10},
        {middleware_call_limit, #{}, 50}
    ]),
    ?assertEqual([mw_stub_hooks, middleware_call_limit, mw_stub_noop],
                 [maps:get(module, M) || M <- Chain]).

init_empty_chain_test() ->
    ?assertEqual([], beamai_middleware_runner:init([])).

%%====================================================================
%% to_filters/1 桥接
%%====================================================================

%% 无 around_* 回调的中间件不产生 filter
to_filters_skips_middleware_without_hooks_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_noop]),
    ?assertEqual([], beamai_middleware_runner:to_filters(Chain)).

to_filters_produces_valid_filter_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_hooks]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    ?assertEqual(true, maps:get('__filter__', Filter)),
    ?assertEqual(<<"mw_mw_stub_hooks">>, maps:get(name, Filter)).

%% 导出了哪些 around_* 就挂哪些 hook
to_filters_collects_all_exported_hooks_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_hooks]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    ?assertNotEqual(undefined, beamai_filter:hook(Filter, around_chat)),
    ?assertNotEqual(undefined, beamai_filter:hook(Filter, around_tool)),
    ?assertNotEqual(undefined, beamai_filter:hook(Filter, around_turn)).

to_filters_only_collects_exported_hooks_test() ->
    %% middleware_model_retry 只导出 around_chat
    Chain = beamai_middleware_runner:init([middleware_model_retry]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    ?assertNotEqual(undefined, beamai_filter:hook(Filter, around_chat)),
    ?assertEqual(undefined, beamai_filter:hook(Filter, around_tool)),
    ?assertEqual(undefined, beamai_filter:hook(Filter, around_turn)).

%% 中间件 init/1 的返回值成为 filter 的初始私有状态
to_filters_uses_middleware_state_as_filter_init_test() ->
    Chain = beamai_middleware_runner:init([{mw_stub_hooks, #{seed => 7}}]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    Init = beamai_filter:init(Filter),
    ?assertEqual(7, maps:get(seed, Init)),
    ?assertEqual(0, maps:get(calls, Init)).

to_filters_preserves_chain_order_test() ->
    Chain = beamai_middleware_runner:init([
        {mw_stub_hooks, #{}, 90},
        {middleware_model_retry, #{}, 10}
    ]),
    Filters = beamai_middleware_runner:to_filters(Chain),
    ?assertEqual([<<"mw_middleware_model_retry">>, <<"mw_mw_stub_hooks">>],
                 [maps:get(name, F) || F <- Filters]).

%%====================================================================
%% 桥接后的钩子确实可执行
%%====================================================================

%% 裸 Resp 返回值不应被当成 {Resp, State}
bridged_hook_handles_bare_response_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_hooks]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    Hook = beamai_filter:hook(Filter, around_tool),
    Next = fun(Req) -> #{result => maps:get(tagged_by, Req)} end,
    ?assertEqual(#{result => tool}, Hook(#{}, #{}, Next)).

%% {Resp, NewState} 返回值原样透传给 filter 层
bridged_hook_handles_stateful_response_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_hooks]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    Hook = beamai_filter:hook(Filter, around_chat),
    Next = fun(Req) -> #{response => maps:get(tagged_by, Req)} end,
    {Resp, NewState} = Hook(#{}, #{calls => 4}, Next),
    ?assertEqual(#{response => chat}, Resp),
    ?assertEqual(5, maps:get(calls, NewState)).

%%====================================================================
%% around_turn 状态回写的地雷
%%====================================================================

%% turn 链的响应是 tuple，不是带 context 的 map，filter_chain 匹配不上
%% `{#{context := _}, NewFCtx}`，会把 {Resp, State} 整个当响应透出，
%% beamai_agent:dispatch_turn_result/4 随即 case_clause 崩溃。
%% 所以桥接层必须丢掉 turn 钩子的状态，只透传裸 Resp。
turn_hook_discards_state_to_avoid_leaking_tuple_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_turn_stateful]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    Hook = beamai_filter:hook(Filter, around_turn),
    TurnResult = {ok, resp, 0, 1, []},
    Next = fun(_Req) -> TurnResult end,
    %% 中间件返回了 {Resp, NewState}，但桥接层只能透出 Resp
    ?assertEqual(TurnResult, Hook(#{}, #{}, Next)).

%% 端到端：带状态的 turn 中间件经 filter_chain 跑完，turn 结果必须原样保持
turn_hook_survives_filter_chain_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_turn_stateful]),
    Filters = beamai_middleware_runner:to_filters(Chain),
    TurnResult = {ok, resp, 0, 1, []},
    Terminal = fun(_Req) -> TurnResult end,
    Req = #{messages => [], context => beamai_context:new()},
    ?assertEqual({ok, TurnResult},
                 beamai_filter_chain:run(Filters, around_turn, Terminal, Req)).

%% chat/tool 钩子不受影响，状态照常回写
chat_hook_still_persists_state_test() ->
    Chain = beamai_middleware_runner:init([mw_stub_hooks]),
    [Filter] = beamai_middleware_runner:to_filters(Chain),
    Hook = beamai_filter:hook(Filter, around_chat),
    ?assertMatch({_, #{calls := 1}},
                 Hook(#{}, #{calls => 0}, fun(_) -> #{response => ok} end)).
