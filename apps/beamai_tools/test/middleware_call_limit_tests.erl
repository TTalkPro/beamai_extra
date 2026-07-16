%%%-------------------------------------------------------------------
%%% @doc middleware_call_limit 单元测试
%%%
%%% 历史缺陷：halt 不 halt。短路时返回 `#{response => {limit_exceeded,_}}' /
%%% `#{result => {limit_exceeded,_}}'，而 kernel 的成功路径正是
%%% `{ok, #{response := R}} -> {ok, R, Ctx}'——于是错误值被当成**成功**透出，
%%% agent 拿它当 LLM 响应用，content 解出 null、tool_calls 解出 []，
%%% 整轮静默返回空答案，限额信息谁也看不到。
%%%
%%% 正确做法是 throw，由 beamai_filter_chain:run/4 收敛成 {error, Reason}。
%%%
%%% 注：工具侧限额（max_tool_calls）已从本中间件移除——filter 拿的是批内只读
%%% 快照，计数天生累加不起来。该能力现由 agent 的 max_tool_calls 提供
%%% （上游 beamai_agent_tool_loop，测试见 beamai_max_tool_calls_tests）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_call_limit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% chat 链：halt 必须真的 halt
%%====================================================================

%% 回归测试：以前这里是 {ok, {limit_exceeded,_}, _}
model_limit_halt_returns_error_test() ->
    K = chat_kernel(#{max_model_calls => 0, on_limit_exceeded => halt}),
    ?assertMatch({error, {limit_exceeded, #{type := model_calls}}},
                 beamai_kernel:invoke_chat(K, msgs(), #{})).

%% 错误里要带足信息，否则运维看不出是撞了哪条线
model_limit_error_carries_details_test() ->
    K = chat_kernel(#{max_model_calls => 3, on_limit_exceeded => halt}),
    %% 这里直连 invoke_chat、不回穿 context，故计数不累加；直接把上限设成 0 来触发。
    %% （真实 agent 循环会把 invoke_chat 返回的 context 穿回下一轮，计数正常累加）
    K0 = chat_kernel(#{max_model_calls => 0, on_limit_exceeded => halt}),
    {error, {limit_exceeded, D}} = beamai_kernel:invoke_chat(K0, msgs(), #{}),
    ?assertEqual(model_calls, maps:get(type, D)),
    ?assertEqual(0, maps:get(max, D)),
    ?assert(is_binary(maps:get(message, D))),
    %% 未超限时照常放行
    ?assertMatch({ok, _, _}, beamai_kernel:invoke_chat(K, msgs(), #{})).

%% iterations 与 model_calls 是同一个计数，谁小谁先触发
iteration_limit_halt_returns_error_test() ->
    K = chat_kernel(#{max_model_calls => 99, max_iterations => 0, on_limit_exceeded => halt}),
    ?assertMatch({error, {limit_exceeded, #{type := iterations}}},
                 beamai_kernel:invoke_chat(K, msgs(), #{})).

%% warn_and_continue 不该短路
warn_and_continue_passes_through_test() ->
    K = chat_kernel(#{max_model_calls => 0, on_limit_exceeded => warn_and_continue}),
    ?assertMatch({ok, _, _}, beamai_kernel:invoke_chat(K, msgs(), #{})).

under_limit_passes_through_test() ->
    K = chat_kernel(#{max_model_calls => 10, on_limit_exceeded => halt}),
    ?assertMatch({ok, _, _}, beamai_kernel:invoke_chat(K, msgs(), #{})).

%%====================================================================
%% 工具侧限额已移除
%%====================================================================

%% 守住它不会被"好心"加回来：filter 拿的是批内只读快照，计数累加不起来，
%% 加回来只会得到一个静默失效的限额。工具调用限额用 agent 的 max_tool_calls。
tool_side_limits_are_gone_test() ->
    code:ensure_loaded(middleware_call_limit),
    ?assertNot(erlang:function_exported(middleware_call_limit, around_tool, 3)),
    St = middleware_call_limit:init(#{}),
    ?assertEqual(error, maps:find(max_tool_calls, St)),
    ?assertEqual(error, maps:find(max_tool_calls_per_turn, St)),
    ?assertEqual(error, maps:find(tool_call_count, St)).

%% agent 那边确实提供了替代能力（上游 beamai_agent_tool_loop）
agent_provides_tool_call_limit_test() ->
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}, max_tool_calls => 5}),
    ?assertEqual(5, maps:get(max_tool_calls, State)),
    {ok, Default} = beamai_agent_state:create(#{llm => {mock, #{}}}),
    ?assertEqual(infinity, maps:get(max_tool_calls, Default)).

%% 限额是语义错误，不该被当成 transient 反复重试
limit_error_is_not_retryable_test() ->
    Err = {limit_exceeded, #{type => tool_calls, count => 0, max => 0, message => <<"m">>}},
    ?assertEqual(semantic, beamai_tool_error:classify(Err)),
    ?assertNot(middleware_tool_retry:is_retryable(Err, middleware_tool_retry:init(#{}))).

%%====================================================================
%% 辅助
%%====================================================================

msgs() -> [#{role => user, content => <<"hi">>}].

filters(Opts) ->
    beamai_middleware_runner:to_filters(
      beamai_middleware_runner:init([{middleware_call_limit, Opts}])).

chat_kernel(Opts) ->
    beamai_kernel:add_service(
      beamai_kernel:new(#{}, filters(Opts)),
      #{module => cl_fake_llm, provider => openai, model => <<"m">>, api_key => <<"k">>}).

