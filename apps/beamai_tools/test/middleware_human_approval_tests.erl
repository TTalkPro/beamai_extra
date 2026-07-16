%%%-------------------------------------------------------------------
%%% @doc middleware_human_approval 单元测试
%%%
%%% 历史缺陷：around_tool 的 Req 里 `tool` 是 tool_spec（map）不是名字，
%%% 但代码直接 maps:get(tool, Req) 当名字用，导致 selective 模式
%%% `lists:member(ToolSpecMap, [<<"名字">>])` 恒为 false——选择性审批从不触发。
%%%
%%% 另：缺省 mode 曾为 none（静默放行），与 presets 的 all 不一致。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_human_approval_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 缺省值
%%====================================================================

%% 审批网关未配置时必须 fail-closed，不能静默放行
default_mode_is_fail_closed_test() ->
    ?assertEqual(all, maps:get(mode, middleware_human_approval:init(#{}))).

%% 与 presets 保持一致（历史上二者分别是 none / all）
default_matches_preset_test() ->
    {middleware_human_approval, PresetOpts, _} = beamai_middleware_presets:human_approval(),
    ModuleDefault = maps:get(mode, middleware_human_approval:init(#{})),
    ?assertEqual(maps:get(mode, PresetOpts), ModuleDefault).

explicit_none_still_works_test() ->
    ?assertEqual(none, maps:get(mode, middleware_human_approval:init(#{mode => none}))).

%%====================================================================
%% selective 模式（回归测试）
%%====================================================================

%% 以前恒不触发：拿 spec map 去和名字列表比
selective_mode_approves_listed_tool_test() ->
    {Result, Called} = run(#{mode => selective,
                             tools_requiring_approval => [<<"danger">>],
                             approval_handler => fun(_, _) -> confirm end}),
    ?assertEqual({ok, <<"done">>, #{}}, Result),
    ?assert(Called).

selective_mode_rejects_listed_tool_test() ->
    {Result, Called} = run(#{mode => selective,
                             tools_requiring_approval => [<<"danger">>],
                             approval_handler => fun(_, _) -> reject end}),
    ?assert(is_rejection(Result)),
    ?assertNot(Called).

%%====================================================================
%% 拒绝结果的形状
%%====================================================================

%% 回归测试：以前把裸元组 {error,{approval_rejected,_}} 塞进 result，
%% kernel 当成功值透出，最后被 io_lib:format("~p") 兜底——模型收到的是
%% Erlang 项式（<<"..">>、#{k => v}）而不是 JSON。现在必须是已编码的 JSON。
rejection_is_encoded_json_test() ->
    {Result, _} = run(#{mode => all, approval_handler => fun(_, _) -> reject end}),
    {ok, Value, _} = Result,
    ?assert(is_binary(Value)),
    Decoded = jsx:decode(Value, [return_maps]),
    Err = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<"approval_rejected">>, maps:get(<<"type">>, Err)),
    ?assertEqual(<<"danger">>, maps:get(<<"tool">>, Err)),
    ?assert(is_binary(maps:get(<<"message">>, Err))).

%% 不能再出现 Erlang 项式的痕迹
rejection_carries_no_erlang_terms_test() ->
    {{ok, Value, _}, _} = run(#{mode => all, approval_handler => fun(_, _) -> reject end}),
    ?assertEqual(nomatch, binary:match(Value, <<"<<">>)),
    ?assertEqual(nomatch, binary:match(Value, <<"=>">>)).

%% 不在列表里的工具直接放行，不问审批
selective_mode_skips_unlisted_tool_test() ->
    Self = self(),
    {Result, Called} = run(#{mode => selective,
                             tools_requiring_approval => [<<"other">>],
                             approval_handler => fun(_, _) -> Self ! asked, confirm end}),
    ?assertEqual({ok, <<"done">>, #{}}, Result),
    ?assert(Called),
    receive asked -> ?assert(false) after 50 -> ok end.

%%====================================================================
%% 审批处理器
%%====================================================================

%% handler 必须收到工具名，而不是整个 tool_spec map
handler_receives_tool_name_test() ->
    Self = self(),
    run(#{mode => all,
          approval_handler => fun(Req, _) -> Self ! {req, Req}, confirm end}),
    receive {req, Req} ->
        ?assertEqual(<<"danger">>, maps:get(function, Req)),
        ?assertEqual(tool_approval, maps:get(type, Req))
    after 200 -> ?assert(false)
    end.

all_mode_confirm_executes_tool_test() ->
    {Result, Called} = run(#{mode => all, approval_handler => fun(_, _) -> confirm end}),
    ?assertEqual({ok, <<"done">>, #{}}, Result),
    ?assert(Called).

%% 未配置 handler 时默认拒绝（fail-closed）
no_handler_rejects_test() ->
    {Result, Called} = run(#{mode => all}),
    ?assert(is_rejection(Result)),
    ?assertNot(Called).

none_mode_bypasses_approval_test() ->
    {Result, Called} = run(#{mode => none, approval_handler => fun(_, _) -> reject end}),
    ?assertEqual({ok, <<"done">>, #{}}, Result),
    ?assert(Called).

handler_crash_falls_back_to_timeout_action_test() ->
    {Result, _} = run(#{mode => all, timeout_action => reject,
                        approval_handler => fun(_, _) -> error(boom) end}),
    ?assert(is_rejection(Result)).

%% 回归测试：处理器崩溃必须**立刻**收场。
%% 早先只 spawn 不 monitor，崩溃后没人发消息，这里会干等满 timeout（缺省 60s）。
handler_crash_does_not_wait_for_timeout_test() ->
    {Elapsed, _} = timer:tc(fun() ->
        run(#{mode => all, timeout => 30000, timeout_action => reject,
              approval_handler => fun(_, _) -> error(boom) end})
    end),
    ?assert(Elapsed div 1000 < 1000).

%% 处理器超时（不崩溃、只是不回）仍走 timeout_action
handler_timeout_uses_timeout_action_test() ->
    {Result, Called} = run(#{mode => all, timeout => 100, timeout_action => reject,
                             approval_handler => fun(_, _) -> timer:sleep(5000), confirm end}),
    ?assert(is_rejection(Result)),
    ?assertNot(Called).

handler_timeout_action_confirm_test() ->
    {Result, Called} = run(#{mode => all, timeout => 100, timeout_action => confirm,
                             approval_handler => fun(_, _) -> timer:sleep(5000), reject end}),
    ?assertEqual({ok, <<"done">>, #{}}, Result),
    ?assert(Called).

%%====================================================================
%% 辅助
%%====================================================================

%% 跑一次真实 kernel 工具调用，返回 {结果, 工具是否真的被执行}
run(Opts) ->
    Counter = counters:new(1, []),
    Handler = fun(_) -> counters:add(Counter, 1, 1), {ok, <<"done">>} end,
    Tool = beamai_tool:new(<<"danger">>, Handler, #{description => <<"d">>}),
    Chain = beamai_middleware_runner:init([{middleware_human_approval, Opts}]),
    Kernel = beamai_kernel:add_tool(
               beamai_kernel:new(#{}, beamai_middleware_runner:to_filters(Chain)), Tool),
    Result = beamai_kernel:invoke_tool(Kernel, <<"danger">>, #{}, beamai_context:new()),
    {Result, counters:get(Counter, 1) > 0}.

%% @private 是否为一个"已拒绝"的编码结果
is_rejection({ok, Value, _}) when is_binary(Value) ->
    case catch jsx:decode(Value, [return_maps]) of
        #{<<"error">> := #{<<"type">> := <<"approval_rejected">>}} -> true;
        _ -> false
    end;
is_rejection(_) -> false.
