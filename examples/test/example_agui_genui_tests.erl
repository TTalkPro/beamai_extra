%%%-------------------------------------------------------------------
%%% @doc Open Generative UI 插件（示例插件，跟着代码放在 examples 里）
%%%
%%% 守的是**猜错了前端不报错、只会静默少渲染**的那几条：snapshot 必须排在任何
%%% delta 之前、`add` 必须带 value（null 整条跳过）、`html` 是数组不是字符串。
%%% 真前端不会为一条错的 patch 报错，所以这些只能由单测盯着。
%%%
%%% ```
%%% cd examples && make test
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_genui_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

feed(Transform, Events) ->
    lists:flatmap(fun(E) -> Transform(E) end, Events).

call_events(Id, Name, ArgsJson) ->
    {Head, Tail} = split(ArgsJson),
    [beamai_agui_event:tool_call_start(Id, Name, <<"msg-1">>),
     beamai_agui_event:tool_call_args(Id, Head),
     beamai_agui_event:tool_call_args(Id, Tail),
     beamai_agui_event:tool_call_end(Id)].

split(Bin) ->
    Half = byte_size(Bin) div 2,
    {binary:part(Bin, 0, Half), binary:part(Bin, Half, byte_size(Bin) - Half)}.

pick(Type, Events) ->
    [E || E <- Events, beamai_agui_event:type_of(E) =:= Type].

client_config() ->
    #{chat_client => beamai_chat_client:add_chat_model(
                       beamai_chat_client:new(),
                       beamai_chat_model:create(mock, #{}))}.

tool_names(Config) ->
    lists:sort([maps:get(name, T)
                || T <- maps:values(beamai_chat_client:tools(maps:get(chat_client, Config)))]).

%%====================================================================
%% Open Generative UI
%%====================================================================

genui_args() ->
    #{<<"initialHeight">> => 400,
      <<"placeholderMessages">> => [<<"working">>],
      <<"css">> => <<"body{}">>,
      <<"html">> => <<"<div/>">>,
      <<"jsFunctions">> => <<"function f(){}">>,
      <<"jsExpressions">> => [<<"f()">>]}.

%% **snapshot 必须在任何 delta 之前**，而且只发一条 —— 消息还没建，前端会把
%% delta 悄悄丢弃（不报错，只是少渲染一块）
genui_snapshot_comes_first_test() ->
    [First | Rest] = example_agui_genui:activity_events(<<"c1">>, genui_args()),
    ?assertEqual(<<"ACTIVITY_SNAPSHOT">>, beamai_agui_event:type_of(First)),
    ?assertEqual(#{<<"generating">> => true, <<"initialHeight">> => 400},
                 maps:get(<<"content">>, First)),
    ?assertEqual([], pick(<<"ACTIVITY_SNAPSHOT">>, Rest)).

%% **html 是数组不是字符串**：先建空数组，再逐块追加，前端 join 起来渲染
genui_html_is_an_array_test() ->
    Events = example_agui_genui:activity_events(<<"c1">>, genui_args()),
    ?assertEqual([{<<"/html">>, []}, {<<"/html/-">>, <<"<div/>">>},
                  {<<"/htmlComplete">>, true}],
                 [{P, V} || {P, V} <- paths(Events),
                            binary:longest_common_prefix([P, <<"/html">>]) >= 5]).

%% 收尾一定有 `generating false`，前端据此收掉占位、定稿
genui_ends_with_generating_false_test() ->
    Events = example_agui_genui:activity_events(<<"c1">>, genui_args()),
    ?assertEqual({<<"/generating">>, false}, lists:last(paths(Events))).

%% **null 值整条跳过**：一条没有 value 的 patch 会让前端把**整批**判非法丢掉
genui_null_values_are_skipped_test() ->
    Events = example_agui_genui:activity_events(
               <<"c1">>, #{<<"css">> => null, <<"jsFunctions">> => null,
                           <<"html">> => <<"<p/>">>}),
    Paths = [P || {P, _} <- paths(Events)],
    ?assertEqual([], [P || P <- Paths, P =:= <<"/css">> orelse P =:= <<"/jsFunctions">>]),
    ?assert(lists:member(<<"/html/-">>, Paths)).

%% 顺序是约定的那一个（模型的 key 顺序在 provider 解析 JSON 时就没了）
genui_key_order_test() ->
    Events = example_agui_genui:activity_events(<<"c1">>, genui_args()),
    ?assertEqual([<<"/placeholderMessages">>, <<"/placeholderMessages/-">>,
                  <<"/css">>, <<"/cssComplete">>,
                  <<"/html">>, <<"/html/-">>, <<"/htmlComplete">>,
                  <<"/jsFunctions">>, <<"/jsFunctionsComplete">>,
                  <<"/jsExpressions">>, <<"/jsExpressions/-">>,
                  <<"/jsExpressionsComplete">>,
                  <<"/generating">>],
                 [P || {P, _} <- paths(Events)]).

%% 扣住工具卡片直到第一条 activity 出去
genui_event_transform_holds_start_test() ->
    T = example_agui_genui:event_transform(),
    Json = <<"{\"html\":\"<p/>\"}">>,
    Out = feed(T, call_events(<<"c1">>, <<"generateSandboxedUi">>, Json)),
    ?assertEqual(<<"ACTIVITY_SNAPSHOT">>, beamai_agui_event:type_of(hd(Out))),
    ?assertEqual(<<"TOOL_CALL_START">>, beamai_agui_event:type_of(lists:nth(2, Out))),
    ?assertEqual(<<"TOOL_CALL_END">>, beamai_agui_event:type_of(lists:last(Out))).

paths(Events) ->
    [{maps:get(<<"path">>, Op), maps:get(<<"value">>, Op)}
     || E <- Events, beamai_agui_event:type_of(E) =:= <<"ACTIVITY_DELTA">>,
        Op <- maps:get(<<"patch">>, E)].

%% 工具**必须真的挂到 chat client 上** —— `beamai_agent:new/1` 没有 `tools` 这一位，
%% 挂错地方编译得过、跑得动，模型却永远看不见（实机联调才照出来的那个 bug）
genui_with_tool_test() ->
    Config = example_agui_genui:with_tool(client_config()),
    ?assertEqual([<<"generateSandboxedUi">>], tool_names(Config)),
    ?assertMatch({_, _}, binary:match(maps:get(system_prompt, Config), <<"shadcn/ui">>)),
    %% 明确说不要设计规范就一个字都不加
    ?assertNot(maps:is_key(system_prompt,
                           example_agui_genui:with_tool(client_config(),
                                                        #{design_skill => undefined}))).

%% 挂不上就直接报错 —— 装不上的插件应当在装配期炸，不是等线上发现模型行为不对
genui_with_tool_requires_chat_client_test() ->
    ?assertError({beamai_agui_plugin, no_chat_client, _},
                 example_agui_genui:with_tool(#{})).

%% 提示词是发给模型的英文散文 —— **必须是纯 ASCII**：Erlang 的 `<<"...">>` 把非
%% ASCII 码点按字节截断，一个破折号进模型就是个控制字符（JSON 里 \u0014）
genui_prompts_are_ascii_test() ->
    lists:foreach(fun(P) ->
        ?assertEqual([], [C || <<C>> <= P, C > 127 orelse (C < 32 andalso C =/= $\n)])
    end, [maps:get(description, example_agui_genui:sandboxed_ui_tool()),
          example_agui_genui:design_skill()]).
