%%%-------------------------------------------------------------------
%%% @doc 三个 activity 类插件 + 公共零件
%%%
%%% 这些用例守的是**猜错了前端不报错、只会静默少渲染**的那几条：op 顺序、
%%% `replace` 位、null 值整条跳过。单测是唯一会替我们盯住它们的东西 ——
%%% 真前端不会为一条错的 patch 报错。
%%%
%%% Open Generative UI 那个插件在 examples 里（它是某个前端 runtime 的中间件约定，
%%% 不是协议），它的用例在 `examples/test/example_agui_genui_tests.erl`。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_plugins_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助：喂一串事件给 transform，收产出
%%====================================================================

feed(Transform, Events) ->
    lists:flatmap(fun(E) -> Transform(E) end, Events).

types(Events) -> [beamai_agui_event:type_of(E) || E <- Events].

%% 一次工具调用的四条事件（args 分两片来 —— 流式路径就是这样）
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

%%====================================================================
%% 公共零件
%%====================================================================

%% 不被盯的工具原样通过 —— 插件不该对别的调用有任何感知
watcher_ignores_other_tools_test() ->
    T = beamai_agui_plugin:tool_watcher(
          #{tools => [<<"watched">>], on_args => fun(_, _, _) -> [marker()] end}),
    Out = feed(T, call_events(<<"c1">>, <<"other">>, <<"{}">>)),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>], types(Out)).

%% 参数分片到达，攒到 END 才解析 —— 流式与非流式两条路都成立
watcher_reassembles_streamed_args_test() ->
    Self = self(),
    T = beamai_agui_plugin:tool_watcher(
          #{tools => [<<"w">>],
            on_args => fun(_Id, _N, Args) -> Self ! {args, Args}, [marker()] end}),
    _ = feed(T, call_events(<<"c1">>, <<"w">>, <<"{\"a\":1,\"b\":\"x\"}">>)),
    receive {args, Args} -> ?assertEqual(#{<<"a">> => 1, <<"b">> => <<"x">>}, Args)
    after 1000 -> ?assert(false)
    end.

%% hold_start：activity 先出，工具卡片随后 —— 否则前端先看见卡片再看见 UI，闪一下
watcher_hold_start_test() ->
    T = beamai_agui_plugin:tool_watcher(
          #{tools => [<<"w">>], hold_start => true,
            on_args => fun(_, _, _) -> [marker(), marker()] end}),
    Out = feed(T, call_events(<<"c1">>, <<"w">>, <<"{}">>)),
    ?assertEqual([<<"CUSTOM">>,                       %% 第一条 activity
                  <<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"CUSTOM">>,                       %% 剩下的 activity
                  <<"TOOL_CALL_END">>], types(Out)).

%% 一条 activity 都没产出时，扣住的照样得放 —— 否则前端连工具调用都看不见
watcher_hold_releases_even_without_activity_test() ->
    T = beamai_agui_plugin:tool_watcher(
          #{tools => [<<"w">>], hold_start => true, on_args => fun(_, _, _) -> [] end}),
    Out = feed(T, call_events(<<"c1">>, <<"w">>, <<"{}">>)),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_END">>], types(Out)).

%% 插件炸了退回原事件，不把 run 带走
watcher_crash_is_contained_test() ->
    T = beamai_agui_plugin:tool_watcher(
          #{tools => [<<"w">>], on_args => fun(_, _, _) -> error(boom) end}),
    Out = feed(T, call_events(<<"c1">>, <<"w">>, <<"{}">>)),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_END">>], types(Out)).

%% compose：前一个的产出喂给后一个（顺序即层序）
compose_chains_transforms_test() ->
    A = fun(E) -> [E#{<<"a">> => true}] end,
    B = fun(E) -> [E#{<<"b">> => maps:get(<<"a">>, E, false)}] end,
    [Out] = (beamai_agui_plugin:compose([A, B]))(beamai_agui_event:custom(<<"x">>, 1)),
    ?assertEqual(true, maps:get(<<"b">>, Out)).

compose_empty_is_identity_test() ->
    E = beamai_agui_event:custom(<<"x">>, 1),
    ?assertEqual([E], (beamai_agui_plugin:compose([]))(E)).

%% 追加不覆盖：插件的用法说明加在宿主人设后面
append_system_prompt_test() ->
    ?assertEqual(#{system_prompt => <<"base\n\nadded">>},
                 beamai_agui_plugin:append_system_prompt(#{system_prompt => <<"base">>},
                                                         <<"added">>)),
    ?assertEqual(#{system_prompt => <<"added">>},
                 beamai_agui_plugin:append_system_prompt(#{}, <<"added">>)).

marker() -> beamai_agui_event:custom(<<"marker">>, 1).

%%====================================================================
%% A2UI
%%====================================================================

%% 三条 op 的**顺序是协议要求的**：先建面，再给组件，最后灌数据。
%% 少一条或反了，前端要么空白要么报「surface 不存在」。
a2ui_operation_order_test() ->
    Args = #{<<"surfaceId">> => <<"s1">>,
             <<"components">> => [#{<<"id">> => <<"root">>, <<"component">> => <<"Column">>}],
             <<"data">> => #{<<"k">> => 1}},
    Ops = beamai_agui_a2ui:surface_ops(Args, <<"cat">>),
    ?assertEqual([<<"createSurface">>, <<"updateComponents">>, <<"updateDataModel">>],
                 [op_name(O) || O <- Ops]),
    [Create | _] = Ops,
    ?assertEqual(<<"cat">>, maps:get(<<"catalogId">>,
                                     maps:get(<<"createSurface">>, Create))).

%% 没有 data 就不发第三条（前端会把空 data model 当成一次真的清空）
a2ui_no_data_no_update_test() ->
    Ops = beamai_agui_a2ui:surface_ops(#{<<"surfaceId">> => <<"s1">>,
                                         <<"components">> => []}, <<"cat">>),
    ?assertEqual([<<"createSurface">>, <<"updateComponents">>], [op_name(O) || O <- Ops]).

%% `replace: true` = 整块换掉，所以根本不需要 delta
a2ui_snapshot_replaces_test() ->
    Event = beamai_agui_a2ui:surface_event(<<"c1">>, #{<<"surfaceId">> => <<"s">>,
                                                       <<"components">> => []}, <<"cat">>),
    ?assertEqual(<<"ACTIVITY_SNAPSHOT">>, beamai_agui_event:type_of(Event)),
    ?assertEqual(true, maps:get(<<"replace">>, Event)),
    ?assertEqual(<<"a2ui-surface">>, maps:get(<<"activityType">>, Event)),
    ?assertEqual(<<"a2ui-surface-c1">>, maps:get(<<"messageId">>, Event)).

a2ui_event_transform_test() ->
    T = beamai_agui_a2ui:event_transform(),
    Json = <<"{\"surfaceId\":\"s1\",\"components\":[{\"id\":\"root\"}]}">>,
    Out = feed(T, call_events(<<"c1">>, <<"render_a2ui">>, Json)),
    %% 不扣住 TOOL_CALL_START —— 工具卡片先出、面随后贴上（同上游）
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_END">>, <<"ACTIVITY_SNAPSHOT">>], types(Out)).

%% 中间态：参数还在流式落地时，组件就该一点点长出来；终态仍在 END 后整块覆盖。
a2ui_partial_event_transform_test() ->
    T = beamai_agui_a2ui:event_transform(),
    Id = <<"c1">>,
    Events =
        [beamai_agui_event:tool_call_start(Id, <<"render_a2ui">>, <<"msg-1">>),
         beamai_agui_event:tool_call_args(Id, a2ui_chunk1()),
         beamai_agui_event:tool_call_args(Id, a2ui_chunk2()),
         beamai_agui_event:tool_call_args(Id, a2ui_chunk3()),
         beamai_agui_event:tool_call_end(Id)],
    Out = feed(T, Events),
    Snapshots = pick(<<"ACTIVITY_SNAPSHOT">>, Out),

    ?assertEqual(2, length(Snapshots)),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"ACTIVITY_SNAPSHOT">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_END">>,
                  <<"ACTIVITY_SNAPSHOT">>], types(Out)),
    ?assertEqual([<<"a2ui-surface-c1">>, <<"a2ui-surface-c1">>],
                 [maps:get(<<"messageId">>, S) || S <- Snapshots]),
    ?assert(lists:all(fun(S) -> maps:get(<<"replace">>, S) end, Snapshots)),

    Counts = [components_count(S) || S <- Snapshots],
    ?assertEqual([4, 5], Counts),
    ?assertEqual(Counts, lists:sort(Counts)),
    FinalOps = operations(lists:last(Snapshots)),
    ?assert(lists:any(fun(Op) -> maps:is_key(<<"updateDataModel">>, Op) end, FinalOps)).

%% catalog 是模型的词汇表，也是「编不出别的组件」的保证 —— 它必须进 prompt
a2ui_with_tool_injects_catalog_test() ->
    Config = beamai_agui_a2ui:with_tool(client_config(<<"You are Bob.">>)),
    ?assertEqual([<<"render_a2ui">>], tool_names(Config)),
    Prompt = maps:get(system_prompt, Config),
    ?assertMatch({_, _}, binary:match(Prompt, <<"You are Bob.">>)),
    ?assertMatch({_, _}, binary:match(Prompt, <<"Available Components">>)),
    ?assertMatch({_, _}, binary:match(Prompt, <<"\"Column\"">>)).

%% 用户点了生成出来的按钮 → 合成一条 user 消息走普通输入的路（没有新增机制）
a2ui_input_transform_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t1">>,
                      <<"messages">> => [],
                      <<"forwardedProps">> =>
                          #{<<"a2uiAction">> =>
                                #{<<"userAction">> =>
                                      #{<<"name">> => <<"submit">>,
                                        <<"surfaceId">> => <<"s1">>,
                                        <<"context">> => #{<<"n">> => 2}}}}}),
    Transformed = (beamai_agui_a2ui:input_transform())(Input),
    {ok, Text} = beamai_agui_types:last_user_message(Transformed),
    ?assertMatch({_, _}, binary:match(Text, <<"submit">>)),
    ?assertMatch({_, _}, binary:match(Text, <<"s1">>)),
    ?assertMatch({_, _}, binary:match(Text, <<"{\"n\":2}">>)).

%% 没有动作就一个字都不改
a2ui_input_transform_noop_test() ->
    {ok, Input} = beamai_agui_types:parse_run_input(#{<<"threadId">> => <<"t1">>}),
    ?assertEqual(Input, (beamai_agui_a2ui:input_transform())(Input)).

op_name(Op) ->
    [Name] = [K || K <- maps:keys(Op), K =/= <<"version">>],
    Name.

operations(Snapshot) ->
    maps:get(<<"a2ui_operations">>, maps:get(<<"content">>, Snapshot)).

components_count(Snapshot) ->
    [Op] = [Op || Op <- operations(Snapshot), maps:is_key(<<"updateComponents">>, Op)],
    length(maps:get(<<"components">>, maps:get(<<"updateComponents">>, Op))).

%% 三片拼成一份超过 200 字节的参数；切点都在完整 component 后。
a2ui_chunk1() ->
    <<"{\"surfaceId\":\"s1\",\"components\":["
      "{\"id\":\"root\",\"component\":\"Column\",\"children\":[\"title\",\"row\"]},"
      "{\"id\":\"title\",\"component\":\"Text\",\"text\":\"Overview\",\"variant\":\"h2\"},">>.

a2ui_chunk2() ->
    <<"{\"id\":\"row\",\"component\":\"Row\",\"children\":[\"c1\",\"c2\"]},"
      "{\"id\":\"c1\",\"component\":\"Text\",\"text\":\"first\"},">>.

a2ui_chunk3() ->
    <<"{\"id\":\"c2\",\"component\":\"Text\",\"text\":\"second\"}],"
      "\"data\":{\"kpi\":\"revenue\"}}">>.

%%====================================================================
%% MCP Apps
%%====================================================================

app_tool() ->
    #{name => <<"chart">>,
      <<"_meta">> => #{<<"ui/resourceUri">> => <<"ui://chart">>,
                       <<"server_id">> => <<"demo">>}}.

plain_tool() ->
    #{name => <<"add">>}.

%% **只认带 UI 的工具** —— 普通 MCP 工具就是普通工具，不该多出一块界面
mcp_app_tools_only_ui_ones_test() ->
    Apps = beamai_agui_mcp_apps:app_tools([app_tool(), plain_tool()]),
    ?assertEqual([<<"chart">>], maps:keys(Apps)),
    ?assertEqual(#{resource_uri => <<"ui://chart">>, server_id => <<"demo">>},
                 maps:get(<<"chart">>, Apps)).

mcp_result_adds_activity_test() ->
    Apps = beamai_agui_mcp_apps:app_tools([app_tool()]),
    T = beamai_agui_mcp_apps:event_transform(#{apps => Apps}),
    Events = call_events(<<"c1">>, <<"chart">>, <<"{\"n\":3}">>)
             ++ [beamai_agui_event:tool_call_result(<<"m1">>, <<"c1">>, <<"done">>)],
    Out = feed(T, Events),
    ?assertEqual([<<"TOOL_CALL_START">>, <<"TOOL_CALL_ARGS">>, <<"TOOL_CALL_ARGS">>,
                  <<"TOOL_CALL_END">>, <<"TOOL_CALL_RESULT">>, <<"ACTIVITY_SNAPSHOT">>],
                 types(Out)),
    [Snapshot] = pick(<<"ACTIVITY_SNAPSHOT">>, Out),
    Content = maps:get(<<"content">>, Snapshot),
    ?assertEqual(<<"ui://chart">>, maps:get(<<"resourceUri">>, Content)),
    ?assertEqual(<<"demo">>, maps:get(<<"serverId">>, Content)),
    %% 那块界面渲染时经常要知道「这次是用什么参数调的」
    ?assertEqual(#{<<"n">> => 3}, maps:get(<<"toolInput">>, Content)),
    ?assertEqual(<<"done">>, maps:get(<<"result">>, Content)).

mcp_plain_tool_gets_no_activity_test() ->
    T = beamai_agui_mcp_apps:event_transform(
          #{apps => beamai_agui_mcp_apps:app_tools([app_tool()])}),
    Events = call_events(<<"c2">>, <<"add">>, <<"{}">>)
             ++ [beamai_agui_event:tool_call_result(<<"m1">>, <<"c2">>, <<"3">>)],
    ?assertEqual([], pick(<<"ACTIVITY_SNAPSHOT">>, feed(T, Events))).

%% 那块界面跑在浏览器里，不能让它随便调 server 的任何东西
mcp_proxy_whitelist_test() ->
    Servers = [#{server_id => <<"demo">>, client => some_client}],
    ?assertEqual({error, {method_not_allowed, <<"resources/subscribe">>}},
                 beamai_agui_mcp_apps:proxy_request(
                   Servers, #{<<"serverId">> => <<"demo">>,
                              <<"method">> => <<"resources/subscribe">>})),
    ?assert(lists:member(<<"tools/call">>, beamai_agui_mcp_apps:proxy_allowed_methods())).

%% 前端不知道 url，只有 serverId —— 认不出来就如实报，不去猜一个
mcp_proxy_unknown_server_test() ->
    ?assertEqual({error, {unknown_server, <<"nope">>}},
                 beamai_agui_mcp_apps:proxy_request(
                   [], #{<<"serverId">> => <<"nope">>, <<"method">> => <<"ping">>})).

%%====================================================================
%% 夹具
%%====================================================================

client_config(Prompt) ->
    Client = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
    Base = #{chat_client => Client},
    case Prompt of
        undefined -> Base;
        _ -> Base#{system_prompt => Prompt}
    end.

tool_names(Config) ->
    lists:sort([maps:get(name, T)
                || T <- maps:values(beamai_chat_client:tools(maps:get(chat_client, Config)))]).
