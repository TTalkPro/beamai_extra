%%%-------------------------------------------------------------------
%%% @doc 共享状态：JSON Patch 差分 + STATE_SNAPSHOT / STATE_DELTA 端到端
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_state_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% JSON Patch 差分
%%====================================================================

no_change_yields_empty_patch_test() ->
    ?assertEqual([], beamai_agui_state:diff(#{<<"a">> => 1}, #{<<"a">> => 1})),
    ?assertEqual([], beamai_agui_state:diff(#{}, #{})).

add_remove_replace_test() ->
    Old = #{<<"keep">> => 1, <<"drop">> => 2, <<"change">> => 3},
    New = #{<<"keep">> => 1, <<"change">> => 4, <<"new">> => 5},
    %% 顺序按键排（change < drop < keep < new），不是随机的
    ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/change">>,
                    <<"value">> => 4},
                  #{<<"op">> => <<"remove">>, <<"path">> => <<"/drop">>},
                  #{<<"op">> => <<"add">>, <<"path">> => <<"/new">>, <<"value">> => 5}],
                 beamai_agui_state:diff(Old, New)).

%% 两边都是 map 才往下走，路径一段段拼
nested_maps_recurse_test() ->
    Old = #{<<"user">> => #{<<"name">> => <<"a">>, <<"age">> => 1}},
    New = #{<<"user">> => #{<<"name">> => <<"b">>, <<"age">> => 1}},
    ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/user/name">>,
                    <<"value">> => <<"b">>}],
                 beamai_agui_state:diff(Old, New)).

%% 数组整体替换：元素级 diff 要一套 LCS 才不会算出一堆无意义的移位补丁
lists_replaced_wholesale_test() ->
    Old = #{<<"xs">> => [1, 2, 3]},
    New = #{<<"xs">> => [1, 9, 3]},
    ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/xs">>,
                    <<"value">> => [1, 9, 3]}],
                 beamai_agui_state:diff(Old, New)).

%% 换型（map → 标量）也是整体替换，不会误把标量当 map 递归
type_change_replaces_test() ->
    ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/v">>,
                    <<"value">> => 7}],
                 beamai_agui_state:diff(#{<<"v">> => #{<<"a">> => 1}}, #{<<"v">> => 7})).

%% JSON Pointer 里 ~ 和 / 有特殊含义（RFC 6901）——键里含它们必须转义，
%% 否则算出来的路径指向别处
path_escaping_test() ->
    ?assertEqual(<<"a~1b">>, beamai_agui_state:escape(<<"a/b">>)),
    ?assertEqual(<<"a~0b">>, beamai_agui_state:escape(<<"a~b">>)),
    %% ~ 必须先转，否则 / → ~1 之后那个 ~ 会被二次转义
    ?assertEqual(<<"~0~1">>, beamai_agui_state:escape(<<"~/">>)),
    [Op] = beamai_agui_state:diff(#{}, #{<<"a/b">> => 1}),
    ?assertEqual(<<"/a~1b">>, maps:get(<<"path">>, Op)).

%% 嵌套里出现 atom 键也认（工具随手写的 map），路径段转成字符串
atom_keys_tolerated_test() ->
    Old = #{<<"cfg">> => #{mode => <<"a">>}},
    New = #{<<"cfg">> => #{mode => <<"b">>}},
    ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/cfg/mode">>,
                    <<"value">> => <<"b">>}],
                 beamai_agui_state:diff(Old, New)).

%% 同样的输入总得到同样的补丁（按键排序），便于测试与比对
deterministic_order_test() ->
    Old = #{},
    New = #{<<"b">> => 2, <<"a">> => 1, <<"c">> => 3},
    Paths = [maps:get(<<"path">>, Op) || Op <- beamai_agui_state:diff(Old, New)],
    ?assertEqual([<<"/a">>, <<"/b">>, <<"/c">>], Paths).

%%====================================================================
%% 端到端：请求带 state → 工具改 → STATE_DELTA 推回
%%====================================================================

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

mock_llm() ->
    CC = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, stream_chat,
        fun(_C, _M, _RawCb, Opts) ->
            counters:add(CC, 1, 1),
            TokenCb = maps:get(on_llm_new_token, Opts),
            case counters:get(CC, 1) of
                1 -> {ok, beamai_chat_response:new(
                        #{content => null, tool_calls => [tc(<<"c1">>, <<"bump">>)],
                          finish_reason => <<"tool_calls">>})};
                _ ->
                    TokenCb(<<"done">>, #{}),
                    {ok, beamai_chat_response:new(
                        #{content => <<"done">>, finish_reason => <<"stop">>})}
            end
        end),
    ok.

%% 工具在种进来的 state 之上做增量：counter + 1
factory() ->
    Bump = fun(_Args, Ctx) ->
        N = beamai_context:state_get(Ctx, <<"counter">>, 0),
        {ok, <<"bumped">>, #{<<"counter">> => N + 1}}
    end,
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                               beamai_chat_model:create(mock, #{})),
        K = beamai_chat_client:add_tool(K0, #{name => <<"bump">>, parameters => #{},
                                              handler => Bump}),
        beamai_agent:new(maps:merge(#{chat_client => K, memory => false}, RuntimeCfg))
    end.

input(State) ->
    {ok, Input} = beamai_agui_types:parse_run_input(
                    #{<<"threadId">> => <<"t-state">>,
                      <<"messages">> => [#{<<"role">> => <<"user">>,
                                           <<"content">> => <<"go">>}],
                      <<"state">> => State}),
    Input.

collect() ->
    receive {ev, Ev} -> [Ev | collect()] after 0 -> [] end.

run(State) ->
    Self = self(),
    ok = beamai_agui_runner:run(input(State), factory(),
                                fun(Ev) -> Self ! {ev, Ev}, ok end),
    collect().

pick(Type, Events) ->
    [E || E <- Events, maps:get(<<"type">>, E) =:= Type].

%% 请求带上来的 state 既作为基线快照发回，也真的种进了 agent
%% （工具读到 41 才可能写出 42）
seeded_state_snapshot_and_delta_test() ->
    ok = mock_llm(),
    try
        Events = run(#{<<"counter">> => 41, <<"keep">> => <<"me">>}),
        [Snapshot] = pick(<<"STATE_SNAPSHOT">>, Events),
        ?assertEqual(#{<<"counter">> => 41, <<"keep">> => <<"me">>},
                     maps:get(<<"snapshot">>, Snapshot)),
        %% 基线必须排在增量前面，否则前端拿补丁没有可打的底
        Types = [maps:get(<<"type">>, E) || E <- Events],
        ?assert(index_of(<<"STATE_SNAPSHOT">>, Types)
                    < index_of(<<"STATE_DELTA">>, Types)),
        [Delta] = pick(<<"STATE_DELTA">>, Events),
        ?assertEqual([#{<<"op">> => <<"replace">>, <<"path">> => <<"/counter">>,
                        <<"value">> => 42}],
                     maps:get(<<"delta">>, Delta))
    after
        meck:unload(beamai_chat_model)
    end.

%% 不带 state 时基线是空 map，工具从 0 起步 → 补丁是 add 而不是 replace
empty_state_yields_add_test() ->
    ok = mock_llm(),
    try
        Events = run(#{}),
        [Snapshot] = pick(<<"STATE_SNAPSHOT">>, Events),
        ?assertEqual(#{}, maps:get(<<"snapshot">>, Snapshot)),
        [Delta] = pick(<<"STATE_DELTA">>, Events),
        ?assertEqual([#{<<"op">> => <<"add">>, <<"path">> => <<"/counter">>,
                        <<"value">> => 1}],
                     maps:get(<<"delta">>, Delta))
    after
        meck:unload(beamai_chat_model)
    end.

%% state 不是 map（外部输入不可信）→ 当没有，不崩
non_map_state_ignored_test() ->
    ok = mock_llm(),
    try
        Events = run(<<"not a map">>),
        [Snapshot] = pick(<<"STATE_SNAPSHOT">>, Events),
        ?assertEqual(#{}, maps:get(<<"snapshot">>, Snapshot)),
        ?assertMatch([_], pick(<<"STATE_DELTA">>, Events))
    after
        meck:unload(beamai_chat_model)
    end.

index_of(X, L) -> index_of(X, L, 1).
index_of(X, [X | _], N) -> N;
index_of(X, [_ | T], N) -> index_of(X, T, N + 1);
index_of(_X, [], _N) -> 9999.
