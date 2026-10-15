%%%-------------------------------------------------------------------
%%% @doc AG-UI Runtime 实机联调：真 LLM + 真 HTTP + 真 SSE
%%%
%%% 单测里 LLM 是 meck 的、事件是直接从 runner 收的。这个例子把整条链路接通：
%%% 真起 cowboy → gun 发 POST → 真模型决定调不调工具 → SSE 帧一帧帧回来。
%%%
%%% 顺带验证**共享状态的闭环**：请求带 state 上来，工具在它之上改，改动以
%%% STATE_DELTA（JSON Patch）推回；客户端把补丁打到自己那份基线上，最后与
%%% 服务端算出来的应当一致 —— 不一致就说明 diff 或 patch 有一边算错了。
%%%
%%% == 用法 ==
%%%
%%% ```
%%% cd examples && make compile
%%% ERL_LIBS=../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts rebar3 shell
%%% 1> example_agui_live:run().            %% MiniMax m2.7
%%% 2> example_agui_live:run(zhipu).       %% ZHIPU glm-5.3-flash
%%% ```
%%%
%%% 环境变量：`MINIMAX_API_KEY` / `ZHIPU_API_KEY`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_live).

-export([run/0, run/1]).

-define(LISTENER, example_agui_listener).
-define(AGENT, <<"todo-agent">>).

%%====================================================================
%% 入口
%%====================================================================

run() -> run(minimax).

run(Provider) ->
    {ok, _} = application:ensure_all_started(beamai_agui),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    ok = beamai_agui_registry:register(?AGENT, factory(Provider)),
    Port = start_http(),
    try
        io:format("~n=== AG-UI live: ~p, http://127.0.0.1:~p/agui/run ===~n",
                  [Provider, Port]),
        Baseline = #{<<"todos">> => [<<"寄快递"/utf8>>]},
        Events = post_run(Port, Baseline,
                          <<"把「买牛奶」和「交房租」加进待办，然后一句话告诉我现在共有几项。"/utf8>>),
        report(Baseline, Events)
    after
        cowboy:stop_listener(?LISTENER)
    end.

%%====================================================================
%% Agent
%%====================================================================

factory(Provider) ->
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:new(#{state_slots => slots()}),
        K1 = beamai_chat_client:add_chat_model(K0, llm(Provider)),
        K2 = beamai_chat_client:add_tool(K1, add_todo_tool()),
        beamai_agent:new(maps:merge(
            #{chat_client => K2,
              memory => false,
              system_prompt => <<"你是待办助手。要加待办就调 add_todo，一次只加一项，"
                                 "可以连着调多次。"/utf8>>},
            RuntimeCfg))
    end.

%% todos 槽的 reducer：写进来的是**一项**，折叠成追加
slots() ->
    #{<<"todos">> => #{init => [],
                       reduce => fun(Acc, Item) -> Acc ++ [Item] end}}.

%% 工具只声明写意图（writes），真正落进 state 由屏障处统一折叠
add_todo_tool() ->
    #{name => <<"add_todo">>,
      description => <<"把一项待办加入清单"/utf8>>,
      parameters => #{<<"text">> => #{type => string, required => true,
                                      description => <<"待办内容"/utf8>>}},
      handler => fun(Args, _Ctx) ->
          Text = maps:get(<<"text">>, Args, maps:get(text, Args, <<>>)),
          {ok, <<"已加入："/utf8, Text/binary>>, #{<<"todos">> => Text}}
      end}.

llm(minimax) ->
    example_llm_config:minimax(#{api_key => env("MINIMAX_API_KEY"),
                                 model => <<"m2.7">>});
llm(zhipu) ->
    example_llm_config:anthropic(#{api_key => env("ZHIPU_API_KEY"),
                                   model => <<"glm-5.3-flash">>}).

env(Name) ->
    case os:getenv(Name) of
        false -> error({missing_env, Name});
        V -> list_to_binary(V)
    end.

%%====================================================================
%% HTTP
%%====================================================================

start_http() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/agui/run", beamai_agui_cowboy_handler, #{agent => ?AGENT}},
               {"/agui/info", beamai_agui_cowboy_handler, #{mode => info}}]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    ranch:get_port(?LISTENER).

%% 发一次 run，边收边打印（不缓冲——要看的就是"流"这件事）
post_run(Port, State, Prompt) ->
    Body = beamai_utils:encode_json(
             #{<<"threadId">> => <<"live-thread">>,
               <<"state">> => State,
               <<"messages">> => [#{<<"role">> => <<"user">>,
                                    <<"content">> => Prompt}]}),
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    try
        Ref = gun:post(Conn, "/agui/run",
                       [{<<"content-type">>, <<"application/json">>}], Body),
        {response, nofin, 200, _Hdrs} = gun:await(Conn, Ref, 30000),
        stream(Conn, Ref, <<>>, [])
    after
        gun:close(Conn)
    end.

stream(Conn, Ref, Buf, Acc) ->
    case gun:await(Conn, Ref, 120000) of
        {data, nofin, Chunk} ->
            {Rest, Events} = decode(<<Buf/binary, Chunk/binary>>),
            stream(Conn, Ref, Rest, Acc ++ show(Events));
        {data, fin, Chunk} ->
            {_Rest, Events} = decode(<<Buf/binary, Chunk/binary>>),
            Acc ++ show(Events);
        {error, Reason} ->
            io:format("  !! stream error: ~p~n", [Reason]),
            Acc
    end.

decode(Data) ->
    {Rest, Frames} = beamai_sse:parse(Data),
    {Rest, [beamai_utils:parse_json(maps:get(data, F)) || F <- Frames]}.

show(Events) ->
    [begin io:format("  ~-26ts ~ts~n", [maps:get(<<"type">>, E), brief(E)]), E end
     || E <- Events].

brief(#{<<"type">> := <<"TEXT_MESSAGE_CONTENT">>, <<"delta">> := D}) -> clip(D);
brief(#{<<"type">> := <<"TOOL_CALL_START">>, <<"toolCallName">> := N}) -> N;
brief(#{<<"type">> := <<"TOOL_CALL_ARGS">>, <<"delta">> := D}) -> clip(D);
brief(#{<<"type">> := <<"TOOL_CALL_RESULT">>, <<"content">> := C}) -> clip(C);
brief(#{<<"type">> := <<"REASONING_MESSAGE_CONTENT">>, <<"delta">> := D}) -> clip(D);
brief(#{<<"type">> := <<"STATE_SNAPSHOT">>, <<"snapshot">> := S}) -> fmt(S);
brief(#{<<"type">> := <<"STATE_DELTA">>, <<"delta">> := D}) -> fmt(D);
brief(#{<<"type">> := <<"RUN_ERROR">>, <<"message">> := M}) -> M;
brief(_) -> <<>>.

%% 按**字符**截断而不是字节：binary:part/3 会把一个 UTF-8 码点劈成两半
clip(B) ->
    case string:length(B) > 40 of
        true -> unicode:characters_to_binary([string:slice(B, 0, 40), "..."]);
        false -> B
    end.

fmt(T) -> iolist_to_binary(io_lib:format("~tp", [T])).

%%====================================================================
%% 校验
%%====================================================================

report(Baseline, Events) ->
    Types = [maps:get(<<"type">>, E) || E <- Events],
    io:format("~n--- event counts ---~n"),
    _ = [io:format("  ~-26ts ~p~n", [T, N])
         || {T, N} <- lists:sort(maps:to_list(counts(Types)))],

    %% 共享状态闭环：客户端把补丁打到自己那份基线上
    Snapshot = first_value(<<"STATE_SNAPSHOT">>, <<"snapshot">>, Events, Baseline),
    Deltas = [maps:get(<<"delta">>, E) || E <- Events,
              maps:get(<<"type">>, E) =:= <<"STATE_DELTA">>],
    Client = lists:foldl(fun apply_patch/2, Snapshot, Deltas),
    io:format("~n--- shared state ---~n"),
    io:format("  baseline    ~tp~n", [Baseline]),
    io:format("  snapshot    ~tp~n", [Snapshot]),
    io:format("  patches     ~tp~n", [Deltas]),
    io:format("  client-side ~tp~n", [Client]),

    Text = iolist_to_binary([maps:get(<<"delta">>, E) || E <- Events,
                             maps:get(<<"type">>, E) =:= <<"TEXT_MESSAGE_CONTENT">>]),
    io:format("~n--- final answer ---~n  ~ts~n", [Text]),
    #{types => Types, state => Client, text => Text}.

counts(Types) ->
    lists:foldl(fun(T, Acc) -> maps:update_with(T, fun(N) -> N + 1 end, 1, Acc) end,
                #{}, Types).

first_value(Type, Key, Events, Default) ->
    case [maps:get(Key, E) || E <- Events, maps:get(<<"type">>, E) =:= Type] of
        [V | _] -> V;
        [] -> Default
    end.

%% @private 极简 JSON Patch 应用（只认 add/replace/remove，够验证闭环）
apply_patch(Ops, State) ->
    lists:foldl(fun(Op, Acc) -> apply_op(Op, Acc) end, State, Ops).

apply_op(#{<<"op">> := <<"remove">>, <<"path">> := Path}, State) ->
    update_at(segments(Path), State, remove);
apply_op(#{<<"path">> := Path, <<"value">> := Value}, State) ->
    update_at(segments(Path), State, {set, Value});
apply_op(_Op, State) ->
    State.

segments(<<"/", Rest/binary>>) ->
    [unescape(S) || S <- binary:split(Rest, <<"/">>, [global])];
segments(_) ->
    [].

unescape(S) ->
    binary:replace(binary:replace(S, <<"~1">>, <<"/">>, [global]),
                   <<"~0">>, <<"~">>, [global]).

update_at([Key], Map, remove) when is_map(Map) ->
    maps:remove(Key, Map);
update_at([Key], Map, {set, Value}) when is_map(Map) ->
    Map#{Key => Value};
update_at([Key | Rest], Map, Action) when is_map(Map) ->
    Child = maps:get(Key, Map, #{}),
    Map#{Key => update_at(Rest, Child, Action)};
update_at(_Path, Other, _Action) ->
    Other.
