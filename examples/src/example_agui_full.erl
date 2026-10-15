%%%-------------------------------------------------------------------
%%% @doc AG-UI 全能力走查 —— **离线可跑**的九个场景
%%%
%%% 一条命令把 `beamai_agui` 的每一样能力都跑一遍，把事件流打在屏幕上，并对
%%% 每个场景的关键契约下断言。用来回答一个问题：**这些东西今天还成立吗**。
%%%
%%% ```
%%% cd examples && make compile
%%% ERL_LIBS=../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts rebar3 shell
%%% 1> example_agui_full:run().              %% 九个场景，离线，约 3 秒
%%% 2> example_agui_full:run(reconnect).     %% 只跑一个
%%% 3> example_agui_full:scenarios().        %% 有哪些
%%%
%%% %% 同样九个场景，换成**真模型**（MiniMax M2.7，需要 MINIMAX_API_KEY）
%%% 4> example_agui_full:live().
%%% 5> example_agui_full:live(a2ui).
%%% 6> example_agui_full:live(zhipu, a2ui).  %% 换一家
%%% ```
%%%
%%% 离线模式的 LLM 由 `example_agui_scripted` 照剧本演 —— 不需要 API key，事件
%%% 序列每次一样。
%%%
%%% == 离线与实机验的不是同一件事 ==
%%%
%%% 离线验的是**协议的形状**：谁先谁后、成不成对、带不带 id。这些与模型说了什么
%%% 无关，所以断言可以全是硬的。
%%%
%%% 实机验的是**假设成立与否**：真 provider 的 chunk 形状认得出吗、模型看得懂
%%% 插件的提示词吗、它真会去调那个工具吗。这一半**取决于模型这一轮的选择** ——
%%% 所以那类断言在实机模式下降级成 `[skip]` 而不是 `[FAIL]`：模型没调工具是它的
%%% 选择，不是我们的 bug；而协议形状那一半照样是硬断言，模型选什么都不该破。
%%%
%%% == 九个场景对应的能力 ==
%%%
%%% | 场景 | 验什么 |
%%% |---|---|
%%% | `basic` | 完整往返：SSE 帧带 `id:`、事件序列、逐字的 `TOOL_CALL_ARGS` |
%%% | `reconnect` | **断线重连不丢输出**：真断开，再凭 `Last-Event-ID` 接回来 |
%%% | `stop` | **跨请求 stop**：另一个 HTTP 请求凭 threadId 停掉在跑的 run |
%%% | `threads` | 只读面：`/info` 的能力位、线程列表、共享状态、事件缓冲 |
%%% | `hitl` | 跨请求 HITL：`outcome:"interrupt"` 收口 → 下一轮 `resume[]` 答复 |
%%% | `a2ui` | 声明式生成式 UI：三条 op 的顺序 + `replace` |
%%% | `genui` | 沙箱生成式 UI：snapshot 在前、html 是数组、`generating false` 收尾 |
%%% | `mcp_apps` | 带 UI 的 MCP 工具出结果时补一条 activity |
%%% | `subagent` | 子 agent lane：`SUBAGENT_*` + 每条事件带 `subagentRunId` |
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_full).

-export([run/0, run/1, live/0, live/1, live/2, scenarios/0]).

-define(LISTENER, example_agui_full_listener).
-define(AGENT, <<"demo-agent">>).

%%====================================================================
%% 入口
%%====================================================================

scenarios() ->
    [basic, reconnect, stop, threads, hitl, a2ui, genui, mcp_apps, subagent].

%% @doc 九个场景，离线（剧本化 LLM）
run() ->
    all(scripted).

%% @doc 只跑一个，离线
run(Scenario) ->
    one(scripted, Scenario).

%% @doc 九个场景，**真模型**（缺省 MiniMax M2.7）
live() ->
    live(minimax).

%% @doc 换一家 provider 跑全部，或用缺省 provider 跑一个
live(Scenario) when is_atom(Scenario) ->
    case lists:member(Scenario, scenarios()) of
        true -> one({live, minimax}, Scenario);
        false -> all({live, Scenario})
    end.

%% @doc 指定 provider + 场景
live(Provider, Scenario) ->
    one({live, Provider}, Scenario).

%%--------------------------------------------------------------------

all(Mode) ->
    io:format("~n### LLM: ~ts~n", [describe(Mode)]),
    Results = [{S, guarded(Mode, S)} || S <- scenarios()],
    summary(Results),
    Results.

one(Mode, Scenario) ->
    put(mode, Mode),
    ok = boot(),
    try
        do_run(Scenario)
    after
        shutdown()
    end.

guarded(Mode, Scenario) ->
    io:format("~n~ts~n", [banner(Scenario)]),
    erase(skipped),
    try one(Mode, Scenario) of
        ok -> case get(skipped) of undefined -> pass; _ -> partial end
    catch
        Class:Reason:Stack ->
            io:format("  !! ~tp:~tp~n     ~tp~n", [Class, Reason, hd(Stack)]),
            fail
    end.

describe(scripted) -> "剧本化（离线，不需要 API key）";
describe({live, Provider}) ->
    io_lib:format("真模型 ~ts", [model_name(Provider)]).

model_name(minimax) -> "MiniMax m2.7";
model_name(zhipu) -> "ZHIPU glm-5.3-flash";
model_name(Other) -> atom_to_list(Other).

summary(Results) ->
    io:format("~n=== 走查结果 ===~n"),
    [io:format("  ~-10s ~ts~n", [atom_to_list(S), mark(R)]) || {S, R} <- Results],
    Failed = [S || {S, fail} <- Results],
    case Failed of
        [] -> io:format("  全部通过~n");
        _ -> io:format("  失败：~p~n", [Failed])
    end.

mark(pass) -> "PASS";
mark(partial) -> "PASS (模型没走到的部分已跳过)";
mark(fail) -> "FAIL".

banner(S) ->
    Line = lists:duplicate(60, $=),
    io_lib:format("~s~n== ~s~n~s", [Line, atom_to_list(S), Line]).

%%====================================================================
%% 环境
%%====================================================================

boot() ->
    %% 事件里全是中文，不设 unicode 的话 io:format 直接 badarg
    _ = io:setopts([{encoding, unicode}]),
    {ok, _} = application:ensure_all_started(beamai_agui),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    ok.

shutdown() ->
    _ = example_agui_scripted:uninstall(),
    %% **先收会话再停 listener**：`reconnect` 那个场景故意留了一个还在跑的 run
    %% （断线之后 run 照跑，那正是它要演的事）。不收就带进下一个场景 —— 上一轮的
    %% run 还在往一条已经没人的流里写，下一个场景起 listener 时撞见它，
    %% 客户端拿到的是 `{stream_error, closed}`。每个场景自己收干净。
    _ = [catch beamai_agui_sessions:forget(T) || T <- beamai_agui_sessions:list()],
    _ = catch cowboy:stop_listener(?LISTENER),
    _ = application:unset_env(beamai_agui, subagent_events),
    ok.

%% @private 起一个带指定插件配置的服务，返回端口
%%
%% 每个场景起自己那一台：插件是**端点配置**（`event_transform` / `input_transform`）
%% 与 `/info` 的能力位，不同场景要的不一样。
serve(Config) ->
    ok = beamai_agui_registry:register(?AGENT, maps:get(factory, Config)),
    RunConfig = maps:with([event_transform, input_transform, suggestions,
                           open_generative_ui, a2ui], Config#{agent => ?AGENT}),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/agui/run", beamai_agui_cowboy_handler, RunConfig#{agent => ?AGENT}},
            {"/agui/info", beamai_agui_cowboy_handler, RunConfig#{mode => info}},
            {"/agui/threads", beamai_agui_cowboy_handler, #{mode => threads}},
            {"/agui/threads/:thread", beamai_agui_cowboy_handler, #{mode => thread}},
            {"/agui/threads/:thread/:sub", beamai_agui_cowboy_handler, #{mode => thread}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, 0}],
                                 #{env => #{dispatch => Dispatch}}),
    ranch:get_port(?LISTENER).

%%====================================================================
%% Agent 工厂
%%====================================================================

%% @private 最小 agent：一个天气工具
factory() -> factory(#{}).

factory(Opts) ->
    Model = llm(),
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:add_chat_model(beamai_chat_client:new(), Model),
        K1 = lists:foldl(fun(T, K) -> beamai_chat_client:add_tool(K, T) end,
                         K0, tools(Opts, RuntimeCfg)),
        Base0 = #{chat_client => K1, memory => false,
                  system_prompt => <<"你是演示助手。"/utf8>>},
        Base = case maps:get(with_plugin_tools, Opts, undefined) of
                   a2ui -> beamai_agui_a2ui:with_tool(Base0);
                   genui -> example_agui_genui:with_tool(Base0);
                   _ -> Base0
               end,
        Interrupt = case maps:get(interrupt_tools, Opts, []) of
                        [] -> #{};
                        L -> #{interrupt_tools => L}
                    end,
        beamai_agent:new(maps:merge(maps:merge(Base, Interrupt), RuntimeCfg))
    end.

%% @private 这个场景的工具表
%%
%% **只给这个场景要演的那把** —— 实机联调时发现，工具表里多一把不相干的
%% `get_weather`，模型就会拿它去应付「画个面板」这种请求，插件那条路根本没走到。
%% 演示要演得成，就别在台上放无关的道具。
tools(Opts, RuntimeCfg) ->
    [weather_tool() || maps:get(weather, Opts, false)]
    ++ [chart_tool() || maps:get(mcp_app_tool, Opts, false)]
    ++ [delegate_tool(RuntimeCfg) || maps:get(delegate, Opts, false)].

weather_tool() ->
    #{name => <<"get_weather">>,
      description => <<"查一个城市的天气"/utf8>>,
      parameters => #{<<"city">> => #{type => string, required => true,
                                      description => <<"城市名"/utf8>>}},
      handler => fun(Args) ->
          City = maps:get(<<"city">>, Args, <<"某地"/utf8>>),
          {ok, <<City/binary, "：晴，24 度。"/utf8>>}
      end}.

%% 带 UI 资源的工具 —— `_meta.ui/resourceUri` 是 MCP Apps 的唯一标记
chart_tool() ->
    #{name => <<"render_chart">>,
      description => <<"画一张图表"/utf8>>,
      parameters => #{<<"series">> => #{type => string, required => true}},
      <<"_meta">> => #{<<"ui/resourceUri">> => <<"ui://demo/chart">>,
                       <<"server_id">> => <<"demo-mcp">>},
      handler => fun(_Args) -> {ok, <<"chart rendered">>} end}.

%% 委派工具由 `beamai_agui_subagent` 包出来；observer 关着时它是 undefined，
%% 包出来的就是上游原样的工具 —— 「不塞就什么都不会发生」
delegate_tool(RuntimeCfg) ->
    beamai_agui_subagent:delegate_tool(
      #{name => <<"research">>,
        description => <<"把一个子任务交给子 agent"/utf8>>,
        subagent => fun(_Args, _Ctx) ->
            K = beamai_chat_client:add_chat_model(beamai_chat_client:new(), llm()),
            #{chat_client => K, memory => false}
        end},
      maps:get(subagent_observer, RuntimeCfg, undefined)).

%% @private 这一轮用哪个模型
llm() ->
    case mode() of
        scripted -> beamai_chat_model:create(mock, #{});
        %% max_tokens 给足：缺省 2048 装不下一棵组件树 / 一段 HTML，
        %% 截断的 JSON 解不出来 —— 插件只会拿到空参数，症状是"调了但什么都没画"
        {live, minimax} -> example_llm_config:minimax(#{api_key => env("MINIMAX_API_KEY"),
                                                       model => <<"m2.7">>,
                                                       max_tokens => 8192});
        {live, zhipu} -> example_llm_config:anthropic(#{api_key => env("ZHIPU_API_KEY"),
                                                        model => <<"glm-5.3-flash">>,
                                                        max_tokens => 8192})
    end.

env(Name) ->
    case os:getenv(Name) of
        false -> error({missing_env, Name});
        V -> list_to_binary(V)
    end.

mode() ->
    case get(mode) of
        undefined -> scripted;
        M -> M
    end.

live_mode() -> mode() =/= scripted.

%% @private 装剧本 —— 实机模式下什么都不装（真模型自己决定说什么）
script(Scenes) ->
    case live_mode() of
        true -> ok;
        false -> example_agui_scripted:install(Scenes)
    end.

%%====================================================================
%% 场景 1：完整往返
%%====================================================================

do_run(basic) ->
    ok = script([example_agui_scripted:tool_call(<<"c1">>, <<"get_weather">>,
                                                 <<"{\"city\":\"北京\"}"/utf8>>),
                 example_agui_scripted:reasoning_then_text(<<"查到了"/utf8>>,
                                                           <<"北京今天晴。"/utf8>>)]),
    Port = serve(#{factory => factory(#{weather => true})}),
    {200, Headers, Data} = post(Port, "/agui/run",
                                body(<<"t-basic">>,
                                     <<"用 get_weather 查一下北京天气，然后一句话告诉我。"/utf8>>)),
    Frames = frames(Data),
    print_frames(Frames),
    io:format("  run id 回在头上：~ts~n", [header(<<"x-agui-run-id">>, Headers)]),

    %% 每一帧都带 `id:`，号从 0 起连续 —— 这是断线重连的全部依据
    Ids = [Id || #{id := Id} <- Frames],
    expect(lists:seq(0, length(Frames) - 1), [binary_to_integer(I) || I <- Ids],
           "SSE 帧的 id: 连续无洞"),
    %% 工具参数是**逐字**流出来的（不是一次性发完整 JSON）
    %% 逐字与否**取决于 provider**：MiniMax 的 anthropic 口一两片就把参数给完了，
    %% 那不是我们的问题（增量路照样成立，只是没有打字机效果）
    expect_model(true, length(pick(<<"TOOL_CALL_ARGS">>, Frames)) > 3,
                 "TOOL_CALL_ARGS 逐字增量（provider 一口给完时会跳过）"),
    %% 思考走 REASONING_* 一族，不是已弃用的 THINKING_*
    expect_model(true, length(pick(<<"REASONING_MESSAGE_CONTENT">>, Frames)) > 0,
                 "REASONING_* 出来了（模型这轮没思考就会跳过）"),
    %% 文本三件套必须成对 —— 这条与模型说什么无关
    expect(length(pick(<<"TEXT_MESSAGE_START">>, Frames)),
           length(pick(<<"TEXT_MESSAGE_END">>, Frames)), "文本消息开合成对"),
    expect(<<"RUN_FINISHED">>, type(lists:last(Frames)), "终态是最后一条"),
    ok;

%%====================================================================
%% 场景 2：断线重连
%%====================================================================

do_run(reconnect) ->
    ok = script([example_agui_scripted:text(long_text())]),
    Port = serve(#{factory => factory()}),
    Thread = "t-reconnect",

    %% 起 run，读几帧就**真的把连接掐了**
    {Conn, Ref} = open(Port, <<"POST">>, "/agui/run",
                       [{<<"content-type">>, <<"application/json">>}],
                       body(list_to_binary(Thread), <<"讲个长故事"/utf8>>)),
    {response, nofin, 200, _} = gun:await(Conn, Ref, 5000),
    {Seen, Last} = read_some(Conn, Ref, 4),
    ok = gun:close(Conn),
    io:format("  断开前收到 ~p 帧，最后一个 id: ~p~n", [length(Seen), Last]),

    %% run 还在跑 —— 另开一个连接凭水位接回来
    {200, _, Data} = get(Port, "/agui/threads/" ++ Thread ++ "/stream?since="
                         ++ integer_to_list(Last)),
    Resumed = frames(Data),
    ResumedIds = [binary_to_integer(I) || #{id := I} <- Resumed],
    io:format("  重连补回 ~p 帧~n", [length(Resumed)]),

    %% 模型可能答得很短，断开前就已经全收完了 —— 那时重连补回的是空集，
    %% 这不是丢失，是本来就没剩下什么。两种情况分开断言。
    case ResumedIds of
        [] ->
            expect_model(true, false, "断开时 run 还没跑完（模型答得太短）");
        _ ->
            expect(Last + 1, hd(ResumedIds), "接着水位往下，一条不漏"),
            expect(true, lists:all(fun(N) -> N > Last end, ResumedIds), "一条不重"),
            expect(<<"RUN_FINISHED">>, type(lists:last(Resumed)),
                   "断线那一下没把这轮的输出弄丢")
    end,
    ok;

%%====================================================================
%% 场景 3：跨请求 stop
%%====================================================================

do_run(stop) ->
    ok = script([example_agui_scripted:hang()]),
    Port = serve(#{factory => factory()}),
    Thread = "t-stop",

    {Conn, Ref} = open(Port, <<"POST">>, "/agui/run",
                       [{<<"content-type">>, <<"application/json">>}],
                       body(list_to_binary(Thread), <<"慢慢想"/utf8>>)),
    {response, nofin, 200, _} = gun:await(Conn, Ref, 5000),
    {_Seen, _Last} = read_some(Conn, Ref, 3),

    %% **另一个 HTTP 请求**，手上只有 threadId
    {200, _, StopBody} = post(Port, "/agui/threads/" ++ Thread ++ "/stop", <<>>),
    io:format("  stop -> ~ts~n", [StopBody]),
    Stopped = maps:get(<<"stopped">>, beamai_utils:parse_json(StopBody)),

    %% 停稳的信号是终态事件，不是 stop 的返回值
    Rest = drain(Conn, Ref),
    ok = gun:close(Conn),
    print_frames(Rest),
    Terminal = lists:last(Rest),
    %% 终态一定有 —— 这条与模型无关（会话替被杀的 run 补）
    expect(<<"RUN_FINISHED">>, type(Terminal), "被停的 run 也有终态事件"),
    %% 但「是不是我们停的」取决于模型有没有在我们发 stop 之前就答完
    expect_model(true, Stopped, "stop 时 run 还在跑"),
    case Stopped of
        true -> expect(#{<<"status">> => <<"cancelled">>},
                       maps:get(<<"result">>, json(Terminal)),
                       "如实说是取消，不是跑完了");
        false -> ok
    end,
    ok;

%%====================================================================
%% 场景 4：只读面
%%====================================================================

do_run(threads) ->
    ok = script([example_agui_scripted:text(<<"好的。"/utf8>>)]),
    Port = serve(#{factory => factory(), a2ui => true, open_generative_ui => true}),

    {200, _, Info} = get(Port, "/agui/info"),
    I = beamai_utils:parse_json(Info),
    io:format("  /info agents = ~tp~n", [maps:keys(maps:get(<<"agents">>, I))]),
    io:format("  /info threadEndpoints = ~tp~n", [maps:get(<<"threadEndpoints">>, I)]),
    %% agents 是**字典不是数组** —— 给数组前端会把下标当 agent id
    expect(true, is_map(maps:get(<<"agents">>, I)), "agents 是以 id 为键的字典"),
    Caps = maps:get(<<"capabilities">>, maps:get(?AGENT, maps:get(<<"agents">>, I))),
    expect(true, maps:get(<<"interrupts">>, maps:get(<<"humanInTheLoop">>, Caps)),
           "如实声明走 interrupt 协议"),
    expect(true, maps:get(<<"a2uiEnabled">>, I), "装了 A2UI 就报 true"),

    {200, _, _} = post(Port, "/agui/run",
                       body_with_state(<<"t-ro">>, <<"记一下"/utf8>>,
                                       #{<<"count">> => 2})),
    {200, _, List} = get(Port, "/agui/threads"),
    io:format("  /threads = ~ts~n", [List]),
    {200, _, State} = get(Port, "/agui/threads/t-ro/state"),
    io:format("  /threads/t-ro/state = ~ts~n", [State]),
    {200, _, Events} = get(Port, "/agui/threads/t-ro/events?since=1"),
    N = length(maps:get(<<"events">>, beamai_utils:parse_json(Events))),
    io:format("  /threads/t-ro/events?since=1 -> ~p 条~n", [N]),

    expect(#{<<"count">> => 2},
           maps:get(<<"state">>, beamai_utils:parse_json(State)),
           "共享状态由流上的 SNAPSHOT 折出来"),
    expect(true, N > 0, "事件缓冲认 since"),
    ok;

%%====================================================================
%% 场景 5：跨请求 HITL
%%====================================================================

do_run(hitl) ->
    %% 模型调一个**只有声明没有实现**的工具 → 停下来等人
    ok = script([example_agui_scripted:tool_call(<<"ask1">>, <<"ask_human">>,
                                                 <<"{\"question\":\"要继续吗\"}"/utf8>>),
                 example_agui_scripted:text(<<"好的，继续。"/utf8>>)]),
    Port = serve(#{factory => factory(
                     #{interrupt_tools =>
                           [#{name => <<"ask_human">>,
                              description => <<"在动手之前向用户确认一句"/utf8>>,
                              parameters =>
                                  #{<<"question">> =>
                                        #{type => string, required => true,
                                          description => <<"要问的问题"/utf8>>}}}]})}),

    io:format("  --- 请求一：跑到一半停下来等人 ---~n"),
    {200, _, D1} = post(
      Port, "/agui/run",
      body(<<"t-hitl">>,
           <<"我要删掉生产库里的一张表。动手之前**必须**先调用 ask_human "
             "问我确认一次。"/utf8>>)),
    F1 = frames(D1),
    print_frames(F1),
    Finished = json(lists:last(F1)),
    case maps:get(<<"outcome">>, Finished, undefined) of
        undefined ->
            %% 模型这轮没去问人（实机下完全可能）—— 后面那半就没得验
            expect_model(true, false, "模型停下来问人了"),
            ok;
        Outcome ->
            hitl_resume(Port, F1, Outcome)
    end;

%%====================================================================
%% 场景 6：A2UI
%%====================================================================

do_run(a2ui) ->
    Args = <<"{\"surfaceId\":\"dash\",\"components\":["
             "{\"id\":\"root\",\"component\":\"Column\",\"children\":[\"t\"]},"
             "{\"id\":\"t\",\"component\":\"Text\",\"text\":\"Hello\"}],"
             "\"data\":{\"n\":1}}">>,
    ok = script([example_agui_scripted:tool_call(<<"c1">>, <<"render_a2ui">>, Args),
                 example_agui_scripted:text(<<"界面贴好了。"/utf8>>)]),
    Port = serve(#{factory => factory(#{with_plugin_tools => a2ui}),
                   event_transform => beamai_agui_a2ui:event_transform(),
                   input_transform => beamai_agui_a2ui:input_transform(),
                   a2ui => true}),

    {200, _, Data} = post(
      Port, "/agui/run",
      body(<<"t-a2ui">>,
           <<"调用 render_a2ui 画一个销售看板：一个 Column 作 root，里面放一个 "
             "h2 标题 Text 和一个 Row，Row 里两个 Text 分别显示"
             "「用户 1,248」和「收入 ¥52,300」。组件不要超过 6 个。"/utf8>>)),
    Frames = frames(Data),
    print_frames(Frames),
    case pick(<<"ACTIVITY_SNAPSHOT">>, Frames) of
        [] ->
            expect_model(true, false, "模型调了 render_a2ui");
        [Snapshot | _] ->
            S = json(Snapshot),
            Ops = maps:get(<<"a2ui_operations">>, maps:get(<<"content">>, S)),
            io:format("  ops = ~tp~n", [[op_name(O) || O <- Ops]]),
            expect(<<"a2ui-surface">>, maps:get(<<"activityType">>, S), "activityType"),
            expect(true, maps:get(<<"replace">>, S), "replace = 整块换掉，不需要 delta"),
            %% **顺序是协议要求的**：先建面，再给组件（有 data 才有第三条）
            expect([<<"createSurface">>, <<"updateComponents">>],
                   lists:sublist([op_name(O) || O <- Ops], 2), "op 顺序：先建面再给组件"),
            %% 模型生成的组件树用的必须是 catalog 里有的组件 —— 这正是 A2UI 的卖点
            report_components(Ops)
    end,
    ok;

%%====================================================================
%% 场景 7：Open Generative UI
%%====================================================================

do_run(genui) ->
    Args = <<"{\"initialHeight\":320,\"css\":\"body{margin:0}\","
             "\"html\":\"<div class=x>hi</div>\",\"jsExpressions\":[\"init()\"]}">>,
    ok = script([example_agui_scripted:tool_call(<<"c1">>, <<"generateSandboxedUi">>, Args),
                 example_agui_scripted:text(<<"UI 生成好了。"/utf8>>)]),
    Port = serve(#{factory => factory(#{with_plugin_tools => genui}),
                   event_transform => example_agui_genui:event_transform(),
                   open_generative_ui => true}),

    {200, _, Data} = post(
      Port, "/agui/run",
      body(<<"t-genui">>,
           <<"调用 generateSandboxedUi 生成一个**很小**的计数器卡片："
             "initialHeight 200，css 十行以内，html 一个 div 加一个按钮，"
             "jsExpressions 一条。整体控制在 30 行以内。"/utf8>>)),
    Frames = frames(Data),
    print_frames(Frames),
    Types = [type(F) || F <- Frames],
    Paths = [maps:get(<<"path">>, Op)
             || F <- Frames, type(F) =:= <<"ACTIVITY_DELTA">>,
                Op <- maps:get(<<"patch">>, json(F))],
    case pick(<<"ACTIVITY_SNAPSHOT">>, Frames) of
        [] ->
            expect_model(true, false, "模型调了 generateSandboxedUi");
        _ ->
            io:format("  patch 路径 = ~tp~n", [Paths]),
            %% **snapshot 必须排在任何 delta 之前**，否则前端把 delta 悄悄丢掉
            expect(true, index_of(<<"ACTIVITY_SNAPSHOT">>, Types)
                         < index_of(<<"ACTIVITY_DELTA">>, Types),
                   "snapshot 在 delta 之前"),
            %% 工具卡片被扣住，activity 先出（否则前端先看见卡片再看见 UI，闪一下）
            expect(true, index_of(<<"ACTIVITY_SNAPSHOT">>, Types)
                         < index_of(<<"TOOL_CALL_START">>, Types),
                   "扣住工具卡片直到第一条 activity 出去"),
            expect(<<"/generating">>, lists:last(Paths), "generating false 收尾"),
            %% **html 是数组不是字符串**（模型没给 html 就没这两条）
            expect_model(true, lists:member(<<"/html">>, Paths) andalso
                               lists:member(<<"/html/-">>, Paths),
                         "html 先建数组再追加")
    end,
    ok;

%%====================================================================
%% 场景 8：MCP Apps
%%====================================================================

do_run(mcp_apps) ->
    ok = script([example_agui_scripted:tool_call(<<"c1">>, <<"render_chart">>,
                                                 <<"{\"series\":\"sales\"}">>),
                 example_agui_scripted:text(<<"图表在上面。"/utf8>>)]),
    Apps = beamai_agui_mcp_apps:app_tools([chart_tool(), weather_tool()]),
    io:format("  带 UI 的工具 = ~tp~n", [maps:keys(Apps)]),
    Port = serve(#{factory => factory(#{mcp_app_tool => true}),
                   event_transform => beamai_agui_mcp_apps:event_transform(#{apps => Apps})}),

    {200, _, Data} = post(Port, "/agui/run",
                          body(<<"t-mcp">>,
                               <<"调用 render_chart 画一张 series 为 sales 的图。"/utf8>>)),
    Frames = frames(Data),
    print_frames(Frames),
    %% 只认带 UI 的工具 —— 普通工具就是普通工具（与模型无关，装配期就定了）
    expect([<<"render_chart">>], maps:keys(Apps), "只认带 UI 的工具"),
    case pick(<<"ACTIVITY_SNAPSHOT">>, Frames) of
        [] ->
            expect_model(true, false, "模型调了 render_chart");
        [Snapshot | _] ->
            Content = maps:get(<<"content">>, json(Snapshot)),
            io:format("  activity content = ~tp~n", [Content]),
            expect(<<"ui://demo/chart">>, maps:get(<<"resourceUri">>, Content), "资源 URI"),
            expect(true, is_map(maps:get(<<"toolInput">>, Content)),
                   "带上这次的调用参数")
    end,
    %% 白名单：那块界面跑在浏览器里
    expect({error, {method_not_allowed, <<"resources/subscribe">>}},
           beamai_agui_mcp_apps:proxy_request(
             [#{server_id => <<"demo-mcp">>, client => none}],
             #{<<"serverId">> => <<"demo-mcp">>,
               <<"method">> => <<"resources/subscribe">>}),
           "UI 代理只放行四个方法"),
    ok;

%%====================================================================
%% 场景 9：子 agent lane
%%====================================================================

do_run(subagent) ->
    application:set_env(beamai_agui, subagent_events, true),
    ok = script([example_agui_scripted:tool_call(<<"d1">>, <<"research">>,
                                                 <<"{\"task\":\"查一下背景\"}"/utf8>>),
                 example_agui_scripted:text(<<"子 agent 说完了。"/utf8>>)]),
    Port = serve(#{factory => factory(#{delegate => true})}),

    {200, _, Data} = post(
      Port, "/agui/run",
      body(<<"t-sub">>,
           <<"调用 research 工具，把「介绍一下 Erlang 的进程模型」这个子任务"
             "交给子 agent，然后把它的结论转述给我。"/utf8>>)),
    Frames = frames(Data),
    print_frames(Frames),
    case pick(<<"SUBAGENT_STARTED">>, Frames) of
        [] ->
            expect_model(true, false, "模型委派给了子 agent");
        Started ->
            Lane = maps:get(<<"subagentRunId">>, json(hd(Started))),
            Tagged = [F || F <- Frames,
                           maps:get(<<"subagentRunId">>, json(F), undefined) =:= Lane],
            io:format("  lane ~ts 上有 ~p 条事件~n", [Lane, length(Tagged)]),
            %% 开合成对 —— 发了 STARTED 没有收尾，前端那张卡片永远转圈
            expect(length(Started),
                   length(pick(<<"SUBAGENT_FINISHED">>, Frames))
                   + length(pick(<<"SUBAGENT_ERROR">>, Frames)), "开合成对"),
            %% 子 agent 说的话在 lane 里，**每条都带 subagentRunId**
            expect(true, length(Tagged) > 2, "lane 里有子 agent 自己的事件"),
            %% 主线的事件不带这个字段 —— 否则前端把父说的话画进子卡片
            MainText = [F || F <- pick(<<"TEXT_MESSAGE_CONTENT">>, Frames),
                             maps:get(<<"subagentRunId">>, json(F), undefined) =:= undefined],
            expect(true, length(MainText) > 0, "主线的事件不带 lane 标签")
    end,
    ok.

%% do_run(hitl) 的后半段（拆成独立函数，否则会插进 do_run 的子句链里）
hitl_resume(Port, F1, Outcome) ->
    [Interrupt] = maps:get(<<"interrupts">>, Outcome),
    InterruptId = maps:get(<<"id">>, Interrupt),
    io:format("  interrupt id = ~ts (= 被中断调用的 tool-call id)~n", [InterruptId]),

    %% 前端看到的是一个**没有结果**的工具调用 —— 那就是「等你回话」
    expect(<<"interrupt">>, maps:get(<<"type">>, Outcome), "RUN_FINISHED 带 interrupt outcome"),
    expect([], pick(<<"TOOL_CALL_RESULT">>, F1), "暂停时那次调用没有结果"),

    io:format("  --- 请求二：把人的决定放在 resume[] 里带回来 ---~n"),
    Body2 = beamai_utils:encode_json(
              #{<<"threadId">> => <<"t-hitl">>,
                <<"messages">> => [user_msg(<<"继续"/utf8>>)],
                <<"resume">> => [#{<<"interruptId">> => InterruptId,
                                   <<"status">> => <<"reply">>,
                                   <<"payload">> => #{<<"message">> => <<"可以，继续"/utf8>>}}]}),
    {200, _, D2} = post(Port, "/agui/run", Body2),
    F2 = frames(D2),
    print_frames(F2),
    [Result] = pick(<<"TOOL_CALL_RESULT">>, F2),
    expect(InterruptId, maps:get(<<"toolCallId">>, json(Result)),
           "补上的结果对回了那次调用"),
    ok.

%% @private 模型生成的组件树用了哪些组件 —— A2UI 的卖点就是它编不出 catalog 之外的
report_components(Ops) ->
    Catalog = maps:keys(maps:get(<<"components">>, beamai_agui_a2ui:basic_catalog())),
    Used = lists:usort([maps:get(<<"component">>, C, <<"?">>)
                        || O <- Ops, maps:is_key(<<"updateComponents">>, O),
                           C <- maps:get(<<"components">>,
                                         maps:get(<<"updateComponents">>, O), [])]),
    io:format("  模型用了这些组件 = ~tp~n", [Used]),
    expect([], [C || C <- Used, not lists:member(C, Catalog)],
           "组件全部来自 catalog（白名单成立）").

%%====================================================================
%% 断言与打印
%%====================================================================

%% @doc 取决于**模型这一轮的选择**的断言
%%
%% 剧本模式下是硬断言（剧本是确定的）；实机模式下降级成 `[skip]` —— 模型没去调
%% 那个工具是它的选择，不是我们的 bug。协议形状那一半照旧走 `expect/3`，
%% 模型选什么都不该破。
expect_model(Expected, Expected, What) ->
    io:format("  [ok]   ~ts~n", [What]);
expect_model(Expected, Got, What) ->
    case live_mode() of
        true ->
            put(skipped, true),
            io:format("  [skip] ~ts（模型这轮没走到这里）~n", [What]);
        false ->
            expect(Expected, Got, What)
    end.

expect(Expected, Expected, What) ->
    io:format("  [ok]   ~ts~n", [What]);
expect(Expected, Got, What) ->
    io:format("  [FAIL] ~ts~n         期望 ~tp~n         实得 ~tp~n",
              [What, Expected, Got]),
    error({assertion_failed, What}).

print_frames(Frames) ->
    [io:format("  ~4ts  ~-26ts ~ts~n",
               [maps:get(id, F, <<"-">>), type(F), brief(json(F))]) || F <- Frames],
    ok.

%% 只打有信息量的那几位 —— 整条 JSON 刷屏就没人看了
brief(Event) ->
    Keep = [<<"delta">>, <<"toolCallName">>, <<"content">>, <<"stepName">>,
            <<"activityType">>, <<"subagentRunId">>, <<"result">>, <<"outcome">>,
            <<"snapshot">>, <<"name">>],
    case maps:to_list(maps:with(Keep, Event)) of
        [] -> "";
        KVs -> io_lib:format("~tp", [maps:from_list(KVs)])
    end.

type(#{data := Data}) -> maps:get(<<"type">>, beamai_utils:parse_json(Data), undefined);
type(_) -> undefined.

json(#{data := Data}) -> beamai_utils:parse_json(Data).

pick(Type, Frames) -> [F || F <- Frames, type(F) =:= Type].

index_of(X, L) -> index_of(X, L, 1).
index_of(X, [X | _], N) -> N;
index_of(X, [_ | T], N) -> index_of(X, T, N + 1);
index_of(_X, [], _N) -> 999999.

op_name(Op) ->
    [Name] = [K || K <- maps:keys(Op), K =/= <<"version">>],
    Name.

long_text() ->
    <<"很久很久以前，有一个会话，它的寿命不再由 HTTP 请求的寿命决定。"/utf8>>.

%%====================================================================
%% 请求体
%%====================================================================

user_msg(Text) -> #{<<"role">> => <<"user">>, <<"content">> => Text}.

body(Thread, Prompt) ->
    beamai_utils:encode_json(#{<<"threadId">> => Thread,
                               <<"messages">> => [user_msg(Prompt)]}).

body_with_state(Thread, Prompt, State) ->
    beamai_utils:encode_json(#{<<"threadId">> => Thread,
                               <<"state">> => State,
                               <<"messages">> => [user_msg(Prompt)]}).

%%====================================================================
%% 迷你 HTTP 客户端（gun）
%%====================================================================

get(Port, Path) -> request(Port, <<"GET">>, Path, [], <<>>).

post(Port, Path, Body) ->
    request(Port, <<"POST">>, Path,
            [{<<"content-type">>, <<"application/json">>}], Body).

request(Port, Method, Path, Headers, Body) ->
    {Conn, Ref} = open(Port, Method, Path, Headers, Body),
    try
        case gun:await(Conn, Ref, 20000) of
            {response, fin, Status, RespHeaders} -> {Status, RespHeaders, <<>>};
            {response, nofin, Status, RespHeaders} ->
                {ok, Data} = gun:await_body(Conn, Ref, 20000),
                {Status, RespHeaders, Data}
        end
    after
        gun:close(Conn)
    end.

open(Port, Method, Path, Headers, Body) ->
    {ok, Conn} = gun:open("127.0.0.1", Port, #{retry => 0}),
    {ok, http} = gun:await_up(Conn, 5000),
    {Conn, gun:request(Conn, Method, list_to_binary(Path), Headers, Body)}.

frames(Data) ->
    {_Rest, Frames} = beamai_sse:parse(Data),
    Frames.

header(Name, Headers) -> proplists:get_value(Name, Headers).

%% @private 读到大约 N 帧就停手（用来模拟"读到一半断开"）
read_some(Conn, Ref, N) -> read_some(Conn, Ref, N, <<>>, [], undefined).

read_some(_Conn, _Ref, N, _Buf, Acc, Last) when N =< 0 ->
    {lists:reverse(Acc), Last};
read_some(Conn, Ref, N, Buf, Acc, Last) ->
    case gun:await(Conn, Ref, 5000) of
        {data, _, Chunk} ->
            {Rest, Frames} = beamai_sse:parse(<<Buf/binary, Chunk/binary>>),
            case Frames of
                [] -> read_some(Conn, Ref, N, Rest, Acc, Last);
                _ ->
                    Ids = [binary_to_integer(maps:get(id, F)) || F <- Frames],
                    read_some(Conn, Ref, N - length(Frames), Rest,
                              lists:reverse(Frames) ++ Acc, lists:last(Ids))
            end;
        _ ->
            {lists:reverse(Acc), Last}
    end.

%% @private 把剩下的读完（到终态为止）
drain(Conn, Ref) -> drain(Conn, Ref, <<>>, []).

drain(Conn, Ref, Buf, Acc) ->
    case gun:await(Conn, Ref, 15000) of
        {data, _, Chunk} ->
            {Rest, Frames} = beamai_sse:parse(<<Buf/binary, Chunk/binary>>),
            Acc1 = Acc ++ Frames,
            case [F || F <- Frames, lists:member(type(F), [<<"RUN_FINISHED">>,
                                                           <<"RUN_ERROR">>])] of
                [] -> drain(Conn, Ref, Rest, Acc1);
                _ -> Acc1
            end;
        _ ->
            Acc
    end.
