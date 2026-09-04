%%%-------------------------------------------------------------------
%%% @doc AG-UI 联调服务器 —— 把所有能力接齐，等前端来连
%%%
%%% `example_agui_full` 是**自己发请求自己断言**的走查；这个是**真起一台服务**
%%% 停在那儿，给前端（CopilotKit 或任意 AG-UI 客户端）连。
%%%
%%% ```
%%% cd examples && make compile
%%% ERL_LIBS=../_build/default/lib:../_build/default/checkouts rebar3 shell
%%% 1> example_agui_server:start().              %% MiniMax m2.7，端口 8080
%%% 2> example_agui_server:start(3001).          %% 换端口
%%% 3> example_agui_server:start(8080, #{provider => zhipu}).
%%% 4> example_agui_server:stop().
%%% ```
%%%
%%% 缺省放行 `localhost` / `127.0.0.1` 的 **3000 / 3001 / 3002 / 3100 / 5173**
%%% 五个端口（并回 `access-control-allow-credentials: true`）。要改：
%%%
%%% ```
%%% 1> example_agui_server:start(8080, #{cors_origins => [<<"https://my.app">>]}).
%%% 2> example_agui_server:start(8080, #{cors_origins => any}).   %% 回到 `*`
%%% ```
%%%
%%% 或者不进 shell 直接跑（Ctrl-C 停）：
%%%
%%% ```
%%% ERL_LIBS=../_build/default/lib:../_build/default/checkouts \\
%%%   erl -noshell +pc unicode -pa _build/default/lib/*/ebin \\
%%%       -eval 'example_agui_server:run(8080)'
%%% ```
%%%
%%% == 端点 ==
%%%
%%% | 方法与路径 | 干什么 |
%%% |---|---|
%%% | `POST /agui/run` | RunAgentInput → SSE 事件流 |
%%% | `GET  /agui/info` | 运行时能力声明（前端第一枪打这儿） |
%%% | `GET  /agui/threads` | 会话列表 |
%%% | `GET  /agui/threads/:id/stream` | **断线重连**（认 `Last-Event-ID` / `?since=`） |
%%% | `GET  /agui/threads/:id/events` | 环形缓冲里现存的事件 |
%%% | `GET  /agui/threads/:id/state` | 该 thread 当前的共享状态 |
%%% | `POST /agui/threads/:id/stop` | 停掉在跑的 run |
%%% | `DELETE /agui/threads/:id` | 摘掉会话 |
%%%
%%% `Last-Event-ID` 已在 `access-control-allow-headers` 里放行（断线重连靠它把水位
%%% 带回来），`x-agui-run-id` 已 expose（跨源前端读得到 run id 才停得掉那条 run），
%%% 预检结果缓存 10 分钟。
%%%
%%% == 这台 agent 会什么 ==
%%%
%%% | 能力 | 怎么试 |
%%% |---|---|
%%% | 逐 token 流式 + 工具调用 | 「北京天气怎么样」 |
%%% | 共享状态（`STATE_DELTA`） | 「把买牛奶加进待办」—— 请求带 `state` 上来，改动以 JSON Patch 推回 |
%%% | HITL（interrupt 协议） | 「删掉生产库的表」—— 它会先调 `ask_human` 停下来等你 |
%%% | A2UI（声明式生成式 UI） | 「用 render_a2ui 画一个销售看板」 |
%%% | Open Generative UI（沙箱） | 「用 generateSandboxedUi 生成一个计数器卡片」 |
%%% | 子 agent lane | 「把『介绍 Erlang 进程模型』交给子 agent」（要开 `subagent_events`） |
%%%
%%% **子 agent 事件缺省关**：`@ag-ui/client` ≤ 0.0.57 见到 `SUBAGENT_STARTED`
%%% 会掐断整条流。前端版本够新再开 —— `start(Port, #{subagent_events => true})`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agui_server).

-export([start/0, start/1, start/2, stop/0, run/0, run/1]).

-define(LISTENER, example_agui_server_listener).
-define(AGENT, <<"beamai-demo">>).
-define(DEFAULT_PORT, 8080).

%%====================================================================
%% 入口
%%====================================================================

start() -> start(?DEFAULT_PORT).

start(Port) -> start(Port, #{}).

-spec start(inet:port_number(), map()) -> {ok, inet:port_number()}.
start(Port, Opts) ->
    io:setopts([{encoding, unicode}]),
    ok = application:set_env(beamai_agui, subagent_events,
                             maps:get(subagent_events, Opts, false)),
    {ok, _} = application:ensure_all_started(beamai_agui),
    {ok, _} = application:ensure_all_started(cowboy),
    Provider = maps:get(provider, Opts, minimax),
    Origins = maps:get(cors_origins, Opts, default_origins()),
    ok = beamai_agui_registry:register(?AGENT, factory(Provider)),
    {ok, _} = cowboy:start_clear(?LISTENER, [{port, Port}],
                                 #{env => #{dispatch => dispatch(Origins)}}),
    Actual = ranch:get_port(?LISTENER),
    banner(Actual, Provider, Opts, Origins),
    {ok, Actual}.

%% @doc 缺省放行的前端源
%%
%% **给列表而不是 `*`**：浏览器不允许 `*` 与凭据并存 —— 前端一旦
%% `credentials: 'include'`（或 `EventSource` 带 `withCredentials`），请求当场被
%% 浏览器拦掉，而且错误只出现在控制台，服务端日志干干净净。给列表就能回显 Origin
%% 并加 `allow-credentials: true`。
%%
%% `localhost` 与 `127.0.0.1` 是**两个不同的源**，浏览器不认为它们相同 —— 两个都放。
default_origins() ->
    [<<"http://", H/binary, ":", P/binary>>
     || H <- [<<"localhost">>, <<"127.0.0.1">>],
        P <- [<<"3000">>, <<"3001">>, <<"3002">>, <<"3100">>, <<"5173">>]].

-spec stop() -> ok.
stop() ->
    _ = [catch beamai_agui_sessions:forget(T) || T <- beamai_agui_sessions:list()],
    _ = catch cowboy:stop_listener(?LISTENER),
    ok.

%% @doc 起了就挂住不退（给 `erl -noshell -eval` 用）
run() -> run(?DEFAULT_PORT).

run(Port) ->
    {ok, _} = start(Port),
    receive stop -> ok end.

%%====================================================================
%% 路由
%%====================================================================

%% 插件挂在**端点配置**上：`event_transform` 把工具调用翻成 activity 事件，
%% `input_transform` 把 A2UI 的用户动作接回本轮输入，后面三个 boolean 是
%% `/info` 的能力位（前端据此才注册对应的 renderer）。
dispatch(Origins) ->
    RunConfig = #{
        agent => ?AGENT,
        cors_origins => Origins,
        event_transform => beamai_agui_plugin:compose(
                             [beamai_agui_a2ui:event_transform(),
                              example_agui_genui:event_transform()]),
        input_transform => beamai_agui_a2ui:input_transform(),
        a2ui => true,
        open_generative_ui => true,
        descriptions => #{?AGENT => <<"BeamAI AG-UI 演示 agent"/utf8>>}
    },
    cowboy_router:compile([
        {'_', [
            {"/agui/run", beamai_agui_cowboy_handler, RunConfig},
            {"/agui/info", beamai_agui_cowboy_handler, RunConfig#{mode => info}},
            {"/agui/threads", beamai_agui_cowboy_handler,
             #{mode => threads, cors_origins => Origins}},
            {"/agui/threads/:thread", beamai_agui_cowboy_handler,
             #{mode => thread, cors_origins => Origins}},
            {"/agui/threads/:thread/:sub", beamai_agui_cowboy_handler,
             #{mode => thread, cors_origins => Origins}}
        ]}
    ]).

%%====================================================================
%% Agent
%%====================================================================

%% @private 工厂必须**可重放**：同一个 thread 的两次请求要造出配置一致的 agent，
%% 否则从 pause_store 恢复的中断态会对不上新 agent 的工具表（HITL 就断了）。
factory(Provider) ->
    Model = llm(Provider),
    fun(_Input, RuntimeCfg) ->
        K0 = beamai_chat_client:new(#{state_slots => slots()}),
        K1 = beamai_chat_client:add_chat_model(K0, Model),
        K2 = lists:foldl(fun(T, K) -> beamai_chat_client:add_tool(K, T) end,
                         K1, [weather_tool(), add_todo_tool(),
                              delegate_tool(RuntimeCfg)]),
        %% 两个插件的工具 + 用法提示词 + catalog（挂在 chat client 上）
        Config = example_agui_genui:with_tool(
                   beamai_agui_a2ui:with_tool(
                     #{chat_client => K2,
                       system_prompt => system_prompt(),
                       %% 前端声明的工具由 runtime 自动挂成 interrupt_tools；
                       %% 这个是**服务端**声明的中断工具，用来演人工审批
                       interrupt_tools => [ask_human_tool()]})),
        beamai_agent:new(maps:merge(Config, RuntimeCfg))
    end.

system_prompt() ->
    <<"你是 BeamAI 的 AG-UI 演示助手。\n"
      "- 查天气用 get_weather；\n"
      "- 记待办用 add_todo（一次一项，可以连着调）；\n"
      "- 用户要「面板 / 看板 / 表单」这类**结构化界面**时用 render_a2ui；\n"
      "- 用户要「自由样式的卡片 / 小组件」时用 generateSandboxedUi；\n"
      "- 需要调研一个独立子任务时用 research 交给子 agent；\n"
      "- 任何**有破坏性**的操作（删数据、发邮件、付款）动手前必须先调 ask_human。\n"
      "回答用中文，简洁。"/utf8>>.

%% todos 槽的 reducer：工具写进来的是**一项**，屏障处折叠成追加
slots() ->
    #{<<"todos">> => #{init => [],
                       reduce => fun(Acc, Item) -> Acc ++ [Item] end}}.

weather_tool() ->
    #{name => <<"get_weather">>,
      description => <<"查一个城市的天气"/utf8>>,
      parameters => #{<<"city">> => #{type => string, required => true,
                                      description => <<"城市名"/utf8>>}},
      handler => fun(Args) ->
          City = maps:get(<<"city">>, Args, <<"某地"/utf8>>),
          {ok, <<City/binary, "：晴，24 度，东北风 2 级。"/utf8>>}
      end}.

%% 工具只声明**写意图**（第三个返回值），真正落进 state 由屏障处统一折叠，
%% 改动以 `STATE_DELTA`（JSON Patch）推回前端
add_todo_tool() ->
    #{name => <<"add_todo">>,
      description => <<"把一项待办加入清单"/utf8>>,
      parameters => #{<<"text">> => #{type => string, required => true,
                                      description => <<"待办内容"/utf8>>}},
      handler => fun(Args) ->
          Text = maps:get(<<"text">>, Args, <<>>),
          {ok, <<"已加入："/utf8, Text/binary>>, #{<<"todos">> => Text}}
      end}.

%% **只有声明没有实现**的工具：模型调到它就暂停，走 AG-UI 的 interrupt 协议
ask_human_tool() ->
    #{name => <<"ask_human">>,
      description => <<"在执行有破坏性的操作之前，向用户确认一次"/utf8>>,
      parameters => #{<<"question">> => #{type => string, required => true,
                                          description => <<"要问用户的问题"/utf8>>}}}.

%% observer 关着时（缺省）`delegate_tool/2` 原样返回上游的工具，一行事件都不多
delegate_tool(RuntimeCfg) ->
    beamai_agui_subagent:delegate_tool(
      #{name => <<"research">>,
        description => <<"把一个独立的子任务交给子 agent 去做"/utf8>>,
        subagent => fun(_Args, _Ctx) ->
            K = beamai_chat_client:add_chat_model(beamai_chat_client:new(),
                                                  llm(current_provider())),
            #{chat_client => K, memory => false,
              system_prompt => <<"你是子 agent，专注把交给你的子任务做完，直接给结论。"/utf8>>}
        end},
      maps:get(subagent_observer, RuntimeCfg, undefined)).

%%====================================================================
%% LLM
%%====================================================================

%% max_tokens 给足：A2UI 的组件树 / genui 的 HTML 都不短，缺省 2048 装不下 ——
%% 截断的 JSON 解不出来，插件只会拿到空参数（症状是"调了但什么都没画"）
llm(minimax) ->
    put(provider, minimax),
    example_llm_config:minimax(#{api_key => env("MINIMAX_API_KEY"),
                                 model => <<"m2.7">>, max_tokens => 8192});
llm(zhipu) ->
    put(provider, zhipu),
    example_llm_config:anthropic(#{api_key => env("ZHIPU_API_KEY"),
                                   model => <<"glm-5.3-flash">>, max_tokens => 8192}).

current_provider() ->
    case get(provider) of
        undefined -> minimax;
        P -> P
    end.

env(Name) ->
    case os:getenv(Name) of
        false -> error({missing_env, Name});
        V -> list_to_binary(V)
    end.

%%====================================================================
%% 开机横幅
%%====================================================================

banner(Port, Provider, Opts, Origins) ->
    B = "http://127.0.0.1:" ++ integer_to_list(Port),
    io:format("~n~s~n", [lists:duplicate(66, $=)]),
    io:format("  BeamAI AG-UI 服务已启动~n", []),
    io:format("~s~n", [lists:duplicate(66, $=)]),
    io:format("  模型          ~ts~n", [Provider]),
    io:format("  agent         ~ts~n", [?AGENT]),
    io:format("  子 agent 事件  ~ts~n",
              [case maps:get(subagent_events, Opts, false) of
                   true -> "开（前端要 @ag-ui/client ≥ 0.0.58）";
                   false -> "关（缺省；老客户端见到 SUBAGENT_* 会掐断流）"
               end]),
    io:format("  CORS          ~ts~n",
              [case Origins of
                   any -> "* （不能带凭据）";
                   L -> io_lib:format("~ts", [lists:join(", ", [binary_to_list(O) || O <- L])])
               end]),
    io:format("~n  端点~n", []),
    io:format("    POST   ~s/agui/run~n", [B]),
    io:format("    GET    ~s/agui/info~n", [B]),
    io:format("    GET    ~s/agui/threads~n", [B]),
    io:format("    GET    ~s/agui/threads/:id/stream   (Last-Event-ID / ?since=)~n", [B]),
    io:format("    GET    ~s/agui/threads/:id/events~n", [B]),
    io:format("    GET    ~s/agui/threads/:id/state~n", [B]),
    io:format("    POST   ~s/agui/threads/:id/stop~n", [B]),
    io:format("    DELETE ~s/agui/threads/:id~n", [B]),
    io:format("~n  先打这一枪看能力位：~n", []),
    io:format("    curl -s ~s/agui/info | jq~n", [B]),
    io:format("~n  发一轮对话（SSE）：~n", []),
    io:format("    curl -N -X POST ~s/agui/run \\\\~n", [B]),
    io:format("      -H 'content-type: application/json' \\\\~n", []),
    io:format("      -d '{\"threadId\":\"t1\",\"state\":{\"todos\":[]},"
              "\"messages\":[{\"role\":\"user\",\"content\":\"北京天气怎么样\"}]}'~n", []),
    io:format("~n  试试这几句：~n", []),
    io:format("    · 北京天气怎么样                    → 工具调用 + 逐 token~n", []),
    io:format("    · 把买牛奶加进待办                  → STATE_DELTA~n", []),
    io:format("    · 用 render_a2ui 画一个销售看板     → ACTIVITY_SNAPSHOT (a2ui-surface)~n", []),
    io:format("    · 用 generateSandboxedUi 做个计数器 → ACTIVITY_SNAPSHOT + DELTA~n", []),
    io:format("    · 帮我删掉生产库那张表              → 停下来等你（interrupt 协议）~n", []),
    io:format("~s~n~n", [lists:duplicate(66, $=)]),
    ok.
