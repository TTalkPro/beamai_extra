%%%-------------------------------------------------------------------
%%% @doc AG-UI HTTP 请求处理（框架无关）
%%%
%%% 只负责：解析/校验请求、定位 agent 工厂、组装只读面的响应体、给出响应头。
%%%
%%% **CORS 的最后一道由适配器盖**：允许哪个源取决于这次请求的 `Origin` 头，而本层
%%% 拿不到它（它是框架无关的）。这里给的是缺省那一份，适配器按 `cors_headers/1`
%%% 重算一遍覆盖上去 —— 见 beamai_agui_cowboy_handler 的 headers_map/2。
%%% 不负责：HTTP 服务器启动、路由、TLS、**SSE 帧怎么写出去**——最后这条是
%%% 刻意的：流式写回各框架姿势不同（cowboy 的 stream_body、其他框架的
%%% chunked writer），把它留给适配器，这一层才能真的框架无关。
%%%
%%% == 端点 ==
%%%
%%% | 方法与路径 | 干什么 |
%%% |---|---|
%%% | `POST /agui/run` | RunAgentInput → SSE 事件流 |
%%% | `GET  /agui/info` | 运行时能力声明（对标 getRuntimeInfo） |
%%% | `GET  /agui/threads` | 会话列表（只读） |
%%% | `GET  /agui/threads/:id` | 一条线程的元信息（不存在回 404） |
%%% | `GET  /agui/threads/:id/messages` | 该线程的消息（从事件流折出来的账本） |
%%% | `GET  /agui/threads/:id/events` | 环形缓冲里现存的事件（只读） |
%%% | `GET  /agui/threads/:id/state` | 该 thread 当前的共享状态（只读） |
%%% | `POST /agui/threads/:id/stop` | 停掉这个 thread 上在跑的 run |
%%% | `DELETE /agui/threads/:id` | 把会话从注册表里摘掉 |
%%%
%%% **只读面没有第二套存储**：线程列表来自会话注册表、事件来自环形缓冲、
%%% 状态由流上的 `STATE_SNAPSHOT` + `STATE_DELTA` 折出来。因此有两条边界要说
%%% 清楚：会话空闲 30 分钟被驱逐后就不在列表里（对话历史归 chat memory，不归
%%% 这里），而环形缓冲是**有界**的（缺省 512 条），久远的事件补不回来。
%%%
%%% == agent 怎么定位 ==
%%%
%%% 三条来路，按优先级：路径里的 `:agent` > 端点配置钉死的 `agent` >
%%% `forwardedProps.agent`。字面的 `default` 另有一条回落（单 agent 的部署认它），
%%% 见 resolve_agent/2；定位不到回 404，候选列表在响应体里给成数据。
%%%
%%% == 用法 ==
%%%
%%% ```erlang
%%% Config = beamai_agui_http_handler:init(#{agent => <<"my-agent">>}),
%%% case beamai_agui_http_handler:handle_run(Body, Headers, Config) of
%%%     {stream, Input, Factory, RespHeaders} ->
%%%         %% 200 + RespHeaders，然后逐帧写 beamai_agui_event:to_sse/2
%%%         ok;
%%%     {error, Status, Json, RespHeaders} ->
%%%         send_response(Status, RespHeaders, Json)
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_http_handler).

-export([init/1]).
-export([handle_run/3, handle_info/1]).
-export([handle_threads/1, handle_thread/2, handle_thread_messages/2,
         handle_thread_events/2, handle_thread_state/2,
         handle_thread_stop/2, handle_thread_delete/2]).
-export([sse_headers/0, json_headers/0]).
-export([cors_headers/0, cors_headers/1]).
-export([since_of/2, run_opts/1]).

-type config() :: #{
    %% 该端点固定服务的 agent 名；不给则从 forwardedProps.agent 取
    agent => binary(),
    %% 路径里的 `:agent`（`{base}/agent/:agent/run`），由适配器填 —— **优先于**
    %% 上面那个，于是一条路由就能服务 `/info` 里报的所有 agent
    path_agent => binary() | undefined,
    %% 事件流 / 入站插件（`beamai_agui_a2ui` / `beamai_agui_mcp_apps` 各产一个，
    %% examples 里的 `example_agui_genui` 也是；装两个以上用
    %% `beamai_agui_plugin:compose/1` 串起来）
    event_transform => beamai_agui_plugin:transform(),
    input_transform => fun((beamai_agui_types:run_input()) ->
                              beamai_agui_types:run_input()),
    %% 允许哪些源跨域访问。`any`（缺省）= `*`；给列表就只放行列表里的，
    %% 并回 `access-control-allow-credentials: true`（见 cors_headers/1）
    cors_origins => any | [binary()],
    %% 本次请求的 `Origin` 头，由适配器填（框架无关层自己拿不到）
    origin => binary() | undefined,
    %% `/info` 里如实声明的可选能力位（都是**装配方**才知道的事，本层不猜）
    suggestions => boolean(),
    open_generative_ui => boolean(),
    a2ui => boolean(),
    %% 收不收得了附件。**装配方才知道**（取决于 wire 认不认部件**且模型有视觉**），
    %% 本层不猜；不给就如实报一份全 false，见 agent_info/2
    multimodal => map(),
    descriptions => #{binary() => binary()}
}.

-export_type([config/0]).

-define(VERSION, <<"beamai_agui/1.1">>).

%%====================================================================
%% API
%%====================================================================

%% @doc 归一化配置
-spec init(map()) -> config().
init(Config) when is_map(Config) ->
    Config.

%% @doc 处理一次 run 请求
%%
%% @returns {stream, Input, Factory, RespHeaders} —— 校验通过，可以开始流式响应
%% @returns {error, Status, JsonBody, RespHeaders} —— 校验失败
-spec handle_run(binary() | map(), [{binary(), binary()}], config()) ->
    {stream, beamai_agui_types:run_input(), beamai_agui_registry:factory(),
     [{binary(), binary()}]} |
    {error, pos_integer(), binary(), [{binary(), binary()}]}.
handle_run(Body, _Headers, Config) ->
    case beamai_agui_types:parse_run_input(Body) of
        {error, Reason} ->
            error_response(400, <<"invalid_request">>, Reason, Config);
        {ok, Input} ->
            case resolve_agent(Input, Config) of
                {error, Reason} ->
                    agent_not_found(Reason, Config);
                {ok, Factory} ->
                    {stream, Input, Factory, sse_headers(Config)}
            end
    end.

%% @doc 运行时能力声明
%%
%% == `agents` 是**以 agent id 为键的字典，不是数组** ==
%%
%% 客户端拿它做 `Object.entries(runtimeInfo.agents)` 再按 id 建 proxy。给数组不会
%% 报错，只会把下标当成 agent id，于是前端去请求 `/agent/0/run` —— 一个只有联调
%% 才照得出来的错。
%%
%% == 只声明我们真支持的 ==
%%
%% 不谎报能力位：没有的键等于告诉客户端「没有」，它会走降级路径；报了却没有会
%% 让它去等一条永远不来的流。所以 `suggestions` / `openGenerativeUIEnabled` /
%% `a2uiEnabled` 缺省全是 false —— 那三样要装了对应插件才有，而插件是**装配方**
%% 的事，本层猜不了。
%%
%% `threadEndpoints` **无论开关开还是关这个键都发**：省略与「明确说没有」在客户端
%% 那儿是两码事 —— `undefined` 只能理解成「这台没说」，于是它盲发一枪 `/threads`
%% 拿 404 当答案。如实说没有不是谎报，谎报是报了没有的那种。
-spec handle_info(config()) -> {ok, binary(), [{binary(), binary()}]}.
handle_info(Config) ->
    AgentIds = beamai_agui_registry:list(),
    Threads = sessions_available(),
    Body = beamai_utils:encode_json(#{
        <<"version">> => ?VERSION,
        <<"runtime">> => <<"beamai_agui">>,
        <<"mode">> => <<"sse">>,
        <<"agents">> => maps:from_list([{Id, agent_info(Id, Config)} || Id <- AgentIds]),
        <<"audioFileTranscriptionEnabled">> => false,
        <<"suggestions">> => maps:get(suggestions, Config, false),
        <<"openGenerativeUIEnabled">> => maps:get(open_generative_ui, Config, false),
        <<"a2uiEnabled">> => maps:get(a2ui, Config, false),
        <<"telemetryDisabled">> => true,
        <<"threadEndpoints">> => #{
            <<"list">> => Threads,
            <<"inspect">> => Threads,
            <<"mutations">> => Threads,
            %% 恒 false：那是 `/threads/subscribe` 的实时推送（云产品的东西），
            %% 我们如实 404，客户端降级轮询。报 true 会让它去等一条不来的流。
            <<"realtimeMetadata">> => false
        }
    }),
    {ok, Body, json_headers(Config)}.

%%====================================================================
%% 只读面 + 运维面
%%====================================================================

%% @doc 会话列表
%%
%% 来自会话注册表，**没有第二套存储**。所以它是「此刻进程里还活着的会话」而不是
%% 「这台服务过的所有对话」—— 后者归 chat memory，那一层没有「列出所有会话」的
%% 协议。空闲 30 分钟被驱逐的会话就不在这儿了，但它的历史与未决暂停都还在。
-spec handle_threads(config()) -> {ok, binary(), [{binary(), binary()}]}.
handle_threads(_Config) ->
    Threads = [thread_summary(T) || T <- beamai_agui_sessions:list()],
    json_response(200, #{<<"threads">> => Threads}).

%% @doc 环形缓冲里现存的事件
%%
%% `since` 之后的那些（不给就是全部）。**有界且不落库** —— 它是为断线续传服务的，
%% 不是事件日志。
-spec handle_thread_events(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread_events(ThreadId, Query) ->
    with_session(ThreadId, fun(Session) ->
        Since = since_of(Query, undefined),
        %% `events` 里是**事件本身**，不是 `{event, seq}` 那样包一层 —— 包了之后
        %% 客户端拿到的每一条都没有 `type`，事件表与时间线全部落空。
        %% `seq` 挪进事件对象里当一个字段：AG-UI 的事件 schema 是 passthrough 的，
        %% 老客户端原样忽略，而要续传的客户端拿得到。
        Events = [Event#{<<"seq">> => Seq}
                  || {Seq, Event} <- beamai_agui_session:events(Session),
                     Since =:= undefined orelse Seq > Since],
        json_response(200, #{<<"id">> => ThreadId, <<"threadId">> => ThreadId,
                             <<"events">> => Events})
    end).

%% @doc 一条线程的元信息
%%
%% `GET /threads/:id` 曾经忽略路径参数、恒回整张表 —— 连不存在的 id 也回 200，
%% 客户端没法据此判断「这条主题没了」。
-spec handle_thread(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread(ThreadId, _Query) ->
    with_session(ThreadId, fun(_Session) ->
        json_response(200, thread_summary(ThreadId))
    end).

%% @doc 一条线程的消息列表
%%
%% 用的是会话从事件流折出来的**消息账本** —— 它是这条流自己的投影，不是第二个
%% 对话历史（那在 chat memory 里）。`/info` 报了 `inspect: true`，这条路由就必须
%% 在：报了能力却没有路由，客户端那一枪必然落空，而**能力位说谎比没有能力更贵**。
-spec handle_thread_messages(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread_messages(ThreadId, _Query) ->
    with_session(ThreadId, fun(Session) ->
        json_response(200, #{<<"id">> => ThreadId, <<"threadId">> => ThreadId,
                             <<"messages">> => beamai_agui_session:messages(Session)})
    end).

%% @doc 该 thread 当前的共享状态
-spec handle_thread_state(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread_state(ThreadId, _Query) ->
    with_session(ThreadId, fun(Session) ->
        json_response(200, #{<<"threadId">> => ThreadId,
                             <<"state">> => beamai_agui_session:state(Session)})
    end).

%% @doc 停掉这个 thread 上在跑的 run（**跨请求**：调用方只有 threadId）
%%
%% 返回 `stopped` 只表示那个 run 进程已经被杀。终态事件（`RUN_FINISHED` +
%% `result.status = "cancelled"`）由会话补在事件流上，订阅者从那里看到它停稳了。
-spec handle_thread_stop(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread_stop(ThreadId, _Query) ->
    with_session(ThreadId, fun(Session) ->
        json_response(200, #{<<"threadId">> => ThreadId,
                             <<"stopped">> => beamai_agui_session:stop_run(Session)})
    end).

%% @doc 把会话从注册表里摘掉（在跑的 run 一并停掉）
%%
%% **不动 chat memory，也不动 pause_store** —— 摘掉的只是进程里那份缓冲与订阅。
%% 要连未决的暂停一起丢，另调 `beamai_agui_thread:clear/1`。
-spec handle_thread_delete(binary(), map()) ->
    {ok, binary(), [{binary(), binary()}]} | {error, pos_integer(), binary(),
                                              [{binary(), binary()}]}.
handle_thread_delete(ThreadId, _Query) ->
    with_session(ThreadId, fun(_Session) ->
        ok = beamai_agui_sessions:forget(ThreadId),
        json_response(200, #{<<"threadId">> => ThreadId, <<"deleted">> => true})
    end).

%%====================================================================
%% 响应头
%%====================================================================

%% @doc SSE 响应头
%%
%% `x-accel-buffering: no` 是给 nginx 的：默认它会缓冲上游响应，
%% 一缓冲流式就变成了"最后一次性吐出来"，前端看不到任何增量。
-spec sse_headers() -> [{binary(), binary()}].
sse_headers() -> sse_headers(#{}).

%% @private 有配置的版本只给本模块装配流响应用。
-spec sse_headers(config()) -> [{binary(), binary()}].
sse_headers(Config) ->
    [{<<"content-type">>, <<"text/event-stream">>},
     {<<"cache-control">>, <<"no-cache">>},
     {<<"connection">>, <<"keep-alive">>},
     {<<"x-accel-buffering">>, <<"no">>}] ++ cors_headers(Config).

%% @doc JSON 响应头（缺省配置，供适配器复用）。
-spec json_headers() -> [{binary(), binary()}].
json_headers() -> json_headers(#{}).

%% @private 有配置的版本只给本模块装配 JSON 响应用。
-spec json_headers(config()) -> [{binary(), binary()}].
json_headers(Config) ->
    [{<<"content-type">>, <<"application/json">>}] ++ cors_headers(Config).

%% @doc CORS 头（AG-UI 前端通常与 runtime 不同源）
-spec cors_headers() -> [{binary(), binary()}].
cors_headers() -> cors_headers(#{}).

%% @doc 按配置算这次请求的 CORS 头
%%
%% == 两种模式，差别在能不能带凭据 ==
%%
%% - `any`（缺省）→ `access-control-allow-origin: *`。够开发用，但浏览器**不允许
%%   `*` 与凭据并存**：前端一旦 `credentials: 'include'`（或 `EventSource` 带
%%   `withCredentials`），请求当场被浏览器拦掉，而且错误只出现在浏览器控制台 ——
%%   服务端日志里干干净净，这类问题最难查。
%% - 给**允许列表** → 命中就把 `Origin` **原样回显**，并加
%%   `access-control-allow-credentials: true`。这是带 cookie / session 时唯一可行的
%%   写法。
%%
%% 没命中就**不发 `access-control-allow-origin`** —— 让浏览器如实拦掉，而不是回一个
%% `*` 假装放行（那会在带凭据时变成一个更难查的错）。
%%
%% `vary: origin` 在给列表时必发：响应随 `Origin` 而变，不告诉缓存这件事，
%% CDN / 浏览器缓存会把 A 源的响应喂给 B 源。
%%
%% `access-control-allow-headers` 里 **`last-event-id` 必须放行** —— 断线重连就靠
%% 这个头把水位带回来；`expose-headers` 里的 `x-agui-run-id` 是跨源前端读得到
%% run id 的唯一途径（它要拿这个 id 才停得掉这条 run）。
-spec cors_headers(config()) -> [{binary(), binary()}].
cors_headers(Config) ->
    Base = [{<<"access-control-allow-methods">>, <<"GET, POST, DELETE, OPTIONS">>},
            {<<"access-control-allow-headers">>,
             <<"content-type, authorization, last-event-id">>},
            {<<"access-control-expose-headers">>, <<"x-agui-run-id, x-agui-since">>},
            %% 预检结果缓存 10 分钟：不缓存的话每个 POST 前面都多一次 OPTIONS 往返
            {<<"access-control-max-age">>, <<"600">>}],
    case cors_origins(Config) of
        any ->
            [{<<"access-control-allow-origin">>, <<"*">>} | Base];
        Allowed ->
            [{<<"vary">>, <<"origin">>} | allow_origin(Config, Allowed) ++ Base]
    end.

%% @private 这个端点允许哪些源
%%
%% 端点配置 > 应用环境 > `any`。应用环境这一层是给「整台服务一个口径」用的：
%% ```erlang
%% {beamai_agui, [{cors_origins, [<<"http://localhost:3002">>]}]}
%% ```
-spec cors_origins(config()) -> any | [binary()].
cors_origins(Config) ->
    case maps:get(cors_origins, Config, undefined) of
        undefined -> application:get_env(beamai_agui, cors_origins, any);
        Value -> Value
    end.

%% @private 命中才回显；没命中一个字都不发（见 cors_headers/1）
allow_origin(Config, Allowed) ->
    case maps:get(origin, Config, undefined) of
        Origin when is_binary(Origin) ->
            case lists:member(Origin, Allowed) of
                true -> [{<<"access-control-allow-origin">>, Origin},
                         {<<"access-control-allow-credentials">>, <<"true">>}];
                false -> []
            end;
        _ ->
            %% 同源请求（浏览器不发 Origin）/ curl —— 本来就不需要 CORS 头
            []
    end.

%%====================================================================
%% 辅助（供适配器复用）
%%====================================================================

%% @private JSON 端点统一经 cowboy_handler 落地。
-spec json_response(pos_integer(), map()) -> {ok, binary(), [{binary(), binary()}]}.
json_response(_Status, Body) ->
    {ok, beamai_utils:encode_json(Body), json_headers()}.

%% @doc 端点配置里属于 `beamai_agui_runner:run/4` 的那几个键
%%
%% 只挑出来、不做别的 —— 适配器不该知道 runner 的选项长什么样。
-spec run_opts(config()) -> beamai_agui_runner:opts().
run_opts(Config) ->
    maps:with([event_transform, input_transform], Config).

%% @doc 从查询串 / 头里解出重连水位
%%
%% 认三种写法：`since` 查询参数、`lastEventId` 查询参数、`Last-Event-ID` 请求头。
%% 第三种是浏览器 `EventSource` **自己**加的 —— 前端一行代码都不用写。
%% 解不出数字就当没给（宁可从头补，也不要因为一个脏参数把整条流错位）。
-spec since_of(map(), integer() | undefined) -> integer() | undefined.
since_of(Query, Default) when is_map(Query) ->
    Raw = first_defined([maps:get(<<"since">>, Query, undefined),
                         maps:get(<<"lastEventId">>, Query, undefined),
                         maps:get(<<"last-event-id">>, Query, undefined)]),
    case Raw of
        undefined ->
            Default;
        Bin when is_binary(Bin) ->
            try binary_to_integer(string:trim(Bin))
            catch error:badarg -> Default
            end;
        Int when is_integer(Int) ->
            Int;
        _ ->
            Default
    end;
since_of(_, Default) ->
    Default.

%%====================================================================
%% 内部
%%====================================================================

%% @private 定位 agent 工厂
%%
%% 三条来路，按优先级：**路径里的 `:agent`** > 端点配置钉死的那个 >
%% `forwardedProps.agent`。
%%
%% 路径优先是要紧的：`/info` 报的是一张 **agents 字典**，标准 AG-UI 客户端据此
%% 拼 `{base}/agent/{id}/run` —— 报了字典却只有一条扁平的 `/run`，客户端没有别的
%% 读法，只能 404。（而 404 那条路上要是没有 CORS 头，浏览器还会把它显示成跨源
%% 被拒，把人指去查 CORS。见 beamai_agui_cowboy_handler 的 fallback 模式。）
%%
%% 查不到时还有**一条**回落，只给字面的 `default`：见 fallback_agent/2。
resolve_agent(Input, Config) ->
    case agent_name(Input, Config) of
        undefined ->
            {error, no_agent_specified};
        Name ->
            case beamai_agui_registry:lookup(Name) of
                {ok, Factory} -> {ok, Factory};
                {error, not_found} -> fallback_agent(Name, Config)
            end
    end.

%% @private `default` 这个 id 认，别的不认
%%
%% 客户端在用户还没从 `/info` 那张字典里挑之前（或者压根没读 `/info`），拼出来的
%% 就是 `{base}/agent/default/run` —— `default` 是「就这一台，你自己知道是谁」的
%% 惯用写法，不是一个真名字。单 agent 的部署认下来是对的：候选唯一，猜不出别的。
%%
%% **只认字面的 `default`**，且**只在注册表里没有这个名字时**（真注册了一个叫
%% `default` 的就走正常那条）。别的名字一律 404 ——
%% `/agent/beamai-dem0/run` 这种手抖要是也被喂给唯一那个 agent，就成了一个
%% 「能跑但跑的不是你以为那个」的错，比 404 难查得多。
%%
%% 两个以上 agent 时也 404：这时 `default` 指谁只能靠猜，而猜错是静悄悄的。
fallback_agent(<<"default">>, Config) ->
    case fallback_name(Config) of
        undefined ->
            unknown_agent(<<"default">>);
        Name ->
            case beamai_agui_registry:lookup(Name) of
                {ok, Factory} -> {ok, Factory};
                {error, not_found} -> unknown_agent(<<"default">>)
            end
    end;
fallback_agent(Name, _Config) ->
    unknown_agent(Name).

%% @private `default` 该落到谁头上：端点钉死的那个优先，否则注册表里唯一那个
fallback_name(Config) ->
    case maps:get(agent, Config, undefined) of
        Pinned when is_binary(Pinned), Pinned =/= <<>>, Pinned =/= <<"default">> ->
            Pinned;
        _ ->
            case beamai_agui_registry:list() of
                [Only] -> Only;
                _ -> undefined
            end
    end.

unknown_agent(Name) ->
    {error, {unknown_agent, Name, beamai_agui_registry:list()}}.

agent_name(Input, Config) ->
    case maps:get(path_agent, Config, undefined) of
        Name when is_binary(Name), Name =/= <<>> ->
            Name;
        _ ->
            case maps:get(agent, Config, undefined) of
                undefined -> beamai_agui_types:agent_name(Input);
                Name -> Name
            end
    end.

%% @private 一个 agent 的能力声明
%%
%% 报的每一位都是真的：
%%
%% - `humanInTheLoop` —— 会暂停等人（`interrupt_tools` / `on_tool_call` 返回
%%   `{interrupt, _}`），敏感工具要审批，且走 AG-UI 的 interrupt 协议
%%   （`RUN_FINISHED outcome=interrupt` + 收 `resume[]`）。
%%   **`approveWithEdits` 不报** —— 改参数再执行还没实现。
%% - `transport.streaming` —— 出口就是 SSE，token 逐个发。这是本层的事实，与模型无关。
%% - `tools.clientProvided` —— `RunAgentInput.tools` 我们认（挂成 interrupt_tools）。
%% - `tools.parallelCalls` —— 上游 `parallel_tools` 缺省开启。
%% - `reasoning` —— 我们发 `REASONING_*` 一族。读作「模型出思考我们就送到」，
%%   **不是**「模型一定有思考」—— 后者本层判不了。
%% - `state` —— `STATE_SNAPSHOT` 立基线 + `STATE_DELTA` 增量，两半都真有。
agent_info(Id, Config) ->
    Descriptions = maps:get(descriptions, Config, #{}),
    #{<<"name">> => Id,
      <<"className">> => <<"BeamAIAgent">>,
      <<"description">> => maps:get(Id, Descriptions, <<>>),
      <<"capabilities">> => #{
          <<"humanInTheLoop">> => #{<<"supported">> => true,
                                    <<"approvals">> => true,
                                    <<"interrupts">> => true},
          <<"transport">> => #{<<"streaming">> => true},
          <<"tools">> => #{<<"supported">> => true,
                           <<"clientProvided">> => true,
                           <<"parallelCalls">> => true},
          <<"reasoning">> => #{<<"supported">> => true, <<"streaming">> => true},
          <<"state">> => #{<<"snapshots">> => true, <<"deltas">> => true},
          <<"multimodal">> => maps:get(multimodal, Config, no_multimodal())
      }}.

%% @private 缺省：如实报「都收不了」
%%
%% **缺席与明确报 false 不是一回事**：缺席在客户端那儿只能读成「不知道」，而按
%% 「不知道就是 false」的惯例，附件入口同样不会出现 —— 但那是猜的。报一份全 false
%% 是**确定的信息**，下游据此把入口收起来。
%%
%% ⚠️ 反过来更贵：报了 `image: true` 而模型没有视觉，用户传了图、模型说看不见。
%% 能不能收附件取决于两件事 —— wire 认不认部件**且模型本身有视觉** —— 后一条本层
%% 判不了，所以要开这一格得由装配方传 `multimodal`。
no_multimodal() ->
    #{<<"input">> => #{<<"image">> => false, <<"pdf">> => false,
                       <<"audio">> => false, <<"video">> => false},
      <<"output">> => #{<<"image">> => false, <<"audio">> => false}}.

%% @private 只读面的一条
%%
%% **主键叫 `id`**：协议里是 `id`，客户端（以及上游 inspector）按它归一化 ——
%% 只给 `threadId` 的话每一条都被判成「没有 id 的坏记录」丢掉，于是列表 200 却
%% 一行都渲染不出来。`threadId` 一并留着，老代码不用改。
%%
%% `name` / `createdAt` / `updatedAt` / `agentId` 是列表页要的那几格；没有名字就
%% 给 `null`（客户端会显示成「未命名」，那是对的）。
%%
%% 运行时内部字段（`seq` / `subscribers` / `stopping`）留着 —— 排障有用，只是不该
%% 顶替协议字段的位置。
thread_summary(ThreadId) ->
    Base = #{<<"id">> => ThreadId, <<"threadId">> => ThreadId},
    case beamai_agui_session:status(ThreadId) of
        undefined ->
            Base#{<<"name">> => null, <<"agentId">> => null,
                  <<"createdAt">> => null, <<"updatedAt">> => null};
        Status ->
            Base#{<<"name">> => null_if_undefined(maps:get(name, Status, undefined)),
                  <<"agentId">> => null_if_undefined(maps:get(agent, Status, undefined)),
                  <<"createdAt">> => maps:get(created_at, Status, null),
                  <<"updatedAt">> => maps:get(last_active, Status),
                  <<"state">> => atom_to_binary(maps:get(state, Status), utf8),
                  <<"runId">> => null_if_undefined(maps:get(run_id, Status)),
                  <<"stopping">> => maps:get(stopping, Status),
                  <<"seq">> => maps:get(seq, Status),
                  <<"subscribers">> => maps:get(subscribers, Status),
                  %% 旧名字，留着不动
                  <<"lastActive">> => maps:get(last_active, Status)}
    end.

%% @private 会话层没起来 → 501；这个 thread 没有会话 → 404
%%
%% 两者不是一回事：前者是"这台没装这个能力"，后者是"这台有，但你要的不在"。
%% 合成一个码会让客户端分不清该不该重试。
with_session(ThreadId, Fun) ->
    case beamai_agui_sessions:whereis_session(ThreadId) of
        {ok, Session} ->
            Fun(Session);
        {error, not_found} ->
            error_response(404, <<"thread_not_found">>, ThreadId);
        {error, unavailable} ->
            error_response(501, <<"sessions_unavailable">>, no_session_registry)
    end.

sessions_available() ->
    erlang:whereis(beamai_agui_sessions) =/= undefined.

first_defined(Values) ->
    case [V || V <- Values, V =/= undefined] of
        [V | _] -> V;
        [] -> undefined
    end.

null_if_undefined(undefined) -> null;
null_if_undefined(V) -> V.

%% @private agent 定位失败 → 404，**把「要的是谁 / 有哪些」摊成字段**
%%
%% 这里曾经回的是 `io_lib:format("~p", [{unknown_agent, ...}])` —— 一个 Erlang
%% 项式塞进 JSON 字符串里：
%%
%% ```json
%% {"error":"agent_not_found","detail":"{unknown_agent,<<\"default\">>,[<<\"beamai-demo\">>]}"}
%% ```
%%
%% 客户端读不动它（要写个项式解析器），能做的只有把这行原样弹给用户；而「换哪个
%% id 再来一次」恰好是它自己能修的事 —— 只要候选列表给成**数据**而不是一行字。
%% `detail` 仍留一句人话，给的是趴在 curl 上的人看。
agent_not_found(Reason, Config) ->
    {Requested, Available} =
        case Reason of
            {unknown_agent, Name, Names} -> {Name, Names};
            %% 谁都没指定：路径里没有 `:agent`，端点没钉死，forwardedProps 也没给
            no_agent_specified -> {null, beamai_agui_registry:list()}
        end,
    Body = beamai_utils:encode_json(#{
        <<"error">> => <<"agent_not_found">>,
        <<"requested">> => Requested,
        <<"available">> => Available,
        <<"detail">> => agent_not_found_detail(Requested, Available)
    }),
    {error, 404, Body, json_headers(Config)}.

agent_not_found_detail(Requested, Available) ->
    Head = case Requested of
               null -> <<"no agent specified">>;
               Name -> <<"no agent registered as '", Name/binary, "'">>
           end,
    Tail = case Available of
               [] -> <<"; no agents registered">>;
               Names -> <<"; available: ",
                          (iolist_to_binary(lists:join(<<", ">>, Names)))/binary>>
           end,
    <<Head/binary, Tail/binary>>.

error_response(Status, Code, Reason) ->
    error_response(Status, Code, Reason, #{}).

error_response(Status, Code, Reason, Config) ->
    Body = beamai_utils:encode_json(#{
        <<"error">> => Code,
        <<"detail">> => iolist_to_binary(io_lib:format("~p", [Reason]))
    }),
    {error, Status, Body, json_headers(Config)}.
