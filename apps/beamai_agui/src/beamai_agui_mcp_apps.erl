%%%-------------------------------------------------------------------
%%% @doc MCP Apps —— 把「带 UI 资源的 MCP 工具」接进 AG-UI（可选插件）
%%%
%%% == 这里只剩前端约定，协议栈在 `beamai_mcp` ==
%%%
%%% 判据是「**协议认不认**」：
%%%
%%% | 归谁 | 什么 |
%%% |---|---|
%%% | `beamai_mcp`（本仓另一个 app） | JSON-RPC 信封、握手、传输、`tools/list`、`tools/call` |
%%% | 本模块（agui） | 带 UI 的工具出结果时补一条 **AG-UI activity 事件**；前端那块 iframe 发起调用时的**代理白名单** |
%%%
%%% activity 消息、`replace: true`、`forwardedProps.__proxiedMCPRequest` 都是
%%% **前端**的约定，不是 MCP 协议的东西 —— 所以它们在这儿，不在协议 app 里。
%%%
%%% == 装 ==
%%%
%%% ```erlang
%%% %% 装配期（不是每轮）问一次 server 有哪些工具，算出哪些带 UI
%%% {ok, Tools} = beamai_mcp_adapter:mcp_tools_to_beamai_tools(...),
%%% Apps = beamai_agui_mcp_apps:app_tools(Tools),
%%%
%%% #{agent => <<"my-agent">>,
%%%   event_transform => beamai_agui_mcp_apps:event_transform(#{apps => Apps,
%%%                                                             servers => Servers})}
%%% ```
%%%
%%% `Servers` 是 `[#{server_id := binary(), client := pid() | atom()}]` ——
%%% **连接由宿主管**（`beamai_mcp_client` 是个 gen_statem，起停归监督树），
%%% 本模块只做**路由 + 白名单**。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_mcp_apps).

-export([activity_type/0, ui_resource/1, server_id_of/1, app_tools/1]).
-export([event_transform/1]).
-export([proxy_allowed_methods/0, proxy_request/2]).

-define(ACTIVITY_TYPE, <<"mcp-apps">>).

%%====================================================================
%% 常量
%%====================================================================

%% @doc AG-UI activity 消息的类型标签。前端按它挑 renderer。
-spec activity_type() -> binary().
activity_type() -> ?ACTIVITY_TYPE.

%% @doc UI 代理**只允许这四个方法**
%%
%% 那块界面跑在浏览器里，不能让它随便调 server 的任何东西。同上游的白名单。
-spec proxy_allowed_methods() -> [binary()].
proxy_allowed_methods() ->
    [<<"tools/call">>, <<"resources/read">>, <<"notifications/message">>, <<"ping">>].

%%====================================================================
%% 哪些工具带 UI
%%====================================================================

%% @doc 一个工具的 UI 资源 URI（不带就是普通工具）
%%
%% MCP 把它放在工具的 `_meta` 里，键是 `ui/resourceUri`。atom 键与 binary 键都认 ——
%% 从 wire 上解出来的是 binary，宿主手写的可能是 atom。
-spec ui_resource(map()) -> binary() | undefined.
ui_resource(Tool) when is_map(Tool) ->
    case meta_of(Tool) of
        Meta when is_map(Meta) ->
            first_defined([maps:get(<<"ui/resourceUri">>, Meta, undefined),
                           maps:get('ui/resourceUri', Meta, undefined),
                           maps:get(<<"resourceUri">>, Meta, undefined)]);
        _ ->
            undefined
    end;
ui_resource(_) ->
    undefined.

%% @doc 工具属于哪个 server（`_meta` 里的 `server_id`，装配时由宿主打上）
-spec server_id_of(map()) -> binary() | undefined.
server_id_of(Tool) when is_map(Tool) ->
    case meta_of(Tool) of
        Meta when is_map(Meta) ->
            first_defined([maps:get(<<"server_id">>, Meta, undefined),
                           maps:get(server_id, Meta, undefined),
                           maps:get(<<"serverId">>, Meta, undefined)]);
        _ ->
            undefined
    end;
server_id_of(_) ->
    undefined.

%% @doc 工具表里哪些是**带 UI 的** → `#{工具名 => #{resource_uri, server_id}}`
%%
%% **装配期算一次**（不是每轮再问 server）：server 挂了宁可起服务时就报，也不要
%% 每轮对话多一次网络往返。
-spec app_tools([map()]) -> #{binary() => map()}.
app_tools(Tools) ->
    lists:foldl(fun(Tool, Acc) ->
        case {tool_name(Tool), ui_resource(Tool)} of
            {Name, Uri} when is_binary(Name), is_binary(Uri) ->
                Acc#{Name => #{resource_uri => Uri, server_id => server_id_of(Tool)}};
            _ ->
                Acc
        end
    end, #{}, Tools).

%%====================================================================
%% 事件流插件
%%====================================================================

%% @doc `event_transform`：带 UI 的工具出结果时，补一条 activity 快照
%%
%% 前端收到它就去把 `resourceUri` 指向的界面拉起来（`replace: true` = 每次结果
%% 整块换掉）。**只认带 UI 的工具** —— 普通 MCP 工具就是普通工具，不该多出一块界面。
%%
%% 快照里带上 `toolInput`：那块界面渲染时经常要知道「这次是用什么参数调的」。
%% 参数从 `TOOL_CALL_ARGS` 攒出来（见 `beamai_agui_plugin`），所以本 transform
%% 内部串了两段：一段攒参数，一段在 `TOOL_CALL_RESULT` 上补事件。
-spec event_transform(map()) -> beamai_agui_plugin:transform().
event_transform(Opts) ->
    Apps = maps:get(apps, Opts, #{}),
    Key = {?MODULE, make_ref()},
    Names = maps:keys(Apps),
    %% 第一段：攒参数（不产出事件，只记账）
    Collector = beamai_agui_plugin:tool_watcher(
                  #{tools => Names,
                    on_args => fun(ToolCallId, Name, Args) ->
                        put_call(Key, ToolCallId, Name, Args),
                        []
                    end}),
    %% 第二段：结果到了补快照
    fun(Event) ->
        lists:flatmap(fun(E) -> on_result(Key, Apps, E) end, Collector(Event))
    end.

on_result(Key, Apps, Event) ->
    case beamai_agui_event:type_of(Event) of
        <<"TOOL_CALL_RESULT">> ->
            ToolCallId = maps:get(<<"toolCallId">>, Event, undefined),
            case take_call(Key, Apps, ToolCallId) of
                undefined -> [Event];
                {App, Args} -> [Event, snapshot(ToolCallId, App, Args, Event)]
            end;
        _ ->
            [Event]
    end.

snapshot(ToolCallId, #{resource_uri := Uri, server_id := ServerId}, Args, Event) ->
    beamai_agui_event:activity_snapshot(
      <<?ACTIVITY_TYPE/binary, "-", ToolCallId/binary>>,
      ?ACTIVITY_TYPE,
      #{<<"resourceUri">> => Uri,
        <<"serverId">> => null_if_undefined(ServerId),
        <<"toolInput">> => Args,
        <<"result">> => maps:get(<<"content">>, Event, <<>>)},
      true).

%%====================================================================
%% 前端 iframe 的代理通道
%%====================================================================

%% @doc 前端那块 MCP UI 发起的调用（`forwardedProps.__proxiedMCPRequest`）
%%
%% **前端不知道 url**，它只有 `serverId`。按它找到是哪一个 server，过一遍方法
%% 白名单，再交给 `beamai_mcp_client`。本函数只做路由 + 白名单 —— 连接的生命周期
%% 归宿主（client 是个 gen_statem，起停在监督树里）。
-spec proxy_request([map()], map()) -> {ok, term()} | {error, term()}.
proxy_request(Servers, Request) when is_map(Request) ->
    ServerId = first_defined([maps:get(<<"serverId">>, Request, undefined),
                              maps:get(server_id, Request, undefined)]),
    Method = first_defined([maps:get(<<"method">>, Request, undefined),
                            maps:get(method, Request, undefined)]),
    Params = case first_defined([maps:get(<<"params">>, Request, undefined),
                                 maps:get(params, Request, undefined)]) of
                 M when is_map(M) -> M;
                 _ -> #{}
             end,
    case find_server(Servers, ServerId) of
        undefined ->
            {error, {unknown_server, ServerId}};
        Server ->
            case lists:member(Method, proxy_allowed_methods()) of
                false -> {error, {method_not_allowed, Method}};
                true -> dispatch(maps:get(client, Server), Method, Params)
            end
    end;
proxy_request(_Servers, _Request) ->
    {error, invalid_request}.

dispatch(Client, <<"tools/call">>, Params) ->
    Name = first_defined([maps:get(<<"name">>, Params, undefined),
                          maps:get(name, Params, undefined)]),
    Arguments = case first_defined([maps:get(<<"arguments">>, Params, undefined),
                                    maps:get(arguments, Params, undefined)]) of
                    A when is_map(A) -> A;
                    _ -> #{}
                end,
    safe(fun() -> beamai_mcp_client:call_tool(Client, Name, Arguments) end);
dispatch(Client, <<"resources/read">>, Params) ->
    Uri = first_defined([maps:get(<<"uri">>, Params, undefined),
                         maps:get(uri, Params, undefined)]),
    safe(fun() -> beamai_mcp_client:read_resource(Client, Uri) end);
dispatch(Client, <<"ping">>, _Params) ->
    safe(fun() -> beamai_mcp_client:ping(Client) end);
dispatch(Client, <<"notifications/message">>, _Params) ->
    %% 协议里这是个通知（没有响应）。代理通道要给前端一个回执，所以借 ping 确认
    %% 连接还活着 —— 上游同款做法。
    safe(fun() ->
        case beamai_mcp_client:ping(Client) of
            ok -> {ok, #{<<"success">> => true}};
            Other -> Other
        end
    end).

safe(Fun) ->
    try Fun() of
        {ok, _} = Ok -> Ok;
        {error, _} = Error -> Error;
        Other -> {ok, Other}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

find_server(Servers, ServerId) ->
    case [S || S <- Servers, is_map(S), maps:get(server_id, S, undefined) =:= ServerId] of
        [Server | _] -> Server;
        [] -> undefined
    end.

%%====================================================================
%% 内部
%%====================================================================

meta_of(Tool) ->
    first_defined([maps:get(<<"_meta">>, Tool, undefined),
                   maps:get('_meta', Tool, undefined),
                   maps:get(meta, Tool, undefined)]).

tool_name(Tool) when is_map(Tool) ->
    first_defined([maps:get(<<"name">>, Tool, undefined),
                   maps:get(name, Tool, undefined)]);
tool_name(_) ->
    undefined.

%% @private `TOOL_CALL_RESULT` 事件里**没有工具名**，只有 tool-call id ——
%% 所以记账时把名字一并留下，结果到了才认得出这次调用属于哪个 app
put_call(Key, ToolCallId, Name, Args) ->
    put(Key, maps:put(ToolCallId, {Name, Args}, calls(Key))),
    ok.

take_call(_Key, _Apps, undefined) ->
    undefined;
take_call(Key, Apps, ToolCallId) ->
    case maps:take(ToolCallId, calls(Key)) of
        {{Name, Args}, Rest} ->
            put(Key, Rest),
            case maps:get(Name, Apps, undefined) of
                undefined -> undefined;
                App -> {App, Args}
            end;
        error ->
            undefined
    end.

calls(Key) ->
    case get(Key) of
        undefined -> #{};
        Map -> Map
    end.

first_defined(Values) ->
    case [V || V <- Values, V =/= undefined] of
        [V | _] -> V;
        [] -> undefined
    end.

null_if_undefined(undefined) -> null;
null_if_undefined(V) -> V.
