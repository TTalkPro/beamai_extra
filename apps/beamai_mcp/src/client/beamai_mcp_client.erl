%%%-------------------------------------------------------------------
%%% @doc MCP 客户端
%%%
%%% 使用 gen_statem 实现 MCP 客户端状态机。
%%% 支持连接 MCP 服务器、调用工具、访问资源等操作。
%%%
%%% == 状态转换 ==
%%%
%%% ```
%%% disconnected -> connecting -> connected
%%%                     |             |
%%%                     v             v
%%%                  [error]     disconnected
%%% ```
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 启动客户端
%%% {ok, Pid} = beamai_mcp_client:start_link(#{
%%%     transport => stdio,
%%%     command => "npx",
%%%     args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
%%%     client_info => #{name => <<"my-client">>, version => <<"1.0">>}
%%% }).
%%%
%%% %% 列出工具
%%% {ok, Tools} = beamai_mcp_client:list_tools(Pid).
%%%
%%% %% 调用工具
%%% {ok, Result} = beamai_mcp_client:call_tool(Pid, <<"read_file">>,
%%%                                           #{<<"path">> => <<"/tmp/test.txt">>}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_client).

-behaviour(gen_statem).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    start_link/1,
    stop/1
]).

%% 生命周期
-export([
    initialize/1,
    ping/1
]).

%% 工具
-export([
    list_tools/1,
    list_tools/2,
    call_tool/3,
    call_tool/4
]).

%% 资源
-export([
    list_resources/1,
    list_resources/2,
    read_resource/2,
    read_resource/3,
    subscribe_resource/2,
    unsubscribe_resource/2
]).

%% 提示
-export([
    list_prompts/1,
    list_prompts/2,
    get_prompt/3,
    get_prompt/4
]).

%% 状态查询
-export([
    get_state/1,
    get_capabilities/1
]).

%%====================================================================
%% gen_statem 回调
%%====================================================================

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4
]).

%% 状态处理
-export([
    disconnected/3,
    connecting/3,
    connected/3
]).

%%====================================================================
%% 类型定义
%%====================================================================

-record(data, {
    %% 配置
    config :: map(),
    client_info :: map(),
    %% 传输层
    transport_mod :: module(),
    transport_state :: term(),
    %% 会话信息
    session_id :: binary() | undefined,
    server_info :: map() | undefined,
    server_capabilities :: map() | undefined,
    protocol_version :: binary() | undefined,
    %% 请求追踪
    pending_requests = #{} :: #{term() => {pid(), reference()}},
    request_id = 1 :: pos_integer(),
    %% 超时配置
    init_timeout :: pos_integer(),
    default_timeout :: pos_integer(),
    tool_timeout :: pos_integer()
}).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动 MCP 客户端
%%
%% @param Config 客户端配置
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

%% @doc 停止客户端
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc 手动初始化连接
-spec initialize(pid()) -> ok | {error, term()}.
initialize(Pid) ->
    gen_statem:call(Pid, initialize).

%% @doc 发送 ping
-spec ping(pid()) -> ok | {error, term()}.
ping(Pid) ->
    gen_statem:call(Pid, ping).

%% @doc 列出工具
-spec list_tools(pid()) -> {ok, map()} | {error, term()}.
list_tools(Pid) ->
    list_tools(Pid, undefined).

-spec list_tools(pid(), binary() | undefined) -> {ok, map()} | {error, term()}.
list_tools(Pid, Cursor) ->
    gen_statem:call(Pid, {list_tools, Cursor}).

%% @doc 调用工具
-spec call_tool(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
call_tool(Pid, Name, Arguments) ->
    call_tool(Pid, Name, Arguments, ?MCP_TOOL_CALL_TIMEOUT).

-spec call_tool(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
call_tool(Pid, Name, Arguments, Timeout) ->
    gen_statem:call(Pid, {call_tool, Name, Arguments}, Timeout).

%% @doc 列出资源
-spec list_resources(pid()) -> {ok, [map()]} | {error, term()}.
list_resources(Pid) ->
    list_resources(Pid, undefined).

-spec list_resources(pid(), binary() | undefined) -> {ok, [map()]} | {error, term()}.
list_resources(Pid, Cursor) ->
    gen_statem:call(Pid, {list_resources, Cursor}).

%% @doc 读取资源
-spec read_resource(pid(), binary()) -> {ok, term()} | {error, term()}.
read_resource(Pid, Uri) ->
    read_resource(Pid, Uri, ?MCP_DEFAULT_TIMEOUT).

-spec read_resource(pid(), binary(), timeout()) -> {ok, term()} | {error, term()}.
read_resource(Pid, Uri, Timeout) ->
    gen_statem:call(Pid, {read_resource, Uri}, Timeout).

%% @doc 订阅资源
-spec subscribe_resource(pid(), binary()) -> ok | {error, term()}.
subscribe_resource(Pid, Uri) ->
    gen_statem:call(Pid, {subscribe_resource, Uri}).

%% @doc 取消订阅资源
-spec unsubscribe_resource(pid(), binary()) -> ok | {error, term()}.
unsubscribe_resource(Pid, Uri) ->
    gen_statem:call(Pid, {unsubscribe_resource, Uri}).

%% @doc 列出提示
-spec list_prompts(pid()) -> {ok, [map()]} | {error, term()}.
list_prompts(Pid) ->
    list_prompts(Pid, undefined).

-spec list_prompts(pid(), binary() | undefined) -> {ok, [map()]} | {error, term()}.
list_prompts(Pid, Cursor) ->
    gen_statem:call(Pid, {list_prompts, Cursor}).

%% @doc 获取提示
-spec get_prompt(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
get_prompt(Pid, Name, Arguments) ->
    get_prompt(Pid, Name, Arguments, ?MCP_DEFAULT_TIMEOUT).

-spec get_prompt(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
get_prompt(Pid, Name, Arguments, Timeout) ->
    gen_statem:call(Pid, {get_prompt, Name, Arguments}, Timeout).

%% @doc 获取客户端状态
-spec get_state(pid()) -> {ok, atom()}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc 获取服务器能力
-spec get_capabilities(pid()) -> {ok, map()} | {error, not_connected}.
get_capabilities(Pid) ->
    gen_statem:call(Pid, get_capabilities).

%%====================================================================
%% gen_statem 回调
%%====================================================================

callback_mode() ->
    [state_functions, state_enter].

init(Config) ->
    %% 提取配置
    ClientInfo = maps:get(client_info, Config, #{
        <<"name">> => <<"erlang-mcp-client">>,
        <<"version">> => <<"0.1.0">>
    }),
    InitTimeout = maps:get(init_timeout, Config, ?MCP_INIT_TIMEOUT),
    DefaultTimeout = maps:get(default_timeout, Config, ?MCP_DEFAULT_TIMEOUT),
    ToolTimeout = maps:get(tool_timeout, Config, ?MCP_TOOL_CALL_TIMEOUT),

    Data = #data{
        config = Config,
        client_info = ClientInfo,
        init_timeout = InitTimeout,
        default_timeout = DefaultTimeout,
        tool_timeout = ToolTimeout
    },

    %% 自动连接
    case maps:get(auto_connect, Config, true) of
        true ->
            {ok, connecting, Data};
        false ->
            {ok, disconnected, Data}
    end.

terminate(_Reason, _State, #data{transport_mod = undefined}) ->
    ok;
terminate(_Reason, _State, #data{transport_mod = Mod, transport_state = TState}) ->
    Mod:close(TState),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% 状态: disconnected
%%====================================================================

disconnected(enter, _OldState, Data) ->
    {keep_state, Data};

disconnected({call, From}, initialize, Data) ->
    {next_state, connecting, Data, [{reply, From, ok}]};

disconnected({call, From}, get_state, Data) ->
    {keep_state, Data, [{reply, From, {ok, disconnected}}]};

disconnected({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, not_connected}}]};

%% 静默吞掉迟到的 info：从 connecting/connected 掉下来时，在途的
%% send_after(100, recv_loop) 或传输层残留消息会追到这里——没有这条子句
%% 就是 function_clause 崩掉整个客户端。
disconnected(info, _Msg, Data) ->
    {keep_state, Data}.

%%====================================================================
%% 状态: connecting
%%====================================================================

%% enter 回调**不允许改状态**（callback_mode 含 state_enter，gen_statem 只接受
%% keep_state/repeat_state 系返回）。历史实现在这里做 transport 连接、失败时返回
%% {next_state, disconnected, ...}——一个干净的 {error, Reason}（stdio 命令不存在、
%% URL 不可达）会变成 bad_state_enter_return_from_state_function 崩溃，还顺着
%% start_link 带崩调用方。所以真正的连接动作挪到 state_timeout 0 事件里做。
connecting(enter, _OldState, Data) ->
    {keep_state, Data, [{state_timeout, 0, connect}]};

connecting(state_timeout, connect, #data{config = Config} = Data) ->
    case beamai_mcp_transport:create(Config) of
        {ok, {Mod, TState}} ->
            NewData = Data#data{
                transport_mod = Mod,
                transport_state = TState
            },
            %% 发送 initialize 请求
            {keep_state, NewData, [{state_timeout, 0, send_initialize}]};
        {error, Reason} ->
            %% 注意不能往 disconnected armed state_timeout：那个状态没有
            %% state_timeout 子句，事件到达即 function_clause
            logger:warning("MCP transport connect failed: ~p", [Reason]),
            {next_state, disconnected, Data}
    end;

connecting(state_timeout, send_initialize, Data) ->
    case send_initialize_request(Data) of
        {ok, NewData} ->
            %% 必须在 connecting 里就开始轮询传输层。
            %% 历史实现只在 connected(enter) 武装 recv_loop——而进入 connected
            %% 的唯一途径是收到 initialize 响应，响应又只能靠 recv_loop 从传输层
            %% 读上来：鸡生蛋死锁。实测任何传输、任何行为端正的 server，客户端
            %% 都在 connecting 干等到 init_timeout 然后掉 disconnected，
            %% list_tools/call_tool 永远 {error, not_connected}。
            self() ! recv_loop,
            {keep_state, NewData, [{state_timeout, Data#data.init_timeout, init_timeout}]};
        {error, Reason} ->
            logger:warning("MCP initialize send failed: ~p", [Reason]),
            {next_state, disconnected, Data}
    end;

connecting(state_timeout, init_timeout, Data) ->
    {next_state, disconnected, Data};

%% 轮询传输层读 initialize 响应（与 connected 的 recv_loop 同构；
%% handle_incoming_message 会把响应转成 {response, Id, Msg} 投回本进程，
%% 由下面既有的 {response, ...} 子句完成状态迁移）
connecting(info, recv_loop, #data{transport_mod = Mod, transport_state = TState} = Data) ->
    case Mod:recv(0, TState) of
        {ok, Message, NewTState} ->
            handle_incoming_message(Message, Data#data{transport_state = NewTState});
        {error, timeout} ->
            erlang:send_after(100, self(), recv_loop),
            {keep_state, Data};
        {error, Reason} ->
            logger:warning("MCP transport recv failed during init: ~p", [Reason]),
            {next_state, disconnected, Data}
    end;

connecting(info, {response, ReqId, Response}, #data{pending_requests = Pending} = Data) ->
    case maps:get(ReqId, Pending, undefined) of
        undefined ->
            {keep_state, Data};
        {_From, _Ref} ->
            %% 处理 initialize 响应
            case handle_initialize_response(Response, Data) of
                {ok, NewData} ->
                    %% 发送 initialized 通知
                    send_initialized_notification(NewData),
                    {next_state, connected, NewData#data{
                        pending_requests = maps:remove(ReqId, Pending)
                    }};
                {error, _Reason} ->
                    {next_state, disconnected, Data}
            end
    end;

connecting({call, From}, get_state, Data) ->
    {keep_state, Data, [{reply, From, {ok, connecting}}]};

connecting({call, From}, _, Data) ->
    {keep_state, Data, [{reply, From, {error, initializing}}]}.

%%====================================================================
%% 状态: connected
%%====================================================================

connected(enter, _OldState, Data) ->
    %% 启动消息接收循环
    self() ! recv_loop,
    {keep_state, Data};

connected(info, recv_loop, #data{transport_mod = Mod, transport_state = TState} = Data) ->
    %% 非阻塞接收
    case Mod:recv(0, TState) of
        {ok, Message, NewTState} ->
            NewData = Data#data{transport_state = NewTState},
            handle_incoming_message(Message, NewData);
        {error, timeout} ->
            %% 继续循环
            erlang:send_after(100, self(), recv_loop),
            {keep_state, Data};
        {error, _Reason} ->
            {next_state, disconnected, Data}
    end;

connected(info, {response, ReqId, Response}, #data{pending_requests = Pending} = Data) ->
    case maps:take(ReqId, Pending) of
        {{From, _Ref}, NewPending} ->
            Result = extract_result(Response),
            gen_statem:reply(From, Result),
            {keep_state, Data#data{pending_requests = NewPending}};
        error ->
            {keep_state, Data}
    end;

connected({call, From}, get_state, Data) ->
    {keep_state, Data, [{reply, From, {ok, connected}}]};

connected({call, From}, get_capabilities, #data{server_capabilities = Caps} = Data) ->
    Result = case Caps of
        undefined -> {error, not_available};
        _ -> {ok, Caps}
    end,
    {keep_state, Data, [{reply, From, Result}]};

connected({call, From}, ping, Data) ->
    handle_call_request(?MCP_METHOD_PING, #{}, From, Data);

connected({call, From}, {list_tools, Cursor}, Data) ->
    handle_call_request(?MCP_METHOD_TOOLS_LIST, build_cursor_params(Cursor), From, Data);

connected({call, From}, {call_tool, Name, Arguments}, Data) ->
    handle_call_request(?MCP_METHOD_TOOLS_CALL,
                        #{<<"name">> => Name, <<"arguments">> => Arguments},
                        From, Data);

connected({call, From}, {list_resources, Cursor}, Data) ->
    handle_call_request(?MCP_METHOD_RESOURCES_LIST, build_cursor_params(Cursor), From, Data);

connected({call, From}, {read_resource, Uri}, Data) ->
    handle_call_request(?MCP_METHOD_RESOURCES_READ, #{<<"uri">> => Uri}, From, Data);

connected({call, From}, {subscribe_resource, Uri}, Data) ->
    handle_call_request(?MCP_METHOD_RESOURCES_SUBSCRIBE, #{<<"uri">> => Uri}, From, Data);

connected({call, From}, {unsubscribe_resource, Uri}, Data) ->
    handle_call_request(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, #{<<"uri">> => Uri}, From, Data);

connected({call, From}, {list_prompts, Cursor}, Data) ->
    handle_call_request(?MCP_METHOD_PROMPTS_LIST, build_cursor_params(Cursor), From, Data);

connected({call, From}, {get_prompt, Name, Arguments}, Data) ->
    handle_call_request(?MCP_METHOD_PROMPTS_GET,
                        #{<<"name">> => Name, <<"arguments">> => Arguments},
                        From, Data);

%% 未识别的 call 要回复而不是崩：没有这条子句，`initialize/1' 打到已连接的
%% 客户端就是 function_clause——客户端进程崩掉，调用方还卡在 gen_statem:call 里。
connected({call, From}, Unsupported, Data) ->
    {keep_state, Data, [{reply, From, {error, {unsupported_in_state, connected, Unsupported}}}]};

%% 静默吞掉未识别的 info：stdio 传输的端口归本进程所有（connecting 阶段
%% open_port），子进程退出的 {Port, {exit_status,_}} / {'EXIT', Port, _} 会
%% 直接落进来——没有这条子句就是 function_clause，MCP server 子进程一退出
%% 就带崩客户端。
connected(info, Msg, Data) ->
    logger:debug("MCP client ignoring info in connected: ~p", [Msg]),
    {keep_state, Data}.

%%====================================================================
%% 内部函数 - 请求处理辅助
%%====================================================================

%% @private 处理标准请求调用
%%
%% 统一处理 connected 状态下的请求发送模式，减少重复代码。
%%
%% @param Method JSON-RPC 方法名
%% @param Params 请求参数
%% @param From 调用者
%% @param Data 状态数据
%% @returns gen_statem 响应
-spec handle_call_request(binary(), map(), gen_statem:from(), #data{}) ->
    gen_statem:event_handler_result(atom()).
handle_call_request(Method, Params, From, Data) ->
    case send_request(Method, Params, From, Data) of
        {ok, NewData} ->
            {keep_state, NewData};
        {error, Reason} ->
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end.

%% @private 构建带游标的参数
%%
%% 用于分页请求（list_tools, list_resources, list_prompts）。
-spec build_cursor_params(binary() | undefined) -> map().
build_cursor_params(undefined) -> #{};
build_cursor_params(Cursor) -> #{<<"cursor">> => Cursor}.

%%====================================================================
%% 内部函数 - 初始化
%%====================================================================

%% @private 发送 initialize 请求
send_initialize_request(#data{client_info = ClientInfo} = Data) ->
    Params = #{
        <<"protocolVersion">> => ?MCP_PROTOCOL_VERSION,
        <<"capabilities">> => #{
            <<"roots">> => #{<<"listChanged">> => true},
            <<"sampling">> => #{}
        },
        <<"clientInfo">> => ClientInfo
    },
    ReqId = make_request_id(Data),
    Message = beamai_mcp_jsonrpc:encode_request(ReqId, ?MCP_METHOD_INITIALIZE, Params),
    case send_message(Message, Data) of
        {ok, NewData} ->
            {ok, NewData#data{
                pending_requests = #{ReqId => {self(), make_ref()}},
                request_id = Data#data.request_id + 1
            }};
        Error ->
            Error
    end.

%% @private 处理 initialize 响应
handle_initialize_response(#{<<"result">> := Result}, Data) ->
    ServerInfo = maps:get(<<"serverInfo">>, Result, #{}),
    Capabilities = maps:get(<<"capabilities">>, Result, #{}),
    Version = maps:get(<<"protocolVersion">>, Result, ?MCP_PROTOCOL_VERSION),
    {ok, Data#data{
        server_info = ServerInfo,
        server_capabilities = Capabilities,
        protocol_version = Version
    }};
handle_initialize_response(#{<<"error">> := Error}, _Data) ->
    {error, Error};
handle_initialize_response(_, _Data) ->
    {error, invalid_response}.

%% @private 发送 initialized 通知
send_initialized_notification(Data) ->
    Message = beamai_mcp_jsonrpc:encode_notification(?MCP_NOTIFY_INITIALIZED, #{}),
    send_message(Message, Data).

%% @private 发送请求
send_request(Method, Params, From, Data) ->
    ReqId = make_request_id(Data),
    Message = beamai_mcp_jsonrpc:encode_request(ReqId, Method, Params),
    case send_message(Message, Data) of
        {ok, NewData} ->
            {ok, NewData#data{
                pending_requests = maps:put(ReqId, {From, make_ref()},
                                            Data#data.pending_requests),
                request_id = Data#data.request_id + 1
            }};
        Error ->
            Error
    end.

%% @private 发送消息
send_message(Message, #data{transport_mod = Mod, transport_state = TState} = Data) ->
    case Mod:send(Message, TState) of
        {ok, NewTState} ->
            {ok, Data#data{transport_state = NewTState}};
        Error ->
            Error
    end.

%% @private 生成请求 ID
make_request_id(#data{request_id = Id}) ->
    Id.

%% @private 处理收到的消息
handle_incoming_message(Message, Data) ->
    case beamai_mcp_jsonrpc:decode(Message) of
        {ok, Decoded} ->
            process_decoded_message(Decoded, Data);
        {error, _} ->
            %% 忽略无效消息
            erlang:send_after(100, self(), recv_loop),
            {keep_state, Data}
    end.

%% @private 处理解码后的消息
process_decoded_message(#{<<"id">> := Id} = Msg, Data)
  when is_map_key(<<"result">>, Msg); is_map_key(<<"error">>, Msg) ->
    %% 这是响应
    self() ! {response, Id, Msg},
    erlang:send_after(100, self(), recv_loop),
    {keep_state, Data};
process_decoded_message(#{<<"method">> := Method} = Msg, Data) ->
    %% 这是通知或请求
    handle_notification(Method, maps:get(<<"params">>, Msg, #{}), Data),
    erlang:send_after(100, self(), recv_loop),
    {keep_state, Data};
process_decoded_message(_, Data) ->
    erlang:send_after(100, self(), recv_loop),
    {keep_state, Data}.

%% @private 处理通知
handle_notification(?MCP_NOTIFY_TOOLS_CHANGED, _Params, _Data) ->
    %% TODO: 通知工具列表已变更
    ok;
handle_notification(?MCP_NOTIFY_RESOURCES_CHANGED, _Params, _Data) ->
    %% TODO: 通知资源列表已变更
    ok;
handle_notification(?MCP_NOTIFY_RESOURCES_UPDATED, _Params, _Data) ->
    %% TODO: 通知资源已更新
    ok;
handle_notification(?MCP_NOTIFY_PROMPTS_CHANGED, _Params, _Data) ->
    %% TODO: 通知提示列表已变更
    ok;
handle_notification(_Method, _Params, _Data) ->
    ok.

%% @private 提取结果
extract_result(#{<<"result">> := Result}) ->
    {ok, Result};
extract_result(#{<<"error">> := Error}) ->
    {error, Error};
extract_result(_) ->
    {error, invalid_response}.
