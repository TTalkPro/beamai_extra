%%%-------------------------------------------------------------------
%%% @doc MCP Streamable HTTP 会话注册表（session id → server 进程映射）
%%%
%%% MCP 是有状态的会话协议：initialize 建立会话，后续请求靠
%%% `mcp-session-id' 头定位**同一个** server 进程。历史实现每个 HTTP 请求
%%% 新建一个 beamai_mcp_server、用完即停——session id 读了也回显了，但从没
%%% 用它查过任何东西。于是 initialize 之后的所有方法都打到全新的、
%%% `initialized = false' 的 server 上，一律 -32600 "Server not initialized"：
%%% 服务端一辈子只能回答一个方法。
%%%
%%% 承载两种会话拓扑：
%%%
%%%   - **Streamable HTTP**（单端点，请求/响应同一条 POST）：
%%%     表项 `#{server => ServerPid}'。register/2 登记，lookup/1 找回 server。
%%%     流程见 beamai_mcp_cowboy_handler：initialize → start_server → 分发 →
%%%     从响应态取回 server 发的 id → register/2 → 响应头带 id；
%%%     后续请求 → lookup/1 → 分发到同一进程；DELETE → delete/1 终止。
%%%
%%%   - **SSE**（双端点，GET 建流 + POST 发消息，响应经流回推）：
%%%     表项 `#{server => ServerPid, sse_loop => LoopPid}'。GET 时
%%%     register_sse/3 登记；POST `/message?session_id=X' 经 lookup_sse/1
%%%     找到 {ServerPid, LoopPid}，分发到 server 后把响应 push 给 loop。
%%%     两个进程任一死亡即摘除整个会话。
%%%
%%% session id 的唯一权威是 server/handler（现已 crypto 强随机），本模块只做映射。
%%%
%%% 进程死亡经 monitor 自动摘除；闲置超过 TTL 的会话被定期清扫
%%% （HTTP 客户端可能不发 DELETE 就消失，不清扫就是无界泄漏）。
%%% TTL 经 app env 配置：`{beamai_mcp, session_ttl}'（毫秒，缺省 30 分钟）、
%%% `{beamai_mcp, session_sweep_interval}'（缺省 60 秒）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_session_registry).

-behaviour(gen_server).

-export([start_link/0]).
-export([register/2, register_sse/3, lookup/1, lookup_sse/1, delete/1, count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
%% 表项: {SessionId, #{server => pid(), sse_loop => pid() | undefined}, [MRef], LastMs}
-define(TABLE, beamai_mcp_sessions).
-define(TS_POS, 4).
-define(DEFAULT_TTL, 30 * 60 * 1000).
-define(DEFAULT_SWEEP, 60 * 1000).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 登记 Streamable HTTP 会话（单端点）：SessionId 由 server 发放
-spec register(binary(), pid()) -> ok.
register(SessionId, ServerPid) when is_binary(SessionId), is_pid(ServerPid) ->
    gen_server:call(?SERVER, {register, SessionId, #{server => ServerPid}}).

%% @doc 登记 SSE 会话（双端点）：ServerPid 处理请求，LoopPid 是回推流的 cowboy_loop 进程
-spec register_sse(binary(), pid(), pid()) -> ok.
register_sse(SessionId, ServerPid, LoopPid)
  when is_binary(SessionId), is_pid(ServerPid), is_pid(LoopPid) ->
    gen_server:call(?SERVER, {register, SessionId,
                              #{server => ServerPid, sse_loop => LoopPid}}).

%% @doc 找回 Streamable HTTP 会话的 server 进程（同时刷新活跃时间）
%%
%% 读走 ETS 不进 gen_server：lookup 是每个请求的热路径。
-spec lookup(binary()) -> {ok, pid()} | {error, not_found}.
lookup(SessionId) ->
    with_live_session(SessionId, fun(#{server := ServerPid}) -> {ok, ServerPid} end).

%% @doc 找回 SSE 会话的 {server, sse_loop}
-spec lookup_sse(binary()) -> {ok, pid(), pid()} | {error, not_found}.
lookup_sse(SessionId) ->
    with_live_session(SessionId,
        fun(#{server := ServerPid, sse_loop := LoopPid}) when is_pid(LoopPid) ->
                {ok, ServerPid, LoopPid};
           (_) ->
                {error, not_found}
        end).

%% @doc 显式终止会话（HTTP DELETE）
-spec delete(binary()) -> ok | {error, not_found}.
delete(SessionId) ->
    gen_server:call(?SERVER, {delete, SessionId}).

%% @doc 当前会话数（观测用）
-spec count() -> non_neg_integer().
count() ->
    ets:info(?TABLE, size).

%% @private 表项存活校验 + 活跃时间刷新，命中则套用 Fun 提取所需字段
with_live_session(SessionId, Fun) ->
    case ets:lookup(?TABLE, SessionId) of
        [{_, Pids, _MRefs, _}] ->
            case all_alive(Pids) of
                true ->
                    ets:update_element(?TABLE, SessionId, {?TS_POS, now_ms()}),
                    Fun(Pids);
                false ->
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

all_alive(Pids) ->
    lists:all(fun is_process_alive/1, pids_of(Pids)).

pids_of(Pids) ->
    [P || P <- [maps:get(server, Pids, undefined),
                maps:get(sse_loop, Pids, undefined)], is_pid(P)].

%%====================================================================
%% gen_server 回调
%%====================================================================

init([]) ->
    ?TABLE = ets:new(?TABLE, [named_table, set, public,
                              {read_concurrency, true}]),
    schedule_sweep(),
    {ok, #{}}.

handle_call({register, SessionId, Pids}, _From, State) ->
    MRefs = [erlang:monitor(process, P) || P <- pids_of(Pids)],
    true = ets:insert(?TABLE, {SessionId, Pids, MRefs, now_ms()}),
    {reply, ok, State};

handle_call({delete, SessionId}, _From, State) ->
    case ets:take(?TABLE, SessionId) of
        [{_, Pids, MRefs, _}] ->
            [erlang:demonitor(M, [flush]) || M <- MRefs],
            stop_session_pids(Pids),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 会话内任一进程死亡（server 崩 / SSE 流断）：摘除整个会话，停掉另一半。
%% 表项第 3 元是 MRef 列表；命中任一即销毁会话。
handle_info({'DOWN', MRef, process, _Pid, _Reason}, State) ->
    Matches = ets:select(?TABLE, [{{'$1', '$2', '$3', '_'},
                                   [], [{{'$1', '$2', '$3'}}]}]),
    lists:foreach(fun({SessionId, Pids, MRefs}) ->
        case lists:member(MRef, MRefs) of
            true ->
                ets:delete(?TABLE, SessionId),
                [erlang:demonitor(M, [flush]) || M <- MRefs, M =/= MRef],
                stop_session_pids(Pids);
            false ->
                ok
        end
    end, Matches),
    {noreply, State};

handle_info(sweep, State) ->
    Cutoff = now_ms() - ttl(),
    Expired = ets:select(?TABLE, [{{'$1', '$2', '$3', '$4'},
                                   [{'<', '$4', Cutoff}],
                                   [{{'$1', '$2', '$3'}}]}]),
    lists:foreach(fun({SessionId, Pids, MRefs}) ->
        logger:info("MCP session ~ts expired after idle timeout", [SessionId]),
        [erlang:demonitor(M, [flush]) || M <- MRefs],
        ets:delete(?TABLE, SessionId),
        stop_session_pids(Pids)
    end, Expired),
    schedule_sweep(),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 停掉会话名下的进程：server 经 sup 停，SSE loop 发 close 让它自行收尾
stop_session_pids(Pids) ->
    case maps:get(server, Pids, undefined) of
        SPid when is_pid(SPid) -> catch beamai_mcp_server_sup:stop_server(SPid);
        _ -> ok
    end,
    case maps:get(sse_loop, Pids, undefined) of
        LPid when is_pid(LPid) -> catch (LPid ! close);
        _ -> ok
    end.

now_ms() ->
    erlang:monotonic_time(millisecond).

ttl() ->
    application:get_env(beamai_mcp, session_ttl, ?DEFAULT_TTL).

schedule_sweep() ->
    Interval = application:get_env(beamai_mcp, session_sweep_interval, ?DEFAULT_SWEEP),
    erlang:send_after(Interval, self(), sweep).
