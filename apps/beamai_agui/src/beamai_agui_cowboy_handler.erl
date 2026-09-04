%%%-------------------------------------------------------------------
%%% @doc AG-UI Cowboy 适配器（可选模块）
%%%
%%% == 路由 ==
%%%
%%% ```erlang
%%% Dispatch = cowboy_router:compile([
%%%     {'_', [
%%%         {"/agui/run",              beamai_agui_cowboy_handler, #{agent => <<"my-agent">>}},
%%%         {"/agui/info",             beamai_agui_cowboy_handler, #{mode => info}},
%%%         {"/agui/threads",          beamai_agui_cowboy_handler, #{mode => threads}},
%%%         {"/agui/threads/:thread",  beamai_agui_cowboy_handler, #{mode => thread}},
%%%         {"/agui/threads/:thread/:sub",
%%%                                    beamai_agui_cowboy_handler, #{mode => thread}}
%%%     ]}
%%% ]),
%%% {ok, _} = cowboy:start_clear(agui_listener, [{port, 8080}],
%%%                              #{env => #{dispatch => Dispatch}}).
%%% ```
%%%
%%% `:sub` 认四个：`events` / `state` / `stop` / `stream`。
%%%
%%% == 两种 run 的活法 ==
%%%
%%% **会话模式（缺省，`beamai_agui` 应用起着时）**：run 挂在
%%% `beamai_agui_session` 下，**寿命与这次 HTTP 请求无关**。请求进程只是一个
%%% 订阅者：它写帧，断了就摘掉，run 照跑。于是
%%%
%%% - 断线重连接得回来 —— `GET /threads/:id/stream`，浏览器 `EventSource` 自带
%%%   `Last-Event-ID`，服务端凭它只补缺口（帧里那行 `id:` 就是给它的）；
%%% - 另一个请求停得掉它 —— `POST /threads/:id/stop`，手上只要有 threadId。
%%%
%%% **请求模式（会话层没起来时的回落）**：run 在请求进程 spawn 出来的进程里跑，
%%% 连接一断它跟着死。宿主没启 `beamai_agui` 应用（只引了几个模块）时就是这样 ——
%%% 少一层能力，不是坏掉。
%%%
%%% == 请求模式下为什么 run 要另起一个进程 ==
%%%
%%% cowboy 的 stream_body 只能在请求进程里调用，而 agent 跑起来是同步阻塞的。
%%% 同一个进程既跑 agent 又写帧，就只能等 agent 跑完再一次性写——那就不是流式了。
%%%
%%% == link 是双向的 ==
%%%
%%% run 进程 link 在请求进程上，为的是**客户端断开时 agent 跟着停**。但 link 是
%%% 双向的：run 进程要是异常退出，会**先把请求进程带走**，前端只剩一条被截断的
%%% 流，连 RUN_ERROR 都发不出去。请求进程又不能 trap_exit —— 它本身就是 cowboy
%%% spawn_link 出来的。解法是让 run 进程**永不异常退出**：自己把异常兜住，把结局
%%% 随 done 消息交回来。于是 link 只剩下我们想要的那个方向。
%%%
%%% （会话模式下这条不适用：会话自己 trap_exit，两个方向都要，见
%%% beamai_agui_session 模块头。）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_cowboy_handler).

-export([init/2]).

%% run 进程交回请求进程的消息（请求模式）
-define(EVENT, agui_event_local).
-define(DONE, agui_done).

%% 心跳间隔：SSE 注释帧（`: ping`）让中间的代理别把闲着的连接掐掉。
%% 客户端会忽略它，也不占 `id:` 的号。
-define(HEARTBEAT, 25000).

%%====================================================================
%% Cowboy 回调
%%====================================================================

init(Req, Config) ->
    Method = cowboy_req:method(Req),
    Mode = maps:get(mode, Config, run),
    dispatch(Method, Mode, Req, Config).

%%====================================================================
%% 分发
%%====================================================================

dispatch(<<"POST">>, run, Req, Config) ->
    handle_run(Req, Config);
dispatch(<<"GET">>, info, Req, Config) ->
    handle_info(Req, Config);
dispatch(<<"GET">>, threads, Req, Config) ->
    reply_result(beamai_agui_http_handler:handle_threads(
                   beamai_agui_http_handler:init(Config)), Req);
dispatch(Method, thread, Req, Config) ->
    handle_thread(Method, Req, Config);
dispatch(<<"OPTIONS">>, _Mode, Req, _Config) ->
    Req2 = cowboy_req:reply(204, headers_map(
                              beamai_agui_http_handler:cors_headers()), <<>>, Req),
    {ok, Req2, undefined};
dispatch(Method, Mode, Req, _Config) ->
    Body = beamai_utils:encode_json(#{
        <<"error">> => <<"method_not_allowed">>,
        <<"method">> => Method,
        <<"allowed">> => allowed(Mode)
    }),
    Headers = headers_map(beamai_agui_http_handler:json_headers()),
    Req2 = cowboy_req:reply(405, Headers#{<<"allow">> => allowed(Mode)}, Body, Req),
    {ok, Req2, undefined}.

allowed(info) -> <<"GET, OPTIONS">>;
allowed(threads) -> <<"GET, OPTIONS">>;
allowed(thread) -> <<"GET, POST, DELETE, OPTIONS">>;
allowed(_) -> <<"POST, OPTIONS">>.

%%====================================================================
%% run 端点
%%====================================================================

handle_run(Req0, Config0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Config = beamai_agui_http_handler:init(Config0),
    Headers = maps:to_list(cowboy_req:headers(Req1)),
    case beamai_agui_http_handler:handle_run(Body, Headers, Config) of
        {error, Status, Json, RespHeaders} ->
            Req2 = cowboy_req:reply(Status, headers_map(RespHeaders), Json, Req1),
            {ok, Req2, undefined};
        {stream, Input, Factory, RespHeaders} ->
            start_run(Input, Factory, RespHeaders,
                      beamai_agui_http_handler:run_opts(Config), Req1)
    end.

%% @private 会话模式优先；会话层没起来就回落到请求模式
start_run(Input, Factory, RespHeaders, RunOpts, Req) ->
    ThreadId = beamai_agui_types:thread_id(Input),
    case beamai_agui_sessions:ensure(ThreadId) of
        {ok, Session} ->
            session_run(Session, Input, Factory, RunOpts, RespHeaders, Req);
        {error, _} ->
            Req2 = cowboy_req:stream_reply(200, headers_map(RespHeaders), Req),
            Req3 = local_run(Input, Factory, RunOpts, Req2),
            {ok, Req3, undefined}
    end.

%% @private 会话模式：起 run（立刻返回）→ 拿起跑前的水位去订阅 → 写帧
%%
%% 水位不能传 undefined：run 一进来就发了 `RUN_STARTED`，而我们要等 start_run
%% 返回才轮到订阅 —— 中间那一段是真空。AG-UI 客户端第一件事就是校验
%% "第一条必须是 RUN_STARTED"，漏了它整条流当场作废。
session_run(Session, Input, Factory, RunOpts, RespHeaders, Req) ->
    case beamai_agui_session:start_run(Session, Input, Factory, #{run_opts => RunOpts}) of
        {started, RunId, Since} ->
            Headers = headers_map(RespHeaders ++ [{<<"x-agui-run-id">>, RunId}]),
            Req2 = cowboy_req:stream_reply(200, Headers, Req),
            {ok, stream_from(Session, Since, Req2), undefined};
        {busy, RunId} ->
            %% 一会话一 run：`on_concurrent` 是 reject 时，第二个请求如实碰壁。
            %% 409 而不是排队 —— 排队会让用户以为没发出去，而它其实在后面等着。
            Body = beamai_utils:encode_json(#{<<"error">> => <<"run_in_progress">>,
                                              <<"runId">> => RunId}),
            Req2 = cowboy_req:reply(409, headers_map(
                                      beamai_agui_http_handler:json_headers()), Body, Req),
            {ok, Req2, undefined};
        {error, Reason} ->
            Body = beamai_utils:encode_json(
                     #{<<"error">> => <<"session_error">>,
                       <<"detail">> => iolist_to_binary(io_lib:format("~p", [Reason]))}),
            Req2 = cowboy_req:reply(500, headers_map(
                                      beamai_agui_http_handler:json_headers()), Body, Req),
            {ok, Req2, undefined}
    end.

%%====================================================================
%% 会话订阅 → SSE
%%====================================================================

%% @private 订阅一个会话，把事件写成带 `id:` 的 SSE 帧
stream_from(Session, Since, Req) ->
    case beamai_agui_session:subscribe(Session, Since) of
        {ok, Ref, Replay, Resync} ->
            ok = maybe_resync(Resync, Req),
            case write_replay(Replay, Req) of
                terminal ->
                    %% 补发的历史里已经有终态了：这条 run 早跑完，没什么可等的
                    ok = beamai_agui_session:unsubscribe(Session, Ref),
                    close_stream(Req);
                ok ->
                    forward_session(Session, Ref, Req)
            end;
        {error, Reason} ->
            ok = write_event(Req, beamai_agui_event:run_error(
                                    format_reason(Reason), <<"subscribe_failed">>)),
            close_stream(Req)
    end.

%% @private 缺口：中间那段永远补不回来了 —— 给一份 `MESSAGES_SNAPSHOT` 让客户端
%% 照它对齐
%%
%% 不假装流是连续的：少几条事件的 UI 会静静地渲染错。也不是只说一声「你漏了」——
%% 客户端漏掉的是增量，增量对不上之后它手里那份就是错的，`MESSAGES_SNAPSHOT`
%% 正是 AG-UI 里用来说「照这份对齐」的那条事件。
maybe_resync(undefined, _Req) ->
    ok;
maybe_resync(Messages, Req) when is_list(Messages) ->
    write_event(Req, beamai_agui_event:messages_snapshot(Messages)).

write_replay([], _Req) ->
    ok;
write_replay([{Seq, Event} | Rest], Req) ->
    ok = cowboy_req:stream_body(beamai_agui_event:to_sse(Event, Seq), nofin, Req),
    case beamai_agui_event:is_terminal(Event) of
        true -> terminal;
        false -> write_replay(Rest, Req)
    end.

%% @private 转发循环：一条一帧，直到终态事件（或会话没了）
forward_session(Session, Ref, Req) ->
    receive
        {agui_event, Ref, Seq, Event} ->
            ok = cowboy_req:stream_body(beamai_agui_event:to_sse(Event, Seq), nofin, Req),
            case beamai_agui_event:is_terminal(Event) of
                true ->
                    ok = beamai_agui_session:unsubscribe(Session, Ref),
                    close_stream(Req);
                false ->
                    forward_session(Session, Ref, Req)
            end;
        {agui_closed, Ref, _Reason} ->
            %% 会话进程走了（空闲驱逐 / 关停）。流得收口，否则客户端一直挂着
            ok = write_event(Req, beamai_agui_event:run_error(
                                    <<"session closed">>, <<"session_closed">>)),
            close_stream(Req)
    after ?HEARTBEAT ->
        ok = cowboy_req:stream_body(<<": ping\n\n">>, nofin, Req),
        forward_session(Session, Ref, Req)
    end.

%%====================================================================
%% run 端点（请求模式回落）
%%====================================================================

%% @private 起 run 进程，请求进程转发事件为 SSE 帧
local_run(Input, Factory, RunOpts, Req) ->
    Parent = self(),
    Ref = make_ref(),
    Sink = fun(Event) -> Parent ! {?EVENT, Ref, Event}, ok end,
    {Pid, MRef} = spawn_monitor(
                    fun() -> run_process(Parent, Ref, Input, Factory, Sink, RunOpts) end),
    forward(Ref, Pid, MRef, Req).

%% @private run 进程主体：**永不异常退出**（见模块头「link 是双向的」）
run_process(Parent, Ref, Input, Factory, Sink, RunOpts) ->
    true = link(Parent),
    Outcome =
        try
            ok = beamai_agui_runner:run(Input, Factory, Sink, RunOpts),
            ok
        catch
            Class:Reason:Stack ->
                logger:error("beamai_agui run process crashed: ~p:~p", [Class, Reason],
                             #{stacktrace => Stack}),
                {error, Reason}
        end,
    Parent ! {?DONE, Ref, Outcome},
    ok.

%% @private 转发循环：收一条写一帧，直到 run 交待结局
forward(Ref, Pid, MRef, Req) ->
    receive
        {?EVENT, Ref, Event} ->
            ok = cowboy_req:stream_body(beamai_agui_event:to_sse(Event), nofin, Req),
            forward(Ref, Pid, MRef, Req);
        {?DONE, Ref, ok} ->
            erlang:demonitor(MRef, [flush]),
            close_stream(Req);
        {?DONE, Ref, {error, Reason}} ->
            erlang:demonitor(MRef, [flush]),
            %% runner 自己没能发出 RUN_ERROR（它整个崩了），这里补一条
            ok = write_event(Req, beamai_agui_event:run_error(
                                    format_reason(Reason), <<"runner_crashed">>)),
            close_stream(Req);
        {'DOWN', MRef, process, Pid, Reason} ->
            %% 兜底：正常到不了这里（run 进程自己兜异常，且 link 双向——它真要是被
            %% 强杀，本进程会先被信号带走）。留着是为了万一 done 消息丢了，
            %% receive 不至于永久挂住把连接吊死。
            ok = write_event(Req, beamai_agui_event:run_error(
                                    format_reason(Reason), <<"runner_died">>)),
            close_stream(Req)
    end.

write_event(Req, Event) ->
    cowboy_req:stream_body(beamai_agui_event:to_sse(Event), nofin, Req).

close_stream(Req) ->
    ok = cowboy_req:stream_body(<<>>, fin, Req),
    Req.

format_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% info 端点
%%====================================================================

handle_info(Req, Config0) ->
    Config = beamai_agui_http_handler:init(Config0),
    reply_result(beamai_agui_http_handler:handle_info(Config), Req).

%%====================================================================
%% threads 只读面 + 运维面
%%====================================================================

handle_thread(Method, Req, Config) ->
    ThreadId = cowboy_req:binding(thread, Req),
    Sub = cowboy_req:binding(sub, Req),
    Query = query_map(Req),
    case {Method, Sub} of
        {<<"GET">>, <<"stream">>} ->
            handle_thread_stream(ThreadId, Query, Req);
        {<<"GET">>, <<"events">>} ->
            reply_result(beamai_agui_http_handler:handle_thread_events(ThreadId, Query), Req);
        {<<"GET">>, <<"state">>} ->
            reply_result(beamai_agui_http_handler:handle_thread_state(ThreadId, Query), Req);
        {<<"POST">>, <<"stop">>} ->
            reply_result(beamai_agui_http_handler:handle_thread_stop(ThreadId, Query), Req);
        {<<"DELETE">>, undefined} ->
            reply_result(beamai_agui_http_handler:handle_thread_delete(ThreadId, Query), Req);
        {<<"GET">>, undefined} ->
            reply_result(beamai_agui_http_handler:handle_threads(Config), Req);
        _ ->
            reply_result({error, 404, beamai_utils:encode_json(
                                        #{<<"error">> => <<"not_found">>}),
                          beamai_agui_http_handler:json_headers()}, Req)
    end.

%% @private 断线重连：只订阅，不起新 run
%%
%% 水位三种写法都认：`?since=` / `?lastEventId=` / `Last-Event-ID` 头。头是浏览器
%% `EventSource` **自己**加的，前端一行代码都不用写；查询参数留给自己拼 SSE 的
%% 客户端（以及测试），显式给了就以它为准。
handle_thread_stream(ThreadId, Query, Req) ->
    case beamai_agui_sessions:whereis_session(ThreadId) of
        {ok, Session} ->
            Since = beamai_agui_http_handler:since_of(
                      Query#{<<"last-event-id">> =>
                                 cowboy_req:header(<<"last-event-id">>, Req)},
                      undefined),
            Headers = headers_map(beamai_agui_http_handler:sse_headers()),
            Req2 = cowboy_req:stream_reply(200, Headers, Req),
            {ok, stream_from(Session, Since, Req2), undefined};
        {error, not_found} ->
            reply_result({error, 404, beamai_utils:encode_json(
                                        #{<<"error">> => <<"thread_not_found">>,
                                          <<"threadId">> => ThreadId}),
                          beamai_agui_http_handler:json_headers()}, Req);
        {error, unavailable} ->
            reply_result({error, 501, beamai_utils:encode_json(
                                        #{<<"error">> => <<"sessions_unavailable">>}),
                          beamai_agui_http_handler:json_headers()}, Req)
    end.

%%====================================================================
%% 内部
%%====================================================================

reply_result({ok, Body, RespHeaders}, Req) ->
    Req2 = cowboy_req:reply(200, headers_map(RespHeaders), Body, Req),
    {ok, Req2, undefined};
reply_result({error, Status, Body, RespHeaders}, Req) ->
    Req2 = cowboy_req:reply(Status, headers_map(RespHeaders), Body, Req),
    {ok, Req2, undefined}.

query_map(Req) ->
    maps:from_list(cowboy_req:parse_qs(Req)).

headers_map(Headers) ->
    maps:from_list(Headers).
