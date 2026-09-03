%%%-------------------------------------------------------------------
%%% @doc AG-UI Cowboy 适配器（可选模块）
%%%
%%% == 路由 ==
%%%
%%% ```erlang
%%% Dispatch = cowboy_router:compile([
%%%     {'_', [
%%%         {"/agui/run",  beamai_agui_cowboy_handler, #{agent => <<"my-agent">>}},
%%%         {"/agui/info", beamai_agui_cowboy_handler, #{mode => info}}
%%%     ]}
%%% ]),
%%% {ok, _} = cowboy:start_clear(agui_listener, [{port, 8080}],
%%%                              #{env => #{dispatch => Dispatch}}).
%%% ```
%%%
%%% == 为什么 run 要另起一个进程 ==
%%%
%%% cowboy 的 stream_body 只能在请求进程里调用，而 agent 跑起来是同步阻塞的。
%%% 同一个进程既跑 agent 又写帧，就只能等 agent 跑完再一次性写——那就不是流式了。
%%% 所以：run 进程跑 agent、把事件发消息给请求进程，请求进程专心写帧。
%%%
%%% == link 是双向的 ==
%%%
%%% run 进程 link 在请求进程上，为的是**客户端断开时 agent 跟着停**——cowboy 杀掉
%%% 请求进程，退出信号顺着 link 传过去，不会留一个没人看的 LLM 调用在烧钱。
%%%
%%% 但 link 是双向的：run 进程要是异常退出，会**先把请求进程带走**，前端只剩一条
%%% 被截断的流，连 RUN_ERROR 都发不出去。请求进程又不能 trap_exit —— 它本身就是
%%% cowboy spawn_link 出来的，trap 了会破坏 cowboy「请求进程随连接消亡」的假设。
%%%
%%% 解法是让 run 进程**永不异常退出**：自己把异常兜住，把结局随 done 消息交回来，
%%% 由请求进程决定发什么。于是 link 只剩下我们想要的那个方向。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_cowboy_handler).

-export([init/2]).

%% run 进程交回请求进程的消息
-define(EVENT, agui_event).
-define(DONE, agui_done).

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
            Req2 = cowboy_req:stream_reply(200, headers_map(RespHeaders), Req1),
            Req3 = stream_run(Input, Factory, Req2),
            {ok, Req3, undefined}
    end.

%% @private 起 run 进程，请求进程转发事件为 SSE 帧
stream_run(Input, Factory, Req) ->
    Parent = self(),
    Ref = make_ref(),
    Sink = fun(Event) -> Parent ! {?EVENT, Ref, Event}, ok end,
    {Pid, MRef} = spawn_monitor(fun() -> run_process(Parent, Ref, Input, Factory, Sink) end),
    forward(Ref, Pid, MRef, Req).

%% @private run 进程主体：**永不异常退出**（见模块头「link 是双向的」）
run_process(Parent, Ref, Input, Factory, Sink) ->
    true = link(Parent),
    Outcome =
        try
            ok = beamai_agui_runner:run(Input, Factory, Sink),
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
    {ok, Body, RespHeaders} = beamai_agui_http_handler:handle_info(Config),
    Req2 = cowboy_req:reply(200, headers_map(RespHeaders), Body, Req),
    {ok, Req2, undefined}.

%%====================================================================
%% 内部
%%====================================================================

headers_map(Headers) ->
    maps:from_list(Headers).
