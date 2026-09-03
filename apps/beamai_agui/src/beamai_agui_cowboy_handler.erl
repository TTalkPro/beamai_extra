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
%%% run 进程是 spawn_link 的：客户端断开时 cowboy 杀掉请求进程，agent 跟着停，
%%% 不会留下一个没人看的 LLM 调用在烧钱。
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
    {Pid, MRef} = spawn_monitor(fun() ->
        %% link 让客户端断开（请求进程死）时这次 run 一起停
        true = link(Parent),
        ok = beamai_agui_runner:run(Input, Factory, Sink),
        Parent ! {?DONE, Ref}
    end),
    forward(Ref, Pid, MRef, Req).

%% @private 转发循环：收一条写一帧，直到 run 结束或进程死掉
forward(Ref, Pid, MRef, Req) ->
    receive
        {?EVENT, Ref, Event} ->
            ok = cowboy_req:stream_body(beamai_agui_event:to_sse(Event), nofin, Req),
            forward(Ref, Pid, MRef, Req);
        {?DONE, Ref} ->
            erlang:demonitor(MRef, [flush]),
            ok = cowboy_req:stream_body(<<>>, fin, Req),
            Req;
        {'DOWN', MRef, process, Pid, normal} ->
            ok = cowboy_req:stream_body(<<>>, fin, Req),
            Req;
        {'DOWN', MRef, process, Pid, Reason} ->
            %% run 进程异常死掉：补一个 RUN_ERROR，别让前端干等
            Event = beamai_agui_event:run_error(
                      iolist_to_binary(io_lib:format("~p", [Reason])),
                      <<"runner_crashed">>),
            ok = cowboy_req:stream_body(beamai_agui_event:to_sse(Event), nofin, Req),
            ok = cowboy_req:stream_body(<<>>, fin, Req),
            Req
    end.

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
