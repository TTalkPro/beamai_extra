%%%-------------------------------------------------------------------
%%% @doc 一个 thread 的会话：run 生命周期 + 事件环形缓冲 + 订阅扇出
%%%
%%% == 它补的是什么 ==
%%%
%%% 没有这一层时，一次 run 的寿命**由 HTTP 请求的寿命决定**：
%%% `beamai_agui_cowboy_handler` 在请求进程下 spawn 一个 run 进程，连接一断
%%% 它跟着死 —— 浏览器刷新、切网、锁屏，这一轮的输出就没了，只能取消重来。
%%% 另一个请求也**看不见**它：想停一个正在跑的 run，手上只有 threadId，没有那个
%%% 进程的 pid。
%%%
%%% 这个 gen_server 把三件事挪到请求之外：
%%%
%%% | 能力 | 怎么来的 |
%%% |---|---|
%%% | 断线重连不丢输出 | 事件进会话的**有界环形缓冲**，重连带 `since` 只补缺口 |
%%% | 跨请求 stop | run 进程挂在会话下，另一个请求凭 threadId 就能停 |
%%% | 一会话一 run | 起 run 走会话这一个入口，`reject`（缺省）或 `supersede` |
%%%
%%% == seq：会话级单调，跨 run 连续 ==
%%%
%%% 每条事件在会话里取一个号。号**不进事件 JSON**（AG-UI 的 BaseEvent 没这一位），
%%% 只写进 SSE 的 `id:` 行 —— 浏览器 `EventSource` 重连时把它放进 `Last-Event-ID`
%%% 请求头，服务端凭它只补缺口。跨 run 连续是刻意的：一个 thread 上第二轮对话的
%%% 事件不该把号重置回 0，否则重连时"补 3 号之后的"会把上一轮的也补一遍。
%%%
%%% == run 进程与会话的关系 ==
%%%
%%% 会话 `trap_exit` + `spawn_link` run 进程，于是两个方向都得到想要的语义：
%%%
%%% - run 崩了 → 会话收到 `{'EXIT', ...}`，**不跟着死**，替它补一条终态事件
%%%   （一条 run 恰好一个终态事件且它是最后一条，这条契约不能因为崩溃就破）；
%%% - 会话没了 → 退出信号传过去，不会留一个没人看的 LLM 调用在烧钱。
%%%
%%% **停一个 run 就是杀掉那个进程**：BEAM 有抢占，不需要 JVM 那套「协作式取消 +
%%% 正在跑的工具会跑完」的妥协。代价是被杀的工具停在半路 —— 它已经产生的副作用
%%% 仍然算数，这一点与任何一种取消都一样。
%%%
%%% == 与 pause_store 的分工 ==
%%%
%%% 会话是**进程内**的东西：节点一重启就没了。跨请求 HITL 的权威仍是
%%% `beamai_agui_thread` 那个 pause_store（可落 DETS），会话只在自己活着的时候
%%% 缓存一份「在等哪次调用的回话」，供 `/threads` 只读面用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_session).

-behaviour(gen_server).

-export([start_link/1]).
-export([start_run/3, start_run/4, subscribe/2, unsubscribe/2]).
-export([stop_run/1, stop_run/2, status/1, events/1, state/1, awaiting/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export_type([start_result/0, subscription/0]).

-define(DEFAULT_BUFFER_SIZE, 512).
-define(DEFAULT_IDLE_TTL, 1800000).       %% 30 分钟
-define(DEFAULT_SUPERSEDE_WAIT, 5000).
-define(IDLE_TICK, 60000).

-type start_result() :: {started, binary(), integer()}
                      | {busy, binary()}
                      | {error, term()}.

%% 订阅者收到的消息：`{agui_event, Ref, Seq, Event}`，会话结束时
%% `{agui_closed, Ref, Reason}`
-type subscription() :: reference().

%%====================================================================
%% API
%%====================================================================

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(ThreadId) ->
    gen_server:start_link(?MODULE, [ThreadId], []).

%% @doc 起一次 run。**立刻返回**，run 在会话下的独立进程里跑。
%%
%% 返回的 `Since` 是**起跑前的 seq 水位**：run 一进来就发 `RUN_STARTED`，而调用方
%% 要等本函数返回才轮到 `subscribe/2`，中间那一段是真空。拿这个水位去订阅，缺口
%% 就补齐了 —— 传 `undefined` 会漏掉 `RUN_STARTED`，AG-UI 客户端第一件事就是报
%% `First event must be 'RUN_STARTED'`。
-spec start_run(pid() | binary(), beamai_agui_types:run_input(),
                beamai_agui_registry:factory()) -> start_result().
start_run(Session, Input, Factory) ->
    start_run(Session, Input, Factory, #{}).

-spec start_run(pid() | binary(), beamai_agui_types:run_input(),
                beamai_agui_registry:factory(), map()) -> start_result().
start_run(Session, Input, Factory, Opts) ->
    call(Session, {start_run, Input, Factory, Opts}, infinity).

%% @doc 订阅这个会话的事件流。返回补发的历史（`Since` 之后的）与订阅句柄。
%%
%% `Gap` 为真表示环形缓冲的起点已经越过 `Since` —— 中间那段**永远补不回来了**，
%% 调用方该告诉客户端重新拉一次历史，而不是假装流是连续的。
-spec subscribe(pid() | binary(), integer() | undefined) ->
    {ok, subscription(), [{integer(), beamai_agui_event:event()}], boolean()}
    | {error, term()}.
subscribe(Session, Since) ->
    call(Session, {subscribe, self(), Since}).

-spec unsubscribe(pid() | binary(), subscription()) -> ok.
unsubscribe(Session, Ref) ->
    _ = call(Session, {unsubscribe, Ref}),
    ok.

%% @doc 停掉这个会话上在跑的 run（跨请求：调用方只有 threadId）
-spec stop_run(pid() | binary()) -> boolean().
stop_run(Session) ->
    stop_run(Session, undefined).

%% @doc 只停指定的那个 run —— 绝不误伤会话上更新的那个
-spec stop_run(pid() | binary(), binary() | undefined) -> boolean().
stop_run(Session, RunId) ->
    case call(Session, {stop_run, RunId}) of
        {error, _} -> false;
        Result -> Result
    end.

-spec status(pid() | binary()) -> map() | undefined.
status(Session) ->
    case call(Session, status) of
        {error, _} -> undefined;
        Status -> Status
    end.

%% @doc 环形缓冲里**现存**的事件（只读）
%%
%% 有界且不落库 —— 它是为断线续传服务的，不是事件日志。
-spec events(pid() | binary()) -> [{integer(), beamai_agui_event:event()}].
events(Session) ->
    case call(Session, events) of
        {error, _} -> [];
        Events -> Events
    end.

%% @doc 这个 thread 当前的共享状态（由流上的 SNAPSHOT + DELTA 折出来）
-spec state(pid() | binary()) -> map().
state(Session) ->
    case call(Session, state) of
        {error, _} -> #{};
        State -> State
    end.

%% @doc 在等哪次调用的回话（会话进程内的缓存，权威仍是 pause_store）
-spec awaiting(pid() | binary()) -> map() | undefined.
awaiting(Session) ->
    case call(Session, awaiting) of
        {error, _} -> undefined;
        Awaiting -> Awaiting
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

init([ThreadId]) ->
    process_flag(trap_exit, true),
    _ = erlang:send_after(?IDLE_TICK, self(), idle_check),
    {ok, #{thread_id => ThreadId,
           seq => -1,
           buffer => queue:new(),
           buffered => 0,
           buffer_size => env(buffer_size, ?DEFAULT_BUFFER_SIZE),
           subs => #{},
           run => undefined,
           pending_start => undefined,
           awaiting => undefined,
           state => #{},
           last_active => now_ms()}}.

handle_call({start_run, Input, Factory, Opts}, From, State) ->
    do_start_run(Input, Factory, Opts, From, touch(State));

handle_call({subscribe, Pid, Since}, _From, State) ->
    #{buffer := Buffer} = State,
    Ref = erlang:monitor(process, Pid),
    Replay = [E || {Seq, _} = E <- queue:to_list(Buffer), after_since(Seq, Since)],
    Gap = has_gap(Buffer, Since),
    Subs = maps:put(Ref, Pid, maps:get(subs, State)),
    {reply, {ok, Ref, Replay, Gap}, touch(State#{subs => Subs})};

handle_call({unsubscribe, Ref}, _From, State) ->
    {reply, ok, drop_sub(Ref, State)};

handle_call({stop_run, RunId}, _From, State) ->
    case maps:get(run, State) of
        #{run_id := Current, pid := Pid} = Run
          when RunId =:= undefined; RunId =:= Current ->
            %% 已经登记过取消就不重复杀（幂等），但也不撒谎说"是我停的"
            case maps:get(stopping, Run, false) of
                true ->
                    {reply, false, State};
                false ->
                    true = exit(Pid, kill),
                    {reply, true,
                     touch(State#{run => Run#{stopping => true}})}
            end;
        _ ->
            {reply, false, State}
    end;

handle_call(status, _From, State) ->
    {reply, do_status(State), State};

handle_call(events, _From, State) ->
    {reply, queue:to_list(maps:get(buffer, State)), State};

handle_call(state, _From, State) ->
    {reply, maps:get(state, State), State};

handle_call(awaiting, _From, State) ->
    {reply, maps:get(awaiting, State), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({event, RunId, Event}, State) ->
    {noreply, publish(Event, RunId, touch(State))};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% run 进程收场（正常退出 / 崩溃 / 被 stop_run 杀掉）
handle_info({'EXIT', Pid, Reason}, #{run := #{pid := Pid}} = State) ->
    {noreply, finish_run(Reason, State)};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

%% 订阅者进程没了：摘掉它，run 不受影响
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    {noreply, drop_sub(Ref, State)};

%% supersede 等旧 run 收尾等超时了：不能无限期挂着调用方
handle_info({supersede_timeout, RunId},
            #{pending_start := {From, _, _, _}, run := #{run_id := RunId}} = State) ->
    gen_server:reply(From, {busy, RunId}),
    {noreply, State#{pending_start => undefined}};
handle_info({supersede_timeout, _RunId}, State) ->
    {noreply, State};

handle_info(idle_check, State) ->
    case idle_expired(State) of
        true ->
            {stop, normal, State};
        false ->
            _ = erlang:send_after(?IDLE_TICK, self(), idle_check),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #{subs := Subs}) ->
    maps:foreach(fun(Ref, Pid) -> Pid ! {agui_closed, Ref, Reason} end, Subs),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部 - 起 run
%%====================================================================

do_start_run(Input, Factory, Opts, From, State) ->
    case maps:get(run, State) of
        undefined ->
            launch_reply(Input, Factory, State);
        #{run_id := RunId} = Run ->
            case on_concurrent(Opts) of
                supersede ->
                    %% 先请求停掉旧的，等它的 EXIT 到了再起新的 —— 收尾要经过
                    %% 本进程（finish_run/2 补终态事件），在这里同步等必死锁
                    ok = request_stop(Run),
                    _ = erlang:send_after(supersede_wait(), self(),
                                          {supersede_timeout, RunId}),
                    {noreply, State#{pending_start => {From, Input, Factory, Opts},
                                     run => Run#{stopping => true}}};
                _ ->
                    {reply, {busy, RunId}, State}
            end
    end.

launch_reply(Input, Factory, State) ->
    {Result, State1} = launch(Input, Factory, State),
    {reply, Result, State1}.

%% @private 起 run 进程（link + trap_exit，见模块头）
launch(Input, Factory, State) ->
    Since = maps:get(seq, State),
    RunId = beamai_agui_types:run_id(Input),
    Session = self(),
    Pid = spawn_link(fun() -> run_process(Session, RunId, Input, Factory) end),
    {{started, RunId, Since},
     State#{run => #{run_id => RunId, pid => Pid,
                     stopping => false, terminal => false}}}.

%% @private run 进程主体：事件经 cast 回会话（同一个发送者到同一个进程，保序）
run_process(Session, RunId, Input, Factory) ->
    Sink = fun(Event) -> gen_server:cast(Session, {event, RunId, Event}), ok end,
    try
        ok = beamai_agui_runner:run(Input, Factory, Sink)
    catch
        Class:Reason:Stack ->
            %% runner 自己兜了绝大部分异常；能到这儿说明连 emitter 都出事了，
            %% 让退出理由带上它，由会话替这条 run 补终态事件
            logger:error("beamai_agui run process crashed: ~p:~p", [Class, Reason],
                         #{stacktrace => Stack}),
            exit({agui_run_crashed, Reason})
    end.

request_stop(#{pid := Pid}) ->
    true = exit(Pid, kill),
    ok.

%% @private run 收场：没说完话的替它补一条终态事件，再看有没有 pending 的新 run
finish_run(Reason, #{run := Run} = State) ->
    State1 = ensure_terminal(Reason, Run, State),
    State2 = State1#{run => undefined, awaiting => reload_awaiting(State)},
    case maps:get(pending_start, State2) of
        undefined ->
            State2;
        {From, Input, Factory, _Opts} ->
            {Result, State3} = launch(Input, Factory, State2#{pending_start => undefined}),
            gen_server:reply(From, Result),
            State3
    end.

%% @private 一条 run 恰好一个终态事件，且它是最后一条 —— 崩溃/被杀也不能破这条
ensure_terminal(_Reason, #{terminal := true}, State) ->
    State;
ensure_terminal(Reason, #{run_id := RunId, stopping := Stopping}, State) ->
    ThreadId = maps:get(thread_id, State),
    Event = case {Stopping, Reason} of
        {true, _} -> beamai_agui_event:run_cancelled(ThreadId, RunId);
        {false, normal} ->
            %% 进程正常退出却没发终态：runner 的契约破了，如实报错而不是假装跑完了
            beamai_agui_event:run_error(<<"run ended without a terminal event">>,
                                        <<"runner_silent">>);
        {false, Other} ->
            beamai_agui_event:run_error(format_reason(Other), <<"runner_died">>)
    end,
    publish(Event, RunId, State).

%% @private run 收场后重新问一次 pause_store：这个 thread 是不是停在等人上
%%
%% 不从事件流里猜 —— 权威在 store（跨请求 HITL 本来就靠它），事件只是它的投影。
reload_awaiting(#{thread_id := ThreadId}) ->
    case beamai_agui_thread:pending(ThreadId) of
        none ->
            undefined;
        {ok, Snapshot} ->
            case beamai_agui_thread:pending_tool_call(Snapshot) of
                TC when is_map(TC) ->
                    {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
                    #{tool_call_id => Id, tool_name => Name, args => Args};
                _ ->
                    #{}
            end
    end.

%%====================================================================
%% 内部 - 事件出口
%%====================================================================

%% @private 取号 → 进缓冲 → 扇出。三件事在会话进程里一次做完，故订阅者不会
%% 收到乱序的号，新订阅者也不会漏掉"重放完到注册完"之间的那几条。
publish(Event, RunId, State) ->
    Seq = maps:get(seq, State) + 1,
    State1 = buffer_push(Seq, Event, State#{seq => Seq}),
    maps:foreach(fun(Ref, Pid) -> Pid ! {agui_event, Ref, Seq, Event} end,
                 maps:get(subs, State1)),
    track(Event, RunId, State1).

buffer_push(Seq, Event, #{buffer := Buffer, buffered := N, buffer_size := Cap} = State) ->
    Buffer1 = queue:in({Seq, Event}, Buffer),
    case N + 1 > Cap of
        true ->
            {{value, _}, Buffer2} = queue:out(Buffer1),
            State#{buffer => Buffer2, buffered => Cap};
        false ->
            State#{buffer => Buffer1, buffered => N + 1}
    end.

%% @private 顺手记账：终态标记与共享状态
track(Event, RunId, State) ->
    State1 = track_state(Event, State),
    case beamai_agui_event:is_terminal(Event) of
        true ->
            case maps:get(run, State1) of
                #{run_id := RunId} = Run -> State1#{run => Run#{terminal => true}};
                _ -> State1
            end;
        false ->
            State1
    end.

%% @private `/threads/:id/state` 要的是"现在是什么"，而流上跑的是基线 + 一串补丁
track_state(Event, State) ->
    case beamai_agui_event:type_of(Event) of
        <<"STATE_SNAPSHOT">> ->
            State#{state => maps:get(<<"snapshot">>, Event, #{})};
        <<"STATE_DELTA">> ->
            Patched = beamai_agui_state:apply_patch(maps:get(state, State),
                                                    maps:get(<<"delta">>, Event, [])),
            State#{state => Patched};
        _ ->
            State
    end.

%%====================================================================
%% 内部 - 辅助
%%====================================================================

%% @private threadId 也能当句柄用：调用方多半只有它
call(Session, Request) ->
    call(Session, Request, 5000).

call(Session, Request, Timeout) when is_pid(Session) ->
    try
        gen_server:call(Session, Request, Timeout)
    catch
        exit:{noproc, _} -> {error, no_session};
        exit:{normal, _} -> {error, no_session};
        exit:{timeout, _} -> {error, timeout}
    end;
call(ThreadId, Request, Timeout) when is_binary(ThreadId) ->
    case beamai_agui_sessions:ensure(ThreadId) of
        {ok, Pid} -> call(Pid, Request, Timeout);
        {error, _} = Error -> Error
    end.

after_since(_Seq, undefined) -> false;
after_since(Seq, Since) -> Seq > Since.

%% @private 缺口：缓冲最老的那条已经在 `Since + 1` 之后了
has_gap(_Buffer, undefined) ->
    false;
has_gap(Buffer, Since) ->
    case queue:peek(Buffer) of
        {value, {Oldest, _}} -> Oldest > Since + 1;
        empty -> false
    end.

drop_sub(Ref, #{subs := Subs} = State) ->
    _ = erlang:demonitor(Ref, [flush]),
    State#{subs => maps:remove(Ref, Subs)}.

do_status(State) ->
    Run = maps:get(run, State),
    Awaiting = maps:get(awaiting, State),
    #{thread_id => maps:get(thread_id, State),
      %% run 在跑 > 停在等人 > 闲着
      state => if Run =/= undefined -> running;
                  Awaiting =/= undefined -> awaiting_resume;
                  true -> idle
               end,
      run_id => case Run of #{run_id := R} -> R; _ -> undefined end,
      %% 「取消已登记、但还没停稳」——UI 该显示"正在停止…"而不是"已停止"
      stopping => case Run of #{stopping := S} -> S; _ -> false end,
      awaiting => Awaiting,
      seq => maps:get(seq, State),
      subscribers => map_size(maps:get(subs, State)),
      last_active => maps:get(last_active, State)}.

%% @private 空闲驱逐：在跑的、有订阅者的、停在等人的都不驱逐
%%
%% 「停在等人的不驱逐」是为了 `/threads` 只读面 —— 权威快照在 pause_store 里，
%% 驱逐了续跑照样成立，只是列表上看不见它了。
idle_expired(#{run := undefined, awaiting := undefined, subs := Subs,
               last_active := Last}) when map_size(Subs) =:= 0 ->
    now_ms() - Last > env(idle_ttl, ?DEFAULT_IDLE_TTL);
idle_expired(_) ->
    false.

on_concurrent(Opts) ->
    maps:get(on_concurrent, Opts, env(on_concurrent, reject)).

supersede_wait() ->
    env(supersede_wait, ?DEFAULT_SUPERSEDE_WAIT).

env(Key, Default) ->
    application:get_env(beamai_agui, Key, Default).

touch(State) ->
    State#{last_active => now_ms()}.

now_ms() ->
    erlang:system_time(millisecond).

format_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
