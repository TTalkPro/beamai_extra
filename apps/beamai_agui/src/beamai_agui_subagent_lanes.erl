%%%-------------------------------------------------------------------
%%% @doc 异步委派的 lane 台账：让 lane 活得比「发起它的那次工具调用」久
%%%
%%% 同步委派（`delegate_tool/2`）的 lane 记账在**工具进程的进程字典**里，工具
%%% handler 返回就收口。异步委派（`management_tools/2` 的 `spawn_subagent`）
%%% 不行：`spawn` 立刻返回，子 agent 在 manager 的 worker 进程里接着跑，那
%%% 时工具进程已经退出、连同 lane 的 ETS 状态表一起回收了。
%%%
%%% 于是把三样记账搬到这里（一个常驻 gen_server）：
%%%
%%% 1. **ETS heir** —— lane 的状态表建出来时把本进程设为属主继任者。工具
%%%    进程死掉，表转到本进程，worker 里那些还在触发的回调照常读写；
%%% 2. **收口时机** —— 优先看 manager 的通知：`await(Id, infinity)` 在一个
%%%    小进程里阻塞，完成（含 failed / killed）时把 outcome 交回来。这条路
%%%    顺带把结果原文带回来了，`SUBAGENT_FINISHED.result` 与同步路径同义；
%%% 3. **兜底** —— 子 agent 配置里的 `on_turn_end` / `on_turn_error` 回调也来
%%%    收口（拿不到 id、或者根本没起成时靠它），再兜一层超时：
%%%    `{beamai_agui, subagent_lane_timeout, Ms}`（缺省 10 分钟，`infinity`
%%%    关掉）。**宁可报一条超时也不让前端那张卡片永远转圈**。
%%%
%%% 收口只做一次（先到先算），两条兜底路径与 manager 通知撞车时后者作废。
%%%
%%% == 有意留下的洞 ==
%%%
%%% `restart_subagent` 复用的是 manager 里存的 spec，那份 spec 里的回调指着
%%% **已经收口、已删掉**的表 —— 重跑出来的子 agent 事件静默丢弃（回调抛异常
%%% 由 `beamai_agent_callbacks` 兜住，不会带走 agent）。要让重跑也开新 lane，
%%% 得让 manager 的 spec 可更新，那是上游的改动。
%%%
%%% 本进程死掉（被 supervisor 重启）时，在飞的 lane 表跟着没 —— worker 的
%%% 回调同样只是静默丢弃。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_subagent_lanes).

-behaviour(gen_server).

-export([start_link/0, ensure_started/0, heir/0]).
-export([track/2, watch/2, settle/2, settle_soft/2, lanes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% 缺省 10 分钟：异步子 agent 跑得再久也该有个说法
-define(DEFAULT_TIMEOUT, 600000).

%% 回调兜底比 manager 的通知晚这么一拍：通知那条路带得上结果原文（走的是宿主
%% 自己的 `result` fn），回调只知道「跑完了」。谁先到谁算，后来者查不到台账就
%% 什么都不做 —— 1 秒足够 worker 把结果交回 manager。
-define(GRACE_MS, 1000).

%%====================================================================
% API
%%====================================================================

%% @doc 启动台账（由 `beamai_agui_sup` 拉起）
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 确保台账在线（幂等，返回 pid）
%%
%% 正常由 `beamai_agui_sup` 启动；裸 eunit / 库式调用（app 没起）时懒启动一个
%% unlink 的孤儿单例 —— 与 `beamai_subagent_manager:ensure_started/0` 同一套做法。
-spec ensure_started() -> pid().
ensure_started() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            Pid;
        undefined ->
            case start_link() of
                {ok, Pid} -> unlink(Pid), Pid;
                {error, {already_started, Pid}} -> Pid
            end
    end.

%% @doc lane 状态表的属主继任者（建表时当 ETS heir 用）
%%
%% **必须是活进程**：建表时给一个死掉的 heir，等于建完就准备着被回收。
-spec heir() -> pid().
heir() ->
    ?MODULE:ensure_started().

%% @doc 登记一条异步 lane（工具进程里，lane 建好之后立刻）
%%
%% `Lane` 是 `beamai_agui_emitter` 的发射器：台账从里面取状态表与 sink。
%% 用 call 而非 cast —— 工具进程随时可能退出，记账必须在返回前落地。
-spec track(binary(), beamai_agui_emitter:emitter()) -> ok.
track(LaneId, Lane) ->
    gen_server:call(ensure_started(), {track, LaneId, Lane}).

%% @doc 把 lane 接到某个子 agent id 上（从 spawn 工具的结果里认出来的）
%%
%% 于是一个小进程去 `beamai_subagent_manager:await(Id, infinity)`：正常跑完、
%% 失败、被 kill 都会回来一条 outcome，lane 就在那一刻收口。
-spec watch(binary(), binary() | undefined) -> ok | {error, unknown_lane}.
watch(_LaneId, undefined) ->
    ok;
watch(LaneId, SubagentId) ->
    gen_server:call(ensure_started(), {watch, LaneId, SubagentId}).

%% @doc 按 outcome 收口一条 lane（`on_turn_end` / `on_turn_error` 回调走这条）
%%
%% `Outcome` 与 `beamai_subagent_manager` 的 outcome 同形：
%% `{ok, Content}` 出 `SUBAGENT_FINISHED`，`{error, Reason}` 出 `SUBAGENT_ERROR`。
%% `{ok, undefined}`（回调只知道「跑完了」，拿不到结果原文）出的是不带
%% `result` 的 FINISHED。
-spec settle(binary(), {ok, binary() | undefined} | {error, term()}) -> ok.
settle(LaneId, Outcome) ->
    gen_server:cast(ensure_started(), {settle, LaneId, Outcome}).

%% @doc 兜底收口：晚 1 秒再落定，给 manager 的通知先到的机会
%%
%% 子 agent 的 `on_turn_end` 比 manager 收到结果**更早**触发，直接收口的话每条
%% lane 都会走「不带 `result`」那条。晚一拍，正常的子 agent 仍由通知收口（带得
%% 上宿主 `result` fn 加工过的结果原文），只有通知没来时才轮到回调。
-spec settle_soft(binary(), {ok, binary() | undefined} | {error, term()}) -> ok.
settle_soft(LaneId, Outcome) ->
    gen_server:cast(ensure_started(), {settle_soft, LaneId, Outcome}).

%% @doc 在飞的 lane（`#{LaneId => #{subagent_id => ..., ...}}`）
%%
%% 给宿主与测试看的：异步委派没有「本次调用开了哪些 lane」那本进程内的账。
-spec lanes() -> map().
lanes() ->
    gen_server:call(ensure_started(), lanes).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @doc Initialize the lane ledger (empty).
-spec init([]) -> {ok, #{lanes := map()}}.
init([]) ->
    {ok, #{lanes => #{}}}.

%% @doc Handle ledger bookkeeping calls.
-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({track, LaneId, Lane}, _From, #{lanes := Lanes} = St) ->
    Entry = #{lane => Lane,
              sink => beamai_agui_emitter:sink(Lane),
              subagent_id => undefined,
              timer => undefined},
    {reply, ok, St#{lanes => put_lane(LaneId, Entry, Lanes)}};
handle_call({watch, LaneId, SubagentId}, _From, St) ->
    case maps:find(LaneId, maps:get(lanes, St)) of
        error ->
            {reply, {error, unknown_lane}, St};
        {ok, Entry} ->
            Watcher = spawn(fun() -> await_settled(LaneId, SubagentId) end),
            Lanes = maps:put(LaneId, Entry#{subagent_id => SubagentId,
                                           watcher => Watcher}, maps:get(lanes, St)),
            {reply, ok, St#{lanes => Lanes}}
    end;
handle_call(lanes, _From, St) ->
    {reply, maps:get(lanes, St), St};
handle_call(_, _From, St) ->
    {reply, ok, St}.

%% @doc Handle settlement requests.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({settle, LaneId, Outcome}, St) ->
    {noreply, do_settle(LaneId, Outcome, St)};
handle_cast({settle_soft, LaneId, Outcome}, St) ->
    _ = erlang:send_after(?GRACE_MS, self(), {soft_settle, LaneId, Outcome}),
    {noreply, St};
handle_cast(_, St) ->
    {noreply, St}.

%% @doc Handle fallback timeouts and delayed settlements.
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info({timeout, LaneId}, St) ->
    {noreply, do_settle(LaneId, {error, lane_timeout}, St)};
handle_info({soft_settle, LaneId, Outcome}, St) ->
    {noreply, do_settle(LaneId, Outcome, St)};
handle_info(_, St) ->
    {noreply, St}.

%%====================================================================
%% 内部
%%====================================================================

%% @private 登记并起超时兜底
put_lane(LaneId, Entry, Lanes) ->
    Timer = start_timer(LaneId),
    Lanes#{LaneId => Entry#{timer => Timer}}.

%% @private 一条 lane 收口：发收尾事件、删状态表、销兜底、销台账
%%
%% **先到先算**：manager 通知与回调兜底撞车时，后来者查不到台账就什么都不做。
do_settle(LaneId, Outcome, #{lanes := Lanes} = St) ->
    case maps:take(LaneId, Lanes) of
        error ->
            St;
        {#{sink := Sink, timer := Timer, lane := #{table := Tab}}, Rest} ->
            cancel(Timer),
            _ = Sink(settle_event(LaneId, Outcome)),
            %% 表可能已经随（继承来的）属主一起没了，删不到不算事
            _ = catch ets:delete(Tab),
            St#{lanes => Rest}
    end.

settle_event(LaneId, {error, Reason}) ->
    beamai_agui_event:subagent_error(LaneId, format(Reason), code(Reason));
settle_event(LaneId, {ok, Content}) when is_binary(Content) ->
    beamai_agui_event:subagent_finished(#{subagent_run_id => LaneId, result => Content});
settle_event(LaneId, {ok, _NoResult}) ->
    beamai_agui_event:subagent_finished(#{subagent_run_id => LaneId}).

%% lane 自己的超时没有上游那份语义，单给一个码
code(lane_timeout) -> <<"subagent_timeout">>;
code(_) -> <<"subagent_failed">>.

%% @private 等 manager 的通知；manager 没起来时安静地走超时兜底
await_settled(LaneId, SubagentId) ->
    Outcome =
        try beamai_subagent_manager:await(SubagentId, infinity)
        catch _:_ -> {error, await_failed}
        end,
    case Outcome of
        %% manager 从没见过这个 id —— 多半是我们从工具结果里认错了。**别拿它收口**
        %% （会把还在跑的子 agent 的 lane 提前关掉），交回兜底路径。
        {error, not_found} ->
            ok;
        _ ->
            settle(LaneId, Outcome)
    end.

%% @private 兜底超时；`infinity` 关掉这一层
start_timer(LaneId) ->
    case application:get_env(beamai_agui, subagent_lane_timeout, ?DEFAULT_TIMEOUT) of
        infinity ->
            undefined;
        Ms when is_integer(Ms), Ms > 0 ->
            erlang:send_after(Ms, self(), {timeout, LaneId});
        _Other ->
            erlang:send_after(?DEFAULT_TIMEOUT, self(), {timeout, LaneId})
    end.

cancel(undefined) -> ok;
cancel(Timer) -> _ = erlang:cancel_timer(Timer), ok.

format(Reason) when is_binary(Reason) -> Reason;
format(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).

