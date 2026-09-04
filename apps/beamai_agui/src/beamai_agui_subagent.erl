%%%-------------------------------------------------------------------
%%% @doc 子 agent lane：把委派出去那一段也变成可渲染的事件
%%%
%%% 没有这一层时，一次 `beamai_agent_delegate` 委派在前端看起来就是**一个跑了很久
%%% 的工具调用** —— 中间发生了什么（子 agent 自己调了哪些工具、说了什么）全在
%%% 服务端日志里。AG-UI 0.0.59 起有 `SUBAGENT_*` 事件族专门画这条 lane。
%%%
%%% == 怎么接上的：没有改上游 ==
%%%
%%% `beamai_agent_delegate` 的 `subagent` 是一个
%%% `fun((Args, Ctx) -> AgentConfig)` —— 它**返回一份 agent 配置**，而配置里可以有
%%% `callbacks`。所以这里的做法是包一层：
%%%
%%% ```
%%% delegate_tool(Config, Observer)
%%%   ├── 包住 Config 的 `subagent` fn：造一条 lane，把 lane 的 callbacks 塞进
%%%   │   子 agent 的配置，并发 SUBAGENT_STARTED
%%%   └── 包住整个工具的 handler：跑完按结果发 SUBAGENT_FINISHED / SUBAGENT_ERROR
%%% ```
%%%
%%% 上游一个字都不用改。`fanout_tool` 同样成立 —— 它对每个子任务调一次
%%% `subagent` fn，于是每个子任务自然得到一条自己的 lane。
%%%
%%% == 三个进程边界 ==
%%%
%%% 1. 工具 handler 跑在 `beamai_tool` spawn 的**子进程**里（超时要能杀）。lane
%%%    的记账用进程字典，因此天然按「一次工具调用」隔离；
%%% 2. **父 emitter 的状态表是 `private`，工具进程读不到它** —— 所以观察者交出去
%%%    的不是 emitter，而是它的三样东西：thread id、run id、sink。前两个是纯数据，
%%%    sink 是个闭包，跨进程调用没有问题（它要么 `gen_server:cast` 给会话，要么
%%%    `!` 给请求进程）。这也是为什么 `SUBAGENT_*` 直接走 sink，不走父 emitter；
%%% 3. 子 agent 本身跑在 `beamai_subagent_manager` 的 **worker 进程**里，而 lane
%%%    的发射器是在工具进程里建的 —— 所以那张状态表必须是 `public`
%%%    （`beamai_agui_emitter` 的 `access` 选项）。
%%%
%%% 一处随之而来的取舍：**lane 的事件不过父 run 的 `event_transform`**。插件的
%%% transform 是有状态的（状态在**父 run 进程**的进程字典里），换个进程跑它本来
%%% 就不成立。要让插件也认 lane 的事件，得给 lane 单独装一个 —— 那是宿主的决定。
%%%
%%% **`management_tools` 那条异步路不支持**：`spawn_subagent` 立刻返回、子 agent
%%% 在工具进程死后还在跑，那时 lane 的状态表已经随属主进程回收了。异步委派要画
%%% lane 得让 manager 透出观察钩子，那是上游的改动。
%%%
%%% == 缺省关 ==
%%%
%%% `{beamai_agui, subagent_events, true}` 才开。**不是保守**：`@ag-ui/client`
%%% ≤ 0.0.57 在 HTTP transport 里拿 discriminated union 校验每一条事件，一条
%%% `SUBAGENT_STARTED` 掐断整条流，客户端侧什么都救不回来。
%%%
%%% 关着时 `beamai_agui_runner` 不往工厂里放 `subagent_observer`，`delegate_tool/2`
%%% 收到 `undefined` 就原样返回上游的工具 —— 「不塞就什么都不会发生」。
%%%
%%% == 子 agent 是非流式的 ==
%%%
%%% `beamai_subagent_manager:do_run/1` 走 `beamai_agent:run/2`，没有 `on_token`。
%%% 所以 lane 里的文本是**整条**发的（`on_message_end` 一次性补
%%% START + CONTENT + END），不是逐字。工具调用与结果照常逐个出。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_subagent).

-export([enabled/0, observer/1]).
-export([delegate_tool/2, fanout_tool/2, with_observer/2]).

%% 一次工具调用期间开出来的 lane（进程字典，见模块头）
-define(LANES, {?MODULE, lanes}).

%% 观察者是**纯数据 + 一个闭包**，不是父 emitter 本身 —— 见模块头「三个进程边界」
-type observer() :: #{thread_id := binary(), run_id := binary(),
                      sink := fun((beamai_agui_event:event()) -> ok)} | undefined.
-export_type([observer/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 有没有开子 agent 事件（缺省关，见模块头）
-spec enabled() -> boolean().
enabled() ->
    application:get_env(beamai_agui, subagent_events, false) =:= true.

%% @doc 由父 run 的 emitter 造一个观察者
%%
%% `beamai_agui_runner` 在开关开着时把它放进交给工厂的 `RuntimeCfg`
%% （键 `subagent_observer`），工厂拿它去包委派工具。
-spec observer(beamai_agui_emitter:emitter()) -> observer().
observer(Emitter) ->
    %% **在 run 进程里取值**：那张表是 private 的，出了这个进程就读不到了
    #{thread_id => beamai_agui_emitter:thread_id(Emitter),
      run_id => beamai_agui_emitter:run_id(Emitter),
      sink => beamai_agui_emitter:sink(Emitter)}.

%% @doc 包一个单委派工具（`beamai_agent_delegate:tool/1`）
%%
%% `Observer` 是 `undefined` 就原样返回上游的工具 —— 一行事件都不会多。
-spec delegate_tool(beamai_agent_delegate:config(), observer()) -> map().
delegate_tool(Config, undefined) ->
    beamai_agent_delegate:tool(Config);
delegate_tool(Config, Observer) ->
    wrap(beamai_agent_delegate:tool(with_observer(Config, Observer)), Observer).

%% @doc 包一个 fan-out 委派工具（`beamai_agent_delegate:fanout_tool/1`）
%%
%% 每个子任务调一次 `subagent` fn，于是每个子任务一条 lane。
-spec fanout_tool(beamai_agent_delegate:config(), observer()) -> map().
fanout_tool(Config, undefined) ->
    beamai_agent_delegate:fanout_tool(Config);
fanout_tool(Config, Observer) ->
    wrap(beamai_agent_delegate:fanout_tool(with_observer(Config, Observer)), Observer).

%% @doc 只包 `subagent` fn（不包 handler）
%%
%% 给自己拼委派工具的宿主用：lane 开了、事件也发，只是没有 `SUBAGENT_FINISHED`
%% （那要知道工具什么时候跑完，得由包住 handler 的那一层给）。
-spec with_observer(beamai_agent_delegate:config(), observer()) ->
    beamai_agent_delegate:config().
with_observer(Config, undefined) ->
    Config;
with_observer(Config, Observer) ->
    Inner = maps:get(subagent, Config),
    Config#{subagent => fun(Args, Ctx) ->
        SubConfig = Inner(Args, Ctx),
        LaneId = beamai_id:gen_id(<<"subagent">>),
        Lane = lane_emitter(Observer, LaneId),
        ok = emit(Observer, beamai_agui_event:subagent_started(
                              #{subagent_run_id => LaneId,
                                name => maps:get(name, Config, <<"subagent">>),
                                task => task_of(Args)})),
        remember(LaneId, Lane),
        SubConfig#{callbacks => beamai_agui_emitter:callbacks(Lane)}
    end}.

%%====================================================================
%% 内部 - 包 handler
%%====================================================================

%% @private 在工具 handler 前后收口：跑完把这次调用开出来的 lane 一条条结掉
%%
%% **必须每条 lane 都收口**：`SUBAGENT_STARTED` 发了却没有对应的 FINISHED，
%% 前端那张卡片会永远转圈。异常路径也要收 —— 所以是 try/after 而不是顺序执行。
wrap(Tool, Observer) ->
    Inner = maps:get(handler, Tool),
    Tool#{handler => fun(Args, Ctx) ->
        put(?LANES, []),
        try
            Result = call(Inner, Args, Ctx),
            settle(Observer, Result),
            Result
        catch
            Class:Reason:Stack ->
                settle(Observer, {error, Reason}),
                erlang:raise(Class, Reason, Stack)
        after
            _ = erase(?LANES)
        end
    end}.

%% @private 上游的委派工具 handler 是 arity 2（`fun(Args, Ctx)`）
call(Fun, Args, Ctx) -> Fun(Args, Ctx).

%% @private 结掉这次调用开出来的每一条 lane
%%
%% 单条 lane 时把工具结果作为 `SUBAGENT_FINISHED.result` 带上（语义同
%% `RUN_FINISHED.result`）；fan-out 有多条，分不清哪条产出了哪一段，就不带 ——
%% **宁可少报一位，也不要报错的那一位**。
settle(Observer, Result) ->
    Lanes = lists:reverse(lanes()),
    put(?LANES, []),
    lists:foreach(fun({LaneId, Lane}) ->
        ok = emit(Observer, settle_event(LaneId, Result, length(Lanes))),
        %% lane 的状态表属主是本进程，显式收掉（进程退出也会回收，但工具进程
        %% 在并发批次里可能还要跑别的调用）
        catch beamai_agui_emitter:close(Lane)
    end, Lanes).

settle_event(LaneId, {error, Reason}, _N) ->
    beamai_agui_event:subagent_error(LaneId, format(Reason), <<"subagent_failed">>);
settle_event(LaneId, {ok, Content}, 1) when is_binary(Content) ->
    beamai_agui_event:subagent_finished(#{subagent_run_id => LaneId, result => Content});
settle_event(LaneId, _Result, _N) ->
    beamai_agui_event:subagent_finished(#{subagent_run_id => LaneId}).

%%====================================================================
%% 内部 - lane 发射器
%%====================================================================

%% @private 一条 lane 一个发射器
%%
%% **不能十路共用一个**：发射器记着「当前开着哪条文本消息」，共用会让并发子 agent
%% 的 token 交错进同一条消息里。
%%
%% `tag` 让这条 lane 的**每一条**事件都带 `subagentRunId` —— 给既有事件多一个字段
%% 是安全的（AG-UI 的事件 schema 是 passthrough，老客户端原样忽略），这也是为什么
%% 只有 `SUBAGENT_*` 三个新类型需要开关，字段不需要。
lane_emitter(#{thread_id := ThreadId, run_id := RunId, sink := Sink}, LaneId) ->
    beamai_agui_emitter:new(
      #{thread_id => ThreadId,
        run_id => RunId,
        sink => Sink,
        tag => #{<<"subagentRunId">> => LaneId},
        %% 回调在 manager 的 worker 进程里触发，表在这儿建（见模块头）
        access => public}).

%% @private lane 级的事件直接走 sink —— 父 emitter 的状态表在别的进程里
emit(#{sink := Sink}, Event) ->
    _ = Sink(Event),
    ok.

remember(LaneId, Lane) ->
    put(?LANES, [{LaneId, Lane} | lanes()]),
    ok.

lanes() ->
    case get(?LANES) of
        undefined -> [];
        L -> L
    end.

task_of(Args) when is_map(Args) ->
    case maps:get(<<"task">>, Args, maps:get(task, Args, undefined)) of
        T when is_binary(T), T =/= <<>> -> T;
        _ -> undefined
    end;
task_of(_) ->
    undefined.

format(Reason) when is_binary(Reason) -> Reason;
format(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).
