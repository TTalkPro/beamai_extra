%%%-------------------------------------------------------------------
%%% @doc thread 的未决暂停：查、清、以及暂停存储句柄
%%%
%%% == 为什么没有单独的 run 存储 ==
%%%
%%% 上游 beamai_agent 已经有一整套跨进程 HITL 机制：agent 配了 `pause_store`
%%% 就会在中断时把**中断态快照**按 `conversation_id` 落库（beamai_agent_pause），
%%% 而 `beamai_agent:resume/3` 在本进程没有中断态时会**透明回落**到这个 store
%%% 把它读回来。所以这里只要把三个 id 对齐：
%%%
%%% ```
%%% AG-UI threadId  ==  beamai conversation_id  ==  pause_store 的键
%%% ```
%%%
%%% 于是「这个请求是不是一次 resume」就等价于「这个 thread 在 store 里有没有
%%% 未决快照」——不需要再造一层 run 表。
%%%
%%% == 存的是什么、不存什么 ==
%%%
%%% 快照是**纯数据**：中断态、被中断的那个 tool_call、已完成的结果、迭代计数。
%%% **不存** ChatClient / tools / callbacks（含 fun，本来也序列化不了）——那些由
%%% agent 工厂在下一个请求里重建。对话历史也不在里面，归 chat memory 管。
%%%
%%% 这决定了一件要紧的事：**工厂必须是可重放的**。同一个 thread 的两次请求要能
%%% 造出配置一致的 agent（同样的工具集、同样的 system prompt），否则 resume
%%% 回来的中断态会对不上新 agent 的工具表。
%%%
%%% == 配置 ==
%%%
%%% - `{beamai_agui, pause_store, {Module, Ref}}` —— 直接给一个自定义 store 句柄
%%%   （持久化到磁盘/数据库时用），给了就不再起内置的 ETS store；
%%% - `{beamai_agui, pause_store_name, Atom}` —— 内置 ETS store 的注册名。
%%%
%%% 内置的是 **ETS**：进程活着就在，节点重启即丢。要跨重启续跑得换成持久实现
%%% （实现 beamai_pause_store behaviour 的三个回调即可），并把 chat memory 也
%%% 换成持久 store——两者缺一，恢复出来的会话都是残的。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_thread).

-export([store/0, available_store/0, store_name/0, own_store/0]).
-export([pending/1, pending_tool_call/1, pending_tool_call_id/1, clear/1]).

-define(DEFAULT_STORE_NAME, beamai_agui_pause_store).

%%====================================================================
%% 存储句柄
%%====================================================================

%% @doc 本运行时使用的 pause_store 句柄
-spec store() -> beamai_pause_store:handle().
store() ->
    case application:get_env(beamai_agui, pause_store) of
        {ok, {Module, _Ref} = Handle} when is_atom(Module) -> Handle;
        _ -> beamai_pause_store_ets:handle(store_name())
    end.

%% @doc 可用的 store 句柄；内置 ETS store 没起来时返回 none
%%
%% 不把死掉的 store 交给 agent：上游在**每个终态**都会 pause_clear，句柄指向
%% 一个不存在的进程就等于让每次正常的 run 都炸在收尾上。宁可退化成「这个节点
%% 没有 HITL 持久化」——其余功能照常。
-spec available_store() -> {ok, beamai_pause_store:handle()} | none.
available_store() ->
    case application:get_env(beamai_agui, pause_store) of
        {ok, {Module, _Ref} = Handle} when is_atom(Module) ->
            {ok, Handle};
        _ ->
            Name = store_name(),
            case whereis(Name) of
                undefined -> none;
                _Pid -> {ok, beamai_pause_store_ets:handle(Name)}
            end
    end.

%% @doc 内置 ETS store 的注册名
-spec store_name() -> atom().
store_name() ->
    application:get_env(beamai_agui, pause_store_name, ?DEFAULT_STORE_NAME).

%% @doc 是否该由本应用的监督树启动内置 store（配了外部句柄就不启）
-spec own_store() -> boolean().
own_store() ->
    application:get_env(beamai_agui, pause_store) =:= undefined.

%%====================================================================
%% 未决暂停
%%====================================================================

%% @doc 这个 thread 有没有未决的暂停
%%
%% store 没起来时按「没有」处理：宿主没启 beamai_agui 应用只是拿不到 HITL，
%% 不该让每个普通请求都炸掉。
-spec pending(binary()) -> {ok, beamai_pause_store:snapshot()} | none.
pending(ThreadId) ->
    case available_store() of
        none ->
            none;
        {ok, Store} ->
            try
                beamai_pause_store:pause_load(Store, ThreadId)
            catch
                exit:_ -> none;
                error:_ -> none
            end
    end.

%% @doc 快照里被中断的那个 tool_call（原始形态）
-spec pending_tool_call(beamai_pause_store:snapshot()) -> map() | undefined.
pending_tool_call(Snapshot) when is_map(Snapshot) ->
    maps:get(pending_tool, Snapshot, undefined);
pending_tool_call(_) ->
    undefined.

%% @doc 被中断的 tool_call 的 id —— 前端要用它把答复对回那次调用
-spec pending_tool_call_id(beamai_pause_store:snapshot()) -> binary() | undefined.
pending_tool_call_id(Snapshot) ->
    case pending_tool_call(Snapshot) of
        TC when is_map(TC) ->
            {Id, _Name, _Args} = beamai_tool:parse_tool_call(TC),
            Id;
        _ ->
            undefined
    end.

%% @doc 清除该 thread 的未决暂停
%%
%% 正常路径不用调：agent 到达终态时上游自己会清。这个导出是给「放弃这次
%% 对话」用的（前端点了取消、或运维要把卡住的 thread 掐掉）。
-spec clear(binary()) -> ok.
clear(ThreadId) ->
    case available_store() of
        none ->
            ok;
        {ok, Store} ->
            try
                beamai_pause_store:pause_clear(Store, ThreadId)
            catch
                exit:_ -> ok;
                error:_ -> ok
            end
    end.
