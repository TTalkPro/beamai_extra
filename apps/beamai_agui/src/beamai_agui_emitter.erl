%%%-------------------------------------------------------------------
%%% @doc 翻译层：beamai_agent 回调 → AG-UI 事件序列
%%%
%%% 这是整个 app 的核心。beamai_agent 的回调是「发生了什么」的流水账，
%%% AG-UI 要的是**成对、带 id、可增量渲染**的事件序列，两者对不上的地方
%%% 全在这里补：
%%%
%%% | AG-UI 事件 | 由哪个回调驱动 |
%%% |---|---|
%%% | `STEP_STARTED` / `STEP_FINISHED` | `on_llm_call` / `on_llm_result` |
%%% | `TEXT_MESSAGE_START` | 第一个 token 到达时**惰性**发出 |
%%% | `TEXT_MESSAGE_CONTENT` | `on_token`（Meta 带 `message_id`） |
%%% | `TEXT_MESSAGE_END` | `on_message_end` |
%%% | `TOOL_CALL_START/ARGS/END` | `on_message_end` 里那条消息的 `tool_calls` |
%%% | `TOOL_CALL_RESULT` | `on_tool_result`（Info 带 `tool_call_id`） |
%%%
%%% == 为什么 TEXT_MESSAGE_START 是惰性的 ==
%%%
%%% `on_message_start` 在 LLM 调用**前**触发，那时还不知道这一轮会产出文本
%%% 还是纯 tool_calls。纯 tool_calls 的回合不该有文本消息，所以这里只记下
%%% id，等第一个 token 真的到了再发 START —— 一个字都没有的回合自然不会有
%%% 空的 START/END 对。
%%%
%%% 非流式（`beamai_agent:run/2`，没有 token）也照样能出文本消息：
%%% `on_message_end` 发现这条消息没开过文本流、但有 content，就一次性补齐
%%% START + CONTENT + END。
%%%
%%% == 状态放在 ETS 里 ==
%%%
%%% 回调是若干个独立的 fun，没法函数式地串状态。agent 的全部回调都在**同一个
%%% 进程**里触发（并发工具的结果由 beamai_tool_batch_worker 代理回父进程再
%%% 触发回调），故用一张 private ETS 存状态，无并发、无需加锁。表的属主是
%%% 调用 new/1 的进程，进程退出表自动消失；显式结束用 close/1。
%%%
%%% == 共享状态 ==
%%%
%%% AG-UI 的 state 是**前端与 agent 共有**的一份数据：请求带 `state` 上来，
%%% runtime 把它种进 agent（`initial_state`），工具经 writes 修改，改动以
%%% `STATE_DELTA`（JSON Patch）推回前端。
%%%
%%% 每次 run 开头先发一条 `STATE_SNAPSHOT` 立基线 —— 补丁是相对某一份具体的
%%% state 算的，没有共同基线就对不上。
%%%
%%% == raw 事件的增量 ==
%%%
%%% `on_llm_event` 透出的是 provider 原样的 SSE chunk，`TOOL_CALL_ARGS` 的逐字
%%% 增量与 `REASONING_*` 都在里面，需按 provider 分支解读 —— 那些分支在
%%% beamai_agui_raw，经 `raw_translator` 配置项接进来（runner 缺省接上）。
%%%
%%% 接上之后两条路会**争同一个 TOOL_CALL_START**：增量路在第一个 args 片段到达时
%%% 就开了调用，而 `on_message_end` 手里那条 assistant 消息也带着完整的 tool_calls。
%%% 故 emitter 记着「哪些 id 已经流过」，`on_message_end` 对它们只补一个
%%% `TOOL_CALL_END`。没有 translator（或非流式）时退化成一次性发完整 JSON——
%%% 语义一样，只是没有打字机效果。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_emitter).

-export([new/1, close/1, callbacks/1]).
-export([thread_id/1, run_id/1, sink/1]).
-export([tool_open/4, tool_args/3, reasoning_delta/2]).
-export([state_snapshot/1]).
-export([run_started/1, run_finished/1, run_finished/2, run_finished/3]).
-export([run_interrupted/2, run_error/2]).
-export([emit/2]).

-export_type([emitter/0]).

-type emitter() :: #{table := ets:tid()}.

%% raw_translator: fun((Event :: map(), Emitter) -> ok)，见模块头
-type opts() :: #{
    thread_id := binary(),
    run_id := binary(),
    sink := fun((beamai_agui_event:event()) -> ok),
    parent_run_id => binary() | undefined,
    %% 每条事件都补上的字段（子 agent lane 的 `subagentRunId` 走这条），见 emit/2
    tag => map(),
    %% 状态表的可见性。缺省 private（同进程无并发）；子 agent lane 要 public ——
    %% 表在**发起委派的那个工具进程**里建，回调却在 subagent manager 的 worker
    %% 进程里触发，跨进程读不到 private 表
    access => private | public,
    raw_translator => fun((map(), emitter()) -> ok),
    %% 事件流插件的通用挂点，见 emit/2
    event_transform => fun((beamai_agui_event:event()) ->
                              beamai_agui_event:event() | [beamai_agui_event:event()] | drop),
    state => map()
}.

%%====================================================================
%% 生命周期
%%====================================================================

%% @doc 新建 emitter（表属主 = 调用进程，即跑 agent 的那个进程）
-spec new(opts()) -> emitter().
new(#{thread_id := ThreadId, run_id := RunId, sink := Sink} = Opts) ->
    Tab = ets:new(beamai_agui_emitter, [set, maps:get(access, Opts, private)]),
    true = ets:insert(Tab, {config, #{thread_id => ThreadId,
                                      run_id => RunId,
                                      sink => Sink,
                                      parent_run_id =>
                                          maps:get(parent_run_id, Opts, undefined),
                                      tag => maps:get(tag, Opts, #{}),
                                      event_transform =>
                                          maps:get(event_transform, Opts, undefined),
                                      raw_translator =>
                                          maps:get(raw_translator, Opts, undefined)}}),
    true = ets:insert(Tab, {text_open, undefined}),
    true = ets:insert(Tab, {step, 0}),
    %% 增量路的状态：index → {Id, Name}（provider 后续 delta 只给 index）、
    %% 已经流过的调用 id、以及推理段是否开着
    true = ets:insert(Tab, {tool_index, #{}}),
    true = ets:insert(Tab, {streamed, #{}}),
    true = ets:insert(Tab, {reasoning, undefined}),
    %% 上一份 state：STATE_DELTA 是相对它算的
    true = ets:insert(Tab, {state, maps:get(state, Opts, #{})}),
    #{table => Tab}.

%% @doc 本次 run 所属的 thread
-spec thread_id(emitter()) -> binary().
thread_id(#{table := Tab}) ->
    maps:get(thread_id, config(Tab)).

%% @doc 本次 run 的 id
-spec run_id(emitter()) -> binary().
run_id(#{table := Tab}) ->
    maps:get(run_id, config(Tab)).

%% @doc 本 emitter 的事件汇
%%
%% 交出去是给**子 agent lane** 用的：lane 是另一个发射器，但事件要进同一条流。
%% sink 是个闭包，跨进程调用没有问题（它要么 cast 给会话，要么发消息给请求进程）。
-spec sink(emitter()) -> fun((beamai_agui_event:event()) -> ok).
sink(#{table := Tab}) ->
    maps:get(sink, config(Tab)).

%% @doc 释放 emitter
-spec close(emitter()) -> ok.
close(#{table := Tab}) ->
    true = ets:delete(Tab),
    ok.

%% @doc 直接发一个事件（供 runner 发 run 级事件、或宿主发 CUSTOM）
%%
%% `event_transform` 是**事件流插件的通用挂点**：可以改写一条事件、吞掉它
%% （返回 `drop`）、或在它前后插事件（返回列表）。A2UI / 生成式 UI 那类插件
%% 就住在这个位置 —— 它们把工具调用翻成 `CUSTOM` 活动事件，而 runner / emitter
%% 一个字都不用改。
%%
%% transform 自己抛异常不该把 run 带走：它是旁路增强，出事就退回原事件。
%%
%% `tag` 在 transform **之后**打：插件产出的事件同样属于这条 lane，漏打会让前端
%% 把子 agent 的 activity 画到主线里。已有的字段不覆盖（见 beamai_agui_event:tag/2）。
-spec emit(emitter(), beamai_agui_event:event()) -> ok.
emit(#{table := Tab}, Event) ->
    #{sink := Sink, tag := Tags} = Config = config(Tab),
    lists:foreach(fun(E) -> _ = Sink(beamai_agui_event:tag(E, Tags)) end,
                  transform(Event, Config)),
    ok.

%% @private 过一遍插件；认不出的返回值按"原样"处理
transform(Event, #{event_transform := Fun}) when is_function(Fun, 1) ->
    try Fun(Event) of
        drop -> [];
        Events when is_list(Events) -> Events;
        Rewritten when is_map(Rewritten) -> [Rewritten];
        _ -> [Event]
    catch
        Class:Reason:Stack ->
            logger:warning("beamai_agui event_transform crashed: ~p:~p", [Class, Reason],
                           #{stacktrace => Stack}),
            [Event]
    end;
transform(Event, _Config) ->
    [Event].

%%====================================================================
%% run 级事件
%%====================================================================

%% @doc RUN_STARTED
-spec run_started(emitter()) -> ok.
run_started(#{table := Tab} = E) ->
    #{thread_id := T, run_id := R, parent_run_id := P} = config(Tab),
    emit(E, beamai_agui_event:run_started(T, R, P)).

%% @doc STATE_SNAPSHOT：把当前这份 state 作为基线告诉前端
%%
%% 每次 run 开头发一次。**必须有基线**，后面的 STATE_DELTA 才有意义 —— JSON Patch
%% 是相对某一份具体的 state 算的，前端手里那份要是和服务端不一致，增量打上去就是错的。
-spec state_snapshot(emitter()) -> ok.
state_snapshot(#{table := Tab} = E) ->
    emit(E, beamai_agui_event:state_snapshot(ets:lookup_element(Tab, state, 2))).

%% @doc RUN_FINISHED
-spec run_finished(emitter()) -> ok.
run_finished(#{table := Tab} = E) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_finished(T, R)).

%% @doc RUN_FINISHED（带结果）
-spec run_finished(emitter(), term()) -> ok.
run_finished(E, Result) ->
    run_finished(E, Result, []).

%% @doc RUN_FINISHED（带结果与用量）
-spec run_finished(emitter(), term(), [map()]) -> ok.
run_finished(#{table := Tab} = E, Result, Usage) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_finished(T, R, Result, Usage)).

%% @doc RUN_FINISHED + `outcome:"interrupt"`（AG-UI 的 interrupt 协议）
-spec run_interrupted(emitter(), [map()]) -> ok.
run_interrupted(#{table := Tab} = E, Interrupts) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_interrupted(T, R, Interrupts)).

%% @doc RUN_ERROR（先闭合还开着的文本消息，避免留下不成对的 START）
-spec run_error(emitter(), term()) -> ok.
run_error(E, Reason) ->
    ok = close_reasoning(E),
    ok = close_open_text(E),
    emit(E, beamai_agui_event:run_error(format_reason(Reason), <<"agent_error">>)).

%%====================================================================
%% 回调装配
%%====================================================================

%% @doc 交给 beamai_agent:new/1 的 callbacks map
-spec callbacks(emitter()) -> beamai_agent_callbacks:callbacks().
callbacks(E) ->
    #{
        on_llm_call      => fun(_Msgs, _Meta) -> step_started(E) end,
        on_llm_result    => fun(_Resp, _Meta) -> step_finished(E) end,
        on_llm_event     => fun(Event, _Meta) -> raw_event(E, Event) end,
        on_message_start => fun(MsgId, _Meta) -> message_start(E, MsgId) end,
        on_message_end   => fun(Msg, Meta) -> message_end(E, Msg, Meta) end,
        on_token         => fun(Token, Meta) -> token(E, Token, Meta) end,
        on_tool_result   => fun(_Name, Result, Info) -> tool_result(E, Result, Info) end,
        on_state_change  => fun(State, _Meta) -> state_changed(E, State) end
    }.

%%====================================================================
%% 增量入口（供 beamai_agui_raw 调用）
%%====================================================================

%% @doc 一次工具调用在流里露头：记下 index→id 并发 TOOL_CALL_START
%%
%% 幂等：同一个 index 重复开只算一次（provider 偶尔会重发首片）。
-spec tool_open(emitter(), term(), binary(), binary()) -> ok.
tool_open(#{table := Tab} = E, Index, Id, Name) ->
    Streams = ets:lookup_element(Tab, tool_index, 2),
    case maps:is_key(Index, Streams) of
        true ->
            ok;
        false ->
            true = ets:insert(Tab, {tool_index, Streams#{Index => Id}}),
            true = ets:insert(Tab, {streamed,
                                    (ets:lookup_element(Tab, streamed, 2))#{Id => true}}),
            ok = close_reasoning(E),
            emit(E, beamai_agui_event:tool_call_start(Id, Name, pending_id(Tab)))
    end.

%% @doc 工具参数的一个片段（AG-UI 的 delta 本就是 JSON **文本**片段，原样透传）
-spec tool_args(emitter(), term(), binary()) -> ok.
tool_args(#{table := Tab} = E, Index, Delta) ->
    case maps:get(Index, ets:lookup_element(Tab, tool_index, 2), undefined) of
        undefined -> ok;  %% 没见过开头的片段无法归属，丢掉
        Id -> emit(E, beamai_agui_event:tool_call_args(Id, Delta))
    end.

%% @doc 推理增量：第一片顺带把 REASONING_START + MESSAGE_START 补上
-spec reasoning_delta(emitter(), binary()) -> ok.
reasoning_delta(_E, <<>>) ->
    ok;
reasoning_delta(#{table := Tab} = E, Delta) ->
    MsgId = case ets:lookup_element(Tab, reasoning, 2) of
        undefined ->
            NewId = beamai_id:gen_id(<<"reasoning">>),
            true = ets:insert(Tab, {reasoning, NewId}),
            ok = emit(E, beamai_agui_event:reasoning_start(NewId)),
            ok = emit(E, beamai_agui_event:reasoning_message_start(NewId)),
            NewId;
        Existing ->
            Existing
    end,
    emit(E, beamai_agui_event:reasoning_message_content(MsgId, Delta)).

%%====================================================================
%% 内部 - 各回调的翻译
%%====================================================================

%% @private 一条 assistant 消息开始：只记 id，START 等第一个 token（见模块头）
message_start(#{table := Tab}, MsgId) ->
    true = ets:insert(Tab, {pending, MsgId}),
    ok.

%% @private 文本增量：第一个 token 顺带把 TEXT_MESSAGE_START 补上
token(_E, <<>>, _Meta) ->
    ok;
token(#{table := Tab} = E, Token, Meta) ->
    %% 正文一开口，推理段就结束了
    ok = close_reasoning(E),
    MsgId = maps:get(message_id, Meta, undefined),
    case {MsgId, ets:lookup_element(Tab, text_open, 2)} of
        {undefined, _} ->
            %% 没有 message_id 的 token 无法归属（旧版 agent），只能丢弃
            ok;
        {Id, Id} ->
            emit(E, beamai_agui_event:text_message_content(Id, Token));
        {Id, _Other} ->
            true = ets:insert(Tab, {text_open, Id}),
            ok = emit(E, beamai_agui_event:text_message_start(Id)),
            emit(E, beamai_agui_event:text_message_content(Id, Token))
    end.

%% @private 一条 assistant 消息落定
%%
%% 三种收尾，互斥：
%%   1. 这条消息开过文本流 → 闭合 TEXT_MESSAGE_END
%%   2. 没开过但有 content（非流式路径）→ 一次性补 START + CONTENT + END
%%   3. Msg=undefined（LLM 出错 / 无可存内容）→ 什么都不发（run_error 收尾）
%% 之后若消息里有 tool_calls，逐个发 START + ARGS + END。
message_end(#{table := Tab} = E, Msg, Meta) ->
    ok = close_reasoning(E),
    MsgId = maps:get(message_id, Meta, undefined),
    Open = ets:lookup_element(Tab, text_open, 2),
    ok = case {Msg, Open =:= MsgId andalso MsgId =/= undefined} of
        {_, true} ->
            true = ets:insert(Tab, {text_open, undefined}),
            emit(E, beamai_agui_event:text_message_end(MsgId));
        {undefined, false} ->
            ok;
        {_, false} ->
            emit_whole_text(E, MsgId, maps:get(content, Msg, null))
    end,
    ok = emit_tool_calls(E, MsgId, tool_calls_of(Msg)),
    %% index 是**每次 LLM 调用**内的序号，下一条消息要从头记
    true = ets:insert(Tab, {tool_index, #{}}),
    ok.

%% @private 非流式路径：整条文本一次性发完
emit_whole_text(_E, _MsgId, null) -> ok;
emit_whole_text(_E, _MsgId, undefined) -> ok;
emit_whole_text(_E, _MsgId, <<>>) -> ok;
emit_whole_text(_E, undefined, _Content) -> ok;
emit_whole_text(E, MsgId, Content) when is_binary(Content) ->
    ok = emit(E, beamai_agui_event:text_message_start(MsgId)),
    ok = emit(E, beamai_agui_event:text_message_content(MsgId, Content)),
    emit(E, beamai_agui_event:text_message_end(MsgId));
emit_whole_text(_E, _MsgId, _Content) ->
    %% 多模态 content_blocks 等非 binary 形态：骨架阶段不翻译
    ok.

%% @private assistant 消息里的 tool_calls → START + ARGS + END
%%
%% 参数一次性发完整 JSON。要逐字增量得接 raw_translator（见模块头）。
emit_tool_calls(_E, _MsgId, []) ->
    ok;
emit_tool_calls(#{table := Tab} = E, MsgId, ToolCalls) ->
    Streamed = ets:lookup_element(Tab, streamed, 2),
    lists:foreach(fun(TC) ->
        {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        case maps:is_key(Id, Streamed) of
            true ->
                %% 增量路已经发过 START + 若干 ARGS，这里只补一个收尾
                ok = emit(E, beamai_agui_event:tool_call_end(Id));
            false ->
                ok = emit(E, beamai_agui_event:tool_call_start(Id, Name, MsgId)),
                ok = emit(E, beamai_agui_event:tool_call_args(
                               Id, beamai_utils:encode_json(Args))),
                ok = emit(E, beamai_agui_event:tool_call_end(Id))
        end
    end, ToolCalls).

%% @private 工具结果：靠 Info 里的 tool_call_id 配回调用
%%
%% 并发批次下 on_tool_result 谁先完成谁先触发，只靠工具名配不出来
%% （同名工具可以在一批里出现多次）——这就是上游把 tool_call_id 透出来的理由。
tool_result(E, Result, Info) ->
    case maps:get(tool_call_id, Info, undefined) of
        undefined ->
            ok;
        ToolCallId ->
            MsgId = beamai_id:gen_id(<<"msg">>),
            emit(E, beamai_agui_event:tool_call_result(
                      MsgId, ToolCallId, to_binary(Result)))
    end.

%% @private state 槽变了：与上一份算 JSON Patch 发出去
%%
%% 上游只在 state **确实变了**时才触发这个回调，但补丁仍可能为空 —— 比如它变回了
%% 上一次快照的样子。空补丁不发：一条什么都不改的 STATE_DELTA 对前端没有意义。
state_changed(#{table := Tab} = E, State) ->
    Last = ets:lookup_element(Tab, state, 2),
    case beamai_agui_state:diff(Last, State) of
        [] ->
            ok;
        Ops ->
            true = ets:insert(Tab, {state, State}),
            emit(E, beamai_agui_event:state_delta(Ops))
    end.

%% @private provider 原始流事件：交给可选的 raw_translator，没配就丢
raw_event(#{table := Tab} = E, Event) ->
    case maps:get(raw_translator, config(Tab)) of
        undefined -> ok;
        Fun when is_function(Fun, 2) -> _ = Fun(Event, E), ok
    end.

%% @private STEP_STARTED（步名用递增序号，够前端区分「第几轮」）
step_started(#{table := Tab} = E) ->
    N = ets:update_counter(Tab, step, 1),
    emit(E, beamai_agui_event:step_started(step_name(N))).

%% @private STEP_FINISHED（与 step_started 用同一个序号）
step_finished(#{table := Tab} = E) ->
    N = ets:lookup_element(Tab, step, 2),
    emit(E, beamai_agui_event:step_finished(step_name(N))).

%%====================================================================
%% 内部 - 辅助
%%====================================================================

config(Tab) ->
    ets:lookup_element(Tab, config, 2).

%% @private 闭合还开着的文本消息（错误路径用）
close_open_text(#{table := Tab} = E) ->
    case ets:lookup_element(Tab, text_open, 2) of
        undefined ->
            ok;
        MsgId ->
            true = ets:insert(Tab, {text_open, undefined}),
            emit(E, beamai_agui_event:text_message_end(MsgId))
    end.

%% @private 收掉还开着的推理段（MESSAGE_END 必须排在 END 前面）
close_reasoning(#{table := Tab} = E) ->
    case ets:lookup_element(Tab, reasoning, 2) of
        undefined ->
            ok;
        MsgId ->
            true = ets:insert(Tab, {reasoning, undefined}),
            ok = emit(E, beamai_agui_event:reasoning_message_end(MsgId)),
            emit(E, beamai_agui_event:reasoning_end(MsgId))
    end.

%% @private 当前这条 assistant 消息的 id（工具调用挂在它下面）
pending_id(Tab) ->
    case ets:lookup(Tab, pending) of
        [{pending, Id}] -> Id;
        [] -> undefined
    end.

tool_calls_of(Msg) when is_map(Msg) -> maps:get(tool_calls, Msg, []);
tool_calls_of(_) -> [].

step_name(N) ->
    <<"step-", (integer_to_binary(N))/binary>>.

to_binary(B) when is_binary(B) -> B;
to_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).

format_reason(Reason) when is_binary(Reason) -> Reason;
format_reason(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).
