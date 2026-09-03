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
%%% == 尚未接上的：raw 事件的增量 ==
%%%
%%% `on_llm_event` 透出的是 provider 原样的 SSE chunk（binary 键、各家形状
%%% 不同），`TOOL_CALL_ARGS` 的逐字增量与 `REASONING_*` 都在里面。要接就得
%%% 按 provider 分支解读，故这里留成可选的 `raw_translator` 配置：给了就用，
%%% 不给则退化成"参数在 on_message_end 时一次性发完整 JSON"——语义正确，只是
%%% 前端看不到工具参数的打字机效果。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_emitter).

-export([new/1, close/1, callbacks/1]).
-export([run_started/1, run_finished/1, run_finished/2, run_error/2]).
-export([emit/2]).

-export_type([emitter/0]).

-type emitter() :: #{table := ets:tid()}.

%% raw_translator: fun((Event :: map(), Emitter) -> ok)，见模块头
-type opts() :: #{
    thread_id := binary(),
    run_id := binary(),
    sink := fun((beamai_agui_event:event()) -> ok),
    raw_translator => fun((map(), emitter()) -> ok)
}.

%%====================================================================
%% 生命周期
%%====================================================================

%% @doc 新建 emitter（表属主 = 调用进程，即跑 agent 的那个进程）
-spec new(opts()) -> emitter().
new(#{thread_id := ThreadId, run_id := RunId, sink := Sink} = Opts) ->
    Tab = ets:new(beamai_agui_emitter, [set, private]),
    true = ets:insert(Tab, {config, #{thread_id => ThreadId,
                                      run_id => RunId,
                                      sink => Sink,
                                      raw_translator =>
                                          maps:get(raw_translator, Opts, undefined)}}),
    true = ets:insert(Tab, {text_open, undefined}),
    true = ets:insert(Tab, {step, 0}),
    #{table => Tab}.

%% @doc 释放 emitter
-spec close(emitter()) -> ok.
close(#{table := Tab}) ->
    true = ets:delete(Tab),
    ok.

%% @doc 直接发一个事件（供 runner 发 run 级事件、或宿主发 CUSTOM）
-spec emit(emitter(), beamai_agui_event:event()) -> ok.
emit(#{table := Tab}, Event) ->
    #{sink := Sink} = config(Tab),
    _ = Sink(Event),
    ok.

%%====================================================================
%% run 级事件
%%====================================================================

%% @doc RUN_STARTED
-spec run_started(emitter()) -> ok.
run_started(#{table := Tab} = E) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_started(T, R)).

%% @doc RUN_FINISHED
-spec run_finished(emitter()) -> ok.
run_finished(#{table := Tab} = E) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_finished(T, R)).

%% @doc RUN_FINISHED（带结果）
-spec run_finished(emitter(), term()) -> ok.
run_finished(#{table := Tab} = E, Result) ->
    #{thread_id := T, run_id := R} = config(Tab),
    emit(E, beamai_agui_event:run_finished(T, R, Result)).

%% @doc RUN_ERROR（先闭合还开着的文本消息，避免留下不成对的 START）
-spec run_error(emitter(), term()) -> ok.
run_error(E, Reason) ->
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
        on_tool_result   => fun(_Name, Result, Info) -> tool_result(E, Result, Info) end
    }.

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
    emit_tool_calls(E, MsgId, tool_calls_of(Msg)).

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
emit_tool_calls(E, MsgId, ToolCalls) ->
    lists:foreach(fun(TC) ->
        {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        ok = emit(E, beamai_agui_event:tool_call_start(Id, Name, MsgId)),
        ok = emit(E, beamai_agui_event:tool_call_args(
                       Id, beamai_utils:encode_json(Args))),
        ok = emit(E, beamai_agui_event:tool_call_end(Id))
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

tool_calls_of(Msg) when is_map(Msg) -> maps:get(tool_calls, Msg, []);
tool_calls_of(_) -> [].

step_name(N) ->
    <<"step-", (integer_to_binary(N))/binary>>.

to_binary(B) when is_binary(B) -> B;
to_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).

format_reason(Reason) when is_binary(Reason) -> Reason;
format_reason(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).
