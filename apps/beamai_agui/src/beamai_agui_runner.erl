%%%-------------------------------------------------------------------
%%% @doc 一次 run 的编排：装配 emitter → 建 agent → 跑（或续跑）→ 收尾
%%%
%%% run/3 在**调用进程**里同步跑完，事件经 Sink 逐个吐出。这是刻意的：
%%% emitter 的状态表是 private ETS，agent 的回调也全在跑 agent 的那个进程里
%%% 触发，同进程就没有并发问题。要流式回 HTTP 就由适配器另起一个进程跑它，
%%% 让连接进程专心写帧（见 beamai_agui_cowboy_handler）。
%%%
%%% == 两条路：新跑 / 续跑 ==
%%%
%%% 进来先问一句「这个 thread 有没有未决暂停」（beamai_agui_thread:pending/1）：
%%%
%%% - 没有 → 新跑：取最后一条 user 消息喂 `beamai_agent:stream/2`。
%%% - 有   → 续跑：解出决策喂 `beamai_agent:resume/3`。中断态由上游从
%%%   pause_store 透明读回，**这里不碰**（也碰不到——它在别的进程里产生的）。
%%%
%%% 两条路都用同一个工厂造 agent，只是第二条造出来的是个"空壳"：新 agent 没有
%%% 中断态，`resume/3` 发现这点就回落去 store 里取。所以工厂必须可重放——
%%% 详见 beamai_agui_thread 模块头。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_runner).

-export([run/3]).

-type sink() :: fun((beamai_agui_event:event()) -> ok).

%% @doc 跑一次 run（或续跑一次暂停），事件推给 Sink
%%
%% @param Input 解析后的 RunAgentInput
%% @param Factory agent 工厂（见 beamai_agui_registry）
%% @param Sink 事件汇（拿到的是事件 map，编码是适配器的事）
%% @returns ok —— 失败也已经作为 RUN_ERROR 发给 Sink 了，不再往上抛
-spec run(beamai_agui_types:run_input(),
          beamai_agui_registry:factory(), sink()) -> ok.
run(Input, Factory, Sink) ->
    ThreadId = beamai_agui_types:thread_id(Input),
    Emitter = beamai_agui_emitter:new(#{
        thread_id => ThreadId,
        run_id => beamai_agui_types:run_id(Input),
        sink => Sink
    }),
    try
        ok = beamai_agui_emitter:run_started(Emitter),
        dispatch(Input, Factory, Emitter, beamai_agui_thread:pending(ThreadId))
    catch
        Class:Reason:Stack ->
            logger:error("beamai_agui run crashed: ~p:~p", [Class, Reason],
                         #{stacktrace => Stack}),
            beamai_agui_emitter:run_error(Emitter, Reason)
    after
        beamai_agui_emitter:close(Emitter)
    end.

%%====================================================================
%% 内部 - 分路
%%====================================================================

dispatch(Input, Factory, Emitter, none) ->
    fresh_run(Input, Factory, Emitter);
dispatch(Input, Factory, Emitter, {ok, Snapshot}) ->
    resume_run(Input, Factory, Emitter, Snapshot).

%%====================================================================
%% 内部 - 新跑
%%====================================================================

fresh_run(Input, Factory, Emitter) ->
    case beamai_agui_types:last_user_message(Input) of
        {error, no_user_message} ->
            beamai_agui_emitter:run_error(Emitter, no_user_message);
        {ok, UserMessage} ->
            with_agent(Input, Factory, Emitter,
                       fun(Agent) ->
                           %% 用 stream/2 而非 run/2：AG-UI 的价值就在逐 token
                           %% 渲染，非流式等于把整个协议降级成一次性返回。
                           beamai_agent:stream(Agent, UserMessage)
                       end)
    end.

%%====================================================================
%% 内部 - 续跑
%%====================================================================

resume_run(Input, Factory, Emitter, Snapshot) ->
    PendingId = beamai_agui_thread:pending_tool_call_id(Snapshot),
    case beamai_agui_types:resume_input(Input, PendingId) of
        none ->
            %% thread 卡在暂停上，而这个请求没带任何可用的输入
            beamai_agui_emitter:run_error(Emitter, {awaiting_input, PendingId});
        {ok, Decision, Payload} ->
            ok = emit_resume_result(Emitter, PendingId, Decision, Payload),
            with_agent(Input, Factory, Emitter,
                       fun(Agent) -> beamai_agent:resume(Agent, Decision, Payload) end)
    end.

%% @private 把人给的答复补成被中断那次调用的 TOOL_CALL_RESULT
%%
%% 前端在暂停时看到的是一个**没有结果**的 TOOL_CALL_START/ARGS/END —— 那正是
%% 「等你回话」的表示。续跑时得把这个坑填上，否则前端那次调用永远悬着。
%%
%% 只有结果**由人给出**时才发得出来（reply / 拒绝 / 自由文本）：approved 是让
%% 服务端真去执行那个工具，输出要等执行完才知道，而上游 resume 的执行路径
%% 没有触发 on_tool_result（execute_tool_calls 没传 on_result），拿不到。
%% 这条缺口记在 README，属于上游可补的一小块。
emit_resume_result(_Emitter, undefined, _Decision, _Payload) ->
    ok;
emit_resume_result(Emitter, ToolCallId, Decision, Payload) ->
    case human_result(Decision, Payload) of
        undefined ->
            ok;
        Content ->
            beamai_agui_emitter:emit(
              Emitter, beamai_agui_event:tool_call_result(
                         beamai_id:gen_id(<<"msg">>), ToolCallId, Content))
    end.

%% 与上游 beamai_agent_interrupt:resume_action/3 的决策关键词保持一致
human_result(<<"approved">>, _Payload) -> undefined;
human_result(<<"rejected">>, Payload) -> reject_text(Payload);
human_result(<<"reject">>, Payload) -> reject_text(Payload);
human_result(<<"reply">>, Payload) -> maps:get(message, Payload, <<>>);
human_result(Text, _Payload) when is_binary(Text) -> Text;
human_result(_Decision, _Payload) -> undefined.

reject_text(#{message := R}) when is_binary(R), R =/= <<>> ->
    <<"已拒绝执行："/utf8, R/binary>>;
reject_text(_) ->
    <<"已拒绝执行"/utf8>>.

%%====================================================================
%% 内部 - 建 agent 与收尾
%%====================================================================

%% @private 造 agent 并把结果分派成事件
with_agent(Input, Factory, Emitter, Go) ->
    case Factory(Input, runtime_config(Input, Emitter)) of
        {ok, Agent} -> finish(Emitter, Go(Agent));
        {error, Reason} -> beamai_agui_emitter:run_error(Emitter, Reason)
    end.

%% @private 交给工厂的运行时配置——**必须整个 merge 进 beamai_agent:new/1**
%%
%% - callbacks：事件流的来源，不传就是个哑巴 agent；
%% - conversation_id：对齐 threadId，跨请求的历史与暂停都靠它索引；
%% - pause_store：中断态落库的去处，不配就没有跨请求 HITL。
runtime_config(Input, Emitter) ->
    Base = #{
        callbacks => beamai_agui_emitter:callbacks(Emitter),
        conversation_id => beamai_agui_types:thread_id(Input)
    },
    case beamai_agui_thread:available_store() of
        {ok, Store} -> Base#{pause_store => Store};
        none -> Base
    end.

finish(Emitter, {ok, Result, _State}) ->
    beamai_agui_emitter:run_finished(Emitter, result_of(Result));
finish(Emitter, {interrupt, Info, _State}) ->
    %% 中断态此刻已由上游落进 pause_store（agent 配了 pause_store）：
    %% 这次 run 到此为止，下一个带答复的请求会从 store 里接着跑。
    ok = beamai_agui_emitter:emit(
           Emitter, beamai_agui_event:custom(<<"interrupt">>, interrupt_of(Info))),
    beamai_agui_emitter:run_finished(Emitter);
finish(Emitter, {error, Reason}) ->
    beamai_agui_emitter:run_error(Emitter, Reason).

%% RUN_FINISHED.result：只回答案文本，run 的统计（iterations/usage）不属于协议
result_of(Result) ->
    #{<<"content">> => maps:get(content, Result, null)}.

%% CUSTOM(interrupt) 的载荷：前端要知道「在等哪次调用的回话」
interrupt_of(Info) ->
    Base = #{<<"type">> =>
                 atom_to_binary(maps:get(interrupt_type, Info, unknown), utf8)},
    case maps:get(interrupted_tool_call, Info, undefined) of
        TC when is_map(TC) ->
            {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
            Base#{<<"toolCallId">> => Id,
                  <<"toolCallName">> => Name,
                  <<"args">> => Args};
        _ ->
            Base
    end.
