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
%%% == 前端工具 ==
%%%
%%% `RunAgentInput.tools` 由 runtime 挂成 agent 的 `interrupt_tools`（见
%%% with_frontend_tools/2），因此前端定义的工具复用整条 HITL 管道：模型调到它
%%% 就暂停，前端执行完把结果当 `tool` 消息送回来续跑。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_runner).

-export([run/3, run/4]).

-type sink() :: fun((beamai_agui_event:event()) -> ok).

%% - `event_transform` —— 事件流插件的挂点，见 beamai_agui_emitter:emit/2
%% - `input_transform` —— 入站挂点：改写本轮的 RunAgentInput（A2UI 的用户动作回流
%%   走这条）。**在解析之后、分路之前**跑，于是新跑与续跑两条路都看得到它的产出
-type opts() :: #{
    event_transform => fun((beamai_agui_event:event()) ->
                              beamai_agui_event:event() | [beamai_agui_event:event()] | drop),
    input_transform => fun((beamai_agui_types:run_input()) ->
                              beamai_agui_types:run_input()),
    %% 显式打开后，共享 state 以 system 文本进入本轮；缺省关闭。
    inject_state => boolean()
}.

-export_type([sink/0, opts/0]).

%% @doc 跑一次 run（或续跑一次暂停），事件推给 Sink
%%
%% @param Input 解析后的 RunAgentInput
%% @param Factory agent 工厂（见 beamai_agui_registry）
%% @param Sink 事件汇（拿到的是事件 map，编码是适配器的事）
%% @returns ok —— 失败也已经作为 RUN_ERROR 发给 Sink 了，不再往上抛
-spec run(beamai_agui_types:run_input(),
          beamai_agui_registry:factory(), sink()) -> ok.
run(Input, Factory, Sink) ->
    run(Input, Factory, Sink, #{}).

%% @doc 同 run/3，另带运行时选项（见 opts/0）
-spec run(beamai_agui_types:run_input(),
          beamai_agui_registry:factory(), sink(), opts()) -> ok.
run(Input0, Factory, Sink, Opts) ->
    Input = transform_input(Input0, Opts),
    ThreadId = beamai_agui_types:thread_id(Input),
    Emitter = beamai_agui_emitter:new(#{
        thread_id => ThreadId,
        run_id => beamai_agui_types:run_id(Input),
        sink => Sink,
        parent_run_id => beamai_agui_types:parent_run_id(Input),
        event_transform => maps:get(event_transform, Opts, undefined),
        %% 缺省接上增量翻译：工具参数逐字、思维链逐字（认不出的 chunk 安静丢弃）
        raw_translator => beamai_agui_raw:translator(),
        state => initial_state(Input)
    }),
    try
        ok = beamai_agui_emitter:run_started(Emitter),
        %% 先立基线，后面的 STATE_DELTA 才有得可依
        ok = beamai_agui_emitter:state_snapshot(Emitter),
        Pending = beamai_agui_thread:pending(ThreadId),
        PromptState = prompt_state(Input, Pending),
        dispatch(Input, Factory, Emitter, PromptState, Pending, Opts)
    catch
        Class:Reason:Stack ->
            logger:error("beamai_agui run crashed: ~p:~p", [Class, Reason],
                         #{stacktrace => Stack}),
            beamai_agui_emitter:run_error(Emitter, Reason)
    after
        beamai_agui_emitter:close(Emitter)
    end.

%%====================================================================
%% 内部 - 入站挂点
%%====================================================================

%% @private 插件改写本轮输入（A2UI 的用户动作回流走这条）
%%
%% 插件炸了退回原输入 —— 与 `event_transform` 同款取舍：旁路增强不该让主流程出事。
transform_input(Input, Opts) ->
    case maps:get(input_transform, Opts, undefined) of
        Fun when is_function(Fun, 1) ->
            try Fun(Input) of
                #{thread_id := _} = Transformed -> Transformed;
                _ -> Input
            catch
                Class:Reason:Stack ->
                    logger:warning("beamai_agui input_transform crashed: ~p:~p",
                                   [Class, Reason], #{stacktrace => Stack}),
                    Input
            end;
        _ ->
            Input
    end.

%%====================================================================
%% 内部 - 分路
%%====================================================================

dispatch(Input, Factory, Emitter, PromptState, Pending, Opts) ->
    case Pending of
        none -> fresh_run(Input, Factory, Emitter, PromptState, Opts);
        {ok, Snapshot} ->
            resume_run(Input, Factory, Emitter, PromptState, Snapshot, Opts)
    end.

%%====================================================================
%% 内部 - 新跑
%%====================================================================

fresh_run(Input, Factory, Emitter, PromptState, Opts) ->
    case beamai_agui_types:last_user_message(Input) of
        {error, no_user_message} ->
            beamai_agui_emitter:run_error(Emitter, no_user_message);
        {ok, UserMessage} ->
            with_agent(Input, Factory, Emitter,
                       fun(Agent) ->
                           %% 用 stream/2 而非 run/2：AG-UI 的价值就在逐 token
                           %% 渲染，非流式等于把整个协议降级成一次性返回。
                           beamai_agent:stream(Agent, UserMessage)
                       end, PromptState, Opts)
    end.

%%====================================================================
%% 内部 - 续跑
%%====================================================================

resume_run(Input, Factory, Emitter, PromptState, Snapshot, Opts) ->
    PendingId = beamai_agui_thread:pending_tool_call_id(Snapshot),
    Kind = pending_kind(Input, Snapshot),
    case beamai_agui_types:resume_input(Input, PendingId) of
        none ->
            %% thread 卡在暂停上，而这个请求没带任何可用的输入
            beamai_agui_emitter:run_error(Emitter, {awaiting_input, PendingId});
        {ok, Decision0, Payload0} ->
            {Decision, Payload} = settle(Kind, Decision0, Payload0),
            ok = emit_resume_result(Emitter, PendingId, Kind, Decision, Payload),
            with_agent(Input, Factory, Emitter,
                       fun(Agent) ->
                           %% stream_resume 而非 resume：人回完话之后的那段答案
                           %% 才是用户在等的正文，它得逐 token 出来
                           beamai_agent:stream_resume(Agent, Decision, Payload)
                       end, PromptState, Opts)
    end.

%% @private 这次暂停在等的是**审批**还是**前端工具的执行结果**
%%
%% 判据只有一条：被中断的那个工具是不是 `RunAgentInput.tools` 里声明的。
%% 客户端每次请求都重发 `tools`，所以续跑那一轮照样算得出来。
pending_kind(Input, Snapshot) ->
    case beamai_agui_thread:pending_tool_call(Snapshot) of
        TC when is_map(TC) ->
            {_Id, Name, _Args} = beamai_tool:parse_tool_call(TC),
            case lists:member(Name, beamai_agui_types:frontend_tool_names(Input)) of
                true -> frontend_tool;
                false -> approval
            end;
        _ ->
            approval
    end.

%% @private 把客户端的答复翻成上游 `resume/3` 认的 {Decision, Payload}
%%
%% **前端工具没有"批准"这回事**：服务端没有实现，批准了也没人去执行。它要的是
%% 这次调用**返回了什么** —— 所以 `payload.result` 一出现就走 `reply`
%% （上游的 `reply` 语义正是"载荷即被中断工具的结果"）。
%%
%% 客户端只回了个决定、没给结果时**不伪造成功**：如实告诉模型这次调用没拿到结果。
%% 曾经这里把协议字段 `status:"resolved"` 当成自由文本发成了工具结果，模型据此
%% 宣布"已经打过招呼啦" —— 而客户端从头到尾没被调用过。
settle(frontend_tool, Decision, Payload) ->
    case frontend_result(Decision, Payload) of
        undefined when Decision =:= <<"rejected">> ->
            %% 人按了取消：如实告诉模型这一步没做成
            {<<"rejected">>, Payload};
        undefined ->
            {<<"reply">>, Payload#{message => no_result_text()}};
        Result ->
            {<<"reply">>, Payload#{message => Result}}
    end;
settle(approval, Decision, Payload) ->
    {Decision, Payload}.

%% @private 客户端把这次调用的结果放哪儿了
%%
%% 三条来路，都是「这次调用返回了什么」的写法：
%%
%% - `payload.result` —— 我们在 `responseSchema` 里要求的那个字段；
%% - `payload.message` —— **AG-UI 的原生路径**：客户端执行完，把结果作为一条
%%   `role=tool` 消息回传，`beamai_agui_types` 把它归一成了 `message`；
%% - 决策位上的一段自由文本 —— 人直接打了一句话（聊天式 HITL 的回落）。
frontend_result(Decision, Payload) ->
    case maps:get(result, Payload, undefined) of
        undefined ->
            first_text([maps:get(message, Payload, undefined), free_text(Decision)]);
        Result ->
            to_text(Result)
    end.

%% @private 决策位上不是关键词的那种，是人打的一句话
free_text(D) when is_binary(D) ->
    case lists:member(D, [<<"approved">>, <<"rejected">>, <<"reject">>, <<"reply">>]) of
        true -> undefined;
        false -> D
    end;
free_text(_) ->
    undefined.

first_text(Values) ->
    case [V || V <- Values, is_binary(V), V =/= <<>>] of
        [V | _] -> V;
        [] -> undefined
    end.

no_result_text() ->
    <<"[客户端没有回传这次调用的结果 —— 前端工具的答复应当放在 "
      "resume[].payload.result 里]"/utf8>>.

%% @private 把人给的答复补成被中断那次调用的 TOOL_CALL_RESULT
%%
%% 前端在暂停时看到的是一个**没有结果**的 TOOL_CALL_START/ARGS/END —— 那正是
%% 「等你回话」的表示。续跑时得把这个坑填上，否则前端那次调用永远悬着。
%%
%% 只有结果**已经确定**时才在这里发（前端回传的结果 / 拒绝语 / 人打的一句话）。
%% `approved` 是让服务端真去执行那个工具，结果得等执行完才知道 —— 那一条由上游
%% resume 执行时触发的 on_tool_result 经 emitter 发出，**不能在这里也发一遍**，
%% 否则前端会看到同一次调用的两条结果。
emit_resume_result(_Emitter, undefined, _Kind, _Decision, _Payload) ->
    ok;
emit_resume_result(Emitter, ToolCallId, Kind, Decision, Payload) ->
    case human_result(Kind, Decision, Payload) of
        undefined ->
            ok;
        Content ->
            beamai_agui_emitter:emit(
              Emitter, beamai_agui_event:tool_call_result(
                         beamai_id:gen_id(<<"msg">>), ToolCallId, Content))
    end.

%% 与上游 beamai_agent_interrupt:resume_action/3 的决策关键词保持一致
%%
%% `settle/3` 已经把前端工具那条路归一成了 `reply`，所以这里两类走同一张表。
human_result(_Kind, <<"approved">>, _Payload) -> undefined;
human_result(_Kind, <<"rejected">>, Payload) -> reject_text(Payload);
human_result(_Kind, <<"reject">>, Payload) -> reject_text(Payload);
human_result(_Kind, <<"reply">>, Payload) -> to_text(maps:get(message, Payload, <<>>));
human_result(_Kind, Text, _Payload) when is_binary(Text) -> Text;
human_result(_Kind, _Decision, _Payload) -> undefined.

reject_text(#{message := R}) when is_binary(R), R =/= <<>> ->
    <<"已拒绝执行："/utf8, R/binary>>;
reject_text(_) ->
    <<"已拒绝执行"/utf8>>.

%% @private 结果可以是任意 JSON 值（前端工具返回一个对象完全正常）
to_text(V) when is_binary(V) -> V;
to_text(V) when is_number(V) -> beamai_utils:encode_json(V);
to_text(V) when is_boolean(V) -> beamai_utils:encode_json(V);
to_text(null) -> <<"null">>;
to_text(V) when is_map(V); is_list(V) -> beamai_utils:encode_json(V);
to_text(V) -> iolist_to_binary(io_lib:format("~p", [V])).

%%====================================================================
%% 内部 - 建 agent 与收尾
%%====================================================================

%% @private 造 agent 并把结果分派成事件
with_agent(Input, Factory, Emitter, Go, PromptState, Opts) ->
    case Factory(Input, runtime_config(Input, Emitter)) of
        {ok, Agent} ->
            Ready = with_prompt(with_frontend_tools(Agent, Input), Input,
                                PromptState, Opts),
            finish(Input, Emitter, Go(Ready));
        {error, Reason} ->
            beamai_agui_emitter:run_error(Emitter, Reason)
    end.

%% @private 把本轮上下文 / 显式开启的 state 注入 system prompt
%%
%% `context` 是前端每轮重发的**页面状态**（`useAgentContext`）：当前选中了哪一行、
%% 用户是谁、页面在哪一屏。它是 turn 级的（这轮带就有，下轮不带就没了），所以
%% **追加而不是覆盖** —— 工厂那份人设是常量配置，不该被一次请求改掉。
%%
%% context 由 runtime 追加而不是让工厂自己读：与前端工具同一条理由，请求带上
%% 来的东西不该渗进"必须可重放"的工厂里。state 是可选注入：默认关闭，只有
%% 宿主显式打开才把整份 state 的 token 成本和暴露面接进来。
with_prompt(Agent, Input, PromptState, Opts) ->
    Agent0 = append_prompt(Agent, beamai_agui_types:context_prompt(Input)),
    case maps:get(inject_state, Opts, false) of
        true -> append_prompt(Agent0, beamai_agui_types:state_prompt(PromptState));
        false -> Agent0
    end.

append_prompt(Agent, undefined) ->
    Agent;
append_prompt(Agent, Text) when is_binary(Text) ->
    case maps:get(system_prompt, Agent, undefined) of
        P when is_binary(P), P =/= <<>> ->
            Agent#{system_prompt => <<P/binary, "\n\n", Text/binary>>};
        _ ->
            Agent#{system_prompt => Text}
    end.

%% 新跑的权威 state 是请求里的 map；续跑的权威 state 是暂停时保存的
%% saved_state —— 客户端在暂停期间的本地改动不应覆盖服务端累积结果。
prompt_state(Input, none) ->
    initial_state(Input);
prompt_state(_Input, {ok, Snapshot}) ->
    case maps:get(interrupt_state, Snapshot, undefined) of
        #{saved_state := State} -> State;
        _ -> #{}
    end.

%% @private 把前端声明的工具挂成 interrupt_tools
%%
%% `RunAgentInput.tools` 是**前端定义、前端执行**的工具：模型该看得见它们
%% （看不见就永远不会调），但服务端没有实现可跑。`interrupt_tools` 正好是这个
%% 语义——命中即暂停、把球交出去。于是前端工具走的是与 HITL **完全相同**的管道：
%% 暂停 → `CUSTOM(interrupt)` + 一个没有结果的 `TOOL_CALL_*` → 前端执行 →
%% 带 `role=tool` 消息回来续跑。
%%
%% 由 runtime 挂而不是让工厂挂：工具是**每个请求**带上来的，工厂却应当是可重放
%% 的常量配置（跨请求要造出一致的 agent，见 beamai_agui_thread）。
%%
%% **追加不覆盖**：工厂自己可能声明了服务端侧的中断工具（`ask_human` 之类），
%% 两者共存；重名时服务端的那个赢——它有真实现，前端那个只是个声明。
%%
%% 上游的 `interrupt_tool_to_spec` / `is_interrupt_tool` 同时认 atom 键与 binary 键，
%% 所以 AG-UI 传上来的定义原样挂上去即可，不用转换。
with_frontend_tools(Agent, Input) ->
    Existing = maps:get(interrupt_tools, Agent, []),
    case frontend_tools(Input, Existing) of
        [] -> Agent;
        New -> Agent#{interrupt_tools => Existing ++ New}
    end.

%% @private 合法（有名字）且不与既有中断工具重名的前端工具
%%
%% 没名字的条目直接丢：它会变成一个 name 为空的 tool spec，把整个 LLM 请求带坏。
frontend_tools(Input, Existing) ->
    Taken = [N || T <- Existing, (N = tool_name(T)) =/= undefined],
    lists:filter(fun(T) ->
        case tool_name(T) of
            undefined -> false;
            Name -> not lists:member(Name, Taken)
        end
    end, beamai_agui_types:tools(Input)).

tool_name(Tool) when is_map(Tool) ->
    case maps:get(<<"name">>, Tool, maps:get(name, Tool, undefined)) of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> undefined
    end;
tool_name(_) ->
    undefined.

%% @private 请求带上来的 state（不是 map 就当没有——外部输入不可信）
%%
%% 注意**续跑不走这条路**：暂停快照里带着服务端累积的 state，`resume` 从那里恢复。
%% 前端在暂停期间对 state 的本地改动不会被采纳 —— 一个停在半路的 turn，
%% 其状态的权威在服务端。
initial_state(Input) ->
    case beamai_agui_types:state(Input) of
        State when is_map(State) -> State;
        _ -> #{}
    end.

%% @private 交给工厂的运行时配置——**必须整个 merge 进 beamai_agent:new/1**
%%
%% - callbacks：事件流的来源，不传就是个哑巴 agent；
%% - conversation_id：对齐 threadId，跨请求的历史与暂停都靠它索引；
%% - initial_state：前端持有的那份共享状态，工具在它之上改；
%% - pause_store：中断态落库的去处，不配就没有跨请求 HITL；
%% - subagent_observer：子 agent lane 的观察者（开关开着才有，见下）。
runtime_config(Input, Emitter) ->
    Base = #{
        callbacks => beamai_agui_emitter:callbacks(Emitter),
        conversation_id => beamai_agui_types:thread_id(Input),
        %% 前端持有的那份 state 种进 agent：工具在它之上增量改
        initial_state => initial_state(Input)
    },
    WithStore = case beamai_agui_thread:available_store() of
        {ok, Store} -> Base#{pause_store => Store};
        none -> Base
    end,
    %% 子 agent lane 的观察者：**开关关着就不放这个键**，工厂那边
    %% `maps:get(subagent_observer, RuntimeCfg, undefined)` 取到 undefined，
    %% `beamai_agui_subagent:delegate_tool/2` 于是原样返回上游的工具 ——
    %% 「不塞就什么都不会发生」。
    case beamai_agui_subagent:enabled() of
        true -> WithStore#{subagent_observer => beamai_agui_subagent:observer(Emitter)};
        false -> WithStore
    end.

finish(_Input, Emitter, {ok, Result, _State}) ->
    beamai_agui_emitter:run_finished(Emitter, result_of(Result),
                                     beamai_agui_event:usage(usage_of(Result)));
finish(Input, Emitter, {interrupt, Info, _State}) ->
    %% 中断态此刻已由上游落进 pause_store（agent 配了 pause_store）：
    %% 这次 run 到此为止，下一个带答复的请求会从 store 里接着跑。
    %%
    %% **只发 `RUN_FINISHED + outcome:"interrupt"`** —— 它是 AG-UI 的一等表达，
    %% 通用客户端（`useInterrupt` / `useHumanInTheLoop`）认的就是它，而且它同时
    %% 把流收口了（run 必须有终态，流才关得掉）。
    %%
    %% 曾经这里还并发一条 `CUSTOM(interrupt)` 作为"我们先于协议实现的那份"。
    %% 已经去掉：同一件事发两遍，接入方看日志会以为是两件事，而且两份**总有一天
    %% 会不同步** —— 两个真相店从来只是麻烦的来源。
    beamai_agui_emitter:run_interrupted(Emitter, [beamai_agui_event:interrupt(
                                                    interrupt_spec(Input, Emitter, Info))]);
finish(_Input, Emitter, {error, Reason}) ->
    beamai_agui_emitter:run_error(Emitter, Reason).

%% RUN_FINISHED.result：只回答案文本；用量走协议自己的 `usage` 位，
%% 迭代次数是我们的内部统计，协议里没有对应位，不硬塞
result_of(Result) ->
    #{<<"content">> => maps:get(content, Result, null)}.

usage_of(Result) -> maps:get(usage, Result, undefined).

%% @private `RUN_FINISHED.outcome.interrupts[]` 那一条的原料
%%
%% `id` 取被中断调用的 tool-call id —— 它本来就唯一，客户端把它原样放回
%% `resume[].interruptId`，服务端据此认得出「回的是不是当前这一条」
%% （beamai_agui_types:resume_input/2）。纯 ask-user 式的暂停没有 tool call，
%% 退回 run id 派生的一个稳定 id。
%%
%% `kind` 决定客户端该回**决定**还是**结果**，见 beamai_agui_event:interrupt/1。
interrupt_spec(Input, Emitter, Info) ->
    Reason = atom_to_binary(maps:get(interrupt_type, Info, paused), utf8),
    case maps:get(interrupted_tool_call, Info, undefined) of
        TC when is_map(TC) ->
            {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
            #{id => Id, reason => Reason, tool_call_id => Id,
              tool_name => Name, args => Args, kind => kind_of(Input, Name)};
        _ ->
            #{id => <<(run_id_of(Emitter))/binary, "-interrupt">>, reason => Reason,
              kind => approval}
    end.

kind_of(Input, Name) ->
    case lists:member(Name, beamai_agui_types:frontend_tool_names(Input)) of
        true -> frontend_tool;
        false -> approval
    end.

run_id_of(Emitter) ->
    beamai_agui_emitter:run_id(Emitter).
