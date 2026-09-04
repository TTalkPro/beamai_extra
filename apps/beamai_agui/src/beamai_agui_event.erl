%%%-------------------------------------------------------------------
%%% @doc AG-UI 事件构造与编码
%%%
%%% 事件是**带 binary 键的 map**，形状即 AG-UI 线上格式（BaseEvent 的
%%% `type` / `timestamp` 加各事件自己的字段），可直接 json 编码。
%%%
%%% == SSE 帧 ==
%%%
%%% AG-UI 的 SSE 帧**不带 event 名**，只有 `data:` 一行 —— 事件类型在 JSON 的
%%% `type` 字段里。故这里不用 beamai_sse:encode_event/2（它会多写一行
%%% `event: ...`，浏览器端 EventSource 会按具名事件分发，AG-UI 客户端收不到）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agui_event).

%% 构造
-export([new/2]).
-export([run_started/2, run_started/3]).
-export([run_finished/2, run_finished/3, run_finished/4]).
-export([run_cancelled/2, run_interrupted/3, run_error/2]).
-export([interrupt/1, usage/1]).
-export([step_started/1, step_finished/1]).
-export([text_message_start/1, text_message_content/2, text_message_end/1]).
-export([tool_call_start/3, tool_call_args/2, tool_call_end/1, tool_call_result/3]).
-export([reasoning_start/1, reasoning_message_start/1,
         reasoning_message_content/2, reasoning_message_end/1, reasoning_end/1]).
-export([state_snapshot/1, state_delta/1, messages_snapshot/1]).
-export([activity_snapshot/3, activity_snapshot/4, activity_delta/3]).
-export([subagent_started/1, subagent_finished/1, subagent_error/3]).
-export([text_message_chunk/2, text_message_chunk/3, tool_call_chunk/2, tool_call_chunk/4]).
-export([custom/2, raw/1, tag/2]).

%% 断言
-export([type_of/1, is_terminal/1]).

%% 编码
-export([to_json/1, to_sse/1, to_sse/2]).

-export_type([event/0]).

-type event() :: #{binary() => term()}.

%%====================================================================
%% 构造
%%====================================================================

%% @doc 通用构造：类型 + 该事件自己的字段（自动补 timestamp）
-spec new(binary(), map()) -> event().
new(Type, Fields) ->
    Fields#{<<"type">> => Type,
            <<"timestamp">> => erlang:system_time(millisecond)}.

%% @doc 一次 run 开始
-spec run_started(binary(), binary()) -> event().
run_started(ThreadId, RunId) ->
    run_started(ThreadId, RunId, undefined).

%% @doc 一次 run 开始（带父 run 声明）
%%
%% `parentRunId` 只有 `RUN_STARTED` 有这一位（`RUN_FINISHED` / `RUN_ERROR` 都没有），
%% 所以父链的声明就这一次机会。我们**不解释它** —— 不建父子索引、不做级联停止，
%% 只把客户端 `RunAgentInput.parentRunId` 原样带出去，让它把这条 run 挂回父链。
-spec run_started(binary(), binary(), binary() | undefined) -> event().
run_started(ThreadId, RunId, undefined) ->
    new(<<"RUN_STARTED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId});
run_started(ThreadId, RunId, ParentRunId) ->
    new(<<"RUN_STARTED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                             <<"parentRunId">> => ParentRunId}).

%% @doc 一次 run 正常结束
-spec run_finished(binary(), binary()) -> event().
run_finished(ThreadId, RunId) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId}).

%% @doc 一次 run 正常结束（带结果）
-spec run_finished(binary(), binary(), term()) -> event().
run_finished(ThreadId, RunId, Result) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                              <<"result">> => Result}).

%% @doc 一次 run 正常结束（带结果与用量）
%%
%% `usage` 是**数组**：一条 run 里可能换过模型、也可能有子 agent，客户端按数组
%% 求和。空数组不发这一位 —— 发一个全 0 的条目会被当成「真的用了 0 个」求和进去。
-spec run_finished(binary(), binary(), term(), [map()]) -> event().
run_finished(ThreadId, RunId, Result, []) ->
    run_finished(ThreadId, RunId, Result);
run_finished(ThreadId, RunId, Result, Usage) when is_list(Usage) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                              <<"result">> => Result,
                              <<"usage">> => Usage}).

%% @doc 一次 run 被取消
%%
%% AG-UI 没有 cancelled 这个事件类型 —— 按「结束了，但带原因」发：run 必须有终态
%% （否则流关不掉），`result.status` 说清了不是跑完了。
-spec run_cancelled(binary(), binary()) -> event().
run_cancelled(ThreadId, RunId) ->
    run_finished(ThreadId, RunId, #{<<"status">> => <<"cancelled">>}).

%% @doc 一次 run 停在中断上（AG-UI 的 interrupt 协议）
%%
%% **收口与告知是同一条事件**：`RUN_FINISHED` 让流关得掉，
%% `outcome:{type:"interrupt", interrupts:[…]}` 又说清了「不是跑完了，是停在这儿
%% 等人」。客户端（`useInterrupt` / `useHumanInTheLoop`）据此接管渲染，人的决定
%% 走**下一次 run 的 `resume[]`** 送回来 —— 协议里没有单独的审批端点，就这一条。
-spec run_interrupted(binary(), binary(), [map()]) -> event().
run_interrupted(ThreadId, RunId, Interrupts) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                              <<"outcome">> => #{<<"type">> => <<"interrupt">>,
                                                 <<"interrupts">> => Interrupts}}).

%% @doc 一次 run 出错
-spec run_error(binary(), term()) -> event().
run_error(Message, Code) ->
    new(<<"RUN_ERROR">>, #{<<"message">> => Message, <<"code">> => Code}).

%% @doc 一个执行步骤开始
-spec step_started(binary()) -> event().
step_started(StepName) ->
    new(<<"STEP_STARTED">>, #{<<"stepName">> => StepName}).

%% @doc 一个执行步骤结束
-spec step_finished(binary()) -> event().
step_finished(StepName) ->
    new(<<"STEP_FINISHED">>, #{<<"stepName">> => StepName}).

%% @doc 一条 assistant 文本消息开始
-spec text_message_start(binary()) -> event().
text_message_start(MessageId) ->
    new(<<"TEXT_MESSAGE_START">>, #{<<"messageId">> => MessageId,
                                    <<"role">> => <<"assistant">>}).

%% @doc 文本增量
-spec text_message_content(binary(), binary()) -> event().
text_message_content(MessageId, Delta) ->
    new(<<"TEXT_MESSAGE_CONTENT">>, #{<<"messageId">> => MessageId,
                                      <<"delta">> => Delta}).

%% @doc 一条 assistant 文本消息结束
-spec text_message_end(binary()) -> event().
text_message_end(MessageId) ->
    new(<<"TEXT_MESSAGE_END">>, #{<<"messageId">> => MessageId}).

%% @doc 工具调用开始（parentMessageId 指向发起该调用的 assistant 消息）
-spec tool_call_start(binary(), binary(), binary() | undefined) -> event().
tool_call_start(ToolCallId, ToolCallName, undefined) ->
    new(<<"TOOL_CALL_START">>, #{<<"toolCallId">> => ToolCallId,
                                 <<"toolCallName">> => ToolCallName});
tool_call_start(ToolCallId, ToolCallName, ParentMessageId) ->
    new(<<"TOOL_CALL_START">>, #{<<"toolCallId">> => ToolCallId,
                                 <<"toolCallName">> => ToolCallName,
                                 <<"parentMessageId">> => ParentMessageId}).

%% @doc 工具调用参数增量（AG-UI 要求 delta 是 JSON 文本片段，不是 map）
-spec tool_call_args(binary(), binary()) -> event().
tool_call_args(ToolCallId, Delta) ->
    new(<<"TOOL_CALL_ARGS">>, #{<<"toolCallId">> => ToolCallId,
                                <<"delta">> => Delta}).

%% @doc 工具调用参数结束
-spec tool_call_end(binary()) -> event().
tool_call_end(ToolCallId) ->
    new(<<"TOOL_CALL_END">>, #{<<"toolCallId">> => ToolCallId}).

%% @doc 工具执行结果
-spec tool_call_result(binary(), binary(), binary()) -> event().
tool_call_result(MessageId, ToolCallId, Content) ->
    new(<<"TOOL_CALL_RESULT">>, #{<<"messageId">> => MessageId,
                                  <<"toolCallId">> => ToolCallId,
                                  <<"content">> => Content,
                                  <<"role">> => <<"tool">>}).

%% @doc 一段推理开始
-spec reasoning_start(binary()) -> event().
reasoning_start(MessageId) ->
    new(<<"REASONING_START">>, #{<<"messageId">> => MessageId}).

%% @doc 推理消息开始
-spec reasoning_message_start(binary()) -> event().
reasoning_message_start(MessageId) ->
    new(<<"REASONING_MESSAGE_START">>, #{<<"messageId">> => MessageId,
                                         <<"role">> => <<"reasoning">>}).

%% @doc 推理增量
-spec reasoning_message_content(binary(), binary()) -> event().
reasoning_message_content(MessageId, Delta) ->
    new(<<"REASONING_MESSAGE_CONTENT">>, #{<<"messageId">> => MessageId,
                                           <<"delta">> => Delta}).

%% @doc 推理消息结束
-spec reasoning_message_end(binary()) -> event().
reasoning_message_end(MessageId) ->
    new(<<"REASONING_MESSAGE_END">>, #{<<"messageId">> => MessageId}).

%% @doc 一段推理结束
-spec reasoning_end(binary()) -> event().
reasoning_end(MessageId) ->
    new(<<"REASONING_END">>, #{<<"messageId">> => MessageId}).

%% @doc 完整状态快照
-spec state_snapshot(term()) -> event().
state_snapshot(Snapshot) ->
    new(<<"STATE_SNAPSHOT">>, #{<<"snapshot">> => Snapshot}).

%% @doc 状态增量（JSON Patch 数组）
-spec state_delta([map()]) -> event().
state_delta(Delta) ->
    new(<<"STATE_DELTA">>, #{<<"delta">> => Delta}).

%% @doc 消息历史快照
%%
%% AG-UI 里「这是权威的消息列表，照它对齐」。我们在**断线重连补不回缺口**时发它
%% （`beamai_agui_session` 的环形缓冲越界）——客户端丢了中间那几条事件，增量已经
%% 对不上了，给它一份完整的消息列表比让它带着一个洞继续渲染强。
-spec messages_snapshot([map()]) -> event().
messages_snapshot(Messages) ->
    new(<<"MESSAGES_SNAPSHOT">>, #{<<"messages">> => Messages}).

%%--------------------------------------------------------------------
%% activity 消息：不是聊天文本、但要在对话流里占一块
%%--------------------------------------------------------------------

%% @doc activity 快照（建消息）
%%
%% 生成式 UI、进度卡片这类东西。产出方是 **activity 类插件**
%% （`beamai_agui_a2ui` / `beamai_agui_mcp_apps`，以及 examples 里的
%% `example_agui_genui`），
%% 核心路径不发这两条 —— 不装插件前端就看不到任何 activity。
%%
%% `activityType` 是前端挑 renderer 的依据；`replace` 为真表示这条快照**整块换掉**
%% 上一条（A2UI 就靠它，每次发完整的一份，于是根本不需要 delta）。
-spec activity_snapshot(binary(), binary(), term()) -> event().
activity_snapshot(MessageId, ActivityType, Content) ->
    activity_snapshot(MessageId, ActivityType, Content, false).

-spec activity_snapshot(binary(), binary(), term(), boolean()) -> event().
activity_snapshot(MessageId, ActivityType, Content, false) ->
    new(<<"ACTIVITY_SNAPSHOT">>, #{<<"messageId">> => MessageId,
                                   <<"activityType">> => ActivityType,
                                   <<"content">> => Content});
activity_snapshot(MessageId, ActivityType, Content, true) ->
    new(<<"ACTIVITY_SNAPSHOT">>, #{<<"messageId">> => MessageId,
                                   <<"activityType">> => ActivityType,
                                   <<"content">> => Content,
                                   <<"replace">> => true}).

%% @doc activity 增量（JSON Patch）
%%
%% **快照必须排在任何 delta 之前**：前端把 delta 打在 `messageId` 对应的 activity
%% 消息上，消息还没建就**悄悄丢弃** —— 不报错，只是少渲染一块。
-spec activity_delta(binary(), binary(), [map()]) -> event().
activity_delta(MessageId, ActivityType, Patch) ->
    new(<<"ACTIVITY_DELTA">>, #{<<"messageId">> => MessageId,
                                <<"activityType">> => ActivityType,
                                <<"patch">> => Patch}).

%%--------------------------------------------------------------------
%% 子 agent lane
%%--------------------------------------------------------------------

%% @doc 一条子 agent lane 开始
%%
%% **这三个类型是老客户端的雷**：`@ag-ui/client` ≤ 0.0.57 在 HTTP transport 里就拿
%% discriminated union 校验每一条事件，一条未知类型掐断整条流，客户端侧什么都救不
%% 回来。所以产出侧有开关（`{beamai_agui, subagent_events, true}`，**缺省关**）。
%%
%% 相比之下**给既有事件多一个 `subagentRunId` 字段是安全的** —— AG-UI 的事件
%% schema 是 passthrough，老客户端原样忽略。
-spec subagent_started(map()) -> event().
subagent_started(Info) ->
    Fields = #{<<"subagentRunId">> => maps:get(subagent_run_id, Info),
               <<"name">> => maps:get(name, Info, <<"subagent">>)},
    new(<<"SUBAGENT_STARTED">>,
        put_some(Fields, [{<<"description">>, maps:get(task, Info, undefined)},
                          {<<"parentSubagentRunId">>,
                           maps:get(parent_subagent_run_id, Info, undefined)},
                          {<<"parentToolCallId">>,
                           maps:get(parent_tool_call_id, Info, undefined)},
                          {<<"parentMessageId">>,
                           maps:get(parent_message_id, Info, undefined)}])).

%% @doc 一条子 agent lane 收场
%%
%% **挂起不是失败**：AG-UI 给它留了 `outcome.type = "suspended"`，报成
%% `SUBAGENT_ERROR` 前端会画成红条。`interruptIds` 与父那条
%% `RUN_FINISHED.outcome.interrupts[].id` 用同一套 id（都取 pending 工具的
%% tool-call id），客户端才对得上号。
-spec subagent_finished(map()) -> event().
subagent_finished(#{outcome := suspended} = Info) ->
    Outcome0 = #{<<"type">> => <<"suspended">>},
    Outcome = case maps:get(interrupt_ids, Info, []) of
        [] -> Outcome0;
        Ids -> Outcome0#{<<"interruptIds">> => Ids}
    end,
    new(<<"SUBAGENT_FINISHED">>,
        #{<<"subagentRunId">> => maps:get(subagent_run_id, Info),
          <<"outcome">> => Outcome});
subagent_finished(Info) ->
    Fields = #{<<"subagentRunId">> => maps:get(subagent_run_id, Info),
               <<"outcome">> => #{<<"type">> => <<"success">>}},
    new(<<"SUBAGENT_FINISHED">>,
        put_some(Fields, [{<<"result">>, maps:get(result, Info, undefined)}])).

%% @doc 一条子 agent lane 出错
%%
%% AG-UI 没有 killed / timeout 这两种收尾，按 `SUBAGENT_ERROR` 发并把原因写进
%% `code` —— 同 `cancelled → RUN_FINISHED + result.status` 的既有取舍：宁可换个
%% 类型说清楚，也不造协议外的类型。
-spec subagent_error(binary(), binary(), binary()) -> event().
subagent_error(SubagentRunId, Message, Code) ->
    new(<<"SUBAGENT_ERROR">>, #{<<"subagentRunId">> => SubagentRunId,
                                <<"message">> => Message,
                                <<"code">> => Code}).

%%--------------------------------------------------------------------
%% chunk 形式（协议留给「分不出边界的生产者」的便利事件）
%%--------------------------------------------------------------------

%% @doc `TEXT_MESSAGE_CHUNK` —— 客户端自己展开成 START/CONTENT/END 三件套
%%
%% **我们缺省不发它**：边界我们分得出（上游 `on_message_start` / `on_message_end`
%% 恒成对），发显式三件套严格更好。留着构造器是给宿主用的 —— 有些代理层只能转发
%% 不带边界的 token 流，那时这一条才是对的表达。两种**不能混着发同一条消息**，
%% 客户端会把它当成两条。
-spec text_message_chunk(binary() | undefined, binary()) -> event().
text_message_chunk(MessageId, Delta) ->
    text_message_chunk(MessageId, Delta, <<"assistant">>).

-spec text_message_chunk(binary() | undefined, binary(), binary()) -> event().
text_message_chunk(MessageId, Delta, Role) ->
    new(<<"TEXT_MESSAGE_CHUNK">>,
        put_some(#{<<"delta">> => Delta},
                 [{<<"messageId">>, MessageId}, {<<"role">>, Role}])).

%% @doc `TOOL_CALL_CHUNK` —— 同上，工具调用那一族的合并形式
-spec tool_call_chunk(binary() | undefined, binary()) -> event().
tool_call_chunk(ToolCallId, Delta) ->
    tool_call_chunk(ToolCallId, undefined, Delta, undefined).

-spec tool_call_chunk(binary() | undefined, binary() | undefined,
                      binary(), binary() | undefined) -> event().
tool_call_chunk(ToolCallId, ToolCallName, Delta, ParentMessageId) ->
    new(<<"TOOL_CALL_CHUNK">>,
        put_some(#{<<"delta">> => Delta},
                 [{<<"toolCallId">>, ToolCallId},
                  {<<"toolCallName">>, ToolCallName},
                  {<<"parentMessageId">>, ParentMessageId}])).

%% @doc 自定义事件（协议留给宿主的扩展位）
-spec custom(binary(), term()) -> event().
custom(Name, Value) ->
    new(<<"CUSTOM">>, #{<<"name">> => Name, <<"value">> => Value}).

%% @doc 原样透传的 provider 事件
-spec raw(term()) -> event().
raw(Event) ->
    new(<<"RAW">>, #{<<"event">> => Event}).

%% @doc 给一条事件补几个字段（已有的不覆盖）
%%
%% 子 agent lane 的 `subagentRunId` 就是这么打上去的：它对所有事件类型一视同仁，
%% 统一在出口打比逐个构造器写一遍强。**已有的不覆盖** —— `SUBAGENT_STARTED`
%% 自己那份是构造时写死的，别让 lane 的标签把它顶掉。
-spec tag(event(), map()) -> event().
tag(Event, Tags) when is_map(Event), is_map(Tags) ->
    maps:fold(fun(K, V, Acc) ->
        case maps:is_key(K, Acc) of
            true -> Acc;
            false -> Acc#{K => V}
        end
    end, Event, Tags);
tag(Event, _) ->
    Event.

%%====================================================================
%% 构造 - 载荷片段
%%====================================================================

%% @doc 一条 `Interrupt`（`RUN_FINISHED.outcome.interrupts[]` 的元素）
%%
%% `id` 取被中断那次调用的 **tool-call id**：它本来就唯一，且与同一条 interrupt 的
%% `toolCallId` 同源 —— 前端把审批条挂回那张工具卡片不用二次关联，客户端把它原样
%% 放在 `resume[].interruptId` 里送回来，服务端据此认得出「回的是不是当前这一条」。
%%
%% == 两类 interrupt 要的东西不一样 ==
%%
%% | `kind` | 谁触发 | 客户端该回什么 |
%% |---|---|---|
%% | `approval` | 服务端声明的中断工具（有实现，等人点头） | 一个**决定**：`{decision: approved\|rejected}` |
%% | `frontend-tool` | `RunAgentInput.tools` 里的工具（服务端没实现） | 这次调用的**结果**：`{result: …}` |
%%
%% 曾经两类共用审批那张 schema，于是前端工具那条路上客户端照着填了个
%% `{decision: "approved"}` —— 它**填不出结果来**，服务端只好瞎编一条，模型据此
%% 宣布工具跑过了，而客户端从头到尾没被调用过。
%%
%% 三处都说同一件事，客户端认哪个都行：`reason` 的措辞、`responseSchema` 的字段、
%% `metadata.kind`。**`responseSchema` 是最省事的那个** —— 客户端照它填就自然回对，
%% 不用把「这是哪一类」编进自己的 if。
-spec interrupt(map()) -> map().
interrupt(Info) ->
    Kind = maps:get(kind, Info, approval),
    Base = #{<<"id">> => maps:get(id, Info),
             <<"reason">> => reason_of(Kind, Info),
             <<"responseSchema">> => response_schema(Kind)},
    case maps:get(tool_call_id, Info, undefined) of
        undefined ->
            Base;
        ToolCallId ->
            Base#{<<"toolCallId">> => ToolCallId,
                  <<"metadata">> => #{<<"kind">> => kind_name(Kind),
                                      %% 上游那个内部的中断类型（`tool_request` 等），
                                      %% 排障时有用，但不该让客户端拿它做判别
                                      <<"interruptType">> =>
                                          maps:get(reason, Info, <<"paused">>),
                                      <<"pendingTool">> =>
                                          #{<<"name">> => maps:get(tool_name, Info, null),
                                            <<"args">> => maps:get(args, Info, #{})}}}
    end.

kind_name(frontend_tool) -> <<"frontend-tool">>;
kind_name(_) -> <<"approval">>.

%% @private `Interrupt.reason` 是必填字符串，让它自己说清是哪一类
reason_of(frontend_tool, Info) ->
    <<"需要客户端执行并回传结果: "/utf8, (tool_label(Info))/binary>>;
reason_of(_, Info) ->
    <<"需要审批: "/utf8, (tool_label(Info))/binary>>.

tool_label(Info) ->
    case maps:get(tool_name, Info, undefined) of
        Name when is_binary(Name), Name =/= <<>> -> Name;
        _ -> maps:get(reason, Info, <<"paused">>)
    end.

%% @private 这一类该回什么（见 interrupt/1）
%%
%% 审批那张**只声明我们真认的两个决策** —— `approveWithEdits`（改参数再执行）
%% 没实现，不谎报。
response_schema(frontend_tool) ->
    #{<<"type">> => <<"object">>,
      <<"properties">> =>
          #{<<"result">> =>
                #{<<"description">> =>
                      <<"The value this client-side tool returned. Execute the tool, "
                        "then send its result back here.">>}},
      <<"required">> => [<<"result">>]};
response_schema(_) ->
    #{<<"type">> => <<"object">>,
      <<"properties">> => #{<<"decision">> =>
                                #{<<"type">> => <<"string">>,
                                  <<"enum">> => [<<"approved">>, <<"rejected">>]}},
      <<"required">> => [<<"decision">>]}.

%% @doc beamai 的 usage map → AG-UI 的 `usage[]` 条目
%%
%% 键名对着 `@ag-ui/core` 的 `RunFinishedEventSchema.usage`，**全部可选** ——
%% 没有的位就不发：发一个 0 会被客户端当成「真的用了 0 个」求和进去。
-spec usage(map() | undefined) -> [map()].
usage(Usage) when is_map(Usage), map_size(Usage) > 0 ->
    Mapped = lists:foldl(fun({From, To}, Acc) ->
        case maps:get(From, Usage, undefined) of
            undefined -> Acc;
            Value -> Acc#{To => Value}
        end
    end, #{}, [{model, <<"model">>},
               {input_tokens, <<"inputTokens">>},
               {output_tokens, <<"outputTokens">>},
               {total_tokens, <<"totalTokens">>},
               {reasoning_tokens, <<"reasoningTokens">>},
               {cache_read_tokens, <<"cachedInputTokens">>}]),
    case map_size(Mapped) of
        0 -> [];
        _ -> [Mapped]
    end;
usage(_) ->
    [].

%% @private 只把非 undefined 的位写进去
%%
%% AG-UI 的这些字段**全部可选**，没有就不发 —— 发一个 null 与「没有这一位」在
%% 客户端那儿不是一回事（`?? ` 只兜 undefined 的写法到处都是）。
put_some(Fields, Pairs) ->
    lists:foldl(fun({_K, undefined}, Acc) -> Acc;
                   ({K, V}, Acc) -> Acc#{K => V}
                end, Fields, Pairs).

%%====================================================================
%% 断言
%%====================================================================

%% @doc 事件类型
-spec type_of(event()) -> binary() | undefined.
type_of(Event) when is_map(Event) -> maps:get(<<"type">>, Event, undefined);
type_of(_) -> undefined.

%% @doc 是不是终态事件
%%
%% 一条 run 恰好有一个终态事件，且它是最后一条 —— 会话层靠这条断言判断
%% 「run 进程死掉时它自己有没有把话说完」，没说完就替它补一条。
-spec is_terminal(event()) -> boolean().
is_terminal(Event) ->
    lists:member(type_of(Event), [<<"RUN_FINISHED">>, <<"RUN_ERROR">>]).

%%====================================================================
%% 编码
%%====================================================================

%% @doc 事件 → JSON
-spec to_json(event()) -> binary().
to_json(Event) ->
    beamai_utils:encode_json(Event).

%% @doc 事件 → 一个 SSE 帧（无 event 名，见模块头）
-spec to_sse(event()) -> binary().
to_sse(Event) ->
    Json = to_json(Event),
    <<"data: ", Json/binary, "\n\n">>.

%% @doc 事件 → 一个带 `id:` 的 SSE 帧
%%
%% `id:` 放会话级单调序号。浏览器 `EventSource` 断线重连时会把最后收到的 id 放进
%% `Last-Event-ID` 请求头 —— 服务端凭它只补缺口，既不重发也不漏发。这就是"断线
%% 重连不丢输出"落到线上的那一格。
-spec to_sse(event(), non_neg_integer()) -> binary().
to_sse(Event, Seq) ->
    <<"id: ", (integer_to_binary(Seq))/binary, "\n", (to_sse(Event))/binary>>.
