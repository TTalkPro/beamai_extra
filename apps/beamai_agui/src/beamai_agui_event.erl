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
-export([custom/2, raw/1]).

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
-spec messages_snapshot([map()]) -> event().
messages_snapshot(Messages) ->
    new(<<"MESSAGES_SNAPSHOT">>, #{<<"messages">> => Messages}).

%% @doc 自定义事件（协议留给宿主的扩展位）
-spec custom(binary(), term()) -> event().
custom(Name, Value) ->
    new(<<"CUSTOM">>, #{<<"name">> => Name, <<"value">> => Value}).

%% @doc 原样透传的 provider 事件
-spec raw(term()) -> event().
raw(Event) ->
    new(<<"RAW">>, #{<<"event">> => Event}).

%%====================================================================
%% 构造 - 载荷片段
%%====================================================================

%% @doc 一条 `Interrupt`（`RUN_FINISHED.outcome.interrupts[]` 的元素）
%%
%% `id` 取被中断那次调用的 **tool-call id**：它本来就唯一，且与同一条 interrupt 的
%% `toolCallId` 同源 —— 前端把审批条挂回那张工具卡片不用二次关联，客户端把它原样
%% 放在 `resume[].interruptId` 里送回来，服务端据此认得出「回的是不是当前这一条」。
%%
%% `responseSchema` 告诉通用客户端「这条该回什么」，它照着渲染审批控件。
%% **只声明我们真认的两个决策** —— `approveWithEdits`（改参数再执行）没实现，不谎报。
-spec interrupt(map()) -> map().
interrupt(Info) ->
    Base = #{<<"id">> => maps:get(id, Info),
             <<"reason">> => maps:get(reason, Info, <<"paused">>),
             <<"responseSchema">> => response_schema()},
    case maps:get(tool_call_id, Info, undefined) of
        undefined ->
            Base;
        ToolCallId ->
            Base#{<<"toolCallId">> => ToolCallId,
                  <<"metadata">> => #{<<"pendingTool">> =>
                                          #{<<"name">> => maps:get(tool_name, Info, null),
                                            <<"args">> => maps:get(args, Info, #{})}}}
    end.

%% @private 审批控件的形状（见 interrupt/1）
response_schema() ->
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
