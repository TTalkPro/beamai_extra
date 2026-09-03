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
-export([run_started/2, run_finished/2, run_finished/3, run_error/2]).
-export([step_started/1, step_finished/1]).
-export([text_message_start/1, text_message_content/2, text_message_end/1]).
-export([tool_call_start/3, tool_call_args/2, tool_call_end/1, tool_call_result/3]).
-export([reasoning_start/1, reasoning_message_start/1,
         reasoning_message_content/2, reasoning_message_end/1, reasoning_end/1]).
-export([state_snapshot/1, state_delta/1, messages_snapshot/1]).
-export([custom/2, raw/1]).

%% 编码
-export([to_json/1, to_sse/1]).

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
    new(<<"RUN_STARTED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId}).

%% @doc 一次 run 正常结束
-spec run_finished(binary(), binary()) -> event().
run_finished(ThreadId, RunId) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId}).

%% @doc 一次 run 正常结束（带结果）
-spec run_finished(binary(), binary(), term()) -> event().
run_finished(ThreadId, RunId, Result) ->
    new(<<"RUN_FINISHED">>, #{<<"threadId">> => ThreadId, <<"runId">> => RunId,
                              <<"result">> => Result}).

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
