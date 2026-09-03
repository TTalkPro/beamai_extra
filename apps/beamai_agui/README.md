# beamai_agui

AG-UI 协议的服务端运行时 —— 把 `beamai_agent` 的一次 run 变成前端可增量渲染的
SSE 事件流。对标 CopilotKit 的 `@copilotkit/runtime`。

## 它在哪一层

```
前端 (CopilotKit / 任意 AG-UI 客户端)
        │  POST /agui/run   ──►  SSE 事件流
        ▼
beamai_agui_cowboy_handler      适配器：写帧、管连接
        │
beamai_agui_http_handler        框架无关：解析、校验、定位 agent
        │
beamai_agui_runner              一次 run 的编排
        │
beamai_agui_emitter             翻译层：agent 回调 → AG-UI 事件
        │
beamai_agent                    ReAct 循环（上游 beamai）
```

与 `beamai_a2a` 是**兄弟不是重复**：A2A 面向 agent↔agent，AG-UI 面向
agent↔前端（逐 token 渲染、工具调用可视化、HITL）。两者共用同一套
「框架无关 handler + cowboy 适配器」骨架。

## 快速开始

```erlang
%% 1. 启动应用（带起 agent 注册表）
{ok, _} = application:ensure_all_started(beamai_agui),

%% 2. 注册一个 agent 工厂
%%    RuntimeCfg（callbacks / conversation_id / pause_store）必须整个 merge 进去
ok = beamai_agui_registry:register(<<"my-agent">>,
    fun(_Input, RuntimeCfg) ->
        K = beamai_chat_client:add_chat_model(
              beamai_chat_client:new(),
              beamai_chat_model:create(anthropic, #{
                  model => <<"glm-4.7">>,
                  api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
                  base_url => <<"https://open.bigmodel.cn/api/anthropic">>})),
        beamai_agent:new(maps:merge(#{chat_client => K}, RuntimeCfg))
    end),

%% 3. 挂上路由
Dispatch = cowboy_router:compile([
    {'_', [
        {"/agui/run",  beamai_agui_cowboy_handler, #{agent => <<"my-agent">>}},
        {"/agui/info", beamai_agui_cowboy_handler, #{mode => info}}
    ]}
]),
{ok, _} = cowboy:start_clear(agui_listener, [{port, 8080}],
                             #{env => #{dispatch => Dispatch}}).
```

## 事件映射

| AG-UI 事件 | 由什么驱动 |
|---|---|
| `RUN_STARTED` / `RUN_FINISHED` / `RUN_ERROR` | `beamai_agui_runner` 在 agent 前后发 |
| `STEP_STARTED` / `STEP_FINISHED` | `on_llm_call` / `on_llm_result`（每轮迭代） |
| `TEXT_MESSAGE_START` | 第一个 token 到达时**惰性**发出 |
| `TEXT_MESSAGE_CONTENT` | `on_token`（`Meta.message_id` 决定归属） |
| `TEXT_MESSAGE_END` | `on_message_end` |
| `TOOL_CALL_START` / `ARGS` / `END` | `on_message_end` 里那条消息的 `tool_calls` |
| `TOOL_CALL_RESULT` | `on_tool_result`（`Info.tool_call_id` 配对） |

`message_id` 与 `tool_call_id` 都来自上游 `beamai_agent` 的回调 —— 没有它们
就没法把 token 归到消息、把结果配回调用（并发工具下触发顺序不定）。

## HITL：一次 run 跨两个请求

agent 停下来等人（`interrupt_tools` 命中、或 `on_tool_call` 返回
`{interrupt, _}`）时：

1. 上游把**中断态快照**按 `conversation_id` 落进 pause store；
2. runtime 发一个 `CUSTOM(interrupt)`（带 `toolCallId` / `toolCallName` / `args`）
   加 `RUN_FINISHED`，这次 HTTP 请求就结束了；
3. 前端此时看到的是一个**没有结果**的 `TOOL_CALL_START/ARGS/END` —— 那就是
   「等你回话」；
4. 下一个请求进来，runtime 发现该 thread 有未决暂停，转入续跑：把人给的答复
   补成那次调用的 `TOOL_CALL_RESULT`，再喂给 `beamai_agent:resume/3`。

三个 id 是对齐的，所以不需要额外的 run 表：

```
AG-UI threadId  ==  beamai conversation_id  ==  pause_store 的键
```

答复有三条来路，按优先级：

| 来路 | 形态 | 语义 |
|---|---|---|
| `forwardedProps.resume` | `#{"decision", "payload"}` | 全语义：`approved` / `rejected` / `reply`，可带 `args` 改参数 |
| `role=tool` 消息 | `toolCallId` 对得上被中断的调用 | AG-UI 原生：前端工具执行完回传结果 |
| 最后一条 `user` 消息 | 纯文本 | 聊天式：agent 问、人直接回一句 |

**存的是纯数据**（中断态、被中断的 tool_call、已完成的结果、迭代计数），
不存 ChatClient / tools / callbacks —— 那些由工厂在下个请求里重建。所以
**工厂必须可重放**：同一个 thread 的两次请求要造出配置一致的 agent。

内置 store 是 ETS（`beamai_agui_sup` 起的），**节点重启即丢**。跨重启续跑要：

- `{beamai_agui, pause_store, {Module, Ref}}` 配一个持久实现（实现
  `beamai_pause_store` 的三个回调即可），并且
- 把 chat memory 也换成持久 store。

两者缺一，恢复出来的会话都是残的。store 不可用时整个机制静默退化成
「这个节点没有 HITL 持久化」，其余功能照常 —— 不会让普通请求跟着崩。

## 骨架尚未覆盖的

按重要性排：

1. **前端工具（frontend tools）**。`RunAgentInput.tools` 现在被解析出来但没有
   注册进 agent —— 要支持前端定义、前端执行的工具，得把它们注册成
   `interrupt_tools`，然后走上面的 HITL 路径。管道已经通了，缺的是注册这一步。
2. **`TOOL_CALL_ARGS` 的逐字增量**。现在参数是在 `on_message_end` 时一次性
   发完整 JSON —— 语义正确，但前端看不到工具参数的打字机效果。真增量在
   `on_llm_event` 的 provider 原始 chunk 里，接法见
   `beamai_agui_emitter` 的 `raw_translator` 配置项。
3. **`REASONING_*`**。同上，数据在 `on_llm_event` 里，需按 provider 分支解读。
4. **`STATE_SNAPSHOT` / `STATE_DELTA`**。`beamai_context` 有 `state_slots`
   与工具 `writes`，从中 diff 即可，不需要改上游。
5. **认证与限流**。`beamai_a2a_auth` / `beamai_a2a_rate_limit` 是现成的样板。

### 续跑的两条 TOOL_CALL_RESULT 来路

| 决策 | 结果从哪来 |
|---|---|
| `reply` / 拒绝 / 自由文本 | 人给的，runtime 在 `emit_resume_result` 里补上 |
| `approved` | 服务端真执行那个工具，走 `on_tool_result` → emitter |

两条互斥，同一次调用只会有一条结果。
