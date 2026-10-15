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
beamai_agui_http_handler        框架无关：解析、校验、定位 agent、只读面
        │
beamai_agui_session             会话：run 生命周期 + 事件环形缓冲 + 订阅扇出
        │                       （run 挂在这儿，**不挂在 HTTP 连接上**）
beamai_agui_runner              一次 run 的编排
        │
beamai_agui_emitter             翻译层：agent 回调 → AG-UI 事件
        │
beamai_agent                    ReAct 循环（上游 beamai）
```

与 `beamai_a2a` 是**兄弟不是重复**：A2A 面向 agent↔agent，AG-UI 面向
agent↔前端（逐 token 渲染、工具调用可视化、HITL）。两者共用同一套
「框架无关 handler + cowboy 适配器」骨架。

## 会话层：run 的寿命不再由 HTTP 请求决定

没有这一层时，run 在请求进程下 spawn，**连接一断它跟着死** —— 浏览器刷新、切网、
锁屏，这一轮的输出就没了，只能取消重来；而另一个请求也看不见它，想停一个正在跑的
run，手上只有 threadId 却没有那个进程的 pid。

`beamai_agui_session`（一个 thread 一个 gen_server）把 run 挪到请求之外：

| 能力 | 怎么来的 | 线上入口 |
|---|---|---|
| 断线重连不丢输出 | 事件进会话的**有界环形缓冲**，重连带水位只补缺口 | `GET /agui/threads/:id/stream`（`Last-Event-ID`） |
| 跨请求 stop | run 进程挂在会话下 | `POST /agui/threads/:id/stop` |
| 一会话一 run | 起 run 走会话这一个入口 | `reject`（缺省，第二个请求 409）或 `supersede` |
| 只读面 | 缓冲 + 会话状态，**没有第二套存储** | `GET /agui/threads[/:id/{events,state}]` |

### seq：会话级单调，跨 run 连续

每条事件在会话里取一个号，写进 SSE 帧的 `id:` 行（**不进事件 JSON** —— AG-UI 的
BaseEvent 没这一位）。浏览器 `EventSource` 断线重连时把它放进 `Last-Event-ID` 请求
头，服务端凭它只补缺口，前端一行代码都不用写。

跨 run 连续是刻意的：一个 thread 上第二轮对话的号不该重置回 0，否则重连时
「补 3 号之后的」会把上一轮的事件也补一遍。

缓冲有界（缺省 512 条，`{beamai_agui, buffer_size, N}`）。水位早于缓冲起点时先发
一条 `CUSTOM(resync)` 再接 live tail —— 中间那段**永远补不回来了**，如实说比假装流
是连续的好：少几条事件的 UI 会静静地渲染错。

### stop 是真停

BEAM 有抢占，`stop` 就是把 run 进程杀掉 —— 不需要 JVM 那套「协作式取消 + 正在跑的
工具会跑完」的妥协。代价是被杀的工具停在半路，它已经产生的副作用仍然算数。

停稳的信号是**终态事件**（`RUN_FINISHED` + `result.status = "cancelled"`），不是
stop 接口的返回值。会话保证「一条 run 恰好一个终态事件且它是最后一条」——
run 崩了、被杀了也一样（会话替它补一条），否则客户端的流永远关不掉。

### 路由

```erlang
Dispatch = cowboy_router:compile([
    {'_', [
        {"/agui/run",                  beamai_agui_cowboy_handler, #{agent => <<"my-agent">>}},
        %% **按 agent 寻址** —— `/info` 报的是一张 agents 字典，标准 AG-UI 客户端
        %% 据此拼 `{base}/agent/{id}/run`；这条不挂，它们连不上
        {"/agui/agent/:agent/run",     beamai_agui_cowboy_handler, #{}},
        {"/agui/info",                 beamai_agui_cowboy_handler, #{mode => info}},
        {"/agui/threads",              beamai_agui_cowboy_handler, #{mode => threads}},
        {"/agui/threads/:thread",      beamai_agui_cowboy_handler, #{mode => thread}},
        {"/agui/threads/:thread/:sub", beamai_agui_cowboy_handler, #{mode => thread}},
        %% **兜底放最后**：没匹配上的回一个**带 CORS 头**的 404
        {"/agui/[...]",                beamai_agui_cowboy_handler, #{mode => fallback}}
    ]}
]).
```

`:sub` 认五个：`events` / `messages` / `state` / `stop` / `stream`。

**`default` 这个 id 认**：客户端在用户还没从 `/info` 那张字典里挑之前（或者压根
没读 `/info`），拼出来的常常是 `{base}/agent/default/run`。注册表里没有叫
`default` 的时候，它落到端点钉死的那个 agent 上，否则落到注册表里**唯一**那个；
两个以上就 404 —— 那时 `default` 指谁只能靠猜。**别的名字一律不回落**：
`/agent/my-agnet/run` 这种手抖要是也被喂给唯一那个 agent，就成了「能跑但跑的不是
你以为那个」，比 404 难查。

404 的响应体把候选摊成字段，客户端换个 id 再来一次即可：

```json
{"error":"agent_not_found","requested":"nope",
 "available":["my-agent"],
 "detail":"no agent registered as 'nope'; available: my-agent"}
```

⚠️ **兜底那条值得挂**：cowboy 自己那条「没有路由匹配」的 404 走不到 handler，
响应上就没有 CORS 头 —— 浏览器会把「没这条路」显示成
`No 'Access-Control-Allow-Origin' header is present`，拿到报文的人第一反应是去查
CORS，而 CORS 是好的。挂上它，走错路的情况就能自己说清（响应体里还带一句
`hint` 指向 `/agui/info`）。

### 配置

| 键 | 缺省 | 说明 |
|---|---|---|
| `{beamai_agui, buffer_size, N}` | 512 | 会话事件环形缓冲条数 |
| `{beamai_agui, idle_ttl, Ms}` | 30 分钟 | 空闲会话驱逐（有 run / 有订阅者 / 停在等人的不驱逐） |
| `{beamai_agui, on_concurrent, reject \| supersede}` | `reject` | 已有 in-flight run 时第二个请求怎么办 |
| `{beamai_agui, supersede_wait, Ms}` | 5000 | supersede 等旧 run 收尾的上限 |

**会话是进程内的东西**，节点一重启就没了。跨重启活下来的是中断态快照
（pause_store）与对话历史（chat memory）—— 见下面两节。

会话层没起来（宿主只引了几个模块、没启 `beamai_agui` 应用）时，cowboy 适配器
**退化成 run 绑在请求上的老行为**：少一层能力，不是坏掉。

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

## 协议覆盖：29 个事件类型

AG-UI 的**非弃用**事件类型全部实现（`beamai_agui_event` 里各有一个构造器）：

| 族 | 类型 | 谁发 |
|---|---|---|
| run | `RUN_STARTED` / `RUN_FINISHED` / `RUN_ERROR` | `beamai_agui_runner` + 会话层补终态 |
| step | `STEP_STARTED` / `STEP_FINISHED` | 每轮 LLM 调用 |
| 文本 | `TEXT_MESSAGE_START` / `CONTENT` / `END` | `on_token` + 消息边界回调 |
| 推理 | `REASONING_START` / `END`、`REASONING_MESSAGE_START` / `CONTENT` / `END` | provider 的 thinking / reasoning 增量 |
| 工具 | `TOOL_CALL_START` / `ARGS` / `END` / `RESULT` | 消息里的 tool_calls + `on_tool_result` |
| 状态 | `STATE_SNAPSHOT` / `STATE_DELTA` | run 开头立基线 + `on_state_change` |
| 消息 | `MESSAGES_SNAPSHOT` | 断线重连补不回缺口时（见会话层） |
| activity | `ACTIVITY_SNAPSHOT` / `ACTIVITY_DELTA` | **插件**（A2UI / 生成式 UI / MCP Apps） |
| 子 agent | `SUBAGENT_STARTED` / `FINISHED` / `ERROR` | `beamai_agui_subagent`（缺省关） |
| 扩展 | `CUSTOM` / `RAW` | 宿主 |
| chunk | `TEXT_MESSAGE_CHUNK` / `TOOL_CALL_CHUNK` | **缺省不发**，见下 |

**`THINKING_*` 一族刻意不做**：AG-UI 0.0.59 起全部 deprecated、1.0 移除，思考走
`REASONING_*`（前端有专门的折叠面板渲染它）。做一个要被移除的东西，只会让将来
多一次清理。

**`*_CHUNK` 两条缺省不发**：它们是协议留给「分不出消息边界的生产者」的便利事件，
客户端收到后自己展开成 START/CONTENT/END 三件套。我们的边界分得出（上游
`on_message_start` / `on_message_end` 恒成对），发显式三件套严格更好。构造器留着
是给宿主用的 —— 有些代理层只能转发不带边界的 token 流。两种**不能混着发同一条
消息**，客户端会把它当成两条。

## 事件映射

| AG-UI 事件 | 由什么驱动 |
|---|---|
| `RUN_STARTED` / `RUN_FINISHED` / `RUN_ERROR` | `beamai_agui_runner` 在 agent 前后发 |
| `STEP_STARTED` | `on_llm_call`（每轮迭代） |
| `STEP_FINISHED` | `on_llm_result` **记账**，等这一步的消息与工具都闭合了才发 —— **步骤是容器，容器最后关** |
| `TEXT_MESSAGE_START` | 第一个 token 到达时**惰性**发出 |
| `TEXT_MESSAGE_CONTENT` | `on_token`（`Meta.message_id` 决定归属） |
| `TEXT_MESSAGE_END` | `on_message_end` |
| `TOOL_CALL_START` / `ARGS` / `END` | `on_message_end` 里那条消息的 `tool_calls` |
| `TOOL_CALL_RESULT` | `on_tool_result`（`Info.tool_call_id` 配对） |
| `STATE_SNAPSHOT` | 每次 run 开头，把请求带来的 `state` 作为基线发回 |
| `STATE_DELTA` | `on_state_change`，与上一份算 JSON Patch |
| `RUN_FINISHED.outcome` | 暂停时带 `{type:"interrupt", interrupts:[…]}`（见下） |
| `RUN_FINISHED.usage` | run 结果里的 token 用量（**数组**，没有就不发这一位） |
| `RUN_STARTED.parentRunId` | 请求带的 `parentRunId`，**原样带着走**，我们不解释它 |

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

答复有四条来路，按优先级：

| 来路 | 形态 | 语义 |
|---|---|---|
| `resume[]` | `#{"interruptId", "status", "payload"}` | **AG-UI interrupt 协议的正路**（见下） |
| `forwardedProps.resume` | `#{"decision", "payload"}` | 我们先于协议实现的扩展位，老前端还在用 |
| `role=tool` 消息 | `toolCallId` 对得上被中断的调用 | 前端工具执行完回传结果 |
| 最后一条 `user` 消息 | 纯文本 | 聊天式：agent 问、人直接回一句 |

### 两类 interrupt：审批 vs 前端工具

它们要的东西**不一样**，所以 schema 也不一样：

| | 审批 | 前端工具 |
|---|---|---|
| 谁触发 | 工厂声明的中断工具（有实现，等人点头） | `RunAgentInput.tools` 里的（服务端没实现） |
| 客户端该回 | 一个**决定** | 这次调用的**结果** |
| `responseSchema.required` | `["decision"]` | `["result"]` |
| `metadata.kind` | `"approval"` | `"frontend-tool"` |
| `reason` | `需要审批: X` | `需要客户端执行并回传结果: X` |

判据只有一条：被中断的那个工具在不在 `RunAgentInput.tools` 里。客户端每次请求都
重发 `tools`，所以续跑那一轮照样算得出来。

**前端工具没有「批准」这回事** —— 服务端没有实现，批准了也没人去执行。它的答复
是结果，三条来路：`payload.result`（我们在 schema 里要的那个）、`role=tool` 消息
（AG-UI 的原生路径）、或人直接打的一句话。**一条都没有时不编造成功**，而是如实
发一条「客户端没有回传结果」的工具结果 —— 让模型知道这一步没拿到东西，比让它
宣布「干完了」强。

### `status` 不是 `decision`

`resume[]` 条目里的 `status` 说的是「这条 interrupt 答完了」（`resolved` /
`completed`），**不是**人做了什么决定。决定在 `payload.decision` 里。

取值顺序：`payload.decision` → 条目的 `decision` → `status`**且仅在它字面上就是
一个决策词时**（有客户端确实发 `[{status:"approved"}]`）。认不出的词**不往下传** ——
上游把不认识的 binary 当「自由文本作结果」，那对聊天式 HITL 是对的，但对一个协议
字段就是把 `"resolved"` 当成了工具的返回值。走自由文本那条路必须是**人真的打了
一句话**（最后一条 user 消息）。

### 走 AG-UI 的 interrupt 协议

暂停编码成 `RUN_FINISHED` + `outcome:{type:"interrupt", interrupts:[…]}` ——
**收口与告知是同一条事件**：`RUN_FINISHED` 让流关得掉（run 必须有终态），
`outcome` 又说清了「不是跑完了，是停在这儿等人」。通用客户端
（`useInterrupt` / `useHumanInTheLoop`）认的是这一条。

`interrupt.id` 取被中断调用的 **tool-call id** —— 它本来就唯一，且与同一条
interrupt 的 `toolCallId` 同源，前端把审批条挂回那张工具卡片不用二次关联。客户端把
它原样放在**下一次 run 的 `resume[]`** 里送回来（协议里没有单独的审批端点），
服务端据此认得出「回的是不是当前这一条」—— **id 对不上就不认**：拿别的 interrupt
的答复去恢复这一个，是把人的意思安到别处。

`responseSchema` 告诉通用客户端该渲染什么控件，只声明 `approved` / `rejected`
两个决策 —— `approveWithEdits`（改参数再执行）没实现，不谎报。

曾经这里还并发一条 `CUSTOM(interrupt)` 作为「我们先于协议实现的那份」。**已经
去掉**：同一件事发两遍，接入方看日志会以为是两件事，而且两份总有一天会不同步。

**存的是纯数据**（中断态、被中断的 tool_call、已完成的结果、迭代计数），
不存 ChatClient / tools / callbacks —— 那些由工厂在下个请求里重建。所以
**工厂必须可重放**：同一个 thread 的两次请求要造出配置一致的 agent。

### 暂停存到哪儿

三选一，按优先级：

| 配置 | 后端 | 跨节点重启 |
|---|---|---|
| `{beamai_agui, pause_store, {Module, Ref}}` | 宿主自带（自己启、自己管） | 看实现 |
| `{beamai_agui, pause_store_file, Path}` | 内置 DETS | ✅ |
| 都不配 | 内置 ETS（缺省） | ❌ |

前两种由 `beamai_agui_sup` 起（`pause_store_name` 可改注册名），第一种它什么都不起。
缺省是 ETS：开发期够用，也不在磁盘上留文件。

生产要跨重启续跑，光配 `pause_store_file` **还不够** —— pause_store 只存
"停在哪儿"，**不存对话历史**。工厂里的 chat memory 也得换成持久的：

```erlang
%% sys.config
{beamai_agui, [{pause_store_file, "/var/lib/agui/pauses.dets"}]}

%% 工厂里
{ok, _} = beamai_chat_memory_dets:start_link(agui_history,
                                             #{file => "/var/lib/agui/history.dets"}),
beamai_agent:new(maps:merge(
    #{chat_client => K, memory => beamai_chat_memory_dets:handle(agui_history)},
    RuntimeCfg))
```

两者缺一，恢复出来的会话都是残的：只有中断态没有历史，模型看不到之前说过什么。

跑一遍看效果（**两个独立的 BEAM 进程**，不是同一个 VM 里 stop 再 start）：

```
cd examples && make compile
D=/tmp/agui_durable_demo
E="../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts"
P="_build/default/lib/beamai_examples/ebin"
ERL_LIBS=$E erl -noshell -pa $P -eval "example_agui_durable:phase1(\"$D\")"
ERL_LIBS=$E erl -noshell -pa $P -eval "example_agui_durable:phase2(\"$D\")"
```

phase1 告诉 agent「我叫小李，项目代号 Falcon」然后进程退出；phase2 是全新的
BEAM，只靠磁盘上那两个文件把会话接回来，再问它刚才说了什么。

DETS 后端另有 `beamai_pause_store_dets:conversations/1` 与 `prune/2`（按
`paused_at` 清理陈旧暂停）。持久化带来的新问题：被放弃的暂停不会自己消失
—— 用户开了个话题、agent 停下来问、人再也没回来，那条快照会一直躺在磁盘上。

store 不可用时整个机制静默退化成「这个节点没有 HITL 持久化」，其余功能照常 ——
不会让普通请求跟着崩。

## 前端工具（frontend tools）

`RunAgentInput.tools` 是**前端定义、前端执行**的工具。runtime 把它们挂成 agent 的
`interrupt_tools`，于是模型看得见（否则永远不会调），调到就暂停把球交出去 ——
走的是与人工审批**完全相同**的管道：

```
模型调用 → 暂停 → CUSTOM(interrupt) + 没有结果的 TOOL_CALL_*
        → 前端执行 → 带 role=tool 消息回来 → 续跑
```

由 runtime 挂而不是让工厂挂：工具是**每个请求**带上来的，工厂却应当是可重放的
常量配置。与工厂自己声明的服务端中断工具**共存**，重名时服务端那个赢
（它有真实现，前端那个只是个声明）。没有 `name` 的条目直接丢 —— 它会变成一个
name 为空的 tool spec，把整个 LLM 请求带坏。

## 增量事件（`TOOL_CALL_ARGS` / `REASONING_*`）

`beamai_agui_raw` 把 provider 原始 chunk 翻成增量事件，runner 缺省接上：

| 形状 | 覆盖的 provider | 取到什么 |
|---|---|---|
| OpenAI 系 | openai / deepseek / zhipu / moonshot / xai / openrouter / siliconflow … | `delta.tool_calls[].function.arguments` 片段、`reasoning_content` |
| Anthropic 系 | anthropic，以及走 anthropic 口的 ZHIPU / MiniMax | `input_json_delta.partial_json`、`thinking_delta.thinking` |

认不出的 chunk **安静丢弃** —— 这条通道是旁路增强，多认一种形状是加分，认不出
不该让主流程有任何感知。

增量路与 `on_message_end` 会争同一个 `TOOL_CALL_START`，故 emitter 记着「哪些 id
已经流过」，对它们只补一个 `TOOL_CALL_END`；没流过的（非流式、或 provider 形状
认不出）仍走一次性发完整 JSON 的回落。

## 共享状态

state 是**前端与 agent 共有**的一份数据：

```
请求带 state ──► runtime 种进 agent（initial_state）
                        │
                   工具经 writes 改
                        │
              STATE_DELTA（JSON Patch）──► 前端打补丁
```

每次 run 开头先发一条 `STATE_SNAPSHOT` **立基线** —— 补丁是相对某一份具体的
state 算的，没有共同基线就对不上。

差分由 `beamai_agui_state` 算，只产 `add` / `remove` / `replace` 三种 op
（RFC 6902 的 `move` / `copy` 对状态槽没意义，`test` 是断言）。只对 map 递归，
数组和标量整体替换 —— 数组的元素级 diff 要一套 LCS 才不会算出一堆无意义的移位
补丁，收益不抵复杂度。路径按 RFC 6901 转义（`~` → `~0`，`/` → `~1`），键里含
斜杠完全可能，不转义会算出一条指向别处的路径。

**续跑不采纳前端的 state**：暂停快照里带着服务端累积的那份，`resume` 从那里恢复。
一个停在半路的 turn，其状态的权威在服务端。

> **注意**：state 是给**工具**读的（`beamai_context:state_get/3`），模型看不到它。
> 要让模型能就当前状态推理（比如"现在共有几项"），得由工厂把 state 渲染进
> system prompt —— 实机测试里模型正是因为看不见基线里已有的那项而数错了数。

## CORS

AG-UI 前端通常与 runtime 不同源，所以每个响应都带 CORS 头。两种模式：

| `cors_origins` | `access-control-allow-origin` | 能带凭据吗 |
|---|---|---|
| `any`（缺省） | `*` | **不能** |
| `[<<"http://localhost:3002">>, …]` | 命中就**原样回显** Origin | 能（`allow-credentials: true`） |

**为什么要给列表**：浏览器不允许 `*` 与凭据并存。前端一旦
`credentials: 'include'`（或 `EventSource` 带 `withCredentials`），请求当场被浏览器
拦掉 —— 而且错误只出现在浏览器控制台，服务端日志里干干净净，这类问题最难查。

没命中就**不发 `access-control-allow-origin`**，让浏览器如实拦掉，而不是回个 `*`
假装放行（那会在带凭据时变成一个更难查的错）。给列表时必发 `vary: origin` ——
响应随 `Origin` 而变，不告诉缓存这件事，CDN 会把 A 源的响应喂给 B 源。

⚠️ `http://localhost:3002` 与 `http://127.0.0.1:3002` 是**两个不同的源**，
浏览器不认为它们相同 —— 要用哪个就写哪个（或者两个都写）。

```erlang
%% 整台服务一个口径
{beamai_agui, [{cors_origins, [<<"http://localhost:3002">>,
                               <<"http://localhost:3100">>]}]}

%% 或按端点（优先于应用环境）
{"/agui/run", beamai_agui_cowboy_handler,
 #{agent => <<"my-agent">>,
   cors_origins => [<<"http://localhost:3002">>, <<"http://localhost:3100">>]}}
```

另有三个位是**协议要求的、不能省**：`access-control-allow-headers` 里的
`last-event-id`（断线重连靠它把水位带回来）、`access-control-expose-headers` 里的
`x-agui-run-id`（跨源前端读得到 run id 才停得掉那条 run）、以及
`access-control-max-age: 600`（不缓存预检的话，每个 POST 前面都多一次 OPTIONS 往返）。

**CORS 的最后一道由适配器盖**：允许哪个源取决于这次请求的 `Origin` 头，而
`beamai_agui_http_handler` 是框架无关的，拿不到它 —— 所以它给缺省那一份，
`beamai_agui_cowboy_handler` 按 `cors_headers/1` 重算一遍覆盖上去（所有回复点都
经过同一个 `headers_map/2`，不会漏）。

## 前端上下文（`RunAgentInput.context`）

`context` 是前端每轮重发的**页面状态**（`useAgentContext`）：当前选中了哪一行、
用户是谁、页面在哪一屏。runtime 把它渲染成几行**追加到 system prompt 后面** ——
追加不覆盖，工厂那份人设是常量配置，不该被一次请求改掉。

抬头那句「以下是前端在本轮提供的上下文（AG-UI context，非用户输入）」是给模型的
出处说明：不写的话这几行会被当成用户指令，而它们其实是页面状态。

它与 state 的分工：**state 是给工具读的**（`beamai_context:state_get/3`，缺省对模型
不可见），**context 是说给模型听的**。确实要让模型推理当前 state 时，打开端点上的
`inject_state => true`，runtime 会把整份 state 渲染成 system 文本追加在 context 后面；
空 state / 非 map 不注入。默认关闭 —— 这等于把整份 UI state 塞进每次 LLM 请求，
token 成本和暴露面必须由宿主显式承担。

```erlang
%% 只给会暴露这仨键的端点开；不要全局无差别打开
{"/agui/run", beamai_agui_cowboy_handler,
 #{agent => <<"my-agent">>, inject_state => true}}
```

续跑注入的是**暂停快照里的 `saved_state`**，不是 resume 请求重新带来的 `state`：
服务端那份才是停在一半的 turn 的权威状态。渲染器是
`beamai_agui_types:state_prompt/1`，抬头明确「共享状态，非用户输入」，降低 prompt
 injection 的风险面。

## 事件流插件（`event_transform`）

`beamai_agui_runner:run/4` 收一个 `event_transform`：可以改写一条事件、吞掉它
（返回 `drop`）、或在它前后插事件（返回列表）。A2UI / Open Generative UI 那类东西
住在这个位置 —— 它们把工具调用翻成活动事件，而 runner / emitter 一个字都不用改。

插件自己抛异常不会把 run 带走：它是旁路增强，出事就退回原事件。

## 多模态输入

客户端可能把 `content` 发成一个部件数组。runtime 把其中的文本部件拼起来往下走，
非文本部件跳过 —— 骨架阶段不往下传图片，**翻不动就别假装翻得动**。不做这一步的话
下游拿到的是一坨结构体，内容全丢且不报错。

## 可选插件：三种生成式 UI

三个插件都住在 `event_transform` 这个通用挂点上 —— 不装就完全不存在，
`/info` 也如实报 false。装配方式一样：往 agent 配置里加工具，往端点配置里加
transform，再把 `/info` 的位报上。

```erlang
%% 工厂里（给模型看得见的工具 + 用法提示词）
beamai_agent:new(maps:merge(beamai_agui_a2ui:with_tool(#{chat_client => K}), RuntimeCfg))

%% 路由里
{"/agui/run", beamai_agui_cowboy_handler,
 #{agent => <<"my-agent">>,
   event_transform => beamai_agui_a2ui:event_transform(),
   input_transform => beamai_agui_a2ui:input_transform(),
   a2ui => true}}                                  %% ← /info 的能力位
```

装两个以上用 `beamai_agui_plugin:compose([A, B])` 串起来。

### `beamai_agui_a2ui` —— A2UI（声明式）

模型生成的是**组件树**，只能用 catalog 里有的组件；前端按自己的组件库渲染。
一条 `ACTIVITY_SNAPSHOT`（`replace: true`，所以不需要 delta），`content` 里装三条
op：`createSurface` → `updateComponents` → `updateDataModel`。**顺序是协议要求的**，
少一条或反了，前端要么空白要么报「surface 不存在」。

缺省 catalog 是 A2UI v0.9 基础 catalog 的 18 个组件。**换成你前端真正注册的那份**
（`with_tool/2` 的 `catalog`）—— catalog 是模型的词汇表，报了前端没有的组件，渲染
就是空白。

用户点了生成出来的按钮 → `forwardedProps.a2uiAction` → `input_transform/0` 把它
合成一条 user 消息接回本轮（走的是与普通输入完全相同的路，没有新增机制）。

### `beamai_agui_genui` —— Open Generative UI（沙箱）

模型生成一整块 HTML/CSS/JS，前端在沙箱 iframe 里跑。一条 `ACTIVITY_SNAPSHOT`
（`generating: true`）加一串 `ACTIVITY_DELTA`（JSON Patch）。三条**猜错了前端不
报错、只会静默少渲染**的细节：

1. **snapshot 必须在任何 delta 之前**，而且只一条 —— 消息还没建，delta 会被悄悄
   丢弃；
2. **`add` 必须带 `value`** —— 模型经常给 `"jsFunctions": null`，一条没有 value 的
   patch 会让前端把**整批**判非法丢掉。所以 null 整条跳过，只留 `…Complete` 标记；
3. **`html` 是数组不是字符串** —— `/html` 先建空数组，之后 `/html/-` 追加。

工具卡片被**扣住**直到第一条 activity 发出去，否则前端先看见卡片再看见 UI，闪一下。

### `beamai_agui_mcp_apps` —— MCP Apps（前端那半边）

协议栈在 `beamai_mcp`；这里只剩**前端约定**：带 `_meta."ui/resourceUri"` 的工具出
结果时补一条 `mcp-apps` activity 快照（带 `resourceUri` / `serverId` / `toolInput`
/ `result`），以及那块 iframe 发起调用时的**代理白名单**（`proxy_request/2`，只放行
`tools/call` / `resources/read` / `notifications/message` / `ping` —— 界面跑在浏览器
里，不能让它随便调 server 的任何东西）。

哪些工具带 UI 在**装配期**算一次（`app_tools/1`），不每轮再问 server。

### 一处与上游的共同差异

三个插件都在 `TOOL_CALL_END` 才翻译（攒齐参数片段再一次解析），而不是边流边贴。
事件的**形状与顺序**与上游逐条一致，少的是中间态。理由在 `beamai_agui_plugin`
模块头。

## 子 agent lane

没有这一层时，一次委派在前端看起来就是**一个跑了很久的工具调用**。开了之后
（`{beamai_agui, subagent_events, true}`）委派出去那一段有自己的 lane：

```erlang
%% 工厂里 —— observer 由 runtime 交下来，关着时它是 undefined
fun(_Input, RuntimeCfg) ->
    Delegate = beamai_agui_subagent:delegate_tool(
                 #{name => <<"research">>, subagent => fun(_A, _C) -> SubConfig end},
                 maps:get(subagent_observer, RuntimeCfg, undefined)),
    ...
end
```

`SUBAGENT_STARTED` → lane 里子 agent 自己的事件（**每条都带 `subagentRunId`**）
→ `SUBAGENT_FINISHED` / `SUBAGENT_ERROR`。

**没有改上游**：`beamai_agent_delegate` 的 `subagent` 是个返回 agent 配置的函数，
而配置里可以有 `callbacks` —— 包一层就够了。`fanout_tool` 同样成立（每个子任务一条
lane）。

**缺省关不是保守**：`@ag-ui/client` ≤ 0.0.57 在 HTTP transport 里拿 discriminated
union 校验每一条事件，一条 `SUBAGENT_STARTED` 掐断整条流，客户端侧救不回来。相比
之下给既有事件多一个 `subagentRunId` 字段是安全的（事件 schema 是 passthrough），
所以只有三个新类型需要开关，字段不需要。

两条边界：子 agent 走 `beamai_agent:run/2`（非流式），所以 lane 里的文本是**整条**
发的；`management_tools` 那条异步委派**不支持**（子 agent 在工具进程死后还在跑，
那时 lane 的状态表已经回收了）。

## 全能力走查（离线可跑）

`examples/src/example_agui_full.erl` 把上面每一样都跑一遍并下断言，**不需要
API key**（LLM 由 `example_agui_scripted` 照剧本演）：

```
cd examples && make compile
ERL_LIBS=../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts rebar3 shell
1> example_agui_full:run().           %% 九个场景
2> example_agui_full:run(reconnect).  %% 只跑一个
```

九个场景：`basic`（帧带 `id:`、逐字 `TOOL_CALL_ARGS`、`REASONING_*`）、
`reconnect`（**真断开**再凭水位接回来）、`stop`（另一个请求停掉在跑的 run）、
`threads`（`/info` 能力位 + 只读面）、`hitl`（`outcome:"interrupt"` → `resume[]`）、
`a2ui` / `genui` / `mcp_apps`（三种 activity）、`subagent`（lane）。

同样九个场景可以换成**真模型**跑：

```
1> example_agui_full:live().             %% MiniMax m2.7（需要 MINIMAX_API_KEY）
2> example_agui_full:live(a2ui).         %% 只跑一个
3> example_agui_full:live(zhipu, genui). %% 换一家
```

**离线与实机验的不是同一件事**。离线验协议的形状（谁先谁后、成不成对、带不带
id），与模型说什么无关，所以断言全是硬的；实机验的是假设成不成立（真 provider 的
chunk 形状认得出吗、模型看得懂插件的提示词吗、它真会去调那个工具吗），这一半
**取决于模型这一轮的选择** —— 所以那类断言在实机模式下降级成 `[skip]`：模型没调
工具是它的选择，不是我们的 bug；协议形状那一半照样是硬断言。

另有 `example_agui_live`（共享状态的闭环）与 `example_agui_durable`（跨 BEAM 进程的
会话恢复）。

### 实机跑出来的三个 bug（离线全绿也照不出来）

1. **插件的工具压根没挂上**。`beamai_agent:new/1` 没有 `tools` 这一位（只有
   `interrupt_tools`），工具挂在 chat client 上。`with_tool` 原来写成
   `Config#{tools => ...}` —— 编译得过、跑得动、模型**永远看不见那个工具**。
   症状是模型反复去调别的工具，还振振有词地说"没有可用的 render_a2ui"。
   现在配置里没有 `chat_client` 就**直接报错**：装不上的插件应当在装配期炸。
2. **提示词里的破折号被按字节截断**。Erlang 的 `<<"...">>` 把非 ASCII 码点截成
   一个字节，`—` 进模型就是个控制字符（JSON 里是 `\u0014`）。插件的提示词是英文
   散文，现在全部改回 ASCII，并有一个测试盯着（`plugin_prompts_are_ascii_test`）。
3. **`max_tokens` 不够**。A2UI 的组件树、genui 的 HTML 都不短，缺省 2048 装不下 ——
   截断的 JSON 解不出来，插件只拿到空参数，症状是"调了但什么都没画"。

顺带一条工程陷阱：`rebar3 eunit` 只更新 `_build/test`，而 examples 经 `ERL_LIBS`
加载的是 `_build/default` —— 只跑过测试就来跑例子，加载的是**上一次 compile 留下
的旧 beam**。`examples/Makefile` 的 `compile` 现在会先编上层。

## 尚未覆盖的

1. **认证与限流**。`beamai_a2a_auth` / `beamai_a2a_rate_limit` 是现成的样板。
2. **未对真实 CopilotKit 前端验证过**。事件形状是照协议文档写的，还没被真客户端
   消费过（真 LLM 已经跑通，见下）。
3. **单进程**。会话在进程内，多实例部署要 sticky session（同一个 threadId 落到
   同一个节点），否则重连会连到一个没有那份缓冲的节点上。
4. **`/threads/:id/messages`**。对话历史归 chat memory，而那层协议没有「列出某个
   会话的消息」的入口 —— 造一个就等于建第二个真相店。会话里那份消息账本只是
   **这条流自己的投影**（用来补缺口），不当历史用。
5. **异步委派的 lane**（`management_tools`）。要让 `beamai_subagent_manager` 透出
   观察钩子，那是上游的改动。
6. **插件的中间态**。三个 activity 插件都在参数到齐时一次翻译，没有「边生成边
   长出来」。

## 实机验证

`examples/src/example_agui_live.erl` 把整条链路接通：真起 cowboy → gun 发 POST →
真模型决定调不调工具 → SSE 帧一帧帧回来，最后把 `STATE_DELTA` 打回客户端那份
基线上比对。

```
cd examples && make compile
ERL_LIBS=../_build/default/lib:../_build/default/lib/beamai_core/apps:../_build/default/lib/beamai_llm/apps:../_build/default/lib/beamai_agent/apps:../_build/default/checkouts rebar3 shell
1> example_agui_live:run().          %% MiniMax m2.7
2> example_agui_live:run(zhipu).     %% ZHIPU glm-5.3-flash
```

两家都跑通了，观察到的：

- `TOOL_CALL_ARGS` 确实是**逐字**流出来的（`{"` → `text` → `":"` → `买` → `牛奶`
  → `"` → `}`）—— 增量翻译在真 provider 上成立，不只是合成 chunk 里成立；
- `REASONING_*` 在 glm-5.3-flash 上整段流出；
- 两个并发工具调用的 `TOOL_CALL_RESULT` **到达顺序与调用顺序相反** —— 这正是
  `tool_call_id` 配对不可省的现场证据；
- 客户端把补丁打到基线上，重建出的 state 与服务端一致。

`example_agui_durable` 则验证了跨 BEAM 进程的会话恢复：phase2 在全新进程里答出
「你叫小李，项目代号是 Falcon」—— 历史确实是从 `history.dets` 读回来的
（MiniMax `m2.7` 与 ZHIPU `glm-5.3-flash` 都验过）。

### 续跑的两条 TOOL_CALL_RESULT 来路

| 决策 | 结果从哪来 |
|---|---|
| `reply` / 拒绝 / 自由文本 | 人给的，runtime 在 `emit_resume_result` 里补上 |
| `approved` | 服务端真执行那个工具，走 `on_tool_result` → emitter |

两条互斥，同一次调用只会有一条结果。
