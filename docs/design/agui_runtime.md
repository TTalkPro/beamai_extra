# AG-UI Runtime (`beamai_agui`): 设计决策

## 背景

CopilotKit 的 `@copilotkit/runtime` 是 AG-UI 协议的服务端：接住前端的
`RunAgentInput`，把一次 agent 执行翻译成可增量渲染的 SSE 事件流。这个想法在
BeamAI 里同样成立，问题只是**放哪个仓库**。

## 决策一：放 beamai_extra，不放 beamai

| | beamai（上游核心库） | beamai_extra |
|---|---|---|
| 依赖 | `gun` / `uuid` / `esqlite` / `poolboy`，**无 cowboy** | 已有 `cowboy 2.12` |
| 定位 | ChatClient 基座 + LLM 客户端 + SimpleAgent | Tools / RAG / 协议实现 |
| 已有协议服务端 | 无 | `beamai_a2a`、`beamai_mcp` |

AG-UI runtime 本质是 HTTP + SSE **服务端**，塞进上游就得引 cowboy，relx release、
dialyzer plt、文档定位全要跟着变。

而 `beamai_a2a` 就是同一形状的东西，只是协议不同：

| CopilotKit runtime | beamai_a2a 已有的对应物 |
|---|---|
| `createCopilotRuntimeHandler`（fetch 原语） | `beamai_a2a_http_handler`（框架无关） |
| Express/Hono adapter | `beamai_a2a_cowboy_handler` |
| `AgentRunner` / thread 持久化 | `beamai_a2a_task` + `beamai_a2a_task_sup` |
| `hooks.onRequest` / middleware | `beamai_a2a_middleware` |
| SSE 响应流 | `beamai_sse`（在 beamai_core） |

注意最后一行：**协议原语在 core，协议服务端在 extra** —— 这条缝已经存在，
AG-UI runtime 落在缝的上侧。

与 A2A 是**兄弟不是重复**：A2A 面向 agent↔agent，AG-UI 面向 agent↔前端
（逐 token 渲染、工具调用可视化、HITL）。

## 分层

```
前端 (CopilotKit / 任意 AG-UI 客户端)
        │  POST /agui/run   ──►  SSE 事件流
        ▼
beamai_agui_cowboy_handler      适配器：写帧、管连接
        │
beamai_agui_http_handler        框架无关：解析、校验、定位 agent
        │
beamai_agui_runner              一次 run 的编排（新跑 / 续跑）
        │
beamai_agui_emitter             翻译层：agent 回调 → AG-UI 事件
        │
beamai_agent                    ReAct 循环（上游）
```

辅助模块：`beamai_agui_registry`（agent 工厂表）、`beamai_agui_types`
（`RunAgentInput` 解析）、`beamai_agui_event`（事件构造 + SSE 编码）、
`beamai_agui_thread`（未决暂停）。

**SSE 帧编码是适配器的事**，不在框架无关层 —— 各框架的流式写法差别很大
（cowboy 的 `stream_body`、别家的 chunked writer），把它留给适配器，
`http_handler` 才是真的框架无关。

## 事件映射

| AG-UI 事件 | 由什么驱动 |
|---|---|
| `RUN_STARTED` / `RUN_FINISHED` / `RUN_ERROR` | runner 在 agent 前后发 |
| `STEP_STARTED` / `STEP_FINISHED` | `on_llm_call` / `on_llm_result`（每轮迭代） |
| `TEXT_MESSAGE_START` | 第一个 token 到达时**惰性**发出 |
| `TEXT_MESSAGE_CONTENT` | `on_token`（`Meta.message_id` 决定归属） |
| `TEXT_MESSAGE_END` | `on_message_end` |
| `TOOL_CALL_START` / `ARGS` / `END` | `on_message_end` 里那条消息的 `tool_calls` |
| `TOOL_CALL_RESULT` | `on_tool_result`（`Info.tool_call_id` 配对） |
| `STATE_SNAPSHOT` | 每次 run 开头，把请求带来的 `state` 作为基线发回 |
| `STATE_DELTA` | `on_state_change`，与上一份算 JSON Patch |

## 决策二：`TEXT_MESSAGE_START` 惰性发出

`on_message_start` 在 LLM 调用**前**触发（必须如此，否则流式 token 到达时还没有
`messageId` 可归属），那时还不知道这一轮会产出文本还是纯 `tool_calls`。

所以 emitter 只记下 id，**等第一个 token 真的到了再发 START** —— 纯工具轮就
自然不会冒出一对空的 START/END。

非流式路径（`run/2`，没有 token）由 `on_message_end` 发现「这条消息没开过文本流、
但有 content」，一次性补齐 START + CONTENT + END。这条回落在续跑走非流式版本时
救过场，是必要的而非防御性的。

## 决策三：run 另起进程，连接进程只写帧

cowboy 的 `stream_body` 只能在请求进程里调用，而 agent 跑起来是同步阻塞的。
同一个进程既跑 agent 又写帧，就只能等 agent 跑完再一次性写 —— 那就不是流式了。

于是：run 进程跑 agent、把事件发消息给请求进程，请求进程专心写帧。
客户端断开时 cowboy 杀掉请求进程，run 跟着停，不会留一个没人看的 LLM 调用在烧钱。

## 决策四：只取最后一条 user 消息

AG-UI 客户端每次会把**整个** `messages` 数组发上来，而 `beamai_agent` 每次只收
一条新消息、跨轮历史由 memory filter 按 `conversation_id` 维护。

全量回灌会让历史重复计入两次。所以只取最后一条 user 消息，历史归 memory 管。

## 决策五：run 持久化不另造存储

上游 `beamai_agent` 已经有一整套跨进程 HITL：agent 配了 `pause_store` 就在中断时
把**中断态快照**按 `conversation_id` 落库（`beamai_agent_pause`），而
`beamai_agent:resume/3` 在本进程没有中断态时会**透明回落**去 store 里读回来。

所以只要把三个 id 对齐：

```
AG-UI threadId  ==  beamai conversation_id  ==  pause_store 的键
```

「这个请求是不是一次 resume」就等价于「这个 thread 在 store 里有没有未决快照」
—— 不需要再造一层 run 表（原计划里的 `run_sup` + run 记录一个都没写）。

### HITL 的一次完整往返

1. agent 停下来等人 → 上游落库快照；
2. runtime 发 `CUSTOM(interrupt)`（带 `toolCallId` / `toolCallName` / `args`）
   加 `RUN_FINISHED`，这次 HTTP 请求结束；
3. 前端此时看到的是一个**没有结果**的 `TOOL_CALL_START/ARGS/END` —— 那就是
   「等你回话」；
4. 下个请求进来，runtime 发现该 thread 有未决暂停，转入续跑。

### 答复的三条来路

| 优先级 | 来路 | 语义 |
|---|---|---|
| 1 | `forwardedProps.resume` = `#{"decision", "payload"}` | 全语义：`approved` / `rejected` / `reply`，可带 `args` 改参数 |
| 2 | `role=tool` 消息，`toolCallId` 对得上 | AG-UI 原生：前端工具执行完回传结果 |
| 3 | 最后一条 `user` 消息 | 聊天式：agent 问、人直接回一句 |

payload 的 binary 键转成上游要的 atom 键时**只认 `message` / `args`**：
请求体是外部输入，拿它 `binary_to_atom` 等于给人一个撑爆原子表的口子。

### `TOOL_CALL_RESULT` 的两条来路（互斥）

| 决策 | 结果从哪来 |
|---|---|
| `reply` / 拒绝 / 自由文本 | 人给的，runtime 在 `emit_resume_result` 里补 |
| `approved` | 服务端真执行那个工具，走 `on_tool_result` → emitter |

同一次调用只会有一条结果。

### 工厂必须可重放

快照是**纯数据**（中断态、被中断的 tool_call、已完成的结果、迭代计数），
**不存** ChatClient / tools / callbacks（含 fun，本来也序列化不了）—— 那些由
agent 工厂在下个请求里重建。

所以同一个 thread 的两次请求必须造出配置一致的 agent（同样的工具集、同样的
system prompt），否则恢复出来的中断态会对不上新 agent 的工具表。

### 暂停存到哪儿

三选一，按优先级：宿主自带的句柄（`pause_store`）> 内置 DETS
（`pause_store_file`）> 内置 ETS（缺省）。前两种由 `beamai_agui_sup` 起，
第一种它什么都不起。

缺省选 ETS 而不是 DETS：开发期够用，也不会在谁的机器上凭空多出一个文件。
生产配 `pause_store_file` 即可跨节点重启续跑 —— 但**光配它不够**，chat memory
也得换成持久的（`beamai_chat_memory_dets`）：pause_store 只存"停在哪儿"，
不存对话历史，两者缺一，恢复出来的会话都是残的。

持久化带来一个 ETS 版没有的新问题：**被放弃的暂停不会自己消失**。ETS 版进程
一死表就回收，DETS 版会一直躺在磁盘上。故 DETS 后端额外提供 `conversations/1`
与 `prune/2`（按 `paused_at` 清理），behaviour 之外，纯运维用。

### store 不可用时静默退化

上游在**每个终态**都会 `pause_clear`。把一个指向死进程的句柄交给 agent，会让
**正常的 run 也崩在收尾上**。故 `runtime_config` 先查 `available_store/0`，
拿不到就不放 `pause_store` 键 —— 退化成「这个节点没有 HITL 持久化」，其余照常。

## 为此在上游 beamai 做的五处改动

runtime 是薄壳，真正的能力在 agent 层。以下五项是「AG-UI 需要但上游没有」的部分，
按落地顺序：

### A. 工具回调透出 `tool_call_id`

`on_tool_call` / `on_tool_result` 原来只给 `(Name, Args)` / `(Name, Result)`。
`parallel_tools` 缺省开启、结果触发顺序不定，**只靠工具名无法配对**（同名工具
可在一批里出现多次）。而 `tool_call_id` 本来就在 CallRecord 里，只是被丢掉了。

改法：按注册函数的 **arity** 分派 —— `Fun/2` 是旧签名，`Fun/3` 末位额外收 `Info`
（`Meta` + `tool_call_id`，`on_tool_result` 另有 `args` / `error`）。选 arity 分派
而不是直接改签名，是因为上游测试、下游 examples 和文档里有十几处旧写法。

### B. 透出 provider 原始流事件

`beamai_chat_client` 的流式 terminal 把 provider 的原始事件回调写死成
`fun(_Event) -> ok end`，而 `extract_token_from_event/1` 只认四种文本形状。于是
`tool_calls` 的 arguments 增量、thinking/reasoning 增量、逐块 usage **在这一层就被
丢干净**，下游任何东西都捞不回来。

改法：新增 chat opts `on_raw_event`（不下发给 provider，在 ChatClient 层剥掉），
agent 侧新增回调 `on_llm_event`。**刻意不归一化** —— 一旦在这层归一化，就等于替
所有宿主决定「哪些字段值得保留」，而这条通道的用户要的恰恰是没被裁剪过的原文。

### C. 消息边界回调

一轮 turn 里 assistant 文本**不止一条消息**（工具循环每迭代产出一条，直返还会再
合成一条），`on_token` 是连续的裸 token 流，分不出边界。

改法：新增 `on_message_start(MessageId, Meta)` / `on_message_end(Message, Meta)`。
**id 在 LLM 调用之前分配**，这样流式 token 到达时它已经在 `on_token` 的
`Meta.message_id` 里。两者**恒成对**：LLM 出错或无内容落定时 `Message` 为
`undefined`，宿主不必为异常路径兜底关闭。

`record_assistant` 是「这条消息到此为止」的唯一权威点，直返合成的回合也走它，
故边界对它同样成立。

### D. `stream_resume`

`resume/2,3` 不接 `stream_token_handler`，续跑那一轮走非流式 `invoke_chat`，
一个 token 都不产生 —— 人回完话之后的那段答案才是用户在等的正文。

改法：`resume/4`（底层入口，Opts 同 `run/3`）+ `stream_resume/2,3`
（`stream` 之于 `run` 的同一件事）。顺带修掉一个元数据问题：tool_loop 原来只往
Info 里塞 `message_id`，而 `stream/3` 的基础 Meta 是进入循环**之前**构造的，那时
本次 run 的 `run_id` 还没生成 —— 改成传本轮完整 Meta 后，四个回调看到的元数据
终于一致。

### F. 状态可观测、可种子

state 槽（`beamai_context` 的 state）此前**没有任何出口**：工具经 writes 写、屏障
处折叠，但没有回调把它透出来，宿主看不见变化；而且 turn 的 context 每轮新建，
也没有种初值的地方 —— 工具只能从空 state 起步。

改法两条：agent 配置新增 `initial_state`（由 `initial_turn_context/1` 种进每轮的
context），回调新增 `on_state_change(State, Meta)`（在屏障处折叠完 writes、且 state
**确实变了**时触发，参数是折叠后的整份 state）。只在变了时发 —— 绝大多数工具不写
state，每批都发等于给宿主刷噪音；要增量由宿主自己与上一份比，框架不替它决定 diff
的形状。

### E. 续跑里执行的工具触发 `on_tool_result`

`resume` 有两处在循环**外面**执行工具（审批通过的调用、环境失败的重跑），
`execute_tool_calls` 都漏传 `on_result`，宿主于是看到一次有始无终的工具调用。

改法：`beamai_agent_tool_loop:agent_result_cb/1`（吃 agent 状态，搭出与循环内
一致的结果回调），两处补上。**刻意不补** `on_tool_call` 那个策略门：被中断的调用
刚被人批过，再过一次门只会又被拦下来，死循环。

## 决策八：共享状态先立基线，再发补丁

AG-UI 的 `STATE_DELTA.delta` 是 **JSON Patch 数组**，前端拿它对本地状态做原地
修改。补丁是相对某一份具体的 state 算的 —— 没有共同基线就对不上，所以每次 run
开头先发一条 `STATE_SNAPSHOT`（内容就是请求带上来的 `state`）。

差分只产 `add` / `remove` / `replace`：RFC 6902 还有 `move` / `copy`（要识别
"同一个值挪了位置"，对状态槽没意义且容易算错）和 `test`（断言）。只对 map 递归，
数组与标量整体替换 —— 数组的元素级 diff 要一套 LCS 才不会算出一堆无意义的移位
补丁，收益不抵复杂度。路径按 RFC 6901 转义（`~` → `~0`，`/` → `~1`）：状态槽的
键是用户给的，含斜杠完全可能，不转义会算出一条指向别处的路径。

**续跑不采纳前端的 state**：暂停快照里带着服务端累积的那份，`resume` 从那里恢复。
一个停在半路的 turn，其状态的权威在服务端。

state 是给**工具**读的（`beamai_context:state_get/3`），模型看不到它。要让模型能
就当前状态推理，得由工厂把 state 渲染进 system prompt —— 实机测试里模型正是因为
看不见基线里已有的那项而数错了数。

## 已知缺口

### runtime 侧

1. **认证与限流**。`beamai_a2a_auth` / `beamai_a2a_rate_limit` 是现成样板。

### 工程侧

- **未对真实 CopilotKit 前端验证过**。事件形状是照协议文档写的，还没被真客户端
  消费过。

## 实机验证

`examples/src/example_agui_live.erl`：真起 cowboy → gun 发 POST → 真模型决定调不调
工具 → SSE 帧一帧帧回来，最后把 `STATE_DELTA` 打回客户端基线比对。
MiniMax `m2.7` 与 ZHIPU `glm-5.3-flash` 两家都跑通。

现场拿到的证据：

- `TOOL_CALL_ARGS` 确实**逐字**流出（`{"` → `text` → `":"` → `买` → `牛奶` → `"`
  → `}`），增量翻译在真 provider 上成立，不只是在合成 chunk 里成立；
- `REASONING_*` 在 glm-5.3-flash 上整段流出；
- 两个并发工具调用的 `TOOL_CALL_RESULT` **到达顺序与调用顺序相反** —— 这是
  `tool_call_id` 配对不可省的现场证据（改动 A 的理由）；
- 客户端把补丁打到基线上，重建出的 state 与服务端一致。

## 决策六：前端工具复用 HITL 管道

`RunAgentInput.tools` 是前端定义、前端执行的工具：模型该看得见它们（看不见就
永远不会调），但服务端没有实现可跑。`interrupt_tools` 恰好是这个语义 ——
命中即暂停、把球交出去。于是前端工具与人工审批走**同一条**暂停/续跑路径，
没有新增任何机制。

由 runtime 挂而不是让工厂挂：工具是**每个请求**带上来的，工厂却应当是可重放的
常量配置（跨请求要造出一致的 agent）。追加而非覆盖，重名时服务端那个赢 ——
它有真实现，前端那个只是个声明。

上游的 `interrupt_tool_to_spec` / `is_interrupt_tool` 同时认 atom 键与 binary 键，
所以 AG-UI 传上来的定义原样挂上去即可，不用转换。

## 决策七：增量事件与一次性回落互斥

`beamai_agui_raw` 按 provider 形状把原始 chunk 翻成 `TOOL_CALL_ARGS` /
`REASONING_*`（OpenAI 系与 Anthropic 系两组分支，认不出的安静丢弃）。

这条增量路会与 `on_message_end` **争同一个 `TOOL_CALL_START`** ——
前者在第一个 args 片段到达时就开了调用，后者手里那条 assistant 消息也带着完整的
`tool_calls`。故 emitter 记着「哪些 id 已经流过」，对它们只补一个
`TOOL_CALL_END`；没流过的仍走一次性发完整 JSON 的回落。两条路互斥，
同一次调用只会有一个 START。

推理段的收尾有两个触发点：正文一开口（`on_token`）或消息落定
（`on_message_end`），并保证 `REASONING_MESSAGE_END` 排在 `REASONING_END` 前面。
