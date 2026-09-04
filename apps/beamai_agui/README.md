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
        {"/agui/info",                 beamai_agui_cowboy_handler, #{mode => info}},
        {"/agui/threads",              beamai_agui_cowboy_handler, #{mode => threads}},
        {"/agui/threads/:thread",      beamai_agui_cowboy_handler, #{mode => thread}},
        {"/agui/threads/:thread/:sub", beamai_agui_cowboy_handler, #{mode => thread}}
    ]}
]).
```

`:sub` 认四个：`events` / `state` / `stop` / `stream`。

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

`CUSTOM(interrupt)` 那一条**也还在发**：它是我们先于协议实现的那条，自家前端在用，
不动它。两条互不冲突（一条是 CUSTOM，一条是终态事件的一个字段）。

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
E="../_build/default/lib:../_build/default/checkouts"
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

## 前端上下文（`RunAgentInput.context`）

`context` 是前端每轮重发的**页面状态**（`useAgentContext`）：当前选中了哪一行、
用户是谁、页面在哪一屏。runtime 把它渲染成几行**追加到 system prompt 后面** ——
追加不覆盖，工厂那份人设是常量配置，不该被一次请求改掉。

抬头那句「以下是前端在本轮提供的上下文（AG-UI context，非用户输入）」是给模型的
出处说明：不写的话这几行会被当成用户指令，而它们其实是页面状态。

它与 state 的分工：**state 是给工具读的**（`beamai_context:state_get/3`，模型看不见），
**context 是说给模型听的**。要让模型能就当前 state 推理，把它渲染进 context 或
system prompt 是唯一的路。

## 事件流插件（`event_transform`）

`beamai_agui_runner:run/4` 收一个 `event_transform`：可以改写一条事件、吞掉它
（返回 `drop`）、或在它前后插事件（返回列表）。A2UI / Open Generative UI 那类东西
住在这个位置 —— 它们把工具调用翻成活动事件，而 runner / emitter 一个字都不用改。

插件自己抛异常不会把 run 带走：它是旁路增强，出事就退回原事件。

## 多模态输入

客户端可能把 `content` 发成一个部件数组。runtime 把其中的文本部件拼起来往下走，
非文本部件跳过 —— 骨架阶段不往下传图片，**翻不动就别假装翻得动**。不做这一步的话
下游拿到的是一坨结构体，内容全丢且不报错。

## 尚未覆盖的

1. **认证与限流**。`beamai_a2a_auth` / `beamai_a2a_rate_limit` 是现成的样板。
2. **未对真实 CopilotKit 前端验证过**。事件形状是照协议文档写的，还没被真客户端
   消费过（真 LLM 已经跑通，见下）。
3. **A2UI / Open Generative UI / MCP Apps 插件**。挂点（`event_transform`）与
   `/info` 的能力位都在了，插件本身还没写 —— 不装就如实报 false。
4. **单进程**。会话在进程内，多实例部署要 sticky session（同一个 threadId 落到
   同一个节点），否则重连会连到一个没有那份缓冲的节点上。
5. **`/threads/:id/messages`**。对话历史归 chat memory，而那层协议没有「列出某个
   会话的消息」的入口 —— 造一个就等于建第二个真相店。

## 实机验证

`examples/src/example_agui_live.erl` 把整条链路接通：真起 cowboy → gun 发 POST →
真模型决定调不调工具 → SSE 帧一帧帧回来，最后把 `STATE_DELTA` 打回客户端那份
基线上比对。

```
cd examples && make compile
ERL_LIBS=../_build/default/lib:../_build/default/checkouts rebar3 shell
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
