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
| `STATE_SNAPSHOT` | 每次 run 开头，把请求带来的 `state` 作为基线发回 |
| `STATE_DELTA` | `on_state_change`，与上一份算 JSON Patch |

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

## 尚未覆盖的

1. **认证与限流**。`beamai_a2a_auth` / `beamai_a2a_rate_limit` 是现成的样板。
2. **未对真实 CopilotKit 前端验证过**。事件形状是照协议文档写的，还没被真客户端
   消费过（真 LLM 已经跑通，见下）。

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
