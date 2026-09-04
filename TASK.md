# BeamAI Extra - Task List

## AG-UI Runtime（`beamai_agui`）（2026-09-03）

> 目标：把 CopilotKit runtime 的能力做进 BeamAI —— agent 一次执行翻译成前端可增量
> 渲染的 SSE 事件流，含跨请求的 HITL。
> 设计与取舍见 [docs/design/agui_runtime.md](docs/design/agui_runtime.md)。
> 所有改动**未提交**（beamai_extra + 上游各有改动，等你确认后统一提交）。

### 已做·上游 beamai（AG-UI 需要但上游缺的五处）

- [x] **A. 工具回调透出 `tool_call_id`**。`on_tool_call` / `on_tool_result` 原来只给
  `(Name, Args)` / `(Name, Result)`；`parallel_tools` 缺省开启、结果触发顺序不定，
  只靠工具名无法配对（同名工具可在一批里出现多次），而 id 本就在 CallRecord 里
  （`beamai_agent_utils.erl:143`）只是被丢掉。改为按注册函数 **arity** 分派：
  `Fun/2` 是旧签名，`Fun/3` 末位收 `Info`（`Meta` + `tool_call_id`，
  `on_tool_result` 另有 `args` / `error`）。选 arity 兼容而非改签名，是因为上游测试、
  下游 examples 与文档里有十几处旧写法。新增 `beamai_agent_callbacks:invoke/4`
  与 `tool_gate/4`；顺带把 `case catch Fun(...)` 换成 `try...of`（原写法会把**抛出**的
  `{interrupt, R}` 也当裁决）。回归测试 `tool_callbacks_carry_tool_call_id_test`。
- [x] **B. 透出 provider 原始流事件**。`beamai_chat_client` 流式 terminal 把原始事件
  回调写死成 `fun(_Event) -> ok end`，`extract_token_from_event/1` 只认四种文本形状
  —— `tool_calls` 的 arguments 增量、thinking/reasoning 增量、逐块 usage 在这一层就
  被丢干净。新增 chat opts `on_raw_event`（ChatClient 层自己消费，剥掉再下发 provider）
  与 agent 回调 `on_llm_event`。**刻意不归一化**（理由见设计文档）。4 个测试，含
  「不渗进 provider opts」与「sink 抛异常不打断流」。
- [x] **C. 消息边界回调**。一轮 turn 里 assistant 文本不止一条消息，`on_token` 是裸
  token 流分不出边界。新增 `on_message_start(MessageId, Meta)` /
  `on_message_end(Message, Meta)`，**id 在 LLM 调用之前分配**（这样流式 token 到达时
  它已在 `on_token` 的 `Meta.message_id` 里）；两者**恒成对**（失败路径 `Message`
  为 `undefined`）。直返合成的回合也发边界 —— 否则只听边界回调的宿主会整条漏掉
  return_direct 的答案。3 个测试。
- [x] **D. `stream_resume`**。`resume/2,3` 不接 `stream_token_handler`，续跑那一轮
  走非流式 `invoke_chat`，一个 token 都不产生。新增 `resume/4`（底层入口，Opts 同
  `run/3`）+ `stream_resume/2,3`。顺带修元数据：tool_loop 原来只往 Info 塞
  `message_id`，而 `stream/3` 的基础 Meta 在进入循环**之前**构造，那时本次 run 的
  `run_id` 还没生成 —— 改传本轮完整 Meta 后四个回调元数据终于一致。3 个测试。
- [x] **F. state 可观测、可种子**（详见下面「共享状态」一节）。
- [x] **E. 续跑里执行的工具触发 `on_tool_result`**。`resume` 有两处在循环外面执行
  工具（审批通过的调用、环境失败的重跑），`execute_tool_calls` 都漏传 `on_result`，
  宿主看到一次有始无终的工具调用。新增 `beamai_agent_tool_loop:agent_result_cb/1`，
  两处补上。**刻意不补** `on_tool_call` 策略门：刚被人批过的调用再过一次门只会又被
  拦下来，死循环。2 个测试（approved 触发 / rejected 不触发）。

上游验证：`rebar3 eunit` 735 tests 0 failures；编译零警告；dialyzer 无警告；
xref 仅既有的 unused-export 类。

### 已做·beamai_extra 新应用 `beamai_agui`

- [x] **应用骨架**（10 个源模块 + 6 个测试模块 + README）。分层照 `beamai_a2a`：
  `cowboy_handler`（写帧、管连接）→ `http_handler`（框架无关）→ `runner`（编排）
  → `emitter`（翻译层）→ `beamai_agent`。辅以 `registry`（agent 工厂表）、
  `types`（`RunAgentInput` 解析）、`event`（构造 + SSE 编码）、`thread`（未决暂停）。
  已登记进 `rebar.config` 的 `project_app_dirs` / `shell` / `relx`。
- [x] **事件翻译层**。惰性 `TEXT_MESSAGE_START`（纯工具轮不冒空的 START/END 对）、
  非流式回落（`on_message_end` 一次性补三件套）、`tool_call_id` 配对
  `TOOL_CALL_RESULT`、错误路径先闭合再发 `RUN_ERROR`。11 个单测。
- [x] **端到端事件序列测试**。mock LLM 跑一轮真工具循环，断言事件序列正好是
  `RUN_STARTED → STEP* → TOOL_CALL_START/ARGS/END → TOOL_CALL_RESULT → STEP_STARTED
  → TEXT_MESSAGE_* → RUN_FINISHED`。作用是**证明翻译层对上游回调的假设成立**
  （emitter 单测喂的是假回调，这里喂的是 `beamai_agent` 真发的）。
- [x] **跨请求 HITL（run 持久化）**。发现上游已有完整机制（`pause_store` +
  `beamai_agent_pause` + `resume` 的透明回落），故**不另造 run 表**，只把三个 id
  对齐：`threadId == conversation_id == pause_store 键`。答复三条来路
  （`forwardedProps.resume` > 对得上 id 的 `tool` 消息 > 最后一条 user 文本）；
  payload 转 atom 键**只认 `message` / `args`**（外部输入不做 `binary_to_atom`）。
  `available_store/0` 保证 store 不可用时静默退化 —— 上游在每个终态都 `pause_clear`，
  把死句柄交给 agent 会让正常的 run 也崩在收尾上。5 个测试，主用例是两次**完全
  独立**的 run，中间只隔着 pause store。

beamai_extra 验证：`rebar3 eunit` **666 tests 0 failures**（`beamai_agui` 贡献 79 个）；
dialyzer 只剩既有的 `beamai_mcp_server.erl:442`；xref 无非 unused-export 警告。
上游验证：`rebar3 eunit` **750 tests 0 failures**；编译零警告；dialyzer 无警告。

### 已做·HTTP 层（此前零覆盖）

- [x] **cowboy 集成测试**（`beamai_agui_cowboy_tests`，6 个）。此前所有用例都直接调
  `beamai_agui_runner:run/3`，绕开了适配器 —— `stream_reply` / `stream_body` /
  `forward` 循环一行都没执行过。现在真起 cowboy（端口 0 由内核挑，避免抢端口）、
  用 gun 真发请求、用 `beamai_sse:parse/1` 读帧：SSE 完整往返（含
  `x-accel-buffering: no`，nginx 缓冲了就不是流式了）、info 端点、未知 agent 404、
  畸形体 400、方法 405、OPTIONS 预检 204 + CORS。**一次跑通**。
- [x] **修 `stream_run/3` 的 link 方向**。`link` 是双向的：它要的是「客户端断开 →
  agent 跟着停」，但 run 进程异常退出会**先把请求进程带走**，前端只剩截断的流，
  连 `RUN_ERROR` 都发不出去；那个补发的 `{'DOWN', ..., Reason}` 分支是死代码。
  请求进程又不能 `trap_exit`（它本身是 cowboy `spawn_link` 出来的，trap 了会破坏
  「请求进程随连接消亡」的假设）。解法：让 run 进程**永不异常退出** —— 自己兜住
  异常、把结局随 `{done, Ref, Outcome}` 交回来，由请求进程决定发什么。于是 link
  只剩下我们想要的那个方向。

### 已做·功能

- [x] **前端工具（frontend tools）**。`RunAgentInput.tools` 挂成 agent 的
  `interrupt_tools`：模型看得见（看不见就永远不会调），调到就暂停把球交出去 ——
  与人工审批**同一条**管道，没有新增任何机制。由 runtime 挂而不是让工厂挂：工具是
  每个请求带上来的，工厂却应当是可重放的常量配置。追加不覆盖，重名时服务端那个赢
  （它有真实现）；没 `name` 的条目直接丢（会变成 name 为空的 spec 把 LLM 请求带坏）。
  上游 `interrupt_tool_to_spec` / `is_interrupt_tool` 同时认 atom 与 binary 键，
  故 AG-UI 的定义原样挂上去即可。4 个测试，含「确实下发给了模型」与完整往返。
- [x] **`TOOL_CALL_ARGS` 逐字增量 + `REASONING_*`**（`beamai_agui_raw`，runner 缺省
  接上）。两组 provider 分支：OpenAI 系（`delta.tool_calls[].function.arguments`
  片段、`reasoning_content` / `reasoning`）与 Anthropic 系
  （`input_json_delta.partial_json`、`thinking_delta.thinking`）。认不出的 chunk
  **安静丢弃** —— 旁路增强不该让主流程有感知。
  增量路会与 `on_message_end` **争同一个 `TOOL_CALL_START`**，故 emitter 记着
  「哪些 id 已流过」，对它们只补 `TOOL_CALL_END`；没流过的仍走一次性发完整 JSON 的
  回落。推理段在正文开口或消息落定时收尾，保证 `MESSAGE_END` 排在 `END` 前。
  9 个测试，含两家形状、混批、index 每条消息重置、陌生 chunk 丢弃。

### 已做·共享状态（`STATE_SNAPSHOT` / `STATE_DELTA`）

- [x] **上游 F：state 可观测、可种子**。state 槽此前**没有任何出口** —— 工具经
  writes 写、屏障处折叠，但没有回调把它透出来；而且 turn 的 context 每轮新建，
  也没有种初值的地方，工具只能从空 state 起步。（此前 README 里"不需要改上游"
  那句话是错的，据此更正。）改法两条：agent 配置新增 `initial_state`
  （`initial_turn_context/1` 种进每轮 context），回调新增
  `on_state_change(State, Meta)`（屏障处折叠完 writes、且 state **确实变了**才触发，
  参数是整份 state）。只在变了时发 —— 绝大多数工具不写 state，每批都发是噪音；
  增量由宿主自己比，框架不替它决定 diff 的形状。上游 4 个测试。
- [x] **`beamai_agui_state`：JSON Patch 差分**。只产 `add` / `remove` / `replace`
  （RFC 6902 的 `move` / `copy` 对状态槽没意义且易算错，`test` 是断言）。只对 map
  递归，数组与标量整体替换（元素级 diff 要 LCS 才不会算出一堆无意义移位补丁，
  收益不抵复杂度）。路径按 RFC 6901 转义（`~`→`~0`、`/`→`~1`）—— 槽键是用户给的，
  含斜杠完全可能，不转义会算出一条指向别处的路径。键序固定，同样输入总得同样补丁。
- [x] **runtime 接线**。请求的 `state` 经 `initial_state` 种进 agent；每次 run 开头
  发 `STATE_SNAPSHOT` **立基线**（补丁相对某一份具体 state 算，没有共同基线就对不上）；
  `on_state_change` → 与上一份算补丁 → `STATE_DELTA`。**续跑不采纳前端 state**：
  暂停快照里带着服务端累积的那份，停在半路的 turn 其状态权威在服务端。
  11 个测试（差分 8 + 端到端 3）。
- [x] 既有 9 处事件序列断言随基线快照更新。

### 已做·实机验证（真 LLM + 真 HTTP + 真 SSE）

- [x] **`examples/src/example_agui_live.erl`**。真起 cowboy → gun 发 POST → 真模型
  决定调不调工具 → SSE 帧一帧帧回来，最后把 `STATE_DELTA` 打回客户端基线比对。
  **MiniMax `m2.7` 与 ZHIPU `glm-5.3-flash` 两家都跑通**。现场证据：
  - `TOOL_CALL_ARGS` 确实**逐字**流出（`{"` → `text` → `":"` → `买` → `牛奶` →
    `"` → `}`）—— 增量翻译在真 provider 上成立，不只在合成 chunk 里成立；
  - `REASONING_*` 在 glm-5.3-flash 上整段流出；
  - 两个并发工具调用的 `TOOL_CALL_RESULT` **到达顺序与调用顺序相反** —— 这是
    `tool_call_id` 配对不可省的现场证据（改动 A 的理由）；
  - 客户端把补丁打到基线上，重建的 state 与服务端一致。
- [x] 顺手修 `examples/src/beamai_examples.app.src` 的 `applications` 仍列
  `jsx` / `hackney`（两者早已从项目移除，谁真去 `ensure_all_started` 就会莫名失败）。

### 发现·待决

- [ ] **模型看不到 state**。state 是给**工具**读的（`beamai_context:state_get/3`），
  不进 prompt。实机测试里模型正是因为看不见基线里已有的那项而数错了数
  （"共有 2 项"，实为 3）。要让模型能就当前状态推理，得由工厂把 state 渲染进
  system prompt —— 是留给宿主自己决定，还是 runtime 提供一个可选的注入？

### 已做·持久化 pause_store（跨节点重启的 HITL）

- [x] **上游 `beamai_pause_store_dets`**。DETS 实现 `beamai_pause_store` 的三个回调，
  `set` 表、key 即 conversation_id（每会话至多一份，覆盖语义天然成立），
  **每次写后 `dets:sync`** —— 一个没来得及落盘的暂停，恰恰是节点崩溃时你最需要
  的那个；暂停本身是"等人"级别的低频事件，多一次 fsync 无关紧要。
  写失败只能记 error 日志：behaviour 契约是 `-> ok`，上游 `beamai_agent_pause:save/1`
  也不看返回值，丢一个暂停比让整个 run 崩在收尾上强，但必须留痕。
- [x] **额外的运维 API**：`conversations/1` + `prune/2`（按 `paused_at` 清理）。
  持久化带来 ETS 版没有的新问题 —— 进程一死表就回收 vs 快照永远躺在磁盘上：
  用户开了个话题、agent 停下来问、人再也没回来。没有 `paused_at` 的快照当作陈旧
  （连什么时候存的都说不清，判断不了该不该留）。
- [x] **上游回归测试**（9 个）：behaviour 契约、覆盖语义、会话隔离、
  **重启后快照还在**、**清除也落了盘**（否则重启会"复活"一个已恢复的暂停）、
  复杂快照无损往返、prune 三例。
- [x] **上游 agent 级测试**（2 个）：暂停落 DETS → store 进程停掉再从同一文件开起来
  → **全新的 agent** 只靠 conversation_id 从磁盘接上并跑完；以及会话隔离
  （另一个会话 resume 应当 `{error, not_interrupted}`，不能捡起别人的暂停）。
  顺带写清楚一件事：新 agent 的 `interrupt_state` 是 `undefined`，但
  `is_interrupted/1` 返回 `true` —— 它会去 store 里问，这正是持久化的意义。
- [x] **runtime 接线**：`beamai_agui_thread:backend/0` 三选一
  （宿主自带句柄 > `pause_store_file` 走 DETS > 缺省 ETS），`beamai_agui_sup`
  据此起对应的 child。缺省仍是 ETS —— 开发期够用，也不会在谁的机器上凭空多出
  一个文件。5 个测试，含后端选择四例与**跨 store 重启的 runtime 级 HITL 往返**。

- [x] **另一半：chat memory 也落 DETS**。pause_store 只存"停在哪儿"，不存对话
  历史；两者缺一，恢复出来的会话都是残的。agent 配 `memory => {store, Handle}`
  即可接 `beamai_chat_memory_dets`（上游已有）。memory 归工厂管，runtime 不越俎
  代庖 —— 但 README 与设计文档把配置示例给全了。
- [x] **`examples/src/example_agui_durable.erl`：跨 BEAM 进程的重启验证**。
  不是同一个 VM 里 stop 再 start，是**两个独立的 OS 进程**共享磁盘上那两个文件：
  phase1 告诉 agent「我叫小李，项目代号 Falcon」后进程退出；phase2 是全新 BEAM，
  从 `history.dets` 读回历史，答出「你叫小李，项目代号是 Falcon」。
  MiniMax `m2.7` 与 ZHIPU `glm-5.3-flash` 都验过。
  （HITL 跨重启由 `beamai_agui_durable_tests` 与上游
  `hitl_survives_store_restart_test` 覆盖，本例只走历史那一半。）
- [x] 顺手修两个例子里按**字节**截断 UTF-8 的显示 bug（`binary:part/3` 会把一个
  码点劈成两半，打出来是乱码）—— 改用 `string:slice/2` 按字符截。

### 已做·会话层：run 的寿命脱离 HTTP 请求（2026-09-04，参照 clj-agent-agui）

> 参照 `~/workspace/clj-agent/modules/clj-agent-agui` 的 `runtime` / `codec` 两个 ns
> 补齐。那边的头号能力（「让 run 的寿命不再由 HTTP 请求的寿命决定」）此前
> beamai_agui 完全没有：run 由 cowboy 适配器在请求进程下 spawn，连接一断就死，
> 另一个请求也看不见它。

- [x] **`beamai_agui_session`**（一个 thread 一个 gen_server）+ `beamai_agui_sessions`
  （threadId → 进程的注册表，读走 ETS、建走 gen_server 串行化）+
  `beamai_agui_session_sup`（`simple_one_for_one`，`temporary` —— 会话里全是易失的
  东西，重启出来是个空壳，且 seq 归零会让没断线的客户端补出上一轮的事件）。
  三者进 `beamai_agui_sup`，顺序有意义（建会话要 `supervisor:start_child`）。
- [x] **事件环形缓冲 + 会话级单调 seq**。号**不进事件 JSON**（AG-UI 的 BaseEvent 没
  这一位），只写进 SSE 帧的 `id:` 行 —— 浏览器 `EventSource` 重连自带
  `Last-Event-ID`，前端一行代码不用写。**跨 run 连续**：第二轮不重置回 0，否则
  「补 3 号之后的」会把上一轮的也补一遍。水位早于缓冲起点时先发 `CUSTOM(resync)`。
- [x] **run 进程挂在会话下**（会话 `trap_exit` + `spawn_link`）。两个方向都要：
  run 崩了会话不跟着死（替它补终态事件），会话没了 run 也不留成孤儿。
  **停一个 run 就是杀掉那个进程** —— BEAM 有抢占，不需要 JVM 那套「协作式取消 +
  正在跑的工具会跑完」的妥协（clj 那边受限于 JVM，只能协作式）。
- [x] **「一条 run 恰好一个终态事件且它是最后一条」不因崩溃而破**：会话在 run 进程
  的 `EXIT` 里看它有没有把话说完，没有就补 —— 被 stop 的补
  `RUN_FINISHED result.status=cancelled`，崩的补 `RUN_ERROR code=runner_died`，
  正常退出却没发终态的补 `runner_silent`（如实报契约破了，而不是假装跑完了）。
- [x] **并发策略**：`reject`（缺省，HTTP 409）/ `supersede`。supersede 要先放掉
  会话这一跳再等旧 run 收尾 —— 收尾本身要回到同一个 gen_server，同步等必死锁；
  做法是把新 run 挂成 `pending_start`，在旧 run 的 `EXIT` 里起它并 `gen_server:reply`，
  另加一个超时兜底（不能让调用方无限期挂着）。
- [x] **空闲驱逐**（缺省 30 分钟）。有 run / 有订阅者 / 停在等人的都不驱逐。
  会话自己拿 `send_after` 定期自检，不长一根扫表线程。
- [x] **HTTP 层**：`GET /threads`、`GET /threads/:id/{events,state}`、
  `GET /threads/:id/stream`（重连，认 `?since=` / `?lastEventId=` / `Last-Event-ID`）、
  `POST /threads/:id/stop`、`DELETE /threads/:id`。只读面**没有第二套存储** ——
  列表来自会话注册表、事件来自环形缓冲、状态由流上的 SNAPSHOT + DELTA 折出来。
  CORS 放行 `last-event-id`、暴露 `x-agui-run-id`。SSE 加 25s 心跳注释帧。
- [x] **`beamai_agui_state:apply_patch/2`**（`diff/2` 的逆）。会话要维护"现在的 state
  是什么"，只记最后一次快照的话，第一个补丁之后就过期了。只认自己产得出的三种 op，
  路径按 RFC 6901 反转义（**`~1` 必须先解**：先解 `~0` 会把 `~01` 变成 `/`）。
  既有的 atom 键沿用 atom 形态，不写成两份。
- [x] **回落不变**：会话层没起来时 cowboy 适配器仍走"run 绑在请求上"的老路径，
  既有 6 个 cowboy 集成测试一字未改照过。

### 已做·协议保真（同批，对着 clj 的 `codec` 逐条核）

- [x] **AG-UI 的 interrupt 协议**。暂停原来只发 `CUSTOM(interrupt)`（自家约定），
  通用客户端不认。现在同时发
  `RUN_FINISHED + outcome:{type:"interrupt", interrupts:[…]}` —— 收口与告知是同一条
  事件。`interrupt.id` 取被中断调用的 tool-call id，带 `responseSchema` 与
  `metadata.pendingTool`。**`approveWithEdits` 不报**（改参数再执行没实现）。
  `CUSTOM` 那条照旧发，两条互不冲突。
- [x] **`RunAgentInput.resume[]`**（协议里答复暂停的唯一入口）。放在四条来路的最前
  面。`interruptId` 对得上 pending 调用才采纳，**对不上就不认** —— 拿别的 interrupt
  的答复去恢复这一个，是把人的意思安到别处；只有一条且不带 id 时宽容采纳（不少
  客户端只发 `[{status:"approved"}]`）。措辞归一（accept/deny/respond → 上游关键词）。
- [x] **`/info` 的 `agents` 从数组改成以 id 为键的字典**。这是个**真 bug**：客户端做
  `Object.entries(runtimeInfo.agents)` 再按 id 建 proxy，给数组它会把下标当 agent id，
  于是去请求 `/agent/0/run`。同批补上 `capabilities`（humanInTheLoop / transport /
  tools / reasoning / state，**每一位都是真的**）、`threadEndpoints`（会话层起没起
  如实报；`realtimeMetadata` 恒 false）、`mode` / `version` / 三个插件位。
  **`threadEndpoints` 无论开关都发** —— 省略与「明确说没有」在客户端那儿是两码事。
- [x] **`RunAgentInput.context` → system prompt**（此前解析了但从没用过）。追加不
  覆盖；抬头写明出处，否则这几行会被模型当成用户指令。与 state 的分工写进 README：
  state 给工具读、context 说给模型听。
- [x] **多模态 `content` 数组**。此前 `content_of/1` 只认 binary，数组直接变 `<<>>`
  —— 内容全丢**且不报错**。现在把文本部件拼起来，非文本部件跳过。
- [x] **`RUN_STARTED.parentRunId`**（原样带着走，不建父子索引、不做级联停止）与
  **`RUN_FINISHED.usage`**（数组；没有的位不发 —— 发 0 会被客户端当真求和进去）。
- [x] **`event_transform`**：事件流插件的通用挂点（改写 / 吞掉 / 前后插事件），
  插件抛异常退回原事件。A2UI / genui / MCP Apps 那三个插件本身**没做** —— 挂点与
  `/info` 的位在了，不装就如实报 false。

新增 33 个测试（会话 10、HTTP 会话能力 6、apply_patch 6、协议解析 11 之外另有
runner 级 7 与 interrupt 协议 2）。验证：`rebar3 eunit` **708 tests 0 failures**
（此前 666）；编译零警告；dialyzer 只剩既有的 `beamai_mcp_server.erl:442`；
xref 无非 unused-export 警告。

### 已做·三个 activity 插件 + 子 agent lane + 协议补全（2026-09-04 第二批）

- [x] **`beamai_agui_plugin`（公共零件）**。三个插件干的是同一件事：盯住某个工具的
  调用，把参数翻成 activity 事件。`tool_watcher/1` 收口这一半：攒 `TOOL_CALL_ARGS`
  的片段（**单独一片解析不了** —— 那是 JSON 文本碎片），到 `TOOL_CALL_END` 一次
  解析。`hold_start` 决定要不要扣住工具卡片（生成式 UI 扣，A2UI 不扣，两边照各自
  上游的取舍来）；**扣住的一定在 END 放完** —— 终态不过 transform，扣着就是真丢了。
  `compose/1` 串多个插件。
  状态放**进程字典**：一次 run 就是一个进程（会话给每个 run spawn 一个），
  「按 run 隔离」由进程边界天然给到，不需要自己造一层；key 里带构造期的
  `make_ref()`，同进程挂两个插件也互不干扰。
- [x] **`beamai_agui_a2ui`**（移植 `@ag-ui/a2ui-middleware`）。`render_a2ui` 工具 +
  用法提示词 + **catalog**（A2UI v0.9 基础 catalog 的 18 个组件；catalog 既是模型的
  词汇表，也是「编不出别的组件」的保证）。事件是一条 `ACTIVITY_SNAPSHOT`
  （`replace: true`，所以不需要 delta），content 里三条 op 的**顺序是协议要求的**
  （createSurface → updateComponents → updateDataModel）。用户动作回流经
  `input_transform/0` 合成一条 user 消息 —— 上游把它塞成工具结果，我们历史是服务端
  权威塞不进去，**语义相同、路径不同**。
- [x] **`beamai_agui_genui`**（移植 CopilotKit 的 open-generative-ui 中间件）。
  三条猜错了前端不报错只会静默少渲染的细节都守住了：snapshot 必须最早且只一条、
  `add` 必须带 value（null 整条跳过）、`html` 是**数组**不是字符串。扣住工具卡片
  直到第一条 activity 出去。键序按约定重排（模型的原始 key 顺序在 provider 解析
  JSON 时就没了，而这个特性的提示词恰恰按顺序要求生成）。
- [x] **`beamai_agui_mcp_apps`**。判据同 clj：**协议认不认**。JSON-RPC / 握手 /
  传输在 `beamai_mcp`；这里只剩前端约定 —— 带 `_meta."ui/resourceUri"` 的工具出结果
  时补一条 `mcp-apps` activity（带 `toolInput`：那块界面要知道这次是用什么参数调的），
  以及 UI 代理的**四方法白名单**（界面跑在浏览器里）。哪些工具带 UI 在装配期算一次。
- [x] **`beamai_agui_subagent`：子 agent lane**。**没有改上游** ——
  `beamai_agent_delegate` 的 `subagent` 是个返回 agent 配置的函数，配置里可以有
  `callbacks`，包一层就够了；再包一层 handler 负责收口。`SUBAGENT_STARTED` →
  lane 事件（每条带 `subagentRunId`）→ `SUBAGENT_FINISHED` / `SUBAGENT_ERROR`。
  三个进程边界踩了一次才对：**父 emitter 的表是 private，工具进程读不到** ——
  所以 observer 交出去的是 thread id / run id / sink 三样，不是 emitter 本身；
  lane 的表要 `public`（回调在 manager 的 worker 进程里触发）。
  **缺省关**：`@ag-ui/client` ≤ 0.0.57 一条未知类型掐断整条流；而给既有事件多一个
  字段是安全的（schema 是 passthrough），所以只有三个新类型要开关。
  emitter 新增 `tag`（每条事件补字段）与 `access`（表可见性）两个选项。
- [x] **协议补全：非弃用的 29 个事件类型全部有构造器**。新增 `ACTIVITY_SNAPSHOT` /
  `ACTIVITY_DELTA`、`SUBAGENT_STARTED` / `FINISHED` / `ERROR`、
  `TEXT_MESSAGE_CHUNK` / `TOOL_CALL_CHUNK`。
  **`THINKING_*` 一族刻意不做**（0.0.59 起 deprecated、1.0 移除，思考走
  `REASONING_*`）。**两条 `*_CHUNK` 缺省不发**：它们是给「分不出边界的生产者」的
  便利事件，而我们的边界分得出，发显式三件套严格更好；构造器留着给宿主。
- [x] **`MESSAGES_SNAPSHOT` 真的发出去了**（此前只有构造器）。会话从事件流折出一份
  **消息账本**，断线重连补不回缺口时发它 —— 客户端丢的是增量，增量对不上之后它
  手里那份就是错的，`MESSAGES_SNAPSHOT` 正是协议里说「照这份对齐」的那条。
  账本**不是第二个真相店**：它是这条流自己的投影，有界（缺省 200 条），完整历史
  仍在 chat memory。子 agent lane 的事件不进账本。
- [x] **`input_transform` 入站挂点**（对称于 `event_transform`）：在解析之后、分路
  之前改写 `RunAgentInput`，A2UI 的用户动作回流走它。端点配置里的
  `event_transform` / `input_transform` 经 `run_opts` 一路传到 `runner:run/4`
  （会话模式与请求回落模式都走同一条路）。

### 已做·全能力走查 examples

- [x] **`examples/src/example_agui_scripted.erl`**：照剧本演的 LLM。四种幕
  （text / tool_call / reasoning_then_text / hang），tool_call 那幕**逐字**吐参数
  （这是 `beamai_agui_raw` 增量路唯一的输入形状）。**不需要 API key**。
- [x] **`examples/src/example_agui_full.erl`**：九个场景一条命令跑完，每个都下断言
  并把事件流打在屏幕上 —— `basic` / `reconnect`（**真断开**再凭水位接回来）/
  `stop` / `threads` / `hitl`（interrupt → `resume[]`）/ `a2ui` / `genui` /
  `mcp_apps` / `subagent`。首次跑通即 9/9。
  顺手记一条：Erlang 的 `io:format` 打中文要 `~ts` 且 `io:setopts([{encoding,
  unicode}])`，`~s` 直接 badarg；而**格式串是普通字符串，不能写 `/utf8`**
  （那是 binary 的语法）。

验证：`rebar3 eunit` **739 tests 0 failures**（此前 708）；编译零警告；dialyzer 只剩
既有的 `beamai_mcp_server.erl:442`；`example_agui_full:run()` 九个场景全过。

### 待办·功能

- [ ] **认证与限流**。抄 `beamai_a2a_auth` / `beamai_a2a_rate_limit`。
- [ ] **多实例部署**。会话在进程内，要么 sticky session，要么把缓冲挪到共享存储。
- [ ] **异步委派的 lane**（`management_tools`）。子 agent 在工具进程死后还在跑，
  那时 lane 的状态表已经回收 —— 要让 `beamai_subagent_manager` 透出观察钩子，
  那是上游的改动。
- [ ] **插件的中间态**。三个 activity 插件都在参数到齐时一次翻译；「边生成边长
  出来」要把 `TOOL_CALL_ARGS` 的片段做增量 JSON 扫描（clj 那边有一个现成的
  `scanner` 可照抄）。

### 待办·验证

- [ ] **对真实 CopilotKit 前端验证协议兼容性**。事件形状是照协议文档写的，
  还没被真客户端消费过。

### 已做·文档

- [x] [docs/design/agui_runtime.md](docs/design/agui_runtime.md) —— 设计与取舍
  （决策 + 理由，含刻意**没**做的部分）。
- [x] [docs/design/agui_runtime_EN.md](docs/design/agui_runtime_EN.md) —— 英文对照。


## 与上游统一 + 优化（2026-07-17）

> 目标：让 beamai_extra 与上游 beamai 在依赖/约定/OTP 版本上对齐，并清理结构。
> 所有改动**未提交**（beamai_extra + 上游各有改动，等你确认后统一提交）。

### 已修·有回归测试

- [x] **JSON-RPC 双重编码 bug**（你追问「为什么编码为 binary」时挖出）。
  `beamai_a2a_jsonrpc` 的错误构造器已返回编码好的 binary，而 a2a_server / http_handler
  的 5 个调用点又套一层 encode——客户端收到「装着 JSON 的 JSON 字符串」，畸形响应。
  MCP 侧不受影响（调用方直接发送、不再套）。修：去掉那 5 处多余编码。
  新增 `beamai_a2a_server_encoding_tests`（旧代码下 2 挂、修后过）。
- [x] **JSON-RPC batch 支持 + 约定统一（1→2→3 计划，2026-07-17）**。
  - **① A2A batch 回归测试**：A2A 早已支持 batch（`do_handle_request({batch,_})` 折叠、
    收集 map、边界一次性 encode），但无测试守着。新增 3 个
    （`beamai_a2a_server_tests`：数组形状 / 保序+id对应 / 单元素不退化）。
  - **② MCP batch 支持（此前完全没有）**。`beamai_mcp_server` 重构：把
    `do_handle_request` 的单请求核心抽成 map 产出的 `build_response/2`（用上游
    `beamai_jsonrpc` 的 map 构造器 + `custom_error/4`），编码统一在 `do_handle_request`
    边界完成；新增 `{batch,_}` 子句（逐个 build → 收集 map → **一次性** encode 成 JSON
    数组，避免元素被二次编码成字符串）；通知无 id 不产响应、全通知 → `no_response`
    （handler/cowboy 回 202）、空数组 → invalid_request。新增 5 个
    `beamai_mcp_server_tests`（数组保序 / 跳过通知 / 全通知no_response / 空数组 / 错误成功混合）。
  - **③ A2A jsonrpc 错误构造器统一返回 map**（消除文档化的「两套约定相反、不能一律
    encode」易错混用）。`beamai_a2a_jsonrpc` 9 个错误构造器去掉 encode 包装、返回 map；
    http_handler(3处)/server(2处) 改为在 HTTP 边界统一 `encode_json`；jsonrpc 测试改为
    直接断言 map。**MCP 侧刻意不动**：MCP server 成功路径在自己边界已 encode 成 binary，
    若把 mcp_jsonrpc 也翻成 map 反而会在 handler 里重新制造「binary 成功 + map 错误」的
    混用；MCP 现状（server=map内部→边界encode，decode失败路径=binary 构造器）本就自洽。
- [x] **`grep_files/4` O(n²) → 线性** + 顺带修正**反序输出** bug。新增 grep 行为测试。
- [x] **三份 `generate_session_id/0` 去重** → `beamai_mcp_types:new_session_id/0`。
- [x] **三份 `get_header/2` 去重** → `beamai_mcp_types:get_header/2`。
- [x] **手写 ID 生成统一 + 进制笔误**（`~.8b` 8 进制、`~.4b` 4 进制）→ `beamai_id:gen_id/1`。
- [x] **a2a SSE 流路由到 `http_pool_stream`**（后端感知，仅 Gun 注入）。3 个 meck 测试。

### 已做·结构/依赖统一

- [x] **删除 hackney，统一 Gun 后端**。删两个重复的 hackney MCP 传输模块
  （`transport_http` / `_sse`），选择器只留 Gun，修过期文档（注释说默认 hackney、实际 Gun）。
  从 mcp/a2a app.src 移除 hackney。rebar.config 的 hackney 直接依赖已由协同改动移除
  （仍作 beamai_core/llm 传递依赖，核实过干净重 fetch 仍解析成功）。
- [x] **删除 735 行死代码** `beamai_tool_provider_mcp` + `beamai_mcp_tool_proxy`
  （无引用、文档引用不存在的 API、与已测试的 `beamai_mcp_adapter` 重叠；不能当测试用例）。
  连带清空 `src/provider/` 目录与 rebar.config 的 src_dir。
- [x] **jsx → OTP 27 stdlib `json` 全量迁移**（与上游统一）。
  - src 38 处 + test 51 处：`jsx:encode(X[, [])` → `beamai_utils:encode_json(X)`（保 binary 契约）；
    `jsx:decode(X, [return_maps])` → `json:decode(X)`；`jsx:is_json`（2 处）→ `try json:decode`。
  - 删 jsx 依赖（rebar.config + 4 个 app.src + 锁），加 `{minimum_otp_vsn, "27"}`。
  - examples 同步删 jsx/hackney 依赖与启动。
  - 全项目 src+test **jsx 归零**，jsx 不再被 fetch。分三步验证（迁 src→测试仍用 jsx 独立验证
    产出一致→迁 test→删依赖），每步 520 测试全过。

### 升级适配（前置）

- [x] **上游 HTTP 连接池拆分（short/stream/longpoll）**：升级零破坏，门面签名未变、
  三池自动启动、不配有缺省。beamai_extra 只经 `beamai_http` 门面调用、不碰内部破坏面。

### 已验证·未修（结构，仍待决策）

- [ ] **两个 JSON-RPC 模块 17 个委托重复**。细看后：这些封装在上游 map 之上加了「编码成
  binary」，**不是纯样板**；合并要么造跨 app 耦合、要么参数化，低价值。**建议不动。**
- [x] ~~**`beamai_a2a_task` 的 `++[X]` 累积**（O(n²)）~~ **已修（2026-07-17）**。
  三处 `List ++ [X]`（messages/history/artifacts）改为 **newest-first prepend**（O(1)），
  唯一读出口 `state_to_map/1` 统一 `lists:reverse`。批量产出物路径用
  `lists:reverse(Batch) ++ Existing`（成本只与批量大小相关）。核实过读者只有
  `state_to_map`（record 私有、跨模块只经 get_task 拿到已还原的时间序 map），无副作用。
  新增 4 个顺序回归测试（多条 add 保序 / init 消息仍居首 / 批量产出物保序）；
  既有 `status_history_test` 作为翻转守卫仍绿。556 测试全过、Dialyzer 无新增警告。

### 新发现·待清理 —— 已核实为**误报**，无需处理

- [x] ~~`beamai_a2a_utils` 一批 unused 函数~~ **误报**。核实：`safe_get`/`safe_merge`/
  `paginate`/`filter_by_time`/`validate_binary`/`validate_map`/`safe_execute` 等在全项目
  **根本不存在**（0 处）——迁移时那批警告是并发修改中途的瞬时状态，干净全编**零警告**。
  xref 另报的真实 unused export（`a2a_utils:generate_id/timestamp`、`a2a_auth:validate_key`、
  `a2a_card:validate`、`a2a_card_cache:invalidate`）均**被测试引用**（3~7 处/个），
  是有测试的 public API，非死代码，保留。

### 上游改进建议（未提）

- [ ] core 的 `beamai_http` 门面无后端感知池辅助（逻辑困在 llm 层 `maybe_inject_pool/3`）。
  建议上游提升到 core，非 LLM 下游（a2a/mcp）就不必复刻那 4 行门控。非 bug。

### 测试覆盖缺口 —— 已补

- [x] **`beamai_a2a_handler`**（JSON-RPC 分发器，此前零直接覆盖）→ `beamai_a2a_handler_tests`
  11 个：dispatch 路由（未知方法 -32601）、5 个方法缺 taskId → -32602、未知任务 -32001、
  Context 错误路径不改。覆盖不依赖 agent/task/push 进程的路径。
- [x] **`beamai_a2a_server`**（gen_server 层）→ `beamai_a2a_server_tests` 5 个：agent card、
  handle_json / handle_request 经「JSON 解码 → dispatch」链路（-32601 / -32001 / -32700）。
  （成功路径需 LLM/进程，由 input_required / http_handler 集成测试承担。）
- [x] **`beamai_rag_utils`**（此前零覆盖）→ `beamai_rag_utils_tests` 8 个：类型转换、向量数学
  （dot_product/vector_norm/safe_divide 边界）、map/列表工具。
  （`beamai_vector_store` 已有 `beamai_vector_store_tests`，非零覆盖。）
- 单元测试总数 520 → **544**，全过。

### Live 冒烟测试（保留，未提交）

> 真连外部服务的端到端验证——把 jsx→json 迁移 + gun 后端 + 连接池串起来对真实服务跑通。
> 需相应环境/服务；examples 下手动运行，非 `rebar3 eunit` 的一部分。

- [x] **`examples/src/live_minimax.erl`** — 真连 MiniMax（anthropic 兼容，需 `MINIMAX_API_KEY`）。
  三段全过：原始 chat、agent 单轮、agent + 工具调用循环。
  验证请求/响应/工具 schema 的 json 编解码 + gun HTTP + 池路由 + tool_calls 回灌。
- [x] **`examples/src/live_embedding.erl`** — 真连本地 Qwen3-Embedding
  （llama.cpp OpenAI 兼容，1024 维，`http://192.168.186.1:8080/v1/embeddings`，无需 key）。
  三段全过：单条 embed_text、批量 embed_batch、余弦相似度语义
  （猫-狗 0.80 > 猫-股市 0.27）。验证 embedding 路径 json 编解码 + 向量数学。
  换服务改文件顶部 `?BASE_URL` / `?MODEL` / `?DIM` 三个宏即可。

---

## 缺陷追踪（2026-07-16 排查）

> 本轮配合上游 beamai 升级做的缺陷排查。**严格区分三档**：
> `已修` = 改了 + 有回归测试（原始代码下测试挂、修后过）；
> `已验证·未修` = 亲手 probe 复现过、但未动手（多为需先定语义或成组待做）；
> `未验证` = 仅来自一次子 agent 扫描的说法，**我没有独立核实过**，动手前须先复现。
>
> 所有已修项在两个仓库里均**未提交**（beamai_extra + 上游 beamai 各有改动）。

### 已修（有回归测试）

**中间件（beamai_tools）**
- [x] `middleware_model_retry` / `model_fallback` 错误分类：改走 `beamai_llm_error:from_reason/1`。
  原来按硬编码原子列表 + 文本子串匹配，对真实错误形状（`{http_error,429,_}` 等）全部漏判，
  重试/降级从不触发；并采纳服务端 `Retry-After`。
- [x] `middleware_tool_retry`：工具失败走 throw（非返回值），原实现 `extract_error` 永不命中，
  重试从未发生；改为 catch throw + 放弃时 re-throw 保持 ChatClient 语义；默认 `retryable_errors => auto`
  经 `beamai_tool_error:classify/1` 只重试 transient（原默认 `all` 会连语义错误一起重试）。
- [x] `middleware_human_approval`：selective 模式取的是 tool_spec map 而非工具名，审批从不触发；
  默认 `mode` 由 none 改 all（fail-closed）；`call_with_timeout` 用 spawn_monitor（原 spawn 崩溃干等 60s）；
  拒绝结果自行 encode 成 JSON（原样漏 Erlang 项式给模型）。
- [x] `middleware_call_limit`：halt 不 halt——短路返回错误值被 ChatClient 当成功透出，agent 收到空答案；
  改为 throw 收敛成 `{error, {limit_exceeded, _}}`。工具侧 `max_tool_calls` / `max_tool_calls_per_turn`
  **删除**（filter 拿批内只读快照，计数累加不起来，见下方上游 `max_tool_calls`）。
- [x] `beamai_middleware_runner`：`around_turn` 钩子丢弃状态——turn 响应是 tuple，
  返回 `{Resp, State}` 会泄漏成响应崩掉 `dispatch_turn_result`。

**工具超时（beamai_tools）**——上游缺省已翻 infinity（c8dca82/0f73809），不声明即无限等待
- [x] file/todo 工具显式声明 timeout（10s/30s/5s）
- [x] `beamai_tool_shell`：`spawn_link` + `exit(Pid,kill)` 会**杀死自己的调用者**（killed 顺链传回），
  改 spawn_monitor；缺省 infinity（长命令跑到底）；`collect_output` 写死的 60s 改跟随请求 deadline
- [x] `beamai_tool_human`：删除阻塞式 handler，改为 interrupt tool（`interrupt_tools/0`），
  经 agent 的 interrupt/resume 等真人；从 `beamai_tools:available/0` 移除

**RAG（beamai_rag）**
- [x] `beamai_rag_splitter` overlap 按字节切断 UTF-8——中文每字 3 字节，除首块外全是乱码
  （实测 4 块 3 坏），喂进 embedding 全废；对齐到字符边界
- [x] splitter 末尾重叠被当独立块输出（上一块尾部纯复制，重复入库）
- [x] `beamai_rag:call_llm`：`catch _:_ -> {ok, 假答案}` 把异常吞成带 `{ok,_}` 标签的假答案
  （无 API key 时回显提示词当回答）；改为如实上抛

**MCP 客户端（beamai_mcp）**——572 行 gen_statem，此前零测试
- [x] **永远连不上**：`{response,_}` 只在 connected 状态产生，进 connected 又需先收到响应——
  死锁。任何传输任何 server 都在 connecting 干等到 init_timeout 掉 disconnected。
  在 connecting 里就武装 recv_loop
- [x] transport 失败崩客户端：`connecting(enter)` 返回 `{next_state,_}`，enter 不允许改状态 →
  `bad_state_enter_return` 穿透 start_link；连接动作挪到 state_timeout 事件
- [x] `disconnected` 缺 state_timeout / 迟到 info 子句 → function_clause；`connected` 缺 info /
  未识别 call 兜底（对已连接客户端调 initialize 就崩）

**MCP 服务端（beamai_mcp）**——Streamable HTTP + SSE 此前均不工作
- [x] **每请求新建 server**：session id 读了回显了但从没查过——initialize 之后一切方法 -32600
  "Server not initialized"。新增 `beamai_mcp_session_registry`（session→server 映射，
  monitor + TTL 清扫），cowboy 适配层按 session id 跨请求分发；补 DELETE 终止会话
- [x] **SSE Server 模式**：`handle_sse_message` 是 TODO 存根，endpoint URL 不带 session id，
  POST 响应永远回不来。GET 起会话级 server + endpoint 带 `?session_id=` + 登记 loop 进程；
  POST `/message` 找回会话分发、响应 push 进 SSE 流
- [x] 工具返回裸值 `try...of` 崩 server（try_clause，catch 接不住 of 分支）→ 归一 isError
- [x] `resources/subscribe`/`unsubscribe` 缺 uri → function_clause 崩 server + Cowboy → 补兜底
- [x] session id 弱随机（时间戳 + 16bit rand，`~.4b` 还是 4 进制）→ crypto 强随机 128bit
- [x] `rebar.config` 的 `src_dirs` 列了不存在的 `src/primitives`

**上游 beamai**
- [x] 新增 agent `max_tool_calls`（整轮工具调用总数上限，缺省 infinity）——工具侧限额只能在
  串行的 tool loop 层做，filter 的批内快照做不到。**注意配套测试 `beamai_max_tool_calls_tests.erl`
  仍是 staged 未进提交**

### 已验证·未修（亲手复现过）

- [x] ~~**RAG `chunk_size` 不是硬上限**~~ **已修（第三轮，见下方已修区）**
  用户决策：硬切片段间**保留 overlap**，**不插 separator**（原文此处本就没有分隔符）。

### 已修·有回归测试（2026-07-16 第二轮）

**RAG 向量运算（beamai_rag）**——此前 beamai_embeddings / beamai_vector_store 零测试
- [x] `beamai_embeddings:cosine_similarity/2`、`euclidean_distance/2` 无兜底子句：向量不等长
  （含空 embedding `[]` 文档参与搜索）直接 `function_clause`。补兜底子句（cosine→0.0，euclidean→1e308）。
- [x] `beamai_vector_store:search` 遇空 embedding 文档崩：`add_document` 默认 `embedding => []`，
  存一篇没 embedding 的文档后任何 search → `function_clause`（根因是上一条）。
  `score_single_document` 增加 `has_valid_embedding/1` 前置检查，跳过空 embedding 文档。
- [x] `beamai_embeddings:generate_hash_embedding/2` 的 `rand:seed/2` 污染调用者进程字典：
  改用 `rand:seed_s/2` + `rand:uniform_s/1`（纯函数式，不动进程字典）。回归测试验证不污染。

**RAG 文本分割（beamai_rag）**——`beamai_rag_splitter` 此前测试覆盖不足
- [x] `chunk_size` **不是硬上限**：判断在 append 之后，单段超长不被切开。
  `size=10` 喂 26 字节无换行文本 → 旧实现输出单个 26 字节块，撑爆 embedding token 上限。
  **用户决策（2026-07-16 第三轮）**：硬切片段间**保留 overlap**，**不插 separator**
  （原文此处本就没有分隔符）。
  实现：`merge_loop` 在 append 之前检查溢出上限；超长段落走 `hard_cut_segment/3`，
  复用现有的 `extract_overlap` + `align_to_char_boundary` 保证 UTF-8 安全，新增
  `backward_align` 处理切割点字符边界。新增 6 个回归测试覆盖 ASCII、中文、混合场景。
  旧有 `chunk_size_is_not_yet_enforced_test`（钉死旧缺陷行为）与 `trailing_overlap_is_not_emitted_as_chunk_test`
  按新行为修正。

**MCP 传输层（beamai_mcp）**——未验证项复现后修复
- [x] `beamai_mcp_transport_http_gun` / `beamai_mcp_transport_sse_gun` 的 `parse_url`：
  `maps:get(path, Parsed, <<"/">>)`——`uri_string:parse` 对无 path 的 URL 返回 `path => <<>>`
  （键存在但值为空），默认值失效 → 空请求行。补 `<<>> -> <<"/">>` 兜底。
- [x] `beamai_mcp_transport_sse_gun:find_endpoint_event`：endpoint 事件数据按 JSON 解析
  （`jsx:decode`），但 MCP 规范发纯 URI 字符串 → `badarg`。改为先试 JSON（自家服务端格式），
  失败则作为原始 URI 字符串（规范格式）。同时补相对 URI 基于 SSE base URL 的解析
  （`uri_string:resolve/2`）。

**上游 beamai HTTP 后端（beamai_core）**
- [x] `beamai_http_pool` + MCP 三个传输的 `protocols => [http2, http]`：Gun 2.1 的 TCP 连接路径
  `[Protocol] = maps:get(protocols, Opts, [http])` 是单元素匹配，传两个元素直接
  `{badmatch, [http2, http]}` 崩——**所有 HTTP 调用（embedding、LLM）全走不通**。
  改为 `[http]`（HTTP/1.1），HTTP/HTTPS 均可用。MCP 三个传输同修。

### 未验证（仅子 agent 说法，动手前须先复现）

> 以下已在 2026-07-16 第二轮排查中逐一核实。

- [x] ~~客户端 SSE 传输连规范 server：endpoint 事件按 JSON 解析，但规范发纯 URI 字符串 → jsx `badarg`；
  相对 URI 从不对 `sse_url` 解析 → `{error, invalid_url}`。~~ **已修**（见上方已修区）
  （注：自家服务端现在发 `{"uri":...}` JSON，与自家客户端对得上，故自连自没问题）
- [x] ~~`http_gun`/`sse_gun`：`maps:get(path, Parsed, <<"/">>)`——`uri_string:parse` 对无 path 的 URL
  返回 `path => <<>>`（键存在），默认值失效 → 空请求行~~ **已修**（见上方已修区）
- [x] **`beamai_mcp_types` 发 `list_changed`，规范是 `listChanged`** → **排除（FALSE POSITIVE）**。
  MCP 规范通知方法名确实是 `notifications/tools/list_changed`（snake_case），而 capability 字段才是
  `listChanged`（camelCase）。`beamai_mcp.hrl` 的宏和 `beamai_mcp_client.erl:510` 均正确使用各自格式。
  子 agent 把通知方法名和 capability 字段名搞混了。
- [x] ~~`beamai_embeddings` 的 `rand:seed/2` 污染调用者进程字典~~ **已修**（见上方已修区）
- [ ] stdio 传输 `stderr_to_stdout` 把 server 日志混进 JSON 流（**stdio 暂不考虑，可忽略**）

### 测试覆盖缺口（非 bug，无测试）—— 已于 2026-07-17 补齐，详见顶部小结

- [x] ~~`beamai_a2a_server` / `beamai_a2a_handler` 零覆盖~~ 已补（handler 11 + server 5 测试）
- [x] ~~`beamai_rag`：`embeddings` / `vector_store` / `rag_utils` 零覆盖~~
  已补（rag_utils 8 测试；embeddings/vector_store 本已有测试文件）+ live embedding 冒烟

### 疑似死代码

- [x] ~~`beamai_tool_provider_mcp`、`beamai_mcp_tool_proxy`~~ **仍待处理**（735 行，仅 app.src 引用、
  无代码/测试引用；前者文档引用不存在的 `beamai_tool_provider` behaviour 和 `beamai_tools:get_tools/2`）。
  2026-07-17 复核仍确认为死代码；删除 vs 保留为外部 API 需产品决策，暂未动。

---

## 优化与升级适配（2026-07-17）

### 已修·有回归测试（结构优化）

> 检查代码找可优化点。结论：无算法/性能热点（扫过 `length==0` 判空、循环内
> `Acc++[X]`、循环内 `lists:member` —— 均无真实 offender），优化空间在结构层。

- [x] **`beamai_tool_file:grep_files/4` 真 O(n²) → 线性**（唯一的算法优化）。
  每轮 `Acc ++ lists:sublist(...)`（尾追加 O(n)）+ 两次 `length(Acc)`（O(n)），
  对文件列表整体二次。改 running counter + 反向累积、末尾一次 reverse。
  **顺带修潜在 bug**：旧实现前向追加后又整体 `lists:reverse`，输出其实是**反序**的
  （最后一条匹配排最前）；新版是正确的文件序 + 行号递增。此前零行为测试，
  新增 `beamai_tool_file_grep_tests`（4 例，锁正序 + max_results 截断）。
- [x] **三份逐字节相同的 `generate_session_id/0` → 一处**。
  `cowboy_handler` / `handler` / `server` 各一份 crypto 强随机代码，
  收敛到 `beamai_mcp_types:new_session_id/0`。
- [x] **三份逐字节相同的 `get_header/2`（大小写不敏感取头）→ 一处**。
  两个 HTTP 传输 + handler 各一份，收敛到 `beamai_mcp_types:get_header/2`。
  （`beamai_a2a_middleware` 那份**行为不同**——只小写 header 名、要求调用方传已小写的
  Name——未并入，避免误改语义。）

### 已验证·未修（结构，需你拍板）

- [x] ~~**hackney vs gun 两套传输后端并存**~~ **已解决（2026-07-17）**：hackney 传输
  已随上游 `beamai_http_hackney` 一并摘除，`apps/beamai_mcp/src/transport/` 只剩
  `_http_gun`/`_sse_gun`/`_stdio`，`rebar.config` / `beamai_mcp_transport.erl` 里仅存
  说明移除的注释。不再并存。
- [ ] **两个 JSON-RPC 模块的委托重复**（`beamai_a2a_jsonrpc` vs `beamai_mcp_jsonrpc`）。
  两者的通用委托（encode/decode/类型检查）逐字相同，但**错误构造器现约定不同**：
  a2a 返回 **map**（边界统一 encode），mcp 返回 **binary**（decode 失败路径直接作 body）。
  这是各自 app 用法决定的（见上方 ③ 的说明），不是可无脑合并的重复；若要合并需把
  「返回 map / 返回 binary」参数化，中等价值、低优先。
- [x] ~~**手写 ID 生成散落 + 进制笔误**~~ **已修（2026-07-17）**。
  `a2a_convert:generate_message_id`（`~.8b` 8 进制）、`a2a_client:generate_request_id`
  （`~.4b` 4 进制，`rand:uniform(16#FFFF)` 低熵）、`tool_todo:generate_id`（非 crypto rand）
  三个改为委托 `beamai_id:gen_id/1`（crypto 强随机 + 时间戳），保留语义命名的薄封装、
  call site 不动。核实过无格式依赖（无代码解析 req-/msg-/todo_ 前缀，request_id 是 JSON-RPC
  opaque id）。`a2a_utils:generate_id`（标准 UUID v4 + crypto，本身没问题）未动——它属"疑似死代码"另议。
  323 个受影响 app 测试全过。
- [x] ~~**`beamai_a2a_task` 的 `Messages/history ++ [X]` 累积**~~ **已修（2026-07-17）**，
  详见顶部「与上游统一 + 优化」小结：newest-first prepend + `state_to_map` 单点 reverse，
  4 个顺序回归测试守着。

### 上游 HTTP 连接池升级（1c6f24d..faaadb0）—— 适配

> 上游把单例 HTTP 池拆成三个命名池：`http_pool_short`（短请求）/ `http_pool_stream`
> （SSE 流）/ `http_pool_longpoll`（长轮询）。

- [x] **升级零破坏，无需为兼容改动**。三下游 API 面（`beamai_http:*` /
  `beamai_chat_model:*` / `beamai_llm_http_client:*`）全向后兼容，`pool` 选项本就存在
  且可选；三池由 `beamai_core_sup` 自动启动，`http_pools` 不配也有可用缺省。
  破坏性变更只在 beamai_core 内部（`beamai_http_pool` API 加池名首参、
  `beamai_http_gun:request_async/5` 返回值变 4 元组），beamai_extra 只经门面调用、不碰这些。
  升级后 515 测试直接通过。
- [x] **采纳：a2a SSE 流路由到 `http_pool_stream`**（`beamai_a2a_client:send_message_stream`）。
  此前默认走 short 池——正是池拆分要隔离的场景（长流不占短请求池连接）。
  **关键陷阱**：Hackney 后端把 `pool` 键当自己的池名，传 Gun 池名会指向不存在的池；
  故本地复刻上游 `beamai_llm_http_client:maybe_inject_pool(stream,...)` 的后端门控
  （仅 Gun 注入，显式 pool 优先）——不给 a2a 加 llm 依赖。3 个 meck 测试锁三路径。

### 上游改进建议（未提，供参考）

- [ ] core 的 `beamai_http` 门面**没有**后端感知的池选择辅助，那段逻辑只在上层
  `beamai_llm_http_client:maybe_inject_pool/3`——门面注释反而指向上层函数。任何非 LLM 下游
  （a2a、将来的 mcp）想正确路由都得复刻那 4 行门控。建议上游把它提升到 core
  `beamai_http`（如 `stream_pool_opt/2`）。非 bug，是分层改进。

---

## Project Restructuring (2026-02-08)

### Completed: Core Library Extraction
- [x] Removed `apps/beamai_core/` - now depends on external beamai
- [x] Removed `apps/beamai_llm/` - now depends on external beamai
- [x] Removed `apps/beamai_memory/` - now depends on external beamai
- [x] Updated `rebar.config` to depend on `https://github.com/TTalkPro/beamai.git`
- [x] Updated project documentation (README.md, README_EN.md)

### Current Project Focus

The project now focuses on extensions to the core BeamAI library:

**Core Dependencies (from BeamAI):**
- beamai_core - ChatClient, Process Framework, Graph Engine, HTTP, Behaviours
- beamai_llm - LLM Chat Completion, Output Parser, Providers
- beamai_memory - Memory Store, Snapshot, Checkpoint

**Project Extensions:**
- beamai_tools - Tool system and Middleware
- beamai_agent - Simple Agent implementation (ReAct pattern)
- beamai_deepagent - Deep Agent (SubAgent architecture)
- beamai_a2a - Agent-to-Agent protocol
- beamai_mcp - Model Context Protocol
- beamai_rag - Retrieval-Augmented Generation

---

## Future Enhancement Tasks

### Phase 1: Documentation Cleanup
- [ ] Update docs/ARCHITECTURE.md for new structure
- [ ] Update docs/API_REFERENCE.md for new structure
- [ ] Update docs/DEPENDENCIES.md for new structure
- [ ] Create migration guide for users

### Phase 2: Examples Verification
- [ ] Test all examples with new dependency structure
- [ ] Update example code if needed
- [ ] Add examples showcasing integration with external beamai

### Phase 3: Feature Enhancements (Project Specific)
- [ ] beamai_tools: Add more middleware options
- [ ] beamai_agent: Enhance callback system
- [ ] beamai_deepagent: Improve parallel execution
- [ ] beamai_a2a: Add more protocol features
- [ ] beamai_mcp: Enhance server/client capabilities
- [ ] beamai_rag: Improve vector store integration

### Phase 4: Testing & Quality
- [ ] Ensure all tests pass with external beamai dependency
- [ ] Add integration tests for beamai_extra apps
- [ ] Performance testing with new structure
- [ ] Dialyzer type checking

---

## Notes

### Working with External BeamAI Dependency

When developing beamai_extra:
1. Core functionality (ChatClient, Process, Graph, LLM, Memory) comes from external beamai
2. Focus on agent implementations, protocols, tools, and RAG
3. Coordinate with beamai repository for any core changes needed
4. Keep beamai_extra compatible with latest beamai main branch

### Development Workflow

```bash
# Compile (will fetch beamai dependency)
rebar3 compile

# Start shell
rebar3 shell

# Run tests
rebar3 eunit

# Run tests for specific app
rebar3 eunit --app=beamai_agent
```
