# feedbacks —— 下游宿主报上来的账

一条一个文件，`<日期>-<slug>.md`。**只记「beamai_agui 这一侧该改」的**：下游自己
绕过去了但绕法很蠢、或者根本绕不过去的那些。下游自己的 bug 不进这里。

每份的格式（症状先行，病因其次，建议最后）：

| 段 | 写什么 |
|---|---|
| **症状** | 下游看到的**那一行**（报错原文 / 或者「一个字都不报」） |
| **位置** | `文件:行`，beamai_agui 这一侧的 |
| **怎么撞上的** | 最短复现路径 |
| **影响面** | 谁会撞、撞了会怎样、**报不报错** |
| **建议** | 可选，下游不替仓主拍板 |

## 现有

| # | 标题 | 报的人 | 严重度 | 回没回 |
|---|---|---|---|---|
| [2026-09-04](2026-09-04-run-endpoint-is-not-per-agent.md) | run 端点在 `{base}/run`，而 `/info` 宣告了 `agents` 字典 ⇒ 客户端按 AG-UI 约定拼出的 `{base}/agent/{id}/run` **404**；而 404 那条路由不带 CORS 头，浏览器把它报成 **CORS 错**，指错方向 | happy | 🔴 整台运行时在标准客户端上连不上 | ✅ 已修 |
| [2026-09-04](2026-09-04-resume-decision-ignored-and-tool-result-fabricated.md) | `resume[].payload.decision` **完全不读**：`approved` 与 `rejected` 产出**逐字相同**的 `TOOL_CALL_RESULT content:"resolved"`，模型两次都说「已经打过招呼啦」。而那条结果是**替客户端工具伪造的** —— 客户端从头到尾没执行过它 | happy | 🔴 不报错；审批形同虚设，前端工具从未真正委派 | ✅ 已修 |
| [2026-09-04](2026-09-04-thread-read-surface-off-spec.md) | 线程只读面四处与 AG-UI 对不上：`GET /threads/:id` 忽略路径参数恒回整张表、记录用 `threadId` 而非 `id` 且缺 `name`/`createdAt`、`/messages` 404 而 `/info` 报 `inspect:true`、`/events` 的元素被包成 `{event,seq}` | happy | 🟡 会话抽屉实测一条都列不出（200 但 0 行） | ✅ 已修 |
| [2026-09-04](2026-09-04-protocol-nits-three.md) | 三条小的：`STEP_FINISHED` 落在**未闭合的**消息/工具生命周期中间；`CUSTOM name:"interrupt"` 与 `outcome.interrupts` 重复一份；`/info` 没有 `multimodal` 能力格（缺席=不知道=关，附件入口不会出现） | happy | 🟢 不挡事 | ✅ 已修 |
| [2026-10-22](2026-10-22-mcp-http-client-remote-servers.md) | `beamai_mcp_client` 连不上任何远端 streamable-HTTP MCP：轮询用 `recv(0)`，传输层 `after 0 -> gun:cancel` 把刚发出的 initialize 当场取消，迟到的 `gun_error badstate` 落进 `connecting/3`（无兜底 info 子句）⇒ **`function_clause` 崩进程**。顺带：`tls_opts` 无入口（今天靠 gun 2.x + OTP 27 的隐式默认才是验证的）；`beamai_mcp_adapter` 把原始 JSON Schema 塞进 `parameters`、handler 返回裸值，beamai_core 分别 `function_clause` / `case_clause` | beamai_muse | 🔴 远端 MCP 完全不可用，崩而不报错 | ✅ 已修 |

> 背景：happy（ClojureScript + UIx 的 AG-UI 客户端）2026-09-04 拿 `beamai_agui/1.1`
> 做联调后端，逐页对着 CopilotKit 的参照实现比，也与另外两台第三方 AG-UI 运行时
> （一台 Clojure、一台 Scheme）横向对照。上面几条是那趟路上留下的账。

## ⭐ 先说做得好的几处（不进上面那张表，但值得记）

* **`GET /threads/:id/stream` + `Last-Event-ID` 真能续传** —— 实测带
  `Last-Event-ID: 3` 打过去，服务端从第 4 帧接着发。⭐ 另外两台参照运行时**都没有**
  这条路（其中一台明说「长回答途中刷新会丢后半段，不粉饰」）。`POST /threads/:id/stop`
  同理：手上只要有 threadId 就停得掉。这两条是这台运行时的**长板**。
* **run 与 HTTP 连接解耦**（会话模式）—— 上面那条能成立的前提，设计上是对的。
* **每帧都带 `id:`**，且 CORS 把 `last-event-id` 放进了 `Allow-Headers`、
  把 `x-agui-run-id` / `x-agui-since` 放进了 `Expose-Headers` —— 这一套是齐的。
* **run 一开始就主动发 `STATE_SNAPSHOT {}`** ⇒ 后续 delta 一定打得上。
  ⭐ 这一格另外两台**都是被我们报了才补上的**，你们一上来就是对的。
* **`GET /threads/:id/state` 把 delta 折进去了**，回的是当前状态而不是「最后一份快照」。
* `RUN_FINISHED` 带 `usage`。
