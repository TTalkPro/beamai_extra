# BeamAI Extra - Task List

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
  重试从未发生；改为 catch throw + 放弃时 re-throw 保持 kernel 语义；默认 `retryable_errors => auto`
  经 `beamai_tool_error:classify/1` 只重试 transient（原默认 `all` 会连语义错误一起重试）。
- [x] `middleware_human_approval`：selective 模式取的是 tool_spec map 而非工具名，审批从不触发；
  默认 `mode` 由 none 改 all（fail-closed）；`call_with_timeout` 用 spawn_monitor（原 spawn 崩溃干等 60s）；
  拒绝结果自行 encode 成 JSON（原样漏 Erlang 项式给模型）。
- [x] `middleware_call_limit`：halt 不 halt——短路返回错误值被 kernel 当成功透出，agent 收到空答案；
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

### 测试覆盖缺口（非 bug，无测试）

- [ ] `beamai_a2a`：有 12 个测试，但 `beamai_a2a_server` / `beamai_a2a_handler`（agent 集成）零覆盖
- [ ] `beamai_rag`：`beamai_embeddings` / `beamai_vector_store` / `beamai_rag_utils` 零覆盖

### 疑似死代码

- [ ] `beamai_tool_provider_mcp`、`beamai_mcp_tool_proxy`：只在 app.src 列出，无代码引用；
  前者文档还引用不存在的 `beamai_tool_provider` behaviour 和 `beamai_tools:get_tools/2`。
  确认后删除或接线

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
- beamai_core - Kernel, Process Framework, Graph Engine, HTTP, Behaviours
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
1. Core functionality (Kernel, Process, Graph, LLM, Memory) comes from external beamai
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
