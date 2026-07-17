# BeamAI Extra - Task List

## 与上游统一 + 优化（2026-07-17）

> 目标：让 beamai_extra 与上游 beamai 在依赖/约定/OTP 版本上对齐，并清理结构。
> 所有改动**未提交**（beamai_extra + 上游各有改动，等你确认后统一提交）。

### 已修·有回归测试

- [x] **JSON-RPC 双重编码 bug**（你追问「为什么编码为 binary」时挖出）。
  `beamai_a2a_jsonrpc` 的错误构造器已返回编码好的 binary，而 a2a_server / http_handler
  的 5 个调用点又套一层 encode——客户端收到「装着 JSON 的 JSON 字符串」，畸形响应。
  MCP 侧不受影响（调用方直接发送、不再套）。修：去掉那 5 处多余编码。
  新增 `beamai_a2a_server_encoding_tests`（旧代码下 2 挂、修后过）。
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
- [ ] **`beamai_a2a_task` 的 `++[X]` 累积**（O(n²)）。单任务生命周期内量级小、prepend+读时
  reverse 会翻转存储序影响读者，churn/风险 > 收益。**建议不动。**

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

### 测试覆盖缺口

- [ ] `beamai_a2a_server` / `beamai_a2a_handler`（agent 集成）零覆盖。
- [ ] `beamai_rag`：`beamai_vector_store` / `beamai_rag_utils` 覆盖仍薄。

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

- [ ] **hackney vs gun 两套传输后端并存**（~1300 行，`transport_sse`/`_sse_gun`、
  `transport_http`/`_http_gun`）。两套都是活的（经 `beamai_mcp_transport:get_*_module/1`
  运行时选，默认 gun）——**是重复，不是死代码**。合并是大重构非删除，取决于是否长期
  维护两个后端。
- [ ] **两个 JSON-RPC 模块 17 个委托重复**（`beamai_a2a_jsonrpc` vs `beamai_mcp_jsonrpc`）。
  编码步骤有差异（a2a `jsx:encode` vs mcp `encode_map`），合并需参数化，中等价值。
- [x] ~~**手写 ID 生成散落 + 进制笔误**~~ **已修（2026-07-17）**。
  `a2a_convert:generate_message_id`（`~.8b` 8 进制）、`a2a_client:generate_request_id`
  （`~.4b` 4 进制，`rand:uniform(16#FFFF)` 低熵）、`tool_todo:generate_id`（非 crypto rand）
  三个改为委托 `beamai_id:gen_id/1`（crypto 强随机 + 时间戳），保留语义命名的薄封装、
  call site 不动。核实过无格式依赖（无代码解析 req-/msg-/todo_ 前缀，request_id 是 JSON-RPC
  opaque id）。`a2a_utils:generate_id`（标准 UUID v4 + crypto，本身没问题）未动——它属"疑似死代码"另议。
  323 个受影响 app 测试全过。
- [ ] **`beamai_a2a_task` 的 `Messages/history ++ [X]` 累积**。gen_server 每次加一条 O(n)，
  任务生命周期内 O(n²)。量级取决于单任务消息数；prepend+读时 reverse 会翻转存储序、
  影响读者，churn/风险不小。

### 上游 HTTP 连接池升级（1c6f24d..faaadb0）—— 适配

> 上游把单例 HTTP 池拆成三个命名池：`http_pool_short`（短请求）/ `http_pool_stream`
> （SSE 流）/ `http_pool_longpoll`（长轮询）。

- [x] **升级零破坏，无需为兼容改动**。三下游 API 面（`beamai_http:*` /
  `beamai_chat_completion:*` / `beamai_llm_http_client:*`）全向后兼容，`pool` 选项本就存在
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
