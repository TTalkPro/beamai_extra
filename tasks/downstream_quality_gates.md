# Downstream Quality Gates 实施计划

> 设计：[designs/downstream_quality_gates.md](../designs/downstream_quality_gates.md)  
> 目标：把上游 `c440eef` 适配排查中发现的问题转化为可验证的下游质量门禁。  
> 状态：执行中。  
> 验证基线：根项目 747 eunit tests / 0 failures；examples 9 eunit tests / 0 failures。

## Phase A — 现状固化与 xref 门禁

### A1. 记录当前告警证据

- [x] 确认上游依赖解析到 `c440eef`。
- [x] 确认根项目编译通过，eunit 通过。
- [x] 收集 xref 输出：362 条均为 `is unused export (Xref)`。
- [x] 确认 xref 无 `undefined_function_calls`、`undefined_functions` 等真实路径错误。
- [x] 记录 dialyzer 唯一告警：`beamai_mcp_server.erl:444` 的不可达 `Other` 分支。

验收：

- [x] 本计划与设计文档能准确引用当前证据。
- [x] 不把公共 API 告警误判为破坏性兼容问题。

### A2. 将 xref 调整为框架库检查集

- [x] 在根 `rebar.config` 增加显式 `{xref_checks, [...]}`。
- [x] 保留 `undefined_function_calls`、`undefined_functions`、`locals_not_used`、
  `deprecated_function_calls`、`deprecated_functions`。
- [x] 显式不启用 `exports_not_used`，理由写入配置注释。
- [x] `rebar3 xref` 返回 0。

验收：

- [x] `rebar3 xref` 不再因公共导出未使用而失败。
- [x] `rebar3 check` 可以继续执行到 dialyzer。

### A3. 修复 MCP tool handler 防御分支

- [x] 在 `beamai_mcp_server` 增加私有 `call_tool_handler/2`。
- [x] 该函数 contract 接收 `fun((map()) -> term())`，返回 `term()`。
- [x] `handle_tools_call/2` 改经该函数调用 handler。
- [x] 保留 `{ok, _}`、`{error, _}`、unknown shape、exception 四类处理。
- [x] 新增回归测试：handler 返回非 `{ok,_}/{error,_}` 形状时，服务端响应
  `isError => true` 且 server 不崩溃。
- [x] 新增或调整测试：handler 正常返回 `{ok,_}` / `{error,_}` 的行为不变。

验收：

- [x] `rebar3 dialyzer` 无 `beamai_mcp_server.erl:444` 告警。
- [x] `rebar3 eunit --app=beamai_mcp` 通过。

### A4. 全量门禁验证

- [x] `rebar3 compile`。
- [x] `rebar3 xref`。
- [x] `rebar3 dialyzer`，0 warning。
- [x] `rebar3 eunit`，0 failures（749 tests）。
- [x] `rebar3 ct`（当前无 ct 套件，0 tests passed）。

验收：

- [x] 根项目门禁可从 xref 一路执行到测试，不再被公共导出噪音提前终止。

提交约定：

- `A2` 单独提交：`chore: align xref with framework library checks`。
- `A3` 单独提交：`fix: keep mcp tool handler failures isolated`。
- 若 A2 / A3 分属代码与配置，仍各自独立提交，不混合无关文档。

## Phase B — examples 构建契约自动化

### B1. 保持 `Makefile` 单点路径

- [x] examples `ERL_LIBS` 包含 `_build/default/lib`。
- [x] examples `ERL_LIBS` 包含上游三个 umbrella 的 `apps` 父目录。
- [x] examples `ERL_LIBS` 包含 `_build/default/checkouts`。
- [x] `make test` 先执行根项目 `rebar3 compile`。

### B2. 消除重复路径示例

- [x] 将 examples 内部模块注释中的裸 `ERL_LIBS` 示例收敛为 `make compile` / `make test` / `make shell`。
- [x] README 中需要保留底层命令时，引用与 `examples/Makefile` 相同的三类路径。
- [x] 不允许出现只包含 `_build/default/lib` 的旧路径（`example_agui_durable.erl` 的
  双进程说明保留完整五段路径，与 Makefile 一致）。

### B3. 增加可脚本化入口

- [x] 新增 `scripts/check_examples.sh` 或等价脚本。
- [x] 脚本行为：根项目 compile → examples `make test`。
- [x] 脚本返回码可被 CI 直接使用。
- [x] 脚本不引入根依赖。

### B4. CI 集成

- [x] CI 根任务执行 `rebar3 compile`、`xref`、`dialyzer`、`eunit`、`ct`。
- [x] CI examples 任务执行 B3 脚本。
- [x] CI 失败日志能区分根项目失败与 examples 加载路径失败（examples 为独立 job，
  失败日志单独可见）。

验收：

- [x] 干净 checkout 中 `make -C examples test` 通过。
- [x] examples 当前基线 9 tests / 0 failures。
- [x] CI 覆盖根项目与 examples。

提交约定：

- `B2` 与文档更新可合并为一个 `docs: clarify examples build path` 提交。
- `B3` / `B4` 合并为 `ci: verify extra examples build`，如有代码调整则代码单独提交。

## Phase C — 上游依赖锁定与升级策略

### C1. 锁定当前上游版本

- [x] `rebar.lock` 固定 `beamai_core` / `beamai_llm` / `beamai_agent` 到
  `c440eefd700990ca7d2755d9fbe00f3c9d999283`。

### C2. 文档化锁定规则

- [x] 在 README 或依赖文档说明：`rebar.config` 使用 `branch => main` 只表示源，
  实际构建以 `rebar.lock` 为准。
- [x] 说明升级时必须提交新的 `rebar.lock`。
- [x] 发布说明需记录解析到的上游 commit（docs/DEPENDENCIES.md）。

### C3. lock 完整性检查

- [x] 增加轻量 escript 或脚本检查 `rebar.lock` 中三个上游依赖存在且 ref 一致。
- [x] 可选输出当前锁定 commit，便于发布说明粘贴。
- [x] 不自动升级依赖；升级仍由人工触发。

验收：

- [x] 清洁构建无需重新解析上游版本即可复现。
- [x] lock 检查脚本在当前仓库通过。

提交约定：

- 文档与脚本分开提交：
  - `docs: document upstream lock policy`
  - `chore: verify upstream dependency lock`

## Phase D — 编译与测试告警清理

### D1. 生产代码

- [x] 复核 `examples/src/test_zhipu_anthropic.erl` 的 `truncate/2`。
- [x] 确认无外部使用后删除；若仍需保留，补充引用或明确注释。
- [x] `rebar3 compile` 后无生产代码告警。

### D2. 测试代码

- [x] 删除 `beamai_a2a_client_tests` 中两个未使用 helper。
- [x] 将 `beamai_mcp_mock_transport` 中未使用 `State` 改为 `_State`。
- [x] 不改变任何测试断言。

### D3. 验证

- [x] `rebar3 compile`。
- [x] `rebar3 eunit`。
- [x] `make -C examples test`。

提交约定：

- 生产代码清理：`chore: remove unused example helper`。
- 测试清理：`test: clean unused helpers`。

## Phase E — 公共 API 审计

> 本阶段不阻塞 A–D。只有在质量门禁稳定后才开始，避免在 362 条噪音中做删除。

### E1. 建立初始清单

- [x] 使用 xref 输出生成按 app 分组的 unused-export 清单
  （`scripts/xref_unused_exports.escript` → `designs/api_audit_inventory.md`）。
- [x] 标记 docs / README / examples 引用为 Public API（215 条）。
- [x] 标记 supervisor、gen_server、cowboy handler、behaviour 回调为 OTP surface
  （3 条 `gen_statem` 状态函数，含 `-callback` 与 stdlib/kernel 声明的比对）。
- [x] 标记仅测试使用的导出为 internal/test candidate（95 条）。
- [x] 标记完全无证据使用者为 deletion candidate（9 条，已逐条复核外部引用为 0）。

结论见 [designs/api_audit.md](../designs/api_audit.md)：额外分出 `internal_only`
（本模块内部调用、无外部调用者，40 条）一类——xref 的 `exports_not_used` 不把
本模块调用算作使用，这类应当去 export 而非删除。

### E2. 分 app 清理

按以下顺序处理，顺序可按实际工作量调整：

1. `beamai_tools`
2. `beamai_rag`
3. `beamai_a2a`
4. `beamai_mcp`
5. `beamai_agui`

每个 app：

- [x] Public API 保留，不因内部未调用而删除。
- [x] internal helper 收缩为局部函数。
- [x] deletion candidate 只在独立提交中删除。
- [x] 删除导出前补一条兼容性说明或测试。

E2 已完成：`beamai_tools` 仅补安全模块测试；`beamai_rag` 39 条均有 README
证据，无动作；`beamai_a2a` / `beamai_mcp` / `beamai_agui` 已去 export 内部
helper，删除候选按复核结果删除或补文档/测试。刷新后的清单为 328 条
（public_api 227、test_only 98、otp_callback 3），`internal_only` 与
`no_evidence` 均为 0。

### E3. 长期规则

- [ ] 新增公共导出必须有 `-spec` 与 doc 注释。
- [ ] 仅为测试暴露的函数不得放在生产 src 的公共 API 中。
- [ ] API 删除必须在提交说明中列出模块、函数、arity 和兼容性影响。

验收：

- [ ] 每条 unused export 能归入 Public API、OTP surface、internal candidate
  或 deletion candidate。
- [ ] 没有无分类依据的直接删除。

## 总验收

### 根项目

- [x] `rebar3 compile` 通过。
- [x] `rebar3 xref` 通过。
- [x] `rebar3 dialyzer` 0 warning。
- [x] `rebar3 eunit` 至少 747 tests / 0 failures（749 tests）。
- [x] `rebar3 ct` 通过。

### examples

- [x] `make -C examples test` 通过。
- [x] examples 至少 9 tests / 0 failures。
- [x] examples 加载的是当前根项目 `_build/default` 中的上游 beam。

### 文档与依赖

- [x] `designs/downstream_quality_gates.md` 描述所有问题、决策与回滚。
- [x] `tasks/downstream_quality_gates.md` 每批有明确验证命令。
- [x] `TASK.md` 顶部能发现当前状态和计划入口。
- [x] `rebar.lock` 提交且指向明确上游 commit。

## 完成定义

1. 任何开发者 clone 后先 `rebar3 compile`，再 `make -C examples test` 即可复现验证。
2. `rebar3 check` 不因下游框架库公共 API 的预期形态失败。
3. Dialyzer 当前已知告警清零。
4. 上游升级只需要三个动作：刷新 lock、跑本计划验证矩阵、提交升级说明。
5. 后续 API 清理不再和工程门禁修复混在同一提交。
