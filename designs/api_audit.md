# 公共 API 审计（Phase E / E1–E2）

> 日期：2026-10-15  
> 执行计划：[tasks/downstream_quality_gates.md](../tasks/downstream_quality_gates.md) Phase E  
> 机器生成清单：[api_audit_inventory.md](api_audit_inventory.md)（`escript scripts/xref_unused_exports.escript designs/api_audit_inventory.md`）

## 1. 为什么要做

根 `rebar.config` 把 `exports_not_used` 移出默认 xref 检查集之后，那 362 条
unused export 不再阻断门禁——但它们本身是信息：它们把「框架库该有的形态」与
「真死代码」混在一起。E1 的任务不是删代码，而是**给每条归类**，让 E2 的删除
有据可依。

## 2. 方法

分类完全自动，靠收集证据而不是靠人扫：

| 分类 | 判定依据 |
|---|---|
| `otp_callback` | 该 `{Fun, Arity}` 在项目 / 依赖 / OTP（stdlib、kernel、sasl）里被 `-callback` 声明；或它是 `gen_statem` 状态函数（模块声明 `-behaviour(gen_statem)`，且函数名在本模块里作为 `next_state` / `keep_state` / `repeat` 的状态名出现） |
| `public_api` | 模块头 `%%%` 文档、根 `README*`、`docs/`、`design*/`、`apps/*/README*`、`examples/` 里出现过调用写法 |
| `test_only` | 上面那些地方都没有，只有 `apps/*/test` 引用 |
| `internal_only` | 没有任何外部调用者，但**本模块内部有调用** |
| `no_evidence` | 没有任何调用点、没有任何文档引用 |

两点值得记下来：

- **`internal_only` 单独成类**。xref 的 `exports_not_used` 只看跨模块调用，
  本模块内部调用不算「使用」。这类函数不是死代码，动作是**去掉 export 保留
  实现**，与「删除」必须分开。
- **`gen_statem` 状态函数是 xref 的固有误报**。`beamai_mcp_client` 的
  `connecting/3` / `connected/3` / `disconnected/3` 由 OTP 状态机分派，项目里
  不存在任何一处字面调用。脚本按状态名识别它们，否则会被当成删除候选。

### 已知局限

1. xref 分析的是 **default profile**，测试里的调用不算「使用」——所以有
   `test_only` 这一类（E1 基线 95 条，E2 后 98 条）。这不是缺陷，是 E3 规则要处理的另一件事：
   仅为测试存在的函数不该占生产 `src` 的公共 API。
2. 动态分派只覆盖两类：`-callback` 声明与 `gen_statem` 状态函数。将来若出现
   「函数以 `fun Mod:Fun/Arity` 形式存进 map 再被调用」的写法，需要在脚本里
   补一条证据源，否则会出现假阳性。
3. 证据只到「文本里出现过调用写法」。同名函数误命中仍可能发生，所以
   `no_evidence` 只是**候选**，删除前要人工复核（本轮已逐条复核，见第 4 节）。

## 3. 结果

### E1 基线

总计 **362 条**：

| 分类 | 条数 | 处置 |
|---|---|---|
| `public_api` | 215 | 保留（缺的补 `-spec` 与文档） |
| `test_only` | 95 | 保留，逐个判断是否该移出生产 `src` |
| `internal_only` | 40 | 去掉 export、保留实现 |
| `no_evidence` | 9 | 删除候选（其中 4 条需产品决策） |
| `otp_callback` | 3 | 保留（xref 误报） |

按 app 分布：

| app | 总数 | public_api | test_only | internal_only | no_evidence | otp_callback |
|---|---|---|---|---|---|---|
| `beamai_a2a` | 124 | 62 | 44 | 17 | 1 | 0 |
| `beamai_agui` | 65 | 29 | 15 | 18 | 3 | 0 |
| `beamai_mcp` | 100 | 54 | 35 | 3 | 5 | 3 |
| `beamai_rag` | 39 | 39 | 0 | 0 | 0 | 0 |
| `beamai_tools` | 34 | 31 | 1 | 2 | 0 | 0 |

### E2 后现状

刷新清单后的总计 **328 条**，`internal_only` 与 `no_evidence` 已清零：

| 分类 | 条数 | 处置 |
|---|---|---|
| `public_api` | 227 | 保留；本轮补齐了关键宿主入口的文档或测试 |
| `test_only` | 98 | 保留，移出生产 API 是 E3 的后续治理项 |
| `internal_only` | 0 | 全部去 export 并保留内部实现 |
| `no_evidence` | 0 | 逐条复核后删除或按框架 API 补文档/测试 |
| `otp_callback` | 3 | 保留（`gen_statem` 状态函数） |

按 app 分布：

| app | 总数 | public_api | test_only | otp_callback |
|---|---:|---:|---:|---:|
| `beamai_a2a` | 107 | 63 | 44 | 0 |
| `beamai_agui` | 49 | 34 | 15 | 0 |
| `beamai_mcp` | 99 | 60 | 36 | 3 |
| `beamai_rag` | 39 | 39 | 0 | 0 |
| `beamai_tools` | 34 | 31 | 3 | 0 |

`beamai_rag` 一条待处理项都没有：它的 unused export 全都写在
`apps/beamai_rag/README.md` 里——这也说明「文档证据」这一档救回了多少本该被
误删的公共 API。

## 4. `no_evidence` 处置记录（E1 候选 9 条）

全仓（`apps/*/src`、`apps/*/test`、`examples/`、`docs/`、README）搜索
`Module:fun(` 与裸 `fun(`，**外部引用均为 0**：

| 模块 | 函数 | 判断 |
|---|---|---|
| `beamai_mcp_adapter` | `extract_content/1` | 保留 + 测试：这是 MCP tool result 的内容抽取合同，`isError` 优先级是关键回归点 |
| `beamai_agui_event` | `raw/1` | 删除：运行时走专用 raw translator，不存在 `RAW` 事件出口 |
| `beamai_agui_a2ui` | `catalog_id/0` | 删除：catalog 常量只由 `basic_catalog/0` 与 transform 使用 |
| `beamai_agui_session` | `awaiting/1` | 删除：`status/1` 已返回同一份 awaiting 信息 |
| `beamai_a2a_task_sup` | `start_task/1` | 保留 + 文档：动态启动任务是有意保留的宿主入口 |
| `beamai_mcp_client_registry` | `get_client_info/1` | 保留 + 文档/测试：宿主内省 API |
| `beamai_mcp_client_registry` | `is_registered/1` | 保留 + 文档/测试：同上 |
| `beamai_mcp_client_registry` | `list_client_pids/0` | 保留 + 文档/测试：同上 |
| `beamai_mcp_client_sup` | `stop_client/1` | 保留 + 文档：动态停 client 的运维入口 |

## 5. E2 收尾

五个 app 都已按独立提交处理。`internal_only` 函数只收缩可见性，不改变运行时
行为；删除的导出已逐条列在对应提交说明中，属于外部调用者的 source-level
breaking change。剩余 328 条分别是文档化的 public API、测试专用 API 与
`gen_statem` 回调，没有待删除候选。
