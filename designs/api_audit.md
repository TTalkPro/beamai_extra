# 公共 API 审计（Phase E / E1）

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
   `test_only` 这一类（95 条）。这不是缺陷，是 E3 规则要处理的另一件事：
   仅为测试存在的函数不该占生产 `src` 的公共 API。
2. 动态分派只覆盖两类：`-callback` 声明与 `gen_statem` 状态函数。将来若出现
   「函数以 `fun Mod:Fun/Arity` 形式存进 map 再被调用」的写法，需要在脚本里
   补一条证据源，否则会出现假阳性。
3. 证据只到「文本里出现过调用写法」。同名函数误命中仍可能发生，所以
   `no_evidence` 只是**候选**，删除前要人工复核（本轮已逐条复核，见第 4 节）。

## 3. 结果

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

`beamai_rag` 一条待处理项都没有：它的 unused export 全都写在
`apps/beamai_rag/README.md` 里——这也说明「文档证据」这一档救回了多少本该被
误删的公共 API。

## 4. `no_evidence` 逐条复核（9 条）

全仓（`apps/*/src`、`apps/*/test`、`examples/`、`docs/`、README）搜索
`Module:fun(` 与裸 `fun(`，**外部引用均为 0**：

| 模块 | 函数 | 判断 |
|---|---|---|
| `beamai_mcp_adapter` | `extract_content/1` | 真死代码 |
| `beamai_agui_event` | `raw/1` | 真死代码 |
| `beamai_agui_a2ui` | `catalog_id/0` | 真死代码 |
| `beamai_agui_session` | `awaiting/1` | 真死代码 |
| `beamai_a2a_task_sup` | `start_task/1` | 真死代码（supervisor 里没有调用者） |
| `beamai_mcp_client_registry` | `get_client_info/1` | **需产品决策**：像是给宿主看的内省 API |
| `beamai_mcp_client_registry` | `is_registered/1` | **需产品决策**：同上 |
| `beamai_mcp_client_registry` | `list_client_pids/0` | **需产品决策**：同上 |
| `beamai_mcp_client_sup` | `stop_client/1` | **需产品决策**：动态停 client 的运维入口 |

后 4 条的共同点：它们不是「写剩下的」，而是**本来该有宿主用、却从没写进
文档**的 API。删掉会让「宿主想查自己注册了哪些 MCP client」这件事没路可走；
留着又是无文档的公共面。合理处置是补文档 + 测试，不是删，也不是就这么悬着。

## 5. 下一步（E2）

按 `beamai_tools` → `beamai_rag` → `beamai_a2a` → `beamai_mcp` → `beamai_agui`
逐 app 清理，每个 app 一次独立提交：

- `public_api`：不动。
- `internal_only`：去 export，保留实现；行为不变，靠现有测试守。
- `test_only`：判断是否该移到测试 helper。
- `no_evidence`：按第 4 节结论执行，提交说明写明模块 / 函数 / arity 与兼容性影响。
