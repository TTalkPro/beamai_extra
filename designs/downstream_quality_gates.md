# BeamAI Extra 下游质量门与依赖适配设计

> 状态：设计完成，执行计划见
> [tasks/downstream_quality_gates.md](../tasks/downstream_quality_gates.md)。  
> 日期：2026-10-15  
> 范围：构建、依赖锁定、examples 加载路径、静态检查与公共 API 治理。

## 背景

当前 `beamai` 已推进到 `c440eef`。`beamai_extra` 使用三个 `git_subdir`
依赖引用同一上游仓库的不同 app。升级排查确认上游公共 API 未发生破坏性变化，
`beamai_extra` 各应用源码不需要为功能兼容做修改；真正暴露的问题集中在下游工程门禁：

1. examples 使用 `ERL_LIBS` 指向上游构建产物时，缺少 `git_subdir` 产生的
   嵌套 app 路径，导致离线 examples 测试出现 `undef`。
2. 根项目 `rebar3 xref` 报出 362 条 `exports_not_used` 告警，并使
   `rebar3 check` 在 xref 阶段提前失败；这些导出绝大多数是下游框架
   提供给宿主使用的公共 API，不应一次性删除。
3. `rebar3 dialyzer` 存在一条真实静态告警：
   `apps/beamai_mcp/src/server/beamai_mcp_server.erl:444` 中
   `Other` 分支按当前类型推断不可达。
4. 上游依赖虽然已在 `rebar.lock` 中落到 `c440eef`，但缺少把
   “根项目 + examples + lock 提交”作为整体验证的自动化入口。

## 目标与非目标

### 目标

- `rebar3 compile`、`rebar3 xref`、`rebar3 dialyzer`、`rebar3 eunit`、
  `rebar3 ct` 构成可重复的根项目门禁。
- examples 离线测试在干净 checkout 中可通过 `examples/Makefile` 直接运行。
- examples 始终加载与根项目同一次构建的 `beamai_core`、`beamai_llm`、
  `beamai_agent` beam 文件。
- 上游依赖默认跟随 `main`，但实际解析版本由 `rebar.lock` 明确固定。
- 公共 API 导出有明确分类：保留、待观察、可删除；静态门禁不被
  `exports_not_used` 噪音淹没。

### 非目标

- 不为追求 xref 零告警而删除公共 API。
- 不把 examples 变成独立 rebar umbrella；继续使用上层构建产物。
- 不引入新的根依赖或运行时依赖。
- 不在本轮引入性能门禁；性能对比仍以上游 benchmark 为准。

## 方案

### 1. examples 构建路径契约

`git_subdir` 会在 `_build/default/lib/<app>` 下保留上游仓库结构，实际
beam 文件位于：

```text
_build/default/lib/beamai_core/apps/beamai_core/ebin
_build/default/lib/beamai_llm/apps/beamai_llm/ebin
_build/default/lib/beamai_agent/apps/beamai_agent/ebin
```

因此 examples 的加载路径必须包含三类目录：

1. 普通 hex 依赖：`../_build/default/lib`。
2. 上游 umbrella 内 app 父目录：
   `../_build/default/lib/beamai_core/apps` 等。
3. 本地 checkout 覆盖目录：`../_build/default/checkouts`。

`examples/Makefile` 是唯一维护点，所有文档优先引导：

```bash
cd examples
make test
```

`make test` 必须先执行 `cd .. && rebar3 compile`，再运行 examples 测试，
避免 `rebar3 eunit` 只更新 `_build/test` 导致 examples 加载旧的
`_build/default` beam。

### 2. xref：框架库语义而非普通应用语义

`beamai_extra` 与上游一样是框架库，大量导出供宿主调用，项目内部没有调用者
是预期状态。当前默认 xref 检查包含 `exports_not_used`，会把公共 API 当噪音输出。

采用与上游一致、显式排除 `exports_not_used` 的策略：

```erlang
{xref_checks, [
    undefined_function_calls,
    undefined_functions,
    locals_not_used,
    deprecated_function_calls,
    deprecated_functions
]}.
```

保留对真实缺陷敏感的检查：

- 未定义函数调用；
- 未定义导出；
- 未使用局部函数；
- deprecated API。

这样 `rebar3 check` 能继续执行到 dialyzer / eunit / ct，真实错误不会被
362 条公共导出告警掩盖。

### 3. Dialyzer 防御分支修正

`#mcp_tool.handler` 的公开类型契约是：

```erlang
fun((map()) -> {ok, term()} | {error, term()})
```

不应放宽为 `fun((map()) -> term())`，否则会把错误契约提升成公共类型。
但 MCP 工具 handler 来自宿主或适配层，运行时仍可能出现非法返回形状；
服务端不能因第三方 handler 返回畸形值而崩溃。

在 `beamai_mcp_server` 内新增一个私有调用辅助函数，把 handler 返回值
从 Dialyzer 的精确契约中还原为 `term()`：

```erlang
-spec call_tool_handler(fun((map()) -> term()), map()) -> term().
call_tool_handler(Handler, Args) ->
    Handler(Args).
```

`handle_tools_call/2` 改为：

```erlang
try call_tool_handler(Handler, Arguments) of
    {ok, Result} -> ...
    {error, Error} -> ...
    Other -> ...
catch
    _:Exception:_Stack -> ...
end
```

同时新增回归测试覆盖 handler 返回非契约形状的场景，确认服务端返回
`isError => true` 且进程不崩溃。该方案保留运行时防御，不放宽公共 handler 类型。

### 4. 依赖锁定策略

开发分支继续允许上游 `main` 作为源，方便及时发现上游兼容性问题；
`rebar.lock` 是实际解析边界，必须随源码提交。

规则如下：

| 场景 | 上游源 | 版本边界 |
|---|---|---|
| 日常开发 | `branch => main` | `rebar.lock` 固定当前上游提交 |
| 升级排查 | 显式刷新依赖 | 更新并提交新 `rebar.lock` |
| 发布 | 仍读取 `rebar.lock` | 发布说明记录上游 commit |

禁止只改 `rebar.config` 中的分支而不提交 lock；也禁止在 CI 或发布构建中
使用会绕过 lock 的临时依赖源。

### 5. 静态告警与公共 API 治理

排除 `exports_not_used` 不等于放弃 API 治理。后续分批建立轻量清单：

| 分类 | 判定 | 动作 |
|---|---|---|
| Public API | README、docs、examples 或宿主集成可能使用 | 保留，不要求内部调用 |
| Supervision / behaviour | OTP 回调、supervisor、handler 入口 | 保留 |
| Test-only helper | 仅测试使用 | 尽量移出 src 或改为局部函数 |
| Internal helper | 无内部调用、无文档、无外部集成证据 | 进入删除候选 |
| Dead code | 连续两个版本无使用者且无产品理由 | 单独提交删除 |

API 清单允许按 app 演进，不要求一次生成 362 条完整注释表；优先处理
`beamai_tools` 中与上游新增 safety 能力重叠的模块，再处理协议类 app。

### 6. 编译与测试告警卫生

以下已知低风险问题纳入清洁批次：

```text
examples/src/test_zhipu_anthropic.erl:273  unused truncate/2
apps/beamai_a2a/test/beamai_a2a_client_tests.erl:98   unused test helper
apps/beamai_a2a/test/beamai_a2a_client_tests.erl:152  unused test helper
apps/beamai_mcp/test/beamai_mcp_mock_transport.erl:46 unused State
```

处理原则：

- 测试专用且确实无用的 helper 直接删除。
- 变量确实不需要语义绑定时改为 `_State`。
- 生产代码未使用函数必须先确认是否为公共 API；只有确认死代码才删除。

## 验证矩阵

每个行为或配置批次至少执行：

```bash
rebar3 compile
rebar3 xref
rebar3 dialyzer
rebar3 eunit
rebar3 ct
make -C examples test
```

验收标准：

1. `rebar3 compile` 无生产代码告警。
2. `rebar3 xref` 无非预期告警；显式检查集下返回 0。
3. `rebar3 dialyzer` 0 warning。
4. `rebar3 eunit` 当前基线 747 tests / 0 failures 或以上。
5. examples 当前基线 9 tests / 0 failures。
6. `rebar.lock` 变更时，提交说明能指出对应上游 commit。

## 风险与回滚

| 风险 | 缓解 | 回滚 |
|---|---|---|
| 排除 `exports_not_used` 掩盖死代码 | 保留 API 审计批次与分类清单 | 恢复默认 xref checks 后按清单处理 |
| 放宽 MCP handler 类型破坏契约 | 不改 record 类型，只在私有调用点做类型隔离 | 恢复原直接调用 |
| examples 路径再次漂移 | `Makefile` 单点维护，CI 执行 `make test` | 还原路径并补回归说明 |
| 删除看似未用的公共导出 | 只走独立 API 审计提交 | 单个 revert 恢复导出 |

## 交付顺序

1. 先让现有质量门禁变绿，建立可靠基线。
2. 再把 examples 验证纳入同一自动化入口。
3. 最后做 API 审计和死代码清理，避免在噪音未消除前做大范围删除。
