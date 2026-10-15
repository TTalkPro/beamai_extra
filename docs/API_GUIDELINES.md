# 公共 API 治理

本文约束 `apps/*/src` 里的公共导出。测试 helper 放在 `apps/*/test`，不要为了
测试可见性把内部函数放进生产模块。

## 新增公共导出

1. 只导出外部确实要调用的函数；同 app 内部调用保持为局部函数。
2. 每个导出必须有 `-spec`。
3. 函数 clause 前必须有 `%% @doc` 说明用途、参数约定和返回形状。
4. 在 `apps/*/src` 新增导出后，CI 会要求它同时满足第 2、3 条。

## test-only API 基线

`designs/api/test_only_api_baseline.txt` 记录 Phase E2 审计中已知的
test-only 公共导出。这条基线只允许收缩，不允许增长；新测试需要生产模块函数时，
优先把函数改为局部函数并在测试模块里通过公共入口覆盖，或给出它作为公共 API
的真实使用场景。

`designs/api/public_export_baseline.txt` 是现有导出的基线。删除导出不需要更新它；
新增导出必须带 `-spec` 和 `%% @doc`。如果新增导出是刻意提供的公共 API，先补齐
文档和契约，再让 CI 通过。

## 删除或收缩 API

删除导出、改名、降低 arity 或改返回形状都属于兼容性变更。提交说明必须逐项列出：

```text
Removed beamai_example:foo/1
Removed beamai_example:bar/2

Compatibility: both were unused outside tests; the test helpers now use the
public entry point. No external callers were documented.
```

每项包含 module、function、arity，并明确外部/测试调用者如何迁移。多个删除可以
放在同一条提交里，但必须属于同一 app 的同一类清理，不要与无关改动混合。

## 本地检查

```bash
rebar3 compile
escript scripts/xref_unused_exports.escript /tmp/api_inventory.md
escript scripts/check_api_rules.escript /tmp/api_inventory.md
```
