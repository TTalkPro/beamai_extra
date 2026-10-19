# BeamAI Framework Dependencies

[English](DEPENDENCIES_EN.md) | 中文

本文档描述 beamai_extra 当前版本的运行时依赖和应用依赖方向。

## 上游 BeamAI

上游依赖通过 `git_subdir` 引入，`rebar.config` 声明 `{tag, "v2.0.0"}`；
实际构建以提交过的 `rebar.lock` 为准。当前三个上游应用都锁定在：

```text
08aa844a11791fabc20ff2dab19ea82a71343881
```

只读校验：

```bash
escript scripts/check_dependency_lock.escript rebar.lock
```

升级上游时必须刷新 `rebar.lock`，并运行根项目与 examples 的编译、xref、dialyzer 和测试。

## 运行时依赖

| 依赖 | 版本 | 用途 |
|------|------|------|
| [gun](https://github.com/ninenines/gun) | 2.7.0 | HTTP/SSE 传输（HTTP/2） |
| [cowboy](https://github.com/ninenines/cowboy) | 2.12.0 | 可选 HTTP 服务端适配 |
| [uuid_erl](https://github.com/okeuday/uuid) | 2.0.6 | UUID 生成 |

传递依赖由 `rebar.lock` 锁定；当前包含 cowlib、quickrand、ranch。

## 测试与插件

| 依赖/插件 | 版本 | 用途 |
|-----------|------|------|
| [meck](https://github.com/eproxus/meck) | 1.2.0 | 测试 mock |
| rebar3_proper | 0.12.1 | 属性测试 |
| rebar3_ex_doc | 0.3.0 | 文档生成 |

examples 测试同样使用 Meck 1.2.0。

## 上游应用依赖

| 应用 | 说明 |
|------|------|
| `beamai_core` | ChatClient、Tool、Filter、Memory、Gun HTTP、协议工具 |
| `beamai_llm` | LLM 配置、provider 适配、output parser |
| `beamai_agent` | SimpleAgent、tool loop、中断/恢复、SubAgent |

依赖方向：`beamai_agent` 和 `beamai_llm` 构建/运行在 `beamai_core` 之上。

## 本项目应用依赖

| 应用 | 必需依赖 | 可选依赖 |
|------|----------|----------|
| `beamai_tools` | `beamai_core`, `beamai_llm` | — |
| `beamai_rag` | `beamai_core` | `beamai_llm`（provider embedding/rerank/generation） |
| `beamai_a2a` | `beamai_core`, `beamai_agent` | `cowboy`（Cowboy HTTP 适配） |
| `beamai_mcp` | `beamai_core` | `cowboy`（server HTTP）, `gun`（HTTP/SSE transport） |
| `beamai_agui` | `beamai_core`, `beamai_agent` | `cowboy`（Cowboy HTTP 适配） |

`optional_applications` 只表示可选运行时功能；构建发布时如需对应模块，应包含可选应用。
