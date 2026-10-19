# BeamAI Framework Dependencies

English | [中文](DEPENDENCIES.md)

This document describes the runtime and application dependencies for the current beamai_extra release.

## Upstream BeamAI

Upstream applications are fetched with `git_subdir`. `rebar.config` declares `{tag, "v2.0.0"}`;
the committed `rebar.lock` is authoritative for builds. All three upstream applications currently resolve to:

```text
08aa844a11791fabc20ff2dab19ea82a71343881
```

Read-only verification:

```bash
escript scripts/check_dependency_lock.escript rebar.lock
```

When upgrading upstream, refresh `rebar.lock` and run compile, xref, Dialyzer, and tests for both the root project and examples.

## Runtime Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| [gun](https://github.com/ninenines/gun) | 2.7.0 | HTTP/SSE transport (HTTP/2) |
| [cowboy](https://github.com/ninenines/cowboy) | 2.12.0 | Optional HTTP server adapter |
| [uuid_erl](https://github.com/okeuday/uuid) | 2.0.6 | UUID generation |

Transitive dependencies are pinned by `rebar.lock`; they currently include cowlib, quickrand, and ranch.

## Test Dependencies and Plugins

| Dependency/plugin | Version | Purpose |
|-------------------|---------|---------|
| [meck](https://github.com/eproxus/meck) | 1.2.0 | Test mocks |
| rebar3_proper | 0.12.1 | Property testing |
| rebar3_ex_doc | 0.3.0 | Documentation generation |

Examples tests also use Meck 1.2.0.

## Upstream Applications

| Application | Responsibility |
|-------------|----------------|
| `beamai_core` | ChatClient, tools, filters, memory, Gun HTTP, protocol utilities |
| `beamai_llm` | LLM configuration, provider adapters, output parsers |
| `beamai_agent` | SimpleAgent, tool loop, interrupt/resume, SubAgents |

Dependency direction: `beamai_agent` and `beamai_llm` build and run on top of `beamai_core`.

## Local Applications

| Application | Required dependencies | Optional dependencies |
|-------------|-----------------------|-----------------------|
| `beamai_tools` | `beamai_core`, `beamai_llm` | — |
| `beamai_rag` | `beamai_core` | `beamai_llm` (provider embedding/rerank/generation) |
| `beamai_a2a` | `beamai_core`, `beamai_agent` | `cowboy` (Cowboy HTTP adapter) |
| `beamai_mcp` | `beamai_core` | `cowboy` (server HTTP), `gun` (HTTP/SSE transport) |
| `beamai_agui` | `beamai_core`, `beamai_agent` | `cowboy` (Cowboy HTTP adapter) |

`optional_applications` marks optional runtime features. Include the optional application in a release when its modules are needed.
