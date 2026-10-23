# Changelog

## [2.0.1] - 2026-10-23

### Fixed

- MCP streamable HTTP 传输改用专用 Gun owner 转发连接事件；`recv(0)` 不再取消在途请求，
  并按请求级截止时间管理超时，修复远端 MCP initialize 阶段崩溃/无法连接。
- HTTP 与 SSE Gun 传输支持 `tls_opts`；未配置 HTTPS TLS 时显式启用系统 CA、`verify_peer`、
  SNI 与 HTTPS hostname 校验，并启用 HTTP/2 协商。
- 消费 Streamable HTTP 通知的 HTTP 确认，避免下一个请求与通知响应竞争；JSON-RPC 请求
  在无参数时省略可选 `params`，兼容严格校验的远端 MCP 服务器。
- Streamable HTTP 响应增加 `max_body` 上限，并兼容远端常见的 CRLF SSE 帧。
- `beamai_mcp_adapter` 将 MCP JSON Schema 转为 beamai 参数 DSL，handler 统一返回
  `{ok, Text}` / `{error, Message}`，可直接通过 `beamai_tool` schema 与 executor。

## [2.0.0] - 2026-10-19

### Changed

- 上游 BeamAI 依赖锁定到 `v2.0.0` tag（commit `08aa844a11791fabc20ff2dab19ea82a71343881`）。
- `beamai_extra` 及全部本地 OTP 应用版本统一升级到 `2.0.0`。
- 跟随上游依赖调整：Gun 升级到 `2.7.0`（Cowlib `2.21.0`），Meck 升级到 `1.2.0`，
  并固定 `rebar3_proper` 和 `rebar3_ex_doc` 插件版本。
- 文档改用 v2.0.0 的 SimpleAgent Memory API 和 11 个内置 LLM provider，
  移除已从上游删除的 Process / DeepAgent 示例与说明。
