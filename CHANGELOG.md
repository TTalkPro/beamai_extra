# Changelog

## [2.1.1] - 2026-10-27

### Fixed

- MCP streamable-HTTP 传输在 `recv(0, _)` 非阻塞轮询超时时会丢掉本轮已经收到
  的响应体分块，分多块到达的大响应（下游实测约 40 KB 的 `tools/call`）永远拼不
  完整，调用方干等到超时。传输层 `recv/2` 契约新增 `{wait, NewState}`：本轮没有
  完整消息但 state 已推进（body 片段、半个 SSE 事件、半行 stdio、响应头里的
  session_id）时交回新 state，`beamai_mcp_client` 的两处轮询都保存后继续。
  Stdio 与 SSE 传输的同类丢数据路径一并修正。
- HTTP 传输按 `stream_ref` 过滤 gun 事件：不属于当前请求的迟到事件（上一个请求
  被取消后的响应、`initialized` 通知的 HTTP 确认）被丢弃，不再冒充当前请求的回复。
- `initialized` 通知的传输 state 不再被丢弃：握手进入 connected 时带上排空确认
  后的 state，避免下一个请求读到通知的 HTTP 确认。

## [2.1.0] - 2026-10-26

### Added

- `beamai_mcp_client:await_connected/1,2`：保持 `start_link/1` 异步语义的同时，
  显式等待 initialize 完成并直接拿到连接/握手失败原因。
- `beamai_mcp_transport` 新增可选 `set_protocol_version/2` 回调；HTTP 传输在
  initialize 协商后为后续请求携带 `mcp-protocol-version`。

### Fixed

- MCP client 保留连接、initialize 和传输读取失败的原始原因；disconnected 后的调用
  返回 `{error, {not_connected, Reason}}`，不再把具体错误降级成日志。
- `connected` 下传输读取失败时，先回复所有 pending 调用方
  `{error, {connection_failed, Reason}}`，再降级到 disconnected，避免调用方等满超时。
- Streamable HTTP 非 2xx 响应在 `max_body` 保护下读取 body，错误返回
  `{http_error, Status, Body}`；超过上限仍立即返回 `response_too_large`。
- Gun owner 在嵌套等待中响应调用方退出和 close 请求，被取消的调用不再占住连接
  直到响应或请求超时。

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
