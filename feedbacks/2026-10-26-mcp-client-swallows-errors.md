# MCP 客户端吞错误：连接/HTTP 失败只落 `disconnected`、在途调用不回复干等到超时；gun owner 在 `recv` 等待期间看不见调用方退出；`http_error` 丢响应体

## 症状

2.0.1（`422284c`）修好了远端 streamable-HTTP 的连通性，deepwiki、Parallel 都能 initialize → tools/call。
但只要出错，调用方**拿不到原因**：

```erlang
{ok, C} = beamai_mcp_client:start_link(#{transport => http, url => <<"http://127.0.0.1:1/mcp">>}).
beamai_mcp_client:call_tool(C, <<"web_search">>, #{}).
%% 连接中：{error, initializing}；连接失败后：{error, not_connected}
%% 真正的原因 {connection_failed, econnrefused} 只进了一行 logger:warning
```

已连接后 tools/call 收到 HTTP 401 / 响应超过 `max_body`：

```erlang
beamai_mcp_client:call_tool(C, <<"web_search">>, Args, 45000).
%% 不回复 —— 45 s 后 gen_statem:call 以 exit({timeout, ...}) 退出
```

beamai_muse 因此**没有用 `beamai_mcp_client`**，而是直接驱动
`beamai_mcp_transport_http_gun` + `beamai_mcp_jsonrpc`（同步 initialize → initialized → tools/call），
这样 `{http_error, 401}`、`response_too_large`、`{connection_failed, _}` 都能拿到，并映射成 OpenMuse
（MCP TS SDK）的报错文案。

## 位置

beamai_extra `422284c`，`apps/beamai_mcp/src/`：

1. **客户端把错误降级成状态**
   - `client/beamai_mcp_client.erl:322-323`：transport 连接失败只 `logger:warning`，转 `disconnected`，原因丢弃；
     `:339`、`:343`、`:359`、`:376` 同理（initialize 发送失败、init 超时、init 期间 recv 失败、initialize 回 error）。
   - `:289` / `:383`：之后所有调用只拿到 `{error, not_connected}` / `{error, initializing}`。
   - `:409-410`：`connected` 下 `recv` 出错直接 `{next_state, disconnected, Data}`，**`pending_requests` 里的调用方一个都不回复**，
     只能等各自的 `gen_statem:call` 超时（`call_tool/3` 默认 60 s）后 exit。
   - `start_link/1` 在 initialize 完成前就返回，没有「等到 connected 或失败」的 API，调用方只能轮询 `get_state/1`。
2. **gun owner 在 `recv` 等待期间看不见调用方退出**
   - `transport/beamai_mcp_transport_http_gun.erl:359-367`：`{recv, Wait}` 分支里嵌套的 `receive` 只匹配 gun 事件，
     `after Wait`；`:341` 的 `{'DOWN', ParentMonitor, ...}` 要等这次嵌套 receive 结束才会被处理。
   - 结果：调用方在等响应时被 kill，gun 连接要等服务器回包或 `Wait`（= 请求超时，beamai_muse 里是 45 s）到期才关。
     beamai_muse 的 `muse_search_tests:caller_death_closes_connection` 为此从「500 ms 内关闭」放宽成「在途响应到达后关闭」。
3. **`http_error` 丢响应体**
   - `transport/beamai_mcp_transport_http_gun.erl:428`：非 2xx 只返回 `{http_error, Status}`，body 被 cancel 掉。
     MCP TS SDK 的报错是 `Error POSTing to endpoint: <body>`，下游复刻不了，只能退化成状态码。
4. （小）请求头里没有 `mcp-protocol-version`（`build_request_headers/2`），规范要求 initialize 之后的 HTTP 请求都带上。

## 怎么撞上的

beamai_muse M8-4「切回 beamai_mcp」：`muse_search` 的单测用一个 cowboy 假 MCP 服务器覆盖
`status/401`、`big`（2 MiB 响应、1 MiB 上限）、`slow`（超时）、`rpcerror`、调用方被 kill 这几条路径。
走 `beamai_mcp_client` 时前两条变成干等超时、报错文案全丢；直接用 transport 后只剩第 2、3 条。

## 影响面

所有用 `beamai_mcp_client` 的下游：远端 MCP 出错时**不报错、只是慢**（等满调用超时），日志里才有原因。
第 2 条对直接用 transport 的下游也成立：被取消的调用会占着一条连接直到超时。

## 建议

- 连接/initialize 失败时把原因存进 `#data{}`，之后的调用回 `{error, {not_connected, Reason}}`；
  或者提供 `start_link` 的同步变体 / `await_connected(Pid, Timeout) -> ok | {error, Reason}`。
- `connected` 里 recv 出错时，用该错误回复 `pending_requests` 里的所有调用方再转 `disconnected`。
- 嵌套 receive 里加上 `{'DOWN', ParentMonitor, ...} -> gun:close(ConnPid)` 和 `{beamai_mcp_http, close}`。
- 非 2xx 时读完（有上限的）body，返回 `{http_error, Status, Body}`。

## 回执（2026-10-26）

已修，回归见 `beamai_mcp_client_statem_tests` 与
`beamai_mcp_transport_http_gun_tests`：

1. 连接/握手失败原因保留在 client state；调用返回
   `{error, {not_connected, Reason}}`。新增 `await_connected/1,2`，成功回 `ok`，
   失败回 `{error, {connection_failed, Reason}}`。
2. `connected` 下传输读取失败时，先统一回复所有 pending 调用方
   `{error, {connection_failed, Reason}}`，再降级到 `disconnected`。
3. Gun owner 的等待 receive 现在同时匹配 caller `DOWN` 和 close 请求，退出时立即
   `gun:close/1`。
4. 非 2xx 响应会在 `max_body` 保护下读取 body，返回
   `{http_error, Status, Body}`；超过上限仍立刻返回 `response_too_large`。
5. HTTP transport 新增可选 `set_protocol_version/2` 回调；initialize 协商后，
   后续请求带 `mcp-protocol-version`。
