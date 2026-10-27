# MCP streamable-HTTP：非阻塞轮询 `recv(0, _)` 超时时丢掉本次已收到的响应体分块，分多块到达的响应永远拼不完整，调用方干等到超时

## 症状

beamai_muse 的任务里，Parallel 的 `web_search`（`https://search.parallel.ai/mcp`，`tools/call` 返回约 40 KB 的
`application/json`）有四成左右等满 45 s 超时（线上一个任务 80 次搜索里 32 次）。同一个端点用 curl / Python 每次 1–3 s 就返回。
连接和 initialize 都正常（约 1 s），卡住的总是 `tools/call`。小响应（initialize 的 2.9 KB）不受影响。

复现（beamai_extra `799383e`，beamai_muse 锁定的版本）：

```erlang
{ok, C} = beamai_mcp_client:start_link(#{transport => http, url => <<"https://search.parallel.ai/mcp">>,
                                         timeout => 20000, init_timeout => 20000}),
ok = beamai_mcp_client:await_connected(C, 20000),
beamai_mcp_client:call_tool(C, <<"web_search">>,
    #{<<"objective">> => <<"旅顺博物馆 开放时间"/utf8>>, <<"search_queries">> => [<<"旅顺博物馆 开放时间"/utf8>>]}, 20000).
%% 10 次里约 3 次：exit({timeout, {gen_statem, call, ...}})
```

对卡住的那次跟踪 `beamai_mcp_transport_http_gun:handle_gun_event/2` 和 `recv/2`（返回值）：

```
SEND  {"id":2,"jsonrpc":"2.0","method":"tools/call",...}
RECV  -> error {error,timeout}           (x14，服务端还没回)
gun_response stream R nofin 200 application/json
RECV  -> error {error,timeout}
gun_data     stream R nofin 7877 bytes
gun_data     stream R nofin 8192 bytes
gun_data     stream R nofin 3997 bytes
RECV  -> error {error,timeout}           <- 这三块随这次 timeout 一起丢了
gun_data     stream R nofin 8192 bytes
gun_data     stream R nofin 8184 bytes
RECV  -> error {error,timeout}           <- 又丢两块
gun_data     stream R nofin 4822 bytes
gun_data     stream R fin 0 bytes
RECV  -> ok "shunmuseum.cn/news/)\n\n[首页](https://lvshunmuseum.cn/) ..."   <- 只剩最后一段，不是合法 JSON
```

最后交给客户端的是响应体的尾巴（不以 `{"jsonrpc"` 开头），解码失败，`id: 2` 的调用永远等不到回复。
整个响应恰好在一次轮询里到齐时才成功，所以是概率性的，响应越大、分块越多越容易出。

## 位置

`apps/beamai_mcp/src/`：

1. `client/beamai_mcp_client.erl:371`（connecting）、`:426`（connected）：`Mod:recv(0, TState)` 每 100 ms 轮询一次，
   `{error, timeout}` 时 `{keep_state, Data}`，保留的是**调用前**的 `transport_state`。
2. `transport/beamai_mcp_transport_http_gun.erl`：
   - `:183` `recv/2`：`Timeout = 0` 时 `nonblocking = true`，`:539` `remaining_timeout/1` 因此返回 0；
   - `:464` `handle_gun_event({gun_data, _, _, nofin, Data}, State)` → `append_data/2` 把块拼进 `buffer`（新 state），
     再经 `:504` `receive_sse_data/2`（非 SSE 分支）调用 `receive_response(remaining_timeout(State), State)`；
   - 下一个事件还没到时这里返回 `{error, timeout}`，**不带 state**，拼好的 `buffer` 随之丢失。
     SSE 分支（`[] ->` 时同样 `receive_response(...)`）有同样的问题：未成帧的半个事件也会丢。

## 原因

`recv/2` 的契约是 `{ok, Msg, State} | {error, Reason}`：错误不返回 state。非阻塞轮询下“本次拿到了一部分数据、
还没完”是正常情况，却走了错误分支，于是本次调用里对 state 的修改（`buffer`、`response_is_sse`、
从响应头里拿到的 `session_id`、`error_response`）全部作废。gun 事件已经从 worker 的队列里取走，不会再来第二次。

## 建议

任选其一：

- `recv/2` 增加“无完整消息但 state 有变化”的返回，例如 `{wait, State}`（或 `{error, timeout, State}`），
  `beamai_mcp_client` 在两处轮询里保存它；
- 或者把分块累积留在 transport 的 worker 进程里（按 `StreamRef` 累积到 fin / 一个完整 SSE 事件再转给调用方），
  `recv/2` 只交付完整消息，调用方 state 里不再有半成品。

顺带：worker 转发事件时没有按 `StreamRef` 过滤（`handle_gun_event` 里是 `_StreamRef`），上一个请求迟到的事件会被当成当前请求的。

## 影响与 beamai_muse 的临时处理

beamai_muse 的 `muse_search`（M8-4 起用 `beamai_mcp_client`）因此大量超时；一个调研任务 6 次运行都在反复搜索、
最终失败。修复前 beamai_muse 先在自己这边绕过（见 beamai_muse tasks/M13）。

## 回执（2026-10-27）

已修，2.1.1。走的是建议里的第一条：给 `recv/2` 一个「本轮没拼完，但 state 推进了」的返回。

1. `beamai_mcp_transport` 行为的 `recv/2` 契约加 `{wait, NewState}`：本轮没有完整
   消息时把新 state 交回调用方，`{error, Reason}` 留给真失败（连接断、worker 死、
   响应过大）。旧的 `{error, timeout}` 仍然合法，第三方传输实现不必改。
2. `beamai_mcp_transport_http_gun`：`receive_response/2` 的「本轮 worker 没有事件」
   不再走错误分支；`handle_gun_event → append_data → receive_sse_data` 一路把更新后的
   state（`buffer`、`response_is_sse`、响应头里的 `session_id`）穿到返回值。非阻塞
   那一轮会把 worker 队列里已经躺着的事件一次读干，不再是每 100 ms 只推进一块；
   SSE 的半个事件同样留在 `buffer` 里接着拼。
3. `beamai_mcp_client` 的 `connecting/3`、`connected/3` 两处轮询都接收并保存
   `{wait, NewTState}`。
4. 同一类丢数据路径在 stdio（半行 JSON 被 `after Timeout` 丢掉）和 SSE 传输
   （半个 SSE 事件）上一并修了。
5. 「顺带」那条也修了：owner 转发来的事件按 `stream_ref` 过滤，不属于当前请求的
   迟到事件直接丢弃；另外 `initialized` 通知的传输 state 不再在握手结束时被丢掉。

回归：`beamai_mcp_transport_http_gun_tests` —— 约 25 KB 的 JSON 分 7 次 write 送达、
SSE 事件的 `data:` 与结束空行分属两次 write；`beamai_mcp_client_statem_tests` ——
`{wait, NewState}` 的 state 穿线（改前这三条全红）。

已知没动的：请求级 deadline 到点后 `recv/2` 仍返回 `{error, timeout}`，在途调用靠
调用方自己的 `gen_statem:call` 超时兜底 —— 只在服务端真的超过 `timeout` 配置时发生。
