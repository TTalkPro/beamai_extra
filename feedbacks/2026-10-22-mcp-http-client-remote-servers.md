# MCP HTTP 客户端连不上任何远端 streamable-HTTP 服务器：`recv(0)` 当场取消请求、gun 消息落进 gen_statem 崩 `function_clause`；另外 `tls_opts` 无入口、adapter 产出的工具 beamai_core 不认

## 症状

`beamai_mcp_client:start_link/1` 指向公网 MCP 服务器，**connect 阶段客户端进程直接崩**：

```erlang
{ok, C} = beamai_mcp_client:start_link(#{transport => http,
                                         url => <<"https://mcp.deepwiki.com/mcp">>,
                                         client_info => #{name => <<"m">>, version => <<"0">>}}).
%% start_link 返回 {ok, Pid}，约 1 s 后：
** State machine <0.132.0> terminating
** Last event = {info,{gun_error,<0.133.0>,#Ref<...>,{badstate,"The stream cannot be found."}}}
** When server state  = {connecting, ...}
** Reason for termination = error:function_clause
**  [{beamai_mcp_client,connecting,[info,{gun_error,...},...]}, ...]
```

2026-10-22 在 bcd77ac 的源码上对 `https://mcp.deepwiki.com/mcp`（无鉴权）重新跑过，复现；
此前对 Parallel 的 MCP 端点也是同样的崩法。
同一台机器上用 ~40 行直接调 gun 的代码（下面「建议」里那段），initialize → `notifications/initialized`（202）
→ `tools/list` 一次就通，deepwiki 列出 3 个工具，Parallel 列出 `web_search`、`web_fetch`。

## 位置

beamai_extra `bcd77ac`：

1. **`recv(0)` 轮询把请求自己取消了**
   - `apps/beamai_mcp/src/client/beamai_mcp_client.erl:349`（`connecting(info, recv_loop, ...)`）
     和 `:395`（`connected(info, recv_loop, ...)`）：`Mod:recv(0, TState)`，拿到 `{error, timeout}` 就
     `send_after(100, self(), recv_loop)` 再来。
   - `apps/beamai_mcp/src/transport/beamai_mcp_transport_http_gun.erl:256-257`（`receive_response/4`）：
     `after Timeout -> gun:cancel(ConnPid, StreamRef), {error, timeout}`。`Timeout = 0` ⇒ POST 刚发出去，
     第一次轮询就把这条 stream **cancel 掉**。`:277-278`、`:318-319`（JSON body / SSE body）同样写法。
   - 而 HTTP 请求与响应之间必然隔着一个 RTT，所以每一次请求都会被取消。
2. **gun 消息根本到不了 `recv` 里的 selective receive**：gun 的 owner 是 gen_statem 进程本身，
   statem 空闲时 mailbox 里的 `gun_response` / `gun_data` / `gun_error` 都被 gen_statem 当作
   `info` 事件派发。`connecting/3`（`beamai_mcp_client.erl:307-384`）**没有兜底的 info 子句**
   （`connected/3` 在 `:467` 有，`disconnected/3` 在 `:295` 有），所以取消后那条
   `{gun_error, _, _, {badstate, "The stream cannot be found."}}` ⇒ `function_clause`。
   就算没被取消，正常的 `gun_response` 也会以同样方式掉进来。
3. **`tls_opts` 没有入口**：`beamai_mcp_transport_http_gun.erl:84-88` 的 `GunOpts` 只有
   `connect_timeout`、`transport`、`protocols => [http]`，config 里的 TLS 选项传不进去；
   `beamai_mcp_transport_sse_gun.erl:347-351`（`create_post_connection/1`）同样。
   ⚠️ 核实过：**眼下并不是不验证** —— gun 2.7 的 `gun:ensure_tls_opts/3`（`gun.erl:1296`）
   会自己补 `cacerts`（系统 CA）、HTTPS 主机名匹配和 SNI，OTP 26+ 的 `ssl` 默认 `verify_peer`，
   而 beamai_extra 的 `minimum_otp_vsn` 是 27。所以问题是「**安全全靠两层隐式默认、且无法定制**」：
   自签/企业 CA、mTLS 客户端证书、证书钉扎都配不了。另外 http 传输写死 `protocols => [http]`，
   用不上 h2。
4. **`beamai_mcp_adapter` 产出的工具 beamai_core 不认**：
   - `apps/beamai_mcp/src/adapter/beamai_mcp_adapter.erl:79`：`parameters => InputSchema`，把 MCP 的
     原始 JSON Schema（`#{<<"type">> => <<"object">>, <<"properties">> => ...}`）直接塞进 `parameters`。
     beamai `apps/beamai_core/src/core/beamai_tool.erl:236`（`to_tool_spec/1`）→ `:607`
     `build_json_schema/1` 把顶层的 `type`、`properties` 当成**参数名**，再对 `<<"object">>` 调
     `:620` `param_to_json_schema(#{type := Type} = Spec)` ⇒ `function_clause`。
   - `beamai_mcp_adapter.erl:80`/`:105`：`adapt_handler/1` 返回 `fun(Args) -> Text end`，**裸值**。
     beamai `apps/beamai_core/src/core/beamai_tool_executor.erl:66-74` 只认 `{ok, V}` / `{ok, V, W}` /
     `{error, R}` ⇒ `case_clause`。

## 怎么撞上的

1. `beamai_mcp_client:start_link(#{transport => http, url => <<"https://mcp.deepwiki.com/mcp">>, ...})`；
2. 进 `connecting`，`send_initialize` 发出 POST，`self() ! recv_loop`；
3. `recv(0)` ⇒ `after 0` ⇒ `gun:cancel` ⇒ `{error, timeout}` ⇒ 100 ms 后再轮询；
4. gun 回 `gun_error ... badstate` 到 statem mailbox ⇒ `connecting(info, {gun_error, ...}, _)` 无匹配 ⇒ 崩。

第 4 条（adapter）：`beamai_mcp_adapter:mcp_tool_to_beamai_tool/1` 的结果交给任何 beamai agent，
第一次 `to_tool_spec` 就崩；绕过去了，第一次工具调用也崩。

## 影响面

- 🔴 **远端 streamable-HTTP MCP 完全不可用**，而且是**崩进程**而不是返回 `{error, _}`；挂在
  `beamai_mcp_client_sup` 下还会被反复拉起、反复崩。本地 stdio 传输不受影响（不走 gun）。
- 🟢 `tls_opts`：今天的默认是安全的（见上），但内网 MCP（私有 CA）/ mTLS 接不上，只能改库。
- 🟡 adapter：MCP 工具接不进 beamai agent，`function_clause` / `case_clause`。
- 附带：`beamai_http` 也帮不上忙 —— 它强制 `Accept: application/json`，MCP 服务器按规范要求
  `application/json, text/event-stream`，回 406。

下游（beamai_muse）已自己写了一个 ~350 行的 gun 客户端绕开（`muse_mcp_http`），所以不挡我们；
但任何照着 README 用 `beamai_mcp_client` 接远端 MCP 的人都会撞上。

## 建议

1. ⭐ **不要用 `recv(0)` 轮询 gun**。gun 本来就是把消息推给 owner 的：让 gen_statem 直接处理
   `gun_response` / `gun_data` / `gun_error` / `gun_down` 这几类 info 事件（按 `StreamRef` 找 pending
   请求，SSE 按 `\n\n` 切事件、取 `data:` 行），删掉 `recv_loop`。最少也要：轮询超时**不要**
   `gun:cancel`，并给 `connecting/3` 补一条兜底 info 子句。
2. **TLS 可配置、且显式安全**：config 里接受 `tls_opts` 原样透传给 gun；不给时显式写上
   ```erlang
   tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()},
                {server_name_indication, binary_to_list(Host)},
                {customize_hostname_check,
                 [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]
   ```
   （与 gun 2.x 自动补的等价，但不再依赖隐式默认）；`protocols => [http2, http]`。
3. 每次请求的超时用调用方给的值（SDK 默认 60 s），超时才 cancel；再给响应体一个大小上限。
4. adapter：把 `inputSchema` 转成 beamai 的 param DSL（`type`/`description`/`required`/`enum`/
   `items`/`properties`，键用 atom），handler 改成 `fun(Args, Ctx) -> {ok, Text} | {error, Msg}`。
   或者干脆让 beamai_core 的 `parameters` 接受一份原样透传的 JSON Schema（例如
   `parameters => {json_schema, Map}`），MCP 这种「schema 已经是 JSON Schema」的场景就不用来回转。

可工作的最小流程（beamai_muse 实测）：

```erlang
{ok, C} = gun:open("mcp.deepwiki.com", 443, #{transport => tls, protocols => [http2, http], tls_opts => TlsOpts}),
{ok, _} = gun:await_up(C, 10000),
H = [{<<"content-type">>, <<"application/json">>},
     {<<"accept">>, <<"application/json, text/event-stream">>}],
S = gun:post(C, <<"/mcp">>, H, json:encode(#{jsonrpc => <<"2.0">>, id => 1, method => <<"initialize">>,
        params => #{protocolVersion => <<"2025-11-25">>, capabilities => #{},
                    clientInfo => #{name => <<"x">>, version => <<"1">>}}})),
{response, nofin, 200, RespH} = gun:await(C, S, 60000),
%% content-type text/event-stream ⇒ 逐个事件取 data: 行 json:decode，跳过 id 不符的（通知等）；
%% 否则 json:decode 整个 body。记下 mcp-session-id（若有），之后每个请求都带上它和
%% mcp-protocol-version；接着发 notifications/initialized（无 id，回 202），再 tools/list、tools/call。
```
