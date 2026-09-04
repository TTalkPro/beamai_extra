# run 端点不在 `{base}/agent/{id}/run` ⇒ 标准 AG-UI 客户端连不上,而且报的是 **CORS 错**

## 症状

浏览器控制台里的那一行:

```
Access to fetch at 'http://localhost:8080/agui/agent/default/run' from origin
'http://localhost:3002' has been blocked by CORS policy: Response to preflight
request doesn't pass access control check: No 'Access-Control-Allow-Origin'
header is present on the requested resource.
```

⚠️ **这条报文指错了方向**:CORS 本身是好的(`/agui/run` 的预检回 204、
`Allow-Origin: *`、`Allow-Methods: GET, POST, DELETE, OPTIONS`,连
`last-event-id` 都在 `Allow-Headers` 里)。真正的原因是**那条路由不存在** ——
而 404 那条路径上没有 CORS 头,于是浏览器把「没这条路」显示成「跨源被拒」。

## 位置

`apps/beamai_agui/src/beamai_agui_cowboy_handler.erl` 顶部那张示例路由表:

```erlang
{"/agui/run",             beamai_agui_cowboy_handler, #{agent => <<"my-agent">>}},
{"/agui/info",            beamai_agui_cowboy_handler, #{mode => info}},
```

agent 是**绑在路由上**的(一条路由一个 agent),所以只有一条扁平的 `/run`。

## 怎么撞上的

1. 客户端 `GET {base}/info` ⇒ 拿到 `agents: {"beamai-demo": {...}}`;
2. 照 AG-UI / CopilotKit 的约定拼 run 地址:`{base}/agent/{agentId}/run`;
3. 404(浏览器里显示成 CORS 错)。

⚠️ **`/info` 宣告了 agents 字典,却没有按 agent 寻址的 run 路径** —— 这两件事
一起出现时,客户端没有别的读法:上游的 `AbstractAgent` / CopilotKit runtime、
它那个 web-inspector、以及我们,都是这么拼的。

## 影响面

🔴 **整台运行时在标准客户端上开不了口**。绕法只有「宿主自己知道那台的 run 路径,
硬编码进客户端」——那正是 `/info` 该消灭的东西。

⚠️ 而且失败现场**指错方向**:同事拿到那行 CORS 报文,第一反应一定是去调 CORS,
而 CORS 是这台里做得最齐的一块。

## 建议(按侵入性从小到大)

1. ⭐ **最小**:再挂一条别名路由,让 `{base}/agent/:agent/run` 与 `{base}/run`
   指同一个 handler(`:agent` 与路由上绑的那个对不上时回 404 + 一句人话)。
   宿主那张示例表里加一行就行,库不用动。
2. ⭐⭐ **顺带**:给 404 也加上 CORS 头。这样「没这条路」就会如实显示成 404,
   而不是伪装成跨源问题。**这条比 ① 更值** —— 它让所有走错路的情况都能自己说清。
3. 可选:`/info` 里明写 run 的地址(例如每个 agent 一个 `runUrl`)。
   ⛔ 这不是 AG-UI 现有字段,只有在你们打算多 agent 共用一个端口时才值得。


---

## 已修（2026-09-04）

两条都做了 —— 你说「② 比 ① 更值」，同意，两条一起才把这个坑填平。

1. **按 agent 寻址的 run 路径**。`beamai_agui_cowboy_handler` 现在认路径里的
   `:agent`，优先于路由上钉死的那个：一条
   `{"/agui/agent/:agent/run", beamai_agui_cowboy_handler, #{}}` 就服务
   `/info` 里报的所有 agent。名字在注册表里查不到时如实 404
   （`{"error":"agent_not_found"}`），不再回落到路由钉死的那个 —— 悄悄换个 agent
   跑比 404 更难查。
2. **404 带 CORS**。新增 `mode => fallback`，挂在路由表最后
   （`{"/agui/[...]", ..., #{mode => fallback}}`）。cowboy 自己那条「没有路由匹配」
   的 404 走不到 handler，所以这条得由宿主挂上 —— 库的路由示例、
   `example_agui_server` 都加了。404 的响应体里还带一句 `hint`，指向 `/agui/info`
   与那条 run 路径。

第三条（`/info` 里明写 `runUrl`）**没做**：不是 AG-UI 现有字段，而按 agent 寻址
的路径已经把问题解决了，多一个非标字段只会多一处要维护的约定。

实测：

```
POST /agui/agent/beamai-demo/run       → 200（SSE 正常）
POST /agui/agent/nope/nonsense         → 404 + access-control-allow-origin: http://localhost:3002
                                          {"error":"not_found","hint":"…见 GET /agui/info…"}
```

覆盖：`beamai_agui_stream_tests` 的 `per_agent_run` / `fallback_has_cors`。
