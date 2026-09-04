# 线程只读面四处与 AG-UI 对不上 ⇒ 会话抽屉实测「200 但一条都列不出」

> ⭐ 这一族的**能力本身是有的**(`/threads` 真的在答、`/state` 折得对),
> 差的只是**形状**。所以这条改起来应该很便宜。

## 症状

我们的会话抽屉打了 `GET /agui/threads`,拿到 **200**,却**一行都没渲染**。

## 逐条

### ① `GET /threads/:id` 忽略路径参数,恒回整张表

```
GET /agui/threads/bt-1          → {"threads":[ …6 条全部… ]}
GET /agui/threads/no-such-one   → 200,同样是那 6 条
```

⇒ 拿它当「读这一条主题的元信息」用是错的;而且**不存在的 id 也回 200**,
客户端没法据此判断「这条主题没了」。

### ② 线程记录不是 AG-UI 的形状

```json
{"threadId":"bt-1","lastActive":1788505494976,"runId":null,
 "seq":7,"state":"idle","stopping":false,"subscribers":0}
```

* 主键叫 **`threadId`**,而协议里是 **`id`** ⇒ 我们(和上游 web-inspector)
  按 `id` 归一化,于是每一条都被判成「没有 id 的坏记录」丢掉 —— **这就是 200 却 0 行的原因**;
* 缺 `name` / `createdAt` / `updatedAt` / `agentId` ⇒ 列表上没有标题、没有时间、
  没法按 agent 分;
* 混进了**运行时内部字段**(`seq` / `subscribers` / `stopping`)。
  ⛔ 不是说不能带,是说**该带的那几个先带上** —— 内部字段留着挺好(排障有用),
  但不该顶替协议字段的位置。

### ③ `/threads/:id/messages` 回 404,而 `/info` 报 `inspect: true`

```
GET /agui/threads/bt-1/messages → 404 {"error":"not_found"}
/info → threadEndpoints: {"list":true,"inspect":true,"mutations":true,…}
```

⇒ **能力位说谎**。上游 inspector 与我们都拿 `inspect` 这一格决定要不要去拉
`/messages`;报 true 而路由不存在,那一枪必然落空。
⭐ 你们的路由文档里 `:sub` 只认 `events` / `state` / `stop` / `stream` ——
那 `inspect` 就该报 false,或者把 `messages` 补上。

### ④ `/threads/:id/events` 的元素被包了一层

```json
{"events":[{"event":{"type":"RUN_STARTED", …},"seq":0}, …],"threadId":"bt-1"}
```

协议里这一格是**事件本身的数组**。包成 `{event, seq}` 之后,客户端拿到的每一条
都没有 `type`,事件表与时间线全部落空。
⭐ `seq` 很有用(续传要它),建议**放进事件对象里**当一个字段,而不是包在外面 ——
AG-UI 的事件 schema 是 passthrough 的,多一个字段老客户端会原样忽略。

## 影响面

🟡 不报错。表现为「会话列表是空的 / 主题详情打不开」,而 HTTP 全是 200 ——
排障时最容易怀疑到自己客户端头上(我们就先查了半天自己的归一化)。

## 建议

按性价比排:

1. **`threadId` → `id`**(或者两个都给)。⭐ 一个字段换回整张列表,最划算。
2. 补 `createdAt` / `updatedAt` / `agentId`;`name` 没有就给 `null`
   (我们会显示成「未命名」,那是对的)。
3. `/threads/:id` 认路径参数:回那一条;不存在回 404。
4. `inspect` 那一格与 `/messages` **二选一对齐**:要么补路由,要么报 false。
5. `/events` 去掉外层包装,`seq` 挪进事件对象。


---

## 已修（2026-09-04）

五条全做了。你说得对，能力都在，差的只是形状。

1. **`threadId` → `id`（两个都给）**。`id` 是主键，`threadId` 留着不动。
2. **补齐列表页那几格**：`name`（没有就 `null`）、`agentId`、`createdAt`、
   `updatedAt`。`agentId` 由路由那一层记进会话（它才知道这次路由到了谁）。
   内部字段（`seq` / `subscribers` / `stopping` / `runId` / `state`）留着 —— 你说
   排障有用，同意，它们只是不该顶替协议字段的位置。
3. **`GET /threads/:id` 认路径参数**：回那一条，不存在回 **404**。
4. **`/threads/:id/messages` 补上了**（`inspect` 那一格因此是真话）。用的是会话
   从事件流折出来的**消息账本** —— 它是这条流自己的投影，不是第二个对话历史
   （那在 chat memory 里）。所以它有界、也不保证是全量历史，这一点写进文档了。
5. **`/events` 去掉外层包装**，`seq` 挪进事件对象当一个字段（AG-UI 的事件 schema
   是 passthrough，老客户端原样忽略）。

实测：

```
GET /threads
  keys: [agentId, createdAt, id, lastActive, name, runId, seq,
         state, stopping, subscribers, threadId, updatedAt]
GET /threads/fb1           → 200
GET /threads/no-such       → 404
GET /threads/fb1/messages  → 200  {"messages":[{"role":"assistant", …}]}
GET /threads/fb1/events    → 元素形如 {type, seq, runId, threadId, timestamp}
```

覆盖：`beamai_agui_stream_tests` 的 `thread_shape`（含 404 与 messages），
`stop_endpoint` 里补了「元素是事件本身、`seq` 在里面」的断言。
