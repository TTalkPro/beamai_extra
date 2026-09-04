# `resume[].payload.decision` 完全不读 + 替客户端工具**伪造结果** ⇒ 审批与前端工具两件事都不成立

## 症状

同一条 interrupt,只把 `decision` 换一个词,**其余逐字相同**:

```
resume [{interruptId: …, status:"resolved", payload:{decision:"approved"}}]
  → TOOL_CALL_RESULT {"role":"tool","toolCallId":"…","content":"resolved"}
  → 模型:「已经和 Zoe 打过招呼啦 👋」

resume [{interruptId: …, status:"resolved", payload:{decision:"rejected"}}]
  → TOOL_CALL_RESULT {"role":"tool","toolCallId":"…","content":"resolved"}   ← 一模一样
  → 模型:「已经跟 Zoe 打过招呼啦 👋」                                        ← 一模一样
```

两件事各自都是账,叠在一起更糟:

### ① `decision` 不读,`status` 被当成了结果

那条 `content` 是 `"resolved"` —— 正是 resume 条目里 **`status` 字段的值**。
⇒ 载荷里的 `decision` 一眼都没看。**「拒绝」与「批准」在这台上是同义词。**

### ② 那条结果是**替客户端工具伪造的**

`sayHello` 是**客户端声明**的工具(`RunAgentInput.tools` 里带上来的,服务端没有实现)。
AG-UI 里这类工具的正路是:服务端发 `TOOL_CALL_*` → 收口 → **客户端执行** →
下一轮把 tool 结果带回来。而这里服务端自己造了一条 `TOOL_CALL_RESULT` 塞给模型,
**客户端从头到尾没被调用过**。⇒ 模型据此宣布「打过招呼了」,而实际上一个招呼都没打。

### ③ 附带:前端工具的 interrupt 用的是**审批**的 schema

```json
{"reason":"tool_request",
 "responseSchema":{"properties":{"decision":{"enum":["approved","rejected"]}},"required":["decision"]},
 "metadata":{"pendingTool":{"name":"sayHello","args":{"name":"David"}}}}
```

它要的其实是**这次调用的结果**,不是决策。客户端照 schema 填 `{decision}`,
自然就填不出结果来 —— ①② 的成因有一半在这里。

## 怎么撞上的

1. `tools` 里声明一个纯客户端工具(`sayHello`,服务端无实现);
2. 「用 sayHello 跟 Zoe 打个招呼」⇒ 收到上面那条 interrupt;
3. 分别用 `decision: approved` / `rejected` 各回一次 ⇒ 两次结果逐字相同。

## 影响面

🔴 **一个字都不报**,而且症状具有强误导性:

* 界面上「批准 / 拒绝」两颗钮点哪个都一样 —— 用户以为拒绝生效了;
* 模型确信工具跑过了,后续推理全建立在一个**没有发生过的结果**上;
* 客户端工具这条能力在这台上等于不存在(`/info` 却报 `tools.clientProvided: true`)。

## 建议

1. 🔴 **读 `decision`**。`rejected` 时不要发 `TOOL_CALL_RESULT`,或者发一条明说
   被拒的结果(内容里带上原因),让模型知道这一步没做成。
   ⛔ 别把 `status` 当结果用:`status` 是**协议层**的「这条 interrupt 答完了」,
   与「工具返回了什么」是两回事。
2. 🔴 **客户端工具不要伪造结果**。挂起之后等客户端把 tool 消息带回来 ——
   你们路由里已经有会话与 seq,续跑的材料是现成的。
3. ⭐ **把两类 interrupt 分开**(另一台参照实现刚做完这件事,可直接抄):
   * `reason` 前缀分开:`需要审批: X` vs `需要客户端执行并回传结果: X`;
   * `responseSchema` 如实反映那一支要什么:`{decision: approved|rejected}` vs `{result: …}`;
   * `metadata` 加一格 `kind`:`"approval"` / `"frontend-tool"`。

   ⭐⭐ 三条里**第二条最省事**:客户端照 schema 填就自然回对了,不用把
   「这是哪一类」编进自己的 if。我们客户端已经按这三条实现了判别
   (优先认 `metadata.kind`,没有就按 `responseSchema` 的属性名推),
   你们照着补,我们这边一行都不用改。


---

## 已修（2026-09-04）

三条全做了。这是这批里最要紧的一条 —— 谢谢把「两次逐字相同」这个证据摆出来，
没有它很难相信「批准和拒绝是同义词」。

### ① `decision` 现在真的读了，`status` 不再当结果

取值顺序改成 `payload.decision` → 条目的 `decision` → `status`
**且仅在它字面上就是一个决策词时**（有客户端确实发 `[{status:"approved"}]`）。
`"resolved"` / `"completed"` 这类协议层措辞一律不当决策。

同样要紧的是**认不出的词不再原样透传**：以前认不出就往下传，上游把不认识的
binary 当"自由文本作结果"，于是 `"resolved"` 就成了工具的返回值。现在认不出就是
`undefined`，走自由文本那条路必须是**人真的打了一句话**（最后一条 user 消息），
不是某个字段恰好是字符串。

### ② 不再替客户端工具伪造结果

runner 现在分得清两类 interrupt（判据：被中断的工具在不在 `RunAgentInput.tools`
里）。前端工具那一支：

* `payload.result` / `role=tool` 消息 / 人打的一句话 → 用它作结果；
* 只回了个决定、没给结果 → **如实说没拿到**
  （`[客户端没有回传这次调用的结果 —— 前端工具的答复应当放在 resume[].payload.result 里]`），
  不编一个成功出来；
* `rejected` → 拒绝语，模型知道这一步没做成。

### ③ 两类 interrupt 分开了 —— 三处都说，你们认哪个都行

| | 审批 | 前端工具 |
|---|---|---|
| `reason` | `需要审批: X` | `需要客户端执行并回传结果: X` |
| `responseSchema.required` | `["decision"]` | `["result"]` |
| `metadata.kind` | `"approval"` | `"frontend-tool"` |

`metadata` 里还多一格 `interruptType`（上游那个内部的中断类型），排障用 ——
**别拿它做判别**，判别用上面三个。

覆盖：`beamai_agui_resume_tests` 的 `approved_and_rejected_differ`（断言两次结果
**不相等**）、`status_is_not_a_decision`、`frontend_tool_asks_for_a_result`、
`frontend_tool_result_is_not_fabricated`。
