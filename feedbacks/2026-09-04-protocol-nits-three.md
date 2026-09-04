# 三条小的:`STEP_FINISHED` 插在未闭合的生命周期里、`CUSTOM` 与 outcome 重复、`/info` 没有 `multimodal`

> 三条都不挡事,记下来是因为它们各自会让某一类客户端多绕一点。

## ① `STEP_FINISHED` 落在**还没闭合**的消息 / 工具生命周期中间

实测两段帧序(逐字):

```
纯文本:  … TEXT_MESSAGE_START → TEXT_MESSAGE_CONTENT → STEP_FINISHED → TEXT_MESSAGE_END → RUN_FINISHED
带工具:  … TOOL_CALL_START → TOOL_CALL_ARGS → STEP_FINISHED → TEXT_MESSAGE_END → TOOL_CALL_END → CUSTOM → RUN_FINISHED
```

⚠️ 第二段里 `STEP_FINISHED` 夹在 `TOOL_CALL_ARGS` 与 `TOOL_CALL_END` **中间**,
`TEXT_MESSAGE_END` 也跑到了步骤收口之后。

**影响面**:🟢 宽容的客户端(我们、上游)照样能重建,因为消息与工具都按 id 归组、
不依赖步骤边界。但**按步骤分组渲染**的客户端(「这一步做了什么」这种视图)
会看到一个「里面还开着两个东西」的步骤。

**建议**:收口顺序改成 `TEXT_MESSAGE_END` / `TOOL_CALL_END` 在前、
`STEP_FINISHED` 在后 —— 步骤是**容器**,容器该最后关。

## ② `CUSTOM name:"interrupt"` 与 `outcome.interrupts` 是同一份

```json
{"type":"CUSTOM","name":"interrupt",
 "value":{"type":"tool_request","toolCallId":"call_…","toolCallName":"sayHello","args":{…}}}
{"type":"RUN_FINISHED","outcome":{"interrupts":[{"id":"call_…","reason":"tool_request", …}]}}
```

同一件事发了两遍。**影响面**:🟢 上游客户端对不认识的 `CUSTOM.name` 一律忽略,
所以不会错;但接入方看日志时会以为是两件事,而且两份**有可能哪天不同步**。

**建议**:留 `outcome` 那一份(它是协议的一等表达),`CUSTOM` 那份要么删,
要么在文档里写明「私有镜像,客户端别读」。

## ③ `/info` 里没有 `multimodal` 能力格

`agents.*.capabilities` 现在有 `transport` / `tools` / `reasoning` / `state` /
`humanInTheLoop` 五类,**没有 `multimodal`**。

**影响面**:🟢 缺席在客户端那儿只能读成「不知道」,而按「不知道就是 false」的
惯例 ⇒ **附件入口不会出现**。也就是说:这台就算能收图,下游也不会让用户传。

**建议**:如实报一格就行,例如

```json
"multimodal": {"input": {"image": false, "pdf": false, "audio": false, "video": false},
               "output": {"image": false, "audio": false}}
```

⭐ 报 `false` 是**有用的信息**(下游据此把入口收起来,是确定的行为);
缺席才是那个让人为难的状态。⚠️ 反过来也别报做不到的:另一台参照实现在这一格上
报过 `image:true` 而模型没有视觉,下游把附件入口打开、用户传了图、模型说看不见 ——
那比不报更贵。


---

## 已修（2026-09-04）

三条都做了。

### ① `STEP_FINISHED` 挪到容器最后

「步骤是容器，容器该最后关」—— 这句话直接写进代码注释了。

成因是回调的触发顺序：`on_llm_result` 在 LLM 一返回就触发，而这一步产出的消息与
工具调用要等 `on_message_end` 才闭合。现在 `on_llm_result` **只记一笔欠账**，
由 `flush_step/1` 在三个时机补发：消息闭合之后、下一步开始之前、run 到终态之前。

```
现在: … TOOL_CALL_START → TOOL_CALL_ARGS → TOOL_CALL_END → STEP_FINISHED → …
      … TEXT_MESSAGE_START → CONTENT → TEXT_MESSAGE_END → STEP_FINISHED → RUN_FINISHED
```

覆盖：`beamai_agui_emitter_tests` 的 `step_finished_closes_after_the_message` 与
`step_finished_flushed_at_terminal`（run 崩了/停了也要把欠着的那条补上，否则步骤
永远不闭合）。九处既有的序列断言随之更新。

### ② `CUSTOM name:"interrupt"` 删了

留 `outcome` 那一份。你那句「两份**有可能哪天不同步**」是对的 —— 与其写一行
「私有镜像，别读」的文档，不如让它没有。

### ③ `/info` 补了 `multimodal`

如实报一份全 false：

```json
"multimodal": {"input": {"image": false, "pdf": false, "audio": false, "video": false},
               "output": {"image": false, "audio": false}}
```

装配方可以用端点配置 `multimodal` 覆盖 —— 但**必须是装配方来传**，本层不猜：
能不能收附件取决于 wire 认不认部件**且模型本身有视觉**，后一条运行时判不了。
你提的那个反例（报 `image:true` 而模型没视觉）写进代码注释了，就当是个警示牌。
