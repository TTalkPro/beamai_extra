# 测试 Anthropic Simple Agent

## 环境变量设置

在运行测试之前，需要设置以下环境变量：

```bash
export ANTHROPIC_BASE_URL="https://api.anthropic.com"
export ANTHROPIC_AUTH_TOKEN="your-anthropic-api-key-here"
```

或者使用兼容的 API（如智谱 AI 的 Anthropic 兼容接口）：

```bash
export ANTHROPIC_BASE_URL="https://open.bigmodel.cn/api/anthropic"
export ANTHROPIC_AUTH_TOKEN="your-zhipu-api-key"
```

或者使用其他兼容 API 端点：

```bash
# 自定义代理或网关
export ANTHROPIC_BASE_URL="https://your-gateway.com/v1/anthropic"
export ANTHROPIC_AUTH_TOKEN="your-token"
```

## 编译和启动

```bash
# 编译项目
rebar3 compile

# 启动 Erlang shell
rebar3 shell
```

## 运行测试

在 Erlang shell 中执行以下命令：

### 1. 简单对话测试

```erlang
test_anthropic_agent:run_simple().
```

这个测试会：
- 创建一个 Simple Agent
- 进行两轮对话
- 验证 Agent 的基本功能

### 2. 带工具的测试

```erlang
test_anthropic_agent:run_with_tools().
```

这个测试会：
- 创建一个带有工具的 Agent
- Agent 可以获取当前时间和进行计算
- 验证工具调用功能

### 3. 多轮对话测试

```erlang
test_anthropic_agent:run_multi_turn().
```

这个测试会：
- 创建一个有记忆功能的 Agent
- 测试 Agent 是否能记住之前对话中的信息
- 验证多轮对话的一致性

## 预期输出

### 简单对话测试

```
=== Testing Simple Agent with Anthropic ===

LLM Config:
  Provider: anthropic
  Base URL: https://api.anthropic.com
  Model: claude-sonnet-4-20250514

Agent created successfully!
Agent ID: <agent-id>

--- Turn 1 ---
User: Hello! Please introduce yourself briefly.

Assistant: I'm Claude, an AI assistant made by Anthropic...
=== Test completed successfully! ===
```

### 带工具测试

```
=== Testing Agent with Tools ===

Agent created with 2 tools

User: What's 25 multiplied by 4? Also, what's the current time?

Assistant: 25 multiplied by 4 is 100. The current time is...
Tools called: [...]
=== Test completed successfully! ===
```

### 多轮对话测试

```
=== Testing Multi-Turn Conversation ===

Agent created

--- Turn 1: Storing information ---
User: My name is Alice and I love Erlang programming.

Assistant: Got it!

--- Turn 2: Testing memory ---
User: What's my name and what programming language do I love?

Assistant: Your name is Alice and you love Erlang programming.
Total messages in history: 6
=== Test completed successfully! ===
```

## 故障排查

### 错误：missing_env

```
Error: {missing_env, "ANTHROPIC_AUTH_TOKEN"}
```

**解决方案**：确保设置了 `ANTHROPIC_AUTH_TOKEN` 环境变量。

### 错误：connection refused

```
Error: econnrefused
```

**解决方案**：
1. 检查网络连接
2. 验证 `ANTHROPIC_BASE_URL` 是否正确
3. 确认 API 端点可访问

### 错误：authentication

```
Error: {http_error, 401, ...}
```

**解决方案**：
1. 检查 `ANTHROPIC_AUTH_TOKEN` 是否正确
2. 确认 API Token 有效且有足够配额

### 错误：module not found

```
Error: module 'beamai_agent' not found
```

**解决方案**：
1. 确保项目已编译：`rebar3 compile`
2. 检查 beamai 依赖是否正确下载
3. 尝试清理重编译：`rebar3 clean && rebar3 compile`

## 调试技巧

### 启用详细日志

在 Erlang shell 中：

```erlag
%% 设置日志级别
logger:set_primary_config(level, all).

%% 重新运行测试
test_anthropic_agent:run_simple().
```

### 查看 LLM 配置

```erlang
%% 获取配置
Config = test_anthropic_agent:get_llm_config().
```

### 检查 Agent 状态

```erlang
%% 创建 Agent
{ok, Agent} = beamai_agent:new(#{
    llm => test_anthropic_agent:get_llm_config(),
    system_prompt => <<"You are helpful.">>
}).

%% 查看 Agent 信息
beamai_agent:id(Agent).
beamai_agent:messages(Agent).
beamai_agent:turn_count(Agent).
```
