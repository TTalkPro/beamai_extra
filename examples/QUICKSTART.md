# Simple Agent 快速验证指南

## 快速开始

### 1. 设置环境变量

```bash
export ANTHROPIC_BASE_URL="https://api.anthropic.com"
export ANTHROPIC_AUTH_TOKEN="sk-ant-api03-..."
```

### 2. 启动 Shell

```bash
rebar3 shell
```

### 3. 运行测试

```erlang
%% 简单对话测试
test_anthropic_agent:run_simple().

%% 带工具的测试
test_anthropic_agent:run_with_tools().

%% 多轮对话测试
test_anthropic_agent:run_multi_turn().
```

## 配置说明

`get_llm_config/0` 函数会从环境变量读取配置：

| 环境变量 | 必填 | 默认值 | 说明 |
|---------|-----|--------|------|
| `ANTHROPIC_AUTH_TOKEN` | 是 | 无 | API 密钥 |
| `ANTHROPIC_BASE_URL` | 否 | `https://api.anthropic.com` | API 端点 |

返回的配置结构：
```erlang
#{
    provider => anthropic,
    api_key => <<"your-token">>,
    base_url => <<"https://api.anthropic.com">>,
    model => <<"claude-sonnet-4-20250514">>,
    max_tokens => 4096
}
```

## 兼容 API 示例

### Anthropic 官方
```bash
export ANTHROPIC_BASE_URL="https://api.anthropic.com"
export ANTHROPIC_AUTH_TOKEN="sk-ant-api03-..."
```

### 智谱 AI (兼容模式)
```bash
export ANTHROPIC_BASE_URL="https://open.bigmodel.cn/api/anthropic"
export ANTHROPIC_AUTH_TOKEN="your-zhipu-key"
```

### 自定义网关
```bash
export ANTHROPIC_BASE_URL="https://your-gateway.com/anthropic"
export ANTHROPIC_AUTH_TOKEN="bearer-token"
```

## 测试用例

### 1. run_simple/0
- 创建基本 Agent
- 两轮对话测试
- 验证基本响应能力

### 2. run_with_tools/0
- 创建带工具的 Agent
- 测试工具调用（计算、获取时间）
- 验证 function calling 能力

### 3. run_multi_turn/0
- 测试对话历史管理
- 验证 Agent 记忆能力
- 多轮上下文保持
