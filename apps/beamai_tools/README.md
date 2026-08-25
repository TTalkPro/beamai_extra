# BeamAI Tools

[English](README_EN.md) | 中文

工具系统模块，提供工具模块加载、Middleware 系统和内置工具集。

## 特性

- 工具模块加载和管理
- Middleware 拦截器链
- 预设 Middleware 配置
- 工具安全验证
- 内置工具模块（文件、Shell、TODO、人工交互）

## 模块概览

### 核心模块

- **beamai_tools** - 主 API（load/2, load_all/2, with_middleware/2, presets/1）
- **beamai_tool_security** - 工具调用安全验证

### Middleware 模块

- **beamai_middleware** - Middleware 行为定义
- **beamai_middleware_runner** - Middleware 链执行器
- **beamai_middleware_presets** - 预设配置（default, minimal, production, development, human_in_loop）
- **middleware_call_limit** - 调用次数限制
- **middleware_tool_retry** - 工具调用重试
- **middleware_model_retry** - 模型调用重试
- **middleware_model_fallback** - 模型降级
- **middleware_human_approval** - 人工审批

### 内置工具模块

- **beamai_tool_file** - 文件操作（读取、写入、列表、glob）
- **beamai_tool_shell** - Shell 命令执行
- **beamai_tool_todo** - 任务管理
- **beamai_tool_human** - 人机交互（interrupt tool 描述符，经 agent 的 `interrupt_tools` 使用，不注册进 ChatClient）

## API 文档

### beamai_tools

```erlang
%% 加载单个工具模块到 ChatClient
beamai_tools:load(ChatClient, beamai_tool_file) -> ChatClient1.

%% 批量加载工具模块
beamai_tools:load_all(ChatClient, [beamai_tool_file, beamai_tool_shell]) -> ChatClient1.

%% 添加 Middleware
beamai_tools:with_middleware(ChatClient, Middlewares) -> ChatClient1.

%% 获取预设 Middleware 配置
beamai_tools:presets(default) -> [MiddlewareSpec].
beamai_tools:presets(production) -> [MiddlewareSpec].

%% 列出可用的内置工具模块
beamai_tools:available() -> [ModuleName].

%% 定义工具
beamai_tools:define_tool(Name, Handler) -> ToolSpec.
beamai_tools:define_tool(Name, Handler, Opts) -> ToolSpec.
beamai_tools:define_tool(Name, Description, Handler, Parameters) -> ToolSpec.
beamai_tools:define_tool(Name, Description, Handler, Parameters, Opts) -> ToolSpec.
```

## 使用示例

### 基本使用

```erlang
%% 创建 ChatClient 并加载工具
ChatClient = beamai_chat_client:new(),
ChatClient1 = beamai_tools:load_all(ChatClient, [beamai_tool_file, beamai_tool_shell]),

%% 获取工具规格供 Agent 使用
Tools = beamai_chat_client:get_tool_specs(ChatClient1).
```

### 使用 Middleware

```erlang
%% 使用预设 Middleware
ChatClient = beamai_chat_client:new(),
ChatClient1 = beamai_tools:load_all(ChatClient, [beamai_tool_file]),
ChatClient2 = beamai_tools:with_middleware(ChatClient1,
    beamai_middleware_presets:production()),

%% 或使用自定义 Middleware 配置
ChatClient3 = beamai_tools:with_middleware(ChatClient1, [
    {middleware_call_limit, #{max_model_calls => 20}},
    {middleware_tool_retry, #{max_retries => 3}}
]).
```

### 自定义工具

```erlang
%% 使用 beamai_tools:define_tool 创建工具
Tool = beamai_tools:define_tool(
    <<"get_weather">>,
    <<"Get weather for a city">>,
    fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25}}
    end,
    #{
        type => object,
        properties => #{
            <<"city">> => #{type => string, description => <<"City name">>}
        },
        required => [<<"city">>]
    }
),

ChatClient = beamai_chat_client:new(),
ChatClient1 = beamai_chat_client:add_tools(ChatClient, [Tool]).
```

## Middleware 预设

| 预设 | 包含 | 描述 |
|------|------|------|
| `default` | call_limit + model_retry | 默认配置 |
| `minimal` | call_limit | 最小配置 |
| `production` | call_limit + tool_retry + model_retry + model_fallback | 生产环境 |
| `development` | call_limit (宽松) + tool_retry | 开发调试 |
| `human_in_loop` | call_limit + human_approval | 人工审批 |

## 依赖

- beamai_core（ChatClient、工具行为定义）
- jsx（JSON 编解码）

## 许可证

Apache-2.0
