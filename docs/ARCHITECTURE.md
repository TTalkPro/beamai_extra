# BeamAI Extra 架构设计

[English](ARCHITECTURE_EN.md) | 中文

本文档详细描述 BeamAI Extra 项目的整体架构、核心设计理念和各模块的职责。

**注意**: 本项目现在依赖外部 [BeamAI 核心库](https://github.com/TTalkPro/beamai) 提供核心功能（beamai_core、beamai_llm、beamai_memory）。本项目专注于扩展功能，包括 Agent 实现、协议支持、工具系统和 RAG。

## 目录

- [架构概览](#架构概览)
- [设计原则](#设计原则)
- [分层架构](#分层架构)
- [核心模块](#核心模块)
- [执行引擎](#执行引擎)
- [工具系统](#工具系统)
- [Middleware 系统](#middleware-系统)
- [存储与持久化](#存储与持久化)
- [协议支持](#协议支持)
- [数据流](#数据流)
- [扩展机制](#扩展机制)

---

## 架构概览

BeamAI Extra 是一个基于 Erlang/OTP 的高性能 AI Agent 扩展框架，采用分层架构设计。核心功能由外部 [BeamAI 库](https://github.com/TTalkPro/beamai) 提供，本项目专注于 Agent 实现、协议支持和扩展功能。

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      应用层 (Applications - 本项目)                     │
│  ┌─────────────────┐  ┌─────────────────┐                              │
│  │  beamai_agent   │  │ beamai_deepagent│                              │
│  │  (Simple Agent) │  │  (Deep Agent)   │                              │
│  └────────┬────────┘  └────────┬────────┘                              │
└───────────┼─────────────────────┼───────────────────────────────────────┘
            │                     │
┌───────────┼─────────────────────┼───────────────────────────────────────┐
│           │          服务层 (Services - 本项目)                        │
│  ┌────────┴────────┐  ┌────────┴────────┐  ┌──────────────────────┐    │
│  │   beamai_llm    │  │   beamai_rag    │  │    beamai_tools      │    │
│  │  (LLM 客户端)*  │  │  (RAG 功能)     │  │(工具库 + 中间件系统) │    │
│  │  *来自 BeamAI   │  │                 │  │                       │    │
│  └─────────────────┘  └─────────────────┘  └──────────────────────┘    │
│  ┌─────────────────┐  ┌─────────────────┐                             │    │
│  │   beamai_a2a    │  │   beamai_mcp    │                             │    │
│  │  (A2A 协议)     │  │  (MCP 协议)     │                             │    │
│  └─────────────────┘  └─────────────────┘                             │    │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │
┌────────────────────────────────┼────────────────────────────────────────┐
│              核心层 (Core - 来自外部 BeamAI 依赖)                       │
│  ┌─────────────────────────────┴─────────────────────────────────────┐  │
│  │                        beamai_core                                 │  │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────────┐   │  │
│  │  │   Graph   │  │  Pregel   │  │   Kernel  │  │   HTTP/SSE    │   │  │
│  │  │   引擎    │  │   计算    │  │   核心    │  │   客户端      │   │  │
│  │  └───────────┘  └───────────┘  └───────────┘  └───────────────┘   │  │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────┐                       │  │
│  │  │  Process  │  │  Behav... │  │   Utils   │                       │  │
│  │  │  框架     │  │  行为定义 │  │   工具    │                       │  │
│  │  └───────────┘  └───────────┘  └───────────┘                       │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                       beamai_memory                                │  │
│  │  ┌───────────┐  ┌───────────┐  ┌───────────────────────────────┐  │  │
│  │  │Checkpoint │  │   Store   │  │      Context Store            │  │  │
│  │  │   管理    │  │   存储    │  │      (ETS/SQLite)             │  │  │
│  │  └───────────┘  └───────────┘  └───────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                    https://github.com/TTalkPro/beamai                   │
└─────────────────────────────────────────────────────────────────────────┘
```

**依赖说明**:
- **本项目管理**: Agent 实现、协议支持（A2A、MCP）、工具系统、RAG 功能
- **BeamAI 提供**: Kernel 核心、Process 框架、Graph 引擎、LLM 客户端、Memory 存储

---

## 设计原则

### 1. OTP 设计模式

- **进程隔离**: 每个 Agent 运行在独立进程中，故障不会影响其他 Agent
- **监督树**: 使用 Supervisor 管理进程生命周期
- **Behaviour**: 使用 behaviour 定义标准接口，如 `beamai_behaviour`、`beamai_tool_provider`、`beamai_middleware`

### 2. 关注点分离

- **核心层**: 提供基础能力，不依赖具体业务
- **服务层**: 提供 LLM、RAG、协议等服务
- **应用层**: 实现具体的 Agent 逻辑

### 3. 可插拔架构

- **Provider 机制**: LLM、工具、存储都支持多种 Provider
- **Middleware 机制**: 通过中间件扩展 Agent 行为
- **策略可配置**: 冲突解决、重试策略等可自定义

### 4. 高性能设计

- **轻量级进程**: 利用 Erlang 轻量级进程实现高并发
- **ETS 存储**: 使用 ETS 实现高速内存存储
- **连接池**: HTTP 请求使用 Hackney 连接池

---

## 分层架构

### 核心层 (来自外部 BeamAI 依赖)

提供框架的基础能力，由 [BeamAI](https://github.com/TTalkPro/beamai) 项目维护：

| 模块 | 来源 | 职责 |
|------|------|------|
| `beamai_core` | BeamAI | Graph 执行引擎、Pregel 计算、Kernel 核心、Process 框架 |
| `beamai_llm` | BeamAI | 多 Provider LLM 客户端、Output Parser |
| `beamai_memory` | BeamAI | 记忆和检查点管理、存储后端 |

详细文档请访问: https://github.com/TTalkPro/beamai

### 服务层 (本项目)

提供 AI 相关的核心服务：

| 模块 | 职责 |
|------|------|
| `beamai_tools` | 工具注册表、Provider 机制、Middleware 系统 |
| `beamai_rag` | 向量嵌入和检索增强生成 |
| `beamai_a2a` | Agent-to-Agent 通信协议 |
| `beamai_mcp` | Model Context Protocol 实现 |

### 应用层 (本项目)

实现具体的 Agent 模式：

| 模块 | 职责 |
|------|------|
| `beamai_agent` | ReAct 模式 Agent，支持 Middleware |
| `beamai_deepagent` | 递归规划 Agent，支持子任务分发 |

---

## 核心模块

### 外部依赖模块 (来自 BeamAI)

#### beamai_core

```
beamai_core (外部依赖)
├── behaviours/                   # 行为定义
│   ├── beamai_llm_behaviour.erl
│   ├── beamai_http_behaviour.erl
│   ├── beamai_step_behaviour.erl
│   └── beamai_process_store_behaviour.erl
├── kernel/                        # Kernel 核心
│   ├── beamai_kernel.erl         # Kernel 主模块
│   ├── beamai_tool.erl           # 工具定义
│   ├── beamai_context.erl        # 上下文管理
│   ├── beamai_filter.erl         # 过滤器
│   ├── beamai_prompt.erl         # 提示词
│   └── beamai_result.erl         # 结果类型
├── graph/                         # Graph 执行引擎
│   ├── graph.erl                 # 图结构
│   ├── graph_builder.erl         # 图构建器
│   ├── graph_runner.erl          # 图执行器
│   ├── graph_state.erl           # 状态管理
│   ├── graph_node.erl            # 节点定义
│   ├── graph_edge.erl            # 边定义
│   └── graph_dsl.erl             # DSL 支持
├── pregel/                        # Pregel 分布式计算
│   ├── pregel.erl                # Pregel 核心
│   ├── pregel_master.erl         # 主控节点
│   ├── pregel_worker.erl         # 工作节点
│   ├── pregel_vertex.erl         # 顶点逻辑
│   └── pregel_barrier.erl        # 屏障同步
├── process/                       # Process 框架
│   ├── beamai_process.erl        # 流程定义
│   ├── beamai_process_builder.erl
│   ├── beamai_process_runtime.erl
│   ├── beamai_process_step.erl
│   └── beamai_process_executor.erl
└── http/                          # HTTP 客户端
    ├── beamai_http.erl           # HTTP 抽象
    ├── beamai_http_gun.erl       # Gun 实现
    ├── beamai_http_hackney.erl   # Hackney 实现
    └── beamai_http_pool.erl      # 连接池
```

#### beamai_llm

```
beamai_llm (外部依赖)
├── beamai_chat_completion.erl    # LLM 聊天完成
├── parser/                        # 输出解析器
│   ├── beamai_output_parser.erl
│   ├── beamai_parser_json.erl
│   └── beamai_parser_retry.erl
├── providers/                     # LLM Provider
│   ├── llm_provider_openai.erl
│   ├── llm_provider_anthropic.erl
│   ├── llm_provider_deepseek.erl
│   ├── llm_provider_zhipu.erl
│   └── llm_provider_ollama.erl
└── adapters/                      # 消息适配器
    ├── llm_message_adapter.erl
    ├── llm_response_adapter.erl
    └── llm_tool_adapter.erl
```

#### beamai_memory

```
beamai_memory (外部依赖)
├── beamai_memory.erl             # 主模块
├── checkpointer/                  # 检查点管理
│   ├── beamai_checkpointer.erl
│   └── (实现模块)
├── store/                         # 存储后端
│   ├── beamai_store.erl
│   ├── beamai_store_ets.erl
│   └── beamai_store_sqlite.erl
└── snapshot/                      # 快照管理
    └── beamai_snapshot.erl
```

详细文档: https://github.com/TTalkPro/beamai

---

## 执行引擎

### Graph 执行引擎

Graph 引擎是 Agent 执行的核心，采用有向图模型：

```
                    ┌─────────────┐
                    │   Entry     │
                    │  (start)    │
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
             ┌──────│    LLM      │──────┐
             │      │   Node      │      │
             │      └─────────────┘      │
             │                           │
        has_tools?                  no_tools?
             │                           │
             ▼                           ▼
      ┌─────────────┐            ┌─────────────┐
      │   Tools     │            │    End      │
      │   Node      │            │  (finish)   │
      └──────┬──────┘            └─────────────┘
             │
             └───────────────┐
                             │
                             ▼
                      ┌─────────────┐
                      │    LLM      │
                      │   Node      │
                      └─────────────┘
```

#### 关键组件

1. **graph_builder**: 构建图结构
2. **graph_runner**: 执行图，管理状态流转
3. **graph_state**: 在节点间传递的状态对象
4. **graph_node**: 节点执行逻辑

#### 状态流转

```erlang
%% 状态结构
State = #{
    messages => [Message],           %% 对话历史
    tools => [ToolDef],              %% 可用工具
    pending_tools => [ToolCall],     %% 待执行工具调用
    tool_results => [ToolResult],    %% 工具执行结果
    last_llm_response => Response,   %% 最近 LLM 响应
    iteration => N,                  %% 当前迭代次数
    %% ... 其他状态
}.
```

---

## 工具系统

### Provider 机制

工具系统采用 Provider 模式，支持多种工具来源：

```
┌─────────────────────────────────────────────────────────────────┐
│                     beamai_tool_registry                         │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    from_config/1                           │  │
│  │  providers => [                                            │  │
│  │      {beamai_deepagent_tool_provider, Config},            │  │
│  │      {beamai_tool_provider_mcp, #{server => ...}},        │  │
│  │      beamai_tool_provider_builtin                          │  │
│  │  ]                                                         │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
            ┌─────────────────┼─────────────────┐
            │                 │                 │
            ▼                 ▼                 ▼
┌───────────────────┐ ┌───────────────┐ ┌───────────────┐
│beamai_deepagent_  │ │beamai_tool_   │ │beamai_tool_   │
│  tool_provider    │ │provider_mcp   │ │provider_builtin│
├───────────────────┤ ├───────────────┤ ├───────────────┤
│ - base_tools      │ │ - MCP Server  │ │ - file tools  │
│ - plan_tools      │ │   连接        │ │ - shell tools │
│ - subtask_tools   │ │ - 远程工具    │ │ - todo tools  │
│ - reflect_tools   │ │   调用        │ │ - human tools │
│ - fs_tools        │ │               │ │               │
│ - todo_tools      │ │               │ │               │
│ - human_tools     │ │               │ │               │
└───────────────────┘ └───────────────┘ └───────────────┘
```

### Provider 行为定义

```erlang
-behaviour(beamai_tool_provider).

%% 必需回调
-callback list_tools(Opts :: map()) -> {ok, [tool_def()]} | {error, term()}.

%% 可选回调
-callback find_tool(Name :: binary(), Opts :: map()) -> {ok, tool_def()} | {error, not_found}.
-callback info() -> map().
-callback available() -> boolean().
```

### 工具定义格式

```erlang
#{
    name => <<"tool_name">>,
    description => <<"工具描述">>,
    parameters => #{
        type => object,
        properties => #{
            <<"param1">> => #{type => string, description => <<"...">>}
        },
        required => [<<"param1">>]
    },
    handler => fun(Args) -> {ok, Result} end,
    %% 可选字段
    category => file | shell | todo | human | plan | custom,
    permissions => [file_read, file_write, shell_access],
    metadata => #{}
}
```

---

## Middleware 系统

Middleware 系统位于 `beamai_tools` 模块中，提供 Agent 执行过程中的拦截、增强和控制能力。
beamai_agent 和 beamai_deepagent 都通过依赖 beamai_tools 来使用 Middleware 系统。

### 执行流程

```
┌─────────────────────────────────────────────────────────────────┐
│                        Agent 执行                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────────┐                                           │
│  │   before_agent   │  ← Agent 开始前                            │
│  └────────┬─────────┘                                           │
│           │                                                      │
│           ▼                                                      │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                     Agent Loop                            │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ before_model │  ← 检查限制、修改消息                    │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │   LLM Call   │                                        │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ after_model  │  ← 处理响应、记录日志                    │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ before_tools │  ← 人工审批、参数验证                    │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │Tool Execution│                                        │   │
│  │  └──────┬───────┘                                        │   │
│  │         ▼                                                │   │
│  │  ┌──────────────┐                                        │   │
│  │  │ after_tools  │  ← 结果验证、失败重试                    │   │
│  │  └──────┬───────┘                                        │   │
│  │         │                                                │   │
│  └─────────┴────────────────────────────────────────────────┘   │
│           │                                                      │
│           ▼                                                      │
│  ┌──────────────────┐                                           │
│  │   after_agent    │  ← 清理资源、记录统计                       │
│  └──────────────────┘                                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Middleware 行为定义

```erlang
-behaviour(beamai_middleware).

%% 所有回调都是可选的
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().

%% 返回值类型
-type middleware_result() ::
    ok |                              %% 继续执行
    {update, map()} |                 %% 更新状态
    {goto, model | tools | '__end__'} |  %% 跳转
    {update_goto, map(), goto_target()} |
    {halt, term()} |                  %% 中止
    {interrupt, interrupt_action()}.  %% 中断等待确认
```

### 内置 Middleware

| Middleware | 职责 |
|------------|------|
| `middleware_call_limit` | 限制模型/工具调用次数 |
| `middleware_summarization` | 自动压缩长对话历史 |
| `middleware_human_approval` | 工具执行前人工确认 |
| `middleware_tool_retry` | 工具失败自动重试 |
| `middleware_model_retry` | LLM 失败自动重试 |
| `middleware_model_fallback` | 主模型失败切换备用 |
| `middleware_pii_detection` | 检测敏感信息 |

---

## 存储与持久化

### 存储架构

```
┌─────────────────────────────────────────────────────────────┐
│                      beamai_memory                           │
│  ┌─────────────────────────────────────────────────────────┐│
│  │                    Unified API                          ││
│  │  save_checkpoint/3, load_checkpoint/2, put/4, search/3  ││
│  └───────────────────────────┬─────────────────────────────┘│
│                              │                               │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │                           │                            │  │
│  │  ┌───────────────┐   ┌────┴────────┐                  │  │
│  │  │ Checkpointer  │   │   Store     │                  │  │
│  │  │  (检查点)      │   │  (键值存储) │                  │  │
│  │  └───────┬───────┘   └──────┬──────┘                  │  │
│  │          │                  │                          │  │
│  │  ┌───────┴───────┐   ┌──────┴──────┐                  │  │
│  │  │  ETS/SQLite   │   │  ETS/SQLite │                  │  │
│  │  │   Backend     │   │   Backend   │                  │  │
│  │  └───────────────┘   └─────────────┘                  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Checkpoint 结构

```erlang
#{
    id => CheckpointId,
    timestamp => Timestamp,
    state_data => #{
        messages => [Message],
        context => #{...},
        %% 其他 Agent 状态
    },
    metadata => #{
        tag => <<"v1">>,
        description => <<"...">>
    }
}
```

---

## 协议支持

### A2A (Agent-to-Agent)

Agent 间通信协议，支持服务发现和消息传递：

```
┌─────────────────┐                    ┌─────────────────┐
│   Agent A       │                    │   Agent B       │
│  ┌───────────┐  │     discover()     │  ┌───────────┐  │
│  │ A2A Client├──┼───────────────────►│  │ A2A Server│  │
│  └───────────┘  │                    │  └───────────┘  │
│                 │◄───────────────────┤                 │
│                 │    Agent Card      │                 │
│  ┌───────────┐  │                    │  ┌───────────┐  │
│  │ A2A Client├──┼──send_message()───►│  │ A2A Server│  │
│  └───────────┘  │                    │  └───────────┘  │
└─────────────────┘                    └─────────────────┘
```

### MCP (Model Context Protocol)

与 MCP 服务器交互，获取工具和资源：

```
┌─────────────────┐                    ┌─────────────────┐
│   BeamAI        │                    │   MCP Server    │
│  ┌───────────┐  │     connect()      │  ┌───────────┐  │
│  │ MCP Client├──┼───────────────────►│  │  Handler  │  │
│  └───────────┘  │                    │  └───────────┘  │
│                 │                    │                 │
│                 │◄──list_tools()────►│                 │
│                 │◄──call_tool()─────►│                 │
│                 │◄──list_resources()►│                 │
│                 │                    │                 │
└─────────────────┘                    └─────────────────┘

支持的传输方式：
- stdio: 标准输入输出
- http: HTTP 请求
- sse: Server-Sent Events
```

---

## 数据流

### Simple Agent 数据流

```
用户输入
    │
    ▼
┌─────────────────┐
│  beamai_agent   │
│  (gen_server)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│  Middleware     │────►│  graph_runner   │
│  (before_agent) │     └────────┬────────┘
└─────────────────┘              │
                                 ▼
                    ┌────────────────────────┐
                    │      Agent Loop        │
                    │  ┌──────────────────┐  │
                    │  │   LLM Node       │  │
                    │  │  (beamai_llm)    │  │
                    │  └────────┬─────────┘  │
                    │           │            │
                    │  ┌────────▼─────────┐  │
                    │  │   Tools Node     │  │
                    │  │(beamai_tools)    │  │
                    │  └──────────────────┘  │
                    └────────────────────────┘
                                 │
                                 ▼
                         最终响应返回用户
```

### Deep Agent 数据流

```
用户输入
    │
    ▼
┌──────────────────────┐
│   beamai_deepagent   │
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│   Planning Phase     │ ← 创建执行计划
│  (create_plan tool)  │
└──────────┬───────────┘
           │
           ▼
┌──────────────────────┐
│   Execution Phase    │
│  ┌────────────────┐  │
│  │   LLM Node     │  │
│  └───────┬────────┘  │
│          │           │
│  ┌───────▼────────┐  │
│  │  Tool Executor │──┼──► spawn_subtask ──► 子任务执行
│  └───────┬────────┘  │
│          │           │
│  ┌───────▼────────┐  │
│  │   Reflection   │  │ ← 反思当前进度
│  │  (reflect tool)│  │
│  └────────────────┘  │
└──────────────────────┘
           │
           ▼
      最终结果
```

---

## 扩展机制

### 1. 自定义 LLM Provider

```erlang
-module(my_llm_provider).
-behaviour(llm_provider).

-export([chat/3, stream_chat/4, info/0]).

info() ->
    #{name => <<"my_provider">>, supports => [chat, stream, tools]}.

chat(Config, Messages, Opts) ->
    %% 实现聊天逻辑
    {ok, Response}.

stream_chat(Config, Messages, Callback, Opts) ->
    %% 实现流式聊天
    {ok, FinalResponse}.
```

### 2. 自定义 Tool Provider

```erlang
-module(my_tool_provider).
-behaviour(beamai_tool_provider).

-export([list_tools/1, find_tool/2, info/0, available/0]).

info() ->
    #{name => <<"my_provider">>, version => <<"1.0.0">>}.

available() -> true.

list_tools(Opts) ->
    Context = maps:get(context, Opts, #{}),
    {ok, filter_tools_by_context(my_tools(), Context)}.

find_tool(Name, Opts) ->
    %% 查找工具
    ...
```

### 3. 自定义 Middleware

```erlang
-module(my_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{my_option => maps:get(my_option, Opts, default)}.

before_model(State, MwState) ->
    %% 在 LLM 调用前执行
    {update, #{my_key => my_value}}.

after_model(State, MwState) ->
    %% 在 LLM 调用后执行
    ok.
```

### 4. 自定义存储后端

```erlang
-module(my_store).
-behaviour(beamai_store).

-export([init/1, put/4, get/3, delete/3, search/3]).

init(Config) ->
    %% 初始化存储
    {ok, State}.

put(State, Namespace, Key, Value) ->
    %% 存储数据
    {ok, NewState}.

%% ...其他回调
```

---

## 部署架构

### 单节点部署

```
┌─────────────────────────────────────────┐
│              Erlang Node                 │
│  ┌─────────────────────────────────────┐│
│  │          Application Sup             ││
│  │  ┌───────────┐  ┌───────────────┐   ││
│  │  │Agent Sup  │  │  Memory Sup   │   ││
│  │  │ ┌───────┐ │  │ ┌───────────┐ │   ││
│  │  │ │Agent 1│ │  │ │ETS Store  │ │   ││
│  │  │ ├───────┤ │  │ ├───────────┤ │   ││
│  │  │ │Agent 2│ │  │ │Checkpointer│ │   ││
│  │  │ └───────┘ │  │ └───────────┘ │   ││
│  │  └───────────┘  └───────────────┘   ││
│  └─────────────────────────────────────┘│
└─────────────────────────────────────────┘
```

### 分布式部署

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│   Node A        │  │   Node B        │  │   Node C        │
│  ┌───────────┐  │  │  ┌───────────┐  │  │  ┌───────────┐  │
│  │ Agent 1   │  │  │  │ Agent 3   │  │  │  │ Agent 5   │  │
│  │ Agent 2   │  │  │  │ Agent 4   │  │  │  │ Agent 6   │  │
│  └───────────┘  │  │  └───────────┘  │  │  └───────────┘  │
│                 │  │                 │  │                 │
│  Pregel Worker  │  │  Pregel Worker  │  │  Pregel Master  │
└────────┬────────┘  └────────┬────────┘  └────────┬────────┘
         │                    │                    │
         └────────────────────┼────────────────────┘
                              │
                      ┌───────┴───────┐
                      │   Erlang      │
                      │  Distribution │
                      └───────────────┘
```

---

## 更多资源

- [README.md](../README.md) - 项目概述和快速开始
- [API_REFERENCE.md](API_REFERENCE.md) - API 参考文档
- [MIDDLEWARE.md](MIDDLEWARE.md) - Middleware 系统文档

### 本项目模块文档
- [beamai_tools](../apps/beamai_tools/README.md) - 工具和中间件系统
- [beamai_agent](../apps/beamai_agent/README.md) - Simple Agent 实现
- [beamai_deepagent](../apps/beamai_deepagent/README.md) - Deep Agent 实现
- [beamai_a2a](../apps/beamai_a2a/README.md) - A2A 协议
- [beamai_mcp](../apps/beamai_mcp/README.md) - MCP 协议
- [beamai_rag](../apps/beamai_rag/README.md) - RAG 功能

### 外部依赖文档
- [BeamAI 核心库](https://github.com/TTalkPro/beamai) - 核心框架文档
  - beamai_core - Kernel、Process、Graph、HTTP
  - beamai_llm - LLM 客户端、Provider、Parser
  - beamai_memory - 存储、Checkpoint、Snapshot
