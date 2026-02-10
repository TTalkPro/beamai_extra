# BeamAI Extra - Erlang Agent Framework Extensions

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-26%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

English | [中文](README.md)

A high-performance AI Agent extension framework based on [BeamAI](https://github.com/TTalkPro/beamai), providing a complete Agent development toolkit.

This project depends on the main branch of the [BeamAI core library](https://github.com/TTalkPro/beamai) and includes the following extension features:

## Features

- **Kernel/Tool Architecture**: Semantic function registration and invocation system
  - Kernel core based on Semantic Kernel concepts
  - Tool management and Middleware pipeline
  - Security validation and permission control

- **Simple Agent**: ReAct Agent based on tool loop
  - Pure functional API (`new/1` -> `run/2` -> new state)
  - Custom tools, Plugin loading, system prompts
  - 8 callback hooks (turn/llm/tool/token/interrupt/resume)
  - Streaming output (`stream/2`)
  - Interrupt and resume support (Human-in-the-Loop)
  - Built-in Memory persistence (`save/1`, `restore/2`)

- **Deep Agent**: Recursive planning Agent based on SubAgent architecture
  - Planner -> Executor -> Reflector pipeline
  - Parallel subtask execution
  - Coordinator multi-Agent orchestration

- **Protocol Support**: A2A and MCP
  - Agent-to-Agent communication protocol
  - Model Context Protocol integration

- **RAG**: Retrieval-Augmented Generation
  - Vector embeddings and similarity search
  - Text splitting

## Quick Start

### 1. Start Shell

```bash
export ZHIPU_API_KEY=your_key_here
# Optional: custom Anthropic-compatible API base URL (default: https://open.bigmodel.cn/api/anthropic)
export ZHIPU_ANTHROPIC_BASE_URL=https://open.bigmodel.cn/api/anthropic
rebar3 shell
```

### 2. Simple Agent (Basic Usage)

```erlang
%% Create LLM configuration (use beamai_chat_completion:create/2)
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

%% Create Agent (pure functional API, returns immutable state)
{ok, Agent0} = beamai_agent:new(#{
    system_prompt => <<"You are a helpful assistant.">>,
    llm => LLM
}),

%% Run Agent (returns result and new state)
{ok, Result, _Agent1} = beamai_agent:run(Agent0, <<"Hello!">>),

%% View result
Content = maps:get(content, Result).
```

### 3. Simple Agent (Multi-turn Conversation)

```erlang
%% Multi-turn conversation through state passing, message history auto-accumulates
{ok, Agent0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a memory assistant.">>
}),
{ok, _, Agent1} = beamai_agent:run(Agent0, <<"My name is John">>),
{ok, Result, _Agent2} = beamai_agent:run(Agent1, <<"What's my name?">>).
%% Agent will remember user's name is John

%% Query conversation state
beamai_agent:turn_count(Agent1).    %% => 1
beamai_agent:messages(Agent1).      %% => message history list
beamai_agent:last_response(Agent1). %% => last assistant reply
```

### 4. Simple Agent (Using Kernel + Tools)

```erlang
%% Method 1: Define tools via Kernel (pre-built Kernel)
Kernel0 = beamai_kernel:new(),
LlmConfig = beamai_chat_completion:create(anthropic, #{
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    model => <<"glm-4.7">>
}),
K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
K2 = beamai_kernel:add_tool(K1, #{
    name => <<"get_weather">>,
    description => <<"Get weather for a city">>,
    parameters => #{
        <<"city">> => #{type => string, required => true, description => <<"City name">>}
    },
    handler => fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25, condition => <<"Sunny">>}}
    end
}),

%% Create Agent with pre-built Kernel
{ok, Agent0} = beamai_agent:new(#{
    kernel => K2,
    system_prompt => <<"You are a weather assistant.">>
}),
{ok, Result, _} = beamai_agent:run(Agent0, <<"What's the weather in Beijing?">>).

%% Method 2: Load built-in tool modules via plugins
{ok, Agent0} = beamai_agent:new(#{
    llm => LlmConfig,
    plugins => [beamai_tool_file],
    system_prompt => <<"You are a file assistant.">>
}),
{ok, Result, _} = beamai_agent:run(Agent0, <<"List .erl files in src/">>).
```

### 5. Simple Agent (With Memory Persistence)

```erlang
%% Create storage backend
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
StateStore = beamai_state_store:new({beamai_store_ets, my_store}),
Mgr = beamai_process_snapshot:new(StateStore),
Memory = {Mgr, #{thread_id => <<"my-session">>}},

%% Create Agent with Memory (auto_save enables per-turn auto-save)
{ok, Agent0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a persistent assistant.">>,
    memory => Memory,
    auto_save => true
}),

%% Conversation (auto-saved)
{ok, _, Agent1} = beamai_agent:run(Agent0, <<"Remember: the password is 12345">>),
{ok, _, _Agent2} = beamai_agent:run(Agent1, <<"OK">>),

%% Manual save
ok = beamai_agent:save(Agent1),

%% Later restore session
{ok, RestoredAgent} = beamai_agent:restore(#{llm => LLM}, Memory),
{ok, Result, _} = beamai_agent:run(RestoredAgent, <<"What's the password?">>).
%% Agent will remember the password is 12345
```

### 6. Deep Agent (SubAgent Orchestration)

```erlang
%% Create Deep Agent configuration (new/1 returns config map directly)
Config = beamai_deepagent:new(#{
    llm => LLM,
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    system_prompt => <<"You are a research expert.">>,
    %% Use Tool modules for tools
    plugins => [beamai_tool_file, beamai_tool_shell]
}),

%% Run complex task (Planner -> Executor -> Reflector)
{ok, Result} = beamai_deepagent:run(Config,
    <<"Analyze this codebase's architecture and provide optimization suggestions.">>),

%% View execution plan and trace
Plan = beamai_deepagent:get_plan(Result),
Trace = beamai_deepagent:get_trace(Result).
```

## Architecture

### Application Structure

**Core Dependencies (from [BeamAI](https://github.com/TTalkPro/beamai)):**

```
beamai (external dependency)
├── beamai_core/        # Core framework
│   ├── Kernel         # beamai_kernel, beamai_tool, beamai_context,
│   │                  # beamai_filter, beamai_prompt, beamai_result
│   ├── LLM            # beamai_llm_response (unified LLM response accessors)
│   ├── Process        # beamai_process, beamai_process_builder,
│   │                  # beamai_process_runtime, beamai_process_step,
│   │                  # beamai_process_executor, beamai_process_event
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_hackney,
│   │                  # beamai_http_pool
│   ├── Behaviours     # beamai_llm_behaviour, beamai_http_behaviour,
│   │                  # beamai_step_behaviour, beamai_process_store_behaviour
│   ├── Graph          # graph, graph_node, graph_edge, graph_builder, graph_dsl,
│   │                  # graph_runner, graph_snapshot, graph_state, graph_command
│   ├── Pregel         # pregel, pregel_master, pregel_worker, pregel_vertex
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_llm/         # LLM client
│   ├── Chat           # beamai_chat_completion
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # llm_message_adapter, llm_response_adapter, llm_tool_adapter
│   └── Providers      # OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama
│
└── beamai_memory/      # Memory and context storage
    ├── Context        # Context management
    ├── Store          # ETS/SQLite storage backends
    └── Snapshot       # Snapshots, branching, time travel
```

**Project Extensions:**

```
apps/
├── beamai_tools/       # Tool system
│   ├── Core           # beamai_tools, beamai_tool_behaviour
│   ├── Middleware     # beamai_middleware, beamai_middleware_runner,
│   │                  # middleware_call_limit, middleware_tool_retry
│   ├── Security       # beamai_tool_security
│   └── Tools          # beamai_tool_file, beamai_tool_shell,
│                      # beamai_tool_human, beamai_tool_todo
│
├── beamai_agent/       # Agent implementation
│   ├── Core           # beamai_agent, beamai_agent_state, beamai_agent_callbacks
│   ├── Memory         # beamai_agent_memory
│   ├── Execution      # beamai_agent_tool_loop, beamai_agent_interrupt
│   └── Process Agent  # beamai_process_agent, beamai_process_agent_llm_step,
│                      # beamai_process_agent_tool_step
│
├── beamai_deepagent/   # Deep Agent (SubAgent architecture)
│   ├── Core           # beamai_deepagent, beamai_deepagent_plan,
│   │                  # beamai_deepagent_dependencies, beamai_deepagent_trace
│   └── SubAgents      # beamai_deepagent_planner, beamai_deepagent_executor,
│                      # beamai_deepagent_reflector, beamai_deepagent_parallel,
│                      # beamai_deepagent_coordinator
│
├── beamai_a2a/         # A2A protocol implementation
│   ├── Server         # A2A server
│   └── Client         # A2A client
│
├── beamai_mcp/         # MCP protocol implementation
│   ├── Server         # MCP server
│   └── Client         # MCP client
│
└── beamai_rag/         # RAG functionality
    ├── Embeddings     # Vector embeddings
    └── Vector Store   # Vector storage
```

### Dependency Relationships

```
┌────────────────────────────────────────────────────┐
│   Agent Implementation Layer (Project Extensions)  │
│  (beamai_agent, beamai_deepagent)                  │
└────────────────┬───────────────────────────────────┘
                 │
┌────────────────┴───────────────────────────────────┐
│   Services Layer (Project Extensions)              │
│  (beamai_tools, beamai_rag, beamai_a2a, beamai_mcp) │
└────────────────┬───────────────────────────────────┘
                 │
┌────────────────┴───────────────────────────────────┐
│   Core Layer (from BeamAI external dependency)     │
│  (beamai_core, beamai_llm, beamai_memory)          │
│       https://github.com/TTalkPro/beamai           │
└────────────────────────────────────────────────────┘
```

**Dependency Notes:**
- This project depends on the main branch of [BeamAI](https://github.com/TTalkPro/beamai) via `rebar.config`
- Core functionality (Kernel, Process Framework, Graph, LLM, Memory) is provided by BeamAI
- This project focuses on Agent implementation, protocol support (A2A, MCP), tool system, and RAG features

See [DEPENDENCIES_EN.md](doc/DEPENDENCIES_EN.md) for details

## Core Concepts

### 1. Kernel Architecture

Kernel is BeamAI's core abstraction, managing Tool registration and invocation:

```erlang
%% Create Kernel instance
Kernel = beamai_kernel:new(),

%% Load tool from module
Kernel1 = beamai_kernel:add_tool_module(Kernel, beamai_tool_file),

%% Or add a single tool
Tool = #{
    name => <<"read_file">>,
    description => <<"Read file contents">>,
    parameters => #{
        <<"path">> => #{type => string, required => true}
    },
    handler => fun(#{<<"path">> := Path}, _Ctx) ->
        file:read_file(Path)
    end
},
Kernel2 = beamai_kernel:add_tool(Kernel1, Tool),

%% Invoke registered tool
{ok, Result, _Ctx} = beamai_kernel:invoke_tool(Kernel2, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### 2. Memory Persistence

Use storage backends for session persistence:

```erlang
%% Create Memory (using ETS storage backend)
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
StateStore = beamai_state_store:new({beamai_store_ets, my_store}),
Mgr = beamai_process_snapshot:new(StateStore),
Memory = {Mgr, #{thread_id => <<"my-session">>}},

%% Create Agent with memory (auto_save enables per-turn auto-save)
{ok, Agent0} = beamai_agent:new(#{llm => LLM, memory => Memory, auto_save => true}),
{ok, _, Agent1} = beamai_agent:run(Agent0, <<"Hello">>),

%% Manual save
ok = beamai_agent:save(Agent1),

%% Restore session from Memory
{ok, RestoredAgent} = beamai_agent:restore(#{llm => LLM}, Memory).
```

### 3. Callbacks

Listen to 8 events during Agent execution:

```erlang
{ok, Agent0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are an assistant">>,
    callbacks => #{
        on_turn_start => fun(Meta) ->
            io:format("Turn started: ~p~n", [maps:get(turn_count, Meta)])
        end,
        on_turn_end => fun(Meta) ->
            io:format("Turn ended~n")
        end,
        on_turn_error => fun(Reason, Meta) ->
            io:format("Turn error: ~p~n", [Reason])
        end,
        on_llm_call => fun(Messages, Meta) ->
            io:format("LLM call: ~B messages~n", [length(Messages)])
        end,
        on_tool_call => fun(Name, Args) ->
            io:format("Tool call: ~s~n", [Name])
            %% Return {interrupt, Reason} to trigger interrupt
        end,
        on_token => fun(Token, Meta) ->
            io:format("~s", [Token])  %% Streaming token
        end,
        on_interrupt => fun(InterruptState, Meta) ->
            io:format("Agent interrupted~n")
        end,
        on_resume => fun(InterruptState, Meta) ->
            io:format("Agent resumed~n")
        end
    }
}).
```

## Configuration

### LLM Configuration

LLM configuration is created using `beamai_chat_completion:create/2`:

```erlang
%% Method 1: Create LLM configuration directly
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    max_tokens => 2048
}),

%% Method 2: Use example_llm_config helper (reads from environment variables)
LLM = example_llm_config:anthropic().     %% Zhipu Anthropic-compatible API (ZHIPU_API_KEY)
LLM = example_llm_config:claude().        %% Anthropic native API (ANTHROPIC_API_KEY)
LLM = example_llm_config:zhipu().         %% Zhipu native API (ZHIPU_API_KEY)
LLM = example_llm_config:openai_glm().    %% Zhipu OpenAI-compatible API (ZHIPU_API_KEY)
LLM = example_llm_config:deepseek().      %% DeepSeek API (DEEPSEEK_API_KEY)
LLM = example_llm_config:openai().        %% OpenAI API (OPENAI_API_KEY)

%% Method 3: Use test_zhipu_anthropic module (supports ZHIPU_ANTHROPIC_BASE_URL env var)
LLM = test_zhipu_anthropic:create_llm().  %% Built from ZHIPU_API_KEY + ZHIPU_ANTHROPIC_BASE_URL

%% Configuration can be reused across multiple Agents
{ok, Agent1} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Research assistant">>}),
{ok, Agent2} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Writing assistant">>}).
```

**Environment Variables:**

| Variable | Description | Required |
|----------|-------------|----------|
| `ZHIPU_API_KEY` | Zhipu API Key | Required for Zhipu-related providers |
| `ZHIPU_ANTHROPIC_BASE_URL` | Zhipu Anthropic-compatible API URL (default: `https://open.bigmodel.cn/api/anthropic`) | Optional |
| `ANTHROPIC_API_KEY` | Anthropic native API Key | Required for Claude |
| `DEEPSEEK_API_KEY` | DeepSeek API Key | Required for DeepSeek |
| `OPENAI_API_KEY` | OpenAI API Key | Required for OpenAI |

**Supported Providers:**

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| `anthropic` | llm_provider_anthropic | Anthropic | Anthropic Claude API |
| `openai` | llm_provider_openai | OpenAI | OpenAI API |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | DeepSeek API |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Zhipu AI (GLM series) |
| `bailian` | llm_provider_bailian | DashScope native | Alibaba Cloud Bailian (Qwen series) |
| `ollama` | llm_provider_ollama | OpenAI compatible | Ollama local models |
| `mock` | llm_provider_mock | Built-in | Mock LLM for testing |

### HTTP Backend Configuration

BeamAI supports both Gun and Hackney HTTP backends, with Gun as the default (supports HTTP/2).

```erlang
%% Configure in sys.config (optional)
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pool, #{
        max_connections => 100,
        connection_timeout => 30000
    }}
]}.
```

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection Pool | Built-in beamai_http_pool | Relies on hackney pool |
| TLS | Automatically uses system CA certificates | hackney default config |
| Use Case | Recommended for production | Legacy system compatibility |

## Documentation

### Core Documentation

- **[doc/API_REFERENCE_EN.md](doc/API_REFERENCE_EN.md)** - API Reference
- **[doc/MIDDLEWARE_EN.md](doc/MIDDLEWARE_EN.md)** - Middleware System Documentation
- **[doc/CALLBACKS_EN.md](doc/CALLBACKS_EN.md)** - Callback System Documentation
- **[doc/ARCHITECTURE_EN.md](doc/ARCHITECTURE_EN.md)** - Architecture Design
- **[DEPENDENCIES_EN.md](doc/DEPENDENCIES_EN.md)** - Dependency Relationship Details

### Module Documentation

| Module | Description | Source | Documentation |
|--------|-------------|--------|---------------|
| **beamai_core** | Core framework: Kernel, Process Framework, Graph Engine, HTTP, Behaviours | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_llm** | LLM client: supports OpenAI, Anthropic, DeepSeek, Zhipu, Bailian, Ollama | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_memory** | Memory management: Checkpoint, Store, time travel, branching | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_tools** | Tool system: tool management, Middleware, security validation | Project | [README](apps/beamai_tools/README.md) |
| **beamai_agent** | Agent implementation: ReAct pattern, callback system, Process Agent | Project | [README](apps/beamai_agent/README.md) |
| **beamai_deepagent** | Deep Agent: SubAgent orchestration, task planning, parallel execution, self-reflection | Project | [README](apps/beamai_deepagent/README_EN.md) |
| **beamai_a2a** | A2A protocol: inter-Agent communication, server/client | Project | [README](apps/beamai_a2a/README_EN.md) |
| **beamai_mcp** | MCP protocol: Model Context Protocol implementation | Project | [README](apps/beamai_mcp/README_EN.md) |
| **beamai_rag** | RAG functionality: vector embeddings, similarity search | Project | [README](apps/beamai_rag/README_EN.md) |

## Running Examples

```bash
# Compile
rebar3 compile

# Start Shell
rebar3 shell
```

### Example Files

| File | Description | Requires LLM |
|------|-------------|--------------|
| `example_llm_config.erl` | LLM configuration helper module | - |
| `example_agent.erl` | Agent basics (single-turn, multi-turn, tool calls, streaming) | Partial |
| `example_agent_hitl.erl` | Agent Human-in-the-Loop interrupt/resume | No (Mock) |
| `example_agent_scenarios.erl` | Agent advanced scenarios (callbacks, concurrency, Coordinator) | Partial |
| `example_deepagent.erl` | DeepAgent planning, reflection, trace inspection | Partial |
| `example_deepagent_tools.erl` | DeepAgent tool integration (file, shell, custom) | Yes |
| `test_anthropic_agent.erl` | Agent and DeepAgent end-to-end tests | Yes |
| `test_zhipu_anthropic.erl` | Zhipu Anthropic-compatible API integration tests | Yes |

### Integration Tests (Agent + DeepAgent)

Use the `test_zhipu_anthropic` module for end-to-end testing of Agent and DeepAgent:

```bash
export ZHIPU_API_KEY=your_key_here
# Optional: custom Anthropic-compatible API base URL
export ZHIPU_ANTHROPIC_BASE_URL=https://open.bigmodel.cn/api/anthropic
rebar3 shell
```

```erlang
%% Run all integration tests (5 test cases)
test_zhipu_anthropic:run_all().

%% Or run individually
test_zhipu_anthropic:test_agent_simple().       %% Agent single-turn conversation
test_zhipu_anthropic:test_agent_multi_turn().   %% Agent multi-turn (context memory)
test_zhipu_anthropic:test_agent_with_tools().   %% Agent tool calling
test_zhipu_anthropic:test_deepagent_simple().   %% DeepAgent simple task
test_zhipu_anthropic:test_deepagent_planned().  %% DeepAgent planned execution
```

## Project Statistics

| Metric | Count |
|--------|-------|
| **OTP Applications (Project)** | 6 |
| **Core Dependencies (BeamAI)** | 3 |
| **Source Modules (Project)** | ~80 |

**Project Applications:**
- beamai_tools
- beamai_agent
- beamai_deepagent
- beamai_a2a
- beamai_mcp
- beamai_rag

**Core Dependencies (from BeamAI):**
- beamai_core
- beamai_llm
- beamai_memory

### Running Tests

```bash
# Run all tests
rebar3 eunit

# Run tests for specific app
rebar3 eunit --app=beamai_agent

# Run type checking
rebar3 dialyzer
```

## Performance

- Based on Erlang/OTP lightweight processes
- Concurrent tool invocations
- HTTP connection pool (Gun, supports HTTP/2)
- ETS high-speed storage

## Design Principles

- **Simple**: Clear API, easy to understand
- **Modular**: Single responsibility for each module
- **Extensible**: Behaviour design, easy to customize
- **High Performance**: Leverages Erlang concurrency features
- **Observable**: Comprehensive logging, tracing, monitoring

## License

Apache-2.0

## Contributing

Issues and Pull Requests are welcome!
