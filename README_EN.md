# BeamAI Extra - Erlang Agent Framework Extensions

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-27%2B-red.svg)](https://www.erlang.org/)
[![Build](https://img.shields.io/badge/build-rebar3-brightgreen.svg)](https://rebar3.org/)

English | [中文](README.md)

A high-performance AI Agent extension framework based on [BeamAI](https://github.com/TTalkPro/beamai), providing a complete Agent development toolkit.

This project depends on the `v2.0.0` tag of the [BeamAI core library](https://github.com/TTalkPro/beamai) and includes the following extension features:

## Features

- **ChatClient/Tool Architecture**: Semantic function registration and invocation system
  - ChatClient core based on Semantic Kernel concepts
  - Tool management and Middleware pipeline
  - Security validation and permission control

- **Simple Agent (upstream BeamAI)**: ReAct Agent based on tool loop
  - Pure functional API (`new/1` -> `run/2` -> new state)
  - Custom tools, Plugin loading, system prompts
  - 8 callback hooks (turn/llm/tool/token/interrupt/resume)
  - Streaming output (`stream/2`)
  - Interrupt and resume support (Human-in-the-Loop)
  - Pluggable cross-turn Memory (window / ETS / DETS / custom provider)
  - SubAgent delegation and parallel execution

- **Protocol Support**: A2A, MCP, and AG-UI
  - Agent-to-Agent communication protocol
  - Model Context Protocol integration
  - Agent event-stream runtime

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
%% Create LLM configuration (use beamai_chat_model:create/2)
LLM = beamai_chat_model:create(anthropic, #{
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

### 4. Simple Agent (Using ChatClient + Tools)

```erlang
%% Method 1: Define tools via ChatClient (pre-built ChatClient)
ChatClient0 = beamai_chat_client:new(),
LlmConfig = beamai_chat_model:create(anthropic, #{
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    model => <<"glm-4.7">>
}),
K1 = beamai_chat_client:add_chat_model(ChatClient0, LlmConfig),
K2 = beamai_chat_client:add_tool(K1, #{
    name => <<"get_weather">>,
    description => <<"Get weather for a city">>,
    parameters => #{
        <<"city">> => #{type => string, required => true, description => <<"City name">>}
    },
    handler => fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25, condition => <<"Sunny">>}}
    end
}),

%% Create Agent with pre-built ChatClient
{ok, Agent0} = beamai_agent:new(#{
    chat_client => K2,
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

### 5. Simple Agent (With Cross-turn Memory)

```erlang
%% Create an ETS memory backend with a controlled lifecycle
{ok, _} = beamai_chat_memory_ets:start_link(my_store),
Memory = {store, beamai_chat_memory_ets:handle(my_store)},

%% Agents with the same conversation_id load the same cross-turn history
{ok, Agent0} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a persistent assistant.">>,
    memory => Memory,
    conversation_id => <<"my-session">>
}),

%% User, assistant, and tool messages are persisted after each turn
{ok, _, Agent1} = beamai_agent:run(Agent0, <<"Remember: the password is 12345">>),

%% Create another Agent pointed at the same conversation
{ok, Agent2} = beamai_agent:new(#{
    llm => LLM,
    system_prompt => <<"You are a persistent assistant.">>,
    memory => Memory,
    conversation_id => <<"my-session">>
}),
{ok, Result, _Agent3} = beamai_agent:run(Agent2, <<"What's the password?">>).
%% Agent will remember the password is 12345
```

## Architecture

### Application Structure

**Core Dependencies (from [BeamAI](https://github.com/TTalkPro/beamai)):**

```
beamai (external dependency)
├── beamai_core/        # Core framework
│   ├── ChatClient         # beamai_chat_client, beamai_tool, beamai_context,
│   │                  # beamai_filter, beamai_prompt, beamai_result
│   ├── LLM            # beamai_chat_response (unified LLM response accessors)
│   ├── HTTP           # beamai_http, beamai_http_gun, beamai_http_pool
│   ├── Memory         # beamai_chat_memory, beamai_memory_provider,
│   │                  # beamai_chat_memory_ets, beamai_memory_provider_default
│   ├── Behaviours     # beamai_chat_behaviour, beamai_http_behaviour,
│   │                  # beamai_chat_memory, beamai_memory_provider
│   └── Utils          # beamai_id, beamai_jsonrpc, beamai_sse, beamai_utils
│
├── beamai_llm/         # LLM client
│   ├── Chat           # beamai_chat_model
│   ├── Parser         # beamai_output_parser, beamai_parser_json
│   ├── Adapters       # llm_message_adapter, llm_response_adapter, llm_tool_adapter
│   └── Providers      # OpenAI, OpenAI Responses, Anthropic, DeepSeek, Zhipu,
│                      # DashScope, Ollama, xAI, Moonshot/Kimi, OpenRouter,
│                      # SiliconFlow
│
├── beamai_agent/       # SimpleAgent (ReAct loop, callbacks, HITL, SubAgents)
│   ├── Core           # beamai_agent, beamai_agent_state, beamai_agent_callbacks,
│   │                  # beamai_agent_event, beamai_agent_interrupt
│   ├── Memory         # branch / timeline and pause snapshot stores
│   ├── Execution      # beamai_agent_tool_loop, beamai_tool_calling_manager,
│   │                  # beamai_tool_batch_worker
│   └── SubAgents      # beamai_agent_delegate, beamai_subagent_manager
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
├── beamai_a2a/         # A2A protocol implementation
│   ├── Server         # A2A server
│   └── Client         # A2A client
│
├── beamai_mcp/         # MCP protocol implementation
│   ├── Server         # MCP server
│   └── Client         # MCP client
│
├── beamai_agui/        # AG-UI protocol implementation
│   ├── Runtime        # sessions, registry, run orchestration
│   └── HTTP           # framework-neutral HTTP layer and Cowboy adapter
│
└── beamai_rag/         # RAG functionality
    ├── Embeddings     # Vector embeddings
    └── Vector Store   # Vector storage
```

### Dependency Relationships

```
┌────────────────────────────────────────────────────┐
│   Agent Implementation Layer (Upstream BeamAI)     │
│  (beamai_agent)                                    │
└────────────────┬───────────────────────────────────┘
                 │
┌────────────────┴───────────────────────────────────┐
│   Services Layer (Project Extensions)              │
│  (beamai_tools, beamai_rag, beamai_a2a,            │
│   beamai_mcp, beamai_agui)                          │
└────────────────┬───────────────────────────────────┘
                 │
┌────────────────┴───────────────────────────────────┐
│   Core Layer (from BeamAI external dependency)     │
│  (beamai_core, beamai_llm, beamai_agent)           │
│       https://github.com/TTalkPro/beamai           │
└────────────────────────────────────────────────────┘
```

**Dependency Notes:**
- This project depends on the `v2.0.0` tag of [BeamAI](https://github.com/TTalkPro/beamai) via `rebar.config`
- Core functionality (ChatClient, LLM, SimpleAgent) is provided by BeamAI
- This project focuses on protocol support (A2A, MCP, AG-UI), the tool system, and RAG features

See [DEPENDENCIES_EN.md](docs/DEPENDENCIES_EN.md) for details

## Core Concepts

### 1. ChatClient Architecture

ChatClient is BeamAI's core abstraction, managing Tool registration and invocation:

```erlang
%% Create ChatClient instance
ChatClient = beamai_chat_client:new(),

%% Load tool from module
ChatClient1 = beamai_chat_client:add_tool_module(ChatClient, beamai_tool_file),

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
ChatClient2 = beamai_chat_client:add_tool(ChatClient1, Tool),

%% Invoke registered tool
{ok, Result, _Ctx} = beamai_tool_executor:invoke(ChatClient2, <<"read_file">>, #{
    <<"path">> => <<"/tmp/test.txt">>
}, beamai_context:new()).
```

### 2. Memory Persistence

Use storage backends for cross-turn session persistence:

```erlang
%% Create Memory (using ETS storage backend)
{ok, _} = beamai_chat_memory_ets:start_link(my_store),
Memory = {store, beamai_chat_memory_ets:handle(my_store)},

%% Agents with the same conversation_id share history automatically
{ok, Agent0} = beamai_agent:new(#{
    llm => LLM,
    memory => Memory,
    conversation_id => <<"my-session">>
}),
{ok, _, Agent1} = beamai_agent:run(Agent0, <<"Hello">>),

%% Point a new instance at the same conversation_id to continue
{ok, RestoredAgent} = beamai_agent:new(#{
    llm => LLM,
    memory => Memory,
    conversation_id => <<"my-session">>
}).
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

LLM configuration is created using `beamai_chat_model:create/2`:

```erlang
%% Method 1: Create LLM configuration directly
LLM = beamai_chat_model:create(anthropic, #{
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
| `openai_responses` | llm_provider_openai_responses | OpenAI Responses | OpenAI Responses API |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | DeepSeek API (default `deepseek-flash`) |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Zhipu AI (GLM series) |
| `dashscope` | llm_provider_dashscope | DashScope native | Alibaba Cloud DashScope (Qwen series) |
| `ollama` | llm_provider_ollama | OpenAI compatible | Ollama local models |
| `xai` | llm_provider_xai | OpenAI compatible | xAI (Grok series) |
| `moonshot` / `kimi` | llm_provider_moonshot | OpenAI compatible | Moonshot (Kimi series) |
| `openrouter` | llm_provider_openrouter | OpenAI compatible | OpenRouter gateway |
| `siliconflow` | llm_provider_siliconflow | OpenAI compatible | SiliconFlow hosted models |
| `mock` | llm_provider_mock | Built-in | Mock LLM for testing |

### HTTP Backend Configuration

BeamAI uses Gun as its single HTTP backend (HTTP/2 supported).

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

| Feature | Gun |
|--------|-----|
| HTTP/2 | Supported |
| Connection Pool | Built-in beamai_http_pool |
| TLS | Automatically uses system CA certificates |
| Use Case | Recommended for production |

## Documentation

### Core Documentation

- **[docs/API_REFERENCE_EN.md](docs/API_REFERENCE_EN.md)** - API Reference
- **[docs/MIDDLEWARE_EN.md](docs/MIDDLEWARE_EN.md)** - Middleware System Documentation
- **[docs/CALLBACKS_EN.md](docs/CALLBACKS_EN.md)** - Callback System Documentation
- **[docs/ARCHITECTURE_EN.md](docs/ARCHITECTURE_EN.md)** - Architecture Design
- **[docs/DEPENDENCIES_EN.md](docs/DEPENDENCIES_EN.md)** - Dependency Relationship Details

### Module Documentation

| Module | Description | Source | Documentation |
|--------|-------------|--------|---------------|
| **beamai_core** | Core framework: ChatClient, Filter, Memory, HTTP, tools | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_llm** | LLM client: supports OpenAI, OpenAI Responses, Anthropic, DeepSeek, Zhipu, DashScope, Ollama, xAI, Moonshot/Kimi, OpenRouter, SiliconFlow | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_agent** | Agent implementation: ReAct pattern, callback system, interrupt/resume, SubAgents | BeamAI | [External Docs](https://github.com/TTalkPro/beamai) |
| **beamai_tools** | Tool system: tool management, Middleware, security validation | Project | [README](apps/beamai_tools/README.md) |
| **beamai_a2a** | A2A protocol: inter-Agent communication, server/client | Project | [README](apps/beamai_a2a/README_EN.md) |
| **beamai_mcp** | MCP protocol: Model Context Protocol implementation | Project | [README](apps/beamai_mcp/README_EN.md) |
| **beamai_rag** | RAG functionality: splitting, vector embeddings, similarity search, reranking, grounded generation | Project | [README](apps/beamai_rag/README_EN.md) |

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
| **OTP Applications (Project)** | 5 |
| **Core Dependencies (BeamAI)** | 3 |
| **Source Modules (Project)** | ~80 |

**Project Applications:**
- beamai_tools
- beamai_a2a
- beamai_mcp
- beamai_agui
- beamai_rag

**Core Dependencies (from BeamAI):**
- beamai_core
- beamai_llm
- beamai_agent

### Running Tests

```bash
# Run all tests
rebar3 eunit

# Run tests for specific app
rebar3 eunit --app=beamai_tools

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
