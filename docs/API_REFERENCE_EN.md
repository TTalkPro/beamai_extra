# API Reference

English | [中文](API_REFERENCE.md)

This document provides the main API reference for each module of the BeamAI Framework.

## Table of Contents

- [beamai_agent - Simple Agent](#beamai_agent---simple-agent)
- [beamai_coordinator - Multi-Agent Coordinator](#beamai_coordinator---multi-agent-coordinator)
- [Middleware System](#middleware-system)
- [beamai_deepagent - Deep Agent](#beamai_deepagent---deep-agent)
- [beamai_llm - LLM Client](#beamai_llm---llm-client)
- [beamai_memory - Memory Management](#beamai_memory---memory-management)
- [beamai_tools - Tool Library](#beamai_tools---tool-library)
- [beamai_core - Core Module](#beamai_core---core-module)
  - [HTTP Client](#http-client)
  - [HTTP Backend Configuration](#http-backend-configuration)
- [beamai_a2a - A2A Protocol](#beamai_a2a---a2a-protocol)
- [beamai_mcp - MCP Protocol](#beamai_mcp---mcp-protocol)
- [beamai_rag - RAG Functionality](#beamai_rag---rag-functionality)

---

## beamai_agent - Simple Agent

Pure functional ReAct Agent implementation using immutable state passing for multi-turn conversations.

### Creating Agent

```erlang
%% Create Agent (returns immutable state)
-spec new(map()) -> {ok, agent_state()} | {error, term()}.
beamai_agent:new(Config).
```

### Execution API

```erlang
%% Run one conversation turn (returns result and new state)
-spec run(agent_state(), binary()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:run(Agent, UserMessage).

%% Run with options
-spec run(agent_state(), binary(), map()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:run(Agent, UserMessage, Opts).

%% Streaming output
-spec stream(agent_state(), binary()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:stream(Agent, UserMessage).

-spec stream(agent_state(), binary(), map()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:stream(Agent, UserMessage, Opts).
```

### Interrupt and Resume (Human-in-the-Loop)

```erlang
%% Resume interrupted Agent
-spec resume(agent_state(), term()) ->
    {ok, run_result(), agent_state()} |
    {interrupt, interrupt_info(), agent_state()} |
    {error, term()}.
beamai_agent:resume(Agent, HumanInput).

%% Load from Memory and resume
-spec resume_from_memory(map(), term(), term()) ->
    {ok, run_result(), agent_state()} |
    {interrupt, interrupt_info(), agent_state()} |
    {error, term()}.
beamai_agent:resume_from_memory(Config, Memory, HumanInput).

%% Check interrupt status
-spec is_interrupted(agent_state()) -> boolean().
beamai_agent:is_interrupted(Agent).

-spec get_interrupt_info(agent_state()) -> interrupt_info() | undefined.
beamai_agent:get_interrupt_info(Agent).
```

### State Queries

```erlang
%% Get Agent ID
-spec id(agent_state()) -> binary().
beamai_agent:id(Agent).

%% Get Agent name
-spec name(agent_state()) -> binary().
beamai_agent:name(Agent).

%% Get conversation message history
-spec messages(agent_state()) -> [map()].
beamai_agent:messages(Agent).

%% Get last assistant response
-spec last_response(agent_state()) -> binary() | undefined.
beamai_agent:last_response(Agent).

%% Get completed conversation turn count
-spec turn_count(agent_state()) -> non_neg_integer().
beamai_agent:turn_count(Agent).

%% Get internal Kernel instance
-spec kernel(agent_state()) -> beamai_kernel:kernel().
beamai_agent:kernel(Agent).
```

### State Modification

```erlang
%% Set system prompt
-spec set_system_prompt(agent_state(), binary()) -> agent_state().
beamai_agent:set_system_prompt(Agent, Prompt).

%% Manually add message to history
-spec add_message(agent_state(), map()) -> agent_state().
beamai_agent:add_message(Agent, Message).

%% Clear message history
-spec clear_messages(agent_state()) -> agent_state().
beamai_agent:clear_messages(Agent).

%% Update user metadata (merge mode)
-spec update_metadata(agent_state(), map()) -> agent_state().
beamai_agent:update_metadata(Agent, Updates).
```

### Persistence

```erlang
%% Save Agent state to Memory
-spec save(agent_state()) -> ok | {error, term()}.
beamai_agent:save(Agent).

%% Restore Agent state from Memory
-spec restore(map(), term()) -> {ok, agent_state()} | {error, term()}.
beamai_agent:restore(Config, Memory).
```

### Configuration Options

```erlang
Config = #{
    %% Kernel construction (Method 1: pre-built Kernel, mutually exclusive with llm/plugins)
    kernel => beamai_kernel:kernel(),

    %% Kernel construction (Method 2: auto-build from components)
    llm => beamai_chat_completion:config(),  %% LLM configuration
    plugins => [module()],                   %% Plugin module list (e.g., beamai_tool_file)
    middlewares => [{module(), map()}],       %% Middleware configuration

    %% Agent configuration
    system_prompt => binary(),               %% System prompt
    max_tool_iterations => pos_integer(),    %% Tool loop max iterations, default 10
    callbacks => callbacks(),                %% Callback function map
    memory => term(),                        %% Persistence backend instance
    auto_save => boolean(),                  %% Auto-save after each turn, default false
    id => binary(),                          %% Agent ID (default auto-generated)
    name => binary(),                        %% Agent name (default <<"agent">>)
    metadata => map(),                       %% User-defined metadata
    kernel_settings => map(),                %% Kernel settings
    interrupt_tools => [map()]               %% Interrupt-related tool definitions
}.
```

### Callback System (8 Hooks)

```erlang
-type callbacks() :: #{
    on_turn_start  => fun((Meta :: map()) -> ok),
    on_turn_end    => fun((Meta :: map()) -> ok),
    on_turn_error  => fun((Reason :: term(), Meta :: map()) -> ok),
    on_llm_call    => fun((Messages :: [map()], Meta :: map()) -> ok),
    on_tool_call   => fun((Name :: binary(), Args :: map()) -> ok | {interrupt, term()}),
    on_token       => fun((Token :: binary(), Meta :: map()) -> ok),
    on_interrupt   => fun((InterruptState :: map(), Meta :: map()) -> ok),
    on_resume      => fun((InterruptState :: map(), Meta :: map()) -> ok)
}.
```

| Callback | Trigger | Special Behavior |
|----------|---------|------------------|
| `on_turn_start` | New turn begins | - |
| `on_turn_end` | Turn completes normally | - |
| `on_turn_error` | Turn error occurs | - |
| `on_llm_call` | Before each LLM call | Injected via pre_chat filter |
| `on_tool_call` | Before each tool call | Can return `{interrupt, Reason}` to trigger interrupt |
| `on_token` | Streaming mode receives token | Stream mode only |
| `on_interrupt` | Agent enters interrupted state | - |
| `on_resume` | Agent resumes from interrupt | - |

### Return Value Types

```erlang
-type run_result() :: #{
    content := binary(),               %% LLM final reply text
    tool_calls_made => [map()],        %% All tool call records from this turn
    finish_reason => binary(),         %% LLM stop reason
    usage => map(),                    %% Token usage statistics
    iterations => non_neg_integer()    %% Tool loop iteration count
}.

-type interrupt_info() :: #{
    reason := term(),                  %% Interrupt reason
    interrupt_type := tool_request | tool_result | callback,
    interrupted_tool_call => map(),
    completed_results => [map()],
    created_at := integer()
}.
```

---

## beamai_coordinator - Multi-Agent Coordinator

The coordinator is used to manage multiple Agents working together, supporting two modes: Pipeline (sequential execution) and Orchestrator (orchestrated execution).

**Important Change (v2.1):** Coordinator API parameters changed from using `Id` to using `CoordinatorPid`. Coordinator metadata is now stored in the Agent's `meta` field, solving the original ETS table process ownership issue.

### Starting Coordinator

```erlang
%% Generic start interface
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_link(Id, Opts).

%% Pipeline mode: tasks are passed sequentially between workers
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_pipeline(Id, Opts).

%% Orchestrator mode: coordinator orchestrates multiple workers
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_orchestrator(Id, Opts).
```

### Configuration Options

```erlang
Opts = #{
    agents => [agent_def()],           %% Worker Agent definition list
    llm => llm_config(),               %% LLM configuration
    system_prompt => binary(),         %% Optional: coordinator system prompt
    max_iterations => integer()        %% Optional: maximum iterations, default 10
}.

%% Agent definition
agent_def() = #{
    name := binary(),                  %% Worker name (required)
    system_prompt := binary()          %% Worker system prompt (required)
}.
```

### Stopping Coordinator

```erlang
%% Stop coordinator and all its workers
-spec stop(pid()) -> ok | {error, term()}.
beamai_coordinator:stop(CoordinatorPid).
```

### Workers Management

```erlang
%% Get all workers
-spec get_workers(pid()) -> {ok, #{binary() => pid()}} | {error, term()}.
beamai_coordinator:get_workers(CoordinatorPid).

%% Get specific worker
-spec get_worker(pid(), binary()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName).
```

### Task Delegation

```erlang
%% Delegate task to specific worker
-spec delegate(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task).

%% Delegate task to multiple workers in parallel
-spec delegate_parallel(pid(), [binary()], binary()) -> {ok, map()} | {error, term()}.
beamai_coordinator:delegate_parallel(CoordinatorPid, WorkerNames, Task).
%% Returns: {ok, #{WorkerName => {ok, Result} | {error, Reason}}}
```

### Usage Examples

```erlang
%% Pipeline example: Translation pipeline
LLM = beamai_chat_completion:create(bailian, #{model => <<"qwen-plus">>, api_key => ApiKey}),

{ok, Pipeline} = beamai_coordinator:start_pipeline(<<"translator">>, #{
    agents => [
        #{name => <<"cn_to_en">>, system_prompt => <<"Translate to English">>},
        #{name => <<"polisher">>, system_prompt => <<"Polish the English">>}
    ],
    llm => LLM
}),

%% Call a specific worker directly
{ok, Result} = beamai_coordinator:delegate(Pipeline, <<"cn_to_en">>, <<"Hello World">>),

%% Stop
beamai_coordinator:stop(Pipeline).
```

```erlang
%% Orchestrator example: Multi-expert consultation
{ok, Panel} = beamai_coordinator:start_orchestrator(<<"experts">>, #{
    agents => [
        #{name => <<"tech">>, system_prompt => <<"Technical expert">>},
        #{name => <<"biz">>, system_prompt => <<"Business expert">>}
    ],
    llm => LLM
}),

%% Consult multiple experts in parallel
{ok, Results} = beamai_coordinator:delegate_parallel(
    Panel, [<<"tech">>, <<"biz">>], <<"Analyze AI trends">>
),

beamai_coordinator:stop(Panel).
```

---

## Middleware System

Interceptor mechanism during Agent execution. The Middleware system is located in the `beamai_tools` module and is shared by `beamai_agent` and `beamai_deepagent`.

Detailed documentation: [MIDDLEWARE.md](MIDDLEWARE.md)

### beamai_middleware Behavior

```erlang
%% All callbacks are optional
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().
```

### Return Value Types

```erlang
-type middleware_result() ::
    ok |                              %% No modification
    {update, map()} |                 %% Update graph state
    {goto, model | tools | '__end__'} |  %% Jump
    {update_goto, map(), goto_target()} |  %% Update and jump
    {halt, term()} |                  %% Halt execution
    {interrupt, interrupt_action()}.  %% Interrupt waiting for confirmation
```

### beamai_middleware_runner

```erlang
%% Initialize Middleware chain
-spec init([middleware_spec()]) -> middleware_chain().
beamai_middleware_runner:init(Specs).

%% Middleware specification format
Specs = [
    {middleware_module, Opts},           %% Module + Options
    {middleware_module, Opts, Priority}, %% Module + Options + Priority
    middleware_module                    %% Module name only
].

%% Execute hook
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().
beamai_middleware_runner:run_hook(HookName, State, Middlewares).

%% Get Middleware state
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
beamai_middleware_runner:get_middleware_state(Module, Chain).
```

### beamai_middleware_presets

```erlang
%% Preset configurations
-spec default() -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% Presets with options
-spec default(map()) -> [middleware_spec()].
beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 25}
}).

%% Individual Middleware configuration
-spec call_limit(map()) -> middleware_spec().
-spec summarization(map()) -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
```

### Built-in Middleware

| Middleware | Module | Main Configuration |
|------------|--------|-------------------|
| Call Limit | `middleware_call_limit` | `max_model_calls`, `max_tool_calls`, `max_iterations` |
| Context Summarization | `middleware_summarization` | `window_size`, `max_tokens`, `summarize` |
| Human Approval | `middleware_human_approval` | `mode`, `timeout`, `tools` |
| Tool Retry | `middleware_tool_retry` | `max_retries`, `backoff` |
| Model Retry | `middleware_model_retry` | `max_retries`, `retryable_errors` |
| Model Fallback | `middleware_model_fallback` | `fallback_models`, `trigger_errors` |
| PII Detection | `middleware_pii_detection` | `action`, `types` |
| Tool Selection | `middleware_tool_selector` | `strategy`, `whitelist` |

### Custom Middleware Example

```erlang
-module(my_logging_middleware).
-behaviour(beamai_middleware).

-export([init/1, before_model/2, after_model/2]).

init(Opts) ->
    #{log_level => maps:get(log_level, Opts, info)}.

before_model(State, #{log_level := Level}) ->
    Messages = graph_state:get(State, messages, []),
    log(Level, "LLM Request: ~p messages", [length(Messages)]),
    {update, #{request_start => erlang:system_time(millisecond)}}.

after_model(State, #{log_level := Level}) ->
    Start = graph_state:get(State, request_start, 0),
    Duration = erlang:system_time(millisecond) - Start,
    log(Level, "LLM Response: ~pms", [Duration]),
    ok.

log(info, Fmt, Args) -> logger:info(Fmt, Args);
log(debug, Fmt, Args) -> logger:debug(Fmt, Args).
```

---

## beamai_deepagent - Deep Agent

Deep Agent supporting planning and parallel execution.

### Creation and Execution

```erlang
%% Create configuration
-spec new() -> config().
-spec new(map()) -> config().
beamai_deepagent:new().
beamai_deepagent:new(Opts).

%% Run Agent
-spec run(config(), binary()) -> {ok, result()} | {error, term()}.
beamai_deepagent:run(Config, Task).
```

### Result Queries

```erlang
%% Get plan
-spec get_plan(result()) -> plan() | undefined.
beamai_deepagent:get_plan(Result).

%% Get execution trace
-spec get_trace(result()) -> trace().
beamai_deepagent:get_trace(Result).
```

### Configuration Options

```erlang
Config = #{
    llm => llm_config(),                 %% LLM configuration
    tools => [tool_def()],               %% Custom tools
    system_prompt => binary(),           %% System prompt
    max_depth => integer(),              %% Maximum recursion depth, default 3
    max_iterations => integer(),         %% Maximum iterations, default 50
    planning_enabled => boolean(),       %% Enable planning, default true
    planning_mode => full | simple,      %% Planning mode, default full
    reflection_enabled => boolean(),     %% Enable reflection, default true
    filesystem_enabled => boolean(),     %% Enable filesystem tools
    filesystem => filesystem_config(),   %% Filesystem configuration
    human_in_loop => #{enabled => boolean()}  %% Human-in-loop configuration
}.
```

### Tool Provider

DeepAgent provides tools through `beamai_deepagent_tool_provider`, implementing the `beamai_tool_provider` behavior.

```erlang
%% Get tools via beamai_tool_registry
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% Direct access to tool collections
beamai_deepagent_tool_provider:base_tools().       %% Base tools
beamai_deepagent_tool_provider:plan_tools().       %% Plan tools
beamai_deepagent_tool_provider:subtask_tools().    %% Subtask tools
beamai_deepagent_tool_provider:reflect_tools().    %% Reflect tools
beamai_deepagent_tool_provider:filesystem_tools(). %% Filesystem tools
beamai_deepagent_tool_provider:todo_tools().       %% TodoList tools
beamai_deepagent_tool_provider:human_tools().      %% Human interaction tools

%% Provider interface
beamai_deepagent_tool_provider:info().             %% Get Provider info
beamai_deepagent_tool_provider:available().        %% Check if available
beamai_deepagent_tool_provider:list_tools(Opts).   %% Get tool list
beamai_deepagent_tool_provider:find_tool(Name, Opts). %% Find tool
```

### Tool Condition Judgment

| Tool Set | Condition |
|----------|-----------|
| Base tools | Always available |
| Plan tools | `planning_mode=full` and `depth=0` |
| TodoList tools | `planning_mode=simple` |
| Subtask tools | `depth < max_depth` |
| Reflect tools | `reflection_enabled=true` |
| Filesystem tools | `filesystem_enabled=true` or has `filesystem` config |
| Human tools | `human_in_loop.enabled=true` |

---

## beamai_llm - LLM Client

LLM client with multi-provider support and unified chat completion interface.

### LLM Configuration Management

LLM configuration is created using `beamai_chat_completion:create/2`:

```erlang
%% Create LLM configuration
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    max_tokens => 2048
}).

%% Configuration reuse: multiple Agents share the same configuration
{ok, Agent1} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Assistant 1">>}),
{ok, Agent2} = beamai_agent:new(#{llm => LLM, system_prompt => <<"Assistant 2">>}).
```

**Advantages:**
- Configuration reuse: multiple Agents share the same LLM configuration
- Centralized management: API Key, model parameters unified configuration
- Provider abstraction: 7 built-in providers + custom provider support
- Automatic retry: built-in exponential backoff retry mechanism

### Configuration and Chat

```erlang
%% Create configuration
-spec create(provider(), map()) -> config().
beamai_chat_completion:create(Provider, Opts).

%% Chat completion
-spec chat(config(), [map()]) -> {ok, map()} | {error, term()}.
beamai_chat_completion:chat(Config, Messages).

%% Chat completion with options (supports tools, tool_choice, max_retries, etc.)
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:chat(Config, Messages, Opts).

%% Streaming chat
-spec stream_chat(config(), [map()], fun()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:stream_chat(Config, Messages, Callback).

-spec stream_chat(config(), [map()], fun(), map()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:stream_chat(Config, Messages, Callback, Opts).
```

### Chat Options

```erlang
Opts = #{
    tools => [tool_spec()],           %% Tool definition list
    tool_choice => auto | none | required, %% Tool selection strategy
    max_retries => integer(),         %% Retry count (default 3)
    retry_delay => integer(),         %% Base retry delay in ms (default 1000)
    on_retry => fun(RetryState) -> ok %% Retry callback
}.
```

### Supported Providers

| Provider | Module | API Mode | Features |
|----------|--------|----------|----------|
| `openai` | llm_provider_openai | OpenAI | Chat, streaming, tool calling |
| `anthropic` | llm_provider_anthropic | Anthropic | Chat, streaming, tool calling |
| `deepseek` | llm_provider_deepseek | OpenAI compatible | Chat, streaming, tool calling |
| `zhipu` | llm_provider_zhipu | OpenAI compatible | Chat, streaming, tool calling, async |
| `bailian` | llm_provider_bailian | DashScope native | Chat, streaming, tool calling, web search |
| `ollama` | llm_provider_ollama | OpenAI compatible | Chat, streaming |
| `mock` | llm_provider_mock | Built-in | Mock LLM for testing |
| `{custom, Module}` | Custom | Custom | User-defined provider |

### DeepSeek Detailed Description

DeepSeek Provider uses OpenAI compatible API, supporting `deepseek-chat` and `deepseek-reasoner` models.

**Supported Models:**
- `deepseek-chat`: General conversation model (default)
- `deepseek-reasoner`: Reasoning enhanced model

**Configuration Example:**
```erlang
LLM = beamai_chat_completion:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY")),
    max_tokens => 4096
}).
```

### Alibaba Cloud Bailian (DashScope) Detailed Description

Bailian Provider uses DashScope native API, automatically selecting endpoints based on model type:
- **Text generation models** (`qwen-plus`, `qwen-max`, `qwen-turbo`): Use `/api/v1/services/aigc/text-generation/generation`
- **Multimodal models** (`qwen-vl-plus`, `qwen-audio`, etc.): Use `/api/v1/services/aigc/multimodal-generation/generation`

**Unique Parameters:**
- `enable_search => true`: Enable web search functionality
- `tool_choice => <<"required">>`: Force tool calling

**Streaming Output:**
- Request header: `X-DashScope-SSE: enable`
- Parameter: `parameters.incremental_output: true`

### LLM Configuration Parameters

`beamai_chat_completion:create/2` supports the following parameters:

```erlang
LLM = beamai_chat_completion:create(Provider, #{
    model => binary(),                   %% Model name (required)
    api_key => binary(),                 %% API Key (required, except ollama/mock)
    base_url => binary(),                %% Optional: custom URL
    timeout => integer(),                %% Optional: timeout (milliseconds)
    max_tokens => integer(),             %% Optional: maximum tokens
    temperature => float()               %% Optional: temperature parameter (0.0 - 2.0)
}).
```

**Provider Types:** `openai | anthropic | deepseek | zhipu | bailian | ollama | mock | {custom, module()}`

**Config Type Marker:** The returned config map includes `'__llm_config__' => true` marker for internal validation.

---

## beamai_memory - Memory Management

Unified memory and checkpoint management system.

### Creation and Configuration

```erlang
%% Create Memory instance
-spec new(map()) -> {ok, memory()} | {error, term()}.
beamai_memory:new(Config).

Config = #{
    checkpointer => #{backend => ets | sqlite},
    store => #{backend => ets | sqlite},
    context_store => {module(), term()}
}.
```

### Checkpoint Operations

```erlang
%% Save checkpoint
-spec save_checkpoint(memory(), config(), state_data()) -> {ok, memory()}.
beamai_memory:save_checkpoint(Memory, Config, StateData).

%% Load checkpoint
-spec load_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
-spec load_latest_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
beamai_memory:load_checkpoint(Memory, Config).
beamai_memory:load_latest_checkpoint(Memory, Config).

%% List checkpoints
-spec list_checkpoints(memory(), config()) -> {ok, [checkpoint_info()]}.
beamai_memory:list_checkpoints(Memory, Config).

%% Checkpoint count
-spec checkpoint_count(memory(), config()) -> non_neg_integer().
beamai_memory:checkpoint_count(Memory, Config).
```

### Store Operations

```erlang
%% Store data
-spec put(memory(), namespace(), key(), value()) -> {ok, memory()}.
beamai_memory:put(Memory, Namespace, Key, Value).

%% Search data
-spec search(memory(), namespace(), filter()) -> {ok, [item()]}.
beamai_memory:search(Memory, Namespace, Filter).
```

---

## beamai_tools - Tool Library and Middleware System

Unified tool definition and management, as well as Agent execution middleware system.

beamai_tools contains two core functionalities:
- **Tool System**: Tool definition, registration, Provider mechanism
- **Middleware System**: Agent execution interception, enhancement, and control (see [Middleware System](#middleware-system))

### Tool Retrieval

```erlang
%% Get tools
-spec get_tools(category() | [category()]) -> [tool_def()].
-spec get_tools(category(), map()) -> [tool_def()].
beamai_tools:get_tools(Categories).
beamai_tools:get_tools(Categories, Opts).

%% Get all tools
-spec get_all_tools() -> [tool_def()].
beamai_tools:get_all_tools().

%% Find tool
-spec find_tool(binary()) -> {ok, tool_def()} | {error, not_found}.
beamai_tools:find_tool(Name).
```

### Tool Execution

```erlang
%% Execute tool
-spec execute(binary(), map()) -> {ok, term()} | {error, term()}.
-spec execute(binary(), map(), map()) -> {ok, term()} | {error, term()}.
beamai_tools:execute(ToolName, Args).
beamai_tools:execute(ToolName, Args, Opts).
```

### Tool Conversion

```erlang
%% Convert to LLM format
-spec to_llm_spec(tool_def()) -> map().
-spec to_llm_specs([tool_def()]) -> [map()].
beamai_tools:to_llm_spec(Tool).
beamai_tools:to_llm_specs(Tools).
```

### Tool Registry

```erlang
%% Build tool list
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, Tools),
R2 = beamai_tool_registry:add_provider(R1, Provider),
Tools = beamai_tool_registry:build(R2).

%% Convenience function
Tools = beamai_tool_registry:from_config(#{
    tools => [Tool1, Tool2],
    providers => [Provider1, Provider2]
}).
```

---

## beamai_core - Core Module

### Graph Execution Engine

```erlang
%% Build Graph
Graph = graph_builder:new()
    |> graph_builder:add_node(NodeName, {Module, Opts})
    |> graph_builder:add_edge(From, To, Condition)
    |> graph_builder:set_entry(EntryNode)
    |> graph_builder:build().

%% Execute Graph
-spec run(graph(), state()) -> {ok, state()} | {error, term()}.
graph_runner:run(Graph, InitialState).

%% Graph DSL
Graph = graph_dsl:compile(#{
    nodes => #{...},
    edges => [...],
    entry => atom()
}).
```

### Graph State

```erlang
%% Create state
-spec new(map()) -> state().
graph_state:new(Data).

%% Read/Write state
-spec get(state(), key()) -> value().
-spec set(state(), key(), value()) -> state().
graph_state:get(State, Key).
graph_state:set(State, Key, Value).

%% User context operations
-spec get_context(state()) -> map().
-spec get_context(state(), key()) -> value() | undefined.
-spec set_context(state(), map()) -> state().
-spec update_context(state(), map()) -> state().
graph_state:get_context(State).
graph_state:get_context(State, Key).
graph_state:set_context(State, Context).
graph_state:update_context(State, Updates).
```

### Graph State Reducer

Field-level reducers for merging node-returned deltas into global state.

```erlang
%% Apply delta
-spec apply_delta(state(), delta(), field_reducers()) -> state().
-spec apply_deltas(state(), [delta()], field_reducers()) -> state().
graph_state_reducer:apply_delta(State, Delta, FieldReducers).
graph_state_reducer:apply_deltas(State, Deltas, FieldReducers).

%% Built-in Reducers
graph_state_reducer:append_reducer(Old, New) -> list().
graph_state_reducer:merge_reducer(Old, New) -> map().
graph_state_reducer:increment_reducer(Old, Delta) -> number().
graph_state_reducer:last_write_win_reducer(Old, New) -> term().
```

**Reducer Configuration Format:**

```erlang
FieldReducers = #{
    %% Normal reducer
    <<"messages">> => fun graph_state_reducer:append_reducer/2,

    %% Transform reducer: accumulate counter_incr to counter
    <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
}.
```

### Pregel Distributed Computing

```erlang
%% Create Pregel graph
{ok, Graph} = pregel_graph:new(Config).

%% Add vertices and edges
pregel_graph:add_vertex(Graph, VertexId, Data).
pregel_graph:add_edge(Graph, From, To, Weight).

%% Run computation
{ok, Result} = pregel:run(Graph, ComputeFn, MaxIterations).
```

### HTTP Client

BeamAI provides a unified HTTP client interface, supporting both Gun and Hackney backends.

```erlang
%% Send request (automatically uses configured backend)
-spec request(method(), url(), headers(), body(), opts()) -> {ok, response()} | {error, term()}.
beamai_http:request(Method, Url, Headers, Body, Opts).

%% Convenience functions
-spec get(url(), headers()) -> {ok, response()} | {error, term()}.
-spec post(url(), headers(), body()) -> {ok, response()} | {error, term()}.
beamai_http:get(Url, Headers).
beamai_http:post(Url, Headers, Body).

%% Streaming request (SSE)
-spec stream_request(url(), headers(), body(), callback(), opts()) -> {ok, term()} | {error, term()}.
beamai_http:stream_request(Url, Headers, Body, Callback, Opts).
```

### HTTP Backend Configuration

```erlang
%% Application configuration (sys.config)
{beamai_core, [
    %% Select HTTP backend: beamai_http_gun (default) or beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun connection pool configuration
    {http_pool, #{
        max_connections => 100,        %% Maximum connections
        connection_timeout => 30000,   %% Connection timeout (milliseconds)
        idle_timeout => 60000          %% Idle timeout (milliseconds)
    }}
]}.
```

**Backend Comparison:**

| Feature | Gun (default) | Hackney |
|---------|---------------|---------|
| HTTP/2 | Supported | Not supported |
| Connection pool | beamai_http_pool | hackney built-in pool |
| TLS | Automatically uses system CA certificates (OTP 25+) | hackney default config |
| Dependency | gun 2.1.0 | hackney |
| Recommended scenario | Production, needs HTTP/2 | Legacy system compatibility |

### HTTP Connection Pool (Gun Backend)

When using the Gun backend, beamai_http_pool is automatically started as a child process of the beamai_core application.

```erlang
%% Connection pool API
-spec checkout(host(), port(), protocol()) -> {ok, connection()} | {error, term()}.
beamai_http_pool:checkout(Host, Port, Protocol).

-spec checkin(connection()) -> ok.
beamai_http_pool:checkin(Conn).

%% View connection pool status
-spec get_stats() -> map().
beamai_http_pool:get_stats().
```

---

## beamai_a2a - A2A Protocol

Agent-to-Agent communication protocol implementation.

### Server

```erlang
%% Start server
-spec start_link(map()) -> {ok, pid()}.
beamai_a2a_server:start_link(Config).

Config = #{
    handler => module(),                 %% Request handler
    port => integer(),                   %% HTTP port
    auth => auth_config()                %% Authentication configuration
}.
```

### Client

```erlang
%% Discover Agent
-spec discover(binary()) -> {ok, agent_card()} | {error, term()}.
beamai_a2a_client:discover(AgentUrl).

%% Send message
-spec send_message(binary(), message()) -> {ok, response()} | {error, term()}.
beamai_a2a_client:send_message(AgentUrl, Message).
```

### Agent Card

```erlang
%% Create Card
-spec new(map()) -> agent_card().
beamai_a2a_card:new(#{
    name => binary(),
    description => binary(),
    url => binary(),
    capabilities => [binary()]
}).

%% Validate Card
-spec validate(agent_card()) -> ok | {error, term()}.
beamai_a2a_card:validate(Card).
```

---

## beamai_mcp - MCP Protocol

Model Context Protocol implementation.

### Client

```erlang
%% Connect to MCP server
-spec connect(config()) -> {ok, client()} | {error, term()}.
beamai_mcp_client:connect(Config).

Config = #{
    transport => stdio | http | sse,
    command => binary(),                 %% stdio: command
    url => binary()                      %% http/sse: URL
}.

%% List tools
-spec list_tools(client()) -> {ok, [tool()]} | {error, term()}.
beamai_mcp_client:list_tools(Client).

%% Call tool
-spec call_tool(client(), binary(), map()) -> {ok, result()} | {error, term()}.
beamai_mcp_client:call_tool(Client, ToolName, Args).

%% List resources
-spec list_resources(client()) -> {ok, [resource()]} | {error, term()}.
beamai_mcp_client:list_resources(Client).
```

### Server

```erlang
%% Start server
-spec start_link(config()) -> {ok, pid()}.
beamai_mcp_server:start_link(Config).

%% Register tool
-spec register_tool(pid(), tool_def()) -> ok.
beamai_mcp_server:register_tool(Server, Tool).

%% Register resource
-spec register_resource(pid(), resource_def()) -> ok.
beamai_mcp_server:register_resource(Server, Resource).
```

---

## beamai_rag - RAG Functionality

Retrieval Augmented Generation module.

### Vector Embeddings

```erlang
%% Generate embeddings
-spec embed(binary(), config()) -> {ok, [float()]} | {error, term()}.
-spec embed_batch([binary()], config()) -> {ok, [[float()]]} | {error, term()}.
beamai_embeddings:embed(Text, Config).
beamai_embeddings:embed_batch(Texts, Config).
```

### Vector Store

```erlang
%% Create store
-spec new(config()) -> {ok, store()}.
beamai_vector_store:new(Config).

%% Add vector
-spec add(store(), binary(), [float()], map()) -> {ok, store()}.
beamai_vector_store:add(Store, Id, Vector, Metadata).

%% Similarity search
-spec search(store(), [float()], integer()) -> {ok, [result()]}.
beamai_vector_store:search(Store, QueryVector, TopK).
```

### RAG Workflow

```erlang
%% Initialize
-spec init(config()) -> {ok, rag_state()}.
beamai_rag:init(Config).

%% Add documents
-spec add_documents(rag_state(), [document()]) -> {ok, rag_state()}.
beamai_rag:add_documents(State, Documents).

%% Retrieve
-spec retrieve(rag_state(), binary(), integer()) -> {ok, [chunk()]}.
beamai_rag:retrieve(State, Query, TopK).

%% RAG query (retrieve + generate)
-spec query(rag_state(), binary()) -> {ok, response()}.
beamai_rag:query(State, Question).
```

---

## Common Types

### Tool Definition (tool_spec)

```erlang
-type tool_spec() :: #{
    name := binary(),                    % Required: tool name
    handler := handler(),                % Required: handler function
    description => binary(),             % Optional: description (for LLM understanding)
    parameters => parameters_schema(),   % Optional: parameter definitions
    tag => binary() | [binary()],        % Optional: classification tags
    timeout => pos_integer(),            % Optional: timeout (milliseconds)
    retry => #{max => integer(), delay => integer()},  % Optional: retry strategy
    metadata => map()                    % Optional: custom metadata
}.

-type handler() ::
    fun((args()) -> tool_result())                        % fun/1: args only
    | fun((args(), beamai_context:t()) -> tool_result())  % fun/2: args + context
    | {module(), atom()}                                  % {M, F}: module function
    | {module(), atom(), [term()]}.                       % {M, F, ExtraArgs}

-type tool_result() ::
    {ok, term()}                          % Success with result
    | {ok, term(), beamai_context:t()}    % Success with result and updated context
    | {error, term()}.                    % Failure
```

### Message Types

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.
```

### LLM Response

```erlang
-type llm_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls := [tool_call()],
    finish_reason := binary(),
    usage => usage_info()
}.
```

---

## Error Handling

All APIs return `{ok, Result}` or `{error, Reason}` format. Common error types:

| Error | Description |
|-------|-------------|
| `{error, missing_api_key}` | API Key not configured |
| `{error, timeout}` | Request timeout |
| `{error, {http_error, Code, Body}}` | HTTP error |
| `{error, {api_error, Details}}` | API returned error |
| `{error, not_found}` | Resource not found |
| `{error, storage_not_enabled}` | Storage not enabled |

---

## More Documentation

- [README.md](../README.md) - Project overview
- [ARCHITECTURE.md](ARCHITECTURE.md) - Architecture design
- Module READMEs:
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)
  - [beamai_deepagent](../apps/beamai_deepagent/README.md)
  - [beamai_memory](../apps/beamai_memory/README.md)
  - [beamai_tools](../apps/beamai_tools/README.md)
  - [beamai_a2a](../apps/beamai_a2a/README.md)
  - [beamai_mcp](../apps/beamai_mcp/README.md)
  - [beamai_rag](../apps/beamai_rag/README.md)
