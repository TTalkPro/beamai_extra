# API 参考文档

[English](API_REFERENCE_EN.md) | 中文

本文档提供 BeamAI Framework 各模块的主要 API 参考。

## 目录

- [beamai_agent - Simple Agent](#beamai_agent---simple-agent)
- [beamai_coordinator - 多 Agent 协调器](#beamai_coordinator---多-agent-协调器)
- [Middleware 系统](#middleware-系统)
- [beamai_deepagent - Deep Agent](#beamai_deepagent---deep-agent)
- [beamai_llm - LLM 客户端](#beamai_llm---llm-客户端)
- [beamai_memory - 记忆管理](#beamai_memory---记忆管理)
- [beamai_tools - 工具库](#beamai_tools---工具库)
- [beamai_core - 核心模块](#beamai_core---核心模块)
  - [HTTP 客户端](#http-客户端)
  - [HTTP 后端配置](#http-后端配置)
- [beamai_a2a - A2A 协议](#beamai_a2a---a2a-协议)
- [beamai_mcp - MCP 协议](#beamai_mcp---mcp-协议)
- [beamai_rag - RAG 功能](#beamai_rag---rag-功能)

---

## beamai_agent - Simple Agent

纯函数式 ReAct Agent 实现，通过不可变状态传递实现多轮对话。

### 创建 Agent

```erlang
%% 创建 Agent（返回不可变状态）
-spec new(map()) -> {ok, agent_state()} | {error, term()}.
beamai_agent:new(Config).
```

### 执行 API

```erlang
%% 运行一轮对话（返回结果和新状态）
-spec run(agent_state(), binary()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:run(Agent, UserMessage).

%% 带选项运行
-spec run(agent_state(), binary(), map()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:run(Agent, UserMessage, Opts).

%% 流式输出
-spec stream(agent_state(), binary()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:stream(Agent, UserMessage).

-spec stream(agent_state(), binary(), map()) ->
    {ok, run_result(), agent_state()} | {error, term()}.
beamai_agent:stream(Agent, UserMessage, Opts).
```

### 中断和恢复（Human-in-the-Loop）

```erlang
%% 恢复被中断的 Agent
-spec resume(agent_state(), term()) ->
    {ok, run_result(), agent_state()} |
    {interrupt, interrupt_info(), agent_state()} |
    {error, term()}.
beamai_agent:resume(Agent, HumanInput).

%% 从 Memory 加载并恢复
-spec resume_from_memory(map(), term(), term()) ->
    {ok, run_result(), agent_state()} |
    {interrupt, interrupt_info(), agent_state()} |
    {error, term()}.
beamai_agent:resume_from_memory(Config, Memory, HumanInput).

%% 检查中断状态
-spec is_interrupted(agent_state()) -> boolean().
beamai_agent:is_interrupted(Agent).

-spec get_interrupt_info(agent_state()) -> interrupt_info() | undefined.
beamai_agent:get_interrupt_info(Agent).
```

### 状态查询

```erlang
%% 获取 Agent ID
-spec id(agent_state()) -> binary().
beamai_agent:id(Agent).

%% 获取 Agent 名称
-spec name(agent_state()) -> binary().
beamai_agent:name(Agent).

%% 获取对话消息历史
-spec messages(agent_state()) -> [map()].
beamai_agent:messages(Agent).

%% 获取上一次助手回复
-spec last_response(agent_state()) -> binary() | undefined.
beamai_agent:last_response(Agent).

%% 获取已完成的对话轮数
-spec turn_count(agent_state()) -> non_neg_integer().
beamai_agent:turn_count(Agent).

%% 获取内部 Kernel 实例
-spec kernel(agent_state()) -> beamai_kernel:kernel().
beamai_agent:kernel(Agent).
```

### 状态修改

```erlang
%% 设置系统提示词
-spec set_system_prompt(agent_state(), binary()) -> agent_state().
beamai_agent:set_system_prompt(Agent, Prompt).

%% 手动添加消息到历史
-spec add_message(agent_state(), map()) -> agent_state().
beamai_agent:add_message(Agent, Message).

%% 清除消息历史
-spec clear_messages(agent_state()) -> agent_state().
beamai_agent:clear_messages(Agent).

%% 更新用户元数据（合并模式）
-spec update_metadata(agent_state(), map()) -> agent_state().
beamai_agent:update_metadata(Agent, Updates).
```

### 持久化

```erlang
%% 保存 Agent 状态到 Memory
-spec save(agent_state()) -> ok | {error, term()}.
beamai_agent:save(Agent).

%% 从 Memory 恢复 Agent 状态
-spec restore(map(), term()) -> {ok, agent_state()} | {error, term()}.
beamai_agent:restore(Config, Memory).
```

### 配置选项

```erlang
Config = #{
    %% Kernel 构建（方式一：预构建 Kernel，与 llm/plugins 互斥）
    kernel => beamai_kernel:kernel(),

    %% Kernel 构建（方式二：从组件自动构建）
    llm => beamai_chat_completion:config(),  %% LLM 配置
    plugins => [module()],                   %% Plugin 模块列表（如 beamai_tool_file）
    middlewares => [{module(), map()}],       %% Middleware 配置

    %% Agent 配置
    system_prompt => binary(),               %% 系统提示词
    max_tool_iterations => pos_integer(),    %% 工具循环最大迭代次数，默认 10
    callbacks => callbacks(),                %% 回调函数 map
    memory => term(),                        %% 持久化后端实例
    auto_save => boolean(),                  %% 每轮后自动保存，默认 false
    id => binary(),                          %% Agent ID（默认自动生成）
    name => binary(),                        %% Agent 名称（默认 <<"agent">>）
    metadata => map(),                       %% 用户自定义元数据
    kernel_settings => map(),                %% Kernel 设置项
    interrupt_tools => [map()]               %% 中断相关 Tool 定义
}.
```

### 回调系统（8 个钩子）

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

| 回调 | 触发时机 | 特殊行为 |
|------|----------|----------|
| `on_turn_start` | 新 turn 开始 | - |
| `on_turn_end` | turn 正常完成 | - |
| `on_turn_error` | turn 出错 | - |
| `on_llm_call` | 每次 LLM 调用前 | 通过 pre_chat filter 注入 |
| `on_tool_call` | 每次工具调用前 | 可返回 `{interrupt, Reason}` 触发中断 |
| `on_token` | 流式模式收到 token | 仅 stream 模式 |
| `on_interrupt` | Agent 进入中断状态 | - |
| `on_resume` | Agent 从中断恢复 | - |

### 返回值类型

```erlang
-type run_result() :: #{
    content := binary(),               %% LLM 最终回复文本
    tool_calls_made => [map()],        %% 本轮执行的所有工具调用记录
    finish_reason => binary(),         %% LLM 停止原因
    usage => map(),                    %% Token 使用统计
    iterations => non_neg_integer()    %% 工具循环迭代次数
}.

-type interrupt_info() :: #{
    reason := term(),                  %% 中断原因
    interrupt_type := tool_request | tool_result | callback,
    interrupted_tool_call => map(),
    completed_results => [map()],
    created_at := integer()
}.
```

---

## beamai_coordinator - 多 Agent 协调器

协调器用于管理多个 Agent 协同工作，支持 Pipeline（顺序执行）和 Orchestrator（编排执行）两种模式。

**重要变更（v2.1）：** 协调器 API 参数从使用 `Id` 改为使用 `CoordinatorPid`。协调器元数据存储在 Agent 的 `meta` 字段中，解决了原 ETS 表的进程所有权问题。

### 启动协调器

```erlang
%% 通用启动接口
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_link(Id, Opts).

%% Pipeline 模式：任务在 workers 间顺序传递
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_pipeline(Id, Opts).

%% Orchestrator 模式：协调器编排多个 workers
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:start_orchestrator(Id, Opts).
```

### 配置选项

```erlang
Opts = #{
    agents => [agent_def()],           %% Worker Agent 定义列表
    llm => llm_config(),               %% LLM 配置
    system_prompt => binary(),         %% 可选：协调器系统提示词
    max_iterations => integer()        %% 可选：最大迭代次数，默认 10
}.

%% Agent 定义
agent_def() = #{
    name := binary(),                  %% Worker 名称（必需）
    system_prompt := binary()          %% Worker 系统提示词（必需）
}.
```

### 停止协调器

```erlang
%% 停止协调器及其所有 workers
-spec stop(pid()) -> ok | {error, term()}.
beamai_coordinator:stop(CoordinatorPid).
```

### Workers 管理

```erlang
%% 获取所有 workers
-spec get_workers(pid()) -> {ok, #{binary() => pid()}} | {error, term()}.
beamai_coordinator:get_workers(CoordinatorPid).

%% 获取指定 worker
-spec get_worker(pid(), binary()) -> {ok, pid()} | {error, term()}.
beamai_coordinator:get_worker(CoordinatorPid, WorkerName).
```

### 任务委托

```erlang
%% 委托任务给指定 worker
-spec delegate(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
beamai_coordinator:delegate(CoordinatorPid, WorkerName, Task).

%% 并行委托任务给多个 workers
-spec delegate_parallel(pid(), [binary()], binary()) -> {ok, map()} | {error, term()}.
beamai_coordinator:delegate_parallel(CoordinatorPid, WorkerNames, Task).
%% 返回: {ok, #{WorkerName => {ok, Result} | {error, Reason}}}
```

### 使用示例

```erlang
%% Pipeline 示例：翻译流水线
LLM = beamai_chat_completion:create(bailian, #{model => <<"qwen-plus">>, api_key => ApiKey}),

{ok, Pipeline} = beamai_coordinator:start_pipeline(<<"translator">>, #{
    agents => [
        #{name => <<"cn_to_en">>, system_prompt => <<"翻译成英文">>},
        #{name => <<"polisher">>, system_prompt => <<"润色英文">>}
    ],
    llm => LLM
}),

%% 直接调用某个 worker
{ok, Result} = beamai_coordinator:delegate(Pipeline, <<"cn_to_en">>, <<"你好世界">>),

%% 停止
beamai_coordinator:stop(Pipeline).
```

```erlang
%% Orchestrator 示例：多专家咨询
{ok, Panel} = beamai_coordinator:start_orchestrator(<<"experts">>, #{
    agents => [
        #{name => <<"tech">>, system_prompt => <<"技术专家">>},
        #{name => <<"biz">>, system_prompt => <<"商业专家">>}
    ],
    llm => LLM
}),

%% 并行咨询多个专家
{ok, Results} = beamai_coordinator:delegate_parallel(
    Panel, [<<"tech">>, <<"biz">>], <<"分析 AI 趋势">>
),

beamai_coordinator:stop(Panel).
```

---

## Middleware 系统

Agent 执行过程中的拦截器机制。Middleware 系统位于 `beamai_tools` 模块中，被 `beamai_agent` 和 `beamai_deepagent` 共享使用。

详细文档：[MIDDLEWARE.md](MIDDLEWARE.md)

### beamai_middleware 行为

```erlang
%% 所有回调都是可选的
-callback init(Opts :: map()) -> middleware_state().
-callback before_agent(State, MwState) -> middleware_result().
-callback after_agent(State, MwState) -> middleware_result().
-callback before_model(State, MwState) -> middleware_result().
-callback after_model(State, MwState) -> middleware_result().
-callback before_tools(State, MwState) -> middleware_result().
-callback after_tools(State, MwState) -> middleware_result().
```

### 返回值类型

```erlang
-type middleware_result() ::
    ok |                              %% 无修改
    {update, map()} |                 %% 更新图状态
    {goto, model | tools | '__end__'} |  %% 跳转
    {update_goto, map(), goto_target()} |  %% 更新并跳转
    {halt, term()} |                  %% 中止执行
    {interrupt, interrupt_action()}.  %% 中断等待确认
```

### beamai_middleware_runner

```erlang
%% 初始化 Middleware 链
-spec init([middleware_spec()]) -> middleware_chain().
beamai_middleware_runner:init(Specs).

%% Middleware 规格格式
Specs = [
    {middleware_module, Opts},           %% 模块 + 选项
    {middleware_module, Opts, Priority}, %% 模块 + 选项 + 优先级
    middleware_module                    %% 仅模块名
].

%% 执行钩子
-spec run_hook(hook_name(), graph_state(), middleware_chain()) -> run_result().
beamai_middleware_runner:run_hook(HookName, State, Middlewares).

%% 获取 Middleware 状态
-spec get_middleware_state(module(), middleware_chain()) -> {ok, state()} | {error, not_found}.
beamai_middleware_runner:get_middleware_state(Module, Chain).
```

### beamai_middleware_presets

```erlang
%% 预设配置
-spec default() -> [middleware_spec()].
-spec minimal() -> [middleware_spec()].
-spec production() -> [middleware_spec()].
-spec development() -> [middleware_spec()].
-spec human_in_loop() -> [middleware_spec()].

%% 带选项的预设
-spec default(map()) -> [middleware_spec()].
beamai_middleware_presets:default(#{
    call_limit => #{max_model_calls => 30},
    summarization => #{window_size => 25}
}).

%% 单独 Middleware 配置
-spec call_limit(map()) -> middleware_spec().
-spec summarization(map()) -> middleware_spec().
-spec human_approval(map()) -> middleware_spec().
-spec tool_retry(map()) -> middleware_spec().
```

### 内置 Middleware

| Middleware | 模块 | 主要配置 |
|------------|------|----------|
| 调用限制 | `middleware_call_limit` | `max_model_calls`, `max_tool_calls`, `max_iterations` |
| 上下文摘要 | `middleware_summarization` | `window_size`, `max_tokens`, `summarize` |
| 人工审批 | `middleware_human_approval` | `mode`, `timeout`, `tools` |
| 工具重试 | `middleware_tool_retry` | `max_retries`, `backoff` |
| 模型重试 | `middleware_model_retry` | `max_retries`, `retryable_errors` |
| 模型降级 | `middleware_model_fallback` | `fallback_models`, `trigger_errors` |
| PII 检测 | `middleware_pii_detection` | `action`, `types` |
| 工具选择 | `middleware_tool_selector` | `strategy`, `whitelist` |

### 自定义 Middleware 示例

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

支持规划和并行执行的深度 Agent。

### 创建和执行

```erlang
%% 创建配置
-spec new() -> config().
-spec new(map()) -> config().
beamai_deepagent:new().
beamai_deepagent:new(Opts).

%% 运行 Agent
-spec run(config(), binary()) -> {ok, result()} | {error, term()}.
beamai_deepagent:run(Config, Task).
```

### 结果查询

```erlang
%% 获取计划
-spec get_plan(result()) -> plan() | undefined.
beamai_deepagent:get_plan(Result).

%% 获取执行轨迹
-spec get_trace(result()) -> trace().
beamai_deepagent:get_trace(Result).
```

### 配置选项

```erlang
Config = #{
    llm => llm_config(),                 %% LLM 配置
    tools => [tool_def()],               %% 自定义工具
    system_prompt => binary(),           %% 系统提示词
    max_depth => integer(),              %% 最大递归深度，默认 3
    max_iterations => integer(),         %% 最大迭代次数，默认 50
    planning_enabled => boolean(),       %% 启用规划，默认 true
    planning_mode => full | simple,      %% 规划模式，默认 full
    reflection_enabled => boolean(),     %% 启用反思，默认 true
    filesystem_enabled => boolean(),     %% 启用文件系统工具
    filesystem => filesystem_config(),   %% 文件系统配置
    human_in_loop => #{enabled => boolean()}  %% Human-in-loop 配置
}.
```

### 工具提供者

DeepAgent 通过 `beamai_deepagent_tool_provider` 提供工具，实现 `beamai_tool_provider` 行为。

```erlang
%% 通过 beamai_tool_registry 获取工具
Config = #{depth => 0, planning_mode => full},
Tools = beamai_tool_registry:from_config(#{
    providers => [{beamai_deepagent_tool_provider, Config}]
}).

%% 直接访问工具集合
beamai_deepagent_tool_provider:base_tools().       %% 基础工具
beamai_deepagent_tool_provider:plan_tools().       %% 计划工具
beamai_deepagent_tool_provider:subtask_tools().    %% 子任务工具
beamai_deepagent_tool_provider:reflect_tools().    %% 反思工具
beamai_deepagent_tool_provider:filesystem_tools(). %% 文件系统工具
beamai_deepagent_tool_provider:todo_tools().       %% TodoList 工具
beamai_deepagent_tool_provider:human_tools().      %% Human 交互工具

%% Provider 接口
beamai_deepagent_tool_provider:info().             %% 获取 Provider 信息
beamai_deepagent_tool_provider:available().        %% 检查是否可用
beamai_deepagent_tool_provider:list_tools(Opts).   %% 获取工具列表
beamai_deepagent_tool_provider:find_tool(Name, Opts). %% 查找工具
```

### 工具条件判断

| 工具集 | 条件 |
|--------|------|
| 基础工具 | 始终可用 |
| 计划工具 | `planning_mode=full` 且 `depth=0` |
| TodoList 工具 | `planning_mode=simple` |
| 子任务工具 | `depth < max_depth` |
| 反思工具 | `reflection_enabled=true` |
| 文件系统工具 | `filesystem_enabled=true` 或有 `filesystem` 配置 |
| Human 工具 | `human_in_loop.enabled=true` |

---

## beamai_llm - LLM 客户端

多 Provider 支持的 LLM 客户端，统一的聊天补全接口。

### LLM 配置管理

LLM 配置使用 `beamai_chat_completion:create/2` 创建：

```erlang
%% 直接创建 LLM 配置
LLM = beamai_chat_completion:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
    max_tokens => 2048
}).

%% 使用辅助模块（从环境变量自动读取配置）
LLM = example_llm_config:anthropic().     %% Zhipu Anthropic 兼容 API
LLM = example_llm_config:claude().        %% Anthropic 原生 API
LLM = example_llm_config:zhipu().         %% Zhipu 原生 API
LLM = example_llm_config:openai_glm().    %% Zhipu OpenAI 兼容 API
LLM = example_llm_config:deepseek().      %% DeepSeek API
LLM = example_llm_config:openai().        %% OpenAI API

%% 支持 ZHIPU_ANTHROPIC_BASE_URL 环境变量自定义 API 地址
LLM = test_zhipu_anthropic:create_llm().

%% 配置复用：多个 Agent 共享同一配置
{ok, Agent1} = beamai_agent:new(#{llm => LLM, system_prompt => <<"助手1"/utf8>>}),
{ok, Agent2} = beamai_agent:new(#{llm => LLM, system_prompt => <<"助手2"/utf8>>}).
```

**环境变量：**

| 环境变量 | 说明 |
|----------|------|
| `ZHIPU_API_KEY` | 智谱 API Key |
| `ZHIPU_ANTHROPIC_BASE_URL` | Zhipu Anthropic 兼容 API 地址（默认 `https://open.bigmodel.cn/api/anthropic`） |
| `ANTHROPIC_API_KEY` | Anthropic 原生 API Key |
| `DEEPSEEK_API_KEY` | DeepSeek API Key |
| `OPENAI_API_KEY` | OpenAI API Key |

**优势：**
- 配置复用：多个 Agent 共享同一 LLM 配置
- 集中管理：API Key、模型参数统一配置
- Provider 抽象：7 种内置 Provider + 自定义 Provider 支持
- 自动重试：内置指数退避重试机制

### 配置和聊天

```erlang
%% 创建配置
-spec create(provider(), map()) -> config().
beamai_chat_completion:create(Provider, Opts).

%% 聊天补全
-spec chat(config(), [map()]) -> {ok, map()} | {error, term()}.
beamai_chat_completion:chat(Config, Messages).

%% 带选项的聊天补全（支持 tools, tool_choice, max_retries 等）
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:chat(Config, Messages, Opts).

%% 流式聊天
-spec stream_chat(config(), [map()], fun()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:stream_chat(Config, Messages, Callback).

-spec stream_chat(config(), [map()], fun(), map()) -> {ok, map()} | {error, term()}.
beamai_chat_completion:stream_chat(Config, Messages, Callback, Opts).
```

### Chat 选项

```erlang
Opts = #{
    tools => [tool_spec()],           %% 工具定义列表
    tool_choice => auto | none | required, %% 工具选择策略
    max_retries => integer(),         %% 重试次数（默认 3）
    retry_delay => integer(),         %% 基础重试延迟（毫秒，默认 1000）
    on_retry => fun(RetryState) -> ok %% 重试回调
}.
```

### 支持的 Provider

| Provider | 模块 | API 模式 | 特性 |
|----------|------|----------|------|
| `openai` | llm_provider_openai | OpenAI | 聊天、流式、工具调用 |
| `anthropic` | llm_provider_anthropic | Anthropic | 聊天、流式、工具调用 |
| `deepseek` | llm_provider_deepseek | OpenAI 兼容 | 聊天、流式、工具调用 |
| `zhipu` | llm_provider_zhipu | OpenAI 兼容 | 聊天、流式、工具调用、异步 |
| `bailian` | llm_provider_bailian | DashScope 原生 | 聊天、流式、工具调用、联网搜索 |
| `ollama` | llm_provider_ollama | OpenAI 兼容 | 聊天、流式 |
| `mock` | llm_provider_mock | 内置 | 测试用 Mock LLM |
| `{custom, Module}` | 自定义 | 自定义 | 用户自定义 Provider |

### Provider 公共模块 (llm_provider_common)

所有 Provider 共享的通用函数：

```erlang
%% URL 构建
llm_provider_common:build_url(Config, DefaultEndpoint, DefaultBaseUrl) -> binary().

%% Bearer 认证头构建
llm_provider_common:build_bearer_auth_headers(Config) -> [{binary(), binary()}].

%% 可选参数添加
llm_provider_common:maybe_add_stream(Body, Request) -> map().
llm_provider_common:maybe_add_tools(Body, Request) -> map().
llm_provider_common:maybe_add_top_p(Body, Request) -> map().

%% OpenAI 格式流式事件累加
llm_provider_common:accumulate_openai_event(Event, Acc) -> map().

%% 工具调用解析
llm_provider_common:parse_tool_calls(Message) -> [map()].
llm_provider_common:parse_single_tool_call(Call) -> map().

%% 使用统计解析
llm_provider_common:parse_usage(Usage) -> #{prompt_tokens, completion_tokens, total_tokens}.
```

### DeepSeek 详细说明

DeepSeek Provider 使用 OpenAI 兼容 API，支持 `deepseek-chat` 和 `deepseek-reasoner` 模型。

**支持的模型：**
- `deepseek-chat`：通用对话模型（默认）
- `deepseek-reasoner`：推理增强模型

**配置示例：**
```erlang
LLM = beamai_chat_completion:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY")),
    max_tokens => 4096
}).
```

### 阿里云百炼 (DashScope) 详细说明

百炼 Provider 使用 DashScope 原生 API，自动根据模型类型选择端点：
- **文本生成模型** (`qwen-plus`, `qwen-max`, `qwen-turbo`)：使用 `/api/v1/services/aigc/text-generation/generation`
- **多模态模型** (`qwen-vl-plus`, `qwen-audio` 等)：使用 `/api/v1/services/aigc/multimodal-generation/generation`

**特有参数：**
- `enable_search => true`：启用联网搜索功能
- `tool_choice => <<"required">>`：强制工具调用

**流式输出：**
- 请求头：`X-DashScope-SSE: enable`
- 参数：`parameters.incremental_output: true`

### LLM 配置参数

`beamai_chat_completion:create/2` 支持以下参数：

```erlang
LLM = beamai_chat_completion:create(Provider, #{
    model => binary(),                   %% 模型名称（必需）
    api_key => binary(),                 %% API Key（必需，ollama/mock 除外）
    base_url => binary(),                %% 可选：自定义 URL
    timeout => integer(),                %% 可选：超时时间（毫秒）
    max_tokens => integer(),             %% 可选：最大 token
    temperature => float()               %% 可选：温度参数 (0.0 - 2.0)
}).
```

**Provider 类型：** `openai | anthropic | deepseek | zhipu | bailian | ollama | mock | {custom, module()}`

**Config 类型标记：** 返回的 config map 包含 `'__llm_config__' => true` 标记，用于内部验证。

---

## beamai_memory - 记忆管理

统一的记忆和检查点管理系统。

### 创建和配置

```erlang
%% 创建 Memory 实例
-spec new(map()) -> {ok, memory()} | {error, term()}.
beamai_memory:new(Config).

Config = #{
    checkpointer => #{backend => ets | sqlite},
    store => #{backend => ets | sqlite},
    context_store => {module(), term()}
}.
```

### Checkpoint 操作

```erlang
%% 保存检查点
-spec save_checkpoint(memory(), config(), state_data()) -> {ok, memory()}.
beamai_memory:save_checkpoint(Memory, Config, StateData).

%% 加载检查点
-spec load_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
-spec load_latest_checkpoint(memory(), config()) -> {ok, state_data()} | {error, not_found}.
beamai_memory:load_checkpoint(Memory, Config).
beamai_memory:load_latest_checkpoint(Memory, Config).

%% 列出检查点
-spec list_checkpoints(memory(), config()) -> {ok, [checkpoint_info()]}.
beamai_memory:list_checkpoints(Memory, Config).

%% 检查点计数
-spec checkpoint_count(memory(), config()) -> non_neg_integer().
beamai_memory:checkpoint_count(Memory, Config).
```

### Store 操作

```erlang
%% 存储数据
-spec put(memory(), namespace(), key(), value()) -> {ok, memory()}.
beamai_memory:put(Memory, Namespace, Key, Value).

%% 搜索数据
-spec search(memory(), namespace(), filter()) -> {ok, [item()]}.
beamai_memory:search(Memory, Namespace, Filter).
```

---

## beamai_tools - 工具库和中间件系统

统一的工具定义和管理，以及 Agent 执行中间件系统。

beamai_tools 包含两大核心功能：
- **工具系统**：工具定义、注册、Provider 机制
- **中间件系统**：Agent 执行拦截、增强和控制（详见 [Middleware 系统](#middleware-系统)）

### 工具获取

```erlang
%% 获取工具
-spec get_tools(category() | [category()]) -> [tool_def()].
-spec get_tools(category(), map()) -> [tool_def()].
beamai_tools:get_tools(Categories).
beamai_tools:get_tools(Categories, Opts).

%% 获取所有工具
-spec get_all_tools() -> [tool_def()].
beamai_tools:get_all_tools().

%% 查找工具
-spec find_tool(binary()) -> {ok, tool_def()} | {error, not_found}.
beamai_tools:find_tool(Name).
```

### 工具执行

```erlang
%% 执行工具
-spec execute(binary(), map()) -> {ok, term()} | {error, term()}.
-spec execute(binary(), map(), map()) -> {ok, term()} | {error, term()}.
beamai_tools:execute(ToolName, Args).
beamai_tools:execute(ToolName, Args, Opts).
```

### 工具转换

```erlang
%% 转换为 LLM 格式
-spec to_llm_spec(tool_def()) -> map().
-spec to_llm_specs([tool_def()]) -> [map()].
beamai_tools:to_llm_spec(Tool).
beamai_tools:to_llm_specs(Tools).
```

### 工具注册表

```erlang
%% 构建工具列表
Registry = beamai_tool_registry:new(),
R1 = beamai_tool_registry:add_tools(Registry, Tools),
R2 = beamai_tool_registry:add_provider(R1, Provider),
Tools = beamai_tool_registry:build(R2).

%% 便捷函数
Tools = beamai_tool_registry:from_config(#{
    tools => [Tool1, Tool2],
    providers => [Provider1, Provider2]
}).
```

---

## beamai_core - 核心模块

### Kernel 和 Tool 系统

Kernel 是 BeamAI 的核心抽象，管理 Tool 注册、LLM 服务配置和 Filter 管道。

```erlang
%% 创建 Kernel
-spec new() -> kernel().
-spec new(kernel_settings()) -> kernel().
beamai_kernel:new().
beamai_kernel:new(Settings).

%% 添加 LLM 服务
-spec add_service(kernel(), beamai_chat_completion:config()) -> kernel().
beamai_kernel:add_service(Kernel, LlmConfig).

%% 添加工具
-spec add_tool(kernel(), tool_spec()) -> kernel().
-spec add_tools(kernel(), [tool_spec()]) -> kernel().
-spec add_tool_module(kernel(), module()) -> kernel().
beamai_kernel:add_tool(Kernel, ToolSpec).
beamai_kernel:add_tools(Kernel, ToolSpecs).
beamai_kernel:add_tool_module(Kernel, Module).

%% 添加过滤器
-spec add_filter(kernel(), filter_def()) -> kernel().
beamai_kernel:add_filter(Kernel, Filter).

%% 聊天（不含工具循环）
-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
beamai_kernel:invoke_chat(Kernel, Messages, Opts).

%% 聊天（含工具调用循环，自动执行工具直到获得文本回复）
-spec invoke(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
beamai_kernel:invoke(Kernel, Messages, Opts).

%% 调用单个工具
-spec invoke_tool(kernel(), binary(), map(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context).

%% 查询工具
-spec get_tool(kernel(), binary()) -> {ok, tool_spec()} | error.
-spec list_tools(kernel()) -> [tool_spec()].
-spec get_tools_by_tag(kernel(), binary()) -> [tool_spec()].
-spec get_tool_specs(kernel()) -> [map()].
-spec get_tool_schemas(kernel()) -> [map()].
-spec get_tool_schemas(kernel(), atom()) -> [map()].
beamai_kernel:get_tool(Kernel, Name).
beamai_kernel:list_tools(Kernel).
beamai_kernel:get_tools_by_tag(Kernel, Tag).
beamai_kernel:get_tool_specs(Kernel).
beamai_kernel:get_tool_schemas(Kernel).          %% 默认 OpenAI 格式
beamai_kernel:get_tool_schemas(Kernel, Provider). %% 指定 Provider 格式

%% 获取 LLM 服务配置
-spec get_service(kernel()) -> beamai_chat_completion:config() | undefined.
beamai_kernel:get_service(Kernel).
```

**Tool 定义示例：**

```erlang
%% 直接定义 Map
Tool = #{
    name => <<"get_weather">>,
    description => <<"获取城市天气"/utf8>>,
    tag => <<"weather">>,
    parameters => #{
        <<"city">> => #{type => string, required => true, description => <<"城市名称"/utf8>>}
    },
    handler => fun(#{<<"city">> := City}, _Ctx) ->
        {ok, #{city => City, temp => 25}}
    end
}.

%% 使用 beamai_tool:new
Tool = beamai_tool:new(<<"my_tool">>, fun handle/2, #{
    description => <<"工具描述"/utf8>>,
    parameters => #{...}
}).

%% 注册到 Kernel
Kernel1 = beamai_kernel:add_tool(Kernel, Tool).
```

**Tool Module（behaviour 模式）：**

```erlang
-module(my_tools).
-behaviour(beamai_tool_behaviour).
-export([tool_info/0, tools/0]).

tool_info() ->
    #{description => <<"我的工具集"/utf8>>, tags => [<<"custom">>]}.

tools() ->
    [
        #{name => <<"tool_a">>, handler => fun ?MODULE:handle_a/2, ...},
        #{name => <<"tool_b">>, handler => fun ?MODULE:handle_b/2, ...}
    ].

%% 加载到 Kernel
Kernel1 = beamai_kernel:add_tool_module(Kernel, my_tools).
```

**内置 Tool 模块：**

| 模块 | 描述 | 工具 |
|------|------|------|
| `beamai_tool_file` | 文件操作 | file_read, file_write, file_list, file_glob |
| `beamai_tool_shell` | Shell 命令 | shell_exec |
| `beamai_tool_todo` | 任务管理 | todo_add, todo_list, todo_update |
| `beamai_tool_human` | 人工交互 | human_input |

### Graph 执行引擎

```erlang
%% 构建 Graph
Graph = graph_builder:new()
    |> graph_builder:add_node(NodeName, {Module, Opts})
    |> graph_builder:add_edge(From, To, Condition)
    |> graph_builder:set_entry(EntryNode)
    |> graph_builder:build().

%% 执行 Graph
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
%% 创建状态
-spec new(map()) -> state().
graph_state:new(Data).

%% 读写状态
-spec get(state(), key()) -> value().
-spec set(state(), key(), value()) -> state().
graph_state:get(State, Key).
graph_state:set(State, Key, Value).

%% 用户上下文操作
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

字段级 Reducer，用于合并节点返回的 delta 到全局状态。

```erlang
%% 应用 delta
-spec apply_delta(state(), delta(), field_reducers()) -> state().
-spec apply_deltas(state(), [delta()], field_reducers()) -> state().
graph_state_reducer:apply_delta(State, Delta, FieldReducers).
graph_state_reducer:apply_deltas(State, Deltas, FieldReducers).

%% 内置 Reducer
graph_state_reducer:append_reducer(Old, New) -> list().
graph_state_reducer:merge_reducer(Old, New) -> map().
graph_state_reducer:increment_reducer(Old, Delta) -> number().
graph_state_reducer:last_write_win_reducer(Old, New) -> term().
```

**Reducer 配置格式：**

```erlang
FieldReducers = #{
    %% 普通 reducer
    <<"messages">> => fun graph_state_reducer:append_reducer/2,

    %% 转换型 reducer：从 counter_incr 累加到 counter
    <<"counter_incr">> => {transform, <<"counter">>, fun graph_state_reducer:increment_reducer/2}
}.
```

### Pregel 分布式计算

```erlang
%% 创建 Pregel 图
{ok, Graph} = pregel_graph:new(Config).

%% 添加顶点和边
pregel_graph:add_vertex(Graph, VertexId, Data).
pregel_graph:add_edge(Graph, From, To, Weight).

%% 运行计算
{ok, Result} = pregel:run(Graph, ComputeFn, MaxIterations).
```

### HTTP 客户端

BeamAI 提供统一的 HTTP 客户端接口，支持 Gun 和 Hackney 两种后端。

```erlang
%% 发送请求（自动使用配置的后端）
-spec request(method(), url(), headers(), body(), opts()) -> {ok, response()} | {error, term()}.
beamai_http:request(Method, Url, Headers, Body, Opts).

%% 便捷函数
-spec get(url(), headers()) -> {ok, response()} | {error, term()}.
-spec post(url(), headers(), body()) -> {ok, response()} | {error, term()}.
beamai_http:get(Url, Headers).
beamai_http:post(Url, Headers, Body).

%% 流式请求（SSE）
-spec stream_request(url(), headers(), body(), callback(), opts()) -> {ok, term()} | {error, term()}.
beamai_http:stream_request(Url, Headers, Body, Callback, Opts).
```

### HTTP 后端配置

```erlang
%% 应用配置（sys.config）
{beamai_core, [
    %% 选择 HTTP 后端：beamai_http_gun（默认）或 beamai_http_hackney
    {http_backend, beamai_http_gun},

    %% Gun 连接池配置
    {http_pool, #{
        max_connections => 100,        %% 最大连接数
        connection_timeout => 30000,   %% 连接超时（毫秒）
        idle_timeout => 60000          %% 空闲超时（毫秒）
    }}
]}.
```

**后端对比：**

| 特性 | Gun（默认） | Hackney |
|------|-------------|---------|
| HTTP/2 | 支持 | 不支持 |
| 连接池 | beamai_http_pool | hackney 内置池 |
| TLS | 自动使用系统 CA 证书（OTP 25+） | hackney 默认配置 |
| 依赖 | gun 2.1.0 | hackney |
| 推荐场景 | 生产环境、需要 HTTP/2 | 兼容旧系统 |

### HTTP 连接池 (Gun 后端)

当使用 Gun 后端时，beamai_http_pool 会作为 beamai_core 应用的子进程自动启动。

```erlang
%% 连接池 API
-spec checkout(host(), port(), protocol()) -> {ok, connection()} | {error, term()}.
beamai_http_pool:checkout(Host, Port, Protocol).

-spec checkin(connection()) -> ok.
beamai_http_pool:checkin(Conn).

%% 查看连接池状态
-spec get_stats() -> map().
beamai_http_pool:get_stats().
```

---

## beamai_a2a - A2A 协议

Agent-to-Agent 通信协议实现。

### 服务端

```erlang
%% 启动服务器
-spec start_link(map()) -> {ok, pid()}.
beamai_a2a_server:start_link(Config).

Config = #{
    handler => module(),                 %% 请求处理器
    port => integer(),                   %% HTTP 端口
    auth => auth_config()                %% 认证配置
}.
```

### 客户端

```erlang
%% 发现 Agent
-spec discover(binary()) -> {ok, agent_card()} | {error, term()}.
beamai_a2a_client:discover(AgentUrl).

%% 发送消息
-spec send_message(binary(), message()) -> {ok, response()} | {error, term()}.
beamai_a2a_client:send_message(AgentUrl, Message).
```

### Agent Card

```erlang
%% 创建 Card
-spec new(map()) -> agent_card().
beamai_a2a_card:new(#{
    name => binary(),
    description => binary(),
    url => binary(),
    capabilities => [binary()]
}).

%% 验证 Card
-spec validate(agent_card()) -> ok | {error, term()}.
beamai_a2a_card:validate(Card).
```

---

## beamai_mcp - MCP 协议

Model Context Protocol 实现。

### 客户端

```erlang
%% 连接 MCP 服务器
-spec connect(config()) -> {ok, client()} | {error, term()}.
beamai_mcp_client:connect(Config).

Config = #{
    transport => stdio | http | sse,
    command => binary(),                 %% stdio: 命令
    url => binary()                      %% http/sse: URL
}.

%% 列出工具
-spec list_tools(client()) -> {ok, [tool()]} | {error, term()}.
beamai_mcp_client:list_tools(Client).

%% 调用工具
-spec call_tool(client(), binary(), map()) -> {ok, result()} | {error, term()}.
beamai_mcp_client:call_tool(Client, ToolName, Args).

%% 列出资源
-spec list_resources(client()) -> {ok, [resource()]} | {error, term()}.
beamai_mcp_client:list_resources(Client).
```

### 服务端

```erlang
%% 启动服务器
-spec start_link(config()) -> {ok, pid()}.
beamai_mcp_server:start_link(Config).

%% 注册工具
-spec register_tool(pid(), tool_def()) -> ok.
beamai_mcp_server:register_tool(Server, Tool).

%% 注册资源
-spec register_resource(pid(), resource_def()) -> ok.
beamai_mcp_server:register_resource(Server, Resource).
```

---

## beamai_rag - RAG 功能

检索增强生成模块。

### 向量嵌入

```erlang
%% 生成嵌入
-spec embed(binary(), config()) -> {ok, [float()]} | {error, term()}.
-spec embed_batch([binary()], config()) -> {ok, [[float()]]} | {error, term()}.
beamai_embeddings:embed(Text, Config).
beamai_embeddings:embed_batch(Texts, Config).
```

### 向量存储

```erlang
%% 创建存储
-spec new(config()) -> {ok, store()}.
beamai_vector_store:new(Config).

%% 添加向量
-spec add(store(), binary(), [float()], map()) -> {ok, store()}.
beamai_vector_store:add(Store, Id, Vector, Metadata).

%% 相似度搜索
-spec search(store(), [float()], integer()) -> {ok, [result()]}.
beamai_vector_store:search(Store, QueryVector, TopK).
```

### RAG 流程

```erlang
%% 初始化
-spec init(config()) -> {ok, rag_state()}.
beamai_rag:init(Config).

%% 添加文档
-spec add_documents(rag_state(), [document()]) -> {ok, rag_state()}.
beamai_rag:add_documents(State, Documents).

%% 检索
-spec retrieve(rag_state(), binary(), integer()) -> {ok, [chunk()]}.
beamai_rag:retrieve(State, Query, TopK).

%% RAG 查询（检索 + 生成）
-spec query(rag_state(), binary()) -> {ok, response()}.
beamai_rag:query(State, Question).
```

---

## 通用类型

### 工具定义（tool_spec）

```erlang
-type tool_spec() :: #{
    name := binary(),                    % 必填：工具名称
    handler := handler(),                % 必填：处理器
    description => binary(),             % 可选：描述（供 LLM 理解）
    parameters => parameters_schema(),   % 可选：参数定义
    tag => binary() | [binary()],        % 可选：分类标签
    timeout => pos_integer(),            % 可选：超时时间（毫秒）
    retry => #{max => integer(), delay => integer()},  % 可选：重试策略
    metadata => map()                    % 可选：自定义元数据
}.

-type handler() ::
    fun((args()) -> tool_result())                        % fun/1：仅接收参数
    | fun((args(), beamai_context:t()) -> tool_result())  % fun/2：参数 + 上下文
    | {module(), atom()}                                  % {M, F}：模块函数
    | {module(), atom(), [term()]}.                       % {M, F, ExtraArgs}

-type tool_result() ::
    {ok, term()}                          % 成功，返回结果
    | {ok, term(), beamai_context:t()}    % 成功，返回结果和更新的上下文
    | {error, term()}.                    % 失败

-type parameters_schema() :: #{
    atom() | binary() => #{
        type := string | integer | float | boolean | array | object,
        description => binary(),
        required => boolean(),
        default => term(),
        enum => [term()],
        items => param_spec(),           % array 元素类型
        properties => parameters_schema() % object 嵌套属性
    }
}.
```

### 消息类型

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary()
}.
```

### LLM 响应

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

## 错误处理

所有 API 返回 `{ok, Result}` 或 `{error, Reason}` 格式。常见错误类型：

| 错误 | 说明 |
|------|------|
| `{error, missing_api_key}` | API Key 未配置 |
| `{error, timeout}` | 请求超时 |
| `{error, {http_error, Code, Body}}` | HTTP 错误 |
| `{error, {api_error, Details}}` | API 返回错误 |
| `{error, not_found}` | 资源未找到 |
| `{error, storage_not_enabled}` | 存储未启用 |

---

## 更多文档

- [README.md](../README.md) - 项目概述
- [ARCHITECTURE.md](ARCHITECTURE.md) - 架构设计
- 各模块 README：
  - [beamai_core](../apps/beamai_core/README.md)
  - [beamai_llm](../apps/beamai_llm/README.md)
  - [beamai_agent](../apps/beamai_agent/README.md)
  - [beamai_deepagent](../apps/beamai_deepagent/README.md)
  - [beamai_memory](../apps/beamai_memory/README.md)
  - [beamai_tools](../apps/beamai_tools/README.md)
  - [beamai_a2a](../apps/beamai_a2a/README.md)
  - [beamai_mcp](../apps/beamai_mcp/README.md)
  - [beamai_rag](../apps/beamai_rag/README.md)
