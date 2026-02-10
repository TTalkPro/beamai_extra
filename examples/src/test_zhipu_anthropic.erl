%%%-------------------------------------------------------------------
%%% @doc 使用 ZHIPU_ANTHROPIC_BASE_URL + ZHIPU_API_KEY 测试 Agent 和 DeepAgent
%%%
%%% 环境变量:
%%%   ZHIPU_ANTHROPIC_BASE_URL - Zhipu Anthropic 兼容 API 地址
%%%                              (默认: https://open.bigmodel.cn/api/anthropic)
%%%   ZHIPU_API_KEY            - Zhipu API Key
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY="your-api-key"
%%% export ZHIPU_ANTHROPIC_BASE_URL="https://open.bigmodel.cn/api/anthropic"
%%% rebar3 shell
%%% ```
%%%
%%% 然后在 shell 中执行:
%%% ```erlang
%%% test_zhipu_anthropic:run_all().
%%% test_zhipu_anthropic:test_agent_simple().
%%% test_zhipu_anthropic:test_agent_multi_turn().
%%% test_zhipu_anthropic:test_agent_with_tools().
%%% test_zhipu_anthropic:test_deepagent_simple().
%%% test_zhipu_anthropic:test_deepagent_planned().
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(test_zhipu_anthropic).

-export([
    run_all/0,
    test_agent_simple/0,
    test_agent_multi_turn/0,
    test_agent_with_tools/0,
    test_deepagent_simple/0,
    test_deepagent_planned/0,
    create_llm/0
]).

-define(DEFAULT_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(DEFAULT_MODEL, <<"glm-4.7">>).
-define(DEFAULT_MAX_TOKENS, 2048).

%%====================================================================
%% LLM Provider 构建
%%====================================================================

%% @doc 从环境变量构建 Anthropic LLM provider
%%
%% 使用 ZHIPU_ANTHROPIC_BASE_URL 和 ZHIPU_API_KEY 环境变量。
-spec create_llm() -> beamai_chat_completion:config().
create_llm() ->
    ApiKey = case os:getenv("ZHIPU_API_KEY") of
        false -> error({missing_env, "ZHIPU_API_KEY"});
        "" -> error({missing_env, "ZHIPU_API_KEY"});
        Key -> list_to_binary(Key)
    end,
    BaseUrl = case os:getenv("ZHIPU_ANTHROPIC_BASE_URL") of
        false -> ?DEFAULT_BASE_URL;
        "" -> ?DEFAULT_BASE_URL;
        Url -> list_to_binary(Url)
    end,
    io:format("[config] Provider: anthropic~n"),
    io:format("[config] Base URL: ~s~n", [BaseUrl]),
    io:format("[config] Model: ~s~n", [?DEFAULT_MODEL]),
    beamai_chat_completion:create(anthropic, #{
        api_key => ApiKey,
        base_url => BaseUrl,
        model => ?DEFAULT_MODEL,
        max_tokens => ?DEFAULT_MAX_TOKENS
    }).

%%====================================================================
%% 运行全部测试
%%====================================================================

%% @doc 运行全部测试
-spec run_all() -> ok.
run_all() ->
    io:format("~n========================================~n"),
    io:format(" Zhipu Anthropic Provider - Full Test~n"),
    io:format("========================================~n~n"),

    ensure_apps(),

    Tests = [
        {"Agent: Simple Conversation", fun test_agent_simple/0},
        {"Agent: Multi-Turn", fun test_agent_multi_turn/0},
        {"Agent: Tool Calling", fun test_agent_with_tools/0},
        {"DeepAgent: Simple (No Planning)", fun test_deepagent_simple/0},
        {"DeepAgent: Planned Execution", fun test_deepagent_planned/0}
    ],

    Results = lists:map(fun({Name, TestFun}) ->
        io:format("~n--- ~s ---~n", [Name]),
        try
            TestFun(),
            io:format("[PASS] ~s~n", [Name]),
            {Name, pass}
        catch
            Class:Reason:Stack ->
                io:format("[FAIL] ~s~n", [Name]),
                io:format("  ~p:~p~n", [Class, Reason]),
                io:format("  ~p~n", [hd(Stack)]),
                {Name, {fail, {Class, Reason}}}
        end
    end, Tests),

    io:format("~n========================================~n"),
    io:format(" Test Summary~n"),
    io:format("========================================~n"),
    Passed = length([N || {N, pass} <- Results]),
    Failed = length([N || {N, {fail, _}} <- Results]),
    lists:foreach(fun
        ({Name, pass}) ->
            io:format("  [PASS] ~s~n", [Name]);
        ({Name, {fail, _}}) ->
            io:format("  [FAIL] ~s~n", [Name])
    end, Results),
    io:format("~nTotal: ~B passed, ~B failed, ~B total~n",
              [Passed, Failed, Passed + Failed]),
    case Failed of
        0 -> io:format("~nAll tests passed!~n");
        _ -> io:format("~nSome tests failed.~n")
    end,
    ok.

%%====================================================================
%% Agent 测试
%%====================================================================

%% @doc 测试 Agent 简单对话
-spec test_agent_simple() -> ok.
test_agent_simple() ->
    ensure_apps(),
    LLM = create_llm(),

    {ok, Agent} = beamai_agent:new(#{
        llm => LLM,
        system_prompt => <<"You are a helpful assistant. Respond concisely in one sentence.">>
    }),
    io:format("Agent created: ~s~n", [beamai_agent:id(Agent)]),

    UserMsg = <<"What is Erlang/OTP?">>,
    io:format("User: ~s~n", [UserMsg]),

    {ok, Result, Agent1} = beamai_agent:run(Agent, UserMsg),
    Content = maps:get(content, Result),
    io:format("Assistant: ~ts~n", [Content]),

    %% 验证
    true = byte_size(Content) > 0,
    1 = beamai_agent:turn_count(Agent1),
    2 = length(beamai_agent:messages(Agent1)),
    io:format("Turn count: ~p, Messages: ~p~n",
              [beamai_agent:turn_count(Agent1), length(beamai_agent:messages(Agent1))]),
    ok.

%% @doc 测试 Agent 多轮对话（验证上下文记忆）
-spec test_agent_multi_turn() -> ok.
test_agent_multi_turn() ->
    ensure_apps(),
    LLM = create_llm(),

    {ok, Agent0} = beamai_agent:new(#{
        llm => LLM,
        system_prompt => <<"You are a helpful assistant. Remember information from previous turns. Be concise.">>
    }),

    %% Turn 1: 提供信息
    Msg1 = <<"My favorite programming language is Erlang and my name is BeamAI.">>,
    io:format("User [Turn 1]: ~s~n", [Msg1]),
    {ok, _R1, Agent1} = beamai_agent:run(Agent0, Msg1),
    io:format("Assistant [Turn 1]: ~ts~n", [maps:get(content, _R1)]),

    %% Turn 2: 测试记忆
    Msg2 = <<"What is my name and favorite language?">>,
    io:format("User [Turn 2]: ~s~n", [Msg2]),
    {ok, R2, Agent2} = beamai_agent:run(Agent1, Msg2),
    Content2 = maps:get(content, R2),
    io:format("Assistant [Turn 2]: ~ts~n", [Content2]),

    %% 验证
    true = byte_size(Content2) > 0,
    2 = beamai_agent:turn_count(Agent2),
    4 = length(beamai_agent:messages(Agent2)),

    %% Turn 3: 继续对话
    Msg3 = <<"What did we talk about so far?">>,
    io:format("User [Turn 3]: ~s~n", [Msg3]),
    {ok, R3, Agent3} = beamai_agent:run(Agent2, Msg3),
    io:format("Assistant [Turn 3]: ~ts~n", [maps:get(content, R3)]),

    3 = beamai_agent:turn_count(Agent3),
    6 = length(beamai_agent:messages(Agent3)),
    io:format("Final turn count: ~p, Messages: ~p~n",
              [beamai_agent:turn_count(Agent3), length(beamai_agent:messages(Agent3))]),
    ok.

%% @doc 测试 Agent 工具调用
-spec test_agent_with_tools() -> ok.
test_agent_with_tools() ->
    ensure_apps(),
    LLM = create_llm(),

    %% 构建带工具的 Kernel
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, LLM),
    K2 = beamai_kernel:add_tools(K1, [
        #{name => <<"calculate">>,
          description => <<"Perform arithmetic. Supports: add, subtract, multiply, divide.">>,
          parameters => #{
              <<"operation">> => #{
                  type => string,
                  description => <<"Operation: add, subtract, multiply, divide">>,
                  enum => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>],
                  required => true
              },
              <<"a">> => #{type => number, description => <<"First number">>, required => true},
              <<"b">> => #{type => number, description => <<"Second number">>, required => true}
          },
          handler => fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}, _Ctx) ->
              Result = case Op of
                  <<"add">> -> A + B;
                  <<"subtract">> -> A - B;
                  <<"multiply">> -> A * B;
                  <<"divide">> when B /= 0 -> A / B;
                  <<"divide">> -> <<"Error: division by zero">>
              end,
              io:format("  [tool] calculate: ~s(~p, ~p) = ~p~n", [Op, A, B, Result]),
              {ok, #{result => Result}}
          end},
        #{name => <<"get_greeting">>,
          description => <<"Get a greeting message for a person.">>,
          parameters => #{
              <<"name">> => #{type => string, description => <<"Person's name">>, required => true}
          },
          handler => fun(#{<<"name">> := Name}, _Ctx) ->
              Greeting = <<"Hello, ", Name/binary, "! Welcome to BeamAI.">>,
              io:format("  [tool] get_greeting: ~s~n", [Name]),
              {ok, #{greeting => Greeting}}
          end}
    ]),

    ToolCallCount = counters:new(1, []),
    {ok, Agent} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a helpful assistant with tools. Use the calculate tool for math questions. Be concise.">>,
        max_tool_iterations => 5,
        callbacks => #{
            on_tool_call => fun(Name, _Args) ->
                counters:add(ToolCallCount, 1, 1),
                io:format("  [callback] tool_call: ~s~n", [Name])
            end
        }
    }),

    %% 测试工具调用
    UserMsg = <<"What is 17 multiplied by 23?">>,
    io:format("User: ~s~n", [UserMsg]),
    {ok, Result, _Agent1} = beamai_agent:run(Agent, UserMsg),
    Content = maps:get(content, Result),
    io:format("Assistant: ~ts~n", [Content]),

    ToolCalls = maps:get(tool_calls_made, Result, []),
    io:format("Tool calls made: ~p~n", [length(ToolCalls)]),
    true = counters:get(ToolCallCount, 1) >= 1,
    true = byte_size(Content) > 0,
    ok.

%%====================================================================
%% DeepAgent 测试
%%====================================================================

%% @doc 测试 DeepAgent 简单任务（无规划）
-spec test_deepagent_simple() -> ok.
test_deepagent_simple() ->
    ensure_apps(),
    LLM = create_llm(),

    Config = beamai_deepagent:new(#{
        llm => LLM,
        planning_enabled => false,
        reflection_enabled => false,
        max_tool_iterations => 1,
        timeout => 30000
    }),

    Task = <<"What is 2+2? Answer with just the number.">>,
    io:format("Task: ~s~n", [Task]),
    {ok, Result} = beamai_deepagent:run(Config, Task),

    Status = maps:get(status, Result),
    Response = maps:get(response, Result),
    io:format("Status: ~p~n", [Status]),
    io:format("Response: ~ts~n", [Response]),

    completed = Status,
    true = byte_size(Response) > 0,
    ok.

%% @doc 测试 DeepAgent 规划执行
-spec test_deepagent_planned() -> ok.
test_deepagent_planned() ->
    ensure_apps(),
    LLM = create_llm(),

    Config = beamai_deepagent:new(#{
        llm => LLM,
        planning_enabled => true,
        reflection_enabled => false,
        max_tool_iterations => 3,
        max_parallel => 2,
        timeout => 60000
    }),

    Task = <<"Write a haiku about Erlang, then explain what makes it a haiku.">>,
    io:format("Task: ~s~n", [Task]),
    {ok, Result} = beamai_deepagent:run(Config, Task),

    Status = maps:get(status, Result),
    Response = maps:get(response, Result),
    io:format("Status: ~p~n", [Status]),
    io:format("Response: ~ts~n", [Response]),

    true = (Status =:= completed orelse Status =:= error),
    true = byte_size(Response) > 0,

    %% 检查计划
    case beamai_deepagent:get_plan(Result) of
        undefined ->
            io:format("No plan generated (direct execution)~n");
        Plan ->
            Steps = beamai_deepagent_plan:get_steps(Plan),
            io:format("Plan steps: ~p~n", [length(Steps)]),
            true = length(Steps) >= 1
    end,

    %% 检查步骤结果
    StepResults = maps:get(step_results, Result, []),
    io:format("Step results: ~p~n", [length(StepResults)]),
    lists:foreach(fun(SR) ->
        io:format("  Step ~p [~p]: ~ts~n", [
            maps:get(step_id, SR, 0),
            maps:get(status, SR, unknown),
            truncate(maps:get(result, SR, <<>>), 100)
        ])
    end, StepResults),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

ensure_apps() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    application:ensure_all_started(gun),
    application:ensure_all_started(beamai_core).

truncate(Bin, MaxLen) when byte_size(Bin) > MaxLen ->
    <<(binary:part(Bin, 0, MaxLen))/binary, "...">>;
truncate(Bin, _) ->
    Bin.
