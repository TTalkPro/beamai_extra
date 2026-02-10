%%%-------------------------------------------------------------------
%%% @doc 使用 Anthropic API 测试 Simple Agent
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY="your-zhipu-api-key"
%%% export ZHIPU_ANTHROPIC_BASE_URL="https://open.bigmodel.cn/api/anthropic"
%%% rebar3 shell
%%% ```
%%%
%%% 然后在 shell 中执行:
%%% ```erlang
%%% test_anthropic_agent:run_simple().
%%% test_anthropic_agent:run_with_tools().
%%% test_anthropic_agent:run_multi_turn().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_anthropic_agent).

-export([
    run_simple/0,
    run_with_tools/0,
    run_multi_turn/0,
    get_llm_config/0
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 获取 LLM 配置（从环境变量）
-spec get_llm_config() -> map().
get_llm_config() ->
    ApiKey = case os:getenv("ZHIPU_API_KEY") of
        false -> error({missing_env, "ZHIPU_API_KEY"});
        "" -> error({missing_env, "ZHIPU_API_KEY"});
        Key -> list_to_binary(Key)
    end,

    BaseUrl = case os:getenv("ZHIPU_ANTHROPIC_BASE_URL") of
        false -> <<"https://open.bigmodel.cn/api/anthropic">>;
        "" -> <<"https://open.bigmodel.cn/api/anthropic">>;
        Url -> list_to_binary(Url)
    end,

    #{
        provider => anthropic,
        api_key => ApiKey,
        base_url => BaseUrl,
        model => <<"glm-4.7">>,
        max_tokens => 2048
    }.

%% @doc 运行简单的对话测试
-spec run_simple() -> {ok, map(), map()} | {error, term()}.
run_simple() ->
    io:format("=== Testing Simple Agent with Anthropic ===~n~n"),

    %% 获取 LLM 配置
    LLMConfig = get_llm_config(),
    io:format("LLM Config:~n"),
    io:format("  Provider: ~p~n", [maps:get(provider, LLMConfig)]),
    io:format("  Base URL: ~s~n", [maps:get(base_url, LLMConfig)]),
    io:format("  Model: ~s~n~n", [maps:get(model, LLMConfig)]),

    %% 创建 LLM（使用 beamai_chat_completion）
    LLM = beamai_chat_completion:create(
        maps:get(provider, LLMConfig),
        maps:without([provider], LLMConfig)
    ),

    %% 创建 Agent
    {ok, Agent} = beamai_agent:new(#{
        llm => LLM,
        system_prompt => <<"You are a helpful assistant. Please respond in concise English.">>
    }),

    io:format("Agent created successfully!~n"),
    io:format("Agent ID: ~s~n~n", [beamai_agent:id(Agent)]),

    %% 第一轮对话
    io:format("--- Turn 1 ---~n"),
    UserMessage1 = <<"Hello! Please introduce yourself briefly.">>,
    io:format("User: ~s~n~n", [UserMessage1]),

    case beamai_agent:run(Agent, UserMessage1) of
        {ok, Result1, Agent1} ->
            Content1 = maps:get(content, Result1),
            io:format("Assistant: ~s~n~n", [Content1]),

            %% 第二轮对话
            io:format("--- Turn 2 ---~n"),
            UserMessage2 = <<"What's Erlang/OTP known for?">>,
            io:format("User: ~s~n~n", [UserMessage2]),

            case beamai_agent:run(Agent1, UserMessage2) of
                {ok, Result2, _Agent2} ->
                    Content2 = maps:get(content, Result2),
                    io:format("Assistant: ~s~n~n", [Content2]),
                    io:format("=== Test completed successfully! ===~n"),
                    {ok, Result1, Result2};
                {error, Reason2} ->
                    io:format("Error on turn 2: ~p~n", [Reason2]),
                    {error, Reason2}
            end;
        {error, Reason1} ->
            io:format("Error on turn 1: ~p~n", [Reason1]),
            {error, Reason1}
    end.

%% @doc 运行带工具的 Agent 测试
-spec run_with_tools() -> {ok, map(), map()} | {error, term()}.
run_with_tools() ->
    io:format("=== Testing Agent with Tools ===~n~n"),

    LLM = beamai_chat_completion:create(
        anthropic,
        maps:without([provider], get_llm_config())
    ),

    %% 创建带工具的 Agent
    {ok, Agent} = beamai_agent:new(#{
        llm => LLM,
        system_prompt => <<"You are a helpful assistant with access to tools. Use them when needed.">>,
        tools => [
            #{
                name => <<"get_current_time">>,
                description => <<"Get the current UTC time">>,
                parameters => #{
                    type => <<"object">>,
                    properties => #{},
                    required => []
                },
                handler => fun(_Args, _Ctx) ->
                    {ok, #{
                        result => list_to_binary(
                            calendar:system_time_to_rfc3339(
                                erlang:system_time(second)
                            )
                        )
                    }}
                end
            },
            #{
                name => <<"calculate">>,
                description => <<"Perform a simple calculation. Supports add, subtract, multiply, divide.">>,
                parameters => #{
                    type => <<"object">>,
                    properties => #{
                        <<"operation">> => #{
                            type => <<"string">>,
                            description => <<"The operation to perform: add, subtract, multiply, divide">>,
                            enum => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                        },
                        <<"a">> => #{
                            type => <<"number">>,
                            description => <<"First number">>
                        },
                        <<"b">> => #{
                            type => <<"number">>,
                            description => <<"Second number">>
                        }
                    },
                    required => [<<"operation">>, <<"a">>, <<"b">>]
                },
                handler => fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}, _Ctx) ->
                    try
                        Result = case Op of
                            <<"add">> -> A + B;
                            <<"subtract">> -> A - B;
                            <<"multiply">> -> A * B;
                            <<"divide">> when B /= 0 -> A / B;
                            <<"divide">> -> error(divide_by_zero)
                        end,
                        {ok, #{result => Result}}
                    catch
                        _:_:E -> {error, E}
                    end
                end
            }
        ]
    }),

    io:format("Agent created with 2 tools~n~n"),

    UserMessage = <<"What's 25 multiplied by 4? Also, what's the current time?">>,
    io:format("User: ~s~n~n", [UserMessage]),

    case beamai_agent:run(Agent, UserMessage) of
        {ok, Result, _AgentFinal} ->
            Content = maps:get(content, Result),
            io:format("Assistant: ~s~n~n", [Content]),

            case maps:find(tool_calls_made, Result) of
                {ok, ToolCalls} when length(ToolCalls) > 0 ->
                    io:format("Tools called: ~p~n", [ToolCalls]);
                _ ->
                    ok
            end,

            io:format("=== Test completed successfully! ===~n"),
            {ok, Result};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 运行多轮对话测试
-spec run_multi_turn() -> ok | {error, term()}.
run_multi_turn() ->
    io:format("=== Testing Multi-Turn Conversation ===~n~n"),

    LLM = beamai_chat_completion:create(
        anthropic,
        maps:without([provider], get_llm_config())
    ),

    {ok, Agent} = beamai_agent:new(#{
        llm => LLM,
        system_prompt => <<"You are a memory assistant. Remember information from previous turns.">>
    }),

    io:format("Agent created~n~n"),

    %% 第一轮：记住信息
    io:format("--- Turn 1: Storing information ---~n"),
    Msg1 = <<"My name is Alice and I love Erlang programming.">>,
    io:format("User: ~s~n~n", [Msg1]),

    case beamai_agent:run(Agent, Msg1) of
        {ok, _Result1, Agent1} ->
            io:format("Assistant: Got it!~n~n"),

            %% 第二轮：测试记忆
            io:format("--- Turn 2: Testing memory ---~n"),
            Msg2 = <<"What's my name and what programming language do I love?">>,
            io:format("User: ~s~n~n", [Msg2]),

            case beamai_agent:run(Agent1, Msg2) of
                {ok, Result2, _Agent2} ->
                    Content2 = maps:get(content, Result2),
                    io:format("Assistant: ~s~n~n", [Content2]),

                    %% 查看对话历史
                    Messages = beamai_agent:messages(_Agent2),
                    io:format("Total messages in history: ~B~n", [length(Messages)]),

                    io:format("=== Test completed successfully! ===~n"),
                    ok;
                {error, Reason2} ->
                    io:format("Error on turn 2: ~p~n", [Reason2]),
                    {error, Reason2}
            end;
        {error, Reason1} ->
            io:format("Error on turn 1: ~p~n", [Reason1]),
            {error, Reason1}
    end.
