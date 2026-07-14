%%%-------------------------------------------------------------------
%%% @doc Agent 两种典型场景示例
%%%
%%% 按 Plugin 维度演示 beamai_agent 用法：
%%%
%%%   1. 无 Plugin — 最简对话
%%%   2. 有 Plugin — 带工具调用的对话
%%%
%%% 使用方法：
%%% ```
%%% cd examples
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%%
%%% %% Mock 版本（无需 API Key）
%%% example_agent_scenarios:no_memory_no_plugin().
%%% example_agent_scenarios:no_memory_with_plugin().
%%%
%%% %% 真实 LLM 版本（需设置 API Key）
%%% example_agent_scenarios:no_memory_no_plugin_live().
%%% example_agent_scenarios:no_memory_with_plugin_live().
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_scenarios).

-export([
    %% Mock 版本
    no_memory_no_plugin/0,
    no_memory_with_plugin/0,
    %% Live 版本
    no_memory_no_plugin_live/0,
    no_memory_with_plugin_live/0
]).

%%====================================================================
%% 场景 1：无 Plugin
%%
%% 最简用法：创建 agent → 对话 → 获取结果
%% 状态仅在内存中，进程结束后丢失
%%====================================================================

%% @doc 无 Plugin（Mock）
-spec no_memory_no_plugin() -> ok.
no_memory_no_plugin() ->
    io:format("~n=== 场景 1: 无 Plugin (Mock) ===~n~n"),

    %% 创建 agent：只需指定 LLM
    {ok, Agent0} = beamai_agent:new(#{
        llm => {mock, #{}},
        system_prompt => <<"你是一个友好的助手，请用简短的话回答。"/utf8>>
    }),

    %% 第一轮对话
    io:format("User: 你好，我是小明~n"),
    {ok, R1, Agent1} = beamai_agent:run(Agent0, <<"你好，我是小明"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R1)]),
    io:format("  turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent1), length(beamai_agent:messages(Agent1))]),

    %% 第二轮对话（Agent1 已包含历史消息）
    io:format("User: 我叫什么名字？~n"),
    {ok, R2, Agent2} = beamai_agent:run(Agent1, <<"我叫什么名字？"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R2)]),
    io:format("  turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent2), length(beamai_agent:messages(Agent2))]),

    %% 第三轮
    io:format("User: 总结一下我们的对话~n"),
    {ok, R3, Agent3} = beamai_agent:run(Agent2, <<"总结一下我们的对话"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R3)]),
    io:format("  turn_count=~p, messages=~p~n",
              [beamai_agent:turn_count(Agent3), length(beamai_agent:messages(Agent3))]),

    io:format("~n注意：进程结束后所有对话状态丢失~n"),
    ok.

%% @doc 无 Plugin（Live LLM）
-spec no_memory_no_plugin_live() -> ok.
no_memory_no_plugin_live() ->
    io:format("~n=== 场景 1: 无 Plugin (Live) ===~n~n"),

    {ok, Agent0} = beamai_agent:new(#{
        llm => example_llm_config:anthropic(),
        system_prompt => <<"你是一个友好的助手，请用简短的一两句话回答。"/utf8>>
    }),

    Turns = [
        <<"你好，我叫小明，我是一个 Erlang 程序员"/utf8>>,
        <<"请问我刚才说我做什么工作？"/utf8>>,
        <<"给我推荐一本适合我的书"/utf8>>
    ],

    lists:foldl(fun(Msg, Acc) ->
        io:format("User: ~ts~n", [Msg]),
        case beamai_agent:run(Acc, Msg) of
            {ok, Result, NewAgent} ->
                io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
                NewAgent;
            {error, Reason} ->
                io:format("Error: ~p~n~n", [Reason]),
                Acc
        end
    end, Agent0, Turns),
    ok.

%%====================================================================
%% 场景 2：有 Plugin
%%
%% Agent 可以调用工具完成任务
%% 适合一次性任务（如查询天气、执行计算等）
%%====================================================================

%% @doc 有 Plugin（Mock）
-spec no_memory_with_plugin() -> ok.
no_memory_with_plugin() ->
    io:format("~n=== 场景 2: 有 Plugin (Mock) ===~n~n"),

    %% 方式一：通过 kernel 注册工具
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),

    %% 注册计算器工具
    AddTool = beamai_tool:new(<<"add">>,
        fun(Args) ->
            A = maps:get(<<"a">>, Args, 0),
            B = maps:get(<<"b">>, Args, 0),
            io:format("  [calculator] ~p + ~p = ~p~n", [A, B, A + B]),
            {ok, #{result => A + B}}
        end,
        #{description => <<"Add two numbers">>,
          parameters => #{
              <<"a">> => #{type => number, description => <<"First number">>, required => true},
              <<"b">> => #{type => number, description => <<"Second number">>, required => true}
          }}),
    MultiplyTool = beamai_tool:new(<<"multiply">>,
        fun(Args) ->
            A = maps:get(<<"a">>, Args, 0),
            B = maps:get(<<"b">>, Args, 0),
            io:format("  [calculator] ~p * ~p = ~p~n", [A, B, A * B]),
            {ok, #{result => A * B}}
        end,
        #{description => <<"Multiply two numbers">>,
          parameters => #{
              <<"a">> => #{type => number, description => <<"First number">>, required => true},
              <<"b">> => #{type => number, description => <<"Second number">>, required => true}
          }}),
    K2 = beamai_kernel:add_tools(K1, [AddTool, MultiplyTool]),

    %% 创建 agent（传入预构建的 kernel）
    {ok, Agent} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a calculator assistant. Use tools to compute.">>,
        callbacks => #{
            on_tool_call => fun(Name, Args) ->
                io:format("  [callback] tool invoked: ~s~n", [Name]),
                io:format("  [callback] args: ~p~n", [Args])
            end
        }
    }),

    %% 查看可用工具
    Tools = beamai_kernel:get_tool_specs(beamai_agent:kernel(Agent)),
    io:format("Available tools: ~p~n~n", [[maps:get(name, T) || T <- Tools]]),

    %% 执行对话（mock LLM 不会触发 tool call，仅展示配置方式）
    io:format("User: What is 42 + 58?~n"),
    {ok, Result, _} = beamai_agent:run(Agent, <<"What is 42 + 58?">>),
    io:format("Assistant: ~ts~n", [maps:get(content, Result)]),
    io:format("Tool calls: ~p~n~n", [maps:get(tool_calls_made, Result, [])]),

    io:format("注意：Mock LLM 不会触发 tool call，使用 live 版本可看到完整效果~n"),
    ok.

%% @doc 有 Plugin（Live LLM）
-spec no_memory_with_plugin_live() -> ok.
no_memory_with_plugin_live() ->
    io:format("~n=== 场景 2: 有 Plugin (Live) ===~n~n"),

    %% 构建带天气和计算工具的 kernel
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, example_llm_config:anthropic()),
    WeatherTool = beamai_tool:new(<<"get_weather">>,
        fun(Args) ->
            City = maps:get(<<"city">>, Args, <<"unknown">>),
            io:format("  [tool] get_weather(~ts)~n", [City]),
            {ok, mock_weather(City)}
        end,
        #{description => <<"Get current weather for a city">>,
          parameters => #{
              <<"city">> => #{type => string, description => <<"City name">>, required => true}
          }}),
    CalcTool = beamai_tool:new(<<"calculate">>,
        fun(Args) ->
            Expr = maps:get(<<"expression">>, Args, <<"0">>),
            io:format("  [tool] calculate(~s)~n", [Expr]),
            {ok, #{expression => Expr, result => <<"42">>}}
        end,
        #{description => <<"Evaluate a math expression">>,
          parameters => #{
              <<"expression">> => #{type => string, description => <<"Math expression">>, required => true}
          }}),
    K2 = beamai_kernel:add_tools(K1, [WeatherTool, CalcTool]),

    {ok, Agent} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a helpful assistant with weather and math tools. "
                          "Always use tools when appropriate. Be concise.">>
    }),

    %% 第一轮：触发天气工具
    Q1 = <<"What's the weather in Tokyo?">>,
    io:format("User: ~s~n", [Q1]),
    case beamai_agent:run(Agent, Q1) of
        {ok, R1, Agent1} ->
            io:format("Assistant: ~ts~n", [maps:get(content, R1)]),
            io:format("  tool_calls: ~p, iterations: ~p~n~n",
                      [length(maps:get(tool_calls_made, R1, [])),
                       maps:get(iterations, R1, 0)]),

            %% 第二轮：触发计算工具
            Q2 = <<"If the temperature is 22°C, what is it in Fahrenheit? Use the calculate tool.">>,
            io:format("User: ~s~n", [Q2]),
            case beamai_agent:run(Agent1, Q2) of
                {ok, R2, _} ->
                    io:format("Assistant: ~ts~n", [maps:get(content, R2)]),
                    io:format("  tool_calls: ~p~n",
                              [length(maps:get(tool_calls_made, R2, []))]);
                {error, Reason2} ->
                    io:format("Error: ~p~n", [Reason2])
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 模拟天气数据
mock_weather(<<"Beijing">>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"Tokyo">>) ->
    #{city => <<"Tokyo">>, temperature => 22, condition => <<"Rainy">>, humidity => 80};
mock_weather(<<"Shanghai">>) ->
    #{city => <<"Shanghai">>, temperature => 32, condition => <<"Cloudy">>, humidity => 75};
mock_weather(<<"New York">>) ->
    #{city => <<"New York">>, temperature => 18, condition => <<"Partly Cloudy">>, humidity => 60};
mock_weather(City) ->
    #{city => City, temperature => 20, condition => <<"Clear">>, humidity => 50}.
