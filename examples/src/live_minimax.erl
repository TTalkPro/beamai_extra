%%%-------------------------------------------------------------------
%%% @doc MiniMax live 测试 —— 端到端验证 jsx→json 迁移 + gun 后端 + 连接池。
%%%
%%% 真连 MiniMax（anthropic 兼容），跑三段：
%%%   1. 原始 chat completion（验证请求 json 编码 / 响应 json 解码 + gun HTTP）
%%%   2. agent 单轮（验证 agent 循环 + 消息 json）
%%%   3. agent + 工具（验证工具 schema json 编码 + 工具调用循环）
%%%
%%% 用法：MINIMAX_API_KEY=... erl ... -eval 'live_minimax:run()'
%%% @end
%%%-------------------------------------------------------------------
-module(live_minimax).

-export([run/0]).

run() ->
    {ok, _} = application:ensure_all_started(beamai_core),
    io:format("~n========== MiniMax Live Test ==========~n"),
    Cfg = example_llm_config:minimax(),
    R1 = t_chat(Cfg),
    R2 = t_agent(Cfg),
    R3 = t_agent_tool(Cfg),
    io:format("~n========== 结果 ==========~n"),
    io:format("1. 原始 chat        : ~ts~n", [tag(R1)]),
    io:format("2. agent 单轮       : ~ts~n", [tag(R2)]),
    io:format("3. agent + 工具循环 : ~ts~n", [tag(R3)]),
    case lists:all(fun(X) -> X =:= ok end, [R1, R2, R3]) of
        true  -> io:format("~n全部通过 ✓~n"), ok;
        false -> io:format("~n有失败 ✗~n"), error
    end.

%%====================================================================

%% 1. 原始 chat completion
t_chat(Cfg) ->
    Messages = [#{role => user, content => <<"用一句话回答：1+1 等于几？"/utf8>>}],
    try beamai_chat_completion:chat(Cfg, Messages) of
        {ok, Resp} ->
            Content = beamai_llm_response:content(Resp),
            io:format("  [chat] -> ~ts~n", [Content]),
            case is_binary(Content) andalso byte_size(Content) > 0 of
                true -> ok; false -> {error, empty_content}
            end;
        {error, Reason} ->
            io:format("  [chat] error: ~p~n", [Reason]), {error, Reason}
    catch C:E:S ->
        io:format("  [chat] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}}
    end.

%% 2. agent 单轮
t_agent(Cfg) ->
    try
        {ok, Agent} = beamai_agent:new(#{
            llm => Cfg,
            system_prompt => <<"你是助手，用简短中文回答。"/utf8>>}),
        case beamai_agent:run(Agent, <<"你好，请介绍一下你自己（一句话）。"/utf8>>) of
            {ok, Result, _} ->
                io:format("  [agent] -> ~ts~n", [maps:get(content, Result)]),
                ok;
            Other ->
                io:format("  [agent] -> ~p~n", [Other]), {error, Other}
        end
    catch C:E:S ->
        io:format("  [agent] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}}
    end.

%% 3. agent + 工具（工具调用循环）—— 用上游 live test 的干净姿势：
%%    预构建 kernel（add_tools + add_service），经 new(#{kernel => K}) 注入。
t_agent_tool(Cfg) ->
    try
        Weather = beamai_tool:new(
            <<"get_weather">>,
            fun(#{<<"city">> := City}) ->
                {ok, <<City/binary, " 今天 25 摄氏度，晴。"/utf8>>}
            end,
            #{description => <<"查询指定城市的天气"/utf8>>,
              parameters => #{<<"city">> => #{type => string,
                                              description => <<"城市名"/utf8>>,
                                              required => true}}}),
        K0 = beamai_kernel:add_tools(beamai_kernel:new(), [Weather]),
        K1 = beamai_kernel:add_service(K0, Cfg),
        {ok, Agent} = beamai_agent:new(#{
            kernel => K1,
            system_prompt => <<"你可以用 get_weather 工具查天气。必须调用工具，不要猜。"/utf8>>}),
        case beamai_agent:run(Agent, <<"北京天气怎么样？"/utf8>>) of
            {ok, Result, A} ->
                io:format("  [tool] -> ~ts~n", [maps:get(content, Result)]),
                io:format("  [tool] turns=~p~n", [beamai_agent:turn_count(A)]),
                ok;
            Other ->
                io:format("  [tool] -> ~p~n", [Other]), {error, Other}
        end
    catch C:E:S ->
        io:format("  [tool] crash: ~p:~p~n~p~n", [C, E, S]), {error, {C, E}}
    end.

tag(ok) -> "✓ PASS";
tag({error, R}) -> lists:flatten(io_lib:format("✗ FAIL (~p)", [R])).
