%%%-------------------------------------------------------------------
%%% @doc beamai_tool_human 的 interrupt/resume 契约测试
%%%
%%% 阻塞式"等真人"是错的设计：上游 c8dca82/0f73809 把三层超时缺省全翻成
%%% infinity 后，阻塞不再是"30 秒被杀"而是**永远挂着**——agent 进程被一个人的
%%% 注意力钉死，重启即丢。正解是把工具注册进
%%% agent 的 interrupt_tools：LLM 一调用就暂停，handler 根本不执行，
%%% 人答完 resume，答复即工具结果。
%%%
%%% 这里跑通完整往返（mock 掉 LLM），锁住几个容易踩的点：
%%%   - interrupt tool 的 parameters 必须是原生 JSON Schema（上游原样透传）
%%%   - handler 绝不能被执行
%%%   - resume 只能用 reply/rejected，不能用 approved
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_human_interrupt_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 描述符形状
%%====================================================================

interrupt_tools_shape_test() ->
    Tools = beamai_tool_human:interrupt_tools(),
    ?assertEqual([<<"ask_human">>, <<"confirm_action">>],
                 [maps:get(name, T) || T <- Tools]),
    %% interrupt tool 不是 tool——不该有 handler
    [?assertEqual(error, maps:find(handler, T)) || T <- Tools].

%% 阻塞式 handler 已彻底删除：本模块不再是 beamai_tool_behaviour，
%% 不提供 kernel 工具，只剩 interrupt 描述符。
%% （挂进 plugins 会让模型看到重名工具——wire schema 是 kernel 工具 ++ interrupt 规格）
blocking_handler_path_is_gone_test() ->
    code:ensure_loaded(beamai_tool_human),
    ?assertNot(erlang:function_exported(beamai_tool_human, tools, 0)),
    ?assertNot(erlang:function_exported(beamai_tool_human, tool_info, 0)),
    ?assertNot(erlang:function_exported(beamai_tool_human, handle_ask_human, 2)),
    ?assertNot(erlang:function_exported(beamai_tool_human, handle_confirm_action, 2)),
    ?assertNot(lists:member(beamai_tool_human, beamai_tools:available())).

%% parameters 必须是原生 JSON Schema：上游 interrupt_tool_to_spec/1 原样透传，
%% 照抄 beamai_tool 那套简写会发出畸形 schema
interrupt_tools_use_raw_json_schema_test() ->
    [Ask | _] = beamai_tool_human:interrupt_tools(),
    Params = maps:get(parameters, Ask),
    ?assertEqual(object, maps:get(type, Params)),
    ?assert(maps:is_key(<<"question">>, maps:get(properties, Params))),
    ?assertEqual([<<"question">>], maps:get(required, Params)).

%% 上游转出来的 wire schema 必须能 JSON 编码——模型真正收到的就是它
wire_schema_is_encodable_test() ->
    Specs = beamai_agent_interrupt:get_interrupt_tool_specs(
              #{interrupt_tools => beamai_tool_human:interrupt_tools()}),
    ?assertEqual(2, length(Specs)),
    [#{type := function, function := #{name := <<"ask_human">>}} | _] = Specs,
    ?assert(is_binary(jsx:encode(Specs))).

%%====================================================================
%% 拦截
%%====================================================================

gate_catches_our_tools_test() ->
    State = #{interrupt_tools => beamai_tool_human:interrupt_tools()},
    ?assertMatch({yes, _, []},
                 beamai_agent_interrupt:find_interrupt_tool([tool_call(<<"c1">>, <<"ask_human">>)], State)),
    ?assertMatch({yes, _, []},
                 beamai_agent_interrupt:find_interrupt_tool([tool_call(<<"c2">>, <<"confirm_action">>)], State)).

gate_ignores_normal_tools_test() ->
    State = #{interrupt_tools => beamai_tool_human:interrupt_tools()},
    ?assertEqual(no,
                 beamai_agent_interrupt:find_interrupt_tool([tool_call(<<"c3">>, <<"file_read">>)], State)).

%%====================================================================
%% 完整往返（mock LLM）
%%====================================================================

interrupt_resume_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun ask_human_pauses_and_resumes/0,
      fun confirm_action_rejected/0]}.

setup() ->
    application:ensure_all_started(beamai_core),
    catch meck:unload(beamai_chat_completion),
    meck:new(beamai_chat_completion, [passthrough, unstick]),
    ok.

cleanup(_) ->
    catch meck:unload(beamai_chat_completion),
    erase(last_messages),
    ok.

%% 第一轮模型调 ask_human -> agent 暂停；resume 后模型给出终答
ask_human_pauses_and_resumes() ->
    Ctr = counters:new(1, []),
    mock_llm(Ctr, <<"ask_human">>, #{<<"question">> => <<"选 A 还是 B?"/utf8>>}),
    Agent = new_agent(),
    {interrupt, Info, Agent1} = beamai_agent:run(Agent, <<"帮我决定"/utf8>>),

    ?assertEqual(tool_request, maps:get(interrupt_type, Info)),
    %% reason 即被中断 tool_call 的参数——问题原样在里面
    Reason = maps:get(reason, Info),
    ?assertEqual(<<"选 A 还是 B?"/utf8>>, arg(<<"question">>, Reason)),
    ?assert(beamai_agent:is_interrupted(Agent1)),

    %% 人答完：reply -> 答复即工具结果
    {ok, Result, Agent2} = beamai_agent:resume(Agent1, <<"reply">>, #{message => <<"选 B"/utf8>>}),
    ?assertEqual(<<"done">>, maps:get(content, Result)),
    ?assertNot(beamai_agent:is_interrupted(Agent2)),
    %% 人的答复确实作为 tool 结果回灌给了模型
    ?assertEqual(<<"选 B"/utf8>>, last_tool_result()).

%% confirm_action 走 rejected：工具不执行，拒绝理由作为结果回模型
confirm_action_rejected() ->
    Ctr = counters:new(1, []),
    mock_llm(Ctr, <<"confirm_action">>,
             #{<<"action">> => <<"删库"/utf8>>, <<"reason">> => <<"清理"/utf8>>}),
    {interrupt, _, Agent1} = beamai_agent:run(new_agent(), <<"清理"/utf8>>),
    {ok, _Result, _} = beamai_agent:resume(Agent1, <<"rejected">>, #{message => <<"不行"/utf8>>}),
    %% 拒绝理由进了 tool 结果
    ?assert(binary:match(last_tool_result(), <<"不行"/utf8>>) =/= nomatch).

%%====================================================================
%% 辅助
%%====================================================================

new_agent() ->
    {ok, Agent} = beamai_agent:new(#{
        llm => #{provider => openai, model => <<"m">>, api_key => <<"k">>},
        memory => false,
        interrupt_tools => beamai_tool_human:interrupt_tools(),
        on_env_error => proceed
    }),
    Agent.

tool_call(Id, Name) ->
    #{<<"id">> => Id, <<"name">> => Name, <<"arguments">> => #{}}.

%% 参数 map 的键可能是 atom 也可能是 binary（取决于 parse_tool_call 的 attempt_atom）
arg(Key, Args) when is_map(Args) ->
    case maps:get(Key, Args, undefined) of
        undefined -> maps:get(binary_to_atom(Key, utf8), Args, undefined);
        V -> V
    end;
arg(_, _) -> undefined.

%% 第一次调用返回指定的 tool_call，之后返回终答。
%% 同时把每次收到的 messages 存起来，供 last_tool_result/0 取证。
mock_llm(Ctr, ToolName, Args) ->
    meck:expect(beamai_chat_completion, chat,
        fun(_Cfg, Messages, _Opts) ->
            put(last_messages, Messages),
            case counters:get(Ctr, 1) of
                0 ->
                    counters:add(Ctr, 1, 1),
                    {ok, #{content => <<>>,
                           tool_calls => [#{id => <<"c1">>, name => ToolName, arguments => Args}],
                           finish_reason => tool_calls}};
                _ ->
                    {ok, #{content => <<"done">>, tool_calls => [], finish_reason => stop}}
            end
        end).

%% 从最后一次 LLM 调用收到的 messages 里，取出 c1 的 tool 结果内容
last_tool_result() ->
    Msgs = get(last_messages),
    case [maps:get(content, M) || M <- Msgs,
                                  maps:get(role, M, undefined) =:= tool,
                                  maps:get(tool_call_id, M, undefined) =:= <<"c1">>] of
        [C | _] -> C;
        [] -> undefined
    end.
