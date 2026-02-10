#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

main(_Args) ->
    %% Start required OTP applications
    application:ensure_all_started(hackney),
    application:ensure_all_started(gun),
    application:ensure_all_started(beamai_core),
    application:ensure_all_started(beamai_llm),
    application:ensure_all_started(beamai_memory),
    application:ensure_all_started(meck),

    %% Collect all examples to run
    Examples = [
        %% === No LLM needed ===
        %% Graph examples
        {"example_graph_simple:run_dsl", fun() -> example_graph_simple:run_dsl() end},
        {"example_graph_simple:run_builder", fun() -> example_graph_simple:run_builder() end},
        {"example_graph_conditional:run_router", fun() -> example_graph_conditional:run_router() end},
        {"example_graph_conditional:run_command", fun() -> example_graph_conditional:run_command() end},
        {"example_graph_fanout:run_static_fanout", fun() -> example_graph_fanout:run_static_fanout() end},
        {"example_graph_fanout:run_dynamic_dispatch", fun() -> example_graph_fanout:run_dynamic_dispatch() end},
        {"example_graph_loop:run_counter", fun() -> example_graph_loop:run_counter() end},
        {"example_graph_loop:run_agent_loop", fun() -> example_graph_loop:run_agent_loop() end},

        %% Prompt (no LLM)
        {"example_prompt:run_template", fun() -> example_prompt:run_template() end},
        {"example_prompt:run_parser", fun() -> example_prompt:run_parser() end},

        %% Filter invoke (no LLM)
        {"example_filter:run_invoke", fun() -> example_filter:run_invoke() end},

        %% Weather plugin direct invoke (no LLM)
        {"example_weather_plugin:invoke_only", fun() -> example_weather_plugin:invoke_only() end},

        %% Tool middleware simple (no LLM)
        {"example_tool_middleware:run_simple", fun() -> example_tool_middleware:run_simple() end},

        %% Agent HITL mock (no LLM)
        {"example_agent_hitl:demo_interrupt_tool", fun() -> example_agent_hitl:demo_interrupt_tool() end},
        {"example_agent_hitl:demo_callback_interrupt", fun() -> example_agent_hitl:demo_callback_interrupt() end},
        {"example_agent_hitl:demo_memory_resume", fun() -> example_agent_hitl:demo_memory_resume() end},

        %% DeepAgent mock (no LLM, uses meck)
        {"example_deepagent:run_simple", fun() -> example_deepagent:run_simple() end},
        {"example_deepagent:run_planned", fun() -> example_deepagent:run_planned() end},
        {"example_deepagent:run_with_callbacks", fun() -> example_deepagent:run_with_callbacks() end},
        {"example_deepagent:run_trace_inspection", fun() -> example_deepagent:run_trace_inspection() end},

        %% === LLM needed (ZHIPU Anthropic) ===
        %% Kernel chat
        {"example_kernel_chat:run", fun() -> example_kernel_chat:run() end},
        {"example_kernel_chat:run_inline", fun() -> example_kernel_chat:run_inline() end},
        {"example_kernel_chat:multi_turn", fun() -> example_kernel_chat:multi_turn() end},

        %% Filter chat
        {"example_filter:run_chat", fun() -> example_filter:run_chat() end},

        %% Prompt structured
        {"example_prompt:run_structured", fun() -> example_prompt:run_structured() end},

        %% Streaming
        {"example_streaming:run", fun() -> example_streaming:run() end},

        %% Weather plugin with LLM
        {"example_weather_plugin:run", fun() -> example_weather_plugin:run() end},

        %% Tool refactored
        {"example_tool_refactored:run", fun() -> example_tool_refactored:run() end},

        %% Tool middleware full
        {"example_tool_middleware:run", fun() -> example_tool_middleware:run() end},

        %% Agent examples
        {"example_agent:run_basic_live", fun() -> example_agent:run_basic_live() end},
        {"example_agent:run_multi_turn_live", fun() -> example_agent:run_multi_turn_live() end},
        {"example_agent:run_with_plugin_live", fun() -> example_agent:run_with_plugin_live() end},
        {"example_agent:run_with_file_plugin", fun() -> example_agent:run_with_file_plugin() end},

        %% Agent scenarios
        {"example_agent_scenarios:no_memory_no_plugin_live", fun() -> example_agent_scenarios:no_memory_no_plugin_live() end},
        {"example_agent_scenarios:no_memory_with_plugin_live", fun() -> example_agent_scenarios:no_memory_with_plugin_live() end},
        {"example_agent_scenarios:with_memory_no_plugin_live", fun() -> example_agent_scenarios:with_memory_no_plugin_live() end},
        {"example_agent_scenarios:with_memory_with_plugin_live", fun() -> example_agent_scenarios:with_memory_with_plugin_live() end},

        %% Agent mock versions
        {"example_agent:run_basic", fun() -> example_agent:run_basic() end},
        {"example_agent:run_multi_turn", fun() -> example_agent:run_multi_turn() end},
        {"example_agent:run_with_plugin", fun() -> example_agent:run_with_plugin() end},
        {"example_agent_scenarios:no_memory_no_plugin", fun() -> example_agent_scenarios:no_memory_no_plugin() end},
        {"example_agent_scenarios:no_memory_with_plugin", fun() -> example_agent_scenarios:no_memory_with_plugin() end},
        {"example_agent_scenarios:with_memory_no_plugin", fun() -> example_agent_scenarios:with_memory_no_plugin() end},
        {"example_agent_scenarios:with_memory_with_plugin", fun() -> example_agent_scenarios:with_memory_with_plugin() end},

        %% Process examples
        {"example_process_simple:run", fun() -> example_process_simple:run() end},
        {"example_process_simple:run_multiturn", fun() -> example_process_simple:run_multiturn() end},
        {"example_process_parallel:run", fun() -> example_process_parallel:run() end},
        {"example_process_hooks:run", fun() -> example_process_hooks:run() end},
        {"example_process_tools:run", fun() -> example_process_tools:run() end},
        {"example_process_tools:run_pipeline", fun() -> example_process_tools:run_pipeline() end},
        {"example_process_agent_test:run_all", fun() -> example_process_agent_test:run_all() end},

        %% Test files
        {"test_anthropic_agent:run_simple", fun() -> test_anthropic_agent:run_simple() end},
        {"test_anthropic_agent:run_with_tools", fun() -> test_anthropic_agent:run_with_tools() end},
        {"test_anthropic_agent:run_multi_turn", fun() -> test_anthropic_agent:run_multi_turn() end},
        {"test_zhipu_anthropic:run_all", fun() -> test_zhipu_anthropic:run_all() end},

        %% DeepAgent live examples
        {"example_deepagent:run_simple_live", fun() -> example_deepagent:run_simple_live() end},
        {"example_deepagent:run_planned_live", fun() -> example_deepagent:run_planned_live() end},
        {"example_deepagent:run_parallel_live", fun() -> example_deepagent:run_parallel_live() end},
        {"example_deepagent:run_with_reflection_live", fun() -> example_deepagent:run_with_reflection_live() end},

        %% DeepAgent tools live
        {"example_deepagent_tools:run_with_plugins_live", fun() -> example_deepagent_tools:run_with_plugins_live() end},
        {"example_deepagent_tools:run_custom_plugins_live", fun() -> example_deepagent_tools:run_custom_plugins_live() end},
        {"example_deepagent_tools:run_code_analysis_live", fun() -> example_deepagent_tools:run_code_analysis_live() end}
    ],

    %% Run all examples, collecting results
    Results = lists:map(fun({Name, Fun}) ->
        io:format("~n~s~n", [lists:duplicate(60, $=)]),
        io:format(">>> ~s~n", [Name]),
        io:format("~s~n", [lists:duplicate(60, $-)]),
        try
            Fun(),
            io:format("~n>>> PASS: ~s~n", [Name]),
            {Name, pass}
        catch
            C:E:S ->
                io:format("~n>>> FAIL: ~s~n  ~p:~p~n  ~p~n", [Name, C, E, hd(S)]),
                {Name, {fail, C, E}}
        end
    end, Examples),

    %% Summary
    io:format("~n~n~s~n", [lists:duplicate(60, $=)]),
    io:format("SUMMARY~n"),
    io:format("~s~n", [lists:duplicate(60, $=)]),
    Passed = [N || {N, pass} <- Results],
    Failed = [{N, R} || {N, {fail, _, _} = R} <- Results],
    io:format("Total: ~p~n", [length(Results)]),
    io:format("Passed: ~p~n", [length(Passed)]),
    io:format("Failed: ~p~n", [length(Failed)]),
    lists:foreach(fun({N, {fail, C, E}}) ->
        io:format("  FAIL: ~s (~p:~p)~n", [N, C, E])
    end, Failed),
    case Failed of
        [] -> io:format("~nAll examples passed!~n");
        _ -> io:format("~nSome examples failed!~n")
    end.
