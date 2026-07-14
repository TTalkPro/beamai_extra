#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

main(_Args) ->
    %% Start required OTP applications
    application:ensure_all_started(hackney),
    application:ensure_all_started(gun),
    application:ensure_all_started(beamai_core),
    application:ensure_all_started(beamai_llm),
    application:ensure_all_started(meck),

    %% Collect all examples to run
    Examples = [
        %% === No LLM needed (Mock) ===
        {"example_agent:run_basic", fun() -> example_agent:run_basic() end},
        {"example_agent:run_multi_turn", fun() -> example_agent:run_multi_turn() end},
        {"example_agent:run_with_plugin", fun() -> example_agent:run_with_plugin() end},
        {"example_agent:run_with_file_plugin", fun() -> example_agent:run_with_file_plugin() end},

        {"example_agent_scenarios:no_memory_no_plugin", fun() -> example_agent_scenarios:no_memory_no_plugin() end},
        {"example_agent_scenarios:no_memory_with_plugin", fun() -> example_agent_scenarios:no_memory_with_plugin() end},

        %% === LLM needed ===
        {"example_agent:run_basic_live", fun() -> example_agent:run_basic_live() end},
        {"example_agent:run_multi_turn_live", fun() -> example_agent:run_multi_turn_live() end},
        {"example_agent:run_with_plugin_live", fun() -> example_agent:run_with_plugin_live() end},

        {"example_agent_scenarios:no_memory_no_plugin_live", fun() -> example_agent_scenarios:no_memory_no_plugin_live() end},
        {"example_agent_scenarios:no_memory_with_plugin_live", fun() -> example_agent_scenarios:no_memory_with_plugin_live() end},

        %% Test files
        {"test_anthropic_agent:run_simple", fun() -> test_anthropic_agent:run_simple() end},
        {"test_anthropic_agent:run_with_tools", fun() -> test_anthropic_agent:run_with_tools() end},
        {"test_anthropic_agent:run_multi_turn", fun() -> test_anthropic_agent:run_multi_turn() end},
        {"test_zhipu_anthropic:run_all", fun() -> test_zhipu_anthropic:run_all() end}
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
