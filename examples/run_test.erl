#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/beamai/_build/default/lib/beamai_core/ebin
%%! -pa ../_build/default/lib/beamai/_build/default/lib/beamai_llm/ebin
%%! -pa ../_build/default/lib/beamai/_build/default/lib/beamai_memory/ebin
%%! -pa ../_build/default/lib/beamai/_build/default/lib/*/ebin
%%! -pa _build/default/lib/beamai_examples/ebin

main(_) ->
    io:format("=== Simple Agent Test with Anthropic ===~n~n"),

    %% 检查环境变量
    AuthToken = case os:getenv("ANTHROPIC_AUTH_TOKEN") of
        false ->
            io:format("Error: ANTHROPIC_AUTH_TOKEN environment variable not set~n"),
            io:format("Please set: export ANTHROPIC_AUTH_TOKEN='your-token'~n"),
            halt(1);
        Token -> Token
    end,

    BaseUrl = case os:getenv("ANTHROPIC_BASE_URL") of
        false -> "https://api.anthropic.com";
        Url -> Url
    end,

    io:format("Configuration:~n"),
    io:format("  Base URL: ~s~n", [BaseUrl]),
    io:format("  Model: claude-sonnet-4-20250514~n~n"),

    %% 确保所有应用已加载
    {ok, Apps} = application:ensure_all_started(beamai_examples),
    io:format("Started applications: ~p~n~n", [Apps]),

    %% 调用测试
    io:format("Running test_anthropic_agent:run_simple()...~n~n"),
    case test_anthropic_agent:run_simple() of
        {ok, Result1, Result2} ->
            io:format("~n=== TEST PASSED ===~n"),
            halt(0);
        {error, Reason} ->
            io:format("~n=== TEST FAILED: ~p ===~n", [Reason]),
            halt(1)
    end.
