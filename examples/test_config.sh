#!/bin/bash

# Test script to verify LLM configuration

cd "$(dirname "$0")"

# 设置代码路径
export ERL_FLAGS="-pa _build/default/lib/beamai_examples/ebin"
export ERL_FLAGS="$ERL_FLAGS -pa ../_build/default/lib/beamai/_build/default/lib/beamai_core/ebin"
export ERL_FLAGS="$ERL_FLAGS -pa ../_build/default/lib/beamai/_build/default/lib/beamai_llm/ebin"
export ERL_FLAGS="$ERL_FLAGS -pa ../_build/default/lib/beamai/_build/default/lib/beamai_memory/ebin"
export ERL_FLAGS="$ERL_FLAGS -pa ../_build/default/lib/beamai/_build/default/lib/*/ebin"

echo "=== Configuration Test ==="
echo ""
echo "Testing get_llm_config()..."
echo ""

# 检查环境变量
if [ -z "$ANTHROPIC_AUTH_TOKEN" ]; then
    echo "Error: ANTHROPIC_AUTH_TOKEN not set"
    exit 1
fi

erl -noshell -eval "
    Config = test_anthropic_agent:get_llm_config(),
    io:format('LLM Configuration:~n'),
    io:format('  Provider: ~p~n', [maps:get(provider, Config)]),
    io:format('  Base URL: ~s~n', [maps:get(base_url, Config)]),
    io:format('  Model: ~s~n', [maps:get(model, Config)]),
    io:format('  API Key: ~s...~n~n', [binary:part(maps:get(api_key, Config), {0, 10})]),
    init:stop(0)
."
