#!/bin/bash

# Simple Agent Test Runner

cd "$(dirname "$0")"

# 设置代码路径
CODEPATH="_build/default/lib/beamai_examples/ebin:../_build/default/lib/beamai/_build/default/lib/beamai_core/ebin:../_build/default/lib/beamai/_build/default/lib/beamai_llm/ebin:../_build/default/lib/beamai/_build/default/lib/beamai_memory/ebin:../_build/default/lib/beamai/_build/default/lib/*/ebin"

# 检查环境变量
if [ -z "$ANTHROPIC_AUTH_TOKEN" ]; then
    echo "Error: ANTHROPIC_AUTH_TOKEN environment variable not set"
    echo "Please set: export ANTHROPIC_AUTH_TOKEN='your-token'"
    echo "Optional: export ANTHROPIC_BASE_URL='https://api.anthropic.com'"
    exit 1
fi

# 显示配置
echo "=== Simple Agent Test with Anthropic ==="
echo ""
echo "Configuration:"
echo "  Base URL: ${ANTHROPIC_BASE_URL:-https://api.anthropic.com}"
echo "  Model: claude-sonnet-4-20250514"
echo ""

# 运行测试
erl -pa $CODEPATH -noshell -eval "
    io:format('Starting test...~n~n'),
    case test_anthropic_agent:run_simple() of
        {ok, _, _} ->
            io:format('~n=== TEST PASSED ===~n'),
            init:stop(0);
        {error, Reason} ->
            io:format('~n=== TEST FAILED: ~p ===~n', [Reason]),
            init:stop(1)
    end
."
