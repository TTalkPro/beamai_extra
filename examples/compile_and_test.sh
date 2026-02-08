#!/bin/bash

# Compile and Test Script

set -e

cd "$(dirname "$0")"

echo "=== Compiling Required Applications ==="
echo ""

# 需要编译的应用
APPS="beamai_tools beamai_agent"

# 获取项目根目录
PROJECT_ROOT="$(cd ../.. && pwd)"

for app in $APPS; do
    echo "Compiling $app..."

    # 创建 ebin 目录
    mkdir -p "$PROJECT_ROOT/apps/$app/ebin"

    # 获取 include 路径
    INCLUDE="-I $PROJECT_ROOT/_build/default/lib/beamai/apps/beamai_core/include"
    INCLUDE="$INCLUDE -I $PROJECT_ROOT/_build/default/lib/beamai/apps/beamai_llm/include"
    INCLUDE="$INCLUDE -I $PROJECT_ROOT/_build/default/lib/beamai/apps/beamai_memory/include"

    # 编译源文件
    for src in "$PROJECT_ROOT/apps/$app/src"/*.erl; do
        if [ -f "$src" ]; then
            erlc -o "$PROJECT_ROOT/apps/$app/ebin" $INCLUDE "$src" 2>&1 | grep -v "Warning:" || true
        fi
    done

    # 计算编译的模块数
    count=$(ls "$PROJECT_ROOT/apps/$app/ebin"/*.beam 2>/dev/null | wc -l)
    echo "  Compiled $count modules in $app"
done

echo ""
echo "=== Running Test ==="
echo ""

# 设置代码路径
PROJECT_ROOT="$(cd ../.. && pwd)"
CODEPATH="_build/default/lib/beamai_examples/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/apps/beamai_tools/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/apps/beamai_agent/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/_build/default/lib/beamai/_build/default/lib/beamai_core/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/_build/default/lib/beamai/_build/default/lib/beamai_llm/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/_build/default/lib/beamai/_build/default/lib/beamai_memory/ebin"
CODEPATH="$CODEPATH:$PROJECT_ROOT/_build/default/lib/beamai/_build/default/lib/*/ebin"

# 检查环境变量
if [ -z "$ANTHROPIC_AUTH_TOKEN" ]; then
    echo "Error: ANTHROPIC_AUTH_TOKEN not set"
    echo "Please set: export ANTHROPIC_AUTH_TOKEN='your-token'"
    exit 1
fi

# 显示配置
echo "Configuration:"
echo "  Base URL: ${ANTHROPIC_BASE_URL:-https://api.anthropic.com}"
echo "  Model: claude-sonnet-4-20250514"
echo ""

# 运行测试
erl -pa $CODEPATH -noshell -eval "
    io:format('Running test_anthropic_agent:run_simple()...~n~n'),
    case test_anthropic_agent:run_simple() of
        {ok, _, _} ->
            io:format('~n=== TEST PASSED ===~n'),
            init:stop(0);
        {error, Reason} ->
            io:format('~n=== TEST FAILED: ~p ===~n', [Reason]),
            init:stop(1)
    end
."
