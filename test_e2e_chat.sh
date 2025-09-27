#!/bin/bash

echo "OSVM Chat E2E Test Suite"
echo "========================"
echo ""

# Function to test a scenario
test_scenario() {
    local name="$1"
    local test_msg="$2"
    local expected="$3"

    echo "Test: $name"
    echo "  Message: '$test_msg'"

    export OSVM_TEST_MESSAGE="$test_msg"
    output=$(CI=1 timeout 2 ./target/release/osvm chat 2>&1 || true)

    if echo "$output" | grep -q "$expected"; then
        echo "  ✅ PASS: Found '$expected'"
    else
        echo "  ❌ FAIL: Expected text not found"
        echo "  Debug output:"
        echo "$output" | grep -i "ai\|planning\|fallback" | head -5 | sed 's/^/    /'
    fi
    echo ""
}

# Test 1: AI not configured
unset OPENAI_URL
unset OPENAI_KEY
echo "=== Test Suite 1: Without AI Configuration ==="
echo ""

test_scenario \
    "Startup message about AI" \
    "help" \
    "AI service not configured"

test_scenario \
    "Fallback mode activation" \
    "What is my balance?" \
    "fallback\|heuristic\|simulated"

# Test 2: With mock AI configuration
echo "=== Test Suite 2: With Mock AI Configuration ==="
echo ""

export OPENAI_URL="http://localhost:99999/v1/chat/completions"  # Non-existent endpoint
export OPENAI_KEY="test-key"

test_scenario \
    "AI configured but unreachable" \
    "Show my transactions" \
    "AI planning\|Planning which tools"

# Test 3: Demo mode features
echo "=== Test Suite 3: Demo Mode Features ==="
echo ""

# Check suggestions display
echo "Test: Suggestions display in advanced mode"
output=$(CI=1 timeout 2 ./target/release/osvm chat --advanced 2>&1 || true)

if echo "$output" | grep -q "1\. Show recent transactions"; then
    echo "  ✅ PASS: Suggestions displayed correctly"
else
    echo "  ❌ FAIL: Suggestions not found"
fi

if echo "$output" | grep -q "\[44;37m"; then
    echo "  ✅ PASS: Blue background color codes present"
else
    echo "  ⚠️  INFO: Color codes not visible in CI mode"
fi

echo ""
echo "=== Summary ==="
echo "Tests completed. Review results above."
echo ""
echo "To enable AI planning:"
echo "  1. Install Ollama: curl -fsSL https://ollama.com/install.sh | sh"
echo "  2. Start server: ollama serve"
echo "  3. Pull model: ollama pull llama2"
echo "  4. Configure: export OPENAI_URL='http://localhost:11434/v1/chat/completions'"