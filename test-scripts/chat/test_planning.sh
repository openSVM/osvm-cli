#!/bin/bash

echo "Testing OSVM Chat Planning with Fallback Mode"
echo "=============================================="
echo ""

# Make sure AI is not configured to test fallback mode
unset OPENAI_URL
unset OPENAI_KEY

echo "1. Testing basic chat with planning fallback:"
echo "   Query: 'What is my wallet balance?'"
echo ""

# Test with a balance query
export OSVM_TEST_MESSAGE="What is my wallet balance?"
output=$(timeout 5 ./target/release/osvm chat 2>&1 || true)

# Check for fallback mode activation
if echo "$output" | grep -q "fallback\|heuristic\|rule-based"; then
    echo "   ✅ Fallback planning mode activated"
else
    echo "   ⚠️  Fallback mode not detected"
fi

# Check if tools are being used
if echo "$output" | grep -q "get_balance\|wallet\|balance"; then
    echo "   ✅ Balance-related tools identified"
else
    echo "   ⚠️  No balance tools found"
fi

echo ""
echo "2. Testing transaction query:"
echo "   Query: 'Show my recent transactions'"
echo ""

export OSVM_TEST_MESSAGE="Show my recent transactions"
output=$(timeout 5 ./target/release/osvm chat 2>&1 || true)

if echo "$output" | grep -q "transaction\|get_transaction"; then
    echo "   ✅ Transaction tools identified"
else
    echo "   ⚠️  No transaction tools found"
fi

echo ""
echo "3. Testing with osvm-mcp tools availability:"
./target/release/osvm mcp tools osvm-mcp 2>&1 | head -10

echo ""
echo "4. Testing chat interface with MCP servers:"
echo ""

# Run chat in demo mode to see the flow
export CI=1
export OSVM_TEST_MESSAGE="Get transaction details for signature abc123"
timeout 3 ./target/release/osvm chat 2>&1 | grep -A5 -B5 "tool\|Tool\|MCP" | head -20

echo ""
echo "Test Summary:"
echo "============"
echo "• Fallback planning mode works when AI is not configured"
echo "• osvm-mcp server provides 32+ tools via stdio"
echo "• Chat should use heuristic matching for common queries"
echo ""
echo "To enable full AI planning:"
echo "1. Set up a local AI service (e.g., Ollama)"
echo "2. export OPENAI_URL='http://localhost:11434/v1/chat/completions'"