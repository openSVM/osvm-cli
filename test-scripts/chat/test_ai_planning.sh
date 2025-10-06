#!/bin/bash

echo "Testing AI Planning Service"
echo "============================"
echo ""

# Check environment variables
echo "1. Checking AI service configuration:"
if [ -z "$OPENAI_URL" ]; then
    echo "   ❌ OPENAI_URL not set (required for AI planning)"
    echo "      Set with: export OPENAI_URL='https://api.openai.com/v1/chat/completions'"
    echo "      Or for local Ollama: export OPENAI_URL='http://localhost:11434/v1/chat/completions'"
else
    echo "   ✅ OPENAI_URL is set: $OPENAI_URL"
fi

if [ -z "$OPENAI_KEY" ]; then
    echo "   ⚠️  OPENAI_KEY not set (may be required depending on endpoint)"
    echo "      Set with: export OPENAI_KEY='your-api-key'"
else
    echo "   ✅ OPENAI_KEY is set"
fi

echo ""
echo "2. Testing AI service endpoint:"

# Try to call the AI service directly
if [ ! -z "$OPENAI_URL" ]; then
    echo "   Testing connection to $OPENAI_URL..."

    # Test with curl (timeout after 5 seconds)
    response=$(curl -s -m 5 -X POST "$OPENAI_URL" \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer ${OPENAI_KEY:-test}" \
        -d '{"model":"gpt-3.5-turbo","messages":[{"role":"user","content":"test"}]}' 2>&1)

    if [ $? -eq 0 ]; then
        if echo "$response" | grep -q "error\|Error"; then
            echo "   ⚠️  Endpoint responded but with error:"
            echo "      $(echo "$response" | head -1)"
        else
            echo "   ✅ AI endpoint is reachable"
        fi
    else
        echo "   ❌ Cannot reach AI endpoint"
        echo "      Check network connection and URL"
    fi
fi

echo ""
echo "3. Testing with OSVM chat (basic planning test):"

# Create a test message that should trigger planning
export OSVM_TEST_MESSAGE="What is my wallet balance?"

echo "   Sending test message: '$OSVM_TEST_MESSAGE'"
echo ""

# Run the chat and capture output
output=$(timeout 3 ./target/release/osvm chat 2>&1)

# Check for planning-related messages
if echo "$output" | grep -q "AI planning"; then
    if echo "$output" | grep -q "AI planning is unavailable"; then
        echo "   ❌ AI planning failed - service unavailable"
        echo ""
        echo "   Troubleshooting:"
        echo "   1. Set OPENAI_URL to a valid AI endpoint"
        echo "   2. Set OPENAI_KEY if using OpenAI"
        echo "   3. For local testing, install Ollama:"
        echo "      curl -fsSL https://ollama.com/install.sh | sh"
        echo "      ollama serve"
        echo "      ollama pull llama2"
        echo "      export OPENAI_URL='http://localhost:11434/v1/chat/completions'"
    else
        echo "   ✅ AI planning service is working"
    fi
elif echo "$output" | grep -q "Planning which tools to use"; then
    echo "   ✅ AI planning initiated successfully"
else
    echo "   ⚠️  Could not determine AI planning status"
    echo "   Output snippet:"
    echo "$output" | grep -A2 -B2 "balance" | head -10
fi

echo ""
echo "4. Recommendations:"
echo "   • For production: Use OpenAI API with proper key"
echo "   • For testing: Use local Ollama server"
echo "   • For demo: System falls back to heuristic planning"