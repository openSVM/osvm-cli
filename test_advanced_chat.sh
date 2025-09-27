#!/bin/bash

echo "Testing OSVM Advanced Chat Interface"
echo "======================================"
echo ""

# Check if we're in a terminal
if [ -t 0 ] && [ -t 1 ]; then
    echo "✅ Running in terminal mode"
    echo ""
    echo "Starting advanced chat interface..."
    echo "Press Ctrl+C to exit when done testing"
    echo ""

    # Run the advanced chat
    ./target/release/osvm chat --advanced
else
    echo "⚠️  Not running in a terminal - testing in demo mode"
    echo ""

    # Test with timeout to see output
    timeout 2 ./target/release/osvm chat --advanced 2>&1 | head -50

    echo ""
    echo "Test Results:"
    echo "-------------"

    # Check if binary exists
    if [ -f "./target/release/osvm" ]; then
        echo "✅ Binary compiled successfully"
    else
        echo "❌ Binary not found"
    fi

    # Test basic command
    if ./target/release/osvm --version > /dev/null 2>&1; then
        echo "✅ Binary runs without errors"
    else
        echo "❌ Binary fails to run"
    fi

    # Test chat fallback
    if CI=1 ./target/release/osvm chat --advanced 2>&1 | grep -q "Running Advanced Agent Chat in demo mode"; then
        echo "✅ Demo mode fallback works"
    else
        echo "❌ Demo mode fallback failed"
    fi

    echo ""
    echo "Note: Full UI testing requires running in a real terminal."
    echo "To test interactively, run this script in a terminal:"
    echo "  ./test_advanced_chat.sh"
fi