#!/bin/bash

# Simplified test to debug the issue
set -e

echo "=== Testing OSVM CLI Basic Functionality ==="

# Test basic help
echo "Test 1: Help command"
./target/release/osvm --help > test_output.log 2>&1
echo "Exit code: $?"
echo "Output:"
head -5 test_output.log
echo ""

# Test version
echo "Test 2: Version command" 
./target/release/osvm --version > test_output.log 2>&1
echo "Exit code: $?"
echo "Output:"
cat test_output.log
echo ""

# Test AI ask command
echo "Test 3: AI ask command"
unset OPENAI_URL OPENAI_KEY
./target/release/osvm ask "What is a buffer overflow?" > test_output.log 2>&1 || echo "Command failed with exit code: $?"
echo "Output:"
cat test_output.log
echo ""

# Clean up
rm -f test_output.log

echo "=== Basic Test Complete ==="