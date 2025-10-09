#!/bin/bash
# Quick comprehensive tests for osvm plan command

OSVM="./target/release/osvm"

echo "🧪 OSVM Plan Command - Quick Test Suite"
echo "========================================"
echo ""

# Test 1: Basic command mapping
echo "Test 1: SVM List Mapping"
timeout 5 $OSVM plan "show me all svms" 2>&1 | grep -q "osvm svm list" && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 2: Balance Check Mapping"
timeout 5 $OSVM plan "check my balance" 2>&1 | grep -q "osvm balance" && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 3: Nodes List Mapping"
timeout 5 $OSVM plan "list all nodes" 2>&1 | grep -q "osvm nodes list" && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 4: Doctor Mapping"
timeout 5 $OSVM plan "check system health" 2>&1 | grep -q "osvm doctor" && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 5: Examples Mapping"
timeout 5 $OSVM plan "show examples" 2>&1 | grep -q "osvm examples" && echo "✅ PASS" || echo "❌ FAIL"

# Test 2: Case insensitivity
echo ""
echo "Test 6: Case Insensitivity"
timeout 5 $OSVM plan "SHOW ME ALL SVMS" 2>&1 | grep -q "osvm svm list" && echo "✅ PASS" || echo "❌ FAIL"

# Test 3: JSON output
echo ""
echo "Test 7: JSON Output Format"
output=$(timeout 5 $OSVM plan "check balance" --json 2>&1)
echo "$output" | grep -q '"reasoning"' && echo "$output" | grep -q '"confidence"' && echo "✅ PASS" || echo "❌ FAIL"

# Test 4: Execution
echo ""
echo "Test 8: Plan Execution"
timeout 10 $OSVM plan "check balance" --execute 2>&1 | grep -q "Execution Plan Results" && echo "✅ PASS" || echo "❌ FAIL"

# Test 5: Confidence score
echo ""
echo "Test 9: Confidence Score Present"
timeout 5 $OSVM plan "show svms" 2>&1 | grep -q "Confidence:" && echo "✅ PASS" || echo "❌ FAIL"

# Test 6: Reasoning present
echo ""
echo "Test 10: Reasoning Present"
timeout 5 $OSVM plan "list nodes" 2>&1 | grep -q "Reasoning:" && echo "✅ PASS" || echo "❌ FAIL"

# Test 7: Expected outcome
echo ""
echo "Test 11: Expected Outcome Present"
timeout 5 $OSVM plan "check health" 2>&1 | grep -q "Expected outcome:" && echo "✅ PASS" || echo "❌ FAIL"

# Test 8: Steps shown
echo ""
echo "Test 12: Steps Displayed"
timeout 5 $OSVM plan "show balance" 2>&1 | grep -q "Steps:" && echo "✅ PASS" || echo "❌ FAIL"

# Test 9: Execution timing
echo ""
echo "Test 13: Execution Shows Timing"
timeout 10 $OSVM plan "show examples" --execute 2>&1 | grep -q "ms" && echo "✅ PASS" || echo "❌ FAIL"

# Test 10: Default fallback for vague queries
echo ""
echo "Test 14: Vague Query Fallback"
timeout 5 $OSVM plan "help me" 2>&1 | grep -q "osvm examples" && echo "✅ PASS" || echo "❌ FAIL"

# Test 11: Natural language variations
echo ""
echo "Test 15: Natural Language - 'wallet' keyword"
timeout 5 $OSVM plan "what's in my wallet?" 2>&1 | grep -q "osvm balance" && echo "✅ PASS" || echo "❌ FAIL"

echo ""
echo "Test 16: Natural Language - 'doctor' keyword"
timeout 5 $OSVM plan "run doctor" 2>&1 | grep -q "osvm doctor" && echo "✅ PASS" || echo "❌ FAIL"

echo ""
echo "Test 17: Natural Language - 'status' keyword"
timeout 5 $OSVM plan "check status" 2>&1 | grep -q "osvm doctor" && echo "✅ PASS" || echo "❌ FAIL"

# Test edge cases
echo ""
echo "Test 18: Short Query"
timeout 5 $OSVM plan "help" 2>&1 | grep -q "osvm examples" && echo "✅ PASS" || echo "❌ FAIL"

echo ""
echo "Test 19: Long Query"
timeout 5 $OSVM plan "I would really like to see all the SVMs that are available" 2>&1 | grep -q "osvm svm list" && echo "✅ PASS" || echo "❌ FAIL"

echo ""
echo "Test 20: Query with Special Characters"
timeout 5 $OSVM plan "what's my balance?" 2>&1 | grep -q "osvm balance" && echo "✅ PASS" || echo "❌ FAIL"

echo ""
echo "========================================"
echo "✨ Quick Test Suite Complete!"
