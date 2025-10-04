#!/bin/bash
# Fast test suite for OSVM Command Planner

OSVM="./target/release/osvm"
pass=0
fail=0

echo "🧪 Fast Test Suite"
echo "=================="

# Test 1: SVM mapping
echo -n "1. SVM mapping... "
if timeout 3 $OSVM plan "show svms" 2>&1 | grep -q "osvm svm list"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 2: Balance mapping
echo -n "2. Balance mapping... "
if timeout 3 $OSVM plan "check balance" 2>&1 | grep -q "osvm balance"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 3: Nodes mapping
echo -n "3. Nodes mapping... "
if timeout 3 $OSVM plan "list nodes" 2>&1 | grep -q "osvm nodes list"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 4: Doctor mapping
echo -n "4. Doctor mapping... "
if timeout 3 $OSVM plan "system health" 2>&1 | grep -q "osvm doctor"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 5: Case insensitive
echo -n "5. Case insensitive... "
if timeout 3 $OSVM plan "SHOW BALANCE" 2>&1 | grep -q "osvm balance"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 6: Wallet keyword
echo -n "6. Wallet keyword... "
if timeout 3 $OSVM plan "my wallet" 2>&1 | grep -q "osvm balance"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 7: JSON output
echo -n "7. JSON output... "
if timeout 3 $OSVM plan "check balance" --json 2>&1 | grep -q '"reasoning"'; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 8: Confidence score
echo -n "8. Confidence score... "
if timeout 3 $OSVM plan "list svms" 2>&1 | grep -q "Confidence:"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 9: Long query handling
echo -n "9. Long query... "
if timeout 3 $OSVM plan "I would really like to see all the svms please" 2>&1 | grep -q "osvm svm"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 10: Agent execution
echo -n "10. Agent execution... "
if timeout 8 $OSVM agent "show examples" 2>&1 | grep -q "Execution Results"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 11: Plan execution
echo -n "11. Plan --execute... "
if timeout 8 $OSVM plan "show examples" --execute 2>&1 | grep -q "✅"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 12: Special characters
echo -n "12. Special chars... "
if timeout 3 $OSVM plan "what's my balance?" 2>&1 | grep -q "osvm balance"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 13: Ambiguous query fallback
echo -n "13. Ambiguous fallback... "
if timeout 3 $OSVM plan "xyz abc 123" 2>&1 | grep -q "osvm examples"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 14: Has reasoning
echo -n "14. Has reasoning... "
if timeout 3 $OSVM plan "check balance" 2>&1 | grep -q "Reasoning:"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

# Test 15: Has expected outcome
echo -n "15. Has expected outcome... "
if timeout 3 $OSVM plan "list nodes" 2>&1 | grep -q "Expected outcome:"; then
    echo "✅"
    ((pass++))
else
    echo "❌"
    ((fail++))
fi

echo ""
echo "=================="
echo "Results: $pass passed, $fail failed"
echo "Success rate: $(( pass * 100 / (pass + fail) ))%"

if [ $fail -eq 0 ]; then
    echo "✅ All tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
