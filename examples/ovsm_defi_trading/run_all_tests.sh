#!/bin/bash

echo "🧪 OVSM Language Feature Tests"
echo "=============================="
echo ""

OSVM="/home/larp/larpdevs/osvm-cli/target/debug/osvm"
PASSED=0
FAILED=0

for script in /tmp/ovsm_examples/*.ovsm; do
    name=$(basename "$script")
    echo "▶ Testing: $name"
    
    if $OSVM ovsm run "$script" > /dev/null 2>&1; then
        echo "  ✅ PASSED"
        ((PASSED++))
    else
        echo "  ❌ FAILED"
        ((FAILED++))
    fi
done

echo ""
echo "=============================="
echo "Results: $PASSED passed, $FAILED failed"
if [ $FAILED -eq 0 ]; then
    echo "🎉 All tests passed!"
    exit 0
else
    echo "⚠️  Some tests failed"
    exit 1
fi
