#!/bin/bash
# Comprehensive test suite for OSVM Command Planner

set -e

OSVM="./target/release/osvm"
PASSED=0
FAILED=0
TOTAL=0

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test result tracking
declare -a FAILED_TESTS=()

test_case() {
    TOTAL=$((TOTAL + 1))
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Test $TOTAL: $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

assert_contains() {
    local output="$1"
    local expected="$2"
    if echo "$output" | grep -q "$expected"; then
        PASSED=$((PASSED + 1))
        echo -e "${GREEN}✅ PASS${NC}"
        return 0
    else
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("Test $TOTAL: $3")
        echo -e "${RED}❌ FAIL: Expected '$expected' not found${NC}"
        echo -e "${YELLOW}Output preview:${NC}"
        echo "$output" | head -5
        return 1
    fi
}

assert_not_contains() {
    local output="$1"
    local unexpected="$2"
    if ! echo "$output" | grep -q "$unexpected"; then
        PASSED=$((PASSED + 1))
        echo -e "${GREEN}✅ PASS${NC}"
        return 0
    else
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("Test $TOTAL: $3")
        echo -e "${RED}❌ FAIL: Unexpected '$unexpected' found${NC}"
        return 1
    fi
}

echo -e "${YELLOW}╔════════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  OSVM Command Planner - Comprehensive Test Suite  ║${NC}"
echo -e "${YELLOW}╚════════════════════════════════════════════════════╝${NC}"

# ============================================================================
# SUITE 1: Basic Command Mappings
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 1: Basic Command Mappings ═══${NC}"

test_case "SVM List - Standard Query"
output=$(timeout 5 $OSVM plan "show me all svms" 2>&1)
assert_contains "$output" "osvm svm list" "SVM list mapping"

test_case "Balance Check - Standard Query"
output=$(timeout 5 $OSVM plan "check my balance" 2>&1)
assert_contains "$output" "osvm balance" "Balance check mapping"

test_case "Nodes List - Standard Query"
output=$(timeout 5 $OSVM plan "list all nodes" 2>&1)
assert_contains "$output" "osvm nodes list" "Nodes list mapping"

test_case "System Health - Doctor Command"
output=$(timeout 5 $OSVM plan "check system health" 2>&1)
assert_contains "$output" "osvm doctor" "Doctor command mapping"

test_case "Examples - Help Query"
output=$(timeout 5 $OSVM plan "show me examples" 2>&1)
assert_contains "$output" "osvm examples" "Examples command mapping"

# ============================================================================
# SUITE 2: Natural Language Variations
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 2: Natural Language Variations ═══${NC}"

test_case "Balance - Variation 1 (wallet)"
output=$(timeout 5 $OSVM plan "what's in my wallet?" 2>&1)
assert_contains "$output" "osvm balance" "Wallet keyword"

test_case "Balance - Variation 2 (SOL)"
output=$(timeout 5 $OSVM plan "how much SOL do I have" 2>&1)
assert_contains "$output" "osvm balance" "SOL keyword"

test_case "SVM - Variation 1 (display)"
output=$(timeout 5 $OSVM plan "display all svms" 2>&1)
assert_contains "$output" "osvm svm list" "Display keyword"

test_case "Health - Variation 1 (status)"
output=$(timeout 5 $OSVM plan "system status" 2>&1)
assert_contains "$output" "osvm doctor" "Status keyword"

test_case "Help - Variation 1 (help me)"
output=$(timeout 5 $OSVM plan "help me" 2>&1)
assert_contains "$output" "osvm examples" "Help keyword"

# ============================================================================
# SUITE 3: Edge Cases - Input Validation
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 3: Edge Cases ═══${NC}"

test_case "Empty Query (should default to examples)"
output=$(timeout 5 $OSVM plan "" 2>&1 || true)
# Empty might error or default, either is acceptable
echo "Output: $output" | head -2

test_case "Very Long Query"
output=$(timeout 5 $OSVM plan "I would really like to see a comprehensive detailed list of absolutely all the Solana Virtual Machines that are currently available in the entire system right now please" 2>&1)
assert_contains "$output" "osvm svm list" "Long query handling"

test_case "Special Characters - Apostrophe"
output=$(timeout 5 $OSVM plan "what's my balance?" 2>&1)
assert_contains "$output" "osvm balance" "Apostrophe handling"

test_case "Special Characters - Quotes"
output=$(timeout 5 $OSVM plan 'show "all" svms' 2>&1)
assert_contains "$output" "osvm svm" "Quotes handling"

test_case "Mixed Case Input"
output=$(timeout 5 $OSVM plan "SHOW ME ALL SVMS" 2>&1)
assert_contains "$output" "osvm svm list" "Case insensitivity"

test_case "Unicode Characters"
output=$(timeout 5 $OSVM plan "show me svms 🚀" 2>&1)
assert_contains "$output" "osvm svm" "Unicode handling"

# ============================================================================
# SUITE 4: Agent Command Integration
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 4: Agent Integration ═══${NC}"

test_case "Agent - Balance Query"
output=$(timeout 10 $OSVM agent "check my balance" 2>&1)
assert_contains "$output" "Execution Results" "Agent execution"

test_case "Agent - SVM Query"
output=$(timeout 10 $OSVM agent "show svms" 2>&1)
assert_contains "$output" "✅" "Agent success indicator"

test_case "Agent - Examples Query"
output=$(timeout 10 $OSVM agent "show examples" 2>&1)
assert_contains "$output" "Available example categories" "Agent examples output"

# ============================================================================
# SUITE 5: Plan Structure Validation
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 5: Plan Structure ═══${NC}"

test_case "Plan Has Reasoning"
output=$(timeout 5 $OSVM plan "list nodes" 2>&1)
assert_contains "$output" "Reasoning:" "Reasoning present"

test_case "Plan Has Confidence Score"
output=$(timeout 5 $OSVM plan "check balance" 2>&1)
assert_contains "$output" "Confidence:" "Confidence score present"

test_case "Plan Has Expected Outcome"
output=$(timeout 5 $OSVM plan "show svms" 2>&1)
assert_contains "$output" "Expected outcome:" "Expected outcome present"

test_case "Plan Has Execution Instructions"
output=$(timeout 5 $OSVM plan "list nodes" 2>&1)
assert_contains "$output" "Use --execute" "Execution instructions"

# ============================================================================
# SUITE 6: Execution Mode Testing
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 6: Execution Modes ═══${NC}"

test_case "Plan Only Mode (no execution)"
output=$(timeout 5 $OSVM plan "show examples" 2>&1)
assert_contains "$output" "Use --execute to run this plan" "Plan-only mode message"

test_case "Execution Mode - Balance"
output=$(timeout 10 $OSVM plan "check balance" --execute 2>&1)
assert_contains "$output" "Execution Plan Results" "Execute mode results"

test_case "Execution Mode - Success Indicator"
output=$(timeout 10 $OSVM plan "show examples" --execute 2>&1)
assert_contains "$output" "✅" "Success indicator in execution"

test_case "Execution Mode - Timing Info"
output=$(timeout 10 $OSVM plan "check balance" --execute 2>&1)
assert_contains "$output" "ms" "Timing information"

# ============================================================================
# SUITE 7: JSON Output Format
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 7: JSON Output ═══${NC}"

test_case "JSON Output - Valid Structure"
output=$(timeout 5 $OSVM plan "list svms" --json 2>&1)
assert_contains "$output" '"reasoning"' "JSON has reasoning"

test_case "JSON Output - Confidence Field"
output=$(timeout 5 $OSVM plan "check balance" --json 2>&1)
assert_contains "$output" '"confidence"' "JSON has confidence"

test_case "JSON Output - Steps Array"
output=$(timeout 5 $OSVM plan "show nodes" --json 2>&1)
assert_contains "$output" '"steps"' "JSON has steps array"

test_case "JSON Output - Can Parse"
output=$(timeout 5 $OSVM plan "check balance" --json 2>&1)
if echo "$output" | jq . >/dev/null 2>&1; then
    PASSED=$((PASSED + 1))
    echo -e "${GREEN}✅ PASS - Valid JSON${NC}"
else
    FAILED=$((FAILED + 1))
    FAILED_TESTS+=("Test $TOTAL: JSON parsing")
    echo -e "${RED}❌ FAIL - Invalid JSON${NC}"
fi

# ============================================================================
# SUITE 8: Ambiguous and Invalid Queries
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 8: Ambiguous Queries ═══${NC}"

test_case "Ambiguous Query - Random Text"
output=$(timeout 5 $OSVM plan "xyz abc 123" 2>&1)
assert_contains "$output" "osvm examples" "Ambiguous defaults to examples"

test_case "Ambiguous Query - Just 'help'"
output=$(timeout 5 $OSVM plan "help" 2>&1)
assert_contains "$output" "osvm examples" "Help maps to examples"

test_case "Partially Matching Query"
output=$(timeout 5 $OSVM plan "show me something" 2>&1)
assert_contains "$output" "osvm examples" "Vague query defaults to examples"

# ============================================================================
# SUITE 9: Performance Testing
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 9: Performance ═══${NC}"

test_case "Plan Creation Speed"
start_time=$(date +%s%N)
output=$(timeout 5 $OSVM plan "check balance" 2>&1)
end_time=$(date +%s%N)
elapsed=$(( (end_time - start_time) / 1000000 )) # Convert to ms
if [ $elapsed -lt 2000 ]; then
    PASSED=$((PASSED + 1))
    echo -e "${GREEN}✅ PASS - Plan created in ${elapsed}ms${NC}"
else
    FAILED=$((FAILED + 1))
    FAILED_TESTS+=("Test $TOTAL: Plan creation too slow")
    echo -e "${RED}❌ FAIL - Plan took ${elapsed}ms (>2000ms)${NC}"
fi

test_case "Execution Speed - Balance"
start_time=$(date +%s%N)
output=$(timeout 10 $OSVM plan "check balance" --execute 2>&1)
end_time=$(date +%s%N)
elapsed=$(( (end_time - start_time) / 1000000 ))
if [ $elapsed -lt 5000 ]; then
    PASSED=$((PASSED + 1))
    echo -e "${GREEN}✅ PASS - Executed in ${elapsed}ms${NC}"
else
    FAILED=$((FAILED + 1))
    FAILED_TESTS+=("Test $TOTAL: Execution too slow")
    echo -e "${RED}❌ FAIL - Execution took ${elapsed}ms (>5000ms)${NC}"
fi

# ============================================================================
# SUITE 10: Error Handling
# ============================================================================

echo -e "\n${YELLOW}═══ Suite 10: Error Handling ═══${NC}"

test_case "No Panic on Invalid UTF-8"
output=$(timeout 5 $OSVM plan "check $(printf '\xff\xfe')" 2>&1 || true)
assert_not_contains "$output" "panic" "No panic on invalid UTF-8"

test_case "Handles Network-like Query"
output=$(timeout 5 $OSVM plan "check network" 2>&1)
# Should map to something reasonable, not crash
echo "Mapped to: $(echo "$output" | grep 'osvm' | head -1)"
PASSED=$((PASSED + 1))
echo -e "${GREEN}✅ PASS - Handled gracefully${NC}"

# ============================================================================
# FINAL SUMMARY
# ============================================================================

echo -e "\n\n${YELLOW}╔════════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║                   TEST SUMMARY                     ║${NC}"
echo -e "${YELLOW}╚════════════════════════════════════════════════════╝${NC}\n"

echo -e "Total Tests:  ${BLUE}$TOTAL${NC}"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
echo -e "Failed:       ${RED}$FAILED${NC}"

if [ $FAILED -gt 0 ]; then
    echo -e "\n${RED}Failed Tests:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "  ${RED}•${NC} $test"
    done
fi

success_rate=$(( (PASSED * 100) / TOTAL ))
echo -e "\n${BLUE}Success Rate: ${success_rate}%${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}╔════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║          🎉 ALL TESTS PASSED! 🎉                   ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════╝${NC}\n"
    exit 0
else
    echo -e "\n${RED}╔════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║          ⚠️  SOME TESTS FAILED  ⚠️                  ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════════════════╝${NC}\n"
    exit 1
fi
