#!/bin/bash
# Comprehensive test suite for osvm plan command

set -e

OSVM="./target/release/osvm"
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

passed=0
failed=0
total=0

# Helper functions
test_start() {
    total=$((total + 1))
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Test $total: $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

test_pass() {
    passed=$((passed + 1))
    echo -e "${GREEN}✅ PASS${NC}\n"
}

test_fail() {
    failed=$((failed + 1))
    echo -e "${RED}❌ FAIL: $1${NC}\n"
}

verify_contains() {
    local output="$1"
    local expected="$2"
    if echo "$output" | grep -q "$expected"; then
        return 0
    else
        return 1
    fi
}

# ============================================================================
# TEST SUITE 1: Basic Command Mapping
# ============================================================================

echo -e "${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 1: Basic Command Mapping            ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "SVM List Query"
output=$($OSVM plan "show me all svms" 2>&1)
if verify_contains "$output" "osvm svm list"; then
    test_pass
else
    test_fail "Expected 'osvm svm list' in output"
    echo "$output"
fi

test_start "Balance Check Query"
output=$($OSVM plan "what's my wallet balance?" 2>&1)
if verify_contains "$output" "osvm balance"; then
    test_pass
else
    test_fail "Expected 'osvm balance' in output"
    echo "$output"
fi

test_start "Node List Query"
output=$($OSVM plan "list all my nodes" 2>&1)
if verify_contains "$output" "osvm nodes list"; then
    test_pass
else
    test_fail "Expected 'osvm nodes list' in output"
    echo "$output"
fi

test_start "System Health Query"
output=$($OSVM plan "check system health" 2>&1)
if verify_contains "$output" "osvm doctor"; then
    test_pass
else
    test_fail "Expected 'osvm doctor' in output"
    echo "$output"
fi

test_start "Examples Query"
output=$($OSVM plan "show me some examples" 2>&1)
if verify_contains "$output" "osvm examples"; then
    test_pass
else
    test_fail "Expected 'osvm examples' in output"
    echo "$output"
fi

# ============================================================================
# TEST SUITE 2: Natural Language Variations
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 2: Natural Language Variations      ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "Casual Balance Query"
output=$($OSVM plan "check my balance" 2>&1)
if verify_contains "$output" "osvm balance"; then
    test_pass
else
    test_fail "Expected 'osvm balance' in output"
fi

test_start "Formal SVM Query"
output=$($OSVM plan "display all available SVMs" 2>&1)
if verify_contains "$output" "osvm svm list"; then
    test_pass
else
    test_fail "Expected 'osvm svm list' in output"
fi

test_start "Mixed Case Query"
output=$($OSVM plan "Show Me ALL SVMS" 2>&1)
if verify_contains "$output" "osvm svm list"; then
    test_pass
else
    test_fail "Expected 'osvm svm list' (case insensitive)"
fi

# ============================================================================
# TEST SUITE 3: Output Formats
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 3: Output Formats                   ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "JSON Output Format"
output=$($OSVM plan "check balance" --json 2>&1)
if echo "$output" | grep -q '"reasoning"' && echo "$output" | grep -q '"confidence"'; then
    test_pass
else
    test_fail "Expected valid JSON output"
    echo "$output"
fi

test_start "Default Pretty Output"
output=$($OSVM plan "list svms" 2>&1)
if verify_contains "$output" "Execution Plan" && verify_contains "$output" "Reasoning:"; then
    test_pass
else
    test_fail "Expected pretty formatted output"
fi

# ============================================================================
# TEST SUITE 4: Execution Modes
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 4: Execution Modes                  ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "Plan Only (No Execution)"
output=$($OSVM plan "show examples" 2>&1)
if verify_contains "$output" "Use --execute to run this plan"; then
    test_pass
else
    test_fail "Expected plan-only mode message"
fi

test_start "Execute Mode"
output=$($OSVM plan "show examples" --execute 2>&1)
if verify_contains "$output" "Executing plan" || verify_contains "$output" "Execution Plan Results"; then
    test_pass
else
    test_fail "Expected execution to occur"
    echo "$output"
fi

# ============================================================================
# TEST SUITE 5: Edge Cases
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 5: Edge Cases                       ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "Very Short Query"
output=$($OSVM plan "help" 2>&1)
if verify_contains "$output" "osvm examples"; then
    test_pass
else
    test_fail "Expected fallback to examples"
fi

test_start "Query with Special Characters"
output=$($OSVM plan "what's my balance?" 2>&1)
if verify_contains "$output" "osvm balance"; then
    test_pass
else
    test_fail "Should handle apostrophes"
fi

test_start "Long Query"
output=$($OSVM plan "I would really like to see a comprehensive list of all the Solana Virtual Machines that are currently available" 2>&1)
if verify_contains "$output" "osvm svm list"; then
    test_pass
else
    test_fail "Should extract intent from long query"
fi

test_start "Ambiguous Query"
output=$($OSVM plan "show me something" 2>&1)
if verify_contains "$output" "osvm examples"; then
    test_pass
else
    test_fail "Should default to examples for vague queries"
fi

# ============================================================================
# TEST SUITE 6: Plan Structure Validation
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 6: Plan Structure Validation        ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "Contains Reasoning"
output=$($OSVM plan "check balance" 2>&1)
if verify_contains "$output" "Reasoning:" || verify_contains "$output" "💭"; then
    test_pass
else
    test_fail "Plan should contain reasoning"
fi

test_start "Contains Confidence Score"
output=$($OSVM plan "list nodes" 2>&1)
if verify_contains "$output" "Confidence:" || verify_contains "$output" "🎯"; then
    test_pass
else
    test_fail "Plan should contain confidence score"
fi

test_start "Contains Expected Outcome"
output=$($OSVM plan "show svms" 2>&1)
if verify_contains "$output" "Expected outcome:" || verify_contains "$output" "✨"; then
    test_pass
else
    test_fail "Plan should contain expected outcome"
fi

test_start "Contains Steps Count"
output=$($OSVM plan "check health" 2>&1)
if verify_contains "$output" "Steps:" || verify_contains "$output" "🔧"; then
    test_pass
else
    test_fail "Plan should show number of steps"
fi

# ============================================================================
# TEST SUITE 7: Execution Results
# ============================================================================

echo -e "\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║  Test Suite 7: Execution Results                ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}"

test_start "Execution Shows Results"
output=$($OSVM plan "check balance" --execute 2>&1)
if verify_contains "$output" "Execution Plan Results" || verify_contains "$output" "📋"; then
    test_pass
else
    test_fail "Execution should show results"
    echo "$output"
fi

test_start "Execution Shows Timing"
output=$($OSVM plan "show examples" --execute 2>&1)
if verify_contains "$output" "Time:" || verify_contains "$output" "ms"; then
    test_pass
else
    test_fail "Execution should show timing"
    echo "$output"
fi

test_start "Execution Shows Success/Fail"
output=$($OSVM plan "check balance" --execute 2>&1)
if verify_contains "$output" "✅" || verify_contains "$output" "❌"; then
    test_pass
else
    test_fail "Execution should show status indicators"
fi

# ============================================================================
# FINAL SUMMARY
# ============================================================================

echo -e "\n\n${YELLOW}╔══════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║                 TEST SUMMARY                     ║${NC}"
echo -e "${YELLOW}╚══════════════════════════════════════════════════╝${NC}\n"

echo -e "Total Tests:  ${BLUE}$total${NC}"
echo -e "Passed:       ${GREEN}$passed${NC}"
echo -e "Failed:       ${RED}$failed${NC}"

if [ $failed -eq 0 ]; then
    echo -e "\n${GREEN}╔══════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║          🎉 ALL TESTS PASSED! 🎉                 ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════╝${NC}\n"
    exit 0
else
    success_rate=$(( (passed * 100) / total ))
    echo -e "\n${YELLOW}Success Rate: ${success_rate}%${NC}\n"
    exit 1
fi
