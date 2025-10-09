#!/bin/bash
# Security Vulnerability Test Runner
#
# This script runs the security vulnerability test suite to verify
# vulnerabilities and validate fixes.
#
# USAGE:
#   ./test_security_vulns.sh [--all|--vuln-N]
#
# OPTIONS:
#   --all       Run all security tests
#   --vuln-1    Test SQL injection in database query
#   --vuln-2    Test command injection in MCP stdio
#   --vuln-3    Test path traversal in account reading
#   --vuln-4    Test SQL injection in batch inserts
#   --positive  Run positive tests (legitimate usage)

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}════════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}  OSVM Security Vulnerability Test Suite${NC}"
echo -e "${YELLOW}════════════════════════════════════════════════════════${NC}"
echo ""

# Function to run a specific test
run_test() {
    local test_name=$1
    local description=$2

    echo -e "${YELLOW}Running: ${NC}${test_name}"
    echo -e "${YELLOW}Testing: ${NC}${description}"
    echo ""

    if cargo test ${test_name} --ignored -- --nocapture; then
        echo -e "${GREEN}✓ Test completed${NC}"
    else
        echo -e "${RED}✗ Test failed${NC}"
    fi
    echo ""
    echo "─────────────────────────────────────────────────────────"
    echo ""
}

# Parse command line arguments
case "${1:-}" in
    --all)
        echo -e "${YELLOW}Running ALL security vulnerability tests${NC}"
        echo ""
        run_test "test_sql_injection_db_query" "Vuln 1: SQL Injection in Database Query"
        run_test "test_command_injection_mcp" "Vuln 2: Command Injection in MCP Stdio"
        run_test "test_path_traversal_accounts" "Vuln 3: Path Traversal in Account Reading"
        run_test "test_sql_injection_batch_insert" "Vuln 4: SQL Injection in Batch Inserts"
        run_test "test_legitimate_query_works" "Positive: Legitimate queries work"
        run_test "test_legitimate_mcp_server_works" "Positive: Legitimate MCP servers work"
        ;;
    --vuln-1)
        run_test "test_sql_injection_db_query" "Vuln 1: SQL Injection in Database Query"
        ;;
    --vuln-2)
        run_test "test_command_injection_mcp" "Vuln 2: Command Injection in MCP Stdio"
        ;;
    --vuln-3)
        run_test "test_path_traversal_accounts" "Vuln 3: Path Traversal in Account Reading"
        ;;
    --vuln-4)
        run_test "test_sql_injection_batch_insert" "Vuln 4: SQL Injection in Batch Inserts"
        ;;
    --positive)
        run_test "test_legitimate_query_works" "Positive: Legitimate queries work"
        run_test "test_legitimate_mcp_server_works" "Positive: Legitimate MCP servers work"
        ;;
    *)
        echo "Usage: $0 [--all|--vuln-N|--positive]"
        echo ""
        echo "Options:"
        echo "  --all       Run all security tests"
        echo "  --vuln-1    Test SQL injection in database query"
        echo "  --vuln-2    Test command injection in MCP stdio"
        echo "  --vuln-3    Test path traversal in account reading"
        echo "  --vuln-4    Test SQL injection in batch inserts"
        echo "  --positive  Run positive tests (legitimate usage)"
        echo ""
        echo "Example:"
        echo "  $0 --all"
        echo "  $0 --vuln-1"
        exit 1
        ;;
esac

echo -e "${GREEN}════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}  Test suite completed${NC}"
echo -e "${GREEN}════════════════════════════════════════════════════════${NC}"
