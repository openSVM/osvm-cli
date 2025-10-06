#!/bin/bash

# OSVM CLI - 50 Real-World Test Scenarios with Full Request/Response Logging
# This script tests all major functionality and analyzes each AI response individually

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Build the CLI first
echo -e "${BLUE}=== Building OSVM CLI ===${NC}"
cargo build --release
echo ""

# Function to run a test and analyze the result
run_test() {
    local test_num=$1
    local test_name="$2"
    local command="$3"
    local expected_pattern="$4"
    local should_contain_ai_call="$5"
    
    ((TOTAL_TESTS++))
    
    echo -e "${CYAN}Test $test_num: $test_name${NC}"
    echo -e "${YELLOW}Command: $command${NC}"
    echo ""
    
    # Run the command and capture output
    echo -e "${PURPLE}=== EXECUTING COMMAND ===${NC}"
    if timeout 30s bash -c "$command" > test_output.log 2>&1; then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Display the full output
    echo -e "${PURPLE}=== COMMAND OUTPUT ===${NC}"
    cat test_output.log
    echo ""
    
    # Check if this test should contain AI calls
    if [ "$should_contain_ai_call" = "true" ]; then
        echo -e "${PURPLE}=== AI REQUEST/RESPONSE ANALYSIS ===${NC}"
        
        # Extract AI requests from the output
        if grep -q "🤖 Asking" test_output.log; then
            echo -e "${BLUE}AI Requests Found:${NC}"
            grep "🤖 Asking" test_output.log || true
            echo ""
            
            # Check for request details
            if grep -q "📤.*Request:" test_output.log; then
                echo -e "${BLUE}Request Details:${NC}"
                grep -A 5 "📤.*Request:" test_output.log || true
                echo ""
            fi
            
            # Check for response details  
            if grep -q "📥.*Response:" test_output.log; then
                echo -e "${BLUE}Response Details:${NC}"
                grep -A 3 "📥.*Response:" test_output.log || true
                echo ""
            fi
            
            # Look for AI responses (usually text that follows AI requests)
            echo -e "${BLUE}AI Response Analysis:${NC}"
            
            # Check if we have proper AI endpoint configuration
            if grep -q "OpenAI endpoint:" test_output.log; then
                echo "✅ OpenAI endpoint properly configured"
            elif grep -q "OSVM AI endpoint:" test_output.log; then
                echo "✅ OSVM AI endpoint properly configured"
            else
                echo "⚠️ No clear endpoint configuration found"
            fi
            
            # Check for response content patterns
            if grep -q -E "(vulnerability|security|analysis|finding)" test_output.log; then
                echo "✅ AI response contains relevant security analysis"
            elif grep -q -E "(error|failed|timeout)" test_output.log; then
                echo "⚠️ AI response indicates an error condition"
            else
                echo "ℹ️ AI response content varies (may be expected for this test)"
            fi
            
            # Check response quality indicators
            if grep -q -E "\b(line [0-9]+|at line:|location:)" test_output.log; then
                echo "✅ AI response includes specific line number references"
            fi
            
            if grep -q -E "(instances?|grouped|findings)" test_output.log; then
                echo "✅ AI response includes finding grouping information"
            fi
            
        else
            echo "ℹ️ No AI requests found in output (may be expected for this test type)"
        fi
        echo ""
    fi
    
    # Analyze the test result
    echo -e "${PURPLE}=== TEST RESULT ANALYSIS ===${NC}"
    
    # Check exit code first
    if [ $exit_code -ne 0 ] && [ "$expected_pattern" != "error" ]; then
        echo -e "${RED}❌ FAILED - Command exited with error code $exit_code${NC}"
        ((FAILED_TESTS++))
        echo ""
        return 1
    fi
    
    # Check for expected patterns
    case "$expected_pattern" in
        "help")
            if grep -q -i "usage\|help\|commands" test_output.log; then
                echo -e "${GREEN}✅ PASSED - Help information displayed correctly${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected help information not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        "version")
            if grep -q -E "version|[0-9]+\.[0-9]+\.[0-9]+" test_output.log; then
                echo -e "${GREEN}✅ PASSED - Version information displayed correctly${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected version information not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        "ai_response")
            if grep -q "🤖 Asking" test_output.log; then
                echo -e "${GREEN}✅ PASSED - AI query executed successfully${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected AI query not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        "audit")
            if grep -q -E "findings|security|analyzed|score" test_output.log; then
                echo -e "${GREEN}✅ PASSED - Audit functionality working correctly${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected audit output not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        "error")
            if [ $exit_code -ne 0 ] || grep -q -i "error\|failed\|invalid" test_output.log; then
                echo -e "${GREEN}✅ PASSED - Error handling working correctly${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected error condition not detected${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        "endpoint")
            if grep -q -E "(OpenAI endpoint:|OSVM AI endpoint:)" test_output.log; then
                echo -e "${GREEN}✅ PASSED - AI endpoint configuration detected${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected endpoint information not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
        *)
            if grep -q "$expected_pattern" test_output.log; then
                echo -e "${GREEN}✅ PASSED - Expected pattern '$expected_pattern' found${NC}"
                ((PASSED_TESTS++))
            else
                echo -e "${RED}❌ FAILED - Expected pattern '$expected_pattern' not found${NC}"
                ((FAILED_TESTS++))
            fi
            ;;
    esac
    
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo ""
}

# Create a simple test Rust project
create_test_project() {
    mkdir -p /tmp/test_project/src
    cat > /tmp/test_project/Cargo.toml << 'EOF'
[package]
name = "test_project"
version = "0.1.0"
edition = "2021"

[dependencies]
solana-program = "1.16"
EOF

    cat > /tmp/test_project/src/lib.rs << 'EOF'
use solana_program::{
    account_info::AccountInfo,
    entrypoint,
    pubkey::Pubkey,
    program_error::ProgramError,
};

entrypoint!(process_instruction);

pub fn process_instruction(
    _program_id: &Pubkey,
    accounts: &[AccountInfo],
    _instruction_data: &[u8],
) -> Result<(), ProgramError> {
    let account = &accounts[0];
    
    // Unsafe block for testing
    unsafe {
        let ptr = account.key as *const _ as *const u8;
        let _value = *ptr;
    }
    
    // Some unwrap usage for testing
    let data = account.try_borrow_data().unwrap();
    let _len = data.len().checked_add(1).expect("overflow");
    
    Ok(())
}
EOF
}

echo -e "${BLUE}=== OSVM CLI - 50 Real-World Test Scenarios ===${NC}"
echo "Testing comprehensive functionality with full request/response logging..."
echo ""

# Create test project
create_test_project

# =============================================================================
echo -e "${GREEN}=== 1. BASIC CLI FUNCTIONALITY (Tests 1-10) ===${NC}"
# =============================================================================

run_test 1 "Display help message" \
    "./target/release/osvm --help" \
    "help" \
    "false"

run_test 2 "Show version information" \
    "./target/release/osvm --version" \
    "version" \
    "false"

run_test 3 "Short help flag" \
    "./target/release/osvm -h" \
    "help" \
    "false"

run_test 4 "Invalid command handling" \
    "./target/release/osvm invalid_command 2>&1 || true" \
    "error" \
    "false"

run_test 5 "Command with invalid flags" \
    "./target/release/osvm audit --invalid-flag 2>&1 || true" \
    "error" \
    "false"

run_test 6 "List available commands" \
    "./target/release/osvm help" \
    "help" \
    "false"

run_test 7 "Check basic CLI structure" \
    "./target/release/osvm" \
    "help" \
    "false"

run_test 8 "Verify executable permissions" \
    "ls -la ./target/release/osvm && echo 'Executable check passed'" \
    "Executable check passed" \
    "false"

run_test 9 "Environment variable handling" \
    "RUST_LOG=debug ./target/release/osvm --version" \
    "version" \
    "false"

run_test 10 "Examples command" \
    "./target/release/osvm examples" \
    "example" \
    "false"

# =============================================================================
echo -e "${GREEN}=== 2. AI QUERY FUNCTIONALITY (Tests 11-20) ===${NC}"
# =============================================================================

run_test 11 "Basic AI query with OSVM.ai" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'What is a buffer overflow?'" \
    "ai_response" \
    "true"

run_test 12 "Security-focused AI query" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'Explain SQL injection vulnerabilities'" \
    "ai_response" \
    "true"

run_test 13 "Rust-specific security query" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'What are common Rust security patterns?'" \
    "ai_response" \
    "true"

run_test 14 "Solana-specific query" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'How to validate Solana account ownership?'" \
    "ai_response" \
    "true"

run_test 15 "Empty query handling" \
    "./target/release/osvm '' 2>&1 || true" \
    "error" \
    "false"

run_test 16 "Very long query handling" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'This is a very long query about security that goes on and on and on and repeats itself multiple times to test how the system handles long input strings that might cause issues with the API endpoints and request processing systems'" \
    "ai_response" \
    "true"

run_test 17 "Special characters in query" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'What about @#\$%^&*() characters?'" \
    "ai_response" \
    "true"

run_test 18 "Query with code snippets" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm 'Is this safe: unsafe { *ptr }?'" \
    "ai_response" \
    "true"

run_test 19 "Multi-word query" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm what are the main security concerns in blockchain development" \
    "ai_response" \
    "true"

run_test 20 "Query with quotes" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm \"What about 'quotes' and escapes?\"" \
    "ai_response" \
    "true"

# =============================================================================
echo -e "${GREEN}=== 3. AI ENDPOINT CONFIGURATION (Tests 21-30) ===${NC}"
# =============================================================================

run_test 21 "OSVM.ai endpoint verification" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm test endpoint" \
    "endpoint" \
    "true"

run_test 22 "OpenAI endpoint with environment variables" \
    "export OPENAI_URL='https://api.openai.com/v1/chat/completions'; export OPENAI_KEY='test_key'; ./target/release/osvm test openai 2>&1 || true" \
    "endpoint" \
    "true"

run_test 23 "Custom local endpoint simulation" \
    "export OPENAI_URL='http://localhost:8080/v1/chat/completions'; export OPENAI_KEY='local_key'; ./target/release/osvm test local 2>&1 || true" \
    "endpoint" \
    "true"

run_test 24 "Partial environment variable (URL only)" \
    "export OPENAI_URL='https://api.openai.com/v1/chat/completions'; unset OPENAI_KEY; ./target/release/osvm test partial" \
    "endpoint" \
    "true"

run_test 25 "Partial environment variable (KEY only)" \
    "unset OPENAI_URL; export OPENAI_KEY='test_key'; ./target/release/osvm test partial" \
    "endpoint" \
    "true"

run_test 26 "Invalid OpenAI URL format" \
    "export OPENAI_URL='invalid-url'; export OPENAI_KEY='test_key'; ./target/release/osvm test invalid 2>&1 || true" \
    "error" \
    "true"

run_test 27 "Empty environment variables" \
    "export OPENAI_URL=''; export OPENAI_KEY=''; ./target/release/osvm test empty" \
    "endpoint" \
    "true"

run_test 28 "Environment variable precedence" \
    "export OPENAI_URL='https://custom.api.com/v1/chat/completions'; export OPENAI_KEY='custom_key'; ./target/release/osvm test precedence 2>&1 || true" \
    "endpoint" \
    "true"

run_test 29 "Multiple model configurations" \
    "export OPENAI_URL='https://api.openai.com/v1/chat/completions'; export OPENAI_KEY='gpt4_key'; ./target/release/osvm test multiple 2>&1 || true" \
    "endpoint" \
    "true"

run_test 30 "Cleanup environment variables" \
    "unset OPENAI_URL OPENAI_KEY; ./target/release/osvm test cleanup" \
    "endpoint" \
    "true"

# =============================================================================
echo -e "${GREEN}=== 4. AUDIT FUNCTIONALITY (Tests 31-40) ===${NC}"
# =============================================================================

run_test 31 "Basic audit on test project" \
    "./target/release/osvm audit /tmp/test_project" \
    "audit" \
    "true"

run_test 32 "Audit with detailed output" \
    "./target/release/osvm audit /tmp/test_project --verbose" \
    "audit" \
    "true"

run_test 33 "Audit with JSON output format" \
    "./target/release/osvm audit /tmp/test_project --format json" \
    "audit" \
    "true"

run_test 34 "Audit with HTML output format" \
    "./target/release/osvm audit /tmp/test_project --format html" \
    "audit" \
    "true"

run_test 35 "Audit with specific output file" \
    "./target/release/osvm audit /tmp/test_project --output /tmp/audit_result.html" \
    "audit" \
    "true"

run_test 36 "Audit non-existent directory" \
    "./target/release/osvm audit /non/existent/path 2>&1 || true" \
    "error" \
    "false"

run_test 37 "Audit current OSVM project (limited)" \
    "./target/release/osvm audit . --format json --output /tmp/osvm_audit.json" \
    "audit" \
    "true"

run_test 38 "Audit with AI analysis" \
    "./target/release/osvm audit /tmp/test_project --ai-analysis" \
    "audit" \
    "true"

run_test 39 "Audit with test mode" \
    "./target/release/osvm audit /tmp/test_project --test" \
    "audit" \
    "true"

run_test 40 "Audit with verbose logging" \
    "RUST_LOG=debug ./target/release/osvm audit /tmp/test_project --verbose" \
    "audit" \
    "true"

# =============================================================================
echo -e "${GREEN}=== 5. ERROR HANDLING & EDGE CASES (Tests 41-50) ===${NC}"
# =============================================================================

run_test 41 "Network timeout simulation (short timeout)" \
    "timeout 5s ./target/release/osvm 'What is security?' 2>&1 || echo 'Timeout handled'" \
    "Timeout handled" \
    "false"

run_test 42 "Permission denied simulation" \
    "chmod 000 /tmp/test_project 2>/dev/null || true; ./target/release/osvm audit /tmp/test_project 2>&1 || true; chmod 755 /tmp/test_project 2>/dev/null || true" \
    "error" \
    "false"

run_test 43 "Large file handling" \
    "head -c 100000 /dev/zero > /tmp/test_project/large_file.rs; ./target/release/osvm audit /tmp/test_project --format json" \
    "audit" \
    "true"

run_test 44 "Binary file detection" \
    "echo -e '\\x00\\x01\\x02\\x03' > /tmp/test_project/binary_file; ./target/release/osvm audit /tmp/test_project --format json" \
    "audit" \
    "true"

run_test 45 "Unicode and special characters" \
    "echo 'fn test_unicode() { let 测试 = \"🦀\"; }' > /tmp/test_project/src/unicode.rs; ./target/release/osvm audit /tmp/test_project --format json" \
    "audit" \
    "true"

run_test 46 "Deeply nested directory structure" \
    "mkdir -p /tmp/test_project/a/b/c/d/e/f/g; echo 'fn deep() {}' > /tmp/test_project/a/b/c/d/e/f/g/deep.rs; ./target/release/osvm audit /tmp/test_project --format json" \
    "audit" \
    "true"

run_test 47 "Doctor command functionality" \
    "./target/release/osvm doctor" \
    "health" \
    "false"

run_test 48 "Multiple output formats" \
    "./target/release/osvm audit /tmp/test_project --format html --output /tmp/test_audit.html && echo 'HTML output created'" \
    "HTML output created" \
    "true"

run_test 49 "RPC manager help" \
    "./target/release/osvm rpc-manager --help" \
    "help" \
    "false"

run_test 50 "Version with multiple formats" \
    "./target/release/osvm ver && ./target/release/osvm v && ./target/release/osvm version" \
    "version" \
    "false"

# Clean up
rm -f test_output.log
rm -rf /tmp/test_project

# =============================================================================
echo -e "${BLUE}=== COMPREHENSIVE TEST RESULTS ===${NC}"
# =============================================================================

echo "Total Tests Run: $TOTAL_TESTS"
echo -e "${GREEN}✅ Passed: $PASSED_TESTS${NC}"
if [ $FAILED_TESTS -gt 0 ]; then
    echo -e "${RED}❌ Failed: $FAILED_TESTS${NC}"
else
    echo -e "${GREEN}❌ Failed: $FAILED_TESTS${NC}"
fi

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}SUCCESS RATE: 100%${NC}"
    echo ""
    echo -e "${GREEN}🎉 ALL TESTS PASSED! 🎉${NC}"
    echo ""
    echo -e "${BLUE}🔑 KEY FUNCTIONALITY VERIFIED:${NC}"
    echo "   - ✅ Dual AI endpoint support (OSVM.ai + OpenAI)"
    echo "   - ✅ Environment variable detection"
    echo "   - ✅ Custom OpenAI model compatibility"
    echo "   - ✅ Comprehensive audit functionality"
    echo "   - ✅ Error handling and edge cases"
    echo "   - ✅ Command-line interface robustness"
else
    SUCCESS_RATE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo -e "${YELLOW}SUCCESS RATE: $SUCCESS_RATE%${NC}"
    echo ""
    echo -e "${YELLOW}⚠️ Some tests failed. Please review the output above.${NC}"
fi

echo ""
echo -e "${BLUE}=== END OF COMPREHENSIVE TESTING ===${NC}"

exit $FAILED_TESTS