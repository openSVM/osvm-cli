#!/bin/bash

# Test OVSM validation with complex Solana queries

echo "Testing OVSM Validation for Complex Solana Queries"
echo "==================================================="
echo ""

# Test 1: Simple valid OVSM
echo "Test 1: Valid OVSM Syntax"
echo "(begin (tool-call \"get_balance\" (address \"test\")))" > /tmp/test1.ovsm
cargo run --bin osvm -- ovsm validate /tmp/test1.ovsm 2>&1 | grep -E "(✓|✗|Valid|Invalid|Error)"
echo ""

# Test 2: Invalid OVSM (unbalanced parens)
echo "Test 2: Invalid OVSM Syntax (unbalanced parens)"
echo "(begin (tool-call \"get_balance\" (address \"test\")" > /tmp/test2.ovsm
cargo run --bin osvm -- ovsm validate /tmp/test2.ovsm 2>&1 | grep -E "(✓|✗|Valid|Invalid|Error)"
echo ""

# Test 3: Complex nested OVSM
echo "Test 3: Complex Nested OVSM"
cat > /tmp/test3.ovsm << 'EOF'
(begin
  (let ((validators (tool-call "get_validators" (count 100))))
    (parallel
      (tool-call "analyze_stake_distribution" (data validators))
      (tool-call "check_commission_rates" (data validators))
      (tool-call "identify_anomalies" 
        (data validators)
        (metrics ["voting_patterns" "uptime" "delinquency"])))))
EOF
cargo run --bin osvm -- ovsm validate /tmp/test3.ovsm 2>&1 | grep -E "(✓|✗|Valid|Invalid|Error)"
echo ""

# Test 4: OVSM with conditional branching
echo "Test 4: OVSM with Conditional Branching"
cat > /tmp/test4.ovsm << 'EOF'
(begin
  (let ((account (tool-call "get_account_stats" (address "7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZse3W2UZHq"))))
    (if (> (get account "transaction_count") 1000)
      (sequential
        (tool-call "analyze_trading_patterns" (account account))
        (tool-call "check_wash_trading" (account account))
        (tool-call "find_scam_connections" (account account)))
      (tool-call "basic_account_summary" (account account)))))
EOF
cargo run --bin osvm -- ovsm validate /tmp/test4.ovsm 2>&1 | grep -E "(✓|✗|Valid|Invalid|Error)"
echo ""

echo "==================================================="
echo "Validation test complete!"
echo ""
echo "Note: The validation ensures that:"
echo "1. Parentheses are balanced"
echo "2. S-expression structure is valid"
echo "3. Tool calls follow proper format"
echo "4. Complex nested structures parse correctly"
