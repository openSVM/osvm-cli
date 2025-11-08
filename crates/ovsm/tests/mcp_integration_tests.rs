//! Integration tests for OVSM with MCP (Model Context Protocol) tools
//!
//! These tests verify that OVSM scripts can correctly interact with
//! blockchain RPC endpoints via MCP tool integration.
//!
//! Note: These tests require a running Solana RPC endpoint (mainnet or devnet)
//! and are marked as `#[ignore]` by default. Run with `cargo test -- --ignored`

use ovsm::{Evaluator, Parser, Scanner, Value};

fn eval_lisp(source: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    let mut evaluator = Evaluator::new();
    Ok(evaluator.execute(&program)?)
}

// ====================
// Basic RPC Operations
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_slot() {
    let source = r#"
;; Get current slot from Solana RPC
(getSlot)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return an integer slot number
    assert!(matches!(result, Value::Int(_)));
    if let Value::Int(slot) = result {
        assert!(slot > 0, "Slot should be positive");
    }
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_block_height() {
    let source = r#"
;; Get current block height
(getBlockHeight)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(_)));
    if let Value::Int(height) = result {
        assert!(height > 0, "Block height should be positive");
    }
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_version() {
    let source = r#"
;; Get Solana version info
(getVersion)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return an object with version info
    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Account Operations
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_balance() {
    let source = r#"
;; Get balance for a known account (Solana Foundation)
(define pubkey "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(getBalance :address pubkey)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return a number (lamports)
    assert!(matches!(result, Value::Int(_)) || matches!(result, Value::Float(_)));
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_account_info() {
    let source = r#"
;; Get account info for a known account
(define pubkey "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(getAccountInfo :address pubkey)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return an object with account data
    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Transaction Queries
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_signatures_for_address() {
    let source = r#"
;; Get recent signatures for an active address
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(getSignaturesForAddress :address wallet :limit 5)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return an array of signature objects
    assert!(matches!(result, Value::Array(_)));
    if let Value::Array(sigs) = result {
        assert!(sigs.len() <= 5, "Should respect limit parameter");
    }
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_transaction() {
    let source = r#"
;; First get a recent signature, then fetch that transaction
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 1))
(define first_sig (first sigs))
(define signature (. first_sig signature))

(getTransaction :signature signature)
"#;
    let result = eval_lisp(source).unwrap();

    // Should return transaction object
    assert!(matches!(result, Value::Object(_)));
}

// ====================
// Complex Queries
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_count_recent_transactions() {
    let source = r#"
;; Count transactions for an address in the last N signatures
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define limit 10)

(define sigs (getSignaturesForAddress :address wallet :limit limit))
(length sigs)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(_)));
    if let Value::Int(count) = result {
        assert!(count >= 0 && count <= 10);
    }
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_filter_successful_transactions() {
    let source = r#"
;; Get only successful (non-error) transactions
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define all_sigs (getSignaturesForAddress :address wallet :limit 20))

;; Filter for transactions without errors
(define successful
  (filter
    (lambda (sig) (null? (. sig err)))
    all_sigs))

(length successful)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(_)));
    if let Value::Int(count) = result {
        assert!(count >= 0 && count <= 20);
    }
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_get_transaction_details() {
    let source = r#"
;; Get detailed info from a transaction
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 1))

(if (> (length sigs) 0)
    (do
      (define first_sig (first sigs))
      (define sig_str (. first_sig signature))
      (define tx (getTransaction :signature sig_str))

      ;; Extract slot from transaction
      (. tx slot))
    null)
"#;
    let result = eval_lisp(source).unwrap();

    // Should either be a slot number or null if no transactions
    assert!(matches!(result, Value::Int(_)) || matches!(result, Value::Null));
}

// ====================
// Time-based Queries
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_recent_activity_time_filter() {
    let source = r#"
;; Get transactions from the last hour
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define hour_ago (- (now) 3600))

(define all_sigs (getSignaturesForAddress :address wallet :limit 100))

;; Filter by blockTime
(define recent
  (filter
    (lambda (sig)
      (>= (. sig blockTime) hour_ago))
    all_sigs))

(length recent)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(_)));
}

// ====================
// Aggregation Tests
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_transaction_count_by_status() {
    let source = r#"
;; Count successful vs failed transactions
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 50))

(define successful_count
  (length
    (filter (lambda (sig) (null? (. sig err))) sigs)))

(define failed_count
  (length
    (filter (lambda (sig) (not (null? (. sig err)))) sigs)))

;; Return object with counts
{:successful successful_count :failed failed_count :total (length sigs)}
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Object(_)));
    if let Value::Object(obj) = result {
        assert!(obj.contains_key(&"successful".to_string()));
        assert!(obj.contains_key(&"failed".to_string()));
        assert!(obj.contains_key(&"total".to_string()));
    }
}

// ====================
// Error Handling
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_invalid_address_handling() {
    let source = r#"
;; Try to get balance for invalid address
(try
  (getBalance :address "invalid_address_format")
  (catch err
    "Error caught"))
"#;
    let result = eval_lisp(source).unwrap();

    // Should handle the error gracefully
    assert!(matches!(result, Value::String(_)));
}

// ====================
// Pagination Tests
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_pagination_with_before() {
    let source = r#"
;; Test pagination by fetching two batches
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")

;; First batch
(define batch1 (getSignaturesForAddress :address wallet :limit 10))

;; Second batch using 'before' cursor
(if (> (length batch1) 0)
    (do
      (define last_sig (. (last batch1) signature))
      (define batch2 (getSignaturesForAddress
                       :address wallet
                       :limit 10
                       :before last_sig))
      (length batch2))
    0)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(_)));
}

// ====================
// Multi-Account Queries
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_compare_multiple_wallets() {
    let source = r#"
;; Compare transaction counts for multiple wallets
(define wallets [
  "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r"
  "Vote111111111111111111111111111111111111111"
])

(define counts
  (map
    (lambda (wallet)
      (define sigs (getSignaturesForAddress :address wallet :limit 5))
      {:wallet wallet :count (length sigs)})
    wallets))

(length counts)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Int(2)));
}

// ====================
// Performance Tests
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_bulk_signature_fetch() {
    let source = r#"
;; Fetch a large batch of signatures efficiently
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define batch_size 100)

(define sigs (getSignaturesForAddress :address wallet :limit batch_size))

;; Verify we got results and they're in order
(if (> (length sigs) 1)
    (do
      (define first (first sigs))
      (define second (nth 1 sigs))

      ;; Signatures should be ordered by slot (descending)
      (>= (. first slot) (. second slot)))
    true)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Bool(true)));
}

// ====================
// Data Validation Tests
// ====================

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_validate_signature_format() {
    let source = r#"
;; Validate that signatures have expected format
(define wallet "GBBbhHigLDyfxV71A24vGM2JLFukdChaNDTXGED1mF5r")
(define sigs (getSignaturesForAddress :address wallet :limit 1))

(if (> (length sigs) 0)
    (do
      (define sig (first sigs))

      ;; Check required fields exist
      (define has_signature (not (null? (. sig signature))))
      (define has_slot (not (null? (. sig slot))))
      (define has_blocktime (not (null? (. sig blockTime))))

      (and has_signature (and has_slot has_blocktime)))
    true)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Bool(true)));
}

#[test]
#[ignore] // Requires live RPC connection
fn test_mcp_slot_progression() {
    let source = r#"
;; Verify that slot numbers are progressing
(define current_slot (getSlot))

;; Wait briefly and check again (simulated)
(define later_slot (getSlot))

;; Slots should be monotonically increasing or equal
(>= later_slot current_slot)
"#;
    let result = eval_lisp(source).unwrap();

    assert!(matches!(result, Value::Bool(true)));
}
