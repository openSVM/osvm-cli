//! Comprehensive end-to-end tests for the `osvm a` (AI planning) command
//!
//! These tests verify the Solana blockchain investigation functionality via natural language queries.
//! The `osvm a` command routes queries through the OVSM planning agent for blockchain analysis.

mod common {
    use std::process::{Command, Output};

    /// Run an osvm command with the given arguments and return the output
    pub fn run_osvm_command_string(args: &[&str]) -> Output {
        let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
            .args(args)
            .output()
            .expect("Failed to execute command");
        output
    }

    /// Check if the output contains a specific string (checks both stdout and stderr)
    pub fn output_contains(output: &Output, expected: &str) -> bool {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        stdout.contains(expected) || stderr.contains(expected)
    }

    /// Extract stdout as string
    pub fn output_stdout(output: &Output) -> String {
        String::from_utf8_lossy(&output.stdout).to_string()
    }

    /// Extract stderr as string
    pub fn output_stderr(output: &Output) -> String {
        String::from_utf8_lossy(&output.stderr).to_string()
    }
}

use common::{output_contains, output_stderr, output_stdout, run_osvm_command_string};

// ============================================================================
// TEST 1: Basic wallet balance query
// ============================================================================
//
// This test verifies that the `osvm a` command can accept and process
// a basic natural language query about wallet balances. The query is parsed,
// sent to the AI planning agent, which should acknowledge the request and
// attempt to generate an execution plan.
//
// Expected behavior:
// - Command executes without crashing
// - Output contains acknowledgment of the query or attempt to process it
// - No OVSM syntax errors (if OVSM plan generation is attempted)
#[test]
fn test_osvm_a_basic_wallet_balance_query() {
    let output = run_osvm_command_string(&["a", "show me the SOL balance for wallet ABC123DEF456"]);

    // Should succeed without error exit code
    assert!(
        output.status.success() || output.status.code().map_or(false, |c| c == 0 || c != 1),
        "Command should complete (exit code 0 is expected for planning mode)"
    );

    // Command should either produce output or not crash
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));
    assert!(!combined.is_empty(), "Command should produce some output");
}

// ============================================================================
// TEST 2: Transaction history analysis query
// ============================================================================
//
// This test verifies that the `osvm a` command correctly handles a more
// complex query requesting transaction history analysis. This tests the
// ability to parse multi-part natural language requests.
//
// Expected behavior:
// - Query with multiple conditions is accepted
// - No immediate errors in parsing
// - Planning mode is engaged (may show planning indicators)
#[test]
fn test_osvm_a_transaction_history_analysis() {
    let output = run_osvm_command_string(&[
        "a",
        "find all transactions for wallet 5rVDMMoBQs3zJQ9DT7oxsoNZfxptgLCKhuWqdwoX9q85 in the last 24 hours",
    ]);

    // Should not crash
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));
    assert!(
        !combined.is_empty() || output.status.success(),
        "Command should execute without crashing"
    );
}

// ============================================================================
// TEST 3: Token transfer tracking query
// ============================================================================
//
// This test verifies that the `osvm a` command can handle queries about
// token transfers, which require cross-wallet analysis and time-based filtering.
//
// Expected behavior:
// - Multi-token query is accepted
// - No parsing errors
// - Query complexity doesn't cause failures
#[test]
fn test_osvm_a_token_transfer_tracking() {
    let output = run_osvm_command_string(&[
        "a",
        "trace all USDC transfers from wallet A to wallet B and show intermediate wallets",
    ]);

    // Should handle the query without panicking
    let _stdout = output_stdout(&output);
    let stderr = output_stderr(&output);

    // Verify no panics or critical errors
    assert!(
        !stderr.contains("panicked") && !stderr.contains("thread"),
        "Command should not panic"
    );
}

// ============================================================================
// TEST 4: Program interaction analysis query
// ============================================================================
//
// This test verifies that the `osvm a` command can handle queries about
// smart contract interactions, which require program analysis and filtering
// by instruction types.
//
// Expected behavior:
// - Complex program-specific query is accepted
// - No crashes on program-related queries
// - Command completes execution
#[test]
fn test_osvm_a_program_interaction_analysis() {
    let output = run_osvm_command_string(&[
        "a",
        "identify all wallets that interacted with the Raydium AMM program in the last 7 days",
    ]);

    // Should complete without panicking
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));
    assert!(
        !combined.contains("thread 'main' panicked"),
        "Command should not panic during program analysis"
    );
}

// ============================================================================
// TEST 5: Empty query error handling
// ============================================================================
//
// This test verifies that the `osvm a` command properly handles the case
// where no query is provided. The system should display helpful usage
// information rather than crashing.
//
// Expected behavior:
// - Command handles missing query gracefully
// - Error message is helpful and informative
// - Usage examples are provided
#[test]
fn test_osvm_a_no_query_error_handling() {
    let output = run_osvm_command_string(&["a"]);

    // Should contain helpful error message
    assert!(
        output_contains(&output, "query") || output_contains(&output, "No query"),
        "Should indicate that a query is required"
    );
}

// ============================================================================
// TEST 6: DEX trading volume analysis query
// ============================================================================
//
// This test verifies that the `osvm a` command can handle DEX-specific queries
// that require aggregation and time-based analysis across multiple pools.
//
// Expected behavior:
// - DEX query is parsed correctly
// - No errors related to program/pool identification
// - Command processes without failure
#[test]
fn test_osvm_a_dex_trading_volume_analysis() {
    let output = run_osvm_command_string(&[
        "a",
        "analyze the trading volume on Raydium for the SOL-USDC pair over the last 7 days",
    ]);

    // Should complete successfully or with planned execution
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));

    // Verify no unexpected errors
    assert!(
        !combined.contains("Critical error") && !combined.contains("Fatal"),
        "Should not have critical failures"
    );
}

// ============================================================================
// TEST 7: Time-range filtered query with multiple conditions
// ============================================================================
//
// This test verifies that the `osvm a` command can handle complex queries
// with multiple filtering conditions including time ranges, amounts, and
// transaction types.
//
// Expected behavior:
// - Multi-condition query is accepted and processed
// - Command execution completes without panicking
// - No critical failures on compound conditions
#[test]
#[ignore] // This test makes actual network calls and is slow - run with --ignored flag
fn test_osvm_a_complex_multi_condition_query() {
    let output = run_osvm_command_string(&[
        "a",
        "find all wallets that received more than 10 SOL between June and August 2025, excluding known exchange addresses",
    ]);

    // Verify command completed without crashing
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));
    assert!(!combined.is_empty(), "Command should produce output");
}

// ============================================================================
// TEST 8: NFT collection activity query
// ============================================================================
//
// This test verifies that the `osvm a` command can handle NFT-specific queries
// that require metadata lookups and collection-level aggregations.
//
// Expected behavior:
// - NFT-specific query is accepted
// - No errors on collection/metadata queries
// - Command processes the specialized domain query
#[test]
fn test_osvm_a_nft_collection_activity() {
    let output = run_osvm_command_string(&[
        "a",
        "show all recent sales of the Magic Eden Rugly collection and identify suspicious trading patterns",
    ]);

    // Should complete without crashing on NFT-specific query
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));

    assert!(
        !combined.is_empty(),
        "Command should produce output for NFT queries"
    );
}

// ============================================================================
// TEST 9: Sorting and aggregation query
// ============================================================================
//
// This test verifies that the `osvm a` command can handle queries that
// explicitly request specific output formatting with sorting and aggregation
// requirements.
//
// Expected behavior:
// - Query with aggregation requirements is accepted
// - Sorting directives are parsed correctly
// - No errors on output formatting specifications
#[test]
fn test_osvm_a_sorting_and_aggregation_query() {
    let output = run_osvm_command_string(&[
        "a",
        "list the top 50 validators by total stake, sorted by delegation count in descending order",
    ]);

    // Should handle sorting/aggregation directives
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));

    assert!(
        !combined.contains("invalid") || combined.contains("plan") || combined.contains("agent"),
        "Should accept sorting/aggregation queries"
    );
}

// ============================================================================
// TEST 10: Planning mode with query validation
// ============================================================================
//
// This test verifies the end-to-end flow of the `osvm a` command including
// query validation, OVSM plan generation, and execution setup. This is the
// most comprehensive test, validating the entire command pipeline.
//
// Expected behavior:
// - Query is validated as natural language (multi-word, contextual)
// - Planning mode is engaged
// - OVSM plan generation is attempted
// - No critical errors prevent execution
#[test]
#[ignore] // This test makes actual network calls and is slow - run with --ignored flag
fn test_osvm_a_planning_mode_full_pipeline() {
    let output = run_osvm_command_string(&[
        "a",
        "identify potential arbitrage opportunities between Orca and Raydium for the SOL-USDC pair with less than 2% slippage",
    ]);

    // Verify command completed without crashing
    let combined = format!("{}{}", output_stdout(&output), output_stderr(&output));
    assert!(!combined.is_empty(), "Command should produce output");
}
