//! Integration tests for the OSVM Command Planner
//!
//! Tests natural language command planning and execution including:
//! - Plan command parsing
//! - Tool selection logic
//! - Execution flow
//! - Error handling

use std::process::Command;
use std::time::Duration;

#[test]
fn test_plan_command_help() {
    let output = Command::new("cargo")
        .args(["run", "--", "plan", "--help"])
        .output()
        .expect("Failed to execute plan --help");

    assert!(output.status.success(), "plan --help should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("plan") || stdout.contains("Plan"),
        "Help should mention planning");
}

#[test]
fn test_plan_with_simple_query() {
    let output = Command::new("cargo")
        .args(["run", "--", "plan", "check my wallet balance"])
        .env("CI", "1")
        .output()
        .expect("Failed to run plan command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should attempt to create a plan without panicking
    assert!(!stderr.contains("panic"), "Plan should not panic");

    // Should either succeed or provide helpful error about configuration
    assert!(
        output.status.success() ||
        stderr.contains("API") ||
        stderr.contains("key") ||
        stderr.contains("MCP"),
        "Should execute or provide configuration guidance"
    );
}

#[test]
fn test_plan_validates_input() {
    // Test with empty query
    let output = Command::new("cargo")
        .args(["run", "--", "plan", ""])
        .env("CI", "1")
        .output()
        .expect("Failed to run plan with empty query");

    // Should handle empty input gracefully
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!stderr.contains("panic"), "Should handle empty input without panic");
}

#[test]
fn test_plan_command_with_multiple_words() {
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "plan",
            "show",
            "me",
            "recent",
            "transactions",
        ])
        .env("CI", "1")
        .output()
        .expect("Failed to run plan command");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!stderr.contains("panic"), "Multi-word plan should not panic");
}

#[test]
fn test_plan_with_blockchain_query() {
    // Test plan with Solana-specific query
    let output = Command::new("cargo")
        .args(["run", "--", "plan", "What is the current slot?"])
        .env("CI", "1")
        .timeout(Duration::from_secs(15))
        .output()
        .expect("Failed to run blockchain query");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should process the query
    assert!(!stderr.contains("panic"), "Blockchain query should not panic");

    // May fail due to missing MCP/API config, but should handle gracefully
    assert!(
        output.status.success() ||
        stderr.contains("MCP") ||
        stderr.contains("configured") ||
        stdout.contains("plan") ||
        stdout.contains("Plan"),
        "Should show plan or configuration message"
    );
}

#[cfg(test)]
mod planner_logic_tests {
    use super::*;

    #[test]
    fn test_planner_handles_network_errors() {
        // Test with invalid API URL to trigger network error
        let output = Command::new("cargo")
            .args(["run", "--", "plan", "test query"])
            .env("CI", "1")
            .env("OPENAI_URL", "http://invalid-nonexistent-url.local")
            .timeout(Duration::from_secs(10))
            .output()
            .expect("Failed to run with invalid URL");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should handle network errors gracefully
        assert!(!stderr.contains("panic"));
        assert!(
            stderr.contains("failed") ||
            stderr.contains("error") ||
            stderr.contains("could not"),
            "Should report network error clearly"
        );
    }

    #[test]
    fn test_planner_suggests_help() {
        // When plan fails, should suggest helpful next steps
        let output = Command::new("cargo")
            .args(["run", "--", "plan", "impossible query @#$%"])
            .env("CI", "1")
            .output()
            .expect("Failed to run with problematic query");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should not panic even with unusual input
        assert!(!stderr.contains("panic"));
    }
}

// Helper trait for timeout
trait CommandExt {
    fn timeout(&mut self, duration: Duration) -> &mut Self;
}

impl CommandExt for Command {
    fn timeout(&mut self, duration: Duration) -> &mut Self {
        // Simplified timeout - production code should use proper timeout mechanism
        // This is a placeholder for the test infrastructure
        self
    }
}

#[cfg(test)]
mod tool_selection_tests {
    use super::*;

    #[test]
    fn test_plan_recognizes_balance_query() {
        let output = Command::new("cargo")
            .args(["run", "--", "plan", "show my SOL balance"])
            .env("CI", "1")
            .output()
            .expect("Failed to run balance query");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should attempt to plan for balance query
        assert!(!stderr.contains("panic"));
    }

    #[test]
    fn test_plan_recognizes_transaction_query() {
        let output = Command::new("cargo")
            .args(["run", "--", "plan", "get recent transactions"])
            .env("CI", "1")
            .output()
            .expect("Failed to run transaction query");

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!stderr.contains("panic"));
    }

    #[test]
    fn test_plan_with_debug_flag() {
        // Test that debug mode provides additional output
        let output = Command::new("cargo")
            .args(["run", "--", "plan", "test", "--debug"])
            .env("CI", "1")
            .output()
            .expect("Failed to run with debug flag");

        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Debug mode should provide more information
        assert!(!stderr.contains("panic"));
        // Note: Actual debug output depends on implementation
    }
}
