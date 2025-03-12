//! End-to-end tests for SVM-related commands

use crate::tests::e2e::common::{output_contains, run_osvm_command, run_osvm_command_string};

#[test]
fn test_svm_list() {
    let output = run_osvm_command_string(&["svm", "list"]);

    // Verify the output contains expected headers
    assert!(output_contains(&output, "Available SVMs in the chain:"));
    assert!(output_contains(&output, "NAME"));
    assert!(output_contains(&output, "DISPLAY NAME"));
    assert!(output_contains(&output, "TOKEN"));

    // Verify the output contains some expected SVMs
    assert!(output_contains(&output, "solana"));
    assert!(output_contains(&output, "sonic"));
    assert!(output_contains(&output, "opensvm"));
}

#[test]
fn test_svm_get_solana() {
    let output = run_osvm_command_string(&["svm", "get", "solana"]);

    // Verify the output contains expected Solana information
    assert!(output_contains(&output, "SVM Information: Solana"));
    assert!(output_contains(&output, "Token: SOL"));
    assert!(output_contains(&output, "Website: https://solana.com"));

    // Verify network information is present
    assert!(output_contains(&output, "MAINNET Network:"));
    assert!(output_contains(&output, "TESTNET Network:"));
    assert!(output_contains(&output, "DEVNET Network:"));

    // Verify system requirements are present
    assert!(output_contains(&output, "Validator Requirements:"));
    assert!(output_contains(&output, "RPC Node Requirements:"));
}

#[test]
fn test_svm_get_invalid() {
    let output = run_osvm_command(&["svm", "get", "invalid_svm"]);

    // Verify the command fails with a non-zero exit code
    assert!(!output.status.success());

    // Verify the error message
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("SVM not found") || error.contains("Error:"));
}

#[test]
#[serial]
fn test_svm_dashboard() {
    // This test is more complex as it involves an interactive dashboard
    // For now, we'll just verify the command doesn't immediately fail with "unknown command"
    let assert = run_osvm_command().args(&["svm", "dashboard"]).assert();

    // The dashboard might not work in a test environment, so we're just checking
    // that the command is recognized
    assert.stderr(predicate::str::contains("Unknown command").not());
}

#[test]
#[serial]
fn test_svm_with_config_file() {
    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run the command with the config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "svm", "list"]);

    // Verify the output contains expected headers
    assert!(output_contains(&output, "Available SVMs in the chain:"));
}

#[test]
#[serial]
fn test_svm_with_url() {
    // Create a mock server
    let mock_server = MockServer::new();
    let _mock = mock_server.mock_svm_list();

    // Run the command with a custom URL
    let output = run_osvm_command_string(&[
        "--url",
        &format!("http://{}", mock_server.server.host_with_port()),
        "svm",
        "list",
    ]);

    // Verify the output contains expected headers
    assert!(output_contains(&output, "Available SVMs in the chain:"));
}
