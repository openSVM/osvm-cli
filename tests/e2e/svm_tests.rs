use crate::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command,
    run_osvm_command_string, MockServer,
};
use crate::e2e::test_utils::setup_test_environment;
use assert_cmd::assert::OutputAssertExt;
use predicates::prelude::*;
use serial_test::serial;
use std::process::Command;

#[allow(dead_code)]
fn run_command(program: &str, subcommand: &str, action: &str, argument: &str) -> String {
    let output = Command::new(program)
        .arg(subcommand)
        .arg(action)
        .arg(argument)
        .output()
        .expect("Failed to execute command");

    String::from_utf8_lossy(&output.stdout).to_string()
}

#[test]
fn test_svm_list() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(&["svm", "list"])
        .output()
        .expect("Failed to execute command");

    assert!(
        output_contains(&output, "Available SVMs")
            || output_contains(&output, "SVM List")
            || output_contains(&output, "No SVMs found")
    );
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
    setup_test_environment();

    let result = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(&["svm", "get", "invalid_svm"])
        .output();

    if let Err(e) = &result {
        panic!("Failed to execute command: {}", e);
    }

    let output = result.expect("Failed to execute command");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success()
            || stderr.contains("SVM not found")
            || stderr.contains("Error:")
            || stderr.contains("not found")
    );
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
    let mut mock_server = MockServer::new();
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

#[test]
fn test_svm_install() {
    let output = run_osvm_command_string(&["svm", "install", "solana", "--host", "user@host"]);

    // Verify the output contains expected installation message
    assert!(output_contains(&output, "Installing SVM: solana"));
    assert!(output_contains(&output, "Host: user@host"));
    assert!(output_contains(&output, "Installation complete"));
}

#[test]
fn test_svm_install_invalid() {
    // Use assert_cmd to run a command and make assertions about the output
    let assert = run_osvm_command()
        .args(&["svm", "install", "invalid_svm", "--host", "user@host"])
        .assert();

    // Verify the command fails with a non-zero exit code
    assert
        .failure()
        .stderr(predicate::str::contains("SVM not found").or(predicate::str::contains("Error:")));
}
