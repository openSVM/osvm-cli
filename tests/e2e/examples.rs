//! Example tests to demonstrate how to write e2e tests

use crate::tests::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command,
    run_osvm_command_string, MockServer,
};
use predicates::prelude::*;
use serial_test::serial;

/// Example test that demonstrates how to test a simple command
#[test]
#[serial]
fn example_test_simple_command() {
    // Run a command and get the output as a string
    let output = run_osvm_command_string(&["--help"]);

    // Check if the output contains expected text
    assert!(output_contains(&output, "USAGE:"));
    assert!(output_contains(&output, "FLAGS:"));
    assert!(output_contains(&output, "SUBCOMMANDS:"));
}

/// Example test that demonstrates how to use assert_cmd for more complex assertions
#[test]
#[serial]
fn example_test_with_assert_cmd() {
    // Use assert_cmd to run a command and make assertions about the output
    let assert = run_osvm_command().arg("--help").assert();

    // Make assertions about the command output
    assert
        .success()
        .stdout(predicate::str::contains("USAGE:"))
        .stdout(predicate::str::contains("FLAGS:"))
        .stdout(predicate::str::contains("SUBCOMMANDS:"));
}

/// Example test that demonstrates how to use a mock server
#[test]
#[serial]
fn example_test_with_mock_server() {
    // Create a mock server
    let mock_server = MockServer::new();

    // Set up a mock endpoint
    let _mock = mock_server.mock_svm_list();

    // Run a command that uses the mock server
    let output = run_osvm_command_string(&[
        "--url",
        &format!("http://{}", mock_server.server.host_with_port()),
        "svm",
        "list",
    ]);

    // Check if the output contains expected text
    assert!(
        output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "NAME")
    );
}

/// Example test that demonstrates how to use a custom config file
#[test]
#[serial]
fn example_test_with_custom_config() {
    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run a command with the custom config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "svm", "list"]);

    // Check if the output contains expected text
    assert!(output_contains(&output, "Available SVMs in the chain:"));
}
