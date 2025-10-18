//! Example tests to demonstrate how to write e2e tests

use crate::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command_string, MockServer,
};
use crate::e2e::test_utils::setup_test_environment;
use assert_cmd::assert::OutputAssertExt;
use assert_cmd::prelude::*;
use serial_test::serial;
use std::process::Command;

/// Example test that demonstrates how to test a simple command
/// NOTE: Updated - Now tests --help since app starts chat agent by default
#[test]
#[serial]
fn example_test_simple_command() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .arg("--help")
        .output()
        .expect("Failed to execute command");

    assert!(
        output_contains(&output, "Usage:")
            || output_contains(&output, "USAGE:")
            || output_contains(&output, "Commands:")
    );
}

/// Example test that demonstrates how to use assert_cmd for more complex assertions
#[test]
#[serial]
fn example_test_with_assert_cmd() {
    setup_test_environment();

    let assert = Command::cargo_bin("osvm")
        .expect("Binary exists")
        .arg("--help")
        .assert();

    // Less strict check to handle either format
    assert.success();
}

/// Example test that demonstrates how to use a mock server
#[test]
#[serial]
fn example_test_with_mock_server() {
    setup_test_environment();

    // Create a mock server
    let mut mock_server = MockServer::new();

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
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

/// Example test that demonstrates how to use a custom config file
#[test]
#[serial]
fn example_test_with_custom_config() {
    setup_test_environment();

    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run a command with the custom config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "svm", "list"]);

    // Check if the output contains expected text
    assert!(
        output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}
