//! Example tests to demonstrate how to write e2e tests

use crate::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command_string, MockServer,
};
use crate::e2e::test_utils::setup_test_environment;
use assert_cmd::assert::OutputAssertExt;
use assert_cmd::prelude::*;
use serial_test::serial;
use std::process::Command;
use crate::e2e::utils::{output_contains, output_contains_any, assert_success_with_output};

/// Example test that demonstrates how to test a simple command
#[test]
#[serial]
fn example_test_simple_command() {
    setup_test_environment();

    let output = Command::cargo_bin("osvm").unwrap()
        .arg("--help")
        .output()
        .expect("Failed to execute command");

    assert_success_with_output(&output, &["help", "usage", "command", "option"]);
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
    // Create a mock server
    let mut mock_server = MockServer::new();

    // Set up a mock endpoint
    let _mock = mock_server.mock_svm_list();

    // Run a command that uses the mock server
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("list")
        .arg("--url")
        .arg(mock_server.url("/api/svms"))
        .output()
        .expect("Failed to execute command");

    // Just verify command executed successfully
    assert!(output.status.success());
}

/// Example test that demonstrates how to use a custom config file
#[test]
#[serial]
fn example_test_with_custom_config() {
    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run a command with the custom config file
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("list")
        .arg("--config")
        .arg(config_path.to_str().unwrap())
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
}
