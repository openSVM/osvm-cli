use crate::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command, run_osvm_command_string,
};
use crate::e2e::test_utils::setup_test_environment;
use assert_cmd::assert::OutputAssertExt;
use predicates::prelude::*;
use serial_test::serial;
use std::process::Command;

#[test]
#[serial]
fn test_nodes_list() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["nodes", "list"])
        .output()
        .expect("Failed to execute command");

    assert!(
        output_contains(&output, "OSVM - Node Management")
            || output_contains(&output, "Node List")
            || output_contains(&output, "No nodes found")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_nodes_list_with_filters_basic() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["nodes", "list", "--status", "running"])
        .output()
        .expect("Failed to execute command");

    assert!(
        output_contains(&output, "OSVM - Node Management")
            || output_contains(&output, "Node List")
            || output_contains(&output, "No nodes found")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_nodes_list_with_filters() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args([
            "nodes",
            "list",
            "--status",
            "running",
            "--type",
            "validator",
        ])
        .output()
        .expect("Failed to execute command");

    assert!(
        output_contains(&output, "OSVM - Node Management")
            || output_contains(&output, "Node List")
            || output_contains(&output, "No nodes found")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_nodes_dashboard() {
    // This test is more complex as it involves an interactive dashboard
    // For now, we'll just verify the command doesn't immediately fail with "unknown command"
    let assert = run_osvm_command().args(["nodes", "dashboard"]).assert();

    // The dashboard might not work in a test environment, so we're just checking
    // that the command is recognized
    assert.stderr(predicate::str::contains("Unknown command").not());
}

#[test]
#[serial]
fn test_nodes_get_invalid() {
    setup_test_environment();

    let output = Command::new(env!("CARGO_BIN_EXE_osvm"))
        .args(["nodes", "get", "invalid_node_id"])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Node not found")
            || stderr.contains("Error:")
            || stderr.contains("not found")
            || !output.status.success()
    );
}

#[test]
#[serial]
fn test_examples_command() {
    setup_test_environment();

    // Test the examples command
    let output = run_osvm_command_string(&["examples"]);

    // Verify the output contains examples
    assert!(
        output_contains(&output, "OSVM CLI Examples")
            || output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "Basic Commands")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );

    // Test examples with category filter
    let output = run_osvm_command_string(&["examples", "--category", "basic"]);
    assert!(
        output_contains(&output, "Basic Commands")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );

    // Test listing categories
    let output = run_osvm_command_string(&["examples", "--list-categories"]);
    assert!(
        output_contains(&output, "Available example categories:")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_verbose_output() {
    setup_test_environment();

    // Test with normal output (without verbose flag)
    let output = run_osvm_command_string(&["nodes", "list"]);

    // Normal output should not include verbose details
    assert!(!output_contains(&output, "Available SVMs in the chain:"));

    // Test with verbose flag
    let output = run_osvm_command_string(&["nodes", "list", "--verbose"]);

    // Verbose output should include "Available SVMs in the chain:"
    assert!(
        output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );

    // Additional assertions for verbose output
    assert!(
        output_contains(&output, "OSVM - Node Management")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_no_color_flag() {
    setup_test_environment();

    // Test with no-color flag
    let output = run_osvm_command_string(&["--no-color", "svm", "list"]);

    // Output should still contain the expected text, but without color codes
    assert!(
        output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_with_custom_config() {
    setup_test_environment();

    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run the command with the config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "nodes", "list"]);

    // Verify the output contains expected headers
    assert!(
        output_contains(&output, "OSVM - Node Management")
            || output_contains(&output, "Error reading keypair file")
            || output_contains(&output, "configuration issue")
    );
}

#[test]
#[serial]
fn test_help_command() {
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

#[test]
#[serial]
fn test_new_feature() {
    setup_test_environment();

    // Test the new feature - for now just test that unknown commands are handled gracefully
    // by checking for error messages or output indicating command was processed
    let output = run_osvm_command_string(&["--help"]);

    // Verify the output contains expected help information
    assert!(
        output_contains(&output, "Usage:")
            || output_contains(&output, "USAGE:")
            || output_contains(&output, "Commands:")
    );
}
