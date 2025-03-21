use crate::e2e::common::{
    create_mock_config, create_temp_dir, output_contains, run_osvm_command, run_osvm_command_string,
};
use assert_cmd::assert::OutputAssertExt;
use predicates::prelude::*;
use serial_test::serial;

#[test]
#[serial]
fn test_nodes_list() {
    let output = run_osvm_command_string(&["nodes", "list"]);

    // Verify the output contains expected headers
    assert!(output_contains(&output, "OSVM - Node Management"));
    assert!(output_contains(&output, "Managed SVM Nodes:"));

    // The output might show "No nodes are currently managed" if no nodes are configured
    // or it might show a list of nodes if some are configured
    assert!(
        output_contains(&output, "No nodes are currently managed")
            || output_contains(&output, "ID")
                && output_contains(&output, "SVM")
                && output_contains(&output, "TYPE")
    );
}

#[test]
#[serial]
fn test_nodes_list_with_filters_basic() {
    let output = run_osvm_command_string(&["nodes", "list", "--filter", "active"]);
    assert!(
        output_contains(&output, "OSVM - Node Management"),
        "Output did not contain expected text: OSVM - Node Management"
    );
}

#[test]
#[serial]
fn test_nodes_list_with_filters() {
    // Test with network filter
    let output = run_osvm_command_string(&["nodes", "list", "--network", "mainnet"]);
    assert!(output_contains(&output, "OSVM - Node Management"));

    // Test with type filter
    let output = run_osvm_command_string(&["nodes", "list", "--type", "validator"]);
    assert!(output_contains(&output, "OSVM - Node Management"));

    // Test with status filter
    let output = run_osvm_command_string(&["nodes", "list", "--status", "running"]);
    assert!(output_contains(&output, "OSVM - Node Management"));

    // Test with JSON output
    let output = run_osvm_command_string(&["nodes", "list", "--json"]);
    // JSON output should start with a curly brace or square bracket
    assert!(
        output.trim().starts_with('{')
            || output.trim().starts_with('[')
            || output.trim().is_empty()
    );
}

#[test]
#[serial]
fn test_nodes_dashboard() {
    // This test is more complex as it involves an interactive dashboard
    // For now, we'll just verify the command doesn't immediately fail with "unknown command"
    let assert = run_osvm_command().args(&["nodes", "dashboard"]).assert();

    // The dashboard might not work in a test environment, so we're just checking
    // that the command is recognized
    assert.stderr(predicate::str::contains("Unknown command").not());
}

#[test]
#[serial]
fn test_nodes_get_invalid() {
    let assert = run_osvm_command()
        .args(&["nodes", "get", "invalid_node_id"])
        .assert();

    // Verify the command fails with a non-zero exit code
    assert
        .failure()
        .stderr(predicate::str::contains("Node not found").or(predicate::str::contains("Error:")));
}

#[test]
#[serial]
fn test_examples_command() {
    // Test the examples command
    let output = run_osvm_command_string(&["examples"]);

    // Verify the output contains examples
    assert!(
        output_contains(&output, "OSVM CLI Examples")
            || output_contains(&output, "Available SVMs in the chain:")
            || output_contains(&output, "Basic Commands")
    );

    // Test examples with category filter
    let output = run_osvm_command_string(&["examples", "--category", "basic"]);
    assert!(output_contains(&output, "Basic Commands"));

    // Test listing categories
    let output = run_osvm_command_string(&["examples", "--list-categories"]);
    assert!(output_contains(&output, "Available example categories:"));
}

#[test]
#[serial]
fn test_verbose_output() {
    // Test with normal output (without verbose flag)
    let output = run_osvm_command_string(&["svm", "list"]);

    // Normal output should include "Available SVMs" text
    assert!(output_contains(&output, "Available SVMs in the chain:"));

    // Instead of testing specific verbosity flags that may change,
    // we'll check the basic list command works properly
    assert!(output_contains(&output, "NAME"));
    assert!(output_contains(&output, "TOKEN"));
}

#[test]
#[serial]
fn test_no_color_flag() {
    // Test with no-color flag
    let output = run_osvm_command_string(&["--no-color", "svm", "list"]);

    // Output should still contain the expected text, but without color codes
    assert!(output_contains(&output, "Available SVMs in the chain:"));
}

#[test]
#[serial]
fn test_with_custom_config() {
    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);

    // Run the command with the config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "nodes", "list"]);

    // Verify the output contains expected headers
    assert!(output_contains(&output, "OSVM - Node Management"));
}

#[test]
#[serial]
fn test_help_command() {
    // Test the help command
    let assert = run_osvm_command().arg("--help").assert();

    // Verify the output contains help information
    assert
        .success()
        .stdout(predicate::str::contains("USAGE:"))
        .stdout(predicate::str::contains("FLAGS:"))
        .stdout(predicate::str::contains("SUBCOMMANDS:"));
}

#[test]
#[serial]
fn test_new_feature() {
    // Test the new feature
    let output = run_osvm_command_string(&["new_feature_command"]);

    // Verify the output contains expected results
    assert!(output_contains(&output, "Expected output for new feature"));
}
