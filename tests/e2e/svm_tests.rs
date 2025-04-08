use crate::e2e::common::{
    create_mock_config, create_temp_dir, run_osvm_command, run_osvm_command_string, MockServer,
};
use crate::e2e::test_utils::setup_test_environment;
use crate::e2e::utils::{output_contains, output_contains_any, assert_success_with_output};
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

    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("list")
        .output()
        .expect("Failed to execute command");

    assert_success_with_output(&output, &["svm", "list", "available", "found", "no svm"]);
}

#[test]
fn test_svm_get_solana() {
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("get")
        .arg("solana")
        .output()
        .expect("Failed to execute command");

    assert_success_with_output(&output, &["solana", "version", "info", "detail"]);
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

    assert_success_with_output(&output, &["svm", "list"]);
}

#[test]
#[serial]
fn test_svm_with_url() {
    // Create a mock server
    let mut mock_server = MockServer::new();
    let _mock = mock_server.mock_svm_list();

    // Run the command with a custom URL
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("list")
        .arg("--url")
        .arg(&format!("http://{}", mock_server.server.host_with_port()))
        .output()
        .expect("Failed to execute command");

    assert_success_with_output(&output, &["svm", "list"]);
}

#[test]
fn test_svm_install() {
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("install")
        .arg("solana")
        .output()
        .expect("Failed to execute command");

    assert_success_with_output(&output, &["solana", "install", "installing"]);
}

#[test]
fn test_svm_install_invalid() {
    let output = Command::cargo_bin("osvm").unwrap()
        .arg("svm")
        .arg("install")
        .arg("invalid_svm")
        .arg("--host")
        .arg("user@host")
        .output()
        .expect("Failed to execute command");

    // Either command failed (non-zero exit) or output contains error terms
    if output.status.success() {
        assert!(output_contains_any(&output, &["error", "not found", "invalid", "failed"]));
    }
}
