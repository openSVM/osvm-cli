//! Common utilities for E2E tests

use mockito::{self, Server};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output};
use tempfile::TempDir;

/// Path to the osvm binary
#[allow(dead_code)]
pub fn osvm_bin_path() -> PathBuf {
    // In a real environment, this would be the path to the installed binary
    // For testing, we'll use the debug build in the target directory
    let mut path = env::current_dir().unwrap();
    path.push("target");
    path.push("debug");
    path.push("osvm");
    path
}

/// Run an osvm command and return the output
pub fn run_osvm_command() -> Command {
    Command::new(env!("CARGO_BIN_EXE_osvm"))
}

/// Run an osvm command with the given arguments and return the output as a process::Output
pub fn run_osvm_command_string(args: &[&str]) -> Output {
    let output = run_osvm_command()
        .args(args)
        .output()
        .expect("Failed to execute command");
    output
}

/// Check if the output contains a specific string (checks both stdout and stderr)
pub fn output_contains(output: &std::process::Output, expected: &str) -> bool {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    stdout.contains(expected) || stderr.contains(expected)
}

/// Create a temporary directory for testing
pub fn create_temp_dir() -> TempDir {
    tempfile::tempdir().expect("Failed to create temp directory")
}

/// Create a mock configuration file for testing
pub fn create_mock_config(temp_dir: &TempDir) -> PathBuf {
    let config_path = temp_dir.path().join("config.yml");
    let mut config_file = File::create(&config_path).expect("Failed to create config file");

    writeln!(
        config_file,
        r#"---
json_rpc_url: "http://localhost:8899"
websocket_url: ""
keypair_path: ""
address_labels:
  "11111111111111111111111111111111": "System Program"
"#
    )
    .expect("Failed to write to config file");

    config_path
}

/// Struct for managing mock server instances
pub struct MockServer {
    pub server: mockito::ServerGuard,
}

impl MockServer {
    /// Create a new mock server
    pub fn new() -> Self {
        MockServer {
            server: Server::new(),
        }
    }

    /// Mock the SVM list endpoint
    pub fn mock_svm_list(&mut self) -> mockito::Mock {
        self.server
            .mock("GET", "/api/svms")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"
                {
                    "svms": [
                        {"name": "solana", "version": "1.16.0", "status": "active"},
                        {"name": "ethereum", "version": "2.0", "status": "inactive"}
                    ]
                }
                "#,
            )
            .create()
    }
}
