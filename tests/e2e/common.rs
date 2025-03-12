//! Common utilities for e2e tests

use assert_cmd::prelude::*;
use mockito::ServerGuard;
use std::env;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// Path to the osvm binary
pub fn osvm_bin_path() -> PathBuf {
    // In a real environment, this would be the path to the installed binary
    // For testing, we'll use the debug build in the target directory
    let mut path = env::current_dir().unwrap();
    path.push("target");
    path.push("debug");
    path.push("osvm");
    path
}

/// Run an OSVM command and return the command for further assertions
pub fn run_osvm_command() -> Command {
    Command::cargo_bin("osvm").expect("Failed to find osvm binary")
}

/// Run an OSVM command with arguments and return the output as a string
pub fn run_osvm_command_string(args: &[&str]) -> String {
    let output = run_osvm_command()
        .args(args)
        .output()
        .expect("Failed to execute osvm command");

    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Check if the output contains the expected text
pub fn output_contains(output: &str, expected: &str) -> bool {
    output.contains(expected)
}

/// Create a temporary directory for test files
pub fn create_temp_dir() -> TempDir {
    TempDir::new().expect("Failed to create temporary directory")
}

/// Create a mock config file in the given directory
pub fn create_mock_config(dir: &TempDir) -> PathBuf {
    let config_path = dir.path().join("config.yml");
    std::fs::write(
        &config_path,
        "json_rpc_url: http://localhost:8899\nkeypair_path: ~/.config/osvm/id.json\n",
    )
    .expect("Failed to write config file");
    config_path
}

/// Mock server for testing SSH deployment
pub struct MockServer {
    pub server: mockito::ServerGuard,
}

impl MockServer {
    /// Create a new mock server
    pub fn new() -> Self {
        MockServer {
            server: mockito::Server::new(),
        }
    }

    /// Get the connection string for the mock server
    pub fn connection_string(&self) -> String {
        format!("test@{}", self.server.host_with_port())
    }

    /// Mock an SVM list endpoint
    pub fn mock_svm_list(&mut self) -> mockito::Mock {
        self.server.mock("GET", "/api/svms")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"solana":{"name":"solana","display_name":"Solana"},"sonic":{"name":"sonic","display_name":"Sonic"}}"#)
            .create()
    }

    /// Mock an SVM get endpoint
    pub fn mock_svm_get(&mut self, svm_name: &str) -> mockito::Mock {
        self.server.mock("GET", format!("/api/svms/{}", svm_name).as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"name":"{}","display_name":"{}","token_symbol":"TEST","token_price_usd":1.0}}"#, svm_name, svm_name.to_uppercase()))
            .create()
    }

    /// Mock an SVM get endpoint with 404 response
    pub fn mock_svm_get_not_found(&mut self, svm_name: &str) -> mockito::Mock {
        self.server
            .mock("GET", format!("/api/svms/{}", svm_name).as_str())
            .with_status(404)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"error":"SVM not found: {}"}}"#, svm_name))
            .create()
    }
}