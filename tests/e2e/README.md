# End-to-End (E2E) Tests for OSVM CLI

This directory contains end-to-end tests for the OSVM CLI application. These tests verify the CLI functionality by running commands and checking their output, simulating real user interactions.

## Test Structure

- `mod.rs`: Main module file that includes all test modules
- `common.rs`: Common utilities for e2e tests, including functions to run commands and check output
- `svm_tests.rs`: Tests for SVM-related commands
- `node_tests.rs`: Tests for node-related commands
- `examples.rs`: Example tests demonstrating how to write e2e tests

## Test Utilities

The `common.rs` file provides several utilities for writing e2e tests:

- `run_osvm_command()`: Returns a Command object for running OSVM commands
- `run_osvm_command_string()`: Runs an OSVM command and returns the output as a string
- `output_contains()`: Checks if the output contains the expected text
- `create_temp_dir()`: Creates a temporary directory for test files
- `create_mock_config()`: Creates a mock config file in the given directory
- `MockServer`: A mock server for testing SSH deployment and API responses

## Writing Tests

### Basic Test Structure

```rust
#[test]
#[serial]  // Run tests serially to avoid conflicts
fn test_name() {
    // Run a command and get the output
    let output = run_osvm_command_string(&["command", "arg1", "arg2"]);
    
    // Check if the output contains expected text
    assert!(output_contains(&output, "Expected text"));
}
```

### Using assert_cmd for More Complex Assertions

```rust
#[test]
#[serial]
fn test_with_assert_cmd() {
    // Use assert_cmd to run a command and make assertions about the output
    let assert = run_osvm_command()
        .args(&["command", "arg1", "arg2"])
        .assert();
    
    // Make assertions about the command output
    assert.success()
        .stdout(predicate::str::contains("Expected text"))
        .stderr(predicate::str::contains("Error").not());
}
```

### Using a Mock Server

```rust
#[test]
#[serial]
fn test_with_mock_server() {
    // Create a mock server
    let mock_server = MockServer::new();
    
    // Set up a mock endpoint
    let _mock = mock_server.mock_svm_list();
    
    // Run a command that uses the mock server
    let output = run_osvm_command_string(&["--url", &format!("http://{}", mock_server.server.host_with_port()), "svm", "list"]);
    
    // Check if the output contains expected text
    assert!(output_contains(&output, "Expected text"));
}
```

### Using a Custom Config File

```rust
#[test]
#[serial]
fn test_with_custom_config() {
    // Create a temporary directory and config file
    let temp_dir = create_temp_dir();
    let config_path = create_mock_config(&temp_dir);
    
    // Run a command with the custom config file
    let output = run_osvm_command_string(&["-C", config_path.to_str().unwrap(), "svm", "list"]);
    
    // Check if the output contains expected text
    assert!(output_contains(&output, "Expected text"));
}
```

## Running Tests

To run all e2e tests:

```bash
cargo test --test main
```

To run a specific test:

```bash
cargo test --test main test_svm_list
```

To run tests with verbose output:

```bash
cargo test --test main -- --nocapture
```

## Test Coverage

The e2e tests cover the following functionality:

### SVM Commands
- `svm list`: List all available SVMs
- `svm get`: Get detailed information about a specific SVM
- `svm dashboard`: Launch the SVM dashboard

### Node Commands
- `nodes list`: List all managed nodes
- `nodes list` with filters: Test filtering by network, type, and status
- `nodes dashboard`: Launch the node dashboard
- `nodes get`: Get detailed information about a specific node

### General CLI Features
- Custom configuration file
- Custom RPC URL
- Verbose output
- No-color mode
- Help command

## Adding New Tests

To add a new test:

1. Decide which module the test belongs to (svm_tests, node_tests, or create a new one)
2. Add a new test function with the `#[test]` and `#[serial]` attributes
3. Use the utilities from `common.rs` to run commands and check output
4. Add assertions to verify the expected behavior

Example:

```rust
#[test]
#[serial]
fn test_new_command() {
    let output = run_osvm_command_string(&["new", "command"]);
    assert!(output_contains(&output, "Expected output"));
}
```