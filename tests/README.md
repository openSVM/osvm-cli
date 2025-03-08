# OSVM CLI Tests

This directory contains tests for the OSVM CLI application.

## End-to-End (E2E) Tests

The `e2e` directory contains end-to-end tests that verify the CLI functionality by running commands and checking their output. These tests simulate real user interactions with the CLI.

### Test Structure

- `common.rs`: Common utilities for e2e tests, including functions to run commands and check output
- `svm_tests.rs`: Tests for SVM-related commands
- `node_tests.rs`: Tests for node-related commands

### Running the Tests

To run all tests:

```bash
cargo test
```

To run only e2e tests:

```bash
cargo test --test main
```

To run a specific test:

```bash
cargo test --test main test_svm_list
```

### Test Coverage

The e2e tests cover the following functionality:

#### SVM Commands
- `svm list`: List all available SVMs
- `svm get`: Get detailed information about a specific SVM
- `svm dashboard`: Launch the SVM dashboard

#### Node Commands
- `nodes list`: List all managed nodes
- `nodes list` with filters: Test filtering by network, type, and status
- `nodes dashboard`: Launch the node dashboard
- `nodes get`: Get detailed information about a specific node

#### General CLI Features
- Custom configuration file
- Custom RPC URL
- Verbose output
- No-color mode
- Help command

### Mock Server

The tests use a mock server to simulate API responses for testing without requiring a real network connection. This allows the tests to run in any environment without external dependencies.

### Test Dependencies

- `assert_cmd`: For running commands and making assertions about their output
- `predicates`: For making assertions about command output
- `tempfile`: For creating temporary directories and files
- `serial_test`: For running tests serially to avoid conflicts
- `mockito`: For creating a mock server to simulate API responses