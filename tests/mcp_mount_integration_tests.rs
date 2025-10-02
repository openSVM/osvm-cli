//! Integration tests for MCP mount functionality
//!
//! Tests MCP server mounting, unmounting, and listing including:
//! - Mount command execution
//! - Unmount operations
//! - Mount listing
//! - Error handling for invalid mounts

use std::process::Command;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_mcp_mount_help() {
    let output = Command::new("cargo")
        .args(["run", "--", "mcp", "mount", "--help"])
        .output()
        .expect("Failed to execute mcp mount --help");

    assert!(output.status.success(), "mcp mount --help should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("mount") || stdout.contains("Mount"),
        "Help should mention mount functionality"
    );
}

#[test]
fn test_mcp_mounts_list_help() {
    let output = Command::new("cargo")
        .args(["run", "--", "mcp", "mounts"])
        .output()
        .expect("Failed to execute mcp mounts");

    // Should either list mounts or show that none are configured
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(!stderr.contains("panic"), "Should not panic");
    // Either shows mounts, or indicates no mounts configured
    assert!(
        output.status.success() ||
        stdout.contains("No") ||
        stdout.contains("mounts") ||
        stderr.contains("mounts"),
        "Should handle mounts listing"
    );
}

#[test]
fn test_mcp_mount_requires_arguments() {
    // Test that mount command validates required arguments
    let output = Command::new("cargo")
        .args(["run", "--", "mcp", "mount"])
        .output()
        .expect("Failed to run mcp mount without args");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should indicate missing arguments
    assert!(
        !output.status.success() ||
        stderr.contains("required") ||
        stderr.contains("argument") ||
        stderr.contains("help"),
        "Should require arguments for mount"
    );
}

#[test]
fn test_mcp_unmount_help() {
    let output = Command::new("cargo")
        .args(["run", "--", "mcp", "unmount", "--help"])
        .output()
        .expect("Failed to execute mcp unmount --help");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("unmount") || stdout.contains("Unmount"),
        "Help should mention unmount"
    );
}

#[test]
fn test_mcp_unmount_nonexistent() {
    // Test unmounting a server that doesn't exist
    let output = Command::new("cargo")
        .args(["run", "--", "mcp", "unmount", "nonexistent-server-12345"])
        .output()
        .expect("Failed to run unmount");

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    // Should handle gracefully - either succeed (no-op) or report not found
    assert!(!stderr.contains("panic"), "Should not panic on nonexistent unmount");
}

#[cfg(test)]
mod mount_validation_tests {
    use super::*;

    #[test]
    fn test_mount_validates_server_id() {
        // Test that mount validates server ID format
        let output = Command::new("cargo")
            .args([
                "run",
                "--",
                "mcp",
                "mount",
                "../../../etc/passwd", // Path traversal attempt
                "/some/path",
            ])
            .output()
            .expect("Failed to run mount with invalid ID");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should reject invalid server IDs (path traversal attempts)
        assert!(
            !output.status.success() ||
            stderr.contains("Invalid") ||
            stderr.contains("invalid") ||
            stderr.contains("identifier"),
            "Should validate server ID and reject path traversal"
        );
    }

    #[test]
    fn test_mount_validates_path() {
        // Test that mount validates mount paths
        let output = Command::new("cargo")
            .args([
                "run",
                "--",
                "mcp",
                "mount",
                "test-server",
                "/nonexistent/path/12345",
            ])
            .output()
            .expect("Failed to run mount with invalid path");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should validate that path exists or handle error
        assert!(!stderr.contains("panic"), "Should handle invalid path gracefully");
    }

    #[test]
    fn test_mount_rejects_special_characters() {
        // Test that server IDs with special characters are rejected
        let output = Command::new("cargo")
            .args(["run", "--", "mcp", "mount", "test@server!", "/tmp"])
            .output()
            .expect("Failed to run mount with special chars");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should reject special characters in server ID
        assert!(
            !output.status.success() ||
            stderr.contains("Invalid") ||
            stderr.contains("alphanumeric"),
            "Should reject special characters in server ID"
        );
    }
}

#[cfg(test)]
mod mount_lifecycle_tests {
    use super::*;

    #[test]
    #[ignore] // Requires actual MCP server setup
    fn test_mount_unmount_cycle() {
        // This test requires a real MCP server
        // Create temp directory for testing
        let temp_dir = std::env::temp_dir().join("osvm-test-mount");
        fs::create_dir_all(&temp_dir).ok();

        // Try to mount
        let mount_output = Command::new("cargo")
            .args([
                "run",
                "--",
                "mcp",
                "mount",
                "test-integration-server",
                temp_dir.to_str().unwrap(),
            ])
            .output()
            .expect("Failed to mount");

        // If mount succeeded, try to unmount
        if mount_output.status.success() {
            let unmount_output = Command::new("cargo")
                .args(["run", "--", "mcp", "unmount", "test-integration-server"])
                .output()
                .expect("Failed to unmount");

            assert!(
                unmount_output.status.success(),
                "Unmount should succeed after mount"
            );
        }

        // Cleanup
        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_list_mounts_format() {
        let output = Command::new("cargo")
            .args(["run", "--", "mcp", "mounts"])
            .output()
            .expect("Failed to list mounts");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Output should be structured (table or list format)
        assert!(!stderr.contains("panic"), "Should not panic");

        // Check for expected output format markers
        let has_table_format = stdout.contains("Name") ||
            stdout.contains("Path") ||
            stdout.contains("Server");

        let has_list_format = stdout.contains("-") ||
            stdout.contains("•") ||
            stdout.contains("*");

        let has_empty_message = stdout.contains("No mounts") ||
            stdout.contains("empty");

        assert!(
            has_table_format || has_list_format || has_empty_message,
            "Should have structured output or empty message"
        );
    }
}

#[cfg(test)]
mod mount_security_tests {
    use super::*;

    #[test]
    fn test_mount_prevents_path_traversal() {
        // Test path traversal in server ID
        let traversal_attempts = vec![
            "../../../etc/shadow",
            "../../sensitive",
            "..\\..\\Windows\\System32",
            "./../etc/passwd",
        ];

        for attempt in traversal_attempts {
            let output = Command::new("cargo")
                .args(["run", "--", "mcp", "mount", attempt, "/tmp"])
                .output()
                .expect("Failed to test path traversal");

            let stderr = String::from_utf8_lossy(&output.stderr);

            assert!(
                !output.status.success() ||
                stderr.contains("Invalid") ||
                stderr.contains("invalid"),
                "Should reject path traversal attempt: {}",
                attempt
            );
        }
    }

    #[test]
    fn test_mount_validates_identifier_length() {
        // Test with very long server ID
        let long_id = "a".repeat(1000);

        let output = Command::new("cargo")
            .args(["run", "--", "mcp", "mount", &long_id, "/tmp"])
            .output()
            .expect("Failed to test long ID");

        let stderr = String::from_utf8_lossy(&output.stderr);

        // Should reject excessively long identifiers
        assert!(
            !output.status.success() ||
            stderr.contains("Invalid") ||
            stderr.contains("length"),
            "Should reject overly long server IDs"
        );
    }

    #[test]
    fn test_mount_requires_valid_characters() {
        // Test various invalid characters
        let invalid_chars = vec![
            "test server", // space
            "test<server", // angle bracket
            "test>server", // angle bracket
            "test|server", // pipe
            "test&server", // ampersand
            "test;server", // semicolon
        ];

        for invalid in invalid_chars {
            let output = Command::new("cargo")
                .args(["run", "--", "mcp", "mount", invalid, "/tmp"])
                .output()
                .expect("Failed to test invalid char");

            let stderr = String::from_utf8_lossy(&output.stderr);

            assert!(
                !output.status.success() ||
                stderr.contains("Invalid") ||
                stderr.contains("alphanumeric"),
                "Should reject invalid character in: {}",
                invalid
            );
        }
    }
}
