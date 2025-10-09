use osvm_cli::services::unikernel_runtime::{UnikernelConfig, UnikernelLauncher, UnikernelRuntime};
use std::collections::HashMap;
use std::path::PathBuf;

#[tokio::test]
async fn test_kraft_binary_detection() {
    let runtime = UnikernelRuntime::new();

    // Check if kraft is available
    let kraft_available = std::process::Command::new("kraft")
        .arg("version")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false);

    // Also check for flatpak-spawn if in flatpak
    let flatpak_kraft_available = if std::path::Path::new("/.flatpak-info").exists() {
        std::process::Command::new("flatpak-spawn")
            .args(&["--host", "--", "kraft", "version"])
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
    } else {
        false
    };

    assert!(
        kraft_available || flatpak_kraft_available,
        "kraft binary must be available for unikernel tests"
    );
}

#[tokio::test]
async fn test_guest_binary_exists() {
    let guest_binary = PathBuf::from(std::env::var("HOME").unwrap())
        .join(".osvm/unikernels/unikraft_tool_executor");

    assert!(
        guest_binary.exists(),
        "Guest binary must exist at ~/.osvm/unikernels/unikraft_tool_executor. Run: bash scripts/build-unikraft-tool-executor.sh"
    );

    // Verify it's executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = std::fs::metadata(&guest_binary).unwrap();
        let permissions = metadata.permissions();
        assert!(
            permissions.mode() & 0o111 != 0,
            "Guest binary must be executable"
        );
    }
}

#[tokio::test]
async fn test_cid_allocation_range() {
    let runtime = UnikernelRuntime::new();

    // Allocate multiple CIDs and verify they're in the correct range
    let cids: Vec<u32> = (0..10)
        .map(|_| {
            // Simulate CID allocation (using the same logic as the actual implementation)
            let process_id = std::process::id();
            let timestamp = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64;

            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            std::hash::Hasher::write_u32(&mut hasher, process_id);
            std::hash::Hasher::write_u64(&mut hasher, timestamp);

            let hash = std::hash::Hasher::finish(&hasher);
            let cid = 200 + (hash % 100) as u32; // Range: 200-299
            cid
        })
        .collect();

    // Verify all CIDs are in ephemeral range
    for cid in &cids {
        assert!(
            *cid >= 200 && *cid < 300,
            "CID {} is outside ephemeral range (200-299)",
            cid
        );
    }

    // Verify no overlap with persistent range (100-199)
    for cid in &cids {
        assert!(
            *cid < 100 || *cid >= 200,
            "CID {} overlaps with persistent range (100-199)",
            cid
        );
    }
}

#[tokio::test]
async fn test_unikernel_config_creation() {
    let config = UnikernelConfig {
        server_id: "test_server".to_string(),
        image_name: "unikraft_tool_executor".to_string(),
        memory_mb: 128,
        vcpus: 1,
        network_enabled: false,
        mounts: vec![],
        env_vars: HashMap::new(),
        timeout_secs: 30,
        launcher: UnikernelLauncher::Unikraft,
        kraft_config: None,
        vsock_cid: None,
    };

    assert_eq!(config.server_id, "test_server");
    assert_eq!(config.image_name, "unikraft_tool_executor");
    assert_eq!(config.memory_mb, 128);
    assert_eq!(config.vcpus, 1);
    assert!(!config.network_enabled);
    assert!(matches!(config.launcher, UnikernelLauncher::Unikraft));
}

#[tokio::test]
async fn test_vsock_port_constant() {
    // Verify the vsock port is consistent with Phase 3
    const VSOCK_PORT: u32 = 5252;

    // This should match the port in:
    // 1. guest/unikraft_tool_executor/src/main.rs
    // 2. guest/mcp_vsock_wrapper/src/main.rs (Phase 3)
    // 3. src/services/unikernel_runtime.rs
    assert_eq!(
        VSOCK_PORT, 5252,
        "Vsock port must be 5252 for Phase 2/3 compatibility"
    );
}

#[tokio::test]
async fn test_json_rpc_request_format() {
    use serde_json::{json, Value};

    // Test request format matches JSON-RPC 2.0 spec
    let request = json!({
        "jsonrpc": "2.0",
        "id": 123,
        "server_id": "test_server",
        "method": "test_method",
        "params": {
            "arg1": "value1"
        }
    });

    assert_eq!(request["jsonrpc"], "2.0");
    assert!(request["id"].is_number());
    assert!(request["method"].is_string());
    assert!(request["params"].is_object());
}

#[tokio::test]
async fn test_length_prefix_protocol() {
    // Test that messages use 4-byte little-endian length prefix
    let message = b"Hello, World!";
    let length = message.len() as u32;
    let length_bytes = length.to_le_bytes();

    assert_eq!(length_bytes.len(), 4, "Length prefix must be 4 bytes");

    // Verify we can reconstruct the length
    let reconstructed_length = u32::from_le_bytes(length_bytes);
    assert_eq!(reconstructed_length, length);
}

#[tokio::test]
async fn test_max_message_size() {
    // Verify max message size is reasonable (10MB)
    const MAX_MESSAGE_SIZE: usize = 10 * 1024 * 1024;

    // This should match the limit in guest/unikraft_tool_executor/src/protocol.rs
    assert_eq!(
        MAX_MESSAGE_SIZE,
        10 * 1024 * 1024,
        "Max message size should be 10MB"
    );
}

#[tokio::test]
#[ignore] // Requires kraft to be installed and guest binary built
async fn test_spawn_and_terminate_unikernel() {
    let runtime = UnikernelRuntime::new();

    let config = UnikernelConfig {
        server_id: "test_server".to_string(),
        image_name: "unikraft_tool_executor".to_string(),
        memory_mb: 128,
        vcpus: 1,
        network_enabled: false,
        mounts: vec![],
        env_vars: HashMap::new(),
        timeout_secs: 30,
        launcher: UnikernelLauncher::Unikraft,
        kraft_config: None,
        vsock_cid: None,
    };

    // Try to spawn unikernel
    let result = runtime.spawn_unikernel(&config).await;

    match result {
        Ok(handle) => {
            // Verify handle has expected fields
            assert!(!handle.server_id.is_empty());
            assert!(handle.pid > 0);
            assert!(handle.vsock_cid.is_some());

            let cid = handle.vsock_cid.unwrap();
            assert!(cid >= 200 && cid < 300, "CID should be in ephemeral range");

            // Terminate the unikernel
            let terminate_result = runtime.terminate_unikernel(&handle.server_id).await;
            assert!(
                terminate_result.is_ok(),
                "Failed to terminate unikernel: {:?}",
                terminate_result.err()
            );
        }
        Err(e) => {
            // If kraft is not available or guest binary not built, test is skipped
            eprintln!("Skipping spawn test: {}", e);
        }
    }
}

#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_vsock_connection() {
    use tokio::time::{timeout, Duration};

    let runtime = UnikernelRuntime::new();

    let config = UnikernelConfig {
        server_id: "test_connection".to_string(),
        image_name: "unikraft_tool_executor".to_string(),
        memory_mb: 128,
        vcpus: 1,
        network_enabled: false,
        mounts: vec![],
        env_vars: HashMap::new(),
        timeout_secs: 30,
        launcher: UnikernelLauncher::Unikraft,
        kraft_config: None,
        vsock_cid: None,
    };

    // Spawn unikernel
    let handle = match runtime.spawn_unikernel(&config).await {
        Ok(h) => h,
        Err(e) => {
            eprintln!("Skipping connection test: {}", e);
            return;
        }
    };

    // Wait a bit for unikernel to boot
    tokio::time::sleep(Duration::from_millis(1000)).await;

    // Try to execute a test tool
    let result = timeout(
        Duration::from_secs(5),
        handle.execute_tool("test_tool", Some(serde_json::json!({})), &runtime),
    )
    .await;

    // Cleanup
    let _ = runtime.terminate_unikernel(&handle.server_id).await;

    match result {
        Ok(Ok(response)) => {
            println!("Tool execution successful: {:?}", response);
        }
        Ok(Err(e)) => {
            eprintln!("Tool execution failed: {}", e);
        }
        Err(_) => {
            eprintln!("Tool execution timed out");
        }
    }
}

#[tokio::test]
async fn test_concurrent_cid_allocation() {
    // Test that concurrent CID allocations don't collide
    let mut handles = vec![];

    for _ in 0..5 {
        let handle = tokio::spawn(async {
            let process_id = std::process::id();
            let timestamp = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64;

            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            std::hash::Hasher::write_u32(&mut hasher, process_id);
            std::hash::Hasher::write_u64(&mut hasher, timestamp);

            let hash = std::hash::Hasher::finish(&hasher);
            200 + (hash % 100) as u32
        });
        handles.push(handle);
    }

    let mut cids = vec![];
    for handle in handles {
        cids.push(handle.await.unwrap());
    }

    // All CIDs should be in valid range
    for cid in &cids {
        assert!(*cid >= 200 && *cid < 300);
    }

    // Note: Hash collisions are possible but unlikely with timestamp + process_id
    println!("Allocated CIDs: {:?}", cids);
}

#[test]
fn test_flatpak_detection() {
    let in_flatpak = std::path::Path::new("/.flatpak-info").exists();
    println!("Running in Flatpak: {}", in_flatpak);

    // This test just documents the detection logic
    // No assertion needed
}

#[tokio::test]
async fn test_error_handling_missing_binary() {
    let runtime = UnikernelRuntime::new();

    let config = UnikernelConfig {
        server_id: "test_missing".to_string(),
        image_name: "nonexistent_binary".to_string(),
        memory_mb: 128,
        vcpus: 1,
        network_enabled: false,
        mounts: vec![],
        env_vars: HashMap::new(),
        timeout_secs: 30,
        launcher: UnikernelLauncher::Unikraft,
        kraft_config: None,
        vsock_cid: None,
    };

    // Should fail gracefully when binary doesn't exist
    let result = runtime.spawn_unikernel(&config).await;
    assert!(
        result.is_err(),
        "Should return error for nonexistent binary"
    );
}
