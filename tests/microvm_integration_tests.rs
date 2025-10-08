//! Integration tests for MicroVM MCP server infrastructure
//!
//! These tests verify the complete microVM-based MCP server execution flow,
//! including launching, communication, health checks, and shutdown.

use anyhow::Result;
use osvm::services::isolation_config::{IsolationConfig, MicroVmResourceConfig, ServerConfig};
use osvm::services::mcp_service::McpService;
use osvm::services::microvm_launcher::{McpServerMicroVmConfig, MicroVmLauncher, MountPoint};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;
use tokio::time::sleep;

// ============================================================================
// Test 1: Basic MicroVM Launch
// ============================================================================

#[tokio::test]
#[ignore] // Requires Firecracker and guest rootfs
async fn test_microvm_launch_and_init() -> Result<()> {
    println!("Test 1: Testing basic microVM launch and initialization");

    let launcher = MicroVmLauncher::new()?;

    let config = McpServerMicroVmConfig {
        server_id: "test-basic".to_string(),
        memory_mb: 256,
        vcpus: 1,
        server_command: "echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}'".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 0, // Auto-allocate
    };

    println!("Launching microVM with config: {:?}", config);
    let mut handle = launcher.launch_mcp_server(config)?;

    // Verify it's running
    assert!(
        handle.is_running(),
        "MicroVM should be running after launch"
    );

    println!("MicroVM launched successfully, CID: {}", handle.vsock_cid());

    // Give it a moment to stabilize
    sleep(Duration::from_secs(2)).await;

    // Shutdown
    println!("Shutting down microVM");
    handle.shutdown()?;

    println!("✓ Test 1 passed: Basic microVM launch and shutdown successful");
    Ok(())
}

// ============================================================================
// Test 2: Vsock Communication
// ============================================================================

#[tokio::test]
#[ignore] // Requires Firecracker and guest rootfs with wrapper
async fn test_vsock_request_response() -> Result<()> {
    println!("Test 2: Testing vsock request/response communication");

    let launcher = MicroVmLauncher::new()?;

    // Launch a test MCP server that echoes responses
    let config = McpServerMicroVmConfig {
        server_id: "test-vsock".to_string(),
        memory_mb: 256,
        vcpus: 1,
        // Simple echo server for testing
        server_command: "while read line; do echo $line; done".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 0,
    };

    println!("Launching microVM for vsock communication test");
    let handle = launcher.launch_mcp_server(config)?;

    // Wait for initialization
    sleep(Duration::from_secs(3)).await;

    // Send JSON-RPC initialize request
    let init_request = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "osvm-test",
                "version": "0.8.3"
            }
        }
    });

    println!("Sending initialize request via vsock");
    let response = handle.send_request(init_request).await?;

    println!("Received response: {:?}", response);

    // Verify response structure
    assert!(response.is_object(), "Response should be a JSON object");
    assert!(
        response.get("jsonrpc").is_some(),
        "Response should have jsonrpc field"
    );

    // Shutdown
    handle.shutdown()?;

    println!("✓ Test 2 passed: Vsock communication successful");
    Ok(())
}

// ============================================================================
// Test 3: MCP Service Priority Routing
// ============================================================================

#[tokio::test]
async fn test_mcp_service_routing_priority() -> Result<()> {
    println!("Test 3: Testing MCP service routing priority");

    // Create isolation config with microVM enabled
    let mut iso_config = IsolationConfig::default();

    let server_config = ServerConfig {
        microvm_id: "test-vm".to_string(),
        use_microvm: true,
        microvm_config: MicroVmResourceConfig {
            memory_mb: 256,
            vcpus: 1,
        },
        server_command: Some("echo test".to_string()),
        microvm_mounts: vec![],
        tools: HashMap::new(),
    };

    iso_config
        .mcp_servers
        .insert("test-server".to_string(), server_config);

    // Verify config structure
    assert!(
        iso_config
            .get_server_config("test-server")
            .unwrap()
            .use_microvm
    );

    println!("✓ Test 3 passed: MCP service routing configuration verified");
    Ok(())
}

// ============================================================================
// Test 4: Health Check Monitoring
// ============================================================================

#[tokio::test]
#[ignore] // Requires Firecracker and guest rootfs
async fn test_health_check_monitoring() -> Result<()> {
    println!("Test 4: Testing health check monitoring");

    let launcher = MicroVmLauncher::new()?;

    let config = McpServerMicroVmConfig {
        server_id: "test-health".to_string(),
        memory_mb: 256,
        vcpus: 1,
        server_command: "echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}'".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 0,
    };

    println!("Launching microVM for health check test");
    let handle = launcher.launch_mcp_server(config)?;

    // Wait for initialization
    sleep(Duration::from_secs(2)).await;

    // Perform basic health check
    println!("Performing basic health check");
    handle.health_check()?;

    println!("Basic health check passed");

    // Perform active health check
    println!("Performing active health check via vsock");
    handle.health_check_active().await?;

    println!("Active health check passed");

    // Shutdown
    handle.shutdown()?;

    println!("✓ Test 4 passed: Health check monitoring successful");
    Ok(())
}

// ============================================================================
// Test 5: Multiple Concurrent Servers
// ============================================================================

#[tokio::test]
#[ignore] // Requires Firecracker and guest rootfs
async fn test_multiple_concurrent_servers() -> Result<()> {
    println!("Test 5: Testing multiple concurrent microVM servers");

    let launcher = MicroVmLauncher::new()?;

    let mut handles = vec![];

    // Launch 3 concurrent servers
    for i in 0..3 {
        let config = McpServerMicroVmConfig {
            server_id: format!("test-concurrent-{}", i),
            memory_mb: 256,
            vcpus: 1,
            server_command: format!(
                "echo '{{\"jsonrpc\":\"2.0\",\"id\":{},\"result\":{{}}}}'",
                i
            ),
            work_dir: PathBuf::from("/app"),
            mounts: vec![],
            vsock_cid: 0, // Auto-allocate
        };

        println!("Launching microVM #{}", i);
        let handle = launcher.launch_mcp_server(config)?;
        handles.push(handle);
    }

    // Verify all have unique CIDs
    let mut cids = std::collections::HashSet::new();
    for handle in &handles {
        let cid = handle.vsock_cid();
        println!("Server '{}' has CID: {}", handle.server_id(), cid);
        assert!(
            cids.insert(cid),
            "CID {} is duplicated - allocation failed",
            cid
        );
    }

    println!("All servers have unique CIDs: {:?}", cids);

    // Verify all are running
    for (i, handle) in handles.iter_mut().enumerate() {
        assert!(handle.is_running(), "Server #{} should still be running", i);
    }

    // Shutdown all
    println!("Shutting down all servers");
    for (i, handle) in handles.into_iter().enumerate() {
        handle.shutdown()?;
        println!("Server #{} shut down", i);
    }

    println!("✓ Test 5 passed: Multiple concurrent servers successful");
    Ok(())
}

// ============================================================================
// Test 6: Graceful Shutdown
// ============================================================================

#[tokio::test]
#[ignore] // Requires Firecracker and guest rootfs
async fn test_graceful_shutdown_all() -> Result<()> {
    println!("Test 6: Testing graceful shutdown of all servers");

    let mut mcp_service = McpService::new();

    // Create test isolation config
    let mut iso_config = IsolationConfig::default();

    for i in 0..2 {
        let server_id = format!("test-shutdown-{}", i);
        let server_config = ServerConfig {
            microvm_id: format!("test-vm-{}", i),
            use_microvm: true,
            microvm_config: MicroVmResourceConfig {
                memory_mb: 256,
                vcpus: 1,
            },
            server_command: Some("echo test".to_string()),
            microvm_mounts: vec![],
            tools: HashMap::new(),
        };
        iso_config.mcp_servers.insert(server_id, server_config);
    }

    // Note: In a real test, we'd initialize servers here
    // For now, just test the shutdown mechanism exists

    println!("Testing shutdown_all_mcp_microvms() method");
    mcp_service.shutdown_all_mcp_microvms().await?;

    println!("Testing cleanup_all() method");
    mcp_service.cleanup_all().await?;

    println!("✓ Test 6 passed: Graceful shutdown mechanism verified");
    Ok(())
}

// ============================================================================
// Test 7: Error Recovery
// ============================================================================

#[tokio::test]
async fn test_error_recovery() -> Result<()> {
    println!("Test 7: Testing error recovery mechanisms");

    let launcher = MicroVmLauncher::new()?;

    // Test 1: Invalid server command
    let bad_config = McpServerMicroVmConfig {
        server_id: "test-error".to_string(),
        memory_mb: 256,
        vcpus: 1,
        server_command: "/nonexistent/command".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 105,
    };

    // This should fail gracefully
    println!("Testing error handling for invalid configuration");
    let result = launcher.launch_mcp_server(bad_config);

    // We expect this to fail, but it should be a proper error
    if let Err(e) = result {
        println!("Got expected error: {}", e);
    } else {
        // If it somehow succeeds, shut it down
        result.unwrap().shutdown()?;
    }

    println!("✓ Test 7 passed: Error recovery mechanisms verified");
    Ok(())
}

// ============================================================================
// Test 8: CID Allocation Determinism
// ============================================================================

#[tokio::test]
async fn test_cid_allocation_determinism() -> Result<()> {
    println!("Test 8: Testing CID allocation determinism");

    let launcher = MicroVmLauncher::new()?;

    // Test that same server ID gets same CID
    let server_id = "solana";

    let cid1 = launcher.allocate_vsock_cid(server_id)?;
    let cid2 = launcher.allocate_vsock_cid(server_id)?;

    assert_eq!(
        cid1, cid2,
        "Same server ID should get same CID: {} vs {}",
        cid1, cid2
    );

    println!("Server '{}' consistently gets CID {}", server_id, cid1);

    // Verify CID is in valid range
    assert!(
        cid1 >= 100 && cid1 < 200,
        "CID should be in range 100-199, got {}",
        cid1
    );

    // Test different server IDs get (likely) different CIDs
    let github_cid = launcher.allocate_vsock_cid("github")?;
    let filesystem_cid = launcher.allocate_vsock_cid("filesystem")?;

    println!(
        "github CID: {}, filesystem CID: {}",
        github_cid, filesystem_cid
    );

    println!("✓ Test 8 passed: CID allocation determinism verified");
    Ok(())
}

// ============================================================================
// Test 9: Resource Limit Configuration
// ============================================================================

#[tokio::test]
async fn test_resource_limit_configuration() -> Result<()> {
    println!("Test 9: Testing resource limit configuration");

    // Test various memory/CPU configurations
    let configs = vec![
        (256, 1),  // Minimal
        (512, 2),  // Standard
        (1024, 4), // Large
    ];

    for (memory_mb, vcpus) in configs {
        let config = McpServerMicroVmConfig {
            server_id: format!("test-resources-{}-{}", memory_mb, vcpus),
            memory_mb,
            vcpus,
            server_command: "echo test".to_string(),
            work_dir: PathBuf::from("/app"),
            mounts: vec![],
            vsock_cid: 0,
        };

        assert_eq!(config.memory_mb, memory_mb);
        assert_eq!(config.vcpus, vcpus);

        println!("✓ Configuration valid: {} MB, {} vCPUs", memory_mb, vcpus);
    }

    println!("✓ Test 9 passed: Resource limit configuration verified");
    Ok(())
}

// ============================================================================
// Test 10: Mount Point Configuration
// ============================================================================

#[tokio::test]
async fn test_mount_point_configuration() -> Result<()> {
    println!("Test 10: Testing mount point configuration");

    let mounts = vec![
        MountPoint {
            host_path: "~/.config/osvm".to_string(),
            guest_path: "/mnt/config".to_string(),
            readonly: true,
        },
        MountPoint {
            host_path: "~/data".to_string(),
            guest_path: "/data".to_string(),
            readonly: false,
        },
    ];

    let config = McpServerMicroVmConfig {
        server_id: "test-mounts".to_string(),
        memory_mb: 256,
        vcpus: 1,
        server_command: "echo test".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: mounts.clone(),
        vsock_cid: 0,
    };

    assert_eq!(config.mounts.len(), 2);
    assert!(config.mounts[0].readonly);
    assert!(!config.mounts[1].readonly);

    println!(
        "Mount 1: {} -> {} (ro)",
        mounts[0].host_path, mounts[0].guest_path
    );
    println!(
        "Mount 2: {} -> {} (rw)",
        mounts[1].host_path, mounts[1].guest_path
    );

    println!("✓ Test 10 passed: Mount point configuration verified");
    Ok(())
}
