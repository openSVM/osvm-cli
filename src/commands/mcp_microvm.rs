/// MCP MicroVM command handler
///
/// This module provides commands to launch and manage MCP servers in isolated microVMs
/// for secure tool execution.

use anyhow::{Context, Result};
use clap::ArgMatches;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::services::mcp_service::McpService;
use crate::services::microvm_launcher::{McpServerMicroVmConfig, MicroVmLauncher};

/// Handle MCP microVM subcommands
pub async fn handle_mcp_microvm_command(matches: &ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("launch", sub_m)) => launch_mcp_microvm(sub_m).await,
        Some(("launch-many", sub_m)) => launch_many_mcp_microvms(sub_m).await,
        Some(("status", sub_m)) => show_microvm_status(sub_m).await,
        Some(("stop", sub_m)) => stop_mcp_microvm(sub_m).await,
        Some(("test", sub_m)) => test_mcp_microvm(sub_m).await,
        _ => {
            println!("MCP MicroVM Management Commands:");
            println!();
            println!("Available commands:");
            println!("  launch <server-id>     Launch an MCP server in a microVM");
            println!("  launch-many <count>    Launch multiple MCP servers for testing");
            println!("  status                 Show status of running MCP microVMs");
            println!("  stop <server-id>       Stop a running MCP microVM");
            println!("  test                   Run microVM integration tests");
            println!();
            println!("Examples:");
            println!("  osvm mcp microvm launch my-server");
            println!("  osvm mcp microvm launch-many 10");
            println!("  osvm mcp microvm status");
            Ok(())
        }
    }
}

/// Launch a single MCP server in a microVM
async fn launch_mcp_microvm(matches: &ArgMatches) -> Result<()> {
    let server_id = matches
        .get_one::<String>("server-id")
        .context("Server ID is required")?;

    let memory_mb: u32 = matches
        .get_one::<String>("memory")
        .and_then(|s| s.parse().ok())
        .unwrap_or(256);

    let vcpus: u32 = matches
        .get_one::<String>("vcpus")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    println!("🚀 Launching MCP server '{}' in microVM...", server_id);
    println!("  Memory: {}MB", memory_mb);
    println!("  vCPUs: {}", vcpus);

    // Create launcher
    let launcher = MicroVmLauncher::new()?;

    // Build config
    let config = McpServerMicroVmConfig {
        server_id: server_id.clone(),
        memory_mb,
        vcpus,
        server_command: matches
            .get_one::<String>("command")
            .cloned()
            .unwrap_or_else(|| format!("mcp-server-{}", server_id)),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 0, // Auto-allocate
    };

    // Launch the microVM
    let mut handle = launcher.launch_mcp_server(config)?;

    if handle.is_running() {
        println!("✅ MicroVM launched successfully!");
        println!("  CID: {}", handle.vsock_cid());
        println!("  Status: Running");
        println!();
        println!("The MCP server is now running in an isolated microVM.");
        println!("Tools executed on this server will run in complete isolation.");
    } else {
        println!("❌ Failed to launch microVM");
        return Err(anyhow::anyhow!("MicroVM launch failed"));
    }

    Ok(())
}

/// Launch multiple MCP servers for load testing
async fn launch_many_mcp_microvms(matches: &ArgMatches) -> Result<()> {
    let count: usize = matches
        .get_one::<String>("count")
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);

    let memory_mb: u32 = matches
        .get_one::<String>("memory")
        .and_then(|s| s.parse().ok())
        .unwrap_or(256);

    println!("🚀 Launching {} MCP servers in microVMs...", count);
    println!("  Memory per VM: {}MB", memory_mb);
    println!("  Total memory needed: {}MB", count * memory_mb as usize);
    println!();

    let launcher = MicroVmLauncher::new()?;
    let mut handles = Vec::new();
    let mut successful = 0;
    let mut failed = 0;

    for i in 0..count {
        let server_id = format!("test-server-{}", i);
        print!("Launching {}... ", server_id);

        let config = McpServerMicroVmConfig {
            server_id: server_id.clone(),
            memory_mb,
            vcpus: 1,
            server_command: format!("echo 'MCP Server {} ready'", i),
            work_dir: PathBuf::from("/app"),
            mounts: vec![],
            vsock_cid: 0,
        };

        match launcher.launch_mcp_server(config) {
            Ok(mut handle) => {
                if handle.is_running() {
                    println!("✅");
                    handles.push(handle);
                    successful += 1;
                } else {
                    println!("❌ (not running)");
                    failed += 1;
                }
            }
            Err(e) => {
                println!("❌ ({})", e);
                failed += 1;
            }
        }

        // Show progress every 10
        if (i + 1) % 10 == 0 {
            println!("  Progress: {}/{}", i + 1, count);
        }
    }

    println!();
    println!("Results:");
    println!("  ✅ Successfully launched: {}", successful);
    if failed > 0 {
        println!("  ❌ Failed: {}", failed);
    }
    println!();

    if successful > 0 {
        println!("Press Enter to shutdown all VMs...");
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        println!("Shutting down VMs...");
        for mut handle in handles {
            let _ = handle.shutdown();
        }
        println!("✅ All VMs shut down");
    }

    Ok(())
}

/// Show status of running MCP microVMs
async fn show_microvm_status(_matches: &ArgMatches) -> Result<()> {
    println!("MCP MicroVM Status");
    println!("==================");

    // Get MCP service to check for registered microVMs
    let _mcp_service = McpService::new_with_debug(false);

    // Count Firecracker processes
    let output = std::process::Command::new("ps")
        .args(&["aux"])
        .output()?;

    let process_list = String::from_utf8_lossy(&output.stdout);
    let firecracker_count = process_list
        .lines()
        .filter(|line| line.contains("firecracker"))
        .count();

    println!("Firecracker processes running: {}", firecracker_count);

    if firecracker_count > 0 {
        println!();
        println!("Note: Use 'sudo killall firecracker' to stop all VMs");
    }

    Ok(())
}

/// Stop a running MCP microVM
async fn stop_mcp_microvm(matches: &ArgMatches) -> Result<()> {
    let server_id = matches
        .get_one::<String>("server-id")
        .context("Server ID is required")?;

    println!("Stopping MCP microVM '{}'...", server_id);

    // This would need to be integrated with the MCP service
    // to properly track and stop specific VMs
    println!("⚠️  Not yet implemented. Use 'sudo killall firecracker' to stop all VMs");

    Ok(())
}

/// Test MCP microVM integration
async fn test_mcp_microvm(_matches: &ArgMatches) -> Result<()> {
    println!("Testing MCP MicroVM Integration");
    println!("================================");
    println!();

    // Test 1: Launch a simple VM
    println!("Test 1: Launching test MCP server...");
    let launcher = MicroVmLauncher::new()?;

    let config = McpServerMicroVmConfig {
        server_id: "integration-test".to_string(),
        memory_mb: 256,
        vcpus: 1,
        server_command: "echo 'Test server ready'".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 0,
    };

    match launcher.launch_mcp_server(config) {
        Ok(mut handle) => {
            if handle.is_running() {
                println!("✅ VM launched successfully");
                println!("   CID: {}", handle.vsock_cid());

                // Give it a moment
                std::thread::sleep(std::time::Duration::from_secs(2));

                if handle.is_running() {
                    println!("✅ VM still running after 2 seconds");
                } else {
                    println!("❌ VM stopped unexpectedly");
                }

                // Shutdown
                handle.shutdown()?;
                println!("✅ VM shut down cleanly");
            } else {
                println!("❌ VM failed to start");
            }
        }
        Err(e) => {
            println!("❌ Failed to launch VM: {}", e);
        }
    }

    println!();
    println!("Integration test complete!");

    Ok(())
}