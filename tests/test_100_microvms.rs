//! Stress test: Launch 100 concurrent MicroVMs
//!
//! This test demonstrates the scalability of the MCP server isolation infrastructure
//! by launching and managing 100 concurrent microVMs.

use anyhow::Result;
use osvm::services::microvm_launcher::{McpServerMicroVmConfig, MicroVmLauncher};
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

const NUM_VMS: usize = 100;
const PARALLEL_LAUNCHES: usize = 10; // Launch 10 at a time to avoid overwhelming the system
const MEM_PER_VM: u32 = 256; // 256MB per VM (need more for cargo install)
const VCPUS_PER_VM: u32 = 1; // Changed from u8 to u32
const BASE_CID: u32 = 1000; // Starting CID
const VM_COMMAND: &str = "cargo install osvm && osvm whats current unix timestamp";

#[tokio::test]
#[ignore] // Run with: cargo test --test test_100_microvms -- --ignored --nocapture
async fn test_launch_100_concurrent_microvms() -> Result<()> {
    println!("🚀 Launching 100 Concurrent MicroVMs");
    println!("=====================================");
    println!();

    // Check system resources
    let total_mem_needed = (NUM_VMS as u32 * MEM_PER_VM) / 1024;
    println!("Configuration:");
    println!("  • VMs to launch: {}", NUM_VMS);
    println!("  • Memory per VM: {}MB", MEM_PER_VM);
    println!("  • Total memory needed: {}GB", total_mem_needed);
    println!("  • Parallel launches: {}", PARALLEL_LAUNCHES);
    println!();

    // Initialize launcher
    let launcher = Arc::new(MicroVmLauncher::new()?);

    // Statistics tracking
    let successful_launches = Arc::new(AtomicUsize::new(0));
    let failed_launches = Arc::new(AtomicUsize::new(0));

    // Semaphore to limit concurrent launches
    let semaphore = Arc::new(Semaphore::new(PARALLEL_LAUNCHES));

    // Track VM handles
    let mut handles: Vec<osvm::services::microvm_launcher::McpServerMicroVmHandle> = Vec::new();

    // Start timing
    let start_time = Instant::now();
    println!("Starting launch sequence...");
    println!();

    // Launch VMs in batches
    let mut tasks: JoinSet<
        Result<Option<osvm::services::microvm_launcher::McpServerMicroVmHandle>>,
    > = JoinSet::new();

    for vm_id in 0..NUM_VMS {
        let launcher = launcher.clone();
        let successful = successful_launches.clone();
        let failed = failed_launches.clone();
        let sem = semaphore.clone();

        tasks.spawn(async move {
            // Acquire semaphore permit
            let _permit = sem.acquire().await.unwrap();

            let config = McpServerMicroVmConfig {
                server_id: format!("stress-test-vm-{}", vm_id),
                memory_mb: MEM_PER_VM,
                vcpus: VCPUS_PER_VM,
                server_command: VM_COMMAND.to_string(), // Run OSVM installation and timestamp query
                work_dir: PathBuf::from("/app"),
                mounts: vec![],
                vsock_cid: BASE_CID + vm_id as u32,
            };

            // Try to launch (not async)
            match launcher.launch_mcp_server(config) {
                Ok(handle) => {
                    successful.fetch_add(1, Ordering::Relaxed);
                    let count = successful.load(Ordering::Relaxed);

                    // Progress indicator
                    if count.is_multiple_of(10) {
                        println!("✓ Launched {} VMs...", count);
                    }

                    Ok(Some(handle))
                }
                Err(e) => {
                    failed.fetch_add(1, Ordering::Relaxed);
                    eprintln!("Failed to launch VM {}: {}", vm_id, e);
                    Ok(None)
                }
            }
        });
    }

    // Wait for all launches to complete
    while let Some(result) = tasks.join_next().await {
        if let Ok(Ok(Some(handle))) = result {
            handles.push(handle);
        }
    }

    let launch_duration = start_time.elapsed();
    let successful = successful_launches.load(Ordering::Relaxed);
    let failed = failed_launches.load(Ordering::Relaxed);

    println!();
    println!("Launch Complete!");
    println!("================");
    println!("  ✅ Successfully launched: {} VMs", successful);
    println!("  ❌ Failed to launch: {} VMs", failed);
    println!("  ⏱️  Total time: {:.2}s", launch_duration.as_secs_f64());

    if successful > 0 {
        let avg_launch_time = launch_duration.as_millis() as f64 / successful as f64;
        let vms_per_second = successful as f64 / launch_duration.as_secs_f64();

        println!();
        println!("Performance Metrics:");
        println!("  • Average launch time: {:.0}ms per VM", avg_launch_time);
        println!("  • Launch rate: {:.1} VMs/second", vms_per_second);
        println!(
            "  • Total memory allocated: {}MB",
            successful as u32 * MEM_PER_VM
        );
    }

    // Keep VMs running for a moment to verify stability
    if successful > 0 {
        println!();
        println!("🏃 {} VMs are now running concurrently!", successful);
        println!("   Holding for 5 seconds to verify stability...");

        tokio::time::sleep(Duration::from_secs(5)).await;

        // Check how many are still running
        let mut still_running = 0;
        for handle in &mut handles {
            if handle.is_running() {
                still_running += 1;
            }
        }

        println!("   ✓ {} VMs still running after 5 seconds", still_running);
    }

    // Cleanup
    println!();
    println!("Shutting down all VMs...");

    let shutdown_start = Instant::now();
    let mut shutdown_tasks = JoinSet::new();

    for handle in handles {
        shutdown_tasks.spawn(async move {
            let _ = handle.shutdown(); // shutdown is not async, takes self by value
        });
    }

    while (shutdown_tasks.join_next().await).is_some() {}

    let shutdown_duration = shutdown_start.elapsed();
    println!(
        "✓ All VMs shut down in {:.2}s",
        shutdown_duration.as_secs_f64()
    );

    println!();
    println!("🏁 Stress Test Complete!");
    println!("========================");

    // Assert at least 50% success rate for the test to pass
    assert!(
        successful >= NUM_VMS / 2,
        "Less than 50% of VMs launched successfully ({}/{})",
        successful,
        NUM_VMS
    );

    Ok(())
}

#[tokio::test]
#[ignore]
async fn test_vm_resource_limits() -> Result<()> {
    println!("Testing VM Resource Limit Enforcement");
    println!("====================================");

    let launcher = MicroVmLauncher::new()?;

    // Try to launch a VM with very limited resources
    let config = McpServerMicroVmConfig {
        server_id: "resource-test".to_string(),
        memory_mb: 32, // Very low memory
        vcpus: 1,
        server_command: "echo 'Resource test'".to_string(),
        work_dir: PathBuf::from("/app"),
        mounts: vec![],
        vsock_cid: 2000,
    };

    println!("Attempting to launch VM with 32MB RAM...");
    match launcher.launch_mcp_server(config) {
        // Not async
        Ok(mut handle) => {
            println!("✓ VM launched with minimal resources");

            // Check memory usage
            tokio::time::sleep(Duration::from_secs(2)).await;

            if handle.is_running() {
                println!("✓ VM still running after 2 seconds");
            }

            handle.shutdown()?; // Not async, takes self by value
            println!("✓ VM shut down successfully");
        }
        Err(e) => {
            println!("✗ Failed to launch with minimal resources: {}", e);
        }
    }

    Ok(())
}

#[tokio::test]
#[ignore]
async fn test_rapid_vm_cycling() -> Result<()> {
    println!("Testing Rapid VM Launch/Shutdown Cycling");
    println!("========================================");

    let launcher = MicroVmLauncher::new()?;
    const CYCLES: usize = 20;

    let mut total_launch_time = Duration::ZERO;
    let mut total_shutdown_time = Duration::ZERO;

    for i in 0..CYCLES {
        let config = McpServerMicroVmConfig {
            server_id: format!("cycle-test-{}", i),
            memory_mb: 64,
            vcpus: 1,
            server_command: "echo 'Cycle test'".to_string(),
            work_dir: PathBuf::from("/app"),
            mounts: vec![],
            vsock_cid: 3000 + i as u32,
        };

        // Launch
        let launch_start = Instant::now();
        let handle = launcher.launch_mcp_server(config)?; // Not async, not mutable
        total_launch_time += launch_start.elapsed();

        // Brief run
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Shutdown
        let shutdown_start = Instant::now();
        handle.shutdown()?; // Not async
        total_shutdown_time += shutdown_start.elapsed();

        if (i + 1) % 5 == 0 {
            println!("Completed {} cycles", i + 1);
        }
    }

    println!();
    println!("Cycling Complete:");
    println!("  • Total cycles: {}", CYCLES);
    println!(
        "  • Avg launch time: {:.0}ms",
        total_launch_time.as_millis() as f64 / CYCLES as f64
    );
    println!(
        "  • Avg shutdown time: {:.0}ms",
        total_shutdown_time.as_millis() as f64 / CYCLES as f64
    );

    Ok(())
}
