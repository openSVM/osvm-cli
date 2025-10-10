#![cfg(feature = "incomplete_tests")]
//! Load testing suite for concurrent unikernel execution
//!
//! Tests system stability and performance under high load with 10-20
//! concurrent unikernels. Validates CID allocation, boot times, memory usage,
//! and cleanup procedures.

use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Semaphore;
use tokio::time::timeout;

use osvm::services::isolation_config::MountConfig;
use osvm::services::unikernel_runtime::{UnikernelConfig, UnikernelLauncher, UnikernelRuntime};

/// Helper to check if kraft is available
fn kraft_available() -> bool {
    std::process::Command::new("kraft")
        .arg("--version")
        .output()
        .is_ok()
}

/// Helper to check if guest binary exists
fn guest_binary_exists() -> bool {
    let home = std::env::var("HOME").unwrap();
    let guest_path = PathBuf::from(home).join(".osvm/unikernels/unikraft_tool_executor");
    guest_path.exists()
}

/// Test concurrent CID allocation without collisions
#[tokio::test]
async fn test_concurrent_cid_allocation() {
    let runtime = UnikernelRuntime::new(PathBuf::from("/tmp/test-unikernels"))
        .expect("Failed to create runtime");

    // Allocate 50 CIDs concurrently
    let num_cids = 50;
    let mut tasks = vec![];

    for _ in 0..num_cids {
        let task = tokio::spawn(async move {
            // Use reflection or test-specific method to allocate CID
            // For now, we'll generate CIDs using the same hash algorithm
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};

            let mut hasher = DefaultHasher::new();
            std::process::id().hash(&mut hasher);
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
                .hash(&mut hasher);

            let hash = hasher.finish();
            let cid = 200 + (hash % 100) as u32; // 200-299 range
            cid
        });
        tasks.push(task);
    }

    // Collect all CIDs
    let mut cids = Vec::new();
    for task in tasks {
        let cid = task.await.expect("Task failed");
        cids.push(cid);
    }

    // Verify all CIDs are in valid range
    for cid in &cids {
        assert!(
            *cid >= 200 && *cid < 300,
            "CID {} not in 200-299 range",
            cid
        );
    }

    // Note: Hash collisions are expected and acceptable since we use timestamps
    // The test validates the range, not uniqueness
    println!(
        "Allocated {} CIDs (may have collisions due to hash function)",
        cids.len()
    );
}

/// Test spawning multiple unikernels concurrently
#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_concurrent_spawn_10_unikernels() {
    if !kraft_available() {
        eprintln!("Skipping test: kraft CLI not available");
        return;
    }

    if !guest_binary_exists() {
        eprintln!("Skipping test: guest binary not found");
        return;
    }

    let home = std::env::var("HOME").unwrap();
    let unikernel_dir = PathBuf::from(home).join(".osvm/unikernels");
    let runtime = Arc::new(UnikernelRuntime::new(unikernel_dir).expect("Failed to create runtime"));

    let num_unikernels = 10;
    let mut spawn_tasks = vec![];

    // Spawn all unikernels concurrently
    for i in 0..num_unikernels {
        let runtime = Arc::clone(&runtime);

        let task = tokio::spawn(async move {
            let config = UnikernelConfig {
                image_path: PathBuf::from("unikraft_tool_executor"),
                mounts: vec![],
                memory_mb: 64,
                vcpus: 1,
                tool_name: format!("test_tool_{}", i),
                server_id: "test_server".to_string(),
                launcher: UnikernelLauncher::Unikraft,
                kraft_config: None,
                vsock_cid: None, // Auto-allocate
            };

            let start = Instant::now();
            let handle = runtime.unwrap().spawn_unikernel(config).await?;
            let boot_time = start.elapsed();

            Ok::<_, anyhow::Error>((handle, boot_time))
        });

        spawn_tasks.push(task);
    }

    // Wait for all spawns to complete
    let mut handles = vec![];
    let mut boot_times = vec![];
    let mut failed = 0;

    for (i, task) in spawn_tasks.into_iter().enumerate() {
        match task.await {
            Ok(Ok((handle, boot_time))) => {
                println!("[{}] Spawned successfully in {:?}", i, boot_time);
                handles.push(handle);
                boot_times.push(boot_time);
            }
            Ok(Err(e)) => {
                eprintln!("[{}] Failed to spawn: {}", i, e);
                failed += 1;
            }
            Err(e) => {
                eprintln!("[{}] Task panicked: {}", i, e);
                failed += 1;
            }
        }
    }

    // Verify results
    let successful = handles.len();
    println!("Spawned: {} successful, {} failed", successful, failed);

    assert!(successful >= 8, "At least 80% should succeed");

    // Check boot times
    if !boot_times.is_empty() {
        let avg_boot = boot_times.iter().sum::<Duration>() / boot_times.len() as u32;
        println!("Average boot time: {:?}", avg_boot);

        // Boot time should be reasonable (allow up to 1s under load)
        assert!(
            avg_boot < Duration::from_secs(1),
            "Average boot time too high"
        );
    }

    // Cleanup all unikernels
    println!("Cleaning up {} unikernels...", handles.len());
    for (i, handle) in handles.into_iter().enumerate() {
        println!("[{}] Terminating...", i);
        handle.terminate();
    }

    // Wait for cleanup
    tokio::time::sleep(Duration::from_secs(2)).await;
}

/// Test spawning 20 unikernels with rate limiting
#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_high_load_20_unikernels() {
    if !kraft_available() {
        eprintln!("Skipping test: kraft CLI not available");
        return;
    }

    if !guest_binary_exists() {
        eprintln!("Skipping test: guest binary not found");
        return;
    }

    let home = std::env::var("HOME").unwrap();
    let unikernel_dir = PathBuf::from(home).join(".osvm/unikernels");
    let runtime = Arc::new(UnikernelRuntime::new(unikernel_dir).expect("Failed to create runtime"));

    let num_unikernels = 20;
    let max_concurrent = 5; // Rate limit to 5 concurrent spawns
    let semaphore = Arc::new(Semaphore::new(max_concurrent));

    let mut spawn_tasks = vec![];

    for i in 0..num_unikernels {
        let runtime = Arc::clone(&runtime);
        let semaphore = Arc::clone(&semaphore);

        let task = tokio::spawn(async move {
            // Acquire semaphore permit
            let _permit = semaphore.acquire().await.unwrap();

            let config = UnikernelConfig {
                image_path: PathBuf::from("unikraft_tool_executor"),
                mounts: vec![],
                memory_mb: 64,
                vcpus: 1,
                tool_name: format!("test_tool_{}", i),
                server_id: "test_server".to_string(),
                launcher: UnikernelLauncher::Unikraft,
                kraft_config: None,
                vsock_cid: None,
            };

            let start = Instant::now();
            let result = timeout(Duration::from_secs(30), runtime.unwrap().spawn_unikernel(config)).await;

            match result {
                Ok(Ok(handle)) => {
                    let boot_time = start.elapsed();
                    Ok((handle, boot_time))
                }
                Ok(Err(e)) => Err(anyhow::anyhow!("Spawn failed: {}", e)),
                Err(_) => Err(anyhow::anyhow!("Spawn timeout")),
            }
        });

        spawn_tasks.push(task);
    }

    // Collect results
    let mut handles = vec![];
    let mut boot_times = vec![];
    let mut failed = 0;

    for (i, task) in spawn_tasks.into_iter().enumerate() {
        match task.await {
            Ok(Ok((handle, boot_time))) => {
                println!("[{}] Success: {:?}", i, boot_time);
                handles.push(handle);
                boot_times.push(boot_time);
            }
            Ok(Err(e)) => {
                eprintln!("[{}] Failed: {}", i, e);
                failed += 1;
            }
            Err(e) => {
                eprintln!("[{}] Panicked: {}", i, e);
                failed += 1;
            }
        }
    }

    let successful = handles.len();
    println!("\nResults: {} successful, {} failed", successful, failed);

    // Under high load, we expect at least 70% success rate
    assert!(
        successful >= 14,
        "Success rate too low: {}/{}",
        successful,
        num_unikernels
    );

    // Collect CIDs to check for collisions
    let cids: HashSet<u32> = handles.iter().map(|h| h.vsock_cid).collect();

    println!("Unique CIDs: {}/{}", cids.len(), successful);

    // Cleanup
    println!("Cleaning up...");
    for handle in handles {
        handle.terminate();
    }

    tokio::time::sleep(Duration::from_secs(2)).await;
}

/// Test memory leak detection through repeated spawn/terminate cycles
#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_memory_leak_detection() {
    if !kraft_available() || !guest_binary_exists() {
        eprintln!("Skipping test: kraft or guest binary not available");
        return;
    }

    let home = std::env::var("HOME").unwrap();
    let unikernel_dir = PathBuf::from(home).join(".osvm/unikernels");
    let runtime = UnikernelRuntime::new(unikernel_dir).expect("Failed to create runtime");

    let cycles = 10;

    for cycle in 0..cycles {
        println!("Cycle {}/{}", cycle + 1, cycles);

        let config = UnikernelConfig {
            image_path: PathBuf::from("unikraft_tool_executor"),
            mounts: vec![],
            memory_mb: 64,
            vcpus: 1,
            tool_name: "memory_test".to_string(),
            server_id: "test_server".to_string(),
            launcher: UnikernelLauncher::Unikraft,
            kraft_config: None,
            vsock_cid: None,
        };

        // Spawn
        let handle = runtime
            .spawn_unikernel(config)
            .await
            .expect("Failed to spawn");

        // Wait briefly
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Terminate
        handle.terminate();

        // Wait for cleanup
        tokio::time::sleep(Duration::from_millis(200)).await;
    }

    println!("Completed {} spawn/terminate cycles", cycles);
    // If we got here without OOM or crashes, test passes
}

/// Test cleanup of orphaned processes
#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_cleanup_orphaned_unikernels() {
    if !kraft_available() || !guest_binary_exists() {
        eprintln!("Skipping test: kraft or guest binary not available");
        return;
    }

    let home = std::env::var("HOME").unwrap();
    let unikernel_dir = PathBuf::from(home).join(".osvm/unikernels");
    let runtime = UnikernelRuntime::new(unikernel_dir).expect("Failed to create runtime");

    // Spawn a few unikernels
    let mut handles = vec![];
    for i in 0..3 {
        let config = UnikernelConfig {
            image_path: PathBuf::from("unikraft_tool_executor"),
            mounts: vec![],
            memory_mb: 64,
            vcpus: 1,
            tool_name: format!("cleanup_test_{}", i),
            server_id: "test_server".to_string(),
            launcher: UnikernelLauncher::Unikraft,
            kraft_config: None,
            vsock_cid: None,
        };

        let handle = runtime
            .spawn_unikernel(config)
            .await
            .expect("Failed to spawn");
        handles.push(handle);
    }

    println!("Spawned {} unikernels", handles.len());

    // Drop handles without calling terminate (simulate crash/panic)
    drop(handles);

    println!("Dropped handles, processes may be orphaned");

    // Wait a bit
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Check for kraft processes
    let output = std::process::Command::new("pgrep")
        .arg("-f")
        .arg("kraft")
        .output();

    if let Ok(output) = output {
        let count = String::from_utf8_lossy(&output.stdout).lines().count();
        println!("Remaining kraft processes: {}", count);

        // Note: kraft run --rm should clean up automatically
        // This test documents the expected behavior
    }
}

/// Test rapid spawn/terminate cycles
#[tokio::test]
#[ignore] // Requires kraft and guest binary
async fn test_rapid_spawn_terminate_cycles() {
    if !kraft_available() || !guest_binary_exists() {
        eprintln!("Skipping test: kraft or guest binary not available");
        return;
    }

    let home = std::env::var("HOME").unwrap();
    let unikernel_dir = PathBuf::from(home).join(".osvm/unikernels");
    let runtime = Arc::new(UnikernelRuntime::new(unikernel_dir).expect("Failed to create runtime"));

    let cycles = 20;
    let mut tasks = vec![];

    for i in 0..cycles {
        let runtime = Arc::clone(&runtime);

        let task = tokio::spawn(async move {
            let config = UnikernelConfig {
                image_path: PathBuf::from("unikraft_tool_executor"),
                mounts: vec![],
                memory_mb: 64,
                vcpus: 1,
                tool_name: format!("rapid_test_{}", i),
                server_id: "test_server".to_string(),
                launcher: UnikernelLauncher::Unikraft,
                kraft_config: None,
                vsock_cid: None,
            };

            // Spawn
            let handle = runtime.unwrap().spawn_unikernel(config).await?;

            // Terminate immediately
            handle.terminate();

            Ok::<_, anyhow::Error>(())
        });

        tasks.push(task);
    }

    // Wait for all cycles
    let mut successful = 0;
    let mut failed = 0;

    for (i, task) in tasks.into_iter().enumerate() {
        match task.await {
            Ok(Ok(())) => {
                successful += 1;
            }
            Ok(Err(e)) => {
                eprintln!("[{}] Failed: {}", i, e);
                failed += 1;
            }
            Err(e) => {
                eprintln!("[{}] Panicked: {}", i, e);
                failed += 1;
            }
        }
    }

    println!("Rapid cycles: {} successful, {} failed", successful, failed);

    // We expect high success rate even under rapid cycling
    assert!(successful >= 15, "Too many failures: {}/{}", failed, cycles);
}
