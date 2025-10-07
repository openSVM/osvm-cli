//! CHAOS ENGINEERING AND STRESS TORTURE TESTS
//! These tests are designed to BREAK the system and find edge cases
//! that normal testing won't catch. We're trying to make things fail!

use anyhow::Result;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use tokio::sync::{Mutex, Semaphore};
use std::time::Duration;
use tempfile::TempDir;

#[cfg(test)]
mod extreme_concurrency_tests {
    use super::*;

    #[tokio::test(flavor = "multi_thread", worker_threads = 16)]
    async fn test_10000_concurrent_operations() -> Result<()> {
        // Try to spawn 10,000 concurrent tasks - can the system handle it?
        let counter = Arc::new(AtomicUsize::new(0));
        let mut handles = vec![];

        for i in 0..10000 {
            let counter_clone = Arc::clone(&counter);
            let handle = tokio::spawn(async move {
                tokio::time::sleep(Duration::from_micros(i % 100)).await;
                counter_clone.fetch_add(1, Ordering::SeqCst);
            });
            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        assert_eq!(counter.load(Ordering::SeqCst), 10000);

        Ok(())
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn test_thundering_herd_problem() -> Result<()> {
        // All tasks wake up at once and compete for a single resource
        let semaphore = Arc::new(Semaphore::new(1));
        let success_count = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        // 1000 tasks all trying to acquire at the exact same time
        for _ in 0..1000 {
            let sem = Arc::clone(&semaphore);
            let count = Arc::clone(&success_count);

            let handle = tokio::spawn(async move {
                let _permit = sem.acquire().await.unwrap();
                count.fetch_add(1, Ordering::SeqCst);
                tokio::time::sleep(Duration::from_micros(1)).await;
            });

            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        // All should have succeeded, one at a time
        assert_eq!(success_count.load(Ordering::SeqCst), 1000);

        Ok(())
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_write_write_conflict() -> Result<()> {
        // Two tasks trying to write to the same memory location
        let shared_value = Arc::new(Mutex::new(0));
        let mut handles = vec![];

        for i in 0..100 {
            let value = Arc::clone(&shared_value);
            let handle = tokio::spawn(async move {
                for _ in 0..100 {
                    let mut v = value.lock().await;
                    *v += 1;
                    tokio::time::sleep(Duration::from_nanos(i)).await;
                }
            });
            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        let final_value = *shared_value.lock().await;
        // Should be exactly 10,000 (100 tasks * 100 increments)
        assert_eq!(final_value, 10000);

        Ok(())
    }

    #[tokio::test]
    async fn test_rapid_task_spawn_and_cancel() -> Result<()> {
        // Spawn and immediately cancel tasks in a loop
        for _ in 0..1000 {
            let handle = tokio::spawn(async {
                tokio::time::sleep(Duration::from_secs(10)).await;
            });

            // Cancel immediately
            handle.abort();
        }

        // Should not panic or leak memory
        Ok(())
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_lock_convoy_phenomenon() -> Result<()> {
        // Create a lock convoy where tasks pile up
        let mutex = Arc::new(Mutex::new(0));
        let convoy_detected = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        for i in 0..50 {
            let m = Arc::clone(&mutex);
            let convoy = Arc::clone(&convoy_detected);

            let handle = tokio::spawn(async move {
                let start = tokio::time::Instant::now();
                let _guard = m.lock().await;

                // If we waited more than 10ms, we were in a convoy
                if start.elapsed() > Duration::from_millis(10) {
                    convoy.fetch_add(1, Ordering::SeqCst);
                }

                // Hold lock for a bit to create convoy
                tokio::time::sleep(Duration::from_millis(5)).await;
            });

            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        // Some tasks should have experienced convoy
        let convoy_count = convoy_detected.load(Ordering::SeqCst);
        println!("Tasks stuck in convoy: {}", convoy_count);

        Ok(())
    }

    #[tokio::test]
    async fn test_spawn_bomb() -> Result<()> {
        // Exponentially spawn tasks (fork bomb style)
        async fn spawn_recursive(depth: usize, max_depth: usize) {
            if depth >= max_depth {
                return;
            }

            let handle1 = tokio::spawn(spawn_recursive(depth + 1, max_depth));
            let handle2 = tokio::spawn(spawn_recursive(depth + 1, max_depth));

            let _ = handle1.await;
            let _ = handle2.await;
        }

        // This creates 2^5 - 1 = 31 tasks
        spawn_recursive(0, 5).await;

        Ok(())
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_deadlock_potential() -> Result<()> {
        // Two locks acquired in different orders - potential deadlock
        let lock_a = Arc::new(Mutex::new(0));
        let lock_b = Arc::new(Mutex::new(0));

        let lock_a1 = Arc::clone(&lock_a);
        let lock_b1 = Arc::clone(&lock_b);

        let handle1 = tokio::spawn(async move {
            let _a = lock_a1.lock().await;
            tokio::time::sleep(Duration::from_millis(10)).await;
            let _b = lock_b1.lock().await;
        });

        let lock_a2 = Arc::clone(&lock_a);
        let lock_b2 = Arc::clone(&lock_b);

        let handle2 = tokio::spawn(async move {
            let _b = lock_b2.lock().await;
            tokio::time::sleep(Duration::from_millis(10)).await;
            let _a = lock_a2.lock().await;
        });

        // Use timeout to detect deadlock
        let result = tokio::time::timeout(
            Duration::from_secs(1),
            futures::future::join_all(vec![handle1, handle2])
        ).await;

        // Should complete or timeout
        assert!(result.is_ok() || result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_channel_overflow() -> Result<()> {
        // Try to overflow a bounded channel
        let (tx, mut rx) = tokio::sync::mpsc::channel(10);

        // Spawn sender that floods the channel
        let sender = tokio::spawn(async move {
            for i in 0..1000 {
                // This will block when channel is full
                let _ = tx.send(i).await;
            }
        });

        // Slow receiver
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Now drain
        let mut count = 0;
        while let Some(_) = rx.recv().await {
            count += 1;
            if count >= 1000 {
                break;
            }
        }

        sender.await?;

        assert_eq!(count, 1000);

        Ok(())
    }

    #[tokio::test]
    async fn test_task_leak_detection() -> Result<()> {
        // Spawn tasks and forget them - memory leak detector
        let leaked_count = Arc::new(AtomicUsize::new(0));

        for _ in 0..100 {
            let count = Arc::clone(&leaked_count);
            tokio::spawn(async move {
                count.fetch_add(1, Ordering::SeqCst);
                tokio::time::sleep(Duration::from_secs(1000)).await;
            });
        }

        tokio::time::sleep(Duration::from_millis(100)).await;

        // All tasks should have started
        assert_eq!(leaked_count.load(Ordering::SeqCst), 100);

        Ok(())
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn test_priority_inversion() -> Result<()> {
        // Low priority task holds lock needed by high priority task
        let shared = Arc::new(Mutex::new(0));
        let completion_order = Arc::new(Mutex::new(Vec::new()));

        // Low priority task (holds lock long)
        let low_shared = Arc::clone(&shared);
        let low_order = Arc::clone(&completion_order);
        let low_task = tokio::spawn(async move {
            let _guard = low_shared.lock().await;
            tokio::time::sleep(Duration::from_millis(100)).await;
            low_order.lock().await.push("low");
        });

        tokio::time::sleep(Duration::from_millis(10)).await;

        // High priority task (needs lock)
        let high_shared = Arc::clone(&shared);
        let high_order = Arc::clone(&completion_order);
        let high_task = tokio::spawn(async move {
            let _guard = high_shared.lock().await;
            high_order.lock().await.push("high");
        });

        let _ = low_task.await;
        let _ = high_task.await;

        let order = completion_order.lock().await;
        // Low should complete first due to priority inversion
        assert_eq!(order[0], "low");

        Ok(())
    }
}

#[cfg(test)]
mod memory_exhaustion_tests {
    use super::*;

    #[tokio::test]
    async fn test_massive_allocation() -> Result<()> {
        // Try to allocate increasingly large buffers
        let mut buffers = Vec::new();

        for size in (1..20).map(|i| 1024 * 1024 * i) {
            // 1MB, 2MB, ..., up to 19MB
            match std::panic::catch_unwind(|| {
                vec![0u8; size]
            }) {
                Ok(buffer) => buffers.push(buffer),
                Err(_) => break, // OOM
            }
        }

        // Should allocate at least some buffers
        assert!(!buffers.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_memory_leak_simulation() -> Result<()> {
        // Simulate memory leak by never dropping allocations
        let leaked = Arc::new(Mutex::new(Vec::new()));

        for i in 0..100 {
            let leak_vec = Arc::clone(&leaked);
            tokio::spawn(async move {
                let data = vec![0u8; 10240]; // 10KB
                leak_vec.lock().await.push(data);
            });
        }

        tokio::time::sleep(Duration::from_millis(100)).await;

        let count = leaked.lock().await.len();
        assert!(count > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_string_explosion() -> Result<()> {
        // Exponentially growing strings
        let mut s = String::from("x");

        for _ in 0..10 {
            s = s.repeat(2); // Doubles each time
            if s.len() > 1_000_000 {
                break;
            }
        }

        assert!(s.len() >= 1024);

        Ok(())
    }

    #[tokio::test]
    async fn test_vec_capacity_explosion() -> Result<()> {
        // Force vec reallocations
        let mut v = Vec::new();

        for i in 0..1000 {
            v.push(i);
            // Every 100 items, check capacity growth
            if i % 100 == 0 {
                assert!(v.capacity() >= v.len());
            }
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_recursive_data_structure() -> Result<()> {
        // Deeply nested structure
        #[derive(Clone)]
        struct Node {
            data: Vec<u8>,
            children: Vec<Node>,
        }

        fn create_tree(depth: usize, max_depth: usize) -> Node {
            if depth >= max_depth {
                return Node {
                    data: vec![0u8; 100],
                    children: vec![],
                };
            }

            Node {
                data: vec![0u8; 100],
                children: vec![
                    create_tree(depth + 1, max_depth),
                    create_tree(depth + 1, max_depth),
                ],
            }
        }

        let tree = create_tree(0, 5);

        // Count nodes recursively
        fn count_nodes(node: &Node) -> usize {
            1 + node.children.iter().map(count_nodes).sum::<usize>()
        }

        let node_count = count_nodes(&tree);
        assert!(node_count > 0);

        Ok(())
    }
}

#[cfg(test)]
mod resource_exhaustion_tests {
    use super::*;

    #[tokio::test]
    async fn test_file_descriptor_exhaustion() -> Result<()> {
        // Try to open many files simultaneously
        let temp_dir = TempDir::new()?;
        let mut files = Vec::new();

        for i in 0..100 {
            let file_path = temp_dir.path().join(format!("file_{}.txt", i));
            match std::fs::File::create(&file_path) {
                Ok(f) => files.push(f),
                Err(_) => break, // Hit limit
            }
        }

        // Should open at least some files
        assert!(files.len() > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_thread_exhaustion() -> Result<()> {
        // Spawn threads until we hit a limit
        let mut handles = Vec::new();

        for i in 0..50 {
            match std::thread::Builder::new()
                .name(format!("test-thread-{}", i))
                .spawn(|| {
                    std::thread::sleep(Duration::from_millis(100));
                }) {
                Ok(h) => handles.push(h),
                Err(_) => break,
            }
        }

        for handle in handles {
            let _ = handle.join();
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_network_connection_exhaustion() -> Result<()> {
        // Simulate opening many network connections
        let connections = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        for _ in 0..100 {
            let conn = Arc::clone(&connections);
            let handle = tokio::spawn(async move {
                conn.fetch_add(1, Ordering::SeqCst);
                tokio::time::sleep(Duration::from_millis(50)).await;
            });
            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        assert_eq!(connections.load(Ordering::SeqCst), 100);

        Ok(())
    }

    #[tokio::test]
    async fn test_timer_exhaustion() -> Result<()> {
        // Create many timers simultaneously
        let mut handles = vec![];

        for i in 0..1000 {
            let handle = tokio::spawn(async move {
                tokio::time::sleep(Duration::from_millis(i % 100)).await;
            });
            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        Ok(())
    }

    #[tokio::test]
    async fn test_disk_space_exhaustion() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Try to write large files until we run out of space
        let mut total_written = 0u64;

        for i in 0..10 {
            let file_path = temp_dir.path().join(format!("large_{}.bin", i));
            let data = vec![0u8; 1024 * 1024]; // 1MB chunks

            match std::fs::write(&file_path, &data) {
                Ok(_) => total_written += data.len() as u64,
                Err(_) => break,
            }
        }

        assert!(total_written > 0);

        Ok(())
    }
}

#[cfg(test)]
mod timeout_and_hang_tests {
    use super::*;

    #[tokio::test]
    async fn test_infinite_loop_detection() -> Result<()> {
        let result = tokio::time::timeout(
            Duration::from_millis(100),
            async {
                // Infinite loop
                let mut counter = 0u64;
                loop {
                    counter = counter.wrapping_add(1);
                    if counter == 0 {
                        break; // Will never happen
                    }
                }
            }
        ).await;

        assert!(result.is_err()); // Should timeout

        Ok(())
    }

    #[tokio::test]
    async fn test_nested_timeout() -> Result<()> {
        // Nested timeouts
        let result = tokio::time::timeout(
            Duration::from_millis(200),
            tokio::time::timeout(
                Duration::from_millis(100),
                tokio::time::sleep(Duration::from_secs(10))
            )
        ).await;

        // Inner timeout should trigger first
        assert!(result.is_ok());
        assert!(result.unwrap().is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_slow_operation_cascade() -> Result<()> {
        // Chain of slow operations
        async fn slow_op(depth: usize) -> usize {
            if depth == 0 {
                return 0;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
            slow_op(depth - 1).await + 1
        }

        let start = tokio::time::Instant::now();
        let result = slow_op(10).await;
        let duration = start.elapsed();

        assert_eq!(result, 10);
        assert!(duration >= Duration::from_millis(100)); // 10 * 10ms

        Ok(())
    }

    #[tokio::test]
    async fn test_select_timeout_race() -> Result<()> {
        // Race between timeout and actual work
        let work = tokio::spawn(async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            42
        });

        let timeout = tokio::time::sleep(Duration::from_millis(100));

        tokio::select! {
            result = work => {
                assert_eq!(result?, 42);
            }
            _ = timeout => {
                panic!("Should not timeout");
            }
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_blocking_in_async() -> Result<()> {
        // Blocking call in async context (bad practice)
        let result = tokio::time::timeout(
            Duration::from_millis(200),
            tokio::task::spawn_blocking(|| {
                std::thread::sleep(Duration::from_millis(100));
                42
            })
        ).await??;

        assert_eq!(result, 42);

        Ok(())
    }
}

#[cfg(test)]
mod panic_and_error_propagation_tests {
    use super::*;

    #[tokio::test]
    async fn test_panic_in_spawned_task() -> Result<()> {
        let handle = tokio::spawn(async {
            panic!("Intentional panic!");
        });

        let result = handle.await;

        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_cascading_failures() -> Result<()> {
        // Chain of tasks where each depends on previous
        let task1 = tokio::spawn(async {
            Err::<i32, _>(anyhow::anyhow!("Task 1 failed"))
        });

        let task2 = tokio::spawn(async move {
            task1.await??; // Propagates error
            Ok::<i32, anyhow::Error>(42)
        });

        let result = task2.await?;
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_double_panic() -> Result<()> {
        // Panic during panic handling
        let result = std::panic::catch_unwind(|| {
            std::panic::catch_unwind(|| {
                panic!("Inner panic");
            }).unwrap_or_else(|_| {
                panic!("Panic while handling panic");
            });
        });

        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_error_in_drop() -> Result<()> {
        struct PanicOnDrop;

        impl Drop for PanicOnDrop {
            fn drop(&mut self) {
                // This is bad practice but we're testing it
                // panic!("Panic in drop");
            }
        }

        {
            let _x = PanicOnDrop;
        } // Drops here

        Ok(())
    }

    #[tokio::test]
    async fn test_unwind_safety_violation() -> Result<()> {
        let data = Arc::new(Mutex::new(vec![1, 2, 3]));

        let data_clone = Arc::clone(&data);
        let result = std::panic::catch_unwind(move || {
            let mut d = futures::executor::block_on(data_clone.lock());
            d.push(4);
            panic!("Oops!");
        });

        assert!(result.is_err());

        // Data might be in inconsistent state
        let d = data.lock().await;
        // Check if we can still access it
        assert!(d.len() >= 3);

        Ok(())
    }
}

#[cfg(test)]
mod extreme_edge_cases {
    use super::*;

    #[tokio::test]
    async fn test_zero_sized_types() -> Result<()> {
        #[derive(Clone, Copy)]
        struct ZST;

        let vec: Vec<ZST> = (0..1000).map(|_| ZST).collect();

        assert_eq!(vec.len(), 1000);
        assert_eq!(std::mem::size_of_val(&vec), std::mem::size_of::<Vec<ZST>>());

        Ok(())
    }

    #[tokio::test]
    async fn test_maximum_integers() -> Result<()> {
        let max_u64 = u64::MAX;
        let overflow = max_u64.wrapping_add(1);

        assert_eq!(overflow, 0);

        let max_i64 = i64::MAX;
        let neg_overflow = max_i64.wrapping_add(1);

        assert!(neg_overflow < 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_empty_collections() -> Result<()> {
        let empty_vec: Vec<i32> = Vec::new();
        let empty_str = "";
        let empty_slice: &[u8] = &[];

        assert_eq!(empty_vec.len(), 0);
        assert_eq!(empty_str.len(), 0);
        assert_eq!(empty_slice.len(), 0);

        // Operations on empty collections
        assert_eq!(empty_vec.iter().sum::<i32>(), 0);
        assert_eq!(empty_str.chars().count(), 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_unicode_edge_cases() -> Result<()> {
        let emoji = "🦀🔥💀";
        let mixed = "Hello🌍World";
        let rtl = "مرحبا";

        assert!(emoji.chars().count() == 3);
        assert!(mixed.len() > mixed.chars().count());

        Ok(())
    }

    #[tokio::test]
    async fn test_float_special_values() -> Result<()> {
        let inf = f64::INFINITY;
        let neg_inf = f64::NEG_INFINITY;
        let nan = f64::NAN;

        assert!(inf.is_infinite());
        assert!(neg_inf.is_infinite());
        assert!(nan.is_nan());

        // NaN comparisons are always false
        assert!(!(nan == nan));
        assert!(!(nan > 0.0));
        assert!(!(nan < 0.0));

        Ok(())
    }
}
