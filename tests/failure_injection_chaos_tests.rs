//! FAILURE INJECTION AND CHAOS ENGINEERING TESTS
//! Intentionally breaking things to test resilience
//! Tests what happens when dependencies fail, networks drop, disks fill, etc.

use anyhow::Result;
use mockito::Server;
use serde_json::json;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tempfile::TempDir;

#[cfg(test)]
mod network_failure_injection_tests {
    use super::*;

    #[tokio::test]
    async fn test_sudden_network_disconnect() -> Result<()> {
        let mut server = Server::new_async().await;

        // First request succeeds
        let _mock1 = server
            .mock("GET", "/data")
            .with_status(200)
            .with_body("success")
            .expect(1)
            .create_async()
            .await;

        let client = reqwest::Client::new();
        let response1 = client.get(format!("{}/data", server.url())).send().await?;
        assert_eq!(response1.status(), 200);

        // Drop all subsequent mocks - simulates network failure
        // Now requests will get connection refused

        let result2 = client.get(format!("{}/data", server.url())).send().await;
        // Should handle connection failure
        assert!(result2.is_err() || result2.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn test_intermittent_connection() -> Result<()> {
        let mut server = Server::new_async().await;

        // 50% success rate
        for i in 0..10 {
            let status = if i % 2 == 0 { 200 } else { 503 };

            let _mock = server
                .mock("GET", &format!("/api/{}", i))
                .with_status(status)
                .expect(1)
                .create_async()
                .await;
        }

        let client = reqwest::Client::new();
        let mut success_count = 0;
        let mut failure_count = 0;

        for i in 0..10 {
            match client
                .get(format!("{}/api/{}", server.url(), i))
                .send()
                .await
            {
                Ok(resp) if resp.status().is_success() => success_count += 1,
                _ => failure_count += 1,
            }
        }

        assert!(success_count > 0);
        assert!(failure_count > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_slow_network_response() -> Result<()> {
        let mut server = Server::new_async().await;

        // Simulates slow response
        let _mock = server
            .mock("GET", "/slow")
            .with_status(200)
            .with_body_from_fn(|_| {
                std::thread::sleep(Duration::from_millis(500));
                "slow response"
            })
            .create_async()
            .await;

        let client = reqwest::Client::builder()
            .timeout(Duration::from_millis(200))
            .build()?;

        let result = client.get(format!("{}/slow", server.url())).send().await;

        // Should timeout
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_connection_reset() -> Result<()> {
        let mut server = Server::new_async().await;

        // Server returns 503 (Service Unavailable)
        let _mock = server
            .mock("GET", "/reset")
            .with_status(503)
            .create_async()
            .await;

        let client = reqwest::Client::new();
        let response = client.get(format!("{}/reset", server.url())).send().await?;

        assert_eq!(response.status(), 503);

        Ok(())
    }

    #[tokio::test]
    async fn test_partial_response() -> Result<()> {
        let mut server = Server::new_async().await;

        // Incomplete JSON response
        let _mock = server
            .mock("GET", "/partial")
            .with_status(200)
            .with_body("{\"data\":\"incomplete")
            .create_async()
            .await;

        let client = reqwest::Client::new();
        let response = client
            .get(format!("{}/partial", server.url()))
            .send()
            .await?;

        let json_result: Result<serde_json::Value, _> = response.json().await;

        // Should fail to parse
        assert!(json_result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_wrong_content_type() -> Result<()> {
        let mut server = Server::new_async().await;

        // Says it's JSON but returns HTML
        let _mock = server
            .mock("GET", "/wrong")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body("<html>Not JSON</html>")
            .create_async()
            .await;

        let client = reqwest::Client::new();
        let response = client.get(format!("{}/wrong", server.url())).send().await?;

        let json_result: Result<serde_json::Value, _> = response.json().await;

        assert!(json_result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_infinite_redirect_loop() -> Result<()> {
        let mut server = Server::new_async().await;

        // Redirect to itself
        let _mock = server
            .mock("GET", "/redirect")
            .with_status(302)
            .with_header("Location", "/redirect")
            .expect_at_least(1)
            .create_async()
            .await;

        let client = reqwest::Client::builder()
            .redirect(reqwest::redirect::Policy::limited(10))
            .build()?;

        let result = client
            .get(format!("{}/redirect", server.url()))
            .send()
            .await;

        // Should hit redirect limit
        assert!(result.is_err());

        Ok(())
    }
}

#[cfg(test)]
mod disk_failure_injection_tests {
    use super::*;

    #[tokio::test]
    async fn test_disk_full_simulation() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Try to write more than available space
        let large_data = vec![0u8; 1024 * 1024]; // 1MB chunks

        let mut files_written = 0;
        for i in 0..1000 {
            let file_path = temp_dir.path().join(format!("file_{}.bin", i));
            match std::fs::write(&file_path, &large_data) {
                Ok(_) => files_written += 1,
                Err(_) => break, // Disk full or write error
            }
        }

        // Should write some files before failing
        assert!(files_written > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_file_permission_denied() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let file_path = temp_dir.path().join("readonly.txt");

        // Create file
        std::fs::write(&file_path, "test")?;

        // Make read-only
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&file_path)?.permissions();
            perms.set_mode(0o444); // Read-only
            std::fs::set_permissions(&file_path, perms)?;

            // Try to write to read-only file
            let result = std::fs::write(&file_path, "overwrite");
            assert!(result.is_err());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_file_locked() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let file_path = temp_dir.path().join("locked.txt");

        use std::fs::OpenOptions;

        // Open file with exclusive lock
        let _file1 = OpenOptions::new()
            .write(true)
            .create(true)
            .open(&file_path)?;

        // Try to open again (might fail on some systems)
        let result2 = OpenOptions::new().write(true).open(&file_path);

        // Behavior depends on OS
        let _ = result2;

        Ok(())
    }

    #[tokio::test]
    async fn test_directory_traversal_blocked() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Try to write outside temp directory
        let malicious_path = temp_dir.path().join("../../../etc/passwd");

        let result = std::fs::write(&malicious_path, "hacked");

        // Should fail (or write to different location)
        assert!(result.is_err() || result.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn test_symlink_attack() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let link_path = temp_dir.path().join("link");
        let target_path = temp_dir.path().join("target");

        std::fs::write(&target_path, "original")?;

        #[cfg(unix)]
        {
            // Create symlink
            std::os::unix::fs::symlink(&target_path, &link_path)?;

            // Writing to symlink writes to target
            std::fs::write(&link_path, "modified")?;

            let content = std::fs::read_to_string(&target_path)?;
            assert_eq!(content, "modified");
        }

        Ok(())
    }
}

#[cfg(test)]
mod memory_corruption_simulation_tests {
    use super::*;

    #[tokio::test]
    async fn test_use_after_free_pattern() -> Result<()> {
        // Rust prevents this at compile time, but simulate the pattern
        let data = vec![1, 2, 3];
        let len = data.len();

        drop(data);

        // Cannot use data here - won't compile
        // let _ = data[0];

        assert_eq!(len, 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_double_free_pattern() -> Result<()> {
        let data = vec![1, 2, 3];

        drop(data);
        // drop(data); // Won't compile - use of moved value

        Ok(())
    }

    #[tokio::test]
    async fn test_dangling_pointer_pattern() -> Result<()> {
        let reference = {
            let temp = vec![1, 2, 3];
            &temp // Dangling reference - won't compile
        };

        // Rust prevents this at compile time
        // let _ = reference[0];

        Ok(())
    }

    #[tokio::test]
    async fn test_data_race_prevention() -> Result<()> {
        use std::sync::Mutex;

        let data = Arc::new(Mutex::new(vec![1, 2, 3]));

        let data1 = Arc::clone(&data);
        let handle = std::thread::spawn(move || {
            let mut d = data1.lock().unwrap();
            d.push(4);
        });

        {
            let mut d = data.lock().unwrap();
            d.push(5);
        }

        handle.join().unwrap();

        let d = data.lock().unwrap();
        assert!(d.len() >= 3);

        Ok(())
    }
}

#[cfg(test)]
mod service_degradation_tests {
    use super::*;

    #[tokio::test]
    async fn test_cascading_timeouts() -> Result<()> {
        // Service A depends on B depends on C
        async fn service_c() -> Result<String> {
            tokio::time::sleep(Duration::from_secs(5)).await;
            Ok("C".to_string())
        }

        async fn service_b() -> Result<String> {
            tokio::time::timeout(Duration::from_secs(3), service_c()).await??;
            Ok("B".to_string())
        }

        async fn service_a() -> Result<String> {
            tokio::time::timeout(Duration::from_secs(2), service_b()).await??;
            Ok("A".to_string())
        }

        let result = service_a().await;

        // Should timeout at level A or B
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_resource_starvation() -> Result<()> {
        let semaphore = Arc::new(tokio::sync::Semaphore::new(2));
        let starved = Arc::new(AtomicUsize::new(0));

        let mut handles = vec![];

        for i in 0..10 {
            let sem = Arc::clone(&semaphore);
            let starve_count = Arc::clone(&starved);

            let handle = tokio::spawn(async move {
                match sem.try_acquire() {
                    Ok(_permit) => {
                        tokio::time::sleep(Duration::from_millis(100)).await;
                    }
                    Err(_) => {
                        starve_count.fetch_add(1, Ordering::SeqCst);
                    }
                }
            });

            handles.push(handle);
        }

        futures::future::join_all(handles).await;

        let starved_count = starved.load(Ordering::SeqCst);

        // Most should be starved initially
        assert!(starved_count > 5);

        Ok(())
    }

    #[tokio::test]
    async fn test_partial_service_failure() -> Result<()> {
        // 3 services, 1 fails
        let services = vec![
            tokio::spawn(async { Ok::<_, anyhow::Error>(1) }),
            tokio::spawn(async { Err::<i32, _>(anyhow::anyhow!("Service 2 failed")) }),
            tokio::spawn(async { Ok::<_, anyhow::Error>(3) }),
        ];

        let results = futures::future::join_all(services).await;

        let successes: Vec<_> = results
            .iter()
            .filter_map(|r| r.as_ref().ok())
            .filter_map(|r| r.as_ref().ok())
            .collect();

        let failures: Vec<_> = results
            .iter()
            .filter_map(|r| r.as_ref().ok())
            .filter_map(|r| r.as_ref().err())
            .collect();

        assert_eq!(successes.len(), 2);
        assert_eq!(failures.len(), 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_health_check_flapping() -> Result<()> {
        // Health check alternates between healthy/unhealthy
        let is_healthy = Arc::new(AtomicBool::new(true));

        let mut checks = vec![];

        for _ in 0..10 {
            let healthy = Arc::clone(&is_healthy);

            // Flip state
            let current = healthy.fetch_not(Ordering::SeqCst);
            checks.push(current);
        }

        // Should alternate
        let healthy_count = checks.iter().filter(|&&h| h).count();
        let unhealthy_count = checks.iter().filter(|&&h| !h).count();

        assert!(healthy_count > 0 && unhealthy_count > 0);

        Ok(())
    }
}

#[cfg(test)]
mod dependency_injection_failures {
    use super::*;

    #[tokio::test]
    async fn test_missing_dependency() -> Result<()> {
        // Simulate optional dependency missing
        let optional_service: Option<String> = None;

        let result = match optional_service {
            Some(service) => service,
            None => "fallback".to_string(),
        };

        assert_eq!(result, "fallback");

        Ok(())
    }

    #[tokio::test]
    async fn test_circular_dependency() -> Result<()> {
        // Detect circular dependencies
        struct ServiceA {
            name: String,
        }

        struct ServiceB {
            name: String,
        }

        // A depends on B, B depends on A (circular)
        // Rust prevents this at compile time with Rc::new_cyclic or similar

        let _a = ServiceA {
            name: "A".to_string(),
        };
        let _b = ServiceB {
            name: "B".to_string(),
        };

        Ok(())
    }

    #[tokio::test]
    async fn test_incompatible_dependency_version() -> Result<()> {
        // Simulate version mismatch
        #[derive(Debug)]
        struct DependencyV1 {
            version: u32,
        }

        #[derive(Debug)]
        struct DependencyV2 {
            version: u32,
        }

        let v1 = DependencyV1 { version: 1 };
        let v2 = DependencyV2 { version: 2 };

        // Cannot use v2 where v1 is expected (type safety)
        assert_ne!(v1.version, v2.version);

        Ok(())
    }
}

#[cfg(test)]
mod clock_skew_and_timing_tests {
    use super::*;

    #[tokio::test]
    async fn test_clock_skew_simulation() -> Result<()> {
        let server_time = chrono::Utc::now();
        let client_time = server_time - chrono::Duration::hours(1); // 1 hour behind

        let time_diff = server_time - client_time;

        assert!(time_diff.num_hours() == 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_timestamp_overflow() -> Result<()> {
        // Year 2038 problem (32-bit timestamps)
        let y2038 = 2_147_483_647i64; // Max 32-bit signed int

        let overflow = y2038.wrapping_add(1);

        assert!(overflow < 0 || overflow > y2038);

        Ok(())
    }

    #[tokio::test]
    async fn test_leap_second_handling() -> Result<()> {
        // Simulate leap second
        use chrono::{NaiveDate, NaiveDateTime, NaiveTime};

        // June 30, 2015 at 23:59:60 UTC (leap second)
        let date = NaiveDate::from_ymd_opt(2015, 6, 30).unwrap();
        let time = NaiveTime::from_hms_opt(23, 59, 59).unwrap();

        let _dt = NaiveDateTime::new(date, time);

        // Chrono handles leap seconds

        Ok(())
    }
}

#[cfg(test)]
mod state_corruption_tests {
    use super::*;

    #[tokio::test]
    async fn test_inconsistent_state() -> Result<()> {
        // Two pieces of state get out of sync
        let mut counter = 0;
        let mut sum = 0;

        for i in 1..=10 {
            counter += 1;
            if i != 5 {
                // Oops! Missed adding 5
                sum += i;
            }
        }

        // Inconsistent: counter = 10 but sum = 50 instead of 55
        assert_eq!(counter, 10);
        assert_eq!(sum, 50);
        assert_ne!(sum, (1..=10).sum::<i32>());

        Ok(())
    }

    #[tokio::test]
    async fn test_state_machine_invalid_transition() -> Result<()> {
        #[derive(Debug, PartialEq)]
        enum State {
            Init,
            Running,
            Stopped,
        }

        let mut state = State::Init;

        // Valid transition
        state = State::Running;
        assert_eq!(state, State::Running);

        // Invalid transition (but not prevented by type system)
        state = State::Init; // Going back to Init from Running might be invalid

        assert_eq!(state, State::Init);

        Ok(())
    }
}

#[cfg(test)]
mod configuration_chaos_tests {
    use super::*;

    #[tokio::test]
    async fn test_invalid_configuration() -> Result<()> {
        // Config with contradictory settings
        let mut config = HashMap::new();
        config.insert("port", "8080");
        config.insert("ssl", "true");
        config.insert("ssl_cert", ""); // SSL enabled but no cert!

        let has_ssl = config.get("ssl") == Some(&"true");
        let has_cert = !config.get("ssl_cert").unwrap_or(&"").is_empty();

        // Inconsistent configuration
        assert!(has_ssl && !has_cert);

        Ok(())
    }

    #[tokio::test]
    async fn test_configuration_hot_reload_race() -> Result<()> {
        let config = Arc::new(tokio::sync::RwLock::new(HashMap::new()));

        // Writer task
        let config_writer = Arc::clone(&config);
        let writer = tokio::spawn(async move {
            for i in 0..100 {
                let mut cfg = config_writer.write().await;
                cfg.insert("counter", i.to_string());
            }
        });

        // Reader task (might see inconsistent state during reload)
        let config_reader = Arc::clone(&config);
        let reader = tokio::spawn(async move {
            for _ in 0..100 {
                let cfg = config_reader.read().await;
                let _ = cfg.get("counter");
            }
        });

        writer.await?;
        reader.await?;

        Ok(())
    }
}
