use osvm::utils::ebpf_deploy::{
    load_program, load_program_id, DeployConfig, DeploymentResult, RpcClientCache,
};
use solana_sdk::pubkey::Pubkey;
use std::fs::File;
use std::io::Write;
use std::time::{Duration, Instant};
use tempfile::tempdir;

/// Test performance degradation thresholds
#[test]
fn test_performance_thresholds() {
    // Test that deployment operations complete within acceptable timeframes
    let dir = tempdir().unwrap();

    // Create a medium-sized program file
    let program_file = dir.path().join("test_program.so");
    let program_data = vec![0u8; 500_000]; // 500KB
    let mut file = File::create(&program_file).unwrap();
    file.write_all(&program_data).unwrap();

    // Benchmark program loading
    let start = Instant::now();
    let _program = load_program(program_file.to_str().unwrap()).unwrap();
    let load_duration = start.elapsed();

    // Should load within 100ms for 500KB file
    assert!(
        load_duration < Duration::from_millis(100),
        "Program loading took too long: {:?}",
        load_duration
    );

    // Test program ID loading performance
    let program_id_file = dir.path().join("program_id.json");
    let program_id_content = r#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#;
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(program_id_content.as_bytes()).unwrap();

    let start = Instant::now();
    let _program_id = load_program_id(program_id_file.to_str().unwrap()).unwrap();
    let id_load_duration = start.elapsed();

    // Should load program ID within 10ms
    assert!(
        id_load_duration < Duration::from_millis(10),
        "Program ID loading took too long: {:?}",
        id_load_duration
    );
}

/// Test RPC client cache performance requirements
#[test]
fn test_rpc_cache_performance() {
    let mut cache = RpcClientCache::new();
    let test_url = "https://api.devnet.solana.com";

    // First access (cache miss)
    let start = Instant::now();
    let _client1 = cache.get_client(test_url);
    let cache_miss_duration = start.elapsed();

    // Second access (cache hit)
    let start = Instant::now();
    let _client2 = cache.get_client(test_url);
    let cache_hit_duration = start.elapsed();

    // Cache hit should be significantly faster than cache miss
    assert!(
        cache_hit_duration < cache_miss_duration / 10,
        "Cache hit not significantly faster than miss. Hit: {:?}, Miss: {:?}",
        cache_hit_duration,
        cache_miss_duration
    );

    // Cache hit should be very fast (< 1ms)
    assert!(
        cache_hit_duration < Duration::from_millis(1),
        "Cache hit too slow: {:?}",
        cache_hit_duration
    );
}

/// Test JSON serialization performance requirements
#[test]
fn test_json_performance() {
    let result = DeploymentResult {
        network: "devnet".to_string(),
        program_id: Pubkey::new_unique(),
        success: true,
        transaction_signature: Some("test_signature".to_string()),
        error_message: None,
        retries_attempted: 3,
        duration_ms: 5000,
    };

    // Test serialization performance
    let start = Instant::now();
    let json = serde_json::to_string(&result).unwrap();
    let serialize_duration = start.elapsed();

    // Should serialize within 1ms
    assert!(
        serialize_duration < Duration::from_millis(1),
        "JSON serialization too slow: {:?}",
        serialize_duration
    );

    // Test deserialization performance
    let start = Instant::now();
    let _: DeploymentResult = serde_json::from_str(&json).unwrap();
    let deserialize_duration = start.elapsed();

    // Should deserialize within 1ms
    assert!(
        deserialize_duration < Duration::from_millis(1),
        "JSON deserialization too slow: {:?}",
        deserialize_duration
    );
}

/// Test deployment configuration performance
#[test]
fn test_config_creation_performance() {
    // Test that config creation is fast enough for CLI usage
    let start = Instant::now();

    for _ in 0..1000 {
        let _config = DeployConfig {
            binary_path: "program.so".to_string(),
            program_id_path: "program_id.json".to_string(),
            owner_path: "owner.json".to_string(),
            fee_payer_path: "fee_payer.json".to_string(),
            publish_idl: true,
            idl_file_path: None,
            network_selection: "devnet".to_string(),
            json_output: false,
            retry_attempts: 3,
            confirm_large_binaries: false,
        };
    }

    let total_duration = start.elapsed();
    let per_config_duration = total_duration / 1000;

    // Should create 1000 configs in under 10ms (10μs per config)
    assert!(
        total_duration < Duration::from_millis(10),
        "Config creation too slow: {:?} per config",
        per_config_duration
    );
}

/// Test large file loading performance with 10% degradation threshold
#[test]
fn test_large_file_performance_degradation() {
    let dir = tempdir().unwrap();

    // Create baseline: 1MB file
    let baseline_file = dir.path().join("baseline.so");
    let baseline_data = vec![0u8; 1_000_000]; // 1MB
    let mut file = File::create(&baseline_file).unwrap();
    file.write_all(&baseline_data).unwrap();

    // Measure baseline performance
    let start = Instant::now();
    let _baseline = load_program(baseline_file.to_str().unwrap()).unwrap();
    let baseline_duration = start.elapsed();

    // Create larger file: 2MB
    let large_file = dir.path().join("large.so");
    let large_data = vec![0u8; 2_000_000]; // 2MB
    let mut file = File::create(&large_file).unwrap();
    file.write_all(&large_data).unwrap();

    // Measure large file performance
    let start = Instant::now();
    let _large = load_program(large_file.to_str().unwrap()).unwrap();
    let large_duration = start.elapsed();

    // Calculate performance ratio
    let ratio = large_duration.as_nanos() as f64 / baseline_duration.as_nanos() as f64;

    // Performance should scale roughly linearly with file size (within 10% tolerance)
    // 2x file size should not take more than 2.1x time (10% degradation threshold)
    assert!(
        ratio <= 2.1,
        "Performance degradation exceeds 10% threshold. Ratio: {:.2}, Baseline: {:?}, Large: {:?}",
        ratio,
        baseline_duration,
        large_duration
    );
}

/// Test memory allocation efficiency
#[test]
fn test_memory_allocation_efficiency() {
    // Test that memory allocation doesn't cause significant overhead
    let sizes = vec![100_000, 500_000, 1_000_000, 2_000_000];
    let mut durations = Vec::new();

    for size in &sizes {
        let start = Instant::now();
        let _data: Vec<u8> = vec![0; *size];
        let duration = start.elapsed();
        durations.push(duration);
    }

    // Check that memory allocation scales roughly linearly
    for i in 1..durations.len() {
        let ratio = durations[i].as_nanos() as f64 / durations[i - 1].as_nanos() as f64;
        let size_ratio = sizes[i] as f64 / sizes[i - 1] as f64;

        // Memory allocation time should not degrade more than 20% from linear scaling
        assert!(
            ratio <= size_ratio * 1.2,
            "Memory allocation efficiency degraded. Size ratio: {:.2}, Time ratio: {:.2}",
            size_ratio,
            ratio
        );
    }
}

/// Test concurrent RPC client cache performance
#[test]
fn test_concurrent_cache_performance() {
    use std::sync::Arc;
    use std::thread;

    let cache = Arc::new(std::sync::Mutex::new(RpcClientCache::new()));
    let urls = vec![
        "https://api.devnet.solana.com",
        "https://api.testnet.solana.com",
        "https://api.mainnet-beta.solana.com",
    ];

    let start = Instant::now();

    // Spawn multiple threads accessing the cache
    let handles: Vec<_> = (0..10)
        .map(|_| {
            let cache_clone = Arc::clone(&cache);
            let urls_clone = urls.clone();

            thread::spawn(move || {
                for url in urls_clone {
                    let mut cache_guard = cache_clone.lock().unwrap();
                    let _client = cache_guard.get_client(url);
                }
            })
        })
        .collect();

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    let total_duration = start.elapsed();

    // 10 threads * 3 URLs each = 30 operations should complete quickly
    assert!(
        total_duration < Duration::from_millis(100),
        "Concurrent cache access too slow: {:?}",
        total_duration
    );
}

/// Test serialization performance at scale
#[test]
fn test_serialization_performance_at_scale() {
    let results: Vec<DeploymentResult> = (0..100)
        .map(|i| DeploymentResult {
            network: format!("network_{}", i),
            program_id: Pubkey::new_unique(),
            success: i % 2 == 0,
            transaction_signature: Some(format!("signature_{}", i)),
            error_message: if i % 3 == 0 {
                Some(format!("error_{}", i))
            } else {
                None
            },
            retries_attempted: i % 5,
            duration_ms: 1000 + (i as u64) * 100,
        })
        .collect();

    // Test serializing many results
    let start = Instant::now();
    let json = serde_json::to_string(&results).unwrap();
    let serialize_duration = start.elapsed();

    // Should serialize 100 results within 10ms
    assert!(
        serialize_duration < Duration::from_millis(10),
        "Batch serialization too slow: {:?}",
        serialize_duration
    );

    // Test deserializing many results
    let start = Instant::now();
    let _: Vec<DeploymentResult> = serde_json::from_str(&json).unwrap();
    let deserialize_duration = start.elapsed();

    // Should deserialize 100 results within 10ms
    assert!(
        deserialize_duration < Duration::from_millis(10),
        "Batch deserialization too slow: {:?}",
        deserialize_duration
    );
}

/// Test configuration validation performance
#[test]
fn test_config_validation_performance() {
    // Test that config validation doesn't add significant overhead
    let start = Instant::now();

    for i in 0..1000 {
        let config = DeployConfig {
            binary_path: format!("program_{}.so", i),
            program_id_path: format!("program_id_{}.json", i),
            owner_path: format!("owner_{}.json", i),
            fee_payer_path: format!("fee_payer_{}.json", i),
            publish_idl: i % 2 == 0,
            idl_file_path: if i % 3 == 0 {
                Some(format!("idl_{}.json", i))
            } else {
                None
            },
            network_selection: if i % 4 == 0 {
                "all".to_string()
            } else {
                "devnet".to_string()
            },
            json_output: i % 5 == 0,
            retry_attempts: (i % 10) + 1,
            confirm_large_binaries: i % 6 == 0,
        };

        // Basic validation
        assert!(!config.binary_path.is_empty());
        assert!(!config.program_id_path.is_empty());
        assert!(config.retry_attempts > 0);
    }

    let validation_duration = start.elapsed();

    // Should validate 1000 configs within 5ms
    assert!(
        validation_duration < Duration::from_millis(5),
        "Config validation too slow: {:?}",
        validation_duration
    );
}
