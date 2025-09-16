use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use osvm::utils::ebpf_deploy::{load_program, load_program_id, DeployConfig, RpcClientCache};
use std::fs::File;
use std::io::Write;
use tempfile::tempdir;

/// Benchmark program loading performance
fn benchmark_load_program(c: &mut Criterion) {
    let dir = tempdir().unwrap();

    // Create test programs of different sizes
    let sizes = vec![
        ("small", 10_000),     // 10KB
        ("medium", 100_000),   // 100KB
        ("large", 1_000_000),  // 1MB
        ("xlarge", 2_000_000), // 2MB
    ];

    let mut group = c.benchmark_group("load_program");

    for (name, size) in sizes {
        let program_path = dir.path().join(format!("program_{}.so", name));
        let program_data = vec![0u8; size];
        let mut file = File::create(&program_path).unwrap();
        file.write_all(&program_data).unwrap();

        group.bench_with_input(
            BenchmarkId::new("load_program", name),
            &program_path.to_string_lossy().to_string(),
            |b, path| {
                b.iter(|| load_program(path).unwrap());
            },
        );
    }

    group.finish();
}

/// Benchmark program ID loading performance
fn benchmark_load_program_id(c: &mut Criterion) {
    let dir = tempdir().unwrap();
    let program_id_file = dir.path().join("program_id.json");

    // Create test program ID file
    let program_id_content = r#"{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}"#;
    let mut file = File::create(&program_id_file).unwrap();
    file.write_all(program_id_content.as_bytes()).unwrap();

    c.bench_function("load_program_id", |b| {
        b.iter(|| load_program_id(program_id_file.to_str().unwrap()).unwrap());
    });
}

/// Benchmark RPC client cache performance
fn benchmark_rpc_client_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("rpc_client_cache");

    // Test cache performance with multiple URLs
    let urls = vec![
        "https://api.devnet.solana.com",
        "https://api.testnet.solana.com",
        "https://api.mainnet-beta.solana.com",
    ];

    group.bench_function("cache_miss", |b| {
        b.iter(|| {
            let mut cache = RpcClientCache::new();
            for url in &urls {
                cache.get_client(url);
            }
        });
    });

    group.bench_function("cache_hit", |b| {
        let mut cache = RpcClientCache::new();
        // Pre-populate cache
        for url in &urls {
            cache.get_client(url);
        }

        b.iter(|| {
            for url in &urls {
                cache.get_client(url);
            }
        });
    });

    group.finish();
}

/// Benchmark deployment configuration creation
fn benchmark_deploy_config_creation(c: &mut Criterion) {
    c.bench_function("deploy_config_creation", |b| {
        b.iter(|| DeployConfig {
            binary_path: "program.so".to_string(),
            program_id_path: "program_id.json".to_string(),
            owner_path: "owner.json".to_string(),
            fee_payer_path: "fee_payer.json".to_string(),
            publish_idl: true,
            idl_file_path: Some("idl.json".to_string()),
            network_selection: "all".to_string(),
            json_output: false,
            retry_attempts: 3,
            confirm_large_binaries: false,
        });
    });
}

/// Benchmark JSON serialization performance
fn benchmark_json_operations(c: &mut Criterion) {
    use osvm::utils::ebpf_deploy::DeploymentResult;
    use solana_sdk::pubkey::Pubkey;

    let result = DeploymentResult {
        network: "devnet".to_string(),
        program_id: Pubkey::new_unique(),
        success: true,
        transaction_signature: Some("test_signature_12345".to_string()),
        error_message: None,
        retries_attempted: 3,
        duration_ms: 5000,
    };

    let mut group = c.benchmark_group("json_operations");

    group.bench_function("serialize", |b| {
        b.iter(|| serde_json::to_string(&result).unwrap());
    });

    let json_str = serde_json::to_string(&result).unwrap();
    group.bench_function("deserialize", |b| {
        b.iter(|| {
            let _: DeploymentResult = serde_json::from_str(&json_str).unwrap();
        });
    });

    group.finish();
}

/// Benchmark file operations for deployment
fn benchmark_file_operations(c: &mut Criterion) {
    let dir = tempdir().unwrap();

    // Create test files of various sizes
    let file_sizes = vec![1_000, 10_000, 100_000, 1_000_000];
    let mut test_files = Vec::new();

    for size in file_sizes {
        let file_path = dir.path().join(format!("test_{}.dat", size));
        let data = vec![0u8; size];
        let mut file = File::create(&file_path).unwrap();
        file.write_all(&data).unwrap();
        test_files.push((size, file_path));
    }

    let mut group = c.benchmark_group("file_operations");

    for (size, file_path) in test_files {
        group.bench_with_input(
            BenchmarkId::new("read_file", size),
            &file_path,
            |b, path| {
                b.iter(|| std::fs::read(path).unwrap());
            },
        );
    }

    group.finish();
}

/// Benchmark memory usage and allocation patterns
fn benchmark_memory_allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_allocation");

    // Test vector allocation for different program sizes
    let sizes = vec![10_000, 100_000, 1_000_000, 5_000_000];

    for size in sizes {
        group.bench_with_input(
            BenchmarkId::new("vec_allocation", size),
            &size,
            |b, &size| {
                b.iter(|| {
                    let _data: Vec<u8> = vec![0; size];
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_load_program,
    benchmark_load_program_id,
    benchmark_rpc_client_cache,
    benchmark_deploy_config_creation,
    benchmark_json_operations,
    benchmark_file_operations,
    benchmark_memory_allocation
);

criterion_main!(benches);
