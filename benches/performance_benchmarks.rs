//! Performance benchmarks for critical operations

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use osvm::{
    services::ai_service::{PlannedTool, ToolPlan},
    utils::{
        isolation::{ComponentId, ResourceLimits},
        secure_logger::SecureLogger,
    },
};
use serde_json::{json, Value};
use std::collections::HashMap;

fn bench_component_id_generation(c: &mut Criterion) {
    c.bench_function("component_id_generation", |b| {
        b.iter(|| std::hint::black_box(ComponentId::new()))
    });
}

fn bench_component_id_serialization(c: &mut Criterion) {
    let id = ComponentId::new();

    c.bench_function("component_id_serialization", |b| {
        b.iter(|| std::hint::black_box(serde_json::to_string(&id).unwrap()))
    });
}

fn bench_tool_plan_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("tool_plan_creation");

    for num_tools in [1, 5, 10, 20].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_tools),
            num_tools,
            |b, &num_tools| {
                b.iter(|| {
                    let tools: Vec<PlannedTool> = (0..num_tools)
                        .map(|i| PlannedTool {
                            server_id: format!("server-{}", i),
                            tool_name: format!("tool-{}", i),
                            reason: format!("reason-{}", i),
                            args: serde_json::json!({}),
                        })
                        .collect();

                    std::hint::black_box(ToolPlan {
                        reasoning: "Test reasoning".to_string(),
                        osvm_tools_to_use: tools,
                        expected_outcome: "Test outcome".to_string(),
                        raw_ovsm_plan: None,
                    })
                })
            },
        );
    }

    group.finish();
}

fn bench_tool_plan_serialization(c: &mut Criterion) {
    let tools: Vec<PlannedTool> = (0..10)
        .map(|i| PlannedTool {
            server_id: format!("server-{}", i),
            tool_name: format!("tool-{}", i),
            reason: format!("reason-{}", i),
            args: serde_json::json!({}),
        })
        .collect();

    let plan = ToolPlan {
        reasoning: "Test reasoning with some more detailed information".to_string(),
        osvm_tools_to_use: tools,
        expected_outcome: "Expected outcome description".to_string(),
        raw_ovsm_plan: Some("(define x 10) (+ x 20)".to_string()),
    };

    c.bench_function("tool_plan_serialization", |b| {
        b.iter(|| std::hint::black_box(serde_json::to_string(&plan).unwrap()))
    });
}

fn bench_secure_logger_sanitization(c: &mut Criterion) {
    let logger = SecureLogger::new(false);

    let test_inputs = [
        "Simple message without sensitive data",
        "API_KEY=sk-1234567890abcdef with sensitive data",
        "User /home/alice/.ssh/id_rsa accessed the file",
        "Connection from 192.168.1.100 to 10.0.0.5 established",
        "Private key: 5KJvsngHeMpm884wtkJNzQGaCErckhHJBGFsvd3VyK5qMZXj3hS",
    ];

    let mut group = c.benchmark_group("secure_logger_sanitization");

    for (i, input) in test_inputs.iter().enumerate() {
        group.bench_with_input(BenchmarkId::new("sanitize", i), input, |b, input| {
            b.iter(|| std::hint::black_box(logger.sanitize(input)))
        });
    }

    group.finish();
}

fn bench_resource_limits_validation(c: &mut Criterion) {
    c.bench_function("resource_limits_validation", |b| {
        b.iter(|| {
            let limits = ResourceLimits {
                max_cpu_cores: Some(std::hint::black_box(4)),
                max_memory_mb: Some(std::hint::black_box(8192)),
                max_disk_mb: Some(std::hint::black_box(102400)), // 100 GB in MB
                max_network_bandwidth_mbps: Some(std::hint::black_box(1000)),
                max_file_descriptors: Some(std::hint::black_box(10000)),
                max_processes: Some(std::hint::black_box(1000)),
                max_execution_time_sec: None,
            };

            // Validate limits
            std::hint::black_box(
                limits.max_cpu_cores.unwrap_or(0) > 0
                    && limits.max_cpu_cores.unwrap_or(0) <= 128
                    && limits.max_memory_mb.unwrap_or(0) > 0
                    && limits.max_memory_mb.unwrap_or(0) <= 524288
                    && limits.max_disk_mb.unwrap_or(0) > 0
                    && limits.max_disk_mb.unwrap_or(0) <= 1024000,
            )
        })
    });
}

fn bench_json_parsing(c: &mut Criterion) {
    let json_data = json!({
        "reasoning": "This is a test reasoning",
        "osvm_tools_to_use": [
            {
                "server_id": "test-server",
                "tool_name": "test_tool",
                "reason": "Testing",
                "args": {
                    "param1": "value1",
                    "param2": 123,
                    "param3": true
                }
            }
        ],
        "expected_outcome": "Test outcome"
    });

    let json_string = json_data.to_string();

    let mut group = c.benchmark_group("json_operations");

    group.bench_function("parse", |b| {
        b.iter(|| std::hint::black_box(serde_json::from_str::<Value>(&json_string).unwrap()))
    });

    group.bench_function("stringify", |b| {
        b.iter(|| std::hint::black_box(serde_json::to_string(&json_data).unwrap()))
    });

    group.finish();
}

fn bench_hashmap_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("hashmap_operations");

    group.bench_function("create_and_insert", |b| {
        b.iter(|| {
            let mut map: HashMap<String, Value> = HashMap::new();
            for i in 0..100 {
                map.insert(format!("key-{}", i), json!(i));
            }
            std::hint::black_box(map)
        })
    });

    let mut large_map: HashMap<String, Value> = HashMap::new();
    for i in 0..1000 {
        large_map.insert(format!("key-{}", i), json!(i));
    }

    group.bench_function("lookup", |b| {
        b.iter(|| std::hint::black_box(large_map.get("key-500")))
    });

    group.finish();
}

fn bench_string_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_operations");

    group.bench_function("format", |b| {
        b.iter(|| std::hint::black_box(format!("server-{}-tool-{}-reason-{}", 1, 2, 3)))
    });

    let long_string = "a".repeat(10000);

    group.bench_function("clone", |b| {
        b.iter(|| std::hint::black_box(long_string.clone()))
    });

    group.bench_function("substring", |b| {
        b.iter(|| std::hint::black_box(&long_string[0..100]))
    });

    group.finish();
}

fn bench_concurrent_component_creation(c: &mut Criterion) {
    use tokio::runtime::Runtime;

    let rt = Runtime::new().unwrap();

    c.bench_function("concurrent_component_creation", |b| {
        b.iter(|| {
            rt.block_on(async {
                let mut handles = vec![];

                for _ in 0..10 {
                    let handle = tokio::spawn(async { ComponentId::new() });
                    handles.push(handle);
                }

                std::hint::black_box(futures::future::join_all(handles).await)
            })
        })
    });
}

criterion_group!(
    benches,
    bench_component_id_generation,
    bench_component_id_serialization,
    bench_tool_plan_creation,
    bench_tool_plan_serialization,
    bench_secure_logger_sanitization,
    bench_resource_limits_validation,
    bench_json_parsing,
    bench_hashmap_operations,
    bench_string_operations,
    bench_concurrent_component_creation,
);

criterion_main!(benches);
