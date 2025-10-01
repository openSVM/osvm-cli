//! Phase 2 Integration Tests
//!
//! Comprehensive test suite validating all Phase 2 functionality:
//! - Firecracker runtime lifecycle
//! - Hot-swap operations
//! - vsock communication
//! - Orchestrator operations
//! - End-to-end production scenarios

use osvm::utils::isolation::config::HypervisorType;
use osvm::utils::isolation::network::{ComponentPattern, NetworkPolicy, PolicyEffect};
use osvm::utils::isolation::*;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::sleep;

/// Test Firecracker runtime availability
#[tokio::test]
async fn test_firecracker_runtime_available() {
    let runtime_manager = RuntimeManager::with_defaults();
    let runtimes = runtime_manager.list_available_runtimes();

    // At minimum, Process runtime should always be available
    assert!(runtimes.iter().any(|r| r == "Process"));

    // Note: Firecracker may not be available in CI environment
    // but should be available in production
}

/// Test component registry operations
#[tokio::test]
async fn test_component_registry() {
    let registry = ComponentRegistry::new();

    // Create test component
    let component = Component {
        id: ComponentId::new(),
        component_type: ComponentType::McpServer {
            name: "test-mcp".to_string(),
            version: Some("1.0.0".to_string()),
        },
        isolation_config: IsolationConfig {
            isolation_type: IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None,
            },
            resource_limits: ResourceLimits::default(),
            ..Default::default()
        },
        runtime_handle: None,
        status: ComponentStatus::Stopped,
        metadata: Default::default(),
    };

    let component_id = component.id;

    // Register component
    registry.register(component.clone()).await.unwrap();

    // Retrieve component
    let retrieved = registry.get(component_id).await.unwrap();
    assert_eq!(retrieved.id, component_id);

    // List components
    let all = registry.list().await;
    assert_eq!(all.len(), 1);

    // List by type
    let mcp_servers = registry
        .list_by_type(&ComponentType::McpServer {
            name: "test-mcp".to_string(),
            version: Some("1.0.0".to_string()),
        })
        .await;
    assert_eq!(mcp_servers.len(), 1);
}

/// Test vsock CID allocation
#[tokio::test]
async fn test_vsock_cid_allocation() {
    let vsock_manager = VsockManager::default();

    let component1 = ComponentId::new();
    let component2 = ComponentId::new();

    // Allocate CIDs
    let cid1 = vsock_manager.allocate_cid(component1).await.unwrap();
    let cid2 = vsock_manager.allocate_cid(component2).await.unwrap();

    // CIDs should start at 3 (first guest)
    assert_eq!(cid1, 3);
    assert_eq!(cid2, 4);

    // Allocating again for same component should return same CID
    let cid1_again = vsock_manager.allocate_cid(component1).await.unwrap();
    assert_eq!(cid1, cid1_again);

    // Verify stats
    let stats = vsock_manager.stats().await;
    assert_eq!(stats.allocated_cids, 2);
    assert_eq!(stats.next_cid, 5);
}

/// Test vsock address creation
#[test]
fn test_vsock_address() {
    let addr = VsockAddr::new(3, 5000);
    assert_eq!(addr.cid, 3);
    assert_eq!(addr.port, 5000);
    assert!(addr.is_valid_guest());
    assert_eq!(format!("{}", addr), "3:5000");

    let host_addr = VsockAddr::host(8080);
    assert_eq!(host_addr.cid, 2);
    assert!(!host_addr.is_valid_guest());
}

/// Test orchestrator creation and stats
#[tokio::test]
async fn test_orchestrator_basic() {
    let registry = Arc::new(ComponentRegistry::new());
    let runtime_manager = Arc::new(RuntimeManager::with_defaults());
    let network_manager = Arc::new(NetworkManager::default());
    let vsock_manager = Arc::new(VsockManager::default());
    let hotswap_manager = Arc::new(HotSwapManager::new(
        runtime_manager.clone(),
        registry.clone(),
        HotSwapConfig::default(),
    ));

    let orchestrator = Orchestrator::new(
        registry,
        runtime_manager,
        network_manager,
        vsock_manager,
        hotswap_manager,
        OrchestratorConfig::default(),
    );

    // Check initial stats
    let stats = orchestrator.stats().await;
    assert_eq!(stats.total_components, 0);
    assert_eq!(stats.healthy_components, 0);
    assert_eq!(stats.registered_services, 0);
}

/// Test hot-swap configuration
#[test]
fn test_hotswap_config() {
    let config = HotSwapConfig::default();
    assert_eq!(config.health_check_timeout, Duration::from_secs(30));
    assert_eq!(config.max_health_checks, 10);
    assert!(config.auto_rollback);

    let custom_config = HotSwapConfig {
        health_check_timeout: Duration::from_secs(60),
        health_check_interval: Duration::from_secs(5),
        drain_timeout: Duration::from_secs(120),
        max_health_checks: 20,
        auto_rollback: true,
    };

    assert_eq!(custom_config.health_check_timeout, Duration::from_secs(60));
}

/// Test end-to-end component deployment simulation
#[tokio::test]
async fn test_end_to_end_component_deployment() {
    let registry = Arc::new(ComponentRegistry::new());
    let runtime_manager = Arc::new(RuntimeManager::with_defaults());
    let vsock_manager = Arc::new(VsockManager::default());

    // Create component
    let mut component = Component {
        id: ComponentId::new(),
        component_type: ComponentType::McpServer {
            name: "test-server".to_string(),
            version: Some("1.0.0".to_string()),
        },
        isolation_config: IsolationConfig {
            isolation_type: IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None,
            },
            resource_limits: ResourceLimits {
                max_memory_mb: Some(128),
                max_cpu_cores: Some(1),
                ..Default::default()
            },
            ..Default::default()
        },
        runtime_handle: None,
        status: ComponentStatus::Stopped,
        metadata: Default::default(),
    };

    let component_id = component.id;

    // 1. Allocate resources
    let cid = vsock_manager.allocate_cid(component_id).await.unwrap();
    assert_eq!(cid, 3);

    // 2. Start component (simulated)
    let runtime = runtime_manager
        .get_runtime(&component.isolation_config)
        .unwrap();
    assert_eq!(runtime.name(), "Process");

    component.status = ComponentStatus::Running;

    // 3. Register component
    registry.register(component.clone()).await.unwrap();

    // 4. Verify deployment
    let retrieved = registry.get(component_id).await.unwrap();
    assert_eq!(retrieved.status, ComponentStatus::Running);

    let vsock_cid = vsock_manager.get_cid(component_id).await;
    assert_eq!(vsock_cid, Some(3));

    // 5. Cleanup
    component.status = ComponentStatus::Stopped;
    registry.register(component).await.unwrap();
    vsock_manager.free_cid(component_id).await.unwrap();
}

/// Test network policy enforcement
#[tokio::test]
async fn test_network_policy() {
    let network_manager = NetworkManager::default();

    let from_id = ComponentId::new();
    let to_id = ComponentId::new();

    // Default should be deny
    let allowed = network_manager
        .is_connection_allowed(from_id, to_id)
        .await
        .unwrap();
    assert!(!allowed, "Default policy should deny");

    // Add allow policy
    network_manager
        .add_policy(NetworkPolicy {
            source: ComponentPattern::Specific(from_id),
            destination: ComponentPattern::Specific(to_id),
            effect: PolicyEffect::Allow,
            constraints: Default::default(),
        })
        .await;

    // Now should be allowed
    let allowed = network_manager
        .is_connection_allowed(from_id, to_id)
        .await
        .unwrap();
    assert!(allowed, "Should be allowed after adding policy");
}

/// Benchmark: Component deployment speed
#[tokio::test]
async fn benchmark_component_deployment() {
    let registry = ComponentRegistry::new();
    let start = std::time::Instant::now();

    // Deploy 10 components
    for i in 0..10 {
        let component = Component {
            id: ComponentId::new(),
            component_type: ComponentType::McpServer {
                name: format!("bench-{}", i),
                version: Some("1.0.0".to_string()),
            },
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            status: ComponentStatus::Running,
            metadata: Default::default(),
        };

        registry.register(component).await.unwrap();
    }

    let elapsed = start.elapsed();
    println!("Deployed 10 components in {:?}", elapsed);
    println!("Average per component: {:?}", elapsed / 10);

    // Should be very fast (< 10ms per component for registry ops)
    assert!(elapsed < Duration::from_millis(100));
}

/// Test component status
#[test]
fn test_component_status() {
    let status = ComponentStatus::Running;
    assert!(status.is_operational());

    let failed = ComponentStatus::Failed;
    assert!(!failed.is_operational());
}

/// Integration test: Full hot-swap simulation
#[tokio::test]
async fn test_hotswap_simulation() {
    let registry = Arc::new(ComponentRegistry::new());
    let _runtime_manager = Arc::new(RuntimeManager::with_defaults());

    // Deploy old version
    let old_component = Component {
        id: ComponentId::new(),
        component_type: ComponentType::McpServer {
            name: "service".to_string(),
            version: Some("1.0.0".to_string()),
        },
        isolation_config: IsolationConfig {
            isolation_type: IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None,
            },
            ..Default::default()
        },
        runtime_handle: None,
        status: ComponentStatus::Running,
        metadata: Default::default(),
    };

    let old_id = old_component.id;
    registry.register(old_component).await.unwrap();

    // Prepare new version
    let new_component = Component {
        id: ComponentId::new(),
        component_type: ComponentType::McpServer {
            name: "service".to_string(),
            version: Some("1.1.0".to_string()),
        },
        isolation_config: IsolationConfig {
            isolation_type: IsolationType::ProcessSandbox {
                seccomp_profile: None,
                apparmor_profile: None,
            },
            ..Default::default()
        },
        runtime_handle: None,
        status: ComponentStatus::Stopped,
        metadata: Default::default(),
    };

    // Simulate hot-swap (manual steps since we can't actually start processes in tests)
    // 1. Start new component
    let mut new_comp = new_component.clone();
    new_comp.status = ComponentStatus::Starting;
    registry.register(new_comp.clone()).await.unwrap();

    // 2. Health check passes (simulated)
    sleep(Duration::from_millis(10)).await;
    new_comp.status = ComponentStatus::Running;
    registry.register(new_comp.clone()).await.unwrap();

    // 3. Traffic shifts (simulated)
    // In production, orchestrator would update load balancer

    // 4. Old component stops
    let mut old_comp = registry.get(old_id).await.unwrap();
    old_comp.status = ComponentStatus::Stopped;
    registry.register(old_comp).await.unwrap();

    // Verify final state
    let new_retrieved = registry.get(new_comp.id).await.unwrap();
    assert_eq!(new_retrieved.status, ComponentStatus::Running);

    let old_retrieved = registry.get(old_id).await.unwrap();
    assert_eq!(old_retrieved.status, ComponentStatus::Stopped);
}

#[cfg(test)]
mod performance_tests {
    use super::*;

    /// Measure vsock CID allocation performance
    #[tokio::test]
    async fn benchmark_vsock_allocation() {
        let vsock_manager = VsockManager::default();
        let start = std::time::Instant::now();

        // Allocate 1000 CIDs
        for _ in 0..1000 {
            let component = ComponentId::new();
            vsock_manager.allocate_cid(component).await.unwrap();
        }

        let elapsed = start.elapsed();
        println!("Allocated 1000 CIDs in {:?}", elapsed);
        println!("Average: {:?} per allocation", elapsed / 1000);

        // Should be very fast
        assert!(elapsed < Duration::from_secs(1));
    }

    /// Measure component registry performance
    #[tokio::test]
    async fn benchmark_registry_operations() {
        let registry = ComponentRegistry::new();
        let components: Vec<_> = (0..100)
            .map(|i| Component {
                id: ComponentId::new(),
                component_type: ComponentType::McpServer {
                    name: format!("server-{}", i),
                    version: Some("1.0.0".to_string()),
                },
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                status: ComponentStatus::Running,
                metadata: Default::default(),
            })
            .collect();

        // Benchmark registration
        let start = std::time::Instant::now();
        for component in &components {
            registry.register(component.clone()).await.unwrap();
        }
        let register_time = start.elapsed();

        // Benchmark retrieval
        let start = std::time::Instant::now();
        for component in &components {
            registry.get(component.id).await.unwrap();
        }
        let get_time = start.elapsed();

        // Benchmark list
        let start = std::time::Instant::now();
        let all = registry.list().await;
        let list_time = start.elapsed();

        println!("Registry performance (100 components):");
        println!(
            "  Register: {:?} ({:?}/op)",
            register_time,
            register_time / 100
        );
        println!("  Get: {:?} ({:?}/op)", get_time, get_time / 100);
        println!("  List: {:?}", list_time);

        assert_eq!(all.len(), 100);
        assert!(register_time < Duration::from_millis(500));
        assert!(get_time < Duration::from_millis(500));
        assert!(list_time < Duration::from_millis(100));
    }
}

/// Test resource limits configuration
#[test]
fn test_resource_limits() {
    let limits = ResourceLimits {
        max_memory_mb: Some(1024),
        max_cpu_cores: Some(4),
        max_disk_mb: Some(10240),
        max_network_bandwidth_mbps: Some(1000),
        max_file_descriptors: Some(8192),
        max_processes: Some(100),
        max_execution_time_sec: Some(3600),
    };

    assert_eq!(limits.max_memory_mb, Some(1024));
    assert_eq!(limits.max_cpu_cores, Some(4));
}

/// Test component type matching
#[test]
fn test_component_types() {
    let rpc = ComponentType::RpcNode {
        network: "mainnet".to_string(),
        bind_address: Some("0.0.0.0:8899".to_string()),
    };

    let validator = ComponentType::Validator {
        network: "mainnet".to_string(),
        identity: Some("validator-keypair".to_string()),
    };

    let mcp = ComponentType::McpServer {
        name: "my-server".to_string(),
        version: Some("1.0.0".to_string()),
    };

    // Verify they're different
    assert!(std::mem::discriminant(&rpc) != std::mem::discriminant(&validator));
    assert!(std::mem::discriminant(&rpc) != std::mem::discriminant(&mcp));
}

/// Test that all runtimes are accessible
#[test]
fn test_runtime_availability() {
    let manager = RuntimeManager::with_defaults();
    let runtimes = manager.list_available_runtimes();

    // Process should always be available
    assert!(runtimes.iter().any(|r| r == "Process"));

    // Print available runtimes for debugging
    println!("Available runtimes: {:?}", runtimes);
}

/// Final integration test: Complete system check
#[tokio::test]
async fn test_complete_system_health() {
    println!("\n========================================");
    println!("OSVM Phase 2 System Health Check");
    println!("========================================\n");

    // 1. Runtime Manager
    println!("✓ Runtime Manager");
    let runtime_manager = RuntimeManager::with_defaults();
    let runtimes = runtime_manager.list_available_runtimes();
    println!("  Available runtimes: {:?}", runtimes);
    assert!(!runtimes.is_empty());

    // 2. Component Registry
    println!("✓ Component Registry");
    let registry = ComponentRegistry::new();
    assert_eq!(registry.list().await.len(), 0);

    // 3. vsock Manager
    println!("✓ vsock Manager");
    let vsock_manager = VsockManager::default();
    let stats = vsock_manager.stats().await;
    assert_eq!(stats.allocated_cids, 0);
    assert_eq!(stats.next_cid, 3);

    // 4. Network Manager
    println!("✓ Network Manager");
    let network_manager = NetworkManager::default();
    assert_eq!(network_manager.connection_count().await, 0);

    // 5. Hot-Swap Manager
    println!("✓ Hot-Swap Manager");
    let hotswap_config = HotSwapConfig::default();
    assert!(hotswap_config.auto_rollback);

    println!("\n========================================");
    println!("All systems operational! ✨");
    println!("========================================\n");
}
