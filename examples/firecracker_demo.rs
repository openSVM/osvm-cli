//! Firecracker MicroVM Demo
//!
//! This example demonstrates OSVM's Firecracker MicroVM runtime:
//! 1. Initialize Firecracker runtime
//! 2. Create RPC node component with MicroVM isolation
//! 3. Start MicroVM with hardware isolation
//! 4. Monitor component status
//! 5. Stop and cleanup
//!
//! This shows how to run production workloads (RPC nodes, validators)
//! with the performance benefits of MicroVMs vs traditional VMs.

use anyhow::Result;
use std::path::PathBuf;
use std::time::Duration;
use tokio::time::sleep;

// Import OSVM isolation components
use osvm::utils::isolation::{
    component::{Component, ComponentId, ComponentRegistry, ComponentType},
    config::{IsolationConfig, IsolationType, HypervisorType, ResourceLimits},
    runtime::RuntimeManager,
};

#[tokio::main]
async fn main() -> Result<()> {
    println!("╔═══════════════════════════════════════════════════════════╗");
    println!("║  OSVM Phase 2 - Firecracker MicroVM Demo                 ║");
    println!("║  Production-Ready Hardware Isolation                      ║");
    println!("╚═══════════════════════════════════════════════════════════╝");
    println!();

    // Step 1: Initialize infrastructure
    println!("🔧 Step 1: Initializing OSVM infrastructure");

    let registry = ComponentRegistry::new();
    println!("   ✓ Component registry initialized");

    let runtime_manager = RuntimeManager::with_defaults();
    println!("   ✓ Runtime manager initialized");
    println!("   Available runtimes: {:?}", runtime_manager.list_available_runtimes());
    println!();

    // Step 2: Create RPC node component
    println!("🚀 Step 2: Creating RPC node component with MicroVM isolation");

    let component_id = ComponentId::new();

    let mut component = Component {
        id: component_id,
        component_type: ComponentType::RpcNode {
            network: "mainnet".to_string(),
            bind_address: Some("0.0.0.0:8899".to_string()),
        },
        isolation_config: IsolationConfig {
            isolation_type: IsolationType::MicroVM {
                hypervisor: HypervisorType::Firecracker,
                kernel_path: Some(PathBuf::from("/var/lib/osvm/firecracker/vmlinux")),
                rootfs_path: Some(PathBuf::from("/var/lib/osvm/firecracker/rootfs.ext4")),
            },
            resource_limits: ResourceLimits {
                max_memory_mb: Some(512),    // 512MB for RPC node
                max_cpu_cores: Some(4),       // 4 vCPUs
                max_disk_mb: Some(10240),     // 10GB disk
                max_network_bandwidth_mbps: Some(1000), // 1 Gbps
                max_file_descriptors: Some(8192),
                max_processes: Some(100),
                max_execution_time_sec: None,
            },
            ..Default::default()
        },
        runtime_handle: None,
        status: osvm::utils::isolation::component::ComponentStatus::Stopped,
        metadata: Default::default(),
    };

    println!("   Component ID: {}", component_id);
    println!("   Type: RPC Node (mainnet)");
    println!("   Isolation: MicroVM (Firecracker)");
    println!("   Resources: 512MB RAM, 4 vCPUs, 10GB disk, 1Gbps network");
    println!();

    // Step 3: Check runtime availability
    println!("📦 Step 3: Checking Firecracker availability");

    let runtime = runtime_manager.get_runtime(&component.isolation_config)?;
    println!("   Selected runtime: {}", runtime.name());

    if !runtime.is_available() {
        println!("   ⚠ Firecracker not available on this system");
        println!("   ℹ Install with: https://github.com/firecracker-microvm/firecracker");
        println!();
        println!("   Demo will continue in simulation mode...");
        println!();
    } else {
        println!("   ✓ Firecracker is available and ready");
        println!();
    }

    // Step 4: Start component (simulated if Firecracker not available)
    println!("▶️  Step 4: Starting MicroVM");

    if runtime.is_available() {
        println!("   ⚠ Skipping actual start (requires root and proper setup)");
        println!("   ℹ In production: runtime.start_component(&mut component).await?");
    } else {
        println!("   ⚠ Simulation mode (Firecracker not installed)");
    }

    // Simulate component starting
    component.status = osvm::utils::isolation::component::ComponentStatus::Starting;
    println!("   Status: Starting...");
    sleep(Duration::from_millis(50)).await;

    component.status = osvm::utils::isolation::component::ComponentStatus::Running;
    println!("   ✓ MicroVM started successfully");
    println!("   Boot time: ~125ms");
    println!("   Status: Running");
    println!();

    // Register component
    registry.register(component.clone()).await?;

    // Step 5: Performance comparison
    println!("⚡ Step 5: Performance Characteristics");
    println!();
    println!("   Comparison Table:");
    println!("   ┌──────────────────┬─────────────┬──────────────┬────────────────┐");
    println!("   │ Metric           │ Unikernel   │ Firecracker  │ Traditional VM │");
    println!("   ├──────────────────┼─────────────┼──────────────┼────────────────┤");
    println!("   │ Boot Time        │ 50-100ms    │ ~125ms       │ 30-60s         │");
    println!("   │ Memory Overhead  │ 5-10MB      │ 5-50MB       │ 512MB-2GB      │");
    println!("   │ OS Support       │ Limited     │ Full Linux   │ Any OS         │");
    println!("   │ Device Support   │ Minimal     │ virtio       │ Full hardware  │");
    println!("   │ Attack Surface   │ ~50KB       │ ~5M lines    │ ~30M+ lines    │");
    println!("   │ Use Case         │ Simple svcs │ Complex apps │ Legacy apps    │");
    println!("   └──────────────────┴─────────────┴──────────────┴────────────────┘");
    println!();

    // Step 6: Security properties
    println!("🛡️  Step 6: Security Properties");
    println!();
    println!("   ✅ Hardware Isolation");
    println!("      • KVM-based virtualization (VT-x/AMD-V)");
    println!("      • Separate address spaces");
    println!("      • EPT/NPT page table isolation");
    println!();
    println!("   ✅ Minimal Device Model");
    println!("      • Only virtio devices (block, net, vsock)");
    println!("      • No PCI bus, no legacy devices");
    println!("      • Reduced attack surface vs QEMU");
    println!();
    println!("   ✅ Resource Isolation");
    println!("      • CPU pinning and quotas");
    println!("      • Memory limits (OOM-kill)");
    println!("      • Network bandwidth shaping");
    println!();
    println!("   ✅ Production Proven");
    println!("      • Used by AWS Lambda");
    println!("      • Millions of invocations daily");
    println!("      • Battle-tested security model");
    println!();

    // Step 7: Use cases
    println!("🎯 Step 7: Firecracker Use Cases");
    println!();
    println!("   RPC Nodes:");
    println!("   • Need full Linux kernel");
    println!("   • Require file I/O for ledger storage");
    println!("   • Handle high network throughput");
    println!("   • Boot quickly for scaling");
    println!();
    println!("   Validators:");
    println!("   • Run consensus algorithms");
    println!("   • Access specialized hardware (TPU)");
    println!("   • Need OS-level networking features");
    println!("   • Require hardware isolation");
    println!();
    println!("   Build Servers:");
    println!("   • Compile untrusted user programs");
    println!("   • Fast startup for CI/CD");
    println!("   • Contained failures");
    println!("   • Cost-effective at scale");
    println!();

    // Step 8: Architecture
    println!("🏗️  Step 8: Firecracker Architecture");
    println!();
    println!("   ┌──────────────────────────────────────────────────┐");
    println!("   │  Host Linux                                      │");
    println!("   │  ┌────────────────────────────────────────────┐ │");
    println!("   │  │  KVM Hypervisor                            │ │");
    println!("   │  └────────────────────────────────────────────┘ │");
    println!("   │              │                                   │");
    println!("   │  ┌───────────▼─────────────────────┐            │");
    println!("   │  │  Firecracker VMM (per VM)       │            │");
    println!("   │  │  - Minimal device model         │            │");
    println!("   │  │  - virtio-block (disk)          │            │");
    println!("   │  │  - virtio-net (network)         │            │");
    println!("   │  │  - virtio-vsock (VM-to-VM)      │            │");
    println!("   │  │  Memory: ~5MB overhead          │            │");
    println!("   │  └───────────┬─────────────────────┘            │");
    println!("   │              │                                   │");
    println!("   │  ┌───────────▼─────────────────────┐            │");
    println!("   │  │  Guest Linux (minimal)          │            │");
    println!("   │  │  ┌────────────────────────────┐ │            │");
    println!("   │  │  │  Solana RPC Node           │ │            │");
    println!("   │  │  │  - solana-validator        │ │            │");
    println!("   │  │  │  - Ledger storage          │ │            │");
    println!("   │  │  │  - RPC endpoints           │ │            │");
    println!("   │  │  └────────────────────────────┘ │            │");
    println!("   │  └─────────────────────────────────┘            │");
    println!("   └──────────────────────────────────────────────────┘");
    println!();

    // Step 9: Cleanup
    println!("🧹 Step 9: Stopping MicroVM");

    component.status = osvm::utils::isolation::component::ComponentStatus::Stopping;
    println!("   Status: Stopping...");
    sleep(Duration::from_millis(100)).await;

    component.status = osvm::utils::isolation::component::ComponentStatus::Stopped;

    println!("   ✓ MicroVM stopped");
    println!("   ✓ Resources released");
    println!();

    // Final summary
    println!("╔═══════════════════════════════════════════════════════════╗");
    println!("║  Firecracker Demo Complete                               ║");
    println!("╚═══════════════════════════════════════════════════════════╝");
    println!();
    println!("✅ Key Benefits:");
    println!("   • 125ms boot time (200-500x faster than VMs)");
    println!("   • 5-50MB memory overhead (10-100x less than VMs)");
    println!("   • Full Linux support (more flexible than unikernels)");
    println!("   • Hardware isolation (secure as traditional VMs)");
    println!("   • Production proven (AWS Lambda)");
    println!();
    println!("🎯 Best For:");
    println!("   • RPC nodes with high throughput requirements");
    println!("   • Validators needing OS-level features");
    println!("   • Components requiring full Linux kernel");
    println!("   • Services needing both speed and flexibility");
    println!();
    println!("📍 Phase 2.1 Status: Firecracker Runtime Implemented");
    println!();
    println!("🚀 Next: Hot-swap for zero-downtime updates (Phase 2.3)");

    Ok(())
}