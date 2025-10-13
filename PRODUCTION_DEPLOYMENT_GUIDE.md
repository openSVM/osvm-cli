# OSVM Production Deployment Guide
## Zero-Downtime Blockchain Infrastructure

**Version**: 2.0 (Phase 2 Complete)
**Target**: Production Operators & DevOps Teams
**Last Updated**: 2025-09-30

---

## 🎯 Quick Start (5 Minutes)

### Prerequisites
```bash
# System requirements
- Linux kernel 4.14+ (with KVM support)
- Rust 1.70+
- 8GB+ RAM
- 50GB+ disk space

# Check KVM support
lsmod | grep kvm
# Should show: kvm_intel or kvm_amd

# Install dependencies
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    libssl-dev \
    pkg-config \
    qemu-kvm \
    libvirt-daemon-system
```

### Installation
```bash
# Clone repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Build release binary
cargo build --release

# Install
sudo cp target/release/osvm /usr/bin/osvm
sudo chmod +x /usr/bin/osvm

# Verify
osvm --version
# Should show: osvm 0.8.3
```

### Quick Deploy RPC Node (Process Runtime)
```bash
# Start local RPC node (development)
osvm rpc local

# Your RPC node is now running on http://localhost:8899
```

---

## 🏗️ Production Deployment Architecture

### Recommended Setup

```
┌─────────────────────────────────────────────────────────┐
│  Production Host (Dedicated Server)                     │
│  Specs: 32 vCPU, 128GB RAM, 2TB NVMe                   │
│                                                         │
│  ┌───────────────────────────────────────────────────┐ │
│  │  OSVM Core Orchestrator                           │ │
│  │  Port: 9000 (internal only)                       │ │
│  └───────────────────────────────────────────────────┘ │
│                      │                                  │
│  ┌───────────────────┴──────────────────────────────┐  │
│  │  KVM Hypervisor                                   │  │
│  └───────────────────┬──────────────────────────────┘  │
│         │             │             │                   │
│  ┌──────▼──────┐ ┌───▼──────┐ ┌───▼──────┐           │
│  │ RPC Node 1  │ │RPC Node 2│ │Validator │           │
│  │ Firecracker │ │Firecracker│ │Firecracker│          │
│  │ 8GB/8vCPU   │ │8GB/8vCPU │ │16GB/16vCPU│          │
│  │ Port: 8899  │ │Port: 8900│ │Port: 8001 │          │
│  └─────────────┘ └──────────┘ └───────────┘          │
│                                                         │
│  ┌─────────────────────────────────────────────────┐   │
│  │  HAProxy Load Balancer                          │   │
│  │  Port: 80/443 (public)                          │   │
│  │  → Round-robin to RPC nodes                     │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

---

## 📋 Step-by-Step Production Deployment

### Step 1: Prepare Host System

```bash
# 1. Update system
sudo apt-get update && sudo apt-get upgrade -y

# 2. Install Firecracker
FIRECRACKER_VERSION="v1.7.0"
curl -LOJ https://github.com/firecracker-microvm/firecracker/releases/download/${FIRECRACKER_VERSION}/firecracker-${FIRECRACKER_VERSION}-x86_64.tgz
tar -xzf firecracker-${FIRECRACKER_VERSION}-x86_64.tgz
sudo mv release-${FIRECRACKER_VERSION}-x86_64/firecracker-${FIRECRACKER_VERSION}-x86_64 /usr/bin/firecracker
sudo chmod +x /usr/bin/firecracker

# 3. Verify Firecracker
firecracker --version

# 4. Create OSVM directories
sudo mkdir -p /var/lib/osvm/{firecracker,certs,vsock}
sudo mkdir -p /var/lib/osvm/firecracker/{kernels,rootfs,vms}
sudo chown -R $(whoami):$(whoami) /var/lib/osvm

# 5. Configure kernel parameters
cat <<EOF | sudo tee -a /etc/sysctl.conf
# OSVM Production Settings
net.ipv4.ip_forward = 1
net.ipv4.conf.all.forwarding = 1
vm.max_map_count = 262144
fs.file-max = 2097152
EOF

sudo sysctl -p
```

### Step 2: Build Firecracker Kernel & Rootfs

```bash
# Download pre-built kernel (recommended)
cd /var/lib/osvm/firecracker/kernels
wget https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.7/x86_64/vmlinux-5.10.bin
mv vmlinux-5.10.bin vmlinux

# Or build custom kernel (advanced)
# See: https://github.com/firecracker-microvm/firecracker/blob/main/docs/rootfs-and-kernel-setup.md

# Build minimal rootfs with Solana
cd /var/lib/osvm/firecracker/rootfs
# This creates a minimal Alpine Linux rootfs with Solana installed
cat > build-rootfs.sh <<'SCRIPT'
#!/bin/bash
set -e

# Create rootfs directory
mkdir -p rootfs
cd rootfs

# Download Alpine mini rootfs
wget https://dl-cdn.alpinelinux.org/alpine/v3.19/releases/x86_64/alpine-minirootfs-3.19.0-x86_64.tar.gz
tar xzf alpine-minirootfs-3.19.0-x86_64.tar.gz
rm alpine-minirootfs-3.19.0-x86_64.tar.gz

# Install Solana
mkdir -p opt
cd opt
wget https://github.com/solana-labs/solana/releases/download/v1.18.0/solana-release-x86_64-unknown-linux-gnu.tar.bz2
tar xjf solana-release-x86_64-unknown-linux-gnu.tar.bz2
mv solana-release solana
rm solana-release-x86_64-unknown-linux-gnu.tar.bz2

# Create ext4 image
cd ..
dd if=/dev/zero of=../rootfs.ext4 bs=1M count=2048
mkfs.ext4 -F ../rootfs.ext4
mkdir -p /tmp/rootfs-mount
sudo mount ../rootfs.ext4 /tmp/rootfs-mount
sudo cp -a rootfs/* /tmp/rootfs-mount/
sudo umount /tmp/rootfs-mount
rmdir /tmp/rootfs-mount

echo "Rootfs created: /var/lib/osvm/firecracker/rootfs/rootfs.ext4"
SCRIPT

chmod +x build-rootfs.sh
./build-rootfs.sh
```

### Step 3: Configure OSVM

```bash
# Create OSVM configuration
mkdir -p ~/.config/osvm
cat > ~/.config/osvm/config.yml <<EOF
# OSVM Production Configuration

# RPC Settings
json_rpc_url: https://api.mainnet-beta.solana.com
commitment: confirmed

# Isolation Settings
isolation:
  default_runtime: firecracker
  firecracker:
    kernel: /var/lib/osvm/firecracker/kernels/vmlinux
    rootfs: /var/lib/osvm/firecracker/rootfs/rootfs.ext4
    work_dir: /var/lib/osvm/firecracker/vms
    use_jailer: true
    default_vcpus: 4
    default_memory_mb: 4096
    enable_vsock: true

# Orchestration Settings
orchestrator:
  health_check_interval: 30s
  max_restart_attempts: 3
  auto_restart: true
  auto_scale: false
  enable_service_mesh: true

# Network Settings
network:
  mTLS_enabled: true
  ca_root_cert: /var/lib/osvm/certs/ca.crt
  default_policy: deny

# vsock Settings
vsock:
  socket_dir: /var/lib/osvm/vsock
  start_cid: 3
  start_port: 1024
EOF
```

### Step 4: Deploy RPC Node Fleet

```bash
# Using OSVM Rust API (recommended for production)
cat > deploy-rpc-fleet.rs <<'RUST'
use osvm::utils::isolation::*;
use std::sync::Arc;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize OSVM infrastructure
    let registry = Arc::new(ComponentRegistry::new());
    let runtime_manager = Arc::new(RuntimeManager::with_defaults());
    let network_manager = Arc::new(NetworkManager::default());
    let vsock_manager = Arc::new(VsockManager::default());
    let hotswap_manager = Arc::new(HotSwapManager::new(
        runtime_manager.clone(),
        registry.clone(),
        HotSwapConfig::default(),
    ));

    let orchestrator = Arc::new(Orchestrator::new(
        registry.clone(),
        runtime_manager,
        network_manager,
        vsock_manager,
        hotswap_manager,
        OrchestratorConfig {
            health_check_interval: std::time::Duration::from_secs(30),
            max_restart_attempts: 3,
            auto_restart: true,
            ..Default::default()
        },
    ));

    // Start orchestrator
    orchestrator.start().await?;

    // Deploy RPC nodes
    for i in 1..=3 {
        let component = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "mainnet".to_string(),
                bind_address: Some(format!("0.0.0.0:{}", 8899 + i - 1)),
            },
            isolation_config: IsolationConfig {
                isolation_type: IsolationType::MicroVM {
                    hypervisor: HypervisorType::Firecracker,
                    kernel_path: Some("/var/lib/osvm/firecracker/kernels/vmlinux".into()),
                    rootfs_path: Some("/var/lib/osvm/firecracker/rootfs/rootfs.ext4".into()),
                },
                resource_limits: ResourceLimits {
                    max_memory_mb: Some(8192),
                    max_cpu_cores: Some(8),
                    max_disk_mb: Some(102400),
                    max_network_bandwidth_mbps: Some(10000),
                    ..Default::default()
                },
                ..Default::default()
            },
            ..Default::default()
        };

        let component_id = orchestrator.deploy_component(component).await?;
        println!("✓ Deployed RPC node #{}: {}", i, component_id);
    }

    // Keep running
    println!("✓ RPC fleet deployed successfully");
    println!("  Health monitoring active (30s intervals)");
    println!("  Auto-restart enabled (max 3 attempts)");

    // Wait forever (orchestrator runs in background)
    tokio::signal::ctrl_c().await?;
    println!("Shutting down...");

    Ok(())
}
RUST

# Compile and run
cargo build --release
./target/release/deploy-rpc-fleet
```

### Step 5: Configure Load Balancer

```bash
# Install HAProxy
sudo apt-get install -y haproxy

# Configure HAProxy
sudo cat > /etc/haproxy/haproxy.cfg <<EOF
global
    log /dev/log local0
    log /dev/log local1 notice
    maxconn 4096
    user haproxy
    group haproxy
    daemon

defaults
    log global
    mode http
    option httplog
    option dontlognull
    timeout connect 5000
    timeout client  50000
    timeout server  50000

frontend solana_rpc
    bind *:80
    bind *:443 ssl crt /etc/ssl/certs/osvm.pem
    default_backend rpc_nodes

backend rpc_nodes
    balance roundrobin
    option httpchk GET /health
    server rpc1 127.0.0.1:8899 check
    server rpc2 127.0.0.1:8900 check
    server rpc3 127.0.0.1:8901 check
EOF

# Restart HAProxy
sudo systemctl restart haproxy
sudo systemctl enable haproxy

# Test
curl http://localhost/health
```

---

## 🔄 Zero-Downtime Update Procedure

### Update Single RPC Node

```bash
# Build new version
cargo build --release --features new-version

# Prepare new component configuration
# (updates from v1.16 to v1.17)

# Perform hot-swap
./target/release/osvm-hotswap \
    --old-component-id abc-123 \
    --new-rootfs /path/to/rootfs-v1.17.ext4 \
    --health-check-timeout 30s \
    --drain-timeout 60s

# Output:
# ✓ Starting new component (125ms)
# ✓ Health checks passed (5s)
# ✓ Traffic shifted (atomic)
# ✓ Draining old connections (60s)
# ✓ Old component stopped
# Total downtime: 0ms
```

### Rolling Update Entire Fleet

```bash
# Update all RPC nodes with zero downtime
for component_id in $(osvm components list --type rpc-node); do
    echo "Updating component: $component_id"

    osvm update $component_id \
        --image /path/to/new-rootfs.ext4 \
        --wait-for-health \
        --auto-rollback

    # Wait between updates to ensure stability
    sleep 10
done

echo "✓ Fleet updated successfully"
echo "  Total user downtime: 0ms"
```

---

## 📊 Monitoring & Observability

### Health Checks

```bash
# Check orchestrator status
curl http://localhost:9000/api/v1/stats

# Response:
{
  "total_components": 5,
  "healthy_components": 5,
  "unhealthy_components": 0,
  "registered_services": 3,
  "total_endpoints": 5
}

# Check specific component
curl http://localhost:9000/api/v1/components/abc-123

# Response:
{
  "id": "abc-123",
  "type": "rpc-node",
  "status": "running",
  "health": "healthy",
  "vsock_cid": 3,
  "uptime": "2h 30m",
  "restart_count": 0
}
```

### Prometheus Metrics

```bash
# OSVM exposes Prometheus metrics on port 9090
curl http://localhost:9090/metrics

# Key metrics:
osvm_component_count{type="rpc-node",status="running"} 3
osvm_component_restarts_total{id="abc-123"} 0
osvm_hotswap_duration_seconds{result="success"} 5.2
osvm_vsock_latency_milliseconds{src="3",dst="5"} 0.3
```

### Grafana Dashboard

```bash
# Import pre-built OSVM dashboard
# Dashboard ID: osvm-production.json
# Displays:
# - Component health status
# - Hot-swap success rate
# - vsock latency heatmap
# - Resource utilization
# - Error rates
```

---

## 🚨 Troubleshooting

### Component Won't Start

```bash
# Check logs
journalctl -u osvm-orchestrator -f

# Check Firecracker availability
firecracker --version

# Check KVM support
lsmod | grep kvm
# If missing: sudo modprobe kvm kvm_intel

# Check resources
free -h  # Ensure enough RAM
df -h    # Ensure enough disk
```

### Hot-Swap Failed

```bash
# Check why hot-swap failed
osvm logs --component abc-123 --last-hotswap

# Common issues:
# 1. Health checks timeout → increase timeout
# 2. New component crashes → check application logs
# 3. Resource exhaustion → increase limits

# Manual rollback (if auto-rollback failed)
osvm rollback abc-123 --to-previous-version
```

### High vsock Latency

```bash
# Check vsock stats
cat /sys/kernel/debug/vsock/stats

# Check if CIDs are allocated correctly
osvm vsock list-cids

# Test vsock directly
osvm vsock-test --from-cid 3 --to-cid 5
# Expected: <1ms latency
```

---

## 🔒 Security Best Practices

### 1. Enable mTLS for All External Communication
```bash
# Generate certificates
osvm certs init --ca-name "OSVM Production CA"
osvm certs issue --component-id abc-123 --valid-days 90

# Configure automatic renewal
osvm certs auto-renew --days-before 30
```

### 2. Configure Network Policies
```bash
# Default deny all
osvm policy set-default deny

# Allow RPC → Validator
osvm policy allow \
    --from rpc-node \
    --to validator \
    --protocol vsock \
    --ports 8001

# Allow external → RPC
osvm policy allow \
    --from external \
    --to rpc-node \
    --protocol https \
    --ports 443
```

### 3. Enable Jailer for Firecracker
```bash
# Jailer adds extra security layer
# Configured in config.yml:
isolation:
  firecracker:
    use_jailer: true
    jailer_bin: /usr/bin/jailer
```

### 4. Regular Security Audits
```bash
# Run security scan
osvm audit --all-components

# Check for CVEs
osvm cve-scan --rootfs /path/to/rootfs.ext4

# Review network policies
osvm policy review
```

---

## 📈 Performance Tuning

### CPU Pinning
```bash
# Pin RPC nodes to specific CPU cores
osvm update-config abc-123 \
    --cpu-affinity "0-7" \
    --cpu-quota 800%  # 8 cores
```

### Memory Optimization
```bash
# Configure huge pages for better performance
echo 1024 | sudo tee /proc/sys/vm/nr_hugepages

# Enable in OSVM config
isolation:
  firecracker:
    use_huge_pages: true
```

### Network Optimization
```bash
# Increase network buffers
sudo sysctl -w net.core.rmem_max=134217728
sudo sysctl -w net.core.wmem_max=134217728

# Enable vsock zero-copy
osvm vsock set-zero-copy true
```

---

## 🎯 Production Checklist

Before going live:

- [ ] KVM support enabled and tested
- [ ] Firecracker installed and verified
- [ ] Kernel and rootfs images built
- [ ] OSVM configured with production settings
- [ ] RPC fleet deployed (3+ nodes recommended)
- [ ] Load balancer configured and tested
- [ ] mTLS certificates issued and auto-renewal enabled
- [ ] Network policies configured (default deny)
- [ ] Monitoring setup (Prometheus + Grafana)
- [ ] Alerting configured (PagerDuty/Slack)
- [ ] Health checks verified (30s intervals)
- [ ] Hot-swap tested in staging
- [ ] Backup procedures documented
- [ ] Incident response playbook created
- [ ] Team trained on operations

---

## 📞 Support

- **Documentation**: https://docs.osvm.ai
- **GitHub Issues**: https://github.com/opensvm/osvm-cli/issues
- **Discord**: https://discord.gg/osvm
- **Email**: support@osvm.ai

---

**Last Updated**: 2025-09-30
**Version**: 2.0 (Phase 2 Complete)