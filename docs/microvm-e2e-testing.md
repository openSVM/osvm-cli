# MicroVM End-to-End Testing Guide

This guide explains how to set up and run end-to-end tests for the OSVM MicroVM-based MCP server infrastructure.

## Overview

The MicroVM testing infrastructure validates:
- Guest-side vsock wrapper functionality
- Host-to-guest JSON-RPC communication
- Real MCP server execution in isolated microVMs
- Health monitoring and graceful shutdown
- Concurrent server operation

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Host System (Testing Environment)                           │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Integration Tests (Rust)                             │  │
│  │  - test_microvm_launch_and_init()                   │  │
│  │  - test_vsock_request_response()                    │  │
│  │  - test_health_check_monitoring()                   │  │
│  │  - test_multiple_concurrent_servers()               │  │
│  └──────────────────────────────────────────────────────┘  │
│           ↕                                                  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ MicroVM Launcher                                     │  │
│  │  - Launches Firecracker instances                   │  │
│  │  - Manages vsock communication                      │  │
│  └──────────────────────────────────────────────────────┘  │
│           ↕ virtio-vsock (port 5252)                        │
│      ┌────┴──────┬────────────┬────────────┐              │
│  ┌───▼────┐  ┌──▼─────┐  ┌──▼──────┐  ┌───────┐          │
│  │Test    │  │Solana  │  │GitHub   │  │Custom │          │
│  │MicroVM │  │MicroVM │  │MicroVM  │  │MicroVM│          │
│  │        │  │        │  │         │  │       │          │
│  │ ✓ Wrapper  │ ✓ Wrapper  │ ✓ Wrapper   │ ✓ Wrapper │  │
│  │ ✓ Node.js  │ ✓ Node.js  │ ✓ Node.js   │ ✓ Node.js │  │
│  │ ✓ MCP Srv  │ ✓ MCP Srv  │ ✓ MCP Srv   │ ✓ MCP Srv │  │
│  └────────┘  └─────────┘  └──────────┘  └───────┘          │
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

### Required Software

1. **Firecracker** (v1.0+)
   ```bash
   # Download latest release
   ARCH="$(uname -m)"
   release_url="https://github.com/firecracker-microvm/firecracker/releases"
   latest=$(basename $(curl -fsSLI -o /dev/null -w %{url_effective} ${release_url}/latest))
   curl -L ${release_url}/download/${latest}/firecracker-${latest}-${ARCH}.tgz \
     | tar -xz
   
   # Install
   sudo mv release-${latest}-${ARCH}/firecracker-${latest}-${ARCH} \
     /usr/local/bin/firecracker
   sudo chmod +x /usr/local/bin/firecracker
   ```

2. **Docker** (for building guest rootfs)
   ```bash
   # Ubuntu/Debian
   sudo apt-get update
   sudo apt-get install docker.io
   sudo usermod -aG docker $USER
   
   # Log out and back in for group changes to take effect
   ```

3. **Rust Toolchain**
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   source $HOME/.cargo/env
   ```

4. **KVM Support**
   ```bash
   # Check if KVM is available
   lsmod | grep kvm
   
   # If not loaded, load the module
   sudo modprobe kvm
   sudo modprobe kvm_intel  # or kvm_amd
   
   # Grant access to /dev/kvm
   sudo chmod 666 /dev/kvm
   ```

### System Requirements

- Linux kernel with KVM and vsock support (4.8+)
- x86_64 architecture
- At least 4GB RAM
- 2GB free disk space for guest rootfs

## Setup

### 1. Build Guest Rootfs

The guest rootfs contains:
- Alpine Linux base system
- Node.js runtime (v18)
- MCP vsock wrapper (Rust binary)
- Pre-installed MCP servers

```bash
# Run the automated build script
./scripts/build-guest-rootfs.sh
```

This creates:
- `~/.osvm/rootfs/mcp-server.cpio` - Guest filesystem
- `~/.osvm/kernel/vmlinux.bin` - Linux kernel

**Build time:** ~5-10 minutes (depending on network speed)

**Output size:** ~100-150MB

### 2. Verify Installation

```bash
# Check kernel
ls -lh ~/.osvm/kernel/vmlinux.bin

# Check rootfs
ls -lh ~/.osvm/rootfs/mcp-server.cpio

# Verify wrapper binary is executable
```

## Running Tests

### Quick Test (Non-Infrastructure)

These tests don't require Firecracker:

```bash
# Run configuration and routing tests
cargo test --test microvm_integration_tests \
  test_mcp_service_routing_priority \
  test_error_recovery \
  test_cid_allocation_determinism \
  test_resource_limit_configuration \
  test_mount_point_configuration
```

**Expected runtime:** < 1 second

### Full E2E Test Suite

Run all tests including microVM infrastructure:

```bash
# Automated test runner
./scripts/run-e2e-tests.sh
```

**Expected runtime:** 2-5 minutes

**What it tests:**
1. Prerequisites check
2. Project build
3. Unit tests
4. Integration tests (non-infrastructure)
5. KVM access verification
6. Infrastructure tests with Firecracker:
   - Basic microVM launch
   - Vsock communication
   - Health check monitoring
   - Multiple concurrent servers
   - Graceful shutdown

### Individual Infrastructure Tests

```bash
# Test 1: Basic launch
cargo test --test microvm_integration_tests \
  test_microvm_launch_and_init -- --nocapture

# Test 2: Communication
cargo test --test microvm_integration_tests \
  test_vsock_request_response -- --nocapture

# Test 3: Health checks
cargo test --test microvm_integration_tests \
  test_health_check_monitoring -- --nocapture

# Test 4: Concurrent servers
cargo test --test microvm_integration_tests \
  test_multiple_concurrent_servers -- --nocapture

# Test 5: Shutdown
cargo test --test microvm_integration_tests \
  test_graceful_shutdown_all -- --nocapture
```

## Test Descriptions

### Test 1: Basic MicroVM Launch (`test_microvm_launch_and_init`)

**Purpose:** Verify microVM can be launched and becomes ready

**Steps:**
1. Create MicroVM configuration
2. Launch Firecracker instance
3. Wait for vsock socket creation
4. Verify process is running
5. Graceful shutdown

**Success criteria:**
- Firecracker starts without errors
- Vsock socket created within 30s
- Process remains running
- Clean shutdown completes

### Test 2: Vsock Communication (`test_vsock_request_response`)

**Purpose:** Validate JSON-RPC communication over vsock

**Steps:**
1. Launch microVM with test MCP server
2. Connect to vsock port 5252
3. Send JSON-RPC initialize request
4. Receive and validate response
5. Shutdown

**Success criteria:**
- Vsock connection succeeds
- Request serialization works
- Response received within timeout
- Valid JSON-RPC structure
- Proper cleanup

### Test 3: Health Check (`test_health_check_monitoring`)

**Purpose:** Verify health monitoring works

**Steps:**
1. Launch microVM
2. Perform basic health check (socket existence)
3. Perform active health check (JSON-RPC initialize)
4. Validate response
5. Shutdown

**Success criteria:**
- Socket exists after launch
- Active health check completes
- Initialize response valid
- No errors in communication

### Test 4: Multiple Concurrent Servers (`test_multiple_concurrent_servers`)

**Purpose:** Ensure multiple microVMs can run simultaneously

**Steps:**
1. Launch 3 microVMs concurrently
2. Verify unique CID allocation
3. Check all are running
4. Verify no crosstalk
5. Shutdown all

**Success criteria:**
- All launches succeed
- CIDs are unique (100-199 range)
- All remain running
- Independent operation
- Clean batch shutdown

### Test 5: Graceful Shutdown (`test_graceful_shutdown_all`)

**Purpose:** Validate shutdown mechanisms

**Steps:**
1. Initialize MCP service
2. Call shutdown_all_mcp_microvms()
3. Call cleanup_all()
4. Verify cleanup

**Success criteria:**
- Methods execute without errors
- All resources cleaned up
- No orphaned processes
- Socket files removed

## Testing with Real MCP Servers

### Solana MCP Server

```bash
# The rootfs includes @modelcontextprotocol/server-solana
# Test configuration in isolation_config.json:

{
  "mcp_servers": {
    "solana": {
      "microvm_id": "μVM-1",
      "use_microvm": true,
      "microvm_config": {
        "memory_mb": 512,
        "vcpus": 2
      },
      "server_command": "npx -y @modelcontextprotocol/server-solana"
    }
  }
}
```

### GitHub MCP Server

```bash
# Test with GitHub server
{
  "github": {
    "use_microvm": true,
    "server_command": "npx -y @modelcontextprotocol/server-github"
  }
}
```

### Custom Test Server

Use the included simple test server:

```bash
# In guest VM
node /usr/local/lib/node_modules/simple-test-mcp-server/index.js
```

Or from host (for local testing):

```bash
./examples/simple_test_mcp_server.js
```

## Troubleshooting

### Issue: Firecracker not found

```bash
which firecracker
# If empty, install following Prerequisites section
```

### Issue: No KVM access

```bash
ls -l /dev/kvm
# Should show read/write for your user

# Fix
sudo chmod 666 /dev/kvm
```

### Issue: Kernel not found

```bash
# Re-run rootfs build to download kernel
./scripts/build-guest-rootfs.sh
```

### Issue: Vsock connection timeout

**Possible causes:**
1. Guest wrapper not starting
2. vsock kernel module not loaded
3. Port conflict

**Debug:**
```bash
# Check kernel modules
lsmod | grep vsock

# Check Firecracker logs
journalctl -xe | grep firecracker

# Verify wrapper in guest
# (Check guest console output)
```

### Issue: Tests fail with "Socket not found"

**Solution:**
- Increase wait timeout in tests
- Check guest init script execution
- Verify wrapper binary in rootfs

### Issue: JSON parse errors

**Cause:** Invalid JSON-RPC format

**Debug:**
```bash
# Enable debug logging in wrapper
RUST_LOG=debug cargo test -- --nocapture
```

## Performance Benchmarks

Expected performance metrics:

| Metric | Target | Typical |
|--------|--------|---------|
| MicroVM launch time | < 1s | ~500ms |
| First vsock connection | < 2s | ~1s |
| JSON-RPC request/response | < 50ms | ~10ms |
| Health check | < 100ms | ~20ms |
| Shutdown time | < 3s | ~1s |

## CI/CD Integration

### GitHub Actions Example

```yaml
name: MicroVM E2E Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install dependencies
      run: |
        # Install Firecracker
        # Install Docker
        # Install Rust
    
    - name: Setup KVM
      run: |
        sudo apt-get install -y cpu-checker
        sudo kvm-ok
        sudo chmod 666 /dev/kvm
    
    - name: Build guest rootfs
      run: ./scripts/build-guest-rootfs.sh
    
    - name: Run E2E tests
      run: ./scripts/run-e2e-tests.sh
```

## Next Steps

After successful E2E testing:

1. **Production Deployment**
   - Deploy rootfs to production servers
   - Configure isolation policies
   - Set up monitoring

2. **Performance Optimization**
   - Implement connection pooling
   - Optimize microVM boot time
   - Tune resource limits

3. **Security Hardening**
   - Add seccomp filters
   - Implement resource monitoring
   - Create security audit tooling

## References

- [Firecracker Documentation](https://github.com/firecracker-microvm/firecracker/tree/main/docs)
- [Model Context Protocol Specification](https://spec.modelcontextprotocol.io/)
- [Linux vsock Documentation](https://www.kernel.org/doc/html/latest/networking/af_vsock.html)
- [OSVM Architecture Documentation](../Architecture.md)

## Support

For issues or questions:
- File bug reports with test logs
- Include guest console output
- Provide Firecracker version
- Share system specifications
