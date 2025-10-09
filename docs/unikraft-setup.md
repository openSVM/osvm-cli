# Unikraft Unikernel Setup Guide

## Overview

OSVM CLI Phase 2 implements ephemeral, isolated tool execution using Unikraft-based unikernels with vsock communication. This provides lightweight process isolation for MCP tool execution without the overhead of full virtual machines.

## Architecture

### Components

```
┌─────────────────────────────────────────────┐
│           OSVM CLI Host Process             │
├─────────────────────────────────────────────┤
│  • MCP Service Layer                        │
│  • Unikernel Runtime Manager                │
│  • Vsock Communication                      │
│  • CID Allocation (200-299)                 │
└─────────────────────────────────────────────┘
                    ↓ vsock (port 5252)
┌─────────────────────────────────────────────┐
│      Unikraft Unikernel (Ephemeral)         │
├─────────────────────────────────────────────┤
│  • Static musl binary (1.4MB)               │
│  • JSON-RPC 2.0 server                      │
│  • Tool execution proxy                     │
│  • No network, minimal mounts               │
└─────────────────────────────────────────────┘
```

### Key Features

- **Lightweight**: 1.4MB static binary, <300ms boot time, <80MB memory
- **Isolated**: No network access, vsock-only communication
- **Ephemeral**: Spawn on-demand, terminate after use
- **Phase 3 Compatible**: Uses same vsock protocol (port 5252)

## Installation

### Prerequisites

1. **Unikraft CLI (kraft)**

```bash
# Install kraft v0.11.6 or later
curl --proto '=https' --tlsv1.2 -sSf \
  https://get.kraftkit.sh | sh

# Verify installation
kraft version
```

2. **Rust Toolchain**

```bash
# Install musl target for static linking
rustup target add x86_64-unknown-linux-musl

# Verify
rustc --version
cargo --version
```

3. **Kernel Modules**

```bash
# Load vsock module
sudo modprobe vhost_vsock

# Verify
lsmod | grep vhost_vsock
```

### Building the Guest Binary

```bash
# Run the build script
bash scripts/build-unikraft-tool-executor.sh

# This will:
# 1. Validate toolchain
# 2. Build static musl binary
# 3. Install to ~/.osvm/unikernels/
# 4. Set executable permissions
```

The resulting binary:
- Location: `~/.osvm/unikernels/unikraft_tool_executor`
- Size: ~1.4MB
- Type: Statically linked (no dependencies)
- Permissions: 755

## Configuration

### Isolation Config

Edit `~/.config/osvm/isolation_config.json`:

```json
{
  "default_execution_mode": "unikernel",
  "mcp_servers": {
    "solana": {
      "tools": {
        "get_balance": {
          "execution_mode": "unikernel",
          "unikernel_image": "unikraft_tool_executor",
          "memory_mb": 128,
          "vcpus": 1,
          "timeout_secs": 30
        },
        "get_transaction": {
          "execution_mode": "unikernel",
          "unikernel_image": "unikraft_tool_executor",
          "memory_mb": 128
        }
      }
    }
  }
}
```

### Configuration Options

| Option | Default | Description |
|--------|---------|-------------|
| `execution_mode` | `"direct"` | Set to `"unikernel"` for isolated execution |
| `unikernel_image` | - | Binary name (without path) |
| `memory_mb` | `128` | Memory allocation (MB) |
| `vcpus` | `1` | Virtual CPUs |
| `timeout_secs` | `30` | Execution timeout |
| `network_enabled` | `false` | Network access (keep false) |
| `mounts` | `[]` | Filesystem mounts (keep empty) |

## Usage

### Basic Tool Execution

```bash
# Execute a tool in unikernel
osvm mcp call-tool solana get_balance \
  '{"address":"DYw8jCTfwHNRJhhmFcbXvVDTqWMEVFBX6ZKUmG5CNSKK"}'

# The system will:
# 1. Allocate ephemeral CID (200-299)
# 2. Spawn unikernel via kraft
# 3. Establish vsock connection
# 4. Send JSON-RPC request
# 5. Receive response
# 6. Terminate unikernel
```

### Monitoring Executions

```bash
# Watch kraft processes
watch 'ps aux | grep kraft'

# Monitor vsock connections
ss -x | grep vsock

# Check unikernel logs
kraft logs <instance-name>
```

### Performance Tuning

**Memory Allocation**:
- Minimum: 64MB (for simple tools)
- Recommended: 128MB (default)
- Maximum: 256MB (for complex operations)

**Timeout Values**:
- Quick tools: 10-15s
- Standard tools: 30s (default)
- Long-running: 60s (max recommended)

## Testing

### 1. End-to-End Test

```bash
# Run comprehensive E2E tests
bash scripts/test-unikernel-execution.sh

# Tests:
# - kraft binary detection
# - Guest binary verification
# - vsock support
# - Isolation configuration
# - Binary execution
# - OSVM CLI build
# - CID allocation
```

### 2. Integration Tests

```bash
# Run Rust integration tests
cargo test unikernel

# Run specific test
cargo test test_spawn_and_terminate_unikernel -- --ignored
```

### 3. Performance Benchmarks

```bash
# Run performance benchmarks
bash scripts/benchmark-unikernel-performance.sh

# Measures:
# - Boot time (<300ms target)
# - Memory usage (<80MB target)
# - Binary size (~1.4MB)
# - CID allocation speed
```

### 4. Security Validation

```bash
# Run security validation
bash scripts/validate-unikernel-security.sh

# Validates:
# - Process isolation
# - Vsock-only communication
# - CID range isolation
# - Mount restrictions
# - Protocol security
# - Timeout protection
```

## Protocol Specification

### Vsock Communication

**Connection Parameters**:
- Port: 5252 (hardcoded)
- CID Range: 200-299 (ephemeral)
- Transport: vsock (no network)

**Message Format**:
```
[4-byte length][JSON payload]
```

Length prefix: Little-endian u32
Max message size: 10MB

### JSON-RPC 2.0 Protocol

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 123,
  "server_id": "solana",
  "method": "get_balance",
  "params": {
    "address": "..."
  }
}
```

**Response (Success)**:
```json
{
  "jsonrpc": "2.0",
  "id": 123,
  "result": {
    "balance": 1000000,
    "decimals": 9
  }
}
```

**Response (Error)**:
```json
{
  "jsonrpc": "2.0",
  "id": 123,
  "error": {
    "code": -32603,
    "message": "Internal error",
    "data": "..."
  }
}
```

## Troubleshooting

### kraft Not Found

```bash
# Check installation
which kraft

# If in Flatpak
flatpak-spawn --host -- kraft version

# Reinstall if needed
curl -sSf https://get.kraftkit.sh | sh
```

### Guest Binary Build Fails

```bash
# Verify Rust toolchain
rustc --version
cargo --version

# Add musl target
rustup target add x86_64-unknown-linux-musl

# Rebuild
bash scripts/build-unikraft-tool-executor.sh
```

### Vsock Connection Fails

```bash
# Load vsock module
sudo modprobe vhost_vsock

# Verify device
ls -l /dev/vsock

# Check permissions
sudo chmod 666 /dev/vsock
```

### Unikernel Won't Start

```bash
# Check kraft logs
kraft logs <instance>

# Verify binary
file ~/.osvm/unikernels/unikraft_tool_executor

# Test binary directly
~/.osvm/unikernels/unikraft_tool_executor --help
```

### Performance Issues

**Slow boot times**:
- Check system resources (CPU, memory)
- Reduce concurrent unikernels
- Increase boot delay in config

**High memory usage**:
- Reduce memory_mb allocation
- Check for memory leaks in guest
- Monitor with `ps aux`

### CID Allocation Conflicts

```bash
# Check active CIDs
ss -x | grep vsock

# Kill orphaned kraft processes
pkill kraft

# Clear stale connections
# (unikernels auto-cleanup on exit)
```

## Advanced Configuration

### Custom Mounts

```json
{
  "mounts": [
    {
      "host_path": "/tmp/shared",
      "guest_path": "/mnt/shared",
      "readonly": true
    }
  ]
}
```

**Security Warning**: Minimize mounts to maintain isolation.

### Environment Variables

```json
{
  "env_vars": {
    "RUST_LOG": "debug",
    "TOOL_TIMEOUT": "60"
  }
}
```

### Kraft CLI Options

Override default kraft behavior:

```json
{
  "kraft_config": {
    "runtime": "unikraft",
    "plat": "qemu",
    "arch": "x86_64"
  }
}
```

## Phase 3 Integration

Phase 2 (ephemeral) and Phase 3 (persistent) share the same protocol:

| Aspect | Phase 2 | Phase 3 |
|--------|---------|---------|
| CID Range | 200-299 | 100-199 |
| Lifecycle | Ephemeral | Persistent |
| Use Case | Tool execution | MCP server |
| Port | 5252 | 5252 |
| Protocol | JSON-RPC 2.0 | JSON-RPC 2.0 |

**Migration Path**: Phase 2 tools can be promoted to Phase 3 persistent servers by:
1. Changing CID range
2. Keeping unikernel running
3. Implementing connection pooling

## Security Considerations

### Isolation Properties

✓ **Process Isolation**: Separate unikernel per execution
✓ **Network Isolation**: No network access (vsock only)
✓ **Filesystem Isolation**: Minimal mounts by default
✓ **Memory Isolation**: Dedicated memory allocation
✓ **Timeout Protection**: 30s default timeout

### Attack Surface

**Minimized**:
- No network stack
- Static binary (no dynamic libraries)
- Vsock-only communication
- Ephemeral lifecycle

**Residual Risks**:
- Vsock protocol vulnerabilities
- kraft CLI exploits
- CID exhaustion (unlikely with 100 CIDs)

### Best Practices

1. **Keep mounts minimal** - avoid unnecessary host paths
2. **Use short timeouts** - prevent resource exhaustion
3. **Monitor CID usage** - track allocation patterns
4. **Update kraft regularly** - security patches
5. **Validate inputs** - sanitize tool parameters

## Performance Characteristics

### Measured Performance

| Metric | Target | Typical |
|--------|--------|---------|
| Boot Time | <300ms | 200-250ms |
| Memory | <80MB | 60-70MB |
| Binary Size | ~1.4MB | 1.4MB |
| Latency | <50ms | 30-40ms |

### Scalability

- **Concurrent unikernels**: Up to 100 (CID limit)
- **Spawn rate**: ~10/second (kraft overhead)
- **Memory footprint**: ~70MB per unikernel

### Optimization Tips

1. **Pre-warm binaries**: Keep in page cache
2. **Batch operations**: Reuse unikernel when possible
3. **Monitor resources**: Use `top`, `vmstat`
4. **Tune timeouts**: Balance safety vs. performance

## Contributing

### Adding New Guest Features

1. Modify `guest/unikraft_tool_executor/src/*.rs`
2. Rebuild with `bash scripts/build-unikraft-tool-executor.sh`
3. Test with integration suite
4. Update documentation

### Extending Protocol

1. Update `types.rs` and `protocol.rs`
2. Maintain backward compatibility
3. Version protocol if breaking changes
4. Test with existing tools

## References

- [Unikraft Documentation](https://unikraft.org/docs/)
- [kraft CLI Guide](https://unikraft.org/docs/cli/)
- [JSON-RPC 2.0 Spec](https://www.jsonrpc.org/specification)
- [Vsock Protocol](https://wiki.qemu.org/Features/VirtioVsock)
- [Phase 3 Documentation](./microvm-e2e-testing.md)

## Support

For issues or questions:

1. Check troubleshooting section
2. Run diagnostic tests
3. Review logs
4. Open GitHub issue with:
   - System info (`uname -a`)
   - kraft version
   - Error messages
   - Test results
