# MCP MicroVM Integration Examples

## Overview

The OSVM CLI now supports running MCP servers in isolated microVMs using Firecracker. This provides hardware-level isolation for secure tool execution.

## Basic Usage

### 1. Launch an MCP Server in a MicroVM

```bash
# Launch a single MCP server
osvm mcp microvm launch my-secure-server

# With custom memory and CPU
osvm mcp microvm launch my-server --memory 512 --vcpus 2

# With custom command
osvm mcp microvm launch my-server --command "cargo run --bin mcp-server"
```

### 2. Check Status

```bash
# Show all running MCP microVMs
osvm mcp microvm status
```

### 3. Stop a MicroVM

```bash
# Stop a specific server
osvm mcp microvm stop my-secure-server

# Stop all VMs
sudo killall firecracker
```

## Advanced Usage

### Launch Multiple MCP Servers (Load Testing)

```bash
# Launch 10 test servers
osvm mcp microvm launch-many 10

# Launch 100 servers with custom memory
osvm mcp microvm launch-many 100 --memory 128
```

### Integration with MCP Tool Calling

Once an MCP server is running in a microVM, tools are automatically executed in isolation:

```bash
# 1. Configure server for microVM execution
osvm mcp add my-server --url stdio --isolation microvm

# 2. Launch the server in a microVM
osvm mcp microvm launch my-server

# 3. Call a tool (executes in the microVM)
osvm mcp call my-server my-tool '{"param": "value"}'
```

## Architecture

```
┌─────────────────────────────────────────────┐
│              Host System                     │
│                                              │
│  ┌─────────────────────────────────────┐    │
│  │         OSVM CLI                     │    │
│  │  ┌──────────────────────────────┐   │    │
│  │  │    MCP Service               │   │    │
│  │  │  ┌──────────────────────┐    │   │    │
│  │  │  │  MicroVM Launcher    │    │   │    │
│  │  │  └──────────┬───────────┘    │   │    │
│  │  └──────────────┼────────────────┘   │    │
│  └─────────────────┼────────────────────┘    │
│                    │                          │
│  ┌─────────────────▼────────────────────┐    │
│  │         Firecracker                  │    │
│  │  ┌──────────────────────────────┐   │    │
│  │  │    MicroVM 1                 │   │    │
│  │  │  ┌────────────────────────┐  │   │    │
│  │  │  │   MCP Server           │  │   │    │
│  │  │  │  ┌─────────────────┐   │  │   │    │
│  │  │  │  │   Tool          │   │  │   │    │
│  │  │  │  │   Execution     │   │  │   │    │
│  │  │  │  └─────────────────┘   │  │   │    │
│  │  │  └────────────────────────┘  │   │    │
│  │  │    Vsock Communication       │   │    │
│  │  └──────────────────────────────┘   │    │
│  │                                      │    │
│  │  ┌──────────────────────────────┐   │    │
│  │  │    MicroVM 2                 │   │    │
│  │  │    ...                       │   │    │
│  │  └──────────────────────────────┘   │    │
│  └──────────────────────────────────────┘    │
└─────────────────────────────────────────────┘
```

## Security Benefits

1. **Hardware Isolation**: Each MCP server runs in its own VM with KVM isolation
2. **Resource Limits**: Memory and CPU strictly controlled per VM
3. **No Network Access**: VMs use vsock for host communication only
4. **Ephemeral Execution**: VMs can be destroyed after tool execution
5. **Parallel Isolation**: Multiple tools can run simultaneously in separate VMs

## Performance Characteristics

- **Launch Time**: ~200-500ms per VM
- **Memory Overhead**: 256MB minimum per VM
- **Concurrent VMs**: Tested with 100+ concurrent VMs
- **Communication**: Low-latency vsock (no network stack overhead)

## Configuration

Add to `~/.osvm/isolation_config.toml`:

```toml
[microvm]
enabled = true
default_memory_mb = 256
default_vcpus = 1
rootfs_path = "/home/user/.osvm/rootfs/mcp-server.cpio"
kernel_path = "/home/user/.osvm/kernel/vmlinux.bin"

[[servers]]
id = "my-secure-server"
name = "My Secure MCP Server"
isolation_mode = "microvm"
microvm_config = { memory_mb = 512, vcpus = 2 }

[[servers.tools]]
name = "dangerous-tool"
isolation = "microvm"
```

## Troubleshooting

### VMs Not Starting

1. Check KVM support:
```bash
ls -la /dev/kvm
```

2. Load vsock module:
```bash
sudo modprobe vhost_vsock
```

3. Check Firecracker:
```bash
firecracker --version
```

### Memory Issues

- Each VM needs at least 256MB RAM
- 100 VMs = 25.6GB RAM minimum
- Check available memory:
```bash
free -h
```

### Debugging

1. Check VM logs:
```bash
ls /tmp/microvm-*/logs/
```

2. Monitor Firecracker processes:
```bash
watch 'ps aux | grep firecracker | wc -l'
```

3. Test single VM:
```bash
osvm mcp microvm test
```

## Performance Testing

Run the included performance test:

```bash
# Test launching 100 concurrent MCP servers
osvm mcp microvm launch-many 100

# Monitor resource usage
htop

# Check VM count
ps aux | grep -c firecracker
```

## Best Practices

1. **Memory Sizing**: Start with 256MB, increase only if needed
2. **Batch Operations**: Launch VMs in batches of 10-20 to avoid overwhelming the system
3. **Cleanup**: Always shut down VMs when done to free resources
4. **Monitoring**: Use `osvm mcp microvm status` to track running VMs
5. **Rootfs Optimization**: Use minimal rootfs images for faster launch times