# Load Testing and Real MCP Tool Execution Guide

This guide covers load testing the unikernel system with concurrent instances and validating real MCP tool execution.

## Prerequisites

### Required Software
- **kraft CLI** v0.11.6+: `curl -sSf https://get.kraftkit.sh | sh`
- **Node.js** v18+: For running test MCP servers
- **vsock module**: `sudo modprobe vhost_vsock`
- **Guest binary**: Built via `bash scripts/build-unikraft-tool-executor.sh`

### Flatpak Support
The load testing script **automatically detects Flatpak** environments and uses `flatpak-spawn --host` to access the host system's kraft CLI. No additional configuration needed - just ensure kraft is installed on your host system.

### System Requirements
- **Memory**: At least 2GB available for load testing
- **CPU**: Multi-core recommended for concurrent testing
- **Disk**: ~1GB for unikernel images and logs

### Verification
```bash
# Check kraft
kraft --version

# Check Node.js
node --version

# Check vsock module
lsmod | grep vhost_vsock

# Check guest binary
ls -lh ~/.osvm/unikernels/unikraft_tool_executor
```

## Load Testing

### Overview
Load testing validates system stability under concurrent unikernel spawning with 10-20+ concurrent instances.

### Running Load Tests

#### Basic Load Test (10 unikernels)
```bash
bash scripts/load-test-unikernels.sh 10
```

#### Medium Load Test (15 unikernels - default)
```bash
bash scripts/load-test-unikernels.sh
```

#### High Load Test (20 unikernels)
```bash
bash scripts/load-test-unikernels.sh 20
```

### Test Phases

The load testing script executes 5 phases:

1. **Phase 1: Concurrent Spawning**
   - Spawns all unikernels concurrently
   - Tracks spawn commands and PIDs
   - Allocates vsock CIDs (200-299 range)

2. **Phase 2: Boot Time Analysis**
   - Measures boot time for each instance
   - Checks vsock connectivity
   - Target: <500ms per instance

3. **Phase 3: Memory Usage Analysis**
   - Calculates total memory usage
   - Computes average per unikernel
   - Target: <80MB per instance

4. **Phase 4: CID Allocation Analysis**
   - Checks for CID collisions
   - Validates uniqueness of allocations
   - Verifies 200-299 range

5. **Phase 5: Cleanup**
   - Terminates all unikernels gracefully
   - Verifies process cleanup
   - Checks for orphaned processes

### Interpreting Results

#### Success Criteria
✓ **All tests pass** when:
- All unikernels spawn successfully
- No CID collisions detected
- Boot times within target (<500ms)
- Memory usage acceptable (<80MB/instance)
- Clean termination without orphans

⚠ **Partial pass** when:
- Most unikernels spawn (>80% success rate)
- Minor CID collisions (hash-based algorithm)
- Boot times slightly over target under load
- Some cleanup issues

✗ **Failure** when:
- Spawn success rate <70%
- Excessive memory usage
- System instability
- Cannot cleanup processes

#### Sample Output
```
================================
Unikernel Load Testing
================================

Configuration:
  Number of unikernels: 15
  Vsock port: 5252
  CID range: 200-299
  Max boot time: 500ms
  Max memory per instance: 80MB
  Test timeout: 60s

Phase 1: Concurrent Spawning
[0] Spawning unikernel with CID 247...
[1] Spawning unikernel with CID 212...
...

Phase 2: Boot Time Analysis
[0] Boot successful: 287ms (CID 247)
[1] Boot successful: 294ms (CID 212)
...

Phase 3: Memory Usage Analysis
Total memory usage: 960MB
Average memory per unikernel: 64MB
✓ Memory usage within target

Phase 4: CID Allocation Analysis
✓ No CID collisions detected (15 unique CIDs)

Phase 5: Cleanup
[0] Terminating PID 12345...
...

Overall Assessment:
✓ PASS - All tests successful
```

### Logs and Debugging

Logs are saved to `/tmp/osvm-load-test/`:
```bash
# View all logs
ls -lh /tmp/osvm-load-test/

# Check specific unikernel log
cat /tmp/osvm-load-test/unikernel_0_cid247.log

# Search for errors
grep -i error /tmp/osvm-load-test/*.log
```

## Rust Load Testing Suite

### Running Rust Tests

```bash
# Run all load tests (requires kraft + guest binary)
cargo test --test unikernel_load_tests -- --ignored --nocapture

# Run specific test
cargo test --test unikernel_load_tests test_concurrent_spawn_10_unikernels -- --ignored --nocapture

# Run high load test
cargo test --test unikernel_load_tests test_high_load_20_unikernels -- --ignored --nocapture
```

### Available Tests

1. **test_concurrent_cid_allocation**
   - Tests CID allocation range (200-299)
   - Validates 50 concurrent allocations
   - No prerequisites (runs without kraft)

2. **test_concurrent_spawn_10_unikernels**
   - Spawns 10 unikernels concurrently
   - Measures boot times
   - Success rate: ≥80%

3. **test_high_load_20_unikernels**
   - Spawns 20 unikernels with rate limiting
   - 5 concurrent spawns max
   - Success rate: ≥70%

4. **test_memory_leak_detection**
   - 10 spawn/terminate cycles
   - Detects memory leaks
   - Validates cleanup

5. **test_cleanup_orphaned_unikernels**
   - Tests orphaned process handling
   - Documents kraft --rm behavior
   - Validates automatic cleanup

6. **test_rapid_spawn_terminate_cycles**
   - 20 rapid spawn/terminate cycles
   - Tests race conditions
   - Success rate: ≥75%

## Real MCP Tool Execution Testing

### Test MCP Server

We provide an echo test server at `examples/test_mcp_servers/echo_server.js`:

**Available Tools:**
- `echo`: Returns input message
- `add`: Adds two numbers
- `get_timestamp`: Returns Unix timestamp
- `error_test`: Throws intentional error

**Running Manually:**
```bash
# Start server
node examples/test_mcp_servers/echo_server.js

# In another terminal, test with stdio
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | node examples/test_mcp_servers/echo_server.js
```

### Running MCP Execution Tests

```bash
# Run all MCP tool tests (requires Node.js)
cargo test --test real_mcp_execution_tests -- --ignored --nocapture

# Run specific test
cargo test --test real_mcp_execution_tests test_echo_tool_execution -- --ignored --nocapture
```

### Available Tests

1. **test_echo_tool_execution**
   - Tests basic echo tool
   - Validates JSON-RPC communication
   - Prerequisites: Node.js, kraft, guest binary

2. **test_add_tool_execution**
   - Tests numeric parameters
   - Validates result computation
   - Prerequisites: Node.js

3. **test_error_handling**
   - Tests intentional errors
   - Validates error responses
   - Prerequisites: Node.js

4. **test_invalid_parameters**
   - Tests missing required params
   - Validates parameter validation
   - Prerequisites: Node.js

5. **test_unknown_tool**
   - Tests nonexistent tool calls
   - Validates error handling
   - Prerequisites: Node.js

6. **test_concurrent_tool_executions**
   - Tests 5 concurrent requests
   - Validates concurrency handling
   - Prerequisites: Node.js

7. **test_tool_execution_timeout**
   - Tests timeout scenarios
   - Validates graceful failure
   - Prerequisites: Node.js

8. **test_list_tools**
   - Tests tools/list method
   - Validates tool enumeration
   - Prerequisites: Node.js

9. **test_full_unikernel_mcp_integration**
   - Full end-to-end integration
   - Currently placeholder
   - Prerequisites: Node.js, kraft, guest binary

### Integration Testing Architecture

For full integration testing, the flow is:

```
┌─────────────┐
│ Test Runner │
└──────┬──────┘
       │
       ├─> 1. Start MCP Server (Node.js)
       │
       ├─> 2. Configure MCP Service
       │      (isolation_config.json)
       │
       ├─> 3. Spawn Unikernel
       │      (kraft run)
       │
       ├─> 4. Execute Tool via Vsock
       │      (CID:5252, JSON-RPC)
       │
       ├─> 5. Verify Response
       │      (Assert expected result)
       │
       └─> 6. Cleanup
              (Terminate unikernel + server)
```

## Performance Benchmarks

### Expected Performance

**Under Normal Load (1-5 concurrent unikernels):**
- Boot time: 200-300ms
- Memory per instance: 60-70MB
- Tool execution latency: <50ms
- Vsock connection: <10ms

**Under High Load (10-20 concurrent unikernels):**
- Boot time: 300-500ms (acceptable)
- Memory per instance: 70-80MB
- Tool execution latency: <100ms
- Vsock connection: <20ms

**Limits:**
- Max concurrent: ~100 (CID range limit)
- Memory ceiling: ~8GB (100 × 80MB)
- Boot time ceiling: ~1s under extreme load

### Benchmarking Tools

Use the existing benchmark script:
```bash
bash scripts/benchmark-unikernel-performance.sh
```

## Troubleshooting

### Common Issues

#### 1. kraft not found
```bash
# Install kraft on host system
curl -sSf https://get.kraftkit.sh | sh

# Add to PATH
export PATH="$HOME/.kraftkit/bin:$PATH"

# If running in Flatpak:
# The script automatically detects Flatpak and uses flatpak-spawn
# Just ensure kraft is installed on the HOST system, not inside Flatpak
```

#### 2. vhost_vsock module not loaded
```bash
# Load module
sudo modprobe vhost_vsock

# Verify
lsmod | grep vhost_vsock

# Make persistent (optional)
echo "vhost_vsock" | sudo tee -a /etc/modules
```

#### 3. Guest binary not found
```bash
# Build guest binary
bash scripts/build-unikraft-tool-executor.sh

# Verify
ls -lh ~/.osvm/unikernels/unikraft_tool_executor
file ~/.osvm/unikernels/unikraft_tool_executor
```

#### 4. Spawn failures under load
- Reduce concurrent count
- Check available memory: `free -h`
- Check system load: `uptime`
- Kill orphaned processes: `pkill -f kraft`

#### 5. CID collisions
- Expected behavior (hash-based allocation)
- Rare with timestamp + PID
- System retries automatically
- Document actual collision rate in tests

#### 6. Cleanup issues
- kraft run --rm should auto-cleanup
- Manual cleanup: `pkill -f kraft`
- Check orphans: `ps aux | grep kraft`
- Force kill: `pkill -9 -f kraft`

### Debug Mode

Enable debug logging:
```bash
# Set environment variable
export RUST_LOG=debug

# Run tests with logging
RUST_LOG=debug cargo test --test unikernel_load_tests -- --ignored --nocapture
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Unikernel Load Tests

on: [push, pull_request]

jobs:
  load-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Install kraft
        run: |
          curl -sSf https://get.kraftkit.sh | sh
          echo "$HOME/.kraftkit/bin" >> $GITHUB_PATH
      
      - name: Load vsock module
        run: sudo modprobe vhost_vsock
      
      - name: Build guest binary
        run: bash scripts/build-unikraft-tool-executor.sh
      
      - name: Run load tests
        run: bash scripts/load-test-unikernels.sh 10
      
      - name: Upload logs
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: load-test-logs
          path: /tmp/osvm-load-test/
```

## Next Steps

After successful load testing:

1. **Production Deployment**
   - Review performance metrics
   - Adjust resource limits
   - Configure monitoring

2. **Scaling Strategy**
   - Plan CID allocation registry (Phase 3)
   - Consider connection pooling
   - Implement load balancing

3. **Enhanced Monitoring**
   - Add Prometheus metrics
   - Track boot time distribution
   - Monitor memory usage trends

4. **Real MCP Integration**
   - Connect to actual MCP servers (Solana, GitHub)
   - Test production workloads
   - Validate security isolation

## References

- [Unikraft Setup Guide](./unikraft-setup.md)
- [Phase 6 Testing Report](../PHASE6_TESTING_COMPLETE.md)
- [Unikernel Runtime Source](../src/services/unikernel_runtime.rs)
- [Load Test Script](../scripts/load-test-unikernels.sh)
