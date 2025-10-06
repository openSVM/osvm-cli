# OSVM Test Scripts

Organized collection of test scripts for the OSVM CLI system.

## Directory Structure

### 📁 `chat/`
Tests for the agent chat interface and AI planning features.
- `test_advanced_chat.sh` - Advanced chat UI testing
- `test_ai_planning.sh` - AI planning functionality
- `test_plan_*.sh` - Various planning command tests
- `test_e2e_chat.sh` - End-to-end chat testing

### 📁 `microvm/`
Tests specific to microVM functionality and isolation.
- `test_ephemeral_microvms.sh` - Ephemeral VM lifecycle testing
- `test-microvm.sh` - General microVM tests
- `test-microvm-nosudo.sh` - Non-privileged microVM tests
- `run-100-vms-test.sh` - Stress test with 100 VMs

### 📁 `integration/`
Integration and general functionality tests.
- `basic_test.sh` - Basic functionality tests
- `comprehensive_test.sh` - Full integration test suite
- `test-deployment.sh` - Deployment testing
- `test_security_vulns.sh` - Security vulnerability testing

### 📁 `performance/`
Performance, load, and stress testing scripts.
- `launch-*.sh` - Various VM launch configurations
- `load-test-*.sh` - Load testing for microVMs and unikernels

## Usage

```bash
# Run a specific test
./test-scripts/microvm/test_ephemeral_microvms.sh

# Run all tests in a category
for test in test-scripts/chat/*.sh; do
    echo "Running $test..."
    bash "$test"
done

# Run performance tests
./test-scripts/performance/launch-100-microvms.sh
```

## Test Categories

### Unit Tests
Located in `tests/` directory (Rust tests)

### Integration Tests
- Chat interface integration
- MCP tool integration
- MicroVM lifecycle

### Performance Tests
- VM launch performance
- Concurrent VM management
- Resource utilization

### Security Tests
- Isolation validation
- Vulnerability scanning
- Permission testing

## Requirements

- Rust toolchain for compilation
- Firecracker for microVM tests
- Appropriate permissions for VM operations
- Sufficient system resources for stress tests

## Contributing

When adding new tests:
1. Place in appropriate category directory
2. Use descriptive names (test_<feature>.sh)
3. Include comments explaining test purpose
4. Ensure proper error handling
5. Make scripts executable (chmod +x)