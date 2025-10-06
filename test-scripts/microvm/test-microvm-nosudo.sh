#!/bin/bash
# Alternative test runner that doesn't require sudo
# This runs the configuration tests only (not the actual microVM tests)

set -e

echo "=================================================="
echo "MicroVM Configuration Test Runner (No Sudo)"
echo "=================================================="
echo ""
echo "This script runs only the configuration tests that don't"
echo "require actual microVM launching (no root privileges needed)."
echo ""

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Change to project directory
cd "$PROJECT_ROOT"
echo "Working directory: $PROJECT_ROOT"
echo ""

# Check that cargo is available
if ! command -v cargo &> /dev/null; then
    echo "❌ Cargo not found. Please ensure Rust is installed."
    echo "   Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 1
fi

echo "Cargo version: $(cargo --version)"
echo "Rustc version: $(rustc --version)"
echo ""

# Check files exist
echo "Checking required files..."
ROOTFS_PATH="$HOME/.osvm/rootfs/mcp-server.cpio"
KERNEL_PATH="$HOME/.osvm/kernel/vmlinux.bin"

if [ -f "$ROOTFS_PATH" ]; then
    echo "✓ Rootfs found: $ROOTFS_PATH ($(du -h $ROOTFS_PATH | cut -f1))"
else
    echo "⚠️  Rootfs not found (not required for config tests)"
fi

if [ -f "$KERNEL_PATH" ]; then
    echo "✓ Kernel found: $KERNEL_PATH ($(du -h $KERNEL_PATH | cut -f1))"
else
    echo "⚠️  Kernel not found (not required for config tests)"
fi
echo ""

# Run the configuration tests
echo "Running configuration tests (no microVM required)..."
echo "=================================================="
echo ""

TESTS=(
    "test_cid_allocation_determinism"
    "test_mcp_service_routing_priority"
    "test_mount_point_configuration"
    "test_resource_limit_configuration"
    "test_error_recovery"
)

PASSED=0
FAILED=0

for test in "${TESTS[@]}"; do
    echo -n "Running $test... "
    if cargo test --test microvm_integration_tests -- --exact "$test" --quiet 2>/dev/null; then
        echo "✓ PASSED"
        ((PASSED++))
    else
        echo "❌ FAILED"
        ((FAILED++))
    fi
done

echo ""
echo "=================================================="
echo "Test Summary"
echo "=================================================="
echo "✅ Passed: $PASSED"
echo "❌ Failed: $FAILED"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "All configuration tests passed!"
else
    echo "Some tests failed. Run with verbose output:"
    echo "  cargo test --test microvm_integration_tests -- --nocapture"
fi

echo ""
echo "Note: To run the full microVM tests (requires root):"
echo "  sudo ./scripts/test-microvm.sh"
echo ""
echo "Or run individual microVM tests with:"
echo "  sudo -E PATH=\$PATH cargo test --test microvm_integration_tests -- --ignored --nocapture"
echo ""