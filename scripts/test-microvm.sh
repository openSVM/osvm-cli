#!/bin/bash
# Test script for microVM integration
# Requires sudo access as Firecracker needs root privileges

set -e

echo "=================================================="
echo "MicroVM Integration Test Runner"
echo "=================================================="
echo ""

# Check if running as root
if [ "$EUID" -eq 0 ]; then
    echo "✓ Running as root"
else
    echo "⚠️  This script requires root privileges for Firecracker"
    echo "Please run with: sudo $0"
    exit 1
fi

# Setup Rust environment for sudo
setup_rust_env() {
    # Try to find the Rust installation
    local RUST_DIRS=(
        "$HOME/.cargo"
        "/home/$SUDO_USER/.cargo"
        "/usr/local/cargo"
        "/opt/rust"
    )

    for dir in "${RUST_DIRS[@]}"; do
        if [ -d "$dir/bin" ]; then
            export PATH="$dir/bin:$PATH"
            export CARGO_HOME="$dir"
            export RUSTUP_HOME="${dir%/.cargo}/.rustup"

            # Check if we can find cargo and rustc
            if [ -x "$dir/bin/cargo" ] && [ -x "$dir/bin/rustc" ]; then
                echo "✓ Found Rust toolchain in: $dir"
                export CARGO="$dir/bin/cargo"
                export RUSTC="$dir/bin/rustc"

                # Disable rustup auto-update during tests
                export RUSTUP_UPDATE_ROOT="https://static.rust-lang.org/rustup"
                export CARGO_NET_OFFLINE=false

                return 0
            fi
        fi
    done

    return 1
}

# Setup Rust environment
echo "Setting up Rust environment..."
if ! setup_rust_env; then
    echo "❌ Could not setup Rust environment."
    echo "   Please ensure Rust is installed for the current user."
    echo "   Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 1
fi

# Verify we can run both cargo and rustc
echo "Verifying Rust toolchain..."
if ! $CARGO --version >/dev/null 2>&1; then
    echo "❌ Cargo is not working properly"
    exit 1
fi

if ! $RUSTC --version >/dev/null 2>&1; then
    echo "❌ Rustc is not working properly"
    exit 1
fi

echo "✓ Cargo version: $($CARGO --version)"
echo "✓ Rustc version: $($RUSTC --version)"

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Change to project directory for cargo commands
cd "$PROJECT_ROOT"
echo "✓ Working directory: $PROJECT_ROOT"
echo ""

# Verify Firecracker is installed
echo "Checking Firecracker installation..."
if ! command -v firecracker &> /dev/null; then
    echo "❌ Firecracker not found. Please install it first:"
    echo "   curl -L https://github.com/firecracker-microvm/firecracker/releases/latest/download/firecracker-v1.6.0-x86_64.tgz | tar -xz"
    echo "   sudo mv release-v1.6.0-x86_64/firecracker-v1.6.0-x86_64 /usr/bin/firecracker"
    echo "   sudo chmod +x /usr/bin/firecracker"
    exit 1
fi
echo "✓ Firecracker found: $(firecracker --version 2>&1 | head -1)"
echo ""

# Verify rootfs and kernel exist
echo "Checking required files..."
ROOTFS_PATH="$HOME/.osvm/rootfs/mcp-server.cpio"
KERNEL_PATH="$HOME/.osvm/kernel/vmlinux.bin"

if [ ! -f "$ROOTFS_PATH" ]; then
    echo "❌ Rootfs not found at: $ROOTFS_PATH"
    echo "   Run: bash scripts/build-guest-rootfs.sh"
    exit 1
fi
echo "✓ Rootfs found: $ROOTFS_PATH ($(du -h $ROOTFS_PATH | cut -f1))"

if [ ! -f "$KERNEL_PATH" ]; then
    echo "❌ Kernel not found at: $KERNEL_PATH"
    echo "   The build script should have downloaded it"
    exit 1
fi
echo "✓ Kernel found: $KERNEL_PATH ($(du -h $KERNEL_PATH | cut -f1))"
echo ""

# Run tests
echo "Running microVM integration tests..."
echo "=================================================="
echo ""

# First run the basic tests that passed
echo "1. Running unit tests (non-microVM)..."
$CARGO test --test microvm_integration_tests -- --exact \
    test_cid_allocation_determinism \
    test_mcp_service_routing_priority \
    test_mount_point_configuration \
    test_resource_limit_configuration \
    test_error_recovery
echo ""

# Then attempt the ignored tests that require actual microVMs
echo "2. Running full microVM tests (requires root)..."
echo "   These tests will:"
echo "   - Launch actual Firecracker microVMs"
echo "   - Test vsock communication"
echo "   - Verify MCP server integration"
echo ""

# Set environment variables for the tests
export OSVM_ROOTFS_PATH="$ROOTFS_PATH"
export OSVM_KERNEL_PATH="$KERNEL_PATH"
export RUST_LOG=debug

# Run ignored tests one at a time for better debugging
TESTS=(
    "test_microvm_launch_and_init"
    "test_vsock_request_response"
    "test_health_check_monitoring"
    "test_multiple_concurrent_servers"
    "test_graceful_shutdown_all"
)

for test in "${TESTS[@]}"; do
    echo "Running: $test"
    if $CARGO test --test microvm_integration_tests -- --ignored --exact "$test" --nocapture; then
        echo "✓ $test passed"
    else
        echo "❌ $test failed"
    fi
    echo ""
done

echo "=================================================="
echo "Test Summary"
echo "=================================================="
echo ""
echo "To run these tests manually:"
echo "  sudo RUST_LOG=debug $CARGO test --test microvm_integration_tests -- --ignored --nocapture"
echo ""
echo "For a specific test:"
echo "  sudo $CARGO test --test microvm_integration_tests test_microvm_launch_and_init -- --ignored --nocapture"
echo ""