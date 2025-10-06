#!/bin/bash
# Wrapper script to run the 100 microVM test with proper environment

echo "🚀 Running 100 MicroVM Test"
echo "============================"
echo ""

# Check if we need sudo
if [ "$EUID" -eq 0 ]; then
    # Already root
    CARGO_PATH="/home/$SUDO_USER/.cargo/bin/cargo"
    if [ ! -x "$CARGO_PATH" ]; then
        CARGO_PATH="/home/larp/.cargo/bin/cargo"
    fi
else
    # Need sudo
    echo "This test requires root privileges for Firecracker."
    echo "Running with sudo..."
    CARGO_PATH="$HOME/.cargo/bin/cargo"
    exec sudo -E PATH=$PATH CARGO_HOME=$HOME/.cargo RUSTUP_HOME=$HOME/.rustup "$0" "$@"
fi

# Ensure we're in the project directory
cd "$(dirname "$0")"

# Check if Rust rootfs exists
RUST_ROOTFS="$HOME/.osvm/rootfs/mcp-server-rust.cpio"
if [ ! -f "$RUST_ROOTFS" ]; then
    echo "⚠️  Rust-enabled rootfs not found at: $RUST_ROOTFS"
    echo ""
    echo "Please build it first with:"
    echo "  ./scripts/build-guest-rootfs-with-rust.sh"
    echo ""
    echo "This rootfs is required for 'cargo install osvm' to work inside VMs."
    exit 1
fi

echo "✓ Found Rust rootfs: $RUST_ROOTFS"
echo ""

# Run the test
echo "Starting test..."
echo "Command: $CARGO_PATH test --test test_100_microvms -- --ignored --nocapture"
echo ""

$CARGO_PATH test --test test_100_microvms -- --ignored --nocapture