#!/bin/bash
# Phase 2 Unikernel Integration - Toolchain Setup Script
#
# This script sets up the Unikraft/kraft toolchain for building ephemeral unikernels

set -e

echo "======================================================"
echo " Phase 2 Unikraft Toolchain Setup"
echo "======================================================"
echo

# Detect if running in Flatpak
if [ -f /.flatpak-info ]; then
    echo "✓ Running in Flatpak environment"
    KRAFT_CMD="flatpak-spawn --host -- kraft"
    CARGO_CMD="cargo"
else
    echo "✓ Running in native environment"
    KRAFT_CMD="kraft"
    CARGO_CMD="cargo"
fi

# Check kraft installation
echo "Checking kraft installation..."
if $KRAFT_CMD version >/dev/null 2>&1; then
    KRAFT_VERSION=$($KRAFT_CMD version | head -1)
    echo "✓ kraft is installed: $KRAFT_VERSION"
else
    echo "✗ kraft is not installed or not accessible"
    echo
    echo "To install kraft, run:"
    echo "  curl --proto '=https' -sSf https://get.kraftkit.sh | sh"
    echo
    echo "Or via package manager (if available):"
    echo "  sudo apt install kraftkit"
    exit 1
fi

# Check KVM access
echo
echo "Checking KVM access..."
if [ -c /dev/kvm ]; then
    echo "✓ /dev/kvm exists"
    if [ -r /dev/kvm ] && [ -w /dev/kvm ]; then
        echo "✓ KVM is accessible (read/write permissions)"
    else
        echo "⚠ KVM device exists but may not be accessible"
        echo "  Current permissions: $(ls -l /dev/kvm)"
        echo "  You may need to add your user to the 'kvm' group:"
        echo "  sudo usermod -aG kvm $USER"
    fi
else
    echo "✗ /dev/kvm not found"
    echo "  Make sure KVM is enabled in your system"
    exit 1
fi

# Check Rust musl target
echo
echo "Checking Rust toolchain..."
if rustc --version >/dev/null 2>&1; then
    RUST_VERSION=$(rustc --version)
    echo "✓ Rust is installed: $RUST_VERSION"
else
    echo "✗ Rust is not installed"
    echo "  Install via: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 1
fi

echo
echo "Checking x86_64-unknown-linux-musl target..."
if rustup target list | grep -q "x86_64-unknown-linux-musl (installed)"; then
    echo "✓ x86_64-unknown-linux-musl target is installed"
else
    echo "⚠ x86_64-unknown-linux-musl target not installed"
    echo "  Installing..."
    rustup target add x86_64-unknown-linux-musl
    echo "✓ Target installed successfully"
fi

# Check musl-tools
echo
echo "Checking musl-tools..."
if which musl-gcc >/dev/null 2>&1; then
    echo "✓ musl-tools are installed"
else
    echo "⚠ musl-tools not found"
    echo "  Install via: sudo apt install musl-tools"
    echo "  This is optional but recommended for better compatibility"
fi

# Create directories
echo
echo "Creating directories..."
mkdir -p ~/.osvm/unikernels
mkdir -p guest/unikraft_tool_executor/src
echo "✓ Directories created"

# Test kraft with a simple command
echo
echo "Testing kraft..."
if $KRAFT_CMD list >/dev/null 2>&1; then
    echo "✓ kraft is working correctly"
else
    echo "⚠ kraft list command failed, but kraft is installed"
fi

echo
echo "======================================================"
echo " Toolchain Setup Complete!"
echo "======================================================"
echo
echo "Next steps:"
echo "  1. Build the unikraft tool executor: scripts/build-unikraft-tool-executor.sh"
echo "  2. Review the implementation plan: PHASE2_UNIKERNEL_IMPLEMENTATION_PLAN.md"
echo
echo "Environment summary:"
echo "  - kraft: $(echo $KRAFT_VERSION)"
echo "  - Rust: $RUST_VERSION"
echo "  - musl target: x86_64-unknown-linux-musl"
echo "  - KVM: /dev/kvm"
echo "  - Unikernel dir: ~/.osvm/unikernels"
echo
