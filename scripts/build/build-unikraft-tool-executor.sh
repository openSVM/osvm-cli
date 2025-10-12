#!/bin/bash
# Build script for Unikraft tool executor
#
# This script builds the unikraft_tool_executor as a static musl binary
# that can be packaged with Unikraft/kraft

set -e

echo "======================================================"
echo " Building Unikraft Tool Executor"
echo "======================================================"
echo

# Detect if running in Flatpak
if [ -f /.flatpak-info ]; then
    echo "✓ Running in Flatpak environment"
    IN_FLATPAK=true
else
    echo "✓ Running in native environment"
    IN_FLATPAK=false
fi

# Project paths
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GUEST_DIR="$PROJECT_ROOT/guest/unikraft_tool_executor"
INSTALL_DIR="$HOME/.osvm/unikernels"

echo "Project root: $PROJECT_ROOT"
echo "Guest directory: $GUEST_DIR"
echo "Install directory: $INSTALL_DIR"
echo

# Check if guest directory exists
if [ ! -d "$GUEST_DIR" ]; then
    echo "✗ Guest directory not found: $GUEST_DIR"
    exit 1
fi

cd "$GUEST_DIR"

# Check Rust musl target
echo "Checking Rust musl target..."
if ! rustup target list | grep -q "x86_64-unknown-linux-musl (installed)"; then
    echo "⚠ x86_64-unknown-linux-musl target not installed"
    echo "  Installing..."
    rustup target add x86_64-unknown-linux-musl
fi

# Build the binary
echo
echo "Building static musl binary..."
echo "Command: cargo build --release --target x86_64-unknown-linux-musl"
echo

if cargo build --release --target x86_64-unknown-linux-musl; then
    echo "✓ Build successful"
else
    echo "✗ Build failed"
    exit 1
fi

# Check binary exists
BINARY_PATH="target/x86_64-unknown-linux-musl/release/unikraft_tool_executor"
if [ ! -f "$BINARY_PATH" ]; then
    echo "✗ Binary not found at $BINARY_PATH"
    exit 1
fi

# Get binary information
echo
echo "Binary information:"
ls -lh "$BINARY_PATH"
file "$BINARY_PATH"

# Check if truly static
echo
echo "Checking if binary is statically linked..."
if ldd "$BINARY_PATH" 2>&1 | grep -q "not a dynamic executable"; then
    echo "✓ Binary is statically linked"
else
    echo "⚠ Binary may have dynamic dependencies:"
    ldd "$BINARY_PATH" || true
fi

# Strip binary to reduce size
echo
echo "Stripping binary to reduce size..."
ORIGINAL_SIZE=$(stat -f %z "$BINARY_PATH" 2>/dev/null || stat -c %s "$BINARY_PATH")
strip "$BINARY_PATH"
STRIPPED_SIZE=$(stat -f %z "$BINARY_PATH" 2>/dev/null || stat -c %s "$BINARY_PATH")
echo "Original size: $(numfmt --to=iec $ORIGINAL_SIZE 2>/dev/null || echo "$ORIGINAL_SIZE bytes")"
echo "Stripped size: $(numfmt --to=iec $STRIPPED_SIZE 2>/dev/null || echo "$STRIPPED_SIZE bytes")"

# Check if binary is small enough (<5MB target)
if [ $STRIPPED_SIZE -lt 5242880 ]; then
    echo "✓ Binary size is under 5MB target"
else
    echo "⚠ Binary size exceeds 5MB target ($(numfmt --to=iec $STRIPPED_SIZE))"
fi

# Create install directory
echo
echo "Creating install directory..."
mkdir -p "$INSTALL_DIR"

# Copy binary to install location
echo "Installing binary to $INSTALL_DIR..."
cp "$BINARY_PATH" "$INSTALL_DIR/unikraft_tool_executor"
chmod +x "$INSTALL_DIR/unikraft_tool_executor"

echo
echo "======================================================"
echo " Build Complete!"
echo "======================================================"
echo
echo "Binary location: $INSTALL_DIR/unikraft_tool_executor"
echo "Binary size: $(numfmt --to=iec $STRIPPED_SIZE 2>/dev/null || echo "$STRIPPED_SIZE bytes")"
echo
echo "Next steps:"
echo "  1. Test the binary: $INSTALL_DIR/unikraft_tool_executor --help"
echo "  2. Package with kraft (if needed)"
echo "  3. Run integration tests"
echo
