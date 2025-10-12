#!/bin/bash
# Build minimal guest rootfs without external dependencies
# This creates a basic rootfs using only the base image and Node.js

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOTFS_DIR="$HOME/.osvm/rootfs"
BUILD_DIR="$ROOTFS_DIR/build"
OUTPUT_DIR="$ROOTFS_DIR"

echo "========================================================="
echo "Building MINIMAL Guest Rootfs (No External Dependencies)"
echo "========================================================="
echo ""

# Check dependencies
echo "[1/7] Checking dependencies..."
if ! command -v docker &> /dev/null; then
    echo "ERROR: docker not found"
    exit 1
fi

if ! command -v cargo &> /dev/null; then
    echo "ERROR: cargo not found"
    exit 1
fi

echo "✓ All dependencies found"
echo ""

# Create directories
echo "[2/7] Creating build directories..."
mkdir -p "$BUILD_DIR"
mkdir -p "$OUTPUT_DIR"
echo "✓ Directories created"
echo ""

# Build mcp_vsock_wrapper
echo "[3/7] Building mcp_vsock_wrapper..."
cd "$PROJECT_ROOT/guest/mcp_vsock_wrapper"
cargo build --release --target x86_64-unknown-linux-musl 2>&1 | grep -E "(Compiling|Finished)" || true

if [ ! -f "target/x86_64-unknown-linux-musl/release/mcp_vsock_wrapper" ]; then
    echo "WARNING: musl target not available, building with default target..."
    cargo build --release
    WRAPPER_BINARY="target/release/mcp_vsock_wrapper"
else
    WRAPPER_BINARY="target/x86_64-unknown-linux-musl/release/mcp_vsock_wrapper"
fi

echo "✓ mcp_vsock_wrapper built: $WRAPPER_BINARY"
echo ""

# Create minimal rootfs using base Node image (already cached)
echo "[4/7] Creating minimal rootfs structure..."
cat > "$BUILD_DIR/Dockerfile" << 'EOF'
FROM node:18-slim

# No external package installation - use what's already in the base image
# node:18-slim already includes bash, coreutils, and basic utilities

# Create directory structure
RUN mkdir -p /app /data /mnt/osvm-config /tmp

# Install MCP servers (this uses npm which is already in the image)
RUN npm install -g \
    @modelcontextprotocol/server-solana \
    @modelcontextprotocol/server-github \
    @modelcontextprotocol/server-filesystem \
    || echo "WARNING: Some MCP servers failed to install"

# Clean npm cache
RUN npm cache clean --force || true
EOF

echo "Building Docker image (using cached layers)..."
docker build -t osvm-guest-rootfs-minimal "$BUILD_DIR" 2>&1 | tail -20

if [ ${PIPESTATUS[0]} -ne 0 ]; then
    echo ""
    echo "WARNING: Docker build had issues, but continuing..."
    echo ""
fi

CONTAINER_ID=$(docker create osvm-guest-rootfs-minimal)
docker export "$CONTAINER_ID" > "$BUILD_DIR/rootfs.tar"
docker rm "$CONTAINER_ID"

echo "✓ Rootfs structure created"
echo ""

# Extract and customize rootfs
echo "[5/7] Extracting and customizing rootfs..."
cd "$BUILD_DIR"
rm -rf rootfs
mkdir -p rootfs
cd rootfs
tar -xf ../rootfs.tar

# Copy mcp_vsock_wrapper
echo "  - Installing mcp_vsock_wrapper..."
mkdir -p usr/local/bin
cp "$PROJECT_ROOT/guest/mcp_vsock_wrapper/$WRAPPER_BINARY" usr/local/bin/mcp_vsock_wrapper
chmod +x usr/local/bin/mcp_vsock_wrapper

# Create init script
echo "  - Creating init script..."
cat > init << 'INIT_SCRIPT'
#!/bin/sh
# Minimal init script for MCP server microVM guest

echo "Starting MCP Server Guest Init (Minimal)..."

# Mount essential filesystems
mount -t proc none /proc 2>/dev/null || true
mount -t sysfs none /sys 2>/dev/null || true
mount -t devtmpfs none /dev 2>/dev/null || true
mkdir -p /dev/pts /dev/shm
mount -t devpts none /dev/pts 2>/dev/null || true
mount -t tmpfs none /dev/shm 2>/dev/null || true

# Load vsock module (may fail in some environments)
modprobe vsock 2>/dev/null || modprobe vhost_vsock 2>/dev/null || echo "Warning: vsock module not loaded"

# Set up networking
ip link set lo up 2>/dev/null || ifconfig lo up 2>/dev/null || true

# Set environment variables
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
export HOME=/root

# Log startup
echo "Guest environment initialized (minimal configuration)"
echo "Server ID: ${OSVM_MCP_SERVER_ID:-unknown}"
echo "Server Command: ${OSVM_MCP_SERVER_CMD:-not set}"

# Check if mcp_vsock_wrapper exists
if [ ! -x /usr/local/bin/mcp_vsock_wrapper ]; then
    echo "ERROR: mcp_vsock_wrapper not found or not executable"
    exec /bin/sh
fi

# Start mcp_vsock_wrapper
echo "Starting mcp_vsock_wrapper..."
exec /usr/local/bin/mcp_vsock_wrapper
INIT_SCRIPT

chmod +x init

echo "✓ Rootfs customized"
echo ""

# Create cpio archive
echo "[6/7] Creating cpio archive..."
find . -print0 | cpio --null -ov --format=newc > "$OUTPUT_DIR/mcp-server.cpio" 2>&1 | grep -v "blocks" || true
echo "✓ CPIO archive created: $OUTPUT_DIR/mcp-server.cpio"
echo ""

# Check kernel
echo "[7/7] Checking kernel..."
KERNEL_DIR="$HOME/.osvm/kernel"
if [ -f "$KERNEL_DIR/vmlinux.bin" ]; then
    echo "✓ Kernel found: $KERNEL_DIR/vmlinux.bin"
else
    echo "⚠ Kernel not found at: $KERNEL_DIR/vmlinux.bin"
    echo "  You'll need to download or build a kernel separately"
fi
echo ""

# Summary
echo "========================================================="
echo "Minimal Guest Rootfs Build Complete"
echo "========================================================="
echo ""
echo "Rootfs:  $OUTPUT_DIR/mcp-server.cpio"
if [ -f "$OUTPUT_DIR/mcp-server.cpio" ]; then
    echo "Size:    $(du -h "$OUTPUT_DIR/mcp-server.cpio" | cut -f1)"
fi
echo ""
echo "Note: This is a minimal build that may have limited functionality"
echo "      due to network issues preventing package installation."
echo ""
echo "To test:"
echo "  cargo test --test microvm_integration_tests -- --ignored --nocapture"
echo ""
echo "========================================================="
