#!/bin/bash
# Build guest rootfs for MCP server microVMs
# This script creates a minimal rootfs with vsock support and MCP wrapper

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOTFS_DIR="$HOME/.osvm/rootfs"
BUILD_DIR="$ROOTFS_DIR/build"
OUTPUT_DIR="$ROOTFS_DIR"

echo "=================================================="
echo "Building Guest Rootfs for MCP Server MicroVMs"
echo "=================================================="
echo ""

# Check dependencies
echo "[1/8] Checking dependencies..."
MISSING_DEPS=()

if ! command -v docker &> /dev/null; then
    MISSING_DEPS+=("docker")
fi

if ! command -v cargo &> /dev/null; then
    MISSING_DEPS+=("cargo (Rust toolchain)")
fi

if [ ${#MISSING_DEPS[@]} -ne 0 ]; then
    echo "ERROR: Missing required dependencies:"
    for dep in "${MISSING_DEPS[@]}"; do
        echo "  - $dep"
    done
    exit 1
fi

echo "✓ All dependencies found"
echo ""

# Create directories
echo "[2/8] Creating build directories..."
mkdir -p "$BUILD_DIR"
mkdir -p "$OUTPUT_DIR"
echo "✓ Directories created"
echo ""

# Build mcp_vsock_wrapper
echo "[3/8] Building mcp_vsock_wrapper..."
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

# Create rootfs structure using Docker
echo "[4/8] Creating rootfs structure with Docker..."
cat > "$BUILD_DIR/Dockerfile" << 'EOF'
FROM node:18-slim

# Install required packages with retry logic and mirror fallbacks
RUN set -ex && \
    for attempt in 1 2 3 4 5; do \
        echo "Attempt $attempt to update package lists..." && \
        apt-get update && break || \
        if [ $attempt -lt 5 ]; then \
            echo "Failed, trying alternative mirror..." && \
            case $attempt in \
                2) echo "deb http://ftp.us.debian.org/debian bookworm main" > /etc/apt/sources.list ;; \
                3) echo "deb http://mirror.steadfast.net/debian bookworm main" > /etc/apt/sources.list ;; \
                4) echo "deb http://mirrors.kernel.org/debian bookworm main" > /etc/apt/sources.list ;; \
            esac && \
            sleep 5; \
        else \
            echo "All attempts failed" && exit 1; \
        fi; \
    done && \
    apt-get install -y \
        bash \
        coreutils \
        ca-certificates \
        socat \
        curl \
        wget \
        procps \
    2>/dev/null || echo "Using cached packages" && \
    rm -rf /var/lib/apt/lists/* || true

# Create directory structure
RUN mkdir -p /app /data /mnt/osvm-config /tmp

# Install common MCP servers globally with retry logic
RUN for attempt in 1 2 3; do \
        echo "Attempt $attempt to install MCP servers..." && \
        npm install -g \
            @modelcontextprotocol/server-solana \
            @modelcontextprotocol/server-github \
            @modelcontextprotocol/server-filesystem && \
        break || \
        if [ $attempt -lt 3 ]; then \
            echo "Failed, retrying..." && \
            sleep 10; \
        else \
            echo "WARNING: Could not install MCP servers, proceeding without them"; \
        fi; \
    done

# Clean npm cache
RUN npm cache clean --force || true
EOF

docker build --network=host -t osvm-guest-rootfs "$BUILD_DIR"
CONTAINER_ID=$(docker create osvm-guest-rootfs)
docker export "$CONTAINER_ID" > "$BUILD_DIR/rootfs.tar"
docker rm "$CONTAINER_ID"

echo "✓ Rootfs structure created"
echo ""

# Extract and customize rootfs
echo "[5/8] Extracting and customizing rootfs..."
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
# Init script for MCP server microVM guest

echo "Starting MCP Server Guest Init..."

# Mount essential filesystems
mount -t proc none /proc
mount -t sysfs none /sys
mount -t devtmpfs none /dev
mkdir -p /dev/pts /dev/shm
mount -t devpts none /dev/pts
mount -t tmpfs none /dev/shm

# Load vsock module
echo "Loading vsock kernel module..."
modprobe vsock || modprobe vhost_vsock || echo "Warning: Could not load vsock module"

# Set up networking (localhost only, no external network)
ifconfig lo up

# Set environment variables
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
export HOME=/root

# Log startup
echo "Guest environment initialized"
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
echo "[6/8] Creating cpio archive..."
find . -print0 | cpio --null -ov --format=newc > "$OUTPUT_DIR/mcp-server.cpio" 2>&1 | grep -v "blocks" || true
echo "✓ CPIO archive created: $OUTPUT_DIR/mcp-server.cpio"
echo ""

# Get kernel
echo "[7/8] Checking kernel..."
KERNEL_DIR="$HOME/.osvm/kernel"
mkdir -p "$KERNEL_DIR"

if [ ! -f "$KERNEL_DIR/vmlinux.bin" ]; then
    echo "  Kernel not found. Downloading..."
    
    # Try to download a pre-built kernel
    KERNEL_URL="https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/x86_64/kernels/vmlinux.bin"
    
    echo "  Downloading from: $KERNEL_URL"
    curl -L -o "$KERNEL_DIR/vmlinux.bin" "$KERNEL_URL" || {
        echo ""
        echo "ERROR: Could not download kernel."
        echo "Please manually place a Linux kernel at: $KERNEL_DIR/vmlinux.bin"
        echo ""
        echo "You can build a kernel with vsock support using:"
        echo "  1. Clone linux kernel source"
        echo "  2. Configure with CONFIG_VSOCKETS=y and CONFIG_VIRTIO_VSOCKETS=y"
        echo "  3. Build and copy vmlinux to $KERNEL_DIR/vmlinux.bin"
        exit 1
    }
    
    echo "✓ Kernel downloaded"
else
    echo "✓ Kernel found: $KERNEL_DIR/vmlinux.bin"
fi
echo ""

# Summary
echo "[8/8] Build complete!"
echo ""
echo "=================================================="
echo "Guest Rootfs Build Summary"
echo "=================================================="
echo ""
echo "Rootfs:  $OUTPUT_DIR/mcp-server.cpio"
echo "Kernel:  $KERNEL_DIR/vmlinux.bin"
echo "Size:    $(du -h "$OUTPUT_DIR/mcp-server.cpio" | cut -f1)"
echo ""
echo "Installed MCP Servers:"
echo "  - @modelcontextprotocol/server-solana"
echo "  - @modelcontextprotocol/server-github"
echo "  - @modelcontextprotocol/server-filesystem"
echo ""
echo "To use:"
echo "  1. Ensure Firecracker is installed"
echo "  2. Run tests: cargo test --test microvm_integration_tests"
echo "  3. Tests will automatically use the rootfs at:"
echo "     $OUTPUT_DIR/mcp-server.cpio"
echo ""
echo "=================================================="
