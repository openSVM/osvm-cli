#!/bin/bash
# Build guest rootfs for MCP server microVMs
# This script creates a minimal rootfs with vsock support and MCP wrapper

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
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

# Mount essential filesystems FIRST
mount -t proc none /proc 2>/dev/null
mount -t sysfs none /sys 2>/dev/null
mount -t devtmpfs none /dev 2>/dev/null
mkdir -p /dev/pts /dev/shm /tmp
mount -t devpts none /dev/pts 2>/dev/null
mount -t tmpfs none /dev/shm 2>/dev/null
mount -t tmpfs none /tmp 2>/dev/null

# NOW setup debug log file and console output
DEBUG_LOG="/tmp/init-debug.log"
CONSOLE="/dev/console"
exec 2>&1  # Redirect stderr to stdout

# Write directly to console to ensure visibility
echo "===== INIT STARTING =====" > "$CONSOLE"
echo "===== MCP Server Guest Init Starting at $(date) =====" > "$DEBUG_LOG"
echo "Starting MCP Server Guest Init..." | tee -a "$DEBUG_LOG" > "$CONSOLE"

echo "Filesystems mounted" | tee -a "$DEBUG_LOG" "$CONSOLE"

# Load vsock module
echo "Loading vsock kernel module..." | tee -a "$DEBUG_LOG" "$CONSOLE"
if modprobe vsock 2>&1 | tee -a "$DEBUG_LOG" "$CONSOLE"; then
    echo "vsock module loaded" | tee -a "$DEBUG_LOG" "$CONSOLE"
elif modprobe vhost_vsock 2>&1 | tee -a "$DEBUG_LOG" "$CONSOLE"; then
    echo "vhost_vsock module loaded" | tee -a "$DEBUG_LOG" "$CONSOLE"
else
    echo "Warning: Could not load vsock module" | tee -a "$DEBUG_LOG" "$CONSOLE"
fi

# Set up networking
echo "Setting up network..." | tee -a "$DEBUG_LOG" "$CONSOLE"
ifconfig lo up 2>&1 | tee -a "$DEBUG_LOG"

# Configure eth0 if available (for internet access)
if ip link show eth0 2>/dev/null | tee -a "$DEBUG_LOG"; then
    echo "Configuring network interface eth0..." | tee -a "$DEBUG_LOG" "$CONSOLE"
    ip addr add 172.16.0.2/24 dev eth0 2>&1 | tee -a "$DEBUG_LOG"
    ip link set eth0 up 2>&1 | tee -a "$DEBUG_LOG"
    ip route add default via 172.16.0.1 2>&1 | tee -a "$DEBUG_LOG"

    # Configure DNS
    echo "nameserver 8.8.8.8" > /etc/resolv.conf
    echo "nameserver 8.8.4.4" >> /etc/resolv.conf

    echo "Network configured: eth0 with IP 172.16.0.2" | tee -a "$DEBUG_LOG" "$CONSOLE"
else
    echo "eth0 not found, skipping network config" | tee -a "$DEBUG_LOG" "$CONSOLE"
fi

# Set environment variables
echo "Setting PATH and HOME..." | tee -a "$DEBUG_LOG" "$CONSOLE"
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
export HOME=/root

# Parse kernel command line for OSVM environment variables
echo "Parsing kernel command line..." | tee -a "$DEBUG_LOG" "$CONSOLE"
echo "Full cmdline: $(cat /proc/cmdline)" | tee -a "$DEBUG_LOG" "$CONSOLE"

for param in $(cat /proc/cmdline); do
    case "$param" in
        OSVM_MCP_SERVER_ID=*)
            export OSVM_MCP_SERVER_ID="${param#*=}"
            echo "Found SERVER_ID: $OSVM_MCP_SERVER_ID" | tee -a "$DEBUG_LOG" "$CONSOLE"
            ;;
        OSVM_MCP_SERVER_CMD=*)
            # Decode spaces and export
            export OSVM_MCP_SERVER_CMD=$(echo "${param#*=}" | sed 's/___SPACE___/ /g')
            echo "Found SERVER_CMD: $OSVM_MCP_SERVER_CMD" | tee -a "$DEBUG_LOG" "$CONSOLE"
            ;;
    esac
done

# Log startup
echo "===== Guest environment initialized =====" | tee -a "$DEBUG_LOG" "$CONSOLE"
echo "Server ID: ${OSVM_MCP_SERVER_ID:-NOT_SET}" | tee -a "$DEBUG_LOG" "$CONSOLE"
echo "Server Command: ${OSVM_MCP_SERVER_CMD:-NOT_SET}" | tee -a "$DEBUG_LOG" "$CONSOLE"

# Check if mcp_vsock_wrapper exists
echo "Checking for mcp_vsock_wrapper..." | tee -a "$DEBUG_LOG" "$CONSOLE"
if [ ! -f /usr/local/bin/mcp_vsock_wrapper ]; then
    echo "ERROR: mcp_vsock_wrapper not found at /usr/local/bin/mcp_vsock_wrapper" | tee -a "$DEBUG_LOG" "$CONSOLE"
    ls -l /usr/local/bin/ | tee -a "$DEBUG_LOG"
    exec /bin/sh
fi

if [ ! -x /usr/local/bin/mcp_vsock_wrapper ]; then
    echo "ERROR: mcp_vsock_wrapper not executable" | tee -a "$DEBUG_LOG" "$CONSOLE"
    ls -l /usr/local/bin/mcp_vsock_wrapper | tee -a "$DEBUG_LOG"
    exec /bin/sh
fi

# Start mcp_vsock_wrapper with output redirected to log
echo "Starting mcp_vsock_wrapper..." | tee -a "$DEBUG_LOG" "$CONSOLE"
echo "Environment:" | tee -a "$DEBUG_LOG" "$CONSOLE"
env | grep OSVM | tee -a "$DEBUG_LOG" "$CONSOLE"

# Run wrapper with logging and capture exit code
/usr/local/bin/mcp_vsock_wrapper 2>&1 | tee -a "$DEBUG_LOG" "$CONSOLE"
EXIT_CODE=$?

# If wrapper exited (shouldn't happen normally), log and dump debug info to console
if [ $EXIT_CODE -ne 0 ]; then
    echo "===== MCP WRAPPER EXITED WITH CODE $EXIT_CODE =====" | tee -a "$DEBUG_LOG" "$CONSOLE"
    echo "===== DEBUG LOG CONTENTS =====" | tee -a "$DEBUG_LOG" "$CONSOLE"
    cat "$DEBUG_LOG" > "$CONSOLE"
    echo "===== ENTERING EMERGENCY SHELL =====" | tee -a "$DEBUG_LOG" "$CONSOLE"
    exec /bin/sh
else
    echo "===== MCP WRAPPER EXITED NORMALLY =====" | tee -a "$DEBUG_LOG" "$CONSOLE"
    cat "$DEBUG_LOG" > "$CONSOLE"
    exec /bin/sh
fi
INIT_SCRIPT

chmod +x init

echo "✓ Rootfs customized"
echo ""

# Create cpio archive (gzip-compressed for kernel 5.10+)
echo "[6/8] Creating gzip-compressed cpio archive..."
find . -print0 | cpio --null -o --format=newc 2>/dev/null | gzip -9 > "$OUTPUT_DIR/mcp-server.cpio.gz"
echo "✓ CPIO archive created: $OUTPUT_DIR/mcp-server.cpio.gz"
echo ""

# Get kernel
echo "[7/8] Checking kernel..."
KERNEL_DIR="$HOME/.osvm/kernel"
mkdir -p "$KERNEL_DIR"

# Prefer 5.10 kernel with vsock support
if [ ! -f "$KERNEL_DIR/vmlinux.bin" ]; then
    if [ -f "$KERNEL_DIR/vmlinux-5.10.bin" ]; then
        echo "  Using kernel 5.10 with vsock support"
        ln -sf "$KERNEL_DIR/vmlinux-5.10.bin" "$KERNEL_DIR/vmlinux.bin"
    else
        echo "  Kernel not found. Downloading 5.10 kernel with vsock support..."

        # Download kernel 5.10 with vsock support
        KERNEL_URL="https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/x86_64/kernels/vmlinux-5.10.bin"

        echo "  Downloading from: $KERNEL_URL"
        curl -L -o "$KERNEL_DIR/vmlinux-5.10.bin" "$KERNEL_URL" && \
        ln -sf "$KERNEL_DIR/vmlinux-5.10.bin" "$KERNEL_DIR/vmlinux.bin" || {
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
    fi
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
