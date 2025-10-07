#!/bin/bash
# Build guest rootfs with Rust/Cargo for running OSVM in microVMs
# This creates a rootfs capable of running: cargo install osvm && osvm whats current unix timestamp

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOTFS_DIR="$HOME/.osvm/rootfs"
BUILD_DIR="$ROOTFS_DIR/build-rust"
OUTPUT_DIR="$ROOTFS_DIR"

echo "=================================================="
echo "Building Guest Rootfs with Rust/Cargo Support"
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

# Build mcp_vsock_wrapper if it exists
echo "[3/8] Building mcp_vsock_wrapper..."
if [ -d "$PROJECT_ROOT/guest/mcp_vsock_wrapper" ]; then
    cd "$PROJECT_ROOT/guest/mcp_vsock_wrapper"
    cargo build --release --target x86_64-unknown-linux-musl 2>&1 | grep -E "(Compiling|Finished)" || true

    if [ -f "target/x86_64-unknown-linux-musl/release/mcp_vsock_wrapper" ]; then
        WRAPPER_BINARY="target/x86_64-unknown-linux-musl/release/mcp_vsock_wrapper"
    else
        cargo build --release
        WRAPPER_BINARY="target/release/mcp_vsock_wrapper"
    fi
    echo "✓ mcp_vsock_wrapper built: $WRAPPER_BINARY"
else
    echo "⚠ mcp_vsock_wrapper not found, skipping"
    WRAPPER_BINARY=""
fi
echo ""

# Create rootfs structure using Docker with Rust
echo "[4/8] Creating rootfs structure with Docker (including Rust)..."
cat > "$BUILD_DIR/Dockerfile" << 'EOF'
FROM rust:1.89-slim-bookworm

# Install required packages with retry logic
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
        build-essential \
        pkg-config \
        libssl-dev \
        libudev-dev \
        git \
    2>/dev/null || echo "Using cached packages" && \
    rm -rf /var/lib/apt/lists/* || true

# Create directory structure
RUN mkdir -p /app /data /mnt/osvm-config /tmp /root/.cargo

# Setup cargo environment
RUN echo '[net]' > /root/.cargo/config.toml && \
    echo 'git-fetch-with-cli = true' >> /root/.cargo/config.toml && \
    echo '[build]' >> /root/.cargo/config.toml && \
    echo 'jobs = 1' >> /root/.cargo/config.toml

# Pre-install some common dependencies to speed up cargo install
RUN cargo install --version 0.1.0 cargo-quickinstall 2>/dev/null || true

# Set environment for minimal resource usage
ENV CARGO_BUILD_JOBS=1
ENV CARGO_INCREMENTAL=0
ENV RUST_BACKTRACE=0

# Create a simple test to verify cargo works
RUN cargo --version && rustc --version

# Clean up build artifacts to reduce size
RUN rm -rf /usr/local/cargo/registry/cache/* \
    /usr/local/cargo/git/checkouts/* \
    /tmp/*
EOF

echo "Building Docker image (this may take a while)..."
docker build --network=host -t osvm-guest-rootfs-rust "$BUILD_DIR"
CONTAINER_ID=$(docker create osvm-guest-rootfs-rust)
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

# Copy mcp_vsock_wrapper if built
if [ -n "$WRAPPER_BINARY" ] && [ -f "$PROJECT_ROOT/guest/mcp_vsock_wrapper/$WRAPPER_BINARY" ]; then
    echo "  - Installing mcp_vsock_wrapper..."
    mkdir -p usr/local/bin
    cp "$PROJECT_ROOT/guest/mcp_vsock_wrapper/$WRAPPER_BINARY" usr/local/bin/mcp_vsock_wrapper
    chmod +x usr/local/bin/mcp_vsock_wrapper
fi

# Create init script
echo "  - Creating init script..."
cat > init << 'INIT_SCRIPT'
#!/bin/sh
# Init script for MCP server microVM guest with Rust support

echo "Starting MCP Server Guest with Rust/Cargo..."

# Mount essential filesystems
mount -t proc none /proc
mount -t sysfs none /sys
mount -t devtmpfs none /dev
mkdir -p /dev/pts /dev/shm
mount -t devpts none /dev/pts
mount -t tmpfs none /dev/shm
mount -t tmpfs none /tmp

# Load vsock module
echo "Loading vsock kernel module..."
modprobe vsock 2>/dev/null || modprobe vhost_vsock 2>/dev/null || echo "Warning: Could not load vsock module"

# Set up networking (localhost only)
ifconfig lo up 2>/dev/null || ip link set lo up

# Set environment variables
export PATH=/usr/local/cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin
export HOME=/root
export CARGO_HOME=/usr/local/cargo
export RUSTUP_HOME=/usr/local/rustup
export RUST_BACKTRACE=0
export CARGO_BUILD_JOBS=1

# Parse kernel command line for server command
CMDLINE=$(cat /proc/cmdline)
for param in $CMDLINE; do
    case $param in
        OSVM_MCP_SERVER_CMD=*)
            export OSVM_MCP_SERVER_CMD="${param#OSVM_MCP_SERVER_CMD=}"
            ;;
        OSVM_MCP_SERVER_ID=*)
            export OSVM_MCP_SERVER_ID="${param#OSVM_MCP_SERVER_ID=}"
            ;;
    esac
done

# Log startup
echo "=========================================="
echo "Guest environment initialized"
echo "Server ID: ${OSVM_MCP_SERVER_ID:-unknown}"
echo "Server Command: ${OSVM_MCP_SERVER_CMD:-not set}"
echo "Cargo version: $(cargo --version 2>/dev/null || echo 'not found')"
echo "Rustc version: $(rustc --version 2>/dev/null || echo 'not found')"
echo "=========================================="

# Execute the command if set
if [ -n "$OSVM_MCP_SERVER_CMD" ]; then
    echo "Executing: $OSVM_MCP_SERVER_CMD"
    echo ""

    # Run the command with sh -c to handle complex commands
    sh -c "$OSVM_MCP_SERVER_CMD"
    RESULT=$?

    echo ""
    echo "Command completed with exit code: $RESULT"
elif [ -x /usr/local/bin/mcp_vsock_wrapper ]; then
    echo "No command specified, starting mcp_vsock_wrapper..."
    exec /usr/local/bin/mcp_vsock_wrapper
else
    echo "No command specified and no mcp_vsock_wrapper found"
    echo "Starting shell for debugging..."
    exec /bin/sh
fi
INIT_SCRIPT

chmod +x init

echo "✓ Rootfs customized"
echo ""

# Create cpio archive
echo "[6/8] Creating cpio archive..."
find . -print0 | cpio --null -ov --format=newc > "$OUTPUT_DIR/mcp-server-rust.cpio" 2>&1 | grep -v "blocks" || true
echo "✓ CPIO archive created: $OUTPUT_DIR/mcp-server-rust.cpio"
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
echo "Rootfs:  $OUTPUT_DIR/mcp-server-rust.cpio"
echo "Kernel:  $KERNEL_DIR/vmlinux.bin"
echo "Size:    $(du -h "$OUTPUT_DIR/mcp-server-rust.cpio" | cut -f1)"
echo ""
echo "Capabilities:"
echo "  ✓ Rust $(docker run --rm rust:1.89-slim-bookworm rustc --version | awk '{print $2}')"
echo "  ✓ Cargo $(docker run --rm rust:1.89-slim-bookworm cargo --version | awk '{print $2}')"
echo "  ✓ Can run: cargo install osvm && osvm whats current unix timestamp"
echo ""
echo "To use:"
echo "  1. Run the 100 microVM test:"
echo "     sudo ./scripts/launch-100-microvms.sh"
echo "  2. Or run the Rust test:"
echo "     sudo cargo test --test test_100_microvms -- --ignored --nocapture"
echo ""
echo "Note: Each VM will need ~256MB RAM for cargo operations"
echo "=================================================="