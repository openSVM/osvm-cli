#!/bin/bash
# Test a single microVM to debug initialization issues

set -e

echo "=================================================="
echo "Single MicroVM Test - Debug Mode"
echo "=================================================="
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo "This script requires root privileges"
    echo "Please run with: sudo $0"
    exit 1
fi

# 1. Check vsock module
echo "[1/6] Checking vsock module..."
if ! lsmod | grep -q vsock; then
    echo "Loading vhost_vsock module..."
    modprobe vhost_vsock 2>/dev/null || modprobe vsock 2>/dev/null || echo "Could not load vsock"
fi
lsmod | grep vsock || echo "No vsock module loaded"
echo ""

# 2. Check KVM
echo "[2/6] Checking KVM..."
if [ -e /dev/kvm ]; then
    echo "✓ KVM available"
    ls -la /dev/kvm
else
    echo "❌ KVM not available - Firecracker won't work!"
    exit 1
fi
echo ""

# 3. Check files
echo "[3/6] Checking rootfs and kernel..."
ROOTFS_RUST="/root/.osvm/rootfs/mcp-server-rust.cpio"
ROOTFS_BASIC="/root/.osvm/rootfs/mcp-server.cpio"
KERNEL="/root/.osvm/kernel/vmlinux.bin"

if [ -f "$ROOTFS_RUST" ]; then
    ROOTFS="$ROOTFS_RUST"
    echo "✓ Using Rust rootfs: $ROOTFS ($(du -h $ROOTFS | cut -f1))"
elif [ -f "$ROOTFS_BASIC" ]; then
    ROOTFS="$ROOTFS_BASIC"
    echo "✓ Using basic rootfs: $ROOTFS ($(du -h $ROOTFS | cut -f1))"
else
    echo "❌ No rootfs found!"
    echo "Build it with: ./scripts/build-guest-rootfs.sh"
    exit 1
fi

if [ -f "$KERNEL" ]; then
    echo "✓ Kernel found: $KERNEL ($(du -h $KERNEL | cut -f1))"
else
    echo "❌ Kernel not found!"
    exit 1
fi
echo ""

# 4. Create test directory
echo "[4/6] Creating test environment..."
TEST_DIR="/tmp/single-vm-test"
rm -rf $TEST_DIR
mkdir -p $TEST_DIR
cd $TEST_DIR
echo "Working directory: $TEST_DIR"
echo ""

# 5. Create Firecracker config
echo "[5/6] Creating Firecracker configuration..."

# Create the log file first
touch $TEST_DIR/firecracker.log

cat > config.json << EOF
{
  "boot-source": {
    "kernel_image_path": "$KERNEL",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init",
    "initrd_path": "$ROOTFS"
  },
  "drives": [],
  "machine-config": {
    "vcpu_count": 1,
    "mem_size_mib": 128,
    "smt": false
  },
  "vsock": {
    "guest_cid": 3,
    "uds_path": "$TEST_DIR/vsock.sock"
  }
}
EOF
echo "Config saved to: $TEST_DIR/config.json"
echo ""

# 6. Launch Firecracker
echo "[6/6] Launching Firecracker microVM..."
echo "Starting Firecracker with:"
echo "  Config: $TEST_DIR/config.json"
echo "  API socket: $TEST_DIR/api.sock"
echo "  Log: $TEST_DIR/firecracker.log"
echo ""

# Create a FIFO for console output
mkfifo $TEST_DIR/console.fifo || true

# Start Firecracker in background
echo "Launching Firecracker..."
timeout 10 firecracker \
    --api-sock $TEST_DIR/api.sock \
    --config-file $TEST_DIR/config.json \
    > $TEST_DIR/console.log 2>&1 &

FC_PID=$!
echo "Firecracker PID: $FC_PID"
echo ""

# Wait a bit
sleep 2

# Check if it's still running
if kill -0 $FC_PID 2>/dev/null; then
    echo "✓ Firecracker is running"

    # Show console output
    echo ""
    echo "Console output (first 50 lines):"
    echo "================================"
    head -50 $TEST_DIR/console.log || echo "No console output yet"
    echo "================================"

    # Kill it
    echo ""
    echo "Stopping Firecracker..."
    kill -TERM $FC_PID 2>/dev/null || true
    sleep 1
    kill -KILL $FC_PID 2>/dev/null || true
else
    echo "❌ Firecracker exited early"
    echo ""
    echo "Console output:"
    cat $TEST_DIR/console.log
fi

echo ""
echo "Firecracker log (last 50 lines):"
echo "================================="
tail -50 $TEST_DIR/firecracker.log 2>/dev/null || echo "No log file created"
echo "================================="

echo ""
echo "Debugging Information:"
echo "====================="
echo "1. Console log: $TEST_DIR/console.log"
echo "2. Firecracker log: $TEST_DIR/firecracker.log"
echo "3. Config: $TEST_DIR/config.json"
echo ""
echo "Common issues:"
echo "  - No vsock module: modprobe vhost_vsock"
echo "  - Wrong kernel: Need vsock-enabled kernel"
echo "  - Init script issues: Check rootfs init script"
echo "  - Memory too low: Increase mem_size_mib"
echo ""