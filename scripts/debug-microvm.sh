#!/bin/bash
# Debug script for microVM launch issues

set -e

echo "=================================================="
echo "MicroVM Debug Helper"
echo "=================================================="
echo ""

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo "This script requires root privileges for Firecracker"
    echo "Please run with: sudo $0"
    exit 1
fi

# Enable debug logging
export RUST_LOG=debug
export RUST_BACKTRACE=1

echo "1. Checking system requirements..."
echo ""

# Check KVM support
echo -n "KVM support: "
if [ -e /dev/kvm ]; then
    echo "✓ Available"
    ls -la /dev/kvm
else
    echo "❌ Not available"
    echo "   KVM is required for Firecracker. Enable virtualization in BIOS."
fi
echo ""

# Check vsock module
echo -n "Vsock module: "
if lsmod | grep -q vsock; then
    echo "✓ Loaded"
    lsmod | grep vsock
else
    echo "⚠️  Not loaded, attempting to load..."
    modprobe vhost_vsock 2>/dev/null || modprobe vsock 2>/dev/null || echo "Could not load vsock"
fi
echo ""

# Check Firecracker
echo -n "Firecracker: "
if command -v firecracker &> /dev/null; then
    firecracker --version 2>&1 | head -1
else
    echo "❌ Not found"
fi
echo ""

# Check files
echo "2. Checking required files..."
ROOTFS="/root/.osvm/rootfs/mcp-server.cpio"
KERNEL="/root/.osvm/kernel/vmlinux.bin"

if [ -f "$ROOTFS" ]; then
    echo "✓ Rootfs: $ROOTFS ($(du -h $ROOTFS | cut -f1))"

    # Extract and check init script
    echo "  Checking init script in rootfs..."
    TEMP_DIR=$(mktemp -d)
    cd $TEMP_DIR
    cpio -idm < $ROOTFS 2>/dev/null
    if [ -f init ]; then
        echo "  ✓ Init script found"
        echo "  First 5 lines of init:"
        head -5 init | sed 's/^/    /'
    else
        echo "  ❌ Init script not found!"
    fi
    cd - > /dev/null
    rm -rf $TEMP_DIR
else
    echo "❌ Rootfs not found at $ROOTFS"
fi
echo ""

if [ -f "$KERNEL" ]; then
    echo "✓ Kernel: $KERNEL ($(du -h $KERNEL | cut -f1))"

    # Check if kernel has vsock support (basic check)
    echo "  Checking kernel for vsock strings..."
    if strings $KERNEL | grep -q vsock; then
        echo "  ✓ Kernel appears to have vsock references"
    else
        echo "  ⚠️  No vsock strings found in kernel (might still be supported)"
    fi
else
    echo "❌ Kernel not found at $KERNEL"
fi
echo ""

echo "3. Testing basic Firecracker launch..."
echo ""

# Create a minimal Firecracker config
CONFIG_FILE="/tmp/firecracker-test-config.json"
cat > $CONFIG_FILE << 'EOF'
{
  "boot-source": {
    "kernel_image_path": "/root/.osvm/kernel/vmlinux.bin",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init",
    "initrd_path": "/root/.osvm/rootfs/mcp-server.cpio"
  },
  "drives": [],
  "machine-config": {
    "vcpu_count": 1,
    "mem_size_mib": 128,
    "smt": false
  },
  "vsock": {
    "guest_cid": 3,
    "uds_path": "/tmp/firecracker-vsock.sock"
  }
}
EOF

echo "Created test config at: $CONFIG_FILE"
echo ""
echo "To manually test Firecracker:"
echo "  1. In terminal 1, start Firecracker:"
echo "     sudo firecracker --api-sock /tmp/firecracker.socket --config-file $CONFIG_FILE"
echo ""
echo "  2. In terminal 2, check the VM:"
echo "     sudo screen /dev/pts/X  # Replace X with the pts number from Firecracker output"
echo ""
echo "  3. Or use socat to connect to vsock:"
echo "     sudo socat - VSOCK-CONNECT:3:5555"
echo ""

echo "4. Running a single test with verbose output..."
echo ""

# Find cargo
CARGO="/home/$SUDO_USER/.cargo/bin/cargo"
if [ ! -x "$CARGO" ]; then
    CARGO="cargo"
fi

# Change to project directory
cd /home/$SUDO_USER/larpdevs/osvm-cli

echo "Running test_microvm_launch_and_init with debug output..."
echo "Command: RUST_LOG=debug $CARGO test --test microvm_integration_tests test_microvm_launch_and_init -- --ignored --nocapture"
echo ""

RUST_LOG=debug $CARGO test --test microvm_integration_tests test_microvm_launch_and_init -- --ignored --nocapture 2>&1 | tee /tmp/microvm-test-debug.log

echo ""
echo "=================================================="
echo "Debug Summary"
echo "=================================================="
echo ""
echo "Full debug log saved to: /tmp/microvm-test-debug.log"
echo ""
echo "Common issues and solutions:"
echo "  1. KVM not available: Enable virtualization in BIOS"
echo "  2. Vsock module not loaded: modprobe vhost_vsock"
echo "  3. Timeout errors: Check if init script is executable"
echo "  4. Permission denied: Ensure running as root"
echo ""
echo "To check Firecracker processes:"
echo "  ps aux | grep firecracker"
echo ""
echo "To check for socket files:"
echo "  ls -la /tmp/*.sock"
echo ""