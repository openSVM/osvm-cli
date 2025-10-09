#!/bin/bash
# Simplified 100 VM launcher - focus on reliability

# Configuration
NUM_VMS=100
BASE_CID=100
MEM_PER_VM=256
VCPUS_PER_VM=1

echo "=========================================="
echo "🚀 Launching 100 MicroVMs (Simplified)"
echo "=========================================="
echo ""

# Check root
if [ "$EUID" -ne 0 ]; then
    echo "❌ Requires root"
    exit 1
fi

# Setup
WORK_DIR="/tmp/vm-simple-test"
rm -rf $WORK_DIR
mkdir -p $WORK_DIR

# Find files
KERNEL="/root/.osvm/kernel/vmlinux.bin"
[ ! -f "$KERNEL" ] && KERNEL="$HOME/.osvm/kernel/vmlinux.bin"

ROOTFS="/root/.osvm/rootfs/mcp-server.cpio"
[ ! -f "$ROOTFS" ] && ROOTFS="$HOME/.osvm/rootfs/mcp-server.cpio"

echo "Using:"
echo "  Kernel: $KERNEL"
echo "  Rootfs: $ROOTFS"
echo ""

# Launch VMs one by one
echo "Launching VMs..."
LAUNCHED=0
FAILED=0

for i in $(seq 0 $((NUM_VMS - 1))); do
    # Create config
    CID=$((BASE_CID + i))
    CONFIG="$WORK_DIR/vm-$i.json"

    cat > $CONFIG << EOF
{
  "boot-source": {
    "kernel_image_path": "$KERNEL",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init quiet",
    "initrd_path": "$ROOTFS"
  },
  "drives": [],
  "machine-config": {
    "vcpu_count": $VCPUS_PER_VM,
    "mem_size_mib": $MEM_PER_VM,
    "smt": false
  },
  "vsock": {
    "guest_cid": $CID,
    "uds_path": "$WORK_DIR/vsock-$i.sock"
  }
}
EOF

    # Launch VM
    API_SOCK="$WORK_DIR/api-$i.sock"
    LOG="$WORK_DIR/vm-$i.log"

    firecracker --api-sock $API_SOCK --config-file $CONFIG > $LOG 2>&1 &
    PID=$!

    # Quick check
    sleep 0.1

    if kill -0 $PID 2>/dev/null; then
        LAUNCHED=$((LAUNCHED + 1))
        echo "✓ VM $i launched (PID: $PID)"
    else
        FAILED=$((FAILED + 1))
        echo "✗ VM $i failed"
    fi

    # Progress every 10
    if [ $((i % 10)) -eq 9 ]; then
        echo "  Progress: $((i + 1))/$NUM_VMS"
    fi

    # Avoid overwhelming system
    if [ $((i % 5)) -eq 4 ]; then
        sleep 0.2
    fi
done

echo ""
echo "=========================================="
echo "Results:"
echo "  ✅ Launched: $LAUNCHED"
echo "  ❌ Failed: $FAILED"
echo ""

# Count running
sleep 1
RUNNING=$(ps aux | grep -c "[f]irecracker")
echo "  🏃 Currently running: $RUNNING"
echo ""

# Check first VM log
if [ -f "$WORK_DIR/vm-0.log" ]; then
    echo "First VM status:"
    tail -2 "$WORK_DIR/vm-0.log"
    echo ""
fi

echo "Commands:"
echo "  View processes: ps aux | grep firecracker"
echo "  Kill all: sudo killall firecracker"
echo "  View logs: ls $WORK_DIR/*.log"
echo ""

if [ $LAUNCHED -eq $NUM_VMS ]; then
    echo "🎉 SUCCESS! All $NUM_VMS VMs launched!"
else
    echo "⚠️  Partial success: $LAUNCHED/$NUM_VMS"
fi
echo ""