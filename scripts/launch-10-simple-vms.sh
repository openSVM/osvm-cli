#!/bin/bash
# Launch 10 simple VMs with echo command as proof of concept

set -e

# Configuration
NUM_VMS=10
BASE_CID=500
MEM_PER_VM=512  # Need more memory for 199MB initrd
VCPUS_PER_VM=1
VM_COMMAND='echo "Hello from VM $(hostname)" && date +%s'

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

echo "=========================================="
echo "🚀 Launching 10 Simple MicroVMs"
echo "=========================================="
echo ""

# Check root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}Requires root privileges${NC}"
    exit 1
fi

# Quick checks
echo "Checking requirements..."
[ ! -e /dev/kvm ] && echo -e "${RED}KVM not available${NC}" && exit 1
[ ! -f /root/.osvm/kernel/vmlinux.bin ] && echo -e "${RED}Kernel not found${NC}" && exit 1

ROOTFS="/root/.osvm/rootfs/mcp-server.cpio"
[ ! -f "$ROOTFS" ] && ROOTFS="/root/.osvm/rootfs/mcp-server-rust.cpio"
[ ! -f "$ROOTFS" ] && echo -e "${RED}Rootfs not found${NC}" && exit 1

echo -e "${GREEN}✓ All requirements met${NC}"
echo ""

# Setup
WORK_DIR="/tmp/simple-vms-test"
rm -rf $WORK_DIR
mkdir -p $WORK_DIR/{configs,sockets,logs,pids}

# Create configs
echo "Creating VM configurations..."
for i in $(seq 0 $((NUM_VMS - 1))); do
    CID=$((BASE_CID + i))
    cat > $WORK_DIR/configs/vm-$i.json << EOF
{
  "boot-source": {
    "kernel_image_path": "/root/.osvm/kernel/vmlinux.bin",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init OSVM_MCP_SERVER_ID=vm-$i",
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
    "uds_path": "$WORK_DIR/sockets/vsock-$i.sock"
  }
}
EOF
done
echo -e "${GREEN}✓ Created $NUM_VMS configurations${NC}"
echo ""

# Launch VMs
echo "Launching VMs..."
LAUNCHED=0
FAILED=0

for i in $(seq 0 $((NUM_VMS - 1))); do
    API_SOCK="$WORK_DIR/sockets/api-$i.sock"
    CONFIG="$WORK_DIR/configs/vm-$i.json"
    LOG="$WORK_DIR/logs/vm-$i.log"
    PID_FILE="$WORK_DIR/pids/vm-$i.pid"

    # Launch
    firecracker --api-sock $API_SOCK --config-file $CONFIG > $LOG 2>&1 &
    PID=$!
    echo $PID > $PID_FILE

    # Check if started
    sleep 0.2
    if kill -0 $PID 2>/dev/null; then
        echo -e "  VM $i: ${GREEN}✓ Launched${NC} (PID: $PID, CID: $((BASE_CID + i)))"
        ((LAUNCHED++))
    else
        echo -e "  VM $i: ${RED}✗ Failed${NC}"
        if [ -f $LOG ]; then
            echo "    Error: $(tail -1 $LOG)"
        fi
        ((FAILED++))
    fi
done

echo ""
echo "=========================================="
echo "Results:"
echo "  ${GREEN}✓ Launched: $LAUNCHED VMs${NC}"
if [ $FAILED -gt 0 ]; then
    echo "  ${RED}✗ Failed: $FAILED VMs${NC}"
fi
echo ""

if [ $LAUNCHED -gt 0 ]; then
    echo "VMs are running. Checking status..."
    sleep 2

    # Count still running
    RUNNING=0
    for PID_FILE in $WORK_DIR/pids/*.pid; do
        if [ -f "$PID_FILE" ]; then
            PID=$(cat $PID_FILE)
            if kill -0 $PID 2>/dev/null; then
                ((RUNNING++))
            fi
        fi
    done
    echo "  Still running after 2 seconds: $RUNNING"
    echo ""

    # Show sample output
    echo "Sample VM output (vm-0):"
    echo "------------------------"
    if [ -f $WORK_DIR/logs/vm-0.log ]; then
        head -20 $WORK_DIR/logs/vm-0.log
    fi
    echo "------------------------"
    echo ""

    echo "Press ENTER to shutdown all VMs..."
    read

    # Cleanup
    echo "Shutting down..."
    for PID_FILE in $WORK_DIR/pids/*.pid; do
        if [ -f "$PID_FILE" ]; then
            PID=$(cat $PID_FILE)
            kill -TERM $PID 2>/dev/null || true
        fi
    done
    sleep 1
    killall firecracker 2>/dev/null || true
fi

echo -e "${GREEN}✓ Complete${NC}"
echo ""
echo "Logs are in: $WORK_DIR/logs/"
echo ""