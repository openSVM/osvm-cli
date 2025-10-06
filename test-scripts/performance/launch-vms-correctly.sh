#!/bin/bash
# Launch VMs with correct paths and memory settings

set -e

# Configuration
NUM_VMS="${1:-10}"  # Default to 10, can override
BASE_CID=500
VCPUS_PER_VM=1

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "🚀 Launching $NUM_VMS MicroVMs (Correct Config)"
echo "=========================================="
echo ""

# Check root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}Requires root privileges${NC}"
    exit 1
fi

# Find the correct paths
KERNEL=""
ROOTFS=""

# Check kernel location
for path in "/root/.osvm/kernel/vmlinux.bin" "/home/$SUDO_USER/.osvm/kernel/vmlinux.bin" "$HOME/.osvm/kernel/vmlinux.bin"; do
    if [ -f "$path" ]; then
        KERNEL="$path"
        break
    fi
done

if [ -z "$KERNEL" ]; then
    echo -e "${RED}Kernel not found!${NC}"
    exit 1
fi

# Find best rootfs (prefer smaller one for speed)
for path in \
    "/home/$SUDO_USER/.osvm/rootfs/osvm-runtime.cpio" \
    "$HOME/.osvm/rootfs/osvm-runtime.cpio" \
    "/home/$SUDO_USER/.osvm/rootfs/mcp-server.cpio" \
    "$HOME/.osvm/rootfs/mcp-server.cpio" \
    "/home/$SUDO_USER/.osvm/rootfs/mcp-server-rust.cpio" \
    "$HOME/.osvm/rootfs/mcp-server-rust.cpio"; do
    if [ -f "$path" ]; then
        ROOTFS="$path"
        break  # Use first (smallest) found
    fi
done

if [ -z "$ROOTFS" ]; then
    echo -e "${RED}Rootfs not found!${NC}"
    exit 1
fi

# Calculate memory based on rootfs size
ROOTFS_SIZE_MB=$(du -m "$ROOTFS" | cut -f1)
# Need at least 2x the rootfs size + 128MB overhead
MEM_PER_VM=$((ROOTFS_SIZE_MB * 2 + 128))
# Minimum 256MB
if [ $MEM_PER_VM -lt 256 ]; then
    MEM_PER_VM=256
fi

echo "Configuration:"
echo "  Kernel: $KERNEL"
echo "  Rootfs: $ROOTFS (${ROOTFS_SIZE_MB}MB)"
echo "  Memory per VM: ${MEM_PER_VM}MB"
echo "  VMs to launch: $NUM_VMS"
echo ""

# Check available memory
TOTAL_MEM=$(free -m | awk '/^Mem:/{print $2}')
REQUIRED_MEM=$((MEM_PER_VM * NUM_VMS))
echo -n "Memory check: "
if [ $TOTAL_MEM -lt $REQUIRED_MEM ]; then
    echo -e "${YELLOW}⚠ Low memory. Have ${TOTAL_MEM}MB, need ${REQUIRED_MEM}MB${NC}"
    echo "Reducing number of VMs or continue anyway? (y/n)"
    read CONTINUE
    if [ "$CONTINUE" != "y" ]; then
        exit 1
    fi
else
    echo -e "${GREEN}✓ ${TOTAL_MEM}MB available (need ${REQUIRED_MEM}MB)${NC}"
fi
echo ""

# Setup
WORK_DIR="/tmp/correct-vms-test"
rm -rf $WORK_DIR
mkdir -p $WORK_DIR/{configs,sockets,logs,pids}

# Create configs
echo "Creating VM configurations..."
for i in $(seq 0 $((NUM_VMS - 1))); do
    CID=$((BASE_CID + i))
    cat > $WORK_DIR/configs/vm-$i.json << EOF
{
  "boot-source": {
    "kernel_image_path": "$KERNEL",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init",
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
    timeout 2 firecracker --api-sock $API_SOCK --config-file $CONFIG > $LOG 2>&1 &
    PID=$!
    echo $PID > $PID_FILE

    # Check if started
    sleep 0.3
    if kill -0 $PID 2>/dev/null; then
        echo -e "  VM $i: ${GREEN}✓ Launched${NC} (PID: $PID, CID: $((BASE_CID + i)))"
        ((LAUNCHED++))
    else
        echo -e "  VM $i: ${RED}✗ Failed${NC}"
        if [ -f $LOG ] && [ -s $LOG ]; then
            ERROR=$(grep -i error $LOG | head -1)
            [ -n "$ERROR" ] && echo "    Error: $ERROR"
        fi
        ((FAILED++))
    fi

    # Limit parallel launches to avoid overwhelming system
    if [ $((i % 5)) -eq 4 ]; then
        sleep 0.5
    fi
done

echo ""
echo "=========================================="
echo "Results:"
echo -e "  ${GREEN}✓ Launched: $LAUNCHED VMs${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "  ${RED}✗ Failed: $FAILED VMs${NC}"
fi
echo ""

if [ $LAUNCHED -gt 0 ]; then
    echo "Checking VM status..."
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

    # Show console output from first VM
    if [ -f $WORK_DIR/logs/vm-0.log ]; then
        echo "VM-0 Console Output (first 30 lines):"
        echo "======================================"
        head -30 $WORK_DIR/logs/vm-0.log
        echo "======================================"
        echo ""
    fi

    echo "Press ENTER to shutdown all VMs..."
    read

    # Cleanup
    echo "Shutting down VMs..."
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

# Summary
if [ $LAUNCHED -eq $NUM_VMS ]; then
    echo -e "${GREEN}🎉 SUCCESS! All $NUM_VMS VMs launched successfully!${NC}"
    echo ""
    echo "To run 100 VMs with cargo install osvm:"
    echo "  1. Build Rust rootfs: ./scripts/build-guest-rootfs-with-rust.sh"
    echo "  2. Run: sudo $0 100"
else
    echo -e "${YELLOW}⚠ Partial success. Check logs for issues.${NC}"
fi
echo ""