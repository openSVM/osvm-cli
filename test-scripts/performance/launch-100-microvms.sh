#!/bin/bash
# Launch 100 concurrent microVMs for stress testing
# This demonstrates the scalability of the MCP server isolation infrastructure

set -e

# Configuration
NUM_VMS=100
BASE_CID=100  # Starting CID for VMs (100-199)
BASE_PORT=6000  # Starting port for API sockets
MEM_PER_VM=256  # MB per VM (256MB * 100 = 25.6GB total) - Need more for cargo
VCPUS_PER_VM=1
PARALLEL_LAUNCHES=10  # Launch 10 VMs at a time
# Command to run in each VM
VM_COMMAND='cargo install osvm && osvm whats current unix timestamp'

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=================================================="
echo "🚀 100 Concurrent MicroVMs Launcher"
echo "=================================================="
echo -e "${NC}"

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}❌ This script requires root privileges for Firecracker${NC}"
    echo "Please run with: sudo $0"
    exit 1
fi

# System checks
echo -e "${YELLOW}[1/7] System Requirements Check${NC}"
echo "================================"

# Check available memory
TOTAL_MEM=$(free -m | awk '/^Mem:/{print $2}')
REQUIRED_MEM=$((MEM_PER_VM * NUM_VMS))
echo -n "Memory check: "
if [ $TOTAL_MEM -lt $REQUIRED_MEM ]; then
    echo -e "${RED}❌ Insufficient memory. Have ${TOTAL_MEM}MB, need ${REQUIRED_MEM}MB${NC}"
    echo "Reduce NUM_VMS or MEM_PER_VM"
    exit 1
else
    echo -e "${GREEN}✓ ${TOTAL_MEM}MB available (need ${REQUIRED_MEM}MB)${NC}"
fi

# Check KVM
echo -n "KVM support: "
if [ ! -e /dev/kvm ]; then
    echo -e "${RED}❌ /dev/kvm not found${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Available${NC}"
fi

# Check Firecracker
echo -n "Firecracker: "
if ! command -v firecracker &> /dev/null; then
    echo -e "${RED}❌ Not found${NC}"
    exit 1
else
    echo -e "${GREEN}✓ $(firecracker --version 2>&1 | head -1)${NC}"
fi

# Check files - Use Rust-enabled rootfs if available
ROOTFS_RUST="/root/.osvm/rootfs/mcp-server-rust.cpio"
ROOTFS_BASIC="/root/.osvm/rootfs/mcp-server.cpio"
KERNEL="/root/.osvm/kernel/vmlinux.bin"

# Prefer Rust rootfs for cargo install command
if [ -f "$ROOTFS_RUST" ]; then
    ROOTFS="$ROOTFS_RUST"
    echo "Using Rust-enabled rootfs"
elif [ -f "$ROOTFS_BASIC" ]; then
    ROOTFS="$ROOTFS_BASIC"
    echo "⚠ Using basic rootfs (cargo install may fail)"
else
    ROOTFS=""
fi

echo -n "Rootfs: "
if [ ! -f "$ROOTFS" ]; then
    echo -e "${RED}❌ Not found at $ROOTFS${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Found ($(du -h $ROOTFS | cut -f1))${NC}"
fi

echo -n "Kernel: "
if [ ! -f "$KERNEL" ]; then
    echo -e "${RED}❌ Not found at $KERNEL${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Found ($(du -h $KERNEL | cut -f1))${NC}"
fi

# Load vsock module
echo -n "Vsock module: "
if ! lsmod | grep -q vsock; then
    modprobe vhost_vsock 2>/dev/null || true
fi
if lsmod | grep -q vsock; then
    echo -e "${GREEN}✓ Loaded${NC}"
else
    echo -e "${YELLOW}⚠ Not loaded (VMs may fail)${NC}"
fi
echo ""

# Create directories
echo -e "${YELLOW}[2/7] Creating Working Directories${NC}"
echo "===================================="
WORK_DIR="/tmp/microvm-stress-test"
rm -rf $WORK_DIR
mkdir -p $WORK_DIR/{configs,sockets,logs,pids}
echo -e "${GREEN}✓ Created $WORK_DIR${NC}"
echo ""

# Function to create Firecracker config
create_vm_config() {
    local VM_ID=$1
    local CID=$((BASE_CID + VM_ID))
    local API_SOCK="$WORK_DIR/sockets/vm-${VM_ID}.sock"
    local VSOCK_PATH="$WORK_DIR/sockets/vsock-${VM_ID}.sock"
    local CONFIG_FILE="$WORK_DIR/configs/vm-${VM_ID}.json"

    cat > $CONFIG_FILE << EOF
{
  "boot-source": {
    "kernel_image_path": "$KERNEL",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off init=/init quiet OSVM_MCP_SERVER_CMD='$VM_COMMAND' OSVM_MCP_SERVER_ID=vm-$VM_ID",
    "initrd_path": "$ROOTFS"
  },
  "drives": [],
  "machine-config": {
    "vcpu_count": $VCPUS_PER_VM,
    "mem_size_mib": $MEM_PER_VM,
    "smt": false,
    "track_dirty_pages": false
  },
  "vsock": {
    "guest_cid": $CID,
    "uds_path": "$VSOCK_PATH"
  }
}
EOF
}

# Function to launch a single VM
launch_vm() {
    local VM_ID=$1
    local API_SOCK="$WORK_DIR/sockets/vm-${VM_ID}.sock"
    local CONFIG_FILE="$WORK_DIR/configs/vm-${VM_ID}.json"
    local PID_FILE="$WORK_DIR/pids/vm-${VM_ID}.pid"
    local LOG_FILE="$WORK_DIR/logs/vm-${VM_ID}-console.log"

    # Launch Firecracker in background
    firecracker --api-sock $API_SOCK --config-file $CONFIG_FILE > $LOG_FILE 2>&1 &
    local PID=$!
    echo $PID > $PID_FILE

    # Quick check if it started
    sleep 0.1
    if kill -0 $PID 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Generate all configs
echo -e "${YELLOW}[3/7] Generating VM Configurations${NC}"
echo "===================================="
for i in $(seq 0 $((NUM_VMS - 1))); do
    create_vm_config $i
    if [ $((i % 10)) -eq 0 ]; then
        echo -ne "\rGenerating configs: $((i + 1))/$NUM_VMS"
    fi
done
echo -e "\n${GREEN}✓ Generated $NUM_VMS configurations${NC}"
echo ""

# Launch VMs in parallel batches
echo -e "${YELLOW}[4/7] Launching $NUM_VMS MicroVMs${NC}"
echo "===================================="
START_TIME=$(date +%s)
LAUNCHED=0
FAILED=0

for batch_start in $(seq 0 $PARALLEL_LAUNCHES $((NUM_VMS - 1))); do
    batch_end=$((batch_start + PARALLEL_LAUNCHES - 1))
    if [ $batch_end -ge $NUM_VMS ]; then
        batch_end=$((NUM_VMS - 1))
    fi

    echo -ne "\rLaunching batch: VMs $batch_start-$batch_end... "

    # Launch batch in parallel
    for i in $(seq $batch_start $batch_end); do
        if [ $i -lt $NUM_VMS ]; then
            launch_vm $i &
        fi
    done

    # Wait for batch to complete
    wait

    # Count successes
    for i in $(seq $batch_start $batch_end); do
        if [ $i -lt $NUM_VMS ]; then
            PID_FILE="$WORK_DIR/pids/vm-${i}.pid"
            if [ -f $PID_FILE ]; then
                PID=$(cat $PID_FILE)
                if kill -0 $PID 2>/dev/null; then
                    ((LAUNCHED++))
                else
                    ((FAILED++))
                fi
            else
                ((FAILED++))
            fi
        fi
    done

    echo -e "${GREEN}✓${NC} (Launched: $LAUNCHED, Failed: $FAILED)"
done

END_TIME=$(date +%s)
LAUNCH_TIME=$((END_TIME - START_TIME))

echo ""
echo -e "${GREEN}✓ Launch complete in ${LAUNCH_TIME} seconds${NC}"
echo "  Successfully launched: $LAUNCHED"
echo "  Failed to launch: $FAILED"
echo ""

# Monitor resource usage
echo -e "${YELLOW}[5/7] Resource Usage Monitoring${NC}"
echo "================================="

# Memory usage
MEM_USED=$(free -m | awk '/^Mem:/{print $3}')
echo "Memory usage: ${MEM_USED}MB"

# CPU usage
CPU_LOAD=$(uptime | awk -F'load average:' '{print $2}')
echo "Load average:$CPU_LOAD"

# Process count
FC_PROCS=$(pgrep -c firecracker || echo "0")
echo "Firecracker processes: $FC_PROCS"

# Vsock connections
VSOCK_COUNT=$(ls -1 $WORK_DIR/sockets/vsock-*.sock 2>/dev/null | wc -l)
echo "Vsock sockets created: $VSOCK_COUNT"
echo ""

# Performance statistics
echo -e "${YELLOW}[6/7] Performance Statistics${NC}"
echo "=============================="
if [ $LAUNCHED -gt 0 ]; then
    AVG_LAUNCH_TIME=$(echo "scale=2; $LAUNCH_TIME * 1000 / $LAUNCHED" | bc)
    echo "Average launch time: ${AVG_LAUNCH_TIME}ms per VM"
    echo "Total VMs launched: $LAUNCHED"
    echo "Memory per VM: ${MEM_PER_VM}MB"
    echo "Total memory allocated: $((LAUNCHED * MEM_PER_VM))MB"
    echo "VMs per second: $(echo "scale=2; $LAUNCHED / $LAUNCH_TIME" | bc)"
else
    echo -e "${RED}No VMs successfully launched${NC}"
fi
echo ""

# Keep VMs running for observation
echo -e "${YELLOW}[7/7] VMs Running${NC}"
echo "==================="
echo -e "${GREEN}✅ $LAUNCHED microVMs are now running!${NC}"
echo ""
echo "Monitor with:"
echo "  watch 'ps aux | grep firecracker | wc -l'"
echo ""
echo "Check a specific VM log:"
echo "  tail -f $WORK_DIR/logs/vm-0-console.log"
echo ""
echo "Test vsock connection to VM 0 (CID 100):"
echo "  socat - VSOCK-CONNECT:100:5555"
echo ""
echo -e "${YELLOW}Press ENTER to shutdown all VMs and cleanup...${NC}"
read

# Cleanup
echo ""
echo -e "${YELLOW}Shutting down all VMs...${NC}"
for PID_FILE in $WORK_DIR/pids/*.pid; do
    if [ -f "$PID_FILE" ]; then
        PID=$(cat $PID_FILE)
        kill -TERM $PID 2>/dev/null || true
    fi
done

sleep 2

# Force kill any remaining
for PID_FILE in $WORK_DIR/pids/*.pid; do
    if [ -f "$PID_FILE" ]; then
        PID=$(cat $PID_FILE)
        kill -KILL $PID 2>/dev/null || true
    fi
done

echo -e "${GREEN}✓ All VMs terminated${NC}"

# Cleanup files
echo -n "Remove temporary files? (y/n): "
read CLEANUP
if [ "$CLEANUP" == "y" ]; then
    rm -rf $WORK_DIR
    echo -e "${GREEN}✓ Cleaned up $WORK_DIR${NC}"
else
    echo "Files preserved in $WORK_DIR"
fi

echo ""
echo -e "${BLUE}=================================================="
echo "🏁 Stress Test Complete!"
echo "=================================================="
echo -e "${NC}"
echo "Summary:"
echo "  • Attempted: $NUM_VMS VMs"
echo "  • Launched: $LAUNCHED VMs"
echo "  • Failed: $FAILED VMs"
echo "  • Launch time: ${LAUNCH_TIME}s"
if [ $LAUNCHED -gt 0 ]; then
    echo "  • Rate: $(echo "scale=1; $LAUNCHED / $LAUNCH_TIME" | bc) VMs/second"
fi
echo ""