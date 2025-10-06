#!/bin/bash
# Working version of 100 VM launcher with proper progress tracking

set -e

# Configuration
NUM_VMS=100
BASE_CID=100
MEM_PER_VM=256
VCPUS_PER_VM=1
PARALLEL_LAUNCHES=10

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=================================================="
echo "🚀 100 Concurrent MicroVMs Launcher (Fixed)"
echo "=================================================="
echo -e "${NC}"

# Check if running as root
if [ "$EUID" -ne 0 ]; then
    echo -e "${RED}❌ Requires root privileges${NC}"
    exit 1
fi

# Find files
KERNEL="/root/.osvm/kernel/vmlinux.bin"
[ ! -f "$KERNEL" ] && KERNEL="$HOME/.osvm/kernel/vmlinux.bin"

ROOTFS="/root/.osvm/rootfs/mcp-server.cpio"
[ ! -f "$ROOTFS" ] && ROOTFS="$HOME/.osvm/rootfs/mcp-server.cpio"

if [ ! -f "$KERNEL" ] || [ ! -f "$ROOTFS" ]; then
    echo -e "${RED}❌ Missing kernel or rootfs${NC}"
    exit 1
fi

echo "Configuration:"
echo "  Kernel: $KERNEL"
echo "  Rootfs: $ROOTFS"
echo "  Memory per VM: ${MEM_PER_VM}MB"
echo "  VMs to launch: $NUM_VMS"
echo ""

# Setup directories
WORK_DIR="/tmp/microvm-100-test"
rm -rf $WORK_DIR
mkdir -p $WORK_DIR/{configs,sockets,logs,pids}

# Create all configs first
echo "Creating configurations..."
for i in $(seq 0 $((NUM_VMS - 1))); do
    CID=$((BASE_CID + i))
    cat > $WORK_DIR/configs/vm-$i.json << EOF
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
START_TIME=$(date +%s)

for batch_start in $(seq 0 $PARALLEL_LAUNCHES $((NUM_VMS - 1))); do
    batch_end=$((batch_start + PARALLEL_LAUNCHES - 1))
    [ $batch_end -ge $NUM_VMS ] && batch_end=$((NUM_VMS - 1))

    echo -ne "\rBatch $((batch_start / PARALLEL_LAUNCHES + 1)): Launching VMs $batch_start-$batch_end... "

    # Launch batch
    for i in $(seq $batch_start $batch_end); do
        [ $i -ge $NUM_VMS ] && break

        API_SOCK="$WORK_DIR/sockets/api-$i.sock"
        CONFIG="$WORK_DIR/configs/vm-$i.json"
        LOG="$WORK_DIR/logs/vm-$i.log"
        PID_FILE="$WORK_DIR/pids/vm-$i.pid"

        # Launch in background
        firecracker --api-sock $API_SOCK --config-file $CONFIG > $LOG 2>&1 &
        echo $! > $PID_FILE
    done

    # Wait a moment for batch to start
    sleep 0.5

    # Check batch results
    BATCH_SUCCESS=0
    for i in $(seq $batch_start $batch_end); do
        [ $i -ge $NUM_VMS ] && break

        PID_FILE="$WORK_DIR/pids/vm-$i.pid"
        if [ -f $PID_FILE ]; then
            PID=$(cat $PID_FILE)
            if kill -0 $PID 2>/dev/null; then
                ((LAUNCHED++))
                ((BATCH_SUCCESS++))
            else
                ((FAILED++))
            fi
        fi
    done

    echo -e "${GREEN}✓${NC} ($BATCH_SUCCESS succeeded)"
done

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""
echo ""
echo -e "${BLUE}=================================================="
echo "Launch Results"
echo "=================================================="
echo -e "${NC}"
echo -e "  ${GREEN}✅ Successfully launched: $LAUNCHED VMs${NC}"
if [ $FAILED -gt 0 ]; then
    echo -e "  ${RED}❌ Failed to launch: $FAILED VMs${NC}"
fi
echo "  ⏱️  Launch time: ${DURATION} seconds"
echo ""

# Check running status
echo "Verifying VMs are running..."
sleep 2
RUNNING=0
for PID_FILE in $WORK_DIR/pids/*.pid; do
    if [ -f "$PID_FILE" ]; then
        PID=$(cat $PID_FILE)
        if kill -0 $PID 2>/dev/null; then
            ((RUNNING++))
        fi
    fi
done

echo -e "  ${GREEN}🏃 Currently running: $RUNNING VMs${NC}"
echo ""

# Performance stats
if [ $LAUNCHED -gt 0 ]; then
    VMS_PER_SEC=$(echo "scale=2; $LAUNCHED / $DURATION" | bc 2>/dev/null || echo "N/A")
    TOTAL_MEM=$((LAUNCHED * MEM_PER_VM))

    echo "Performance Statistics:"
    echo "  • Launch rate: $VMS_PER_SEC VMs/second"
    echo "  • Total memory used: ${TOTAL_MEM}MB"
    echo "  • Average time per VM: $(echo "scale=0; $DURATION * 1000 / $LAUNCHED" | bc 2>/dev/null || echo "N/A")ms"
    echo ""
fi

# Show sample outputs
if [ $LAUNCHED -gt 0 ]; then
    echo "Sample VM Status (first 3 VMs):"
    echo "================================"
    for i in 0 1 2; do
        LOG="$WORK_DIR/logs/vm-$i.log"
        if [ -f "$LOG" ]; then
            STATUS=$(tail -1 "$LOG" | grep -o "Successfully started" || echo "Unknown")
            echo "  VM-$i: $STATUS"
        fi
    done
    echo ""
fi

# Monitor and control
echo -e "${YELLOW}Options:${NC}"
echo "  [s] Show VM statistics"
echo "  [k] Kill all VMs and exit"
echo "  [q] Quit (leave VMs running)"
echo ""
echo -n "Choice: "
read -n 1 CHOICE
echo ""

case $CHOICE in
    s|S)
        echo ""
        echo "Detailed Statistics:"
        echo "==================="
        ps aux | grep "[f]irecracker" | wc -l | xargs echo "Firecracker processes:"
        free -m | grep "^Mem:" | awk '{print "Memory used: " $3 "MB / " $2 "MB"}'
        echo "CPU Load: $(uptime | awk -F'load average:' '{print $2}')"
        echo ""
        ;;
    k|K)
        echo "Shutting down all VMs..."
        for PID_FILE in $WORK_DIR/pids/*.pid; do
            if [ -f "$PID_FILE" ]; then
                PID=$(cat $PID_FILE)
                kill -TERM $PID 2>/dev/null || true
            fi
        done
        sleep 1
        killall firecracker 2>/dev/null || true
        echo -e "${GREEN}✓ All VMs terminated${NC}"
        ;;
    *)
        echo "VMs left running. To kill later:"
        echo "  killall firecracker"
        ;;
esac

echo ""
if [ $LAUNCHED -eq $NUM_VMS ]; then
    echo -e "${GREEN}🎉 SUCCESS! All $NUM_VMS VMs launched!${NC}"
else
    echo -e "${YELLOW}⚠ Partial success: $LAUNCHED/$NUM_VMS VMs launched${NC}"
fi

echo ""
echo "Logs: $WORK_DIR/logs/"
echo ""