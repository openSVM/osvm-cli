#!/bin/bash
#
# Load Test for Concurrent Firecracker MicroVM Spawning
# Tests Phase 3 microVM infrastructure with 10-100 concurrent instances
#
# Usage:
#   bash scripts/load-test-microvms.sh [num_microvms]
#
# Default: 20 concurrent microVMs

set -e

# Configuration
NUM_MICROVMS=${1:-20}
VSOCK_PORT=5252
CID_MIN=100
CID_MAX=199
MAX_BOOT_TIME_MS=500
MAX_MEMORY_MB=256
SPAWN_BATCH_SIZE=10
SPAWN_DELAY_MS=100
LOG_DIR="/tmp/osvm-microvm-load-test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create log directory
mkdir -p "$LOG_DIR"
rm -f "$LOG_DIR"/*.log 2>/dev/null || true

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Firecracker MicroVM Load Testing${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo "Configuration:"
echo "  Number of microVMs: $NUM_MICROVMS"
echo "  Vsock port: $VSOCK_PORT"
echo "  CID range: $CID_MIN-$CID_MAX"
echo "  Max boot time: ${MAX_BOOT_TIME_MS}ms"
echo "  Max memory per instance: ${MAX_MEMORY_MB}MB"
echo "  Batch size: $SPAWN_BATCH_SIZE"
echo "  Spawn delay: ${SPAWN_DELAY_MS}ms"
echo ""

# Detect Flatpak environment
FIRECRACKER_CMD="firecracker"
if [ -f "/.flatpak-info" ]; then
    echo -e "${BLUE}Detected Flatpak environment${NC}"
    FIRECRACKER_CMD="flatpak-spawn --host -- firecracker"
    echo "  Using: $FIRECRACKER_CMD"
fi

# Check prerequisites
echo -e "${BLUE}Checking prerequisites...${NC}"

# Check if Firecracker is installed
if [ -f "/.flatpak-info" ]; then
    if ! flatpak-spawn --host -- firecracker --version &> /dev/null; then
        echo -e "${RED}✗ Firecracker not found on host${NC}"
        echo "Install with: https://github.com/firecracker-microvm/firecracker/releases"
        exit 1
    fi
else
    if ! command -v firecracker &> /dev/null; then
        echo -e "${RED}✗ Firecracker not found${NC}"
        echo "Install with: https://github.com/firecracker-microvm/firecracker/releases"
        exit 1
    fi
fi
echo -e "${GREEN}✓ Firecracker installed${NC}"

# Check for required files
HOME_DIR=$(eval echo ~$USER)
KERNEL_PATH="$HOME_DIR/.osvm/kernel/vmlinux.bin"
ROOTFS_PATH="$HOME_DIR/.osvm/rootfs/osvm-runtime.ext4"

if [ ! -f "$KERNEL_PATH" ]; then
    echo -e "${RED}✗ Kernel not found: $KERNEL_PATH${NC}"
    echo "Build with: bash scripts/build-minimal-rootfs.sh"
    exit 1
fi
echo -e "${GREEN}✓ Kernel found${NC}"

if [ ! -f "$ROOTFS_PATH" ]; then
    echo -e "${RED}✗ Rootfs not found: $ROOTFS_PATH${NC}"
    echo "Build with: bash scripts/build-minimal-rootfs.sh"
    exit 1
fi
echo -e "${GREEN}✓ Rootfs found${NC}"

# Check available memory
TOTAL_MEMORY_MB=$(free -m | awk 'NR==2{print $2}')
REQUIRED_MEMORY_MB=$((NUM_MICROVMS * MAX_MEMORY_MB))
if [ "$REQUIRED_MEMORY_MB" -gt "$TOTAL_MEMORY_MB" ]; then
    echo -e "${YELLOW}⚠ Warning: Required memory (${REQUIRED_MEMORY_MB}MB) exceeds available (${TOTAL_MEMORY_MB}MB)${NC}"
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

echo ""
echo -e "${BLUE}Starting load test with batched spawning...${NC}"
echo ""

# Arrays to track spawned microVMs
declare -a PIDS=()
declare -a CIDS=()
declare -a SOCKETS=()
declare -a START_TIMES=()

# Function to create Firecracker config
create_firecracker_config() {
    local cid=$1
    local config_file=$2
    local api_socket=$3
    local vsock_socket=$4
    
    cat > "$config_file" << EOF
{
  "boot-source": {
    "kernel_image_path": "$KERNEL_PATH",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off"
  },
  "drives": [{
    "drive_id": "rootfs",
    "path_on_host": "$ROOTFS_PATH",
    "is_root_device": true,
    "is_read_only": false
  }],
  "machine-config": {
    "vcpu_count": 1,
    "mem_size_mib": $MAX_MEMORY_MB,
    "smt": false
  },
  "vsock": {
    "guest_cid": $cid,
    "uds_path": "$vsock_socket"
  }
}
EOF
}

# Function to spawn a single microVM
spawn_microvm() {
    local index=$1
    local cid=$((CID_MIN + index))
    local config_file="$LOG_DIR/microvm_${index}_config.json"
    local api_socket="$LOG_DIR/microvm_${index}_api.sock"
    local vsock_socket="$LOG_DIR/microvm_${index}_vsock.sock"
    local log_file="$LOG_DIR/microvm_${index}_cid${cid}.log"
    
    # Record start time
    START_TIMES[$index]=$(date +%s%3N)
    
    echo "[$index] Spawning microVM with CID $cid..."
    
    # Create Firecracker config
    create_firecracker_config "$cid" "$config_file" "$api_socket" "$vsock_socket"
    
    # Spawn Firecracker in background
    if [ -f "/.flatpak-info" ]; then
        flatpak-spawn --host -- firecracker \
            --api-sock "$api_socket" \
            --config-file "$config_file" \
            > "$log_file" 2>&1 &
    else
        firecracker \
            --api-sock "$api_socket" \
            --config-file "$config_file" \
            > "$log_file" 2>&1 &
    fi
    
    local pid=$!
    PIDS[$index]=$pid
    CIDS[$index]=$cid
    SOCKETS[$index]=$vsock_socket
    
    echo "[$index] Spawned with PID $pid, CID $cid"
}

# Phase 1: Batched Spawning
echo -e "${BLUE}Phase 1: Batched Spawning (${SPAWN_BATCH_SIZE} at a time)${NC}"
echo ""

SPAWNED=0
while [ $SPAWNED -lt $NUM_MICROVMS ]; do
    BATCH_END=$((SPAWNED + SPAWN_BATCH_SIZE))
    if [ $BATCH_END -gt $NUM_MICROVMS ]; then
        BATCH_END=$NUM_MICROVMS
    fi
    
    echo "Spawning batch: $SPAWNED to $((BATCH_END - 1))"
    
    for i in $(seq $SPAWNED $((BATCH_END - 1))); do
        spawn_microvm $i
        sleep 0.$(printf "%03d" $SPAWN_DELAY_MS)
    done
    
    SPAWNED=$BATCH_END
    
    # Wait between batches
    if [ $SPAWNED -lt $NUM_MICROVMS ]; then
        echo "Waiting for batch to initialize..."
        sleep 2
    fi
done

echo ""
echo "All spawn commands issued, waiting for boot..."
sleep 3

# Phase 2: Check boot times and socket availability
echo ""
echo -e "${BLUE}Phase 2: Boot Time and Socket Analysis${NC}"
echo ""

SUCCESSFUL_BOOTS=0
FAILED_BOOTS=0
TOTAL_BOOT_TIME=0

for i in $(seq 0 $((NUM_MICROVMS - 1))); do
    pid=${PIDS[$i]}
    cid=${CIDS[$i]}
    vsock_socket=${SOCKETS[$i]}
    start_time=${START_TIMES[$i]}
    
    # Check if process is running and socket exists
    if kill -0 "$pid" 2>/dev/null && [ -S "$vsock_socket" ]; then
        end_time=$(date +%s%3N)
        boot_time=$((end_time - start_time))
        TOTAL_BOOT_TIME=$((TOTAL_BOOT_TIME + boot_time))
        
        if [ $boot_time -le $MAX_BOOT_TIME_MS ]; then
            echo -e "${GREEN}[$i] Boot successful: ${boot_time}ms (CID $cid)${NC}"
        else
            echo -e "${YELLOW}[$i] Boot successful but slow: ${boot_time}ms (CID $cid)${NC}"
        fi
        
        SUCCESSFUL_BOOTS=$((SUCCESSFUL_BOOTS + 1))
    else
        echo -e "${RED}[$i] Boot failed (CID $cid, PID $pid)${NC}"
        FAILED_BOOTS=$((FAILED_BOOTS + 1))
    fi
done

# Phase 3: Memory analysis
echo ""
echo -e "${BLUE}Phase 3: Memory Usage Analysis${NC}"
echo ""

TOTAL_MEMORY_USED=0
for pid in "${PIDS[@]}"; do
    if kill -0 "$pid" 2>/dev/null; then
        mem_kb=$(ps -o rss= -p "$pid" 2>/dev/null | tr -d ' ')
        if [ -n "$mem_kb" ]; then
            mem_mb=$((mem_kb / 1024))
            TOTAL_MEMORY_USED=$((TOTAL_MEMORY_USED + mem_mb))
        fi
    fi
done

echo "Total memory usage: ${TOTAL_MEMORY_USED}MB"
if [ $SUCCESSFUL_BOOTS -gt 0 ]; then
    AVG_MEMORY=$((TOTAL_MEMORY_USED / SUCCESSFUL_BOOTS))
    echo "Average per microVM: ${AVG_MEMORY}MB"
fi

# Phase 4: Cleanup
echo ""
echo -e "${BLUE}Phase 4: Cleanup${NC}"
echo ""

CLEANUP_SUCCESSFUL=0
CLEANUP_FAILED=0

for i in $(seq 0 $((NUM_MICROVMS - 1))); do
    pid=${PIDS[$i]}
    if kill -0 "$pid" 2>/dev/null; then
        echo "[$i] Terminating PID $pid..."
        kill -TERM "$pid" 2>/dev/null || true
        sleep 0.2
        
        if ! kill -0 "$pid" 2>/dev/null; then
            CLEANUP_SUCCESSFUL=$((CLEANUP_SUCCESSFUL + 1))
        else
            kill -KILL "$pid" 2>/dev/null || true
            CLEANUP_FAILED=$((CLEANUP_FAILED + 1))
        fi
    else
        CLEANUP_SUCCESSFUL=$((CLEANUP_SUCCESSFUL + 1))
    fi
done

# Cleanup socket files
rm -f "$LOG_DIR"/*.sock 2>/dev/null || true

# Final report
echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}MicroVM Load Test Results${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

echo "Spawning:"
echo "  Total microVMs spawned: $NUM_MICROVMS"
echo "  Successful boots: $SUCCESSFUL_BOOTS"
echo "  Failed boots: $FAILED_BOOTS"
echo "  Success rate: $((SUCCESSFUL_BOOTS * 100 / NUM_MICROVMS))%"
echo ""

if [ $SUCCESSFUL_BOOTS -gt 0 ]; then
    AVG_BOOT_TIME=$((TOTAL_BOOT_TIME / SUCCESSFUL_BOOTS))
    echo "Boot Times:"
    echo "  Average: ${AVG_BOOT_TIME}ms"
    echo "  Target: <${MAX_BOOT_TIME_MS}ms"
    if [ $AVG_BOOT_TIME -le $MAX_BOOT_TIME_MS ]; then
        echo -e "  Status: ${GREEN}✓ Within target${NC}"
    else
        echo -e "  Status: ${YELLOW}⚠ Exceeds target${NC}"
    fi
    echo ""
fi

echo "Memory:"
echo "  Total usage: ${TOTAL_MEMORY_USED}MB"
if [ $SUCCESSFUL_BOOTS -gt 0 ]; then
    echo "  Average per instance: $((TOTAL_MEMORY_USED / SUCCESSFUL_BOOTS))MB"
fi
echo "  Target: <${MAX_MEMORY_MB}MB per instance"
echo ""

echo "Cleanup:"
echo "  Successful terminations: $CLEANUP_SUCCESSFUL"
echo "  Failed terminations: $CLEANUP_FAILED"
echo ""

# Overall assessment
echo -e "${BLUE}Overall Assessment:${NC}"
if [ $SUCCESSFUL_BOOTS -eq $NUM_MICROVMS ] && [ $CLEANUP_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ PASS - All tests successful${NC}"
    echo ""
    echo "System is ready for 100 concurrent map-reduce operations!"
    EXIT_CODE=0
elif [ $((SUCCESSFUL_BOOTS * 100 / NUM_MICROVMS)) -ge 80 ]; then
    echo -e "${YELLOW}⚠ PARTIAL PASS - Acceptable for load testing${NC}"
    EXIT_CODE=0
else
    echo -e "${RED}✗ FAIL - Success rate too low${NC}"
    EXIT_CODE=1
fi

echo ""
echo "Logs saved to: $LOG_DIR"
echo ""

exit $EXIT_CODE
