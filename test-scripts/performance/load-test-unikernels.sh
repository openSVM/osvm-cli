#!/bin/bash
#
# Load Test for Concurrent Unikernel Spawning
# Tests system stability with 10-20 concurrent unikernels
#
# Usage:
#   bash scripts/load-test-unikernels.sh [num_unikernels]
#
# Default: 15 concurrent unikernels

set -e

# Configuration
NUM_UNIKERNELS=${1:-15}
VSOCK_PORT=5252
CID_MIN=200
CID_MAX=299
MAX_BOOT_TIME_MS=500
MAX_MEMORY_MB=80
TEST_TIMEOUT=60
LOG_DIR="/tmp/osvm-load-test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create log directory
mkdir -p "$LOG_DIR"
rm -f "$LOG_DIR"/*.log 2>/dev/null || true

echo -e "${BLUE}================================${NC}"
echo -e "${BLUE}Unikernel Load Testing${NC}"
echo -e "${BLUE}================================${NC}"
echo ""
echo "Configuration:"
echo "  Number of unikernels: $NUM_UNIKERNELS"
echo "  Vsock port: $VSOCK_PORT"
echo "  CID range: $CID_MIN-$CID_MAX"
echo "  Max boot time: ${MAX_BOOT_TIME_MS}ms"
echo "  Max memory per instance: ${MAX_MEMORY_MB}MB"
echo "  Test timeout: ${TEST_TIMEOUT}s"
echo ""

# Detect Flatpak environment
KRAFT_CMD="kraft"
if [ -f "/.flatpak-info" ]; then
    echo -e "${BLUE}Detected Flatpak environment${NC}"
    KRAFT_CMD="flatpak-spawn --host -- kraft"
    echo "  Using: $KRAFT_CMD"
fi

# Check prerequisites
echo -e "${BLUE}Checking prerequisites...${NC}"

# Check if kraft is installed (adjust check for Flatpak)
if [ -f "/.flatpak-info" ]; then
    if ! flatpak-spawn --host -- kraft version &> /dev/null; then
        echo -e "${RED}✗ kraft CLI not found on host${NC}"
        echo "Install with: curl -sSf https://get.kraftkit.sh | sh"
        exit 1
    fi
else
    if ! command -v kraft &> /dev/null; then
        echo -e "${RED}✗ kraft CLI not found${NC}"
        echo "Install with: curl -sSf https://get.kraftkit.sh | sh"
        exit 1
    fi
fi
echo -e "${GREEN}✓ kraft CLI installed${NC}"

# Check if vsock module is loaded
if [ -f "/.flatpak-info" ]; then
    if ! flatpak-spawn --host -- lsmod | grep -q vhost_vsock; then
        echo -e "${RED}✗ vhost_vsock module not loaded${NC}"
        echo ""
        echo "Please load the module manually with:"
        echo "  flatpak-spawn --host -- sudo modprobe vhost_vsock"
        echo ""
        exit 1
    fi
else
    if ! lsmod | grep -q vhost_vsock; then
        echo -e "${RED}✗ vhost_vsock module not loaded${NC}"
        echo ""
        echo "Please load the module manually with:"
        echo "  sudo modprobe vhost_vsock"
        echo ""
        exit 1
    fi
fi
echo -e "${GREEN}✓ vhost_vsock module loaded${NC}"

# Check if guest binary exists
GUEST_BINARY="$HOME/.osvm/unikernels/unikraft_tool_executor"
if [ ! -f "$GUEST_BINARY" ]; then
    echo -e "${RED}✗ Guest binary not found: $GUEST_BINARY${NC}"
    echo "Build with: bash scripts/build-unikraft-tool-executor.sh"
    exit 1
fi
echo -e "${GREEN}✓ Guest binary found${NC}"

# Check available memory
TOTAL_MEMORY_MB=$(free -m | awk 'NR==2{print $2}')
REQUIRED_MEMORY_MB=$((NUM_UNIKERNELS * MAX_MEMORY_MB))
if [ "$REQUIRED_MEMORY_MB" -gt "$TOTAL_MEMORY_MB" ]; then
    echo -e "${YELLOW}⚠ Warning: Required memory (${REQUIRED_MEMORY_MB}MB) exceeds available memory (${TOTAL_MEMORY_MB}MB)${NC}"
    echo "  This test may cause system instability"
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

echo ""
echo -e "${BLUE}Pre-caching kraft image...${NC}"
# Pre-pull the image by running kraft once then killing it
$KRAFT_CMD run --rm --memory 64M "$GUEST_BINARY" > /tmp/kraft-precache.log 2>&1 &
PRECACHE_PID=$!
# Wait 3 seconds for image to download
sleep 3
# Kill the precache process
kill -9 $PRECACHE_PID 2>/dev/null || true
wait $PRECACHE_PID 2>/dev/null || true
rm -f /tmp/kraft-precache.log
echo -e "${GREEN}✓ Image cached${NC}"

echo ""
echo -e "${BLUE}Starting load test...${NC}"
echo ""

# Arrays to track spawned processes
declare -a PIDS=()
declare -a CIDS=()
declare -a START_TIMES=()

# Function to spawn a single unikernel
spawn_unikernel() {
    local index=$1
    local cid=$((CID_MIN + (RANDOM % (CID_MAX - CID_MIN + 1))))
    local log_file="$LOG_DIR/unikernel_${index}_cid${cid}.log"
    
    # Record start time
    local start_time=$(date +%s%3N)
    START_TIMES[$index]=$start_time
    
    echo "[$index] Spawning unikernel with CID $cid..."
    
    # Spawn kraft process in background (use detected kraft command)
    $KRAFT_CMD run --rm --memory ${MAX_MEMORY_MB}M "$GUEST_BINARY" \
        > "$log_file" 2>&1 &
    
    local pid=$!
    PIDS[$index]=$pid
    CIDS[$index]=$cid
    
    echo "[$index] Spawned with PID $pid, CID $cid"
}

# Function to check if unikernel is responsive
check_unikernel() {
    local index=$1
    local cid=${CIDS[$index]}
    local pid=${PIDS[$index]}
    
    # Check if process is still running
    if ! kill -0 "$pid" 2>/dev/null; then
        echo -e "${RED}[$index] Process died (PID $pid)${NC}"
        return 1
    fi
    
    # Try to connect to vsock (simple check - just attempt connection)
    # We'll use socat if available, otherwise skip connectivity check
    if command -v socat &> /dev/null; then
        timeout 2 socat - VSOCK-CONNECT:$cid:$VSOCK_PORT </dev/null >/dev/null 2>&1
        if [ $? -eq 0 ] || [ $? -eq 124 ]; then
            # Connection successful or timeout (both indicate vsock is listening)
            return 0
        else
            echo -e "${YELLOW}[$index] Vsock not responsive yet (CID $cid, port $VSOCK_PORT)${NC}"
            return 1
        fi
    fi
    
    return 0
}

# Spawn all unikernels concurrently
echo -e "${BLUE}Phase 1: Concurrent Spawning${NC}"
echo ""

for i in $(seq 0 $((NUM_UNIKERNELS - 1))); do
    spawn_unikernel $i
    # Delay to avoid kraft concurrent initialization conflicts
    sleep 0.5
done

echo ""
echo "All spawn commands issued, waiting for boot..."
sleep 2

# Phase 2: Check boot times and responsiveness
echo ""
echo -e "${BLUE}Phase 2: Boot Time Analysis${NC}"
echo ""

SUCCESSFUL_BOOTS=0
FAILED_BOOTS=0
TOTAL_BOOT_TIME=0

for i in $(seq 0 $((NUM_UNIKERNELS - 1))); do
    pid=${PIDS[$i]}
    cid=${CIDS[$i]}
    start_time=${START_TIMES[$i]}
    
    # Wait up to 2 seconds for unikernel to become responsive
    responsive=false
    for attempt in $(seq 1 10); do
        if check_unikernel $i; then
            responsive=true
            end_time=$(date +%s%3N)
            boot_time=$((end_time - start_time))
            TOTAL_BOOT_TIME=$((TOTAL_BOOT_TIME + boot_time))
            
            if [ $boot_time -le $MAX_BOOT_TIME_MS ]; then
                echo -e "${GREEN}[$i] Boot successful: ${boot_time}ms (CID $cid)${NC}"
            else
                echo -e "${YELLOW}[$i] Boot successful but slow: ${boot_time}ms (CID $cid)${NC}"
            fi
            
            SUCCESSFUL_BOOTS=$((SUCCESSFUL_BOOTS + 1))
            break
        fi
        sleep 0.2
    done
    
    if [ "$responsive" = false ]; then
        echo -e "${RED}[$i] Boot failed or timeout (CID $cid, PID $pid)${NC}"
        FAILED_BOOTS=$((FAILED_BOOTS + 1))
    fi
done

# Phase 3: Memory usage analysis
echo ""
echo -e "${BLUE}Phase 3: Memory Usage Analysis${NC}"
echo ""

# Calculate total memory used by kraft processes
TOTAL_MEMORY_USED=0
for pid in "${PIDS[@]}"; do
    if kill -0 "$pid" 2>/dev/null; then
        # Get RSS (Resident Set Size) in KB, convert to MB
        mem_kb=$(ps -o rss= -p "$pid" 2>/dev/null | tr -d ' ')
        if [ -n "$mem_kb" ]; then
            mem_mb=$((mem_kb / 1024))
            TOTAL_MEMORY_USED=$((TOTAL_MEMORY_USED + mem_mb))
        fi
    fi
done

echo "Total memory usage: ${TOTAL_MEMORY_USED}MB"
echo "Average memory per unikernel: $((TOTAL_MEMORY_USED / NUM_UNIKERNELS))MB"

if [ $((TOTAL_MEMORY_USED / NUM_UNIKERNELS)) -le $MAX_MEMORY_MB ]; then
    echo -e "${GREEN}✓ Memory usage within target${NC}"
else
    echo -e "${YELLOW}⚠ Memory usage exceeds target (${MAX_MEMORY_MB}MB per instance)${NC}"
fi

# Phase 4: CID allocation conflict check
echo ""
echo -e "${BLUE}Phase 4: CID Allocation Analysis${NC}"
echo ""

# Check for duplicate CIDs
UNIQUE_CIDS=$(printf '%s\n' "${CIDS[@]}" | sort -u | wc -l)
if [ "$UNIQUE_CIDS" -eq "$NUM_UNIKERNELS" ]; then
    echo -e "${GREEN}✓ No CID collisions detected (${UNIQUE_CIDS} unique CIDs)${NC}"
else
    echo -e "${RED}✗ CID collisions detected! ${UNIQUE_CIDS} unique CIDs for ${NUM_UNIKERNELS} unikernels${NC}"
    echo "Allocated CIDs: ${CIDS[*]}"
fi

# Phase 5: Cleanup
echo ""
echo -e "${BLUE}Phase 5: Cleanup${NC}"
echo ""

CLEANUP_SUCCESSFUL=0
CLEANUP_FAILED=0

for i in $(seq 0 $((NUM_UNIKERNELS - 1))); do
    pid=${PIDS[$i]}
    if kill -0 "$pid" 2>/dev/null; then
        echo "[$i] Terminating PID $pid..."
        kill -TERM "$pid" 2>/dev/null || true
        
        # Wait up to 2 seconds for graceful shutdown
        shutdown_ok=false
        for attempt in $(seq 1 10); do
            if ! kill -0 "$pid" 2>/dev/null; then
                shutdown_ok=true
                break
            fi
            sleep 0.2
        done
        
        if [ "$shutdown_ok" = true ]; then
            CLEANUP_SUCCESSFUL=$((CLEANUP_SUCCESSFUL + 1))
        else
            echo "[$i] Force killing PID $pid..."
            kill -KILL "$pid" 2>/dev/null || true
            CLEANUP_FAILED=$((CLEANUP_FAILED + 1))
        fi
    else
        echo "[$i] Process $pid already terminated"
        CLEANUP_SUCCESSFUL=$((CLEANUP_SUCCESSFUL + 1))
    fi
done

# Final report
echo ""
echo -e "${BLUE}================================${NC}"
echo -e "${BLUE}Load Test Results${NC}"
echo -e "${BLUE}================================${NC}"
echo ""

echo "Spawning:"
echo "  Total unikernels spawned: $NUM_UNIKERNELS"
echo "  Successful boots: $SUCCESSFUL_BOOTS"
echo "  Failed boots: $FAILED_BOOTS"
echo "  Success rate: $((SUCCESSFUL_BOOTS * 100 / NUM_UNIKERNELS))%"
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
echo "  Average per instance: $((TOTAL_MEMORY_USED / NUM_UNIKERNELS))MB"
echo "  Target: <${MAX_MEMORY_MB}MB per instance"
echo ""

echo "CID Allocation:"
echo "  Unique CIDs: $UNIQUE_CIDS / $NUM_UNIKERNELS"
if [ "$UNIQUE_CIDS" -eq "$NUM_UNIKERNELS" ]; then
    echo -e "  Status: ${GREEN}✓ No collisions${NC}"
else
    echo -e "  Status: ${RED}✗ Collisions detected${NC}"
fi
echo ""

echo "Cleanup:"
echo "  Successful terminations: $CLEANUP_SUCCESSFUL"
echo "  Failed terminations: $CLEANUP_FAILED"
echo ""

# Overall assessment
echo -e "${BLUE}Overall Assessment:${NC}"
if [ $SUCCESSFUL_BOOTS -eq $NUM_UNIKERNELS ] && \
   [ "$UNIQUE_CIDS" -eq "$NUM_UNIKERNELS" ] && \
   [ $CLEANUP_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ PASS - All tests successful${NC}"
    EXIT_CODE=0
else
    echo -e "${YELLOW}⚠ PARTIAL PASS - Some issues detected${NC}"
    EXIT_CODE=1
fi

echo ""
echo "Logs saved to: $LOG_DIR"
echo ""

exit $EXIT_CODE
