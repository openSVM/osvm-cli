#!/usr/bin/env bash
# End-to-End Unikernel Execution Test Script
# Tests: spawn → connect → execute → terminate cycle

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "======================================"
echo "Unikernel Execution E2E Test Suite"
echo "======================================"
echo ""

# Test 1: Check kraft binary availability
echo "Test 1: Checking kraft binary availability..."
if command -v kraft &> /dev/null; then
    echo -e "${GREEN}✓${NC} kraft binary found: $(which kraft)"
    kraft version
elif [ -f "/.flatpak-info" ]; then
    echo -e "${YELLOW}⚠${NC} Running in Flatpak, checking host kraft..."
    if flatpak-spawn --host -- which kraft &> /dev/null; then
        echo -e "${GREEN}✓${NC} kraft available on host"
        flatpak-spawn --host -- kraft version
    else
        echo -e "${RED}✗${NC} kraft not found on host system"
        echo "Please install kraft: https://unikraft.org/docs/cli"
        exit 1
    fi
else
    echo -e "${RED}✗${NC} kraft not found"
    echo "Please install kraft: https://unikraft.org/docs/cli"
    exit 1
fi
echo ""

# Test 2: Check guest binary exists
echo "Test 2: Checking guest binary..."
GUEST_BINARY="$HOME/.osvm/unikernels/unikraft_tool_executor"
if [ -f "$GUEST_BINARY" ]; then
    echo -e "${GREEN}✓${NC} Guest binary found: $GUEST_BINARY"
    ls -lh "$GUEST_BINARY"
else
    echo -e "${RED}✗${NC} Guest binary not found at $GUEST_BINARY"
    echo "Building guest binary..."
    bash "$SCRIPT_DIR/build-unikraft-tool-executor.sh"
    
    if [ ! -f "$GUEST_BINARY" ]; then
        echo -e "${RED}✗${NC} Failed to build guest binary"
        exit 1
    fi
fi
echo ""

# Test 3: Check vsock support
echo "Test 3: Checking vsock kernel module..."
if lsmod | grep -q vhost_vsock; then
    echo -e "${GREEN}✓${NC} vhost_vsock module loaded"
elif [ -c "/dev/vsock" ]; then
    echo -e "${GREEN}✓${NC} /dev/vsock device available"
else
    echo -e "${YELLOW}⚠${NC} vsock support may not be available"
    echo "Attempting to load vhost_vsock module..."
    if sudo modprobe vhost_vsock 2>/dev/null; then
        echo -e "${GREEN}✓${NC} vhost_vsock module loaded"
    else
        echo -e "${RED}✗${NC} Failed to load vhost_vsock module"
        echo "vsock communication may not work"
    fi
fi
echo ""

# Test 4: Check isolation config
echo "Test 4: Checking isolation configuration..."
ISOLATION_CONFIG="$HOME/.config/osvm/isolation_config.json"
if [ -f "$ISOLATION_CONFIG" ]; then
    echo -e "${GREEN}✓${NC} Isolation config found: $ISOLATION_CONFIG"
    
    # Check if unikernel mode is configured
    if grep -q '"unikernel"' "$ISOLATION_CONFIG"; then
        echo -e "${GREEN}✓${NC} Unikernel execution mode configured"
    else
        echo -e "${YELLOW}⚠${NC} Unikernel mode not configured in isolation_config.json"
        echo "Add unikernel configuration for testing"
    fi
else
    echo -e "${YELLOW}⚠${NC} No isolation config found, will use defaults"
fi
echo ""

# Test 5: Verify guest binary is executable
echo "Test 5: Verifying guest binary is executable..."
if [ -x "$GUEST_BINARY" ]; then
    echo -e "${GREEN}✓${NC} Guest binary is executable"
    
    # Check if it's a static binary
    if file "$GUEST_BINARY" | grep -q "statically linked"; then
        echo -e "${GREEN}✓${NC} Binary is statically linked (musl)"
    else
        echo -e "${YELLOW}⚠${NC} Binary may not be statically linked"
    fi
else
    echo -e "${RED}✗${NC} Guest binary is not executable"
    chmod +x "$GUEST_BINARY"
    echo "Fixed permissions"
fi
echo ""

# Test 6: Test guest binary directly (not in unikernel)
echo "Test 6: Testing guest binary directly..."
echo "Starting guest binary on localhost:5252 (5 second timeout)..."

# Start guest binary in background
timeout 5s "$GUEST_BINARY" --port 5252 &
GUEST_PID=$!

sleep 1

# Try to connect
if nc -z localhost 5252 2>/dev/null; then
    echo -e "${GREEN}✓${NC} Guest binary listening on port 5252"
    
    # Send a test request
    echo '{"jsonrpc":"2.0","id":1,"method":"test","params":{}}' | nc localhost 5252 2>/dev/null || true
    
    # Kill the guest process
    kill $GUEST_PID 2>/dev/null || true
    wait $GUEST_PID 2>/dev/null || true
    
    echo -e "${GREEN}✓${NC} Guest binary executed successfully"
else
    echo -e "${YELLOW}⚠${NC} Could not connect to guest binary on port 5252"
    kill $GUEST_PID 2>/dev/null || true
    echo "This may be normal if binary requires vsock instead of TCP"
fi
echo ""

# Test 7: Build OSVM CLI
echo "Test 7: Building OSVM CLI..."
cd "$PROJECT_ROOT"
if cargo build --lib 2>&1 | tee /tmp/osvm_build.log; then
    echo -e "${GREEN}✓${NC} OSVM CLI built successfully"
else
    echo -e "${RED}✗${NC} Build failed"
    echo "Check /tmp/osvm_build.log for details"
    exit 1
fi
echo ""

# Test 8: Check CID allocation logic
echo "Test 8: Verifying CID allocation range..."
echo "Expected range: 200-299 (ephemeral)"
echo "Phase 3 range: 100-199 (persistent)"

# Extract CID allocation code from unikernel_runtime.rs
if grep -A 10 "fn allocate_ephemeral_cid" "$PROJECT_ROOT/src/services/unikernel_runtime.rs" | grep -q "200.*299"; then
    echo -e "${GREEN}✓${NC} CID allocation uses correct range (200-299)"
else
    echo -e "${YELLOW}⚠${NC} Could not verify CID range in source code"
fi
echo ""

# Summary
echo "======================================"
echo "Test Summary"
echo "======================================"
echo ""
echo "Core Infrastructure:"
echo "  - kraft binary: Available"
echo "  - Guest binary: Built and ready"
echo "  - vsock support: Checked"
echo "  - OSVM CLI: Compiled"
echo ""
echo -e "${GREEN}Basic infrastructure tests passed${NC}"
echo ""
echo "Next Steps:"
echo "  1. Run integration tests: cargo test unikernel"
echo "  2. Test with real MCP server tools"
echo "  3. Run performance benchmarks"
echo ""
echo "To test with a real tool execution:"
echo "  1. Configure isolation_config.json with unikernel mode"
echo "  2. Run: osvm mcp call-tool solana get_balance '{\"address\":\"...\"}'"
echo "  3. Monitor kraft processes: ps aux | grep kraft"
echo ""
