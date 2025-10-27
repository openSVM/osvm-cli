#!/bin/bash
set -e

echo "🔧 Fixing Firecracker Networking Setup..."
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Step 1: Update sudoers to include setcap, getcap, and firecracker
echo -e "${YELLOW}Step 1: Updating sudoers for passwordless network commands...${NC}"

# Find firecracker binary
FIRECRACKER_PATH=$(which firecracker 2>/dev/null || echo "/home/$USER/.osvm/bin/firecracker")
if [ ! -f "$FIRECRACKER_PATH" ]; then
    echo -e "${RED}✗ Firecracker binary not found${NC}"
    exit 1
fi

echo "$USER ALL=(ALL) NOPASSWD: /usr/bin/ip, /usr/sbin/sysctl, /usr/sbin/iptables, /usr/sbin/setcap, /usr/sbin/getcap, $FIRECRACKER_PATH" | sudo tee /etc/sudoers.d/osvm-network > /dev/null
sudo chmod 0440 /etc/sudoers.d/osvm-network
echo -e "${GREEN}✓ Sudoers updated (firecracker: $FIRECRACKER_PATH)${NC}"
echo ""

# Step 2: Grant CAP_NET_ADMIN capability to Firecracker
echo -e "${YELLOW}Step 2: Granting CAP_NET_ADMIN capability to Firecracker...${NC}"
FIRECRACKER_PATH=$(which firecracker)
if [ -z "$FIRECRACKER_PATH" ]; then
    echo -e "${RED}✗ Firecracker binary not found in PATH${NC}"
    exit 1
fi
echo "Firecracker binary: $FIRECRACKER_PATH"
sudo setcap cap_net_admin+ep "$FIRECRACKER_PATH"
echo -e "${GREEN}✓ Capability granted${NC}"
echo ""

# Step 3: Verify the capability was granted
echo -e "${YELLOW}Step 3: Verifying capabilities...${NC}"
CAPS=$(getcap "$FIRECRACKER_PATH")
if [[ "$CAPS" == *"cap_net_admin"* ]]; then
    echo -e "${GREEN}✓ Capabilities verified: $CAPS${NC}"
else
    echo -e "${RED}✗ Failed to verify capabilities: $CAPS${NC}"
    exit 1
fi
echo ""

# Step 4: Clean up old TAP devices
echo -e "${YELLOW}Step 4: Cleaning up old TAP devices...${NC}"
for tap in osvm-tap3 osvm-tap-test osvm-tap4; do
    if ip link show "$tap" &>/dev/null; then
        sudo ip link delete "$tap" 2>/dev/null || true
        echo "  Deleted: $tap"
    fi
done
echo -e "${GREEN}✓ TAP devices cleaned up${NC}"
echo ""

# Step 5: Verify binary exists
echo -e "${YELLOW}Step 5: Checking osvm binary...${NC}"
if [ ! -f "./target/release/osvm" ]; then
    echo -e "${RED}✗ Binary not found. Building...${NC}"
    cargo build --release
fi
echo -e "${GREEN}✓ Binary ready${NC}"
echo ""

# Step 6: Test the fix
echo -e "${YELLOW}Step 6: Testing MCP call with ephemeral MicroVM...${NC}"
echo "This will launch a Firecracker VM and test network connectivity..."
echo ""

timeout 60 ./target/release/osvm --debug mcp call osvm-mcp get_balance --args '{"address":"11111111111111111111111111111111"}' 2>&1 | tee /tmp/osvm-test.log

# Check if it succeeded
if grep -q "Failed to call tool" /tmp/osvm-test.log; then
    echo ""
    echo -e "${RED}✗ Test failed. Checking diagnostics...${NC}"
    echo ""
    echo "TAP device status:"
    ip link show | grep osvm-tap || echo "No osvm-tap devices found"
    echo ""
    echo "Testing ping to guest VM (172.16.0.2):"
    ping -c 2 -W 1 172.16.0.2 || echo "Ping failed"
    exit 1
else
    echo ""
    echo -e "${GREEN}✓ Test completed!${NC}"
    echo ""
    echo "Checking final TAP device status:"
    ip link show | grep osvm-tap || echo "No osvm-tap devices found"
fi
