#!/bin/bash

# Agave Validator + ngrok Tunnel Management Script
# Enhanced for Business plan features with HTTP header tracking

set -e

# Configuration
VALIDATOR_RPC_PORT=8899
VALIDATOR_WS_PORT=8900
NGROK_CONFIG="$HOME/.config/ngrok/ngrok.yml"
VALIDATOR_LOG="/tmp/agave-validator.log"
TUNNEL_LOG="/tmp/ngrok-tunnels.log"
TUNNEL_URLS_FILE="/tmp/ngrok-tunnel-urls.json"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check if Agave validator is running
check_validator() {
    if pgrep -f "agave-validator\|solana-test-validator" > /dev/null; then
        return 0
    else
        return 1
    fi
}

# Check if ngrok is running
check_ngrok() {
    if pgrep -f "ngrok" > /dev/null; then
        return 0
    else
        return 1
    fi
}

# Start Agave test validator with proper network binding
start_validator() {
    log "Starting Agave test validator..."
    
    if check_validator; then
        warning "Validator is already running"
        return 0
    fi
    
    # Start validator bound to all interfaces with custom gossip host
    nohup solana-test-validator \
        --rpc-port $VALIDATOR_RPC_PORT \
        --gossip-host osvm.dev \
        --gossip-port 8001 \
        --dynamic-port-range 8002-8020 \
        --log \
        --reset \
        > $VALIDATOR_LOG 2>&1 &
    
    local validator_pid=$!
    echo $validator_pid > /tmp/agave-validator.pid
    
    # Wait for validator to start
    log "Waiting for validator to start..."
    for i in {1..30}; do
        if curl -s -X POST -H "Content-Type: application/json" \
           -d '{"jsonrpc":"2.0","id":1,"method":"getVersion"}' \
           http://localhost:$VALIDATOR_RPC_PORT > /dev/null 2>&1; then
            success "Validator started successfully (PID: $validator_pid)"
            return 0
        fi
        sleep 2
    done
    
    error "Validator failed to start within 60 seconds"
    return 1
}

# Start all ngrok tunnels
start_tunnels() {
    log "Starting ngrok tunnels..."
    
    if check_ngrok; then
        warning "ngrok is already running"
        return 0
    fi
    
    # Start all tunnels defined in configuration
    nohup ngrok start --all --config="$NGROK_CONFIG" \
        > $TUNNEL_LOG 2>&1 &
    
    local ngrok_pid=$!
    echo $ngrok_pid > /tmp/ngrok.pid
    
    # Wait for tunnels to establish
    log "Waiting for tunnels to establish..."
    for i in {1..20}; do
        if curl -s http://localhost:4040/api/tunnels > /dev/null 2>&1; then
            success "ngrok tunnels started successfully (PID: $ngrok_pid)"
            get_tunnel_urls
            return 0
        fi
        sleep 3
    done
    
    error "ngrok tunnels failed to start within 60 seconds"
    return 1
}

# Get and display tunnel URLs
get_tunnel_urls() {
    log "Retrieving tunnel URLs..."
    
    if ! curl -s http://localhost:4040/api/tunnels > $TUNNEL_URLS_FILE 2>/dev/null; then
        error "Could not retrieve tunnel URLs"
        return 1
    fi
    
    echo ""
    echo -e "${GREEN}🌐 Public Tunnel Endpoints:${NC}"
    echo "=================================================="
    
    # Parse and display tunnel URLs
    if command -v jq > /dev/null 2>&1; then
        jq -r '.tunnels[] | "🔗 \(.name | ascii_upcase): \(.public_url)"' $TUNNEL_URLS_FILE
    else
        # Fallback without jq
        grep -o '"name":"[^"]*"' $TUNNEL_URLS_FILE | sed 's/"name":"//;s/"//' | while read name; do
            url=$(grep -A 5 "\"name\":\"$name\"" $TUNNEL_URLS_FILE | grep -o '"public_url":"[^"]*"' | sed 's/"public_url":"//;s/"//')
            echo "🔗 $(echo $name | tr '[:lower:]' '[:upper:]'): $url"
        done
    fi
    
    echo ""
    echo -e "${BLUE}📊 Enhanced Features Active:${NC}"
    echo "• HTTP request headers with client tracking"
    echo "• Geographic location logging"
    echo "• Custom CORS headers for web apps"
    echo "• Professional domain endpoints"
    echo ""
}

# Monitor tunnel traffic and headers
monitor_traffic() {
    log "Monitoring tunnel traffic (Press Ctrl+C to stop)..."
    
    if ! check_ngrok; then
        error "ngrok is not running"
        return 1
    fi
    
    echo -e "${YELLOW}Real-time Request Monitoring:${NC}"
    echo "============================================"
    
    while true; do
        if command -v jq > /dev/null 2>&1; then
            # Enhanced monitoring with jq
            curl -s http://localhost:4040/api/requests/http | \
                jq -r '.requests[-5:][] | "[\(.timestamp | strftime("%H:%M:%S"))] \(.method) \(.uri) from \(.request.headers["X-Client-Loc"] // "Unknown")"' 2>/dev/null || true
        else
            # Basic monitoring
            curl -s http://localhost:4040/api/requests/http | \
                grep -o '"uri":"[^"]*"' | head -5
        fi
        sleep 5
    done
}

# Stop validator
stop_validator() {
    log "Stopping Agave validator..."
    
    if [ -f /tmp/agave-validator.pid ]; then
        local pid=$(cat /tmp/agave-validator.pid)
        if kill $pid 2>/dev/null; then
            success "Validator stopped (PID: $pid)"
        fi
        rm -f /tmp/agave-validator.pid
    fi
    
    # Kill any remaining validator processes
    pkill -f "agave-validator\|solana-test-validator" 2>/dev/null || true
}

# Stop ngrok tunnels
stop_tunnels() {
    log "Stopping ngrok tunnels..."
    
    if [ -f /tmp/ngrok.pid ]; then
        local pid=$(cat /tmp/ngrok.pid)
        if kill $pid 2>/dev/null; then
            success "ngrok tunnels stopped (PID: $pid)"
        fi
        rm -f /tmp/ngrok.pid
    fi
    
    # Kill any remaining ngrok processes
    pkill -f "ngrok" 2>/dev/null || true
    rm -f $TUNNEL_URLS_FILE
}

# Show status
status() {
    echo -e "${BLUE}🔍 Agave Validator + ngrok Status${NC}"
    echo "================================="
    
    # Validator status
    if check_validator; then
        echo -e "Validator: ${GREEN}✅ Running${NC}"
        # Test RPC connection
        if curl -s -X POST -H "Content-Type: application/json" \
           -d '{"jsonrpc":"2.0","id":1,"method":"getVersion"}' \
           http://localhost:$VALIDATOR_RPC_PORT > /dev/null 2>&1; then
            echo -e "RPC: ${GREEN}✅ Accessible${NC}"
        else
            echo -e "RPC: ${RED}❌ Not responding${NC}"
        fi
    else
        echo -e "Validator: ${RED}❌ Not running${NC}"
    fi
    
    # ngrok status
    if check_ngrok; then
        echo -e "ngrok: ${GREEN}✅ Running${NC}"
        get_tunnel_urls 2>/dev/null || echo -e "Tunnels: ${YELLOW}⚠️ Establishing...${NC}"
    else
        echo -e "ngrok: ${RED}❌ Not running${NC}"
    fi
    
    echo ""
}

# Test tunnel connectivity
test_tunnels() {
    log "Testing tunnel connectivity..."
    
    if [ ! -f $TUNNEL_URLS_FILE ]; then
        error "No tunnel URLs found. Start tunnels first."
        return 1
    fi
    
    echo -e "${BLUE}Testing public endpoints:${NC}"
    
    # Test RPC endpoint
    local rpc_url=$(grep -A 3 '"name":"rpc"' $TUNNEL_URLS_FILE | grep -o '"public_url":"[^"]*"' | sed 's/"public_url":"//;s/"//' | head -1)
    if [ -n "$rpc_url" ]; then
        echo -n "Testing RPC ($rpc_url): "
        if curl -s -X POST -H "Content-Type: application/json" \
           -d '{"jsonrpc":"2.0","id":1,"method":"getVersion"}' \
           "$rpc_url" > /dev/null 2>&1; then
            echo -e "${GREEN}✅ OK${NC}"
        else
            echo -e "${RED}❌ Failed${NC}"
        fi
    fi
    
    # Test WebSocket endpoint  
    local ws_url=$(grep -A 3 '"name":"websocket"' $TUNNEL_URLS_FILE | grep -o '"public_url":"[^"]*"' | sed 's/"public_url":"//;s/"//' | head -1)
    if [ -n "$ws_url" ]; then
        echo -n "Testing WebSocket ($ws_url): "
        if curl -s -I "$ws_url" | grep -q "101\|200"; then
            echo -e "${GREEN}✅ OK${NC}"
        else
            echo -e "${RED}❌ Failed${NC}"
        fi
    fi
}

# Main command handler
case "${1:-help}" in
    start)
        log "Starting Agave validator with ngrok tunnels..."
        start_validator && start_tunnels
        ;;
    stop)
        log "Stopping Agave validator and ngrok tunnels..."
        stop_tunnels && stop_validator
        ;;
    restart)
        $0 stop
        sleep 2
        $0 start
        ;;
    start-validator)
        start_validator
        ;;
    start-tunnels)
        start_tunnels
        ;;
    stop-validator)
        stop_validator
        ;;
    stop-tunnels)
        stop_tunnels
        ;;
    status)
        status
        ;;
    urls)
        get_tunnel_urls
        ;;
    test)
        test_tunnels
        ;;
    monitor)
        monitor_traffic
        ;;
    logs)
        if [ "$2" = "validator" ]; then
            tail -f $VALIDATOR_LOG
        elif [ "$2" = "ngrok" ]; then
            tail -f $TUNNEL_LOG
        else
            echo "Usage: $0 logs [validator|ngrok]"
        fi
        ;;
    help|*)
        echo "Agave Validator + ngrok Tunnel Manager"
        echo ""
        echo "Usage: $0 <command>"
        echo ""
        echo "Commands:"
        echo "  start              Start validator and all tunnels"
        echo "  stop               Stop validator and all tunnels"
        echo "  restart            Restart everything"
        echo "  start-validator    Start only the validator"
        echo "  start-tunnels      Start only the tunnels"
        echo "  stop-validator     Stop only the validator"
        echo "  stop-tunnels       Stop only the tunnels"
        echo "  status             Show current status"
        echo "  urls               Display tunnel URLs"
        echo "  test               Test tunnel connectivity"
        echo "  monitor            Monitor real-time traffic"
        echo "  logs <type>        Show logs (validator|ngrok)"
        echo "  help               Show this help"
        ;;
esac
