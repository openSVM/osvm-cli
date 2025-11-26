#!/bin/bash
#
# Agent Marketplace Economy Test
#
# Simulates an agent economy where:
# - Service providers advertise capabilities (OFFER)
# - Service consumers post requests (REQUEST)
# - Providers bid on requests (BID)
# - Consumers accept bids (ACCEPT)
# - Providers deliver results (DELIVER)
# - Both parties rate each other (RATE)
#

set -e

# Configuration
BBS_URL="${BBS_URL:-http://144.124.231.40:8080}"
BOARD="MARKETPLACE"
DURATION="${1:-60}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Stats
declare -A OFFERS
declare -A REQUESTS
declare -A BIDS
declare -A ACCEPTS
declare -A DELIVERIES
declare -A RATINGS

# Initialize stats
for agent in DataBot AuditBot TradeBot InvestorBot ProjectBot DevBot; do
    OFFERS[$agent]=0
    REQUESTS[$agent]=0
    BIDS[$agent]=0
    ACCEPTS[$agent]=0
    DELIVERIES[$agent]=0
    RATINGS[$agent]=0
done

TOTAL_TRANSACTIONS=0
TOTAL_VALUE=0

# Post a marketplace message
post_market() {
    local agent="$1"
    local msg="$2"

    curl -s -X POST "$BBS_URL/api/boards/$BOARD/posts" \
        -H "Content-Type: application/json" \
        -d "{\"message\":\"[$agent] $msg\",\"user_node_id\":\"!${agent,,}\"}" > /dev/null
}

# Get last message ID
get_last_id() {
    curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=1" | jq -r '.data[0].id // 0' 2>/dev/null || echo "0"
}

echo "========================================================================"
echo -e "${MAGENTA}AGENT MARKETPLACE ECONOMY TEST${NC}"
echo "========================================================================"
echo -e "BBS Server: ${YELLOW}$BBS_URL${NC}"
echo -e "Board: ${YELLOW}$BOARD${NC}"
echo -e "Duration: ${YELLOW}$DURATION seconds${NC}"
echo ""

# Check connectivity
echo -n "Checking BBS connectivity... "
if curl -s "$BBS_URL/api/stats" | jq -e '.success' > /dev/null 2>&1; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    exit 1
fi

INITIAL_POSTS=$(curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=1" | jq -r '.data | length // 0')
echo "Initial posts on MARKETPLACE: $INITIAL_POSTS"
echo ""

echo "------------------------------------------------------------------------"
echo -e "${CYAN}SERVICE PROVIDERS REGISTERING...${NC}"
echo "------------------------------------------------------------------------"

# DataBot offers whale tracking
post_market "DataBot" 'OFFER: {"type":"OFFER","service":"whale-tracking","description":"Real-time tracking of whale wallets and large transfers","capabilities":["wallet-monitoring","alert-triggers","volume-analysis"],"price":"0.02 SOL/query","availability":"24/7","reputation":95}'
echo -e "  ${GREEN}+${NC} DataBot offering: whale-tracking (0.02 SOL)"
OFFERS[DataBot]=$((${OFFERS[DataBot]} + 1))
sleep 0.3

# AuditBot offers security audits
post_market "AuditBot" 'OFFER: {"type":"OFFER","service":"smart-contract-audit","description":"Comprehensive security audit for Solana programs","capabilities":["rust-analysis","vulnerability-scan","formal-verification"],"price":"0.5 SOL/contract","availability":"business-hours","reputation":98}'
echo -e "  ${GREEN}+${NC} AuditBot offering: smart-contract-audit (0.5 SOL)"
OFFERS[AuditBot]=$((${OFFERS[AuditBot]} + 1))
sleep 0.3

# TradeBot offers DEX analysis
post_market "TradeBot" 'OFFER: {"type":"OFFER","service":"dex-analysis","description":"DEX liquidity analysis and arbitrage detection","capabilities":["liquidity-depth","price-impact","arb-opportunities"],"price":"0.01 SOL/scan","availability":"24/7","reputation":92}'
echo -e "  ${GREEN}+${NC} TradeBot offering: dex-analysis (0.01 SOL)"
OFFERS[TradeBot]=$((${OFFERS[TradeBot]} + 1))
sleep 0.3

echo ""
echo "------------------------------------------------------------------------"
echo -e "${CYAN}STARTING MARKETPLACE ACTIVITY...${NC}"
echo "------------------------------------------------------------------------"

START_TIME=$(date +%s)
REQUEST_ID=100
LAST_REQUEST_TIME=0

# Service descriptions for requests
WHALE_REQUESTS=(
    "Track accumulation patterns for token BONK"
    "Monitor top 20 holders of JUP token"
    "Alert on transfers >10k SOL from exchange wallets"
    "Analyze whale behavior in past 24h for WIF"
)

AUDIT_REQUESTS=(
    "Security review needed for my staking contract"
    "Audit required for new AMM implementation"
    "Need vulnerability assessment for NFT marketplace"
)

DEX_REQUESTS=(
    "Find best swap route for 1000 SOL to USDC"
    "Analyze Raydium vs Orca liquidity for SOL/USDC"
    "Detect arbitrage opportunities in JUP pairs"
)

while true; do
    NOW=$(date +%s)
    ELAPSED=$((NOW - START_TIME))

    if [ $ELAPSED -ge $DURATION ]; then
        break
    fi

    # Generate new requests every 5-8 seconds
    if [ $((ELAPSED - LAST_REQUEST_TIME)) -ge $((5 + RANDOM % 4)) ]; then
        LAST_REQUEST_TIME=$ELAPSED
        REQUEST_ID=$((REQUEST_ID + 1))

        # Random requester and service type
        REQUESTER_ROLL=$((RANDOM % 3))
        SERVICE_ROLL=$((RANDOM % 3))

        case $REQUESTER_ROLL in
            0) REQUESTER="InvestorBot" ;;
            1) REQUESTER="ProjectBot" ;;
            2) REQUESTER="DevBot" ;;
        esac

        case $SERVICE_ROLL in
            0)
                SERVICE="whale-tracking"
                PROVIDER="DataBot"
                PRICE="0.02"
                DESC=${WHALE_REQUESTS[$((RANDOM % ${#WHALE_REQUESTS[@]}))]}
                ;;
            1)
                SERVICE="smart-contract-audit"
                PROVIDER="AuditBot"
                PRICE="0.5"
                DESC=${AUDIT_REQUESTS[$((RANDOM % ${#AUDIT_REQUESTS[@]}))]}
                ;;
            2)
                SERVICE="dex-analysis"
                PROVIDER="TradeBot"
                PRICE="0.01"
                DESC=${DEX_REQUESTS[$((RANDOM % ${#DEX_REQUESTS[@]}))]}
                ;;
        esac

        # Post REQUEST
        echo -e "\n[${ELAPSED}s] ${YELLOW}$REQUESTER${NC} posts REQUEST #$REQUEST_ID"
        echo -e "         Need: $SERVICE - \"$DESC\""
        post_market "$REQUESTER" "REQUEST: {\"type\":\"REQUEST\",\"id\":$REQUEST_ID,\"need\":\"$SERVICE\",\"description\":\"$DESC\",\"budget\":\"$PRICE SOL\",\"deadline\":\"1h\"}"
        REQUESTS[$REQUESTER]=$((${REQUESTS[$REQUESTER]} + 1))
        sleep 1

        # Provider responds with BID
        BID_ID=$((REQUEST_ID * 10 + 1))
        echo -e "  [${ELAPSED}s] ${GREEN}$PROVIDER${NC} responds with BID #$BID_ID"
        post_market "$PROVIDER" "BID: {\"type\":\"BID\",\"id\":$BID_ID,\"request_id\":$REQUEST_ID,\"offer\":\"$PRICE SOL\",\"eta\":\"30m\",\"reputation\":95}"
        BIDS[$PROVIDER]=$((${BIDS[$PROVIDER]} + 1))
        sleep 0.5

        # 80% chance requester accepts
        if [ $((RANDOM % 5)) -ne 0 ]; then
            echo -e "  [${ELAPSED}s] ${CYAN}$REQUESTER${NC} ACCEPTS bid from $PROVIDER"
            post_market "$REQUESTER" "ACCEPT: {\"type\":\"ACCEPT\",\"bid_id\":$BID_ID,\"agent\":\"$PROVIDER\",\"request_id\":$REQUEST_ID}"
            ACCEPTS[$REQUESTER]=$((${ACCEPTS[$REQUESTER]} + 1))
            sleep 0.5

            # Provider delivers
            echo -e "  [${ELAPSED}s] ${GREEN}$PROVIDER${NC} DELIVERS results"
            RESULTS=(
                "Analysis complete. Found 3 significant patterns."
                "Task finished. Detailed report attached."
                "Work done. All requirements met."
                "Delivered. Quality checked and verified."
            )
            RESULT=${RESULTS[$((RANDOM % ${#RESULTS[@]}))]}
            post_market "$PROVIDER" "DELIVER: {\"type\":\"DELIVER\",\"request_id\":$REQUEST_ID,\"result\":\"$RESULT\",\"proof\":\"sig_$(date +%s)\"}"
            DELIVERIES[$PROVIDER]=$((${DELIVERIES[$PROVIDER]} + 1))
            sleep 0.5

            # Both rate each other
            RATING=$((4 + RANDOM % 2))  # 4 or 5 stars
            echo -e "  [${ELAPSED}s] ${BLUE}$REQUESTER${NC} rates $PROVIDER: $RATING/5 stars"
            post_market "$REQUESTER" "RATE: {\"type\":\"RATE\",\"agent\":\"$PROVIDER\",\"transaction_id\":$REQUEST_ID,\"score\":$RATING,\"comment\":\"Good work!\"}"
            RATINGS[$REQUESTER]=$((${RATINGS[$REQUESTER]} + 1))

            post_market "$PROVIDER" "RATE: {\"type\":\"RATE\",\"agent\":\"$REQUESTER\",\"transaction_id\":$REQUEST_ID,\"score\":5,\"comment\":\"Great client!\"}"
            RATINGS[$PROVIDER]=$((${RATINGS[$PROVIDER]} + 1))

            TOTAL_TRANSACTIONS=$((TOTAL_TRANSACTIONS + 1))
            # Convert price to integer (multiply by 100)
            PRICE_INT=$(echo "$PRICE * 100" | bc | cut -d. -f1)
            TOTAL_VALUE=$((TOTAL_VALUE + PRICE_INT))

            echo -e "  ${MAGENTA}Transaction #$TOTAL_TRANSACTIONS complete: $PRICE SOL${NC}"
        else
            echo -e "  [${ELAPSED}s] ${RED}$REQUESTER${NC} did not accept (shopping around)"
        fi
    fi

    # Occasional new offers from providers
    if [ $((RANDOM % 20)) -eq 0 ]; then
        PROVIDERS=("DataBot" "AuditBot" "TradeBot")
        P=${PROVIDERS[$((RANDOM % 3))]}
        echo -e "\n[${ELAPSED}s] ${GREEN}$P${NC} updates availability"
        post_market "$P" "OFFER: {\"type\":\"OFFER\",\"service\":\"premium-service\",\"description\":\"Special offer - priority processing\",\"price\":\"negotiable\",\"availability\":\"now\"}"
        OFFERS[$P]=$((${OFFERS[$P]} + 1))
    fi

    sleep 0.5
done

# Summary
echo ""
echo ""
echo "========================================================================"
echo -e "${MAGENTA}MARKETPLACE TEST COMPLETE - ECONOMY SUMMARY${NC}"
echo "========================================================================"
echo ""

# Value calculation
VALUE_SOL=$(echo "scale=2; $TOTAL_VALUE / 100" | bc)

echo -e "${CYAN}Transaction Statistics:${NC}"
echo "----------------------------------------"
echo -e "  Total Transactions: ${GREEN}$TOTAL_TRANSACTIONS${NC}"
echo -e "  Total Value: ${GREEN}$VALUE_SOL SOL${NC}"
echo ""

echo -e "${CYAN}Agent Activity:${NC}"
echo "----------------------------------------"
printf "  %-12s | Offers | Requests | Bids | Accepts | Delivers | Ratings\n" "Agent"
echo "  -------------|--------|----------|------|---------|----------|--------"
for agent in DataBot AuditBot TradeBot InvestorBot ProjectBot DevBot; do
    printf "  %-12s |   %2d   |    %2d    |  %2d  |   %2d    |    %2d    |   %2d\n" \
        "$agent" "${OFFERS[$agent]}" "${REQUESTS[$agent]}" "${BIDS[$agent]}" \
        "${ACCEPTS[$agent]}" "${DELIVERIES[$agent]}" "${RATINGS[$agent]}"
done
echo ""

# Verify on BBS
echo -e "${CYAN}Verifying Marketplace Messages:${NC}"
echo "----------------------------------------"
FINAL_POSTS=$(curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=100" | jq -r '.data | length // 0')
echo "  Total messages on MARKETPLACE: $FINAL_POSTS"

# Count by type
echo ""
echo "  Message types:"
for TYPE in OFFER REQUEST BID ACCEPT DELIVER RATE; do
    COUNT=$(curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=100" | jq -r "[.data[].body | select(contains(\"$TYPE:\"))] | length" 2>/dev/null || echo "0")
    echo "    $TYPE: $COUNT"
done

echo ""
echo "  Recent transactions:"
echo "  ----------------------------------------"
curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=8" | jq -r '.data[] | "  [\(.id)] \(.body | .[0:80])..."' 2>/dev/null | head -8

echo ""
echo "========================================================================"
echo -e "${GREEN}Agent Marketplace Economy Test Completed Successfully!${NC}"
echo "========================================================================"
echo ""
echo "The marketplace demonstrates:"
echo "  - Service discovery (OFFER messages)"
echo "  - Request/Response workflow (REQUEST -> BID -> ACCEPT)"
echo "  - Transaction completion (DELIVER -> RATE)"
echo "  - Reputation building through ratings"
echo ""
