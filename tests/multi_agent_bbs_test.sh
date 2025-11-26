#!/bin/bash
#
# Multi-Agent BBS Communication Test
#
# 5 specialized agents communicate via the BBS to collaborate on tasks:
# 1. Coordinator - Assigns tasks and monitors progress
# 2. Researcher - Looks up information and reports findings
# 3. Analyst - Analyzes data and provides insights
# 4. Validator - Validates results and flags issues
# 5. Reporter - Summarizes and creates final reports
#

set -e

# Configuration
BBS_URL="${BBS_URL:-http://144.124.231.40:8080}"
BOARD="RESEARCH"
DURATION="${1:-45}"  # Duration in seconds, default 45

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Agent stats
declare -A SENT
declare -A READ
SENT[Coordinator]=0; SENT[Researcher]=0; SENT[Analyst]=0; SENT[Validator]=0; SENT[Reporter]=0
READ[Coordinator]=0; READ[Researcher]=0; READ[Analyst]=0; READ[Validator]=0; READ[Reporter]=0

# Post a message
post_message() {
    local agent="$1"
    local message="$2"
    local reply_to="$3"

    local full_msg="[$agent] $message"

    if [ -n "$reply_to" ]; then
        curl -s -X POST "$BBS_URL/api/posts/$reply_to/reply" \
            -H "Content-Type: application/json" \
            -d "{\"message\":\"$full_msg\",\"user_node_id\":\"!${agent,,}01\"}" > /dev/null
    else
        curl -s -X POST "$BBS_URL/api/boards/$BOARD/posts" \
            -H "Content-Type: application/json" \
            -d "{\"message\":\"$full_msg\",\"user_node_id\":\"!${agent,,}01\"}" > /dev/null
    fi

    SENT[$agent]=$((${SENT[$agent]} + 1))
}

# Get messages from board
get_messages() {
    curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=50" | jq -r '.data[] | "\(.id)|\(.body)"' 2>/dev/null || echo ""
}

# Get latest message ID
get_latest_id() {
    curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=1" | jq -r '.data[0].id // 0' 2>/dev/null || echo "0"
}

echo "========================================================================"
echo -e "${CYAN}MULTI-AGENT BBS COMMUNICATION TEST${NC}"
echo "========================================================================"
echo -e "BBS Server: ${YELLOW}$BBS_URL${NC}"
echo -e "Board: ${YELLOW}$BOARD${NC}"
echo -e "Duration: ${YELLOW}$DURATION seconds${NC}"
echo ""

# Check BBS is reachable
echo -n "Checking BBS connectivity... "
if curl -s "$BBS_URL/api/stats" | jq -e '.success' > /dev/null 2>&1; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    echo "Cannot connect to BBS at $BBS_URL"
    exit 1
fi

# Get initial state
INITIAL_POSTS=$(curl -s "$BBS_URL/api/stats" | jq -r '.data.posts // 0')
echo "Initial posts on server: $INITIAL_POSTS"
echo ""

echo "------------------------------------------------------------------------"
echo -e "${CYAN}Agents announcing presence...${NC}"
echo "------------------------------------------------------------------------"

post_message "Coordinator" "Agent Coordinator (task coordinator) is online and ready"
echo -e "  ${GREEN}✓${NC} Coordinator online"
sleep 0.3

post_message "Researcher" "Agent Researcher (information gatherer) is online and ready"
echo -e "  ${GREEN}✓${NC} Researcher online"
sleep 0.3

post_message "Analyst" "Agent Analyst (data analyzer) is online and ready"
echo -e "  ${GREEN}✓${NC} Analyst online"
sleep 0.3

post_message "Validator" "Agent Validator (result validator) is online and ready"
echo -e "  ${GREEN}✓${NC} Validator online"
sleep 0.3

post_message "Reporter" "Agent Reporter (summary reporter) is online and ready"
echo -e "  ${GREEN}✓${NC} Reporter online"

echo ""
echo "------------------------------------------------------------------------"
echo -e "${CYAN}Starting agent collaboration loop...${NC}"
echo "------------------------------------------------------------------------"

START_TIME=$(date +%s)
TASK_COUNT=0
LAST_TASK_TIME=0
LAST_RESEARCHER_MSG=""
LAST_ANALYST_MSG=""

# Research topics
TOPICS=(
    "Solana TPS metrics"
    "DEX volume analysis"
    "NFT collection trends"
    "Validator performance"
    "MEV activity patterns"
    "Token holder distribution"
    "Smart contract interactions"
    "Cross-chain bridge activity"
)

# Task targets
ASSIGNEES=("Researcher" "Analyst" "Researcher" "Analyst")

while true; do
    NOW=$(date +%s)
    ELAPSED=$((NOW - START_TIME))

    if [ $ELAPSED -ge $DURATION ]; then
        break
    fi

    # Coordinator assigns tasks every 8 seconds
    if [ $((ELAPSED - LAST_TASK_TIME)) -ge 8 ]; then
        LAST_TASK_TIME=$ELAPSED
        TASK_COUNT=$((TASK_COUNT + 1))

        ASSIGNEE=${ASSIGNEES[$((RANDOM % ${#ASSIGNEES[@]}))]}
        TOPIC=${TOPICS[$((RANDOM % ${#TOPICS[@]}))]}

        echo -e "\n[${ELAPSED}s] ${YELLOW}Coordinator${NC} assigning task #$TASK_COUNT to $ASSIGNEE"
        post_message "Coordinator" "TASK for $ASSIGNEE: Investigate $TOPIC and report findings"
    fi

    # Researcher responds to tasks
    if [ $((RANDOM % 3)) -eq 0 ]; then
        FINDINGS=(
            "Found 3 relevant data points showing positive trends"
            "Analysis reveals interesting patterns in the data"
            "Data indicates significant activity increase"
            "Research complete: metrics are within normal range"
            "Discovered anomalies that warrant further investigation"
        )
        FINDING=${FINDINGS[$((RANDOM % ${#FINDINGS[@]}))]}

        if [ "$LAST_RESEARCHER_MSG" != "$FINDING" ]; then
            echo -e "  [${ELAPSED}s] ${GREEN}Researcher${NC} completed research"
            post_message "Researcher" "RESULT: $FINDING"
            LAST_RESEARCHER_MSG="$FINDING"
        fi
    fi

    # Analyst provides insights
    if [ $((RANDOM % 4)) -eq 0 ]; then
        INSIGHTS=(
            "Pattern analysis reveals bullish indicators"
            "Anomaly detected: unusual volume spike identified"
            "Correlation found between key metrics"
            "Risk assessment: exposure level is moderate"
            "Prediction model suggests continued stability"
        )
        INSIGHT=${INSIGHTS[$((RANDOM % ${#INSIGHTS[@]}))]}

        if [ "$LAST_ANALYST_MSG" != "$INSIGHT" ]; then
            echo -e "  [${ELAPSED}s] ${BLUE}Analyst${NC} provided analysis"
            post_message "Analyst" "ANALYSIS: $INSIGHT"
            LAST_ANALYST_MSG="$INSIGHT"
        fi
    fi

    # Validator validates results
    if [ $((RANDOM % 5)) -eq 0 ]; then
        if [ $((RANDOM % 5)) -eq 0 ]; then
            echo -e "  [${ELAPSED}s] ${RED}Validator${NC} flagged issue"
            post_message "Validator" "FLAGGED: Potential data inconsistency detected - requires review"
        else
            echo -e "  [${ELAPSED}s] ${GREEN}Validator${NC} validated result"
            post_message "Validator" "VALIDATED: Data integrity confirmed, results are reliable"
        fi
    fi

    sleep 1
done

echo ""
echo "------------------------------------------------------------------------"
echo -e "${CYAN}Creating final report...${NC}"
echo "------------------------------------------------------------------------"

post_message "Reporter" "REPORT: Multi-agent collaboration complete. $TASK_COUNT tasks processed. All agents performed within expected parameters. System health: GOOD"
echo -e "  ${GREEN}✓${NC} Reporter created final summary"

# Summary
echo ""
echo "========================================================================"
echo -e "${CYAN}TEST COMPLETE - SUMMARY${NC}"
echo "========================================================================"
echo ""
echo "Agent Statistics:"
echo "----------------------------------------"
printf "  %-15s | Sent: %3d\n" "Coordinator" "${SENT[Coordinator]}"
printf "  %-15s | Sent: %3d\n" "Researcher" "${SENT[Researcher]}"
printf "  %-15s | Sent: %3d\n" "Analyst" "${SENT[Analyst]}"
printf "  %-15s | Sent: %3d\n" "Validator" "${SENT[Validator]}"
printf "  %-15s | Sent: %3d\n" "Reporter" "${SENT[Reporter]}"
echo "----------------------------------------"
TOTAL=$((${SENT[Coordinator]} + ${SENT[Researcher]} + ${SENT[Analyst]} + ${SENT[Validator]} + ${SENT[Reporter]}))
printf "  %-15s | Sent: %3d\n" "TOTAL" "$TOTAL"
echo ""

# Verify messages on BBS
echo "Verifying messages on BBS..."
FINAL_POSTS=$(curl -s "$BBS_URL/api/stats" | jq -r '.data.posts // 0')
NEW_POSTS=$((FINAL_POSTS - INITIAL_POSTS))
echo "  Posts before test: $INITIAL_POSTS"
echo "  Posts after test: $FINAL_POSTS"
echo -e "  ${GREEN}New posts created: $NEW_POSTS${NC}"

echo ""
echo "Sample messages from $BOARD board:"
echo "----------------------------------------"
curl -s "$BBS_URL/api/boards/$BOARD/posts?limit=5" | jq -r '.data[] | "  [\(.id)] \(.body)"' 2>/dev/null | head -10

echo ""
echo "========================================================================"
echo -e "${GREEN}Multi-agent BBS communication test completed successfully!${NC}"
echo "========================================================================"
