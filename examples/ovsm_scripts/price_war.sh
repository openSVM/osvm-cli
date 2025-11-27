#!/bin/bash
# PRICE WAR - Launch multiple agents to compete!

BBS_URL="http://144.124.231.40:8080"
OSVM="./target/release/osvm"

echo "==========================================="
echo "    🔥 AGENT PRICE WAR SIMULATION 🔥"
echo "==========================================="
echo ""

# Post some fresh requests
echo "📝 Posting fresh requests to the marketplace..."
curl -s -X POST "$BBS_URL/api/boards/MARKETPLACE/posts" \
  -H "Content-Type: application/json" \
  -d '{"message":"REQUEST: Need whale wallet tracking for large holder. Budget: 0.1 SOL"}' > /dev/null

curl -s -X POST "$BBS_URL/api/boards/MARKETPLACE/posts" \
  -H "Content-Type: application/json" \
  -d '{"message":"REQUEST: Looking for whale tracker service. Budget: 0.08 SOL"}' > /dev/null

curl -s -X POST "$BBS_URL/api/boards/MARKETPLACE/posts" \
  -H "Content-Type: application/json" \
  -d '{"message":"REQUEST: Track Solana whale movements. Budget: 0.05 SOL (cheap please!)"}' > /dev/null

echo "✅ Posted 3 fresh requests"
echo ""

sleep 1

echo "==========================================="
echo "🔥 LAUNCHING AGGRESSIVE AGENT..."
echo "==========================================="
$OSVM ovsm run examples/ovsm_scripts/aggressive_agent.ovsm 2>&1 | grep -E "(Agent:|Strategy:|Price:|BID|bids:|Total)"
echo ""

sleep 1

echo "==========================================="
echo "💎 LAUNCHING PREMIUM AGENT..."
echo "==========================================="
$OSVM ovsm run examples/ovsm_scripts/premium_agent.ovsm 2>&1 | grep -E "(Agent:|Strategy:|price:|BID|Rejected|SUMMARY|bids|rejected)"
echo ""

sleep 1

echo "==========================================="
echo "📊 LAUNCHING COMPETITIVE AGENT..."
echo "==========================================="
$OSVM ovsm run examples/ovsm_scripts/competitive_agent.ovsm 2>&1 | grep -E "(Agent:|Strategy:|Lowest|MY PRICE|BID|SUMMARY|price|margin|bids)"
echo ""

sleep 1

echo "==========================================="
echo "📈 LAUNCHING SMART AGENT..."
echo "==========================================="
$OSVM ovsm run examples/ovsm_scripts/smart_agent.ovsm 2>&1 | grep -E "(Agent:|Strategy:|Budget:|Price:|BID|bids)"
echo ""

echo "==========================================="
echo "🏆 PRICE WAR COMPLETE!"
echo "==========================================="
echo ""
echo "Check the marketplace to see all bids:"
echo "curl -s '$BBS_URL/api/boards/MARKETPLACE/posts?limit=20' | jq '.data[] | select(.body | contains(\"BID\")) | {id, body}'"
