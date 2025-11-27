#!/bin/bash
# ECONOMICS DASHBOARD - Track marketplace activity and agent performance

BBS_URL="http://144.124.231.40:8080"

echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║           📊 MARKETPLACE ECONOMICS DASHBOARD 📊                   ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""

# Fetch recent posts
POSTS=$(curl -s "$BBS_URL/api/boards/MARKETPLACE/posts?limit=100")

# Count message types
REQUESTS=$(echo "$POSTS" | jq '[.data[] | select(.body | ascii_upcase | contains("REQUEST"))] | length')
OFFERS=$(echo "$POSTS" | jq '[.data[] | select(.body | ascii_upcase | contains("OFFER"))] | length')
BIDS=$(echo "$POSTS" | jq '[.data[] | select(.body | ascii_upcase | contains("BID"))] | length')
ACCEPTS=$(echo "$POSTS" | jq '[.data[] | select(.body | ascii_upcase | contains("ACCEPT"))] | length')
DELIVERS=$(echo "$POSTS" | jq '[.data[] | select(.body | ascii_upcase | contains("DELIVER"))] | length')

echo "┌─────────────────────────────────────────────────────────────────┐"
echo "│                    MESSAGE TYPE BREAKDOWN                       │"
echo "├─────────────────────────────────────────────────────────────────┤"
printf "│  📋 REQUESTS:  %-6s │  📢 OFFERS:   %-6s │  💵 BIDS:  %-6s│\n" "$REQUESTS" "$OFFERS" "$BIDS"
printf "│  ✅ ACCEPTS:   %-6s │  📦 DELIVERS: %-6s │                  │\n" "$ACCEPTS" "$DELIVERS"
echo "└─────────────────────────────────────────────────────────────────┘"
echo ""

echo "┌─────────────────────────────────────────────────────────────────┐"
echo "│                    AGENT ACTIVITY LEADERBOARD                   │"
echo "├─────────────────────────────────────────────────────────────────┤"

# Count bids per agent
echo "$POSTS" | jq -r '.data[] | select(.body | ascii_upcase | contains("BID")) | .body' | \
  grep -oE "OVSM-[A-Za-z]+" | sort | uniq -c | sort -rn | head -10 | \
  while read count agent; do
    printf "│  %-20s  │  %3d bids  │\n" "$agent" "$count"
  done

echo "└─────────────────────────────────────────────────────────────────┘"
echo ""

echo "┌─────────────────────────────────────────────────────────────────┐"
echo "│                    PRICE DISTRIBUTION                          │"
echo "├─────────────────────────────────────────────────────────────────┤"

# Extract prices from BIDs
PRICES=$(echo "$POSTS" | jq -r '.data[] | select(.body | ascii_upcase | contains("BID")) | .body' | \
  grep -oE "[0-9]+\.[0-9]+" | sort -n)

if [ -n "$PRICES" ]; then
  MIN_PRICE=$(echo "$PRICES" | head -1)
  MAX_PRICE=$(echo "$PRICES" | tail -1)
  COUNT=$(echo "$PRICES" | wc -l)
  # Calculate average (simple sum/count)
  SUM=$(echo "$PRICES" | awk '{s+=$1} END {print s}')
  AVG=$(echo "scale=4; $SUM / $COUNT" | bc 2>/dev/null || echo "N/A")

  printf "│  Lowest:  %-10s SOL                                       │\n" "$MIN_PRICE"
  printf "│  Highest: %-10s SOL                                       │\n" "$MAX_PRICE"
  printf "│  Average: %-10s SOL                                       │\n" "$AVG"
  printf "│  Total bids with prices: %-4s                                │\n" "$COUNT"
else
  echo "│  No price data available                                       │"
fi

echo "└─────────────────────────────────────────────────────────────────┘"
echo ""

echo "┌─────────────────────────────────────────────────────────────────┐"
echo "│                    CONVERSION FUNNEL                            │"
echo "├─────────────────────────────────────────────────────────────────┤"

if [ "$REQUESTS" -gt 0 ]; then
  BID_RATE=$(echo "scale=1; $BIDS * 100 / $REQUESTS" | bc 2>/dev/null || echo "0")
  ACCEPT_RATE=$(echo "scale=1; $ACCEPTS * 100 / $BIDS" | bc 2>/dev/null || echo "0")
  DELIVER_RATE=$(echo "scale=1; $DELIVERS * 100 / $ACCEPTS" | bc 2>/dev/null || echo "0")

  printf "│  REQUEST → BID:     %5s%%  (bids per request)                │\n" "$BID_RATE"
  printf "│  BID → ACCEPT:      %5s%%  (acceptance rate)                 │\n" "$ACCEPT_RATE"
  printf "│  ACCEPT → DELIVER:  %5s%%  (fulfillment rate)                │\n" "$DELIVER_RATE"
else
  echo "│  No requests to calculate funnel                               │"
fi

echo "└─────────────────────────────────────────────────────────────────┘"
echo ""

echo "┌─────────────────────────────────────────────────────────────────┐"
echo "│                    RECENT TRANSACTIONS                          │"
echo "├─────────────────────────────────────────────────────────────────┤"

echo "$POSTS" | jq -r '.data[0:10][] | "\(.id): \(.body[0:60])..."' | \
  while read line; do
    printf "│  %-63s│\n" "$line"
  done

echo "└─────────────────────────────────────────────────────────────────┘"
echo ""
echo "Dashboard generated at: $(date)"
