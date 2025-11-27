#!/bin/bash
# MULTI-ROUND PRICE WAR - Watch agents adapt over multiple rounds

BBS_URL="http://144.124.231.40:8080"
OSVM="./target/release/osvm"
ROUNDS=5

echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║        🔄 MULTI-ROUND PRICE WAR SIMULATION 🔄                     ║"
echo "║                                                                   ║"
echo "║   Watch how agent prices EVOLVE over $ROUNDS rounds               ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""

# Track equilibrium prices over rounds
declare -a EQUILIBRIUM_PRICES
declare -a MARKET_AVGS

for ROUND in $(seq 1 $ROUNDS); do
  echo "═══════════════════════════════════════════════════════════════════"
  echo "                        ROUND $ROUND / $ROUNDS"
  echo "═══════════════════════════════════════════════════════════════════"

  # Post fresh requests with varying budgets
  BUDGET=$(echo "scale=2; 0.04 + ($ROUND * 0.01)" | bc)
  curl -s -X POST "$BBS_URL/api/boards/MARKETPLACE/posts" \
    -H "Content-Type: application/json" \
    -d "{\"message\":\"REQUEST: [Round $ROUND] Whale tracking needed. Budget: $BUDGET SOL\"}" > /dev/null
  echo "📝 Posted request with budget: $BUDGET SOL"
  echo ""

  # Run each agent (abbreviated output)
  echo "🔥 Aggressor (race to bottom)..."
  AGG_OUT=$($OSVM ovsm run examples/ovsm_scripts/aggressive_agent.ovsm 2>&1)
  AGG_PRICE=$(echo "$AGG_OUT" | grep -oE "Price: [0-9.]+ SOL" | head -1)
  echo "   $AGG_PRICE"

  echo "📊 Competitor (undercut 10%)..."
  COMP_OUT=$($OSVM ovsm run examples/ovsm_scripts/competitive_agent.ovsm 2>&1)
  COMP_PRICE=$(echo "$COMP_OUT" | grep -oE "MY PRICE: [0-9.]+ SOL" | head -1)
  echo "   $COMP_PRICE"

  echo "⚖️  Equilibrium (Nash)..."
  EQ_OUT=$($OSVM ovsm run examples/ovsm_scripts/equilibrium_agent.ovsm 2>&1)
  EQ_PRICE=$(echo "$EQ_OUT" | grep "My equilibrium price:" | grep -oE "[0-9]+\.[0-9]+")
  MKT_AVG=$(echo "$EQ_OUT" | grep "Market avg:" | grep -oE "[0-9]+\.[0-9]+")
  echo "   Equilibrium: $EQ_PRICE SOL (market avg: $MKT_AVG SOL)"

  # Store for trend analysis
  EQUILIBRIUM_PRICES+=("$EQ_PRICE")
  MARKET_AVGS+=("$MKT_AVG")

  echo "💎 Premium (quality focus)..."
  PREM_OUT=$($OSVM ovsm run examples/ovsm_scripts/premium_agent.ovsm 2>&1)
  PREM_BIDS=$(echo "$PREM_OUT" | grep "Premium bids placed:" | grep -oE "[0-9]+")
  echo "   Fixed: 0.08 SOL ($PREM_BIDS premium bids)"

  echo "📈 Smart (budget-relative)..."
  SMART_OUT=$($OSVM ovsm run examples/ovsm_scripts/smart_agent.ovsm 2>&1)
  SMART_BIDS=$(echo "$SMART_OUT" | grep "Bids made:" | grep -oE "[0-9]+")
  echo "   Dynamic pricing ($SMART_BIDS bids)"

  # Run buyer
  echo ""
  echo "🛒 Buyer evaluating bids..."
  BUYER_OUT=$($OSVM ovsm run examples/ovsm_scripts/buyer_agent.ovsm 2>&1)
  WINNER=$(echo "$BUYER_OUT" | grep "Best bid:" | head -1)
  ACCEPTED=$(echo "$BUYER_OUT" | grep "accepted-price" | grep -oE "[0-9]+\.[0-9]+")
  echo "   $WINNER"
  echo "   ✅ Accepted at: $ACCEPTED SOL"

  echo ""
  sleep 1
done

echo ""
echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║                    📈 PRICE EVOLUTION SUMMARY                     ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""
echo "Equilibrium Price Trend:"
for i in $(seq 0 $((ROUNDS-1))); do
  printf "  Round %d: %s SOL\n" "$((i+1))" "${EQUILIBRIUM_PRICES[$i]}"
done
echo ""
echo "Market Average Trend:"
for i in $(seq 0 $((ROUNDS-1))); do
  printf "  Round %d: %s SOL\n" "$((i+1))" "${MARKET_AVGS[$i]}"
done
echo ""
echo "Analysis: If equilibrium price stabilizes, market reached Nash equilibrium!"
