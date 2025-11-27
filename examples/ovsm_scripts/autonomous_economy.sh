#!/bin/bash
# ═══════════════════════════════════════════════════════════════════
#                 AUTONOMOUS AGENT ECONOMY
# ═══════════════════════════════════════════════════════════════════
#
# Self-sustaining marketplace that runs continuously:
# - Auto-generates requests at random intervals
# - Multiple seller agents compete
# - Quality-aware buyers evaluate and accept
# - Winners execute and deliver
# - Dashboard tracks economics
#
# Press Ctrl+C to stop

BBS_URL="http://144.124.231.40:8080"
OSVM="./target/release/osvm"
CYCLE_DELAY=5  # seconds between cycles
MAX_CYCLES=20  # set to 0 for infinite

trap cleanup EXIT
cleanup() {
  echo ""
  echo "═══════════════════════════════════════════════════════════════════"
  echo "                    ECONOMY SHUTDOWN"
  echo "═══════════════════════════════════════════════════════════════════"
  echo ""
  bash examples/ovsm_scripts/economics_dashboard.sh
  echo ""
  echo "Autonomous economy stopped at: $(date)"
}

echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║              🌐 AUTONOMOUS AGENT ECONOMY 🌐                       ║"
echo "║                                                                   ║"
echo "║   Self-sustaining marketplace simulation                         ║"
echo "║   • Auto-generates requests                                      ║"
echo "║   • Agents compete and adapt                                     ║"
echo "║   • Quality-aware buyers accept best value                       ║"
echo "║   • Real deliveries with blockchain data                         ║"
echo "║                                                                   ║"
echo "║   Press Ctrl+C to stop and see final dashboard                   ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""

CYCLE=0
TOTAL_REQUESTS=0
TOTAL_ACCEPTS=0
TOTAL_DELIVERS=0

while true; do
  CYCLE=$((CYCLE + 1))

  if [ "$MAX_CYCLES" -gt 0 ] && [ "$CYCLE" -gt "$MAX_CYCLES" ]; then
    echo "Reached max cycles ($MAX_CYCLES). Stopping..."
    break
  fi

  echo ""
  echo "═══════════════════════════════════════════════════════════════════"
  echo "                    CYCLE $CYCLE"
  echo "═══════════════════════════════════════════════════════════════════"
  TIMESTAMP=$(date +"%H:%M:%S")
  echo "Time: $TIMESTAMP"
  echo ""

  # ─────────────────────────────────────────────────────────────────────
  # PHASE 1: Generate random request
  # ─────────────────────────────────────────────────────────────────────
  BUDGET_CENTS=$((RANDOM % 8 + 3))  # 3-10 cents = 0.03-0.10 SOL
  BUDGET="0.0$BUDGET_CENTS"
  REQUEST_TYPE=$((RANDOM % 3))

  case $REQUEST_TYPE in
    0) SERVICE="whale wallet tracking" ;;
    1) SERVICE="token flow analysis" ;;
    2) SERVICE="DeFi position monitoring" ;;
  esac

  echo "📝 AUTO-GENERATING REQUEST..."
  echo "   Service: $SERVICE"
  echo "   Budget: $BUDGET SOL"

  curl -s -X POST "$BBS_URL/api/boards/MARKETPLACE/posts" \
    -H "Content-Type: application/json" \
    -d "{\"message\":\"REQUEST: [AUTO-$CYCLE] Need $SERVICE. Budget: $BUDGET SOL\",\"agent\":\"ECONOMY-Generator\"}" > /dev/null
  TOTAL_REQUESTS=$((TOTAL_REQUESTS + 1))
  echo ""

  # ─────────────────────────────────────────────────────────────────────
  # PHASE 2: Seller agents compete (run 2-3 random agents)
  # ─────────────────────────────────────────────────────────────────────
  echo "🏪 SELLERS COMPETING..."

  # Always run equilibrium (it adapts)
  EQ_OUT=$($OSVM ovsm run examples/ovsm_scripts/equilibrium_agent.ovsm 2>&1)
  EQ_PRICE=$(echo "$EQ_OUT" | grep "equilibrium-price" | grep -oE "[0-9]+\.[0-9]+")
  echo "   ⚖️  Equilibrium: $EQ_PRICE SOL"

  # Random chance for other agents
  if [ $((RANDOM % 2)) -eq 0 ]; then
    AGG_OUT=$($OSVM ovsm run examples/ovsm_scripts/aggressive_agent.ovsm 2>&1)
    echo "   🔥 Aggressor: 0.015 SOL (race to bottom)"
  fi

  if [ $((RANDOM % 2)) -eq 0 ]; then
    $OSVM ovsm run examples/ovsm_scripts/premium_agent.ovsm 2>&1 > /dev/null
    echo "   💎 Premium: 0.08 SOL (quality focus)"
  fi

  if [ $((RANDOM % 2)) -eq 0 ]; then
    COMP_OUT=$($OSVM ovsm run examples/ovsm_scripts/competitive_agent.ovsm 2>&1)
    COMP_PRICE=$(echo "$COMP_OUT" | grep "My price" | grep -oE "[0-9]+\.[0-9]+")
    echo "   📊 Competitor: $COMP_PRICE SOL (undercut)"
  fi

  echo ""

  # ─────────────────────────────────────────────────────────────────────
  # PHASE 3: Buyer evaluates and accepts
  # ─────────────────────────────────────────────────────────────────────
  echo "🛒 BUYER EVALUATING..."

  # Alternate between simple buyer and smart buyer
  if [ $((CYCLE % 2)) -eq 0 ]; then
    BUYER_OUT=$($OSVM ovsm run examples/ovsm_scripts/buyer_agent.ovsm 2>&1)
    BUYER_TYPE="Price-focused"
  else
    BUYER_OUT=$($OSVM ovsm run examples/ovsm_scripts/smart_buyer.ovsm 2>&1)
    BUYER_TYPE="Quality-focused"
  fi

  ACCEPTED=$(echo "$BUYER_OUT" | grep "accepted-price\|winner-price" | grep -oE "[0-9]+\.[0-9]+")
  WINNER_BID=$(echo "$BUYER_OUT" | grep "accepted-bid\|winner-bid" | grep -oE "[0-9]+")

  if [ -n "$ACCEPTED" ] && [ "$ACCEPTED" != "0" ]; then
    echo "   ✅ $BUYER_TYPE buyer ACCEPTED at $ACCEPTED SOL (bid #$WINNER_BID)"
    TOTAL_ACCEPTS=$((TOTAL_ACCEPTS + 1))

    # ─────────────────────────────────────────────────────────────────────
    # PHASE 4: Winner delivers (simulated)
    # ─────────────────────────────────────────────────────────────────────
    echo ""
    echo "📦 DELIVERY..."

    # Run handle_accept to do real blockchain work
    DELIVER_OUT=$($OSVM ovsm run examples/ovsm_scripts/handle_accept.ovsm 2>&1)
    if echo "$DELIVER_OUT" | grep -q "DELIVERED"; then
      echo "   ✅ Real blockchain data delivered!"
      TOTAL_DELIVERS=$((TOTAL_DELIVERS + 1))
    else
      echo "   ⏳ Delivery pending..."
    fi
  else
    echo "   ❌ No acceptable bids within budget"
  fi

  # ─────────────────────────────────────────────────────────────────────
  # PHASE 5: Economy stats
  # ─────────────────────────────────────────────────────────────────────
  echo ""
  echo "📈 ECONOMY STATS:"
  echo "   Requests: $TOTAL_REQUESTS | Accepts: $TOTAL_ACCEPTS | Delivers: $TOTAL_DELIVERS"

  if [ "$TOTAL_REQUESTS" -gt 0 ]; then
    ACCEPT_RATE=$((TOTAL_ACCEPTS * 100 / TOTAL_REQUESTS))
    echo "   Acceptance rate: $ACCEPT_RATE%"
  fi

  if [ "$TOTAL_ACCEPTS" -gt 0 ]; then
    FULFILL_RATE=$((TOTAL_DELIVERS * 100 / TOTAL_ACCEPTS))
    echo "   Fulfillment rate: $FULFILL_RATE%"
  fi

  echo ""
  echo "⏳ Next cycle in $CYCLE_DELAY seconds..."
  sleep $CYCLE_DELAY
done
