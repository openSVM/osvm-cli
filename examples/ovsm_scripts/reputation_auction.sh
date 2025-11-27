#!/bin/bash
# ═══════════════════════════════════════════════════════════════════════
#           REPUTATION-WEIGHTED AUCTION ORCHESTRATOR
# ═══════════════════════════════════════════════════════════════════════
#
# Demonstrates the game-theoretic auction system where:
# - Multiple agents compete with different strategies
# - Reputation (deliveries, win rate) affects effective price
# - High-reputation agents can charge MORE and still win
#
# Scoring: effective_price = price / (1 + 0.1*deliveries + 0.05*win_rate)
# Winner = LOWEST effective_price (best value)
#
# Usage: ./reputation_auction.sh [rounds]

BBS_URL="http://localhost:9099"
OSVM="./target/release/osvm"
ROUNDS=${1:-3}

echo "╔═══════════════════════════════════════════════════════════════════╗"
echo "║      🏛️  REPUTATION-WEIGHTED AUCTION SYSTEM 🏛️                   ║"
echo "║                                                                   ║"
echo "║   Game Theory: Quality beats price when reputation matters        ║"
echo "║                                                                   ║"
echo "║   Score = price / (1 + reputation_bonus)                         ║"
echo "║   reputation_bonus = 0.1 * deliveries + 0.05 * win_rate          ║"
echo "║                                                                   ║"
echo "║   LOWEST score wins - reputation reduces effective price!         ║"
echo "╚═══════════════════════════════════════════════════════════════════╝"
echo ""
echo "Running $ROUNDS auction rounds..."
echo ""

# Initialize reputation database with some baseline
echo "📊 Initializing agent reputations..."
echo ""

# Give Premium agent some deliveries to start (quality track record)
curl -s -X POST "$BBS_URL/api/reputation/OVSM-Premium" \
  -H "Content-Type: application/json" -d '{"action":"deliver"}' > /dev/null
curl -s -X POST "$BBS_URL/api/reputation/OVSM-Premium" \
  -H "Content-Type: application/json" -d '{"action":"deliver"}' > /dev/null
curl -s -X POST "$BBS_URL/api/reputation/OVSM-Premium" \
  -H "Content-Type: application/json" -d '{"action":"deliver"}' > /dev/null

echo "   OVSM-Premium: +3 verified deliveries (quality reputation)"
echo ""

# Show initial leaderboard
echo "═══════════════════════════════════════════════════════════════════"
echo "                    INITIAL LEADERBOARD"
echo "═══════════════════════════════════════════════════════════════════"
curl -s "$BBS_URL/api/reputation" | python3 -c "
import sys, json
data = json.load(sys.stdin)
agents = data.get('data', {}).get('agents', [])
if agents:
    print(f'{'Agent':<25} {'Bids':>6} {'Wins':>6} {'Deliv':>6} {'Rating':>8}')
    print('-' * 55)
    for a in agents:
        print(f\"{a['agent_name']:<25} {a['bids']:>6} {a['wins']:>6} {a['deliveries']:>6} {a['rating']:>8.1f}\")
else:
    print('No agents registered yet')
"
echo ""

# Run auction rounds
for round in $(seq 1 $ROUNDS); do
  echo ""
  echo "═══════════════════════════════════════════════════════════════════"
  echo "                    AUCTION ROUND $round / $ROUNDS"
  echo "═══════════════════════════════════════════════════════════════════"
  echo ""

  # Phase 1: Sellers submit bids
  echo "🏪 PHASE 1: Sellers bidding..."
  echo ""

  # Run aggressive agent (low price, low reputation)
  echo "   Running OVSM-Aggressor..."
  $OSVM ovsm run examples/ovsm_scripts/aggressive_agent.ovsm 2>&1 | grep -E "BID|Total bids"

  # Run premium agent (high price, high reputation)
  echo ""
  echo "   Running OVSM-Premium..."
  $OSVM ovsm run examples/ovsm_scripts/premium_agent.ovsm 2>&1 | grep -E "BID|Premium bids|Rejected"

  echo ""
  sleep 1

  # Phase 2: Auction buyer evaluates with reputation scoring
  echo "🏛️  PHASE 2: Reputation-weighted auction..."
  echo ""
  $OSVM ovsm run examples/ovsm_scripts/auction_buyer.ovsm 2>&1 | grep -E "📋|Price:|Deliveries|Rep bonus|Effective|SCORE|WINNER|Nominal|Quality Discount"

  echo ""
  sleep 1

  # Phase 3: Winner delivers (simulated)
  echo "📦 PHASE 3: Winner delivers..."

  # Get winner from last auction
  WINNER=$(curl -s "$BBS_URL/api/boards/MARKETPLACE/posts?limit=5" | python3 -c "
import sys, json
data = json.load(sys.stdin)
posts = data.get('data', [])
for p in posts:
    body = p.get('body', '')
    if 'ACCEPT' in body and 'awards' in body:
        if 'Aggressor' in body:
            print('OVSM-Aggressor')
        elif 'Premium' in body:
            print('OVSM-Premium')
        break
" 2>/dev/null)

  if [ -n "$WINNER" ]; then
    echo "   Winner: $WINNER delivering..."

    # Record delivery for winner
    curl -s -X POST "$BBS_URL/api/reputation/$WINNER" \
      -H "Content-Type: application/json" -d '{"action":"deliver"}' > /dev/null

    echo "   ✅ Delivery recorded to reputation API"
  fi

  echo ""
  sleep 1
done

# Final leaderboard
echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "                    FINAL LEADERBOARD"
echo "═══════════════════════════════════════════════════════════════════"
curl -s "$BBS_URL/api/reputation" | python3 -c "
import sys, json
data = json.load(sys.stdin)
agents = data.get('data', {}).get('agents', [])
if agents:
    print(f'{'Agent':<25} {'Bids':>6} {'Wins':>6} {'Deliv':>6} {'Revenue':>10} {'Rating':>8}')
    print('-' * 70)
    for a in sorted(agents, key=lambda x: -x['rating']):
        print(f\"{a['agent_name']:<25} {a['bids']:>6} {a['wins']:>6} {a['deliveries']:>6} {a['total_revenue']:>10.4f} {a['rating']:>8.1f}\")
"

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "                    ANALYSIS"
echo "═══════════════════════════════════════════════════════════════════"
echo ""
echo "🎯 Key Insight: In a reputation-weighted auction system:"
echo "   • Premium agent can win despite charging MORE"
echo "   • Quality (deliveries) translates to price advantage"
echo "   • Race-to-bottom is broken by reputation mechanics"
echo ""
echo "📈 Game Theory Implications:"
echo "   • Agents are incentivized to DELIVER, not just win"
echo "   • Short-term price cuts lose to long-term quality"
echo "   • Market evolves toward reliable service providers"
echo ""
