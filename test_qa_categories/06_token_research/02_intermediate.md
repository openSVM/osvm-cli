# Token Research - Intermediate Analysis (Q5101-Q5120)

**Difficulty:** Intermediate
**Focus:** Holder analysis, liquidity assessment, trading patterns, protocol interactions
**Questions:** Q5101-Q5120 (20 questions)

---

## Q5101: "What is the holder distribution for 'Pyth Network' (PYTH)? Calculate the Gini coefficient."

**Expected Plan:**
[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, CALCULATE_GINI_COEFFICIENT

**Main Branch:**
$token_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht"
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)
$gini = CALCULATE_GINI_COEFFICIENT(holders: $holders)

**Decision Point:** Evaluate decentralization
  BRANCH A ($gini > 0.9):
    $distribution = "Extremely Centralized"
  BRANCH B ($gini > 0.7):
    $distribution = "Centralized"
  BRANCH C (default):
    $distribution = "Relatively Distributed"

**Action:**
RETURN {distribution_type: $distribution, gini_coefficient: $gini, confidence: 95}

---

## Q5102: "How much liquidity is available for 'Bonk' (BONK) across all Raydium and Orca pools?"

**Expected Plan:**
[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library: RAYDIUM_GET_POOLS, ORCA_GET_POOLS, SUM

**Main Branch:**
$token_mint = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
$raydium_pools = RAYDIUM_GET_POOLS(mint: $token_mint)
$orca_pools = ORCA_GET_POOLS(mint: $token_mint)
$total_liquidity = SUM($raydium_pools.liquidity) + SUM($orca_pools.liquidity)

**Action:**
RETURN {token: "BONK", total_liquidity_usd: $total_liquidity, confidence: 96}

---

## Q5103: "Identify wallets that bought 'WIF' in the first hour of launch."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, FILTER

**Main Branch:**
$token_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"
$launch_time = getFirstTransactionTime(mint: $token_mint)
$early_window = $launch_time + 3600
$all_sigs = getSignaturesForAddress(address: $token_mint, limit: 5000)
$early_buyers = FILTER($all_sigs, sig => sig.blockTime < $early_window)

**Action:**
RETURN {token: "WIF", early_buyers: COUNT($early_buyers), confidence: 92}

---

## Q5104: "Analyze 'JUP' trading patterns. What percentage is bot vs human trading?"

**Expected Plan:**
[TIME: ~2m] [COST: ~0.02 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, DETECT_BOT_PATTERN

**Main Branch:**
$jup_mint = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"
$signatures = getSignaturesForAddress(address: $jup_mint, limit: 1000)
$bot_count = COUNT(FILTER($signatures, sig => DETECT_BOT_PATTERN(sig)))
$bot_pct = ($bot_count / COUNT($signatures)) * 100

**Action:**
RETURN {token: "JUP", bot_volume_pct: $bot_pct, confidence: 88}

---

## Q5105: "Find wallets holding both 'mSOL' and 'jitoSOL'."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, FIND_COMMON_HOLDERS

**Main Branch:**
$msol = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"
$jitosol = "J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yYac6Y7kGCPn"
$msol_holders = getTokenLargestAccounts(mint: $msol, limit: 500)
$jitosol_holders = getTokenLargestAccounts(mint: $jitosol, limit: 500)
$common = FIND_COMMON_HOLDERS(list1: $msol_holders, list2: $jitosol_holders)

**Action:**
RETURN {common_holders: COUNT($common), confidence: 93}

---

## Q5106: "What is the average time between swaps for top 20 'BONK' traders?"

**Expected Plan:**
[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, GROUP_BY, MEAN

**Main Branch:**
$bonk = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
$sigs = getSignaturesForAddress(address: $bonk, limit: 5000)
$grouped = GROUP_BY(collection: $sigs, key: "signer")
$top_20 = SORT_BY($grouped, "count", desc)[0..20]
$avg_interval = MEAN(MAP($top_20, t => calculateInterval(t.timestamps)))

**Action:**
RETURN {token: "BONK", avg_swap_interval_seconds: $avg_interval, confidence: 90}

---

## Q5107: "Calculate impermanent loss for SOL-USDC pool on Orca over 30 days."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library: GET_HISTORICAL_PRICE, CALCULATE_IMPERMANENT_LOSS

**Main Branch:**
$start_date = NOW() - (30 * 86400)
$sol_price_start = GET_HISTORICAL_PRICE(token: "SOL", timestamp: $start_date)
$sol_price_end = GET_HISTORICAL_PRICE(token: "SOL", timestamp: NOW())
$price_change = (($sol_price_end - $sol_price_start) / $sol_price_start) * 100
$il_pct = CALCULATE_IMPERMANENT_LOSS(price_change_pct: $price_change)

**Action:**
RETURN {pool: "SOL-USDC-Orca", impermanent_loss_pct: $il_pct, confidence: 94}

---

## Q5108: "Find tokens with >50% price increase in 24h with $100K+ liquidity."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.025 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: SCAN_ALL_TOKENS, GET_PRICE_CHANGE, FILTER

**Main Branch:**
$all_tokens = SCAN_ALL_TOKENS(min_liquidity: 100000)
$pumping = FILTER($all_tokens, t => GET_PRICE_CHANGE(t, "24h") > 0.50)
$sorted = SORT_BY($pumping, "price_change", desc)

**Action:**
RETURN {tokens_found: COUNT($pumping), top_10_gainers: $sorted[0..10], confidence: 91}

---

## Q5109: "Compare swap success rates for 'WIF' on Raydium vs Orca in last 6 hours."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, FILTER

**Main Branch:**
$wif = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"
$cutoff = NOW() - (6 * 3600)
$sigs = getSignaturesForAddress(address: $wif, limit: 2000)
$recent = FILTER($sigs, sig => sig.blockTime > $cutoff)
$raydium_success = calculateSuccessRate(FILTER($recent, isRaydiumSwap))
$orca_success = calculateSuccessRate(FILTER($recent, isOrcaSwap))

**Action:**
RETURN {token: "WIF", raydium_success_pct: $raydium_success, orca_success_pct: $orca_success, confidence: 89}

---

## Q5110: "Find tokens deployed in last 24h with >$10K initial liquidity."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.035 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: FIND_NEW_TOKEN_DEPLOYMENTS, ANALYZE_CREATOR_HISTORY

**Main Branch:**
$cutoff = NOW() - 86400
$new_tokens = FIND_NEW_TOKEN_DEPLOYMENTS(since: $cutoff)
$qualified = FILTER($new_tokens, t => t.initial_liquidity > 10000)
$with_analysis = MAP($qualified, t => {
  $history = ANALYZE_CREATOR_HISTORY(wallet: t.creator)
  RETURN {token: t, creator_history: $history}
})

**Action:**
RETURN {new_tokens_found: COUNT($qualified), tokens: $with_analysis, confidence: 87}

---

## Q5111: "Calculate correlation between 'SOL' and 'JUP' prices over 90 days."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, CORRELATE

**Main Branch:**
$sol_prices = GET_PRICE_HISTORY(token: "SOL", period: "90d", interval: "1d")
$jup_prices = GET_PRICE_HISTORY(token: "JUP", period: "90d", interval: "1d")
$correlation = CORRELATE(data1: $sol_prices, data2: $jup_prices)

**Decision Point:** Interpret correlation
  BRANCH A ($correlation > 0.7):
    $interpretation = "Strong positive correlation"
  BRANCH B ($correlation > 0.3):
    $interpretation = "Moderate positive correlation"
  BRANCH C (default):
    $interpretation = "Weak or no correlation"

**Action:**
RETURN {correlation_coefficient: $correlation, interpretation: $interpretation, confidence: 96}

---

## Q5112: "Identify 'wash trading' patterns in 'BONK' by finding wallets trading with each other."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.04 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, BUILD_TRADING_GRAPH, DETECT_CIRCULAR_TRADING

**Main Branch:**
$bonk = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
$sigs = getSignaturesForAddress(address: $bonk, limit: 3000)
$graph = BUILD_TRADING_GRAPH(transactions: $sigs)
$patterns = DETECT_CIRCULAR_TRADING(graph: $graph, min_cycle: 2)

**Action:**
RETURN {token: "BONK", wash_trading_patterns: COUNT($patterns), confidence: 83}

---

## Q5113: "What is average slippage for >$10K 'WIF' swaps on Jupiter?"

**Expected Plan:**
[TIME: ~2m] [COST: ~0.025 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, CALCULATE_SLIPPAGE, MEAN

**Main Branch:**
$wif = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"
$jupiter = "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4"
$sigs = getSignaturesForAddress(address: $wif, limit: 2000)
$jupiter_swaps = FILTER($sigs, sig => invokesProgram(sig, $jupiter))
$large_swaps = FILTER($jupiter_swaps, s => extractValue(s) > 10000)
$avg_slippage = MEAN(MAP($large_swaps, s => CALCULATE_SLIPPAGE(s)))

**Action:**
RETURN {token: "WIF", avg_slippage_pct: $avg_slippage, confidence: 88}

---

## Q5114: "Find wallets that consistently buy tokens immediately after Jupiter listings."

**Expected Plan:**
[TIME: ~4m] [COST: ~0.05 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: GET_JUPITER_NEW_LISTINGS, FIND_EARLY_BUYERS, CALCULATE_WALLET_PNL

**Main Branch:**
$listings = GET_JUPITER_NEW_LISTINGS(period: "30d")
$early_buyer_map = {}
FOR $listing IN $listings:
  $early_buyers = FIND_EARLY_BUYERS(token: $listing.mint, window: 30)
  FOR $buyer IN $early_buyers:
    $early_buyer_map[$buyer] = APPEND($early_buyer_map[$buyer], $listing.mint)

$consistent = FILTER($early_buyer_map, w => COUNT(w.tokens) >= 5)

**Action:**
RETURN {consistent_early_buyers: COUNT($consistent), confidence: 85}

---

## Q5115: "Analyze fee tier distribution for 'mSOL' liquidity pools."

**Expected Plan:**
[TIME: ~45s] [COST: ~0.006 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: GET_ALL_POOLS_FOR_TOKEN, GROUP_BY

**Main Branch:**
$msol = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"
$pools = GET_ALL_POOLS_FOR_TOKEN(token: $msol)
$by_fee = GROUP_BY($pools, "fee_tier")
$total_liquidity = SUM(MAP($pools, p => p.liquidity_usd))
$low_fee_pct = (SUM($by_fee.low_fee.liquidity) / $total_liquidity) * 100

**Action:**
RETURN {token: "mSOL", low_fee_liquidity_pct: $low_fee_pct, confidence: 93}

---

## Q5116: "Detect potential front-running in 'BONK' swaps by analyzing mempool."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.06 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: MONITOR_MEMPOOL, DETECT_FRONTRUNNING

**Main Branch:**
$bonk = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
$mempool_data = MONITOR_MEMPOOL(token: $bonk, duration: 600)
$frontrunning = DETECT_FRONTRUNNING(
  mempool_txs: $mempool_data.pending,
  confirmed_txs: $mempool_data.confirmed
)
$suspicious = FILTER($frontrunning, p => p.confidence > 0.75)

**Action:**
RETURN {token: "BONK", potential_frontrunning_events: COUNT($suspicious), confidence: 80}

---

## Q5117: "Compare yield for staking SOL directly vs depositing mSOL into lending."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: GET_STAKING_APY, GET_LENDING_APY

**Main Branch:**
$direct_apy = GET_STAKING_APY(token: "SOL", protocol: "native")
$marinade_apy = GET_STAKING_APY(token: "SOL", protocol: "Marinade")
$lending_protocols = ["Kamino", "Solend", "MarginFi"]
$lending_apys = MAP($lending_protocols, p => GET_LENDING_APY(token: "mSOL", protocol: p))
$combined_yield = $marinade_apy + MEAN($lending_apys)

**Action:**
RETURN {direct_staking_apy: $direct_apy, combined_yield: $combined_yield, advantage: $combined_yield - $direct_apy, confidence: 95}

---

## Q5118: "Find tokens with highest holder-to-transaction ratio (high retention)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.04 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library: SCAN_ALL_TOKENS, GET_HOLDER_COUNT, GET_TRANSACTION_COUNT

**Main Branch:**
$all_tokens = SCAN_ALL_TOKENS(min_market_cap: 1000000)
$retention = MAP($all_tokens, t => {
  $holders = GET_HOLDER_COUNT(token: t.mint)
  $txs = GET_TRANSACTION_COUNT(token: t.mint)
  RETURN {token: t, retention_ratio: $holders / $txs}
})
$top = SORT_BY($retention, "retention_ratio", desc)[0..10]

**Action:**
RETURN {top_10_by_retention: $top, confidence: 86}

---

## Q5119: "Calculate whale concentration risk for 'PYTH' (top 10 holder supply %)."

**Expected Plan:**
[TIME: ~30s] [COST: ~0.003 SOL] [CONFIDENCE: 97%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, getTokenSupply, SUM

**Main Branch:**
$pyth = "HZ1JovNiVvGrGNiiYvEozEVgZ58xdM1xzybapC8G4wEGGkZwyTDt1v"
$top_10 = getTokenLargestAccounts(mint: $pyth, limit: 10)
$total_supply = getTokenSupply(mint: $pyth).uiAmount
$top_10_supply = SUM(MAP($top_10, h => h.uiAmount))
$concentration = ($top_10_supply / $total_supply) * 100

**Decision Point:** Assess risk
  BRANCH A ($concentration > 50):
    $risk = "CRITICAL"
  BRANCH B ($concentration > 30):
    $risk = "HIGH"
  BRANCH C (default):
    $risk = "MODERATE"

**Action:**
RETURN {token: "PYTH", top_10_concentration_pct: $concentration, risk_level: $risk, confidence: 97}

---

## Q5120: "Analyze OpenBook order book depth for 'JUP' - bid-ask spread and liquidity."

**Expected Plan:**
[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: GET_OPENBOOK_ORDERBOOK, CALCULATE_SPREAD

**Main Branch:**
$jup = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"
$market = FIND_OPENBOOK_MARKET(token: $jup, quote: "USDC")
$orderbook = GET_OPENBOOK_ORDERBOOK(market: $market)
$best_bid = $orderbook.bids[0].price
$best_ask = $orderbook.asks[0].price
$mid_price = ($best_bid + $best_ask) / 2
$spread_bps = (($best_ask - $best_bid) / $mid_price) * 10000
$liquidity_1pct = calculateLiquidityInRange($orderbook, $mid_price, 0.01)

**Action:**
RETURN {token: "JUP", mid_price: $mid_price, bid_ask_spread_bps: $spread_bps, liquidity_within_1pct: $liquidity_1pct, confidence: 92}
