# Token Research - Actionable On-Chain Patterns (Q401-Q500)

**Category:** Token Research
**Difficulty:** Advanced
**Focus:** Identifying actionable on-chain patterns, smart money tracking, accumulation/distribution analysis, behavioral patterns
**Questions:** Q401-Q500

---

## Q5401: "How can I identify 'smart money' wallets that consistently buy new tokens like 'WIF' before they experience a major price pump?"

**Expected Plan:**

[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - getTransaction (Solana RPC)
  - IDENTIFY_SMART_MONEY (Pattern Analysis)
  - CALCULATE_WALLET_PNL (Forensics)
  - MAP, FILTER, SORT_BY (Data Processing)

**Main Branch:**
$token_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm" // WIF mint address
$token_launch_date = "2023-12-20"

// Get early transactions for the token
$signatures = getSignaturesForAddress(address: $token_mint, until: $token_launch_date + " 7d")
$transactions = MAP(collection: $signatures, fn: sig => getTransaction(signature: sig.signature))

// Identify wallets that bought early
$early_buyers = MAP(collection: $transactions, fn: tx => tx.signer)

// Analyze the historical performance of these early buyers
$smart_money_candidates = IDENTIFY_SMART_MONEY(
  wallets: UNIQUE($early_buyers),
  min_pnl: 100000, // Minimum $100k profit across all trades
  min_win_rate: 0.6 // At least 60% win rate
)

$top_wallets = SORT_BY(collection: $smart_money_candidates, field: "total_pnl", direction: "desc")

**Decision Point:** Evaluate the quality of the identified smart money.
  BRANCH A (COUNT($top_wallets) > 5):
    $verdict = "Found several high-performing wallets."
    $details = "Multiple wallets show a pattern of early buys on successful tokens."
  BRANCH B (COUNT($top_wallets) > 0):
    $verdict = "Found a few promising wallets."
    $details = "A small number of wallets meet the smart money criteria."
  BRANCH C (default):
    $verdict = "No clear smart money patterns found for this token."
    $details = "Early buyers do not have a consistent history of high performance."

**Action:**
RETURN {
  verdict: $verdict,
  details: $details,
  smart_money_wallets: SLICE($top_wallets, start: 0, end: 10),
  confidence: 85
}

---

## Q5402: "What on-chain patterns indicate a large-scale accumulation of the 'PYTH' token by institutional players or whales?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - getTransaction (Solana RPC)
  - DETECT_ACCUMULATION_PATTERN (Pattern Analysis)
  - IDENTIFY_WALLET_CLUSTERS (Forensics)

**Main Branch:**
$token_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht" // PYTH mint address
$time_period = "30d"

// Get recent transactions
$signatures = getSignaturesForAddress(address: $token_mint, time_period: $time_period, limit: 1000)
$transactions = MAP(collection: $signatures, fn: sig => getTransaction(signature: sig.signature))

// Analyze for accumulation patterns
$accumulation_analysis = DETECT_ACCUMULATION_PATTERN(
  transactions: $transactions,
  min_total_volume: 1000000 // Look for accumulation of at least 1M tokens
)

// Check if accumulation is from a cluster of related wallets
$wallet_clusters = IDENTIFY_WALLET_CLUSTERS(wallets: $accumulation_analysis.accumulating_wallets)

**Decision Point:** Assess the nature of the accumulation.
  BRANCH A ($accumulation_analysis.is_detected && $wallet_clusters.has_strong_cluster):
    $finding = "Coordinated accumulation detected."
    $explanation = "A cluster of related wallets is steadily accumulating large amounts of the token, suggesting a single entity is building a position."
  BRANCH B ($accumulation_analysis.is_detected):
    $finding = "Dispersed accumulation detected."
    $explanation = "Multiple independent large wallets are accumulating the token, suggesting broad whale interest."
  BRANCH C (default):
    $finding = "No significant accumulation pattern detected."
    $explanation = "On-chain flows appear to be normal buy/sell activity, not systematic accumulation."

**Action:**
RETURN {
  finding: $finding,
  explanation: $explanation,
  total_accumulated: $accumulation_analysis.total_volume,
  accumulating_wallets: $accumulation_analysis.accumulating_wallets,
  is_coordinated: $wallet_clusters.has_strong_cluster,
  confidence: 90
}

---

## Q5403: "How can I identify patterns of airdrop farming for a potential 'Kamino Finance' token by analyzing wallet interactions?"

**Expected Plan:**

[TIME: ~120s] [COST: ~0.02 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts (Solana RPC)
  - GET_WALLET_INTERACTIONS (Forensics)
  - DETECT_SYBIL_PATTERNS (Pattern Analysis)

**Main Branch:**
$kamino_program_id = "..."

// Get all accounts that have interacted with Kamino
$user_accounts = getProgramAccounts(programId: $kamino_program_id)
$user_wallets = MAP(collection: $user_accounts, fn: acc => acc.owner)

// Analyze wallets for Sybil-like behavior
$sybil_analysis = DETECT_SYBIL_PATTERNS(wallets: UNIQUE($user_wallets))

**Decision Point:** Evaluate the evidence of airdrop farming.
  BRANCH A ($sybil_analysis.has_strong_clusters && $sybil_analysis.funding_pattern == "single_source"):
    $verdict = "High probability of large-scale airdrop farming."
    $reason = "Detected large clusters of wallets funded from a single source, all performing identical minimal actions on the protocol."
  BRANCH B ($sybil_analysis.has_weak_clusters):
    $verdict = "Moderate evidence of airdrop farming."
    $reason = "Some small wallet clusters with similar activity patterns were found, but no single large-scale operation."
  BRANCH C (default):
    $verdict = "Low evidence of systematic airdrop farming."
    $reason = "Wallet activity appears diverse and organic."

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  sybil_probability_score: $sybil_analysis.score,
  total_wallets_analyzed: COUNT(UNIQUE($user_wallets)),
  sybil_wallets_detected: $sybil_analysis.sybil_count,
  example_sybil_cluster: $sybil_analysis.clusters[0],
  confidence: 88
}

---

## Q5404: "What wallet activity patterns preceded the recent 'Tensor' (TNSR) CEX listing announcement, suggesting potential insider trading?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - getTransaction (Solana RPC)
  - DETECT_INSIDER_TRADING_PATTERNS (Pattern Analysis)

**Main Branch:**
$token_mint = "TNSR" // Placeholder for TNSR mint address
$listing_announcement_date = "2024-04-08"

// Look at activity in the 7 days before the announcement
$start_date = $listing_announcement_date - "7d"
$end_date = $listing_announcement_date

// Get transactions in that period
$signatures = getSignaturesForAddress(address: $token_mint, start_date: $start_date, end_date: $end_date)
$transactions = MAP(collection: $signatures, fn: sig => getTransaction(signature: sig.signature))

// Analyze for insider trading patterns
$insider_analysis = DETECT_INSIDER_TRADING_PATTERNS(
  transactions: $transactions,
  significant_event_date: $listing_announcement_date
)

**Decision Point:** Assess the evidence for insider trading.
  BRANCH A ($insider_analysis.high_confidence_insiders_found):
    $finding = "Strong evidence of insider trading."
    $explanation = "Found newly created wallets making large purchases shortly before the announcement, with no other activity. These wallets were funded by a common source."
  BRANCH B ($insider_analysis.medium_confidence_insiders_found):
    $finding = "Suspicious activity detected."
    $explanation = "Several wallets made unusually large purchases before the announcement, but their prior history is inconclusive."
  BRANCH C (default):
    $finding = "No clear evidence of insider trading."
    $explanation = "Buying activity before the announcement appears to be random and not concentrated in specific, suspicious wallets."

**Action:**
RETURN {
  finding: $finding,
  explanation: $explanation,
  suspicious_wallets: $insider_analysis.suspicious_wallets,
  total_volume_from_suspects: $insider_analysis.total_volume,
  confidence: 82
}

---

## Q5405: "How can I identify patterns of token distribution from the 'SharkyFi' team/investor wallets to detect if they are selling into market rallies?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.006 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - GET_WALLET_INTERACTIONS (Forensics)
  - CORRELATE_WITH_PRICE_DATA (Pattern Analysis)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$team_wallets = ["...", "...", "..."] // Known team/vesting contract addresses for SharkyFi
$token_mint = "SHRK" // Placeholder

// Get all outgoing transactions from the team wallets
$all_outgoing_txs = []
FOR $wallet IN $team_wallets:
  $interactions = GET_WALLET_INTERACTIONS(wallet_address: $wallet, direction: "out")
  $all_outgoing_txs = APPEND(array: $all_outgoing_txs, items: $interactions.transactions)

// Filter for transfers of the SHRK token
$shrk_transfers = FILTER(collection: $all_outgoing_txs, predicate: tx => tx.token_mint == $token_mint)

// Correlate the timing of these transfers with the SHRK price chart
$correlation_analysis = CORRELATE_WITH_PRICE_DATA(
  token: $token_mint
)

**Decision Point:** Determine if there's a pattern of selling on pumps.
  BRANCH A ($correlation_analysis.correlation_score > 0.8 && $correlation_analysis.event_type == "price_peak"):
    $pattern = "Strong pattern of selling into strength."
    $conclusion = "Team-affiliated wallets consistently transfer tokens out during or immediately after significant price rallies."
    $pattern = "Moderate correlation found."
    $conclusion = "There is some correlation between team transfers and price peaks, but it's not consistent across all events."
  BRANCH C (default):
    $pattern = "No clear pattern of selling into strength."
    $conclusion = "Token transfers from team wallets appear to follow a regular schedule or are uncorrelated with short-term price action."

**Action:**
RETURN {
  pattern_detected: $pattern,
  conclusion: $conclusion,
  correlation_score: $correlation_analysis.correlation_score,
  examples: $correlation_analysis.correlated_events,
  confidence: 92
}

---

## Q5406: "What are the behavioral patterns of wallets that successfully trade Solana memecoins, and how can I find more wallets like them?"

**Expected Plan:**

[TIME: ~120s] [COST: ~0.02 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - BIRDEYE_GET_TOP_TRADERS (Birdeye API)
  - GET_WALLET_INTERACTIONS (Forensics)
  - CLASSIFY_TRADING_BEHAVIOR (Pattern Analysis)
  - FIND_SIMILAR_WALLETS (Pattern Analysis)

**Main Branch:**
// 1. Find top memecoin traders from an external source
$top_traders = BIRDEYE_GET_TOP_TRADERS(category: "memecoins", time_period: "30d")

// 2. Analyze the behavior of the top 10 traders
$behavioral_profiles = []
FOR $trader IN SLICE($top_traders, 0, 10):
  $interactions = GET_WALLET_INTERACTIONS(wallet_address: $trader.address, limit: 200)
  $profile = CLASSIFY_TRADING_BEHAVIOR(transactions: $interactions.transactions)
  $behavioral_profiles = APPEND(array: $behavioral_profiles, item: $profile)

// 3. Create an aggregate "successful memecoin trader" profile
$aggregate_profile = AGGREGATE_PROFILES($behavioral_profiles)

// 4. Search for other wallets that match this aggregate profile
$similar_wallets = FIND_SIMILAR_WALLETS(profile: $aggregate_profile, search_space_size: 10000)

**Decision Point:** Summarize the dominant successful trading pattern.
  BRANCH A ($aggregate_profile.dominant_trait == "Sniper"):
    $pattern = "Early Entry (Sniping). Successful traders buy within the first few minutes of a token's launch."
  BRANCH B ($aggregate_profile.dominant_trait == "Swing Trader"):
    $pattern = "Swing Trading. Successful traders hold for several days to weeks, capturing the main part of the price curve."
  BRANCH C ($aggregate_profile.dominant_trait == "Quick Flipper"):
    $pattern = "Quick Flips. Successful traders rapidly enter and exit positions within hours, taking small but frequent profits."

  ## Q411: "Identify a 'wash trading' pattern for a specific NFT collection on Magic Eden to inflate its perceived volume and floor price."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: GET_NFT_TRADES, DETECT_WASH_TRADING_PATTERN

**Main Branch:**
$nft_collection_id = "..." // e.g., DeGods collection ID on Magic Eden

// Get all trades for the collection in the last 7 days
$trades = GET_NFT_TRADES(collection: $nft_collection_id, marketplace: "Magic Eden", period: "7d")

// Analyze trades for wash trading patterns
$wash_trade_analysis = DETECT_WASH_TRADING_PATTERN(trades: $trades)

**Decision Point:** Evaluate the extent of wash trading.
  BRANCH A ($wash_trade_analysis.wash_trade_percentage > 30):
    $verdict = "Significant Wash Trading Detected"
    $summary = "A substantial portion of the trading volume appears to be non-economic, originating from a small cluster of wallets trading back and forth."
  BRANCH B ($wash_trade_analysis.wash_trade_percentage > 10):
    $verdict = "Moderate Wash Trading Detected"
    $summary = "Some wash trading activity is present, likely from a few users trying to manipulate rankings or farm rewards."
  BRANCH C (default):
    $verdict = "Minimal Wash Trading"
    $summary = "Trading activity appears largely organic."

**Action:**
RETURN {
  verdict: $verdict,
  summary: $summary,
  wash_trade_percentage: $wash_trade_analysis.wash_trade_percentage,
  wash_trading_wallets: $wash_trade_analysis.wash_trading_wallets,
  estimated_fake_volume: $wash_trade_analysis.fake_volume_usd,
  confidence: 90
}

---

## Q5412: "Find the pattern of a 'Jito backrun' MEV, where a bot sees a large DEX swap and places a trade immediately after it in the same Jito bundle to profit from the price slippage."

**Expected Plan:**
[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: GET_JITO_BUNDLES, ANALYZE_JITO_BUNDLE_MEV

**Main Branch:**
$time_period = "24h"

// Get recent Jito bundles
$bundles = GET_JITO_BUNDLES(period: $time_period)

// Analyze each bundle for the backrun pattern
$backrun_events = []
FOR $bundle IN $bundles:
  $analysis = ANALYZE_JITO_BUNDLE_MEV(bundle: $bundle)
  IF $analysis.type == "Jito Backrun":
    $backrun_events = APPEND(array: $backrun_events, item: $analysis)

**Action:**
RETURN {
  analysis: "Jito Backrun MEV Pattern",
  events_found_24h: COUNT($backrun_events),
  total_profit_extracted_usd: SUM(MAP($backrun_events, e => e.profit_usd)),
  top_5_backrun_bots: SLICE(SORT_BY(GROUP_BY($backrun_events, "bot_address"), "total_profit", "desc"), 0, 5),
  example_backrun_event: $backrun_events[0],
  confidence: 92
}

---

## Q5413: "Identify a 'funding-and-dumping' pattern where a fresh wallet is funded by a CEX, buys a large amount of a new token, and then dumps it shortly after the first pump."

**Expected Plan:**
[TIME: ~70s] [COST: ~0.01 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADES_HISTORY, TRACE_FUNDS, GET_WALLET_CREATION_DATE

**Main Branch:**
$token_mint = "..."
$trades = GET_TOKEN_TRADES_HISTORY(token: $token_mint, period: "30d")

// Find wallets with a large net sell volume
$large_sellers = FIND_LARGE_SELLERS(trades: $trades, min_sell_usd: 50000)

// Analyze these sellers for the pattern
$funder_dumper_wallets = FILTER(collection: $large_sellers, predicate: wallet => {
  $creation_date = GET_WALLET_CREATION_DATE(wallet: wallet)
  $funding_trace = TRACE_FUNDS(start_address: wallet, direction: "inbound", depth: 1)
  $first_buy_time = FIND_FIRST_BUY(wallet: wallet, token: $token_mint).timestamp
  $first_sell_time = FIND_FIRST_SELL(wallet: wallet, token: $token_mint).timestamp

  $is_new_wallet = (NOW() - $creation_date) < 30 * 86400 // Created within 30 days
  $funded_by_cex = $funding_trace.source_is_cex
  $quick_dump = ($first_sell_time - $first_buy_time) < 7 * 86400 // Dumped within 7 days

  RETURN $is_new_wallet && $funded_by_cex && $quick_dump
})

**Action:**
RETURN {
  pattern: "Funding and Dumping",
  wallets_matching_pattern: $funder_dumper_wallets,
  count: COUNT($funder_dumper_wallets),
  confidence: 88
}

---

## Q5414: "Detect the pattern of a 'sandwich attack' on a specific Raydium liquidity pool."

**Expected Plan:**
[TIME: ~50s] [COST: ~0.006 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library: GET_POOL_TRANSACTIONS, DETECT_SANDWICH_ATTACKS

**Main Branch:**
$pool_address = "..." // e.g., SOL/USDC Raydium pool

// Get recent transactions for the specified pool
$transactions = GET_POOL_TRANSACTIONS(pool: $pool_address, period: "1h")

// Analyze transactions for sandwich attack patterns
$sandwich_attacks = DETECT_SANDWICH_ATTACKS(transactions: $transactions)

**Action:**
RETURN {
  analysis: "Sandwich Attack Pattern Detection",
  pool_address: $pool_address,
  attacks_detected_in_last_hour: COUNT($sandwich_attacks),
  total_profit_extracted_usd: SUM(MAP($sandwich_attacks, a => a.profit_usd)),
  most_common_victim_size_usd: MEDIAN(MAP($sandwich_attacks, a => a.victim_trade_size_usd)),
  top_attacker_bot: MAX_BY(GROUP_BY($sandwich_attacks, "attacker_address"), "total_profit"),
  confidence: 94
}

---

## Q5415: "Identify the pattern of 'cyclical whale dumping' for a token, where a large holder sells off a fixed amount every few days."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.007 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, GET_WALLET_TRANSACTION_HISTORY, DETECT_PERIODIC_SELLING

**Main Branch:**
$token_mint = "..."
$top_holders = getTokenLargestAccounts(mint: $token_mint, limit: 20)

// Analyze each top holder for periodic selling
$periodic_sellers = []
FOR $holder IN $top_holders:
  $sell_history = GET_WALLET_TRANSACTION_HISTORY(wallet: $holder.address, token: $token_mint, type: "sell")
  $analysis = DETECT_PERIODIC_SELLING(transactions: $sell_history)
  IF $analysis.is_periodic:
    $periodic_sellers = APPEND(array: $periodic_sellers, item: {
      wallet: $holder.address,
      period_days: $analysis.period_days,
      sell_amount: $analysis.average_amount
    })

**Action:**
RETURN {
  pattern: "Cyclical Whale Dumping",
  wallets_exhibiting_pattern: $periodic_sellers,
  count: COUNT($periodic_sellers),
  confidence: 85
}

---

## Q5416: "Find the on-chain footprint of a 'social media influencer pump', characterized by a surge of small buys immediately following a known influencer's post."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: SEARCH_WEB, GET_TOKEN_TRADES_HISTORY, CORRELATE_SOCIAL_TO_ONCHAIN

**Main Branch:**
$token_mint = "..."
$influencer_handle = "@cobie"

// Find timestamps of influencer's recent posts about the token
$post_timestamps = SEARCH_WEB(query: "tweets by " + $influencer_handle + " about $" + $token_mint)

// Get on-chain trades around those timestamps
$trades = GET_TOKEN_TRADES_HISTORY(token: $token_mint, period: "30d")

// Correlate posts with on-chain buy volume
$correlation = CORRELATE_SOCIAL_TO_ONCHAIN(social_events: $post_timestamps, onchain_events: $trades)

**Decision Point:** Evaluate the correlation.
  BRANCH A ($correlation.correlation_score > 0.7):
    $verdict = "Strong Influencer Pump Pattern Detected"
    $summary = "A clear spike in small-to-medium buy orders is visible within minutes of the influencer's posts."
  BRANCH B (default):
    $verdict = "No Clear Pattern Detected"
    $summary = "On-chain activity does not show a significant correlation with the influencer's social media posts."

**Action:**
RETURN {
  verdict: $verdict,
  summary: $summary,
  correlation_score: $correlation.correlation_score,
  average_volume_spike_after_post: $correlation.avg_volume_spike_usd,
  confidence: 80
}

---

## Q5417: "Detect a 'stop-loss hunting' pattern where a sharp, temporary price drop triggers a cascade of liquidations or stop-loss orders, followed by an immediate recovery."

**Expected Plan:**
[TIME: ~55s] [COST: ~0.006 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, DETECT_PRICE_ANOMALIES

**Main Branch:**
$token_mint = "..."
$price_history_1m = GET_PRICE_HISTORY(token: $token_mint, period: "7d", interval: "1m")

// Detect sharp, V-shaped price drops
$anomalies = DETECT_PRICE_ANOMALIES(prices: $price_history_1m, anomaly_type: "v_shape_drop")

// For each anomaly, check for a corresponding spike in liquidations or DEX trades
$stop_hunts = FILTER(collection: $anomalies, predicate: anomaly => {
  $trade_spike = GET_DEX_VOLUME_IN_WINDOW(token: $token_mint, start: anomaly.start_time, end: anomaly.end_time) > 100000
  $liquidation_spike = GET_LIQUIDATION_VOLUME_IN_WINDOW(token: $token_mint, start: anomaly.start_time, end: anomaly.end_time) > 50000
  RETURN $trade_spike || $liquidation_spike
})

**Action:**
RETURN {
  pattern: "Stop-Loss Hunting",
  events_detected_7d: COUNT($stop_hunts),
  example_event: $stop_hunts[0],
  confidence: 87
}

---

## Q5418: "Identify the pattern of a 'developer rug pull' on Raydium, characterized by the deployer removing liquidity shortly after launch."

**Expected Plan:**
[TIME: ~40s] [COST: ~0.004 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: GET_TOKEN_LAUNCH_INFO, GET_LIQUIDITY_EVENTS, TRACE_FUNDS

**Main Branch:**
$token_mint = "..."

// Get launch and deployer info
$launch_info = GET_TOKEN_LAUNCH_INFO(token: $token_mint)
$deployer_wallet = $launch_info.deployer

// Get all liquidity events for the token's pools
$lp_events = GET_LIQUIDITY_EVENTS(token: $token_mint)

// Find a liquidity removal event by the deployer
$rug_pull_event = FIND(collection: $lp_events, predicate: event => {
  RETURN event.type == "Remove" && event.wallet == $deployer_wallet
})

**Decision Point:** Confirm the rug pull.
  BRANCH A ($rug_pull_event != null && ($rug_pull_event.timestamp - $launch_info.timestamp) < 3 * 86400):
    $verdict = "Classic Developer Rug Pull Detected"
    $summary = "The deployer wallet removed liquidity within 3 days of launching the token."
    $trace = TRACE_FUNDS(start_address: $deployer_wallet, start_tx: $rug_pull_event.signature)
  BRANCH B (default):
    $verdict = "No Direct Developer Rug Pull Found"
    $summary = "The deployer wallet has not removed the initial liquidity."

**Action:**
RETURN {
  verdict: $verdict,
  summary: $summary,
  deployer_wallet: $deployer_wallet,
  rug_pull_transaction: $rug_pull_event,
  funds_destination_trace: $trace,
  confidence: 95
}

---

## Q5419: "Find a 'token impersonation' pattern where a scammer airdrops a fake token with the same name/symbol as a popular token to a large number of wallets."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.007 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: FIND_IMPERSONATING_TOKENS, GET_AIRDROP_RECIPIENTS

**Main Branch:**
$popular_token_name = "Jupiter"
$popular_token_symbol = "JUP"

// Find tokens that are trying to impersonate the popular one
$impersonators = FIND_IMPERSONATING_TOKENS(name: $popular_token_name, symbol: $popular_token_symbol)

// Analyze the distribution of these fake tokens
$scam_airdrops = FILTER(collection: $impersonators, predicate: token => {
  $recipients = GET_AIRDROP_RECIPIENTS(token: token.mint)
  RETURN COUNT($recipients) > 10000 // Airdropped to over 10,000 wallets
})

**Action:**
RETURN {
  pattern: "Token Impersonation Airdrop",
  impersonated_token: $popular_token_name,
  scam_airdrops_found: MAP($scam_airdrops, s => {
    mint: s.mint,
    name: s.name,
    symbol: s.symbol,
    holders: s.holder_count
  }),
  confidence: 91
}

---

## Q5420: "Detect a 'flash loan arbitrage' pattern where a user borrows a large sum from a protocol like Solend, manipulates prices on a DEX, and repays the loan in the same transaction."

**Expected Plan:**
[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: GET_RECENT_TRANSACTIONS, ANALYZE_TRANSACTION_FOR_FLASH_LOAN

**Main Branch:**
$dex_program_id = "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin" // Serum DEX example

// Get recent transactions on the DEX
$transactions = GET_RECENT_TRANSACTIONS(program: $dex_program_id, limit: 500)

// Filter for transactions that contain a flash loan pattern
$flash_loan_txs = FILTER(collection: $transactions, predicate: tx => {
  $analysis = ANALYZE_TRANSACTION_FOR_FLASH_LOAN(transaction: tx)
  RETURN $analysis.is_flash_loan
})

// Extract details from the flash loan transactions
$arbitrage_details = MAP(collection: $flash_loan_txs, fn: tx => {
  $analysis = ANALYZE_TRANSACTION_FOR_FLASH_LOAN(transaction: tx)
  RETURN {
    signature: tx.signature,
    loan_protocol: $analysis.loan_protocol,
    loan_amount_usd: $analysis.loan_amount_usd,
    profit_usd: $analysis.profit_usd
  }
})

**Action:**
RETURN {
  pattern: "Flash Loan Arbitrage",
  arbitrage_events_found: COUNT($arbitrage_details),
  total_profit_extracted_usd: SUM(MAP($arbitrage_details, a => a.profit_usd)),
  example_arbitrage_tx: $arbitrage_details[0],
  confidence: 93
}

**Action:**
RETURN {
  dominant_pattern: $pattern,
  profile_details: $aggregate_profile,
  top_trader_examples: SLICE($top_traders, 0, 5),
  newly_found_similar_wallets: $similar_wallets,
  confidence: 80
}

---

## Q5407: "How can I detect a 'token migration' scam pattern, where a fake token is created to trick holders of a legitimate token?"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - getAccountInfo (Solana RPC)
  - DETECT_TOKEN_IMPERSONATION (Pattern Analysis)

**Main Branch:**
$legitimate_token_mint = "..." // e.g., the real RNDR token mint
$suspicious_token_mint = "..." // The mint of the token claiming to be the new version

// 1. Get info for both tokens
$legit_info = getAccountInfo(pubkey: $legitimate_token_mint)
$fake_info = getAccountInfo(pubkey: $suspicious_token_mint)

// 2. Analyze for impersonation patterns
$impersonation_analysis = DETECT_TOKEN_IMPERSONATION(
  original_token: $legit_info,
  suspicious_token: $fake_info
)

// 3. Check if the scam token was airdropped to real holders
$legit_holders = getTokenLargestAccounts(mint: $legitimate_token_mint, limit: 100)
$scam_holders = getTokenLargestAccounts(mint: $suspicious_token_mint, limit: 100)
$holder_overlap = INTERSECTION(MAP($legit_holders, h=>h.address), MAP($scam_holders, h=>h.address))

**Decision Point:** Determine if it's a migration scam.
  BRANCH A ($impersonation_analysis.is_impersonation && COUNT($holder_overlap) > 50):
    $verdict = "Confirmed Token Migration Scam."
    $reason = "The suspicious token mimics the original's metadata and has been airdropped to a large number of the original token's holders to create confusion."
  BRANCH B ($impersonation_analysis.is_impersonation):
    $verdict = "Likely Impersonation Scam."
    $reason = "The token metadata (name, symbol) is designed to impersonate the legitimate token, but it hasn't been widely distributed to holders yet."
  BRANCH C (default):
    $verdict = "Not a clear migration scam."
    $reason = "The tokens have different metadata and holder bases."

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  impersonation_score: $impersonation_analysis.score,
  holder_overlap_percentage: COUNT($holder_overlap) / 100,
  confidence: 95
}

---

## Q5408: "What on-chain patterns differentiate a genuine community-driven memecoin from one manipulated by a small group of insiders?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.007 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - getSignaturesForAddress (Solana RPC)
  - CALCULATE_GINI_COEFFICIENT (Statistical)
  - ANALYZE_HOLDER_DISTRIBUTION (Pattern Analysis)

**Main Branch:**
$token_mint = "..."

// 1. Analyze holder concentration
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)
$gini_coefficient = CALCULATE_GINI_COEFFICIENT(holders: $holders)
$top_100_percentage = SUM(MAP(SLICE($holders, 0, 100), h => h.percentage))

// 2. Analyze initial distribution
$signatures = getSignaturesForAddress(address: $token_mint, limit: 1000, direction: "earliest")
$initial_distribution = ANALYZE_HOLDER_DISTRIBUTION(signatures: $signatures)

**Decision Point:** Classify the token's distribution pattern.
  BRANCH A ($gini_coefficient > 0.9 || $top_100_percentage > 80):
    $verdict = "Insider Manipulated"
    $reason = "Holder distribution is extremely concentrated. A small number of wallets hold the vast majority of the supply, giving them full control over the price."
  BRANCH B ($initial_distribution.was_fair_launch == false && $gini_coefficient > 0.8):
    $verdict = "Insider Manipulated"
    $reason = "The initial distribution was not a fair launch; a large portion of the supply was allocated to a few wallets before public trading began."
  BRANCH C ($gini_coefficient < 0.7 && $initial_distribution.was_fair_launch == true):
    $verdict = "Community Driven"
    $reason = "Holder distribution is relatively widespread, and the initial launch was fair, indicating organic growth."
  BRANCH D (default):
    $verdict = "Mixed Signals"
    $reason = "The token shows some signs of concentration but also has a reasonably broad holder base."

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  gini_coefficient: $gini_coefficient,
  top_100_holders_own_pct: $top_100_percentage,
  was_fair_launch: $initial_distribution.was_fair_launch,
  confidence: 89
}

---

## Q5409: "How can I identify a 'slow rug' pattern, where the team gradually sells off their holdings instead of a single large liquidity pull?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - GET_WALLET_INTERACTIONS (Forensics)
  - IDENTIFY_WALLET_CLUSTERS (Forensics)
  - DETECT_PERIODIC_SELLING (Pattern Analysis)

**Main Branch:**
$project_main_wallet = "..." // The project's treasury or deployer wallet
$token_mint = "..."

// 1. Identify the cluster of team/project wallets
$team_cluster = IDENTIFY_WALLET_CLUSTERS(source_wallet: $project_main_wallet, depth: 3)

// 2. Get all outgoing transactions from this cluster
$all_outgoing = []
FOR $wallet IN $team_cluster.wallets:
  $interactions = GET_WALLET_INTERACTIONS(wallet_address: $wallet, direction: "out", token: $token_mint)
  $all_outgoing = APPEND(array: $all_outgoing, items: $interactions.transactions)

// 3. Analyze the selling patterns
$selling_analysis = DETECT_PERIODIC_SELLING(transactions: $all_outgoing)

**Decision Point:** Determine if a slow rug pattern is present.
  BRANCH A ($selling_analysis.is_periodic && $selling_analysis.avg_size_usd < 5000):
    $pattern = "Probable Slow Rug"
    $explanation = "The team wallet cluster is making small, regular sales to CEX deposit addresses. The amounts are small enough to avoid causing panic but add up over time."
  BRANCH B ($selling_analysis.is_correlated_with_pumps):
    $pattern = "Opportunistic Slow Rug"
    $explanation = "The team is not selling on a fixed schedule, but consistently sells portions of their holdings into any significant market rally."
  BRANCH C (default):
    $pattern = "No Clear Slow Rug Pattern"
    $explanation = "Outgoing transactions are infrequent and do not correlate with price action, possibly indicating legitimate operational expenses."

**Action:**
RETURN {
  pattern_detected: $pattern,
  explanation: $explanation,
  selling_frequency: $selling_analysis.frequency,
  average_sell_amount_usd: $selling_analysis.avg_size_usd,
  total_sold_last_30d: $selling_analysis.total_sold_30d,
  destination_addresses: $selling_analysis.destinations,
  confidence: 85
}

---

## Q5410: "What on-chain patterns indicate that a token's social media hype on Twitter/Telegram is artificially inflated by bots?"

**Expected Plan:**

[TIME: ~90s] [COST: ~0.01 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - TWITTER_GET_MENTIONS (Social API)
  - GET_WALLET_CREATION_DATE (Forensics)
  - CORRELATE_SOCIAL_TO_ONCHAIN (Pattern Analysis)

**Main Branch:**
$token_cashtag = "$WIF"
$token_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"

// 1. Get social media mentions and on-chain buys
$mentions = TWITTER_GET_MENTIONS(query: $token_cashtag, time_period: "24h")
$buys = GET_ONCHAIN_BUYS(token_mint: $token_mint, time_period: "24h")

// 2. Correlate the two data streams
$correlation = CORRELATE_SOCIAL_TO_ONCHAIN(social_events: $mentions, onchain_events: $buys)

// 3. Analyze the wallets that buy immediately after social media posts
$buyer_wallets = MAP(collection: $correlation.correlated_buys, fn: buy => buy.buyer_address)
$wallet_ages = MAP(collection: $buyer_wallets, fn: wallet => GET_WALLET_CREATION_DATE(wallet_address: wallet))
$new_wallet_percentage = COUNT(FILTER($wallet_ages, age => age.days_old < 1)) / COUNT($wallet_ages)

**Decision Point:** Assess the authenticity of the social hype.
  BRANCH A ($correlation.correlation_score > 0.9 && $new_wallet_percentage > 0.5):
    $verdict = "Artificial Hype (Bot Activity)"
    $reason = "There is an extremely high correlation between Twitter posts and on-chain buys. Over 50% of the wallets buying immediately after posts are brand new, indicating they were created specifically for this purpose."
  BRANCH B ($correlation.correlation_score > 0.7):
    $verdict = "Suspicious Hype"
    $reason = "A strong correlation exists between social posts and buys, suggesting some level of coordination or bot activity, but wallet ages are mixed."
  BRANCH C (default):
    $verdict = "Organic Hype"
    $reason = "The correlation between social media mentions and on-chain buys is low and spread across a diverse range of established wallets."

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  correlation_score: $correlation.correlation_score,
  new_buyer_wallet_percentage: $new_wallet_percentage,
  confidence: 78
}

---

## Q5421: "Map a 'crypto laundering service' that accepts dirty funds and returns 'clean' crypto for a fee. Track all clients, calculate service volume, and find operator wallets."

**Expected Plan:**

[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - DETECT_LAUNDERING_SERVICE (Advanced Forensics)
  - BUILD_SERVICE_CLIENT_MAP (Analysis)

**Main Branch:**
$suspected_service_wallet = "..."

// Detect laundering service pattern
$service_analysis = DETECT_LAUNDERING_SERVICE(
  suspected_wallet: $suspected_service_wallet,
  time_period: "180d"
)

// Build client map
$clients = BUILD_SERVICE_CLIENT_MAP(
  service_wallet: $suspected_service_wallet,
  identify_dirty_sources: true
)

// Calculate service metrics
$total_volume = SUM(MAP($clients, c => c.total_sent_sol))
$estimated_fee = $total_volume * 0.05 // Assume 5% service fee

**Action:**
RETURN {
  investigation: "Crypto Laundering Service",
  service_wallet: $suspected_service_wallet,
  total_clients: COUNT($clients),
  total_volume_sol: $total_volume,
  estimated_fees_sol: $estimated_fee,
  operator_wallets: $service_analysis.operator_cluster,
  confidence: 81
}

---

## Q5422: "Detect 'insider trading' on token launches by identifying wallets that buy large amounts seconds after deployment but before public announcement."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - FIND_STEALTH_LAUNCH_INSIDERS (Forensics)

**Main Branch:**
$token_mint = "..."
$public_announcement_time = 1728432000

$launch_time = getAccountInfo($token_mint).data.created_at

// Find buyers between deployment and announcement
$insider_buyers = FIND_STEALTH_LAUNCH_INSIDERS(
  token: $token_mint,
  launch_time: $launch_time,
  announcement_time: $public_announcement_time,
  min_buy_amount_sol: 10
)

**Action:**
RETURN {
  investigation: "Insider Trading Detection",
  token: $token_mint,
  stealth_period_minutes: ($public_announcement_time - $launch_time) / 60,
  insider_buyers: COUNT($insider_buyers),
  total_insider_volume_sol: SUM(MAP($insider_buyers, i => i.buy_amount_sol)),
  insider_wallets: MAP($insider_buyers, i => i.wallet),
  confidence: 86
}

---

## Q5423: "Identify 'MEV protection failures' by finding transactions that paid for MEV protection but still got sandwiched. Calculate wasted protection fees."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - FIND_FAILED_MEV_PROTECTION (MEV Analysis)

**Main Branch:**
$time_period = "30d"

$failed_protections = FIND_FAILED_MEV_PROTECTION(
  period: $time_period,
  protection_services: ["Jito", "Flashbots", "Eden"]
)

$wasted_fees = SUM(MAP($failed_protections, f => f.protection_fee_sol))

**Action:**
RETURN {
  investigation: "MEV Protection Failures",
  failed_protections: COUNT($failed_protections),
  total_wasted_fees_sol: $wasted_fees,
  most_common_failure_reason: MOST_COMMON(MAP($failed_protections, f => f.failure_reason)),
  confidence: 84
}

---

## Q5424: "Map a 'Ponzi token' with referral rewards. Build the referral tree, calculate each level's profitability, and identify the collapse point."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - BUILD_REFERRAL_TREE (Analysis)
  - CALCULATE_PONZI_COLLAPSE_POINT (Financial Analysis)

**Main Branch:**
$ponzi_token = "..."

$referral_tree = BUILD_REFERRAL_TREE(
  token: $ponzi_token,
  root_wallet: getAccountInfo($ponzi_token).data.authority
)

$collapse_point = CALCULATE_PONZI_COLLAPSE_POINT(
  tree: $referral_tree,
  promised_returns: 10 // 10% monthly
)

**Action:**
RETURN {
  investigation: "Ponzi Token Referral Analysis",
  token: $ponzi_token,
  total_participants: COUNT($referral_tree.all_participants),
  pyramid_levels: COUNT($referral_tree.levels),
  collapse_date: $collapse_point.estimated_collapse_date,
  winners: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit > 0)),
  losers: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit < 0)),
  confidence: 87
}

---

## Q5425: "Detect 'fake volume bots' by analyzing trading patterns: repetitive amounts, perfect timing intervals, circular fund flows between limited wallets."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - DETECT_FAKE_VOLUME_BOTS (Pattern Analysis)

**Main Branch:**
$token_mint = "..."

$fake_volume = DETECT_FAKE_VOLUME_BOTS(
  token: $token_mint,
  detection_methods: ["repetitive_amounts", "timing_regularity", "circular_flows"]
)

$real_volume = GET_TOKEN_VOLUME(token: $token_mint, period: "24h") - $fake_volume.estimated_fake_volume_sol
$fake_percentage = ($fake_volume.estimated_fake_volume_sol / GET_TOKEN_VOLUME(token: $token_mint, period: "24h")) * 100

**Action:**
RETURN {
  investigation: "Fake Volume Bot Detection",
  token: $token_mint,
  fake_volume_bots: COUNT($fake_volume.detected_bots),
  estimated_fake_volume_sol: $fake_volume.estimated_fake_volume_sol,
  estimated_real_volume_sol: $real_volume,
  fake_volume_percentage: $fake_percentage,
  bot_wallets: $fake_volume.bot_wallets,
  confidence: 88
}

---

## Q5426: "Investigate a 'liquidity migration scam' where devs drain LP from one DEX, promise to migrate to another, but never do. Track fund flow."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TRACE_LIQUIDITY_REMOVAL (Analysis)

**Main Branch:**
$token_mint = "..."
$old_pool = "..." // Raydium pool
$promised_new_pool = "..." // Orca pool (never created)

$lp_removal = TRACE_LIQUIDITY_REMOVAL(
  token: $token_mint,
  pool: $old_pool
)

// Check if new pool was ever created
$new_pool_exists = getAccountInfo($promised_new_pool) != null

// Trace removed funds
$fund_trace = TRACE_FUNDS(
  start: $lp_removal.remover_wallet,
  depth: 5
)

**Action:**
RETURN {
  investigation: "Liquidity Migration Scam",
  token: $token_mint,
  old_pool: $old_pool,
  lp_removed_sol: $lp_removal.total_removed_sol,
  new_pool_created: $new_pool_exists,
  removed_funds_destination: $fund_trace.final_addresses,
  is_confirmed_scam: !$new_pool_exists,
  confidence: 85
}

---

## Q5427: "Map 'connected scammer networks' by finding common funding sources, shared infrastructure wallets, or coordinated launch timing across multiple rugs."

**Expected Plan:**

[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - BUILD_SCAMMER_NETWORK_MAP (Advanced Forensics)

**Main Branch:**
$known_rugs = [...] // List of known rug pulls

$network_map = BUILD_SCAMMER_NETWORK_MAP(
  scam_projects: $known_rugs,
  link_types: ["funding_source", "infrastructure_wallets", "launch_timing"]
)

**Action:**
RETURN {
  investigation: "Connected Scammer Networks",
  rugs_analyzed: COUNT($known_rugs),
  distinct_networks: COUNT($network_map.detected_networks),
  largest_network_size: MAX(MAP($network_map.detected_networks, n => COUNT(n.projects))),
  network_details: $network_map.detected_networks,
  confidence: 83
}

---

## Q5428: "Detect 'pump and dump discord groups' by correlating coordinated buying with Discord/Telegram message timestamps and member wallets."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - CORRELATE_SOCIAL_WITH_ONCHAIN (Cross-Platform Analysis)

**Main Branch:**
$suspected_tokens = [...]
$discord_timestamps = [...] // Call timestamps from known pump groups

$correlation = CORRELATE_SOCIAL_WITH_ONCHAIN(
  tokens: $suspected_tokens,
  social_signals: $discord_timestamps,
  correlation_window: 300 // 5 minutes
)

**Action:**
RETURN {
  investigation: "Discord Pump Group Detection",
  tokens_analyzed: COUNT($suspected_tokens),
  strong_correlations: COUNT(FILTER($correlation, c => c.correlation_strength > 0.8)),
  pump_group_members: $correlation.suspected_member_wallets,
  confidence: 79
}

---

## Q5429: "Identify 'token lockup bypass exploits' where teams find ways to access supposedly locked tokens early. Trace the exploit path."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - DETECT_LOCKUP_BYPASS (Security Analysis)

**Main Branch:**
$token_mint = "..."
$lockup_contract = "..."

$bypass_detection = DETECT_LOCKUP_BYPASS(
  token: $token_mint,
  lockup_contract: $lockup_contract
)

**Action:**
RETURN {
  investigation: "Token Lockup Bypass Detection",
  token: $token_mint,
  bypass_detected: $bypass_detection.bypass_found,
  bypass_method: $bypass_detection.exploit_method,
  tokens_unlocked_early: $bypass_detection.early_unlock_amount,
  bypass_transactions: $bypass_detection.exploit_transactions,
  confidence: 86
}

---

## Q5430: "Map 'validator collusion' where multiple validators coordinate to censor specific transactions or prioritize their own MEV extraction."

**Expected Plan:**

[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - DETECT_VALIDATOR_COLLUSION (Advanced Forensics)

**Main Branch:**
$suspected_validators = [...]
$time_period = {start: 1727222400, end: 1728432000}

$collusion_analysis = DETECT_VALIDATOR_COLLUSION(
  validators: $suspected_validators,
  period: $time_period,
  detection_methods: ["censorship_patterns", "coordinated_mev", "shared_infrastructure"]
)

**Action:**
RETURN {
  investigation: "Validator Collusion Detection",
  validators_analyzed: COUNT($suspected_validators),
  collusion_detected: $collusion_analysis.collusion_found,
  colluding_validators: $collusion_analysis.colluding_group,
  evidence: $collusion_analysis.evidence,
  estimated_unfair_mev_sol: $collusion_analysis.total_coordinated_mev_sol,
  confidence: 82
}


---

## Q5431: "Detect 'spoofed LP tokens' where scammers create fake LP tokens that look identical to real ones to trick users into depositing value."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: VERIFY_LP_TOKEN_AUTHENTICITY, DETECT_FAKE_LP_TOKENS

**Main Branch:**
$suspected_lp_token = "..."
$real_pool_address = "..."
$verification = VERIFY_LP_TOKEN_AUTHENTICITY(lp_token: $suspected_lp_token, known_pool: $real_pool_address)
IF !$verification.is_authentic:
  $victims = FIND_FAKE_LP_TOKEN_VICTIMS(fake_token: $suspected_lp_token)

**Action:**
RETURN {investigation: "Spoofed LP Token Scam", is_fake: !$verification.is_authentic, victim_count: COUNT($victims), total_loss_sol: SUM(MAP($victims, v => v.loss)), confidence: 89}

---

## Q5432: "Map a 'DAO treasury raid' where malicious proposal drains funds. Trace the proposal creation, voting patterns, and fund destinations."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_DAO_PROPOSAL, TRACE_TREASURY_FUNDS

**Main Branch:**
$dao_program = "..."
$malicious_proposal = "..."
$proposal_analysis = ANALYZE_DAO_PROPOSAL(dao: $dao_program, proposal: $malicious_proposal)
$fund_trace = TRACE_TREASURY_FUNDS(source: $proposal_analysis.treasury_address, drain_tx: $proposal_analysis.execution_tx)

**Action:**
RETURN {investigation: "DAO Treasury Raid", drained_amount_sol: $proposal_analysis.amount_drained, voting_manipulation: $proposal_analysis.suspicious_votes, fund_destinations: $fund_trace.final_addresses, confidence: 84}

---

## Q5433: "Identify 'circular trading bots' that create artificial volume by trading between their own wallets with no real market impact."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: DETECT_CIRCULAR_TRADING, BUILD_BOT_CLUSTER

**Main Branch:**
$token = "..."
$trades = GET_TOKEN_TRADES(token: $token, period: "24h")
$circular = DETECT_CIRCULAR_TRADING(trades: $trades, max_circle_size: 10)
$bot_cluster = BUILD_BOT_CLUSTER(circular_traders: $circular.involved_wallets)

**Action:**
RETURN {investigation: "Circular Trading Bot Network", bot_count: COUNT($bot_cluster.wallets), fake_volume_sol: $circular.total_circular_volume, real_volume_pct: $circular.real_volume_percentage, confidence: 87}

---

## Q5434: "Investigate a 'bridge exploit' where attacker mints unlimited tokens by exploiting cross-chain message verification."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_BRIDGE_EXPLOIT, TRACE_MINTED_TOKENS

**Main Branch:**
$bridge_contract = "..."
$exploit_tx = "..."
$analysis = ANALYZE_BRIDGE_EXPLOIT(bridge: $bridge_contract, exploit: $exploit_tx)
$minted_trace = TRACE_MINTED_TOKENS(source: $analysis.attacker_wallet, token: $analysis.exploited_token)

**Action:**
RETURN {investigation: "Bridge Exploit - Unlimited Mint", exploit_method: $analysis.vulnerability_type, tokens_minted: $analysis.mint_amount, attacker_profit_usd: $analysis.realized_profit, confidence: 81}

---

## Q5435: "Detect 'governance token rental markets' where users rent voting power, potentially enabling vote manipulation."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: FIND_TOKEN_RENTAL_PLATFORMS, ANALYZE_RENTAL_VOTING_IMPACT

**Main Branch:**
$gov_token = "..."
$rentals = FIND_TOKEN_RENTAL_PLATFORMS(token: $gov_token)
$voting_impact = ANALYZE_RENTAL_VOTING_IMPACT(rentals: $rentals, proposals: GET_RECENT_PROPOSALS($gov_token))

**Action:**
RETURN {investigation: "Governance Token Rental Market", rental_volume_daily: $rentals.avg_daily_volume, proposals_affected: COUNT($voting_impact.influenced_proposals), manipulation_risk: $voting_impact.risk_score, confidence: 85}

---

## Q5436: "Map a 'MEV sandwich cartel' where multiple bots coordinate to avoid competing on the same victim transactions."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: DETECT_MEV_BOT_COORDINATION, ANALYZE_VICTIM_ALLOCATION

**Main Branch:**
$mev_bots = FIND_SANDWICH_BOTS(period: "7d", min_profit: 10)
$coordination = DETECT_MEV_BOT_COORDINATION(bots: $mev_bots)
$allocation = ANALYZE_VICTIM_ALLOCATION(coordinated_bots: $coordination.cartel_members)

**Action:**
RETURN {investigation: "MEV Sandwich Cartel", cartel_size: COUNT($coordination.cartel_members), coordination_evidence: $coordination.evidence_score, profit_sharing_detected: $allocation.shows_coordination, confidence: 82}

---

## Q5437: "Investigate 'NFT metadata poisoning' where scammers inject malicious URLs into NFT metadata to phish holders."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: SCAN_NFT_METADATA_URLS, DETECT_PHISHING_LINKS

**Main Branch:**
$collection = "..."
$all_nfts = GET_COLLECTION_NFTS(collection: $collection)
$url_scan = SCAN_NFT_METADATA_URLS(nfts: $all_nfts)
$malicious = DETECT_PHISHING_LINKS(urls: $url_scan.all_urls)

**Action:**
RETURN {investigation: "NFT Metadata Poisoning", poisoned_nfts: COUNT($malicious.nfts_with_bad_urls), phishing_domains: $malicious.malicious_domains, holder_risk: $malicious.estimated_victims, confidence: 90}

---

## Q5438: "Detect 'stablecoin depeg manipulation' where whales intentionally cause depegs to profit from panic selling."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: ANALYZE_DEPEG_EVENT, DETECT_MANIPULATION_TRADING

**Main Branch:**
$stablecoin = "..."
$depeg_timestamp = 1728432000
$depeg_analysis = ANALYZE_DEPEG_EVENT(token: $stablecoin, timestamp: $depeg_timestamp)
$manipulation = DETECT_MANIPULATION_TRADING(event: $depeg_analysis)

**Action:**
RETURN {investigation: "Stablecoin Depeg Manipulation", manipulation_detected: $manipulation.intentional_probability > 0.7, whale_traders: $manipulation.suspicious_wallets, manipulator_profit_usd: $manipulation.estimated_profit, confidence: 79}

---

## Q5439: "Map 'validator sandwich attack coordination' where validators collude to sandwich user transactions in their own blocks."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_SANDWICHES, DETECT_CROSS_VALIDATOR_COLLUSION

**Main Branch:**
$validators = [...]
$sandwiches = ANALYZE_VALIDATOR_SANDWICHES(validators: $validators, period: "14d")
$collusion = DETECT_CROSS_VALIDATOR_COLLUSION(sandwich_events: $sandwiches)

**Action:**
RETURN {investigation: "Validator Sandwich Collusion", colluding_validators: COUNT($collusion.coordinated_validators), total_extracted_sol: $collusion.total_mev, victim_transactions: COUNT($sandwiches.all_victims), confidence: 80}

---

## Q5440: "Investigate 'pump-and-dump as a service' platforms that coordinate paid pump campaigns for tokens."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: IDENTIFY_PUMP_SERVICE_WALLETS, MAP_PUMP_CAMPAIGNS

**Main Branch:**
$service_operator = "..."
$campaigns = MAP_PUMP_CAMPAIGNS(operator: $service_operator, period: "60d")
$client_analysis = ANALYZE_PUMP_CLIENTS(campaigns: $campaigns)

**Action:**
RETURN {investigation: "Pump-as-a-Service Platform", total_campaigns: COUNT($campaigns), client_count: COUNT($client_analysis.unique_clients), service_revenue_sol: $campaigns.total_fees_collected, confidence: 83}

---

## Q5441: "Detect 'fake liquidity lock' where scammers lock liquidity in a contract they control and can withdraw anytime."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: ANALYZE_LIQUIDITY_LOCK, DETECT_BACKDOOR_ACCESS

**Main Branch:**
$lock_contract = "..."
$lock_analysis = ANALYZE_LIQUIDITY_LOCK(contract: $lock_contract)
$backdoors = DETECT_BACKDOOR_ACCESS(contract: $lock_contract, lock_data: $lock_analysis)

**Action:**
RETURN {investigation: "Fake Liquidity Lock", is_fake_lock: $backdoors.can_bypass_lock, bypass_method: $backdoors.method, locked_amount_sol: $lock_analysis.locked_value, confidence: 88}

---

## Q5442: "Map 'cNFT minting exploits' where attackers mint compressed NFTs beyond the tree capacity by exploiting Merkle tree verification."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_CNFT_TREE, DETECT_OVERMINT_EXPLOIT

**Main Branch:**
$tree_address = "..."
$tree_data = ANALYZE_CNFT_TREE(tree: $tree_address)
$overmint = DETECT_OVERMINT_EXPLOIT(tree: $tree_address, expected_capacity: $tree_data.max_capacity)

**Action:**
RETURN {investigation: "cNFT Overmint Exploit", exploit_detected: $overmint.detected, extra_mints: $overmint.mints_beyond_capacity, attacker_wallet: $overmint.exploiter, confidence: 81}

---

## Q5443: "Investigate 'prediction market oracle manipulation' where insiders with early knowledge manipulate market odds before public announcement."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library: ANALYZE_PREDICTION_MARKET, DETECT_INSIDER_TRADING

**Main Branch:**
$market_address = "..."
$event_result = "..."
$market_analysis = ANALYZE_PREDICTION_MARKET(market: $market_address, result: $event_result)
$insider_trades = DETECT_INSIDER_TRADING(trades: $market_analysis.pre_result_trades, result_time: $market_analysis.result_timestamp)

**Action:**
RETURN {investigation: "Prediction Market Insider Trading", suspicious_traders: COUNT($insider_trades.likely_insiders), insider_profit_sol: $insider_trades.total_profit, trade_timing_correlation: $insider_trades.correlation_score, confidence: 76}

---

## Q5444: "Detect 'token supply inflation' attacks where minter role is exploited to create unlimited tokens and dump on market."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: ANALYZE_TOKEN_SUPPLY_CHANGES, DETECT_UNAUTHORIZED_MINTS

**Main Branch:**
$token_mint = "..."
$supply_history = ANALYZE_TOKEN_SUPPLY_CHANGES(mint: $token_mint, period: "30d")
$unauthorized = DETECT_UNAUTHORIZED_MINTS(supply_changes: $supply_history)

**Action:**
RETURN {investigation: "Token Supply Inflation Attack", inflation_detected: $unauthorized.detected, tokens_minted: $unauthorized.total_minted, dumped_amount: $unauthorized.sold_amount, price_impact_pct: $unauthorized.price_drop_pct, confidence: 91}

---

## Q5445: "Map a 'liquidation bot frontrunning cartel' where bots coordinate to avoid competing on the same liquidation opportunities."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: FIND_LIQUIDATION_BOTS, DETECT_BOT_COORDINATION

**Main Branch:**
$lending_protocol = "..."
$bots = FIND_LIQUIDATION_BOTS(protocol: $lending_protocol, period: "14d")
$coordination = DETECT_BOT_COORDINATION(bots: $bots, activity_type: "liquidations")

**Action:**
RETURN {investigation: "Liquidation Bot Cartel", cartel_size: COUNT($coordination.coordinated_bots), non_competitive_rate: $coordination.overlap_rate, shared_infrastructure: $coordination.common_funding, confidence: 82}

---

## Q5446: "Investigate 'fake airdrop claim' scams that require users to approve unlimited token spending to claim worthless tokens."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: ANALYZE_AIRDROP_CONTRACT, DETECT_APPROVAL_DRAIN_PATTERN

**Main Branch:**
$airdrop_contract = "..."
$contract_analysis = ANALYZE_AIRDROP_CONTRACT(contract: $airdrop_contract)
$drain_pattern = DETECT_APPROVAL_DRAIN_PATTERN(contract: $airdrop_contract, claimers: GET_AIRDROP_CLAIMERS($airdrop_contract))

**Action:**
RETURN {investigation: "Fake Airdrop Drain Scam", is_drainer: $drain_pattern.is_malicious, victims_drained: COUNT($drain_pattern.drained_wallets), total_stolen_usd: $drain_pattern.total_stolen, confidence: 89}

---

## Q5447: "Detect 'validator censorship' where specific validators consistently exclude certain transactions or protocols from their blocks."

**Expected Plan:**
[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_BLOCK_CONTENT, DETECT_CENSORSHIP_PATTERNS

**Main Branch:**
$validator = "..."
$blocks = GET_VALIDATOR_BLOCKS(validator: $validator, period: "30d")
$content_analysis = ANALYZE_VALIDATOR_BLOCK_CONTENT(blocks: $blocks)
$censorship = DETECT_CENSORSHIP_PATTERNS(content: $content_analysis)

**Action:**
RETURN {investigation: "Validator Censorship", censorship_detected: $censorship.is_censoring, censored_protocols: $censorship.excluded_programs, censorship_rate_pct: $censorship.exclusion_rate, confidence: 78}

---

## Q5448: "Map 'cross-DEX arbitrage bot networks' that share infrastructure and coordinate to reduce gas competition."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: FIND_ARBITRAGE_BOTS, BUILD_BOT_NETWORK

**Main Branch:**
$dexes = ["Raydium", "Orca", "Jupiter"]
$arb_bots = FIND_ARBITRAGE_BOTS(dexes: $dexes, period: "14d", min_profit: 5)
$network = BUILD_BOT_NETWORK(bots: $arb_bots, link_by: ["funding", "timing", "strategy"])

**Action:**
RETURN {investigation: "Arbitrage Bot Network", network_clusters: COUNT($network.clusters), largest_cluster: MAX(MAP($network.clusters, c => COUNT(c.bots))), cluster_profit_sol: SUM(MAP($network.clusters, c => c.total_profit)), confidence: 83}

---

## Q5449: "Investigate 'mempool frontrunning on Solana' by analyzing Jito bundle patterns and identifying systematic frontrunners."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_JITO_BUNDLES, DETECT_SYSTEMATIC_FRONTRUNNING

**Main Branch:**
$time_period = {start: 1728000000, end: 1728432000}
$bundles = ANALYZE_JITO_BUNDLES(period: $time_period)
$frontrunners = DETECT_SYSTEMATIC_FRONTRUNNING(bundles: $bundles)

**Action:**
RETURN {investigation: "Jito Bundle Frontrunning", systematic_frontrunners: COUNT($frontrunners.serial_frontrunners), total_frontrun_profit_sol: $frontrunners.total_profit, avg_tip_sol: $frontrunners.avg_jito_tip, confidence: 84}

---

## Q5450: "Detect 'rug pull via upgrade authority' where devs use program upgrade authority to inject malicious code after launch."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: MONITOR_PROGRAM_UPGRADES, DETECT_MALICIOUS_UPGRADES

**Main Branch:**
$program_id = "..."
$upgrades = MONITOR_PROGRAM_UPGRADES(program: $program_id)
$malicious = DETECT_MALICIOUS_UPGRADES(upgrades: $upgrades, check_for: ["drain_functions", "backdoors", "authority_changes"])

**Action:**
RETURN {investigation: "Malicious Program Upgrade", malicious_upgrade_detected: COUNT($malicious.suspicious_upgrades) > 0, upgrade_type: $malicious.suspicious_upgrades[0].malicious_changes, funds_at_risk_sol: $malicious.tvl_at_upgrade, confidence: 87}


## Q5451: "Identify wallets that consistently front-run trades on a specific DEX."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, FILTER, DETECT_PATTERNS

**Main Branch:**
$dex_program_id = "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin" // Serum DEX example
$signatures = getSignaturesForAddress(address: $dex_program_id, limit: 1000)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

// Detect front-running patterns
$front_run_patterns = DETECT_PATTERNS(data: $transactions, pattern: "front_run")

// Extract front-runner wallets
$front_runners = MAP(collection: $front_run_patterns, fn: p => p.front_runner_wallet)
$unique_front_runners = UNIQUE($front_runners)

**Action:**
RETURN {
  front_runner_wallets: $unique_front_runners,
  confidence: 85,
  sample_size: COUNT($transactions)
}

## Q5452: "Find the original funding source for a known scam wallet."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: TRACE_FUNDS

**Main Branch:**
$scam_wallet = "..."
$trace = TRACE_FUNDS(start_address: $scam_wallet, depth: 10, direction: "inbound")

// The first transaction in the inbound trace is the origin
$funding_source = $trace.transactions.first().signer

**Action:**
RETURN {
  scam_wallet: $scam_wallet,
  original_funding_source: $funding_source,
  trace: $trace,
  confidence: 90
}

## Q5453: "List all tokens held by a wallet that are not on any major token list."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.02 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getTokenAccountsByOwner, SEARCH_WEB, MAP, FILTER

**Main Branch:**
$wallet_address = "..."
$token_accounts = getTokenAccountsByOwner(owner: $wallet_address)
$token_mints = MAP(collection: $token_accounts, fn: acc => acc.mint)

// Check each mint against a known list (simulated via web search)
$major_token_lists = SEARCH_WEB(query: "Solana major token lists")
$unlisted_tokens = FILTER(collection: $token_mints, predicate: mint => {
  RETURN !IS_IN_LIST(mint, $major_token_lists)
})

**Action:**
RETURN {
  wallet: $wallet_address,
  unlisted_tokens: $unlisted_tokens,
  confidence: 95
}

## Q5454: "Detect if a token has a 'honeypot' mechanism where tokens can be bought but not sold."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.1 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: simulateTransaction, getTransaction, MAP, FILTER

**Main Branch:**
$token_mint = "..."
$test_wallet = create_temp_wallet()

// Simulate a buy transaction
$buy_tx = createSwapTx(from: "SOL", to: $token_mint, amount: 0.1, wallet: $test_wallet)
$sim_buy = simulateTransaction($buy_tx)

GUARD $sim_buy.err == null ELSE RETURN { verdict: "Cannot even buy the token." }

// Simulate a sell transaction
$sell_tx = createSwapTx(from: $token_mint, to: "SOL", amount: "all", wallet: $test_wallet)
$sim_sell = simulateTransaction($sell_tx)

**Decision Point:** Analyze simulation results
  BRANCH A ($sim_buy.err == null && $sim_sell.err != null):
    $verdict = "Honeypot detected. Sell transaction failed."
    $reason = $sim_sell.err
  BRANCH B ($sim_buy.err == null && $sim_sell.err == null):
    $verdict = "Not a honeypot. Buy and sell simulations were successful."
  BRANCH C (default):
    $verdict = "Analysis inconclusive."
    $reason = "Could not simulate buy or other error."

**Action:**
RETURN {
  token_mint: $token_mint,
  verdict: $verdict,
  reason: $reason,
  confidence: 80
}

## Q5455: "Find accounts that received tokens from multiple different airdrops."

**Expected Plan:**
[TIME: ~15m] [COST: ~0.2 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, FLATTEN, GROUP_BY

**Main Branch:**
$airdrop_contracts = ["...", "...", "..."] // List of known airdrop contracts

$all_recipients = MAP(collection: $airdrop_contracts, fn: contract => {
  $signatures = getSignaturesForAddress(address: contract, limit: 1000)
  $txs = MAP(collection: $signatures, fn: s => getTransaction(s.signature))
  RETURN MAP($txs, tx => extractAirdropRecipient(tx))
})

$flat_recipients = FLATTEN($all_recipients)
$grouped_recipients = GROUP_BY(collection: $flat_recipients, key: r => r)

$multi_airdrop_hunters = FILTER(collection: $grouped_recipients, predicate: group => COUNT(group.items) > 1)

**Action:**
RETURN {
  airdrop_hunters: MAP($multi_airdrop_hunters, g => g.key),
  count: COUNT($multi_airdrop_hunters),
  confidence: 85
}

## Q5456: "Analyze the distribution of a token's supply across its top 100 holders."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.01 SOL] [CONFIDENCE: 99%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, getTokenSupply

**Main Branch:**
$token_mint = "..."
$top_100_holders = getTokenLargestAccounts(mint: $token_mint, limit: 100)
$total_supply = getTokenSupply(mint: $token_mint).uiAmount

$top_100_supply = SUM(MAP($top_100_holders, h => h.uiAmount))
$percentage_held_by_top_100 = ($top_100_supply / $total_supply) * 100

**Action:**
RETURN {
  token_mint: $token_mint,
  total_supply: $total_supply,
  top_100_holders_supply: $top_100_supply,
  percentage_held_by_top_100: $percentage_held_by_top_100,
  top_holders: $top_100_holders,
  confidence: 99
}

## Q5457: "Identify if a wallet has interacted with any known malicious dApps or contracts."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, SEARCH_WEB, MAP, FILTER

**Main Branch:**
$wallet_address = "..."
$known_malicious_list = SEARCH_WEB(query: "list of known malicious solana programs")

$signatures = getSignaturesForAddress(address: $wallet_address, limit: 100)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

$interacted_programs = FLATTEN(MAP($transactions, tx => tx.instructions.programId))
$unique_interacted = UNIQUE($interacted_programs)

$malicious_interactions = FILTER(collection: $unique_interacted, predicate: program => {
  RETURN IS_IN_LIST(program, $known_malicious_list)
})

**Action:**
RETURN {
  wallet: $wallet_address,
  has_interacted_with_malicious: COUNT($malicious_interactions) > 0,
  malicious_programs_interacted_with: $malicious_interactions,
  confidence: 80,
  caveats: ["Depends on the completeness of the malicious programs list."]
}

## Q5458: "Find the transaction where a specific NFT was minted."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, FIND

**Main Branch:**
$nft_mint_address = "..."
// The mint address is an account, so we look for signatures for it.
$signatures = getSignaturesForAddress(address: $nft_mint_address, limit: 100)

// The minting transaction is usually the first one.
$first_tx_sig = $signatures.last().signature
$mint_tx = getTransaction($first_tx_sig)

// Verify it's a minting instruction
$mint_instruction = FIND(collection: $mint_tx.instructions, predicate: ix => isMintToInstruction(ix))

**Decision Point:** Check if it's the correct mint tx
  BRANCH A ($mint_instruction != null):
    $result = $mint_tx
  BRANCH B (default):
    // This would require a more complex search if the first tx isn't it
    $result = "Could not definitively find mint transaction in first 100 signatures."

**Action:**
RETURN {
  nft_mint: $nft_mint_address,
  mint_transaction: $result,
  confidence: 95
}

## Q5459: "Calculate the total fees paid by a wallet in a given time period."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.1 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, SUM

**Main Branch:**
$wallet_address = "..."
// This might require pagination for an active wallet
$signatures = getSignaturesForAddress(address: $wallet_address, limit: 1000)
$transactions = MAP(collection: $signatures, fn: s => getTransaction(s.signature))

$fees = MAP(collection: $transactions, fn: tx => tx.meta.fee)
$total_fees_lamports = SUM($fees)
$total_fees_sol = $total_fees_lamports / 1000000000

**Action:**
RETURN {
  wallet: $wallet_address,
  transaction_count: COUNT($transactions),
  total_fees_lamports: $total_fees_lamports,
  total_fees_sol: $total_fees_sol,
  confidence: 98
}

## Q5460: "Check if a program is upgradeable and who the upgrade authority is."

**Expected Plan:**
[TIME: ~1m] [COST: ~0.01 SOL] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library: getAccountInfo

**Main Branch:**
$program_id = "..."
$account_info = getAccountInfo($program_id)

// For an upgradeable program, the owner is the BPF Upgradeable Loader
$bpf_loader_upgradeable = "BPFLoaderUpgradeab1e11111111111111111111111"
$is_upgradeable = $account_info.owner == $bpf_loader_upgradeable

$upgrade_authority = null
IF $is_upgradeable THEN
  // The program data account holds the upgrade authority
  $program_data_address = findProgramDataAddress($program_id)
  $program_data_info = getAccountInfo($program_data_address)
  $upgrade_authority = parseUpgradeAuthority($program_data_info.data)

**Action:**
RETURN {
  program_id: $program_id,
  is_upgradeable: $is_upgradeable,
  upgrade_authority: $upgrade_authority,
  confidence: 100
}

---

## Q5461: "Detect recurring wallet-to-wallet transfers that may indicate money laundering patterns."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.02 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, GROUP_BY, DETECT_RECURRING_TRANSFERS

**Main Branch:**
$wallet = "..."
$signatures = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $signatures, fn: s => getTransaction(s.signature))
$recurring = DETECT_RECURRING_TRANSFERS(transactions: $txs, min_repeats: 5)

**Action:**
RETURN {wallet: $wallet, recurring_patterns: $recurring, confidence: 92}

---

## Q5462: "Identify wallets that frequently interact with newly deployed programs within 24 hours of deployment."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, FIND_NEW_PROGRAMS, MAP, FILTER

**Main Branch:**
$wallet = "..."
$new_programs = FIND_NEW_PROGRAMS(period: "24h")
$interactions = []
FOR $program IN $new_programs:
  $sigs = getSignaturesForAddress(address: $program, limit: 1000)
  $txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
  IF ANY($txs, tx => tx.message.accountKeys[0] == $wallet):
    $interactions = APPEND($interactions, $program)

**Action:**
RETURN {wallet: $wallet, new_program_interactions: $interactions, confidence: 90}

---

## Q5463: "Detect flash loan attack patterns by analyzing rapid borrow/repay cycles in a single block."

**Expected Plan:**
[TIME: ~4m] [COST: ~0.04 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: getBlock, MAP, DETECT_FLASH_LOAN_CYCLES

**Main Branch:**
$slot = ...
$block = getBlock(slot: $slot)
$flash_loan_patterns = DETECT_FLASH_LOAN_CYCLES(block: $block, min_cycles: 2)

**Action:**
RETURN {slot: $slot, flash_loan_patterns: $flash_loan_patterns, confidence: 93}

---

## Q5464: "Find wallets that repeatedly provide and remove liquidity from the same pool within short intervals."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_LIQUIDITY_CHURN

**Main Branch:**
$pool = "..."
$sigs = getSignaturesForAddress(address: $pool, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$churners = DETECT_LIQUIDITY_CHURN(transactions: $txs, min_cycles: 3, max_interval_min: 60)

**Action:**
RETURN {pool: $pool, churn_wallets: $churners, confidence: 91}

---

## Q5465: "Detect sandwich attack patterns in DEX swap transactions."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getBlock, MAP, DETECT_SANDWICH_ATTACKS

**Main Branch:**
$slot = ...
$block = getBlock(slot: $slot)
$sandwiches = DETECT_SANDWICH_ATTACKS(block: $block)

**Action:**
RETURN {slot: $slot, sandwich_attacks: $sandwiches, confidence: 95}

---

## Q5466: "Identify wallets that consistently frontrun large trades on Raydium."

**Expected Plan:**
[TIME: ~4m] [COST: ~0.04 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: getBlock, MAP, DETECT_FRONTRUNNING

**Main Branch:**
$slot = ...
$block = getBlock(slot: $slot)
$frontrunners = DETECT_FRONTRUNNING(block: $block, min_trade_size: 10000)

**Action:**
RETURN {slot: $slot, frontrunners: $frontrunners, confidence: 92}

---

## Q5467: "Detect wallets that frequently interact with known scam tokens."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.02 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, FILTER, KNOWN_SCAM_TOKENS

**Main Branch:**
$wallet = "..."
$scam_tokens = KNOWN_SCAM_TOKENS()
$interactions = []
FOR $token IN $scam_tokens:
  $sigs = getSignaturesForAddress(address: $token, limit: 1000)
  $txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
  IF ANY($txs, tx => tx.message.accountKeys[0] == $wallet):
    $interactions = APPEND($interactions, $token)

**Action:**
RETURN {wallet: $wallet, scam_token_interactions: $interactions, confidence: 90}

---

## Q5468: "Find wallets that receive airdrops from multiple unrelated projects in a short time window."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, GROUP_BY, DETECT_AIRDROP_PATTERNS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$airdrop_patterns = DETECT_AIRDROP_PATTERNS(transactions: $txs, min_projects: 3, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, airdrop_patterns: $airdrop_patterns, confidence: 91}

---

## Q5469: "Detect wallets that split large incoming transfers into many small outgoing transfers (smurfing)."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.02 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_SMURFING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$smurfing = DETECT_SMURFING(transactions: $txs, min_split: 5)

**Action:**
RETURN {wallet: $wallet, smurfing_patterns: $smurfing, confidence: 93}

---

## Q5470: "Identify wallets that repeatedly interact with the same set of addresses (potential botnets)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, GROUP_BY, DETECT_BOTNETS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$botnet_patterns = DETECT_BOTNETS(transactions: $txs, min_peers: 5, min_interactions: 10)

**Action:**
RETURN {wallet: $wallet, botnet_patterns: $botnet_patterns, confidence: 90}

---

## Q5471: "Detect wallets that frequently participate in pump-and-dump schemes."

**Expected Plan:**
[TIME: ~4m] [COST: ~0.04 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_PUMP_AND_DUMP

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$pump_and_dump = DETECT_PUMP_AND_DUMP(transactions: $txs)

**Action:**
RETURN {wallet: $wallet, pump_and_dump_patterns: $pump_and_dump, confidence: 92}

---

## Q5472: "Identify wallets that interact with multiple DEXes in rapid succession (arbitrage bots)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_ARBITRAGE_BOTS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$arbitrage_patterns = DETECT_ARBITRAGE_BOTS(transactions: $txs, min_dexes: 2, max_window_min: 10)

**Action:**
RETURN {wallet: $wallet, arbitrage_patterns: $arbitrage_patterns, confidence: 91}

---

## Q5473: "Detect wallets that frequently interact with bridge contracts (potential cross-chain arbitrage)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_BRIDGE_ARBITRAGE

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$bridge_arbitrage = DETECT_BRIDGE_ARBITRAGE(transactions: $txs)

**Action:**
RETURN {wallet: $wallet, bridge_arbitrage_patterns: $bridge_arbitrage, confidence: 90}

---

## Q5474: "Identify wallets that receive funds from many unrelated sources in a short period (potential mixers)."

**Expected Plan:**
[TIME: ~2m] [COST: ~0.02 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_MIXER_USAGE

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$mixer_usage = DETECT_MIXER_USAGE(transactions: $txs, min_sources: 10, max_window_hr: 1)

**Action:**
RETURN {wallet: $wallet, mixer_usage_patterns: $mixer_usage, confidence: 91}

---

## Q5475: "Detect wallets that repeatedly interact with the same NFT minting contract (potential NFT botting)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_NFT_BOTTING

**Main Branch:**
$wallet = "..."
$nft_contract = "..."
$sigs = getSignaturesForAddress(address: $nft_contract, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$nft_botting = DETECT_NFT_BOTTING(transactions: $txs, wallet: $wallet)

**Action:**
RETURN {wallet: $wallet, nft_botting_patterns: $nft_botting, confidence: 90}

---

## Q5476: "Identify wallets that frequently interact with governance contracts just before major proposals."

**Expected Plan:**
[TIME: ~4m] [COST: ~0.04 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_GOVERNANCE_ACTIVITY

**Main Branch:**
$wallet = "..."
$governance_contract = "..."
$sigs = getSignaturesForAddress(address: $governance_contract, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$gov_activity = DETECT_GOVERNANCE_ACTIVITY(transactions: $txs, wallet: $wallet, pre_proposal_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, governance_activity_patterns: $gov_activity, confidence: 91}

---

## Q5477: "Detect wallets that interact with multiple meme tokens in a short period (potential airdrop farmers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_AIRDROP_FARMING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$airdrop_farming = DETECT_AIRDROP_FARMING(transactions: $txs, min_tokens: 5, max_window_hr: 48)

**Action:**
RETURN {wallet: $wallet, airdrop_farming_patterns: $airdrop_farming, confidence: 90}

---

## Q5478: "Detect wallets that frequently interact with the same validator vote account (potential collusion)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_VALIDATOR_COLLUSION

**Main Branch:**
$wallet = "..."
$validator_vote = "..."
$sigs = getSignaturesForAddress(address: $validator_vote, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$collusion = DETECT_VALIDATOR_COLLUSION(transactions: $txs, wallet: $wallet)

**Action:**
RETURN {wallet: $wallet, validator_collusion_patterns: $collusion, confidence: 91}

---

## Q5479: "Identify wallets that interact with multiple NFT collections in rapid succession (potential NFT flippers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_NFT_FLIPPING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$nft_flipping = DETECT_NFT_FLIPPING(transactions: $txs, min_collections: 3, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, nft_flipping_patterns: $nft_flipping, confidence: 90}

---

## Q5480: "Detect wallets that frequently interact with token mint authorities (potential insider activity)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_MINT_AUTHORITY_INTERACTIONS

**Main Branch:**
$wallet = "..."
$mint_authority = "..."
$sigs = getSignaturesForAddress(address: $mint_authority, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$insider_activity = DETECT_MINT_AUTHORITY_INTERACTIONS(transactions: $txs, wallet: $wallet)

**Action:**
RETURN {wallet: $wallet, mint_authority_interaction_patterns: $insider_activity, confidence: 91}

---

## Q5481: "Detect wallets that interact with multiple launchpad contracts in a short period (potential launchpad snipers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_LAUNCHPAD_SNIPING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$launchpad_sniping = DETECT_LAUNCHPAD_SNIPING(transactions: $txs, min_launchpads: 2, max_window_hr: 12)

**Action:**
RETURN {wallet: $wallet, launchpad_sniping_patterns: $launchpad_sniping, confidence: 90}

---

## Q5482: "Identify wallets that frequently interact with the same set of CEX deposit addresses (potential off-ramping)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_CEX_OFFRAMPING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$offramping = DETECT_CEX_OFFRAMPING(transactions: $txs, min_cex: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, cex_offramping_patterns: $offramping, confidence: 91}

---

## Q5483: "Detect wallets that interact with multiple meme token deployers (potential serial deployers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_SERIAL_DEPLOYERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$serial_deployers = DETECT_SERIAL_DEPLOYERS(transactions: $txs, min_deployers: 3, max_window_hr: 48)

**Action:**
RETURN {wallet: $wallet, serial_deployer_patterns: $serial_deployers, confidence: 90}

---

## Q5484: "Identify wallets that frequently interact with the same set of NFT marketplaces (potential wash traders)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_NFT_MARKETPLACE_WASH_TRADING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$marketplace_wash_trading = DETECT_NFT_MARKETPLACE_WASH_TRADING(transactions: $txs, min_marketplaces: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, nft_marketplace_wash_trading_patterns: $marketplace_wash_trading, confidence: 91}

---

## Q5485: "Detect wallets that interact with multiple staking pools in a short period (potential yield chasers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_YIELD_CHASING

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$yield_chasing = DETECT_YIELD_CHASING(transactions: $txs, min_pools: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, yield_chasing_patterns: $yield_chasing, confidence: 90}

---

## Q5486: "Identify wallets that frequently interact with the same set of DAO treasuries (potential governance attackers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_DAO_ATTACKERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$dao_attackers = DETECT_DAO_ATTACKERS(transactions: $txs, min_daos: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, dao_attacker_patterns: $dao_attackers, confidence: 91}

---

## Q5487: "Detect wallets that interact with multiple lending protocols in a short period (potential liquidation bots)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_LIQUIDATION_BOTS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$liquidation_bots = DETECT_LIQUIDATION_BOTS(transactions: $txs, min_protocols: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, liquidation_bot_patterns: $liquidation_bots, confidence: 90}

---

## Q5488: "Identify wallets that frequently interact with the same set of token mints (potential token deployers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_TOKEN_DEPLOYERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$token_deployers = DETECT_TOKEN_DEPLOYERS(transactions: $txs, min_mints: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, token_deployer_patterns: $token_deployers, confidence: 91}

---

## Q5489: "Detect wallets that interact with multiple prediction market contracts in a short period (potential prediction market manipulators)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_PREDICTION_MARKET_MANIPULATION

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$prediction_market_manipulation = DETECT_PREDICTION_MARKET_MANIPULATION(transactions: $txs, min_markets: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, prediction_market_manipulation_patterns: $prediction_market_manipulation, confidence: 90}

---

## Q5490: "Identify wallets that frequently interact with the same set of token swap pools (potential liquidity providers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_LIQUIDITY_PROVIDERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$liquidity_providers = DETECT_LIQUIDITY_PROVIDERS(transactions: $txs, min_pools: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, liquidity_provider_patterns: $liquidity_providers, confidence: 91}

---

## Q5491: "Detect wallets that interact with multiple token burn contracts in a short period (potential burn bots)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_BURN_BOTS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$burn_bots = DETECT_BURN_BOTS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, burn_bot_patterns: $burn_bots, confidence: 90}

---

## Q5492: "Identify wallets that frequently interact with the same set of token vesting contracts (potential early unlockers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_EARLY_UNLOCKERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$early_unlockers = DETECT_EARLY_UNLOCKERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, early_unlocker_patterns: $early_unlockers, confidence: 91}

---

## Q5493: "Detect wallets that interact with multiple token faucet contracts in a short period (potential faucet abusers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_FAUCET_ABUSERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$faucet_abusers = DETECT_FAUCET_ABUSERS(transactions: $txs, min_faucets: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, faucet_abuser_patterns: $faucet_abusers, confidence: 90}

---

## Q5494: "Identify wallets that frequently interact with the same set of token lock contracts (potential lock bypassers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_LOCK_BYPASSERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$lock_bypassers = DETECT_LOCK_BYPASSERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, lock_bypasser_patterns: $lock_bypassers, confidence: 91}

---

## Q5495: "Detect wallets that interact with multiple token migration contracts in a short period (potential migration exploiters)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_MIGRATION_EXPLOITERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$migration_exploiters = DETECT_MIGRATION_EXPLOITERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, migration_exploiter_patterns: $migration_exploiters, confidence: 90}

---

## Q5496: "Identify wallets that frequently interact with the same set of token claim contracts (potential claim farmers)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_CLAIM_FARMERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$claim_farmers = DETECT_CLAIM_FARMERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, claim_farmer_patterns: $claim_farmers, confidence: 91}

---

## Q5497: "Detect wallets that interact with multiple token distribution contracts in a short period (potential distribution exploiters)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_DISTRIBUTION_EXPLOITERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$distribution_exploiters = DETECT_DISTRIBUTION_EXPLOITERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, distribution_exploiter_patterns: $distribution_exploiters, confidence: 90}

---

## Q5498: "Identify wallets that frequently interact with the same set of token governance contracts (potential governance manipulators)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_GOVERNANCE_MANIPULATORS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$governance_manipulators = DETECT_GOVERNANCE_MANIPULATORS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, governance_manipulator_patterns: $governance_manipulators, confidence: 91}

---

## Q5499: "Detect wallets that interact with multiple token airdrop claim contracts in a short period (potential airdrop exploiters)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_AIRDROP_EXPLOITERS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$airdrop_exploiters = DETECT_AIRDROP_EXPLOITERS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, airdrop_exploiter_patterns: $airdrop_exploiters, confidence: 90}

---

## Q5500: "Identify wallets that frequently interact with the same set of token buyback contracts (potential buyback manipulators)."

**Expected Plan:**
[TIME: ~3m] [COST: ~0.03 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getSignaturesForAddress, getTransaction, MAP, DETECT_BUYBACK_MANIPULATORS

**Main Branch:**
$wallet = "..."
$sigs = getSignaturesForAddress(address: $wallet, limit: 1000)
$txs = MAP(collection: $sigs, fn: s => getTransaction(s.signature))
$buyback_manipulators = DETECT_BUYBACK_MANIPULATORS(transactions: $txs, min_contracts: 2, max_window_hr: 24)

**Action:**
RETURN {wallet: $wallet, buyback_manipulator_patterns: $buyback_manipulators, confidence: 91}
