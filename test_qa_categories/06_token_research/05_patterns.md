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

## Q5411: "Map a 'crypto laundering service' that accepts dirty funds and returns 'clean' crypto for a fee. Track all clients, calculate service volume, and find operator wallets."

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

