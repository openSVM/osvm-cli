# Token Research - Intermediate On-Chain Analysis (Q101-Q200)

**Category:** Token Research
**Difficulty:** Intermediate
**Focus:** Holder analysis, liquidity assessment, social sentiment, basic forensics, protocol interaction, competitive positioning
**Questions:** Q101-Q200

---

## Q101: "What is the holder distribution for 'Pyth Network' (PYTH)? Calculate the Gini coefficient and the percentage of supply held by the top 100 wallets."

**Expected Plan:**

[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 95%]

**Available Tools:**
  - getTokenLargestAccounts (Solana RPC)
  - CALCULATE_GINI_COEFFICIENT (Statistical)
  - SUM, MAP (Data Processing)

**Main Branch:**
$token_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht" // PYTH

$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)

// Calculate Gini coefficient for holder distribution
$gini_coefficient = CALCULATE_GINI_COEFFICIENT(holders: $holders)

// Calculate percentage held by top 100
$top_100_holders = SLICE($holders, 0, 100)
$top_100_percentage = SUM(MAP(collection: $top_100_holders, fn: h => h.percentage))

**Decision Point:** Evaluate the decentralization of holders.
  BRANCH A ($gini_coefficient > 0.9):
    $distribution = "Extremely Centralized"
    $summary = "Holder distribution is highly centralized. A very small number of wallets control the vast majority of the supply."
  BRANCH B ($gini_coefficient > 0.7):
    $distribution = "Centralized"
    $summary = "Holder distribution is centralized, with significant concentration among the top wallets. This could pose a risk to price stability."
  BRANCH C (default):
    $distribution = "Relatively Distributed"
    $summary = "Holder distribution is more distributed than many other tokens, though top holders still have significant influence."

**Action:**
RETURN {
  distribution_type: $distribution,
  summary: $summary,
  gini_coefficient: $gini_coefficient,
  percentage_held_by_top_100: $top_100_percentage,
  confidence: 95
}

---

## Q102: "How much liquidity is available for 'Bonk' (BONK) across all Raydium and Orca pools, and what is the 24-hour trading volume?"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library:
  - RAYDIUM_GET_POOLS_FOR_TOKEN (Raydium API)
  - ORCA_GET_POOLS_FOR_TOKEN (Orca API)
  - SUM, MAP (Data Processing)

**Main Branch:**
$token_mint = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263" // BONK


## Q121: "Which meme tokens have the highest percentage of holders in profit, and what entry prices did they buy at?"

## Q122: "How can you analyze the liquidity depth of a new meme token to determine the maximum safe buy size for a trader?"

## Q123: "What is the best way to track the top 10 wallets accumulating a meme token before a major event?"

## Q124: "How can you use prediction market odds to inform entry and exit points for meme token trades?"

## Q125: "What is the average time between a meme token's launch and its first major whale sell-off?"

## Q126: "How can you identify wallets that are likely to dump a meme token based on on-chain behavioral patterns?"

## Q127: "What is the most profitable strategy for rotating between meme tokens and prediction market bets during high volatility?"

## Q128: "How can you use on-chain volume and order flow to predict the next meme token to trend on social media?"

## Q129: "What is the best way to hedge a meme token position using prediction markets on Solana?"

## Q130: "How can you detect and avoid wallets that are likely to be exit liquidity in a meme token pump and dump?"
$raydium_pools = RAYDIUM_GET_POOLS_FOR_TOKEN(mint: $token_mint)
$orca_pools = ORCA_GET_POOLS_FOR_TOKEN(mint: $token_mint)

$all_pools = APPEND(array: $raydium_pools, item: $orca_pools)

GUARD COUNT($all_pools) > 0 ELSE
  RETURN ERROR(message: "No liquidity pools found for BONK.")

// Sum the liquidity and volume across all found pools
$total_liquidity_usd = SUM(MAP(collection: $all_pools, fn: p => p.liquidity_usd))
$total_volume_24h_usd = SUM(MAP(collection: $all_pools, fn: p => p.volume_24h_usd))

**Decision Point:** Assess the overall liquidity health.
  BRANCH A ($total_liquidity_usd > 10000000):
    $health = "Healthy"
    $assessment = "The token has deep liquidity across multiple major DEXs, allowing for large trades with minimal price impact."
  BRANCH B ($total_liquidity_usd > 1000000):
    $health = "Moderate"
    $assessment = "The token has sufficient liquidity for most retail-sized trades, but very large trades may cause significant slippage."
  BRANCH C (default):
    $health = "Shallow"
    $assessment = "Liquidity is low. The token is susceptible to high price volatility and large trades are not recommended."

**Action:**
RETURN {
  liquidity_health: $health,
  assessment: $assessment,
  total_liquidity_usd: $total_liquidity_usd,
  total_volume_24h_usd: $total_volume_24h_usd,
  pool_count: COUNT($all_pools),
  pools_breakdown: $all_pools,
  confidence: 96
}

---

## Q103: "What is the average sentiment score for 'dogwifhat' (WIF) on Twitter over the last 7 days?"

**Expected Plan:**

[TIME: ~40s] [COST: ~0.003 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - TWITTER_GET_SENTIMENT_HISTORY (Social API)
  - MEAN (Statistical)

**Main Branch:**
$cashtag = "$WIF"

// Get the sentiment history from a social media API
$sentiment_history = TWITTER_GET_SENTIMENT_HISTORY(
  query: $cashtag,
  time_period: "7d",
  interval: "1d"
)

GUARD COUNT($sentiment_history) > 0 ELSE
  RETURN ERROR(message: "Could not retrieve sentiment data.")

// Calculate the average sentiment score
$average_sentiment = MEAN(MAP(collection: $sentiment_history, fn: s => s.score))

**Decision Point:** Interpret the sentiment.
  BRANCH A ($average_sentiment > 0.7):
    $sentiment = "Overwhelmingly Positive"
    $summary = "Social media sentiment is extremely bullish, indicating strong community support and positive buzz."
  BRANCH B ($average_sentiment > 0.3):
    $sentiment = "Positive"
    $summary = "Overall sentiment is positive, with more favorable mentions than negative ones."
  BRANCH C ($average_sentiment < -0.3):
    $sentiment = "Negative"
    $summary = "Overall sentiment is negative, indicating potential issues, FUD, or community dissatisfaction."
  BRANCH D (default):
    $sentiment = "Neutral"
    $summary = "Sentiment is balanced, with no strong positive or negative trend."

**Action:**
RETURN {
  average_sentiment_7d: $average_sentiment,
  sentiment_category: $sentiment,
  summary: $summary,
  daily_sentiment_scores: $sentiment_history,
  confidence: 88
}

---

## Q104: "Trace the origin of the funds used to create the 'Jeo Boden' (BODEN) token's liquidity pool on Raydium. Did they come from a centralized exchange?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - RAYDIUM_GET_LP_CREATION_TX (Raydium API)
  - SOLANA_TRACE_FUNDS (Forensics)

**Main Branch:**
$token_mint = "3psH1Mj1f7yUfaD5gh6a1sHk4S_..." // BODEN mint address

// 1. Find the transaction that created the initial liquidity pool
$creation_tx = RAYDIUM_GET_LP_CREATION_TX(mint: $token_mint)
$creator_wallet = $creation_tx.creator_address

// 2. Trace the funds in the creator's wallet backwards
$trace = SOLANA_TRACE_FUNDS(
  start_address: $creator_wallet,
  direction: "backward",
  depth: 3 // Look back 3 transactions
)

**Decision Point:** Identify the source of the funds.
  BRANCH A ($trace.source_is_cex):
    $source = "Centralized Exchange"
    $conclusion = "The funds used to create the liquidity pool originated from a known CEX wallet (" + $trace.cex_name + "). This is a common funding method."
  BRANCH B ($trace.source_is_mixer):
    $source = "Mixer / Privacy Protocol"
    $conclusion = "The funds were sent from a privacy protocol, making their ultimate origin difficult to trace. This is a potential red flag."
  BRANCH C ($trace.source_is_fresh_wallet):
    $source = "Fresh Wallet"
    $conclusion = "The funds came from a newly created wallet with no prior history, which itself was funded by a CEX. This is a common pattern for developers."
  BRANCH D (default):
    $source = "Established Solana Wallet"
    $conclusion = "The funds came from an established wallet with a long history of activity on Solana."

**Action:**
RETURN {
  funding_source_type: $source,
  conclusion: $conclusion,
  lp_creator_wallet: $creator_wallet,
  funding_trace_path: $trace.path,
  confidence: 90
}

---

## Q105: "How many unique wallets have interacted with the 'Tensor' (TNSR) smart contract in the last 30 days?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - GET_PROGRAM_INTERACTIONS (Analysis)
  - COUNT, UNIQUE (Data Processing)

**Main Branch:**
$tnsr_program_id = "..." // Tensor's main program ID

// Get all transactions that have interacted with the program in the last 30 days
$interactions = GET_PROGRAM_INTERACTIONS(
  program_id: $tnsr_program_id,
  time_period: "30d"
)

// Count the number of unique wallets that signed these transactions
$unique_wallets = COUNT(UNIQUE(MAP(collection: $interactions, fn: i => i.signer)))

**Decision Point:** Evaluate the user base size.
  BRANCH A ($unique_wallets > 100000):
    $user_base = "Very Large"
    $assessment = "The protocol has a very large and active user base, indicating widespread adoption."
  BRANCH B ($unique_wallets > 20000):
    $user_base = "Large"
    $assessment = "The protocol has a substantial user base, suggesting it is a major player in its sector."
  BRANCH C (default):
    $user_base = "Moderate"
    $assessment = "The protocol has a moderate user base, indicating it is established but may still be in a growth phase."

**Action:**
RETURN {
  unique_interacting_wallets_30d: $unique_wallets,
  user_base_category: $user_base,
  assessment: $assessment,
  confidence: 94
}

---

## Q106: "What is the ratio of buys to sells for the 'Popcat' (POPCAT) token on all DEXs in the last hour?"

**Expected Plan:**

[TIME: ~40s] [COST: ~0.003 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - BIRDEYE_GET_TRADES_HISTORY (Birdeye API)
  - COUNT, FILTER (Data Processing)

**Main Branch:**
$token_address = "..." // POPCAT address

// Get all trades for the token in the last hour from a data aggregator
$trades = BIRDEYE_GET_TRADES_HISTORY(
  token_address: $token_address,
  time_period: "1h"
)

GUARD COUNT($trades) > 0 ELSE
  RETURN ERROR(message: "No trades found in the last hour.")

// Count buys and sells
$buys = COUNT(FILTER(collection: $trades, predicate: t => t.side == "buy"))
$sells = COUNT(FILTER(collection: $trades, predicate: t => t.side == "sell"))

$buy_sell_ratio = $buys / MAX($sells, 1)

**Decision Point:** Interpret the short-term market pressure.
  BRANCH A ($buy_sell_ratio > 2.0):
    $pressure = "Strong Buying Pressure"
    $summary = "There are more than twice as many buy orders as sell orders, indicating strong positive momentum in the short term."
  BRANCH B ($buy_sell_ratio < 0.5):
    $pressure = "Strong Selling Pressure"
    $summary = "There are more than twice as many sell orders as buy orders, indicating strong negative momentum in the short term."
  BRANCH C (default):
    $pressure = "Balanced"
    $summary = "The number of buy and sell orders is relatively balanced, indicating market consolidation or equilibrium."

**Action:**
RETURN {
  market_pressure: $pressure,
  summary: $summary,
  buy_sell_ratio_1h: $buy_sell_ratio,
  buy_count_1h: $buys,
  sell_count_1h: $sells,
  confidence: 92
}

---

## Q107: "Compare the market cap and 7-day performance of 'Jupiter' (JUP) vs. 'Pyth Network' (PYTH)."

**Expected Plan:**

[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library:
  - BIRDEYE_GET_TOKEN_METRICS (Birdeye API)

**Main Branch:**
$jup_address = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"
$pyth_address = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht"

// Get metrics for both tokens
$jup_metrics = BIRDEYE_GET_TOKEN_METRICS(token_address: $jup_address)
$pyth_metrics = BIRDEYE_GET_TOKEN_METRICS(token_address: $pyth_address)

$comparison = {
  jup: {
    market_cap_usd: $jup_metrics.market_cap,
    price_change_7d_pct: $jup_metrics.price_change_7d_pct
  },
  pyth: {
    market_cap_usd: $pyth_metrics.market_cap,
    price_change_7d_pct: $pyth_metrics.price_change_7d_pct
  }
}

**Decision Point:** Determine the relative winner.
  BRANCH A ($comparison.jup.price_change_7d_pct > $comparison.pyth.price_change_7d_pct):
    $winner = "Jupiter (JUP)"
    $reason = "JUP has outperformed PYTH over the last 7 days."
  BRANCH B (default):
    $winner = "Pyth Network (PYTH)"
    $reason = "PYTH has outperformed JUP over the last 7 days."

**Action:**
RETURN {
  performance_winner_7d: $winner,
  reason: $reason,
  comparison_data: $comparison,
  confidence: 98
}

---

## Q108: "What percentage of the 'Marinade Staked SOL' (mSOL) supply is currently being used as collateral in the Kamino Lend protocol?"

**Expected Plan:**

[TIME: ~40s] [COST: ~0.003 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTokenSupply (Solana RPC)
  - KAMINO_GET_COLLATERAL_INFO (Kamino API)

**Main Branch:**
$msol_mint = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"

// 1. Get the total supply of mSOL
$msol_supply = getTokenSupply(mint: $msol_mint).value.uiAmount

// 2. Get the amount of mSOL deposited as collateral in Kamino
$kamino_collateral = KAMINO_GET_COLLATERAL_INFO(token_mint: $msol_mint)
$msol_in_kamino = $kamino_collateral.total_deposits

// 3. Calculate the percentage
$percentage_in_kamino = ($msol_in_kamino / $msol_supply) * 100

**Decision Point:** Evaluate the significance of Kamino as a source of utility for mSOL.
  BRANCH A ($percentage_in_kamino > 20):
    $significance = "Very Significant"
    $summary = "Kamino Lend is a primary source of utility for mSOL, with over 20% of the total supply being used as collateral on the platform."
  BRANCH B ($percentage_in_kamino > 5):
    $significance = "Significant"
    $summary = "Kamino is an important DeFi integration for mSOL, providing a significant source of demand for the token."
  BRANCH C (default):
    $significance = "Minor"
    $summary = "While some mSOL is used on Kamino, it is not a primary driver of its utility or demand."

**Action:**
RETURN {
  significance: $significance,
  summary: $summary,
  percentage_of_msol_in_kamino: $percentage_in_kamino,
  total_msol_supply: $msol_supply,
  msol_deposited_in_kamino: $msol_in_kamino,
  confidence: 95
}

---

## Q109: "Identify the first 10 wallets that bought 'MichiCoin' (MICHI) after the liquidity pool was created. Are any of them associated with the token deployer?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.006 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - RAYDIUM_GET_LP_CREATION_TX (Raydium API)
  - GET_TRANSACTIONS_AFTER (Analysis)
  - ARE_WALLETS_CONNECTED (Forensics)

**Main Branch:**
$token_mint = "..." // MICHI mint address

// 1. Find the LP creation transaction to get the timestamp and deployer wallet
$lp_creation_tx = RAYDIUM_GET_LP_CREATION_TX(mint: $token_mint)
$deployer_wallet = $lp_creation_tx.creator_address
$creation_time = $lp_creation_tx.timestamp

// 2. Get the first 100 transactions involving the token after the LP was created
$trades_after_creation = GET_TRANSACTIONS_AFTER(
  token_mint: $token_mint,
  timestamp: $creation_time,
  limit: 100
)

// 3. Filter for buys and get the first 10 unique buyers
$first_10_buyers = UNIQUE(MAP(FILTER($trades_after_creation, t => t.side == "buy"), t => t.buyer_address), limit: 10)

// 4. Check if any of these buyers have a funding relationship with the deployer
$connected_wallets = ARE_WALLETS_CONNECTED(
  wallet_a: $deployer_wallet,
  wallets_b: $first_10_buyers
)

**Decision Point:** Check for insider activity.
  BRANCH A (COUNT($connected_wallets) > 0):
    $finding = "Insider Activity Likely"
    $conclusion = "Yes, " + COUNT($connected_wallets) + " of the first 10 buyers were funded by the deployer wallet, indicating likely insider buying before the token was publicly announced."
  BRANCH B (default):
    $finding = "No Direct Connection Found"
    $conclusion = "None of the first 10 buyers appear to have a direct funding link to the deployer wallet based on a simple trace."

**Action:**
RETURN {
  finding: $finding,
  conclusion: $conclusion,
  deployer_wallet: $deployer_wallet,
  first_10_buyer_wallets: $first_10_buyers,
  connected_insider_wallets: $connected_wallets,
  confidence: 91
}

---

## Q110: "What is the average gas fee (priority fee) in USD paid for transactions involving the 'WIF' token in the last hour?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - GET_RECENT_TRANSACTIONS (Analysis)
  - MEAN (Statistical)
  - GET_SOL_PRICE (Price API)

**Main Branch:**
$token_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm" // WIF

// Get recent transactions for the token
$recent_txs = GET_RECENT_TRANSACTIONS(
  token_mint: $token_mint,
  time_period: "1h"
)

GUARD COUNT($recent_txs) > 0 ELSE
  RETURN ERROR(message: "No transactions found for WIF in the last hour.")

// Calculate the average priority fee in lamports
$avg_priority_fee_lamports = MEAN(MAP(collection: $recent_txs, fn: tx => tx.priority_fee))

// Convert to SOL and then to USD
$sol_price_usd = GET_SOL_PRICE()
$avg_priority_fee_sol = $avg_priority_fee_lamports / 1000000000
$avg_priority_fee_usd = $avg_priority_fee_sol * $sol_price_usd

**Decision Point:** Evaluate the fee level.
  BRANCH A ($avg_priority_fee_usd > 0.10):
    $level = "High"
    $summary = "The average priority fee is high, indicating significant network congestion or high demand for transactions involving this token."
  BRANCH B ($avg_priority_fee_usd > 0.01):
    $level = "Moderate"
    $summary = "Priority fees are noticeable, suggesting a moderate level of demand or network activity."
  BRANCH C (default):
    $level = "Low"
    $summary = "Priority fees are very low, indicating normal network conditions."

**Action:**
RETURN {
  fee_level: $level,
  summary: $summary,
  average_priority_fee_usd: $avg_priority_fee_usd,
  average_priority_fee_lamports: $avg_priority_fee_lamports,
  sample_size_txs: COUNT($recent_txs),
  confidence: 93
}

---

## Q131: "Detect 'spoofed LP tokens' where scammers create fake LP tokens that look identical to real ones to trick users into depositing value."

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

## Q132: "Map a 'DAO treasury raid' where malicious proposal drains funds. Trace the proposal creation, voting patterns, and fund destinations."

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

## Q133: "Identify 'circular trading bots' that create artificial volume by trading between their own wallets with no real market impact."

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

## Q134: "Investigate a 'bridge exploit' where attacker mints unlimited tokens by exploiting cross-chain message verification."

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

## Q135: "Detect 'governance token rental markets' where users rent voting power, potentially enabling vote manipulation."

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

## Q136: "Map a 'MEV sandwich cartel' where multiple bots coordinate to avoid competing on the same victim transactions."

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

## Q137: "Investigate 'NFT metadata poisoning' where scammers inject malicious URLs into NFT metadata to phish holders."

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

## Q138: "Detect 'stablecoin depeg manipulation' where whales intentionally cause depegs to profit from panic selling."

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

## Q139: "Map 'validator sandwich attack coordination' where validators collude to sandwich user transactions in their own blocks."

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

## Q140: "Investigate 'pump-and-dump as a service' platforms that coordinate paid pump campaigns for tokens."

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

## Q141: "Detect 'fake liquidity lock' where scammers lock liquidity in a contract they control and can withdraw anytime."

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

## Q142: "Map 'cNFT minting exploits' where attackers mint compressed NFTs beyond the tree capacity by exploiting Merkle tree verification."

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

## Q143: "Investigate 'prediction market oracle manipulation' where insiders with early knowledge manipulate market odds before public announcement."

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

## Q144: "Detect 'token supply inflation' attacks where minter role is exploited to create unlimited tokens and dump on market."

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

## Q145: "Map a 'liquidation bot frontrunning cartel' where bots coordinate to avoid competing on the same liquidation opportunities."

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

## Q146: "Investigate 'fake airdrop claim' scams that require users to approve unlimited token spending to claim worthless tokens."

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

## Q147: "Detect 'validator censorship' where specific validators consistently exclude certain transactions or protocols from their blocks."

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

## Q148: "Map 'cross-DEX arbitrage bot networks' that share infrastructure and coordinate to reduce gas competition."

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

## Q149: "Investigate 'mempool frontrunning on Solana' by analyzing Jito bundle patterns and identifying systematic frontrunners."

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

## Q150: "Detect 'rug pull via upgrade authority' where devs use program upgrade authority to inject malicious code after launch."

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


## Q111: "Analyze the on-chain volume impact on 'WEN' token after its listing on a major CEX like Binance or Coinbase."

**Expected Plan:**
[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADES_HISTORY, SEARCH_WEB, FILTER, SUM

**Main Branch:**
$token_mint = "WENWENvqqNya429ub2VjAbhX4k4F85FvH6C2eswMZX" // WEN
$listing_date = SEARCH_WEB(query: "WEN token Binance listing date")

// Get trade history before and after the listing
$trades_before = GET_TOKEN_TRADES_HISTORY(token: $token_mint, start_date: $listing_date - "7d", end_date: $listing_date)
$trades_after = GET_TOKEN_TRADES_HISTORY(token: $token_mint, start_date: $listing_date, end_date: $listing_date + "7d")

$volume_before_usd = SUM(MAP($trades_before, t => t.volume_usd))
$volume_after_usd = SUM(MAP($trades_after, t => t.volume_usd))
$volume_change_pct = (($volume_after_usd - $volume_before_usd) / $volume_before_usd) * 100

**Decision Point:** Evaluate the impact of the CEX listing.
  BRANCH A ($volume_change_pct > 50):
    $impact = "Significant Increase"
    $summary = "The CEX listing led to a significant increase in on-chain trading volume, likely due to arbitrage and increased awareness."
  BRANCH B ($volume_change_pct < -50):
    $impact = "Significant Decrease"
    $summary = "On-chain volume decreased significantly as trading activity migrated to the centralized exchange."
  BRANCH C (default):
    $impact = "Neutral"
    $summary = "The CEX listing did not have a strong immediate impact on on-chain trading volume."

**Action:**
RETURN {
  impact: $impact,
  summary: $summary,
  volume_change_percentage: $volume_change_pct,
  onchain_volume_before_listing_usd: $volume_before_usd,
  onchain_volume_after_listing_usd: $volume_after_usd,
  confidence: 92
}

---

## Q112: "Compare the holder growth rate of 'cat in a dogs world' (MEW) vs. 'dogwifhat' (WIF) over the last 30 days."

**Expected Plan:**
[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library: GET_HOLDER_COUNT_HISTORY

**Main Branch:**
$mew_mint = "MEW1gQWJ3nEXg2qgERiKu7FAFj79PHvQVREQUzScPP5"
$wif_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"

// Get historical holder counts for both tokens
$mew_history = GET_HOLDER_COUNT_HISTORY(token: $mew_mint, period: "30d")
$wif_history = GET_HOLDER_COUNT_HISTORY(token: $wif_mint, period: "30d")

$mew_growth = ($mew_history.latest / $mew_history.initial) - 1
$wif_growth = ($wif_history.latest / $wif_history.initial) - 1

**Decision Point:** Determine which token grew faster.
  BRANCH A ($mew_growth > $wif_growth):
    $winner = "cat in a dogs world (MEW)"
    $reason = "MEW experienced a higher percentage growth in its holder base over the last 30 days."
  BRANCH B (default):
    $winner = "dogwifhat (WIF)"
    $reason = "WIF experienced a higher percentage growth in its holder base over the last 30 days."

**Action:**
RETURN {
  faster_growth_token: $winner,
  reason: $reason,
  mew_growth_percentage: $mew_growth * 100,
  wif_growth_percentage: $wif_growth * 100,
  confidence: 94
}

---

## Q113: "Identify the top 5 most active trading pairs for the 'Jito' (JTO) token on Raydium."

**Expected Plan:**
[TIME: ~35s] [COST: ~0.003 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library: RAYDIUM_GET_POOLS_FOR_TOKEN, SORT_BY, SLICE

**Main Branch:**
$jto_mint = "jtojtomepa8beP8AuQc6eXt5FriJwfFMwQx2v2f9mCL"

// Get all Raydium pools for JTO
$jto_pools = RAYDIUM_GET_POOLS_FOR_TOKEN(mint: $jto_mint)

// Sort pools by 24-hour volume in descending order
$sorted_pools = SORT_BY(collection: $jto_pools, field: "volume_24h_usd", direction: "desc")

// Get the top 5
$top_5_pairs = SLICE($sorted_pools, 0, 5)

**Action:**
RETURN {
  token: "Jito (JTO)",
  top_5_trading_pairs_on_raydium: MAP($top_5_pairs, p => {
    pair: p.pair_name,
    volume_24h_usd: p.volume_24h_usd,
    liquidity_usd: p.liquidity_usd
  }),
  confidence: 96
}

---

## Q114: "Find wallets that are providing liquidity for 'BONK' on both Orca and Raydium simultaneously."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.007 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: RAYDIUM_GET_LP_HOLDERS, ORCA_GET_LP_HOLDERS, INTERSECT

**Main Branch:**
$bonk_mint = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"

// Get LP holders from both DEXs for BONK pools
$raydium_lps = RAYDIUM_GET_LP_HOLDERS(token_mint: $bonk_mint)
$orca_lps = ORCA_GET_LP_HOLDERS(token_mint: $bonk_mint)

// Find the intersection of the two lists of wallets
$common_lps = INTERSECT(list_a: $raydium_lps, list_b: $orca_lps)

**Action:**
RETURN {
  token: "BONK",
  common_lp_wallets: $common_lps,
  common_lp_count: COUNT($common_lps),
  confidence: 90,
  caveats: ["This only checks for wallets holding LP tokens, not necessarily actively staking them."]
}

---

## Q115: "Calculate the average holding duration for the 'Pyth Network' (PYTH) token among its top 1000 holders."

**Expected Plan:**
[TIME: ~70s] [COST: ~0.01 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, GET_WALLET_FIRST_TXN, MEAN

**Main Branch:**
$pyth_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht"
$top_holders = getTokenLargestAccounts(mint: $pyth_mint, limit: 1000)

// Find the first transaction date for each holder
$holding_durations = MAP(collection: $top_holders, fn: h => {
  $first_tx = GET_WALLET_FIRST_TXN(wallet: h.address, token: $pyth_mint)
  RETURN NOW() - $first_tx.blockTime
})

$average_duration_seconds = MEAN($holding_durations)
$average_duration_days = $average_duration_seconds / 86400

**Action:**
RETURN {
  token: "Pyth Network (PYTH)",
  average_holding_duration_days: $average_duration_days,
  sample_size: COUNT($top_holders),
  confidence: 88,
  caveats: ["Assumes the first transaction for the token represents the start of the holding period."]
}

---

## Q116: "Detect large transfers of 'mSOL' from personal wallets to known CEX deposit addresses in the last 24 hours."

**Expected Plan:**
[TIME: ~40s] [COST: ~0.004 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRANSFERS, FILTER, IS_CEX_DEPOSIT_ADDRESS

**Main Branch:**
$msol_mint = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"

// Get all mSOL transfers in the last 24 hours
$transfers = GET_TOKEN_TRANSFERS(token: $msol_mint, period: "24h")

// Filter for large transfers to CEX addresses
$large_cex_deposits = FILTER(collection: $transfers, predicate: t => {
  RETURN t.amount_ui > 1000 && IS_CEX_DEPOSIT_ADDRESS(t.destination)
})

**Action:**
RETURN {
  token: "mSOL",
  large_cex_deposits_found: COUNT($large_cex_deposits),
  total_deposited_amount: SUM(MAP($large_cex_deposits, d => d.amount_ui)),
  transactions: $large_cex_deposits,
  confidence: 93
}

---

## Q117: "What percentage of the 'Tensor' (TNSR) token supply is held in smart contracts (e.g., vesting, staking, LPs) vs. regular wallets?"

**Expected Plan:**
[TIME: ~55s] [COST: ~0.006 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: getTokenLargestAccounts, GET_ACCOUNT_TYPE, getTokenSupply

**Main Branch:**
$tnsr_mint = "TNSRxcUxoT9xBG3de7PiJyTDYu7kskLqcpddxnEJAS6"
$total_supply = getTokenSupply(mint: $tnsr_mint).uiAmount
$top_holders = getTokenLargestAccounts(mint: $tnsr_mint, limit: 1000)

$supply_in_contracts = 0
FOR $holder IN $top_holders:
  $account_type = GET_ACCOUNT_TYPE(address: $holder.address)
  IF $account_type == "program" OR $account_type == "pda":
    $supply_in_contracts += $holder.uiAmount

$percentage_in_contracts = ($supply_in_contracts / $total_supply) * 100

**Action:**
RETURN {
  token: "Tensor (TNSR)",
  percentage_in_smart_contracts: $percentage_in_contracts,
  supply_in_contracts: $supply_in_contracts,
  total_supply: $total_supply,
  confidence: 91,
  caveats: ["Analysis is based on the top 1000 holders, not the entire supply."]
}

---

## Q118: "Identify wallets that are accumulating 'BODEN' ahead of the US election, and correlate their activity with polling data."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADES_HISTORY, SEARCH_WEB, CORRELATE, FILTER

**Main Branch:**
$boden_mint = "3psH1Mj1f7yUfaD5gh6a1sHk4S_..." // BODEN
$trades = GET_TOKEN_TRADES_HISTORY(token: $boden_mint, period: "90d")
$accumulators = FIND_ACCUMULATORS(trades: $trades, min_net_buy: 10000)

// Get polling data from an external source
$polling_data = SEARCH_WEB(query: "US election polling data last 90 days")

// Correlate accumulation spikes with polling shifts
$correlation = CORRELATE(
  data1: MAP($accumulators, a => { date: a.date, volume: a.buy_volume }),
  data2: $polling_data
)

**Action:**
RETURN {
  token: "Jeo Boden (BODEN)",
  top_accumulators: SLICE(SORT_BY($accumulators, "net_buy", "desc"), 0, 10),
  correlation_with_polls: $correlation.summary,
  correlation_score: $correlation.score,
  confidence: 85
}

---

## Q119: "Track the change in 'token velocity' for 'dogwifhat' (WIF) over the past 3 months. Is it increasing or decreasing?"

**Expected Plan:**
[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: GET_TOKEN_VELOCITY_HISTORY

**Main Branch:**
$wif_mint = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm"

// Get monthly velocity for the last 3 months
$velocity_history = GET_TOKEN_VELOCITY_HISTORY(token: $wif_mint, period: "3M", interval: "1M")

$latest_velocity = $velocity_history.last().velocity
$first_velocity = $velocity_history.first().velocity
$trend = ($latest_velocity > $first_velocity) ? "Increasing" : "Decreasing"

**Action:**
RETURN {
  token: "dogwifhat (WIF)",
  velocity_trend: $trend,
  monthly_velocity_data: $velocity_history,
  summary: "Token velocity is " + $trend + ", indicating that the frequency of the token being transacted is changing.",
  confidence: 90
}

---

## Q120: "Find the largest single sell order for 'Popcat' (POPCAT) in the last 7 days and trace where the proceeds went."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADES_HISTORY, SORT_BY, TRACE_FUNDS

**Main Branch:**
$popcat_address = "..." // POPCAT address
$trades = GET_TOKEN_TRADES_HISTORY(token_address: $popcat_address, time_period: "7d")

// Find the largest sell trade
$sell_trades = FILTER($trades, t => t.side == "sell")
$largest_sell = SORT_BY($sell_trades, "volume_usd", "desc").first()

// Trace the proceeds (SOL or USDC) from the seller's wallet
$seller_wallet = $largest_sell.seller_address
$proceeds_trace = TRACE_FUNDS(start_address: $seller_wallet, start_tx: $largest_sell.signature, depth: 3)

**Action:**
RETURN {
  token: "Popcat (POPCAT)",
  largest_sell_order: {
    signature: $largest_sell.signature,
    amount_usd: $largest_sell.volume_usd,
    seller: $seller_wallet
  },
  proceeds_destination: $proceeds_trace.final_summary,
  proceeds_trace_path: $proceeds_trace.path,
  confidence: 92
}


---


