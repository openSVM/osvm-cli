# Token Research - Advanced On-Chain Intelligence (Q201-Q300)

**Category:** Token Research
**Difficulty:** Advanced
**Focus:** MEV analysis, advanced DeFi strategy simulation, cross-chain forensics, smart money tracking, program-level analysis, launch analysis, smart contract decompilation
**Questions:** Q201-Q300

---

## Q5201: "How much MEV was extracted via sandwich attacks on the 'WIF'/'SOL' Raydium pool in the last 24 hours, and which three bots were the most profitable?"

**Expected Plan:**

[TIME: ~90s] [COST: ~0.01 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - JITO_GET_BLOCKS (Jito API)
  - ANALYZE_SANDWICH_ATTACKS (MEV)
  - MAP, FILTER, SUM, GROUP_BY, SORT_BY (Data Processing)

**Main Branch:**
$pool_address = "..." // WIF/SOL Raydium Pool

// Get recent blocks from Jito, which contain MEV bundle data
$blocks = JITO_GET_BLOCKS(limit: 5000) // Approx last ~40 mins

// Analyze transactions in these blocks to identify sandwich attacks targeting the specified pool
$sandwich_attacks = ANALYZE_SANDWICH_ATTACKS(
  blocks: $blocks,
  target_pool: $pool_address
)

GUARD COUNT($sandwich_attacks) > 0 ELSE
  RETURN ERROR(message: "No sandwich attacks found in the sampled blocks.")

// Calculate total and per-bot profit
$total_mev_extracted_sol = SUM(MAP(collection: $sandwich_attacks, fn: a => a.profit_sol))
$bot_profits = GROUP_BY(collection: $sandwich_attacks, field: "bot_address", aggregate: "SUM(profit_sol)")
$top_bots = SORT_BY(collection: $bot_profits, field: "aggregated_value", direction: "desc")

**Decision Point:** Evaluate the intensity of MEV activity.
  BRANCH A ($total_mev_extracted_sol > 100):
    $mev_level = "Extremely High"
    $summary = "The pool is a major target for MEV bots, with significant value being extracted. Traders are experiencing substantial slippage due to sandwich attacks."
  BRANCH B ($total_mev_extracted_sol > 10):
    $mev_level = "High"
    $summary = "The pool is actively targeted by MEV bots. Traders should use Jito bundles or other MEV protection to avoid losses."
  BRANCH C (default):
    $mev_level = "Moderate"
    $summary = "Some MEV activity is present, but it is not extreme. Standard slippage settings may be insufficient during high volume periods."

**Action:**
RETURN {
  mev_level: $mev_level,
  summary: $summary,
  total_mev_extracted_sol_24h_estimate: $total_mev_extracted_sol * 36, // Extrapolate from sample
  top_3_bots: SLICE($top_bots, 0, 3),
  total_sandwiches_detected: COUNT($sandwich_attacks),
  confidence: 90
}

---

## Q5202: "Simulate the 30-day performance of a $10,000 liquidity position in the 'PYTH'/'SOL' Orca Whirlpool (0.3% fee tier), assuming a price range of +/- 20% from the entry price. What would be the impermanent loss vs. fees earned?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.005 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - ORCA_GET_HISTORICAL_DATA (Orca API)
  - SIMULATE_LP_PERFORMANCE (DeFi)

**Main Branch:**
$pool_address = "..." // PYTH/SOL Orca Whirlpool
$investment_usd = 10000
$price_range_pct = 20

// Get historical price, volume, and liquidity data for the pool
$historical_data = ORCA_GET_HISTORICAL_DATA(pool: $pool_address, days: 30)

// Simulate the performance of the concentrated liquidity position
$simulation = SIMULATE_LP_PERFORMANCE(
  historical_data: $historical_data,
  investment_usd: $investment_usd,
  price_range_percentage: $price_range_pct
)

**Decision Point:** Evaluate the profitability of the LP strategy.
  BRANCH A ($simulation.net_pnl_usd > 0):
    $outcome = "Profitable"
    $verdict = "The strategy would have been profitable. The fees earned ($" + $simulation.fees_earned_usd + ") successfully outweighed the impermanent loss ($" + $simulation.impermanent_loss_usd + ")."
  BRANCH B ($simulation.net_pnl_usd <= 0):
    $outcome = "Unprofitable"
    $verdict = "The strategy would have resulted in a net loss. The impermanent loss ($" + $simulation.impermanent_loss_usd + ") was greater than the fees earned ($" + $simulation.fees_earned_usd + ")."

**Action:**
RETURN {
  strategy_outcome: $outcome,
  verdict: $verdict,
  net_pnl_usd: $simulation.net_pnl_usd,
  impermanent_loss_usd: $simulation.impermanent_loss_usd,
  fees_earned_usd: $simulation.fees_earned_usd,
  hodl_value_usd: $simulation.hodl_value_usd,
  lp_position_final_value_usd: $simulation.final_value_usd,
  time_in_range_pct: $simulation.time_in_range_pct,
  confidence: 95
}

---

## Q5203: "For the Jito liquid staking pool, which three validators have received the most 'JitoSOL' stake in the last 7 days, and what is their MEV reward performance compared to their overall uptime?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.008 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - JITO_GET_VALIDATORS (Jito API)
  - JITO_GET_STAKE_DELEGATIONS (Jito API)
  - MAP, FILTER, SORT_BY (Data Processing)

**Main Branch:**
// Get current validator set and their performance metrics from Jito
$validators = JITO_GET_VALIDATORS()

// Get stake delegation changes over the last 7 days
$delegation_changes = JITO_GET_STAKE_DELEGATIONS(time_period: "7d")

// Correlate delegation changes with validator performance
$enriched_validators = MAP(collection: $validators, fn: v => {
  ...v,
  stake_change_7d: FILTER($delegation_changes, d => d.validator == v.pubkey)[0].stake_change
})

$top_new_stake_validators = SORT_BY(collection: $enriched_validators, field: "stake_change_7d", direction: "desc")

**Decision Point:** Analyze the characteristics of the top validators.
  BRANCH A (ALL(MAP(SLICE($top_new_stake_validators, 0, 3), v => v.mev_rewards > 90th_percentile))):
    $reason = "High MEV Rewards"
    $summary = "The validators attracting the most new stake are top-tier MEV performers, indicating the Jito stake pool is effectively allocating to maximize rewards."
  BRANCH B (ALL(MAP(SLICE($top_new_stake_validators, 0, 3), v => v.commission < 5))):
    $reason = "Low Commission"
    $summary = "Validators with the lowest commissions are attracting the most new stake, suggesting stakers are highly sensitive to fees."
  BRANCH C (default):
    $reason = "Mixed Factors"
    $summary = "A mix of factors, including MEV performance, commission rates, and decentralization scores, appear to be driving stake allocation."

**Action:**
RETURN {
  primary_driver_for_new_stake: $reason,
  summary: $summary,
  top_3_validators_by_new_stake: SLICE($top_new_stake_validators, 0, 3),
  confidence: 92
}

---

## Q5204: "A wallet flagged for exploiting a Mango Markets vulnerability on 2023-10-11 bridged 100 ETH to Solana via Wormhole. Trace these funds and identify the final destination addresses or tokens they were swapped into."

**Expected Plan:**

[TIME: ~120s] [COST: ~0.02 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - WORMHOLE_TRACE_FUNDS (Bridge Forensics)
  - SOLANA_TRACE_FUNDS (Forensics)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$source_chain = "Ethereum"
$source_tx_hash = "0x..." // The exploit transaction hash
$bridge = "Wormhole"

// 1. Trace the funds from Ethereum to their arrival on Solana
$bridge_trace = WORMHOLE_TRACE_FUNDS(
  source_chain: $source_chain,
  source_tx_hash: $source_tx_hash
)

GUARD $bridge_trace.completed ELSE
  RETURN ERROR(message: "Failed to trace funds across Wormhole.")

$solana_entry_address = $bridge_trace.destination_address
$solana_entry_amount = $bridge_trace.amount

// 2. Trace the funds from the Solana entry point
$solana_trace = SOLANA_TRACE_FUNDS(
  start_address: $solana_entry_address,
  depth: 5 // Trace 5 levels deep
)

**Decision Point:** Determine the final state of the funds.
  BRANCH A ($solana_trace.is_in_cex):
    $final_destination = "Centralized Exchange"
    $details = "The funds were deposited into a known CEX wallet (" + $solana_trace.cex_address + "), likely to be cashed out."
  BRANCH B ($solana_trace.is_in_defi):
    $final_destination = "DeFi Protocol"
    $details = "The funds are currently held in a DeFi protocol (" + $solana_trace.protocol_name + "), possibly being used for farming or lending."
  BRANCH C ($solana_trace.is_swapped_to_tokens):
    $final_destination = "Swapped to Tokens"
    $details = "The funds were swapped into multiple tokens. The largest positions are: " + $solana_trace.token_holdings
  BRANCH D (default):
    $final_destination = "Dispersed to Wallets"
    $details = "The funds were split and sent to multiple new wallets, indicating an attempt to obfuscate the trail."

**Action:**
RETURN {
  final_destination_type: $final_destination,
  details: $details,
  full_solana_trace_path: $solana_trace.path,
  confidence: 85
}

---

## Q5205: "Identify the top 10 'smart money' wallets that were earliest to invest in 'dogwifhat' (WIF). What other memecoins do these wallets currently hold in common?"

**Expected Plan:**

[TIME: ~80s] [COST: ~0.01 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - IDENTIFY_SMART_MONEY (Analysis)
  - GET_WALLET_PORTFOLIO (Analysis)
  - FIND_COMMON_ASSETS (Analysis)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$target_token = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm" // WIF

// Identify wallets that were early and profitable on the target token
$smart_wallets = IDENTIFY_SMART_MONEY(
  token_mint: $target_token,
  criteria: "early_and_profitable",
  limit: 10
)

// Get the current portfolio for each of these smart wallets
$portfolios = MAP(collection: $smart_wallets, fn: w => GET_WALLET_PORTFOLIO(wallet_address: w.address))

// Find which other memecoins are commonly held across these portfolios
$common_memecoins = FIND_COMMON_ASSETS(
  portfolios: $portfolios,
  category: "memecoin"
)

**Decision Point:** Evaluate the significance of the common holdings.
  BRANCH A (COUNT($common_memecoins) > 0):
    $finding = "Significant Overlap Found"
    $conclusion = "These smart money wallets show a clear pattern of rotating into similar new memecoins. The most commonly held is '" + $common_memecoins[0].name + "', held by " + $common_memecoins[0].holder_count + " of the 10 wallets."
  BRANCH B (default):
    $finding = "No Significant Overlap"
    $conclusion = "The smart money wallets that were early to WIF have since diversified into a wide range of different assets with no clear common theme."

**Action:**
RETURN {
  finding: $finding,
  conclusion: $conclusion,
  top_common_memecoins: $common_memecoins,
  top_10_smart_wallets: $smart_wallets,
  confidence: 90
}

---

## Q5206: "For the Kamino Lend protocol, what were the three most frequently called instructions on its smart contract in the last 7 days, and does this indicate a trend towards 'looping' or standard borrowing?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.007 SOL] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - GET_PROGRAM_INSTRUCTIONS_HISTORY (Analysis)
  - ANALYZE_INSTRUCTION_PATTERNS (Analysis)
  - MAP, GROUP_BY, SORT_BY (Data Processing)

**Main Branch:**
$kamino_program_id = "..."

// Get a history of all instructions called on the Kamino program
$instruction_history = GET_PROGRAM_INSTRUCTIONS_HISTORY(
  program_id: $kamino_program_id,
  time_period: "7d"
)

// Group and count the instructions
$instruction_counts = GROUP_BY(collection: $instruction_history, field: "instruction_name", aggregate: "COUNT")
$top_instructions = SORT_BY(collection: $instruction_counts, field: "aggregated_value", direction: "desc")

// Analyze the sequence of instructions for common user behaviors
$pattern_analysis = ANALYZE_INSTRUCTION_PATTERNS(history: $instruction_history)

**Decision Point:** Interpret the user behavior from instruction patterns.
  BRANCH A ($pattern_analysis.looping_sequences > $pattern_analysis.simple_borrow_sequences):
    $dominant_behavior = "Leverage Looping"
    $summary = "The most common user behavior is 'looping' - repeatedly depositing collateral and borrowing against it to build leveraged positions. This is indicated by a high frequency of sequential 'deposit' and 'borrow' instructions."
  BRANCH B (default):
    $dominant_behavior = "Standard Borrow/Lend"
    $summary = "The most common behaviors are simple deposits and borrows, without immediate re-depositing. This suggests users are primarily using the protocol for standard credit."

**Action:**
RETURN {
  dominant_user_behavior: $dominant_behavior,
  summary: $summary,
  top_3_instructions: SLICE($top_instructions, 0, 3),
  looping_activity_score: $pattern_analysis.looping_score,
  confidence: 93
}

---

## Q5207: "Analyze the sales history for the 'Mad Lads' NFT collection on Tensor. Identify the top 3 wallets that appear to be wash trading based on frequent sales between a closed loop of accounts."

**Expected Plan:**

[TIME: ~90s] [COST: ~0.01 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - TENSOR_GET_SALES_HISTORY (Tensor API)
  - DETECT_WASH_TRADING_LOOPS (Forensics)
  - MAP, FILTER, SORT_BY (Data Processing)

**Main Branch:**
$collection_slug = "mad-lads"

// Get the last 30 days of sales history for the collection
$sales_history = TENSOR_GET_SALES_HISTORY(collection: $collection_slug, days: 30)

// Analyze the sales data to find clusters of wallets trading NFTs among themselves
$wash_trading_analysis = DETECT_WASH_TRADING_LOOPS(sales: $sales_history)

$top_loops = SORT_BY(collection: $wash_trading_analysis.loops, field: "wash_trade_volume_sol", direction: "desc")

**Decision Point:** Assess the severity of wash trading.
  BRANCH A (COUNT($top_loops) > 0 && $top_loops[0].wash_trade_volume_sol > 1000):
    $severity = "High"
    $conclusion = "Significant wash trading detected. A small cluster of wallets is responsible for a large volume of artificial sales, likely to manipulate the collection's perceived value and volume."
  BRANCH B (COUNT($top_loops) > 0):
    $severity = "Moderate"
    $conclusion = "Some evidence of wash trading was found, but the volume is not substantial enough to heavily distort the overall market activity."
  BRANCH C (default):
    $severity = "Low"
    $conclusion = "No significant wash trading loops were detected. The vast majority of sales appear to be legitimate transactions between independent wallets."

**Action:**
RETURN {
  wash_trading_severity: $severity,
  conclusion: $conclusion,
  top_3_wash_trading_loops: SLICE($top_loops, 0, 3),
  total_wash_traded_volume_sol: SUM(MAP($top_loops, l => l.wash_trade_volume_sol)),
  confidence: 88
}

---

## Q5208: "For the recent launch of the 'GME' token on Pump.fun, what percentage of the first 500 buys were from bots, and how much profit did the top 5 bot wallets make in the first hour?"

**Expected Plan:**

[TIME: ~70s] [COST: ~0.008 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - PUMPFUN_GET_LAUNCH_TRANSACTIONS (Pump.fun API)
  - IDENTIFY_BOT_TRANSACTIONS (Analysis)
  - CALCULATE_PNL (Analysis)
  - MAP, FILTER, SUM (Data Processing)

**Main Branch:**
$token_mint = "..." // GME on Pump.fun

// Get the first 500 transactions for the token launch
$launch_txs = PUMPFUN_GET_LAUNCH_TRANSACTIONS(mint: $token_mint, limit: 500)

// Identify which of these transactions were likely made by bots
$bot_analysis = IDENTIFY_BOT_TRANSACTIONS(transactions: $launch_txs)
$bot_txs = FILTER($launch_txs, tx => $bot_analysis[tx.signature].is_bot)
$human_txs = FILTER($launch_txs, tx => !$bot_analysis[tx.signature].is_bot)

$bot_percentage = (COUNT($bot_txs) / COUNT($launch_txs)) * 100

// Calculate PnL for the top bot wallets in the first hour
$top_bots = $bot_analysis.top_bots
$bot_pnl = MAP(collection: SLICE($top_bots, 0, 5), fn: bot => {
  wallet: bot.address,
  pnl_1h_sol: CALCULATE_PNL(wallet: bot.address, token: $token_mint, time_period: "1h")
})

**Decision Point:** Characterize the nature of the launch.
  BRANCH A ($bot_percentage > 75):
    $launch_type = "Bot Dominated"
    $summary = "The launch was heavily dominated by bots, with over 75% of the initial buys being automated. Human buyers faced significant disadvantages."
  BRANCH B ($bot_percentage > 40):
    $launch_type = "Mixed Participation"
    $summary = "Both bots and human traders participated in the launch, but bots still constituted a significant portion of the early buyers."
  BRANCH C (default):
    $launch_type = "Human Driven"
    $summary = "The launch was primarily driven by human buyers, with relatively low bot activity."

**Action:**
RETURN {
  launch_type: $launch_type,
  summary: $summary,
  bot_participation_percentage: $bot_percentage,
  top_5_bot_pnl_first_hour: $bot_pnl,
  confidence: 94
}

---

## Q5209: "Correlate the hourly on-chain trading volume of 'PONKE' with its Twitter sentiment score over the past 3 days. Is there a statistically significant leading relationship between sentiment and volume?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.006 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - BIRDEYE_GET_VOLUME_HISTORY (Birdeye API)
  - TWITTER_GET_SENTIMENT_HISTORY (Social API)
  - CORRELATE_TIME_SERIES (Statistical)

**Main Branch:**
$token_address = "5z3EqYQo9HiCEs3R8qzh3K9ocVbY2cYGDUsf16inMskf" // PONKE
$cashtag = "$PONKE"

// Get hourly volume and sentiment data for the last 3 days (72 hours)
$volume_data = BIRDEYE_GET_VOLUME_HISTORY(token_address: $token_address, time_period: "3d", interval: "1h")
$sentiment_data = TWITTER_GET_SENTIMENT_HISTORY(query: $cashtag, time_period: "3d", interval: "1h")

// Perform a time-series correlation, testing if sentiment leads volume
$correlation = CORRELATE_TIME_SERIES(
  series_A: $sentiment_data, // The potential leading series
  series_B: $volume_data,    // The lagging series
  max_lag_hours: 6           // Test for sentiment leading by up to 6 hours
)

**Decision Point:** Determine the nature of the relationship.
  BRANCH A ($correlation.is_significant && $correlation.best_lag_hours > 0):
    $relationship = "Sentiment Leads Volume"
    $conclusion = "A statistically significant leading relationship was found. Changes in Twitter sentiment precede changes in trading volume by approximately " + $correlation.best_lag_hours + " hours."
  BRANCH B ($correlation.is_significant && $correlation.best_lag_hours == 0):
    $relationship = "Sentiment and Volume are Coincident"
    $conclusion = "Sentiment and volume move together, but neither consistently leads the other. They are likely driven by the same external events (news, influencers)."
  BRANCH C (default):
    $relationship = "No Significant Correlation"
    $conclusion = "There is no statistically significant correlation between Twitter sentiment and on-chain volume for this token in the analyzed period."

**Action:**
RETURN {
  relationship: $relationship,
  conclusion: $conclusion,
  correlation_score_at_best_lag: $correlation.score,
  p_value: $correlation.p_value,
  best_lag_hours: $correlation.best_lag_hours,
  confidence: 85
}

---

## Q5210: "A new, unverified Solana program is attracting significant liquidity. Decompile the program's bytecode, summarize its core functions, and flag any potentially malicious logic like a 'rug pull' function."

**Expected Plan:**

[TIME: ~180s] [COST: ~0.02 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - DECOMPILE_PROGRAM (Security)
  - ANALYZE_DECOMPILED_CODE (Security)

**Main Branch:**
$program_id = "..." // The suspicious, unverified program ID

// 1. Fetch the program's executable data from the chain
$program_account = getAccountInfo(pubkey: $program_id)
GUARD $program_account.executable ELSE
  RETURN ERROR(message: "Address does not belong to an executable program.")
$bytecode = $program_account.data

// 2. Decompile the bytecode into a human-readable (though imperfect) representation
$decompiled_code = DECOMPILE_PROGRAM(bytecode: $bytecode)

// 3. Analyze the decompiled code for common malicious patterns
$security_analysis = ANALYZE_DECOMPILED_CODE(code: $decompiled_code)

**Decision Point:** Assess the security risk based on the analysis.
  BRANCH A ($security_analysis.has_rug_pull_function):
    $risk = "CRITICAL"
    $finding = "A potential 'rug pull' function was identified. It appears to allow an authority to withdraw all SOL or SPL tokens from the program's accounts."
  BRANCH B ($security_analysis.has_unrestricted_authority):
    $risk = "HIGH"
    $finding = "The program has functions that grant an admin key unrestricted power to change critical state or parameters, which could be abused."
  BRANCH C ($security_analysis.has_hidden_fee_mechanism):
    $risk = "MEDIUM"
    $finding = "The code contains logic for a hidden or unusually high fee that is not apparent from the public interface."
  BRANCH D (default):
    $risk = "UNCLEAR"
    $finding = "No obviously malicious functions were found, but the code is complex and obfuscated. The absence of verified source code remains a major risk."

**Action:**
RETURN {
  risk_level: $risk,
  key_finding: $finding,
  function_summary: $security_analysis.function_summaries,
  suspicious_functions: $security_analysis.suspicious_functions,
  confidence: 80,
  caveat: "Decompilation is imperfect and may not fully represent the program's logic. This analysis is not a substitute for a professional audit."
}
