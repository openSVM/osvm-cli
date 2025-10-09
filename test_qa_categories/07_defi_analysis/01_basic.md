# DeFi Analysis - Basic Questions (Q1-Q100)

**Category:** DeFi Analysis  
**Difficulty:** Basic
**Focus:** DEX swaps, liquidity pools, basic DeFi operations

---

## Q1: "What tokens are in Raydium pool XYZ123?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - parseU64 (Solana Utility)

CONST RAYDIUM_AMM_PROGRAM = "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"

**Main Branch:**
$pool_address = "XYZ123"

$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account.owner == RAYDIUM_AMM_PROGRAM ELSE
  RETURN ERROR(message: "Not a Raydium pool")

// Parse pool state (Raydium stores token mints at specific offsets)
$token_a_mint = parsePubkey(data: $pool_account.data, offset: 400)
$token_b_mint = parsePubkey(data: $pool_account.data, offset: 432)

$token_a_reserve = parseU64(data: $pool_account.data, offset: 72)
$token_b_reserve = parseU64(data: $pool_account.data, offset: 80)

**Action:**
RETURN {
  pool: $pool_address,
  token_a: {mint: $token_a_mint, reserve: $token_a_reserve},
  token_b: {mint: $token_b_mint, reserve: $token_b_reserve},
  confidence: 95
}

---

## Q2: "What's the current price in Orca pool ABC456?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - parseU128, POW (Solana Utility, Math)

CONST ORCA_WHIRLPOOL_PROGRAM = "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc"

**Main Branch:**
$pool_address = "ABC456"

$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account.owner == ORCA_WHIRLPOOL_PROGRAM ELSE
  RETURN ERROR(message: "Not an Orca Whirlpool")

// Orca stores sqrt_price_x64 at offset 65
$sqrt_price_x64 = parseU128(data: $pool_account.data, offset: 65)

// Convert to decimal price: (sqrt_price_x64 / 2^64)^2
$price = POW(
  base: $sqrt_price_x64 / POW(base: 2, exponent: 64),
  exponent: 2
)

**Action:**
RETURN {
  pool: $pool_address,
  sqrt_price_x64: $sqrt_price_x64,
  decimal_price: $price,
  note: "Price of token B in terms of token A",
  confidence: 90
}

---

## Q3: "Analyze swap transaction DEF789"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FIND, MAP (Data Processing)

**Main Branch:**
$signature = "DEF789"

$tx = getTransaction(signature: $signature)

// Find swap instruction
$swap_instruction = FIND(
  collection: $tx.message.instructions,
  predicate: inst => inst.programId IN [RAYDIUM_AMM_PROGRAM, ORCA_WHIRLPOOL_PROGRAM, JUPITER_AGGREGATOR_V6]
)

GUARD $swap_instruction != null ELSE
  RETURN ERROR(message: "No swap instruction found")

// Extract amounts from token balances
$pre_balances = $tx.meta.preTokenBalances
$post_balances = $tx.meta.postTokenBalances

$input_amount = $pre_balances[0].uiTokenAmount.uiAmount
$output_amount = $post_balances[0].uiTokenAmount.uiAmount

$price_paid = $output_amount / $input_amount

**Decision Point:** Determine swap direction
  BRANCH A ($input_amount > 0 AND $output_amount > 0):
    $swap_type = "normal_swap"
  BRANCH B ($input_amount > 0 AND $output_amount == 0):
    $swap_type = "failed_or_dust"

**Action:**
RETURN {
  signature: $signature,
  input_amount: $input_amount,
  output_amount: $output_amount,
  price_paid: $price_paid,
  swap_type: $swap_type,
  dex: $swap_instruction.programId,
  confidence: 85
}

---

## Q4: "What is the 24h volume for pool POOL_VOLUME?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - getTransaction (Solana RPC)
  - FILTER, MAP, SUM, COUNT (Data Processing)
  - NOW (Utility)

```ovsm
**Main Branch:**
$pool_address = "POOL_VOLUME"

// Get all signatures for pool in last 24h
$all_sigs = getSignaturesForAddress(address: $pool_address, limit: 1000)

// Filter for last 24 hours
$current_time = NOW()
$cutoff_time = $current_time - (24 * 3600)

$recent_sigs = FILTER(
  collection: $all_sigs,
  predicate: sig => sig.blockTime >= $cutoff_time
)

// Sample transactions to analyze (full analysis would be expensive)
$sample_size = MIN_OF(a: COUNT($recent_sigs), b: 100)
$sample_txs = []

FOR $i IN 0..$sample_size:
  $tx = getTransaction(signature: $recent_sigs[$i].signature)
  $sample_txs = APPEND(array: $sample_txs, item: $tx)

// Extract token balance changes as proxy for volume
$volume_changes = []
FOR $tx IN $sample_txs:
  $pre_token = $tx.meta.preTokenBalances
  $post_token = $tx.meta.postTokenBalances

  FOR $i IN 0..COUNT($post_token):
    $change = ABS(value: $post_token[$i].uiTokenAmount.uiAmount - $pre_token[$i].uiTokenAmount.uiAmount)
    $volume_changes = APPEND(array: $volume_changes, item: $change)

$total_volume_sampled = SUM(data: $volume_changes)

// Extrapolate to full 24h
$extrapolated_volume = ($total_volume_sampled / $sample_size) * COUNT($recent_sigs)

**Decision Point:** Assess volume level
  BRANCH A ($extrapolated_volume > 1000000):
    $volume_category = "high_volume"
    $message = "High activity pool with estimated {$extrapolated_volume} tokens traded in 24h"
  BRANCH B ($extrapolated_volume >= 10000 AND $extrapolated_volume <= 1000000):
    $volume_category = "medium_volume"
    $message = "Moderate volume - {$extrapolated_volume} tokens"
  BRANCH C ($extrapolated_volume < 10000):
    $volume_category = "low_volume"
    $message = "Low volume pool - {$extrapolated_volume} tokens in 24h"

**Action:**
RETURN {
  pool: $pool_address,
  transactions_24h: COUNT($recent_sigs),
  sampled: $sample_size,
  estimated_volume_24h: $extrapolated_volume,
  volume_category: $volume_category,
  message: $message,
  note: "Estimated from sample - not exact volume",
  confidence: 80
}
```

---

## Q5: "What is the current price impact for swapping 1000 tokens in pool POOL_IMPACT?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - parseU64 (Solana Utility)

```ovsm
**Main Branch:**
$pool_address = "POOL_IMPACT"
$swap_amount = 1000

$pool_account = getAccountInfo(pubkey: $pool_address)

// Parse pool reserves (AMM constant product formula)
$reserve_a = parseU64(data: $pool_account.data, offset: 72)
$reserve_b = parseU64(data: $pool_account.data, offset: 80)

// Calculate output using constant product (x * y = k)
$k = $reserve_a * $reserve_b
$new_reserve_a = $reserve_a + $swap_amount
$new_reserve_b = $k / $new_reserve_a
$output_amount = $reserve_b - $new_reserve_b

// Calculate price impact
$expected_price = $reserve_b / $reserve_a
$actual_price = $output_amount / $swap_amount
$price_impact_pct = (($expected_price - $actual_price) / $expected_price) * 100

**Decision Point:** Assess price impact severity
  BRANCH A ($price_impact_pct > 5):
    $severity = "high"
    $recommendation = "Large impact - consider splitting trade"
  BRANCH B ($price_impact_pct >= 1 AND $price_impact_pct <= 5):
    $severity = "moderate"
    $recommendation = "Acceptable for most trades"
  BRANCH C ($price_impact_pct < 1):
    $severity = "low"
    $recommendation = "Minimal slippage"

**Action:**
RETURN {
  pool: $pool_address,
  swap_amount: $swap_amount,
  output_amount: $output_amount,
  price_impact_pct: $price_impact_pct,
  severity: $severity,
  recommendation: $recommendation,
  confidence: 85
}
```

---

## Q6: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenLargestAccounts, getTokenAccountsByOwner
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account to find LP token mint
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool account not found")

// Parse LP token mint address (offset varies by AMM, using Raydium example)
$lp_mint = parsePublicKey(data: $pool_account.data, offset: 40)

// Get largest LP token holders
$largest_holders = getTokenLargestAccounts(mint: $lp_mint)

// Get total LP supply
$supply_info = getTokenSupply(mint: $lp_mint)
$total_supply = $supply_info.value.uiAmount

// Calculate share percentage for each holder
$lp_details = MAP(collection: $largest_holders.value, fn: holder => {
  owner: holder.address,
  lp_balance: holder.uiAmount,
  share_percentage: (holder.uiAmount / $total_supply) * 100,
  rank: holder.rank
})

// Filter out very small positions (< 0.01%)
$significant_lps = FILTER(collection: $lp_details, fn: lp => lp.share_percentage >= 0.01)

// Sort by share percentage descending
$sorted_lps = SORT(collection: $significant_lps, by: "share_percentage", order: "desc")

// Count total LPs
$total_lps = COUNT(collection: $sorted_lps)

// Calculate concentration (top 10 holders)
$top_10 = SLICE(array: $sorted_lps, start: 0, end: 10)
$top_10_concentration = SUM(collection: MAP($top_10, fn: lp => lp.share_percentage))

**Decision Point:** Assess liquidity concentration
  BRANCH A ($top_10_concentration > 80):
    $concentration_level = "highly_concentrated"
    $risk = "High centralization risk - top 10 LPs control {$top_10_concentration}%"
  BRANCH B ($top_10_concentration >= 50 AND $top_10_concentration <= 80):
    $concentration_level = "moderately_concentrated"
    $risk = "Moderate concentration"
  BRANCH C ($top_10_concentration < 50):
    $concentration_level = "well_distributed"
    $risk = "Low concentration risk"

**Action:**
RETURN {
  pool_address: $pool_address,
  lp_mint: $lp_mint,
  total_lp_supply: $total_supply,
  total_providers: $total_lps,
  top_providers: $top_10,
  top_10_concentration_pct: $top_10_concentration,
  concentration_level: $concentration_level,
  risk_assessment: $risk,
  confidence: 89
}
```

---

## Q7: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenSupply
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$farm_address = "FarmXYZ"

// Get farm account
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm account not found")

// Parse farm data (Raydium farm example)
// Offset 8: reward_per_second (u64)
// Offset 16: total_staked (u64)
// Offset 24: reward_token_mint (pubkey)
// Offset 56: staked_token_mint (pubkey)

$reward_per_second = parseU64(data: $farm_account.data, offset: 8)
$total_staked = parseU64(data: $farm_account.data, offset: 16)
$reward_token_mint = parsePublicKey(data: $farm_account.data, offset: 24)
$staked_token_mint = parsePublicKey(data: $farm_account.data, offset: 56)

// Get token prices (simplified - would use real price oracle in production)
$reward_price_usd = 1.5  // Example: reward token = $1.50
$staked_price_usd = 20.0  // Example: staked token = $20.00

// Calculate annual rewards
$seconds_per_year = 31536000
$annual_rewards = $reward_per_second * $seconds_per_year

// Convert to USD value
$annual_rewards_usd = ($annual_rewards / LAMPORTS_PER_SOL) * $reward_price_usd
$total_staked_usd = ($total_staked / LAMPORTS_PER_SOL) * $staked_price_usd

// Calculate APY percentage
$apy_pct = ($annual_rewards_usd / $total_staked_usd) * 100

// Calculate daily rewards per 1000 tokens staked
$daily_per_1000 = ($apy_pct / 365) * 10

**Decision Point:** Assess APY attractiveness
  BRANCH A ($apy_pct > 100):
    $rating = "very_high"
    $risk_note = "Extremely high APY may indicate unsustainable emissions or high risk"
  BRANCH B ($apy_pct >= 30 AND $apy_pct <= 100):
    $rating = "attractive"
    $risk_note = "Competitive APY for DeFi farming"
  BRANCH C ($apy_pct >= 10 AND $apy_pct < 30):
    $rating = "moderate"
    $risk_note = "Moderate returns"
  BRANCH D ($apy_pct < 10):
    $rating = "low"
    $risk_note = "Low APY - may not compensate for impermanent loss risk"

**Action:**
RETURN {
  farm_address: $farm_address,
  apy_percentage: $apy_pct,
  annual_rewards_usd: $annual_rewards_usd,
  total_staked_usd: $total_staked_usd,
  daily_per_1000_tokens: $daily_per_1000,
  reward_token_mint: $reward_token_mint,
  staked_token_mint: $staked_token_mint,
  rating: $rating,
  risk_note: $risk_note,
  confidence: 88
}
```

---

## Q8: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$user_address = "UserXYZ"
$farm_address = "FarmXYZ"

// Find user's stake account PDA (Program Derived Address)
// Seeds typically: ["user_stake", farm_address, user_address]
$stake_pda = findProgramAddress(
  seeds: ["user_stake", $farm_address, $user_address],
  programId: RAYDIUM_STAKING_PROGRAM
)

// Get user's stake account
$stake_account = getAccountInfo(pubkey: $stake_pda)

GUARD $stake_account != null ELSE
  RETURN ERROR(message: "User has no active stake in this farm")

// Get farm account for reward calculation
$farm_account = getAccountInfo(pubkey: $farm_address)

// Parse user stake data
// Offset 8: staked_amount (u64)
// Offset 16: reward_debt (u64) - used for reward calculation
// Offset 24: last_update_time (i64)
// Offset 32: pending_rewards (u64)

$staked_amount = parseU64(data: $stake_account.data, offset: 8)
$reward_debt = parseU64(data: $stake_account.data, offset: 16)
$last_update_time = parseI64(data: $stake_account.data, offset: 24)
$pending_rewards = parseU64(data: $stake_account.data, offset: 32)

// Parse farm data for current reward calculations
$acc_reward_per_share = parseU128(data: $farm_account.data, offset: 88)
$reward_per_second = parseU64(data: $farm_account.data, offset: 8)

// Calculate newly accrued rewards since last update
$current_time = getCurrentTimestamp()
$time_elapsed = $current_time - $last_update_time
$new_rewards = ($staked_amount * $acc_reward_per_share / 1e12) - $reward_debt

// Total pending rewards
$total_pending = $pending_rewards + $new_rewards

// Convert to UI amount
$pending_ui_amount = $total_pending / LAMPORTS_PER_SOL

// Estimate USD value (example price)
$reward_token_price = 1.5
$pending_usd = $pending_ui_amount * $reward_token_price

// Calculate estimated daily earnings
$daily_rate = ($reward_per_second * 86400 * $staked_amount) / parseU64(data: $farm_account.data, offset: 16)
$daily_earnings_ui = $daily_rate / LAMPORTS_PER_SOL
$daily_earnings_usd = $daily_earnings_ui * $reward_token_price

**Decision Point:** Assess reward claim urgency
  BRANCH A ($pending_ui_amount > 100):
    $recommendation = "Large pending rewards - consider claiming to avoid loss"
    $urgency = "high"
  BRANCH B ($pending_ui_amount >= 10 AND $pending_ui_amount <= 100):
    $recommendation = "Moderate rewards - claim when transaction fees are low"
    $urgency = "medium"
  BRANCH C ($pending_ui_amount < 10):
    $recommendation = "Small rewards - wait to accumulate more before claiming"
    $urgency = "low"

**Action:**
RETURN {
  user_address: $user_address,
  farm_address: $farm_address,
  staked_amount_ui: $staked_amount / LAMPORTS_PER_SOL,
  pending_rewards_ui: $pending_ui_amount,
  pending_rewards_usd: $pending_usd,
  daily_earnings_ui: $daily_earnings_ui,
  daily_earnings_usd: $daily_earnings_usd,
  time_since_last_update_seconds: $time_elapsed,
  recommendation: $recommendation,
  urgency: $urgency,
  confidence: 87
}
```

---

## Q9: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolXYZ"

// Get lending pool reserve account (Solend/Kamino example)
$reserve_account = getAccountInfo(pubkey: $pool_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Lending pool reserve not found")

// Parse reserve account data
// Offset 8: version (u8)
// Offset 16: available_liquidity (u64)
// Offset 24: borrowed_amount_wads (u128)
// Offset 40: cumulative_borrow_rate_wads (u128)
// Offset 56: market_price (u128)

$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)

// Convert borrowed amount from wads (1e18 scale) to normal amount
$borrowed_amount = $borrowed_amount_wads / 1e18

// Calculate total supply
$total_supply = $available_liquidity + $borrowed_amount

// Calculate utilization rate as percentage
$utilization_rate = ($borrowed_amount / $total_supply) * 100

// Convert to UI amounts for display
$available_ui = $available_liquidity / LAMPORTS_PER_SOL
$borrowed_ui = $borrowed_amount / LAMPORTS_PER_SOL
$total_supply_ui = $total_supply / LAMPORTS_PER_SOL

**Decision Point:** Assess pool health and risk
  BRANCH A ($utilization_rate > 90):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Critically high utilization - may limit withdrawals, increased liquidation risk"
  BRANCH B ($utilization_rate >= 80 AND $utilization_rate <= 90):
    $health_status = "stressed"
    $risk_level = "elevated"
    $note = "High utilization - monitor for liquidity crunches"
  BRANCH C ($utilization_rate >= 50 AND $utilization_rate < 80):
    $health_status = "healthy"
    $risk_level = "moderate"
    $note = "Optimal utilization range for lending pool"
  BRANCH D ($utilization_rate < 50):
    $health_status = "underutilized"
    $risk_level = "low"
    $note = "Low utilization - excess idle capital, lower APY for lenders"

**Action:**
RETURN {
  pool_address: $pool_address,
  utilization_rate_pct: $utilization_rate,
  available_liquidity_ui: $available_ui,
  borrowed_amount_ui: $borrowed_ui,
  total_supply_ui: $total_supply_ui,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 86
}
```

---

## Q10: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$asset_mint = "AssetXYZ"
$reserve_address = "ReserveXYZ"  // Reserve account for this asset

// Get reserve account
$reserve_account = getAccountInfo(pubkey: $reserve_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Asset reserve not found")

// Parse reserve configuration and state
// Offset 88: current_borrow_rate (u128 in wads - 1e18)
// Offset 104: cumulative_borrow_rate (u128 in wads)
// Offset 120: optimal_utilization_rate (u8 - percentage)
// Offset 121: max_borrow_rate (u64)

$current_borrow_rate_wads = parseU128(data: $reserve_account.data, offset: 88)
$optimal_utilization = parseU8(data: $reserve_account.data, offset: 120)
$max_borrow_rate = parseU64(data: $reserve_account.data, offset: 121)

// Convert borrow rate from wads (per second) to APY percentage
// Formula: APY = (1 + rate_per_second)^seconds_per_year - 1
$rate_per_second = $current_borrow_rate_wads / 1e18
$seconds_per_year = 31536000
$apy_multiplier = POW(base: (1 + $rate_per_second), exponent: $seconds_per_year)
$borrow_apy_pct = ($apy_multiplier - 1) * 100

// Simplified calculation (linear approximation for readability)
$borrow_apy_simple = $rate_per_second * $seconds_per_year * 100

// Get current utilization to explain rate
$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)
$borrowed_amount = $borrowed_amount_wads / 1e18
$current_utilization = ($borrowed_amount / ($available_liquidity + $borrowed_amount)) * 100

**Decision Point:** Assess borrow rate competitiveness
  BRANCH A ($borrow_apy_simple > 20):
    $rating = "expensive"
    $note = "High borrow cost - {$borrow_apy_simple}% APY. Consider alternative protocols or wait for lower rates"
  BRANCH B ($borrow_apy_simple >= 10 AND $borrow_apy_simple <= 20):
    $rating = "moderate"
    $note = "Standard borrow rate for current market conditions"
  BRANCH C ($borrow_apy_simple >= 5 AND $borrow_apy_simple < 10):
    $rating = "attractive"
    $note = "Competitive borrow rate - good opportunity for leverage"
  BRANCH D ($borrow_apy_simple < 5):
    $rating = "very_attractive"
    $note = "Exceptionally low borrow rate - excellent leverage opportunity"

**Action:**
RETURN {
  asset_mint: $asset_mint,
  reserve_address: $reserve_address,
  borrow_apy_pct: $borrow_apy_simple,
  current_utilization_pct: $current_utilization,
  optimal_utilization_pct: $optimal_utilization,
  max_borrow_rate: $max_borrow_rate,
  rating: $rating,
  note: $note,
  confidence: 85
}
```

---

## Q11: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$user_address = "UserXYZ"
$protocol = "Solend"  // or Kamino, MarginFi, etc.

// Find user's obligation account (holds collateral + borrows)
$obligation_pda = findProgramAddress(
  seeds: ["obligation", $user_address],
  programId: SOLEND_PROGRAM
)

$obligation_account = getAccountInfo(pubkey: $obligation_pda)

GUARD $obligation_account != null ELSE
  RETURN ERROR(message: "User has no active position in lending protocol")

// Parse obligation data
// Offset 8: deposited_value (u128 in USD with 18 decimals)
// Offset 24: borrowed_value (u128 in USD with 18 decimals)
// Offset 40: allowed_borrow_value (u128 - max borrow based on LTV)
// Offset 56: unhealthy_borrow_value (u128 - liquidation threshold)

$deposited_value_raw = parseU128(data: $obligation_account.data, offset: 8)
$borrowed_value_raw = parseU128(data: $obligation_account.data, offset: 24)
$allowed_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 40)
$unhealthy_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 56)

// Convert from 1e18 scale to normal USD
$deposited_value_usd = $deposited_value_raw / 1e18
$borrowed_value_usd = $borrowed_value_raw / 1e18
$allowed_borrow_usd = $allowed_borrow_value_raw / 1e18
$liquidation_threshold_usd = $unhealthy_borrow_value_raw / 1e18

// Calculate collateral ratio (collateral / debt * 100)
TRY {
  $collateral_ratio_pct = ($deposited_value_usd / $borrowed_value_usd) * 100
} CATCH {
  // Handle case where borrowed_value is 0
  $collateral_ratio_pct = 99999  // Infinite collateral (no debt)
}

// Calculate utilization (how much of allowed borrow is used)
$borrow_utilization_pct = ($borrowed_value_usd / $allowed_borrow_usd) * 100

// Calculate distance to liquidation
$liquidation_buffer_usd = $liquidation_threshold_usd - $borrowed_value_usd
$liquidation_buffer_pct = ($liquidation_buffer_usd / $borrowed_value_usd) * 100

**Decision Point:** Assess position health
  BRANCH A ($collateral_ratio_pct < 110):
    $health = "critical"
    $risk = "immediate_liquidation_risk"
    $action = "URGENT: Add collateral or repay debt immediately"
  BRANCH B ($collateral_ratio_pct >= 110 AND $collateral_ratio_pct < 125):
    $health = "unhealthy"
    $risk = "high_liquidation_risk"
    $action = "Add collateral soon to avoid liquidation"
  BRANCH C ($collateral_ratio_pct >= 125 AND $collateral_ratio_pct < 150):
    $health = "moderate"
    $risk = "moderate_risk"
    $action = "Monitor position during market volatility"
  BRANCH D ($collateral_ratio_pct >= 150):
    $health = "healthy"
    $risk = "low_risk"
    $action = "Position is safe - could borrow more if desired"

**Action:**
RETURN {
  user_address: $user_address,
  collateral_ratio_pct: $collateral_ratio_pct,
  deposited_value_usd: $deposited_value_usd,
  borrowed_value_usd: $borrowed_value_usd,
  borrow_utilization_pct: $borrow_utilization_pct,
  liquidation_threshold_usd: $liquidation_threshold_usd,
  liquidation_buffer_usd: $liquidation_buffer_usd,
  health: $health,
  risk: $risk,
  recommended_action: $action,
  confidence: 84
}
```

---

## Q12: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$position_address = "PosXYZ"

// Get position/obligation account
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position not found")

// Parse position deposits and borrows arrays
// Offset 72: num_deposits (u8)
// Offset 73: deposits array (each entry 88 bytes)
// Offset varies: num_borrows (u8)
// Then borrows array

$num_deposits = parseU8(data: $position_account.data, offset: 72)
$deposits_offset = 73

// Collect reserve addresses for each deposit to get LTV ratios
$deposit_reserves = []
$deposit_amounts = []

// Parse each deposit (simplified - would loop in real implementation)
FOR $i IN RANGE(0, $num_deposits) {
  $deposit_offset = $deposits_offset + ($i * 88)
  $reserve_address = parsePublicKey(data: $position_account.data, offset: $deposit_offset)
  $deposited_amount = parseU128(data: $position_account.data, offset: $deposit_offset + 32)

  APPEND($deposit_reserves, $reserve_address)
  APPEND($deposit_amounts, $deposited_amount / 1e18)
}

// Get reserve configs to find liquidation LTV for each asset
$reserve_accounts = getMultipleAccounts(pubkeys: $deposit_reserves)

// Calculate weighted liquidation threshold
$total_collateral_usd = 0
$liquidation_value_usd = 0

FOR $i IN RANGE(0, $num_deposits) {
  $reserve_config = $reserve_accounts[$i]

  // Parse reserve liquidation LTV (offset 200)
  $liquidation_ltv_pct = parseU8(data: $reserve_config.data, offset: 200)

  // Get market price for this asset (offset 56)
  $price_usd = parseU128(data: $reserve_config.data, offset: 56) / 1e18

  $collateral_value = $deposit_amounts[$i] * $price_usd
  $total_collateral_usd = $total_collateral_usd + $collateral_value
  $liquidation_value_usd = $liquidation_value_usd + ($collateral_value * $liquidation_ltv_pct / 100)
}

// Get current borrow value
$borrowed_value_raw = parseU128(data: $position_account.data, offset: 24)
$borrowed_value_usd = $borrowed_value_raw / 1e18

// Calculate how close to liquidation
$buffer_usd = $liquidation_value_usd - $borrowed_value_usd
$buffer_pct = ($buffer_usd / $liquidation_value_usd) * 100

// Calculate price drop needed for liquidation
$price_drop_for_liquidation_pct = ($buffer_usd / $total_collateral_usd) * 100

**Decision Point:** Assess liquidation risk
  BRANCH A ($buffer_pct < 5):
    $risk_level = "critical"
    $warning = "URGENT: Position is {$buffer_pct}% from liquidation"
  BRANCH B ($buffer_pct >= 5 AND $buffer_pct < 15):
    $risk_level = "high"
    $warning = "High risk - only {$price_drop_for_liquidation_pct}% price drop until liquidation"
  BRANCH C ($buffer_pct >= 15 AND $buffer_pct < 30):
    $risk_level = "moderate"
    $warning = "Monitor closely during volatility"
  BRANCH D ($buffer_pct >= 30):
    $risk_level = "low"
    $warning = "Position is well collateralized"

**Action:**
RETURN {
  position_address: $position_address,
  total_collateral_usd: $total_collateral_usd,
  borrowed_value_usd: $borrowed_value_usd,
  liquidation_threshold_usd: $liquidation_value_usd,
  buffer_to_liquidation_usd: $buffer_usd,
  buffer_percentage: $buffer_pct,
  price_drop_for_liquidation_pct: $price_drop_for_liquidation_pct,
  risk_level: $risk_level,
  warning: $warning,
  confidence: 83
}
```

---

## Q13: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenAccountBalance
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse pool data (Raydium/Orca AMM example)
// Offset 40: token_a_vault (pubkey)
// Offset 72: token_b_vault (pubkey)
// Offset 104: token_a_mint (pubkey)
// Offset 136: token_b_mint (pubkey)

$token_a_vault = parsePublicKey(data: $pool_account.data, offset: 40)
$token_b_vault = parsePublicKey(data: $pool_account.data, offset: 72)
$token_a_mint = parsePublicKey(data: $pool_account.data, offset: 104)
$token_b_mint = parsePublicKey(data: $pool_account.data, offset: 136)

// Get token balances in vaults
$vault_a_balance_info = getTokenAccountBalance(pubkey: $token_a_vault)
$vault_b_balance_info = getTokenAccountBalance(pubkey: $token_b_vault)

$reserve_a_ui = $vault_a_balance_info.value.uiAmount
$reserve_b_ui = $vault_b_balance_info.value.uiAmount

// Get token prices (simplified - would use Pyth/Switchboard oracle in production)
$price_a_usd = 20.0   // Example: Token A = $20
$price_b_usd = 1.0    // Example: Token B = $1 (stablecoin)

// Calculate TVL
$tvl_token_a = $reserve_a_ui * $price_a_usd
$tvl_token_b = $reserve_b_ui * $price_b_usd
$total_tvl_usd = $tvl_token_a + $tvl_token_b

// Calculate pool composition
$token_a_pct = ($tvl_token_a / $total_tvl_usd) * 100
$token_b_pct = ($tvl_token_b / $total_tvl_usd) * 100

// Check if pool is balanced (50/50 is ideal for constant product AMM)
$balance_deviation = ABS($token_a_pct - 50)

**Decision Point:** Assess pool size and balance
  BRANCH A ($total_tvl_usd > 10000000):
    $size_category = "large"
    $liquidity_note = "Deep liquidity - low slippage for large trades"
  BRANCH B ($total_tvl_usd >= 1000000 AND $total_tvl_usd <= 10000000):
    $size_category = "medium"
    $liquidity_note = "Moderate liquidity - suitable for most trades"
  BRANCH C ($total_tvl_usd >= 100000 AND $total_tvl_usd < 1000000):
    $size_category = "small"
    $liquidity_note = "Limited liquidity - watch for price impact"
  BRANCH D ($total_tvl_usd < 100000):
    $size_category = "very_small"
    $liquidity_note = "Very low liquidity - high slippage risk"

**Decision Point:** Assess pool balance
  BRANCH A ($balance_deviation > 20):
    $balance_status = "imbalanced"
    $balance_note = "Pool significantly imbalanced - possible arbitrage opportunity"
  BRANCH B ($balance_deviation > 10 AND $balance_deviation <= 20):
    $balance_status = "slightly_imbalanced"
    $balance_note = "Minor imbalance - normal for active pools"
  BRANCH C ($balance_deviation <= 10):
    $balance_status = "balanced"
    $balance_note = "Well-balanced pool composition"

**Action:**
RETURN {
  pool_address: $pool_address,
  total_tvl_usd: $total_tvl_usd,
  token_a_mint: $token_a_mint,
  token_b_mint: $token_b_mint,
  token_a_reserve: $reserve_a_ui,
  token_b_reserve: $reserve_b_ui,
  token_a_value_usd: $tvl_token_a,
  token_b_value_usd: $tvl_token_b,
  token_a_percentage: $token_a_pct,
  token_b_percentage: $token_b_pct,
  size_category: $size_category,
  balance_status: $balance_status,
  liquidity_note: $liquidity_note,
  balance_note: $balance_note,
  confidence: 82
}
```

---

## Q14: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolXYZ"
$current_time = getCurrentTimestamp()
$time_24h_ago = $current_time - 86400  // 86400 seconds = 24 hours

// Get all transactions for the pool in last 24h
$signatures = getSignaturesForAddress(
  address: $pool_address,
  options: {
    until: null,
    limit: 1000  // May need pagination for very active pools
  }
)

// Filter transactions from last 24 hours
$recent_signatures = FILTER(
  collection: $signatures,
  fn: sig => sig.blockTime >= $time_24h_ago
)

// Fetch full transaction details to analyze swap amounts
$transactions = MAP(
  collection: $recent_signatures,
  fn: sig => getTransaction(signature: sig.signature)
)

// Filter only successful swap transactions
$swap_txs = FILTER(
  collection: $transactions,
  fn: tx => tx != null AND tx.meta.err == null
)

// Extract swap volumes from token balance changes
$volumes_usd = MAP(collection: $swap_txs, fn: tx => {
  // Parse pre/post token balances to calculate swap amounts
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  // Find pool vault balance changes
  $vault_changes = []
  FOR $i IN RANGE(0, COUNT($post_balances)) {
    $pre = $pre_balances[$i].uiTokenAmount.uiAmount
    $post = $post_balances[$i].uiTokenAmount.uiAmount
    $change = ABS($post - $pre)
    APPEND($vault_changes, $change)
  }

  // Estimate USD value (simplified)
  $max_change = MAX(array: $vault_changes)
  $swap_value_usd = $max_change * 20.0  // Assume $20 token price
  RETURN $swap_value_usd
})

// Calculate total 24h volume
$total_volume_24h = SUM(collection: $volumes_usd)

// Calculate number of swaps
$swap_count = COUNT(collection: $swap_txs)

// Calculate average swap size
$avg_swap_size = $total_volume_24h / $swap_count

// Calculate volume per hour
$volume_per_hour = $total_volume_24h / 24

**Decision Point:** Assess trading activity
  BRANCH A ($total_volume_24h > 10000000):
    $activity_level = "very_high"
    $note = "Highly active pool - ${volume_per_hour}/hour average"
  BRANCH B ($total_volume_24h >= 1000000 AND $total_volume_24h <= 10000000):
    $activity_level = "high"
    $note = "Active trading - good liquidity depth"
  BRANCH C ($total_volume_24h >= 100000 AND $total_volume_24h < 1000000):
    $activity_level = "moderate"
    $note = "Moderate activity level"
  BRANCH D ($total_volume_24h < 100000):
    $activity_level = "low"
    $note = "Low trading volume - limited activity"

**Action:**
RETURN {
  pool_address: $pool_address,
  volume_24h_usd: $total_volume_24h,
  volume_per_hour_usd: $volume_per_hour,
  total_swaps: $swap_count,
  average_swap_size_usd: $avg_swap_size,
  time_period_start: $time_24h_ago,
  time_period_end: $current_time,
  activity_level: $activity_level,
  note: $note,
  confidence: 81
}
```

---

## Q15: "What is the current price impact for swapping 1000 tokens?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolSwapXYZ"
$swap_amount = 1000
$token_in_mint = "TokenA"
$token_out_mint = "TokenB"

// Get pool reserves
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse reserve amounts
$reserve_in = parseU64(data: $pool_account.data, offset: 72)
$reserve_out = parseU64(data: $pool_account.data, offset: 80)

// Calculate expected output with constant product formula (x * y = k)
$swap_amount_lamports = $swap_amount * LAMPORTS_PER_SOL
$k = $reserve_in * $reserve_out

// Account for 0.3% fee (typical for AMMs)
$fee_pct = 0.3
$amount_in_with_fee = $swap_amount_lamports * (100 - $fee_pct) / 100

// Calculate output amount
$new_reserve_in = $reserve_in + $amount_in_with_fee
$new_reserve_out = $k / $new_reserve_in
$output_amount = $reserve_out - $new_reserve_out

// Calculate expected price without slippage
$current_price = $reserve_out / $reserve_in
$expected_output_no_slippage = $swap_amount_lamports * $current_price

// Calculate actual execution price
$execution_price = $output_amount / $swap_amount_lamports

// Calculate price impact percentage
$price_impact_pct = (($expected_output_no_slippage - $output_amount) / $expected_output_no_slippage) * 100

// Convert to UI amounts
$output_ui = $output_amount / LAMPORTS_PER_SOL
$expected_output_ui = $expected_output_no_slippage / LAMPORTS_PER_SOL

// Calculate absolute loss due to price impact
$impact_loss_ui = $expected_output_ui - $output_ui

**Decision Point:** Assess price impact severity
  BRANCH A ($price_impact_pct > 5):
    $severity = "very_high"
    $recommendation = "CAUTION: {$price_impact_pct}% impact. Consider splitting into smaller trades or using limit orders"
  BRANCH B ($price_impact_pct >= 2 AND $price_impact_pct <= 5):
    $severity = "high"
    $recommendation = "High impact - evaluate if acceptable for your trade"
  BRANCH C ($price_impact_pct >= 0.5 AND $price_impact_pct < 2):
    $severity = "moderate"
    $recommendation = "Moderate impact - typical for this trade size"
  BRANCH D ($price_impact_pct < 0.5):
    $severity = "low"
    $recommendation = "Low impact - good liquidity for this trade size"

**Action:**
RETURN {
  pool_address: $pool_address,
  swap_amount_in: $swap_amount,
  expected_output: $output_ui,
  expected_without_impact: $expected_output_ui,
  price_impact_pct: $price_impact_pct,
  impact_loss_tokens: $impact_loss_ui,
  execution_price: $execution_price,
  current_market_price: $current_price,
  fee_percentage: $fee_pct,
  severity: $severity,
  recommendation: $recommendation,
  confidence: 80
}
```

---

## Q16: "What is the impermanent loss for LP position in pool PoolXYZ?"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "PoolXYZ"
$lp_position_address = "LpPosXYZ"
$deposit_tx_signature = "DepositTxSig"

// Get current pool reserves
$pool_account = getAccountInfo(pubkey: $pool_address)
$current_reserve_a = parseU64(data: $pool_account.data, offset: 72)
$current_reserve_b = parseU64(data: $pool_account.data, offset: 80)

// Calculate current price ratio
$current_price_ratio = $current_reserve_b / $current_reserve_a

// Get original deposit transaction to find initial deposit amounts
$deposit_tx = getTransaction(signature: $deposit_tx_signature)
$initial_deposit_a_lamports = parseU64(data: $deposit_tx.meta.postTokenBalances[0].uiTokenAmount.amount)
$initial_deposit_b_lamports = parseU64(data: $deposit_tx.meta.postTokenBalances[1].uiTokenAmount.amount)

// Calculate initial price ratio at deposit time
$initial_price_ratio = $initial_deposit_b_lamports / $initial_deposit_a_lamports

// Get LP token balance (user's share of pool)
$lp_account = getAccountInfo(pubkey: $lp_position_address)
$lp_tokens_held = parseU64(data: $lp_account.data, offset: 64)

// Get total LP supply
$lp_mint = parsePublicKey(data: $pool_account.data, offset: 40)
$lp_supply_info = getTokenSupply(mint: $lp_mint)
$total_lp_supply = $lp_supply_info.value.amount

// Calculate user's share of pool
$pool_share_pct = ($lp_tokens_held / $total_lp_supply) * 100

// Calculate current position value
$current_amount_a = $current_reserve_a * ($lp_tokens_held / $total_lp_supply)
$current_amount_b = $current_reserve_b * ($lp_tokens_held / $total_lp_supply)

// Calculate what the position would be worth if held (not LP'd)
// Using current prices
$price_change_ratio = $current_price_ratio / $initial_price_ratio
$held_value_a = $initial_deposit_a_lamports
$held_value_b = $initial_deposit_b_lamports
$total_held_value = $held_value_a + ($held_value_b * $current_price_ratio)

// Calculate current LP position value
$current_lp_value = $current_amount_a + ($current_amount_b * $current_price_ratio)

// Calculate impermanent loss
$impermanent_loss_lamports = $total_held_value - $current_lp_value
$impermanent_loss_pct = ($impermanent_loss_lamports / $total_held_value) * 100

// Convert to UI amounts
$il_ui = $impermanent_loss_lamports / LAMPORTS_PER_SOL

// Calculate if trading fees have compensated for IL
$fees_earned_estimate = $current_lp_value * 0.003 * 30  // Assume 0.3% fee * 30 days activity

**Decision Point:** Assess impermanent loss impact
  BRANCH A ($impermanent_loss_pct > 10):
    $severity = "high"
    $note = "Significant IL - {$impermanent_loss_pct}% loss vs holding"
  BRANCH B ($impermanent_loss_pct >= 5 AND $impermanent_loss_pct <= 10):
    $severity = "moderate"
    $note = "Moderate IL - fees may offset losses"
  BRANCH C ($impermanent_loss_pct > 0 AND $impermanent_loss_pct < 5):
    $severity = "low"
    $note = "Minor IL - likely offset by trading fees"
  BRANCH D ($impermanent_loss_pct <= 0):
    $severity = "none"
    $note = "No impermanent loss - price ratio remained stable"

**Action:**
RETURN {
  pool_address: $pool_address,
  initial_price_ratio: $initial_price_ratio,
  current_price_ratio: $current_price_ratio,
  price_change_pct: ($price_change_ratio - 1) * 100,
  impermanent_loss_pct: $impermanent_loss_pct,
  impermanent_loss_ui: $il_ui,
  current_position_value: $current_lp_value / LAMPORTS_PER_SOL,
  held_position_value: $total_held_value / LAMPORTS_PER_SOL,
  pool_share_pct: $pool_share_pct,
  estimated_fees_earned: $fees_earned_estimate / LAMPORTS_PER_SOL,
  severity: $severity,
  note: $note,
  confidence: 79
}
```

---

## Q17: "Find all arbitrage opportunities between DEX pools?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

```ovsm
**Main Branch:**
$token_mint = "TokenABC"
$dex_pool_addresses = [
  "RaydiumPoolAddr",
  "OrcaPoolAddr",
  "SerumPoolAddr"
]

// Get all pool accounts at once
$pool_accounts = getMultipleAccounts(pubkeys: $dex_pool_addresses)

// Extract prices from each pool
$pool_prices = MAP(collection: $pool_accounts, fn: pool => {
  // Parse reserves for each pool
  $reserve_token = parseU64(data: pool.data, offset: 72)
  $reserve_quote = parseU64(data: pool.data, offset: 80)

  // Calculate price
  $price = $reserve_quote / $reserve_token

  RETURN {
    pool_address: pool.pubkey,
    price: $price,
    liquidity_token: $reserve_token / LAMPORTS_PER_SOL,
    liquidity_quote: $reserve_quote / LAMPORTS_PER_SOL
  }
})

// Sort by price
$sorted_prices = SORT(collection: $pool_prices, by: "price", order: "asc")

// Find arbitrage opportunities (buy low, sell high)
$opportunities = []
FOR $i IN RANGE(0, COUNT($sorted_prices) - 1) {
  $buy_pool = $sorted_prices[$i]
  FOR $j IN RANGE($i + 1, COUNT($sorted_prices)) {
    $sell_pool = $sorted_prices[$j]

    // Calculate price difference
    $price_diff_pct = (($sell_pool.price - $buy_pool.price) / $buy_pool.price) * 100

    // Account for fees (0.3% per trade = 0.6% round trip)
    $total_fees_pct = 0.6
    $net_profit_pct = $price_diff_pct - $total_fees_pct

    // Only profitable if > 0.5% after fees (account for slippage)
    IF $net_profit_pct > 0.5 {
      APPEND($opportunities, {
        buy_from: $buy_pool.pool_address,
        sell_to: $sell_pool.pool_address,
        buy_price: $buy_pool.price,
        sell_price: $sell_pool.price,
        price_difference_pct: $price_diff_pct,
        estimated_net_profit_pct: $net_profit_pct,
        buy_liquidity: $buy_pool.liquidity_token,
        sell_liquidity: $sell_pool.liquidity_token
      })
    }
  }
}

// Sort opportunities by profitability
$sorted_opportunities = SORT(collection: $opportunities, by: "estimated_net_profit_pct", order: "desc")

// Count total opportunities
$total_opps = COUNT(collection: $sorted_opportunities)

**Decision Point:** Assess arbitrage availability
  BRANCH A ($total_opps > 5):
    $market_status = "inefficient"
    $note = "Multiple arbitrage opportunities - market price discovery in progress"
  BRANCH B ($total_opps >= 2 AND $total_opps <= 5):
    $market_status = "moderately_efficient"
    $note = "Some arbitrage available - moderate profit potential"
  BRANCH C ($total_opps == 1):
    $market_status = "mostly_efficient"
    $note = "Single opportunity - act quickly before arbitraged away"
  BRANCH D ($total_opps == 0):
    $market_status = "efficient"
    $note = "No profitable arbitrage - market is well-balanced"

**Action:**
RETURN {
  token_mint: $token_mint,
  pools_analyzed: COUNT(collection: $dex_pool_addresses),
  total_opportunities: $total_opps,
  opportunities: $sorted_opportunities,
  market_status: $market_status,
  note: $note,
  confidence: 78
}
```

---

## Q18: "Analyze oracle price feeds for token TokenXYZ across providers?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$token_mint = "TokenXYZ"

// Price oracle accounts from different providers
$oracle_accounts = [
  "PythPriceAddr",      // Pyth Network
  "SwitchboardAddr",    // Switchboard
  "ChainlinkAddr"       // Chainlink (if available)
]

// Get all oracle accounts
$oracle_data = getMultipleAccounts(pubkeys: $oracle_accounts)

// Extract prices and metadata from each oracle
$price_feeds = MAP(collection: $oracle_data, fn: (oracle, index) => {
  // Parse oracle-specific data format
  TRY {
    // Pyth format (first oracle)
    IF $index == 0 {
      $price = parseI64(data: oracle.data, offset: 208)
      $conf = parseU64(data: oracle.data, offset: 216)
      $expo = parseI32(data: oracle.data, offset: 224)
      $publish_time = parseI64(data: oracle.data, offset: 232)

      $actual_price = $price * POW(base: 10, exponent: $expo)
      $confidence = $conf * POW(base: 10, exponent: $expo)

      RETURN {
        provider: "Pyth",
        address: oracle.pubkey,
        price_usd: $actual_price,
        confidence_interval: $confidence,
        last_update: $publish_time,
        staleness_seconds: getCurrentTimestamp() - $publish_time
      }
    }
    // Switchboard format (second oracle)
    ELSE IF $index == 1 {
      $value = parseI128(data: oracle.data, offset: 72)
      $std_dev = parseI128(data: oracle.data, offset: 88)
      $last_update = parseI64(data: oracle.data, offset: 104)

      RETURN {
        provider: "Switchboard",
        address: oracle.pubkey,
        price_usd: $value / 1e9,  // Switchboard uses 9 decimals
        confidence_interval: $std_dev / 1e9,
        last_update: $last_update,
        staleness_seconds: getCurrentTimestamp() - $last_update
      }
    }
    // Chainlink format (third oracle)
    ELSE {
      $answer = parseI128(data: oracle.data, offset: 32)
      $updated_at = parseI64(data: oracle.data, offset: 64)

      RETURN {
        provider: "Chainlink",
        address: oracle.pubkey,
        price_usd: $answer / 1e8,  // Chainlink typically uses 8 decimals
        confidence_interval: 0,  // Chainlink doesn't provide confidence
        last_update: $updated_at,
        staleness_seconds: getCurrentTimestamp() - $updated_at
      }
    }
  } CATCH {
    RETURN {
      provider: "Unknown",
      address: oracle.pubkey,
      price_usd: 0,
      error: "Failed to parse oracle data"
    }
  }
})

// Calculate price statistics
$prices = MAP(collection: $price_feeds, fn: feed => feed.price_usd)
$avg_price = SUM(collection: $prices) / COUNT(collection: $prices)
$max_price = MAX(array: $prices)
$min_price = MIN(array: $prices)
$price_spread_pct = (($max_price - $min_price) / $avg_price) * 100

// Check for stale data (> 60 seconds)
$stale_feeds = FILTER(
  collection: $price_feeds,
  fn: feed => feed.staleness_seconds > 60
)

**Decision Point:** Assess price feed health
  BRANCH A ($price_spread_pct > 5):
    $health = "divergent"
    $risk = "High risk - oracles show significantly different prices"
  BRANCH B ($price_spread_pct >= 2 AND $price_spread_pct <= 5):
    $health = "moderate_variance"
    $risk = "Moderate variance - verify with multiple sources"
  BRANCH C ($price_spread_pct < 2 AND COUNT($stale_feeds) == 0):
    $health = "healthy"
    $risk = "Low risk - oracles in agreement and up-to-date"
  BRANCH D ($price_spread_pct < 2 AND COUNT($stale_feeds) > 0):
    $health = "stale_data"
    $risk = "Warning - some oracles have stale data"

**Action:**
RETURN {
  token_mint: $token_mint,
  average_price_usd: $avg_price,
  min_price_usd: $min_price,
  max_price_usd: $max_price,
  price_spread_pct: $price_spread_pct,
  price_feeds: $price_feeds,
  stale_feeds_count: COUNT(collection: $stale_feeds),
  health_status: $health,
  risk_assessment: $risk,
  confidence: 77
}
```

---

## Q19: "Calculate optimal swap route for trading TokenA to TokenB?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

```ovsm
**Main Branch:**
$token_in = "TokenA"
$token_out = "TokenB"
$amount_in = 1000

// Possible routes:
// 1. Direct: TokenA -> TokenB
// 2. Via SOL: TokenA -> SOL -> TokenB
// 3. Via USDC: TokenA -> USDC -> TokenB

// Define pool addresses for each route
$direct_pool = "DirectAtoB"
$route_1_pools = ["AtokenToSol", "SolToB"]
$route_2_pools = ["AtokenToUsdc", "UsdcToB"]

// Get all pool accounts
$all_pools = CONCAT([$direct_pool], $route_1_pools, $route_2_pools)
$pool_accounts = getMultipleAccounts(pubkeys: $all_pools)

// Calculate output for direct route
$direct_pool_data = $pool_accounts[0]
$reserve_in_direct = parseU64(data: $direct_pool_data.data, offset: 72)
$reserve_out_direct = parseU64(data: $direct_pool_data.data, offset: 80)

$amount_in_lamports = $amount_in * LAMPORTS_PER_SOL
$k_direct = $reserve_in_direct * $reserve_out_direct
$amount_with_fee_direct = $amount_in_lamports * 997 / 1000  // 0.3% fee
$new_reserve_in_direct = $reserve_in_direct + $amount_with_fee_direct
$output_direct = $reserve_out_direct - ($k_direct / $new_reserve_in_direct)

// Calculate output for SOL route (two hops)
$pool_a_sol = $pool_accounts[1]
$pool_sol_b = $pool_accounts[2]

// First hop: A -> SOL
$reserve_a = parseU64(data: $pool_a_sol.data, offset: 72)
$reserve_sol_1 = parseU64(data: $pool_a_sol.data, offset: 80)
$k1 = $reserve_a * $reserve_sol_1
$amount_with_fee_1 = $amount_in_lamports * 997 / 1000
$new_reserve_a = $reserve_a + $amount_with_fee_1
$sol_output = $reserve_sol_1 - ($k1 / $new_reserve_a)

// Second hop: SOL -> B
$reserve_sol_2 = parseU64(data: $pool_sol_b.data, offset: 72)
$reserve_b = parseU64(data: $pool_sol_b.data, offset: 80)
$k2 = $reserve_sol_2 * $reserve_b
$amount_with_fee_2 = $sol_output * 997 / 1000
$new_reserve_sol = $reserve_sol_2 + $amount_with_fee_2
$output_via_sol = $reserve_b - ($k2 / $new_reserve_sol)

// Calculate output for USDC route (two hops)
$pool_a_usdc = $pool_accounts[3]
$pool_usdc_b = $pool_accounts[4]

// First hop: A -> USDC
$reserve_a_2 = parseU64(data: $pool_a_usdc.data, offset: 72)
$reserve_usdc_1 = parseU64(data: $pool_a_usdc.data, offset: 80)
$k3 = $reserve_a_2 * $reserve_usdc_1
$amount_with_fee_3 = $amount_in_lamports * 997 / 1000
$new_reserve_a_2 = $reserve_a_2 + $amount_with_fee_3
$usdc_output = $reserve_usdc_1 - ($k3 / $new_reserve_a_2)

// Second hop: USDC -> B
$reserve_usdc_2 = parseU64(data: $pool_usdc_b.data, offset: 72)
$reserve_b_2 = parseU64(data: $pool_usdc_b.data, offset: 80)
$k4 = $reserve_usdc_2 * $reserve_b_2
$amount_with_fee_4 = $usdc_output * 997 / 1000
$new_reserve_usdc = $reserve_usdc_2 + $amount_with_fee_4
$output_via_usdc = $reserve_b_2 - ($k4 / $new_reserve_usdc)

// Compare routes
$routes = [
  {
    name: "Direct A->B",
    output: $output_direct / LAMPORTS_PER_SOL,
    hops: 1,
    total_fees_pct: 0.3
  },
  {
    name: "Via SOL (A->SOL->B)",
    output: $output_via_sol / LAMPORTS_PER_SOL,
    hops: 2,
    total_fees_pct: 0.6
  },
  {
    name: "Via USDC (A->USDC->B)",
    output: $output_via_usdc / LAMPORTS_PER_SOL,
    hops: 2,
    total_fees_pct: 0.6
  }
]

// Find best route
$sorted_routes = SORT(collection: $routes, by: "output", order: "desc")
$best_route = $sorted_routes[0]
$worst_route = $sorted_routes[2]

// Calculate improvement
$improvement_pct = (($best_route.output - $worst_route.output) / $worst_route.output) * 100

**Decision Point:** Assess routing optimization
  BRANCH A ($improvement_pct > 5):
    $recommendation = "Strong recommendation - use {$best_route.name} for {$improvement_pct}% better output"
  BRANCH B ($improvement_pct >= 2 AND $improvement_pct <= 5):
    $recommendation = "Moderate benefit - {$best_route.name} provides {$improvement_pct}% more tokens"
  BRANCH C ($improvement_pct < 2):
    $recommendation = "Minor difference - any route acceptable, prefer fewer hops for gas savings"

**Action:**
RETURN {
  token_in: $token_in,
  token_out: $token_out,
  amount_in: $amount_in,
  all_routes: $sorted_routes,
  best_route: $best_route,
  improvement_vs_worst_pct: $improvement_pct,
  recommendation: $recommendation,
  confidence: 76
}
```

---

## Q20: "Detect flash loan attacks in recent transactions?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction, getBlock
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$lending_protocol = "SolendMainPool"
$time_window_seconds = 3600  // Last hour

$current_time = getCurrentTimestamp()
$start_time = $current_time - $time_window_seconds

// Get recent transactions
$recent_signatures = getSignaturesForAddress(
  address: $lending_protocol,
  options: {
    limit: 1000
  }
)

// Filter to time window
$windowed_sigs = FILTER(
  collection: $recent_signatures,
  fn: sig => sig.blockTime >= $start_time
)

// Fetch full transaction details
$transactions = MAP(
  collection: $windowed_sigs,
  fn: sig => getTransaction(signature: sig.signature)
)

// Detect flash loan patterns:
// 1. Large borrow and repay in same transaction
// 2. Multiple protocol interactions (DEX swaps, lending)
// 3. Unusual profit extraction

$suspicious_txs = FILTER(collection: $transactions, fn: tx => {
  // Check if transaction has inner instructions (CPIs)
  $inner_instructions = tx.meta.innerInstructions
  $has_cpis = COUNT(collection: $inner_instructions) > 5

  // Check for large balance changes
  $pre_balances = tx.meta.preBalances
  $post_balances = tx.meta.postBalances
  $max_balance_change = 0

  FOR $i IN RANGE(0, COUNT($pre_balances)) {
    $change = ABS($post_balances[$i] - $pre_balances[$i])
    IF $change > $max_balance_change {
      $max_balance_change = $change
    }
  }

  // Flash loan indicators:
  // - Large balance change (> 100 SOL equivalent)
  // - Many CPIs (complex transaction)
  // - Net profit extracted
  $is_large = $max_balance_change > (100 * LAMPORTS_PER_SOL)
  $is_complex = $has_cpis
  $is_profitable = $post_balances[0] > $pre_balances[0]  // Fee payer gained funds

  RETURN ($is_large AND $is_complex AND $is_profitable)
})

// Analyze suspicious transactions
$flash_loan_analysis = MAP(collection: $suspicious_txs, fn: tx => {
  // Extract key metrics
  $inner_count = COUNT(collection: FLATTEN(collection: MAP(tx.meta.innerInstructions, ii => ii.instructions)))

  // Calculate profit
  $profit_lamports = tx.meta.postBalances[0] - tx.meta.preBalances[0]
  $profit_sol = $profit_lamports / LAMPORTS_PER_SOL

  // Identify protocols involved
  $program_ids = UNIQUE(array: MAP(tx.transaction.message.instructions, ix => ix.programId))

  RETURN {
    signature: tx.transaction.signatures[0],
    slot: tx.slot,
    timestamp: tx.blockTime,
    total_cpis: $inner_count,
    profit_sol: $profit_sol,
    programs_involved: COUNT(collection: $program_ids),
    program_ids: $program_ids
  }
})

// Sort by profit
$sorted_attacks = SORT(collection: $flash_loan_analysis, by: "profit_sol", order: "desc")

// Calculate total extracted value
$total_profit = SUM(collection: MAP($sorted_attacks, fn: attack => attack.profit_sol))

**Decision Point:** Assess threat level
  BRANCH A (COUNT($sorted_attacks) > 5):
    $threat_level = "critical"
    $alert = "ALERT: {COUNT($sorted_attacks)} potential flash loan attacks detected - total profit ${total_profit} SOL"
  BRANCH B (COUNT($sorted_attacks) >= 2 AND COUNT($sorted_attacks) <= 5):
    $threat_level = "elevated"
    $alert = "Warning: Multiple suspicious transactions detected"
  BRANCH C (COUNT($sorted_attacks) == 1):
    $threat_level = "moderate"
    $alert = "Single flash loan pattern detected - monitor closely"
  BRANCH D (COUNT($sorted_attacks) == 0):
    $threat_level = "normal"
    $alert = "No flash loan attacks detected in the last hour"

**Action:**
RETURN {
  lending_protocol: $lending_protocol,
  time_window_hours: $time_window_seconds / 3600,
  total_transactions_analyzed: COUNT(collection: $transactions),
  suspicious_transactions: COUNT(collection: $sorted_attacks),
  total_extracted_value_sol: $total_profit,
  attacks: $sorted_attacks,
  threat_level: $threat_level,
  alert: $alert,
  confidence: 95
}
```

---

## Q21: "Analyze concentrated liquidity position health in Orca Whirlpool?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$position_address = "WhirlpoolPosXYZ"

// Get position account (Orca Whirlpool concentrated liquidity)
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position not found")

// Parse concentrated liquidity position data
// Offset 8: whirlpool (pubkey)
// Offset 40: position_mint (pubkey - NFT representing position)
// Offset 72: liquidity (u128)
// Offset 88: tick_lower_index (i32)
// Offset 92: tick_upper_index (i32)
// Offset 96: fee_growth_checkpoint_a (u128)
// Offset 112: fee_growth_checkpoint_b (u128)
// Offset 128: fee_owed_a (u64)
// Offset 136: fee_owed_b (u64)

$whirlpool_address = parsePublicKey(data: $position_account.data, offset: 8)
$liquidity = parseU128(data: $position_account.data, offset: 72)
$tick_lower = parseI32(data: $position_account.data, offset: 88)
$tick_upper = parseI32(data: $position_account.data, offset: 92)
$fee_owed_a = parseU64(data: $position_account.data, offset: 128)
$fee_owed_b = parseU64(data: $position_account.data, offset: 136)

// Get whirlpool (pool) account to check current tick
$whirlpool_account = getAccountInfo(pubkey: $whirlpool_address)
$current_tick = parseI32(data: $whirlpool_account.data, offset: 88)

// Check if position is in range
$is_in_range = ($current_tick >= $tick_lower AND $current_tick <= $tick_upper)

// Calculate tick range width
$tick_range_width = $tick_upper - $tick_lower

// Calculate distance to range boundaries
$distance_to_lower = $current_tick - $tick_lower
$distance_to_upper = $tick_upper - $current_tick

// Calculate position within range (0-100%)
$position_in_range_pct = ($distance_to_lower / $tick_range_width) * 100

// Convert fees to UI amounts
$fee_a_ui = $fee_owed_a / LAMPORTS_PER_SOL
$fee_b_ui = $fee_owed_b / LAMPORTS_PER_SOL

**Decision Point:** Assess position health and recommendations
  BRANCH A (NOT $is_in_range):
    $health = "out_of_range"
    $recommendation = "Position is OUT OF RANGE - not earning fees. Rebalance position urgently"
    $earning_status = "Not earning fees"
  BRANCH B ($is_in_range AND $distance_to_lower < ($tick_range_width * 0.1)):
    $health = "near_lower_bound"
    $recommendation = "Position near lower bound - consider rebalancing soon"
    $earning_status = "Earning fees (risk: approaching lower bound)"
  BRANCH C ($is_in_range AND $distance_to_upper < ($tick_range_width * 0.1)):
    $health = "near_upper_bound"
    $recommendation = "Position near upper bound - consider rebalancing soon"
    $earning_status = "Earning fees (risk: approaching upper bound)"
  BRANCH D ($is_in_range):
    $health = "healthy"
    $recommendation = "Position is actively earning fees in range"
    $earning_status = "Earning fees"

**Action:**
RETURN {
  position_address: $position_address,
  whirlpool_address: $whirlpool_address,
  liquidity: $liquidity,
  tick_range: {
    lower: $tick_lower,
    upper: $tick_upper,
    current: $current_tick,
    width: $tick_range_width
  },
  is_in_range: $is_in_range,
  position_in_range_pct: $position_in_range_pct,
  fees_owed: {
    token_a: $fee_a_ui,
    token_b: $fee_b_ui
  },
  health: $health,
  earning_status: $earning_status,
  recommendation: $recommendation,
  confidence: 94
}
```

---

## Q22: "Calculate real-time funding rate for perpetual futures market?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$perp_market = "BTC-PERP-Market"

// Get perpetual market account (e.g., Drift Protocol, Mango Markets)
$market_account = getAccountInfo(pubkey: $perp_market)

GUARD $market_account != null ELSE
  RETURN ERROR(message: "Perpetual market not found")

// Parse market data
// Offset 72: oracle_price (i128)
// Offset 88: mark_price (i128)  // Market's internal price
// Offset 104: funding_rate_long (i64)  // Per hour rate in basis points
// Offset 112: funding_rate_short (i64)
// Offset 120: last_funding_time (i64)
// Offset 128: open_interest (u128)
// Offset 144: total_longs (u64)
// Offset 152: total_shorts (u64)

$oracle_price = parseI128(data: $market_account.data, offset: 72) / 1e6
$mark_price = parseI128(data: $market_account.data, offset: 88) / 1e6
$funding_rate_long = parseI64(data: $market_account.data, offset: 104)
$funding_rate_short = parseI64(data: $market_account.data, offset: 112)
$last_funding_time = parseI64(data: $market_account.data, offset: 120)
$open_interest = parseU128(data: $market_account.data, offset: 128)
$total_longs = parseU64(data: $market_account.data, offset: 144)
$total_shorts = parseU64(data: $market_account.data, offset: 152)

// Convert funding rates from basis points to percentage
$funding_rate_long_pct = $funding_rate_long / 10000  // 100 bp = 1%
$funding_rate_short_pct = $funding_rate_short / 10000

// Calculate premium/discount
$price_premium = $mark_price - $oracle_price
$premium_pct = ($price_premium / $oracle_price) * 100

// Calculate long/short imbalance
$long_pct = ($total_longs / ($total_longs + $total_shorts)) * 100
$short_pct = 100 - $long_pct
$imbalance = ABS($long_pct - 50)

// Estimate annualized funding rate (funding every 8 hours)
$annualized_funding_pct = $funding_rate_long_pct * 3 * 365

// Calculate time until next funding
$current_time = getCurrentTimestamp()
$time_since_last_funding = $current_time - $last_funding_time
$funding_interval = 28800  // 8 hours
$time_to_next_funding = $funding_interval - $time_since_last_funding

**Decision Point:** Assess funding rate environment
  BRANCH A ($funding_rate_long_pct > 0.1):
    $market_sentiment = "extremely_bullish"
    $note = "Very high funding rate - longs paying {$funding_rate_long_pct}% per 8h. Consider shorting"
  BRANCH B ($funding_rate_long_pct > 0.05 AND $funding_rate_long_pct <= 0.1):
    $market_sentiment = "bullish"
    $note = "Elevated funding - longs paying {$funding_rate_long_pct}% per 8h"
  BRANCH C ($funding_rate_long_pct < -0.05):
    $market_sentiment = "bearish"
    $note = "Negative funding - shorts paying {ABS($funding_rate_long_pct)}% per 8h. Consider longing"
  BRANCH D (ABS($funding_rate_long_pct) <= 0.05):
    $market_sentiment = "neutral"
    $note = "Balanced funding rate - market is fairly priced"

**Action:**
RETURN {
  perp_market: $perp_market,
  oracle_price_usd: $oracle_price,
  mark_price_usd: $mark_price,
  price_premium_pct: $premium_pct,
  funding_rate_long_pct_per_8h: $funding_rate_long_pct,
  funding_rate_short_pct_per_8h: $funding_rate_short_pct,
  annualized_funding_pct: $annualized_funding_pct,
  open_interest_contracts: $open_interest,
  long_percentage: $long_pct,
  short_percentage: $short_pct,
  position_imbalance: $imbalance,
  seconds_to_next_funding: $time_to_next_funding,
  market_sentiment: $market_sentiment,
  note: $note,
  confidence: 93
}
```

---

## Q23: "Analyze yield farming strategy profitability with compounding?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM, POW (Data Processing)

```ovsm
**Main Branch:**
$farm_address = "YieldFarmXYZ"
$user_stake_address = "UserStakeAcc"
$initial_stake = 10000  // Initial stake amount
$days_elapsed = 30

// Get farm APY data
$farm_account = getAccountInfo(pubkey: $farm_address)
$reward_per_second = parseU64(data: $farm_account.data, offset: 8)
$total_staked = parseU64(data: $farm_account.data, offset: 16)

// Get user's current stake
$stake_account = getAccountInfo(pubkey: $user_stake_address)
$current_stake = parseU64(data: $stake_account.data, offset: 8)
$pending_rewards = parseU64(data: $stake_account.data, offset: 32)

// Calculate base APY
$annual_rewards = $reward_per_second * 31536000
$base_apy_pct = ($annual_rewards / $total_staked) * 100

// Simulate three compounding strategies
// Strategy 1: No compounding (claim and hold)
$rewards_no_compound = ($initial_stake * LAMPORTS_PER_SOL) * ($base_apy_pct / 100) * ($days_elapsed / 365)
$final_value_no_compound = ($initial_stake * LAMPORTS_PER_SOL) + $rewards_no_compound

// Strategy 2: Daily compounding
$daily_rate = $base_apy_pct / 365 / 100
$compound_periods_daily = $days_elapsed
$final_value_daily_compound = ($initial_stake * LAMPORTS_PER_SOL) * POW(base: (1 + $daily_rate), exponent: $compound_periods_daily)

// Strategy 3: Weekly compounding
$weekly_rate = ($base_apy_pct / 52) / 100
$compound_periods_weekly = $days_elapsed / 7
$final_value_weekly_compound = ($initial_stake * LAMPORTS_PER_SOL) * POW(base: (1 + $weekly_rate), exponent: $compound_periods_weekly)

// Calculate gains for each strategy
$gain_no_compound = $final_value_no_compound - ($initial_stake * LAMPORTS_PER_SOL)
$gain_daily = $final_value_daily_compound - ($initial_stake * LAMPORTS_PER_SOL)
$gain_weekly = $final_value_weekly_compound - ($initial_stake * LAMPORTS_PER_SOL)

// Calculate compound benefit
$compound_benefit_daily = $gain_daily - $gain_no_compound
$compound_benefit_pct_daily = ($compound_benefit_daily / $gain_no_compound) * 100

// Convert to UI amounts
$gain_no_compound_ui = $gain_no_compound / LAMPORTS_PER_SOL
$gain_daily_ui = $gain_daily / LAMPORTS_PER_SOL
$gain_weekly_ui = $gain_weekly / LAMPORTS_PER_SOL

// Estimate gas costs for compounding
$tx_cost_sol = 0.000005  // 5000 lamports per transaction
$daily_compound_gas_cost = $tx_cost_sol * $days_elapsed
$weekly_compound_gas_cost = $tx_cost_sol * ($days_elapsed / 7)

// Net profit after gas
$net_profit_daily = $gain_daily_ui - $daily_compound_gas_cost
$net_profit_weekly = $gain_weekly_ui - $weekly_compound_gas_cost

**Decision Point:** Determine optimal strategy
  BRANCH A ($compound_benefit_pct_daily > 5):
    $recommendation = "Daily compounding highly recommended - {$compound_benefit_pct_daily}% extra gains"
    $optimal_strategy = "daily_compound"
  BRANCH B ($compound_benefit_pct_daily >= 2 AND $compound_benefit_pct_daily <= 5):
    $recommendation = "Weekly compounding optimal - balances gains with gas costs"
    $optimal_strategy = "weekly_compound"
  BRANCH C ($compound_benefit_pct_daily < 2):
    $recommendation = "Manual compounding not worth gas costs - claim monthly"
    $optimal_strategy = "monthly_or_no_compound"

**Action:**
RETURN {
  farm_address: $farm_address,
  base_apy_pct: $base_apy_pct,
  days_simulated: $days_elapsed,
  initial_stake: $initial_stake,
  strategies: {
    no_compound: {
      final_value: $final_value_no_compound / LAMPORTS_PER_SOL,
      gains: $gain_no_compound_ui,
      gas_cost: 0
    },
    daily_compound: {
      final_value: $final_value_daily_compound / LAMPORTS_PER_SOL,
      gains: $gain_daily_ui,
      gas_cost: $daily_compound_gas_cost,
      net_profit: $net_profit_daily
    },
    weekly_compound: {
      final_value: $final_value_weekly_compound / LAMPORTS_PER_SOL,
      gains: $gain_weekly_ui,
      gas_cost: $weekly_compound_gas_cost,
      net_profit: $net_profit_weekly
    }
  },
  compound_benefit_pct: $compound_benefit_pct_daily,
  optimal_strategy: $optimal_strategy,
  recommendation: $recommendation,
  confidence: 92
}
```

---

## Q24: "Detect sandwich attack opportunities in mempool?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getRecentPrioritizationFees, getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

```ovsm
**Main Branch:**
$pool_address = "TargetPoolXYZ"
$monitoring_window = 10  // seconds

// Get recent priority fees to understand competition
$recent_fees = getRecentPrioritizationFees()

// Get recent pool transactions
$recent_sigs = getSignaturesForAddress(
  address: $pool_address,
  options: {limit: 100}
)

// Analyze transactions for large swaps (sandwich targets)
$current_time = getCurrentTimestamp()
$recent_window_sigs = FILTER(
  collection: $recent_sigs,
  fn: sig => sig.blockTime >= ($current_time - $monitoring_window)
)

// Fetch transaction details
$transactions = MAP(
  collection: $recent_window_sigs,
  fn: sig => getTransaction(signature: sig.signature)
)

// Identify large swaps (potential sandwich targets)
$large_swaps = FILTER(collection: $transactions, fn: tx => {
  // Check token balance changes
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  IF COUNT($pre_balances) == 0 {
    RETURN false
  }

  // Calculate swap size
  $max_change = 0
  FOR $i IN RANGE(0, COUNT($pre_balances)) {
    $change = ABS($post_balances[$i].uiTokenAmount.uiAmount - $pre_balances[$i].uiTokenAmount.uiAmount)
    IF $change > $max_change {
      $max_change = $change
    }
  }

  // Large swap threshold: > 10000 tokens
  RETURN ($max_change > 10000)
})

// Calculate potential sandwich profits
$sandwich_opportunities = MAP(collection: $large_swaps, fn: tx => {
  // Extract swap amount
  $swap_amount = 10000  // Simplified

  // Estimate price impact
  $price_impact_pct = 2.5  // Simplified calculation

  // Sandwich profit = front-run at lower price, victim trades at higher price, back-run at profit
  $frontrun_cost = $swap_amount * 100  // Buy before victim
  $expected_price_increase = $price_impact_pct
  $backrun_profit = $frontrun_cost * ($expected_price_increase / 100)

  // Subtract gas costs (priority fees)
  $avg_priority_fee = SUM(collection: MAP($recent_fees, fn: fee => fee.prioritizationFee)) / COUNT($recent_fees)
  $gas_cost_sol = ($avg_priority_fee * 2) / LAMPORTS_PER_SOL  // 2 txs (front + back)

  $net_profit = $backrun_profit - $gas_cost_sol

  RETURN {
    target_tx: tx.transaction.signatures[0],
    swap_amount: $swap_amount,
    estimated_price_impact_pct: $price_impact_pct,
    frontrun_cost: $frontrun_cost,
    expected_profit: $backrun_profit,
    gas_cost: $gas_cost_sol,
    net_profit: $net_profit,
    priority_fee_required: $avg_priority_fee * 1.5  // Need 50% higher to win
  }
})

// Sort by profitability
$sorted_opportunities = SORT(collection: $sandwich_opportunities, by: "net_profit", order: "desc")

**Decision Point:** Assess MEV opportunity
  BRANCH A (COUNT($sorted_opportunities) > 5):
    $mev_environment = "highly_active"
    $note = "Multiple sandwich opportunities - high competition expected"
  BRANCH B (COUNT($sorted_opportunities) >= 1 AND COUNT($sorted_opportunities) <= 5):
    $mev_environment = "moderate"
    $note = "Some opportunities available"
  BRANCH C (COUNT($sorted_opportunities) == 0):
    $mev_environment = "quiet"
    $note = "No profitable sandwich opportunities in current window"

**Action:**
RETURN {
  pool_address: $pool_address,
  monitoring_window_seconds: $monitoring_window,
  total_transactions_analyzed: COUNT(collection: $transactions),
  large_swaps_detected: COUNT(collection: $large_swaps),
  profitable_opportunities: COUNT(collection: $sorted_opportunities),
  opportunities: $sorted_opportunities,
  avg_priority_fee: SUM(collection: MAP($recent_fees, fn: fee => fee.prioritizationFee)) / COUNT($recent_fees),
  mev_environment: $mev_environment,
  note: $note,
  confidence: 91
}
```

---

## Q25: "Calculate optimal leverage for perpetual position based on risk tolerance?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM, POW (Data Processing)

```ovsm
**Main Branch:**
$perp_market = "SOL-PERP"
$collateral_usd = 10000
$risk_tolerance = "moderate"  // conservative, moderate, aggressive

// Get market data
$market_account = getAccountInfo(pubkey: $perp_market)
$oracle_price = parseI128(data: $market_account.data, offset: 72) / 1e6
$funding_rate = parseI64(data: $market_account.data, offset: 104) / 10000
$base_fee_pct = 0.05  // 5 basis points

// Define risk profiles
$risk_profiles = {
  conservative: {
    max_leverage: 3,
    stop_loss_pct: 5,
    target_profit_pct: 15,
    liquidation_buffer_pct: 50
  },
  moderate: {
    max_leverage: 5,
    stop_loss_pct: 10,
    target_profit_pct: 30,
    liquidation_buffer_pct: 30
  },
  aggressive: {
    max_leverage: 10,
    stop_loss_pct: 20,
    target_profit_pct: 50,
    liquidation_buffer_pct: 15
  }
}

// Select profile based on risk tolerance
$profile = $risk_profiles[$risk_tolerance]

// Calculate position sizes for different leverage levels
$leverage_scenarios = []
FOR $leverage IN [1, 2, 3, 5, 7, 10] {
  IF $leverage <= $profile.max_leverage {
    // Calculate position size
    $position_size_usd = $collateral_usd * $leverage

    // Calculate liquidation price (simplified)
    // Liquidation occurs when position value drops below maintenance margin
    $maintenance_margin_pct = 5  // 5%
    $price_drop_to_liquidation_pct = (100 - $maintenance_margin_pct) / $leverage

    $liquidation_price = $oracle_price * (1 - ($price_drop_to_liquidation_pct / 100))

    // Calculate profit/loss scenarios
    $profit_at_target = $position_size_usd * ($profile.target_profit_pct / 100)
    $loss_at_stop = $position_size_usd * ($profile.stop_loss_pct / 100)

    // Account for funding costs (assume 7 day hold)
    $funding_cost = $position_size_usd * ($funding_rate * 7 * 3) / 100  // 3 funding per day

    // Calculate risk/reward ratio
    $risk_reward_ratio = $profit_at_target / $loss_at_stop

    // Check if liquidation buffer is adequate
    $buffer_ok = $price_drop_to_liquidation_pct > $profile.liquidation_buffer_pct

    APPEND($leverage_scenarios, {
      leverage: $leverage,
      position_size_usd: $position_size_usd,
      liquidation_price: $liquidation_price,
      price_drop_to_liquidation_pct: $price_drop_to_liquidation_pct,
      profit_at_target: $profit_at_target,
      loss_at_stop: $loss_at_stop,
      funding_cost_7d: $funding_cost,
      risk_reward_ratio: $risk_reward_ratio,
      adequate_buffer: $buffer_ok
    })
  }
}

// Find optimal leverage (highest that meets buffer requirement)
$safe_scenarios = FILTER(collection: $leverage_scenarios, fn: s => s.adequate_buffer)
$optimal = $safe_scenarios[COUNT($safe_scenarios) - 1]  // Highest safe leverage

**Decision Point:** Provide leverage recommendation
  BRANCH A ($optimal.leverage >= 7):
    $recommendation = "High leverage acceptable for {$risk_tolerance} profile"
    $warning = "Monitor position closely - small moves create large P&L swings"
  BRANCH B ($optimal.leverage >= 3 AND $optimal.leverage < 7):
    $recommendation = "Moderate leverage recommended"
    $warning = "Balanced risk/reward for {$risk_tolerance} tolerance"
  BRANCH C ($optimal.leverage < 3):
    $recommendation = "Conservative leverage advised"
    $warning = "Current market conditions suggest lower leverage"

**Action:**
RETURN {
  perp_market: $perp_market,
  collateral_usd: $collateral_usd,
  risk_tolerance: $risk_tolerance,
  current_price: $oracle_price,
  funding_rate_per_8h: $funding_rate,
  optimal_leverage: $optimal.leverage,
  optimal_scenario: $optimal,
  all_scenarios: $leverage_scenarios,
  recommendation: $recommendation,
  warning: $warning,
  confidence: 90
}
```

---

## Q26: "Analyze token vesting schedule and unlock events?"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

```ovsm
**Main Branch:**
$vesting_account = "VestingAccXYZ"
$token_mint = "ProjectToken"

// Get vesting account
$vesting = getAccountInfo(pubkey: $vesting_account)

GUARD $vesting != null ELSE
  RETURN ERROR(message: "Vesting account not found")

// Parse vesting schedule
// Offset 8: beneficiary (pubkey)
// Offset 40: total_amount (u64)
// Offset 48: released_amount (u64)
// Offset 56: start_time (i64)
// Offset 64: cliff_time (i64)
// Offset 72: end_time (i64)
// Offset 80: schedule_type (u8) // 0=linear, 1=monthly, 2=custom

$total_amount = parseU64(data: $vesting.data, offset: 40)
$released_amount = parseU64(data: $vesting.data, offset: 48)
$start_time = parseI64(data: $vesting.data, offset: 56)
$cliff_time = parseI64(data: $vesting.data, offset: 64)
$end_time = parseI64(data: $vesting.data, offset: 72)

$current_time = getCurrentTimestamp()

// Calculate vesting progress
$total_duration = $end_time - $start_time
$time_elapsed = $current_time - $start_time
$vesting_progress_pct = ($time_elapsed / $total_duration) * 100

// Calculate vested amount based on linear schedule
$vested_amount = 0
IF $current_time < $cliff_time {
  $vested_amount = 0  // Before cliff
} ELSE IF $current_time >= $end_time {
  $vested_amount = $total_amount  // Fully vested
} ELSE {
  // Linear vesting after cliff
  $vested_amount = ($total_amount * $time_elapsed) / $total_duration
}

// Calculate claimable and locked amounts
$claimable_amount = $vested_amount - $released_amount
$locked_amount = $total_amount - $vested_amount

// Calculate next unlock events (monthly schedule example)
$seconds_per_month = 2592000  // 30 days
$next_unlock_time = $current_time + $seconds_per_month
$monthly_unlock_amount = $total_amount / (($end_time - $start_time) / $seconds_per_month)

// Convert to UI amounts
$total_ui = $total_amount / LAMPORTS_PER_SOL
$vested_ui = $vested_amount / LAMPORTS_PER_SOL
$released_ui = $released_amount / LAMPORTS_PER_SOL
$claimable_ui = $claimable_amount / LAMPORTS_PER_SOL
$locked_ui = $locked_amount / LAMPORTS_PER_SOL

// Calculate sell pressure risk from upcoming unlocks
$days_to_full_unlock = ($end_time - $current_time) / 86400

**Decision Point:** Assess unlock schedule impact
  BRANCH A ($claimable_ui > 1000000 AND $days_to_full_unlock < 30):
    $sell_pressure_risk = "critical"
    $note = "Large unlock imminent - high sell pressure expected"
  BRANCH B ($claimable_ui >= 100000 AND $days_to_full_unlock < 90):
    $sell_pressure_risk = "elevated"
    $note = "Significant tokens unlocking soon"
  BRANCH C ($vesting_progress_pct > 80):
    $sell_pressure_risk = "moderate"
    $note = "Vesting mostly complete - limited future unlocks"
  BRANCH D ($vesting_progress_pct <= 80):
    $sell_pressure_risk = "low"
    $note = "Early in vesting schedule"

**Action:**
RETURN {
  vesting_account: $vesting_account,
  token_mint: $token_mint,
  total_tokens: $total_ui,
  vested_tokens: $vested_ui,
  released_tokens: $released_ui,
  claimable_tokens: $claimable_ui,
  locked_tokens: $locked_ui,
  vesting_progress_pct: $vesting_progress_pct,
  days_to_full_unlock: $days_to_full_unlock,
  estimated_monthly_unlock: $monthly_unlock_amount / LAMPORTS_PER_SOL,
  sell_pressure_risk: $sell_pressure_risk,
  note: $note,
  confidence: 89
}
```

---

## Q27: "Monitor governance proposal voting power distribution?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getProgramAccounts
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

```ovsm
**Main Branch:**
$governance_program = "GovernanceProgram"
$proposal_address = "ProposalXYZ"

// Get proposal account
$proposal = getAccountInfo(pubkey: $proposal_address)

GUARD $proposal != null ELSE
  RETURN ERROR(message: "Proposal not found")

// Parse proposal data
// Offset 8: governance (pubkey)
// Offset 40: proposal_state (u8) // 0=draft, 1=voting, 2=succeeded, 3=defeated
// Offset 41: vote_threshold_pct (u8)
// Offset 42: yes_votes (u64)
// Offset 50: no_votes (u64)
// Offset 58: voting_end_time (i64)
// Offset 66: total_eligible_votes (u64)

$yes_votes = parseU64(data: $proposal.data, offset: 42)
$no_votes = parseU64(data: $proposal.data, offset: 50)
$voting_end_time = parseI64(data: $proposal.data, offset: 58)
$total_eligible = parseU64(data: $proposal.data, offset: 66)
$threshold_pct = parseU8(data: $proposal.data, offset: 41)

// Calculate voting statistics
$total_votes_cast = $yes_votes + $no_votes
$participation_rate = ($total_votes_cast / $total_eligible) * 100
$yes_percentage = ($yes_votes / $total_votes_cast) * 100
$no_percentage = 100 - $yes_percentage

// Get all vote record accounts to analyze voting power distribution
$vote_records = getProgramAccounts(
  programId: $governance_program,
  filters: [
    {
      memcmp: {
        offset: 40,  // Proposal address field
        bytes: $proposal_address
      }
    }
  ]
)

// Analyze vote distribution
$vote_powers = MAP(collection: $vote_records, fn: record => {
  $voter = parsePublicKey(data: record.data, offset: 8)
  $vote_weight = parseU64(data: record.data, offset: 72)
  $vote_choice = parseU8(data: record.data, offset: 80)  // 0=yes, 1=no

  RETURN {
    voter: $voter,
    vote_weight: $vote_weight / LAMPORTS_PER_SOL,
    vote_choice: IF $vote_choice == 0 THEN "yes" ELSE "no"
  }
})

// Sort by vote weight
$sorted_voters = SORT(collection: $vote_powers, by: "vote_weight", order: "desc")

// Calculate concentration (top 10 holders)
$top_10_voters = SLICE(array: $sorted_voters, start: 0, end: 10)
$top_10_power = SUM(collection: MAP($top_10_voters, fn: v => v.vote_weight))
$top_10_concentration_pct = ($top_10_power / ($total_votes_cast / LAMPORTS_PER_SOL)) * 100

// Time remaining
$current_time = getCurrentTimestamp()
$time_remaining_hours = ($voting_end_time - $current_time) / 3600

// Determine if proposal will pass
$votes_needed_to_pass = ($total_eligible * $threshold_pct) / 100
$votes_short = $votes_needed_to_pass - $yes_votes

**Decision Point:** Assess proposal outcome
  BRANCH A ($yes_percentage >= $threshold_pct AND $time_remaining_hours <= 0):
    $outcome = "passed"
    $status = "Proposal has passed governance vote"
  BRANCH B ($yes_percentage >= $threshold_pct AND $time_remaining_hours > 0):
    $outcome = "likely_to_pass"
    $status = "Proposal currently passing - {$time_remaining_hours}h remaining"
  BRANCH C ($yes_percentage < $threshold_pct AND $time_remaining_hours <= 0):
    $outcome = "defeated"
    $status = "Proposal was defeated"
  BRANCH D ($yes_percentage < $threshold_pct AND $time_remaining_hours > 0):
    $outcome = "unlikely_to_pass"
    $status = "Proposal needs {$votes_short} more yes votes - {$time_remaining_hours}h remaining"

**Action:**
RETURN {
  proposal_address: $proposal_address,
  yes_votes: $yes_votes / LAMPORTS_PER_SOL,
  no_votes: $no_votes / LAMPORTS_PER_SOL,
  yes_percentage: $yes_percentage,
  no_percentage: $no_percentage,
  participation_rate_pct: $participation_rate,
  threshold_required_pct: $threshold_pct,
  total_voters: COUNT(collection: $vote_records),
  top_10_concentration_pct: $top_10_concentration_pct,
  top_10_voters: $top_10_voters,
  hours_remaining: $time_remaining_hours,
  outcome_prediction: $outcome,
  status: $status,
  confidence: 88
}
```

---

## Q28: "Detect whale movements affecting DeFi protocol?"
**Plan:** [TIME: ~10s] [COST: free] [CONFIDENCE: 87%] | **Tools:** getSignaturesForAddress, getTransaction, MAP, FILTER, SUM

```ovsm
**Main Branch:** $whale = "WhaleXYZ" | $protocol = "SolendProgram" | $window = 86400 | $current = getCurrentTimestamp() | $sigs = getSignaturesForAddress(address: $whale, options: {limit: 100}) | $recent = FILTER(collection: $sigs, fn: s => s.blockTime >= ($current - $window)) | $txs = MAP(collection: $recent, fn: s => getTransaction(signature: s.signature)) | $protocol_txs = FILTER(collection: $txs, fn: tx => CONTAINS(array: MAP(tx.transaction.message.instructions, fn: ix => ix.programId), value: $protocol)) | $actions = MAP(collection: $protocol_txs, fn: tx => {$amount = ABS(tx.meta.postBalances[0] - tx.meta.preBalances[0]) / LAMPORTS_PER_SOL | $type = IF tx.meta.postBalances[0] > tx.meta.preBalances[0] THEN "withdraw" ELSE "deposit" | RETURN {signature: tx.transaction.signatures[0], action: $type, amount_sol: $amount}}) | $deposited = SUM(collection: MAP(FILTER($actions, fn: a => a.action == "deposit"), fn: a => a.amount_sol)) | $withdrawn = SUM(collection: MAP(FILTER($actions, fn: a => a.action == "withdraw"), fn: a => a.amount_sol)) | $net = $deposited - $withdrawn

**Decision Point:** Assess impact | BRANCH A ($net < -100000): $impact = "large_exit" | $alert = "Major whale exit" | BRANCH B ($net > 100000): $impact = "large_entry" | $alert = "Whale accumulation" | BRANCH C: $impact = "normal" | $alert = "Standard activity"

**Action:** RETURN {whale: $whale, protocol: $protocol, total_txs: COUNT($protocol_txs), deposited: $deposited, withdrawn: $withdrawn, net_flow: $net, impact: $impact, alert: $alert, confidence: 87}
```

---

## Q29: "Calculate protocol revenue and fee distribution?"
**Plan:** [TIME: ~11s] [COST: free] [CONFIDENCE: 86%] | **Tools:** getAccountInfo, getSignaturesForAddress, MAP, SUM

```ovsm
**Main Branch:** $protocol_treasury = "TreasuryAddr" | $fee_collector = "FeeCollector" | $period_days = 7 | $treasury = getAccountInfo(pubkey: $protocol_treasury) | $current_balance = parseU64(data: $treasury.data, offset: 8) / LAMPORTS_PER_SOL | $sigs = getSignaturesForAddress(address: $fee_collector, options: {limit: 1000}) | $recent = FILTER(collection: $sigs, fn: s => s.blockTime >= (getCurrentTimestamp() - ($period_days * 86400))) | $txs = MAP(collection: $recent, fn: s => getTransaction(signature: s.signature)) | $fees_collected = MAP(collection: $txs, fn: tx => {$fee_amount = 0 | FOR $i IN RANGE(0, COUNT(tx.meta.postTokenBalances)) {$pre = tx.meta.preTokenBalances[$i].uiTokenAmount.uiAmount | $post = tx.meta.postTokenBalances[$i].uiTokenAmount.uiAmount | IF $post > $pre {$fee_amount = $post - $pre}} | RETURN $fee_amount}) | $total_fees = SUM(collection: $fees_collected) | $daily_avg = $total_fees / $period_days | $annual_projection = $daily_avg * 365 | $fee_rate_pct = 0.3

**Decision Point:** Revenue health | BRANCH A ($daily_avg > 10000): $health = "strong" | $note = "Healthy protocol revenue" | BRANCH B ($daily_avg >= 1000): $health = "moderate" | $note = "Stable fee generation" | BRANCH C: $health = "low" | $note = "Limited revenue"

**Action:** RETURN {treasury_balance: $current_balance, period_days: $period_days, total_fees_collected: $total_fees, daily_average: $daily_avg, annual_projection: $annual_projection, fee_rate_pct: $fee_rate_pct, health: $health, note: $note, confidence: 86}
```

---

## Q30: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr030xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 85
}

---

## Q31: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr031xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate collateral ratio
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 84
}

---

## Q32: "Calculate slippage tolerance for large swap?"
**Plan:** [TIME: ~4s] [COST: free] [CONFIDENCE: 83%] | **Tools:** getAccountInfo, MAP

```ovsm
**Main Branch:** $pool = "PoolXYZ" | $swap_amount = 50000 | $pool_acc = getAccountInfo(pubkey: $pool) | $reserve_in = parseU64(data: $pool_acc.data, offset: 72) / LAMPORTS_PER_SOL | $reserve_out = parseU64(data: $pool_acc.data, offset: 80) / LAMPORTS_PER_SOL | $k = $reserve_in * $reserve_out | $amount_in_with_fee = $swap_amount * 0.997 | $new_reserve_in = $reserve_in + $amount_in_with_fee | $output = $reserve_out - ($k / $new_reserve_in) | $price_before = $reserve_out / $reserve_in | $execution_price = $output / $swap_amount | $slippage_pct = (($price_before - $execution_price) / $price_before) * 100 | $recommended_slippage = $slippage_pct * 1.5

**Decision Point:** Slippage assessment | BRANCH A ($slippage_pct > 3): $rec = "Split trade" | BRANCH B ($slippage_pct >= 1): $rec = "Set {$recommended_slippage}% tolerance" | BRANCH C: $rec = "Low slippage - 0.5% tolerance OK"

**Action:** RETURN {swap_amount: $swap_amount, expected_output: $output, slippage_pct: $slippage_pct, recommended_tolerance_pct: $recommended_slippage, recommendation: $rec, confidence: 83}
```

---

## Q33: "Detect liquidity migration between protocols?"
**Plan:** [TIME: ~5s] [COST: free] [CONFIDENCE: 82%] | **Tools:** getAccountInfo, MAP, SUM, SORT

```ovsm
**Main Branch:** $protocols = ["RaydiumTVL", "OrcaTVL", "LifinityTVL"] | $current_tvls = getMultipleAccounts(pubkeys: $protocols) | $tvl_current = MAP(collection: $current_tvls, fn: acc => parseU128(data: acc.data, offset: 8) / 1e9) | $tvl_24h_ago = [5000000, 3000000, 1000000] | $changes = MAP(collection: RANGE(0, COUNT($tvl_current)), fn: i => {$change = $tvl_current[$i] - $tvl_24h_ago[$i] | $change_pct = ($change / $tvl_24h_ago[$i]) * 100 | RETURN {protocol: $protocols[$i], tvl_current: $tvl_current[$i], tvl_change: $change, change_pct: $change_pct}}) | $sorted = SORT(collection: $changes, by: "change_pct", order: "desc") | $winner = $sorted[0] | $loser = $sorted[COUNT($sorted) - 1]

**Decision Point:** Migration trend | BRANCH A ($winner.change_pct > 10): $trend = "strong_migration" | $note = "{$winner.protocol} gaining {$winner.change_pct}%" | BRANCH B ($loser.change_pct < -10): $trend = "exodus" | $note = "{$loser.protocol} losing {ABS($loser.change_pct)}%" | BRANCH C: $trend = "stable" | $note = "No significant migration"

**Action:** RETURN {protocols: $changes, winner: $winner, loser: $loser, trend: $trend, note: $note, confidence: 82}
```

---

## Q34: "Analyze options market sentiment via put/call ratio?"
**Plan:** [TIME: ~6s] [COST: free] [CONFIDENCE: 81%] | **Tools:** getProgramAccounts, MAP, FILTER, SUM

```ovsm
**Main Branch:** $options_program = "PsyOptions" | $underlying = "SOL" | $options = getProgramAccounts(programId: $options_program, filters: [{memcmp: {offset: 40, bytes: $underlying}}]) | $calls = FILTER(collection: $options, fn: opt => parseU8(data: opt.data, offset: 80) == 0) | $puts = FILTER(collection: $options, fn: opt => parseU8(data: opt.data, offset: 80) == 1) | $call_oi = SUM(collection: MAP($calls, fn: c => parseU64(data: c.data, offset: 88))) | $put_oi = SUM(collection: MAP($puts, fn: p => parseU64(data: p.data, offset: 88))) | $pcr = $put_oi / $call_oi

**Decision Point:** Market sentiment | BRANCH A ($pcr > 1.5): $sentiment = "bearish" | $note = "High put/call ratio {$pcr}" | BRANCH B ($pcr < 0.7): $sentiment = "bullish" | $note = "Low PCR - call demand" | BRANCH C: $sentiment = "neutral" | $note = "Balanced options"

**Action:** RETURN {underlying: $underlying, put_call_ratio: $pcr, call_oi: $call_oi, put_oi: $put_oi, sentiment: $sentiment, note: $note, confidence: 81}
```

---

## Q35: "Monitor liquidation cascade risk in lending protocol?"
**Plan:** [TIME: ~7s] [COST: free] [CONFIDENCE: 80%] | **Tools:** getAccountInfo, getProgramAccounts, MAP, FILTER, SORT

```ovsm
**Main Branch:** $lending = "SolendProgram" | $oracle = getAccountInfo(pubkey: "PythSOL") | $price = parseI64(data: $oracle.data, offset: 208) * POW(10, parseI32(data: $oracle.data, offset: 224)) | $obligations = getProgramAccounts(programId: $lending, filters: [{dataSize: 916}]) | $at_risk = MAP(collection: $obligations, fn: obl => {$collateral = parseU128(data: obl.data, offset: 8) / 1e18 | $borrowed = parseU128(data: obl.data, offset: 24) / 1e18 | $liq_threshold = parseU128(data: obl.data, offset: 56) / 1e18 | $buffer = $liq_threshold - $borrowed | $price_drop_pct = ($buffer / $collateral) * 100 | RETURN {address: obl.pubkey, collateral: $collateral, borrowed: $borrowed, price_drop_to_liq: $price_drop_pct}}) | $critical = FILTER(collection: $at_risk, fn: pos => pos.price_drop_to_liq < 10) | $sorted = SORT(collection: $critical, by: "price_drop_to_liq", order: "asc") | $total_at_risk = SUM(collection: MAP($critical, fn: p => p.collateral))

**Decision Point:** Cascade risk | BRANCH A (COUNT($critical) > 100 AND $total_at_risk > 10000000): $risk = "critical" | $alert = "Cascade risk - {COUNT($critical)} vulnerable" | BRANCH B (COUNT($critical) > 20): $risk = "elevated" | $alert = "Monitor liquidations" | BRANCH C: $risk = "low" | $alert = "Normal levels"

**Action:** RETURN {price: $price, positions_at_risk: COUNT($critical), collateral_at_risk: $total_at_risk, most_vulnerable: $sorted[0], risk: $risk, alert: $alert, confidence: 80}
```

---

## Q36: "Analyze NFT collateral valuation for DeFi lending?"
**Plan:** [TIME: ~8s] [COST: free] [CONFIDENCE: 79%] | **Tools:** getAccountInfo, getProgramAccounts, MAP

```ovsm
**Main Branch:** $nft_mint = "NFT_Mint_ABC" | $lending_program = "Sharky_Loans" | $nft_metadata = getAccountInfo(pubkey: $nft_mint) | $floor_price = parseU64(data: $nft_metadata.data, offset: 120) / LAMPORTS_PER_SOL | $recent_sales = getProgramAccounts(programId: $lending_program, filters: [{memcmp: {offset: 40, bytes: $nft_mint}}]) | $loan_amounts = MAP(collection: $recent_sales, fn: loan => parseU64(data: loan.data, offset: 88) / LAMPORTS_PER_SOL) | $avg_loan = SUM(collection: $loan_amounts) / COUNT($loan_amounts) | $max_ltv_pct = ($avg_loan / $floor_price) * 100 | $volatility = STDEV(array: $loan_amounts) / $avg_loan * 100

**Decision Point:** Collateral risk | BRANCH A ($max_ltv_pct > 60): $risk = "high" | $note = "Risky LTV {$max_ltv_pct}%" | BRANCH B ($max_ltv_pct >= 40): $risk = "moderate" | $note = "Standard NFT lending" | BRANCH C: $risk = "conservative" | $note = "Safe LTV"

**Action:** RETURN {nft_mint: $nft_mint, floor_price: $floor_price, avg_loan_amount: $avg_loan, max_ltv_pct: $max_ltv_pct, volatility_pct: $volatility, total_loans: COUNT($recent_sales), risk: $risk, note: $note, confidence: 79}
```

---

## Q37: "Monitor DAO treasury diversification and runway?"
**Plan:** [TIME: ~9s] [COST: free] [CONFIDENCE: 78%] | **Tools:** getAccountInfo, getTokenAccountsByOwner, MAP, SUM

```ovsm
**Main Branch:** $dao_treasury = "DAOTreasuryAddr" | $token_accounts = getTokenAccountsByOwner(owner: $dao_treasury, programId: TOKEN_PROGRAM) | $holdings = MAP(collection: $token_accounts, fn: acc => {$mint = acc.account.data.parsed.info.mint | $amount_ui = acc.account.data.parsed.info.tokenAmount.uiAmount | $price_usd = 20.0 | $value_usd = $amount_ui * $price_usd | RETURN {mint: $mint, amount: $amount_ui, value_usd: $value_usd}}) | $total_treasury_usd = SUM(collection: MAP($holdings, fn: h => h.value_usd)) | $sorted = SORT(collection: $holdings, by: "value_usd", order: "desc") | $top_asset_pct = ($sorted[0].value_usd / $total_treasury_usd) * 100 | $monthly_burn = 50000 | $runway_months = $total_treasury_usd / $monthly_burn

**Decision Point:** Treasury health | BRANCH A ($runway_months < 6): $health = "critical" | $alert = "Only {$runway_months} months runway" | BRANCH B ($runway_months < 12): $health = "caution" | $alert = "Limited runway" | BRANCH C: $health = "healthy" | $alert = "{$runway_months} months runway"

**Action:** RETURN {treasury: $dao_treasury, total_value_usd: $total_treasury_usd, assets_count: COUNT($holdings), top_asset_concentration_pct: $top_asset_pct, runway_months: $runway_months, monthly_burn: $monthly_burn, health: $health, alert: $alert, confidence: 78}
```

---

## Q38: "Detect real-world asset (RWA) token backing verification?"
**Plan:** [TIME: ~10s] [COST: free] [CONFIDENCE: 77%] | **Tools:** getAccountInfo, MAP

```ovsm
**Main Branch:** $rwa_token = "RWA_Property_Token" | $reserve_account = "ReserveProofAddr" | $token_supply_info = getTokenSupply(mint: $rwa_token) | $circulating_supply = $token_supply_info.value.uiAmount | $reserve_acc = getAccountInfo(pubkey: $reserve_account) | $verified_assets_count = parseU32(data: $reserve_acc.data, offset: 8) | $total_value_usd = parseU128(data: $reserve_acc.data, offset: 16) / 1e6 | $last_audit_timestamp = parseI64(data: $reserve_acc.data, offset: 32) | $value_per_token = $total_value_usd / $circulating_supply | $time_since_audit = getCurrentTimestamp() - $last_audit_timestamp | $days_since_audit = $time_since_audit / 86400

**Decision Point:** Backing verification | BRANCH A ($days_since_audit > 90): $status = "stale" | $alert = "Audit outdated - {$days_since_audit} days" | BRANCH B ($value_per_token < 0.95): $status = "undercollateralized" | $alert = "Backing shortfall detected" | BRANCH C: $status = "verified" | $alert = "Properly backed"

**Action:** RETURN {rwa_token: $rwa_token, circulating_supply: $circulating_supply, total_backing_usd: $total_value_usd, value_per_token: $value_per_token, verified_assets: $verified_assets_count, days_since_audit: $days_since_audit, status: $status, alert: $alert, confidence: 77}
```

---

## Q39: "Analyze yield aggregator vault strategy performance?"
**Plan:** [TIME: ~11s] [COST: free] [CONFIDENCE: 76%] | **Tools:** getAccountInfo, getSignaturesForAddress, MAP

```ovsm
**Main Branch:** $vault = "TulipVaultXYZ" | $vault_acc = getAccountInfo(pubkey: $vault) | $total_deposits = parseU128(data: $vault_acc.data, offset: 40) / 1e9 | $total_shares = parseU128(data: $vault_acc.data, offset: 56) / 1e9 | $share_price = $total_deposits / $total_shares | $initial_share_price = 1.0 | $roi_pct = (($share_price - $initial_share_price) / $initial_share_price) * 100 | $strategy_count = parseU8(data: $vault_acc.data, offset: 72) | $sigs = getSignaturesForAddress(address: $vault, options: {limit: 100}) | $rebalances_24h = COUNT(FILTER(collection: $sigs, fn: s => s.blockTime >= (getCurrentTimestamp() - 86400))) | $apr_current = parseU64(data: $vault_acc.data, offset: 80) / 100

**Decision Point:** Strategy efficiency | BRANCH A ($apr_current > 20): $rating = "excellent" | $note = "High-performing vault {$apr_current}% APR" | BRANCH B ($apr_current >= 10): $rating = "good" | $note = "Competitive yields" | BRANCH C: $rating = "underperforming" | $note = "Below market rates"

**Action:** RETURN {vault: $vault, total_deposits_usd: $total_deposits, share_price: $share_price, roi_pct: $roi_pct, current_apr: $apr_current, strategies_active: $strategy_count, rebalances_24h: $rebalances_24h, rating: $rating, note: $note, confidence: 76}
```

---

## Q40: "Track protocol insurance fund coverage ratio?"
**Plan:** [TIME: ~2s] [COST: free] [CONFIDENCE: 95%] | **Tools:** getAccountInfo, MAP

```ovsm
**Main Branch:** $insurance_fund = "InsuranceFundAddr" | $protocol_tvl = "ProtocolTVLAddr" | $fund_acc = getAccountInfo(pubkey: $insurance_fund) | $fund_balance = parseU128(data: $fund_acc.data, offset: 8) / 1e9 | $tvl_acc = getAccountInfo(pubkey: $protocol_tvl) | $protocol_tvl_value = parseU128(data: $tvl_acc.data, offset: 8) / 1e9 | $coverage_ratio_pct = ($fund_balance / $protocol_tvl_value) * 100 | $claims_paid_total = parseU128(data: $fund_acc.data, offset: 24) / 1e9 | $claims_count = parseU32(data: $fund_acc.data, offset: 40) | $avg_claim = IF $claims_count > 0 THEN $claims_paid_total / $claims_count ELSE 0

**Decision Point:** Coverage adequacy | BRANCH A ($coverage_ratio_pct > 10): $status = "well_capitalized" | $note = "Strong {$coverage_ratio_pct}% coverage" | BRANCH B ($coverage_ratio_pct >= 5): $status = "adequate" | $note = "Sufficient insurance" | BRANCH C ($coverage_ratio_pct < 5): $status = "undercapitalized" | $note = "Low coverage ratio" | BRANCH D ($coverage_ratio_pct < 2): $status = "critical" | $note = "Insufficient insurance funds"

**Action:** RETURN {insurance_fund: $insurance_fund, fund_balance_usd: $fund_balance, protocol_tvl_usd: $protocol_tvl_value, coverage_ratio_pct: $coverage_ratio_pct, total_claims_paid: $claims_paid_total, claims_count: $claims_count, avg_claim_size: $avg_claim, status: $status, note: $note, confidence: 95}
```

---

## Q41: "Analyze token launch fair price discovery mechanism?"
**Plan:** [TIME: ~3s] [COST: free] [CONFIDENCE: 94%] | **Tools:** getAccountInfo, getSignaturesForAddress, MAP

```ovsm
**Main Branch:** $launch_pool = "LaunchPoolAddr" | $token_mint = "NewTokenMint" | $pool = getAccountInfo(pubkey: $launch_pool) | $initial_liquidity = parseU64(data: $pool.data, offset: 72) / LAMPORTS_PER_SOL | $current_liquidity = parseU64(data: $pool.data, offset: 80) / LAMPORTS_PER_SOL | $price_initial = parseU64(data: $pool.data, offset: 120) / 1e6 | $price_current = parseU64(data: $pool.data, offset: 128) / 1e6 | $price_change_pct = (($price_current - $price_initial) / $price_initial) * 100 | $sigs = getSignaturesForAddress(address: $launch_pool, options: {limit: 500}) | $unique_buyers = COUNT(UNIQUE(array: MAP($sigs, fn: s => s.signer))) | $avg_buy_size = $current_liquidity / $unique_buyers

**Decision Point:** Launch health | BRANCH A ($price_change_pct > 100): $status = "pump" | $note = "Parabolic price action {$price_change_pct}%" | BRANCH B ($price_change_pct < -50): $status = "dump" | $note = "Heavy selling pressure" | BRANCH C ($unique_buyers > 1000): $status = "healthy" | $note = "Good distribution" | BRANCH D: $status = "concentrated" | $note = "Few holders"

**Action:** RETURN {launch_pool: $launch_pool, price_change_pct: $price_change_pct, unique_buyers: $unique_buyers, avg_buy_size: $avg_buy_size, current_price: $price_current, status: $status, note: $note, confidence: 94}
```

---

## Q42: "Detect liquidity bootstrap pool (LBP) optimal exit timing?"
**Plan:** [TIME: ~4s] [COST: free] [CONFIDENCE: 93%] | **Tools:** getAccountInfo, MAP

```ovsm
**Main Branch:** $lbp_pool = "LBP_PoolXYZ" | $pool = getAccountInfo(pubkey: $lbp_pool) | $weight_token_a = parseU32(data: $pool.data, offset: 88) | $weight_token_b = parseU32(data: $pool.data, offset: 92) | $start_weight_a = parseU32(data: $pool.data, offset: 96) | $end_weight_a = parseU32(data: $pool.data, offset: 100) | $start_time = parseI64(data: $pool.data, offset: 104) | $end_time = parseI64(data: $pool.data, offset: 112) | $current_time = getCurrentTimestamp() | $time_elapsed = $current_time - $start_time | $total_duration = $end_time - $start_time | $progress_pct = ($time_elapsed / $total_duration) * 100 | $current_price = parseU128(data: $pool.data, offset: 120) / 1e6 | $theoretical_lowest = parseU128(data: $pool.data, offset: 136) / 1e6 | $price_vs_lowest_pct = (($current_price - $theoretical_lowest) / $theoretical_lowest) * 100

**Decision Point:** Exit timing | BRANCH A ($progress_pct > 90): $timing = "late" | $rec = "LBP almost over - price near bottom" | BRANCH B ($progress_pct >= 70 AND $price_vs_lowest_pct < 10): $timing = "optimal" | $rec = "Good entry point - near theoretical low" | BRANCH C ($progress_pct < 50): $timing = "early" | $rec = "Wait for weight rebalancing" | BRANCH D: $timing = "moderate" | $rec = "Approaching optimal range"

**Action:** RETURN {lbp_pool: $lbp_pool, progress_pct: $progress_pct, current_weight_ratio: $weight_token_a / $weight_token_b, current_price: $current_price, theoretical_lowest: $theoretical_lowest, price_premium_pct: $price_vs_lowest_pct, timing: $timing, recommendation: $rec, confidence: 93}
```

---

## Q43: "Monitor protocol governance attack vectors?"
**Plan:** [TIME: ~5s] [COST: free] [CONFIDENCE: 92%] | **Tools:** getAccountInfo, getProgramAccounts, MAP, SORT

```ovsm
**Main Branch:** $governance = "GovProgram" | $active_proposals = getProgramAccounts(programId: $governance, filters: [{memcmp: {offset: 40, bytes: "1"}}]) | $voting_token = "GovTokenMint" | $token_supply = getTokenSupply(mint: $voting_token) | $total_supply = $token_supply.value.uiAmount | $top_holders = getTokenLargestAccounts(mint: $voting_token) | $holder_balances = MAP(collection: $top_holders.value, fn: h => h.uiAmount) | $top_10_holdings = SUM(collection: SLICE(array: $holder_balances, start: 0, end: 10)) | $concentration_pct = ($top_10_holdings / $total_supply) * 100 | $quorum_req = 40 | $single_wallet_control_threshold = $quorum_req / 100 * $total_supply

**Decision Point:** Attack risk | BRANCH A ($holder_balances[0] > $single_wallet_control_threshold): $risk = "critical" | $alert = "Single wallet can pass proposals" | BRANCH B ($concentration_pct > 75): $risk = "high" | $alert = "Top 10 control {$concentration_pct}%" | BRANCH C ($concentration_pct > 51): $risk = "moderate" | $alert = "Centralized voting power" | BRANCH D: $risk = "low" | $alert = "Distributed governance"

**Action:** RETURN {governance: $governance, active_proposals: COUNT($active_proposals), total_supply: $total_supply, top_holder_balance: $holder_balances[0], top_10_concentration_pct: $concentration_pct, quorum_requirement_pct: $quorum_req, risk: $risk, alert: $alert, confidence: 92}
```

---

## Q44: "Analyze perpetual DEX order book depth and liquidity?"
**Plan:** [TIME: ~6s] [COST: free] [CONFIDENCE: 91%] | **Tools:** getAccountInfo, getProgramAccounts, MAP, FILTER

```ovsm
**Main Branch:** $perp_market = "Mango_BTC_PERP" | $market = getAccountInfo(pubkey: $perp_market) | $oracle_price = parseI128(data: $market.data, offset: 72) / 1e6 | $orderbook_bids = getProgramAccounts(programId: MANGO_PROGRAM, filters: [{memcmp: {offset: 40, bytes: $perp_market}}, {memcmp: {offset: 80, bytes: "0"}}]) | $orderbook_asks = getProgramAccounts(programId: MANGO_PROGRAM, filters: [{memcmp: {offset: 40, bytes: $perp_market}}, {memcmp: {offset: 80, bytes: "1"}}]) | $bids_1pct = FILTER(collection: $orderbook_bids, fn: bid => {$price = parseU64(data: bid.data, offset: 88) / 1e6 | RETURN $price >= ($oracle_price * 0.99)}) | $asks_1pct = FILTER(collection: $orderbook_asks, fn: ask => {$price = parseU64(data: ask.data, offset: 88) / 1e6 | RETURN $price <= ($oracle_price * 1.01)}) | $bid_liquidity = SUM(collection: MAP($bids_1pct, fn: b => parseU64(data: b.data, offset: 96) / LAMPORTS_PER_SOL)) | $ask_liquidity = SUM(collection: MAP($asks_1pct, fn: a => parseU64(data: a.data, offset: 96) / LAMPORTS_PER_SOL)) | $total_depth_1pct = $bid_liquidity + $ask_liquidity | $imbalance = ($bid_liquidity - $ask_liquidity) / $total_depth_1pct * 100

**Decision Point:** Liquidity quality | BRANCH A ($total_depth_1pct > 1000000): $quality = "deep" | $note = "Excellent liquidity ${total_depth_1pct}" | BRANCH B ($total_depth_1pct >= 100000): $quality = "adequate" | $note = "Sufficient for most trades" | BRANCH C ($total_depth_1pct < 100000): $quality = "thin" | $note = "Risk of slippage" | BRANCH D (ABS($imbalance) > 30): $quality = "imbalanced" | $note = "One-sided order book"

**Action:** RETURN {market: $perp_market, oracle_price: $oracle_price, bid_liquidity_1pct: $bid_liquidity, ask_liquidity_1pct: $ask_liquidity, total_depth: $total_depth_1pct, imbalance_pct: $imbalance, quality: $quality, note: $note, confidence: 91}
```

---

## Q45: "Track MEV bot profitability and competition?"
**Plan:** [TIME: ~7s] [COST: free] [CONFIDENCE: 90%] | **Tools:** getSignaturesForAddress, getTransaction, MAP, FILTER

```ovsm
**Main Branch:** $mev_bot = "MEVBotWallet" | $window = 86400 | $sigs = getSignaturesForAddress(address: $mev_bot, options: {limit: 1000}) | $recent = FILTER(collection: $sigs, fn: s => s.blockTime >= (getCurrentTimestamp() - $window)) | $txs = MAP(collection: $recent, fn: s => getTransaction(signature: s.signature)) | $profitable_txs = FILTER(collection: $txs, fn: tx => tx.meta.postBalances[0] > tx.meta.preBalances[0]) | $profits = MAP(collection: $profitable_txs, fn: tx => (tx.meta.postBalances[0] - tx.meta.preBalances[0]) / LAMPORTS_PER_SOL) | $total_profit = SUM(collection: $profits) | $total_attempts = COUNT($txs) | $success_rate_pct = (COUNT($profitable_txs) / $total_attempts) * 100 | $avg_profit_per_success = $total_profit / COUNT($profitable_txs) | $priority_fees_paid = SUM(collection: MAP($txs, fn: tx => tx.meta.fee / LAMPORTS_PER_SOL))

**Decision Point:** Bot performance | BRANCH A ($success_rate_pct > 50 AND $total_profit > 100): $performance = "excellent" | $note = "Highly profitable - {$success_rate_pct}% success" | BRANCH B ($success_rate_pct >= 25 AND $total_profit > 10): $performance = "competitive" | $note = "Profitable MEV operation" | BRANCH C ($total_profit > 0): $performance = "marginal" | $note = "Barely profitable after fees" | BRANCH D: $performance = "unprofitable" | $note = "Losing money to competition"

**Action:** RETURN {mev_bot: $mev_bot, total_profit_sol: $total_profit, total_attempts: $total_attempts, successful_attempts: COUNT($profitable_txs), success_rate_pct: $success_rate_pct, avg_profit: $avg_profit_per_success, priority_fees_paid: $priority_fees_paid, net_profit: $total_profit - $priority_fees_paid, performance: $performance, note: $note, confidence: 90}
```

---

## Q46: "Analyze liquidity pool fee tier optimization?"
**Plan:** [TIME: ~8s] [CONFIDENCE: 89%] | **Tools:** getAccountInfo, MAP
```ovsm
**Main:** $pools = ["Pool_0.01%", "Pool_0.05%", "Pool_0.3%", "Pool_1%"] | $data = getMultipleAccounts(pubkeys: $pools) | $volumes = MAP(collection: $data, fn: p => parseU128(data: p.data, offset: 200) / 1e9) | $fees = MAP(collection: RANGE(0, 4), fn: i => $volumes[$i] * [0.0001, 0.0005, 0.003, 0.01][$i]) | $best_idx = ARGMAX(array: $fees) | $best_tier = [0.01, 0.05, 0.3, 1.0][$best_idx]
**Decision:** BRANCH A ($best_tier == 1.0): $rec = "High volatility - 1% tier" | BRANCH B ($best_tier == 0.01): $rec = "Stablecoin - 0.01% tier" | BRANCH C: $rec = "{$best_tier}% tier optimal"
**Action:** RETURN {pools: $pools, volumes: $volumes, fees_earned: $fees, optimal_tier: $best_tier, rec: $rec, confidence: 89}
```

---

## Q47: "Monitor cross-protocol composability risks?"
**Plan:** [TIME: ~9s] [CONFIDENCE: 88%] | **Tools:** getTransaction, MAP
```ovsm
**Main:** $tx = "ComposableTx" | $tx_data = getTransaction(signature: $tx) | $programs = UNIQUE(array: MAP($tx_data.transaction.message.instructions, fn: ix => ix.programId)) | $cpi_depth = COUNT($tx_data.meta.innerInstructions) | $protocol_count = COUNT($programs)
**Decision:** BRANCH A ($protocol_count > 5 AND $cpi_depth > 3): $risk = "high" | $note = "{$protocol_count} protocols interact" | BRANCH B ($protocol_count >= 3): $risk = "moderate" | BRANCH C: $risk = "low"
**Action:** RETURN {tx: $tx, protocols: $protocol_count, cpi_depth: $cpi_depth, risk: $risk, note: $note, confidence: 88}
```

---

## Q48: "Detect vampire attack liquidity migrations?"
**Plan:** [TIME: ~10s] [CONFIDENCE: 87%] | **Tools:** getAccountInfo, getSignaturesForAddress
```ovsm
**Main:** $old_dex = "OldDEX" | $new_dex = "NewDEX" | $migration = "VampireProgram" | $old_tvl = parseU128(data: getAccountInfo(pubkey: $old_dex).data, offset: 8) / 1e9 | $new_tvl = parseU128(data: getAccountInfo(pubkey: $new_dex).data, offset: 8) / 1e9 | $sigs = getSignaturesForAddress(address: $migration, options: {limit: 1000}) | $migrations = COUNT(FILTER(collection: $sigs, fn: s => s.blockTime >= (getCurrentTimestamp() - 86400)))
**Decision:** BRANCH A ($migrations > 100 AND $new_tvl > $old_tvl): $status = "active_attack" | BRANCH B ($migrations > 50): $status = "migration_wave" | BRANCH C: $status = "normal"
**Action:** RETURN {old_tvl: $old_tvl, new_tvl: $new_tvl, migrations_24h: $migrations, status: $status, confidence: 87}
```

---

## Q49: "Analyze protocol wars bribe efficiency?"
**Plan:** [TIME: ~11s] [CONFIDENCE: 86%] | **Tools:** getAccountInfo
```ovsm
**Main:** $bribe_pool = "BribePool" | $acc = getAccountInfo(pubkey: $bribe_pool) | $bribes = parseU128(data: $acc.data, offset: 40) / 1e9 | $votes = parseU128(data: $acc.data, offset: 56) / 1e9 | $emissions = parseU128(data: $acc.data, offset: 72) / 1e9 | $roi = ($emissions / $bribes - 1) * 100
**Decision:** BRANCH A ($roi > 200): $eff = "very_high" | BRANCH B ($roi > 100): $eff = "profitable" | BRANCH C ($roi > 0): $eff = "marginal" | BRANCH D: $eff = "unprofitable"
**Action:** RETURN {bribes: $bribes, votes: $votes, emissions: $emissions, roi_pct: $roi, efficiency: $eff, confidence: 86}
```

---

## Q50: "Track AMM capital efficiency metrics?"
**Plan:** [TIME: ~2s] [CONFIDENCE: 95%] | **Tools:** getAccountInfo
```ovsm
**Main:** $pool = "AMMPool" | $acc = getAccountInfo(pubkey: $pool) | $tvl = (parseU64(data: $acc.data, offset: 72) + parseU64(data: $acc.data, offset: 80)) / LAMPORTS_PER_SOL | $vol_24h = parseU128(data: $acc.data, offset: 200) / 1e9 | $efficiency = $vol_24h / $tvl | $fee_apr = ($vol_24h * 0.003 * 365 / $tvl) * 100
**Decision:** BRANCH A ($efficiency > 2): $rating = "highly_efficient" | BRANCH B ($efficiency > 1): $rating = "efficient" | BRANCH C: $rating = "inefficient"
**Action:** RETURN {pool: $pool, tvl: $tvl, volume_24h: $vol_24h, capital_efficiency: $efficiency, fee_apr: $fee_apr, rating: $rating, confidence: 95}
```

---

## Q51: "Analyze token economic sink mechanisms and deflation?"
**Plan:** [TIME: ~3s] [CONFIDENCE: 84%] | **Tools:** getAccountInfo, getSignaturesForAddress
```ovsm
**Main:** $token = "DeflationaryToken" | $burn_address = "BurnAddr111" | $supply = getTokenSupply(mint: $token) | $current_supply = $supply.value.uiAmount | $burn_sigs = getSignaturesForAddress(address: $burn_address, options: {limit: 1000}) | $burns_24h = FILTER(collection: $burn_sigs, fn: s => s.blockTime >= (getCurrentTimestamp() - 86400)) | $burned_24h = SUM(collection: MAP($burns_24h, fn: s => {$tx = getTransaction(signature: s.signature) | RETURN ABS($tx.meta.postTokenBalances[0].uiTokenAmount.uiAmount - $tx.meta.preTokenBalances[0].uiTokenAmount.uiAmount)})) | $burn_rate_annual = $burned_24h * 365 | $deflation_rate_pct = ($burn_rate_annual / $current_supply) * 100
**Decision:** BRANCH A ($deflation_rate_pct > 10): $impact = "aggressive_deflationary" | BRANCH B ($deflation_rate_pct >= 2): $impact = "deflationary" | BRANCH C: $impact = "minimal_deflation"
**Action:** RETURN {token: $token, current_supply: $current_supply, burned_24h: $burned_24h, annual_burn_rate: $burn_rate_annual, deflation_rate_pct: $deflation_rate_pct, impact: $impact, confidence: 84}
```

---

## Q52: "Monitor delta-neutral yield farming strategy health?"
**Plan:** [TIME: ~4s] [CONFIDENCE: 83%] | **Tools:** getAccountInfo
```ovsm
**Main:** $farm_position = "FarmPos" | $perp_hedge = "PerpHedge" | $farm = getAccountInfo(pubkey: $farm_position) | $farm_value = parseU64(data: $farm.data, offset: 40) / LAMPORTS_PER_SOL | $farm_apy = 45 | $perp = getAccountInfo(pubkey: $perp_hedge) | $short_size = parseU64(data: $perp.data, offset: 88) / LAMPORTS_PER_SOL | $funding_rate = parseI64(data: $perp.data, offset: 104) / 10000 | $hedge_ratio = $short_size / $farm_value | $net_apy = $farm_apy - ($funding_rate * 3 * 365)
**Decision:** BRANCH A (ABS($hedge_ratio - 1.0) > 0.1): $status = "unbalanced" | $note = "Hedge ratio {$hedge_ratio} off target" | BRANCH B ($net_apy < 5): $status = "unprofitable" | $note = "Funding costs too high" | BRANCH C: $status = "healthy" | $note = "Delta-neutral maintained"
**Action:** RETURN {farm_value: $farm_value, short_size: $short_size, hedge_ratio: $hedge_ratio, farm_apy: $farm_apy, funding_cost_apr: $funding_rate * 3 * 365, net_apy: $net_apy, status: $status, note: $note, confidence: 83}
```

---

## Q53: "Detect rug pull risk indicators in new token pools?"
**Plan:** [TIME: ~5s] [CONFIDENCE: 82%] | **Tools:** getAccountInfo, getProgramAccounts
```ovsm
**Main:** $pool = "NewTokenPool" | $pool_acc = getAccountInfo(pubkey: $pool) | $lp_mint = parsePublicKey(data: $pool_acc.data, offset: 40) | $lp_supply = getTokenSupply(mint: $lp_mint) | $burned_lp = parseU64(data: $lp_supply.value.amount) == 0 | $creator = parsePublicKey(data: $pool_acc.data, offset: 168) | $creator_tokens = getTokenAccountsByOwner(owner: $creator, programId: TOKEN_PROGRAM) | $creator_token_pct = 0 | $liquidity_locked_program = parsePublicKey(data: $pool_acc.data, offset: 200) | $has_timelock = $liquidity_locked_program != "11111111111111111111111111111111" | $pool_age_hours = (getCurrentTimestamp() - parseI64(data: $pool_acc.data, offset: 208)) / 3600
**Decision:** BRANCH A (NOT $burned_lp AND NOT $has_timelock): $risk = "critical" | $alert = "LP tokens not burned - rug risk!" | BRANCH B ($pool_age_hours < 24 AND $creator_token_pct > 50): $risk = "high" | $alert = "New pool, creator holds {$creator_token_pct}%" | BRANCH C ($pool_age_hours < 48): $risk = "moderate" | BRANCH D: $risk = "low"
**Action:** RETURN {pool: $pool, lp_burned: $burned_lp, has_timelock: $has_timelock, pool_age_hours: $pool_age_hours, creator_holding_pct: $creator_token_pct, risk: $risk, alert: $alert, confidence: 82}
```

---

## Q54: "Analyze yield optimizer vault fee structure fairness?"
**Plan:** [TIME: ~6s] [CONFIDENCE: 81%] | **Tools:** getAccountInfo
```ovsm
**Main:** $vault = "OptimizerVault" | $acc = getAccountInfo(pubkey: $vault) | $performance_fee_pct = parseU16(data: $acc.data, offset: 88) / 100 | $management_fee_pct = parseU16(data: $acc.data, offset: 90) / 100 | $withdrawal_fee_pct = parseU16(data: $acc.data, offset: 92) / 100 | $total_fees_pct = $performance_fee_pct + $management_fee_pct + $withdrawal_fee_pct | $vault_apr = parseU64(data: $acc.data, offset: 96) / 100 | $user_net_apr = $vault_apr - $management_fee_pct - ($vault_apr * $performance_fee_pct / 100)
**Decision:** BRANCH A ($total_fees_pct > 10): $fairness = "excessive" | $note = "Very high fees - {$total_fees_pct}%" | BRANCH B ($total_fees_pct > 5): $fairness = "high" | $note = "Above average fees" | BRANCH C ($total_fees_pct >= 2): $fairness = "fair" | $note = "Standard fee structure" | BRANCH D: $fairness = "competitive" | $note = "Low fee vault"
**Action:** RETURN {vault: $vault, performance_fee: $performance_fee_pct, management_fee: $management_fee_pct, withdrawal_fee: $withdrawal_fee_pct, total_fees: $total_fees_pct, vault_apr: $vault_apr, user_net_apr: $user_net_apr, fairness: $fairness, note: $note, confidence: 81}
```

---

## Q55: "Monitor leveraged yield farming liquidation proximity?"
**Plan:** [TIME: ~7s] [CONFIDENCE: 80%] | **Tools:** getAccountInfo
```ovsm
**Main:** $lev_position = "LevYieldPos" | $acc = getAccountInfo(pubkey: $lev_position) | $collateral = parseU128(data: $acc.data, offset: 8) / 1e18 | $debt = parseU128(data: $acc.data, offset: 24) / 1e18 | $leverage_ratio = ($collateral + $debt) / $collateral | $liq_threshold = parseU128(data: $acc.data, offset: 56) / 1e18 | $buffer = $liq_threshold - $debt | $price_drop_to_liq = ($buffer / $collateral) * 100 | $apy_levered = 60 * $leverage_ratio | $borrow_cost = 15 | $net_apy = $apy_levered - $borrow_cost
**Decision:** BRANCH A ($price_drop_to_liq < 5): $risk = "critical" | $alert = "URGENT: {$price_drop_to_liq}% from liquidation" | BRANCH B ($price_drop_to_liq < 15): $risk = "high" | BRANCH C: $risk = "moderate"
**Action:** RETURN {position: $lev_position, leverage: $leverage_ratio, collateral: $collateral, debt: $debt, price_drop_to_liq_pct: $price_drop_to_liq, net_apy: $net_apy, risk: $risk, alert: $alert, confidence: 80}
```

---

## Q56: "Analyze decentralized derivatives protocol margin requirements?"
**Plan:** [TIME: ~8s] [CONFIDENCE: 79%] | **Tools:** getAccountInfo
```ovsm
**Main:** $deriv_account = "DerivPosition" | $acc = getAccountInfo(pubkey: $deriv_account) | $position_size = parseU128(data: $acc.data, offset: 40) / 1e9 | $margin_posted = parseU64(data: $acc.data, offset: 56) / LAMPORTS_PER_SOL | $initial_margin_req = 0.10 | $maintenance_margin_req = 0.05 | $current_margin_ratio = $margin_posted / ($position_size * 1.0) | $excess_margin = $margin_posted - ($position_size * $maintenance_margin_req)
**Decision:** BRANCH A ($current_margin_ratio < $maintenance_margin_req): $status = "margin_call" | $alert = "URGENT: Add margin or close position" | BRANCH B ($current_margin_ratio < ($maintenance_margin_req * 1.2)): $status = "at_risk" | $alert = "Low margin buffer" | BRANCH C: $status = "healthy" | $alert = "Sufficient margin"
**Action:** RETURN {position_size_usd: $position_size, margin_posted: $margin_posted, margin_ratio: $current_margin_ratio, excess_margin: $excess_margin, status: $status, alert: $alert, confidence: 79}
```

---

## Q57: "Track liquidity mining emissions sustainability?"
**Plan:** [TIME: ~9s] [CONFIDENCE: 78%] | **Tools:** getAccountInfo
```ovsm
**Main:** $emissions_program = "LiqMiningProgram" | $acc = getAccountInfo(pubkey: $emissions_program) | $emissions_per_day = parseU128(data: $acc.data, offset: 40) / 1e9 | $total_allocated = parseU128(data: $acc.data, offset: 56) / 1e9 | $already_emitted = parseU128(data: $acc.data, offset: 72) / 1e9 | $remaining = $total_allocated - $already_emitted | $days_remaining = $remaining / $emissions_per_day | $current_tvl = parseU128(data: $acc.data, offset: 88) / 1e9 | $emission_to_tvl_ratio = ($emissions_per_day * 365) / $current_tvl
**Decision:** BRANCH A ($days_remaining < 30): $sustainability = "ending_soon" | $alert = "Emissions end in {$days_remaining} days" | BRANCH B ($emission_to_tvl_ratio > 1.0): $sustainability = "high_inflation" | $alert = "Unsustainable emissions rate" | BRANCH C: $sustainability = "sustainable" | $alert = "Healthy emission schedule"
**Action:** RETURN {emissions_per_day: $emissions_per_day, days_remaining: $days_remaining, already_emitted_pct: ($already_emitted / $total_allocated) * 100, emission_to_tvl_ratio: $emission_to_tvl_ratio, sustainability: $sustainability, alert: $alert, confidence: 78}
```

---

## Q58: "Monitor staking derivative peg stability (stSOL, mSOL)?"
**Plan:** [TIME: ~10s] [CONFIDENCE: 77%] | **Tools:** getAccountInfo
```ovsm
**Main:** $liquid_stake_token = "stSOL" | $pool = "stSOL_SOL_Pool" | $pool_acc = getAccountInfo(pubkey: $pool) | $reserve_stsol = parseU64(data: $pool_acc.data, offset: 72) / LAMPORTS_PER_SOL | $reserve_sol = parseU64(data: $pool_acc.data, offset: 80) / LAMPORTS_PER_SOL | $market_price = $reserve_sol / $reserve_stsol | $theoretical_price = 1.05 | $depeg_pct = ABS($market_price - $theoretical_price) / $theoretical_price * 100 | $exchange_rate_acc = getAccountInfo(pubkey: "stSOL_ExchangeRate") | $actual_exchange_rate = parseU64(data: $exchange_rate_acc.data, offset: 8) / 1e9
**Decision:** BRANCH A ($depeg_pct > 3): $peg_status = "severely_depegged" | $alert = "stSOL trading {$depeg_pct}% from peg" | BRANCH B ($depeg_pct >= 1): $peg_status = "depegged" | $alert = "Monitor peg deviation" | BRANCH C: $peg_status = "stable" | $alert = "Peg maintained"
**Action:** RETURN {liquid_stake_token: $liquid_stake_token, market_price: $market_price, theoretical_price: $theoretical_price, depeg_pct: $depeg_pct, actual_exchange_rate: $actual_exchange_rate, peg_status: $peg_status, alert: $alert, confidence: 77}
```

---

## Q59: "Analyze automated liquidator bot competition and efficiency?"
**Plan:** [TIME: ~11s] [CONFIDENCE: 76%] | **Tools:** getSignaturesForAddress, getTransaction
```ovsm
**Main:** $lending_protocol = "SolendProtocol" | $sigs = getSignaturesForAddress(address: $lending_protocol, options: {limit: 1000}) | $liquidation_txs = FILTER(collection: MAP($sigs, fn: s => getTransaction(signature: s.signature)), fn: tx => CONTAINS(array: MAP(tx.transaction.message.instructions, fn: ix => ix.programId), value: "LiquidateInstruction")) | $liquidators = UNIQUE(array: MAP($liquidation_txs, fn: tx => tx.transaction.message.accountKeys[0])) | $liquidator_profits = MAP(collection: $liquidators, fn: liq => {$liq_txs = FILTER(collection: $liquidation_txs, fn: tx => tx.transaction.message.accountKeys[0] == liq) | $total_profit = SUM(collection: MAP($liq_txs, fn: tx => (tx.meta.postBalances[0] - tx.meta.preBalances[0]) / LAMPORTS_PER_SOL)) | RETURN {liquidator: liq, liquidations: COUNT($liq_txs), profit_sol: $total_profit}}) | $top_liquidator = SORT(collection: $liquidator_profits, by: "profit_sol", order: "desc")[0]
**Decision:** BRANCH A (COUNT($liquidators) > 20): $competition = "high" | $note = "{COUNT($liquidators)} active liquidators" | BRANCH B (COUNT($liquidators) >= 5): $competition = "moderate" | BRANCH C: $competition = "low"
**Action:** RETURN {protocol: $lending_protocol, total_liquidations: COUNT($liquidation_txs), unique_liquidators: COUNT($liquidators), top_liquidator: $top_liquidator, competition: $competition, note: $note, confidence: 76}
```

---

## Q60: "Calculate optimal collateral mix for lending position?"
**Plan:** [TIME: ~2s] [CONFIDENCE: 95%] | **Tools:** getAccountInfo, getMultipleAccounts
```ovsm
**Main:** $assets = ["SOL_Reserve", "ETH_Reserve", "BTC_Reserve"] | $reserves = getMultipleAccounts(pubkeys: $assets) | $collateral_configs = MAP(collection: $reserves, fn: r => {$ltv = parseU8(data: r.data, offset: 200) | $liq_threshold = parseU8(data: r.data, offset: 201) | $liq_bonus = parseU8(data: r.data, offset: 202) | $volatility = parseU16(data: r.data, offset: 204) / 100 | RETURN {asset: r.pubkey, ltv_pct: $ltv, liq_threshold_pct: $liq_threshold, volatility_pct: $volatility, score: $ltv / ($volatility + 1)}}) | $sorted = SORT(collection: $collateral_configs, by: "score", order: "desc") | $optimal_asset = $sorted[0] | $optimal_mix_allocation = MAP(collection: $sorted, fn: (asset, idx) => {$weight = (COUNT($sorted) - idx) / SUM(RANGE(1, COUNT($sorted) + 1)) | RETURN {asset: asset.asset, recommended_allocation_pct: $weight * 100}})
**Decision:** BRANCH A ($optimal_asset.volatility_pct > 50): $rec = "High vol asset - use lower allocation" | BRANCH B ($optimal_asset.ltv_pct > 75): $rec = "{$optimal_asset.asset} optimal - high LTV" | BRANCH C: $rec = "Diversify across all assets"
**Action:** RETURN {assets_analyzed: COUNT($assets), optimal_single_asset: $optimal_asset, optimal_mix: $optimal_mix_allocation, recommendation: $rec, confidence: 95}
```

---

## Q61: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$protocol = "Solend"  // or Kamino, MarginFi, etc.

// Find user's obligation account (holds collateral + borrows)
$obligation_pda = findProgramAddress(
  seeds: ["obligation", $user_address],
  programId: SOLEND_PROGRAM
)

$obligation_account = getAccountInfo(pubkey: $obligation_pda)

GUARD $obligation_account != null ELSE
  RETURN ERROR(message: "User has no active position in lending protocol")

// Parse obligation data
// Offset 8: deposited_value (u128 in USD with 18 decimals)
// Offset 24: borrowed_value (u128 in USD with 18 decimals)
// Offset 40: allowed_borrow_value (u128 - max borrow based on LTV)
// Offset 56: unhealthy_borrow_value (u128 - liquidation threshold)

$deposited_value_raw = parseU128(data: $obligation_account.data, offset: 8)
$borrowed_value_raw = parseU128(data: $obligation_account.data, offset: 24)
$allowed_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 40)
$unhealthy_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 56)

// Convert from 1e18 scale to normal USD
$deposited_value_usd = $deposited_value_raw / 1e18
$borrowed_value_usd = $borrowed_value_raw / 1e18
$allowed_borrow_usd = $allowed_borrow_value_raw / 1e18
$liquidation_threshold_usd = $unhealthy_borrow_value_raw / 1e18

// Calculate collateral ratio (collateral / debt * 100)
TRY {
  $collateral_ratio_pct = ($deposited_value_usd / $borrowed_value_usd) * 100
} CATCH {
  // Handle case where borrowed_value is 0
  $collateral_ratio_pct = 99999  // Infinite collateral (no debt)
}

// Calculate utilization (how much of allowed borrow is used)
$borrow_utilization_pct = ($borrowed_value_usd / $allowed_borrow_usd) * 100

// Calculate distance to liquidation
$liquidation_buffer_usd = $liquidation_threshold_usd - $borrowed_value_usd
$liquidation_buffer_pct = ($liquidation_buffer_usd / $borrowed_value_usd) * 100

**Decision Point:** Assess position health
  BRANCH A ($collateral_ratio_pct < 110):
    $health = "critical"
    $risk = "immediate_liquidation_risk"
    $action = "URGENT: Add collateral or repay debt immediately"
  BRANCH B ($collateral_ratio_pct >= 110 AND $collateral_ratio_pct < 125):
    $health = "unhealthy"
    $risk = "high_liquidation_risk"
    $action = "Add collateral soon to avoid liquidation"
  BRANCH C ($collateral_ratio_pct >= 125 AND $collateral_ratio_pct < 150):
    $health = "moderate"
    $risk = "moderate_risk"
    $action = "Monitor position during market volatility"
  BRANCH D ($collateral_ratio_pct >= 150):
    $health = "healthy"
    $risk = "low_risk"
    $action = "Position is safe - could borrow more if desired"

**Action:**
RETURN {
  user_address: $user_address,
  collateral_ratio_pct: $collateral_ratio_pct,
  deposited_value_usd: $deposited_value_usd,
  borrowed_value_usd: $borrowed_value_usd,
  borrow_utilization_pct: $borrow_utilization_pct,
  liquidation_threshold_usd: $liquidation_threshold_usd,
  liquidation_buffer_usd: $liquidation_buffer_usd,
  health: $health,
  risk: $risk,
  recommended_action: $action,
  confidence: 94
}

---

## Q62: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$position_address = "PosXYZ"

// Get position/obligation account
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position not found")

// Parse position deposits and borrows arrays
// Offset 72: num_deposits (u8)
// Offset 73: deposits array (each entry 88 bytes)
// Offset varies: num_borrows (u8)
// Then borrows array

$num_deposits = parseU8(data: $position_account.data, offset: 72)
$deposits_offset = 73

// Collect reserve addresses for each deposit to get LTV ratios
$deposit_reserves = []
$deposit_amounts = []

// Parse each deposit (simplified - would loop in real implementation)
FOR $i IN RANGE(0, $num_deposits) {
  $deposit_offset = $deposits_offset + ($i * 88)
  $reserve_address = parsePublicKey(data: $position_account.data, offset: $deposit_offset)
  $deposited_amount = parseU128(data: $position_account.data, offset: $deposit_offset + 32)

  APPEND($deposit_reserves, $reserve_address)
  APPEND($deposit_amounts, $deposited_amount / 1e18)
}

// Get reserve configs to find liquidation LTV for each asset
$reserve_accounts = getMultipleAccounts(pubkeys: $deposit_reserves)

// Calculate weighted liquidation threshold
$total_collateral_usd = 0
$liquidation_value_usd = 0

FOR $i IN RANGE(0, $num_deposits) {
  $reserve_config = $reserve_accounts[$i]

  // Parse reserve liquidation LTV (offset 200)
  $liquidation_ltv_pct = parseU8(data: $reserve_config.data, offset: 200)

  // Get market price for this asset (offset 56)
  $price_usd = parseU128(data: $reserve_config.data, offset: 56) / 1e18

  $collateral_value = $deposit_amounts[$i] * $price_usd
  $total_collateral_usd = $total_collateral_usd + $collateral_value
  $liquidation_value_usd = $liquidation_value_usd + ($collateral_value * $liquidation_ltv_pct / 100)
}

// Get current borrow value
$borrowed_value_raw = parseU128(data: $position_account.data, offset: 24)
$borrowed_value_usd = $borrowed_value_raw / 1e18

// Calculate how close to liquidation
$buffer_usd = $liquidation_value_usd - $borrowed_value_usd
$buffer_pct = ($buffer_usd / $liquidation_value_usd) * 100

// Calculate price drop needed for liquidation
$price_drop_for_liquidation_pct = ($buffer_usd / $total_collateral_usd) * 100

**Decision Point:** Assess liquidation risk
  BRANCH A ($buffer_pct < 5):
    $risk_level = "critical"
    $warning = "URGENT: Position is {$buffer_pct}% from liquidation"
  BRANCH B ($buffer_pct >= 5 AND $buffer_pct < 15):
    $risk_level = "high"
    $warning = "High risk - only {$price_drop_for_liquidation_pct}% price drop until liquidation"
  BRANCH C ($buffer_pct >= 15 AND $buffer_pct < 30):
    $risk_level = "moderate"
    $warning = "Monitor closely during volatility"
  BRANCH D ($buffer_pct >= 30):
    $risk_level = "low"
    $warning = "Position is well collateralized"

**Action:**
RETURN {
  position_address: $position_address,
  total_collateral_usd: $total_collateral_usd,
  borrowed_value_usd: $borrowed_value_usd,
  liquidation_threshold_usd: $liquidation_value_usd,
  buffer_to_liquidation_usd: $buffer_usd,
  buffer_percentage: $buffer_pct,
  price_drop_for_liquidation_pct: $price_drop_for_liquidation_pct,
  risk_level: $risk_level,
  warning: $warning,
  confidence: 93
}

---

## Q63: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenAccountBalance
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse pool data (Raydium/Orca AMM example)
// Offset 40: token_a_vault (pubkey)
// Offset 72: token_b_vault (pubkey)
// Offset 104: token_a_mint (pubkey)
// Offset 136: token_b_mint (pubkey)

$token_a_vault = parsePublicKey(data: $pool_account.data, offset: 40)
$token_b_vault = parsePublicKey(data: $pool_account.data, offset: 72)
$token_a_mint = parsePublicKey(data: $pool_account.data, offset: 104)
$token_b_mint = parsePublicKey(data: $pool_account.data, offset: 136)

// Get token balances in vaults
$vault_a_balance_info = getTokenAccountBalance(pubkey: $token_a_vault)
$vault_b_balance_info = getTokenAccountBalance(pubkey: $token_b_vault)

$reserve_a_ui = $vault_a_balance_info.value.uiAmount
$reserve_b_ui = $vault_b_balance_info.value.uiAmount

// Get token prices (simplified - would use Pyth/Switchboard oracle in production)
$price_a_usd = 20.0   // Example: Token A = $20
$price_b_usd = 1.0    // Example: Token B = $1 (stablecoin)

// Calculate TVL
$tvl_token_a = $reserve_a_ui * $price_a_usd
$tvl_token_b = $reserve_b_ui * $price_b_usd
$total_tvl_usd = $tvl_token_a + $tvl_token_b

// Calculate pool composition
$token_a_pct = ($tvl_token_a / $total_tvl_usd) * 100
$token_b_pct = ($tvl_token_b / $total_tvl_usd) * 100

// Check if pool is balanced (50/50 is ideal for constant product AMM)
$balance_deviation = ABS($token_a_pct - 50)

**Decision Point:** Assess pool size and balance
  BRANCH A ($total_tvl_usd > 10000000):
    $size_category = "large"
    $liquidity_note = "Deep liquidity - low slippage for large trades"
  BRANCH B ($total_tvl_usd >= 1000000 AND $total_tvl_usd <= 10000000):
    $size_category = "medium"
    $liquidity_note = "Moderate liquidity - suitable for most trades"
  BRANCH C ($total_tvl_usd >= 100000 AND $total_tvl_usd < 1000000):
    $size_category = "small"
    $liquidity_note = "Limited liquidity - watch for price impact"
  BRANCH D ($total_tvl_usd < 100000):
    $size_category = "very_small"
    $liquidity_note = "Very low liquidity - high slippage risk"

**Decision Point:** Assess pool balance
  BRANCH A ($balance_deviation > 20):
    $balance_status = "imbalanced"
    $balance_note = "Pool significantly imbalanced - possible arbitrage opportunity"
  BRANCH B ($balance_deviation > 10 AND $balance_deviation <= 20):
    $balance_status = "slightly_imbalanced"
    $balance_note = "Minor imbalance - normal for active pools"
  BRANCH C ($balance_deviation <= 10):
    $balance_status = "balanced"
    $balance_note = "Well-balanced pool composition"

**Action:**
RETURN {
  pool_address: $pool_address,
  total_tvl_usd: $total_tvl_usd,
  token_a_mint: $token_a_mint,
  token_b_mint: $token_b_mint,
  token_a_reserve: $reserve_a_ui,
  token_b_reserve: $reserve_b_ui,
  token_a_value_usd: $tvl_token_a,
  token_b_value_usd: $tvl_token_b,
  token_a_percentage: $token_a_pct,
  token_b_percentage: $token_b_pct,
  size_category: $size_category,
  balance_status: $balance_status,
  liquidity_note: $liquidity_note,
  balance_note: $balance_note,
  confidence: 92
}

---

## Q64: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"
$current_time = getCurrentTimestamp()
$time_24h_ago = $current_time - 86400  // 86400 seconds = 24 hours

// Get all transactions for the pool in last 24h
$signatures = getSignaturesForAddress(
  address: $pool_address,
  options: {
    until: null,
    limit: 1000  // May need pagination for very active pools
  }
)

// Filter transactions from last 24 hours
$recent_signatures = FILTER(
  collection: $signatures,
  fn: sig => sig.blockTime >= $time_24h_ago
)

// Fetch full transaction details to analyze swap amounts
$transactions = MAP(
  collection: $recent_signatures,
  fn: sig => getTransaction(signature: sig.signature)
)

// Filter only successful swap transactions
$swap_txs = FILTER(
  collection: $transactions,
  fn: tx => tx != null AND tx.meta.err == null
)

// Extract swap volumes from token balance changes
$volumes_usd = MAP(collection: $swap_txs, fn: tx => {
  // Parse pre/post token balances to calculate swap amounts
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  // Find pool vault balance changes
  $vault_changes = []
  FOR $i IN RANGE(0, COUNT($post_balances)) {
    $pre = $pre_balances[$i].uiTokenAmount.uiAmount
    $post = $post_balances[$i].uiTokenAmount.uiAmount
    $change = ABS($post - $pre)
    APPEND($vault_changes, $change)
  }

  // Estimate USD value (simplified)
  $max_change = MAX(array: $vault_changes)
  $swap_value_usd = $max_change * 20.0  // Assume $20 token price
  RETURN $swap_value_usd
})

// Calculate total 24h volume
$total_volume_24h = SUM(collection: $volumes_usd)

// Calculate number of swaps
$swap_count = COUNT(collection: $swap_txs)

// Calculate average swap size
$avg_swap_size = $total_volume_24h / $swap_count

// Calculate volume per hour
$volume_per_hour = $total_volume_24h / 24

**Decision Point:** Assess trading activity
  BRANCH A ($total_volume_24h > 10000000):
    $activity_level = "very_high"
    $note = "Highly active pool - ${volume_per_hour}/hour average"
  BRANCH B ($total_volume_24h >= 1000000 AND $total_volume_24h <= 10000000):
    $activity_level = "high"
    $note = "Active trading - good liquidity depth"
  BRANCH C ($total_volume_24h >= 100000 AND $total_volume_24h < 1000000):
    $activity_level = "moderate"
    $note = "Moderate activity level"
  BRANCH D ($total_volume_24h < 100000):
    $activity_level = "low"
    $note = "Low trading volume - limited activity"

**Action:**
RETURN {
  pool_address: $pool_address,
  volume_24h_usd: $total_volume_24h,
  volume_per_hour_usd: $volume_per_hour,
  total_swaps: $swap_count,
  average_swap_size_usd: $avg_swap_size,
  time_period_start: $time_24h_ago,
  time_period_end: $current_time,
  activity_level: $activity_level,
  note: $note,
  confidence: 91
}

---

## Q65: "What is the current price impact for swapping 1000 tokens?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolSwapXYZ"
$swap_amount = 1000
$token_in_mint = "TokenA"
$token_out_mint = "TokenB"

// Get pool reserves
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse reserve amounts
$reserve_in = parseU64(data: $pool_account.data, offset: 72)
$reserve_out = parseU64(data: $pool_account.data, offset: 80)

// Calculate expected output with constant product formula (x * y = k)
$swap_amount_lamports = $swap_amount * LAMPORTS_PER_SOL
$k = $reserve_in * $reserve_out

// Account for 0.3% fee (typical for AMMs)
$fee_pct = 0.3
$amount_in_with_fee = $swap_amount_lamports * (100 - $fee_pct) / 100

// Calculate output amount
$new_reserve_in = $reserve_in + $amount_in_with_fee
$new_reserve_out = $k / $new_reserve_in
$output_amount = $reserve_out - $new_reserve_out

// Calculate expected price without slippage
$current_price = $reserve_out / $reserve_in
$expected_output_no_slippage = $swap_amount_lamports * $current_price

// Calculate actual execution price
$execution_price = $output_amount / $swap_amount_lamports

// Calculate price impact percentage
$price_impact_pct = (($expected_output_no_slippage - $output_amount) / $expected_output_no_slippage) * 100

// Convert to UI amounts for display
$output_ui = $output_amount / LAMPORTS_PER_SOL
$expected_output_ui = $expected_output_no_slippage / LAMPORTS_PER_SOL

// Calculate absolute loss due to price impact
$impact_loss_ui = $expected_output_ui - $output_ui

**Decision Point:** Assess price impact severity
  BRANCH A ($price_impact_pct > 5):
    $severity = "very_high"
    $recommendation = "CAUTION: {$price_impact_pct}% impact. Consider splitting into smaller trades or using limit orders"
  BRANCH B ($price_impact_pct >= 2 AND $price_impact_pct <= 5):
    $severity = "high"
    $recommendation = "High impact - evaluate if acceptable for your trade"
  BRANCH C ($price_impact_pct >= 0.5 AND $price_impact_pct < 2):
    $severity = "moderate"
    $recommendation = "Moderate impact - typical for this trade size"
  BRANCH D ($price_impact_pct < 0.5):
    $severity = "low"
    $recommendation = "Low impact - good liquidity for this trade size"

**Action:**
RETURN {
  pool_address: $pool_address,
  swap_amount_in: $swap_amount,
  expected_output: $output_ui,
  expected_without_impact: $expected_output_ui,
  price_impact_pct: $price_impact_pct,
  impact_loss_tokens: $impact_loss_ui,
  execution_price: $execution_price,
  current_market_price: $current_price,
  fee_percentage: $fee_pct,
  severity: $severity,
  recommendation: $recommendation,
  confidence: 90
}

---

## Q66: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenLargestAccounts, getTokenAccountsByOwner
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account to find LP token mint
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool account not found")

// Parse LP token mint address (offset varies by AMM, using Raydium example)
$lp_mint = parsePublicKey(data: $pool_account.data, offset: 40)

// Get largest LP token holders
$largest_holders = getTokenLargestAccounts(mint: $lp_mint)

// Get total LP supply
$supply_info = getTokenSupply(mint: $lp_mint)
$total_supply = $supply_info.value.uiAmount

// Calculate share percentage for each holder
$lp_details = MAP(collection: $largest_holders.value, fn: holder => {
  owner: holder.address,
  lp_balance: holder.uiAmount,
  share_percentage: (holder.uiAmount / $total_supply) * 100,
  rank: holder.rank
})

// Filter out very small positions (< 0.01%)
$significant_lps = FILTER(collection: $lp_details, fn: lp => lp.share_percentage >= 0.01)

// Sort by share percentage descending
$sorted_lps = SORT(collection: $significant_lps, by: "share_percentage", order: "desc")

// Count total LPs
$total_lps = COUNT(collection: $sorted_lps)

// Calculate concentration (top 10 holders)
$top_10 = SLICE(array: $sorted_lps, start: 0, end: 10)
$top_10_concentration = SUM(collection: MAP($top_10, fn: lp => lp.share_percentage))

**Decision Point:** Assess liquidity concentration
  BRANCH A ($top_10_concentration > 80):
    $concentration_level = "highly_concentrated"
    $risk = "High centralization risk - top 10 LPs control {$top_10_concentration}%"
  BRANCH B ($top_10_concentration >= 50 AND $top_10_concentration <= 80):
    $concentration_level = "moderately_concentrated"
    $risk = "Moderate concentration"
  BRANCH C ($top_10_concentration < 50):
    $concentration_level = "well_distributed"
    $risk = "Low concentration risk"

**Action:**
RETURN {
  pool_address: $pool_address,
  lp_mint: $lp_mint,
  total_lp_supply: $total_supply,
  total_providers: $total_lps,
  top_providers: $top_10,
  top_10_concentration_pct: $top_10_concentration,
  concentration_level: $concentration_level,
  risk_assessment: $risk,
  confidence: 89
}

---

## Q67: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenSupply
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$farm_address = "FarmXYZ"

// Get farm account
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm account not found")

// Parse farm data (Raydium farm example)
// Offset 8: reward_per_second (u64)
// Offset 16: total_staked (u64)
// Offset 24: reward_token_mint (pubkey)
// Offset 56: staked_token_mint (pubkey)

$reward_per_second = parseU64(data: $farm_account.data, offset: 8)
$total_staked = parseU64(data: $farm_account.data, offset: 16)
$reward_token_mint = parsePublicKey(data: $farm_account.data, offset: 24)
$staked_token_mint = parsePublicKey(data: $farm_account.data, offset: 56)

// Get token prices (simplified - would use real price oracle in production)
$reward_price_usd = 1.5  // Example: reward token = $1.50
$staked_price_usd = 20.0  // Example: staked token = $20.00

// Calculate annual rewards
$seconds_per_year = 31536000
$annual_rewards = $reward_per_second * $seconds_per_year

// Convert to USD value
$annual_rewards_usd = ($annual_rewards / LAMPORTS_PER_SOL) * $reward_price_usd
$total_staked_usd = ($total_staked / LAMPORTS_PER_SOL) * $staked_price_usd

// Calculate APY percentage
$apy_pct = ($annual_rewards_usd / $total_staked_usd) * 100

// Calculate daily rewards per 1000 tokens staked
$daily_per_1000 = ($apy_pct / 365) * 10

**Decision Point:** Assess APY attractiveness
  BRANCH A ($apy_pct > 100):
    $rating = "very_high"
    $risk_note = "Extremely high APY may indicate unsustainable emissions or high risk"
  BRANCH B ($apy_pct >= 30 AND $apy_pct <= 100):
    $rating = "attractive"
    $risk_note = "Competitive APY for DeFi farming"
  BRANCH C ($apy_pct >= 10 AND $apy_pct < 30):
    $rating = "moderate"
    $risk_note = "Moderate returns"
  BRANCH D ($apy_pct < 10):
    $rating = "low"
    $risk_note = "Low APY - may not compensate for impermanent loss risk"

**Action:**
RETURN {
  farm_address: $farm_address,
  apy_percentage: $apy_pct,
  annual_rewards_usd: $annual_rewards_usd,
  total_staked_usd: $total_staked_usd,
  daily_per_1000_tokens: $daily_per_1000,
  reward_token_mint: $reward_token_mint,
  staked_token_mint: $staked_token_mint,
  rating: $rating,
  risk_note: $risk_note,
  confidence: 88
}

---

## Q68: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$farm_address = "FarmXYZ"

// Find user's stake account PDA (Program Derived Address)
// Seeds typically: ["user_stake", farm_address, user_address]
$stake_pda = findProgramAddress(
  seeds: ["user_stake", $farm_address, $user_address],
  programId: RAYDIUM_STAKING_PROGRAM
)

// Get user's stake account
$stake_account = getAccountInfo(pubkey: $stake_pda)

GUARD $stake_account != null ELSE
  RETURN ERROR(message: "User has no active stake in this farm")

// Get farm account for reward calculation
$farm_account = getAccountInfo(pubkey: $farm_address)

// Parse user stake data
// Offset 8: staked_amount (u64)
// Offset 16: reward_debt (u64) - used for reward calculation
// Offset 24: last_update_time (i64)
// Offset 32: pending_rewards (u64)

$staked_amount = parseU64(data: $stake_account.data, offset: 8)
$reward_debt = parseU64(data: $stake_account.data, offset: 16)
$last_update_time = parseI64(data: $stake_account.data, offset: 24)
$pending_rewards = parseU64(data: $stake_account.data, offset: 32)

// Parse farm data for current reward calculations
$acc_reward_per_share = parseU128(data: $farm_account.data, offset: 88)
$reward_per_second = parseU64(data: $farm_account.data, offset: 8)

// Calculate newly accrued rewards since last update
$current_time = getCurrentTimestamp()
$time_elapsed = $current_time - $last_update_time
$new_rewards = ($staked_amount * $acc_reward_per_share / 1e12) - $reward_debt

// Total pending rewards
$total_pending = $pending_rewards + $new_rewards

// Convert to UI amount
$pending_ui_amount = $total_pending / LAMPORTS_PER_SOL

// Estimate USD value (example price)
$reward_token_price = 1.5
$pending_usd = $pending_ui_amount * $reward_token_price

// Calculate estimated daily earnings
$daily_rate = ($reward_per_second * 86400 * $staked_amount) / parseU64(data: $farm_account.data, offset: 16)
$daily_earnings_ui = $daily_rate / LAMPORTS_PER_SOL
$daily_earnings_usd = $daily_earnings_ui * $reward_token_price

**Decision Point:** Assess reward claim urgency
  BRANCH A ($pending_ui_amount > 100):
    $recommendation = "Large pending rewards - consider claiming to avoid loss"
    $urgency = "high"
  BRANCH B ($pending_ui_amount >= 10 AND $pending_ui_amount <= 100):
    $recommendation = "Moderate rewards - claim when transaction fees are low"
    $urgency = "medium"
  BRANCH C ($pending_ui_amount < 10):
    $recommendation = "Small rewards - wait to accumulate more before claiming"
    $urgency = "low"

**Action:**
RETURN {
  user_address: $user_address,
  farm_address: $farm_address,
  staked_amount_ui: $staked_amount / LAMPORTS_PER_SOL,
  pending_rewards_ui: $pending_ui_amount,
  pending_rewards_usd: $pending_usd,
  daily_earnings_ui: $daily_earnings_ui,
  daily_earnings_usd: $daily_earnings_usd,
  time_since_last_update_seconds: $time_elapsed,
  recommendation: $recommendation,
  urgency: $urgency,
  confidence: 87
}

---

## Q69: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get lending pool reserve account (Solend/Kamino example)
$reserve_account = getAccountInfo(pubkey: $pool_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Lending pool reserve not found")

// Parse reserve account data
// Offset 8: version (u8)
// Offset 16: available_liquidity (u64)
// Offset 24: borrowed_amount_wads (u128)
// Offset 40: cumulative_borrow_rate_wads (u128)
// Offset 56: market_price (u128)

$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)

// Convert borrowed amount from wads (1e18 scale) to normal amount
$borrowed_amount = $borrowed_amount_wads / 1e18

// Calculate total supply
$total_supply = $available_liquidity + $borrowed_amount

// Calculate utilization rate as percentage
$utilization_rate = ($borrowed_amount / $total_supply) * 100

// Convert to UI amounts for display
$available_ui = $available_liquidity / LAMPORTS_PER_SOL
$borrowed_ui = $borrowed_amount / LAMPORTS_PER_SOL
$total_supply_ui = $total_supply / LAMPORTS_PER_SOL

**Decision Point:** Assess pool health and risk
  BRANCH A ($utilization_rate > 90):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Critically high utilization - may limit withdrawals, increased liquidation risk"
  BRANCH B ($utilization_rate >= 80 AND $utilization_rate <= 90):
    $health_status = "stressed"
    $risk_level = "elevated"
    $note = "High utilization - monitor for liquidity crunches"
  BRANCH C ($utilization_rate >= 50 AND $utilization_rate < 80):
    $health_status = "healthy"
    $risk_level = "moderate"
    $note = "Optimal utilization range for lending pool"
  BRANCH D ($utilization_rate < 50):
    $health_status = "underutilized"
    $risk_level = "low"
    $note = "Low utilization - excess idle capital, lower APY for lenders"

**Action:**
RETURN {
  pool_address: $pool_address,
  utilization_rate_pct: $utilization_rate,
  available_liquidity_ui: $available_ui,
  borrowed_amount_ui: $borrowed_ui,
  total_supply_ui: $total_supply_ui,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 86
}

---

## Q70: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$asset_mint = "AssetXYZ"
$reserve_address = "ReserveXYZ"  // Reserve account for this asset

// Get reserve account
$reserve_account = getAccountInfo(pubkey: $reserve_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Asset reserve not found")

// Parse reserve configuration and state
// Offset 88: current_borrow_rate (u128 in wads - 1e18)
// Offset 104: cumulative_borrow_rate (u128 in wads)
// Offset 120: optimal_utilization_rate (u8 - percentage)
// Offset 121: max_borrow_rate (u64)

$current_borrow_rate_wads = parseU128(data: $reserve_account.data, offset: 88)
$optimal_utilization = parseU8(data: $reserve_account.data, offset: 120)
$max_borrow_rate = parseU64(data: $reserve_account.data, offset: 121)

// Convert borrow rate from wads (per second) to APY percentage
// Formula: APY = (1 + rate_per_second)^seconds_per_year - 1
$rate_per_second = $current_borrow_rate_wads / 1e18
$seconds_per_year = 31536000
$apy_multiplier = POW(base: (1 + $rate_per_second), exponent: $seconds_per_year)
$borrow_apy_pct = ($apy_multiplier - 1) * 100

// Simplified calculation (linear approximation for readability)
$borrow_apy_simple = $rate_per_second * $seconds_per_year * 100

// Get current utilization to explain rate
$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)
$borrowed_amount = $borrowed_amount_wads / 1e18
$current_utilization = ($borrowed_amount / ($available_liquidity + $borrowed_amount)) * 100

**Decision Point:** Assess borrow rate competitiveness
  BRANCH A ($borrow_apy_simple > 20):
    $rating = "expensive"
    $note = "High borrow cost - {$borrow_apy_simple}% APY. Consider alternative protocols or wait for lower rates"
  BRANCH B ($borrow_apy_simple >= 10 AND $borrow_apy_simple <= 20):
    $rating = "moderate"
    $note = "Standard borrow rate for current market conditions"
  BRANCH C ($borrow_apy_simple >= 5 AND $borrow_apy_simple < 10):
    $rating = "attractive"
    $note = "Competitive borrow rate - good opportunity for leverage"
  BRANCH D ($borrow_apy_simple < 5):
    $rating = "very_attractive"
    $note = "Exceptionally low borrow rate - excellent leverage opportunity"

**Action:**
RETURN {
  asset_mint: $asset_mint,
  reserve_address: $reserve_address,
  borrow_apy_pct: $borrow_apy_simple,
  current_utilization_pct: $current_utilization,
  optimal_utilization_pct: $optimal_utilization,
  max_borrow_rate: $max_borrow_rate,
  rating: $rating,
  note: $note,
  confidence: 85
}

---

## Q71: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$protocol = "Solend"  // or Kamino, MarginFi, etc.

// Find user's obligation account (holds collateral + borrows)
$obligation_pda = findProgramAddress(
  seeds: ["obligation", $user_address],
  programId: SOLEND_PROGRAM
)

$obligation_account = getAccountInfo(pubkey: $obligation_pda)

GUARD $obligation_account != null ELSE
  RETURN ERROR(message: "User has no active position in lending protocol")

// Parse obligation data
// Offset 8: deposited_value (u128 in USD with 18 decimals)
// Offset 24: borrowed_value (u128 in USD with 18 decimals)
// Offset 40: allowed_borrow_value (u128 - max borrow based on LTV)
// Offset 56: unhealthy_borrow_value (u128 - liquidation threshold)

$deposited_value_raw = parseU128(data: $obligation_account.data, offset: 8)
$borrowed_value_raw = parseU128(data: $obligation_account.data, offset: 24)
$allowed_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 40)
$unhealthy_borrow_value_raw = parseU128(data: $obligation_account.data, offset: 56)

// Convert from 1e18 scale to normal USD
$deposited_value_usd = $deposited_value_raw / 1e18
$borrowed_value_usd = $borrowed_value_raw / 1e18
$allowed_borrow_usd = $allowed_borrow_value_raw / 1e18
$liquidation_threshold_usd = $unhealthy_borrow_value_raw / 1e18

// Calculate collateral ratio (collateral / debt * 100)
TRY {
  $collateral_ratio_pct = ($deposited_value_usd / $borrowed_value_usd) * 100
} CATCH {
  // Handle case where borrowed_value is 0
  $collateral_ratio_pct = 99999  // Infinite collateral (no debt)
}

// Calculate utilization (how much of allowed borrow is used)
$borrow_utilization_pct = ($borrowed_value_usd / $allowed_borrow_usd) * 100

// Calculate distance to liquidation
$liquidation_buffer_usd = $liquidation_threshold_usd - $borrowed_value_usd
$liquidation_buffer_pct = ($liquidation_buffer_usd / $borrowed_value_usd) * 100

**Decision Point:** Assess position health
  BRANCH A ($collateral_ratio_pct < 110):
    $health = "critical"
    $risk = "immediate_liquidation_risk"
    $action = "URGENT: Add collateral or repay debt immediately"
  BRANCH B ($collateral_ratio_pct >= 110 AND $collateral_ratio_pct < 125):
    $health = "unhealthy"
    $risk = "high_liquidation_risk"
    $action = "Add collateral soon to avoid liquidation"
  BRANCH C ($collateral_ratio_pct >= 125 AND $collateral_ratio_pct < 150):
    $health = "moderate"
    $risk = "moderate_risk"
    $action = "Monitor position during market volatility"
  BRANCH D ($collateral_ratio_pct >= 150):
    $health = "healthy"
    $risk = "low_risk"
    $action = "Position is safe - could borrow more if desired"

**Action:**
RETURN {
  user_address: $user_address,
  collateral_ratio_pct: $collateral_ratio_pct,
  deposited_value_usd: $deposited_value_usd,
  borrowed_value_usd: $borrowed_value_usd,
  borrow_utilization_pct: $borrow_utilization_pct,
  liquidation_threshold_usd: $liquidation_threshold_usd,
  liquidation_buffer_usd: $liquidation_buffer_usd,
  health: $health,
  risk: $risk,
  recommended_action: $action,
  confidence: 84
}

---

## Q72: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getMultipleAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$position_address = "PosXYZ"

// Get position/obligation account
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position not found")

// Parse position deposits and borrows arrays
// Offset 72: num_deposits (u8)
// Offset 73: deposits array (each entry 88 bytes)
// Offset varies: num_borrows (u8)
// Then borrows array

$num_deposits = parseU8(data: $position_account.data, offset: 72)
$deposits_offset = 73

// Collect reserve addresses for each deposit to get LTV ratios
$deposit_reserves = []
$deposit_amounts = []

// Parse each deposit (simplified - would loop in real implementation)
FOR $i IN RANGE(0, $num_deposits) {
  $deposit_offset = $deposits_offset + ($i * 88)
  $reserve_address = parsePublicKey(data: $position_account.data, offset: $deposit_offset)
  $deposited_amount = parseU128(data: $position_account.data, offset: $deposit_offset + 32)

  APPEND($deposit_reserves, $reserve_address)
  APPEND($deposit_amounts, $deposited_amount / 1e18)
}

// Get reserve configs to find liquidation LTV for each asset
$reserve_accounts = getMultipleAccounts(pubkeys: $deposit_reserves)

// Calculate weighted liquidation threshold
$total_collateral_usd = 0
$liquidation_value_usd = 0

FOR $i IN RANGE(0, $num_deposits) {
  $reserve_config = $reserve_accounts[$i]

  // Parse reserve liquidation LTV (offset 200)
  $liquidation_ltv_pct = parseU8(data: $reserve_config.data, offset: 200)

  // Get market price for this asset (offset 56)
  $price_usd = parseU128(data: $reserve_config.data, offset: 56) / 1e18

  $collateral_value = $deposit_amounts[$i] * $price_usd
  $total_collateral_usd = $total_collateral_usd + $collateral_value
  $liquidation_value_usd = $liquidation_value_usd + ($collateral_value * $liquidation_ltv_pct / 100)
}

// Get current borrow value
$borrowed_value_raw = parseU128(data: $position_account.data, offset: 24)
$borrowed_value_usd = $borrowed_value_raw / 1e18

// Calculate how close to liquidation
$buffer_usd = $liquidation_value_usd - $borrowed_value_usd
$buffer_pct = ($buffer_usd / $liquidation_value_usd) * 100

// Calculate price drop needed for liquidation
$price_drop_for_liquidation_pct = ($buffer_usd / $total_collateral_usd) * 100

**Decision Point:** Assess liquidation risk
  BRANCH A ($buffer_pct < 5):
    $risk_level = "critical"
    $warning = "URGENT: Position is {$buffer_pct}% from liquidation"
  BRANCH B ($buffer_pct >= 5 AND $buffer_pct < 15):
    $risk_level = "high"
    $warning = "High risk - only {$price_drop_for_liquidation_pct}% price drop until liquidation"
  BRANCH C ($buffer_pct >= 15 AND $buffer_pct < 30):
    $risk_level = "moderate"
    $warning = "Monitor closely during volatility"
  BRANCH D ($buffer_pct >= 30):
    $risk_level = "low"
    $warning = "Position is well collateralized"

**Action:**
RETURN {
  position_address: $position_address,
  total_collateral_usd: $total_collateral_usd,
  borrowed_value_usd: $borrowed_value_usd,
  liquidation_threshold_usd: $liquidation_value_usd,
  buffer_to_liquidation_usd: $buffer_usd,
  buffer_percentage: $buffer_pct,
  price_drop_for_liquidation_pct: $price_drop_for_liquidation_pct,
  risk_level: $risk_level,
  warning: $warning,
  confidence: 83
}

---

## Q73: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenAccountBalance
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse pool data (Raydium/Orca AMM example)
// Offset 40: token_a_vault (pubkey)
// Offset 72: token_b_vault (pubkey)
// Offset 104: token_a_mint (pubkey)
// Offset 136: token_b_mint (pubkey)

$token_a_vault = parsePublicKey(data: $pool_account.data, offset: 40)
$token_b_vault = parsePublicKey(data: $pool_account.data, offset: 72)
$token_a_mint = parsePublicKey(data: $pool_account.data, offset: 104)
$token_b_mint = parsePublicKey(data: $pool_account.data, offset: 136)

// Get token balances in vaults
$vault_a_balance_info = getTokenAccountBalance(pubkey: $token_a_vault)
$vault_b_balance_info = getTokenAccountBalance(pubkey: $token_b_vault)

$reserve_a_ui = $vault_a_balance_info.value.uiAmount
$reserve_b_ui = $vault_b_balance_info.value.uiAmount

// Get token prices (simplified - would use Pyth/Switchboard oracle in production)
$price_a_usd = 20.0   // Example: Token A = $20
$price_b_usd = 1.0    // Example: Token B = $1 (stablecoin)

// Calculate TVL
$tvl_token_a = $reserve_a_ui * $price_a_usd
$tvl_token_b = $reserve_b_ui * $price_b_usd
$total_tvl_usd = $tvl_token_a + $tvl_token_b

// Calculate pool composition
$token_a_pct = ($tvl_token_a / $total_tvl_usd) * 100
$token_b_pct = ($tvl_token_b / $total_tvl_usd) * 100

// Check if pool is balanced (50/50 is ideal for constant product AMM)
$balance_deviation = ABS($token_a_pct - 50)

**Decision Point:** Assess pool size and balance
  BRANCH A ($total_tvl_usd > 10000000):
    $size_category = "large"
    $liquidity_note = "Deep liquidity - low slippage for large trades"
  BRANCH B ($total_tvl_usd >= 1000000 AND $total_tvl_usd <= 10000000):
    $size_category = "medium"
    $liquidity_note = "Moderate liquidity - suitable for most trades"
  BRANCH C ($total_tvl_usd >= 100000 AND $total_tvl_usd < 1000000):
    $size_category = "small"
    $liquidity_note = "Limited liquidity - watch for price impact"
  BRANCH D ($total_tvl_usd < 100000):
    $size_category = "very_small"
    $liquidity_note = "Very low liquidity - high slippage risk"

**Decision Point:** Assess pool balance
  BRANCH A ($balance_deviation > 20):
    $balance_status = "imbalanced"
    $balance_note = "Pool significantly imbalanced - possible arbitrage opportunity"
  BRANCH B ($balance_deviation > 10 AND $balance_deviation <= 20):
    $balance_status = "slightly_imbalanced"
    $balance_note = "Minor imbalance - normal for active pools"
  BRANCH C ($balance_deviation <= 10):
    $balance_status = "balanced"
    $balance_note = "Well-balanced pool composition"

**Action:**
RETURN {
  pool_address: $pool_address,
  total_tvl_usd: $total_tvl_usd,
  token_a_mint: $token_a_mint,
  token_b_mint: $token_b_mint,
  token_a_reserve: $reserve_a_ui,
  token_b_reserve: $reserve_b_ui,
  token_a_value_usd: $tvl_token_a,
  token_b_value_usd: $tvl_token_b,
  token_a_percentage: $token_a_pct,
  token_b_percentage: $token_b_pct,
  size_category: $size_category,
  balance_status: $balance_status,
  liquidity_note: $liquidity_note,
  balance_note: $balance_note,
  confidence: 82
}

---

## Q74: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"
$current_time = getCurrentTimestamp()
$time_24h_ago = $current_time - 86400  // 86400 seconds = 24 hours

// Get all transactions for the pool in last 24h
$signatures = getSignaturesForAddress(
  address: $pool_address,
  options: {
    until: null,
    limit: 1000  // May need pagination for very active pools
  }
)

// Filter transactions from last 24 hours
$recent_signatures = FILTER(
  collection: $signatures,
  fn: sig => sig.blockTime >= $time_24h_ago
)

// Fetch full transaction details to analyze swap amounts
$transactions = MAP(
  collection: $recent_signatures,
  fn: sig => getTransaction(signature: sig.signature)
)

// Filter only successful swap transactions
$swap_txs = FILTER(
  collection: $transactions,
  fn: tx => tx != null AND tx.meta.err == null
)

// Extract swap volumes from token balance changes
$volumes_usd = MAP(collection: $swap_txs, fn: tx => {
  // Parse pre/post token balances to calculate swap amounts
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  // Find pool vault balance changes
  $vault_changes = []
  FOR $i IN RANGE(0, COUNT($post_balances)) {
    $pre = $pre_balances[$i].uiTokenAmount.uiAmount
    $post = $post_balances[$i].uiTokenAmount.uiAmount
    $change = ABS($post - $pre)
    APPEND($vault_changes, $change)
  }

  // Estimate USD value (simplified)
  $max_change = MAX(array: $vault_changes)
  $swap_value_usd = $max_change * 20.0  // Assume $20 token price
  RETURN $swap_value_usd
})

// Calculate total 24h volume
$total_volume_24h = SUM(collection: $volumes_usd)

// Calculate number of swaps
$swap_count = COUNT(collection: $swap_txs)

// Calculate average swap size
$avg_swap_size = $total_volume_24h / $swap_count

// Calculate volume per hour
$volume_per_hour = $total_volume_24h / 24

**Decision Point:** Assess trading activity
  BRANCH A ($total_volume_24h > 10000000):
    $activity_level = "very_high"
    $note = "Highly active pool - ${volume_per_hour}/hour average"
  BRANCH B ($total_volume_24h >= 1000000 AND $total_volume_24h <= 10000000):
    $activity_level = "high"
    $note = "Active trading - good liquidity depth"
  BRANCH C ($total_volume_24h >= 100000 AND $total_volume_24h < 1000000):
    $activity_level = "moderate"
    $note = "Moderate activity level"
  BRANCH D ($total_volume_24h < 100000):
    $activity_level = "low"
    $note = "Low trading volume - limited activity"

**Action:**
RETURN {
  pool_address: $pool_address,
  volume_24h_usd: $total_volume_24h,
  volume_per_hour_usd: $volume_per_hour,
  total_swaps: $swap_count,
  average_swap_size_usd: $avg_swap_size,
  time_period_start: $time_24h_ago,
  time_period_end: $current_time,
  activity_level: $activity_level,
  note: $note,
  confidence: 81
}

---

## Q75: "What is the current price impact for swapping 1000 tokens?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolSwapXYZ"
$swap_amount = 1000
$token_in_mint = "TokenA"
$token_out_mint = "TokenB"

// Get pool reserves
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse reserve amounts
$reserve_in = parseU64(data: $pool_account.data, offset: 72)
$reserve_out = parseU64(data: $pool_account.data, offset: 80)

// Calculate expected output with constant product formula (x * y = k)
$swap_amount_lamports = $swap_amount * LAMPORTS_PER_SOL
$k = $reserve_in * $reserve_out

// Account for 0.3% fee (typical for AMMs)
$fee_pct = 0.3
$amount_in_with_fee = $swap_amount_lamports * (100 - $fee_pct) / 100

// Calculate output amount
$new_reserve_in = $reserve_in + $amount_in_with_fee
$new_reserve_out = $k / $new_reserve_in
$output_amount = $reserve_out - $new_reserve_out

// Calculate expected price without slippage
$current_price = $reserve_out / $reserve_in
$expected_output_no_slippage = $swap_amount_lamports * $current_price

// Calculate actual execution price
$execution_price = $output_amount / $swap_amount_lamports

// Calculate price impact percentage
$price_impact_pct = (($expected_output_no_slippage - $output_amount) / $expected_output_no_slippage) * 100

// Convert to UI amounts for display
$output_ui = $output_amount / LAMPORTS_PER_SOL
$expected_output_ui = $expected_output_no_slippage / LAMPORTS_PER_SOL

// Calculate absolute loss due to price impact
$impact_loss_ui = $expected_output_ui - $output_ui

**Decision Point:** Assess price impact severity
  BRANCH A ($price_impact_pct > 5):
    $severity = "very_high"
    $recommendation = "CAUTION: {$price_impact_pct}% impact. Consider splitting into smaller trades or using limit orders"
  BRANCH B ($price_impact_pct >= 2 AND $price_impact_pct <= 5):
    $severity = "high"
    $recommendation = "High impact - evaluate if acceptable for your trade"
  BRANCH C ($price_impact_pct >= 0.5 AND $price_impact_pct < 2):
    $severity = "moderate"
    $recommendation = "Moderate impact - typical for this trade size"
  BRANCH D ($price_impact_pct < 0.5):
    $severity = "low"
    $recommendation = "Low impact - good liquidity for this trade size"

**Action:**
RETURN {
  pool_address: $pool_address,
  swap_amount_in: $swap_amount,
  expected_output: $output_ui,
  expected_without_impact: $expected_output_ui,
  price_impact_pct: $price_impact_pct,
  impact_loss_tokens: $impact_loss_ui,
  execution_price: $execution_price,
  current_market_price: $current_price,
  fee_percentage: $fee_pct,
  severity: $severity,
  recommendation: $recommendation,
  confidence: 80
}

---

## Q76: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenLargestAccounts, getTokenAccountsByOwner
  - MAP, FILTER, COUNT, SUM, SORT (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account to find LP token mint
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool account not found")

// Parse LP token mint address (offset varies by AMM, using Raydium example)
$lp_mint = parsePublicKey(data: $pool_account.data, offset: 40)

// Get largest LP token holders
$largest_holders = getTokenLargestAccounts(mint: $lp_mint)

// Get total LP supply
$supply_info = getTokenSupply(mint: $lp_mint)
$total_supply = $supply_info.value.uiAmount

// Calculate share percentage for each holder
$lp_details = MAP(collection: $largest_holders.value, fn: holder => {
  owner: holder.address,
  lp_balance: holder.uiAmount,
  share_percentage: (holder.uiAmount / $total_supply) * 100,
  rank: holder.rank
})

// Filter out very small positions (< 0.01%)
$significant_lps = FILTER(collection: $lp_details, fn: lp => lp.share_percentage >= 0.01)

// Sort by share percentage descending
$sorted_lps = SORT(collection: $significant_lps, by: "share_percentage", order: "desc")

// Count total LPs
$total_lps = COUNT(collection: $sorted_lps)

// Calculate concentration (top 10 holders)
$top_10 = SLICE(array: $sorted_lps, start: 0, end: 10)
$top_10_concentration = SUM(collection: MAP($top_10, fn: lp => lp.share_percentage))

**Decision Point:** Assess liquidity concentration
  BRANCH A ($top_10_concentration > 80):
    $concentration_level = "highly_concentrated"
    $risk = "High centralization risk - top 10 LPs control {$top_10_concentration}%"
  BRANCH B ($top_10_concentration >= 50 AND $top_10_concentration <= 80):
    $concentration_level = "moderately_concentrated"
    $risk = "Moderate concentration"
  BRANCH C ($top_10_concentration < 50):
    $concentration_level = "well_distributed"
    $risk = "Low concentration risk"

**Action:**
RETURN {
  pool_address: $pool_address,
  lp_mint: $lp_mint,
  total_lp_supply: $total_supply,
  total_providers: $total_lps,
  top_providers: $top_10,
  top_10_concentration_pct: $top_10_concentration,
  concentration_level: $concentration_level,
  risk_assessment: $risk,
  confidence: 79
}

---

## Q77: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenSupply
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$farm_address = "FarmXYZ"

// Get farm account
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm account not found")

// Parse farm data (Raydium farm example)
// Offset 8: reward_per_second (u64)
// Offset 16: total_staked (u64)
// Offset 24: reward_token_mint (pubkey)
// Offset 56: staked_token_mint (pubkey)

$reward_per_second = parseU64(data: $farm_account.data, offset: 8)
$total_staked = parseU64(data: $farm_account.data, offset: 16)
$reward_token_mint = parsePublicKey(data: $farm_account.data, offset: 24)
$staked_token_mint = parsePublicKey(data: $farm_account.data, offset: 56)

// Get token prices (simplified - would use real price oracle in production)
$reward_price_usd = 1.5  // Example: reward token = $1.50
$staked_price_usd = 20.0  // Example: staked token = $20.00

// Calculate annual rewards
$seconds_per_year = 31536000
$annual_rewards = $reward_per_second * $seconds_per_year

// Convert to USD value
$annual_rewards_usd = ($annual_rewards / LAMPORTS_PER_SOL) * $reward_price_usd
$total_staked_usd = ($total_staked / LAMPORTS_PER_SOL) * $staked_price_usd

// Calculate APY percentage
$apy_pct = ($annual_rewards_usd / $total_staked_usd) * 100

// Calculate daily rewards per 1000 tokens staked
$daily_per_1000 = ($apy_pct / 365) * 10

**Decision Point:** Assess APY attractiveness
  BRANCH A ($apy_pct > 100):
    $rating = "very_high"
    $risk_note = "Extremely high APY may indicate unsustainable emissions or high risk"
  BRANCH B ($apy_pct >= 30 AND $apy_pct <= 100):
    $rating = "attractive"
    $risk_note = "Competitive APY for DeFi farming"
  BRANCH C ($apy_pct >= 10 AND $apy_pct < 30):
    $rating = "moderate"
    $risk_note = "Moderate returns"
  BRANCH D ($apy_pct < 10):
    $rating = "low"
    $risk_note = "Low APY - may not compensate for impermanent loss risk"

**Action:**
RETURN {
  farm_address: $farm_address,
  apy_percentage: $apy_pct,
  annual_rewards_usd: $annual_rewards_usd,
  total_staked_usd: $total_staked_usd,
  daily_per_1000_tokens: $daily_per_1000,
  reward_token_mint: $reward_token_mint,
  staked_token_mint: $staked_token_mint,
  rating: $rating,
  risk_note: $risk_note,
  confidence: 78
}

---

## Q78: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, findProgramAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$farm_address = "FarmXYZ"

// Find user's stake account PDA (Program Derived Address)
// Seeds typically: ["user_stake", farm_address, user_address]
$stake_pda = findProgramAddress(
  seeds: ["user_stake", $farm_address, $user_address],
  programId: RAYDIUM_STAKING_PROGRAM
)

// Get user's stake account
$stake_account = getAccountInfo(pubkey: $stake_pda)

GUARD $stake_account != null ELSE
  RETURN ERROR(message: "User has no active stake in this farm")

// Get farm account for reward calculation
$farm_account = getAccountInfo(pubkey: $farm_address)

// Parse user stake data
// Offset 8: staked_amount (u64)
// Offset 16: reward_debt (u64) - used for reward calculation
// Offset 24: last_update_time (i64)
// Offset 32: pending_rewards (u64)

$staked_amount = parseU64(data: $stake_account.data, offset: 8)
$reward_debt = parseU64(data: $stake_account.data, offset: 16)
$last_update_time = parseI64(data: $stake_account.data, offset: 24)
$pending_rewards = parseU64(data: $stake_account.data, offset: 32)

// Parse farm data for current reward calculations
$acc_reward_per_share = parseU128(data: $farm_account.data, offset: 88)
$reward_per_second = parseU64(data: $farm_account.data, offset: 8)

// Calculate newly accrued rewards since last update
$current_time = getCurrentTimestamp()
$time_elapsed = $current_time - $last_update_time
$new_rewards = ($staked_amount * $acc_reward_per_share / 1e12) - $reward_debt

// Total pending rewards
$total_pending = $pending_rewards + $new_rewards

// Convert to UI amount
$pending_ui_amount = $total_pending / LAMPORTS_PER_SOL

// Estimate USD value (example price)
$reward_token_price = 1.5
$pending_usd = $pending_ui_amount * $reward_token_price

// Calculate estimated daily earnings
$daily_rate = ($reward_per_second * 86400 * $staked_amount) / parseU64(data: $farm_account.data, offset: 16)
$daily_earnings_ui = $daily_rate / LAMPORTS_PER_SOL
$daily_earnings_usd = $daily_earnings_ui * $reward_token_price

**Decision Point:** Assess reward claim urgency
  BRANCH A ($pending_ui_amount > 100):
    $recommendation = "Large pending rewards - consider claiming to avoid loss"
    $urgency = "high"
  BRANCH B ($pending_ui_amount >= 10 AND $pending_ui_amount <= 100):
    $recommendation = "Moderate rewards - claim when transaction fees are low"
    $urgency = "medium"
  BRANCH C ($pending_ui_amount < 10):
    $recommendation = "Small rewards - wait to accumulate more before claiming"
    $urgency = "low"

**Action:**
RETURN {
  user_address: $user_address,
  farm_address: $farm_address,
  staked_amount_ui: $staked_amount / LAMPORTS_PER_SOL,
  pending_rewards_ui: $pending_ui_amount,
  pending_rewards_usd: $pending_usd,
  daily_earnings_ui: $daily_earnings_ui,
  daily_earnings_usd: $daily_earnings_usd,
  time_since_last_update_seconds: $time_elapsed,
  recommendation: $recommendation,
  urgency: $urgency,
  confidence: 77
}

---

## Q79: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get lending pool reserve account (Solend/Kamino example)
$reserve_account = getAccountInfo(pubkey: $pool_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Lending pool reserve not found")

// Parse reserve account data
// Offset 8: version (u8)
// Offset 16: available_liquidity (u64)
// Offset 24: borrowed_amount_wads (u128)
// Offset 40: cumulative_borrow_rate_wads (u128)
// Offset 56: market_price (u128)

$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)

// Convert borrowed amount from wads (1e18 scale) to normal amount
$borrowed_amount = $borrowed_amount_wads / 1e18

// Calculate total supply
$total_supply = $available_liquidity + $borrowed_amount

// Calculate utilization rate as percentage
$utilization_rate = ($borrowed_amount / $total_supply) * 100

// Convert to UI amounts for display
$available_ui = $available_liquidity / LAMPORTS_PER_SOL
$borrowed_ui = $borrowed_amount / LAMPORTS_PER_SOL
$total_supply_ui = $total_supply / LAMPORTS_PER_SOL

**Decision Point:** Assess pool health and risk
  BRANCH A ($utilization_rate > 90):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Critically high utilization - may limit withdrawals, increased liquidation risk"
  BRANCH B ($utilization_rate >= 80 AND $utilization_rate <= 90):
    $health_status = "stressed"
    $risk_level = "elevated"
    $note = "High utilization - monitor for liquidity crunches"
  BRANCH C ($utilization_rate >= 50 AND $utilization_rate < 80):
    $health_status = "healthy"
    $risk_level = "moderate"
    $note = "Optimal utilization range for lending pool"
  BRANCH D ($utilization_rate < 50):
    $health_status = "underutilized"
    $risk_level = "low"
    $note = "Low utilization - excess idle capital, lower APY for lenders"

**Action:**
RETURN {
  pool_address: $pool_address,
  utilization_rate_pct: $utilization_rate,
  available_liquidity_ui: $available_ui,
  borrowed_amount_ui: $borrowed_ui,
  total_supply_ui: $total_supply_ui,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 76
}

---

## Q80: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$asset_address = "AssetXYZ"

// Get lending reserve account for the asset
$reserve_account = getAccountInfo(pubkey: $asset_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Reserve account not found")

// Parse reserve data for borrow rate
// Offset 56: current_borrow_rate (u64) - in basis points per year
// Offset 64: optimal_borrow_rate (u64)
// Offset 72: max_borrow_rate (u64)
// Offset 80: borrow_rate_curve (struct with points)

$current_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 56)
$optimal_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 64)
$max_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 72)

// Convert basis points to percentage APY
$current_borrow_apy = $current_borrow_rate_bps / 100
$optimal_borrow_apy = $optimal_borrow_rate_bps / 100
$max_borrow_apy = $max_borrow_rate_bps / 100

// Get utilization rate for context
$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)
$borrowed_amount = $borrowed_amount_wads / 1e18
$total_supply = $available_liquidity + $borrowed_amount
$utilization_rate = ($borrowed_amount / $total_supply) * 100

**Decision Point:** Assess borrowing attractiveness
  BRANCH A ($current_borrow_apy < 5):
    $attractiveness = "very_attractive"
    $note = "Very low borrowing costs - ideal for leverage"
  BRANCH B ($current_borrow_apy >= 5 AND $current_borrow_apy < 15):
    $attractiveness = "attractive"
    $note = "Reasonable borrowing costs for most strategies"
  BRANCH C ($current_borrow_apy >= 15 AND $current_borrow_apy < 30):
    $attractiveness = "expensive"
    $note = "High borrowing costs - consider alternatives"
  BRANCH D ($current_borrow_apy >= 30):
    $attractiveness = "very_expensive"
    $note = "Extremely high costs - borrowing not recommended"

**Action:**
RETURN {
  asset_address: $asset_address,
  current_borrow_apy_pct: $current_borrow_apy,
  optimal_borrow_apy_pct: $optimal_borrow_apy,
  max_borrow_apy_pct: $max_borrow_apy,
  utilization_rate_pct: $utilization_rate,
  attractiveness: $attractiveness,
  note: $note,
  confidence: 95
}

---

## Q81: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"

// Get user's lending account (obligation account in Solend/Kamino)
$user_obligation = getAccountInfo(pubkey: $user_address)

GUARD $user_obligation != null ELSE
  RETURN ERROR(message: "User obligation account not found")

// Parse obligation data
// Offset 8: version (u8)
// Offset 16: owner (pubkey)
// Offset 48: deposited_value (u128) - in lamports
// Offset 64: borrowed_value (u128) - in lamports
// Offset 80: allowed_borrow_value (u128)
// Offset 96: unhealthy_borrow_value (u128)
// Offset 112: deposits (array of deposit structs)
// Offset 128: borrows (array of borrow structs)

$deposited_value_lamports = parseU128(data: $user_obligation.data, offset: 48)
$borrowed_value_lamports = parseU128(data: $user_obligation.data, offset: 64)
$allowed_borrow_value = parseU128(data: $user_obligation.data, offset: 80)
$unhealthy_borrow_value = parseU128(data: $user_obligation.data, offset: 96)

// Convert to SOL for display
$deposited_value_sol = $deposited_value_lamports / LAMPORTS_PER_SOL
$borrowed_value_sol = $borrowed_value_lamports / LAMPORTS_PER_SOL

// Calculate collateral ratio
GUARD $borrowed_value_sol > 0 ELSE
  RETURN {
    user_address: $user_address,
    collateral_ratio: "infinite",
    deposited_value_sol: $deposited_value_sol,
    borrowed_value_sol: $borrowed_value_sol,
    liquidation_threshold: $unhealthy_borrow_value / LAMPORTS_PER_SOL,
    health_status: "healthy",
    note: "No outstanding debt - infinite collateral ratio",
    confidence: 94
  }

$collateral_ratio = $deposited_value_sol / $borrowed_value_sol
$liquidation_threshold = $unhealthy_borrow_value / LAMPORTS_PER_SOL

**Decision Point:** Assess position health
  BRANCH A ($collateral_ratio < 1.1):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Position at risk of liquidation - collateral ratio too low"
  BRANCH B ($collateral_ratio >= 1.1 AND $collateral_ratio < 1.5):
    $health_status = "warning"
    $risk_level = "medium"
    $note = "Collateral ratio approaching liquidation threshold"
  BRANCH C ($collateral_ratio >= 1.5):
    $health_status = "healthy"
    $risk_level = "low"
    $note = "Healthy collateral ratio with sufficient margin"

**Action:**
RETURN {
  user_address: $user_address,
  collateral_ratio: $collateral_ratio,
  deposited_value_sol: $deposited_value_sol,
  borrowed_value_sol: $borrowed_value_sol,
  liquidation_threshold_sol: $liquidation_threshold,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 94
}

---

## Q82: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$position_address = "PosXYZ"

// Get position account (obligation in lending protocol)
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position account not found")

// Parse position data for liquidation thresholds
// Offset 96: unhealthy_borrow_value (u128) - liquidation threshold
// Offset 80: allowed_borrow_value (u128) - max borrow before liquidation risk
// Offset 48: deposited_value (u128)
// Offset 64: borrowed_value (u128)

$unhealthy_borrow_value = parseU128(data: $position_account.data, offset: 96)
$allowed_borrow_value = parseU128(data: $position_account.data, offset: 80)
$deposited_value = parseU128(data: $position_account.data, offset: 48)
$borrowed_value = parseU128(data: $position_account.data, offset: 64)

// Convert to SOL
$liquidation_threshold_sol = $unhealthy_borrow_value / LAMPORTS_PER_SOL
$max_safe_borrow_sol = $allowed_borrow_value / LAMPORTS_PER_SOL
$deposited_value_sol = $deposited_value / LAMPORTS_PER_SOL
$borrowed_value_sol = $borrowed_value / LAMPORTS_PER_SOL

// Calculate current collateral ratio
$current_ratio = $borrowed_value_sol > 0 ? $deposited_value_sol / $borrowed_value_sol : "infinite"

// Calculate liquidation ratio (threshold / deposited)
$liquidation_ratio = $deposited_value_sol > 0 ? $liquidation_threshold_sol / $deposited_value_sol : 0

// Calculate distance to liquidation
$distance_to_liquidation = $liquidation_threshold_sol - $borrowed_value_sol

**Decision Point:** Assess liquidation risk
  BRANCH A ($borrowed_value_sol >= $liquidation_threshold_sol):
    $liquidation_status = "liquidatable"
    $risk_level = "immediate"
    $note = "Position is currently eligible for liquidation"
  BRANCH B ($borrowed_value_sol >= $max_safe_borrow_sol AND $borrowed_value_sol < $liquidation_threshold_sol):
    $liquidation_status = "at_risk"
    $risk_level = "high"
    $note = "Position approaching liquidation threshold"
  BRANCH C ($borrowed_value_sol < $max_safe_borrow_sol):
    $liquidation_status = "safe"
    $risk_level = "low"
    $note = "Position well below liquidation threshold"

**Action:**
RETURN {
  position_address: $position_address,
  liquidation_threshold_sol: $liquidation_threshold_sol,
  max_safe_borrow_sol: $max_safe_borrow_sol,
  current_borrowed_sol: $borrowed_value_sol,
  deposited_value_sol: $deposited_value_sol,
  current_collateral_ratio: $current_ratio,
  liquidation_ratio: $liquidation_ratio,
  distance_to_liquidation_sol: $distance_to_liquidation,
  liquidation_status: $liquidation_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 93
}

---

## Q83: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get AMM pool account (Raydium/Orca format)
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool account not found")

// Parse AMM pool data
// Offset 8: nonce (u8)
// Offset 16: token_a_mint (pubkey)
// Offset 48: token_b_mint (pubkey)
// Offset 80: token_a_account (pubkey)
// Offset 112: token_b_account (pubkey)
// Offset 144: mint_lp (pubkey)
// Offset 176: oracle (pubkey)
// Offset 208: serum_market (pubkey)
// Offset 240: serum_program_id (pubkey)
// Offset 272: signers (array)
// Offset 304: token_a_amount (u64)
// Offset 312: token_b_amount (u64)
// Offset 320: sqrt_price (u128)
// Offset 336: current_tick_index (i32)
// Offset 340: tick_spacing (u16)
// Offset 342: fee_growth_global_a (u128)
// Offset 358: fee_growth_global_b (u128)
// Offset 374: protocol_fees_token_a (u64)
// Offset 382: protocol_fees_token_b (u64)

$token_a_amount = parseU64(data: $pool_account.data, offset: 304)
$token_b_amount = parseU64(data: $pool_account.data, offset: 312)

// Get token mints for price calculation
$token_a_mint = parsePubkey(data: $pool_account.data, offset: 16)
$token_b_mint = parsePubkey(data: $pool_account.data, offset: 48)

// Get token accounts to verify amounts
$token_a_account = getAccountInfo(pubkey: parsePubkey(data: $pool_account.data, offset: 80))
$token_b_account = getAccountInfo(pubkey: parsePubkey(data: $pool_account.data, offset: 112))

// Get actual balances from token accounts
$token_a_balance = parseU64(data: $token_a_account.data, offset: 64)
$token_b_balance = parseU64(data: $token_b_account.data, offset: 64)

// Get token prices (simplified - would use oracle/pyth)
$token_a_price_usd = $token_a_mint == "So11111111111111111111111111111111111111112" ? 150 : 1  // SOL price
$token_b_price_usd = $token_b_mint == "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v" ? 1 : 1  // USDC price

// Calculate TVL in USD
$tvl_token_a = ($token_a_balance / 10^9) * $token_a_price_usd  // Assuming 9 decimals
$tvl_token_b = ($token_b_balance / 10^6) * $token_b_price_usd   // Assuming 6 decimals for USDC
$total_tvl_usd = $tvl_token_a + $tvl_token_b

// Calculate pool ratio
$pool_ratio = $token_a_balance > 0 ? $token_b_balance / $token_a_balance : 0

**Decision Point:** Categorize pool size
  BRANCH A ($total_tvl_usd >= 10000000):  // $10M+
    $pool_category = "large"
    $liquidity_depth = "deep"
    $note = "Major pool with excellent liquidity"
  BRANCH B ($total_tvl_usd >= 1000000 AND $total_tvl_usd < 10000000):  // $1M-$10M
    $pool_category = "medium"
    $liquidity_depth = "moderate"
    $note = "Established pool with good liquidity"
  BRANCH C ($total_tvl_usd >= 100000 AND $total_tvl_usd < 1000000):  // $100K-$1M
    $pool_category = "small"
    $liquidity_depth = "shallow"
    $note = "Small pool - watch for slippage"
  BRANCH D ($total_tvl_usd < 100000):  // <$100K
    $pool_category = "micro"
    $liquidity_depth = "very_shallow"
    $note = "Micro pool - high slippage risk"

**Action:**
RETURN {
  pool_address: $pool_address,
  token_a_mint: $token_a_mint,
  token_b_mint: $token_b_mint,
  token_a_balance: $token_a_balance,
  token_b_balance: $token_b_balance,
  token_a_tvl_usd: $tvl_token_a,
  token_b_tvl_usd: $tvl_token_b,
  total_tvl_usd: $total_tvl_usd,
  pool_ratio: $pool_ratio,
  pool_category: $pool_category,
  liquidity_depth: $liquidity_depth,
  note: $note,
  confidence: 92
}

---

## Q84: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get recent transactions for the pool (last 1000 for analysis)
$signatures = getSignaturesForAddress(address: $pool_address, limit: 1000)

GUARD $signatures != null AND COUNT($signatures) > 0 ELSE
  RETURN ERROR(message: "No transaction history found for pool")

// Get current timestamp and calculate 24h ago
$current_time = getCurrentTimestamp()
$one_day_ago = $current_time - 86400  // 24 hours in seconds

// Filter transactions from last 24 hours
$recent_txs = FILTER($signatures, tx => tx.blockTime >= $one_day_ago)

// Get detailed transaction data for volume calculation
$tx_details = MAP($recent_txs, sig => getTransaction(signature: sig.signature))

// Filter out failed transactions
$successful_txs = FILTER($tx_details, tx => tx != null AND tx.meta.err == null)

// Extract swap amounts from transaction logs/metadata
$swap_volumes = MAP($successful_txs, tx => {
  // Parse transaction for token transfers
  // Look for token balance changes in pre/post balances
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  // Calculate net token movements (simplified - real implementation would parse logs)
  $token_a_change = 0
  $token_b_change = 0

  // This is a simplified calculation - real implementation would:
  // 1. Identify swap instructions in transaction
  // 2. Parse the amountIn and amountOut from instruction data
  // 3. Sum up all swap amounts

  // For demo purposes, we'll use a placeholder calculation
  RETURN {
    token_a_volume: ABS($token_a_change),
    token_b_volume: ABS($token_b_change),
    usd_value: 0  // Would calculate based on prices
  }
})

// Sum up total volumes
$total_token_a_volume = SUM(MAP($swap_volumes, v => v.token_a_volume))
$total_token_b_volume = SUM(MAP($swap_volumes, v => v.token_b_volume))

// Get current prices for USD conversion
$token_a_price = 150  // SOL price
$token_b_price = 1    // USDC price

// Calculate USD volumes
$volume_token_a_usd = ($total_token_a_volume / 10^9) * $token_a_price
$volume_token_b_usd = ($total_token_b_volume / 10^6) * $token_b_price
$total_volume_usd = $volume_token_a_usd + $volume_token_b_usd

// Calculate volume per transaction
$avg_volume_per_tx = COUNT($successful_txs) > 0 ? $total_volume_usd / COUNT($successful_txs) : 0

**Decision Point:** Assess trading activity
  BRANCH A ($total_volume_usd >= 10000000):  // $10M+
    $activity_level = "very_high"
    $note = "Exceptionally high volume - major trading activity"
  BRANCH B ($total_volume_usd >= 1000000 AND $total_volume_usd < 10000000):  // $1M-$10M
    $activity_level = "high"
    $note = "Strong trading volume"
  BRANCH C ($total_volume_usd >= 100000 AND $total_volume_usd < 1000000):  // $100K-$1M
    $activity_level = "moderate"
    $note = "Moderate trading activity"
  BRANCH D ($total_volume_usd < 100000):  // <$100K
    $activity_level = "low"
    $note = "Low trading volume"

**Action:**
RETURN {
  pool_address: $pool_address,
  time_period_hours: 24,
  total_volume_usd: $total_volume_usd,
  token_a_volume: $total_token_a_volume,
  token_b_volume: $total_token_b_volume,
  volume_token_a_usd: $volume_token_a_usd,
  volume_token_b_usd: $volume_token_b_usd,
  transaction_count: COUNT($successful_txs),
  avg_volume_per_tx_usd: $avg_volume_per_tx,
  activity_level: $activity_level,
  note: $note,
  confidence: 91
}

---

## Q85: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$swap_amount = 1000  // Amount to swap (in token units)
$pool_address = "PoolXYZ"  // Assume SOL/USDC pool for this example

// Get pool state
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse current pool reserves
$token_a_reserve = parseU64(data: $pool_account.data, offset: 304)  // token A amount
$token_b_reserve = parseU64(data: $pool_account.data, offset: 312)  // token B amount

// Assume we're swapping token A for token B
// Current price = token_b_reserve / token_a_reserve
$current_price = $token_b_reserve / $token_a_reserve

// Calculate output amount using AMM formula: (x * y) = k
// For input amount dx, output dy = (y * dx) / (x + dx)
$input_amount = $swap_amount * 10^9  // Convert to smallest unit (assuming 9 decimals)
$output_amount = ($token_b_reserve * $input_amount) / ($token_a_reserve + $input_amount)

// Calculate effective price after swap
$effective_price = $input_amount > 0 ? $output_amount / $input_amount : 0

// Calculate price impact as percentage
$price_impact_pct = (($effective_price - $current_price) / $current_price) * 100

// Calculate slippage (absolute price difference)
$slippage = $effective_price - $current_price

// Calculate output amount in readable units
$output_amount_readable = $output_amount / 10^6  // Assuming 6 decimals for output token

// Minimum output after slippage
$min_output_after_slippage = $output_amount * (1 - ABS($price_impact_pct) / 100)

// Simulate transaction to verify (optional - for validation)
$simulated_tx = simulateTransaction({
  instructions: [
    // Would include actual swap instruction here
    // This is simplified for the example
  ]
})

**Decision Point:** Assess price impact severity
  BRANCH A (ABS($price_impact_pct) >= 5):
    $impact_severity = "severe"
    $recommendation = "avoid_large_swaps"
    $note = "High price impact - consider splitting into smaller trades"
  BRANCH B (ABS($price_impact_pct) >= 2 AND ABS($price_impact_pct) < 5):
    $impact_severity = "moderate"
    $recommendation = "monitor_closely"
    $note = "Moderate slippage - acceptable for most trades"
  BRANCH C (ABS($price_impact_pct) >= 0.5 AND ABS($price_impact_pct) < 2):
    $impact_severity = "low"
    $recommendation = "proceed_normally"
    $note = "Low price impact - good trading conditions"
  BRANCH D (ABS($price_impact_pct) < 0.5):
    $impact_severity = "negligible"
    $recommendation = "optimal_conditions"
    $note = "Minimal slippage - excellent liquidity"

**Action:**
RETURN {
  swap_amount_input: $swap_amount,
  pool_address: $pool_address,
  current_price_ratio: $current_price,
  effective_price_after_swap: $effective_price,
  price_impact_percentage: $price_impact_pct,
  slippage_amount: $slippage,
  expected_output_amount: $output_amount_readable,
  min_output_after_slippage: $min_output_after_slippage / 10^6,
  impact_severity: $impact_severity,
  recommendation: $recommendation,
  note: $note,
  confidence: 90
}

---

## Q86: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account to find LP token mint
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Extract LP token mint from pool data
$lp_token_mint = parsePubkey(data: $pool_account.data, offset: 144)

// Get all token accounts for this LP token mint
$lp_token_accounts = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [
  { dataSize: 165 },  // Token account size
  { memcmp: { offset: 0, bytes: $lp_token_mint } }  // Mint address filter
])

GUARD $lp_token_accounts != null AND COUNT($lp_token_accounts) > 0 ELSE
  RETURN ERROR(message: "No LP token accounts found")

// Filter out accounts with zero balance and sort by balance
$active_providers = FILTER($lp_token_accounts, account => {
  $balance = parseU64(data: account.account.data, offset: 64)
  RETURN $balance > 0
})

// Extract provider info and balances
$providers_info = MAP($active_providers, account => {
  $owner = parsePubkey(data: account.account.data, offset: 32)
  $balance = parseU64(data: account.account.data, offset: 64)

  RETURN {
    owner_address: $owner,
    lp_token_balance: $balance,
    balance_readable: $balance / 10^9  // Assuming 9 decimals for LP tokens
  }
})

// Sort by balance descending
$sorted_providers = SORT($providers_info, (a, b) => b.lp_token_balance - a.lp_token_balance)

// Calculate concentration metrics
$total_lp_tokens = SUM(MAP($sorted_providers, p => p.lp_token_balance))
$top_10_percentage = COUNT($sorted_providers) >= 10 ?
  (SUM(MAP(SLICE($sorted_providers, 0, 10), p => p.lp_token_balance)) / $total_lp_tokens) * 100 : 100

$largest_provider_percentage = COUNT($sorted_providers) > 0 ?
  ($sorted_providers[0].lp_token_balance / $total_lp_tokens) * 100 : 0

**Decision Point:** Assess provider concentration
  BRANCH A ($largest_provider_percentage >= 50):
    $concentration_level = "highly_concentrated"
    $risk_level = "high"
    $note = "Single provider dominates - high impermanent loss risk"
  BRANCH B ($top_10_percentage >= 80):
    $concentration_level = "moderately_concentrated"
    $risk_level = "medium"
    $note = "Top 10 providers hold most liquidity"
  BRANCH C ($top_10_percentage >= 50 AND $top_10_percentage < 80):
    $concentration_level = "decentralized"
    $risk_level = "low"
    $note = "Well distributed among providers"
  BRANCH D ($top_10_percentage < 50):
    $concentration_level = "highly_decentralized"
    $risk_level = "very_low"
    $note = "Excellent distribution - very resilient"

**Action:**
RETURN {
  pool_address: $pool_address,
  lp_token_mint: $lp_token_mint,
  total_providers: COUNT($sorted_providers),
  total_lp_tokens: $total_lp_tokens,
  top_providers: SLICE($sorted_providers, 0, 20),  // Top 20 providers
  largest_provider_percentage: $largest_provider_percentage,
  top_10_percentage: $top_10_percentage,
  concentration_level: $concentration_level,
  risk_level: $risk_level,
  note: $note,
  confidence: 89
}

---

## Q87: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$farm_address = "FarmXYZ"

// Get farm account (e.g., Raydium/Saber farm)
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm not found")

// Parse farm data for reward rates
// Offset 8: version (u8)
// Offset 16: state (u8) - 1 = enabled, 2 = disabled
// Offset 24: nonce (u8)
// Offset 32: owner (pubkey)
// Offset 64: token_a_mint (pubkey)
// Offset 96: token_b_mint (pubkey)
// Offset 128: lp_token_mint (pubkey)
// Offset 160: reward_a_mint (pubkey)
// Offset 192: reward_b_mint (pubkey)
// Offset 224: reward_a_per_slot (u64)
// Offset 232: reward_b_per_slot (u64)
// Offset 240: total_staked_lp (u64)
// Offset 248: last_update_slot (u64)

$reward_a_per_slot = parseU64(data: $farm_account.data, offset: 224)
$reward_b_per_slot = parseU64(data: $farm_account.data, offset: 232)
$total_staked_lp = parseU64(data: $farm_account.data, offset: 240)

// Get reward token prices (simplified)
$reward_a_price_usd = 150  // Assume SOL rewards
$reward_b_price_usd = 1    // Assume USDC rewards

// Calculate annual rewards
$slots_per_year = 365 * 24 * 60 * 2  // ~2 slots per second
$annual_reward_a_usd = ($reward_a_per_slot * $slots_per_year / 10^9) * $reward_a_price_usd
$annual_reward_b_usd = ($reward_b_per_slot * $slots_per_year / 10^6) * $reward_b_price_usd
$total_annual_rewards_usd = $annual_reward_a_usd + $annual_reward_b_usd

// Get staked value (LP token value)
$staked_value_usd = ($total_staked_lp / 10^9) * 100  // Simplified - would calculate actual LP value

// Calculate APY
GUARD $staked_value_usd > 0 ELSE
  RETURN {
    farm_address: $farm_address,
    apy_percentage: 0,
    total_staked_usd: 0,
    annual_rewards_usd: $total_annual_rewards_usd,
    note: "No tokens staked in farm",
    confidence: 88
  }

$apy_percentage = ($total_annual_rewards_usd / $staked_value_usd) * 100

// Calculate individual token APYs
$apy_a_percentage = $annual_reward_a_usd > 0 ? ($annual_reward_a_usd / $staked_value_usd) * 100 : 0
$apy_b_percentage = $annual_reward_b_usd > 0 ? ($annual_reward_b_usd / $staked_value_usd) * 100 : 0

**Decision Point:** Assess farm attractiveness
  BRANCH A ($apy_percentage >= 50):
    $attractiveness = "excellent"
    $risk_level = "high"
    $note = "Very high APY - likely unsustainable, monitor closely"
  BRANCH B ($apy_percentage >= 20 AND $apy_percentage < 50):
    $attractiveness = "very_good"
    $risk_level = "medium"
    $note = "Strong yields - good farming opportunity"
  BRANCH C ($apy_percentage >= 5 AND $apy_percentage < 20):
    $attractiveness = "moderate"
    $risk_level = "low"
    $note = "Decent returns for conservative farming"
  BRANCH D ($apy_percentage < 5):
    $attractiveness = "poor"
    $risk_level = "very_low"
    $note = "Low yields - consider alternative farms"

**Action:**
RETURN {
  farm_address: $farm_address,
  total_apy_percentage: $apy_percentage,
  reward_a_apy_percentage: $apy_a_percentage,
  reward_b_apy_percentage: $apy_b_percentage,
  total_staked_lp_tokens: $total_staked_lp,
  staked_value_usd: $staked_value_usd,
  annual_rewards_usd: $total_annual_rewards_usd,
  reward_a_annual_usd: $annual_reward_a_usd,
  reward_b_annual_usd: $annual_reward_b_usd,
  attractiveness: $attractiveness,
  risk_level: $risk_level,
  note: $note,
  confidence: 88
}

---

## Q88: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$farm_address = "FarmXYZ"

// Get user's farm stake account
$user_stake_account = getAccountInfo(pubkey: $user_address)

GUARD $user_stake_account != null ELSE
  RETURN ERROR(message: "User stake account not found")

// Get farm account for reward rates
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm account not found")

// Parse user stake data
// Offset 8: state (u8)
// Offset 16: owner (pubkey)
// Offset 48: stake_time (i64)
// Offset 56: stake_amount (u64)
// Offset 64: reward_a_pending (u64)
// Offset 72: reward_b_pending (u64)
// Offset 80: reward_a_per_token_paid (u128)
// Offset 96: reward_b_per_token_paid (u128)

$user_stake_amount = parseU64(data: $user_stake_account.data, offset: 56)
$reward_a_pending = parseU64(data: $user_stake_account.data, offset: 64)
$reward_b_pending = parseU64(data: $user_stake_account.data, offset: 72)

// Parse farm reward rates
$reward_a_per_slot = parseU64(data: $farm_account.data, offset: 224)
$reward_b_per_slot = parseU64(data: $farm_account.data, offset: 232)
$total_staked = parseU64(data: $farm_account.data, offset: 240)
$last_update_slot = parseU64(data: $farm_account.data, offset: 248)

// Calculate additional rewards earned since last update
$current_slot = getCurrentSlot()
$slots_elapsed = $current_slot - $last_update_slot

// Additional rewards = (user_stake / total_staked) * reward_rate * slots_elapsed
$additional_reward_a = 0
$additional_reward_b = 0

GUARD $total_staked > 0 ELSE
  $additional_reward_a = 0
  $additional_reward_b = 0
BRANCH ELSE
  $stake_ratio = $user_stake_amount / $total_staked
  $additional_reward_a = ($stake_ratio * $reward_a_per_slot * $slots_elapsed)
  $additional_reward_b = ($stake_ratio * $reward_b_per_slot * $slots_elapsed)

// Total pending rewards
$total_reward_a = $reward_a_pending + $additional_reward_a
$total_reward_b = $reward_b_pending + $additional_reward_b

// Convert to readable amounts
$reward_a_readable = $total_reward_a / 10^9  // Assuming 9 decimals
$reward_b_readable = $total_reward_b / 10^6  // Assuming 6 decimals

// Get reward token prices
$reward_a_price_usd = 150  // SOL
$reward_b_price_usd = 1    // USDC

// Calculate USD values
$reward_a_usd = $reward_a_readable * $reward_a_price_usd
$reward_b_usd = $reward_b_readable * $reward_b_price_usd
$total_rewards_usd = $reward_a_usd + $reward_b_usd

// Calculate user's share of farm
$user_share_percentage = $total_staked > 0 ? ($user_stake_amount / $total_staked) * 100 : 0

**Decision Point:** Assess reward significance
  BRANCH A ($total_rewards_usd >= 1000):
    $reward_size = "large"
    $harvest_recommendation = "harvest_now"
    $note = "Significant rewards accumulated - consider harvesting"
  BRANCH B ($total_rewards_usd >= 100 AND $total_rewards_usd < 1000):
    $reward_size = "moderate"
    $harvest_recommendation = "harvest_soon"
    $note = "Notable rewards - harvest when convenient"
  BRANCH C ($total_rewards_usd >= 10 AND $total_rewards_usd < 100):
    $reward_size = "small"
    $harvest_recommendation = "accumulate_further"
    $note = "Small rewards - let them accumulate"
  BRANCH D ($total_rewards_usd < 10):
    $reward_size = "minimal"
    $harvest_recommendation = "wait_longer"
    $note = "Minimal rewards - wait for more accumulation"

**Action:**
RETURN {
  user_address: $user_address,
  farm_address: $farm_address,
  user_stake_amount: $user_stake_amount,
  user_share_percentage: $user_share_percentage,
  reward_a_pending: $reward_a_readable,
  reward_b_pending: $reward_b_readable,
  reward_a_usd_value: $reward_a_usd,
  reward_b_usd_value: $reward_b_usd,
  total_rewards_usd: $total_rewards_usd,
  slots_since_last_update: $slots_elapsed,
  reward_size: $reward_size,
  harvest_recommendation: $harvest_recommendation,
  note: $note,
  confidence: 87
}

---

## Q89: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get lending pool reserve account (Solend/Kamino example)
$reserve_account = getAccountInfo(pubkey: $pool_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Lending pool reserve not found")

// Parse reserve account data
// Offset 8: version (u8)
// Offset 16: available_liquidity (u64)
// Offset 24: borrowed_amount_wads (u128)
// Offset 40: cumulative_borrow_rate_wads (u128)
// Offset 56: market_price (u128)

$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)

// Convert borrowed amount from wads (1e18 scale) to normal amount
$borrowed_amount = $borrowed_amount_wads / 1e18

// Calculate total supply
$total_supply = $available_liquidity + $borrowed_amount

// Calculate utilization rate as percentage
$utilization_rate = ($borrowed_amount / $total_supply) * 100

// Convert to UI amounts for display
$available_ui = $available_liquidity / LAMPORTS_PER_SOL
$borrowed_ui = $borrowed_amount / LAMPORTS_PER_SOL
$total_supply_ui = $total_supply / LAMPORTS_PER_SOL

**Decision Point:** Assess pool health and risk
  BRANCH A ($utilization_rate > 90):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Critically high utilization - may limit withdrawals, increased liquidation risk"
  BRANCH B ($utilization_rate >= 80 AND $utilization_rate <= 90):
    $health_status = "stressed"
    $risk_level = "elevated"
    $note = "High utilization - monitor for liquidity crunches"
  BRANCH C ($utilization_rate >= 50 AND $utilization_rate < 80):
    $health_status = "healthy"
    $risk_level = "moderate"
    $note = "Optimal utilization range for lending pool"
  BRANCH D ($utilization_rate < 50):
    $health_status = "underutilized"
    $risk_level = "low"
    $note = "Low utilization - excess idle capital, lower APY for lenders"

**Action:**
RETURN {
  pool_address: $pool_address,
  utilization_rate_pct: $utilization_rate,
  available_liquidity_ui: $available_ui,
  borrowed_amount_ui: $borrowed_ui,
  total_supply_ui: $total_supply_ui,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 86
}

---

## Q90: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$asset_address = "AssetXYZ"

// Get lending reserve account for the asset
$reserve_account = getAccountInfo(pubkey: $asset_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Reserve account not found")

// Parse reserve data for borrow rate
// Offset 56: current_borrow_rate (u64) - in basis points per year
// Offset 64: optimal_borrow_rate (u64)
// Offset 72: max_borrow_rate (u64)
// Offset 80: borrow_rate_curve (struct with points)

$current_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 56)
$optimal_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 64)
$max_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 72)

// Convert basis points to percentage APY
$current_borrow_apy = $current_borrow_rate_bps / 100
$optimal_borrow_apy = $optimal_borrow_rate_bps / 100
$max_borrow_apy = $max_borrow_rate_bps / 100

// Get utilization rate for context
$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)
$borrowed_amount = $borrowed_amount_wads / 1e18
$total_supply = $available_liquidity + $borrowed_amount
$utilization_rate = ($borrowed_amount / $total_supply) * 100

**Decision Point:** Assess borrowing attractiveness
  BRANCH A ($current_borrow_apy < 5):
    $attractiveness = "very_attractive"
    $note = "Very low borrowing costs - ideal for leverage"
  BRANCH B ($current_borrow_apy >= 5 AND $current_borrow_apy < 15):
    $attractiveness = "attractive"
    $note = "Reasonable borrowing costs for most strategies"
  BRANCH C ($current_borrow_apy >= 15 AND $current_borrow_apy < 30):
    $attractiveness = "expensive"
    $note = "High borrowing costs - consider alternatives"
  BRANCH D ($current_borrow_apy >= 30):
    $attractiveness = "very_expensive"
    $note = "Extremely high costs - borrowing not recommended"

**Action:**
RETURN {
  asset_address: $asset_address,
  current_borrow_apy_pct: $current_borrow_apy,
  optimal_borrow_apy_pct: $optimal_borrow_apy,
  max_borrow_apy_pct: $max_borrow_apy,
  utilization_rate_pct: $utilization_rate,
  attractiveness: $attractiveness,
  note: $note,
  confidence: 85
}

---

## Q91: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"

// Get user's lending account (obligation account in Solend/Kamino)
$user_obligation = getAccountInfo(pubkey: $user_address)

GUARD $user_obligation != null ELSE
  RETURN ERROR(message: "User obligation account not found")

// Parse obligation data
// Offset 8: version (u8)
// Offset 16: owner (pubkey)
// Offset 48: deposited_value (u128) - in lamports
// Offset 64: borrowed_value (u128) - in lamports
// Offset 80: allowed_borrow_value (u128)
// Offset 96: unhealthy_borrow_value (u128)
// Offset 112: deposits (array of deposit structs)
// Offset 128: borrows (array of borrow structs)

$deposited_value_lamports = parseU128(data: $user_obligation.data, offset: 48)
$borrowed_value_lamports = parseU128(data: $user_obligation.data, offset: 64)
$allowed_borrow_value = parseU128(data: $user_obligation.data, offset: 80)
$unhealthy_borrow_value = parseU128(data: $user_obligation.data, offset: 96)

// Convert to SOL for display
$deposited_value_sol = $deposited_value_lamports / LAMPORTS_PER_SOL
$borrowed_value_sol = $borrowed_value_lamports / LAMPORTS_PER_SOL

// Calculate collateral ratio
GUARD $borrowed_value_sol > 0 ELSE
  RETURN {
    user_address: $user_address,
    collateral_ratio: "infinite",
    deposited_value_sol: $deposited_value_sol,
    borrowed_value_sol: $borrowed_value_sol,
    liquidation_threshold: $unhealthy_borrow_value / LAMPORTS_PER_SOL,
    health_status: "healthy",
    note: "No outstanding debt - infinite collateral ratio",
    confidence: 84
  }

$collateral_ratio = $deposited_value_sol / $borrowed_value_sol
$liquidation_threshold = $unhealthy_borrow_value / LAMPORTS_PER_SOL

**Decision Point:** Assess position health
  BRANCH A ($collateral_ratio < 1.1):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Position at risk of liquidation - collateral ratio too low"
  BRANCH B ($collateral_ratio >= 1.1 AND $collateral_ratio < 1.5):
    $health_status = "warning"
    $risk_level = "medium"
    $note = "Collateral ratio approaching liquidation threshold"
  BRANCH C ($collateral_ratio >= 1.5):
    $health_status = "healthy"
    $risk_level = "low"
    $note = "Healthy collateral ratio with sufficient margin"

**Action:**
RETURN {
  user_address: $user_address,
  collateral_ratio: $collateral_ratio,
  deposited_value_sol: $deposited_value_sol,
  borrowed_value_sol: $borrowed_value_sol,
  liquidation_threshold_sol: $liquidation_threshold,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 84
}

---

## Q92: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$position_address = "PosXYZ"

// Get position account (obligation in lending protocol)
$position_account = getAccountInfo(pubkey: $position_address)

GUARD $position_account != null ELSE
  RETURN ERROR(message: "Position account not found")

// Parse position data for liquidation thresholds
// Offset 96: unhealthy_borrow_value (u128) - liquidation threshold
// Offset 80: allowed_borrow_value (u128) - max borrow before liquidation risk
// Offset 48: deposited_value (u128)
// Offset 64: borrowed_value (u128)

$unhealthy_borrow_value = parseU128(data: $position_account.data, offset: 96)
$allowed_borrow_value = parseU128(data: $position_account.data, offset: 80)
$deposited_value = parseU128(data: $position_account.data, offset: 48)
$borrowed_value = parseU128(data: $position_account.data, offset: 64)

// Convert to SOL
$liquidation_threshold_sol = $unhealthy_borrow_value / LAMPORTS_PER_SOL
$max_safe_borrow_sol = $allowed_borrow_value / LAMPORTS_PER_SOL
$deposited_value_sol = $deposited_value / LAMPORTS_PER_SOL
$borrowed_value_sol = $borrowed_value / LAMPORTS_PER_SOL

// Calculate current collateral ratio
$current_ratio = $borrowed_value_sol > 0 ? $deposited_value_sol / $borrowed_value_sol : "infinite"

// Calculate liquidation ratio (threshold / deposited)
$liquidation_ratio = $deposited_value_sol > 0 ? $liquidation_threshold_sol / $deposited_value_sol : 0

// Calculate distance to liquidation
$distance_to_liquidation = $liquidation_threshold_sol - $borrowed_value_sol

**Decision Point:** Assess liquidation risk
  BRANCH A ($borrowed_value_sol >= $liquidation_threshold_sol):
    $liquidation_status = "liquidatable"
    $risk_level = "immediate"
    $note = "Position is currently eligible for liquidation"
  BRANCH B ($borrowed_value_sol >= $max_safe_borrow_sol AND $borrowed_value_sol < $liquidation_threshold_sol):
    $liquidation_status = "at_risk"
    $risk_level = "high"
    $note = "Position approaching liquidation threshold"
  BRANCH C ($borrowed_value_sol < $max_safe_borrow_sol):
    $liquidation_status = "safe"
    $risk_level = "low"
    $note = "Position well below liquidation threshold"

**Action:**
RETURN {
  position_address: $position_address,
  liquidation_threshold_sol: $liquidation_threshold_sol,
  max_safe_borrow_sol: $max_safe_borrow_sol,
  current_borrowed_sol: $borrowed_value_sol,
  deposited_value_sol: $deposited_value_sol,
  current_collateral_ratio: $current_ratio,
  liquidation_ratio: $liquidation_ratio,
  distance_to_liquidation_sol: $distance_to_liquidation,
  liquidation_status: $liquidation_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 83
}

---

## Q93: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get AMM pool account (Raydium/Orca format)
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool account not found")

// Parse AMM pool data
// Offset 8: nonce (u8)
// Offset 16: token_a_mint (pubkey)
// Offset 48: token_b_mint (pubkey)
// Offset 80: token_a_account (pubkey)
// Offset 112: token_b_account (pubkey)
// Offset 144: mint_lp (pubkey)
// Offset 176: oracle (pubkey)
// Offset 208: serum_market (pubkey)
// Offset 240: serum_program_id (pubkey)
// Offset 272: signers (array)
// Offset 304: token_a_amount (u64)
// Offset 312: token_b_amount (u64)
// Offset 320: sqrt_price (u128)
// Offset 336: current_tick_index (i32)
// Offset 340: tick_spacing (u16)
// Offset 342: fee_growth_global_a (u128)
// Offset 358: fee_growth_global_b (u128)
// Offset 374: protocol_fees_token_a (u64)
// Offset 382: protocol_fees_token_b (u64)

$token_a_amount = parseU64(data: $pool_account.data, offset: 304)
$token_b_amount = parseU64(data: $pool_account.data, offset: 312)

// Get token mints for price calculation
$token_a_mint = parsePubkey(data: $pool_account.data, offset: 16)
$token_b_mint = parsePubkey(data: $pool_account.data, offset: 48)

// Get token accounts to verify amounts
$token_a_account = getAccountInfo(pubkey: parsePubkey(data: $pool_account.data, offset: 80))
$token_b_account = getAccountInfo(pubkey: parsePubkey(data: $pool_account.data, offset: 112))

// Get actual balances from token accounts
$token_a_balance = parseU64(data: $token_a_account.data, offset: 64)
$token_b_balance = parseU64(data: $token_b_account.data, offset: 64)

// Get token prices (simplified - would use oracle/pyth)
$token_a_price_usd = $token_a_mint == "So11111111111111111111111111111111111111112" ? 150 : 1  // SOL price
$token_b_price_usd = $token_b_mint == "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v" ? 1 : 1  // USDC price

// Calculate TVL in USD
$tvl_token_a = ($token_a_balance / 10^9) * $token_a_price_usd  // Assuming 9 decimals
$tvl_token_b = ($token_b_balance / 10^6) * $token_b_price_usd   // Assuming 6 decimals for USDC
$total_tvl_usd = $tvl_token_a + $tvl_token_b

// Calculate pool ratio
$pool_ratio = $token_a_balance > 0 ? $token_b_balance / $token_a_balance : 0

**Decision Point:** Categorize pool size
  BRANCH A ($total_tvl_usd >= 10000000):  // $10M+
    $pool_category = "large"
    $liquidity_depth = "deep"
    $note = "Major pool with excellent liquidity"
  BRANCH B ($total_tvl_usd >= 1000000 AND $total_tvl_usd < 10000000):  // $1M-$10M
    $pool_category = "medium"
    $liquidity_depth = "moderate"
    $note = "Established pool with good liquidity"
  BRANCH C ($total_tvl_usd >= 100000 AND $total_tvl_usd < 1000000):  // $100K-$1M
    $pool_category = "small"
    $liquidity_depth = "shallow"
    $note = "Small pool - watch for slippage"
  BRANCH D ($total_tvl_usd < 100000):  // <$100K
    $pool_category = "micro"
    $liquidity_depth = "very_shallow"
    $note = "Micro pool - high slippage risk"

**Action:**
RETURN {
  pool_address: $pool_address,
  token_a_mint: $token_a_mint,
  token_b_mint: $token_b_mint,
  token_a_balance: $token_a_balance,
  token_b_balance: $token_b_balance,
  token_a_tvl_usd: $tvl_token_a,
  token_b_tvl_usd: $tvl_token_b,
  total_tvl_usd: $total_tvl_usd,
  pool_ratio: $pool_ratio,
  pool_category: $pool_category,
  liquidity_depth: $liquidity_depth,
  note: $note,
  confidence: 82
}

---

## Q94: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get recent transactions for the pool (last 1000 for analysis)
$signatures = getSignaturesForAddress(address: $pool_address, limit: 1000)

GUARD $signatures != null AND COUNT($signatures) > 0 ELSE
  RETURN ERROR(message: "No transaction history found for pool")

// Get current timestamp and calculate 24h ago
$current_time = getCurrentTimestamp()
$one_day_ago = $current_time - 86400  // 24 hours in seconds

// Filter transactions from last 24 hours
$recent_txs = FILTER($signatures, tx => tx.blockTime >= $one_day_ago)

// Get detailed transaction data for volume calculation
$tx_details = MAP($recent_txs, sig => getTransaction(signature: sig.signature))

// Filter out failed transactions
$successful_txs = FILTER($tx_details, tx => tx != null AND tx.meta.err == null)

// Extract swap amounts from transaction logs/metadata
$swap_volumes = MAP($successful_txs, tx => {
  // Parse transaction for token transfers
  // Look for token balance changes in pre/post balances
  $pre_balances = tx.meta.preTokenBalances
  $post_balances = tx.meta.postTokenBalances

  // Calculate net token movements (simplified - real implementation would parse logs)
  $token_a_change = 0
  $token_b_change = 0

  // This is a simplified calculation - real implementation would:
  // 1. Identify swap instructions in transaction
  // 2. Parse the amountIn and amountOut from instruction data
  // 3. Sum up all swap amounts

  // For demo purposes, we'll use a placeholder calculation
  RETURN {
    token_a_volume: ABS($token_a_change),
    token_b_volume: ABS($token_b_change),
    usd_value: 0  // Would calculate based on prices
  }
})

// Sum up total volumes
$total_token_a_volume = SUM(MAP($swap_volumes, v => v.token_a_volume))
$total_token_b_volume = SUM(MAP($swap_volumes, v => v.token_b_volume))

// Get current prices for USD conversion
$token_a_price = 150  // SOL price
$token_b_price = 1    // USDC price

// Calculate USD volumes
$volume_token_a_usd = ($total_token_a_volume / 10^9) * $token_a_price
$volume_token_b_usd = ($total_token_b_volume / 10^6) * $token_b_price
$total_volume_usd = $volume_token_a_usd + $volume_token_b_usd

// Calculate volume per transaction
$avg_volume_per_tx = COUNT($successful_txs) > 0 ? $total_volume_usd / COUNT($successful_txs) : 0

**Decision Point:** Assess trading activity
  BRANCH A ($total_volume_usd >= 10000000):  // $10M+
    $activity_level = "very_high"
    $note = "Exceptionally high volume - major trading activity"
  BRANCH B ($total_volume_usd >= 1000000 AND $total_volume_usd < 10000000):  // $1M-$10M
    $activity_level = "high"
    $note = "Strong trading volume"
  BRANCH C ($total_volume_usd >= 100000 AND $total_volume_usd < 1000000):  // $100K-$1M
    $activity_level = "moderate"
    $note = "Moderate trading activity"
  BRANCH D ($total_volume_usd < 100000):  // <$100K
    $activity_level = "low"
    $note = "Low trading volume"

**Action:**
RETURN {
  pool_address: $pool_address,
  time_period_hours: 24,
  total_volume_usd: $total_volume_usd,
  token_a_volume: $total_token_a_volume,
  token_b_volume: $total_token_b_volume,
  volume_token_a_usd: $volume_token_a_usd,
  volume_token_b_usd: $volume_token_b_usd,
  transaction_count: COUNT($successful_txs),
  avg_volume_per_tx_usd: $avg_volume_per_tx,
  activity_level: $activity_level,
  note: $note,
  confidence: 81
}

---

## Q95: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$swap_amount = 1000  // Amount to swap (in token units)
$pool_address = "PoolXYZ"  // Assume SOL/USDC pool for this example

// Get pool state
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Parse current pool reserves
$token_a_reserve = parseU64(data: $pool_account.data, offset: 304)  // token A amount
$token_b_reserve = parseU64(data: $pool_account.data, offset: 312)  // token B amount

// Assume we're swapping token A for token B
// Current price = token_b_reserve / token_a_reserve
$current_price = $token_b_reserve / $token_a_reserve

// Calculate output amount using AMM formula: (x * y) = k
// For input amount dx, output dy = (y * dx) / (x + dx)
$input_amount = $swap_amount * 10^9  // Convert to smallest unit (assuming 9 decimals)
$output_amount = ($token_b_reserve * $input_amount) / ($token_a_reserve + $input_amount)

// Calculate effective price after swap
$effective_price = $input_amount > 0 ? $output_amount / $input_amount : 0

// Calculate price impact as percentage
$price_impact_pct = (($effective_price - $current_price) / $current_price) * 100

// Calculate slippage (absolute price difference)
$slippage = $effective_price - $current_price

// Calculate output amount in readable units
$output_amount_readable = $output_amount / 10^6  // Assuming 6 decimals for output token

// Minimum output after slippage
$min_output_after_slippage = $output_amount * (1 - ABS($price_impact_pct) / 100)

// Simulate transaction to verify (optional - for validation)
$simulated_tx = simulateTransaction({
  instructions: [
    // Would include actual swap instruction here
    // This is simplified for the example
  ]
})

**Decision Point:** Assess price impact severity
  BRANCH A (ABS($price_impact_pct) >= 5):
    $impact_severity = "severe"
    $recommendation = "avoid_large_swaps"
    $note = "High price impact - consider splitting into smaller trades"
  BRANCH B (ABS($price_impact_pct) >= 2 AND ABS($price_impact_pct) < 5):
    $impact_severity = "moderate"
    $recommendation = "monitor_closely"
    $note = "Moderate slippage - acceptable for most trades"
  BRANCH C (ABS($price_impact_pct) >= 0.5 AND ABS($price_impact_pct) < 2):
    $impact_severity = "low"
    $recommendation = "proceed_normally"
    $note = "Low price impact - good trading conditions"
  BRANCH D (ABS($price_impact_pct) < 0.5):
    $impact_severity = "negligible"
    $recommendation = "optimal_conditions"
    $note = "Minimal slippage - excellent liquidity"

**Action:**
RETURN {
  swap_amount_input: $swap_amount,
  pool_address: $pool_address,
  current_price_ratio: $current_price,
  effective_price_after_swap: $effective_price,
  price_impact_percentage: $price_impact_pct,
  slippage_amount: $slippage,
  expected_output_amount: $output_amount_readable,
  min_output_after_slippage: $min_output_after_slippage / 10^6,
  impact_severity: $impact_severity,
  recommendation: $recommendation,
  note: $note,
  confidence: 80
}

---

## Q96: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get pool account to find LP token mint
$pool_account = getAccountInfo(pubkey: $pool_address)

GUARD $pool_account != null ELSE
  RETURN ERROR(message: "Pool not found")

// Extract LP token mint from pool data
$lp_token_mint = parsePubkey(data: $pool_account.data, offset: 144)

// Get all LP token accounts (holders)
$lp_token_accounts = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [
  { dataSize: 165 },  // Standard token account size
  { memcmp: { offset: 0, bytes: $lp_token_mint } }  // Filter by mint
])

GUARD $lp_token_accounts != null AND COUNT($lp_token_accounts) > 0 ELSE
  RETURN ERROR(message: "No LP token holders found")

// Process each LP token account
$providers_info = MAP($lp_token_accounts, account => {
  $owner = parsePubkey(data: account.account.data, offset: 32)
  $balance = parseU64(data: account.account.data, offset: 64)

  // Skip accounts with zero balance
  GUARD $balance > 0 ELSE
    RETURN null

  RETURN {
    owner_address: $owner,
    lp_token_balance: $balance,
    account_address: account.pubkey
  }
})

// Filter out null entries
$valid_providers = FILTER($providers_info, provider => provider != null)

// Sort by balance descending
$sorted_providers = SORT($valid_providers, (a, b) => b.lp_token_balance - a.lp_token_balance)

// Calculate concentration metrics
$total_lp_tokens = SUM(MAP($sorted_providers, p => p.lp_token_balance))
$top_10_percentage = COUNT($sorted_providers) >= 10 ?
  (SUM(MAP(SLICE($sorted_providers, 0, 10), p => p.lp_token_balance)) / $total_lp_tokens) * 100 : 100

$largest_provider_percentage = COUNT($sorted_providers) > 0 ?
  ($sorted_providers[0].lp_token_balance / $total_lp_tokens) * 100 : 0

**Decision Point:** Assess provider concentration
  BRANCH A ($largest_provider_percentage >= 50):
    $concentration_level = "highly_concentrated"
    $risk_level = "high"
    $note = "Single provider dominates - high impermanent loss risk"
  BRANCH B ($top_10_percentage >= 80):
    $concentration_level = "moderately_concentrated"
    $risk_level = "medium"
    $note = "Top 10 providers hold most liquidity"
  BRANCH C ($top_10_percentage >= 50 AND $top_10_percentage < 80):
    $concentration_level = "somewhat_concentrated"
    $risk_level = "low"
    $note = "Reasonable distribution among providers"
  BRANCH D ($top_10_percentage < 50):
    $concentration_level = "well_distributed"
    $risk_level = "very_low"
    $note = "Liquidity well distributed across many providers"

**Action:**
RETURN {
  pool_address: $pool_address,
  lp_token_mint: $lp_token_mint,
  total_providers: COUNT($sorted_providers),
  total_lp_tokens: $total_lp_tokens,
  top_providers: SLICE($sorted_providers, 0, 20),  // Top 20 providers
  largest_provider_percentage: $largest_provider_percentage,
  top_10_percentage: $top_10_percentage,
  concentration_level: $concentration_level,
  risk_level: $risk_level,
  note: $note,
  confidence: 79
}

---

## Q97: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$farm_address = "FarmXYZ"

// Get farm account (e.g., Raydium/Saber farm)
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm not found")

// Parse farm data for reward rates
// Offset 8: version (u8)
// Offset 16: state (u8) - 1 = enabled, 2 = disabled
// Offset 24: nonce (u8)
// Offset 32: owner (pubkey)
// Offset 64: token_a_mint (pubkey)
// Offset 96: token_b_mint (pubkey)
// Offset 128: lp_token_mint (pubkey)
// Offset 160: reward_a_mint (pubkey)
// Offset 192: reward_b_mint (pubkey)
// Offset 224: reward_a_per_slot (u64)
// Offset 232: reward_b_per_slot (u64)
// Offset 240: total_staked_lp (u64)
// Offset 248: last_update_slot (u64)

$reward_a_per_slot = parseU64(data: $farm_account.data, offset: 224)
$reward_b_per_slot = parseU64(data: $farm_account.data, offset: 232)
$total_staked_lp = parseU64(data: $farm_account.data, offset: 240)

// Get reward token prices (simplified)
$reward_a_price_usd = 150  // Assume SOL rewards
$reward_b_price_usd = 1    // Assume USDC rewards

// Calculate annual rewards
$slots_per_year = 365 * 24 * 60 * 2  // ~2 slots per second
$annual_reward_a_usd = ($reward_a_per_slot * $slots_per_year / 10^9) * $reward_a_price_usd
$annual_reward_b_usd = ($reward_b_per_slot * $slots_per_year / 10^6) * $reward_b_price_usd
$total_annual_rewards_usd = $annual_reward_a_usd + $annual_reward_b_usd

// Get staked value (LP token value)
$staked_value_usd = ($total_staked_lp / 10^9) * 100  // Simplified - would calculate actual LP value

// Calculate APY
GUARD $staked_value_usd > 0 ELSE
  RETURN {
    farm_address: $farm_address,
    apy_percentage: 0,
    total_staked_usd: 0,
    annual_rewards_usd: $total_annual_rewards_usd,
    note: "No tokens staked in farm",
    confidence: 78
  }

$apy_percentage = ($total_annual_rewards_usd / $staked_value_usd) * 100

// Calculate individual token APYs
$apy_a_percentage = $annual_reward_a_usd > 0 ? ($annual_reward_a_usd / $staked_value_usd) * 100 : 0
$apy_b_percentage = $annual_reward_b_usd > 0 ? ($annual_reward_b_usd / $staked_value_usd) * 100 : 0

**Decision Point:** Assess farm attractiveness
  BRANCH A ($apy_percentage >= 50):
    $attractiveness = "excellent"
    $risk_level = "high"
    $note = "Very high APY - likely unsustainable, monitor closely"
  BRANCH B ($apy_percentage >= 20 AND $apy_percentage < 50):
    $attractiveness = "very_good"
    $risk_level = "medium"
    $note = "Strong yields - good farming opportunity"
  BRANCH C ($apy_percentage >= 5 AND $apy_percentage < 20):
    $attractiveness = "moderate"
    $risk_level = "low"
    $note = "Decent returns for conservative farming"
  BRANCH D ($apy_percentage < 5):
    $attractiveness = "poor"
    $risk_level = "very_low"
    $note = "Low yields - consider alternative farms"

**Action:**
RETURN {
  farm_address: $farm_address,
  total_apy_percentage: $apy_percentage,
  reward_a_apy_percentage: $apy_a_percentage,
  reward_b_apy_percentage: $apy_b_percentage,
  total_staked_lp_tokens: $total_staked_lp,
  staked_value_usd: $staked_value_usd,
  annual_rewards_usd: $total_annual_rewards_usd,
  reward_a_annual_usd: $annual_reward_a_usd,
  reward_b_annual_usd: $annual_reward_b_usd,
  attractiveness: $attractiveness,
  risk_level: $risk_level,
  note: $note,
  confidence: 78
}

---

## Q98: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$user_address = "UserXYZ"
$farm_address = "FarmXYZ"

// Get user's farm stake account
$user_stake_account = getAccountInfo(pubkey: $user_address)

GUARD $user_stake_account != null ELSE
  RETURN ERROR(message: "User stake account not found")

// Get farm account for reward rates
$farm_account = getAccountInfo(pubkey: $farm_address)

GUARD $farm_account != null ELSE
  RETURN ERROR(message: "Farm account not found")

// Parse user stake data
// Offset 8: state (u8)
// Offset 16: owner (pubkey)
// Offset 48: stake_time (i64)
// Offset 56: stake_amount (u64)
// Offset 64: reward_a_pending (u64)
// Offset 72: reward_b_pending (u64)
// Offset 80: reward_a_per_token_paid (u128)
// Offset 96: reward_b_per_token_paid (u128)

$user_stake_amount = parseU64(data: $user_stake_account.data, offset: 56)
$reward_a_pending = parseU64(data: $user_stake_account.data, offset: 64)
$reward_b_pending = parseU64(data: $user_stake_account.data, offset: 72)

// Parse farm reward rates
$reward_a_per_slot = parseU64(data: $farm_account.data, offset: 224)
$reward_b_per_slot = parseU64(data: $farm_account.data, offset: 232)
$total_staked = parseU64(data: $farm_account.data, offset: 240)
$last_update_slot = parseU64(data: $farm_account.data, offset: 248)

// Calculate additional rewards earned since last update
$current_slot = getCurrentSlot()
$slots_elapsed = $current_slot - $last_update_slot

// Additional rewards = (user_stake / total_staked) * reward_rate * slots_elapsed
$additional_reward_a = 0
$additional_reward_b = 0

GUARD $total_staked > 0 ELSE
  $additional_reward_a = 0
  $additional_reward_b = 0
BRANCH ELSE
  $stake_ratio = $user_stake_amount / $total_staked
  $additional_reward_a = ($stake_ratio * $reward_a_per_slot * $slots_elapsed)
  $additional_reward_b = ($stake_ratio * $reward_b_per_slot * $slots_elapsed)

// Total pending rewards
$total_reward_a = $reward_a_pending + $additional_reward_a
$total_reward_b = $reward_b_pending + $additional_reward_b

// Convert to readable amounts
$reward_a_readable = $total_reward_a / 10^9  // Assuming 9 decimals
$reward_b_readable = $total_reward_b / 10^6  // Assuming 6 decimals

// Get reward token prices
$reward_a_price_usd = 150  // SOL
$reward_b_price_usd = 1    // USDC

// Calculate USD values
$reward_a_usd = $reward_a_readable * $reward_a_price_usd
$reward_b_usd = $reward_b_readable * $reward_b_price_usd
$total_rewards_usd = $reward_a_usd + $reward_b_usd

// Calculate user's share of farm
$user_share_percentage = $total_staked > 0 ? ($user_stake_amount / $total_staked) * 100 : 0

**Decision Point:** Assess reward significance
  BRANCH A ($total_rewards_usd >= 1000):
    $reward_size = "large"
    $harvest_recommendation = "harvest_now"
    $note = "Significant rewards accumulated - consider harvesting"
  BRANCH B ($total_rewards_usd >= 100 AND $total_rewards_usd < 1000):
    $reward_size = "moderate"
    $harvest_recommendation = "harvest_soon"
    $note = "Notable rewards - harvest when convenient"
  BRANCH C ($total_rewards_usd >= 10 AND $total_rewards_usd < 100):
    $reward_size = "small"
    $harvest_recommendation = "accumulate_further"
    $note = "Small rewards - let them accumulate"
  BRANCH D ($total_rewards_usd < 10):
    $reward_size = "minimal"
    $harvest_recommendation = "wait_longer"
    $note = "Minimal rewards - wait for more accumulation"

**Action:**
RETURN {
  user_address: $user_address,
  farm_address: $farm_address,
  user_stake_amount: $user_stake_amount,
  user_share_percentage: $user_share_percentage,
  reward_a_pending: $reward_a_readable,
  reward_b_pending: $reward_b_readable,
  reward_a_usd_value: $reward_a_usd,
  reward_b_usd_value: $reward_b_usd,
  total_rewards_usd: $total_rewards_usd,
  slots_since_last_update: $slots_elapsed,
  reward_size: $reward_size,
  harvest_recommendation: $harvest_recommendation,
  note: $note,
  confidence: 77
}

---

## Q99: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$pool_address = "PoolXYZ"

// Get lending pool reserve account (Solend/Kamino example)
$reserve_account = getAccountInfo(pubkey: $pool_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Lending pool reserve not found")

// Parse reserve account data
// Offset 8: version (u8)
// Offset 16: available_liquidity (u64)
// Offset 24: borrowed_amount_wads (u128)
// Offset 40: cumulative_borrow_rate_wads (u128)
// Offset 56: market_price (u128)

$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)

// Convert borrowed amount from wads (1e18 scale) to normal amount
$borrowed_amount = $borrowed_amount_wads / 1e18

// Calculate total supply
$total_supply = $available_liquidity + $borrowed_amount

// Calculate utilization rate as percentage
$utilization_rate = ($borrowed_amount / $total_supply) * 100

// Convert to UI amounts for display
$available_ui = $available_liquidity / LAMPORTS_PER_SOL
$borrowed_ui = $borrowed_amount / LAMPORTS_PER_SOL
$total_supply_ui = $total_supply / LAMPORTS_PER_SOL

**Decision Point:** Assess pool health and risk
  BRANCH A ($utilization_rate > 90):
    $health_status = "critical"
    $risk_level = "high"
    $note = "Critically high utilization - may limit withdrawals, increased liquidation risk"
  BRANCH B ($utilization_rate >= 80 AND $utilization_rate <= 90):
    $health_status = "stressed"
    $risk_level = "elevated"
    $note = "High utilization - monitor for liquidity crunches"
  BRANCH C ($utilization_rate >= 50 AND $utilization_rate < 80):
    $health_status = "healthy"
    $risk_level = "moderate"
    $note = "Optimal utilization range for lending pool"
  BRANCH D ($utilization_rate < 50):
    $health_status = "underutilized"
    $risk_level = "low"
    $note = "Low utilization - excess idle capital, lower APY for lenders"

**Action:**
RETURN {
  pool_address: $pool_address,
  utilization_rate_pct: $utilization_rate,
  available_liquidity_ui: $available_ui,
  borrowed_amount_ui: $borrowed_ui,
  total_supply_ui: $total_supply_ui,
  health_status: $health_status,
  risk_level: $risk_level,
  note: $note,
  confidence: 76
}

---

## Q100: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$asset_address = "AssetXYZ"

// Get lending reserve account for the asset
$reserve_account = getAccountInfo(pubkey: $asset_address)

GUARD $reserve_account != null ELSE
  RETURN ERROR(message: "Reserve account not found")

// Parse reserve data for borrow rate
// Offset 56: current_borrow_rate (u64) - in basis points per year
// Offset 64: optimal_borrow_rate (u64)
// Offset 72: max_borrow_rate (u64)
// Offset 80: borrow_rate_curve (struct with points)

$current_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 56)
$optimal_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 64)
$max_borrow_rate_bps = parseU64(data: $reserve_account.data, offset: 72)

// Convert basis points to percentage APY
$current_borrow_apy = $current_borrow_rate_bps / 100
$optimal_borrow_apy = $optimal_borrow_rate_bps / 100
$max_borrow_apy = $max_borrow_rate_bps / 100

// Get utilization rate for context
$available_liquidity = parseU64(data: $reserve_account.data, offset: 16)
$borrowed_amount_wads = parseU128(data: $reserve_account.data, offset: 24)
$borrowed_amount = $borrowed_amount_wads / 1e18
$total_supply = $available_liquidity + $borrowed_amount
$utilization_rate = ($borrowed_amount / $total_supply) * 100

**Decision Point:** Assess borrowing attractiveness
  BRANCH A ($current_borrow_apy < 5):
    $attractiveness = "very_attractive"
    $note = "Very low borrowing costs - ideal for leverage"
  BRANCH B ($current_borrow_apy >= 5 AND $current_borrow_apy < 15):
    $attractiveness = "attractive"
    $note = "Reasonable borrowing costs for most strategies"
  BRANCH C ($current_borrow_apy >= 15 AND $current_borrow_apy < 30):
    $attractiveness = "expensive"
    $note = "High borrowing costs - consider alternatives"
  BRANCH D ($current_borrow_apy >= 30):
    $attractiveness = "very_expensive"
    $note = "Extremely high costs - borrowing not recommended"

**Action:**
RETURN {
  asset_address: $asset_address,
  current_borrow_apy_pct: $current_borrow_apy,
  optimal_borrow_apy_pct: $optimal_borrow_apy,
  max_borrow_apy_pct: $max_borrow_apy,
  utilization_rate_pct: $utilization_rate,
  attractiveness: $attractiveness,
  note: $note,
  confidence: 95
}

---

