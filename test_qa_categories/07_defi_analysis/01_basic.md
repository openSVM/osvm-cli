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

[Q4-Q100 continue with: liquidity pool TVL, farm APY calculations, oracle prices, lending positions, etc.]
## Q4: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr004xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 91
}

---

## Q5: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr005xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 90
}

---

## Q6: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr006xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 89
}

---

## Q7: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr007xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 88
}

---

## Q8: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr008xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 87
}

---

## Q9: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr009xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 86
}

---

## Q10: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr010xyz"

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

## Q11: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr011xyz"

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

## Q12: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr012xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 83
}

---

## Q13: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr013xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 82
}

---

## Q14: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr014xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 81
}

---

## Q15: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr015xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 80
}

---

## Q16: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr016xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 79
}

---

## Q17: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr017xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 78
}

---

## Q18: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr018xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 77
}

---

## Q19: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr019xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 76
}

---

## Q20: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr020xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 95
}

---

## Q21: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr021xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate collateral ratio
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 94
}

---

## Q22: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr022xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 93
}

---

## Q23: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr023xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 92
}

---

## Q24: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr024xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 91
}

---

## Q25: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr025xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 90
}

---

## Q26: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr026xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 89
}

---

## Q27: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr027xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 88
}

---

## Q28: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr028xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 87
}

---

## Q29: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr029xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 86
}

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

## Q32: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr032xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 83
}

---

## Q33: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr033xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 82
}

---

## Q34: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr034xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 81
}

---

## Q35: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr035xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 80
}

---

## Q36: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr036xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 79
}

---

## Q37: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr037xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 78
}

---

## Q38: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr038xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 77
}

---

## Q39: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr039xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 76
}

---

## Q40: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr040xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 95
}

---

## Q41: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr041xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate collateral ratio
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 94
}

---

## Q42: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr042xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 93
}

---

## Q43: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr043xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 92
}

---

## Q44: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr044xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 91
}

---

## Q45: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr045xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 90
}

---

## Q46: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr046xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 89
}

---

## Q47: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr047xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 88
}

---

## Q48: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr048xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 87
}

---

## Q49: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr049xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 86
}

---

## Q50: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr050xyz"

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

## Q51: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr051xyz"

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

## Q52: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr052xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 83
}

---

## Q53: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr053xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 82
}

---

## Q54: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr054xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 81
}

---

## Q55: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr055xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 80
}

---

## Q56: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr056xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 79
}

---

## Q57: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr057xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 78
}

---

## Q58: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr058xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 77
}

---

## Q59: "What is the utilization rate of lending pool PoolXYZ?"

**Expected Plan:**

[TIME: ~11s] [COST: free] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr059xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 76
}

---

## Q60: "What is the borrow APY for asset AssetXYZ?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr060xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 95
}

---

## Q61: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr061xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate collateral ratio
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 94
}

---

## Q62: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 93%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr062xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 93
}

---

## Q63: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr063xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 92
}

---

## Q64: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr064xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 91
}

---

## Q65: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr065xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 90
}

---

## Q66: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr066xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 89
}

---

## Q67: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr067xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 88
}

---

## Q68: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr068xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr069xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr070xyz"

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

## Q71: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr071xyz"

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

## Q72: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr072xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 83
}

---

## Q73: "What is the TVL of liquidity pool PoolXYZ?"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr073xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 82
}

---

## Q74: "What is the 24h volume for pool PoolXYZ?"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr074xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 81
}

---

## Q75: "What is the current price impact for swapping 1000?"

**Expected Plan:**

[TIME: ~7s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - simulateTransaction
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr075xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 80
}

---

## Q76: "List all liquidity providers for pool PoolXYZ"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr076xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 79
}

---

## Q77: "What is the APY for farm FarmXYZ?"

**Expected Plan:**

[TIME: ~9s] [COST: free] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr077xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 78
}

---

## Q78: "What rewards are pending for user UserXYZ in farm FarmXYZ?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr078xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr079xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr080xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr081xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate collateral ratio
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr082xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr083xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr084xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr085xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr086xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr087xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr088xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr089xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr090xyz"

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

## Q91: "What is user UserXYZ's collateral ratio in lending protocol?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr091xyz"

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

## Q92: "What is the liquidation threshold for position PosXYZ?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - getAccountInfo
  - MAP, FILTER, COUNT, SUM (Data Processing)

**Main Branch:**
$input = "Addr092xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get liquidation threshold
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr093xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate total value locked
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr094xyz"

$result = getSignaturesForAddress({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate daily volume
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr095xyz"

$result = simulateTransaction({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate price impact
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr096xyz"

$result = getProgramAccounts({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Find LP token holders
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr097xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate farm yield
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr098xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get pending rewards
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr099xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Calculate utilization
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
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
$input = "Addr100xyz"

$result = getAccountInfo({appropriate params})

GUARD $result != null ELSE
  RETURN ERROR(message: "Resource not found")

// Get borrow rate
$processed_data = // Process $result appropriately

**Action:**
RETURN {
  input: $input,
  result: $processed_data,
  confidence: 95
}

---

