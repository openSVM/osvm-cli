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
