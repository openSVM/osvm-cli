# Advanced Scenarios - Basic Questions (Q1-Q100)

**Category:** Advanced Scenarios
**Difficulty:** Basic  
**Focus:** MEV detection, wallet clustering, security analysis

---

## Q1: "Detect potential sandwich attack in transaction GHI789"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock (Solana RPC)
  - FILTER, MAP, FIND (Data Processing)

**Main Branch:**
$victim_tx_sig = "GHI789"

$victim_tx = getTransaction(signature: $victim_tx_sig)
$victim_slot = $victim_tx.slot

// Get the block to see surrounding transactions
$block = getBlock(slot: $victim_slot)

// Find victim transaction position
$victim_index = FIND(
  collection: $block.transactions,
  predicate: tx => tx.signature == $victim_tx_sig
)

// Check transactions immediately before and after
$before_tx = $victim_index > 0 ? $block.transactions[$victim_index - 1] : null
$after_tx = $victim_index < COUNT($block.transactions) - 1 ? $block.transactions[$victim_index + 1] : null

**Decision Point:** Check for sandwich pattern
  BRANCH A ($before_tx != null AND $after_tx != null):
    // Check if same address and same pool
    $same_signer = $before_tx.message.accountKeys[0] == $after_tx.message.accountKeys[0]
    $targets_same_pool = checkSamePool($before_tx, $victim_tx, $after_tx)

    IF $same_signer AND $targets_same_pool THEN
      $is_sandwich = true
      $confidence = 80
    ELSE
      $is_sandwich = false
      $confidence = 60

  BRANCH B (no surrounding transactions):
    $is_sandwich = false
    $confidence = 90

**Action:**
RETURN {
  victim_transaction: $victim_tx_sig,
  is_likely_sandwich: $is_sandwich,
  before_tx: $before_tx?.signature,
  after_tx: $after_tx?.signature,
  same_signer: $same_signer,
  confidence: $confidence,
  note: "Sandwich attacks show buy→victim→sell pattern from same address"
}

---

## Q2: "Are wallets ADDR1 and ADDR2 likely controlled by the same entity?"

**Expected Plan:**

[TIME: ~30s] [COST: free] [CONFIDENCE: 70%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - FILTER, MAP, CORRELATE (Data Processing, Statistical)

**Main Branch:**
$addr1 = INPUT(prompt: "address_1")
$addr2 = INPUT(prompt: "address_2")

// Get transaction history for both
PARALLEL {
  $sigs1 = getSignaturesForAddress(address: $addr1, limit: 100)
  $sigs2 = getSignaturesForAddress(address: $addr2, limit: 100)
}
WAIT_ALL

// Check for common patterns
$timestamps1 = MAP($sigs1, sig => sig.blockTime)
$timestamps2 = MAP($sigs2, sig => sig.blockTime)

$time_correlation = CORRELATE(x: $timestamps1, y: $timestamps2)

// Check for funding relationships
$shared_funding = checkSharedFundingSource($addr1, $addr2)

**Decision Point:** Assess likelihood of same controller
  BRANCH A ($time_correlation > 0.7 AND $shared_funding):
    $same_entity_probability = "high"
    $score = 85
  BRANCH B ($time_correlation > 0.5 OR $shared_funding):
    $same_entity_probability = "medium"
    $score = 60
  BRANCH C ($time_correlation < 0.3 AND NOT $shared_funding):
    $same_entity_probability = "low"
    $score = 20

**Action:**
RETURN {
  address_1: $addr1,
  address_2: $addr2,
  time_correlation: $time_correlation,
  shared_funding_source: $shared_funding,
  same_entity_probability: $same_entity_probability,
  likelihood_score: $score,
  confidence: 70,
  note: "Based on transaction timing patterns and funding relationships"
}

---

## Q3: "Is token XYZ a potential rugpull risk?"

**Expected Plan:**

[TIME: ~20s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getTokenSupply, getTokenLargestAccounts (Solana RPC)
  - SUM, MAP, FILTER (Data Processing)

**Main Branch:**
$mint_address = INPUT(prompt: "token_mint")

// Check mint authority
$mint_account = getAccountInfo(pubkey: $mint_address)
$mint_authority = $mint_account.data.parsed.info.mintAuthority
$freeze_authority = $mint_account.data.parsed.info.freezeAuthority

// Check holder concentration
$top_holders = getTokenLargestAccounts(mint: $mint_address, limit: 10)
$top_10_total = SUM(MAP($top_holders, h => h.uiAmount))

$total_supply = getTokenSupply(mint: $mint_address)
$concentration_percent = ($top_10_total / $total_supply.value.uiAmount) * 100

**Decision Point:** Assess rugpull risk
  BRANCH A ($mint_authority != null):
    // Can mint unlimited - HIGH RISK
    $risk_level = "high"
    $risk_score = 80
    $reason = "Mint authority not revoked, unlimited minting possible"

  BRANCH B ($freeze_authority != null):
    // Can freeze accounts - MEDIUM-HIGH RISK
    $risk_level = "medium-high"
    $risk_score = 60
    $reason = "Freeze authority exists, tokens can be frozen"

  BRANCH C ($concentration_percent > 50):
    // Top 10 hold >50% - MEDIUM RISK
    $risk_level = "medium"
    $risk_score = 50
    $reason = "High holder concentration ({$concentration_percent}%)"

  BRANCH D (all safe):
    $risk_level = "low"
    $risk_score = 20
    $reason = "No mint authority, no freeze authority, distributed holdings"

**Action:**
RETURN {
  token_mint: $mint_address,
  risk_level: $risk_level,
  risk_score: $risk_score,
  reasons: [$reason],
  mint_authority: $mint_authority,
  freeze_authority: $freeze_authority,
  top_10_concentration: $concentration_percent,
  confidence: 80,
  recommendation: $risk_score > 60 ? "HIGH RISK - Avoid" : "Proceed with caution"
}

---

## Q4: "Find MEV bot activity in block 123456"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 65%]

**Available Tools:**
From Standard Library:
  - getBlock, getTransaction (Solana RPC)
  - FILTER, MAP, GROUP_BY (Data Processing)

**Main Branch:**
$slot = 123456

$block = getBlock(slot: $slot)

// Group transactions by signer
$by_signer = GROUP_BY(
  collection: $block.transactions,
  key: tx => tx.message.accountKeys[0]
)

// Look for addresses with multiple txs in same block
$potential_bots = FILTER(
  collection: $by_signer,
  predicate: group => COUNT(group) >= 3
)

**Decision Point:** Analyze transaction patterns
  BRANCH A (COUNT($potential_bots) > 0):
    // Found potential MEV activity
    FOR $bot_group IN $potential_bots:
      // Check if transactions target DEX pools (MEV indicator)
      $dex_txs = FILTER($bot_group, tx => isDEXInteraction(tx))

      IF COUNT($dex_txs) >= 2 THEN
        $likely_mev = true
        BREAK

  BRANCH B (COUNT($potential_bots) == 0):
    $likely_mev = false

**Action:**
RETURN {
  slot: $slot,
  potential_mev_addresses: MAP($potential_bots, group => group[0].signer),
  mev_likelihood: $likely_mev ? "high" : "low",
  confidence: 65,
  note: "Based on multiple transactions per block targeting DEX pools"
}

---

[Q5-Q100 continue with: liquidation detection, governance analysis, protocol revenue tracking, etc.]
