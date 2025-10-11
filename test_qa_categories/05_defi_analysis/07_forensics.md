# DeFi Analysis - Forensics Level

## Q1: "Detect flash loan attack patterns"

**Expected Plan:**

[TIME: ~15s] [COST: ~0.01 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock (Solana RPC)
  - FILTER, MAP, SUM (Data Processing)
  - FIND_PATTERNS (Statistical)

**Main Branch:**
$tx_signature = INPUT(prompt: "Enter transaction to analyze for flash loan attack")

TRY:
  $tx = getTransaction(signature: $tx_signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Get the full block for context
$block = getBlock(slot: $tx.slot)

// Analyze instruction sequence
$instructions = $tx.transaction.message.instructions
$inner_instructions = $tx.meta.innerInstructions
$account_keys = $tx.transaction.message.accountKeys

// Identify DeFi protocols involved
$defi_programs = identifyDeFiPrograms(instructions: $instructions, account_keys: $account_keys)

// Track balance changes
$balance_changes = calculateBalanceChanges(transaction: $tx)

// Look for flash loan indicators:
// 1. Large intra-transaction balance swings
// 2. Multiple DEX interactions
// 3. Net-zero balance change for certain accounts
// 4. Complex CPI chain

$max_balance_swing = 0
FOR $change IN $balance_changes:
  $abs_change = ABS(value: $change.max_balance - $change.min_balance)
  IF $abs_change > $max_balance_swing:
    $max_balance_swing = $abs_change

$dex_interaction_count = COUNT(collection: FILTER(
  collection: $defi_programs,
  predicate: prog => prog.type == "dex"
))

// Check for arbitrage pattern
$arbitrage_pattern = detectArbitragePattern(
  instructions: $instructions,
  balance_changes: $balance_changes
)

**Decision Point:** Classify attack likelihood
  BRANCH A ($max_balance_swing > 1000000000000 AND $dex_interaction_count >= 2 AND $arbitrage_pattern.detected):
    $classification = "flash_loan_attack"
    $confidence_score = 90
    $evidence = "Large balance swing, multiple DEX interactions, arbitrage pattern"
    
  BRANCH B ($max_balance_swing > 100000000000 AND $dex_interaction_count >= 3):
    $classification = "suspicious_arbitrage"
    $confidence_score = 75
    $evidence = "Significant balance movements across multiple DEXs"
    
  BRANCH C ($arbitrage_pattern.detected):
    $classification = "legitimate_arbitrage"
    $confidence_score = 60
    $evidence = "Arbitrage detected but parameters within normal range"
    
  BRANCH D (true):
    $classification = "normal_defi_interaction"
    $confidence_score = 50
    $evidence = "No flash loan indicators found"

// Calculate profit/loss
$net_profit = calculateNetProfit(balance_changes: $balance_changes)

**Action:**
RETURN {
  transaction: $tx_signature,
  classification: $classification,
  confidence: $confidence_score,
  evidence: $evidence,
  max_balance_swing_lamports: $max_balance_swing,
  dex_interactions: $dex_interaction_count,
  defi_programs: $defi_programs,
  net_profit_lamports: $net_profit,
  arbitrage_pattern: $arbitrage_pattern,
  caveats: ["Requires domain expertise to confirm", "May miss novel attack vectors"]
}

---

## Q2: "Analyze liquidity pool manipulation"

**Expected Plan:**

[TIME: ~20s] [COST: ~0.015 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, SORT_BY (Data Processing)
  - MEAN, STDDEV, PERCENTILE (Statistical)
  - FIND_PATTERNS (Statistical)

**Main Branch:**
$pool_account = INPUT(prompt: "Enter liquidity pool account")

// Get pool state
TRY:
  $pool_info = getAccountInfo(pubkey: $pool_account)
CATCH FATAL:
  RETURN ERROR(message: "Pool account not found")

// Get recent transactions
$signatures = getSignaturesForAddress(address: $pool_account, limit: 500)

// Analyze transaction volume over time
$time_series = []
FOR $sig IN $signatures:
  $tx = getTransaction(signature: $sig.signature)
  $volume = extractSwapVolume(transaction: $tx, pool: $pool_account)
  
  $time_series = APPEND(array: $time_series, item: {
    timestamp: $tx.blockTime,
    volume: $volume,
    signature: $sig.signature
  })

// Sort by time
$sorted_series = SORT_BY(collection: $time_series, key: "timestamp")

// Calculate volume statistics
$volumes = MAP(collection: $sorted_series, fn: item => item.volume)
$mean_volume = MEAN(data: $volumes)
$stddev_volume = STDDEV(data: $volumes)

// Detect volume spikes
$volume_spikes = FILTER(
  collection: $sorted_series,
  predicate: item => item.volume > ($mean_volume + 3 * $stddev_volume)
)

// Check for sandwich attacks
$sandwich_attacks = []
FOR $i IN 1..(COUNT(collection: $sorted_series) - 1):
  $prev = $sorted_series[$i - 1]
  $curr = $sorted_series[$i]
  $next = $sorted_series[$i + 1]
  
  // Pattern: large trade, victim trade, large opposite trade
  IF $prev.volume > $mean_volume * 2 AND 
     $next.volume > $mean_volume * 2 AND
     $curr.volume < $mean_volume AND
     ($next.timestamp - $prev.timestamp) < 60:
    $sandwich_attacks = APPEND(array: $sandwich_attacks, item: {
      victim_tx: $curr.signature,
      front_run: $prev.signature,
      back_run: $next.signature,
      time_window: $next.timestamp - $prev.timestamp
    })

// Detect wash trading
$wash_trading_score = detectWashTrading(
  transactions: $sorted_series,
  pool: $pool_account
)

**Decision Point:** Assess manipulation
  BRANCH A (COUNT(collection: $sandwich_attacks) > 10):
    $manipulation_type = "sandwich_attack_pattern"
    $severity = "high"
    $recommendation = "Pool is actively being sandwich attacked"
    
  BRANCH B ($wash_trading_score > 0.7):
    $manipulation_type = "wash_trading"
    $severity = "high"
    $recommendation = "Significant wash trading detected"
    
  BRANCH C (COUNT(collection: $volume_spikes) > COUNT(collection: $sorted_series) * 0.1):
    $manipulation_type = "volume_manipulation"
    $severity = "medium"
    $recommendation = "Unusual volume spikes detected"
    
  BRANCH D (true):
    $manipulation_type = "normal_activity"
    $severity = "low"
    $recommendation = "No significant manipulation detected"

**Action:**
RETURN {
  pool: $pool_account,
  total_transactions: COUNT(collection: $signatures),
  mean_volume: $mean_volume,
  stddev_volume: $stddev_volume,
  volume_spikes: COUNT(collection: $volume_spikes),
  sandwich_attacks: $sandwich_attacks,
  sandwich_count: COUNT(collection: $sandwich_attacks),
  wash_trading_score: $wash_trading_score,
  manipulation_type: $manipulation_type,
  severity: $severity,
  recommendation: $recommendation,
  confidence: 80
}
