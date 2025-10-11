#!/usr/bin/env python3
"""
Enhanced QA Content Generator for OSVM CLI Test Suite
Generates comprehensive QA samples with forensics, debugging, and diverse tool usage
"""

import os
from pathlib import Path

BASE_PATH = Path("test_qa_categories")

# Enhanced content templates with specific forensics and debugging scenarios

def create_account_forensics():
    return """# Account State - Forensics Level

## Q1: "Detect suspicious account draining pattern"

**Expected Plan:**

[TIME: ~12s] [COST: ~0.005 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress (Solana RPC)
  - MAP, FILTER, SORT_BY (Data Processing)
  - MEAN, STDDEV, DETECT_OUTLIERS (Statistical)

**Main Branch:**
$account = INPUT(prompt: "Enter account address to investigate")

// Get account current state
TRY:
  $account_info = getAccountInfo(pubkey: $account)
CATCH FATAL:
  RETURN ERROR(message: "Account not found")

// Get transaction history
$signatures = getSignaturesForAddress(address: $account, limit: 1000)

// Analyze transaction patterns
$withdrawals = FILTER(
  collection: $signatures,
  predicate: sig => sig.meta.postBalances[0] < sig.meta.preBalances[0]
)

// Calculate withdrawal statistics
$withdrawal_amounts = MAP(
  collection: $withdrawals,
  fn: tx => tx.meta.preBalances[0] - tx.meta.postBalances[0]
)

$mean_withdrawal = MEAN(data: $withdrawal_amounts)
$stddev_withdrawal = STDDEV(data: $withdrawal_amounts)

// Detect outliers (potential draining)
$outliers = DETECT_OUTLIERS(
  data: $withdrawal_amounts,
  threshold: 3  // 3 standard deviations
)

// Check timing patterns
$withdrawal_timestamps = MAP(collection: $withdrawals, fn: tx => tx.blockTime)
$time_intervals = []
FOR $i IN 1..COUNT(collection: $withdrawal_timestamps):
  $interval = $withdrawal_timestamps[$i] - $withdrawal_timestamps[$i-1]
  $time_intervals = APPEND(array: $time_intervals, item: $interval)

$avg_interval = MEAN(data: $time_intervals)

**Decision Point:** Assess drainage risk
  BRANCH A (COUNT(collection: $outliers) > 5 AND $avg_interval < 300):
    $risk_level = "critical"
    $pattern = "automated_draining"
    $explanation = "Multiple large withdrawals in short time - likely bot attack"
    
  BRANCH B (COUNT(collection: $outliers) > 2):
    $risk_level = "high"
    $pattern = "suspicious_activity"
    $explanation = "Unusual withdrawal amounts detected"
    
  BRANCH C ($account_info.lamports < 1000000):
    $risk_level = "medium"
    $pattern = "nearly_drained"
    $explanation = "Account has very low balance remaining"
    
  BRANCH D (true):
    $risk_level = "low"
    $pattern = "normal_activity"
    $explanation = "No suspicious patterns detected"

**Action:**
RETURN {
  account: $account,
  current_balance: $account_info.lamports,
  total_withdrawals: COUNT(collection: $withdrawals),
  mean_withdrawal: $mean_withdrawal,
  outlier_count: COUNT(collection: $outliers),
  average_time_between: $avg_interval,
  risk_level: $risk_level,
  pattern: $pattern,
  explanation: $explanation,
  suspicious_transactions: $outliers,
  confidence: 88,
  caveats: ["Requires manual review for confirmation", "May miss sophisticated attacks"]
}

---

## Q2: "Trace account ownership chain and detect suspicious transfers"

**Expected Plan:**

[TIME: ~15s] [COST: ~0.008 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, FLATTEN, UNIQUE (Data Processing)
  - GROUP_BY (Collection operations)

**Main Branch:**
$account = INPUT(prompt: "Enter account to trace")

// Build ownership chain
$ownership_chain = []
$current_account = $account
$max_depth = 10
$depth = 0

LOOP WHILE $depth < $max_depth:
  TRY:
    $info = getAccountInfo(pubkey: $current_account)
  CATCH RECOVERABLE:
    BREAK  // Account doesn't exist or inaccessible
  
  // Check if it's a PDA
  $is_pda = isPDA(account: $current_account)
  
  IF $is_pda:
    $pda_info = derivePDAOwner(account: $current_account)
    $ownership_chain = APPEND(array: $ownership_chain, item: {
      account: $current_account,
      type: "pda",
      owner: $info.owner,
      parent: $pda_info.program_id
    })
    $current_account = $pda_info.program_id
  ELSE:
    $ownership_chain = APPEND(array: $ownership_chain, item: {
      account: $current_account,
      type: "regular",
      owner: $info.owner
    })
    BREAK  // Reached regular account
  
  $depth += 1

// Analyze recent transfers
$signatures = getSignaturesForAddress(address: $account, limit: 500)

// Get full transaction details in parallel
$txs = []
PARALLEL {
  FOR $sig IN SLICE(collection: $signatures, start: 0, end: 50):
    $tx = getTransaction(signature: $sig.signature)
    $txs = APPEND(array: $txs, item: $tx)
}
WAIT_ALL

// Extract unique interacting accounts
$interacting_accounts = []
FOR $tx IN $txs:
  $accounts = $tx.transaction.message.accountKeys
  FOR $acc IN $accounts:
    IF $acc != $account:
      $interacting_accounts = APPEND(array: $interacting_accounts, item: $acc)

$unique_interactions = UNIQUE(collection: $interacting_accounts)

// Group by frequency
$interaction_counts = {}
FOR $acc IN $interacting_accounts:
  IF $interaction_counts[$acc] exists:
    $interaction_counts[$acc] += 1
  ELSE:
    $interaction_counts[$acc] = 1

// Find most frequent interactors
$frequent_interactors = FILTER(
  collection: $interaction_counts,
  predicate: (acc, count) => count > 5
)

**Decision Point:** Identify suspicious patterns
  BRANCH A (COUNT(collection: $frequent_interactors) == 1):
    $suspicion = "centralized_control"
    $note = "Single account dominates interactions - potential honey pot"
    
  BRANCH B (COUNT(collection: $unique_interactions) < 3 AND COUNT(collection: $txs) > 20):
    $suspicion = "limited_interactions"
    $note = "Many transactions with few unique accounts - bot activity"
    
  BRANCH C ($depth > 5):
    $suspicion = "deep_pda_nesting"
    $note = "Unusual PDA depth - potential obfuscation"
    
  BRANCH D (true):
    $suspicion = "normal"
    $note = "No unusual patterns detected"

**Action:**
RETURN {
  account: $account,
  ownership_chain: $ownership_chain,
  chain_depth: $depth,
  total_transactions: COUNT(collection: $signatures),
  unique_interactors: COUNT(collection: $unique_interactions),
  frequent_interactors: $frequent_interactors,
  suspicion_level: $suspicion,
  note: $note,
  confidence: 85
}

---

## Q3: "Debug rent-exempt calculation discrepancy"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - ABS (Math operations)

**Main Branch:**
$account = INPUT(prompt: "Enter account with rent issues")

TRY:
  $info = getAccountInfo(pubkey: $account)
CATCH FATAL:
  RETURN ERROR(message: "Account not found")

$current_lamports = $info.lamports
$data_size = COUNT(collection: $info.data)

// Calculate expected rent-exempt minimum
// Formula: (128 + data_size) * 6.8 lamports per byte-epoch * 2 years of epochs
$rent_exempt_minimum = ($data_size + 128) * 6960  // 6960 = 6.8 * 365 * 2 / 0.365

// Check actual vs expected
$difference = $current_lamports - $rent_exempt_minimum
$difference_abs = ABS(value: $difference)

**Decision Point:** Diagnose rent status
  BRANCH A ($difference >= 0 AND $difference < 10000):
    $status = "rent_exempt"
    $diagnosis = "Correctly rent-exempt"
    $action_needed = "None"
    
  BRANCH B ($difference < 0):
    $status = "not_rent_exempt"
    $diagnosis = "Account does not have enough lamports for rent exemption"
    $action_needed = "Add " + ABS(value: $difference) + " lamports"
    
  BRANCH C ($difference > $rent_exempt_minimum * 0.5):
    $status = "over_funded"
    $diagnosis = "Account has significantly more than needed"
    $action_needed = "Consider withdrawing excess: " + ($difference - 10000) + " lamports"
    
  BRANCH D (true):
    $status = "optimal"
    $diagnosis = "Account is optimally funded"
    $action_needed = "None"

**Action:**
RETURN {
  account: $account,
  current_lamports: $current_lamports,
  data_size: $data_size,
  required_minimum: $rent_exempt_minimum,
  difference: $difference,
  status: $status,
  diagnosis: $diagnosis,
  action_needed: $action_needed,
  confidence: 95
}

---

## Q4: "Detect account cloning or duplication attacks"

**Expected Plan:**

[TIME: ~20s] [COST: ~0.01 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getProgramAccounts (Solana RPC)
  - FILTER, MAP (Data Processing)
  - CORRELATE (Statistical)

**Main Branch:**
$target_account = INPUT(prompt: "Enter account to check for clones")

TRY:
  $target_info = getAccountInfo(pubkey: $target_account)
CATCH FATAL:
  RETURN ERROR(message: "Target account not found")

$target_owner = $target_info.owner
$target_data = $target_info.data
$target_data_hash = HASH(data: $target_data)

// Find all accounts with same owner
TRY:
  $similar_accounts = getProgramAccounts(programId: $target_owner)
CATCH RECOVERABLE:
  $similar_accounts = []
  LOG(level: "warn", message: "Could not fetch program accounts")

// Compare data structures
$potential_clones = []
FOR $acc IN $similar_accounts:
  IF $acc.pubkey != $target_account:
    $similarity_score = calculateDataSimilarity(
      data1: $target_data,
      data2: $acc.account.data
    )
    
    IF $similarity_score > 0.9:
      $potential_clones = APPEND(array: $potential_clones, item: {
        pubkey: $acc.pubkey,
        similarity: $similarity_score,
        lamports: $acc.account.lamports
      })

// Analyze clone characteristics
$clone_count = COUNT(collection: $potential_clones)

**Decision Point:** Assess cloning risk
  BRANCH A ($clone_count > 50):
    $risk = "critical"
    $assessment = "Mass cloning detected - likely attack"
    
  BRANCH B ($clone_count > 10):
    $risk = "high"
    $assessment = "Multiple clones found - investigate origin"
    
  BRANCH C ($clone_count > 0):
    $risk = "medium"
    $assessment = "Some similar accounts found - may be legitimate"
    
  BRANCH D (true):
    $risk = "none"
    $assessment = "No clones detected"

**Action:**
RETURN {
  target_account: $target_account,
  owner_program: $target_owner,
  potential_clones: $potential_clones,
  clone_count: $clone_count,
  risk_level: $risk,
  assessment: $assessment,
  confidence: 80,
  caveats: ["Similarity based on data structure only", "May include legitimate duplicates"]
}

---

## Q5: "Forensic analysis of deleted/closed account"

**Expected Plan:**

[TIME: ~10s] [COST: ~0.003 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - FILTER, LAST (Data Processing)

**Main Branch:**
$closed_account = INPUT(prompt: "Enter closed account address")

// Get transaction history (may still be available)
TRY:
  $signatures = getSignaturesForAddress(address: $closed_account, limit: 100)
CATCH RECOVERABLE:
  RETURN ERROR(message: "No transaction history available for closed account")

GUARD COUNT(collection: $signatures) > 0 ELSE
  RETURN ERROR(message: "Account has no transaction history")

// Find the closing transaction
$last_signature = LAST(collection: $signatures)
$last_tx = getTransaction(signature: $last_signature.signature)

// Analyze the closing
$pre_balances = $last_tx.meta.preBalances
$post_balances = $last_tx.meta.postBalances
$account_keys = $last_tx.transaction.message.accountKeys

// Find the account index
$account_index = FIND_INDEX(
  collection: $account_keys,
  predicate: key => key == $closed_account
)

$pre_balance = $pre_balances[$account_index]
$post_balance = $post_balances[$account_index]

// Check if account was truly closed
$was_closed = $post_balance == 0 AND $pre_balance > 0

IF $was_closed:
  $closed_at = $last_tx.blockTime
  $recipient = findLamportRecipient(transaction: $last_tx, source_index: $account_index)
  
  // Analyze pre-closing activity
  $recent_txs = SLICE(collection: $signatures, start: 0, end: 10)
  $activity_pattern = analyzeActivityPattern(transactions: $recent_txs)
  
  **Decision Point:** Determine closure reason
    BRANCH A (CONTAINS($activity_pattern, "exploit")):
      $reason = "potential_exploit"
      $explanation = "Suspicious activity before closure"
      
    BRANCH B (CONTAINS($activity_pattern, "normal_cleanup")):
      $reason = "normal_closure"
      $explanation = "Standard account cleanup"
      
    BRANCH C ($pre_balance > 10000000000):
      $reason = "high_value_closure"
      $explanation = "Large account closed - investigate beneficiary"
      
    BRANCH D (true):
      $reason = "unknown"
      $explanation = "Closure reason unclear"
ELSE:
  $reason = "not_closed"
  $explanation = "Account still has balance - not actually closed"

**Action:**
RETURN {
  account: $closed_account,
  was_closed: $was_closed,
  pre_close_balance: $pre_balance,
  post_close_balance: $post_balance,
  closed_at: $closed_at,
  recipient: $recipient,
  closure_reason: $reason,
  explanation: $explanation,
  total_lifetime_txs: COUNT(collection: $signatures),
  confidence: 75,
  caveats: ["Historical data may be incomplete", "Requires archive node for full history"]
}
"""

def create_defi_forensics():
    return """# DeFi Analysis - Forensics Level

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
"""

def create_nft_forensics():
    return """# NFT Analysis - Forensics Level

## Q1: "Detect NFT wash trading and price manipulation"

**Expected Plan:**

[TIME: ~18s] [COST: ~0.012 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, UNIQUE (Data Processing)
  - FIND_PATTERNS, CORRELATE (Statistical)

**Main Branch:**
$nft_mint = INPUT(prompt: "Enter NFT mint address to investigate")

// Get all transfer transactions
$signatures = getSignaturesForAddress(address: $nft_mint, limit: 1000)

// Build transaction graph
$transfers = []
FOR $sig IN $signatures:
  TRY:
    $tx = getTransaction(signature: $sig.signature)
    $transfer_info = extractNFTTransfer(transaction: $tx, mint: $nft_mint)
    
    IF $transfer_info != null:
      $transfers = APPEND(array: $transfers, item: {
        signature: $sig.signature,
        timestamp: $tx.blockTime,
        from: $transfer_info.from,
        to: $transfer_info.to,
        price: $transfer_info.price,
        marketplace: $transfer_info.marketplace
      })
  CATCH RECOVERABLE:
    LOG(level: "debug", message: "Could not parse transaction")

// Identify unique traders
$sellers = UNIQUE(collection: MAP(collection: $transfers, fn: t => t.from))
$buyers = UNIQUE(collection: MAP(collection: $transfers, fn: t => t.to))

// Detect circular trading
$circular_trades = []
FOR $transfer IN $transfers:
  // Check if seller was recent buyer
  $recent_purchases = FILTER(
    collection: $transfers,
    predicate: t => t.to == $transfer.from AND 
                    t.timestamp < $transfer.timestamp AND
                    t.timestamp > ($transfer.timestamp - 86400)  // within 24h
  )
  
  IF COUNT(collection: $recent_purchases) > 0:
    $circular_trades = APPEND(array: $circular_trades, item: $transfer)

// Analyze price progression
$prices = MAP(collection: $transfers, fn: t => t.price)
$price_volatility = STDDEV(data: $prices)
$mean_price = MEAN(data: $prices)

// Check for coordinated activity
$trader_overlap = FILTER(
  collection: $sellers,
  predicate: seller => CONTAINS($buyers, seller)
)

// Calculate wash trading score
$wash_score = (COUNT(collection: $trader_overlap) / COUNT(collection: $sellers)) * 100

**Decision Point:** Assess wash trading likelihood
  BRANCH A ($wash_score > 50 AND COUNT(collection: $circular_trades) > 10):
    $verdict = "confirmed_wash_trading"
    $confidence_level = 90
    $explanation = "High overlap between buyers/sellers with many circular trades"
    
  BRANCH B ($wash_score > 30):
    $verdict = "likely_wash_trading"
    $confidence_level = 75
    $explanation = "Significant trader overlap suggests wash trading"
    
  BRANCH C (COUNT(collection: $circular_trades) > 5):
    $verdict = "suspicious_activity"
    $confidence_level = 60
    $explanation = "Multiple circular trades detected"
    
  BRANCH D ($price_volatility > $mean_price):
    $verdict = "price_manipulation"
    $confidence_level = 65
    $explanation = "Extreme price volatility suggests manipulation"
    
  BRANCH E (true):
    $verdict = "legitimate_trading"
    $confidence_level = 70
    $explanation = "No significant wash trading indicators"

**Action:**
RETURN {
  nft_mint: $nft_mint,
  total_transfers: COUNT(collection: $transfers),
  unique_sellers: COUNT(collection: $sellers),
  unique_buyers: COUNT(collection: $buyers),
  trader_overlap: COUNT(collection: $trader_overlap),
  wash_score: $wash_score,
  circular_trades: COUNT(collection: $circular_trades),
  mean_price: $mean_price,
  price_volatility: $price_volatility,
  verdict: $verdict,
  confidence: $confidence_level,
  explanation: $explanation,
  suspicious_transactions: $circular_trades
}
"""

def create_network_forensics():
    return """# Network Analysis - Forensics Level

## Q1: "Debug network performance degradation"

**Expected Plan:**

[TIME: ~25s] [COST: ~0.02 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getSlot, getRecentPerformanceSamples, getVoteAccounts (Solana RPC)
  - MAP, FILTER, SORT_BY (Data Processing)
  - MEAN, PERCENTILE, CORRELATE (Statistical)

**Main Branch:**
// Gather network metrics
PARALLEL {
  $current_slot = getSlot()
  $performance_samples = getRecentPerformanceSamples(limit: 720)  // ~1 hour
  $vote_accounts = getVoteAccounts()
}
WAIT_ALL

// Analyze performance samples
$sample_metrics = MAP(
  collection: $performance_samples,
  fn: sample => {
    slot: sample.slot,
    num_transactions: sample.numTransactions,
    num_slots: sample.numSlots,
    sample_period_secs: sample.samplePeriodSecs,
    tps: sample.numTransactions / sample.samplePeriodSecs,
    slots_per_second: sample.numSlots / sample.samplePeriodSecs
  }
)

// Calculate TPS statistics
$tps_values = MAP(collection: $sample_metrics, fn: m => m.tps)
$mean_tps = MEAN(data: $tps_values)
$p50_tps = PERCENTILE(data: $tps_values, percentile: 50)
$p95_tps = PERCENTILE(data: $tps_values, percentile: 95)
$min_tps = MIN(values: $tps_values)
$max_tps = MAX(values: $tps_values)

// Detect performance anomalies
$low_performance_periods = FILTER(
  collection: $sample_metrics,
  predicate: sample => sample.tps < ($mean_tps * 0.5)
)

// Analyze validator performance
$delinquent_validators = FILTER(
  collection: $vote_accounts.delinquent,
  predicate: v => v.activatedStake > 0
)

$total_stake = $vote_accounts.current[0].activatedStake + $vote_accounts.delinquent[0].activatedStake
$delinquent_stake = SUM(values: MAP(collection: $delinquent_validators, fn: v => v.activatedStake))
$delinquent_stake_pct = ($delinquent_stake / $total_stake) * 100

**Decision Point:** Diagnose performance issue
  BRANCH A (COUNT(collection: $low_performance_periods) > 100):
    $diagnosis = "sustained_degradation"
    $severity = "critical"
    $root_cause = "Network experiencing prolonged performance issues"
    $recommended_action = "Check for network-wide consensus issues"
    
  BRANCH B ($delinquent_stake_pct > 10):
    $diagnosis = "validator_issues"
    $severity = "high"
    $root_cause = "High percentage of stake is delinquent"
    $recommended_action = "Investigate validator connectivity and performance"
    
  BRANCH C ($mean_tps < 1000):
    $diagnosis = "low_throughput"
    $severity = "medium"
    $root_cause = "Network throughput below normal levels"
    $recommended_action = "Monitor for congestion or spam"
    
  BRANCH D ($max_tps - $min_tps > $mean_tps * 2):
    $diagnosis = "high_volatility"
    $severity = "medium"
    $root_cause = "Highly variable performance"
    $recommended_action = "Check for traffic spikes or validator instability"
    
  BRANCH E (true):
    $diagnosis = "healthy"
    $severity = "low"
    $root_cause = "Network performing within normal parameters"
    $recommended_action = "Continue monitoring"

**Action:**
RETURN {
  current_slot: $current_slot,
  mean_tps: $mean_tps,
  p50_tps: $p50_tps,
  p95_tps: $p95_tps,
  min_tps: $min_tps,
  max_tps: $max_tps,
  low_performance_periods: COUNT(collection: $low_performance_periods),
  delinquent_validators: COUNT(collection: $delinquent_validators),
  delinquent_stake_pct: $delinquent_stake_pct,
  diagnosis: $diagnosis,
  severity: $severity,
  root_cause: $root_cause,
  recommended_action: $recommended_action,
  confidence: 88
}
"""

def main():
    """Create enhanced forensics files"""
    
    # Define which files to create/enhance
    enhancements = {
        "02_account_state/07_forensics.md": create_account_forensics,
        "05_defi_analysis/07_forensics.md": create_defi_forensics,
        "06_nft_analysis/07_forensics.md": create_nft_forensics,
        "07_network_analysis/07_forensics.md": create_network_forensics,
    }
    
    for file_path, generator_func in enhancements.items():
        full_path = BASE_PATH / file_path
        content = generator_func()
        
        with open(full_path, 'w') as f:
            f.write(content)
        
        print(f"Enhanced: {full_path}")
    
    print("\nForensics files enhanced successfully!")

if __name__ == "__main__":
    main()
