# Transaction Analysis - Forensics Level

## Q1: "Debug a failed transaction and identify root cause"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, FIND (Data Processing)
  - CONTAINS (String operations)

**Main Branch:**
$signature = INPUT(prompt: "Enter failed transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Verify it actually failed
GUARD $tx.meta.err != null ELSE
  RETURN ERROR(message: "Transaction succeeded - nothing to debug")

$error_info = $tx.meta.err
$log_messages = $tx.meta.logMessages

// Extract error-related logs
$error_logs = FILTER(
  collection: $log_messages,
  predicate: msg => CONTAINS(msg, "Error") OR CONTAINS(msg, "failed") OR CONTAINS(msg, "insufficient")
)

// Find the failing instruction
$program_invocations = FILTER(
  collection: $log_messages,
  predicate: msg => CONTAINS(msg, "Program") AND CONTAINS(msg, "invoke")
)

$failed_invocation = FIND(
  collection: $program_invocations,
  predicate: msg => CONTAINS(msg, "failed")
)

// Identify failure type
**Decision Point:** Classify error
  BRANCH A (CONTAINS($error_info, "InsufficientFunds")):
    $error_type = "insufficient_funds"
    $root_cause = "Account does not have enough lamports"
    $solution = "Ensure account has sufficient balance before transaction"
    
  BRANCH B (CONTAINS($error_info, "AccountNotFound")):
    $error_type = "account_not_found"
    $root_cause = "Required account does not exist"
    $solution = "Verify all account addresses are correct"
    
  BRANCH C (CONTAINS($error_info, "InvalidAccountData")):
    $error_type = "invalid_account_data"
    $root_cause = "Account data format is incorrect"
    $solution = "Check account initialization and data structure"
    
  BRANCH D (CONTAINS($error_info, "Custom")):
    $error_type = "program_error"
    $root_cause = "Program returned custom error"
    $solution = "Check program logs for specific error code"
    
  BRANCH E (true):
    $error_type = "unknown"
    $root_cause = "Unclassified error"
    $solution = "Analyze full error logs"

// Extract compute budget info
$compute_consumed = $tx.meta.computeUnitsConsumed

**Action:**
RETURN {
  signature: $signature,
  error_type: $error_type,
  error_info: $error_info,
  root_cause: $root_cause,
  solution: $solution,
  failed_at: $failed_invocation,
  error_logs: $error_logs,
  compute_consumed: $compute_consumed,
  full_logs: $log_messages,
  confidence: 90,
  caveats: ["Manual log review may be needed for complex errors"]
}

---

## Q2: "Detect MEV (Maximal Extractable Value) in transaction"

**Expected Plan:**

[TIME: ~10s] [COST: ~0.001 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock (Solana RPC)
  - FILTER, MAP (Data Processing)
  - CONTAINS (String operations)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Get block to analyze transaction ordering
$slot = $tx.slot
$block = getBlock(slot: $slot)

// Find transaction position in block
$block_txs = $block.transactions
$tx_index = FIND_INDEX(collection: $block_txs, predicate: btx => btx.signature == $signature)

// Analyze for MEV indicators
$instructions = $tx.transaction.message.instructions
$account_keys = $tx.transaction.message.accountKeys

// Check for DEX interactions
$dex_programs = [
  "9xQeWvG816bUx9EPjHmaT23yvVM2ZWbrrpZb9PusVFin",  // Serum
  "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc",  // Orca Whirlpool
  "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8",  // Raydium
]

$has_dex_interaction = ANY(
  collection: $instructions,
  predicate: ix => CONTAINS($dex_programs, $account_keys[ix.programIdIndex])
)

// Check transaction timing and priority fee
$priority_fee = extractPriorityFee(transaction: $tx)
$block_time = $tx.blockTime

// Analyze surrounding transactions for sandwiching
$surrounding_window = 5
$prev_txs = SLICE(collection: $block_txs, start: MAX_OF($tx_index - $surrounding_window, 0), end: $tx_index)
$next_txs = SLICE(collection: $block_txs, start: $tx_index + 1, end: MIN_OF($tx_index + $surrounding_window + 1, COUNT(collection: $block_txs)))

// Check if surrounded by similar DEX transactions
$prev_dex_count = COUNT(collection: FILTER($prev_txs, tx => hasDEXInteraction(tx)))
$next_dex_count = COUNT(collection: FILTER($next_txs, tx => hasDEXInteraction(tx)))

**Decision Point:** Assess MEV likelihood
  BRANCH A ($has_dex_interaction AND $priority_fee > 10000 AND $prev_dex_count > 0 AND $next_dex_count > 0):
    $mev_likelihood = "high"
    $mev_type = "sandwich_attack_victim"
    $explanation = "Transaction is sandwiched between other DEX transactions with high priority fees"
    
  BRANCH B ($has_dex_interaction AND $priority_fee > 50000):
    $mev_likelihood = "medium"
    $mev_type = "possible_frontrun"
    $explanation = "High priority fee on DEX transaction suggests MEV attempt"
    
  BRANCH C ($has_dex_interaction AND $tx_index < 10):
    $mev_likelihood = "medium"
    $mev_type = "early_block_placement"
    $explanation = "DEX transaction placed early in block"
    
  BRANCH D (true):
    $mev_likelihood = "low"
    $mev_type = "normal_transaction"
    $explanation = "No significant MEV indicators detected"

**Action:**
RETURN {
  signature: $signature,
  mev_likelihood: $mev_likelihood,
  mev_type: $mev_type,
  explanation: $explanation,
  has_dex_interaction: $has_dex_interaction,
  priority_fee: $priority_fee,
  tx_index_in_block: $tx_index,
  prev_dex_count: $prev_dex_count,
  next_dex_count: $next_dex_count,
  confidence: 85,
  caveats: ["MEV detection is probabilistic", "Requires block-level analysis"]
}
