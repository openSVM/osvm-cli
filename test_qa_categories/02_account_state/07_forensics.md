# Account State - Forensics Level

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
