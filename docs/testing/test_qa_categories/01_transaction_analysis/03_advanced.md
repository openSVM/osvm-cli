# Transaction Analysis - Advanced Level

## Q1: "Analyze the Cross-Program Invocation (CPI) chain for transaction 1abc...def"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, FILTER, FLATTEN (Data Processing)
  - COUNT (Collection operations)

**Main Branch:**
$signature = "1abc...def"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Extract inner instructions (CPIs)
$inner_instructions = $tx.meta.innerInstructions
$cpi_count = COUNT(collection: $inner_instructions)

// Map CPI chain
$cpi_chain = MAP(
  collection: $inner_instructions,
  fn: inner => {
    program: $tx.transaction.message.accountKeys[inner.index],
    instructions: inner.instructions,
    depth: calculateCPIDepth(inner)
  }
)

// Calculate metrics
$total_cpi_calls = SUM(
  values: MAP(collection: $cpi_chain, fn: item => COUNT(collection: item.instructions))
)

$max_depth = MAX(values: MAP(collection: $cpi_chain, fn: item => item.depth))

**Decision Point:** Analyze CPI complexity
  BRANCH A ($max_depth > 4):
    $complexity = "very_high"
    $warning = "Deep CPI nesting detected - potential security risk"
  BRANCH B ($max_depth > 2):
    $complexity = "high"
    $warning = "Moderate CPI nesting"
  BRANCH C ($max_depth <= 2):
    $complexity = "normal"
    $warning = "Standard CPI usage"

**Action:**
RETURN {
  signature: $signature,
  cpi_count: $cpi_count,
  total_cpi_calls: $total_cpi_calls,
  max_depth: $max_depth,
  cpi_chain: $cpi_chain,
  complexity: $complexity,
  warning: $warning,
  confidence: 95
}

---

## Q2: "Detect if transaction contains priority fees and calculate optimal fee"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - getRecentPrioritizationFees (Solana RPC)
  - extractPriorityFee (Solana Utilities)
  - MEAN, PERCENTILE (Statistical)

**Main Branch:**
$signature = "2def...ghi"

PARALLEL {
  $tx = getTransaction(signature: $signature)
  $recent_fees = getRecentPrioritizationFees()
}
WAIT_ALL

// Extract priority fee from transaction
$priority_fee = extractPriorityFee(transaction: $tx)

// Calculate optimal fee from recent data
$fee_values = MAP(collection: $recent_fees, fn: fee => fee.prioritizationFee)
$avg_fee = MEAN(data: $fee_values)
$p50_fee = PERCENTILE(data: $fee_values, percentile: 50)
$p75_fee = PERCENTILE(data: $fee_values, percentile: 75)
$p90_fee = PERCENTILE(data: $fee_values, percentile: 90)

**Decision Point:** Analyze fee efficiency
  BRANCH A ($priority_fee > $p90_fee):
    $efficiency = "overpaid"
    $recommendation = "Reduce priority fee to p75 level"
  BRANCH B ($priority_fee >= $p50_fee AND $priority_fee <= $p75_fee):
    $efficiency = "optimal"
    $recommendation = "Fee is in optimal range"
  BRANCH C ($priority_fee < $p50_fee):
    $efficiency = "underpaid"
    $recommendation = "Consider increasing for faster confirmation"
  BRANCH D ($priority_fee == 0):
    $efficiency = "none"
    $recommendation = "No priority fee - may face delays"

**Action:**
RETURN {
  signature: $signature,
  priority_fee_paid: $priority_fee,
  average_market_fee: $avg_fee,
  p50: $p50_fee,
  p75: $p75_fee,
  p90: $p90_fee,
  efficiency: $efficiency,
  recommendation: $recommendation,
  potential_savings: MAX_OF($priority_fee - $p75_fee, 0),
  confidence: 90
}

---

## Q3: "Reconstruct the execution flow and identify failed instructions"

**Expected Plan:**

[TIME: ~6s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, FILTER (Data Processing)
  - FIND (Collection operations)

**Main Branch:**
$signature = "3ghi...jkl"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Check if transaction failed
$error = $tx.meta.err

**Decision Point:** Transaction status
  BRANCH A ($error != null):
    // Transaction failed - identify failing instruction
    $log_messages = $tx.meta.logMessages
    
    // Parse logs to find error
    $error_logs = FILTER(
      collection: $log_messages,
      predicate: msg => CONTAINS(msg, "failed") OR CONTAINS(msg, "Error")
    )
    
    // Find instruction index that failed
    $failed_instruction_match = FIND(
      collection: $log_messages,
      predicate: msg => CONTAINS(msg, "Program") AND CONTAINS(msg, "failed")
    )
    
    // Extract program that failed
    $failed_program = extractProgramFromLog(log: $failed_instruction_match)
    
    // Map execution flow up to failure
    $execution_flow = []
    FOR $msg IN $log_messages:
      IF CONTAINS($msg, "Program") AND CONTAINS($msg, "invoke"):
        $execution_flow = APPEND(array: $execution_flow, item: {
          action: "invoke",
          message: $msg,
          success: NOT CONTAINS($msg, "failed")
        })
        BREAK IF CONTAINS($msg, "failed")
    
    $status = "failed"
    $failure_point = COUNT(collection: $execution_flow)
    
  BRANCH B ($error == null):
    // Transaction succeeded
    $log_messages = $tx.meta.logMessages
    $execution_flow = MAP(
      collection: FILTER(
        collection: $log_messages,
        predicate: msg => CONTAINS(msg, "Program") AND CONTAINS(msg, "invoke")
      ),
      fn: msg => {
        action: "invoke",
        message: msg,
        success: true
      }
    )
    
    $status = "success"
    $failed_program = null
    $failure_point = null

**Action:**
RETURN {
  signature: $signature,
  status: $status,
  error: $error,
  execution_flow: $execution_flow,
  total_instructions: COUNT(collection: $tx.transaction.message.instructions),
  executed_instructions: COUNT(collection: $execution_flow),
  failed_program: $failed_program,
  failure_point: $failure_point,
  confidence: 95
}

---

## Q4: "Calculate the effective cost per compute unit and compare to network average"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - getRecentPrioritizationFees (Solana RPC)
  - MEAN, STDDEV (Statistical)

**Main Branch:**
$signature = "4jkl...mno"

PARALLEL {
  $tx = getTransaction(signature: $signature)
  $recent_fees = getRecentPrioritizationFees()
}
WAIT_ALL

// Calculate this transaction's metrics
$total_fee = $tx.meta.fee
$compute_units = $tx.meta.computeUnitsConsumed
$priority_fee = extractPriorityFee(transaction: $tx)

GUARD $compute_units > 0 ELSE
  RETURN ERROR(message: "Compute units not available")

$cost_per_cu = $total_fee / $compute_units
$priority_cost_per_cu = $priority_fee / $compute_units

// Calculate network averages
$network_fees = MAP(collection: $recent_fees, fn: fee => fee.prioritizationFee)
$avg_network_fee = MEAN(data: $network_fees)
$stddev_fee = STDDEV(data: $network_fees)

// Estimate average compute units (use typical value)
$typical_cu = 200000
$estimated_network_cost_per_cu = $avg_network_fee / $typical_cu

**Decision Point:** Compare to network
  BRANCH A ($cost_per_cu > $estimated_network_cost_per_cu * 1.5):
    $assessment = "expensive"
    $note = "Transaction is significantly more expensive than network average"
  BRANCH B ($cost_per_cu > $estimated_network_cost_per_cu):
    $assessment = "above_average"
    $note = "Transaction costs more than network average"
  BRANCH C ($cost_per_cu <= $estimated_network_cost_per_cu):
    $assessment = "efficient"
    $note = "Transaction is cost-efficient"

**Action:**
RETURN {
  signature: $signature,
  total_fee_lamports: $total_fee,
  compute_units: $compute_units,
  cost_per_cu: $cost_per_cu,
  priority_cost_per_cu: $priority_cost_per_cu,
  network_avg_cost_per_cu: $estimated_network_cost_per_cu,
  assessment: $assessment,
  note: $note,
  confidence: 88,
  caveats: ["Network average estimated from recent fees and typical compute units"]
}

---

## Q5: "Analyze account write locks and detect potential race conditions"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)
  - UNIQUE (Collection operations)

**Main Branch:**
$signature = "5mno...pqr"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$message = $tx.transaction.message
$account_keys = $message.accountKeys

// Extract writable accounts
$num_required_sigs = $message.header.numRequiredSignatures
$num_readonly_signed = $message.header.numReadonlySignedAccounts
$num_readonly_unsigned = $message.header.numReadonlyUnsignedAccounts

// Writable signed accounts
$writable_signed = []
FOR $i IN 0..($num_required_sigs - $num_readonly_signed):
  $writable_signed = APPEND(array: $writable_signed, item: $account_keys[$i])

// Writable unsigned accounts  
$start_unsigned = $num_required_sigs
$end_unsigned = COUNT(collection: $account_keys) - $num_readonly_unsigned
$writable_unsigned = []
FOR $i IN $start_unsigned..$end_unsigned:
  $writable_unsigned = APPEND(array: $writable_unsigned, item: $account_keys[$i])

$all_writable = FLATTEN(collection: [$writable_signed, $writable_unsigned])
$unique_writable = UNIQUE(collection: $all_writable)

// Analyze write patterns
$write_lock_count = COUNT(collection: $unique_writable)

// Check for common patterns that indicate race conditions
$has_token_accounts = ANY(
  collection: $unique_writable,
  predicate: acc => isTokenAccount(acc)
)

$has_pdas = ANY(
  collection: $unique_writable,
  predicate: acc => isPDA(acc)
)

**Decision Point:** Assess race condition risk
  BRANCH A ($write_lock_count > 10):
    $risk_level = "high"
    $warning = "Many write locks - high contention risk"
  BRANCH B ($write_lock_count > 5 AND $has_token_accounts):
    $risk_level = "medium"
    $warning = "Multiple token account writes - potential race condition"
  BRANCH C ($has_pdas AND $write_lock_count > 3):
    $risk_level = "medium"
    $warning = "PDA writes with multiple accounts - monitor for conflicts"
  BRANCH D (true):
    $risk_level = "low"
    $warning = "Normal write lock pattern"

**Action:**
RETURN {
  signature: $signature,
  writable_accounts: $unique_writable,
  write_lock_count: $write_lock_count,
  writable_signed_count: COUNT(collection: $writable_signed),
  writable_unsigned_count: COUNT(collection: $writable_unsigned),
  has_token_accounts: $has_token_accounts,
  has_pdas: $has_pdas,
  risk_level: $risk_level,
  warning: $warning,
  confidence: 92
}
