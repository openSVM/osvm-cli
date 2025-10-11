# Transaction Analysis - Basic Queries

## Q1001: "Show me the last 10 transactions on Solana"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - SLICE, LAST (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()

TRY:
  $block = getBlock(slot: $current_slot)
  $transactions = $block.transactions
CATCH RECOVERABLE:
  // Block might be skipped
  $current_slot = $current_slot - 1
  $block = getBlock(slot: $current_slot)
  $transactions = $block.transactions

$recent_txs = SLICE(collection: $transactions, start: 0, end: 10)
```

**Decision Point:** Check if we have enough transactions
  BRANCH A (enough txs in one block):
    $result = $recent_txs
  BRANCH B (need more blocks):
    $all_txs = []
    FOR $i IN 0..5:
      $block = getBlock(slot: $current_slot - $i)
      $all_txs = APPEND(array: $all_txs, item: $block.transactions)
      BREAK IF COUNT(collection: $all_txs) >= 10
    $result = SLICE(collection: $all_txs, start: 0, end: 10)

**Action:**
RETURN {
  transactions: $result,
  count: COUNT(collection: $result),
  confidence: 100
}

---

## Q1002: "Get details for transaction 5j7s6NiJS3JAkvgkoc9wGpM8hBvPxYT6k8ezH"

**Expected Plan:**
[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
```ovsm
$signature = "5j7s6NiJS3JAkvgkoc9wGpM8hBvPxYT6k8ezH"

TRY:
  $tx = getTransaction(
    signature: $signature,
    maxSupportedTransactionVersion: 0
  )
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found or invalid signature")

$slot = $tx.slot
$fee = $tx.meta.fee
$success = $tx.meta.err == null
$compute_units = $tx.meta.computeUnitsConsumed
```

**Decision Point:** Analyze transaction result
  BRANCH A (success):
    $status = "succeeded"
    $instructions = MAP($tx.message.instructions, i => {
      program: $tx.message.accountKeys[i.programIdIndex],
      accounts: i.accounts
    })
  BRANCH B (failed):
    $status = "failed"
    $error_message = $tx.meta.err
    $logs = $tx.meta.logMessages

**Action:**
RETURN {
  signature: $signature,
  slot: $slot,
  status: $status,
  fee_lamports: $fee,
  compute_units: $compute_units,
  instruction_count: COUNT(collection: $tx.message.instructions),
  confidence: 100
}

---

## Q1003: "How many transactions has address 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv made?"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - COUNT, SUM (Data Processing)

**Main Branch:**
```ovsm
$address = "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"
$all_signatures = []
$before = null

// Paginate through all transactions
LOOP WHILE true:
  TRY:
    $batch = getSignaturesForAddress(
      address: $address,
      limit: 1000,
      before: $before
    )
  CATCH FATAL:
    BREAK
  
  GUARD COUNT(collection: $batch) > 0 ELSE BREAK
  
  $all_signatures = APPEND(array: $all_signatures, item: $batch)
  $before = LAST(collection: $batch).signature
  
  // Prevent infinite loops
  BREAK IF COUNT(collection: $all_signatures) > 100000

$total_count = SUM(collection: MAP($all_signatures, batch => COUNT(collection: batch)))
```

**Decision Point:** Analyze transaction volume
  BRANCH A (high volume > 10000):
    $activity_level = "highly active"
    $confidence = 95  // May have missed some due to pagination limits
  BRANCH B (medium volume 1000-10000):
    $activity_level = "moderately active"
    $confidence = 99
  BRANCH C (low volume < 1000):
    $activity_level = "low activity"
    $confidence = 100

**Action:**
RETURN {
  address: $address,
  total_transactions: $total_count,
  activity_level: $activity_level,
  confidence: $confidence,
  caveats: ["Based on getSignaturesForAddress pagination"]
}

---

## Q1004: "Show me all failed transactions in the last 100 slots"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, COUNT, MAP (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$start_slot = $current_slot - 100
$failed_txs = []

FOR $slot IN $start_slot..$current_slot:
  TRY:
    $block = getBlock(slot: $slot)
    
    // Filter for failed transactions
    $failed_in_block = FILTER(
      collection: $block.transactions,
      predicate: tx => tx.meta.err != null
    )
    
    IF COUNT(collection: $failed_in_block) > 0:
      $failed_txs = APPEND(array: $failed_txs, item: $failed_in_block)
  CATCH RECOVERABLE:
    // Block might be skipped, continue
    CONTINUE
```

**Decision Point:** Analyze failure patterns
  BRANCH A (many similar errors):
    $grouped = GROUP_BY(collection: $failed_txs, key: tx => tx.meta.err)
    $analysis = "Detected error patterns"
  BRANCH B (diverse errors):
    $analysis = "Various error types"
  BRANCH C (no failures):
    $analysis = "No failed transactions in range"

**Action:**
RETURN {
  slot_range: [$start_slot, $current_slot],
  failed_count: COUNT(collection: FLATTEN(collection: $failed_txs)),
  analysis: $analysis,
  confidence: 100
}

---

## Q1005: "What are the average transaction fees in the last 1000 transactions?"

**Expected Plan:**
[TIME: ~12s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MEAN, MEDIAN, STDDEV, MAP, FLATTEN (Statistical)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$all_txs = []
$slots_checked = 0

// Collect 1000 transactions
LOOP WHILE COUNT(collection: FLATTEN(collection: $all_txs)) < 1000 AND $slots_checked < 500:
  $slot = $current_slot - $slots_checked
  
  TRY:
    $block = getBlock(slot: $slot)
    $all_txs = APPEND(array: $all_txs, item: $block.transactions)
  CATCH RECOVERABLE:
    // Skip missing blocks
    SKIP
  
  $slots_checked = $slots_checked + 1

$transactions = SLICE(
  collection: FLATTEN(collection: $all_txs),
  start: 0,
  end: 1000
)

$fees = MAP(collection: $transactions, fn: tx => tx.meta.fee)
$mean_fee = MEAN(data: $fees)
$median_fee = MEDIAN(data: $fees)
$stddev = STDDEV(data: $fees)
```

**Decision Point:** Analyze fee distribution
  BRANCH A (high variance, $stddev / $mean_fee > 0.5):
    $distribution = "high variance"
    $outliers = FILTER(
      collection: $fees,
      predicate: fee => ABS(fee - $mean_fee) > 2 * $stddev
    )
    $note = "Significant fee variation detected"
  BRANCH B (normal distribution):
    $distribution = "normal"
    $note = "Consistent fee patterns"

**Action:**
RETURN {
  sample_size: COUNT(collection: $fees),
  mean_fee_lamports: $mean_fee,
  mean_fee_sol: $mean_fee / LAMPORTS_PER_SOL,
  median_fee_lamports: $median_fee,
  stddev: $stddev,
  distribution: $distribution,
  slots_analyzed: $slots_checked,
  confidence: 95,
  note: $note
}

---

## Q1006: "Find all transactions that called the Token program"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - COUNT, SLICE (Data Processing)

**Main Branch:**
```ovsm
CONST TOKEN_PROGRAM = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

$signatures = getSignaturesForAddress(
  address: TOKEN_PROGRAM,
  limit: 100
)

$recent_token_txs = MAP(
  collection: $signatures,
  fn: sig => {
    signature: sig.signature,
    slot: sig.slot,
    err: sig.err
  }
)
```

**Decision Point:** Sample for instruction analysis
  BRANCH A (analyze instruction types):
    $samples = SLICE(collection: $signatures, start: 0, end: 10)
    
    $instruction_types = MAP(
      collection: $samples,
      fn: sig => {
        $tx = getTransaction(signature: sig.signature)
        $instructions = FILTER(
          collection: $tx.message.instructions,
          predicate: inst => $tx.message.accountKeys[inst.programIdIndex] == TOKEN_PROGRAM
        )
        RETURN MAP($instructions, i => i.data[0])  // First byte is instruction discriminator
      }
    )
  BRANCH B (count only):
    $instruction_types = null

**Action:**
RETURN {
  token_program: TOKEN_PROGRAM,
  recent_transactions: COUNT(collection: $signatures),
  transactions: $recent_token_txs,
  instruction_samples: $instruction_types,
  confidence: 100
}

---

## Q1007: "What's the average time between transactions in the last hour?"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - COUNT, SUM, MEAN (Statistical)

**Main Branch:**
```ovsm
$current_slot = getSlot()
// Approximate: 400ms per slot, 9000 slots per hour
$slots_per_hour = 9000
$start_slot = $current_slot - $slots_per_hour

$blocks = []
$total_txs = 0

FOR $slot IN $start_slot..$current_slot:
  TRY:
    $block = getBlock(slot: $slot)
    $blocks = APPEND(array: $blocks, item: $block)
    $total_txs = $total_txs + COUNT(collection: $block.transactions)
  CATCH RECOVERABLE:
    CONTINUE

GUARD COUNT(collection: $blocks) > 0 ELSE
  RETURN ERROR(message: "No blocks found in time range")

$first_block = $blocks[0]
$last_block = $blocks[COUNT(collection: $blocks) - 1]

$time_span_seconds = $last_block.blockTime - $first_block.blockTime
$avg_interval = $time_span_seconds / $total_txs
```

**Decision Point:** Validate data quality
  BRANCH A (complete data):
    $confidence = 95
    $note = "Complete block data"
  BRANCH B (missing blocks > 10% expected):
    $expected_blocks = $slots_per_hour
    $actual_blocks = COUNT(collection: $blocks)
    $missing_ratio = ($expected_blocks - $actual_blocks) / $expected_blocks
    
    IF $missing_ratio > 0.1:
      $confidence = 80
      $note = "Some blocks missing, estimate adjusted"

**Action:**
RETURN {
  time_range_seconds: $time_span_seconds,
  total_transactions: $total_txs,
  average_interval_seconds: $avg_interval,
  average_interval_ms: $avg_interval * 1000,
  blocks_analyzed: COUNT(collection: $blocks),
  confidence: $confidence,
  note: $note
}

---

## Q1008: "Parse the instructions in transaction ABC123"

**Expected Plan:**
[TIME: ~2s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, FLATTEN (Data Processing)

**Main Branch:**
```ovsm
$signature = "ABC123"

TRY:
  $tx = getTransaction(
    signature: $signature,
    maxSupportedTransactionVersion: 0
  )
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$instructions = MAP(
  collection: $tx.message.instructions,
  fn: inst => {
    program_id: $tx.message.accountKeys[inst.programIdIndex],
    accounts: MAP(inst.accounts, idx => $tx.message.accountKeys[idx]),
    data: inst.data
  }
)
```

**Decision Point:** Parse inner instructions (CPIs)
  BRANCH A (has inner instructions):
    $inner = FLATTEN(
      collection: MAP(
        collection: $tx.meta.innerInstructions,
        fn: inner_group => MAP(
          collection: inner_group.instructions,
          fn: inst => {
            program_id: $tx.message.accountKeys[inst.programIdIndex],
            accounts: MAP(inst.accounts, idx => $tx.message.accountKeys[idx]),
            depth: "CPI"
          }
        )
      )
    )
    $all_instructions = APPEND(array: $instructions, item: $inner)
  BRANCH B (no CPIs):
    $all_instructions = $instructions

**Action:**
RETURN {
  signature: $signature,
  instruction_count: COUNT(collection: $instructions),
  inner_instruction_count: COUNT(collection: $tx.meta.innerInstructions),
  instructions: $all_instructions,
  confidence: 95,
  caveats: ["Raw instruction data - program-specific parsing needed"]
}

---

## Q1009: "Find all multisig transactions from address XYZ"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - FILTER, COUNT, MAP (Data Processing)

**Main Branch:**
```ovsm
$address = "XYZ"

$signatures = getSignaturesForAddress(
  address: $address,
  limit: 1000
)

// Multisig transactions have multiple signatures
$multisig_candidates = []

FOR $sig IN $signatures:
  TRY:
    $tx = getTransaction(signature: $sig.signature)
    
    // Check signature count
    IF COUNT(collection: $tx.transaction.signatures) > 1:
      $multisig_candidates = APPEND(
        array: $multisig_candidates,
        item: {
          signature: $sig.signature,
          signer_count: COUNT(collection: $tx.transaction.signatures),
          slot: $sig.slot
        }
      )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze multisig type
  BRANCH A (SPL Multisig detected):
    CONST MULTISIG_PROGRAM = "msig1111111111111111111111111111111111111111"
    $spl_multisig = FILTER(
      collection: $multisig_candidates,
      predicate: tx => {
        $full_tx = getTransaction(signature: tx.signature)
        ANY(
          collection: $full_tx.message.instructions,
          predicate: inst => $full_tx.message.accountKeys[inst.programIdIndex] == MULTISIG_PROGRAM
        )
      }
    )
    $type = "SPL Multisig"
  BRANCH B (Multiple signers):
    $type = "Multi-signer transaction"

**Action:**
RETURN {
  address: $address,
  multisig_count: COUNT(collection: $multisig_candidates),
  multisig_transactions: $multisig_candidates,
  type: $type,
  confidence: 90,
  note: "Multisig detected by signature count"
}

---

## Q1010: "Show me the logs from transaction signature SIG123"

**Expected Plan:**
[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
```ovsm
$signature = "SIG123"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$logs = $tx.meta.logMessages
$success = $tx.meta.err == null
```

**Decision Point:** Parse log structure
  BRANCH A (success - parse invocations):
    $program_invocations = FILTER(
      collection: $logs,
      predicate: log => log CONTAINS "Program" AND log CONTAINS "invoke"
    )
    
    $program_returns = FILTER(
      collection: $logs,
      predicate: log => log CONTAINS "Program" AND log CONTAINS "success"
    )
    
    $analysis = {
      programs_invoked: COUNT(collection: $program_invocations),
      all_succeeded: true
    }
    
  BRANCH B (failure - identify error):
    $error_logs = FILTER(
      collection: $logs,
      predicate: log => log CONTAINS "failed" OR log CONTAINS "Error"
    )
    
    $analysis = {
      error_detected: true,
      error_location: FIND(
        collection: $logs,
        predicate: log => log CONTAINS "failed"
      )
    }

**Action:**
RETURN {
  signature: $signature,
  success: $success,
  log_count: COUNT(collection: $logs),
  logs: $logs,
  analysis: $analysis,
  confidence: 100
}

---

## Q1011: "Find all transactions that touched over 50 accounts"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT, MAP (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$large_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    // Filter for large transactions
    $candidates = FILTER(
      collection: $block.transactions,
      predicate: tx => COUNT(collection: tx.transaction.message.accountKeys) > 50
    )
    
    IF COUNT(collection: $candidates) > 0:
      FOR $candidate IN $candidates:
        $details = getTransaction(signature: $candidate.transaction.signatures[0])
        $large_txs = APPEND(
          array: $large_txs,
          item: {
            signature: $candidate.transaction.signatures[0],
            account_count: COUNT(collection: $details.transaction.message.accountKeys),
            slot: $slot
          }
        )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze account usage patterns
  BRANCH A (v0 transactions with lookup tables):
    $v0_txs = FILTER(
      collection: $large_txs,
      predicate: tx => {
        $full_tx = getTransaction(signature: tx.signature)
        $full_tx.version == 0
      }
    )
    $analysis = "Using address lookup tables"
  BRANCH B (legacy large transactions):
    $analysis = "Legacy transaction format"

**Action:**
RETURN {
  large_transaction_count: COUNT(collection: $large_txs),
  transactions: $large_txs,
  slots_searched: 100,
  analysis: $analysis,
  confidence: 95
}

---

## Q1012: "What's the most expensive transaction in the last 1000 blocks?"

**Expected Plan:**
[TIME: ~25s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - MAX_BY, MAP, FLATTEN (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$start_slot = $current_slot - 1000
$all_txs = []

FOR $slot IN $start_slot..$current_slot:
  TRY:
    $block = getBlock(slot: $slot)
    
    $txs_with_fees = MAP(
      collection: $block.transactions,
      fn: tx => {
        signature: tx.transaction.signatures[0],
        fee: tx.meta.fee,
        slot: $slot
      }
    )
    
    $all_txs = APPEND(array: $all_txs, item: $txs_with_fees)
  CATCH RECOVERABLE:
    CONTINUE

$flattened = FLATTEN(collection: $all_txs)

$most_expensive = MAX_BY(
  collection: $flattened,
  key: tx => tx.fee
)
```

**Decision Point:** Analyze why fee is high
  BRANCH A (get detailed analysis):
    $full_tx = getTransaction(signature: $most_expensive.signature)
    
    $compute_units = $full_tx.meta.computeUnitsConsumed
    $instruction_count = COUNT(collection: $full_tx.message.instructions)
    
    $analysis = {
      compute_units: $compute_units,
      instructions: $instruction_count,
      fee_per_compute: $most_expensive.fee / $compute_units
    }
  BRANCH B (simple return):
    $analysis = null

**Action:**
RETURN {
  signature: $most_expensive.signature,
  fee_lamports: $most_expensive.fee,
  fee_sol: $most_expensive.fee / LAMPORTS_PER_SOL,
  slot: $most_expensive.slot,
  blocks_searched: 1000,
  analysis: $analysis,
  confidence: 100
}

---

## Q1013: "Show me all versioned transactions (v0) in the last 100 blocks"

**Expected Plan:**
[TIME: ~12s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, COUNT, MAP (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$start_slot = $current_slot - 100
$v0_txs = []
$total_txs = 0

FOR $slot IN $start_slot..$current_slot:
  TRY:
    $block = getBlock(
      slot: $slot,
      maxSupportedTransactionVersion: 0
    )
    
    $total_txs = $total_txs + COUNT(collection: $block.transactions)
    
    // Check for v0 transactions
    FOR $tx IN $block.transactions:
      TRY:
        IF $tx.version == 0:
          $v0_txs = APPEND(
            array: $v0_txs,
            item: {
              signature: $tx.transaction.signatures[0],
              slot: $slot
            }
          )
      CATCH:
        // Legacy transaction, skip
        CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Calculate adoption metrics
  BRANCH A (has v0 transactions):
    $v0_count = COUNT(collection: $v0_txs)
    $adoption_percentage = ($v0_count / $total_txs) * 100
    $analysis = "Versioned transaction adoption: " + $adoption_percentage + "%"
  BRANCH B (no v0 transactions):
    $analysis = "No versioned transactions found"

**Action:**
RETURN {
  v0_transaction_count: COUNT(collection: $v0_txs),
  total_transactions: $total_txs,
  adoption_percentage: $adoption_percentage,
  blocks_searched: 100,
  transactions: $v0_txs,
  analysis: $analysis,
  confidence: 100
}

---

## Q1014: "What percentage of transactions use priority fees?"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - COUNT, FILTER (Data Processing)

**Main Branch:**
```ovsm
CONST COMPUTE_BUDGET_PROGRAM = "ComputeBudget111111111111111111111111111111"

$current_slot = getSlot()
$sample_blocks = 100
$transactions_with_priority = 0
$total_transactions = 0

FOR $i IN 0..$sample_blocks:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    $total_transactions = $total_transactions + COUNT(collection: $block.transactions)
    
    FOR $tx IN $block.transactions:
      // Check for compute budget instructions
      $has_priority_fee = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst => 
          $tx.transaction.message.accountKeys[inst.programIdIndex] == COMPUTE_BUDGET_PROGRAM
      )
      
      IF $has_priority_fee:
        $transactions_with_priority = $transactions_with_priority + 1
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Calculate percentage
  BRANCH A (has data):
    $percentage = ($transactions_with_priority / $total_transactions) * 100
    $confidence = 95
  BRANCH B (no data):
    RETURN ERROR(message: "Insufficient data")

**Action:**
RETURN {
  sample_blocks: $sample_blocks,
  total_transactions: $total_transactions,
  transactions_with_priority_fees: $transactions_with_priority,
  adoption_percentage: $percentage,
  confidence: $confidence,
  note: "Based on presence of ComputeBudget instructions"
}

---

## Q1015: "Which transaction consumed the most compute units today?"

**Expected Plan:**
[TIME: ~30s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - MAX_BY, NOW (Utilities)

**Main Branch:**
```ovsm
$current_time = NOW()
$current_slot = getSlot()

// Estimate slots since midnight (400ms per slot)
$seconds_today = $current_time % 86400  // Seconds since midnight
$slots_today = $seconds_today * 2.5  // 400ms = 0.4s per slot
$start_slot = $current_slot - $slots_today

$max_compute_tx = null
$max_compute = 0

FOR $slot IN $start_slot..$current_slot:
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $compute = $tx.meta.computeUnitsConsumed
      
      IF $compute > $max_compute:
        $max_compute = $compute
        $max_compute_tx = {
          signature: $tx.transaction.signatures[0],
          compute_units: $compute,
          slot: $slot
        }
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze compute usage
  BRANCH A (extremely high > 1000000):
    CONST COMPUTE_LIMIT = 1400000
    $percentage = ($max_compute / COMPUTE_LIMIT) * 100
    $analysis = "Very high compute usage: " + $percentage + "% of limit"
  BRANCH B (normal):
    $analysis = "Normal compute usage"

**Action:**
RETURN {
  signature: $max_compute_tx.signature,
  compute_units: $max_compute,
  slot: $max_compute_tx.slot,
  slots_searched: $slots_today,
  analysis: $analysis,
  confidence: 90,
  caveats: ["Approximate slot calculation for 'today'"]
}

---

## Q1016: "Find all atomic transactions (succeeded or all failed)"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, ALL (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$atomic_txs = []

FOR $i IN 0..50:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      // In Solana, ALL transactions are atomic by design
      // Either all instructions succeed or all fail (rollback)
      $is_atomic = true
      
      $atomic_txs = APPEND(
        array: $atomic_txs,
        item: {
          signature: $tx.transaction.signatures[0],
          success: $tx.meta.err == null,
          slot: $slot,
          is_atomic: $is_atomic
        }
      )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Categorize by outcome
  BRANCH A (all succeeded):
    $succeeded = FILTER(
      collection: $atomic_txs,
      predicate: tx => tx.success == true
    )
  BRANCH B (all failed):
    $failed = FILTER(
      collection: $atomic_txs,
      predicate: tx => tx.success == false
    )

**Action:**
RETURN {
  atomic_transactions: $atomic_txs,
  succeeded_count: COUNT(collection: $succeeded),
  failed_count: COUNT(collection: $failed),
  confidence: 100,
  note: "All Solana transactions are atomic by design"
}

---

## Q1017: "What's the median transaction confirmation time?"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MEDIAN, MAP (Statistical)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$confirmation_times = []

// Estimate confirmation time based on slot difference from recent blockhash
FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    $block_time = $block.blockTime
    
    FOR $tx IN $block.transactions:
      // Approximate: assume tx was submitted shortly before inclusion
      // This is an estimate without mempool data
      $estimated_confirmation = 1  // Most txs confirm within 1 slot
      $confirmation_times = APPEND(array: $confirmation_times, item: $estimated_confirmation)
  CATCH RECOVERABLE:
    CONTINUE

$median_confirmation = MEDIAN(data: $confirmation_times)
$median_seconds = $median_confirmation * 0.4  // 400ms per slot
```

**Decision Point:** Data availability
  BRANCH A (have mempool data):
    // Would use actual submission timestamps
    $confidence = 95
    $note = "Actual confirmation times from mempool"
  BRANCH B (estimate only):
    $confidence = 80
    $note = "Estimated - actual mempool data unavailable"

**Action:**
RETURN {
  median_confirmation_slots: $median_confirmation,
  median_confirmation_seconds: $median_seconds,
  sample_size: COUNT(collection: $confirmation_times),
  confidence: $confidence,
  caveats: [$note, "Requires mempool monitoring for precise measurements"]
}

---

## Q1018: "Find duplicate transactions (same signature attempted twice)"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - GROUP_BY, FILTER (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$all_signatures = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    $signatures = MAP(
      collection: $block.transactions,
      fn: tx => {
        signature: tx.transaction.signatures[0],
        slot: $slot
      }
    )
    
    $all_signatures = APPEND(array: $all_signatures, item: $signatures)
  CATCH RECOVERABLE:
    CONTINUE

$flattened = FLATTEN(collection: $all_signatures)

// Group by signature to find duplicates
$grouped = GROUP_BY(
  collection: $flattened,
  key: tx => tx.signature
)
```

**Decision Point:** Identify collision type
  BRANCH A (found duplicates):
    $duplicates = FILTER(
      collection: $grouped,
      predicate: group => COUNT(collection: group.value) > 1
    )
    
    IF COUNT(collection: $duplicates) > 0:
      $result = "Duplicate signatures detected (extremely rare!)"
      $analysis = "Potential hash collision or replay attempt"
    ELSE:
      $result = "No duplicate signatures"
      $analysis = "Transaction signatures are unique"
  BRANCH B (no duplicates - expected):
    $result = "No duplicate signatures"
    $analysis = "Normal - signatures should be unique"

**Action:**
RETURN {
  transactions_checked: COUNT(collection: $flattened),
  duplicate_count: COUNT(collection: $duplicates),
  duplicates: $duplicates,
  result: $result,
  analysis: $analysis,
  confidence: 100
}

---

## Q1019: "Show me all transactions with logs containing 'Error'"

**Expected Plan:**
[TIME: ~20s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$error_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $signature = $tx.transaction.signatures[0]
      
      TRY:
        $full_tx = getTransaction(signature: $signature)
        $logs = $full_tx.meta.logMessages
        
        // Check if any log contains "Error"
        $has_error = ANY(
          collection: $logs,
          predicate: log => log CONTAINS "Error" OR log CONTAINS "failed"
        )
        
        IF $has_error:
          $error_txs = APPEND(
            array: $error_txs,
            item: {
              signature: $signature,
              slot: $slot,
              logs: FILTER(
                collection: $logs,
                predicate: log => log CONTAINS "Error" OR log CONTAINS "failed"
              )
            }
          )
      CATCH RECOVERABLE:
        CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Categorize errors
  BRANCH A (multiple error types):
    $error_patterns = GROUP_BY(
      collection: $error_txs,
      key: tx => tx.logs[0]  // Group by first error log
    )
    $analysis = "Multiple error types detected"
  BRANCH B (single error pattern):
    $analysis = "Common error pattern"

**Action:**
RETURN {
  error_transaction_count: COUNT(collection: $error_txs),
  transactions: $error_txs,
  blocks_searched: 100,
  analysis: $analysis,
  confidence: 100
}

---

## Q1020: "Get the transaction size in bytes for signature XYZ"

**Expected Plan:**
[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - COUNT (Data Processing)

**Main Branch:**
```ovsm
$signature = "XYZ"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Calculate approximate size
$signature_count = COUNT(collection: $tx.transaction.signatures)
$signature_bytes = $signature_count * 64  // Each signature is 64 bytes

$account_count = COUNT(collection: $tx.transaction.message.accountKeys)
$account_bytes = $account_count * 32  // Each pubkey is 32 bytes

$instruction_count = COUNT(collection: $tx.transaction.message.instructions)

// Approximate instruction data size
$instruction_data_bytes = SUM(
  collection: MAP(
    collection: $tx.transaction.message.instructions,
    fn: inst => COUNT(collection: inst.data)
  )
)

$total_size = $signature_bytes + $account_bytes + $instruction_data_bytes + 100  // +overhead
```

**Decision Point:** Check transaction version
  BRANCH A (v0 with lookup tables):
    $has_lookup_tables = $tx.version == 0
    $note = "Versioned transaction - uses address lookup tables"
  BRANCH B (legacy):
    $note = "Legacy transaction format"

**Action:**
RETURN {
  signature: $signature,
  approximate_size_bytes: $total_size,
  signature_bytes: $signature_bytes,
  account_bytes: $account_bytes,
  instruction_data_bytes: $instruction_data_bytes,
  signature_count: $signature_count,
  account_count: $account_count,
  instruction_count: $instruction_count,
  note: $note,
  confidence: 100,
  caveats: ["Approximate calculation - actual size may vary slightly"]
}
