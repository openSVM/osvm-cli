# Transaction Analysis - Basic Questions (Q1-Q100)

**Category:** Transaction Analysis
**Difficulty:** Basic
**Focus:** Simple transaction lookups, fee calculations, instruction counting

---

## Q1: "What is the total fee paid for transaction signature 5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = "5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$fee_lamports = $tx.meta.fee
$fee_sol = $fee_lamports / LAMPORTS_PER_SOL

**Action:**
RETURN {
  signature: $signature,
  fee_lamports: $fee_lamports,
  fee_sol: $fee_sol,
  confidence: 100
}

---

## Q2: "How many instructions are in transaction 5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - COUNT (Data Processing)

**Main Branch:**
$signature = "5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx"

$tx = getTransaction(signature: $signature)

GUARD $tx != null ELSE RETURN ERROR(message: "Transaction not found")

$instruction_count = COUNT(collection: $tx.message.instructions)
$inner_instruction_count = COUNT(collection: $tx.meta.innerInstructions)
$total_instructions = $instruction_count + $inner_instruction_count

**Action:**
RETURN {
  signature: $signature,
  top_level_instructions: $instruction_count,
  inner_instructions: $inner_instruction_count,
  total_instructions: $total_instructions,
  confidence: 100
}

---

## Q3: "Which programs were invoked in transaction 5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, UNIQUE (Data Processing)

**Main Branch:**
$signature = "5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx"

$tx = getTransaction(signature: $signature)

// Extract all program IDs from instructions
$program_ids = MAP(
  collection: $tx.message.instructions,
  fn: inst => inst.programId
)

// Get unique programs
$unique_programs = UNIQUE(collection: $program_ids)

**Action:**
RETURN {
  signature: $signature,
  programs_invoked: $unique_programs,
  total_programs: COUNT(collection: $unique_programs),
  confidence: 100
}

---

## Q4: "Was transaction 5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx successful or did it fail?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = "5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx"

$tx = getTransaction(signature: $signature)
$error = $tx.meta.err

**Decision Point:** Check transaction status
  BRANCH A ($error == null):
    $status = "success"
    $error_msg = null
  BRANCH B ($error != null):
    $status = "failed"
    $error_msg = $error

**Action:**
RETURN {
  signature: $signature,
  status: $status,
  error: $error_msg,
  confidence: 100
}

---

## Q5: "How much compute units did transaction 5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx consume?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = "5J8sFH3kGxNy2Bbv7vPo8hKqM9dRT4yL5KzPr6DqM2Wx"

$tx = getTransaction(signature: $signature)

$compute_consumed = $tx.meta.computeUnitsConsumed
$percentage_of_max = ($compute_consumed / MAX_COMPUTE_UNITS) * 100

**Decision Point:** Check compute usage level
  BRANCH A ($compute_consumed < 100000):
    $usage_level = "low"
  BRANCH B ($compute_consumed >= 100000 AND $compute_consumed < 500000):
    $usage_level = "moderate"
  BRANCH C ($compute_consumed >= 500000):
    $usage_level = "high"

**Action:**
RETURN {
  signature: $signature,
  compute_units_consumed: $compute_consumed,
  percentage_of_max: $percentage_of_max,
  usage_level: $usage_level,
  confidence: 100
}

---

## Q6: "Show me the last 10 transactions on Solana"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - SLICE, FLATTEN (Data Processing)

**Main Branch:**
$current_slot = getSlot()
$block = getBlock(slot: $current_slot)

$transactions = $block.transactions

**Decision Point:** Check if block has enough transactions
  BRANCH A (COUNT($transactions) >= 10):
    $last_10 = SLICE(array: $transactions, start: 0, end: 10)
  BRANCH B (COUNT($transactions) < 10):
    // Need to fetch previous block too
    $prev_block = getBlock(slot: $current_slot - 1)
    $combined = APPEND(array: $transactions, item: $prev_block.transactions)
    $last_10 = SLICE(array: FLATTEN($combined), start: 0, end: 10)

**Action:**
RETURN {
  transactions: MAP($last_10, tx => tx.signature),
  count: COUNT(collection: $last_10),
  confidence: 95
}

---

## Q7: "What are the average transaction fees in the last 100 transactions?"

**Expected Plan:**

[TIME: ~10s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FLATTEN, MEAN, MEDIAN, STDDEV (Data Processing & Statistical)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..10:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))
$fees = MAP(collection: $all_txs, fn: tx => tx.meta.fee)

// Take first 100
$sample_fees = SLICE(array: $fees, start: 0, end: 100)

$mean_fee = MEAN(data: $sample_fees)
$median_fee = MEDIAN(data: $sample_fees)
$stddev = STDDEV(data: $sample_fees)

**Decision Point:** Check fee distribution
  BRANCH A ($stddev / $mean_fee < 0.5):
    // Normal distribution, use mean
    $representative_fee = $mean_fee
    $note = "Normal distribution"
  BRANCH B ($stddev / $mean_fee >= 0.5):
    // High variance, use median
    $representative_fee = $median_fee
    $note = "High variance detected"

**Action:**
RETURN {
  average_fee: $representative_fee,
  mean: $mean_fee,
  median: $median_fee,
  stddev: $stddev,
  sample_size: COUNT(collection: $sample_fees),
  note: $note,
  confidence: 90
}

---

## Q8: "Find all failed transactions in the last 50 blocks"

**Expected Plan:**

[TIME: ~30s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FILTER, FLATTEN (Data Processing)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..50:
  TRY:
    $block = getBlock(slot: $current_slot - $i)
    $blocks = APPEND(array: $blocks, item: $block)
  CATCH:
    CONTINUE // Skip missing blocks

$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))

$failed_txs = FILTER(
  collection: $all_txs,
  predicate: tx => tx.meta.err != null
)

**Decision Point:** Check if failed transactions found
  BRANCH A (COUNT($failed_txs) > 0):
    $failure_rate = (COUNT($failed_txs) / COUNT($all_txs)) * 100
    $has_failures = true
  BRANCH B (COUNT($failed_txs) == 0):
    $failure_rate = 0
    $has_failures = false

**Action:**
RETURN {
  failed_transactions: MAP($failed_txs, tx => {
    signature: tx.signature,
    error: tx.meta.err,
    slot: tx.slot
  }),
  total_failed: COUNT(collection: $failed_txs),
  total_transactions: COUNT(collection: $all_txs),
  failure_rate_percent: $failure_rate,
  confidence: 95
}

---

## Q9: "What is the most expensive transaction in the last 100 blocks?"

**Expected Plan:**

[TIME: ~45s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FLATTEN, MAX_BY (Data Processing)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..100:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))

$most_expensive = MAX_BY(
  collection: $all_txs,
  by: tx => tx.meta.fee
)

GUARD $most_expensive != null ELSE
  RETURN ERROR(message: "No transactions found")

$fee_sol = $most_expensive.meta.fee / LAMPORTS_PER_SOL

**Decision Point:** Analyze why fee is high
  BRANCH A ($most_expensive.meta.fee > 100000):
    // Very high fee - investigate priority fees
    $reason = "Likely used priority fees for fast inclusion"
  BRANCH B ($most_expensive.meta.fee <= 100000):
    // Normal range
    $reason = "Standard transaction fee"

**Action:**
RETURN {
  signature: $most_expensive.signature,
  fee_lamports: $most_expensive.meta.fee,
  fee_sol: $fee_sol,
  slot: $most_expensive.slot,
  reason: $reason,
  confidence: 90
}

---

## Q10: "Count how many transactions used priority fees in the last 20 blocks"

**Expected Plan:**

[TIME: ~15s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FILTER, FLATTEN, COUNT, ANY (Data Processing)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..20:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))

// Check for SetComputeUnitPrice instruction (priority fee indicator)
$txs_with_priority = FILTER(
  collection: $all_txs,
  predicate: tx => ANY(
    collection: tx.message.instructions,
    predicate: inst => inst.programId == "ComputeBudget111111111111111111111111111111"
  )
)

$total_txs = COUNT(collection: $all_txs)
$priority_txs = COUNT(collection: $txs_with_priority)
$percentage = ($priority_txs / $total_txs) * 100

**Decision Point:** Assess priority fee adoption
  BRANCH A ($percentage > 50):
    $adoption_level = "high"
  BRANCH B ($percentage >= 20 AND $percentage <= 50):
    $adoption_level = "moderate"
  BRANCH C ($percentage < 20):
    $adoption_level = "low"

**Action:**
RETURN {
  total_transactions: $total_txs,
  transactions_with_priority_fees: $priority_txs,
  percentage: $percentage,
  adoption_level: $adoption_level,
  blocks_analyzed: 20,
  confidence: 85
}

---

[Continue with Q11-Q100 following the same proper OVSM format...]

## Q11: "Show me all transactions that called the Token program in the last block"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
$current_slot = getSlot()
$block = getBlock(slot: $current_slot)

$token_txs = FILTER(
  collection: $block.transactions,
  predicate: tx => ANY(
    collection: tx.message.instructions,
    predicate: inst => inst.programId == TOKEN_PROGRAM
  )
)

**Action:**
RETURN {
  slot: $current_slot,
  token_transactions: MAP($token_txs, tx => tx.signature),
  count: COUNT(collection: $token_txs),
  total_transactions: COUNT(collection: $block.transactions),
  confidence: 100
}

---

## Q12: "What was the confirmation time for transaction 5J8...?"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - NOW (Utility)

**Main Branch:**
$signature = INPUT(prompt: "transaction_signature")

$tx = getTransaction(signature: $signature)

GUARD $tx.blockTime != null ELSE
  RETURN ERROR(message: "Block time not available")

// Note: We can only get confirmation time if we know submission time
// Here we estimate based on blockhash age

$confirmed_at = $tx.blockTime
$current_time = NOW()
$blocks_ago = $current_time - $confirmed_at

**Decision Point:** Can we determine actual confirmation time?
  BRANCH A (have submission data):
    // If we tracked submission (not usually available)
    $confirmation_time = $confirmed_at - $submission_time
    $confidence = 95
  BRANCH B (no submission data):
    // Can only show when it was confirmed, not latency
    $confirmation_time = null
    $confidence = 60

**Action:**
RETURN {
  signature: $signature,
  confirmed_at_timestamp: $confirmed_at,
  seconds_ago: $blocks_ago,
  confirmation_latency: $confirmation_time,
  note: "Submission time not available, showing confirmation timestamp only",
  confidence: $confidence
}

---

[Questions Q13-Q100 would continue with similar proper OVSM format covering topics like:
- Q13-20: Transaction size analysis
- Q21-30: CPI depth and complexity
- Q31-40: Account interaction patterns
- Q41-50: Compute unit usage
- Q51-60: Transaction versioning (v0 vs legacy)
- Q61-70: Blockhash and nonce analysis
- Q71-80: Multi-signature transactions
- Q81-90: Transaction logs and error parsing
- Q91-100: Basic transaction statistics]
