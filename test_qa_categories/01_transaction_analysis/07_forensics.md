# Transaction Analysis - Forensics Level

## Q1: "My transaction failed with 'custom program error: 0x1'. What went wrong and how do I fix it?"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, FIND, MAP (Data Processing)
  - CONTAINS, SPLIT (String operations)
  - getProgramAccounts (Solana RPC)

**Main Branch:**
```ovsm
$signature = INPUT(prompt: "Enter failed transaction signature")

TRY:
  $tx = getTransaction(signature: $signature, maxSupportedTransactionVersion: 0)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found on chain")

// Verify it failed
GUARD $tx.meta.err != null ELSE
  RETURN ERROR(message: "Transaction succeeded - no debugging needed")

$error_code = $tx.meta.err
$logs = $tx.meta.logMessages
$instructions = $tx.transaction.message.instructions
$accounts = $tx.transaction.message.accountKeys

// Find which program failed
$program_errors = FILTER(
  collection: $logs,
  predicate: log => CONTAINS(log, "failed: custom program error")
)

$failed_program = null
FOR $log IN $program_errors:
  $parts = SPLIT(string: $log, delimiter: " ")
  $program_line = FIND(collection: $parts, predicate: p => CONTAINS(p, "Program"))
  IF $program_line != null THEN
    $failed_program = $parts[1]  // Program address is usually second element

// Extract error code
$error_hex = "0x1"
IF CONTAINS($error_code, "Custom") THEN
  $error_parts = SPLIT(string: $error_code, delimiter: ": ")
  $error_hex = LAST(collection: $error_parts)

// Analyze account states
$pre_balances = $tx.meta.preBalances
$post_balances = $tx.meta.postBalances
$balance_changes = []

FOR $i IN 0..COUNT(collection: $accounts):
  $change = $post_balances[$i] - $pre_balances[$i]
  IF $change != 0 THEN
    $balance_changes = APPEND(
      array: $balance_changes,
      item: {account: $accounts[$i], change_lamports: $change}
    )
```

**Decision Point:** Classify error type
  BRANCH A (CONTAINS($error_hex, "0x1") AND $failed_program != null):
    $error_type = "InsufficientFunds"
    $diagnosis = "Program needs more SOL for rent or operation"
    $solution = "Add ~0.01 SOL to the account: " + $failed_program
    $confidence = 90
    
  BRANCH B (CONTAINS($error_hex, "0x0")):
    $error_type = "InvalidInstruction"
    $diagnosis = "Instruction data format doesn't match program expectations"
    $solution = "Check if you're using the correct instruction encoding"
    $confidence = 85
    
  BRANCH C (CONTAINS($logs, "already in use")):
    $error_type = "AccountInUse"
    $diagnosis = "Account is being modified by another transaction"
    $solution = "Retry transaction after a few seconds"
    $confidence = 95
    
  BRANCH D (CONTAINS($logs, "InvalidAccountData")):
    $error_type = "AccountDataInvalid"
    $diagnosis = "Account state is corrupted or unexpected"
    $solution = "Verify account was initialized properly"
    $confidence = 80

**Action:**
RETURN {
  signature: $signature,
  error_code: $error_hex,
  error_type: $error_type,
  diagnosis: $diagnosis,
  solution: $solution,
  failed_program: $failed_program,
  relevant_logs: $program_errors,
  balance_changes: $balance_changes,
  compute_units_used: $tx.meta.computeUnitsConsumed,
  confidence: $confidence,
  caveats: [
    "Error codes vary by program - check program documentation",
    "Some programs use custom error numbering schemes"
  ]
}

---

## Q2: "Someone stole tokens from my wallet. Trace the theft path across multiple hops"

**Expected Plan:**
[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getTransaction, getSignaturesForAddress (Solana RPC)
  - FLATTEN, MAP, FILTER, UNIQUE (Data Processing)
  - ADD_NODE, ADD_EDGE, FIND_PATH (Knowledge Graph)

**Main Branch:**
```ovsm
$victim_wallet = INPUT(prompt: "Enter your wallet address")
$theft_signature = INPUT(prompt: "Enter the theft transaction signature")
$max_hops = 10

// Initialize knowledge graph for tracking flow
$graph = INIT_KNOWLEDGE_GRAPH()
$graph = ADD_NODE(graph: $graph, id: $victim_wallet, type: "victim", label: "Victim Wallet")

// Get theft transaction
$theft_tx = getTransaction(signature: $theft_signature, maxSupportedTransactionVersion: 0)

// Extract stolen amount and recipient
$pre_balances = $theft_tx.meta.preTokenBalances
$post_balances = $theft_tx.meta.postTokenBalances

$stolen_amount = 0
$thief_wallet = null

FOR $i IN 0..COUNT(collection: $post_balances):
  $pre = FIND(collection: $pre_balances, predicate: p => p.accountIndex == $post_balances[$i].accountIndex)
  IF $pre != null THEN
    $diff = $post_balances[$i].uiTokenAmount.uiAmount - $pre.uiTokenAmount.uiAmount
    IF $diff > 0 AND $post_balances[$i].owner != $victim_wallet THEN
      $stolen_amount = $diff
      $thief_wallet = $post_balances[$i].owner
      $graph = ADD_NODE(graph: $graph, id: $thief_wallet, type: "thief", label: "Initial Recipient")
      $graph = ADD_EDGE(
        graph: $graph,
        from: $victim_wallet,
        to: $thief_wallet,
        relationship: "stolen_transfer",
        amount: $stolen_amount,
        signature: $theft_signature
      )

GUARD $thief_wallet != null ELSE
  RETURN ERROR(message: "Could not identify theft recipient in transaction")

// Trace forward through multiple hops
$current_wallets = [$thief_wallet]
$visited = [$victim_wallet, $thief_wallet]
$hop_count = 0

WHILE $hop_count < $max_hops AND COUNT(collection: $current_wallets) > 0:
  $next_wallets = []
  
  FOR $wallet IN $current_wallets:
    TRY:
      // Get all outgoing transactions from this wallet
      $signatures = getSignaturesForAddress(address: $wallet, limit: 100)
      
      FOR $sig IN $signatures:
        // Only check transactions after the theft
        IF $sig.blockTime > $theft_tx.blockTime THEN
          $tx = getTransaction(signature: $sig.signature, maxSupportedTransactionVersion: 0)
          
          // Look for token transfers
          IF $tx.meta.postTokenBalances != null THEN
            $recipients = MAP(
              collection: $tx.meta.postTokenBalances,
              fn: balance => balance.owner
            )
            
            FOR $recipient IN $recipients:
              IF NOT CONTAINS($visited, $recipient) THEN
                $graph = ADD_NODE(
                  graph: $graph,
                  id: $recipient,
                  type: "intermediary",
                  label: "Hop " + ($hop_count + 1)
                )
                $graph = ADD_EDGE(
                  graph: $graph,
                  from: $wallet,
                  to: $recipient,
                  relationship: "forwarded",
                  signature: $sig.signature
                )
                $next_wallets = APPEND(array: $next_wallets, item: $recipient)
                $visited = APPEND(array: $visited, item: $recipient)
        
        SLEEP(duration: 100)  // Rate limiting
    CATCH RECOVERABLE:
      LOG("Failed to fetch transactions for " + $wallet)
  
  $current_wallets = $next_wallets
  $hop_count += 1

// Identify potential final destinations (likely exchanges/mixers)
$leaf_nodes = FILTER(
  collection: $visited,
  predicate: node => {
    $outgoing = FILTER($graph.edges, e => e.from == node)
    COUNT(collection: $outgoing) == 0 AND node != $victim_wallet
  }
)
```

**Decision Point:** Analyze theft pattern
  BRANCH A (COUNT(collection: $leaf_nodes) == 1 AND $hop_count <= 2):
    $pattern = "direct_transfer"
    $analysis = "Tokens sent directly to single destination - likely inexperienced thief"
    $recovery_chance = "medium"
    $confidence = 85
    
  BRANCH B (COUNT(collection: $leaf_nodes) > 5 AND $hop_count >= 3):
    $pattern = "sophisticated_laundering"
    $analysis = "Tokens split and mixed through multiple wallets - professional operation"
    $recovery_chance = "low"
    $confidence = 70
    
  BRANCH C ($hop_count >= 5):
    $pattern = "exchange_deposit"
    $analysis = "Tokens likely deposited to centralized exchange"
    $recovery_chance = "medium"
    $confidence = 75

**Action:**
RETURN {
  victim_wallet: $victim_wallet,
  stolen_amount: $stolen_amount,
  initial_thief: $thief_wallet,
  total_hops: $hop_count,
  unique_wallets: COUNT(collection: $visited),
  final_destinations: $leaf_nodes,
  pattern: $pattern,
  analysis: $analysis,
  recovery_chance: $recovery_chance,
  full_graph: $graph,
  recommendations: [
    "Report addresses to Solana ecosystem security channels",
    "Contact exchanges where tokens ended up",
    "File police report if amount is significant",
    "Check if thief wallet has other victims (pattern matching)"
  ],
  confidence: $confidence,
  caveats: [
    "May not capture all hops if tokens converted to different SPL tokens",
    "Some intermediary wallets might be innocent users",
    "CEX deposits are identifiable but require legal process to recover"
  ]
}

---

## Q3: "This transaction shows 0 SOL transferred but someone claims they paid me. Is this a scam?"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, MAP, ANY (Data Processing)
  - CONTAINS (String operations)

**Main Branch:**
```ovsm
$signature = INPUT(prompt: "Enter transaction signature")
$my_wallet = INPUT(prompt: "Enter your wallet address")

$tx = getTransaction(signature: $signature, maxSupportedTransactionVersion: 0)

// Check SOL balance changes
$accounts = $tx.transaction.message.accountKeys
$pre_balances = $tx.meta.preBalances
$post_balances = $tx.meta.postBalances

$my_account_index = null
FOR $i IN 0..COUNT(collection: $accounts):
  IF $accounts[$i] == $my_wallet THEN
    $my_account_index = $i
    BREAK

$sol_received = 0
IF $my_account_index != null THEN
  $sol_received = ($post_balances[$my_account_index] - $pre_balances[$my_account_index]) / 1000000000

// Check SPL token transfers
$token_transfers = []
IF $tx.meta.postTokenBalances != null THEN
  FOR $post IN $tx.meta.postTokenBalances:
    $pre = FIND(
      collection: $tx.meta.preTokenBalances,
      predicate: p => p.accountIndex == $post.accountIndex
    )
    
    IF $post.owner == $my_wallet THEN
      $amount_change = $post.uiTokenAmount.uiAmount
      IF $pre != null THEN
        $amount_change = $amount_change - $pre.uiTokenAmount.uiAmount
      
      IF $amount_change > 0 THEN
        $token_transfers = APPEND(
          array: $token_transfers,
          item: {
            mint: $post.mint,
            amount: $amount_change,
            decimals: $post.uiTokenAmount.decimals
          }
        )

// Check for memo program (scammers often use this)
$has_memo = ANY(
  collection: $tx.transaction.message.instructions,
  predicate: ix => CONTAINS($accounts[ix.programIdIndex], "Memo")
)

// Extract memo if present
$memo_text = null
IF $has_memo THEN
  $memo_logs = FILTER(
    collection: $tx.meta.logMessages,
    predicate: log => CONTAINS(log, "Program log: Memo")
  )
  IF COUNT(collection: $memo_logs) > 0 THEN
    $memo_text = $memo_logs[0]
```

**Decision Point:** Determine if scam
  BRANCH A ($sol_received == 0 AND COUNT(collection: $token_transfers) == 0 AND $has_memo == true):
    $verdict = "SCAM"
    $explanation = "This is a MEMO SCAM - no value was transferred, only a message"
    $risk_level = "high"
    $confidence = 99
    
  BRANCH B ($sol_received == 0 AND COUNT(collection: $token_transfers) > 0):
    $verdict = "LEGITIMATE_TOKEN_TRANSFER"
    $explanation = "No SOL but tokens were transferred - check token value"
    $risk_level = "low"
    $confidence = 95
    
  BRANCH C ($sol_received > 0):
    $verdict = "LEGITIMATE_SOL_TRANSFER"
    $explanation = "SOL was actually transferred to your wallet"
    $risk_level = "none"
    $confidence = 100
    
  BRANCH D ($my_account_index == null):
    $verdict = "NOT_YOUR_TRANSACTION"
    $explanation = "Your wallet is not involved in this transaction"
    $risk_level = "scam_attempt"
    $confidence = 100

**Action:**
RETURN {
  verdict: $verdict,
  explanation: $explanation,
  sol_received: $sol_received,
  token_transfers: $token_transfers,
  has_memo: $has_memo,
  memo_content: $memo_text,
  risk_level: $risk_level,
  warnings: [
    "Never trust transaction links from strangers",
    "Always verify in your own wallet, not someone else's link",
    "Memo programs can contain any text - they don't prove payment",
    "Check if your actual balance increased"
  ],
  confidence: $confidence
}

---

## Q4: "Find all failed transactions in the last 1000 blocks and identify common failure patterns"

**Expected Plan:**
[TIME: ~120s] [COST: ~0.01 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, MAP, FLATTEN, GROUP_BY, SORT_BY (Data Processing)
  - CONTAINS (String operations)
  - COUNT, SUM, PERCENTILE (Statistical)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$failed_txs = []
$error_patterns = {}
$blocks_analyzed = 0
$target_blocks = 1000

FOR $i IN 0..$target_blocks:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot, maxSupportedTransactionVersion: 0)
    $blocks_analyzed += 1
    
    // Extract failed transactions
    FOR $tx IN $block.transactions:
      IF $tx.meta.err != null THEN
        $error_type = JSON_STRINGIFY($tx.meta.err)
        $program_errors = FILTER(
          collection: $tx.meta.logMessages,
          predicate: log => CONTAINS(log, "failed")
        )
        
        $failed_txs = APPEND(
          array: $failed_txs,
          item: {
            signature: $tx.transaction.signatures[0],
            slot: $slot,
            error: $error_type,
            logs: $program_errors,
            compute_units: $tx.meta.computeUnitsConsumed,
            fee: $tx.meta.fee
          }
        )
        
        // Count error types
        IF $error_patterns[$error_type] == null THEN
          $error_patterns[$error_type] = 0
        $error_patterns[$error_type] += 1
  
  CATCH RECOVERABLE:
    LOG("Slot " + $slot + " skipped or unavailable")
    CONTINUE

// Analyze patterns
$total_failed = COUNT(collection: $failed_txs)
$error_distribution = []

FOR $error_type IN KEYS($error_patterns):
  $count = $error_patterns[$error_type]
  $percentage = ($count / $total_failed) * 100
  $error_distribution = APPEND(
    array: $error_distribution,
    item: {
      error_type: $error_type,
      count: $count,
      percentage: $percentage
    }
  )

$error_distribution = SORT_BY(
  collection: $error_distribution,
  key: item => item.count,
  order: "desc"
)

// Calculate financial impact
$total_fees_wasted = SUM(
  collection: MAP($failed_txs, tx => tx.fee)
) / 1000000000

$avg_compute = MEAN(
  data: MAP($failed_txs, tx => tx.compute_units)
)
```

**Decision Point:** Identify dominant failure patterns
  BRANCH A ($error_distribution[0].percentage > 50):
    $pattern_analysis = "Single error type dominates - likely systematic issue"
    $dominant_error = $error_distribution[0].error_type
    
  BRANCH B ($error_distribution[0].percentage < 20):
    $pattern_analysis = "Diverse error types - normal network activity"
    $dominant_error = "none"
    
  BRANCH C (COUNT(collection: $error_distribution) < 5):
    $pattern_analysis = "Few error types - possible targeted attack or bug"
    $dominant_error = "multiple_related"

**Action:**
RETURN {
  blocks_analyzed: $blocks_analyzed,
  total_failed_txs: $total_failed,
  failure_rate_percent: ($total_failed / ($blocks_analyzed * 200)) * 100,  // Assume ~200 tx/block
  error_distribution: $error_distribution,
  dominant_error: $dominant_error,
  pattern_analysis: $pattern_analysis,
  total_fees_wasted_sol: $total_fees_wasted,
  avg_compute_units: $avg_compute,
  top_5_errors: SLICE(collection: $error_distribution, start: 0, end: 5),
  recommendations: [
    "Monitor top error types for systematic issues",
    "Check if specific programs are causing most failures",
    "Compare failure rates across different time periods",
    "Investigate unusually high compute unit consumption"
  ],
  confidence: 80,
  caveats: [
    "Some slots may be skipped in analysis",
    "Error patterns vary by network congestion",
    "Custom program errors need program-specific decoding"
  ]
}

---

## Q5: "Reconstruct what happened in this complex multi-program transaction with 15 instructions"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getTransaction, getAccountInfo (Solana RPC)
  - MAP, FILTER, SLICE (Data Processing)
  - CONTAINS, SPLIT (String operations)
  - anchorDeserialize, borshDeserialize (Solana Utilities)

**Main Branch:**
```ovsm
$signature = INPUT(prompt: "Enter complex transaction signature")

$tx = getTransaction(signature: $signature, maxSupportedTransactionVersion: 0)

$accounts = $tx.transaction.message.accountKeys
$instructions = $tx.transaction.message.instructions
$logs = $tx.meta.logMessages

// Build execution timeline from logs
$execution_timeline = []
$current_depth = 0
$instruction_index = 0

FOR $log IN $logs:
  IF CONTAINS($log, "Program") AND CONTAINS($log, "invoke") THEN
    $parts = SPLIT(string: $log, delimiter: " ")
    $program = $parts[1]
    
    IF CONTAINS($log, "invoke [1]") THEN
      $current_depth = 1
      $instruction_index += 1
    ELSE IF CONTAINS($log, "invoke [") THEN
      // Extract depth from [N]
      $depth_str = FILTER(collection: $parts, predicate: p => CONTAINS(p, "["))
      $current_depth += 1
    
    $execution_timeline = APPEND(
      array: $execution_timeline,
      item: {
        instruction_number: $instruction_index,
        depth: $current_depth,
        program: $program,
        type: "invoke"
      }
    )
    
  ELSE IF CONTAINS($log, "success") OR CONTAINS($log, "failed") THEN
    IF CONTAINS($log, "success") THEN
      $status = "success"
    ELSE
      $status = "failed"
    
    $execution_timeline = APPEND(
      array: $execution_timeline,
      item: {
        instruction_number: $instruction_index,
        depth: $current_depth,
        status: $status,
        type: "result"
      }
    )
    $current_depth -= 1
    
  ELSE IF CONTAINS($log, "Program log:") OR CONTAINS($log, "Program data:") THEN
    $log_data = SLICE(string: $log, start: 13, end: -1)  // Remove "Program log: " prefix
    $execution_timeline = APPEND(
      array: $execution_timeline,
      item: {
        instruction_number: $instruction_index,
        depth: $current_depth,
        message: $log_data,
        type: "log"
      }
    )

// Analyze account changes
$account_changes = []
FOR $i IN 0..COUNT(collection: $accounts):
  $pre_balance = $tx.meta.preBalances[$i]
  $post_balance = $tx.meta.postBalances[$i]
  $sol_change = ($post_balance - $pre_balance) / 1000000000
  
  IF $sol_change != 0 OR $i < 5 THEN  // Always include first 5 accounts
    $account_changes = APPEND(
      array: $account_changes,
      item: {
        index: $i,
        address: $accounts[$i],
        sol_change: $sol_change,
        pre_balance_sol: $pre_balance / 1000000000,
        post_balance_sol: $post_balance / 1000000000,
        is_signer: $i < COUNT(collection: $tx.transaction.message.header.numRequiredSignatures),
        is_writable: true  // Simplified - would need to check actual writeability
      }
    )

// Identify program types
$program_types = {}
FOR $ix IN $instructions:
  $program = $accounts[$ix.programIdIndex]
  
  IF CONTAINS($program, "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA") THEN
    $program_types[$program] = "SPL Token"
  ELSE IF CONTAINS($program, "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL") THEN
    $program_types[$program] = "Associated Token Account"
  ELSE IF CONTAINS($program, "11111111111111111111111111111111") THEN
    $program_types[$program] = "System Program"
  ELSE
    $program_types[$program] = "Custom Program"

// Build human-readable summary
$instruction_summary = []
$ix_num = 1

FOR $ix IN $instructions:
  $program = $accounts[$ix.programIdIndex]
  $program_type = $program_types[$program]
  
  $involved_accounts = MAP(
    collection: $ix.accounts,
    fn: idx => $accounts[idx]
  )
  
  $instruction_summary = APPEND(
    array: $instruction_summary,
    item: {
      number: $ix_num,
      program: $program,
      program_type: $program_type,
      accounts_involved: COUNT(collection: $involved_accounts),
      account_addresses: $involved_accounts
    }
  )
  $ix_num += 1
```

**Decision Point:** Classify transaction complexity
  BRANCH A (COUNT(collection: $instructions) > 10 AND COUNT(collection: UNIQUE($program_types)) > 3):
    $complexity = "very_high"
    $description = "Multi-program DeFi operation (likely swap + stake or complex arbitrage)"
    
  BRANCH B (COUNT(collection: $instructions) > 5 AND ANY($program_types, pt => pt == "SPL Token")):
    $complexity = "high"
    $description = "Complex token operation (likely multi-token swap or liquidity management)"
    
  BRANCH C (COUNT(collection: $instructions) <= 5):
    $complexity = "moderate"
    $description = "Standard multi-step operation"

**Action:**
RETURN {
  signature: $signature,
  complexity: $complexity,
  description: $description,
  total_instructions: COUNT(collection: $instructions),
  unique_programs: COUNT(collection: UNIQUE(KEYS($program_types))),
  programs_used: $program_types,
  execution_timeline: $execution_timeline,
  instruction_summary: $instruction_summary,
  account_changes: $account_changes,
  total_compute_units: $tx.meta.computeUnitsConsumed,
  transaction_fee_sol: $tx.meta.fee / 1000000000,
  success: $tx.meta.err == null,
  human_summary: "This transaction executed " + COUNT(collection: $instructions) + " instructions across " + COUNT(collection: UNIQUE(KEYS($program_types))) + " programs. " + $description,
  confidence: 75,
  caveats: [
    "Instruction data not decoded - would need program-specific decoders",
    "Some CPI (cross-program invocation) details may be simplified",
    "Token balance changes require additional decoding"
  ]
}
