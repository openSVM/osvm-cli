# Transaction Analysis - Expert Level

## Q1: "Detect and analyze MEV sandwich attacks in real-time on Jupiter swaps"

**Expected Plan:**
[TIME: ~60s] [COST: ~0.01 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, MAP, SORT_BY, GROUP_BY (Data Processing)
  - CONTAINS, SPLIT (String operations)
  - DETECT_OUTLIERS, FIND_PATTERNS, CORRELATE (Statistical)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$sandwich_attacks = []
$blocks_analyzed = 0
$target_blocks = 50

// Jupiter program IDs
$jupiter_v6 = "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4"
$jupiter_v4 = "JUP4Fb2cqiRUcaTHdrPC8h2gNsA2ETXiPDD33WcGuJB"

FOR $i IN 0..$target_blocks:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot, maxSupportedTransactionVersion: 0)
    $blocks_analyzed += 1
    
    // Extract all Jupiter swaps with their position in block
    $jupiter_swaps = []
    $tx_index = 0
    
    FOR $tx IN $block.transactions:
      $accounts = $tx.transaction.message.accountKeys
      $is_jupiter = ANY(
        collection: $accounts,
        predicate: acc => acc == $jupiter_v6 OR acc == $jupiter_v4
      )
      
      IF $is_jupiter AND $tx.meta.err == null THEN
        // Extract swap details from token balance changes
        $pre_token = $tx.meta.preTokenBalances
        $post_token = $tx.meta.postTokenBalances
        
        $input_token = null
        $output_token = null
        $input_amount = 0
        $output_amount = 0
        $user = $accounts[0]  // First signer is usually the user
        
        // Find tokens that decreased (input)
        FOR $pre IN $pre_token:
          $post = FIND(
            collection: $post_token,
            predicate: p => p.accountIndex == $pre.accountIndex
          )
          IF $post != null THEN
            $diff = $post.uiTokenAmount.uiAmount - $pre.uiTokenAmount.uiAmount
            IF $diff < 0 THEN
              $input_token = $pre.mint
              $input_amount = ABS($diff)
            ELSE IF $diff > 0 THEN
              $output_token = $post.mint
              $output_amount = $diff
        
        IF $input_token != null AND $output_token != null THEN
          $jupiter_swaps = APPEND(
            array: $jupiter_swaps,
            item: {
              tx_index: $tx_index,
              signature: $tx.transaction.signatures[0],
              user: $user,
              input_token: $input_token,
              output_token: $output_token,
              input_amount: $input_amount,
              output_amount: $output_amount,
              slot: $slot,
              priority_fee: extractPriorityFee($tx)
            }
          )
      
      $tx_index += 1
    
    // Group swaps by token pair
    $grouped = GROUP_BY(
      collection: $jupiter_swaps,
      key: swap => swap.input_token + "_" + swap.output_token
    )
    
    // Look for sandwich pattern: buy -> victim -> sell in sequence
    FOR $pair IN KEYS($grouped):
      $swaps = $grouped[$pair]
      
      IF COUNT(collection: $swaps) >= 3 THEN
        // Sort by transaction index
        $sorted = SORT_BY(collection: $swaps, key: s => s.tx_index, order: "asc")
        
        // Check for sandwich: large buy, smaller swap(s), large sell
        FOR $i IN 0..(COUNT(collection: $sorted) - 2):
          $first = $sorted[$i]
          $middle = $sorted[$i + 1]
          $last = $sorted[$i + 2]
          
          // Sandwich pattern detection
          $same_attacker = $first.user == $last.user
          $different_victim = $middle.user != $first.user
          $sequential = ($last.tx_index - $first.tx_index) <= 5
          $size_pattern = $first.input_amount > $middle.input_amount * 2
          $reverses_direction = $first.input_token == $last.output_token
          
          IF $same_attacker AND $different_victim AND $sequential AND $reverses_direction THEN
            // Calculate victim's loss due to price impact
            $normal_rate = $middle.output_amount / $middle.input_amount
            
            // Estimate what rate victim would have got without sandwich
            // (simplified - real calc would need AMM state)
            $estimated_better_rate = $normal_rate * 1.02  // 2% better
            $estimated_better_output = $middle.input_amount * $estimated_better_rate
            $victim_loss = $estimated_better_output - $middle.output_amount
            
            // Calculate attacker profit
            $attacker_profit = ($last.output_amount - $first.input_amount)
            
            $sandwich_attacks = APPEND(
              array: $sandwich_attacks,
              item: {
                slot: $slot,
                attacker: $first.user,
                victim: $middle.user,
                token_pair: $pair,
                front_run_tx: $first.signature,
                victim_tx: $middle.signature,
                back_run_tx: $last.signature,
                victim_loss_estimated: $victim_loss,
                attacker_profit_estimated: $attacker_profit,
                front_run_priority_fee: $first.priority_fee,
                victim_priority_fee: $middle.priority_fee,
                back_run_priority_fee: $last.priority_fee,
                tx_separation: $last.tx_index - $first.tx_index
              }
            )
  
  CATCH RECOVERABLE:
    LOG("Slot " + $slot + " unavailable")
    CONTINUE
  
  SLEEP(duration: 50)  // Rate limiting

// Aggregate statistics
$total_attacks = COUNT(collection: $sandwich_attacks)

GUARD $total_attacks > 0 ELSE
  RETURN {
    message: "No sandwich attacks detected in analyzed blocks",
    blocks_analyzed: $blocks_analyzed,
    confidence: 90
  }

$total_victim_loss = SUM(
  collection: MAP($sandwich_attacks, a => a.victim_loss_estimated)
)

$total_attacker_profit = SUM(
  collection: MAP($sandwich_attacks, a => a.attacker_profit_estimated)
)

// Identify top attackers
$attacker_counts = GROUP_BY(
  collection: $sandwich_attacks,
  key: attack => attack.attacker
)

$top_attackers = []
FOR $attacker IN KEYS($attacker_counts):
  $attacks = $attacker_counts[$attacker]
  $total_profit = SUM(
    collection: MAP($attacks, a => a.attacker_profit_estimated)
  )
  $top_attackers = APPEND(
    array: $top_attackers,
    item: {
      address: $attacker,
      attack_count: COUNT(collection: $attacks),
      total_profit: $total_profit
    }
  )

$top_attackers = SORT_BY(
  collection: $top_attackers,
  key: a => a.total_profit,
  order: "desc"
)

// Analyze priority fee strategies
$avg_attacker_fee = MEAN(
  data: FLATTEN([
    MAP($sandwich_attacks, a => a.front_run_priority_fee),
    MAP($sandwich_attacks, a => a.back_run_priority_fee)
  ])
)

$avg_victim_fee = MEAN(
  data: MAP($sandwich_attacks, a => a.victim_priority_fee)
)

$fee_advantage = $avg_attacker_fee - $avg_victim_fee
```

**Decision Point:** Assess MEV severity
  BRANCH A ($total_attacks > 10 AND $fee_advantage > 1000):
    $severity = "CRITICAL"
    $analysis = "High frequency sophisticated MEV operation with significant fee advantages"
    $recommendation = "Users should increase priority fees or use private RPC"
    
  BRANCH B ($total_attacks > 5 AND $total_victim_loss > 100):
    $severity = "HIGH"
    $analysis = "Moderate MEV activity causing measurable user losses"
    $recommendation = "Consider using MEV-protected RPC endpoints"
    
  BRANCH C ($total_attacks <= 5):
    $severity = "LOW"
    $analysis = "Minimal MEV activity detected"
    $recommendation = "Normal network conditions"

**Action:**
RETURN {
  blocks_analyzed: $blocks_analyzed,
  total_sandwich_attacks: $total_attacks,
  severity: $severity,
  analysis: $analysis,
  total_victim_loss_tokens: $total_victim_loss,
  total_attacker_profit_tokens: $total_attacker_profit,
  top_attackers: SLICE(collection: $top_attackers, start: 0, end: 5),
  avg_attacker_priority_fee_lamports: $avg_attacker_fee,
  avg_victim_priority_fee_lamports: $avg_victim_fee,
  fee_advantage_lamports: $fee_advantage,
  all_attacks: $sandwich_attacks,
  recommendations: [
    $recommendation,
    "Use limit orders instead of market orders when possible",
    "Split large trades into smaller chunks",
    "Monitor slippage tolerance carefully"
  ],
  confidence: 75,
  caveats: [
    "Profit estimates simplified - actual values depend on AMM curves",
    "May miss sandwiches using multiple wallets",
    "Private mempool transactions not visible",
    "Some patterns may be legitimate arbitrage"
  ]
}

---

## Q2: "Analyze transaction clustering to identify bot networks and coordinated trading"

**Expected Plan:**
[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 70%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, FLATTEN (Data Processing)
  - CORRELATE, FIND_PATTERNS, identifyPatterns (Statistical)
  - ADD_NODE, ADD_EDGE, FIND_CLUSTERS (Knowledge Graph)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$blocks_to_analyze = 100
$graph = INIT_KNOWLEDGE_GRAPH()
$all_interactions = []

// Collect transaction data
FOR $i IN 0..$blocks_to_analyze:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot, maxSupportedTransactionVersion: 0)
    
    FOR $tx IN $block.transactions:
      IF $tx.meta.err == null THEN  // Only successful transactions
        $accounts = $tx.transaction.message.accountKeys
        $signer = $accounts[0]
        $signature = $tx.transaction.signatures[0]
        
        // Build interaction graph
        FOR $j IN 1..MIN_OF(COUNT(collection: $accounts), 10):  // Check first 10 accounts
          $interacted_with = $accounts[$j]
          
          $all_interactions = APPEND(
            array: $all_interactions,
            item: {
              from: $signer,
              to: $interacted_with,
              slot: $slot,
              signature: $signature,
              compute_units: $tx.meta.computeUnitsConsumed,
              fee: $tx.meta.fee,
              num_instructions: COUNT(collection: $tx.transaction.message.instructions)
            }
          )
          
          // Add to graph
          $graph = ADD_NODE(graph: $graph, id: $signer, type: "wallet")
          $graph = ADD_NODE(graph: $graph, id: $interacted_with, type: "account")
          $graph = ADD_EDGE(
            graph: $graph,
            from: $signer,
            to: $interacted_with,
            relationship: "interacted",
            slot: $slot
          )
  
  CATCH RECOVERABLE:
    CONTINUE

// Analyze patterns
// 1. Find wallets with identical behavior patterns
$wallet_patterns = {}

$grouped_by_wallet = GROUP_BY(
  collection: $all_interactions,
  key: ix => ix.from
)

FOR $wallet IN KEYS($grouped_by_wallet):
  $interactions = $grouped_by_wallet[$wallet]
  
  // Create behavioral fingerprint
  $avg_compute = MEAN(data: MAP($interactions, i => i.compute_units))
  $avg_fee = MEAN(data: MAP($interactions, i => i.fee))
  $avg_instructions = MEAN(data: MAP($interactions, i => i.num_instructions))
  $interaction_targets = UNIQUE(MAP($interactions, i => i.to))
  $tx_count = COUNT(collection: $interactions)
  
  // Time pattern analysis
  $slots = MAP($interactions, i => i.slot)
  $sorted_slots = SORT_BY(collection: $slots, key: s => s, order: "asc")
  $slot_gaps = []
  
  FOR $k IN 0..(COUNT(collection: $sorted_slots) - 1):
    $gap = $sorted_slots[$k + 1] - $sorted_slots[$k]
    $slot_gaps = APPEND(array: $slot_gaps, item: $gap)
  
  $avg_gap = MEAN(data: $slot_gaps)
  $gap_stddev = STDDEV(data: $slot_gaps)
  
  $wallet_patterns[$wallet] = {
    tx_count: $tx_count,
    avg_compute: ROUND($avg_compute),
    avg_fee: ROUND($avg_fee),
    avg_instructions: ROUND($avg_instructions * 10) / 10,  // Round to 1 decimal
    unique_targets: COUNT(collection: $interaction_targets),
    avg_slot_gap: ROUND($avg_gap),
    gap_regularity: $gap_stddev,
    targets: $interaction_targets
  }

// 2. Find clusters of similar wallets (likely bots)
$clusters = []

$analyzed_wallets = []
FOR $wallet_a IN KEYS($wallet_patterns):
  IF CONTAINS($analyzed_wallets, $wallet_a) THEN
    CONTINUE
  
  $pattern_a = $wallet_patterns[$wallet_a]
  $cluster_members = [$wallet_a]
  
  FOR $wallet_b IN KEYS($wallet_patterns):
    IF $wallet_a == $wallet_b OR CONTAINS($analyzed_wallets, $wallet_b) THEN
      CONTINUE
    
    $pattern_b = $wallet_patterns[$wallet_b]
    
    // Check similarity
    $compute_similar = ABS($pattern_a.avg_compute - $pattern_b.avg_compute) < 100
    $fee_similar = ABS($pattern_a.avg_fee - $pattern_b.avg_fee) < 100
    $instruction_similar = ABS($pattern_a.avg_instructions - $pattern_b.avg_instructions) < 0.5
    $timing_similar = ABS($pattern_a.avg_slot_gap - $pattern_b.avg_slot_gap) < 3
    
    // Check if they interact with same targets
    $common_targets = []
    FOR $target IN $pattern_a.targets:
      IF CONTAINS($pattern_b.targets, $target) THEN
        $common_targets = APPEND(array: $common_targets, item: $target)
    
    $target_overlap = COUNT(collection: $common_targets) / 
                      MIN_OF($pattern_a.unique_targets, $pattern_b.unique_targets)
    
    // Bot cluster criteria
    IF $compute_similar AND $fee_similar AND $instruction_similar AND 
       $timing_similar AND $target_overlap > 0.7 THEN
      $cluster_members = APPEND(array: $cluster_members, item: $wallet_b)
      $analyzed_wallets = APPEND(array: $analyzed_wallets, item: $wallet_b)
  
  IF COUNT(collection: $cluster_members) >= 2 THEN
    $clusters = APPEND(
      array: $clusters,
      item: {
        size: COUNT(collection: $cluster_members),
        members: $cluster_members,
        pattern: $pattern_a,
        common_targets: $pattern_a.targets
      }
    )
    
    FOR $member IN $cluster_members:
      $analyzed_wallets = APPEND(array: $analyzed_wallets, item: $member)

// 3. Detect coordinated timing patterns
$coordinated_groups = []

FOR $cluster IN $clusters:
  IF $cluster.size >= 3 THEN
    // Get all transactions from cluster members
    $cluster_txs = []
    FOR $member IN $cluster.members:
      $member_txs = $grouped_by_wallet[$member]
      FOR $tx IN $member_txs:
        $cluster_txs = APPEND(
          array: $cluster_txs,
          item: {wallet: $member, slot: $tx.slot, signature: $tx.signature}
        )
    
    // Group by slot
    $by_slot = GROUP_BY(collection: $cluster_txs, key: tx => tx.slot)
    
    // Find slots where multiple cluster members acted together
    $coordinated_slots = []
    FOR $slot IN KEYS($by_slot):
      $txs_in_slot = $by_slot[$slot]
      IF COUNT(collection: $txs_in_slot) >= 2 THEN
        $coordinated_slots = APPEND(array: $coordinated_slots, item: $slot)
    
    IF COUNT(collection: $coordinated_slots) >= 5 THEN
      $coordinated_groups = APPEND(
        array: $coordinated_groups,
        item: {
          cluster_size: $cluster.size,
          members: $cluster.members,
          coordinated_slots: $coordinated_slots,
          coordination_frequency: COUNT(collection: $coordinated_slots)
        }
      )
```

**Decision Point:** Classify bot network sophistication
  BRANCH A (COUNT(collection: $clusters) > 5 AND COUNT(collection: $coordinated_groups) > 0):
    $threat_level = "HIGH"
    $analysis = "Sophisticated bot network with coordinated timing - likely MEV/market manipulation"
    $confidence = 85
    
  BRANCH B (COUNT(collection: $clusters) > 0):
    $threat_level = "MEDIUM"
    $analysis = "Bot clusters detected but coordination unclear - may be independent automated trading"
    $confidence = 75
    
  BRANCH C (COUNT(collection: $clusters) == 0):
    $threat_level = "LOW"
    $analysis = "No clear bot clustering detected - mostly organic activity"
    $confidence = 60

**Action:**
RETURN {
  blocks_analyzed: $blocks_to_analyze,
  total_unique_wallets: COUNT(collection: KEYS($wallet_patterns)),
  bot_clusters_found: COUNT(collection: $clusters),
  coordinated_groups: COUNT(collection: $coordinated_groups),
  threat_level: $threat_level,
  analysis: $analysis,
  largest_cluster: MAX_BY(collection: $clusters, key: c => c.size),
  most_coordinated: MAX_BY(collection: $coordinated_groups, key: g => g.coordination_frequency),
  all_clusters: $clusters,
  all_coordinated_groups: $coordinated_groups,
  interaction_graph: $graph,
  recommendations: [
    "Monitor identified clusters for market manipulation",
    "Check if clusters are attacking specific protocols",
    "Analyze transaction patterns for exploit signatures",
    "Report suspicious coordinated activity to protocol teams"
  ],
  confidence: $confidence,
  caveats: [
    "Similar patterns don't always mean coordinated behavior",
    "Legitimate market makers may appear as bot clusters",
    "Analysis limited to successful transactions only",
    "Private transactions not included"
  ]
}

---

## Q3: "Reverse-engineer the complete state transitions in a failed DeFi transaction to identify the exact failure point"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getTransaction, getAccountInfo (Solana RPC)
  - MAP, FILTER, FIND (Data Processing)
  - CONTAINS, SPLIT (String operations)
  - borshDeserialize, anchorDeserialize (Solana Utilities)

**Main Branch:**
```ovsm
$signature = INPUT(prompt: "Enter failed transaction signature")

$tx = getTransaction(signature: $signature, maxSupportedTransactionVersion: 0)

GUARD $tx.meta.err != null ELSE
  RETURN ERROR(message: "Transaction succeeded - no failure to analyze")

$logs = $tx.meta.logMessages
$accounts = $tx.transaction.message.accountKeys
$instructions = $tx.transaction.message.instructions

// Build execution trace
$execution_trace = []
$program_stack = []
$instruction_num = 0
$current_program = null

FOR $log IN $logs:
  IF CONTAINS($log, "Program") AND CONTAINS($log, "invoke") THEN
    // New program invocation
    $parts = SPLIT(string: $log, delimiter: " ")
    $program = $parts[1]
    
    IF CONTAINS($log, "invoke [1]") THEN
      // Top-level instruction
      $instruction_num += 1
      $current_program = $program
      $program_stack = [$program]
      
      $execution_trace = APPEND(
        array: $execution_trace,
        item: {
          type: "instruction_start",
          instruction_num: $instruction_num,
          program: $program,
          depth: 1,
          status: "executing"
        }
      )
    ELSE
      // CPI (cross-program invocation)
      $depth = COUNT(collection: $program_stack) + 1
      $program_stack = APPEND(array: $program_stack, item: $program)
      
      $execution_trace = APPEND(
        array: $execution_trace,
        item: {
          type: "cpi",
          instruction_num: $instruction_num,
          caller: LAST(collection: $program_stack),
          callee: $program,
          depth: $depth,
          status: "executing"
        }
      )
  
  ELSE IF CONTAINS($log, "success") THEN
    // Program completed successfully
    $completed_program = LAST(collection: $program_stack)
    $program_stack = SLICE(collection: $program_stack, start: 0, end: -1)
    
    $execution_trace = APPEND(
      array: $execution_trace,
      item: {
        type: "success",
        program: $completed_program,
        instruction_num: $instruction_num,
        depth: COUNT(collection: $program_stack) + 1
      }
    )
  
  ELSE IF CONTAINS($log, "failed") THEN
    // FAILURE POINT IDENTIFIED
    $failed_program = LAST(collection: $program_stack)
    $failure_depth = COUNT(collection: $program_stack)
    
    // Extract error message
    $error_msg = $log
    $error_code = null
    
    IF CONTAINS($log, "custom program error:") THEN
      $parts = SPLIT(string: $log, delimiter: ":")
      $error_code = TRIM(LAST(collection: $parts))
    
    $execution_trace = APPEND(
      array: $execution_trace,
      item: {
        type: "FAILURE",
        program: $failed_program,
        instruction_num: $instruction_num,
        depth: $failure_depth,
        error_message: $error_msg,
        error_code: $error_code,
        program_stack: $program_stack
      }
    )
    
    BREAK  // Execution stops at failure
  
  ELSE IF CONTAINS($log, "Program log:") OR CONTAINS($log, "Program data:") THEN
    // Program emitted a log
    $log_content = SLICE(string: $log, start: 13, end: -1)
    
    $execution_trace = APPEND(
      array: $execution_trace,
      item: {
        type: "log",
        program: LAST(collection: $program_stack),
        instruction_num: $instruction_num,
        depth: COUNT(collection: $program_stack),
        message: $log_content
      }
    )

// Find the exact failure point
$failure_event = FIND(
  collection: $execution_trace,
  predicate: event => event.type == "FAILURE"
)

GUARD $failure_event != null ELSE
  RETURN ERROR(message: "Could not identify failure point in logs")

// Analyze account state at failure
$account_states = []

FOR $i IN 0..MIN_OF(COUNT(collection: $accounts), 20):
  $account = $accounts[$i]
  
  TRY:
    $account_info = getAccountInfo(pubkey: $account)
    
    $account_states = APPEND(
      array: $account_states,
      item: {
        index: $i,
        address: $account,
        owner: $account_info.owner,
        lamports: $account_info.lamports,
        data_size: COUNT(collection: $account_info.data),
        executable: $account_info.executable,
        rent_epoch: $account_info.rentEpoch
      }
    )
  CATCH:
    // Account might not exist
    $account_states = APPEND(
      array: $account_states,
      item: {
        index: $i,
        address: $account,
        status: "not_found_or_closed"
      }
    )

// Analyze what the failing instruction was trying to do
$failing_instruction_index = $failure_event.instruction_num - 1
$failing_instruction = $instructions[$failing_instruction_index]

$failing_program = $accounts[$failing_instruction.programIdIndex]
$instruction_accounts = MAP(
  collection: $failing_instruction.accounts,
  fn: idx => $accounts[idx]
)

// Check for common failure causes
$failure_causes = []

// Check 1: Insufficient funds
FOR $state IN $account_states:
  IF $state.lamports != null AND $state.lamports < 890880 THEN  // Rent exemption threshold
    $failure_causes = APPEND(
      array: $failure_causes,
      item: "Account " + $state.address + " may lack rent exemption (" + $state.lamports + " lamports)"
    )

// Check 2: Account not initialized
FOR $state IN $account_states:
  IF $state.status == "not_found_or_closed" THEN
    $failure_causes = APPEND(
      array: $failure_causes,
      item: "Account " + $state.address + " does not exist or was closed"
    )

// Check 3: Wrong account owner
$expected_owner = $failing_program
FOR $state IN $account_states:
  IF $state.owner != null AND $state.owner != $expected_owner AND 
     $state.owner != "11111111111111111111111111111111" THEN  // Not system program
    $failure_causes = APPEND(
      array: $failure_causes,
      item: "Account " + $state.address + " owned by " + $state.owner + ", expected " + $expected_owner
    )
```

**Decision Point:** Determine root cause category
  BRANCH A (CONTAINS($failure_event.error_message, "InsufficientFunds") OR 
            ANY($failure_causes, c => CONTAINS(c, "lamports"))):
    $root_cause = "INSUFFICIENT_FUNDS"
    $explanation = "Account lacks required SOL for rent or operation"
    $fix = "Add SOL to the account: " + $instruction_accounts[0]
    
  BRANCH B (CONTAINS($failure_event.error_message, "AccountNotInitialized") OR
            ANY($failure_causes, c => CONTAINS(c, "does not exist"))):
    $root_cause = "UNINITIALIZED_ACCOUNT"
    $explanation = "Trying to use an account that hasn't been created"
    $fix = "Initialize the account before use"
    
  BRANCH C (CONTAINS($failure_event.error_message, "InvalidAccountData")):
    $root_cause = "CORRUPTED_ACCOUNT_DATA"
    $explanation = "Account data doesn't match expected format"
    $fix = "Verify account was created by correct program"
    
  BRANCH D (ANY($failure_causes, c => CONTAINS(c, "owned by"))):
    $root_cause = "WRONG_ACCOUNT_OWNER"
    $explanation = "Account is owned by wrong program"
    $fix = "Ensure you're passing the correct account"
    
  BRANCH E (CONTAINS($failure_event.error_code, "0x")):
    $root_cause = "CUSTOM_PROGRAM_ERROR"
    $explanation = "Program-specific error: " + $failure_event.error_code
    $fix = "Check program documentation for error code " + $failure_event.error_code

**Action:**
RETURN {
  signature: $signature,
  root_cause: $root_cause,
  explanation: $explanation,
  fix_recommendation: $fix,
  failure_point: {
    instruction_number: $failure_event.instruction_num,
    failing_program: $failure_event.program,
    execution_depth: $failure_event.depth,
    call_stack: $failure_event.program_stack,
    error_message: $failure_event.error_message,
    error_code: $failure_event.error_code
  },
  full_execution_trace: $execution_trace,
  account_states_at_failure: $account_states,
  failing_instruction_accounts: $instruction_accounts,
  likely_causes: $failure_causes,
  instructions_executed_before_failure: $failure_event.instruction_num - 1,
  compute_units_consumed: $tx.meta.computeUnitsConsumed,
  fee_paid: $tx.meta.fee / 1000000000,
  detailed_steps: [
    "1. Transaction started with instruction #1",
    "2. Executed successfully until instruction #" + $failure_event.instruction_num,
    "3. Failure occurred in " + $failure_event.program + " at depth " + $failure_event.depth,
    "4. Root cause: " + $explanation,
    "5. Fix: " + $fix
  ],
  confidence: 80,
  caveats: [
    "Some programs use custom error encoding",
    "Account states shown are current, not at failure time",
    "CPI details may be simplified",
    "Some failure causes require program-specific knowledge"
  ]
}
