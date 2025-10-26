# Account State - Forensics Level

## Q1: "My wallet was drained. Reconstruct exactly how the attacker got access and what they stole"

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - CONTAINS (String operations)
  - SUM, MEAN (Statistical)

**Main Branch:**
```ovsm
$victim_wallet = INPUT(prompt: "Enter your wallet address")

// Get current account state
TRY:
  $current_state = getAccountInfo(pubkey: $victim_wallet)
  $current_balance = $current_state.lamports / 1000000000
CATCH FATAL:
  RETURN ERROR(message: "Wallet does not exist on chain")

// Get all transaction history
$all_signatures = []
$before = null

LOOP WHILE true:
  TRY:
    $batch = getSignaturesForAddress(
      address: $victim_wallet,
      limit: 1000,
      before: $before
    )
  CATCH:
    BREAK
  
  GUARD COUNT(collection: $batch) > 0 ELSE BREAK
  
  $all_signatures = APPEND(array: $all_signatures, item: $batch)
  $before = LAST(collection: $batch).signature
  
  BREAK IF COUNT(collection: $all_signatures) > 5  // Limit to 5000 txs

$flat_signatures = FLATTEN(collection: $all_signatures)

// Analyze all transactions for suspicious activity
$timeline = []
$total_sol_lost = 0
$total_tokens_lost = []
$suspicious_txs = []

FOR $sig_info IN $flat_signatures:
  TRY:
    $tx = getTransaction(signature: $sig_info.signature, maxSupportedTransactionVersion: 0)
    
    $accounts = $tx.transaction.message.accountKeys
    $is_signer = $accounts[0] == $victim_wallet
    $pre_balances = $tx.meta.preBalances
    $post_balances = $tx.meta.postBalances
    
    // Find wallet index
    $wallet_index = null
    FOR $i IN 0..COUNT(collection: $accounts):
      IF $accounts[$i] == $victim_wallet THEN
        $wallet_index = $i
        BREAK
    
    IF $wallet_index != null THEN
      // Calculate SOL change
      $sol_before = $pre_balances[$wallet_index] / 1000000000
      $sol_after = $post_balances[$wallet_index] / 1000000000
      $sol_change = $sol_after - $sol_before
      
      // Analyze token changes
      $token_changes = []
      IF $tx.meta.preTokenBalances != null AND $tx.meta.postTokenBalances != null THEN
        FOR $pre IN $tx.meta.preTokenBalances:
          IF $pre.owner == $victim_wallet THEN
            $post = FIND(
              collection: $tx.meta.postTokenBalances,
              predicate: p => p.accountIndex == $pre.accountIndex
            )
            
            IF $post != null THEN
              $amount_before = $pre.uiTokenAmount.uiAmount
              $amount_after = $post.uiTokenAmount.uiAmount
              $token_change = $amount_after - $amount_before
              
              IF $token_change < 0 THEN  // Lost tokens
                $token_changes = APPEND(
                  array: $token_changes,
                  item: {
                    mint: $pre.mint,
                    amount_lost: ABS($token_change),
                    decimals: $pre.uiTokenAmount.decimals
                  }
                )
      
      // Detect suspicious patterns
      $is_suspicious = false
      $suspicion_reasons = []
      
      // Red flag 1: Large withdrawal not signed by wallet
      IF $sol_change < -0.1 AND NOT $is_signer THEN
        $is_suspicious = true
        $suspicion_reasons = APPEND(
          array: $suspicion_reasons,
          item: "Large SOL withdrawal without your signature"
        )
      
      // Red flag 2: Token delegation/approval abuse
      $logs = $tx.meta.logMessages
      IF ANY($logs, log => CONTAINS(log, "Approve") OR CONTAINS(log, "SetAuthority")) THEN
        IF NOT $is_signer THEN
          $is_suspicious = true
          $suspicion_reasons = APPEND(
            array: $suspicion_reasons,
            item: "Token authority changed without your signature"
          )
      
      // Red flag 3: Multiple token drains
      IF COUNT(collection: $token_changes) > 3 THEN
        $is_suspicious = true
        $suspicion_reasons = APPEND(
          array: $suspicion_reasons,
          item: "Multiple tokens drained in single transaction"
        )
      
      // Red flag 4: Unusual programs
      $programs_used = UNIQUE(MAP(
        collection: $tx.transaction.message.instructions,
        fn: ix => $accounts[ix.programIdIndex]
      ))
      
      $known_programs = [
        "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA",  // SPL Token
        "11111111111111111111111111111111",  // System
        "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL"  // ATA
      ]
      
      $unknown_programs = FILTER(
        collection: $programs_used,
        predicate: p => NOT ANY($known_programs, known => known == p)
      )
      
      IF COUNT(collection: $unknown_programs) > 0 AND $sol_change < -0.01 THEN
        $is_suspicious = true
        $suspicion_reasons = APPEND(
          array: $suspicion_reasons,
          item: "Interaction with unknown programs during fund loss"
        )
      
      $timeline = APPEND(
        array: $timeline,
        item: {
          timestamp: $sig_info.blockTime,
          signature: $sig_info.signature,
          slot: $sig_info.slot,
          sol_change: $sol_change,
          token_changes: $token_changes,
          was_signer: $is_signer,
          success: $tx.meta.err == null,
          programs_used: $programs_used,
          is_suspicious: $is_suspicious,
          suspicion_reasons: $suspicion_reasons
        }
      )
      
      IF $is_suspicious THEN
        $suspicious_txs = APPEND(array: $suspicious_txs, item: $timeline[COUNT(collection: $timeline) - 1])
      
      // Accumulate losses
      IF $sol_change < 0 THEN
        $total_sol_lost += ABS($sol_change)
      
      FOR $token_loss IN $token_changes:
        $total_tokens_lost = APPEND(array: $total_tokens_lost, item: $token_loss)
  
  CATCH:
    CONTINUE  // Skip failed transaction fetches

// Sort timeline chronologically
$timeline = SORT_BY(collection: $timeline, key: event => event.timestamp, order: "asc")
$suspicious_txs = SORT_BY(collection: $suspicious_txs, key: event => event.timestamp, order: "asc")

// Find the likely attack transaction
$attack_tx = FIND(
  collection: $suspicious_txs,
  predicate: tx => tx.is_suspicious AND (tx.sol_change < -0.5 OR COUNT(collection: tx.token_changes) > 2)
)

// Identify attack vector
$attack_vector = "unknown"
$attack_details = ""

IF $attack_tx != null THEN
  IF NOT $attack_tx.was_signer THEN
    $attack_vector = "delegate_abuse"
    $attack_details = "Attacker used previously granted token/account delegation to drain funds"
  ELSE IF ANY($attack_tx.suspicion_reasons, r => CONTAINS(r, "unknown programs")) THEN
    $attack_vector = "malicious_program"
    $attack_details = "You signed a transaction that called a malicious program"
  ELSE
    $attack_vector = "compromised_key"
    $attack_details = "Your private key was compromised - attacker signed transaction as you"

// Group losses by type
$token_loss_summary = GROUP_BY(
  collection: $total_tokens_lost,
  key: loss => loss.mint
)

$aggregated_token_losses = []
FOR $mint IN KEYS($token_loss_summary):
  $losses = $token_loss_summary[$mint]
  $total = SUM(collection: MAP($losses, l => l.amount_lost))
  $aggregated_token_losses = APPEND(
    array: $aggregated_token_losses,
    item: {
      mint: $mint,
      total_lost: $total,
      decimals: $losses[0].decimals
    }
  )
```

**Decision Point:** Assess recovery possibility
  BRANCH A ($attack_vector == "delegate_abuse"):
    $recovery_chance = "low"
    $action_steps = [
      "Revoke all token delegations immediately",
      "Move remaining funds to new wallet",
      "Check for any remaining approvals on Solscan"
    ]
    
  BRANCH B ($attack_vector == "compromised_key"):
    $recovery_chance = "very_low"
    $action_steps = [
      "URGENT: Move ANY remaining funds to new wallet NOW",
      "Generate new wallet with secure seed phrase",
      "Investigate how key was compromised",
      "Report to exchanges if funds were sent there"
    ]
    
  BRANCH C ($attack_vector == "malicious_program"):
    $recovery_chance = "very_low"
    $action_steps = [
      "Check for any residual program authorities",
      "Close any PDAs that might still be exploitable",
      "Report malicious program to Solana ecosystem"
    ]

**Action:**
RETURN {
  victim_wallet: $victim_wallet,
  current_balance_sol: $current_balance,
  total_sol_lost: $total_sol_lost,
  token_losses: $aggregated_token_losses,
  attack_vector: $attack_vector,
  attack_details: $attack_details,
  likely_attack_transaction: $attack_tx,
  all_suspicious_transactions: $suspicious_txs,
  full_timeline: $timeline,
  recovery_chance: $recovery_chance,
  immediate_actions: $action_steps,
  total_events_analyzed: COUNT(collection: $timeline),
  forensic_summary: "Analyzed " + COUNT(collection: $timeline) + " transactions. Found " + 
    COUNT(collection: $suspicious_txs) + " suspicious events. Total loss: " + 
    $total_sol_lost + " SOL + " + COUNT(collection: $aggregated_token_losses) + " token types.",
  confidence: 85,
  caveats: [
    "Cannot access private key compromise details",
    "Off-chain attack vectors not detectable",
    "Token values not calculated - only quantities",
    "Historical account state limited to transaction data"
  ]
}

---

## Q2: "Detect accounts with unusual data growth patterns that might indicate spam or exploits"

**Expected Plan:**
[TIME: ~45s] [COST: ~0.006 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo (Solana RPC)
  - MAP, FILTER, SORT_BY (Data Processing)
  - DETECT_OUTLIERS, MEAN, STDDEV (Statistical)

**Main Branch:**
```ovsm
$program_id = INPUT(prompt: "Enter program ID to analyze (or 'system' for all)")

// Sample accounts
$accounts_to_check = []

IF $program_id == "system" THEN
  // Would need to sample from multiple sources
  RETURN ERROR(message: "System-wide analysis not implemented - specify a program ID")
ELSE
  TRY:
    // Get all accounts owned by program
    $program_accounts = getProgramAccounts(
      programId: $program_id,
      encoding: "base64",
      dataSlice: {offset: 0, length: 0}  // Just get metadata
    )
  CATCH FATAL:
    RETURN ERROR(message: "Could not fetch program accounts - invalid program ID?")

$account_data_sizes = []
$account_growth_rates = []
$suspicious_accounts = []

FOR $account IN $program_accounts:
  $pubkey = $account.pubkey
  $data_size = COUNT(collection: $account.account.data)
  $lamports = $account.account.lamports
  
  $account_data_sizes = APPEND(
    array: $account_data_sizes,
    item: {
      address: $pubkey,
      size: $data_size,
      lamports: $lamports,
      rent_per_byte: $lamports / ($data_size + 128)  // Approximate
    }
  )

// Statistical analysis
$sizes = MAP($account_data_sizes, acc => acc.size)
$mean_size = MEAN(data: $sizes)
$stddev_size = STDDEV(data: $sizes)

// Find outliers (unusual sizes)
$outliers = DETECT_OUTLIERS(
  data: $sizes,
  method: "zscore",
  threshold: 3.0
)

FOR $i IN 0..COUNT(collection: $account_data_sizes):
  $acc = $account_data_sizes[$i]
  $z_score = ($acc.size - $mean_size) / $stddev_size
  
  IF ABS($z_score) > 3.0 THEN  // More than 3 standard deviations
    $suspicious_accounts = APPEND(
      array: $suspicious_accounts,
      item: {
        address: $acc.address,
        size_bytes: $acc.size,
        z_score: $z_score,
        suspicion: $z_score > 0 ? "unusually_large" : "unusually_small",
        rent_inefficiency: $acc.rent_per_byte < 6900 ? "under_rented" : "normal"
      }
    )

// Sort by suspicion level
$suspicious_accounts = SORT_BY(
  collection: $suspicious_accounts,
  key: acc => ABS(acc.z_score),
  order: "desc"
)
```

**Decision Point:** Classify threat level
  BRANCH A (COUNT(collection: $suspicious_accounts) > 100):
    $threat = "HIGH"
    $analysis = "Mass spam or exploit campaign - many outlier accounts"
    
  BRANCH B (COUNT(collection: $suspicious_accounts) > 10):
    $threat = "MEDIUM"
    $analysis = "Some unusual account growth - investigate further"
    
  BRANCH C (COUNT(collection: $suspicious_accounts) <= 10):
    $threat = "LOW"
    $analysis = "Normal variation in account sizes"

**Action:**
RETURN {
  program_id: $program_id,
  total_accounts_analyzed: COUNT(collection: $account_data_sizes),
  mean_account_size: ROUND($mean_size),
  stddev_account_size: ROUND($stddev_size),
  suspicious_accounts_found: COUNT(collection: $suspicious_accounts),
  threat_level: $threat,
  analysis: $analysis,
  top_10_suspicious: SLICE(collection: $suspicious_accounts, start: 0, end: 10),
  recommendations: [
    "Investigate accounts with z-score > 5",
    "Check for data spam attacks",
    "Verify rent collection is working properly"
  ],
  confidence: 75
}
