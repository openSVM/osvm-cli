# Transaction Analysis - Intermediate Queries

## Q1021: "Track all program interactions for a specific transaction"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, FLATTEN, UNIQUE (Data Processing)

**Main Branch:**
```ovsm
$signature = "ABC123"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Extract top-level program interactions
$top_level_programs = MAP(
  collection: $tx.message.instructions,
  fn: inst => $tx.message.accountKeys[inst.programIdIndex]
)

// Extract inner instruction (CPI) programs
$inner_programs = FLATTEN(
  collection: MAP(
    collection: $tx.meta.innerInstructions,
    fn: inner_group => MAP(
      collection: inner_group.instructions,
      fn: inst => $tx.message.accountKeys[inst.programIdIndex]
    )
  )
)

$all_programs = APPEND(array: $top_level_programs, item: $inner_programs)
$unique_programs = UNIQUE(collection: $all_programs)
```

**Decision Point:** Identify program types
  BRANCH A (known system programs):
    $system_programs = FILTER(
      collection: $unique_programs,
      predicate: prog => 
        prog == "11111111111111111111111111111111" OR
        prog == "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"
    )
    $custom_programs = FILTER(
      collection: $unique_programs,
      predicate: prog => NOT ($prog IN $system_programs)
    )
  BRANCH B (all custom):
    $custom_programs = $unique_programs

**Action:**
RETURN {
  signature: $signature,
  total_programs: COUNT(collection: $unique_programs),
  unique_programs: $unique_programs,
  top_level_count: COUNT(collection: $top_level_programs),
  cpi_count: COUNT(collection: $inner_programs),
  confidence: 100
}

---

## Q1022: "Analyze CPI depth and call tree for complex transaction"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, MAX (Data Processing)

**Main Branch:**
```ovsm
$signature = "XYZ789"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Parse logs to find CPI depth
$logs = $tx.meta.logMessages
$invoke_pattern = "invoke \\[(\\d+)\\]"  // Regex to extract depth

$depths = []

FOR $log IN $logs:
  IF $log CONTAINS "invoke [":
    // Extract depth number
    $depth_match = REGEX_EXTRACT(text: $log, pattern: $invoke_pattern)
    IF $depth_match != null:
      $depths = APPEND(array: $depths, item: $depth_match)

$max_depth = MAX(collection: $depths)
```

**Decision Point:** Analyze complexity
  BRANCH A (deep CPI > 4):
    $complexity = "highly complex"
    $analysis = "Deep cross-program invocation chain"
    
    // Build call tree from logs
    $call_tree = []
    FOR $log IN $logs:
      IF $log CONTAINS "Program" AND ($log CONTAINS "invoke" OR $log CONTAINS "success"):
        $call_tree = APPEND(array: $call_tree, item: $log)
  
  BRANCH B (moderate CPI 2-4):
    $complexity = "moderate"
    $analysis = "Standard CPI usage"
  
  BRANCH C (no CPI):
    $complexity = "simple"
    $analysis = "No cross-program invocations"

**Action:**
RETURN {
  signature: $signature,
  max_cpi_depth: $max_depth,
  complexity: $complexity,
  total_invocations: COUNT(collection: $depths),
  call_tree: $call_tree,
  analysis: $analysis,
  confidence: 95
}

---

## Q1023: "Parse and decode System Program instructions"

**Expected Plan:**
[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
```ovsm
$signature = "SYS123"
CONST SYSTEM_PROGRAM = "11111111111111111111111111111111"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Filter for System Program instructions
$system_instructions = FILTER(
  collection: $tx.message.instructions,
  predicate: inst => $tx.message.accountKeys[inst.programIdIndex] == SYSTEM_PROGRAM
)

$decoded_instructions = []

FOR $inst IN $system_instructions:
  // Decode based on instruction discriminator (first 4 bytes)
  $instruction_type = $inst.data[0]
  
  $decoded = {
    type: "unknown",
    accounts: MAP(collection: $inst.accounts, fn: idx => $tx.message.accountKeys[idx])
  }
```

**Decision Point:** Decode instruction type
  BRANCH A (CreateAccount - discriminator 0):
    IF $instruction_type == 0:
      $decoded.type = "CreateAccount"
      $decoded.params = {
        from: $decoded.accounts[0],
        to: $decoded.accounts[1],
        lamports: parseU64($inst.data[1..9]),
        space: parseU64($inst.data[9..17])
      }
  
  BRANCH B (Transfer - discriminator 2):
    IF $instruction_type == 2:
      $decoded.type = "Transfer"
      $decoded.params = {
        from: $decoded.accounts[0],
        to: $decoded.accounts[1],
        lamports: parseU64($inst.data[1..9])
      }
  
  BRANCH C (Assign - discriminator 1):
    IF $instruction_type == 1:
      $decoded.type = "Assign"
      $decoded.params = {
        account: $decoded.accounts[0],
        owner: parsePubkey($inst.data[1..33])
      }

  $decoded_instructions = APPEND(array: $decoded_instructions, item: $decoded)

**Action:**
RETURN {
  signature: $signature,
  system_instruction_count: COUNT(collection: $system_instructions),
  decoded_instructions: $decoded_instructions,
  confidence: 100
}

---

## Q1024: "Parse and decode Token Program instructions"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
```ovsm
$signature = "TOKEN456"
CONST TOKEN_PROGRAM = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$token_instructions = FILTER(
  collection: $tx.message.instructions,
  predicate: inst => $tx.message.accountKeys[inst.programIdIndex] == TOKEN_PROGRAM
)

$decoded_instructions = []

FOR $inst IN $token_instructions:
  $instruction_type = $inst.data[0]
  
  $decoded = {
    type: "unknown",
    accounts: MAP(collection: $inst.accounts, fn: idx => $tx.message.accountKeys[idx])
  }
```

**Decision Point:** Decode Token instruction type
  BRANCH A (Transfer - discriminator 3):
    IF $instruction_type == 3:
      $decoded.type = "Transfer"
      $decoded.params = {
        source: $decoded.accounts[0],
        destination: $decoded.accounts[1],
        authority: $decoded.accounts[2],
        amount: parseU64($inst.data[1..9])
      }
  
  BRANCH B (MintTo - discriminator 7):
    IF $instruction_type == 7:
      $decoded.type = "MintTo"
      $decoded.params = {
        mint: $decoded.accounts[0],
        destination: $decoded.accounts[1],
        authority: $decoded.accounts[2],
        amount: parseU64($inst.data[1..9])
      }
  
  BRANCH C (Burn - discriminator 8):
    IF $instruction_type == 8:
      $decoded.type = "Burn"
      $decoded.params = {
        account: $decoded.accounts[0],
        mint: $decoded.accounts[1],
        authority: $decoded.accounts[2],
        amount: parseU64($inst.data[1..9])
      }
  
  BRANCH D (Approve - discriminator 4):
    IF $instruction_type == 4:
      $decoded.type = "Approve"
      $decoded.params = {
        source: $decoded.accounts[0],
        delegate: $decoded.accounts[1],
        owner: $decoded.accounts[2],
        amount: parseU64($inst.data[1..9])
      }

  $decoded_instructions = APPEND(array: $decoded_instructions, item: $decoded)

**Action:**
RETURN {
  signature: $signature,
  token_instruction_count: COUNT(collection: $token_instructions),
  decoded_instructions: $decoded_instructions,
  confidence: 100
}

---

## Q1025: "Find all transactions that created new token accounts"

**Expected Plan:**
[TIME: ~12s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
CONST TOKEN_PROGRAM = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"
CONST SYSTEM_PROGRAM = "11111111111111111111111111111111"

$current_slot = getSlot()
$account_creations = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $signature = $tx.transaction.signatures[0]
      
      // Check for CreateAccount + InitializeAccount pattern
      $has_create = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst => 
          $tx.transaction.message.accountKeys[inst.programIdIndex] == SYSTEM_PROGRAM AND
          inst.data[0] == 0  // CreateAccount discriminator
      )
      
      $has_init = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == TOKEN_PROGRAM AND
          inst.data[0] == 1  // InitializeAccount discriminator
      )
      
      IF $has_create AND $has_init:
        $account_creations = APPEND(
          array: $account_creations,
          item: {
            signature: $signature,
            slot: $slot
          }
        )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze creation patterns
  BRANCH A (many creations - possible airdrop):
    IF COUNT(collection: $account_creations) > 50:
      $analysis = "High volume token account creation"
  BRANCH B (moderate):
    $analysis = "Normal token account activity"

**Action:**
RETURN {
  creation_count: COUNT(collection: $account_creations),
  transactions: $account_creations,
  blocks_searched: 100,
  analysis: $analysis,
  confidence: 95
}

---

## Q1026: "Extract all compute budget instructions from recent transactions"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
```ovsm
CONST COMPUTE_BUDGET_PROGRAM = "ComputeBudget111111111111111111111111111111"

$current_slot = getSlot()
$compute_budget_txs = []

FOR $i IN 0..50:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $has_compute_budget = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == COMPUTE_BUDGET_PROGRAM
      )
      
      IF $has_compute_budget:
        $signature = $tx.transaction.signatures[0]
        
        TRY:
          $full_tx = getTransaction(signature: $signature)
          
          $compute_instructions = FILTER(
            collection: $full_tx.message.instructions,
            predicate: inst =>
              $full_tx.message.accountKeys[inst.programIdIndex] == COMPUTE_BUDGET_PROGRAM
          )
          
          $decoded = MAP(
            collection: $compute_instructions,
            fn: inst => {
              discriminator: inst.data[0],
              data: inst.data
            }
          )
        CATCH RECOVERABLE:
          $decoded = null
        
        $compute_budget_txs = APPEND(
          array: $compute_budget_txs,
          item: {
            signature: $signature,
            slot: $slot,
            instructions: $decoded
          }
        )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Decode compute budget instruction types
  BRANCH A (SetComputeUnitLimit - discriminator 2):
    FOR $inst IN $decoded:
      IF $inst.discriminator == 2:
        $inst.type = "SetComputeUnitLimit"
        $inst.value = parseU32($inst.data[1..5])
  
  BRANCH B (SetComputeUnitPrice - discriminator 3):
    FOR $inst IN $decoded:
      IF $inst.discriminator == 3:
        $inst.type = "SetComputeUnitPrice"
        $inst.value = parseU64($inst.data[1..9])  // Micro-lamports per CU

**Action:**
RETURN {
  transaction_count: COUNT(collection: $compute_budget_txs),
  transactions: $compute_budget_txs,
  blocks_searched: 50,
  confidence: 100
}

---

## Q1027: "Identify transactions with address lookup tables"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$alt_transactions = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(
      slot: $slot,
      maxSupportedTransactionVersion: 0
    )
    
    FOR $tx IN $block.transactions:
      // V0 transactions can use ALTs
      IF $tx.version == 0:
        $signature = $tx.transaction.signatures[0]
        
        // Check if it has address table lookups
        $has_lookups = $tx.transaction.message.addressTableLookups != null AND
                       COUNT(collection: $tx.transaction.message.addressTableLookups) > 0
        
        IF $has_lookups:
          $lookup_tables = MAP(
            collection: $tx.transaction.message.addressTableLookups,
            fn: lookup => {
              table_address: lookup.accountKey,
              writable_indexes: COUNT(collection: lookup.writableIndexes),
              readonly_indexes: COUNT(collection: lookup.readonlyIndexes)
            }
          )
          
          $alt_transactions = APPEND(
            array: $alt_transactions,
            item: {
              signature: $signature,
              slot: $slot,
              lookup_tables: $lookup_tables,
              total_lookups: COUNT(collection: $lookup_tables)
            }
          )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze ALT usage
  BRANCH A (high adoption):
    IF COUNT(collection: $alt_transactions) > 20:
      $analysis = "Significant ALT adoption"
      $avg_lookups = MEAN(
        data: MAP(collection: $alt_transactions, fn: tx => tx.total_lookups)
      )
  BRANCH B (low adoption):
    $analysis = "Limited ALT usage"

**Action:**
RETURN {
  alt_transaction_count: COUNT(collection: $alt_transactions),
  transactions: $alt_transactions,
  blocks_searched: 100,
  analysis: $analysis,
  confidence: 100
}

---

## Q1028: "Analyze transaction account access patterns"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - COUNT, FILTER, GROUP_BY (Data Processing)

**Main Branch:**
```ovsm
$signature = "PATTERN123"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$total_accounts = COUNT(collection: $tx.message.accountKeys)

// Analyze writable vs readonly
$writable_accounts = []
$readonly_accounts = []

FOR $i IN 0..$total_accounts:
  $is_writable = $i < $tx.message.header.numRequiredSignatures OR
                 ($i >= $tx.message.header.numRequiredSignatures AND
                  $i < $tx.message.header.numRequiredSignatures + $tx.message.header.numReadonlySignedAccounts)
  
  IF $is_writable:
    $writable_accounts = APPEND(array: $writable_accounts, item: $tx.message.accountKeys[$i])
  ELSE:
    $readonly_accounts = APPEND(array: $readonly_accounts, item: $tx.message.accountKeys[$i])
```

**Decision Point:** Analyze account roles
  BRANCH A (many writable - complex state changes):
    IF COUNT(collection: $writable_accounts) > 10:
      $complexity = "high"
      $analysis = "Many state changes"
  
  BRANCH B (few writable - simple operation):
    $complexity = "low"
    $analysis = "Simple state changes"
  
  BRANCH C (identify program accounts):
    $program_accounts = []
    FOR $inst IN $tx.message.instructions:
      $program = $tx.message.accountKeys[inst.programIdIndex]
      $program_accounts = APPEND(array: $program_accounts, item: $program)
    
    $unique_programs = UNIQUE(collection: $program_accounts)

**Action:**
RETURN {
  signature: $signature,
  total_accounts: $total_accounts,
  writable_count: COUNT(collection: $writable_accounts),
  readonly_count: COUNT(collection: $readonly_accounts),
  unique_programs: COUNT(collection: $unique_programs),
  complexity: $complexity,
  analysis: $analysis,
  confidence: 95
}

---

## Q1029: "Find transactions with failed CPI calls"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$failed_cpi_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      // Transaction succeeded but check for CPI failures in logs
      IF $tx.meta.err == null:
        $signature = $tx.transaction.signatures[0]
        
        TRY:
          $full_tx = getTransaction(signature: $signature)
          $logs = $full_tx.meta.logMessages
          
          // Look for CPI failures in logs
          $has_cpi_failure = ANY(
            collection: $logs,
            predicate: log => 
              log CONTAINS "Program failed" OR
              log CONTAINS "returned error" OR
              (log CONTAINS "invoke" AND NEXT(log) CONTAINS "failed")
          )
          
          IF $has_cpi_failure:
            $failed_logs = FILTER(
              collection: $logs,
              predicate: log => log CONTAINS "failed" OR log CONTAINS "error"
            )
            
            $failed_cpi_txs = APPEND(
              array: $failed_cpi_txs,
              item: {
                signature: $signature,
                slot: $slot,
                error_logs: $failed_logs
              }
            )
        CATCH RECOVERABLE:
          CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Categorize CPI failures
  BRANCH A (program errors):
    $program_errors = FILTER(
      collection: $failed_cpi_txs,
      predicate: tx => ANY(
        collection: tx.error_logs,
        predicate: log => log CONTAINS "custom program error"
      )
    )
  
  BRANCH B (insufficient funds):
    $fund_errors = FILTER(
      collection: $failed_cpi_txs,
      predicate: tx => ANY(
        collection: tx.error_logs,
        predicate: log => log CONTAINS "insufficient"
      )
    )

**Action:**
RETURN {
  failed_cpi_count: COUNT(collection: $failed_cpi_txs),
  transactions: $failed_cpi_txs,
  blocks_searched: 100,
  confidence: 90,
  note: "CPI failures within successful transactions"
}

---

## Q1030: "Extract priority fees from recent transactions"

**Expected Plan:**
[TIME: ~12s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, MAP, MEAN, MEDIAN (Statistical)

**Main Branch:**
```ovsm
CONST COMPUTE_BUDGET_PROGRAM = "ComputeBudget111111111111111111111111111111"

$current_slot = getSlot()
$priority_fees = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $has_priority = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == COMPUTE_BUDGET_PROGRAM
      )
      
      IF $has_priority:
        $signature = $tx.transaction.signatures[0]
        
        TRY:
          $full_tx = getTransaction(signature: $signature)
          
          // Find SetComputeUnitPrice instruction (discriminator 3)
          $price_inst = FIND(
            collection: $full_tx.message.instructions,
            predicate: inst =>
              $full_tx.message.accountKeys[inst.programIdIndex] == COMPUTE_BUDGET_PROGRAM AND
              inst.data[0] == 3
          )
          
          IF $price_inst != null:
            $micro_lamports_per_cu = parseU64($price_inst.data[1..9])
            $compute_units = $full_tx.meta.computeUnitsConsumed
            
            $priority_fee = ($micro_lamports_per_cu * $compute_units) / 1000000
            
            $priority_fees = APPEND(
              array: $priority_fees,
              item: {
                signature: $signature,
                slot: $slot,
                price_micro_lamports: $micro_lamports_per_cu,
                compute_units: $compute_units,
                priority_fee_lamports: $priority_fee
              }
            )
        CATCH RECOVERABLE:
          CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze fee distribution
  BRANCH A (calculate statistics):
    $fee_values = MAP(collection: $priority_fees, fn: tx => tx.priority_fee_lamports)
    $mean_fee = MEAN(data: $fee_values)
    $median_fee = MEDIAN(data: $fee_values)
    $max_fee = MAX(collection: $fee_values)
    
    $stats = {
      mean: $mean_fee,
      median: $median_fee,
      max: $max_fee
    }
  BRANCH B (no priority fees found):
    $stats = null

**Action:**
RETURN {
  transaction_count: COUNT(collection: $priority_fees),
  transactions: $priority_fees,
  statistics: $stats,
  blocks_searched: 100,
  confidence: 100
}

---

## Q1031: "Identify transactions using memo program"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, MAP (Data Processing)

**Main Branch:**
```ovsm
CONST MEMO_PROGRAM = "MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr"
CONST MEMO_PROGRAM_V1 = "Memo1UhkJRfHyvLMcVucJwxXeuD728EqVDDwQDxFMNo"

$current_slot = getSlot()
$memo_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $has_memo = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst => {
          $program = $tx.transaction.message.accountKeys[inst.programIdIndex]
          RETURN $program == MEMO_PROGRAM OR $program == MEMO_PROGRAM_V1
        }
      )
      
      IF $has_memo:
        $signature = $tx.transaction.signatures[0]
        
        TRY:
          $full_tx = getTransaction(signature: $signature)
          
          // Extract memo data
          $memo_instructions = FILTER(
            collection: $full_tx.message.instructions,
            predicate: inst => {
              $program = $full_tx.message.accountKeys[inst.programIdIndex]
              RETURN $program == MEMO_PROGRAM OR $program == MEMO_PROGRAM_V1
            }
          )
          
          $memos = MAP(
            collection: $memo_instructions,
            fn: inst => parseString(inst.data)
          )
          
          $memo_txs = APPEND(
            array: $memo_txs,
            item: {
              signature: $signature,
              slot: $slot,
              memos: $memos
            }
          )
        CATCH RECOVERABLE:
          CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze memo usage patterns
  BRANCH A (has memo content):
    $analysis = "Memo program usage detected"
    $avg_memo_length = MEAN(
      data: MAP(
        collection: $memo_txs,
        fn: tx => COUNT(collection: tx.memos[0])
      )
    )
  BRANCH B (no memos):
    $analysis = "No memo transactions found"

**Action:**
RETURN {
  memo_transaction_count: COUNT(collection: $memo_txs),
  transactions: $memo_txs,
  blocks_searched: 100,
  analysis: $analysis,
  confidence: 100
}

---

## Q1032: "Parse Anchor program instructions with IDL"

**Expected Plan:**
[TIME: ~5s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction, getAccountInfo (Solana RPC)
  - anchorDeserialize (Solana Utilities)

**Main Branch:**
```ovsm
$signature = "ANCHOR789"
$program_id = "YourAnchorProgramIDHere"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Derive IDL PDA
$idl_pda = derivePDA(
  seeds: ["anchor:idl", $program_id],
  program_id: "BPFLoaderUpgradeab1e11111111111111111111111"
)

TRY:
  $idl_account = getAccountInfo(pubkey: $idl_pda)
  $idl = anchorDeserialize(data: $idl_account.data)
CATCH RECOVERABLE:
  RETURN ERROR(message: "IDL not found - program may not be Anchor-based")

// Filter for target program instructions
$program_instructions = FILTER(
  collection: $tx.message.instructions,
  predicate: inst => $tx.message.accountKeys[inst.programIdIndex] == $program_id
)
```

**Decision Point:** Decode with IDL
  BRANCH A (IDL available):
    $decoded_instructions = MAP(
      collection: $program_instructions,
      fn: inst => {
        // First 8 bytes are discriminator
        $discriminator = inst.data[0..8]
        
        // Find matching instruction in IDL
        $ix_def = FIND(
          collection: $idl.instructions,
          predicate: ix => ix.discriminator == $discriminator
        )
        
        IF $ix_def != null:
          RETURN {
            name: $ix_def.name,
            discriminator: $discriminator,
            accounts: MAP(collection: inst.accounts, fn: idx => $tx.message.accountKeys[idx])
          }
        ELSE:
          RETURN {
            name: "unknown",
            discriminator: $discriminator
          }
      }
    )
  
  BRANCH B (no IDL):
    $decoded_instructions = MAP(
      collection: $program_instructions,
      fn: inst => {
        discriminator: inst.data[0..8],
        raw_data: inst.data
      }
    )

**Action:**
RETURN {
  signature: $signature,
  program_id: $program_id,
  instruction_count: COUNT(collection: $program_instructions),
  decoded_instructions: $decoded_instructions,
  confidence: 85,
  caveats: ["Requires valid IDL", "Custom programs may have different layouts"]
}

---

## Q1033: "Analyze transaction rent economics"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - COUNT, SUM (Data Processing)

**Main Branch:**
```ovsm
$signature = "RENT123"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Calculate rent-exempt minimum for accounts
$account_creations = FILTER(
  collection: $tx.message.instructions,
  predicate: inst => 
    $tx.message.accountKeys[inst.programIdIndex] == "11111111111111111111111111111111" AND
    inst.data[0] == 0  // CreateAccount
)

$total_rent_allocated = 0

FOR $creation IN $account_creations:
  // Extract lamports (bytes 1-8) and space (bytes 9-16)
  $lamports = parseU64($creation.data[1..9])
  $space = parseU64($creation.data[9..17])
  
  // Calculate minimum rent-exempt balance
  // Formula: (space + 128) * rent_per_byte_year * 2
  // Approximate: ~0.00089 SOL per byte-year * 2 years
  $rent_exempt_min = ($space + 128) * 6960  // lamports
  
  $is_rent_exempt = $lamports >= $rent_exempt_min
  
  $total_rent_allocated = $total_rent_allocated + $lamports
```

**Decision Point:** Analyze rent status
  BRANCH A (all rent-exempt):
    $analysis = "All accounts funded as rent-exempt"
    $recommendation = "Good practice - no ongoing rent costs"
  
  BRANCH B (some not rent-exempt):
    $analysis = "Some accounts below rent-exempt threshold"
    $recommendation = "Risk of account closure due to rent"
  
  BRANCH C (no account creations):
    $analysis = "No new accounts created"
    $recommendation = "N/A"

**Action:**
RETURN {
  signature: $signature,
  accounts_created: COUNT(collection: $account_creations),
  total_rent_allocated_lamports: $total_rent_allocated,
  total_rent_allocated_sol: $total_rent_allocated / LAMPORTS_PER_SOL,
  analysis: $analysis,
  recommendation: $recommendation,
  confidence: 90
}

---

## Q1034: "Find transactions with secp256k1 instruction"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
CONST SECP256K1_PROGRAM = "KeccakSecp256k11111111111111111111111111111"

$current_slot = getSlot()
$secp_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $has_secp = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == SECP256K1_PROGRAM
      )
      
      IF $has_secp:
        $signature = $tx.transaction.signatures[0]
        
        $secp_txs = APPEND(
          array: $secp_txs,
          item: {
            signature: $signature,
            slot: $slot
          }
        )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze usage context
  BRANCH A (Ethereum signature verification):
    $analysis = "Likely Ethereum signature verification"
    $use_case = "Cross-chain bridge or Ethereum compatibility"
  
  BRANCH B (custom cryptography):
    $analysis = "Custom secp256k1 usage"
    $use_case = "Advanced cryptographic operations"

**Action:**
RETURN {
  secp256k1_transaction_count: COUNT(collection: $secp_txs),
  transactions: $secp_txs,
  blocks_searched: 100,
  analysis: $analysis,
  use_case: $use_case,
  confidence: 100
}

---

## Q1035: "Extract all account metadata changes in transaction"

**Expected Plan:**
[TIME: ~4s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, FILTER (Data Processing)

**Main Branch:**
```ovsm
$signature = "META456"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Pre-transaction balances (before)
$pre_balances = $tx.meta.preBalances
$post_balances = $tx.meta.postBalances

$balance_changes = []

FOR $i IN 0..COUNT(collection: $tx.message.accountKeys):
  $account = $tx.message.accountKeys[$i]
  $pre = $pre_balances[$i]
  $post = $post_balances[$i]
  $change = $post - $pre
  
  IF $change != 0:
    $balance_changes = APPEND(
      array: $balance_changes,
      item: {
        account: $account,
        pre_balance: $pre,
        post_balance: $post,
        change_lamports: $change,
        change_sol: $change / LAMPORTS_PER_SOL
      }
    )
```

**Decision Point:** Categorize changes
  BRANCH A (increased balances - recipients):
    $recipients = FILTER(
      collection: $balance_changes,
      predicate: change => change.change_lamports > 0
    )
  
  BRANCH B (decreased balances - payers):
    $payers = FILTER(
      collection: $balance_changes,
      predicate: change => change.change_lamports < 0
    )
  
  BRANCH C (analyze token balance changes):
    // Check for token balance changes in preTokenBalances/postTokenBalances
    IF $tx.meta.preTokenBalances != null:
      $token_changes = MAP(
        collection: $tx.meta.postTokenBalances,
        fn: post => {
          $pre = FIND(
            collection: $tx.meta.preTokenBalances,
            predicate: pre_token => pre_token.accountIndex == post.accountIndex
          )
          
          RETURN {
            account: $tx.message.accountKeys[post.accountIndex],
            mint: post.mint,
            pre_amount: $pre != null ? $pre.uiTokenAmount.amount : 0,
            post_amount: post.uiTokenAmount.amount,
            change: post.uiTokenAmount.amount - ($pre != null ? $pre.uiTokenAmount.amount : 0)
          }
        }
      )

**Action:**
RETURN {
  signature: $signature,
  sol_balance_changes: $balance_changes,
  recipients: $recipients,
  payers: $payers,
  token_balance_changes: $token_changes,
  confidence: 90
}

---

## Q1036: "Detect nonce account usage in transactions"

**Expected Plan:**
[TIME: ~10s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock, getTransaction (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
CONST SYSTEM_PROGRAM = "11111111111111111111111111111111"

$current_slot = getSlot()
$nonce_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      // Nonce transactions have AdvanceNonceAccount instruction
      $has_nonce = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == SYSTEM_PROGRAM AND
          inst.data[0] == 4  // AdvanceNonceAccount discriminator
      )
      
      IF $has_nonce:
        $signature = $tx.transaction.signatures[0]
        
        TRY:
          $full_tx = getTransaction(signature: $signature)
          
          $nonce_inst = FIND(
            collection: $full_tx.message.instructions,
            predicate: inst => inst.data[0] == 4
          )
          
          $nonce_account = $full_tx.message.accountKeys[$nonce_inst.accounts[0]]
          
          $nonce_txs = APPEND(
            array: $nonce_txs,
            item: {
              signature: $signature,
              slot: $slot,
              nonce_account: $nonce_account
            }
          )
        CATCH RECOVERABLE:
          CONTINUE
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze nonce usage
  BRANCH A (durable transaction nonces):
    $analysis = "Using durable transaction nonces"
    $use_case = "Offline signing or scheduled transactions"
  
  BRANCH B (rare usage):
    IF COUNT(collection: $nonce_txs) < 5:
      $analysis = "Rare nonce usage"

**Action:**
RETURN {
  nonce_transaction_count: COUNT(collection: $nonce_txs),
  transactions: $nonce_txs,
  blocks_searched: 100,
  analysis: $analysis,
  use_case: $use_case,
  confidence: 95
}

---

## Q1037: "Find transactions with Ed25519 instruction"

**Expected Plan:**
[TIME: ~8s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
CONST ED25519_PROGRAM = "Ed25519SigVerify111111111111111111111111111"

$current_slot = getSlot()
$ed25519_txs = []

FOR $i IN 0..100:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      $has_ed25519 = ANY(
        collection: $tx.transaction.message.instructions,
        predicate: inst =>
          $tx.transaction.message.accountKeys[inst.programIdIndex] == ED25519_PROGRAM
      )
      
      IF $has_ed25519:
        $signature = $tx.transaction.signatures[0]
        
        $ed25519_txs = APPEND(
          array: $ed25519_txs,
          item: {
            signature: $signature,
            slot: $slot
          }
        )
  CATCH RECOVERABLE:
    CONTINUE
```

**Decision Point:** Analyze signature verification usage
  BRANCH A (custom signature verification):
    $analysis = "Ed25519 signature verification in use"
    $use_case = "Custom signature validation or multi-sig"
  
  BRANCH B (rare usage):
    $analysis = "Limited Ed25519 verification usage"

**Action:**
RETURN {
  ed25519_transaction_count: COUNT(collection: $ed25519_txs),
  transactions: $ed25519_txs,
  blocks_searched: 100,
  analysis: $analysis,
  use_case: $use_case,
  confidence: 100
}

---

## Q1038: "Analyze transaction instruction ordering and dependencies"

**Expected Plan:**
[TIME: ~4s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP, COUNT (Data Processing)

**Main Branch:**
```ovsm
$signature = "ORDER789"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$instruction_flow = MAP(
  collection: $tx.message.instructions,
  fn: (inst, index) => {
    program: $tx.message.accountKeys[inst.programIdIndex],
    accounts: MAP(collection: inst.accounts, fn: idx => $tx.message.accountKeys[idx]),
    position: index,
    data_size: COUNT(collection: inst.data)
  }
)

// Analyze account access patterns
$account_access_order = []

FOR $inst IN $instruction_flow:
  FOR $account IN $inst.accounts:
    $account_access_order = APPEND(
      array: $account_access_order,
      item: {
        instruction_index: $inst.position,
        account: $account
      }
    )
```

**Decision Point:** Identify dependencies
  BRANCH A (sequential dependencies):
    // Check if later instructions read accounts written by earlier ones
    $dependencies = []
    
    FOR $i IN 0..COUNT(collection: $instruction_flow):
      FOR $j IN ($i + 1)..COUNT(collection: $instruction_flow):
        $earlier_accounts = $instruction_flow[$i].accounts
        $later_accounts = $instruction_flow[$j].accounts
        
        // Check for overlapping accounts
        $shared = FILTER(
          collection: $later_accounts,
          predicate: acc => $acc IN $earlier_accounts
        )
        
        IF COUNT(collection: $shared) > 0:
          $dependencies = APPEND(
            array: $dependencies,
            item: {
              from_instruction: $i,
              to_instruction: $j,
              shared_accounts: $shared
            }
          )
  
  BRANCH B (independent instructions):
    $analysis = "Instructions are independent"

**Action:**
RETURN {
  signature: $signature,
  instruction_count: COUNT(collection: $instruction_flow),
  instruction_flow: $instruction_flow,
  dependencies: $dependencies,
  analysis: COUNT(collection: $dependencies) > 0 ? "Sequential dependencies detected" : "Independent instructions",
  confidence: 90
}

---

## Q1039: "Extract reward distribution information from transaction"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - FILTER, SUM (Data Processing)

**Main Branch:**
```ovsm
$signature = "REWARD123"

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

// Check if transaction has rewards data
GUARD $tx.meta.rewards != null ELSE
  RETURN ERROR(message: "No reward information in transaction")

$rewards = MAP(
  collection: $tx.meta.rewards,
  fn: reward => {
    pubkey: reward.pubkey,
    lamports: reward.lamports,
    post_balance: reward.postBalance,
    reward_type: reward.rewardType,
    commission: reward.commission
  }
)

$total_rewards = SUM(
  collection: MAP(collection: $rewards, fn: r => r.lamports)
)
```

**Decision Point:** Categorize reward types
  BRANCH A (staking rewards):
    $staking_rewards = FILTER(
      collection: $rewards,
      predicate: r => r.reward_type == "staking"
    )
    $analysis = "Staking rewards distribution"
  
  BRANCH B (voting rewards):
    $voting_rewards = FILTER(
      collection: $rewards,
      predicate: r => r.reward_type == "voting"
    )
    $analysis = "Voting rewards distribution"
  
  BRANCH C (fee rewards):
    $fee_rewards = FILTER(
      collection: $rewards,
      predicate: r => r.reward_type == "fee"
    )
    $analysis = "Fee rewards distribution"

**Action:**
RETURN {
  signature: $signature,
  reward_count: COUNT(collection: $rewards),
  total_rewards_lamports: $total_rewards,
  total_rewards_sol: $total_rewards / LAMPORTS_PER_SOL,
  rewards: $rewards,
  analysis: $analysis,
  confidence: 85
}

---

## Q1040: "Identify batch transactions from the same sender"

**Expected Plan:**
[TIME: ~12s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - GROUP_BY, FILTER, COUNT (Data Processing)

**Main Branch:**
```ovsm
$current_slot = getSlot()
$all_txs = []

FOR $i IN 0..50:
  $slot = $current_slot - $i
  
  TRY:
    $block = getBlock(slot: $slot)
    
    FOR $tx IN $block.transactions:
      // Fee payer is the first signature
      $fee_payer = $tx.transaction.message.accountKeys[0]
      
      $all_txs = APPEND(
        array: $all_txs,
        item: {
          signature: $tx.transaction.signatures[0],
          slot: $slot,
          fee_payer: $fee_payer,
          block_time: $block.blockTime
        }
      )
  CATCH RECOVERABLE:
    CONTINUE

// Group by fee payer
$grouped_by_sender = GROUP_BY(
  collection: $all_txs,
  key: tx => tx.fee_payer
)

// Find senders with multiple transactions
$batch_senders = FILTER(
  collection: $grouped_by_sender,
  predicate: group => COUNT(collection: group.value) > 5
)
```

**Decision Point:** Analyze batching patterns
  BRANCH A (high frequency senders):
    $analysis = MAP(
      collection: $batch_senders,
      fn: sender => {
        // Calculate transaction rate
        $transactions = sender.value
        $first_time = MIN_BY(collection: $transactions, key: tx => tx.block_time).block_time
        $last_time = MAX_BY(collection: $transactions, key: tx => tx.block_time).block_time
        $time_span = $last_time - $first_time
        
        RETURN {
          sender: sender.key,
          transaction_count: COUNT(collection: $transactions),
          time_span_seconds: $time_span,
          rate_per_minute: (COUNT(collection: $transactions) / $time_span) * 60
        }
      }
    )
  
  BRANCH B (normal activity):
    $analysis = "No significant batching detected"

**Action:**
RETURN {
  batch_sender_count: COUNT(collection: $batch_senders),
  batch_senders: $analysis,
  blocks_searched: 50,
  confidence: 90
}
