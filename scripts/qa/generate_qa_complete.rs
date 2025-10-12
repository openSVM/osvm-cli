use std::fs;
use std::path::Path;

const CATEGORIES: [(&str, &str); 10] = [
    ("01_transaction_analysis", "Transaction Analysis"),
    ("02_account_state", "Account State"),
    ("03_program_interaction", "Program Interaction"),
    ("04_token_research", "Token Research"),
    ("05_defi_analysis", "DeFi Analysis"),
    ("06_nft_analysis", "NFT Analysis"),
    ("07_network_analysis", "Network Analysis"),
    ("08_validator_research", "Validator Research"),
    ("09_historical_analysis", "Historical Analysis"),
    ("10_advanced_scenarios", "Advanced Scenarios"),
];

const DIFFICULTY_LEVELS: [(&str, &str); 10] = [
    ("01_basic", "Basic"),
    ("02_intermediate", "Intermediate"),
    ("03_advanced", "Advanced"),
    ("04_analysis", "Analysis"),
    ("05_patterns", "Patterns"),
    ("06_optimization", "Optimization"),
    ("07_forensics", "Forensics"),
    ("08_historical", "Historical"),
    ("09_edge_cases", "Edge Cases"),
    ("10_expert", "Expert"),
];

fn main() -> std::io::Result<()> {
    let base_path = "test_qa_categories";
    
    for (cat_dir, cat_name) in CATEGORIES.iter() {
        let cat_path = Path::new(base_path).join(cat_dir);
        
        // Create category directory if it doesn't exist
        fs::create_dir_all(&cat_path)?;
        
        for (level_file, level_name) in DIFFICULTY_LEVELS.iter() {
            let file_path = cat_path.join(format!("{}.md", level_file));
            
            // Skip if file already exists
            if file_path.exists() {
                println!("Skipping existing file: {:?}", file_path);
                continue;
            }
            
            let content = generate_content(cat_name, cat_dir, level_name, level_file);
            fs::write(&file_path, content)?;
            println!("Created: {:?}", file_path);
        }
    }
    
    println!("\nAll QA files generated successfully!");
    Ok(())
}

fn generate_content(cat_name: &str, cat_dir: &str, level_name: &str, level_file: &str) -> String {
    let header = format!("# {} - {} Level\n\n", cat_name, level_name);
    
    // Generate 5 questions per file (can be expanded)
    let questions = match (cat_dir, level_file) {
        // Transaction Analysis Questions
        ("01_transaction_analysis", "01_basic") => generate_transaction_basic(),
        ("01_transaction_analysis", "02_intermediate") => generate_transaction_intermediate(),
        ("01_transaction_analysis", "04_analysis") => generate_transaction_analysis(),
        ("01_transaction_analysis", "05_patterns") => generate_transaction_patterns(),
        ("01_transaction_analysis", "06_optimization") => generate_transaction_optimization(),
        ("01_transaction_analysis", "07_forensics") => generate_transaction_forensics(),
        ("01_transaction_analysis", "08_historical") => generate_transaction_historical(),
        ("01_transaction_analysis", "09_edge_cases") => generate_transaction_edge_cases(),
        ("01_transaction_analysis", "10_expert") => generate_transaction_expert(),
        
        // Account State Questions
        ("02_account_state", "01_basic") => generate_account_basic(),
        ("02_account_state", "02_intermediate") => generate_account_intermediate(),
        ("02_account_state", "03_advanced") => generate_account_advanced(),
        ("02_account_state", "04_analysis") => generate_account_analysis(),
        ("02_account_state", "05_patterns") => generate_account_patterns(),
        ("02_account_state", "06_optimization") => generate_account_optimization(),
        ("02_account_state", "07_forensics") => generate_account_forensics(),
        ("02_account_state", "08_historical") => generate_account_historical(),
        ("02_account_state", "09_edge_cases") => generate_account_edge_cases(),
        ("02_account_state", "10_expert") => generate_account_expert(),
        
        // Add more category-specific generators...
        _ => generate_generic_questions(cat_name, level_name),
    };
    
    format!("{}{}", header, questions)
}

fn generate_transaction_basic() -> String {
    r#"## Q1: "What was the fee for a specific transaction?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$fee_lamports = $tx.meta.fee
$fee_sol = $fee_lamports / 1000000000

**Action:**
RETURN {
  signature: $signature,
  fee_lamports: $fee_lamports,
  fee_sol: $fee_sol,
  confidence: 100
}

---

## Q2: "Check if a transaction succeeded or failed"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$error = $tx.meta.err

**Decision Point:** Check status
  BRANCH A ($error == null):
    $status = "success"
    $message = "Transaction completed successfully"
  BRANCH B ($error != null):
    $status = "failed"
    $message = "Transaction failed: " + $error

**Action:**
RETURN {
  signature: $signature,
  status: $status,
  error: $error,
  message: $message,
  confidence: 100
}

---

## Q3: "Get the block time of a transaction"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$block_time = $tx.blockTime

GUARD $block_time != null ELSE
  RETURN ERROR(message: "Block time not available")

**Action:**
RETURN {
  signature: $signature,
  block_time: $block_time,
  human_readable: TIMESTAMP_TO_DATE(timestamp: $block_time),
  confidence: 100
}

---

## Q4: "Count instructions in a transaction"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - COUNT (Data Processing)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$instructions = $tx.transaction.message.instructions
$instruction_count = COUNT(collection: $instructions)

**Action:**
RETURN {
  signature: $signature,
  instruction_count: $instruction_count,
  confidence: 100
}

---

## Q5: "Get compute units consumed by transaction"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$compute_units = $tx.meta.computeUnitsConsumed

GUARD $compute_units != null ELSE
  RETURN ERROR(message: "Compute units data not available")

**Action:**
RETURN {
  signature: $signature,
  compute_units: $compute_units,
  confidence: 100
}
"#.to_string()
}

fn generate_transaction_intermediate() -> String {
    r#"## Q1: "Analyze all instructions in a transaction"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP (Data Processing)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$instructions = $tx.transaction.message.instructions
$account_keys = $tx.transaction.message.accountKeys

// Map each instruction to its program
$instruction_details = MAP(
  collection: $instructions,
  fn: (ix, idx) => {
    index: idx,
    program_id: $account_keys[ix.programIdIndex],
    data: ix.data,
    accounts: ix.accounts
  }
)

**Action:**
RETURN {
  signature: $signature,
  instruction_count: COUNT(collection: $instructions),
  instructions: $instruction_details,
  confidence: 95
}

---

## Q2: "Calculate net balance changes for all accounts"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - MAP (Data Processing)

**Main Branch:**
$signature = INPUT(prompt: "Enter transaction signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

$account_keys = $tx.transaction.message.accountKeys
$pre_balances = $tx.meta.preBalances
$post_balances = $tx.meta.postBalances

// Calculate changes for each account
$balance_changes = []
FOR $i IN 0..COUNT(collection: $account_keys):
  $change = $post_balances[$i] - $pre_balances[$i]
  $balance_changes = APPEND(array: $balance_changes, item: {
    account: $account_keys[$i],
    pre_balance: $pre_balances[$i],
    post_balance: $post_balances[$i],
    change_lamports: $change,
    change_sol: $change / 1000000000
  })

**Action:**
RETURN {
  signature: $signature,
  balance_changes: $balance_changes,
  confidence: 95
}
"#.to_string()
}

// Add more generator functions for other categories...
fn generate_transaction_forensics() -> String {
    r#"## Q1: "Debug a failed transaction and identify root cause"

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
"#.to_string()
}

// Generic fallback for categories without specific generators
fn generate_generic_questions(cat_name: &str, level_name: &str) -> String {
    format!(r#"## Q1: "Sample {} question at {} level"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FILTER (Data Processing)

**Main Branch:**
$current_slot = getSlot()

TRY:
  $block = getBlock(slot: $current_slot)
CATCH FATAL:
  RETURN ERROR(message: "Block not available")

// Process data
$result = processData(block: $block)

**Action:**
RETURN {{
  slot: $current_slot,
  result: $result,
  confidence: 90
}}

---

## Q2: "Another sample {} question"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - Standard tools applicable to {}

**Main Branch:**
$data = INPUT(prompt: "Enter required data")

// Implementation details
$processed = processInput(data: $data)

**Action:**
RETURN {{
  input: $data,
  output: $processed,
  confidence: 95
}}
"#, cat_name, level_name, cat_name, cat_name)
}

// Placeholder generators - these should be expanded
fn generate_transaction_analysis() -> String { generate_generic_questions("Transaction Analysis", "Analysis") }
fn generate_transaction_patterns() -> String { generate_generic_questions("Transaction Analysis", "Patterns") }
fn generate_transaction_optimization() -> String { generate_generic_questions("Transaction Analysis", "Optimization") }
fn generate_transaction_historical() -> String { generate_generic_questions("Transaction Analysis", "Historical") }
fn generate_transaction_edge_cases() -> String { generate_generic_questions("Transaction Analysis", "Edge Cases") }
fn generate_transaction_expert() -> String { generate_generic_questions("Transaction Analysis", "Expert") }

fn generate_account_basic() -> String { generate_generic_questions("Account State", "Basic") }
fn generate_account_intermediate() -> String { generate_generic_questions("Account State", "Intermediate") }
fn generate_account_advanced() -> String { generate_generic_questions("Account State", "Advanced") }
fn generate_account_analysis() -> String { generate_generic_questions("Account State", "Analysis") }
fn generate_account_patterns() -> String { generate_generic_questions("Account State", "Patterns") }
fn generate_account_optimization() -> String { generate_generic_questions("Account State", "Optimization") }
fn generate_account_forensics() -> String { generate_generic_questions("Account State", "Forensics") }
fn generate_account_historical() -> String { generate_generic_questions("Account State", "Historical") }
fn generate_account_edge_cases() -> String { generate_generic_questions("Account State", "Edge Cases") }
fn generate_account_expert() -> String { generate_generic_questions("Account State", "Expert") }
