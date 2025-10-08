# Transaction Analysis - Basic Queries (Q1-Q100)

## Q1: "Show me the last 10 transactions on Solana"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentBlockhash` → get current slot
  - Tool: `getBlock` with current slot → get recent transactions
  - Limit: 10 transactions
- **Decision Point:** If no transactions in current block
  - Branch A: Query previous 5 blocks until 10 transactions found
  - Branch B: Use `getSignaturesForAddress` for high-activity address
- **Action:** Display transaction list with signatures, slot, and status

## Q2: "Get details for transaction 5j7s6NiJS3JAkvgkoc9wGpM8hBvPxYT6k8ezH"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getTransaction` with signature
  - Parameters: maxSupportedTransactionVersion=0
- **Decision Point:** Check transaction.meta for errors
  - **If Success:** Extract instructions, accounts, logs, compute units
  - **If Failed:**
    - Branch A: Parse error message
    - Branch B: Tool: `simulateTransaction` to understand failure
    - Branch C: Find similar successful transactions for comparison
- **Action:** Return comprehensive transaction breakdown with decoded instructions

## Q3: "How many transactions has address 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv made?"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSignaturesForAddress` with limit=1000
  - Loop: Paginate using `before` parameter until no more results
  - Count: Total signatures
- **Decision Point:** If count > 10000
  - Branch A: Provide exact count
  - Branch B: Sample and estimate total with statistical confidence
- **Secondary Analysis:**
  - Calculate: transactions per day/week/month
  - Identify: Most active time periods
- **Action:** Return total count + activity timeline

## Q4: "Show me all failed transactions in the last 100 slots"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSlot` → current_slot
  - Loop: `getBlock` for slots [current_slot - 100...current_slot]
  - Filter: transactions where meta.err !== null
- **Decision Point:** Check failure patterns
  - **If many similar errors:**
    - Branch A: Group by error type
    - Branch B: Identify if network-wide issue or address-specific
    - Tool: `getRecentPerformanceSamples` to check congestion
  - **If diverse errors:**
    - Parse each error individually
- **Action:** Display failed transactions with error categorization

## Q5: "What are the average transaction fees in the last 1000 transactions?"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentBlockhash` → current slot
  - Tool: `getBlock` for last N blocks until 1000 transactions collected
  - Extract: meta.fee from each transaction
  - Calculate: mean(fees), median(fees), std_dev(fees)
- **Decision Point:** Check fee distribution
  - **If high variance:**
    - Branch A: Separate priority fees from base fees
    - Tool: `getRecentPrioritizationFees` for context
    - Branch B: Identify outliers (>2 std dev)
    - Branch C: Group by transaction type (simple transfer vs complex)
  - **If normal distribution:**
    - Provide simple statistics
- **Secondary Analysis:**
  - Compare: fees during congestion vs normal periods
  - Tool: `getRecentPerformanceSamples` for TPS context
- **Action:** Return average in lamports/SOL + distribution analysis + context

## Q6: "Find all transactions that called the Token program"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSignaturesForAddress` for TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
  - Parameters: limit=1000
- **Decision Point:** User wants recent or all-time?
  - **Branch A (Recent):** Last 24 hours only
  - **Branch B (All-time):** Paginate through all history
  - Ask AI: Clarify user intent based on query context
- **Secondary Analysis:**
  - Group by instruction type: Transfer, MintTo, Burn, Approve, etc.
  - Tool: `getTransaction` for sample transactions to identify patterns
  - Calculate: transaction type distribution
- **Action:** Display token program activity with breakdown by instruction type

## Q7: "What's the average time between transactions in the last hour?"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSlot` → current_slot
  - Calculate: slots_per_hour ≈ 9000 slots (assuming 400ms/slot)
  - Tool: `getBlock` for last ~9000 slots with timestamps
  - Extract: blockTime and transaction count per block
- **Decision Point:** Check data completeness
  - **If missing blocks:**
    - Branch A: Use `getBlocks` to identify skipped slots
    - Adjust calculation for actual block production
  - **If complete:**
    - Direct calculation
- **Calculate:**
  - total_time_seconds = latest_blockTime - earliest_blockTime
  - total_transactions = sum of all transaction counts
  - average_interval = total_time_seconds / total_transactions
- **Secondary Analysis:**
  - Identify: peak and off-peak intervals
  - Tool: `getRecentPerformanceSamples` for TPS trends
- **Action:** Return average interval in seconds/milliseconds + trend analysis

## Q8: "Parse the instructions in transaction ABC123"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getTransaction` with signature="ABC123"
  - Extract: transaction.message.instructions
- **Decision Point:** Check instruction types
  - **For each instruction:**
    - Branch A (Native programs): Use known instruction layouts
      - System: CreateAccount, Transfer, Assign, etc.
      - Token: Transfer, MintTo, Burn, etc.
    - Branch B (Custom programs):
      - Tool: `getAccountInfo` for program address
      - If Anchor: Tool: derive IDL PDA and `getAccountInfo` to fetch IDL
      - Parse using IDL or raw discriminator
    - Branch C (Unknown): Display raw hex data
- **For CPIs:**
  - Extract inner instructions from meta.innerInstructions
  - Recursively parse each level
  - Build call tree
- **Action:** Return human-readable instruction breakdown with call hierarchy

## Q9: "Find all multisig transactions from address XYZ"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSignaturesForAddress` for address XYZ
  - Tool: `getTransaction` for each signature
  - Filter: transactions.signatures.length > 1
- **Decision Point:** Identify multisig type
  - **Branch A (SPL Multisig):**
    - Look for multisig program invocations
    - Tool: `getAccountInfo` for multisig account to get signer config
  - **Branch B (Squads/Goki):**
    - Identify specific multisig program
    - Parse transaction approval flow
  - **Branch C (Custom):**
    - Analyze signature patterns
- **Secondary Analysis:**
  - Extract: All signer addresses
  - Calculate: Average signers per transaction
  - Identify: Which signers sign most frequently
- **Action:** Display multisig transactions with signer analysis

## Q10: "Show me the logs from transaction signature SIG123"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getTransaction` with signature="SIG123"
  - Extract: meta.logMessages
- **Decision Point:** Parse log content
  - **Branch A (Success logs):**
    - Identify: Program invocations and returns
    - Parse: "Program X invoke [depth]" patterns
    - Extract: Data logs and return values
  - **Branch B (Error logs):**
    - Identify: Error location (which program/instruction)
    - Parse: Error code and message
    - Tool: `simulateTransaction` if error unclear
  - **Branch C (Complex CPIs):**
    - Build: Visual call tree from invoke/return patterns
    - Map: Each log line to specific instruction
- **Action:** Display formatted logs with syntax highlighting and call tree

## Q11: "Find all transactions that touched over 50 accounts"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentBlockhash` → current_slot
  - Tool: `getBlock` for recent blocks
  - Filter: transaction.message.accountKeys.length > 50
- **Decision Point:** Analyze account usage
  - **For each mega-transaction:**
    - Count: writable vs readonly accounts
    - Tool: `getTransaction` with full details
    - Identify: Which programs used most accounts
    - Check: If using address lookup tables
- **Secondary Analysis:**
  - Compare: v0 transactions (with lookup tables) vs legacy
  - Calculate: Average accounts per instruction
  - Identify: Most common patterns for high account usage
- **Action:** Display mega-transactions with account usage breakdown

## Q12: "What's the most expensive transaction in the last 1000 blocks?"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSlot` → current_slot
  - Tool: `getBlock` for [current_slot - 1000...current_slot]
  - Extract: meta.fee from all transactions
  - Sort: descending by fee
  - Top: highest fee transaction
- **Decision Point:** Analyze why fee is high
  - Tool: `getTransaction` for top transaction
  - **Branch A (High priority fee):**
    - Calculate: computeUnitPrice * computeUnits
    - Tool: `getRecentPrioritizationFees` for context
    - Determine: If fee was necessary or overpaid
  - **Branch B (High compute units):**
    - Analyze: Instruction complexity
    - Identify: Which program consumed most compute
  - **Branch C (Network congestion):**
    - Tool: `getRecentPerformanceSamples` for that time period
    - Check: If congestion justified high fee
- **Action:** Return most expensive transaction with cost breakdown and justification analysis

## Q13: "Detect potential wash trading patterns between two addresses"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSignaturesForAddress` for both addresses
  - Extract: All transactions for time range
  - Cross-reference: Find common transactions
- **Decision Point:** Analyze transaction patterns
  - **Branch A (Direct transfers):**
    - Identify: Circular SOL/token flows
    - Calculate: Volume in each direction
    - Check: Time intervals between transfers
    - Pattern: A→B→A cycles
  - **Branch B (DEX trades):**
    - Tool: `getTransaction` for each trade
    - Parse: Swap amounts and prices
    - Identify: Trades at unusual prices
    - Check: If same tokens traded back and forth
    - Tool: Oracle prices for comparison
  - **Branch C (Statistical analysis):**
    - Calculate: Transaction timing correlation
    - Measure: Volume concentration at specific times
    - Compare: Against normal trading patterns
- **Risk Score Calculation:**
  - Factor: Circular flow volume
  - Factor: Timing correlation coefficient
  - Factor: Price deviation from market
  - Factor: Transaction frequency
  - Output: Wash trading probability (0-100%)
- **Action:** Return detailed report with probability score and evidence

## Q14: "Show me all versioned transactions (v0) in the last epoch"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getEpochInfo` → current epoch, slots in epoch
  - Tool: `getSlot` → current_slot
  - Calculate: epoch_start_slot = current_slot - (current_slot % slotsInEpoch)
  - Loop: `getBlock` for all slots in epoch with maxSupportedTransactionVersion=0
  - Filter: transactions where version === 0
- **Decision Point:** Analyze adoption
  - Count: Total v0 transactions vs legacy
  - **Branch A (High adoption):**
    - Calculate: Percentage of v0 transactions
    - Identify: Which programs use v0 most
    - Measure: Average space savings
  - **Branch B (Low adoption):**
    - Identify: Barriers to adoption
    - Check: Which programs could benefit but don't use
- **Secondary Analysis:**
  - For v0 transactions:
    - Count: Address lookup tables used
    - Calculate: Average account references via ALTs
    - Measure: Transaction size reduction
- **Action:** Return v0 adoption metrics with space savings analysis

## Q15: "Find the longest transaction chain (consecutive dependent txs)"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentBlockhash` → current_slot
  - Tool: `getBlock` for recent blocks
  - Build: Transaction dependency graph
    - Node: Each transaction
    - Edge: If transaction B reads account written by transaction A
- **Decision Point:** Analyze dependencies
  - **Branch A (Build forward chain):**
    - For each transaction:
      - Track: Which accounts it writes
      - Find: Next transactions reading those accounts
      - Build: Dependency chain forward
  - **Branch B (Build backward chain):**
    - For each transaction:
      - Track: Which accounts it reads
      - Find: Previous transactions that wrote those accounts
      - Build: Dependency chain backward
- **Graph Analysis:**
  - Algorithm: Find longest path in DAG
  - Identify: Critical path transactions
  - Calculate: Maximum dependency depth
  - Check: If any circular dependencies (shouldn't exist)
- **Secondary Analysis:**
  - Identify: Bottleneck transactions
  - Calculate: Parallelization opportunities
  - Measure: Actual vs theoretical throughput
- **Action:** Display longest chain with dependency graph visualization

## Q16: "What percentage of transactions use priority fees?"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentPrioritizationFees` → get current fee levels
  - Tool: `getBlock` for sample blocks
  - Extract: Compute budget instructions from transactions
  - Parse: SetComputeUnitPrice instructions
- **Decision Point:** Sampling strategy
  - **Branch A (Recent snapshot):**
    - Sample: Last 100 blocks
    - Fast but less accurate
  - **Branch B (Time-based analysis):**
    - Sample: Blocks across 24 hours
    - Slower but captures daily patterns
  - AI: Choose based on user urgency vs accuracy needs
- **Analysis:**
  - Count: Transactions with SetComputeUnitPrice instruction
  - Calculate: percentage = (with_priority / total) * 100
  - Group by: Priority fee ranges (0-100, 100-1000, 1000+)
- **Correlation Analysis:**
  - Tool: `getRecentPerformanceSamples`
  - Check: Priority fee usage vs network congestion
  - Identify: Trends over time
- **Action:** Return adoption percentage + distribution + trends

## Q17: "Find all transactions that got front-run in the last 100 blocks"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getSlot` → current_slot
  - Tool: `getBlock` for [current_slot - 100...current_slot]
- **Decision Point:** Identify front-running patterns
  - **Branch A (Sandwich attacks on DEX):**
    - For each block:
      - Find: DEX swap transactions
      - Check: If surrounded by same-pool swaps from same address
      - Pattern: buy → victim swap → sell
      - Tool: `getTransaction` for each to parse amounts
      - Calculate: Profit from sandwich
  - **Branch B (Liquidation front-running):**
    - Find: Lending protocol liquidation txs
    - Check: If multiple liquidators target same position
    - Identify: Which tx landed first
      - Compare: Priority fees paid
  - **Branch C (NFT snip ing):**
    - Find: NFT purchase transactions
    - Check: If multiple txs target same NFT
    - Identify: Winner and failed transactions
- **Advanced Analysis:**
  - For each front-run transaction:
    - Extract: Original tx details (may have failed)
    - Compare: Priority fees (attacker vs victim)
    - Calculate: MEV captured by front-runner
    - Identify: Front-runner address patterns
- **Action:** Display front-running incidents with profit analysis

## Q18: "Show me the most complex transaction (most CPI depth)"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getRecentBlockhash` → current_slot
  - Tool: `getBlock` for recent blocks
  - Tool: `getTransaction` for each transaction
  - Parse: meta.logMessages for "Program X invoke [N]" patterns
- **Decision Point:** Measure complexity
  - **Branch A (CPI depth):**
    - Extract: Maximum depth number from logs
    - Pattern: "invoke [4]" means depth 4
    - Track: Deepest CPI chain
  - **Branch B (Total CPI count):**
    - Count: Total inner instructions
    - Alternative complexity measure
  - **Branch C (Unique programs):**
    - Count: Distinct programs invoked
    - Measure composability
- **For winner transaction:**
  - Build: Complete call tree
  - Identify: Each program in chain
  - Extract: What each program did
  - Calculate: Compute units per program
  - Map: Data flow through CPI chain
- **Action:** Return most complex transaction with visual call tree and analysis

## Q19: "Which transaction consumed the most compute units today?"
**Expected Plan:**
- **Main Branch:**
  - Get current time and calculate: slots_since_midnight
  - Tool: `getSlot` → current_slot
  - Calculate: first_slot_today = current_slot - (current_time - midnight_timestamp) / 0.4
  - Loop: `getBlock` for all slots today
  - Extract: meta.computeUnitsConsumed from each transaction
  - Sort: descending
  - Top: highest compute transaction
- **Decision Point:** Analyze high compute usage
  - Tool: `getTransaction` for top transaction
  - **Branch A (Complex computation):**
    - Parse instructions
    - Identify: Which instruction consumed most
    - Check: If compute efficiently used
  - **Branch B (Inefficient code):**
    - Check: requested vs consumed compute
    - Identify: Wasteful patterns
  - **Branch C (Multiple operations):**
    - Count: Total instructions
    - Calculate: Compute per instruction
    - Check: If transaction could be split
- **Comparison:**
  - Tool: Get compute limit (1.4M CU)
  - Calculate: Percentage of limit used
  - Check: If approaching limit
- **Action:** Return highest compute transaction with efficiency analysis

## Q20: "Find all transactions that were censored/dropped by validators"
**Expected Plan:**
- **Main Branch:**
  - Monitor: Mempool for pending transactions (requires special RPC)
  - Track: Transaction submissions
  - Wait: Reasonable timeout period
  - Check: Which never landed on-chain
- **Decision Point:** Identify censorship vs network issues
  - **For each dropped transaction:**
    - **Branch A (Expired blockhash):**
      - Check: blockhash age
      - Compare: Against 150 slot limit
      - Conclusion: Network delay, not censorship
    - **Branch B (Insufficient fee):**
      - Tool: `getRecentPrioritizationFees`
      - Check: If fee too low for congestion
      - Conclusion: Economic filtering, not censorship
    - **Branch C (Content-based filtering):**
      - Check: If interacts with sanctioned addresses
      - Check: If validator consistently drops similar txs
      - Check: If other validators accept it
      - Conclusion: Possible censorship
- **Statistical Analysis:**
  - Group by: Validator who dropped
  - Identify: Patterns in dropped transactions
  - Compare: Drop rates across validators
- **Action:** Display dropped transactions with censorship likelihood scores

## Q21-Q100: [Continue with 80 more sophisticated questions with complex conditional branching plans...]

## Q21: "Show me all atomic transactions (succeeded or all failed)"
**Expected Plan:**
- **Main Branch:**
  - Tool: `getBlock` for recent blocks
  - Tool: `getTransaction` with full details
  - Check: meta.err status
  - Parse: meta.innerInstructions
- **Decision Point:** Verify atomicity
  - **Branch A (All succeeded):**
    - Verify: All inner instructions succeeded
    - Check: No partial failures in logs
    - Confirm: Final state matches expectations
  - **Branch B (All failed):**
    - Identify: First instruction that failed
    - Verify: No state changes occurred
    - Check: All rollbacks executed properly
  - **Branch C (Partial - SHOULD NOT EXIST):**
    - Flag: Potential bug or security issue
    - Deep dive: Transaction analysis
    - Report: Anomaly
- **Action:** Display verified atomic transactions

## Q22: "What's the median transaction confirmation time?"
**Expected Plan:**
- **Main Branch:**
  - Sample: 1000 recent transactions
  - Track: Submission timestamp (requires mempool access)
  - Track: Confirmation timestamp from blockTime
  - Calculate: confirmation_time - submission_time
- **Decision Point:** Handle missing submission times
  - **Branch A (Have submission times):**
    - Direct calculation of latency
  - **Branch B (No submission times):**
    - Estimate: Based on blockhash age
    - Use: Statistical modeling
  - **Branch C (Mixed data):**
    - Separate: Known vs estimated
    - Combine: With confidence intervals
- **Statistical Analysis:**
  - Calculate: median, mean, p95, p99
  - Identify: Outliers
  - Tool: `getRecentPerformanceSamples` for network context
  - Correlate: Confirmation time with priority fees
- **Action:** Return median with full distribution statistics

## Q23: "Find duplicate transactions (same signature attempted twice)"
**Expected Plan:**
- **Main Branch:**
  - Monitor: Transaction submissions (mempool)
  - Track: Signature = hash(message + signatures)
  - Identify: Signature collisions
- **Decision Point:** Classify collision type
  - **Branch A (Replay attack attempt):**
    - Check: If identical transaction submitted twice
    - Verify: Nonce or recent blockhash prevents replay
    - Status: Should fail, security working
  - **Branch B (Signature collision):**
    - Analyze: Hash collision (EXTREMELY rare)
    - Calculate: Probability
    - Check: If malicious or accidental
  - **Branch C (Network duplicate):**
    - Check: If same tx from different nodes
    - Status: Normal network behavior
- **Action:** Display collision details with security implications

[Continue with remaining 77 questions, each with sophisticated multi-branch conditional plans...]
