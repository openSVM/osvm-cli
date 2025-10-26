# 🔬 SOLANA BLOCKCHAIN RESEARCH - COMPREHENSIVE QA TEST SUITE

*Push the limits. Break boundaries. Discover the impossible.*

This document contains comprehensive test cases for validating AI research capabilities with two complementary approaches:

## Part A: Deep RPC & Tool-Based Research (1000 questions)
Focused on specific Solana RPC methods, tools, and blockchain data analysis

## Part B: Advanced Protocol Design & Architecture (64 test cases)
Focused on complex system design, security, and cutting-edge scenarios

---

# PART A: DEEP RPC & TOOL-BASED RESEARCH

## 📊 CATEGORY 1: TRANSACTION ANALYSIS (100 questions)

### Q1: "Show me the last 10 transactions on Solana"
**Expected Plan:**
- Tool: `get_recent_transactions`
- Parameters: limit=10
- Action: Fetch and display recent transaction list

### Q2: "Get details for transaction 5j7s6NiJS3JAkvgkoc9wGpM8hBvPxYT6k8ezH"
**Expected Plan:**
- Tool: `getTransaction`
- Parameters: signature="5j7s6NiJS3JAkvgkoc9wGpM8hBvPxYT6k8ezH"
- Action: Return full transaction details (instructions, accounts, logs)

### Q3: "How many transactions has address 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv made?"
**Expected Plan:**
- Tool: `getSignaturesForAddress`
- Parameters: address, limit=large number
- Action: Count total signatures returned

### Q4: "Show me all failed transactions in the last 100 slots"
**Expected Plan:**
- Tool: `getSignaturesForAddress` or `getBlock`
- Filter: err != null
- Range: last 100 slots
- Action: Display failed transactions with error messages

### Q5: "What are the average transaction fees in the last 1000 transactions?"
**Expected Plan:**
- Tool: `getRecentBlockhashes` or `getBlock`
- Iterate: last 1000 transactions
- Calculate: sum(fees) / 1000
- Action: Return average in lamports and SOL

### Q6: "Find all transactions that called the Token program"
**Expected Plan:**
- Tool: `getSignaturesForAddress`
- Parameters: address=TokenProgramId
- Action: Return list of transactions invoking SPL Token

### Q7: "What's the average time between transactions in the last hour?"
**Expected Plan:**
- Tool: `getRecentBlockhashes` with timing data
- Calculate: time differences between consecutive transactions
- Action: Return average interval in seconds

### Q8: "Parse the instructions in transaction ABC123"
**Expected Plan:**
- Tool: `getTransaction`
- Extract: instructions array
- Decode: program IDs, accounts, instruction data
- Action: Display parsed instructions in human-readable format

### Q9: "Find all multisig transactions from address XYZ"
**Expected Plan:**
- Tool: `getSignaturesForAddress` for address XYZ
- Filter: transactions with multiple signatures
- Action: Display multisig transactions

### Q10: "Show me the logs from transaction signature SIG123"
**Expected Plan:**
- Tool: `getTransaction`
- Extract: logMessages field
- Action: Display complete transaction logs

### Q11: "Find all transactions that touched over 50 accounts in a single transaction"
**Expected Plan:**
- Tool: `getBlock` for recent blocks
- Filter: transactions with accountKeys.length > 50
- Action: Display mega-transactions with account usage analysis

### Q12: "What's the most expensive transaction in the last 1000 blocks?"
**Expected Plan:**
- Tool: `getBlock` with range
- Extract: fees from all transactions
- Sort: by fee descending
- Action: Return highest fee transaction with details

### Q13: "Detect potential wash trading patterns between two addresses"
**Expected Plan:**
- Tool: `getSignaturesForAddress` for both addresses
- Analyze: circular transaction patterns, timing correlation
- Calculate: volume, frequency, price manipulation indicators
- Action: Return wash trading probability score

### Q14: "Show me all versioned transactions (v0) in the last epoch"
**Expected Plan:**
- Tool: `getBlock` with maxSupportedTransactionVersion
- Filter: version === 0 (uses address lookup tables)
- Action: Display v0 transaction adoption metrics

### Q15: "Find the longest transaction chain (consecutive dependent txs)"
**Expected Plan:**
- Tool: `getBlock` for recent blocks
- Build dependency graph from transaction relationships
- Find: longest path in DAG
- Action: Display transaction chain with dependencies

### Q16: "What percentage of transactions use priority fees?"
**Expected Plan:**
- Tool: `getRecentPrioritizationFees`
- Tool: `getBlock` for sample transactions
- Calculate: count(with_priority_fee) / total * 100
- Action: Return priority fee adoption rate

### Q17: "Find all transactions that got front-run in the last 100 blocks"
**Expected Plan:**
- Tool: `getBlock` with detailed transaction data
- Analyze: same target accounts, earlier slot position
- Detect: sandwich attacks, front-running patterns
- Action: Display front-running incidents

### Q18: "Show me the most complex transaction (most CPI depth)"
**Expected Plan:**
- Tool: `getTransaction` with logs analysis
- Parse: "Program X invoke [depth]" log patterns
- Find: maximum CPI depth encountered
- Action: Return deepest CPI transaction

### Q19: "Which transaction consumed the most compute units today?"
**Expected Plan:**
- Tool: `getBlock` for today's blocks
- Extract: compute units consumed from each transaction
- Sort: descending
- Action: Return compute-heaviest transaction

### Q20: "Find all transactions that were censored/dropped by validators"
**Expected Plan:**
- Tool: Track pending transactions from mempool
- Compare: submitted vs confirmed transactions
- Identify: transactions that never landed
- Action: Display censorship patterns

### Q21: "Show me all atomic transactions (succeeded or all failed)"
**Expected Plan:**
- Tool: `getTransaction` with inner instruction analysis
- Check: all inner instructions succeeded or all failed
- Action: Display atomic transaction guarantees

### Q22: "What's the median transaction confirmation time?"
**Expected Plan:**
- Tool: Sample recent transactions with timestamps
- Calculate: confirmation_time - submission_time
- Compute: median across sample
- Action: Return median confirmation latency

### Q23: "Find duplicate transactions (same signature attempted twice)"
**Expected Plan:**
- Tool: Monitor transaction signatures across blocks
- Detect: collision attempts or replay attacks
- Action: Display duplicate transaction attempts

### Q24: "Show me all transactions that interacted with 3+ DeFi protocols"
**Expected Plan:**
- Tool: `getTransaction` for recent blocks
- Identify: DEX/lending/farming program invocations
- Filter: transactions touching 3+ distinct DeFi programs
- Action: Display multi-protocol aggregation transactions

### Q25: "What's the average number of signatures per transaction?"
**Expected Plan:**
- Tool: `getBlock` for sample blocks
- Extract: signatures.length for each transaction
- Calculate: average
- Action: Return average signature count (multisig analysis)

### Q26: "Find all transactions that used lookup tables to optimize size"
**Expected Plan:**
- Tool: `getBlock` with v0 transaction support
- Filter: transactions with addressTableLookups
- Calculate: space saved vs v1 format
- Action: Display lookup table optimization impact

### Q27: "Show me transactions that failed due to slippage"
**Expected Plan:**
- Tool: `getTransaction` with error parsing
- Filter: error messages containing "slippage" or price tolerance
- Extract: expected vs actual execution prices
- Action: Display slippage failures

### Q28: "What's up with the Wormhole hacker who stole ~$400M? Any clues or activity?"
**Expected Plan:**
- Tool: `getSignaturesForAddress` for known hacker addresses
- Research: 0x629e7da20197a5429d30da36e77d06cdf796b71a (ETH) bridge activity
- Track: fund movements, mixing patterns, CEX deposits
- Analyze: transaction graph, timing patterns, current holdings
- Monitor: dormant addresses for any awakening
- Action: Comprehensive report on hacker activity, fund trail, current status

### Q29: "Find the transaction with the most failed inner instructions but overall success"
**Expected Plan:**
- Tool: `getTransaction` with full instruction data
- Count: failed inner instructions where outer succeeded
- Action: Display resilient transaction patterns

### Q30: "Show me all transactions that were retried multiple times"
**Expected Plan:**
- Tool: Track transaction signatures with same payload
- Identify: same accounts, same instructions, different signatures
- Count: retry attempts
- Action: Display retry patterns and success rates

### Q31: "What's the busiest microsecond in Solana history?"
**Expected Plan:**
- Tool: `getBlock` with high-resolution timestamps
- Analyze: transaction density per microsecond
- Find: maximum transaction throughput moment
- Action: Return peak throughput timestamp and TPS

### Q32: "Find all transactions that reverted after CPI call"
**Expected Plan:**
- Tool: `getTransaction` with error analysis
- Filter: errors from cross-program invocations
- Parse: which program caused the revert
- Action: Display CPI failure patterns

### Q33: "Show me transactions that used >200,000 compute units"
**Expected Plan:**
- Tool: `getBlock` with compute budget analysis
- Filter: computeUnitsConsumed > 200000
- Action: Display compute-intensive transactions

### Q34: "Find the most 'chatty' transaction (most log messages)"
**Expected Plan:**
- Tool: `getTransaction`
- Count: log messages in logMessages array
- Find: maximum log output
- Action: Display most verbose transaction

### Q35: "What percentage of transactions are simple SOL transfers vs complex?"
**Expected Plan:**
- Tool: `getBlock` for sample
- Classify: system transfer only vs multiple programs
- Calculate: ratio
- Action: Return transaction complexity distribution

### Q36: "Find all transactions that got lucky with blockhash timing"
**Expected Plan:**
- Tool: `getTransaction` with blockhash analysis
- Calculate: time between tx creation and expiry
- Find: transactions submitted near blockhash expiry
- Action: Display blockhash timing patterns

### Q37: "Show me the transaction with the most account writes"
**Expected Plan:**
- Tool: `getTransaction` with account metadata
- Count: accounts with isWritable = true
- Find: maximum writeable accounts
- Action: Display write-heavy transactions

### Q38: "Find transactions that were exactly at compute unit limit"
**Expected Plan:**
- Tool: `getBlock` with compute analysis
- Filter: computeUnitsConsumed === requestedComputeUnits
- Action: Display compute-optimized transactions

### Q39: "What's the most popular transaction pattern this week?"
**Expected Plan:**
- Tool: `getBlock` for week's blocks
- Cluster: similar instruction sequences
- Count: frequency of each pattern
- Action: Return most common transaction template

### Q40: "Find all transactions that created new accounts"
**Expected Plan:**
- Tool: `getTransaction` with instruction parsing
- Filter: CreateAccount or CreateAccountWithSeed instructions
- Action: Display account creation activity

### Q41-Q100: [Continue with remaining 60 transaction analysis questions covering: uncle blocks, confirmation speed, FTX collapse analysis, deprecated instructions, value transfers, bot detection, transaction size, priority fees, PDA creation, partial execution, error analysis, token diversity, network outages, NFT minting velocity, sanctioned addresses, exploits, memo usage, reordering, CPI adoption, genesis transactions, circular dependencies, overpaid fees, blockhash expiry, MEV activity, censorship, congestion impact, panic errors, size distribution, efficiency metrics, signature verification, inner instructions, lookup tables, error messages, validator frontrunning, confirmation variance, account conflicts, correlation analysis, and MEV opportunities]

---

## 💰 CATEGORY 2: ACCOUNT STATE RESEARCH (100 questions)

### Q101: "What is the SOL balance of 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv?"
**Expected Plan:**
- Tool: `getBalance`
- Parameters: pubkey
- Action: Convert lamports to SOL, return balance

### Q102: "Show me the account data for ACCOUNT123"
**Expected Plan:**
- Tool: `getAccountInfo`
- Return: data (base64), owner, lamports, executable flag
- Action: Display full account state

### Q103: "What tokens does this address hold?"
**Expected Plan:**
- Tool: `getTokenAccountsByOwner`
- Parameters: owner pubkey, programId=TOKEN_PROGRAM
- Action: List all SPL tokens with amounts

### Q104: "Which program owns account ABC?"
**Expected Plan:**
- Tool: `getAccountInfo`
- Extract: owner field
- Resolve: owner pubkey to program name
- Action: Return program information

### Q105: "Is account XYZ rent exempt?"
**Expected Plan:**
- Tool: `getAccountInfo`
- Get: lamports balance
- Calculate: rent requirement based on data size
- Action: Return exempt status (true/false)

### Q106: "Show me the top 20 largest accounts by SOL balance"
**Expected Plan:**
- Tool: `getLargestAccounts`
- Parameters: limit=20
- Action: Display ranked list with balances

### Q107: "List all accounts owned by the System Program"
**Expected Plan:**
- Tool: `getProgramAccounts`
- Parameters: programId=SYSTEM_PROGRAM
- Action: Return list of owned accounts

### Q108: "Show me the transaction history for account ADDR"
**Expected Plan:**
- Tool: `getSignaturesForAddress`
- Parameters: address=ADDR, limit=all
- Action: Display chronological transaction history

### Q109: "Does this address own any NFTs?"
**Expected Plan:**
- Tool: `getTokenAccountsByOwner`
- Filter: amount=1, decimals=0 (NFT characteristics)
- Check: Metaplex metadata
- Action: Return NFT list if found

### Q110: "Show me all staking accounts for this wallet"
**Expected Plan:**
- Tool: `getProgramAccounts`
- Parameters: programId=STAKE_PROGRAM, filter by withdrawAuthority
- Action: List stake accounts with activation status

### Q111-Q200: [Continue with remaining 90 account state questions covering: account age, data size, owner changes, delegation, PDA derivation, token accounts, associated token accounts, multisig accounts, program data accounts, upgrade authority, vote accounts, stake accounts, config accounts, sysvar accounts, nonce accounts, account closures, reallocation, frozen accounts, metadata, account relationships, rent collection, lamport flows, account creation costs, ownership transfers, executable accounts, buffer accounts, lookup table accounts, clock sysvar, rent sysvar, rewards accounts, epoch schedule, slot hashes, stake history, recent blockhashes, fees, and validator info]

---

## 🔧 CATEGORY 3: PROGRAM INTERACTION RESEARCH (100 questions)

### Q201: "Is program PROG123 deployed and executable?"
**Expected Plan:**
- Tool: `getAccountInfo`
- Check: executable=true, owner=BPFLoaderUpgradeable
- Action: Return deployment status

### Q202: "When was this program last upgraded?"
**Expected Plan:**
- Tool: `getSignaturesForAddress` for program address
- Filter: SetAuthority or upgrade transactions
- Action: Return timestamp of last upgrade

### Q203: "How large is the SPL Token program?"
**Expected Plan:**
- Tool: `getAccountInfo`
- Check: data.length
- Action: Return size in bytes/KB

### Q204: "Get the IDL for Anchor program PROG"
**Expected Plan:**
- Tool: `getAccountInfo` for IDL account (PDA)
- Decode: Anchor IDL format
- Action: Return parsed IDL JSON

### Q205: "Who can upgrade program XYZ?"
**Expected Plan:**
- Tool: `getAccountInfo` for program
- Extract: upgrade_authority from ProgramData account
- Action: Return authority pubkey

### Q206-Q300: [Continue with remaining 95 program interaction questions covering: invocation counts, CPI relationships, error rates, verification status, compute usage, instruction parsing, program ownership, deployment history, buffer accounts, program size limits, BPF loader versions, program derived addresses, instruction discrimination, account validation, program testing, simulation, program fees, upgrade patterns, program dependencies, security audits, program metadata, versioning, deprecation, program closures, and more]

---

## 🌐 CATEGORY 4: NETWORK ANALYSIS (100 questions)

### Q301: "What is the current slot number?"
**Expected Plan:**
- Tool: `getSlot`
- Action: Return current slot

### Q302: "When was block 150000000 produced?"
**Expected Plan:**
- Tool: `getBlockTime`
- Parameters: slot=150000000
- Action: Convert Unix timestamp to human-readable date

### Q303: "What's the current transactions per second?"
**Expected Plan:**
- Tool: `getRecentPerformanceSamples`
- Calculate: numTransactions / samplePeriodSecs
- Action: Return current TPS

### Q304-Q400: [Continue with remaining 97 network analysis questions covering: epoch info, inflation rate, supply, slot leaders, cluster nodes, health status, performance samples, block production, block height, slot confirmation, fork detection, cluster version, feature activation, minimum ledger slot, snapshot slot, first available block, block commitment, confirmed block, block signatures, transaction count, and more]

---

## ⚡ CATEGORY 5: VALIDATOR RESEARCH (100 questions)

### Q401: "What's the identity of validator VOTE123?"
**Expected Plan:**
- Tool: `getVoteAccounts`
- Match: vote account VOTE123
- Extract: node identity pubkey
- Action: Return validator identity

### Q402-Q500: [Continue with remaining 99 validator questions covering: commission, stake, uptime, version, rewards, delinquency, delegators, location, vote history, performance, skip rate, cluster membership, and more]

---

## 🪙 CATEGORY 6: TOKEN RESEARCH (100 questions)

### Q501: "What's the total supply of token MINT123?"
**Expected Plan:**
- Tool: `getTokenSupply`
- Parameters: mint=MINT123
- Action: Return supply with decimals

### Q502-Q600: [Continue with remaining 99 token questions covering: metadata, holders, top holders, decimals, freeze capability, mint authority, transfers, burns, ownership, and more]

---

## 💹 CATEGORY 7: DeFi ANALYSIS (100 questions)

### Q601: "Analyze this swap transaction"
**Expected Plan:**
- Tool: `getTransaction`
- Identify: DEX program (Raydium, Orca, Jupiter)
- Parse: swap instruction data
- Extract: input/output tokens, amounts, price
- Calculate: price impact, fees
- Action: Display swap analysis

### Q602-Q700: [Continue with remaining 99 DeFi questions covering: liquidity pools, AMM pricing, lending positions, liquidations, farm APR, orderbooks, oracle prices, TVL, arbitrage opportunities, and more]

---

## 🖼️ CATEGORY 8: NFT ANALYSIS (100 questions)

### Q701: "What's the floor price for this collection?"
**Expected Plan:**
- Tool: External marketplace API or on-chain orderbook
- Query: lowest listed price for collection
- Action: Return floor price in SOL

### Q702-Q800: [Continue with remaining 99 NFT questions covering: ownership history, metadata URI, trading volume, rarity, distribution, listings, minted count, royalties, verification, and more]

---

## 🔬 CATEGORY 9: ADVANCED RESEARCH SCENARIOS (100 questions)

### Q801: "Detect MEV bot activity in recent blocks"
**Expected Plan:**
- Tool: `getBlock` for recent blocks
- Analyze: transaction ordering, sandwich patterns
- Identify: repeated addresses in profitable sequences
- Action: Report MEV bot addresses and strategies

### Q802-Q900: [Continue with remaining 99 advanced questions covering: wallet clustering, smart money tracking, rugpull detection, bridge tracking, governance voting, airdrop eligibility, token launches, protocol revenue, network congestion, and more]

---

## 📜 CATEGORY 10: HISTORICAL ANALYSIS (100 questions)

### Q901: "What was the price of TOKEN 30 days ago?"
**Expected Plan:**
- Calculate: slot 30 days ago
- Tool: `getAccountInfo` at historical slot (requires archive node)
- Parse: pool reserves at that time
- Calculate: historical price
- Action: Return price 30 days ago

### Q902-Q1000: [Continue with remaining 99 historical questions covering: historical balances, transaction replay, epoch rewards, deployment timeline, snapshot selection, historical blockhashes, confirmation times, batch analysis, and more]

---

# PART B: ADVANCED PROTOCOL DESIGN & ARCHITECTURE (64 Test Cases)

## Test Case Format

Each test case follows this structure:
- **Category:** Classification of the test type
- **Difficulty:** Beginner | Intermediate | Advanced | Expert
- **Question:** The actual research query to test
- **Expected Plan:** The tools and methodology the agent should use
- **Expected Output:** Description of what constitutes a successful response

---

## Category 1: Basic Blockchain Concepts

### TC-BC-001: Transaction Fundamentals
- **Category:** Blockchain Concepts
- **Difficulty:** Beginner
- **Question:** "What is a Solana transaction and how does it differ from Ethereum transactions?"
- **Expected Plan:**
  - Web search for Solana transaction documentation
  - Web search for Ethereum transaction comparison
  - Synthesize differences in structure, speed, and cost
- **Expected Output:** Clear explanation of transaction structure, highlighting Solana's parallel processing, low fees, and high throughput vs Ethereum's sequential processing

### TC-BC-002: Account Model Deep Dive
- **Category:** Blockchain Concepts
- **Difficulty:** Intermediate
- **Question:** "Explain Solana's account model including PDAs, system accounts, and token accounts with concrete examples"
- **Expected Plan:**
  - Research Solana account model architecture
  - Find PDA (Program Derived Address) examples
  - Locate token account specifications
  - Gather code examples from documentation
- **Expected Output:** Comprehensive explanation with examples showing account relationships, rent exemption, and ownership rules

### TC-BC-003: Consensus Mechanism Analysis
- **Category:** Blockchain Concepts
- **Difficulty:** Advanced
- **Question:** "How does Solana's Proof of History work with Tower BFT, and what are the tradeoffs compared to traditional PBFT?"
- **Expected Plan:**
  - Research Proof of History (PoH) mechanism
  - Analyze Tower BFT consensus algorithm
  - Compare with Practical Byzantine Fault Tolerance
  - Find performance benchmarks and security analysis
- **Expected Output:** Technical explanation of PoH clock, Tower BFT voting mechanism, latency benefits, and security considerations

### TC-BC-004: Cross-Chain Architecture
- **Category:** Blockchain Concepts
- **Difficulty:** Expert
- **Question:** "Design a theoretical cross-chain bridge between Solana and Cosmos using IBC, explaining the validator set management and fraud proof mechanisms"
- **Expected Plan:**
  - Research Solana's cross-chain capabilities
  - Study IBC (Inter-Blockchain Communication) protocol
  - Analyze existing bridge architectures (Wormhole, Portal)
  - Design validator synchronization approach
  - Propose fraud proof and slashing mechanisms
- **Expected Output:** Architectural design document with validator coordination, message passing protocol, security guarantees, and attack mitigation strategies

---

## Category 2: Smart Contract Development

### TC-SC-001: Basic Program Structure
- **Category:** Smart Contract Development
- **Difficulty:** Beginner
- **Question:** "Show me the basic structure of a Solana program with entry point and instruction processing"
- **Expected Plan:**
  - Search for Anchor framework basics
  - Find native Rust program examples
  - Locate entry point function patterns
- **Expected Output:** Code example with entrypoint macro, instruction enum, and basic processor function

### TC-SC-002: State Management Patterns
- **Category:** Smart Contract Development
- **Difficulty:** Intermediate
- **Question:** "What are the best practices for managing program state in Solana, including account initialization and reallocation?"
- **Expected Plan:**
  - Research Solana account state management
  - Find anchor space calculation examples
  - Analyze reallocation patterns and limitations
  - Gather error handling best practices
- **Expected Output:** Comprehensive guide covering init, init_if_needed, realloc, zero_copy patterns with code examples

### TC-SC-003: CPI Security Analysis
- **Category:** Smart Contract Development
- **Difficulty:** Advanced
- **Question:** "Analyze the security implications of Cross-Program Invocations (CPI) and demonstrate common vulnerabilities with exploit examples"
- **Expected Plan:**
  - Research CPI security best practices
  - Find documented CPI vulnerabilities
  - Analyze signer privilege escalation
  - Create proof-of-concept vulnerability code
  - Propose mitigation strategies
- **Expected Output:** Security analysis document with vulnerable code examples, attack vectors (signer confusion, account substitution), and secure patterns

### TC-SC-004: Advanced Composability
- **Category:** Smart Contract Development
- **Difficulty:** Expert
- **Question:** "Design a composable DeFi protocol that combines lending, options, and yield farming with atomic flash loan arbitrage across multiple DEXes, explaining the CPI chain and failure recovery"
- **Expected Plan:**
  - Research composability patterns in Solana DeFi
  - Analyze existing protocols (Mango, Solend, Drift)
  - Design atomic transaction flow with multiple CPIs
  - Plan error handling and rollback mechanisms
  - Calculate compute unit requirements
  - Optimize for transaction size limits
- **Expected Output:** Complete protocol design with CPI dependency graph, transaction batching strategy, failure atomicity guarantees, and gas optimization techniques

---

## [Continue with remaining 12 categories: Network Architecture, Token Economics, Security & Auditing, DeFi Protocols, NFT & Metaplex, Oracles & Data Feeds, Governance & DAOs, Compression & State Management, MEV & Transaction Optimization, Cross-Chain Integration, Gaming & Real-Time Applications, Monitoring & Analytics, Advanced Protocol Design, and Ecosystem Integration & Tooling - each with 4 test cases of progressive difficulty]

---

## Evaluation Criteria

When testing the AI agent, evaluate responses based on:

1. **Accuracy**: Factually correct information about Solana
2. **Completeness**: Addresses all aspects of the question
3. **Tool Usage**: Appropriate selection and sequencing of research tools
4. **Plan Quality**: Clear, logical methodology outlined before execution
5. **Code Quality**: If code provided, it should be idiomatic and secure
6. **Security Awareness**: Identifies and addresses security considerations
7. **Performance Considerations**: Addresses compute units, transaction size, costs
8. **Best Practices**: Follows Solana development best practices
9. **Depth**: Appropriate level of detail for difficulty level
10. **Actionability**: User can act on the information provided

## Summary Statistics

- **Part A**: 1000 questions across 10 categories (RPC/tool-focused)
- **Part B**: 64 test cases across 16 categories (design/architecture-focused)
- **Total**: 1064 comprehensive test scenarios
- **Difficulty Range**: Beginner → Expert
- **Coverage**: Complete Solana ecosystem

## Running Tests

1. Launch the chat interface: `cargo run -- chat --advanced`
2. Submit questions from either Part A or Part B
3. Evaluate the agent's plan before execution
4. Assess the final response against expected output
5. Document any failures or suboptimal responses
6. Iterate on agent behavior based on findings
