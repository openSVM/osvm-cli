# Quick Reference Guide
**Solana Research Q&A Dataset**

## Navigation

### By Category
```
01_transaction_analysis/  - Transaction fees, instructions, CPIs
02_account_state/         - Balances, ownership, PDAs
03_program_interaction/   - Deployments, upgrades, invocations
04_network_analysis/      - Slots, blocks, TPS, epochs
05_validator_research/    - Validators, stake, rewards
06_token_research/        - Token supply, holders, transfers
07_defi_analysis/         - DEX, lending, liquidity
08_nft_analysis/          - NFTs, collections, trading
09_advanced_scenarios/    - MEV, rugpulls, security
10_historical_analysis/   - Time-series, trends, replays
```

### By Difficulty
```
01_basic.md        - Q1-Q100     (5-10s, free)
02_intermediate.md - Q101-Q200   (5-10s, free)
03_advanced.md     - Q201-Q300   (10-30s, ~0.001 SOL)
04_analysis.md     - Q301-Q400   (10-30s, ~0.001 SOL)
05_patterns.md     - Q401-Q500   (10-30s, ~0.001 SOL)
06_optimization.md - Q501-Q600   (30-60s, ~0.005 SOL)
07_forensics.md    - Q601-Q700   (30-60s, ~0.005 SOL)
08_historical.md   - Q701-Q800   (30-60s, ~0.005 SOL)
09_edge_cases.md   - Q801-Q900   (1-5min, ~0.01 SOL)
10_expert.md       - Q901-Q1000  (1-5min, ~0.01 SOL)
```

## Finding Specific Questions

### By Question Number
- Q1-Q1000: Category 01 (Transaction Analysis)
- Q1-Q1000: Category 02 (Account State)
- ... (each category has Q1-Q1000)

### By Topic
**Transaction Questions:**
- Fees: 01_transaction_analysis/01_basic.md
- CPI analysis: 01_transaction_analysis/02_intermediate.md
- MEV: 01_transaction_analysis/10_expert.md

**Account Questions:**
- Balances: 02_account_state/01_basic.md
- PDAs: 02_account_state/02_intermediate.md
- Forensics: 02_account_state/10_expert.md

**Program Questions:**
- Deployments: 03_program_interaction/01_basic.md
- Security: 03_program_interaction/07_forensics.md
- Exploits: 03_program_interaction/10_expert.md

**Network Questions:**
- TPS: 04_network_analysis/01_basic.md
- Performance: 04_network_analysis/03_advanced.md
- Diagnostics: 04_network_analysis/10_expert.md

**Validator Questions:**
- Stake: 05_validator_research/01_basic.md
- Rewards: 05_validator_research/02_intermediate.md
- Analytics: 05_validator_research/10_expert.md

**Token Questions:**
- Supply: 06_token_research/01_basic.md
- Distribution: 06_token_research/03_advanced.md
- Anomalies: 06_token_research/07_forensics.md

**DeFi Questions:**
- Pools: 07_defi_analysis/02_intermediate.md
- Strategies: 07_defi_analysis/03_advanced.md
- Exploits: 07_defi_analysis/07_forensics.md

**NFT Questions:**
- Collections: 08_nft_analysis/02_intermediate.md
- Trading: 08_nft_analysis/03_advanced.md
- Fraud: 08_nft_analysis/07_forensics.md

**Security Questions:**
- MEV: 09_advanced_scenarios/02_intermediate.md
- Attacks: 09_advanced_scenarios/03_advanced.md
- Rugpulls: 09_advanced_scenarios/07_forensics.md

**Historical Questions:**
- Trends: 10_historical_analysis/04_analysis.md
- Replays: 10_historical_analysis/08_historical.md
- Time-series: 10_historical_analysis/10_expert.md

## Common RPC Tools by Category

### Transaction Analysis
- `getTransaction` - Get transaction details
- `getRecentBlockhash` - Get recent blockhash
- `simulateTransaction` - Simulate transaction
- `getSignaturesForAddress` - Get signatures for address

### Account State
- `getAccountInfo` - Get account details
- `getBalance` - Get SOL balance
- `getProgramAccounts` - Get program accounts
- `getMultipleAccounts` - Batch account queries

### Program Interaction
- `getProgramAccounts` - Query program accounts
- `getAccountInfo` - Get program data
- `simulateTransaction` - Test program calls
- `getTransaction` - Analyze invocations

### Network Analysis
- `getSlot` - Current slot number
- `getBlock` - Block details
- `getEpochInfo` - Epoch information
- `getClusterNodes` - Cluster nodes
- `getRecentPerformanceSamples` - Performance metrics

### Validator Research
- `getVoteAccounts` - Vote accounts
- `getStakeActivation` - Stake activation
- `getInflationReward` - Reward calculation
- `getLeaderSchedule` - Leader schedule

### Token Research
- `getTokenAccountsByOwner` - Owner's tokens
- `getTokenSupply` - Token supply
- `getTokenAccountBalance` - Token balance
- `getTokenLargestAccounts` - Top holders

### DeFi/NFT/Advanced
- `getProgramAccounts` - Protocol state
- `getAccountInfo` - Pool/collection data
- `getTransaction` - Trading history
- `getSignaturesForAddress` - Activity tracking

## OVSM Syntax Quick Reference

### Variables
```
$signature = "value"
$result = TOOL.method($variable)
```

### Tool Calls
```
TOOL.getTransaction($signature)
TOOL.getAccountInfo($address)
TOOL.getBlock($slot)
```

### Decisions
```
DECISION: Check if result exists
  BRANCH A (condition):
    actions
  BRANCH B (condition):
    actions
```

### Operations
```
$data = PARSE($result)
$metrics = ANALYZE($data)
OUTPUT: $metrics
ERROR: "message"
```

## Usage Examples

### Testing a Tool
1. Navigate to category: `cd 01_transaction_analysis/`
2. Open basic file: `cat 01_basic.md`
3. Find relevant question: `grep "Q5:" 01_basic.md -A 40`
4. Implement the OVSM plan
5. Test with real RPC

### Training an AI Model
1. Load all questions: `cat */01_basic.md > training_data.md`
2. Parse OVSM syntax
3. Train on question-plan pairs
4. Validate on expert questions

### Benchmarking
1. Select category by performance level
2. Run questions sequentially
3. Measure execution time
4. Compare against estimates

### Educational Use
1. Start with basic questions
2. Progress through difficulty levels
3. Practice OVSM syntax
4. Build understanding of Solana

## File Paths

### Absolute Paths
```
/home/larp/larpdevs/osvm-cli/test_qa_categories/
├── 01_transaction_analysis/01_basic.md
├── 02_account_state/01_basic.md
├── ...
└── 10_historical_analysis/10_expert.md
```

### Relative Paths (from test_qa_categories/)
```
./01_transaction_analysis/01_basic.md
./02_account_state/02_intermediate.md
./09_advanced_scenarios/10_expert.md
```

## Search Commands

### Find Questions by Keyword
```bash
# Search all files
grep -r "MEV" test_qa_categories/

# Search specific category
grep -r "liquidity pool" test_qa_categories/07_defi_analysis/

# Count questions with keyword
grep -rc "validator" test_qa_categories/ | grep -v ":0"
```

### Find by Question Number
```bash
# Find Q500
grep "^## Q500:" test_qa_categories/*/*.md

# Find range
grep "^## Q[5-9][0-9][0-9]:" test_qa_categories/01_*/*.md
```

### List All Questions in Category
```bash
# List questions
grep "^## Q" test_qa_categories/01_transaction_analysis/*.md

# Count questions
grep "^## Q" test_qa_categories/01_transaction_analysis/*.md | wc -l
```

## Validation

### Check Dataset Integrity
```bash
cd test_qa_categories/
./validation_check.sh
```

### Verify Question Format
```bash
# Check OVSM structure
grep -c "**Expected Plan:**" 01_transaction_analysis/01_basic.md
# Should output: 100

grep -c "**Decision Point:**" 01_transaction_analysis/01_basic.md
# Should output: 100
```

### Count Total Questions
```bash
grep -r "^## Q" {01..10}_*/ | wc -l
# Should output: 10000
```

## Tips

1. **Start Simple**: Begin with basic questions (01_basic.md files)
2. **Progress Gradually**: Move through difficulty levels sequentially
3. **Use Grep**: Search for specific topics or tools
4. **Validate Format**: Check OVSM syntax is correct
5. **Test Incrementally**: Test questions in small batches
6. **Track Progress**: Mark which questions you've implemented
7. **Combine Categories**: Mix questions for comprehensive testing
8. **Time Estimates**: Use time estimates for planning
9. **Cost Awareness**: Monitor SOL costs for advanced questions
10. **Error Handling**: Always implement both branches

## Support

- **Main Documentation**: README.md
- **Generation Details**: GENERATION_SUMMARY.md
- **This Guide**: QUICK_REFERENCE.md
- **Validation Script**: validation_check.sh

## Statistics

- **Total Questions**: 10,000
- **Total Files**: 100
- **Total Categories**: 10
- **Total Size**: ~11 MB
- **Difficulty Levels**: 10
- **Questions per File**: 100
- **Questions per Category**: 1,000
