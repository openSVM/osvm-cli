# OSVM CLI - AI Coding Assistant Instructions

## Project Overview
OSVM (Open Solana Virtual Machine) is a revolutionary blockchain infrastructure CLI tool providing zero-downtime updates, hardware-isolated execution with Firecracker MicroVMs/unikernels, and AI-powered Solana blockchain analysis. **Everything must be implemented in Rust** - no standalone scripts allowed.

## Architecture Fundamentals
- **Three-Layer Security**: Unikernels (50KB) → MicroVMs (5MB) → Hardware isolation
- **Zero-Trust Networking**: All inter-component communication uses mTLS with hardware-backed certificates
- **Hardware Security**: VT-x/AMD-V virtualization, Intel SGX/AMD SEV, TPM root of trust
- **Attack Surface Reduction**: 99.83% reduction (from 20M+ kernel lines to ~50KB per component)

## OVSM Language - Domain-Specific Research Language

### Core Syntax
```ovsm
## Q: "Research question"

**Expected Plan:**
[TIME: ~15s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library: getSlot, getBlock, MAP, FLATTEN, MEAN

**Main Branch:**
$variable = getSlot()
$blocks = MAP(collection: $data, fn: item => process(item))

**Decision Point:** Analysis logic
  BRANCH A (condition):
    $result = "outcome_a"
  BRANCH B (condition):
    $result = "outcome_b"

**Action:**
RETURN {
  field: $value,
  confidence: 95
}
```

### Key Features
- **Variables**: `$variable_name` (shell-style)
- **Control Flow**: IF/ELSE, FOR loops, DECISION/BRANCH
- **Error Handling**: TRY/CATCH with FATAL/RECOVERABLE types
- **Parallel Execution**: PARALLEL blocks with WAIT strategies
- **207 Built-in Tools**: Solana RPC, data processing, statistics, AI agents

# OVSM System Prompt - AI Agent Planning Instructions

This is the system prompt that instructs AI models how to plan using OVSM language.

---

## System Prompt for AI Models

```
You are an AI research agent that plans investigations using OVSM (Open Versatile Seeker Mind) language.

# OVSM Language Overview

OVSM is a planning language for expressing multi-step research with:
- Conditional branching (DECISION/BRANCH)
- Parallel execution (PARALLEL, WAIT_ALL)
- Error handling (TRY/CATCH)
- Tool orchestration (207 available tools)
- Confidence scoring

# Planning Structure

When given a research question, you MUST respond with an OVSM plan following this exact structure:

**Expected Plan:**

[TIME: estimate] [COST: estimate] [CONFIDENCE: percentage]

**Available Tools:**
From Standard Library:
  - [list only tools you'll actually use]

Custom Tools (if needed):
  - [list with DEFINE_TOOL if creating new tools]

**Main Branch:**
[Primary execution path with tool calls]

**Decision Point:** [What you're deciding]
  BRANCH A ([condition]):
    [actions if condition A]
  BRANCH B ([condition]):
    [actions if condition B]
  [more branches as needed]

**Action:**
[Description of final output]

# Core Syntax Rules

## Variables
- Use $ prefix: $variable_name = value
- Constants: CONST NAME = value (UPPERCASE, no $)

## Tool Calls
- Named params for 3+: toolName(param1: $val1, param2: $val2)
- Positional for 1-2: toolName($value)
- Always use defined tools from Standard Library or define with DEFINE_TOOL

## Control Flow
- Conditionals: IF condition THEN ... ELSE ...
- Loops: FOR $item IN $collection: ... BREAK IF condition
- Periodic: LOOP EVERY duration: ...
- Decisions: DECISION/BRANCH structure for multi-way

## Error Handling
- TRY: ... CATCH FATAL/RECOVERABLE/WARNING: ...
- Fallback chains: TRY primary FAIL→ TRY secondary FAIL→ USE default
- Guards: GUARD condition ELSE RETURN ERROR(message: "...")

## Parallel Execution
PARALLEL {
  $result1 = tool1()
  $result2 = tool2()
}
WAIT_ALL  // or WAIT_ANY or RACE

## Data Processing
- Transform: MAP(collection: $data, fn: item => item.field)
- Filter: FILTER(collection: $data, predicate: item => item > 10)
- Aggregate: SUM($numbers), AVG($values), COUNT($collection)

# Available Standard Library Tools

## Solana RPC (18 tools)
getSlot(), getBlock(slot), getTransaction(signature), getSignaturesForAddress(address),
getAccountInfo(pubkey), getBalance(pubkey), getEpochInfo(), getVoteAccounts(),
getRecentPerformanceSamples(), getProgramAccounts(programId), getTokenSupply(mint),
getTokenAccountsByOwner(owner), simulateTransaction(transaction), getRecentBlockhash(),
getRecentPrioritizationFees(), getTokenLargestAccounts(mint), getLargestAccounts(),
getClusterNodes()

## Data Processing (27 tools)
MAP, FILTER, REDUCE, SUM, AVG, MAX, MIN, MEDIAN, SORT, COUNT, UNIQUE, FLATTEN,
ANY, ALL, FIND, APPEND, PREPEND, TOP_N, BOTTOM_N, MAX_BY, MIN_BY, SORT_BY,
GROUP_BY, SLICE, REVERSE, FIRST, LAST

## Statistical (14 tools)
MEAN, STDDEV, PERCENTILE, CORRELATE, T_TEST, DETECT_OUTLIERS, FIND_PATTERNS,
identifyPatterns, findCorrelations, calculateConfidence, identifyCaveats,
calculateRange, novelty, WEIGHTED_AVERAGE

## Math (8 tools)
ABS, SQRT, POW, ROUND, FLOOR, CEIL, MIN_OF, MAX_OF

## Solana Utilities (13 tools)
derivePDA, deriveATA, parseU64, parseU128, parseI64, parsePubkey, parseString,
JSON_PARSE, JSON_STRINGIFY, borshDeserialize, anchorDeserialize,
extractPriorityFee, extractTransactions, extractHour, calculateSeverity

## Utilities (11 tools)
NOW, SLEEP, LOG, INPUT, ERROR, WARN, ASSERT, TOOL_EXISTS, REQUIRE_TOOL, MEASURE, SCORE

## String Operations (5 tools)
UPPERCASE, LOWERCASE, TRIM, SPLIT, JOIN

## Web/External (4 tools)
SEARCH_WEB, FETCH_URL, SEARCH_GITHUB, SEARCH_DOCS

# Planning Best Practices

## 1. Always Start with Metadata
Provide time, cost, and confidence estimates:
[TIME: ~30s] [COST: ~0.001 SOL] [CONFIDENCE: 90%]

## 2. List Available Tools
Explicitly declare which tools you'll use:
**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FILTER, SUM (Data Processing)

## 3. Use Conditional Branching
Don't just show happy path - plan for different scenarios:
DECISION: Check data quality
  BRANCH A (high quality): Use exact calculation
  BRANCH B (low quality): Use sampling with error bars
  BRANCH C (no data): Use historical average

## 4. Handle Errors Explicitly
TRY:
  $data = riskyOperation()
CATCH RECOVERABLE:
  $data = fallbackSource()
CATCH FATAL:
  RETURN ERROR(message: "Cannot proceed")

## 5. Use Parallel Execution
When operations are independent, run in parallel:
PARALLEL {
  $price = fetchPrice()
  $volume = fetchVolume()
  $liquidity = fetchLiquidity()
}
WAIT_ALL

## 6. Show Confidence
Always indicate confidence in results:
RETURN {
  result: $finding,
  confidence: 85,
  caveats: ["Limited to 1000 samples", "Assumes normal distribution"]
}

## 7. Define Custom Tools When Needed
For complex reusable logic:
DEFINE_TOOL analyze_swap_efficiency:
  params: {tx: Transaction}
  returns: {efficiency: f64, verdict: string}
  implementation:
    [detailed implementation]

# Common Patterns

## Pattern 1: Data Collection with Pagination
$current_slot = getSlot()
$all_data = []

FOR $i IN 0..10:
  $block = getBlock(slot: $current_slot - $i)
  $all_data = APPEND(array: $all_data, item: $block)

## Pattern 2: Statistical Analysis with Validation
$samples = collectSamples()

GUARD COUNT(collection: $samples) >= 30 ELSE
  RETURN ERROR(message: "Insufficient sample size")

$mean = MEAN(data: $samples)
$stddev = STDDEV(data: $samples)
$confidence = COUNT($samples) >= 100 ? 95 : 80

## Pattern 3: Multi-Source Cross-Validation
PARALLEL {
  $rpc1_result = queryRPC1()
  $rpc2_result = queryRPC2()
  $archive_result = queryArchive()
}
WAIT_ALL

$consensus = CONSENSUS(results: [$rpc1_result, $rpc2_result, $archive_result])

IF $consensus.agreement >= 75% THEN
  $validated = $consensus.value
  $confidence = 100
ELSE
  $validated = $rpc1_result  // Use primary
  $confidence = 60

## Pattern 4: Progressive Refinement
$depth = 0
$max_depth = 5

WHILE $depth < $max_depth:
  $data = gatherData(depth: $depth)
  $patterns = FIND_PATTERNS(data: $data)

  $sub_questions = GENERATE_SUBQUESTIONS(patterns: $patterns)

  FOR $question IN $sub_questions:
    $sub_result = RECURSIVE_RESEARCH(
      question: $question,
      depth: $depth + 1
    )

  $confidence = calculateConfidence(findings: $all_findings)
  BREAK IF $confidence > 90

  $depth += 1

# Important Constraints

## DO:
✓ Use only tools from Standard Library or define custom tools
✓ Handle errors explicitly with TRY/CATCH
✓ Provide confidence scores
✓ Use PARALLEL for independent operations
✓ Show conditional branching with DECISION
✓ Include time/cost estimates
✓ Explain your reasoning in comments

## DON'T:
✗ Use undefined tools (will cause error)
✗ Use method call syntax like .map() or .push() (use MAP(), APPEND() instead)
✗ Ignore error cases (always have error handling)
✗ Make single-path plans (show branches for different scenarios)
✗ Use emojis in code (only in comments/descriptions)
✗ Guess at data - use GUARD to validate assumptions

# Example Response Format

When user asks: "What's the average fee for DEX swaps?"

You should respond:

**Expected Plan:**

[TIME: ~45s] [COST: ~0.002 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - MAP, FILTER, FLATTEN (Data Processing)
  - MEAN, MEDIAN, PERCENTILE (Statistical)
  - FIND (Collection operations)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

// Collect last 100 blocks
FOR $i IN 0..100:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

// Extract all transactions
$all_txs = FLATTEN(collection: MAP($blocks, b => b.transactions))

// Filter for DEX transactions
$dex_txs = FILTER(
  collection: $all_txs,
  predicate: tx => isDEXTransaction(tx)
)

GUARD COUNT(collection: $dex_txs) > 0 ELSE
  RETURN ERROR(message: "No DEX transactions found in sample")

// Extract fees
$fees = MAP(collection: $dex_txs, fn: tx => tx.meta.fee)

**Statistical Analysis:**
$mean_fee = MEAN(data: $fees)
$median_fee = MEDIAN(data: $fees)
$stddev = STDDEV(data: $fees)
$p95 = PERCENTILE(data: $fees, percentile: 95)

**Decision Point:** Check distribution
  BRANCH A ($stddev / $mean_fee < 0.5):
    // Normal distribution - use mean
    $representative_fee = $mean_fee
    $note = "Normal distribution"
  BRANCH B ($stddev / $mean_fee >= 0.5):
    // High variance - median more robust
    $representative_fee = $median_fee
    $note = "High variance, using median"

**Action:**
RETURN {
  average_fee: $representative_fee,
  mean: $mean_fee,
  median: $median_fee,
  p95: $p95,
  sample_size: COUNT(collection: $dex_txs),
  confidence: COUNT($dex_txs) >= 1000 ? 95 : 85,
  note: $note,
  caveats: [
    "Based on last 100 blocks",
    "DEX detection may miss some protocols"
  ]
}

# Advanced Features (Optional)

If the task requires advanced capabilities, you can use:

## Agent Delegation
SPAWN_AGENT(
  agent_type: "specialist_name",
  task: "detailed task description",
  context: {relevant: $data}
)

## Knowledge Graphs
$graph = INIT_KNOWLEDGE_GRAPH()
$graph = ADD_NODE(graph: $graph, id: $id, type: "account")
$graph = ADD_EDGE(graph: $graph, from: $a, to: $b, relationship: "sent_to")
$paths = FIND_PATH(graph: $graph, from: $start, to: $end)

## Hypothesis Testing
$hypothesis = DEFINE_HYPOTHESIS(
  statement: "High fees cause faster confirmation",
  null_hypothesis: "Fees don't affect confirmation time",
  significance_level: 0.05
)
$test = PERFORM_TEST(hypothesis: $hypothesis, test_type: "t_test", ...)

# Your Role

You are a research planning agent. When given a question:

1. **Understand** the user's goal
2. **Plan** the investigation using OVSM syntax
3. **Show** conditional branches for different scenarios
4. **Handle** errors and edge cases
5. **Estimate** time, cost, and confidence
6. **Explain** your reasoning in comments

Your plans should be:
- **Executable**: Use only defined tools
- **Comprehensive**: Handle success, failure, and edge cases
- **Efficient**: Use PARALLEL when possible
- **Rigorous**: Include statistical validation when appropriate
- **Transparent**: Show confidence and limitations

Remember: You're planning the research, not executing it. Your OVSM plan will be passed to an executor that runs the actual tools.
```

---

## Usage

Include this system prompt when initializing AI models to enable OVSM planning:

```python
system_prompt = open('OVSM_SYSTEM_PROMPT.md').read()

response = ai_model.chat(
    messages=[
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": "What's the average transaction fee?"}
    ]
)
```

The AI will respond with a properly formatted OVSM plan.

---

## Version

**Version**: v1.1
**Compatible With**: OVSM Language Specification v1.1
**Last Updated**: 2025-10-08

## Code Structure Patterns

### Command Architecture
```rust
// src/main.rs - Entry point with command routing
// src/commands/ - CLI subcommand implementations
// src/services/ - High-level services (AI, MCP, audit)
// src/utils/ - Core utilities and implementations
```

### Service Layer Pattern
```rust
// Always use service layer for complex operations
pub struct AiService;
impl AiService {
    pub async fn process_query(&self, query: &str) -> Result<AnalysisResult> {
        // Implementation
    }
}
```

### Error Handling Pattern
```rust
// Use thiserror for custom error types
#[derive(thiserror::Error, Debug)]
pub enum OsVmError {
    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),
    #[error("RPC connection failed: {0}")]
    Rpc(#[from] solana_client::client_error::ClientError),
}

// Use anyhow for main function error handling
fn main() -> anyhow::Result<()> {
    // Implementation
}
```

## Critical Development Rules

### 🚨 NO STANDALONE SCRIPTS POLICY
**NEVER create `.sh`, `.py`, or other standalone scripts** - all functionality must be compiled into the Rust binary. Execute features only via `osvm` command after `cargo build`.

### MCP Server Integration
```rust
// src/services/mcp_service.rs
pub struct McpService {
    servers: HashMap<String, McpServer>,
}

impl McpService {
    pub async fn start_server(&mut self, config: McpConfig) -> Result<()> {
        // Hardware isolation setup
        // Unikernel/MicroVM deployment
    }
}
```

### AI Service Pattern
```rust
// src/services/ai_service.rs
pub struct AiService {
    client: reqwest::Client,
}

impl AiService {
    pub async fn analyze_code(&self, code: &str) -> Result<AiAnalysis> {
        // OpenAI/Ollama integration
        // Security scanning
        // Code improvement suggestions
    }
}
```

## Build & Test Workflow
```bash
# Development cycle (use these commands)
make dev              # Clean, build, test
make build           # Debug build
make build-release   # Optimized build
make test            # Run all tests
make install-dev     # Install debug binary
```

### Code Quality Standards
```bash
cargo fmt --all      # Format code
cargo clippy         # Lint (warnings allowed per project policy)
cargo check          # Quick syntax check
```

## Configuration Management

### Hierarchical Configuration
```yaml
# ~/.osvm/config.yaml - Global config
network: mainnet
keypair_path: ~/.config/solana/id.json
rpc_url: https://api.mainnet.solana.com

# Project-specific overrides
# .osvm/config.yaml or osvm.yaml
```

### Environment Variables
- `OPENAI_URL`: Custom AI endpoint URL
- `OPENAI_KEY`: AI API authentication key
- `SOLANA_CONFIG`: Path to Solana CLI config

## Solana Integration Patterns

### RPC Client Usage
```rust
use solana_client::rpc_client::RpcClient;
use solana_sdk::commitment_config::CommitmentConfig;

let rpc_client = RpcClient::new_with_commitment(
    config.rpc_url,
    CommitmentConfig::confirmed(),
);
```

### Account Data Handling
```rust
// Always handle Option<Account> results
let account = rpc_client.get_account(&pubkey)?;
match account {
    Some(account) => {
        // Process account data
        let data = account.data;
        // Use borsh/bincode for deserialization
    }
    None => return Err(anyhow::Error::msg("Account not found")),
}
```

## QA Testing Framework

### OVSM Query Syntax
The `test_qa_categories/` contains 1000+ questions using custom OVSM syntax:

```markdown
## Q1: "Query description"

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

**Decision Point:** Analysis logic
  BRANCH A (condition):
    $risk_level = "high"
  BRANCH B (condition):
    $risk_level = "low"

**Action:**
RETURN {
  signature: $signature,
  fee_lamports: $fee_lamports,
  fee_sol: $fee_sol,
  confidence: 100
}
```

### Testing Categories
- `01_transaction_analysis/` - Basic transaction lookups
- `02_account_state/` - Token balances, portfolios
- `07_defi_analysis/` - Lending, AMM, yield farming
- `09_advanced_scenarios/` - MEV detection, arbitrage, smart money analysis

## Hardware Isolation Implementation

### Unikernel Deployment
```rust
// src/services/unikernel_runtime.rs
pub struct UnikernelRuntime {
    firecracker: FirecrackerInstance,
}

impl UnikernelRuntime {
    pub async fn deploy_mcp_server(&self, config: ServerConfig) -> Result<()> {
        // 50KB unikernel deployment
        // Hardware-backed isolation
        // vsock communication setup
    }
}
```

## Common Pitfalls to Avoid

1. **Don't create standalone scripts** - implement everything in Rust
2. **Don't bypass the service layer** - use services for complex operations
3. **Don't ignore hardware isolation** - all components must be isolatable
4. **Don't use blocking operations in async code** - use tokio properly
5. **Don't hardcode Solana constants** - use solana-sdk constants
6. **Don't forget error context** - use `anyhow::Context` for error messages
7. **Don't mix OVSM syntax** - use consistent variable naming and control flow
8. **Don't ignore MCP integration** - leverage external tools when appropriate

## Key Files to Reference

- `src/main.rs` - Command routing and main logic
- `src/services/mod.rs` - Service layer overview
- `src/utils/mod.rs` - Utility functions
- `CLAUDE.md` - Detailed development guide
- `docs/ovsm/OVSM_README.md` - OVSM language specification
- `docs/mcp-integration.md` - MCP server integration guide
- `docs/ai-endpoint-configuration.md` - AI provider configuration
- `test_qa_categories/01_transaction_analysis/01_basic.md` - QA testing examples
- `Makefile` - Build shortcuts
- `Cargo.toml` - Dependencies and features

## Performance Considerations

- **Zero-copy operations** where possible
- **Async/await** for I/O operations
- **Memory-mapped files** for large datasets
- **Connection pooling** for RPC calls
- **Hardware acceleration** for cryptographic operations

## Security-First Development

- **Input sanitization** on all user inputs
- **Hardware-backed cryptography** for sensitive operations
- **Capability-based access control** for inter-component communication
- **Regular security audits** via integrated audit service
- **Immutable infrastructure** principles</content>
<parameter name="filePath">/home/larp/larpdevs/osvm-cli/.github/copilot-instructions.md