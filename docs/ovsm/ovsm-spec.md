# OVSM Language Specification v1.1
## Open Versatile Seeker Mind

**Purpose**: A human-readable language for expressing AI agent research plans with explicit control flow, data handling, and decision-making logic.

**What is OVSM?**

OVSM (Open Versatile Seeker Mind) is a domain-specific language designed for planning and expressing multi-agent research workflows. It enables AI agents to articulate complex investigation strategies with conditional branching, parallel execution, error handling, and adaptive learning.

**Pronunciation**: "OH-vism" (rhymes with "chasm")

**File Extension**: `.ovsm`

**Design Goals**:
- Clear and unambiguous
- Easy for humans to read and write
- Expresses complex branching logic
- Shows data flow explicitly
- Programming-familiar syntax
- No emojis or visual gimmicks
- Optimized for AI agent planning and research

---

## Table of Contents

1. [Basic Syntax](#basic-syntax)
2. [Variables and Data Types](#variables-and-data-types)
3. [Operators](#operators)
4. [Control Flow](#control-flow)
5. [Tool Invocation](#tool-invocation)
6. [Standard Tools Library](#standard-tools-library)
7. [Parallel Execution](#parallel-execution)
8. [Error Handling](#error-handling)
9. [Data Transformation](#data-transformation)
10. [Decision Logic](#decision-logic)
11. [Comments and Metadata](#comments-and-metadata)
12. [Complete Examples](#complete-examples)

---

## Basic Syntax

### General Structure

```
**Expected Plan:**

[METADATA]

**Available Tools:**
  [tool declarations]

**Main Branch:**
  [statements]

**Decision Point:** [condition]
  [branches]

**Action:** [final output description]
```

### Tool Declaration Requirement

**IMPORTANT**: All tools must be either:
1. Declared in the plan's "Available Tools" section, OR
2. Part of the Standard Tools Library, OR
3. Defined inline with `DEFINE_TOOL`

Calling undefined tools will result in an error.

### Keywords (Case-Insensitive)

Core keywords are in UPPERCASE for clarity:
- `PARALLEL`, `WAIT_ALL`, `WAIT_ANY`, `RACE`
- `IF`, `ELSE`, `THEN`, `WHILE`, `FOR`, `BREAK`, `CONTINUE`, `LOOP`, `EVERY`
- `TRY`, `CATCH`, `FAIL`, `TIMEOUT`
- `RETURN`, `GOTO`, `LABEL`
- `AND`, `OR`, `NOT`, `IN`, `BETWEEN`
- `DECISION`, `BRANCH`, `GUARD`, `MATCH`
- `TOOL`, `DEFINE`, `DEFINE_TOOL`, `CONST`
- `AWAIT`, `ASYNC`

---

## Variables and Data Types

### Variable Declaration

Variables use `$` prefix (shell/PHP style):

```
$variable_name = value
$count = 100
$address = "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"
```

### Constants

Constants use UPPERCASE without `$` prefix and are immutable:

```
// Solana System Programs
CONST SYSTEM_PROGRAM = "11111111111111111111111111111111"
CONST TOKEN_PROGRAM = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"
CONST TOKEN_2022_PROGRAM = "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb"
CONST ASSOCIATED_TOKEN_PROGRAM = "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL"

// DeFi Programs
CONST RAYDIUM_AMM_PROGRAM = "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"
CONST ORCA_WHIRLPOOL_PROGRAM = "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc"
CONST JUPITER_AGGREGATOR_V6 = "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4"

// Lending Programs
CONST SOLEND_PROGRAM = "So1endDq2YkqhipRh3WViPa8hdiSpxWy6z3Z6tMCpAo"
CONST MANGO_V4_PROGRAM = "4MangoMjqJ2firMokCjjGgoK8d4MXcrgL7XJaL3w6fVg"

// System Limits
CONST MAX_COMPUTE_UNITS = 1400000
CONST MAX_TRANSACTION_SIZE = 1232
CONST MAX_ACCOUNTS = 256
CONST MAX_CPI_DEPTH = 4
CONST LAMPORTS_PER_SOL = 1000000000

// Network
CONST SLOTS_PER_EPOCH = 432000
CONST SLOT_DURATION_MS = 400

// Aliases for clarity
CONST DEX_PROGRAM_ID = RAYDIUM_AMM_PROGRAM  // Example: can be any DEX
CONST ORCA_PROGRAM_ID = ORCA_WHIRLPOOL_PROGRAM
CONST LENDING_PROGRAM = SOLEND_PROGRAM      // Example: can be any lending protocol

// Usage in conditionals
IF $program_id == DEX_PROGRAM_ID THEN
  // Handle DEX transaction

IF $compute_units > MAX_COMPUTE_UNITS THEN
  ERROR(message: "Exceeds compute limit")

IF $accounts_count > MAX_ACCOUNTS THEN
  ERROR(message: "Too many accounts")
```

### Data Types

**Primitives:**
```
$number = 42
$float = 3.14
$string = "hello"
$boolean = true
$null = null
```

**Collections:**
```
$array = [1, 2, 3, 4, 5]
$object = {key: "value", count: 10}
$tuple = (value1, value2, value3)
```

### Type Annotations (Optional)

```
$result: Transaction = getTransaction($sig)
$balance: u64 = getBalance($address)
$prices: Array<f64> = fetchPrices()
```

### Destructuring

```
{$slot, $epoch} = getEpochInfo()
[$first, $second, ...$rest] = $array
{$meta: {$fee, $err}} = $transaction
```

---

## Operators

### Arithmetic
```
+ - * / % **  (power)
$total = $price * $quantity
$average = $sum / $count
```

**Division by Zero:**
Division by zero results in runtime error. Use guard clauses to prevent:
```
GUARD $count > 0 ELSE RETURN ERROR(message: "Division by zero")
$average = $sum / $count
```

### Comparison
```
== != > < >= <=
$is_high = $value > 1000
$matches = $type == "transfer"
```

### Logical
```
AND OR NOT XOR
$valid = $exists AND $not_expired
$should_retry = NOT $success OR $timeout
```

### Data Flow Arrow

**Important**: Arrows are **documentation/annotation only**, not executable operators.

```
→  (single arrow for data flow annotation)
⇒  (double arrow for significant transformation annotation)
```

**Usage Pattern 1: Pipeline Annotation (for clarity)**
```
// Shows data flow for reader understanding
$slot = getSlot()        // → current slot number
$block = getBlock($slot) // → block data
$txs = $block.transactions // → transaction array
```

**Usage Pattern 2: Inline Comments**
```
getSlot() → getBlock() → extractTransactions()
// Read as: "getSlot FLOWS TO getBlock FLOWS TO extractTransactions"
// This is a COMMENT showing the conceptual flow
```

**What arrows are NOT:**
- NOT a pipe operator (use function composition instead)
- NOT an assignment operator (use `=`)
- NOT an executable statement

**Correct Usage:**
```
// Good: Arrow as annotation
$data = processData()  // → cleaned data

// Good: Showing conceptual flow in comment
// Flow: getData() → transform() → save()

// Bad: Trying to use arrow as operator
$result = getData() → transform()  // WRONG - not valid syntax
```

**For Actual Piping:**
Use function composition or chaining:
```
$result = transform(data: getData())
// Or define a pipe tool if needed
```

### Optional/Null Handling
```
?.  (optional chaining)
??  (null coalescing)

$fee = $transaction?.meta?.fee ?? 0
$name = $account?.data?.name ?? "Unknown"
```

### Ternary Conditional
```
condition ? value_if_true : value_if_false

$strategy = $count > 10000 ? "sample" : "exact"
$label = $score >= 90 ? "good" : "needs_improvement"
```

### Range and Membership
```
IN BETWEEN
$valid = $value IN [1, 2, 3, 4, 5]
$in_range = $score BETWEEN 0 AND 100
```

---

## Control Flow

### Conditional Statements

**IF-ELSE:**
```
IF condition THEN
  [statements]
ELSE IF other_condition THEN
  [statements]
ELSE
  [statements]
```

**Example:**
```
IF $count > 1000 THEN
  $strategy = "sample"
ELSE IF $count > 100 THEN
  $strategy = "paginate"
ELSE
  $strategy = "fetch_all"
```

### Loops

**WHILE:**
```
WHILE condition:
  [statements]
  BREAK IF exit_condition
  CONTINUE IF skip_condition
```

**FOR:**
```
FOR $item IN $collection:
  [process $item]
  BREAK IF satisfied
  CONTINUE IF should_skip
```

**FOR with Range:**
```
FOR $i IN 0..100:
  [process iteration]
```

**FOR with Index:**
```
FOR $item, $index IN $collection:
  [use $item and $index]
```

### Loop Control

```
BREAK         // Exit loop immediately
CONTINUE      // Skip to next iteration
BREAK IF condition
CONTINUE IF condition
```

### Periodic Loop

```
LOOP EVERY duration:
  [statements]
  BREAK IF condition
  CONTINUE IF condition
```

**Examples:**
```
// Check every 30 seconds
LOOP EVERY 30s:
  $value = checkMetric()
  LOG(message: "Current value: {$value}")
  BREAK IF $value < threshold

// Check every 5 minutes
LOOP EVERY 5min:
  $data = collectData()
  processData($data)
  BREAK IF dataComplete()

// With timeout
TIMEOUT 10min:
  LOOP EVERY 1s:
    $status = checkStatus()
    BREAK IF $status == "ready"
```

### Return Early

```
RETURN $result IF condition_met
RETURN early IF $data == null
```

---

## Tool Invocation

### Tool Definition

Tools must be defined before use, specifying their interface:

```
TOOL getSlot:
  description: "Get current slot number"
  returns: u64
  cost: 0

TOOL getTransaction:
  description: "Fetch transaction details by signature"
  params:
    signature: string (required)
    maxSupportedTransactionVersion: u8 (optional, default: 0)
    commitment: string (optional, default: "finalized")
  returns: Transaction
  cost: 0.00001 SOL
  can_fail: true

TOOL getBlock:
  description: "Fetch block by slot number"
  params:
    slot: u64 (required)
    commitment: string (optional)
    maxSupportedTransactionVersion: u8 (optional)
  returns: Block
  cost: 0.00005 SOL
  can_fail: true
  rate_limit: 10/second
```

### Available Tools Registry

Tools are organized by category:

```
AVAILABLE_TOOLS:
  solana_rpc: [
    getSlot,
    getBlock,
    getTransaction,
    getSignaturesForAddress,
    getAccountInfo,
    getBalance,
    getEpochInfo,
    getRecentBlockhash,
    getVoteAccounts,
    // ... all RPC methods
  ]

  data_processing: [
    MAP,
    FILTER,
    REDUCE,
    SUM,
    AVG,
    MAX,
    MIN,
    SORT,
    // ... data functions
  ]

  analysis: [
    CORRELATE,
    T_TEST,
    DETECT_OUTLIERS,
    FIND_PATTERNS,
    CALCULATE_CONFIDENCE,
    // ... statistical functions
  ]
```

### Basic Invocation

Tool calls must use defined tools:

```
// Direct invocation
$slot = CALL getSlot()
$tx = CALL getTransaction(signature: $sig)

// Shorthand (when unambiguous)
$slot = getSlot()
$tx = getTransaction($signature)
```

### Checking Tool Availability

```
IF TOOL_EXISTS("getSlot") THEN
  $slot = getSlot()
ELSE
  ERROR("Tool getSlot not available")

// Or use guard
REQUIRE_TOOL getSlot
$slot = getSlot()
```

### Chained Invocation

```
$result = getSlot() → getBlock($result) → extractTransactions($result)
$data = fetchData() → parseData() → validateData()
```

### Named Parameters

```
$block = CALL getBlock(
  slot: $current_slot,
  commitment: "finalized",
  maxSupportedTransactionVersion: 0
)
```

### Tool Call with Error Handling

```
TRY:
  $transactions = CALL getSignaturesForAddress(
    address: $pubkey,
    limit: 1000,
    before: $cursor
  )
CATCH:
  ERROR(message: "Failed to fetch signatures")
```

### Parameter Syntax Rules

**Positional Parameters** (use when obvious and 1-2 params):
```
$count = COUNT($array)
$first = FIRST($array)
$sum = SUM($numbers)
```

**Named Parameters** (use when 3+ params OR clarity needed):
```
$block = getBlock(
  slot: $current_slot,
  commitment: "finalized",
  maxSupportedTransactionVersion: 0
)

$fee_analysis = analyze_fees(block: $block)
```

**Mixed Parameters** (positional first, named after):
```
$filtered = FILTER($array, item => item > 10)
// OR with named for clarity
$filtered = FILTER(
  collection: $array,
  predicate: item => item > 10
)
```

**Consistency Rule**: Within a single plan, be consistent. Pick one style and use it throughout.

### Custom Tool Definition

Define your own tools for reusability:

```
DEFINE_TOOL analyze_fees:
  description: "Analyze transaction fees from a block"
  params:
    block: Block (required)
  returns: {mean: f64, median: f64, p95: f64}
  implementation:
    $transactions = $block.transactions
    $fees = MAP(collection: $transactions, fn: tx => tx.meta.fee)
    RETURN {
      mean: MEAN(data: $fees),
      median: MEDIAN(data: $fees),
      p95: PERCENTILE(data: $fees, percentile: 95)
    }

// Use custom tool
$block = getBlock(slot: $slot)
$fee_analysis = analyze_fees(block: $block)
```

### Helper Functions Within DEFINE_TOOL

For internal helper functions that aren't reusable tools:

```
DEFINE_TOOL queryOrcaPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    // Define local helper functions with DEFINE
    DEFINE derivePoolAddress(pair):
      $seeds = ["whirlpool", pair[0], pair[1]]
      RETURN derivePDA(seeds: $seeds, program: ORCA_PROGRAM_ID)

    DEFINE parsePoolPrice(data):
      $sqrt_price = parseU64(data: $data, offset: 0)
      RETURN ($sqrt_price / 2^64) ** 2

    // Use helpers
    $pool_address = derivePoolAddress($token_pair)
    $pool_account = getAccountInfo(pubkey: $pool_address)
    $price = parsePoolPrice($pool_account.data)
    RETURN $price
```

**Important**: Helper functions defined with `DEFINE` (not `DEFINE_TOOL`) are:
- Scoped to the parent tool only
- Not available globally
- Simpler syntax (no params/returns declaration needed for simple cases)

### Tool Composition

```
DEFINE_TOOL get_and_analyze_block:
  params:
    slot: u64
  returns: BlockAnalysis
  implementation:
    $block = CALL getBlock(slot: $slot)
    $tx_count = COUNT($block.transactions)
    $fee_stats = analyze_fees($block)
    RETURN {
      slot: $slot,
      transaction_count: $tx_count,
      fee_analysis: $fee_stats
    }
```

---

## Parallel Execution

### PARALLEL Block

Execute multiple operations concurrently:

```
PARALLEL {
  $account1 = getAccountInfo($addr1)
  $account2 = getAccountInfo($addr2)
  $tx = getTransaction($sig)
}
```

### Wait Strategies

**WAIT_ALL:**
Wait for all parallel operations to complete.

```
PARALLEL {
  $price1 = fetchPrice("DEX1")
  $price2 = fetchPrice("DEX2")
  $price3 = fetchPrice("DEX3")
}
WAIT_ALL
$best_price = MAX([$price1, $price2, $price3])
```

**WAIT_ANY:**
Continue when any operation completes.

```
PARALLEL {
  $result1 = queryNode1()
  $result2 = queryNode2()
}
WAIT_ANY → $first_result
// Use whichever completed first
```

**RACE:**
Take first result, cancel others.

```
RACE {
  $data = slowButAccurate()
  $estimate = fastButApproximate()
}
TAKE_FIRST → $result
```

### Timeout Guards

```
TIMEOUT 30s:
  $result = longRunningOperation()
CATCH TIMEOUT:
  $result = fallbackValue()
```

---

## Error Handling

### TRY-CATCH

```
TRY:
  $result = riskyOperation()
CATCH:
  [error handling]
```

### Error Classification

```
TRY:
  $data = fetchData()
CATCH:
  FATAL → abort with error message
  RECOVERABLE → retry with backoff
  WARNING → log and continue with defaults
```

### Error Types

```
FATAL       // Unrecoverable, abort execution
RECOVERABLE // Retry possible
WARNING     // Log but continue
```

### Fallback Chains

```
TRY primary_source()
  FAIL→ TRY secondary_source()
    FAIL→ TRY tertiary_source()
      FAIL→ USE default_value()
```

**Multi-line Fallback:**
```
TRY:
  $price = fetchFromOracle("primary")
CATCH RECOVERABLE:
  TRY:
    $price = fetchFromOracle("secondary")
  CATCH RECOVERABLE:
    $price = calculateFromDEX()
```

### Retry Logic

```
RETRY 3x ON_FAIL:
  $result = unreliableOperation()
WITH_BACKOFF: [1s, 2s, 4s]
```

### Circuit Breaker

```
CIRCUIT_BREAK AFTER 10 failures FOR 60s:
  $result = externalService()
```

---

## Data Transformation

### Map, Filter, Reduce

**MAP:**
```
$fees = MAP($transactions, tx => tx.meta.fee)
$squares = MAP($numbers, n => n * n)
```

**FILTER:**
```
$successful = FILTER($transactions, tx => tx.meta.err == null)
$large = FILTER($amounts, amt => amt > 1000)
```

**REDUCE:**
```
$sum = REDUCE($numbers, (acc, n) => acc + n, 0)
$total_fees = REDUCE($transactions, (sum, tx) => sum + tx.fee, 0)
```

### Aggregation Functions

```
SUM($collection)
AVG($collection)
MIN($collection)
MAX($collection)
COUNT($collection)
MEDIAN($collection)
```

**Examples:**
```
$total_fees = SUM(MAP($transactions, tx => tx.fee))
$avg_compute = AVG(MAP($transactions, tx => tx.computeUnits))
$highest_fee = MAX($fees)
```

### Statistical Functions

```
MEAN($data)
MEDIAN($data)
MODE($data)
STDDEV($data)
PERCENTILE($data, 95)
```

### Array Operations

```
$first = FIRST($array)
$last = LAST($array)
$slice = SLICE($array, start: 0, end: 10)
$reversed = REVERSE($array)
$sorted = SORT($array)
$unique = UNIQUE($array)
```

### String Operations

```
$upper = UPPERCASE($string)
$lower = LOWERCASE($string)
$trimmed = TRIM($string)
$parts = SPLIT($string, delimiter: ",")
$joined = JOIN($array, separator: ", ")
```

---

## Decision Logic

### Decision Points

Explicit decision-making with multiple branches:

```
DECISION: [description of what we're deciding]
  BRANCH A ([condition]):
    [statements]
  BRANCH B ([condition]):
    [statements]
  BRANCH C ([condition]):
    [statements]
```

**Example:**
```
DECISION: Determine data collection strategy
  BRANCH A (count < 100):
    // Fetch all data exactly
    $data = fetchAll()
  BRANCH B (100 <= count < 10000):
    // Paginate through results
    $data = paginate($count)
  BRANCH C (count >= 10000):
    // Statistical sampling
    $data = sample($count, sample_size: 1000)
```

### Nested Decisions

```
DECISION: Check transaction status
  BRANCH A (success):
    $result = extractSuccessData($tx)

    DECISION: Check if CPI involved
      BRANCH A1 (has_cpi):
        $calls = parseInnerInstructions($tx)
      BRANCH A2 (no_cpi):
        $calls = [$tx.instruction]

  BRANCH B (failed):
    $error = parseError($tx)
```

### AI Decision Points

When AI needs to infer user intent:

```
AI_DECISION: Determine time range
  ANALYZE: Query context
    - Contains "recent" → last 24 hours
    - Contains "historical" → all time
    - Contains specific date → that date range
    - No time qualifier → default to last 7 days
  SELECT: Appropriate branch
```

### Pattern Matching

```
MATCH $type:
  "transfer" → handleTransfer()
  "mint" → handleMint()
  "burn" → handleBurn()
  _ → handleUnknown()  // default case
```

### Guard Clauses

```
GUARD $data != null ELSE RETURN ERROR(message: "No data")
GUARD $balance >= $amount ELSE RETURN ERROR(message: "Insufficient balance")
```

---

## Comments and Metadata

### Inline Comments

```
// Single line comment
$result = calculate()  // End of line comment

/* Multi-line comment
   spanning multiple lines
   for detailed explanations */
```

### Metadata Tags

**Time Estimates:**
```
[TIME: ~30s]
[TIME: 1-5min]
[TIME: variable, depends on data size]
```

**Cost Estimates:**
```
[COST: ~0.001 SOL]
[COST: free, read-only]
[COST: ~$0.50 in RPC fees]
```

**Data Volume:**
```
[DATA: ~100 transactions]
[DATA: up to 10MB]
[DATA: streaming, unbounded]
```

**Complexity:**
```
[COMPLEXITY: O(n)]
[COMPLEXITY: O(n log n)]
[COMPLEXITY: O(1)]
```

**Confidence:**
```
[CONFIDENCE: 100%] // Exact data
[CONFIDENCE: 95%]  // High confidence estimate
[CONFIDENCE: 60%]  // Rough estimate
```

**Requirements:**
```
[REQUIRES: archive node]
[REQUIRES: write permissions]
[REQUIRES: API key]
```

**Dependencies:**
```
[DEPENDS_ON: tool1, tool2]
[DEPENDS_ON: external_service]
```

### Documentation Comments

```
/**
 * Purpose: Fetch and analyze transaction
 * Inputs: signature (string)
 * Outputs: analysis object
 * Side effects: none
 * Complexity: O(1)
 */
$analysis = analyzeTransaction($signature)
```

---

## Tool Naming Conventions

To maintain consistency and clarity, the AI Planning Meta-Language uses the following tool naming conventions:

**1. Solana RPC Tools (lowercase camelCase)**
- Follow the official Solana SDK naming: `getSlot`, `getBlock`, `getTransaction`, `getAccountInfo`
- Rationale: Matches the Solana RPC API exactly, making it easy to reference documentation

**2. Data Processing Tools (UPPERCASE)**
- Core operations: `MAP`, `FILTER`, `REDUCE`, `SUM`, `AVG`, `COUNT`, `SORT`
- Rationale: Stands out visually as fundamental data operations, similar to SQL keywords

**3. Custom Tools (snake_case)**
- User-defined tools: `analyze_fees`, `query_oracle_pool`, `extract_priority_fee`
- Rationale: Distinct from both RPC and built-in operations, follows common programming conventions

**4. Statistical Tools (UPPERCASE)**
- Statistical operations: `MEAN`, `MEDIAN`, `STDDEV`, `PERCENTILE`, `CORRELATE`
- Rationale: Mathematical operations treated similarly to data processing tools

**5. Utility Tools (UPPERCASE)**
- System utilities: `NOW`, `LOG`, `ERROR`, `SLEEP`, `ASSERT`
- Rationale: System-level operations stand out as keywords

**Examples:**
```
// RPC tools (lowercase) - matches Solana SDK
$slot = getSlot()
$block = getBlock(slot: $slot)
$account = getAccountInfo(pubkey: $address)

// Data processing (UPPERCASE) - stands out
$fees = MAP($transactions, tx => tx.meta.fee)
$high_fees = FILTER($fees, fee => fee > 1000)
$total = SUM($fees)

// Custom tools (snake_case) - user-defined
$analysis = analyze_fees(block: $block)
$pool_price = query_oracle_pool(token_pair: $pair)

// Statistical tools (UPPERCASE) - mathematical operations
$average = MEAN($data)
$correlation = CORRELATE(x: $x_data, y: $y_data)

// Utility tools (UPPERCASE) - system operations
$timestamp = NOW()
LOG(message: "Processing complete")
```

---

## Standard Tools Library

### Solana RPC Tools

```
TOOL getSlot:
  returns: u64

TOOL getBlock:
  params: {slot: u64, commitment?: string, maxSupportedTransactionVersion?: u8}
  returns: Block

TOOL getTransaction:
  params: {signature: string, maxSupportedTransactionVersion?: u8}
  returns: Transaction

TOOL getSignaturesForAddress:
  params: {address: string, limit?: u64, before?: string, until?: string}
  returns: Array<TransactionSignature>

TOOL getAccountInfo:
  params: {pubkey: string, commitment?: string}
  returns: AccountInfo

TOOL getBalance:
  params: {pubkey: string}
  returns: u64

TOOL getEpochInfo:
  returns: EpochInfo

TOOL getRecentBlockhash:
  returns: string

TOOL getRecentPrioritizationFees:
  returns: Array<PrioritizationFee>

TOOL getVoteAccounts:
  returns: VoteAccounts

TOOL getTokenAccountsByOwner:
  params: {owner: string, mint?: string, programId?: string}
  returns: Array<TokenAccount>

TOOL simulateTransaction:
  params: {transaction: Transaction}
  returns: SimulationResult
```

### Data Processing Tools

```
TOOL MAP:
  params: {collection: Array<T>, fn: (T) => U}
  returns: Array<U>

TOOL FILTER:
  params: {collection: Array<T>, predicate: (T) => bool}
  returns: Array<T>

TOOL REDUCE:
  params: {collection: Array<T>, fn: (acc: U, item: T) => U, initial: U}
  returns: U

TOOL SUM:
  params: {collection: Array<number>}
  returns: number

TOOL AVG:
  params: {collection: Array<number>}
  returns: number

TOOL MAX:
  params: {collection: Array<number>}
  returns: number

TOOL MIN:
  params: {collection: Array<number>}
  returns: number

TOOL MEDIAN:
  params: {collection: Array<number>}
  returns: number

TOOL SORT:
  params: {collection: Array<T>, compareFn?: (a: T, b: T) => number}
  returns: Array<T>

TOOL UNIQUE:
  params: {collection: Array<T>}
  returns: Array<T>

TOOL FLATTEN:
  params: {collection: Array<Array<T>>}
  returns: Array<T>

TOOL ANY:
  params: {collection: Array<T>, predicate: (T) => bool}
  returns: bool

TOOL ALL:
  params: {collection: Array<T>, predicate: (T) => bool}
  returns: bool

TOOL FIND:
  params: {collection: Array<T>, predicate: (T) => bool}
  returns: T | null

TOOL APPEND:
  params: {array: Array<T>, item: T}
  returns: Array<T>

TOOL PREPEND:
  params: {array: Array<T>, item: T}
  returns: Array<T>

TOOL TOP_N:
  params: {collection: Array<T>, n: u32, by?: string | function}
  returns: Array<T>

TOOL BOTTOM_N:
  params: {collection: Array<T>, n: u32, by?: string | function}
  returns: Array<T>

TOOL MAX_BY:
  description: "Find item with maximum value by key/function"
  params:
    collection: Array<T> (required)
    by: string | function (required)
  returns: T | null

TOOL MIN_BY:
  description: "Find item with minimum value by key/function"
  params:
    collection: Array<T> (required)
    by: string | function (required)
  returns: T | null

TOOL SORT_BY:
  description: "Sort collection by key/function"
  params:
    collection: Array<T> (required)
    by: string | function (required)
    order: string (optional, default: "asc") // "asc" or "desc"
  returns: Array<T>

TOOL GROUP_BY:
  params: {collection: Array<T>, key: string | function}
  returns: Map<any, Array<T>>

TOOL SLICE:
  params: {array: Array<T>, start: u32, end?: u32}
  returns: Array<T>

TOOL REVERSE:
  params: {array: Array<T>}
  returns: Array<T>

TOOL FIRST:
  params: {array: Array<T>}
  returns: T | null

TOOL LAST:
  params: {array: Array<T>}
  returns: T | null
```

### Statistical Analysis Tools

```
TOOL MEAN:
  params: {data: Array<number>}
  returns: f64

TOOL STDDEV:
  params: {data: Array<number>}
  returns: f64

TOOL PERCENTILE:
  params: {data: Array<number>, percentile: u8}
  returns: f64

TOOL CORRELATE:
  params: {x: Array<number>, y: Array<number>}
  returns: f64

TOOL T_TEST:
  params: {sample1: Array<number>, sample2: Array<number>}
  returns: {t_statistic: f64, p_value: f64}

TOOL DETECT_OUTLIERS:
  params: {data: Array<number>, method?: string}
  returns: Array<number>

TOOL FIND_PATTERNS:
  params: {data: Array<T>, pattern_type?: string}
  returns: Array<Pattern>

TOOL identifyPatterns:
  description: "Identify patterns in data (alias for FIND_PATTERNS)"
  params: {data: Array<any>}
  returns: Array<Pattern>

TOOL findCorrelations:
  description: "Find correlations between datasets"
  params: {datasets: Array<Array<number>>}
  returns: Array<Correlation>

TOOL calculateConfidence:
  description: "Calculate confidence from evidence or factors"
  params: {evidence: Array<any>}
  returns: f64

TOOL identifyCaveats:
  description: "Identify limitations and caveats"
  params: {finding: any, methodology: string}
  returns: Array<string>

TOOL calculateRange:
  description: "Calculate value range from uncertainty"
  params: {value: number, uncertainty: f64}
  returns: {lower: number, upper: number}

TOOL novelty:
  description: "Calculate novelty score of a pattern"
  params: {pattern: Pattern, existing: Array<Pattern>}
  returns: f64
```

### Web Search & External Tools

```
TOOL SEARCH_WEB:
  params: {query: string, max_results?: u32}
  returns: Array<SearchResult>

TOOL FETCH_URL:
  params: {url: string}
  returns: string

TOOL SEARCH_GITHUB:
  params: {query: string, language?: string}
  returns: Array<Repository>

TOOL SEARCH_DOCS:
  params: {query: string, source?: string}
  returns: Array<Document>
```

### Math Tools

```
TOOL ABS:
  params: {value: number}
  returns: number

TOOL SQRT:
  params: {value: number}
  returns: number

TOOL POW:
  params: {base: number, exponent: number}
  returns: number

TOOL ROUND:
  params: {value: number, decimals?: u32}
  returns: number

TOOL FLOOR:
  params: {value: number}
  returns: number

TOOL CEIL:
  params: {value: number}
  returns: number

TOOL MIN_OF:
  params: {a: number, b: number}
  returns: number

TOOL MAX_OF:
  params: {a: number, b: number}
  returns: number
```

### Utility Tools

```
TOOL NOW:
  returns: Timestamp

TOOL SLEEP:
  params: {duration: Duration}
  returns: void

TOOL LOG:
  params: {message: string, level?: string}
  returns: void

TOOL ASSERT:
  params: {condition: bool, message: string}
  returns: void

TOOL INPUT:
  params: {prompt: string, type?: string}
  returns: string

TOOL ERROR:
  params: {message: string}
  returns: never

TOOL WARN:
  params: {message: string}
  returns: void

TOOL TOOL_EXISTS:
  description: "Check if a tool is available"
  params: {tool_name: string}
  returns: bool

TOOL REQUIRE_TOOL:
  description: "Assert that a tool exists, error if not"
  params: {tool_name: string}
  returns: void
```

### String Manipulation Tools

```
TOOL UPPERCASE:
  params: {text: string}
  returns: string

TOOL LOWERCASE:
  params: {text: string}
  returns: string

TOOL TRIM:
  params: {text: string}
  returns: string

TOOL SPLIT:
  params: {text: string, delimiter: string}
  returns: Array<string>

TOOL JOIN:
  params: {array: Array<string>, separator: string}
  returns: string
```

### Advanced Analysis Tools

```
TOOL WEIGHTED_AVERAGE:
  params: {values: Array<number>, weights: Array<number>}
  returns: f64

TOOL MODE:
  params: {data: Array<any>}
  returns: any

TOOL EXTRACT:
  description: "Extract field from objects"
  params: {collection: Array<object>, field: string}
  returns: Array<any>

TOOL MEASURE:
  description: "Measure a metric value"
  params: {metric: string, context?: object}
  returns: f64

TOOL SCORE:
  description: "Calculate composite score"
  params: {metrics: object, weights?: object}
  returns: f64

TOOL QUERY:
  description: "Generic query executor"
  params: {source: string, query: object}
  returns: any

TOOL THRESHOLD_EXCEEDED:
  params: {value: f64, threshold: f64, condition: string}
  returns: bool
```

### Additional Solana Tools

```
TOOL getRecentPerformanceSamples:
  returns: Array<PerformanceSample>

TOOL getProgramAccounts:
  params: {programId: string, filters?: Array<Filter>}
  returns: Array<Account>

TOOL getTokenSupply:
  params: {mint: string}
  returns: TokenSupply

TOOL getTokenLargestAccounts:
  params: {mint: string, limit?: u32}
  returns: Array<TokenAccount>

TOOL getLargestAccounts:
  params: {limit?: u32}
  returns: Array<Account>

TOOL getClusterNodes:
  returns: Array<ClusterNode>
```

### Solana Utility Tools

```
TOOL derivePDA:
  description: "Derive Program Derived Address"
  params:
    seeds: Array<bytes> (required)
    program: string (required)
  returns: string

TOOL deriveATA:
  description: "Derive Associated Token Account address"
  params:
    owner: string (required)
    mint: string (required)
  returns: string

TOOL parseU64:
  description: "Parse u64 from byte array at offset"
  params:
    data: bytes (required)
    offset: u32 (required)
  returns: u64

TOOL parseU128:
  description: "Parse u128 from byte array at offset"
  params:
    data: bytes (required)
    offset: u32 (required)
  returns: u128

TOOL parseI64:
  description: "Parse i64 from byte array at offset"
  params:
    data: bytes (required)
    offset: u32 (required)
  returns: i64

TOOL parsePubkey:
  description: "Parse public key from byte array"
  params:
    data: bytes (required)
    offset: u32 (required)
  returns: string

TOOL parseString:
  description: "Parse UTF-8 string from byte array"
  params:
    data: bytes (required)
    offset: u32 (required)
    length: u32 (required)
  returns: string

TOOL JSON_PARSE:
  description: "Parse JSON string to object"
  params:
    json_string: string (required)
  returns: object

TOOL JSON_STRINGIFY:
  description: "Convert object to JSON string"
  params:
    obj: object (required)
    pretty: bool (optional, default: false)
  returns: string

TOOL borshDeserialize:
  description: "Deserialize Borsh-encoded data"
  params:
    data: bytes (required)
    schema: object (required)
  returns: object

TOOL anchorDeserialize:
  description: "Deserialize Anchor instruction data"
  params:
    data: bytes (required)
    idl: IDL (required)
  returns: object
```

### Solana Analysis Helpers

```
TOOL extractPriorityFee:
  description: "Extract priority fee from transaction"
  params:
    tx: Transaction (required)
  returns: u64

TOOL extractTransactions:
  description: "Extract transactions from block"
  params:
    block: Block (required)
  returns: Array<Transaction>

TOOL extractHour:
  description: "Extract hour (0-23) from timestamp"
  params:
    timestamp: Timestamp (required)
  returns: u8

TOOL calculateSeverity:
  description: "Calculate alert severity level"
  params:
    current_value: f64 (required)
    threshold: f64 (required)
  returns: string  // "low", "medium", "high", "critical"
```

---

## Complete Examples

### Example 1: Simple Query with Branching

```
## Q: "What is the average transaction fee?"

**Expected Plan:**

[TIME: ~15s] [COST: free] [DATA: ~1000 transactions]

**Available Tools:**
From Standard Library:
  - getSlot (Solana RPC)
  - getBlock (Solana RPC)
  - MAP, FLATTEN (Data Processing)
  - MEAN, MEDIAN, STDDEV (Statistical)

**Main Branch:**
$current_slot = getSlot()
$blocks = []

FOR $i IN 0..10:
  $block = getBlock(slot: $current_slot - $i)
  $blocks = APPEND(array: $blocks, item: $block)

$all_transactions = FLATTEN(collection: MAP($blocks, b => b.transactions))
$fees = MAP(collection: $all_transactions, fn: tx => tx.meta.fee)

**Statistics:**
$mean_fee = MEAN(data: $fees)
$median_fee = MEDIAN(data: $fees)
$stddev = STDDEV(data: $fees)

DECISION: Check distribution
  BRANCH A ($stddev / $mean_fee < 0.5):
    // Normal distribution, use mean
    $result = $mean_fee
  BRANCH B ($stddev / $mean_fee >= 0.5):
    // High variance, use median
    $result = $median_fee
    $note = "High variance detected, using median"

**Action:**
RETURN {
  average_fee: $result,
  confidence: 95,
  sample_size: COUNT(collection: $fees),
  note: $note ?? "Normal distribution"
}
```

### Example 2: Parallel Execution with Error Handling

```
## Q: "Compare prices across multiple DEXes"

**Expected Plan:**

[TIME: ~10s] [COST: ~0.0001 SOL] [DATA: 3 price feeds]

**Available Tools:**
From Standard Library:
  - NOW, FILTER, MAX, MIN, AVG, COUNT, GUARD (Utility)
  - getAccountInfo (Solana RPC)
  - derivePDA, parseU64, parseU128, JSON_PARSE, FETCH_URL (Solana Utility)
  - POW (Math)

Custom Tools:
  - queryOrcaPool (defined below)
  - queryRaydiumPool (defined below)
  - getJupiterQuote (defined below)

DEFINE_TOOL queryOrcaPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    // Helper: Derive Orca pool address from token pair
    DEFINE deriveOrcaPoolAddress(pair):
      $seeds = [
        "whirlpool",
        pair[0],  // Token A mint
        pair[1]   // Token B mint
      ]
      RETURN derivePDA(seeds: $seeds, program: ORCA_PROGRAM_ID)

    // Helper: Parse price from Orca pool account data
    DEFINE parseOrcaPoolPrice(data):
      // Orca stores sqrt_price_x64 at offset 65
      $sqrt_price_x64 = parseU128(data: $data, offset: 65)
      // Convert to decimal price
      $price = POW(base: $sqrt_price_x64 / POW(base: 2, exponent: 64), exponent: 2)
      RETURN $price

    $pool_address = deriveOrcaPoolAddress($token_pair)
    TRY:
      $pool_account = getAccountInfo(pubkey: $pool_address)
      $price = parseOrcaPoolPrice($pool_account.data)
      RETURN $price
    CATCH:
      RETURN null

DEFINE_TOOL queryRaydiumPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    // Helper: Derive Raydium AMM pool address
    DEFINE deriveRaydiumPoolAddress(pair):
      $seeds = [
        "amm_associated_seed",
        pair[0],
        pair[1]
      ]
      RETURN derivePDA(seeds: $seeds, program: RAYDIUM_AMM_PROGRAM)

    // Helper: Parse price from Raydium pool state
    DEFINE parseRaydiumPoolPrice(data):
      // Raydium stores reserves at specific offsets
      $base_reserve = parseU64(data: $data, offset: 8)
      $quote_reserve = parseU64(data: $data, offset: 16)
      $price = $quote_reserve / $base_reserve
      RETURN $price

    $pool_address = deriveRaydiumPoolAddress($token_pair)
    TRY:
      $pool_account = getAccountInfo(pubkey: $pool_address)
      $price = parseRaydiumPoolPrice($pool_account.data)
      RETURN $price
    CATCH:
      RETURN null

DEFINE_TOOL getJupiterQuote:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    // Helper: Parse Jupiter API response
    DEFINE parseJupiterQuote(json_response):
      $parsed = JSON_PARSE($json_response)
      $in_amount = $parsed.inAmount
      $out_amount = $parsed.outAmount
      $price = $out_amount / $in_amount
      RETURN $price

    TRY:
      $url = "https://quote-api.jup.ag/v6/quote?inputMint={$token_pair[0]}&outputMint={$token_pair[1]}&amount=1000000"
      $response = FETCH_URL(url: $url)
      $price = parseJupiterQuote($response)
      RETURN $price
    CATCH:
      RETURN null

**Main Branch:**
$token_pair = ("SOL", "USDC")
$timestamp = NOW()

PARALLEL {
  TRY:
    $orca_price = queryOrcaPool(token_pair: $token_pair)
  CATCH RECOVERABLE:
    $orca_price = null

  TRY:
    $raydium_price = queryRaydiumPool(token_pair: $token_pair)
  CATCH RECOVERABLE:
    $raydium_price = null

  TRY:
    $jupiter_quote = getJupiterQuote(token_pair: $token_pair)
  CATCH RECOVERABLE:
    $jupiter_quote = null
}
WAIT_ALL

**Data Validation:**
$valid_prices = FILTER(
  [$orca_price, $raydium_price, $jupiter_quote],
  p => p != null
)

GUARD COUNT($valid_prices) > 0 ELSE RETURN ERROR(message: "No prices available")

**Analysis:**
$best_price = MAX($valid_prices)
$worst_price = MIN($valid_prices)
$avg_price = AVG($valid_prices)
$spread = ($best_price - $worst_price) / $avg_price * 100

**Action:**
RETURN {
  prices: {
    orca: $orca_price,
    raydium: $raydium_price,
    jupiter: $jupiter_quote
  },
  analysis: {
    best: $best_price,
    worst: $worst_price,
    average: $avg_price,
    spread_percent: $spread
  },
  recommendation: $spread > 5 ? "Significant arbitrage opportunity" : "Prices aligned"
}
```

### Example 3: Complex Branching with Fallbacks

```
## Q: "Analyze transaction and determine if it was optimal"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getTransaction, getAccountInfo (Solana RPC)
  - INPUT, ERROR (Utility)
  - MAP, FILTER, FIND, COUNT, MAX_BY, APPEND (Data Processing)
  - parseU64 (Solana Utility)

Custom Tools:
  - analyzeSwap (defined below)
  - analyzeTransfer (defined below)
  - getOptimalRoute (defined below)

DEFINE_TOOL analyzeSwap:
  params: {tx: Transaction}
  returns: SwapAnalysis
  implementation:
    $swap_instruction = FIND(
      collection: $tx.message.instructions,
      predicate: inst => inst.programId == DEX_PROGRAM_ID
    )

    // Parse swap instruction data
    $input_amount = parseU64(data: $swap_instruction.data, offset: 8)
    $output_amount = $tx.meta.postTokenBalances[0].uiAmount

    // Extract token mints from token balances
    $token_a = $tx.meta.preTokenBalances[0].mint
    $token_b = $tx.meta.postTokenBalances[0].mint

    RETURN {
      input_amount: $input_amount,
      output_amount: $output_amount,
      tokens: {from: $token_a, to: $token_b}
    }

DEFINE_TOOL analyzeTransfer:
  params: {tx: Transaction}
  returns: TransferAnalysis
  implementation:
    $amount = $tx.meta.postBalances[1] - $tx.meta.preBalances[1]
    RETURN {
      amount: $amount,
      from: $tx.message.accountKeys[0],
      to: $tx.message.accountKeys[1]
    }

DEFINE_TOOL getOptimalRoute:
  params: {input_amount: u64, tokens: object}
  returns: Route | null
  implementation:
    // Helper: Find all DEX pools for token pair
    DEFINE findPoolsForPair(token_a, token_b):
      PARALLEL {
        $orca_pool = deriveOrcaPoolAddress((token_a, token_b))
        $raydium_pool = deriveRaydiumPoolAddress((token_a, token_b))
      }
      WAIT_ALL
      RETURN [$orca_pool, $raydium_pool]

    // Helper: Calculate routes through pools
    DEFINE calculateRoutes(pools, amount):
      $routes = []
      FOR $pool IN $pools:
        TRY:
          $pool_account = getAccountInfo(pubkey: $pool)
          $expected_output = simulateSwap($pool_account, amount)
          $route = {
            pool: $pool,
            expected_output: $expected_output
          }
          $routes = APPEND(array: $routes, item: $route)
        CATCH:
          CONTINUE
      RETURN $routes

    // Helper: Simulate swap through a pool
    DEFINE simulateSwap(pool_account, input_amount):
      // Simplified constant product formula x * y = k
      $base_reserve = parseU64(data: $pool_account.data, offset: 8)
      $quote_reserve = parseU64(data: $pool_account.data, offset: 16)
      $k = $base_reserve * $quote_reserve
      $new_base = $base_reserve + $input_amount
      $new_quote = $k / $new_base
      $output = $quote_reserve - $new_quote
      RETURN $output

    $pools = findPoolsForPair($tokens.from, $tokens.to)
    $routes = calculateRoutes($pools, $input_amount)

    IF COUNT(collection: $routes) == 0 THEN
      RETURN null

    $best = MAX_BY($routes, r => r.expected_output)
    RETURN $best

**Main Branch:**
$signature = INPUT(prompt: "transaction_signature")

TRY:
  $tx = getTransaction(signature: $signature)
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

**Transaction Analysis:**
$instruction = $tx.message.instructions[0]
$program_id = $instruction.programId

DECISION: Identify transaction type
  BRANCH A ($program_id == DEX_PROGRAM):
    $type = "swap"
    $analysis = analyzeSwap(tx: $tx)

  BRANCH B ($program_id == LENDING_PROGRAM):
    $type = "lending"
    $analysis = {type: "lending"} // Simplified

  BRANCH C ($program_id == SYSTEM_PROGRAM):
    $type = "transfer"
    $analysis = analyzeTransfer(tx: $tx)

  BRANCH D (default):
    $type = "unknown"
    $analysis = {basic: true}

**Optimization Check:**
IF $type == "swap" THEN
  // Detailed swap analysis
  $input = $analysis.input_amount
  $output = $analysis.output_amount
  $actual_price = $output / $input

  PARALLEL {
    TRY:
      $optimal_route = getOptimalRoute(
        input_amount: $input,
        tokens: $analysis.tokens
      )
    CATCH:
      $optimal_route = null

    TRY:
      $alternative_prices = [] // Simplified for example
    CATCH:
      $alternative_prices = []
  }
  WAIT_ALL

  IF $optimal_route != null THEN
    $optimal_price = $optimal_route.expected_output / $input
    $efficiency = ($actual_price / $optimal_price) * 100
  ELSE
    $efficiency = null

  $verdict = $efficiency >= 98 ? "optimal" :
             $efficiency >= 95 ? "good" :
             $efficiency >= 90 ? "acceptable" : "suboptimal"
ELSE
  $efficiency = null
  $verdict = "not_applicable"

**Final Report:**
RETURN {
  transaction: $signature,
  type: $type,
  analysis: $analysis,
  optimization: {
    efficiency_score: $efficiency,
    verdict: $verdict,
    could_have_saved: $efficiency != null ?
      ($optimal_route.expected_output - $output) : null
  },
  confidence: $efficiency != null ? 90 : 60
}
```

### Example 4: Loop with Early Exit

```
## Q: "Find first block containing a specific transaction type"

**Expected Plan:**

[TIME: ~60s max] [COST: variable] [DATA: up to 1000 blocks]

**Available Tools:**
From Standard Library:
  - getSlot, getBlock (Solana RPC)
  - INPUT (Utility)
  - ANY (Data Processing)

**Main Branch:**
$target_program = INPUT("program_id")
$current_slot = getSlot()
$max_blocks = 1000
$found = false

FOR $i IN 0..$max_blocks:
  $slot = $current_slot - $i

  TRY:
    $block = getBlock($slot)
  CATCH:
    CONTINUE  // Skip missing blocks

  $transactions = $block.transactions

  FOR $tx IN $transactions:
    $has_program = ANY(
      $tx.message.instructions,
      inst => inst.programId == $target_program
    )

    IF $has_program THEN
      $found_tx = $tx
      $found_slot = $slot
      $found = true
      BREAK  // Exit inner loop

  BREAK IF $found  // Exit outer loop

**Result:**
IF $found THEN
  RETURN {
    found: true,
    transaction: $found_tx,
    slot: $found_slot,
    blocks_searched: $i + 1
  }
ELSE
  RETURN {
    found: false,
    blocks_searched: $max_blocks,
    message: "No transaction found in last {$max_blocks} blocks"
  }
```

---

## Best Practices

### 1. Use Clear Variable Names
```
// Good
$transaction_fee = $tx.meta.fee
$average_compute_units = AVG($compute_values)

// Bad
$f = $tx.meta.fee
$avg = AVG($vals)
```

### 2. Show Data Flow Explicitly
```
// Good
$slot = getSlot() → $current_slot
$block = getBlock($current_slot) → $block_data
$transactions = EXTRACT($block_data.transactions)

// Also acceptable
$slot = getSlot()
$block = getBlock($slot)
$transactions = $block.transactions
```

### 3. Use Metadata Tags
```
// Good - shows expectations upfront
[TIME: ~30s] [COST: ~0.001 SOL] [CONFIDENCE: 95%]
$result = complexOperation()

// Less informative
$result = complexOperation()
```

### 4. Handle Errors Explicitly
```
// Good
TRY:
  $data = riskyOperation()
CATCH RECOVERABLE:
  $data = fallbackOperation()
CATCH FATAL:
  RETURN ERROR(message: "Operation failed")

// Bad - vague
$data = riskyOperation() // handle errors somehow
```

### 5. Break Complex Logic into Sections
```
// Good
**Data Collection:**
[collection logic]

**Data Transformation:**
[transformation logic]

**Analysis:**
[analysis logic]

// Bad - everything in one block
[all logic together]
```

### 6. Use Decision Points for Clarity
```
// Good
DECISION: Determine sampling strategy
  BRANCH A (small dataset):
    [exact approach]
  BRANCH B (large dataset):
    [sampling approach]

// Less clear
IF small THEN [exact] ELSE [sampling]
```

### 7. Document Complex Decisions
```
// Good
/**
 * We use median instead of mean when stddev is high
 * because outlier transactions can skew the average.
 * Threshold: stddev/mean > 0.5
 */
$result = $stddev/$mean > 0.5 ? MEDIAN($data) : MEAN($data)
```

---

## Language Grammar (EBNF)

```ebnf
program = metadata? constants? tool_definitions? main_branch decision_point* action

metadata = "[" tag_name ":" tag_value "]"+

constants = ("CONST" UPPERCASE_ID "=" value)+

tool_definitions = (tool_def | custom_tool_def)+

tool_def = "TOOL" identifier ":" params? "returns" ":" type

custom_tool_def = "DEFINE_TOOL" identifier ":" params? "returns" ":" type "implementation" ":" statement+

main_branch = "**Main Branch:**" statement+

decision_point = "**Decision Point:**" description branch+

branch = "BRANCH" identifier "(" condition "):" statement+

statement = assignment | const_def | tool_call | control_flow | comment | helper_def

assignment = "$" identifier "=" expression

const_def = "CONST" UPPERCASE_ID "=" value

helper_def = "DEFINE" identifier "(" params? ")" ":" statement+

tool_call = identifier "(" arguments? ")"

expression = literal | variable | binary_op | ternary | tool_call | optional_chain

binary_op = expression operator expression

ternary = condition "?" expression ":" expression

optional_chain = expression ("?." identifier)+

control_flow = if_stmt | while_loop | for_loop | loop_every | try_catch | parallel | guard

if_stmt = "IF" condition "THEN" statement+ ("ELSE IF" condition "THEN" statement+)* ("ELSE" statement+)?

while_loop = "WHILE" condition ":" statement+ ("BREAK IF" condition)?

for_loop = "FOR" variable "IN" expression ":" statement+

loop_every = "LOOP" "EVERY" duration ":" statement+

try_catch = "TRY:" statement+ "CATCH" error_type? ":" statement+

error_type = "FATAL" | "RECOVERABLE" | "WARNING"

guard = "GUARD" condition "ELSE" statement

parallel = "PARALLEL" "{" statement+ "}" wait_strategy?

wait_strategy = "WAIT_ALL" | "WAIT_ANY" | "RACE"

action = "**Action:**" description
```

---

## Version History

**v1.1 (Current)**
- Added tool naming convention documentation
- Fixed all method call syntax (.map, .push, .to_text)
- Improved Examples 1-4 with "Available Tools" sections
- Consistency improvements

**v1.0**
- Initial specification
- Core language features
- Error handling
- Parallel execution
- Decision logic
- No emoji support (by design)

---

## Future Considerations

### Multi-Agent & Research Features

**1. Agent Delegation & Subtask Spawning**
```
SPAWN_AGENT research_agent WITH:
  task: "Analyze validator performance"
  context: $validator_data
  timeout: 60s
AWAIT $agent_result

// Parallel agent spawning
PARALLEL_AGENTS {
  $price_analysis = SPAWN price_analyzer($token_pair)
  $volume_analysis = SPAWN volume_analyzer($token_pair)
  $sentiment_analysis = SPAWN sentiment_analyzer($token_pair)
}
MERGE_RESULTS($price_analysis, $volume_analysis, $sentiment_analysis)
```

**2. Research State Management**
```
// Define research-specific tools
DEFINE_TOOL collect_fees:
  params: {time_range: Duration}
  returns: Array<f64>
  implementation:
    $current_slot = getSlot()
    $blocks = []
    FOR $i IN 0..1000:
      $block = getBlock(slot: $current_slot - $i)
      $blocks = APPEND(array: $blocks, item: $block)
    $all_txs = FLATTEN(MAP($blocks, b => b.transactions))
    $fees = MAP($all_txs, tx => tx.meta.fee)
    RETURN $fees

DEFINE_TOOL collect_congestion:
  params: {time_range: Duration}
  returns: Array<f64>
  implementation:
    $samples = getRecentPerformanceSamples()
    $tps_values = MAP($samples, s => s.numTransactions / s.samplePeriodSecs)
    RETURN $tps_values

RESEARCH_STATE:
  hypothesis: "High fees correlate with network congestion"
  evidence: []
  confidence: 0%

GATHER_EVIDENCE:
  $fee_data = collect_fees(time_range: "7 days")
  $congestion_data = collect_congestion(time_range: "7 days")
  $correlation = CORRELATE(x: $fee_data, y: $congestion_data)

  UPDATE_STATE:
    evidence = APPEND(array: evidence, item: $correlation)
    confidence = calculateConfidence(evidence: evidence)

IF confidence > 95% THEN
  CONCLUDE hypothesis AS "proven"
ELSE IF confidence > 70% THEN
  CONCLUDE hypothesis AS "likely"
ELSE
  CONCLUDE hypothesis AS "inconclusive"
  SUGGEST_NEXT_STEPS: ["gather more data", "refine hypothesis"]
```

**3. Knowledge Graph Building**
```
KNOWLEDGE_GRAPH:
  nodes: []
  edges: []

FOR $transaction IN $dataset:
  EXTRACT_ENTITIES($transaction) → $entities

  FOR $entity IN $entities:
    GRAPH.ADD_NODE(
      type: $entity.type,
      id: $entity.id,
      properties: $entity.data
    )

  EXTRACT_RELATIONSHIPS($transaction) → $relationships

  FOR $rel IN $relationships:
    GRAPH.ADD_EDGE(
      from: $rel.source,
      to: $rel.target,
      type: $rel.relationship,
      weight: $rel.strength
    )

QUERY_GRAPH:
  FIND_PATH(from: "address_A", to: "address_B", max_hops: 5)
  FIND_CLUSTERS(algorithm: "community_detection")
  FIND_CENTRAL_NODES(metric: "betweenness")
```

**4. Hypothesis Testing Framework**
```
HYPOTHESIS "Validators in US have lower skip rates":
  null_hypothesis: "Location has no effect on skip rate"
  alternative: "US validators have lower skip rates"
  significance_level: 0.05

COLLECT_DATA:
  $us_validators = FILTER($all_validators, v => v.location == "US")
  $other_validators = FILTER($all_validators, v => v.location != "US")

  $us_skip_rates = MAP($us_validators, v => v.skip_rate)
  $other_skip_rates = MAP($other_validators, v => v.skip_rate)

STATISTICAL_TEST:
  $test = T_TEST($us_skip_rates, $other_skip_rates)
  $p_value = $test.p_value

  IF $p_value < 0.05 THEN
    REJECT null_hypothesis
    CONFIDENCE: "statistically significant"
  ELSE
    FAIL_TO_REJECT null_hypothesis
    CONFIDENCE: "not statistically significant"
```

**5. Progressive Refinement**
```
// Define analysis tools
DEFINE_TOOL analyze_fees_distribution:
  returns: Array<f64>
  implementation:
    $current_slot = getSlot()
    $blocks = []
    FOR $i IN 0..100:
      $block = getBlock(slot: $current_slot - $i)
      $blocks = APPEND(array: $blocks, item: $block)
    $txs = FLATTEN(collection: MAP($blocks, b => b.transactions))
    $fees = MAP(collection: $txs, fn: tx => tx.meta.fee)
    RETURN $fees

ITERATIVE_RESEARCH:
  initial_question: "What causes high transaction fees?"
  depth: 0
  max_depth: 5

RESEARCH_LOOP:
  depth += 1

  STEP_1: Gather broad data
    $fee_distribution = analyze_fees_distribution()

  STEP_2: Identify patterns
    $outliers = DETECT_OUTLIERS(data: $fee_distribution)
    $patterns = FIND_PATTERNS(data: $outliers)

  STEP_3: Generate sub-questions
    $sub_questions = []
    FOR $pattern IN $patterns:
      $sub_questions = APPEND(array: $sub_questions, item: "Why does {$pattern} occur?")

  STEP_4: Research each sub-question
    FOR $question IN $sub_questions:
      IF depth < max_depth THEN
        RECURSIVE_RESEARCH($question, depth)

  STEP_5: Synthesize findings
    MERGE_INSIGHTS($all_findings) → $comprehensive_answer

PRESENT_FINDINGS:
  structure: "tree"
  show_evidence: true
  confidence_scores: true
```

**6. Cross-Validation with Multiple Data Sources**
```
CROSS_VALIDATE:
  sources: ["RPC1", "RPC2", "Archive", "Indexer"]
  query: $original_query

PARALLEL {
  $result1 = QUERY(source: "RPC1", query: $query)
  $result2 = QUERY(source: "RPC2", query: $query)
  $result3 = QUERY(source: "Archive", query: $query)
  $result4 = QUERY(source: "Indexer", query: $query)
}
WAIT_ALL

CONSISTENCY_CHECK:
  $agreements = COMPARE_RESULTS([$result1, $result2, $result3, $result4])

  IF $agreements.consensus >= 75% THEN
    $validated_result = $agreements.consensus_value
    $confidence = 100%
  ELSE IF $agreements.consensus >= 50% THEN
    $validated_result = $agreements.majority_value
    $confidence = 75%
    $note = "Some disagreement between sources"
  ELSE
    $validated_result = null
    $confidence = 0%
    INVESTIGATE_DISCREPANCY($agreements.differences)
```

**7. Contextual Memory & Learning**
```
MEMORY_CONTEXT:
  short_term: []   // Current session findings
  long_term: []    // Persistent knowledge
  working_set: []  // Active hypotheses

LEARN_FROM_QUERY:
  $query_result = executeQuery($query)

  EXTRACT_INSIGHTS:
    $patterns = identifyPatterns($query_result)
    $anomalies = detectAnomalies($query_result)
    $correlations = findCorrelations($query_result)

  STORE_KNOWLEDGE:
    FOR $pattern IN $patterns:
      IF novelty(pattern: $pattern, existing: MEMORY.long_term) > threshold THEN
        MEMORY.long_term = APPEND(array: MEMORY.long_term, item: {
          pattern: $pattern,
          confidence: calculateConfidence(evidence: [$pattern]),
          first_seen: NOW(),
          occurrences: 1
        })
      ELSE
        UPDATE_EXISTING_PATTERN($pattern)

APPLY_LEARNED_KNOWLEDGE:
  $relevant_knowledge = QUERY_MEMORY(
    context: $current_query,
    similarity_threshold: 0.8
  )

  USE_KNOWLEDGE_TO_OPTIMIZE:
    - Skip redundant queries
    - Pre-filter unlikely hypotheses
    - Suggest related investigations
```

**8. Confidence Scoring & Uncertainty Quantification**
```
CONFIDENCE_CALCULATION:
  factors: {
    data_quality: 0..100,
    sample_size: 0..100,
    consistency: 0..100,
    source_reliability: 0..100,
    method_robustness: 0..100
  }

FOR $finding IN $research_findings:
  $confidence = WEIGHTED_AVERAGE([
    $factors.data_quality * 0.25,
    $factors.sample_size * 0.20,
    $factors.consistency * 0.25,
    $factors.source_reliability * 0.15,
    $factors.method_robustness * 0.15
  ])

  $uncertainty = 100 - $confidence

  ANNOTATE_RESULT:
    result: $finding,
    confidence: "{$confidence}%",
    uncertainty_range: calculateRange($uncertainty),
    caveats: identifyCaveats($finding)
```

**9. Automated Literature Review (Code & Documentation)**
```
LITERATURE_REVIEW:
  topic: "Solana consensus mechanism"
  sources: ["github", "docs", "forums", "papers"]

GATHER_SOURCES:
  PARALLEL {
    $code = SEARCH_GITHUB(query: "solana consensus", language: "rust")
    $docs = SEARCH_DOCS(query: "proof of history")
    $discussions = SEARCH_FORUMS(query: "tower bft")
    $papers = SEARCH_ARXIV(query: "solana blockchain")
  }
  WAIT_ALL

ANALYZE_SOURCES:
  FOR $source IN ALL_SOURCES:
    $key_concepts = EXTRACT_CONCEPTS($source)
    $relationships = MAP_RELATIONSHIPS($key_concepts)

    CLASSIFY:
      category: $source.type,
      relevance: CALCULATE_RELEVANCE($source, $topic),
      credibility: ASSESS_CREDIBILITY($source),
      recency: $source.date

SYNTHESIZE_REVIEW:
  GROUP_BY_THEME($all_concepts) → $themes

  FOR $theme IN $themes:
    $summary = SUMMARIZE_THEME($theme)
    $consensus = FIND_CONSENSUS($theme.sources)
    $conflicts = IDENTIFY_CONFLICTS($theme.sources)

    RETURN {
      theme: $theme.name,
      summary: $summary,
      consensus: $consensus,
      conflicts: $conflicts,
      key_sources: TOP_N($theme.sources, n: 5, by: "credibility")
    }
```

**10. Dynamic Query Planning**
```
ADAPTIVE_QUERY_PLANNER:
  goal: $user_goal
  constraints: {
    max_time: 60s,
    max_cost: 0.01 SOL,
    min_confidence: 80%
  }

PLAN_GENERATION:
  $possible_plans = ENUMERATE_PLANS($goal)

  FOR $plan IN $possible_plans:
    ESTIMATE:
      time: $plan.estimated_time
      cost: $plan.estimated_cost
      confidence: $plan.expected_confidence

    SCORE:
      $score = CALCULATE_SCORE(
        meets_constraints: CHECK_CONSTRAINTS($plan),
        efficiency: $plan.cost / $plan.confidence,
        reliability: $plan.robustness
      )

SELECT_OPTIMAL_PLAN:
  $best_plan = MAX_BY($possible_plans, plan => plan.score)

EXECUTE_WITH_MONITORING:
  $start_time = NOW()
  $actual_cost = 0

  FOR $step IN $best_plan.steps:
    EXECUTE($step)

    // Monitor and adapt
    IF NOW() - $start_time > $constraints.max_time * 0.8 THEN
      SWITCH_TO_FASTER_PLAN()

    IF $actual_cost > $constraints.max_cost * 0.8 THEN
      SWITCH_TO_CHEAPER_PLAN()
```

**11. Causal Inference**
```
CAUSAL_ANALYSIS:
  question: "Does increasing priority fee cause faster confirmation?"

DESIGN_EXPERIMENT:
  treatment: "high priority fee"
  control: "low priority fee"
  outcome: "confirmation time"
  confounders: ["network congestion", "time of day", "transaction size"]

COLLECT_OBSERVATIONAL_DATA:
  $data = GATHER_TRANSACTIONS(
    time_range: "last 7 days",
    sample_size: 10000
  )

CONTROL_FOR_CONFOUNDERS:
  $matched_data = PROPENSITY_SCORE_MATCHING(
    data: $data,
    treatment_var: "priority_fee",
    confounders: ["congestion", "time", "size"]
  )

ESTIMATE_EFFECT:
  $treatment_group = FILTER($matched_data, tx => tx.priority_fee > threshold)
  $control_group = FILTER($matched_data, tx => tx.priority_fee <= threshold)

  $treatment_avg_time = AVG(collection: MAP($treatment_group, tx => tx.confirmation_time))
  $control_avg_time = AVG(collection: MAP($control_group, tx => tx.confirmation_time))

  $causal_effect = $control_avg_time - $treatment_avg_time

  STATISTICAL_SIGNIFICANCE:
    $p_value = CALCULATE_P_VALUE($treatment_group, $control_group)

  RETURN {
    effect: $causal_effect,
    significant: $p_value < 0.05,
    conclusion: $causal_effect > 0 ?
      "Higher priority fees DO cause faster confirmation" :
      "No causal relationship found"
  }
```

**12. Multi-Modal Data Fusion**
```
FUSION_ANALYSIS:
  modalities: ["on-chain", "social", "code", "price"]

PARALLEL_COLLECTION:
  PARALLEL {
    $chain_data = ANALYZE_CHAIN($token_address)
    $social_data = ANALYZE_SOCIAL($token_name)
    $code_data = ANALYZE_CODE($program_id)
    $price_data = ANALYZE_PRICE($token_address)
  }
  WAIT_ALL

ALIGN_TIMELINES:
  $unified_timeline = ALIGN_BY_TIMESTAMP([
    $chain_data,
    $social_data,
    $code_data,
    $price_data
  ])

CROSS_MODAL_ANALYSIS:
  FOR $event IN $unified_timeline:
    CORRELATE:
      - Chain activity spike → Social sentiment
      - Code deployment → Price movement
      - Social buzz → Chain activity

    DETECT_LEADING_INDICATORS:
      $lag_analysis = CALCULATE_TIME_LAGS($correlations)

    IDENTIFY_CAUSALITY:
      $granger_test = GRANGER_CAUSALITY_TEST($event.signals)

SYNTHESIZE_INSIGHTS:
  RETURN {
    relationships: $correlations,
    leading_indicators: $lag_analysis,
    causal_chains: $granger_test,
    predictive_signals: EXTRACT_PREDICTIVE_SIGNALS($relationships)
  }
```

**13. Explanation Generation**
```
EXPLAINABLE_RESEARCH:
  finding: $research_result
  target_audience: "non-technical" | "technical" | "expert"

GENERATE_EXPLANATION:
  STEP_1: Identify key concepts
    $concepts = EXTRACT_KEY_CONCEPTS($finding)

  STEP_2: Build explanation tree
    $tree = CONSTRUCT_EXPLANATION_TREE(
      root: $finding,
      depth: BASED_ON($target_audience),
      detail: BASED_ON($target_audience)
    )

  STEP_3: Generate natural language
    FOR $node IN $tree:
      IF $node.requires_explanation THEN
        $explanation = EXPLAIN(
          concept: $node.concept,
          use_analogies: $target_audience == "non-technical",
          include_technical_details: $target_audience == "expert"
        )
        $node.text = $explanation

  STEP_4: Add supporting evidence
    FOR $claim IN $finding.claims:
      $evidence = GATHER_EVIDENCE($claim)
      ANNOTATE_WITH_CITATIONS($claim, $evidence)

  STEP_5: Provide confidence rationale
    EXPLAIN_CONFIDENCE:
      "We are {$confidence}% confident because:"
      FOR $factor IN $confidence_factors:
        "- {$factor.name}: {$factor.description}"

RETURN formatted_explanation WITH:
  - Clear introduction
  - Step-by-step reasoning
  - Supporting evidence
  - Limitations and caveats
  - Confidence explanation
  - Suggested follow-up questions
```

**14. Real-Time Monitoring & Alerting**
```
MONITORING_CONTEXT:
  watches: []
  triggers: []
  alerts: []

SETUP_WATCH:
  WATCH validator_performance:
    metric: "skip_rate"
    threshold: 5%
    duration: "1 hour"
    action: ALERT + INVESTIGATE

CONTINUOUS_MONITORING:
  LOOP EVERY 30s:
    FOR $watch IN $watches:
      $current_value = MEASURE($watch.metric)

      IF THRESHOLD_EXCEEDED($current_value, $watch.threshold) THEN
        $alert = CREATE_ALERT(
          type: $watch.metric,
          severity: calculateSeverity($current_value),
          context: GATHER_CONTEXT($watch)
        )

        TRIGGER_INVESTIGATION:
          SPAWN_AGENT investigator WITH:
            focus: $alert.type
            context: $alert.context
            goal: "Determine root cause"

          $findings = AWAIT investigator

          IF $findings.actionable THEN
            RECOMMEND_ACTIONS($findings)
```

**15. Meta-Learning & Strategy Optimization**
```
META_LEARNING:
  history: []  // Past queries and their performance

TRACK_PERFORMANCE:
  FOR $completed_query IN $history:
    RECORD:
      query_type: $query.type
      strategy_used: $query.strategy
      time_taken: $query.duration
      cost: $query.cost
      accuracy: $query.accuracy
      user_satisfaction: $query.rating

LEARN_OPTIMAL_STRATEGIES:
  FOR $query_type IN UNIQUE(collection: MAP($history, q => q.type)):
    $queries_of_type = FILTER($history, q => q.type == $query_type)

    $best_strategy = MAX_BY($queries_of_type, q =>
      SCORE(q.accuracy, q.duration, q.cost, q.user_satisfaction)
    )

    STORE_LEARNING:
      query_type: $query_type
      recommended_strategy: $best_strategy.strategy
      confidence: CALCULATE_CONFIDENCE($queries_of_type.length)

APPLY_LEARNING:
  WHEN new_query ARRIVES:
    $learned_strategy = LOOKUP_STRATEGY($new_query.type)

    IF $learned_strategy.confidence > 80% THEN
      USE_STRATEGY($learned_strategy)
    ELSE
      EXPLORE_NEW_STRATEGY() // Epsilon-greedy approach
```

### Other Advanced Features

**16. Type System with Inference**
- Static type checking for tool parameters
- Generic types for reusable patterns
- Union and intersection types
- Gradual typing support

**17. Module/Function Definitions**
```
DEFINE_FUNCTION analyze_fees($transactions):
  $fees = MAP($transactions, tx => tx.fee)
  RETURN {
    mean: MEAN($fees),
    median: MEDIAN($fees),
    p95: PERCENTILE($fees, 95)
  }

IMPORT common_functions FROM "stdlib"
USE analyze_fees, calculate_confidence
```

**18. Assertions and Contracts**
```
ASSERT $balance > 0, "Balance must be positive"
REQUIRE $data.length >= 100, "Insufficient sample size"
ENSURE $result.confidence >= 80%, "Result not confident enough"

INVARIANT DURING analysis:
  $state.consistency == true
```

**19. Performance Profiling**
```
PROFILE:
  $result = expensive_operation()
WITH_METRICS:
  time: $elapsed_time
  memory: $memory_used
  api_calls: $total_calls
  cost: $total_cost
```

**20. Debugging Directives**
```
BREAKPOINT WHEN $count > 1000
LOG $intermediate_result AT checkpoint
TRACE $data_flow FROM source TO destination
WATCH $variable FOR changes
```

---

## License

This specification is released under MIT License.
Free to use, modify, and distribute with attribution.

---

## Contributing

To propose changes to this specification:
1. Document the use case
2. Provide example syntax
3. Explain benefits over current approach
4. Show backward compatibility plan (if breaking change)

---

**END OF SPECIFICATION**
