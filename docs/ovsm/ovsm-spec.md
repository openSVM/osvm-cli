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

Variables use LISP-style define:

```
(define variable_name value)
(define count 100)
(define address "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv")
```

### Constants

Constants use const and are immutable:

```
;; Solana System Programs
(const SYSTEM_PROGRAM "11111111111111111111111111111111")
(const TOKEN_PROGRAM "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA")
(const TOKEN_2022_PROGRAM "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb")
(const ASSOCIATED_TOKEN_PROGRAM "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL")

;; DeFi Programs
(const RAYDIUM_AMM_PROGRAM "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8")
(const ORCA_WHIRLPOOL_PROGRAM "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc")
(const JUPITER_AGGREGATOR_V6 "JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4")

;; Lending Programs
(const SOLEND_PROGRAM "So1endDq2YkqhipRh3WViPa8hdiSpxWy6z3Z6tMCpAo")
(const MANGO_V4_PROGRAM "4MangoMjqJ2firMokCjjGgoK8d4MXcrgL7XJaL3w6fVg")

;; System Limits
(const MAX_COMPUTE_UNITS 1400000)
(const MAX_TRANSACTION_SIZE 1232)
(const MAX_ACCOUNTS 256)
(const MAX_CPI_DEPTH 4)
(const LAMPORTS_PER_SOL 1000000000)

;; Network
(const SLOTS_PER_EPOCH 432000)
(const SLOT_DURATION_MS 400)

;; Aliases for clarity
(const DEX_PROGRAM_ID RAYDIUM_AMM_PROGRAM)  ;; Example: can be any DEX
(const ORCA_PROGRAM_ID ORCA_WHIRLPOOL_PROGRAM)
(const LENDING_PROGRAM SOLEND_PROGRAM)      ;; Example: can be any lending protocol

;; Usage in conditionals
(if (= program_id DEX_PROGRAM_ID)
  ;; Handle DEX transaction
  null)

(if (> compute_units MAX_COMPUTE_UNITS)
  (ERROR :message "Exceeds compute limit")
  null)

(if (> accounts_count MAX_ACCOUNTS)
  (ERROR :message "Too many accounts")
  null)
```

### Data Types

**Primitives:**
```
(define number 42)
(define float 3.14)
(define string "hello")
(define boolean true)
(define null_value null)
```

**Collections:**
```
(define array [1 2 3 4 5])
(define object {:key "value" :count 10})
(define tuple [value1 value2 value3])
```

### Type Annotations (Optional)

```
;; Type annotations are comments - LISP is dynamically typed
(define result (getTransaction sig))  ;; Transaction
(define balance (getBalance address))  ;; u64
(define prices (fetchPrices))  ;; Array<f64>
```

### Destructuring

```
(define epoch_info (getEpochInfo))
(define slot (. epoch_info slot))
(define epoch (. epoch_info epoch))

(define first (nth array 0))
(define second (nth array 1))
(define rest (slice array 2))

(define transaction tx_data)
(define fee (. (. transaction meta) fee))
(define err (. (. transaction meta) err))
```

---

## Operators

### Arithmetic
```
;; Variadic operators
(+ 1 2 3)       ;; Addition
(- 10 3 2)      ;; Subtraction
(* 2 3 4)       ;; Multiplication
(/ 100 2 5)     ;; Division
(% 17 5)        ;; Modulo

(define total (* price quantity))
(define average (/ sum count))
```

**Division by Zero:**
Division by zero results in runtime error. Use guard clauses to prevent:
```
(if (<= count 0)
    (ERROR :message "Division by zero")
    (define average (/ sum count)))
```

### Comparison
```
(= x y)         ;; Equal
(!= x y)        ;; Not equal
(> x y)         ;; Greater than
(< x y)         ;; Less than
(>= x y)        ;; Greater or equal
(<= x y)        ;; Less or equal

(define is_high (> value 1000))
(define matches (= type "transfer"))
```

### Logical
```
(and true false)         ;; Logical AND
(or true false)          ;; Logical OR
(not true)               ;; Logical NOT

(define valid (and exists (not expired)))
(define should_retry (or (not success) timeout))
```

### Data Flow Arrow

**Important**: Arrows are **documentation/annotation only**, not executable operators.

```
→  (single arrow for data flow annotation)
⇒  (double arrow for significant transformation annotation)
```

**Usage Pattern 1: Pipeline Annotation (for clarity)**
```
;; Shows data flow for reader understanding
(define slot (getSlot))        ;; → current slot number
(define block (getBlock slot)) ;; → block data
(define txs (. block transactions)) ;; → transaction array
```

**Usage Pattern 2: Inline Comments**
```
;; getSlot() → getBlock() → extractTransactions()
;; Read as: "getSlot FLOWS TO getBlock FLOWS TO extractTransactions"
;; This is a COMMENT showing the conceptual flow
```

**What arrows are NOT:**
- NOT a pipe operator (use function composition instead)
- NOT an assignment operator (use `define`)
- NOT an executable statement

**Correct Usage:**
```
;; Good: Arrow as annotation
(define data (processData))  ;; → cleaned data

;; Good: Showing conceptual flow in comment
;; Flow: getData() → transform() → save()

;; Bad: Trying to use arrow as operator
;; (define result (→ (getData) (transform)))  ;; WRONG - not valid syntax
```

**For Actual Piping:**
Use function composition or chaining:
```
(define result (transform :data (getData)))
;; Or define a pipe tool if needed
```

### Optional/Null Handling
```
;; Use conditional checks for null handling
(define fee
  (if (and transaction (. transaction meta))
      (. (. transaction meta) fee)
      0))

(define name
  (if (and account (. account data))
      (. (. account data) name)
      "Unknown"))
```

### Ternary Conditional
```
;; Use if expressions for conditional values
(define strategy (if (> count 10000) "sample" "exact"))
(define label (if (>= score 90) "good" "needs_improvement"))
```

### Range and Membership
```
;; Use functions for membership testing
(define valid (contains? [1 2 3 4 5] value))
(define in_range (and (>= score 0) (<= score 100)))
```

---

## Control Flow

### Conditional Statements

**IF-ELSE:**
```
(if condition
  [statements]
ELSE (if other_condition
  [statements]
ELSE
  [statements]
```

**Example:**
```
(define strategy
  (if (> count 1000)
      "sample"
      (if (> count 100)
          "paginate"
          "fetch_all")))
```

### Loops

**WHILE:**
```
while condition
  [statements]
  BREAK IF exit_condition
  CONTINUE IF skip_condition
```

**FOR:**
```
for (item collection)
  [process item]
  BREAK IF satisfied
  CONTINUE IF should_skip
```

**FOR with Range:**
```
for (i 0..100)
  [process iteration]
```

**FOR with Index:**
```
FOR item, index IN collection:
  [use item and index]
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
;; Check every 30 seconds
(loop-every 30s
  (do
    (define value (checkMetric))
    (log :message (format "Current value: ~a" value))
    (if (< value threshold)
        (break)
        null)))

;; Check every 5 minutes
(loop-every 5min
  (do
    (define data (collectData))
    (processData data)
    (if (dataComplete)
        (break)
        null)))

;; With timeout
(timeout 10min
  (loop-every 1s
    (do
      (define status (checkStatus))
      (if (= status "ready")
          (break)
          null))))
```

### Return Early

```
RETURN result IF condition_met
RETURN early IF data == null
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
    ;; ... all RPC methods
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
    ;; ... data functions
  ]

  analysis: [
    CORRELATE,
    T_TEST,
    DETECT_OUTLIERS,
    FIND_PATTERNS,
    CALCULATE_CONFIDENCE,
    ;; ... statistical functions
  ]
```

### Basic Invocation

Tool calls must use defined tools:

```
;; Direct invocation
(define slot CALL getSlot())
(define tx CALL getTransaction(signature: sig))

;; Shorthand (when unambiguous)
(define slot getSlot())
(define tx getTransaction(signature))
```

### Checking Tool Availability

```
(if TOOL_EXISTS("getSlot")
  (define slot getSlot())
ELSE
  ERROR("Tool getSlot not available")

;; Or use guard
REQUIRE_TOOL getSlot
(define slot getSlot())
```

### Chained Invocation

```
(define result getSlot() → getBlock(result) → extractTransactions(result))
(define data fetchData() → parseData() → validateData())
```

### Named Parameters

```
(define block CALL getBlock()
  slot: current_slot,
  commitment: "finalized",
  maxSupportedTransactionVersion: 0
)
```

### Tool Call with Error Handling

```
TRY:
  (define transactions CALL getSignaturesForAddress()
    address: pubkey,
    limit: 1000,
    before: $cursor
  )
CATCH:
  ERROR(message: "Failed to fetch signatures")
```

### Parameter Syntax Rules

**Positional Parameters** (use when obvious and 1-2 params):
```
(define count COUNT(array))
(define first FIRST(array))
(define sum SUM(numbers))
```

**Named Parameters** (use when 3+ params OR clarity needed):
```
(define block getBlock()
  slot: current_slot,
  commitment: "finalized",
  maxSupportedTransactionVersion: 0
)

(define fee_analysis analyze_fees(block: block))
```

**Mixed Parameters** (positional first, named after):
```
(define filtered FILTER(array, item => item > 10))
;; OR with named for clarity
(define filtered FILTER()
  collection: array,
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
    (define transactions block.transactions)
    (define fees MAP(collection: transactions, fn: tx => tx.meta.fee))
    RETURN {
      mean: MEAN(data: fees),
      median: MEDIAN(data: fees),
      p95: PERCENTILE(data: fees, percentile: 95)
    }

;; Use custom tool
(define block getBlock(slot: slot))
(define fee_analysis analyze_fees(block: block))
```

### Helper Functions Within DEFINE_TOOL

For internal helper functions that aren't reusable tools:

```
DEFINE_TOOL queryOrcaPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    ;; Define local helper functions with DEFINE
    DEFINE derivePoolAddress(pair):
      (define seeds ["whirlpool", pair[0], pair[1]])
      RETURN derivePDA(seeds: seeds, program: ORCA_PROGRAM_ID)

    DEFINE parsePoolPrice(data):
      (define sqrt_price parseU64(data: data, offset: 0))
      RETURN (sqrt_price / 2^64) ** 2

    ;; Use helpers
    (define pool_address derivePoolAddress(token_pair))
    (define pool_account getAccountInfo(pubkey: pool_address))
    (define price parsePoolPrice(pool_account.data))
    price
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
    (define block CALL getBlock(slot: slot))
    (define tx_count COUNT(block.transactions))
    (define fee_stats analyze_fees(block))
    RETURN {
      slot: slot,
      transaction_count: tx_count,
      fee_analysis: $fee_stats
    }
```

---

## Parallel Execution

### PARALLEL Block

Execute multiple operations concurrently:

```
PARALLEL {
  (define account1 getAccountInfo(addr1))
  (define account2 getAccountInfo(addr2))
  (define tx getTransaction(sig))
}
```

### Wait Strategies

**WAIT_ALL:**
Wait for all parallel operations to complete.

```
PARALLEL {
  (define price1 fetchPrice("DEX1"))
  (define price2 fetchPrice("DEX2"))
  (define price3 fetchPrice("DEX3"))
}
WAIT_ALL
(define best_price MAX([price1, price2, price3]))
```

**WAIT_ANY:**
Continue when any operation completes.

```
PARALLEL {
  (define result1 queryNode1())
  (define result2 queryNode2())
}
WAIT_ANY → $first_result
;; Use whichever completed first
```

**RACE:**
Take first result, cancel others.

```
RACE {
  (define data slowButAccurate())
  (define estimate fastButApproximate())
}
TAKE_FIRST → $result
```

### Timeout Guards

```
TIMEOUT 30s:
  (define result longRunningOperation())
CATCH TIMEOUT:
  (define result fallbackValue())
```

---

## Error Handling

### TRY-CATCH

```
TRY:
  (define result riskyOperation())
CATCH:
  [error handling]
```

### Error Classification

```
TRY:
  (define data fetchData())
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
  (define price fetchFromOracle("primary"))
CATCH RECOVERABLE:
  TRY:
    (define price fetchFromOracle("secondary"))
  CATCH RECOVERABLE:
    (define price calculateFromDEX())
```

### Retry Logic

```
RETRY 3x ON_FAIL:
  (define result unreliableOperation())
WITH_BACKOFF: [1s, 2s, 4s]
```

### Circuit Breaker

```
CIRCUIT_BREAK AFTER 10 failures FOR 60s:
  (define result externalService())
```

---

## Data Transformation

### Map, Filter, Reduce

**MAP:**
```
(define fees MAP(transactions, tx => tx.meta.fee))
(define squares MAP(numbers, n => n * n))
```

**FILTER:**
```
(define successful FILTER(transactions, tx => tx.meta.err == null))
(define large FILTER(amounts, amt => amt > 1000))
```

**REDUCE:**
```
(define sum REDUCE(numbers, (acc, n) => acc + n, 0))
(define total_fees REDUCE(transactions, (sum, tx) => sum + tx.fee, 0))
```

### Aggregation Functions

```
SUM(collection)
AVG(collection)
MIN(collection)
MAX(collection)
COUNT(collection)
MEDIAN(collection)
```

**Examples:**
```
(define total_fees SUM(MAP(transactions, tx => tx.fee)))
(define avg_compute AVG(MAP(transactions, tx => tx.computeUnits)))
(define highest_fee MAX(fees))
```

### Statistical Functions

```
MEAN(data)
MEDIAN(data)
MODE(data)
STDDEV(data)
PERCENTILE(data, 95)
```

### Array Operations

```
(define first FIRST(array))
(define last LAST(array))
(define slice SLICE(array, start: 0, end: 10))
(define reversed REVERSE(array))
(define sorted SORT(array))
(define unique UNIQUE(array))
```

### String Operations

```
(define upper UPPERCASE(string))
(define lower LOWERCASE(string))
(define trimmed TRIM(string))
(define parts SPLIT(string, delimiter: ","))
(define joined JOIN(array, separator: ", "))
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
    ;; Fetch all data exactly
    (define data fetchAll())
  BRANCH B (100 <= count < 10000):
    ;; Paginate through results
    (define data paginate(count))
  BRANCH C (count >= 10000):
    ;; Statistical sampling
    (define data sample(count, sample_size: 1000))
```

### Nested Decisions

```
DECISION: Check transaction status
  BRANCH A (success):
    (define result extractSuccessData(tx))

    DECISION: Check if CPI involved
      BRANCH A1 (has_cpi):
        (define calls parseInnerInstructions(tx))
      BRANCH A2 (no_cpi):
        (define calls [tx.instruction])

  BRANCH B (failed):
    (define error parseError(tx))
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
MATCH type:
  "transfer" → handleTransfer()
  "mint" → handleMint()
  "burn" → handleBurn()
  _ → handleUnknown()  // default case
```

### Guard Clauses

```
GUARD data != null ELSE RETURN ERROR(message: "No data")
GUARD balance >= amount ELSE RETURN ERROR(message: "Insufficient balance")
```

---

## Comments and Metadata

### Inline Comments

```
;; Single line comment
(define result calculate()  // End of line comment)

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
(define analysis analyzeTransaction(signature))
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
;; RPC tools (lowercase) - matches Solana SDK
(define slot getSlot())
(define block getBlock(slot: slot))
(define account getAccountInfo(pubkey: address))

;; Data processing (UPPERCASE) - stands out
(define fees MAP(transactions, tx => tx.meta.fee))
(define high_fees FILTER(fees, fee => fee > 1000))
(define total SUM(fees))

;; Custom tools (snake_case) - user-defined
(define analysis analyze_fees(block: block))
(define pool_price query_oracle_pool(token_pair: pair))

;; Statistical tools (UPPERCASE) - mathematical operations
(define average MEAN(data))
(define correlation CORRELATE(x: x_data, y: y_data))

;; Utility tools (UPPERCASE) - system operations
(define timestamp NOW())
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
(do
  (define current_slot (getSlot))
  (define blocks [])

  (for (i (range 0 10))
    (do
      (define block (getBlock :slot (- current_slot i)))
      (set! blocks (append blocks block))))

  (define all_transactions (flatten (map blocks (lambda (b) (. b transactions)))))
  (define fees (map all_transactions (lambda (tx) (. (. tx meta) fee))))

  **Statistics:**
  (define mean_fee (MEAN :data fees))
  (define median_fee (MEDIAN :data fees))
  (define stddev (STDDEV :data fees))

  DECISION: Check distribution
    BRANCH A (< (/ stddev mean_fee) 0.5):
      ;; Normal distribution, use mean
      (define result mean_fee)
    BRANCH B (>= (/ stddev mean_fee) 0.5):
      ;; High variance, use median
      (define result median_fee)
      (define note "High variance detected, using median")

  **Action:**
  (return {:average_fee result
           :confidence 95
           :sample_size (COUNT :collection fees)
           :note (if (null? note) "Normal distribution" note)}))
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
    ;; Helper: Derive Orca pool address from token pair
    DEFINE deriveOrcaPoolAddress(pair):
      (define seeds [)
        "whirlpool",
        pair[0],  // Token A mint
        pair[1]   // Token B mint
      ]
      RETURN derivePDA(seeds: seeds, program: ORCA_PROGRAM_ID)

    ;; Helper: Parse price from Orca pool account data
    DEFINE parseOrcaPoolPrice(data):
      ;; Orca stores sqrt_price_x64 at offset 65
      (define sqrt_price_x64 parseU128(data: data, offset: 65))
      ;; Convert to decimal price
      (define price POW(base: sqrt_price_x64 / POW(base: 2, exponent: 64), exponent: 2))
      price

    (define pool_address deriveOrcaPoolAddress(token_pair))
    TRY:
      (define pool_account getAccountInfo(pubkey: pool_address))
      (define price parseOrcaPoolPrice(pool_account.data))
      price
    CATCH:
      RETURN null

DEFINE_TOOL queryRaydiumPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    ;; Helper: Derive Raydium AMM pool address
    DEFINE deriveRaydiumPoolAddress(pair):
      (define seeds [)
        "amm_associated_seed",
        pair[0],
        pair[1]
      ]
      RETURN derivePDA(seeds: seeds, program: RAYDIUM_AMM_PROGRAM)

    ;; Helper: Parse price from Raydium pool state
    DEFINE parseRaydiumPoolPrice(data):
      ;; Raydium stores reserves at specific offsets
      (define base_reserve parseU64(data: data, offset: 8))
      (define quote_reserve parseU64(data: data, offset: 16))
      (define price quote_reserve / base_reserve)
      price

    (define pool_address deriveRaydiumPoolAddress(token_pair))
    TRY:
      (define pool_account getAccountInfo(pubkey: pool_address))
      (define price parseRaydiumPoolPrice(pool_account.data))
      price
    CATCH:
      RETURN null

DEFINE_TOOL getJupiterQuote:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    ;; Helper: Parse Jupiter API response
    DEFINE parseJupiterQuote(json_response):
      (define parsed JSON_PARSE(json_response))
      (define in_amount parsed.inAmount)
      (define out_amount parsed.outAmount)
      (define price out_amount / in_amount)
      price

    TRY:
      (define url "https://quote-api.jup.ag/v6/quote?inputMint={token_pair[0]}&outputMint={token_pair[1]}&amount=1000000")
      (define response FETCH_URL(url: url))
      (define price parseJupiterQuote(response))
      price
    CATCH:
      RETURN null

**Main Branch:**
(define token_pair ("SOL", "USDC"))
(define timestamp NOW())

PARALLEL {
  TRY:
    (define orca_price queryOrcaPool(token_pair: token_pair))
  CATCH RECOVERABLE:
    (define orca_price null)

  TRY:
    (define raydium_price queryRaydiumPool(token_pair: token_pair))
  CATCH RECOVERABLE:
    (define raydium_price null)

  TRY:
    (define jupiter_quote getJupiterQuote(token_pair: token_pair))
  CATCH RECOVERABLE:
    (define jupiter_quote null)
}
WAIT_ALL

**Data Validation:**
(define valid_prices FILTER()
  [orca_price, raydium_price, jupiter_quote],
  p => p != null
)

GUARD COUNT(valid_prices) > 0 ELSE RETURN ERROR(message: "No prices available")

**Analysis:**
(define best_price MAX(valid_prices))
(define worst_price MIN(valid_prices))
(define avg_price AVG(valid_prices))
(define spread (best_price - worst_price) / avg_price * 100)

**Action:**
RETURN {
  prices: {
    orca: orca_price,
    raydium: raydium_price,
    jupiter: $jupiter_quote
  },
  analysis: {
    best: best_price,
    worst: worst_price,
    average: avg_price,
    spread_percent: $spread
  },
  recommendation: spread > 5 ? "Significant arbitrage opportunity" : "Prices aligned"
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
    (define swap_instruction FIND()
      collection: tx.message.instructions,
      predicate: inst => inst.programId == DEX_PROGRAM_ID
    )

    ;; Parse swap instruction data
    (define input_amount parseU64(data: swap_instruction.data, offset: 8))
    (define output_amount tx.meta.postTokenBalances[0].uiAmount)

    ;; Extract token mints from token balances
    (define token_a tx.meta.preTokenBalances[0].mint)
    (define token_b tx.meta.postTokenBalances[0].mint)

    RETURN {
      input_amount: input_amount,
      output_amount: output_amount,
      tokens: {from: token_a, to: token_b}
    }

DEFINE_TOOL analyzeTransfer:
  params: {tx: Transaction}
  returns: TransferAnalysis
  implementation:
    (define amount tx.meta.postBalances[1] - tx.meta.preBalances[1])
    RETURN {
      amount: amount,
      from: tx.message.accountKeys[0],
      to: tx.message.accountKeys[1]
    }

DEFINE_TOOL getOptimalRoute:
  params: {input_amount: u64, tokens: object}
  returns: Route | null
  implementation:
    ;; Helper: Find all DEX pools for token pair
    DEFINE findPoolsForPair(token_a, token_b):
      PARALLEL {
        (define orca_pool deriveOrcaPoolAddress((token_a, token_b)))
        (define raydium_pool deriveRaydiumPoolAddress((token_a, token_b)))
      }
      WAIT_ALL
      RETURN [orca_pool, raydium_pool]

    ;; Helper: Calculate routes through pools
    DEFINE calculateRoutes(pools, amount):
      (define routes [])
      for (pool pools)
        TRY:
          (define pool_account getAccountInfo(pubkey: pool))
          (define expected_output simulateSwap(pool_account, amount))
          (define route {)
            pool: pool,
            expected_output: $expected_output
          }
          (define routes APPEND(array: routes, item: route))
        CATCH:
          CONTINUE
      routes

    ;; Helper: Simulate swap through a pool
    DEFINE simulateSwap(pool_account, input_amount):
      ;; Simplified constant product formula x * y = k
      (define base_reserve parseU64(data: pool_account.data, offset: 8))
      (define quote_reserve parseU64(data: pool_account.data, offset: 16))
      (define k base_reserve * quote_reserve)
      (define new_base base_reserve + input_amount)
      (define new_quote k / new_base)
      (define output quote_reserve - new_quote)
      output

    (define pools findPoolsForPair(tokens.from, tokens.to))
    (define routes calculateRoutes(pools, input_amount))

    (if COUNT(collection: routes) == 0
      RETURN null

    (define best MAX_BY(routes, r => r.expected_output))
    best

**Main Branch:**
(define signature INPUT(prompt: "transaction_signature"))

TRY:
  (define tx getTransaction(signature: signature))
CATCH FATAL:
  RETURN ERROR(message: "Transaction not found")

**Transaction Analysis:**
(define instruction tx.message.instructions[0])
(define program_id instruction.programId)

DECISION: Identify transaction type
  BRANCH A (program_id == DEX_PROGRAM):
    (define type "swap")
    (define analysis analyzeSwap(tx: tx))

  BRANCH B (program_id == LENDING_PROGRAM):
    (define type "lending")
    (define analysis {type: "lending"} // Simplified)

  BRANCH C (program_id == SYSTEM_PROGRAM):
    (define type "transfer")
    (define analysis analyzeTransfer(tx: tx))

  BRANCH D (default):
    (define type "unknown")
    (define analysis {basic: true})

**Optimization Check:**
(if type == "swap"
  ;; Detailed swap analysis
  (define input analysis.input_amount)
  (define output analysis.output_amount)
  (define actual_price output / input)

  PARALLEL {
    TRY:
      (define optimal_route getOptimalRoute()
        input_amount: input,
        tokens: analysis.tokens
      )
    CATCH:
      (define optimal_route null)

    TRY:
      (define alternative_prices [] // Simplified for example)
    CATCH:
      (define alternative_prices [])
  }
  WAIT_ALL

  (if optimal_route != null
    (define optimal_price optimal_route.expected_output / input)
    (define efficiency (actual_price / optimal_price) * 100)
  ELSE
    (define efficiency null)

  (define verdict efficiency >= 98 ? "optimal" :)
             efficiency >= 95 ? "good" :
             efficiency >= 90 ? "acceptable" : "suboptimal"
ELSE
  (define efficiency null)
  (define verdict "not_applicable")

**Final Report:**
RETURN {
  transaction: signature,
  type: type,
  analysis: analysis,
  optimization: {
    efficiency_score: efficiency,
    verdict: verdict,
    could_have_saved: efficiency != null ?
      (optimal_route.expected_output - output) : null
  },
  confidence: efficiency != null ? 90 : 60
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
(define target_program INPUT("program_id"))
(define current_slot getSlot())
(define max_blocks 1000)
(define found false)

for (i 0..max_blocks)
  (define slot current_slot - i)

  TRY:
    (define block getBlock(slot))
  CATCH:
    CONTINUE  // Skip missing blocks

  (define transactions block.transactions)

  for (tx transactions)
    (define has_program ANY()
      tx.message.instructions,
      inst => inst.programId == $target_program
    )

    (if $has_program
      (define found_tx tx)
      (define found_slot slot)
      (define found true)
      BREAK  // Exit inner loop

  BREAK IF found  // Exit outer loop

**Result:**
(if $found
  RETURN {
    found: true,
    transaction: found_tx,
    slot: found_slot,
    blocks_searched: i + 1
  }
ELSE
  RETURN {
    found: false,
    blocks_searched: max_blocks,
    message: "No transaction found in last {max_blocks} blocks"
  }
```

---

## Best Practices

### 1. Use Clear Variable Names
```
;; Good
(define transaction_fee tx.meta.fee)
(define average_compute_units AVG(compute_values))

;; Bad
(define f tx.meta.fee)
(define avg AVG(vals))
```

### 2. Show Data Flow Explicitly
```
;; Good
(define slot getSlot() → current_slot)
(define block getBlock(current_slot) → block_data)
(define transactions EXTRACT(block_data.transactions))

;; Also acceptable
(define slot getSlot())
(define block getBlock(slot))
(define transactions block.transactions)
```

### 3. Use Metadata Tags
```
;; Good - shows expectations upfront
[TIME: ~30s] [COST: ~0.001 SOL] [CONFIDENCE: 95%]
(define result complexOperation())

;; Less informative
(define result complexOperation())
```

### 4. Handle Errors Explicitly
```
;; Good
TRY:
  (define data riskyOperation())
CATCH RECOVERABLE:
  (define data fallbackOperation())
CATCH FATAL:
  RETURN ERROR(message: "Operation failed")

;; Bad - vague
(define data riskyOperation() // handle errors somehow)
```

### 5. Break Complex Logic into Sections
```
;; Good
**Data Collection:**
[collection logic]

**Data Transformation:**
[transformation logic]

**Analysis:**
[analysis logic]

;; Bad - everything in one block
[all logic together]
```

### 6. Use Decision Points for Clarity
```
;; Good
DECISION: Determine sampling strategy
  BRANCH A (small dataset):
    [exact approach]
  BRANCH B (large dataset):
    [sampling approach]

;; Less clear
(if small [exact] ELSE [sampling]
```

### 7. Document Complex Decisions
```
;; Good
/**
 * We use median instead of mean when stddev is high
 * because outlier transactions can skew the average.
 * Threshold: stddev/mean > 0.5
 */
(define result stddev/mean > 0.5 ? MEDIAN(data) : MEAN(data))
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

;; Parallel agent spawning
PARALLEL_AGENTS {
  (define price_analysis SPAWN price_analyzer(token_pair))
  (define volume_analysis SPAWN volume_analyzer(token_pair))
  (define sentiment_analysis SPAWN sentiment_analyzer(token_pair))
}
MERGE_RESULTS(price_analysis, volume_analysis, sentiment_analysis)
```

**2. Research State Management**
```
;; Define research-specific tools
DEFINE_TOOL collect_fees:
  params: {time_range: Duration}
  returns: Array<f64>
  implementation:
    (define current_slot getSlot())
    (define blocks [])
    for (i 0..1000)
      (define block getBlock(slot: current_slot - i))
      (define blocks APPEND(array: blocks, item: block))
    (define all_txs FLATTEN(MAP(blocks, b => b.transactions)))
    (define fees MAP(all_txs, tx => tx.meta.fee))
    fees

DEFINE_TOOL collect_congestion:
  params: {time_range: Duration}
  returns: Array<f64>
  implementation:
    (define samples getRecentPerformanceSamples())
    (define tps_values MAP(samples, s => s.numTransactions / s.samplePeriodSecs))
    tps_values

RESEARCH_STATE:
  hypothesis: "High fees correlate with network congestion"
  evidence: []
  confidence: 0%

GATHER_EVIDENCE:
  (define fee_data collect_fees(time_range: "7 days"))
  (define congestion_data collect_congestion(time_range: "7 days"))
  (define correlation CORRELATE(x: fee_data, y: congestion_data))

  UPDATE_STATE:
    evidence = APPEND(array: evidence, item: correlation)
    confidence = calculateConfidence(evidence: evidence)

(if confidence > 95%
  CONCLUDE hypothesis AS "proven"
ELSE (if confidence > 70%
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

for (transaction dataset)
  EXTRACT_ENTITIES(transaction) → $entities

  for (entity entities)
    GRAPH.ADD_NODE(
      type: entity.type,
      id: entity.id,
      properties: entity.data
    )

  EXTRACT_RELATIONSHIPS(transaction) → $relationships

  for (rel relationships)
    GRAPH.ADD_EDGE(
      from: rel.source,
      to: rel.target,
      type: rel.relationship,
      weight: rel.strength
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
  (define us_validators FILTER(all_validators, v => v.location == "US"))
  (define other_validators FILTER(all_validators, v => v.location != "US"))

  (define us_skip_rates MAP(us_validators, v => v.skip_rate))
  (define other_skip_rates MAP(other_validators, v => v.skip_rate))

STATISTICAL_TEST:
  (define test T_TEST(us_skip_rates, other_skip_rates))
  (define p_value test.p_value)

  (if p_value < 0.05
    REJECT null_hypothesis
    CONFIDENCE: "statistically significant"
  ELSE
    FAIL_TO_REJECT null_hypothesis
    CONFIDENCE: "not statistically significant"
```

**5. Progressive Refinement**
```
;; Define analysis tools
DEFINE_TOOL analyze_fees_distribution:
  returns: Array<f64>
  implementation:
    (define current_slot getSlot())
    (define blocks [])
    for (i 0..100)
      (define block getBlock(slot: current_slot - i))
      (define blocks APPEND(array: blocks, item: block))
    (define txs FLATTEN(collection: MAP(blocks, b => b.transactions)))
    (define fees MAP(collection: txs, fn: tx => tx.meta.fee))
    fees

ITERATIVE_RESEARCH:
  initial_question: "What causes high transaction fees?"
  depth: 0
  max_depth: 5

RESEARCH_LOOP:
  depth += 1

  STEP_1: Gather broad data
    (define fee_distribution analyze_fees_distribution())

  STEP_2: Identify patterns
    (define outliers DETECT_OUTLIERS(data: fee_distribution))
    (define patterns FIND_PATTERNS(data: outliers))

  STEP_3: Generate sub-questions
    (define sub_questions [])
    for (pattern patterns)
      (define sub_questions APPEND(array: sub_questions, item: "Why does {pattern} occur?"))

  STEP_4: Research each sub-question
    for (question sub_questions)
      (if depth < max_depth
        RECURSIVE_RESEARCH(question, depth)

  STEP_5: Synthesize findings
    MERGE_INSIGHTS(all_findings) → $comprehensive_answer

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
  (define result1 QUERY(source: "RPC1", query: query))
  (define result2 QUERY(source: "RPC2", query: query))
  (define result3 QUERY(source: "Archive", query: query))
  (define result4 QUERY(source: "Indexer", query: query))
}
WAIT_ALL

CONSISTENCY_CHECK:
  (define agreements COMPARE_RESULTS([result1, result2, result3, result4]))

  (if agreements.consensus >= 75%
    (define validated_result agreements.consensus_value)
    (define confidence 100%)
  ELSE (if agreements.consensus >= 50%
    (define validated_result agreements.majority_value)
    (define confidence 75%)
    (define note "Some disagreement between sources")
  ELSE
    (define validated_result null)
    (define confidence 0%)
    INVESTIGATE_DISCREPANCY(agreements.differences)
```

**7. Contextual Memory & Learning**
```
MEMORY_CONTEXT:
  short_term: []   // Current session findings
  long_term: []    // Persistent knowledge
  working_set: []  // Active hypotheses

LEARN_FROM_QUERY:
  (define query_result executeQuery(query))

  EXTRACT_INSIGHTS:
    (define patterns identifyPatterns(query_result))
    (define anomalies detectAnomalies(query_result))
    (define correlations findCorrelations(query_result))

  STORE_KNOWLEDGE:
    for (pattern patterns)
      (if novelty(pattern: pattern, existing: MEMORY.long_term) > threshold
        MEMORY.long_term = APPEND(array: MEMORY.long_term, item: {
          pattern: pattern,
          confidence: calculateConfidence(evidence: [pattern]),
          first_seen: NOW(),
          occurrences: 1
        })
      ELSE
        UPDATE_EXISTING_PATTERN(pattern)

APPLY_LEARNED_KNOWLEDGE:
  (define relevant_knowledge QUERY_MEMORY()
    context: current_query,
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

for (finding research_findings)
  (define confidence WEIGHTED_AVERAGE([)
    factors.data_quality * 0.25,
    factors.sample_size * 0.20,
    factors.consistency * 0.25,
    factors.source_reliability * 0.15,
    factors.method_robustness * 0.15
  ])

  (define uncertainty 100 - confidence)

  ANNOTATE_RESULT:
    result: finding,
    confidence: "{confidence}%",
    uncertainty_range: calculateRange(uncertainty),
    caveats: identifyCaveats(finding)
```

**9. Automated Literature Review (Code & Documentation)**
```
LITERATURE_REVIEW:
  topic: "Solana consensus mechanism"
  sources: ["github", "docs", "forums", "papers"]

GATHER_SOURCES:
  PARALLEL {
    (define code SEARCH_GITHUB(query: "solana consensus", language: "rust"))
    (define docs SEARCH_DOCS(query: "proof of history"))
    (define discussions SEARCH_FORUMS(query: "tower bft"))
    (define papers SEARCH_ARXIV(query: "solana blockchain"))
  }
  WAIT_ALL

ANALYZE_SOURCES:
  for (source ALL_SOURCES)
    (define key_concepts EXTRACT_CONCEPTS(source))
    (define relationships MAP_RELATIONSHIPS(key_concepts))

    CLASSIFY:
      category: source.type,
      relevance: CALCULATE_RELEVANCE(source, topic),
      credibility: ASSESS_CREDIBILITY(source),
      recency: source.date

SYNTHESIZE_REVIEW:
  GROUP_BY_THEME(all_concepts) → $themes

  for (theme themes)
    (define summary SUMMARIZE_THEME(theme))
    (define consensus FIND_CONSENSUS(theme.sources))
    (define conflicts IDENTIFY_CONFLICTS(theme.sources))

    RETURN {
      theme: theme.name,
      summary: summary,
      consensus: consensus,
      conflicts: conflicts,
      key_sources: TOP_N(theme.sources, n: 5, by: "credibility")
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
  (define possible_plans ENUMERATE_PLANS(goal))

  for (plan possible_plans)
    ESTIMATE:
      time: plan.estimated_time
      cost: plan.estimated_cost
      confidence: plan.expected_confidence

    SCORE:
      (define score CALCULATE_SCORE()
        meets_constraints: CHECK_CONSTRAINTS(plan),
        efficiency: plan.cost / plan.confidence,
        reliability: plan.robustness
      )

SELECT_OPTIMAL_PLAN:
  (define best_plan MAX_BY(possible_plans, plan => plan.score))

EXECUTE_WITH_MONITORING:
  (define start_time NOW())
  (define actual_cost 0)

  for (step best_plan.steps)
    EXECUTE(step)

    ;; Monitor and adapt
    (if NOW() - start_time > constraints.max_time * 0.8
      SWITCH_TO_FASTER_PLAN()

    (if actual_cost > constraints.max_cost * 0.8
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
  (define data GATHER_TRANSACTIONS()
    time_range: "last 7 days",
    sample_size: 10000
  )

CONTROL_FOR_CONFOUNDERS:
  (define matched_data PROPENSITY_SCORE_MATCHING()
    data: data,
    treatment_var: "priority_fee",
    confounders: ["congestion", "time", "size"]
  )

ESTIMATE_EFFECT:
  (define treatment_group FILTER(matched_data, tx => tx.priority_fee > threshold))
  (define control_group FILTER(matched_data, tx => tx.priority_fee <= threshold))

  (define treatment_avg_time AVG(collection: MAP(treatment_group, tx => tx.confirmation_time)))
  (define control_avg_time AVG(collection: MAP(control_group, tx => tx.confirmation_time)))

  (define causal_effect control_avg_time - treatment_avg_time)

  STATISTICAL_SIGNIFICANCE:
    (define p_value CALCULATE_P_VALUE(treatment_group, control_group))

  RETURN {
    effect: causal_effect,
    significant: p_value < 0.05,
    conclusion: causal_effect > 0 ?
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
    (define chain_data ANALYZE_CHAIN(token_address))
    (define social_data ANALYZE_SOCIAL(token_name))
    (define code_data ANALYZE_CODE(program_id))
    (define price_data ANALYZE_PRICE(token_address))
  }
  WAIT_ALL

ALIGN_TIMELINES:
  (define unified_timeline ALIGN_BY_TIMESTAMP([)
    chain_data,
    social_data,
    code_data,
    $price_data
  ])

CROSS_MODAL_ANALYSIS:
  for (event unified_timeline)
    CORRELATE:
      - Chain activity spike → Social sentiment
      - Code deployment → Price movement
      - Social buzz → Chain activity

    DETECT_LEADING_INDICATORS:
      (define lag_analysis CALCULATE_TIME_LAGS(correlations))

    IDENTIFY_CAUSALITY:
      (define granger_test GRANGER_CAUSALITY_TEST(event.signals))

SYNTHESIZE_INSIGHTS:
  RETURN {
    relationships: correlations,
    leading_indicators: lag_analysis,
    causal_chains: granger_test,
    predictive_signals: EXTRACT_PREDICTIVE_SIGNALS(relationships)
  }
```

**13. Explanation Generation**
```
EXPLAINABLE_RESEARCH:
  finding: $research_result
  target_audience: "non-technical" | "technical" | "expert"

GENERATE_EXPLANATION:
  STEP_1: Identify key concepts
    (define concepts EXTRACT_KEY_CONCEPTS(finding))

  STEP_2: Build explanation tree
    (define tree CONSTRUCT_EXPLANATION_TREE()
      root: finding,
      depth: BASED_ON(target_audience),
      detail: BASED_ON(target_audience)
    )

  STEP_3: Generate natural language
    for (node tree)
      (if node.requires_explanation
        (define explanation EXPLAIN()
          concept: node.concept,
          use_analogies: target_audience == "non-technical",
          include_technical_details: target_audience == "expert"
        )
        node.text = $explanation

  STEP_4: Add supporting evidence
    for (claim finding.claims)
      (define evidence GATHER_EVIDENCE(claim))
      ANNOTATE_WITH_CITATIONS(claim, evidence)

  STEP_5: Provide confidence rationale
    EXPLAIN_CONFIDENCE:
      "We are {confidence}% confident because:"
      for (factor confidence_factors)
        "- {factor.name}: {factor.description}"

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
    for (watch watches)
      (define current_value MEASURE(watch.metric))

      (if THRESHOLD_EXCEEDED(current_value, watch.threshold)
        (define alert CREATE_ALERT()
          type: watch.metric,
          severity: calculateSeverity(current_value),
          context: GATHER_CONTEXT(watch)
        )

        TRIGGER_INVESTIGATION:
          SPAWN_AGENT investigator WITH:
            focus: alert.type
            context: alert.context
            goal: "Determine root cause"

          (define findings AWAIT investigator)

          (if findings.actionable
            RECOMMEND_ACTIONS(findings)
```

**15. Meta-Learning & Strategy Optimization**
```
META_LEARNING:
  history: []  // Past queries and their performance

TRACK_PERFORMANCE:
  for (completed_query history)
    RECORD:
      query_type: query.type
      strategy_used: query.strategy
      time_taken: query.duration
      cost: query.cost
      accuracy: query.accuracy
      user_satisfaction: query.rating

LEARN_OPTIMAL_STRATEGIES:
  for (query_type UNIQUE(collection: MAP(history, q => q.type)))
    (define queries_of_type FILTER(history, q => q.type == query_type))

    (define best_strategy MAX_BY(queries_of_type, q =>)
      SCORE(q.accuracy, q.duration, q.cost, q.user_satisfaction)
    )

    STORE_LEARNING:
      query_type: $query_type
      recommended_strategy: best_strategy.strategy
      confidence: CALCULATE_CONFIDENCE(queries_of_type.length)

APPLY_LEARNING:
  WHEN new_query ARRIVES:
    (define learned_strategy LOOKUP_STRATEGY(new_query.type))

    (if learned_strategy.confidence > 80%
      USE_STRATEGY(learned_strategy)
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
DEFINE_FUNCTION analyze_fees(transactions):
  (define fees MAP(transactions, tx => tx.fee))
  RETURN {
    mean: MEAN(fees),
    median: MEDIAN(fees),
    p95: PERCENTILE(fees, 95)
  }

IMPORT common_functions FROM "stdlib"
USE analyze_fees, calculate_confidence
```

**18. Assertions and Contracts**
```
ASSERT balance > 0, "Balance must be positive"
REQUIRE data.length >= 100, "Insufficient sample size"
ENSURE result.confidence >= 80%, "Result not confident enough"

INVARIANT DURING analysis:
  state.consistency == true
```

**19. Performance Profiling**
```
PROFILE:
  (define result expensive_operation())
WITH_METRICS:
  time: $elapsed_time
  memory: $memory_used
  api_calls: $total_calls
  cost: $total_cost
```

**20. Debugging Directives**
```
BREAKPOINT WHEN count > 1000
LOG intermediate_result AT checkpoint
TRACE data_flow FROM source TO destination
WATCH variable FOR changes
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
