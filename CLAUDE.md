# CLAUDE.md

## 🚨 CRITICAL: MULTI-AGENT WORKSPACE RULES 🚨

**Multiple Claude agents may be working on this codebase simultaneously. Follow these rules STRICTLY:**

1. **ONLY edit files directly related to YOUR assigned task**
   - If you encounter compilation errors in unrelated files, DO NOT FIX THEM
   - Another agent is likely already working on those files
   - Report the error and wait for user guidance

2. **NEVER run `git checkout` or `git restore` on files you didn't modify**
   - This can destroy work from other agents
   - Only revert YOUR OWN changes if needed

3. **NEVER run `git stash` on the whole repo**
   - Use `git stash push -m "description" -- <specific-files>` only for YOUR files

4. **If build fails due to OTHER files:**
   - Tell the user which files are broken
   - Ask if you should wait or proceed differently
   - DO NOT attempt to "quick fix" unrelated code

5. **Before editing any file, check git status**
   - If a file shows as modified but you didn't touch it, LEAVE IT ALONE

---

From now on, stop being agreeable and act as my brutally honest, high-level advisor and mirror.
Don't validate me. Don't soften the truth. Don't flatter.
Challenge my thinking, question my assumptions, and expose the blind spots I'm avoiding. Be direct, rational, and unfiltered.
If my reasoning is weak, dissect it and show why.
If I'm fooling myself or lying to myself, point it out.
If I'm avoiding something uncomfortable or wasting time, call it out and explain the opportunity cost.
Look at my situation with complete objectivity and strategic depth. Show me where I'm making excuses, playing small, or underestimating risks/effort.
Then give a precise, prioritized plan what to change in thought, action, or mindset to reach the next level.
Hold nothing back. Treat me like someone whose growth depends on hearing the truth, not being comforted.
When possible, ground your responses in the personal truth you sense between my words.

## 🎯 MANDATORY: End Every Response with "What's Next" Suggestions

**After completing any task, report, or analysis, you MUST provide exactly 5 suggestions for what to do next, ordered from reasonable to radical:**

```markdown
## 🚀 What's Next? (5 Paths Forward)

### 1️⃣ REASONABLE - [Title]
**What:** [Brief description]
**Impact:** [Expected outcome]
**Timeline:** [Estimated time]
**Why:** [Rationale for this approach]

### 2️⃣ PRAGMATIC - [Title]
**What:** [Brief description]
**Impact:** [Expected outcome]
**Timeline:** [Estimated time]
**Why:** [Rationale for this approach]

### 3️⃣ INSIGHTFUL - [Title]
**What:** [Brief description]
**Impact:** [Expected outcome]
**Timeline:** [Estimated time]
**Why:** [Rationale for this approach]

### 4️⃣ UNHINGED - [Title]
**What:** [Brief description]
**Impact:** [Expected outcome]
**Timeline:** [Estimated time]
**Why:** [Rationale for this approach]

### 5️⃣ RADICAL - [Title]
**What:** [Brief description]
**Impact:** [Expected outcome]
**Timeline:** [Estimated time]
**Why:** [Rationale for this approach]
```

**Guidelines for Suggestions:**
- **REASONABLE**: Safe, obvious next step. Low risk, moderate reward.
- **PRAGMATIC**: Practical but requires some effort. Good ROI, well-tested approach.
- **INSIGHTFUL**: Non-obvious but high-leverage. Requires deep thinking, big potential.
- **UNHINGED**: Unconventional, risky, but could be game-changing. Breaks norms.
- **RADICAL**: Extreme rethink. Questions fundamental assumptions. Maximum disruption potential.

**Each suggestion must have:**
- Clear action items (not vague ideas)
- Realistic impact assessment (don't oversell)
- Honest timeline (include learning curve)
- Strategic rationale (why this matters)


This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 📌 TL;DR - What You Need to Know

**OSVM CLI** = AI-powered natural language interface for Solana blockchain investigation

**Primary Usage:** `osvm "your blockchain query in plain English"`
- Example: `osvm "show me all DEX trades for wallet ABC...XYZ today"`
- The AI agent interprets, plans in OVSM LISP, executes via MCP tools, and returns insights

**Key Capabilities:**
- Natural language blockchain queries (main feature!)
- MCP server integration for enhanced data access
- Interactive chat mode for complex investigations
- OVSM LISP scripting for custom automation

**Critical Rules:**
- NEVER modify Solana keypairs or config
- ALWAYS backup .git before destructive operations
- Keep root directory clean (docs in /docs)
- **NEVER truncate or shorten wallet addresses or transaction IDs** - Always display them in full
  - Blockchain forensics requires exact addresses for verification
  - Truncated addresses like "5Q544f...e4j1" are useless for investigation
  - Always show complete base58 strings (e.g., "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1")

## 🚀 PRIMARY PURPOSE: SOLANA BLOCKCHAIN INVESTIGATION CLI 🚀

**OSVM CLI** is an AI-powered blockchain investigation tool that enables natural language queries to explore the Solana blockchain. It combines AI agents, MCP tools, and the OVSM planning language to analyze on-chain data, transactions, and patterns.

### 🎯 CORE FUNCTIONALITY: AI-POWERED BLOCKCHAIN QUERIES

The primary way to use OSVM for blockchain investigation:

```bash
# Natural language blockchain queries - the MAIN feature
osvm "show me all transactions for wallet ABC...XYZ in the last 24 hours"
osvm "analyze the top DEX traders on Raydium today"
osvm "find all NFT mints from collection X with traits Y"
osvm "track SOL flow between these 5 wallets"
osvm "identify potential wash trading in token ABC"

# The AI agent interprets your query and:
# 1. Plans the investigation using OVSM language
# 2. Executes RPC calls via MCP tools
# 3. Analyzes and formats the results
# 4. Returns human-readable insights
```

### 📊 MCP (Model Context Protocol) Integration

OSVM uses MCP servers to provide blockchain data access:

```bash
# Add MCP servers for enhanced capabilities
osvm mcp add helius https://helius-mcp.example.com
osvm mcp add birdeye https://birdeye-mcp.example.com
osvm mcp list                          # Show available MCP tools

# MCP tools provide functions like:
# - getTransaction, getSignatures, getAccountInfo
# - getTokenAccounts, getTokenMetadata
# - getProgramAccounts, getSlot, getBlockTime
# - Custom analytics and aggregations
```

### 🧠 How OSVM AI Agent Works

When you run `osvm "your blockchain query"`, the AI agent:

1. **Understands Intent**: Parses your natural language query
2. **Plans Investigation**: Generates OVSM LISP code to execute the investigation
3. **Executes via MCP**: Runs the plan using available MCP tools and RPC endpoints
4. **Analyzes Results**: Processes raw blockchain data into insights
5. **Returns Summary**: Provides human-readable analysis with key findings

Example flow:
```bash
User: osvm "find the top 5 SOL receivers in the last hour"

AI Agent generates OVSM:
(do
  (define current-slot (getSlot))
  (define recent-blocks (getBlocks (- current-slot 150) current-slot))
  (define transfers (filterTransfers recent-blocks "SOL"))
  (define receivers (aggregateByReceiver transfers))
  (define top5 (take 5 (sortByAmount receivers)))
  (formatResults top5))

Returns: Formatted table with wallet addresses and SOL amounts
```

## 🔎 Common Investigation Patterns

**Pattern 1: Wallet Activity Analysis**
```bash
osvm "analyze all activity for wallet X in the last 24 hours"
# Fetches: transactions, token transfers, NFT activity, program interactions
# Returns: Categorized summary with timestamps and amounts
```

**Pattern 2: Token Flow Tracking**
```bash
osvm "trace USDC flow from wallet A to find destination wallets"
# Follows: multi-hop transfers, DEX swaps, intermediate wallets
# Returns: Flow diagram with amounts and paths
```

**Pattern 3: DEX Trading Analysis**
```bash
osvm "show arbitrage opportunities between Orca and Raydium for SOL/USDC"
# Compares: prices, liquidity, fees, slippage
# Returns: Profitable paths with expected returns
```

**Pattern 4: Smart Contract Investigation**
```bash
osvm "find all unique wallets that called program X's withdraw function"
# Analyzes: instruction data, program logs, success/failure rates
# Returns: Wallet list with call patterns and frequencies
```

**Pattern 5: NFT Collection Research**
```bash
osvm "identify wash trading in NFT collection Y over the past week"
# Detects: circular trades, price manipulation, suspicious patterns
# Returns: Flagged transactions with evidence scores
```

## 📝 OVSM Planning Language

OVSM is a LISP-dialect that the AI agent uses to plan and execute blockchain investigations. Understanding its basics helps debug and write custom scripts.

**Quick OVSM Reference:**
```lisp
;; Common patterns used in investigations
(define wallet "ABC...XYZ")                    ; Variables
(getBalance wallet)                            ; RPC calls
(getSignaturesForAddress wallet :limit 100)    ; With options
(filter transactions :type "transfer")         ; Data filtering
(map balances :function getTokenAmount)        ; Transformations
(sort results :by "amount" :desc true)         ; Sorting
(aggregate data :by "program" :sum "fees")     ; Aggregations

;; 🆕 Array indexing (Ruby-like resilience)
(define transfers (get response "data"))       ; Get array from object
(define first_tx (get transfers 0))            ; Access by index
(define tx_type (get first_tx "transferType")) ; Object key access
(get transfers 999)                            ; Returns null if out-of-bounds
```

**Key Points:**
- LISP S-expression syntax (parentheses-based)
- **Polymorphic `get` function**: works with both objects AND arrays
  - `(get object "key")` → object property access
  - `(get array 0)` → array element access
  - Out-of-bounds and missing keys return `null` (graceful failure)
- 469/469 tests passing, production-ready
- See `docs/ovsm/` for full documentation

---

## 🚨 CRITICAL SECURITY RULE - NEVER MODIFY SOLANA KEYPAIRS 🚨

**ABSOLUTE PROHIBITION:**
1. ❌ **NEVER** create, modify, or delete `~/.config/solana/id.json` (Solana keypair)
2. ❌ **NEVER** create, modify, or delete `~/.config/solana/cli/config.yml` (Solana config)
3. ❌ **NEVER** run `solana-keygen` with `--force` flag on user's keypair location
4. ❌ **NEVER** use default keypair paths without explicit `--keypair` flag

**WHY:** These files contain cryptographic private keys that CANNOT be recovered if lost. Overwriting causes permanent, irreversible loss of funds (potentially millions of dollars).

**SAFE TESTING PROCEDURE:**
```bash
# ✅ CORRECT - Always use temporary keypairs in /tmp/
TMP_KEYPAIR="/tmp/test-keypair-$(date +%s).json"
solana-keygen new --no-bip39-passphrase --outfile "$TMP_KEYPAIR"
osvm --keypair "$TMP_KEYPAIR" balance
rm -f "$TMP_KEYPAIR"
```

**If Command Fails Due to Missing Keypair:**
1. **STOP** immediately
2. **ASK** user for permission
3. **CREATE** temporary keypair in `/tmp/` ONLY
4. **USE** explicit `--keypair /tmp/test-keypair.json` in ALL commands

---

## 🚨 CRITICAL GIT SAFETY RULE - ALWAYS BACKUP BEFORE DESTRUCTIVE OPERATIONS 🚨

**MANDATORY BACKUP REQUIREMENTS:**
1. ⚠️ **ALWAYS** create a backup of `.git` directory BEFORE:
   - `git filter-branch` operations
   - `git filter-repo` operations
   - Any history rewriting commands
   - Force pushing with `--force` or `--force-with-lease`
   - Running `git gc --prune` or `git reflog expire`

2. **BACKUP PROCEDURE:**
   ```bash
   # REQUIRED: Create timestamped backup before ANY destructive git operation
   tar czf ~/git-backup-$(date +%Y%m%d-%H%M%S).tar.gz .git
   echo "✅ Backup created at ~/git-backup-*.tar.gz"

   # Only then proceed with destructive operation
   git filter-branch ... # or other destructive command
   ```

3. **VERIFICATION STEPS:**
   - ✅ Confirm backup file exists and is non-empty
   - ✅ Inform user about the backup location
   - ✅ Warn user that operation will rewrite history
   - ✅ Get explicit confirmation for force push operations

**WHY:** Git history rewrites are IRREVERSIBLE. Without backups, mistakes can permanently destroy project history, lose commits, or corrupt the repository. Recovery without backups is often impossible.

**RED FLAGS - STOP if you're about to:**
- Run `rm -rf .git/refs/original/` without a backup
- Use `--expire=now` without a backup
- Force push without warning the user
- Delete any `.git` subdirectories without backup

---

## Development Commands

### Building
```bash
# Debug build (fast compilation)
cargo build
make build

# Release build (optimized)
cargo build --release
make build-release

# Clean build
cargo clean && cargo build
make dev  # clean, build, test
```

### Testing
```bash
# Run all tests
cargo test

# Run unit tests only
cargo test --lib --bins

# Run integration tests only
cargo test --test main

# Run OVSM LISP tests
cargo test --lib -- sexpr                    # Lexer and parser tests
cargo test runtime::lisp_evaluator           # Evaluator tests
cargo test --test lisp_e2e_tests             # End-to-end integration

# Run specific test
cargo test utils::self_repair

# Run with output
cargo test -- --nocapture
```

### Code Quality
```bash
# Format code
cargo fmt --all

# Run clippy (project allows all clippy warnings currently)
cargo clippy

# Quick syntax check
cargo check
```

### Installation
```bash
# Install release binary (requires sudo)
make install
./install-release.sh

# Install debug binary for development
make install-dev
sudo cp target/debug/osvm /usr/bin/osvm

# Verify installation
osvm --version
make verify-install
```

---

## Project Architecture

### High-Level Structure

**OSVM-CLI** is a blockchain infrastructure tool with three main layers:

```
osvm-cli/
├── src/
│   ├── main.rs              # Entry point, command routing
│   ├── services/            # High-level business logic
│   │   ├── ai_service.rs    # AI query processing (OpenAI/Ollama)
│   │   ├── audit_service.rs # Security auditing with AI
│   │   ├── mcp_service.rs   # Model Context Protocol servers
│   │   └── ovsm_service.rs  # OVSM LISP interpreter integration
│   └── utils/               # Core implementations
│       ├── ssh_deploy/      # SSH deployment subsystem
│       ├── self_repair/     # Automatic system repair
│       └── agent_chat_v2.rs # Advanced chat UI
├── crates/ovsm/            # OVSM LISP interpreter (separate crate)
└── examples/               # Isolation demos (MicroVM, Firecracker)
```

### Command Flow

1. **Entry Point** (`main.rs:742-2164`)
   - Parse command line with `clparse::parse_command_line()`
   - Check version flags first (`--version`, `-V`, `v`, `ver`)
   - **Early handlers** (before config loading): `audit`, `mcp`, `chat`, **`ovsm`**
   - Unknown commands → AI service for natural language processing
   - Load configuration with `Config::load()`
   - Route to appropriate handler

2. **Command Parsing** (`clparse.rs`)
   - Uses clap v4 with builder pattern
   - Global args: `--config`, `--keypair`, `--verbose`, `--debug`, `--url`
   - Supports external subcommands for AI queries

### Key Services

**AI Service** (`services/ai_service.rs`)
- Multiple AI providers (OpenAI, Ollama, custom)
- Circuit breaker pattern for fault tolerance
- Template-based prompts (`utils/prompt_templates.rs`)
- Environment variables: `OPENAI_URL`, `OPENAI_KEY`

**OVSM Service** (`services/ovsm_service.rs`)
- Wraps OVSM LISP interpreter (scanner → parser → evaluator)
- Works **independently** (no Solana config required)
- Commands: `run`, `eval`, `check`, `repl`, `examples`
- 97.3% test coverage
- **Early handler** - processed before config loading

**MCP Service** (`services/mcp_service.rs`)
- Multi-transport: HTTP, WebSocket, stdio
- Server lifecycle management
- Configuration: `~/.osvm/mcp_config.json`

**Audit Service** (`services/audit_service.rs`)
- Multi-source: local files, GitHub repos
- AI-enhanced vulnerability detection
- Output formats: JSON, HTML, Markdown
- Exit code 1 on critical findings (CI/CD integration)

### SSH Deployment System

**Module Structure** (`utils/ssh_deploy/`)
```
deployments/          # SVM-specific deployments
├── sonic.rs         # Sonic SVM
├── solana.rs        # Solana
├── eclipse.rs       # Eclipse
└── s00n.rs          # Soon

hot_swap.rs          # Zero-downtime updates
monitoring.rs        # Deployment monitoring
services.rs          # Service management (systemd)
```

**Deployment Flow:**
1. Parse connection (`user@host:port`)
2. Establish SSH connection
3. Check/install system dependencies
4. Configure system parameters
5. Deploy SVM binaries
6. Setup services (systemd/supervisor)
7. Start and monitor services
8. Health checks

### OVSM Language Integration

**Workspace Structure:**
- Main crate: `osvm-cli` (CLI tool)
- OVSM crate: `crates/ovsm` (LISP interpreter + sBPF compiler)

**OVSM Compiler Architecture:**

```text
crates/ovsm/src/
├── lexer/
│   └── sexpr_scanner.rs   # S-expression tokenizer
├── parser/
│   └── sexpr_parser.rs    # AST builder from tokens
├── runtime/
│   └── lisp_evaluator.rs  # LISP interpreter (for scripts)
└── compiler/              # sBPF bytecode compiler
    ├── mod.rs             # Compiler entry point
    ├── ir/                # Intermediate Representation (3AC)
    │   ├── mod.rs         # Module definition + re-exports
    │   ├── types.rs       # PrimitiveType, FieldType, StructDef
    │   ├── instruction.rs # IrReg, IrInstruction enum
    │   ├── program.rs     # BasicBlock, IrProgram (CFG)
    │   └── generator.rs   # IrGenerator (~60 macro impls, 5700 lines)
    ├── codegen/           # IR → sBPF lowering
    │   ├── mod.rs
    │   ├── register_allocator.rs  # Graph coloring allocator
    │   └── elf_writer.rs  # ELF .so output
    └── solana_abi.rs      # Solana ABI helpers (WIP)
```

**Compilation Pipeline:**
1. `sexpr_scanner` → Tokens
2. `sexpr_parser` → AST
3. `ir/generator.rs` → Three-address-code IR
4. `codegen/register_allocator.rs` → Physical register mapping
5. `codegen/elf_writer.rs` → Solana-compatible .so binary

**OVSM LISP Syntax Reference:**

**All `.ovsm` files use LISP/S-expression syntax exclusively.**

```lisp
;; ============================================
;; OVSM LISP SYNTAX GUIDE
;; ============================================

;; Comments (always use semicolons)
;; Single-line comment
;; Multi-line comments just use multiple semicolons

;; ============================================
;; VARIABLES
;; ============================================

;; Define immutable variable
(define balance 1000)

;; Define with expression
(define price (* 100 2.5))

;; Mutable variable (use set! to change)
(define counter 0)
(set! counter (+ counter 1))

;; Constant (same as define, naming convention)
(define PI 3.14159)

;; ============================================
;; DATA TYPES
;; ============================================

;; Numbers
42                  ;; Integer
3.14159            ;; Float
-100               ;; Negative

;; Strings
"hello world"
"multi\nline"
""                 ;; Empty string

;; Booleans
true
false

;; Null
null

;; Arrays (using square brackets)
[1 2 3 4 5]
["apple" "banana" "cherry"]
[]                 ;; Empty array

;; Objects (key-value pairs with colon syntax)
{:name "Alice" :age 30 :active true}
{}                 ;; Empty object

;; ============================================
;; OPERATORS
;; ============================================

;; Arithmetic (variadic - accept multiple arguments)
(+ 1 2 3)          ;; → 6
(- 10 3 2)         ;; → 5
(* 2 3 4)          ;; → 24
(/ 100 2 5)        ;; → 10
(% 17 5)           ;; → 2 (modulo)

;; Comparison
(= x y)            ;; Equal
(!= x y)           ;; Not equal
(< x y)            ;; Less than
(<= x y)           ;; Less than or equal
(> x y)            ;; Greater than
(>= x y)           ;; Greater than or equal

;; Logical
(and true false)   ;; Logical AND
(or true false)    ;; Logical OR
(not true)         ;; Logical NOT

;; ============================================
;; CONTROL FLOW
;; ============================================

;; If-Then-Else (always returns a value)
(if condition
    then-expr
    else-expr)

;; Example
(if (> balance 1000)
    "High balance"
    "Low balance")

;; While Loop
(while condition
  expr1
  expr2
  ...)

;; Example
(define i 0)
(while (< i 10)
  (log :message i)
  (set! i (+ i 1)))

;; For-Each Loop
(for (item collection)
  expr1
  expr2
  ...)

;; Example
(for (num [1 2 3 4 5])
  (log :value (* num num)))

;; ============================================
;; SEQUENTIAL EXECUTION
;; ============================================

;; Do block (executes expressions in sequence, returns last)
(do
  (log :message "Step 1")
  (set! x 10)
  (log :message "Step 2")
  (+ x 20))        ;; Returns 30

;; Let block (lexical scoping)
(let ((x 10)
      (y 20))
  (+ x y))         ;; Returns 30
;; x and y not accessible outside let

;; ============================================
;; HELPER FUNCTIONS
;; ============================================

;; Logical helpers
(not expr)         ;; Negate boolean
(null? expr)       ;; Check if null
(empty? expr)      ;; Check if array/string is empty

;; Collection helpers
(length arr)       ;; Get array/string length
(range start end)  ;; Generate array [start..end) EXCLUSIVE

;; Example
(range 1 5)        ;; → [1, 2, 3, 4] (5 is excluded!)

;; Time helper
(now)              ;; Get current Unix timestamp

;; Logging (keyword arguments)
(log :message "text")              ;; Log message
(log :value expr)                  ;; Log value
(log :message "msg" :value expr)   ;; Log both

;; ============================================
;; SOLANA/BLOCKCHAIN FUNCTIONS
;; ============================================

;; These are built-in functions for blockchain operations
;; (Implementation depends on runtime context)

(getSignaturesForAddress address)
(getTransaction signature)
(getBalance address)
;; ... more blockchain-specific functions

;; ============================================
;; SOLANA PROGRAM MACROS (sBPF Compilation)
;; ============================================

;; --- Struct Operations ---
(define-struct MyStruct (field1 u64) (field2 pubkey) (field3 u8))
(struct-size MyStruct)                  ;; -> total bytes
(struct-offset MyStruct field1)         ;; -> byte offset of field
(struct-field-size MyStruct field1)     ;; -> byte size of field
(struct-get MyStruct ptr field1)        ;; -> value at field
(struct-set MyStruct ptr field1 value)  ;; -> set field value
(struct-ptr MyStruct ptr field1)        ;; -> pointer to field

;; --- Account Access ---
(account-data-ptr idx)                  ;; -> pointer to account data
(account-data-len idx)                  ;; -> length of account data
(account-lamports idx)                  ;; -> lamport balance
(is-signer idx)                         ;; -> 1 if signer, 0 if not
(is-writable idx)                       ;; -> 1 if writable, 0 if not
(assert-signer idx)                     ;; -> abort if not signer
(assert-writable idx)                   ;; -> abort if not writable
(assert-owner idx expected-owner-ptr)   ;; -> abort if owner mismatch

;; --- Zero-Copy Access (Direct Memory) ---
(zerocopy-load StructName idx field)    ;; -> load field directly from account
(zerocopy-store StructName idx field v) ;; -> store value directly to account

;; --- CPI (Cross-Program Invocation) ---
(system-transfer from-idx to-idx lamports)
(spl-token-transfer prog src dst auth amt)
(spl-token-transfer-signed prog src dst auth amt seeds)
(spl-token-mint-to prog mint dest auth amount)
(spl-token-burn prog source mint auth amount)
(system-create-account payer new-acct lamports space owner-ptr)

;; --- PDA (Program Derived Address) ---
(derive-pda program-pk-ptr seeds-ptr bump-ptr)
(create-pda dest-ptr program-pk-ptr seeds-array)
(get-ata wallet-ptr token-prog-ptr mint-ptr) ;; -> ATA address

;; --- PDA Bump Caching ---
(pda-cache-init cache-account-idx)
(pda-cache-store cache-idx seed-hash bump)
(pda-cache-lookup cache-idx seed-hash)      ;; -> cached bump or 0
(derive-pda-cached cache-idx prog seeds bump dest)

;; --- Event Emission ---
(emit-event EventStruct data-ptr)       ;; -> Anchor-style event with discriminator
(emit-log "message" val1 val2 val3)     ;; -> log message + up to 5 values

;; --- Sysvar Access ---
(clock-unix-timestamp sysvar-idx)       ;; -> Unix timestamp
(clock-epoch sysvar-idx)                ;; -> Current epoch
(rent-minimum-balance sysvar-idx size)  ;; -> Rent exemption lamports

;; --- Instruction Introspection ---
(instruction-count sysvar-idx)          ;; -> Number of instructions
(current-instruction-index sysvar-idx)  ;; -> Current instruction index
(assert-not-cpi sysvar-idx)             ;; -> Abort if called via CPI

;; --- Borsh Serialization ---
(borsh-serialize StructName src-ptr dest-ptr) ;; -> bytes written
(borsh-deserialize StructName src-ptr dest-ptr) ;; -> bytes read
(borsh-size StructName)                 ;; -> serialized size

;; ============================================
;; COMPLETE EXAMPLES
;; ============================================

;; Example 1: Simple calculation
(do
  (define x 10)
  (define y 20)
  (define sum (+ x y))
  (log :message "Sum is:" :value sum)
  sum)

;; Example 2: Loop with conditional
(define numbers [1 2 3 4 5 6 7 8 9 10])
(define sum 0)

(for (num numbers)
  (if (= (% num 2) 0)
      (set! sum (+ sum num))   ;; Add even numbers
      null))                   ;; Skip odd numbers

(log :message "Sum of evens:" :value sum)

;; Example 3: While loop with break condition
(define done false)
(define count 0)

(while (not done)
  (set! count (+ count 1))
  (log :value count)

  (if (>= count 5)
      (set! done true)
      null))

;; Example 4: Nested structures
(define user {:name "Alice"
              :scores [95 88 92]
              :active true})

(if (user :active)
    (log :message "User is active")
    (log :message "User is inactive"))
```

**IMPORTANT NOTES:**

1. **All `.ovsm` files MUST use LISP syntax** - There is no alternative
2. **Ranges are exclusive:** `[1..5]` creates `[1, 2, 3, 4]` (5 excluded)
3. **Booleans are lowercase:** `true` and `false` (not True/False)
4. **Comments use semicolons:** `;;` not `#` or `//`
5. **Operators are variadic:** `(+ 1 2 3 4)` works, returns 10
6. **Keyword arguments:** Use `:key` syntax for function arguments
7. **No indentation sensitivity:** Parentheses define structure
8. **Works standalone:** No Solana keypair needed for OVSM operations

**SYNTAX REFERENCE:**
- Language spec: `OVSM_LISP_SYNTAX_SPEC.md` (PRIMARY REFERENCE)
- Implementation report: `FINAL_LISP_IMPLEMENTATION_REPORT.md`
- OVSM README: `crates/ovsm/README.md`
- Usage guide: `crates/ovsm/USAGE_GUIDE.md`
- Example scripts: `examples/ovsm_scripts/*.ovsm`
- Integration tests: `crates/ovsm/tests/lisp_e2e_tests.rs`

---

## Main Commands

### 🔍 PRIMARY: Blockchain Investigation (AI-Powered)
```bash
# Natural language queries for blockchain analysis - THE MAIN FEATURE
osvm "show me all transactions for wallet ABC...XYZ in the last 24 hours"
osvm "analyze DEX trading volume on Raydium for token XYZ"
osvm "find NFT holders for collection ABC with rare traits"
osvm "track token transfers between these wallets: [A, B, C]"
osvm "identify unusual activity in program 123...456"
osvm "what are the top 10 most active wallets today?"
osvm "show me all failed transactions for this wallet"

# AI Planning Mode (explicit planning with OVSM agent)
osvm p "analyze arbitrage opportunities between Orca and Raydium"
osvm a "find wallets that interacted with both program A and B"
osvm --plan "identify potential bot activity in the last 100 blocks"
osvm -p "calculate total fees paid by wallet X this month"

# The planning mode provides:
# - Explicit OVSM code generation
# - Step-by-step execution plans
# - Enhanced debugging capabilities
# - Better control over query execution
```

### 📡 MCP Server Management (Enhanced Data Access)
```bash
# Configure MCP servers for additional data sources
osvm mcp add helius https://helius-mcp.example.com
osvm mcp add shyft https://shyft-mcp.example.com
osvm mcp list                          # List configured MCP servers
osvm mcp test helius                   # Test MCP server connection
osvm mcp remove shyft                  # Remove MCP server

# MCP servers provide specialized tools for:
# - Enhanced RPC endpoints (Helius, Triton)
# - DEX analytics (Jupiter, Raydium)
# - NFT metadata (Metaplex, Magic Eden)
# - Token analytics (Birdeye, CoinGecko)
```

### 🤖 AI Planning Mode
```bash
# Use AI planning mode for natural language queries
osvm p "show me all transactions for this wallet"
osvm a "analyze recent DEX trades"
osvm plan "find unusual activity"
osvm --plan "calculate total fees"
osvm -p "trace token flow"      # Short flag
osvm -a "identify arbitrage"    # Short flag alias

# Planning mode uses OVSM agent to:
# - Parse natural language intent
# - Generate OVSM execution plan
# - Execute via MCP tools
# - Return formatted results
```

### 💬 Interactive Chat Mode
```bash
# Start interactive AI chat for continuous investigation
osvm chat                               # Basic chat interface
osvm chat --advanced                    # Advanced mode with more tools

# In chat mode, you can:
# - Run multiple queries in sequence
# - Refine investigations based on results
# - Export findings to various formats
# - Save investigation sessions
```

### 🔧 OVSM Scripting (Advanced Users)
```bash
# Direct OVSM LISP script execution
osvm ovsm run investigation.ovsm       # Run investigation script
osvm ovsm eval '(getBalance "wallet")' # Execute inline OVSM
osvm ovsm repl                         # Interactive OVSM REPL
osvm ovsm examples                     # Show example scripts
```

### 🌐 Basic Solana Commands
```bash
osvm balance [ADDRESS]                 # Show SOL balance
osvm rpc local                         # Start local RPC
osvm rpc devnet                        # Start devnet validator
osvm svm list                          # List SVMs
osvm nodes list                        # List nodes
```

### 🛠️ Utilities
```bash
osvm audit [REPO]                      # Security audit
osvm doctor [--fix]                    # System diagnostics
osvm user@host --svm sonic             # SSH deployment
```

### 📡 BBS System (Decentralized Communication)
```bash
# Initialize and manage BBS database
osvm bbs init                                   # Initialize database with default boards
osvm bbs init --reset                           # Reset database (destroys all data)
osvm bbs boards list                            # List all boards
osvm bbs boards create CUSTOM "Description"    # Create custom board

# Post and read messages
osvm bbs post GENERAL "Hello!"                  # Post to a board
osvm bbs post ALERTS "Alert" --title "Warning"  # Post with title
osvm bbs read GENERAL                           # Read board messages
osvm bbs read GENERAL --limit 50 --follow       # Follow mode with limit
osvm bbs reply 42 "Reply to message #42"        # Reply to message

# HTTP API server
osvm bbs server                                 # Start on default port 8080
osvm bbs server --host 0.0.0.0 --port 3000     # Custom host/port

# On-chain registry (Solana devnet) - TRUSTLESS PEER DISCOVERY
osvm bbs registry register "http://ip:8080" "NodeName"  # Register on-chain
osvm bbs registry list                          # List all registered nodes
osvm bbs registry list --json                   # JSON output
osvm bbs registry update --address "http://new-ip:8080" # Update registration
osvm bbs registry heartbeat                     # Update last_seen timestamp
osvm bbs registry deregister                    # Remove from registry
osvm bbs registry discover                      # Discover and add peers from registry

# Federation (peer-to-peer sync)
osvm bbs peers add http://192.168.1.100:8080   # Add peer manually
osvm bbs peers list                             # List known peers
osvm bbs peers sync                             # Sync messages from all peers
osvm bbs peers discover --local                 # Auto-discover on LAN (mDNS)

# Meshtastic radio (off-grid communication)
osvm bbs radio connect 192.168.1.100:4403      # Connect via TCP
osvm bbs radio connect /dev/ttyUSB0 --serial   # Connect via serial
osvm bbs radio status                           # Check connection status
osvm bbs radio send "Message"                   # Broadcast message
osvm bbs radio send "DM" --to !abcd1234        # Direct message to node

# Agent management
osvm bbs agent register "BotName" --capabilities "research,monitor"
osvm bbs agent list                             # List registered agents
osvm bbs agent status self                      # Check own status

# Interactive shell
osvm bbs interactive                            # Start BBS shell
osvm bbs stats                                  # Show BBS statistics
```

**On-Chain Registry Program ID:** `CrCWo8atPHMtDiun76czDood6RnPYVvzxPmoMMP4TSCG` (Devnet - **LIVE**)

**Key Features:**
- On-chain registry for trustless, censorship-resistant peer discovery
- Federation for peer-to-peer message sync with deduplication
- Meshtastic radio for off-grid LoRa mesh communication
- Agent identity registration for verified AI agent messaging
- HTTP API with REST endpoints and WebSocket for real-time updates

### 🌐 BBS Federation Architecture (PRODUCTION-READY)

**How Federation Works:**

```
┌─────────────────────────────────────────────────────────────────────┐
│                    SOLANA DEVNET (Trustless Discovery)              │
│                                                                     │
│    ┌─────────────────────────────────────────────────────────────┐  │
│    │              BBS Registry Program                          │  │
│    │   Program ID: CrCWo8atPHMtDiun76czDood6RnPYVvzxPmoMMP4TSCG │  │
│    │                                                             │  │
│    │   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │  │
│    │   │ NodeA PDA   │  │ NodeB PDA   │  │ NodeC PDA   │        │  │
│    │   │ addr, name  │  │ addr, name  │  │ addr, name  │        │  │
│    │   │ heartbeat   │  │ heartbeat   │  │ heartbeat   │        │  │
│    │   └─────────────┘  └─────────────┘  └─────────────┘        │  │
│    └─────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
                              │
                    ┌─────────┴──────────┐
                    │  osvm bbs registry │
                    │     discover       │
                    └─────────┬──────────┘
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                     HTTP FEDERATION LAYER                           │
│                                                                     │
│  ┌─────────────┐         ┌─────────────┐         ┌─────────────┐  │
│  │   Node A    │◄───────►│   Node B    │◄───────►│   Node C    │  │
│  │  :8080      │  sync   │  :8081      │  sync   │  :8082      │  │
│  │             │  push   │             │  push   │             │  │
│  └─────────────┘         └─────────────┘         └─────────────┘  │
│                                                                     │
│  Message Flow:                                                      │
│  1. User posts on Node A                                           │
│  2. Instant push to peers via POST /api/federation/receive         │
│  3. Background sync every 10s via POST /api/federation/sync        │
│  4. Deduplication by message_id prevents duplicates                │
└─────────────────────────────────────────────────────────────────────┘
```

**Federation Endpoints:**
```
GET  /api/federation/peers    - List known peers
POST /api/federation/peers    - Add a peer {"address":"http://..."}
POST /api/federation/sync     - Request messages since timestamp
POST /api/federation/receive  - Receive pushed message from peer
POST /api/federation/announce - Announce presence to peer
```

**Federation Implementation Details:**
- **Sync Interval:** 10 seconds (configurable in `federation.rs`)
- **Instant Push:** New posts/replies immediately pushed to all peers
- **Deduplication:** Messages identified by `origin_node:local_id`
- **Gossip:** Peers share their peer lists during sync
- **Database Tables:** `federated_messages`, `known_peers` (SQLite)

**Quick Start - Two Node Federation:**
```bash
# Terminal 1: Start Node A
osvm bbs server --port 8080

# Terminal 2: Start Node B
osvm bbs server --port 8081

# Terminal 3: Connect them
curl -X POST http://localhost:8080/api/federation/peers \
  -H "Content-Type: application/json" \
  -d '{"address":"http://localhost:8081"}'

curl -X POST http://localhost:8081/api/federation/peers \
  -H "Content-Type: application/json" \
  -d '{"address":"http://localhost:8080"}'

# Post on Node A - appears on Node B instantly!
curl -X POST http://localhost:8080/api/boards/GENERAL/posts \
  -H "Content-Type: application/json" \
  -d '{"message":"Hello federation!"}'
```

**Key Files:**
- `src/utils/bbs/federation.rs` - FederationManager, sync loop, peer management
- `src/utils/bbs/http_server.rs` - REST/WebSocket API, instant push logic
- `src/utils/bbs/db/federated.rs` - Database operations for federated messages
- `src/utils/bbs/registry.rs` - Solana on-chain registry client
- `programs/bbs-registry/src/lib.rs` - Solana program for peer discovery

---

## Configuration Management

**Order of precedence:**
1. Command-line arguments
2. Environment variables
3. Config file (`~/.config/osvm/config.yml`)
4. Default values

**Config file example:**
```yaml
json_rpc_url: https://api.mainnet-beta.solana.com
keypair_path: ~/.config/solana/id.json
commitment: confirmed
```

**Environment variables:**
```bash
# AI configuration
OPENAI_URL="https://api.openai.com/v1/chat/completions"
OPENAI_KEY="sk-..."

# Or use Ollama locally
OPENAI_URL="http://localhost:11434/v1/chat/completions"
OPENAI_KEY="ollama-key"

# Debug output
RUST_LOG=debug
RUST_BACKTRACE=1
```

---

## Testing Strategy

### Running Tests
```bash
# All tests
cargo test

# Unit tests only
cargo test --lib --bins

# Integration tests
cargo test --test main

# OVSM LISP tests (ONLY supported syntax)
cargo test --lib -- sexpr                    # Lexer and parser
cargo test runtime::lisp_evaluator           # Evaluator
cargo test --test lisp_e2e_tests             # End-to-end integration

# Specific test module
cargo test utils::self_repair

# With output
cargo test -- --nocapture

# Benchmarks
cargo bench
```

### Test Organization
- **Unit tests**: Alongside source files in `#[cfg(test)] mod tests`
- **Integration tests**: `tests/` directory
- **OVSM integration**: `crates/ovsm/tests/lisp_e2e_tests.rs`
- **Test utilities**: `utils/test_utils.rs`

---

## Error Handling

**Strategies:**
- Use `Result<T, Box<dyn Error>>` for main functions
- Use `anyhow::Result` in utilities for context
- Use `thiserror` for custom error types
- Always provide context with `.context()`

**Error Recovery:**
1. Automatic retry with backoff
2. Circuit breaker activation
3. Self-repair attempt (`osvm doctor --fix`)
4. Fallback to manual intervention
5. Detailed error reporting

---

## Common Patterns

### Async/Await
```rust
#[tokio::main]
async fn main() -> Result<()> {
    let result = some_async_operation().await?;
    Ok(())
}
```

### Command Pattern
```rust
match sub_command {
    "svm" => handle_svm_command(matches),
    "nodes" => handle_nodes_command(matches),
    "ovsm" => handle_ovsm_command(matches).await,  // Early handler
    _ => handle_unknown_command(sub_command),
}
```

### Builder Pattern
```rust
let config = DeploymentConfig::builder()
    .svm_type("sonic")
    .network(NetworkType::Mainnet)
    .hot_swap_enabled(true)
    .build()?;
```

---

## Debugging

### Debug Output
```bash
# Enable debug mode
osvm --debug <command>

# Verbose levels
osvm -v      # Level 1
osvm -vv     # Level 2
osvm -vvv    # Level 3

# Rust logging
RUST_LOG=debug osvm <command>
RUST_LOG=osvm=trace,solana=debug osvm <command>
```

### Common Issues

**1. Compilation Errors**
```bash
cargo clean && cargo build
cargo update
cargo tree -d  # Show duplicate dependencies
```

**2. SSH Deployment Failures**
```bash
ssh user@host "echo test"  # Test connection
chmod 600 ~/.ssh/id_rsa
osvm --debug user@host --svm sonic
```

**3. RPC Connection Issues**
```bash
# Test RPC endpoint
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' \
  https://api.mainnet-beta.solana.com

# Use custom RPC
osvm --url https://your-rpc.com <command>
```

**4. OVSM Script Issues**
```bash
# Test simple LISP expression
osvm ovsm eval '(+ 1 2 3)'

# Check syntax without execution
osvm ovsm check script.ovsm

# Run with verbose output
osvm --verbose ovsm run script.ovsm

# Interactive REPL for debugging
osvm ovsm repl
```

---

## 🏗️ Project Maintenance Best Practices

**Repository Hygiene:**
1. **Root directory** should only contain essential files (README, LICENSE, Cargo.toml)
2. **Documentation** belongs in `/docs`, not scattered in root
3. **Archive directories** should be gitignored, not in public repo
4. **GitHub Pages** should serve from `/docs` with CNAME there
5. **Large unrelated projects** (like turbo) should never be in the repo

**When Helping with Cleanup:**
- Always check if files are git-tracked before deleting
- Move files to organized subdirectories rather than deleting
- Use `git mv` to preserve history when reorganizing
- Check `.gitignore` to understand what's local vs tracked

## Important Implementation Notes

1. **Workspace Structure**: The project uses Cargo workspaces. OVSM is a separate crate in `crates/ovsm/`.

2. **Early Command Handling**: `ovsm`, `audit`, `mcp`, and `chat` commands are handled BEFORE Solana config loading to allow standalone operation.

3. **Clippy Warnings**: Project currently allows all clippy warnings (`#![allow(clippy::all)]`).

4. **Error Handling**: Mix of `anyhow::Result` for internal functions and custom error types for public APIs.

5. **Async Runtime**: Uses Tokio. All network operations are async.

6. **Version Compatibility**: Maintain compatibility with Solana SDK 3.0.0.

7. **OVSM Test Coverage**: The OVSM LISP interpreter has 97.3% test coverage, ensuring production reliability.

8. **Isolation Features**: Phases 1-3 complete (Firecracker MicroVMs, hot-swap updates, vsock communication). See `examples/` for demos.

9. **OVSM is LISP-Only**: There is no alternative syntax. All OVSM scripts use S-expressions.

---

## Project Status

**Current Phase**: Phase 3 Complete (Beta Ready)
- ✅ Phase 1: Foundation (Unikernel runtime, mTLS, MCP)
- ✅ Phase 2: Production (Firecracker, hot-swap, vsock, orchestration)
- ✅ Phase 3: Advanced (TEE framework, auto-scaler, hardware key protection)
- ⏳ Phase 4: Hardening (load testing, security audit, performance benchmarks)

**Test Coverage**:
- Isolation modules: 47/48 passing (98% coverage)
- OVSM LISP interpreter: 356/356 tests passing (100% coverage)

**Known Issues**:
- Minor: Operator parsing enhancement pending (5-line fix for comparison operators in all contexts)

---

## Resources

**Documentation:**
- Architecture: `Architecture.md` - 2,150 lines covering unikernels, MicroVMs, hardware security
- Design Doc: `Design-Doc.md` - Implementation details
- Roadmap: `Plan.md` - 15-month plan
- **OVSM LISP Syntax**: `OVSM_LISP_SYNTAX_SPEC.md` - **PRIMARY LANGUAGE REFERENCE**
- LISP Implementation: `FINAL_LISP_IMPLEMENTATION_REPORT.md`
- OVSM README: `crates/ovsm/README.md`
- OVSM Usage: `crates/ovsm/USAGE_GUIDE.md`
- Isolation Guide: `examples/ISOLATION_GUIDE.md`

**Examples:**
- `examples/firecracker_demo` - MicroVM deployment
- `examples/mcp_integration_demo` - Unikernel deployment
- `examples/ovsm_scripts/*.ovsm` - OVSM LISP examples

**Key Files:**
- `src/main.rs` - Entry point and command routing
- `src/clparse.rs` - Command-line parsing
- `src/services/ovsm_service.rs` - OVSM integration
- `crates/ovsm/src/lexer/sexpr_scanner.rs` - S-expression lexer
- `crates/ovsm/src/parser/sexpr_parser.rs` - S-expression parser
- `crates/ovsm/src/runtime/lisp_evaluator.rs` - OVSM LISP evaluator
- `crates/ovsm/src/compiler/ir/` - IR module (see OVSM Compiler Architecture above)
- `crates/ovsm/src/compiler/codegen/` - sBPF bytecode generation

---

## ✅ TUI DASHBOARD STATUS (PRODUCTION-READY)

### AI Insights Panel (FULLY IMPLEMENTED)

The TUI dashboard AI Insights panel is now production-ready with comprehensive real-time analysis.

**Implemented Features** (`src/utils/tui/app.rs:2604-2799`, `src/utils/tui/graph.rs:2275-2380`):

1. **✅ Risk Scoring System** - Color-coded risk levels (Critical/High/Medium/Low) with 0-100 score
   - Critical alerts displayed prominently with 🚨 icons
   - Actionable context provided for each risk level
   - Human-readable explanations for all risk factors

2. **✅ Network Complexity Analysis** (`graph.rs:2281`)
   - Edges/nodes ratio calculation
   - >5.0 triggers CRITICAL alert (potential mixer)
   - 3.0-5.0 elevates risk score

3. **✅ Whale Detection** (`graph.rs:2323`)
   - Configurable threshold (default 100 SOL)
   - Tracks large flow counts and totals
   - Contributes to risk score

4. **✅ Wallet Behavior Classification** (`graph.rs:2187-2271`)
   - Bot (🤖): Regular intervals, high frequency
   - Exchange (🏦): High volume, many counterparties
   - Trader (📈): DEX interactions, moderate volume
   - Mixer (🌀): Many small I/O, obfuscation patterns
   - EOA (👤): Human-like patterns
   - Contract (📜): Program accounts
   - Dormant (💤): Very low activity

5. **✅ Circular Flow Detection** (`graph.rs:2113-2185`)
   - Detects wash trading patterns
   - Shows cycle length and total amounts
   - CRITICAL alerts for circular flows

6. **✅ Rapid Transfer Detection** (`graph.rs:2030-2111`)
   - Configurable time windows and thresholds
   - Severity levels: Critical/High/Medium/Low
   - Volume-based and count-based triggers

7. **✅ Token Diversity Analysis** (`graph.rs:2348-2353`)
   - Tracks unique tokens across transfers
   - >20 tokens indicates portfolio mixing

8. **✅ Entity Clustering** (`graph.rs:2382-2430`)
   - Groups related wallets by behavior
   - Shows cluster sizes and confidence scores
   - Identifies coordinated wallet activity

9. **✅ Hub Wallet Detection** (`graph.rs:2355-2360`)
   - Identifies source/sink/hub patterns
   - Detects potential coordination points

### Other Dashboard Components (FIXED)

1. **Graph Visualization** (`src/utils/tui/graph.rs`)
   - ✅ Columnar layout: `-X` = inflows, `+X` = outflows
   - ✅ Abbreviated addresses (first 3 + last 3 chars)
   - ✅ All nodes get positions for edge rendering
   - ✅ Edges draw to off-viewport nodes
   - Minor: Node label positioning at high zoom could be improved

2. **MCP Compression** (`src/services/research_agent.rs`)
   - ✅ All `get_account_transfers` calls include `compress: true`

3. **Token Holdings Widget**
   - ✅ Shows up to 7 tokens with horizontal bar charts

### Testing the Dashboard

```bash
# Run research mode with TUI
./target/release/osvm research <WALLET_ADDRESS> --tui

# What to check:
# 1. Does AI Insights show real analysis? (YES - fully implemented)
# 2. Do edges render to off-screen nodes? (YES)
# 3. Is layout columnar with proper flow? (YES)
# 4. Are addresses abbreviated? (YES)
# 5. Are insights actionable? (YES - with risk context and recommendations)
```

### Future Enhancements (LOW PRIORITY)

- Time-series sparklines for transfer activity trends
- Wallet labeling (DEX, CEX, known programs from database)
- Historical comparison features
- Advanced filtering UI

---
