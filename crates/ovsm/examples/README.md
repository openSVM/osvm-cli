# OVSM Examples

This directory contains example OVSM scripts and Rust programs demonstrating the interpreter.

## Running Examples

### Execute OVSM Script Files

```bash
cargo run --example run_file <script.ovsm>
```

### Available Scripts

| Script | Description | Output |
|--------|-------------|--------|
| `hello_world.ovsm` | Simple hello world | `String("Hello from OVSM! 🚀")` |
| `factorial.ovsm` | Calculate 5! with FOR loop | `Int(120)` |
| `fibonacci.ovsm` | Calculate 10th Fibonacci number | `Int(55)` |
| `array_operations.ovsm` | Array iteration and average | `Int(3)` |
| `conditional_logic.ovsm` | Nested IF/ELSE for grading | `String("Grade: B...")` |
| `loop_control.ovsm` | BREAK and CONTINUE demo | `Int(64)` |

### Run All Examples

```bash
# Hello World
cargo run --example run_file examples/hello_world.ovsm

# Factorial (5! = 120)
cargo run --example run_file examples/factorial.ovsm

# Fibonacci (10th number = 55)
cargo run --example run_file examples/fibonacci.ovsm

# Array operations (average = 3)
cargo run --example run_file examples/array_operations.ovsm

# Conditional logic (grade based on score)
cargo run --example run_file examples/conditional_logic.ovsm

# Loop control (BREAK/CONTINUE)
cargo run --example run_file examples/loop_control.ovsm
```

## Example Rust Programs

| File | Description |
|------|-------------|
| `run_file.rs` | Execute OVSM scripts from files |
| `complete_demo.rs` | Comprehensive feature demonstration |
| `comprehensive_tools.rs` | Tool system examples |
| `qa_test_runner.rs` | QA test suite runner |
| `showcase_new_features.rs` | Feature showcase |
| `tools_demo.rs` | Standard library tools |

### Running Rust Examples

```bash
cargo run --example complete_demo
cargo run --example tools_demo
# etc.
```

## Creating Your Own Scripts

### Basic Template

```ovsm
// your_script.ovsm
$result = 0

FOR $i IN [1..10]:
    $result = $result + $i

RETURN $result
```

### Run It

```bash
cargo run --example run_file your_script.ovsm
```

## Important Notes

1. **Block Termination:** Use explicit ELSE branches or BREAK statements to avoid ambiguity
2. **Variable Scope:** Define variables before loops that need them
3. **Ranges:** `[1..10]` creates range 1-9 (exclusive end)
4. **Comments:** Use `//` for single-line comments

## Real-World Examples (`real_world/`)

Production-ready OVSM scripts demonstrating advanced patterns:

### Async & Streaming
| Script | Description |
|--------|-------------|
| `async_stream_example.ovsm` | Async stream processing with backpressure handling |
| `event_driven_stream.ovsm` | Event-driven architecture patterns |

### AI & Agents
| Script | Description |
|--------|-------------|
| `ai_compatibility_demo.ovsm` | AI model integration patterns |
| `onchain_ai_agent.ovsm` | Fully on-chain AI agent (11KB) |

### DeFi & Governance
| Script | Description |
|--------|-------------|
| `dao_simulation.ovsm` | Complete DAO governance simulation |
| `pumpfun_graduation_tracker.ovsm` | Track pump.fun token graduations to Raydium |
| `grad_tracker_final.ovsm` | Optimized graduation tracker |
| `whale_hunter.ovsm` | Whale wallet detection and tracking (11KB) |

### Wallet & Token Analysis
| Script | Description |
|--------|-------------|
| `wallet_discovery_deep.ovsm` | Multi-hop wallet relationship discovery |
| `analyze_wallet.ovsm` | Basic wallet activity analysis |
| `token_analysis_paginated.ovsm` | Paginated token flow analysis |
| `token_flow_analysis.ovsm` | Track token movement patterns |
| `working_token_analysis.ovsm` | Complete token analysis workflow |

### On-Chain Operations
| Script | Description |
|--------|-------------|
| `sol_transfer.ovsm` | Native SOL transfer via system program |
| `spl_token_transfer_signed.ovsm` | SPL token transfer with PDA signing |
| `ovsm_full_analysis.ovsm` | Comprehensive market analysis |
| `final_demo.ovsm` | Feature demonstration script |

### Running Real-World Examples

```bash
# Using OSVM CLI
osvm ovsm run crates/ovsm/examples/real_world/whale_hunter.ovsm

# Check syntax only
osvm ovsm check crates/ovsm/examples/real_world/dao_simulation.ovsm

# Compile to Solana BPF
osvm ovsm compile crates/ovsm/examples/real_world/onchain_ai_agent.ovsm -o agent.so
```

## Deployed Programs (Devnet)

These OVSM programs are live on Solana devnet:

| Program | Program ID | Description |
|---------|-----------|-------------|
| Strategy Registry | `1yEJEpCk1cKDmVGC7iFp8pf9EUcvKkczUmgHWKeX7p2` | Hot-swappable agent strategies |
| Basic Strategy | `2uiPuUnNjh4R1x3bwiX3ZRzqr7gbQyYtZ2PvXwU68xyh` | Entry-level agent strategy |

## See Also

- `../USAGE_GUIDE.md` - Complete language reference
- `../TEST_RESULTS_SUMMARY.md` - Implementation status
- `../tests/` - Unit test examples
- `../../examples/ovsm_scripts/` - More OVSM program examples
