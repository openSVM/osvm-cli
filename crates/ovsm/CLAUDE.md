# OVSM Crate - CLAUDE.md

## Overview

**OVSM** (Open Versatile Seeker Mind) is a LISP-dialect interpreter and sBPF compiler for Solana blockchain automation and on-chain program development.

## Architecture

```
src/
├── lib.rs              # Public API exports
├── error.rs            # Error types (OvsmError, Result)
├── lexer/              # S-expression tokenizer
│   └── sexpr_scanner.rs
├── parser/             # AST construction
│   └── sexpr_parser.rs
├── runtime/            # Interpreter
│   └── lisp_evaluator.rs
├── compiler/           # sBPF code generation
│   ├── mod.rs          # Compiler orchestration
│   └── ir.rs           # IR generation (4800+ lines, main logic)
├── decompiler/         # sBPF to OVSM reverse engineering
├── ai/                 # AI-assisted code generation
├── metrics/            # Performance metrics
├── parallel/           # Concurrent execution
└── tools/              # Utility functions
```

## Key Components

### Lexer (`lexer/sexpr_scanner.rs`)
- Tokenizes LISP S-expressions
- Handles: `(`, `)`, strings, numbers, symbols, comments
- Line/column tracking for error messages

### Parser (`parser/sexpr_parser.rs`)
- Builds AST from tokens
- Expression types: `ToolCall`, `Variable`, `StringLiteral`, `IntLiteral`, etc.
- Validates syntax structure

### Evaluator (`runtime/lisp_evaluator.rs`)
- Interprets OVSM code in-memory
- Built-in functions: arithmetic, control flow, data structures
- MCP tool integration for blockchain operations

### Compiler (`compiler/ir.rs`) - **MOST IMPORTANT FILE**
- Generates sBPF bytecode from OVSM source
- 6100+ lines containing ALL macro implementations
- Key sections:
  - Struct operations (lines ~1100-1600)
  - CPI helpers (lines ~1900-3000)
  - SPL Token CPIs (lines ~3000-3400)
  - Round 4 macros (lines ~3380-4300)
  - Anchor error handling (lines ~4290-4400)
  - PDA operations (lines ~4400-4600)
  - Event emission (lines ~4800-4900)
  - Sysvar access (lines ~4900-5100)
  - PDA caching (lines ~5100-5300)

## Building & Testing

```bash
# Build the crate
cargo build -p ovsm

# Run all tests
cargo test -p ovsm

# Run specific test module
cargo test -p ovsm sexpr          # Lexer/parser tests
cargo test -p ovsm lisp_evaluator # Evaluator tests

# Compile an OVSM file to sBPF
cargo run --bin osvm -- ovsm compile input.ovsm -o output.so
```

## Adding New Macros

When implementing new macros in `compiler/ir.rs`:

1. **Location**: Add after existing similar macros (e.g., CPI macros near other CPIs)
2. **Pattern**: Match on `name == "macro-name" && args.len() == N`
3. **Registers**: Use `self.alloc_reg()` for temp registers
4. **Instructions**: Emit via `self.emit(IrInstruction::*)`
5. **Return**: Always `return Ok(Some(result_reg))` or `Ok(Some(zero))`

Example:
```rust
if name == "my-macro" && args.len() == 2 {
    let arg1 = self.generate_expr(&args[0].value)?
        .ok_or_else(|| Error::runtime("arg1 has no result"))?;
    let result = self.alloc_reg();
    self.emit(IrInstruction::ConstI64(result, 42));
    return Ok(Some(result));
}
```

## Important Constants

- **Account size**: 10336 bytes in serialized input
- **Account offsets**:
  - is_signer: +1
  - is_writable: +2
  - owner: +40
  - lamports: +56
  - data_len: +64
  - data: +72
- **Input pointer (R1)**: 1
- **Heap base**: 0x300000000

## Available Macros Reference

### Struct Operations
- `define-struct`, `struct-size`, `struct-offset`, `struct-field-size`
- `struct-get`, `struct-set`, `struct-ptr`, `struct-idl`

### Account Access
- `account-data-ptr`, `account-data-len`, `account-lamports`
- `is-signer`, `is-writable`, `assert-signer`, `assert-writable`, `assert-owner`
- `zerocopy-load`, `zerocopy-store`

### CPI Helpers
- `system-transfer`, `system-create-account`
- `spl-token-transfer`, `spl-token-transfer-signed`
- `spl-token-mint-to`, `spl-token-burn`

### Round 4 CPI & Error Handling (NEW)
- `spl-close-account` - Close token account, reclaim lamports
- `spl-close-account-signed` - Close with PDA authority
- `system-allocate` - Allocate space in account (System Program)
- `system-allocate-signed` - Allocate with PDA signer
- `system-assign` - Assign account ownership to program
- `system-assign-signed` - Assign with PDA signer
- `anchor-error` - Return Anchor-compatible error (6000 + code)
- `require` - Assert condition or abort with Anchor error
- `msg` - Log message (Anchor-style, uses `sol_log_`)

### PDA Operations
- `derive-pda`, `create-pda`, `get-ata`
- `pda-cache-init`, `pda-cache-store`, `pda-cache-lookup`

### Events & Sysvars
- `emit-event`, `emit-log`
- `clock-unix-timestamp`, `clock-epoch`, `rent-minimum-balance`
- `instruction-count`, `assert-not-cpi`

### Borsh Serialization
- `borsh-serialize`, `borsh-deserialize`, `borsh-size`

## Test Coverage

- 469/469 tests passing (100%)
- Integration tests in `tests/lisp_e2e_tests.rs`
- Compilation tests in `/tmp/*.ovsm` during development
