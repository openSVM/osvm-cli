# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**See also:**
- `.claude/rules.md` - Behavioral preferences & workflow rules
- `.claude/architecture.md` - Technical architecture deep dive

---

## CRITICAL SECURITY RULES

### Never Modify Solana Keypairs

**ABSOLUTE PROHIBITION:**
- NEVER create, modify, or delete `~/.config/solana/id.json`
- NEVER create, modify, or delete `~/.config/solana/cli/config.yml`
- NEVER run `solana-keygen` with `--force` on user's keypair location

**WHY:** These contain irreplaceable cryptographic private keys. Overwriting causes permanent, irreversible loss of funds.

**Safe Testing:**
```bash
TMP_KEYPAIR="/tmp/test-keypair-$(date +%s).json"
solana-keygen new --no-bip39-passphrase --outfile "$TMP_KEYPAIR"
osvm --keypair "$TMP_KEYPAIR" balance
rm -f "$TMP_KEYPAIR"
```

### Git Safety

Before destructive operations (`git filter-branch`, `git filter-repo`, force push):
```bash
tar czf ~/git-backup-$(date +%Y%m%d-%H%M%S).tar.gz .git
```

### Never Truncate Wallet Addresses

Blockchain forensics requires exact addresses. Always display full base58 strings:
- BAD: `5Q544f...e4j1`
- GOOD: `5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1`

---

## Project Overview

**OSVM CLI** = AI-powered natural language interface for Solana blockchain investigation

```bash
# Primary usage - natural language queries
osvm "show me all DEX trades for wallet ABC...XYZ today"

# AI planning mode
osvm p "analyze arbitrage opportunities"
osvm -p "trace token flow from wallet X"

# Direct OVSM execution
osvm ovsm eval '(+ 1 2 3)'
osvm ovsm run script.ovsm
```

---

## Development Commands

### Building
```bash
cargo build                    # Debug build
cargo build --release          # Optimized build
make build                     # Via Makefile
make dev                       # Clean, build, test
```

### Testing
```bash
cargo test                     # All tests
cargo test --lib --bins        # Unit tests only
cargo test -- --nocapture      # With output
cargo test <name>              # Specific test

# OVSM-specific tests
cargo test --lib -- sexpr      # Lexer/parser
cargo test runtime::lisp_evaluator
cargo test --test lisp_e2e_tests
```

### Code Quality
```bash
cargo fmt --all                # Format
cargo clippy                   # Lint (warnings allowed)
cargo check                    # Quick syntax check
```

### Installation
```bash
make install                   # Release binary to /usr/bin
make install-dev               # Debug binary to /usr/bin
osvm --version                 # Verify
```

---

## Key Files

| File | Purpose |
|------|---------|
| `src/main.rs` | Entry point, command routing |
| `src/clparse.rs` | CLI argument parsing (clap v4) |
| `src/services/*.rs` | High-level business logic |
| `src/utils/*.rs` | Core implementations |
| `crates/ovsm/` | OVSM LISP interpreter + sBPF compiler |
| `crates/ovsm-lsp/` | Language Server Protocol |

---

## OVSM LISP Reference

All `.ovsm` files use LISP S-expression syntax. **Full spec:** `OVSM_LISP_SYNTAX_SPEC.md`

**Quick Reference:**
```lisp
;; Variables
(define balance 1000)
(set! balance (+ balance 100))

;; Data types
42  3.14  "string"  true  false  null
[1 2 3]                    ;; Array
{:name "Alice" :age 30}    ;; Object

;; Control flow
(if (> x 10) "high" "low")
(for (item collection) (log :value item))
(while (< i 10) (set! i (+ i 1)))

;; Polymorphic get (works with objects AND arrays)
(get response "data")      ;; Object key
(get array 0)              ;; Array index
(get nested 999)           ;; Returns null if missing
```

**Key Gotchas:**
1. Booleans are lowercase: `true`, `false`
2. Comments use `;;` not `#` or `//`
3. Ranges are exclusive: `(range 1 5)` → `[1, 2, 3, 4]`
4. Operators are variadic: `(+ 1 2 3 4)` → `10`
5. Works standalone - no Solana config needed

---

## Main Commands

```bash
# Blockchain investigation (primary feature)
osvm "your natural language query"
osvm p "query"                 # Planning mode
osvm chat                      # Interactive chat
osvm chat --advanced           # Advanced mode

# OVSM scripting
osvm ovsm run script.ovsm
osvm ovsm eval '(expression)'
osvm ovsm repl
osvm ovsm check script.ovsm

# MCP servers
osvm mcp add <name> <url>
osvm mcp list
osvm mcp test <name>

# BBS system
osvm bbs init
osvm bbs post GENERAL "Hello!"
osvm bbs server --port 8080
osvm bbs registry list         # On-chain peer discovery

# Utilities
osvm balance [ADDRESS]
osvm audit [REPO]
osvm doctor [--fix]
osvm research <wallet> --tui
```

---

## Configuration

**Precedence:** CLI args > env vars > config file > defaults

```yaml
# ~/.config/osvm/config.yml
json_rpc_url: https://api.mainnet-beta.solana.com
keypair_path: ~/.config/solana/id.json
commitment: confirmed
```

```bash
# Environment variables
OPENAI_URL="https://api.openai.com/v1/chat/completions"
OPENAI_KEY="sk-..."
RUST_LOG=debug
```

---

## Implementation Notes

1. **Early Command Handlers**: `ovsm`, `audit`, `mcp`, `chat` work WITHOUT Solana config
2. **Clippy Warnings**: Project allows all warnings (`#![allow(clippy::all)]`)
3. **Async Runtime**: Tokio - all network operations are async
4. **Error Handling**: Mix of `anyhow::Result` and `thiserror` custom types
5. **Workspace**: Main crate + `crates/ovsm` + `crates/ovsm-lsp` + `guest/mcp_vsock_wrapper`

---

## Debugging

```bash
osvm --debug <command>         # Debug mode
osvm -v / -vv / -vvv          # Verbose levels
RUST_LOG=debug osvm <cmd>      # Rust logging
RUST_LOG=osvm=trace osvm <cmd> # Trace specific crate
```

**Common Issues:**
```bash
# Compilation errors
cargo clean && cargo build

# Dependency conflicts
cargo tree -d

# RPC connection
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' \
  https://api.mainnet-beta.solana.com
```

---

## Documentation

| Document | Purpose |
|----------|---------|
| `README.md` | User-facing documentation |
| `OVSM_LISP_SYNTAX_SPEC.md` | Complete OVSM language specification |
| `Architecture.md` | System design (2,150 lines) |
| `crates/ovsm/README.md` | OVSM crate documentation |
| `crates/ovsm/USAGE_GUIDE.md` | OVSM scripting guide |
| `examples/ISOLATION_GUIDE.md` | MicroVM/unikernel isolation |

---

## Repository Hygiene

- Root directory: Only essential files (README, LICENSE, Cargo.toml)
- Documentation: Lives in `/docs`
- Before deleting files: Check if git-tracked
- Use `git mv` to preserve history when reorganizing
