# OSVM Architecture Deep Dive

Technical architecture documentation for developers.

---

## Workspace Structure

```
osvm-cli/                    # Root workspace
├── src/                     # Main CLI crate
│   ├── main.rs             # Entry point, command routing
│   ├── clparse.rs          # CLI argument parsing (clap v4)
│   ├── commands/           # Command handlers
│   ├── services/           # High-level business logic
│   └── utils/              # Core implementations
├── crates/
│   ├── ovsm/               # OVSM LISP interpreter + sBPF compiler
│   ├── ovsm-lsp/           # Language Server Protocol for OVSM
│   └── osvm-web/           # Web interface (if applicable)
├── guest/
│   └── mcp_vsock_wrapper/  # MicroVM guest-side vsock communication
└── programs/
    └── bbs-registry/       # Solana program for BBS peer discovery
```

### Crate Interactions

| Crate | Purpose | Dependencies |
|-------|---------|--------------|
| `osvm` (root) | CLI binary | Depends on `ovsm` |
| `ovsm` | LISP interpreter, sBPF compiler | Standalone |
| `ovsm-lsp` | IDE integration | Depends on `ovsm` |
| `mcp_vsock_wrapper` | Guest-side MicroVM comms | vsock, serde |

---

## Command Flow

```
main.rs
   │
   ├── clparse::parse_command_line()
   │
   ├── [EARLY HANDLERS - before config loading]
   │   ├── "audit" → services/audit_service.rs
   │   ├── "mcp"   → services/mcp_service.rs
   │   ├── "chat"  → utils/agent_chat_v2.rs
   │   └── "ovsm"  → services/ovsm_service.rs
   │
   ├── Config::load() [only if needed]
   │
   └── [NORMAL HANDLERS]
       ├── "balance", "rpc", "svm", "nodes" → Solana commands
       ├── "bbs" → utils/bbs/mod.rs
       ├── "research" → services/research_agent.rs
       └── [unknown] → AI service for natural language
```

**Key Insight**: `ovsm`, `audit`, `mcp`, and `chat` commands work WITHOUT Solana config, enabling standalone operation.

---

## OVSM Compiler Pipeline

```
Source (.ovsm)
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ 1. LEXER (crates/ovsm/src/parser/sexpr_scanner.rs)          │
│    S-expression tokenization                                 │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. PARSER (crates/ovsm/src/parser/sexpr_parser.rs)          │
│    AST construction from tokens                              │
└─────────────────────────────────────────────────────────────┘
    │
    ├──► [INTERPRETATION PATH]
    │    └─► runtime/lisp_evaluator.rs → Immediate execution
    │
    └──► [COMPILATION PATH]
         │
         ▼
    ┌─────────────────────────────────────────────────────────┐
    │ 3. IR GENERATOR (compiler/ir/generator.rs)              │
    │    ~60 macro implementations, 5700 lines                 │
    │    Three-address-code IR                                 │
    └─────────────────────────────────────────────────────────┘
         │
         ▼
    ┌─────────────────────────────────────────────────────────┐
    │ 4. REGISTER ALLOCATOR (compiler/codegen/register_*.rs)  │
    │    Graph coloring for physical register mapping          │
    └─────────────────────────────────────────────────────────┘
         │
         ▼
    ┌─────────────────────────────────────────────────────────┐
    │ 5. ELF WRITER (compiler/codegen/elf_writer.rs)          │
    │    Solana-compatible .so binary output                   │
    └─────────────────────────────────────────────────────────┘
```

---

## Three-Layer Security Model

```
┌─────────────────────────────────────────────────────────────┐
│ LAYER 3: Hardware Isolation                                  │
│ - VT-x/AMD-V virtualization                                  │
│ - Intel SGX / AMD SEV for memory encryption                  │
│ - TPM root of trust                                          │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ LAYER 2: MicroVMs (Firecracker)                             │
│ - 5MB overhead per instance                                  │
│ - 125ms boot time                                            │
│ - Used for validators, RPC nodes                             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ LAYER 1: Unikernels                                          │
│ - 50KB single-purpose OS                                     │
│ - 10-50ms boot time                                          │
│ - Used for untrusted MCP servers                             │
└─────────────────────────────────────────────────────────────┘
```

**Communication**: All layers use mTLS + vsock (0.3ms latency vs 5-50ms TCP)

---

## BBS Federation Architecture

```
┌─────────────────────────────────────────────────────────────┐
│              SOLANA DEVNET (Trustless Discovery)            │
│  Program ID: CrCWo8atPHMtDiun76czDood6RnPYVvzxPmoMMP4TSCG   │
│                                                              │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐                   │
│  │ NodeA    │  │ NodeB    │  │ NodeC    │  (PDAs)           │
│  │ PDA      │  │ PDA      │  │ PDA      │                   │
│  └──────────┘  └──────────┘  └──────────┘                   │
└─────────────────────────────────────────────────────────────┘
                         │
                         ▼ osvm bbs registry discover
┌─────────────────────────────────────────────────────────────┐
│                 HTTP FEDERATION LAYER                        │
│                                                              │
│  Node A ◄──────────► Node B ◄──────────► Node C             │
│  :8080      sync      :8081      sync      :8082            │
│             push               push                          │
│                                                              │
│  Endpoints:                                                  │
│  - POST /api/federation/sync     (request messages)          │
│  - POST /api/federation/receive  (instant push)              │
│  - POST /api/federation/announce (peer discovery)            │
└─────────────────────────────────────────────────────────────┘
                         │
                         ▼ (optional off-grid)
┌─────────────────────────────────────────────────────────────┐
│               MESHTASTIC RADIO LAYER                         │
│  LoRa mesh for areas without internet                        │
└─────────────────────────────────────────────────────────────┘
```

**Key Files:**
- `src/utils/bbs/federation.rs` - Sync loop, peer management
- `src/utils/bbs/http_server.rs` - REST/WebSocket API
- `src/utils/bbs/registry.rs` - Solana on-chain registry client
- `src/utils/bbs/db/*.rs` - SQLite via Diesel ORM

---

## Service Layer Pattern

All complex operations go through the service layer:

```rust
// src/services/mod.rs
pub mod ai_service;       // OpenAI/Ollama integration
pub mod audit_service;    // Security scanning
pub mod mcp_service;      // Model Context Protocol servers
pub mod ovsm_service;     // OVSM LISP interpreter wrapper
pub mod research_agent;   // Blockchain investigation agent
```

### Error Handling

```rust
// Use thiserror for custom error types in libraries
#[derive(thiserror::Error, Debug)]
pub enum OsvmError {
    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),
}

// Use anyhow::Result in applications for context
async fn do_thing() -> anyhow::Result<()> {
    risky_operation().context("Failed during risky operation")?;
    Ok(())
}
```

---

## Feature Flags

| Flag | Purpose | Enables |
|------|---------|---------|
| `default` | Standard build | Core functionality |
| `remote-wallet` | Hardware wallet support | `solana-remote-wallet` |
| `incomplete_tests` | Experimental tests | WIP test files |
| `mdns` | LAN peer discovery | mDNS for BBS federation |

```bash
# Build with specific features
cargo build --features "mdns,remote-wallet"

# Run tests including incomplete ones
cargo test --features incomplete_tests
```

---

## Configuration Hierarchy

1. **Command-line arguments** (highest priority)
2. **Environment variables**
3. **Config file** (`~/.config/osvm/config.yml`)
4. **Default values** (lowest priority)

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `OPENAI_URL` | AI endpoint (OpenAI or Ollama) |
| `OPENAI_KEY` | AI API key |
| `RUST_LOG` | Logging level (`debug`, `trace`) |
| `RUST_BACKTRACE` | Stack traces (`1`, `full`) |

---

## Database Schemas

### BBS (SQLite via Diesel)

```sql
-- Core tables
boards (id, name, description, created_at)
posts (id, board_id, author, title, message, created_at)
users (id, pubkey, username, created_at)

-- Federation tables
known_peers (id, address, last_seen, is_active)
federated_messages (id, origin_node, local_id, message_hash)
```

**Migrations**: Run `diesel migration run` if schema changes.

---

## TUI Dashboard Components

The research TUI (`osvm research <wallet> --tui`) has these panels:

| Panel | File | Purpose |
|-------|------|---------|
| Graph Visualization | `src/utils/tui/graph.rs` | Transaction flow graph |
| AI Insights | `src/utils/tui/app.rs:2604-2799` | Risk scoring, pattern detection |
| Token Holdings | `src/utils/tui/widgets.rs` | Portfolio display |

**Key Algorithms:**
- Circular flow detection (wash trading)
- Wallet behavior classification (Bot/Exchange/Trader/Mixer/EOA)
- Entity clustering for related wallet detection
- Rapid transfer detection with severity levels
