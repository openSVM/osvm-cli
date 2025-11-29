# OVSM-LSP Crate - CLAUDE.md

## Overview

**OVSM-LSP** is a Language Server Protocol implementation for OVSM LISP, providing IDE features like autocompletion, hover documentation, diagnostics, and semantic highlighting.

## Architecture

```
src/
├── main.rs                    # LSP server entry point
├── lib.rs                     # Public exports
├── backend.rs                 # Main LSP backend (32K lines)
├── diagnostics.rs             # Error/warning diagnostics
├── documentation.rs           # Hover docs & function signatures
├── semantic_tokens.rs         # Syntax highlighting
├── symbols.rs                 # Document/workspace symbols
├── ai_completion.rs           # AI-powered completions
├── blockchain_types.rs        # Solana type definitions
├── blockchain_rpc.rs          # Live blockchain data fetching
├── investigation_templates.rs # Pre-built investigation snippets
├── knowledge_transfer.rs      # Learning from user patterns
├── repl.rs                    # Interactive REPL support
└── telemetry.rs              # Usage analytics
```

## Key Components

### Backend (`backend.rs`)
- Implements `tower_lsp::LanguageServer` trait
- Handles all LSP requests/responses
- Document sync, completion, hover, go-to-definition
- ~32K lines - the core of the LSP

### AI Completion (`ai_completion.rs`)
- Integrates with OpenAI/Ollama for smart completions
- Context-aware suggestions based on cursor position
- Learns from user's coding patterns

### Blockchain Integration
- **`blockchain_types.rs`**: Solana account/instruction type definitions
- **`blockchain_rpc.rs`**: Fetches live data (balances, accounts) for hover info
- Shows real-time blockchain state in IDE

### Investigation Templates (`investigation_templates.rs`)
- Pre-built OVSM snippets for common blockchain investigations
- Wallet analysis, token flow tracking, DEX analysis
- Available via completion trigger

## Building & Running

```bash
# Build the LSP binary
cargo build -p ovsm-lsp --release

# Run the LSP server (connects via stdio)
./target/release/ovsm-lsp

# Test (requires tokio-test)
cargo test -p ovsm-lsp
```

## VS Code Integration

Add to `.vscode/settings.json`:
```json
{
  "ovsm.lspPath": "/path/to/ovsm-lsp",
  "ovsm.aiCompletion": true
}
```

Or use the VS Code extension (if published).

## LSP Capabilities

### Implemented
- `textDocument/completion` - Context-aware completions
- `textDocument/hover` - Function docs, type info, live blockchain data
- `textDocument/definition` - Go to symbol definition
- `textDocument/references` - Find all references
- `textDocument/documentSymbol` - Outline view
- `textDocument/semanticTokens` - Syntax highlighting
- `textDocument/publishDiagnostics` - Error/warning squiggles

### Planned
- `textDocument/rename` - Symbol renaming
- `textDocument/codeAction` - Quick fixes
- `textDocument/formatting` - Auto-format

## Adding New Completions

In `backend.rs`, find the completion handler and add items:

```rust
// Add macro completion
completions.push(CompletionItem {
    label: "my-new-macro".to_string(),
    kind: Some(CompletionItemKind::FUNCTION),
    detail: Some("(my-new-macro arg1 arg2) -> result".to_string()),
    documentation: Some(Documentation::String("Description here".to_string())),
    insert_text: Some("(my-new-macro ${1:arg1} ${2:arg2})".to_string()),
    insert_text_format: Some(InsertTextFormat::SNIPPET),
    ..Default::default()
});
```

## Dependencies

- `tower-lsp` - LSP protocol implementation
- `ovsm` - Core language crate (lexer, parser, errors)
- `reqwest` - HTTP for AI completion API
- `dashmap` - Concurrent document storage
- `dirs` - User config directory lookup

## Environment Variables

- `OPENAI_URL` - AI completion endpoint
- `OPENAI_KEY` - API key for completions
- `OVSM_LSP_LOG` - Logging level (trace/debug/info/warn/error)
