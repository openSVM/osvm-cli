# OVSM Language Support for Helix

OVSM LISP support for the [Helix](https://helix-editor.com/) editor.

## Quick Setup

### Prerequisites

- Helix 23.03+
- OVSM LSP server installed

### Install the LSP Server

```bash
cd osvm-cli
cargo build --release -p ovsm-lsp
sudo cp target/release/ovsm-lsp /usr/local/bin/
```

### Configure Helix

Add to `~/.config/helix/languages.toml`:

```toml
# Language definition for OVSM
[[language]]
name = "ovsm"
scope = "source.ovsm"
injection-regex = "ovsm"
file-types = ["ovsm"]
comment-token = ";"
indent = { tab-width = 2, unit = "  " }
language-servers = ["ovsm-lsp"]
auto-format = false

# Bracket pairs for auto-closing
[[language.auto-pairs]]
open = "("
close = ")"

[[language.auto-pairs]]
open = "["
close = "]"

[[language.auto-pairs]]
open = "{"
close = "}"

[[language.auto-pairs]]
open = '"'
close = '"'

# LSP server configuration
[language-server.ovsm-lsp]
command = "ovsm-lsp"
```

### Tree-sitter Grammar (Optional)

For syntax highlighting, Helix needs a tree-sitter grammar. You can use the built-in Scheme/Lisp grammar as a fallback:

```toml
[[language]]
name = "ovsm"
scope = "source.ovsm"
injection-regex = "ovsm"
file-types = ["ovsm"]
comment-token = ";"
indent = { tab-width = 2, unit = "  " }
language-servers = ["ovsm-lsp"]
# Use Scheme's tree-sitter for basic highlighting
grammar = "scheme"
```

Or create a symlink to use Scheme highlighting:

```bash
mkdir -p ~/.config/helix/runtime/grammars
ln -s /usr/share/helix/runtime/grammars/scheme.so ~/.config/helix/runtime/grammars/ovsm.so
```

## Complete Configuration File

Save this as `~/.config/helix/languages.toml`:

```toml
# OVSM Language Support
# =====================

[[language]]
name = "ovsm"
scope = "source.ovsm"
injection-regex = "ovsm"
file-types = ["ovsm"]
roots = []
comment-token = ";"
indent = { tab-width = 2, unit = "  " }
language-servers = ["ovsm-lsp"]
# Use Scheme grammar for basic highlighting until native OVSM grammar exists
grammar = "scheme"

[language-server.ovsm-lsp]
command = "ovsm-lsp"
args = []

# Optional: Enable debug logging
# [language-server.ovsm-lsp.environment]
# RUST_LOG = "debug"
```

## Features

| Feature | Status |
|---------|--------|
| Syntax highlighting | ✅ Via Scheme grammar (basic) or LSP semantic tokens |
| Real-time errors | ✅ LSP diagnostics |
| Hover documentation | ✅ Hover with `Space+k` |
| Auto-completion | ✅ LSP completion |
| Bracket matching | ✅ Built-in |
| Go to definition | ⏳ Coming soon |

## Key Bindings

Default Helix LSP bindings work:

| Key | Action |
|-----|--------|
| `Space + k` | Show hover documentation |
| `gd` | Go to definition |
| `gr` | Go to references |
| `Space + a` | Code actions |
| `Space + r` | Rename symbol |
| `Space + ?` | Show diagnostics |

## Troubleshooting

### Check if LSP is Running

```
:lsp-workspace-command
```

### View LSP Status

```
:log-open
```

### Verify Server Path

```bash
which ovsm-lsp
# Should output: /usr/local/bin/ovsm-lsp
```

### Test Server Manually

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ovsm-lsp
```

## Example OVSM File

Create `test.ovsm`:

```ovsm
;; OVSM Example - Helix should highlight this

(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))

(define result (factorial 5))
(log :message "Result:" :value result)
```

Open in Helix:
```bash
hx test.ovsm
```
