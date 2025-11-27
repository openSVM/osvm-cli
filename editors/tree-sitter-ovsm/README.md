# Tree-sitter Grammar for OVSM LISP

A Tree-sitter grammar for OVSM, a Common Lisp dialect for blockchain automation and Solana analysis.

## Features

- Full S-expression parsing with proper nesting
- Special form highlighting (define, let, lambda, if, etc.)
- Built-in function recognition (mapcar, filter, reduce, etc.)
- Blockchain function highlighting (getBalance, getTransaction, etc.)
- Keyword argument support (`:key` syntax)
- Quote/quasiquote/unquote expressions
- Object and array literals

## Installation

### Prerequisites

- Node.js 14+
- tree-sitter-cli (`npm install -g tree-sitter-cli`)

### Build

```bash
cd editors/tree-sitter-ovsm
npm install
npm run build
```

This generates the parser in `src/`.

### Test

```bash
npm test
```

## Editor Integration

### Helix

1. Build the grammar:
   ```bash
   npm run build
   ```

2. Copy to Helix runtime:
   ```bash
   mkdir -p ~/.config/helix/runtime/grammars
   cp src/parser.c ~/.config/helix/runtime/grammars/
   # Or build with tree-sitter and copy the .so file
   tree-sitter build-wasm
   ```

3. Add to `~/.config/helix/languages.toml`:
   ```toml
   [[language]]
   name = "ovsm"
   scope = "source.ovsm"
   file-types = ["ovsm"]
   roots = []
   comment-token = ";"
   language-servers = ["ovsm-lsp"]
   grammar = "ovsm"
   ```

4. Copy queries:
   ```bash
   mkdir -p ~/.config/helix/runtime/queries/ovsm
   cp queries/*.scm ~/.config/helix/runtime/queries/ovsm/
   ```

### Neovim (with nvim-treesitter)

1. Add to your nvim-treesitter config:
   ```lua
   local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
   parser_config.ovsm = {
     install_info = {
       url = "path/to/tree-sitter-ovsm",
       files = {"src/parser.c"},
       branch = "main",
     },
     filetype = "ovsm",
   }
   ```

2. Install the parser:
   ```vim
   :TSInstall ovsm
   ```

3. Copy queries to `~/.config/nvim/after/queries/ovsm/`:
   ```bash
   mkdir -p ~/.config/nvim/after/queries/ovsm
   cp queries/highlights.scm ~/.config/nvim/after/queries/ovsm/
   ```

## Grammar Overview

### Node Types

| Node Type | Description | Example |
|-----------|-------------|---------|
| `source_file` | Root node | Entire file |
| `list` | S-expression | `(+ 1 2)` |
| `array` | Array literal | `[1 2 3]` |
| `object` | Object literal | `{:key "value"}` |
| `special_form` | Keywords | `define`, `if`, `let` |
| `builtin_function` | Built-ins | `mapcar`, `filter` |
| `symbol` | Identifiers | `my-var`, `factorial` |
| `keyword_argument` | Keywords | `:message`, `:value` |
| `string` | Strings | `"hello"` |
| `number` | Numbers | `42`, `3.14` |
| `boolean` | Booleans | `true`, `false` |
| `comment` | Comments | `; comment` |

### Highlight Groups

The grammar supports these highlight captures:

- `@keyword` - Special forms (if, define, let, etc.)
- `@function.builtin` - Built-in functions
- `@function.call` - Function calls
- `@function.definition` - Function definitions
- `@variable` - Variables
- `@variable.parameter` - Parameters
- `@variable.definition` - Variable definitions
- `@string` - Strings
- `@number` - Numbers
- `@constant.builtin` - true, false, null
- `@comment` - Comments
- `@operator` - Operators
- `@punctuation.bracket` - Parentheses, brackets, braces

## Example OVSM Code

```ovsm
; Analyze wallet transactions
(defun analyze-wallet (address)
  (let ((transfers (get_account_transfers address :compress true)))
    (mapcar (lambda (tx)
              {:type (get tx "transferType")
               :amount (get tx "amount")
               :time (get tx "timestamp")})
            transfers)))

; Find large transfers
(define whale-threshold 1000)
(define whales
  (filter (lambda (tx)
            (> (get tx "amount") whale-threshold))
          (analyze-wallet "ABC...XYZ")))

(log :message "Whale transactions:" :value (length whales))
```

## Contributing

1. Edit `grammar.js`
2. Run `npm run build` to regenerate the parser
3. Run `npm test` to verify
4. Add test cases in `test/corpus/`

## License

MIT
