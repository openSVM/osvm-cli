# OVSM Language Support for VS Code

Language support for OVSM LISP - the scripting language for blockchain automation.

## Features

- **Syntax Highlighting**: Full syntax highlighting for OVSM LISP
- **Error Detection**: Real-time parenthesis matching and syntax errors
- **Hover Documentation**: Hover over built-in functions to see documentation
- **Auto-completion**: Completion suggestions for built-in functions
- **Bracket Matching**: Automatic bracket matching and closing

## Requirements

You need the OVSM LSP server installed:

```bash
# Build and install from source
cd osvm-cli
cargo build --release -p ovsm-lsp
sudo cp target/release/ovsm-lsp /usr/local/bin/

# Or add to PATH
export PATH="$PATH:/path/to/osvm-cli/target/release"
```

## Installation

### From VSIX (Recommended)

1. Build the extension:
   ```bash
   cd editors/vscode
   npm install
   npm run package
   ```

2. Install in VS Code:
   - Open VS Code
   - Press `Ctrl+Shift+P` → "Extensions: Install from VSIX..."
   - Select the generated `.vsix` file

### Development

1. Clone the repository
2. Open `editors/vscode` in VS Code
3. Run `npm install`
4. Press `F5` to launch Extension Development Host

## Configuration

### `ovsm.lsp.path`

Path to the `ovsm-lsp` executable. Default: `"ovsm-lsp"` (uses PATH).

```json
{
    "ovsm.lsp.path": "/path/to/ovsm-lsp"
}
```

### `ovsm.lsp.trace.server`

Traces communication between VS Code and the language server. Options: `"off"`, `"messages"`, `"verbose"`.

## Commands

- **OVSM: Restart Server** - Restart the language server

## Supported File Types

- `.ovsm` - OVSM LISP files

## Example OVSM Code

```ovsm
;; Define a function
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Use the function
(define result (factorial 5))
(log :message "5! =" :value result)
```

## Troubleshooting

### LSP Server Not Found

Make sure `ovsm-lsp` is in your PATH or configure `ovsm.lsp.path`.

### No Syntax Highlighting

Ensure the file has `.ovsm` extension.

### Check LSP Logs

View → Output → Select "OVSM Language Server" or "OVSM LSP Trace".

## License

MIT
