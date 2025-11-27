# OVSM Language Support for Neovim

Complete OVSM LISP support for Neovim including LSP integration and Tree-sitter highlighting.

## Quick Setup

### Prerequisites

- Neovim 0.8+
- [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
- OVSM LSP server installed

### Install the LSP Server

```bash
cd osvm-cli
cargo build --release -p ovsm-lsp
sudo cp target/release/ovsm-lsp /usr/local/bin/
```

### Configure LSP (Lua)

Add to your `init.lua` or Lua config:

```lua
-- Register OVSM filetype
vim.filetype.add({
  extension = {
    ovsm = "ovsm",
  },
})

-- Configure OVSM LSP
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define OVSM language server config
if not configs.ovsm then
  configs.ovsm = {
    default_config = {
      cmd = { "ovsm-lsp" },
      filetypes = { "ovsm" },
      root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname)
          or lspconfig.util.path.dirname(fname)
      end,
      single_file_support = true,
      settings = {},
    },
    docs = {
      description = [[
OVSM Language Server

OVSM LISP language server for blockchain automation scripting.

Install:
  cargo install --path crates/ovsm-lsp

Or build from source:
  cargo build --release -p ovsm-lsp
  sudo cp target/release/ovsm-lsp /usr/local/bin/
]],
    },
  }
end

-- Enable OVSM LSP
lspconfig.ovsm.setup({
  on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings
    local opts = { buffer = bufnr }
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
  end,
  capabilities = require('cmp_nvim_lsp').default_capabilities(),
})
```

### Syntax Highlighting (Without Tree-sitter)

Create `~/.config/nvim/syntax/ovsm.vim`:

```vim
" Vim syntax file for OVSM LISP
if exists("b:current_syntax")
  finish
endif

" Comments
syn match ovsmComment ";.*$"

" Strings
syn region ovsmString start='"' end='"' skip='\\"'

" Numbers
syn match ovsmNumber "-\?\d\+\.\?\d*"

" Keywords (special forms)
syn keyword ovsmKeyword if define set! let let* lambda do progn while for
syn keyword ovsmKeyword when unless cond case defun defn const catch throw loop

" Built-in functions
syn keyword ovsmBuiltin length get first rest last cons append reverse range
syn keyword ovsmBuiltin mapcar map filter reduce sort keys values merge
syn keyword ovsmBuiltin concat split join trim upper lower replace
syn keyword ovsmBuiltin sqrt pow abs min max floor ceiling round
syn keyword ovsmBuiltin log print assert error now not and or
syn keyword ovsmBuiltin null? empty? evenp oddp zerop typeof

" Constants
syn keyword ovsmConstant true false null nil

" Keyword arguments
syn match ovsmKeywordArg ":[a-zA-Z_][a-zA-Z0-9_-]*"

" Brackets
syn match ovsmParen "[()]"
syn match ovsmBracket "[\[\]]"
syn match ovsmBrace "[{}]"

" Highlighting
hi def link ovsmComment Comment
hi def link ovsmString String
hi def link ovsmNumber Number
hi def link ovsmKeyword Keyword
hi def link ovsmBuiltin Function
hi def link ovsmConstant Constant
hi def link ovsmKeywordArg Special
hi def link ovsmParen Delimiter
hi def link ovsmBracket Delimiter
hi def link ovsmBrace Delimiter

let b:current_syntax = "ovsm"
```

### Tree-sitter Grammar (Advanced)

For advanced highlighting, you can use Tree-sitter. A tree-sitter grammar would need to be developed separately. For now, the LSP provides semantic token highlighting which enhances the basic syntax.

## Features

| Feature | Status |
|---------|--------|
| Syntax highlighting | ✅ Via syntax file or LSP semantic tokens |
| Real-time errors | ✅ LSP diagnostics |
| Hover documentation | ✅ Press `K` on any symbol |
| Auto-completion | ✅ Via omnifunc or nvim-cmp |
| Bracket matching | ✅ Built-in Neovim feature |

## Troubleshooting

### LSP Not Starting

Check if the server is in PATH:
```bash
which ovsm-lsp
```

Check LSP logs:
```vim
:LspLog
```

### No Syntax Highlighting

Ensure filetype detection works:
```vim
:set filetype?
" Should show: filetype=ovsm
```

## Example Config with lazy.nvim

```lua
-- In your lazy.nvim plugins spec
return {
  {
    'neovim/nvim-lspconfig',
    config = function()
      -- OVSM setup as shown above
    end,
  },
}
```
