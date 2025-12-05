# OSVM IDE TUI - Steering Document

**Command:** `osvm ide`
**Purpose:** VS Code-like terminal IDE with extension support
**Priority:** Medium-High - Ambitious, complex infrastructure

---

## Executive Summary

The IDE TUI brings a full-featured development environment to the terminal, inspired by VS Code but optimized for keyboard-driven workflows. The killer feature is **VS Code extension compatibility** through a custom extension host adapter, allowing users to leverage the vast VS Code extension ecosystem from their terminal.

---

## Design Philosophy

### Core Principles

1. **Keyboard-First** - Every action accessible via keyboard
2. **Extension-Compatible** - Run VS Code extensions where possible
3. **Performance** - Sub-100ms response times for all operations
4. **Memory Efficient** - Fraction of VS Code's memory footprint
5. **Offline-First** - Works without network (except extensions)

### What We're NOT Building

- A full Electron/web replacement
- UI-heavy extensions (themes, icons)
- GUI debugging (use DAP protocol instead)
- Extension marketplace browser (CLI install)

---

## User Stories

### Primary Users
1. **Remote Developers** - SSH sessions, servers, containers
2. **Vim/Emacs Users** - Want IDE features with terminal workflow
3. **Resource-Constrained** - Low-memory environments
4. **Terminal Purists** - Refuse to leave the terminal

### Key User Stories

| ID | Story | Priority |
|----|-------|----------|
| IDE-1 | As a developer, I want syntax highlighting for all major languages | P0 |
| IDE-2 | As a developer, I want code completion (LSP-powered) | P0 |
| IDE-3 | As a developer, I want to navigate files in a tree view | P0 |
| IDE-4 | As a developer, I want split panes for multiple files | P0 |
| IDE-5 | As a developer, I want integrated terminal | P0 |
| IDE-6 | As a developer, I want search across files (ripgrep) | P0 |
| IDE-7 | As a developer, I want Git integration | P0 |
| IDE-8 | As a developer, I want to install VS Code extensions | P1 |
| IDE-9 | As a developer, I want code actions (quick fixes) | P1 |
| IDE-10 | As a developer, I want go-to-definition and references | P1 |
| IDE-11 | As a developer, I want debugging support (DAP) | P2 |
| IDE-12 | As a developer, I want collaborative editing | P3 |

---

## Architecture

### High-Level Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            OSVM IDE TUI                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────┐  ┌─────────────────────────┐  ┌─────────────────────────┐ │
│  │   File      │  │      Editor Core        │  │     Extension Host      │ │
│  │   Explorer  │  │  ┌─────────────────┐    │  │  ┌─────────────────┐   │ │
│  │             │  │  │  Text Buffer    │    │  │  │  Node.js Host   │   │ │
│  │  ┌───────┐  │  │  │  Piece Table    │    │  │  │  (extensions)   │   │ │
│  │  │ Tree  │  │  │  └─────────────────┘    │  │  └────────┬────────┘   │ │
│  │  │ View  │  │  │  ┌─────────────────┐    │  │           │            │ │
│  │  └───────┘  │  │  │  Syntax Engine  │    │  │  ┌────────▼────────┐   │ │
│  │             │  │  │  Tree-sitter    │    │  │  │  Extension API  │   │ │
│  │             │  │  └─────────────────┘    │  │  │  Adapter        │   │ │
│  │             │  │  ┌─────────────────┐    │  │  └────────┬────────┘   │ │
│  └─────────────┘  │  │  LSP Client     │◀───┼──┼──────────▶│            │ │
│                   │  │  (completions)  │    │  │           │            │ │
│  ┌─────────────┐  │  └─────────────────┘    │  │  ┌────────▼────────┐   │ │
│  │   Search    │  │                         │  │  │  Message Bus    │   │ │
│  │   Results   │  │  ┌─────────────────┐    │  │  │  (JSON-RPC)     │   │ │
│  │             │  │  │  View Layer     │    │  │  └─────────────────┘   │ │
│  └─────────────┘  │  │  Split Panes    │    │  │                        │ │
│                   │  └─────────────────┘    │  │                        │ │
│  ┌─────────────┐  │                         │  │                        │ │
│  │  Terminal   │  └─────────────────────────┘  └─────────────────────────┘ │
│  │  Emulator   │                                                           │
│  └─────────────┘                                                           │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                            Status Bar                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Component Hierarchy

```
IdeApp
├── TitleBar
│   ├── MenuBar (File, Edit, View, Go, Terminal, Help)
│   └── Breadcrumbs
├── SideBar
│   ├── ActivityBar
│   │   ├── Explorer (files)
│   │   ├── Search
│   │   ├── SourceControl (git)
│   │   ├── Extensions
│   │   └── CustomViews (from extensions)
│   └── SideBarContent
│       ├── FileTree
│       ├── SearchPanel
│       ├── GitPanel
│       └── ExtensionsPanel
├── EditorArea
│   ├── TabBar
│   │   ├── Tab (per open file)
│   │   └── TabActions (close, split)
│   ├── EditorGroup (supports splits)
│   │   ├── Editor
│   │   │   ├── LineNumbers
│   │   │   ├── TextContent
│   │   │   ├── Minimap (optional)
│   │   │   ├── CompletionPopup
│   │   │   └── HoverInfo
│   │   └── EditorGroup (nested for splits)
│   └── EditorWidgets
│       ├── FindReplace
│       └── PeekView
├── Panel (Bottom)
│   ├── PanelTabs
│   │   ├── Terminal
│   │   ├── Problems
│   │   ├── Output
│   │   └── DebugConsole
│   └── PanelContent
│       ├── TerminalEmulator
│       ├── DiagnosticsList
│       └── OutputChannel
├── StatusBar
│   ├── BranchName
│   ├── Diagnostics (errors/warnings)
│   ├── CursorPosition
│   ├── Encoding
│   ├── LineEnding
│   ├── Language
│   └── Notifications
└── Overlays
    ├── CommandPalette
    ├── QuickOpen
    ├── SymbolPicker
    └── Notifications
```

---

## Extension System Architecture

### VS Code Extension Compatibility

The key innovation is a **compatibility layer** that allows many VS Code extensions to run in our terminal IDE. This works by:

1. **Headless Extension Host** - Node.js process running extensions
2. **API Shim** - Implements VS Code API subset
3. **UI Adaptation** - Converts GUI operations to TUI equivalents

### Extension Host Design

```
┌─────────────────────────────────────────────────────────────────┐
│                    Extension Host (Node.js)                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    Extension Sandbox                         ││
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       ││
│  │  │  Extension A │  │  Extension B │  │  Extension C │       ││
│  │  │  (Rust Anl.) │  │  (Prettier)  │  │  (GitLens)   │       ││
│  │  └──────────────┘  └──────────────┘  └──────────────┘       ││
│  └─────────────────────────────────────────────────────────────┘│
│                              │                                   │
│                              ▼                                   │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    VS Code API Shim                         ││
│  │                                                              ││
│  │  vscode.window      → TUI window adapter                    ││
│  │  vscode.workspace   → File system adapter                   ││
│  │  vscode.languages   → LSP bridge                            ││
│  │  vscode.commands    → Command registry                      ││
│  │  vscode.extensions  → Extension management                  ││
│  │                                                              ││
│  └─────────────────────────────────────────────────────────────┘│
│                              │                                   │
│                              ▼                                   │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │               JSON-RPC Message Bridge                        ││
│  │          (stdio / Unix socket / WebSocket)                   ││
│  └─────────────────────────────────────────────────────────────┘│
│                                                                  │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │   OSVM IDE (Rust)     │
                    │   Main Process        │
                    └───────────────────────┘
```

### Supported VS Code APIs

| API | Support Level | Notes |
|-----|--------------|-------|
| `vscode.workspace` | High | File operations, settings |
| `vscode.window` | Partial | No webviews, adapted UI |
| `vscode.languages` | High | Full LSP support |
| `vscode.commands` | Full | Command registration |
| `vscode.env` | Partial | Environment info |
| `vscode.extensions` | Full | Extension management |
| `vscode.debug` | Medium | DAP protocol |
| `vscode.scm` | Medium | Git operations |
| `vscode.tasks` | Low | Basic task running |
| `vscode.notebook` | None | Not supported |
| `vscode.webview` | None | No GUI |
| `vscode.tests` | Medium | Test discovery/run |

### Extension Compatibility Tiers

| Tier | Description | Examples |
|------|-------------|----------|
| **Full** | Works without modification | Prettier, ESLint, rust-analyzer |
| **Adapted** | Minor UI differences | GitLens (no blame annotations) |
| **Partial** | Core features only | GitHub Copilot (suggestions only) |
| **Unsupported** | GUI-dependent | Themes, icon packs, webview-based |

---

## State Management

### Core State Structure

```rust
pub struct IdeState {
    // Workspace
    pub workspace_root: PathBuf,
    pub open_files: Vec<OpenFile>,
    pub active_editor: Option<usize>,
    pub editor_groups: EditorLayout,

    // Editor state
    pub buffers: HashMap<PathBuf, TextBuffer>,
    pub cursors: HashMap<PathBuf, CursorState>,
    pub selections: HashMap<PathBuf, Vec<Selection>>,

    // Sidebar
    pub sidebar_visible: bool,
    pub active_sidebar: SidebarPanel,
    pub file_tree: FileTree,
    pub search_state: SearchState,
    pub git_state: GitState,

    // Panel
    pub panel_visible: bool,
    pub active_panel: PanelType,
    pub terminals: Vec<TerminalState>,
    pub diagnostics: Vec<Diagnostic>,

    // LSP
    pub lsp_clients: HashMap<String, LspClient>,
    pub completions: Option<CompletionState>,
    pub hover_info: Option<HoverInfo>,

    // Extensions
    pub extension_host: Option<ExtensionHost>,
    pub installed_extensions: Vec<ExtensionInfo>,

    // UI
    pub command_palette_open: bool,
    pub quick_open_open: bool,
    pub focus: IdeFocus,
}

pub struct TextBuffer {
    pub content: Rope,  // Using xi-rope or similar
    pub syntax_tree: Option<Tree>,  // Tree-sitter
    pub language: String,
    pub modified: bool,
    pub version: u64,
    pub undo_stack: Vec<Edit>,
    pub redo_stack: Vec<Edit>,
}

pub struct EditorLayout {
    pub root: EditorGroupNode,
}

pub enum EditorGroupNode {
    Leaf {
        tabs: Vec<PathBuf>,
        active: usize,
    },
    Split {
        direction: SplitDirection,
        children: Vec<EditorGroupNode>,
        sizes: Vec<f32>,
    },
}

pub enum SplitDirection {
    Horizontal,
    Vertical,
}

pub struct CursorState {
    pub line: usize,
    pub column: usize,
    pub desired_column: usize,  // For vertical movement
    pub selection_anchor: Option<(usize, usize)>,
}

pub struct CompletionState {
    pub items: Vec<CompletionItem>,
    pub selected: usize,
    pub filter: String,
    pub trigger_position: (usize, usize),
}
```

---

## Editor Core

### Text Buffer (Piece Table / Rope)

```rust
/// Efficient text buffer using rope data structure
pub struct TextBuffer {
    rope: Rope,
    line_cache: LineCache,
}

impl TextBuffer {
    pub fn new(content: &str) -> Self;
    pub fn insert(&mut self, pos: usize, text: &str);
    pub fn delete(&mut self, range: Range<usize>);
    pub fn line(&self, line_num: usize) -> Option<&str>;
    pub fn line_count(&self) -> usize;
    pub fn char_count(&self) -> usize;

    // Efficient operations
    pub fn slice(&self, range: Range<usize>) -> RopeSlice;
    pub fn lines_in_range(&self, range: Range<usize>) -> impl Iterator<Item = &str>;
}
```

### Syntax Highlighting (Tree-sitter)

```rust
pub struct SyntaxEngine {
    parsers: HashMap<String, Parser>,
    queries: HashMap<String, Query>,
}

impl SyntaxEngine {
    pub fn parse(&mut self, language: &str, source: &str) -> Option<Tree>;

    pub fn highlight(
        &self,
        language: &str,
        tree: &Tree,
        source: &str,
        range: Range<usize>,
    ) -> Vec<HighlightSpan>;

    pub fn get_scope_at(&self, tree: &Tree, position: Point) -> Vec<String>;
}

#[derive(Clone)]
pub struct HighlightSpan {
    pub start: usize,
    pub end: usize,
    pub highlight_type: HighlightType,
}

#[derive(Clone, Copy)]
pub enum HighlightType {
    Keyword,
    String,
    Number,
    Comment,
    Function,
    Type,
    Variable,
    Operator,
    Punctuation,
    // ... more
}
```

### LSP Integration

```rust
pub struct LspManager {
    clients: HashMap<String, LspClient>,
    pending_requests: HashMap<RequestId, PendingRequest>,
}

impl LspManager {
    pub async fn start_server(&mut self, language: &str, command: &str) -> Result<()>;

    pub async fn completion(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Result<Vec<CompletionItem>>;

    pub async fn hover(&self, uri: &Uri, position: Position) -> Result<Option<Hover>>;

    pub async fn goto_definition(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Result<Vec<Location>>;

    pub async fn find_references(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Result<Vec<Location>>;

    pub async fn code_actions(
        &self,
        uri: &Uri,
        range: Range,
        diagnostics: &[Diagnostic],
    ) -> Result<Vec<CodeAction>>;

    pub async fn format_document(&self, uri: &Uri) -> Result<Vec<TextEdit>>;

    pub fn handle_notification(&mut self, notification: Notification);
}
```

---

## Keyboard Shortcuts

### Editor (Normal Mode)

| Key | Action |
|-----|--------|
| `Ctrl+P` | Quick open (file picker) |
| `Ctrl+Shift+P` | Command palette |
| `Ctrl+G` | Go to line |
| `Ctrl+Shift+O` | Go to symbol |
| `F12` | Go to definition |
| `Shift+F12` | Find references |
| `Ctrl+.` | Quick fix / code actions |
| `Ctrl+Space` | Trigger completion |
| `Ctrl+/` | Toggle comment |
| `Ctrl+D` | Select word / next occurrence |
| `Ctrl+Shift+K` | Delete line |
| `Alt+↑/↓` | Move line up/down |
| `Ctrl+Shift+[/]` | Fold/unfold |

### Navigation

| Key | Action |
|-----|--------|
| `Ctrl+Tab` | Switch editor |
| `Ctrl+1-9` | Go to editor group |
| `Ctrl+\` | Split editor |
| `Ctrl+W` | Close editor |
| `Ctrl+B` | Toggle sidebar |
| `Ctrl+J` | Toggle panel |
| `Ctrl+`` ` | Toggle terminal |

### File Operations

| Key | Action |
|-----|--------|
| `Ctrl+N` | New file |
| `Ctrl+O` | Open file |
| `Ctrl+S` | Save |
| `Ctrl+Shift+S` | Save as |
| `Ctrl+Shift+N` | New window |

### Search

| Key | Action |
|-----|--------|
| `Ctrl+F` | Find in file |
| `Ctrl+H` | Find and replace |
| `Ctrl+Shift+F` | Find in files |
| `Ctrl+Shift+H` | Replace in files |

### Optional: Vim Mode

| Key | Action |
|-----|--------|
| `i` | Insert mode |
| `Esc` | Normal mode |
| `v` | Visual mode |
| `V` | Visual line mode |
| `:` | Command mode |
| Full vim bindings... | ... |

---

## UI Mockups

### Main IDE View

```
┌─ OSVM IDE ─ ~/projects/my-app ──────────────────────────────────────────────┐
│ File  Edit  View  Go  Terminal  Help                     src/main.rs        │
├───────────────────────────────────────────────────────────────────────────────┤
││▶ │ EXPLORER                │ main.rs × │ lib.rs │ Cargo.toml │            ││
├│  │─────────────────────────┼───────────────────────────────────────────────┤│
││▼ │ ▼ my-app                │  1│ use std::io;                              ││
││  │   ▼ src                 │  2│ use tokio::runtime::Runtime;              ││
││  │     ▶ main.rs           │  3│                                           ││
││  │     ▶ lib.rs            │  4│ mod config;                               ││
││  │     ▼ utils             │  5│ mod server;                               ││
││  │       ▶ helpers.rs      │  6│                                           ││
││  │       ▶ crypto.rs       │  7│ fn main() -> io::Result<()> {             ││
││🔍│   ▼ tests               │  8│     let rt = Runtime::new()?;             ││
││  │     ▶ integration.rs    │  9│     rt.block_on(async {                   ││
││  │   ▶ Cargo.toml          │ 10│         server::run().await               ││
││  │   ▶ README.md           │ 11│     })                                    ││
││🌿│                         │ 12│ }                                         ││
││  │                         │ 13│                                           ││
││⚙ │                         │ 14│ #[cfg(test)]                              ││
││  │                         │ 15│ mod tests {                               ││
││  │                         │ 16│     use super::*;                         ││
││  │                         │ 17│                                           ││
││  │                         │ 18│     #[test]                               ││
││  │                         │ 19│     fn test_main() {                      ││
││  │                         │ 20│         assert!(true);                    ││
││  │                         │ 21│     }                                     ││
││  │                         │ 22│ }                                         ││
│├──┴─────────────────────────┼───────────────────────────────────────────────┤│
││ TERMINAL                                                                   ││
│├────────────────────────────────────────────────────────────────────────────┤│
││ ~/projects/my-app $ cargo build                                            ││
││    Compiling my-app v0.1.0                                                 ││
││     Finished dev [unoptimized + debuginfo] target(s) in 2.34s              ││
││ ~/projects/my-app $ █                                                      ││
│└────────────────────────────────────────────────────────────────────────────┘│
├───────────────────────────────────────────────────────────────────────────────┤
│ main │ ✓ 0 ✗ 0 │ Ln 7, Col 23 │ UTF-8 │ LF │ Rust │ rust-analyzer ●        │
└───────────────────────────────────────────────────────────────────────────────┘
```

### Command Palette

```
┌─ Command Palette ───────────────────────────────────────────────────────────┐
│ > format                                                                    │
├─────────────────────────────────────────────────────────────────────────────┤
│ ▶ Format Document                                      Ctrl+Shift+I        │
│   Format Selection                                     Ctrl+K Ctrl+F       │
│   Format Document With...                                                   │
│   Format On Save: Toggle                                                    │
│   Preferences: Open Settings (format)                                       │
├─────────────────────────────────────────────────────────────────────────────┤
│ recently used                                                               │
│   File: Save                                           Ctrl+S              │
│   View: Toggle Terminal                                Ctrl+`              │
│   Git: Commit                                                               │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Quick Open (File Picker)

```
┌─ Go to File ────────────────────────────────────────────────────────────────┐
│ main                                                                        │
├─────────────────────────────────────────────────────────────────────────────┤
│ ▶ main.rs                                              src/main.rs         │
│   main_test.rs                                         tests/main_test.rs  │
│   maintenance.rs                                       src/utils/maint...  │
│   domain.rs                                            src/models/domain.. │
├─────────────────────────────────────────────────────────────────────────────┤
│ history                                                                     │
│   lib.rs                                               src/lib.rs          │
│   Cargo.toml                                           Cargo.toml          │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Completion Popup

```
│  7│ fn main() -> io::Result<()> {
│  8│     let config = Config::
│   │                         ┌──────────────────────────────────────┐
│   │                         │ ▶ new()            fn() -> Config    │
│   │                         │   from_file(path)  fn(&str) -> ...   │
│   │                         │   default()        fn() -> Config    │
│   │                         │   builder()        fn() -> Builder   │
│   │                         │   validate(&self)  fn(&Self) -> ...  │
│   │                         └──────────────────────────────────────┘
│  9│     let rt = Runtime::new()?;
```

### Split View

```
┌───────────────────────────────────┬─────────────────────────────────────────┐
│ main.rs ×                         │ lib.rs ×                                │
├───────────────────────────────────┼─────────────────────────────────────────┤
│  1│ use std::io;                  │  1│ //! Library crate for my-app       │
│  2│ use tokio::runtime::Runtime;  │  2│                                    │
│  3│                               │  3│ pub mod config;                    │
│  4│ mod config;                   │  4│ pub mod server;                    │
│  5│ mod server;                   │  5│ pub mod utils;                     │
│  6│                               │  6│                                    │
│  7│ fn main() -> io::Result<()> { │  7│ pub use config::Config;            │
│  8│     let rt = Runtime::new()?; │  8│ pub use server::Server;            │
│  9│     rt.block_on(async {       │  9│                                    │
│ 10│         server::run().await   │ 10│ /// Re-export common utilities     │
│ 11│     })                        │ 11│ pub mod prelude {                  │
│ 12│ }                             │ 12│     pub use crate::config::*;      │
│                                   │ 13│     pub use crate::server::*;      │
└───────────────────────────────────┴─────────────────────────────────────────┘
```

---

## Implementation Plan

### Phase 1: Core Editor (Week 1-3)

| Task | Description | Files |
|------|-------------|-------|
| 1.1 | Text buffer with rope | `src/utils/tui/ide/buffer.rs` |
| 1.2 | Basic editor view | `src/utils/tui/ide/views/editor.rs` |
| 1.3 | Cursor and selection | `src/utils/tui/ide/cursor.rs` |
| 1.4 | File tree sidebar | `src/utils/tui/ide/views/filetree.rs` |
| 1.5 | Tab bar and editor groups | `src/utils/tui/ide/views/tabs.rs` |
| 1.6 | Basic keyboard navigation | `src/utils/tui/ide/input.rs` |

### Phase 2: Syntax & LSP (Week 4-6)

| Task | Description | Files |
|------|-------------|-------|
| 2.1 | Tree-sitter integration | `src/utils/tui/ide/syntax.rs` |
| 2.2 | Syntax highlighting render | `src/utils/tui/ide/highlight.rs` |
| 2.3 | LSP client manager | `src/utils/tui/ide/lsp/manager.rs` |
| 2.4 | Completion popup | `src/utils/tui/ide/views/completion.rs` |
| 2.5 | Hover information | `src/utils/tui/ide/views/hover.rs` |
| 2.6 | Go to definition | Navigation integration |

### Phase 3: IDE Features (Week 7-9)

| Task | Description | Files |
|------|-------------|-------|
| 3.1 | Command palette | `src/utils/tui/ide/views/palette.rs` |
| 3.2 | Quick open (fuzzy finder) | `src/utils/tui/ide/views/quickopen.rs` |
| 3.3 | Search in files | `src/utils/tui/ide/search.rs` |
| 3.4 | Integrated terminal | `src/utils/tui/ide/terminal.rs` |
| 3.5 | Git integration | `src/utils/tui/ide/git.rs` |
| 3.6 | Problems panel | `src/utils/tui/ide/views/problems.rs` |

### Phase 4: Extension System (Week 10-14)

| Task | Description | Files |
|------|-------------|-------|
| 4.1 | Extension host (Node.js) | `extension-host/` (separate) |
| 4.2 | VS Code API shim | `extension-host/api/` |
| 4.3 | JSON-RPC bridge | `src/utils/tui/ide/extension/bridge.rs` |
| 4.4 | Extension installer | `src/utils/tui/ide/extension/install.rs` |
| 4.5 | Extension settings | `src/utils/tui/ide/extension/settings.rs` |
| 4.6 | Extension testing | Integration tests |

### Phase 5: Polish (Week 15-16)

| Task | Description | Files |
|------|-------------|-------|
| 5.1 | Vim mode (optional) | `src/utils/tui/ide/vim.rs` |
| 5.2 | Settings UI | `src/utils/tui/ide/views/settings.rs` |
| 5.3 | Debug adapter protocol | `src/utils/tui/ide/debug.rs` |
| 5.4 | Performance optimization | Across modules |
| 5.5 | Documentation | `docs/IDE_USER_GUIDE.md` |

---

## File Structure

```
src/utils/tui/ide/
├── mod.rs
├── app.rs                  # Main IdeApp state
├── buffer.rs               # Text buffer (rope)
├── cursor.rs               # Cursor/selection
├── syntax.rs               # Tree-sitter integration
├── highlight.rs            # Syntax highlighting
├── input.rs                # Keyboard handling
├── vim.rs                  # Vim emulation (optional)
├── lsp/
│   ├── mod.rs
│   ├── manager.rs          # LSP client manager
│   ├── client.rs           # Individual LSP client
│   └── protocol.rs         # LSP types
├── extension/
│   ├── mod.rs
│   ├── host.rs             # Extension host manager
│   ├── bridge.rs           # JSON-RPC bridge
│   ├── api.rs              # API type definitions
│   ├── install.rs          # Extension installation
│   └── registry.rs         # Extension registry
├── views/
│   ├── mod.rs
│   ├── editor.rs           # Main editor view
│   ├── filetree.rs         # File explorer
│   ├── tabs.rs             # Tab bar
│   ├── completion.rs       # Autocomplete popup
│   ├── hover.rs            # Hover information
│   ├── palette.rs          # Command palette
│   ├── quickopen.rs        # File picker
│   ├── search.rs           # Search results
│   ├── terminal.rs         # Terminal panel
│   ├── problems.rs         # Diagnostics panel
│   ├── git.rs              # Git panel
│   └── settings.rs         # Settings editor
├── search.rs               # Search engine (ripgrep)
├── git.rs                  # Git operations
├── terminal.rs             # Terminal emulation
├── debug.rs                # DAP integration
└── tests.rs

# Extension host (separate Node.js project)
extension-host/
├── package.json
├── src/
│   ├── index.ts            # Main entry
│   ├── host.ts             # Extension loading
│   ├── api/
│   │   ├── vscode.ts       # Main API shim
│   │   ├── window.ts       # vscode.window
│   │   ├── workspace.ts    # vscode.workspace
│   │   ├── languages.ts    # vscode.languages
│   │   └── commands.ts     # vscode.commands
│   └── bridge/
│       ├── jsonrpc.ts      # Message protocol
│       └── adapter.ts      # UI adaptation
└── tsconfig.json
```

---

## Performance Targets

| Metric | Target |
|--------|--------|
| Startup time | < 500ms |
| File open | < 100ms (for < 1MB files) |
| Keystroke latency | < 16ms |
| Completion popup | < 100ms |
| Memory per 1MB file | < 5MB |
| Total memory (baseline) | < 100MB |
| Extension host memory | < 200MB |

---

## Dependencies

### Core
- `ratatui`, `crossterm` - TUI framework
- `ropey` - Efficient text buffer
- `tree-sitter` - Syntax parsing
- `tower-lsp` - LSP client
- `tokio` - Async runtime
- `portable-pty` - Terminal emulation

### Extension Host (Node.js)
- `typescript` - Type safety
- `vscode` types - API types
- `jsonrpc` - Message protocol

---

## CLI Integration

```rust
// src/clparse.rs addition
#[derive(Subcommand)]
pub enum Commands {
    /// Open terminal IDE
    Ide {
        /// File or directory to open
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Open in diff mode
        #[arg(long)]
        diff: Option<PathBuf>,

        /// Disable extension host
        #[arg(long)]
        no_extensions: bool,

        /// Vim keybindings
        #[arg(long)]
        vim: bool,
    },
}
```

---

## Security Considerations

### Extension Sandboxing

1. **Process Isolation** - Extensions run in separate Node process
2. **Permission Model** - Extensions declare required permissions
3. **Network Restrictions** - Configurable network access
4. **File System Scope** - Limit to workspace by default

### Data Safety

1. **Autosave** - Configurable autosave interval
2. **Crash Recovery** - Save state for recovery
3. **Backup Files** - Optional backup on save

---

## Open Questions

1. **Extension Marketplace** - Build custom or use Open VSX?
2. **Remote Development** - SSH editing support priority?
3. **Collaborative Editing** - OT/CRDT implementation?
4. **Mobile/Tablet** - Termux support considerations?
5. **Debugger Priority** - Which DAP adapters first?
6. **Vim vs VSCode Bindings** - Default keybinding set?
