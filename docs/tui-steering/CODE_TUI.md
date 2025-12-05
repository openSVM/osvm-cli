# OSVM Code TUI - Steering Document

**Command:** `osvm code <prompt>`
**Purpose:** Claude Code clone - AI-powered coding assistant in ratatui
**Priority:** High - Core differentiating feature

---

## Executive Summary

OSVM Code brings the Claude Code experience to a native terminal interface built with ratatui. It provides an AI-powered coding assistant that can read, write, and modify code across your entire project, execute commands, search the web, and maintain context across long coding sessions—all through a beautiful, responsive TUI.

---

## Design Philosophy

### Core Principles

1. **Agentic** - AI can autonomously explore, plan, and execute
2. **Transparent** - Show every tool call and decision
3. **Controllable** - User approval for dangerous operations
4. **Contextual** - Maintain deep understanding of the codebase
5. **Performant** - Instant feedback, streaming responses

### What Makes This Different from Chat

| Feature | Basic Chat | OSVM Code |
|---------|-----------|-----------|
| Context | Current conversation | Entire codebase |
| Actions | Suggestions only | Direct file editing |
| Commands | Manual copy-paste | Autonomous execution |
| Planning | Single response | Multi-step reasoning |
| Memory | Session only | Persistent memory |

---

## User Stories

### Primary Users
1. **Developers** - Want AI assistance for coding tasks
2. **Teams** - Need consistent code generation
3. **Learners** - Want explanations alongside code
4. **Maintainers** - Need help understanding legacy code

### Key User Stories

| ID | Story | Priority |
|----|-------|----------|
| CODE-1 | As a developer, I want to describe a feature and have AI implement it | P0 |
| CODE-2 | As a developer, I want AI to read and understand my codebase | P0 |
| CODE-3 | As a developer, I want AI to edit existing files safely | P0 |
| CODE-4 | As a developer, I want to see AI's thought process | P0 |
| CODE-5 | As a developer, I want AI to run commands for me | P0 |
| CODE-6 | As a developer, I want to approve/reject proposed changes | P0 |
| CODE-7 | As a developer, I want AI to search the web for solutions | P1 |
| CODE-8 | As a developer, I want AI to run and fix failing tests | P1 |
| CODE-9 | As a developer, I want to maintain context across sessions | P1 |
| CODE-10 | As a developer, I want AI to create PRs and commits | P2 |
| CODE-11 | As a developer, I want custom tool definitions | P2 |
| CODE-12 | As a developer, I want MCP server integration | P2 |

---

## Architecture

### High-Level Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           OSVM Code TUI                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │                    Conversation Interface                                ││
│  │  ┌─────────────────────────────────────────────────────────────────────┐││
│  │  │  Message Stream                                                     │││
│  │  │  - User prompts                                                     │││
│  │  │  - AI responses (streaming)                                         │││
│  │  │  - Tool calls (with status)                                         │││
│  │  │  - File diffs                                                       │││
│  │  │  - Command output                                                   │││
│  │  └─────────────────────────────────────────────────────────────────────┘││
│  │  ┌─────────────────────────────────────────────────────────────────────┐││
│  │  │  Input Area (multi-line)                                            │││
│  │  └─────────────────────────────────────────────────────────────────────┘││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                                                                             │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────────────┐ │
│  │   Tool Engine   │  │   Context Engine │  │    Permission System       │ │
│  │                 │  │                  │  │                            │ │
│  │  - Read files   │  │  - File index    │  │  - Auto-approve (safe)     │ │
│  │  - Write files  │  │  - Symbol table  │  │  - Prompt (risky)          │ │
│  │  - Run commands │  │  - Conversation  │  │  - Deny (dangerous)        │ │
│  │  - Search       │  │  - Memory store  │  │                            │ │
│  │  - Web fetch    │  │                  │  │                            │ │
│  └────────┬────────┘  └────────┬─────────┘  └─────────────┬──────────────┘ │
│           │                    │                          │                 │
│           └────────────────────┼──────────────────────────┘                 │
│                                │                                            │
│                    ┌───────────▼───────────┐                               │
│                    │      AI Backend       │                               │
│                    │  (Claude API / Local) │                               │
│                    └───────────────────────┘                               │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Component Hierarchy

```
CodeApp
├── HeaderBar
│   ├── ProjectInfo (name, git branch)
│   ├── ModelSelector (claude-3, local models)
│   ├── TokenUsage (input/output tokens)
│   └── ConnectionStatus
├── MainArea
│   ├── ConversationView
│   │   ├── MessageList
│   │   │   ├── UserMessage
│   │   │   ├── AssistantMessage (streaming)
│   │   │   │   ├── TextContent
│   │   │   │   ├── ThinkingBlock (collapsible)
│   │   │   │   ├── ToolCallBlock
│   │   │   │   │   ├── ToolName
│   │   │   │   │   ├── ToolInput (preview)
│   │   │   │   │   ├── ToolStatus (pending/running/success/error)
│   │   │   │   │   └── ToolOutput (collapsible)
│   │   │   │   ├── CodeBlock (syntax highlighted)
│   │   │   │   └── DiffView
│   │   │   └── SystemMessage (errors, notices)
│   │   └── ScrollControls
│   └── SidePanel (optional)
│       ├── ContextFiles (open files)
│       ├── TodoList (from AI)
│       └── GitChanges
├── InputArea
│   ├── MultiLineInput
│   │   ├── TextEditor (mini)
│   │   └── CharacterCount
│   ├── QuickActions
│   │   ├── AddContext (@file)
│   │   ├── InsertCode (```)
│   │   └── VoiceInput (future)
│   └── SendButton
├── ApprovalModal (when needed)
│   ├── ProposedAction
│   ├── RiskAssessment
│   └── ApproveRejectButtons
├── StatusBar
│   ├── CurrentAction
│   ├── APIStatus
│   ├── ContextSize
│   └── KeyboardHints
└── Overlays
    ├── HelpOverlay
    ├── SettingsOverlay
    └── FilePreviewModal
```

---

## Tool System

### Built-in Tools

```rust
/// Core tool trait for all tools
#[async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn parameters(&self) -> serde_json::Value;  // JSON Schema

    async fn execute(
        &self,
        params: serde_json::Value,
        context: &ToolContext,
    ) -> Result<ToolResult>;

    fn risk_level(&self) -> RiskLevel;
    fn requires_approval(&self, params: &serde_json::Value) -> bool;
}

#[derive(Clone, Copy)]
pub enum RiskLevel {
    Safe,       // Read-only operations
    Low,        // Reversible writes
    Medium,     // Potentially destructive
    High,       // System-level changes
    Critical,   // Irreversible/dangerous
}
```

### Tool Definitions

```rust
// File Operations
pub struct ReadTool;       // Read file contents
pub struct WriteTool;      // Create/overwrite files
pub struct EditTool;       // Surgical string replacement
pub struct GlobTool;       // Find files by pattern
pub struct GrepTool;       // Search file contents

// Command Execution
pub struct BashTool;       // Run shell commands
pub struct BashOutputTool; // Get output from background process

// Navigation & Context
pub struct ListDirTool;    // List directory contents
pub struct TreeTool;       // Directory tree view

// Git Operations
pub struct GitStatusTool;
pub struct GitDiffTool;
pub struct GitCommitTool;
pub struct GitLogTool;

// Web & Search
pub struct WebSearchTool;  // Search the web
pub struct WebFetchTool;   // Fetch URL content

// Special
pub struct AskUserTool;    // Ask user for input
pub struct TodoWriteTool;  // Track task progress
pub struct MCPTool;        // Call MCP server tools
```

### Tool Execution Flow

```
User Request
     │
     ▼
┌──────────────┐
│   AI Model   │ ───▶ "I need to read main.rs"
└──────────────┘
     │
     ▼ tool_call(read, {path: "src/main.rs"})
┌──────────────┐
│ Permission   │ ───▶ RiskLevel::Safe → Auto-approve
│   Check      │
└──────────────┘
     │
     ▼
┌──────────────┐
│   Execute    │ ───▶ Read file contents
│    Tool      │
└──────────────┘
     │
     ▼
┌──────────────┐
│   AI Model   │ ───▶ "Now I'll edit line 42..."
└──────────────┘
     │
     ▼ tool_call(edit, {path: "...", old: "...", new: "..."})
┌──────────────┐
│ Permission   │ ───▶ RiskLevel::Low → Show diff, approve?
│   Check      │
└──────────────┘
     │
     ▼ (User approves)
┌──────────────┐
│   Execute    │ ───▶ Apply edit
│    Tool      │
└──────────────┘
```

---

## State Management

### Core State Structure

```rust
pub struct CodeState {
    // Conversation
    pub messages: Vec<Message>,
    pub current_input: String,
    pub input_mode: InputMode,
    pub scroll_position: usize,

    // AI State
    pub streaming: bool,
    pub current_response: StreamingResponse,
    pub pending_tool_calls: Vec<PendingToolCall>,
    pub tool_results: HashMap<String, ToolResult>,

    // Context
    pub project_root: PathBuf,
    pub context_files: Vec<ContextFile>,
    pub file_index: FileIndex,
    pub conversation_context: ConversationContext,

    // Settings
    pub model: ModelConfig,
    pub permissions: PermissionConfig,
    pub auto_approve: HashSet<String>,  // Tool names

    // UI
    pub focus: CodeFocus,
    pub expanded_blocks: HashSet<BlockId>,
    pub approval_modal: Option<ApprovalRequest>,
}

pub struct Message {
    pub id: MessageId,
    pub role: Role,
    pub content: MessageContent,
    pub timestamp: DateTime<Utc>,
    pub token_count: Option<usize>,
}

pub enum MessageContent {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

pub enum ContentBlock {
    Text(String),
    Thinking {
        content: String,
        collapsed: bool,
    },
    ToolCall {
        id: String,
        name: String,
        input: serde_json::Value,
        status: ToolStatus,
        output: Option<String>,
        collapsed: bool,
    },
    Code {
        language: String,
        content: String,
    },
    Diff {
        path: PathBuf,
        hunks: Vec<DiffHunk>,
    },
    Image {
        // For future multimodal support
        description: String,
    },
}

pub enum ToolStatus {
    Pending,
    AwaitingApproval,
    Running,
    Success,
    Error(String),
    Cancelled,
}

pub struct StreamingResponse {
    pub message_id: MessageId,
    pub content: String,
    pub tool_calls: Vec<ToolCall>,
    pub done: bool,
}

pub struct ApprovalRequest {
    pub tool_name: String,
    pub tool_input: serde_json::Value,
    pub risk_level: RiskLevel,
    pub explanation: String,
    pub preview: Option<String>,  // e.g., diff preview
}

pub struct ConversationContext {
    pub system_prompt: String,
    pub project_info: ProjectInfo,
    pub recent_files: Vec<PathBuf>,
    pub environment: EnvironmentInfo,
}
```

---

## AI Backend Integration

### Anthropic API (Claude)

```rust
pub struct ClaudeBackend {
    client: reqwest::Client,
    api_key: String,
    model: String,
}

impl ClaudeBackend {
    pub async fn stream_response(
        &self,
        messages: &[ApiMessage],
        tools: &[ToolDefinition],
        system: &str,
    ) -> impl Stream<Item = Result<StreamEvent>> {
        // Server-sent events streaming
    }

    pub async fn complete(
        &self,
        messages: &[ApiMessage],
        tools: &[ToolDefinition],
        system: &str,
    ) -> Result<Response>;
}

pub enum StreamEvent {
    MessageStart { id: String },
    ContentBlockStart { index: usize, content_type: String },
    ContentBlockDelta { index: usize, delta: Delta },
    ContentBlockStop { index: usize },
    MessageDelta { stop_reason: Option<String> },
    MessageStop,
}

pub enum Delta {
    TextDelta { text: String },
    ThinkingDelta { thinking: String },
    InputJsonDelta { partial_json: String },
}
```

### Local Model Support (Future)

```rust
pub trait AiBackend: Send + Sync {
    async fn stream_response(
        &self,
        messages: &[ApiMessage],
        tools: &[ToolDefinition],
        system: &str,
    ) -> Pin<Box<dyn Stream<Item = Result<StreamEvent>>>>;
}

// Future implementations
pub struct OllamaBackend { /* ... */ }
pub struct LlamaCppBackend { /* ... */ }
pub struct OpenAIBackend { /* ... */ }  // Compatible API
```

---

## Keyboard Shortcuts

### Global

| Key | Action |
|-----|--------|
| `Ctrl+C` | Interrupt current operation |
| `Ctrl+D` | Exit (if input empty) |
| `Ctrl+L` | Clear screen |
| `Escape` | Cancel / Close modal |
| `?` | Help overlay |
| `Ctrl+,` | Settings |

### Input Area

| Key | Action |
|-----|--------|
| `Enter` | Send message (single line mode) |
| `Shift+Enter` | New line |
| `Ctrl+Enter` | Send message (multi-line mode) |
| `↑/↓` | Navigate history |
| `Tab` | Autocomplete @mentions |
| `Ctrl+U` | Clear input |
| `Ctrl+V` | Paste |

### Conversation View

| Key | Action |
|-----|--------|
| `↑/↓` / `j/k` | Scroll |
| `Page Up/Down` | Page scroll |
| `Home/End` | Top/bottom |
| `Enter` | Expand/collapse block |
| `c` | Copy selected block |
| `o` | Open file at cursor |
| `d` | Show full diff |

### Approval Modal

| Key | Action |
|-----|--------|
| `y` / `Enter` | Approve |
| `n` / `Escape` | Reject |
| `e` | Edit before approve |
| `a` | Always approve this tool |
| `d` | Show details |

### Quick Commands (in input)

| Input | Action |
|-------|--------|
| `/clear` | Clear conversation |
| `/context` | Show context files |
| `/model` | Change model |
| `/undo` | Undo last edit |
| `/diff` | Show all changes |
| `/save` | Save conversation |
| `/load` | Load conversation |
| `@file.rs` | Add file to context |
| `@@` | Add all open files |

---

## UI Mockups

### Main Conversation View

```
┌─ OSVM Code ─────────────────────────────────────────────────────────────────┐
│ my-project (main) │ claude-sonnet-4-20250514 │ 12.3k tokens │ ● Connected   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─ You ────────────────────────────────────────────────────────────────┐  │
│  │ Add a function to parse JSON config files with error handling        │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                                                             │
│  ┌─ Assistant ──────────────────────────────────────────────────────────┐  │
│  │                                                                      │  │
│  │  I'll add a config parsing function. Let me first check the         │  │
│  │  existing code structure.                                            │  │
│  │                                                                      │  │
│  │  ┌─ 🔍 Read ─────────────────────────────────────────────────────┐  │  │
│  │  │  src/config.rs                                       ✓ Done  │  │  │
│  │  └──────────────────────────────────────────────────────────────┘  │  │
│  │                                                                      │  │
│  │  I see you have a basic Config struct. I'll add JSON parsing        │  │
│  │  with proper error handling.                                         │  │
│  │                                                                      │  │
│  │  ┌─ ✏️ Edit ─────────────────────────────────────────────────────┐  │  │
│  │  │  src/config.rs                                    ⏳ Pending  │  │  │
│  │  │  ┌────────────────────────────────────────────────────────┐   │  │  │
│  │  │  │ @@ -15,6 +15,25 @@                                    │   │  │  │
│  │  │  │  impl Config {                                        │   │  │  │
│  │  │  │      pub fn new() -> Self { ... }                     │   │  │  │
│  │  │  │ +                                                     │   │  │  │
│  │  │  │ +    /// Load configuration from a JSON file          │   │  │  │
│  │  │  │ +    pub fn from_json_file(path: &Path) -> Result<Self> │  │  │  │
│  │  │  │ +        let content = std::fs::read_to_string(path)  │   │  │  │
│  │  │  │ +            .map_err(|e| ConfigError::IoError(e))?;  │   │  │  │
│  │  │  │ +        serde_json::from_str(&content)               │   │  │  │
│  │  │  │ +            .map_err(|e| ConfigError::ParseError(e)) │   │  │  │
│  │  │  │ +    }                                                │   │  │  │
│  │  │  └────────────────────────────────────────────────────────┘   │  │  │
│  │  │                                                               │  │  │
│  │  │  [y] Approve  [n] Reject  [e] Edit  [d] Details              │  │  │
│  │  └──────────────────────────────────────────────────────────────┘  │  │
│  │                                                                      │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│ > add tests for the new config function                                     │
│                                                                      [⏎ Send]│
├─────────────────────────────────────────────────────────────────────────────┤
│ Ready │ Context: 3 files │ [Ctrl+C] Cancel │ [?] Help                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Streaming Response

```
│  ┌─ Assistant ──────────────────────────────────────────────────────────┐  │
│  │                                                                      │  │
│  │  I'll implement the feature in several steps:                       │  │
│  │                                                                      │  │
│  │  1. First, let me check the existing error types█                   │  │
│  │                                                                      │  │
│  │  ┌─ 🔍 Read ─────────────────────────────────────────────────────┐  │  │
│  │  │  src/error.rs                                       ⏳ Running │  │  │
│  │  │  ████████████░░░░░░░░░░░░░░░░░░░░░░                           │  │  │
│  │  └──────────────────────────────────────────────────────────────┘  │  │
│  │                                                                      │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
```

### Thinking Block (Expanded)

```
│  ┌─ Assistant ──────────────────────────────────────────────────────────┐  │
│  │                                                                      │  │
│  │  ┌─ 💭 Thinking ──────────────────────────────────────── [▼ Collapse]│  │
│  │  │                                                                  ││  │
│  │  │  The user wants to add JSON config parsing. Let me think about  ││  │
│  │  │  the best approach:                                              ││  │
│  │  │                                                                  ││  │
│  │  │  1. They have a Config struct in src/config.rs                  ││  │
│  │  │  2. They're using serde already (saw #[derive(Deserialize)])    ││  │
│  │  │  3. Need error handling - should I use thiserror or anyhow?     ││  │
│  │  │  4. Looking at their error.rs, they use thiserror              ││  │
│  │  │                                                                  ││  │
│  │  │  I'll add a from_json_file method that:                         ││  │
│  │  │  - Takes a Path reference                                        ││  │
│  │  │  - Returns Result<Self, ConfigError>                            ││  │
│  │  │  - Uses fs::read_to_string + serde_json                         ││  │
│  │  │                                                                  ││  │
│  │  └──────────────────────────────────────────────────────────────────┘│  │
│  │                                                                      │  │
│  │  I'll add the JSON parsing functionality...                         │  │
```

### Command Execution

```
│  ┌─ 🖥️ Bash ───────────────────────────────────────────────────────────┐  │
│  │  cargo test config::tests                              ⏳ Running   │  │
│  │  ┌────────────────────────────────────────────────────────────────┐ │  │
│  │  │ running 3 tests                                                │ │  │
│  │  │ test config::tests::test_from_json_file ... ok                │ │  │
│  │  │ test config::tests::test_invalid_json ... ok                  │ │  │
│  │  │ test config::tests::test_missing_file ... ok                  │ │  │
│  │  │                                                                │ │  │
│  │  │ test result: ok. 3 passed; 0 failed; 0 ignored                │ │  │
│  │  └────────────────────────────────────────────────────────────────┘ │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
```

### Approval Modal

```
┌─ Approve Action? ───────────────────────────────────────────────────────────┐
│                                                                             │
│  Tool: bash                                                                 │
│  Risk: ⚠️ Medium                                                            │
│                                                                             │
│  Command:                                                                   │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ rm -rf ./target/debug/build                                             ││
│  └─────────────────────────────────────────────────────────────────────────┘│
│                                                                             │
│  This command will:                                                         │
│  • Delete the build directory recursively                                   │
│  • Remove all cached build artifacts                                        │
│  • Require full rebuild on next cargo build                                │
│                                                                             │
│  ─────────────────────────────────────────────────────────────────────────  │
│                                                                             │
│    [y] Approve    [n] Reject    [e] Edit    [a] Always Allow               │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Implementation Plan

### Phase 1: Core Foundation (Week 1-2)

| Task | Description | Files |
|------|-------------|-------|
| 1.1 | CodeApp state machine | `src/utils/tui/code/app.rs` |
| 1.2 | Message rendering | `src/utils/tui/code/views/messages.rs` |
| 1.3 | Input area (multi-line) | `src/utils/tui/code/views/input.rs` |
| 1.4 | Basic Claude API integration | `src/utils/tui/code/backend/claude.rs` |
| 1.5 | Streaming response handling | `src/utils/tui/code/streaming.rs` |

### Phase 2: Tool System (Week 3-4)

| Task | Description | Files |
|------|-------------|-------|
| 2.1 | Tool trait and registry | `src/utils/tui/code/tools/mod.rs` |
| 2.2 | Read/Write/Edit tools | `src/utils/tui/code/tools/files.rs` |
| 2.3 | Bash tool | `src/utils/tui/code/tools/bash.rs` |
| 2.4 | Search tools (glob, grep) | `src/utils/tui/code/tools/search.rs` |
| 2.5 | Permission system | `src/utils/tui/code/permissions.rs` |
| 2.6 | Approval modal UI | `src/utils/tui/code/views/approval.rs` |

### Phase 3: Context & Intelligence (Week 5-6)

| Task | Description | Files |
|------|-------------|-------|
| 3.1 | File indexing | `src/utils/tui/code/context/index.rs` |
| 3.2 | Context management | `src/utils/tui/code/context/manager.rs` |
| 3.3 | System prompt builder | `src/utils/tui/code/prompt.rs` |
| 3.4 | Thinking blocks | `src/utils/tui/code/views/thinking.rs` |
| 3.5 | Code block rendering | `src/utils/tui/code/views/codeblock.rs` |
| 3.6 | Diff view | `src/utils/tui/code/views/diff.rs` |

### Phase 4: Advanced Features (Week 7-8)

| Task | Description | Files |
|------|-------------|-------|
| 4.1 | Git integration tools | `src/utils/tui/code/tools/git.rs` |
| 4.2 | Web search/fetch tools | `src/utils/tui/code/tools/web.rs` |
| 4.3 | Todo tracking | `src/utils/tui/code/tools/todo.rs` |
| 4.4 | Conversation persistence | `src/utils/tui/code/storage.rs` |
| 4.5 | Settings UI | `src/utils/tui/code/views/settings.rs` |

### Phase 5: Polish & Extensions (Week 9-10)

| Task | Description | Files |
|------|-------------|-------|
| 5.1 | MCP integration | `src/utils/tui/code/mcp.rs` |
| 5.2 | Custom tool definitions | `src/utils/tui/code/tools/custom.rs` |
| 5.3 | Performance optimization | Across modules |
| 5.4 | Testing suite | `tests/tui_code_tests.rs` |
| 5.5 | Documentation | `docs/CODE_USER_GUIDE.md` |

---

## File Structure

```
src/utils/tui/code/
├── mod.rs                  # Module exports
├── app.rs                  # Main CodeApp state machine
├── state.rs                # State types
├── input.rs                # Input handling
├── streaming.rs            # Response streaming
├── permissions.rs          # Permission system
├── prompt.rs               # System prompt builder
├── storage.rs              # Conversation persistence
├── backend/
│   ├── mod.rs
│   ├── claude.rs           # Anthropic API
│   ├── openai.rs           # OpenAI-compatible
│   └── local.rs            # Local models (future)
├── tools/
│   ├── mod.rs              # Tool trait and registry
│   ├── files.rs            # Read, Write, Edit
│   ├── bash.rs             # Command execution
│   ├── search.rs           # Glob, Grep
│   ├── git.rs              # Git operations
│   ├── web.rs              # WebSearch, WebFetch
│   ├── todo.rs             # TodoWrite
│   ├── ask.rs              # AskUser
│   └── custom.rs           # User-defined tools
├── context/
│   ├── mod.rs
│   ├── index.rs            # File indexing
│   ├── manager.rs          # Context management
│   └── memory.rs           # Persistent memory
├── views/
│   ├── mod.rs
│   ├── messages.rs         # Message list
│   ├── input.rs            # Input area
│   ├── approval.rs         # Approval modal
│   ├── thinking.rs         # Thinking blocks
│   ├── codeblock.rs        # Code rendering
│   ├── diff.rs             # Diff view
│   ├── tool_call.rs        # Tool call display
│   ├── settings.rs         # Settings overlay
│   └── help.rs             # Help overlay
├── mcp.rs                  # MCP server integration
└── tests.rs
```

---

## Permission System

### Risk Assessment

```rust
impl Tool for BashTool {
    fn risk_level(&self) -> RiskLevel {
        RiskLevel::Medium  // Default for bash
    }

    fn requires_approval(&self, params: &serde_json::Value) -> bool {
        let command = params.get("command").and_then(|v| v.as_str()).unwrap_or("");

        // Auto-approve safe commands
        let safe_patterns = [
            r"^(ls|cat|head|tail|wc|echo|pwd|date|whoami)\b",
            r"^cargo (build|test|check|fmt|clippy)\b",
            r"^npm (test|run lint|run build)\b",
            r"^git (status|log|diff|branch)\b",
        ];

        // Require approval for risky commands
        let risky_patterns = [
            r"\brm\b",
            r"\bmv\b.*-f",
            r">\s*/",  // Writing to root
            r"\bsudo\b",
            r"\bchmod\b",
            r"\bcurl\b.*\|.*sh",  // Pipe to shell
        ];

        // Check patterns
        for pattern in &risky_patterns {
            if Regex::new(pattern).unwrap().is_match(command) {
                return true;
            }
        }

        for pattern in &safe_patterns {
            if Regex::new(pattern).unwrap().is_match(command) {
                return false;
            }
        }

        true  // Default to requiring approval
    }
}
```

### Permission Configuration

```rust
pub struct PermissionConfig {
    // Global settings
    pub require_approval: ApprovalMode,
    pub auto_approve_reads: bool,
    pub auto_approve_safe_commands: bool,

    // Per-tool settings
    pub tool_permissions: HashMap<String, ToolPermission>,

    // Blocklist
    pub blocked_patterns: Vec<String>,
    pub blocked_paths: Vec<PathBuf>,
}

pub enum ApprovalMode {
    Always,     // Approve everything automatically
    Smart,      // Auto-approve safe operations
    Always_Ask, // Always ask for approval
}

pub struct ToolPermission {
    pub enabled: bool,
    pub auto_approve: bool,
    pub max_executions: Option<u32>,  // Per session
}
```

---

## Performance Requirements

| Metric | Target |
|--------|--------|
| Startup time | < 300ms |
| Token streaming | Real-time (< 50ms latency) |
| Tool execution start | < 100ms |
| File reading | < 50ms for < 1MB |
| Input responsiveness | < 16ms |
| Memory (baseline) | < 50MB |
| Memory (large context) | < 200MB |

---

## Security Considerations

### API Key Safety

1. **Environment Variable** - Prefer `ANTHROPIC_API_KEY` env var
2. **Config File** - `~/.config/osvm/credentials` with 600 permissions
3. **No Logging** - Never log API keys
4. **Clear on Exit** - Zero memory on application exit

### Command Execution Safety

1. **Sandboxing** - Optional sandboxed execution
2. **Path Restrictions** - Configurable allowed paths
3. **Command Blocklist** - Block dangerous commands
4. **Output Limits** - Truncate large outputs
5. **Timeout** - Kill long-running commands

### Code Injection Prevention

1. **Input Sanitization** - Escape special characters
2. **No eval()** - Never execute arbitrary code
3. **Path Validation** - Prevent directory traversal

---

## Dependencies

### Core
- `ratatui`, `crossterm` - TUI
- `reqwest` - HTTP client
- `tokio` - Async runtime
- `serde_json` - JSON handling
- `syntect` - Syntax highlighting
- `similar` - Diff generation

### Optional
- `tree-sitter` - Advanced parsing
- `git2` - Git operations

---

## CLI Integration

```rust
// src/clparse.rs addition
#[derive(Subcommand)]
pub enum Commands {
    /// AI-powered coding assistant
    Code {
        /// Initial prompt (optional)
        prompt: Option<String>,

        /// Project directory
        #[arg(long, short, default_value = ".")]
        directory: PathBuf,

        /// Model to use
        #[arg(long, default_value = "claude-sonnet-4-20250514")]
        model: String,

        /// Resume previous conversation
        #[arg(long)]
        resume: Option<String>,

        /// Auto-approve all operations (dangerous)
        #[arg(long)]
        yolo: bool,

        /// Disable tool usage
        #[arg(long)]
        no_tools: bool,
    },
}
```

---

## Example Usage

```bash
# Interactive mode
osvm code

# With initial prompt
osvm code "add unit tests for the auth module"

# Specific directory
osvm code -d ./my-project "fix the build errors"

# Resume previous conversation
osvm code --resume last

# Different model
osvm code --model claude-opus-4-20250514 "refactor for better performance"

# YOLO mode (auto-approve everything)
osvm code --yolo "update all dependencies"
```

---

## Open Questions

1. **Voice Input** - Priority for voice-to-text input?
2. **Image Support** - Screenshot sharing for debugging?
3. **Multi-Model** - Route different tasks to different models?
4. **Collaborative** - Share sessions with team?
5. **Vim Mode** - Vim keybindings for input area?
6. **Memory Scope** - What should persist across sessions?
