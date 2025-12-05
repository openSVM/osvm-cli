# osvm code - MVP Specification

**Timeline:** 3-4 weeks
**New Code:** ~1,800 LOC
**Leverage:** 70% existing infrastructure

---

## Scope: What We're Building

A Claude Code-style AI coding assistant in ratatui that can:
1. ✅ Chat with streaming responses (EXISTS)
2. ✅ Show thinking/reasoning (EXISTS)
3. ✅ Display tool calls (EXISTS)
4. 🔨 Read files from the project
5. 🔨 Write/edit files with approval
6. 🔨 Run bash commands with approval
7. 🔨 Show diffs before applying changes

**NOT in MVP:**
- Web search (defer)
- Git tools (defer)
- MCP custom tools (defer)
- Persistent memory (defer)
- Multi-model switching (defer)

---

## Week 1: Tool System Foundation

### Day 1-2: Tool Trait + Registry

```rust
// src/utils/tui/code/tools/mod.rs

#[async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn risk_level(&self) -> RiskLevel;

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput>;
    fn requires_approval(&self, params: &Value) -> bool;
}

#[derive(Clone, Copy)]
pub enum RiskLevel {
    Safe,    // Read operations
    Low,     // Reversible writes
    Medium,  // Potentially destructive
}

pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        let mut registry = Self { tools: HashMap::new() };
        registry.register(Arc::new(ReadTool));
        registry.register(Arc::new(WriteTool));
        registry.register(Arc::new(EditTool));
        registry.register(Arc::new(BashTool));
        registry.register(Arc::new(GlobTool));
        registry.register(Arc::new(GrepTool));
        registry
    }
}
```

### Day 3-4: File Tools

```rust
// src/utils/tui/code/tools/files.rs

pub struct ReadTool;

#[async_trait]
impl Tool for ReadTool {
    fn name(&self) -> &str { "read" }
    fn risk_level(&self) -> RiskLevel { RiskLevel::Safe }
    fn requires_approval(&self, _: &Value) -> bool { false }  // Auto-approve

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let path = params["file_path"].as_str().ok_or(anyhow!("missing file_path"))?;
        let abs_path = ctx.resolve_path(path)?;

        // Security: Must be within project root
        if !abs_path.starts_with(&ctx.project_root) {
            return Err(anyhow!("Path outside project: {}", path));
        }

        let content = tokio::fs::read_to_string(&abs_path).await?;
        let line_count = content.lines().count();

        // Truncate large files
        let output = if content.len() > 50_000 {
            format!("{}...\n\n[Truncated: {} lines total]",
                &content[..50_000], line_count)
        } else {
            content
        };

        Ok(ToolOutput::text(output))
    }
}

pub struct WriteTool;

#[async_trait]
impl Tool for WriteTool {
    fn name(&self) -> &str { "write" }
    fn risk_level(&self) -> RiskLevel { RiskLevel::Low }
    fn requires_approval(&self, _: &Value) -> bool { true }  // Always approve

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let path = params["file_path"].as_str().ok_or(anyhow!("missing file_path"))?;
        let content = params["content"].as_str().ok_or(anyhow!("missing content"))?;

        let abs_path = ctx.resolve_path(path)?;

        // Create parent dirs
        if let Some(parent) = abs_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        tokio::fs::write(&abs_path, content).await?;
        Ok(ToolOutput::text(format!("Wrote {} bytes to {}", content.len(), path)))
    }
}

pub struct EditTool;

#[async_trait]
impl Tool for EditTool {
    fn name(&self) -> &str { "edit" }
    fn risk_level(&self) -> RiskLevel { RiskLevel::Low }
    fn requires_approval(&self, _: &Value) -> bool { true }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let path = params["file_path"].as_str().ok_or(anyhow!("missing file_path"))?;
        let old_string = params["old_string"].as_str().ok_or(anyhow!("missing old_string"))?;
        let new_string = params["new_string"].as_str().ok_or(anyhow!("missing new_string"))?;

        let abs_path = ctx.resolve_path(path)?;
        let content = tokio::fs::read_to_string(&abs_path).await?;

        // Find and replace (must be unique)
        let count = content.matches(old_string).count();
        if count == 0 {
            return Err(anyhow!("old_string not found in file"));
        }
        if count > 1 {
            return Err(anyhow!("old_string appears {} times, must be unique", count));
        }

        let new_content = content.replace(old_string, new_string);
        tokio::fs::write(&abs_path, &new_content).await?;

        Ok(ToolOutput::text(format!("Edited {}", path)))
    }
}
```

### Day 5: Bash Tool

```rust
// src/utils/tui/code/tools/bash.rs

pub struct BashTool;

#[async_trait]
impl Tool for BashTool {
    fn name(&self) -> &str { "bash" }
    fn risk_level(&self) -> RiskLevel { RiskLevel::Medium }

    fn requires_approval(&self, params: &Value) -> bool {
        let cmd = params["command"].as_str().unwrap_or("");

        // Auto-approve safe commands
        let safe_patterns = [
            "^ls\\b", "^cat\\b", "^head\\b", "^tail\\b",
            "^cargo (build|test|check|fmt|clippy)\\b",
            "^git (status|log|diff|branch)\\b",
            "^pwd$", "^echo\\b",
        ];

        for pattern in safe_patterns {
            if regex::Regex::new(pattern).unwrap().is_match(cmd) {
                return false;  // Auto-approve
            }
        }
        true  // Require approval
    }

    async fn execute(&self, params: Value, ctx: &ToolContext) -> Result<ToolOutput> {
        let command = params["command"].as_str().ok_or(anyhow!("missing command"))?;
        let timeout_ms = params["timeout"].as_u64().unwrap_or(120_000);

        let output = tokio::time::timeout(
            Duration::from_millis(timeout_ms),
            tokio::process::Command::new("bash")
                .arg("-c")
                .arg(command)
                .current_dir(&ctx.project_root)
                .output()
        ).await??;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        let result = if output.status.success() {
            format!("{}{}", stdout, stderr)
        } else {
            format!("Exit code: {}\n{}{}", output.status.code().unwrap_or(-1), stdout, stderr)
        };

        // Truncate large output
        let truncated = if result.len() > 30_000 {
            format!("{}...\n[Output truncated]", &result[..30_000])
        } else {
            result
        };

        Ok(ToolOutput::text(truncated))
    }
}
```

---

## Week 2: Permission System + UI Integration

### Day 1-2: Permission System

```rust
// src/utils/tui/code/permissions.rs

pub struct PermissionManager {
    auto_approved: HashSet<String>,  // Tool names always approved
    session_approved: HashSet<String>,  // "Always approve" for session
}

impl PermissionManager {
    pub fn new() -> Self {
        let mut auto = HashSet::new();
        auto.insert("read".into());
        auto.insert("glob".into());
        auto.insert("grep".into());
        Self { auto_approved: auto, session_approved: HashSet::new() }
    }

    pub fn needs_approval(&self, tool: &dyn Tool, params: &Value) -> bool {
        if self.auto_approved.contains(tool.name()) {
            return false;
        }
        if self.session_approved.contains(tool.name()) {
            return false;
        }
        tool.requires_approval(params)
    }

    pub fn approve_always(&mut self, tool_name: &str) {
        self.session_approved.insert(tool_name.into());
    }
}
```

### Day 3-4: Approval Modal

```rust
// src/utils/tui/code/views/approval.rs

pub struct ApprovalModal {
    pub tool_name: String,
    pub params: Value,
    pub risk_level: RiskLevel,
    pub preview: Option<String>,  // Diff preview for edits
    pub selected: ApprovalChoice,
}

pub enum ApprovalChoice {
    Approve,
    Reject,
    Edit,
    AlwaysApprove,
}

impl ApprovalModal {
    pub fn render(&self, f: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Approve Action? ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Yellow));

        let inner = block.inner(area);
        f.render_widget(Clear, area);
        f.render_widget(block, area);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Tool info
                Constraint::Min(5),     // Preview
                Constraint::Length(3),  // Actions
            ])
            .split(inner);

        // Tool info
        let info = format!("Tool: {}  Risk: {:?}", self.tool_name, self.risk_level);
        f.render_widget(Paragraph::new(info), chunks[0]);

        // Preview (diff for edits, command for bash)
        if let Some(preview) = &self.preview {
            let preview_widget = Paragraph::new(preview.as_str())
                .block(Block::default().borders(Borders::ALL).title("Preview"))
                .wrap(Wrap { trim: false });
            f.render_widget(preview_widget, chunks[1]);
        }

        // Actions
        let actions = Line::from(vec![
            Span::styled("[y] Approve  ", self.style_for(ApprovalChoice::Approve)),
            Span::styled("[n] Reject  ", self.style_for(ApprovalChoice::Reject)),
            Span::styled("[a] Always  ", self.style_for(ApprovalChoice::AlwaysApprove)),
        ]);
        f.render_widget(Paragraph::new(actions), chunks[2]);
    }
}
```

### Day 5: Integrate with Existing Chat

Modify `src/utils/tui/app.rs` to add code mode:

```rust
// Add to OsvmApp
pub code_tools: Option<ToolRegistry>,
pub code_permissions: PermissionManager,
pub pending_approval: Option<ApprovalModal>,

// Add to TabIndex
Code = 7,  // New tab for Claude Code mode

// Add render_code_tab function
fn render_code_tab(&mut self, f: &mut Frame, area: Rect) {
    // Reuse existing chat rendering
    self.render_chat(f, area);

    // Overlay approval modal if pending
    if let Some(modal) = &self.pending_approval {
        let modal_area = centered_rect(60, 50, area);
        modal.render(f, modal_area);
    }
}
```

---

## Week 3: AI Integration + Streaming

### Day 1-2: Tool Call Processing

```rust
// src/utils/tui/code/agent.rs

pub async fn process_tool_calls(
    tool_calls: Vec<ToolCall>,
    registry: &ToolRegistry,
    permissions: &mut PermissionManager,
    tx: &Sender<ChatResponse>,
    ctx: &ToolContext,
) -> Vec<ToolResult> {
    let mut results = Vec::new();

    for call in tool_calls {
        // Send "calling tool" message
        let _ = tx.send(ChatResponse {
            content: format!("🔧 Calling {}...", call.name),
            response_type: ChatResponseType::ToolCall(call.name.clone()),
        });

        let tool = match registry.get(&call.name) {
            Some(t) => t,
            None => {
                results.push(ToolResult::error(&call.id, "Unknown tool"));
                continue;
            }
        };

        // Check permissions
        if permissions.needs_approval(tool.as_ref(), &call.input) {
            // Queue for approval (handled in main loop)
            results.push(ToolResult::pending(&call.id, "Awaiting approval"));
            continue;
        }

        // Execute
        match tool.execute(call.input.clone(), ctx).await {
            Ok(output) => {
                let _ = tx.send(ChatResponse {
                    content: format!("✅ {} completed", call.name),
                    response_type: ChatResponseType::ToolResult(call.name.clone()),
                });
                results.push(ToolResult::success(&call.id, output.text));
            }
            Err(e) => {
                let _ = tx.send(ChatResponse {
                    content: format!("❌ {} failed: {}", call.name, e),
                    response_type: ChatResponseType::ToolResult(call.name.clone()),
                });
                results.push(ToolResult::error(&call.id, &e.to_string()));
            }
        }
    }

    results
}
```

### Day 3-4: System Prompt

```rust
// src/utils/tui/code/prompt.rs

pub fn build_system_prompt(ctx: &ToolContext) -> String {
    format!(r#"You are an AI coding assistant helping with a software project.

Project: {}
Working Directory: {}

You have these tools:
- read: Read file contents. Params: {{ "file_path": "path" }}
- write: Create/overwrite file. Params: {{ "file_path": "path", "content": "..." }}
- edit: Replace text in file. Params: {{ "file_path": "path", "old_string": "...", "new_string": "..." }}
- bash: Run shell command. Params: {{ "command": "..." }}
- glob: Find files. Params: {{ "pattern": "**/*.rs" }}
- grep: Search content. Params: {{ "pattern": "TODO", "path": "src/" }}

Guidelines:
1. Read files before editing - understand existing code
2. Make minimal, focused changes
3. Don't add unnecessary comments or docstrings
4. Use edit for surgical changes, write for new files
5. Run tests after making changes

You're helping with a Rust/Solana project. The user will give you tasks."#,
        ctx.project_name,
        ctx.project_root.display()
    )
}
```

### Day 5: Diff Generation

```rust
// src/utils/tui/code/diff.rs

use similar::{ChangeTag, TextDiff};

pub fn generate_diff(old: &str, new: &str, path: &str) -> String {
    let diff = TextDiff::from_lines(old, new);
    let mut output = format!("--- a/{}\n+++ b/{}\n", path, path);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            output.push_str("...\n");
        }

        for op in group {
            for change in diff.iter_inline_changes(op) {
                let sign = match change.tag() {
                    ChangeTag::Delete => "-",
                    ChangeTag::Insert => "+",
                    ChangeTag::Equal => " ",
                };
                output.push_str(sign);
                for (_, value) in change.iter_strings_lossy() {
                    output.push_str(&value);
                }
                if change.missing_newline() {
                    output.push('\n');
                }
            }
        }
    }

    output
}
```

---

## Week 4: Polish + Testing

### Day 1-2: Collapsible Blocks

Add to message rendering:

```rust
fn render_message_block(&self, block: &ContentBlock, f: &mut Frame, area: Rect) {
    match block {
        ContentBlock::Thinking { content, collapsed } => {
            let header = if *collapsed {
                "💭 Thinking... [+]"
            } else {
                "💭 Thinking [-]"
            };

            let block_widget = Block::default()
                .title(header)
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::DarkGray));

            if *collapsed {
                f.render_widget(block_widget, area);
            } else {
                let inner = block_widget.inner(area);
                f.render_widget(block_widget, area);
                f.render_widget(
                    Paragraph::new(content.as_str()).wrap(Wrap { trim: false }),
                    inner
                );
            }
        }
        ContentBlock::ToolCall { name, status, output, collapsed, .. } => {
            let icon = match status {
                ToolStatus::Running => "⏳",
                ToolStatus::Success => "✅",
                ToolStatus::Error(_) => "❌",
                _ => "🔧",
            };

            let header = format!("{} {} [{}]", icon, name, if *collapsed { "+" } else { "-" });
            // Similar rendering...
        }
        _ => {}
    }
}
```

### Day 3-4: Keyboard Shortcuts

```rust
// Handle keys in code tab
KeyCode::Char('y') if self.pending_approval.is_some() => {
    self.approve_pending_tool(true);
}
KeyCode::Char('n') if self.pending_approval.is_some() => {
    self.approve_pending_tool(false);
}
KeyCode::Char('a') if self.pending_approval.is_some() => {
    self.approve_pending_tool_always();
}
KeyCode::Enter if !self.chat_input.is_empty() => {
    self.send_code_message();
}
KeyCode::Tab => {
    self.toggle_block_collapse();
}
```

### Day 5: Testing

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_read_tool_within_project() {
        // ...
    }

    #[test]
    fn test_read_tool_outside_project_fails() {
        // ...
    }

    #[test]
    fn test_edit_tool_unique_match() {
        // ...
    }

    #[test]
    fn test_bash_safe_commands_auto_approve() {
        let tool = BashTool;
        assert!(!tool.requires_approval(&json!({"command": "ls -la"})));
        assert!(!tool.requires_approval(&json!({"command": "cargo test"})));
        assert!(tool.requires_approval(&json!({"command": "rm -rf /"})));
    }

    #[test]
    fn test_diff_generation() {
        let old = "line1\nline2\nline3";
        let new = "line1\nmodified\nline3";
        let diff = generate_diff(old, new, "test.txt");
        assert!(diff.contains("-line2"));
        assert!(diff.contains("+modified"));
    }
}
```

---

## File Structure

```
src/utils/tui/code/
├── mod.rs           # Module exports
├── tools/
│   ├── mod.rs       # Tool trait, registry
│   ├── files.rs     # read, write, edit
│   ├── bash.rs      # bash execution
│   └── search.rs    # glob, grep
├── permissions.rs   # Permission manager
├── agent.rs         # Tool call processing
├── prompt.rs        # System prompt builder
├── diff.rs          # Diff generation
└── views/
    └── approval.rs  # Approval modal
```

---

## CLI Entry Point

```rust
// Add to src/clparse.rs
Code {
    /// Initial prompt
    prompt: Option<String>,

    /// Project directory
    #[arg(short, long, default_value = ".")]
    directory: PathBuf,
},

// Add to main.rs handler
Commands::Code { prompt, directory } => {
    let mut app = OsvmApp::new_code_mode(directory)?;
    if let Some(p) = prompt {
        app.send_initial_prompt(&p);
    }
    app.run()?;
}
```

---

## Success Criteria

MVP is complete when:
1. ✅ Can read files from project
2. ✅ Can write new files with approval
3. ✅ Can edit existing files with diff preview
4. ✅ Can run bash commands with smart approval
5. ✅ Streaming responses display correctly
6. ✅ Thinking blocks collapse/expand
7. ✅ Tool calls show status (running/success/error)
8. ✅ All tests pass
