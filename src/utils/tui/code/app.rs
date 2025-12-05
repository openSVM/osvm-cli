//! Code assistant TUI application state
//!
//! Main state struct for the ratatui-based code assistant.

use super::permissions::{ApprovalRequest, ApprovalResponse, PermissionManager};
use super::prompt::{build_system_prompt, extract_text_content, parse_tool_calls, ParsedToolCall};
use super::tools::{Tool, ToolContext, ToolOutput, ToolRegistry};
use super::views::approval::ApprovalModal;
use crate::services::ai_service::AiService;
use anyhow::Result;
use std::path::PathBuf;
use std::sync::Arc;

/// A message in the conversation
#[derive(Debug, Clone)]
pub struct Message {
    pub role: MessageRole,
    pub content: String,
    pub tool_calls: Vec<ToolCallDisplay>,
}

/// Role of a message
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageRole {
    User,
    Assistant,
    System,
    ToolResult,
}

/// Tool call for display
#[derive(Debug, Clone)]
pub struct ToolCallDisplay {
    pub name: String,
    pub status: ToolCallStatus,
    pub output: Option<String>,
}

/// Status of a tool call
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolCallStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Skipped,
}

/// Main code assistant application state
pub struct CodeApp {
    // === Project Context ===
    pub project_root: PathBuf,
    pub project_name: String,

    // === Services ===
    pub ai_service: Arc<AiService>,
    pub registry: ToolRegistry,
    pub context: ToolContext,
    pub permissions: PermissionManager,
    pub system_prompt: String,

    // === Conversation ===
    pub messages: Vec<Message>,
    pub conversation_context: Vec<String>,

    // === Input ===
    pub input: String,
    pub input_cursor: usize,

    // === UI State ===
    pub scroll_offset: usize,
    pub focus: CodeFocus,
    pub status: AppStatus,

    // === Modal State ===
    pub approval_modal: Option<ApprovalModal>,
    pub pending_tool_call: Option<ParsedToolCall>,

    // === Settings ===
    pub yolo_mode: bool,
    pub no_tools: bool,
    pub debug: bool,

    // === General ===
    pub should_quit: bool,
}

/// Current UI focus
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeFocus {
    Input,
    Messages,
}

/// Application status
#[derive(Debug, Clone)]
pub enum AppStatus {
    Ready,
    Thinking,
    ExecutingTool(String),
    WaitingApproval,
    Error(String),
}

impl CodeApp {
    /// Create a new code app
    pub fn new(project_root: PathBuf, yolo_mode: bool, no_tools: bool, debug: bool) -> Self {
        let project_name = project_root
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("project")
            .to_string();

        let context = ToolContext::new(project_root.clone());
        let registry = ToolRegistry::new();
        let system_prompt = build_system_prompt(&context, &registry);

        Self {
            project_root,
            project_name,
            ai_service: Arc::new(AiService::new()),
            registry,
            context,
            permissions: PermissionManager::new(),
            system_prompt,
            messages: Vec::new(),
            conversation_context: Vec::new(),
            input: String::new(),
            input_cursor: 0,
            scroll_offset: 0,
            focus: CodeFocus::Input,
            status: AppStatus::Ready,
            approval_modal: None,
            pending_tool_call: None,
            yolo_mode,
            no_tools,
            debug,
            should_quit: false,
        }
    }

    /// Add a user message
    pub fn add_user_message(&mut self, content: String) {
        self.messages.push(Message {
            role: MessageRole::User,
            content,
            tool_calls: Vec::new(),
        });
    }

    /// Add an assistant message
    pub fn add_assistant_message(&mut self, content: String, tool_calls: Vec<ToolCallDisplay>) {
        self.messages.push(Message {
            role: MessageRole::Assistant,
            content,
            tool_calls,
        });
    }

    /// Add a tool result message
    pub fn add_tool_result(&mut self, tool_name: &str, output: &str, success: bool) {
        self.messages.push(Message {
            role: MessageRole::ToolResult,
            content: format!(
                "{} {}: {}",
                if success { "✓" } else { "✗" },
                tool_name,
                output
            ),
            tool_calls: Vec::new(),
        });
    }

    /// Build the full prompt for the AI
    pub fn build_full_prompt(&self) -> String {
        let mut prompt = format!("{}\n\n", self.system_prompt);

        // Add conversation history (last 10 messages)
        let history_start = self.conversation_context.len().saturating_sub(10);
        for msg in &self.conversation_context[history_start..] {
            prompt.push_str(msg);
            prompt.push_str("\n\n");
        }

        prompt.push_str("ASSISTANT:");
        prompt
    }

    /// Handle text input
    pub fn handle_char(&mut self, c: char) {
        self.input.insert(self.input_cursor, c);
        self.input_cursor += 1;
    }

    /// Handle backspace
    pub fn handle_backspace(&mut self) {
        if self.input_cursor > 0 {
            self.input_cursor -= 1;
            self.input.remove(self.input_cursor);
        }
    }

    /// Handle delete
    pub fn handle_delete(&mut self) {
        if self.input_cursor < self.input.len() {
            self.input.remove(self.input_cursor);
        }
    }

    /// Move cursor left
    pub fn cursor_left(&mut self) {
        if self.input_cursor > 0 {
            self.input_cursor -= 1;
        }
    }

    /// Move cursor right
    pub fn cursor_right(&mut self) {
        if self.input_cursor < self.input.len() {
            self.input_cursor += 1;
        }
    }

    /// Move cursor to start
    pub fn cursor_home(&mut self) {
        self.input_cursor = 0;
    }

    /// Move cursor to end
    pub fn cursor_end(&mut self) {
        self.input_cursor = self.input.len();
    }

    /// Submit the current input
    pub fn submit_input(&mut self) -> Option<String> {
        let input = self.input.trim().to_string();
        if input.is_empty() {
            return None;
        }

        // Add to conversation context
        self.conversation_context.push(format!("USER: {}", input));
        self.add_user_message(input.clone());

        // Clear input
        self.input.clear();
        self.input_cursor = 0;

        Some(input)
    }

    /// Scroll up in messages
    pub fn scroll_up(&mut self) {
        if self.scroll_offset > 0 {
            self.scroll_offset -= 1;
        }
    }

    /// Scroll down in messages
    pub fn scroll_down(&mut self) {
        self.scroll_offset += 1;
    }

    /// Scroll to bottom
    pub fn scroll_to_bottom(&mut self) {
        // Will be clamped during rendering
        self.scroll_offset = self.messages.len().saturating_sub(1);
    }

    /// Clear conversation
    pub fn clear_conversation(&mut self) {
        self.messages.clear();
        self.conversation_context.clear();
        self.scroll_offset = 0;
        self.permissions.reset_session();
        self.status = AppStatus::Ready;
    }

    /// Toggle focus between input and messages
    pub fn toggle_focus(&mut self) {
        self.focus = match self.focus {
            CodeFocus::Input => CodeFocus::Messages,
            CodeFocus::Messages => CodeFocus::Input,
        };
    }

    /// Check if a tool needs approval
    pub fn needs_approval(&self, tool: &dyn Tool, params: &serde_json::Value) -> bool {
        !self.yolo_mode && self.permissions.needs_approval(tool, params)
    }

    /// Show approval modal for a tool call
    pub fn show_approval_modal(&mut self, tool_call: ParsedToolCall) {
        if let Some(tool) = self.registry.get(&tool_call.name) {
            let preview = tool.generate_preview(&tool_call.params, &self.context);
            let request = ApprovalRequest::new(
                &tool_call.name,
                tool_call.params.clone(),
                tool.risk_level(),
                preview,
            );
            self.approval_modal = Some(ApprovalModal::new(request));
            self.pending_tool_call = Some(tool_call);
            self.status = AppStatus::WaitingApproval;
        }
    }

    /// Handle approval response
    pub fn handle_approval(&mut self, response: ApprovalResponse) -> Option<(ParsedToolCall, bool)> {
        self.approval_modal = None;
        self.status = AppStatus::Ready;

        let tool_call = self.pending_tool_call.take()?;

        match response {
            ApprovalResponse::Approve => Some((tool_call, true)),
            ApprovalResponse::ApproveAlways => {
                self.permissions.approve_tool_for_session(&tool_call.name);
                Some((tool_call, true))
            }
            ApprovalResponse::Reject => Some((tool_call, false)),
            ApprovalResponse::Edit => {
                // TODO: Implement edit mode
                Some((tool_call, false))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_code_app_creation() {
        let app = CodeApp::new(PathBuf::from("/tmp/test"), false, false, false);
        assert_eq!(app.project_name, "test");
        assert!(app.messages.is_empty());
        assert_eq!(app.focus, CodeFocus::Input);
    }

    #[test]
    fn test_input_handling() {
        let mut app = CodeApp::new(PathBuf::from("/tmp/test"), false, false, false);

        app.handle_char('h');
        app.handle_char('i');
        assert_eq!(app.input, "hi");
        assert_eq!(app.input_cursor, 2);

        app.cursor_left();
        assert_eq!(app.input_cursor, 1);

        app.handle_char('X');
        assert_eq!(app.input, "hXi");

        app.handle_backspace();
        assert_eq!(app.input, "hi");
    }

    #[test]
    fn test_submit_input() {
        let mut app = CodeApp::new(PathBuf::from("/tmp/test"), false, false, false);

        app.input = "test message".to_string();
        let result = app.submit_input();

        assert_eq!(result, Some("test message".to_string()));
        assert!(app.input.is_empty());
        assert_eq!(app.messages.len(), 1);
    }
}
