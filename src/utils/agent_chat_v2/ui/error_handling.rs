//! Enhanced error handling with user-friendly messages and recovery suggestions

use cursive::traits::Resizable;
use cursive::views::Dialog;
use cursive::Cursive;

/// Common error types with recovery suggestions
pub enum ChatError {
    /// Agent is stuck in processing state
    AgentStuck,
    /// MCP server connection failed
    McpServerFailed(String),
    /// AI service unavailable
    AiServiceUnavailable,
    /// Session not found
    SessionNotFound,
    /// Message send failed
    MessageSendFailed(String),
    /// File operation failed
    FileOperationFailed(String),
    /// Terminal too small
    TerminalTooSmall,
    /// Unknown error with context
    Unknown(String),
}

impl ChatError {
    /// Get user-friendly error message with emoji icon
    pub fn message(&self) -> String {
        match self {
            ChatError::AgentStuck => {
                "⚠️ Agent Stuck\n\n\
                The agent appears to be stuck in processing state.\n\n\
                This usually happens when:\n\
                • AI service is slow to respond\n\
                • Network connection is unstable\n\
                • Processing message wasn't cleaned up".to_string()
            }
            ChatError::McpServerFailed(server) => {
                format!(
                    "🔌 MCP Server Connection Failed\n\n\
                    Could not connect to MCP server: {}\n\n\
                    This usually happens when:\n\
                    • Server is not running\n\
                    • Server configuration is incorrect\n\
                    • Network connectivity issues",
                    server
                )
            }
            ChatError::AiServiceUnavailable => {
                "🤖 AI Service Unavailable\n\n\
                The AI service is not responding.\n\n\
                This usually happens when:\n\
                • API key is invalid or missing\n\
                • Service is temporarily down\n\
                • Network connection issues\n\
                • Rate limit exceeded".to_string()
            }
            ChatError::SessionNotFound => {
                "💬 Session Not Found\n\n\
                The chat session could not be found.\n\n\
                This usually happens when:\n\
                • Session was deleted\n\
                • Application state was reset\n\
                • Internal error occurred".to_string()
            }
            ChatError::MessageSendFailed(reason) => {
                format!(
                    "📤 Message Send Failed\n\n\
                    Could not send message: {}\n\n\
                    This usually happens when:\n\
                    • Agent is not ready\n\
                    • Session is in error state\n\
                    • Network connection issues",
                    reason
                )
            }
            ChatError::FileOperationFailed(operation) => {
                format!(
                    "📁 File Operation Failed\n\n\
                    Could not complete file operation: {}\n\n\
                    This usually happens when:\n\
                    • Insufficient permissions\n\
                    • Disk space full\n\
                    • File path is invalid\n\
                    • File is in use by another process",
                    operation
                )
            }
            ChatError::TerminalTooSmall => {
                "📏 Terminal Too Small\n\n\
                Your terminal window is too small for the chat interface.\n\n\
                Minimum required size: 60 columns × 15 rows\n\
                Please resize your terminal and restart.".to_string()
            }
            ChatError::Unknown(context) => {
                format!(
                    "❌ Unknown Error\n\n\
                    An unexpected error occurred: {}\n\n\
                    If this persists, please report it to:\n\
                    https://github.com/opensvm/osvm-cli/issues",
                    context
                )
            }
        }
    }

    /// Get recovery suggestions for this error
    pub fn recovery_suggestions(&self) -> Vec<String> {
        match self {
            ChatError::AgentStuck => vec![
                "Press Alt+X to emergency clear processing state".to_string(),
                "Wait a few more seconds for the agent to respond".to_string(),
                "Check your network connection".to_string(),
                "Restart the chat if the issue persists".to_string(),
            ],
            ChatError::McpServerFailed(_) => vec![
                "Run 'osvm mcp setup' to configure MCP servers".to_string(),
                "Check MCP server status in Settings".to_string(),
                "Verify server URL and authentication".to_string(),
                "Try restarting the chat application".to_string(),
            ],
            ChatError::AiServiceUnavailable => vec![
                "Check OPENAI_KEY environment variable".to_string(),
                "Verify API key is valid and has credits".to_string(),
                "Check your internet connection".to_string(),
                "Try again in a few minutes (rate limit)".to_string(),
                "Use a different AI service endpoint".to_string(),
            ],
            ChatError::SessionNotFound => vec![
                "Create a new chat session with 'New Chat'".to_string(),
                "Select a different session from the list".to_string(),
                "Restart the application if issue persists".to_string(),
            ],
            ChatError::MessageSendFailed(_) => vec![
                "Check that agent is not paused or stopped".to_string(),
                "Try restarting the agent with 'Run' button".to_string(),
                "Clear stuck processing with Alt+X".to_string(),
                "Check error logs for more details".to_string(),
            ],
            ChatError::FileOperationFailed(_) => vec![
                "Check file permissions with ls -la".to_string(),
                "Verify disk space with df -h".to_string(),
                "Ensure directory exists and is writable".to_string(),
                "Close other applications using the file".to_string(),
            ],
            ChatError::TerminalTooSmall => vec![
                "Resize terminal to at least 60×15".to_string(),
                "Use fullscreen mode (F11 in most terminals)".to_string(),
                "Maximize the terminal window".to_string(),
                "Increase terminal font size if needed".to_string(),
            ],
            ChatError::Unknown(_) => vec![
                "Try the operation again".to_string(),
                "Restart the application".to_string(),
                "Check application logs for details".to_string(),
                "Report the issue if it persists".to_string(),
            ],
        }
    }

    /// Show error dialog with recovery suggestions
    pub fn show_error_dialog(&self, siv: &mut Cursive) {
        let message = self.message();
        let suggestions = self.recovery_suggestions();

        let mut full_message = message.clone();
        full_message.push_str("\n\n");
        full_message.push_str("═══════════════════════════════════════\n");
        full_message.push_str("💡 Recovery Suggestions:\n");
        full_message.push_str("═══════════════════════════════════════\n");
        for (i, suggestion) in suggestions.iter().enumerate() {
            full_message.push_str(&format!("\n{}. {}", i + 1, suggestion));
        }

        siv.add_layer(
            Dialog::text(full_message)
                .title("⚠️ Error")
                .button("OK", |s| {
                    s.pop_layer();
                })
                .button("Show Help", |s| {
                    s.pop_layer();
                    super::handlers::show_advanced_help(s);
                })
                .max_width(80),
        );
    }
}

/// Quick error helpers for common scenarios
pub fn show_quick_error(siv: &mut Cursive, title: &str, message: &str) {
    siv.add_layer(
        Dialog::text(format!("❌ {}", message))
            .title(title)
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}

pub fn show_quick_success(siv: &mut Cursive, title: &str, message: &str) {
    siv.add_layer(
        Dialog::text(format!("✅ {}", message))
            .title(title)
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}

pub fn show_quick_warning(siv: &mut Cursive, title: &str, message: &str) {
    siv.add_layer(
        Dialog::text(format!("⚠️ {}", message))
            .title(title)
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}

pub fn show_quick_info(siv: &mut Cursive, title: &str, message: &str) {
    siv.add_layer(
        Dialog::text(format!("ℹ️ {}", message))
            .title(title)
            .button("OK", |s| {
                s.pop_layer();
            }),
    );
}
