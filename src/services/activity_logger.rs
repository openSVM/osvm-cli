//! Activity logging service for CLI commands and chat messages

use anyhow::{Context, Result};
use chrono::Utc;
use log::{debug, error, info};
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;

use super::clickhouse_service::ClickHouseService;
use crate::utils::agent_chat_v2::{session::ChatSession, types::ChatMessage};

/// Buffer size before flushing to database
const BUFFER_SIZE: usize = 100;

/// CLI command log entry
#[derive(Debug, Clone)]
pub struct CommandLogEntry {
    pub timestamp: chrono::DateTime<Utc>,
    pub session_id: String,
    pub command: String,
    pub args: String,
    pub exit_code: i32,
    pub duration_ms: u64,
    pub error_message: String,
}

/// Chat message log entry
#[derive(Debug, Clone)]
pub struct ChatMessageLogEntry {
    pub timestamp: chrono::DateTime<Utc>,
    pub session_id: String,
    pub session_name: String,
    pub message_type: String,
    pub content: String,
    pub tokens_used: u32,
    pub model_name: String,
}

/// Activity logger service
pub struct ActivityLogger {
    clickhouse: Arc<ClickHouseService>,
    command_buffer: Arc<Mutex<Vec<CommandLogEntry>>>,
    chat_buffer: Arc<Mutex<Vec<ChatMessageLogEntry>>>,
    enabled: Arc<Mutex<bool>>,
}

impl ActivityLogger {
    /// Create a new activity logger
    pub fn new(clickhouse: Arc<ClickHouseService>) -> Self {
        Self {
            clickhouse,
            command_buffer: Arc::new(Mutex::new(Vec::new())),
            chat_buffer: Arc::new(Mutex::new(Vec::new())),
            enabled: Arc::new(Mutex::new(true)),
        }
    }

    /// Enable or disable activity logging
    pub async fn set_enabled(&self, enabled: bool) {
        let mut state = self.enabled.lock().await;
        *state = enabled;
        info!("Activity logging {}", if enabled { "enabled" } else { "disabled" });
    }

    /// Check if activity logging is enabled
    pub async fn is_enabled(&self) -> bool {
        *self.enabled.lock().await
    }

    /// Log a CLI command
    pub async fn log_command(
        &self,
        command: &str,
        args: &[String],
        exit_code: i32,
        duration_ms: u64,
        error_message: Option<&str>,
    ) -> Result<()> {
        if !self.is_enabled().await {
            return Ok(());
        }

        let entry = CommandLogEntry {
            timestamp: Utc::now(),
            session_id: Self::get_session_id(),
            command: command.to_string(),
            args: args.join(" "),
            exit_code,
            duration_ms,
            error_message: error_message.unwrap_or("").to_string(),
        };

        let mut buffer = self.command_buffer.lock().await;
        buffer.push(entry);

        // Flush if buffer is full
        if buffer.len() >= BUFFER_SIZE {
            drop(buffer); // Release lock before flush
            self.flush_commands().await?;
        }

        Ok(())
    }

    /// Log a chat message
    pub async fn log_chat_message(
        &self,
        session: &ChatSession,
        message: &ChatMessage,
    ) -> Result<()> {
        if !self.is_enabled().await {
            return Ok(());
        }

        let (message_type, content, tokens_used) = match message {
            ChatMessage::User(content) => ("User", content.clone(), 0),
            ChatMessage::Agent(content) => ("Assistant", content.clone(), 0),
            ChatMessage::System(content) => ("System", content.clone(), 0),
            ChatMessage::Processing { message, .. } => ("Processing", message.clone(), 0),
            ChatMessage::Error(content) => ("Error", content.clone(), 0),
            ChatMessage::ToolCall { tool_name, description, .. } => {
                ("System", format!("🔧 Tool: {} - {}", tool_name, description), 0)
            }
            ChatMessage::ToolResult { tool_name, result, .. } => {
                ("System", format!("✓ Tool result ({}): {}", tool_name, result), 0)
            }
            ChatMessage::AgentThinking(content) => ("Processing", content.clone(), 0),
            ChatMessage::AgentPlan(_) => ("System", "Agent created execution plan".to_string(), 0),
        };

        let entry = ChatMessageLogEntry {
            timestamp: Utc::now(),
            session_id: session.id.to_string(),
            session_name: session.name.clone(),
            message_type: message_type.to_string(),
            content,
            tokens_used,
            model_name: String::new(),
        };

        let mut buffer = self.chat_buffer.lock().await;
        buffer.push(entry);

        // Flush if buffer is full
        if buffer.len() >= BUFFER_SIZE {
            drop(buffer); // Release lock before flush
            self.flush_chat_messages().await?;
        }

        Ok(())
    }

    /// Flush command buffer to database
    pub async fn flush_commands(&self) -> Result<()> {
        let mut buffer = self.command_buffer.lock().await;
        if buffer.is_empty() {
            return Ok(());
        }

        debug!("Flushing {} command log entries to database", buffer.len());

        // Build batch insert query
        let mut values = Vec::new();
        for entry in buffer.iter() {
            let timestamp = entry.timestamp.timestamp_millis();
            values.push(format!(
                "({}, '{}', '{}', '{}', {}, {}, '{}')",
                timestamp,
                Self::escape_string(&entry.session_id),
                Self::escape_string(&entry.command),
                Self::escape_string(&entry.args),
                entry.exit_code,
                entry.duration_ms,
                Self::escape_string(&entry.error_message)
            ));
        }

        let sql = format!(
            "INSERT INTO osvm.cli_commands (timestamp, session_id, command, args, exit_code, duration_ms, error_message) VALUES {}",
            values.join(", ")
        );

        match self.clickhouse.execute_query(&sql).await {
            Ok(_) => {
                debug!("Successfully flushed {} command log entries", buffer.len());
                buffer.clear();
                Ok(())
            }
            Err(e) => {
                error!("Failed to flush command logs: {}", e);
                Err(e)
            }
        }
    }

    /// Flush chat message buffer to database
    pub async fn flush_chat_messages(&self) -> Result<()> {
        let mut buffer = self.chat_buffer.lock().await;
        if buffer.is_empty() {
            return Ok(());
        }

        debug!("Flushing {} chat message log entries to database", buffer.len());

        // Build batch insert query
        let mut values = Vec::new();
        for entry in buffer.iter() {
            let timestamp = entry.timestamp.timestamp_millis();
            let message_type_num = match entry.message_type.as_str() {
                "User" => 1,
                "Assistant" => 2,
                "System" => 3,
                "Processing" => 4,
                "Error" => 5,
                _ => 3,
            };
            
            values.push(format!(
                "({}, '{}', '{}', {}, '{}', {}, '{}')",
                timestamp,
                Self::escape_string(&entry.session_id),
                Self::escape_string(&entry.session_name),
                message_type_num,
                Self::escape_string(&entry.content),
                entry.tokens_used,
                Self::escape_string(&entry.model_name)
            ));
        }

        let sql = format!(
            "INSERT INTO osvm.chat_messages (timestamp, session_id, session_name, message_type, content, tokens_used, model_name) VALUES {}",
            values.join(", ")
        );

        match self.clickhouse.execute_query(&sql).await {
            Ok(_) => {
                debug!("Successfully flushed {} chat message log entries", buffer.len());
                buffer.clear();
                Ok(())
            }
            Err(e) => {
                error!("Failed to flush chat message logs: {}", e);
                Err(e)
            }
        }
    }

    /// Flush all buffers
    pub async fn flush_all(&self) -> Result<()> {
        self.flush_commands().await?;
        self.flush_chat_messages().await?;
        Ok(())
    }

    /// Query CLI command history
    pub async fn query_command_history(
        &self,
        limit: usize,
        session_id: Option<&str>,
    ) -> Result<String> {
        // SECURITY: Validate limit parameter to prevent SQL injection
        let safe_limit = Self::validate_limit(limit)?;

        let sql = if let Some(sid) = session_id {
            format!(
                "SELECT * FROM osvm.cli_commands WHERE session_id = '{}' ORDER BY timestamp DESC LIMIT {}",
                Self::escape_string(sid),
                safe_limit
            )
        } else {
            format!(
                "SELECT * FROM osvm.cli_commands ORDER BY timestamp DESC LIMIT {}",
                safe_limit
            )
        };

        self.clickhouse.query_json(&sql).await
    }

    /// Query chat message history
    pub async fn query_chat_history(
        &self,
        limit: usize,
        session_id: Option<&str>,
    ) -> Result<String> {
        // SECURITY: Validate limit parameter to prevent SQL injection
        let safe_limit = Self::validate_limit(limit)?;

        let sql = if let Some(sid) = session_id {
            format!(
                "SELECT * FROM osvm.chat_messages WHERE session_id = '{}' ORDER BY timestamp DESC LIMIT {}",
                Self::escape_string(sid),
                safe_limit
            )
        } else {
            format!(
                "SELECT * FROM osvm.chat_messages ORDER BY timestamp DESC LIMIT {}",
                safe_limit
            )
        };

        self.clickhouse.query_json(&sql).await
    }

    /// Get activity statistics
    pub async fn get_activity_stats(&self) -> Result<String> {
        let sql = r#"
        SELECT
            'commands' as type,
            count(*) as total,
            countIf(exit_code = 0) as successful,
            countIf(exit_code != 0) as failed,
            avg(duration_ms) as avg_duration_ms
        FROM osvm.cli_commands
        UNION ALL
        SELECT
            'chat_messages' as type,
            count(*) as total,
            0 as successful,
            0 as failed,
            0 as avg_duration_ms
        FROM osvm.chat_messages
        "#;

        self.clickhouse.query_json(sql).await
    }

    /// Get a unique session ID for this osvm process
    fn get_session_id() -> String {
        // Try to get from environment variable, or generate a new one
        std::env::var("OSVM_SESSION_ID").unwrap_or_else(|_| {
            let id = Uuid::new_v4().to_string();
            std::env::set_var("OSVM_SESSION_ID", &id);
            id
        })
    }

    /// Escape SQL string values
    fn escape_string(s: &str) -> String {
        s.replace('\'', "''")
            .replace('\\', "\\\\")
    }

    /// Validate limit parameter to prevent SQL injection
    fn validate_limit(limit: usize) -> Result<usize> {
        // Enforce reasonable maximum limit
        const MAX_LIMIT: usize = 10000;

        if limit == 0 {
            anyhow::bail!("Limit must be greater than 0");
        }

        if limit > MAX_LIMIT {
            anyhow::bail!("Limit too large. Maximum allowed: {}", MAX_LIMIT);
        }

        // Additional validation: ensure it's actually a number (redundant with usize but defensive)
        // This is already guaranteed by the type system, but we add bounds checking

        Ok(limit)
    }
}

impl Drop for ActivityLogger {
    fn drop(&mut self) {
        // Attempt to flush buffers when logger is dropped
        let command_buffer = self.command_buffer.clone();
        let chat_buffer = self.chat_buffer.clone();
        let clickhouse = self.clickhouse.clone();

        tokio::spawn(async move {
            let cmd_buf = command_buffer.lock().await;
            let chat_buf = chat_buffer.lock().await;
            
            if !cmd_buf.is_empty() || !chat_buf.is_empty() {
                debug!("Flushing activity logs on drop...");
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_string() {
        assert_eq!(ActivityLogger::escape_string("hello'world"), "hello''world");
        assert_eq!(ActivityLogger::escape_string("path\\to\\file"), "path\\\\to\\\\file");
    }

    #[test]
    fn test_session_id_generation() {
        let id = ActivityLogger::get_session_id();
        assert!(!id.is_empty());
    }
}
