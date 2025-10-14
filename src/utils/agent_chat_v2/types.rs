//! Core data structures and types for the advanced agent chat interface

use crate::services::ai_service::PlannedTool;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use uuid::Uuid;

/// Message types in the advanced chat interface
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ChatMessage {
    User(String),
    Agent(String),
    System(String),
    ToolCall {
        tool_name: String,
        description: String,
        args: Option<Value>,
        execution_id: String,
    },
    ToolResult {
        tool_name: String,
        result: Value,
        execution_id: String,
    },
    Error(String),
    AgentThinking(String),
    AgentPlan(Vec<PlannedTool>),
    Processing {
        message: String,
        spinner_index: usize,
    }, // Shows processing with spinner
}

impl ChatMessage {
    /// Get a human-readable timestamp for this message
    /// Returns a formatted time string like "14:32:15"
    pub fn get_display_timestamp() -> String {
        Utc::now().format("%H:%M:%S").to_string()
    }

    /// Get the message type as a string for display
    pub fn message_type(&self) -> &str {
        match self {
            ChatMessage::User(_) => "User",
            ChatMessage::Agent(_) => "Agent",
            ChatMessage::System(_) => "System",
            ChatMessage::ToolCall { .. } => "Tool Call",
            ChatMessage::ToolResult { .. } => "Tool Result",
            ChatMessage::Error(_) => "Error",
            ChatMessage::AgentThinking(_) => "Thinking",
            ChatMessage::AgentPlan(_) => "Plan",
            ChatMessage::Processing { .. } => "Processing",
        }
    }
}

/// Agent execution state
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum AgentState {
    Idle,
    Thinking,
    Planning,
    ExecutingTool(String),
    Waiting,
    Paused,
    Error(String),
}
