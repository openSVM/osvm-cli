//! Core data structures and types for the advanced agent chat interface

use serde::{Serialize, Deserialize};
use serde_json::Value;
use uuid::Uuid;
use chrono::{DateTime, Utc};
use crate::services::ai_service::PlannedTool;

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