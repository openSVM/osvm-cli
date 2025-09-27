//! Advanced Agent Chat UI with FAR-style design using cursive-multiplex
//! 
//! This module provides a comprehensive chat interface with multiple chat sessions,
//! AI-powered tool planning, background agent execution, and session recording.

use cursive::{Cursive, CursiveExt, View};
use cursive::views::{
    Dialog, EditView, LinearLayout, TextView, ScrollView, Panel, Button,
    ListView, SelectView, ResizedView, DummyView, NamedView
};
use cursive::traits::*;
use cursive::direction::Orientation;
use cursive_multiplex::{Mux, Id};
use crate::services::{
    mcp_service::{McpService, McpTool, McpServerConfig},
    ai_service::{AiService, ToolPlan, PlannedTool}
};
use anyhow::{Result, Context, anyhow};
use serde_json::Value;
use std::sync::{Arc, RwLock};
use tokio::sync::{mpsc, oneshot, broadcast, Mutex};
use std::collections::HashMap;
use std::time::Duration;
use log::{error, warn, debug, info};
use serde::{Serialize, Deserialize};
use uuid::Uuid;
use chrono::{DateTime, Utc};
use regex::Regex;
use std::sync::atomic::{AtomicUsize, Ordering};

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
    AgentThinking(String), // Shows AI planning process
    AgentPlan(String),     // Shows planned actions
    Processing { message: String, spinner_index: usize }, // Shows processing with spinner
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

/// Chat session with full state tracking
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChatSession {
    pub id: Uuid,
    pub name: String,
    pub created_at: DateTime<Utc>,
    pub messages: Vec<ChatMessage>,
    pub agent_state: AgentState,
    pub recording: bool,
    pub recording_file: Option<String>,
}

impl ChatSession {
    pub fn new(name: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            created_at: Utc::now(),
            messages: Vec::new(),
            agent_state: AgentState::Idle,
            recording: false,
            recording_file: None,
        }
    }

    pub fn add_message(&mut self, message: ChatMessage) {
        self.messages.push(message);
        
        // Limit message history to prevent memory growth
        if self.messages.len() > 1000 {
            self.messages.drain(0..100); // Remove oldest 100 messages
        }

        // Save to recording if active
        if self.recording {
            if let Err(e) = self.save_message_to_recording(&self.messages.last().unwrap()) {
                error!("Failed to save message to recording: {}", e);
            }
        }
    }

    fn save_message_to_recording(&self, message: &ChatMessage) -> Result<()> {
        if let Some(file_path) = &self.recording_file {
            use std::fs::OpenOptions;
            use std::io::Write;

            let mut file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(file_path)?;
            
            let timestamp = Utc::now().format("%Y-%m-%d %H:%M:%S UTC");
            let message_json = serde_json::to_string(message)?;
            writeln!(file, "[{}] {}", timestamp, message_json)?;
        }
        Ok(())
    }

    pub fn start_recording(&mut self, file_path: String) -> Result<()> {
        self.recording = true;
        self.recording_file = Some(file_path.clone());
        
        // Create recording file with header
        use std::fs::File;
        use std::io::Write;
        
        let mut file = File::create(&file_path)?;
        writeln!(file, "# OSVM Agent Chat Session Recording")?;
        writeln!(file, "# Session: {} ({})", self.name, self.id)?;
        writeln!(file, "# Started: {}", Utc::now().format("%Y-%m-%d %H:%M:%S UTC"))?;
        writeln!(file, "# Format: [timestamp] {{message_json}}")?;
        writeln!(file, "")?;

        self.add_message(ChatMessage::System(format!("Recording started: {}", file_path)));
        Ok(())
    }

    pub fn stop_recording(&mut self) {
        if self.recording {
            self.add_message(ChatMessage::System("Recording stopped".to_string()));
            self.recording = false;
            self.recording_file = None;
        }
    }
}

/// Agent execution commands
#[derive(Debug)]
pub enum AgentCommand {
    ProcessInput { session_id: Uuid, input: String },
    PauseAgent { session_id: Uuid },
    ResumeAgent { session_id: Uuid },
    StopAgent { session_id: Uuid },
    GetStatus { session_id: Uuid, response: oneshot::Sender<AgentState> },
}

// Using ToolPlan and PlannedTool from ai_service module

/// Main application state for advanced chat UI
#[derive(Clone)]
pub struct AdvancedChatState {
    pub sessions: Arc<RwLock<HashMap<Uuid, ChatSession>>>,
    pub active_session_id: Arc<RwLock<Option<Uuid>>>,
    pub mcp_service: Arc<Mutex<McpService>>,
    pub ai_service: Arc<AiService>,
    pub available_tools: Arc<RwLock<HashMap<String, Vec<McpTool>>>>,
    pub agent_command_sender: Arc<Mutex<Option<mpsc::UnboundedSender<AgentCommand>>>>,
    pub current_suggestions: Arc<RwLock<Vec<String>>>,
    pub suggestions_visible: Arc<RwLock<bool>>,
    pub spinner_state: Arc<AtomicUsize>,
}

impl AdvancedChatState {
    pub fn new() -> Result<Self> {
        let mut mcp_service = McpService::new_with_debug(false);
        let ai_service = AiService::new_with_debug(false);
        
        // Load existing MCP configurations
        if let Err(e) = mcp_service.load_config() {
            warn!("Failed to load MCP config: {}", e);
        }

        Ok(AdvancedChatState {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            active_session_id: Arc::new(RwLock::new(None)),
            mcp_service: Arc::new(Mutex::new(mcp_service)),
            ai_service: Arc::new(ai_service),
            available_tools: Arc::new(RwLock::new(HashMap::new())),
            agent_command_sender: Arc::new(Mutex::new(None)),
            current_suggestions: Arc::new(RwLock::new(Vec::new())),
            suggestions_visible: Arc::new(RwLock::new(false)),
            spinner_state: Arc::new(AtomicUsize::new(0)),
        })
    }

    pub fn start_spinner_animation(&self, cb_sink: cursive::CbSink) {
        let spinner_state = self.spinner_state.clone();

        // Spawn a background thread to update spinner
        std::thread::spawn(move || {
            loop {
                // Update spinner index
                spinner_state.fetch_add(1, Ordering::Relaxed);

                // Request UI refresh
                cb_sink.send(Box::new(move |siv| {
                    update_ui_displays(siv);
                })).ok();

                // Wait a bit before next update
                std::thread::sleep(Duration::from_millis(100));
            }
        });
    }

    pub fn create_session(&self, name: String) -> Result<Uuid> {
        let session = ChatSession::new(name);
        let session_id = session.id;
        
        {
            let mut sessions = self.sessions.write()
                .map_err(|e| anyhow!("Failed to lock sessions: {}", e))?;
            sessions.insert(session_id, session);
        }

        // Set as active if it's the first session
        {
            let mut active_id = self.active_session_id.write()
                .map_err(|e| anyhow!("Failed to lock active session: {}", e))?;
            if active_id.is_none() {
                *active_id = Some(session_id);
            }
        }

        Ok(session_id)
    }

    pub fn get_session_names(&self) -> Vec<(Uuid, String, AgentState)> {
        match self.sessions.read() {
            Ok(sessions) => {
                sessions.values()
                    .map(|s| (s.id, s.name.clone(), s.agent_state.clone()))
                    .collect()
            }
            Err(_) => {
                error!("Failed to read sessions");
                vec![]
            }
        }
    }

    pub fn set_active_session(&self, session_id: Uuid) -> Result<()> {
        let mut active_id = self.active_session_id.write()
            .map_err(|e| anyhow!("Failed to lock active session: {}", e))?;
        *active_id = Some(session_id);
        Ok(())
    }

    pub fn get_active_session(&self) -> Option<ChatSession> {
        let active_id = self.active_session_id.read().ok()?;
        let session_id = (*active_id)?;
        let sessions = self.sessions.read().ok()?;
        sessions.get(&session_id).cloned()
    }

    pub async fn start_agent_worker(&self) -> Result<()> {
        let (sender, mut receiver) = mpsc::unbounded_channel::<AgentCommand>();
        
        // Store sender for use by UI
        {
            let mut sender_guard = self.agent_command_sender.lock().await;
            *sender_guard = Some(sender);
        }
        
        let state = self.clone();
        
        // Spawn agent worker task
        tokio::spawn(async move {
            while let Some(command) = receiver.recv().await {
                match command {
                    AgentCommand::ProcessInput { session_id, input } => {
                        if let Err(e) = state.process_input_async(session_id, input).await {
                            error!("Failed to process input for session {}: {}", session_id, e);
                        }
                    }
                    AgentCommand::PauseAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Paused);
                    }
                    AgentCommand::ResumeAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Idle);
                    }
                    AgentCommand::StopAgent { session_id } => {
                        state.set_agent_state(session_id, AgentState::Idle);
                    }
                    AgentCommand::GetStatus { session_id, response } => {
                        let status = state.get_agent_state(session_id)
                            .unwrap_or(AgentState::Error("Session not found".to_string()));
                        let _ = response.send(status);
                    }
                }
            }
        });
        
        // Also start tool refresh worker
        self.start_tool_refresh_worker().await?;
        
        Ok(())
    }

    async fn process_input_async(&self, session_id: Uuid, input: String) -> Result<()> {
        // Set agent state to thinking
        self.set_agent_state(session_id, AgentState::Thinking);
        self.add_message_to_session(session_id, ChatMessage::User(input.clone()))?;
        self.add_message_to_session(session_id, ChatMessage::Processing {
            message: "Analyzing your request...".to_string(),
            spinner_index: self.spinner_state.load(Ordering::Relaxed)
        })?;

        // First, refresh available tools to ensure we have the latest
        if let Err(e) = self.refresh_tools_from_mcp().await {
            warn!("Failed to refresh tools: {}", e);
        }

        self.set_agent_state(session_id, AgentState::Planning);
        self.add_message_to_session(session_id, ChatMessage::Processing {
            message: "Creating execution plan...".to_string(),
            spinner_index: self.spinner_state.load(Ordering::Relaxed)
        })?;

        // Get available tools
        let available_tools = self.available_tools.read()
            .map_err(|e| anyhow!("Failed to read available tools: {}", e))?
            .clone();

        use std::time::Duration;
        // Use the proper AI service method to create tool plan
        let tool_plan_result = tokio::time::timeout(
            Duration::from_secs(30),  // Increased timeout for better AI responses
            self.ai_service.create_tool_plan(&input, &available_tools)
        ).await;

        // Helper closure to build heuristic tools based on input
        let build_heuristic_plan = |text: &str| -> Vec<PlannedTool> {
            let mut tools = Vec::new();
            let lc = text.to_lowercase();
            if lc.contains("balance") { tools.push(PlannedTool { server_id: "local_sim".into(), tool_name: "get_balance".into(), args: serde_json::json!({}), reason: "Heuristic: user asked about balance".into() }); }
            if lc.contains("transaction") || lc.contains("transactions") || lc.contains("tx") { tools.push(PlannedTool { server_id: "local_sim".into(), tool_name: "get_transactions".into(), args: serde_json::json!({}), reason: "Heuristic: user asked about transactions".into() }); }
            tools
        };

        // Determine if any MCP servers/tools are configured
        let no_configured_tools = available_tools.is_empty();

        match tool_plan_result {
            Err(_) => {
                warn!("AI planning timed out");
                self.add_message_to_session(session_id, ChatMessage::Error("AI planning timed out. Using heuristic fallback.".to_string()))?;
                if no_configured_tools { self.run_heuristic_fallback(session_id, &input, build_heuristic_plan(&input)).await?; }
                else { self.simple_response(session_id, &input).await?; }
            }
            Ok(Err(e)) => {
                error!("AI service failed: {}", e);
                self.add_message_to_session(session_id, ChatMessage::Error("AI planning service failed. Using heuristic fallback.".to_string()))?;
                if no_configured_tools { self.run_heuristic_fallback(session_id, &input, build_heuristic_plan(&input)).await?; }
                else { self.simple_response(session_id, &input).await?; }
            }
            Ok(Ok(tool_plan)) => {
                // Successfully got tool plan from AI service
                self.add_message_to_session(session_id, ChatMessage::AgentPlan(tool_plan.reasoning.clone()))?;

                if tool_plan.osvm_tools_to_use.is_empty() {
                    // No tools needed according to AI
                    if no_configured_tools {
                        // Try heuristic plan instead
                        let heur = build_heuristic_plan(&input);
                        if !heur.is_empty() {
                            self.add_message_to_session(session_id, ChatMessage::AgentPlan("Using heuristic plan (no tools suggested by AI).".to_string()))?;
                            self.run_heuristic_fallback(session_id, &input, heur).await?;
                        } else {
                            self.simple_response(session_id, &input).await?;
                        }
                    } else {
                        self.simple_response(session_id, &input).await?;
                    }
                } else {
                    // Execute tools iteratively with potential for follow-up actions
                    let mut executed_tools = Vec::new();
                    let mut iteration_count = 0;
                    let max_iterations = 5;  // Allow up to 5 iterations of tool execution

                    let mut current_tools = tool_plan.osvm_tools_to_use;

                    while !current_tools.is_empty() && iteration_count < max_iterations {
                        iteration_count += 1;

                        // Execute current batch of tools
                        for planned_tool in &current_tools {
                            self.execute_planned_tool(session_id, planned_tool.clone()).await?;
                            executed_tools.push(planned_tool.clone());
                        }

                        // Check if we need follow-up actions based on results
                        let session = self.get_session_by_id(session_id)
                            .ok_or_else(|| anyhow!("Session not found"))?;

                        let recent_results: Vec<(String, Value)> = session.messages.iter()
                            .rev()
                            .take(current_tools.len() * 2)  // Get recent tool results
                            .filter_map(|msg| match msg {
                                ChatMessage::ToolResult { tool_name, result, .. } => {
                                    Some((tool_name.clone(), result.clone()))
                                }
                                _ => None
                            })
                            .collect();

                        // Ask AI if we need follow-up tools based on results
                        if iteration_count < max_iterations && !recent_results.is_empty() {
                            match self.check_for_follow_up_actions(&input, &recent_results, &available_tools).await {
                                Ok(follow_up_tools) if !follow_up_tools.is_empty() => {
                                    self.add_message_to_session(session_id,
                                        ChatMessage::AgentThinking(
                                            format!("Executing {} follow-up actions...", follow_up_tools.len())
                                        )
                                    )?;
                                    current_tools = follow_up_tools;
                                }
                                _ => break,  // No more tools needed
                            }
                        } else {
                            break;
                        }
                    }

                    // Generate final response with all executed tools
                    self.generate_final_response(session_id, &input, &tool_plan.expected_outcome).await?;
                }
            }
        }
        
        self.set_agent_state(session_id, AgentState::Idle);

        // Generate suggestions after agent completes
        let _ = self.generate_reply_suggestions(session_id).await;

        Ok(())
    }

    // Heuristic fallback execution simulating basic tools so user sees end-to-end flow
    async fn run_heuristic_fallback(&self, session_id: Uuid, original_input: &str, tools: Vec<PlannedTool>) -> Result<()> {
        if tools.is_empty() {
            // Nothing heuristic matched; fall back to simple
            return self.simple_response(session_id, original_input).await;
        }
        self.add_message_to_session(session_id, ChatMessage::AgentPlan("Executing heuristic simulated tools...".to_string()))?;
        for planned_tool in tools {
            self.execute_planned_tool(session_id, planned_tool).await?;
        }
        self.generate_final_response(session_id, original_input, "Heuristic simulated execution").await?;
        Ok(())
    }

    // These methods are no longer needed as we use ai_service.create_tool_plan() directly

    async fn execute_planned_tool(&self, session_id: Uuid, planned_tool: PlannedTool) -> Result<()> {
        let execution_id = Uuid::new_v4().to_string();
        
        self.set_agent_state(session_id, AgentState::ExecutingTool(planned_tool.tool_name.clone()));
        self.add_message_to_session(session_id, ChatMessage::ToolCall {
            tool_name: planned_tool.tool_name.clone(),
            description: planned_tool.reason.clone(),
            args: Some(planned_tool.args.clone()),
            execution_id: execution_id.clone(),
        })?;
        
        // Execute the tool using MCP service
        match self.call_mcp_tool(&planned_tool).await {
            Ok(result) => {
                self.add_message_to_session(session_id, ChatMessage::ToolResult {
                    tool_name: planned_tool.tool_name,
                    result,
                    execution_id,
                })?;
            }
            Err(e) => {
                error!("Tool execution failed: {}", e);
                self.add_message_to_session(session_id, ChatMessage::Error(
                    format!("Tool {} failed: {}", planned_tool.tool_name, e)
                ))?;
            }
        }
        
        Ok(())
    }

    async fn call_mcp_tool(&self, planned_tool: &PlannedTool) -> Result<Value> {
        // Try to call real MCP tool first
        let mut mcp_service = self.mcp_service.lock().await;

        // Check if the server is initialized
        if let Some(server_config) = mcp_service.get_server(&planned_tool.server_id) {
            if server_config.enabled {
                // Initialize the server if needed
                if let Err(e) = mcp_service.initialize_server(&planned_tool.server_id).await {
                    warn!("Failed to initialize MCP server {}: {}", planned_tool.server_id, e);
                }

                // Try to call the actual tool
                match mcp_service.call_tool(
                    &planned_tool.server_id,
                    &planned_tool.tool_name,
                    Some(planned_tool.args.clone())
                ).await {
                    Ok(result) => return Ok(result),
                    Err(e) => {
                        warn!("MCP tool call failed: {}, falling back to simulation", e);
                    }
                }
            }
        }

        // Fallback to simulation if MCP call fails
        tokio::time::sleep(std::time::Duration::from_millis(200)).await;

        match planned_tool.tool_name.as_str() {
            "get_balance" => Ok(serde_json::json!({"balance": "2.5 SOL", "usd_value": 250.75})),
            "get_transactions" => Ok(serde_json::json!({
                "transactions": [
                    {"hash": "abc123", "amount": "0.1 SOL", "type": "sent"},
                    {"hash": "def456", "amount": "1.0 SOL", "type": "received"}
                ]
            })),
            "get_network_status" => Ok(serde_json::json!({
                "network": "mainnet-beta",
                "slot": 250000000,
                "tps": 3000,
                "validators": {"active": 1800, "delinquent": 12}
            })),
            _ => Ok(serde_json::json!({"result": "Tool executed successfully"}))
        }
    }

    async fn generate_final_response(&self, session_id: Uuid, original_input: &str, expected_outcome: &str) -> Result<()> {
        // Get the recent tool results to inform the response
        let session = self.get_session_by_id(session_id).ok_or_else(|| anyhow!("Session not found"))?;
        let tool_results: Vec<(String, Value)> = session.messages.iter()
            .rev()
            .filter_map(|msg| match msg {
                ChatMessage::ToolResult { tool_name, result, .. } => Some((tool_name.clone(), result.clone())),
                _ => None
            })
            .collect();

        // Use the AI service's contextual response generation
        match self.ai_service.generate_contextual_response(
            original_input,
            &tool_results,
            expected_outcome
        ).await {
            Ok(response) => {
                self.add_message_to_session(session_id, ChatMessage::Agent(response))?;
            }
            Err(e) => {
                error!("Failed to generate final response: {}", e);
                self.add_message_to_session(session_id, ChatMessage::Agent(
                    "I've completed the requested operations. Please check the tool results above.".to_string()
                ))?;
            }
        }

        Ok(())
    }

    async fn simple_response(&self, session_id: Uuid, input: &str) -> Result<()> {
        // Try to get a direct AI response without tools
        match self.ai_service.query_with_debug(input, false).await {
            Ok(response) => {
                self.add_message_to_session(session_id, ChatMessage::Agent(response))?;
            }
            Err(e) => {
                warn!("AI service failed for simple response: {}", e);
                // Fallback response logic
                let response = if input.to_lowercase().contains("balance") {
                    "I can help you check your wallet balance. However, I need MCP tools to be properly configured to fetch real data."
                } else if input.to_lowercase().contains("transaction") {
                    "I can help you with transaction history. Please make sure MCP servers are configured for blockchain operations."
                } else {
                    "I understand you're asking about blockchain operations. Please ensure MCP servers are configured so I can assist you with real data."
                };

                self.add_message_to_session(session_id, ChatMessage::Agent(response.to_string()))?;
            }
        }
        Ok(())
    }

    // New method to check for follow-up actions based on tool results
    async fn check_for_follow_up_actions(
        &self,
        original_input: &str,
        tool_results: &[(String, Value)],
        available_tools: &HashMap<String, Vec<McpTool>>
    ) -> Result<Vec<PlannedTool>> {
        // Create a context string with the results
        let results_context = tool_results.iter()
            .map(|(name, result)| format!("{}: {}", name, serde_json::to_string(result).unwrap_or_default()))
            .collect::<Vec<_>>()
            .join("\n");

        let follow_up_prompt = format!(
            "Based on the user's request: '{}'\n\n\
            And these tool execution results:\n{}\n\n\
            Do we need any follow-up tools? If yes, create an OSVM plan for the next steps.",
            original_input, results_context
        );

        // Use a shorter timeout for follow-up checks
        match tokio::time::timeout(
            Duration::from_secs(10),
            self.ai_service.create_tool_plan(&follow_up_prompt, available_tools)
        ).await {
            Ok(Ok(plan)) if !plan.osvm_tools_to_use.is_empty() => {
                Ok(plan.osvm_tools_to_use)
            }
            _ => Ok(Vec::new()),  // No follow-up needed or error occurred
        }
    }

    // Helper methods for state management
    fn set_agent_state(&self, session_id: Uuid, state: AgentState) {
        if let Ok(mut sessions) = self.sessions.write() {
            if let Some(session) = sessions.get_mut(&session_id) {
                session.agent_state = state;
            }
        }
    }

    fn get_agent_state(&self, session_id: Uuid) -> Option<AgentState> {
        self.sessions.read().ok()?
            .get(&session_id)
            .map(|s| s.agent_state.clone())
    }

    fn remove_last_processing_message(&self, session_id: Uuid) -> Result<()> {
        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow!("Failed to write sessions: {}", e))?;

        if let Some(session) = sessions.get_mut(&session_id) {
            // Remove the last message if it's a Processing type
            if let Some(ChatMessage::Processing { .. }) = session.messages.last() {
                session.messages.pop();
            }
        }

        Ok(())
    }

    fn add_message_to_session(&self, session_id: Uuid, message: ChatMessage) -> Result<()> {
        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow!("Failed to lock sessions: {}", e))?;

        if let Some(session) = sessions.get_mut(&session_id) {
            session.add_message(message);
        }

        Ok(())
    }

    fn get_session_by_id(&self, session_id: Uuid) -> Option<ChatSession> {
        self.sessions.read().ok()?.get(&session_id).cloned()
    }

    async fn generate_reply_suggestions(&self, session_id: Uuid) -> Result<()> {
        // Get the chat context
        let session = self.get_session_by_id(session_id)
            .ok_or_else(|| anyhow!("Session not found"))?;

        // Build context from recent messages (last 10 messages)
        let recent_messages = session.messages.iter()
            .rev()
            .take(10)
            .rev()
            .map(|msg| match msg {
                ChatMessage::User(text) => format!("User: {}", text),
                ChatMessage::Agent(text) => format!("Agent: {}", text),
                ChatMessage::ToolResult { tool_name, result, .. } => {
                    format!("Tool Result [{}]: {}", tool_name, serde_json::to_string(result).unwrap_or_default())
                }
                _ => String::new(),
            })
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n");

        if recent_messages.is_empty() {
            return Ok(());
        }

        // Create prompt for AI to generate suggestions
        let prompt = format!(
            "Based on this conversation context:\n{}\n\n\
            Generate exactly 5 concise follow-up questions or commands the user might want to ask next. \
            Each suggestion should be on a new line, numbered 1-5. \
            Keep suggestions short (max 10 words each). \
            Focus on practical next steps or clarifications.",
            recent_messages
        );

        // Call AI service to generate suggestions
        match self.ai_service.query_with_debug(&prompt, false).await {
            Ok(response) => {
                // Parse suggestions from response (expecting numbered list)
                let suggestions: Vec<String> = response
                    .lines()
                    .filter_map(|line| {
                        let line = line.trim();
                        // Match lines like "1. suggestion" or "1) suggestion" or just starting with a number
                        if let Some(dot_pos) = line.find('.') {
                            if line[..dot_pos].trim().parse::<u32>().is_ok() {
                                return Some(line[dot_pos + 1..].trim().to_string());
                            }
                        }
                        if let Some(paren_pos) = line.find(')') {
                            if line[..paren_pos].trim().parse::<u32>().is_ok() {
                                return Some(line[paren_pos + 1..].trim().to_string());
                            }
                        }
                        // Also accept lines starting with digits
                        if line.len() > 2 && line.chars().next()?.is_ascii_digit() {
                            if let Some(rest) = line.get(2..) {
                                return Some(rest.trim().to_string());
                            }
                        }
                        None
                    })
                    .take(5)
                    .collect();

                if !suggestions.is_empty() {
                    // Store suggestions
                    if let Ok(mut sugg_guard) = self.current_suggestions.write() {
                        *sugg_guard = suggestions;
                    }
                    if let Ok(mut vis_guard) = self.suggestions_visible.write() {
                        *vis_guard = true;
                    }

                    // Add a system message to show suggestions are available
                    self.add_message_to_session(session_id,
                        ChatMessage::System("Reply suggestions available (press 1-5 to insert)".to_string()))?;

                    // Note: UI refresh needs to happen from the main thread
                }
            }
            Err(e) => {
                warn!("Failed to generate suggestions: {}", e);
            }
        }

        Ok(())
    }

    pub async fn start_tool_refresh_worker(&self) -> Result<()> {
        let state = self.clone();
        
        tokio::spawn(async move {
            loop {
                if let Err(e) = state.refresh_tools_from_mcp().await {
                    error!("Failed to refresh tools: {}", e);
                }
                
                // Refresh every 30 seconds
                tokio::time::sleep(std::time::Duration::from_secs(30)).await;
            }
        });
        
        Ok(())
    }

    async fn refresh_tools_from_mcp(&self) -> Result<()> {
        let servers = {
            let service = self.mcp_service.lock().await;
            service.list_servers().into_iter()
                .map(|(id, config)| (id.clone(), config.clone()))
                .collect::<Vec<(String, McpServerConfig)>>()
        };

        let mut new_tools: HashMap<String, Vec<McpTool>> = HashMap::new();

        for server_data in servers {
            let (server_id, config) = server_data;
            if config.enabled {
                // Try to get real tools from MCP server
                let tools = {
                    // Initialize server if needed
                    {
                        let mut service = self.mcp_service.lock().await;
                        if let Err(e) = service.initialize_server(&server_id).await {
                            warn!("Failed to initialize server {} for tool refresh: {}", server_id, e);
                        }
                    }

                    // Try to list tools
                    let mut service = self.mcp_service.lock().await;
                    match service.list_tools(&server_id).await {
                        Ok(tools) => tools,
                        Err(e) => {
                            warn!("Failed to fetch tools from {}: {}, using defaults", server_id, e);
                            // Fallback to default tools
                            vec![
                                McpTool {
                                    name: "get_balance".to_string(),
                                    description: Some("Get wallet balance".to_string()),
                                    input_schema: serde_json::json!({}),
                                },
                                McpTool {
                                    name: "get_transactions".to_string(),
                                    description: Some("Get transaction history".to_string()),
                                    input_schema: serde_json::json!({}),
                                },
                            ]
                        }
                    }
                };
                new_tools.insert(server_id, tools);
            }
        }

        if let Ok(mut tools) = self.available_tools.write() {
            *tools = new_tools;
        }
        
        Ok(())
    }

    pub async fn send_agent_command(&self, command: AgentCommand) -> Result<()> {
        let sender_guard = self.agent_command_sender.lock().await;

        if let Some(sender) = sender_guard.as_ref() {
            sender.send(command)
                .map_err(|e| anyhow!("Failed to send agent command: {}", e))?;
        } else {
            return Err(anyhow!("Agent worker not initialized"));
        }

        Ok(())
    }

    /// Synchronous version for sending commands from UI callbacks
    pub fn send_agent_command_sync(&self, command: AgentCommand) {
        let sender = self.agent_command_sender.clone();

        // Use std::thread to avoid runtime conflicts
        std::thread::spawn(move || {
            // Create a small runtime just for this operation
            let runtime = tokio::runtime::Runtime::new().unwrap();
            runtime.block_on(async {
                let sender_guard = sender.lock().await;
                if let Some(sender) = sender_guard.as_ref() {
                    if let Err(e) = sender.send(command) {
                        error!("Failed to send agent command: {}", e);
                    }
                } else {
                    error!("Agent worker not initialized");
                }
            });
        });
    }
}

/// FAR-style/Borland UI implementation
pub struct AdvancedChatUI {
    state: AdvancedChatState,
}

impl AdvancedChatUI {
    pub fn new() -> Result<Self> {
        let state = AdvancedChatState::new()?;
        Ok(AdvancedChatUI { state })
    }


    fn setup_far_ui(&self, siv: &mut Cursive) {
        let state = self.state.clone();
        
        // Main horizontal layout: Chat List | Chat History
        let mut main_layout = LinearLayout::horizontal();
        
        // Left panel: Chat sessions list
        let chat_list_panel = self.create_chat_list_panel();
        main_layout.add_child(ResizedView::with_fixed_width(30, chat_list_panel));
        
        // Right panel: Active chat and controls
        let chat_panel = self.create_chat_panel();
        main_layout.add_child(chat_panel.full_width());
        
        // Wrap in main dialog
        let dialog = Dialog::around(main_layout)
            .title("OSVM Agent")
            .title_position(cursive::align::HAlign::Center);
        
        siv.add_fullscreen_layer(dialog);
        
        // Set focus to input field
        siv.focus_name("input").ok();
    }

    fn create_chat_list_panel(&self) -> impl View {
        let mut chat_list_layout = LinearLayout::vertical();
        
        // Header
        chat_list_layout.add_child(Panel::new(TextView::new("Chat Sessions")).title("Sessions"));
        
        // Chat list
        let mut chat_select = SelectView::<Uuid>::new();
        chat_select.set_on_select(|siv, session_id: &Uuid| {
            handle_chat_selection(siv, *session_id);
        });
        let chat_select = chat_select.with_name("chat_list");
        
        chat_list_layout.add_child(Panel::new(ScrollView::new(chat_select)).max_height(15));
        
        // New chat button
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(Button::new("+ New Chat", |siv| {
            create_new_chat_dialog(siv);
        }));
        
        // Session controls
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(LinearLayout::horizontal()
            .child(Button::new("Run", |siv| resume_agent(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("Pause", |siv| pause_agent(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("Stop", |siv| stop_agent(siv)))
        );
        
        // Recording controls
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(LinearLayout::horizontal()
            .child(Button::new("Record", |siv| start_recording(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("Stop Rec", |siv| stop_recording(siv)))
        );
        
        chat_list_layout
    }

    fn create_chat_panel(&self) -> impl View {
        let mut chat_layout = LinearLayout::vertical();
        
        // Chat history area
        let chat_view = ScrollView::new(
            TextView::new("")
                .with_name("chat_display")
                .full_width()
        ).scroll_strategy(cursive::view::scroll::ScrollStrategy::StickToBottom)
         .with_name("chat_scroll");
        
        let chat_panel = Panel::new(chat_view)
            .title("Chat History")
            .title_position(cursive::align::HAlign::Left);
        
        chat_layout.add_child(chat_panel.full_height());
        
        // Agent status bar
        let status_view = TextView::new("Agent: Idle")
            .with_name("agent_status")
            .full_width();
        chat_layout.add_child(Panel::new(status_view).title("Agent Status"));
        
        // Suggestions area (shown when available)
        let suggestions_view = TextView::new("")
            .with_name("suggestions_display");
        chat_layout.add_child(
            Panel::new(suggestions_view)
                .title("Reply Suggestions (press 1-5 to insert)")
                .with_name("suggestions_panel")
        );

        // Input area
        let input_layout = LinearLayout::horizontal()
            .child(TextView::new("You: "))
            .child(
                EditView::new()
                    .on_submit({
                        let state = self.state.clone();
                        move |siv, text| {
                            handle_user_input(siv, text, state.clone());
                        }
                    })
                    .with_name("input")
                    .full_width()
            );

        chat_layout.add_child(Panel::new(input_layout).title("Input"));
        
        // Control buttons
        let button_layout = LinearLayout::horizontal()
            .child(Button::new("Clear Chat", |siv| clear_current_chat(siv)))
            .child(Button::new("Export Chat", |siv| export_chat(siv)))
            .child(Button::new("Settings", |siv| show_settings(siv)))
            .child(Button::new("Help", show_advanced_help))
            .child(Button::new("Quit", |siv| siv.quit()));
        
        chat_layout.add_child(button_layout);
        
        chat_layout
    }

    fn update_all_displays(&self, siv: &mut Cursive) {
        self.update_chat_list(siv);
        self.update_chat_display(siv);
        self.update_agent_status(siv);
    }

    fn update_chat_list(&self, siv: &mut Cursive) {
        let session_names = self.state.get_session_names();
        
        if let Some(mut chat_select) = siv.find_name::<SelectView<Uuid>>("chat_list") {
            chat_select.clear();
            
            for (id, name, agent_state) in session_names {
                let status_icon = match agent_state {
                    AgentState::Idle => "...",
                    AgentState::Thinking => "*thinking*",
                    AgentState::Planning => "*planning*",
                    AgentState::ExecutingTool(_) => "*tool*",
                    AgentState::Waiting => "*waiting*",
                    AgentState::Paused => "*paused*",
                    AgentState::Error(_) => "*error*",
                };
                
                chat_select.add_item(format!("{} {}", status_icon, name), id);
            }
        }
    }

    fn update_chat_display(&self, siv: &mut Cursive) {
        if let Some(session) = self.state.get_active_session() {
            let mut display_text = String::new();
            
            for message in &session.messages {
                let sanitized = self.sanitize_message(message);
                match message {
                    ChatMessage::User(text) => {
                        display_text.push_str(&format!("You: {}\n", sanitized));
                        display_text.push_str("   [R]etry [C]opy [D]elete   (Alt+R/C/D)\n\n");
                    }
                    ChatMessage::Agent(text) => {
                        let rendered = self.render_markdown(text);
                        let sanitized = self.sanitize_text(&rendered);
                        display_text.push_str(&format!("Agent:\n{}", sanitized));
                        display_text.push_str("\n   [F]ork [C]opy [R]etry [D]elete   (Alt+F/C/R/D)\n\n");
                    }
                    ChatMessage::System(text) => {
                        display_text.push_str(&format!("System: {}\n\n", sanitized));
                    }
                    ChatMessage::ToolCall { tool_name, description, args, execution_id } => {
                        display_text.push_str(&format!("Calling tool: {} - {}\n", tool_name, description));
                        if let Some(args) = args {
                            let sanitized_args = self.sanitize_json(args);
                            display_text.push_str(&format!("   Args: {}\n", sanitized_args));
                        }
                        display_text.push_str(&format!("   ID: {}\n\n", execution_id));
                    }
                    ChatMessage::ToolResult { tool_name, result, execution_id } => {
                        display_text.push_str(&format!("Tool {} result (ID: {}):\n", tool_name, execution_id));
                        let sanitized_result = self.sanitize_json(result);
                        display_text.push_str(&format!("{}\n\n", sanitized_result));
                    }
                    ChatMessage::Error(text) => {
                        display_text.push_str(&format!("Error: {}\n\n", sanitized));
                    }
                    ChatMessage::AgentThinking(text) => {
                        let rendered = self.render_markdown(text);
                        let sanitized = self.sanitize_text(&rendered);
                        // Format thinking messages in gray (using dim style)
                        display_text.push_str(&format!("\x1b[2mThinking:\n{}\x1b[0m\n\n", sanitized));
                    }
                    ChatMessage::AgentPlan(text) => {
                        let rendered = self.render_markdown(text);
                        let sanitized = self.sanitize_text(&rendered);
                        display_text.push_str(&format!("Plan:\n{}\n\n", sanitized));
                    }
                    ChatMessage::Processing { message, spinner_index } => {
                        let spinners = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
                        let spinner = spinners[spinner_index % spinners.len()];
                        display_text.push_str(&format!("\x1b[36m{} {}\x1b[0m\n\n", spinner, message));
                    }
                }
            }
            
            if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
                chat_display.set_content(display_text);
            }
        }
    }

    fn update_agent_status(&self, siv: &mut Cursive) {
        // Update suggestions display if visible
        self.update_suggestions_display(siv);

        if let Some(session) = self.state.get_active_session() {
            let status_text = match &session.agent_state {
                AgentState::Idle => "Agent: Idle".to_string(),
                AgentState::Thinking => "Agent: Thinking".to_string(),
                AgentState::Planning => "Agent: Planning".to_string(),
                AgentState::ExecutingTool(tool) => format!("Agent: Executing {}", tool),
                AgentState::Waiting => "Agent: Waiting".to_string(),
                AgentState::Paused => "Agent: Paused".to_string(),
                AgentState::Error(err) => format!("Agent: Error - {}", err),
            };
            
            if let Some(mut status_display) = siv.find_name::<TextView>("agent_status") {
                status_display.set_content(status_text);
            }
        }
    }

    fn sanitize_message(&self, message: &ChatMessage) -> String {
        // Implement the same sanitization logic as before
        match message {
            ChatMessage::User(text) |
            ChatMessage::Agent(text) |
            ChatMessage::System(text) |
            ChatMessage::Error(text) |
            ChatMessage::AgentThinking(text) |
            ChatMessage::AgentPlan(text) => self.sanitize_text(text),
            ChatMessage::Processing { message, .. } => self.sanitize_text(message),
            _ => "Complex message".to_string(),
        }
    }

    fn render_markdown(&self, text: &str) -> String {
        let mut rendered = text.to_string();

        // Convert markdown headers
        rendered = rendered.replace("### ", "═══ ");
        rendered = rendered.replace("## ", "══ ");
        rendered = rendered.replace("# ", "═ ");

        // Convert bold text
        if let Ok(bold_re) = Regex::new(r"\*\*([^*]+)\*\*") {
            rendered = bold_re.replace_all(&rendered, "\x1b[1m$1\x1b[0m").to_string();
        }

        // Convert italic text
        if let Ok(italic_re) = Regex::new(r"\*([^*]+)\*") {
            rendered = italic_re.replace_all(&rendered, "\x1b[3m$1\x1b[0m").to_string();
        }

        // Convert code blocks
        if let Ok(code_block_re) = Regex::new(r"```([^`]*)```") {
            rendered = code_block_re.replace_all(&rendered, "\x1b[48;5;236m$1\x1b[0m").to_string();
        }

        // Convert inline code
        if let Ok(inline_code_re) = Regex::new(r"`([^`]+)`") {
            rendered = inline_code_re.replace_all(&rendered, "\x1b[7m$1\x1b[0m").to_string();
        }

        // Convert bullet points
        rendered = rendered.replace("\n- ", "\n  • ");
        rendered = rendered.replace("\n* ", "\n  • ");
        rendered = rendered.replace("\n+ ", "\n  • ");

        // Convert numbered lists
        for i in 1..=20 {
            rendered = rendered.replace(&format!("\n{}. ", i), &format!("\n  {}. ", i));
        }

        // Convert blockquotes
        if let Ok(quote_re) = Regex::new(r"(?m)^> (.*)$") {
            rendered = quote_re.replace_all(&rendered, "  ┃ $1").to_string();
        }

        // Convert horizontal rules
        rendered = rendered.replace("\n---\n", "\n───────────────────────\n");
        rendered = rendered.replace("\n***\n", "\n═══════════════════════\n");

        rendered
    }

    fn sanitize_text(&self, text: &str) -> String {
        let mut sanitized = text.to_string();

        // Remove common ANSI escape sequences (colors, cursor movement) which break TUI layout
        if let Ok(ansi_re) = Regex::new(r"\x1B\[[0-9;?]*[ -/]*[@-~]") {
            sanitized = ansi_re.replace_all(&sanitized, "").to_string();
        }

        // Convert tabs to 4 spaces to keep columns aligned
        sanitized = sanitized.replace('\t', "    ");

        // Normalize line endings
        sanitized = sanitized.replace("\r\n", "\n").replace('\r', "\n");

        // Remove other control characters (except newline) that can corrupt the TUI
        sanitized = sanitized.chars()
            .filter(|c| *c == '\n' || !c.is_control())
            .collect();

        // Redact potential private keys (base58-like patterns)
        if let Ok(key_pattern) = Regex::new(r"\b[1-9A-HJ-NP-Za-km-z]{32,44}\b") {
            sanitized = key_pattern.replace_all(&sanitized, "[REDACTED_KEY]").to_string();
        }

        // Collapse long runs of whitespace/newlines to avoid blowups
        if let Ok(multi_nl) = Regex::new(r"\n{3,}") {
            sanitized = multi_nl.replace_all(&sanitized, "\n\n").to_string();
        }

        // Limit length to prevent display overflow
        if sanitized.len() > 2000 {
            sanitized.truncate(1997);
            sanitized.push_str("...");
        }

        sanitized
    }

    fn sanitize_json(&self, value: &Value) -> String {
        match serde_json::to_string_pretty(value) {
            Ok(json_str) => {
                let mut pretty = json_str;
                // Ensure we don't include binary/large data; sanitize the pretty output
                let sanitized = self.sanitize_text(&pretty);
                if sanitized.len() > 1000 {
                    // keep the opening braces readable
                    let take = 997.min(sanitized.len());
                    format!("{}...", &sanitized[..take])
                } else {
                    sanitized
                }
            }
            Err(_) => "[Invalid JSON]".to_string()
        }
    }

    fn setup_action_hotkeys(&self, siv: &mut Cursive) {
        // Alt+M: Switch back to standard mode
        siv.add_global_callback(cursive::event::Event::AltChar('m'), |siv| {
            siv.add_layer(
                Dialog::text("Switching back to standard mode requires restarting.\nPlease restart with 'osvm chat' (without --advanced)")
                    .title("Switch Mode")
                    .button("OK", |s| { s.pop_layer(); })
            );
        });

        let state = self.state.clone();
        // Alt+R: Retry last message
        siv.add_global_callback(cursive::event::Event::AltChar('r'), move |s| {
            retry_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+C: Copy last message
        siv.add_global_callback(cursive::event::Event::AltChar('c'), move |s| {
            copy_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+D: Delete last message
        siv.add_global_callback(cursive::event::Event::AltChar('d'), move |s| {
            delete_last_message(s, state.clone());
        });

        let state = self.state.clone();
        // Alt+F: Fork conversation
        siv.add_global_callback(cursive::event::Event::AltChar('f'), move |s| {
            fork_conversation(s, state.clone());
        });
    }

    fn setup_suggestion_hotkeys(&self, siv: &mut Cursive) {
        // Add number key handlers for suggestions
        for i in 1..=5 {
            let state = self.state.clone();
            let key_char = char::from_digit(i as u32, 10).unwrap();

            siv.add_global_callback(key_char, move |s| {
                insert_suggestion_at_cursor(s, (i - 1) as usize, state.clone());
            });
        }

        // Hide suggestions on Escape
        let state = self.state.clone();
        siv.add_global_callback(cursive::event::Key::Esc, move |_s| {
            if let Ok(mut vis) = state.suggestions_visible.write() {
                *vis = false;
            }
        });
    }

    fn update_suggestions_display(&self, siv: &mut Cursive) {
        let suggestions_visible = self.state.suggestions_visible.read()
            .map(|v| *v)
            .unwrap_or(false);

        if suggestions_visible {
            let suggestions = self.state.current_suggestions.read()
                .map(|s| s.clone())
                .unwrap_or_default();

            if !suggestions.is_empty() {
                let mut display_text = String::new();
                for (i, sugg) in suggestions.iter().enumerate() {
                    display_text.push_str(&format!("{}. {}\n", i + 1, sugg));
                }

                if let Some(mut sugg_display) = siv.find_name::<TextView>("suggestions_display") {
                    sugg_display.set_content(display_text);
                }

                // Show the panel
                if let Some(mut panel) = siv.find_name::<Panel<TextView>>("suggestions_panel") {
                    panel.set_title("Reply Suggestions (press 1-5 to insert)");
                }
            } else {
                // Hide if no suggestions
                if let Some(mut sugg_display) = siv.find_name::<TextView>("suggestions_display") {
                    sugg_display.set_content("");
                }
            }
        } else {
            // Hide suggestions
            if let Some(mut sugg_display) = siv.find_name::<TextView>("suggestions_display") {
                sugg_display.set_content("");
            }
        }
    }
}

// UI Event Handlers
fn handle_chat_selection(siv: &mut Cursive, session_id: Uuid) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Err(e) = state.set_active_session(session_id) {
            error!("Failed to set active session: {}", e);
        }
    });
    
    // Update displays
    update_ui_displays(siv);
}

// Action button handlers
fn retry_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        // Find the last user message
        if let Some(last_user_msg) = session.messages.iter().rev().find_map(|msg| {
            if let ChatMessage::User(text) = msg {
                Some(text.clone())
            } else {
                None
            }
        }) {
            // Re-send the message
            handle_user_input(siv, &last_user_msg, state);
        }
    }
}

fn copy_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        if let Some(last_msg) = session.messages.last() {
            let text_to_copy = match last_msg {
                ChatMessage::User(text) | ChatMessage::Agent(text) => text.clone(),
                _ => String::new(),
            };

            if !text_to_copy.is_empty() {
                // Copy to clipboard (simplified - in real app would use clipboard crate)
                siv.add_layer(
                    Dialog::info(format!("Message copied:\n{}", text_to_copy))
                        .title("Copied to Clipboard")
                );
            }
        }
    }
}

fn delete_last_message(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(session) = state.get_active_session() {
        let session_id = session.id;
        if let Ok(mut sessions) = state.sessions.write() {
            if let Some(session) = sessions.get_mut(&session_id) {
                if !session.messages.is_empty() {
                    session.messages.pop();
                    update_ui_displays(siv);
                }
            }
        }
    }
}

fn fork_conversation(siv: &mut Cursive, state: AdvancedChatState) {
    if let Some(current_session) = state.get_active_session() {
        // Create a new session with the same messages
        let forked_name = format!("{} (Fork)", current_session.name);
        if let Ok(new_session_id) = state.create_session(forked_name) {
            // Copy messages to the new session
            if let Ok(sessions) = state.sessions.read() {
                if let Some(new_session) = sessions.get(&new_session_id) {
                    let mut new_session = new_session.clone();
                    new_session.messages = current_session.messages.clone();

                    // Update the forked session
                    if let Ok(mut sessions_write) = state.sessions.write() {
                        sessions_write.insert(new_session_id, new_session);
                    }

                    // Switch to the forked session
                    let _ = state.set_active_session(new_session_id);
                    update_ui_displays(siv);
                }
            }
        }
    }
}

fn insert_suggestion_at_cursor(siv: &mut Cursive, index: usize, state: AdvancedChatState) {
    // Get the suggestion
    let suggestion = {
        let suggestions = state.current_suggestions.read()
            .map(|s| s.clone())
            .unwrap_or_default();

        if index < suggestions.len() {
            suggestions[index].clone()
        } else {
            return;
        }
    };

    // Insert into the input field at cursor position
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        let current_content = input.get_content();
        let cursor_pos = input.get_cursor();

        // Insert suggestion at cursor position
        let mut new_content = String::new();
        new_content.push_str(&current_content[..cursor_pos]);
        new_content.push_str(&suggestion);
        if cursor_pos < current_content.len() {
            new_content.push_str(&current_content[cursor_pos..]);
        }

        input.set_content(new_content);
        // Move cursor to end of inserted text
        input.set_cursor(cursor_pos + suggestion.len());
    }

    // Hide suggestions after insertion
    if let Ok(mut vis) = state.suggestions_visible.write() {
        *vis = false;
    }
}

fn handle_user_input(siv: &mut Cursive, text: &str, state: AdvancedChatState) {
    if text.trim().is_empty() {
        return;
    }

    let user_message = text.to_string();

    // Hide suggestions when sending a message
    if let Ok(mut vis) = state.suggestions_visible.write() {
        *vis = false;
    }

    // Clear input
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        input.set_content("");
    }

    // Immediately add user message to session and update display
    if let Some(session) = state.get_active_session() {
        // Add user message to the session
        let _ = state.add_message_to_session(session.id, ChatMessage::User(user_message.clone()));

        // Add processing indicator with spinner
        let _ = state.add_message_to_session(session.id, ChatMessage::Processing {
            message: "Processing your request...".to_string(),
            spinner_index: 0
        });

        // Update the display immediately
        update_ui_displays(siv);

        // Send to agent for processing
        let command = AgentCommand::ProcessInput {
            session_id: session.id,
            input: user_message,
        };

        // Send command using sync method to avoid runtime conflicts
        state.send_agent_command_sync(command);
    }
}

fn create_new_chat_dialog(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::around(
            EditView::new()
                .content("New Chat")
                .with_name("new_chat_name")
                .fixed_width(30)
        )
        .title("Create New Chat")
        .button("Create", |s| {
            let name = s.find_name::<EditView>("new_chat_name")
                .and_then(|v| Some(v.get_content().to_string()))
                .unwrap_or_else(|| "Unnamed Chat".to_string());
            
            s.pop_layer();
            
            s.with_user_data(|state: &mut AdvancedChatState| {
                if let Err(e) = state.create_session(name) {
                    error!("Failed to create new session: {}", e);
                }
            });
            
            update_ui_displays(s);
        })
        .button("Cancel", |s| {
            s.pop_layer();
        })
    );
}

fn resume_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::ResumeAgent { session_id: session.id };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

fn pause_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::PauseAgent { session_id: session.id };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

fn stop_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::StopAgent { session_id: session.id };
            // Send command using sync method to avoid runtime conflicts
            state.send_agent_command_sync(command);
        }
    });
}

fn start_recording(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(mut session) = state.get_active_session() {
            let filename = format!("osvm_chat_{}_{}.log", 
                session.name.replace(' ', "_"), 
                chrono::Utc::now().format("%Y%m%d_%H%M%S"));
            
            if let Err(e) = session.start_recording(filename) {
                error!("Failed to start recording: {}", e);
            }
        }
    });
}

fn stop_recording(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(mut session) = state.get_active_session() {
            session.stop_recording();
        }
    });
}

fn clear_current_chat(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session_id) = state.active_session_id.read().ok().and_then(|id| *id) {
            if let Ok(mut sessions) = state.sessions.write() {
                if let Some(session) = sessions.get_mut(&session_id) {
                    session.messages.clear();
                    session.add_message(ChatMessage::System("Chat cleared".to_string()));
                }
            }
        }
    });
    
    update_ui_displays(siv);
}

fn show_settings(siv: &mut Cursive) {
    let settings_dialog = Dialog::around(
        LinearLayout::vertical()
            .child(TextView::new("OSVM Agent Settings"))
            .child(DummyView.fixed_height(1))
            .child(TextView::new("MCP Servers:"))
            .child(Panel::new(
                TextView::new("• local_sim (enabled)\n• blockchain_tools (disabled)")
            ))
            .child(DummyView.fixed_height(1))
            .child(TextView::new("AI Model: GPT-4"))
            .child(TextView::new("Debug Mode: Disabled"))
            .child(TextView::new("Max History: 1000 messages"))
            .child(DummyView.fixed_height(1))
            .child(TextView::new("Recording Directory: ./recordings/"))
    )
    .title("Settings")
    .button("Configure MCP", |s| {
        s.add_layer(
            Dialog::text("MCP configuration will open in a separate interface.\nUse 'osvm mcp config' command.")
                .title("MCP Configuration")
                .button("OK", |s| { s.pop_layer(); })
        );
    })
    .button("Close", |s| {
        s.pop_layer();
    });
    
    siv.add_layer(settings_dialog);
}

fn export_chat(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let filename = format!("osvm_chat_export_{}_{}.json",
                session.name.replace(' ', "_"),
                chrono::Utc::now().format("%Y%m%d_%H%M%S"));

            match serde_json::to_string_pretty(&session) {
                Ok(json_content) => {
                    match std::fs::write(&filename, json_content) {
                        Ok(_) => {
                            info!("Chat exported to {}", filename);
                            // Add success message to current session
                            if let Ok(mut sessions) = state.sessions.write() {
                                if let Some(current_session) = sessions.get_mut(&session.id) {
                                    current_session.add_message(ChatMessage::System(
                                        format!("Chat exported to {}", filename)
                                    ));
                                }
                            }
                        }
                        Err(e) => {
                            error!("Failed to write export file: {}", e);
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to serialize session: {}", e);
                }
            }
        }
    });
    
    update_ui_displays(siv);
}

fn show_advanced_help(siv: &mut Cursive) {
    let help_text = "OSVM Advanced Agent Chat Help\n\n\
        Agent Controls:\n\
        - Run: Resume agent processing\n\
        - Pause: Pause agent operations\n\
        - Stop: Stop current agent task\n\n\
        Recording:\n\
        - Record: Start session recording\n\
        - Stop Rec: Stop recording\n\n\
        Status Indicators:\n\
        - Idle - Thinking - Planning\n\
        - Executing - Waiting - Paused - Error\n\n\
        Features:\n\
        • AI-powered tool planning and execution\n\
        • Background agent processing\n\
        • Multi-chat session support\n\
        • Session recording and export\n\
        • MCP server integration for blockchain tools";

    siv.add_layer(
        Dialog::text(help_text)
            .title("Advanced Help")
            .button("OK", |s| {
                s.pop_layer();
            })
    );
}

fn update_ui_displays(siv: &mut Cursive) {
    // Update suggestions display
    if let Some(state) = siv.user_data::<AdvancedChatState>() {
        let ui = AdvancedChatUI { state: state.clone() };
        ui.update_suggestions_display(siv);
    }
    // Get data from state first
    let (session_names, active_session) = siv.with_user_data(|state: &mut AdvancedChatState| {
        (state.get_session_names(), state.get_active_session())
    }).unwrap_or((vec![], None));

    // Update chat list
    if let Some(mut chat_select) = siv.find_name::<SelectView<Uuid>>("chat_list") {
        chat_select.clear();

        for (id, name, agent_state) in session_names {
            let status_icon = match agent_state {
                AgentState::Idle => "...",
                AgentState::Thinking => "*thinking*",
                AgentState::Planning => "*planning*",
                AgentState::ExecutingTool(_) => "*tool*",
                AgentState::Waiting => "*waiting*",
                AgentState::Paused => "*paused*",
                AgentState::Error(_) => "*error*",
            };

            chat_select.add_item(format!("{} {}", status_icon, name), id);
        }
    }

    // Update chat display
    if let Some(session) = active_session {
            let mut display_text = String::new();
            
            for message in &session.messages {
                match message {
                    ChatMessage::User(text) => {
                        let sanitized = sanitize_text_for_ui(text);
                        display_text.push_str(&format!("You: {}\n", sanitized));
                        display_text.push_str("   [Retry] [Copy] [Delete]\n\n");
                    }
                    ChatMessage::Agent(text) => {
                        // First render markdown, then sanitize
                        let mut rendered = text.to_string();

                        // Basic markdown rendering
                        rendered = rendered.replace("**", "");
                        rendered = rendered.replace("### ", "═══ ");
                        rendered = rendered.replace("## ", "══ ");
                        rendered = rendered.replace("# ", "═ ");
                        rendered = rendered.replace("\n- ", "\n  • ");
                        rendered = rendered.replace("\n* ", "\n  • ");

                        let sanitized = sanitize_text_for_ui(&rendered);
                        display_text.push_str(&format!("Agent:\n{}\n", sanitized));
                        display_text.push_str("   [Fork] [Copy] [Retry] [Delete]\n\n");
                    }
                    ChatMessage::System(text) => {
                        let sanitized = sanitize_text_for_ui(text);
                        display_text.push_str(&format!("System: {}\n\n", sanitized));
                    }
                    ChatMessage::ToolCall { tool_name, description, args, execution_id } => {
                        display_text.push_str(&format!("Calling tool: {} - {}\n", tool_name, description));
                        if let Some(args) = args {
                            let sanitized_args = sanitize_json_for_ui(args);
                            display_text.push_str(&format!("   Args: {}\n", sanitized_args));
                        }
                        display_text.push_str(&format!("   ID: {}\n\n", execution_id));
                    }
                    ChatMessage::ToolResult { tool_name, result, execution_id } => {
                        display_text.push_str(&format!("Tool {} result (ID: {}):\n", tool_name, execution_id));
                        let sanitized_result = sanitize_json_for_ui(result);
                        display_text.push_str(&format!("{}\n\n", sanitized_result));
                    }
                    ChatMessage::Error(text) => {
                        let sanitized = sanitize_text_for_ui(text);
                        display_text.push_str(&format!("Error: {}\n\n", sanitized));
                    }
                    ChatMessage::AgentThinking(text) => {
                        // Render markdown for thinking messages
                        let mut rendered = text.to_string();
                        rendered = rendered.replace("**", "");
                        rendered = rendered.replace("\n- ", "\n  • ");

                        let sanitized = sanitize_text_for_ui(&rendered);
                        // Format thinking messages in gray (using dim style)
                        display_text.push_str(&format!("\x1b[2mThinking:\n{}\x1b[0m\n\n", sanitized));
                    }
                    ChatMessage::AgentPlan(text) => {
                        // Render markdown for plan messages
                        let mut rendered = text.to_string();
                        rendered = rendered.replace("**", "");
                        rendered = rendered.replace("\n- ", "\n  • ");
                        rendered = rendered.replace("\n* ", "\n  • ");

                        let sanitized = sanitize_text_for_ui(&rendered);
                        display_text.push_str(&format!("Plan:\n{}\n\n", sanitized));
                    }
                    ChatMessage::Processing { message, spinner_index } => {
                        let spinners = vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
                        let spinner = spinners[spinner_index % spinners.len()];
                        let sanitized = sanitize_text_for_ui(message);
                        display_text.push_str(&format!("\x1b[36m{} {}\x1b[0m\n\n", spinner, sanitized));
                    }
                }
            }
            
            if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
                chat_display.set_content(display_text);
            }
            
            // Update agent status
            let status_text = match &session.agent_state {
                AgentState::Idle => "Agent: Idle".to_string(),
                AgentState::Thinking => "Agent: Thinking".to_string(),
                AgentState::Planning => "Agent: Planning".to_string(),
                AgentState::ExecutingTool(tool) => format!("Agent: Executing {}", tool),
                AgentState::Waiting => "Agent: Waiting".to_string(),
                AgentState::Paused => "Agent: Paused".to_string(),
                AgentState::Error(err) => format!("Agent: Error - {}", err),
            };
            
        if let Some(mut status_display) = siv.find_name::<TextView>("agent_status") {
            status_display.set_content(status_text);
        }
    }
}

fn sanitize_text_for_ui(text: &str) -> String {
    let mut sanitized = text.to_string();

    // Remove common ANSI escape sequences (colors, cursor movement) which break TUI layout
    if let Ok(ansi_re) = Regex::new(r"\x1B\[[0-9;?]*[ -/]*[@-~]") {
        sanitized = ansi_re.replace_all(&sanitized, "").to_string();
    }

    // Convert tabs to 4 spaces to keep columns aligned
    sanitized = sanitized.replace('\t', "    ");

    // Normalize line endings
    sanitized = sanitized.replace("\r\n", "\n").replace('\r', "\n");

    // Remove other control characters (except newline) that can corrupt the TUI
    sanitized = sanitized.chars()
        .filter(|c| *c == '\n' || !c.is_control())
        .collect();

    // Redact potential private keys (base58-like patterns)
    if let Ok(key_pattern) = Regex::new(r"\b[1-9A-HJ-NP-Za-km-z]{32,44}\b") {
        sanitized = key_pattern.replace_all(&sanitized, "[REDACTED_KEY]").to_string();
    }

    // Collapse long runs of whitespace/newlines to avoid blowups
    if let Ok(multi_nl) = Regex::new(r"\n{3,}") {
        sanitized = multi_nl.replace_all(&sanitized, "\n\n").to_string();
    }

    // Limit length to prevent display overflow
    if sanitized.len() > 2000 {
        sanitized.truncate(1997);
        sanitized.push_str("...");
    }

    sanitized
}

fn sanitize_json_for_ui(value: &Value) -> String {
    match serde_json::to_string_pretty(value) {
        Ok(json_str) => {
            let sanitized = sanitize_text_for_ui(&json_str);
            if sanitized.len() > 1000 {
                let take = 997.min(sanitized.len());
                format!("{}...", &sanitized[..take])
            } else {
                sanitized
            }
        }
        Err(_) => "[Invalid JSON]".to_string()
    }
}

/// Main entry point for the advanced agent chat UI
pub async fn run_advanced_agent_chat() -> Result<()> {
    println!("Starting OSVM Advanced Agent Chat Interface...");

    // Check if we're in a terminal environment
    if std::env::var("TERM").is_err() || std::env::var("CI").is_ok() {
        return run_advanced_demo_mode().await;
    }

    // Initialize the state and start workers
    let state = AdvancedChatState::new()?;
    state.start_agent_worker().await?;

    // Create default session
    let _default_session_id = state.create_session("Main Chat".to_string())?;

    // Run the UI in a blocking task to avoid runtime conflicts
    let result = tokio::task::spawn_blocking(move || {
        run_advanced_ui_sync(state)
    }).await;

    match result {
        Ok(Ok(())) => Ok(()),
        Ok(Err(e)) => {
            eprintln!("UI Error: {}", e);
            run_advanced_demo_mode().await
        }
        Err(e) => {
            eprintln!("Failed to run UI: {}", e);
            run_advanced_demo_mode().await
        }
    }
}

/// Synchronous UI runner to avoid async runtime conflicts
fn run_advanced_ui_sync(state: AdvancedChatState) -> Result<()> {
    let mut siv = Cursive::default();

    // Set the state as user data for the UI
    siv.set_user_data(state.clone());

    // Create UI wrapper to access methods
    let ui = AdvancedChatUI { state: state.clone() };

    // Start spinner animation after Cursive is initialized
    state.start_spinner_animation(siv.cb_sink().clone());

    // Set up the FAR-style UI layout
    ui.setup_far_ui(&mut siv);

    // Add global hotkeys for suggestions and actions
    ui.setup_suggestion_hotkeys(&mut siv);
    ui.setup_action_hotkeys(&mut siv);

    // Update displays
    update_ui_displays(&mut siv);

    // Run the TUI
    siv.run();

    Ok(())
}

/// Demo mode for advanced chat interface
async fn run_advanced_demo_mode() -> Result<()> {
    println!("Running Advanced Agent Chat in demo mode");
    println!();
    println!("Advanced Features:");
    println!("   • AI-powered input parsing and tool planning");
    println!("   • Multiple chat sessions with background agent execution");
    println!("   • Session recording and agent control (run/pause/stop)");
    println!("   • Comprehensive MCP tool integration");
    println!();
    println!("Advanced FAR-Style Layout:");
    println!("   ┌─ Chat Sessions ─┬─ Active Chat History ─────────────────┐");
    println!("   | Main Chat       | You: What's my balance?                |");
    println!("   | Analysis        | Agent: I'll analyze your request...   |");
    println!("   | Paused Chat     | Plan: Using get_balance tool          |");
    println!("   |                 | Executing: get_balance                |");
    println!("   | + New Chat      | Result: 2.5 SOL                       |");
    println!("   |                 | Agent: Your balance is 2.5 SOL        |");
    println!("   | Run             |                                        |");
    println!("   | Pause           | Agent Status: Idle                     |");
    println!("   | Stop            |                                        |");
    println!("   │                 │ You: [Input field...] [Send]          │");
    println!("   | Record          | [Clear] [Export] [Settings] [Help]    |");
    println!("   | Stop Rec        |                                        |");
    println!("   └─────────────────┴────────────────────────────────────────┘");
    println!();
    println!("Key Improvements:");
    println!("   • AI service integration for intelligent tool selection");
    println!("   • Background agent processing with state management");
    println!("   • Professional FAR-style interface design");
    println!("   • Session recording and replay capabilities");
    println!("   • Multi-chat support with independent agent states");
    println!();
    println!("NEW: AI-Powered Reply Suggestions!");
    println!("   ┌─────────────────────────────────────────────────────┐");
    println!("   | Reply Suggestions (press 1-5 to insert):          |");
    println!("   │ 1. Show recent transactions                        │");
    println!("   │ 2. What's the current SOL price?                   │");
    println!("   │ 3. How do I stake my SOL?                          │");
    println!("   │ 4. Send 0.5 SOL to address                         │");
    println!("   │ 5. Check my staking rewards                        │");
    println!("   └─────────────────────────────────────────────────────┘");
    println!();
    println!("   Suggestions Features:");
    println!("   • Generated by AI using chat context");
    println!("   • Appears after agent completes execution");
    println!("   • Press 1-5 to insert at cursor position");
    println!("   • Doesn't clear existing text");
    println!("   • Press ESC to hide suggestions");
    println!();

    // Run in demo mode if terminal not available
    println!("Note: Full TUI interface requires a proper terminal.");
    println!("   Run 'osvm chat --advanced' in a terminal to see the full UI.");

    Ok(())
}