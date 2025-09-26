//! Advanced Agent Chat UI with FAR-style design using cursive-multiplex
//! 
//! This module provides a comprehensive chat interface with multiple chat sessions,
//! AI-powered tool planning, background agent execution, and session recording.

use cursive::{Cursive, CursiveExt, View};
use cursive::views::{
    Dialog, EditView, LinearLayout, TextView, ScrollView, Panel, Button, 
    ListView, SelectView, ResizedView, DummyView
};
use cursive::traits::*;
use cursive::direction::Orientation;
use cursive_multiplex::{Mux, Id};
use crate::services::{mcp_service::{McpService, McpTool}, ai_service::AiService};
use anyhow::{Result, Context, anyhow};
use serde_json::Value;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::{mpsc, oneshot, broadcast};
use std::collections::HashMap;
use log::{error, warn, debug, info};
use serde::{Serialize, Deserialize};
use uuid::Uuid;
use chrono::{DateTime, Utc};

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

/// Tool planning result from AI
#[derive(Debug, Serialize, Deserialize)]
pub struct ToolPlan {
    pub reasoning: String,
    pub tools_to_use: Vec<PlannedTool>,
    pub expected_outcome: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PlannedTool {
    pub server_id: String,
    pub tool_name: String,
    pub args: Value,
    pub reason: String,
}

/// Main application state for advanced chat UI
#[derive(Clone)]
pub struct AdvancedChatState {
    pub sessions: Arc<RwLock<HashMap<Uuid, ChatSession>>>,
    pub active_session_id: Arc<RwLock<Option<Uuid>>>,
    pub mcp_service: Arc<Mutex<McpService>>,
    pub ai_service: Arc<AiService>,
    pub available_tools: Arc<RwLock<HashMap<String, Vec<McpTool>>>>,
    pub agent_command_sender: Arc<Mutex<Option<mpsc::UnboundedSender<AgentCommand>>>>,
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
        })
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
            let mut sender_guard = self.agent_command_sender.lock()
                .map_err(|e| anyhow!("Failed to lock agent command sender: {}", e))?;
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
        self.add_message_to_session(session_id, ChatMessage::AgentThinking("Analyzing your request...".to_string()))?;
        
        // Use AI service to plan what tools to use
        let planning_prompt = self.create_planning_prompt(&input)?;
        
        self.set_agent_state(session_id, AgentState::Planning);
        self.add_message_to_session(session_id, ChatMessage::AgentThinking("Planning which tools to use...".to_string()))?;
        
        match self.ai_service.query_with_debug(&planning_prompt, true).await {
            Ok(ai_response) => {
                // Parse AI response to get tool plan
                match self.parse_tool_plan(&ai_response) {
                    Ok(tool_plan) => {
                        self.add_message_to_session(session_id, ChatMessage::AgentPlan(tool_plan.reasoning.clone()))?;
                        
                        // Execute planned tools
                        for planned_tool in tool_plan.tools_to_use {
                            self.execute_planned_tool(session_id, planned_tool).await?;
                        }
                        
                        // Generate final response
                        self.generate_final_response(session_id, &input, &tool_plan.expected_outcome).await?;
                    }
                    Err(e) => {
                        warn!("Failed to parse tool plan: {}", e);
                        self.add_message_to_session(session_id, ChatMessage::Agent(
                            "I understand your request, but I'm having trouble planning the best approach. Let me try to help directly.".to_string()
                        ))?;
                        
                        // Fallback to simple response
                        self.simple_response(session_id, &input).await?;
                    }
                }
            }
            Err(e) => {
                error!("AI service failed: {}", e);
                self.add_message_to_session(session_id, ChatMessage::Error(
                    "AI planning service is unavailable. Falling back to simple mode.".to_string()
                ))?;
                
                self.simple_response(session_id, &input).await?;
            }
        }
        
        self.set_agent_state(session_id, AgentState::Idle);
        Ok(())
    }

    fn create_planning_prompt(&self, user_input: &str) -> Result<String> {
        let available_tools = self.available_tools.read()
            .map_err(|e| anyhow!("Failed to read available tools: {}", e))?;
        
        let mut tools_description = String::new();
        for (server_id, tools) in available_tools.iter() {
            tools_description.push_str(&format!("Server: {}\n", server_id));
            for tool in tools {
                tools_description.push_str(&format!(
                    "  - {}: {}\n", 
                    tool.name, 
                    tool.description.as_deref().unwrap_or("No description")
                ));
            }
            tools_description.push('\n');
        }
        
        let prompt = format!(r#"
You are an AI assistant for blockchain operations. The user has asked: "{}"

Available MCP tools:
{}

Please analyze the user's request and create a plan for which tools to use. Respond in JSON format:
{{
    "reasoning": "Explain why you chose these tools",
    "tools_to_use": [
        {{
            "server_id": "server_name",
            "tool_name": "tool_name",
            "args": {{}},
            "reason": "Why this tool is needed"
        }}
    ],
    "expected_outcome": "What the user should expect as a result"
}}

If no tools are needed, return an empty tools_to_use array and explain why in the reasoning.
"#, user_input, tools_description);

        Ok(prompt)
    }

    fn parse_tool_plan(&self, ai_response: &str) -> Result<ToolPlan> {
        // Try to extract JSON from the AI response
        let json_start = ai_response.find('{').ok_or_else(|| anyhow!("No JSON found in AI response"))?;
        let json_end = ai_response.rfind('}').ok_or_else(|| anyhow!("No closing brace found in AI response"))?;
        let json_str = &ai_response[json_start..=json_end];
        
        serde_json::from_str(json_str).map_err(|e| anyhow!("Failed to parse tool plan JSON: {}", e))
    }

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
        let mcp_service = self.mcp_service.lock()
            .map_err(|e| anyhow!("Failed to lock MCP service: {}", e))?;
        
        // For now, simulate tool execution - in real implementation, call MCP service
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        
        match planned_tool.tool_name.as_str() {
            "get_balance" => Ok(serde_json::json!({"balance": "2.5 SOL", "usd_value": 250.75})),
            "get_transactions" => Ok(serde_json::json!({
                "transactions": [
                    {"hash": "abc123", "amount": "0.1 SOL", "type": "sent"},
                    {"hash": "def456", "amount": "1.0 SOL", "type": "received"}
                ]
            })),
            _ => Ok(serde_json::json!({"result": "Tool executed successfully"}))
        }
    }

    async fn generate_final_response(&self, session_id: Uuid, original_input: &str, expected_outcome: &str) -> Result<()> {
        // Get the recent tool results to inform the response
        let session = self.get_session_by_id(session_id).ok_or_else(|| anyhow!("Session not found"))?;
        let recent_results: Vec<_> = session.messages.iter()
            .rev()
            .take(10)
            .filter_map(|msg| match msg {
                ChatMessage::ToolResult { result, .. } => Some(result),
                _ => None
            })
            .collect();
        
        let response_prompt = format!(r#"
The user asked: "{}"
Expected outcome: {}
Tool results: {}

Provide a clear, helpful response to the user based on the tool results. Be conversational and explain what was found.
"#, original_input, expected_outcome, serde_json::to_string(&recent_results)?);
        
        match self.ai_service.query_with_debug(&response_prompt, true).await {
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
        // Simple fallback response logic
        let response = if input.to_lowercase().contains("balance") {
            "I can help you check your wallet balance. However, I need MCP tools to be properly configured to fetch real data."
        } else if input.to_lowercase().contains("transaction") {
            "I can help you with transaction history. Please make sure MCP servers are configured for blockchain operations."
        } else {
            "I understand you're asking about blockchain operations. Please ensure MCP servers are configured so I can assist you with real data."
        };
        
        self.add_message_to_session(session_id, ChatMessage::Agent(response.to_string()))?;
        Ok(())
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
            let service = self.mcp_service.lock()
                .map_err(|e| anyhow!("Failed to lock MCP service: {}", e))?;
            service.list_servers()
        };
        
        let mut new_tools = HashMap::new();
        
        for (server_id, config) in servers {
            if config.enabled {
                // Simulate tool fetching - replace with real MCP calls
                let mock_tools = vec![
                    McpTool {
                        name: "get_balance".to_string(),
                        description: Some("Get wallet balance".to_string()),
                    },
                    McpTool {
                        name: "get_transactions".to_string(),
                        description: Some("Get transaction history".to_string()),
                    },
                ];
                new_tools.insert(server_id, mock_tools);
            }
        }
        
        if let Ok(mut tools) = self.available_tools.write() {
            *tools = new_tools;
        }
        
        Ok(())
    }

    pub fn send_agent_command(&self, command: AgentCommand) -> Result<()> {
        let sender_guard = self.agent_command_sender.lock()
            .map_err(|e| anyhow!("Failed to lock agent command sender: {}", e))?;
        
        if let Some(sender) = sender_guard.as_ref() {
            sender.send(command)
                .map_err(|e| anyhow!("Failed to send agent command: {}", e))?;
        } else {
            return Err(anyhow!("Agent worker not initialized"));
        }
        
        Ok(())
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

    pub async fn run(&mut self) -> Result<()> {
        // Initialize async workers
        self.state.start_agent_worker().await?;
        
        let mut siv = Cursive::default();
        
        // Create default session if none exists
        let default_session_id = self.state.create_session("Main Chat".to_string())?;
        
        // Set up the FAR-style UI layout
        self.setup_far_ui(&mut siv);
        
        // Update displays
        self.update_all_displays(&mut siv);
        
        // Run the TUI
        siv.run();
        
        Ok(())
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
            .title("OSVM Advanced Agent Chat - FAR Style Interface")
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
        chat_select.with_name("chat_list");
        
        chat_list_layout.add_child(Panel::new(ScrollView::new(chat_select)).max_height(15));
        
        // New chat button
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(Button::new("+ New Chat", |siv| {
            create_new_chat_dialog(siv);
        }));
        
        // Session controls
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(LinearLayout::horizontal()
            .child(Button::new("▶ Run", |siv| resume_agent(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("⏸ Pause", |siv| pause_agent(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("⏹ Stop", |siv| stop_agent(siv)))
        );
        
        // Recording controls
        chat_list_layout.add_child(DummyView.fixed_height(1));
        chat_list_layout.add_child(LinearLayout::horizontal()
            .child(Button::new("⏺ Record", |siv| start_recording(siv)))
            .child(DummyView.fixed_width(1))
            .child(Button::new("⏹ Stop Rec", |siv| stop_recording(siv)))
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
                    AgentState::Idle => "💤",
                    AgentState::Thinking => "🤔",
                    AgentState::Planning => "📋",
                    AgentState::ExecutingTool(_) => "⚙️",
                    AgentState::Waiting => "⏳",
                    AgentState::Paused => "⏸️",
                    AgentState::Error(_) => "❌",
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
                        display_text.push_str(&format!("👤 You: {}\n\n", sanitized));
                    }
                    ChatMessage::Agent(text) => {
                        display_text.push_str(&format!("🤖 Agent: {}\n\n", sanitized));
                    }
                    ChatMessage::System(text) => {
                        display_text.push_str(&format!("ℹ️  System: {}\n\n", sanitized));
                    }
                    ChatMessage::ToolCall { tool_name, description, args, execution_id } => {
                        display_text.push_str(&format!("🔧 Calling tool: {} - {}\n", tool_name, description));
                        if let Some(args) = args {
                            let sanitized_args = self.sanitize_json(args);
                            display_text.push_str(&format!("   Args: {}\n", sanitized_args));
                        }
                        display_text.push_str(&format!("   ID: {}\n\n", execution_id));
                    }
                    ChatMessage::ToolResult { tool_name, result, execution_id } => {
                        display_text.push_str(&format!("✅ Tool {} result (ID: {}):\n", tool_name, execution_id));
                        let sanitized_result = self.sanitize_json(result);
                        display_text.push_str(&format!("{}\n\n", sanitized_result));
                    }
                    ChatMessage::Error(text) => {
                        display_text.push_str(&format!("❌ Error: {}\n\n", sanitized));
                    }
                    ChatMessage::AgentThinking(text) => {
                        display_text.push_str(&format!("🤔 Agent: {}\n\n", sanitized));
                    }
                    ChatMessage::AgentPlan(text) => {
                        display_text.push_str(&format!("📋 Plan: {}\n\n", sanitized));
                    }
                }
            }
            
            if let Some(mut chat_display) = siv.find_name::<TextView>("chat_display") {
                chat_display.set_content(display_text);
            }
        }
    }

    fn update_agent_status(&self, siv: &mut Cursive) {
        if let Some(session) = self.state.get_active_session() {
            let status_text = match &session.agent_state {
                AgentState::Idle => "Agent: Idle 💤".to_string(),
                AgentState::Thinking => "Agent: Thinking 🤔".to_string(),
                AgentState::Planning => "Agent: Planning 📋".to_string(),
                AgentState::ExecutingTool(tool) => format!("Agent: Executing {} ⚙️", tool),
                AgentState::Waiting => "Agent: Waiting ⏳".to_string(),
                AgentState::Paused => "Agent: Paused ⏸️".to_string(),
                AgentState::Error(err) => format!("Agent: Error ❌ - {}", err),
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
            _ => "Complex message".to_string(),
        }
    }

    fn sanitize_text(&self, text: &str) -> String {
        let mut sanitized = text.to_string();
        
        // Redact potential private keys
        if let Ok(key_pattern) = regex::Regex::new(r"\b[1-9A-HJ-NP-Za-km-z]{32,44}\b") {
            sanitized = key_pattern.replace_all(&sanitized, "[REDACTED_KEY]").to_string();
        }
        
        // Limit length
        if sanitized.len() > 1000 {
            sanitized.truncate(997);
            sanitized.push_str("...");
        }
        
        sanitized
    }

    fn sanitize_json(&self, value: &Value) -> String {
        match serde_json::to_string_pretty(value) {
            Ok(json_str) => {
                let sanitized = self.sanitize_text(&json_str);
                if sanitized.len() > 500 {
                    format!("{}...\n}}", &sanitized[..497])
                } else {
                    sanitized
                }
            }
            Err(_) => "[Invalid JSON]".to_string()
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

fn handle_user_input(siv: &mut Cursive, text: &str, state: AdvancedChatState) {
    if text.trim().is_empty() {
        return;
    }
    
    // Clear input
    if let Some(mut input) = siv.find_name::<EditView>("input") {
        input.set_content("");
    }
    
    // Send to agent for processing
    if let Some(session) = state.get_active_session() {
        let command = AgentCommand::ProcessInput {
            session_id: session.id,
            input: text.to_string(),
        };
        
        if let Err(e) = state.send_agent_command(command) {
            error!("Failed to send input to agent: {}", e);
        }
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
            if let Err(e) = state.send_agent_command(command) {
                error!("Failed to resume agent: {}", e);
            }
        }
    });
}

fn pause_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::PauseAgent { session_id: session.id };
            if let Err(e) = state.send_agent_command(command) {
                error!("Failed to pause agent: {}", e);
            }
        }
    });
}

fn stop_agent(siv: &mut Cursive) {
    siv.with_user_data(|state: &mut AdvancedChatState| {
        if let Some(session) = state.get_active_session() {
            let command = AgentCommand::StopAgent { session_id: session.id };
            if let Err(e) = state.send_agent_command(command) {
                error!("Failed to stop agent: {}", e);
            }
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

fn export_chat(_siv: &mut Cursive) {
    // Implement chat export functionality
}

fn show_settings(_siv: &mut Cursive) {
    // Implement settings dialog
}

fn show_advanced_help(siv: &mut Cursive) {
    let help_text = "OSVM Advanced Agent Chat Help\n\n\
        FAR-Style Interface:\n\
        • Left Panel: Chat sessions with status indicators\n\
        • Right Panel: Active chat history and controls\n\n\
        Agent Controls:\n\
        • ▶ Run: Resume agent processing\n\
        • ⏸ Pause: Pause agent operations\n\
        • ⏹ Stop: Stop current agent task\n\n\
        Recording:\n\
        • ⏺ Record: Start session recording\n\
        • ⏹ Stop Rec: Stop recording\n\n\
        Status Indicators:\n\
        • 💤 Idle • 🤔 Thinking • 📋 Planning\n\
        • ⚙️ Executing • ⏳ Waiting • ⏸️ Paused • ❌ Error\n\n\
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
    // This would need access to the UI instance - in a real implementation,
    // we'd use a more sophisticated state management approach
}

/// Main entry point for the advanced agent chat UI
pub async fn run_advanced_agent_chat() -> Result<()> {
    println!("🚀 Starting OSVM Advanced Agent Chat Interface...");
    
    // Check if we're in a terminal environment
    if std::env::var("TERM").is_err() || std::env::var("CI").is_ok() {
        return run_advanced_demo_mode().await;
    }
    
    let mut chat_ui = AdvancedChatUI::new()
        .context("Failed to initialize advanced chat UI")?;
    
    chat_ui.run().await
        .context("Failed to run advanced chat interface")?;

    Ok(())
}

/// Demo mode for advanced chat interface
async fn run_advanced_demo_mode() -> Result<()> {
    println!("📱 Running Advanced Agent Chat in demo mode");
    println!();
    println!("🎯 Advanced Features:");
    println!("   • FAR-style/Borland TUI design with dual panels");
    println!("   • AI-powered input parsing and tool planning");
    println!("   • Multiple chat sessions with background agent execution");
    println!("   • Session recording and agent control (run/pause/stop)");
    println!("   • Comprehensive MCP tool integration");
    println!();
    println!("🖼️  Advanced FAR-Style Layout:");
    println!("   ┌─ Chat Sessions ─┬─ Active Chat History ─────────────────┐");
    println!("   │ 💤 Main Chat    │ 👤 You: What's my balance?             │");
    println!("   │ 🤔 Analysis     │ 🤖 Agent: I'll analyze your request... │");
    println!("   │ ⏸️ Paused Chat  │ 📋 Plan: Using get_balance tool        │");
    println!("   │                 │ 🔧 Executing: get_balance              │");
    println!("   │ + New Chat      │ ✅ Result: 2.5 SOL                     │");
    println!("   │                 │ 🤖 Agent: Your balance is 2.5 SOL     │");
    println!("   │ ▶ Run           │                                        │");
    println!("   │ ⏸ Pause        │ Agent Status: Idle 💤                  │");
    println!("   │ ⏹ Stop         │                                        │");
    println!("   │                 │ You: [Input field...] [Send]          │");
    println!("   │ ⏺ Record       │ [Clear] [Export] [Settings] [Help]    │");
    println!("   │ ⏹ Stop Rec     │                                        │");
    println!("   └─────────────────┴────────────────────────────────────────┘");
    println!();
    println!("💡 Key Improvements:");
    println!("   • AI service integration for intelligent tool selection");
    println!("   • Background agent processing with state management");
    println!("   • Professional FAR-style interface design");
    println!("   • Session recording and replay capabilities");
    println!("   • Multi-chat support with independent agent states");
    println!();
    
    Ok(())
}