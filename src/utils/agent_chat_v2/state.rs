//! Main application state for advanced chat UI

use anyhow::{Result, Context, anyhow};
use log::{error, warn, info};
use std::sync::{Arc, RwLock};
use tokio::sync::{mpsc, Mutex};
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use uuid::Uuid;

use crate::services::{
    mcp_service::{McpService, McpTool, McpServerConfig},
    ai_service::AiService
};

use super::types::{ChatMessage, AgentState};
use super::session::ChatSession;
use super::agent::AgentCommand;

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
        let state = Self {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            active_session_id: Arc::new(RwLock::new(None)),
            mcp_service: Arc::new(Mutex::new(McpService::new())),
            ai_service: Arc::new(AiService::new()),
            available_tools: Arc::new(RwLock::new(HashMap::new())),
            agent_command_sender: Arc::new(Mutex::new(None)),
            spinner_state: Arc::new(AtomicUsize::new(0)),
            current_suggestions: Arc::new(RwLock::new(Vec::new())),
            suggestions_visible: Arc::new(RwLock::new(false)),
        };

        // Create default sessions so the UI has content to navigate
        let main_session_id = state.create_session("Main Chat".to_string())?;
        let analysis_session_id = state.create_session("Analysis".to_string())?;
        let work_session_id = state.create_session("Work Chat".to_string())?;

        if let Ok(mut active_id) = state.active_session_id.write() {
            *active_id = Some(main_session_id);
        }

        // Don't block on tool refresh during creation - it will be done asynchronously
        Ok(state)
    }

    pub async fn initialize(&self) -> Result<()> {
        // Load MCP configuration first
        {
            let mut service = self.mcp_service.lock().await;
            if let Err(e) = service.load_config() {
                warn!("Failed to load MCP config: {}", e);
            }
        }

        // Perform the initial refresh of tools after creation
        self.refresh_tools_from_mcp().await
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

    pub fn add_message_to_session(&self, session_id: Uuid, message: ChatMessage) -> Result<()> {
        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow!("Failed to lock sessions: {}", e))?;

        if let Some(session) = sessions.get_mut(&session_id) {
            session.add_message(message);
        }

        Ok(())
    }

    pub fn get_session_by_id(&self, session_id: Uuid) -> Option<ChatSession> {
        self.sessions.read().ok()?.get(&session_id).cloned()
    }

    pub fn set_agent_state(&self, session_id: Uuid, state: AgentState) {
        if let Ok(mut sessions) = self.sessions.write() {
            if let Some(session) = sessions.get_mut(&session_id) {
                session.agent_state = state;
            }
        }
    }

    pub fn get_agent_state(&self, session_id: Uuid) -> Option<AgentState> {
        self.sessions.read().ok()?
            .get(&session_id)
            .map(|s| s.agent_state.clone())
    }

    pub fn remove_last_processing_message(&self, session_id: Uuid) -> Result<()> {
        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow::anyhow!("Failed to write sessions: {}", e))?;

        if let Some(session) = sessions.get_mut(&session_id) {
            // Remove the last message if it's a Processing type
            if let Some(ChatMessage::Processing { .. }) = session.messages.last() {
                session.messages.pop();
            }
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

    /// Create a new chat session
    pub fn create_session(&self, name: String) -> Result<Uuid> {
        let session_id = Uuid::new_v4();
        let session = ChatSession {
            id: session_id,
            name,
            created_at: chrono::Utc::now(),
            messages: Vec::new(),
            agent_state: AgentState::Idle,
            recording: false,
            recording_file: None,
        };

        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow!("Failed to lock sessions: {}", e))?;
        sessions.insert(session_id, session);

        // Set as active session if it's the first one
        let is_first_session = sessions.len() == 1;
        drop(sessions); // Release the write lock before calling set_active_session

        if is_first_session {
            self.set_active_session(session_id)?;
        }

        Ok(session_id)
    }

    /// Start spinner animation for processing states
    pub fn start_spinner_animation(&self, cb_sink: cursive::CbSink) {
        let spinner_state = self.spinner_state.clone();

        std::thread::spawn(move || {
            loop {
                std::thread::sleep(std::time::Duration::from_millis(100));
                let current = spinner_state.load(std::sync::atomic::Ordering::Relaxed);
                spinner_state.store((current + 1) % 10, std::sync::atomic::Ordering::Relaxed);

                // Send callback to update UI
                if cb_sink.send(Box::new(|siv| {
                    // UI update handled by periodic refresh
                })).is_err() {
                    break; // Exit if UI is closed
                }
            }
        });
    }

    pub async fn refresh_tools_from_mcp(&self) -> Result<()> {
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

    pub fn update_processing_message(&self, session_id: Uuid, message: String, spinner_index: usize) -> Result<()> {
        let mut sessions = self.sessions.write()
            .map_err(|e| anyhow::anyhow!("Failed to write sessions: {}", e))?;
        if let Some(session) = sessions.get_mut(&session_id) {
            if let Some(last_msg) = session.messages.last_mut() {
                if let ChatMessage::Processing { .. } = last_msg {
                    // Update the existing processing message
                    *last_msg = ChatMessage::Processing { message, spinner_index };
                }
            }
        }
        Ok(())
    }

    pub fn get_available_tools_context(&self) -> String {
        let tools = match self.available_tools.read() {
            Ok(tools) => tools,
            Err(_) => {
                // Clear poison and retry
                self.available_tools.clear_poison();
                self.available_tools.read().unwrap()
            }
        };

        if tools.is_empty() {
            return "No MCP tools currently available.".to_string();
        }

        let mut context = String::new();
        context.push_str("Available MCP Tools:\n");

        for (server_id, tool_list) in tools.iter() {
            context.push_str(&format!("\nServer: {}\n", server_id));
            for tool in tool_list.iter() {
                context.push_str(&format!("  - {}: {}\n",
                    tool.name,
                    tool.description.as_ref().unwrap_or(&"No description".to_string())
                ));

                // Add input schema info if available
                if let Some(schema) = tool.input_schema.as_object() {
                    if let Some(properties) = schema.get("properties").and_then(|p| p.as_object()) {
                        context.push_str("    Parameters: ");
                        let params: Vec<String> = properties.keys().cloned().collect();
                        context.push_str(&params.join(", "));
                        context.push_str("\n");
                    }
                }
            }
        }

        context
    }
}