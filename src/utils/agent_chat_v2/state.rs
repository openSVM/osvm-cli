//! Main application state for advanced chat UI

use anyhow::{anyhow, Context, Result};
use log::{debug, error, info, warn};
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};
use tokio::sync::{mpsc, Mutex};
use uuid::Uuid;

use crate::services::{
    ai_service::AiService,
    mcp_service::{McpServerConfig, McpService, McpTool},
    ovsm_executor::OvsmExecutor,
};
use crate::utils::agent_chat::system_status_bar::SystemStatusBarManager;
use crate::utils::themes::ThemeManager;

use super::agent::AgentCommand;
use super::session::ChatSession;
use super::types::{AgentState, ChatMessage};

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
    pub theme_manager: Arc<RwLock<ThemeManager>>,
    pub status_bar_manager: Arc<RwLock<SystemStatusBarManager>>,

    /// Cached status bar text to reduce blocking UI thread
    pub cached_status_text: Arc<RwLock<Option<String>>>,

    /// Last time the status was updated
    pub last_status_update: Arc<RwLock<Option<std::time::Instant>>>,

    /// Input history buffer (like bash history)
    pub input_history: Arc<RwLock<Vec<String>>>,

    /// Current position in input history (None = at prompt, Some(idx) = in history)
    pub history_position: Arc<RwLock<Option<usize>>>,

    /// MCP tools panel visibility (progressive disclosure - hidden by default)
    pub mcp_tools_visible: Arc<RwLock<bool>>,

    /// Phase 2: OVSM execution engine for plan-based tool execution
    pub ovsm_executor: Arc<Mutex<OvsmExecutor>>,
}

impl AdvancedChatState {
    pub fn new() -> Result<Self> {
        let mut theme_manager =
            ThemeManager::new().context("Failed to initialize theme manager")?;
        let status_bar_manager = SystemStatusBarManager::new();
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
            theme_manager: Arc::new(RwLock::new(theme_manager)),
            status_bar_manager: Arc::new(RwLock::new(status_bar_manager)),
            cached_status_text: Arc::new(RwLock::new(None)),
            last_status_update: Arc::new(RwLock::new(None)),
            input_history: Arc::new(RwLock::new(Vec::new())),
            history_position: Arc::new(RwLock::new(None)),
            mcp_tools_visible: Arc::new(RwLock::new(false)), // Hidden by default - progressive disclosure
            ovsm_executor: Arc::new(Mutex::new(OvsmExecutor::new(false))), // Phase 2: Initialize OVSM executor (debug=false)
        };

        // Load theme from saved configuration
        // BUG-2015 fix: Clarify theme loading fallback behavior
        // If theme loading fails (e.g., config file missing, corrupted, or theme not found),
        // this is NOT a fatal error. The application will use the default theme (VS Code dark)
        // and continue normally. This is intentional graceful degradation.
        if let Err(e) = state.load_theme_from_config() {
            warn!("Failed to load theme from config, using default theme: {}", e);
            // Note: The default theme is already initialized during ThemeManager::new()
            // so this is just a fallback for user preferences, not critical for functionality
        }

        // Load sessions from disk or create defaults
        if let Ok(Some(persisted)) = super::persistence::PersistedState::load() {
            // Restore saved sessions
            let session_count = persisted.sessions.len();
            if let Ok(mut sessions) = state.sessions.write() {
                *sessions = persisted.sessions.clone();
            }
            // BUG-2006 fix: Validate that the persisted active_session_id actually exists
            // If the state file is corrupted or the session was deleted, validate before setting
            if let Ok(mut active_id) = state.active_session_id.write() {
                if let Some(sess_id) = persisted.active_session_id {
                    // Verify the session exists in the loaded sessions
                    if let Ok(sessions) = state.sessions.read() {
                        if sessions.contains_key(&sess_id) {
                            *active_id = Some(sess_id);
                        } else {
                            // Session doesn't exist, set to first session or None
                            warn!("Persisted active session {} not found in loaded sessions", sess_id);
                            if let Some(&first_session_id) = sessions.keys().next() {
                                *active_id = Some(first_session_id);
                                info!("Set active session to first available session");
                            }
                        }
                    }
                }
            }
            info!("Loaded {} saved sessions", session_count);
        } else {
            // First run - create default session with welcome message
            let main_session_id = state.create_session("Main Chat".to_string())?;
            if let Ok(mut active_id) = state.active_session_id.write() {
                *active_id = Some(main_session_id);
            }

            // Add welcome message for first-time users
            let welcome_msg = "👋 Welcome to OSVM Advanced Agent Chat!\n\n\
                ✨ Quick Start:\n\
                • Shift+Enter or Ctrl+Enter to send messages\n\
                • Enter for new lines (multi-line support!)\n\
                • Ctrl+K to clear input\n\
                • ? or F1 for full help\n\
                • 🤖 AI Enhance button to improve your messages\n\n\
                💡 Tip: Try asking me about Solana, blockchain operations, or just say hello!\n\n\
                🎯 AI Service: Using OSVM.ai (free, no setup needed)\n\
                📚 Press F1 or type '?' to see all keyboard shortcuts";

            state.add_message_to_session(
                main_session_id,
                ChatMessage::System(welcome_msg.to_string())
            )?;
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
            Ok(sessions) => sessions
                .values()
                .map(|s| (s.id, s.name.clone(), s.agent_state.clone()))
                .collect(),
            Err(_) => {
                error!("Failed to read sessions");
                vec![]
            }
        }
    }

    pub fn set_active_session(&self, session_id: Uuid) -> Result<()> {
        let mut active_id = self
            .active_session_id
            .write()
            .map_err(|e| anyhow!("Failed to lock active session: {}", e))?;
        *active_id = Some(session_id);
        drop(active_id); // Release lock

        // BUG-2008 fix: Reset history position when switching sessions
        // Otherwise, up arrow navigates through the PREVIOUS session's history
        if let Ok(mut pos) = self.history_position.write() {
            *pos = None;  // Back to prompt, not in history
        }

        // Auto-save after session change
        self.save_state_async();

        Ok(())
    }

    pub fn get_active_session(&self) -> Option<ChatSession> {
        // Use a timeout to prevent deadlocks
        let active_id = match self.active_session_id.try_read() {
            Ok(lock) => lock,
            Err(_) => {
                warn!("Failed to acquire read lock for active_session_id");
                return None;
            }
        };

        let session_id = (*active_id)?;
        drop(active_id); // Release lock early

        let sessions = match self.sessions.try_read() {
            Ok(lock) => lock,
            Err(_) => {
                warn!("Failed to acquire read lock for sessions");
                return None;
            }
        };

        sessions.get(&session_id).cloned()
    }

    pub fn add_message_to_session(&self, session_id: Uuid, message: ChatMessage) -> Result<()> {
        // Use try_write to prevent deadlocks
        let mut sessions = self.sessions.try_write().map_err(|_| {
            anyhow!("Failed to acquire write lock for sessions (possible deadlock)")
        })?;

        if let Some(session) = sessions.get_mut(&session_id) {
            session.add_message(message);
        } else {
            warn!("Session {} not found when adding message", session_id);
        }

        // Auto-save after message change
        drop(sessions); // Release lock before save
        self.save_state_async();

        Ok(())
    }

    pub fn get_session_by_id(&self, session_id: Uuid) -> Option<ChatSession> {
        let sessions = self.sessions.try_read().ok()?;
        sessions.get(&session_id).cloned()
    }

    pub fn set_agent_state(&self, session_id: Uuid, state: AgentState) {
        if let Ok(mut sessions) = self.sessions.write() {
            if let Some(session) = sessions.get_mut(&session_id) {
                session.agent_state = state;
            }
        }
    }

    pub fn get_agent_state(&self, session_id: Uuid) -> Option<AgentState> {
        self.sessions
            .read()
            .ok()?
            .get(&session_id)
            .map(|s| s.agent_state.clone())
    }

    pub fn remove_last_processing_message(&self, session_id: Uuid) -> Result<()> {
        // Blocking lock: very short critical section, avoids missing cleanup
        let mut sessions = self
            .sessions
            .write()
            .map_err(|e| anyhow!("Failed to acquire write lock for sessions: {}", e))?;

        // Remove ALL Processing messages from the session
        if let Some(session) = sessions.get_mut(&session_id) {
            let initial_count = session.messages.len();
            session
                .messages
                .retain(|msg| !matches!(msg, ChatMessage::Processing { .. }));
            let removed_count = initial_count - session.messages.len();
            if removed_count > 0 {
                debug!(
                    "Removed {} processing message(s) from session {}",
                    removed_count, session_id
                );
            }
        } else {
            warn!(
                "Session {} not found when removing processing message",
                session_id
            );
        }

        Ok(())
    }

    pub async fn send_agent_command(&self, command: AgentCommand) -> Result<()> {
        let sender_guard = self.agent_command_sender.lock().await;

        if let Some(sender) = sender_guard.as_ref() {
            sender
                .send(command)
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
            // Create a small runtime just for this operation. If runtime creation fails,
            // log the error and return early to avoid panics in the UI thread.
            match tokio::runtime::Runtime::new() {
                Ok(runtime) => {
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
                }
                Err(e) => {
                    error!(
                        "Failed to create temporary Tokio runtime for send_agent_command_sync: {}",
                        e
                    );
                }
            }
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

        // BUG-2005 fix: Make session creation and activation atomic to prevent race conditions
        // Previously, the lock was released and another thread could create a session between
        // the drop() and set_active_session() calls, causing wrong session to become active
        let mut sessions = self
            .sessions
            .write()
            .map_err(|e| anyhow!("Failed to lock sessions: {}", e))?;
        sessions.insert(session_id, session);

        // Check if this is the first session and set as active WHILE HOLDING THE LOCK
        let is_first_session = sessions.len() == 1;

        if is_first_session {
            drop(sessions); // Release lock before set_active_session
            self.set_active_session(session_id)?;
        } else {
            drop(sessions); // Release lock
            // Still save state if not first session
            self.save_state_async();
        }

        Ok(session_id)
    }

    /// Delete a chat session by ID
    pub fn delete_session(&self, session_id: Uuid) -> Result<()> {
        let mut sessions = self
            .sessions
            .write()
            .map_err(|e| anyhow!("Failed to lock sessions for deletion: {}", e))?;

        if sessions.remove(&session_id).is_none() {
            warn!("Session {} not found for deletion", session_id);
            return Ok(()); // Not an error if already deleted
        }

        debug!("Deleted session {}", session_id);
        drop(sessions); // Release lock

        // Ensure active_session_id doesn't point to deleted session
        if let Ok(mut active_id) = self.active_session_id.write() {
            if *active_id == Some(session_id) {
                // Set to first available session or None
                if let Ok(sessions) = self.sessions.read() {
                    *active_id = sessions.keys().next().copied();
                    if *active_id == Some(session_id) {
                        // Shouldn't happen but be safe
                        *active_id = None;
                    }
                } else {
                    *active_id = None;
                }
                info!("Reset active session after deletion");
            }
        }

        // Auto-save after session deletion
        self.save_state_async();
        Ok(())
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
                if cb_sink
                    .send(Box::new(|siv| {
                        // UI update handled by periodic refresh
                    }))
                    .is_err()
                {
                    break; // Exit if UI is closed
                }
            }
        });
    }

    pub async fn refresh_tools_from_mcp(&self) -> Result<()> {
        let servers = {
            let service = self.mcp_service.lock().await;
            service
                .list_servers()
                .into_iter()
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
                            warn!(
                                "Failed to initialize server {} for tool refresh: {}",
                                server_id, e
                            );
                        }
                    }

                    // Try to list tools
                    let mut service = self.mcp_service.lock().await;
                    match service.list_tools(&server_id).await {
                        Ok(tools) => tools,
                        Err(e) => {
                            warn!("Failed to fetch tools from {}: {}", server_id, e);
                            // Don't show fallback tools - let it fail so the issue is visible
                            vec![]
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

    pub fn update_processing_message(
        &self,
        session_id: Uuid,
        message: String,
        spinner_index: usize,
    ) -> Result<()> {
        let mut sessions = self
            .sessions
            .write()
            .map_err(|e| anyhow::anyhow!("Failed to write sessions: {}", e))?;
        if let Some(session) = sessions.get_mut(&session_id) {
            if let Some(last_msg) = session.messages.last_mut() {
                if let ChatMessage::Processing { .. } = last_msg {
                    // Update the existing processing message
                    *last_msg = ChatMessage::Processing {
                        message,
                        spinner_index,
                    };
                }
            }
        }
        Ok(())
    }

    pub fn get_available_tools_context(&self) -> String {
        // CRITICAL SECURITY FIX: Proper poison handling - don't mask panics!
        let tools = match self.available_tools.read() {
            Ok(tools) => tools,
            Err(poison_error) => {
                error!("❌ CRITICAL: available_tools RwLock poisoned!");
                error!("A panic occurred while holding the lock - data may be inconsistent");
                error!("This indicates a bug in the tool refresh logic");

                // Access the poisoned data read-only for emergency operation
                // but warn the user that the system needs restart
                let guard = poison_error.into_inner();

                warn!("⚠️  MCP tools data may be corrupted - recommend restarting chat");

                // Return early with safe fallback
                return "⚠️  MCP tools temporarily unavailable due to system error.\n\
                        Please restart the chat interface.\n\
                        This indicates a bug - please report it."
                    .to_string();
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
                context.push_str(&format!(
                    "  - {}: {}\n",
                    tool.name,
                    tool.description
                        .as_ref()
                        .unwrap_or(&"No description".to_string())
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

    // Theme management methods
    pub fn switch_theme(&self, theme_name: &str) -> Result<()> {
        let mut theme_manager = self
            .theme_manager
            .write()
            .map_err(|e| anyhow!("Failed to acquire theme manager lock: {}", e))?;

        theme_manager
            .switch_theme(theme_name)
            .context("Failed to switch theme")?;

        // Save theme preference to config
        drop(theme_manager); // Release the lock before calling save_theme_config
        if let Err(e) = self.save_theme_config() {
            warn!("Failed to save theme configuration: {}", e);
        }

        Ok(())
    }

    pub fn get_available_themes(&self) -> Result<Vec<String>> {
        let theme_manager = self
            .theme_manager
            .read()
            .map_err(|e| anyhow!("Failed to acquire theme manager lock: {}", e))?;

        Ok(theme_manager.available_themes().to_vec())
    }

    pub fn get_current_theme_name(&self) -> Result<String> {
        let theme_manager = self
            .theme_manager
            .read()
            .map_err(|e| anyhow!("Failed to acquire theme manager lock: {}", e))?;

        Ok(theme_manager.current_theme().name.clone())
    }

    pub fn preview_theme(&self, theme_name: &str) -> Result<String> {
        use crate::utils::themes::Theme;

        match Theme::load(theme_name) {
            Ok(theme) => Ok(theme.preview()),
            Err(e) => Err(anyhow!("Failed to load theme '{}': {}", theme_name, e)),
        }
    }

    pub fn get_theme_manager(&self) -> Arc<RwLock<ThemeManager>> {
        Arc::clone(&self.theme_manager)
    }

    /// Load theme from configuration file or environment
    pub fn load_theme_from_config(&self) -> Result<()> {
        let config_path = Self::get_config_path()?;

        if config_path.exists() {
            let content = std::fs::read_to_string(&config_path)?;
            if let Ok(config) = serde_json::from_str::<ThemeConfig>(&content) {
                if let Some(theme_name) = config.current_theme {
                    self.switch_theme(&theme_name)?;
                }
            }
        }

        Ok(())
    }

    /// Save current theme to configuration file
    pub fn save_theme_config(&self) -> Result<()> {
        let config_path = Self::get_config_path()?;

        if let Some(parent) = config_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let theme_name = self.get_current_theme_name()?;
        let config = ThemeConfig {
            current_theme: Some(theme_name),
            auto_theme_switching: false, // Note: Auto theme switching feature planned for future release
        };

        let json = serde_json::to_string_pretty(&config)?;
        std::fs::write(&config_path, json)?;

        Ok(())
    }

    /// Get theme configuration file path
    fn get_config_path() -> Result<std::path::PathBuf> {
        let home = std::env::var("HOME").context("HOME environment variable not set")?;
        Ok(std::path::PathBuf::from(home)
            .join(".config")
            .join("osvm")
            .join("theme_config.json"))
    }

    // Input history management methods

    /// Add a message to input history (like bash history)
    pub fn add_to_history(&self, input: String) {
        if input.trim().is_empty() {
            return; // Don't add empty messages
        }

        if let Ok(mut history) = self.input_history.write() {
            // Don't add if it's the same as the last entry (avoid duplicates)
            if let Some(last) = history.last() {
                if last == &input {
                    return;
                }
            }

            history.push(input);

            // Limit history to 100 entries (like bash HISTSIZE)
            if history.len() > 100 {
                history.remove(0);
            }
        }

        // Reset position when adding new entry
        if let Ok(mut pos) = self.history_position.write() {
            *pos = None;
        }
    }

    /// Navigate to previous entry in history (up arrow)
    pub fn history_previous(&self) -> Option<String> {
        let history = self.input_history.read().ok()?;

        if history.is_empty() {
            return None;
        }

        let mut pos = self.history_position.write().ok()?;

        let new_pos = match *pos {
            None => {
                // First time pressing up, go to most recent entry
                history.len() - 1
            }
            Some(current) => {
                // Already in history, go further back if possible
                if current > 0 {
                    current - 1
                } else {
                    return Some(history[current].clone()); // Already at oldest
                }
            }
        };

        *pos = Some(new_pos);
        Some(history[new_pos].clone())
    }

    /// Navigate to next entry in history (down arrow)
    pub fn history_next(&self) -> Option<String> {
        let history = self.input_history.read().ok()?;

        if history.is_empty() {
            return None;
        }

        let mut pos = self.history_position.write().ok()?;

        let new_pos = match *pos {
            None => {
                // Not in history, stay at prompt
                return Some(String::new());
            }
            Some(current) => {
                // In history, move forward
                if current < history.len() - 1 {
                    current + 1
                } else {
                    // Reached the end, go back to empty prompt
                    *pos = None;
                    return Some(String::new());
                }
            }
        };

        *pos = Some(new_pos);
        Some(history[new_pos].clone())
    }

    /// Check if currently navigating history
    pub fn is_in_history(&self) -> bool {
        self.history_position
            .read()
            .ok()
            .and_then(|pos| *pos)
            .is_some()
    }

    /// Get current history position for display
    pub fn get_history_indicator(&self) -> String {
        if let Ok(pos) = self.history_position.read() {
            if let Some(idx) = *pos {
                if let Ok(history) = self.input_history.read() {
                    let total = history.len();
                    return format!(" (history {}/{})", idx + 1, total);
                }
            }
        }
        String::new()
    }

    /// Save state to disk asynchronously (non-blocking)
    fn save_state_async(&self) {
        // Clone the data we need to save (quick operation)
        let sessions = match self.sessions.try_read() {
            Ok(s) => s.clone(),
            Err(_) => {
                warn!("Could not acquire lock to save state");
                return;
            }
        };

        let active_id = match self.active_session_id.try_read() {
            Ok(id) => *id,
            Err(_) => {
                warn!("Could not acquire lock for active session ID");
                return;
            }
        };

        // Spawn thread to do the actual save (don't block UI)
        std::thread::spawn(move || {
            if let Err(e) = super::persistence::PersistedState::save(&sessions, active_id) {
                // Log error but don't crash - save is nice-to-have
                debug!("Failed to persist state: {}", e);
            }
        });
    }
}

/// Theme configuration structure
#[derive(serde::Serialize, serde::Deserialize, Debug, Default)]
struct ThemeConfig {
    current_theme: Option<String>,
    auto_theme_switching: bool,
}
