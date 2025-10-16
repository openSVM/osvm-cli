//! Command providers - Dynamic command sources that keep the palette fresh

use super::{Command, CommandAction, CommandCategory, CommandProvider};
use std::sync::{Arc, RwLock};

/// File provider - quick access to files
pub struct FileProvider {
    project_root: String,
    recent_files: Vec<String>,
}

impl FileProvider {
    pub fn new() -> Self {
        Self {
            project_root: std::env::current_dir()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string(),
            recent_files: Vec::new(),
        }
    }
}

impl CommandProvider for FileProvider {
    fn get_commands(&self) -> Vec<Command> {
        let mut commands = Vec::new();

        // Recent files
        for file in &self.recent_files {
            commands.push(Command {
                id: format!("open_file_{}", file),
                label: format!("Open: {}", file),
                description: Some("Open recent file".to_string()),
                category: CommandCategory::Navigation,
                shortcut: None,
                icon: Some("📄".to_string()),
                action: CommandAction::GoToFile(file.clone()),
                keywords: vec!["file", "open", "recent"].into_iter()
                    .map(String::from).collect(),
            });
        }

        commands
    }

    fn refresh(&mut self) {
        // Would scan for new files
    }
}

/// Session provider - quick access to chat sessions
pub struct SessionProvider {
    sessions: Arc<RwLock<Vec<SessionInfo>>>,
}

#[derive(Clone)]
struct SessionInfo {
    id: String,
    name: String,
    message_count: usize,
    last_active: String,
}

impl SessionProvider {
    pub fn new() -> Self {
        Self {
            sessions: Arc::new(RwLock::new(vec![
                SessionInfo {
                    id: "main".to_string(),
                    name: "Main Chat".to_string(),
                    message_count: 42,
                    last_active: "2 min ago".to_string(),
                },
                SessionInfo {
                    id: "analysis".to_string(),
                    name: "Analysis".to_string(),
                    message_count: 18,
                    last_active: "1 hour ago".to_string(),
                },
            ])),
        }
    }
}

impl CommandProvider for SessionProvider {
    fn get_commands(&self) -> Vec<Command> {
        let mut commands = Vec::new();

        if let Ok(sessions) = self.sessions.read() {
            for session in sessions.iter() {
                commands.push(Command {
                    id: format!("session_{}", session.id),
                    label: format!("Session: {}", session.name),
                    description: Some(format!("{} messages, {}",
                        session.message_count, session.last_active)),
                    category: CommandCategory::Navigation,
                    shortcut: None,
                    icon: Some("💬".to_string()),
                    action: CommandAction::GoToSession(session.id.clone()),
                    keywords: vec!["session", "chat", &session.name].into_iter()
                        .map(String::from).collect(),
                });
            }
        }

        commands
    }

    fn refresh(&mut self) {
        // Would update session list
    }
}

/// MCP Tool provider - access to all MCP tools
pub struct MCPToolProvider {
    tools: Vec<ToolInfo>,
}

struct ToolInfo {
    name: String,
    description: String,
    server: String,
}

impl MCPToolProvider {
    pub fn new() -> Self {
        Self {
            tools: vec![
                ToolInfo {
                    name: "get_balance".to_string(),
                    description: "Get wallet balance".to_string(),
                    server: "osvm-mcp".to_string(),
                },
                ToolInfo {
                    name: "get_transaction".to_string(),
                    description: "Get transaction details".to_string(),
                    server: "osvm-mcp".to_string(),
                },
            ],
        }
    }
}

impl CommandProvider for MCPToolProvider {
    fn get_commands(&self) -> Vec<Command> {
        self.tools.iter().map(|tool| {
            Command {
                id: format!("tool_{}", tool.name),
                label: format!("Tool: {}", tool.name),
                description: Some(tool.description.clone()),
                category: CommandCategory::Tool,
                shortcut: None,
                icon: Some("🔧".to_string()),
                action: CommandAction::ExecuteTool(tool.name.clone()),
                keywords: vec!["tool", "mcp", &tool.name, &tool.server]
                    .into_iter().map(String::from).collect(),
            }
        }).collect()
    }

    fn refresh(&mut self) {
        // Would query MCP servers for new tools
    }
}

/// AI suggestion provider - smart contextual commands
pub struct AIProvider {
    context: Arc<RwLock<ChatContext>>,
}

struct ChatContext {
    last_message: String,
    current_topic: String,
    user_intent: String,
}

impl AIProvider {
    pub fn new() -> Self {
        Self {
            context: Arc::new(RwLock::new(ChatContext {
                last_message: String::new(),
                current_topic: "general".to_string(),
                user_intent: "exploring".to_string(),
            })),
        }
    }

    fn generate_contextual_commands(&self) -> Vec<Command> {
        let mut commands = Vec::new();

        if let Ok(ctx) = self.context.read() {
            // If user asked about balance, suggest related commands
            if ctx.last_message.contains("balance") {
                commands.push(Command {
                    id: "suggested_stake".to_string(),
                    label: "💡 Stake your SOL".to_string(),
                    description: Some("AI suggests: You have enough to stake".to_string()),
                    category: CommandCategory::Action,
                    shortcut: None,
                    icon: Some("🤖".to_string()),
                    action: CommandAction::StakeSOL,
                    keywords: vec!["ai", "suggestion", "stake"].into_iter()
                        .map(String::from).collect(),
                });
            }

            // If discussing transactions, suggest viewing history
            if ctx.current_topic == "transactions" {
                commands.push(Command {
                    id: "suggested_history".to_string(),
                    label: "💡 View transaction history".to_string(),
                    description: Some("AI suggests: Check your recent activity".to_string()),
                    category: CommandCategory::Action,
                    shortcut: None,
                    icon: Some("🤖".to_string()),
                    action: CommandAction::Custom(Arc::new(|_| {
                        // Show transaction history
                    })),
                    keywords: vec!["ai", "suggestion", "history"].into_iter()
                        .map(String::from).collect(),
                });
            }
        }

        commands
    }
}

impl CommandProvider for AIProvider {
    fn get_commands(&self) -> Vec<Command> {
        self.generate_contextual_commands()
    }

    fn refresh(&mut self) {
        // Would update context based on chat
    }
}

/// Create all default providers
pub fn create_default_providers() -> Vec<Box<dyn CommandProvider>> {
    vec![
        Box::new(FileProvider::new()),
        Box::new(SessionProvider::new()),
        Box::new(MCPToolProvider::new()),
        Box::new(AIProvider::new()),
    ]
}