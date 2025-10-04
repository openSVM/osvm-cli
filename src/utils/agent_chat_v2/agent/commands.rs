//! Agent execution commands and types

use super::super::types::AgentState;
use tokio::sync::oneshot;
use uuid::Uuid;

/// Agent execution commands
#[derive(Debug)]
pub enum AgentCommand {
    ProcessInput {
        session_id: Uuid,
        input: String,
    },
    PauseAgent {
        session_id: Uuid,
    },
    ResumeAgent {
        session_id: Uuid,
    },
    StopAgent {
        session_id: Uuid,
    },
    GetStatus {
        session_id: Uuid,
        response: oneshot::Sender<AgentState>,
    },
    ThemeCommand {
        session_id: Uuid,
        command: ThemeCommandType,
    },
}

/// Theme-related commands
#[derive(Debug)]
pub enum ThemeCommandType {
    ListThemes,
    SwitchTheme(String),
    PreviewTheme(Option<String>), // None for current theme
    ShowCurrentTheme,
}
