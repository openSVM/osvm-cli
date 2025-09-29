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
}
