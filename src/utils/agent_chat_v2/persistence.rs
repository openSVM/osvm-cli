//! Session persistence for chat2
//! Save and load chat sessions from disk

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use uuid::Uuid;

use super::session::ChatSession;

/// Persisted state container
#[derive(Serialize, Deserialize)]
pub struct PersistedState {
    pub sessions: HashMap<Uuid, ChatSession>,
    pub active_session_id: Option<Uuid>,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}

impl PersistedState {
    /// Get the path to the persistence file
    pub fn get_path() -> Result<PathBuf> {
        let home = std::env::var("HOME").context("HOME not set")?;
        let path = PathBuf::from(home)
            .join(".osvm")
            .join("chat_sessions")
            .join("state.json");

        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        Ok(path)
    }

    /// Load state from disk
    pub fn load() -> Result<Option<Self>> {
        let path = Self::get_path()?;

        if !path.exists() {
            return Ok(None);
        }

        let content = std::fs::read_to_string(&path)?;
        let state: PersistedState =
            serde_json::from_str(&content).context("Failed to parse persisted state")?;

        Ok(Some(state))
    }

    /// Save state to disk
    pub fn save(
        sessions: &HashMap<Uuid, ChatSession>,
        active_session_id: Option<Uuid>,
    ) -> Result<()> {
        let state = PersistedState {
            sessions: sessions.clone(),
            active_session_id,
            last_updated: chrono::Utc::now(),
        };

        let path = Self::get_path()?;
        let json = serde_json::to_string_pretty(&state)?;
        std::fs::write(&path, json)?;

        Ok(())
    }

    /// Create default sessions (only if no saved state exists)
    pub fn create_defaults() -> HashMap<Uuid, ChatSession> {
        let mut sessions = HashMap::new();

        let main_id = Uuid::new_v4();
        sessions.insert(
            main_id,
            ChatSession {
                id: main_id,
                name: "Main Chat".to_string(),
                created_at: chrono::Utc::now(),
                messages: Vec::new(),
                agent_state: super::types::AgentState::Idle,
                recording: false,
                recording_file: None,
            },
        );

        sessions
    }
}
