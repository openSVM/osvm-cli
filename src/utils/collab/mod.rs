//! Collaborative TUI Investigation System
//!
//! This module enables real-time collaborative blockchain investigation where multiple
//! investigators can share a TUI session, see each other's cursors, and annotate findings.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                    COLLABORATIVE INVESTIGATION                          │
//! │                                                                         │
//! │  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐             │
//! │  │  Investigator │    │  Investigator │    │  Investigator │             │
//! │  │      A        │    │      B        │    │      C        │             │
//! │  │  (Terminal)   │    │  (Browser)    │    │  (SSH)        │             │
//! │  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘             │
//! │         │                   │                   │                      │
//! │         │ tmux attach       │ WebSocket         │ tmux attach          │
//! │         │                   │                   │                      │
//! │         ▼                   ▼                   ▼                      │
//! │  ┌─────────────────────────────────────────────────────────────┐      │
//! │  │                   CollaborativeSession                       │      │
//! │  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │      │
//! │  │  │  Presence   │  │ Annotations │  │   Cursor    │         │      │
//! │  │  │  Manager    │  │   Store     │  │  Tracker    │         │      │
//! │  │  └─────────────┘  └─────────────┘  └─────────────┘         │      │
//! │  │                                                              │      │
//! │  │  ┌─────────────────────────────────────────────────────┐   │      │
//! │  │  │              Shared tmux Session                     │   │      │
//! │  │  │  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐    │   │      │
//! │  │  │  │ Pane 0 │  │ Pane 1 │  │ Pane 2 │  │ Pane 3 │    │   │      │
//! │  │  │  │Research│  │ Graph  │  │ Chat   │  │ Notes  │    │   │      │
//! │  │  │  └────────┘  └────────┘  └────────┘  └────────┘    │   │      │
//! │  │  └─────────────────────────────────────────────────────┘   │      │
//! │  └─────────────────────────────────────────────────────────────┘      │
//! │                                                                         │
//! │  ┌─────────────────────────────────────────────────────────────┐      │
//! │  │                    WebSocket Server                          │      │
//! │  │  - Real-time presence updates                                │      │
//! │  │  - Annotation sync                                           │      │
//! │  │  - Screen streaming for browser clients                      │      │
//! │  └─────────────────────────────────────────────────────────────┘      │
//! └─────────────────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Usage
//!
//! ## Starting a Collaborative Session (Host)
//!
//! ```bash
//! # Start a new collaborative investigation
//! osvm collab start --wallet 5Q544f...e4j1 --name "Hack Investigation"
//!
//! # Output:
//! # Session ID: abc123
//! # Invite URL: https://osvm.local:8080/collab/abc123
//! # tmux attach: tmux attach -t osvm_collab_abc123
//! ```
//!
//! ## Joining a Session (Participant)
//!
//! ```bash
//! # Join via CLI (direct tmux)
//! osvm collab join abc123
//!
//! # Or via browser at the invite URL
//! ```
//!
//! ## Annotations
//!
//! ```bash
//! # Add annotation to a wallet (visible to all participants)
//! osvm collab annotate 5Q544f...e4j1 "Likely exchange hot wallet"
//!
//! # List annotations
//! osvm collab annotations
//! ```

pub mod session;
pub mod presence;
pub mod annotations;
pub mod websocket;
pub mod federation;

pub use session::{CollaborativeSession, SessionConfig, SessionRole};
pub use presence::{PresenceManager, UserPresence, CursorPosition};
pub use annotations::{Annotation, AnnotationType, AnnotationStore, AnnotationSeverity};
pub use websocket::{CollabWebSocketServer, CollabEvent};
pub use federation::{
    CollabFederationManager, FederatedSessionAnnouncement, FederatedSessionStatus,
    FederatedAnnotation, SessionRelayInfo, FederatedSessionBuilder, FederationState,
    COLLAB_SESSIONS_BOARD, COLLAB_ANNOTATIONS_BOARD,
};

use std::sync::Arc;
use tokio::sync::RwLock;

/// Global collaborative session registry
pub struct CollabRegistry {
    sessions: RwLock<std::collections::HashMap<String, Arc<CollaborativeSession>>>,
}

impl CollabRegistry {
    pub fn new() -> Self {
        Self {
            sessions: RwLock::new(std::collections::HashMap::new()),
        }
    }

    /// Create a new collaborative session
    pub async fn create_session(&self, config: SessionConfig) -> Result<Arc<CollaborativeSession>, CollabError> {
        let session = Arc::new(CollaborativeSession::new(config).await?);
        let session_id = session.id().to_string();

        let mut sessions = self.sessions.write().await;
        sessions.insert(session_id.clone(), Arc::clone(&session));

        Ok(session)
    }

    /// Get a session by ID
    pub async fn get_session(&self, id: &str) -> Option<Arc<CollaborativeSession>> {
        let sessions = self.sessions.read().await;
        sessions.get(id).cloned()
    }

    /// List all active sessions
    pub async fn list_sessions(&self) -> Vec<(String, String)> {
        let sessions = self.sessions.read().await;
        sessions.iter()
            .map(|(id, session)| (id.clone(), session.name().to_string()))
            .collect()
    }

    /// Remove a session
    pub async fn remove_session(&self, id: &str) -> Option<Arc<CollaborativeSession>> {
        let mut sessions = self.sessions.write().await;
        sessions.remove(id)
    }
}

impl Default for CollabRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur in collaborative sessions
#[derive(Debug, thiserror::Error)]
pub enum CollabError {
    #[error("tmux error: {0}")]
    TmuxError(String),

    #[error("session not found: {0}")]
    SessionNotFound(String),

    #[error("permission denied: {0}")]
    PermissionDenied(String),

    #[error("WebSocket error: {0}")]
    WebSocketError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("serialization error: {0}")]
    SerializationError(String),

    #[error("session full: max {0} participants")]
    SessionFull(usize),

    #[error("invalid invite code")]
    InvalidInviteCode,
}
