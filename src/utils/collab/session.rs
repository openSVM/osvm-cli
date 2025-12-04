//! Collaborative Session Management
//!
//! Manages shared tmux sessions for real-time collaborative investigation.

use super::{AnnotationStore, CollabError, PresenceManager};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::process::Command;
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};
use uuid::Uuid;

/// Configuration for creating a collaborative session
#[derive(Debug, Clone)]
pub struct SessionConfig {
    /// Human-readable session name
    pub name: String,
    /// Initial wallet address to investigate (optional)
    pub target_wallet: Option<String>,
    /// Maximum number of participants (0 = unlimited)
    pub max_participants: usize,
    /// Terminal width
    pub width: u16,
    /// Terminal height
    pub height: u16,
    /// Password for joining (optional)
    pub password: Option<String>,
    /// Allow browser-based participants
    pub allow_browser_clients: bool,
    /// Session expiry duration in hours (0 = never)
    pub expiry_hours: u32,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            name: "Investigation".to_string(),
            target_wallet: None,
            max_participants: 10,
            width: 200,
            height: 60,
            password: None,
            allow_browser_clients: true,
            expiry_hours: 24,
        }
    }
}

/// Role of a participant in the session
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SessionRole {
    /// Full control - can send input, add annotations
    Host,
    /// Can send input, add annotations
    Editor,
    /// Can only view (read-only)
    Viewer,
}

/// Information about a session participant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Participant {
    pub id: String,
    pub name: String,
    pub role: SessionRole,
    pub connected_at: DateTime<Utc>,
    pub last_activity: DateTime<Utc>,
    pub client_type: ClientType,
    pub color: ParticipantColor,
}

/// How the participant is connected
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ClientType {
    /// Direct tmux attach
    Tmux,
    /// WebSocket from browser
    Browser,
    /// SSH tunnel
    Ssh,
}

/// Color assigned to participant for cursor/annotation display
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ParticipantColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl ParticipantColor {
    /// Generate a unique color based on participant index
    pub fn from_index(index: usize) -> Self {
        const COLORS: [(u8, u8, u8); 10] = [
            (0, 255, 127),   // Spring Green
            (255, 99, 71),   // Tomato
            (100, 149, 237), // Cornflower Blue
            (255, 215, 0),   // Gold
            (186, 85, 211),  // Medium Orchid
            (0, 206, 209),   // Dark Turquoise
            (255, 127, 80),  // Coral
            (144, 238, 144), // Light Green
            (255, 182, 193), // Light Pink
            (176, 196, 222), // Light Steel Blue
        ];
        let (r, g, b) = COLORS[index % COLORS.len()];
        Self { r, g, b }
    }

    /// Convert to ANSI escape code
    pub fn to_ansi(&self) -> String {
        format!("\x1b[38;2;{};{};{}m", self.r, self.g, self.b)
    }
}

/// Events broadcast to all session participants
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum SessionEvent {
    /// Participant joined
    ParticipantJoined { participant: Participant },
    /// Participant left
    ParticipantLeft { participant_id: String },
    /// Screen content update (for browser clients)
    ScreenUpdate { content: String, pane: usize },
    /// Cursor moved
    CursorMoved {
        participant_id: String,
        x: u16,
        y: u16,
        pane: usize,
    },
    /// Annotation added
    AnnotationAdded {
        annotation_id: String,
        participant_id: String,
        target: String,
        text: String,
    },
    /// Chat message
    ChatMessage {
        participant_id: String,
        message: String,
    },
    /// Session configuration changed
    ConfigChanged {
        name: String,
        target_wallet: Option<String>,
    },
    /// Pane focused
    PaneFocused { participant_id: String, pane: usize },
}

/// A collaborative investigation session
pub struct CollaborativeSession {
    /// Unique session ID
    id: String,
    /// Session name
    name: String,
    /// tmux session name
    tmux_session: String,
    /// Session configuration
    config: SessionConfig,
    /// Connected participants
    participants: RwLock<HashMap<String, Participant>>,
    /// Presence manager
    presence: Arc<PresenceManager>,
    /// Annotation store
    annotations: Arc<AnnotationStore>,
    /// Event broadcast channel
    event_tx: broadcast::Sender<SessionEvent>,
    /// Creation time
    created_at: DateTime<Utc>,
    /// Invite code (short code for sharing)
    invite_code: String,
    /// Number of panes
    pane_count: RwLock<usize>,
}

impl CollaborativeSession {
    /// Create a new collaborative session
    pub async fn new(config: SessionConfig) -> Result<Self, CollabError> {
        let id = Uuid::new_v4().to_string();
        let invite_code = Self::generate_invite_code();
        let tmux_session = format!("osvm_collab_{}", &id[..8]);

        // Create the tmux session
        let output = Command::new("tmux")
            .args([
                "new-session",
                "-d",
                "-s",
                &tmux_session,
                "-x",
                &config.width.to_string(),
                "-y",
                &config.height.to_string(),
            ])
            .output()
            .map_err(|e| CollabError::IoError(e))?;

        if !output.status.success() {
            return Err(CollabError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        // If we have a target wallet, start the investigation TUI
        if let Some(wallet) = &config.target_wallet {
            let _ = Command::new("tmux")
                .args([
                    "send-keys",
                    "-t",
                    &tmux_session,
                    &format!("osvm research {} --tui", wallet),
                    "Enter",
                ])
                .output();
        }

        let (event_tx, _) = broadcast::channel(1024);

        Ok(Self {
            id,
            name: config.name.clone(),
            tmux_session,
            config,
            participants: RwLock::new(HashMap::new()),
            presence: Arc::new(PresenceManager::new()),
            annotations: Arc::new(AnnotationStore::new()),
            event_tx,
            created_at: Utc::now(),
            invite_code,
            pane_count: RwLock::new(1),
        })
    }

    /// Generate a short invite code
    fn generate_invite_code() -> String {
        use rand::Rng;
        const CHARSET: &[u8] = b"ABCDEFGHJKLMNPQRSTUVWXYZ23456789";
        let mut rng = rand::rng();
        (0..6)
            .map(|_| {
                let idx = rng.random_range(0..CHARSET.len());
                CHARSET[idx] as char
            })
            .collect()
    }

    /// Get session ID
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get session name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get invite code
    pub fn invite_code(&self) -> &str {
        &self.invite_code
    }

    /// Get tmux session name
    pub fn tmux_session(&self) -> &str {
        &self.tmux_session
    }

    /// Subscribe to session events
    pub fn subscribe(&self) -> broadcast::Receiver<SessionEvent> {
        self.event_tx.subscribe()
    }

    /// Add a participant to the session
    pub async fn add_participant(
        &self,
        name: String,
        role: SessionRole,
        client_type: ClientType,
    ) -> Result<Participant, CollabError> {
        let mut participants = self.participants.write().await;

        // Check max participants
        if self.config.max_participants > 0 && participants.len() >= self.config.max_participants {
            return Err(CollabError::SessionFull(self.config.max_participants));
        }

        let participant = Participant {
            id: Uuid::new_v4().to_string(),
            name,
            role,
            connected_at: Utc::now(),
            last_activity: Utc::now(),
            client_type,
            color: ParticipantColor::from_index(participants.len()),
        };

        participants.insert(participant.id.clone(), participant.clone());

        // Broadcast join event
        let _ = self.event_tx.send(SessionEvent::ParticipantJoined {
            participant: participant.clone(),
        });

        Ok(participant)
    }

    /// Remove a participant
    pub async fn remove_participant(&self, participant_id: &str) {
        let mut participants = self.participants.write().await;
        if participants.remove(participant_id).is_some() {
            let _ = self.event_tx.send(SessionEvent::ParticipantLeft {
                participant_id: participant_id.to_string(),
            });
        }
    }

    /// Get list of participants
    pub async fn list_participants(&self) -> Vec<Participant> {
        let participants = self.participants.read().await;
        participants.values().cloned().collect()
    }

    /// Get participant count
    pub async fn participant_count(&self) -> usize {
        let participants = self.participants.read().await;
        participants.len()
    }

    /// Send keys to the tmux session (for input)
    pub fn send_keys(&self, keys: &str, pane: Option<usize>) -> Result<(), CollabError> {
        let target = match pane {
            Some(p) => format!("{}:{}.{}", self.tmux_session, 0, p),
            None => self.tmux_session.clone(),
        };

        let output = Command::new("tmux")
            .args(["send-keys", "-t", &target, keys])
            .output()
            .map_err(|e| CollabError::IoError(e))?;

        if !output.status.success() {
            return Err(CollabError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(())
    }

    /// Capture current screen content
    pub fn capture_screen(&self, pane: Option<usize>) -> Result<String, CollabError> {
        let target = match pane {
            Some(p) => format!("{}:{}.{}", self.tmux_session, 0, p),
            None => format!("{}:{}", self.tmux_session, 0),
        };

        let output = Command::new("tmux")
            .args(["capture-pane", "-t", &target, "-p", "-e"])
            .output()
            .map_err(|e| CollabError::IoError(e))?;

        if !output.status.success() {
            return Err(CollabError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    /// Capture screen with ANSI codes (for browser rendering)
    pub fn capture_screen_ansi(&self, pane: Option<usize>) -> Result<String, CollabError> {
        let target = match pane {
            Some(p) => format!("{}:{}.{}", self.tmux_session, 0, p),
            None => format!("{}:{}", self.tmux_session, 0),
        };

        let output = Command::new("tmux")
            .args(["capture-pane", "-t", &target, "-p", "-e"])
            .output()
            .map_err(|e| CollabError::IoError(e))?;

        if !output.status.success() {
            return Err(CollabError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    /// Split into multiple panes
    pub async fn split_panes(&self, count: usize) -> Result<(), CollabError> {
        let mut current_count = self.pane_count.write().await;

        for i in *current_count..count {
            let direction = if i % 2 == 1 { "-h" } else { "-v" };

            let output = Command::new("tmux")
                .args(["split-window", direction, "-t", &self.tmux_session])
                .output()
                .map_err(|e| CollabError::IoError(e))?;

            if !output.status.success() {
                return Err(CollabError::TmuxError(
                    String::from_utf8_lossy(&output.stderr).to_string(),
                ));
            }
        }

        // Apply tiled layout
        let _ = Command::new("tmux")
            .args(["select-layout", "-t", &self.tmux_session, "tiled"])
            .output();

        *current_count = count;
        Ok(())
    }

    /// Get the presence manager
    pub fn presence(&self) -> &Arc<PresenceManager> {
        &self.presence
    }

    /// Get the annotation store
    pub fn annotations(&self) -> &Arc<AnnotationStore> {
        &self.annotations
    }

    /// Broadcast a chat message
    pub fn broadcast_chat(&self, participant_id: &str, message: &str) {
        let _ = self.event_tx.send(SessionEvent::ChatMessage {
            participant_id: participant_id.to_string(),
            message: message.to_string(),
        });
    }

    /// Get connection info for joining
    pub fn connection_info(&self) -> SessionConnectionInfo {
        SessionConnectionInfo {
            session_id: self.id.clone(),
            invite_code: self.invite_code.clone(),
            tmux_session: self.tmux_session.clone(),
            name: self.name.clone(),
            target_wallet: self.config.target_wallet.clone(),
            allow_browser: self.config.allow_browser_clients,
            created_at: self.created_at,
        }
    }

    /// Check if session is expired
    pub fn is_expired(&self) -> bool {
        if self.config.expiry_hours == 0 {
            return false;
        }
        let expiry = self.created_at + chrono::Duration::hours(self.config.expiry_hours as i64);
        Utc::now() > expiry
    }
}

impl Drop for CollaborativeSession {
    fn drop(&mut self) {
        // Kill the tmux session when done
        let _ = Command::new("tmux")
            .args(["kill-session", "-t", &self.tmux_session])
            .output();
    }
}

/// Information needed to connect to a session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionConnectionInfo {
    pub session_id: String,
    pub invite_code: String,
    pub tmux_session: String,
    pub name: String,
    pub target_wallet: Option<String>,
    pub allow_browser: bool,
    pub created_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_participant_colors() {
        for i in 0..15 {
            let color = ParticipantColor::from_index(i);
            let ansi = color.to_ansi();
            assert!(ansi.starts_with("\x1b[38;2;"));
        }
    }

    #[test]
    fn test_invite_code_generation() {
        let code1 = CollaborativeSession::generate_invite_code();
        let code2 = CollaborativeSession::generate_invite_code();

        assert_eq!(code1.len(), 6);
        assert_eq!(code2.len(), 6);
        assert_ne!(code1, code2); // Statistically improbable to be equal
    }

    #[test]
    fn test_session_config_default() {
        let config = SessionConfig::default();
        assert_eq!(config.max_participants, 10);
        assert_eq!(config.width, 200);
        assert_eq!(config.height, 60);
        assert!(config.allow_browser_clients);
    }
}
