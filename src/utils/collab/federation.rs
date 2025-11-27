//! Federated Collaborative Investigation Network
//!
//! Connects collaborative sessions across multiple OSVM instances using
//! the BBS federation infrastructure.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────────┐
//! │                    FEDERATED INVESTIGATION NETWORK                          │
//! │                                                                             │
//! │    ┌─────────────────┐          ┌─────────────────┐          ┌────────────┐│
//! │    │   OSVM Node A   │          │   OSVM Node B   │          │ OSVM Node C││
//! │    │  (Investigator) │          │  (Analyst)      │          │ (Expert)   ││
//! │    │                 │          │                 │          │            ││
//! │    │ ┌─────────────┐ │          │ ┌─────────────┐ │          │            ││
//! │    │ │Session:     │ │◄────────►│ │Remote View  │ │◄────────►│            ││
//! │    │ │ "Hack Inv"  │ │ WebSocket│ │of Session   │ │          │            ││
//! │    │ └─────────────┘ │  Relay   │ └─────────────┘ │          │            ││
//! │    └────────┬────────┘          └────────┬────────┘          └────────────┘│
//! │             │                            │                                  │
//! │             │      BBS Federation        │                                  │
//! │             │    ┌────────────────┐     │                                  │
//! │             └───►│ COLLAB_SESSIONS│◄────┘                                  │
//! │                  │     Board      │                                        │
//! │                  │                │                                        │
//! │                  │ • Session list │                                        │
//! │                  │ • Annotations  │                                        │
//! │                  │ • Presence     │                                        │
//! │                  └────────────────┘                                        │
//! └─────────────────────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Protocol
//!
//! 1. **Session Publishing**: When a session is created with `--federated`, it's
//!    posted to the COLLAB_SESSIONS board
//!
//! 2. **Discovery**: Nodes sync the board to discover available sessions
//!
//! 3. **Joining**: Remote participants connect via WebSocket relay through the
//!    hosting node's collab server
//!
//! 4. **Annotation Sync**: Annotations are posted to the session's annotation
//!    thread, syncing across all federated nodes

use super::{
    CollabError, CollaborativeSession, SessionConfig,
    Annotation, AnnotationType, AnnotationSeverity,
    session::ParticipantColor,
};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::{DateTime, Utc};

/// Board name for collaborative session announcements
pub const COLLAB_SESSIONS_BOARD: &str = "COLLAB_SESSIONS";

/// Board name for federated annotations
pub const COLLAB_ANNOTATIONS_BOARD: &str = "COLLAB_ANNOTATIONS";

/// A federated session announcement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederatedSessionAnnouncement {
    /// Session ID
    pub session_id: String,
    /// Session name
    pub name: String,
    /// Invite code
    pub invite_code: String,
    /// Host node ID
    pub host_node_id: String,
    /// Host node address (WebSocket URL)
    pub host_address: String,
    /// Target wallet being investigated (optional)
    pub target_wallet: Option<String>,
    /// Session description
    pub description: Option<String>,
    /// Current participant count
    pub participant_count: usize,
    /// Maximum participants (0 = unlimited)
    pub max_participants: usize,
    /// Is password protected?
    pub password_protected: bool,
    /// Session creation time
    pub created_at: DateTime<Utc>,
    /// Last activity time
    pub last_activity: DateTime<Utc>,
    /// Session tags for filtering
    pub tags: Vec<String>,
    /// Session status
    pub status: FederatedSessionStatus,
}

/// Status of a federated session
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FederatedSessionStatus {
    /// Session is active and accepting participants
    Active,
    /// Session is full
    Full,
    /// Session is paused (host away)
    Paused,
    /// Session has ended
    Ended,
}

impl FederatedSessionAnnouncement {
    /// Create announcement from a local session
    pub fn from_session(
        session: &CollaborativeSession,
        host_node_id: &str,
        host_address: &str,
        participant_count: usize,
    ) -> Self {
        let info = session.connection_info();
        Self {
            session_id: info.session_id.clone(),
            name: info.name.clone(),
            invite_code: info.invite_code.clone(),
            host_node_id: host_node_id.to_string(),
            host_address: host_address.to_string(),
            target_wallet: info.target_wallet.clone(),
            description: None,
            participant_count,
            max_participants: 10, // Default
            password_protected: false,
            created_at: info.created_at,
            last_activity: Utc::now(),
            tags: Vec::new(),
            status: FederatedSessionStatus::Active,
        }
    }

    /// Get the WebSocket URL for joining
    pub fn websocket_url(&self) -> String {
        // Convert HTTP address to WebSocket
        let ws_addr = self.host_address
            .replace("http://", "ws://")
            .replace("https://", "wss://");
        format!("{}/collab/{}", ws_addr, self.session_id)
    }

    /// Check if session is joinable
    pub fn is_joinable(&self) -> bool {
        self.status == FederatedSessionStatus::Active
            && (self.max_participants == 0 || self.participant_count < self.max_participants)
    }
}

/// A federated annotation (synced via BBS)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederatedAnnotation {
    /// Unique annotation ID
    pub annotation_id: String,
    /// Session this annotation belongs to
    pub session_id: String,
    /// Target type (wallet, transaction, etc.)
    pub target_type: String,
    /// Target identifier
    pub target_id: String,
    /// Annotation text
    pub text: String,
    /// Severity level
    pub severity: String,
    /// Author node ID
    pub author_node_id: String,
    /// Author display name
    pub author_name: String,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Tags
    pub tags: Vec<String>,
}

impl FederatedAnnotation {
    /// Create from a local annotation
    pub fn from_annotation(
        annotation: &Annotation,
        session_id: &str,
        author_node_id: &str,
    ) -> Self {
        let (target_type, target_id) = match &annotation.target {
            AnnotationType::Wallet(addr) => ("wallet", addr.clone()),
            AnnotationType::Transaction(sig) => ("transaction", sig.clone()),
            AnnotationType::Token(mint) => ("token", mint.clone()),
            AnnotationType::Program(id) => ("program", id.clone()),
            AnnotationType::ScreenPosition { x, y, pane } => {
                ("screen", format!("{}:{}:{}", x, y, pane))
            }
            AnnotationType::Note => ("note", "".to_string()),
        };

        Self {
            annotation_id: annotation.id.clone(),
            session_id: session_id.to_string(),
            target_type: target_type.to_string(),
            target_id,
            text: annotation.text.clone(),
            severity: format!("{:?}", annotation.severity),
            author_node_id: author_node_id.to_string(),
            author_name: annotation.author_name.clone(),
            created_at: annotation.created_at,
            tags: annotation.tags.clone(),
        }
    }

    /// Convert back to local annotation
    pub fn to_annotation(&self) -> Annotation {
        let target = match self.target_type.as_str() {
            "wallet" => AnnotationType::Wallet(self.target_id.clone()),
            "transaction" => AnnotationType::Transaction(self.target_id.clone()),
            "token" => AnnotationType::Token(self.target_id.clone()),
            "program" => AnnotationType::Program(self.target_id.clone()),
            "screen" => {
                let parts: Vec<&str> = self.target_id.split(':').collect();
                if parts.len() == 3 {
                    AnnotationType::ScreenPosition {
                        x: parts[0].parse().unwrap_or(0),
                        y: parts[1].parse().unwrap_or(0),
                        pane: parts[2].parse().unwrap_or(0),
                    }
                } else {
                    AnnotationType::Note
                }
            }
            _ => AnnotationType::Note,
        };

        let severity = match self.severity.to_lowercase().as_str() {
            "critical" => AnnotationSeverity::Critical,
            "warning" => AnnotationSeverity::Warning,
            "important" => AnnotationSeverity::Important,
            "question" => AnnotationSeverity::Question,
            _ => AnnotationSeverity::Info,
        };

        Annotation::new(
            target,
            self.text.clone(),
            severity,
            self.author_node_id.clone(),
            self.author_name.clone(),
            ParticipantColor::from_index(0),
        )
    }
}

/// Persistent state for federation (saved to disk)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct FederationState {
    /// Our node ID
    pub node_id: String,
    /// Our public address
    pub public_address: String,
    /// Configured peers (node_id -> address)
    pub peers: HashMap<String, String>,
    /// Known sessions (for quick re-discovery)
    pub known_sessions: Vec<FederatedSessionAnnouncement>,
    /// Last sync timestamp
    pub last_sync: Option<DateTime<Utc>>,
}

impl FederationState {
    /// Default state file path
    pub fn default_path() -> std::path::PathBuf {
        dirs::home_dir()
            .unwrap_or_else(|| std::path::PathBuf::from("."))
            .join(".osvm")
            .join("collab")
            .join("federation.json")
    }

    /// Load state from file
    pub fn load() -> Self {
        Self::load_from(&Self::default_path())
    }

    /// Load state from specific path
    pub fn load_from(path: &std::path::Path) -> Self {
        if let Ok(contents) = std::fs::read_to_string(path) {
            if let Ok(state) = serde_json::from_str(&contents) {
                return state;
            }
        }
        Self::default()
    }

    /// Save state to file
    pub fn save(&self) -> Result<(), std::io::Error> {
        self.save_to(&Self::default_path())
    }

    /// Save state to specific path
    pub fn save_to(&self, path: &std::path::Path) -> Result<(), std::io::Error> {
        // Create parent directories if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let json = serde_json::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;

        std::fs::write(path, json)
    }
}

/// Manager for federated collaborative sessions
pub struct CollabFederationManager {
    /// Our node ID
    node_id: String,
    /// Our public address
    public_address: String,
    /// Known federated sessions (session_id -> announcement)
    sessions: Arc<RwLock<HashMap<String, FederatedSessionAnnouncement>>>,
    /// Local sessions we're hosting
    local_sessions: Arc<RwLock<HashMap<String, Arc<CollaborativeSession>>>>,
    /// HTTP client
    client: reqwest::Client,
    /// Federation peers (node_id -> address)
    peers: Arc<RwLock<HashMap<String, String>>>,
    /// State file path
    state_path: std::path::PathBuf,
}

impl CollabFederationManager {
    /// Create a new federation manager (loads state from disk)
    pub fn new(node_id: &str, public_address: &str) -> Self {
        Self::new_with_path(node_id, public_address, FederationState::default_path())
    }

    /// Create with custom state path
    pub fn new_with_path(node_id: &str, public_address: &str, state_path: std::path::PathBuf) -> Self {
        // Load existing state
        let state = FederationState::load_from(&state_path);

        // Use stored node_id if available, otherwise use provided
        let effective_node_id = if !state.node_id.is_empty() {
            state.node_id.clone()
        } else {
            node_id.to_string()
        };

        // Initialize sessions from stored known_sessions
        let mut sessions_map = HashMap::new();
        for session in state.known_sessions {
            if session.status != FederatedSessionStatus::Ended {
                sessions_map.insert(session.session_id.clone(), session);
            }
        }

        Self {
            node_id: effective_node_id,
            public_address: public_address.to_string(),
            sessions: Arc::new(RwLock::new(sessions_map)),
            local_sessions: Arc::new(RwLock::new(HashMap::new())),
            client: reqwest::Client::builder()
                .timeout(std::time::Duration::from_secs(10))
                .build()
                .expect("Failed to build HTTP client"),
            peers: Arc::new(RwLock::new(state.peers)),
            state_path,
        }
    }

    /// Save current state to disk
    pub async fn save_state(&self) -> Result<(), std::io::Error> {
        let peers = self.peers.read().await;
        let sessions = self.sessions.read().await;

        let state = FederationState {
            node_id: self.node_id.clone(),
            public_address: self.public_address.clone(),
            peers: peers.clone(),
            known_sessions: sessions.values().cloned().collect(),
            last_sync: Some(Utc::now()),
        };

        state.save_to(&self.state_path)
    }

    /// Get our node ID
    pub fn node_id(&self) -> &str {
        &self.node_id
    }

    /// Add a federation peer (auto-saves to disk)
    pub async fn add_peer(&self, node_id: &str, address: &str) -> Result<(), std::io::Error> {
        {
            let mut peers = self.peers.write().await;
            peers.insert(node_id.to_string(), address.to_string());
        }
        self.save_state().await
    }

    /// Remove a federation peer (auto-saves to disk)
    pub async fn remove_peer(&self, node_id: &str) -> Result<(), std::io::Error> {
        {
            let mut peers = self.peers.write().await;
            peers.remove(node_id);
        }
        self.save_state().await
    }

    /// List all known peers
    pub async fn list_peers(&self) -> Vec<(String, String)> {
        let peers = self.peers.read().await;
        peers.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }

    /// Publish a local session to the federation
    pub async fn publish_session(&self, session: Arc<CollaborativeSession>) -> Result<(), CollabError> {
        let participant_count = session.participant_count().await;
        let announcement = FederatedSessionAnnouncement::from_session(
            &session,
            &self.node_id,
            &self.public_address,
            participant_count,
        );

        // Store locally
        {
            let mut local = self.local_sessions.write().await;
            local.insert(session.id().to_string(), Arc::clone(&session));
        }

        {
            let mut sessions = self.sessions.write().await;
            sessions.insert(announcement.session_id.clone(), announcement.clone());
        }

        // Broadcast to peers
        self.broadcast_announcement(&announcement).await;

        Ok(())
    }

    /// Unpublish a session
    pub async fn unpublish_session(&self, session_id: &str) {
        {
            let mut local = self.local_sessions.write().await;
            local.remove(session_id);
        }

        {
            let mut sessions = self.sessions.write().await;
            if let Some(mut announcement) = sessions.get_mut(session_id) {
                announcement.status = FederatedSessionStatus::Ended;
            }
        }

        // Broadcast the end status
        if let Some(announcement) = self.sessions.read().await.get(session_id) {
            self.broadcast_announcement(announcement).await;
        }
    }

    /// Broadcast an announcement to all peers
    async fn broadcast_announcement(&self, announcement: &FederatedSessionAnnouncement) {
        let peers = self.peers.read().await;

        for (_, peer_address) in peers.iter() {
            let url = format!("{}/api/collab/federation/announce", peer_address);
            let _ = self.client
                .post(&url)
                .json(announcement)
                .send()
                .await;
        }
    }

    /// Receive an announcement from a peer
    pub async fn receive_announcement(&self, announcement: FederatedSessionAnnouncement) {
        // Don't store our own announcements received back
        if announcement.host_node_id == self.node_id {
            return;
        }

        let mut sessions = self.sessions.write().await;
        sessions.insert(announcement.session_id.clone(), announcement);
    }

    /// Discover sessions from all peers
    pub async fn discover_sessions(&self) -> Result<Vec<FederatedSessionAnnouncement>, CollabError> {
        let peers = self.peers.read().await;
        let mut discovered = Vec::new();

        for (_, peer_address) in peers.iter() {
            let url = format!("{}/api/collab/federation/sessions", peer_address);

            match self.client.get(&url).send().await {
                Ok(response) => {
                    if let Ok(sessions) = response.json::<Vec<FederatedSessionAnnouncement>>().await {
                        for session in sessions {
                            if session.host_node_id != self.node_id {
                                discovered.push(session);
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Failed to discover from {}: {}", peer_address, e);
                }
            }
        }

        // Update our cache
        {
            let mut sessions = self.sessions.write().await;
            for session in &discovered {
                sessions.insert(session.session_id.clone(), session.clone());
            }
        }

        Ok(discovered)
    }

    /// Get all known sessions (local + remote)
    pub async fn list_all_sessions(&self) -> Vec<FederatedSessionAnnouncement> {
        let sessions = self.sessions.read().await;
        sessions.values()
            .filter(|s| s.status != FederatedSessionStatus::Ended)
            .cloned()
            .collect()
    }

    /// Get active joinable sessions
    pub async fn list_joinable_sessions(&self) -> Vec<FederatedSessionAnnouncement> {
        let sessions = self.sessions.read().await;
        sessions.values()
            .filter(|s| s.is_joinable())
            .cloned()
            .collect()
    }

    /// Find a session by invite code (searches all known sessions)
    pub async fn find_by_invite_code(&self, invite_code: &str) -> Option<FederatedSessionAnnouncement> {
        let sessions = self.sessions.read().await;
        sessions.values()
            .find(|s| s.invite_code.eq_ignore_ascii_case(invite_code))
            .cloned()
    }

    /// Publish an annotation to federation
    pub async fn publish_annotation(&self, annotation: &Annotation, session_id: &str) {
        let federated = FederatedAnnotation::from_annotation(
            annotation,
            session_id,
            &self.node_id,
        );

        let peers = self.peers.read().await;

        for (_, peer_address) in peers.iter() {
            let url = format!("{}/api/collab/federation/annotations", peer_address);
            let _ = self.client
                .post(&url)
                .json(&federated)
                .send()
                .await;
        }
    }

    /// Sync annotations from a peer for a session
    pub async fn sync_annotations(&self, session_id: &str) -> Result<Vec<FederatedAnnotation>, CollabError> {
        let peers = self.peers.read().await;
        let mut annotations = Vec::new();

        for (_, peer_address) in peers.iter() {
            let url = format!(
                "{}/api/collab/federation/annotations?session_id={}",
                peer_address, session_id
            );

            match self.client.get(&url).send().await {
                Ok(response) => {
                    if let Ok(peer_annotations) = response.json::<Vec<FederatedAnnotation>>().await {
                        annotations.extend(peer_annotations);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to sync annotations from {}: {}", peer_address, e);
                }
            }
        }

        Ok(annotations)
    }

    /// Get relay info for joining a remote session
    pub async fn get_relay_info(&self, session_id: &str) -> Option<SessionRelayInfo> {
        let sessions = self.sessions.read().await;

        sessions.get(session_id).map(|s| SessionRelayInfo {
            session_id: s.session_id.clone(),
            websocket_url: s.websocket_url(),
            host_node_id: s.host_node_id.clone(),
            invite_code: s.invite_code.clone(),
        })
    }
}

/// Information needed to connect to a remote session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionRelayInfo {
    pub session_id: String,
    pub websocket_url: String,
    pub host_node_id: String,
    pub invite_code: String,
}

/// Builder for federated session config
pub struct FederatedSessionBuilder {
    config: SessionConfig,
    federated: bool,
    tags: Vec<String>,
    description: Option<String>,
}

impl FederatedSessionBuilder {
    pub fn new() -> Self {
        Self {
            config: SessionConfig::default(),
            federated: false,
            tags: Vec::new(),
            description: None,
        }
    }

    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.config.name = name.into();
        self
    }

    pub fn wallet(mut self, wallet: impl Into<String>) -> Self {
        self.config.target_wallet = Some(wallet.into());
        self
    }

    pub fn federated(mut self, federated: bool) -> Self {
        self.federated = federated;
        self
    }

    pub fn tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    pub fn password(mut self, password: impl Into<String>) -> Self {
        self.config.password = Some(password.into());
        self
    }

    pub fn max_participants(mut self, max: usize) -> Self {
        self.config.max_participants = max;
        self
    }

    pub fn build(self) -> (SessionConfig, bool, Vec<String>, Option<String>) {
        (self.config, self.federated, self.tags, self.description)
    }
}

impl Default for FederatedSessionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_announcement_websocket_url() {
        let announcement = FederatedSessionAnnouncement {
            session_id: "test123".to_string(),
            name: "Test".to_string(),
            invite_code: "ABC123".to_string(),
            host_node_id: "!abcd1234".to_string(),
            host_address: "http://192.168.1.100:8080".to_string(),
            target_wallet: None,
            description: None,
            participant_count: 1,
            max_participants: 10,
            password_protected: false,
            created_at: Utc::now(),
            last_activity: Utc::now(),
            tags: vec![],
            status: FederatedSessionStatus::Active,
        };

        assert_eq!(
            announcement.websocket_url(),
            "ws://192.168.1.100:8080/collab/test123"
        );
    }

    #[test]
    fn test_session_joinable() {
        let mut announcement = FederatedSessionAnnouncement {
            session_id: "test".to_string(),
            name: "Test".to_string(),
            invite_code: "ABC123".to_string(),
            host_node_id: "!abcd".to_string(),
            host_address: "http://localhost:8080".to_string(),
            target_wallet: None,
            description: None,
            participant_count: 5,
            max_participants: 10,
            password_protected: false,
            created_at: Utc::now(),
            last_activity: Utc::now(),
            tags: vec![],
            status: FederatedSessionStatus::Active,
        };

        assert!(announcement.is_joinable());

        announcement.participant_count = 10;
        assert!(!announcement.is_joinable());

        announcement.participant_count = 5;
        announcement.status = FederatedSessionStatus::Ended;
        assert!(!announcement.is_joinable());
    }

    #[test]
    fn test_federated_annotation_roundtrip() {
        let annotation = Annotation::new(
            AnnotationType::Wallet("5Q544fKr...".to_string()),
            "Test annotation".to_string(),
            AnnotationSeverity::Warning,
            "user1".to_string(),
            "Alice".to_string(),
            ParticipantColor::from_index(0),
        );

        let federated = FederatedAnnotation::from_annotation(&annotation, "session1", "!node1");
        let recovered = federated.to_annotation();

        assert_eq!(recovered.text, annotation.text);
        assert_eq!(recovered.severity, annotation.severity);
    }

    #[tokio::test]
    async fn test_federation_manager() {
        // Use a temp directory for the test state file
        let temp_dir = std::env::temp_dir().join(format!("osvm_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&temp_dir);
        let state_path = temp_dir.join("federation.json");

        let manager = CollabFederationManager::new_with_path(
            "!test1234",
            "http://localhost:8080",
            state_path.clone(),
        );

        assert_eq!(manager.node_id(), "!test1234");

        manager.add_peer("!peer1", "http://peer1:8080").await.expect("add peer 1");
        manager.add_peer("!peer2", "http://peer2:8080").await.expect("add peer 2");

        let peers = manager.list_peers().await;
        assert_eq!(peers.len(), 2);

        manager.remove_peer("!peer1").await.expect("remove peer 1");
        let peers = manager.list_peers().await;
        assert_eq!(peers.len(), 1);

        // Verify state file was created
        assert!(state_path.exists(), "State file should exist after peer operations");

        // Cleanup
        let _ = std::fs::remove_file(&state_path);
        let _ = std::fs::remove_dir(&temp_dir);
    }

    #[tokio::test]
    async fn test_federation_persistence() {
        // Test that peers persist across manager instances
        let temp_dir = std::env::temp_dir().join(format!("osvm_persist_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&temp_dir);
        let state_path = temp_dir.join("federation.json");

        // First instance: add peers
        {
            let manager = CollabFederationManager::new_with_path(
                "!node1",
                "http://localhost:8080",
                state_path.clone(),
            );
            manager.add_peer("!peer1", "http://peer1:8080").await.expect("add peer");
            manager.add_peer("!peer2", "http://peer2:8080").await.expect("add peer");
        }

        // Second instance: peers should still be there
        {
            let manager = CollabFederationManager::new_with_path(
                "!node1",  // Same node
                "http://localhost:8080",
                state_path.clone(),
            );
            let peers = manager.list_peers().await;
            assert_eq!(peers.len(), 2, "Peers should persist across manager instances");
        }

        // Cleanup
        let _ = std::fs::remove_file(&state_path);
        let _ = std::fs::remove_dir(&temp_dir);
    }
}
