//! Federation and Peer Discovery for BBS
//!
//! Enables multiple BBS nodes to discover each other and sync messages.
//!
//! Discovery Methods:
//! 1. Manual Peers - Explicitly add peer addresses
//! 2. Local Network (mDNS) - Auto-discover on LAN
//! 3. Bootstrap Server - Query known servers for peer list
//! 4. Gossip - Peers share their peer lists
//!
//! Sync Protocol:
//! - Each message has a unique ID (origin_node + timestamp)
//! - Peers periodically pull new messages from each other
//! - Conflict resolution: earliest timestamp wins

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Default bootstrap servers (can be overridden)
pub const DEFAULT_BOOTSTRAP_SERVERS: &[&str] = &[
    // Add community bootstrap servers here
    // "https://bbs-bootstrap.example.com",
];

/// Peer status
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PeerStatus {
    Unknown,
    Online,
    Offline,
    Syncing,
    Error(String),
}

/// A known peer node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Peer {
    /// Unique node ID (hex format like !abcd1234)
    pub node_id: String,
    /// Display name
    pub name: Option<String>,
    /// HTTP API address (e.g., "http://192.168.1.100:8080")
    pub address: String,
    /// Current status
    pub status: PeerStatus,
    /// Last successful sync timestamp
    pub last_sync: Option<u64>,
    /// Last seen timestamp
    pub last_seen: Option<u64>,
    /// Number of sync failures
    pub failure_count: u32,
    /// Is this a bootstrap server?
    pub is_bootstrap: bool,
}

impl Peer {
    pub fn new(address: &str) -> Self {
        // Generate node ID from address hash
        let node_id = format!("!{:08x}", crc32fast::hash(address.as_bytes()));
        Self {
            node_id,
            name: None,
            address: address.to_string(),
            status: PeerStatus::Unknown,
            last_sync: None,
            last_seen: None,
            failure_count: 0,
            is_bootstrap: false,
        }
    }

    pub fn bootstrap(address: &str) -> Self {
        let mut peer = Self::new(address);
        peer.is_bootstrap = true;
        peer
    }
}

/// Message for federation sync
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FederatedMessage {
    /// Origin node ID
    pub origin_node: String,
    /// Unique message ID (origin_node + local_id)
    pub message_id: String,
    /// Board name (uppercase)
    pub board: String,
    /// Author node ID
    pub author_node: String,
    /// Author display name
    pub author_name: String,
    /// Message body
    pub body: String,
    /// Unix timestamp (seconds)
    pub timestamp: u64,
    /// Parent message ID (for replies)
    pub parent_id: Option<String>,
    /// Signature (optional, for verification)
    pub signature: Option<String>,
}

/// Sync request/response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyncRequest {
    /// Our node ID
    pub from_node: String,
    /// Messages since this timestamp
    pub since_timestamp: u64,
    /// Board filter (None = all boards)
    pub board: Option<String>,
    /// Max messages to return
    pub limit: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SyncResponse {
    /// Responding node ID
    pub node_id: String,
    /// Node's current timestamp
    pub timestamp: u64,
    /// Messages since requested timestamp
    pub messages: Vec<FederatedMessage>,
    /// Known peers (for gossip)
    pub peers: Vec<String>,
}

/// Federation manager
pub struct FederationManager {
    /// Our node ID
    pub node_id: String,
    /// Known peers
    peers: Arc<RwLock<HashMap<String, Peer>>>,
    /// HTTP client for peer communication
    client: reqwest::Client,
    /// Sync interval
    sync_interval: Duration,
}

impl FederationManager {
    /// Create new federation manager
    pub fn new(node_id: &str) -> Self {
        Self {
            node_id: node_id.to_string(),
            peers: Arc::new(RwLock::new(HashMap::new())),
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(10))
                .build()
                .unwrap_or_default(),
            sync_interval: Duration::from_secs(60),
        }
    }

    /// Add a peer manually
    pub async fn add_peer(&self, address: &str) -> Result<Peer, String> {
        let peer = Peer::new(address);
        let node_id = peer.node_id.clone();

        // Try to ping the peer
        let mut peer = peer;
        if self.ping_peer(&mut peer).await {
            peer.status = PeerStatus::Online;
        }

        self.peers.write().await.insert(node_id.clone(), peer.clone());
        Ok(peer)
    }

    /// Remove a peer
    pub async fn remove_peer(&self, node_id: &str) -> bool {
        self.peers.write().await.remove(node_id).is_some()
    }

    /// List all peers
    pub async fn list_peers(&self) -> Vec<Peer> {
        self.peers.read().await.values().cloned().collect()
    }

    /// Get online peers only
    pub async fn online_peers(&self) -> Vec<Peer> {
        self.peers
            .read()
            .await
            .values()
            .filter(|p| p.status == PeerStatus::Online)
            .cloned()
            .collect()
    }

    /// Ping a peer to check if it's online
    pub async fn ping_peer(&self, peer: &mut Peer) -> bool {
        let url = format!("{}/api/stats", peer.address);
        match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => {
                peer.status = PeerStatus::Online;
                peer.last_seen = Some(now_timestamp());
                peer.failure_count = 0;
                true
            }
            Ok(_) => {
                peer.status = PeerStatus::Offline;
                peer.failure_count += 1;
                false
            }
            Err(e) => {
                peer.status = PeerStatus::Error(e.to_string());
                peer.failure_count += 1;
                false
            }
        }
    }

    /// Sync messages from a peer
    pub async fn sync_from_peer(
        &self,
        peer: &mut Peer,
        since: u64,
    ) -> Result<Vec<FederatedMessage>, String> {
        peer.status = PeerStatus::Syncing;

        let url = format!("{}/api/federation/sync", peer.address);
        let request = SyncRequest {
            from_node: self.node_id.clone(),
            since_timestamp: since,
            board: None,
            limit: 100,
        };

        match self.client.post(&url).json(&request).send().await {
            Ok(resp) if resp.status().is_success() => {
                match resp.json::<SyncResponse>().await {
                    Ok(sync_resp) => {
                        peer.status = PeerStatus::Online;
                        peer.last_sync = Some(now_timestamp());
                        peer.last_seen = Some(now_timestamp());
                        peer.failure_count = 0;

                        // Process gossip - add any new peers
                        for peer_addr in sync_resp.peers {
                            if !self.peers.read().await.values().any(|p| p.address == peer_addr) {
                                let _ = self.add_peer(&peer_addr).await;
                            }
                        }

                        Ok(sync_resp.messages)
                    }
                    Err(e) => {
                        peer.status = PeerStatus::Error(format!("Parse error: {}", e));
                        peer.failure_count += 1;
                        Err(e.to_string())
                    }
                }
            }
            Ok(resp) => {
                let status = resp.status();
                peer.status = PeerStatus::Error(format!("HTTP {}", status));
                peer.failure_count += 1;
                Err(format!("HTTP error: {}", status))
            }
            Err(e) => {
                peer.status = PeerStatus::Error(e.to_string());
                peer.failure_count += 1;
                Err(e.to_string())
            }
        }
    }

    /// Push a message to all online peers
    pub async fn broadcast_message(&self, message: &FederatedMessage) -> Vec<(String, bool)> {
        let peers = self.online_peers().await;
        let mut results = Vec::new();

        for peer in peers {
            let url = format!("{}/api/federation/receive", peer.address);
            let success = match self.client.post(&url).json(message).send().await {
                Ok(resp) => resp.status().is_success(),
                Err(_) => false,
            };
            results.push((peer.node_id, success));
        }

        results
    }

    /// Discover peers from bootstrap servers
    pub async fn discover_from_bootstrap(&self) -> Vec<Peer> {
        let mut discovered = Vec::new();

        for bootstrap_url in DEFAULT_BOOTSTRAP_SERVERS {
            let url = format!("{}/api/federation/peers", bootstrap_url);
            if let Ok(resp) = self.client.get(&url).send().await {
                if let Ok(peers) = resp.json::<Vec<String>>().await {
                    for addr in peers {
                        if let Ok(peer) = self.add_peer(&addr).await {
                            discovered.push(peer);
                        }
                    }
                }
            }
        }

        discovered
    }

    /// Discover peers on local network using mDNS
    #[cfg(feature = "mdns")]
    pub async fn discover_local(&self) -> Vec<Peer> {
        // mDNS discovery would go here
        // Using service name "_osvm-bbs._tcp.local"
        Vec::new()
    }

    /// Start background sync task
    pub fn start_sync_task(self: Arc<Self>) -> tokio::task::JoinHandle<()> {
        let manager = self;
        tokio::spawn(async move {
            loop {
                // Sync from all peers
                let peers: Vec<_> = manager.peers.read().await.values().cloned().collect();
                for mut peer in peers {
                    let since = peer.last_sync.unwrap_or(0);
                    if let Ok(messages) = manager.sync_from_peer(&mut peer, since).await {
                        // Store messages in local database
                        // This would call into db::posts::create_federated()
                        tracing::info!(
                            "Synced {} messages from {}",
                            messages.len(),
                            peer.node_id
                        );
                    }
                    // Update peer in map
                    manager
                        .peers
                        .write()
                        .await
                        .insert(peer.node_id.clone(), peer);
                }

                tokio::time::sleep(manager.sync_interval).await;
            }
        })
    }
}

/// Get current Unix timestamp
fn now_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

/// Generate a unique message ID
pub fn generate_message_id(origin_node: &str, local_id: i32) -> String {
    format!("{}:{}", origin_node, local_id)
}

/// Parse a message ID into (origin_node, local_id)
pub fn parse_message_id(message_id: &str) -> Option<(String, i32)> {
    let parts: Vec<&str> = message_id.splitn(2, ':').collect();
    if parts.len() == 2 {
        if let Ok(local_id) = parts[1].parse() {
            return Some((parts[0].to_string(), local_id));
        }
    }
    None
}

// ============================================
// API Endpoints for Federation
// ============================================

use axum::{
    extract::State,
    response::Json,
    routing::{get, post},
    Router,
};

/// Federation API state
pub struct FederationState {
    pub manager: Arc<FederationManager>,
}

/// Add federation routes to router
pub fn federation_routes(state: Arc<FederationState>) -> Router {
    Router::new()
        .route("/api/federation/peers", get(list_peers_handler))
        .route("/api/federation/peers", post(add_peer_handler))
        .route("/api/federation/sync", post(sync_handler))
        .route("/api/federation/receive", post(receive_handler))
        .route("/api/federation/announce", post(announce_handler))
        .with_state(state)
}

#[derive(Deserialize)]
struct AddPeerRequest {
    address: String,
}

async fn list_peers_handler(
    State(state): State<Arc<FederationState>>,
) -> Json<Vec<Peer>> {
    Json(state.manager.list_peers().await)
}

async fn add_peer_handler(
    State(state): State<Arc<FederationState>>,
    Json(req): Json<AddPeerRequest>,
) -> Json<serde_json::Value> {
    match state.manager.add_peer(&req.address).await {
        Ok(peer) => Json(serde_json::json!({
            "success": true,
            "peer": peer,
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e,
        })),
    }
}

async fn sync_handler(
    State(state): State<Arc<FederationState>>,
    Json(req): Json<SyncRequest>,
) -> Json<SyncResponse> {
    // Get messages from local database since timestamp
    // This would query db::posts with timestamp filter

    let peers: Vec<String> = state
        .manager
        .list_peers()
        .await
        .iter()
        .map(|p| p.address.clone())
        .collect();

    Json(SyncResponse {
        node_id: state.manager.node_id.clone(),
        timestamp: now_timestamp(),
        messages: Vec::new(), // TODO: Query from database
        peers,
    })
}

async fn receive_handler(
    State(_state): State<Arc<FederationState>>,
    Json(message): Json<FederatedMessage>,
) -> Json<serde_json::Value> {
    // Store received message in local database
    // This would call db::posts::create_federated()

    Json(serde_json::json!({
        "success": true,
        "message_id": message.message_id,
    }))
}

#[derive(Deserialize)]
struct AnnounceRequest {
    node_id: String,
    address: String,
    name: Option<String>,
}

async fn announce_handler(
    State(state): State<Arc<FederationState>>,
    Json(req): Json<AnnounceRequest>,
) -> Json<serde_json::Value> {
    // A peer is announcing itself
    match state.manager.add_peer(&req.address).await {
        Ok(_) => Json(serde_json::json!({
            "success": true,
            "your_node_id": req.node_id,
            "our_node_id": state.manager.node_id,
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e,
        })),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_id() {
        let id = generate_message_id("!abcd1234", 42);
        assert_eq!(id, "!abcd1234:42");

        let (node, local) = parse_message_id(&id).unwrap();
        assert_eq!(node, "!abcd1234");
        assert_eq!(local, 42);
    }

    #[test]
    fn test_peer_creation() {
        let peer = Peer::new("http://192.168.1.100:8080");
        assert!(peer.node_id.starts_with('!'));
        assert_eq!(peer.status, PeerStatus::Unknown);
        assert!(!peer.is_bootstrap);

        let bootstrap = Peer::bootstrap("https://bootstrap.example.com");
        assert!(bootstrap.is_bootstrap);
    }
}
