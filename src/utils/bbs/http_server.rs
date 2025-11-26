//! HTTP API Server for BBS
//!
//! Provides REST API and WebSocket endpoints for internet-based BBS access.
//! Use this when Meshtastic radio is not available.
//!
//! Endpoints:
//! - GET  /api/boards              - List all boards
//! - GET  /api/boards/:name        - Get board info
//! - POST /api/boards              - Create board
//! - DELETE /api/boards/:name      - Delete board
//! - GET  /api/boards/:name/posts  - List posts in board
//! - POST /api/boards/:name/posts  - Create post
//! - POST /api/posts/:id/reply     - Reply to post
//! - GET  /api/stats               - Get BBS statistics
//! - WS   /ws                      - WebSocket for real-time updates

use axum::{
    extract::{Path, State, WebSocketUpgrade, ws::{Message, WebSocket}},
    http::StatusCode,
    response::{IntoResponse, Json},
    routing::{get, post, delete},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};
use tower_http::cors::{Any, CorsLayer};

use crate::utils::bbs::db;
use crate::utils::bbs::federation::{FederationManager, Peer, SyncRequest, SyncResponse, FederatedMessage};

/// Server state shared across handlers
pub struct ServerState {
    /// Broadcast channel for real-time updates
    pub tx: broadcast::Sender<BroadcastEvent>,
    /// Federation manager for peer discovery/sync
    pub federation: Arc<FederationManager>,
    /// Our node ID
    pub node_id: String,
}

/// Events broadcast to WebSocket clients
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "type")]
pub enum BroadcastEvent {
    NewPost {
        board: String,
        post_id: i32,
        user: String,
        body: String,
        parent_id: Option<i32>,
    },
    BoardCreated {
        name: String,
        description: String,
    },
    BoardDeleted {
        name: String,
    },
}

/// API response wrapper
#[derive(Serialize)]
struct ApiResponse<T> {
    success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<T>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

impl<T: Serialize> ApiResponse<T> {
    fn ok(data: T) -> Json<Self> {
        Json(Self {
            success: true,
            data: Some(data),
            error: None,
        })
    }
}

impl ApiResponse<()> {
    fn err<S: Into<String>>(msg: S) -> Json<Self> {
        Json(Self {
            success: false,
            data: None,
            error: Some(msg.into()),
        })
    }
}

/// Helper to return typed error responses
fn api_err<T, S: Into<String>>(msg: S) -> Json<ApiResponse<T>> {
    Json(ApiResponse {
        success: false,
        data: None,
        error: Some(msg.into()),
    })
}

// ============================================
// Request/Response types
// ============================================

#[derive(Deserialize)]
pub struct CreateBoardRequest {
    name: String,
    description: String,
}

#[derive(Deserialize)]
pub struct CreatePostRequest {
    message: String,
    #[serde(default)]
    user_node_id: Option<String>,
}

#[derive(Deserialize)]
pub struct ReplyRequest {
    message: String,
    #[serde(default)]
    user_node_id: Option<String>,
}

#[derive(Serialize)]
pub struct BoardInfo {
    id: i32,
    name: String,
    description: String,
    post_count: i64,
    creator_id: Option<i32>,
}

#[derive(Serialize)]
pub struct PostInfo {
    id: i32,
    board_id: i32,
    user_id: i32,
    user_name: String,
    body: String,
    created_at: i64,
    parent_id: Option<i32>,
    reply_count: i64,
}

#[derive(Serialize)]
pub struct StatsInfo {
    boards: i64,
    posts: i64,
    users_total: i64,
    users_active: i64,
    server_mode: String,
}

// ============================================
// Handlers
// ============================================

/// List all boards
async fn list_boards() -> impl IntoResponse {
    let conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };
    let mut conn = conn;

    match db::boards::list(&mut conn) {
        Ok(boards) => {
            let infos: Vec<BoardInfo> = boards
                .iter()
                .map(|b| {
                    let post_count = db::posts::list_for_board(&mut conn, b.id, 10000)
                        .map(|p| p.len() as i64)
                        .unwrap_or(0);
                    BoardInfo {
                        id: b.id,
                        name: b.name.clone(),
                        description: b.description.clone(),
                        post_count,
                        creator_id: b.creator_id,
                    }
                })
                .collect();
            ApiResponse::ok(infos)
        }
        Err(e) => api_err(format!("Failed to list boards: {}", e)),
    }
}

/// Get board by name
async fn get_board(Path(name): Path<String>) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    match db::boards::get_by_name(&mut conn, &name.to_uppercase()) {
        Ok(b) => {
            let post_count = db::posts::list_for_board(&mut conn, b.id, 10000)
                .map(|p| p.len() as i64)
                .unwrap_or(0);
            ApiResponse::ok(BoardInfo {
                id: b.id,
                name: b.name,
                description: b.description,
                post_count,
                creator_id: b.creator_id,
            })
        }
        Err(e) => api_err(format!("Board not found: {}", e)),
    }
}

/// Create a new board
async fn create_board(
    State(state): State<Arc<ServerState>>,
    Json(req): Json<CreateBoardRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("DB error: {}", e))),
    };

    let name = req.name.to_uppercase();
    match db::boards::create(&mut conn, &name, &req.description) {
        Ok(b) => {
            // Broadcast event
            let _ = state.tx.send(BroadcastEvent::BoardCreated {
                name: b.name.clone(),
                description: b.description.clone(),
            });

            (StatusCode::CREATED, ApiResponse::ok(BoardInfo {
                id: b.id,
                name: b.name,
                description: b.description,
                post_count: 0,
                creator_id: b.creator_id,
            }))
        }
        Err(e) => (StatusCode::BAD_REQUEST, api_err(format!("Failed to create board: {}", e))),
    }
}

/// Delete a board
async fn delete_board(
    State(state): State<Arc<ServerState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    let name = name.to_uppercase();

    // Get board first
    let board = match db::boards::get_by_name(&mut conn, &name) {
        Ok(b) => b,
        Err(e) => return api_err(format!("Board not found: {}", e)),
    };

    // For API, we allow deletion if board has no creator (legacy) or use system user
    // In production, you'd check auth tokens
    let system_user_id = board.creator_id.unwrap_or(1);

    match db::boards::delete(&mut conn, board.id, system_user_id) {
        Ok(true) => {
            let _ = state.tx.send(BroadcastEvent::BoardDeleted { name: name.clone() });
            ApiResponse::ok(())
        }
        Ok(false) => api_err("Permission denied"),
        Err(e) => api_err(format!("Failed to delete: {}", e)),
    }
}

/// List posts in a board
async fn list_posts(Path(name): Path<String>) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    let board = match db::boards::get_by_name(&mut conn, &name.to_uppercase()) {
        Ok(b) => b,
        Err(e) => return api_err(format!("Board not found: {}", e)),
    };

    match db::posts::list_for_board(&mut conn, board.id, 100) {
        Ok(posts) => {
            let infos: Vec<PostInfo> = posts
                .iter()
                .map(|p| {
                    let user_name = db::users::get_by_user_id(&mut conn, p.user_id)
                        .map(|u| u.short_name)
                        .unwrap_or_else(|_| "???".to_string());
                    let reply_count = db::posts::reply_count(&mut conn, p.id);
                    PostInfo {
                        id: p.id,
                        board_id: p.board_id,
                        user_id: p.user_id,
                        user_name,
                        body: p.body.clone(),
                        created_at: p.created_at_us / 1_000_000,
                        parent_id: p.parent_id,
                        reply_count,
                    }
                })
                .collect();
            ApiResponse::ok(infos)
        }
        Err(e) => api_err(format!("Failed to list posts: {}", e)),
    }
}

/// Create a post in a board
async fn create_post(
    State(state): State<Arc<ServerState>>,
    Path(name): Path<String>,
    Json(req): Json<CreatePostRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("DB error: {}", e))),
    };

    let board = match db::boards::get_by_name(&mut conn, &name.to_uppercase()) {
        Ok(b) => b,
        Err(e) => return (StatusCode::NOT_FOUND, api_err(format!("Board not found: {}", e))),
    };

    // Get or create user
    let node_id = req.user_node_id.as_deref().unwrap_or("!apiuser1");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API User"), ts) {
        Ok(u) => u,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("User error: {}", e))),
    };

    match db::posts::create(&mut conn, board.id, user.id, &req.message) {
        Ok(p) => {
            let reply_count = db::posts::reply_count(&mut conn, p.id);

            // Broadcast to local WebSocket clients
            let _ = state.tx.send(BroadcastEvent::NewPost {
                board: board.name.clone(),
                post_id: p.id,
                user: user.short_name.clone(),
                body: p.body.clone(),
                parent_id: p.parent_id,
            });

            // Push to federation peers immediately (non-blocking)
            let fed_message = FederatedMessage {
                origin_node: state.node_id.clone(),
                message_id: format!("{}:{}", state.node_id, p.id),
                board: board.name.clone(),
                author_node: node_id.to_string(),
                author_name: user.short_name.clone(),
                body: p.body.clone(),
                timestamp: (p.created_at_us / 1_000_000) as u64,
                parent_id: p.parent_id.map(|pid| format!("{}:{}", state.node_id, pid)),
                signature: None,
            };
            let federation = state.federation.clone();
            tokio::spawn(async move {
                let results = federation.broadcast_message(&fed_message).await;
                let success_count = results.iter().filter(|(_, ok)| *ok).count();
                if !results.is_empty() {
                    tracing::info!(
                        "Pushed message {} to {} peers ({} successful)",
                        fed_message.message_id,
                        results.len(),
                        success_count
                    );
                }
            });

            (StatusCode::CREATED, ApiResponse::ok(PostInfo {
                id: p.id,
                board_id: p.board_id,
                user_id: p.user_id,
                user_name: user.short_name,
                body: p.body,
                created_at: p.created_at_us / 1_000_000,
                parent_id: p.parent_id,
                reply_count,
            }))
        }
        Err(e) => (StatusCode::BAD_REQUEST, api_err(format!("Failed to create post: {}", e))),
    }
}

/// Reply to a post
async fn reply_to_post(
    State(state): State<Arc<ServerState>>,
    Path(post_id): Path<i32>,
    Json(req): Json<ReplyRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("DB error: {}", e))),
    };

    // Get parent post
    let parent = match db::posts::get(&mut conn, post_id) {
        Ok(p) => p,
        Err(e) => return (StatusCode::NOT_FOUND, api_err(format!("Post not found: {}", e))),
    };

    let board = match db::boards::get(&mut conn, parent.board_id) {
        Ok(b) => b,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("Board error: {}", e))),
    };

    // Get or create user
    let node_id = req.user_node_id.as_deref().unwrap_or("!apiuser1");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API User"), ts) {
        Ok(u) => u,
        Err(e) => return (StatusCode::INTERNAL_SERVER_ERROR, api_err(format!("User error: {}", e))),
    };

    match db::posts::reply(&mut conn, post_id, user.id, &req.message) {
        Ok(p) => {
            let reply_count = db::posts::reply_count(&mut conn, p.id);

            // Broadcast to local WebSocket clients
            let _ = state.tx.send(BroadcastEvent::NewPost {
                board: board.name.clone(),
                post_id: p.id,
                user: user.short_name.clone(),
                body: p.body.clone(),
                parent_id: p.parent_id,
            });

            // Push to federation peers immediately (non-blocking)
            let fed_message = FederatedMessage {
                origin_node: state.node_id.clone(),
                message_id: format!("{}:{}", state.node_id, p.id),
                board: board.name.clone(),
                author_node: node_id.to_string(),
                author_name: user.short_name.clone(),
                body: p.body.clone(),
                timestamp: (p.created_at_us / 1_000_000) as u64,
                parent_id: p.parent_id.map(|pid| format!("{}:{}", state.node_id, pid)),
                signature: None,
            };
            let federation = state.federation.clone();
            tokio::spawn(async move {
                let results = federation.broadcast_message(&fed_message).await;
                let success_count = results.iter().filter(|(_, ok)| *ok).count();
                if !results.is_empty() {
                    tracing::info!(
                        "Pushed reply {} to {} peers ({} successful)",
                        fed_message.message_id,
                        results.len(),
                        success_count
                    );
                }
            });

            (StatusCode::CREATED, ApiResponse::ok(PostInfo {
                id: p.id,
                board_id: p.board_id,
                user_id: p.user_id,
                user_name: user.short_name,
                body: p.body,
                created_at: p.created_at_us / 1_000_000,
                parent_id: p.parent_id,
                reply_count,
            }))
        }
        Err(e) => (StatusCode::BAD_REQUEST, api_err(format!("Failed to reply: {}", e))),
    }
}

/// Get BBS statistics
async fn get_stats() -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    let boards = db::boards::count(&mut conn);
    let posts = db::posts::count(&mut conn);
    let (users_total, users_active) = db::users::counts(&mut conn);

    ApiResponse::ok(StatsInfo {
        boards,
        posts,
        users_total,
        users_active,
        server_mode: "http".to_string(),
    })
}

/// WebSocket handler for real-time updates
async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(state): State<Arc<ServerState>>,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_websocket(socket, state))
}

async fn handle_websocket(mut socket: WebSocket, state: Arc<ServerState>) {
    let mut rx = state.tx.subscribe();

    // Send welcome message
    let welcome = serde_json::json!({
        "type": "connected",
        "message": "Connected to BBS WebSocket"
    });
    let _ = socket.send(Message::Text(welcome.to_string())).await;

    // Forward broadcast events to client
    loop {
        tokio::select! {
            // Receive from broadcast channel
            Ok(event) = rx.recv() => {
                let json = serde_json::to_string(&event).unwrap_or_default();
                if socket.send(Message::Text(json)).await.is_err() {
                    break; // Client disconnected
                }
            }
            // Receive from WebSocket (handle pings, close)
            Some(msg) = socket.recv() => {
                match msg {
                    Ok(Message::Ping(data)) => {
                        let _ = socket.send(Message::Pong(data)).await;
                    }
                    Ok(Message::Close(_)) | Err(_) => break,
                    _ => {}
                }
            }
            else => break,
        }
    }
}

// ============================================
// Server startup
// ============================================

// ============================================
// Federation API Handlers
// ============================================

#[derive(Deserialize)]
struct AddPeerRequest {
    address: String,
}

#[derive(Deserialize)]
struct AnnounceRequest {
    node_id: String,
    address: String,
    #[allow(dead_code)]
    name: Option<String>,
}

/// List federation peers
async fn list_federation_peers(
    State(state): State<Arc<ServerState>>,
) -> impl IntoResponse {
    let peers = state.federation.list_peers().await;
    ApiResponse::ok(peers)
}

/// Add a federation peer
async fn add_federation_peer(
    State(state): State<Arc<ServerState>>,
    Json(req): Json<AddPeerRequest>,
) -> impl IntoResponse {
    match state.federation.add_peer(&req.address).await {
        Ok(peer) => ApiResponse::ok(peer),
        Err(e) => api_err(format!("Failed to add peer: {}", e)),
    }
}

/// Handle sync requests from other nodes
async fn federation_sync(
    State(state): State<Arc<ServerState>>,
    Json(req): Json<SyncRequest>,
) -> impl IntoResponse {
    let peers: Vec<String> = state
        .federation
        .list_peers()
        .await
        .iter()
        .map(|p| p.address.clone())
        .collect();

    // Get messages from database since timestamp
    let messages: Vec<FederatedMessage> = {
        let mut conn = match db::establish_connection() {
            Ok(c) => c,
            Err(_) => {
                return api_err("Database error");
            }
        };

        // Get local posts and convert to federated format
        let limit = req.limit.min(100) as i64;
        let since_us = (req.since_timestamp as i64) * 1_000_000; // Convert to microseconds

        // Query local posts from all boards
        let boards = db::boards::list(&mut conn).unwrap_or_default();
        let mut all_messages = Vec::new();

        for board in boards {
            if let Ok(posts) = db::posts::list_for_board(&mut conn, board.id, limit) {
                for post in posts {
                    if post.created_at_us > since_us {
                        let author_name = db::users::get_by_user_id(&mut conn, post.user_id)
                            .map(|u| u.short_name)
                            .unwrap_or_else(|_| "???".to_string());
                        let author_node = db::users::get_by_user_id(&mut conn, post.user_id)
                            .map(|u| u.node_id)
                            .unwrap_or_else(|_| "!unknown".to_string());

                        all_messages.push(FederatedMessage {
                            origin_node: state.node_id.clone(),
                            message_id: format!("{}:{}", state.node_id, post.id),
                            board: board.name.clone(),
                            author_node,
                            author_name,
                            body: post.body,
                            timestamp: (post.created_at_us / 1_000_000) as u64,
                            parent_id: post.parent_id.map(|pid| format!("{}:{}", state.node_id, pid)),
                            signature: None,
                        });
                    }
                }
            }
        }

        // Also include received federated messages
        if let Ok(fed_msgs) = db::federated::get_messages_since(&mut conn, req.since_timestamp as i64, limit) {
            for msg in fed_msgs {
                all_messages.push(FederatedMessage {
                    origin_node: msg.origin_node,
                    message_id: msg.message_id,
                    board: msg.board,
                    author_node: msg.author_node,
                    author_name: msg.author_name,
                    body: msg.body,
                    timestamp: msg.created_at as u64,
                    parent_id: msg.parent_id,
                    signature: msg.signature,
                });
            }
        }

        all_messages
    };

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    ApiResponse::ok(SyncResponse {
        node_id: state.node_id.clone(),
        timestamp: now,
        messages,
        peers,
    })
}

/// Receive federated message from another node
async fn federation_receive(
    State(_state): State<Arc<ServerState>>,
    Json(message): Json<FederatedMessage>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("Database error: {}", e)),
    };

    // Check if message already exists (deduplication)
    if db::federated::message_exists(&mut conn, &message.message_id) {
        return ApiResponse::ok(serde_json::json!({
            "success": true,
            "message_id": message.message_id,
            "status": "already_exists",
        }));
    }

    // Store the message
    match db::federated::store_message(
        &mut conn,
        &message.message_id,
        &message.origin_node,
        &message.board,
        &message.author_node,
        &message.author_name,
        &message.body,
        message.parent_id.as_deref(),
        message.timestamp as i64,
        message.signature.as_deref(),
    ) {
        Ok(stored) => ApiResponse::ok(serde_json::json!({
            "success": true,
            "message_id": stored.message_id,
            "status": "stored",
        })),
        Err(e) => api_err(format!("Failed to store message: {}", e)),
    }
}

/// Handle node announcements
async fn federation_announce(
    State(state): State<Arc<ServerState>>,
    Json(req): Json<AnnounceRequest>,
) -> impl IntoResponse {
    match state.federation.add_peer(&req.address).await {
        Ok(_) => ApiResponse::ok(serde_json::json!({
            "success": true,
            "your_node_id": req.node_id,
            "our_node_id": state.node_id,
        })),
        Err(e) => api_err(format!("Failed to register peer: {}", e)),
    }
}

/// Create the router
pub fn create_router(state: Arc<ServerState>) -> Router {
    Router::new()
        // Board routes
        .route("/api/boards", get(list_boards).post(create_board))
        .route("/api/boards/:name", get(get_board).delete(delete_board))
        .route("/api/boards/:name/posts", get(list_posts).post(create_post))
        // Post routes
        .route("/api/posts/:id/reply", post(reply_to_post))
        // Stats
        .route("/api/stats", get(get_stats))
        // Federation routes
        .route("/api/federation/peers", get(list_federation_peers).post(add_federation_peer))
        .route("/api/federation/sync", post(federation_sync))
        .route("/api/federation/receive", post(federation_receive))
        .route("/api/federation/announce", post(federation_announce))
        // WebSocket
        .route("/ws", get(websocket_handler))
        // CORS for browser access
        .layer(CorsLayer::new()
            .allow_origin(Any)
            .allow_methods(Any)
            .allow_headers(Any))
        .with_state(state)
}

/// Start the HTTP server
pub async fn start_server(host: &str, port: u16) -> Result<(), Box<dyn std::error::Error>> {
    // Initialize database
    let mut conn = db::establish_connection()?;
    db::initialize_database(&mut conn)?;
    db::run_migrations(&mut conn)?;
    drop(conn);

    // Generate node ID from hostname + port
    let node_id = format!("!{:08x}", crc32fast::hash(format!("{}:{}", host, port).as_bytes()));

    // Create broadcast channel
    let (tx, _) = broadcast::channel::<BroadcastEvent>(100);

    // Create federation manager and start background sync
    let federation = Arc::new(FederationManager::new(&node_id));
    let _sync_handle = federation.clone().start_sync_task();

    let state = Arc::new(ServerState {
        tx,
        federation,
        node_id: node_id.clone(),
    });

    let router = create_router(state);
    let addr = format!("{}:{}", host, port);

    println!("🌐 BBS HTTP Server starting on http://{}", addr);
    println!("   Node ID: {}", node_id);
    println!("   REST API: http://{}/api/", addr);
    println!("   WebSocket: ws://{}/ws", addr);
    println!("");
    println!("   Endpoints:");
    println!("   GET  /api/boards              - List boards");
    println!("   GET  /api/boards/:name        - Get board");
    println!("   POST /api/boards              - Create board");
    println!("   GET  /api/boards/:name/posts  - List posts");
    println!("   POST /api/boards/:name/posts  - Create post");
    println!("   POST /api/posts/:id/reply     - Reply to post");
    println!("   GET  /api/stats               - Statistics");
    println!("   WS   /ws                      - Real-time updates");
    println!("");
    println!("   Federation:");
    println!("   GET  /api/federation/peers    - List known peers");
    println!("   POST /api/federation/peers    - Add a peer");
    println!("   POST /api/federation/sync     - Sync messages");
    println!("   POST /api/federation/receive  - Receive message");
    println!("   POST /api/federation/announce - Announce presence");

    let listener = tokio::net::TcpListener::bind(&addr).await?;
    axum::serve(listener, router).await?;

    Ok(())
}
