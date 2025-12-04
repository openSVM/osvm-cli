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
//! - GET  /api/boards/:name/threads - Get threaded view
//! - POST /api/posts/:id/reply     - Reply to post
//! - POST /api/posts/:id/vote      - Vote on post {"vote": 1 or -1}
//! - POST /api/posts/:id/upvote    - Upvote post
//! - POST /api/posts/:id/downvote  - Downvote post
//! - GET  /api/stats               - Get BBS statistics
//! - WS   /ws                      - WebSocket for real-time updates

use axum::{
    extract::{
        ws::{Message, WebSocket},
        Path, State, WebSocketUpgrade,
    },
    http::StatusCode,
    response::{IntoResponse, Json},
    routing::{delete, get, post},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};
use tower_http::cors::{Any, CorsLayer};

use crate::utils::bbs::db;
use crate::utils::bbs::federation::{
    FederatedMessage, FederationManager, Peer, SyncRequest, SyncResponse,
};
use diesel::RunQueryDsl;

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
    /// Score update from upvote/downvote
    ScoreUpdate {
        post_id: i32,
        new_score: i32,
    },
    /// Reply notification for post author
    ReplyNotification {
        original_post_id: String,
        original_author_node: String,
        reply_id: String,
        reply_author: String,
        reply_preview: String,
        board: String,
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

/// Request to reply to any post (local or federated) using unified string ID
#[derive(Deserialize)]
pub struct UnifiedReplyRequest {
    /// Parent post ID - can be local "123" or federated "!abc123:5"
    parent_id: String,
    /// Board name (required for federated parents where we can't look up the board)
    board: String,
    /// Reply message
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

/// Unified post info that includes both local and federated posts
/// Uses string IDs to handle both local (numeric) and federated (node:id) formats
#[derive(Serialize, Clone)]
pub struct UnifiedPostInfo {
    /// Unique identifier: either local ID (as string) or federated "node:id"
    pub id: String,
    /// Board name (uppercase)
    pub board: String,
    /// Author display name
    pub author: String,
    /// Author node ID (for federated posts) or "local" for local posts
    pub author_node: String,
    /// Message content
    pub body: String,
    /// Creation timestamp (Unix seconds)
    pub created_at: i64,
    /// Parent post ID for replies (same format as id)
    pub parent_id: Option<String>,
    /// Whether this is a local or federated post
    pub is_local: bool,
    /// Reply count
    pub reply_count: i64,
    /// Upvote/downvote score (only for local posts, 0 for federated)
    pub score: i32,
}

/// Thread structure for hierarchical display
/// Each top-level post contains its nested replies recursively
#[derive(Serialize, Clone)]
pub struct ThreadedPostInfo {
    /// Post details
    #[serde(flatten)]
    post: UnifiedPostInfo,
    /// Nested replies (recursive)
    replies: Vec<ThreadedPostInfo>,
    /// Thread depth (0 = top-level)
    depth: u32,
    /// Whether thread is collapsed (for UI, based on score)
    collapsed: bool,
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
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("DB error: {}", e)),
            )
        }
    };

    let name = req.name.to_uppercase();
    match db::boards::create(&mut conn, &name, &req.description) {
        Ok(b) => {
            // Broadcast event
            let _ = state.tx.send(BroadcastEvent::BoardCreated {
                name: b.name.clone(),
                description: b.description.clone(),
            });

            (
                StatusCode::CREATED,
                ApiResponse::ok(BoardInfo {
                    id: b.id,
                    name: b.name,
                    description: b.description,
                    post_count: 0,
                    creator_id: b.creator_id,
                }),
            )
        }
        Err(e) => (
            StatusCode::BAD_REQUEST,
            api_err(format!("Failed to create board: {}", e)),
        ),
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
            let _ = state
                .tx
                .send(BroadcastEvent::BoardDeleted { name: name.clone() });
            ApiResponse::ok(())
        }
        Ok(false) => api_err("Permission denied"),
        Err(e) => api_err(format!("Failed to delete: {}", e)),
    }
}

/// List posts in a board (includes both local and federated posts)
async fn list_posts(
    State(state): State<Arc<ServerState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    let board_name = name.to_uppercase();
    let our_node_id = &state.node_id;

    // Get the board (needed for local post queries)
    let board = match db::boards::get_by_name(&mut conn, &board_name) {
        Ok(b) => Some(b),
        Err(_) => None, // Board may not exist locally but have federated posts
    };

    let mut unified_posts: Vec<UnifiedPostInfo> = Vec::new();

    // Add local posts if board exists
    if let Some(ref b) = board {
        if let Ok(posts) = db::posts::list_for_board(&mut conn, b.id, 100) {
            for p in posts {
                let user_name = db::users::get_by_user_id(&mut conn, p.user_id)
                    .map(|u| u.short_name)
                    .unwrap_or_else(|_| "???".to_string());
                let reply_count = db::posts::reply_count(&mut conn, p.id);

                // Use unified_parent_id() which prefers federated_parent_id over local parent_id
                let parent_id = p.unified_parent_id();

                unified_posts.push(UnifiedPostInfo {
                    id: p.id.to_string(),
                    board: board_name.clone(),
                    author: user_name,
                    author_node: "local".to_string(),
                    body: p.body.clone(),
                    created_at: p.created_at_us / 1_000_000,
                    parent_id,
                    is_local: true,
                    reply_count,
                    score: p.score,
                });
            }
        }
    }

    // Add federated posts for this board
    // Use timestamp 0 to get all messages, limit to 100
    if let Ok(fed_posts) = db::federated::get_messages_for_board(&mut conn, &board_name, 0, 100) {
        for fp in fed_posts {
            // Skip federated messages that originated from our own node
            // (we already have them as local posts)
            if fp.origin_node == *our_node_id {
                continue;
            }

            // Count replies to this federated message
            let reply_count = db::federated::reply_count(&mut conn, &fp.message_id);

            unified_posts.push(UnifiedPostInfo {
                id: fp.message_id.clone(),
                board: fp.board.clone(),
                author: fp.author_name.clone(),
                author_node: fp.author_node.clone(),
                body: fp.body.clone(),
                created_at: fp.created_at,
                parent_id: fp.parent_id.clone(),
                is_local: false,
                reply_count,
                score: 0,
            });
        }
    }

    // Sort by created_at descending (newest first)
    unified_posts.sort_by(|a, b| b.created_at.cmp(&a.created_at));

    // Limit to 100 total posts
    unified_posts.truncate(100);

    ApiResponse::ok(unified_posts)
}

/// Threshold below which threads are auto-collapsed
const COLLAPSE_SCORE_THRESHOLD: i32 = -3;

/// List posts organized as threads (hierarchical view)
/// Returns only top-level posts with nested replies
async fn list_threads(
    State(state): State<Arc<ServerState>>,
    Path(name): Path<String>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    let board_name = name.to_uppercase();
    let our_node_id = &state.node_id;

    // Get the board (needed for local post queries)
    let board = match db::boards::get_by_name(&mut conn, &board_name) {
        Ok(b) => Some(b),
        Err(_) => None,
    };

    // Collect all posts first (flat list)
    let mut all_posts: Vec<UnifiedPostInfo> = Vec::new();

    // Add local posts if board exists
    if let Some(ref b) = board {
        if let Ok(posts) = db::posts::list_for_board(&mut conn, b.id, 500) {
            for p in posts {
                let user_name = db::users::get_by_user_id(&mut conn, p.user_id)
                    .map(|u| u.short_name)
                    .unwrap_or_else(|_| "???".to_string());
                let reply_count = db::posts::reply_count(&mut conn, p.id);
                let parent_id = p.unified_parent_id();

                all_posts.push(UnifiedPostInfo {
                    id: p.id.to_string(),
                    board: board_name.clone(),
                    author: user_name,
                    author_node: "local".to_string(),
                    body: p.body.clone(),
                    created_at: p.created_at_us / 1_000_000,
                    parent_id,
                    is_local: true,
                    reply_count,
                    score: p.score,
                });
            }
        }
    }

    // Add federated posts
    if let Ok(fed_posts) = db::federated::get_messages_for_board(&mut conn, &board_name, 0, 500) {
        for fp in fed_posts {
            if fp.origin_node == *our_node_id {
                continue;
            }
            let reply_count = db::federated::reply_count(&mut conn, &fp.message_id);

            all_posts.push(UnifiedPostInfo {
                id: fp.message_id.clone(),
                board: fp.board.clone(),
                author: fp.author_name.clone(),
                author_node: fp.author_node.clone(),
                body: fp.body.clone(),
                created_at: fp.created_at,
                parent_id: fp.parent_id.clone(),
                is_local: false,
                reply_count,
                score: 0,
            });
        }
    }

    // Build threads from flat post list
    let threads = build_thread_tree(&all_posts);

    ApiResponse::ok(threads)
}

/// Build a hierarchical thread tree from a flat list of posts
fn build_thread_tree(posts: &[UnifiedPostInfo]) -> Vec<ThreadedPostInfo> {
    use std::collections::HashMap;

    // Create a map of id -> post for quick lookup
    let post_map: HashMap<String, &UnifiedPostInfo> =
        posts.iter().map(|p| (p.id.clone(), p)).collect();

    // Create a map of parent_id -> children
    let mut children_map: HashMap<String, Vec<&UnifiedPostInfo>> = HashMap::new();
    let mut roots: Vec<&UnifiedPostInfo> = Vec::new();

    for post in posts {
        if let Some(ref parent_id) = post.parent_id {
            children_map
                .entry(parent_id.clone())
                .or_default()
                .push(post);
        } else {
            roots.push(post);
        }
    }

    // Sort roots by score (descending) then by created_at (descending)
    roots.sort_by(|a, b| match b.score.cmp(&a.score) {
        std::cmp::Ordering::Equal => b.created_at.cmp(&a.created_at),
        other => other,
    });

    // Recursively build threaded posts
    fn build_thread(
        post: &UnifiedPostInfo,
        children_map: &HashMap<String, Vec<&UnifiedPostInfo>>,
        depth: u32,
    ) -> ThreadedPostInfo {
        let mut replies = Vec::new();

        if let Some(children) = children_map.get(&post.id) {
            // Sort children by created_at ascending (chronological order for replies)
            let mut sorted_children: Vec<_> = children.iter().copied().collect();
            sorted_children.sort_by(|a, b| a.created_at.cmp(&b.created_at));

            for child in sorted_children {
                replies.push(build_thread(child, children_map, depth + 1));
            }
        }

        ThreadedPostInfo {
            post: post.clone(),
            replies,
            depth,
            collapsed: post.score < COLLAPSE_SCORE_THRESHOLD,
        }
    }

    roots
        .into_iter()
        .map(|root| build_thread(root, &children_map, 0))
        .collect()
}

/// Create a post in a board
async fn create_post(
    State(state): State<Arc<ServerState>>,
    Path(name): Path<String>,
    Json(req): Json<CreatePostRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("DB error: {}", e)),
            )
        }
    };

    let board = match db::boards::get_by_name(&mut conn, &name.to_uppercase()) {
        Ok(b) => b,
        Err(e) => {
            return (
                StatusCode::NOT_FOUND,
                api_err(format!("Board not found: {}", e)),
            )
        }
    };

    // Get or create user
    let node_id = req.user_node_id.as_deref().unwrap_or("!apiuser1");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API User"), ts)
    {
        Ok(u) => u,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("User error: {}", e)),
            )
        }
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

            (
                StatusCode::CREATED,
                ApiResponse::ok(PostInfo {
                    id: p.id,
                    board_id: p.board_id,
                    user_id: p.user_id,
                    user_name: user.short_name,
                    body: p.body,
                    created_at: p.created_at_us / 1_000_000,
                    parent_id: p.parent_id,
                    reply_count,
                }),
            )
        }
        Err(e) => (
            StatusCode::BAD_REQUEST,
            api_err(format!("Failed to create post: {}", e)),
        ),
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
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("DB error: {}", e)),
            )
        }
    };

    // Get parent post
    let parent = match db::posts::get(&mut conn, post_id) {
        Ok(p) => p,
        Err(e) => {
            return (
                StatusCode::NOT_FOUND,
                api_err(format!("Post not found: {}", e)),
            )
        }
    };

    let board = match db::boards::get(&mut conn, parent.board_id) {
        Ok(b) => b,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("Board error: {}", e)),
            )
        }
    };

    // Get or create user
    let node_id = req.user_node_id.as_deref().unwrap_or("!apiuser1");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API User"), ts)
    {
        Ok(u) => u,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("User error: {}", e)),
            )
        }
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

            (
                StatusCode::CREATED,
                ApiResponse::ok(PostInfo {
                    id: p.id,
                    board_id: p.board_id,
                    user_id: p.user_id,
                    user_name: user.short_name,
                    body: p.body,
                    created_at: p.created_at_us / 1_000_000,
                    parent_id: p.parent_id,
                    reply_count,
                }),
            )
        }
        Err(e) => (
            StatusCode::BAD_REQUEST,
            api_err(format!("Failed to reply: {}", e)),
        ),
    }
}

/// Vote request with optional user identification
#[derive(Deserialize)]
pub struct VoteRequest {
    #[serde(default)]
    user_node_id: Option<String>,
}

/// Unified vote request with vote type
#[derive(Deserialize)]
pub struct UnifiedVoteRequest {
    /// Vote value: 1 for upvote, -1 for downvote
    vote: i32,
    #[serde(default)]
    user_node_id: Option<String>,
}

/// Unified vote endpoint - POST /api/posts/:id/vote with {"vote": 1} or {"vote": -1}
async fn vote_unified(
    State(state): State<Arc<ServerState>>,
    Path(post_id): Path<i32>,
    Json(req): Json<UnifiedVoteRequest>,
) -> impl IntoResponse {
    // Normalize vote value: any positive = upvote, any negative = downvote
    let vote_type = if req.vote > 0 {
        1
    } else if req.vote < 0 {
        -1
    } else {
        1
    };
    vote_post(state, post_id, vote_type, req.user_node_id).await
}

/// Upvote a post (+1 score)
async fn upvote_post(
    State(state): State<Arc<ServerState>>,
    Path(post_id): Path<i32>,
    body: Option<Json<VoteRequest>>,
) -> impl IntoResponse {
    let user_node_id = body.and_then(|b| b.user_node_id.clone());
    vote_post(state, post_id, 1, user_node_id).await
}

/// Downvote a post (-1 score)
async fn downvote_post(
    State(state): State<Arc<ServerState>>,
    Path(post_id): Path<i32>,
    body: Option<Json<VoteRequest>>,
) -> impl IntoResponse {
    let user_node_id = body.and_then(|b| b.user_node_id.clone());
    vote_post(state, post_id, -1, user_node_id).await
}

/// Common vote handler with user tracking
async fn vote_post(
    state: Arc<ServerState>,
    post_id: i32,
    vote_type: i32,
    user_node_id: Option<String>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("DB error: {}", e)),
    };

    // Get or create user
    let node_id = user_node_id.as_deref().unwrap_or("!apivoter");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API Voter"), ts)
    {
        Ok(u) => u,
        Err(e) => return api_err(format!("User error: {}", e)),
    };

    // Cast vote with user tracking
    match db::votes::cast_vote(&mut conn, user.id, post_id, vote_type) {
        Ok(result) => {
            use db::votes::VoteResult;
            let (new_score, action) = match result {
                VoteResult::Voted { new_score } => (new_score, "voted"),
                VoteResult::Changed {
                    new_score,
                    old_vote: _,
                } => (new_score, "changed"),
                VoteResult::Removed { new_score } => (new_score, "removed"),
            };

            // Broadcast score update to WebSocket clients
            let _ = state
                .tx
                .send(BroadcastEvent::ScoreUpdate { post_id, new_score });

            ApiResponse::ok(serde_json::json!({
                "post_id": post_id,
                "new_score": new_score,
                "vote_type": vote_type,
                "action": action,
            }))
        }
        Err(e) => api_err(format!("Failed to cast vote: {}", e)),
    }
}

/// Reply to any post (local or federated) using unified string ID
///
/// This endpoint allows replying to federated posts from other nodes.
/// The parent_id can be:
/// - A local numeric ID like "123"
/// - A federated ID like "!abc123:5" (node_id:local_id)
async fn reply_unified(
    State(state): State<Arc<ServerState>>,
    Json(req): Json<UnifiedReplyRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("DB error: {}", e)),
            )
        }
    };

    let board_name = req.board.to_uppercase();

    // Get or create the board
    let board = match db::boards::get_by_name(&mut conn, &board_name) {
        Ok(b) => b,
        Err(_) => {
            // Board doesn't exist locally - this is an error
            return (
                StatusCode::NOT_FOUND,
                api_err(format!("Board '{}' not found", board_name)),
            );
        }
    };

    // Validate parent_id exists and get author info for notification
    let (parent_exists, parent_author_node) = if req.parent_id.starts_with('!') {
        // Federated parent - check federated_messages table
        let exists = db::federated::message_exists(&mut conn, &req.parent_id);
        let author_node = if exists {
            db::federated::get_message(&mut conn, &req.parent_id)
                .map(|m| m.author_node)
                .ok()
        } else {
            None
        };
        (exists, author_node)
    } else {
        // Local parent - check posts table
        let maybe_post = req
            .parent_id
            .parse::<i32>()
            .ok()
            .and_then(|id| db::posts::get(&mut conn, id).ok());
        let exists = maybe_post.is_some();
        let author_node = maybe_post.and_then(|p| {
            db::users::get_by_user_id(&mut conn, p.user_id)
                .ok()
                .map(|u| u.node_id)
        });
        (exists, author_node)
    };

    if !parent_exists {
        return (
            StatusCode::NOT_FOUND,
            api_err(format!("Parent post '{}' not found", req.parent_id)),
        );
    }

    // Get or create user
    let node_id = req.user_node_id.as_deref().unwrap_or("!apiuser1");
    let ts = db::now_as_useconds();
    let (user, _) = match db::users::observe(&mut conn, node_id, Some("API"), Some("API User"), ts)
    {
        Ok(u) => u,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                api_err(format!("User error: {}", e)),
            )
        }
    };

    // Determine the parent_id format for federation
    // If it's a local ID, prefix with our node_id; if federated, use as-is
    let federated_parent_id = if req.parent_id.starts_with('!') {
        req.parent_id.clone()
    } else {
        format!("{}:{}", state.node_id, req.parent_id)
    };

    // Create the reply as a new post with proper parent reference
    // If parent is federated (starts with !), use federated_parent_id
    // If parent is local, use parent_id
    let create_result = if req.parent_id.starts_with('!') {
        db::posts::create_with_federated_parent(
            &mut conn,
            board.id,
            user.id,
            &req.message,
            &federated_parent_id,
        )
    } else {
        // Local parent - parse to i32
        let local_parent_id = req.parent_id.parse::<i32>().ok();
        db::posts::create_full(
            &mut conn,
            board.id,
            user.id,
            &req.message,
            local_parent_id,
            Some(&federated_parent_id),
        )
    };

    match create_result {
        Ok(p) => {
            // Broadcast to local WebSocket clients
            let _ = state.tx.send(BroadcastEvent::NewPost {
                board: board.name.clone(),
                post_id: p.id,
                user: user.short_name.clone(),
                body: p.body.clone(),
                parent_id: None, // Parent might be federated
            });

            // Send reply notification to the parent post's author
            if let Some(ref author_node) = parent_author_node {
                let reply_preview = if p.body.len() > 50 {
                    format!("{}...", &p.body[..47])
                } else {
                    p.body.clone()
                };
                let _ = state.tx.send(BroadcastEvent::ReplyNotification {
                    original_post_id: req.parent_id.clone(),
                    original_author_node: author_node.clone(),
                    reply_id: format!("{}:{}", state.node_id, p.id),
                    reply_author: user.short_name.clone(),
                    reply_preview,
                    board: board.name.clone(),
                });
            }

            // Push to federation peers with proper parent reference
            let fed_message = FederatedMessage {
                origin_node: state.node_id.clone(),
                message_id: format!("{}:{}", state.node_id, p.id),
                board: board.name.clone(),
                author_node: node_id.to_string(),
                author_name: user.short_name.clone(),
                body: p.body.clone(),
                timestamp: (p.created_at_us / 1_000_000) as u64,
                parent_id: Some(federated_parent_id.clone()),
                signature: None,
            };
            let federation = state.federation.clone();
            let log_parent_id = federated_parent_id.clone();
            tokio::spawn(async move {
                let results = federation.broadcast_message(&fed_message).await;
                let success_count = results.iter().filter(|(_, ok)| *ok).count();
                if !results.is_empty() {
                    tracing::info!(
                        "Pushed threaded reply {} (parent: {}) to {} peers ({} successful)",
                        fed_message.message_id,
                        log_parent_id,
                        results.len(),
                        success_count
                    );
                }
            });

            (
                StatusCode::CREATED,
                ApiResponse::ok(UnifiedPostInfo {
                    id: format!("{}:{}", state.node_id, p.id),
                    board: board.name,
                    author: user.short_name,
                    author_node: node_id.to_string(),
                    body: p.body,
                    created_at: p.created_at_us / 1_000_000,
                    parent_id: Some(federated_parent_id),
                    is_local: true,
                    reply_count: 0,
                    score: 0,
                }),
            )
        }
        Err(e) => (
            StatusCode::BAD_REQUEST,
            api_err(format!("Failed to create reply: {}", e)),
        ),
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
async fn list_federation_peers(State(state): State<Arc<ServerState>>) -> impl IntoResponse {
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
                            parent_id: post
                                .parent_id
                                .map(|pid| format!("{}:{}", state.node_id, pid)),
                            signature: None,
                        });
                    }
                }
            }
        }

        // Also include received federated messages
        if let Ok(fed_msgs) =
            db::federated::get_messages_since(&mut conn, req.since_timestamp as i64, limit)
        {
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

// ============================================================================
// Agent Reputation System
// ============================================================================

/// Agent reputation data
#[derive(Debug, Serialize, Deserialize)]
pub struct AgentReputation {
    pub agent_name: String,
    pub bids: i32,
    pub wins: i32,
    pub deliveries: i32,
    pub total_revenue: f64,
    pub avg_price: f64,
    pub rating: f64,
    pub last_active: i64,
}

/// Request to update agent reputation
#[derive(Debug, Deserialize)]
pub struct UpdateReputationRequest {
    pub action: String, // "bid", "win", "deliver"
    pub price: Option<f64>,
}

/// List all agents with their reputation
async fn list_agent_reputation() -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("Database error: {}", e)),
    };

    // Query all agents
    match diesel::sql_query(
        "SELECT agent_name, bids, wins, deliveries, total_revenue, avg_price, rating, last_active
         FROM agent_reputation ORDER BY rating DESC, deliveries DESC",
    )
    .load::<AgentReputationRow>(&mut conn)
    {
        Ok(agents) => {
            let result: Vec<AgentReputation> = agents
                .into_iter()
                .map(|a| AgentReputation {
                    agent_name: a.agent_name,
                    bids: a.bids,
                    wins: a.wins,
                    deliveries: a.deliveries,
                    total_revenue: a.total_revenue,
                    avg_price: a.avg_price,
                    rating: a.rating,
                    last_active: a.last_active,
                })
                .collect();
            ApiResponse::ok(serde_json::json!({
                "agents": result,
                "count": result.len()
            }))
        }
        Err(_) => ApiResponse::ok(serde_json::json!({"agents": [], "count": 0})),
    }
}

/// Get specific agent reputation
async fn get_agent_reputation(Path(agent_name): Path<String>) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("Database error: {}", e)),
    };

    match diesel::sql_query(format!(
        "SELECT agent_name, bids, wins, deliveries, total_revenue, avg_price, rating, last_active
         FROM agent_reputation WHERE agent_name = '{}'",
        agent_name.replace("'", "''")
    ))
    .load::<AgentReputationRow>(&mut conn)
    {
        Ok(agents) if !agents.is_empty() => {
            let a = &agents[0];
            ApiResponse::ok(serde_json::json!({
                "agent_name": a.agent_name,
                "bids": a.bids,
                "wins": a.wins,
                "deliveries": a.deliveries,
                "total_revenue": a.total_revenue,
                "avg_price": a.avg_price,
                "rating": a.rating,
                "win_rate": if a.bids > 0 { (a.wins as f64 / a.bids as f64) * 100.0 } else { 0.0 },
                "delivery_rate": if a.wins > 0 { (a.deliveries as f64 / a.wins as f64) * 100.0 } else { 0.0 },
            }))
        }
        _ => api_err(format!("Agent '{}' not found", agent_name)),
    }
}

/// Update agent reputation (record bid, win, or delivery)
async fn update_agent_reputation(
    Path(agent_name): Path<String>,
    Json(req): Json<UpdateReputationRequest>,
) -> impl IntoResponse {
    let mut conn = match db::establish_connection() {
        Ok(c) => c,
        Err(e) => return api_err(format!("Database error: {}", e)),
    };

    let now = db::now_as_useconds();
    let safe_name = agent_name.replace("'", "''");

    // Insert agent if not exists
    let _ = diesel::sql_query(format!(
        "INSERT OR IGNORE INTO agent_reputation (agent_name, last_active, created_at) VALUES ('{}', {}, {})",
        safe_name, now, now
    )).execute(&mut conn);

    // Update based on action
    let result = match req.action.as_str() {
        "bid" => diesel::sql_query(format!(
            "UPDATE agent_reputation SET bids = bids + 1, last_active = {} WHERE agent_name = '{}'",
            now, safe_name
        ))
        .execute(&mut conn),

        "win" => {
            let price = req.price.unwrap_or(0.0);
            diesel::sql_query(format!(
                "UPDATE agent_reputation SET
                    wins = wins + 1,
                    total_revenue = total_revenue + {},
                    avg_price = (total_revenue + {}) / (wins + 1),
                    last_active = {}
                 WHERE agent_name = '{}'",
                price, price, now, safe_name
            ))
            .execute(&mut conn)
        }

        "deliver" => {
            // Delivery increases rating significantly
            diesel::sql_query(format!(
                "UPDATE agent_reputation SET
                    deliveries = deliveries + 1,
                    rating = (deliveries + 1) * 10.0 + (wins * 2.0),
                    last_active = {}
                 WHERE agent_name = '{}'",
                now, safe_name
            ))
            .execute(&mut conn)
        }

        _ => {
            return api_err(format!(
                "Unknown action: {}. Use 'bid', 'win', or 'deliver'",
                req.action
            ))
        }
    };

    match result {
        Ok(_) => ApiResponse::ok(serde_json::json!({
            "success": true,
            "agent": agent_name,
            "action": req.action,
        })),
        Err(e) => api_err(format!("Failed to update reputation: {}", e)),
    }
}

/// Row type for SQL query
#[derive(diesel::QueryableByName, Debug)]
struct AgentReputationRow {
    #[diesel(sql_type = diesel::sql_types::Text)]
    agent_name: String,
    #[diesel(sql_type = diesel::sql_types::Integer)]
    bids: i32,
    #[diesel(sql_type = diesel::sql_types::Integer)]
    wins: i32,
    #[diesel(sql_type = diesel::sql_types::Integer)]
    deliveries: i32,
    #[diesel(sql_type = diesel::sql_types::Double)]
    total_revenue: f64,
    #[diesel(sql_type = diesel::sql_types::Double)]
    avg_price: f64,
    #[diesel(sql_type = diesel::sql_types::Double)]
    rating: f64,
    #[diesel(sql_type = diesel::sql_types::BigInt)]
    last_active: i64,
}

/// Create the router
pub fn create_router(state: Arc<ServerState>) -> Router {
    Router::new()
        // Board routes
        .route("/api/boards", get(list_boards).post(create_board))
        .route("/api/boards/:name", get(get_board).delete(delete_board))
        .route("/api/boards/:name/posts", get(list_posts).post(create_post))
        .route("/api/boards/:name/threads", get(list_threads)) // Hierarchical threaded view
        // Post routes
        .route("/api/posts/:id/reply", post(reply_to_post))
        .route("/api/posts/:id/vote", post(vote_unified)) // Unified vote: {"vote": 1} or {"vote": -1}
        .route("/api/posts/:id/upvote", post(upvote_post))
        .route("/api/posts/:id/downvote", post(downvote_post))
        .route("/api/reply", post(reply_unified)) // Unified reply for both local and federated posts
        // Stats
        .route("/api/stats", get(get_stats))
        // Agent reputation routes
        .route("/api/reputation", get(list_agent_reputation))
        .route(
            "/api/reputation/:agent_name",
            get(get_agent_reputation).post(update_agent_reputation),
        )
        // Federation routes
        .route(
            "/api/federation/peers",
            get(list_federation_peers).post(add_federation_peer),
        )
        .route("/api/federation/sync", post(federation_sync))
        .route("/api/federation/receive", post(federation_receive))
        .route("/api/federation/announce", post(federation_announce))
        // WebSocket
        .route("/ws", get(websocket_handler))
        // CORS for browser access
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        )
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
    let node_id = format!(
        "!{:08x}",
        crc32fast::hash(format!("{}:{}", host, port).as_bytes())
    );

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
    println!("   GET  /api/boards/:name/posts    - List posts (flat)");
    println!("   GET  /api/boards/:name/threads  - List threads (hierarchical)");
    println!("   POST /api/boards/:name/posts    - Create post");
    println!("   POST /api/posts/:id/reply       - Reply to post");
    println!("   POST /api/posts/:id/upvote      - Upvote post (+1)");
    println!("   POST /api/posts/:id/downvote    - Downvote post (-1)");
    println!("   POST /api/reply                 - Unified reply (local or federated)");
    println!("   GET  /api/stats                 - Statistics");
    println!("   WS   /ws                      - Real-time updates");
    println!("");
    println!("   Agent Reputation:");
    println!("   GET  /api/reputation          - List all agents");
    println!("   GET  /api/reputation/:name    - Get agent reputation");
    println!("   POST /api/reputation/:name    - Update reputation (bid/win/deliver)");
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
