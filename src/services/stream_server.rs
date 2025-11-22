use anyhow::Result;
use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        Path, Query, State,
    },
    http::StatusCode,
    response::{sse::{Event, KeepAlive, Sse}, IntoResponse, Json},
    routing::{get, post},
    Router,
};
use futures::{stream::Stream, StreamExt};
use serde::{Deserialize, Serialize};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::broadcast;
use tower_http::cors::{Any, CorsLayer};

use super::stream_service::{EventFilter, SolanaEvent, StreamService, StreamStats};

/// Stream server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamServerConfig {
    pub host: String,
    pub port: u16,
    pub enable_websocket: bool,
    pub enable_sse: bool,
    pub enable_http: bool,
}

impl Default for StreamServerConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".to_string(),
            port: 8080,
            enable_websocket: true,
            enable_sse: true,
            enable_http: true,
        }
    }
}

/// Shared application state
#[derive(Clone)]
struct AppState {
    stream_service: Arc<StreamService>,
}

/// Start the streaming server
pub async fn start_server(
    config: StreamServerConfig,
    stream_service: Arc<StreamService>,
) -> Result<()> {
    let app_state = AppState {
        stream_service: stream_service.clone(),
    };

    // Start the stream service
    stream_service.start().await?;

    // Build the router
    let mut app = Router::new();

    if config.enable_websocket {
        app = app.route("/ws", get(websocket_handler));
    }

    if config.enable_sse {
        app = app.route("/stream", get(sse_handler));
    }

    if config.enable_http {
        app = app
            .route("/events", get(get_events_handler))
            .route("/stats", get(get_stats_handler))
            .route("/filter", post(set_filter_handler))
            .route("/health", get(health_handler));
    }

    app = app
        .with_state(app_state)
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        );

    let addr: SocketAddr = format!("{}:{}", config.host, config.port).parse()?;
    tracing::info!("Stream server listening on {}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// WebSocket handler
async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
) -> impl IntoResponse {
    ws.on_upgrade(|socket| handle_websocket(socket, state))
}

async fn handle_websocket(socket: WebSocket, state: AppState) {
    let (mut sender, mut receiver) = socket.split();
    let mut rx = state.stream_service.subscribe();

    // Spawn a task to send events to the WebSocket client
    let send_task = tokio::spawn(async move {
        while let Ok(event) = rx.recv().await {
            let json = serde_json::to_string(&event).unwrap();
            if sender.send(Message::Text(json)).await.is_err() {
                break;
            }
        }
    });

    // Spawn a task to handle incoming messages (for filter updates, etc.)
    let recv_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = receiver.next().await {
            if let Message::Text(text) = msg {
                // Parse filter commands
                if let Ok(filter) = serde_json::from_str::<EventFilter>(&text) {
                    state.stream_service.add_filter(filter);
                }
            }
        }
    });

    // Wait for either task to complete
    tokio::select! {
        _ = send_task => {}
        _ = recv_task => {}
    }
}

/// Server-Sent Events (SSE) handler
async fn sse_handler(
    State(state): State<AppState>,
) -> Sse<impl Stream<Item = Result<Event, Infallible>>> {
    let rx = state.stream_service.subscribe();

    let stream = tokio_stream::wrappers::BroadcastStream::new(rx)
        .filter_map(|result| async move {
            match result {
                Ok(event) => {
                    let json = serde_json::to_string(&event).ok()?;
                    Some(Ok(Event::default().data(json)))
                }
                Err(_) => None,
            }
        });

    Sse::new(stream).keep_alive(KeepAlive::default())
}

/// HTTP endpoint to get recent events (polling)
#[derive(Deserialize)]
struct EventsQuery {
    #[serde(default)]
    limit: Option<usize>,
}

async fn get_events_handler(
    Query(query): Query<EventsQuery>,
    State(state): State<AppState>,
) -> Json<Vec<SolanaEvent>> {
    // This is a simplified implementation
    // In production, you'd maintain a buffer of recent events
    let limit = query.limit.unwrap_or(10);
    let mut events = Vec::new();

    let mut rx = state.stream_service.subscribe();

    // Collect events for a short duration
    let timeout = tokio::time::sleep(tokio::time::Duration::from_millis(100));
    tokio::pin!(timeout);

    loop {
        tokio::select! {
            Ok(event) = rx.recv() => {
                events.push(event);
                if events.len() >= limit {
                    break;
                }
            }
            _ = &mut timeout => break,
        }
    }

    Json(events)
}

/// Get streaming statistics
async fn get_stats_handler(State(state): State<AppState>) -> Json<StreamStats> {
    Json(state.stream_service.get_stats())
}

/// Set event filter via HTTP
async fn set_filter_handler(
    State(state): State<AppState>,
    Json(filter): Json<EventFilter>,
) -> StatusCode {
    state.stream_service.add_filter(filter);
    StatusCode::OK
}

/// Health check endpoint
async fn health_handler() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "status": "ok",
        "timestamp": std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stream_service_creation() {
        let service = StreamService::new("https://api.mainnet-beta.solana.com".to_string());
        let stats = service.get_stats();
        assert_eq!(stats.events_processed, 0);
    }
}
