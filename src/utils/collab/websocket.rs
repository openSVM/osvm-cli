//! WebSocket Server for Collaborative Sessions
//!
//! Enables browser-based participants to join collaborative investigation sessions.
//! Provides real-time screen streaming, presence updates, and annotation sync.

use super::{
    CollabError,
    session::{CollaborativeSession, SessionRole, ClientType, ParticipantColor, SessionEvent, Participant},
    presence::{PresenceManager, UserPresence, ActivityStatus},
    annotations::{AnnotationStore, Annotation, AnnotationType, AnnotationSeverity},
};

use std::sync::Arc;
use std::collections::HashMap;
use tokio::sync::{broadcast, RwLock};
use axum::{
    Router,
    routing::{get, post},
    extract::{
        State, Path, Query, WebSocketUpgrade,
        ws::{Message, WebSocket},
    },
    response::{IntoResponse, Html},
    Json,
    http::StatusCode,
};
use serde::{Serialize, Deserialize};
use chrono::{DateTime, Utc};

/// Events sent to/from WebSocket clients
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum CollabEvent {
    // Client -> Server events
    /// Client requesting to join
    Join {
        name: String,
        invite_code: String,
        password: Option<String>,
    },
    /// Client sending input (keystrokes)
    Input {
        keys: String,
        pane: Option<usize>,
    },
    /// Client cursor moved
    CursorMove {
        x: u16,
        y: u16,
        pane: usize,
    },
    /// Client adding annotation
    AddAnnotation {
        target_type: String,
        target_id: String,
        text: String,
        severity: String,
    },
    /// Client sending chat message
    Chat {
        message: String,
    },
    /// Client requesting screen refresh
    RequestScreen {
        pane: Option<usize>,
    },
    /// Client setting status
    SetStatus {
        status: String,
    },
    /// Client pinging
    Ping,

    // Server -> Client events
    /// Welcome message on connect
    Welcome {
        session_id: String,
        session_name: String,
        participant_id: String,
        role: String,
        color: [u8; 3],
    },
    /// Screen content update
    Screen {
        content: String,
        pane: usize,
        width: u16,
        height: u16,
    },
    /// Participant list update
    Participants {
        participants: Vec<ParticipantInfo>,
    },
    /// Someone else's cursor position
    OtherCursor {
        participant_id: String,
        name: String,
        x: u16,
        y: u16,
        pane: usize,
        color: [u8; 3],
    },
    /// Annotation update
    AnnotationUpdate {
        annotation: AnnotationInfo,
    },
    /// Chat message received
    ChatReceived {
        from: String,
        message: String,
        timestamp: DateTime<Utc>,
    },
    /// Error message
    Error {
        code: String,
        message: String,
    },
    /// Pong response
    Pong,
}

/// Simplified participant info for serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParticipantInfo {
    pub id: String,
    pub name: String,
    pub role: String,
    pub status: String,
    pub color: [u8; 3],
    pub cursor: Option<CursorInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CursorInfo {
    pub x: u16,
    pub y: u16,
    pub pane: usize,
}

/// Simplified annotation info for serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotationInfo {
    pub id: String,
    pub target_type: String,
    pub target_id: String,
    pub text: String,
    pub severity: String,
    pub author_name: String,
    pub color: [u8; 3],
    pub created_at: DateTime<Utc>,
}

/// WebSocket connection state
struct WsConnection {
    participant_id: String,
    session_id: String,
}

/// Server state for collaborative WebSocket handling
pub struct CollabWebSocketServer {
    /// Active sessions by ID
    sessions: RwLock<HashMap<String, Arc<CollaborativeSession>>>,
    /// Sessions by invite code (for lookup)
    by_invite_code: RwLock<HashMap<String, String>>,
    /// Screen update interval in milliseconds
    screen_update_interval_ms: u64,
}

impl CollabWebSocketServer {
    pub fn new() -> Self {
        Self {
            sessions: RwLock::new(HashMap::new()),
            by_invite_code: RwLock::new(HashMap::new()),
            screen_update_interval_ms: 100,
        }
    }

    /// Register a session
    pub async fn register_session(&self, session: Arc<CollaborativeSession>) {
        let session_id = session.id().to_string();
        let invite_code = session.invite_code().to_string();

        let mut sessions = self.sessions.write().await;
        let mut by_invite = self.by_invite_code.write().await;

        sessions.insert(session_id.clone(), Arc::clone(&session));
        by_invite.insert(invite_code, session_id);
    }

    /// Unregister a session
    pub async fn unregister_session(&self, session_id: &str) {
        let mut sessions = self.sessions.write().await;
        if let Some(session) = sessions.remove(session_id) {
            let mut by_invite = self.by_invite_code.write().await;
            by_invite.remove(session.invite_code());
        }
    }

    /// Get session by ID
    pub async fn get_session(&self, session_id: &str) -> Option<Arc<CollaborativeSession>> {
        let sessions = self.sessions.read().await;
        sessions.get(session_id).cloned()
    }

    /// Get session by invite code
    pub async fn get_session_by_invite(&self, invite_code: &str) -> Option<Arc<CollaborativeSession>> {
        let by_invite = self.by_invite_code.read().await;
        if let Some(session_id) = by_invite.get(invite_code) {
            let sessions = self.sessions.read().await;
            return sessions.get(session_id).cloned();
        }
        None
    }

    /// Create the Axum router for collaborative endpoints
    pub fn router(self: Arc<Self>) -> Router {
        Router::new()
            .route("/collab/:session_id", get(Self::websocket_handler))
            .route("/collab/join/:invite_code", get(Self::join_page))
            .route("/api/collab/sessions", get(Self::list_sessions))
            .route("/api/collab/sessions/:id", get(Self::get_session_info))
            .route("/api/collab/sessions/:id/annotations", get(Self::get_annotations))
            .with_state(self)
    }

    /// WebSocket upgrade handler
    async fn websocket_handler(
        ws: WebSocketUpgrade,
        State(state): State<Arc<Self>>,
        Path(session_id): Path<String>,
    ) -> impl IntoResponse {
        ws.on_upgrade(move |socket| Self::handle_websocket(socket, state, session_id))
    }

    /// Handle a WebSocket connection
    async fn handle_websocket(
        socket: WebSocket,
        state: Arc<Self>,
        session_id: String,
    ) {
        use futures::{SinkExt, StreamExt};
        let (mut sender, mut receiver) = socket.split();

        // Get the session
        let session = match state.get_session(&session_id).await {
            Some(s) => s,
            None => {
                let error = CollabEvent::Error {
                    code: "SESSION_NOT_FOUND".to_string(),
                    message: "Session not found".to_string(),
                };
                let _ = sender.send(Message::Text(serde_json::to_string(&error).unwrap())).await;
                return;
            }
        };

        // Wait for join message
        let mut participant_id: Option<String> = None;
        let mut event_rx: Option<broadcast::Receiver<SessionEvent>> = None;

        // Message handling loop
        loop {
            tokio::select! {
                // Handle incoming WebSocket messages
                Some(msg) = receiver.next() => {
                    let msg = match msg {
                        Ok(Message::Text(text)) => text,
                        Ok(Message::Close(_)) => break,
                        Ok(Message::Ping(data)) => {
                            let _ = sender.send(Message::Pong(data)).await;
                            continue;
                        }
                        Err(_) => break,
                        _ => continue,
                    };

                    // Parse the event
                    let event: CollabEvent = match serde_json::from_str(&msg) {
                        Ok(e) => e,
                        Err(_) => continue,
                    };

                    match event {
                        CollabEvent::Join { name, invite_code, password } => {
                            // Verify invite code matches
                            if invite_code != session.invite_code() {
                                let error = CollabEvent::Error {
                                    code: "INVALID_INVITE".to_string(),
                                    message: "Invalid invite code".to_string(),
                                };
                                let _ = sender.send(Message::Text(serde_json::to_string(&error).unwrap())).await;
                                continue;
                            }

                            // Add participant
                            match session.add_participant(name, SessionRole::Editor, ClientType::Browser).await {
                                Ok(p) => {
                                    participant_id = Some(p.id.clone());
                                    event_rx = Some(session.subscribe());

                                    // Send welcome
                                    let welcome = CollabEvent::Welcome {
                                        session_id: session.id().to_string(),
                                        session_name: session.name().to_string(),
                                        participant_id: p.id.clone(),
                                        role: format!("{:?}", p.role),
                                        color: [p.color.r, p.color.g, p.color.b],
                                    };
                                    let _ = sender.send(Message::Text(serde_json::to_string(&welcome).unwrap())).await;

                                    // Send initial screen
                                    if let Ok(content) = session.capture_screen_ansi(None) {
                                        let screen = CollabEvent::Screen {
                                            content,
                                            pane: 0,
                                            width: 200,
                                            height: 60,
                                        };
                                        let _ = sender.send(Message::Text(serde_json::to_string(&screen).unwrap())).await;
                                    }
                                }
                                Err(e) => {
                                    let error = CollabEvent::Error {
                                        code: "JOIN_FAILED".to_string(),
                                        message: e.to_string(),
                                    };
                                    let _ = sender.send(Message::Text(serde_json::to_string(&error).unwrap())).await;
                                }
                            }
                        }

                        CollabEvent::Input { keys, pane } => {
                            if participant_id.is_some() {
                                let _ = session.send_keys(&keys, pane);
                            }
                        }

                        CollabEvent::CursorMove { x, y, pane } => {
                            if let Some(ref pid) = participant_id {
                                session.presence().update_cursor(pid, x, y, pane).await;
                            }
                        }

                        CollabEvent::Chat { message } => {
                            if let Some(ref pid) = participant_id {
                                session.broadcast_chat(pid, &message);
                            }
                        }

                        CollabEvent::RequestScreen { pane } => {
                            if let Ok(content) = session.capture_screen_ansi(pane) {
                                let screen = CollabEvent::Screen {
                                    content,
                                    pane: pane.unwrap_or(0),
                                    width: 200,
                                    height: 60,
                                };
                                let _ = sender.send(Message::Text(serde_json::to_string(&screen).unwrap())).await;
                            }
                        }

                        CollabEvent::Ping => {
                            let _ = sender.send(Message::Text(serde_json::to_string(&CollabEvent::Pong).unwrap())).await;
                        }

                        _ => {}
                    }
                }

                // Handle session events (if joined)
                result = async {
                    if let Some(ref mut rx) = event_rx {
                        rx.recv().await.ok()
                    } else {
                        None
                    }
                } => {
                    if let Some(session_event) = result {
                        // Convert session event to collab event and send
                        let event = match session_event {
                            SessionEvent::ParticipantJoined { participant } => {
                                // Send updated participant list
                                let participants = session.list_participants().await;
                                CollabEvent::Participants {
                                    participants: participants.iter().map(|p| ParticipantInfo {
                                        id: p.id.clone(),
                                        name: p.name.clone(),
                                        role: format!("{:?}", p.role),
                                        status: "active".to_string(),
                                        color: [p.color.r, p.color.g, p.color.b],
                                        cursor: None,
                                    }).collect(),
                                }
                            }
                            SessionEvent::ParticipantLeft { .. } => {
                                let participants = session.list_participants().await;
                                CollabEvent::Participants {
                                    participants: participants.iter().map(|p| ParticipantInfo {
                                        id: p.id.clone(),
                                        name: p.name.clone(),
                                        role: format!("{:?}", p.role),
                                        status: "active".to_string(),
                                        color: [p.color.r, p.color.g, p.color.b],
                                        cursor: None,
                                    }).collect(),
                                }
                            }
                            SessionEvent::ChatMessage { participant_id: pid, message } => {
                                CollabEvent::ChatReceived {
                                    from: pid,
                                    message,
                                    timestamp: Utc::now(),
                                }
                            }
                            _ => continue,
                        };

                        let _ = sender.send(Message::Text(serde_json::to_string(&event).unwrap())).await;
                    }
                }
            }
        }

        // Cleanup on disconnect
        if let Some(pid) = participant_id {
            session.remove_participant(&pid).await;
        }
    }

    /// Serve a simple join page
    async fn join_page(
        State(state): State<Arc<Self>>,
        Path(invite_code): Path<String>,
    ) -> impl IntoResponse {
        let session = state.get_session_by_invite(&invite_code).await;

        let html = match session {
            Some(s) => format!(r#"
<!DOCTYPE html>
<html>
<head>
    <title>Join: {}</title>
    <style>
        body {{ font-family: monospace; background: #1e1e1e; color: #d4d4d4; padding: 20px; }}
        #terminal {{ background: #000; padding: 10px; white-space: pre; font-size: 12px; }}
        .participant {{ display: inline-block; margin: 5px; padding: 5px 10px; border-radius: 3px; }}
        input {{ background: #333; color: #fff; border: 1px solid #555; padding: 5px; }}
        button {{ background: #0078d4; color: #fff; border: none; padding: 8px 16px; cursor: pointer; }}
    </style>
</head>
<body>
    <h1>Join Investigation: {}</h1>
    <div id="join-form">
        <input type="text" id="name" placeholder="Your name" />
        <button onclick="join()">Join Session</button>
    </div>
    <div id="session" style="display:none;">
        <div id="participants"></div>
        <div id="terminal"></div>
        <input type="text" id="chat" placeholder="Chat message..." onkeypress="sendChat(event)" />
    </div>
    <script>
        let ws;
        let participantId;

        function join() {{
            const name = document.getElementById('name').value || 'Anonymous';
            const wsUrl = `ws://${{window.location.host}}/collab/{}`;
            ws = new WebSocket(wsUrl);

            ws.onopen = () => {{
                ws.send(JSON.stringify({{
                    type: 'Join',
                    name: name,
                    invite_code: '{}',
                    password: null
                }}));
            }};

            ws.onmessage = (event) => {{
                const data = JSON.parse(event.data);
                handleEvent(data);
            }};

            ws.onclose = () => {{
                alert('Disconnected from session');
            }};
        }}

        function handleEvent(event) {{
            switch(event.type) {{
                case 'Welcome':
                    participantId = event.participant_id;
                    document.getElementById('join-form').style.display = 'none';
                    document.getElementById('session').style.display = 'block';
                    break;
                case 'Screen':
                    document.getElementById('terminal').innerHTML = ansiToHtml(event.content);
                    break;
                case 'Participants':
                    updateParticipants(event.participants);
                    break;
                case 'ChatReceived':
                    console.log(`[${{event.from}}]: ${{event.message}}`);
                    break;
                case 'Error':
                    alert(event.message);
                    break;
            }}
        }}

        function ansiToHtml(text) {{
            // Basic ANSI to HTML conversion
            return text
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
        }}

        function updateParticipants(participants) {{
            const el = document.getElementById('participants');
            el.innerHTML = participants.map(p =>
                `<span class="participant" style="background:rgb(${{p.color.join(',')}})">${{p.name}}</span>`
            ).join('');
        }}

        function sendChat(e) {{
            if (e.key === 'Enter' && ws) {{
                const input = document.getElementById('chat');
                ws.send(JSON.stringify({{ type: 'Chat', message: input.value }}));
                input.value = '';
            }}
        }}

        // Request screen updates periodically
        setInterval(() => {{
            if (ws && ws.readyState === 1) {{
                ws.send(JSON.stringify({{ type: 'RequestScreen', pane: null }}));
            }}
        }}, 200);
    </script>
</body>
</html>
"#, s.name(), s.name(), s.id(), invite_code),
            None => r#"
<!DOCTYPE html>
<html>
<head><title>Session Not Found</title></head>
<body style="font-family: monospace; background: #1e1e1e; color: #d4d4d4; padding: 20px;">
    <h1>Session Not Found</h1>
    <p>The invite code may have expired or be invalid.</p>
</body>
</html>
"#.to_string(),
        };

        Html(html)
    }

    /// List active sessions (API endpoint)
    async fn list_sessions(
        State(state): State<Arc<Self>>,
    ) -> impl IntoResponse {
        let sessions = state.sessions.read().await;
        let list: Vec<_> = sessions.values()
            .map(|s| serde_json::json!({
                "id": s.id(),
                "name": s.name(),
                "invite_code": s.invite_code(),
            }))
            .collect();

        Json(list)
    }

    /// Get session info (API endpoint)
    async fn get_session_info(
        State(state): State<Arc<Self>>,
        Path(session_id): Path<String>,
    ) -> impl IntoResponse {
        match state.get_session(&session_id).await {
            Some(session) => {
                let participants = session.list_participants().await;
                let info = serde_json::json!({
                    "id": session.id(),
                    "name": session.name(),
                    "invite_code": session.invite_code(),
                    "participants": participants.len(),
                    "participant_list": participants.iter().map(|p| serde_json::json!({
                        "id": p.id,
                        "name": p.name,
                        "role": format!("{:?}", p.role),
                    })).collect::<Vec<_>>(),
                });
                (StatusCode::OK, Json(info)).into_response()
            }
            None => {
                (StatusCode::NOT_FOUND, Json(serde_json::json!({
                    "error": "Session not found"
                }))).into_response()
            }
        }
    }

    /// Get session annotations (API endpoint)
    async fn get_annotations(
        State(state): State<Arc<Self>>,
        Path(session_id): Path<String>,
    ) -> impl IntoResponse {
        match state.get_session(&session_id).await {
            Some(session) => {
                let annotations = session.annotations().get_all().await;
                let list: Vec<_> = annotations.iter().map(|a| serde_json::json!({
                    "id": a.id,
                    "target": format!("{}", a.target),
                    "text": a.text,
                    "severity": format!("{:?}", a.severity),
                    "author": a.author_name,
                    "created_at": a.created_at,
                })).collect();

                (StatusCode::OK, Json(list)).into_response()
            }
            None => {
                (StatusCode::NOT_FOUND, Json(serde_json::json!({
                    "error": "Session not found"
                }))).into_response()
            }
        }
    }
}

impl Default for CollabWebSocketServer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collab_event_serialization() {
        let event = CollabEvent::Join {
            name: "Alice".to_string(),
            invite_code: "ABC123".to_string(),
            password: None,
        };

        let json = serde_json::to_string(&event).unwrap();
        assert!(json.contains("Join"));
        assert!(json.contains("Alice"));
    }

    #[test]
    fn test_participant_info() {
        let info = ParticipantInfo {
            id: "123".to_string(),
            name: "Alice".to_string(),
            role: "Editor".to_string(),
            status: "active".to_string(),
            color: [255, 100, 50],
            cursor: Some(CursorInfo { x: 10, y: 20, pane: 0 }),
        };

        let json = serde_json::to_string(&info).unwrap();
        assert!(json.contains("Alice"));
        assert!(json.contains("[255,100,50]"));
    }
}
