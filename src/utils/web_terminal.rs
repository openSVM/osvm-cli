//! Web Terminal Streaming Server
//!
//! Streams terminal output to a browser using WebSocket for bidirectional communication.
//! Opens at http://localhost:13370 (default) with xterm.js terminal emulator.
//!
//! Features:
//! - Output streaming: Terminal → Browser (base64 encoded)
//! - Input streaming: Browser → Terminal (keyboard events)
//! - Multiple viewers supported
//!
//! Usage:
//! ```ignore
//! let (server, sender, input_rx) = WebTerminal::start(13370).await?;
//! sender.send("Hello from terminal!\n")?;
//! // input_rx receives KeyEvent from browser
//! ```

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        State,
    },
    response::{Html, IntoResponse},
    routing::get,
    Router,
};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use futures::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, RwLock};
use tower_http::cors::{Any, CorsLayer};

/// Default port for web terminal
pub const DEFAULT_PORT: u16 = 13370;

/// Maximum number of frames to buffer for new connections
const HISTORY_SIZE: usize = 100;

/// Keyboard event from browser
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebKeyEvent {
    /// Key code (e.g., "Enter", "Escape", "a", "ArrowUp")
    pub key: String,
    /// Ctrl key pressed
    #[serde(default)]
    pub ctrl: bool,
    /// Shift key pressed
    #[serde(default)]
    pub shift: bool,
    /// Alt key pressed
    #[serde(default)]
    pub alt: bool,
}

impl WebKeyEvent {
    /// Convert to crossterm KeyEvent
    pub fn to_crossterm(&self) -> Option<KeyEvent> {
        let mut modifiers = KeyModifiers::empty();
        if self.ctrl {
            modifiers |= KeyModifiers::CONTROL;
        }
        if self.shift {
            modifiers |= KeyModifiers::SHIFT;
        }
        if self.alt {
            modifiers |= KeyModifiers::ALT;
        }

        let code = match self.key.as_str() {
            "Enter" => KeyCode::Enter,
            "Escape" => KeyCode::Esc,
            "Tab" => KeyCode::Tab,
            "Backspace" => KeyCode::Backspace,
            "Delete" => KeyCode::Delete,
            "ArrowUp" => KeyCode::Up,
            "ArrowDown" => KeyCode::Down,
            "ArrowLeft" => KeyCode::Left,
            "ArrowRight" => KeyCode::Right,
            "Home" => KeyCode::Home,
            "End" => KeyCode::End,
            "PageUp" => KeyCode::PageUp,
            "PageDown" => KeyCode::PageDown,
            "F1" => KeyCode::F(1),
            "F2" => KeyCode::F(2),
            "F3" => KeyCode::F(3),
            "F4" => KeyCode::F(4),
            "F5" => KeyCode::F(5),
            "F6" => KeyCode::F(6),
            "F7" => KeyCode::F(7),
            "F8" => KeyCode::F(8),
            "F9" => KeyCode::F(9),
            "F10" => KeyCode::F(10),
            "F11" => KeyCode::F(11),
            "F12" => KeyCode::F(12),
            " " => KeyCode::Char(' '),
            s if s.len() == 1 => {
                let c = s.chars().next().unwrap();
                KeyCode::Char(c)
            }
            _ => return None,
        };

        Some(KeyEvent::new(code, modifiers))
    }
}

/// Web terminal server state
struct WebTerminalStateInner {
    /// Broadcast channel for terminal output
    output_tx: broadcast::Sender<String>,
    /// History buffer for new connections
    history: RwLock<VecDeque<String>>,
    /// Input channel sender (to forward keys to TUI)
    input_tx: mpsc::Sender<KeyEvent>,
    /// Connected viewer count
    viewer_count: AtomicUsize,
}

/// Handle for sending output to the web terminal
#[derive(Clone)]
pub struct WebTerminalSender {
    tx: broadcast::Sender<String>,
    history: Arc<RwLock<VecDeque<String>>>,
}

impl WebTerminalSender {
    /// Send raw data to the web terminal (base64 encoded for transport)
    pub fn send(&self, line: &str) -> Result<(), broadcast::error::SendError<String>> {
        use base64::Engine;
        let encoded = base64::engine::general_purpose::STANDARD.encode(line.as_bytes());
        let history = Arc::clone(&self.history);
        let encoded_clone = encoded.clone();
        tokio::spawn(async move {
            let mut h = history.write().await;
            h.push_back(encoded_clone);
            if h.len() > HISTORY_SIZE {
                h.pop_front();
            }
        });
        self.tx.send(encoded)?;
        Ok(())
    }

    /// Send formatted line with newline
    pub fn println(&self, line: &str) {
        use base64::Engine;
        let msg = format!("{}\r\n", line);
        let encoded = base64::engine::general_purpose::STANDARD.encode(msg.as_bytes());
        let history = Arc::clone(&self.history);
        let encoded_clone = encoded.clone();
        tokio::spawn(async move {
            let mut h = history.write().await;
            h.push_back(encoded_clone);
            if h.len() > HISTORY_SIZE {
                h.pop_front();
            }
        });
        let _ = self.tx.send(encoded);
    }
}

/// Receiver for keyboard input from browser
pub struct WebInputReceiver {
    rx: mpsc::Receiver<KeyEvent>,
}

impl WebInputReceiver {
    /// Try to receive a key event (non-blocking)
    pub fn try_recv(&mut self) -> Option<KeyEvent> {
        self.rx.try_recv().ok()
    }

    /// Receive a key event (blocking)
    pub async fn recv(&mut self) -> Option<KeyEvent> {
        self.rx.recv().await
    }
}

/// Web terminal streaming server
pub struct WebTerminal;

impl WebTerminal {
    /// Start the web server with bidirectional WebSocket
    /// Returns: (server_handle, output_sender, input_receiver)
    pub async fn start(
        port: u16,
    ) -> anyhow::Result<(
        tokio::task::JoinHandle<()>,
        WebTerminalSender,
        WebInputReceiver,
    )> {
        let (output_tx, _) = broadcast::channel(1024);
        let (input_tx, input_rx) = mpsc::channel(256);
        let history = Arc::new(RwLock::new(VecDeque::with_capacity(HISTORY_SIZE)));

        let state = Arc::new(WebTerminalStateInner {
            output_tx: output_tx.clone(),
            history: RwLock::new(VecDeque::new()),
            input_tx,
            viewer_count: AtomicUsize::new(0),
        });

        let sender = WebTerminalSender {
            tx: output_tx,
            history: Arc::clone(&history),
        };

        let receiver = WebInputReceiver { rx: input_rx };

        // Update state history to use shared history
        {
            let mut h = state.history.write().await;
            *h = VecDeque::with_capacity(HISTORY_SIZE);
        }

        // Build router with WebSocket
        let app = Router::new()
            .route("/", get(serve_index))
            .route("/ws", get(ws_handler))
            .with_state(state)
            .layer(CorsLayer::new().allow_origin(Any));

        // Start server
        let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{}", port)).await?;
        let handle = tokio::spawn(async move {
            axum::serve(listener, app).await.ok();
        });

        Ok((handle, sender, receiver))
    }
}

/// Serve the HTML page
async fn serve_index() -> Html<&'static str> {
    Html(INDEX_HTML)
}

/// WebSocket handler for bidirectional communication
async fn ws_handler(
    ws: WebSocketUpgrade,
    State(state): State<Arc<WebTerminalStateInner>>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, state))
}

/// Handle individual WebSocket connection
async fn handle_socket(socket: WebSocket, state: Arc<WebTerminalStateInner>) {
    let (mut ws_sender, mut ws_receiver) = socket.split();

    // Increment viewer count
    let viewers = state.viewer_count.fetch_add(1, Ordering::SeqCst) + 1;
    eprintln!("🌐 Web viewer connected ({} total)", viewers);

    // Subscribe to output broadcast
    let mut output_rx = state.output_tx.subscribe();

    // Send history first
    {
        let history = state.history.read().await;
        for msg in history.iter() {
            let json = serde_json::json!({ "type": "output", "data": msg });
            if ws_sender
                .send(Message::Text(json.to_string().into()))
                .await
                .is_err()
            {
                break;
            }
        }
    }

    // Spawn task to forward output to WebSocket
    let output_task = {
        let mut ws_sender = ws_sender;
        tokio::spawn(async move {
            loop {
                match output_rx.recv().await {
                    Ok(msg) => {
                        let json = serde_json::json!({ "type": "output", "data": msg });
                        if ws_sender
                            .send(Message::Text(json.to_string().into()))
                            .await
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(broadcast::error::RecvError::Closed) => break,
                }
            }
        })
    };

    // Handle incoming messages (keyboard input)
    let input_tx = state.input_tx.clone();
    while let Some(msg) = ws_receiver.next().await {
        match msg {
            Ok(Message::Text(text)) => {
                // Try to parse as WebKeyEvent
                if let Ok(key_event) = serde_json::from_str::<WebKeyEvent>(&text) {
                    if let Some(crossterm_event) = key_event.to_crossterm() {
                        let _ = input_tx.send(crossterm_event).await;
                    }
                }
            }
            Ok(Message::Close(_)) => break,
            Err(_) => break,
            _ => {}
        }
    }

    // Cleanup
    output_task.abort();
    let viewers = state.viewer_count.fetch_sub(1, Ordering::SeqCst) - 1;
    eprintln!("🌐 Web viewer disconnected ({} remaining)", viewers);
}

/// Embedded HTML page with xterm.js terminal and keyboard input
const INDEX_HTML: &str = r##"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OSVM Research Terminal</title>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css">
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            background: #0d1117;
            color: #c9d1d9;
            font-family: 'SF Mono', 'Monaco', 'Inconsolata', 'Fira Mono', monospace;
            height: 100vh;
            display: flex;
            flex-direction: column;
        }
        .header {
            background: linear-gradient(135deg, #1a1f29 0%, #0d1117 100%);
            border-bottom: 1px solid #30363d;
            padding: 12px 20px;
            display: flex;
            align-items: center;
            gap: 12px;
        }
        .logo { font-size: 24px; }
        .title { font-size: 16px; font-weight: 600; color: #58a6ff; }
        .subtitle { font-size: 12px; color: #8b949e; margin-left: auto; }
        .status { display: flex; align-items: center; gap: 6px; font-size: 12px; }
        .status-dot {
            width: 8px; height: 8px; border-radius: 50%;
            background: #238636; animation: pulse 2s infinite;
        }
        .status-dot.disconnected { background: #f85149; animation: none; }
        @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
        #terminal-container { flex: 1; padding: 10px; overflow: hidden; }
        #terminal { height: 100%; }
        .footer {
            background: #161b22;
            border-top: 1px solid #30363d;
            padding: 8px 20px;
            font-size: 11px;
            color: #8b949e;
            display: flex;
            justify-content: space-between;
        }
        .footer a { color: #58a6ff; text-decoration: none; }
        .input-mode {
            background: #238636;
            color: white;
            padding: 2px 8px;
            border-radius: 4px;
            font-size: 10px;
            margin-left: 10px;
        }
    </style>
</head>
<body>
    <div class="header">
        <span class="logo">🔬</span>
        <span class="title">OSVM Research Terminal</span>
        <span class="input-mode">INTERACTIVE</span>
        <span class="subtitle">Click terminal to enable keyboard input</span>
        <div class="status">
            <div class="status-dot" id="status-dot"></div>
            <span id="status-text">Connecting...</span>
        </div>
    </div>
    <div id="terminal-container">
        <div id="terminal"></div>
    </div>
    <div class="footer">
        <span>⌨️ Keyboard input enabled - type to control the TUI</span>
        <span>Powered by <a href="https://github.com/openSVM/osvm-cli" target="_blank">OSVM CLI</a></span>
    </div>

    <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/lib/xterm-addon-fit.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/xterm-addon-web-links@0.9.0/lib/xterm-addon-web-links.min.js"></script>
    <script>
        const term = new Terminal({
            theme: {
                background: '#0d1117',
                foreground: '#c9d1d9',
                cursor: '#58a6ff',
                cursorAccent: '#0d1117',
                selectionBackground: '#388bfd33',
                black: '#484f58', red: '#ff7b72', green: '#3fb950', yellow: '#d29922',
                blue: '#58a6ff', magenta: '#bc8cff', cyan: '#39c5cf', white: '#b1bac4',
                brightBlack: '#6e7681', brightRed: '#ffa198', brightGreen: '#56d364',
                brightYellow: '#e3b341', brightBlue: '#79c0ff', brightMagenta: '#d2a8ff',
                brightCyan: '#56d4dd', brightWhite: '#f0f6fc',
            },
            fontFamily: '"SF Mono", "Monaco", "Inconsolata", "Fira Mono", monospace',
            fontSize: 14,
            lineHeight: 1.2,
            cursorBlink: true,
            cursorStyle: 'bar',
            scrollback: 10000,
            convertEol: true,
        });

        const fitAddon = new FitAddon.FitAddon();
        const webLinksAddon = new WebLinksAddon.WebLinksAddon();
        term.loadAddon(fitAddon);
        term.loadAddon(webLinksAddon);
        term.open(document.getElementById('terminal'));
        fitAddon.fit();
        window.addEventListener('resize', () => fitAddon.fit());

        // Welcome message
        term.writeln('\x1b[38;5;33m╔══════════════════════════════════════════════════════════════╗\x1b[0m');
        term.writeln('\x1b[38;5;33m║\x1b[0m  \x1b[1;36m🔬 OSVM Research Terminal\x1b[0m                                    \x1b[38;5;33m║\x1b[0m');
        term.writeln('\x1b[38;5;33m║\x1b[0m  \x1b[90mInteractive mode - keyboard input enabled\x1b[0m                    \x1b[38;5;33m║\x1b[0m');
        term.writeln('\x1b[38;5;33m╚══════════════════════════════════════════════════════════════╝\x1b[0m');
        term.writeln('');

        const statusDot = document.getElementById('status-dot');
        const statusText = document.getElementById('status-text');
        let ws = null;

        function connect() {
            const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            ws = new WebSocket(`${protocol}//${window.location.host}/ws`);

            ws.onopen = () => {
                statusDot.classList.remove('disconnected');
                statusText.textContent = 'Connected';
                term.writeln('\x1b[32m✓ Connected to OSVM - keyboard input active\x1b[0m');
                term.writeln('');
                term.focus();
            };

            ws.onmessage = (event) => {
                try {
                    const msg = JSON.parse(event.data);
                    if (msg.type === 'output' && msg.data) {
                        // Decode base64 -> bytes -> UTF-8
                        const binaryString = atob(msg.data);
                        const bytes = new Uint8Array(binaryString.length);
                        for (let i = 0; i < binaryString.length; i++) {
                            bytes[i] = binaryString.charCodeAt(i);
                        }
                        const decoded = new TextDecoder('utf-8').decode(bytes);
                        term.write(decoded);
                    }
                } catch (e) {
                    console.error('Parse error:', e);
                }
            };

            ws.onclose = () => {
                statusDot.classList.add('disconnected');
                statusText.textContent = 'Reconnecting...';
                setTimeout(connect, 2000);
            };

            ws.onerror = () => {
                ws.close();
            };
        }

        // Capture keyboard input and send to server
        term.onKey(({ key, domEvent }) => {
            if (!ws || ws.readyState !== WebSocket.OPEN) return;

            const keyEvent = {
                key: domEvent.key,
                ctrl: domEvent.ctrlKey,
                shift: domEvent.shiftKey,
                alt: domEvent.altKey
            };

            // Don't send browser shortcuts
            if (domEvent.ctrlKey && ['c', 'v', 'a', 'x'].includes(domEvent.key.toLowerCase())) {
                return;
            }

            ws.send(JSON.stringify(keyEvent));
            domEvent.preventDefault();
        });

        // Focus terminal on click
        document.getElementById('terminal-container').addEventListener('click', () => {
            term.focus();
        });

        connect();
    </script>
</body>
</html>
"##;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_web_key_event_conversion() {
        let event = WebKeyEvent {
            key: "Enter".to_string(),
            ctrl: false,
            shift: false,
            alt: false,
        };
        let crossterm = event.to_crossterm().unwrap();
        assert_eq!(crossterm.code, KeyCode::Enter);

        let event = WebKeyEvent {
            key: "c".to_string(),
            ctrl: true,
            shift: false,
            alt: false,
        };
        let crossterm = event.to_crossterm().unwrap();
        assert_eq!(crossterm.code, KeyCode::Char('c'));
        assert!(crossterm.modifiers.contains(KeyModifiers::CONTROL));
    }
}
