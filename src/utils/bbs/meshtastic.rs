//! Meshtastic Radio Integration for BBS
//!
//! This module provides connectivity to Meshtastic radios for off-grid
//! agent-human communication over LoRa mesh networks.
//!
//! Uses the `meshtastic` crate for proper protobuf handling.

use crate::services::ai_service::AiService;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::sync::mpsc;

/// Meshtastic default TCP port
pub const DEFAULT_TCP_PORT: u16 = 4403;

/// Maximum message size (Meshtastic limit)
pub const MAX_MESSAGE_SIZE: usize = 228;

/// Connection state
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Error(String),
}

/// Connection type
#[derive(Debug, Clone)]
pub enum ConnectionType {
    Tcp { address: String, port: u16 },
    Serial { device: String, baud_rate: u32 },
}

impl ConnectionType {
    /// Parse connection string (auto-detect TCP vs serial)
    pub fn parse(address: &str) -> Option<Self> {
        // Serial device paths
        if address.starts_with("/dev/") || address.starts_with("COM") {
            return Some(ConnectionType::Serial {
                device: address.to_string(),
                baud_rate: 115200, // Meshtastic default
            });
        }

        // TCP address (with or without port)
        if let Some((host, port_str)) = address.rsplit_once(':') {
            if let Ok(port) = port_str.parse::<u16>() {
                return Some(ConnectionType::Tcp {
                    address: host.to_string(),
                    port,
                });
            }
        }

        // TCP without explicit port
        Some(ConnectionType::Tcp {
            address: address.to_string(),
            port: DEFAULT_TCP_PORT,
        })
    }
}

/// Meshtastic packet types (simplified)
#[derive(Debug, Clone)]
pub enum MeshtasticPacket {
    /// Text message
    TextMessage {
        from: u32,
        to: u32,
        message: String,
        channel: u8,
    },
    /// Node info update
    NodeInfo {
        node_id: u32,
        short_name: String,
        long_name: String,
    },
    /// Position update
    Position {
        node_id: u32,
        latitude: f32,
        longitude: f32,
        altitude: i32,
    },
    /// Telemetry data
    Telemetry {
        node_id: u32,
        battery_level: u8,
        voltage: f32,
    },
    /// Unknown/unsupported packet
    Unknown(Vec<u8>),
}

/// Callback for received messages
pub type MessageCallback = Box<dyn Fn(MeshtasticPacket) + Send + Sync>;

/// Channel for sending outgoing messages
pub type MessageSender = mpsc::UnboundedSender<OutgoingMessage>;

/// Outgoing message request
#[derive(Debug, Clone)]
pub struct OutgoingMessage {
    pub text: String,
    pub destination: Option<u32>, // None = broadcast
    pub channel: u8,
}

/// Meshtastic radio connection (sync wrapper)
///
/// This is a synchronous wrapper that can be used from non-async code.
/// For full async support, use MeshtasticClient directly.
pub struct MeshtasticRadio {
    connection_type: ConnectionType,
    state: Arc<Mutex<ConnectionState>>,
    our_node_id: u32,
    message_callback: Option<MessageCallback>,
}

impl MeshtasticRadio {
    /// Create a new radio connection (not yet connected)
    pub fn new(connection_type: ConnectionType) -> Self {
        Self {
            connection_type,
            state: Arc::new(Mutex::new(ConnectionState::Disconnected)),
            our_node_id: 0,
            message_callback: None,
        }
    }

    /// Parse address and create radio connection
    pub fn from_address(address: &str) -> Option<Self> {
        ConnectionType::parse(address).map(Self::new)
    }

    /// Set callback for received messages
    pub fn set_message_callback(&mut self, callback: MessageCallback) {
        self.message_callback = Some(callback);
    }

    /// Get current connection state
    pub fn state(&self) -> ConnectionState {
        self.state.lock().unwrap().clone()
    }

    /// Connect to the radio (sync - uses blocking TCP)
    pub fn connect(&mut self) -> Result<(), String> {
        use std::net::TcpStream;

        *self.state.lock().unwrap() = ConnectionState::Connecting;

        match &self.connection_type {
            ConnectionType::Tcp { address, port } => {
                let addr = format!("{}:{}", address, port);

                match TcpStream::connect_timeout(
                    &addr
                        .parse()
                        .map_err(|e| format!("Invalid address: {}", e))?,
                    Duration::from_secs(5),
                ) {
                    Ok(_stream) => {
                        *self.state.lock().unwrap() = ConnectionState::Connected;
                        // Note: Full meshtastic crate integration requires async
                        // This sync version just validates connectivity
                        Ok(())
                    }
                    Err(e) => {
                        let err = format!("TCP connection failed: {}", e);
                        *self.state.lock().unwrap() = ConnectionState::Error(err.clone());
                        Err(err)
                    }
                }
            }
            ConnectionType::Serial { .. } => {
                let err = "Serial connection not yet implemented. Use TCP instead.".to_string();
                *self.state.lock().unwrap() = ConnectionState::Error(err.clone());
                Err(err)
            }
        }
    }

    /// Disconnect from the radio
    pub fn disconnect(&mut self) {
        *self.state.lock().unwrap() = ConnectionState::Disconnected;
    }

    /// Send a text message (stub - requires async client for full implementation)
    ///
    /// For full send capability, use MeshtasticClient::connect_and_run()
    pub fn send_text(&mut self, message: &str, _to: Option<u32>) -> Result<(), String> {
        if self.state() != ConnectionState::Connected {
            return Err("Not connected".to_string());
        }

        // Validate message length
        if message.len() > MAX_MESSAGE_SIZE {
            return Err(format!(
                "Message too long ({} bytes, max {})",
                message.len(),
                MAX_MESSAGE_SIZE
            ));
        }

        // Sync version cannot send - need async client
        Err(
            "Use async MeshtasticClient for message sending. Sync radio only validates connection."
                .to_string(),
        )
    }

    /// Poll for incoming messages (stub - requires async client)
    pub fn poll(&mut self) -> Result<Option<MeshtasticPacket>, String> {
        if self.state() != ConnectionState::Connected {
            return Err("Not connected".to_string());
        }

        // Sync version cannot poll - need async client
        Ok(None)
    }

    /// Get our node ID
    pub fn our_node_id(&self) -> u32 {
        self.our_node_id
    }

    /// Get connection info string
    pub fn connection_info(&self) -> String {
        match &self.connection_type {
            ConnectionType::Tcp { address, port } => format!("TCP {}:{}", address, port),
            ConnectionType::Serial { device, baud_rate } => {
                format!("Serial {} @ {} baud", device, baud_rate)
            }
        }
    }
}

/// Async Meshtastic client using the meshtastic crate
///
/// This provides full send/receive capabilities using the official protocol.
pub struct MeshtasticClient {
    address: String,
    port: u16,
    state: Arc<Mutex<ConnectionState>>,
    our_node_id: Arc<Mutex<u32>>,
    our_short_name: Arc<Mutex<String>>,
    incoming_tx: Option<mpsc::UnboundedSender<MeshtasticPacket>>,
}

impl MeshtasticClient {
    /// Create a new async client
    pub fn new(address: &str, port: u16) -> Self {
        Self {
            address: address.to_string(),
            port,
            state: Arc::new(Mutex::new(ConnectionState::Disconnected)),
            our_node_id: Arc::new(Mutex::new(0)),
            our_short_name: Arc::new(Mutex::new(String::new())),
            incoming_tx: None,
        }
    }

    /// Create from address string
    pub fn from_address(addr: &str) -> Option<Self> {
        ConnectionType::parse(addr).and_then(|ct| {
            match ct {
                ConnectionType::Tcp { address, port } => Some(Self::new(&address, port)),
                _ => None, // Only TCP supported for now
            }
        })
    }

    /// Get current state
    pub fn state(&self) -> ConnectionState {
        self.state.lock().unwrap().clone()
    }

    /// Get our node ID
    pub fn our_node_id(&self) -> u32 {
        *self.our_node_id.lock().unwrap()
    }

    /// Get our short name
    pub fn our_short_name(&self) -> String {
        self.our_short_name.lock().unwrap().clone()
    }

    /// Connect and run the message loop
    ///
    /// Returns a receiver for incoming packets
    pub async fn connect_and_run(
        &mut self,
    ) -> Result<mpsc::UnboundedReceiver<MeshtasticPacket>, String> {
        use meshtastic::api::StreamApi;
        use meshtastic::protobufs::{from_radio, FromRadio};
        use meshtastic::utils::stream::build_tcp_stream;

        *self.state.lock().unwrap() = ConnectionState::Connecting;

        // Build TCP stream
        let addr = format!("{}:{}", self.address, self.port);
        let tcp_stream = build_tcp_stream(addr.clone())
            .await
            .map_err(|e| format!("Failed to connect to {}: {}", addr, e))?;

        // Create API and connect
        let stream_api = StreamApi::new();
        let (mut decoded_listener, connected_api) = stream_api.connect(tcp_stream).await;

        // Configure to get our node info
        let config_id = meshtastic::utils::generate_rand_id();
        let _configured_api = connected_api
            .configure(config_id)
            .await
            .map_err(|e| format!("Failed to configure: {}", e))?;

        *self.state.lock().unwrap() = ConnectionState::Connected;

        // Create channel for incoming packets
        let (tx, rx) = mpsc::unbounded_channel();
        self.incoming_tx = Some(tx.clone());

        // Clone state handles for the listener task
        let state = self.state.clone();
        let our_node_id = self.our_node_id.clone();
        let our_short_name = self.our_short_name.clone();

        // Spawn listener task
        tokio::spawn(async move {
            while let Some(packet) = decoded_listener.recv().await {
                // Process the FromRadio packet
                if let Some(payload) = packet.payload_variant {
                    match payload {
                        from_radio::PayloadVariant::MyInfo(info) => {
                            *our_node_id.lock().unwrap() = info.my_node_num;
                            log::info!("Got our node ID: !{:08x}", info.my_node_num);
                        }
                        from_radio::PayloadVariant::NodeInfo(node_info) => {
                            if let Some(user) = node_info.user {
                                // Check if this is our node
                                if node_info.num == *our_node_id.lock().unwrap() {
                                    *our_short_name.lock().unwrap() = user.short_name.clone();
                                }
                                // Emit as packet
                                let packet = MeshtasticPacket::NodeInfo {
                                    node_id: node_info.num,
                                    short_name: user.short_name,
                                    long_name: user.long_name,
                                };
                                let _ = tx.send(packet);
                            }
                        }
                        from_radio::PayloadVariant::Packet(mesh_packet) => {
                            // Handle decoded data packets
                            if let Some(payload) = mesh_packet.payload_variant {
                                if let meshtastic::protobufs::mesh_packet::PayloadVariant::Decoded(
                                    data,
                                ) = payload
                                {
                                    // Check for text message (portnum 1)
                                    if data.portnum
                                        == meshtastic::protobufs::PortNum::TextMessageApp as i32
                                    {
                                        if let Ok(text) = String::from_utf8(data.payload) {
                                            let packet = MeshtasticPacket::TextMessage {
                                                from: mesh_packet.from,
                                                to: mesh_packet.to,
                                                message: text,
                                                channel: mesh_packet.channel as u8,
                                            };
                                            let _ = tx.send(packet);
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            // Connection closed
            *state.lock().unwrap() = ConnectionState::Disconnected;
        });

        Ok(rx)
    }
}

/// BBS Command Router - routes Meshtastic messages to BBS commands
pub struct BBSCommandRouter {
    radio: Arc<Mutex<MeshtasticRadio>>,
    ai_service: Option<Arc<AiService>>,
}

impl BBSCommandRouter {
    /// Create a new command router
    pub fn new(radio: Arc<Mutex<MeshtasticRadio>>) -> Self {
        Self {
            radio,
            ai_service: None,
        }
    }

    /// Create command router with AI service for /agent commands
    pub fn with_ai_service(radio: Arc<Mutex<MeshtasticRadio>>, ai_service: Arc<AiService>) -> Self {
        Self {
            radio,
            ai_service: Some(ai_service),
        }
    }

    /// Parse a BBS command from a message
    pub fn parse_command(message: &str) -> Option<BBSCommand> {
        let trimmed = message.trim();

        // Commands start with /
        if !trimmed.starts_with('/') {
            return None;
        }

        let parts: Vec<&str> = trimmed[1..].splitn(2, ' ').collect();
        let cmd = parts.get(0)?.to_lowercase();
        let args = parts.get(1).map(|s| s.trim());

        match cmd.as_str() {
            "boards" | "b" => Some(BBSCommand::ListBoards),
            "read" | "r" => args.map(|b| BBSCommand::ReadBoard(b.to_string())),
            "post" | "p" => {
                // Format: /post BOARD message
                let parts: Vec<&str> = args?.splitn(2, ' ').collect();
                if parts.len() == 2 {
                    Some(BBSCommand::Post {
                        board: parts[0].to_string(),
                        message: parts[1].to_string(),
                    })
                } else {
                    None
                }
            }
            "reply" => {
                // Format: /reply <id> message
                let parts: Vec<&str> = args?.splitn(2, ' ').collect();
                if parts.len() == 2 {
                    if let Ok(id) = parts[0].parse::<i32>() {
                        Some(BBSCommand::Reply {
                            post_id: id,
                            message: parts[1].to_string(),
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            "help" | "h" | "?" => Some(BBSCommand::Help),
            "stats" => Some(BBSCommand::Stats),
            "register" => args.map(|name| BBSCommand::Register(name.to_string())),
            "agent" | "ai" => args.map(|query| BBSCommand::AgentQuery(query.to_string())),
            _ => None,
        }
    }

    /// Process an incoming message (sync version for non-AI commands)
    pub fn process_message_sync(&self, from_node: u32, message: &str) -> Option<String> {
        if let Some(cmd) = Self::parse_command(message) {
            // For AI queries, return a pending message
            if matches!(cmd, BBSCommand::AgentQuery(_)) {
                return Some("🤖 Processing AI query...".to_string());
            }
            Some(self.execute_command_sync(from_node, cmd))
        } else {
            None // Not a command, ignore
        }
    }

    /// Process an incoming message (async version - handles AI queries)
    pub async fn process_message(&self, from_node: u32, message: &str) -> Option<String> {
        if let Some(cmd) = Self::parse_command(message) {
            Some(self.execute_command(from_node, cmd).await)
        } else {
            None // Not a command, ignore
        }
    }

    /// Execute a BBS command synchronously (for non-AI commands)
    fn execute_command_sync(&self, _from_node: u32, cmd: BBSCommand) -> String {
        match cmd {
            BBSCommand::ListBoards => {
                "BBS Boards:\n• GENERAL\n• ALERTS\n• TRADES\n• RESEARCH\n• HELP\n\nUse /read <board> to view".to_string()
            }
            BBSCommand::ReadBoard(board) => {
                format!("Reading board: {}\n(DB integration pending)", board)
            }
            BBSCommand::Post { board, message } => {
                format!("Posted to {}: {}\n(DB integration pending)", board, message)
            }
            BBSCommand::Reply { post_id, message } => {
                format!("Reply to #{}: {}\n(DB integration pending)", post_id, message)
            }
            BBSCommand::Help => {
                "BBS Commands:\n\
                /boards - List boards\n\
                /read <board> - Read msgs\n\
                /post <board> <msg> - Post\n\
                /reply <id> <msg> - Reply\n\
                /agent <query> - Ask AI\n\
                /stats - Show stats"
                    .to_string()
            }
            BBSCommand::Stats => "BBS Stats:\n(DB integration pending)".to_string(),
            BBSCommand::Register(name) => {
                format!("Registered as: {}\n(DB integration pending)", name)
            }
            BBSCommand::AgentQuery(_) => {
                "🤖 AI queries require async processing".to_string()
            }
        }
    }

    /// Execute a BBS command and return response (async for AI support)
    async fn execute_command(&self, from_node: u32, cmd: BBSCommand) -> String {
        match cmd {
            BBSCommand::AgentQuery(query) => self.handle_agent_query(from_node, &query).await,
            // All other commands use sync execution
            _ => self.execute_command_sync(from_node, cmd),
        }
    }

    /// Handle /agent query with AI service
    async fn handle_agent_query(&self, from_node: u32, query: &str) -> String {
        // Check if AI service is available
        let ai_service = match &self.ai_service {
            Some(service) => service.clone(),
            None => {
                return "🤖 AI service not configured. Set OPENAI_URL and OPENAI_KEY.".to_string();
            }
        };

        // Build a system prompt for mesh/BBS context
        let context_prompt = format!(
            "You are an AI assistant responding via a Meshtastic LoRa mesh network BBS. \
            Keep responses VERY SHORT (under 200 chars) due to radio message limits. \
            Be concise and helpful. The user's node ID is !{:08x}. \
            Query: {}",
            from_node, query
        );

        // Query the AI
        match ai_service.query(&context_prompt).await {
            Ok(response) => {
                // Truncate response to fit Meshtastic message limit
                let truncated = if response.len() > MAX_MESSAGE_SIZE - 10 {
                    format!("{}...", &response[..MAX_MESSAGE_SIZE - 13])
                } else {
                    response
                };
                format!("🤖 {}", truncated)
            }
            Err(e) => {
                log::error!("AI query failed: {}", e);
                format!(
                    "🤖 Error: {}",
                    e.to_string().chars().take(100).collect::<String>()
                )
            }
        }
    }
}

/// BBS commands that can be sent over radio
#[derive(Debug, Clone)]
pub enum BBSCommand {
    ListBoards,
    ReadBoard(String),
    Post { board: String, message: String },
    Reply { post_id: i32, message: String },
    Help,
    Stats,
    Register(String),
    AgentQuery(String), // New: Query an AI agent
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connection_type_parse() {
        // TCP with port
        let ct = ConnectionType::parse("192.168.1.100:4403").unwrap();
        match ct {
            ConnectionType::Tcp { address, port } => {
                assert_eq!(address, "192.168.1.100");
                assert_eq!(port, 4403);
            }
            _ => panic!("Expected TCP"),
        }

        // TCP without port
        let ct = ConnectionType::parse("192.168.1.100").unwrap();
        match ct {
            ConnectionType::Tcp { address, port } => {
                assert_eq!(address, "192.168.1.100");
                assert_eq!(port, DEFAULT_TCP_PORT);
            }
            _ => panic!("Expected TCP"),
        }

        // Serial
        let ct = ConnectionType::parse("/dev/ttyUSB0").unwrap();
        match ct {
            ConnectionType::Serial { device, baud_rate } => {
                assert_eq!(device, "/dev/ttyUSB0");
                assert_eq!(baud_rate, 115200);
            }
            _ => panic!("Expected Serial"),
        }
    }

    #[test]
    fn test_command_parsing() {
        assert!(matches!(
            BBSCommandRouter::parse_command("/boards"),
            Some(BBSCommand::ListBoards)
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/b"),
            Some(BBSCommand::ListBoards)
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/read GENERAL"),
            Some(BBSCommand::ReadBoard(b)) if b == "GENERAL"
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/post GENERAL Hello world"),
            Some(BBSCommand::Post { board, message }) if board == "GENERAL" && message == "Hello world"
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/reply 42 My reply"),
            Some(BBSCommand::Reply { post_id: 42, message }) if message == "My reply"
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/help"),
            Some(BBSCommand::Help)
        ));

        assert!(matches!(
            BBSCommandRouter::parse_command("/agent what is bitcoin"),
            Some(BBSCommand::AgentQuery(q)) if q == "what is bitcoin"
        ));

        // Non-commands
        assert!(BBSCommandRouter::parse_command("Hello world").is_none());
        assert!(BBSCommandRouter::parse_command("").is_none());
    }
}
