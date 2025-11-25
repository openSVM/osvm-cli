//! Meshtastic Radio Integration for BBS
//!
//! This module provides connectivity to Meshtastic radios for off-grid
//! agent-human communication over LoRa mesh networks.
//!
//! Supported connection types:
//! - TCP: Connect to radio via IP address (e.g., 192.168.1.100:4403)
//! - Serial: Connect via USB serial port (e.g., /dev/ttyUSB0)
//!
//! Protocol: Meshtastic uses Protocol Buffers for message encoding.
//! Text messages use PortNum::TEXT_MESSAGE_APP (1).

use std::io::{Read, Write};
use std::net::TcpStream;
use std::sync::{Arc, Mutex};
use std::time::Duration;

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

/// Meshtastic radio connection
pub struct MeshtasticRadio {
    connection_type: ConnectionType,
    state: Arc<Mutex<ConnectionState>>,
    tcp_stream: Option<TcpStream>,
    our_node_id: u32,
    message_callback: Option<MessageCallback>,
}

impl MeshtasticRadio {
    /// Create a new radio connection (not yet connected)
    pub fn new(connection_type: ConnectionType) -> Self {
        Self {
            connection_type,
            state: Arc::new(Mutex::new(ConnectionState::Disconnected)),
            tcp_stream: None,
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

    /// Connect to the radio
    pub fn connect(&mut self) -> Result<(), String> {
        *self.state.lock().unwrap() = ConnectionState::Connecting;

        // Clone connection info to avoid borrow issues
        let conn_type = self.connection_type.clone();
        match conn_type {
            ConnectionType::Tcp { address, port } => {
                self.connect_tcp(&address, port)
            }
            ConnectionType::Serial { device, baud_rate } => {
                self.connect_serial(&device, baud_rate)
            }
        }
    }

    /// Connect via TCP
    fn connect_tcp(&mut self, address: &str, port: u16) -> Result<(), String> {
        let addr = format!("{}:{}", address, port);

        match TcpStream::connect_timeout(
            &addr.parse().map_err(|e| format!("Invalid address: {}", e))?,
            Duration::from_secs(10),
        ) {
            Ok(stream) => {
                stream.set_read_timeout(Some(Duration::from_millis(100)))
                    .map_err(|e| format!("Failed to set timeout: {}", e))?;
                stream.set_nodelay(true)
                    .map_err(|e| format!("Failed to set nodelay: {}", e))?;

                self.tcp_stream = Some(stream);
                *self.state.lock().unwrap() = ConnectionState::Connected;

                // TODO: Send config request to get our node ID
                // This requires implementing the protobuf protocol

                Ok(())
            }
            Err(e) => {
                let err = format!("TCP connection failed: {}", e);
                *self.state.lock().unwrap() = ConnectionState::Error(err.clone());
                Err(err)
            }
        }
    }

    /// Connect via serial port
    fn connect_serial(&mut self, _device: &str, _baud_rate: u32) -> Result<(), String> {
        // Serial connection requires the `serialport` crate
        // For now, return an error indicating it's not implemented
        let err = "Serial connection not yet implemented. Use TCP instead.".to_string();
        *self.state.lock().unwrap() = ConnectionState::Error(err.clone());
        Err(err)
    }

    /// Disconnect from the radio
    pub fn disconnect(&mut self) {
        self.tcp_stream = None;
        *self.state.lock().unwrap() = ConnectionState::Disconnected;
    }

    /// Send a text message
    pub fn send_text(&mut self, message: &str, to: Option<u32>) -> Result<(), String> {
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

        // TODO: Implement protobuf encoding and send
        // This requires the meshtastic protobuf definitions
        //
        // The packet structure is:
        // 1. Start byte (0x94)
        // 2. Length (2 bytes, little-endian)
        // 3. Protobuf payload (ToRadio message)
        //
        // ToRadio contains a MeshPacket with:
        // - from: our node ID
        // - to: destination (0xFFFFFFFF for broadcast)
        // - decoded: Data message with portnum = TEXT_MESSAGE_APP

        Err("Message sending not yet implemented - requires protobuf encoding".to_string())
    }

    /// Poll for incoming messages (non-blocking)
    pub fn poll(&mut self) -> Result<Option<MeshtasticPacket>, String> {
        if self.state() != ConnectionState::Connected {
            return Err("Not connected".to_string());
        }

        // TODO: Implement protobuf decoding
        // Read from stream, decode FromRadio messages

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

/// BBS Command Router - routes Meshtastic messages to BBS commands
pub struct BBSCommandRouter {
    radio: Arc<Mutex<MeshtasticRadio>>,
}

impl BBSCommandRouter {
    /// Create a new command router
    pub fn new(radio: Arc<Mutex<MeshtasticRadio>>) -> Self {
        Self { radio }
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
            _ => None,
        }
    }

    /// Process an incoming message
    pub fn process_message(&self, from_node: u32, message: &str) -> Option<String> {
        if let Some(cmd) = Self::parse_command(message) {
            Some(self.execute_command(from_node, cmd))
        } else {
            None // Not a command, ignore
        }
    }

    /// Execute a BBS command and return response
    fn execute_command(&self, _from_node: u32, cmd: BBSCommand) -> String {
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
                /read <board> - Read messages\n\
                /post <board> <msg> - Post\n\
                /reply <id> <msg> - Reply\n\
                /stats - Show stats\n\
                /register <name> - Register"
                    .to_string()
            }
            BBSCommand::Stats => "BBS Stats:\n(DB integration pending)".to_string(),
            BBSCommand::Register(name) => {
                format!("Registered as: {}\n(DB integration pending)", name)
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

        // Non-commands
        assert!(BBSCommandRouter::parse_command("Hello world").is_none());
        assert!(BBSCommandRouter::parse_command("").is_none());
    }
}
