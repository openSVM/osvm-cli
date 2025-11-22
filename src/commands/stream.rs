use anyhow::Result;
use clap::Parser;
use std::sync::Arc;

use crate::services::{
    stream_server::{start_server, StreamServerConfig},
    stream_service::{EventFilter, StreamService},
};

#[derive(Parser, Debug)]
#[command(name = "stream")]
#[command(about = "Start real-time event streaming server", long_about = None)]
pub struct StreamCommand {
    /// RPC URL to connect to
    #[arg(long, env = "SOLANA_RPC_URL", default_value = "https://api.mainnet-beta.solana.com")]
    pub rpc_url: String,

    /// Server host to bind to
    #[arg(long, default_value = "127.0.0.1")]
    pub host: String,

    /// Server port to bind to
    #[arg(long, short = 'p', default_value = "8080")]
    pub port: u16,

    /// Enable WebSocket streaming
    #[arg(long, default_value = "true")]
    pub websocket: bool,

    /// Enable Server-Sent Events (SSE) streaming
    #[arg(long, default_value = "true")]
    pub sse: bool,

    /// Enable HTTP polling endpoints
    #[arg(long, default_value = "true")]
    pub http: bool,

    /// Filter by program IDs (comma-separated)
    #[arg(long)]
    pub programs: Option<String>,

    /// Filter by account addresses (comma-separated)
    #[arg(long)]
    pub accounts: Option<String>,

    /// Filter by event types (comma-separated: transaction,account_update,log_message,token_transfer,program_invocation,slot_update)
    #[arg(long)]
    pub event_types: Option<String>,

    /// Only stream successful transactions
    #[arg(long)]
    pub success_only: bool,

    /// Minimum transaction fee in lamports
    #[arg(long)]
    pub min_fee: Option<u64>,
}

pub async fn execute(cmd: StreamCommand) -> Result<()> {
    println!("🚀 Starting OSVM Streaming Server");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("RPC URL: {}", cmd.rpc_url);
    println!("Server: {}:{}", cmd.host, cmd.port);
    println!("");

    // Create stream service
    let stream_service = Arc::new(StreamService::new(cmd.rpc_url.clone()));

    // Apply filters if specified
    if cmd.programs.is_some()
        || cmd.accounts.is_some()
        || cmd.event_types.is_some()
        || cmd.success_only
        || cmd.min_fee.is_some()
    {
        let filter = EventFilter {
            program_ids: cmd.programs.as_ref().map(|p| {
                p.split(',')
                    .map(|s| s.trim().to_string())
                    .collect()
            }),
            accounts: cmd.accounts.as_ref().map(|a| {
                a.split(',')
                    .map(|s| s.trim().to_string())
                    .collect()
            }),
            event_types: cmd.event_types.as_ref().map(|e| {
                e.split(',')
                    .map(|s| s.trim().to_string())
                    .collect()
            }),
            min_fee: cmd.min_fee,
            success_only: cmd.success_only,
        };

        stream_service.add_filter(filter);
        println!("📋 Filters applied:");
        if let Some(ref programs) = cmd.programs {
            println!("   Programs: {}", programs);
        }
        if let Some(ref accounts) = cmd.accounts {
            println!("   Accounts: {}", accounts);
        }
        if let Some(ref event_types) = cmd.event_types {
            println!("   Event types: {}", event_types);
        }
        if cmd.success_only {
            println!("   Success only: true");
        }
        if let Some(min_fee) = cmd.min_fee {
            println!("   Min fee: {} lamports", min_fee);
        }
        println!("");
    }

    println!("📡 Available endpoints:");
    if cmd.websocket {
        println!("   WebSocket:  ws://{}:{}/ws", cmd.host, cmd.port);
    }
    if cmd.sse {
        println!("   SSE Stream: http://{}:{}/stream", cmd.host, cmd.port);
    }
    if cmd.http {
        println!("   HTTP Poll:  http://{}:{}/events", cmd.host, cmd.port);
        println!("   Stats:      http://{}:{}/stats", cmd.host, cmd.port);
        println!("   Health:     http://{}:{}/health", cmd.host, cmd.port);
    }
    println!("");

    println!("🎯 Usage examples:");
    println!("");
    println!("WebSocket (JavaScript):");
    println!("  const ws = new WebSocket('ws://{}:{}/ws');", cmd.host, cmd.port);
    println!("  ws.onmessage = (event) => {{");
    println!("    const data = JSON.parse(event.data);");
    println!("    console.log('Event:', data);");
    println!("  }};");
    println!("");
    println!("SSE (JavaScript):");
    println!("  const eventSource = new EventSource('http://{}:{}/stream');", cmd.host, cmd.port);
    println!("  eventSource.onmessage = (event) => {{");
    println!("    const data = JSON.parse(event.data);");
    println!("    console.log('Event:', data);");
    println!("  }};");
    println!("");
    println!("HTTP Polling (curl):");
    println!("  curl http://{}:{}/events?limit=10", cmd.host, cmd.port);
    println!("");
    println!("Set filter (curl):");
    println!("  curl -X POST http://{}:{}/filter \\", cmd.host, cmd.port);
    println!("    -H 'Content-Type: application/json' \\");
    println!("    -d '{{\"success_only\": true, \"event_types\": [\"transaction\"]}}'");
    println!("");

    println!("✨ Server starting...");
    println!("");

    // Create server config
    let config = StreamServerConfig {
        host: cmd.host,
        port: cmd.port,
        enable_websocket: cmd.websocket,
        enable_sse: cmd.sse,
        enable_http: cmd.http,
    };

    // Start the server
    start_server(config, stream_service).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stream_command_parsing() {
        let cmd = StreamCommand::parse_from(&[
            "stream",
            "--rpc-url",
            "https://api.devnet.solana.com",
            "--port",
            "9090",
            "--success-only",
        ]);

        assert_eq!(cmd.rpc_url, "https://api.devnet.solana.com");
        assert_eq!(cmd.port, 9090);
        assert!(cmd.success_only);
    }
}
