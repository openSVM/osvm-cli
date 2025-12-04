use anyhow::Result;
use clap::Parser;
use std::sync::Arc;

use crate::services::{
    stream_server::{start_server, StreamServerConfig},
    stream_service::{EventFilter, StreamService},
};
use crate::utils::program_aliases::{
    list_program_aliases, list_token_symbols, resolve_programs, resolve_token,
};

#[derive(Parser, Debug)]
#[command(name = "stream")]
#[command(about = "Start real-time event streaming server", long_about = None)]
pub struct StreamCommand {
    /// RPC URL to connect to
    #[arg(long, default_value = "https://api.mainnet-beta.solana.com")]
    pub rpc_url: String,

    /// Server host to bind to
    #[arg(long, default_value = "127.0.0.1")]
    pub host: String,

    /// Server port to bind to
    #[arg(long, short = 'p', default_value = "8080")]
    pub port: u16,

    /// Enable WebSocket streaming
    #[arg(long, default_value_t = true)]
    pub websocket: bool,

    /// Enable Server-Sent Events (SSE) streaming
    #[arg(long, default_value_t = true)]
    pub sse: bool,

    /// Enable HTTP polling endpoints
    #[arg(long, default_value_t = true)]
    pub http: bool,

    /// Filter by program IDs or aliases (comma-separated, e.g. "pumpfun,raydium,jupiter")
    #[arg(long)]
    pub programs: Option<String>,

    /// Filter by account addresses (comma-separated)
    #[arg(long)]
    pub accounts: Option<String>,

    /// Filter by token symbols or mint addresses (comma-separated, e.g. "USDC,BONK,SOL")
    #[arg(long)]
    pub tokens: Option<String>,

    /// Filter by liquidity pools (comma-separated, e.g. "SOL-USDC,BONK-SOL")
    #[arg(long)]
    pub pools: Option<String>,

    /// Filter by event types (comma-separated: transaction,account_update,log_message,token_transfer,program_invocation,slot_update)
    #[arg(long)]
    pub event_types: Option<String>,

    /// Only stream successful transactions
    #[arg(long)]
    pub success_only: bool,

    /// Minimum transaction fee in lamports
    #[arg(long)]
    pub min_fee: Option<u64>,

    /// List all available program aliases
    #[arg(long)]
    pub list_programs: bool,

    /// List all available token symbols
    #[arg(long)]
    pub list_tokens: bool,
}

pub async fn execute(cmd: StreamCommand) -> Result<()> {
    // Handle list commands first
    if cmd.list_programs {
        println!("📋 Available Program Aliases:");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        for (alias, program_id) in list_program_aliases() {
            println!("  {:20} → {}", alias, program_id);
        }
        return Ok(());
    }

    if cmd.list_tokens {
        println!("💰 Available Token Symbols:");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        for (symbol, mint) in list_token_symbols() {
            println!("  {:10} → {}", symbol, mint);
        }
        return Ok(());
    }

    println!("🚀 Starting OSVM Streaming Server");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("RPC URL: {}", cmd.rpc_url);
    println!("Server: {}:{}", cmd.host, cmd.port);
    println!();

    // Create stream service
    let stream_service = Arc::new(StreamService::new(cmd.rpc_url.clone()));

    // Resolve program aliases to IDs
    let resolved_programs = cmd.programs.as_ref().map(|p| resolve_programs(p));

    // Resolve token symbols to mint addresses
    let resolved_tokens = cmd.tokens.as_ref().map(|t| {
        t.split(',')
            .filter_map(|s| {
                let trimmed = s.trim();
                // Try to resolve as symbol, otherwise use as-is (assume it's a mint address)
                resolve_token(trimmed).or_else(|| Some(trimmed.to_string()))
            })
            .collect::<Vec<_>>()
            .join(",")
    });

    // Apply filters if specified
    if resolved_programs.is_some()
        || cmd.accounts.is_some()
        || resolved_tokens.is_some()
        || cmd.pools.is_some()
        || cmd.event_types.is_some()
        || cmd.success_only
        || cmd.min_fee.is_some()
    {
        let filter = EventFilter {
            program_ids: resolved_programs
                .as_ref()
                .map(|p| p.split(',').map(|s| s.trim().to_string()).collect()),
            accounts: cmd
                .accounts
                .as_ref()
                .map(|a| a.split(',').map(|s| s.trim().to_string()).collect()),
            event_types: cmd
                .event_types
                .as_ref()
                .map(|e| e.split(',').map(|s| s.trim().to_string()).collect()),
            min_fee: cmd.min_fee,
            success_only: cmd.success_only,
        };

        stream_service.add_filter(filter);
        println!("📋 Filters applied:");
        if let Some(ref programs) = cmd.programs {
            if let Some(ref resolved) = resolved_programs {
                println!("   Programs: {} → {}", programs, resolved);
            }
        }
        if let Some(ref accounts) = cmd.accounts {
            println!("   Accounts: {}", accounts);
        }
        if let Some(ref tokens) = cmd.tokens {
            if let Some(ref resolved) = resolved_tokens {
                println!("   Tokens: {} → {}", tokens, resolved);
            }
        }
        if let Some(ref pools) = cmd.pools {
            println!("   Pools: {} (pool filtering coming soon)", pools);
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
        println!();
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
    println!();

    println!("🎯 Usage examples:");
    println!();
    println!("List available aliases:");
    println!("  osvm stream --list-programs");
    println!("  osvm stream --list-tokens");
    println!();
    println!("Stream with friendly names:");
    println!("  osvm stream --programs pumpfun");
    println!("  osvm stream --programs \"raydium,orca,jupiter\"");
    println!("  osvm stream --tokens \"USDC,BONK,SOL\"");
    println!("  osvm stream --programs raydium --tokens BONK --success-only");
    println!();
    println!("WebSocket (JavaScript):");
    println!(
        "  const ws = new WebSocket('ws://{}:{}/ws');",
        cmd.host, cmd.port
    );
    println!("  ws.onmessage = (event) => {{");
    println!("    const data = JSON.parse(event.data);");
    println!("    console.log('Event:', data);");
    println!("  }};");
    println!();
    println!("SSE (JavaScript):");
    println!(
        "  const eventSource = new EventSource('http://{}:{}/stream');",
        cmd.host, cmd.port
    );
    println!("  eventSource.onmessage = (event) => {{");
    println!("    const data = JSON.parse(event.data);");
    println!("    console.log('Event:', data);");
    println!("  }};");
    println!();
    println!("HTTP Polling (curl):");
    println!("  curl http://{}:{}/events?limit=10", cmd.host, cmd.port);
    println!();
    println!("Set filter (curl):");
    println!("  curl -X POST http://{}:{}/filter \\", cmd.host, cmd.port);
    println!("    -H 'Content-Type: application/json' \\");
    println!("    -d '{{\"success_only\": true, \"event_types\": [\"transaction\"]}}'");
    println!();

    println!("✨ Server starting...");
    println!();

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
