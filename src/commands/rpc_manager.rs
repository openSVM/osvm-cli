//! RPC command handlers
//!
//! This module handles all rpc subcommands including:
//! - sonic: Deploy Sonic RPC via SSH
//! - query-solana: Query Solana network
//! - local: Start local RPC
//! - devnet: Start devnet validator
//! - test: Start test validator

use crate::utils::ssh_deploy;
use anyhow::Result;
use std::process::exit;

/// Handle all rpc commands
pub async fn handle_rpc_manager(matches: &clap::ArgMatches) -> Result<()> {
    let Some((rpc_sub_command, rpc_sub_matches)) = matches.subcommand() else {
        eprintln!("No RPC subcommand provided");
        exit(1);
    };

    match rpc_sub_command {
        "solana" => handle_solana(rpc_sub_matches).await,
        "devnet" => handle_devnet(rpc_sub_matches).await,
        "sonic" => handle_sonic(rpc_sub_matches).await,
        "query-solana" => handle_query_solana(rpc_sub_matches).await,
        "local" => handle_local(rpc_sub_matches).await,
        "test" => handle_test(rpc_sub_matches).await,
        _ => {
            eprintln!("Unknown RPC subcommand: {}", rpc_sub_command);
            exit(1);
        }
    }
}

/// Handle devnet RPC command
async fn handle_devnet(matches: &clap::ArgMatches) -> Result<()> {
    use crate::utils::devnet_rpc::{start_devnet_rpc, DevnetRpcConfig};

    let ledger_path = matches
        .get_one::<String>("ledger-path")
        .map(|s| s.as_str())
        .unwrap_or("devnet-ledger");
    let rpc_port = matches
        .get_one::<String>("rpc-port")
        .and_then(|s| s.parse::<u16>().ok())
        .unwrap_or(8899);
    let gossip_port = 8001u16; // Gossip port not in clparse, using default
    let background = matches.get_flag("background");

    let config = DevnetRpcConfig {
        ledger_path: ledger_path.to_string(),
        rpc_port,
        gossip_port,
        background,
    };

    match start_devnet_rpc(config).await {
        Ok(_info) => {
            println!("✅ Devnet RPC started successfully");
            Ok(())
        }
        Err(e) => {
            eprintln!("❌ Failed to start devnet RPC: {}", e);
            exit(1);
        }
    }
}

/// Handle Solana RPC deployment via SSH
async fn handle_solana(matches: &clap::ArgMatches) -> Result<()> {
    let connection_str = matches
        .get_one::<String>("connection")
        .map(|s| s.as_str())
        .unwrap();
    let network_str = matches
        .get_one::<String>("network")
        .map(|s| s.as_str())
        .unwrap();
    let version = matches.get_one::<String>("version").map(|s| s.as_str());
    let client_type = matches.get_one::<String>("client-type").map(|s| s.as_str());
    let hot_swap_enabled = matches.get_flag("hot-swap");

    // Parse connection string
    let connection = match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
        Ok(conn) => conn,
        Err(e) => {
            eprintln!("Error parsing SSH connection string: {}", e);
            exit(1);
        }
    };

    // Parse network type
    let network = match network_str.to_lowercase().as_str() {
        "mainnet" => ssh_deploy::NetworkType::Mainnet,
        "testnet" => ssh_deploy::NetworkType::Testnet,
        "devnet" => ssh_deploy::NetworkType::Devnet,
        _ => {
            eprintln!("Invalid network: {}", network_str);
            exit(1);
        }
    };

    // Create deployment config
    let deploy_config = ssh_deploy::DeploymentConfig {
        svm_type: "solana".to_string(),
        node_type: "rpc".to_string(),
        network,
        node_name: "solana-rpc".to_string(),
        rpc_url: None,
        additional_params: std::collections::HashMap::new(),
        version: version.map(|v| v.to_string()),
        client_type: client_type.map(|c| c.to_string()),
        hot_swap_enabled,
        metrics_config: None,
        disk_config: None,
    };

    println!(
        "🚀 Deploying Solana {} RPC node to {}...",
        network_str, connection_str
    );
    if let Some(ver) = version {
        println!("📦 Version: {}", ver);
    }
    if let Some(client) = client_type {
        println!("🔧 Client: {}", client);
    }
    if hot_swap_enabled {
        println!("♨️  Hot-swap: enabled (zero-downtime updates)");
    }
    println!();

    if let Err(e) = ssh_deploy::deploy_svm_node(connection, deploy_config, None).await {
        eprintln!("❌ Deployment error: {}", e);
        exit(1);
    }

    println!("✅ Solana RPC node deployed successfully!");
    println!(
        "🔗 Your RPC endpoint: http://{}:8899",
        connection_str.split('@').nth(1).unwrap_or(connection_str)
    );
    Ok(())
}

/// Handle sonic RPC deployment
async fn handle_sonic(matches: &clap::ArgMatches) -> Result<()> {
    let connection_str = matches
        .get_one::<String>("connection")
        .map(|s| s.as_str())
        .unwrap();
    let network_str = matches
        .get_one::<String>("network")
        .map(|s| s.as_str())
        .unwrap();

    // Parse connection string
    let connection = match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
        Ok(conn) => conn,
        Err(e) => {
            eprintln!("Error parsing SSH connection string: {}", e);
            exit(1);
        }
    };

    // Parse network type
    let network = match network_str.to_lowercase().as_str() {
        "mainnet" => ssh_deploy::NetworkType::Mainnet,
        "testnet" => ssh_deploy::NetworkType::Testnet,
        "devnet" => ssh_deploy::NetworkType::Devnet,
        _ => {
            eprintln!("Invalid network: {}", network_str);
            exit(1);
        }
    };

    // Create deployment config
    let deploy_config = ssh_deploy::DeploymentConfig {
        svm_type: "sonic".to_string(),
        node_type: "rpc".to_string(),
        network,
        node_name: "sonic-rpc".to_string(),
        rpc_url: None,
        additional_params: std::collections::HashMap::new(),
        version: None,
        client_type: None,
        hot_swap_enabled: false,
        metrics_config: None,
        disk_config: None,
    };

    println!("Deploying Sonic RPC node to {}...", connection_str);

    if let Err(e) = ssh_deploy::deploy_svm_node(connection, deploy_config, None).await {
        eprintln!("Deployment error: {}", e);
        exit(1);
    }

    println!("Sonic RPC node deployed successfully!");
    Ok(())
}

/// Handle Solana network queries
async fn handle_query_solana(matches: &clap::ArgMatches) -> Result<()> {
    let network = matches
        .get_one::<String>("network")
        .map(|s| s.as_str())
        .unwrap_or("mainnet");
    let custom_url = matches.get_one::<String>("custom-url").map(|s| s.as_str());
    let monitor = matches.get_flag("monitor");
    let health = matches.get_flag("health");

    if monitor {
        // Monitor network activity
        match crate::utils::solana_rpc::monitor_network(network, custom_url).await {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("❌ Error monitoring network: {}", e);
                exit(1);
            }
        }
    } else if health {
        // Check network health
        match crate::utils::solana_rpc::check_network_health(network, custom_url).await {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("❌ Error checking network health: {}", e);
                exit(1);
            }
        }
    } else {
        // Display network info
        match crate::utils::solana_rpc::show_network_info(network, custom_url).await {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("❌ Error getting network info: {}", e);
                exit(1);
            }
        }
    }
}

/// Handle local RPC
async fn handle_local(_matches: &clap::ArgMatches) -> Result<()> {
    println!("🚀 Starting local RPC server...");
    println!("💡 Local RPC implementation coming soon");
    Ok(())
}

/// Handle test validator
async fn handle_test(_matches: &clap::ArgMatches) -> Result<()> {
    println!("🚀 Starting test validator...");
    println!("💡 Test validator implementation coming soon");
    Ok(())
}
