//! Solana RPC connectivity and monitoring
//!
//! This module provides functionality to connect to Solana's official RPC endpoints
//! and monitor network activity.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use solana_client::rpc_client::RpcClient;
use solana_commitment_config::CommitmentConfig;
use solana_sdk::clock::Slot;
use std::time::Duration;
use tokio::time::{interval, sleep};

/// Solana network endpoints
pub struct SolanaNetworks;

impl SolanaNetworks {
    pub const MAINNET: &'static str = "https://api.mainnet-beta.solana.com";
    pub const TESTNET: &'static str = "https://api.testnet.solana.com";
    pub const DEVNET: &'static str = "https://api.devnet.solana.com";

    pub fn get_rpc_url(network: &str) -> &'static str {
        match network.to_lowercase().as_str() {
            "mainnet" | "mainnet-beta" => Self::MAINNET,
            "testnet" => Self::TESTNET,
            "devnet" => Self::DEVNET,
            _ => Self::MAINNET, // Default to mainnet
        }
    }

    pub fn get_websocket_url(network: &str) -> &'static str {
        match network.to_lowercase().as_str() {
            "mainnet" | "mainnet-beta" => "wss://api.mainnet-beta.solana.com/",
            "testnet" => "wss://api.testnet.solana.com/",
            "devnet" => "wss://api.devnet.solana.com/",
            _ => "wss://api.mainnet-beta.solana.com/", // Default to mainnet
        }
    }
}

/// Network health information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkHealth {
    pub network: String,
    pub rpc_url: String,
    pub healthy: bool,
    pub slot_height: Option<Slot>,
    pub epoch: Option<u64>,
    pub block_time: Option<u64>,
    pub transaction_count: Option<u64>,
    pub response_time_ms: Option<u64>,
    pub validator_count: Option<usize>,
    pub voting_validators: Option<usize>,
    pub stake_percent: Option<f64>,
}

/// Real-time network statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkStats {
    pub slot: Slot,
    pub confirmed_slot: Slot,
    pub finalized_slot: Slot,
    pub epoch: u64,
    pub epoch_progress: f64,
    pub transactions: u64,
    pub tps: f64,
    pub block_time: Option<u64>,
    pub leader: Option<String>,
}

/// Connect to Solana RPC and get network health
pub async fn check_network_health(
    network: &str,
    custom_url: Option<&str>,
) -> Result<NetworkHealth> {
    let rpc_url = custom_url.unwrap_or_else(|| SolanaNetworks::get_rpc_url(network));
    let client = RpcClient::new(rpc_url.to_string());

    let start_time = std::time::Instant::now();

    // Check if RPC is responsive
    let health_result = client.get_health();
    let response_time = start_time.elapsed().as_millis() as u64;

    match health_result {
        Ok(_) => {
            // Get additional network information
            let slot_height = client.get_slot().ok();
            let epoch_info = client.get_epoch_info().ok();
            let block_time = if let Some(slot) = slot_height {
                client.get_block_time(slot).ok().and_then(|t| Some(t))
            } else {
                None
            };

            // Get validator information
            let vote_accounts = client.get_vote_accounts().ok();
            let (validator_count, voting_validators, stake_percent) =
                if let Some(accounts) = vote_accounts {
                    let total_validators = accounts.current.len() + accounts.delinquent.len();
                    let voting = accounts.current.len();
                    let total_stake: u64 = accounts.current.iter().map(|v| v.activated_stake).sum();
                    let stake_pct = if total_stake > 0 {
                        (total_stake as f64 / 1_000_000_000_000_000.0) * 100.0 // Convert lamports to SOL percentage approximation
                    } else {
                        0.0
                    };
                    (Some(total_validators), Some(voting), Some(stake_pct))
                } else {
                    (None, None, None)
                };

            Ok(NetworkHealth {
                network: network.to_string(),
                rpc_url: rpc_url.to_string(),
                healthy: true,
                slot_height,
                epoch: epoch_info.as_ref().map(|e| e.epoch),
                block_time: block_time.map(|t| t as u64),
                transaction_count: None, // Would need block parsing for accurate count
                response_time_ms: Some(response_time),
                validator_count,
                voting_validators,
                stake_percent,
            })
        }
        Err(e) => Ok(NetworkHealth {
            network: network.to_string(),
            rpc_url: rpc_url.to_string(),
            healthy: false,
            slot_height: None,
            epoch: None,
            block_time: None,
            transaction_count: None,
            response_time_ms: Some(response_time),
            validator_count: None,
            voting_validators: None,
            stake_percent: None,
        }),
    }
}

/// Monitor network activity in real-time
pub async fn monitor_network(network: &str, custom_url: Option<&str>) -> Result<()> {
    let rpc_url = custom_url.unwrap_or_else(|| SolanaNetworks::get_rpc_url(network));
    let client = RpcClient::new(rpc_url.to_string());

    println!("🔗 Connected to Solana {} Network", network.to_uppercase());
    println!("📡 RPC URL: {}", rpc_url);
    println!("⏱️  Monitoring network activity... (Press Ctrl+C to stop)\n");

    let mut interval = interval(Duration::from_secs(1));
    let mut last_slot = 0u64;
    let mut slot_times = std::collections::VecDeque::new();

    loop {
        interval.tick().await;

        match get_network_stats(&client).await {
            Ok(stats) => {
                // Calculate TPS approximation
                let slot_diff = if stats.slot > last_slot {
                    stats.slot - last_slot
                } else {
                    0
                };

                slot_times.push_back(std::time::Instant::now());
                if slot_times.len() > 10 {
                    // Keep last 10 samples
                    slot_times.pop_front();
                }

                let tps = if slot_diff > 0 {
                    slot_diff as f64 * 400.0
                } else {
                    0.0
                }; // Rough approximation

                // Display current stats
                let time_str = chrono::Local::now().format("%H:%M:%S").to_string();
                println!("{} | Slot: {} | Confirmed: {} | Finalized: {} | Epoch: {} ({:.1}%) | Estimated TPS: {:.0}",
                    time_str,
                    stats.slot,
                    stats.confirmed_slot,
                    stats.finalized_slot,
                    stats.epoch,
                    stats.epoch_progress * 100.0,
                    tps
                );

                last_slot = stats.slot;
            }
            Err(e) => {
                eprintln!("❌ Error getting network stats: {}", e);
                sleep(Duration::from_secs(5)).await; // Wait longer on error
            }
        }
    }
}

/// Get current network statistics
async fn get_network_stats(client: &RpcClient) -> Result<NetworkStats> {
    // Get slot information
    let slot = client.get_slot_with_commitment(CommitmentConfig::processed())?;
    let confirmed_slot = client.get_slot_with_commitment(CommitmentConfig::confirmed())?;
    let finalized_slot = client.get_slot_with_commitment(CommitmentConfig::finalized())?;

    // Get epoch information
    let epoch_info = client.get_epoch_info()?;
    let epoch_progress = epoch_info.slot_index as f64 / epoch_info.slots_in_epoch as f64;

    // Get block time if available
    let block_time = client.get_block_time(slot).ok().and_then(|t| Some(t));

    // Get leader schedule for current slot (if available)
    let leader = client
        .get_slot_leaders(slot, 1)
        .ok()
        .and_then(|leaders| leaders.first().map(|l| l.to_string()));

    Ok(NetworkStats {
        slot,
        confirmed_slot,
        finalized_slot,
        epoch: epoch_info.epoch,
        epoch_progress,
        transactions: 0, // Would need block parsing for accurate count
        tps: 0.0,        // Would need historical data for accurate TPS
        block_time: block_time.map(|t| t as u64),
        leader,
    })
}

/// Display network information
pub async fn show_network_info(network: &str, custom_url: Option<&str>) -> Result<()> {
    let rpc_url = custom_url.unwrap_or_else(|| SolanaNetworks::get_rpc_url(network));

    println!("🌐 Solana {} Network Information", network.to_uppercase());
    println!("================================");
    println!("📡 RPC Endpoint: {}", rpc_url);
    println!(
        "🔗 WebSocket: {}",
        SolanaNetworks::get_websocket_url(network)
    );
    println!();

    // Get and display network health
    match check_network_health(network, custom_url).await {
        Ok(health) => {
            println!(
                "🏥 Network Health: {}",
                if health.healthy {
                    "✅ Healthy"
                } else {
                    "❌ Unhealthy"
                }
            );
            if let Some(response_time) = health.response_time_ms {
                println!("⚡ Response Time: {}ms", response_time);
            }

            if let Some(slot) = health.slot_height {
                println!("📊 Current Slot: {}", slot);
            }

            if let Some(epoch) = health.epoch {
                println!("🗓️  Current Epoch: {}", epoch);
            }

            if let Some(validators) = health.validator_count {
                println!("🏛️  Total Validators: {}", validators);
            }

            if let Some(voting) = health.voting_validators {
                println!("🗳️  Voting Validators: {}", voting);
            }

            if let Some(stake) = health.stake_percent {
                println!("💰 Stake Participation: {:.2}%", stake);
            }
        }
        Err(e) => {
            println!("❌ Failed to get network health: {}", e);
        }
    }

    println!();
    println!("Available commands:");
    println!("  osvm rpc solana --monitor              # Monitor real-time activity");
    println!("  osvm rpc solana --health               # Check network health");
    println!("  osvm rpc solana --network devnet       # Connect to devnet");
    println!("  osvm rpc solana --custom-url <URL>     # Use custom RPC endpoint");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_network_urls() {
        assert_eq!(
            SolanaNetworks::get_rpc_url("mainnet"),
            SolanaNetworks::MAINNET
        );
        assert_eq!(
            SolanaNetworks::get_rpc_url("devnet"),
            SolanaNetworks::DEVNET
        );
        assert_eq!(
            SolanaNetworks::get_rpc_url("testnet"),
            SolanaNetworks::TESTNET
        );
        assert_eq!(
            SolanaNetworks::get_rpc_url("invalid"),
            SolanaNetworks::MAINNET
        ); // Default
    }
}
