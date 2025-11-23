use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use solana_client::rpc_client::RpcClient;
use solana_client::nonblocking::pubsub_client::PubsubClient;
use solana_client::rpc_config::{RpcTransactionLogsConfig, RpcTransactionLogsFilter};
use solana_commitment_config::CommitmentConfig;
use solana_sdk::pubkey::Pubkey;
use solana_sdk::signature::Signature;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use std::str::FromStr;
use tokio::sync::broadcast;
use tokio::time;
use futures_util::StreamExt;

/// Event types that can be streamed
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum SolanaEvent {
    Transaction {
        signature: String,
        slot: u64,
        timestamp: u64,
        success: bool,
        fee: u64,
        signer: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        program_ids: Option<Vec<String>>,
    },
    AccountUpdate {
        pubkey: String,
        slot: u64,
        lamports: u64,
        owner: String,
        data_len: usize,
    },
    LogMessage {
        signature: String,
        logs: Vec<String>,
        slot: u64,
    },
    TokenTransfer {
        signature: String,
        from: String,
        to: String,
        amount: f64,
        token: String,
        decimals: u8,
    },
    ProgramInvocation {
        signature: String,
        program_id: String,
        instruction_data: String,
        accounts: Vec<String>,
    },
    SlotUpdate {
        slot: u64,
        parent: u64,
        timestamp: u64,
    },
}

/// Filter configuration for event streaming
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EventFilter {
    /// Filter by program ID
    pub program_ids: Option<Vec<String>>,
    /// Filter by account addresses
    pub accounts: Option<Vec<String>>,
    /// Filter by event type
    pub event_types: Option<Vec<String>>,
    /// Minimum transaction fee (in lamports)
    pub min_fee: Option<u64>,
    /// Only successful transactions
    pub success_only: bool,
}

impl Default for EventFilter {
    fn default() -> Self {
        Self {
            program_ids: None,
            accounts: None,
            event_types: None,
            min_fee: None,
            success_only: false,
        }
    }
}

impl EventFilter {
    pub fn matches(&self, event: &SolanaEvent) -> bool {
        // Check event type filter
        if let Some(ref types) = self.event_types {
            let event_type = match event {
                SolanaEvent::Transaction { .. } => "transaction",
                SolanaEvent::AccountUpdate { .. } => "account_update",
                SolanaEvent::LogMessage { .. } => "log_message",
                SolanaEvent::TokenTransfer { .. } => "token_transfer",
                SolanaEvent::ProgramInvocation { .. } => "program_invocation",
                SolanaEvent::SlotUpdate { .. } => "slot_update",
            };
            if !types.contains(&event_type.to_string()) {
                return false;
            }
        }

        // Check success_only filter
        if self.success_only {
            if let SolanaEvent::Transaction { success, .. } = event {
                if !success {
                    return false;
                }
            }
        }

        // Check min_fee filter
        if let Some(min_fee) = self.min_fee {
            if let SolanaEvent::Transaction { fee, .. } = event {
                if *fee < min_fee {
                    return false;
                }
            }
        }

        // Check account filter
        if let Some(ref accounts) = self.accounts {
            match event {
                SolanaEvent::Transaction { signer, .. } => {
                    if !accounts.contains(signer) {
                        return false;
                    }
                }
                SolanaEvent::AccountUpdate { pubkey, .. } => {
                    if !accounts.contains(pubkey) {
                        return false;
                    }
                }
                SolanaEvent::TokenTransfer { from, to, .. } => {
                    if !accounts.contains(from) && !accounts.contains(to) {
                        return false;
                    }
                }
                _ => {}
            }
        }

        // Check program_id filter
        if let Some(ref programs) = self.program_ids {
            match event {
                SolanaEvent::Transaction { program_ids: Some(tx_programs), .. } => {
                    // Transaction must involve at least one of the filtered programs
                    if !tx_programs.iter().any(|p| programs.contains(p)) {
                        return false;
                    }
                }
                SolanaEvent::Transaction { program_ids: None, .. } => {
                    // Transaction without program_ids can't be filtered, skip it
                    return false;
                }
                SolanaEvent::ProgramInvocation { program_id, .. } => {
                    if !programs.contains(program_id) {
                        return false;
                    }
                }
                // Allow SlotUpdate, AccountUpdate, and other events through
                // They provide context for the filtered transactions
                _ => {}
            }
        }

        true
    }
}

/// Streaming service that listens to Solana events
pub struct StreamService {
    rpc_url: String,
    tx: broadcast::Sender<SolanaEvent>,
    rx: broadcast::Receiver<SolanaEvent>,
    filters: Arc<Mutex<Vec<EventFilter>>>,
    running: Arc<Mutex<bool>>,
    stats: Arc<Mutex<StreamStats>>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct StreamStats {
    pub events_processed: u64,
    pub events_filtered: u64,
    pub events_sent: u64,
    pub uptime_secs: u64,
    pub connected_clients: usize,
    pub last_slot: u64,
}

impl StreamService {
    pub fn new(rpc_url: String) -> Self {
        let (tx, rx) = broadcast::channel(1000);
        Self {
            rpc_url,
            tx,
            rx,
            filters: Arc::new(Mutex::new(Vec::new())),
            running: Arc::new(Mutex::new(false)),
            stats: Arc::new(Mutex::new(StreamStats::default())),
        }
    }

    /// Subscribe to the event stream
    pub fn subscribe(&self) -> broadcast::Receiver<SolanaEvent> {
        let mut stats = self.stats.lock().unwrap();
        stats.connected_clients += 1;
        self.tx.subscribe()
    }

    /// Add a filter to the stream
    pub fn add_filter(&self, filter: EventFilter) {
        self.filters.lock().unwrap().push(filter);
    }

    /// Clear all filters
    pub fn clear_filters(&self) {
        self.filters.lock().unwrap().clear();
    }

    /// Get current streaming statistics
    pub fn get_stats(&self) -> StreamStats {
        self.stats.lock().unwrap().clone()
    }

    /// Start the streaming service
    pub async fn start(&self) -> Result<()> {
        *self.running.lock().unwrap() = true;

        let rpc_url = self.rpc_url.clone();
        let tx = self.tx.clone();
        let filters = Arc::clone(&self.filters);
        let running = Arc::clone(&self.running);
        let stats = Arc::clone(&self.stats);

        // Check if we have program filters - if so, use WebSocket subscriptions
        let has_program_filters = {
            let filters_vec = filters.lock().unwrap();
            filters_vec.iter().any(|f| f.program_ids.is_some())
        };

        if has_program_filters {
            // Use WebSocket logsSubscribe for program-specific streaming
            tracing::info!("Using WebSocket programSubscribe for program-specific streaming");
            tokio::spawn(async move {
                if let Err(e) = Self::subscribe_to_programs(&rpc_url, &tx, &filters, &running, &stats).await {
                    tracing::error!("Program subscription error: {}", e);
                }
            });
        } else {
            // Use HTTP polling for general block streaming
            tracing::info!("Using HTTP polling for general block streaming");
            tokio::spawn(async move {
                let start_time = SystemTime::now();

                loop {
                    if !*running.lock().unwrap() {
                        break;
                    }

                    // Update uptime
                    if let Ok(elapsed) = start_time.elapsed() {
                        stats.lock().unwrap().uptime_secs = elapsed.as_secs();
                    }

                    // Poll for new events
                    if let Err(e) = Self::poll_events(&rpc_url, &tx, &filters, &stats).await {
                        tracing::error!("Error polling events: {}", e);
                    }

                    // Sleep to avoid overwhelming the RPC
                    time::sleep(Duration::from_millis(100)).await;
                }
            });
        }

        Ok(())
    }

    /// Stop the streaming service
    pub fn stop(&self) {
        *self.running.lock().unwrap() = false;
    }

    /// Subscribe to program-specific events using WebSocket
    async fn subscribe_to_programs(
        rpc_url: &str,
        tx: &broadcast::Sender<SolanaEvent>,
        filters: &Arc<Mutex<Vec<EventFilter>>>,
        running: &Arc<Mutex<bool>>,
        stats: &Arc<Mutex<StreamStats>>,
    ) -> Result<()> {
        // Convert HTTP URL to WebSocket URL
        let ws_url = rpc_url.replace("https://", "wss://").replace("http://", "ws://");

        // Get all program IDs from filters
        let program_ids: Vec<String> = {
            let filters_vec = filters.lock().unwrap();
            filters_vec.iter()
                .filter_map(|f| f.program_ids.as_ref())
                .flatten()
                .cloned()
                .collect()
        };

        tracing::info!("Subscribing to {} programs via WebSocket: {}", program_ids.len(), ws_url);

        // Subscribe to logs for each program
        for program_id in program_ids {
            tracing::info!("Subscribing to program: {}", program_id);

            let tx_clone = tx.clone();
            let running_clone = Arc::clone(running);
            let stats_clone = Arc::clone(stats);
            let filters_clone = Arc::clone(filters);
            let ws_url_clone = ws_url.clone();

            tokio::spawn(async move {
                // Create new PubsubClient for each subscription
                let pubsub = match PubsubClient::new(&ws_url_clone).await {
                    Ok(client) => client,
                    Err(e) => {
                        tracing::error!("Failed to connect to WebSocket for {}: {}", program_id, e);
                        return;
                    }
                };

                let config = RpcTransactionLogsConfig {
                    commitment: Some(CommitmentConfig::confirmed()),
                };

                let filter = RpcTransactionLogsFilter::Mentions(vec![program_id.clone()]);

                let (mut stream, unsubscribe) = match pubsub.logs_subscribe(filter, config).await {
                    Ok(result) => result,
                    Err(e) => {
                        tracing::error!("Failed to subscribe to program {}: {}", program_id, e);
                        return;
                    }
                };

                tracing::info!("Successfully subscribed to program: {}", program_id);

                // Keep pubsub and unsubscribe alive for the duration
                while *running_clone.lock().unwrap() {
                    match stream.next().await {
                        Some(response) => {
                            // Process the log response
                            let logs = response.value.logs;
                            let signature = response.value.signature;

                            // Create log message event
                            let event = SolanaEvent::LogMessage {
                                signature: signature.clone(),
                                logs: logs.clone(),
                                slot: 0, // Slot not provided in logs_subscribe
                            };

                            stats_clone.lock().unwrap().events_processed += 1;

                            // Check filters
                            let filters_vec = filters_clone.lock().unwrap();
                            let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&event));

                            if should_send {
                                let _ = tx_clone.send(event);
                                stats_clone.lock().unwrap().events_sent += 1;
                            } else {
                                stats_clone.lock().unwrap().events_filtered += 1;
                            }

                            tracing::debug!("Received log for signature: {}", signature);
                        }
                        None => {
                            tracing::warn!("WebSocket stream ended for program: {}", program_id);
                            break;
                        }
                    }
                }
                // pubsub and unsubscribe automatically kept alive while stream exists
            });
        }

        Ok(())
    }

    /// Parse SPL token transfer from transaction logs
    /// Returns (from, to, amount, decimals) if found
    fn parse_spl_transfer(logs: &[String]) -> Option<(String, String, f64, u8)> {
        // SPL Token program log format:
        // "Program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA invoke [1]"
        // "Program log: Instruction: Transfer"
        // Transfer format in logs varies, but we can extract from inner instructions or parse logs

        // Look for transfer instruction
        let mut found_transfer = false;
        for log in logs {
            if log.contains("Program log: Instruction: Transfer") {
                found_transfer = true;
                break;
            }
        }

        if !found_transfer {
            return None;
        }

        // Try to extract amount from logs
        // Common patterns:
        // "Program log: Transfer <amount> tokens"
        // "Program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA consumed"
        for log in logs {
            if log.contains("Transfer") && log.contains("tokens") {
                // Try to parse amount
                // This is simplified - in production, parse from instruction data
                // For now, return placeholder values
                return Some((
                    "unknown".to_string(),
                    "unknown".to_string(),
                    0.0,
                    9, // Most SPL tokens use 9 decimals (like USDC)
                ));
            }
        }

        None
    }

    /// Poll for new Solana events
    async fn poll_events(
        rpc_url: &str,
        tx: &broadcast::Sender<SolanaEvent>,
        filters: &Arc<Mutex<Vec<EventFilter>>>,
        stats: &Arc<Mutex<StreamStats>>,
    ) -> Result<()> {
        let client = RpcClient::new_with_commitment(rpc_url.to_string(), CommitmentConfig::confirmed());

        // Get current slot
        let slot = client.get_slot().context("Failed to get current slot")?;
        stats.lock().unwrap().last_slot = slot;

        // Emit slot update event
        let slot_event = SolanaEvent::SlotUpdate {
            slot,
            parent: slot.saturating_sub(1),
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        };

        let filters_vec = filters.lock().unwrap().clone();
        let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&slot_event));

        stats.lock().unwrap().events_processed += 1;

        if should_send {
            let _ = tx.send(slot_event);
            stats.lock().unwrap().events_sent += 1;
        } else {
            stats.lock().unwrap().events_filtered += 1;
        }

        // Get recent block with transactions
        if let Ok(block) = client.get_block(slot) {
            for tx_with_meta in block.transactions.iter().take(10) {
                if let Some(transaction) = &tx_with_meta.transaction.decode() {
                    if let Some(meta) = &tx_with_meta.meta {
                        let signature = transaction.signatures.get(0).map(|s| s.to_string()).unwrap_or_default();
                        let signer = transaction.message.static_account_keys().get(0).map(|k| k.to_string()).unwrap_or_default();

                        // Extract all program IDs from the transaction
                        let program_ids: Vec<String> = transaction
                            .message
                            .instructions()
                            .iter()
                            .filter_map(|ix| {
                                transaction.message.static_account_keys().get(ix.program_id_index as usize)
                            })
                            .map(|pubkey| pubkey.to_string())
                            .collect();

                        let event = SolanaEvent::Transaction {
                            signature: signature.clone(),
                            slot,
                            timestamp: block.block_time.unwrap_or(0) as u64,
                            success: meta.status.is_ok(),
                            fee: meta.fee,
                            signer,
                            program_ids: if program_ids.is_empty() { None } else { Some(program_ids) },
                        };

                        stats.lock().unwrap().events_processed += 1;

                        let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&event));

                        if should_send {
                            let _ = tx.send(event);
                            stats.lock().unwrap().events_sent += 1;
                        } else {
                            stats.lock().unwrap().events_filtered += 1;
                        }

                        // Parse token transfers from logs and inner instructions
                        // In Solana SDK 3.0, log_messages uses OptionSerializer enum
                        use solana_transaction_status::option_serializer::OptionSerializer;

                        // Try to parse SPL token transfers from pre/post token balances
                        if let (OptionSerializer::Some(pre_balances), OptionSerializer::Some(post_balances)) =
                            (&meta.pre_token_balances, &meta.post_token_balances)
                        {
                            // Match pre and post balances to detect transfers
                            for post in post_balances {
                                if let Some(pre) = pre_balances.iter().find(|p| p.account_index == post.account_index) {
                                    // Calculate change in balance
                                    let pre_amount = pre.ui_token_amount.ui_amount.unwrap_or(0.0);
                                    let post_amount = post.ui_token_amount.ui_amount.unwrap_or(0.0);
                                    let change = post_amount - pre_amount;

                                    if change.abs() > 0.0 {
                                        let token_transfer = SolanaEvent::TokenTransfer {
                                            signature: signature.clone(),
                                            from: if change < 0.0 {
                                                post.owner.clone().unwrap_or_else(|| "unknown".to_string())
                                            } else {
                                                "unknown".to_string()
                                            },
                                            to: if change > 0.0 {
                                                post.owner.clone().unwrap_or_else(|| "unknown".to_string())
                                            } else {
                                                "unknown".to_string()
                                            },
                                            amount: change.abs(),
                                            token: post.mint.clone(),
                                            decimals: post.ui_token_amount.decimals,
                                        };

                                        stats.lock().unwrap().events_processed += 1;

                                        let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&token_transfer));

                                        if should_send {
                                            let _ = tx.send(token_transfer);
                                            stats.lock().unwrap().events_sent += 1;
                                        } else {
                                            stats.lock().unwrap().events_filtered += 1;
                                        }
                                    }
                                }
                            }
                        }

                        // Also emit log messages for transfer instructions
                        if let OptionSerializer::Some(log_messages) = &meta.log_messages {
                            let has_transfer = log_messages.iter().any(|log|
                                log.contains("Program log: Instruction: Transfer")
                            );

                            if has_transfer {
                                let log_event = SolanaEvent::LogMessage {
                                    signature: signature.clone(),
                                    logs: log_messages.clone(),
                                    slot,
                                };

                                stats.lock().unwrap().events_processed += 1;

                                let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&log_event));

                                if should_send {
                                    let _ = tx.send(log_event);
                                    stats.lock().unwrap().events_sent += 1;
                                } else {
                                    stats.lock().unwrap().events_filtered += 1;
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_filter() {
        let filter = EventFilter {
            event_types: Some(vec!["transaction".to_string()]),
            success_only: true,
            ..Default::default()
        };

        let success_tx = SolanaEvent::Transaction {
            signature: "test".to_string(),
            slot: 100,
            timestamp: 0,
            success: true,
            fee: 5000,
            signer: "addr".to_string(),
        };

        let failed_tx = SolanaEvent::Transaction {
            signature: "test2".to_string(),
            slot: 100,
            timestamp: 0,
            success: false,
            fee: 5000,
            signer: "addr".to_string(),
        };

        assert!(filter.matches(&success_tx));
        assert!(!filter.matches(&failed_tx));
    }
}
