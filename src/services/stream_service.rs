use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use solana_client::rpc_client::RpcClient;
use solana_client::rpc_config::{RpcTransactionLogsConfig, RpcTransactionLogsFilter};
use solana_sdk::commitment_config::CommitmentConfig;
use solana_sdk::pubkey::Pubkey;
use solana_sdk::signature::Signature;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::broadcast;
use tokio::time;

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
            if let SolanaEvent::ProgramInvocation { program_id, .. } = event {
                if !programs.contains(program_id) {
                    return false;
                }
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

        Ok(())
    }

    /// Stop the streaming service
    pub fn stop(&self) {
        *self.running.lock().unwrap() = false;
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
                        let signature = transaction.signatures[0].to_string();

                        let event = SolanaEvent::Transaction {
                            signature: signature.clone(),
                            slot,
                            timestamp: block.block_time.unwrap_or(0) as u64,
                            success: meta.status.is_ok(),
                            fee: meta.fee,
                            signer: transaction.message.account_keys[0].to_string(),
                        };

                        stats.lock().unwrap().events_processed += 1;

                        let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&event));

                        if should_send {
                            let _ = tx.send(event);
                            stats.lock().unwrap().events_sent += 1;
                        } else {
                            stats.lock().unwrap().events_filtered += 1;
                        }

                        // Parse token transfers from logs if available
                        if let Some(log_messages) = &meta.log_messages {
                            for log in log_messages {
                                if log.contains("Transfer") && log.contains("from:") {
                                    // Simplified token transfer parsing
                                    // In production, use proper SPL token parsing
                                    let token_event = SolanaEvent::LogMessage {
                                        signature: signature.clone(),
                                        logs: vec![log.clone()],
                                        slot,
                                    };

                                    stats.lock().unwrap().events_processed += 1;

                                    let should_send = filters_vec.is_empty() || filters_vec.iter().any(|f| f.matches(&token_event));

                                    if should_send {
                                        let _ = tx.send(token_event);
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
