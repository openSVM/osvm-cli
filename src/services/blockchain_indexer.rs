//! Blockchain data indexer for syncing RocksDB and snapshots to ClickHouse

use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};
use sha2::{Digest, Sha256};
use solana_sdk::pubkey::Pubkey;
use std::ops::Range;
use std::str::FromStr;
use std::sync::Arc;

use super::{
    account_decoders::{format_decoded_account, DecodedAccount, DecoderRegistry},
    clickhouse_service::ClickHouseService,
    ledger_service::LedgerService,
    snapshot_service::SnapshotService,
};

/// Indexing mode configuration
#[derive(Debug, Clone)]
pub enum IndexingMode {
    /// Index all historical data
    FullHistorical,
    /// Index only the last N days
    LastNDays(u32),
    /// Start fresh, index only new data going forward
    RealtimeOnly,
    /// Custom configuration with filters
    Custom(Box<IndexingConfig>),
}

/// Indexing configuration with filters
#[derive(Debug, Clone)]
pub struct IndexingConfig {
    pub mode: IndexingMode,
    pub time_range: Option<TimeRange>,
    pub program_filters: Vec<Pubkey>,
    pub account_filters: Vec<Pubkey>,
    pub data_patterns: Vec<BytePattern>,
}

#[derive(Debug, Clone)]
pub struct TimeRange {
    pub start: chrono::DateTime<chrono::Utc>,
    pub end: Option<chrono::DateTime<chrono::Utc>>,
}

#[derive(Debug, Clone)]
pub struct BytePattern {
    pub pattern: Vec<u8>,
    pub offset: Option<usize>,
}

impl Default for IndexingConfig {
    fn default() -> Self {
        Self {
            mode: IndexingMode::RealtimeOnly,
            time_range: None,
            program_filters: Vec::new(),
            account_filters: Vec::new(),
            data_patterns: Vec::new(),
        }
    }
}

/// Blockchain indexer service
pub struct BlockchainIndexer {
    ledger_service: LedgerService,
    snapshot_service: SnapshotService,
    clickhouse: Arc<ClickHouseService>,
    decoder_registry: Arc<DecoderRegistry>,
    config: IndexingConfig,
}

impl BlockchainIndexer {
    /// Create a new blockchain indexer
    pub fn new(
        ledger_path: Option<String>,
        snapshot_path: Option<String>,
        clickhouse: Arc<ClickHouseService>,
        config: IndexingConfig,
    ) -> Result<Self> {
        use std::path::PathBuf;
        
        let ledger_pb = ledger_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("devnet-ledger"));
        let ledger_service = LedgerService::new(ledger_pb)?;
        
        let snapshot_pb = snapshot_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."));
        let snapshot_service = SnapshotService::new(snapshot_pb)?;
        
        let decoder_registry = Arc::new(DecoderRegistry::new());

        Ok(Self {
            ledger_service,
            snapshot_service,
            clickhouse,
            decoder_registry,
            config,
        })
    }

    /// Sync account data from snapshot to ClickHouse
    pub async fn sync_accounts(&self, slot: u64) -> Result<usize> {
        info!("Syncing accounts from snapshot at slot {}", slot);

        // Read accounts from snapshot directory
        let accounts_dir = self.snapshot_service.snapshot_dir.join("accounts");
        if !accounts_dir.exists() {
            warn!("Accounts directory not found in snapshot, using slot 0 as placeholder");
        }

        let entries: Vec<_> = std::fs::read_dir(&accounts_dir)
            .context("Failed to read accounts directory")?
            .filter_map(|e| e.ok())
            .collect();

        info!("Found {} account files to process", entries.len());

        let mut indexed_count = 0;
        let mut batch_values = Vec::new();

        for entry in entries {
            let path = entry.path();

            // SECURITY: Validate file path to prevent path traversal attacks
            if let Err(e) = Self::validate_account_file_path(&path, &accounts_dir) {
                debug!("Skipping invalid file {:?}: {}", path, e);
                continue;
            }

            // Read account data from file
            let account = match self.read_account_from_file(&path) {
                Ok(acc) => acc,
                Err(e) => {
                    debug!("Failed to read account file {:?}: {}", path, e);
                    continue;
                }
            };

            // Apply filters
            if let Some(ref pubkey) = account.pubkey {
                if self.should_filter_account(pubkey, &account.owner, &account.data) {
                    continue;
                }
            }

            // Decode account if possible
            let decoded = self.decoder_registry.decode(&account.owner, &account.data).unwrap_or(
                crate::services::account_decoders::DecodedAccount::Unknown
            );
            let decoded_json = serde_json::to_string(&format_decoded_account(&decoded))?;

            // Calculate data hash
            let data_hash = if !account.data.is_empty() {
                use sha2::{Digest, Sha256};
                let mut hasher = Sha256::new();
                hasher.update(&account.data);
                format!("{:x}", hasher.finalize())
            } else {
                String::new()
            };

            // Get first 100 bytes as sample
            let data_sample: Vec<u8> = account.data.iter().take(100).copied().collect();
            let data_sample_str = data_sample
                .iter()
                .map(|b| b.to_string())
                .collect::<Vec<_>>()
                .join(",");

            // SECURITY: Validate data_sample_str contains only digits and commas to prevent SQL injection
            if !data_sample_str.chars().all(|c| c.is_numeric() || c == ',') {
                return Err(anyhow::anyhow!(
                    "Invalid data sample format - contains non-numeric characters"
                ));
            }

            // Determine program type from decoded account
            let program_type = if matches!(decoded, DecodedAccount::Unknown) {
                "Unknown".to_string()
            } else {
                // Extract variant name from debug representation
                let debug_str = format!("{:?}", decoded);
                debug_str.split('(').next().unwrap_or("Unknown").to_string()
            };

            // SECURITY: Sanitize program_type to prevent SQL injection through debug output
            let safe_program_type = Self::sanitize_identifier(&program_type);

            let pubkey_str = account.pubkey
                .map(|p| p.to_string())
                .unwrap_or_else(|| entry.file_name().to_string_lossy().to_string());

            batch_values.push(format!(
                "({}, '{}', '{}', {}, {}, '{}', {}, {}, '{}', '{}', [{}])",
                slot,
                Self::escape_string(&pubkey_str),
                Self::escape_string(&account.owner.to_string()),
                account.lamports,
                account.data.len(),
                Self::escape_string(&data_hash),
                if account.executable { 1 } else { 0 },
                account.rent_epoch,
                Self::escape_string(&safe_program_type),
                Self::escape_string(&decoded_json),
                data_sample_str
            ));

            indexed_count += 1;

            // Batch insert every 1000 accounts
            if batch_values.len() >= 1000 {
                self.insert_account_batch(&batch_values).await?;
                batch_values.clear();
                info!("Indexed {} accounts so far...", indexed_count);
            }
        }

        // Insert remaining accounts
        if !batch_values.is_empty() {
            self.insert_account_batch(&batch_values).await?;
        }

        info!("Successfully indexed {} accounts from snapshot", indexed_count);
        Ok(indexed_count)
    }

    /// Read account data from snapshot file
    fn read_account_from_file(&self, path: &std::path::Path) -> Result<super::snapshot_service::AccountInfo> {
        use std::fs::File;
        use std::io::Read;

        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;

        if buffer.len() < 100 {
            return Err(anyhow!("File too small to be a valid account"));
        }

        // Parse the account storage format
        let data_len = u64::from_le_bytes(buffer[8..16].try_into()?);
        let owner_bytes: [u8; 32] = buffer[17..49].try_into()?;
        let owner = Pubkey::new_from_array(owner_bytes);
        let lamports = u64::from_le_bytes(buffer[49..57].try_into()?);
        let rent_epoch = u64::from_le_bytes(buffer[57..65].try_into()?);

        // Try to extract pubkey if available
        let pubkey = if buffer.len() > 81 {
            if let Ok(pubkey_bytes) = buffer[65..97].try_into() {
                Some(Pubkey::new_from_array(pubkey_bytes))
            } else {
                None
            }
        } else {
            None
        };

        let executable = false;

        let data_start = 130;
        let data = if buffer.len() > data_start {
            buffer[data_start..].to_vec()
        } else {
            Vec::new()
        };

        Ok(super::snapshot_service::AccountInfo {
            pubkey,
            lamports,
            data_len,
            owner,
            executable,
            rent_epoch,
            data,
        })
    }

    /// Insert a batch of accounts into ClickHouse (placeholder)
    #[allow(dead_code)]
    async fn insert_account_batch(&self, batch_values: &[String]) -> Result<()> {
        if batch_values.is_empty() {
            return Ok(());
        }

        let sql = format!(
            "INSERT INTO osvm.blockchain_accounts \
             (slot, pubkey, owner, lamports, data_len, data_hash, executable, rent_epoch, program_type, decoded_data, data_sample) \
             VALUES {}",
            batch_values.join(", ")
        );

        self.clickhouse.execute_query(&sql).await
            .context("Failed to insert account batch")?;

        debug!("Inserted batch of {} accounts", batch_values.len());
        Ok(())
    }

    /// Sync transaction data from ledger to ClickHouse
    pub async fn sync_transactions(&self, slot_range: Range<u64>) -> Result<usize> {
        info!("Syncing transactions from ledger for slot range {:?}", slot_range);
        
        if !self.ledger_service.has_transaction_history() {
            return Err(anyhow!("Ledger does not have transaction history. Start validator with --enable-rpc-transaction-history"));
        }
        
        // Stream transactions in the slot range
        let transactions = self.ledger_service
            .stream_transactions_in_range(slot_range.start, slot_range.end, None)
            .await?;
        
        info!("Found {} transactions to index", transactions.len());
        
        let mut batch_values = Vec::new();
        let mut indexed_count = 0;
        
        for tx in transactions {
            // Apply filters if configured
            if self.should_filter_transaction(&tx) {
                continue;
            }
            
            // Build batch value for ClickHouse insert
            let accounts_str = self.format_transaction_accounts(&tx);
            let program_ids_str = self.format_transaction_programs(&tx);
            let instruction_data_str = self.format_instruction_data(&tx);
            let logs_str = self.format_transaction_logs(&tx);
            
            batch_values.push(format!(
                "({}, '{}', {}, {}, {}, '{}', '{}', '{}', '{}', {})",
                tx.slot,
                Self::escape_string(&tx.signature),
                tx.block_time.unwrap_or(0),
                tx.fee,
                if tx.success { 1 } else { 0 },
                Self::escape_string(&accounts_str),
                Self::escape_string(&program_ids_str),
                Self::escape_string(&instruction_data_str),
                Self::escape_string(&logs_str),
                tx.instructions.len()
            ));
            
            indexed_count += 1;
            
            // Batch insert every 1000 transactions
            if batch_values.len() >= 1000 {
                self.insert_transaction_batch(&batch_values).await?;
                batch_values.clear();
                info!("Indexed {} transactions so far...", indexed_count);
            }
        }
        
        // Insert remaining transactions
        if !batch_values.is_empty() {
            self.insert_transaction_batch(&batch_values).await?;
        }
        
        info!("Successfully indexed {} transactions from ledger", indexed_count);
        Ok(indexed_count)
    }

    /// Insert a batch of transactions into ClickHouse
    async fn insert_transaction_batch(&self, batch_values: &[String]) -> Result<()> {
        if batch_values.is_empty() {
            return Ok(());
        }

        let sql = format!(
            "INSERT INTO osvm.blockchain_transactions \
             (slot, signature, block_time, fee, success, accounts, program_ids, instruction_data, logs, instruction_count) \
             VALUES {}",
            batch_values.join(", ")
        );

        self.clickhouse.execute_query(&sql).await
            .context("Failed to insert transaction batch")?;

        debug!("Inserted batch of {} transactions", batch_values.len());
        Ok(())
    }

    /// Check if transaction should be filtered based on indexing config
    fn should_filter_transaction(&self, tx: &super::ledger_service::TransactionInfo) -> bool {
        // Filter by program IDs if configured
        if !self.config.program_filters.is_empty() {
            let tx_has_program = tx.instructions.iter().any(|ix| {
                self.config.program_filters.iter().any(|filter_program| {
                    ix.program.contains(&filter_program.to_string())
                })
            });
            if !tx_has_program {
                return true; // Filter out
            }
        }
        
        false // Don't filter
    }
    
    /// Format transaction accounts as JSON array string
    fn format_transaction_accounts(&self, tx: &super::ledger_service::TransactionInfo) -> String {
        if tx.accounts.is_empty() {
            return "[]".to_string();
        }
        
        let accounts: Vec<String> = tx.accounts
            .iter()
            .map(|acc| format!("\"{}\"", acc))
            .collect();
        format!("[{}]", accounts.join(","))
    }
    
    /// Format transaction program IDs as JSON array string
    fn format_transaction_programs(&self, tx: &super::ledger_service::TransactionInfo) -> String {
        let programs: Vec<String> = tx.instructions
            .iter()
            .map(|ix| format!("\"{}\"", ix.program))
            .collect();
        format!("[{}]", programs.join(","))
    }
    
    /// Format instruction data as JSON array string
    fn format_instruction_data(&self, tx: &super::ledger_service::TransactionInfo) -> String {
        let instructions: Vec<String> = tx.instructions
            .iter()
            .map(|ix| {
                format!("{{\"program\":\"{}\",\"type\":\"{}\"}}", ix.program, ix.instruction_type)
            })
            .collect();
        format!("[{}]", instructions.join(","))
    }
    
    /// Format transaction logs as JSON array string
    fn format_transaction_logs(&self, tx: &super::ledger_service::TransactionInfo) -> String {
        if tx.logs.is_empty() {
            return "[]".to_string();
        }
        
        let logs: Vec<String> = tx.logs
            .iter()
            .map(|log| format!("\"{}\"", Self::escape_string(log)))
            .collect();
        format!("[{}]", logs.join(","))
    }

    /// Check if account should be filtered based on indexing config
    #[allow(dead_code)]
    fn should_filter_account(&self, pubkey: &Pubkey, owner: &Pubkey, data: &[u8]) -> bool {
        // Filter by specific accounts
        if !self.config.account_filters.is_empty() {
            if !self.config.account_filters.contains(pubkey) {
                return true;
            }
        }

        // Filter by program/owner
        if !self.config.program_filters.is_empty() {
            if !self.config.program_filters.contains(owner) {
                return true;
            }
        }

        // Filter by data patterns
        if !self.config.data_patterns.is_empty() {
            let mut pattern_matched = false;
            for pattern in &self.config.data_patterns {
                if Self::data_matches_pattern(data, &pattern.pattern, pattern.offset) {
                    pattern_matched = true;
                    break;
                }
            }
            if !pattern_matched {
                return true;
            }
        }

        false
    }

    /// Check if data matches a byte pattern
    fn data_matches_pattern(data: &[u8], pattern: &[u8], offset: Option<usize>) -> bool {
        if let Some(off) = offset {
            if off + pattern.len() > data.len() {
                return false;
            }
            &data[off..off + pattern.len()] == pattern
        } else {
            // Search for pattern anywhere in data
            data.windows(pattern.len()).any(|window| window == pattern)
        }
    }

    /// Escape SQL string values
    #[allow(dead_code)]
    fn escape_string(s: &str) -> String {
        s.replace('\'', "''")
            .replace('\\', "\\\\")
    }

    /// Sanitize identifier to prevent SQL injection
    /// Removes any characters that aren't alphanumeric or underscore
    fn sanitize_identifier(s: &str) -> String {
        s.chars()
            .filter(|c| c.is_alphanumeric() || *c == '_')
            .collect()
    }

    /// Validate account file path to prevent path traversal attacks
    fn validate_account_file_path(file_path: &std::path::Path, accounts_dir: &std::path::Path) -> Result<()> {
        use std::fs;

        // Check if file exists
        if !file_path.exists() {
            anyhow::bail!("File does not exist");
        }

        // Get metadata using symlink_metadata to detect symlinks
        let metadata = fs::symlink_metadata(file_path)
            .context("Failed to get file metadata")?;

        // Reject symlinks
        if metadata.file_type().is_symlink() {
            anyhow::bail!("Symlinks are not allowed for security reasons");
        }

        // Must be a regular file
        if !metadata.is_file() {
            anyhow::bail!("Not a regular file");
        }

        // Canonicalize both paths to resolve any .. or . components
        let canonical_file = file_path.canonicalize()
            .context("Failed to canonicalize file path")?;
        let canonical_dir = accounts_dir.canonicalize()
            .context("Failed to canonicalize accounts directory")?;

        // Verify file is within accounts directory
        if !canonical_file.starts_with(&canonical_dir) {
            anyhow::bail!(
                "File is outside allowed directory. File: {:?}, Allowed dir: {:?}",
                canonical_file,
                canonical_dir
            );
        }

        // Check file size (max 100MB to prevent memory issues)
        const MAX_FILE_SIZE: u64 = 100 * 1024 * 1024;
        if metadata.len() > MAX_FILE_SIZE {
            anyhow::bail!("File too large (max 100MB)");
        }

        Ok(())
    }
}

/// Sync arguments for CLI commands
#[derive(Debug, Clone)]
pub struct SyncArgs {
    pub mode: IndexingMode,
    pub programs: Vec<String>,
    pub accounts: Vec<String>,
    pub patterns: Vec<String>,
    pub ledger_path: Option<String>,
    pub snapshot_path: Option<String>,
}

impl Default for SyncArgs {
    fn default() -> Self {
        Self {
            mode: IndexingMode::LastNDays(30),
            programs: Vec::new(),
            accounts: Vec::new(),
            patterns: Vec::new(),
            ledger_path: None,
            snapshot_path: None,
        }
    }
}

/// Parse sync arguments into indexing configuration
pub fn parse_sync_args(args: &SyncArgs) -> Result<IndexingConfig> {
    let mut config = IndexingConfig::default();
    config.mode = args.mode.clone();

    // Parse program filters
    for prog_str in &args.programs {
        if let Ok(pubkey) = Pubkey::from_str(prog_str) {
            config.program_filters.push(pubkey);
        } else {
            warn!("Invalid program ID: {}", prog_str);
        }
    }

    // Parse account filters
    for acc_str in &args.accounts {
        if let Ok(pubkey) = Pubkey::from_str(acc_str) {
            config.account_filters.push(pubkey);
        } else {
            warn!("Invalid account pubkey: {}", acc_str);
        }
    }

    // Parse data patterns (hex strings)
    for pattern_str in &args.patterns {
        if let Ok(bytes) = hex::decode(pattern_str.trim_start_matches("0x")) {
            config.data_patterns.push(BytePattern {
                pattern: bytes,
                offset: None,
            });
        } else {
            warn!("Invalid hex pattern: {}", pattern_str);
        }
    }

    Ok(config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data_pattern_matching() {
        let data = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let pattern = vec![0x02, 0x03];
        
        assert!(BlockchainIndexer::data_matches_pattern(&data, &pattern, None));
        assert!(BlockchainIndexer::data_matches_pattern(&data, &pattern, Some(1)));
        assert!(!BlockchainIndexer::data_matches_pattern(&data, &pattern, Some(0)));
    }

    #[test]
    fn test_escape_string() {
        assert_eq!(BlockchainIndexer::escape_string("test'value"), "test''value");
        assert_eq!(BlockchainIndexer::escape_string("path\\to\\file"), "path\\\\to\\\\file");
    }
}
