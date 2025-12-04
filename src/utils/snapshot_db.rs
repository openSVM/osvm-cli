//! RocksDB-backed snapshot data storage for high-performance Solana queries
//!
//! This module provides a RocksDB backend for storing and querying Solana ledger snapshots.
//! Unlike SQLite (used for investigation metadata), RocksDB is optimized for:
//! - Billions of records (full Solana state)
//! - High write throughput (snapshot sync)
//! - Efficient range queries (account scans)
//! - Large datasets (terabytes)

use anyhow::{Context, Result};
use rocksdb::{IteratorMode, Options, WriteBatch, DB};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;

/// Column families for different data types
pub mod cf {
    pub const ACCOUNTS: &str = "accounts"; // Account state data
    pub const TRANSACTIONS: &str = "transactions"; // Transaction history
    pub const PROGRAM_DATA: &str = "program_data"; // Program account data
    pub const TOKEN_ACCOUNTS: &str = "token_accounts"; // Token-specific data
    pub const METADATA: &str = "metadata"; // Snapshot metadata
}

/// Account data stored in RocksDB
#[derive(Debug, Clone, Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct AccountRecord {
    pub address: String,
    pub lamports: u64,
    pub owner: String,
    pub executable: bool,
    pub rent_epoch: u64,
    pub data: Vec<u8>,
    pub slot: u64, // When this was last updated
}

/// Transaction record
#[derive(Debug, Clone, Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct TransactionRecord {
    pub signature: String,
    pub slot: u64,
    pub block_time: Option<i64>,
    pub fee: u64,
    pub accounts: Vec<String>,
    pub instructions: Vec<InstructionRecord>,
    pub success: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct InstructionRecord {
    pub program_id: String,
    pub accounts: Vec<String>,
    pub data: Vec<u8>,
}

/// Token account data
#[derive(Debug, Clone, Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct TokenAccountRecord {
    pub address: String,
    pub mint: String,
    pub owner: String,
    pub amount: u64,
    pub delegate: Option<String>,
    pub slot: u64,
}

/// Snapshot metadata
#[derive(Debug, Clone, Serialize, Deserialize, bincode::Encode, bincode::Decode)]
pub struct SnapshotMetadata {
    pub slot: u64,
    pub hash: String,
    pub timestamp: i64,
    pub accounts_synced: u64,
    pub transactions_synced: u64,
    pub sync_started: i64,
    pub sync_completed: Option<i64>,
}

/// RocksDB snapshot storage
pub struct SnapshotDB {
    db: Arc<DB>,
}

impl SnapshotDB {
    /// Open or create snapshot database
    pub fn open() -> Result<Self> {
        let db_path = Self::db_path()?;

        // Ensure directory exists
        if let Some(parent) = db_path.parent() {
            std::fs::create_dir_all(parent).context("Failed to create snapshot db directory")?;
        }

        // Configure RocksDB for high performance
        let mut opts = Options::default();
        opts.create_if_missing(true);
        opts.create_missing_column_families(true);

        // Performance tuning
        opts.set_max_background_jobs(4);
        opts.set_bytes_per_sync(1048576); // 1MB
        opts.set_write_buffer_size(256 * 1024 * 1024); // 256MB write buffer
        opts.set_max_write_buffer_number(3);
        opts.set_target_file_size_base(256 * 1024 * 1024); // 256MB SST files

        // Enable compression
        opts.set_compression_type(rocksdb::DBCompressionType::Lz4);

        // Create column families
        let cfs = vec![
            cf::ACCOUNTS,
            cf::TRANSACTIONS,
            cf::PROGRAM_DATA,
            cf::TOKEN_ACCOUNTS,
            cf::METADATA,
        ];

        let db = DB::open_cf(&opts, &db_path, &cfs).context("Failed to open snapshot database")?;

        Ok(Self { db: Arc::new(db) })
    }

    fn db_path() -> Result<PathBuf> {
        let home = dirs::home_dir().context("Failed to get home directory")?;
        Ok(home.join(".osvm").join("snapshot.rocksdb"))
    }

    /// Open snapshot database at a specific path (useful for testing)
    #[cfg(test)]
    pub fn open_path(path: &std::path::Path) -> Result<Self> {
        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).context("Failed to create snapshot db directory")?;
        }

        // Configure RocksDB for high performance
        let mut opts = Options::default();
        opts.create_if_missing(true);
        opts.create_missing_column_families(true);

        // Performance tuning
        opts.set_max_background_jobs(4);
        opts.set_bytes_per_sync(1048576); // 1MB
        opts.set_write_buffer_size(256 * 1024 * 1024); // 256MB write buffer
        opts.set_max_write_buffer_number(3);
        opts.set_target_file_size_base(256 * 1024 * 1024); // 256MB SST files

        // Enable compression
        opts.set_compression_type(rocksdb::DBCompressionType::Lz4);

        // Create column families
        let cfs = vec![
            cf::ACCOUNTS,
            cf::TRANSACTIONS,
            cf::PROGRAM_DATA,
            cf::TOKEN_ACCOUNTS,
            cf::METADATA,
        ];

        let db = DB::open_cf(&opts, path, &cfs).context("Failed to open snapshot database")?;

        Ok(Self { db: Arc::new(db) })
    }

    /// Store account data
    pub fn put_account(&self, account: &AccountRecord) -> Result<()> {
        let cf = self
            .db
            .cf_handle(cf::ACCOUNTS)
            .context("Accounts column family not found")?;

        let key = account.address.as_bytes();
        let value = bincode::encode_to_vec(account, bincode::config::standard())
            .context("Failed to serialize account")?;

        self.db
            .put_cf(cf, key, value)
            .context("Failed to write account")?;

        Ok(())
    }

    /// Get account data
    pub fn get_account(&self, address: &str) -> Result<Option<AccountRecord>> {
        let cf = self
            .db
            .cf_handle(cf::ACCOUNTS)
            .context("Accounts column family not found")?;

        let key = address.as_bytes();

        match self.db.get_cf(cf, key)? {
            Some(value) => {
                let (account, _): (AccountRecord, _) =
                    bincode::decode_from_slice(&value, bincode::config::standard())
                        .context("Failed to deserialize account")?;
                Ok(Some(account))
            }
            None => Ok(None),
        }
    }

    /// Batch insert accounts (for snapshot sync)
    pub fn put_accounts_batch(&self, accounts: &[AccountRecord]) -> Result<()> {
        let cf = self
            .db
            .cf_handle(cf::ACCOUNTS)
            .context("Accounts column family not found")?;

        let mut batch = WriteBatch::default();

        for account in accounts {
            let key = account.address.as_bytes();
            let value = bincode::encode_to_vec(account, bincode::config::standard())
                .context("Failed to serialize account")?;
            batch.put_cf(cf, key, value);
        }

        self.db
            .write(batch)
            .context("Failed to write account batch")?;

        Ok(())
    }

    /// Store transaction
    pub fn put_transaction(&self, tx: &TransactionRecord) -> Result<()> {
        let cf = self
            .db
            .cf_handle(cf::TRANSACTIONS)
            .context("Transactions column family not found")?;

        let key = tx.signature.as_bytes();
        let value = bincode::encode_to_vec(tx, bincode::config::standard())
            .context("Failed to serialize transaction")?;

        self.db
            .put_cf(cf, key, value)
            .context("Failed to write transaction")?;

        Ok(())
    }

    /// Get transaction
    pub fn get_transaction(&self, signature: &str) -> Result<Option<TransactionRecord>> {
        let cf = self
            .db
            .cf_handle(cf::TRANSACTIONS)
            .context("Transactions column family not found")?;

        let key = signature.as_bytes();

        match self.db.get_cf(cf, key)? {
            Some(value) => {
                let (tx, _): (TransactionRecord, _) =
                    bincode::decode_from_slice(&value, bincode::config::standard())
                        .context("Failed to deserialize transaction")?;
                Ok(Some(tx))
            }
            None => Ok(None),
        }
    }

    /// Store token account
    pub fn put_token_account(&self, token_account: &TokenAccountRecord) -> Result<()> {
        let cf = self
            .db
            .cf_handle(cf::TOKEN_ACCOUNTS)
            .context("Token accounts column family not found")?;

        let key = token_account.address.as_bytes();
        let value = bincode::encode_to_vec(token_account, bincode::config::standard())
            .context("Failed to serialize token account")?;

        self.db
            .put_cf(cf, key, value)
            .context("Failed to write token account")?;

        Ok(())
    }

    /// Get token account
    pub fn get_token_account(&self, address: &str) -> Result<Option<TokenAccountRecord>> {
        let cf = self
            .db
            .cf_handle(cf::TOKEN_ACCOUNTS)
            .context("Token accounts column family not found")?;

        let key = address.as_bytes();

        match self.db.get_cf(cf, key)? {
            Some(value) => {
                let (token, _): (TokenAccountRecord, _) =
                    bincode::decode_from_slice(&value, bincode::config::standard())
                        .context("Failed to deserialize token account")?;
                Ok(Some(token))
            }
            None => Ok(None),
        }
    }

    /// Get all token accounts for a wallet
    pub fn get_wallet_tokens(&self, owner: &str) -> Result<Vec<TokenAccountRecord>> {
        let cf = self
            .db
            .cf_handle(cf::TOKEN_ACCOUNTS)
            .context("Token accounts column family not found")?;

        let mut tokens = Vec::new();

        let iter = self.db.iterator_cf(cf, IteratorMode::Start);
        for item in iter {
            let (_, value) = item?;
            let (token, _): (TokenAccountRecord, _) =
                bincode::decode_from_slice(&value, bincode::config::standard())
                    .context("Failed to deserialize token account")?;

            if token.owner == owner {
                tokens.push(token);
            }
        }

        Ok(tokens)
    }

    /// Get all accounts owned by a program
    pub fn get_program_accounts(&self, program_id: &str) -> Result<Vec<AccountRecord>> {
        let cf = self
            .db
            .cf_handle(cf::ACCOUNTS)
            .context("Accounts column family not found")?;

        let mut accounts = Vec::new();

        let iter = self.db.iterator_cf(cf, IteratorMode::Start);
        for item in iter {
            let (_, value) = item?;
            let (account, _): (AccountRecord, _) =
                bincode::decode_from_slice(&value, bincode::config::standard())
                    .context("Failed to deserialize account")?;

            if account.owner == program_id {
                accounts.push(account);
            }
        }

        Ok(accounts)
    }

    /// Store snapshot metadata
    pub fn put_snapshot_metadata(&self, metadata: &SnapshotMetadata) -> Result<()> {
        let cf = self
            .db
            .cf_handle(cf::METADATA)
            .context("Metadata column family not found")?;

        let key = b"current_snapshot";
        let value = bincode::encode_to_vec(metadata, bincode::config::standard())
            .context("Failed to serialize metadata")?;

        self.db
            .put_cf(cf, key, value)
            .context("Failed to write metadata")?;

        Ok(())
    }

    /// Get current snapshot metadata
    pub fn get_snapshot_metadata(&self) -> Result<Option<SnapshotMetadata>> {
        let cf = self
            .db
            .cf_handle(cf::METADATA)
            .context("Metadata column family not found")?;

        let key = b"current_snapshot";

        match self.db.get_cf(cf, key)? {
            Some(value) => {
                let (metadata, _): (SnapshotMetadata, _) =
                    bincode::decode_from_slice(&value, bincode::config::standard())
                        .context("Failed to deserialize metadata")?;
                Ok(Some(metadata))
            }
            None => Ok(None),
        }
    }

    /// Get database statistics
    pub fn get_stats(&self) -> Result<DatabaseStats> {
        let accounts_cf = self
            .db
            .cf_handle(cf::ACCOUNTS)
            .context("Accounts column family not found")?;
        let txs_cf = self
            .db
            .cf_handle(cf::TRANSACTIONS)
            .context("Transactions column family not found")?;
        let tokens_cf = self
            .db
            .cf_handle(cf::TOKEN_ACCOUNTS)
            .context("Token accounts column family not found")?;

        let accounts_count = self.estimate_num_keys(accounts_cf)?;
        let transactions_count = self.estimate_num_keys(txs_cf)?;
        let token_accounts_count = self.estimate_num_keys(tokens_cf)?;

        Ok(DatabaseStats {
            accounts_count,
            transactions_count,
            token_accounts_count,
            db_size_bytes: self.get_db_size()?,
        })
    }

    fn estimate_num_keys(&self, cf: &rocksdb::ColumnFamily) -> Result<u64> {
        // RocksDB property for approximate count
        match self
            .db
            .property_int_value_cf(cf, "rocksdb.estimate-num-keys")?
        {
            Some(count) => Ok(count),
            None => Ok(0),
        }
    }

    fn get_db_size(&self) -> Result<u64> {
        let path = Self::db_path()?;
        let mut size = 0u64;

        if path.exists() {
            for entry in std::fs::read_dir(&path)? {
                let entry = entry?;
                if let Ok(metadata) = entry.metadata() {
                    size += metadata.len();
                }
            }
        }

        Ok(size)
    }

    /// Compact database (optimize storage)
    pub fn compact(&self) -> Result<()> {
        self.db.compact_range::<&[u8], &[u8]>(None, None);
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatabaseStats {
    pub accounts_count: u64,
    pub transactions_count: u64,
    pub token_accounts_count: u64,
    pub db_size_bytes: u64,
}

impl DatabaseStats {
    pub fn db_size_mb(&self) -> f64 {
        self.db_size_bytes as f64 / (1024.0 * 1024.0)
    }

    pub fn db_size_gb(&self) -> f64 {
        self.db_size_bytes as f64 / (1024.0 * 1024.0 * 1024.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn setup_test_db() -> (SnapshotDB, TempDir) {
        let tmp_dir = TempDir::new().expect("Failed to create temp dir");
        let db_path = tmp_dir.path().join("test.rocksdb");
        let db = SnapshotDB::open_path(&db_path).expect("Failed to open test db");
        (db, tmp_dir)
    }

    #[test]
    fn test_account_storage() {
        let (db, _tmp_dir) = setup_test_db();

        let account = AccountRecord {
            address: "TestAccount123".to_string(),
            lamports: 1000000,
            owner: "11111111111111111111111111111111".to_string(),
            executable: false,
            rent_epoch: 100,
            data: vec![1, 2, 3, 4],
            slot: 12345,
        };

        db.put_account(&account).unwrap();

        let retrieved = db.get_account("TestAccount123").unwrap();
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().lamports, 1000000);
    }

    #[test]
    fn test_batch_insert() {
        let (db, _tmp_dir) = setup_test_db();

        let accounts = vec![
            AccountRecord {
                address: "Batch1".to_string(),
                lamports: 100,
                owner: "Prog1".to_string(),
                executable: false,
                rent_epoch: 1,
                data: vec![],
                slot: 1,
            },
            AccountRecord {
                address: "Batch2".to_string(),
                lamports: 200,
                owner: "Prog1".to_string(),
                executable: false,
                rent_epoch: 1,
                data: vec![],
                slot: 1,
            },
        ];

        db.put_accounts_batch(&accounts).unwrap();

        assert!(db.get_account("Batch1").unwrap().is_some());
        assert!(db.get_account("Batch2").unwrap().is_some());
    }
}
