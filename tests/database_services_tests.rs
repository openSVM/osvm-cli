//! Comprehensive tests for database services (ClickHouse, RocksDB, Ledger)

use anyhow::Result;
use mockito::Server;
use osvm::services::{
    clickhouse_service::{ClickHouseConfig, ClickHouseService, QueryResult},
    ledger_service::{LedgerEntry, LedgerQuery, LedgerService},
    rocksdb_parser::{KeyValuePair, RocksDBConfig, RocksDBParser},
};
use serde_json::json;
use std::collections::HashMap;
use std::path::PathBuf;
use tempfile::TempDir;

#[cfg(test)]
mod clickhouse_tests {
    use super::*;

    #[tokio::test]
    async fn test_clickhouse_connection() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock ClickHouse health check
        let health_mock = server
            .mock("GET", "/ping")
            .with_status(200)
            .with_body("Ok.\n")
            .create_async()
            .await;

        let config = ClickHouseConfig {
            host: server.host_with_port(),
            database: "osvm_test".to_string(),
            username: Some("default".to_string()),
            password: None,
            use_https: false,
            connection_timeout_ms: 5000,
            query_timeout_ms: 30000,
        };

        let service = ClickHouseService::new(config)?;
        let is_connected = service.check_connection().await?;

        assert!(is_connected);
        health_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_clickhouse_query_execution() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock query response
        let query_mock = server
            .mock("POST", "/")
            .match_body(mockito::Matcher::Regex("SELECT.*transactions".to_string()))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                json!({
                    "data": [
                        {
                            "signature": "5j7s6NiJS3JAk...txHash",
                            "block_time": 1234567890,
                            "slot": 150000000,
                            "fee": 5000
                        },
                        {
                            "signature": "3k8t7MjKT4KBl...txHash2",
                            "block_time": 1234567891,
                            "slot": 150000001,
                            "fee": 5000
                        }
                    ],
                    "rows": 2
                })
                .to_string(),
            )
            .create_async()
            .await;

        let config = ClickHouseConfig {
            host: server.host_with_port(),
            database: "osvm_test".to_string(),
            username: None,
            password: None,
            use_https: false,
            connection_timeout_ms: 5000,
            query_timeout_ms: 30000,
        };

        let service = ClickHouseService::new(config)?;

        let result = service.query("SELECT * FROM transactions LIMIT 2").await?;

        assert_eq!(result.rows, 2);
        assert_eq!(result.data.len(), 2);

        query_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_clickhouse_batch_insert() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock insert response
        let insert_mock = server
            .mock("POST", "/")
            .match_body(mockito::Matcher::Regex("INSERT INTO.*".to_string()))
            .with_status(200)
            .with_body("")
            .create_async()
            .await;

        let config = ClickHouseConfig {
            host: server.host_with_port(),
            database: "osvm_test".to_string(),
            username: None,
            password: None,
            use_https: false,
            connection_timeout_ms: 5000,
            query_timeout_ms: 30000,
        };

        let service = ClickHouseService::new(config)?;

        let transactions = vec![
            json!({
                "signature": "tx1",
                "block_time": 1234567890,
                "slot": 100,
                "fee": 5000
            }),
            json!({
                "signature": "tx2",
                "block_time": 1234567891,
                "slot": 101,
                "fee": 5000
            }),
        ];

        service.batch_insert("transactions", &transactions).await?;

        insert_mock.assert_async().await;

        Ok(())
    }

    #[tokio::test]
    async fn test_clickhouse_query_timeout() -> Result<()> {
        let mut server = Server::new_async().await;

        // Mock slow query
        let slow_mock = server
            .mock("POST", "/")
            .with_status(200)
            .with_body_from_fn(|_| {
                std::thread::sleep(std::time::Duration::from_secs(2));
                "delayed response"
            })
            .create_async()
            .await;

        let config = ClickHouseConfig {
            host: server.host_with_port(),
            database: "osvm_test".to_string(),
            username: None,
            password: None,
            use_https: false,
            connection_timeout_ms: 5000,
            query_timeout_ms: 500, // Short timeout
        };

        let service = ClickHouseService::new(config)?;

        let result = tokio::time::timeout(
            std::time::Duration::from_millis(600),
            service.query("SELECT * FROM slow_table"),
        )
        .await;

        assert!(result.is_err() || result.unwrap().is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_clickhouse_connection_retry() -> Result<()> {
        let mut server = Server::new_async().await;

        // First attempt fails, second succeeds
        let retry_mock = server
            .mock("GET", "/ping")
            .with_status(500)
            .expect(1)
            .create_async()
            .await;

        let success_mock = server
            .mock("GET", "/ping")
            .with_status(200)
            .with_body("Ok.\n")
            .expect(1)
            .create_async()
            .await;

        let config = ClickHouseConfig {
            host: server.host_with_port(),
            database: "osvm_test".to_string(),
            username: None,
            password: None,
            use_https: false,
            connection_timeout_ms: 5000,
            query_timeout_ms: 30000,
        };

        let service = ClickHouseService::new(config)?;

        // First check should fail
        let first_check = service.check_connection().await;
        assert!(first_check.is_err() || !first_check.unwrap());

        // Second check should succeed
        let second_check = service.check_connection().await?;
        assert!(second_check);

        retry_mock.assert_async().await;
        success_mock.assert_async().await;

        Ok(())
    }
}

#[cfg(test)]
mod rocksdb_tests {
    use super::*;

    #[tokio::test]
    async fn test_rocksdb_open() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: false,
            create_if_missing: true,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let parser = RocksDBParser::new(config)?;

        assert!(parser.is_open());

        Ok(())
    }

    #[tokio::test]
    async fn test_rocksdb_put_get() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: false,
            create_if_missing: true,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let parser = RocksDBParser::new(config)?;

        // Put key-value
        parser.put(b"test_key", b"test_value")?;

        // Get value
        let value = parser.get(b"test_key")?;
        assert_eq!(value, Some(b"test_value".to_vec()));

        Ok(())
    }

    #[tokio::test]
    async fn test_rocksdb_batch_operations() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: false,
            create_if_missing: true,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let parser = RocksDBParser::new(config)?;

        // Batch write
        let batch = vec![
            (b"key1".to_vec(), b"value1".to_vec()),
            (b"key2".to_vec(), b"value2".to_vec()),
            (b"key3".to_vec(), b"value3".to_vec()),
        ];

        parser.batch_put(&batch)?;

        // Verify all keys
        assert_eq!(parser.get(b"key1")?, Some(b"value1".to_vec()));
        assert_eq!(parser.get(b"key2")?, Some(b"value2".to_vec()));
        assert_eq!(parser.get(b"key3")?, Some(b"value3".to_vec()));

        Ok(())
    }

    #[tokio::test]
    async fn test_rocksdb_iterator() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: false,
            create_if_missing: true,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let parser = RocksDBParser::new(config)?;

        // Insert ordered keys
        for i in 0..10 {
            let key = format!("key_{:03}", i);
            let value = format!("value_{}", i);
            parser.put(key.as_bytes(), value.as_bytes())?;
        }

        // Iterate and count
        let mut count = 0;
        let iter = parser.iter();
        for item in iter {
            count += 1;
            assert!(item.is_ok());
        }

        assert_eq!(count, 10);

        Ok(())
    }

    #[tokio::test]
    async fn test_rocksdb_delete() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: false,
            create_if_missing: true,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let parser = RocksDBParser::new(config)?;

        // Put then delete
        parser.put(b"temp_key", b"temp_value")?;
        assert_eq!(parser.get(b"temp_key")?, Some(b"temp_value".to_vec()));

        parser.delete(b"temp_key")?;
        assert_eq!(parser.get(b"temp_key")?, None);

        Ok(())
    }

    #[tokio::test]
    async fn test_rocksdb_read_only_mode() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Create DB and write some data
        {
            let config = RocksDBConfig {
                path: temp_dir.path().to_path_buf(),
                read_only: false,
                create_if_missing: true,
                max_open_files: 1000,
                cache_size_mb: 128,
            };

            let parser = RocksDBParser::new(config)?;
            parser.put(b"readonly_test", b"value")?;
        }

        // Open in read-only mode
        let ro_config = RocksDBConfig {
            path: temp_dir.path().to_path_buf(),
            read_only: true,
            create_if_missing: false,
            max_open_files: 1000,
            cache_size_mb: 128,
        };

        let ro_parser = RocksDBParser::new(ro_config)?;

        // Can read
        assert_eq!(ro_parser.get(b"readonly_test")?, Some(b"value".to_vec()));

        // Cannot write
        let write_result = ro_parser.put(b"new_key", b"new_value");
        assert!(write_result.is_err());

        Ok(())
    }
}

#[cfg(test)]
mod ledger_tests {
    use super::*;

    #[tokio::test]
    async fn test_ledger_service_creation() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let service = LedgerService::new(temp_dir.path().to_path_buf())?;

        assert!(service.is_initialized());

        Ok(())
    }

    #[tokio::test]
    async fn test_ledger_append_entry() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let service = LedgerService::new(temp_dir.path().to_path_buf())?;

        let entry = LedgerEntry {
            slot: 100,
            block_time: 1234567890,
            transaction_count: 150,
            block_hash: "abc123...".to_string(),
            parent_hash: "xyz789...".to_string(),
            metadata: HashMap::new(),
        };

        service.append_entry(entry).await?;

        let count = service.get_entry_count().await?;
        assert_eq!(count, 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_ledger_query_by_slot() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let service = LedgerService::new(temp_dir.path().to_path_buf())?;

        // Add multiple entries
        for slot in 100..105 {
            let entry = LedgerEntry {
                slot,
                block_time: 1234567890 + slot,
                transaction_count: 100,
                block_hash: format!("hash_{}", slot),
                parent_hash: format!("parent_{}", slot - 1),
                metadata: HashMap::new(),
            };
            service.append_entry(entry).await?;
        }

        // Query specific slot
        let entry = service.get_entry_by_slot(102).await?;
        assert!(entry.is_some());
        assert_eq!(entry.unwrap().slot, 102);

        Ok(())
    }

    #[tokio::test]
    async fn test_ledger_query_range() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let service = LedgerService::new(temp_dir.path().to_path_buf())?;

        // Add entries
        for slot in 100..110 {
            let entry = LedgerEntry {
                slot,
                block_time: 1234567890 + slot,
                transaction_count: 100,
                block_hash: format!("hash_{}", slot),
                parent_hash: format!("parent_{}", slot - 1),
                metadata: HashMap::new(),
            };
            service.append_entry(entry).await?;
        }

        // Query range
        let query = LedgerQuery {
            start_slot: 103,
            end_slot: 107,
            limit: None,
        };

        let entries = service.query_range(query).await?;

        assert_eq!(entries.len(), 5); // Slots 103, 104, 105, 106, 107
        assert_eq!(entries[0].slot, 103);
        assert_eq!(entries[4].slot, 107);

        Ok(())
    }

    #[tokio::test]
    async fn test_ledger_concurrent_writes() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let service = std::sync::Arc::new(LedgerService::new(temp_dir.path().to_path_buf())?);

        let mut handles = vec![];

        // Concurrent writes
        for i in 0..10 {
            let service_clone = std::sync::Arc::clone(&service);
            let handle = tokio::spawn(async move {
                let entry = LedgerEntry {
                    slot: 100 + i,
                    block_time: 1234567890 + i,
                    transaction_count: 50,
                    block_hash: format!("hash_{}", i),
                    parent_hash: format!("parent_{}", i),
                    metadata: HashMap::new(),
                };
                service_clone.append_entry(entry).await
            });
            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        for result in results {
            assert!(result?.is_ok());
        }

        let count = service.get_entry_count().await?;
        assert_eq!(count, 10);

        Ok(())
    }

    #[tokio::test]
    async fn test_ledger_persistence() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Create and write
        {
            let service = LedgerService::new(temp_dir.path().to_path_buf())?;
            let entry = LedgerEntry {
                slot: 200,
                block_time: 1234567890,
                transaction_count: 75,
                block_hash: "persistent_hash".to_string(),
                parent_hash: "parent_hash".to_string(),
                metadata: HashMap::new(),
            };
            service.append_entry(entry).await?;
        }

        // Reopen and verify
        {
            let service = LedgerService::new(temp_dir.path().to_path_buf())?;
            let entry = service.get_entry_by_slot(200).await?;

            assert!(entry.is_some());
            let entry = entry.unwrap();
            assert_eq!(entry.slot, 200);
            assert_eq!(entry.block_hash, "persistent_hash");
        }

        Ok(())
    }
}
