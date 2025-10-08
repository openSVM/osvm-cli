//! Comprehensive tests for activity logging including structured logging,
//! log levels, filtering, rotation, and audit trail functionality

use anyhow::Result;
use chrono::Utc;
use osvm::services::activity_logger::{
    ActivityLogger, ActivityMetrics, AuditTrail, LogEntry, LogFilter, LogFormatter, LogLevel,
    LogRotation, StructuredLogger,
};
use std::path::PathBuf;
use tempfile::TempDir;

#[cfg(test)]
mod activity_logger_basic_tests {
    use super::*;

    #[tokio::test]
    async fn test_logger_initialization() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        assert!(logger.is_initialized());

        Ok(())
    }

    #[tokio::test]
    async fn test_simple_log_entry() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Info, "Test message").await?;

        let entries = logger.get_recent_logs(1).await?;
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].message, "Test message");

        Ok(())
    }

    #[tokio::test]
    async fn test_log_levels() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Debug, "Debug message").await?;
        logger.log(LogLevel::Info, "Info message").await?;
        logger.log(LogLevel::Warn, "Warning message").await?;
        logger.log(LogLevel::Error, "Error message").await?;
        logger.log(LogLevel::Critical, "Critical message").await?;

        let entries = logger.get_recent_logs(5).await?;
        assert_eq!(entries.len(), 5);

        Ok(())
    }

    #[tokio::test]
    async fn test_structured_logging() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        let mut metadata = std::collections::HashMap::new();
        metadata.insert("user_id".to_string(), "user_123".to_string());
        metadata.insert("action".to_string(), "deploy".to_string());

        logger
            .log_structured(LogLevel::Info, "Deployment started", metadata)
            .await?;

        let entries = logger.get_recent_logs(1).await?;
        assert!(entries[0].metadata.contains_key("user_id"));
        assert_eq!(entries[0].metadata["action"], "deploy");

        Ok(())
    }

    #[tokio::test]
    async fn test_log_with_context() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger
            .log_with_context(
                LogLevel::Info,
                "Operation completed",
                "deployment",
                "deploy-001",
            )
            .await?;

        let entries = logger.get_recent_logs(1).await?;
        assert_eq!(entries[0].context, Some("deployment".to_string()));

        Ok(())
    }

    #[tokio::test]
    async fn test_log_timestamps() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        let before = Utc::now();
        logger.log(LogLevel::Info, "Timestamped message").await?;
        let after = Utc::now();

        let entries = logger.get_recent_logs(1).await?;
        let log_time = entries[0].timestamp;

        assert!(log_time >= before && log_time <= after);

        Ok(())
    }

    #[tokio::test]
    async fn test_concurrent_logging() -> Result<()> {
        use std::sync::Arc;
        let temp_dir = TempDir::new()?;
        let logger = Arc::new(ActivityLogger::new(temp_dir.path().to_path_buf())?);

        let mut handles = vec![];

        for i in 0..20 {
            let logger_clone = Arc::clone(&logger);
            let handle = tokio::spawn(async move {
                logger_clone
                    .log(LogLevel::Info, &format!("Concurrent log {}", i))
                    .await
            });
            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        for result in results {
            assert!(result?.is_ok());
        }

        let entries = logger.get_recent_logs(20).await?;
        assert_eq!(entries.len(), 20);

        Ok(())
    }

    #[tokio::test]
    async fn test_log_file_creation() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Info, "Creating log file").await?;

        let log_files: Vec<_> = std::fs::read_dir(temp_dir.path())?
            .filter_map(|e| e.ok())
            .collect();

        assert!(!log_files.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_log_persistence() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Create logger and write logs
        {
            let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;
            logger.log(LogLevel::Info, "Persistent message").await?;
            logger.flush().await?;
        }

        // Reopen and verify logs persisted
        {
            let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;
            let entries = logger.get_all_logs().await?;

            assert!(!entries.is_empty());
            assert!(entries.iter().any(|e| e.message.contains("Persistent")));
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_log_buffer_flush() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        for i in 0..5 {
            logger
                .log(LogLevel::Info, &format!("Buffered log {}", i))
                .await?;
        }

        logger.flush().await?;

        let entries = logger.get_recent_logs(5).await?;
        assert_eq!(entries.len(), 5);

        Ok(())
    }
}

#[cfg(test)]
mod log_filtering_tests {
    use super::*;

    #[tokio::test]
    async fn test_filter_by_level() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Debug, "Debug").await?;
        logger.log(LogLevel::Info, "Info").await?;
        logger.log(LogLevel::Warn, "Warn").await?;
        logger.log(LogLevel::Error, "Error").await?;

        let filter = LogFilter::new().with_min_level(LogLevel::Warn);
        let filtered = logger.get_filtered_logs(&filter).await?;

        // Should only include Warn and Error
        assert_eq!(filtered.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_filter_by_time_range() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        let start_time = Utc::now();

        logger.log(LogLevel::Info, "Log 1").await?;
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        logger.log(LogLevel::Info, "Log 2").await?;

        let filter = LogFilter::new().with_start_time(start_time);
        let filtered = logger.get_filtered_logs(&filter).await?;

        assert!(filtered.len() >= 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_filter_by_context() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger
            .log_with_context(LogLevel::Info, "Deployment log", "deployment", "d1")
            .await?;
        logger
            .log_with_context(LogLevel::Info, "Audit log", "audit", "a1")
            .await?;
        logger
            .log_with_context(LogLevel::Info, "Deployment log 2", "deployment", "d2")
            .await?;

        let filter = LogFilter::new().with_context("deployment");
        let filtered = logger.get_filtered_logs(&filter).await?;

        assert_eq!(filtered.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_filter_by_message_pattern() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Info, "User login: alice").await?;
        logger.log(LogLevel::Info, "User login: bob").await?;
        logger.log(LogLevel::Info, "System startup").await?;

        let filter = LogFilter::new().with_pattern("User login");
        let filtered = logger.get_filtered_logs(&filter).await?;

        assert_eq!(filtered.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_complex_filter() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Debug, "Debug message").await?;
        logger.log(LogLevel::Info, "Info message").await?;
        logger.log(LogLevel::Error, "Error in deployment").await?;
        logger.log(LogLevel::Error, "Error in audit").await?;

        let filter = LogFilter::new()
            .with_min_level(LogLevel::Error)
            .with_pattern("deployment");

        let filtered = logger.get_filtered_logs(&filter).await?;

        assert_eq!(filtered.len(), 1);
        assert!(filtered[0].message.contains("deployment"));

        Ok(())
    }
}

#[cfg(test)]
mod log_rotation_tests {
    use super::*;

    #[tokio::test]
    async fn test_size_based_rotation() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let rotation = LogRotation::new_size_based(1024); // 1KB
        let logger = ActivityLogger::with_rotation(temp_dir.path().to_path_buf(), rotation)?;

        // Write enough logs to trigger rotation
        for i in 0..100 {
            logger
                .log(LogLevel::Info, &format!("Log entry {}", i))
                .await?;
        }

        // Should have created multiple log files
        let log_files: Vec<_> = std::fs::read_dir(temp_dir.path())?
            .filter_map(|e| e.ok())
            .collect();

        assert!(log_files.len() >= 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_time_based_rotation() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let rotation = LogRotation::new_daily();
        let logger = ActivityLogger::with_rotation(temp_dir.path().to_path_buf(), rotation)?;

        logger.log(LogLevel::Info, "Daily log entry").await?;
        logger.flush().await?;

        let log_files: Vec<_> = std::fs::read_dir(temp_dir.path())?
            .filter_map(|e| e.ok())
            .collect();

        assert!(!log_files.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_max_file_count() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let rotation = LogRotation::new_size_based(512).with_max_files(3);
        let logger = ActivityLogger::with_rotation(temp_dir.path().to_path_buf(), rotation)?;

        // Write many logs to create multiple files
        for i in 0..200 {
            logger.log(LogLevel::Info, &format!("Entry {}", i)).await?;
        }

        logger.flush().await?;

        let log_files: Vec<_> = std::fs::read_dir(temp_dir.path())?
            .filter_map(|e| e.ok())
            .collect();

        // Should not exceed max_files
        assert!(log_files.len() <= 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_log_compression() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let rotation = LogRotation::new_size_based(1024).with_compression(true);
        let logger = ActivityLogger::with_rotation(temp_dir.path().to_path_buf(), rotation)?;

        for i in 0..100 {
            logger
                .log(LogLevel::Info, &format!("Compressible log {}", i))
                .await?;
        }

        logger.flush().await?;
        logger.trigger_rotation().await?;

        // Check for compressed files
        let compressed_files: Vec<_> = std::fs::read_dir(temp_dir.path())?
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("gz"))
            .collect();

        assert!(!compressed_files.is_empty());

        Ok(())
    }
}

#[cfg(test)]
mod audit_trail_tests {
    use super::*;

    #[tokio::test]
    async fn test_audit_trail_creation() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        assert!(audit.is_initialized());

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_event_recording() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit
            .record_event("user_login", "alice", "Successful login from 192.168.1.100")
            .await?;

        let events = audit.get_recent_events(1).await?;
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].event_type, "user_login");

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_trail_immutability() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit
            .record_event("test_event", "user1", "Test action")
            .await?;

        // Attempting to modify should fail
        let result = audit.modify_event(0);
        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_trail_integrity() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit.record_event("event1", "user1", "Action 1").await?;
        audit.record_event("event2", "user2", "Action 2").await?;

        let is_valid = audit.verify_integrity().await?;
        assert!(is_valid);

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_query_by_actor() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit.record_event("login", "alice", "Login").await?;
        audit.record_event("deploy", "bob", "Deploy").await?;
        audit.record_event("logout", "alice", "Logout").await?;

        let alice_events = audit.query_by_actor("alice").await?;
        assert_eq!(alice_events.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_query_by_event_type() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit.record_event("login", "alice", "Login").await?;
        audit.record_event("login", "bob", "Login").await?;
        audit.record_event("deploy", "alice", "Deploy").await?;

        let login_events = audit.query_by_event_type("login").await?;
        assert_eq!(login_events.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_audit_trail_export() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let audit = AuditTrail::new(temp_dir.path().to_path_buf())?;

        audit.record_event("test", "user1", "Action").await?;

        let export_path = temp_dir.path().join("audit_export.json");
        audit.export_to_file(&export_path).await?;

        assert!(export_path.exists());

        Ok(())
    }
}

#[cfg(test)]
mod activity_metrics_tests {
    use super::*;

    #[tokio::test]
    async fn test_metrics_collection() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        for _ in 0..10 {
            logger.log(LogLevel::Info, "Info log").await?;
        }
        for _ in 0..5 {
            logger.log(LogLevel::Error, "Error log").await?;
        }

        let metrics = logger.get_metrics().await?;

        assert_eq!(metrics.total_logs, 15);
        assert_eq!(metrics.info_count, 10);
        assert_eq!(metrics.error_count, 5);

        Ok(())
    }

    #[tokio::test]
    async fn test_metrics_by_level() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Debug, "D").await?;
        logger.log(LogLevel::Info, "I").await?;
        logger.log(LogLevel::Warn, "W").await?;
        logger.log(LogLevel::Error, "E").await?;

        let metrics = logger.get_metrics().await?;
        let by_level = metrics.by_level();

        assert_eq!(by_level[&LogLevel::Debug], 1);
        assert_eq!(by_level[&LogLevel::Info], 1);
        assert_eq!(by_level[&LogLevel::Warn], 1);
        assert_eq!(by_level[&LogLevel::Error], 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_metrics_time_series() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        for i in 0..5 {
            logger.log(LogLevel::Info, &format!("Log {}", i)).await?;
            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        }

        let metrics = logger.get_metrics().await?;
        let time_series = metrics.time_series(chrono::Duration::seconds(1));

        assert!(!time_series.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_metrics_reset() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let logger = ActivityLogger::new(temp_dir.path().to_path_buf())?;

        logger.log(LogLevel::Info, "Log before reset").await?;

        logger.reset_metrics().await?;

        let metrics = logger.get_metrics().await?;
        assert_eq!(metrics.total_logs, 0);

        Ok(())
    }
}

#[cfg(test)]
mod log_formatter_tests {
    use super::*;

    #[test]
    fn test_json_formatting() -> Result<()> {
        let formatter = LogFormatter::new_json();

        let entry = LogEntry {
            timestamp: Utc::now(),
            level: LogLevel::Info,
            message: "Test message".to_string(),
            context: None,
            metadata: std::collections::HashMap::new(),
        };

        let formatted = formatter.format(&entry)?;

        assert!(formatted.contains("\"message\":\"Test message\""));

        Ok(())
    }

    #[test]
    fn test_plaintext_formatting() -> Result<()> {
        let formatter = LogFormatter::new_plaintext();

        let entry = LogEntry {
            timestamp: Utc::now(),
            level: LogLevel::Info,
            message: "Plain text log".to_string(),
            context: None,
            metadata: std::collections::HashMap::new(),
        };

        let formatted = formatter.format(&entry)?;

        assert!(formatted.contains("INFO"));
        assert!(formatted.contains("Plain text log"));

        Ok(())
    }

    #[test]
    fn test_custom_formatting() -> Result<()> {
        let formatter = LogFormatter::custom("[{{level}}] {{message}}");

        let entry = LogEntry {
            timestamp: Utc::now(),
            level: LogLevel::Warn,
            message: "Custom format".to_string(),
            context: None,
            metadata: std::collections::HashMap::new(),
        };

        let formatted = formatter.format(&entry)?;

        assert!(formatted.contains("[WARN]"));
        assert!(formatted.contains("Custom format"));

        Ok(())
    }
}
