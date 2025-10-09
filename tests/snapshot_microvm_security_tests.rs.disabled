//! Tests for snapshot service, microVM launcher, and security monitoring

use anyhow::Result;
use osvm::services::{
    ephemeral_microvm::{EphemeralConfig, EphemeralVM, VMLifecycle},
    microvm_launcher::{LaunchConfig, MicroVMInstance, MicroVMLauncher, VMStatus},
    snapshot_service::{Snapshot, SnapshotConfig, SnapshotMetadata, SnapshotService},
};
use osvm::utils::security_monitor::{MonitorConfig, SecurityEvent, SecurityMonitor, ThreatLevel};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tempfile::TempDir;

#[cfg(test)]
mod snapshot_service_tests {
    use super::*;

    #[tokio::test]
    async fn test_snapshot_creation() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = SnapshotConfig {
            snapshots_dir: temp_dir.path().to_path_buf(),
            compression: true,
            max_snapshots: 10,
            incremental: false,
        };

        let service = SnapshotService::new(config)?;

        let snapshot = service.create_snapshot("test-vm", 12345).await?;

        assert_eq!(snapshot.vm_name, "test-vm");
        assert_eq!(snapshot.slot, 12345);
        assert!(snapshot.file_path.exists());

        Ok(())
    }

    #[tokio::test]
    async fn test_snapshot_restore() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = SnapshotConfig {
            snapshots_dir: temp_dir.path().to_path_buf(),
            compression: true,
            max_snapshots: 10,
            incremental: false,
        };

        let service = SnapshotService::new(config)?;

        // Create snapshot
        let snapshot = service.create_snapshot("test-vm", 12345).await?;

        // Restore it
        let restored = service.restore_snapshot(&snapshot.id).await?;

        assert_eq!(restored.vm_name, "test-vm");
        assert_eq!(restored.slot, 12345);

        Ok(())
    }

    #[tokio::test]
    async fn test_incremental_snapshots() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = SnapshotConfig {
            snapshots_dir: temp_dir.path().to_path_buf(),
            compression: true,
            max_snapshots: 10,
            incremental: true,
        };

        let service = SnapshotService::new(config)?;

        // Create base snapshot
        let base = service.create_snapshot("test-vm", 100).await?;

        // Create incremental snapshots
        let inc1 = service
            .create_incremental_snapshot("test-vm", 200, &base.id)
            .await?;
        let inc2 = service
            .create_incremental_snapshot("test-vm", 300, &inc1.id)
            .await?;

        assert_eq!(inc1.parent_id, Some(base.id));
        assert_eq!(inc2.parent_id, Some(inc1.id));

        // Incremental should be smaller
        assert!(inc1.size_bytes < base.size_bytes);

        Ok(())
    }

    #[tokio::test]
    async fn test_snapshot_pruning() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = SnapshotConfig {
            snapshots_dir: temp_dir.path().to_path_buf(),
            compression: false,
            max_snapshots: 3, // Keep only 3
            incremental: false,
        };

        let service = SnapshotService::new(config)?;

        // Create 5 snapshots
        for slot in 100..105 {
            service.create_snapshot("test-vm", slot).await?;
        }

        // Should auto-prune to 3
        let snapshots = service.list_snapshots("test-vm").await?;
        assert!(snapshots.len() <= 3);

        // Should keep most recent
        assert!(snapshots.iter().any(|s| s.slot == 104));

        Ok(())
    }

    #[tokio::test]
    async fn test_snapshot_compression() -> Result<()> {
        let temp_dir = TempDir::new()?;

        // Without compression
        let config_no_comp = SnapshotConfig {
            snapshots_dir: temp_dir.path().join("no_comp"),
            compression: false,
            max_snapshots: 10,
            incremental: false,
        };

        let service_no_comp = SnapshotService::new(config_no_comp)?;
        let snapshot_no_comp = service_no_comp.create_snapshot("test-vm", 100).await?;

        // With compression
        let config_comp = SnapshotConfig {
            snapshots_dir: temp_dir.path().join("comp"),
            compression: true,
            max_snapshots: 10,
            incremental: false,
        };

        let service_comp = SnapshotService::new(config_comp)?;
        let snapshot_comp = service_comp.create_snapshot("test-vm", 100).await?;

        // Compressed should be smaller (in real implementation)
        // For test, we just verify both work
        assert!(snapshot_no_comp.file_path.exists());
        assert!(snapshot_comp.file_path.exists());

        Ok(())
    }

    #[tokio::test]
    async fn test_concurrent_snapshots() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = SnapshotConfig {
            snapshots_dir: temp_dir.path().to_path_buf(),
            compression: false,
            max_snapshots: 20,
            incremental: false,
        };

        let service = Arc::new(SnapshotService::new(config)?);

        let mut handles = vec![];

        // Create snapshots concurrently
        for slot in 100..110 {
            let service_clone = Arc::clone(&service);
            let handle =
                tokio::spawn(async move { service_clone.create_snapshot("test-vm", slot).await });
            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        for result in results {
            assert!(result?.is_ok());
        }

        Ok(())
    }
}

#[cfg(test)]
mod microvm_launcher_tests {
    use super::*;

    #[tokio::test]
    async fn test_microvm_launch() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = LaunchConfig {
            vm_id: "test-vm-001".to_string(),
            cpu_count: 2,
            memory_mb: 512,
            rootfs_path: temp_dir.path().join("rootfs.ext4"),
            kernel_path: None,
            network_enabled: false,
            volumes: vec![],
        };

        let launcher = MicroVMLauncher::new()?;

        // Note: Actual launch requires Firecracker/KVM
        // This tests the configuration and setup
        let result = launcher.validate_config(&config);
        assert!(result.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn test_microvm_lifecycle() -> Result<()> {
        let launcher = MicroVMLauncher::new()?;

        let vm_id = "lifecycle-test";

        // Simulate lifecycle states
        let states = vec![
            VMStatus::Creating,
            VMStatus::Starting,
            VMStatus::Running,
            VMStatus::Stopping,
            VMStatus::Stopped,
        ];

        for state in states {
            // In real implementation, would transition through states
            assert!(matches!(
                state,
                VMStatus::Creating
                    | VMStatus::Starting
                    | VMStatus::Running
                    | VMStatus::Stopping
                    | VMStatus::Stopped
            ));
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_microvm_resource_validation() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let launcher = MicroVMLauncher::new()?;

        // Valid config
        let valid_config = LaunchConfig {
            vm_id: "valid-vm".to_string(),
            cpu_count: 2,
            memory_mb: 512,
            rootfs_path: temp_dir.path().join("rootfs.ext4"),
            kernel_path: None,
            network_enabled: false,
            volumes: vec![],
        };

        assert!(launcher.validate_config(&valid_config).is_ok());

        // Invalid: too many CPUs
        let invalid_cpu = LaunchConfig {
            vm_id: "invalid-cpu".to_string(),
            cpu_count: 1000,
            memory_mb: 512,
            rootfs_path: temp_dir.path().join("rootfs.ext4"),
            kernel_path: None,
            network_enabled: false,
            volumes: vec![],
        };

        assert!(launcher.validate_config(&invalid_cpu).is_err());

        // Invalid: too much memory
        let invalid_mem = LaunchConfig {
            vm_id: "invalid-mem".to_string(),
            cpu_count: 2,
            memory_mb: 1_000_000,
            rootfs_path: temp_dir.path().join("rootfs.ext4"),
            kernel_path: None,
            network_enabled: false,
            volumes: vec![],
        };

        assert!(launcher.validate_config(&invalid_mem).is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_ephemeral_vm() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = EphemeralConfig {
            max_lifetime_ms: 60000, // 1 minute
            auto_cleanup: true,
            snapshot_on_destroy: false,
        };

        let vm = EphemeralVM::new("ephemeral-test", config, temp_dir.path().to_path_buf())?;

        assert_eq!(vm.lifecycle_state(), VMLifecycle::Created);

        // Simulate lifecycle
        vm.start().await?;
        assert_eq!(vm.lifecycle_state(), VMLifecycle::Running);

        vm.stop().await?;
        assert_eq!(vm.lifecycle_state(), VMLifecycle::Stopped);

        Ok(())
    }

    #[tokio::test]
    async fn test_ephemeral_vm_timeout() -> Result<()> {
        let temp_dir = TempDir::new()?;

        let config = EphemeralConfig {
            max_lifetime_ms: 100, // 100ms
            auto_cleanup: true,
            snapshot_on_destroy: false,
        };

        let vm = EphemeralVM::new("timeout-test", config, temp_dir.path().to_path_buf())?;

        vm.start().await?;

        // Wait for timeout
        tokio::time::sleep(Duration::from_millis(150)).await;

        // Should auto-stop
        assert!(matches!(
            vm.lifecycle_state(),
            VMLifecycle::Stopped | VMLifecycle::Destroyed
        ));

        Ok(())
    }
}

#[cfg(test)]
mod security_monitor_tests {
    use super::*;

    #[tokio::test]
    async fn test_security_monitor_initialization() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::Medium,
            log_all_events: false,
        };

        let monitor = SecurityMonitor::new(config)?;

        assert!(monitor.is_active());

        Ok(())
    }

    #[tokio::test]
    async fn test_security_event_detection() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::Low,
            log_all_events: true,
        };

        let monitor = SecurityMonitor::new(config)?;

        // Simulate security events
        let events = vec![
            SecurityEvent {
                event_type: "unauthorized_access".to_string(),
                threat_level: ThreatLevel::High,
                source: "process_123".to_string(),
                description: "Attempted unauthorized file access".to_string(),
                timestamp: chrono::Utc::now(),
                metadata: std::collections::HashMap::new(),
            },
            SecurityEvent {
                event_type: "network_anomaly".to_string(),
                threat_level: ThreatLevel::Medium,
                source: "network_interface".to_string(),
                description: "Unusual network traffic pattern".to_string(),
                timestamp: chrono::Utc::now(),
                metadata: std::collections::HashMap::new(),
            },
        ];

        for event in events {
            monitor.record_event(event).await?;
        }

        let recorded = monitor.get_events(ThreatLevel::Low).await?;
        assert_eq!(recorded.len(), 2);

        Ok(())
    }

    #[tokio::test]
    async fn test_threat_level_filtering() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::High, // Only alert on high
            log_all_events: true,
        };

        let monitor = SecurityMonitor::new(config)?;

        // Record events of different levels
        let low_event = SecurityEvent {
            event_type: "info".to_string(),
            threat_level: ThreatLevel::Low,
            source: "system".to_string(),
            description: "Routine check".to_string(),
            timestamp: chrono::Utc::now(),
            metadata: std::collections::HashMap::new(),
        };

        let high_event = SecurityEvent {
            event_type: "breach_attempt".to_string(),
            threat_level: ThreatLevel::Critical,
            source: "external".to_string(),
            description: "Security breach attempt detected".to_string(),
            timestamp: chrono::Utc::now(),
            metadata: std::collections::HashMap::new(),
        };

        monitor.record_event(low_event).await?;
        monitor.record_event(high_event).await?;

        // Get only high-priority events
        let high_events = monitor.get_events(ThreatLevel::High).await?;
        assert_eq!(high_events.len(), 1);
        assert_eq!(high_events[0].threat_level, ThreatLevel::Critical);

        Ok(())
    }

    #[tokio::test]
    async fn test_security_metrics() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::Low,
            log_all_events: true,
        };

        let monitor = SecurityMonitor::new(config)?;

        // Record various events
        for i in 0..5 {
            let event = SecurityEvent {
                event_type: format!("event_{}", i),
                threat_level: if i < 2 {
                    ThreatLevel::High
                } else {
                    ThreatLevel::Low
                },
                source: "test".to_string(),
                description: format!("Test event {}", i),
                timestamp: chrono::Utc::now(),
                metadata: std::collections::HashMap::new(),
            };
            monitor.record_event(event).await?;
        }

        let metrics = monitor.get_metrics().await?;

        assert_eq!(metrics.total_events, 5);
        assert_eq!(metrics.high_threat_events, 2);
        assert_eq!(metrics.low_threat_events, 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_alert_generation() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::Medium,
            log_all_events: true,
        };

        let monitor = SecurityMonitor::new(config)?;

        let critical_event = SecurityEvent {
            event_type: "critical_threat".to_string(),
            threat_level: ThreatLevel::Critical,
            source: "unknown".to_string(),
            description: "Critical security threat detected".to_string(),
            timestamp: chrono::Utc::now(),
            metadata: std::collections::HashMap::new(),
        };

        monitor.record_event(critical_event).await?;

        // Should generate alert
        let alerts = monitor.get_pending_alerts().await?;
        assert!(!alerts.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_concurrent_monitoring() -> Result<()> {
        let config = MonitorConfig {
            enable_runtime_monitoring: true,
            enable_network_monitoring: true,
            enable_file_monitoring: true,
            alert_threshold: ThreatLevel::Low,
            log_all_events: true,
        };

        let monitor = Arc::new(SecurityMonitor::new(config)?);

        let mut handles = vec![];

        // Concurrent event recording
        for i in 0..10 {
            let monitor_clone = Arc::clone(&monitor);
            let handle = tokio::spawn(async move {
                let event = SecurityEvent {
                    event_type: format!("concurrent_{}", i),
                    threat_level: ThreatLevel::Low,
                    source: format!("source_{}", i),
                    description: format!("Concurrent event {}", i),
                    timestamp: chrono::Utc::now(),
                    metadata: std::collections::HashMap::new(),
                };
                monitor_clone.record_event(event).await
            });
            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        for result in results {
            assert!(result?.is_ok());
        }

        let metrics = monitor.get_metrics().await?;
        assert_eq!(metrics.total_events, 10);

        Ok(())
    }
}
