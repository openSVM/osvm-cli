//! Forensics configuration for customizable thresholds and detection sensitivity

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use anyhow::{Result, Context};

/// Forensic analysis configuration with adjustable thresholds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForensicsConfig {
    pub thresholds: ThresholdConfig,
    pub behavior: BehaviorConfig,
    pub risk: RiskConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThresholdConfig {
    /// Minimum SOL amount to be considered a "whale" transfer
    pub whale_amount_sol: f64,

    /// Critical rapid activity threshold (transactions per minute)
    pub rapid_txns_critical: f64,

    /// Critical rapid volume threshold (SOL per minute)
    pub rapid_volume_critical: f64,

    /// High complexity threshold (edges per node ratio)
    pub complexity_high: f64,

    /// Suspicious complexity threshold (edges per node ratio)
    pub complexity_suspicious: f64,

    /// Time windows for rapid transfer detection (seconds)
    pub time_windows: Vec<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BehaviorConfig {
    /// Minimum SOL average for mixer detection
    pub mixer_max_avg_amount: f64,

    /// Minimum counterparties for mixer classification
    pub mixer_min_counterparties: usize,

    /// Minimum SOL volume for exchange classification
    pub exchange_min_volume: f64,

    /// Minimum counterparties for exchange classification
    pub exchange_min_counterparties: usize,

    /// Minimum transactions for bot detection
    pub bot_min_transactions: usize,

    /// Maximum timing variance ratio for bot detection (std_dev / mean)
    pub bot_max_variance_ratio: f64,

    /// Minimum transactions to avoid "dormant" classification
    pub dormant_max_transactions: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskConfig {
    /// Weight for network complexity in risk score (0-100)
    pub complexity_weight: f64,

    /// Weight for rapid transfers in risk score (per burst, 0-100)
    pub rapid_transfer_weight: f64,

    /// Weight for circular flows in risk score (per cycle, 0-100)
    pub circular_flow_weight: f64,

    /// Weight for whale activity in risk score (0-100)
    pub whale_activity_weight: f64,

    /// Weight for mixer behavior in risk score (0-100)
    pub mixer_behavior_weight: f64,

    /// Weight for bot behavior in risk score (0-100)
    pub bot_behavior_weight: f64,

    /// Risk level thresholds
    pub critical_threshold: f64,
    pub high_threshold: f64,
    pub medium_threshold: f64,
}

impl Default for ForensicsConfig {
    fn default() -> Self {
        Self {
            thresholds: ThresholdConfig {
                whale_amount_sol: 100.0,
                rapid_txns_critical: 20.0,
                rapid_volume_critical: 1000.0,
                complexity_high: 5.0,
                complexity_suspicious: 3.0,
                time_windows: vec![60, 300, 600, 3600], // 1min, 5min, 10min, 1hr
            },
            behavior: BehaviorConfig {
                mixer_max_avg_amount: 1.0,
                mixer_min_counterparties: 20,
                exchange_min_volume: 10000.0,
                exchange_min_counterparties: 50,
                bot_min_transactions: 20,
                bot_max_variance_ratio: 0.2,
                dormant_max_transactions: 5,
            },
            risk: RiskConfig {
                complexity_weight: 30.0,
                rapid_transfer_weight: 15.0,
                circular_flow_weight: 20.0,
                whale_activity_weight: 15.0,
                mixer_behavior_weight: 25.0,
                bot_behavior_weight: 10.0,
                critical_threshold: 75.0,
                high_threshold: 50.0,
                medium_threshold: 25.0,
            },
        }
    }
}

impl ForensicsConfig {
    /// Load configuration from file, or create default if it doesn't exist
    pub fn load() -> Result<Self> {
        let config_path = Self::config_path()?;

        if config_path.exists() {
            let content = fs::read_to_string(&config_path)
                .context("Failed to read forensics config file")?;

            toml::from_str(&content)
                .context("Failed to parse forensics config TOML")
        } else {
            // Create default config
            let config = Self::default();
            config.save()?;
            Ok(config)
        }
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = Self::config_path()?;

        // Ensure parent directory exists
        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)
                .context("Failed to create config directory")?;
        }

        let content = toml::to_string_pretty(self)
            .context("Failed to serialize forensics config")?;

        fs::write(&config_path, content)
            .context("Failed to write forensics config file")?;

        Ok(())
    }

    /// Get the config file path
    fn config_path() -> Result<PathBuf> {
        let home = dirs::home_dir()
            .context("Failed to get home directory")?;

        Ok(home.join(".osvm").join("forensics_config.toml"))
    }

    /// Get the config file path as a string (for display)
    pub fn config_path_str() -> String {
        Self::config_path()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| "~/.osvm/forensics_config.toml".to_string())
    }

    /// Reset to default configuration
    pub fn reset_to_default() -> Result<Self> {
        let config = Self::default();
        config.save()?;
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = ForensicsConfig::default();
        assert_eq!(config.thresholds.whale_amount_sol, 100.0);
        assert_eq!(config.behavior.bot_min_transactions, 20);
        assert_eq!(config.risk.critical_threshold, 75.0);
    }

    #[test]
    fn test_serialization() {
        let config = ForensicsConfig::default();
        let toml = toml::to_string(&config).unwrap();
        let deserialized: ForensicsConfig = toml::from_str(&toml).unwrap();
        assert_eq!(config.thresholds.whale_amount_sol, deserialized.thresholds.whale_amount_sol);
    }
}
