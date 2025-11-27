//! Self-Improving LSP Telemetry and Learning System
//!
//! Tracks usage patterns and adapts the LSP behavior over time:
//! - Completion acceptance/rejection rates
//! - Function hover frequency (what users look up)
//! - Error patterns and fixes
//! - Investigation workflows (sequences of operations)
//!
//! # Privacy
//! All data is stored locally in ~/.ovsm/lsp_learning.json
//! No data is sent externally unless explicitly configured.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::RwLock;

/// Configuration for the learning system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LearningConfig {
    /// Enable/disable learning (default: true)
    pub enabled: bool,
    /// Path to store learning data
    pub data_path: PathBuf,
    /// Minimum events before pattern analysis kicks in
    pub min_events_for_analysis: usize,
    /// Decay factor for older events (0.0-1.0, higher = longer memory)
    pub decay_factor: f64,
    /// Auto-save interval in seconds
    pub save_interval_secs: u64,
}

impl Default for LearningConfig {
    fn default() -> Self {
        let data_path = dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".ovsm")
            .join("lsp_learning.json");

        Self {
            enabled: true,
            data_path,
            min_events_for_analysis: 10,
            decay_factor: 0.95,
            save_interval_secs: 60,
        }
    }
}

/// Types of events we track
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum EventType {
    /// User accepted a completion
    CompletionAccepted { label: String },
    /// User rejected/ignored a completion
    CompletionIgnored { label: String },
    /// User hovered over a function/symbol
    HoverLookup { symbol: String },
    /// User triggered go-to-definition
    GotoDefinition { symbol: String },
    /// Code execution (REPL)
    CodeExecuted { function: String, success: bool },
    /// Error encountered
    ErrorEncountered { error_type: String, context: String },
    /// Error was fixed (same location, error gone)
    ErrorFixed { error_type: String },
    /// Investigation sequence started
    InvestigationStart { topic: String },
    /// Wallet address analyzed
    WalletAnalyzed { address_prefix: String },
}

/// A single logged event with timestamp
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub event_type: EventType,
    pub timestamp: u64,
    pub session_id: String,
}

/// Learned patterns from usage data
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LearnedPatterns {
    /// Function usage frequency (higher = suggest first)
    pub function_frequency: HashMap<String, f64>,
    /// Completion acceptance rates (label -> accept_rate)
    pub completion_rates: HashMap<String, f64>,
    /// Common investigation sequences
    pub investigation_sequences: Vec<InvestigationSequence>,
    /// Error-fix mappings
    pub error_fixes: HashMap<String, Vec<String>>,
    /// Frequently analyzed wallet prefixes
    pub wallet_patterns: HashMap<String, usize>,
    /// User's preferred coding style
    pub style_preferences: StylePreferences,
}

/// A learned investigation workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvestigationSequence {
    /// Functions called in order
    pub functions: Vec<String>,
    /// How often this sequence occurs
    pub frequency: usize,
    /// Confidence score (0.0-1.0)
    pub confidence: f64,
}

/// User's coding style preferences
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StylePreferences {
    /// Prefers verbose variable names
    pub verbose_names: bool,
    /// Uses define vs let
    pub define_over_let: bool,
    /// Typical indentation (spaces)
    pub indentation: usize,
    /// Uses comments frequently
    pub uses_comments: bool,
}

/// The main learning engine
#[derive(Debug)]
pub struct LearningEngine {
    config: LearningConfig,
    events: Arc<RwLock<Vec<Event>>>,
    patterns: Arc<RwLock<LearnedPatterns>>,
    session_id: String,
    last_save: Arc<RwLock<SystemTime>>,
}

impl LearningEngine {
    /// Create a new learning engine
    pub fn new(config: LearningConfig) -> Self {
        let session_id = format!("{:x}", SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis());

        let engine = Self {
            config: config.clone(),
            events: Arc::new(RwLock::new(Vec::new())),
            patterns: Arc::new(RwLock::new(LearnedPatterns::default())),
            session_id,
            last_save: Arc::new(RwLock::new(SystemTime::now())),
        };

        // Try to load existing data (only if not in an async context)
        // We use try_write to avoid blocking in async contexts
        if config.data_path.exists() {
            if let Ok(data) = fs::read_to_string(&config.data_path) {
                if let Ok(saved) = serde_json::from_str::<SavedLearningData>(&data) {
                    let event_count = saved.events.len();

                    // Use try_write to avoid panicking if called from async context
                    if let Ok(mut events) = engine.events.try_write() {
                        *events = saved.events;
                        drop(events);

                        if let Ok(mut patterns) = engine.patterns.try_write() {
                            *patterns = saved.patterns;
                            drop(patterns);

                            tracing::info!("Loaded {} learning events from disk", event_count);
                        }
                    } else {
                        tracing::debug!("Skipping load: write lock not available (async context)");
                    }
                }
            }
        }

        engine
    }

    /// Create with default config
    pub fn default() -> Self {
        Self::new(LearningConfig::default())
    }

    /// Log an event
    pub async fn log_event(&self, event_type: EventType) {
        if !self.config.enabled {
            return;
        }

        let event = Event {
            event_type,
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            session_id: self.session_id.clone(),
        };

        let mut events = self.events.write().await;
        events.push(event);

        // Check if we should save
        let should_save = {
            let last_save = self.last_save.read().await;
            last_save.elapsed().unwrap_or_default() > Duration::from_secs(self.config.save_interval_secs)
        };

        if should_save && events.len() >= self.config.min_events_for_analysis {
            drop(events);
            self.analyze_and_save().await;
        }
    }

    /// Log a completion being shown
    pub async fn log_completion_shown(&self, label: &str) {
        // We'll track this separately and compare with accepted
        // For now, just note it was shown
        self.log_event(EventType::CompletionIgnored {
            label: label.to_string()
        }).await;
    }

    /// Log a completion being accepted
    pub async fn log_completion_accepted(&self, label: &str) {
        self.log_event(EventType::CompletionAccepted {
            label: label.to_string()
        }).await;
    }

    /// Log a hover lookup
    pub async fn log_hover(&self, symbol: &str) {
        self.log_event(EventType::HoverLookup {
            symbol: symbol.to_string()
        }).await;
    }

    /// Log code execution
    pub async fn log_execution(&self, function: &str, success: bool) {
        self.log_event(EventType::CodeExecuted {
            function: function.to_string(),
            success,
        }).await;
    }

    /// Log an error
    pub async fn log_error(&self, error_type: &str, context: &str) {
        self.log_event(EventType::ErrorEncountered {
            error_type: error_type.to_string(),
            context: context.to_string(),
        }).await;
    }

    /// Log a wallet analysis
    pub async fn log_wallet_analysis(&self, address: &str) {
        // Store just the prefix for privacy
        let prefix = if address.len() > 4 {
            address[..4].to_string()
        } else {
            address.to_string()
        };

        self.log_event(EventType::WalletAnalyzed {
            address_prefix: prefix
        }).await;
    }

    /// Analyze events and update patterns
    pub async fn analyze_and_save(&self) {
        let events = self.events.read().await;
        if events.len() < self.config.min_events_for_analysis {
            return;
        }

        let mut patterns = self.patterns.write().await;

        // Analyze function frequency from hovers and executions
        let mut function_counts: HashMap<String, usize> = HashMap::new();
        for event in events.iter() {
            match &event.event_type {
                EventType::HoverLookup { symbol } => {
                    *function_counts.entry(symbol.clone()).or_default() += 1;
                }
                EventType::CodeExecuted { function, success: true } => {
                    *function_counts.entry(function.clone()).or_default() += 2; // Weight successful execution higher
                }
                _ => {}
            }
        }

        // Convert to frequencies with decay
        let total: usize = function_counts.values().sum();
        if total > 0 {
            for (func, count) in function_counts {
                let old_freq = patterns.function_frequency.get(&func).copied().unwrap_or(0.0);
                let new_freq = count as f64 / total as f64;
                // Exponential moving average
                let combined = old_freq * self.config.decay_factor + new_freq * (1.0 - self.config.decay_factor);
                patterns.function_frequency.insert(func, combined);
            }
        }

        // Analyze completion acceptance rates
        let mut shown: HashMap<String, usize> = HashMap::new();
        let mut accepted: HashMap<String, usize> = HashMap::new();

        for event in events.iter() {
            match &event.event_type {
                EventType::CompletionIgnored { label } => {
                    *shown.entry(label.clone()).or_default() += 1;
                }
                EventType::CompletionAccepted { label } => {
                    *shown.entry(label.clone()).or_default() += 1;
                    *accepted.entry(label.clone()).or_default() += 1;
                }
                _ => {}
            }
        }

        for (label, total) in shown {
            let acc = accepted.get(&label).copied().unwrap_or(0) as f64;
            let rate = acc / total as f64;
            patterns.completion_rates.insert(label, rate);
        }

        // Analyze investigation sequences
        let mut current_sequence: Vec<String> = Vec::new();
        let mut sequences: Vec<Vec<String>> = Vec::new();

        for event in events.iter() {
            if let EventType::CodeExecuted { function, success: true } = &event.event_type {
                current_sequence.push(function.clone());
                if current_sequence.len() >= 3 {
                    sequences.push(current_sequence.clone());
                    current_sequence.remove(0);
                }
            }
        }

        // Find common sequences
        let mut sequence_counts: HashMap<Vec<String>, usize> = HashMap::new();
        for seq in sequences {
            *sequence_counts.entry(seq).or_default() += 1;
        }

        patterns.investigation_sequences = sequence_counts
            .into_iter()
            .filter(|(_, count)| *count >= 2)
            .map(|(functions, frequency)| InvestigationSequence {
                confidence: (frequency as f64 / events.len() as f64).min(1.0),
                functions,
                frequency,
            })
            .collect();

        // Analyze wallet patterns
        for event in events.iter() {
            if let EventType::WalletAnalyzed { address_prefix } = &event.event_type {
                *patterns.wallet_patterns.entry(address_prefix.clone()).or_default() += 1;
            }
        }

        drop(events);
        drop(patterns);

        // Save to disk
        self.save().await;
    }

    /// Save current state to disk
    async fn save(&self) {
        let events = self.events.read().await;
        let patterns = self.patterns.read().await;

        let data = SavedLearningData {
            events: events.clone(),
            patterns: patterns.clone(),
        };

        drop(events);
        drop(patterns);

        // Ensure directory exists
        if let Some(parent) = self.config.data_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        if let Ok(json) = serde_json::to_string_pretty(&data) {
            if let Err(e) = fs::write(&self.config.data_path, json) {
                tracing::warn!("Failed to save learning data: {}", e);
            } else {
                let mut last_save = self.last_save.write().await;
                *last_save = SystemTime::now();
                tracing::debug!("Saved learning data to {:?}", self.config.data_path);
            }
        }
    }

    /// Get completion boost score for a label (higher = suggest first)
    pub async fn get_completion_boost(&self, label: &str) -> f64 {
        let patterns = self.patterns.read().await;

        // Combine frequency and acceptance rate
        let freq = patterns.function_frequency.get(label).copied().unwrap_or(0.0);
        let rate = patterns.completion_rates.get(label).copied().unwrap_or(0.5);

        // Weighted combination: frequency matters more for common operations
        freq * 0.6 + rate * 0.4
    }

    /// Get suggested next functions based on current context
    pub async fn get_next_suggestions(&self, current_function: &str) -> Vec<String> {
        let patterns = self.patterns.read().await;

        let mut suggestions: Vec<(String, f64)> = Vec::new();

        for seq in &patterns.investigation_sequences {
            if let Some(pos) = seq.functions.iter().position(|f| f == current_function) {
                if pos + 1 < seq.functions.len() {
                    let next = seq.functions[pos + 1].clone();
                    let score = seq.confidence * seq.frequency as f64;
                    suggestions.push((next, score));
                }
            }
        }

        // Sort by score and deduplicate
        suggestions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        suggestions.dedup_by(|a, b| a.0 == b.0);

        suggestions.into_iter().map(|(s, _)| s).take(5).collect()
    }

    /// Get frequently used functions for quick access
    pub async fn get_frequent_functions(&self, limit: usize) -> Vec<String> {
        let patterns = self.patterns.read().await;

        let mut funcs: Vec<_> = patterns.function_frequency
            .iter()
            .map(|(k, v)| (k.clone(), *v))
            .collect();

        funcs.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        funcs.into_iter().take(limit).map(|(f, _)| f).collect()
    }

    /// Get learning stats for display
    pub async fn get_stats(&self) -> LearningStats {
        let events = self.events.read().await;
        let patterns = self.patterns.read().await;

        let total_events = events.len();
        let unique_functions = patterns.function_frequency.len();
        let sequences_learned = patterns.investigation_sequences.len();

        let avg_acceptance = if patterns.completion_rates.is_empty() {
            0.0
        } else {
            patterns.completion_rates.values().sum::<f64>() / patterns.completion_rates.len() as f64
        };

        LearningStats {
            total_events,
            unique_functions,
            sequences_learned,
            avg_acceptance_rate: avg_acceptance,
        }
    }

    /// Get a clone of the current learned patterns (for export)
    pub async fn get_patterns(&self) -> LearnedPatterns {
        let patterns = self.patterns.read().await;
        patterns.clone()
    }

    /// Update patterns (for import)
    pub async fn set_patterns(&self, new_patterns: LearnedPatterns) {
        let mut patterns = self.patterns.write().await;
        *patterns = new_patterns;
    }

    /// Merge patterns from another source
    pub async fn merge_patterns(&self, other: &LearnedPatterns, weight: f64) {
        let mut patterns = self.patterns.write().await;

        // Merge function frequencies
        for (func, freq) in &other.function_frequency {
            let existing = patterns.function_frequency.get(func).copied().unwrap_or(0.0);
            let merged = existing * (1.0 - weight) + freq * weight;
            patterns.function_frequency.insert(func.clone(), merged);
        }

        // Add new sequences
        for seq in &other.investigation_sequences {
            if !patterns.investigation_sequences.iter().any(|s| s.functions == seq.functions) {
                patterns.investigation_sequences.push(seq.clone());
            }
        }
    }
}

/// Stats about learning progress
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LearningStats {
    pub total_events: usize,
    pub unique_functions: usize,
    pub sequences_learned: usize,
    pub avg_acceptance_rate: f64,
}

/// Serializable learning data
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SavedLearningData {
    events: Vec<Event>,
    patterns: LearnedPatterns,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_learning_engine_creation() {
        let config = LearningConfig {
            enabled: true,
            data_path: PathBuf::from("/tmp/test_learning.json"),
            min_events_for_analysis: 2,
            decay_factor: 0.9,
            save_interval_secs: 1,
        };

        let engine = LearningEngine::new(config);
        assert!(!engine.session_id.is_empty());
    }

    #[tokio::test]
    async fn test_event_logging() {
        let config = LearningConfig {
            enabled: true,
            data_path: PathBuf::from("/tmp/test_learning_log.json"),
            min_events_for_analysis: 100, // High threshold to prevent auto-analyze
            decay_factor: 0.9,
            save_interval_secs: 3600,
        };

        let engine = LearningEngine::new(config);

        engine.log_hover("getBalance").await;
        engine.log_hover("getBalance").await;
        engine.log_hover("getTransaction").await;
        engine.log_completion_accepted("getBalance").await;

        let events = engine.events.read().await;
        assert_eq!(events.len(), 4);
    }

    #[tokio::test]
    async fn test_completion_boost() {
        let config = LearningConfig {
            enabled: true,
            data_path: PathBuf::from("/tmp/test_learning_boost.json"),
            min_events_for_analysis: 2,
            decay_factor: 0.9,
            save_interval_secs: 3600,
        };

        let engine = LearningEngine::new(config);

        // Log some events
        for _ in 0..5 {
            engine.log_hover("getBalance").await;
            engine.log_completion_accepted("getBalance").await;
        }
        engine.log_hover("getTransaction").await;

        // Manually trigger analysis
        engine.analyze_and_save().await;

        // getBalance should have higher boost
        let balance_boost = engine.get_completion_boost("getBalance").await;
        let tx_boost = engine.get_completion_boost("getTransaction").await;

        assert!(balance_boost > tx_boost);
    }

    #[tokio::test]
    async fn test_sequence_learning() {
        let config = LearningConfig {
            enabled: true,
            data_path: PathBuf::from("/tmp/test_learning_seq.json"),
            min_events_for_analysis: 2,
            decay_factor: 0.9,
            save_interval_secs: 3600,
        };

        let engine = LearningEngine::new(config);

        // Log a repeated sequence
        for _ in 0..3 {
            engine.log_execution("getSignatures", true).await;
            engine.log_execution("getTransaction", true).await;
            engine.log_execution("analyzeTransfer", true).await;
        }

        engine.analyze_and_save().await;

        // Should suggest getTransaction after getSignatures
        let suggestions = engine.get_next_suggestions("getSignatures").await;
        assert!(!suggestions.is_empty());
        assert!(suggestions.contains(&"getTransaction".to_string()));
    }

    #[tokio::test]
    async fn test_stats() {
        // Use unique file name to avoid conflicts with other test runs
        let unique_path = format!("/tmp/test_learning_stats_{}.json", std::process::id());
        let _ = std::fs::remove_file(&unique_path); // Clean up any existing file

        let config = LearningConfig {
            enabled: true,
            data_path: PathBuf::from(&unique_path),
            min_events_for_analysis: 2,
            decay_factor: 0.9,
            save_interval_secs: 3600,
        };

        let engine = LearningEngine::new(config);

        engine.log_hover("func1").await;
        engine.log_hover("func2").await;
        engine.analyze_and_save().await;

        let stats = engine.get_stats().await;
        assert_eq!(stats.total_events, 2);

        // Cleanup
        let _ = std::fs::remove_file(&unique_path);
    }
}
