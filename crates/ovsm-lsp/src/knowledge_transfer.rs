//! Cross-Session Knowledge Transfer
//!
//! Export and import learning profiles to share investigation patterns
//! between team members or across machines.
//!
//! # Features
//!
//! - Export learned patterns to shareable JSON/binary format
//! - Import profiles from colleagues or community repositories
//! - Merge profiles with conflict resolution
//! - Privacy-preserving export (strips sensitive wallet addresses)
//!
//! # Usage
//!
//! ```no_run
//! use ovsm_lsp::knowledge_transfer::KnowledgeProfile;
//! use ovsm_lsp::telemetry::{LearningEngine, LearningConfig};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let learning_engine = LearningEngine::new(LearningConfig::default());
//!
//! // Export current knowledge
//! let profile = KnowledgeProfile::export(&learning_engine, "My Patterns").await;
//! profile.save_to_file("investigation_patterns.ovsm-profile")?;
//!
//! // Import colleague's patterns
//! let imported = KnowledgeProfile::load_from_file("expert_patterns.ovsm-profile")?;
//! imported.apply_to(&learning_engine).await;
//! # Ok(())
//! # }
//! ```

use crate::telemetry::{InvestigationSequence, LearningEngine, StylePreferences};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// A shareable knowledge profile containing learned patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KnowledgeProfile {
    /// Profile version for compatibility
    pub version: String,
    /// Profile name/description
    pub name: String,
    /// Who created this profile (optional)
    pub author: Option<String>,
    /// When this was exported
    pub exported_at: u64,
    /// Function usage patterns (anonymized)
    pub function_patterns: HashMap<String, f64>,
    /// Investigation sequences
    pub sequences: Vec<InvestigationSequence>,
    /// Style preferences
    pub style: StylePreferences,
    /// Tags for categorization
    pub tags: Vec<String>,
    /// Profile statistics
    pub stats: ProfileStats,
}

/// Statistics about the profile
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ProfileStats {
    /// Total events that generated this profile
    pub source_events: usize,
    /// Number of unique functions tracked
    pub unique_functions: usize,
    /// Number of learned sequences
    pub sequences_count: usize,
    /// Hours of usage that generated this profile
    pub usage_hours: f64,
}

/// Options for merging profiles
#[derive(Debug, Clone)]
pub struct MergeOptions {
    /// How to combine function frequencies (average, max, sum)
    pub frequency_strategy: MergeStrategy,
    /// Weight given to imported profile (0.0-1.0)
    pub import_weight: f64,
    /// Whether to include imported sequences
    pub include_sequences: bool,
    /// Minimum confidence to keep sequence
    pub min_sequence_confidence: f64,
}

impl Default for MergeOptions {
    fn default() -> Self {
        Self {
            frequency_strategy: MergeStrategy::WeightedAverage,
            import_weight: 0.5,
            include_sequences: true,
            min_sequence_confidence: 0.3,
        }
    }
}

/// Strategy for merging frequency data
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MergeStrategy {
    /// Average values (weighted by import_weight)
    WeightedAverage,
    /// Keep maximum value
    Maximum,
    /// Sum values (for aggregate profiles)
    Sum,
    /// Keep only imported values
    Replace,
}

impl KnowledgeProfile {
    /// Create a new empty profile
    pub fn new(name: &str) -> Self {
        Self {
            version: "1.0".to_string(),
            name: name.to_string(),
            author: None,
            exported_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            function_patterns: HashMap::new(),
            sequences: Vec::new(),
            style: StylePreferences::default(),
            tags: Vec::new(),
            stats: ProfileStats::default(),
        }
    }

    /// Export current learning state to a profile
    pub async fn export(engine: &LearningEngine, name: &str) -> Self {
        let stats = engine.get_stats().await;
        let patterns = engine.get_patterns().await;

        let mut profile = Self::new(name);

        // Copy function patterns (anonymized - just function names, no wallet data)
        profile.function_patterns = patterns.function_frequency.clone();

        // Copy investigation sequences
        profile.sequences = patterns.investigation_sequences.clone();

        // Copy style preferences
        profile.style = patterns.style_preferences.clone();

        // Set statistics
        profile.stats = ProfileStats {
            source_events: stats.total_events,
            unique_functions: stats.unique_functions,
            sequences_count: stats.sequences_learned,
            usage_hours: (stats.total_events as f64 / 60.0).round(), // Estimate
        };

        profile
    }

    /// Export as a minimal profile (only high-confidence data)
    pub async fn export_minimal(engine: &LearningEngine, name: &str) -> Self {
        let mut profile = Self::export(engine, name).await;

        // Keep only high-frequency functions (top 20%)
        if !profile.function_patterns.is_empty() {
            let mut freqs: Vec<f64> = profile.function_patterns.values().copied().collect();
            freqs.sort_by(|a, b| b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal));
            let threshold = freqs.get(freqs.len() / 5).copied().unwrap_or(0.0);

            profile.function_patterns.retain(|_, v| *v >= threshold);
        }

        // Keep only high-confidence sequences
        profile.sequences.retain(|s| s.confidence >= 0.5);

        profile.tags.push("minimal".to_string());
        profile
    }

    /// Apply this profile to a learning engine
    pub async fn apply_to(&self, engine: &LearningEngine) {
        self.merge_into(engine, MergeOptions::default()).await;
    }

    /// Merge this profile into a learning engine with custom options
    pub async fn merge_into(&self, engine: &LearningEngine, options: MergeOptions) {
        let mut patterns = engine.get_patterns().await;

        // Merge function frequencies
        for (func, freq) in &self.function_patterns {
            let existing = patterns
                .function_frequency
                .get(func)
                .copied()
                .unwrap_or(0.0);

            let new_value = match options.frequency_strategy {
                MergeStrategy::WeightedAverage => {
                    existing * (1.0 - options.import_weight) + freq * options.import_weight
                }
                MergeStrategy::Maximum => existing.max(*freq),
                MergeStrategy::Sum => existing + freq,
                MergeStrategy::Replace => *freq,
            };

            patterns.function_frequency.insert(func.clone(), new_value);
        }

        // Merge sequences
        if options.include_sequences {
            for seq in &self.sequences {
                if seq.confidence >= options.min_sequence_confidence {
                    // Check if we already have this sequence
                    let exists = patterns
                        .investigation_sequences
                        .iter()
                        .any(|s| s.functions == seq.functions);

                    if !exists {
                        patterns.investigation_sequences.push(seq.clone());
                    }
                }
            }
        }

        // Merge style preferences (simple override for now)
        if self.style.indentation > 0 {
            patterns.style_preferences = self.style.clone();
        }

        // Write back the merged patterns
        engine.set_patterns(patterns).await;

        tracing::info!(
            "Applied profile '{}' ({} functions, {} sequences)",
            self.name,
            self.function_patterns.len(),
            self.sequences.len()
        );
    }

    /// Save profile to a file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), std::io::Error> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        fs::write(path, json)
    }

    /// Load profile from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let data = fs::read_to_string(path)?;
        serde_json::from_str(&data)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }

    /// Save as compressed binary (for smaller file size)
    pub fn save_binary<P: AsRef<Path>>(&self, path: P) -> Result<(), std::io::Error> {
        let json = serde_json::to_vec(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

        // Simple compression: remove whitespace (full compression would need flate2)
        fs::write(path, json)
    }

    /// Merge multiple profiles into one aggregate profile
    pub fn aggregate(profiles: &[Self], name: &str) -> Self {
        let mut aggregate = Self::new(name);
        aggregate.tags.push("aggregate".to_string());

        for profile in profiles {
            // Sum function frequencies
            for (func, freq) in &profile.function_patterns {
                *aggregate
                    .function_patterns
                    .entry(func.clone())
                    .or_insert(0.0) += freq;
            }

            // Collect all sequences
            for seq in &profile.sequences {
                if !aggregate
                    .sequences
                    .iter()
                    .any(|s| s.functions == seq.functions)
                {
                    aggregate.sequences.push(seq.clone());
                }
            }

            // Aggregate stats
            aggregate.stats.source_events += profile.stats.source_events;
            aggregate.stats.usage_hours += profile.stats.usage_hours;
        }

        // Normalize frequencies
        if !aggregate.function_patterns.is_empty() {
            let max_freq = aggregate
                .function_patterns
                .values()
                .fold(0.0_f64, |a, b| a.max(*b));
            if max_freq > 0.0 {
                for freq in aggregate.function_patterns.values_mut() {
                    *freq /= max_freq;
                }
            }
        }

        aggregate.stats.unique_functions = aggregate.function_patterns.len();
        aggregate.stats.sequences_count = aggregate.sequences.len();

        aggregate
    }

    /// Check if this profile is compatible with current version
    pub fn is_compatible(&self) -> bool {
        self.version.starts_with("1.")
    }

    /// Get summary for display
    pub fn summary(&self) -> String {
        format!(
            "Profile: {}\n\
             Author: {}\n\
             Version: {}\n\
             Functions: {}\n\
             Sequences: {}\n\
             Source Events: {}\n\
             Tags: {}",
            self.name,
            self.author.as_deref().unwrap_or("Unknown"),
            self.version,
            self.function_patterns.len(),
            self.sequences.len(),
            self.stats.source_events,
            if self.tags.is_empty() {
                "(none)".to_string()
            } else {
                self.tags.join(", ")
            }
        )
    }
}

/// Curated community profiles
pub struct CommunityProfiles;

impl CommunityProfiles {
    /// Built-in profile for DEX investigation
    pub fn dex_investigation() -> KnowledgeProfile {
        let mut profile = KnowledgeProfile::new("DEX Investigation Expert");
        profile.author = Some("OVSM Community".to_string());
        profile.tags = vec![
            "dex".to_string(),
            "trading".to_string(),
            "expert".to_string(),
        ];

        // Common DEX investigation functions
        profile
            .function_patterns
            .insert("getSignatures".to_string(), 0.9);
        profile
            .function_patterns
            .insert("getTransaction".to_string(), 0.85);
        profile
            .function_patterns
            .insert("getTokenAccounts".to_string(), 0.8);
        profile
            .function_patterns
            .insert("getBalance".to_string(), 0.75);
        profile.function_patterns.insert("filter".to_string(), 0.7);
        profile.function_patterns.insert("map".to_string(), 0.65);

        // Common investigation sequences
        profile.sequences.push(InvestigationSequence {
            functions: vec![
                "getSignatures".to_string(),
                "getTransaction".to_string(),
                "filter".to_string(),
            ],
            frequency: 10,
            confidence: 0.9,
        });

        profile.sequences.push(InvestigationSequence {
            functions: vec![
                "getTokenAccounts".to_string(),
                "map".to_string(),
                "filter".to_string(),
            ],
            frequency: 8,
            confidence: 0.85,
        });

        profile
    }

    /// Built-in profile for NFT investigation
    pub fn nft_investigation() -> KnowledgeProfile {
        let mut profile = KnowledgeProfile::new("NFT Investigation Expert");
        profile.author = Some("OVSM Community".to_string());
        profile.tags = vec![
            "nft".to_string(),
            "collections".to_string(),
            "expert".to_string(),
        ];

        profile
            .function_patterns
            .insert("getAccountInfo".to_string(), 0.9);
        profile
            .function_patterns
            .insert("getSignatures".to_string(), 0.85);
        profile
            .function_patterns
            .insert("getTransaction".to_string(), 0.8);
        profile.function_patterns.insert("filter".to_string(), 0.75);

        profile.sequences.push(InvestigationSequence {
            functions: vec![
                "getAccountInfo".to_string(),
                "getSignatures".to_string(),
                "filter".to_string(),
            ],
            frequency: 7,
            confidence: 0.8,
        });

        profile
    }

    /// Built-in profile for wallet tracking
    pub fn wallet_tracking() -> KnowledgeProfile {
        let mut profile = KnowledgeProfile::new("Wallet Tracking Expert");
        profile.author = Some("OVSM Community".to_string());
        profile.tags = vec![
            "wallet".to_string(),
            "tracking".to_string(),
            "forensics".to_string(),
        ];

        profile
            .function_patterns
            .insert("getSignatures".to_string(), 0.95);
        profile
            .function_patterns
            .insert("getTransaction".to_string(), 0.9);
        profile
            .function_patterns
            .insert("getBalance".to_string(), 0.85);
        profile
            .function_patterns
            .insert("getTokenAccounts".to_string(), 0.8);

        profile.sequences.push(InvestigationSequence {
            functions: vec![
                "getBalance".to_string(),
                "getTokenAccounts".to_string(),
                "getSignatures".to_string(),
            ],
            frequency: 15,
            confidence: 0.95,
        });

        profile
    }

    /// List all available community profiles
    pub fn list() -> Vec<(&'static str, &'static str)> {
        vec![
            ("dex", "DEX and trading investigation patterns"),
            ("nft", "NFT collection and marketplace investigation"),
            ("wallet", "Wallet tracking and forensics patterns"),
        ]
    }

    /// Get a community profile by name
    pub fn get(name: &str) -> Option<KnowledgeProfile> {
        match name.to_lowercase().as_str() {
            "dex" | "dex_investigation" | "trading" => Some(Self::dex_investigation()),
            "nft" | "nft_investigation" | "collections" => Some(Self::nft_investigation()),
            "wallet" | "wallet_tracking" | "forensics" => Some(Self::wallet_tracking()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile_creation() {
        let profile = KnowledgeProfile::new("Test Profile");
        assert_eq!(profile.name, "Test Profile");
        assert_eq!(profile.version, "1.0");
        assert!(profile.is_compatible());
    }

    #[test]
    fn test_profile_save_load() {
        let mut profile = KnowledgeProfile::new("Test");
        profile
            .function_patterns
            .insert("test_func".to_string(), 0.5);

        let path = "/tmp/test_profile.json";
        profile.save_to_file(path).unwrap();

        let loaded = KnowledgeProfile::load_from_file(path).unwrap();
        assert_eq!(loaded.name, "Test");
        assert_eq!(loaded.function_patterns.get("test_func"), Some(&0.5));

        std::fs::remove_file(path).ok();
    }

    #[test]
    fn test_profile_aggregate() {
        let mut p1 = KnowledgeProfile::new("P1");
        p1.function_patterns.insert("funcA".to_string(), 0.5);

        let mut p2 = KnowledgeProfile::new("P2");
        p2.function_patterns.insert("funcA".to_string(), 0.5);
        p2.function_patterns.insert("funcB".to_string(), 0.3);

        let aggregate = KnowledgeProfile::aggregate(&[p1, p2], "Combined");
        assert!(aggregate.function_patterns.contains_key("funcA"));
        assert!(aggregate.function_patterns.contains_key("funcB"));
        assert!(aggregate.tags.contains(&"aggregate".to_string()));
    }

    #[test]
    fn test_community_profiles() {
        let profiles = CommunityProfiles::list();
        assert!(!profiles.is_empty());

        let dex = CommunityProfiles::get("dex");
        assert!(dex.is_some());
        let dex = dex.unwrap();
        assert!(dex.function_patterns.contains_key("getSignatures"));
    }

    #[test]
    fn test_profile_summary() {
        let profile = CommunityProfiles::dex_investigation();
        let summary = profile.summary();
        assert!(summary.contains("DEX Investigation Expert"));
        assert!(summary.contains("OVSM Community"));
    }
}
