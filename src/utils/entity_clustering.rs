//! Entity clustering for identifying wallets controlled by the same entity

use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

/// Entity cluster representing wallets controlled by the same actor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityCluster {
    pub cluster_id: usize,
    pub wallet_addresses: Vec<String>,
    pub confidence: f64,           // 0.0-1.0
    pub signals: Vec<ClusterSignal>,
    pub risk_amplification: f64,   // How much this cluster increases risk
}

/// Signal indicating wallets belong to same entity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClusterSignal {
    CommonFundingSource {
        source_wallet: String,
        funded_wallets: Vec<String>,
        confidence: f64,
    },
    TimingCorrelation {
        wallets: Vec<String>,
        correlation_score: f64,  // 0.0-1.0
        time_window_secs: u64,
    },
    SharedPrograms {
        wallets: Vec<String>,
        programs: Vec<String>,
        confidence: f64,
    },
    GasPricePattern {
        wallets: Vec<String>,
        gas_price_variance: f64,  // Low variance = same bot config
        confidence: f64,
    },
    IdenticalBehavior {
        wallets: Vec<String>,
        behavior_type: String,
        similarity_score: f64,
    },
}

impl ClusterSignal {
    pub fn confidence(&self) -> f64 {
        match self {
            ClusterSignal::CommonFundingSource { confidence, .. } => *confidence,
            ClusterSignal::TimingCorrelation { correlation_score, .. } => *correlation_score,
            ClusterSignal::SharedPrograms { confidence, .. } => *confidence,
            ClusterSignal::GasPricePattern { confidence, .. } => *confidence,
            ClusterSignal::IdenticalBehavior { similarity_score, .. } => *similarity_score,
        }
    }

    pub fn wallets(&self) -> Vec<String> {
        match self {
            ClusterSignal::CommonFundingSource { funded_wallets, .. } => funded_wallets.clone(),
            ClusterSignal::TimingCorrelation { wallets, .. } => wallets.clone(),
            ClusterSignal::SharedPrograms { wallets, .. } => wallets.clone(),
            ClusterSignal::GasPricePattern { wallets, .. } => wallets.clone(),
            ClusterSignal::IdenticalBehavior { wallets, .. } => wallets.clone(),
        }
    }
}

/// Entity clustering analyzer
pub struct EntityClusterer {
    min_confidence: f64,
    min_cluster_size: usize,
}

impl Default for EntityClusterer {
    fn default() -> Self {
        Self {
            min_confidence: 0.6,  // 60% confidence threshold
            min_cluster_size: 2,   // At least 2 wallets
        }
    }
}

impl EntityClusterer {
    pub fn new(min_confidence: f64, min_cluster_size: usize) -> Self {
        Self {
            min_confidence,
            min_cluster_size,
        }
    }

    /// Detect clusters from wallet graph data
    pub fn detect_clusters(
        &self,
        wallets: &[(String, WalletMetadata)],
        connections: &[(usize, usize, TransferMetadata)],
    ) -> Vec<EntityCluster> {
        let mut clusters = Vec::new();

        // 1. Common funding source detection
        let funding_clusters = self.detect_common_funding(wallets, connections);
        clusters.extend(funding_clusters);

        // 2. Timing correlation
        let timing_clusters = self.detect_timing_correlation(wallets, connections);
        clusters.extend(timing_clusters);

        // 3. Merge overlapping clusters
        self.merge_clusters(clusters)
    }

    /// Detect wallets funded by the same source
    fn detect_common_funding(
        &self,
        wallets: &[(String, WalletMetadata)],
        connections: &[(usize, usize, TransferMetadata)],
    ) -> Vec<EntityCluster> {
        let mut funding_map: HashMap<String, Vec<String>> = HashMap::new();

        // Build funding graph: source -> [recipients]
        for (from_idx, to_idx, metadata) in connections {
            let from_wallet = &wallets[*from_idx].0;
            let to_wallet = &wallets[*to_idx].0;

            // Check if this is an initial funding transaction
            // (heuristic: first transaction to a wallet, or large amount)
            if metadata.is_initial_funding {
                funding_map
                    .entry(from_wallet.clone())
                    .or_insert_with(Vec::new)
                    .push(to_wallet.clone());
            }
        }

        // Create clusters from funding groups
        let mut clusters = Vec::new();
        for (source, funded) in funding_map {
            if funded.len() >= self.min_cluster_size {
                let confidence = self.calculate_funding_confidence(&funded, connections);

                if confidence >= self.min_confidence {
                    clusters.push(EntityCluster {
                        cluster_id: clusters.len(),
                        wallet_addresses: funded.clone(),
                        confidence,
                        signals: vec![ClusterSignal::CommonFundingSource {
                            source_wallet: source,
                            funded_wallets: funded,
                            confidence,
                        }],
                        risk_amplification: 1.0 + (confidence * 0.5), // Up to 1.5x risk
                    });
                }
            }
        }

        clusters
    }

    /// Detect wallets with correlated transaction timing
    fn detect_timing_correlation(
        &self,
        wallets: &[(String, WalletMetadata)],
        connections: &[(usize, usize, TransferMetadata)],
    ) -> Vec<EntityCluster> {
        let mut clusters = Vec::new();

        // Group transactions by time windows
        let mut time_groups: HashMap<u64, Vec<(usize, u64)>> = HashMap::new();

        for (from_idx, _, metadata) in connections {
            if let Some(timestamp) = metadata.timestamp {
                let time_bucket = timestamp / 60; // 1-minute buckets
                time_groups
                    .entry(time_bucket)
                    .or_insert_with(Vec::new)
                    .push((*from_idx, timestamp));
            }
        }

        // Find wallets that consistently transact together
        let mut wallet_pairs: HashMap<(usize, usize), usize> = HashMap::new();

        for (_bucket, txns) in time_groups {
            if txns.len() >= 2 {
                // Count co-occurrences
                for i in 0..txns.len() {
                    for j in (i + 1)..txns.len() {
                        let wallet1 = txns[i].0;
                        let wallet2 = txns[j].0;
                        let time_diff = txns[i].1.abs_diff(txns[j].1);

                        // Within 10 seconds
                        if time_diff <= 10 {
                            let key = if wallet1 < wallet2 {
                                (wallet1, wallet2)
                            } else {
                                (wallet2, wallet1)
                            };
                            *wallet_pairs.entry(key).or_insert(0) += 1;
                        }
                    }
                }
            }
        }

        // Create clusters from highly correlated pairs
        let mut used_wallets = HashSet::new();
        for ((w1, w2), count) in wallet_pairs {
            if count >= 3 && !used_wallets.contains(&w1) && !used_wallets.contains(&w2) {
                let correlation_score = (count as f64 / 10.0).min(1.0);

                if correlation_score >= self.min_confidence {
                    used_wallets.insert(w1);
                    used_wallets.insert(w2);

                    clusters.push(EntityCluster {
                        cluster_id: clusters.len(),
                        wallet_addresses: vec![
                            wallets[w1].0.clone(),
                            wallets[w2].0.clone(),
                        ],
                        confidence: correlation_score,
                        signals: vec![ClusterSignal::TimingCorrelation {
                            wallets: vec![wallets[w1].0.clone(), wallets[w2].0.clone()],
                            correlation_score,
                            time_window_secs: 10,
                        }],
                        risk_amplification: 1.0 + (correlation_score * 0.3),
                    });
                }
            }
        }

        clusters
    }

    /// Calculate confidence for funding-based clusters
    fn calculate_funding_confidence(
        &self,
        funded: &[String],
        connections: &[(usize, usize, TransferMetadata)],
    ) -> f64 {
        let funded_count = funded.len();

        // Base confidence on cluster size
        let size_confidence = (funded_count as f64 / 10.0).min(1.0);

        // Check if wallets have similar patterns after funding
        // (simplified: would check transaction patterns, amounts, etc.)
        let pattern_confidence = 0.7; // Placeholder

        // Combined confidence
        (size_confidence * 0.6 + pattern_confidence * 0.4).min(1.0)
    }

    /// Merge overlapping clusters
    fn merge_clusters(&self, mut clusters: Vec<EntityCluster>) -> Vec<EntityCluster> {
        if clusters.is_empty() {
            return clusters;
        }

        let mut merged = Vec::new();
        let mut used = HashSet::new();

        for i in 0..clusters.len() {
            if used.contains(&i) {
                continue;
            }

            let mut current = clusters[i].clone();
            let mut current_wallets: HashSet<String> =
                current.wallet_addresses.iter().cloned().collect();

            // Find overlapping clusters
            for j in (i + 1)..clusters.len() {
                if used.contains(&j) {
                    continue;
                }

                let other_wallets: HashSet<String> =
                    clusters[j].wallet_addresses.iter().cloned().collect();

                let overlap = current_wallets.intersection(&other_wallets).count();

                // If >50% overlap, merge
                if overlap > 0
                    && overlap as f64 / current_wallets.len().min(other_wallets.len()) as f64 > 0.5
                {
                    current_wallets.extend(other_wallets);
                    current.signals.extend(clusters[j].signals.clone());
                    current.confidence =
                        (current.confidence + clusters[j].confidence) / 2.0;
                    current.risk_amplification = current.risk_amplification.max(clusters[j].risk_amplification);
                    used.insert(j);
                }
            }

            current.wallet_addresses = current_wallets.into_iter().collect();
            current.wallet_addresses.sort();
            merged.push(current);
            used.insert(i);
        }

        // Re-assign cluster IDs
        for (i, cluster) in merged.iter_mut().enumerate() {
            cluster.cluster_id = i;
        }

        merged
    }

    /// Get cluster color for visualization (consistent hashing)
    pub fn get_cluster_color(cluster_id: usize) -> (u8, u8, u8) {
        // Predefined color palette for clusters
        let colors = [
            (255, 100, 100), // Red
            (100, 255, 100), // Green
            (100, 100, 255), // Blue
            (255, 255, 100), // Yellow
            (255, 100, 255), // Magenta
            (100, 255, 255), // Cyan
            (255, 150, 100), // Orange
            (150, 100, 255), // Purple
            (100, 255, 150), // Mint
            (255, 200, 100), // Gold
        ];

        colors[cluster_id % colors.len()]
    }
}

/// Wallet metadata for clustering analysis
#[derive(Debug, Clone)]
pub struct WalletMetadata {
    pub address: String,
    pub first_seen: Option<u64>,
    pub transaction_count: usize,
    pub behavior_type: Option<String>,
}

/// Transfer metadata for clustering analysis
#[derive(Debug, Clone)]
pub struct TransferMetadata {
    pub amount: f64,
    pub token: String,
    pub timestamp: Option<u64>,
    pub is_initial_funding: bool,  // Heuristic: first tx or large amount
    pub gas_price: Option<f64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_common_funding_detection() {
        let wallets = vec![
            ("source".to_string(), WalletMetadata {
                address: "source".to_string(),
                first_seen: Some(1000),
                transaction_count: 10,
                behavior_type: None,
            }),
            ("wallet1".to_string(), WalletMetadata {
                address: "wallet1".to_string(),
                first_seen: Some(1100),
                transaction_count: 5,
                behavior_type: None,
            }),
            ("wallet2".to_string(), WalletMetadata {
                address: "wallet2".to_string(),
                first_seen: Some(1200),
                transaction_count: 5,
                behavior_type: None,
            }),
        ];

        let connections = vec![
            (0, 1, TransferMetadata {
                amount: 10.0,
                token: "SOL".to_string(),
                timestamp: Some(1100),
                is_initial_funding: true,
                gas_price: None,
            }),
            (0, 2, TransferMetadata {
                amount: 10.0,
                token: "SOL".to_string(),
                timestamp: Some(1200),
                is_initial_funding: true,
                gas_price: None,
            }),
        ];

        // Use lower min_confidence threshold since test has only 2 wallets
        // (default 0.6 threshold, but 2 wallets yields ~0.4 confidence)
        let clusterer = EntityClusterer::new(0.3, 2);
        let clusters = clusterer.detect_clusters(&wallets, &connections);

        assert!(!clusters.is_empty(), "Should detect common funding cluster");
    }
}
