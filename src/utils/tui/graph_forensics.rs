//! Graph forensics module - Blockchain transaction pattern detection algorithms
//!
//! This module contains all forensic analysis algorithms for detecting suspicious patterns
//! in wallet transaction graphs. Extracted from graph.rs for better testability and reuse.
//!
//! # Features
//! - Circular flow detection (wash trading patterns)
//! - Rapid transfer burst analysis
//! - Mixer/tumbling node detection
//! - Wallet behavior classification
//! - Explainable risk scoring
//!
//! # Usage
//! ```ignore
//! use crate::utils::tui::graph_forensics::{GraphForensics, ForensicsAnalysis};
//!
//! let forensics = GraphForensics::new(&config);
//! let analysis = forensics.analyze(&graph_data);
//! println!("Risk: {:?}", analysis.risk.level);
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use crate::utils::forensics_config::ForensicsConfig;

// ============================================================================
// TYPES - Core forensics data structures
// ============================================================================

/// Wallet behavior classification based on transaction patterns
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WalletBehaviorType {
    /// Regular intervals, high frequency, programmatic patterns
    Bot,
    /// Very high volume, many counterparties, round amounts
    Exchange,
    /// DEX interactions, moderate volume, varied timing
    Trader,
    /// Many small inputs/outputs, obfuscation patterns
    Mixer,
    /// Externally Owned Account - human-like patterns
    EOA,
    /// Program account with deposits/withdrawals
    Contract,
    /// Very low activity
    Dormant,
}

impl WalletBehaviorType {
    /// Get an emoji icon for this behavior type
    pub fn icon(&self) -> &'static str {
        match self {
            WalletBehaviorType::Bot => "🤖",
            WalletBehaviorType::Exchange => "🏦",
            WalletBehaviorType::Trader => "📈",
            WalletBehaviorType::Mixer => "🌀",
            WalletBehaviorType::EOA => "👤",
            WalletBehaviorType::Contract => "📜",
            WalletBehaviorType::Dormant => "💤",
        }
    }
}

/// Alert severity levels for forensic findings
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AlertSeverity {
    Low,
    Medium,
    High,
    Critical,
}

impl AlertSeverity {
    pub fn icon(&self) -> &'static str {
        match self {
            AlertSeverity::Critical => "🔴",
            AlertSeverity::High => "🟠",
            AlertSeverity::Medium => "🟡",
            AlertSeverity::Low => "🟢",
        }
    }
}

/// Risk level classification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RiskLevel {
    Critical, // 75-100
    High,     // 50-75
    Medium,   // 25-50
    Low,      // 0-25
}

impl RiskLevel {
    pub fn icon(&self) -> &'static str {
        match self {
            RiskLevel::Critical => "🔴",
            RiskLevel::High => "🟠",
            RiskLevel::Medium => "🟡",
            RiskLevel::Low => "🟢",
        }
    }

    pub fn from_score(score: f64) -> Self {
        if score >= 75.0 {
            RiskLevel::Critical
        } else if score >= 50.0 {
            RiskLevel::High
        } else if score >= 25.0 {
            RiskLevel::Medium
        } else {
            RiskLevel::Low
        }
    }
}

/// Rapid transfer detection alert
#[derive(Debug, Clone)]
pub struct RapidFlowAlert {
    pub transfer_count: usize,
    pub time_window_secs: u64,
    pub total_volume: f64,
    pub token: String,
    pub severity: AlertSeverity,
}

/// Circular flow pattern detection result
#[derive(Debug, Clone)]
pub struct CircularFlow {
    pub path: Vec<String>,      // Wallet addresses in the cycle
    pub total_amount: f64,
    pub token: String,
    pub cycle_length: usize,
}

/// Statistics for a mixer/tumbling node
#[derive(Debug, Clone)]
pub struct MixerStats {
    pub sources: usize,       // Number of incoming wallets
    pub destinations: usize,  // Number of outgoing wallets
    pub total_in: f64,        // Total amount received
    pub total_out: f64,       // Total amount sent
    pub unique_tokens: usize, // Number of different tokens
}

/// Explainable risk assessment with detailed reasoning
#[derive(Debug, Clone)]
pub struct RiskExplanation {
    pub score: f64,              // 0-100
    pub level: RiskLevel,
    pub reasons: Vec<String>,    // Human-readable explanations
    pub alerts: Vec<String>,     // Critical findings
}

impl Default for RiskExplanation {
    fn default() -> Self {
        Self {
            score: 0.0,
            level: RiskLevel::Low,
            reasons: Vec::new(),
            alerts: Vec::new(),
        }
    }
}

// ============================================================================
// GRAPH DATA TRAIT - Interface for graph implementations
// ============================================================================

/// Trait for accessing graph data needed by forensics algorithms
/// This decouples forensics from the specific WalletGraph implementation
pub trait GraphData {
    /// Number of nodes in the graph
    fn node_count(&self) -> usize;

    /// Number of edges in the graph
    fn edge_count(&self) -> usize;

    /// Get wallet address at node index
    fn node_address(&self, idx: usize) -> Option<&str>;

    /// Get node type description (e.g., "Target", "DeFi", "Funding")
    fn node_type(&self, idx: usize) -> Option<&str>;

    /// Iterate over edges: (from_idx, to_idx, amount, token, timestamp)
    fn edges(&self) -> Vec<(usize, usize, f64, String, Option<String>)>;

    /// Get incoming edge indices for a node
    fn incoming_edges(&self, node_idx: usize) -> Vec<usize>;

    /// Get outgoing edge indices for a node
    fn outgoing_edges(&self, node_idx: usize) -> Vec<usize>;

    /// Get edge data by index: (from_idx, to_idx, amount, token, timestamp)
    fn edge_data(&self, edge_idx: usize) -> Option<(usize, usize, f64, String, Option<String>)>;
}

// ============================================================================
// FORENSICS ENGINE - Main analysis interface
// ============================================================================

/// Graph forensics analysis engine
pub struct GraphForensics {
    config: ForensicsConfig,
}

impl GraphForensics {
    /// Create new forensics engine with configuration
    pub fn new(config: ForensicsConfig) -> Self {
        Self { config }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self {
            config: ForensicsConfig::default(),
        }
    }

    /// Get reference to configuration
    pub fn config(&self) -> &ForensicsConfig {
        &self.config
    }

    // ========================================================================
    // CIRCULAR FLOW DETECTION
    // ========================================================================

    /// Detect circular flow patterns (A→B→C→A) in the graph
    /// Uses Johnson's algorithm-inspired approach with depth limiting for performance
    pub fn detect_circular_flows<G: GraphData>(&self, graph: &G) -> Vec<CircularFlow> {
        let mut cycles = Vec::new();
        let max_depth = 5; // Limit cycle search depth

        // Build adjacency list: node -> [(target, amount, token)]
        let edges = graph.edges();
        let mut adj: HashMap<usize, Vec<(usize, f64, String)>> = HashMap::new();

        for (from, to, amount, token, _ts) in edges {
            adj.entry(from)
                .or_insert_with(Vec::new)
                .push((to, amount, token));
        }

        // DFS from each node to find cycles
        let mut visited = HashSet::new();
        let mut path: Vec<(usize, String, f64)> = Vec::new();

        for start_idx in 0..graph.node_count() {
            self.dfs_find_cycles(
                graph,
                start_idx,
                start_idx,
                &adj,
                &mut visited,
                &mut path,
                &mut cycles,
                0,
                max_depth,
            );
        }

        // Deduplicate cycles (same cycle can be found from different start points)
        self.deduplicate_cycles(&mut cycles);

        cycles
    }

    fn dfs_find_cycles<G: GraphData>(
        &self,
        graph: &G,
        start: usize,
        current: usize,
        adj: &HashMap<usize, Vec<(usize, f64, String)>>,
        visited: &mut HashSet<usize>,
        path: &mut Vec<(usize, String, f64)>,
        cycles: &mut Vec<CircularFlow>,
        depth: usize,
        max_depth: usize,
    ) {
        if depth > max_depth {
            return;
        }

        if visited.contains(&current) {
            // Check if we've returned to start (completing a cycle)
            if current == start && path.len() >= 3 {
                let mut cycle_path = Vec::new();
                let mut total_amount = 0.0;
                let token = path.first().map(|(_, t, _)| t.clone()).unwrap_or_default();

                for (idx, _token, amount) in path.iter() {
                    if let Some(addr) = graph.node_address(*idx) {
                        cycle_path.push(addr.to_string());
                    }
                    total_amount += *amount;
                }

                cycles.push(CircularFlow {
                    path: cycle_path,
                    total_amount,
                    token,
                    cycle_length: path.len(),
                });
            }
            return;
        }

        visited.insert(current);

        if let Some(neighbors) = adj.get(&current) {
            for (next, amount, token) in neighbors {
                path.push((current, token.clone(), *amount));
                self.dfs_find_cycles(
                    graph, start, *next, adj, visited, path, cycles, depth + 1, max_depth,
                );
                path.pop();
            }
        }

        visited.remove(&current);
    }

    fn deduplicate_cycles(&self, cycles: &mut Vec<CircularFlow>) {
        // Sort paths for comparison and deduplicate
        let mut seen: HashSet<String> = HashSet::new();
        cycles.retain(|cycle| {
            let mut sorted_path = cycle.path.clone();
            sorted_path.sort();
            let key = sorted_path.join(",");
            if seen.contains(&key) {
                false
            } else {
                seen.insert(key);
                true
            }
        });
    }

    // ========================================================================
    // RAPID TRANSFER DETECTION
    // ========================================================================

    /// Detect rapid transfer patterns (velocity analysis)
    pub fn detect_rapid_transfers<G: GraphData>(&self, graph: &G) -> Vec<RapidFlowAlert> {
        let mut alerts = Vec::new();
        let edges = graph.edges();

        // Parse timestamps and group by time windows
        let mut time_buckets: HashMap<String, Vec<(u64, f64, String)>> = HashMap::new();

        for (_from, _to, amount, token, timestamp) in edges {
            if let Some(ts_str) = timestamp {
                // Try to parse timestamp (Unix timestamp or ISO 8601)
                let unix_time = self.parse_timestamp(&ts_str);
                if let Some(unix) = unix_time {
                    // Hour buckets by token
                    let bucket_key = format!("{}_{}", token, unix / 3600);
                    time_buckets
                        .entry(bucket_key)
                        .or_insert_with(Vec::new)
                        .push((unix, amount, token));
                }
            }
        }

        // Analyze each bucket for rapid activity
        for (_key, transfers) in time_buckets {
            if transfers.len() < 2 {
                continue;
            }

            let mut sorted_transfers = transfers;
            sorted_transfers.sort_by_key(|(time, _, _)| *time);

            // Sliding window analysis with configurable time windows
            for window_size in &self.config.thresholds.time_windows {
                let mut i = 0;
                while i < sorted_transfers.len() {
                    let start_time = sorted_transfers[i].0;
                    let mut j = i;
                    let mut count = 0;
                    let mut total_volume = 0.0;

                    // Find all transfers within the window
                    while j < sorted_transfers.len()
                        && sorted_transfers[j].0 - start_time <= *window_size
                    {
                        count += 1;
                        total_volume += sorted_transfers[j].1;
                        j += 1;
                    }

                    // Determine severity using configured thresholds
                    let txns_per_min = count as f64 / (*window_size as f64 / 60.0);
                    let vol_per_min = total_volume / (*window_size as f64 / 60.0);

                    let severity = if txns_per_min > self.config.thresholds.rapid_txns_critical
                        || vol_per_min > self.config.thresholds.rapid_volume_critical
                    {
                        AlertSeverity::Critical
                    } else if txns_per_min > self.config.thresholds.rapid_txns_critical / 2.0
                        || vol_per_min > self.config.thresholds.rapid_volume_critical / 2.0
                    {
                        AlertSeverity::High
                    } else if txns_per_min > self.config.thresholds.rapid_txns_critical / 4.0
                        || vol_per_min > self.config.thresholds.rapid_volume_critical / 4.0
                    {
                        AlertSeverity::Medium
                    } else if count >= 5 {
                        AlertSeverity::Low
                    } else {
                        i += 1;
                        continue;
                    };

                    if count >= 5 {
                        alerts.push(RapidFlowAlert {
                            transfer_count: count,
                            time_window_secs: *window_size,
                            total_volume,
                            token: sorted_transfers[i].2.clone(),
                            severity,
                        });
                    }

                    i += 1;
                }
            }
        }

        alerts
    }

    /// Parse timestamp string to Unix timestamp
    fn parse_timestamp(&self, ts_str: &str) -> Option<u64> {
        // Try Unix timestamp first
        if let Ok(unix) = ts_str.parse::<u64>() {
            return Some(unix);
        }

        // Try ISO 8601 format (e.g., "2024-01-15T12:00:00Z")
        // Note: For a production implementation, use chrono crate
        // This is a simplified parser for common formats
        if ts_str.contains('T') && ts_str.len() >= 19 {
            // Very basic ISO 8601 parsing - replace with chrono for production
            // Format: YYYY-MM-DDTHH:MM:SS
            let parts: Vec<&str> = ts_str.split('T').collect();
            if parts.len() >= 2 {
                let date_parts: Vec<&str> = parts[0].split('-').collect();
                let time_parts: Vec<&str> = parts[1].split(':').collect();

                if date_parts.len() >= 3 && time_parts.len() >= 3 {
                    let year: i64 = date_parts[0].parse().ok()?;
                    let month: i64 = date_parts[1].parse().ok()?;
                    let day: i64 = date_parts[2].parse().ok()?;
                    let hour: i64 = time_parts[0].parse().ok()?;
                    let minute: i64 = time_parts[1].parse().ok()?;
                    let second: i64 = time_parts[2]
                        .chars()
                        .take_while(|c| c.is_ascii_digit())
                        .collect::<String>()
                        .parse()
                        .ok()?;

                    // Simplified calculation (not accounting for leap years properly)
                    let days_since_epoch = (year - 1970) * 365
                        + (year - 1969) / 4  // Leap years
                        + (month - 1) * 30   // Approximate
                        + day - 1;
                    let unix = days_since_epoch * 86400 + hour * 3600 + minute * 60 + second;

                    return Some(unix as u64);
                }
            }
        }

        None
    }

    // ========================================================================
    // MIXER DETECTION
    // ========================================================================

    /// Detect mixing/laundering patterns and return list of mixer node indices
    /// Pattern: high in-degree + high out-degree + rapid turnover
    pub fn detect_mixer_nodes<G: GraphData>(&self, graph: &G) -> Vec<usize> {
        let mut mixers = Vec::new();

        for idx in 0..graph.node_count() {
            let in_count = graph.incoming_edges(idx).len();
            let out_count = graph.outgoing_edges(idx).len();

            // Heuristic: Mixer if receives from 3+ sources AND sends to 3+ destinations
            // This catches split→shuffle→consolidate patterns
            if in_count >= 3 && out_count >= 3 {
                mixers.push(idx);
            }
        }

        mixers
    }

    /// Calculate mixer statistics for a specific node
    pub fn get_mixer_stats<G: GraphData>(&self, graph: &G, node_idx: usize) -> MixerStats {
        let incoming = graph.incoming_edges(node_idx);
        let outgoing = graph.outgoing_edges(node_idx);

        let mut total_in = 0.0;
        let mut total_out = 0.0;
        let mut unique_tokens: HashSet<String> = HashSet::new();

        for edge_idx in &incoming {
            if let Some((_, _, amount, token, _)) = graph.edge_data(*edge_idx) {
                total_in += amount;
                unique_tokens.insert(token);
            }
        }

        for edge_idx in &outgoing {
            if let Some((_, _, amount, token, _)) = graph.edge_data(*edge_idx) {
                total_out += amount;
                unique_tokens.insert(token);
            }
        }

        MixerStats {
            sources: incoming.len(),
            destinations: outgoing.len(),
            total_in,
            total_out,
            unique_tokens: unique_tokens.len(),
        }
    }

    // ========================================================================
    // WALLET BEHAVIOR CLASSIFICATION
    // ========================================================================

    /// Classify wallet behavior based on transaction patterns
    pub fn classify_wallet_behavior<G: GraphData>(
        &self,
        graph: &G,
        wallet_idx: usize,
    ) -> WalletBehaviorType {
        let edges = graph.edges();

        // Count incoming and outgoing edges
        let mut incoming = 0;
        let mut outgoing = 0;
        let mut total_in = 0.0;
        let mut total_out = 0.0;
        let mut counterparties: HashMap<usize, usize> = HashMap::new();
        let mut timestamps: Vec<u64> = Vec::new();

        for (from, to, amount, _token, timestamp) in &edges {
            if *from == wallet_idx {
                outgoing += 1;
                total_out += amount;
                *counterparties.entry(*to).or_insert(0) += 1;
                if let Some(ts) = timestamp {
                    if let Some(unix) = self.parse_timestamp(ts) {
                        timestamps.push(unix);
                    }
                }
            }
            if *to == wallet_idx {
                incoming += 1;
                total_in += amount;
                *counterparties.entry(*from).or_insert(0) += 1;
                if let Some(ts) = timestamp {
                    if let Some(unix) = self.parse_timestamp(ts) {
                        timestamps.push(unix);
                    }
                }
            }
        }

        let total_txns = incoming + outgoing;
        let total_volume = total_in + total_out;

        // Dormant check
        if total_txns < self.config.behavior.dormant_max_transactions {
            return WalletBehaviorType::Dormant;
        }

        // Exchange characteristics: very high volume, many unique counterparties
        if total_volume > self.config.behavior.exchange_min_volume
            && counterparties.len() > self.config.behavior.exchange_min_counterparties
        {
            return WalletBehaviorType::Exchange;
        }

        // Mixer characteristics: many small inputs/outputs, high counterparty diversity
        let avg_amount = total_volume / total_txns as f64;
        if avg_amount < self.config.behavior.mixer_max_avg_amount
            && counterparties.len() > self.config.behavior.mixer_min_counterparties
        {
            return WalletBehaviorType::Mixer;
        }

        // Bot characteristics: regular timing intervals
        if timestamps.len() > self.config.behavior.bot_min_transactions {
            timestamps.sort();
            let mut intervals = Vec::new();
            for i in 1..timestamps.len() {
                intervals.push(timestamps[i] - timestamps[i - 1]);
            }
            let avg_interval = intervals.iter().sum::<u64>() as f64 / intervals.len() as f64;
            let variance: f64 = intervals
                .iter()
                .map(|&x| (x as f64 - avg_interval).powi(2))
                .sum::<f64>()
                / intervals.len() as f64;
            let std_dev = variance.sqrt();

            // Low variance = regular intervals = bot
            if std_dev < avg_interval * self.config.behavior.bot_max_variance_ratio
                && total_txns > self.config.behavior.bot_min_transactions
            {
                return WalletBehaviorType::Bot;
            }
        }

        // Trader characteristics: moderate volume, DEX interactions
        if total_volume > 100.0 && total_volume < self.config.behavior.exchange_min_volume {
            return WalletBehaviorType::Trader;
        }

        // Contract check: check node type
        if let Some(node_type) = graph.node_type(wallet_idx) {
            if node_type == "DeFi" || node_type == "Token" {
                return WalletBehaviorType::Contract;
            }
        }

        WalletBehaviorType::EOA
    }

    // ========================================================================
    // RISK SCORING
    // ========================================================================

    /// Calculate explainable risk score with detailed reasoning
    pub fn calculate_explainable_risk<G: GraphData>(&self, graph: &G) -> RiskExplanation {
        let mut score: f64 = 0.0;
        let mut reasons = Vec::new();
        let mut alerts = Vec::new();

        // 1. Network complexity analysis
        let node_count = graph.node_count().max(1);
        let edge_count = graph.edge_count();
        let complexity_ratio = edge_count as f64 / node_count as f64;

        if complexity_ratio > self.config.thresholds.complexity_high {
            score += self.config.risk.complexity_weight;
            alerts.push(format!(
                "🚨 CRITICAL: Very high network complexity ({:.1} edges/node)",
                complexity_ratio
            ));
            reasons.push(format!(
                "Network complexity ratio of {:.1} indicates potential mixing/obfuscation",
                complexity_ratio
            ));
        } else if complexity_ratio > self.config.thresholds.complexity_suspicious {
            score += self.config.risk.complexity_weight / 2.0;
            reasons.push(format!(
                "Elevated network complexity ({:.1} edges/node)",
                complexity_ratio
            ));
        }

        // 2. Rapid transfer detection
        let rapid_flows = self.detect_rapid_transfers(graph);
        for alert in rapid_flows
            .iter()
            .filter(|a| matches!(a.severity, AlertSeverity::Critical | AlertSeverity::High))
        {
            score += self.config.risk.rapid_transfer_weight;
            alerts.push(format!(
                "⚡ RAPID ACTIVITY: {} transfers in {}s ({:.2} {})",
                alert.transfer_count, alert.time_window_secs, alert.total_volume, alert.token
            ));
        }
        if !rapid_flows.is_empty() {
            reasons.push(format!(
                "Detected {} rapid transfer burst(s)",
                rapid_flows.len()
            ));
        }

        // 3. Circular flow detection
        let circular_flows = self.detect_circular_flows(graph);
        for cycle in circular_flows.iter().take(3) {
            score += self.config.risk.circular_flow_weight;
            alerts.push(format!(
                "🔄 CIRCULAR FLOW: {} wallets, {:.2} {} total",
                cycle.cycle_length, cycle.total_amount, cycle.token
            ));
        }
        if !circular_flows.is_empty() {
            reasons.push(format!(
                "Detected {} circular flow pattern(s) - potential wash trading",
                circular_flows.len()
            ));
        }

        // 4. Whale activity
        let whale_flows = self.get_whale_flows(graph);
        if whale_flows.len() > 5 {
            score += self.config.risk.whale_activity_weight;
            let total_whale: f64 = whale_flows.iter().map(|(amount, _)| amount).sum();
            reasons.push(format!(
                "High whale activity: {} large transfers totaling {:.2} SOL",
                whale_flows.len(),
                total_whale
            ));
        }

        // 5. Wallet behavior analysis (target wallet at index 0)
        if graph.node_count() > 0 {
            let target_behavior = self.classify_wallet_behavior(graph, 0);
            match target_behavior {
                WalletBehaviorType::Mixer => {
                    score += self.config.risk.mixer_behavior_weight;
                    alerts.push(
                        "🔴 MIXER DETECTED: Wallet exhibits mixing/tumbling behavior".to_string(),
                    );
                }
                WalletBehaviorType::Bot => {
                    score += self.config.risk.bot_behavior_weight;
                    reasons.push("Programmatic bot activity detected (regular intervals)".to_string());
                }
                WalletBehaviorType::Exchange => {
                    score -= 5.0; // Exchanges are high-volume but legitimate
                    reasons.push(
                        "Exchange-like behavior (high volume, many counterparties)".to_string(),
                    );
                }
                _ => {}
            }
        }

        // 6. Token diversity
        let tokens = self.get_unique_tokens(graph);
        if tokens.len() > 20 {
            score += 10.0;
            reasons.push(format!(
                "Very high token diversity ({} tokens) may indicate portfolio mixing",
                tokens.len()
            ));
        }

        // 7. Hub wallet detection
        let (sources, sinks, hubs) = self.analyze_wallet_patterns(graph);
        if hubs > 3 {
            score += 10.0;
            reasons.push(format!(
                "{} hub wallets detected - potential coordination point",
                hubs
            ));
        }

        // Cap score and determine level
        score = score.min(100.0_f64).max(0.0_f64);
        let level = RiskLevel::from_score(score);

        RiskExplanation {
            score,
            level,
            reasons,
            alerts,
        }
    }

    /// Calculate simple network risk score (0-100)
    pub fn calculate_network_risk_score<G: GraphData>(&self, graph: &G) -> f64 {
        let explanation = self.calculate_explainable_risk(graph);
        explanation.score
    }

    // ========================================================================
    // UTILITY METHODS
    // ========================================================================

    /// Get whale flows (transfers above threshold)
    pub fn get_whale_flows<G: GraphData>(&self, graph: &G) -> Vec<(f64, String)> {
        let edges = graph.edges();
        edges
            .into_iter()
            .filter(|(_, _, amount, _, _)| *amount >= self.config.thresholds.whale_amount_sol)
            .map(|(_, _, amount, token, _)| (amount, token))
            .collect()
    }

    /// Get unique tokens in the graph
    pub fn get_unique_tokens<G: GraphData>(&self, graph: &G) -> Vec<String> {
        let edges = graph.edges();
        let mut tokens: HashSet<String> = HashSet::new();
        for (_, _, _, token, _) in edges {
            tokens.insert(token);
        }
        tokens.into_iter().collect()
    }

    /// Analyze wallet patterns - returns (sources, sinks, hubs)
    pub fn analyze_wallet_patterns<G: GraphData>(&self, graph: &G) -> (usize, usize, usize) {
        let edges = graph.edges();
        let mut inflows: HashMap<usize, f64> = HashMap::new();
        let mut outflows: HashMap<usize, f64> = HashMap::new();

        for (from, to, amount, _, _) in edges {
            *outflows.entry(from).or_insert(0.0) += amount;
            *inflows.entry(to).or_insert(0.0) += amount;
        }

        let mut sources = 0;
        let mut sinks = 0;
        let mut hubs = 0;

        for idx in 0..graph.node_count() {
            let in_amt = inflows.get(&idx).copied().unwrap_or(0.0);
            let out_amt = outflows.get(&idx).copied().unwrap_or(0.0);
            let total = in_amt + out_amt;

            if total > 0.0 {
                let out_ratio = out_amt / total;

                if out_ratio > 0.8 {
                    sources += 1; // Mostly outflows
                } else if out_ratio < 0.2 {
                    sinks += 1; // Mostly inflows
                } else if total > 100.0 {
                    hubs += 1; // High throughput
                }
            }
        }

        (sources, sinks, hubs)
    }

    /// Trace token path depth between two nodes
    pub fn trace_token_path_depth<G: GraphData>(
        &self,
        graph: &G,
        from_idx: usize,
        to_idx: usize,
        token: &str,
    ) -> usize {
        let edges = graph.edges();
        let mut visited = HashSet::new();
        let mut max_depth = 0;

        // Build adjacency list for the specific token
        let mut adj: HashMap<usize, Vec<usize>> = HashMap::new();
        let mut rev_adj: HashMap<usize, Vec<usize>> = HashMap::new();

        for (from, to, _, tok, _) in &edges {
            if tok == token {
                adj.entry(*from).or_insert_with(Vec::new).push(*to);
                rev_adj.entry(*to).or_insert_with(Vec::new).push(*from);
            }
        }

        // Trace backwards (inflows) up to 5 hops
        let mut queue = VecDeque::new();
        queue.push_back((from_idx, 0));
        visited.insert(from_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= 5 {
                continue;
            }
            max_depth = max_depth.max(depth);

            if let Some(sources) = rev_adj.get(&current) {
                for &src in sources {
                    if !visited.contains(&src) {
                        visited.insert(src);
                        queue.push_back((src, depth + 1));
                    }
                }
            }
        }

        // Trace forwards (outflows) up to 5 hops
        visited.clear();
        queue.clear();
        queue.push_back((to_idx, 0));
        visited.insert(to_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= 5 {
                continue;
            }
            max_depth = max_depth.max(depth);

            if let Some(targets) = adj.get(&current) {
                for &tgt in targets {
                    if !visited.contains(&tgt) {
                        visited.insert(tgt);
                        queue.push_back((tgt, depth + 1));
                    }
                }
            }
        }

        max_depth
    }

    /// Count mixer nodes in the path between two nodes
    pub fn count_mixers_in_path<G: GraphData>(
        &self,
        graph: &G,
        from_idx: usize,
        to_idx: usize,
    ) -> usize {
        let mixers = self.detect_mixer_nodes(graph);
        let edges = graph.edges();
        let mut count = 0;

        // Check if any mixer is connected to either endpoint
        for mixer_idx in mixers {
            let has_from_connection = edges.iter().any(|(f, t, _, _, _)| {
                (*f == from_idx && *t == mixer_idx) || (*f == mixer_idx && *t == from_idx)
            });
            let has_to_connection = edges.iter().any(|(f, t, _, _, _)| {
                (*f == to_idx && *t == mixer_idx) || (*f == mixer_idx && *t == to_idx)
            });

            if has_from_connection || has_to_connection {
                count += 1;
            }
        }

        count
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Simple test graph implementation
    struct TestGraph {
        nodes: Vec<(String, String)>, // (address, type)
        edges: Vec<(usize, usize, f64, String, Option<String>)>,
    }

    impl GraphData for TestGraph {
        fn node_count(&self) -> usize {
            self.nodes.len()
        }

        fn edge_count(&self) -> usize {
            self.edges.len()
        }

        fn node_address(&self, idx: usize) -> Option<&str> {
            self.nodes.get(idx).map(|(addr, _)| addr.as_str())
        }

        fn node_type(&self, idx: usize) -> Option<&str> {
            self.nodes.get(idx).map(|(_, t)| t.as_str())
        }

        fn edges(&self) -> Vec<(usize, usize, f64, String, Option<String>)> {
            self.edges.clone()
        }

        fn incoming_edges(&self, node_idx: usize) -> Vec<usize> {
            self.edges
                .iter()
                .enumerate()
                .filter(|(_, (_, to, _, _, _))| *to == node_idx)
                .map(|(i, _)| i)
                .collect()
        }

        fn outgoing_edges(&self, node_idx: usize) -> Vec<usize> {
            self.edges
                .iter()
                .enumerate()
                .filter(|(_, (from, _, _, _, _))| *from == node_idx)
                .map(|(i, _)| i)
                .collect()
        }

        fn edge_data(&self, edge_idx: usize) -> Option<(usize, usize, f64, String, Option<String>)> {
            self.edges.get(edge_idx).cloned()
        }
    }

    #[test]
    fn test_circular_flow_detection() {
        let graph = TestGraph {
            nodes: vec![
                ("A".to_string(), "Target".to_string()),
                ("B".to_string(), "Wallet".to_string()),
                ("C".to_string(), "Wallet".to_string()),
            ],
            edges: vec![
                (0, 1, 10.0, "SOL".to_string(), None), // A -> B
                (1, 2, 10.0, "SOL".to_string(), None), // B -> C
                (2, 0, 10.0, "SOL".to_string(), None), // C -> A (cycle!)
            ],
        };

        let forensics = GraphForensics::with_defaults();
        let cycles = forensics.detect_circular_flows(&graph);

        assert!(!cycles.is_empty(), "Should detect circular flow A->B->C->A");
        assert_eq!(cycles[0].cycle_length, 3);
    }

    #[test]
    fn test_mixer_detection() {
        let graph = TestGraph {
            nodes: vec![
                ("mixer".to_string(), "Target".to_string()),
                ("src1".to_string(), "Wallet".to_string()),
                ("src2".to_string(), "Wallet".to_string()),
                ("src3".to_string(), "Wallet".to_string()),
                ("dst1".to_string(), "Wallet".to_string()),
                ("dst2".to_string(), "Wallet".to_string()),
                ("dst3".to_string(), "Wallet".to_string()),
            ],
            edges: vec![
                (1, 0, 1.0, "SOL".to_string(), None), // src1 -> mixer
                (2, 0, 1.0, "SOL".to_string(), None), // src2 -> mixer
                (3, 0, 1.0, "SOL".to_string(), None), // src3 -> mixer
                (0, 4, 1.0, "SOL".to_string(), None), // mixer -> dst1
                (0, 5, 1.0, "SOL".to_string(), None), // mixer -> dst2
                (0, 6, 1.0, "SOL".to_string(), None), // mixer -> dst3
            ],
        };

        let forensics = GraphForensics::with_defaults();
        let mixers = forensics.detect_mixer_nodes(&graph);

        assert!(mixers.contains(&0), "Node 0 should be detected as mixer");
    }

    #[test]
    fn test_behavior_classification_dormant() {
        let graph = TestGraph {
            nodes: vec![("dormant".to_string(), "Wallet".to_string())],
            edges: vec![],
        };

        let forensics = GraphForensics::with_defaults();
        let behavior = forensics.classify_wallet_behavior(&graph, 0);

        assert_eq!(behavior, WalletBehaviorType::Dormant);
    }

    #[test]
    fn test_risk_level_from_score() {
        assert_eq!(RiskLevel::from_score(80.0), RiskLevel::Critical);
        assert_eq!(RiskLevel::from_score(60.0), RiskLevel::High);
        assert_eq!(RiskLevel::from_score(40.0), RiskLevel::Medium);
        assert_eq!(RiskLevel::from_score(10.0), RiskLevel::Low);
    }

    #[test]
    fn test_timestamp_parsing() {
        let forensics = GraphForensics::with_defaults();

        // Unix timestamp
        assert_eq!(forensics.parse_timestamp("1700000000"), Some(1700000000));

        // ISO 8601 (approximate - real implementation should use chrono)
        let ts = forensics.parse_timestamp("2024-01-15T12:00:00Z");
        assert!(ts.is_some());
    }
}
