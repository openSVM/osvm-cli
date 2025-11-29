//! WalletGraph visualization - copied from main TUI
//! This is a 2,600+ line module, so we allow unused code to avoid noise
#![allow(unused)]

use ratzilla::ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Widget, Wrap, canvas::{Canvas, Points, Line as CanvasLine, Circle, Rectangle}},
    Frame,
};
use std::collections::HashMap;
// Clipboard not available in WASM - use browser APIs instead

// ═══════════════════════════════════════════════════════════════════════════
// WASM Stub Types (simplified versions for browser)
// These are minimal implementations to allow the graph to render.
// Full implementations exist in the main osvm-cli crate.
// ═══════════════════════════════════════════════════════════════════════════

/// Stub EntityCluster for WASM (simplified)
#[derive(Debug, Clone, Default)]
pub struct EntityCluster {
    pub cluster_id: usize,
    pub wallet_addresses: Vec<String>,
    pub confidence: f64,
}

/// Stub ThresholdConfig for WASM (matches main crate structure)
#[derive(Debug, Clone)]
pub struct ThresholdConfig {
    pub whale_amount_sol: f64,
    pub rapid_txns_critical: f64,
    pub rapid_volume_critical: f64,
    pub complexity_high: f64,
    pub complexity_suspicious: f64,
    pub time_windows: Vec<u64>,
}

impl Default for ThresholdConfig {
    fn default() -> Self {
        Self {
            whale_amount_sol: 100.0,
            rapid_txns_critical: 20.0,
            rapid_volume_critical: 1000.0,
            complexity_high: 5.0,
            complexity_suspicious: 3.0,
            time_windows: vec![60, 300, 600, 3600], // 1min, 5min, 10min, 1hr
        }
    }
}

/// Stub ForensicsConfig for WASM (matches main crate structure)
#[derive(Debug, Clone)]
pub struct ForensicsConfig {
    pub thresholds: ThresholdConfig,
}

impl Default for ForensicsConfig {
    fn default() -> Self {
        Self {
            thresholds: ThresholdConfig::default(),
        }
    }
}

impl ForensicsConfig {
    pub fn load() -> Option<Self> {
        Some(Self::default())
    }
}

/// Stub WalletMetadata for WASM
#[derive(Debug, Clone)]
pub struct ClusterWalletMetadata {
    pub address: String,
    pub first_seen: Option<u64>,
    pub transaction_count: usize,
    pub behavior_type: Option<String>,
}

/// Stub TransferMetadata for WASM
#[derive(Debug, Clone)]
pub struct ClusterTransferMetadata {
    pub amount: f64,
    pub token: String,
    pub timestamp: Option<u64>,
    pub is_initial_funding: bool,
    pub gas_price: Option<f64>,
}

/// Stub EntityClusterer for WASM
#[derive(Debug, Default)]
pub struct EntityClusterer {
    min_confidence: f64,
    min_cluster_size: usize,
}

impl EntityClusterer {
    pub fn new(min_confidence: f64, min_cluster_size: usize) -> Self {
        Self { min_confidence, min_cluster_size }
    }

    pub fn detect_clusters(
        &self,
        _wallets: &[(String, ClusterWalletMetadata)],
        _connections: &[(usize, usize, ClusterTransferMetadata)],
    ) -> Vec<EntityCluster> {
        // Simplified stub - no clustering in WASM version
        Vec::new()
    }

    pub fn get_cluster_color(cluster_id: usize) -> (u8, u8, u8) {
        let colors = [
            (255, 100, 100), (100, 255, 100), (100, 100, 255),
            (255, 255, 100), (255, 100, 255), (100, 255, 255),
        ];
        colors[cluster_id % colors.len()]
    }
}

#[derive(Debug, Clone)]
pub enum WalletNodeType {
    Target,      // Red - the wallet being investigated
    Funding,     // Green - wallets that funded the target
    Recipient,   // Blue - wallets that received from target
    DeFi,        // Magenta - DEX/DeFi protocols
    Token,       // Yellow - token contracts
    Mixer,       // DarkGray - detected mixing/tumbling node
}

impl WalletNodeType {
    pub fn color(&self) -> Color {
        match self {
            WalletNodeType::Target => Color::Red,
            WalletNodeType::Funding => Color::Green,
            WalletNodeType::Recipient => Color::Blue,
            WalletNodeType::DeFi => Color::Magenta,
            WalletNodeType::Token => Color::Yellow,
            WalletNodeType::Mixer => Color::DarkGray,
        }
    }

    pub fn symbol(&self) -> &str {
        match self {
            WalletNodeType::Target => "🔴",
            WalletNodeType::Funding => "🟢",
            WalletNodeType::Recipient => "🔵",
            WalletNodeType::DeFi => "🟣",
            WalletNodeType::Token => "🟡",
            WalletNodeType::Mixer => "⚫",
        }
    }
}

pub struct WalletNode {
    pub address: String,
    pub node_type: WalletNodeType,
    pub label: String,
    pub amount: Option<f64>,
    pub token: Option<String>,
}

/// Edge label with transaction metadata
#[derive(Debug, Clone)]
pub struct EdgeLabel {
    pub amount: f64,
    pub token: String,
    pub timestamp: Option<String>,
    pub signature: Option<String>,  // Transaction signature for Solana Explorer
}

impl EdgeLabel {
    pub fn format_short(&self) -> String {
        if self.amount >= 1000.0 {
            format!("{:.1}k {}", self.amount / 1000.0, self.token)
        } else if self.amount >= 1.0 {
            format!("{:.2} {}", self.amount, self.token)
        } else {
            format!("{:.4} {}", self.amount, self.token)
        }
    }
}

/// Selection mode - navigating nodes vs edges
#[derive(Debug, Clone, PartialEq)]
pub enum SelectionMode {
    Node(usize),  // Selected node index
    Edge { edge_idx: usize, from_node: usize, to_node: usize },  // Selected edge with endpoints
}

/// Wallet behavior classification based on transaction patterns
#[derive(Debug, Clone, PartialEq)]
pub enum WalletBehaviorType {
    Bot,           // Regular intervals, high frequency, programmatic patterns
    Exchange,      // Very high volume, many counterparties, round amounts
    Trader,        // DEX interactions, moderate volume, varied timing
    Mixer,         // Many small inputs/outputs, obfuscation patterns
    EOA,           // Externally Owned Account - human-like patterns
    Contract,      // Program account with deposits/withdrawals
    Dormant,       // Very low activity
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

/// Alert severity levels
#[derive(Debug, Clone, PartialEq)]
pub enum AlertSeverity {
    Critical,  // >20 txns/min or >1000 SOL/min
    High,      // >10 txns/min or >500 SOL/min
    Medium,    // >5 txns/min or >100 SOL/min
    Low,       // Informational
}

/// Circular flow pattern detection
#[derive(Debug, Clone)]
pub struct CircularFlow {
    pub path: Vec<String>,      // Wallet addresses in the cycle
    pub total_amount: f64,
    pub token: String,
    pub cycle_length: usize,
}

/// Explainable risk assessment
#[derive(Debug, Clone)]
pub struct RiskExplanation {
    pub score: f64,              // 0-100
    pub level: RiskLevel,
    pub reasons: Vec<String>,    // Human-readable explanations
    pub alerts: Vec<String>,     // Critical findings
}

#[derive(Debug, Clone, PartialEq)]
pub enum RiskLevel {
    Critical,  // 75-100
    High,      // 50-75
    Medium,    // 25-50
    Low,       // 0-25
}

/// Investigation trail - breadcrumb navigation through the graph
#[derive(Debug, Clone)]
pub struct InvestigationTrail {
    pub steps: Vec<TrailStep>,
    pub current_index: usize,
}

/// Get current time in milliseconds (WASM-compatible)
fn now_ms() -> f64 {
    web_sys::window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
}

#[derive(Debug, Clone)]
pub struct TrailStep {
    pub node_index: usize,
    pub wallet_address: String,
    pub risk_level: RiskLevel,
    pub timestamp: f64, // milliseconds from Performance.now() (WASM-compatible)
    pub note: Option<String>,
}

impl InvestigationTrail {
    pub fn new(start_node: usize, start_address: String, start_risk: RiskLevel) -> Self {
        Self {
            steps: vec![TrailStep {
                node_index: start_node,
                wallet_address: start_address,
                risk_level: start_risk,
                timestamp: now_ms(),
                note: Some("Investigation start".to_string()),
            }],
            current_index: 0,
        }
    }

    pub fn add_step(&mut self, node_index: usize, address: String, risk: RiskLevel, note: Option<String>) {
        // If we're not at the end of the trail, truncate forward history
        if self.current_index < self.steps.len() - 1 {
            self.steps.truncate(self.current_index + 1);
        }

        self.steps.push(TrailStep {
            node_index,
            wallet_address: address,
            risk_level: risk,
            timestamp: now_ms(),
            note,
        });
        self.current_index = self.steps.len() - 1;
    }

    pub fn go_back(&mut self) -> Option<usize> {
        if self.current_index > 0 {
            self.current_index -= 1;
            Some(self.steps[self.current_index].node_index)
        } else {
            None
        }
    }

    pub fn go_forward(&mut self) -> Option<usize> {
        if self.current_index < self.steps.len() - 1 {
            self.current_index += 1;
            Some(self.steps[self.current_index].node_index)
        } else {
            None
        }
    }

    pub fn current_step(&self) -> &TrailStep {
        &self.steps[self.current_index]
    }

    pub fn export_summary(&self) -> String {
        let mut summary = String::from("Investigation Trail:\n");
        for (i, step) in self.steps.iter().enumerate() {
            let marker = if i == self.current_index { "→" } else { " " };
            let risk_icon = match step.risk_level {
                RiskLevel::Critical => "🔴",
                RiskLevel::High => "🟠",
                RiskLevel::Medium => "🟡",
                RiskLevel::Low => "🟢",
            };
            summary.push_str(&format!(
                "{} {} {} - {:?} {}\n",
                marker,
                risk_icon,
                step.wallet_address,
                step.risk_level,
                step.note.as_ref().map(|n| format!("({})", n)).unwrap_or_default()
            ));
        }
        summary
    }
}

pub struct WalletGraph {
    nodes: Vec<(String, WalletNode)>, // (address, node_data)
    connections: Vec<(usize, usize, EdgeLabel)>, // (from_idx, to_idx, edge_data)
    target_wallet: String,
    /// Current selection (node or edge)
    pub selection: SelectionMode,
    /// Collapsed node indices (hidden children)
    pub collapsed_nodes: std::collections::HashSet<usize>,
    /// Node positions for canvas rendering (x, y) in graph space
    pub node_positions: Vec<(f64, f64)>,
    /// Viewport for large graphs (center_x, center_y, zoom_level)
    pub viewport: (f64, f64, f64),
    /// Max depth for BFS exploration (increased to 15 for deep investigation)
    pub max_depth: usize,
    /// Current depth reached
    pub current_depth: usize,
    /// Search query and results
    pub search_query: String,
    pub search_active: bool,
    pub search_results: Vec<usize>,  // Node indices matching search
    pub search_result_idx: usize,    // Current result position
    /// Toast notification
    pub toast_message: Option<String>,
    pub toast_timer: u8,  // Frames remaining to show toast
    /// In-memory wallet cache for multi-hop discovery (address -> metadata)
    pub wallet_cache: HashMap<String, WalletCacheEntry>,
    /// Path-connected nodes (only nodes with paths to target)
    pub connected_nodes: std::collections::HashSet<usize>,
    /// Entity clusters (wallets controlled by same entity)
    pub entity_clusters: Vec<EntityCluster>,
    /// Wallet to cluster ID mapping
    pub wallet_to_cluster: HashMap<String, usize>,
    /// Investigation trail (breadcrumb navigation)
    pub investigation_trail: Option<InvestigationTrail>,
    /// Show minimap in corner
    pub show_minimap: bool,
    /// Show investigation trail at bottom
    pub show_trail: bool,
    /// Detail panel scroll position
    pub detail_scroll: usize,
    /// Filtered nodes (nodes with only 1 inflow OR 1 outflow - hidden from display)
    pub filtered_nodes: std::collections::HashSet<usize>,
    /// Folded nodes by direction (depth, flow_direction) -> vec of filtered node indices
    pub folded_groups: HashMap<(usize, i32), Vec<usize>>,
}

/// Cached wallet data for multi-hop path discovery
#[derive(Debug, Clone)]
pub struct WalletCacheEntry {
    pub address: String,
    pub inflows: Vec<(String, f64, String)>,  // (from_address, amount, token)
    pub outflows: Vec<(String, f64, String)>, // (to_address, amount, token)
    pub discovered_at_depth: usize,
    pub is_rendered: bool,  // false if not yet connected to target
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

/// Input event for graph navigation
#[derive(Debug, Clone, PartialEq)]
pub enum GraphInput {
    Up,
    Down,
    Left,
    Right,
    Toggle,
    Select,
    Escape,
    ZoomIn,
    ZoomOut,
    PanUp,
    PanDown,
    PanLeft,
    PanRight,
    IncreaseDepth,
    DecreaseDepth,
    StartSearch,
    SearchChar(char),
    SearchBackspace,
    SearchNext,
    SearchPrev,
    Copy,
    HopToWallet,  // Re-center graph on selected wallet
    ScrollDetailUp,   // Scroll detail panel up
    ScrollDetailDown, // Scroll detail panel down
}

impl WalletGraph {
    pub fn new(target_wallet: String) -> Self {
        let mut nodes = Vec::new();

        // Add target wallet as first node
        let target_label = format!("{}...{}", &target_wallet[..6], &target_wallet[target_wallet.len()-4..]);
        nodes.push((
            target_wallet.clone(),
            WalletNode {
                address: target_wallet.clone(),
                node_type: WalletNodeType::Target,
                label: target_label,
                amount: None,
                token: None,
            },
        ));

        let mut connected_nodes = std::collections::HashSet::new();
        connected_nodes.insert(0); // Target wallet is always connected

        Self {
            nodes,
            connections: Vec::new(),
            target_wallet: target_wallet.clone(),
            selection: SelectionMode::Node(0), // Start with target wallet selected
            collapsed_nodes: std::collections::HashSet::new(),
            node_positions: vec![(0.0, 0.0)], // Target wallet at origin
            viewport: (0.0, 0.0, 1.0), // (center_x, center_y, zoom=1.0)
            max_depth: 15, // Increased for deep blockchain investigation
            current_depth: 0,
            search_query: String::new(),
            search_active: false,
            search_results: Vec::new(),
            search_result_idx: 0,
            toast_message: None,
            toast_timer: 0,
            wallet_cache: HashMap::new(),
            connected_nodes,
            entity_clusters: Vec::new(),
            wallet_to_cluster: HashMap::new(),
            investigation_trail: Some(InvestigationTrail::new(0, target_wallet, RiskLevel::Low)),
            show_minimap: true,
            show_trail: true,
            detail_scroll: 0,
            filtered_nodes: std::collections::HashSet::new(),
            folded_groups: HashMap::new(),
        }
    }

    /// Get selected node index (for backward compatibility)
    fn selected_node(&self) -> Option<usize> {
        match &self.selection {
            SelectionMode::Node(idx) => Some(*idx),
            SelectionMode::Edge { from_node, .. } => Some(*from_node),
        }
    }

    /// Set selected node (for backward compatibility)
    fn set_selected_node(&mut self, node_idx: Option<usize>) {
        if let Some(idx) = node_idx {
            self.selection = SelectionMode::Node(idx);
            self.update_trail_on_selection(idx);
            self.center_camera_on_selection();
        }
    }

    /// Find node index by wallet address
    pub fn find_node_by_address(&self, address: &str) -> Option<usize> {
        self.nodes.iter().position(|(addr, _)| addr == address)
    }

    /// Select a node by wallet address (public API for external sync)
    pub fn select_by_address(&mut self, address: &str) {
        if let Some(idx) = self.find_node_by_address(address) {
            self.selection = SelectionMode::Node(idx);
            self.update_trail_on_selection(idx);
            self.center_camera_on_selection();
        }
    }

    /// Auto-center camera on current selection (node or edge midpoint)
    fn center_camera_on_selection(&mut self) {
        match &self.selection {
            SelectionMode::Node(idx) => {
                if let Some(pos) = self.node_positions.get(*idx) {
                    self.viewport.0 = pos.0;
                    self.viewport.1 = pos.1;
                }
            }
            SelectionMode::Edge { from_node, to_node, .. } => {
                // Center on edge midpoint
                if let (Some(from_pos), Some(to_pos)) = (
                    self.node_positions.get(*from_node),
                    self.node_positions.get(*to_node)
                ) {
                    self.viewport.0 = (from_pos.0 + to_pos.0) / 2.0;
                    self.viewport.1 = (from_pos.1 + to_pos.1) / 2.0;
                }
            }
        }
    }

    /// Get outgoing edges from a node
    fn outgoing_edges(&self, node_idx: usize) -> Vec<usize> {
        self.connections
            .iter()
            .enumerate()
            .filter_map(|(edge_idx, (from, _, _))| {
                if *from == node_idx {
                    Some(edge_idx)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get incoming edges to a node
    fn incoming_edges(&self, node_idx: usize) -> Vec<usize> {
        self.connections
            .iter()
            .enumerate()
            .filter_map(|(edge_idx, (_, to, _))| {
                if *to == node_idx {
                    Some(edge_idx)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Handle keyboard input for graph navigation
    pub fn handle_input(&mut self, input: GraphInput) -> Option<String> {
        match input {
            // EDGE-AWARE NAVIGATION (follows transaction flows)
            GraphInput::Up => {
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        // When on node, up/down just moves in list (fallback)
                        if *node_idx > 0 {
                            let new_idx = node_idx - 1;
                            self.selection = SelectionMode::Node(new_idx);
                            self.update_trail_on_selection(new_idx);
                        }
                    }
                    SelectionMode::Edge { edge_idx, from_node, to_node } => {
                        // When on edge, up/down cycles through edges from same source
                        let outgoing = self.outgoing_edges(*from_node);
                        if let Some(pos) = outgoing.iter().position(|e| e == edge_idx) {
                            if pos > 0 {
                                let new_edge = outgoing[pos - 1];
                                let (_, to, _) = self.connections[new_edge];
                                self.selection = SelectionMode::Edge {
                                    edge_idx: new_edge,
                                    from_node: *from_node,
                                    to_node: to,
                                };
                            }
                        }
                    }
                }
                self.center_camera_on_selection();
                None
            }
            GraphInput::Down => {
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        if node_idx + 1 < self.nodes.len() {
                            let new_idx = node_idx + 1;
                            self.selection = SelectionMode::Node(new_idx);
                            self.update_trail_on_selection(new_idx);
                        }
                    }
                    SelectionMode::Edge { edge_idx, from_node, to_node } => {
                        // Cycle through outgoing edges
                        let outgoing = self.outgoing_edges(*from_node);
                        if let Some(pos) = outgoing.iter().position(|e| e == edge_idx) {
                            if pos + 1 < outgoing.len() {
                                let new_edge = outgoing[pos + 1];
                                let (_, to, _) = self.connections[new_edge];
                                self.selection = SelectionMode::Edge {
                                    edge_idx: new_edge,
                                    from_node: *from_node,
                                    to_node: to,
                                };
                            }
                        }
                    }
                }
                self.center_camera_on_selection();
                None
            }
            GraphInput::Left => {
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        // LEFT from node → select incoming edge
                        let incoming = self.incoming_edges(*node_idx);
                        if !incoming.is_empty() {
                            let first_edge = incoming[0];
                            let (from, _, _) = self.connections[first_edge];
                            self.selection = SelectionMode::Edge {
                                edge_idx: first_edge,
                                from_node: from,
                                to_node: *node_idx,
                            };
                        }
                    }
                    SelectionMode::Edge { from_node, .. } => {
                        // LEFT from edge → go back to source wallet
                        let node_idx = *from_node;
                        self.selection = SelectionMode::Node(node_idx);
                        self.update_trail_on_selection(node_idx);
                    }
                }
                self.center_camera_on_selection();
                None
            }
            GraphInput::Right => {
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        // RIGHT from node → select outgoing edge
                        let outgoing = self.outgoing_edges(*node_idx);
                        if !outgoing.is_empty() {
                            let first_edge = outgoing[0];
                            let (_, to, _) = self.connections[first_edge];
                            self.selection = SelectionMode::Edge {
                                edge_idx: first_edge,
                                from_node: *node_idx,
                                to_node: to,
                            };
                        }
                    }
                    SelectionMode::Edge { to_node, .. } => {
                        // RIGHT from edge → jump to destination wallet
                        let node_idx = *to_node;
                        self.selection = SelectionMode::Node(node_idx);
                        self.update_trail_on_selection(node_idx);
                    }
                }
                self.center_camera_on_selection();
                None
            }
            // Viewport panning (FASTER for better UX)
            GraphInput::PanUp => {
                self.viewport.1 += 20.0 / self.viewport.2;
                None
            }
            GraphInput::PanDown => {
                self.viewport.1 -= 20.0 / self.viewport.2;
                None
            }
            GraphInput::PanLeft => {
                self.viewport.0 -= 20.0 / self.viewport.2;
                None
            }
            GraphInput::PanRight => {
                self.viewport.0 += 20.0 / self.viewport.2;
                None
            }
            // Zoom controls
            GraphInput::ZoomIn => {
                self.viewport.2 = (self.viewport.2 * 1.2).min(10.0);
                None
            }
            GraphInput::ZoomOut => {
                self.viewport.2 = (self.viewport.2 / 1.2).max(0.1);
                None
            }
            GraphInput::Toggle => {
                if let Some(idx) = self.selected_node() {
                    if self.collapsed_nodes.contains(&idx) {
                        self.collapsed_nodes.remove(&idx);
                    } else {
                        self.collapsed_nodes.insert(idx);
                    }
                }
                None
            }
            GraphInput::Select => {
                self.selected_node()
                    .and_then(|idx| self.nodes.get(idx))
                    .map(|(addr, _)| addr.clone())
            }
            GraphInput::Escape => {
                // Escape deselects (go back to target wallet)
                self.set_selected_node(Some(0));
                None
            }
            // Depth control for BFS exploration
            GraphInput::IncreaseDepth => {
                if self.max_depth < 20 {
                    self.max_depth += 1;
                }
                None
            }
            GraphInput::DecreaseDepth => {
                if self.max_depth > 1 {
                    self.max_depth -= 1;
                }
                None
            }
            // Search functionality
            GraphInput::StartSearch => {
                self.search_active = true;
                self.search_query.clear();
                self.search_results.clear();
                self.search_result_idx = 0;
                None
            }
            GraphInput::SearchChar(c) => {
                if self.search_active {
                    self.search_query.push(c);
                    self.perform_search();
                }
                None
            }
            GraphInput::SearchBackspace => {
                if self.search_active {
                    self.search_query.pop();
                    self.perform_search();
                }
                None
            }
            GraphInput::SearchNext => {
                if !self.search_results.is_empty() {
                    self.search_result_idx = (self.search_result_idx + 1) % self.search_results.len();
                    self.set_selected_node(Some(self.search_results[self.search_result_idx]));
                    // Center viewport on result
                    if let Some(pos) = self.node_positions.get(self.search_results[self.search_result_idx]) {
                        self.viewport.0 = pos.0;
                        self.viewport.1 = pos.1;
                    }
                }
                None
            }
            GraphInput::SearchPrev => {
                if !self.search_results.is_empty() {
                    self.search_result_idx = if self.search_result_idx == 0 {
                        self.search_results.len() - 1
                    } else {
                        self.search_result_idx - 1
                    };
                    self.set_selected_node(Some(self.search_results[self.search_result_idx]));
                    // Center viewport on result
                    if let Some(pos) = self.node_positions.get(self.search_results[self.search_result_idx]) {
                        self.viewport.0 = pos.0;
                        self.viewport.1 = pos.1;
                    }
                }
                None
            }
            // Copy to clipboard (WASM: uses browser clipboard API)
            GraphInput::Copy => {
                if let Some(idx) = self.selected_node() {
                    if let Some((addr, _)) = self.nodes.get(idx) {
                        // In WASM, we'd use navigator.clipboard.writeText via web-sys
                        // For now, just show toast with address
                        self.show_toast(format!("Address: {}...{}", &addr[..8.min(addr.len())], &addr[addr.len().saturating_sub(8)..]));
                    }
                }
                None
            }
            // Hop to wallet - re-center graph on selected wallet
            GraphInput::HopToWallet => {
                if let Some(idx) = self.selected_node() {
                    if let Some(pos) = self.node_positions.get(idx) {
                        // Center viewport on selected wallet
                        self.viewport.0 = pos.0;
                        self.viewport.1 = pos.1;

                        // Show confirmation toast
                        if let Some((addr, _)) = self.nodes.get(idx) {
                            self.show_toast(format!("Centered on: {}...{}", &addr[..8], &addr[addr.len()-8..]));
                        }
                    }
                }
                None
            }
            // Detail panel scrolling
            GraphInput::ScrollDetailUp => {
                if self.detail_scroll > 0 {
                    self.detail_scroll -= 1;
                }
                None
            }
            GraphInput::ScrollDetailDown => {
                self.detail_scroll += 1;
                None
            }
        }
    }

    /// Perform fuzzy search on wallet addresses and labels
    fn perform_search(&mut self) {
        self.search_results.clear();
        let query = self.search_query.to_lowercase();

        if query.is_empty() {
            return;
        }

        for (idx, (addr, node)) in self.nodes.iter().enumerate() {
            // Check if address or label contains query (case-insensitive)
            if addr.to_lowercase().contains(&query) || node.label.to_lowercase().contains(&query) {
                self.search_results.push(idx);
            }
        }

        // Auto-select first result
        if !self.search_results.is_empty() {
            self.search_result_idx = 0;
            self.set_selected_node(Some(self.search_results[0]));
            // Center viewport on first result
            if let Some(pos) = self.node_positions.get(self.search_results[0]) {
                self.viewport.0 = pos.0;
                self.viewport.1 = pos.1;
            }
        }
    }

    /// Show toast notification
    fn show_toast(&mut self, message: String) {
        self.toast_message = Some(message);
        self.toast_timer = 120; // Show for ~2 seconds at 60 FPS
    }

    /// Update toast timer (call every frame)
    pub fn tick_toast(&mut self) {
        if self.toast_timer > 0 {
            self.toast_timer -= 1;
            if self.toast_timer == 0 {
                self.toast_message = None;
            }
        }
    }

    /// Get outgoing connections from a node
    pub fn get_outgoing(&self, node_idx: usize) -> Vec<usize> {
        self.connections
            .iter()
            .filter(|(from, _, _)| *from == node_idx)
            .map(|(_, to, _)| *to)
            .collect()
    }

    pub fn is_collapsed(&self, node_idx: usize) -> bool {
        self.collapsed_nodes.contains(&node_idx)
    }

    pub fn is_selected(&self, node_idx: usize) -> bool {
        self.selected_node() == Some(node_idx)
    }

    /// Get info about selected node or edge (with scrolling support)
    pub fn get_selected_info(&self) -> Option<String> {
        match &self.selection {
            SelectionMode::Node(idx) => {
                self.nodes.get(*idx).map(|(addr, node)| {
                    let mut info = String::new();

                    // Header
                    info.push_str(&format!("{} WALLET DETAILS\n", node.node_type.symbol()));
                    info.push_str(&format!("═══════════════════════════\n"));
                    info.push_str(&format!("Address: {}\n", addr));
                    info.push_str(&format!("Label: {}\n\n", node.label));

                    // Get incoming and outgoing transfers
                    let incoming: Vec<_> = self.connections
                        .iter()
                        .enumerate()
                        .filter(|(_, (_, to, _))| *to == *idx)
                        .collect();
                    let outgoing: Vec<_> = self.connections
                        .iter()
                        .enumerate()
                        .filter(|(_, (from, _, _))| *from == *idx)
                        .collect();

                    info.push_str(&format!("📥 INFLOWS: {} transfers\n", incoming.len()));
                    info.push_str("─────────────────────────\n");

                    if incoming.is_empty() {
                        info.push_str("  (none)\n");
                    } else {
                        // Show incoming transfers with scroll support
                        for (i, (edge_idx, (from, _, label))) in incoming.iter().enumerate() {
                            if i < self.detail_scroll {
                                continue; // Skip scrolled-past items
                            }
                            if i >= self.detail_scroll + 10 {
                                info.push_str(&format!("  ... and {} more (scroll down)\n", incoming.len() - i));
                                break;
                            }

                            let from_addr = self.nodes.get(*from)
                                .map(|(a, _)| if a.len() > 12 {
                                    format!("{}...{}", &a[..6], &a[a.len()-4..])
                                } else {
                                    a.clone()
                                })
                                .unwrap_or_else(|| "Unknown".to_string());

                            info.push_str(&format!(
                                "  {}. {} - {:.4} {}\n",
                                i + 1,
                                from_addr,
                                label.amount,
                                label.token
                            ));
                            if let Some(ts) = &label.timestamp {
                                info.push_str(&format!("      Time: {}\n", ts));
                            }
                        }
                    }

                    info.push_str(&format!("\n📤 OUTFLOWS: {} transfers\n", outgoing.len()));
                    info.push_str("─────────────────────────\n");

                    if outgoing.is_empty() {
                        info.push_str("  (none)\n");
                    } else {
                        // Show outgoing transfers with scroll support
                        let scroll_offset = self.detail_scroll.saturating_sub(incoming.len() + 3);
                        for (i, (edge_idx, (_, to, label))) in outgoing.iter().enumerate() {
                            if i < scroll_offset {
                                continue;
                            }
                            if i >= scroll_offset + 10 {
                                info.push_str(&format!("  ... and {} more (scroll down)\n", outgoing.len() - i));
                                break;
                            }

                            let to_addr = self.nodes.get(*to)
                                .map(|(a, _)| if a.len() > 12 {
                                    format!("{}...{}", &a[..6], &a[a.len()-4..])
                                } else {
                                    a.clone()
                                })
                                .unwrap_or_else(|| "Unknown".to_string());

                            info.push_str(&format!(
                                "  {}. {} - {:.4} {}\n",
                                i + 1,
                                to_addr,
                                label.amount,
                                label.token
                            ));
                            if let Some(ts) = &label.timestamp {
                                info.push_str(&format!("      Time: {}\n", ts));
                            }
                        }
                    }

                    info.push_str("\n[j/k to scroll]");
                    if self.is_collapsed(*idx) {
                        info.push_str(" [▶ Collapsed]");
                    }

                    info
                })
            }
            SelectionMode::Edge { edge_idx, from_node, to_node } => {
                // Get edge metadata
                if let Some((from_addr, to_addr, edge_data)) = self.connections.get(*edge_idx) {
                    let from_label = self.nodes.get(*from_node)
                        .map(|(_, n)| n.label.clone())
                        .unwrap_or_else(|| "Unknown".to_string());
                    let to_label = self.nodes.get(*to_node)
                        .map(|(_, n)| n.label.clone())
                        .unwrap_or_else(|| "Unknown".to_string());

                    Some(format!(
                        "🔄 TRANSFER DETAILS\n\
                        ═══════════════════\n\
                        From: {}\n\
                        To: {}\n\
                        Amount: {:.4} {}\n\
                        Type: {}\n\
                        Time: {}\n\
                        Signature: {}\n\
                        \n\
                        📊 FLOW ANALYSIS\n\
                        ═══════════════════\n\
                        Token Path Depth: {} hops\n\
                        Mixer Nodes: {} detected\n\
                        Risk Score: {}",
                        from_label,
                        to_label,
                        edge_data.amount,
                        edge_data.token,
                        "SPL Transfer",  // Default transfer type
                        edge_data.timestamp.as_deref().unwrap_or("Unknown"),
                        edge_data.signature.as_deref()
                            .and_then(|s| if s.len() > 16 {
                                Some(format!("{}...{}", &s[..8], &s[s.len()-8..]))
                            } else {
                                Some(s.to_string())
                            })
                            .unwrap_or_else(|| "N/A".to_string()),
                        self.trace_token_path_depth(*from_node, *to_node, &edge_data.token),
                        self.count_mixers_in_path(*from_node, *to_node),
                        self.calculate_risk_score(*from_node, *to_node)
                    ))
                } else {
                    Some("Edge data not found".to_string())
                }
            }
        }
    }

    pub fn add_wallet(
        &mut self,
        address: String,
        node_type: WalletNodeType,
        label: String,
        amount: Option<f64>,
        token: Option<String>,
    ) -> usize {
        // Check if node already exists
        if let Some(pos) = self.nodes.iter().position(|(addr, _)| addr == &address) {
            return pos;
        }

        let short_label = if address.len() > 12 {
            format!("{}...{}", &address[..6], &address[address.len()-4..])
        } else {
            address.clone()
        };

        self.nodes.push((
            address.clone(),
            WalletNode {
                address,
                node_type,
                label: short_label,
                amount,
                token,
            },
        ));

        // Add node with layered circular layout
        let idx = self.nodes.len() - 1;
        self.position_node_in_layer(idx);

        // During bootstrap (no connections), add new nodes to connected set
        if self.connections.is_empty() {
            self.connected_nodes.insert(idx);
        }

        idx
    }

    /// Position node in concentric layers based on distance from center
    fn position_node_in_layer(&mut self, idx: usize) {
        // Calculate layer based on shortest path to target wallet
        let layer = self.calculate_node_layer(idx);

        // Count nodes in this layer
        let nodes_in_layer = self.count_nodes_in_layer(layer);
        let position_in_layer = nodes_in_layer; // This node's position

        // Calculate position in a circle for this layer
        let radius = 15.0 * (layer as f64 + 1.0); // Each layer 15 units apart
        let angle = (position_in_layer as f64) * 2.0 * std::f64::consts::PI / (nodes_in_layer as f64 + 1.0).max(6.0);

        self.node_positions.push((
            radius * angle.cos(),
            radius * angle.sin(),
        ));
    }

    fn calculate_node_layer(&self, idx: usize) -> usize {
        // Target wallet is layer 0
        if idx == 0 { return 0; }

        // Calculate based on connection depth
        // For now, use a simple heuristic based on node index
        // TODO: Use BFS to calculate actual distance
        ((idx as f64).log2() as usize).min(5)
    }

    fn count_nodes_in_layer(&self, layer: usize) -> usize {
        self.nodes.iter().enumerate()
            .filter(|(idx, _)| self.calculate_node_layer(*idx) == layer)
            .count()
    }

    pub fn add_connection(
        &mut self,
        from_address: &str,
        to_address: &str,
        amount: f64,
        token: String,
        timestamp: Option<String>,
        signature: Option<String>,
    ) {
        let from_idx = self.nodes.iter().position(|(addr, _)| addr == from_address);
        let to_idx = self.nodes.iter().position(|(addr, _)| addr == to_address);

        if let (Some(from), Some(to)) = (from_idx, to_idx) {
            let edge_label = EdgeLabel {
                amount,
                token,
                timestamp,
                signature,
            };
            self.connections.push((from, to, edge_label));
        }
    }

    /// Compute hierarchical layout: inflows left, target center, outflows right
    /// Uses columnar layout with BFS depth levels
    pub fn compute_hierarchical_layout(&mut self) {
        use std::collections::{HashMap, VecDeque};

        if self.nodes.is_empty() {
            return;
        }

        // Find selected node (or default to index 0)
        let center_idx = self.selected_node().unwrap_or(0);

        // BFS to compute distances and flow direction from center
        let mut distances: HashMap<usize, usize> = HashMap::new();
        let mut flow_direction: HashMap<usize, i32> = HashMap::new(); // -1 = inflow, 1 = outflow
        let mut queue = VecDeque::new();

        distances.insert(center_idx, 0);
        flow_direction.insert(center_idx, 0);
        queue.push_back(center_idx);

        while let Some(current) = queue.pop_front() {
            let current_dist = distances[&current];
            let current_flow = flow_direction[&current];

            // Find all connected nodes
            for (from, to, _) in &self.connections {
                let (neighbor, is_inflow) = if *from == current {
                    (Some(*to), false) // Current sends to neighbor (outflow)
                } else if *to == current {
                    (Some(*from), true) // Neighbor sends to current (inflow)
                } else {
                    (None, false)
                };

                if let Some(n) = neighbor {
                    if !distances.contains_key(&n) {
                        distances.insert(n, current_dist + 1);

                        // Determine flow: inherit from parent or determine from edge direction
                        let node_flow = if current_flow != 0 {
                            current_flow // Inherit parent's direction
                        } else {
                            if is_inflow { -1 } else { 1 } // First level: determine by edge
                        };
                        flow_direction.insert(n, node_flow);
                        queue.push_back(n);
                    }
                }
            }
        }

        // Group nodes by (depth, flow_direction)
        #[derive(Debug)]
        struct ColumnGroup {
            nodes: Vec<usize>,
            total_amount: HashMap<usize, f64>, // For sorting by transfer amount
        }

        let mut columns: HashMap<(usize, i32), ColumnGroup> = HashMap::new();

        for (node_idx, dist) in &distances {
            let flow = flow_direction.get(node_idx).copied().unwrap_or(0);
            let key = (*dist, flow);

            let group = columns.entry(key).or_insert_with(|| ColumnGroup {
                nodes: Vec::new(),
                total_amount: HashMap::new(),
            });

            // Calculate total transfer amount for this node
            let total = self.connections.iter()
                .filter(|(from, to, _)| *from == *node_idx || *to == *node_idx)
                .map(|(_, _, edge)| edge.amount)
                .sum::<f64>();

            group.total_amount.insert(*node_idx, total);
            group.nodes.push(*node_idx);
        }

        // Clear and rebuild positions
        self.node_positions.clear();
        self.node_positions.resize(self.nodes.len(), (0.0, 0.0));

        // Position center node at (0, 0)
        self.node_positions[center_idx] = (0.0, 0.0);

        // Row spacing (now vertical layout - inflows at top, outflows at bottom)
        let row_height = 60.0;  // Was column_width, now controls Y spacing
        let node_spacing = 18.0; // Horizontal padding between nodes

        // Position nodes in rows (rotated 90 degrees)
        for ((depth, flow), mut group) in columns.into_iter() {
            if depth == 0 { continue; } // Skip center

            // Sort nodes by total transfer amount (descending)
            group.nodes.sort_by(|a, b| {
                let amount_a = group.total_amount.get(a).copied().unwrap_or(0.0);
                let amount_b = group.total_amount.get(b).copied().unwrap_or(0.0);
                amount_b.partial_cmp(&amount_a).unwrap()
            });

            // Calculate Y position (row) - negative flow = inflows (top), positive = outflows (bottom)
            let y = flow as f64 * row_height * depth as f64;

            // Calculate X positions (stack horizontally with padding)
            let total_width = group.nodes.len() as f64 * node_spacing;
            let start_x = -total_width / 2.0;

            for (i, &node_idx) in group.nodes.iter().enumerate() {
                let x = start_x + i as f64 * node_spacing;

                if node_idx < self.node_positions.len() {
                    self.node_positions[node_idx] = (x, y);  // Swapped from (x,y) to maintain (x,y) format
                }
            }
        }

        // Handle disconnected nodes (place at far edges)
        let mut edge_count = 0;
        for i in 0..self.nodes.len() {
            if !distances.contains_key(&i) {
                let y = if edge_count % 2 == 0 { -200.0 } else { 200.0 };  // Swapped
                let x = (edge_count / 2) as f64 * node_spacing;              // Swapped
                self.node_positions[i] = (x, y);
                edge_count += 1;
            }
        }
    }

    pub fn add_transfer(
        &mut self,
        from: String,
        to: String,
        amount: f64,
        token: String,
        node_type_from: WalletNodeType,
        node_type_to: WalletNodeType,
        timestamp: Option<String>,
        signature: Option<String>,
    ) {
        // Add nodes if they don't exist
        let from_label = format!("{} (Source)", &from[..8.min(from.len())]);
        let to_label = format!("{} (Dest)", &to[..8.min(to.len())]);

        self.add_wallet(
            from.clone(),
            node_type_from,
            from_label,
            Some(amount),
            Some(token.clone()),
        );

        self.add_wallet(
            to.clone(),
            node_type_to,
            to_label,
            Some(amount),
            Some(token.clone()),
        );

        // Add connection with transfer info including timestamp and signature
        self.add_connection(&from, &to, amount, token, timestamp, signature);

        // Rebuild connected nodes after adding transfer
        self.rebuild_connected_nodes();
    }

    /// Rebuild the set of path-connected nodes using BFS from target wallet
    /// Only nodes reachable from target (via incoming OR outgoing edges) are marked connected
    fn rebuild_connected_nodes(&mut self) {
        use std::collections::VecDeque;

        self.connected_nodes.clear();

        // If no connections yet, mark ALL nodes as connected (bootstrap phase)
        if self.connections.is_empty() {
            for idx in 0..self.nodes.len() {
                self.connected_nodes.insert(idx);
            }
            return;
        }

        // Find target wallet index (should be 0, but let's be safe)
        let target_idx = self.nodes.iter()
            .position(|(addr, _)| addr == &self.target_wallet)
            .unwrap_or(0);

        // BFS from target wallet (bidirectional - follow edges in BOTH directions)
        let mut queue = VecDeque::new();
        queue.push_back(target_idx);
        self.connected_nodes.insert(target_idx);

        while let Some(current_idx) = queue.pop_front() {
            // Follow outgoing edges (money sent FROM current wallet)
            for (from, to, _) in &self.connections {
                if *from == current_idx && !self.connected_nodes.contains(to) {
                    self.connected_nodes.insert(*to);
                    queue.push_back(*to);
                }
            }

            // Follow incoming edges (money sent TO current wallet)
            for (from, to, _) in &self.connections {
                if *to == current_idx && !self.connected_nodes.contains(from) {
                    self.connected_nodes.insert(*from);
                    queue.push_back(*from);
                }
            }
        }
    }

    /// Check if a node should be rendered (must be path-connected to target)
    fn should_render_node(&self, node_idx: usize) -> bool {
        // ALWAYS return true - we want ALL nodes to have positions
        // so that edges can be drawn even to off-screen nodes
        // Visual filtering can happen later based on viewport, but positions must exist
        true
    }

    /// Update filtered nodes - hide wallets with exactly 1 inflow OR exactly 1 outflow
    pub fn update_node_filtering(&mut self) {
        self.filtered_nodes.clear();
        self.folded_groups.clear();

        // Identify nodes to filter (skip target wallet index 0)
        for idx in 1..self.nodes.len() {
            let in_count = self.connections.iter()
                .filter(|(_, to, _)| *to == idx)
                .count();
            let out_count = self.connections.iter()
                .filter(|(from, _, _)| *from == idx)
                .count();

            // Filter if has exactly 1 inflow OR exactly 1 outflow
            // Keep if has 0 (isolated) or 2+ (significant hub)
            if in_count == 1 || out_count == 1 {
                self.filtered_nodes.insert(idx);
            }
        }
    }

    /// Get folded node info for a group (depth, direction)
    pub fn get_folded_group_info(&self, depth: usize, flow: i32) -> Option<(usize, Vec<String>)> {
        let filtered_in_group: Vec<_> = self.filtered_nodes.iter()
            .filter(|&&idx| {
                // Check if this node belongs to this depth/flow group
                // This is a heuristic based on position
                if idx >= self.node_positions.len() {
                    return false;
                }

                // Simple heuristic: check if node is in approximately the right column
                let (x, _) = self.node_positions[idx];
                let expected_x_range = if flow < 0 {
                    // Inflow: negative X
                    (-(depth as f64 * 60.0 + 30.0), -(depth as f64 * 60.0 - 30.0))
                } else if flow > 0 {
                    // Outflow: positive X
                    (depth as f64 * 60.0 - 30.0, depth as f64 * 60.0 + 30.0)
                } else {
                    return false;
                };

                x >= expected_x_range.0 && x <= expected_x_range.1
            })
            .copied()
            .collect();

        if filtered_in_group.is_empty() {
            return None;
        }

        let addresses: Vec<String> = filtered_in_group.iter()
            .filter_map(|&idx| self.nodes.get(idx).map(|(addr, _)| addr.clone()))
            .collect();

        Some((filtered_in_group.len(), addresses))
    }

    /// Trace token path depth (inflows and outflows up to 5 hops)
    fn trace_token_path_depth(&self, from_idx: usize, to_idx: usize, token: &str) -> usize {
        use std::collections::{HashSet, VecDeque};

        let mut visited = HashSet::new();
        let mut max_depth = 0;

        // Trace backwards (inflows) up to 5 hops
        let mut queue = VecDeque::new();
        queue.push_back((from_idx, 0));
        visited.insert(from_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= 5 { continue; }
            max_depth = max_depth.max(depth);

            for (idx, (from, to, data)) in self.connections.iter().enumerate() {
                if *to == current && data.token == token && !visited.contains(from) {
                    visited.insert(*from);
                    queue.push_back((*from, depth + 1));
                }
            }
        }

        // Trace forwards (outflows) up to 5 hops
        visited.clear();
        queue.clear();
        queue.push_back((to_idx, 0));
        visited.insert(to_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= 5 { continue; }
            max_depth = max_depth.max(depth);

            for (idx, (from, to, data)) in self.connections.iter().enumerate() {
                if *from == current && data.token == token && !visited.contains(to) {
                    visited.insert(*to);
                    queue.push_back((*to, depth + 1));
                }
            }
        }

        max_depth
    }

    /// Count mixer nodes in the path between two nodes
    fn count_mixers_in_path(&self, from_idx: usize, to_idx: usize) -> usize {
        let mixers = self.detect_mixer_nodes();
        let mut count = 0;

        // Simple heuristic: count mixers that are connected to both nodes
        for mixer_idx in mixers {
            let has_from_connection = self.connections.iter()
                .any(|(f, t, _)| (*f == from_idx && *t == mixer_idx) || (*f == mixer_idx && *t == from_idx));
            let has_to_connection = self.connections.iter()
                .any(|(f, t, _)| (*f == to_idx && *t == mixer_idx) || (*f == mixer_idx && *t == to_idx));

            if has_from_connection || has_to_connection {
                count += 1;
            }
        }

        count
    }

    /// Calculate risk score based on mixer involvement and path complexity
    fn calculate_risk_score(&self, from_idx: usize, to_idx: usize) -> String {
        let mixer_count = self.count_mixers_in_path(from_idx, to_idx);
        let from_connections = self.connections.iter()
            .filter(|(f, _, _)| *f == from_idx).count();
        let to_connections = self.connections.iter()
            .filter(|(_, t, _)| *t == to_idx).count();

        let score = if mixer_count > 2 {
            "HIGH ⚠️"
        } else if mixer_count > 0 || (from_connections > 5 && to_connections > 5) {
            "MEDIUM ⚡"
        } else {
            "LOW ✓"
        };

        score.to_string()
    }

    /// Trace complete token flow path (inflows and outflows) for highlighting
    fn trace_token_flow_path(&self, from_idx: usize, to_idx: usize, token: &str, max_depth: usize) -> Vec<usize> {
        use std::collections::{HashSet, VecDeque};

        let mut path_edges = Vec::new();
        let mut visited_nodes = HashSet::new();
        let mixers = self.detect_mixer_nodes();

        // Trace backwards (inflows) from source
        let mut queue = VecDeque::new();
        queue.push_back((from_idx, 0));
        visited_nodes.insert(from_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= max_depth { continue; }

            for (edge_idx, (from, to, data)) in self.connections.iter().enumerate() {
                if *to == current && data.token == token && !visited_nodes.contains(from) {
                    path_edges.push(edge_idx);
                    visited_nodes.insert(*from);

                    // Skip expanding through mixers to reduce complexity
                    if !mixers.contains(from) {
                        queue.push_back((*from, depth + 1));
                    }
                }
            }
        }

        // Trace forwards (outflows) from destination
        visited_nodes.clear();
        queue.clear();
        queue.push_back((to_idx, 0));
        visited_nodes.insert(to_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= max_depth { continue; }

            for (edge_idx, (from, to, data)) in self.connections.iter().enumerate() {
                if *from == current && data.token == token && !visited_nodes.contains(to) {
                    path_edges.push(edge_idx);
                    visited_nodes.insert(*to);

                    // Skip expanding through mixers to reduce complexity
                    if !mixers.contains(to) {
                        queue.push_back((*to, depth + 1));
                    }
                }
            }
        }

        path_edges
    }

    /// Detect mixing/laundering patterns and return list of mixer node indices
    /// Pattern: high in-degree + high out-degree + rapid turnover
    fn detect_mixer_nodes(&self) -> Vec<usize> {
        let mut mixers = Vec::new();

        for (idx, _) in self.nodes.iter().enumerate() {
            let in_count = self.incoming_edges(idx).len();
            let out_count = self.outgoing_edges(idx).len();

            // Heuristic: Mixer if receives from 3+ sources AND sends to 3+ destinations
            // This catches split→shuffle→consolidate patterns
            if in_count >= 3 && out_count >= 3 {
                mixers.push(idx);
            }
        }

        mixers
    }

    /// Calculate mixer statistics (for collapsed visualization)
    fn get_mixer_stats(&self, node_idx: usize) -> MixerStats {
        let incoming = self.incoming_edges(node_idx);
        let outgoing = self.outgoing_edges(node_idx);

        let mut total_in = 0.0;
        let mut total_out = 0.0;
        let mut unique_tokens = std::collections::HashSet::new();

        for edge_idx in &incoming {
            if let Some((_, _, label)) = self.connections.get(*edge_idx) {
                total_in += label.amount;
                unique_tokens.insert(label.token.clone());
            }
        }

        for edge_idx in &outgoing {
            if let Some((_, _, label)) = self.connections.get(*edge_idx) {
                total_out += label.amount;
                unique_tokens.insert(label.token.clone());
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

    pub fn render(&mut self, f: &mut Frame, area: Rect) {
        use ratzilla::ratatui::widgets::{Paragraph, canvas::Canvas};
        use ratzilla::ratatui::text::{Line, Span};

        // Split area for trail if enabled
        let (graph_area, trail_area) = if self.show_trail && self.investigation_trail.is_some() {
            let chunks = ratzilla::ratatui::layout::Layout::default()
                .direction(ratzilla::ratatui::layout::Direction::Vertical)
                .constraints([
                    ratzilla::ratatui::layout::Constraint::Min(10),
                    ratzilla::ratatui::layout::Constraint::Length(5),  // Increased from 3 to 5 for better visibility
                ])
                .split(area);
            (chunks[0], Some(chunks[1]))
        } else {
            (area, None)
        };

        // Detect mixer nodes for visualization
        let mixer_nodes = self.detect_mixer_nodes();

        // Compute hierarchical layout on every render (cheap operation)
        if !self.nodes.is_empty() && !self.connections.is_empty() {
            self.compute_hierarchical_layout();
            self.update_node_filtering();  // Update filtering after layout
        }

        if self.nodes.is_empty() {
            let widget = Paragraph::new("  Waiting for transfer data...")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" Graph │ 0w 0tx ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Green)));
            f.render_widget(widget, graph_area);
            return;
        }

        // Render investigation trail if enabled
        if let Some(trail_rect) = trail_area {
            self.render_investigation_trail(f, trail_rect);
        }

        // Canvas-based graph for ANY size (scales to 10K+ nodes)
        let (cx, cy, zoom) = self.viewport;

        // Add trail indicator to title if trail is active
        let trail_indicator = if self.show_trail && self.investigation_trail.is_some() {
            if let Some(ref trail) = self.investigation_trail {
                format!(" 🔍 trail:{} ", trail.steps.len())
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        let canvas = Canvas::default()
            .block(Block::default()
                .title(format!(" Network Graph │ {}w {}tx │ depth:{}/{} │ zoom:{:.1}x │{}←↑↓→ pan, +/- zoom, [/] depth, T=trail ",
                    self.nodes.len(), self.connections.len(),
                    self.current_depth, self.max_depth, zoom, trail_indicator))
                .borders(Borders::ALL)
                .border_type(ratzilla::ratatui::widgets::BorderType::Rounded)
                .border_style(Style::default().fg(Color::Green)))
            .x_bounds([cx - 80.0 / zoom, cx + 80.0 / zoom])   // Narrower horizontal (nodes stack horizontally)
            .y_bounds([cy - 100.0 / zoom, cy + 100.0 / zoom]) // Taller vertical (flows top-to-bottom)
            .paint(|ctx| {
                // Collect selected edge info for path highlighting
                let selected_edge_idx = match &self.selection {
                    SelectionMode::Edge { edge_idx, .. } => Some(*edge_idx),
                    _ => None,
                };

                // When an edge is selected, trace token flows
                let highlighted_path = if let Some(edge_idx) = selected_edge_idx {
                    if let Some((from, to, data)) = self.connections.get(edge_idx) {
                        self.trace_token_flow_path(*from, *to, &data.token, 5)
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                // Draw ALL edges (even to off-screen nodes) with improved visual encoding
                // This includes edges where one or both endpoints are outside the visible viewport
                for (edge_idx, (from_idx, to_idx, edge_label)) in self.connections.iter().enumerate() {
                    // Only check if positions exist, NOT if nodes are visible/filtered
                    if *from_idx < self.node_positions.len() && *to_idx < self.node_positions.len() {
                        let (x1, y1) = self.node_positions[*from_idx];
                        let (x2, y2) = self.node_positions[*to_idx];

                        // Calculate edge metrics
                        let edge_length = ((x2 - x1).powi(2) + (y2 - y1).powi(2)).sqrt();
                        let amount = edge_label.amount;

                        // Determine edge color and thickness based on state and amount
                        let (edge_color, thickness) = if Some(edge_idx) == selected_edge_idx {
                            (Color::Yellow, 3)  // Selected edge - bright and thick
                        } else if highlighted_path.contains(&edge_idx) {
                            (Color::LightYellow, 2)  // Related flow - medium
                        } else {
                            // Distance-based fade for unselected edges
                            let dist_from_center = ((x1.powi(2) + y1.powi(2)).sqrt() +
                                                   (x2.powi(2) + y2.powi(2)).sqrt()) / 2.0;

                            // Color intensity based on transfer amount
                            let color = if amount > 1000.0 {
                                Color::Cyan  // Large transfer
                            } else if amount > 100.0 {
                                Color::Blue  // Medium transfer
                            } else if dist_from_center < 50.0 {
                                Color::LightBlue  // Close to center
                            } else {
                                Color::DarkGray  // Small/distant
                            };

                            // Thickness based on amount
                            let edge_thickness = if amount > 1000.0 { 2 } else { 1 };
                            (color, edge_thickness)
                        };

                        // Draw edge with appropriate thickness
                        for i in 0..thickness {
                            let offset = if thickness > 1 { (i as f64 - thickness as f64 / 2.0) * 0.3 } else { 0.0 };
                            let dx = (y2 - y1) / edge_length * offset;
                            let dy = (x1 - x2) / edge_length * offset;

                            ctx.draw(&CanvasLine {
                                x1: x1 + dx,
                                y1: y1 + dy,
                                x2: x2 + dx,
                                y2: y2 + dy,
                                color: edge_color,
                            });
                        }

                        // Add directional arrow for selected edge only
                        if Some(edge_idx) == selected_edge_idx {
                            let arrow_pos = 0.7; // Position along edge
                            let ax = x1 + (x2 - x1) * arrow_pos;
                            let ay = y1 + (y2 - y1) * arrow_pos;
                            let arrow_size = 2.0;

                            // Simple arrow using small lines
                            let dx = x2 - x1;
                            let dy = y2 - y1;
                            let norm = (dx.powi(2) + dy.powi(2)).sqrt();
                            if norm > 0.0 {
                                let ux = dx / norm;
                                let uy = dy / norm;

                                ctx.draw(&CanvasLine {
                                    x1: ax,
                                    y1: ay,
                                    x2: ax - ux * arrow_size + uy * arrow_size * 0.5,
                                    y2: ay - uy * arrow_size - ux * arrow_size * 0.5,
                                    color: Color::Yellow,
                                });
                                ctx.draw(&CanvasLine {
                                    x1: ax,
                                    y1: ay,
                                    x2: ax - ux * arrow_size - uy * arrow_size * 0.5,
                                    y2: ay - uy * arrow_size + ux * arrow_size * 0.5,
                                    color: Color::Yellow,
                                });
                            }
                        }

                        // NO TEXT LABELS ON CANVAS - all details shown in side panel
                    }
                }

                // Draw nodes with enhanced visual density
                for (idx, (_, node)) in self.nodes.iter().enumerate() {
                    // FILTER: Only render path-connected nodes
                    if !self.should_render_node(idx) {
                        continue;
                    }

                    // FILTER: Skip nodes with exactly 1 inflow OR 1 outflow (folded)
                    if self.filtered_nodes.contains(&idx) {
                        continue;
                    }

                    if idx < self.node_positions.len() {
                        let (x, y) = self.node_positions[idx];

                        // Check if this is a mixer node
                        let is_mixer = mixer_nodes.contains(&idx);
                        let is_selected = Some(idx) == self.selected_node();
                        let is_search_match = self.search_results.contains(&idx);

                        // Calculate node metrics for visual encoding
                        let in_degree = self.connections.iter()
                            .filter(|(_, to, _)| *to == idx).count();
                        let out_degree = self.connections.iter()
                            .filter(|(from, _, _)| *from == idx).count();
                        let total_degree = in_degree + out_degree;

                        // Size based on connection count (more connections = larger)
                        let base_radius = if idx == 0 { 5.0 } // Target largest
                            else if is_mixer { 4.5 }
                            else { 2.0 + (total_degree as f64).sqrt() * 0.5 };
                        let radius = base_radius.min(6.0); // Cap maximum size

                        // Color determination with cluster support
                        let color = if is_selected {
                            Color::White  // Selected node
                        } else if is_mixer {
                            Color::Red  // MIXER - highlight in RED
                        } else if is_search_match {
                            Color::Yellow  // Search match
                        } else if idx == 0 {
                            Color::Magenta  // Target wallet
                        } else if let Some((r, g, b)) = self.get_cluster_color(&self.nodes[idx].0) {
                            // Clustered wallet - use cluster color
                            Color::Rgb(r, g, b)
                        } else {
                            // Use color based on flow direction
                            if in_degree > out_degree {
                                Color::Green  // Net receiver
                            } else if out_degree > in_degree {
                                Color::Blue  // Net sender
                            } else {
                                Color::Cyan  // Balanced
                            }
                        };

                        // Draw main circle
                        ctx.draw(&Circle {
                            x, y,
                            radius,
                            color,
                        });

                        // Draw inner circle for visual depth
                        if radius > 2.5 {
                            ctx.draw(&Circle {
                                x, y,
                                radius: radius * 0.6,
                                color: if is_selected { Color::Yellow }
                                    else if is_mixer { Color::DarkGray }
                                    else { Color::Black },
                            });
                        }

                        // Add halo for selected/important nodes
                        if is_selected || idx == 0 {
                            ctx.draw(&Circle {
                                x, y,
                                radius: radius + 2.0,
                                color: if is_selected { Color::Yellow } else { Color::Magenta },
                            });
                        }

                        // Show abbreviated address (first 3 + last 3 chars)
                        let (addr, _) = &self.nodes[idx];
                        let short_addr = if addr.len() >= 6 {
                            format!("{}{}", &addr[..3], &addr[addr.len()-3..])
                        } else {
                            addr.clone()
                        };

                        // Always show abbreviated address for all nodes
                        if idx == 0 {
                            // Target wallet - show with special marker
                            ctx.print(x, y - radius - 2.0, format!("🎯{}", short_addr));
                        } else if is_mixer {
                            // Mixer - show with warning marker and degree
                            ctx.print(x, y - radius - 2.0, format!("⚠{}:{}↔{}", short_addr, in_degree, out_degree));
                        } else if is_selected {
                            // Selected - highlight
                            ctx.print(x, y - radius - 2.0, format!("▶{}", short_addr));
                        } else {
                            // Normal wallet - just address
                            ctx.print(x, y - radius - 2.0, short_addr);
                        }
                    }
                }

                // Draw folded circles for filtered nodes (grouped by row)
                // Group filtered nodes by their approximate row position (vertical layout)
                use std::collections::HashMap;
                let mut row_groups: HashMap<i32, Vec<usize>> = HashMap::new();

                for &filtered_idx in &self.filtered_nodes {
                    if filtered_idx >= self.node_positions.len() {
                        continue;
                    }

                    let (_, y) = self.node_positions[filtered_idx];
                    // Determine row based on Y position (now using Y for rows)
                    let row = (y / 60.0).round() as i32;
                    row_groups.entry(row).or_insert_with(Vec::new).push(filtered_idx);
                }

                // Draw one folded circle per row
                for (row, nodes_in_row) in row_groups.iter() {
                    if nodes_in_row.is_empty() {
                        continue;
                    }

                    let count = nodes_in_row.len();
                    // Calculate average X position for this group (horizontal now)
                    let avg_x: f64 = nodes_in_row.iter()
                        .filter_map(|&idx| self.node_positions.get(idx).map(|(x, _)| x))
                        .sum::<f64>() / count as f64;

                    let fold_x = avg_x;
                    let fold_y = *row as f64 * 60.0;

                    // Draw folded circle with count
                    let fold_radius = 3.5;

                    // Outer circle (gray, indicating folded/hidden nodes)
                    ctx.draw(&Circle {
                        x: fold_x,
                        y: fold_y,
                        radius: fold_radius,
                        color: Color::DarkGray,
                    });

                    // Inner circle for depth
                    ctx.draw(&Circle {
                        x: fold_x,
                        y: fold_y,
                        radius: fold_radius * 0.6,
                        color: Color::Black,
                    });

                    // Show count label
                    ctx.print(fold_x, fold_y - fold_radius - 1.5, format!("📦{}", count));
                }
            });

        f.render_widget(canvas, area);

        // Render search bar if active
        if self.search_active {
            use ratzilla::ratatui::widgets::Paragraph;
            let search_text = if self.search_results.is_empty() && !self.search_query.is_empty() {
                format!("Search: {} (no results)", self.search_query)
            } else if !self.search_results.is_empty() {
                format!("Search: {} ({}/{} results)", self.search_query, self.search_result_idx + 1, self.search_results.len())
            } else {
                format!("Search: {}_", self.search_query)
            };

            let search_area = Rect {
                x: area.x + 2,
                y: area.y + area.height - 3,
                width: area.width.saturating_sub(4),
                height: 3,
            };

            let search_widget = Paragraph::new(search_text)
                .block(Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Yellow))
                    .title(" Search (ESC to cancel, n/N for next/prev) "))
                .style(Style::default().fg(Color::White));

            f.render_widget(search_widget, search_area);
        }

        // Render toast notification if present
        if let Some(ref toast_msg) = self.toast_message {
            use ratzilla::ratatui::widgets::Paragraph;
            use ratzilla::ratatui::text::Span;

            let toast_area = Rect {
                x: area.x + (area.width.saturating_sub(toast_msg.len() as u16 + 4)) / 2,
                y: area.y + 2,
                width: (toast_msg.len() as u16 + 4).min(area.width),
                height: 3,
            };

            let toast_widget = Paragraph::new(vec![
                ratzilla::ratatui::text::Line::from(Span::styled(
                    toast_msg.as_str(),
                    Style::default().fg(Color::Black).bg(Color::Green).add_modifier(Modifier::BOLD)
                ))
            ])
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Green)));

            f.render_widget(toast_widget, toast_area);
        }
    }


    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn edge_count(&self) -> usize {
        self.connections.len()
    }

    /// Iterate over nodes (address, node_data)
    pub fn nodes_iter(&self) -> impl Iterator<Item = (&String, &WalletNode)> {
        self.nodes.iter().map(|(addr, node)| (addr, node))
    }

    // Helper to build graph from research agent data
    // ONLY shows direct SPL token transfers - filters out DeFi swaps/noise
    pub fn build_from_transfers(&mut self, transfers: &[TransferData]) {
        for transfer in transfers {
            // CRITICAL: Skip DeFi transactions to reduce graph noise
            // Only show wallet-to-wallet SPL token transfers
            if transfer.is_defi {
                continue;
            }

            let node_type_from = if transfer.from == self.target_wallet {
                WalletNodeType::Target
            } else {
                WalletNodeType::Funding
            };

            let node_type_to = if transfer.to == self.target_wallet {
                WalletNodeType::Target
            } else {
                WalletNodeType::Recipient
            };

            self.add_transfer(
                transfer.from.clone(),
                transfer.to.clone(),
                transfer.amount,
                transfer.token.clone(),
                node_type_from,
                node_type_to,
                transfer.timestamp.clone(),
                transfer.signature.clone(),
            );
        }
    }

    /// Get whale flows (transfers above threshold)
    pub fn get_whale_flows(&self, threshold: f64) -> Vec<&EdgeLabel> {
        self.connections.iter()
            .map(|(_, _, label)| label)
            .filter(|label| label.amount >= threshold)
            .collect()
    }

    /// Analyze wallet patterns - returns (sources, sinks, hubs)
    pub fn analyze_wallet_patterns(&self) -> (usize, usize, usize) {
        use std::collections::HashMap;

        let mut inflows: HashMap<usize, f64> = HashMap::new();
        let mut outflows: HashMap<usize, f64> = HashMap::new();

        for (from, to, label) in &self.connections {
            *outflows.entry(*from).or_insert(0.0) += label.amount;
            *inflows.entry(*to).or_insert(0.0) += label.amount;
        }

        let mut sources = 0;
        let mut sinks = 0;
        let mut hubs = 0;

        for idx in 0..self.nodes.len() {
            let in_amt = inflows.get(&idx).copied().unwrap_or(0.0);
            let out_amt = outflows.get(&idx).copied().unwrap_or(0.0);
            let total = in_amt + out_amt;

            if total > 0.0 {
                let out_ratio = out_amt / total;

                if out_ratio > 0.8 {
                    sources += 1;  // Mostly outflows
                } else if out_ratio < 0.2 {
                    sinks += 1;    // Mostly inflows
                } else if total > 100.0 {
                    hubs += 1;     // High throughput
                }
            }
        }

        (sources, sinks, hubs)
    }

    /// Get unique tokens in the graph
    pub fn get_unique_tokens(&self) -> Vec<String> {
        use std::collections::HashSet;

        let mut tokens: HashSet<String> = HashSet::new();
        for (_, _, label) in &self.connections {
            tokens.insert(label.token.clone());
        }
        tokens.into_iter().collect()
    }

    /// Calculate network risk score based on structure analysis
    pub fn calculate_network_risk_score(&self) -> f64 {
        let mut risk = 0.0;

        // High complexity adds risk
        let complexity_ratio = self.connections.len() as f64 / self.nodes.len().max(1) as f64;
        if complexity_ratio > 5.0 {
            risk += 30.0;
        } else if complexity_ratio > 3.0 {
            risk += 15.0;
        }

        // Whale activity adds risk
        let whale_count = self.get_whale_flows(100.0).len();
        risk += (whale_count as f64 * 5.0).min(25.0);

        // High token diversity might indicate mixing
        let token_count = self.get_unique_tokens().len();
        if token_count > 10 {
            risk += 20.0;
        } else if token_count > 5 {
            risk += 10.0;
        }

        // Hub wallets can indicate coordination
        let (_, _, hubs) = self.analyze_wallet_patterns();
        risk += (hubs as f64 * 3.0).min(15.0);

        // Mixer nodes are high risk
        let mixer_count = self.nodes.iter()
            .filter(|(_, node)| matches!(node.node_type, WalletNodeType::Mixer))
            .count();
        risk += (mixer_count as f64 * 10.0).min(20.0);

        risk.min(100.0)
    }

    /// Detect rapid transfer patterns (velocity analysis)
    pub fn detect_rapid_transfers(&self) -> Vec<RapidFlowAlert> {
        self.detect_rapid_transfers_with_config(&ForensicsConfig::load().unwrap_or_default())
    }

    /// Detect rapid transfer patterns with custom configuration
    pub fn detect_rapid_transfers_with_config(&self, config: &ForensicsConfig) -> Vec<RapidFlowAlert> {
        use std::collections::HashMap;

        let mut alerts = Vec::new();

        // Parse timestamps and group by time windows
        let mut time_buckets: HashMap<String, Vec<(u64, &EdgeLabel)>> = HashMap::new();

        for (_, _, label) in &self.connections {
            if let Some(timestamp_str) = &label.timestamp {
                // Try to parse ISO 8601 or Unix timestamp
                if let Ok(unix_time) = timestamp_str.parse::<u64>() {
                    let bucket_key = format!("{}_{}", label.token, unix_time / 3600); // Hour buckets
                    time_buckets.entry(bucket_key).or_insert_with(Vec::new).push((unix_time, label));
                }
            }
        }

        // Analyze each bucket for rapid activity
        for (_, transfers) in time_buckets {
            if transfers.len() < 2 {
                continue;
            }

            let mut sorted_transfers = transfers;
            sorted_transfers.sort_by_key(|(time, _)| *time);

            // Sliding window: check for bursts
            for window_size in &config.thresholds.time_windows {
                let mut i = 0;
                while i < sorted_transfers.len() {
                    let start_time = sorted_transfers[i].0;
                    let mut j = i;
                    let mut count = 0;
                    let mut total_volume = 0.0;

                    while j < sorted_transfers.len() && sorted_transfers[j].0 - start_time <= *window_size {
                        count += 1;
                        total_volume += sorted_transfers[j].1.amount;
                        j += 1;
                    }

                    // Determine severity using configured thresholds
                    let txns_per_min = count as f64 / (*window_size as f64 / 60.0);
                    let vol_per_min = total_volume / (*window_size as f64 / 60.0);

                    let severity = if txns_per_min > config.thresholds.rapid_txns_critical || vol_per_min > config.thresholds.rapid_volume_critical {
                        AlertSeverity::Critical
                    } else if txns_per_min > config.thresholds.rapid_txns_critical / 2.0 || vol_per_min > config.thresholds.rapid_volume_critical / 2.0 {
                        AlertSeverity::High
                    } else if txns_per_min > config.thresholds.rapid_txns_critical / 4.0 || vol_per_min > config.thresholds.rapid_volume_critical / 4.0 {
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
                            token: sorted_transfers[i].1.token.clone(),
                            severity,
                        });
                    }

                    i += 1;
                }
            }
        }

        alerts
    }

    /// Detect circular flow patterns (A→B→C→A)
    pub fn detect_circular_flows(&self) -> Vec<CircularFlow> {
        use std::collections::{HashMap, HashSet};

        let mut cycles = Vec::new();

        // Build adjacency list
        let mut adj: HashMap<usize, Vec<(usize, &EdgeLabel)>> = HashMap::new();
        for (from, to, label) in &self.connections {
            adj.entry(*from).or_insert_with(Vec::new).push((*to, label));
        }

        // DFS to find cycles (limit depth to 5 for performance)
        let mut visited = HashSet::new();
        let mut path = Vec::new();

        for start_idx in 0..self.nodes.len() {
            self.dfs_find_cycles(start_idx, start_idx, &adj, &mut visited, &mut path, &mut cycles, 0, 5);
        }

        cycles
    }

    fn dfs_find_cycles(
        &self,
        start: usize,
        current: usize,
        adj: &std::collections::HashMap<usize, Vec<(usize, &EdgeLabel)>>,
        visited: &mut std::collections::HashSet<usize>,
        path: &mut Vec<(usize, String, f64)>,
        cycles: &mut Vec<CircularFlow>,
        depth: usize,
        max_depth: usize,
    ) {
        if depth > max_depth {
            return;
        }

        if visited.contains(&current) {
            // Check if we've returned to start
            if current == start && path.len() >= 3 {
                let mut cycle_path = Vec::new();
                let mut total_amount = 0.0;
                let token = path[0].1.clone();
                let cycle_length = path.len();

                for (idx, _token, amount) in path.iter() {
                    cycle_path.push(self.nodes[*idx].0.clone());
                    total_amount += *amount;
                }

                cycles.push(CircularFlow {
                    path: cycle_path,
                    total_amount,
                    token,
                    cycle_length,
                });
            }
            return;
        }

        visited.insert(current);

        if let Some(neighbors) = adj.get(&current) {
            for (next, label) in neighbors {
                path.push((current, label.token.clone(), label.amount));
                self.dfs_find_cycles(start, *next, adj, visited, path, cycles, depth + 1, max_depth);
                path.pop();
            }
        }

        visited.remove(&current);
    }

    /// Classify wallet behavior based on transaction patterns
    pub fn classify_wallet_behavior(&self, wallet_idx: usize) -> WalletBehaviorType {
        use std::collections::HashMap;

        // Count incoming and outgoing edges
        let mut incoming = 0;
        let mut outgoing = 0;
        let mut total_in = 0.0;
        let mut total_out = 0.0;
        let mut counterparties: HashMap<usize, usize> = HashMap::new();
        let mut timestamps = Vec::new();

        for (from, to, label) in &self.connections {
            if *from == wallet_idx {
                outgoing += 1;
                total_out += label.amount;
                *counterparties.entry(*to).or_insert(0) += 1;
                if let Some(ts) = &label.timestamp {
                    if let Ok(unix) = ts.parse::<u64>() {
                        timestamps.push(unix);
                    }
                }
            }
            if *to == wallet_idx {
                incoming += 1;
                total_in += label.amount;
                *counterparties.entry(*from).or_insert(0) += 1;
                if let Some(ts) = &label.timestamp {
                    if let Ok(unix) = ts.parse::<u64>() {
                        timestamps.push(unix);
                    }
                }
            }
        }

        let total_txns = incoming + outgoing;
        let total_volume = total_in + total_out;

        // Dormant check
        if total_txns < 5 {
            return WalletBehaviorType::Dormant;
        }

        // Exchange characteristics: very high volume, many unique counterparties
        if total_volume > 10000.0 && counterparties.len() > 50 {
            return WalletBehaviorType::Exchange;
        }

        // Mixer characteristics: many small inputs/outputs, high counterparty diversity
        let avg_amount = total_volume / total_txns as f64;
        if avg_amount < 1.0 && counterparties.len() > 20 {
            return WalletBehaviorType::Mixer;
        }

        // Bot characteristics: regular timing intervals
        if timestamps.len() > 10 {
            timestamps.sort();
            let mut intervals = Vec::new();
            for i in 1..timestamps.len() {
                intervals.push(timestamps[i] - timestamps[i-1]);
            }
            let avg_interval = intervals.iter().sum::<u64>() as f64 / intervals.len() as f64;
            let variance: f64 = intervals.iter()
                .map(|&x| (x as f64 - avg_interval).powi(2))
                .sum::<f64>() / intervals.len() as f64;
            let std_dev = variance.sqrt();

            // Low variance = regular intervals = bot
            if std_dev < avg_interval * 0.2 && total_txns > 20 {
                return WalletBehaviorType::Bot;
            }
        }

        // Trader characteristics: moderate volume, DEX interactions
        if total_volume > 100.0 && total_volume < 10000.0 {
            return WalletBehaviorType::Trader;
        }

        // Contract check: check node type
        if let Some((_, node)) = self.nodes.get(wallet_idx) {
            if matches!(node.node_type, WalletNodeType::DeFi | WalletNodeType::Token) {
                return WalletBehaviorType::Contract;
            }
        }

        WalletBehaviorType::EOA
    }

    /// Calculate explainable risk score with detailed reasoning
    pub fn calculate_explainable_risk(&self) -> RiskExplanation {
        let mut score: f64 = 0.0;
        let mut reasons = Vec::new();
        let mut alerts = Vec::new();

        // 1. Network complexity analysis
        let complexity_ratio = self.connections.len() as f64 / self.nodes.len().max(1) as f64;
        if complexity_ratio > 5.0 {
            score += 30.0;
            alerts.push(format!("🚨 CRITICAL: Very high network complexity ({:.1} edges/node)", complexity_ratio));
            reasons.push(format!("Network complexity ratio of {:.1} indicates potential mixing/obfuscation", complexity_ratio));
        } else if complexity_ratio > 3.0 {
            score += 15.0;
            reasons.push(format!("Elevated network complexity ({:.1} edges/node)", complexity_ratio));
        }

        // 2. Rapid transfer detection
        let rapid_flows = self.detect_rapid_transfers();
        for alert in rapid_flows.iter().filter(|a| matches!(a.severity, AlertSeverity::Critical | AlertSeverity::High)) {
            score += 15.0;
            alerts.push(format!(
                "⚡ RAPID ACTIVITY: {} transfers in {}s ({:.2} {})",
                alert.transfer_count,
                alert.time_window_secs,
                alert.total_volume,
                alert.token
            ));
        }
        if !rapid_flows.is_empty() {
            reasons.push(format!("Detected {} rapid transfer burst(s)", rapid_flows.len()));
        }

        // 3. Circular flow detection
        let circular_flows = self.detect_circular_flows();
        for cycle in circular_flows.iter().take(3) {
            score += 20.0;
            alerts.push(format!(
                "🔄 CIRCULAR FLOW: {} wallets, {:.2} {} total",
                cycle.cycle_length,
                cycle.total_amount,
                cycle.token
            ));
        }
        if !circular_flows.is_empty() {
            reasons.push(format!("Detected {} circular flow pattern(s) - potential wash trading", circular_flows.len()));
        }

        // 4. Whale activity
        let whale_flows = self.get_whale_flows(100.0);
        if whale_flows.len() > 5 {
            score += 15.0;
            let total_whale: f64 = whale_flows.iter().map(|e| e.amount).sum();
            reasons.push(format!("High whale activity: {} large transfers totaling {:.2} SOL", whale_flows.len(), total_whale));
        }

        // 5. Wallet behavior analysis
        let target_behavior = self.classify_wallet_behavior(0);
        match target_behavior {
            WalletBehaviorType::Mixer => {
                score += 25.0;
                alerts.push("🔴 MIXER DETECTED: Wallet exhibits mixing/tumbling behavior".to_string());
            }
            WalletBehaviorType::Bot => {
                score += 10.0;
                reasons.push("Programmatic bot activity detected (regular intervals)".to_string());
            }
            WalletBehaviorType::Exchange => {
                score -= 5.0; // Exchanges are high-volume but legitimate
                reasons.push("Exchange-like behavior (high volume, many counterparties)".to_string());
            }
            _ => {}
        }

        // 6. Token diversity
        let tokens = self.get_unique_tokens();
        if tokens.len() > 20 {
            score += 10.0;
            reasons.push(format!("Very high token diversity ({} tokens) may indicate portfolio mixing", tokens.len()));
        }

        // 7. Hub wallet detection
        let (sources, sinks, hubs) = self.analyze_wallet_patterns();
        if hubs > 3 {
            score += 10.0;
            reasons.push(format!("{} hub wallets detected - potential coordination point", hubs));
        }

        // Cap score and determine level
        score = score.min(100.0_f64).max(0.0_f64);
        let level = if score >= 75.0 {
            RiskLevel::Critical
        } else if score >= 50.0 {
            RiskLevel::High
        } else if score >= 25.0 {
            RiskLevel::Medium
        } else {
            RiskLevel::Low
        };

        RiskExplanation {
            score,
            level,
            reasons,
            alerts,
        }
    }

    /// Detect entity clusters in the wallet graph
    pub fn detect_entity_clusters(&mut self) {
        // Use local stub types (WalletMetadata -> ClusterWalletMetadata, etc.)
        type WalletMetadata = ClusterWalletMetadata;
        type TransferMetadata = ClusterTransferMetadata;

        // Build metadata from graph
        let wallets: Vec<(String, WalletMetadata)> = self.nodes.iter().map(|(addr, node)| {
            (addr.clone(), WalletMetadata {
                address: addr.clone(),
                first_seen: None, // TODO: extract from timestamps
                transaction_count: self.connections.iter().filter(|(from, to, _)| {
                    self.nodes[*from].0 == *addr || self.nodes[*to].0 == *addr
                }).count(),
                behavior_type: Some(format!("{:?}", node.node_type)),
            })
        }).collect();

        let connections: Vec<(usize, usize, TransferMetadata)> = self.connections.iter().map(|(from, to, label)| {
            let timestamp = label.timestamp.as_ref()
                .and_then(|ts| ts.parse::<u64>().ok());

            // Heuristic for initial funding: first transfer or large amount
            let is_initial_funding = label.amount > 1.0; // >1 SOL

            (*from, *to, TransferMetadata {
                amount: label.amount,
                token: label.token.clone(),
                timestamp,
                is_initial_funding,
                gas_price: None,
            })
        }).collect();

        // Run clustering
        let clusterer = EntityClusterer::default();
        self.entity_clusters = clusterer.detect_clusters(&wallets, &connections);

        // Build wallet-to-cluster mapping
        self.wallet_to_cluster.clear();
        for cluster in &self.entity_clusters {
            for wallet in &cluster.wallet_addresses {
                self.wallet_to_cluster.insert(wallet.clone(), cluster.cluster_id);
            }
        }
    }

    /// Get cluster info for a wallet
    pub fn get_wallet_cluster(&self, wallet: &str) -> Option<&EntityCluster> {
        self.wallet_to_cluster.get(wallet)
            .and_then(|cluster_id| self.entity_clusters.get(*cluster_id))
    }

    /// Get cluster color for visualization
    pub fn get_cluster_color(&self, wallet: &str) -> Option<(u8, u8, u8)> {
        self.wallet_to_cluster.get(wallet)
            .map(|cluster_id| EntityClusterer::get_cluster_color(*cluster_id))
    }

    /// Calculate risk level for a specific node (simplified for trail)
    pub fn calculate_node_risk_level(&self, node_idx: usize) -> RiskLevel {
        if node_idx >= self.nodes.len() {
            return RiskLevel::Low;
        }

        let node = &self.nodes[node_idx].1;

        // Quick heuristic-based risk assessment
        match node.node_type {
            WalletNodeType::Mixer => RiskLevel::Critical,
            WalletNodeType::Target => RiskLevel::Low, // Target itself is neutral
            _ => {
                // Check transfer patterns
                let in_degree = self.connections.iter().filter(|(_, to, _)| *to == node_idx).count();
                let out_degree = self.connections.iter().filter(|(from, _, _)| *from == node_idx).count();
                let total_degree = in_degree + out_degree;

                // Check total volume
                let total_volume: f64 = self.connections.iter()
                    .filter(|(from, to, _)| *from == node_idx || *to == node_idx)
                    .map(|(_, _, label)| label.amount)
                    .sum();

                // Risk based on activity level and volume
                if total_volume > 1000.0 || total_degree > 20 {
                    RiskLevel::High
                } else if total_volume > 100.0 || total_degree > 10 {
                    RiskLevel::Medium
                } else {
                    RiskLevel::Low
                }
            }
        }
    }

    /// Update investigation trail when node is selected
    pub fn update_trail_on_selection(&mut self, new_node_idx: usize) {
        // Check if we should add to trail
        let should_add = if let Some(ref trail) = self.investigation_trail {
            trail.current_step().node_index != new_node_idx && new_node_idx < self.nodes.len()
        } else {
            false
        };

        if should_add {
            // Calculate values before mutable borrow
            let address = self.nodes[new_node_idx].0.clone();
            let risk_level = self.calculate_node_risk_level(new_node_idx);

            // Generate contextual note
            let note = if matches!(self.nodes[new_node_idx].1.node_type, WalletNodeType::Mixer) {
                Some("Mixer detected".to_string())
            } else if let Some(cluster) = self.get_wallet_cluster(&address) {
                Some(format!("Cluster #{} ({} wallets)", cluster.cluster_id, cluster.wallet_addresses.len()))
            } else {
                None
            };

            // Now add to trail
            if let Some(ref mut trail) = self.investigation_trail {
                trail.add_step(new_node_idx, address, risk_level, note);
            }
        }
    }

    /// Render investigation trail breadcrumb bar
    fn render_investigation_trail(&self, f: &mut Frame, area: Rect) {
        use ratzilla::ratatui::text::{Line, Span};
        use ratzilla::ratatui::widgets::{Block, Borders, Paragraph, Wrap};

        let Some(ref trail) = self.investigation_trail else {
            return;
        };

        let block = Block::default()
            .title(format!(" 🔍 Trail (step {}/{}) ", trail.current_index + 1, trail.steps.len()))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Blue));

        let inner = block.inner(area);
        f.render_widget(block, area);

        // Smart truncation: show only last N steps that fit in available width
        // Each step is ~15 chars (icon + 4...3 address + arrow)
        // Leave 10 char safety margin for emoji width variance
        let available_width = (inner.width as usize).saturating_sub(10);
        let chars_per_step = 15;
        let max_visible_steps = (available_width / chars_per_step).max(2).min(20); // Min 2, max 20

        // Build breadcrumb line
        let mut spans = Vec::new();

        // Show ellipsis if we're hiding steps
        let start_idx = if trail.steps.len() > max_visible_steps {
            spans.push(Span::styled("... → ", Style::default().fg(Color::DarkGray)));
            trail.steps.len() - max_visible_steps
        } else {
            0
        };

        for (i, step) in trail.steps.iter().enumerate().skip(start_idx) {
            let is_current = i == trail.current_index;

            // Risk icon
            let icon = match step.risk_level {
                RiskLevel::Critical => "🔴",
                RiskLevel::High => "🟠",
                RiskLevel::Medium => "🟡",
                RiskLevel::Low => "🟢",
            };

            // Abbreviated address
            let addr = if step.wallet_address.len() > 10 {
                format!("{}...{}", &step.wallet_address[..4], &step.wallet_address[step.wallet_address.len().saturating_sub(3)..])
            } else {
                step.wallet_address.clone()
            };

            // Style
            let style = if is_current {
                Style::default().fg(Color::White).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::DarkGray)
            };

            spans.push(Span::styled(format!("{} {}", icon, addr), style));

            // Arrow separator
            if i < trail.steps.len() - 1 {
                spans.push(Span::styled(" → ", Style::default().fg(Color::DarkGray)));
            }
        }

        let line = Line::from(spans);
        let paragraph = Paragraph::new(line).wrap(Wrap { trim: true });

        f.render_widget(paragraph, inner);
    }

}

// Data structure for transfer information
#[derive(Debug, Clone)]
pub struct TransferData {
    pub from: String,
    pub to: String,
    pub amount: f64,
    pub token: String,
    pub is_defi: bool,
    pub timestamp: Option<String>,
    pub signature: Option<String>,  // Transaction signature
}

impl TransferData {
    pub fn from_json(data: &serde_json::Value) -> Vec<Self> {
        let mut transfers = Vec::new();

        if let Some(data_array) = data.get("data").and_then(|d| d.get("data")).and_then(|d| d.as_array()) {
            for item in data_array {
                if let (Some(from), Some(to), Some(token_amount), Some(token_symbol)) = (
                    item.get("from").and_then(|v| v.as_str()),
                    item.get("to").and_then(|v| v.as_str()),
                    item.get("tokenAmount").and_then(|v| v.as_str()),
                    item.get("tokenSymbol").and_then(|v| v.as_str()),
                ) {
                    let amount: f64 = token_amount.parse().unwrap_or(0.0);
                    let is_defi = item.get("txType")
                        .and_then(|v| v.as_str())
                        .map(|t| t == "defi")
                        .unwrap_or(false);

                    let timestamp = item.get("date")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());

                    let signature = item.get("signature")
                        .or_else(|| item.get("txHash"))
                        .or_else(|| item.get("transactionHash"))
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());

                    transfers.push(TransferData {
                        from: from.to_string(),
                        to: to.to_string(),
                        amount,
                        token: token_symbol.to_string(),
                        is_defi,
                        timestamp,
                        signature,
                    });
                }
            }
        }

        transfers
    }
}
