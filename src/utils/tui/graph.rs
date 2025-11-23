use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Widget, Wrap, canvas::{Canvas, Points, Line as CanvasLine, Circle, Rectangle}},
    Frame,
};
use std::collections::HashMap;
use arboard::Clipboard;

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
            target_wallet,
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
                            self.selection = SelectionMode::Node(node_idx - 1);
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
                            self.selection = SelectionMode::Node(node_idx + 1);
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
                        self.selection = SelectionMode::Node(*from_node);
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
                        self.selection = SelectionMode::Node(*to_node);
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
            // Copy to clipboard
            GraphInput::Copy => {
                if let Some(idx) = self.selected_node() {
                    if let Some((addr, _)) = self.nodes.get(idx) {
                        match Clipboard::new().and_then(|mut clip| clip.set_text(addr.to_string())) {
                            Ok(_) => {
                                self.show_toast(format!("Copied: {}...{}", &addr[..8], &addr[addr.len()-8..]));
                            }
                            Err(e) => {
                                self.show_toast(format!("Copy failed: {}", e));
                            }
                        }
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

    /// Get info about selected node or edge
    pub fn get_selected_info(&self) -> Option<String> {
        match &self.selection {
            SelectionMode::Node(idx) => {
                self.nodes.get(*idx).map(|(addr, node)| {
                    let outgoing = self.get_outgoing(*idx);
                    let incoming: Vec<_> = self.connections
                        .iter()
                        .filter(|(_, to, _)| *to == *idx)
                        .collect();
                    format!(
                        "{} {}\nAddress: {}\nIn: {} | Out: {}{}",
                        node.node_type.symbol(),
                        node.label,
                        addr,
                        incoming.len(),
                        outgoing.len(),
                        if self.is_collapsed(*idx) { " [▶]" } else { "" }
                    )
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

        // Column spacing
        let column_width = 60.0;
        let node_spacing = 18.0; // Vertical padding between nodes

        // Position nodes in columns
        for ((depth, flow), mut group) in columns.into_iter() {
            if depth == 0 { continue; } // Skip center

            // Sort nodes by total transfer amount (descending)
            group.nodes.sort_by(|a, b| {
                let amount_a = group.total_amount.get(a).copied().unwrap_or(0.0);
                let amount_b = group.total_amount.get(b).copied().unwrap_or(0.0);
                amount_b.partial_cmp(&amount_a).unwrap()
            });

            // Calculate X position (column)
            let x = flow as f64 * column_width * depth as f64;

            // Calculate Y positions (stack vertically with padding)
            let total_height = group.nodes.len() as f64 * node_spacing;
            let start_y = -total_height / 2.0;

            for (i, &node_idx) in group.nodes.iter().enumerate() {
                let y = start_y + i as f64 * node_spacing;

                if node_idx < self.node_positions.len() {
                    self.node_positions[node_idx] = (x, y);
                }
            }
        }

        // Handle disconnected nodes (place at far edges)
        let mut edge_count = 0;
        for i in 0..self.nodes.len() {
            if !distances.contains_key(&i) {
                let x = if edge_count % 2 == 0 { -200.0 } else { 200.0 };
                let y = (edge_count / 2) as f64 * node_spacing;
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
        // DISABLE filtering if no connections yet (show everything during graph build)
        if self.connections.is_empty() {
            return true;
        }

        // Otherwise, only show path-connected nodes
        self.connected_nodes.contains(&node_idx)
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
        use ratatui::widgets::{Paragraph, canvas::Canvas};
        use ratatui::text::{Line, Span};

        // Detect mixer nodes for visualization
        let mixer_nodes = self.detect_mixer_nodes();

        // Compute hierarchical layout on every render (cheap operation)
        if !self.nodes.is_empty() && !self.connections.is_empty() {
            self.compute_hierarchical_layout();
        }

        if self.nodes.is_empty() {
            let widget = Paragraph::new("  Waiting for transfer data...")
                .style(Style::default().fg(Color::DarkGray))
                .block(Block::default()
                    .title(" Graph │ 0w 0tx ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Green)));
            f.render_widget(widget, area);
            return;
        }

        // Canvas-based graph for ANY size (scales to 10K+ nodes)
        let (cx, cy, zoom) = self.viewport;
        let canvas = Canvas::default()
            .block(Block::default()
                .title(format!(" Network Graph │ {}w {}tx │ depth:{}/{} │ zoom:{:.1}x │ ←↑↓→ pan, +/- zoom, [/] depth ",
                    self.nodes.len(), self.connections.len(),
                    self.current_depth, self.max_depth, zoom))
                .borders(Borders::ALL)
                .border_type(ratatui::widgets::BorderType::Rounded)
                .border_style(Style::default().fg(Color::Green)))
            .x_bounds([cx - 100.0 / zoom, cx + 100.0 / zoom])
            .y_bounds([cy - 50.0 / zoom, cy + 50.0 / zoom])
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

                        // Color intensity based on activity
                        let color = if is_selected {
                            Color::White  // Selected node
                        } else if is_mixer {
                            Color::Red  // MIXER - highlight in RED
                        } else if is_search_match {
                            Color::Yellow  // Search match
                        } else if idx == 0 {
                            Color::Magenta  // Target wallet
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
            });

        f.render_widget(canvas, area);

        // Render search bar if active
        if self.search_active {
            use ratatui::widgets::Paragraph;
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
            use ratatui::widgets::Paragraph;
            use ratatui::text::Span;

            let toast_area = Rect {
                x: area.x + (area.width.saturating_sub(toast_msg.len() as u16 + 4)) / 2,
                y: area.y + 2,
                width: (toast_msg.len() as u16 + 4).min(area.width),
                height: 3,
            };

            let toast_widget = Paragraph::new(vec![
                ratatui::text::Line::from(Span::styled(
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
