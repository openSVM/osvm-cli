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
}

impl WalletNodeType {
    pub fn color(&self) -> Color {
        match self {
            WalletNodeType::Target => Color::Red,
            WalletNodeType::Funding => Color::Green,
            WalletNodeType::Recipient => Color::Blue,
            WalletNodeType::DeFi => Color::Magenta,
            WalletNodeType::Token => Color::Yellow,
        }
    }

    pub fn symbol(&self) -> &str {
        match self {
            WalletNodeType::Target => "🔴",
            WalletNodeType::Funding => "🟢",
            WalletNodeType::Recipient => "🔵",
            WalletNodeType::DeFi => "🟣",
            WalletNodeType::Token => "🟡",
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

pub struct WalletGraph {
    nodes: Vec<(String, WalletNode)>, // (address, node_data)
    connections: Vec<(usize, usize, EdgeLabel)>, // (from_idx, to_idx, edge_data)
    target_wallet: String,
    /// Currently selected node index for keyboard navigation
    pub selected_node: Option<usize>,
    /// Collapsed node indices (hidden children)
    pub collapsed_nodes: std::collections::HashSet<usize>,
    /// Node positions for canvas rendering (x, y) in graph space
    pub node_positions: Vec<(f64, f64)>,
    /// Viewport for large graphs (center_x, center_y, zoom_level)
    pub viewport: (f64, f64, f64),
    /// Max depth for BFS exploration (default: 5)
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

        Self {
            nodes,
            connections: Vec::new(),
            target_wallet,
            selected_node: Some(0),
            collapsed_nodes: std::collections::HashSet::new(),
            node_positions: vec![(0.0, 0.0)], // Target wallet at origin
            viewport: (0.0, 0.0, 1.0), // (center_x, center_y, zoom=1.0)
            max_depth: 5, // Default BFS depth
            current_depth: 0,
            search_query: String::new(),
            search_active: false,
            search_results: Vec::new(),
            search_result_idx: 0,
            toast_message: None,
            toast_timer: 0,
        }
    }

    /// Handle keyboard input for graph navigation
    pub fn handle_input(&mut self, input: GraphInput) -> Option<String> {
        match input {
            // Node selection navigation
            GraphInput::Up => {
                if let Some(idx) = self.selected_node {
                    if idx > 0 {
                        self.selected_node = Some(idx - 1);
                    }
                } else if !self.nodes.is_empty() {
                    self.selected_node = Some(0);
                }
                None
            }
            GraphInput::Down => {
                if let Some(idx) = self.selected_node {
                    if idx + 1 < self.nodes.len() {
                        self.selected_node = Some(idx + 1);
                    }
                } else if !self.nodes.is_empty() {
                    self.selected_node = Some(0);
                }
                None
            }
            GraphInput::Left => {
                if let Some(idx) = self.selected_node {
                    if idx > 0 {
                        self.selected_node = Some(idx - 1);
                    }
                }
                None
            }
            GraphInput::Right => {
                if let Some(idx) = self.selected_node {
                    if idx + 1 < self.nodes.len() {
                        self.selected_node = Some(idx + 1);
                    }
                }
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
                if let Some(idx) = self.selected_node {
                    if self.collapsed_nodes.contains(&idx) {
                        self.collapsed_nodes.remove(&idx);
                    } else {
                        self.collapsed_nodes.insert(idx);
                    }
                }
                None
            }
            GraphInput::Select => {
                self.selected_node
                    .and_then(|idx| self.nodes.get(idx))
                    .map(|(addr, _)| addr.clone())
            }
            GraphInput::Escape => {
                self.selected_node = None;
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
                    self.selected_node = Some(self.search_results[self.search_result_idx]);
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
                    self.selected_node = Some(self.search_results[self.search_result_idx]);
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
                if let Some(idx) = self.selected_node {
                    if let Some((addr, _)) = self.nodes.get(idx) {
                        match Clipboard::new().and_then(|mut clip| clip.set_text(addr.clone())) {
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
                if let Some(idx) = self.selected_node {
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
            self.selected_node = Some(self.search_results[0]);
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
        self.selected_node == Some(node_idx)
    }

    /// Get info about selected node
    pub fn get_selected_info(&self) -> Option<String> {
        self.selected_node.and_then(|idx| {
            self.nodes.get(idx).map(|(addr, node)| {
                let outgoing = self.get_outgoing(idx);
                let incoming: Vec<_> = self.connections
                    .iter()
                    .filter(|(_, to, _)| *to == idx)
                    .collect();
                format!(
                    "{} {}\nAddress: {}\nIn: {} | Out: {}{}",
                    node.node_type.symbol(),
                    node.label,
                    addr,
                    incoming.len(),
                    outgoing.len(),
                    if self.is_collapsed(idx) { " [▶]" } else { "" }
                )
            })
        })
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

        // Add initial random position for new node
        let idx = self.nodes.len() - 1;
        let angle = (idx as f64) * 2.0 * std::f64::consts::PI / 20.0;
        let radius = 10.0 + (idx as f64) * 2.0;
        self.node_positions.push((
            radius * angle.cos(),
            radius * angle.sin(),
        ));

        idx
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
    pub fn compute_hierarchical_layout(&mut self) {
        if self.nodes.is_empty() {
            return;
        }

        // Find target wallet index (should be index 0)
        let target_idx = 0;

        // Categorize nodes into inflows (send TO target) and outflows (receive FROM target)
        let mut inflow_nodes = Vec::new();
        let mut outflow_nodes = Vec::new();
        let mut other_nodes = Vec::new();

        for (idx, (addr, _)) in self.nodes.iter().enumerate() {
            if idx == target_idx {
                continue; // Skip target itself
            }

            // Check if this node sends to target (inflow)
            let sends_to_target = self.connections.iter()
                .any(|(from, to, _)| *from == idx && *to == target_idx);

            // Check if this node receives from target (outflow)
            let receives_from_target = self.connections.iter()
                .any(|(from, to, _)| *from == target_idx && *to == idx);

            if sends_to_target {
                inflow_nodes.push(idx);
            } else if receives_from_target {
                outflow_nodes.push(idx);
            } else {
                other_nodes.push(idx);
            }
        }

        // Clear and rebuild positions
        self.node_positions.clear();
        self.node_positions.resize(self.nodes.len(), (0.0, 0.0));

        // Position target at center (0, 0)
        self.node_positions[target_idx] = (0.0, 0.0);

        // Position inflows on the LEFT (negative X)
        let inflow_count = inflow_nodes.len();
        for (i, &idx) in inflow_nodes.iter().enumerate() {
            let y_offset = (i as f64 - inflow_count as f64 / 2.0) * 15.0;
            self.node_positions[idx] = (-50.0, y_offset);
        }

        // Position outflows on the RIGHT (positive X)
        let outflow_count = outflow_nodes.len();
        for (i, &idx) in outflow_nodes.iter().enumerate() {
            let y_offset = (i as f64 - outflow_count as f64 / 2.0) * 15.0;
            self.node_positions[idx] = (50.0, y_offset);
        }

        // Position other nodes (secondary connections) further out
        let other_count = other_nodes.len();
        for (i, &idx) in other_nodes.iter().enumerate() {
            // Alternate between left and right
            let side = if i % 2 == 0 { -1.0 } else { 1.0 };
            let y_offset = (i as f64 / 2.0 - other_count as f64 / 4.0) * 15.0;
            self.node_positions[idx] = (side * 80.0, y_offset);
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
    }

    pub fn render(&mut self, f: &mut Frame, area: Rect) {
        use ratatui::widgets::{Paragraph, canvas::Canvas};
        use ratatui::text::{Line, Span};

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
                // Draw edges FIRST with HIGH VISIBILITY
                for (from_idx, to_idx, edge_label) in &self.connections {
                    if *from_idx < self.node_positions.len() && *to_idx < self.node_positions.len() {
                        let (x1, y1) = self.node_positions[*from_idx];
                        let (x2, y2) = self.node_positions[*to_idx];

                        // Draw BRIGHT CYAN lines for maximum visibility
                        ctx.draw(&CanvasLine {
                            x1, y1, x2, y2,
                            color: Color::Cyan,
                        });

                        // Draw edge label at midpoint (only if zoomed in enough)
                        if zoom > 0.8 {
                            let mid_x = (x1 + x2) / 2.0;
                            let mid_y = (y1 + y2) / 2.0;
                            let label_text = edge_label.format_short();
                            ctx.print(mid_x, mid_y - 1.0, label_text);
                        }
                    }
                }

                // Draw nodes as LARGER circles
                for (idx, (_, node)) in self.nodes.iter().enumerate() {
                    if idx < self.node_positions.len() {
                        let (x, y) = self.node_positions[idx];

                        // Determine node color based on state
                        let color = if Some(idx) == self.selected_node {
                            Color::White  // Selected node
                        } else if self.search_results.contains(&idx) {
                            Color::Yellow  // Search match
                        } else {
                            node.node_type.color()  // Normal color
                        };

                        // Draw node as BIGGER circle (radius 3.0) for better visibility
                        ctx.draw(&Circle {
                            x, y,
                            radius: 3.0,
                            color,
                        });

                        // Always show labels with more spacing
                        let label = node.label.clone();
                        ctx.print(x + 4.0, y, label);
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
