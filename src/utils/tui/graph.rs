use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Widget, Wrap},
    Frame,
};
use std::collections::HashMap;
use tui_nodes::{Connection, NodeGraph, NodeLayout};

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

pub struct WalletGraph {
    nodes: Vec<(String, WalletNode)>, // (address, node_data)
    connections: Vec<(usize, usize, String)>, // (from_idx, to_idx, label)
    target_wallet: String,
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

        self.nodes.len() - 1
    }

    pub fn add_connection(
        &mut self,
        from_address: &str,
        to_address: &str,
        label: String,
    ) {
        let from_idx = self.nodes.iter().position(|(addr, _)| addr == from_address);
        let to_idx = self.nodes.iter().position(|(addr, _)| addr == to_address);

        if let (Some(from), Some(to)) = (from_idx, to_idx) {
            self.connections.push((from, to, label));
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

        // Add connection with transfer info
        let label = if amount > 1000.0 {
            format!("{:.1}K {}", amount / 1000.0, token)
        } else {
            format!("{:.2} {}", amount, token)
        };

        self.add_connection(&from, &to, label);
    }

    pub fn render(&self, f: &mut Frame, area: Rect) {
        // Use tui-nodes NodeGraph widget for actual graph rendering
        if self.nodes.len() > 1 && !self.connections.is_empty() {
            let node_graph = self.build_node_graph();
            f.render_stateful_widget(node_graph, area, &mut ());
        } else {
            // Fallback for empty state
            use ratatui::widgets::Paragraph;
            use ratatui::text::{Line, Span, Text};

            let mut lines = Vec::new();
            lines.push(Line::from(vec![
                Span::styled("Wallet Transfer Network", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            ]));
            lines.push(Line::from(""));

            if let Some((_, target)) = self.nodes.first() {
                lines.push(Line::from(vec![
                    Span::styled(target.node_type.symbol(), Style::default()),
                    Span::styled(" Target: ", Style::default().fg(Color::White)),
                    Span::styled(&target.label, Style::default().fg(target.node_type.color()).add_modifier(Modifier::BOLD)),
                ]));
            }

            lines.push(Line::from(""));
            lines.push(Line::from(vec![
                Span::styled("⚠️  No transfer data yet", Style::default().fg(Color::Yellow))
            ]));
            lines.push(Line::from(vec![
                Span::styled("Waiting for investigation to discover transfers...", Style::default().fg(Color::DarkGray))
            ]));

            let widget = Paragraph::new(Text::from(lines))
                .block(
                    Block::default()
                        .title("Wallet Transfer Graph")
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(Color::Green)),
                );

            f.render_widget(widget, area);
        }
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn edge_count(&self) -> usize {
        self.connections.len()
    }

    // Helper to build graph from research agent data
    pub fn build_from_transfers(&mut self, transfers: &[TransferData]) {
        for transfer in transfers {
            let node_type_from = if transfer.from == self.target_wallet {
                WalletNodeType::Target
            } else if transfer.is_defi {
                WalletNodeType::DeFi
            } else {
                WalletNodeType::Funding
            };

            let node_type_to = if transfer.to == self.target_wallet {
                WalletNodeType::Target
            } else if transfer.is_defi {
                WalletNodeType::DeFi
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
            );
        }
    }

    // Get tui-nodes NodeGraph (lifetime-safe version)
    pub fn build_node_graph(&self) -> NodeGraph<'_> {
        let mut node_layouts = Vec::new();
        let mut tui_connections = Vec::new();

        // Build NodeLayout for each wallet
        for (idx, (_, node)) in self.nodes.iter().enumerate() {
            let layout = NodeLayout::new((15, 3))
                .with_title(&node.label)
                .with_border_style(Style::default().fg(node.node_type.color()));
            node_layouts.push(layout);
        }

        // Build Connection objects
        for (from_idx, to_idx, _label) in &self.connections {
            let conn = Connection::new(*from_idx, 0, *to_idx, 0);
            tui_connections.push(conn);
        }

        // Create NodeGraph with calculated width and height
        NodeGraph::new(node_layouts, tui_connections, 100, 50)
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

                    transfers.push(TransferData {
                        from: from.to_string(),
                        to: to.to_string(),
                        amount,
                        token: token_symbol.to_string(),
                        is_defi,
                        timestamp,
                    });
                }
            }
        }

        transfers
    }
}
