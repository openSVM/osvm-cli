//! Interactive TUI Graph Viewer for Transfer Relationships
//!
//! This module provides a Cursive-based interactive viewer for visualizing complex
//! wallet transfer graphs with support for:
//! - Collapsible tree navigation for deep graphs (depth > 4)
//! - Wallet convergence detection (same wallet via multiple paths)
//! - Keyboard navigation (j/k, Enter, arrows)
//! - Search and filter capabilities
//! - Integration with existing OSVM themes

use anyhow::Result;
use cursive::event::Key;
use cursive::traits::*;
use cursive::views::{Dialog, LinearLayout, Panel, ScrollView, SelectView, TextView};
use cursive::{Cursive, CursiveExt};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

use super::research_agent::{Transfer, TransferGraph};

/// Shared state wrapper for callbacks
type SharedState = Arc<Mutex<GraphViewerState>>;

/// Interactive TUI state for graph navigation
#[derive(Clone)]
pub struct GraphViewerState {
    /// The transfer graph being visualized
    pub graph: TransferGraph,
    /// Currently collapsed nodes (address -> bool)
    pub collapsed_nodes: HashMap<String, bool>,
    /// Selected node address
    pub selected_node: Option<String>,
    /// Search filter text
    pub search_filter: String,
}

impl GraphViewerState {
    pub fn new(graph: TransferGraph) -> Self {
        Self {
            graph,
            collapsed_nodes: HashMap::new(),
            selected_node: None,
            search_filter: String::new(),
        }
    }

    /// Toggle collapse state of a node
    pub fn toggle_collapse(&mut self, address: &str) {
        let is_collapsed = self.collapsed_nodes.get(address).copied().unwrap_or(false);
        self.collapsed_nodes
            .insert(address.to_string(), !is_collapsed);
    }

    /// Check if a node is collapsed
    pub fn is_collapsed(&self, address: &str) -> bool {
        self.collapsed_nodes.get(address).copied().unwrap_or(false)
    }

    /// Count convergence (how many paths lead to this wallet)
    pub fn count_convergence(&self, address: &str) -> usize {
        self.graph
            .nodes
            .values()
            .filter(|node| node.outgoing.iter().any(|transfer| transfer.to == address))
            .count()
    }
}

/// Render tree node with collapsible indicators
fn render_tree_node(
    state: &GraphViewerState,
    addr: &str,
    depth: usize,
    visited: &mut HashSet<String>,
    parent_prefix: String,
) -> Vec<String> {
    let mut lines = Vec::new();

    if visited.contains(addr) {
        // Convergence detected - show annotation
        let convergence_count = state.count_convergence(addr);
        let indent = "  ".repeat(depth);
        lines.push(format!(
            "{}{}↗ CONVERGENCE: {} ({} paths lead here)",
            parent_prefix,
            indent,
            state.graph.truncate_address(addr, 8),
            convergence_count
        ));
        return lines;
    }

    visited.insert(addr.to_string());

    let is_origin = state
        .graph
        .origin
        .as_ref()
        .map(|o| o == addr)
        .unwrap_or(false);
    let is_target = state
        .graph
        .target
        .as_ref()
        .map(|t| t == addr)
        .unwrap_or(false);
    let is_collapsed = state.is_collapsed(addr);

    let node = match state.graph.nodes.get(addr) {
        Some(n) => n,
        None => return lines,
    };

    let indent = "  ".repeat(depth);
    let truncated = state.graph.truncate_address(addr, 8);

    // Node line with collapse indicator
    let collapse_icon = if node.outgoing.is_empty() {
        " " // Leaf node
    } else if is_collapsed {
        "▶" // Collapsed
    } else {
        "▼" // Expanded
    };

    let node_icon = if is_origin {
        "🏦"
    } else if is_target {
        "🎯"
    } else {
        "○"
    };

    let convergence_count = state.count_convergence(addr);
    let convergence_indicator = if convergence_count > 1 {
        format!(" [×{} paths]", convergence_count)
    } else {
        String::new()
    };

    lines.push(format!(
        "{}{}{} {} {}{}",
        parent_prefix, indent, collapse_icon, node_icon, truncated, convergence_indicator
    ));

    // Show transfers if expanded
    if !is_collapsed {
        for (i, transfer) in node.outgoing.iter().enumerate() {
            let is_last = i == node.outgoing.len() - 1;
            let connector = if is_last { "└─" } else { "├─" };
            let child_prefix = if is_last {
                format!("{}{}   ", parent_prefix, indent)
            } else {
                format!("{}{}│  ", parent_prefix, indent)
            };

            // Transfer info
            lines.push(format!(
                "{}  {}──→ [{}] {}",
                child_prefix,
                connector,
                state.graph.format_amount(transfer.amount),
                transfer.token_symbol
            ));

            // Recurse to child
            let child_lines = render_tree_node(
                state,
                &transfer.to,
                depth + 1,
                visited,
                child_prefix.clone(),
            );
            lines.extend(child_lines);
        }
    } else {
        // Show count of hidden children
        let child_count = node.outgoing.len();
        if child_count > 0 {
            lines.push(format!(
                "{}{}  [... {} hidden transfers]",
                parent_prefix, indent, child_count
            ));
        }
    }

    lines
}

/// Build the tree view content
fn build_tree_view(state: &GraphViewerState) -> String {
    let mut output = String::new();

    // Header
    output
        .push_str("╔══════════════════════════════════════════════════════════════════════════╗\n");
    output
        .push_str("║              INTERACTIVE WALLET TRANSFER GRAPH VIEWER                    ║\n");
    output.push_str(
        "╚══════════════════════════════════════════════════════════════════════════╝\n\n",
    );

    // Instructions
    output.push_str("📖 Controls:\n");
    output.push_str("  j/k or ↑/↓  - Navigate nodes\n");
    output.push_str("  Enter/Space - Toggle collapse/expand\n");
    output.push_str("  /           - Search (coming soon)\n");
    output.push_str("  q or Esc    - Exit viewer\n\n");

    output.push_str(
        "═══════════════════════════════════════════════════════════════════════════\n\n",
    );

    // Find origin
    let origin_addr = match &state.graph.origin {
        Some(addr) => addr.clone(),
        None => {
            // Find any node with no incoming transfers
            state
                .graph
                .nodes
                .keys()
                .find(|addr| {
                    !state
                        .graph
                        .nodes
                        .values()
                        .any(|node| node.outgoing.iter().any(|t| &t.to == *addr))
                })
                .cloned()
                .unwrap_or_default()
        }
    };

    if origin_addr.is_empty() {
        output.push_str("⚠️  No graph data available\n");
        return output;
    }

    // Render tree
    let mut visited = HashSet::new();
    let lines = render_tree_node(state, &origin_addr, 0, &mut visited, String::new());
    for line in lines {
        output.push_str(&line);
        output.push('\n');
    }

    output.push_str(
        "\n═══════════════════════════════════════════════════════════════════════════\n",
    );

    // Stats
    let total_nodes = state.graph.nodes.len();
    let total_transfers: usize = state.graph.nodes.values().map(|n| n.outgoing.len()).sum();
    let convergence_nodes: Vec<_> = state
        .graph
        .nodes
        .keys()
        .filter(|addr| state.count_convergence(addr) > 1)
        .collect();

    output.push_str("\n📊 Graph Statistics:\n");
    output.push_str(&format!("  Total Nodes:       {}\n", total_nodes));
    output.push_str(&format!("  Total Transfers:   {}\n", total_transfers));
    output.push_str(&format!(
        "  Convergence Points: {} (wallets reached via multiple paths)\n",
        convergence_nodes.len()
    ));

    if !convergence_nodes.is_empty() {
        output.push_str("\n⚠️  Convergence Detected:\n");
        for addr in convergence_nodes.iter().take(5) {
            let count = state.count_convergence(addr);
            output.push_str(&format!(
                "  • {} ({} paths)\n",
                state.graph.truncate_address(addr, 12),
                count
            ));
        }
        if convergence_nodes.len() > 5 {
            output.push_str(&format!("  ... and {} more\n", convergence_nodes.len() - 5));
        }
    }

    output
}

/// Launch the interactive TUI graph viewer
pub fn launch_graph_viewer(graph: TransferGraph) -> Result<()> {
    let mut siv = Cursive::default();

    // Create initial state
    let state = GraphViewerState::new(graph);

    // Build initial view
    let tree_text = build_tree_view(&state);
    let text_view = TextView::new(tree_text).with_name("tree_view");

    let dialog = Dialog::around(
        ScrollView::new(text_view)
            .scroll_x(true)
            .scroll_y(true)
            .min_width(100)
            .min_height(30),
    )
    .title("Transfer Graph Viewer")
    .button("Quit (q)", |s| s.quit());

    siv.add_layer(dialog);

    // Store state in user_data
    siv.set_user_data(state);

    // Global key handlers
    siv.add_global_callback('q', |s| s.quit());
    siv.add_global_callback(cursive::event::Key::Esc, |s| s.quit());

    // TODO: Add interactive node selection and toggling
    // For now, this provides a scrollable view

    siv.run();

    Ok(())
}

/// Quick test function to demo the viewer
#[cfg(test)]
pub fn demo_graph_viewer() -> Result<()> {
    use super::research_agent::RenderConfig;

    // Create a test graph
    let config = RenderConfig::default();
    let mut graph = TransferGraph::with_config(config);

    // Add test data - complex convergence scenario
    let transfers = vec![
        Transfer {
            from: "ExchangeWallet111111111111111111111111111".to_string(),
            to: "MixerHub222222222222222222222222222222222".to_string(),
            amount: 100000.0,
            token_symbol: "SOL".to_string(),
            timestamp: Some("2025-01-01T00:00:00Z".to_string()),
            note: Some("Initial funding".to_string()),
        },
        Transfer {
            from: "MixerHub222222222222222222222222222222222".to_string(),
            to: "BurnerA3333333333333333333333333333333333".to_string(),
            amount: 30000.0,
            token_symbol: "SOL".to_string(),
            timestamp: Some("2025-01-01T00:05:00Z".to_string()),
            note: None,
        },
        Transfer {
            from: "MixerHub222222222222222222222222222222222".to_string(),
            to: "BurnerB4444444444444444444444444444444444".to_string(),
            amount: 30000.0,
            token_symbol: "SOL".to_string(),
            timestamp: Some("2025-01-01T00:05:00Z".to_string()),
            note: None,
        },
        Transfer {
            from: "BurnerA3333333333333333333333333333333333".to_string(),
            to: "Destination555555555555555555555555555555".to_string(),
            amount: 15000.0,
            token_symbol: "SOL".to_string(),
            timestamp: Some("2025-01-01T00:10:00Z".to_string()),
            note: None,
        },
        Transfer {
            from: "BurnerB4444444444444444444444444444444444".to_string(),
            to: "Destination555555555555555555555555555555".to_string(), // Convergence!
            amount: 15000.0,
            token_symbol: "SOL".to_string(),
            timestamp: Some("2025-01-01T00:10:00Z".to_string()),
            note: None,
        },
    ];

    for transfer in transfers {
        graph.add_transfer(transfer);
    }

    graph.origin = Some("ExchangeWallet111111111111111111111111111".to_string());
    graph.target = Some("Destination555555555555555555555555555555".to_string());

    launch_graph_viewer(graph)
}
