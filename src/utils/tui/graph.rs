use arboard::Clipboard;
use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    widgets::{
        canvas::{Canvas, Circle, Line as CanvasLine, Points, Rectangle},
        Block, Borders, Widget, Wrap,
    },
    Frame,
};
use rayon::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{
    atomic::{AtomicBool, AtomicU64, Ordering},
    Arc,
};
use std::thread::{self, JoinHandle};
use std::time::Instant;

// ============================================================================
// INCREMENTAL FORCE-DIRECTED LAYOUT ENGINE
// ============================================================================
// Purpose-built for streaming graphs. Key innovations:
// 1. Position Inheritance: New nodes spawn near their connected neighbors
// 2. Local Updates: Only compute forces for affected neighborhood
// 3. Persistent Spatial Index: Incremental quadtree updates vs full rebuild
// 4. Per-Node Temperature: Cold settled nodes, hot new nodes
// 5. Arrival Queue: Batch process incoming nodes efficiently

/// Per-node state for incremental layout
#[derive(Debug, Clone)]
pub struct NodeLayoutState {
    /// Node temperature (high = active, low = settled)
    pub temperature: f64,
    /// When this node was added (for age-based cooling)
    pub added_tick: u64,
    /// Number of layout iterations this node has participated in
    pub iterations: u32,
    /// Whether this node needs force recalculation
    pub dirty: bool,
    /// Cached force from last tick (for interpolation)
    pub cached_force: (f64, f64),
    /// Distance to nearest anchor (for stability)
    pub anchor_distance: f64,
}

impl Default for NodeLayoutState {
    fn default() -> Self {
        Self {
            temperature: 100.0, // Start hot
            added_tick: 0,
            iterations: 0,
            dirty: true,
            cached_force: (0.0, 0.0),
            anchor_distance: f64::MAX,
        }
    }
}

/// Arrival queue entry for batched node processing
#[derive(Debug, Clone)]
pub struct NodeArrival {
    pub node_idx: usize,
    pub connected_to: Vec<usize>, // Existing nodes this connects to
    pub inherited_position: Option<(f64, f64)>, // Pre-calculated from neighbors
}

/// Incremental Force-Directed Layout Engine
/// Designed for streaming graphs with O(k log n) per-node updates
#[derive(Debug)]
pub struct IncrementalLayout {
    /// Per-node layout state
    pub node_states: Vec<NodeLayoutState>,
    /// Queue of newly arrived nodes pending integration
    pub arrival_queue: VecDeque<NodeArrival>,
    /// Current tick counter
    pub tick: u64,
    /// Global minimum temperature (simulation never fully stops)
    pub min_temperature: f64,
    /// Per-node cooling rate
    pub node_cooling_rate: f64,
    /// Neighborhood radius for local updates (in ideal_length units)
    pub local_update_radius: f64,
    /// Whether incremental mode is active
    pub enabled: bool,
    /// Number of arrivals processed per tick
    pub arrivals_per_tick: usize,
    /// Hot zone: nodes within this many iterations are "hot"
    pub hot_threshold: u32,
    /// Nodes marked for local update this tick
    affected_nodes: HashSet<usize>,
    /// Performance stats
    pub stats: IncrementalLayoutStats,
}

/// Performance statistics for incremental layout
#[derive(Debug, Default, Clone)]
pub struct IncrementalLayoutStats {
    pub nodes_processed: usize,
    pub arrivals_processed: usize,
    pub local_updates: usize,
    pub full_updates: usize,
    pub avg_neighborhood_size: f64,
    pub last_tick_duration_us: u64,
}

impl Default for IncrementalLayout {
    fn default() -> Self {
        Self {
            node_states: Vec::new(),
            arrival_queue: VecDeque::new(),
            tick: 0,
            min_temperature: 2.0,     // Never fully stop
            node_cooling_rate: 0.92,  // Slightly faster than global cooling
            local_update_radius: 3.0, // Update nodes within 3 * ideal_length
            enabled: true,
            arrivals_per_tick: 5, // Process up to 5 new nodes per tick
            hot_threshold: 50,    // Nodes are "hot" for 50 iterations
            affected_nodes: HashSet::new(),
            stats: IncrementalLayoutStats::default(),
        }
    }
}

impl IncrementalLayout {
    /// Create new incremental layout engine
    pub fn new() -> Self {
        Self::default()
    }

    /// Ensure node states vector matches node count
    pub fn ensure_capacity(&mut self, n: usize) {
        while self.node_states.len() < n {
            let mut state = NodeLayoutState::default();
            state.added_tick = self.tick;
            self.node_states.push(state);
        }
    }

    /// Queue a new node for integration
    /// connected_to: indices of existing nodes this new node connects to
    pub fn queue_arrival(&mut self, node_idx: usize, connected_to: Vec<usize>) {
        self.arrival_queue.push_back(NodeArrival {
            node_idx,
            connected_to,
            inherited_position: None,
        });
    }

    /// Calculate inherited position for a new node based on its neighbors
    /// Returns centroid of connected neighbors with slight jitter
    pub fn calculate_inherited_position(
        &self,
        connected_to: &[usize],
        positions: &[(f64, f64)],
        ideal_length: f64,
    ) -> (f64, f64) {
        if connected_to.is_empty() || positions.is_empty() {
            // No neighbors - random position near origin
            let angle = (self.tick as f64 * 2.399) % (2.0 * std::f64::consts::PI);
            let r = ideal_length * 2.0;
            return (r * angle.cos(), r * angle.sin());
        }

        // Calculate centroid of connected neighbors
        let mut cx = 0.0;
        let mut cy = 0.0;
        let mut count = 0;

        for &idx in connected_to {
            if idx < positions.len() {
                let (x, y) = positions[idx];
                cx += x;
                cy += y;
                count += 1;
            }
        }

        if count == 0 {
            let angle = (self.tick as f64 * 2.399) % (2.0 * std::f64::consts::PI);
            let r = ideal_length * 2.0;
            return (r * angle.cos(), r * angle.sin());
        }

        cx /= count as f64;
        cy /= count as f64;

        // Add jitter: offset from centroid by ~1 ideal_length in a unique direction
        let jitter_angle =
            (self.tick as f64 + connected_to.len() as f64) * 0.618 * std::f64::consts::PI;
        let jitter_r = ideal_length * 0.5;

        (
            cx + jitter_r * jitter_angle.cos(),
            cy + jitter_r * jitter_angle.sin(),
        )
    }

    /// Process pending arrivals and integrate them into the layout
    /// Returns (new_node_indices, positions_to_set)
    pub fn process_arrivals(
        &mut self,
        positions: &[(f64, f64)],
        ideal_length: f64,
    ) -> Vec<(usize, f64, f64)> {
        let mut results = Vec::new();
        let count = self.arrivals_per_tick.min(self.arrival_queue.len());

        for _ in 0..count {
            if let Some(mut arrival) = self.arrival_queue.pop_front() {
                // Calculate position based on neighbors
                let pos = if let Some(p) = arrival.inherited_position {
                    p
                } else {
                    self.calculate_inherited_position(
                        &arrival.connected_to,
                        positions,
                        ideal_length,
                    )
                };

                results.push((arrival.node_idx, pos.0, pos.1));

                // Mark this node as hot
                self.ensure_capacity(arrival.node_idx + 1);
                self.node_states[arrival.node_idx].temperature = 100.0;
                self.node_states[arrival.node_idx].added_tick = self.tick;
                self.node_states[arrival.node_idx].dirty = true;

                // Mark connected neighbors for local update
                for &neighbor in &arrival.connected_to {
                    self.affected_nodes.insert(neighbor);
                    if neighbor < self.node_states.len() {
                        self.node_states[neighbor].dirty = true;
                        // Slightly reheat neighbors
                        self.node_states[neighbor].temperature =
                            (self.node_states[neighbor].temperature + 10.0).min(50.0);
                    }
                }
                self.affected_nodes.insert(arrival.node_idx);

                self.stats.arrivals_processed += 1;
            }
        }

        results
    }

    /// Get nodes that need force updates this tick
    /// In incremental mode: only affected neighborhood
    /// Falls back to all nodes if too many are affected
    pub fn get_nodes_to_update(&mut self, total_nodes: usize) -> Vec<usize> {
        if !self.enabled || self.affected_nodes.len() > total_nodes / 2 {
            // Too many affected - do full update
            self.stats.full_updates += 1;
            return (0..total_nodes).collect();
        }

        self.stats.local_updates += 1;
        self.affected_nodes.drain().collect()
    }

    /// Cool down node temperatures and prepare for next tick
    pub fn tick(&mut self) {
        self.tick += 1;
        let hot_cutoff = self.tick.saturating_sub(self.hot_threshold as u64);

        for state in &mut self.node_states {
            // Cool down temperature
            state.temperature =
                (state.temperature * self.node_cooling_rate).max(self.min_temperature);
            state.iterations += 1;

            // Mark old, cold nodes as stable (skip force calc if neighbors unchanged)
            if state.added_tick < hot_cutoff && state.temperature < 5.0 {
                state.dirty = false;
            }
        }

        self.affected_nodes.clear();
    }

    /// Check if a node is "hot" (recently added or disturbed)
    pub fn is_hot(&self, idx: usize) -> bool {
        if idx >= self.node_states.len() {
            return true; // Unknown nodes are hot
        }
        let state = &self.node_states[idx];
        state.temperature > 10.0 || self.tick - state.added_tick < self.hot_threshold as u64
    }

    /// Get effective temperature for a node (for force scaling)
    pub fn get_temperature(&self, idx: usize) -> f64 {
        if idx >= self.node_states.len() {
            100.0
        } else {
            self.node_states[idx].temperature
        }
    }

    /// Mark a node as needing update (e.g., new edge added)
    pub fn mark_dirty(&mut self, idx: usize) {
        if idx < self.node_states.len() {
            self.node_states[idx].dirty = true;
            self.node_states[idx].temperature =
                (self.node_states[idx].temperature + 15.0).min(80.0);
        }
        self.affected_nodes.insert(idx);
    }

    /// Get layout statistics
    pub fn get_stats(&self) -> IncrementalLayoutStats {
        self.stats.clone()
    }
}

// Re-export forensics types for backward compatibility
pub use super::graph_forensics::{
    AlertSeverity, CircularFlow, GraphData, GraphForensics, MixerStats, RapidFlowAlert,
    RiskExplanation, RiskLevel, WalletBehaviorType,
};

#[derive(Debug, Clone)]
pub enum WalletNodeType {
    Target,    // Red - the wallet being investigated
    Funding,   // Green - wallets that funded the target
    Recipient, // Blue - wallets that received from target
    DeFi,      // Magenta - DEX/DeFi protocols
    Token,     // Yellow - token contracts
    Mixer,     // DarkGray - detected mixing/tumbling node
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
    pub signature: Option<String>, // Transaction signature for Solana Explorer
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

// ============================================================================
// Barnes-Hut QuadTree for O(n log n) Force Approximation
// ============================================================================
//
// The Barnes-Hut algorithm approximates N-body repulsion forces in O(n log n)
// instead of O(n²) by treating distant groups of nodes as single point masses.
//
// Key parameters:
// - theta (θ): Controls accuracy vs speed tradeoff
//   - θ = 0.0: Exact O(n²) calculation (no approximation)
//   - θ = 0.5: Good balance (typical value)
//   - θ = 1.0: Very fast but less accurate
//
// The algorithm:
// 1. Build a quadtree that recursively subdivides space
// 2. For each internal node, compute center of mass of all contained points
// 3. When computing force on a point, traverse tree:
//    - If node is far enough (width/distance < θ), use center of mass
//    - Otherwise, recurse into children

/// Axis-aligned bounding box for QuadTree
#[derive(Debug, Clone, Copy)]
struct BoundingBox {
    x: f64,      // Center X
    y: f64,      // Center Y
    half_w: f64, // Half-width
    half_h: f64, // Half-height
}

impl BoundingBox {
    fn new(x: f64, y: f64, half_w: f64, half_h: f64) -> Self {
        Self {
            x,
            y,
            half_w,
            half_h,
        }
    }

    fn contains(&self, px: f64, py: f64) -> bool {
        px >= self.x - self.half_w
            && px <= self.x + self.half_w
            && py >= self.y - self.half_h
            && py <= self.y + self.half_h
    }

    /// Return which quadrant (0-3) a point belongs to, or None if outside
    fn quadrant(&self, px: f64, py: f64) -> Option<usize> {
        if !self.contains(px, py) {
            return None;
        }
        let right = px >= self.x;
        let top = py >= self.y;
        Some(match (right, top) {
            (false, true) => 0,  // NW
            (true, true) => 1,   // NE
            (false, false) => 2, // SW
            (true, false) => 3,  // SE
        })
    }

    fn subdivide(&self, quadrant: usize) -> Self {
        let qw = self.half_w / 2.0;
        let qh = self.half_h / 2.0;
        match quadrant {
            0 => Self::new(self.x - qw, self.y + qh, qw, qh), // NW
            1 => Self::new(self.x + qw, self.y + qh, qw, qh), // NE
            2 => Self::new(self.x - qw, self.y - qh, qw, qh), // SW
            3 => Self::new(self.x + qw, self.y - qh, qw, qh), // SE
            _ => *self,
        }
    }

    fn width(&self) -> f64 {
        self.half_w * 2.0
    }
}

/// QuadTree node for Barnes-Hut algorithm
#[derive(Debug)]
enum QuadTreeNode {
    /// Empty node (no points)
    Empty,
    /// Leaf node containing a single point (index into positions array)
    Leaf { idx: usize, x: f64, y: f64 },
    /// Internal node with 4 children and aggregated mass data
    Internal {
        children: Box<[QuadTreeNode; 4]>,
        /// Total "mass" (number of nodes in subtree)
        mass: f64,
        /// Center of mass X
        com_x: f64,
        /// Center of mass Y
        com_y: f64,
    },
}

impl Default for QuadTreeNode {
    fn default() -> Self {
        QuadTreeNode::Empty
    }
}

/// Barnes-Hut QuadTree for efficient N-body force calculation
pub struct BarnesHutTree {
    root: QuadTreeNode,
    bounds: BoundingBox,
    /// Theta parameter: higher = faster but less accurate (0.5 is typical)
    pub theta: f64,
}

impl BarnesHutTree {
    /// Build a new Barnes-Hut tree from node positions
    pub fn build(positions: &[(f64, f64)], theta: f64) -> Self {
        if positions.is_empty() {
            return Self {
                root: QuadTreeNode::Empty,
                bounds: BoundingBox::new(0.0, 0.0, 1.0, 1.0),
                theta,
            };
        }

        // Compute bounding box with some padding
        let (min_x, max_x, min_y, max_y) = positions.iter().fold(
            (f64::MAX, f64::MIN, f64::MAX, f64::MIN),
            |(min_x, max_x, min_y, max_y), &(x, y)| {
                (min_x.min(x), max_x.max(x), min_y.min(y), max_y.max(y))
            },
        );

        let cx = (min_x + max_x) / 2.0;
        let cy = (min_y + max_y) / 2.0;
        let half_w = ((max_x - min_x) / 2.0 + 10.0).max(10.0); // At least 10 units
        let half_h = ((max_y - min_y) / 2.0 + 10.0).max(10.0);
        let half_size = half_w.max(half_h); // Make it square for simplicity

        let bounds = BoundingBox::new(cx, cy, half_size, half_size);

        let mut tree = Self {
            root: QuadTreeNode::Empty,
            bounds,
            theta,
        };

        // Insert all points
        for (idx, &(x, y)) in positions.iter().enumerate() {
            tree.insert(idx, x, y);
        }

        // Compute centers of mass
        tree.compute_mass();

        tree
    }

    /// Insert a point into the tree
    fn insert(&mut self, idx: usize, x: f64, y: f64) {
        Self::insert_into(&mut self.root, self.bounds, idx, x, y, 0);
    }

    fn insert_into(
        node: &mut QuadTreeNode,
        bounds: BoundingBox,
        idx: usize,
        x: f64,
        y: f64,
        depth: usize,
    ) {
        // Prevent infinite recursion from numerical issues
        if depth > 50 {
            return;
        }

        match node {
            QuadTreeNode::Empty => {
                *node = QuadTreeNode::Leaf { idx, x, y };
            }
            QuadTreeNode::Leaf {
                idx: old_idx,
                x: old_x,
                y: old_y,
            } => {
                // Convert to internal node and reinsert both points
                let old_idx = *old_idx;
                let old_x = *old_x;
                let old_y = *old_y;

                *node = QuadTreeNode::Internal {
                    children: Box::new([
                        QuadTreeNode::Empty,
                        QuadTreeNode::Empty,
                        QuadTreeNode::Empty,
                        QuadTreeNode::Empty,
                    ]),
                    mass: 0.0,
                    com_x: 0.0,
                    com_y: 0.0,
                };

                // Reinsert old point
                Self::insert_into(node, bounds, old_idx, old_x, old_y, depth + 1);
                // Insert new point
                Self::insert_into(node, bounds, idx, x, y, depth + 1);
            }
            QuadTreeNode::Internal { children, .. } => {
                // Find which quadrant and insert there
                if let Some(q) = bounds.quadrant(x, y) {
                    let child_bounds = bounds.subdivide(q);
                    Self::insert_into(&mut children[q], child_bounds, idx, x, y, depth + 1);
                }
            }
        }
    }

    /// Compute center of mass for all internal nodes (bottom-up)
    fn compute_mass(&mut self) {
        Self::compute_mass_recursive(&mut self.root);
    }

    fn compute_mass_recursive(node: &mut QuadTreeNode) -> (f64, f64, f64) {
        match node {
            QuadTreeNode::Empty => (0.0, 0.0, 0.0),
            QuadTreeNode::Leaf { x, y, .. } => (1.0, *x, *y),
            QuadTreeNode::Internal {
                children,
                mass,
                com_x,
                com_y,
            } => {
                let mut total_mass = 0.0;
                let mut weighted_x = 0.0;
                let mut weighted_y = 0.0;

                for child in children.iter_mut() {
                    let (m, cx, cy) = Self::compute_mass_recursive(child);
                    total_mass += m;
                    weighted_x += m * cx;
                    weighted_y += m * cy;
                }

                if total_mass > 0.0 {
                    *mass = total_mass;
                    *com_x = weighted_x / total_mass;
                    *com_y = weighted_y / total_mass;
                }

                (total_mass, *com_x, *com_y)
            }
        }
    }

    /// Calculate repulsive force on a single node using Barnes-Hut approximation
    /// Returns (fx, fy) force vector
    pub fn calculate_force(
        &self,
        node_idx: usize,
        node_x: f64,
        node_y: f64,
        k2: f64,
        repulsion: f64,
    ) -> (f64, f64) {
        self.calculate_force_recursive(
            &self.root,
            self.bounds,
            node_idx,
            node_x,
            node_y,
            k2,
            repulsion,
        )
    }

    fn calculate_force_recursive(
        &self,
        node: &QuadTreeNode,
        bounds: BoundingBox,
        target_idx: usize,
        target_x: f64,
        target_y: f64,
        k2: f64,
        repulsion: f64,
    ) -> (f64, f64) {
        match node {
            QuadTreeNode::Empty => (0.0, 0.0),
            QuadTreeNode::Leaf { idx, x, y } => {
                if *idx == target_idx {
                    return (0.0, 0.0); // Don't compute self-force
                }
                // Direct force calculation
                let dx = target_x - *x;
                let dy = target_y - *y;
                let dist_sq = dx * dx + dy * dy;
                let dist = dist_sq.sqrt().max(0.1);

                // Repulsion: k² / d
                let force = repulsion * k2 / dist;
                ((dx / dist) * force, (dy / dist) * force)
            }
            QuadTreeNode::Internal {
                children,
                mass,
                com_x,
                com_y,
            } => {
                if *mass == 0.0 {
                    return (0.0, 0.0);
                }

                // Calculate distance to center of mass
                let dx = target_x - *com_x;
                let dy = target_y - *com_y;
                let dist_sq = dx * dx + dy * dy;
                let dist = dist_sq.sqrt().max(0.1);

                // Barnes-Hut criterion: width/distance < theta
                let width = bounds.width();
                if width / dist < self.theta {
                    // Use center of mass approximation
                    // Scale force by mass (number of nodes in cluster)
                    let force = repulsion * k2 * *mass / dist;
                    ((dx / dist) * force, (dy / dist) * force)
                } else {
                    // Recurse into children
                    let mut fx = 0.0;
                    let mut fy = 0.0;
                    for (q, child) in children.iter().enumerate() {
                        let child_bounds = bounds.subdivide(q);
                        let (cfx, cfy) = self.calculate_force_recursive(
                            child,
                            child_bounds,
                            target_idx,
                            target_x,
                            target_y,
                            k2,
                            repulsion,
                        );
                        fx += cfx;
                        fy += cfy;
                    }
                    (fx, fy)
                }
            }
        }
    }
}

/// Selection mode - navigating nodes vs edges
#[derive(Debug, Clone, PartialEq)]
pub enum SelectionMode {
    Node(usize), // Selected node index
    Edge {
        edge_idx: usize,
        from_node: usize,
        to_node: usize,
    }, // Selected edge with endpoints
}

// NOTE: WalletBehaviorType, RapidFlowAlert, AlertSeverity, CircularFlow,
// RiskExplanation, RiskLevel are re-exported from graph_forensics module above

/// Incremental risk statistics - updated O(1) on each edge/node add
/// Avoids full graph traversal for most risk factors
#[derive(Debug, Clone, Default)]
pub struct IncrementalRiskStats {
    /// Whale transfers (amount > 100 SOL)
    pub whale_count: usize,
    pub whale_total_volume: f64,
    /// Unique tokens seen (for diversity calculation)
    pub unique_tokens: HashSet<String>,
    /// Per-token transfer timestamps for rapid detection
    /// Maps token -> sorted list of (timestamp_secs, amount)
    pub token_transfer_times: HashMap<String, Vec<(u64, f64)>>,
    /// Running total transfer volume
    pub total_volume: f64,
    /// Running transfer count
    pub transfer_count: usize,
    /// Cached circular flow count (only updated on full recalc)
    pub cached_circular_count: usize,
    /// Cached rapid flow alerts (only updated on full recalc)
    pub cached_rapid_alerts: Vec<RapidFlowAlert>,
    /// Whether circular/rapid need full recalc (set to true after N incremental updates)
    pub needs_deep_recalc: bool,
    /// Counter for incremental updates since last deep recalc
    pub updates_since_deep_recalc: usize,
}

impl IncrementalRiskStats {
    /// Process a new edge and update incremental stats in O(1)
    pub fn on_edge_added(&mut self, amount: f64, token: &str, timestamp: Option<&str>) {
        self.transfer_count += 1;
        self.total_volume += amount;
        self.updates_since_deep_recalc += 1;

        // Track whale activity (>100 SOL)
        if amount > 100.0 {
            self.whale_count += 1;
            self.whale_total_volume += amount;
        }

        // Track token diversity
        self.unique_tokens.insert(token.to_string());

        // Track transfer timestamps for rapid detection
        if let Some(ts_str) = timestamp {
            if let Ok(ts) = ts_str.parse::<u64>() {
                self.token_transfer_times
                    .entry(token.to_string())
                    .or_insert_with(Vec::new)
                    .push((ts, amount));
            }
        }

        // Trigger deep recalc every 50 updates to keep circular flow detection fresh
        if self.updates_since_deep_recalc > 50 {
            self.needs_deep_recalc = true;
        }
    }

    /// Calculate rapid transfer alerts from cached timestamp data
    /// This is O(T * log T) where T is number of transfers per token (typically small)
    pub fn detect_rapid_transfers_incremental(
        &self,
        window_secs: u64,
        threshold: usize,
    ) -> Vec<RapidFlowAlert> {
        let mut alerts = Vec::new();

        for (token, times) in &self.token_transfer_times {
            if times.len() < threshold {
                continue;
            }

            // Sort by timestamp (already mostly sorted due to append-order)
            let mut sorted_times: Vec<_> = times.clone();
            sorted_times.sort_by_key(|(ts, _)| *ts);

            // Sliding window to find rapid bursts
            let mut window_start = 0;
            for window_end in 0..sorted_times.len() {
                // Shrink window from left if outside time window
                while sorted_times[window_end].0 - sorted_times[window_start].0 > window_secs {
                    window_start += 1;
                }

                let count = window_end - window_start + 1;
                if count >= threshold {
                    let volume: f64 = sorted_times[window_start..=window_end]
                        .iter()
                        .map(|(_, amt)| amt)
                        .sum();

                    let severity = if count >= 20 || volume > 1000.0 {
                        AlertSeverity::Critical
                    } else if count >= 10 || volume > 500.0 {
                        AlertSeverity::High
                    } else if count >= 5 || volume > 100.0 {
                        AlertSeverity::Medium
                    } else {
                        AlertSeverity::Low
                    };

                    alerts.push(RapidFlowAlert {
                        transfer_count: count,
                        time_window_secs: window_secs,
                        total_volume: volume,
                        token: token.clone(),
                        severity,
                    });
                    break; // One alert per token is enough
                }
            }
        }

        alerts
    }

    /// Calculate incremental risk score (O(1) for most factors)
    pub fn calculate_incremental_score(&self, node_count: usize, edge_count: usize) -> f64 {
        let mut score = 0.0;

        // 1. Network complexity (already O(1))
        let complexity = edge_count as f64 / node_count.max(1) as f64;
        if complexity > 5.0 {
            score += 30.0;
        } else if complexity > 3.0 {
            score += 15.0;
        }

        // 2. Whale activity (O(1) - pre-computed)
        if self.whale_count > 5 {
            score += 15.0;
        }

        // 3. Token diversity (O(1) - just check set size)
        if self.unique_tokens.len() > 20 {
            score += 10.0;
        }

        // 4. Rapid transfers (use cached if available, else compute)
        let rapid_count = self
            .cached_rapid_alerts
            .iter()
            .filter(|a| matches!(a.severity, AlertSeverity::Critical | AlertSeverity::High))
            .count();
        score += (rapid_count as f64) * 15.0;

        // 5. Circular flows (use cached count from last deep recalc)
        score += (self.cached_circular_count.min(3) as f64) * 20.0;

        score.min(100.0)
    }
}

/// Graph layout algorithm mode
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum LayoutMode {
    /// Force-directed physics simulation (default - better for seeing clusters)
    #[default]
    ForceDirected,
    /// Hierarchical BFS layers (inflows left, outflows right - better for tracing flows)
    Hierarchical,
}

impl LayoutMode {
    /// Toggle to the next layout mode
    pub fn toggle(&mut self) {
        *self = match self {
            LayoutMode::ForceDirected => LayoutMode::Hierarchical,
            LayoutMode::Hierarchical => LayoutMode::ForceDirected,
        };
    }

    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            LayoutMode::ForceDirected => "Force",
            LayoutMode::Hierarchical => "Hier",
        }
    }
}

/// Force-directed graph layout physics simulation parameters
/// Based on Fruchterman-Reingold algorithm with enhancements for money flow visualization
#[derive(Debug, Clone)]
pub struct ForceDirectedPhysics {
    /// Current simulation temperature (decreases over iterations for cooling)
    pub temperature: f64,
    /// Initial temperature (reset on new layout)
    pub initial_temp: f64,
    /// Cooling rate per iteration (0.95 = slow cooling, 0.8 = fast)
    pub cooling_rate: f64,
    /// Ideal edge length (k = sqrt(area/|V|))
    pub ideal_length: f64,
    /// Repulsion strength multiplier (higher = more spacing)
    pub repulsion_strength: f64,
    /// Attraction strength multiplier for connected nodes
    pub attraction_strength: f64,
    /// Gravity toward center (prevents graph flying apart)
    pub gravity: f64,
    /// Damping factor for velocity (0.0-1.0, prevents oscillation)
    pub damping: f64,
    /// Number of iterations completed
    pub iteration: usize,
    /// Max iterations before stopping simulation
    pub max_iterations: usize,
    /// Whether simulation is actively running
    pub running: bool,
    /// Whether to pin the target wallet at center
    pub pin_target: bool,
    /// Minimum movement threshold to consider converged
    pub convergence_threshold: f64,
    /// Barnes-Hut theta parameter (0.0=exact, 0.5=balanced, 1.0=fast)
    /// Higher values = faster but less accurate
    pub barnes_hut_theta: f64,
    /// Threshold node count to enable Barnes-Hut (O(n log n) vs O(n²))
    /// For small graphs, direct calculation is faster due to tree overhead
    pub barnes_hut_threshold: usize,
    /// Threshold for parallel computation (use rayon for graphs > this size)
    /// Below this, sequential is faster due to thread spawn overhead
    pub parallel_threshold: usize,
}

impl Default for ForceDirectedPhysics {
    fn default() -> Self {
        Self {
            temperature: 100.0,
            initial_temp: 100.0,
            cooling_rate: 0.95,
            ideal_length: 50.0, // Will be recalculated based on node count
            repulsion_strength: 1.0,
            attraction_strength: 0.5,
            gravity: 0.1,
            damping: 0.85,
            iteration: 0,
            max_iterations: 500,
            running: true,
            pin_target: true,
            convergence_threshold: 0.1,
            barnes_hut_theta: 0.5,     // Balanced accuracy/speed
            barnes_hut_threshold: 100, // Use Barnes-Hut for graphs > 100 nodes
            parallel_threshold: 200,   // Use rayon parallel for graphs > 200 nodes
        }
    }
}

impl ForceDirectedPhysics {
    /// Reset simulation for a new layout
    pub fn reset(&mut self, node_count: usize, area_width: f64, area_height: f64) {
        // Calculate ideal edge length: k = C * sqrt(area / |V|)
        let area = area_width * area_height;
        self.ideal_length = 2.0 * (area / node_count.max(1) as f64).sqrt();
        self.temperature = self.initial_temp;
        self.iteration = 0;
        self.running = true;
    }

    /// Cool down temperature for next iteration
    pub fn cool(&mut self) {
        self.temperature *= self.cooling_rate;
        self.iteration += 1;

        // Stop if temperature is too low or max iterations reached
        if self.temperature < self.convergence_threshold || self.iteration >= self.max_iterations {
            self.running = false;
        }
    }

    /// Check if simulation should continue
    pub fn should_continue(&self) -> bool {
        self.running && self.temperature > self.convergence_threshold
    }

    /// Reheat the simulation slightly for incremental updates (new nodes/edges)
    /// This allows the graph to settle gradually without full reset
    pub fn reheat_incremental(&mut self) {
        // Add a small amount of heat proportional to current temperature
        // This creates a "warm" simulation that can absorb changes
        let heat_boost = 5.0; // Small boost for smooth settling
        self.temperature = (self.temperature + heat_boost).min(self.initial_temp * 0.5);
        self.running = true;
        // Don't reset iteration count - we want continuous operation
    }

    /// Keep the simulation warm for streaming mode
    /// Maintains minimum temperature to always allow some movement
    pub fn maintain_warmth(&mut self, min_temp: f64) {
        if self.temperature < min_temp {
            self.temperature = min_temp;
            self.running = true;
        }
    }
}

/// Investigation trail - breadcrumb navigation through the graph
/// Now tracks edges between nodes for visual path rendering
#[derive(Debug, Clone)]
pub struct InvestigationTrail {
    pub steps: Vec<TrailStep>,
    pub current_index: usize,
    /// Index of currently selected outgoing edge from current node (for up/down cycling)
    pub tentative_edge_idx: Option<usize>,
    /// Cached outgoing edges from current node for quick cycling
    pub available_edges: Vec<usize>,
    /// Trail branch name (for multi-trail comparison)
    pub branch_name: String,
    /// Branch color for visual distinction
    pub branch_color: Color,
    /// Created timestamp for persistence
    pub created_at: u64,
    /// Notes/hypothesis for this trail branch
    pub hypothesis: Option<String>,
}

#[derive(Debug, Clone)]
pub struct TrailStep {
    pub node_index: usize,
    pub wallet_address: String,
    pub risk_level: RiskLevel,
    pub timestamp: std::time::Instant,
    pub note: Option<String>,
    /// Edge index used to reach this node from previous step (None for first step)
    pub edge_from_prev: Option<usize>,
    /// Amount transferred on this edge
    pub edge_amount: Option<f64>,
    /// Token transferred on this edge
    pub edge_token: Option<String>,
}

/// Serializable trail format for persistence
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SavedTrail {
    pub branch_name: String,
    pub hypothesis: Option<String>,
    pub created_at: u64,
    pub target_wallet: String,
    pub steps: Vec<SavedTrailStep>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SavedTrailStep {
    pub wallet_address: String,
    pub risk_level: String, // "Critical", "High", "Medium", "Low"
    pub note: Option<String>,
    pub edge_amount: Option<f64>,
    pub edge_token: Option<String>,
}

/// Trail file format containing multiple branches
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TrailFile {
    pub version: u32,
    pub target_wallet: String,
    pub trails: Vec<SavedTrail>,
    pub active_branch: usize,
}

/// Branch colors for multi-trail visual distinction
const BRANCH_COLORS: [Color; 8] = [
    Color::Rgb(30, 144, 255),  // Dodger Blue (primary)
    Color::Rgb(255, 165, 0),   // Orange
    Color::Rgb(50, 205, 50),   // Lime Green
    Color::Rgb(255, 105, 180), // Hot Pink
    Color::Rgb(64, 224, 208),  // Turquoise
    Color::Rgb(255, 215, 0),   // Gold
    Color::Rgb(186, 85, 211),  // Medium Orchid
    Color::Rgb(255, 99, 71),   // Tomato
];

impl InvestigationTrail {
    pub fn new(start_node: usize, start_address: String, start_risk: RiskLevel) -> Self {
        Self::new_branch(start_node, start_address, start_risk, "main".to_string(), 0)
    }

    /// Create a new named branch trail
    pub fn new_branch(
        start_node: usize,
        start_address: String,
        start_risk: RiskLevel,
        name: String,
        branch_idx: usize,
    ) -> Self {
        Self {
            steps: vec![TrailStep {
                node_index: start_node,
                wallet_address: start_address,
                risk_level: start_risk,
                timestamp: std::time::Instant::now(),
                note: Some("Investigation start".to_string()),
                edge_from_prev: None,
                edge_amount: None,
                edge_token: None,
            }],
            current_index: 0,
            tentative_edge_idx: None,
            available_edges: Vec::new(),
            branch_name: name,
            branch_color: BRANCH_COLORS[branch_idx % BRANCH_COLORS.len()],
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            hypothesis: None,
        }
    }

    /// Fork the current trail into a new branch from current position
    pub fn fork(&self, new_name: String, branch_idx: usize) -> Self {
        let mut forked = Self {
            steps: self.steps[..=self.current_index].to_vec(),
            current_index: self.current_index,
            tentative_edge_idx: None,
            available_edges: Vec::new(),
            branch_name: new_name,
            branch_color: BRANCH_COLORS[branch_idx % BRANCH_COLORS.len()],
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            hypothesis: None,
        };
        // Add fork note to the current step
        if let Some(last) = forked.steps.last_mut() {
            last.note = Some(format!("Forked from '{}' trail", self.branch_name));
        }
        forked
    }

    /// Convert to serializable format
    pub fn to_saved(&self, target_wallet: &str) -> SavedTrail {
        SavedTrail {
            branch_name: self.branch_name.clone(),
            hypothesis: self.hypothesis.clone(),
            created_at: self.created_at,
            target_wallet: target_wallet.to_string(),
            steps: self
                .steps
                .iter()
                .map(|s| SavedTrailStep {
                    wallet_address: s.wallet_address.clone(),
                    risk_level: format!("{:?}", s.risk_level),
                    note: s.note.clone(),
                    edge_amount: s.edge_amount,
                    edge_token: s.edge_token.clone(),
                })
                .collect(),
        }
    }

    /// Generate AI analysis context for this trail
    pub fn generate_ai_context(&self) -> String {
        let mut ctx = String::new();
        ctx.push_str(&format!("=== Trail Analysis: '{}' ===\n", self.branch_name));

        if let Some(h) = &self.hypothesis {
            ctx.push_str(&format!("Hypothesis: {}\n", h));
        }

        ctx.push_str(&format!(
            "Steps: {} | Total Flow: {:.2}\n\n",
            self.steps.len(),
            self.total_amount()
        ));

        ctx.push_str("Flow Path:\n");
        for (i, step) in self.steps.iter().enumerate() {
            let arrow = if i < self.steps.len() - 1 {
                "→"
            } else {
                "●"
            };
            let edge_info = match (&step.edge_amount, &step.edge_token) {
                (Some(amt), Some(tok)) => format!(" [{:.2} {}]", amt, tok),
                _ => String::new(),
            };
            ctx.push_str(&format!(
                "  {} {} ({:?}){}\n",
                arrow, step.wallet_address, step.risk_level, edge_info
            ));
        }

        // Token distribution
        let tokens = self.unique_tokens();
        if !tokens.is_empty() {
            ctx.push_str(&format!("\nTokens involved: {}\n", tokens.join(", ")));
        }

        // Risk distribution
        let critical = self
            .steps
            .iter()
            .filter(|s| matches!(s.risk_level, RiskLevel::Critical))
            .count();
        let high = self
            .steps
            .iter()
            .filter(|s| matches!(s.risk_level, RiskLevel::High))
            .count();
        if critical > 0 || high > 0 {
            ctx.push_str(&format!(
                "Risk wallets: {} critical, {} high\n",
                critical, high
            ));
        }

        ctx
    }

    /// Add a step to the trail with edge information
    pub fn add_step_with_edge(
        &mut self,
        node_index: usize,
        address: String,
        risk: RiskLevel,
        note: Option<String>,
        edge_idx: Option<usize>,
        edge_amount: Option<f64>,
        edge_token: Option<String>,
    ) {
        // If we're not at the end of the trail, truncate forward history
        if self.current_index < self.steps.len() - 1 {
            self.steps.truncate(self.current_index + 1);
        }

        self.steps.push(TrailStep {
            node_index,
            wallet_address: address,
            risk_level: risk,
            timestamp: std::time::Instant::now(),
            note,
            edge_from_prev: edge_idx,
            edge_amount,
            edge_token,
        });
        self.current_index = self.steps.len() - 1;
        // Reset tentative edge when we finalize a step
        self.tentative_edge_idx = None;
    }

    /// Legacy add_step for backward compatibility
    pub fn add_step(
        &mut self,
        node_index: usize,
        address: String,
        risk: RiskLevel,
        note: Option<String>,
    ) {
        self.add_step_with_edge(node_index, address, risk, note, None, None, None);
    }

    /// Go back one step (LEFT arrow) - removes the last step and returns to previous node
    pub fn go_back(&mut self) -> Option<usize> {
        if self.current_index > 0 {
            // Remove the current step (we're retracting)
            self.steps.truncate(self.current_index);
            self.current_index -= 1;
            self.tentative_edge_idx = None;
            Some(self.steps[self.current_index].node_index)
        } else {
            None
        }
    }

    /// Go forward in existing trail history (if we went back)
    pub fn go_forward(&mut self) -> Option<usize> {
        if self.current_index < self.steps.len() - 1 {
            self.current_index += 1;
            Some(self.steps[self.current_index].node_index)
        } else {
            None
        }
    }

    /// Set available edges for current node (for up/down cycling)
    pub fn set_available_edges(&mut self, edges: Vec<usize>) {
        self.available_edges = edges;
        if !self.available_edges.is_empty() && self.tentative_edge_idx.is_none() {
            self.tentative_edge_idx = Some(0); // Select first edge by default
        }
    }

    /// Cycle to previous edge (UP arrow)
    pub fn cycle_edge_up(&mut self) -> Option<usize> {
        if self.available_edges.is_empty() {
            return None;
        }
        if let Some(idx) = self.tentative_edge_idx {
            if idx > 0 {
                self.tentative_edge_idx = Some(idx - 1);
            } else {
                // Wrap to end
                self.tentative_edge_idx = Some(self.available_edges.len() - 1);
            }
        } else {
            self.tentative_edge_idx = Some(self.available_edges.len() - 1);
        }
        self.tentative_edge_idx.map(|i| self.available_edges[i])
    }

    /// Cycle to next edge (DOWN arrow)
    pub fn cycle_edge_down(&mut self) -> Option<usize> {
        if self.available_edges.is_empty() {
            return None;
        }
        if let Some(idx) = self.tentative_edge_idx {
            if idx + 1 < self.available_edges.len() {
                self.tentative_edge_idx = Some(idx + 1);
            } else {
                // Wrap to beginning
                self.tentative_edge_idx = Some(0);
            }
        } else {
            self.tentative_edge_idx = Some(0);
        }
        self.tentative_edge_idx.map(|i| self.available_edges[i])
    }

    /// Get the currently tentative edge index
    pub fn get_tentative_edge(&self) -> Option<usize> {
        self.tentative_edge_idx
            .and_then(|i| self.available_edges.get(i).copied())
    }

    pub fn current_step(&self) -> &TrailStep {
        &self.steps[self.current_index]
    }

    /// Get all edge indices in the trail (for rendering)
    pub fn trail_edges(&self) -> Vec<usize> {
        self.steps.iter().filter_map(|s| s.edge_from_prev).collect()
    }

    /// Get all node indices in the trail (for rendering)
    pub fn trail_nodes(&self) -> Vec<usize> {
        self.steps.iter().map(|s| s.node_index).collect()
    }

    /// Calculate total SOL equivalent in trail (sum of edge amounts)
    pub fn total_amount(&self) -> f64 {
        self.steps.iter().filter_map(|s| s.edge_amount).sum()
    }

    /// Get unique tokens in trail
    pub fn unique_tokens(&self) -> Vec<String> {
        let mut tokens: Vec<String> = self
            .steps
            .iter()
            .filter_map(|s| s.edge_token.clone())
            .collect();
        tokens.sort();
        tokens.dedup();
        tokens
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
            let edge_info = if let Some(amt) = step.edge_amount {
                format!(
                    " [{:.2} {}]",
                    amt,
                    step.edge_token.as_deref().unwrap_or("?")
                )
            } else {
                String::new()
            };
            summary.push_str(&format!(
                "{} {} {}{} - {:?} {}\n",
                marker,
                risk_icon,
                step.wallet_address,
                edge_info,
                step.risk_level,
                step.note
                    .as_ref()
                    .map(|n| format!("({})", n))
                    .unwrap_or_default()
            ));
        }
        summary
    }
}

pub struct WalletGraph {
    nodes: Vec<(String, WalletNode)>,            // (address, node_data)
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
    pub search_results: Vec<usize>, // Node indices matching search
    pub search_result_idx: usize,   // Current result position
    /// Toast notification
    pub toast_message: Option<String>,
    pub toast_timer: u8, // Frames remaining to show toast
    /// In-memory wallet cache for multi-hop discovery (address -> metadata)
    pub wallet_cache: HashMap<String, WalletCacheEntry>,
    /// Path-connected nodes (only nodes with paths to target)
    pub connected_nodes: std::collections::HashSet<usize>,
    /// Entity clusters (wallets controlled by same entity)
    pub entity_clusters: Vec<crate::utils::entity_clustering::EntityCluster>,
    /// Wallet to cluster ID mapping
    pub wallet_to_cluster: HashMap<String, usize>,
    /// Investigation trail (breadcrumb navigation) - active trail
    pub investigation_trail: Option<InvestigationTrail>,
    /// All trail branches (for comparison mode)
    pub all_trails: Vec<InvestigationTrail>,
    /// Active trail index in all_trails
    pub active_trail_idx: usize,
    /// Show minimap in corner
    pub show_minimap: bool,
    /// Minimap heatmap mode (true = density heatmap, false = normal node view)
    pub minimap_heatmap: bool,
    /// Show investigation trail at bottom
    pub show_trail: bool,
    /// Trail navigation mode active (arrow keys navigate trail instead of normal selection)
    pub trail_mode: bool,
    /// Trail comparison mode (show all branches overlaid)
    pub trail_compare_mode: bool,
    /// Trail-only mode: when true, only show nodes/edges that are part of investigation trails
    pub trail_only_mode: bool,
    /// Detail panel scroll position
    pub detail_scroll: usize,
    /// Filtered nodes (nodes with only 1 inflow OR 1 outflow - hidden from display)
    pub filtered_nodes: std::collections::HashSet<usize>,
    /// Folded nodes by direction (depth, flow_direction) -> vec of filtered node indices
    pub folded_groups: HashMap<(usize, i32), Vec<usize>>,
    /// Filter modal state for selective node/token display
    pub filter_modal: FilterModal,
    /// Whether user-defined filters are active (affects which nodes are rendered)
    pub user_filter_active: bool,
    /// Cached risk explanation to avoid expensive recalculation on every frame
    cached_risk_explanation: Option<RiskExplanation>,
    /// Flag indicating if risk cache needs recalculation (set when graph changes)
    risk_cache_dirty: bool,
    /// Flag indicating if layout needs recalculation (set when nodes/edges change)
    layout_dirty: bool,
    /// Counter for incremental updates since last full relayout
    /// Full relayout only happens when this exceeds threshold (batch mode)
    incremental_updates_pending: usize,
    /// Threshold for triggering full relayout (default 50)
    incremental_relayout_threshold: usize,
    /// Whether we're in streaming mode (suppresses full relayout)
    streaming_mode: bool,
    /// Incremental risk tracking - updated O(1) on each edge/node add
    incremental_risk: IncrementalRiskStats,
    /// Cached BFS distances from target node for proper layer calculation
    bfs_distances: HashMap<usize, usize>,
    /// Forensics engine instance for reuse
    forensics: GraphForensics,
    /// Cached wallet behavior classifications (wallet_idx -> behavior)
    /// Invalidated when edges involving the wallet change
    cached_wallet_behaviors: HashMap<usize, WalletBehaviorType>,
    /// Cached mixer node indices (HashSet for O(1) lookup)
    /// Recomputed when layout_dirty is true
    cached_mixer_nodes: HashSet<usize>,
    /// Flag indicating if entity clusters need recalculation
    entity_clusters_dirty: bool,
    /// Force-directed layout: node velocities (vx, vy)
    node_velocities: Vec<(f64, f64)>,
    /// Force-directed layout: physics simulation state
    physics: ForceDirectedPhysics,
    /// Current layout algorithm mode (toggle with 'L' key)
    pub layout_mode: LayoutMode,
    // ============================================================================
    // Double-Buffer + Physics Thread Decoupling
    // ============================================================================
    // The render thread reads from `render_positions` (never blocks)
    // The physics thread writes to `node_positions` then copies to `render_positions`
    // This completely decouples physics from rendering for smooth 60fps
    /// Render buffer - positions used by render() (read-only during render)
    /// Physics thread copies here after each simulation step
    render_positions: Vec<(f64, f64)>,
    /// Frame counter - incremented when render_positions is updated
    /// Render can check if new data is available
    physics_frame: Arc<AtomicU64>,
    /// Last frame rendered - compare to physics_frame to detect updates
    last_rendered_frame: u64,
    /// Signal to stop physics thread (set true on drop)
    physics_stop_signal: Arc<AtomicBool>,
    /// Physics thread dirty flag - set when node_positions changed
    physics_dirty: Arc<AtomicBool>,
    /// Incremental layout engine for streaming graphs
    incremental_layout: IncrementalLayout,
    /// Start time for physics timestamps
    start_time: Instant,
}

/// Cached wallet data for multi-hop path discovery
#[derive(Debug, Clone)]
pub struct WalletCacheEntry {
    pub address: String,
    pub inflows: Vec<(String, f64, String)>, // (from_address, amount, token)
    pub outflows: Vec<(String, f64, String)>, // (to_address, amount, token)
    pub discovered_at_depth: usize,
    pub is_rendered: bool, // false if not yet connected to target
}

// MixerStats is now in graph_forensics module (re-exported above)

/// Filter modal tab selection
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FilterTab {
    Wallets,
    Programs,
    Tokens,
}

/// Filter modal state
#[derive(Debug, Clone)]
pub struct FilterModal {
    pub active: bool,
    pub current_tab: FilterTab,
    pub selected_index: usize,
    pub scroll_offset: usize,
    /// Selected wallets (addresses that are visible)
    pub selected_wallets: std::collections::HashSet<String>,
    /// Selected tokens (tokens that are visible)
    pub selected_tokens: std::collections::HashSet<String>,
    /// Selected programs/DeFi (program addresses that are visible)
    pub selected_programs: std::collections::HashSet<String>,
    /// Cached list of all wallets for display
    pub all_wallets: Vec<(String, WalletNodeType)>,
    /// Cached list of all tokens for display
    pub all_tokens: Vec<String>,
    /// Cached list of all programs for display
    pub all_programs: Vec<String>,
}

impl Default for FilterModal {
    fn default() -> Self {
        Self {
            active: false,
            current_tab: FilterTab::Wallets,
            selected_index: 0,
            scroll_offset: 0,
            selected_wallets: std::collections::HashSet::new(),
            selected_tokens: std::collections::HashSet::new(),
            selected_programs: std::collections::HashSet::new(),
            all_wallets: Vec::new(),
            all_tokens: Vec::new(),
            all_programs: Vec::new(),
        }
    }
}

impl FilterModal {
    /// Get items for current tab
    pub fn current_items(&self) -> Vec<(String, bool)> {
        match self.current_tab {
            FilterTab::Wallets => self
                .all_wallets
                .iter()
                .map(|(addr, _)| (addr.clone(), self.selected_wallets.contains(addr)))
                .collect(),
            FilterTab::Programs => self
                .all_programs
                .iter()
                .map(|addr| (addr.clone(), self.selected_programs.contains(addr)))
                .collect(),
            FilterTab::Tokens => self
                .all_tokens
                .iter()
                .map(|token| (token.clone(), self.selected_tokens.contains(token)))
                .collect(),
        }
    }

    /// Toggle selection of current item
    pub fn toggle_current(&mut self) {
        let items = self.current_items();
        if self.selected_index >= items.len() {
            return;
        }
        let (item, _) = &items[self.selected_index];

        match self.current_tab {
            FilterTab::Wallets => {
                if self.selected_wallets.contains(item) {
                    self.selected_wallets.remove(item);
                } else {
                    self.selected_wallets.insert(item.clone());
                }
            }
            FilterTab::Programs => {
                if self.selected_programs.contains(item) {
                    self.selected_programs.remove(item);
                } else {
                    self.selected_programs.insert(item.clone());
                }
            }
            FilterTab::Tokens => {
                if self.selected_tokens.contains(item) {
                    self.selected_tokens.remove(item);
                } else {
                    self.selected_tokens.insert(item.clone());
                }
            }
        }
    }

    /// Select all items in current tab
    pub fn select_all(&mut self) {
        match self.current_tab {
            FilterTab::Wallets => {
                for (addr, _) in &self.all_wallets {
                    self.selected_wallets.insert(addr.clone());
                }
            }
            FilterTab::Programs => {
                for addr in &self.all_programs {
                    self.selected_programs.insert(addr.clone());
                }
            }
            FilterTab::Tokens => {
                for token in &self.all_tokens {
                    self.selected_tokens.insert(token.clone());
                }
            }
        }
    }

    /// Deselect all items in current tab
    pub fn deselect_all(&mut self) {
        match self.current_tab {
            FilterTab::Wallets => {
                self.selected_wallets.clear();
            }
            FilterTab::Programs => {
                self.selected_programs.clear();
            }
            FilterTab::Tokens => {
                self.selected_tokens.clear();
            }
        }
    }

    /// Next tab
    pub fn next_tab(&mut self) {
        self.current_tab = match self.current_tab {
            FilterTab::Wallets => FilterTab::Programs,
            FilterTab::Programs => FilterTab::Tokens,
            FilterTab::Tokens => FilterTab::Wallets,
        };
        self.selected_index = 0;
        self.scroll_offset = 0;
    }

    /// Previous tab
    pub fn prev_tab(&mut self) {
        self.current_tab = match self.current_tab {
            FilterTab::Wallets => FilterTab::Tokens,
            FilterTab::Programs => FilterTab::Wallets,
            FilterTab::Tokens => FilterTab::Programs,
        };
        self.selected_index = 0;
        self.scroll_offset = 0;
    }
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
    HopToWallet,      // Re-center graph on selected wallet
    ScrollDetailUp,   // Scroll detail panel up
    ScrollDetailDown, // Scroll detail panel down
    // Filter modal inputs
    OpenFilterModal,
    CloseFilterModal,
    FilterNextTab,
    FilterPrevTab,
    FilterToggleItem,
    FilterSelectAll,
    FilterDeselectAll,
    // Layout controls
    ToggleLayout,  // 'L' key - switch between force-directed and hierarchical
    ToggleMinimap, // 'M' key - toggle minimap visibility
    ToggleHeatmap, // 'H' key - toggle minimap heatmap mode
    // Trail navigation (arrow keys when trail mode is active)
    TrailExtend,     // RIGHT arrow - add tentative edge to trail and move to next node
    TrailRetract,    // LEFT arrow - remove last step from trail
    TrailEdgeUp,     // UP arrow - cycle to previous available edge
    TrailEdgeDown,   // DOWN arrow - cycle to next available edge
    ToggleTrailMode, // 'T' key - toggle trail navigation mode
    // Trail persistence and branching
    SaveTrails,          // Ctrl+S - save all trails to file
    LoadTrails,          // Ctrl+L - load trails from file
    ForkTrail,           // 'F' key - fork current trail into new branch
    NextTrailBranch,     // Tab - switch to next trail branch
    PrevTrailBranch,     // Shift+Tab - switch to previous trail branch
    ToggleCompareMode,   // 'C' key - toggle trail comparison overlay
    ToggleTrailOnlyMode, // '\' key - toggle showing only trail nodes/edges
    DeleteTrailBranch,   // 'X' key - delete current trail branch
    RenameTrailBranch,   // 'R' key - rename current branch (starts text input mode)
}

impl WalletGraph {
    pub fn new(target_wallet: String) -> Self {
        let mut nodes = Vec::new();

        // Add target wallet as first node
        let target_label = format!(
            "{}...{}",
            &target_wallet[..6],
            &target_wallet[target_wallet.len() - 4..]
        );
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
            viewport: (0.0, 0.0, 1.0),        // (center_x, center_y, zoom=1.0)
            max_depth: 15,                    // Increased for deep blockchain investigation
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
            investigation_trail: Some(InvestigationTrail::new(
                0,
                target_wallet.clone(),
                RiskLevel::Low,
            )),
            all_trails: vec![InvestigationTrail::new(0, target_wallet, RiskLevel::Low)],
            active_trail_idx: 0,
            show_minimap: true,
            minimap_heatmap: false, // Start in normal node view
            show_trail: true,
            trail_mode: true, // Trail mode on by default - arrow keys navigate trail
            trail_compare_mode: false,
            trail_only_mode: false, // Start with all nodes visible
            detail_scroll: 0,
            filtered_nodes: std::collections::HashSet::new(),
            folded_groups: HashMap::new(),
            filter_modal: FilterModal::default(),
            user_filter_active: false,
            cached_risk_explanation: None,
            risk_cache_dirty: true,
            layout_dirty: true, // Need initial layout calculation
            incremental_updates_pending: 0,
            incremental_relayout_threshold: 50, // Only relayout after 50 streaming updates
            streaming_mode: false,
            incremental_risk: IncrementalRiskStats::default(),
            bfs_distances: HashMap::new(),
            forensics: GraphForensics::with_defaults(),
            cached_wallet_behaviors: HashMap::new(),
            cached_mixer_nodes: HashSet::new(),
            entity_clusters_dirty: true, // Need computation on first access
            node_velocities: vec![(0.0, 0.0)], // Target wallet starts at rest
            physics: ForceDirectedPhysics::default(),
            layout_mode: LayoutMode::default(), // Force-directed by default
            // Double-buffer initialization
            render_positions: vec![(0.0, 0.0)], // Copy of node_positions for rendering
            physics_frame: Arc::new(AtomicU64::new(0)),
            last_rendered_frame: 0,
            physics_stop_signal: Arc::new(AtomicBool::new(false)),
            physics_dirty: Arc::new(AtomicBool::new(true)),
            // Incremental layout engine
            incremental_layout: IncrementalLayout::new(),
            start_time: Instant::now(),
        }
    }

    /// Enable/disable streaming mode (suppresses full relayout until batch threshold)
    /// Call this before starting streaming updates to avoid jarring layout changes
    pub fn set_streaming_mode(&mut self, enabled: bool) {
        self.streaming_mode = enabled;
        if !enabled {
            // When disabling streaming, trigger full relayout if updates are pending
            if self.incremental_updates_pending > 0 {
                self.layout_dirty = true;
                self.incremental_updates_pending = 0;
            }
        }
    }

    /// Force a full relayout (useful after batch of streaming updates)
    pub fn force_relayout(&mut self) {
        self.layout_dirty = true;
        self.incremental_updates_pending = 0;
    }

    /// Check if in streaming mode
    pub fn is_streaming_mode(&self) -> bool {
        self.streaming_mode
    }

    // ============================================================================
    // Double-Buffer Physics Decoupling API
    // ============================================================================

    /// Sync physics positions to render buffer
    /// Call this after physics updates to make new positions visible to render
    /// This is a fast O(n) copy operation
    pub fn sync_render_buffer(&mut self) {
        if self.node_positions.len() != self.render_positions.len() {
            self.render_positions
                .resize(self.node_positions.len(), (0.0, 0.0));
        }
        self.render_positions.clone_from(&self.node_positions);
        self.physics_frame.fetch_add(1, Ordering::Release);
        self.physics_dirty.store(false, Ordering::Release);
    }

    /// Run physics tick and sync to render buffer if dirty
    /// This is the public API for external physics loop
    /// Returns true if physics is still running (should call again next tick)
    pub fn tick_physics(&mut self) -> bool {
        if !matches!(self.layout_mode, LayoutMode::ForceDirected) {
            return false;
        }

        if self.nodes.is_empty() {
            return false;
        }

        // Maintain warmth in streaming mode
        if self.streaming_mode {
            self.physics.maintain_warmth(2.0);

            // === INCREMENTAL LAYOUT: Tick per-node temperatures ===
            self.incremental_layout.tick();
        }

        if !self.physics.should_continue() {
            return false;
        }

        // Run physics steps based on graph size
        let n = self.nodes.len();
        let iterations = if n < 100 {
            3
        } else if n < 500 {
            2
        } else {
            1
        };

        for _ in 0..iterations {
            self.step_force_directed_layout();
        }

        // Sync to render buffer after physics update
        self.sync_render_buffer();

        self.physics.should_continue()
    }

    /// Check if physics has new positions ready for render
    pub fn has_new_frame(&self) -> bool {
        let current = self.physics_frame.load(Ordering::Acquire);
        current > self.last_rendered_frame
    }

    /// Mark current frame as rendered (call after render completes)
    pub fn mark_frame_rendered(&mut self) {
        self.last_rendered_frame = self.physics_frame.load(Ordering::Acquire);
    }

    /// Get read-only access to render positions
    /// Use this in render() instead of node_positions
    pub fn get_render_positions(&self) -> &[(f64, f64)] {
        &self.render_positions
    }

    /// Get signals for physics thread control (for external physics loop)
    pub fn get_physics_signals(&self) -> (Arc<AtomicBool>, Arc<AtomicBool>, Arc<AtomicU64>) {
        (
            Arc::clone(&self.physics_stop_signal),
            Arc::clone(&self.physics_dirty),
            Arc::clone(&self.physics_frame),
        )
    }

    /// Signal physics thread to stop (call before dropping graph)
    pub fn stop_physics(&self) {
        self.physics_stop_signal.store(true, Ordering::Release);
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

    /// Auto-center camera on current selection (node or edge midpoint)
    fn center_camera_on_selection(&mut self) {
        match &self.selection {
            SelectionMode::Node(idx) => {
                if let Some(pos) = self.node_positions.get(*idx) {
                    self.viewport.0 = pos.0;
                    self.viewport.1 = pos.1;
                }
            }
            SelectionMode::Edge {
                from_node, to_node, ..
            } => {
                // Center on edge midpoint
                if let (Some(from_pos), Some(to_pos)) = (
                    self.node_positions.get(*from_node),
                    self.node_positions.get(*to_node),
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

    /// Toggle layout mode between force-directed and hierarchical
    /// Triggers layout recalculation
    pub fn toggle_layout_mode(&mut self) {
        self.layout_mode.toggle();
        self.layout_dirty = true; // Trigger recalculation
        self.show_toast(format!("Layout: {} (L to toggle)", self.layout_mode.name()));
    }

    /// Handle keyboard input for graph navigation
    pub fn handle_input(&mut self, input: GraphInput) -> Option<String> {
        match input {
            // EDGE-AWARE NAVIGATION (follows transaction flows)
            // In trail mode: Up/Down cycle edges, Left/Right extend/retract trail
            GraphInput::Up => {
                if self.trail_mode {
                    // Trail mode: cycle to previous available edge
                    return self.handle_input(GraphInput::TrailEdgeUp);
                }
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        // When on node, up/down just moves in list (fallback)
                        if *node_idx > 0 {
                            let new_idx = node_idx - 1;
                            self.selection = SelectionMode::Node(new_idx);
                            self.update_trail_on_selection(new_idx);
                        }
                    }
                    SelectionMode::Edge {
                        edge_idx,
                        from_node,
                        to_node,
                    } => {
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
                if self.trail_mode {
                    // Trail mode: cycle to next available edge
                    return self.handle_input(GraphInput::TrailEdgeDown);
                }
                match &self.selection {
                    SelectionMode::Node(node_idx) => {
                        if node_idx + 1 < self.nodes.len() {
                            let new_idx = node_idx + 1;
                            self.selection = SelectionMode::Node(new_idx);
                            self.update_trail_on_selection(new_idx);
                        }
                    }
                    SelectionMode::Edge {
                        edge_idx,
                        from_node,
                        to_node,
                    } => {
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
                if self.trail_mode {
                    // Trail mode: retract trail (go back one step)
                    return self.handle_input(GraphInput::TrailRetract);
                }
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
                if self.trail_mode {
                    // Trail mode: extend trail (add tentative edge and move to next node)
                    return self.handle_input(GraphInput::TrailExtend);
                }
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
            GraphInput::Select => self
                .selected_node()
                .and_then(|idx| self.nodes.get(idx))
                .map(|(addr, _)| addr.clone()),
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
                    self.search_result_idx =
                        (self.search_result_idx + 1) % self.search_results.len();
                    self.set_selected_node(Some(self.search_results[self.search_result_idx]));
                    // Center viewport on result
                    if let Some(pos) = self
                        .node_positions
                        .get(self.search_results[self.search_result_idx])
                    {
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
                    if let Some(pos) = self
                        .node_positions
                        .get(self.search_results[self.search_result_idx])
                    {
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
                        match Clipboard::new().and_then(|mut clip| clip.set_text(addr.to_string()))
                        {
                            Ok(_) => {
                                self.show_toast(format!(
                                    "Copied: {}...{}",
                                    &addr[..8],
                                    &addr[addr.len() - 8..]
                                ));
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
                            self.show_toast(format!(
                                "Centered on: {}...{}",
                                &addr[..8],
                                &addr[addr.len() - 8..]
                            ));
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
            // Filter modal inputs
            GraphInput::OpenFilterModal => {
                self.refresh_filter_lists();
                self.filter_modal.active = true;
                None
            }
            GraphInput::CloseFilterModal => {
                self.filter_modal.active = false;
                // Check if any filters are active
                self.user_filter_active = !self.filter_modal.selected_wallets.is_empty()
                    || !self.filter_modal.selected_tokens.is_empty()
                    || !self.filter_modal.selected_programs.is_empty();
                None
            }
            GraphInput::FilterNextTab => {
                self.filter_modal.next_tab();
                None
            }
            GraphInput::FilterPrevTab => {
                self.filter_modal.prev_tab();
                None
            }
            GraphInput::FilterToggleItem => {
                self.filter_modal.toggle_current();
                None
            }
            GraphInput::FilterSelectAll => {
                self.filter_modal.select_all();
                None
            }
            GraphInput::FilterDeselectAll => {
                self.filter_modal.deselect_all();
                None
            }
            GraphInput::ToggleLayout => {
                self.toggle_layout_mode();
                None
            }
            GraphInput::ToggleMinimap => {
                self.show_minimap = !self.show_minimap;
                self.show_toast(format!(
                    "Minimap: {}",
                    if self.show_minimap { "ON" } else { "OFF" }
                ));
                None
            }
            GraphInput::ToggleHeatmap => {
                self.minimap_heatmap = !self.minimap_heatmap;
                self.show_toast(format!(
                    "Minimap mode: {}",
                    if self.minimap_heatmap {
                        "Heatmap 🔥"
                    } else {
                        "Nodes 🔵"
                    }
                ));
                None
            }
            GraphInput::ToggleTrailMode => {
                self.trail_mode = !self.trail_mode;
                if self.trail_mode {
                    // Initialize available edges for current node when entering trail mode
                    self.refresh_trail_available_edges();
                }
                self.show_toast(format!(
                    "Trail mode: {} [←→↑↓]",
                    if self.trail_mode { "ON 🔍" } else { "OFF" }
                ));
                None
            }
            GraphInput::TrailEdgeUp => {
                if !self.trail_mode {
                    return None;
                }
                if let Some(ref mut trail) = self.investigation_trail {
                    if let Some(edge_idx) = trail.cycle_edge_up() {
                        // Update selection to show the tentative edge
                        if let Some((from, to, _)) = self.connections.get(edge_idx) {
                            self.selection = SelectionMode::Edge {
                                edge_idx,
                                from_node: *from,
                                to_node: *to,
                            };
                            self.center_camera_on_selection();
                        }
                    }
                }
                None
            }
            GraphInput::TrailEdgeDown => {
                if !self.trail_mode {
                    return None;
                }
                if let Some(ref mut trail) = self.investigation_trail {
                    if let Some(edge_idx) = trail.cycle_edge_down() {
                        // Update selection to show the tentative edge
                        if let Some((from, to, _)) = self.connections.get(edge_idx) {
                            self.selection = SelectionMode::Edge {
                                edge_idx,
                                from_node: *from,
                                to_node: *to,
                            };
                            self.center_camera_on_selection();
                        }
                    }
                }
                None
            }
            GraphInput::TrailExtend => {
                if !self.trail_mode {
                    return None;
                }
                // Get the tentative edge and finalize it
                let edge_info = if let Some(ref trail) = self.investigation_trail {
                    trail.get_tentative_edge().and_then(|edge_idx| {
                        self.connections.get(edge_idx).map(|(_, to, label)| {
                            (*to, edge_idx, label.amount, label.token.clone())
                        })
                    })
                } else {
                    None
                };

                if let Some((to_node, edge_idx, amount, token)) = edge_info {
                    // Get node info
                    let address = self
                        .nodes
                        .get(to_node)
                        .map(|(a, _)| a.clone())
                        .unwrap_or_default();
                    let risk_level = self.calculate_node_risk_level(to_node);

                    // Generate contextual note
                    let note = if to_node < self.nodes.len() {
                        if matches!(self.nodes[to_node].1.node_type, WalletNodeType::Mixer) {
                            Some("Mixer detected".to_string())
                        } else if let Some(cluster) = self.get_wallet_cluster(&address) {
                            Some(format!("Cluster #{}", cluster.cluster_id))
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Add step to trail with edge info
                    if let Some(ref mut trail) = self.investigation_trail {
                        trail.add_step_with_edge(
                            to_node,
                            address.clone(),
                            risk_level,
                            note,
                            Some(edge_idx),
                            Some(amount),
                            Some(token),
                        );
                    }

                    // Update selection to the new node
                    self.selection = SelectionMode::Node(to_node);
                    self.center_camera_on_selection();

                    // Refresh available edges for the new node
                    self.refresh_trail_available_edges();

                    self.show_toast(format!("Trail → {}", &address[..8.min(address.len())]));
                }
                None
            }
            GraphInput::TrailRetract => {
                if !self.trail_mode {
                    return None;
                }
                // Go back one step in the trail
                if let Some(ref mut trail) = self.investigation_trail {
                    if let Some(prev_node) = trail.go_back() {
                        self.selection = SelectionMode::Node(prev_node);
                        self.center_camera_on_selection();
                        self.refresh_trail_available_edges();
                        self.show_toast("Trail ← back".to_string());
                    }
                }
                None
            }

            // === Trail Persistence & Branching ===
            GraphInput::SaveTrails => {
                // Save all trails to ~/.osvm/trails/<target_wallet>.json
                match self.save_trails_to_disk() {
                    Ok(path) => self.show_toast(format!("Saved trails to {}", path)),
                    Err(e) => self.show_toast(format!("Save failed: {}", e)),
                }
                None
            }
            GraphInput::LoadTrails => {
                // Load trails from ~/.osvm/trails/<target_wallet>.json
                match self.load_trails_from_disk() {
                    Ok(count) => self.show_toast(format!("Loaded {} trail branches", count)),
                    Err(e) => self.show_toast(format!("Load failed: {}", e)),
                }
                None
            }
            GraphInput::ForkTrail => {
                // Fork current trail into new branch
                if let Some(ref trail) = self.investigation_trail {
                    let new_name = format!("branch-{}", self.all_trails.len());
                    let forked = trail.fork(new_name.clone(), self.all_trails.len());
                    self.all_trails.push(forked.clone());
                    self.active_trail_idx = self.all_trails.len() - 1;
                    self.investigation_trail = Some(forked);
                    self.show_toast(format!(
                        "Forked to branch '{}' [{}]",
                        new_name,
                        self.all_trails.len()
                    ));
                }
                None
            }
            GraphInput::NextTrailBranch => {
                if self.all_trails.is_empty() {
                    return None;
                }
                self.active_trail_idx = (self.active_trail_idx + 1) % self.all_trails.len();
                self.investigation_trail = Some(self.all_trails[self.active_trail_idx].clone());
                let name = self
                    .investigation_trail
                    .as_ref()
                    .map(|t| t.branch_name.clone())
                    .unwrap_or_default();
                self.show_toast(format!(
                    "Trail: '{}' [{}/{}]",
                    name,
                    self.active_trail_idx + 1,
                    self.all_trails.len()
                ));
                self.refresh_trail_available_edges();
                None
            }
            GraphInput::PrevTrailBranch => {
                if self.all_trails.is_empty() {
                    return None;
                }
                if self.active_trail_idx == 0 {
                    self.active_trail_idx = self.all_trails.len() - 1;
                } else {
                    self.active_trail_idx -= 1;
                }
                self.investigation_trail = Some(self.all_trails[self.active_trail_idx].clone());
                let name = self
                    .investigation_trail
                    .as_ref()
                    .map(|t| t.branch_name.clone())
                    .unwrap_or_default();
                self.show_toast(format!(
                    "Trail: '{}' [{}/{}]",
                    name,
                    self.active_trail_idx + 1,
                    self.all_trails.len()
                ));
                self.refresh_trail_available_edges();
                None
            }
            GraphInput::ToggleCompareMode => {
                self.trail_compare_mode = !self.trail_compare_mode;
                self.show_toast(format!(
                    "Compare mode: {}",
                    if self.trail_compare_mode {
                        "ON (all branches)"
                    } else {
                        "OFF"
                    }
                ));
                None
            }
            GraphInput::ToggleTrailOnlyMode => {
                self.trail_only_mode = !self.trail_only_mode;
                self.show_toast(format!(
                    "Trail-only mode: {}",
                    if self.trail_only_mode {
                        "ON (showing trail nodes only)"
                    } else {
                        "OFF (showing all nodes)"
                    }
                ));
                None
            }
            GraphInput::DeleteTrailBranch => {
                if self.all_trails.len() <= 1 {
                    self.show_toast("Cannot delete the only trail branch".to_string());
                    return None;
                }
                let deleted_name = self.all_trails[self.active_trail_idx].branch_name.clone();
                self.all_trails.remove(self.active_trail_idx);
                if self.active_trail_idx >= self.all_trails.len() {
                    self.active_trail_idx = self.all_trails.len() - 1;
                }
                self.investigation_trail = Some(self.all_trails[self.active_trail_idx].clone());
                self.show_toast(format!(
                    "Deleted '{}', now on '{}'",
                    deleted_name,
                    self.investigation_trail
                        .as_ref()
                        .map(|t| &t.branch_name)
                        .unwrap_or(&String::new())
                ));
                None
            }
            GraphInput::RenameTrailBranch => {
                // TODO: This would need text input mode - for now just show a message
                self.show_toast("Rename: use :rename <name> in future version".to_string());
                None
            }
            GraphInput::ToggleTrailOnlyMode => {
                // Toggle showing only trail nodes/edges vs full graph
                // TODO: Implement trail-only rendering mode
                self.show_toast("Trail-only mode: coming soon".to_string());
                None
            }
        }
    }

    /// Refresh available edges for trail navigation from current trail node
    fn refresh_trail_available_edges(&mut self) {
        // First get the current node from trail (immutable borrow)
        let current_node = self
            .investigation_trail
            .as_ref()
            .map(|t| t.current_step().node_index);

        if let Some(node_idx) = current_node {
            // Now compute outgoing edges (still immutable)
            let outgoing = self.outgoing_edges(node_idx);

            // Finally update the trail (mutable borrow)
            if let Some(ref mut trail) = self.investigation_trail {
                trail.set_available_edges(outgoing);
            }
        }
    }

    /// Refresh the filter lists from current graph data
    pub fn refresh_filter_lists(&mut self) {
        // Collect all wallets
        self.filter_modal.all_wallets = self
            .nodes
            .iter()
            .map(|(addr, node)| (addr.clone(), node.node_type.clone()))
            .collect();

        // Collect unique tokens from edges
        let mut tokens: std::collections::HashSet<String> = std::collections::HashSet::new();
        for (_, _, edge) in &self.connections {
            tokens.insert(edge.token.clone());
        }
        self.filter_modal.all_tokens = tokens.into_iter().collect();
        self.filter_modal.all_tokens.sort();

        // Collect programs (DeFi nodes)
        self.filter_modal.all_programs = self
            .nodes
            .iter()
            .filter(|(_, node)| matches!(node.node_type, WalletNodeType::DeFi))
            .map(|(addr, _)| addr.clone())
            .collect();

        // If no selections yet, select all by default
        if self.filter_modal.selected_wallets.is_empty()
            && self.filter_modal.selected_tokens.is_empty()
            && self.filter_modal.selected_programs.is_empty()
        {
            // Select all wallets
            for (addr, _) in &self.filter_modal.all_wallets {
                self.filter_modal.selected_wallets.insert(addr.clone());
            }
            // Select all tokens
            for token in &self.filter_modal.all_tokens {
                self.filter_modal.selected_tokens.insert(token.clone());
            }
            // Select all programs
            for addr in &self.filter_modal.all_programs {
                self.filter_modal.selected_programs.insert(addr.clone());
            }
        }
    }

    // =========================================================================
    // Trail Persistence Methods
    // =========================================================================

    /// Save all trail branches to disk (~/.osvm/trails/<target_wallet>.json)
    pub fn save_trails_to_disk(&self) -> Result<String, String> {
        let home = std::env::var("HOME").map_err(|_| "HOME not set")?;
        let trails_dir = std::path::PathBuf::from(&home).join(".osvm/trails");

        // Create directory if needed
        std::fs::create_dir_all(&trails_dir)
            .map_err(|e| format!("Failed to create trails dir: {}", e))?;

        // Build the trail file
        let trail_file = TrailFile {
            version: 1,
            target_wallet: self.target_wallet.clone(),
            trails: self
                .all_trails
                .iter()
                .map(|t| t.to_saved(&self.target_wallet))
                .collect(),
            active_branch: self.active_trail_idx,
        };

        // Generate filename from first 8 chars of wallet + timestamp
        let wallet_short = &self.target_wallet[..8.min(self.target_wallet.len())];
        let filename = format!("{}.json", wallet_short);
        let path = trails_dir.join(&filename);

        // Serialize and write
        let json = serde_json::to_string_pretty(&trail_file)
            .map_err(|e| format!("Serialize failed: {}", e))?;
        std::fs::write(&path, json).map_err(|e| format!("Write failed: {}", e))?;

        Ok(format!("~/.osvm/trails/{}", filename))
    }

    /// Load trail branches from disk
    pub fn load_trails_from_disk(&mut self) -> Result<usize, String> {
        let home = std::env::var("HOME").map_err(|_| "HOME not set")?;
        let wallet_short = &self.target_wallet[..8.min(self.target_wallet.len())];
        let path = std::path::PathBuf::from(&home)
            .join(".osvm/trails")
            .join(format!("{}.json", wallet_short));

        if !path.exists() {
            return Err("No saved trails found".to_string());
        }

        // Read and parse
        let json = std::fs::read_to_string(&path).map_err(|e| format!("Read failed: {}", e))?;
        let trail_file: TrailFile =
            serde_json::from_str(&json).map_err(|e| format!("Parse failed: {}", e))?;

        // Convert saved trails back to InvestigationTrail
        // Note: We can't fully restore node indices, so this recreates trails
        // with address-based reconstruction
        let mut loaded_trails = Vec::new();
        for (idx, saved) in trail_file.trails.iter().enumerate() {
            // Find the starting node in current graph
            let start_node = self.find_node_by_address(
                &saved
                    .steps
                    .first()
                    .map(|s| &s.wallet_address)
                    .unwrap_or(&self.target_wallet),
            );

            if let Some(start_idx) = start_node {
                let risk = self.parse_risk_level(
                    &saved
                        .steps
                        .first()
                        .map(|s| &s.risk_level)
                        .unwrap_or(&"Low".to_string()),
                );

                let mut trail = InvestigationTrail::new_branch(
                    start_idx,
                    saved
                        .steps
                        .first()
                        .map(|s| s.wallet_address.clone())
                        .unwrap_or_default(),
                    risk,
                    saved.branch_name.clone(),
                    idx,
                );
                trail.hypothesis = saved.hypothesis.clone();
                trail.created_at = saved.created_at;

                // Reconstruct remaining steps
                for step in saved.steps.iter().skip(1) {
                    if let Some(node_idx) = self.find_node_by_address(&step.wallet_address) {
                        let risk = self.parse_risk_level(&step.risk_level);
                        trail.add_step_with_edge(
                            node_idx,
                            step.wallet_address.clone(),
                            risk,
                            step.note.clone(),
                            None, // Can't restore edge index
                            step.edge_amount,
                            step.edge_token.clone(),
                        );
                    }
                }
                loaded_trails.push(trail);
            }
        }

        if loaded_trails.is_empty() {
            return Err("No trails matched current graph".to_string());
        }

        let count = loaded_trails.len();
        self.all_trails = loaded_trails;
        self.active_trail_idx = trail_file.active_branch.min(self.all_trails.len() - 1);
        self.investigation_trail = Some(self.all_trails[self.active_trail_idx].clone());

        Ok(count)
    }

    fn find_node_by_address(&self, address: &str) -> Option<usize> {
        self.nodes.iter().position(|(addr, _)| addr == address)
    }

    fn parse_risk_level(&self, level: &str) -> RiskLevel {
        match level {
            "Critical" => RiskLevel::Critical,
            "High" => RiskLevel::High,
            "Medium" => RiskLevel::Medium,
            _ => RiskLevel::Low,
        }
    }

    /// Get AI analysis context for current trail (for trail-guided AI)
    pub fn get_trail_ai_context(&self) -> Option<String> {
        self.investigation_trail
            .as_ref()
            .map(|t| t.generate_ai_context())
    }

    /// Get AI comparison context for all trails (when compare mode is on)
    pub fn get_trails_comparison_context(&self) -> String {
        let mut ctx = String::from("=== Multi-Trail Comparison ===\n\n");

        for (i, trail) in self.all_trails.iter().enumerate() {
            let active = if i == self.active_trail_idx {
                " ← ACTIVE"
            } else {
                ""
            };
            ctx.push_str(&format!(
                "--- Branch {}: '{}'{} ---\n",
                i + 1,
                trail.branch_name,
                active
            ));
            ctx.push_str(&trail.generate_ai_context());
            ctx.push('\n');
        }

        // Summary comparison
        ctx.push_str("\n=== Trail Comparison Summary ===\n");
        for trail in &self.all_trails {
            ctx.push_str(&format!(
                "• '{}': {} steps, {:.2} total flow, {} tokens\n",
                trail.branch_name,
                trail.steps.len(),
                trail.total_amount(),
                trail.unique_tokens().len()
            ));
        }

        ctx
    }

    /// Check if a node is in the current trail
    pub fn is_node_in_trail(&self, node_idx: usize) -> bool {
        self.investigation_trail
            .as_ref()
            .map(|t| t.trail_nodes().contains(&node_idx))
            .unwrap_or(false)
    }

    /// Check if an edge is in the current trail
    pub fn is_edge_in_trail(&self, edge_idx: usize) -> bool {
        self.investigation_trail
            .as_ref()
            .map(|t| t.trail_edges().contains(&edge_idx))
            .unwrap_or(false)
    }

    /// Check if a node is in any trail (for compare mode)
    pub fn is_node_in_any_trail(&self, node_idx: usize) -> Option<usize> {
        for (i, trail) in self.all_trails.iter().enumerate() {
            if trail.trail_nodes().contains(&node_idx) {
                return Some(i);
            }
        }
        None
    }

    /// Check if a node passes the current filter
    pub fn node_passes_filter(&self, node_idx: usize) -> bool {
        if !self.user_filter_active {
            return true;
        }

        if let Some((addr, _)) = self.nodes.get(node_idx) {
            self.filter_modal.selected_wallets.contains(addr)
        } else {
            false
        }
    }

    /// Check if an edge passes the current filter (both nodes visible and token selected)
    pub fn edge_passes_filter(&self, edge_idx: usize) -> bool {
        if !self.user_filter_active {
            return true;
        }

        if let Some((from, to, edge)) = self.connections.get(edge_idx) {
            // Both endpoints must be visible
            let from_visible = self.node_passes_filter(*from);
            let to_visible = self.node_passes_filter(*to);
            // Token must be selected
            let token_selected = self.filter_modal.selected_tokens.contains(&edge.token);

            from_visible && to_visible && token_selected
        } else {
            false
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
                    let incoming: Vec<_> = self
                        .connections
                        .iter()
                        .enumerate()
                        .filter(|(_, (_, to, _))| *to == *idx)
                        .collect();
                    let outgoing: Vec<_> = self
                        .connections
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
                                info.push_str(&format!(
                                    "  ... and {} more (scroll down)\n",
                                    incoming.len() - i
                                ));
                                break;
                            }

                            let from_addr = self
                                .nodes
                                .get(*from)
                                .map(|(a, _)| {
                                    if a.len() > 12 {
                                        format!("{}...{}", &a[..6], &a[a.len() - 4..])
                                    } else {
                                        a.clone()
                                    }
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
                                info.push_str(&format!(
                                    "  ... and {} more (scroll down)\n",
                                    outgoing.len() - i
                                ));
                                break;
                            }

                            let to_addr = self
                                .nodes
                                .get(*to)
                                .map(|(a, _)| {
                                    if a.len() > 12 {
                                        format!("{}...{}", &a[..6], &a[a.len() - 4..])
                                    } else {
                                        a.clone()
                                    }
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
            SelectionMode::Edge {
                edge_idx,
                from_node,
                to_node,
            } => {
                // Get edge metadata
                if let Some((from_addr, to_addr, edge_data)) = self.connections.get(*edge_idx) {
                    let from_label = self
                        .nodes
                        .get(*from_node)
                        .map(|(_, n)| n.label.clone())
                        .unwrap_or_else(|| "Unknown".to_string());
                    let to_label = self
                        .nodes
                        .get(*to_node)
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
                        "SPL Transfer", // Default transfer type
                        edge_data.timestamp.as_deref().unwrap_or("Unknown"),
                        edge_data
                            .signature
                            .as_deref()
                            .and_then(|s| if s.len() > 16 {
                                Some(format!("{}...{}", &s[..8], &s[s.len() - 8..]))
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
            format!("{}...{}", &address[..6], &address[address.len() - 4..])
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

        // Add node with appropriate positioning based on mode
        let idx = self.nodes.len() - 1;

        if self.streaming_mode && matches!(self.layout_mode, LayoutMode::ForceDirected) {
            // === INCREMENTAL LAYOUT: Queue node for intelligent positioning ===
            // Find which existing nodes this will connect to (we don't know yet,
            // but we can check if caller provided connected_to info)
            // For now, use neighbor-based positioning with the incremental engine

            // Use incremental layout to calculate inherited position
            let connected_to: Vec<usize> = Vec::new(); // Will be populated when edges are added
            let pos = self.incremental_layout.calculate_inherited_position(
                &connected_to,
                &self.node_positions,
                self.physics.ideal_length,
            );

            // Add position directly (not via queue since we need it now)
            self.node_positions.push(pos);
            self.render_positions.push(pos);
            self.node_velocities.push((0.0, 0.0));

            // Register node with incremental layout engine
            self.incremental_layout.ensure_capacity(idx + 1);
            self.incremental_layout.mark_dirty(idx);

            self.physics_dirty.store(true, Ordering::Release);
        } else {
            // Standard mode: use layered circular layout
            self.position_node_in_layer(idx);
        }

        // During bootstrap (no connections), add new nodes to connected set
        if self.connections.is_empty() {
            self.connected_nodes.insert(idx);
        }

        // Invalidate caches since graph structure changed
        self.risk_cache_dirty = true;

        // In streaming mode, DON'T trigger full relayout - let incremental layout handle it
        if self.streaming_mode {
            self.incremental_updates_pending += 1;
            // Removed: full relayout trigger - incremental layout handles this now
        } else {
            self.layout_dirty = true; // New node requires layout recomputation
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
        let angle = (position_in_layer as f64) * 2.0 * std::f64::consts::PI
            / (nodes_in_layer as f64 + 1.0).max(6.0);

        let pos = (radius * angle.cos(), radius * angle.sin());
        self.node_positions.push(pos);
        // Also add to render buffer for double-buffering
        self.render_positions.push(pos);
        // Also add velocity for force-directed physics
        self.node_velocities.push((0.0, 0.0));
        // Mark physics as dirty for sync
        self.physics_dirty.store(true, Ordering::Release);
    }

    /// Position a new node near its connected neighbor (for incremental force-directed)
    /// This finds the best connected neighbor and places the new node nearby with some offset
    /// Returns the position and a suggested initial velocity toward neighbors
    fn position_new_node_near_neighbor(&mut self, idx: usize, connected_to: Option<usize>) {
        // Default fallback: place at random position if no neighbor known
        let default_radius = self.physics.ideal_length * 2.0;
        let default_angle = (idx as f64 * 2.399) % (2.0 * std::f64::consts::PI); // Golden angle

        let (neighbor_pos, has_neighbor) = if let Some(neighbor_idx) = connected_to {
            if let Some(&pos) = self.node_positions.get(neighbor_idx) {
                (pos, true)
            } else {
                ((0.0, 0.0), false)
            }
        } else {
            // Try to find any neighbor from existing connections
            let mut found_neighbor: Option<(f64, f64)> = None;
            for (from, to, _) in &self.connections {
                if *from == idx && *to < self.node_positions.len() {
                    found_neighbor = Some(self.node_positions[*to]);
                    break;
                }
                if *to == idx && *from < self.node_positions.len() {
                    found_neighbor = Some(self.node_positions[*from]);
                    break;
                }
            }
            match found_neighbor {
                Some(pos) => (pos, true),
                None => ((0.0, 0.0), false),
            }
        };

        let (x, y) = if has_neighbor {
            // Place near neighbor with random offset
            let offset_dist = self.physics.ideal_length;
            let offset_angle = (idx as f64 * 2.399) % (2.0 * std::f64::consts::PI);
            (
                neighbor_pos.0 + offset_dist * offset_angle.cos(),
                neighbor_pos.1 + offset_dist * offset_angle.sin(),
            )
        } else {
            // No neighbor found, use default circular position
            (
                default_radius * default_angle.cos(),
                default_radius * default_angle.sin(),
            )
        };

        // Add position to both physics and render buffers
        self.node_positions.push((x, y));
        self.render_positions.push((x, y));

        // Give new node initial velocity toward center (helps it settle into the graph)
        let vel_to_center = if has_neighbor {
            // Velocity toward neighbor to help attraction
            let dx = neighbor_pos.0 - x;
            let dy = neighbor_pos.1 - y;
            let dist = (dx * dx + dy * dy).sqrt().max(1.0);
            (dx / dist * 2.0, dy / dist * 2.0)
        } else {
            // Velocity toward center
            let dist = (x * x + y * y).sqrt().max(1.0);
            (-x / dist * 2.0, -y / dist * 2.0)
        };
        self.node_velocities.push(vel_to_center);

        // Reheat physics slightly to allow settling
        self.physics.reheat_incremental();
        // Mark physics as dirty for sync
        self.physics_dirty.store(true, Ordering::Release);
    }

    /// Calculate node layer using cached BFS distances
    /// Uses the pre-computed bfs_distances map for O(1) lookup
    fn calculate_node_layer(&self, idx: usize) -> usize {
        // Target wallet is always layer 0
        if idx == 0 {
            return 0;
        }

        // Use cached BFS distance if available, fallback to 5 for disconnected nodes
        self.bfs_distances.get(&idx).copied().unwrap_or(5)
    }

    /// Compute BFS distances from target node to all other nodes
    /// This should be called when the graph structure changes (layout_dirty == true)
    fn compute_bfs_distances(&mut self) {
        self.bfs_distances.clear();

        // Target wallet (idx 0) is at distance 0
        let target_idx = 0;
        self.bfs_distances.insert(target_idx, 0);

        // BFS from target wallet following edges in BOTH directions
        let mut queue = VecDeque::new();
        queue.push_back((target_idx, 0usize));

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= 10 {
                continue; // Cap depth to avoid runaway
            }

            // Follow outgoing edges
            for (from, to, _) in &self.connections {
                if *from == current && !self.bfs_distances.contains_key(to) {
                    self.bfs_distances.insert(*to, depth + 1);
                    queue.push_back((*to, depth + 1));
                }
            }

            // Follow incoming edges (bidirectional traversal)
            for (from, to, _) in &self.connections {
                if *to == current && !self.bfs_distances.contains_key(from) {
                    self.bfs_distances.insert(*from, depth + 1);
                    queue.push_back((*from, depth + 1));
                }
            }
        }
    }

    fn count_nodes_in_layer(&self, layer: usize) -> usize {
        self.nodes
            .iter()
            .enumerate()
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
            // Update incremental risk stats BEFORE moving token
            self.incremental_risk
                .on_edge_added(amount, &token, timestamp.as_deref());

            let edge_label = EdgeLabel {
                amount,
                token,
                timestamp,
                signature,
            };
            self.connections.push((from, to, edge_label));
            // Invalidate caches since graph structure changed
            self.risk_cache_dirty = true;

            // In streaming mode, use incremental updates instead of full relayout
            if self.streaming_mode {
                self.incremental_updates_pending += 1;

                // For force-directed mode: use incremental layout engine
                if matches!(self.layout_mode, LayoutMode::ForceDirected) {
                    // Mark both endpoints as dirty for local force updates
                    self.incremental_layout.mark_dirty(from);
                    self.incremental_layout.mark_dirty(to);

                    // Reheat the physics slightly (incremental layout handles per-node temps)
                    self.physics.reheat_incremental();
                }
                // DON'T trigger full relayout - let incremental layout handle it
            } else {
                self.layout_dirty = true;
            }

            // Invalidate behavior cache for affected wallets (from and to)
            // since their transaction patterns have changed
            self.cached_wallet_behaviors.remove(&from);
            self.cached_wallet_behaviors.remove(&to);
            // Invalidate entity clusters (graph structure changed)
            self.entity_clusters_dirty = true;
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
                            if is_inflow {
                                -1
                            } else {
                                1
                            } // First level: determine by edge
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
            let total = self
                .connections
                .iter()
                .filter(|(from, to, _)| *from == *node_idx || *to == *node_idx)
                .map(|(_, _, edge)| edge.amount)
                .sum::<f64>();

            group.total_amount.insert(*node_idx, total);
            group.nodes.push(*node_idx);
        }

        // Clear and rebuild positions (both physics and render buffers)
        self.node_positions.clear();
        self.node_positions.resize(self.nodes.len(), (0.0, 0.0));
        self.render_positions.clear();
        self.render_positions.resize(self.nodes.len(), (0.0, 0.0));

        // Position center node at (0, 0)
        self.node_positions[center_idx] = (0.0, 0.0);

        // Row spacing (now vertical layout - inflows at top, outflows at bottom)
        let row_height = 60.0; // Was column_width, now controls Y spacing
        let node_spacing = 18.0; // Horizontal padding between nodes

        // Position nodes in rows (rotated 90 degrees)
        for ((depth, flow), mut group) in columns.into_iter() {
            if depth == 0 {
                continue;
            } // Skip center

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
                    self.node_positions[node_idx] = (x, y); // Swapped from (x,y) to maintain (x,y) format
                }
            }
        }

        // Handle disconnected nodes (place at far edges)
        let mut edge_count = 0;
        for i in 0..self.nodes.len() {
            if !distances.contains_key(&i) {
                let y = if edge_count % 2 == 0 { -200.0 } else { 200.0 }; // Swapped
                let x = (edge_count / 2) as f64 * node_spacing; // Swapped
                self.node_positions[i] = (x, y);
                edge_count += 1;
            }
        }

        // Sync to render buffer (hierarchical layout is instant, no physics)
        self.render_positions.clone_from(&self.node_positions);
        self.physics_dirty.store(true, Ordering::Release);
    }

    /// Initialize force-directed layout with random positions around center
    /// This sets up initial positions for the physics simulation to start from
    pub fn init_force_directed_layout(&mut self, area_width: f64, area_height: f64) {
        use std::f64::consts::PI;

        let n = self.nodes.len();
        if n == 0 {
            return;
        }

        // Track how many positions existed before this call
        let existing_count = self.node_positions.len();

        // Ensure vectors are sized correctly (both physics and render buffers)
        self.node_positions.resize(n, (0.0, 0.0));
        self.render_positions.resize(n, (0.0, 0.0));
        self.node_velocities.resize(n, (0.0, 0.0));

        // === STREAMING MODE: Only position NEW nodes, preserve existing ===
        // This prevents the jarring "jump" when new nodes are added
        if self.streaming_mode && existing_count > 0 {
            // Just reheat physics slightly to integrate new nodes
            self.physics.reheat_incremental();

            // Position only NEW nodes (from existing_count to n)
            // Place them near the outer edge where they'll flow into position
            let radius = self.physics.ideal_length * 2.0;
            for i in existing_count..n {
                // Use golden angle for even distribution
                let angle = (i as f64 * 2.399) % (2.0 * PI);
                let pos = (radius * angle.cos(), radius * angle.sin());
                self.node_positions[i] = pos;
                self.render_positions[i] = pos;
                self.node_velocities[i] = (0.0, 0.0);
            }
            self.physics_dirty.store(true, Ordering::Release);
            return;
        }

        // === NON-STREAMING MODE: Full layout reset (initial load) ===
        // Reset physics simulation
        self.physics.reset(n, area_width, area_height);

        // Place target wallet at center
        self.node_positions[0] = (0.0, 0.0);
        self.node_velocities[0] = (0.0, 0.0);

        // Place other nodes in a circle around center with some random jitter
        let radius = self.physics.ideal_length * (n as f64).sqrt() / 2.0;
        for i in 1..n {
            let angle = 2.0 * PI * (i as f64) / ((n - 1).max(1) as f64);
            let jitter_r = radius * 0.2 * (i as f64 % 7.0 - 3.0) / 3.0;
            let jitter_a = 0.1 * (i as f64 % 11.0 - 5.0) / 5.0;
            let r = radius + jitter_r;
            let a = angle + jitter_a;

            self.node_positions[i] = (r * a.cos(), r * a.sin());
            self.node_velocities[i] = (0.0, 0.0);
        }

        // Sync initial positions to render buffer
        self.render_positions.clone_from(&self.node_positions);
        self.physics_dirty.store(true, Ordering::Release);
    }

    /// Run one iteration of Fruchterman-Reingold force-directed layout
    /// Call this each frame while physics.should_continue() is true
    pub fn step_force_directed_layout(&mut self) {
        if !self.physics.should_continue() {
            return;
        }

        let n = self.nodes.len();
        if n < 2 {
            self.physics.running = false;
            return;
        }

        let k = self.physics.ideal_length;
        let k2 = k * k;
        let temp = self.physics.temperature;
        let repulsion = self.physics.repulsion_strength;
        let attraction = self.physics.attraction_strength;
        let gravity = self.physics.gravity;
        let damping = self.physics.damping;

        // Calculate forces for each node
        let mut forces: Vec<(f64, f64)> = vec![(0.0, 0.0); n];

        // 1. Repulsive forces: Use Barnes-Hut O(n log n) for large graphs, O(n²) for small
        let use_barnes_hut = n >= self.physics.barnes_hut_threshold;
        let use_parallel = n >= self.physics.parallel_threshold;

        if use_barnes_hut {
            // Build Barnes-Hut quadtree for efficient force approximation
            let tree = Arc::new(BarnesHutTree::build(
                &self.node_positions,
                self.physics.barnes_hut_theta,
            ));
            let positions = &self.node_positions;

            if use_parallel {
                // Parallel Barnes-Hut: O(n log n) with multicore speedup
                let parallel_forces: Vec<(f64, f64)> = (0..n)
                    .into_par_iter()
                    .map(|i| {
                        let (xi, yi) = positions[i];
                        tree.calculate_force(i, xi, yi, k2, repulsion)
                    })
                    .collect();

                for (i, (fx, fy)) in parallel_forces.into_iter().enumerate() {
                    forces[i].0 += fx;
                    forces[i].1 += fy;
                }
            } else {
                // Sequential Barnes-Hut: O(n log n)
                for i in 0..n {
                    let (xi, yi) = self.node_positions[i];
                    let (fx, fy) = tree.calculate_force(i, xi, yi, k2, repulsion);
                    forces[i].0 += fx;
                    forces[i].1 += fy;
                }
            }
        } else if use_parallel {
            // Parallel direct calculation for medium graphs (100-200 nodes)
            // Note: For O(n²) we compute per-node forces independently
            let positions = &self.node_positions;
            let parallel_forces: Vec<(f64, f64)> = (0..n)
                .into_par_iter()
                .map(|i| {
                    let (xi, yi) = positions[i];
                    let mut fx = 0.0;
                    let mut fy = 0.0;

                    for j in 0..n {
                        if i == j {
                            continue;
                        }
                        let (xj, yj) = positions[j];

                        let dx = xi - xj;
                        let dy = yi - yj;
                        let dist_sq = dx * dx + dy * dy;
                        let dist = dist_sq.sqrt().max(0.1);

                        let force = repulsion * k2 / dist;
                        fx += (dx / dist) * force;
                        fy += (dy / dist) * force;
                    }
                    (fx, fy)
                })
                .collect();

            for (i, (fx, fy)) in parallel_forces.into_iter().enumerate() {
                forces[i].0 += fx;
                forces[i].1 += fy;
            }
        } else {
            // Sequential direct O(n²) calculation for small graphs (faster due to no overhead)
            for i in 0..n {
                let (xi, yi) = self.node_positions[i];

                for j in (i + 1)..n {
                    let (xj, yj) = self.node_positions[j];

                    let dx = xi - xj;
                    let dy = yi - yj;
                    let dist_sq = dx * dx + dy * dy;
                    let dist = dist_sq.sqrt().max(0.1); // Avoid division by zero

                    // Repulsion: k² / d
                    let force = repulsion * k2 / dist;
                    let fx = (dx / dist) * force;
                    let fy = (dy / dist) * force;

                    forces[i].0 += fx;
                    forces[i].1 += fy;
                    forces[j].0 -= fx;
                    forces[j].1 -= fy;
                }
            }
        }

        // 2. Attractive forces: connected nodes attract (O(e))
        // Edge weight is based on transfer amount (log scale to handle large differences)
        for (from, to, edge_data) in &self.connections {
            let from = *from;
            let to = *to;
            if from >= n || to >= n {
                continue;
            }

            let (x1, y1) = self.node_positions[from];
            let (x2, y2) = self.node_positions[to];

            let dx = x2 - x1;
            let dy = y2 - y1;
            let dist = (dx * dx + dy * dy).sqrt().max(0.1);

            // Weight by transfer amount (log scale: 1 + log10(amount + 1))
            // This makes $1000 about 4x stronger than $1, but not 1000x
            let amount_weight = 1.0 + (edge_data.amount + 1.0).log10();

            // Attraction: (d² / k) * weight
            let force = attraction * dist * dist / k * amount_weight;
            let fx = (dx / dist) * force;
            let fy = (dy / dist) * force;

            forces[from].0 += fx;
            forces[from].1 += fy;
            forces[to].0 -= fx;
            forces[to].1 -= fy;
        }

        // 3. Gravity toward center (prevents graph flying apart)
        for i in 0..n {
            let (x, y) = self.node_positions[i];
            let dist = (x * x + y * y).sqrt().max(0.1);
            forces[i].0 -= gravity * x;
            forces[i].1 -= gravity * y;
        }

        // 4. Apply forces with temperature limiting and damping
        // In streaming mode, use per-node temperatures from incremental layout
        let use_per_node_temp = self.streaming_mode && self.incremental_layout.enabled;

        for i in 0..n {
            // Skip pinned target wallet
            if self.physics.pin_target && i == 0 {
                continue;
            }

            let (fx, fy) = forces[i];
            let force_mag = (fx * fx + fy * fy).sqrt().max(0.01);

            // Get effective temperature: global or per-node in streaming mode
            let node_temp = if use_per_node_temp {
                self.incremental_layout.get_temperature(i)
            } else {
                temp
            };

            // Limit displacement by temperature
            let scale = node_temp.min(force_mag) / force_mag;
            let dx = fx * scale;
            let dy = fy * scale;

            // Update velocity with damping
            // Hot nodes move more freely, cold nodes are more damped
            let node_damping = if use_per_node_temp && node_temp < 10.0 {
                damping * 0.95 // Extra damping for cold nodes
            } else {
                damping
            };

            self.node_velocities[i].0 = self.node_velocities[i].0 * node_damping + dx;
            self.node_velocities[i].1 = self.node_velocities[i].1 * node_damping + dy;

            // Update position
            self.node_positions[i].0 += self.node_velocities[i].0;
            self.node_positions[i].1 += self.node_velocities[i].1;
        }

        // 5. Cool down
        self.physics.cool();
    }

    /// Run force-directed layout until convergence or max iterations
    /// Use this for batch computation, or use step_force_directed_layout() for animation
    pub fn compute_force_directed_layout(&mut self, area_width: f64, area_height: f64) {
        self.init_force_directed_layout(area_width, area_height);

        // Run until convergence
        while self.physics.should_continue() {
            self.step_force_directed_layout();
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

        // Refresh trail available edges if we're in trail mode and new edges might affect current node
        // This ensures arrow keys work immediately when new edges stream in
        if self.trail_mode {
            self.refresh_trail_available_edges();
        }
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
        let target_idx = self
            .nodes
            .iter()
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
            let in_count = self
                .connections
                .iter()
                .filter(|(_, to, _)| *to == idx)
                .count();
            let out_count = self
                .connections
                .iter()
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
        let filtered_in_group: Vec<_> = self
            .filtered_nodes
            .iter()
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

        let addresses: Vec<String> = filtered_in_group
            .iter()
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
            if depth >= 5 {
                continue;
            }
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
            if depth >= 5 {
                continue;
            }
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
            let has_from_connection = self.connections.iter().any(|(f, t, _)| {
                (*f == from_idx && *t == mixer_idx) || (*f == mixer_idx && *t == from_idx)
            });
            let has_to_connection = self.connections.iter().any(|(f, t, _)| {
                (*f == to_idx && *t == mixer_idx) || (*f == mixer_idx && *t == to_idx)
            });

            if has_from_connection || has_to_connection {
                count += 1;
            }
        }

        count
    }

    /// Calculate risk score based on mixer involvement and path complexity
    fn calculate_risk_score(&self, from_idx: usize, to_idx: usize) -> String {
        let mixer_count = self.count_mixers_in_path(from_idx, to_idx);
        let from_connections = self
            .connections
            .iter()
            .filter(|(f, _, _)| *f == from_idx)
            .count();
        let to_connections = self
            .connections
            .iter()
            .filter(|(_, t, _)| *t == to_idx)
            .count();

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
    fn trace_token_flow_path(
        &self,
        from_idx: usize,
        to_idx: usize,
        token: &str,
        max_depth: usize,
    ) -> Vec<usize> {
        use std::collections::{HashSet, VecDeque};

        let mut path_edges = Vec::new();
        let mut visited_nodes = HashSet::new();
        let mixers = self.detect_mixer_nodes();

        // Trace backwards (inflows) from source
        let mut queue = VecDeque::new();
        queue.push_back((from_idx, 0));
        visited_nodes.insert(from_idx);

        while let Some((current, depth)) = queue.pop_front() {
            if depth >= max_depth {
                continue;
            }

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
            if depth >= max_depth {
                continue;
            }

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
    /// Delegates to forensics engine for pattern detection
    fn detect_mixer_nodes(&self) -> Vec<usize> {
        self.forensics.detect_mixer_nodes(self)
    }

    pub fn render(&mut self, f: &mut Frame, area: Rect) {
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{canvas::Canvas, Paragraph};

        // Split area: [main area | stats panel (if trail mode)]
        let (main_area, stats_area) = if self.trail_mode && self.investigation_trail.is_some() {
            let h_chunks = ratatui::layout::Layout::default()
                .direction(ratatui::layout::Direction::Horizontal)
                .constraints([
                    ratatui::layout::Constraint::Min(40),
                    ratatui::layout::Constraint::Length(30), // Stats panel width
                ])
                .split(area);
            (h_chunks[0], Some(h_chunks[1]))
        } else {
            (area, None)
        };

        // Split main area for trail breadcrumb if enabled
        let (graph_area, trail_area) = if self.show_trail && self.investigation_trail.is_some() {
            let chunks = ratatui::layout::Layout::default()
                .direction(ratatui::layout::Direction::Vertical)
                .constraints([
                    ratatui::layout::Constraint::Min(10),
                    ratatui::layout::Constraint::Length(5), // Increased from 3 to 5 for better visibility
                ])
                .split(main_area);
            (chunks[0], Some(chunks[1]))
        } else {
            (main_area, None)
        };

        // Render stats panel if trail mode active
        if let Some(stats_rect) = stats_area {
            self.render_trail_stats(f, stats_rect);
        }

        // Only recompute layout and cached data when graph structure changes
        // This is O(n²) and should NOT run every frame
        if self.layout_dirty && !self.nodes.is_empty() && !self.connections.is_empty() {
            self.compute_bfs_distances(); // Compute proper BFS distances
            self.update_node_filtering();
            // Update cached mixer nodes (converts Vec to HashSet for O(1) lookup)
            let mixer_vec = self.forensics.detect_mixer_nodes(self);
            self.cached_mixer_nodes = mixer_vec.into_iter().collect();

            // Initialize layout based on current mode
            match self.layout_mode {
                LayoutMode::ForceDirected => {
                    // 400x400 is a reasonable default working area for the physics simulation
                    self.init_force_directed_layout(400.0, 400.0);
                }
                LayoutMode::Hierarchical => {
                    // Use BFS-based hierarchical layout (inflows/outflows)
                    self.compute_hierarchical_layout();
                    self.physics.running = false; // Disable physics simulation
                }
            }
            self.layout_dirty = false;
        }

        // Run physics tick (runs simulation + syncs to render buffer)
        // This uses the decoupled tick_physics() API which handles all the complexity
        // Physics runs with adaptive iterations based on graph size and syncs to render buffer
        self.tick_physics();

        if self.nodes.is_empty() {
            let widget = Paragraph::new("  Waiting for transfer data...")
                .style(Style::default().fg(Color::DarkGray))
                .block(
                    Block::default()
                        .title(" Graph │ 0w 0tx ")
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(Color::Green)),
                );
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

        // Build improved title with layout mode and physics status
        let layout_indicator = match self.layout_mode {
            LayoutMode::ForceDirected => {
                if self.physics.running {
                    format!("⚡{} ({})", self.layout_mode.name(), self.physics.iteration)
                } else {
                    format!("📍{}", self.layout_mode.name())
                }
            }
            LayoutMode::Hierarchical => format!("📊{}", self.layout_mode.name()),
        };

        let canvas = Canvas::default()
            .block(
                Block::default()
                    .title(format!(
                        " 🔗 Graph │ {}w {}tx │ d:{}/{} │ z:{:.1}x │ {} │{}?=help L=layout ",
                        self.nodes.len(),
                        self.connections.len(),
                        self.current_depth,
                        self.max_depth,
                        zoom,
                        layout_indicator,
                        trail_indicator
                    ))
                    .borders(Borders::ALL)
                    .border_type(ratatui::widgets::BorderType::Rounded)
                    .border_style(Style::default().fg(Color::Green)),
            )
            .x_bounds([cx - 80.0 / zoom, cx + 80.0 / zoom]) // Narrower horizontal (nodes stack horizontally)
            .y_bounds([cy - 100.0 / zoom, cy + 100.0 / zoom]) // Taller vertical (flows top-to-bottom)
            .paint(|ctx| {
                // Use render_positions for drawing (double-buffered, never blocks)
                let positions = &self.render_positions;

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

                // Collect trail edges and nodes for highlighting
                let (trail_edges, trail_nodes, tentative_edge) =
                    if let Some(ref trail) = self.investigation_trail {
                        let edges: std::collections::HashSet<usize> =
                            trail.trail_edges().into_iter().collect();
                        let nodes: std::collections::HashSet<usize> =
                            trail.trail_nodes().into_iter().collect();
                        let tentative = trail.get_tentative_edge();
                        (edges, nodes, tentative)
                    } else {
                        (
                            std::collections::HashSet::new(),
                            std::collections::HashSet::new(),
                            None,
                        )
                    };

                // Draw ALL edges (even to off-screen nodes) with improved visual encoding
                // This includes edges where one or both endpoints are outside the visible viewport
                for (edge_idx, (from_idx, to_idx, edge_label)) in
                    self.connections.iter().enumerate()
                {
                    // FILTER: Trail-only mode - skip edges where either endpoint is not in any trail
                    if self.trail_only_mode
                        && self.is_node_in_any_trail(*from_idx).is_none()
                        && self.is_node_in_any_trail(*to_idx).is_none()
                    {
                        continue;
                    }

                    // Only check if positions exist, NOT if nodes are visible/filtered
                    if *from_idx < positions.len() && *to_idx < positions.len() {
                        let (x1, y1) = positions[*from_idx];
                        let (x2, y2) = positions[*to_idx];

                        // Calculate edge metrics
                        let edge_length = ((x2 - x1).powi(2) + (y2 - y1).powi(2)).sqrt();
                        let amount = edge_label.amount;

                        // Determine edge color and thickness based on state and amount
                        // Priority: tentative > selected > trail > highlighted_path > normal
                        let is_trail_edge = trail_edges.contains(&edge_idx);
                        let is_tentative = tentative_edge == Some(edge_idx);

                        let (edge_color, thickness) = if is_tentative {
                            // Tentative edge - cyan (#44ccff) for "preview" before confirming
                            (Color::Rgb(68, 204, 255), 4) // Cyan, extra thick
                        } else if Some(edge_idx) == selected_edge_idx {
                            (Color::Yellow, 3) // Selected edge - bright and thick
                        } else if is_trail_edge {
                            // Trail edge - thick yellow line (finalized path)
                            (Color::Yellow, 4) // Yellow, thick
                        } else if highlighted_path.contains(&edge_idx) {
                            (Color::LightYellow, 2) // Related flow - medium
                        } else {
                            // Distance-based fade for unselected edges
                            let dist_from_center = ((x1.powi(2) + y1.powi(2)).sqrt()
                                + (x2.powi(2) + y2.powi(2)).sqrt())
                                / 2.0;

                            // Color intensity based on transfer amount
                            let color = if amount > 1000.0 {
                                Color::Cyan // Large transfer
                            } else if amount > 100.0 {
                                Color::Blue // Medium transfer
                            } else if dist_from_center < 50.0 {
                                Color::LightBlue // Close to center
                            } else {
                                Color::DarkGray // Small/distant
                            };

                            // Thickness based on amount
                            let edge_thickness = if amount > 1000.0 { 2 } else { 1 };
                            (color, edge_thickness)
                        };

                        // Draw edge with appropriate thickness
                        for i in 0..thickness {
                            let offset = if thickness > 1 {
                                (i as f64 - thickness as f64 / 2.0) * 0.3
                            } else {
                                0.0
                            };
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

                    // FILTER: Trail-only mode - skip nodes not in any trail
                    if self.trail_only_mode && self.is_node_in_any_trail(idx).is_none() {
                        continue;
                    }

                    if idx < positions.len() {
                        let (x, y) = positions[idx];

                        // Check if this is a mixer node (using cached HashSet for O(1) lookup)
                        let is_mixer = self.cached_mixer_nodes.contains(&idx);
                        let is_selected = Some(idx) == self.selected_node();
                        let is_search_match = self.search_results.contains(&idx);

                        // Calculate node metrics for visual encoding
                        let in_degree = self
                            .connections
                            .iter()
                            .filter(|(_, to, _)| *to == idx)
                            .count();
                        let out_degree = self
                            .connections
                            .iter()
                            .filter(|(from, _, _)| *from == idx)
                            .count();
                        let total_degree = in_degree + out_degree;

                        // Size based on connection count (more connections = larger)
                        let base_radius = if idx == 0 {
                            5.0
                        }
                        // Target largest
                        else if is_mixer {
                            4.5
                        } else {
                            2.0 + (total_degree as f64).sqrt() * 0.5
                        };
                        let radius = base_radius.min(6.0); // Cap maximum size

                        // Check if this node is on the trail
                        let is_trail_node = trail_nodes.contains(&idx);

                        // Color determination with cluster support and trail highlighting
                        // Priority: selected > trail > mixer > search_match > target > cluster > flow-based
                        let color = if is_selected {
                            Color::White // Selected node
                        } else if is_trail_node {
                            Color::Rgb(30, 144, 255) // Dodger Blue for trail nodes
                        } else if is_mixer {
                            Color::Red // MIXER - highlight in RED
                        } else if is_search_match {
                            Color::Yellow // Search match
                        } else if idx == 0 {
                            Color::Magenta // Target wallet
                        } else if let Some((r, g, b)) = self.get_cluster_color(&self.nodes[idx].0) {
                            // Clustered wallet - use cluster color
                            Color::Rgb(r, g, b)
                        } else {
                            // Use color based on flow direction
                            if in_degree > out_degree {
                                Color::Green // Net receiver
                            } else if out_degree > in_degree {
                                Color::Blue // Net sender
                            } else {
                                Color::Cyan // Balanced
                            }
                        };

                        // Draw main circle
                        ctx.draw(&Circle {
                            x,
                            y,
                            radius,
                            color,
                        });

                        // Draw inner circle for visual depth
                        if radius > 2.5 {
                            ctx.draw(&Circle {
                                x,
                                y,
                                radius: radius * 0.6,
                                color: if is_selected {
                                    Color::Yellow
                                } else if is_mixer {
                                    Color::DarkGray
                                } else {
                                    Color::Black
                                },
                            });
                        }

                        // Add halo for selected/important/trail nodes
                        if is_selected || idx == 0 || is_trail_node {
                            ctx.draw(&Circle {
                                x,
                                y,
                                radius: radius + 2.0,
                                color: if is_selected {
                                    Color::Yellow
                                } else if is_trail_node {
                                    Color::Rgb(100, 180, 255)
                                }
                                // Light blue halo for trail
                                else {
                                    Color::Magenta
                                },
                            });
                        }

                        // Show abbreviated address (first 3 + last 3 chars)
                        let (addr, _) = &self.nodes[idx];
                        let short_addr = if addr.len() >= 6 {
                            format!("{}{}", &addr[..3], &addr[addr.len() - 3..])
                        } else {
                            addr.clone()
                        };

                        // Always show abbreviated address for all nodes
                        if idx == 0 {
                            // Target wallet - show with special marker
                            ctx.print(x, y - radius - 2.0, format!("🎯{}", short_addr));
                        } else if is_mixer {
                            // Mixer - show with warning marker and degree
                            ctx.print(
                                x,
                                y - radius - 2.0,
                                format!("⚠{}:{}↔{}", short_addr, in_degree, out_degree),
                            );
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
                    if filtered_idx >= positions.len() {
                        continue;
                    }

                    let (_, y) = positions[filtered_idx];
                    // Determine row based on Y position (now using Y for rows)
                    let row = (y / 60.0).round() as i32;
                    row_groups
                        .entry(row)
                        .or_insert_with(Vec::new)
                        .push(filtered_idx);
                }

                // Draw one folded circle per row
                for (row, nodes_in_row) in row_groups.iter() {
                    if nodes_in_row.is_empty() {
                        continue;
                    }

                    let count = nodes_in_row.len();
                    // Calculate average X position for this group (horizontal now)
                    let avg_x: f64 = nodes_in_row
                        .iter()
                        .filter_map(|&idx| positions.get(idx).map(|(x, _)| x))
                        .sum::<f64>()
                        / count as f64;

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
            use ratatui::widgets::Paragraph;
            let search_text = if self.search_results.is_empty() && !self.search_query.is_empty() {
                format!("Search: {} (no results)", self.search_query)
            } else if !self.search_results.is_empty() {
                format!(
                    "Search: {} ({}/{} results)",
                    self.search_query,
                    self.search_result_idx + 1,
                    self.search_results.len()
                )
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
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .border_style(Style::default().fg(Color::Yellow))
                        .title(" Search (ESC to cancel, n/N for next/prev) "),
                )
                .style(Style::default().fg(Color::White));

            f.render_widget(search_widget, search_area);
        }

        // Render toast notification if present
        if let Some(ref toast_msg) = self.toast_message {
            use ratatui::text::Span;
            use ratatui::widgets::Paragraph;

            let toast_area = Rect {
                x: area.x + (area.width.saturating_sub(toast_msg.len() as u16 + 4)) / 2,
                y: area.y + 2,
                width: (toast_msg.len() as u16 + 4).min(area.width),
                height: 3,
            };

            let toast_widget = Paragraph::new(vec![ratatui::text::Line::from(Span::styled(
                toast_msg.as_str(),
                Style::default()
                    .fg(Color::Black)
                    .bg(Color::Green)
                    .add_modifier(Modifier::BOLD),
            ))])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Green)),
            );

            f.render_widget(toast_widget, toast_area);
        }

        // Render minimap in bottom-right corner
        if self.show_minimap && !self.render_positions.is_empty() {
            self.render_minimap(f, graph_area);
        }
    }

    /// Render a minimap in the bottom-right corner showing full graph extent with viewport indicator
    /// Supports two modes: normal (node dots) and heatmap (density grid)
    fn render_minimap(&self, f: &mut Frame, area: Rect) {
        use ratatui::text::Span;
        use ratatui::widgets::{canvas::Canvas, Paragraph};

        // Minimap dimensions (fixed size overlay)
        let minimap_width = 22u16;
        let minimap_height = 14u16;

        // Position in bottom-right corner with padding
        let minimap_rect = Rect {
            x: area.x + area.width.saturating_sub(minimap_width + 2),
            y: area.y + area.height.saturating_sub(minimap_height + 2),
            width: minimap_width,
            height: minimap_height,
        };

        // Calculate graph bounding box (use render_positions for minimap)
        let (mut min_x, mut max_x, mut min_y, mut max_y) = (f64::MAX, f64::MIN, f64::MAX, f64::MIN);
        for (x, y) in &self.render_positions {
            min_x = min_x.min(*x);
            max_x = max_x.max(*x);
            min_y = min_y.min(*y);
            max_y = max_y.max(*y);
        }

        // Add padding to bounds
        let padding = 10.0;
        min_x -= padding;
        max_x += padding;
        min_y -= padding;
        max_y += padding;

        // Get current viewport bounds
        let (cx, cy, zoom) = self.viewport;
        let view_half_w = 80.0 / zoom;
        let view_half_h = 100.0 / zoom;
        let view_min_x = cx - view_half_w;
        let view_max_x = cx + view_half_w;
        let view_min_y = cy - view_half_h;
        let view_max_y = cy + view_half_h;

        // Get selected node index for highlighting
        let selected_idx = self.selected_node();

        // Calculate heatmap grid if in heatmap mode
        let grid_size = 8; // 8x8 grid
        let range_x = max_x - min_x;
        let range_y = max_y - min_y;
        let cell_w = range_x / grid_size as f64;
        let cell_h = range_y / grid_size as f64;

        // Count nodes per cell (for heatmap mode)
        let mut density_grid: [[usize; 8]; 8] = [[0; 8]; 8];
        let mut max_density = 1usize;

        if self.minimap_heatmap {
            for (x, y) in &self.render_positions {
                let gx = ((x - min_x) / cell_w).floor() as usize;
                let gy = ((y - min_y) / cell_h).floor() as usize;
                let gx = gx.min(grid_size - 1);
                let gy = gy.min(grid_size - 1);
                density_grid[gy][gx] += 1;
                max_density = max_density.max(density_grid[gy][gx]);
            }
        }

        // Title shows mode indicator
        let title = if self.minimap_heatmap {
            Span::styled(" 🔥 Heat ", Style::default().fg(Color::Red))
        } else {
            Span::styled(" 🗺 Map ", Style::default().fg(Color::Cyan))
        };

        let minimap = Canvas::default()
            .block(
                Block::default()
                    .title(title)
                    .borders(Borders::ALL)
                    .border_type(ratatui::widgets::BorderType::Rounded)
                    .border_style(Style::default().fg(Color::DarkGray)),
            )
            .x_bounds([min_x, max_x])
            .y_bounds([min_y, max_y])
            .paint(|ctx| {
                if self.minimap_heatmap {
                    // HEATMAP MODE: Draw density grid cells
                    for gy in 0..grid_size {
                        for gx in 0..grid_size {
                            let count = density_grid[gy][gx];
                            if count > 0 {
                                // Color based on density (blue → green → yellow → red)
                                let intensity = count as f64 / max_density as f64;
                                let color = if intensity > 0.75 {
                                    Color::Red // Hot spot
                                } else if intensity > 0.5 {
                                    Color::LightRed
                                } else if intensity > 0.25 {
                                    Color::Yellow
                                } else if intensity > 0.1 {
                                    Color::Green
                                } else {
                                    Color::DarkGray
                                };

                                // Draw filled rectangle for this cell
                                let x1 = min_x + gx as f64 * cell_w;
                                let y1 = min_y + gy as f64 * cell_h;
                                let x2 = x1 + cell_w;
                                let y2 = y1 + cell_h;

                                // Draw rectangle outline
                                ctx.draw(&Rectangle {
                                    x: x1,
                                    y: y1,
                                    width: cell_w,
                                    height: cell_h,
                                    color,
                                });
                            }
                        }
                    }
                } else {
                    // NORMAL MODE: Draw edges and nodes
                    // Draw all edges as thin lines (simplified for minimap)
                    for (from_idx, to_idx, _) in &self.connections {
                        if *from_idx < self.render_positions.len()
                            && *to_idx < self.render_positions.len()
                        {
                            let (x1, y1) = self.render_positions[*from_idx];
                            let (x2, y2) = self.render_positions[*to_idx];
                            ctx.draw(&CanvasLine {
                                x1,
                                y1,
                                x2,
                                y2,
                                color: Color::DarkGray,
                            });
                        }
                    }

                    // Draw all nodes as dots with selection highlighting
                    for (idx, (x, y)) in self.render_positions.iter().enumerate() {
                        let is_selected = Some(idx) == selected_idx;

                        // Determine base color
                        let color = if is_selected {
                            Color::White // Selected - bright white
                        } else if idx == 0 {
                            Color::Magenta // Target
                        } else if self.cached_mixer_nodes.contains(&idx) {
                            Color::Red // Mixer
                        } else {
                            Color::Green // Normal
                        };

                        // Draw selection halo first (larger circle behind)
                        if is_selected {
                            ctx.draw(&Circle {
                                x: *x,
                                y: *y,
                                radius: 3.0,
                                color: Color::Yellow,
                            });
                        }

                        // Draw the node
                        ctx.draw(&Circle {
                            x: *x,
                            y: *y,
                            radius: if is_selected { 2.0 } else { 1.0 },
                            color,
                        });
                    }
                }

                // Draw viewport rectangle (both modes)
                // Top edge
                ctx.draw(&CanvasLine {
                    x1: view_min_x,
                    y1: view_max_y,
                    x2: view_max_x,
                    y2: view_max_y,
                    color: Color::Yellow,
                });
                // Bottom edge
                ctx.draw(&CanvasLine {
                    x1: view_min_x,
                    y1: view_min_y,
                    x2: view_max_x,
                    y2: view_min_y,
                    color: Color::Yellow,
                });
                // Left edge
                ctx.draw(&CanvasLine {
                    x1: view_min_x,
                    y1: view_min_y,
                    x2: view_min_x,
                    y2: view_max_y,
                    color: Color::Yellow,
                });
                // Right edge
                ctx.draw(&CanvasLine {
                    x1: view_max_x,
                    y1: view_min_y,
                    x2: view_max_x,
                    y2: view_max_y,
                    color: Color::Yellow,
                });
            });

        f.render_widget(minimap, minimap_rect);
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
        self.connections
            .iter()
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
        let mixer_count = self
            .nodes
            .iter()
            .filter(|(_, node)| matches!(node.node_type, WalletNodeType::Mixer))
            .count();
        risk += (mixer_count as f64 * 10.0).min(20.0);

        risk.min(100.0)
    }

    /// Detect rapid transfer patterns (velocity analysis)
    /// Delegates to the forensics engine for burst detection
    pub fn detect_rapid_transfers(&self) -> Vec<RapidFlowAlert> {
        self.forensics.detect_rapid_transfers(self)
    }

    /// Detect circular flow patterns (A→B→C→A)
    /// Delegates to the forensics engine for proper cycle detection
    pub fn detect_circular_flows(&self) -> Vec<CircularFlow> {
        self.forensics.detect_circular_flows(self)
    }

    /// Classify wallet behavior based on transaction patterns (cached)
    /// Uses memoization to avoid recomputing for the same wallet on every frame
    pub fn classify_wallet_behavior(&mut self, wallet_idx: usize) -> WalletBehaviorType {
        // Check cache first - O(1) lookup
        if let Some(cached) = self.cached_wallet_behaviors.get(&wallet_idx) {
            return cached.clone();
        }

        // Compute and cache (use local implementation)
        let behavior = self.classify_wallet_behavior_impl(wallet_idx);
        self.cached_wallet_behaviors
            .insert(wallet_idx, behavior.clone());
        behavior
    }

    /// Internal implementation of wallet behavior classification
    fn classify_wallet_behavior_impl(&self, wallet_idx: usize) -> WalletBehaviorType {
        // Count incoming and outgoing edges
        let mut incoming = 0;
        let mut outgoing = 0;
        let mut total_in = 0.0;
        let mut total_out = 0.0;
        let mut counterparties: std::collections::HashMap<usize, usize> =
            std::collections::HashMap::new();
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

    /// Mark risk cache as dirty (needs recalculation)
    /// Call this when the graph structure changes (nodes/edges added)
    pub fn invalidate_risk_cache(&mut self) {
        self.risk_cache_dirty = true;
        self.cached_risk_explanation = None;
    }

    /// Get cached risk explanation, recalculating only if cache is dirty
    /// Uses incremental stats for O(1) updates when possible
    /// This is the method that should be used during rendering for performance
    pub fn get_cached_risk_explanation(&mut self) -> RiskExplanation {
        // If we have cached data and just need incremental update
        if !self.risk_cache_dirty && self.cached_risk_explanation.is_some() {
            return self.cached_risk_explanation.clone().unwrap();
        }

        // Decide if we need full recalc or can use incremental
        let needs_deep_recalc = self.incremental_risk.needs_deep_recalc
            || self.cached_risk_explanation.is_none()
            || self.incremental_risk.cached_circular_count == 0;

        let explanation = if needs_deep_recalc {
            // Full recalculation (expensive but accurate)
            let result = self.calculate_explainable_risk_impl();
            // Update cached values in incremental stats
            self.incremental_risk.cached_circular_count = self.detect_circular_flows().len();
            self.incremental_risk.cached_rapid_alerts = self
                .incremental_risk
                .detect_rapid_transfers_incremental(60, 5);
            self.incremental_risk.needs_deep_recalc = false;
            self.incremental_risk.updates_since_deep_recalc = 0;
            result
        } else {
            // Fast incremental calculation
            self.calculate_risk_incremental()
        };

        self.cached_risk_explanation = Some(explanation.clone());
        self.risk_cache_dirty = false;
        explanation
    }

    /// Calculate risk using pre-computed incremental stats (O(1) for most factors)
    fn calculate_risk_incremental(&self) -> RiskExplanation {
        let mut score: f64 = 0.0;
        let mut reasons = Vec::new();
        let mut alerts = Vec::new();

        // 1. Network complexity (O(1) - just check counts)
        let complexity_ratio = self.connections.len() as f64 / self.nodes.len().max(1) as f64;
        if complexity_ratio > 5.0 {
            score += 30.0;
            alerts.push(format!(
                "🚨 CRITICAL: Very high network complexity ({:.1} edges/node)",
                complexity_ratio
            ));
            reasons.push(format!(
                "Network complexity ratio of {:.1} indicates potential mixing/obfuscation",
                complexity_ratio
            ));
        } else if complexity_ratio > 3.0 {
            score += 15.0;
            reasons.push(format!(
                "Elevated network complexity ({:.1} edges/node)",
                complexity_ratio
            ));
        }

        // 2. Rapid transfer detection (use cached from incremental stats)
        for alert in self
            .incremental_risk
            .cached_rapid_alerts
            .iter()
            .filter(|a| matches!(a.severity, AlertSeverity::Critical | AlertSeverity::High))
        {
            score += 15.0;
            alerts.push(format!(
                "⚡ RAPID ACTIVITY: {} transfers in {}s ({:.2} {})",
                alert.transfer_count, alert.time_window_secs, alert.total_volume, alert.token
            ));
        }
        if !self.incremental_risk.cached_rapid_alerts.is_empty() {
            reasons.push(format!(
                "Detected {} rapid transfer burst(s)",
                self.incremental_risk.cached_rapid_alerts.len()
            ));
        }

        // 3. Circular flow detection (use cached count)
        let circular_count = self.incremental_risk.cached_circular_count;
        if circular_count > 0 {
            score += (circular_count.min(3) as f64) * 20.0;
            alerts.push(format!(
                "🔄 CIRCULAR FLOW: {} pattern(s) detected",
                circular_count
            ));
            reasons.push(format!(
                "Detected {} circular flow pattern(s) - potential wash trading",
                circular_count
            ));
        }

        // 4. Whale activity (O(1) - use pre-computed from incremental stats)
        if self.incremental_risk.whale_count > 5 {
            score += 15.0;
            reasons.push(format!(
                "High whale activity: {} large transfers totaling {:.2} SOL",
                self.incremental_risk.whale_count, self.incremental_risk.whale_total_volume
            ));
        }

        // 5. Token diversity (O(1) - check HashSet size)
        if self.incremental_risk.unique_tokens.len() > 20 {
            score += 10.0;
            reasons.push(format!(
                "Very high token diversity ({} tokens) may indicate portfolio mixing",
                self.incremental_risk.unique_tokens.len()
            ));
        }

        // 6. Hub wallet detection (O(1) - use cached)
        // (Skip for incremental - will be updated on deep recalc)

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

    /// Calculate explainable risk score with detailed reasoning (internal implementation)
    /// Delegates to forensics engine for comprehensive analysis
    fn calculate_explainable_risk_impl(&self) -> RiskExplanation {
        self.forensics.calculate_explainable_risk(self)
    }

    /// Get entity clusters with lazy computation (cached)
    /// Recomputes only when graph structure has changed
    pub fn get_entity_clusters(&mut self) -> &Vec<crate::utils::entity_clustering::EntityCluster> {
        if self.entity_clusters_dirty {
            self.detect_entity_clusters_impl();
            self.entity_clusters_dirty = false;
        }
        &self.entity_clusters
    }

    /// Detect entity clusters in the wallet graph (internal implementation)
    fn detect_entity_clusters_impl(&mut self) {
        use crate::utils::entity_clustering::{EntityClusterer, TransferMetadata, WalletMetadata};

        // Build metadata from graph
        let wallets: Vec<(String, WalletMetadata)> = self
            .nodes
            .iter()
            .map(|(addr, node)| {
                (
                    addr.clone(),
                    WalletMetadata {
                        address: addr.clone(),
                        first_seen: None, // TODO: extract from timestamps
                        transaction_count: self
                            .connections
                            .iter()
                            .filter(|(from, to, _)| {
                                self.nodes[*from].0 == *addr || self.nodes[*to].0 == *addr
                            })
                            .count(),
                        behavior_type: Some(format!("{:?}", node.node_type)),
                    },
                )
            })
            .collect();

        let connections: Vec<(usize, usize, TransferMetadata)> = self
            .connections
            .iter()
            .map(|(from, to, label)| {
                let timestamp = label
                    .timestamp
                    .as_ref()
                    .and_then(|ts| ts.parse::<u64>().ok());

                // Heuristic for initial funding: first transfer or large amount
                let is_initial_funding = label.amount > 1.0; // >1 SOL

                (
                    *from,
                    *to,
                    TransferMetadata {
                        amount: label.amount,
                        token: label.token.clone(),
                        timestamp,
                        is_initial_funding,
                        gas_price: None,
                    },
                )
            })
            .collect();

        // Run clustering
        let clusterer = EntityClusterer::default();
        self.entity_clusters = clusterer.detect_clusters(&wallets, &connections);

        // Build wallet-to-cluster mapping
        self.wallet_to_cluster.clear();
        for cluster in &self.entity_clusters {
            for wallet in &cluster.wallet_addresses {
                self.wallet_to_cluster
                    .insert(wallet.clone(), cluster.cluster_id);
            }
        }
    }

    /// Get cluster info for a wallet
    pub fn get_wallet_cluster(
        &self,
        wallet: &str,
    ) -> Option<&crate::utils::entity_clustering::EntityCluster> {
        self.wallet_to_cluster
            .get(wallet)
            .and_then(|cluster_id| self.entity_clusters.get(*cluster_id))
    }

    /// Get cluster color for visualization
    pub fn get_cluster_color(&self, wallet: &str) -> Option<(u8, u8, u8)> {
        self.wallet_to_cluster.get(wallet).map(|cluster_id| {
            crate::utils::entity_clustering::EntityClusterer::get_cluster_color(*cluster_id)
        })
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
                let in_degree = self
                    .connections
                    .iter()
                    .filter(|(_, to, _)| *to == node_idx)
                    .count();
                let out_degree = self
                    .connections
                    .iter()
                    .filter(|(from, _, _)| *from == node_idx)
                    .count();
                let total_degree = in_degree + out_degree;

                // Check total volume
                let total_volume: f64 = self
                    .connections
                    .iter()
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
                Some(format!(
                    "Cluster #{} ({} wallets)",
                    cluster.cluster_id,
                    cluster.wallet_addresses.len()
                ))
            } else {
                None
            };

            // Now add to trail
            if let Some(ref mut trail) = self.investigation_trail {
                trail.add_step(new_node_idx, address, risk_level, note);
            }
        }
    }

    /// Render investigation trail breadcrumb bar with statistics
    fn render_investigation_trail(&self, f: &mut Frame, area: Rect) {
        use ratatui::layout::{Constraint, Direction, Layout};
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

        let Some(ref trail) = self.investigation_trail else {
            return;
        };

        // Split area: trail on left (70%), stats on right (30%)
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(70), Constraint::Percentage(30)])
            .split(area);

        let trail_area = chunks[0];
        let stats_area = chunks[1];

        // === Trail Breadcrumb (Left Panel) ===
        let trail_mode_indicator = if self.trail_mode { "🔍" } else { "📍" };
        let trail_block = Block::default()
            .title(format!(
                " {} Trail ({}/{}) [T]=toggle [←→↑↓]=nav ",
                trail_mode_indicator,
                trail.current_index + 1,
                trail.steps.len()
            ))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(if self.trail_mode {
                Color::Rgb(30, 144, 255)
            } else {
                Color::Blue
            }));

        let trail_inner = trail_block.inner(trail_area);
        f.render_widget(trail_block, trail_area);

        // Smart truncation: show only last N steps that fit in available width
        let available_width = (trail_inner.width as usize).saturating_sub(10);
        let chars_per_step = 15;
        let max_visible_steps = (available_width / chars_per_step).max(2).min(20);

        // Build breadcrumb line
        let mut spans = Vec::new();

        let start_idx = if trail.steps.len() > max_visible_steps {
            spans.push(Span::styled("... → ", Style::default().fg(Color::DarkGray)));
            trail.steps.len() - max_visible_steps
        } else {
            0
        };

        for (i, step) in trail.steps.iter().enumerate().skip(start_idx) {
            let is_current = i == trail.current_index;

            let icon = match step.risk_level {
                RiskLevel::Critical => "🔴",
                RiskLevel::High => "🟠",
                RiskLevel::Medium => "🟡",
                RiskLevel::Low => "🟢",
            };

            let addr = if step.wallet_address.len() > 10 {
                format!(
                    "{}...{}",
                    &step.wallet_address[..4],
                    &step.wallet_address[step.wallet_address.len().saturating_sub(3)..]
                )
            } else {
                step.wallet_address.clone()
            };

            let style = if is_current {
                Style::default()
                    .fg(Color::White)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::DarkGray)
            };

            spans.push(Span::styled(format!("{} {}", icon, addr), style));

            if i < trail.steps.len() - 1 {
                spans.push(Span::styled(" → ", Style::default().fg(Color::DarkGray)));
            }
        }

        let trail_line = Line::from(spans);
        let trail_para = Paragraph::new(trail_line).wrap(Wrap { trim: true });
        f.render_widget(trail_para, trail_inner);

        // === Statistics Panel (Right) ===
        let stats_block = Block::default()
            .title(" 📊 Stats ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Cyan));

        let stats_inner = stats_block.inner(stats_area);
        f.render_widget(stats_block, stats_area);

        // Calculate trail statistics
        let trail_total = trail.total_amount();
        let trail_tokens = trail.unique_tokens();
        let trail_transfers = trail.steps.len().saturating_sub(1); // Edges = nodes - 1

        // Calculate graph-wide statistics
        let graph_total: f64 = self.connections.iter().map(|(_, _, l)| l.amount).sum();
        let graph_nodes = self.nodes.len();
        let graph_edges = self.connections.len();

        // Build statistics display
        let mut stats_spans = vec![];

        // Trail stats (compact format)
        stats_spans.push(Span::styled("Trail: ", Style::default().fg(Color::Yellow)));
        stats_spans.push(Span::styled(
            format!(
                "{:.1} ",
                if trail_total > 1000.0 {
                    format!("{:.1}K", trail_total / 1000.0)
                } else {
                    format!("{:.1}", trail_total)
                }
            ),
            Style::default().fg(Color::White),
        ));
        stats_spans.push(Span::styled(
            format!("{}tx ", trail_transfers),
            Style::default().fg(Color::Green),
        ));

        // Token list (abbreviated)
        let token_str = if trail_tokens.is_empty() {
            "".to_string()
        } else if trail_tokens.len() <= 2 {
            trail_tokens.join(",")
        } else {
            format!("{}+{}", trail_tokens[0], trail_tokens.len() - 1)
        };
        if !token_str.is_empty() {
            stats_spans.push(Span::styled(
                format!("[{}] ", token_str),
                Style::default().fg(Color::Magenta),
            ));
        }

        // Graph stats (second line if space permits)
        stats_spans.push(Span::styled(
            "│ Graph: ",
            Style::default().fg(Color::DarkGray),
        ));
        stats_spans.push(Span::styled(
            format!("{}w {}e", graph_nodes, graph_edges),
            Style::default().fg(Color::DarkGray),
        ));

        let stats_line = Line::from(stats_spans);
        let stats_para = Paragraph::new(stats_line).wrap(Wrap { trim: true });
        f.render_widget(stats_para, stats_inner);
    }

    /// Render detailed trail statistics panel (right side when trail mode active)
    fn render_trail_stats(&self, f: &mut Frame, area: Rect) {
        use ratatui::layout::{Constraint, Direction, Layout};
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Paragraph, Wrap};

        let Some(ref trail) = self.investigation_trail else {
            return;
        };

        // Main stats block
        let block = Block::default()
            .title(" 📊 Trail Stats ")
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Rgb(30, 144, 255)));

        let inner = block.inner(area);
        f.render_widget(block, area);

        // Split into sections
        let sections = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(6), // Trail summary (5 lines + 1 padding)
                Constraint::Length(6), // Graph summary
                Constraint::Min(4),    // Navigation hint
            ])
            .split(inner);

        // === TRAIL SECTION ===
        let trail_total = trail.total_amount();
        let trail_tokens = trail.unique_tokens();
        let trail_transfers = trail.steps.len().saturating_sub(1);
        let critical_count = trail
            .steps
            .iter()
            .filter(|s| matches!(s.risk_level, RiskLevel::Critical))
            .count();
        let high_count = trail
            .steps
            .iter()
            .filter(|s| matches!(s.risk_level, RiskLevel::High))
            .count();

        // Format token list
        let token_str = if trail_tokens.is_empty() {
            "-".to_string()
        } else if trail_tokens.len() <= 2 {
            trail_tokens.join(", ")
        } else {
            format!("{} +{}", trail_tokens[0], trail_tokens.len() - 1)
        };

        let trail_lines = vec![
            Line::from(vec![
                Span::styled(
                    "🔗 Trail ",
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD),
                ),
                Span::styled(
                    format!("'{}'", trail.branch_name),
                    Style::default().fg(Color::Cyan),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Steps: "),
                Span::styled(
                    format!("{}", trail.steps.len()),
                    Style::default().fg(Color::White),
                ),
                Span::raw(" │ Txs: "),
                Span::styled(
                    format!("{}", trail_transfers),
                    Style::default().fg(Color::Green),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Flow: "),
                Span::styled(
                    if trail_total > 1000.0 {
                        format!("{:.1}K", trail_total / 1000.0)
                    } else {
                        format!("{:.2}", trail_total)
                    },
                    Style::default()
                        .fg(Color::White)
                        .add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Tokens: "),
                Span::styled(token_str, Style::default().fg(Color::Magenta)),
            ]),
            Line::from(vec![
                Span::raw("  Risk: "),
                Span::styled(
                    format!("{}🔴", critical_count),
                    Style::default().fg(Color::Red),
                ),
                Span::raw(" "),
                Span::styled(
                    format!("{}🟠", high_count),
                    Style::default().fg(Color::Yellow),
                ),
            ]),
        ];

        let trail_para = Paragraph::new(trail_lines);
        f.render_widget(trail_para, sections[0]);

        // === GRAPH SECTION ===
        let graph_total: f64 = self.connections.iter().map(|(_, _, l)| l.amount).sum();
        let graph_nodes = self.nodes.len();
        let graph_edges = self.connections.len();
        let mixer_count = self.cached_mixer_nodes.len();

        // Get available edges for current node
        let available_edge_count = trail.available_edges.len();
        let current_edge_idx = trail.tentative_edge_idx.unwrap_or(0);

        let graph_lines = vec![
            Line::from(vec![Span::styled(
                "📈 Graph",
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            )]),
            Line::from(vec![
                Span::raw("  Wallets: "),
                Span::styled(
                    format!("{}", graph_nodes),
                    Style::default().fg(Color::White),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Edges: "),
                Span::styled(
                    format!("{}", graph_edges),
                    Style::default().fg(Color::White),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Total: "),
                Span::styled(
                    if graph_total > 1000000.0 {
                        format!("{:.1}M", graph_total / 1000000.0)
                    } else if graph_total > 1000.0 {
                        format!("{:.1}K", graph_total / 1000.0)
                    } else {
                        format!("{:.0}", graph_total)
                    },
                    Style::default().fg(Color::Green),
                ),
            ]),
            Line::from(vec![
                Span::raw("  Mixers: "),
                Span::styled(
                    format!("{}", mixer_count),
                    Style::default().fg(if mixer_count > 0 {
                        Color::Red
                    } else {
                        Color::Green
                    }),
                ),
            ]),
        ];

        let graph_para = Paragraph::new(graph_lines);
        f.render_widget(graph_para, sections[1]);

        // === NAVIGATION HINT ===
        let nav_lines = vec![
            Line::from(vec![Span::styled(
                "━━━ Navigate ━━━",
                Style::default().fg(Color::DarkGray),
            )]),
            Line::from(vec![Span::styled(
                format!(
                    "Edge {}/{}",
                    current_edge_idx + 1,
                    available_edge_count.max(1)
                ),
                Style::default().fg(Color::Rgb(68, 204, 255)),
            )]),
            Line::from(vec![
                Span::styled("↑↓", Style::default().fg(Color::Yellow)),
                Span::raw(" cycle edges"),
            ]),
            Line::from(vec![
                Span::styled("→", Style::default().fg(Color::Yellow)),
                Span::raw(" extend trail"),
            ]),
            Line::from(vec![
                Span::styled("←", Style::default().fg(Color::Yellow)),
                Span::raw(" retract trail"),
            ]),
        ];

        let nav_para = Paragraph::new(nav_lines);
        f.render_widget(nav_para, sections[2]);
    }
}

// ============================================================================
// GraphData trait implementation for WalletGraph
// This allows the forensics engine to analyze the graph
// ============================================================================

impl GraphData for WalletGraph {
    fn node_count(&self) -> usize {
        self.nodes.len()
    }

    fn edge_count(&self) -> usize {
        self.connections.len()
    }

    fn node_address(&self, idx: usize) -> Option<&str> {
        self.nodes.get(idx).map(|(addr, _)| addr.as_str())
    }

    fn node_type(&self, idx: usize) -> Option<&str> {
        self.nodes.get(idx).map(|(_, node)| match node.node_type {
            WalletNodeType::Target => "Target",
            WalletNodeType::Funding => "Funding",
            WalletNodeType::Recipient => "Recipient",
            WalletNodeType::DeFi => "DeFi",
            WalletNodeType::Token => "Token",
            WalletNodeType::Mixer => "Mixer",
        })
    }

    fn edges(&self) -> Vec<(usize, usize, f64, String, Option<String>)> {
        self.connections
            .iter()
            .map(|(from, to, label)| {
                (
                    *from,
                    *to,
                    label.amount,
                    label.token.clone(),
                    label.timestamp.clone(),
                )
            })
            .collect()
    }

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

    fn edge_data(&self, edge_idx: usize) -> Option<(usize, usize, f64, String, Option<String>)> {
        self.connections.get(edge_idx).map(|(from, to, label)| {
            (
                *from,
                *to,
                label.amount,
                label.token.clone(),
                label.timestamp.clone(),
            )
        })
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
    pub signature: Option<String>, // Transaction signature
}

impl TransferData {
    pub fn from_json(data: &serde_json::Value) -> Vec<Self> {
        let mut transfers = Vec::new();

        if let Some(data_array) = data
            .get("data")
            .and_then(|d| d.get("data"))
            .and_then(|d| d.as_array())
        {
            for item in data_array {
                if let (Some(from), Some(to), Some(token_amount), Some(token_symbol)) = (
                    item.get("from").and_then(|v| v.as_str()),
                    item.get("to").and_then(|v| v.as_str()),
                    item.get("tokenAmount").and_then(|v| v.as_str()),
                    item.get("tokenSymbol").and_then(|v| v.as_str()),
                ) {
                    let amount: f64 = token_amount.parse().unwrap_or(0.0);
                    let is_defi = item
                        .get("txType")
                        .and_then(|v| v.as_str())
                        .map(|t| t == "defi")
                        .unwrap_or(false);

                    let timestamp = item
                        .get("date")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());

                    let signature = item
                        .get("signature")
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
