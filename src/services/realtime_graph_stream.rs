//! Real-time Streaming Graph Visualization
//!
//! Progressive rendering system that updates the graph display in real-time
//! as new blockchain data arrives, without waiting for complete data fetch.

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use tokio::time::{interval, Duration};
use anyhow::Result;
use crossterm::{
    cursor,
    execute,
    terminal::{self, ClearType},
    style::{Color, Print, ResetColor, SetForegroundColor, Stylize},
    ExecutableCommand,
};
use std::io::{stdout, Write};

/// Events that trigger graph updates
#[derive(Debug, Clone)]
pub enum GraphUpdateEvent {
    NodeDiscovered {
        address: String,
        label: Option<String>,
        depth: usize,
        node_type: NodeType,
    },
    EdgeDiscovered {
        from: String,
        to: String,
        amount: f64,
        token: String,
        timestamp: Option<String>,
    },
    ClusterDetected {
        addresses: Vec<String>,
        cluster_type: ClusterType,
    },
    DataFetchComplete,
    Error(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Origin,
    Target,
    Exchange,
    Mixer,
    Burner,
    Normal,
    Suspicious,
}

#[derive(Debug, Clone)]
pub enum ClusterType {
    RoundTrip,
    Mixer,
    Exchange,
    Coordinated,
}

/// Real-time graph state that updates progressively
pub struct StreamingGraph {
    nodes: Arc<RwLock<HashMap<String, StreamNode>>>,
    edges: Arc<RwLock<Vec<StreamEdge>>>,
    clusters: Arc<RwLock<Vec<Cluster>>>,
    layout: Arc<RwLock<LayoutState>>,
    render_buffer: Arc<RwLock<RenderBuffer>>,
    pub update_channel: mpsc::UnboundedSender<GraphUpdateEvent>,
    stats: Arc<RwLock<StreamStats>>,
}

#[derive(Debug, Clone)]
struct StreamNode {
    address: String,
    label: Option<String>,
    node_type: NodeType,
    depth: usize,
    x: f32,
    y: f32,
    discovered_at: std::time::Instant,
    incoming_count: usize,
    outgoing_count: usize,
    total_volume: f64,
    is_new: bool,  // For animation
}

#[derive(Debug, Clone)]
struct StreamEdge {
    from: String,
    to: String,
    amount: f64,
    token: String,
    timestamp: Option<String>,
    is_new: bool,  // For animation
    animation_progress: f32,
}

#[derive(Debug, Clone)]
struct Cluster {
    addresses: HashSet<String>,
    cluster_type: ClusterType,
    center_x: f32,
    center_y: f32,
}

/// Layout state that adapts as new nodes arrive
struct LayoutState {
    viewport_width: u16,
    viewport_height: u16,
    camera_x: f32,
    camera_y: f32,
    zoom: f32,
    needs_relayout: bool,
    depth_positions: HashMap<usize, Vec<String>>,
}

/// Double-buffered rendering for smooth updates
struct RenderBuffer {
    front: Vec<Vec<char>>,
    back: Vec<Vec<char>>,
    width: usize,
    height: usize,
    dirty_regions: Vec<DirtyRegion>,
}

#[derive(Debug, Clone)]
struct DirtyRegion {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
}

/// Statistics for the streaming process
struct StreamStats {
    nodes_discovered: usize,
    edges_discovered: usize,
    clusters_found: usize,
    render_fps: f32,
    data_rate: f32,  // nodes per second
    start_time: std::time::Instant,
}

impl StreamingGraph {
    /// Create a new streaming graph visualization
    pub fn new() -> (Self, mpsc::UnboundedReceiver<GraphUpdateEvent>) {
        let (tx, rx) = mpsc::unbounded_channel();

        let (width, height) = terminal::size().unwrap_or((120, 40));

        let graph = Self {
            nodes: Arc::new(RwLock::new(HashMap::new())),
            edges: Arc::new(RwLock::new(Vec::new())),
            clusters: Arc::new(RwLock::new(Vec::new())),
            layout: Arc::new(RwLock::new(LayoutState {
                viewport_width: width,
                viewport_height: height,
                camera_x: 0.0,
                camera_y: 0.0,
                zoom: 1.0,
                needs_relayout: false,
                depth_positions: HashMap::new(),
            })),
            render_buffer: Arc::new(RwLock::new(RenderBuffer::new(width as usize, height as usize))),
            update_channel: tx,
            stats: Arc::new(RwLock::new(StreamStats {
                nodes_discovered: 0,
                edges_discovered: 0,
                clusters_found: 0,
                render_fps: 0.0,
                data_rate: 0.0,
                start_time: std::time::Instant::now(),
            })),
        };

        (graph, rx)
    }

    /// Start the real-time rendering loop
    pub async fn start_rendering(&self, mut event_rx: mpsc::UnboundedReceiver<GraphUpdateEvent>) -> Result<()> {
        // Setup terminal for real-time updates
        terminal::enable_raw_mode()?;
        execute!(stdout(), terminal::Clear(ClearType::All), cursor::Hide)?;

        // Clone Arc references for async tasks
        let nodes = Arc::clone(&self.nodes);
        let edges = Arc::clone(&self.edges);
        let clusters = Arc::clone(&self.clusters);
        let layout = Arc::clone(&self.layout);
        let render_buffer = Arc::clone(&self.render_buffer);
        let stats = Arc::clone(&self.stats);

        // Spawn render loop (60 FPS)
        let render_handle = tokio::spawn(async move {
            let mut render_interval = interval(Duration::from_millis(16)); // ~60 FPS
            let mut frame_count = 0u32;

            loop {
                render_interval.tick().await;

                // Perform incremental render
                if let Err(e) = Self::render_frame(&nodes, &edges, &clusters, &layout, &render_buffer, &stats, frame_count).await {
                    eprintln!("Render error: {}", e);
                    break;
                }

                frame_count = frame_count.wrapping_add(1);
            }
        });

        // Process incoming events
        let nodes_clone = Arc::clone(&self.nodes);
        let edges_clone = Arc::clone(&self.edges);
        let clusters_clone = Arc::clone(&self.clusters);
        let layout_clone = Arc::clone(&self.layout);
        let stats_clone = Arc::clone(&self.stats);

        let event_handle = tokio::spawn(async move {
            while let Some(event) = event_rx.recv().await {
                match event {
                    GraphUpdateEvent::NodeDiscovered { address, label, depth, node_type } => {
                        Self::handle_new_node(&nodes_clone, &layout_clone, &stats_clone, address, label, depth, node_type).await;
                    }
                    GraphUpdateEvent::EdgeDiscovered { from, to, amount, token, timestamp } => {
                        Self::handle_new_edge(&edges_clone, &nodes_clone, &stats_clone, from, to, amount, token, timestamp).await;
                    }
                    GraphUpdateEvent::ClusterDetected { addresses, cluster_type } => {
                        Self::handle_new_cluster(&clusters_clone, &nodes_clone, &stats_clone, addresses, cluster_type).await;
                    }
                    GraphUpdateEvent::DataFetchComplete => {
                        // Final layout optimization
                        layout_clone.write().await.needs_relayout = true;
                        break;
                    }
                    GraphUpdateEvent::Error(msg) => {
                        eprintln!("Stream error: {}", msg);
                    }
                }
            }
        });

        // Wait for both tasks
        tokio::select! {
            _ = render_handle => {}
            _ = event_handle => {}
        }

        // Cleanup terminal
        execute!(stdout(), cursor::Show, terminal::Clear(ClearType::All))?;
        terminal::disable_raw_mode()?;

        Ok(())
    }

    /// Handle new node discovery
    async fn handle_new_node(
        nodes: &Arc<RwLock<HashMap<String, StreamNode>>>,
        layout: &Arc<RwLock<LayoutState>>,
        stats: &Arc<RwLock<StreamStats>>,
        address: String,
        label: Option<String>,
        depth: usize,
        node_type: NodeType,
    ) {
        let mut nodes = nodes.write().await;
        let mut layout = layout.write().await;
        let mut stats = stats.write().await;

        // Calculate initial position based on depth
        let depth_nodes = layout.depth_positions.entry(depth).or_insert_with(Vec::new);
        let y_offset = depth_nodes.len() as f32 * 5.0;

        let node = StreamNode {
            address: address.clone(),
            label,
            node_type,
            depth,
            x: depth as f32 * 100.0,
            y: 20.0 + y_offset,
            discovered_at: std::time::Instant::now(),
            incoming_count: 0,
            outgoing_count: 0,
            total_volume: 0.0,
            is_new: true,
        };

        nodes.insert(address.clone(), node);
        depth_nodes.push(address);

        stats.nodes_discovered += 1;
        stats.data_rate = stats.nodes_discovered as f32 / stats.start_time.elapsed().as_secs_f32();

        // Mark for relayout after batch of updates
        layout.needs_relayout = true;
    }

    /// Handle new edge discovery
    async fn handle_new_edge(
        edges: &Arc<RwLock<Vec<StreamEdge>>>,
        nodes: &Arc<RwLock<HashMap<String, StreamNode>>>,
        stats: &Arc<RwLock<StreamStats>>,
        from: String,
        to: String,
        amount: f64,
        token: String,
        timestamp: Option<String>,
    ) {
        let mut edges = edges.write().await;
        let mut nodes = nodes.write().await;
        let mut stats = stats.write().await;

        // Create edge
        let edge = StreamEdge {
            from: from.clone(),
            to: to.clone(),
            amount,
            token,
            timestamp,
            is_new: true,
            animation_progress: 0.0,
        };

        edges.push(edge);

        // Update node statistics
        if let Some(from_node) = nodes.get_mut(&from) {
            from_node.outgoing_count += 1;
            from_node.total_volume += amount;
        }

        if let Some(to_node) = nodes.get_mut(&to) {
            to_node.incoming_count += 1;
            to_node.total_volume += amount;
        }

        stats.edges_discovered += 1;
    }

    /// Handle cluster detection
    async fn handle_new_cluster(
        clusters: &Arc<RwLock<Vec<Cluster>>>,
        nodes: &Arc<RwLock<HashMap<String, StreamNode>>>,
        stats: &Arc<RwLock<StreamStats>>,
        addresses: Vec<String>,
        cluster_type: ClusterType,
    ) {
        let mut clusters = clusters.write().await;
        let nodes = nodes.read().await;
        let mut stats = stats.write().await;

        // Calculate cluster center
        let mut center_x = 0.0;
        let mut center_y = 0.0;
        let mut count = 0;

        for addr in &addresses {
            if let Some(node) = nodes.get(addr) {
                center_x += node.x;
                center_y += node.y;
                count += 1;
            }
        }

        if count > 0 {
            center_x /= count as f32;
            center_y /= count as f32;
        }

        clusters.push(Cluster {
            addresses: addresses.into_iter().collect(),
            cluster_type,
            center_x,
            center_y,
        });

        stats.clusters_found += 1;
    }

    /// Render a single frame with incremental updates
    async fn render_frame(
        nodes: &Arc<RwLock<HashMap<String, StreamNode>>>,
        edges: &Arc<RwLock<Vec<StreamEdge>>>,
        clusters: &Arc<RwLock<Vec<Cluster>>>,
        layout: &Arc<RwLock<LayoutState>>,
        render_buffer: &Arc<RwLock<RenderBuffer>>,
        stats: &Arc<RwLock<StreamStats>>,
        frame: u32,
    ) -> Result<()> {
        let nodes = nodes.read().await;
        let mut edges = edges.write().await;
        let clusters = clusters.read().await;
        let layout = layout.read().await;
        let mut buffer = render_buffer.write().await;
        let stats = stats.read().await;

        // Clear back buffer for new frame
        buffer.clear_back();

        // Draw background grid for depth levels
        for depth in 0..10 {
            let x = (depth as f32 * 100.0 * layout.zoom) as usize;
            if x < buffer.width {
                for y in 0..buffer.height {
                    let ch = if y % 5 == 0 { '·' } else { ' ' };
                    buffer.put_back(x, y, ch, Color::DarkGrey);
                }
            }
        }

        // Draw edges with animation
        for edge in edges.iter_mut() {
            if let (Some(from_node), Some(to_node)) = (nodes.get(&edge.from), nodes.get(&edge.to)) {
                // Animate new edges
                if edge.is_new {
                    edge.animation_progress = (edge.animation_progress + 0.05).min(1.0);
                    if edge.animation_progress >= 1.0 {
                        edge.is_new = false;
                    }
                }

                // Calculate edge positions
                let from_x = (from_node.x * layout.zoom) as usize;
                let from_y = (from_node.y * layout.zoom) as usize;
                let to_x = (to_node.x * layout.zoom) as usize;
                let to_y = (to_node.y * layout.zoom) as usize;

                // Draw animated flow line
                Self::draw_animated_edge(&mut buffer, from_x, from_y, to_x, to_y,
                                        edge.amount, edge.animation_progress, frame);
            }
        }

        // Draw clusters
        for cluster in clusters.iter() {
            let x = (cluster.center_x * layout.zoom) as usize;
            let y = (cluster.center_y * layout.zoom) as usize;

            Self::draw_cluster(&mut buffer, x, y, &cluster.cluster_type, cluster.addresses.len());
        }

        // Draw nodes
        for node in nodes.values() {
            let x = (node.x * layout.zoom) as usize;
            let y = (node.y * layout.zoom) as usize;

            // Pulse animation for new nodes
            let pulse = if node.is_new {
                let age = node.discovered_at.elapsed().as_secs_f32();
                if age < 1.0 {
                    (age * std::f32::consts::PI * 2.0).sin().abs()
                } else {
                    1.0
                }
            } else {
                1.0
            };

            Self::draw_node(&mut buffer, x, y, node, pulse);
        }

        // Draw stats overlay
        Self::draw_stats_overlay(&mut buffer, &stats);

        // Calculate and render only dirty regions
        let dirty_regions = buffer.calculate_dirty_regions();
        Self::render_dirty_regions(&buffer, &dirty_regions)?;

        // Swap buffers
        buffer.swap();

        Ok(())
    }

    /// Draw an animated edge with flow effect
    fn draw_animated_edge(
        buffer: &mut RenderBuffer,
        from_x: usize,
        from_y: usize,
        to_x: usize,
        to_y: usize,
        volume: f64,
        progress: f32,
        frame: u32,
    ) {
        // Choose line style based on volume
        let (line_char, color) = if volume > 1_000_000.0 {
            ('═', Color::Red)
        } else if volume > 100_000.0 {
            ('━', Color::Yellow)
        } else {
            ('─', Color::Blue)
        };

        // Draw line with moving dots for flow animation
        let dx = (to_x as i32 - from_x as i32).abs();
        let dy = (to_y as i32 - from_y as i32).abs();
        let steps = dx.max(dy) as usize;

        for i in 0..=(steps as f32 * progress) as usize {
            let t = i as f32 / steps as f32;
            let x = (from_x as f32 + (to_x as f32 - from_x as f32) * t) as usize;
            let y = (from_y as f32 + (to_y as f32 - from_y as f32) * t) as usize;

            // Animated dots along the line
            let is_dot = (i + frame as usize / 4) % 8 == 0;
            let ch = if is_dot { '●' } else { line_char };

            buffer.put_back(x, y, ch, color);
        }

        // Arrow head at the end
        if progress >= 1.0 {
            buffer.put_back(to_x, to_y, '→', color);
        }
    }

    /// Draw a node with appropriate styling
    fn draw_node(buffer: &mut RenderBuffer, x: usize, y: usize, node: &StreamNode, pulse: f32) {
        let (icon, color) = match node.node_type {
            NodeType::Origin => ('◆', Color::Green),
            NodeType::Target => ('◉', Color::Red),
            NodeType::Exchange => ('🏦', Color::Cyan),
            NodeType::Mixer => ('🌀', Color::Magenta),
            NodeType::Burner => ('🔥', Color::DarkRed),
            NodeType::Suspicious => ('⚠', Color::Yellow),
            NodeType::Normal => ('○', Color::White),
        };

        // Draw node box
        let width = 50;
        let height = 3;

        // Adjust brightness based on pulse
        let final_color = if pulse < 1.0 {
            Color::Rgb {
                r: (255.0 * pulse) as u8,
                g: (255.0 * pulse) as u8,
                b: (255.0 * pulse) as u8,
            }
        } else {
            color
        };

        // Top border
        buffer.put_back(x, y, '┌', final_color);
        for i in 1..width-1 {
            buffer.put_back(x + i, y, '─', final_color);
        }
        buffer.put_back(x + width - 1, y, '┐', final_color);

        // Content
        let addr_display = format!("{} {}...{}",
            icon,
            &node.address[..6],
            &node.address[node.address.len().saturating_sub(4)..]);
        buffer.put_string_back(x + 1, y + 1, &addr_display, final_color);

        // Stats
        let stats = format!("↓{} ↑{} ${:.1}K",
            node.incoming_count,
            node.outgoing_count,
            node.total_volume / 1000.0);
        buffer.put_string_back(x + width - stats.len() - 1, y + 1, &stats, Color::DarkGrey);

        // Bottom border
        buffer.put_back(x, y + height - 1, '└', final_color);
        for i in 1..width-1 {
            buffer.put_back(x + i, y + height - 1, '─', final_color);
        }
        buffer.put_back(x + width - 1, y + height - 1, '┘', final_color);
    }

    /// Draw cluster visualization
    fn draw_cluster(buffer: &mut RenderBuffer, x: usize, y: usize, cluster_type: &ClusterType, size: usize) {
        let (label, color) = match cluster_type {
            ClusterType::RoundTrip => ("ROUND-TRIP", Color::Red),
            ClusterType::Mixer => ("MIXER", Color::Magenta),
            ClusterType::Exchange => ("EXCHANGE", Color::Cyan),
            ClusterType::Coordinated => ("COORDINATED", Color::Yellow),
        };

        // Draw cluster boundary
        let radius = (size as f32).sqrt() as usize * 3;

        for angle in 0..360 {
            let rad = angle as f32 * std::f32::consts::PI / 180.0;
            let cx = x as i32 + (radius as f32 * rad.cos()) as i32;
            let cy = y as i32 + (radius as f32 * rad.sin() / 2.0) as i32; // Ellipse

            if cx >= 0 && cy >= 0 {
                buffer.put_back(cx as usize, cy as usize, '◦', color);
            }
        }

        buffer.put_string_back(x.saturating_sub(label.len() / 2), y, label, color);
    }

    /// Draw stats overlay
    fn draw_stats_overlay(buffer: &mut RenderBuffer, stats: &StreamStats) {
        let lines = vec![
            format!("Nodes: {} ({:.1}/s)", stats.nodes_discovered, stats.data_rate),
            format!("Edges: {}", stats.edges_discovered),
            format!("Clusters: {}", stats.clusters_found),
            format!("Time: {:.1}s", stats.start_time.elapsed().as_secs_f32()),
        ];

        // Draw in top-right corner
        let x = buffer.width.saturating_sub(25);
        for (i, line) in lines.iter().enumerate() {
            buffer.put_string_back(x, i + 1, line, Color::DarkGreen);
        }
    }

    /// Render only the dirty regions to terminal
    fn render_dirty_regions(buffer: &RenderBuffer, regions: &[DirtyRegion]) -> Result<()> {
        let mut stdout = stdout();

        for region in regions {
            for y in region.y..region.y + region.height {
                execute!(stdout, cursor::MoveTo(region.x as u16, y as u16))?;

                for x in region.x..region.x + region.width {
                    let ch = buffer.get_front(x, y);
                    print!("{}", ch);
                }
            }
        }

        stdout.flush()?;
        Ok(())
    }
}

impl RenderBuffer {
    fn new(width: usize, height: usize) -> Self {
        Self {
            front: vec![vec![' '; width]; height],
            back: vec![vec![' '; width]; height],
            width,
            height,
            dirty_regions: Vec::new(),
        }
    }

    fn clear_back(&mut self) {
        for row in &mut self.back {
            row.fill(' ');
        }
    }

    fn put_back(&mut self, x: usize, y: usize, ch: char, _color: Color) {
        if x < self.width && y < self.height {
            self.back[y][x] = ch;
        }
    }

    fn put_string_back(&mut self, x: usize, y: usize, s: &str, color: Color) {
        for (i, ch) in s.chars().enumerate() {
            self.put_back(x + i, y, ch, color);
        }
    }

    fn get_front(&self, x: usize, y: usize) -> char {
        if x < self.width && y < self.height {
            self.front[y][x]
        } else {
            ' '
        }
    }

    fn calculate_dirty_regions(&mut self) -> Vec<DirtyRegion> {
        self.dirty_regions.clear();

        // Simple approach: check entire screen for changes
        // Could be optimized to merge adjacent regions
        for y in 0..self.height {
            let mut region_start = None;

            for x in 0..self.width {
                let changed = self.front[y][x] != self.back[y][x];

                match (changed, region_start) {
                    (true, None) => region_start = Some(x),
                    (false, Some(start)) => {
                        self.dirty_regions.push(DirtyRegion {
                            x: start,
                            y,
                            width: x - start,
                            height: 1,
                        });
                        region_start = None;
                    }
                    _ => {}
                }
            }

            // Handle region extending to end of line
            if let Some(start) = region_start {
                self.dirty_regions.push(DirtyRegion {
                    x: start,
                    y,
                    width: self.width - start,
                    height: 1,
                });
            }
        }

        self.dirty_regions.clone()
    }

    fn swap(&mut self) {
        std::mem::swap(&mut self.front, &mut self.back);
    }
}

/// Example usage: Stream wallet investigation data
pub async fn demo_streaming_investigation(wallet: &str) -> Result<()> {
    let (graph, rx) = StreamingGraph::new();
    let tx = graph.update_channel.clone();
    let wallet_owned = wallet.to_string(); // Clone wallet for the spawned task

    // Start rendering in background
    let graph_handle = tokio::spawn(async move {
        graph.start_rendering(rx).await
    });

    // Simulate progressive data discovery
    tokio::spawn(async move {
        // Discover origin wallet
        tx.send(GraphUpdateEvent::NodeDiscovered {
            address: wallet_owned.clone(),
            label: Some("Target".to_string()),
            depth: 0,
            node_type: NodeType::Target,
        }).unwrap();

        tokio::time::sleep(Duration::from_millis(500)).await;

        // Simulate discovering connected wallets
        for i in 1..10 {
            let addr = format!("wallet_{}", i);
            tx.send(GraphUpdateEvent::NodeDiscovered {
                address: addr.clone(),
                label: None,
                depth: 1,
                node_type: NodeType::Normal,
            }).unwrap();

            tokio::time::sleep(Duration::from_millis(200)).await;

            // Add edge
            tx.send(GraphUpdateEvent::EdgeDiscovered {
                from: wallet_owned.clone(),
                to: addr,
                amount: 100000.0 * i as f64,
                token: "SOL".to_string(),
                timestamp: None,
            }).unwrap();
        }

        // Detect cluster
        tokio::time::sleep(Duration::from_secs(1)).await;
        tx.send(GraphUpdateEvent::ClusterDetected {
            addresses: vec!["wallet_1".to_string(), "wallet_2".to_string()],
            cluster_type: ClusterType::RoundTrip,
        }).unwrap();

        // Signal completion
        tokio::time::sleep(Duration::from_secs(2)).await;
        tx.send(GraphUpdateEvent::DataFetchComplete).unwrap();
    });

    graph_handle.await??;

    Ok(())
}