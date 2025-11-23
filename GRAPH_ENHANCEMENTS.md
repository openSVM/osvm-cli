# Graph Page Enhancements - Investigation Trail & Minimap

## Features Added

### 1. Investigation Trail (Breadcrumb Navigation) ✅

**Purpose**: Track the investigator's path through the graph, creating a narrative of the investigation.

**Data Structures**:
```rust
pub struct InvestigationTrail {
    pub steps: Vec<TrailStep>,
    pub current_index: usize,
}

pub struct TrailStep {
    pub node_index: usize,
    pub wallet_address: String,
    pub risk_level: RiskLevel,
    pub timestamp: std::time::Instant,
    pub note: Option<String>,
}
```

**Features**:
- Automatic trail recording when navigating nodes
- Breadcrumb-style history (like browser back/forward)
- Risk-level color coding for each step
- Exportable investigation summary
- Fork-able trails (truncates forward history on branch)

**Implementation Status**: ✅ Data structures complete

---

### 2. Minimap Overview Panel

**Purpose**: Provide global context for large graphs (100+ nodes).

**Features**:
- Small fixed-size overview (100x100px) in corner
- Shows all nodes as colored dots
- Current viewport as white rectangle
- Click-to-jump navigation
- Real-time pan/zoom sync

**Implementation Status**: ⏳ Next (rendering code needed)

---

## Usage

### Investigation Trail

**Automatic Tracking**:
```rust
// When user selects a node (Enter key)
if let Some(trail) = &mut self.investigation_trail {
    let risk_level = self.calculate_node_risk(selected_node);
    trail.add_step(
        selected_node,
        self.nodes[selected_node].0.clone(),
        risk_level,
        Some("Followed high-value transfer".to_string())
    );
}
```

**Navigation**:
```rust
// Back button (Backspace)
if let Some(trail) = &mut self.investigation_trail {
    if let Some(prev_node) = trail.go_back() {
        self.selection = SelectionMode::Node(prev_node);
    }
}

// Forward button (Shift+Backspace)
if let Some(trail) = &mut self.investigation_trail {
    if let Some(next_node) = trail.go_forward() {
        self.selection = SelectionMode::Node(next_node);
    }
}
```

**Export Trail**:
```rust
// Export investigation summary
if let Some(trail) = &self.investigation_trail {
    let summary = trail.export_summary();
    // Save to file or include in report
}
```

**Example Output**:
```
Investigation Trail:
  🟢 5Q544f...e4j1 - Low (Investigation start)
  🟡 ABC123...XYZ - Medium (Large transfer detected)
→ 🟠 DEF456...789 - High (Bot behavior detected)
  🔴 GHI789...012 - Critical (Mixer detected)
```

---

## Rendering Implementation (TODO)

### Trail Breadcrumb Bar

**Location**: Bottom of graph view (below canvas)

**Layout**:
```
┌─────────────────────────────────────────────────────────────┐
│                    Main Graph Canvas                        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
┌─ Trail ──────────────────────────────────────────────────────┐
│ 🟢 Target → 🟡 WalletA → 🟠 WalletB → 🔴 Mixer → 🏦 Exchange│
│              (2m ago)      (1m ago)    (30s ago)   (now)     │
└──────────────────────────────────────────────────────────────┘
```

**Implementation**:
```rust
fn render_investigation_trail(&self, f: &mut Frame, area: Rect) {
    if !self.show_trail {
        return;
    }

    let Some(ref trail) = self.investigation_trail else {
        return;
    };

    let block = Block::default()
        .title(" 🔍 Investigation Trail ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Blue));

    let inner = block.inner(area);
    f.render_widget(block, area);

    // Render breadcrumb steps
    let mut spans = Vec::new();

    for (i, step) in trail.steps.iter().enumerate() {
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
            format!("{}..{}", &step.wallet_address[..4], &step.wallet_address[step.wallet_address.len()-3..])
        } else {
            step.wallet_address.clone()
        };

        // Style
        let style = if is_current {
            Style::default().fg(Color::White).add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
        } else {
            Style::default().fg(Color::Gray)
        };

        spans.push(Span::styled(format!("{} {}", icon, addr), style));

        // Arrow separator
        if i < trail.steps.len() - 1 {
            spans.push(Span::styled(" → ", Style::default().fg(Color::DarkGray)));
        }
    }

    let line = Line::from(spans);
    let paragraph = Paragraph::new(line)
        .wrap(Wrap { trim: false });

    f.render_widget(paragraph, inner);
}
```

---

### Minimap Panel

**Location**: Top-right corner of graph view

**Layout**:
```
┌─────────────────────────────────┬──────────┐
│                                 │  🗺️ Map  │
│     Main Graph Canvas           │ ┌───────┐│
│                                 │ │□ □□ □ ││
│     [Detailed view]             │ │ □▣□ □ ││
│                                 │ │□ □ □□ ││
│                                 │ └───────┘│
└─────────────────────────────────┴──────────┘
```

**Implementation**:
```rust
fn render_minimap(&self, f: &mut Frame, area: Rect) {
    if !self.show_minimap || self.nodes.len() < 20 {
        return; // Only show for complex graphs
    }

    let minimap_size = Rect {
        x: area.x + area.width - 22,
        y: area.y + 1,
        width: 20,
        height: 12,
    };

    let block = Block::default()
        .title(" Map ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::DarkGray));

    let inner = block.inner(minimap_size);
    f.render_widget(block, minimap_size);

    // Calculate minimap bounds (all nodes)
    let (min_x, max_x, min_y, max_y) = self.calculate_graph_bounds();

    let canvas = Canvas::default()
        .x_bounds([min_x, max_x])
        .y_bounds([min_y, max_y])
        .paint(|ctx| {
            // Draw all nodes as small dots
            for (idx, _) in self.nodes.iter().enumerate() {
                if idx < self.node_positions.len() {
                    let (x, y) = self.node_positions[idx];

                    let color = if Some(idx) == self.selected_node() {
                        Color::White
                    } else if self.is_high_risk_node(idx) {
                        Color::Red
                    } else {
                        Color::Gray
                    };

                    ctx.print(x, y, "•", color);
                }
            }

            // Draw viewport rectangle
            let (cx, cy, zoom) = self.viewport;
            let vp_width = 200.0 / zoom;
            let vp_height = 100.0 / zoom;

            // Top edge
            ctx.draw(&ratatui::widgets::canvas::Line {
                x1: cx - vp_width / 2.0,
                y1: cy + vp_height / 2.0,
                x2: cx + vp_width / 2.0,
                y2: cy + vp_height / 2.0,
                color: Color::White,
            });
            // Right edge
            ctx.draw(&ratatui::widgets::canvas::Line {
                x1: cx + vp_width / 2.0,
                y1: cy + vp_height / 2.0,
                x2: cx + vp_width / 2.0,
                y2: cy - vp_height / 2.0,
                color: Color::White,
            });
            // Bottom edge
            ctx.draw(&ratatui::widgets::canvas::Line {
                x1: cx + vp_width / 2.0,
                y1: cy - vp_height / 2.0,
                x2: cx - vp_width / 2.0,
                y2: cy - vp_height / 2.0,
                color: Color::White,
            });
            // Left edge
            ctx.draw(&ratatui::widgets::canvas::Line {
                x1: cx - vp_width / 2.0,
                y1: cy - vp_height / 2.0,
                x2: cx - vp_width / 2.0,
                y2: cy + vp_height / 2.0,
                color: Color::White,
            });
        });

    f.render_widget(canvas, inner);
}

fn calculate_graph_bounds(&self) -> (f64, f64, f64, f64) {
    let mut min_x = f64::MAX;
    let mut max_x = f64::MIN;
    let mut min_y = f64::MAX;
    let mut max_y = f64::MIN;

    for (x, y) in &self.node_positions {
        min_x = min_x.min(*x);
        max_x = max_x.max(*x);
        min_y = min_y.min(*y);
        max_y = max_y.max(*y);
    }

    // Add padding
    let padding = 10.0;
    (min_x - padding, max_x + padding, min_y - padding, max_y + padding)
}

fn is_high_risk_node(&self, idx: usize) -> bool {
    // Calculate risk for this node (simplified)
    matches!(self.nodes[idx].1.node_type, WalletNodeType::Mixer)
}
```

---

## Keybindings

### Investigation Trail

| Key | Action |
|-----|--------|
| `Backspace` | Go back in trail |
| `Shift+Backspace` | Go forward in trail |
| `t` | Toggle trail visibility |
| `T` | Export trail summary |
| `n` | Add note to current step |

### Minimap

| Key | Action |
|-----|--------|
| `m` | Toggle minimap visibility |
| Click minimap | Jump to location (future) |

---

## Integration with Existing Features

### 1. Entity Clustering
When clusters are detected, trail shows cluster info:
```
→ 🟠 WalletB - High (Member of cluster #3, 8 wallets)
```

### 2. Risk Scoring
Trail automatically records risk level at each step:
```rust
let risk = self.calculate_explainable_risk();
trail.add_step(node, address, risk.level, Some("High risk detected"));
```

### 3. Export
Trail is included in investigation JSON export:
```json
{
  "investigation": {
    "trail": {
      "steps": [
        {"wallet": "5Q544f...", "risk": "Low", "note": "Start"},
        {"wallet": "ABC123...", "risk": "Medium", "note": "Large transfer"},
        {"wallet": "DEF456...", "risk": "High", "note": "Bot detected"}
      ]
    }
  }
}
```

---

## Benefits

### For Investigators:
1. **Navigation**: Easy to backtrack and explore alternate paths
2. **Context**: Always know where you've been and how you got here
3. **Documentation**: Trail becomes investigation report
4. **Sharing**: Export trail to share findings with team

### For Presentations:
1. **Storytelling**: "Let me show you the trail from target to exit point"
2. **Evidence**: Time-stamped breadcrumb trail is forensically defensible
3. **Clarity**: Non-technical stakeholders can follow the investigation logic

### For Analysis:
1. **Pattern Recognition**: See common investigation paths
2. **Optimization**: Identify dead ends and optimize future investigations
3. **Training**: New investigators can learn from expert trails

---

## Example Investigation Flow

```
1. Start at target wallet (🟢 Low risk)
   User: "Let's investigate this wallet"

2. Follow large transfer to WalletA (🟡 Medium risk)
   System adds: "Followed 500 SOL transfer"
   User: Enter on edge

3. Discover bot behavior at WalletB (🟠 High risk)
   System adds: "Bot detected (regular intervals)"
   User: Enter on node

4. Trace to mixer (🔴 Critical risk)
   System adds: "Mixer detected (obfuscation patterns)"
   User: Enter on node

5. Find exit to exchange (🏦)
   System adds: "Exchange detected (high volume)"
   User: Enter on node

Trail Summary:
→ 🟢 Target → 🟡 WalletA → 🟠 WalletB → 🔴 Mixer → 🏦 Binance
  (Start)   (500 SOL)   (Bot)      (Critical)  (Exit)

Report: "Investigation reveals coordinated bot network funneling funds through mixer to Binance exchange."
```

---

## Next Steps

### Immediate (This Session):
- [ ] Add node risk calculation helper
- [ ] Add trail update on node selection
- [ ] Implement trail rendering
- [ ] Implement minimap rendering
- [ ] Add keybindings (Backspace, t, m)

### Short Term:
- [ ] Trail notes input dialog
- [ ] Minimap click-to-jump
- [ ] Trail export to file
- [ ] Trail replay animation

### Long Term:
- [ ] Multi-investigator trails (collaboration)
- [ ] Trail templates (common patterns)
- [ ] AI-suggested next steps based on trail
- [ ] Trail comparison (find similar investigations)

---

## Code Changes Summary

**Files Modified**:
- `src/utils/tui/graph.rs` (+150 lines)
  - Added `InvestigationTrail` struct
  - Added `TrailStep` struct
  - Added trail methods (add_step, go_back, go_forward, export_summary)
  - Added fields to `WalletGraph` (investigation_trail, show_minimap, show_trail)

**Files To Modify** (rendering implementation):
- `src/utils/tui/graph.rs` (render methods)
  - Add `render_investigation_trail()`
  - Add `render_minimap()`
  - Update `render()` to call new methods
  - Add trail update on node selection

**Total Estimated Effort**: 2-3 hours

---

## Demo Script

```bash
# Start investigation
./target/release/osvm research <WALLET> --tui

# Switch to graph view (Tab 2)
# Notice minimap in top-right corner
# Notice trail at bottom starting with target wallet

# Navigate to a wallet (arrow keys + Enter)
# Trail updates: "Target → WalletA"

# Continue navigating
# Trail grows: "Target → WalletA → WalletB → Mixer"

# Press Backspace to go back
# Selection jumps to WalletB

# Press 't' to toggle trail visibility
# Press 'm' to toggle minimap

# Press 'T' to export trail
# Saved to: investigation_trail_<timestamp>.txt
```

---

**Status**: Data structures complete ✅
**Next**: Rendering implementation (2-3 hours)

The foundation is solid - now just need to add the visual layer!
