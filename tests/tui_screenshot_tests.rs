//! TUI Screenshot Tests
//!
//! Visual regression tests for the research TUI components.
//! Run with `UPDATE_GOLDENS=1 cargo test tui_screenshot` to update golden images.
//!
//! These tests verify that the AI Insights panel and other TUI components
//! render correctly without visual regressions.
//!
//! Color assertion tests verify that risk levels display with correct colors:
//! - Critical: Red
//! - High: LightRed/Orange
//! - Medium: Yellow
//! - Low: Green

use osvm::utils::tui::screenshot_test::{TuiScreenshot, ScreenshotTestRunner};
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, BorderType, Paragraph, Wrap},
};

/// Helper to get the golden directory path
fn golden_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("golden")
        .join("tui")
}

/// Risk level for test data
#[derive(Debug, Clone, Copy)]
enum TestRiskLevel {
    Critical,
    High,
    Medium,
    Low,
}

/// Test risk data structure
struct TestRiskData {
    score: f64,
    level: TestRiskLevel,
    reasons: Vec<String>,
    alerts: Vec<String>,
}

/// Test behavior types
#[derive(Debug, Clone, Copy)]
enum TestBehavior {
    Bot,
    Exchange,
    Trader,
    Mixer,
    EOA,
    Contract,
    Dormant,
}

/// Render AI Insights panel mockup for screenshot testing
fn render_ai_insights_panel(f: &mut ratatui::Frame, area: Rect, risk: &TestRiskData) {
    let block = Block::default()
        .title(Span::styled(
            " 💡 AI Insights ",
            Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD)
        ))
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::DarkGray));

    let inner_area = block.inner(area);
    f.render_widget(block, area);

    let mut lines = Vec::new();

    // Display critical alerts first
    for alert in &risk.alerts {
        let color = if alert.contains("CRITICAL") || alert.contains("🚨") {
            Color::Red
        } else if alert.contains("RAPID") || alert.contains("⚡") {
            Color::LightRed
        } else if alert.contains("CIRCULAR") || alert.contains("🔄") {
            Color::Yellow
        } else {
            Color::LightYellow
        };

        lines.push(Line::from(vec![
            Span::styled(" ", Style::default()),
            Span::styled(alert.clone(), Style::default().fg(color).add_modifier(Modifier::BOLD)),
        ]));
    }

    // Add separator after alerts
    if !risk.alerts.is_empty() {
        lines.push(Line::from(vec![
            Span::styled(" ─────────────────────────────", Style::default().fg(Color::DarkGray)),
        ]));
    }

    // Show risk score
    let (risk_color, risk_icon) = match risk.level {
        TestRiskLevel::Critical => (Color::Red, "🔴"),
        TestRiskLevel::High => (Color::LightRed, "🟠"),
        TestRiskLevel::Medium => (Color::Yellow, "🟡"),
        TestRiskLevel::Low => (Color::Green, "🟢"),
    };

    lines.push(Line::from(vec![
        Span::styled(" ", Style::default()),
        Span::styled(
            format!("{} RISK SCORE: {:.0}/100 ({:?})", risk_icon, risk.score, risk.level),
            Style::default().fg(risk_color).add_modifier(Modifier::BOLD)
        ),
    ]));

    // Actionable context
    let risk_context = match risk.level {
        TestRiskLevel::Critical => "  ⚠ Action: Review mixer patterns, verify source wallets",
        TestRiskLevel::High => "  ⚠ Action: Verify counterparties, analyze timing patterns",
        TestRiskLevel::Medium => "  ℹ Action: Monitor for changes, track volume spikes",
        TestRiskLevel::Low => "  ✓ Status: Normal patterns observed",
    };
    lines.push(Line::from(vec![
        Span::styled(risk_context, Style::default().fg(Color::DarkGray)),
    ]));

    // Display reasons
    if !risk.reasons.is_empty() {
        lines.push(Line::from(vec![
            Span::styled("  Reasons:", Style::default().fg(Color::Cyan)),
        ]));
    }
    for (idx, reason) in risk.reasons.iter().take(5).enumerate() {
        let prefix = if idx == risk.reasons.len() - 1 || idx == 4 {
            "   └─ "
        } else {
            "   ├─ "
        };
        lines.push(Line::from(vec![
            Span::styled(prefix, Style::default().fg(Color::DarkGray)),
            Span::styled(reason.clone(), Style::default().fg(Color::Gray)),
        ]));
    }

    let widget = Paragraph::new(lines)
        .wrap(Wrap { trim: false });
    f.render_widget(widget, inner_area);
}

/// Render wallet behavior classification mockup
fn render_behavior_panel(f: &mut ratatui::Frame, area: Rect, behavior: TestBehavior) {
    let (behavior_icon, behavior_color) = match behavior {
        TestBehavior::Bot => ("🤖", Color::Yellow),
        TestBehavior::Exchange => ("🏦", Color::Cyan),
        TestBehavior::Trader => ("📈", Color::Green),
        TestBehavior::Mixer => ("🌀", Color::Red),
        TestBehavior::EOA => ("👤", Color::Blue),
        TestBehavior::Contract => ("📜", Color::Magenta),
        TestBehavior::Dormant => ("💤", Color::DarkGray),
    };

    let block = Block::default()
        .title(Span::styled(
            " Behavior ",
            Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
        ))
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded);

    let inner = block.inner(area);
    f.render_widget(block, area);

    let content = Paragraph::new(Line::from(vec![
        Span::styled(format!("{} ", behavior_icon), Style::default()),
        Span::styled(format!("{:?}", behavior), Style::default().fg(behavior_color)),
    ]));
    f.render_widget(content, inner);
}

// ============================================================================
// SCREENSHOT TESTS
// ============================================================================

#[test]
fn screenshot_ai_insights_low_risk() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 15.0,
        level: TestRiskLevel::Low,
        reasons: vec![
            "Normal transaction frequency".to_string(),
            "Standard token distribution".to_string(),
        ],
        alerts: vec![],
    };

    runner.test("ai_insights_low_risk", 60, 15, |f, area| {
        render_ai_insights_panel(f, area, &risk);
    }).unwrap();
}

#[test]
fn screenshot_ai_insights_medium_risk() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 45.0,
        level: TestRiskLevel::Medium,
        reasons: vec![
            "Elevated network complexity (3.2 edges/node)".to_string(),
            "Multiple DEX interactions detected".to_string(),
            "Token diversity above average (12 tokens)".to_string(),
        ],
        alerts: vec![],
    };

    runner.test("ai_insights_medium_risk", 60, 18, |f, area| {
        render_ai_insights_panel(f, area, &risk);
    }).unwrap();
}

#[test]
fn screenshot_ai_insights_high_risk() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 72.0,
        level: TestRiskLevel::High,
        reasons: vec![
            "High network complexity (5.8 edges/node)".to_string(),
            "Detected 3 rapid transfer burst(s)".to_string(),
            "High whale activity: 8 large transfers".to_string(),
            "3 hub wallets detected - potential coordination".to_string(),
        ],
        alerts: vec![
            "⚡ RAPID ACTIVITY: 15 transfers in 60s".to_string(),
        ],
    };

    runner.test("ai_insights_high_risk", 60, 20, |f, area| {
        render_ai_insights_panel(f, area, &risk);
    }).unwrap();
}

#[test]
fn screenshot_ai_insights_critical_risk() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 92.0,
        level: TestRiskLevel::Critical,
        reasons: vec![
            "Network complexity ratio of 8.5 indicates mixing".to_string(),
            "Detected 5 circular flow pattern(s)".to_string(),
            "Very high token diversity (28 tokens)".to_string(),
            "5 hub wallets - active coordination point".to_string(),
        ],
        alerts: vec![
            "🚨 CRITICAL: Very high network complexity (8.5 edges/node)".to_string(),
            "🔄 CIRCULAR FLOW: 4 wallets, 125.50 SOL total".to_string(),
            "🔴 MIXER DETECTED: Wallet exhibits mixing behavior".to_string(),
        ],
    };

    runner.test("ai_insights_critical_risk", 65, 25, |f, area| {
        render_ai_insights_panel(f, area, &risk);
    }).unwrap();
}

#[test]
fn screenshot_behavior_bot() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("behavior_bot", 25, 5, |f, area| {
        render_behavior_panel(f, area, TestBehavior::Bot);
    }).unwrap();
}

#[test]
fn screenshot_behavior_mixer() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("behavior_mixer", 25, 5, |f, area| {
        render_behavior_panel(f, area, TestBehavior::Mixer);
    }).unwrap();
}

#[test]
fn screenshot_behavior_exchange() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("behavior_exchange", 25, 5, |f, area| {
        render_behavior_panel(f, area, TestBehavior::Exchange);
    }).unwrap();
}

#[test]
fn screenshot_combined_dashboard_panel() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 58.0,
        level: TestRiskLevel::High,
        reasons: vec![
            "High network complexity (4.2 edges/node)".to_string(),
            "Multiple hub wallets detected".to_string(),
        ],
        alerts: vec![
            "⚡ RAPID ACTIVITY: 12 transfers in 30s".to_string(),
        ],
    };

    runner.test("combined_dashboard_panel", 80, 30, |f, area| {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(70),
                Constraint::Percentage(30),
            ])
            .split(area);

        // Left: AI Insights
        render_ai_insights_panel(f, chunks[0], &risk);

        // Right: Behavior + Stats
        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(5),
                Constraint::Min(0),
            ])
            .split(chunks[1]);

        render_behavior_panel(f, right_chunks[0], TestBehavior::Trader);

        // Stats panel
        let stats_block = Block::default()
            .title(" Stats ")
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded);
        let stats_inner = stats_block.inner(right_chunks[1]);
        f.render_widget(stats_block, right_chunks[1]);

        let stats = Paragraph::new(vec![
            Line::from("Nodes: 127"),
            Line::from("Edges: 534"),
            Line::from("Clusters: 8"),
            Line::from("Explored: 42%"),
        ]);
        f.render_widget(stats, stats_inner);
    }).unwrap();
}

#[test]
fn screenshot_empty_state() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    runner.test("empty_state", 60, 10, |f, area| {
        let block = Block::default()
            .title(Span::styled(
                " 💡 AI Insights ",
                Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD)
            ))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner = block.inner(area);
        f.render_widget(block, area);

        let content = Paragraph::new(vec![
            Line::from(""),
            Line::from(Span::styled(
                "  Initializing graph analysis...",
                Style::default().fg(Color::DarkGray)
            )),
        ]);
        f.render_widget(content, inner);
    }).unwrap();
}

// ============================================================================
// TOKEN VOLUME WIDGET TESTS
// ============================================================================

/// Test token data structure
struct TestTokenVolume {
    symbol: String,
    amount: f64,
}

/// Render token volume bars widget (matches render_volume_bars in app.rs)
fn render_token_volumes(f: &mut ratatui::Frame, area: Rect, tokens: &[TestTokenVolume]) {
    let colors = [Color::Yellow, Color::Green, Color::Cyan, Color::Magenta, Color::Blue, Color::Red];

    let block = Block::default()
        .title(Span::styled(" ◇ Token Volumes ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)))
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::DarkGray));

    let inner_area = block.inner(area);
    f.render_widget(block, area);

    let mut lines = Vec::new();

    if tokens.is_empty() {
        lines.push(Line::from(Span::styled("  Collecting data...", Style::default().fg(Color::DarkGray))));
    } else {
        let max_vol = tokens.iter().map(|v| v.amount).fold(0.0_f64, f64::max);
        let bar_width = (inner_area.width as usize).saturating_sub(20);

        for (i, vol) in tokens.iter().take(7).enumerate() {
            let bar_len = if max_vol > 0.0 {
                ((vol.amount / max_vol) * bar_width as f64) as usize
            } else { 0 };
            let bar = "▓".repeat(bar_len.max(1));
            let pad = "░".repeat(bar_width.saturating_sub(bar_len));

            let amount_str = if vol.amount >= 1_000_000.0 {
                format!("{:.1}M", vol.amount / 1_000_000.0)
            } else if vol.amount >= 1_000.0 {
                format!("{:.1}K", vol.amount / 1_000.0)
            } else {
                format!("{:.0}", vol.amount)
            };

            lines.push(Line::from(vec![
                Span::styled(format!("{:>6} ", vol.symbol), Style::default().fg(colors[i % colors.len()]).add_modifier(Modifier::BOLD)),
                Span::styled(bar, Style::default().fg(colors[i % colors.len()])),
                Span::styled(pad, Style::default().fg(Color::DarkGray)),
                Span::styled(format!(" {:>7}", amount_str), Style::default().fg(Color::White)),
            ]));
        }
    }

    let volume_widget = Paragraph::new(lines);
    f.render_widget(volume_widget, inner_area);
}

#[test]
fn screenshot_token_volumes_with_data() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let tokens = vec![
        TestTokenVolume { symbol: "SOL".to_string(), amount: 125000.0 },
        TestTokenVolume { symbol: "USDC".to_string(), amount: 85000.0 },
        TestTokenVolume { symbol: "USDT".to_string(), amount: 42000.0 },
        TestTokenVolume { symbol: "RAY".to_string(), amount: 15000.0 },
        TestTokenVolume { symbol: "JUP".to_string(), amount: 8500.0 },
    ];

    runner.test("token_volumes_with_data", 60, 12, |f, area| {
        render_token_volumes(f, area, &tokens);
    }).unwrap();
}

#[test]
fn screenshot_token_volumes_empty() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    runner.test("token_volumes_empty", 60, 6, |f, area| {
        render_token_volumes(f, area, &[]);
    }).unwrap();
}

#[test]
fn screenshot_token_volumes_large_amounts() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let tokens = vec![
        TestTokenVolume { symbol: "SOL".to_string(), amount: 5_250_000.0 },
        TestTokenVolume { symbol: "USDC".to_string(), amount: 1_800_000.0 },
        TestTokenVolume { symbol: "BONK".to_string(), amount: 850_000_000.0 },
    ];

    runner.test("token_volumes_large_amounts", 60, 8, |f, area| {
        render_token_volumes(f, area, &tokens);
    }).unwrap();
}

// ============================================================================
// SOL FLOW WIDGET TESTS
// ============================================================================

/// Render SOL flow widget (matches render_network_stats_panel SOL FLOW section)
fn render_sol_flow(f: &mut ratatui::Frame, area: Rect, total_in: f64, total_out: f64) {
    let net_flow = total_in - total_out;
    let flow_color = if net_flow > 0.0 { Color::Green }
                    else if net_flow < 0.0 { Color::Red }
                    else { Color::Yellow };
    let flow_symbol = if net_flow > 0.0 { "↑" }
                     else if net_flow < 0.0 { "↓" }
                     else { "=" };

    let flow_text = vec![
        Line::from(Span::styled(" 💸 SOL FLOW ", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD))),
        Line::from(vec![
            Span::styled(" Net: ", Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{} {:.4}", flow_symbol, net_flow.abs()), Style::default().fg(flow_color).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled(" In: ", Style::default().fg(Color::Green)),
            Span::styled(format!("{:.4}", total_in), Style::default().fg(Color::Green)),
        ]),
        Line::from(vec![
            Span::styled(" Out: ", Style::default().fg(Color::Red)),
            Span::styled(format!("{:.4}", total_out), Style::default().fg(Color::Red)),
        ]),
    ];
    let flow_widget = Paragraph::new(flow_text)
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Green)));
    f.render_widget(flow_widget, area);
}

#[test]
fn screenshot_sol_flow_positive() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("sol_flow_positive", 30, 8, |f, area| {
        render_sol_flow(f, area, 150.5, 45.25);
    }).unwrap();
}

#[test]
fn screenshot_sol_flow_negative() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("sol_flow_negative", 30, 8, |f, area| {
        render_sol_flow(f, area, 25.0, 180.75);
    }).unwrap();
}

#[test]
fn screenshot_sol_flow_balanced() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("sol_flow_balanced", 30, 8, |f, area| {
        render_sol_flow(f, area, 100.0, 100.0);
    }).unwrap();
}

// ============================================================================
// PROGRESS GAUGE WIDGET TESTS
// ============================================================================

/// Render progress gauge line
fn render_gauge_line(label: &str, value: usize, max: usize, color: Color) -> Line<'static> {
    let pct = ((value as f64 / max as f64) * 100.0).min(100.0);
    let bar_width = 20;
    let filled = ((pct / 100.0) * bar_width as f64) as usize;
    let empty = bar_width - filled;

    let bar = format!("{}{}", "█".repeat(filled), "░".repeat(empty));

    Line::from(vec![
        Span::styled(format!("{:<10}", label), Style::default().fg(Color::White)),
        Span::styled(bar, Style::default().fg(color)),
        Span::styled(format!(" {:>3}/{:<3}", value, max), Style::default().fg(Color::DarkGray)),
    ])
}

/// Render progress gauges panel
fn render_progress_gauges(f: &mut ratatui::Frame, area: Rect, wallets: usize, transfers: usize, depth: usize, elapsed_secs: u64) {
    let block = Block::default()
        .title(Span::styled(" ◈ Progress ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)))
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::DarkGray));

    let inner = block.inner(area);
    f.render_widget(block, area);

    let layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(0),
        ])
        .split(inner);

    let wallet_gauge = Paragraph::new(render_gauge_line("Wallets", wallets, 100, Color::Green));
    f.render_widget(wallet_gauge, layout[0]);

    let transfer_gauge = Paragraph::new(render_gauge_line("Transfers", transfers, 200, Color::Yellow));
    f.render_widget(transfer_gauge, layout[1]);

    let depth_gauge = Paragraph::new(render_gauge_line("Depth", depth, 5, Color::Magenta));
    f.render_widget(depth_gauge, layout[2]);

    let time_text = Paragraph::new(Line::from(vec![
        Span::styled("Time: ", Style::default().fg(Color::DarkGray)),
        Span::styled(format!("{}s", elapsed_secs), Style::default().fg(Color::Cyan)),
    ]));
    f.render_widget(time_text, layout[3]);
}

#[test]
fn screenshot_progress_early() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("progress_early", 50, 8, |f, area| {
        render_progress_gauges(f, area, 12, 28, 1, 15);
    }).unwrap();
}

#[test]
fn screenshot_progress_midway() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("progress_midway", 50, 8, |f, area| {
        render_progress_gauges(f, area, 55, 120, 3, 67);
    }).unwrap();
}

#[test]
fn screenshot_progress_complete() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("progress_complete", 50, 8, |f, area| {
        render_progress_gauges(f, area, 100, 200, 5, 180);
    }).unwrap();
}

// ============================================================================
// STATUS BAR WIDGET TESTS
// ============================================================================

/// Render status bar (matches render_btop_statusbar in app.rs)
fn render_status_bar(f: &mut ratatui::Frame, area: Rect, active_tab: usize, nodes: usize, edges: usize, logs: usize) {
    let line1 = Line::from(vec![
        Span::styled(" [", Style::default().fg(Color::DarkGray)),
        Span::styled("1", Style::default().fg(if active_tab == 0 { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Dashboard ", Style::default().fg(Color::White)),
        Span::styled("[", Style::default().fg(Color::DarkGray)),
        Span::styled("2", Style::default().fg(if active_tab == 1 { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Graph ", Style::default().fg(Color::White)),
        Span::styled("[", Style::default().fg(Color::DarkGray)),
        Span::styled("3", Style::default().fg(if active_tab == 2 { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Logs ", Style::default().fg(Color::White)),
        Span::styled("│ ", Style::default().fg(Color::DarkGray)),
        Span::styled(format!("{} ", nodes), Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
        Span::styled("wallets ", Style::default().fg(Color::DarkGray)),
        Span::styled(format!("{} ", edges), Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        Span::styled("transfers ", Style::default().fg(Color::DarkGray)),
        Span::styled(format!("{} ", logs), Style::default().fg(Color::Blue).add_modifier(Modifier::BOLD)),
        Span::styled("logs", Style::default().fg(Color::DarkGray)),
    ]);

    let line2 = Line::from(vec![
        Span::styled(" [", Style::default().fg(Color::DarkGray)),
        Span::styled("?/F1", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Help ", Style::default().fg(Color::DarkGray)),
        Span::styled("[", Style::default().fg(Color::DarkGray)),
        Span::styled("Tab", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Switch ", Style::default().fg(Color::DarkGray)),
        Span::styled("[", Style::default().fg(Color::DarkGray)),
        Span::styled("j/k", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Nav ", Style::default().fg(Color::DarkGray)),
        Span::styled("[", Style::default().fg(Color::DarkGray)),
        Span::styled("q/Esc", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
        Span::styled("]", Style::default().fg(Color::DarkGray)),
        Span::styled("Quit", Style::default().fg(Color::DarkGray)),
    ]);

    let status = Paragraph::new(vec![line1, line2]);
    f.render_widget(status, area);
}

#[test]
fn screenshot_status_bar_dashboard_active() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("status_bar_dashboard", 80, 2, |f, area| {
        render_status_bar(f, area, 0, 127, 534, 42);
    }).unwrap();
}

#[test]
fn screenshot_status_bar_graph_active() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("status_bar_graph", 80, 2, |f, area| {
        render_status_bar(f, area, 1, 85, 312, 28);
    }).unwrap();
}

#[test]
fn screenshot_status_bar_logs_active() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("status_bar_logs", 80, 2, |f, area| {
        render_status_bar(f, area, 2, 42, 156, 99);
    }).unwrap();
}

// ============================================================================
// TRANSFER FEED WIDGET TESTS
// ============================================================================

/// Test transaction data
struct TestTransaction {
    signature: String,
    success: bool,
    tx_type: String,
    timestamp: String,
    amount_sol: f64,
}

/// Render live transaction feed (matches render_transfer_feed in app.rs)
fn render_transfer_feed(f: &mut ratatui::Frame, area: Rect, transactions: &[TestTransaction]) {
    let block = Block::default()
        .title(Span::styled(" 🔴 LIVE Transactions ", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)))
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::Red));

    let inner_area = block.inner(area);
    f.render_widget(block, area);

    let mut items = Vec::new();

    if transactions.is_empty() {
        items.push(Line::from(Span::styled("  🔄 Fetching live transactions...", Style::default().fg(Color::Yellow))));
    } else {
        for tx in transactions.iter().take(5) {
            let (icon, color) = if tx.success {
                ("✓", Color::Green)
            } else {
                ("✗", Color::Red)
            };

            let type_icon = match tx.tx_type.as_str() {
                "Transfer" => "💸",
                "Contract" => "⚙️",
                _ => "📄",
            };

            items.push(Line::from(vec![
                Span::styled(format!(" {} ", icon), Style::default().fg(color).add_modifier(Modifier::BOLD)),
                Span::styled(type_icon, Style::default()),
                Span::styled(format!(" {}", &tx.signature[..16]), Style::default().fg(Color::Cyan)),
                Span::styled("...", Style::default().fg(Color::DarkGray)),
            ]));
            items.push(Line::from(vec![
                Span::styled("    ", Style::default()),
                Span::styled(format!("{} • ", tx.timestamp), Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{:.4} SOL", tx.amount_sol), Style::default().fg(Color::Yellow)),
            ]));
        }
    }

    let list = Paragraph::new(items);
    f.render_widget(list, inner_area);
}

#[test]
fn screenshot_transfer_feed_with_txs() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let transactions = vec![
        TestTransaction {
            signature: "5KtPn2r8vY3xN7mQ4jLw9aBcDefGhIjKlMnOpQrStUvWxYz".to_string(),
            success: true,
            tx_type: "Transfer".to_string(),
            timestamp: "12:34:56".to_string(),
            amount_sol: 2.5,
        },
        TestTransaction {
            signature: "3AbCdEfGhIjKlMnOpQrStUvWxYz1234567890AbCdEfGhIj".to_string(),
            success: true,
            tx_type: "Contract".to_string(),
            timestamp: "12:34:52".to_string(),
            amount_sol: 0.0125,
        },
        TestTransaction {
            signature: "9XyZaBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789AbCdEf".to_string(),
            success: false,
            tx_type: "Transfer".to_string(),
            timestamp: "12:34:48".to_string(),
            amount_sol: 15.0,
        },
    ];

    runner.test("transfer_feed_with_txs", 55, 14, |f, area| {
        render_transfer_feed(f, area, &transactions);
    }).unwrap();
}

#[test]
fn screenshot_transfer_feed_loading() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("transfer_feed_loading", 55, 6, |f, area| {
        render_transfer_feed(f, area, &[]);
    }).unwrap();
}

// ============================================================================
// TARGET WALLET PANEL TESTS
// ============================================================================

/// Render target wallet panel (matches TARGET WALLET section of render_network_stats_panel)
fn render_target_wallet(f: &mut ratatui::Frame, area: Rect, wallet: &str, nodes_explored: usize) {
    let truncated = if wallet.len() > 12 {
        format!("{}...{}", &wallet[..6], &wallet[wallet.len()-6..])
    } else {
        wallet.to_string()
    };

    let wallet_text = vec![
        Line::from(Span::styled(" 🎯 TARGET WALLET ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))),
        Line::from(vec![
            Span::styled(" Addr: ", Style::default().fg(Color::DarkGray)),
            Span::styled(truncated, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled(" Nodes: ", Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{}", nodes_explored), Style::default().fg(Color::Yellow)),
        ]),
    ];
    let wallet_widget = Paragraph::new(wallet_text)
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Cyan)));
    f.render_widget(wallet_widget, area);
}

#[test]
fn screenshot_target_wallet() {
    let runner = ScreenshotTestRunner::new(golden_dir());
    runner.test("target_wallet", 30, 6, |f, area| {
        render_target_wallet(f, area, "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1", 127);
    }).unwrap();
}

// ============================================================================
// FULL DASHBOARD LAYOUT TEST
// ============================================================================

#[test]
fn screenshot_full_dashboard() {
    let runner = ScreenshotTestRunner::new(golden_dir());

    let risk = TestRiskData {
        score: 65.0,
        level: TestRiskLevel::High,
        reasons: vec![
            "High network complexity (4.8 edges/node)".to_string(),
            "3 hub wallets detected".to_string(),
        ],
        alerts: vec![
            "⚡ RAPID ACTIVITY: 18 transfers in 45s".to_string(),
        ],
    };

    let tokens = vec![
        TestTokenVolume { symbol: "SOL".to_string(), amount: 85000.0 },
        TestTokenVolume { symbol: "USDC".to_string(), amount: 42000.0 },
        TestTokenVolume { symbol: "RAY".to_string(), amount: 15000.0 },
    ];

    runner.test("full_dashboard", 120, 40, |f, area| {
        // Main layout: header, body, status
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(6),  // Network stats row
                Constraint::Min(0),     // Main content
                Constraint::Length(2),  // Status bar
            ])
            .split(area);

        // Network stats row (4 panels)
        let stats_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
            ])
            .split(main_chunks[0]);

        render_target_wallet(f, stats_chunks[0], "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1", 127);
        render_sol_flow(f, stats_chunks[3], 150.5, 82.25);

        // Main content: left (AI Insights), right (tokens + progress)
        let content_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(60),
                Constraint::Percentage(40),
            ])
            .split(main_chunks[1]);

        render_ai_insights_panel(f, content_chunks[0], &risk);

        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(50),
                Constraint::Percentage(50),
            ])
            .split(content_chunks[1]);

        render_token_volumes(f, right_chunks[0], &tokens);
        render_progress_gauges(f, right_chunks[1], 72, 156, 3, 95);

        // Status bar
        render_status_bar(f, main_chunks[2], 0, 127, 534, 42);
    }).unwrap();
}

// ============================================================================
// COLOR ASSERTION TESTS - RISK LEVEL COLOR VALIDATION
// ============================================================================

/// Test that LOW risk displays with GREEN color
#[test]
fn color_test_low_risk_is_green() {
    let risk = TestRiskData {
        score: 15.0,
        level: TestRiskLevel::Low,
        reasons: vec!["Normal patterns".to_string()],
        alerts: vec![],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 60, 15);

    // Low risk should display as GREEN
    screenshot.assert_has_color(Color::Green)
        .expect("Low risk panel should contain GREEN");

    // The risk score icon "🟢" should be green (or the RISK SCORE text)
    screenshot.assert_text_has_color("RISK SCORE", Color::Green)
        .expect("'RISK SCORE' text should be green for low risk");

    // Should NOT have red (critical color)
    screenshot.assert_no_color(Color::Red)
        .expect("Low risk panel should NOT contain RED");
}

/// Test that MEDIUM risk displays with YELLOW color
#[test]
fn color_test_medium_risk_is_yellow() {
    let risk = TestRiskData {
        score: 45.0,
        level: TestRiskLevel::Medium,
        reasons: vec!["Elevated complexity".to_string()],
        alerts: vec![],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 60, 15);

    // Medium risk should display as YELLOW
    screenshot.assert_has_color(Color::Yellow)
        .expect("Medium risk panel should contain YELLOW");

    // The risk score text should be yellow
    screenshot.assert_text_has_color("RISK SCORE", Color::Yellow)
        .expect("'RISK SCORE' text should be yellow for medium risk");
}

/// Test that HIGH risk displays with LIGHT RED (orange) color
#[test]
fn color_test_high_risk_is_light_red() {
    let risk = TestRiskData {
        score: 72.0,
        level: TestRiskLevel::High,
        reasons: vec!["High network complexity".to_string()],
        alerts: vec!["⚡ RAPID ACTIVITY".to_string()],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 60, 18);

    // High risk should display as LIGHT_RED (orange)
    screenshot.assert_has_color(Color::LightRed)
        .expect("High risk panel should contain LIGHT_RED");

    // The risk score text should be light red
    screenshot.assert_text_has_color("RISK SCORE", Color::LightRed)
        .expect("'RISK SCORE' text should be light red for high risk");
}

/// Test that CRITICAL risk displays with RED color
#[test]
fn color_test_critical_risk_is_red() {
    let risk = TestRiskData {
        score: 92.0,
        level: TestRiskLevel::Critical,
        reasons: vec!["Mixer behavior detected".to_string()],
        alerts: vec![
            "🚨 CRITICAL: Very high complexity".to_string(),
            "🔄 CIRCULAR FLOW detected".to_string(),
        ],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 65, 20);

    // Critical risk should display as RED
    screenshot.assert_has_color(Color::Red)
        .expect("Critical risk panel should contain RED");

    // The risk score text should be red
    screenshot.assert_text_has_color("RISK SCORE", Color::Red)
        .expect("'RISK SCORE' text should be red for critical risk");
}

/// Test that alerts display with correct danger colors
#[test]
fn color_test_alerts_are_highlighted() {
    let risk = TestRiskData {
        score: 85.0,
        level: TestRiskLevel::Critical,
        reasons: vec![],
        alerts: vec![
            "🚨 CRITICAL: Network complexity".to_string(),
        ],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 65, 15);

    // Alert text should be RED
    screenshot.assert_text_has_color("CRITICAL", Color::Red)
        .expect("'CRITICAL' alert should be red");
}

/// Test SOL flow colors: positive = green, negative = red
#[test]
fn color_test_sol_flow_positive_is_green() {
    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_sol_flow(f, area, 150.0, 50.0); // Net positive
    }, 30, 8);

    // Positive flow should show green
    screenshot.assert_has_color(Color::Green)
        .expect("Positive SOL flow should have GREEN");
}

#[test]
fn color_test_sol_flow_negative_is_red() {
    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_sol_flow(f, area, 50.0, 150.0); // Net negative
    }, 30, 8);

    // Negative flow should show red
    screenshot.assert_has_color(Color::Red)
        .expect("Negative SOL flow should have RED");
}

/// Test that the color summary works correctly
#[test]
fn color_test_summary_detection() {
    let risk = TestRiskData {
        score: 92.0,
        level: TestRiskLevel::Critical,
        reasons: vec!["Test".to_string()],
        alerts: vec!["🚨 CRITICAL".to_string()],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 65, 15);

    let summary = screenshot.color_summary();

    // Critical panel should have red and magenta (title)
    assert!(summary.has_red, "Critical risk should have red");
    assert!(summary.has_magenta, "Panel title should have magenta");
}

/// Test fluent API for multiple color assertions
#[test]
fn color_test_fluent_api() {
    let risk = TestRiskData {
        score: 15.0,
        level: TestRiskLevel::Low,
        reasons: vec!["Normal".to_string()],
        alerts: vec![],
    };

    let screenshot = TuiScreenshot::capture_widget(|f, area| {
        render_ai_insights_panel(f, area, &risk);
    }, 60, 15);

    // Fluent assertions
    screenshot.assert_colors()
        .has_color(Color::Green)
        .and_then(|a| a.has_color(Color::Magenta)) // Title color
        .and_then(|a| a.no_color(Color::Red))
        .expect("Low risk should be green, have magenta title, no red");
}
