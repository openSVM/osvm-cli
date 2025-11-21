use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph, Tabs, Wrap},
    Frame, Terminal,
};
use std::io;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use super::graph::WalletGraph;

#[derive(Clone, Copy, PartialEq)]
pub enum TabIndex {
    AgentOutput = 0,
    WalletGraph = 1,
    Analytics = 2,
    Logs = 3,
}

pub struct OsvmApp {
    pub active_tab: TabIndex,
    pub agent_output: Arc<Mutex<Vec<String>>>,
    pub wallet_graph: WalletGraph,
    pub analytics_data: Vec<(String, f64)>,
    pub logs: Arc<Mutex<Vec<String>>>,
    pub should_quit: bool,
    pub iteration: usize,
    pub findings_count: usize,
    pub target_wallet: String,
}

impl OsvmApp {
    pub fn new(target_wallet: String) -> Self {
        Self {
            active_tab: TabIndex::AgentOutput,
            agent_output: Arc::new(Mutex::new(Vec::new())),
            wallet_graph: WalletGraph::new(target_wallet.clone()),
            analytics_data: Vec::new(),
            logs: Arc::new(Mutex::new(Vec::new())),
            should_quit: false,
            iteration: 0,
            findings_count: 0,
            target_wallet,
        }
    }

    pub fn add_log(&mut self, message: String) {
        let mut logs = self.logs.lock().unwrap();
        logs.push(format!("[{}] {}", chrono::Local::now().format("%H:%M:%S"), message));
        if logs.len() > 1000 {
            logs.drain(0..500); // Keep last 500 logs when limit reached
        }
    }

    pub fn add_agent_output(&mut self, message: String) {
        let mut output = self.agent_output.lock().unwrap();
        output.push(message);
        if output.len() > 500 {
            output.drain(0..250);
        }
    }

    pub fn run(&mut self) -> Result<()> {
        // Check if we have a TTY
        if !crossterm::tty::IsTty::is_tty(&io::stdin()) {
            anyhow::bail!("TUI requires an interactive terminal (TTY). Please run in a terminal, not a pipe or redirect.");
        }

        // Setup terminal
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;

        // Clear the terminal before starting
        terminal.clear()?;

        // Set up panic handler to restore terminal on crash
        let original_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |panic_info| {
            let _ = disable_raw_mode();
            let _ = execute!(io::stdout(), LeaveAlternateScreen, DisableMouseCapture);
            original_hook(panic_info);
        }));

        // Run event loop
        let res = self.event_loop(&mut terminal);

        // Restore terminal (always runs, even on error)
        let cleanup_result = (|| -> Result<()> {
            disable_raw_mode()?;
            execute!(
                terminal.backend_mut(),
                LeaveAlternateScreen,
                DisableMouseCapture
            )?;
            terminal.show_cursor()?;
            Ok(())
        })();

        // Restore original panic hook
        let _ = std::panic::take_hook();

        // Handle errors
        if let Err(err) = res {
            eprintln!("TUI Error: {:?}", err);
            return Err(err);
        }

        cleanup_result?;
        Ok(())
    }

    fn event_loop(&mut self, terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
        loop {
            terminal.draw(|f| self.ui(f))?;

            // Handle events with timeout
            if event::poll(Duration::from_millis(100))? {
                if let Event::Key(key) = event::read()? {
                    match key.code {
                        KeyCode::Char('q') | KeyCode::Esc => {
                            self.should_quit = true;
                        }
                        KeyCode::Char('1') => self.active_tab = TabIndex::AgentOutput,
                        KeyCode::Char('2') => self.active_tab = TabIndex::WalletGraph,
                        KeyCode::Char('3') => self.active_tab = TabIndex::Analytics,
                        KeyCode::Char('4') => self.active_tab = TabIndex::Logs,
                        KeyCode::Tab => self.next_tab(),
                        KeyCode::BackTab => self.previous_tab(),
                        _ => {}
                    }
                }
            }

            if self.should_quit {
                break;
            }
        }
        Ok(())
    }

    fn next_tab(&mut self) {
        self.active_tab = match self.active_tab {
            TabIndex::AgentOutput => TabIndex::WalletGraph,
            TabIndex::WalletGraph => TabIndex::Analytics,
            TabIndex::Analytics => TabIndex::Logs,
            TabIndex::Logs => TabIndex::AgentOutput,
        };
    }

    fn previous_tab(&mut self) {
        self.active_tab = match self.active_tab {
            TabIndex::AgentOutput => TabIndex::Logs,
            TabIndex::WalletGraph => TabIndex::AgentOutput,
            TabIndex::Analytics => TabIndex::WalletGraph,
            TabIndex::Logs => TabIndex::Analytics,
        };
    }

    fn ui(&mut self, f: &mut Frame) {
        let size = f.area();

        // Create main layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),  // Header
                Constraint::Length(3),  // Tabs
                Constraint::Min(0),     // Content
                Constraint::Length(3),  // Status bar
            ])
            .split(size);

        // Render header
        self.render_header(f, chunks[0]);

        // Render tabs
        self.render_tabs(f, chunks[1]);

        // Render active content
        match self.active_tab {
            TabIndex::AgentOutput => self.render_agent_output(f, chunks[2]),
            TabIndex::WalletGraph => self.render_wallet_graph(f, chunks[2]),
            TabIndex::Analytics => self.render_analytics(f, chunks[2]),
            TabIndex::Logs => self.render_logs(f, chunks[2]),
        }

        // Render status bar
        self.render_status_bar(f, chunks[3]);
    }

    fn render_header(&self, f: &mut Frame, area: Rect) {
        let title = Paragraph::new(vec![
            Line::from(vec![
                Span::styled("🚀 OSVM ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("Research Agent", Style::default().fg(Color::White)),
                Span::styled(" • ", Style::default().fg(Color::DarkGray)),
                Span::styled(&self.target_wallet[..12], Style::default().fg(Color::Yellow)),
                Span::styled("...", Style::default().fg(Color::DarkGray)),
            ]),
        ])
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Cyan)))
        .style(Style::default().bg(Color::Black));

        f.render_widget(title, area);
    }

    fn render_tabs(&self, f: &mut Frame, area: Rect) {
        let titles = vec!["[1] Agent Output", "[2] Wallet Graph", "[3] Analytics", "[4] Logs"];
        let tabs = Tabs::new(titles)
            .block(Block::default().borders(Borders::ALL).title("Navigation"))
            .select(self.active_tab as usize)
            .style(Style::default().fg(Color::White))
            .highlight_style(
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            );

        f.render_widget(tabs, area);
    }

    fn render_agent_output(&self, f: &mut Frame, area: Rect) {
        let output = self.agent_output.lock().unwrap();
        let items: Vec<ListItem> = output
            .iter()
            .rev() // Show newest first
            .take(100)
            .map(|line| {
                let style = if line.contains("✅") {
                    Style::default().fg(Color::Green)
                } else if line.contains("⚠️") || line.contains("ERROR") {
                    Style::default().fg(Color::Red)
                } else if line.contains("🔍") || line.contains("DEBUG") {
                    Style::default().fg(Color::Blue)
                } else {
                    Style::default().fg(Color::White)
                };
                ListItem::new(line.as_str()).style(style)
            })
            .collect();

        let list = List::new(items)
            .block(
                Block::default()
                    .title("Agent Output (Live)")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Green)),
            )
            .style(Style::default().fg(Color::White));

        f.render_widget(list, area);
    }

    fn render_wallet_graph(&mut self, f: &mut Frame, area: Rect) {
        // Split area for graph and legend
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(75), Constraint::Percentage(25)])
            .split(area);

        // Render graph
        self.wallet_graph.render(f, chunks[0]);

        // Render legend
        let legend_items = vec![
            "Graph Controls:",
            "",
            "↑/↓/←/→ - Navigate",
            "Space - Center view",
            "R - Reset zoom",
            "",
            "Legend:",
            "🔴 Target wallet",
            "🟢 Funding sources",
            "🔵 Recipients",
            "🟣 DEX/DeFi",
        ];

        let legend: Vec<ListItem> = legend_items
            .iter()
            .map(|&line| ListItem::new(line))
            .collect();

        let legend_widget = List::new(legend)
            .block(
                Block::default()
                    .title("Legend")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Magenta)),
            );

        f.render_widget(legend_widget, chunks[1]);
    }

    fn render_analytics(&self, f: &mut Frame, area: Rect) {
        // Create 2x2 grid for analytics
        let v_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        let top_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(v_chunks[0]);

        let bottom_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(v_chunks[1]);

        // Token volume chart (top left)
        self.render_volume_chart(f, top_chunks[0]);

        // Transfer timeline (top right)
        self.render_timeline(f, top_chunks[1]);

        // Wallet types pie (bottom left)
        self.render_wallet_types(f, bottom_chunks[0]);

        // Statistics (bottom right)
        self.render_statistics(f, bottom_chunks[1]);
    }

    fn render_volume_chart(&self, f: &mut Frame, area: Rect) {
        let text = Text::from(vec![
            Line::from("Token Volume (Last 24h)"),
            Line::from(""),
            Line::from(vec![
                Span::styled("SVMAI: ", Style::default().fg(Color::Yellow)),
                Span::raw("20.5M ████████████"),
            ]),
            Line::from(vec![
                Span::styled("SLONANA: ", Style::default().fg(Color::Green)),
                Span::raw("3.6M  ████"),
            ]),
            Line::from(vec![
                Span::styled("OVSM: ", Style::default().fg(Color::Cyan)),
                Span::raw("90K   █"),
            ]),
        ]);

        let widget = Paragraph::new(text)
            .block(
                Block::default()
                    .title("Token Volume")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Yellow)),
            );

        f.render_widget(widget, area);
    }

    fn render_timeline(& self, f: &mut Frame, area: Rect) {
        let text = Text::from(vec![
            Line::from("Transfer Activity Timeline"),
            Line::from(""),
            Line::from("Nov 15 ████████ (880K SLONANA)"),
            Line::from("Nov 02 ████████████ (20.5M SVMAI)"),
            Line::from("Oct 30 ████ (3.6M SLONANA)"),
        ]);

        let widget = Paragraph::new(text)
            .block(
                Block::default()
                    .title("Timeline")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Blue)),
            );

        f.render_widget(widget, area);
    }

    fn render_wallet_types(&self, f: &mut Frame, area: Rect) {
        let text = Text::from(vec![
            Line::from("Wallet Type Distribution"),
            Line::from(""),
            Line::from(vec![
                Span::styled("🔴 Target: ", Style::default().fg(Color::Red)),
                Span::raw("1 (4%)"),
            ]),
            Line::from(vec![
                Span::styled("🟢 Funding: ", Style::default().fg(Color::Green)),
                Span::raw("3 (13%)"),
            ]),
            Line::from(vec![
                Span::styled("🔵 Recipients: ", Style::default().fg(Color::Blue)),
                Span::raw("1 (4%)"),
            ]),
            Line::from(vec![
                Span::styled("🟣 DEX/DeFi: ", Style::default().fg(Color::Magenta)),
                Span::raw("18 (78%)"),
            ]),
        ]);

        let widget = Paragraph::new(text)
            .block(
                Block::default()
                    .title("Wallet Types")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Magenta)),
            );

        f.render_widget(widget, area);
    }

    fn render_statistics(&self, f: &mut Frame, area: Rect) {
        let text = Text::from(vec![
            Line::from("Investigation Statistics"),
            Line::from(""),
            Line::from(vec![
                Span::styled("Iteration: ", Style::default().fg(Color::Cyan)),
                Span::raw(format!("{}", self.iteration)),
            ]),
            Line::from(vec![
                Span::styled("Findings: ", Style::default().fg(Color::Green)),
                Span::raw(format!("{}", self.findings_count)),
            ]),
            Line::from(vec![
                Span::styled("Paths: ", Style::default().fg(Color::Yellow)),
                Span::raw(format!("{}", self.wallet_graph.node_count())),
            ]),
            Line::from(vec![
                Span::styled("Connections: ", Style::default().fg(Color::Blue)),
                Span::raw(format!("{}", self.wallet_graph.edge_count())),
            ]),
        ]);

        let widget = Paragraph::new(text)
            .block(
                Block::default()
                    .title("Statistics")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Cyan)),
            );

        f.render_widget(widget, area);
    }

    fn render_logs(&self, f: &mut Frame, area: Rect) {
        let logs = self.logs.lock().unwrap();
        let items: Vec<ListItem> = logs
            .iter()
            .rev()
            .take(100)
            .map(|line| ListItem::new(line.as_str()))
            .collect();

        let list = List::new(items)
            .block(
                Block::default()
                    .title(format!("Debug Logs ({} total)", logs.len()))
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::DarkGray)),
            )
            .style(Style::default().fg(Color::Gray));

        f.render_widget(list, area);
    }

    fn render_status_bar(&self, f: &mut Frame, area: Rect) {
        let status = Paragraph::new(Line::from(vec![
            Span::styled("Press ", Style::default().fg(Color::DarkGray)),
            Span::styled("q", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
            Span::styled(" to quit • ", Style::default().fg(Color::DarkGray)),
            Span::styled("Tab", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(" to switch tabs • ", Style::default().fg(Color::DarkGray)),
            Span::styled("1-4", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::styled(" direct tab access", Style::default().fg(Color::DarkGray)),
        ]))
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::DarkGray)))
        .style(Style::default().bg(Color::Black));

        f.render_widget(status, area);
    }
}
