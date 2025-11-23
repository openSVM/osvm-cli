use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect, Alignment},
    style::{Color, Modifier, Style},
    symbols,
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph, Sparkline, Tabs, Wrap, Gauge, BorderType, Clear, BarChart, LineGauge},
    Frame, Terminal,
};
use std::io;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use super::graph::{WalletGraph, GraphInput};

#[derive(Clone, Copy, PartialEq)]
pub enum TabIndex {
    Dashboard = 0,
    Graph = 1,
    Logs = 2,
    SearchResults = 3,
}

#[derive(Clone, Copy, PartialEq)]
pub enum FilterMode {
    All,
    Errors,
    Success,
    Transfers,
}

/// Token volume entry for analytics
#[derive(Clone, Debug, serde::Serialize)]
pub struct TokenVolume {
    pub symbol: String,
    pub amount: f64,
}

/// Transfer event for timeline
#[derive(Clone, Debug, serde::Serialize)]
pub struct TransferEvent {
    pub timestamp: String,
    pub amount: f64,
    pub token: String,
    pub direction: String, // "IN" or "OUT"
}

pub struct OsvmApp {
    pub active_tab: TabIndex,
    pub agent_output: Arc<Mutex<Vec<String>>>,
    pub wallet_graph: Arc<Mutex<WalletGraph>>,
    pub token_volumes: Arc<Mutex<Vec<TokenVolume>>>,
    pub transfer_events: Arc<Mutex<Vec<TransferEvent>>>,
    pub logs: Arc<Mutex<Vec<String>>>,
    pub should_quit: bool,
    pub show_help: bool,
    pub iteration: usize,
    pub findings_count: usize,
    pub target_wallet: String,
    pub status: Arc<Mutex<String>>,
    pub phase: Arc<Mutex<String>>,
    // btop-style activity history (60 data points for sparklines)
    pub activity_history: Vec<u64>,
    pub transfer_history: Vec<u64>,
    pub sol_flow_history: Vec<u64>,
    // Exploration stats
    pub depth_reached: usize,
    pub wallets_explored: usize,
    pub start_time: std::time::Instant,
    // Additional btop-style metrics
    pub total_sol_in: f64,
    pub total_sol_out: f64,
    pub api_calls: usize,
    pub rpc_latency_ms: u64,
    // Scroll position for logs
    pub log_scroll: usize,
    pub output_scroll: usize,
    pub help_scroll: usize,
    // AI insights
    pub ai_insights: Arc<Mutex<Vec<String>>>,
    // Search/Filter
    pub search_active: bool,
    pub search_query: String,
    pub filter_mode: FilterMode,
    // Global search modal
    pub global_search_active: bool,
    pub global_search_query: String,
    pub search_history: Vec<String>,  // Last 5 searches
    pub search_suggestions: Vec<SearchSuggestion>,
    pub selected_suggestion: usize,
    pub search_loading: bool,
    pub search_result: Option<SearchResult>,
    pub search_error: Option<String>,
    // Search results tab data
    pub search_results_data: Arc<Mutex<SearchResultsData>>,
    pub search_results_scroll: usize,
    // Real-time network stats
    pub network_stats: Arc<Mutex<NetworkStats>>,
    pub live_transactions: Arc<Mutex<Vec<LiveTransaction>>>,
    pub last_refresh: std::time::Instant,
}

/// Real-time network statistics
#[derive(Clone, Debug, Default)]
pub struct NetworkStats {
    pub current_slot: u64,
    pub current_epoch: u64,
    pub tps: f64,
    pub block_time_ms: u64,
    pub total_transactions: u64,
    pub active_validators: usize,
    pub health: String,
}

/// Live transaction feed entry
#[derive(Clone, Debug)]
pub struct LiveTransaction {
    pub signature: String,
    pub timestamp: String,
    pub amount_sol: f64,
    pub success: bool,
    pub tx_type: String, // "Transfer", "Swap", "NFT Mint", etc.
}

#[derive(Clone, Debug, Default)]
pub struct SearchResultsData {
    pub query: String,
    pub entity_type: String,
    pub wallets_found: Vec<WalletMatch>,
    pub transactions_found: Vec<TransactionMatch>,
    pub tokens_found: Vec<TokenMatch>,
    pub total_matches: usize,
    pub search_timestamp: String,
}

#[derive(Clone, Debug)]
pub struct WalletMatch {
    pub address: String,
    pub balance_sol: f64,
    pub transfer_count: usize,
    pub last_activity: String,
    pub match_reason: String,
}

#[derive(Clone, Debug)]
pub struct TransactionMatch {
    pub signature: String,
    pub timestamp: String,
    pub amount_sol: f64,
    pub from: String,
    pub to: String,
    pub match_reason: String,
}

#[derive(Clone, Debug)]
pub struct TokenMatch {
    pub symbol: String,
    pub volume: f64,
    pub transfer_count: usize,
    pub match_reason: String,
}

#[derive(Clone, Debug)]
pub struct SearchSuggestion {
    pub text: String,
    pub entity_type: EntityType,
    pub description: String,
    pub match_score: u8,  // 0-100 relevance score
}

#[derive(Clone, Debug)]
pub struct SearchResult {
    pub entity_type: EntityType,
    pub address: String,
    pub preview_data: Vec<(String, String)>,  // Key-value pairs for preview
    pub timestamp: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EntityType {
    Wallet,
    Token,
    Program,
    Transaction,
    Recent,  // From history
}

impl SearchSuggestion {
    pub fn entity_type_str(&self) -> &str {
        match self.entity_type {
            EntityType::Wallet => "Wallet",
            EntityType::Token => "Token",
            EntityType::Program => "Program",
            EntityType::Transaction => "Transaction",
            EntityType::Recent => "Recent",
        }
    }
}

impl OsvmApp {
    pub fn new(target_wallet: String) -> Self {
        Self {
            active_tab: TabIndex::Dashboard,
            agent_output: Arc::new(Mutex::new(Vec::new())),
            wallet_graph: Arc::new(Mutex::new(WalletGraph::new(target_wallet.clone()))),
            token_volumes: Arc::new(Mutex::new(Vec::new())),
            transfer_events: Arc::new(Mutex::new(Vec::new())),
            logs: Arc::new(Mutex::new(Vec::new())),
            should_quit: false,
            show_help: false,
            iteration: 0,
            findings_count: 0,
            target_wallet,
            status: Arc::new(Mutex::new("Initializing...".to_string())),
            phase: Arc::new(Mutex::new("INIT".to_string())),
            activity_history: vec![0; 60],
            transfer_history: vec![0; 60],
            sol_flow_history: vec![0; 60],
            depth_reached: 0,
            wallets_explored: 0,
            start_time: std::time::Instant::now(),
            total_sol_in: 0.0,
            total_sol_out: 0.0,
            api_calls: 0,
            rpc_latency_ms: 0,
            log_scroll: 0,
            output_scroll: 0,
            help_scroll: 0,
            ai_insights: Arc::new(Mutex::new(Vec::new())),
            search_active: false,
            search_query: String::new(),
            filter_mode: FilterMode::All,
            global_search_active: false,
            global_search_query: String::new(),
            search_history: Self::load_search_history(),
            search_suggestions: Vec::new(),
            selected_suggestion: 0,
            search_loading: false,
            search_result: None,
            search_error: None,
            search_results_data: Arc::new(Mutex::new(SearchResultsData::default())),
            search_results_scroll: 0,
            network_stats: Arc::new(Mutex::new(NetworkStats::default())),
            live_transactions: Arc::new(Mutex::new(Vec::new())),
            last_refresh: std::time::Instant::now(),
        }
    }

    /// Get handles for background thread to update analytics
    pub fn get_analytics_handles(&self) -> (Arc<Mutex<Vec<TokenVolume>>>, Arc<Mutex<Vec<TransferEvent>>>) {
        (Arc::clone(&self.token_volumes), Arc::clone(&self.transfer_events))
    }

    /// Get a clone of the wallet_graph Arc for sharing with background threads
    pub fn get_graph_handle(&self) -> Arc<Mutex<WalletGraph>> {
        Arc::clone(&self.wallet_graph)
    }

    /// Get AI insights handle for background thread updates
    pub fn get_insights_handle(&self) -> Arc<Mutex<Vec<String>>> {
        Arc::clone(&self.ai_insights)
    }

    /// Get network stats handle for background thread updates
    pub fn get_network_stats_handle(&self) -> Arc<Mutex<NetworkStats>> {
        Arc::clone(&self.network_stats)
    }

    /// Get live transactions handle for background thread updates
    pub fn get_live_tx_handle(&self) -> Arc<Mutex<Vec<LiveTransaction>>> {
        Arc::clone(&self.live_transactions)
    }

    pub fn set_status(&self, status: &str) {
        *self.status.lock().unwrap() = status.to_string();
    }

    pub fn set_phase(&self, phase: &str) {
        *self.phase.lock().unwrap() = phase.to_string();
    }

    /// Open global search modal
    pub fn open_global_search(&mut self) {
        self.global_search_active = true;
        self.global_search_query.clear();
        self.selected_suggestion = 0;
        self.update_search_suggestions();
    }

    /// Update search suggestions based on query with scoring
    pub fn update_search_suggestions(&mut self) {
        self.search_suggestions.clear();
        self.search_error = None;

        let query = self.global_search_query.to_lowercase();

        // If no query, show recent searches only
        if query.is_empty() {
            for search in self.search_history.iter().take(5) {
                self.search_suggestions.push(SearchSuggestion {
                    text: search.clone(),
                    entity_type: EntityType::Recent,
                    description: "Recent search".to_string(),
                    match_score: 100,
                });
            }
            return;
        }

        let mut suggestions = Vec::new();

        // Recent searches with fuzzy matching
        for search in &self.search_history {
            if search.to_lowercase().contains(&query) {
                let score = self.calculate_match_score(&query, &search.to_lowercase());
                suggestions.push(SearchSuggestion {
                    text: search.clone(),
                    entity_type: EntityType::Recent,
                    description: "Recent search".to_string(),
                    match_score: score,
                });
            }
        }

        // Smart entity type detection
        let query_original = &self.global_search_query;

        // Wallet/Program address (32-44 chars, base58)
        if query_original.len() >= 32 && query_original.chars().all(|c| c.is_alphanumeric()) {
            let base_score = ((query_original.len() as f32 / 44.0) * 100.0) as u8;

            suggestions.push(SearchSuggestion {
                text: query_original.clone(),
                entity_type: EntityType::Wallet,
                description: format!("Wallet address • {} chars", query_original.len()),
                match_score: base_score.min(95),
            });

            suggestions.push(SearchSuggestion {
                text: query_original.clone(),
                entity_type: EntityType::Program,
                description: format!("Program ID • {} chars", query_original.len()),
                match_score: (base_score - 5).min(90),
            });

            // Transaction signature (typically 87-88 chars)
            if query_original.len() >= 80 {
                suggestions.push(SearchSuggestion {
                    text: query_original.clone(),
                    entity_type: EntityType::Transaction,
                    description: format!("Transaction signature • {} chars", query_original.len()),
                    match_score: 98,
                });
            }
        }

        // Token symbol (short uppercase)
        if query_original.len() <= 10 && query_original.chars().all(|c| c.is_ascii_uppercase() || c.is_numeric() || c == '-') {
            let score = if query_original.len() >= 3 && query_original.len() <= 5 {
                95  // Common token length
            } else {
                70
            };

            suggestions.push(SearchSuggestion {
                text: query_original.clone(),
                entity_type: EntityType::Token,
                description: format!("Token symbol • Search DEXs & markets"),
                match_score: score,
            });
        }

        // Sort by score (highest first)
        suggestions.sort_by(|a, b| b.match_score.cmp(&a.match_score));
        self.search_suggestions = suggestions;

        // Reset selection if out of bounds
        if self.selected_suggestion >= self.search_suggestions.len() {
            self.selected_suggestion = 0;
        }
    }

    /// Calculate fuzzy match score (0-100)
    fn calculate_match_score(&self, query: &str, target: &str) -> u8 {
        if target == query {
            return 100;
        }
        if target.starts_with(query) {
            return 90;
        }
        if target.contains(query) {
            return 70;
        }

        // Fuzzy matching - count matching chars
        let matching_chars = query.chars()
            .filter(|c| target.contains(*c))
            .count();

        let score = ((matching_chars as f32 / query.len() as f32) * 50.0) as u8;
        score.max(10)
    }

    /// Execute search for selected suggestion - REAL SEARCH of indexed data
    pub fn execute_search(&mut self) {
        if self.search_suggestions.is_empty() {
            return;
        }

        let suggestion = self.search_suggestions[self.selected_suggestion].clone();

        // Add to history (max 5, avoid duplicates)
        self.search_history.retain(|s| s != &suggestion.text);
        self.search_history.insert(0, suggestion.text.clone());
        if self.search_history.len() > 5 {
            self.search_history.pop();
        }

        // Save history to disk
        Self::save_search_history(&self.search_history);

        // Set loading state
        self.search_loading = true;
        self.search_error = None;

        // ACTUALLY SEARCH THE INDEXED DATA
        let search_results = self.search_indexed_data(&suggestion.text, &suggestion.entity_type);

        // Update search results data
        {
            let mut results_data = self.search_results_data.lock().unwrap();
            *results_data = search_results;
        }

        // Log and switch to search results tab
        let total_matches = self.search_results_data.lock().unwrap().total_matches;
        let log_msg = match suggestion.entity_type {
            EntityType::Wallet => {
                format!("🔍 Found {} matches for wallet: {}", total_matches, Self::truncate_address(&suggestion.text))
            }
            EntityType::Token => {
                format!("🪙 Found {} token matches for: {}", total_matches, suggestion.text)
            }
            EntityType::Program => {
                format!("⚙️  Found {} program matches for: {}", total_matches, Self::truncate_address(&suggestion.text))
            }
            EntityType::Transaction => {
                format!("📜 Found {} transaction matches for: {}", total_matches, Self::truncate_address(&suggestion.text))
            }
            EntityType::Recent => {
                format!("🕒 Re-searching: {} matches found", total_matches)
            }
        };

        self.add_log(log_msg);
        self.set_status(&format!("Search complete: {} matches", total_matches));

        // Switch to SearchResults tab to show findings
        self.active_tab = TabIndex::SearchResults;
        self.search_results_scroll = 0;

        // Close modal
        self.global_search_active = false;
        self.search_loading = false;
    }

    /// Search the indexed local data (WalletGraph, TransferEvents, TokenVolumes)
    fn search_indexed_data(&self, query: &str, entity_type: &EntityType) -> SearchResultsData {
        let mut results = SearchResultsData {
            query: query.to_string(),
            entity_type: format!("{:?}", entity_type),
            search_timestamp: chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string(),
            ..Default::default()
        };

        let query_lower = query.to_lowercase();

        // Search WalletGraph nodes
        {
            let graph = self.wallet_graph.lock().unwrap();
            for (address, node) in graph.nodes_iter() {
                if address.to_lowercase().contains(&query_lower) ||
                   node.label.to_lowercase().contains(&query_lower) {
                    results.wallets_found.push(WalletMatch {
                        address: address.clone(),
                        balance_sol: node.amount.unwrap_or(0.0),
                        transfer_count: 0, // Simplified for now - would need connection counting
                        last_activity: "Recent".to_string(),
                        match_reason: format!("Address contains '{}'", query),
                    });
                }
            }
        }

        // Search TransferEvents
        {
            let events = self.transfer_events.lock().unwrap();
            for event in events.iter() {
                if event.token.to_lowercase().contains(&query_lower) ||
                   event.timestamp.contains(query) {
                    // This is a simplified match - in real implementation, extract addresses
                    results.transactions_found.push(TransactionMatch {
                        signature: format!("tx_{}", results.transactions_found.len()),
                        timestamp: event.timestamp.clone(),
                        amount_sol: event.amount,
                        from: "N/A".to_string(),
                        to: "N/A".to_string(),
                        match_reason: format!("Transfer of {} {}", event.amount, event.token),
                    });
                }
            }
        }

        // Search TokenVolumes
        {
            let tokens = self.token_volumes.lock().unwrap();
            for token in tokens.iter() {
                if token.symbol.to_lowercase().contains(&query_lower) {
                    results.tokens_found.push(TokenMatch {
                        symbol: token.symbol.clone(),
                        volume: token.amount,
                        transfer_count: 0, // Would need to count from events
                        match_reason: format!("Token symbol matches '{}'", query),
                    });
                }
            }
        }

        results.total_matches = results.wallets_found.len() +
                                results.transactions_found.len() +
                                results.tokens_found.len();

        results
    }

    /// Load search history from disk
    fn load_search_history() -> Vec<String> {
        let history_path = dirs::home_dir()
            .map(|h| h.join(".osvm").join("search_history.json"))
            .unwrap_or_default();

        if let Ok(content) = std::fs::read_to_string(&history_path) {
            serde_json::from_str(&content).unwrap_or_default()
        } else {
            Vec::new()
        }
    }

    /// Save search history to disk
    fn save_search_history(history: &[String]) {
        if let Some(home) = dirs::home_dir() {
            let osvm_dir = home.join(".osvm");
            let _ = std::fs::create_dir_all(&osvm_dir);

            let history_path = osvm_dir.join("search_history.json");
            if let Ok(json) = serde_json::to_string_pretty(history) {
                let _ = std::fs::write(history_path, json);
            }
        }
    }

    /// Truncate address for display
    fn truncate_address(addr: &str) -> String {
        if addr.len() > 16 {
            format!("{}...{}", &addr[..8], &addr[addr.len()-4..])
        } else {
            addr.to_string()
        }
    }

    pub fn add_log(&mut self, message: String) {
        let mut logs = self.logs.lock().unwrap();
        logs.push(format!("[{}] {}", chrono::Local::now().format("%H:%M:%S"), message));
        if logs.len() > 1000 {
            logs.remove(0);
        }
    }

    /// Update activity sparkline - called on each tick
    pub fn tick(&mut self) {
        self.iteration += 1;

        // Update activity history based on current state
        let (nodes, edges) = self.wallet_graph.lock()
            .map(|g| (g.node_count(), g.edge_count()))
            .unwrap_or((0, 0));

        // Shift and add new activity value
        self.activity_history.remove(0);
        self.activity_history.push(nodes as u64);

        self.transfer_history.remove(0);
        self.transfer_history.push(edges as u64);

        // SOL flow history (simulated based on transfer events)
        let events_len = self.transfer_events.lock().map(|e| e.len()).unwrap_or(0);
        self.sol_flow_history.remove(0);
        self.sol_flow_history.push((events_len % 100) as u64);

        self.wallets_explored = nodes;
    }

    /// Simple run method for backwards compatibility
    pub fn run(&mut self) -> Result<()> {
        self.run_tui(|app| app.tick())
    }

    pub fn run_tui<F>(&mut self, on_tick: F) -> Result<()>
    where
        F: Fn(&mut Self) + Send + 'static,
    {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        // REMOVED EnableMouseCapture to allow terminal text selection and paste
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;

        let result = self.event_loop(&mut terminal, on_tick);

        disable_raw_mode()?;
        execute!(
            terminal.backend_mut(),
            LeaveAlternateScreen
        )?;
        terminal.show_cursor()?;

        result
    }

    fn event_loop<B, F>(&mut self, terminal: &mut Terminal<B>, on_tick: F) -> Result<()>
    where
        B: ratatui::backend::Backend + std::io::Write,
        F: Fn(&mut Self),
    {
        loop {
            terminal.draw(|f| self.ui(f))?;

            if event::poll(Duration::from_millis(100))? {
                if let Event::Key(key) = event::read()? {
                    match key.code {
                        KeyCode::Char('q') | KeyCode::Esc => {
                            // Priority: global search > help > graph search > quit
                            if self.global_search_active {
                                self.global_search_active = false;
                            } else if self.show_help {
                                self.show_help = false;
                                self.help_scroll = 0;  // Reset scroll when closing
                            } else if self.active_tab == TabIndex::Graph {
                                // Check if search is active in graph
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    if graph.search_active {
                                        graph.search_active = false;
                                        graph.search_query.clear();
                                        graph.search_results.clear();
                                    } else {
                                        self.should_quit = true;
                                    }
                                } else {
                                    self.should_quit = true;
                                }
                            } else {
                                self.should_quit = true;
                            }
                        }
                        KeyCode::Char('k') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            // GLOBAL SEARCH MODAL - works everywhere
                            if !self.global_search_active {
                                self.open_global_search();
                            }
                        }
                        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            self.should_quit = true;
                        }
                        KeyCode::Char('e') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            // Export investigation
                            if let Err(e) = self.export_investigation() {
                                self.add_log(format!("Export failed: {}", e));
                            } else {
                                self.add_log("Investigation exported successfully".to_string());
                            }
                        }
                        KeyCode::Char('/') => {
                            // Toggle search in Dashboard
                            if self.active_tab == TabIndex::Dashboard {
                                self.search_active = !self.search_active;
                                if !self.search_active {
                                    self.search_query.clear();
                                }
                            }
                        }
                        KeyCode::Char('f') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            // Cycle filter mode
                            if self.active_tab == TabIndex::Dashboard {
                                self.filter_mode = match self.filter_mode {
                                    FilterMode::All => FilterMode::Errors,
                                    FilterMode::Errors => FilterMode::Success,
                                    FilterMode::Success => FilterMode::Transfers,
                                    FilterMode::Transfers => FilterMode::All,
                                };
                            }
                        }
                        KeyCode::Backspace => {
                            if self.global_search_active && !self.global_search_query.is_empty() {
                                self.global_search_query.pop();
                                self.update_search_suggestions();
                            } else if self.search_active && !self.search_query.is_empty() {
                                self.search_query.pop();
                            }
                        }
                        // Paste from clipboard (Ctrl+V) into search
                        KeyCode::Char('v') if key.modifiers.contains(KeyModifiers::CONTROL) && self.global_search_active => {
                            if let Ok(mut clipboard) = arboard::Clipboard::new() {
                                if let Ok(text) = clipboard.get_text() {
                                    self.global_search_query.push_str(&text);
                                    self.update_search_suggestions();
                                    self.add_log(format!("📋 Pasted {} chars from clipboard", text.len()));
                                }
                            }
                        }
                        KeyCode::Char(c) if self.global_search_active => {
                            if !key.modifiers.contains(KeyModifiers::CONTROL) {
                                self.global_search_query.push(c);
                                self.update_search_suggestions();
                            }
                        }
                        KeyCode::Char(c) if self.search_active => {
                            if !key.modifiers.contains(KeyModifiers::CONTROL) {
                                self.search_query.push(c);
                            }
                        }
                        KeyCode::Char('?') | KeyCode::F(1) => {
                            self.show_help = !self.show_help;
                            if !self.show_help {
                                self.help_scroll = 0;  // Reset scroll when closing
                            }
                        }
                        KeyCode::Tab => self.next_tab(),
                        KeyCode::BackTab => self.previous_tab(),
                        KeyCode::Char('1') => self.active_tab = TabIndex::Dashboard,
                        KeyCode::Char('2') => self.active_tab = TabIndex::Graph,
                        KeyCode::Char('3') => self.active_tab = TabIndex::Logs,
                        KeyCode::Char('4') => self.active_tab = TabIndex::SearchResults,
                        // Graph navigation / Help scrolling / Search suggestions
                        KeyCode::Char('j') | KeyCode::Down => {
                            if self.global_search_active {
                                if !self.search_suggestions.is_empty() {
                                    self.selected_suggestion = (self.selected_suggestion + 1) % self.search_suggestions.len();
                                }
                            } else if self.show_help {
                                self.help_scroll = self.help_scroll.saturating_add(1);
                            } else if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Down);
                                }
                            } else if self.active_tab == TabIndex::Logs {
                                self.log_scroll = self.log_scroll.saturating_add(1);
                            }
                        }
                        KeyCode::Char('k') | KeyCode::Up => {
                            if self.global_search_active {
                                if !self.search_suggestions.is_empty() {
                                    self.selected_suggestion = if self.selected_suggestion == 0 {
                                        self.search_suggestions.len() - 1
                                    } else {
                                        self.selected_suggestion - 1
                                    };
                                }
                            } else if self.show_help {
                                self.help_scroll = self.help_scroll.saturating_sub(1);
                            } else if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Up);
                                }
                            } else if self.active_tab == TabIndex::Logs {
                                self.log_scroll = self.log_scroll.saturating_sub(1);
                            }
                        }
                        KeyCode::Enter => {
                            if self.global_search_active {
                                self.execute_search();
                            } else if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::HopToWallet);
                                }
                            }
                        }
                        KeyCode::Char(' ') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Toggle);
                                }
                            }
                        }
                        KeyCode::Char('h') | KeyCode::Left => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Left);
                                }
                            }
                        }
                        KeyCode::Char('l') | KeyCode::Right => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Right);
                                }
                            }
                        }
                        // Graph zoom/pan controls (Shift + arrows for panning, +/- for zoom)
                        KeyCode::Char('=') | KeyCode::Char('+') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::ZoomIn);
                                }
                            }
                        }
                        KeyCode::Char('-') | KeyCode::Char('_') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::ZoomOut);
                                }
                            }
                        }
                        KeyCode::Char('w') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::PanUp);
                                }
                            }
                        }
                        KeyCode::Char('s') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::PanDown);
                                }
                            }
                        }
                        KeyCode::Char('a') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::PanLeft);
                                }
                            }
                        }
                        KeyCode::Char('d') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::PanRight);
                                }
                            }
                        }
                        KeyCode::PageUp => {
                            self.log_scroll = self.log_scroll.saturating_sub(10);
                        }
                        KeyCode::PageDown => {
                            self.log_scroll = self.log_scroll.saturating_add(10);
                        }
                        KeyCode::Home => {
                            self.log_scroll = 0;
                        }
                        KeyCode::End => {
                            let logs_len = self.logs.lock().map(|l| l.len()).unwrap_or(0);
                            self.log_scroll = logs_len.saturating_sub(20);
                        }
                        KeyCode::Char('[') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::DecreaseDepth);
                                }
                            }
                        }
                        KeyCode::Char(']') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::IncreaseDepth);
                                }
                            }
                        }
                        // Search functionality
                        KeyCode::Char('/') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::StartSearch);
                                }
                            }
                        }
                        KeyCode::Char('n') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::SearchNext);
                                }
                            }
                        }
                        KeyCode::Char('N') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::SearchPrev);
                                }
                            }
                        }
                        // Copy to clipboard
                        KeyCode::Char('y') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::Copy);
                                }
                            } else if self.active_tab == TabIndex::SearchResults {
                                // Copy all search results to clipboard
                                let (output, total_matches) = {
                                    let results = self.search_results_data.lock().unwrap();
                                    let mut output = format!("Search Results for: {}\n", results.query);
                                    output.push_str(&format!("Total Matches: {}\n", results.total_matches));
                                    output.push_str(&format!("Timestamp: {}\n\n", results.search_timestamp));

                                    if !results.wallets_found.is_empty() {
                                        output.push_str(&format!("=== WALLETS ({}) ===\n", results.wallets_found.len()));
                                        for w in &results.wallets_found {
                                            output.push_str(&format!("{}\n  Balance: {:.4} SOL\n  Transfers: {}\n\n",
                                                w.address, w.balance_sol, w.transfer_count));
                                        }
                                    }

                                    if !results.tokens_found.is_empty() {
                                        output.push_str(&format!("=== TOKENS ({}) ===\n", results.tokens_found.len()));
                                        for t in &results.tokens_found {
                                            output.push_str(&format!("{}\n  Volume: {:.2}\n\n", t.symbol, t.volume));
                                        }
                                    }

                                    if !results.transactions_found.is_empty() {
                                        output.push_str(&format!("=== TRANSACTIONS ({}) ===\n", results.transactions_found.len()));
                                        for tx in &results.transactions_found {
                                            output.push_str(&format!("{}\n  Time: {}\n  Amount: {:.4} SOL\n\n",
                                                tx.signature, tx.timestamp, tx.amount_sol));
                                        }
                                    }

                                    (output, results.total_matches)
                                };

                                if let Ok(mut clipboard) = arboard::Clipboard::new() {
                                    if clipboard.set_text(&output).is_ok() {
                                        self.add_log(format!("📋 Copied {} search results to clipboard", total_matches));
                                    }
                                }
                            }
                        }
                        // Handle typing in search mode
                        KeyCode::Char(c) => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    if graph.search_active {
                                        graph.handle_input(GraphInput::SearchChar(c));
                                    }
                                }
                            }
                        }
                        KeyCode::Backspace => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    if graph.search_active {
                                        graph.handle_input(GraphInput::SearchBackspace);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            if self.should_quit {
                break;
            }

            on_tick(self);

            // Update toast timer
            if let Ok(mut graph) = self.wallet_graph.lock() {
                graph.tick_toast();
            }
        }

        Ok(())
    }

    fn next_tab(&mut self) {
        self.active_tab = match self.active_tab {
            TabIndex::Dashboard => TabIndex::Graph,
            TabIndex::Graph => TabIndex::Logs,
            TabIndex::Logs => TabIndex::SearchResults,
            TabIndex::SearchResults => TabIndex::Dashboard,
        };
    }

    fn previous_tab(&mut self) {
        self.active_tab = match self.active_tab {
            TabIndex::Dashboard => TabIndex::SearchResults,
            TabIndex::Graph => TabIndex::Dashboard,
            TabIndex::Logs => TabIndex::Graph,
            TabIndex::SearchResults => TabIndex::Logs,
        };
    }

    fn ui(&mut self, f: &mut Frame) {
        let size = f.area();

        match self.active_tab {
            TabIndex::Dashboard => self.render_dashboard(f, size),
            TabIndex::Graph => self.render_full_graph(f, size),
            TabIndex::Logs => self.render_full_logs(f, size),
            TabIndex::SearchResults => self.render_search_results_tab(f, size),
        }

        // Render help overlay if active
        if self.show_help {
            self.render_help_overlay(f, size);
        }

        // Render global search modal (HIGHEST PRIORITY - renders on top)
        if self.global_search_active {
            self.render_global_search(f, size);
        }
    }

    /// btop-style dashboard with all panels visible
    fn render_dashboard(&mut self, f: &mut Frame, area: Rect) {
        // Main vertical split: header bar, network stats, content, footer
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),   // Header with tabs
                Constraint::Length(5),   // Real-time network stats (NEW!)
                Constraint::Min(0),      // Content
                Constraint::Length(2),   // Status bar (btop style)
            ])
            .split(area);

        self.render_btop_header(f, main_chunks[0]);
        self.render_network_stats_panel(f, main_chunks[1]);  // NEW!

        // Content area: left (50%) + right (50%)
        let content_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(main_chunks[2]);

        // Left side: Activity feed + Mini graph
        let left_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(60),  // Activity feed
                Constraint::Percentage(40),  // Mini graph preview
            ])
            .split(content_chunks[0]);

        self.render_activity_feed(f, left_chunks[0]);
        self.render_mini_graph(f, left_chunks[1]);

        // Right side: Stats + Metrics + AI Insights + Transfers
        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(7),   // Progress gauges (btop CPU style)
                Constraint::Length(9),   // Token volumes with bars
                Constraint::Length(6),   // AI insights
                Constraint::Min(0),      // Transfer list
            ])
            .split(content_chunks[1]);

        self.render_progress_gauges(f, right_chunks[0]);
        self.render_volume_bars(f, right_chunks[1]);
        self.render_ai_insights(f, right_chunks[2]);
        self.render_transfer_feed(f, right_chunks[3]);

        self.render_btop_statusbar(f, main_chunks[2]);
    }

    fn render_btop_header(&self, f: &mut Frame, area: Rect) {
        let elapsed = self.start_time.elapsed();
        let mins = elapsed.as_secs() / 60;
        let secs = elapsed.as_secs() % 60;

        let phase_str = self.phase.lock().unwrap().clone();
        let phase_color = match phase_str.as_str() {
            "INIT" => Color::Yellow,
            "PLANNING" => Color::Cyan,
            "INVESTIGATING" => Color::Green,
            "ANALYZING" => Color::Magenta,
            "COMPLETE" => Color::Green,
            "ERROR" => Color::Red,
            _ => Color::White,
        };

        // Animated spinner (btop-style)
        let spinners = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"];
        let spinner = spinners[self.iteration % spinners.len()];

        // Tab indicators
        let tabs: Vec<Span> = vec![
            Span::styled("│", Style::default().fg(Color::DarkGray)),
            if self.active_tab == TabIndex::Dashboard {
                Span::styled(" ▣ Dashboard ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            } else {
                Span::styled(" □ Dashboard ", Style::default().fg(Color::DarkGray))
            },
            if self.active_tab == TabIndex::Graph {
                Span::styled(" ▣ Graph ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            } else {
                Span::styled(" □ Graph ", Style::default().fg(Color::DarkGray))
            },
            if self.active_tab == TabIndex::Logs {
                Span::styled(" ▣ Logs ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            } else {
                Span::styled(" □ Logs ", Style::default().fg(Color::DarkGray))
            },
        ];

        let target_short = if self.target_wallet.len() > 12 {
            format!("{}…{}", &self.target_wallet[..4], &self.target_wallet[self.target_wallet.len()-4..])
        } else {
            self.target_wallet.clone()
        };

        let mut header_spans = vec![
            Span::styled(" ", Style::default()),
            Span::styled(spinner, Style::default().fg(Color::Cyan)),
            Span::styled(" osvm", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(" wallet-explorer ", Style::default().fg(Color::White)),
        ];
        header_spans.extend(tabs);
        header_spans.extend(vec![
            Span::styled("│", Style::default().fg(Color::DarkGray)),
            Span::styled(format!(" {} ", phase_str), Style::default().fg(phase_color).add_modifier(Modifier::BOLD)),
            Span::styled(format!("{}:{:02}", mins, secs), Style::default().fg(Color::Yellow)),
            Span::styled(" │ ", Style::default().fg(Color::DarkGray)),
            Span::styled("🎯 ", Style::default()),
            Span::styled(target_short, Style::default().fg(Color::Magenta)),
        ]);

        let header = Paragraph::new(Line::from(header_spans))
            .block(Block::default()
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .border_style(Style::default().fg(Color::Cyan)));

        f.render_widget(header, area);
    }

    fn render_btop_statusbar(&self, f: &mut Frame, area: Rect) {
        let (nodes, edges) = self.wallet_graph.lock()
            .map(|g| (g.node_count(), g.edge_count()))
            .unwrap_or((0, 0));

        let logs_len = self.logs.lock().map(|l| l.len()).unwrap_or(0);

        let line1 = Line::from(vec![
            Span::styled(" [", Style::default().fg(Color::DarkGray)),
            Span::styled("1", Style::default().fg(if self.active_tab == TabIndex::Dashboard { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
            Span::styled("]", Style::default().fg(Color::DarkGray)),
            Span::styled("Dashboard ", Style::default().fg(Color::White)),
            Span::styled("[", Style::default().fg(Color::DarkGray)),
            Span::styled("2", Style::default().fg(if self.active_tab == TabIndex::Graph { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
            Span::styled("]", Style::default().fg(Color::DarkGray)),
            Span::styled("Graph ", Style::default().fg(Color::White)),
            Span::styled("[", Style::default().fg(Color::DarkGray)),
            Span::styled("3", Style::default().fg(if self.active_tab == TabIndex::Logs { Color::Cyan } else { Color::DarkGray }).add_modifier(Modifier::BOLD)),
            Span::styled("]", Style::default().fg(Color::DarkGray)),
            Span::styled("Logs ", Style::default().fg(Color::White)),
            Span::styled("│ ", Style::default().fg(Color::DarkGray)),
            Span::styled("", Style::default().fg(Color::Green)),
            Span::styled(format!("{} ", nodes), Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
            Span::styled("wallets ", Style::default().fg(Color::DarkGray)),
            Span::styled("", Style::default().fg(Color::Yellow)),
            Span::styled(format!("{} ", edges), Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::styled("transfers ", Style::default().fg(Color::DarkGray)),
            Span::styled("", Style::default().fg(Color::Blue)),
            Span::styled(format!("{} ", logs_len), Style::default().fg(Color::Blue).add_modifier(Modifier::BOLD)),
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

    fn render_activity_feed(&self, f: &mut Frame, area: Rect) {
        let output = self.agent_output.lock().unwrap();

        // Apply filter and search
        let filtered: Vec<&String> = output
            .iter()
            .filter(|line| self.matches_filter(line))
            .collect();

        let items: Vec<ListItem> = filtered
            .iter()
            .rev()
            .take((area.height as usize).saturating_sub(if self.search_active { 3 } else { 2 }))
            .map(|line| {
                let (icon, color) = if line.contains("✅") || line.contains("Found") || line.contains("SUCCESS") {
                    ("▶", Color::Green)
                } else if line.contains("⚠") || line.contains("ERROR") || line.contains("error") || line.contains("Failed") {
                    ("▶", Color::Red)
                } else if line.contains("🔍") || line.contains("Exploring") || line.contains("Fetching") || line.contains("Analyzing") {
                    ("▶", Color::Blue)
                } else if line.contains("→") || line.contains("transfer") {
                    ("◆", Color::Yellow)
                } else if line.contains("Phase") || line.contains("Step") {
                    ("●", Color::Magenta)
                } else {
                    ("○", Color::DarkGray)
                };

                // Truncate long lines
                let max_len = (area.width as usize).saturating_sub(5);
                let display = if line.len() > max_len {
                    format!("{} {}…", icon, &line[..max_len.saturating_sub(2)])
                } else {
                    format!("{} {}", icon, line)
                };

                ListItem::new(display).style(Style::default().fg(color))
            })
            .collect();

        // Title with filter mode indicator
        let filter_indicator = match self.filter_mode {
            FilterMode::All => "",
            FilterMode::Errors => " [ERRORS]",
            FilterMode::Success => " [SUCCESS]",
            FilterMode::Transfers => " [TRANSFERS]",
        };

        let title = format!(" ◉ Activity{} ({}/{}) ", filter_indicator, filtered.len(), output.len());

        let block = Block::default()
            .title(Span::styled(title, Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)))
            .title_alignment(Alignment::Left)
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let list = List::new(items).block(block);
        f.render_widget(list, area);

        // Render search bar if active
        if self.search_active {
            let search_area = Rect {
                x: area.x + 2,
                y: area.y + area.height - 2,
                width: area.width.saturating_sub(4),
                height: 1,
            };

            let search_text = format!("Search: {}█", self.search_query);
            let search_widget = Paragraph::new(search_text)
                .style(Style::default().fg(Color::Yellow).bg(Color::DarkGray));
            f.render_widget(search_widget, search_area);
        }
    }

    /// Mini graph preview in dashboard (btop style overview)
    fn render_mini_graph(&self, f: &mut Frame, area: Rect) {
        let (nodes, edges) = self.wallet_graph.lock()
            .map(|g| (g.node_count(), g.edge_count()))
            .unwrap_or((0, 0));

        // Get wallet type breakdown
        let (funding, recipients, defi, target_count) = if let Ok(graph) = self.wallet_graph.lock() {
            let mut fund = 0usize;
            let mut recv = 0usize;
            let mut dex = 0usize;
            let mut tgt = 0usize;
            for (_, node) in graph.nodes_iter() {
                match node.node_type {
                    super::graph::WalletNodeType::Funding => fund += 1,
                    super::graph::WalletNodeType::Recipient => recv += 1,
                    super::graph::WalletNodeType::DeFi => dex += 1,
                    super::graph::WalletNodeType::Target => tgt += 1,
                    _ => {}
                }
            }
            (fund, recv, dex, tgt)
        } else {
            (0, 0, 0, 0)
        };

        let inner = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(3),  // Sparkline
                Constraint::Min(0),     // Stats
            ])
            .split(area);

        let block = Block::default()
            .title(Span::styled(" ◎ Network Overview ", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));
        f.render_widget(block, area);

        // Activity sparkline
        let sparkline = Sparkline::default()
            .data(&self.activity_history)
            .style(Style::default().fg(Color::Green));
        f.render_widget(sparkline, inner[0]);

        // Network composition
        let total = (funding + recipients + defi + target_count).max(1) as f64;
        let text = vec![
            Line::from(vec![
                Span::styled("🔴", Style::default()),
                Span::styled(format!(" Target: {} ", target_count), Style::default().fg(Color::Red)),
                Span::styled("🟢", Style::default()),
                Span::styled(format!(" Fund: {} ", funding), Style::default().fg(Color::Green)),
            ]),
            Line::from(vec![
                Span::styled("🔵", Style::default()),
                Span::styled(format!(" Recv: {} ", recipients), Style::default().fg(Color::Blue)),
                Span::styled("🟣", Style::default()),
                Span::styled(format!(" DeFi: {} ", defi), Style::default().fg(Color::Magenta)),
            ]),
            Line::from(""),
            Line::from(vec![
                Span::styled(format!("Total: {} nodes, {} edges", nodes, edges), Style::default().fg(Color::DarkGray)),
            ]),
        ];

        let stats = Paragraph::new(text);
        f.render_widget(stats, inner[1]);
    }

    /// btop-style progress gauges (like CPU meters)
    fn render_progress_gauges(&self, f: &mut Frame, area: Rect) {
        let (nodes, edges) = self.wallet_graph.lock()
            .map(|g| (g.node_count(), g.edge_count()))
            .unwrap_or((0, 0));

        let events_count = self.transfer_events.lock().map(|e| e.len()).unwrap_or(0);
        let elapsed = self.start_time.elapsed().as_secs();

        let inner = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([
                Constraint::Length(1),
                Constraint::Length(1),
                Constraint::Length(1),
                Constraint::Min(0),
            ])
            .split(area);

        let block = Block::default()
            .title(Span::styled(" ◈ Progress ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));
        f.render_widget(block, area);

        // Exploration progress (wallets discovered / estimated total)
        let explore_pct = ((nodes as f64 / 100.0) * 100.0).min(100.0) as u16;
        let explore_bar = self.render_gauge_line("Wallets", nodes, 100, Color::Green);
        f.render_widget(explore_bar, inner[0]);

        // Transfer discovery progress
        let transfer_pct = ((edges as f64 / 200.0) * 100.0).min(100.0) as u16;
        let transfer_bar = self.render_gauge_line("Transfers", edges, 200, Color::Yellow);
        f.render_widget(transfer_bar, inner[1]);

        // Depth progress (0-5 levels)
        let depth_bar = self.render_gauge_line("Depth", self.depth_reached, 5, Color::Magenta);
        f.render_widget(depth_bar, inner[2]);

        // Time elapsed
        let time_text = Paragraph::new(Line::from(vec![
            Span::styled("Time: ", Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{}s", elapsed), Style::default().fg(Color::Cyan)),
            Span::styled(" │ Events: ", Style::default().fg(Color::DarkGray)),
            Span::styled(format!("{}", events_count), Style::default().fg(Color::Blue)),
        ]));
        f.render_widget(time_text, inner[3]);
    }

    fn render_gauge_line(&self, label: &str, value: usize, max: usize, color: Color) -> Paragraph<'static> {
        let pct = ((value as f64 / max as f64) * 100.0).min(100.0);
        let bar_width = 20;
        let filled = ((pct / 100.0) * bar_width as f64) as usize;
        let empty = bar_width - filled;

        let bar = format!("{}{}", "█".repeat(filled), "░".repeat(empty));

        Paragraph::new(Line::from(vec![
            Span::styled(format!("{:<10}", label), Style::default().fg(Color::White)),
            Span::styled(bar, Style::default().fg(color)),
            Span::styled(format!(" {:>4}/{:<4}", value, max), Style::default().fg(Color::DarkGray)),
        ]))
    }

    /// Token volume bars (btop memory style)
    fn render_volume_bars(&self, f: &mut Frame, area: Rect) {
        let volumes = self.token_volumes.lock().ok();
        let colors = [Color::Yellow, Color::Green, Color::Cyan, Color::Magenta, Color::Blue, Color::Red];

        let block = Block::default()
            .title(Span::styled(" ◇ Token Volumes ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner_area = block.inner(area);
        f.render_widget(block, area);

        let mut lines = Vec::new();

        if let Some(vols) = volumes {
            if vols.is_empty() {
                lines.push(Line::from(Span::styled("  Collecting data...", Style::default().fg(Color::DarkGray))));
            } else {
                let max_vol = vols.iter().map(|v| v.amount).fold(0.0_f64, f64::max);
                let bar_width = (inner_area.width as usize).saturating_sub(20);

                for (i, vol) in vols.iter().take(6).enumerate() {
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
        } else {
            lines.push(Line::from(Span::styled("  No data", Style::default().fg(Color::DarkGray))));
        }

        let volume_widget = Paragraph::new(lines);
        f.render_widget(volume_widget, inner_area);
    }

    fn render_transfer_feed(&self, f: &mut Frame, area: Rect) {
        let events = self.transfer_events.lock().ok();

        let block = Block::default()
            .title(Span::styled(" ↕ Transfers ", Style::default().fg(Color::Blue).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner_area = block.inner(area);
        f.render_widget(block.clone(), area);

        let mut items: Vec<ListItem> = Vec::new();

        if let Some(evts) = events {
            if evts.is_empty() {
                items.push(ListItem::new("  Waiting for transfers...").style(Style::default().fg(Color::DarkGray)));
            } else {
                for evt in evts.iter().rev().take((inner_area.height as usize).saturating_sub(1)) {
                    let (icon, color) = if evt.direction == "IN" {
                        ("↓", Color::Green)
                    } else {
                        ("↑", Color::Red)
                    };
                    let amount_str = if evt.amount >= 1_000_000.0 {
                        format!("{:.1}M", evt.amount / 1_000_000.0)
                    } else if evt.amount >= 1_000.0 {
                        format!("{:.1}K", evt.amount / 1_000.0)
                    } else {
                        format!("{:.2}", evt.amount)
                    };
                    items.push(ListItem::new(format!(
                        " {} {:>8} {:>8} {}",
                        icon, evt.timestamp, amount_str, evt.token
                    )).style(Style::default().fg(color)));
                }
            }
        }

        let list = List::new(items);
        f.render_widget(list, inner_area);
    }

    /// Full-screen graph view with node info sidebar
    fn render_full_graph(&mut self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),   // Mini header
                Constraint::Min(0),      // Graph + info
                Constraint::Length(2),   // Status bar
            ])
            .split(area);

        // Graph with info sidebar
        let graph_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Min(0), Constraint::Length(30)])
            .split(chunks[1]);

        // Use try_lock to avoid blocking if background thread holds lock
        let (nodes, edges, info_text) = if let Ok(mut graph) = self.wallet_graph.try_lock() {
            let n = graph.node_count();
            let e = graph.edge_count();
            graph.render(f, graph_chunks[0]);
            let info = graph.get_selected_info()
                .unwrap_or_else(|| "j/k to navigate".to_string());
            (n, e, info)
        } else {
            // Lock busy - show placeholder
            let placeholder = Paragraph::new("Loading...")
                .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::DarkGray)));
            f.render_widget(placeholder, graph_chunks[0]);
            (0, 0, "Graph updating...".to_string())
        };

        // Mini header
        let header = Paragraph::new(Line::from(vec![
            Span::styled(" GRAPH ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(format!("{}w {}tx ", nodes, edges), Style::default().fg(Color::Green)),
            Span::styled("│ j/k nav Enter toggle", Style::default().fg(Color::DarkGray)),
        ]));
        f.render_widget(header, chunks[0]);

        // Info panel with styling
        let info = Paragraph::new(info_text)
            .block(Block::default()
                .title(Span::styled(" Node Info ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)))
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .border_style(Style::default().fg(Color::Cyan)))
            .wrap(Wrap { trim: false });
        f.render_widget(info, graph_chunks[1]);

        self.render_btop_statusbar(f, chunks[2]);
    }

    /// Full-screen logs view with scrolling
    fn render_full_logs(&self, f: &mut Frame, area: Rect) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),
                Constraint::Min(0),
                Constraint::Length(2),
            ])
            .split(area);

        // Use try_lock to avoid blocking
        let (items, total_logs) = if let Ok(logs) = self.logs.try_lock() {
            let total = logs.len();
            let visible_height = (chunks[1].height as usize).saturating_sub(2);
            let max_scroll = total.saturating_sub(visible_height);
            let actual_scroll = self.log_scroll.min(max_scroll);

            let items: Vec<ListItem> = logs
                .iter()
                .skip(actual_scroll)
                .take(visible_height)
                .enumerate()
                .map(|(i, line)| {
                    let line_num = actual_scroll + i + 1;
                    let color = if line.contains("ERROR") || line.contains("error") {
                        Color::Red
                    } else if line.contains("WARN") || line.contains("warn") {
                        Color::Yellow
                    } else if line.contains("SUCCESS") || line.contains("✅") {
                        Color::Green
                    } else {
                        Color::DarkGray
                    };
                    ListItem::new(Line::from(vec![
                        Span::styled(format!("{:>4} ", line_num), Style::default().fg(Color::DarkGray)),
                        Span::styled(line.clone(), Style::default().fg(color)),
                    ]))
                })
                .collect();
            (items, total)
        } else {
            (vec![ListItem::new("  Loading logs...").style(Style::default().fg(Color::DarkGray))], 0)
        };

        let header = Paragraph::new(Line::from(vec![
            Span::styled(" LOGS ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(format!("│ {} entries │ j/k scroll", total_logs), Style::default().fg(Color::DarkGray)),
        ]));
        f.render_widget(header, chunks[0]);

        let list = List::new(items)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_type(BorderType::Rounded)
                .border_style(Style::default().fg(Color::DarkGray)));
        f.render_widget(list, chunks[1]);

        self.render_btop_statusbar(f, chunks[2]);
    }

    fn render_global_search(&mut self, f: &mut Frame, area: Rect) {
        use ratatui::widgets::{Clear, Paragraph};

        // BIG CENTERED MODAL (90 cols x 30 rows)
        let width = 90.min(area.width.saturating_sub(4));
        let height = 30.min(area.height.saturating_sub(4));
        let x = (area.width.saturating_sub(width)) / 2;
        let y = (area.height.saturating_sub(height)) / 2;
        let modal_area = Rect::new(x, y, width, height);

        // Semi-transparent background overlay
        f.render_widget(Clear, modal_area);

        let mut lines = Vec::new();
        lines.push(Line::from(""));

        // Header with gradient effect
        lines.push(Line::from(vec![
            Span::styled(" ⚡ ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::styled("GLOBAL BLOCKCHAIN SEARCH", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(" ⚡", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]));

        lines.push(Line::from(Span::styled(
            " Search wallets • tokens • programs • transactions across Solana ",
            Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)
        )));
        lines.push(Line::from(""));

        // Search input with animated cursor
        let cursor_char = if (self.iteration / 10) % 2 == 0 { "█" } else { "▌" };
        lines.push(Line::from(vec![
            Span::styled(" 🔍 ", Style::default().fg(Color::Yellow)),
            Span::styled(
                if self.global_search_query.is_empty() {
                    "Type wallet address, token symbol, program ID, or tx signature..."
                } else {
                    &self.global_search_query
                },
                Style::default()
                    .fg(if self.global_search_query.is_empty() { Color::DarkGray } else { Color::White })
                    .add_modifier(if !self.global_search_query.is_empty() { Modifier::BOLD } else { Modifier::ITALIC })
            ),
            Span::styled(cursor_char, Style::default().fg(Color::Cyan)),
        ]));
        lines.push(Line::from(""));

        // Error display
        if let Some(ref error) = self.search_error {
            lines.push(Line::from(vec![
                Span::styled(" ❌ Error: ", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
                Span::styled(error, Style::default().fg(Color::LightRed)),
            ]));
            lines.push(Line::from(""));
        }

        // Loading indicator
        if self.search_loading {
            let spinner = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
            let spinner_char = spinner[(self.iteration / 5) % spinner.len()];
            lines.push(Line::from(vec![
                Span::styled(format!(" {} Searching blockchain...", spinner_char),
                    Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            ]));
            lines.push(Line::from(""));
        }

        // Suggestions with scores
        if !self.search_suggestions.is_empty() {
            let results_text = if self.global_search_query.is_empty() {
                " ━━━ Recent Searches ━━━".to_string()
            } else {
                format!(" ━━━ {} Smart Suggestions ━━━", self.search_suggestions.len())
            };

            lines.push(Line::from(Span::styled(
                results_text,
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            )));
            lines.push(Line::from(""));

            // Show up to 10 suggestions
            for (i, suggestion) in self.search_suggestions.iter().take(10).enumerate() {
                let is_selected = i == self.selected_suggestion;
                let prefix = if is_selected { " ❯ " } else { "   " };

                let (icon, type_color) = match suggestion.entity_type {
                    EntityType::Wallet => ("👛", Color::Green),
                    EntityType::Token => ("🪙", Color::Yellow),
                    EntityType::Program => ("⚙️ ", Color::Magenta),
                    EntityType::Transaction => ("📜", Color::Cyan),
                    EntityType::Recent => ("🕒", Color::Blue),
                };

                // Score bar visualization
                let score_blocks = (suggestion.match_score as usize) / 10;
                let score_bar = "█".repeat(score_blocks);
                let score_empty = "░".repeat(10 - score_blocks);

                // Main suggestion line with truncation for long addresses
                let display_text = if suggestion.text.len() > 60 {
                    format!("{}...{}", &suggestion.text[..28], &suggestion.text[suggestion.text.len()-28..])
                } else {
                    suggestion.text.clone()
                };

                lines.push(Line::from(vec![
                    Span::styled(prefix, Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                    Span::raw(icon),
                    Span::raw(" "),
                    Span::styled(
                        display_text,
                        Style::default()
                            .fg(if is_selected { Color::White } else { Color::Gray })
                            .add_modifier(if is_selected { Modifier::BOLD | Modifier::UNDERLINED } else { Modifier::empty() })
                    ),
                ]));

                // Description with score
                lines.push(Line::from(vec![
                    Span::raw("     "),
                    Span::styled(
                        &suggestion.description,
                        Style::default().fg(if is_selected { Color::White } else { Color::DarkGray })
                    ),
                    Span::raw("  "),
                    Span::styled(score_bar, Style::default().fg(type_color)),
                    Span::styled(score_empty, Style::default().fg(Color::DarkGray)),
                    Span::styled(format!(" {}%", suggestion.match_score),
                        Style::default().fg(Color::DarkGray)),
                ]));
                lines.push(Line::from(""));
            }

            if self.search_suggestions.len() > 10 {
                lines.push(Line::from(Span::styled(
                    format!(" ... and {} more results", self.search_suggestions.len() - 10),
                    Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)
                )));
            }
        } else if !self.global_search_query.is_empty() {
            lines.push(Line::from(vec![
                Span::styled(" 💡 ", Style::default().fg(Color::Yellow)),
                Span::styled("No matches found. Try:", Style::default().fg(Color::White)),
            ]));
            lines.push(Line::from(Span::styled(
                "    • Full wallet address (32-44 chars)",
                Style::default().fg(Color::DarkGray)
            )));
            lines.push(Line::from(Span::styled(
                "    • Token symbol (e.g., SOL, USDC, BONK)",
                Style::default().fg(Color::DarkGray)
            )));
            lines.push(Line::from(Span::styled(
                "    • Program ID or transaction signature",
                Style::default().fg(Color::DarkGray)
            )));
        }

        // Footer with keyboard shortcuts
        lines.push(Line::from(""));
        lines.push(Line::from(Span::styled(
            "────────────────────────────────────────────────────────────────────────────────────────",
            Style::default().fg(Color::DarkGray)
        )));
        lines.push(Line::from(vec![
            Span::styled(" ↑↓", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::styled(" Navigate  ", Style::default().fg(Color::White)),
            Span::styled("⏎", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
            Span::styled(" Select  ", Style::default().fg(Color::White)),
            Span::styled("ESC", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
            Span::styled(" Close  ", Style::default().fg(Color::White)),
            Span::styled("Ctrl+K", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(" Search Anywhere", Style::default().fg(Color::White)),
        ]));

        let search_widget = Paragraph::new(lines)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_type(BorderType::Double)
                .border_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
                .title(vec![
                    Span::raw(" "),
                    Span::styled("⌕", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
                    Span::raw(" "),
                    Span::styled("OSVM Search", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                    Span::raw(" "),
                ]))
            .style(Style::default().bg(Color::Black));

        f.render_widget(search_widget, modal_area);
    }

    fn render_help_overlay(&mut self, f: &mut Frame, area: Rect) {
        // Center the help box
        let width = 70.min(area.width.saturating_sub(4));
        let height = 30.min(area.height.saturating_sub(4));
        let x = (area.width.saturating_sub(width)) / 2;
        let y = (area.height.saturating_sub(height)) / 2;
        let popup_area = Rect::new(x, y, width, height);

        // Clear background
        f.render_widget(Clear, popup_area);

        let help_lines = vec![
            Line::from(""),
            Line::from(Span::styled(" osvm wallet-explorer ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))),
            Line::from(Span::styled(" Real-time blockchain investigation TUI ", Style::default().fg(Color::DarkGray))),
            Line::from(""),
            Line::from(Span::styled(" ─── Navigation ──────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   1/2/3/4      Switch views (Dashboard/Graph/Logs/Search)"),
            Line::from("   Tab          Cycle through views"),
            Line::from("   ?/F1         Toggle this help"),
            Line::from("   q/Esc        Quit (or close help)"),
            Line::from("   Ctrl+C       Force quit"),
            Line::from("   Ctrl+V       Paste from clipboard (in search modal)"),
            Line::from(""),
            Line::from(Span::styled(" ─── Graph View ──────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   j/k/↑/↓      Select node up/down"),
            Line::from("   h/l/←/→      Navigate left/right"),
            Line::from("   W/A/S/D      Pan viewport up/left/down/right"),
            Line::from("   +/-          Zoom in/out"),
            Line::from("   [/]          Decrease/increase BFS exploration depth"),
            Line::from("   /            Start search (ESC to cancel)"),
            Line::from("   n/N          Next/previous search result"),
            Line::from("   y            Copy to clipboard (wallet/search results)"),
            Line::from("   Enter        Center graph on selected wallet (hop)"),
            Line::from("   Space        Expand or collapse node"),
            Line::from(""),
            Line::from(Span::styled(" ─── Logs View ───────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   j/k          Scroll line by line"),
            Line::from("   PgUp/PgDn    Scroll by page"),
            Line::from("   Home/End     Jump to start/end"),
            Line::from(""),
            Line::from(Span::styled(" ─── Legend ──────────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   🔴 Target    Red wallet being investigated"),
            Line::from("   🟢 Funding   Green wallets that funded the target"),
            Line::from("   🔵 Recipient Blue wallets that received from target"),
            Line::from("   🟣 DeFi      Magenta DEX/DeFi protocol addresses"),
            Line::from("   🟡 Token     Yellow token contract addresses"),
            Line::from(""),
            Line::from(Span::styled(" ─── About ───────────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   OSVM CLI provides AI-powered blockchain investigation"),
            Line::from("   for Solana wallets using natural language queries."),
            Line::from("   Use BFS exploration to discover connections (depth 1-20)."),
            Line::from("   Adjust exploration depth with [ and ] keys in Graph view."),
            Line::from(""),
            Line::from(Span::styled(" Use ↑/↓ or j/k to scroll this help | Press ? or Esc to close ", Style::default().fg(Color::DarkGray))),
        ];

        let total_lines = help_lines.len();
        let visible_height = (height as usize).saturating_sub(2);
        let max_scroll = total_lines.saturating_sub(visible_height);
        let actual_scroll = self.help_scroll.min(max_scroll);

        let visible_lines: Vec<Line> = help_lines
            .into_iter()
            .skip(actual_scroll)
            .take(visible_height)
            .collect();

        let help = Paragraph::new(visible_lines)
            .block(Block::default()
                .title(Span::styled(
                    format!(" Help ({}/{}) ", actual_scroll + 1, total_lines),
                    Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
                ))
                .borders(Borders::ALL)
                .border_type(BorderType::Double)
                .border_style(Style::default().fg(Color::Cyan)))
            .style(Style::default().bg(Color::Black));

        f.render_widget(help, popup_area);
    }

    /// Render AI insights panel
    fn render_ai_insights(&self, f: &mut Frame, area: Rect) {
        let insights = self.ai_insights.lock().ok();

        let block = Block::default()
            .title(Span::styled(" 💡 AI Insights ", Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner_area = block.inner(area);
        f.render_widget(block, area);

        let mut lines = Vec::new();

        if let Some(insights_vec) = insights {
            if insights_vec.is_empty() {
                lines.push(Line::from(Span::styled("  Analyzing patterns...", Style::default().fg(Color::DarkGray))));
            } else {
                for (i, insight) in insights_vec.iter().rev().take((inner_area.height as usize).saturating_sub(1)).enumerate() {
                    let color = if insight.contains("suspicious") || insight.contains("risk") {
                        Color::Red
                    } else if insight.contains("whale") || insight.contains("exchange") {
                        Color::Yellow
                    } else {
                        Color::Cyan
                    };

                    let truncated = if insight.len() > (inner_area.width as usize).saturating_sub(4) {
                        format!("{}…", &insight[..(inner_area.width as usize).saturating_sub(5)])
                    } else {
                        insight.clone()
                    };

                    lines.push(Line::from(vec![
                        Span::styled(" • ", Style::default().fg(color)),
                        Span::styled(truncated, Style::default().fg(color)),
                    ]));
                }
            }
        } else {
            lines.push(Line::from(Span::styled("  Initializing...", Style::default().fg(Color::DarkGray))));
        }

        let widget = Paragraph::new(lines);
        f.render_widget(widget, inner_area);
    }

    /// Export investigation to JSON file
    fn export_investigation(&self) -> Result<()> {
        use std::fs::File;
        use std::io::Write;

        let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let filename = format!("investigation_{}_{}.json", self.target_wallet[..8].to_string(), timestamp);

        let (nodes, edges) = self.wallet_graph.lock()
            .map(|g| (g.node_count(), g.edge_count()))
            .unwrap_or((0, 0));

        let export_data = serde_json::json!({
            "wallet": self.target_wallet,
            "timestamp": timestamp.to_string(),
            "duration_secs": self.start_time.elapsed().as_secs(),
            "stats": {
                "wallets_discovered": nodes,
                "transfers_found": edges,
                "depth_reached": self.depth_reached,
                "total_sol_in": self.total_sol_in,
                "total_sol_out": self.total_sol_out,
            },
            "insights": self.ai_insights.lock().ok().map(|i| i.clone()).unwrap_or_default(),
            "activity": self.agent_output.lock().ok().map(|a| a.clone()).unwrap_or_default(),
            "token_volumes": self.token_volumes.lock().ok().map(|t| t.clone()).unwrap_or_default(),
            "transfers": self.transfer_events.lock().ok().map(|t| t.clone()).unwrap_or_default(),
        });

        let mut file = File::create(&filename)?;
        file.write_all(serde_json::to_string_pretty(&export_data)?.as_bytes())?;

        Ok(())
    }

    /// Check if a line matches the current filter/search
    fn matches_filter(&self, line: &str) -> bool {
        // Check filter mode
        let filter_match = match self.filter_mode {
            FilterMode::All => true,
            FilterMode::Errors => line.contains("ERROR") || line.contains("error") || line.contains("⚠"),
            FilterMode::Success => line.contains("✅") || line.contains("Found") || line.contains("SUCCESS"),
            FilterMode::Transfers => line.contains("transfer") || line.contains("→"),
        };

        if !filter_match {
            return false;
        }

        // Check search query
        if !self.search_query.is_empty() {
            line.to_lowercase().contains(&self.search_query.to_lowercase())
        } else {
            true
        }
    }

    /// Render real-time network statistics panel
    fn render_network_stats_panel(&self, f: &mut Frame, area: Rect) {
        let stats = self.network_stats.lock().unwrap();
        
        // Split into columns
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
            ])
            .split(area);

        // Slot & Epoch
        let slot_text = vec![
            Line::from(Span::styled(" 📍 NETWORK ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" Slot: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", stats.current_slot), Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" Epoch: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", stats.current_epoch), Style::default().fg(Color::Yellow)),
            ]),
        ];
        let slot_widget = Paragraph::new(slot_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Cyan)));
        f.render_widget(slot_widget, chunks[0]);

        // TPS
        let tps_color = if stats.tps > 2000.0 { Color::Green } 
                       else if stats.tps > 1000.0 { Color::Yellow } 
                       else { Color::Red };
        let tps_text = vec![
            Line::from(Span::styled(" ⚡ PERFORMANCE ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" TPS: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{:.0}", stats.tps), Style::default().fg(tps_color).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" Block: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}ms", stats.block_time_ms), Style::default().fg(Color::Cyan)),
            ]),
        ];
        let tps_widget = Paragraph::new(tps_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Yellow)));
        f.render_widget(tps_widget, chunks[1]);

        // Transactions
        let tx_text = vec![
            Line::from(Span::styled(" 📊 ACTIVITY ", Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" Total TX: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", stats.total_transactions), Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" Validators: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", stats.active_validators), Style::default().fg(Color::Green)),
            ]),
        ];
        let tx_widget = Paragraph::new(tx_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Magenta)));
        f.render_widget(tx_widget, chunks[2]);

        // Health
        let health_color = if stats.health == "ok" { Color::Green } else { Color::Red };
        let health_status = stats.health.to_uppercase();
        let refresh_secs = self.last_refresh.elapsed().as_secs();
        let health_text = vec![
            Line::from(Span::styled(" 🏥 HEALTH ", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" Status: ", Style::default().fg(Color::DarkGray)),
                Span::styled(health_status, Style::default().fg(health_color).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" Refresh: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}s ago", refresh_secs), Style::default().fg(Color::Cyan)),
            ]),
        ];
        let health_widget = Paragraph::new(health_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Green)));
        f.render_widget(health_widget, chunks[3]);
    }
    /// Render Search Results Tab - shows actual findings from indexed data
    fn render_search_results_tab(&mut self, f: &mut Frame, area: Rect) {
        let results_data = self.search_results_data.lock().unwrap().clone();

        // Main layout: [Header][Results][Status]
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(4),  // Search info header
                Constraint::Min(0),      // Results
                Constraint::Length(2),   // Status bar
            ])
            .split(area);

        // Search Info Header
        let header_text = vec![
            Line::from(vec![
                Span::styled(" 🔍 Search Query: ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
                Span::styled(&results_data.query, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" 📊 Type: ", Style::default().fg(Color::Cyan)),
                Span::styled(&results_data.entity_type, Style::default().fg(Color::Green)),
                Span::styled("  •  Matches: ", Style::default().fg(Color::Cyan)),
                Span::styled(results_data.total_matches.to_string(), Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
                Span::styled("  •  Time: ", Style::default().fg(Color::Cyan)),
                Span::styled(&results_data.search_timestamp, Style::default().fg(Color::DarkGray)),
            ]),
        ];

        let header = Paragraph::new(header_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Cyan)));
        f.render_widget(header, chunks[0]);

        // Results sections
        let mut result_lines = Vec::new();

        if results_data.total_matches == 0 {
            result_lines.push(Line::from(""));
            result_lines.push(Line::from(Span::styled(
                " No matches found in indexed data.",
                Style::default().fg(Color::Yellow).add_modifier(Modifier::ITALIC)
            )));
            result_lines.push(Line::from(""));
            result_lines.push(Line::from(Span::styled(
                " Try:", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)
            )));
            result_lines.push(Line::from(Span::styled(
                "   • Wait for more data to be indexed from blockchain",
                Style::default().fg(Color::DarkGray)
            )));
            result_lines.push(Line::from(Span::styled(
                "   • Search for different terms (wallet address, token symbol)",
                Style::default().fg(Color::DarkGray)
            )));
            result_lines.push(Line::from(Span::styled(
                "   • Press Ctrl+K to open search again",
                Style::default().fg(Color::DarkGray)
            )));
        } else {
            // Wallets Section
            if !results_data.wallets_found.is_empty() {
                result_lines.push(Line::from(""));
                result_lines.push(Line::from(Span::styled(
                    format!(" ━━━ Wallets ({}) ━━━", results_data.wallets_found.len()),
                    Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)
                )));
                result_lines.push(Line::from(""));

                for wallet in &results_data.wallets_found {
                    result_lines.push(Line::from(vec![
                        Span::styled(" 👛 ", Style::default().fg(Color::Green)),
                        Span::styled(&wallet.address, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
                    ]));
                    result_lines.push(Line::from(vec![
                        Span::raw("    "),
                        Span::styled("Balance: ", Style::default().fg(Color::DarkGray)),
                        Span::styled(format!("{:.4} SOL", wallet.balance_sol), Style::default().fg(Color::Yellow)),
                        Span::raw("  •  "),
                        Span::styled("Transfers: ", Style::default().fg(Color::DarkGray)),
                        Span::styled(wallet.transfer_count.to_string(), Style::default().fg(Color::Cyan)),
                    ]));
                    result_lines.push(Line::from(vec![
                        Span::raw("    "),
                        Span::styled(&wallet.match_reason, Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)),
                    ]));
                    result_lines.push(Line::from(""));
                }
            }

            // Tokens Section
            if !results_data.tokens_found.is_empty() {
                result_lines.push(Line::from(Span::styled(
                    format!(" ━━━ Tokens ({}) ━━━", results_data.tokens_found.len()),
                    Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
                )));
                result_lines.push(Line::from(""));

                for token in &results_data.tokens_found {
                    result_lines.push(Line::from(vec![
                        Span::styled(" 🪙 ", Style::default().fg(Color::Yellow)),
                        Span::styled(&token.symbol, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
                    ]));
                    result_lines.push(Line::from(vec![
                        Span::raw("    "),
                        Span::styled("Volume: ", Style::default().fg(Color::DarkGray)),
                        Span::styled(format!("{:.2}", token.volume), Style::default().fg(Color::Cyan)),
                        Span::raw("  •  "),
                        Span::styled(&token.match_reason, Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)),
                    ]));
                    result_lines.push(Line::from(""));
                }
            }

            // Transactions Section
            if !results_data.transactions_found.is_empty() {
                result_lines.push(Line::from(Span::styled(
                    format!(" ━━━ Transactions ({}) ━━━", results_data.transactions_found.len()),
                    Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
                )));
                result_lines.push(Line::from(""));

                for tx in &results_data.transactions_found {
                    result_lines.push(Line::from(vec![
                        Span::styled(" 📜 ", Style::default().fg(Color::Cyan)),
                        Span::styled(&tx.signature, Style::default().fg(Color::White)),
                    ]));
                    result_lines.push(Line::from(vec![
                        Span::raw("    "),
                        Span::styled(&tx.timestamp, Style::default().fg(Color::DarkGray)),
                        Span::raw("  •  "),
                        Span::styled(format!("{:.4} SOL", tx.amount_sol), Style::default().fg(Color::Yellow)),
                    ]));
                    result_lines.push(Line::from(vec![
                        Span::raw("    "),
                        Span::styled(&tx.match_reason, Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)),
                    ]));
                    result_lines.push(Line::from(""));
                }
            }
        }

        let results_paragraph = Paragraph::new(result_lines)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::White))
                .title(Span::styled(" Search Results ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))))
            .scroll((self.search_results_scroll as u16, 0));

        f.render_widget(results_paragraph, chunks[1]);

        // Status line (simplified)
        let status_text = Line::from(vec![
            Span::styled(" Search Results ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::raw(" | "),
            Span::styled(format!("{} matches", results_data.total_matches), Style::default().fg(Color::Yellow)),
            Span::raw(" | Press "),
            Span::styled("Ctrl+K", Style::default().fg(Color::Green)),
            Span::raw(" to search again"),
        ]);
        let status = Paragraph::new(status_text);
        f.render_widget(status, chunks[2]);
    }
}
