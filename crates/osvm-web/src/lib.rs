//! OSVM Research TUI - Browser Version with Live RPC
//!
//! High-performance blockchain investigation in the browser.
//! - DOM or WebGL2 backend for rendering
//! - Live Solana RPC calls via gloo-net
//! - Real-time transfer visualization
//! - Reuses WalletGraph from main TUI (2,600+ lines!)

pub mod graph;

use std::{cell::RefCell, rc::Rc};
use ratzilla::ratatui::{
    layout::{Constraint, Direction, Layout, Rect, Alignment},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, List, ListItem, BorderType, Gauge},
    Frame, Terminal,
};
use ratzilla::{event::KeyCode, DomBackend, WebRenderer};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen::closure::Closure;
use wasm_bindgen_futures::spawn_local;
use gloo_net::http::Request;
use serde::{Deserialize, Serialize};

// Import the real WalletGraph from our copied graph.rs module
use graph::{WalletGraph, WalletNodeType, GraphInput};

// ═══════════════════════════════════════════════════════════════════════════
// URL Parameter Parsing for Multi-Wallet Mode
// ═══════════════════════════════════════════════════════════════════════════

/// Parse wallet addresses from URL parameters
/// Supports: ?wallet=ADDRESS or ?wallets=A,B,C
fn get_wallets_from_url() -> Vec<String> {
    let default = vec!["7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZRuJosgAsU".to_string()];

    let window = match web_sys::window() {
        Some(w) => w,
        None => return default,
    };

    let location = match window.location().href() {
        Ok(href) => href,
        Err(_) => return default,
    };

    // Parse URL to get search params
    let url = match web_sys::Url::new(&location) {
        Ok(u) => u,
        Err(_) => return default,
    };

    let params = url.search_params();

    // Check for single wallet first
    if let Some(wallet) = params.get("wallet") {
        if !wallet.is_empty() && wallet.len() >= 32 && wallet.len() <= 44 {
            log::info!("URL param: wallet={}", wallet);
            return vec![wallet];
        }
    }

    // Check for multiple wallets (comma-separated)
    if let Some(wallets) = params.get("wallets") {
        let addrs: Vec<String> = wallets
            .split(',')
            .filter(|s| !s.is_empty() && s.len() >= 32 && s.len() <= 44)
            .map(|s| s.trim().to_string())
            .collect();

        if !addrs.is_empty() {
            log::info!("URL param: wallets={:?}", addrs);
            return addrs;
        }
    }

    default
}

// ═══════════════════════════════════════════════════════════════════════════
// URL Hash State Persistence (shareable links!)
// ═══════════════════════════════════════════════════════════════════════════

/// View state that can be serialized to URL hash
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ViewState {
    x: f64,      // viewport X offset
    y: f64,      // viewport Y offset
    z: f64,      // zoom level
    #[serde(skip_serializing_if = "Option::is_none")]
    sel: Option<usize>, // selected node index
}

/// Parse view state from URL hash (e.g., #x=100&y=50&z=1.5)
fn get_view_from_hash() -> Option<ViewState> {
    let window = web_sys::window()?;
    let hash = window.location().hash().ok()?;

    if hash.len() <= 1 {
        return None;
    }

    // Remove leading # and parse as query params
    let hash_content = &hash[1..];
    let params = web_sys::UrlSearchParams::new_with_str(hash_content).ok()?;

    Some(ViewState {
        x: params.get("x").and_then(|v| v.parse().ok()).unwrap_or(0.0),
        y: params.get("y").and_then(|v| v.parse().ok()).unwrap_or(0.0),
        z: params.get("z").and_then(|v| v.parse().ok()).unwrap_or(1.0),
        sel: params.get("sel").and_then(|v| v.parse().ok()),
    })
}

/// Save view state to URL hash (without page reload)
fn save_view_to_hash(viewport: (f64, f64, f64), selected: Option<usize>) {
    if let Some(window) = web_sys::window() {
        let hash = if let Some(sel) = selected {
            format!("#x={:.1}&y={:.1}&z={:.2}&sel={}", viewport.0, viewport.1, viewport.2, sel)
        } else {
            format!("#x={:.1}&y={:.1}&z={:.2}", viewport.0, viewport.1, viewport.2)
        };

        // Use replaceState to update hash without adding history entry
        if let Ok(history) = window.history() {
            let _ = history.replace_state_with_url(&JsValue::NULL, "", Some(&hash));
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Touch/Gesture State for Mobile Support
// ═══════════════════════════════════════════════════════════════════════════

/// Track touch state for pinch-to-zoom and pan gestures
#[derive(Debug, Clone, Default)]
pub struct TouchState {
    /// Active touches for tracking multi-touch
    active_touches: Vec<(i32, f64, f64)>, // (identifier, x, y)
    /// Initial pinch distance for zoom calculation
    initial_pinch_distance: Option<f64>,
    /// Initial zoom when pinch started
    initial_zoom: f64,
    /// Last known position for single-finger pan
    last_position: Option<(f64, f64)>,
    /// Whether we're currently in a pinch gesture
    is_pinching: bool,
}

impl TouchState {
    fn new() -> Self {
        Self::default()
    }

    /// Calculate distance between two touches (for pinch zoom)
    fn pinch_distance(&self) -> Option<f64> {
        if self.active_touches.len() >= 2 {
            let t0 = &self.active_touches[0];
            let t1 = &self.active_touches[1];
            let dx = t1.1 - t0.1;
            let dy = t1.2 - t0.2;
            Some((dx * dx + dy * dy).sqrt())
        } else {
            None
        }
    }

    /// Get center point of all touches (for pan)
    fn center(&self) -> Option<(f64, f64)> {
        if self.active_touches.is_empty() {
            return None;
        }
        let sum_x: f64 = self.active_touches.iter().map(|t| t.1).sum();
        let sum_y: f64 = self.active_touches.iter().map(|t| t.2).sum();
        let n = self.active_touches.len() as f64;
        Some((sum_x / n, sum_y / n))
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Solana RPC Types (serde needs all fields even if unused)
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Serialize)]
struct RpcRequest {
    jsonrpc: &'static str,
    id: u32,
    method: &'static str,
    params: Vec<serde_json::Value>,
}

#[derive(Deserialize, Debug)]
struct RpcResponse<T> {
    result: Option<T>,
    error: Option<RpcError>,
}

#[derive(Deserialize, Debug)]
struct RpcError {
    code: i64,
    message: String,
}

#[derive(Deserialize, Debug)]
struct GetBalanceResult {
    value: u64,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
struct SignatureInfo {
    signature: String,
    slot: u64,
    #[serde(default)]
    err: Option<serde_json::Value>,
    #[serde(default)]
    memo: Option<String>,
    #[serde(rename = "blockTime")]
    block_time: Option<i64>,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
struct TransactionMeta {
    err: Option<serde_json::Value>,
    fee: u64,
    #[serde(rename = "preBalances")]
    pre_balances: Vec<u64>,
    #[serde(rename = "postBalances")]
    post_balances: Vec<u64>,
}

#[derive(Deserialize, Debug)]
struct TransactionMessage {
    #[serde(rename = "accountKeys")]
    account_keys: Vec<String>,
}

#[derive(Deserialize, Debug)]
struct TransactionData {
    message: TransactionMessage,
}

#[allow(dead_code)]
#[derive(Deserialize, Debug)]
struct GetTransactionResult {
    slot: u64,
    transaction: TransactionData,
    meta: Option<TransactionMeta>,
    #[serde(rename = "blockTime")]
    block_time: Option<i64>,
}

// ═══════════════════════════════════════════════════════════════════════════
// App Data Types
// ═══════════════════════════════════════════════════════════════════════════

/// Transfer between wallets
#[derive(Clone, Debug)]
pub struct Transfer {
    pub from: String,
    pub to: String,
    pub amount: f64,
    pub token: String,
    pub signature: String,
    pub timestamp: Option<i64>,
}

/// Wallet node in the graph
#[derive(Clone, Debug)]
pub struct WalletNode {
    pub address: String,
    pub label: Option<String>,
    pub balance: f64,
    pub risk_score: u8,
    pub is_center: bool,
    pub tx_count: usize,
}

/// Loading state
#[derive(Clone, Debug, PartialEq)]
pub enum LoadingState {
    Idle,
    Loading(String),
    Error(String),
    Complete,
}

/// App state
pub struct AppState {
    pub frame: usize,
    pub wallets: Vec<WalletNode>,
    pub transfers: Vec<Transfer>,
    pub selected_wallet: usize,
    pub scroll_offset: i32,
    pub zoom: f32,
    pub show_help: bool,
    pub loading: LoadingState,
    pub target_wallet: String,
    pub rpc_url: String,
    pub last_update: Option<i64>,
    // Real WalletGraph from graph.rs (2,600+ lines of visualization!)
    pub wallet_graph: WalletGraph,
    // Touch state for mobile gestures
    pub touch_state: TouchState,
    // Frame counter for throttling hash updates
    pub hash_update_counter: u8,
    // Start time for help hint animation (ms from Performance.now())
    pub start_time: f64,
}

impl Default for AppState {
    fn default() -> Self {
        // Get target from URL or use default
        // Supports: ?wallet=ADDRESS or ?wallets=A,B,C (first is target)
        let wallets = get_wallets_from_url();
        let target = wallets.first()
            .cloned()
            .unwrap_or_else(|| "7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZRuJosgAsU".to_string());

        // Restore view state from URL hash if present
        let view_state = get_view_from_hash();
        let initial_viewport = view_state
            .as_ref()
            .map(|v| (v.x, v.y, v.z))
            .unwrap_or((0.0, 0.0, 1.0));
        let initial_selection = view_state.as_ref().and_then(|v| v.sel).unwrap_or(0);

        let mut wallet_graph = WalletGraph::new(target.clone());
        // Apply restored viewport
        wallet_graph.viewport = initial_viewport;

        // Get start time for help hint animation
        let start_time = web_sys::window()
            .and_then(|w| w.performance())
            .map(|p| p.now())
            .unwrap_or(0.0);

        Self {
            frame: 0,
            wallets: vec![],
            transfers: vec![],
            selected_wallet: initial_selection,
            scroll_offset: 0,
            zoom: initial_viewport.2 as f32,
            show_help: false,
            loading: LoadingState::Idle,
            target_wallet: target,
            rpc_url: "https://osvm.ai/api/proxy/rpc".to_string(),
            last_update: None,
            wallet_graph,
            touch_state: TouchState::new(),
            hash_update_counter: 0,
            start_time,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Solana RPC Client
// ═══════════════════════════════════════════════════════════════════════════

async fn fetch_balance(rpc_url: &str, address: &str) -> Result<f64, String> {
    let request = RpcRequest {
        jsonrpc: "2.0",
        id: 1,
        method: "getBalance",
        params: vec![serde_json::json!(address)],
    };

    let response = Request::post(rpc_url)
        .header("Content-Type", "application/json")
        .json(&request)
        .map_err(|e| format!("Request build error: {:?}", e))?
        .send()
        .await
        .map_err(|e| format!("Network error: {:?}", e))?;

    let data: RpcResponse<GetBalanceResult> = response
        .json()
        .await
        .map_err(|e| format!("Parse error: {:?}", e))?;

    if let Some(err) = data.error {
        return Err(format!("RPC error: {}", err.message));
    }

    Ok(data.result.map(|r| r.value as f64 / 1_000_000_000.0).unwrap_or(0.0))
}

async fn fetch_signatures(rpc_url: &str, address: &str, limit: usize) -> Result<Vec<SignatureInfo>, String> {
    let request = RpcRequest {
        jsonrpc: "2.0",
        id: 1,
        method: "getSignaturesForAddress",
        params: vec![
            serde_json::json!(address),
            serde_json::json!({"limit": limit}),
        ],
    };

    let response = Request::post(rpc_url)
        .header("Content-Type", "application/json")
        .json(&request)
        .map_err(|e| format!("Request build error: {:?}", e))?
        .send()
        .await
        .map_err(|e| format!("Network error: {:?}", e))?;

    let data: RpcResponse<Vec<SignatureInfo>> = response
        .json()
        .await
        .map_err(|e| format!("Parse error: {:?}", e))?;

    if let Some(err) = data.error {
        return Err(format!("RPC error: {}", err.message));
    }

    Ok(data.result.unwrap_or_default())
}

async fn fetch_transaction(rpc_url: &str, signature: &str) -> Result<Option<GetTransactionResult>, String> {
    let request = RpcRequest {
        jsonrpc: "2.0",
        id: 1,
        method: "getTransaction",
        params: vec![
            serde_json::json!(signature),
            serde_json::json!({"encoding": "json", "maxSupportedTransactionVersion": 0}),
        ],
    };

    let response = Request::post(rpc_url)
        .header("Content-Type", "application/json")
        .json(&request)
        .map_err(|e| format!("Request build error: {:?}", e))?
        .send()
        .await
        .map_err(|e| format!("Network error: {:?}", e))?;

    let data: RpcResponse<GetTransactionResult> = response
        .json()
        .await
        .map_err(|e| format!("Parse error: {:?}", e))?;

    if let Some(err) = data.error {
        // Not found is not an error
        if err.code == -32009 || err.message.contains("not found") {
            return Ok(None);
        }
        return Err(format!("RPC error: {}", err.message));
    }

    Ok(data.result)
}

/// Load wallet data from Solana RPC
async fn load_wallet_data(state: Rc<RefCell<AppState>>) {
    let (rpc_url, target) = {
        let s = state.borrow();
        (s.rpc_url.clone(), s.target_wallet.clone())
    };

    // Set loading state
    state.borrow_mut().loading = LoadingState::Loading("Fetching balance...".into());

    // Fetch balance
    let balance = match fetch_balance(&rpc_url, &target).await {
        Ok(b) => b,
        Err(e) => {
            state.borrow_mut().loading = LoadingState::Error(e);
            return;
        }
    };

    state.borrow_mut().loading = LoadingState::Loading("Fetching transactions...".into());

    // Fetch recent signatures
    let signatures = match fetch_signatures(&rpc_url, &target, 10).await {
        Ok(s) => s,
        Err(e) => {
            state.borrow_mut().loading = LoadingState::Error(e);
            return;
        }
    };

    // Create center wallet
    let center_wallet = WalletNode {
        address: target.clone(),
        label: Some("Investigation Target".into()),
        balance,
        risk_score: 50, // Will calculate based on patterns
        is_center: true,
        tx_count: signatures.len(),
    };

    let mut wallets = vec![center_wallet];
    let mut transfers = vec![];
    let mut seen_addresses = std::collections::HashSet::new();
    seen_addresses.insert(target.clone());

    // Fetch transaction details for each signature and populate WalletGraph
    for (i, sig_info) in signatures.iter().take(5).enumerate() {
        state.borrow_mut().loading = LoadingState::Loading(
            format!("Parsing tx {}/{}...", i + 1, signatures.len().min(5))
        );

        if let Ok(Some(tx)) = fetch_transaction(&rpc_url, &sig_info.signature).await {
            let accounts = &tx.transaction.message.account_keys;

            if accounts.len() >= 2 {
                // Simple heuristic: first account is usually sender, second is receiver
                let from = &accounts[0];
                let to = if accounts.len() > 1 { &accounts[1] } else { from };

                // Calculate transfer amount from balance changes
                let amount = if let Some(meta) = &tx.meta {
                    if meta.pre_balances.len() > 1 && meta.post_balances.len() > 1 {
                        let diff = meta.pre_balances[0] as i64 - meta.post_balances[0] as i64;
                        (diff.abs() as f64 - meta.fee as f64) / 1_000_000_000.0
                    } else {
                        0.0
                    }
                } else {
                    0.0
                };

                // Determine node types based on flow direction relative to target
                let (from_type, to_type) = if *from == target {
                    (WalletNodeType::Target, WalletNodeType::Recipient)
                } else if *to == target {
                    (WalletNodeType::Funding, WalletNodeType::Target)
                } else {
                    (WalletNodeType::Funding, WalletNodeType::Recipient)
                };

                // Add to WalletGraph!
                {
                    let mut s = state.borrow_mut();
                    s.wallet_graph.add_transfer(
                        from.clone(),
                        to.clone(),
                        amount,
                        "SOL".to_string(),
                        from_type,
                        to_type,
                        sig_info.block_time.map(|t| t.to_string()),
                        Some(sig_info.signature.clone()),
                    );
                }

                // Add transfer to simple list (kept for backwards compat)
                transfers.push(Transfer {
                    from: from.clone(),
                    to: to.clone(),
                    amount,
                    token: "SOL".into(),
                    signature: sig_info.signature.clone(),
                    timestamp: sig_info.block_time,
                });

                // Add new wallets
                for addr in [from, to] {
                    if !seen_addresses.contains(addr) {
                        seen_addresses.insert(addr.clone());
                        wallets.push(WalletNode {
                            address: addr.clone(),
                            label: None,
                            balance: 0.0, // Could fetch if needed
                            risk_score: 30,
                            is_center: false,
                            tx_count: 0,
                        });
                    }
                }
            }
        }
    }

    // Update state
    {
        let mut s = state.borrow_mut();
        s.wallets = wallets;
        s.transfers = transfers;
        s.loading = LoadingState::Complete;
        s.last_update = Some(0);
        // Note: positions are calculated during render()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Rendering
// ═══════════════════════════════════════════════════════════════════════════

fn abbreviate_address(addr: &str) -> String {
    if addr.len() > 8 {
        format!("{}...{}", &addr[..4], &addr[addr.len()-4..])
    } else {
        addr.to_string()
    }
}

fn risk_color(score: u8) -> Color {
    match score {
        0..=25 => Color::Green,
        26..=50 => Color::Yellow,
        51..=75 => Color::Rgb(255, 165, 0),
        _ => Color::Red,
    }
}

fn render_header(f: &mut Frame, area: Rect, state: &AppState) {
    let status = match &state.loading {
        LoadingState::Idle => "Ready".to_string(),
        LoadingState::Loading(msg) => format!("Loading: {}", msg),
        LoadingState::Error(e) => format!("Error: {}", e),
        LoadingState::Complete => format!("{} wallets, {} transfers", state.wallets.len(), state.transfers.len()),
    };

    let header_text = vec![
        Line::from(vec![
            Span::styled("OSVM ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled("Research", Style::default().fg(Color::White)),
            Span::raw(" | "),
            Span::styled("WebGL2", Style::default().fg(Color::Magenta)),
            Span::raw(" | "),
            Span::styled(status, Style::default().fg(Color::Yellow)),
        ]),
    ];

    let header = Paragraph::new(header_text)
        .style(Style::default())
        .block(Block::default()
            .borders(Borders::BOTTOM)
            .border_style(Style::default().fg(Color::DarkGray)));

    f.render_widget(header, area);
}

// NOTE: render_graph was removed - we now use WalletGraph::render() from graph.rs

fn render_wallet_list(f: &mut Frame, area: Rect, state: &AppState) {
    let block = Block::default()
        .title(" Wallets ")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::Blue));

    if state.wallets.is_empty() {
        let empty = Paragraph::new("No wallets loaded")
            .style(Style::default().fg(Color::DarkGray))
            .block(block);
        f.render_widget(empty, area);
        return;
    }

    let items: Vec<ListItem> = state.wallets
        .iter()
        .enumerate()
        .map(|(i, wallet)| {
            let risk_indicator = match wallet.risk_score {
                0..=25 => "[OK]",
                26..=50 => "[--]",
                51..=75 => "[!!]",
                _ => "[XX]",
            };

            let label = wallet.label.as_deref().unwrap_or("Unknown");
            let addr = abbreviate_address(&wallet.address);

            let style = if i == state.selected_wallet {
                Style::default().fg(Color::Black).bg(Color::Cyan)
            } else if wallet.is_center {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::White)
            };

            let balance_str = if wallet.balance > 0.0 {
                format!(" {:.2} SOL", wallet.balance)
            } else {
                String::new()
            };

            ListItem::new(Line::from(vec![
                Span::styled(risk_indicator, Style::default().fg(risk_color(wallet.risk_score))),
                Span::raw(" "),
                Span::styled(format!("{:<14}", label), style),
                Span::styled(format!(" {}", addr), Style::default().fg(Color::DarkGray)),
                Span::styled(balance_str, Style::default().fg(Color::Green)),
            ]))
        })
        .collect();

    let list = List::new(items)
        .block(block)
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED));

    f.render_widget(list, area);
}

fn render_insights(f: &mut Frame, area: Rect, state: &AppState) {
    let block = Block::default()
        .title(" AI Insights ")
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::Magenta));

    let inner = block.inner(area);
    f.render_widget(block, area);

    // Calculate risk based on patterns
    let total_volume: f64 = state.transfers.iter().map(|t| t.amount).sum();
    let unique_wallets = state.wallets.len();
    let risk_score = if state.transfers.is_empty() {
        0
    } else {
        ((unique_wallets as f64 / state.transfers.len() as f64) * 30.0 +
         (total_volume.log10() * 10.0).min(40.0)) as u16
    }.min(100);

    let gauge = Gauge::default()
        .block(Block::default().title("Risk Score"))
        .gauge_style(Style::default().fg(risk_color(risk_score as u8)))
        .percent(risk_score)
        .label(format!("{}%", risk_score));

    let gauge_area = Rect::new(inner.x, inner.y, inner.width, 3);
    f.render_widget(gauge, gauge_area);

    let insights = if state.transfers.is_empty() {
        vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("* ", Style::default().fg(Color::Yellow)),
                Span::raw("Press [r] to load data"),
            ]),
        ]
    } else {
        vec![
            Line::from(""),
            Line::from(vec![
                Span::styled("* ", Style::default().fg(Color::Yellow)),
                Span::raw(format!("{} transfers analyzed", state.transfers.len())),
            ]),
            Line::from(vec![
                Span::styled("* ", Style::default().fg(Color::Yellow)),
                Span::raw(format!("{:.4} SOL total volume", total_volume)),
            ]),
            Line::from(vec![
                Span::styled("* ", Style::default().fg(Color::Yellow)),
                Span::raw(format!("{} unique wallets", unique_wallets)),
            ]),
            Line::from(vec![
                Span::styled("* ", Style::default().fg(Color::Cyan)),
                Span::raw("Live data via osvm.ai proxy"),
            ]),
        ]
    };

    let insights_area = Rect::new(inner.x, inner.y + 3, inner.width, inner.height.saturating_sub(3));
    let insights_widget = Paragraph::new(insights)
        .style(Style::default().fg(Color::White));

    f.render_widget(insights_widget, insights_area);
}

fn render_help(f: &mut Frame, area: Rect, state: &AppState) {
    // Check if we should show the animated help hint (first 5 seconds)
    let current_time = web_sys::window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0);
    let elapsed_ms = current_time - state.start_time;
    let show_hint = elapsed_ms < 5000.0; // Show for first 5 seconds

    // Pulsing effect: visible on even frame groups (creates ~2 Hz blink at 60fps)
    let pulse_visible = (state.frame / 15) % 2 == 0;

    let mut spans = vec![
        Span::styled("[?]", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        Span::raw(" Help  "),
    ];

    // Add animated hint during first 5 seconds
    if show_hint && pulse_visible {
        spans.push(Span::styled(
            "◀ Press ? for shortcuts! ",
            Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
        ));
    }

    spans.extend(vec![
        Span::styled("[r]", Style::default().fg(Color::Green)),
        Span::raw(" Fetch  "),
        Span::styled("[j/k]", Style::default().fg(Color::Cyan)),
        Span::raw(" Nav  "),
        Span::styled("[Enter]", Style::default().fg(Color::Magenta)),
        Span::raw(" Follow  "),
        Span::styled("[Bksp]", Style::default().fg(Color::Magenta)),
        Span::raw(" Back  "),
        Span::styled("[+/-]", Style::default().fg(Color::Yellow)),
        Span::raw(" Zoom  "),
        Span::styled("[0]", Style::default().fg(Color::Yellow)),
        Span::raw(" Reset"),
    ]);

    let help_text = vec![Line::from(spans)];

    let help = Paragraph::new(help_text)
        .style(Style::default().fg(Color::DarkGray))
        .alignment(Alignment::Center)
        .block(Block::default().borders(Borders::TOP).border_style(Style::default().fg(Color::DarkGray)));

    f.render_widget(help, area);
}

/// Render full help overlay modal when ? is pressed
fn render_help_overlay(f: &mut Frame) {
    // Calculate centered overlay area (60% width, 70% height)
    let area = f.area();
    let overlay_width = (area.width as f32 * 0.7) as u16;
    let overlay_height = (area.height as f32 * 0.8) as u16;
    let x = (area.width - overlay_width) / 2;
    let y = (area.height - overlay_height) / 2;
    let overlay_area = Rect::new(x, y, overlay_width, overlay_height);

    // Clear area (semi-transparent background effect via block)
    let clear = Block::default()
        .style(Style::default().bg(Color::Black));
    f.render_widget(clear, area);

    // Help content
    let help_lines = vec![
        Line::from(vec![
            Span::styled("  OSVM Research TUI - Keyboard Shortcuts  ",
                Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  NAVIGATION", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    j / ↓     ", Style::default().fg(Color::Green)),
            Span::raw("Move selection down"),
        ]),
        Line::from(vec![
            Span::styled("    k / ↑     ", Style::default().fg(Color::Green)),
            Span::raw("Move selection up"),
        ]),
        Line::from(vec![
            Span::styled("    h / ←     ", Style::default().fg(Color::Green)),
            Span::raw("Pan graph left"),
        ]),
        Line::from(vec![
            Span::styled("    l / →     ", Style::default().fg(Color::Green)),
            Span::raw("Pan graph right"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  GRAPH EXPLORATION", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    Enter     ", Style::default().fg(Color::Magenta)),
            Span::raw("Follow money flow (next edge)"),
        ]),
        Line::from(vec![
            Span::styled("    Backspace ", Style::default().fg(Color::Magenta)),
            Span::raw("Go back to previous node"),
        ]),
        Line::from(vec![
            Span::styled("    Space     ", Style::default().fg(Color::Magenta)),
            Span::raw("Select current node"),
        ]),
        Line::from(vec![
            Span::styled("    Esc       ", Style::default().fg(Color::Magenta)),
            Span::raw("Cancel edge selection"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  ZOOM & VIEW", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    + / =     ", Style::default().fg(Color::Cyan)),
            Span::raw("Zoom in"),
        ]),
        Line::from(vec![
            Span::styled("    - / _     ", Style::default().fg(Color::Cyan)),
            Span::raw("Zoom out"),
        ]),
        Line::from(vec![
            Span::styled("    0         ", Style::default().fg(Color::Cyan)),
            Span::raw("Reset view to center"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  DATA", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    r / R     ", Style::default().fg(Color::Green)),
            Span::raw("Fetch live data from Solana"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  TOUCH GESTURES (Mobile)", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    Pinch     ", Style::default().fg(Color::Blue)),
            Span::raw("Zoom in/out"),
        ]),
        Line::from(vec![
            Span::styled("    1-finger  ", Style::default().fg(Color::Blue)),
            Span::raw("Pan view"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  URL PARAMETERS", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(vec![
            Span::styled("    ?wallet=  ", Style::default().fg(Color::White)),
            Span::raw("Load specific wallet address"),
        ]),
        Line::from(vec![
            Span::styled("    ?wallets= ", Style::default().fg(Color::White)),
            Span::raw("Load multiple wallets (comma-separated)"),
        ]),
        Line::from(vec![
            Span::styled("    #x=&y=&z= ", Style::default().fg(Color::White)),
            Span::raw("Restore viewport state (shareable)"),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  Press ", Style::default().fg(Color::DarkGray)),
            Span::styled("?", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            Span::styled(" or ", Style::default().fg(Color::DarkGray)),
            Span::styled("Esc", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            Span::styled(" to close", Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let help_paragraph = Paragraph::new(help_lines)
        .block(Block::default()
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::Cyan))
            .style(Style::default().bg(Color::Black)));

    f.render_widget(help_paragraph, overlay_area);
}

fn ui(f: &mut Frame, state: &mut AppState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(2),
            Constraint::Min(10),
            Constraint::Length(2),
        ])
        .split(f.area());

    render_header(f, chunks[0], state);

    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(60),
            Constraint::Percentage(40),
        ])
        .split(chunks[1]);

    // Use the REAL WalletGraph renderer from graph.rs (2,600+ lines of viz!)
    state.wallet_graph.render(f, main_chunks[0]);

    let side_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(50),
            Constraint::Percentage(50),
        ])
        .split(main_chunks[1]);

    render_wallet_list(f, side_chunks[0], state);
    render_insights(f, side_chunks[1], state);

    render_help(f, chunks[2], state);

    // Help overlay (renders on top of everything when show_help is true)
    if state.show_help {
        render_help_overlay(f);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// WASM Entry Point
// ═══════════════════════════════════════════════════════════════════════════

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    console_log::init_with_level(log::Level::Info).ok();

    log::info!("OSVM Research TUI (DOM) starting...");

    let state = Rc::new(RefCell::new(AppState::default()));

    // Create DOM backend (simpler, works everywhere)
    let backend = DomBackend::new()
        .map_err(|e| JsValue::from_str(&format!("DOM backend error: {:?}", e)))?;

    let terminal = Terminal::new(backend)
        .map_err(|e| JsValue::from_str(&format!("Terminal error: {:?}", e)))?;

    // Handle keyboard input - wired to WalletGraph controls
    let state_for_keys = Rc::clone(&state);
    terminal.on_key_event(move |key_event| {
        let mut s = state_for_keys.borrow_mut();
        match key_event.code {
            // Navigate wallet list + sync graph selection
            KeyCode::Up | KeyCode::Char('k') => {
                if s.selected_wallet > 0 {
                    s.selected_wallet -= 1;
                }
                // Sync graph selection - clone address to satisfy borrow checker
                let addr = s.wallets.get(s.selected_wallet).map(|w| w.address.clone());
                if let Some(address) = addr {
                    s.wallet_graph.select_by_address(&address);
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                if !s.wallets.is_empty() && s.selected_wallet < s.wallets.len() - 1 {
                    s.selected_wallet += 1;
                }
                // Sync graph selection - clone address to satisfy borrow checker
                let addr = s.wallets.get(s.selected_wallet).map(|w| w.address.clone());
                if let Some(address) = addr {
                    s.wallet_graph.select_by_address(&address);
                }
            }
            // Refresh data from RPC
            KeyCode::Char('r') | KeyCode::Char('R') => {
                if s.loading != LoadingState::Loading("".into()) {
                    let state_clone = Rc::clone(&state_for_keys);
                    drop(s); // Release borrow before spawn
                    spawn_local(async move {
                        load_wallet_data(state_clone).await;
                    });
                    return;
                }
            }
            // Pan graph left/right
            KeyCode::Left | KeyCode::Char('h') => {
                s.scroll_offset -= 1;
                s.wallet_graph.viewport.0 -= 30.0;
            }
            KeyCode::Right | KeyCode::Char('l') => {
                s.scroll_offset += 1;
                s.wallet_graph.viewport.0 += 30.0;
            }
            // Zoom controls for WalletGraph
            KeyCode::Char('+') | KeyCode::Char('=') => {
                // Zoom in (increase zoom level, max 5.0)
                let current_zoom = s.wallet_graph.viewport.2;
                s.wallet_graph.viewport.2 = (current_zoom * 1.2).min(5.0);
            }
            KeyCode::Char('-') | KeyCode::Char('_') => {
                // Zoom out (decrease zoom level, min 0.2)
                let current_zoom = s.wallet_graph.viewport.2;
                s.wallet_graph.viewport.2 = (current_zoom / 1.2).max(0.2);
            }
            // Reset view
            KeyCode::Char('0') => {
                // Reset viewport to center (0,0) with default zoom 1.0
                s.wallet_graph.viewport = (0.0, 0.0, 1.0);
            }
            // Help toggle
            KeyCode::Char('?') => {
                s.show_help = !s.show_help;
            }
            // Edge navigation - follow the money!
            KeyCode::Enter | KeyCode::Tab => {
                // Follow outgoing edge from current node
                s.wallet_graph.handle_input(GraphInput::Right);
            }
            KeyCode::Backspace => {
                // Go back to source node
                s.wallet_graph.handle_input(GraphInput::Left);
            }
            KeyCode::Char(' ') => {
                // Select current selection (confirm edge traversal)
                s.wallet_graph.handle_input(GraphInput::Select);
            }
            KeyCode::Esc => {
                // Close help overlay if open, otherwise escape from edge selection
                if s.show_help {
                    s.show_help = false;
                } else {
                    s.wallet_graph.handle_input(GraphInput::Escape);
                }
            }
            _ => {}
        }
    });

    // ═══════════════════════════════════════════════════════════════════════════
    // Touch Event Handlers for Mobile Gesture Support
    // ═══════════════════════════════════════════════════════════════════════════

    // Set up touch event listeners on document
    if let Some(window) = web_sys::window() {
        if let Some(document) = window.document() {
            if let Some(body) = document.body() {
                let body_elem: web_sys::EventTarget = body.into();

                // Touch Start - capture initial touch points
                let state_for_touch_start = Rc::clone(&state);
                let touch_start_closure = Closure::wrap(Box::new(move |event: web_sys::TouchEvent| {
                    event.prevent_default();
                    let mut s = state_for_touch_start.borrow_mut();
                    let touches = event.touches();

                    // Clear and rebuild active touches
                    s.touch_state.active_touches.clear();
                    for i in 0..touches.length() {
                        if let Some(touch) = touches.get(i) {
                            s.touch_state.active_touches.push((
                                touch.identifier(),
                                touch.client_x() as f64,
                                touch.client_y() as f64,
                            ));
                        }
                    }

                    // Initialize pinch state if 2 fingers
                    if s.touch_state.active_touches.len() >= 2 {
                        s.touch_state.is_pinching = true;
                        s.touch_state.initial_pinch_distance = s.touch_state.pinch_distance();
                        s.touch_state.initial_zoom = s.wallet_graph.viewport.2;
                    } else {
                        s.touch_state.is_pinching = false;
                        s.touch_state.last_position = s.touch_state.center();
                    }
                }) as Box<dyn FnMut(_)>);

                let _ = body_elem.add_event_listener_with_callback(
                    "touchstart",
                    touch_start_closure.as_ref().unchecked_ref(),
                );
                touch_start_closure.forget(); // Leak the closure (it lives forever)

                // Touch Move - handle pinch zoom and pan
                let state_for_touch_move = Rc::clone(&state);
                let touch_move_closure = Closure::wrap(Box::new(move |event: web_sys::TouchEvent| {
                    event.prevent_default();
                    let mut s = state_for_touch_move.borrow_mut();
                    let touches = event.touches();

                    // Update touch positions
                    s.touch_state.active_touches.clear();
                    for i in 0..touches.length() {
                        if let Some(touch) = touches.get(i) {
                            s.touch_state.active_touches.push((
                                touch.identifier(),
                                touch.client_x() as f64,
                                touch.client_y() as f64,
                            ));
                        }
                    }

                    // Handle pinch zoom (2 fingers)
                    if s.touch_state.is_pinching && s.touch_state.active_touches.len() >= 2 {
                        if let (Some(initial_dist), Some(current_dist)) = (
                            s.touch_state.initial_pinch_distance,
                            s.touch_state.pinch_distance(),
                        ) {
                            // Calculate zoom scale relative to initial distance
                            let scale = current_dist / initial_dist;
                            let new_zoom = (s.touch_state.initial_zoom * scale)
                                .clamp(0.2, 5.0);
                            s.wallet_graph.viewport.2 = new_zoom;
                        }
                    }
                    // Handle single-finger pan
                    else if s.touch_state.active_touches.len() == 1 {
                        if let (Some(last_pos), Some(current_pos)) = (
                            s.touch_state.last_position,
                            s.touch_state.center(),
                        ) {
                            // Calculate delta and apply to viewport (all f64)
                            let dx = current_pos.0 - last_pos.0;
                            let dy = current_pos.1 - last_pos.1;
                            // Invert for natural scrolling feel
                            s.wallet_graph.viewport.0 -= dx;
                            s.wallet_graph.viewport.1 += dy;
                        }
                        s.touch_state.last_position = s.touch_state.center();
                    }
                }) as Box<dyn FnMut(_)>);

                let _ = body_elem.add_event_listener_with_callback(
                    "touchmove",
                    touch_move_closure.as_ref().unchecked_ref(),
                );
                touch_move_closure.forget();

                // Touch End - reset state
                let state_for_touch_end = Rc::clone(&state);
                let touch_end_closure = Closure::wrap(Box::new(move |event: web_sys::TouchEvent| {
                    event.prevent_default();
                    let mut s = state_for_touch_end.borrow_mut();
                    let touches = event.touches();

                    // Update remaining touches
                    s.touch_state.active_touches.clear();
                    for i in 0..touches.length() {
                        if let Some(touch) = touches.get(i) {
                            s.touch_state.active_touches.push((
                                touch.identifier(),
                                touch.client_x() as f64,
                                touch.client_y() as f64,
                            ));
                        }
                    }

                    // Reset pinch state when going below 2 fingers
                    if s.touch_state.active_touches.len() < 2 {
                        s.touch_state.is_pinching = false;
                        s.touch_state.initial_pinch_distance = None;
                        // Update last position for potential single-finger pan
                        s.touch_state.last_position = s.touch_state.center();
                    }
                }) as Box<dyn FnMut(_)>);

                let _ = body_elem.add_event_listener_with_callback(
                    "touchend",
                    touch_end_closure.as_ref().unchecked_ref(),
                );
                touch_end_closure.forget();

                log::info!("Touch event handlers registered for mobile gestures");
            }
        }
    }

    // Render loop - now with full WalletGraph visualization!
    let state_for_render = Rc::clone(&state);
    terminal.draw_web(move |f| {
        let mut s = state_for_render.borrow_mut();
        s.frame = s.frame.wrapping_add(1);
        // Tick toast notifications in the graph
        s.wallet_graph.tick_toast();

        // Periodically save view state to URL hash (every 30 frames ≈ 0.5s)
        s.hash_update_counter = s.hash_update_counter.wrapping_add(1);
        if s.hash_update_counter % 30 == 0 {
            save_view_to_hash(s.wallet_graph.viewport, Some(s.selected_wallet));
        }

        ui(f, &mut s);
    });

    log::info!("OSVM WebGL2 TUI initialized - press [r] to fetch live data");

    Ok(())
}
