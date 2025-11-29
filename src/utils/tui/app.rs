use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyModifiers},
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
use std::sync::mpsc::{self, Receiver, Sender};
use std::time::Duration;

use super::graph::{WalletGraph, GraphInput};

/// Response from AI chat processing - supports streaming agent updates
#[derive(Clone, Debug)]
pub struct ChatResponse {
    pub content: String,
    pub response_type: ChatResponseType,
}

/// Types of chat responses for streaming agent updates
#[derive(Clone, Debug, PartialEq)]
pub enum ChatResponseType {
    /// Final answer - replace "Thinking..." message
    FinalAnswer,
    /// Intermediate thinking step - append to current message
    ThinkingStep,
    /// Tool being called - append to current message
    ToolCall(String),  // tool name
    /// Tool result received - append to current message
    ToolResult(String), // tool name
    /// Error occurred
    Error,
    /// Graph update - contains transfer data to visualize
    GraphUpdate(Vec<TransferData>),
}

/// Transfer data for graph updates from agent
#[derive(Clone, Debug, PartialEq)]
pub struct TransferData {
    pub from: String,
    pub to: String,
    pub amount: f64,
    pub token: String,
    pub timestamp: Option<String>,
}

/// Conversation turn for memory - stores Q&A pairs
#[derive(Clone, Debug)]
pub struct ConversationTurn {
    pub query: String,
    pub response: String,
    pub tools_used: Vec<String>,
    pub timestamp: String,
}

// ═══════════════════════════════════════════════════════════════════════════
// Autonomous Investigation Types
// ═══════════════════════════════════════════════════════════════════════════

/// State of an autonomous investigation
#[derive(Clone, Debug, Default)]
pub struct InvestigationProgress {
    pub is_running: bool,
    pub cancel_requested: bool,  // Flag to gracefully stop investigation
    pub target_wallet: String,
    pub hypothesis: Option<String>,  // User's hypothesis to test
    pub phase: InvestigationPhaseType,
    pub wallets_explored: usize,
    pub wallets_pending: usize,
    pub max_wallets: usize,
    pub current_depth: usize,
    pub max_depth: usize,
    pub transfers_found: usize,
    pub findings: Vec<InvestigationFinding>,
    pub evidence_for: Vec<String>,    // Evidence supporting hypothesis
    pub evidence_against: Vec<String>, // Evidence refuting hypothesis
    pub start_time: Option<std::time::Instant>,
    pub status_message: String,
}

/// Hypothesis keywords that trigger hypothesis-driven investigation
const HYPOTHESIS_TRIGGERS: &[&str] = &[
    "i think",
    "i believe",
    "i suspect",
    "check if",
    "check whether",
    "investigate if",
    "investigate whether",
    "is this",
    "could this be",
    "might be",
    "possibly",
    "hypothesis:",
];

/// Phases of autonomous investigation
#[derive(Clone, Debug, Default, PartialEq)]
pub enum InvestigationPhaseType {
    #[default]
    Idle,
    Initializing,
    ExploringDepth(usize),  // Current depth level
    AnalyzingPatterns,
    GeneratingReport,
    Complete,
    Failed(String),
}

/// A finding from the investigation
#[derive(Clone, Debug)]
pub struct InvestigationFinding {
    pub category: FindingCategory,
    pub severity: FindingSeverity,
    pub title: String,
    pub description: String,
    pub wallets_involved: Vec<String>,
    pub evidence: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FindingCategory {
    FundingSource,
    LargeTransfer,
    CircularFlow,
    ClusterDetected,
    SuspiciousPattern,
    HighActivity,
    DormantWallet,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FindingSeverity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

/// Keywords that trigger autonomous investigation
const INVESTIGATION_TRIGGERS: &[&str] = &[
    "investigate",
    "full investigation",
    "investigate fully",
    "deep dive",
    "analyze fully",
    "full analysis",
    "forensic",
    "trace everything",
    "find everything",
    "complete analysis",
];

#[derive(Clone, Copy, PartialEq)]
pub enum TabIndex {
    Chat = 0,           // NEW: Chat is now the default tab
    Dashboard = 1,
    Graph = 2,
    Logs = 3,
    SearchResults = 4,
    BBS = 5,            // Meshtastic BBS for agent-human communication
    Federation = 6,     // Federation network dashboard
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

/// Chat message structure for AI conversation
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ChatMessage {
    pub role: String,      // "user" or "assistant"
    pub content: String,
    pub timestamp: String,
    pub status: MessageStatus,
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum MessageStatus {
    Sending,     // User message being sent
    Delivered,   // Successfully delivered
    Streaming,   // Assistant response streaming in
    Complete,    // Assistant response finished
    Error,       // Failed to send/receive
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
    pub ai_insights_scroll: usize,  // Scroll position for AI Insights panel
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
    // RPC endpoint for live blockchain queries
    pub rpc_url: Option<String>,
    // Chat interface
    pub chat_messages: Arc<Mutex<Vec<ChatMessage>>>,
    pub chat_input: String,
    pub chat_input_active: bool,
    pub chat_scroll: usize,
    pub chat_auto_scroll: bool,  // Auto-scroll to bottom on new messages
    // Chat AI integration channels
    pub chat_response_rx: Option<Receiver<ChatResponse>>,
    pub chat_response_tx: Option<Sender<ChatResponse>>,
    pub runtime_handle: Option<tokio::runtime::Handle>,
    // Conversation memory for context-aware follow-ups
    pub conversation_history: Arc<Mutex<Vec<ConversationTurn>>>,
    // Autonomous investigation state
    pub investigation_progress: Arc<Mutex<InvestigationProgress>>,
    // BBS interface
    pub bbs_state: Arc<Mutex<crate::utils::bbs::tui_widgets::BBSTuiState>>,
    // Federation dashboard state
    pub federation_state: Arc<Mutex<FederationDashboardState>>,
    pub federation_scroll: usize,
    pub federation_selected_peer: Option<usize>,
    pub federation_input_active: bool,
    pub federation_input_buffer: String,
    pub federation_refresh_pending: bool,
    pub federation_add_pending: Option<String>,
    pub federation_delete_pending: Option<String>,
    pub federation_selected_session: Option<usize>,
    pub federation_show_annotations: bool,
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

/// Federation network state for TUI dashboard
#[derive(Clone, Debug, Default)]
pub struct FederationDashboardState {
    pub node_id: String,
    pub peers: Vec<FederationPeerInfo>,
    pub sessions: Vec<FederationSessionInfo>,
    pub last_refresh: Option<std::time::Instant>,
    pub total_annotations: usize,
    pub connection_graph: Vec<(String, String)>, // (from_node, to_node) edges
    pub live_annotations: Vec<LiveAnnotation>,   // Real-time annotation stream
}

/// Live annotation from federated session
#[derive(Clone, Debug)]
pub struct LiveAnnotation {
    pub session_id: String,
    pub author: String,
    pub author_node: String,
    pub target: String,       // wallet or transaction
    pub text: String,
    pub severity: String,
    pub timestamp: String,
}

/// Peer information for federation dashboard
#[derive(Clone, Debug)]
pub struct FederationPeerInfo {
    pub node_id: String,
    pub address: String,
    pub status: PeerStatus,
    pub latency_ms: Option<u64>,
    pub sessions_hosted: usize,
    pub last_seen: Option<String>,
}

/// Session information for federation dashboard
#[derive(Clone, Debug)]
pub struct FederationSessionInfo {
    pub session_id: String,
    pub name: String,
    pub host_node_id: String,
    pub participant_count: usize,
    pub status: String,
}

/// Peer connection status
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum PeerStatus {
    Online,
    Offline,
    Connecting,
    Unknown,
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

// ═══════════════════════════════════════════════════════════════════════════
// Chat Agent Loop - ReAct-style agent for blockchain investigation
// With: Conversation Memory, Graph Updates, Multi-Turn Reflection
// ═══════════════════════════════════════════════════════════════════════════

/// Maximum number of reflection iterations to prevent infinite loops
const MAX_REFLECTION_ITERATIONS: usize = 3;

/// Run the full chat agent loop with conversation memory and graph updates
async fn run_chat_agent_loop(
    tx: Sender<ChatResponse>,
    query: String,
    target_wallet: String,
    conversation_history: Arc<Mutex<Vec<ConversationTurn>>>,
    wallet_graph: Arc<Mutex<WalletGraph>>,
) {
    // Step 1: Send initial thinking message
    let _ = tx.send(ChatResponse {
        content: "🔍 Analyzing your question...".to_string(),
        response_type: ChatResponseType::ThinkingStep,
    });

    // Create AI service
    let ai_service = crate::services::ai_service::AiService::new();

    // Build conversation context from history
    let history_context = build_conversation_context(&conversation_history);

    // Step 2: Ask AI to plan what tools to use (with conversation context)
    let planning_prompt = format!(
        r#"You are a blockchain investigator assistant. The user is researching wallet: {}.

{}User question: {}

You have access to these blockchain analysis tools:
- get_account_transfers: Get transfer history (params: address, limit) - USE THIS FOR WALLET RELATIONSHIPS
- get_account_portfolio: Get current token holdings (params: address)
- get_account_stats: Get account activity stats (params: address)
- get_account_transactions: Get detailed transactions (params: address, limit)

Decide what tool(s) to use to answer this question. Respond in this exact JSON format:
{{
    "thinking": "Brief explanation of your approach",
    "tools": [
        {{"tool": "tool_name", "params": {{"address": "wallet_address", "limit": 100}}}}
    ],
    "direct_answer": null
}}

If the question can be answered from conversation history or general knowledge:
{{
    "thinking": "Explanation",
    "tools": [],
    "direct_answer": "Your answer here"
}}

Respond ONLY with valid JSON, no other text."#,
        target_wallet,
        history_context,
        query
    );

    let plan = match parse_ai_plan(&ai_service, &planning_prompt, &tx).await {
        Some(p) => p,
        None => return, // Error already sent
    };

    // Step 3: Check if we have a direct answer (from history or knowledge)
    if let Some(direct) = plan.get("direct_answer").and_then(|d| d.as_str()) {
        if !direct.is_empty() && direct != "null" {
            let _ = tx.send(ChatResponse {
                content: direct.to_string(),
                response_type: ChatResponseType::FinalAnswer,
            });
            store_conversation(&conversation_history, &query, direct, &[]);
            return;
        }
    }

    // Step 4: Send thinking step
    if let Some(thinking) = plan.get("thinking").and_then(|t| t.as_str()) {
        let _ = tx.send(ChatResponse {
            content: format!("💭 {}", thinking),
            response_type: ChatResponseType::ThinkingStep,
        });
    }

    // Get initial tools
    let mut pending_tools: Vec<serde_json::Value> = plan.get("tools")
        .and_then(|t| t.as_array())
        .cloned()
        .unwrap_or_default();

    if pending_tools.is_empty() {
        let _ = tx.send(ChatResponse {
            content: "I don't have specific blockchain data access right now. Let me provide general guidance based on your question.".to_string(),
            response_type: ChatResponseType::FinalAnswer,
        });
        return;
    }

    // Initialize MCP service
    let mut mcp_service = crate::services::mcp_service::McpService::new_with_debug(false);
    let _ = mcp_service.load_config();

    let servers = mcp_service.list_servers();
    let server_id = if servers.is_empty() {
        let _ = tx.send(ChatResponse {
            content: format!(
                "⚠️ No MCP data servers configured. To get real blockchain data, run:\n\n\
                `osvm mcp add helius https://helius.dev/api`\n\n\
                The wallet {} would need an MCP-connected data provider.",
                target_wallet
            ),
            response_type: ChatResponseType::FinalAnswer,
        });
        return;
    } else {
        servers[0].0.clone()
    };

    if let Err(e) = mcp_service.initialize_server(&server_id).await {
        let _ = tx.send(ChatResponse {
            content: format!("⚠️ Failed to connect to data server: {}", e),
            response_type: ChatResponseType::Error,
        });
        return;
    }

    // Step 5: Multi-turn execution with reflection
    let mut all_tool_results: Vec<String> = Vec::new();
    let mut tools_used: Vec<String> = Vec::new();
    let mut iteration = 0;

    while !pending_tools.is_empty() && iteration < MAX_REFLECTION_ITERATIONS {
        iteration += 1;

        // Execute current batch of tools
        for tool_spec in &pending_tools {
            let tool_name = tool_spec.get("tool").and_then(|t| t.as_str()).unwrap_or("unknown");
            let params = tool_spec.get("params").cloned();

            let _ = tx.send(ChatResponse {
                content: format!("🔧 Calling {}...", tool_name),
                response_type: ChatResponseType::ToolCall(tool_name.to_string()),
            });

            match mcp_service.call_tool(&server_id, tool_name, params).await {
                Ok(result) => {
                    let result_str = serde_json::to_string_pretty(&result).unwrap_or_default();

                    // Update graph if this is transfer data
                    if tool_name.contains("transfer") {
                        update_graph_from_transfers(&wallet_graph, &result, &target_wallet);
                        let _ = tx.send(ChatResponse {
                            content: "📊 Graph updated with transfer data".to_string(),
                            response_type: ChatResponseType::ThinkingStep,
                        });
                    }

                    let _ = tx.send(ChatResponse {
                        content: format!("✅ {} returned {} bytes", tool_name, result_str.len()),
                        response_type: ChatResponseType::ToolResult(tool_name.to_string()),
                    });

                    all_tool_results.push(format!("Tool: {}\nResult:\n{}", tool_name, result_str));
                    tools_used.push(tool_name.to_string());
                }
                Err(e) => {
                    let _ = tx.send(ChatResponse {
                        content: format!("⚠️ {} failed: {}", tool_name, e),
                        response_type: ChatResponseType::ToolResult(tool_name.to_string()),
                    });
                    all_tool_results.push(format!("Tool: {}\nError: {}", tool_name, e));
                }
            }
        }

        // Reflection: Ask AI if more tools are needed
        if iteration < MAX_REFLECTION_ITERATIONS {
            let _ = tx.send(ChatResponse {
                content: "🤔 Reflecting on results...".to_string(),
                response_type: ChatResponseType::ThinkingStep,
            });

            let reflection_prompt = format!(
                r#"You are investigating wallet {} for the user's question: "{}"

Tool results so far:
{}

Do you have enough information to answer the question? If not, what additional tools would help?

Respond in JSON:
{{
    "have_enough_info": true/false,
    "reasoning": "explanation",
    "additional_tools": [
        {{"tool": "tool_name", "params": {{"address": "...", "limit": 100}}}}
    ]
}}

Only request additional tools if TRULY necessary. Respond ONLY with JSON."#,
                target_wallet,
                query,
                all_tool_results.join("\n\n---\n\n")
            );

            if let Some(reflection) = parse_ai_plan(&ai_service, &reflection_prompt, &tx).await {
                let have_enough = reflection.get("have_enough_info")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(true);

                if have_enough {
                    pending_tools.clear();
                } else {
                    pending_tools = reflection.get("additional_tools")
                        .and_then(|t| t.as_array())
                        .cloned()
                        .unwrap_or_default();

                    if !pending_tools.is_empty() {
                        if let Some(reasoning) = reflection.get("reasoning").and_then(|r| r.as_str()) {
                            let _ = tx.send(ChatResponse {
                                content: format!("💭 {}", reasoning),
                                response_type: ChatResponseType::ThinkingStep,
                            });
                        }
                    }
                }
            } else {
                pending_tools.clear();
            }
        } else {
            pending_tools.clear();
        }
    }

    // Step 6: Synthesize final answer with conversation context
    let _ = tx.send(ChatResponse {
        content: "📝 Synthesizing answer...".to_string(),
        response_type: ChatResponseType::ThinkingStep,
    });

    let synthesis_prompt = format!(
        r#"You are a blockchain analyst. The user asked: "{}"
{}
About wallet: {}

Tool results:
{}

Provide a clear, helpful answer. Include:
1. Direct answer to their question
2. Key findings (with specific numbers/addresses when available)
3. Any notable patterns or concerns

Be concise but thorough. Use bullet points for clarity. ALWAYS show full wallet addresses, never truncate."#,
        query,
        history_context,
        target_wallet,
        all_tool_results.join("\n\n---\n\n")
    );

    match ai_service.query_osvm_ai_with_options(&synthesis_prompt, None, Some(true), false).await {
        Ok(answer) => {
            let _ = tx.send(ChatResponse {
                content: answer.clone(),
                response_type: ChatResponseType::FinalAnswer,
            });
            store_conversation(&conversation_history, &query, &answer, &tools_used);
        }
        Err(e) => {
            let error_msg = format!(
                "⚠️ Analysis failed: {}\n\nRaw tool results:\n{}",
                e,
                all_tool_results.join("\n\n")
            );
            let _ = tx.send(ChatResponse {
                content: error_msg.clone(),
                response_type: ChatResponseType::Error,
            });
            store_conversation(&conversation_history, &query, &error_msg, &tools_used);
        }
    }
}

/// Build conversation context string from history
fn build_conversation_context(history: &Arc<Mutex<Vec<ConversationTurn>>>) -> String {
    if let Ok(turns) = history.lock() {
        if turns.is_empty() {
            return String::new();
        }
        let context: Vec<String> = turns.iter()
            .rev()
            .take(5) // Last 5 turns
            .rev()
            .map(|turn| format!(
                "Previous Q: {}\nPrevious A: {}\n",
                turn.query,
                // Truncate long responses
                if turn.response.len() > 500 {
                    format!("{}...", &turn.response[..500])
                } else {
                    turn.response.clone()
                }
            ))
            .collect();
        if context.is_empty() {
            String::new()
        } else {
            format!("Conversation history:\n{}\n", context.join("\n"))
        }
    } else {
        String::new()
    }
}

/// Store completed conversation turn
fn store_conversation(
    history: &Arc<Mutex<Vec<ConversationTurn>>>,
    query: &str,
    response: &str,
    tools: &[String],
) {
    if let Ok(mut turns) = history.lock() {
        turns.push(ConversationTurn {
            query: query.to_string(),
            response: response.to_string(),
            tools_used: tools.to_vec(),
            timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
        });
        // Keep only last 20 turns to prevent memory bloat
        if turns.len() > 20 {
            turns.remove(0);
        }
    }
}

/// Parse AI response as JSON plan
async fn parse_ai_plan(
    ai_service: &crate::services::ai_service::AiService,
    prompt: &str,
    tx: &Sender<ChatResponse>,
) -> Option<serde_json::Value> {
    match ai_service.query_osvm_ai_with_options(prompt, None, Some(true), false).await {
        Ok(response) => {
            // Try to parse as JSON
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&response) {
                return Some(json);
            }
            // Try to extract JSON from response
            if let Some(start) = response.find('{') {
                if let Some(end) = response.rfind('}') {
                    if let Ok(json) = serde_json::from_str::<serde_json::Value>(&response[start..=end]) {
                        return Some(json);
                    }
                }
            }
            // Give up - return response as final answer
            let _ = tx.send(ChatResponse {
                content: response,
                response_type: ChatResponseType::FinalAnswer,
            });
            None
        }
        Err(e) => {
            let _ = tx.send(ChatResponse {
                content: format!("⚠️ AI error: {}", e),
                response_type: ChatResponseType::Error,
            });
            None
        }
    }
}

/// Update wallet graph from transfer data
fn update_graph_from_transfers(
    wallet_graph: &Arc<Mutex<WalletGraph>>,
    result: &serde_json::Value,
    target_wallet: &str,
) {
    use super::graph::WalletNodeType;

    // Try to extract transfers from various response formats
    let transfers = result.get("transfers")
        .or_else(|| result.get("data"))
        .or_else(|| result.get("items"))
        .and_then(|v| v.as_array());

    if let Some(transfer_list) = transfers {
        if let Ok(mut graph) = wallet_graph.lock() {
            for transfer in transfer_list.iter().take(50) { // Limit to prevent UI overload
                let from = transfer.get("from")
                    .or_else(|| transfer.get("source"))
                    .or_else(|| transfer.get("sender"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("");

                let to = transfer.get("to")
                    .or_else(|| transfer.get("destination"))
                    .or_else(|| transfer.get("receiver"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("");

                let amount = transfer.get("amount")
                    .or_else(|| transfer.get("value"))
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0);

                let token = transfer.get("token")
                    .or_else(|| transfer.get("symbol"))
                    .or_else(|| transfer.get("mint"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("SOL");

                let timestamp = transfer.get("timestamp")
                    .or_else(|| transfer.get("blockTime"))
                    .and_then(|v| v.as_str().map(|s| s.to_string()));

                if !from.is_empty() && !to.is_empty() {
                    // Determine node types based on relation to target
                    let from_type = if from == target_wallet {
                        WalletNodeType::Target
                    } else {
                        WalletNodeType::Funding
                    };

                    let to_type = if to == target_wallet {
                        WalletNodeType::Target
                    } else {
                        WalletNodeType::Recipient
                    };

                    graph.add_transfer(
                        from.to_string(),
                        to.to_string(),
                        amount,
                        token.to_string(),
                        from_type,
                        to_type,
                        timestamp,
                        None, // No signature in this context
                    );
                }
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Autonomous Investigation Engine
// ═══════════════════════════════════════════════════════════════════════════

/// Configuration for autonomous investigation
const MAX_INVESTIGATION_DEPTH: usize = 3;
const MAX_INVESTIGATION_WALLETS: usize = 50;
const TRANSFERS_PER_WALLET: usize = 100;

/// Check if a query triggers autonomous investigation
fn is_investigation_trigger(query: &str) -> bool {
    let query_lower = query.to_lowercase();
    INVESTIGATION_TRIGGERS.iter().any(|trigger| query_lower.contains(trigger))
}

/// Extract hypothesis from user query if present
fn extract_hypothesis(query: &str) -> Option<String> {
    let query_lower = query.to_lowercase();
    for trigger in HYPOTHESIS_TRIGGERS {
        if let Some(pos) = query_lower.find(trigger) {
            // Extract everything after the trigger phrase
            let start = pos + trigger.len();
            let hypothesis = query[start..].trim();
            if !hypothesis.is_empty() {
                return Some(hypothesis.to_string());
            }
        }
    }
    None
}

/// Run autonomous investigation with BFS exploration
/// Supports hypothesis-driven investigation when hypothesis is provided
async fn run_autonomous_investigation(
    tx: Sender<ChatResponse>,
    target_wallet: String,
    hypothesis: Option<String>,
    investigation_progress: Arc<Mutex<InvestigationProgress>>,
    wallet_graph: Arc<Mutex<WalletGraph>>,
    conversation_history: Arc<Mutex<Vec<ConversationTurn>>>,
) {
    use std::collections::{HashSet, VecDeque};

    let is_hypothesis_mode = hypothesis.is_some();

    // Initialize investigation state (reset all counters)
    {
        let mut progress = investigation_progress.lock().unwrap();
        progress.is_running = true;
        progress.cancel_requested = false;  // Reset cancel flag
        progress.target_wallet = target_wallet.clone();
        progress.hypothesis = hypothesis.clone();
        progress.phase = InvestigationPhaseType::Initializing;
        progress.wallets_explored = 0;
        progress.wallets_pending = 0;
        progress.evidence_for.clear();
        progress.evidence_against.clear();
        progress.max_depth = MAX_INVESTIGATION_DEPTH;
        progress.max_wallets = MAX_INVESTIGATION_WALLETS;
        progress.current_depth = 0;
        progress.transfers_found = 0;
        progress.findings.clear();
        progress.start_time = Some(std::time::Instant::now());
        progress.status_message = "Starting autonomous investigation...".to_string();
    }

    let hypothesis_msg = if let Some(ref h) = hypothesis {
        format!("\n🎯 **Hypothesis Mode:** Testing \"{}\"", h)
    } else {
        String::new()
    };

    let _ = tx.send(ChatResponse {
        content: format!(
            "🔬 **AUTONOMOUS INVESTIGATION INITIATED**\n\n\
            Target: `{}`\n\
            Max Depth: {} hops\n\
            Max Wallets: {}{}\n\n\
            Starting BFS exploration...\n\n\
            💡 Press **Ctrl+X** to cancel investigation",
            target_wallet, MAX_INVESTIGATION_DEPTH, MAX_INVESTIGATION_WALLETS, hypothesis_msg
        ),
        response_type: ChatResponseType::ThinkingStep,
    });

    // Initialize MCP service
    let mut mcp_service = crate::services::mcp_service::McpService::new_with_debug(false);
    let _ = mcp_service.load_config();

    let servers = mcp_service.list_servers();
    let server_id = if servers.is_empty() {
        let _ = tx.send(ChatResponse {
            content: "⚠️ No MCP servers configured. Cannot perform autonomous investigation without data access.".to_string(),
            response_type: ChatResponseType::Error,
        });
        update_investigation_failed(&investigation_progress, "No MCP servers available");
        return;
    } else {
        servers[0].0.clone()
    };

    if let Err(e) = mcp_service.initialize_server(&server_id).await {
        let _ = tx.send(ChatResponse {
            content: format!("⚠️ Failed to initialize MCP server: {}", e),
            response_type: ChatResponseType::Error,
        });
        update_investigation_failed(&investigation_progress, &format!("MCP init failed: {}", e));
        return;
    }

    // BFS exploration state
    let mut visited: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<(String, usize)> = VecDeque::new(); // (wallet, depth)
    let mut all_transfers: Vec<serde_json::Value> = Vec::new();
    let mut wallet_summaries: Vec<String> = Vec::new();

    // Start with target wallet
    queue.push_back((target_wallet.clone(), 0));
    visited.insert(target_wallet.clone());

    // BFS exploration loop
    while let Some((current_wallet, depth)) = queue.pop_front() {
        // Check for cancellation request
        {
            let progress = investigation_progress.lock().unwrap();
            if progress.cancel_requested {
                drop(progress);
                let _ = tx.send(ChatResponse {
                    content: "🛑 **Investigation cancelled by user.** Proceeding to analyze collected data...".to_string(),
                    response_type: ChatResponseType::ThinkingStep,
                });
                break;
            }
        }

        // Check limits
        if visited.len() > MAX_INVESTIGATION_WALLETS {
            let _ = tx.send(ChatResponse {
                content: format!("⚡ Reached wallet limit ({}). Proceeding to analysis.", MAX_INVESTIGATION_WALLETS),
                response_type: ChatResponseType::ThinkingStep,
            });
            break;
        }

        if depth > MAX_INVESTIGATION_DEPTH {
            continue;
        }

        // Update progress
        {
            let mut progress = investigation_progress.lock().unwrap();
            progress.phase = InvestigationPhaseType::ExploringDepth(depth);
            progress.wallets_explored = visited.len();
            progress.wallets_pending = queue.len();
            progress.current_depth = depth;
            progress.status_message = format!("Exploring {} (depth {})", &current_wallet[..8.min(current_wallet.len())], depth);
        }

        let _ = tx.send(ChatResponse {
            content: format!(
                "🔍 Depth {}: Exploring `{}...{}` ({}/{} wallets)",
                depth,
                &current_wallet[..6.min(current_wallet.len())],
                &current_wallet[current_wallet.len().saturating_sub(4)..],
                visited.len(),
                MAX_INVESTIGATION_WALLETS
            ),
            response_type: ChatResponseType::ThinkingStep,
        });

        // Fetch transfers for this wallet
        let result = mcp_service.call_tool(
            &server_id,
            "get_account_transfers",
            Some(serde_json::json!({
                "address": current_wallet,
                "limit": TRANSFERS_PER_WALLET,
                "compress": true
            }))
        ).await;

        match result {
            Ok(data) => {
                // Update graph with transfers
                update_graph_from_transfers(&wallet_graph, &data, &target_wallet);

                // Extract connected wallets
                let transfers = data.get("transfers")
                    .or_else(|| data.get("data"))
                    .or_else(|| data.get("items"))
                    .and_then(|v| v.as_array());

                if let Some(transfer_list) = transfers {
                    let transfer_count = transfer_list.len();

                    // Update progress
                    {
                        let mut progress = investigation_progress.lock().unwrap();
                        progress.transfers_found += transfer_count;
                    }

                    // Store transfers for analysis
                    all_transfers.extend(transfer_list.clone());

                    // Add connected wallets to queue
                    for transfer in transfer_list {
                        let from = transfer.get("from")
                            .or_else(|| transfer.get("source"))
                            .and_then(|v| v.as_str());
                        let to = transfer.get("to")
                            .or_else(|| transfer.get("destination"))
                            .and_then(|v| v.as_str());

                        for addr in [from, to].into_iter().flatten() {
                            if !visited.contains(addr) && visited.len() < MAX_INVESTIGATION_WALLETS {
                                visited.insert(addr.to_string());
                                queue.push_back((addr.to_string(), depth + 1));
                            }
                        }
                    }

                    wallet_summaries.push(format!(
                        "Wallet {} (depth {}): {} transfers found",
                        &current_wallet[..8.min(current_wallet.len())],
                        depth,
                        transfer_count
                    ));
                }
            }
            Err(e) => {
                wallet_summaries.push(format!(
                    "Wallet {} (depth {}): Error - {}",
                    &current_wallet[..8.min(current_wallet.len())],
                    depth,
                    e
                ));
            }
        }

        // Small delay to avoid rate limiting
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    }

    // Phase 2: Pattern Analysis
    let _ = tx.send(ChatResponse {
        content: format!(
            "📊 **EXPLORATION COMPLETE**\n\
            • Wallets explored: {}\n\
            • Transfers found: {}\n\
            • Max depth reached: {}\n\n\
            Analyzing patterns...",
            visited.len(),
            all_transfers.len(),
            MAX_INVESTIGATION_DEPTH
        ),
        response_type: ChatResponseType::ThinkingStep,
    });

    {
        let mut progress = investigation_progress.lock().unwrap();
        progress.phase = InvestigationPhaseType::AnalyzingPatterns;
        progress.status_message = "Analyzing patterns and generating findings...".to_string();
    }

    // Analyze patterns and generate findings
    let findings = analyze_investigation_patterns(&all_transfers, &target_wallet, &visited);

    // Store findings
    {
        let mut progress = investigation_progress.lock().unwrap();
        progress.findings = findings.clone();
    }

    // Phase 3: AI Synthesis
    let _ = tx.send(ChatResponse {
        content: "🤖 Generating AI analysis and report...".to_string(),
        response_type: ChatResponseType::ThinkingStep,
    });

    {
        let mut progress = investigation_progress.lock().unwrap();
        progress.phase = InvestigationPhaseType::GeneratingReport;
    }

    // Create AI service for synthesis
    let ai_service = crate::services::ai_service::AiService::new();

    // Build hypothesis section if provided
    let hypothesis_section = if let Some(ref h) = hypothesis {
        format!(r#"

HYPOTHESIS TO TEST: "{}"

You MUST include a dedicated "Hypothesis Verdict" section with:
- **SUPPORTED** / **REFUTED** / **INCONCLUSIVE** verdict
- Evidence supporting the hypothesis
- Evidence against the hypothesis
- Confidence level (High/Medium/Low)
"#, h)
    } else {
        String::new()
    };

    let analysis_prompt = format!(
        r#"You are a blockchain forensics expert. Analyze this investigation data and provide a comprehensive report.

TARGET WALLET: {}{}

EXPLORATION SUMMARY:
- Wallets explored: {}
- Total transfers found: {}
- Max depth: {}

AUTOMATED FINDINGS:
{}

WALLET EXPLORATION LOG:
{}

Generate a forensic report with:
1. **Executive Summary** (2-3 sentences){}
2. **Key Findings** (bullet points)
3. **Risk Assessment** (Low/Medium/High/Critical with explanation)
4. **Wallet Relationships** (key connections identified)
5. **Recommendations** (what to investigate further)

Be specific. Use actual wallet addresses (never truncate). Include amounts where relevant."#,
        target_wallet,
        hypothesis_section,
        visited.len(),
        all_transfers.len(),
        MAX_INVESTIGATION_DEPTH,
        findings.iter().map(|f| format!("• [{}] {}: {}", severity_str(&f.severity), f.title, f.description)).collect::<Vec<_>>().join("\n"),
        wallet_summaries.join("\n"),
        if hypothesis.is_some() { "\n6. **Hypothesis Verdict** (SUPPORTED/REFUTED/INCONCLUSIVE with evidence)" } else { "" }
    );

    let report = match ai_service.query_osvm_ai_with_options(&analysis_prompt, None, Some(true), false).await {
        Ok(response) => response,
        Err(e) => format!("AI analysis failed: {}. Raw findings:\n{}", e,
            findings.iter().map(|f| format!("• {}: {}", f.title, f.description)).collect::<Vec<_>>().join("\n"))
    };

    // Generate final report
    let elapsed = investigation_progress.lock().unwrap()
        .start_time
        .map(|t| t.elapsed().as_secs())
        .unwrap_or(0);

    let final_report = format!(
        "# 🔬 AUTONOMOUS INVESTIGATION REPORT\n\n\
        **Target:** `{}`\n\
        **Duration:** {}s\n\
        **Wallets Analyzed:** {}\n\
        **Transfers Processed:** {}\n\n\
        ---\n\n\
        {}\n\n\
        ---\n\n\
        *Report generated by OSVM autonomous investigation engine*",
        target_wallet,
        elapsed,
        visited.len(),
        all_transfers.len(),
        report
    );

    // Export report to file
    let export_result = export_investigation_report(&target_wallet, &final_report);
    let export_msg = match &export_result {
        Ok(path) => format!("\n\n📁 **Report exported to:** `{}`", path),
        Err(e) => format!("\n\n⚠️ Failed to export report: {}", e),
    };

    let final_report_with_export = format!("{}{}", final_report, export_msg);

    // Mark complete
    {
        let mut progress = investigation_progress.lock().unwrap();
        progress.phase = InvestigationPhaseType::Complete;
        progress.is_running = false;
        progress.status_message = export_result
            .as_ref()
            .map(|p| format!("Complete - saved to {}", p))
            .unwrap_or_else(|_| "Investigation complete".to_string());
    }

    // Store in conversation history
    store_conversation(
        &conversation_history,
        &format!("Autonomous investigation of {}", target_wallet),
        &final_report_with_export,
        &["get_account_transfers".to_string()],
    );

    // Send final report
    let _ = tx.send(ChatResponse {
        content: final_report_with_export,
        response_type: ChatResponseType::FinalAnswer,
    });
}

/// Export investigation report to file
fn export_investigation_report(target_wallet: &str, report: &str) -> Result<String, String> {
    use std::fs;

    // Create reports directory
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let reports_dir = format!("{}/.osvm/reports", home);

    if let Err(e) = fs::create_dir_all(&reports_dir) {
        return Err(format!("Failed to create reports directory: {}", e));
    }

    // Generate filename with timestamp and wallet prefix
    let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let wallet_prefix = &target_wallet[..8.min(target_wallet.len())];
    let filename = format!("investigation_{}_{}.md", wallet_prefix, timestamp);
    let filepath = format!("{}/{}", reports_dir, filename);

    // Write markdown report
    if let Err(e) = fs::write(&filepath, report) {
        return Err(format!("Failed to write report: {}", e));
    }

    // Also generate HTML version
    let html_report = generate_html_report(target_wallet, report);
    let html_filepath = filepath.replace(".md", ".html");
    if let Err(e) = fs::write(&html_filepath, html_report) {
        // Non-fatal - markdown was saved
        eprintln!("Warning: Failed to write HTML report: {}", e);
    }

    Ok(filepath)
}

/// Generate HTML version of investigation report
fn generate_html_report(target_wallet: &str, markdown_report: &str) -> String {
    // Simple markdown-to-HTML conversion for key elements
    let html_content = markdown_report
        .replace("# ", "<h1>")
        .replace("\n## ", "</h1>\n<h2>")
        .replace("\n### ", "</h2>\n<h3>")
        .replace("\n**", "\n<strong>")
        .replace("**\n", "</strong>\n")
        .replace("**", "</strong><strong>")  // Handle inline bold
        .replace("`", "<code>")
        .replace("• ", "<li>")
        .replace("\n- ", "\n<li>")
        .replace("---", "<hr>");

    format!(r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OSVM Investigation Report - {}</title>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, monospace;
            max-width: 900px;
            margin: 0 auto;
            padding: 20px;
            background: #0d1117;
            color: #c9d1d9;
            line-height: 1.6;
        }}
        h1 {{ color: #58a6ff; border-bottom: 1px solid #30363d; padding-bottom: 10px; }}
        h2 {{ color: #7ee787; margin-top: 30px; }}
        h3 {{ color: #ffa657; }}
        code {{
            background: #161b22;
            padding: 2px 6px;
            border-radius: 4px;
            font-family: 'Fira Code', monospace;
            color: #79c0ff;
        }}
        strong {{ color: #f0883e; }}
        hr {{ border: none; border-top: 1px solid #30363d; margin: 20px 0; }}
        li {{ margin: 5px 0; }}
        .header {{
            background: linear-gradient(135deg, #238636 0%, #1f6feb 100%);
            padding: 20px;
            border-radius: 8px;
            margin-bottom: 20px;
        }}
        .header h1 {{ color: white; border: none; margin: 0; }}
        .finding {{
            background: #161b22;
            border-left: 3px solid #ffa657;
            padding: 10px 15px;
            margin: 10px 0;
            border-radius: 0 4px 4px 0;
        }}
        .critical {{ border-left-color: #f85149; }}
        .high {{ border-left-color: #ffa657; }}
        .medium {{ border-left-color: #d29922; }}
        .low {{ border-left-color: #3fb950; }}
        .footer {{
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #30363d;
            text-align: center;
            color: #8b949e;
            font-size: 0.9em;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>🔬 OSVM Investigation Report</h1>
    </div>
    {}
    <div class="footer">
        Generated by OSVM Autonomous Investigation Engine<br>
        Report Date: {}
    </div>
</body>
</html>"#,
        target_wallet,
        html_content,
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
    )
}

/// Analyze patterns in collected transfer data
fn analyze_investigation_patterns(
    transfers: &[serde_json::Value],
    target_wallet: &str,
    visited_wallets: &std::collections::HashSet<String>,
) -> Vec<InvestigationFinding> {
    use std::collections::{HashMap, HashSet};

    let mut findings = Vec::new();

    // Track wallet activity
    let mut wallet_inflows: HashMap<String, f64> = HashMap::new();
    let mut wallet_outflows: HashMap<String, f64> = HashMap::new();
    let mut wallet_counterparties: HashMap<String, HashSet<String>> = HashMap::new();

    for transfer in transfers {
        let from = transfer.get("from")
            .or_else(|| transfer.get("source"))
            .and_then(|v| v.as_str())
            .unwrap_or("");
        let to = transfer.get("to")
            .or_else(|| transfer.get("destination"))
            .and_then(|v| v.as_str())
            .unwrap_or("");
        let amount = transfer.get("amount")
            .or_else(|| transfer.get("value"))
            .and_then(|v| v.as_f64())
            .unwrap_or(0.0);

        if !from.is_empty() && !to.is_empty() {
            *wallet_outflows.entry(from.to_string()).or_insert(0.0) += amount;
            *wallet_inflows.entry(to.to_string()).or_insert(0.0) += amount;

            wallet_counterparties.entry(from.to_string())
                .or_insert_with(HashSet::new)
                .insert(to.to_string());
            wallet_counterparties.entry(to.to_string())
                .or_insert_with(HashSet::new)
                .insert(from.to_string());
        }
    }

    // Finding 1: Major funding sources
    let target_inflow = wallet_inflows.get(target_wallet).copied().unwrap_or(0.0);
    if target_inflow > 0.0 {
        findings.push(InvestigationFinding {
            category: FindingCategory::FundingSource,
            severity: FindingSeverity::Info,
            title: "Funding Sources Identified".to_string(),
            description: format!("Target wallet received total inflow of {:.4} across {} transfers", target_inflow, transfers.len()),
            wallets_involved: vec![target_wallet.to_string()],
            evidence: format!("Total inflow: {:.4}", target_inflow),
        });
    }

    // Finding 2: Large transfers (>100 SOL equivalent)
    for transfer in transfers {
        let amount = transfer.get("amount")
            .or_else(|| transfer.get("value"))
            .and_then(|v| v.as_f64())
            .unwrap_or(0.0);

        if amount > 100.0 {
            let from = transfer.get("from").and_then(|v| v.as_str()).unwrap_or("unknown");
            let to = transfer.get("to").and_then(|v| v.as_str()).unwrap_or("unknown");

            findings.push(InvestigationFinding {
                category: FindingCategory::LargeTransfer,
                severity: FindingSeverity::Medium,
                title: "Large Transfer Detected".to_string(),
                description: format!("{:.2} transferred from {} to {}", amount, &from[..8.min(from.len())], &to[..8.min(to.len())]),
                wallets_involved: vec![from.to_string(), to.to_string()],
                evidence: format!("Amount: {:.4}", amount),
            });
        }
    }

    // Finding 3: High activity wallets (>10 counterparties)
    for (wallet, counterparties) in &wallet_counterparties {
        if counterparties.len() > 10 && wallet != target_wallet {
            findings.push(InvestigationFinding {
                category: FindingCategory::HighActivity,
                severity: FindingSeverity::Low,
                title: "High Activity Wallet".to_string(),
                description: format!("Wallet {} has {} counterparties", &wallet[..8.min(wallet.len())], counterparties.len()),
                wallets_involved: vec![wallet.clone()],
                evidence: format!("Counterparty count: {}", counterparties.len()),
            });
        }
    }

    // Finding 4: Potential clusters (wallets that share many counterparties)
    let mut cluster_candidates: Vec<(String, String, usize)> = Vec::new();
    let wallets: Vec<_> = wallet_counterparties.keys().collect();
    for i in 0..wallets.len() {
        for j in (i+1)..wallets.len() {
            let w1 = wallets[i];
            let w2 = wallets[j];
            if let (Some(c1), Some(c2)) = (wallet_counterparties.get(w1), wallet_counterparties.get(w2)) {
                let shared = c1.intersection(c2).count();
                if shared > 3 {
                    cluster_candidates.push((w1.clone(), w2.clone(), shared));
                }
            }
        }
    }

    if !cluster_candidates.is_empty() {
        cluster_candidates.sort_by(|a, b| b.2.cmp(&a.2));
        let top = &cluster_candidates[0];
        findings.push(InvestigationFinding {
            category: FindingCategory::ClusterDetected,
            severity: FindingSeverity::High,
            title: "Potential Wallet Cluster".to_string(),
            description: format!("Wallets {} and {} share {} common counterparties - possible same owner",
                &top.0[..8.min(top.0.len())], &top.1[..8.min(top.1.len())], top.2),
            wallets_involved: vec![top.0.clone(), top.1.clone()],
            evidence: format!("Shared counterparties: {}", top.2),
        });
    }

    // Summary finding
    findings.push(InvestigationFinding {
        category: FindingCategory::FundingSource,
        severity: FindingSeverity::Info,
        title: "Investigation Summary".to_string(),
        description: format!("Explored {} wallets, found {} transfers, identified {} findings",
            visited_wallets.len(), transfers.len(), findings.len()),
        wallets_involved: vec![target_wallet.to_string()],
        evidence: "Automated BFS exploration complete".to_string(),
    });

    findings
}

/// Helper to get severity string
fn severity_str(severity: &FindingSeverity) -> &'static str {
    match severity {
        FindingSeverity::Info => "INFO",
        FindingSeverity::Low => "LOW",
        FindingSeverity::Medium => "MEDIUM",
        FindingSeverity::High => "HIGH",
        FindingSeverity::Critical => "CRITICAL",
    }
}

/// Update investigation state on failure
fn update_investigation_failed(progress: &Arc<Mutex<InvestigationProgress>>, reason: &str) {
    if let Ok(mut p) = progress.lock() {
        p.is_running = false;
        p.phase = InvestigationPhaseType::Failed(reason.to_string());
        p.status_message = format!("Failed: {}", reason);
    }
}

impl OsvmApp {
    pub fn new(target_wallet: String) -> Self {
        Self {
            active_tab: TabIndex::Chat,  // Chat is the default tab
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
            ai_insights_scroll: 0,
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
            rpc_url: None,
            // Chat interface initialization
            chat_messages: Arc::new(Mutex::new(Self::get_welcome_message())),
            chat_input: String::new(),
            chat_input_active: false,
            chat_scroll: 0,
            chat_auto_scroll: true,
            // Chat AI integration - channels created lazily when runtime is set
            chat_response_rx: None,
            chat_response_tx: None,
            runtime_handle: None,
            // Conversation memory - stores Q&A pairs for context
            conversation_history: Arc::new(Mutex::new(Vec::new())),
            // Autonomous investigation - starts idle
            investigation_progress: Arc::new(Mutex::new(InvestigationProgress::default())),
            // BBS interface initialization
            bbs_state: Arc::new(Mutex::new(crate::utils::bbs::tui_widgets::BBSTuiState::new())),
            // Federation dashboard initialization
            federation_state: Arc::new(Mutex::new(FederationDashboardState::default())),
            federation_scroll: 0,
            federation_selected_peer: None,
            federation_input_active: false,
            federation_input_buffer: String::new(),
            federation_refresh_pending: true,  // Load on first tick
            federation_add_pending: None,
            federation_delete_pending: None,
            federation_selected_session: None,
            federation_show_annotations: false,
        }
    }

    /// Set the tokio runtime handle and initialize chat channels
    /// This must be called before chat will work
    pub fn set_runtime_handle(&mut self, handle: tokio::runtime::Handle) {
        let (tx, rx) = mpsc::channel::<ChatResponse>();
        self.runtime_handle = Some(handle);
        self.chat_response_tx = Some(tx);
        self.chat_response_rx = Some(rx);
    }

    /// Get a clone of the chat response sender for background tasks
    pub fn get_chat_tx(&self) -> Option<Sender<ChatResponse>> {
        self.chat_response_tx.clone()
    }

    /// Create welcome message for chat
    fn get_welcome_message() -> Vec<ChatMessage> {
        vec![
            ChatMessage {
                role: "assistant".to_string(),
                content: "👋 Welcome to OSVM Research Chat!\n\n\
                    I can help you investigate blockchain activity, analyze wallets, trace transactions, \
                    and explore on-chain patterns.\n\n\
                    Try asking:\n\
                    • \"Analyze the activity of wallet X\"\n\
                    • \"Show me recent DEX trades for token Y\"\n\
                    • \"Find wallets connected to this address\"\n\
                    • \"What are the top token holders?\"\n\n\
                    Press 'i' to start typing your question.".to_string(),
                timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
                status: MessageStatus::Complete,
            }
        ]
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

    /// Get federation state handle for background thread updates
    pub fn get_federation_handle(&self) -> Arc<Mutex<FederationDashboardState>> {
        Arc::clone(&self.federation_state)
    }

    /// Load federation state from persisted file (synchronous, no health checks)
    pub fn load_federation_state_sync(&mut self) {
        use crate::utils::collab::FederationState;

        let state = FederationState::load();
        let mut fed_state = FederationDashboardState {
            node_id: state.node_id.clone(),
            peers: state.peers.iter().map(|(node_id, address)| {
                FederationPeerInfo {
                    node_id: node_id.clone(),
                    address: address.clone(),
                    status: PeerStatus::Unknown,
                    latency_ms: None,
                    sessions_hosted: 0,
                    last_seen: None,
                }
            }).collect(),
            sessions: state.known_sessions.iter().map(|s| {
                FederationSessionInfo {
                    session_id: s.session_id.clone(),
                    name: s.name.clone(),
                    host_node_id: s.host_node_id.clone(),
                    participant_count: s.participant_count,
                    status: format!("{:?}", s.status),
                }
            }).collect(),
            last_refresh: Some(std::time::Instant::now()),
            total_annotations: 0,
            connection_graph: Vec::new(),
            live_annotations: Vec::new(),
        };

        // Generate node_id if empty
        if fed_state.node_id.is_empty() {
            fed_state.node_id = format!("!{:08x}", std::process::id());
        }

        if let Ok(mut lock) = self.federation_state.lock() {
            *lock = fed_state;
        }
    }

    /// Handle adding a federation peer (synchronous)
    fn handle_federation_add_peer(&mut self, address: &str) {
        use crate::utils::collab::FederationState;

        // Load current state
        let mut state = FederationState::load();

        // Generate node ID from address
        let node_id = format!("!{:08x}", crc32fast::hash(address.as_bytes()));

        // Add peer
        state.peers.insert(node_id.clone(), address.to_string());

        // Save state
        if let Err(e) = state.save() {
            self.add_log(format!("❌ Failed to save peer: {}", e));
            return;
        }

        self.add_log(format!("✓ Added peer {} ({})", node_id, address));

        // Reload state to update UI
        self.load_federation_state_sync();
    }

    /// Handle deleting a federation peer (synchronous)
    fn handle_federation_delete_peer(&mut self, node_id: &str) {
        use crate::utils::collab::FederationState;

        // Load current state
        let mut state = FederationState::load();

        // Remove peer
        if state.peers.remove(node_id).is_none() {
            self.add_log(format!("⚠️ Peer {} not found", node_id));
            return;
        }

        // Save state
        if let Err(e) = state.save() {
            self.add_log(format!("❌ Failed to save: {}", e));
            return;
        }

        self.add_log(format!("✓ Removed peer {}", node_id));

        // Clear selection if deleted peer was selected
        self.federation_selected_peer = None;

        // Reload state to update UI
        self.load_federation_state_sync();
    }

    /// Start background thread to poll RPC for real-time network stats AND live transactions
    pub fn start_network_stats_polling(&mut self, rpc_url: String) {
        // Store RPC URL for later use in search
        self.rpc_url = Some(rpc_url.clone());

        let stats_handle = Arc::clone(&self.network_stats);
        let tx_handle = Arc::clone(&self.live_transactions);

        std::thread::spawn(move || {
            use solana_client::rpc_client::RpcClient;
            use solana_commitment_config::CommitmentConfig;

            let client = RpcClient::new_with_commitment(rpc_url, CommitmentConfig::confirmed());

            loop {
                // Fetch network stats
                let mut updated_stats = NetworkStats::default();

                if let Ok(slot) = client.get_slot() {
                    updated_stats.current_slot = slot;
                }

                if let Ok(epoch_info) = client.get_epoch_info() {
                    updated_stats.current_epoch = epoch_info.epoch;
                }

                if let Ok(perf_samples) = client.get_recent_performance_samples(Some(1)) {
                    if let Some(sample) = perf_samples.first() {
                        updated_stats.tps = sample.num_transactions as f64 / sample.sample_period_secs as f64;
                        updated_stats.block_time_ms = ((sample.sample_period_secs as u64 * 1000) / sample.num_slots.max(1) as u64);
                        updated_stats.total_transactions = sample.num_transactions;
                    }
                }

                if let Ok(vote_accounts) = client.get_vote_accounts() {
                    updated_stats.active_validators = vote_accounts.current.len();
                }

                updated_stats.health = if client.get_health().is_ok() {
                    "ok".to_string()
                } else {
                    "error".to_string()
                };

                // Update shared state
                *stats_handle.lock().unwrap() = updated_stats;

                // Fetch recent transactions from latest block
                if let Ok(current_slot) = client.get_slot() {
                    if let Ok(block) = client.get_block(current_slot) {
                        let mut live_txs = Vec::new();

                        for tx_with_meta in block.transactions.iter().take(10) {
                            if let Some(transaction) = &tx_with_meta.transaction.decode() {
                                if let Some(meta) = &tx_with_meta.meta {
                                    let sig = transaction.signatures.first()
                                        .map(|s| s.to_string())
                                        .unwrap_or_else(|| "unknown".to_string());

                                    let timestamp = chrono::Local::now().format("%H:%M:%S").to_string();
                                    let success = meta.status.is_ok();
                                    let amount_sol = meta.post_balances.first()
                                        .zip(meta.pre_balances.first())
                                        .map(|(post, pre)| (*post as i64 - *pre as i64).abs() as f64 / 1_000_000_000.0)
                                        .unwrap_or(0.0);

                                    let tx_type = if transaction.message.instructions().is_empty() {
                                        "Unknown"
                                    } else if amount_sol > 0.0 {
                                        "Transfer"
                                    } else {
                                        "Contract"
                                    }.to_string();

                                    live_txs.push(LiveTransaction {
                                        signature: sig[..8].to_string(), // Truncate for display
                                        timestamp,
                                        amount_sol,
                                        success,
                                        tx_type,
                                    });
                                }
                            }
                        }

                        *tx_handle.lock().unwrap() = live_txs;
                    }
                }

                // Sleep 5 seconds before next poll
                std::thread::sleep(std::time::Duration::from_secs(5));
            }
        });
    }

    /// Start background thread for federation peer health checks
    pub fn start_federation_health_polling(&mut self) {
        let federation_handle = Arc::clone(&self.federation_state);

        std::thread::spawn(move || {
            let client = reqwest::blocking::Client::builder()
                .timeout(std::time::Duration::from_secs(5))
                .build()
                .unwrap_or_else(|_| reqwest::blocking::Client::new());

            loop {
                // Read current peers
                let peers_to_check: Vec<(String, String)> = {
                    let state = federation_handle.lock().unwrap();
                    state.peers.iter()
                        .map(|p| (p.node_id.clone(), p.address.clone()))
                        .collect()
                };

                // Check each peer's health
                let mut updated_peers: Vec<FederationPeerInfo> = Vec::new();

                for (node_id, address) in peers_to_check {
                    let start = std::time::Instant::now();
                    let health_url = format!("{}/api/health", address);

                    let (status, latency_ms) = match client.get(&health_url).send() {
                        Ok(response) => {
                            let latency = start.elapsed().as_millis() as u64;
                            if response.status().is_success() {
                                (PeerStatus::Online, Some(latency))
                            } else {
                                // Try root URL
                                match client.get(&address).send() {
                                    Ok(_) => (PeerStatus::Online, Some(start.elapsed().as_millis() as u64)),
                                    Err(_) => (PeerStatus::Offline, None),
                                }
                            }
                        }
                        Err(_) => {
                            // Try root URL as fallback
                            match client.get(&address).send() {
                                Ok(_) => (PeerStatus::Online, Some(start.elapsed().as_millis() as u64)),
                                Err(_) => (PeerStatus::Offline, None),
                            }
                        }
                    };

                    updated_peers.push(FederationPeerInfo {
                        node_id,
                        address,
                        status,
                        latency_ms,
                        sessions_hosted: 0,
                        last_seen: if status == PeerStatus::Online {
                            Some(chrono::Local::now().format("%H:%M:%S").to_string())
                        } else {
                            None
                        },
                    });
                }

                // Update shared state
                if let Ok(mut state) = federation_handle.lock() {
                    // Preserve node_id and other fields
                    state.peers = updated_peers;
                    state.last_refresh = Some(std::time::Instant::now());
                }

                // Sleep 30 seconds before next health check
                std::thread::sleep(std::time::Duration::from_secs(30));
            }
        });
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
    /// AND fetch live data from blockchain for transaction signatures
    fn search_indexed_data(&self, query: &str, entity_type: &EntityType) -> SearchResultsData {
        let mut results = SearchResultsData {
            query: query.to_string(),
            entity_type: format!("{:?}", entity_type),
            search_timestamp: chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string(),
            ..Default::default()
        };

        let query_lower = query.to_lowercase();

        // 🔥 LIVE BLOCKCHAIN FETCH: Check if query looks like a transaction signature
        // Pattern: base58 encoded string, 87-88 characters (Solana transaction signature format)
        // This works REGARDLESS of entity_type - we detect based on string pattern
        let looks_like_signature = query.len() >= 87 && query.len() <= 88 &&
                                   query.chars().all(|c| c.is_alphanumeric());

        if looks_like_signature {
            // Log attempt
            eprintln!("🔍 Detected potential transaction signature: {} chars", query.len());

            if let Some(ref rpc_url) = self.rpc_url {
                // Extract domain from URL for cleaner logging
                let domain = rpc_url
                    .strip_prefix("https://")
                    .or_else(|| rpc_url.strip_prefix("http://"))
                    .and_then(|s| s.split('/').next())
                    .unwrap_or(rpc_url);
                eprintln!("🌐 Fetching from RPC: {}", domain);

                match self.fetch_transaction_from_blockchain(query, rpc_url) {
                    Some(tx_match) => {
                        eprintln!("✅ Successfully fetched transaction from blockchain");
                        results.transactions_found.push(tx_match);
                    }
                    None => {
                        eprintln!("❌ Failed to fetch transaction (may not exist or RPC error)");
                    }
                }
            } else {
                eprintln!("⚠️  No RPC URL configured - cannot fetch transaction");
            }
        }

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

    /// Fetch transaction details from the blockchain via RPC
    /// Returns TransactionMatch if found, None if not found or error
    fn fetch_transaction_from_blockchain(&self, signature: &str, rpc_url: &str) -> Option<TransactionMatch> {
        use solana_client::rpc_client::RpcClient;
        use solana_commitment_config::CommitmentConfig;
        use solana_sdk::signature::Signature;
        use std::str::FromStr;

        // Parse signature
        let sig = match Signature::from_str(signature) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("❌ Failed to parse signature: {}", e);
                return None;
            }
        };

        // Create RPC client
        let client = RpcClient::new_with_commitment(rpc_url.to_string(), CommitmentConfig::confirmed());

        // Fetch transaction details with full encoding
        let tx = match client.get_transaction(&sig, solana_transaction_status::UiTransactionEncoding::JsonParsed) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("❌ RPC error fetching transaction: {}", e);
                return None;
            }
        };

        // Extract transaction metadata
        let meta = tx.transaction.meta?;
        let transaction = tx.transaction.transaction.decode()?;

        // Get timestamp (block time)
        let timestamp = tx.block_time
            .map(|bt| chrono::DateTime::from_timestamp(bt, 0)
                .map(|dt| dt.format("%Y-%m-%d %H:%M:%S").to_string())
                .unwrap_or_else(|| "Unknown".to_string()))
            .unwrap_or_else(|| "Unknown".to_string());

        // Get fee in SOL
        let fee_lamports = meta.fee;
        let fee_sol = fee_lamports as f64 / 1_000_000_000.0;

        // Get signer (first account key)
        let from = transaction.message.static_account_keys()
            .get(0)
            .map(|k| k.to_string())
            .unwrap_or_else(|| "Unknown".to_string());

        // Try to detect recipient (simplified - would need full parsing for accuracy)
        let to = transaction.message.static_account_keys()
            .get(1)
            .map(|k| k.to_string())
            .unwrap_or_else(|| "Unknown".to_string());

        // Determine transaction status
        let success = meta.status.is_ok();
        let status_str = if success { "Success" } else { "Failed" };

        // Calculate SOL amount transferred (simplified - sum of pre/post balance changes)
        let mut amount_sol = 0.0;
        if let (Some(pre_balances), Some(post_balances)) = (meta.pre_balances.first(), meta.post_balances.first()) {
            let change = (*post_balances as i64 - *pre_balances as i64).abs() as f64 / 1_000_000_000.0;
            amount_sol = change;
        }

        Some(TransactionMatch {
            signature: signature.to_string(),
            timestamp,
            amount_sol,
            from,
            to,
            match_reason: format!("Live blockchain fetch - {} - Fee: {:.6} SOL", status_str, fee_sol),
        })
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

    /// Send a chat message to the AI
    fn send_chat_message(&mut self) {
        let user_message = self.chat_input.trim().to_string();
        if user_message.is_empty() {
            return;
        }

        // Add user message to chat history
        if let Ok(mut messages) = self.chat_messages.lock() {
            messages.push(ChatMessage {
                role: "user".to_string(),
                content: user_message.clone(),
                timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
                status: MessageStatus::Delivered,
            });

            // Add placeholder for assistant response
            messages.push(ChatMessage {
                role: "assistant".to_string(),
                content: "Thinking...".to_string(),
                timestamp: chrono::Local::now().format("%H:%M:%S").to_string(),
                status: MessageStatus::Streaming,
            });
        }

        // Clear input
        self.chat_input.clear();

        // Auto-scroll to bottom
        self.chat_auto_scroll = true;
        self.chat_scroll = 0;

        // Log the query
        self.add_log(format!("🤖 Processing query: {}", user_message));

        // Check if this is an investigation trigger
        let is_investigation = is_investigation_trigger(&user_message);

        // Spawn background task
        if let (Some(handle), Some(tx)) = (&self.runtime_handle, &self.chat_response_tx) {
            let tx = tx.clone();
            let query = user_message.clone();
            let target_wallet = self.target_wallet.clone();
            let conversation_history = Arc::clone(&self.conversation_history);
            let wallet_graph = Arc::clone(&self.wallet_graph);
            let investigation_progress = Arc::clone(&self.investigation_progress);

            if is_investigation {
                // Check if investigation is already running
                let already_running = investigation_progress.lock()
                    .map(|p| p.is_running)
                    .unwrap_or(false);

                if already_running {
                    // Already running - update chat message
                    if let Ok(mut messages) = self.chat_messages.lock() {
                        if let Some(last) = messages.last_mut() {
                            if last.role == "assistant" {
                                last.content = "⚠️ An investigation is already in progress. Please wait for it to complete.".to_string();
                                last.status = MessageStatus::Complete;
                            }
                        }
                    }
                    return;
                }

                // Extract hypothesis if user provided one
                let hypothesis = extract_hypothesis(&query);

                // Note: Investigation is being launched (logged above with query)
                handle.spawn(async move {
                    // Run autonomous investigation (with optional hypothesis)
                    run_autonomous_investigation(
                        tx,
                        target_wallet,
                        hypothesis,
                        investigation_progress,
                        wallet_graph,
                        conversation_history,
                    ).await;
                });
            } else {
                handle.spawn(async move {
                    // Run the regular agent loop with conversation history and graph updates
                    run_chat_agent_loop(tx, query, target_wallet, conversation_history, wallet_graph).await;
                });
            }
        } else {
            // No runtime available - show error immediately
            if let Ok(mut messages) = self.chat_messages.lock() {
                if let Some(last) = messages.last_mut() {
                    if last.role == "assistant" && last.status == MessageStatus::Streaming {
                        last.content = "⚠️ Chat AI not initialized. Please restart the TUI with a valid context.".to_string();
                        last.status = MessageStatus::Error;
                    }
                }
            }
        }
    }

    /// Check for incoming chat responses and update UI
    /// Handles streaming updates from the agent loop
    fn poll_chat_responses(&mut self) {
        if let Some(rx) = &self.chat_response_rx {
            // Non-blocking check for responses
            while let Ok(response) = rx.try_recv() {
                if let Ok(mut messages) = self.chat_messages.lock() {
                    // Find the last assistant message that's still streaming
                    if let Some(last) = messages.last_mut() {
                        if last.role == "assistant" && last.status == MessageStatus::Streaming {
                            match response.response_type {
                                ChatResponseType::ThinkingStep => {
                                    // Append thinking step to existing content
                                    if last.content == "Thinking..." {
                                        last.content = response.content;
                                    } else {
                                        last.content = format!("{}\n{}", last.content, response.content);
                                    }
                                }
                                ChatResponseType::ToolCall(_) | ChatResponseType::ToolResult(_) => {
                                    // Append tool status to existing content
                                    last.content = format!("{}\n{}", last.content, response.content);
                                }
                                ChatResponseType::FinalAnswer => {
                                    // Replace with final answer
                                    last.content = response.content;
                                    last.status = MessageStatus::Complete;
                                }
                                ChatResponseType::Error => {
                                    // Show error
                                    last.content = response.content;
                                    last.status = MessageStatus::Error;
                                }
                                ChatResponseType::GraphUpdate(_) => {
                                    // Graph updates are handled directly in the agent loop
                                    // Just append a note to the chat
                                    last.content = format!("{}\n📊 Graph visualization updated", last.content);
                                }
                            }
                            last.timestamp = chrono::Local::now().format("%H:%M:%S").to_string();
                        }
                    }
                }
                // Enable auto-scroll when new response arrives
                self.chat_auto_scroll = true;
            }
        }
    }

    /// Update activity sparkline - called on each tick
    pub fn tick(&mut self) {
        self.iteration += 1;

        // Poll for chat AI responses (non-blocking)
        self.poll_chat_responses();

        // Handle federation refresh (synchronous load from file)
        if self.federation_refresh_pending {
            self.load_federation_state_sync();
            self.federation_refresh_pending = false;
        }

        // Handle federation add peer operation
        if let Some(address) = self.federation_add_pending.take() {
            self.handle_federation_add_peer(&address);
        }

        // Handle federation delete peer operation
        if let Some(node_id) = self.federation_delete_pending.take() {
            self.handle_federation_delete_peer(&node_id);
        }

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

    /// Run TUI with web streaming enabled - streams to browser AND local terminal
    /// Also accepts keyboard input from web browsers
    pub fn run_with_web<F>(
        &mut self,
        web_sender: crate::utils::web_terminal::WebTerminalSender,
        mut web_input: crate::utils::web_terminal::WebInputReceiver,
        on_tick: F,
    ) -> Result<()>
    where
        F: Fn(&mut Self) + Send + 'static,
    {
        use crate::utils::web_tui_backend::DualBackend;

        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;

        // Get terminal size for web backend
        let (width, height) = crossterm::terminal::size().unwrap_or((120, 40));

        // Create dual backend that writes to both terminal and web
        let backend = DualBackend::new(stdout, web_sender, width, height);
        let mut terminal = Terminal::new(backend)?;

        // Run event loop with web input support
        let result = self.event_loop_with_web_input(&mut terminal, &mut web_input, on_tick);

        disable_raw_mode()?;
        execute!(
            terminal.backend_mut(),
            LeaveAlternateScreen
        )?;
        terminal.show_cursor()?;

        result
    }

    /// Event loop that also accepts input from web browsers
    fn event_loop_with_web_input<B, F>(
        &mut self,
        terminal: &mut Terminal<B>,
        web_input: &mut crate::utils::web_terminal::WebInputReceiver,
        on_tick: F,
    ) -> Result<()>
    where
        B: ratatui::backend::Backend + std::io::Write,
        F: Fn(&mut Self),
    {
        loop {
            terminal.draw(|f| self.ui(f))?;

            // Check for web input first (non-blocking)
            while let Some(key) = web_input.try_recv() {
                self.handle_key_event(key);
                if self.should_quit {
                    return Ok(());
                }
            }

            // Then check for local terminal input
            if event::poll(Duration::from_millis(50))? {
                if let Event::Key(key) = event::read()? {
                    self.handle_key_event(key);
                }
            } else {
                on_tick(self);
            }

            if self.should_quit {
                return Ok(());
            }
        }
    }

    /// Handle a key event (from either local terminal or web)
    /// Mirrors the main event_loop key handling for consistent behavior
    fn handle_key_event(&mut self, key: KeyEvent) {
        // Check if filter modal is active first
        let filter_modal_active = if self.active_tab == TabIndex::Graph {
            self.wallet_graph.lock()
                .map(|g| g.filter_modal.active)
                .unwrap_or(false)
        } else {
            false
        };

        if filter_modal_active {
            self.handle_filter_modal_key(key);
            return;
        }

        match key.code {
            // Quit/Escape - handle different contexts
            KeyCode::Char('q') | KeyCode::Esc => {
                if self.global_search_active {
                    self.global_search_active = false;
                } else if self.show_help {
                    self.show_help = false;
                    self.help_scroll = 0;
                } else if self.active_tab == TabIndex::Graph {
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
                } else if self.chat_input_active {
                    self.chat_input.clear();
                    self.chat_input_active = false;
                } else {
                    self.should_quit = true;
                }
            }
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                self.should_quit = true;
            }
            // Tab navigation
            KeyCode::Tab => {
                self.next_tab();
            }
            KeyCode::BackTab => {
                // Reverse tab - inline prev_tab logic
                self.active_tab = match self.active_tab {
                    TabIndex::Chat => TabIndex::Federation,
                    TabIndex::Dashboard => TabIndex::Chat,
                    TabIndex::Graph => TabIndex::Dashboard,
                    TabIndex::Logs => TabIndex::Graph,
                    TabIndex::SearchResults => TabIndex::Logs,
                    TabIndex::BBS => TabIndex::SearchResults,
                    TabIndex::Federation => TabIndex::BBS,
                };
            }
            // Help toggle
            KeyCode::Char('?') => {
                self.show_help = !self.show_help;
            }
            // Navigation - Up/k
            KeyCode::Up | KeyCode::Char('k') if !self.chat_input_active => {
                if self.show_help {
                    self.help_scroll = self.help_scroll.saturating_sub(1);
                } else {
                    match self.active_tab {
                        TabIndex::Chat => { self.chat_scroll = self.chat_scroll.saturating_sub(1); }
                        TabIndex::Dashboard => { self.ai_insights_scroll = self.ai_insights_scroll.saturating_sub(1); }
                        TabIndex::Logs => { self.log_scroll = self.log_scroll.saturating_sub(1); }
                        TabIndex::Graph => {
                            if let Ok(mut graph) = self.wallet_graph.lock() {
                                graph.handle_input(GraphInput::Up);
                            }
                        }
                        _ => {}
                    }
                }
            }
            // Navigation - Down/j
            KeyCode::Down | KeyCode::Char('j') if !self.chat_input_active => {
                if self.show_help {
                    self.help_scroll = self.help_scroll.saturating_add(1);
                } else {
                    match self.active_tab {
                        TabIndex::Chat => { self.chat_scroll = self.chat_scroll.saturating_add(1); }
                        TabIndex::Dashboard => { self.ai_insights_scroll = self.ai_insights_scroll.saturating_add(1); }
                        TabIndex::Logs => { self.log_scroll = self.log_scroll.saturating_add(1); }
                        TabIndex::Graph => {
                            if let Ok(mut graph) = self.wallet_graph.lock() {
                                graph.handle_input(GraphInput::Down);
                            }
                        }
                        _ => {}
                    }
                }
            }
            // Navigation - Left/h (graph node/edge selection)
            KeyCode::Left | KeyCode::Char('h') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::Left);
                }
            }
            // Navigation - Right/l (graph node/edge selection)
            KeyCode::Right | KeyCode::Char('l') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::Right);
                }
            }
            // Enter - Hop to selected wallet
            KeyCode::Enter if self.active_tab == TabIndex::Graph && !self.chat_input_active => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::HopToWallet);
                }
            }
            // Space - Toggle node collapse/expand
            KeyCode::Char(' ') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::Toggle);
                }
            }
            // Graph zoom
            KeyCode::Char('+') | KeyCode::Char('=') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::ZoomIn);
                }
            }
            KeyCode::Char('-') | KeyCode::Char('_') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::ZoomOut);
                }
            }
            // Graph panning (w/a/s/d for pan)
            KeyCode::Char('w') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::PanUp);
                }
            }
            KeyCode::Char('s') if self.active_tab == TabIndex::Graph && !self.chat_input_active => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::PanDown);
                }
            }
            KeyCode::Char('a') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::PanLeft);
                }
            }
            KeyCode::Char('d') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::PanRight);
                }
            }
            // Graph depth control
            KeyCode::Char('[') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::DecreaseDepth);
                }
            }
            KeyCode::Char(']') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::IncreaseDepth);
                }
            }
            // Graph search
            KeyCode::Char('/') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::StartSearch);
                }
            }
            KeyCode::Char('n') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::SearchNext);
                }
            }
            KeyCode::Char('N') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::SearchPrev);
                }
            }
            // Graph copy to clipboard
            KeyCode::Char('y') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::Copy);
                }
            }
            // Graph trail toggle
            KeyCode::Char('t') | KeyCode::Char('T') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.show_trail = !graph.show_trail;
                }
            }
            // Graph reset view
            KeyCode::Char('r') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.viewport = (0.0, 0.0, 1.0);
                }
            }
            // Detail panel scroll (< and >)
            KeyCode::Char('<') | KeyCode::Char(',') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::ScrollDetailUp);
                }
            }
            KeyCode::Char('>') | KeyCode::Char('.') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::ScrollDetailDown);
                }
            }
            // Filter modal (backtick key)
            KeyCode::Char('`') if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::OpenFilterModal);
                }
            }
            // Number keys 1-5 to select tabs
            KeyCode::Char('1') if !self.chat_input_active => {
                self.active_tab = TabIndex::Chat;
            }
            KeyCode::Char('2') if !self.chat_input_active => {
                self.active_tab = TabIndex::Dashboard;
            }
            KeyCode::Char('3') if !self.chat_input_active => {
                self.active_tab = TabIndex::Graph;
            }
            KeyCode::Char('4') if !self.chat_input_active => {
                self.active_tab = TabIndex::Logs;
            }
            KeyCode::Char('5') if !self.chat_input_active => {
                self.active_tab = TabIndex::SearchResults;
            }
            // Chat input handling
            KeyCode::Char('i') if self.active_tab == TabIndex::Chat && !self.chat_input_active => {
                self.chat_input_active = true;
            }
            KeyCode::Char(c) if self.chat_input_active => {
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    self.chat_input.push(c);
                }
            }
            KeyCode::Backspace => {
                if self.chat_input_active {
                    self.chat_input.pop();
                } else if self.active_tab == TabIndex::Graph {
                    if let Ok(mut graph) = self.wallet_graph.lock() {
                        if graph.search_active {
                            graph.handle_input(GraphInput::SearchBackspace);
                        }
                    }
                }
            }
            KeyCode::Enter if self.chat_input_active => {
                if !self.chat_input.trim().is_empty() {
                    self.send_chat_message();
                }
                self.chat_input_active = false;
            }
            // Handle graph search character input
            KeyCode::Char(c) if self.active_tab == TabIndex::Graph => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    if graph.search_active {
                        graph.handle_input(GraphInput::SearchChar(c));
                    }
                }
            }
            _ => {}
        }
    }

    /// Handle key events when filter modal is active
    fn handle_filter_modal_key(&mut self, key: KeyEvent) {
        match key.code {
            // Close modal
            KeyCode::Esc | KeyCode::Char('`') | KeyCode::Char('q') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::CloseFilterModal);
                }
            }
            // Navigate items
            KeyCode::Up | KeyCode::Char('k') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    if graph.filter_modal.selected_index > 0 {
                        graph.filter_modal.selected_index -= 1;
                        // Adjust scroll offset if needed
                        if graph.filter_modal.selected_index < graph.filter_modal.scroll_offset {
                            graph.filter_modal.scroll_offset = graph.filter_modal.selected_index;
                        }
                    }
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    let max_idx = graph.filter_modal.current_items().len().saturating_sub(1);
                    if graph.filter_modal.selected_index < max_idx {
                        graph.filter_modal.selected_index += 1;
                        // Adjust scroll offset if needed (assume 10 visible items)
                        if graph.filter_modal.selected_index >= graph.filter_modal.scroll_offset + 15 {
                            graph.filter_modal.scroll_offset = graph.filter_modal.selected_index.saturating_sub(14);
                        }
                    }
                }
            }
            // Toggle selection with Space
            KeyCode::Char(' ') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::FilterToggleItem);
                }
            }
            // Tab to switch between Wallets/Programs/Tokens tabs
            KeyCode::Tab => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::FilterNextTab);
                }
            }
            KeyCode::BackTab => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::FilterPrevTab);
                }
            }
            // Select all with 'a'
            KeyCode::Char('a') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::FilterSelectAll);
                }
            }
            // Deselect all with 'x'
            KeyCode::Char('x') => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::FilterDeselectAll);
                }
            }
            // Enter to apply and close
            KeyCode::Enter => {
                if let Ok(mut graph) = self.wallet_graph.lock() {
                    graph.handle_input(GraphInput::CloseFilterModal);
                }
            }
            _ => {}
        }
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
                        // CHAT INPUT HANDLING
                        KeyCode::Char('i') if self.active_tab == TabIndex::Chat && !self.chat_input_active && !self.global_search_active => {
                            // Activate chat input mode
                            self.chat_input_active = true;
                            self.add_log("Chat input activated (press Enter to send, Esc to cancel)".to_string());
                        }
                        KeyCode::Char(c) if self.chat_input_active => {
                            if !key.modifiers.contains(KeyModifiers::CONTROL) {
                                self.chat_input.push(c);
                            }
                        }
                        KeyCode::Backspace if self.chat_input_active => {
                            self.chat_input.pop();
                        }
                        KeyCode::Enter if self.chat_input_active => {
                            // Send chat message
                            if !self.chat_input.trim().is_empty() {
                                self.send_chat_message();
                            }
                            self.chat_input_active = false;
                        }
                        KeyCode::Esc if self.chat_input_active => {
                            // Cancel chat input
                            self.chat_input.clear();
                            self.chat_input_active = false;
                            self.add_log("Chat input cancelled".to_string());
                        }
                        // Ctrl+X to cancel running investigation
                        KeyCode::Char('x') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                            let was_running = if let Ok(mut progress) = self.investigation_progress.lock() {
                                if progress.is_running && !progress.cancel_requested {
                                    progress.cancel_requested = true;
                                    progress.status_message = "Cancellation requested...".to_string();
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            };
                            if was_running {
                                self.add_log("🛑 Investigation cancellation requested (Ctrl+X)".to_string());
                            }
                        }
                        // Chat scrolling (j/k when not in input mode)
                        KeyCode::Char('j') if self.active_tab == TabIndex::Chat && !self.chat_input_active => {
                            self.chat_scroll = self.chat_scroll.saturating_add(1);
                        }
                        KeyCode::Char('k') if self.active_tab == TabIndex::Chat && !self.chat_input_active => {
                            self.chat_scroll = self.chat_scroll.saturating_sub(1);
                        }
                        // BBS INPUT HANDLING (when input is active) - must come BEFORE 'i' handler!
                        KeyCode::Char(c) if self.active_tab == TabIndex::BBS && {
                            // Check if input is active (need to peek at state for guard)
                            self.bbs_state.lock().map(|bbs| bbs.input_active).unwrap_or(false)
                        } => {
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                                    bbs.input_buffer.push(c);
                                }
                            }
                        }
                        // BBS INPUT MODE ACTIVATION (only when NOT in input mode)
                        KeyCode::Char('i') if self.active_tab == TabIndex::BBS => {
                            // Only activate if not already in input mode
                            let mut should_log = false;
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !bbs.input_active {
                                    bbs.input_active = true;
                                    should_log = true;
                                }
                            }
                            if should_log {
                                self.add_log("BBS input activated (press Enter to send, Esc to cancel)".to_string());
                            }
                        }
                        KeyCode::Backspace if self.active_tab == TabIndex::BBS => {
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if bbs.input_active {
                                    bbs.input_buffer.pop();
                                }
                            }
                        }
                        KeyCode::Enter if self.active_tab == TabIndex::BBS => {
                            let mut result_msg = None;
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if bbs.input_active && !bbs.input_buffer.trim().is_empty() {
                                    let message = bbs.input_buffer.clone();
                                    // Actually post the message to the database
                                    match bbs.post_message(&message) {
                                        Ok(post_id) => {
                                            result_msg = Some(format!("📬 Posted (ID: {}): {}", post_id, message));
                                            // Refresh posts to show the new message
                                            let _ = bbs.load_posts();
                                        }
                                        Err(e) => {
                                            result_msg = Some(format!("❌ Failed to post: {}", e));
                                        }
                                    }
                                    bbs.input_buffer.clear();
                                    bbs.input_active = false;
                                }
                            }
                            if let Some(msg) = result_msg {
                                self.add_log(msg);
                            }
                        }
                        KeyCode::Esc if self.active_tab == TabIndex::BBS => {
                            let mut should_log = false;
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if bbs.input_active {
                                    bbs.input_buffer.clear();
                                    bbs.input_active = false;
                                    should_log = true;
                                }
                            }
                            if should_log {
                                self.add_log("BBS input cancelled".to_string());
                            }
                        }
                        // BBS scrolling (j/k for scrolling posts - only when NOT in input mode)
                        KeyCode::Char('j') | KeyCode::Down if self.active_tab == TabIndex::BBS => {
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !bbs.input_active {
                                    bbs.scroll_offset = bbs.scroll_offset.saturating_add(1);
                                }
                            }
                        }
                        KeyCode::Char('k') | KeyCode::Up if self.active_tab == TabIndex::BBS => {
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !bbs.input_active {
                                    bbs.scroll_offset = bbs.scroll_offset.saturating_sub(1);
                                }
                            }
                        }
                        // BBS board selection (1-9 to select board - only when NOT in input mode)
                        KeyCode::Char(c @ '1'..='9') if self.active_tab == TabIndex::BBS => {
                            let mut log_msg = None;
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !bbs.input_active {
                                    let board_idx = c.to_digit(10).unwrap() as usize - 1;
                                    if board_idx < bbs.boards.len() {
                                        let board_id = bbs.boards[board_idx].id;
                                        let board_name = bbs.boards[board_idx].name.clone();
                                        bbs.current_board = Some(board_id);
                                        bbs.selected_board_index = Some(board_idx);
                                        let _ = bbs.load_posts();
                                        bbs.scroll_offset = 0;
                                        log_msg = Some(format!("📋 Switched to board: {}", board_name));
                                    }
                                }
                            }
                            if let Some(msg) = log_msg {
                                self.add_log(msg);
                            }
                        }
                        // BBS refresh (r key - only when NOT in input mode)
                        KeyCode::Char('r') if self.active_tab == TabIndex::BBS => {
                            let mut should_log = false;
                            if let Ok(mut bbs) = self.bbs_state.lock() {
                                if !bbs.input_active {
                                    let _ = bbs.refresh_boards();
                                    if bbs.current_board.is_some() {
                                        let _ = bbs.load_posts();
                                    }
                                    should_log = true;
                                }
                            }
                            if should_log {
                                self.add_log("🔄 BBS refreshed".to_string());
                            }
                        }
                        // FEDERATION: Refresh (r key triggers async health check)
                        KeyCode::Char('r') if self.active_tab == TabIndex::Federation => {
                            self.federation_refresh_pending = true;
                            self.add_log("🔄 Refreshing federation with health checks...".to_string());
                        }
                        // FEDERATION: Scroll sessions (j/k)
                        KeyCode::Char('j') | KeyCode::Down if self.active_tab == TabIndex::Federation => {
                            self.federation_scroll = self.federation_scroll.saturating_add(1);
                        }
                        KeyCode::Char('k') | KeyCode::Up if self.active_tab == TabIndex::Federation => {
                            self.federation_scroll = self.federation_scroll.saturating_sub(1);
                        }
                        // FEDERATION: Navigate peer selection
                        KeyCode::Char('n') | KeyCode::Char('N') if self.active_tab == TabIndex::Federation => {
                            // Select next peer
                            let peer_count = self.federation_state.lock()
                                .map(|s| s.peers.len())
                                .unwrap_or(0);
                            if peer_count > 0 {
                                self.federation_selected_peer = Some(
                                    self.federation_selected_peer
                                        .map(|i| (i + 1) % peer_count)
                                        .unwrap_or(0)
                                );
                            }
                        }
                        KeyCode::Char('p') | KeyCode::Char('P') if self.active_tab == TabIndex::Federation => {
                            // Select previous peer
                            let peer_count = self.federation_state.lock()
                                .map(|s| s.peers.len())
                                .unwrap_or(0);
                            if peer_count > 0 {
                                self.federation_selected_peer = Some(
                                    self.federation_selected_peer
                                        .map(|i| if i == 0 { peer_count - 1 } else { i - 1 })
                                        .unwrap_or(peer_count - 1)
                                );
                            }
                        }
                        // FEDERATION: Add peer (a key opens input mode)
                        KeyCode::Char('a') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            self.federation_input_active = true;
                            self.federation_input_buffer.clear();
                            self.add_log("Adding peer - enter address (e.g., http://192.168.1.100:8080)".to_string());
                        }
                        // FEDERATION: Delete selected peer (d key)
                        KeyCode::Char('d') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            if let Some(idx) = self.federation_selected_peer {
                                let node_id = self.federation_state.lock()
                                    .ok()
                                    .and_then(|s| s.peers.get(idx).map(|p| p.node_id.clone()));
                                if let Some(node_id) = node_id {
                                    self.federation_delete_pending = Some(node_id.clone());
                                    self.add_log(format!("🗑️ Deleting peer {}...", node_id));
                                }
                            }
                        }
                        // FEDERATION: Input handling
                        KeyCode::Char(c) if self.active_tab == TabIndex::Federation && self.federation_input_active => {
                            self.federation_input_buffer.push(c);
                        }
                        KeyCode::Backspace if self.active_tab == TabIndex::Federation && self.federation_input_active => {
                            self.federation_input_buffer.pop();
                        }
                        KeyCode::Enter if self.active_tab == TabIndex::Federation && self.federation_input_active => {
                            if !self.federation_input_buffer.trim().is_empty() {
                                self.federation_add_pending = Some(self.federation_input_buffer.clone());
                                self.add_log(format!("➕ Adding peer {}...", self.federation_input_buffer));
                            }
                            self.federation_input_buffer.clear();
                            self.federation_input_active = false;
                        }
                        KeyCode::Esc if self.active_tab == TabIndex::Federation && self.federation_input_active => {
                            self.federation_input_buffer.clear();
                            self.federation_input_active = false;
                            self.add_log("Add peer cancelled".to_string());
                        }
                        // FEDERATION: Session selection (s to select next, S to select prev)
                        KeyCode::Char('s') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            let session_count = self.federation_state.lock()
                                .map(|s| s.sessions.len())
                                .unwrap_or(0);
                            if session_count > 0 {
                                self.federation_selected_session = Some(
                                    self.federation_selected_session
                                        .map(|i| (i + 1) % session_count)
                                        .unwrap_or(0)
                                );
                            }
                        }
                        KeyCode::Char('S') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            let session_count = self.federation_state.lock()
                                .map(|s| s.sessions.len())
                                .unwrap_or(0);
                            if session_count > 0 {
                                self.federation_selected_session = Some(
                                    self.federation_selected_session
                                        .map(|i| if i == 0 { session_count - 1 } else { i - 1 })
                                        .unwrap_or(session_count - 1)
                                );
                            }
                        }
                        // FEDERATION: Join selected session (J key)
                        KeyCode::Char('J') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            if let Some(idx) = self.federation_selected_session {
                                let session_info = self.federation_state.lock()
                                    .ok()
                                    .and_then(|s| s.sessions.get(idx).cloned());
                                if let Some(session) = session_info {
                                    // Show join command
                                    self.add_log(format!("📌 Session: {} ({})", session.name, session.session_id));
                                    self.add_log(format!("👤 Host: {} | Users: {}", session.host_node_id, session.participant_count));
                                    self.add_log(format!("🔗 Join: osvm collab join {}", session.session_id));
                                    self.add_log("   (Copy command and run in new terminal to join)".to_string());
                                }
                            } else {
                                self.add_log("⚠️ Select a session first (s/S to navigate)".to_string());
                            }
                        }
                        // FEDERATION: Toggle annotation stream display (A key)
                        KeyCode::Char('A') if self.active_tab == TabIndex::Federation && !self.federation_input_active => {
                            self.federation_show_annotations = !self.federation_show_annotations;
                            if self.federation_show_annotations {
                                // Show annotations from selected session (or all)
                                let annotations = self.federation_state.lock()
                                    .map(|s| s.live_annotations.clone())
                                    .unwrap_or_default();

                                if annotations.is_empty() {
                                    self.add_log("📝 ANNOTATION STREAM ENABLED".to_string());
                                    self.add_log("   No annotations yet. To add:".to_string());
                                    self.add_log("   osvm collab annotate <wallet> \"Note\"".to_string());
                                } else {
                                    self.add_log("📝 LIVE ANNOTATIONS:".to_string());
                                    for ann in annotations.iter().take(10) {
                                        let severity_icon = match ann.severity.as_str() {
                                            "critical" => "🚨",
                                            "warning" => "⚠️",
                                            "important" => "⭐",
                                            "question" => "❓",
                                            _ => "ℹ️",
                                        };
                                        self.add_log(format!("   {} {} @{}: {}", severity_icon, ann.timestamp, ann.author, ann.text));
                                        self.add_log(format!("      Target: {}", ann.target));
                                    }
                                }
                            } else {
                                self.add_log("📝 Annotation stream disabled".to_string());
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
                        KeyCode::Char('0') => self.active_tab = TabIndex::Chat,
                        KeyCode::Char('1') => self.active_tab = TabIndex::Dashboard,
                        KeyCode::Char('2') => self.active_tab = TabIndex::Graph,
                        KeyCode::Char('3') => self.active_tab = TabIndex::Logs,
                        KeyCode::Char('4') => self.active_tab = TabIndex::SearchResults,
                        KeyCode::Char('5') => self.active_tab = TabIndex::BBS,
                        KeyCode::Char('6') => self.active_tab = TabIndex::Federation,
                        KeyCode::Char('t') | KeyCode::Char('T') => {
                            // Toggle investigation trail visibility in graph view
                            if self.active_tab == TabIndex::Graph {
                                let mut log_message = String::new();
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.show_trail = !graph.show_trail;
                                    // Prepare feedback message
                                    let status = if graph.show_trail { "shown" } else { "hidden" };
                                    log_message = format!("Investigation trail {}", status);
                                }
                                if !log_message.is_empty() {
                                    self.add_log(log_message);
                                }
                            }
                        }
                        // Graph navigation / Help scrolling / Search suggestions / AI Insights scrolling
                        KeyCode::Char('j') | KeyCode::Down => {
                            if self.global_search_active {
                                if !self.search_suggestions.is_empty() {
                                    self.selected_suggestion = (self.selected_suggestion + 1) % self.search_suggestions.len();
                                }
                            } else if self.show_help {
                                self.help_scroll = self.help_scroll.saturating_add(1);
                            } else if self.active_tab == TabIndex::Dashboard {
                                // Scroll AI Insights panel in dashboard
                                self.ai_insights_scroll = self.ai_insights_scroll.saturating_add(1);
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
                            } else if self.active_tab == TabIndex::Dashboard {
                                // Scroll AI Insights panel in dashboard
                                self.ai_insights_scroll = self.ai_insights_scroll.saturating_sub(1);
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
                        KeyCode::Char('j') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::ScrollDetailDown);
                                }
                            }
                        }
                        KeyCode::Char('k') => {
                            if self.active_tab == TabIndex::Graph {
                                if let Ok(mut graph) = self.wallet_graph.lock() {
                                    graph.handle_input(GraphInput::ScrollDetailUp);
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
            TabIndex::Chat => TabIndex::Dashboard,
            TabIndex::Dashboard => TabIndex::Graph,
            TabIndex::Graph => TabIndex::Logs,
            TabIndex::Logs => TabIndex::SearchResults,
            TabIndex::SearchResults => TabIndex::BBS,
            TabIndex::BBS => TabIndex::Federation,
            TabIndex::Federation => TabIndex::Chat,
        };
    }

    fn previous_tab(&mut self) {
        self.active_tab = match self.active_tab {
            TabIndex::Chat => TabIndex::Federation,
            TabIndex::Dashboard => TabIndex::Chat,
            TabIndex::Graph => TabIndex::Dashboard,
            TabIndex::Logs => TabIndex::Graph,
            TabIndex::SearchResults => TabIndex::Logs,
            TabIndex::BBS => TabIndex::SearchResults,
            TabIndex::Federation => TabIndex::BBS,
        };
    }

    fn ui(&mut self, f: &mut Frame) {
        let size = f.area();

        match self.active_tab {
            TabIndex::Chat => self.render_chat(f, size),
            TabIndex::Dashboard => self.render_dashboard(f, size),
            TabIndex::Graph => self.render_full_graph(f, size),
            TabIndex::Logs => self.render_full_logs(f, size),
            TabIndex::SearchResults => self.render_search_results_tab(f, size),
            TabIndex::BBS => {
                if let Ok(mut bbs_state) = self.bbs_state.lock() {
                    crate::utils::bbs::tui_widgets::render_bbs_tab(f, size, &mut bbs_state);
                }
            }
            TabIndex::Federation => {
                // TODO: Implement federation dashboard
                // For now, reuse the BBS render since federation is related
                if let Ok(mut bbs_state) = self.bbs_state.lock() {
                    crate::utils::bbs::tui_widgets::render_bbs_tab(f, size, &mut bbs_state);
                }
            }
        }

        // Render filter modal if active (only on Graph tab)
        let show_filter_modal = if self.active_tab == TabIndex::Graph {
            self.wallet_graph.lock()
                .map(|g| g.filter_modal.active)
                .unwrap_or(false)
        } else {
            false
        };

        if show_filter_modal {
            self.render_filter_modal(f, size);
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
        // Check if investigation is running or has data
        let show_investigation_panel = self.investigation_progress.lock()
            .map(|p| p.is_running || p.wallets_explored > 0)
            .unwrap_or(false);

        // Main vertical split: header bar, network stats/investigation, content, footer
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),   // Header with tabs
                Constraint::Length(if show_investigation_panel { 7 } else { 5 }),   // Investigation or network stats
                Constraint::Min(0),      // Content
                Constraint::Length(2),   // Status bar (btop style)
            ])
            .split(area);

        self.render_btop_header(f, main_chunks[0]);

        // Show investigation progress panel when investigation is running or completed
        if show_investigation_panel {
            self.render_investigation_progress(f, main_chunks[1]);
        } else {
            self.render_network_stats_panel(f, main_chunks[1]);
        }

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
                Constraint::Percentage(40), // AI insights - flexible but capped at 40% of available space
                Constraint::Min(8),      // Transfer list - minimum 8 lines, takes remaining space
            ])
            .split(content_chunks[1]);

        self.render_progress_gauges(f, right_chunks[0]);
        self.render_volume_bars(f, right_chunks[1]);
        self.render_ai_insights(f, right_chunks[2]);
        self.render_transfer_feed(f, right_chunks[3]);

        self.render_btop_statusbar(f, main_chunks[2]);
    }

    /// Render chat interface inspired by Tenere
    fn render_chat(&mut self, f: &mut Frame, area: Rect) {
        // Main layout: header, chat messages, input
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),   // Header with tabs
                Constraint::Min(0),      // Chat messages area
                Constraint::Length(3),   // Input box
                Constraint::Length(2),   // Status bar
            ])
            .split(area);

        // Render header
        self.render_btop_header(f, chunks[0]);

        // Render chat messages
        self.render_chat_messages(f, chunks[1]);

        // Render input box
        self.render_chat_input(f, chunks[2]);

        // Render status bar
        self.render_btop_statusbar(f, chunks[3]);
    }

    /// Render chat messages with proper styling
    fn render_chat_messages(&mut self, f: &mut Frame, area: Rect) {
        let messages = self.chat_messages.lock().unwrap();

        let mut lines: Vec<Line> = Vec::new();

        for msg in messages.iter() {
            // Message header with timestamp and role
            let role_style = match msg.role.as_str() {
                "user" => Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
                "assistant" => Style::default().fg(Color::Green).add_modifier(Modifier::BOLD),
                _ => Style::default(),
            };

            let status_indicator = match msg.status {
                MessageStatus::Sending => " ⏳",
                MessageStatus::Delivered => " ✓",
                MessageStatus::Streaming => " ⋯",
                MessageStatus::Complete => "",
                MessageStatus::Error => " ✗",
            };

            lines.push(Line::from(vec![
                Span::styled(
                    format!("[{}] ", msg.timestamp),
                    Style::default().fg(Color::DarkGray),
                ),
                Span::styled(
                    format!("{}{}", msg.role.to_uppercase(), status_indicator),
                    role_style,
                ),
            ]));

            // Message content with wrapping
            for line in msg.content.lines() {
                let content_style = match msg.role.as_str() {
                    "user" => Style::default().fg(Color::White),
                    "assistant" => Style::default().fg(Color::Gray),
                    _ => Style::default(),
                };
                lines.push(Line::from(Span::styled(line.to_string(), content_style)));
            }

            // Add spacing between messages
            lines.push(Line::from(""));
        }

        // Calculate scroll position
        let total_lines = lines.len();
        let visible_lines = area.height.saturating_sub(2) as usize; // Account for borders

        // Auto-scroll to bottom if enabled
        if self.chat_auto_scroll {
            self.chat_scroll = total_lines.saturating_sub(visible_lines);
        }

        // Ensure scroll doesn't exceed bounds
        let max_scroll = total_lines.saturating_sub(visible_lines);
        self.chat_scroll = self.chat_scroll.min(max_scroll);

        // Slice visible lines
        let visible_lines_vec: Vec<Line> = lines
            .into_iter()
            .skip(self.chat_scroll)
            .take(visible_lines)
            .collect();

        let scroll_indicator = if total_lines > visible_lines {
            format!(" 📜 {}/{} ", self.chat_scroll + visible_lines, total_lines)
        } else {
            String::new()
        };

        let paragraph = Paragraph::new(visible_lines_vec)
            .block(
                Block::default()
                    .title(Span::styled(
                        format!(" 💬 AI Research Chat{} ", scroll_indicator),
                        Style::default()
                            .fg(Color::Cyan)
                            .add_modifier(Modifier::BOLD),
                    ))
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Cyan))
                    .border_type(BorderType::Rounded),
            )
            .wrap(Wrap { trim: false });

        f.render_widget(paragraph, area);
    }

    /// Render chat input box
    fn render_chat_input(&self, f: &mut Frame, area: Rect) {
        let input_text = if self.chat_input_active {
            format!("{}█", self.chat_input)  // Show cursor
        } else {
            if self.chat_input.is_empty() {
                "Press 'i' to start typing...".to_string()
            } else {
                self.chat_input.clone()
            }
        };

        let input_style = if self.chat_input_active {
            Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let border_style = if self.chat_input_active {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let paragraph = Paragraph::new(input_text)
            .style(input_style)
            .block(
                Block::default()
                    .title(" 📝 Input ")
                    .borders(Borders::ALL)
                    .border_style(border_style)
                    .border_type(BorderType::Rounded),
            );

        f.render_widget(paragraph, area);
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
            if self.active_tab == TabIndex::Chat {
                Span::styled(" ▣ Chat ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            } else {
                Span::styled(" □ Chat ", Style::default().fg(Color::DarkGray))
            },
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
            if self.active_tab == TabIndex::BBS {
                Span::styled(" ▣ BBS ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            } else {
                Span::styled(" □ BBS ", Style::default().fg(Color::DarkGray))
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

    /// Investigation progress panel - shows when autonomous investigation is running
    fn render_investigation_progress(&self, f: &mut Frame, area: Rect) {
        let progress = self.investigation_progress.lock().ok();

        let (is_running, phase, wallets_explored, wallets_pending, max_wallets,
             current_depth, max_depth, transfers_found, findings_count, elapsed_secs, status) =
            progress.map(|p| (
                p.is_running,
                format!("{:?}", p.phase),
                p.wallets_explored,
                p.wallets_pending,
                p.max_wallets,
                p.current_depth,
                p.max_depth,
                p.transfers_found,
                p.findings.len(),
                p.start_time.map(|t| t.elapsed().as_secs()).unwrap_or(0),
                p.status_message.clone(),
            )).unwrap_or((false, "Idle".to_string(), 0, 0, 50, 0, 3, 0, 0, 0, "No investigation".to_string()));

        let title_style = if is_running {
            Style::default().fg(Color::Green).add_modifier(Modifier::BOLD | Modifier::SLOW_BLINK)
        } else {
            Style::default().fg(Color::DarkGray)
        };

        let border_color = if is_running { Color::Green } else { Color::DarkGray };

        let block = Block::default()
            .title(Span::styled(" 🔬 Investigation ", title_style))
            .borders(Borders::ALL)
            .border_type(BorderType::Double)
            .border_style(Style::default().fg(border_color));

        let inner = block.inner(area);
        f.render_widget(block, area);

        if !is_running && wallets_explored == 0 {
            // No investigation running or completed
            let hint = Paragraph::new(Line::from(vec![
                Span::styled("Type ", Style::default().fg(Color::DarkGray)),
                Span::styled("\"investigate\"", Style::default().fg(Color::Yellow)),
                Span::styled(" to start autonomous forensics", Style::default().fg(Color::DarkGray)),
            ])).alignment(Alignment::Center);
            f.render_widget(hint, inner);
            return;
        }

        // Layout for progress content
        let content_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1),  // Status line
                Constraint::Length(1),  // Wallets progress
                Constraint::Length(1),  // Depth progress
                Constraint::Length(1),  // Stats line
                Constraint::Min(0),     // Padding
            ])
            .split(inner);

        // Status line with phase indicator
        let phase_icon = match phase.as_str() {
            s if s.contains("Initializing") => "🚀",
            s if s.contains("Exploring") => "🔍",
            s if s.contains("Analyzing") => "📊",
            s if s.contains("Generating") => "📝",
            s if s.contains("Complete") => "✅",
            s if s.contains("Failed") => "❌",
            _ => "⏸️",
        };
        let status_line = Paragraph::new(Line::from(vec![
            Span::styled(format!("{} ", phase_icon), Style::default()),
            Span::styled(&status, Style::default().fg(if is_running { Color::Cyan } else { Color::Green })),
        ]));
        f.render_widget(status_line, content_chunks[0]);

        // Wallets progress bar
        let wallet_pct = if max_wallets > 0 { (wallets_explored as f64 / max_wallets as f64 * 100.0).min(100.0) } else { 0.0 };
        let wallet_bar_width = inner.width.saturating_sub(20) as usize;
        let wallet_filled = ((wallet_pct / 100.0) * wallet_bar_width as f64) as usize;
        let wallet_empty = wallet_bar_width.saturating_sub(wallet_filled);
        let wallet_line = Paragraph::new(Line::from(vec![
            Span::styled("Wallets  ", Style::default().fg(Color::White)),
            Span::styled("█".repeat(wallet_filled), Style::default().fg(Color::Green)),
            Span::styled("░".repeat(wallet_empty), Style::default().fg(Color::DarkGray)),
            Span::styled(format!(" {}/{}", wallets_explored, max_wallets), Style::default().fg(Color::Cyan)),
        ]));
        f.render_widget(wallet_line, content_chunks[1]);

        // Depth progress bar
        let depth_pct = if max_depth > 0 { (current_depth as f64 / max_depth as f64 * 100.0).min(100.0) } else { 0.0 };
        let depth_bar_width = inner.width.saturating_sub(20) as usize;
        let depth_filled = ((depth_pct / 100.0) * depth_bar_width as f64) as usize;
        let depth_empty = depth_bar_width.saturating_sub(depth_filled);
        let depth_line = Paragraph::new(Line::from(vec![
            Span::styled("Depth    ", Style::default().fg(Color::White)),
            Span::styled("█".repeat(depth_filled), Style::default().fg(Color::Magenta)),
            Span::styled("░".repeat(depth_empty), Style::default().fg(Color::DarkGray)),
            Span::styled(format!(" {}/{}", current_depth, max_depth), Style::default().fg(Color::Cyan)),
        ]));
        f.render_widget(depth_line, content_chunks[2]);

        // Stats line
        let stats_line = Paragraph::new(Line::from(vec![
            Span::styled("📨 ", Style::default()),
            Span::styled(format!("{}", transfers_found), Style::default().fg(Color::Yellow)),
            Span::styled(" transfers │ ", Style::default().fg(Color::DarkGray)),
            Span::styled("🔎 ", Style::default()),
            Span::styled(format!("{}", findings_count), Style::default().fg(Color::Red)),
            Span::styled(" findings │ ", Style::default().fg(Color::DarkGray)),
            Span::styled("⏱ ", Style::default()),
            Span::styled(format!("{}s", elapsed_secs), Style::default().fg(Color::Blue)),
            if wallets_pending > 0 {
                Span::styled(format!(" │ 📋 {} queued", wallets_pending), Style::default().fg(Color::DarkGray))
            } else {
                Span::raw("")
            },
        ]));
        f.render_widget(stats_line, content_chunks[3]);
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

                // Show up to 7 tokens (fits in 9-line panel with title/borders)
                for (i, vol) in vols.iter().take(7).enumerate() {
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
        // LIVE TRANSACTION FEED (updated every 5s from latest blocks)
        let live_txs = self.live_transactions.lock().ok();

        let block = Block::default()
            .title(Span::styled(" 🔴 LIVE Transactions ", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::Red));

        let inner_area = block.inner(area);
        f.render_widget(block.clone(), area);

        let mut items: Vec<ListItem> = Vec::new();

        if let Some(txs) = live_txs {
            if txs.is_empty() {
                items.push(ListItem::new("  🔄 Fetching live transactions...").style(Style::default().fg(Color::Yellow)));
            } else {
                for tx in txs.iter().take((inner_area.height as usize).saturating_sub(1)) {
                    let (icon, color) = if tx.success {
                        ("✓", Color::Green)
                    } else {
                        ("✗", Color::Red)
                    };

                    let type_icon = match tx.tx_type.as_str() {
                        "Transfer" => "💸",
                        "Contract" => "⚙️ ",
                        _ => "📄",
                    };

                    items.push(ListItem::new(vec![
                        Line::from(vec![
                            Span::styled(format!(" {} ", icon), Style::default().fg(color).add_modifier(Modifier::BOLD)),
                            Span::styled(type_icon, Style::default()),
                            Span::styled(format!(" {}", tx.signature), Style::default().fg(Color::Cyan)),
                        ]),
                        Line::from(vec![
                            Span::styled("    ", Style::default()),
                            Span::styled(format!("{} • ", tx.timestamp), Style::default().fg(Color::DarkGray)),
                            Span::styled(format!("{:.4} SOL", tx.amount_sol), Style::default().fg(Color::Yellow)),
                        ]),
                    ]));
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

    /// Render the filter modal for graph node/token filtering
    fn render_filter_modal(&mut self, f: &mut Frame, area: Rect) {
        use super::graph::FilterTab;

        // Get filter modal state
        let (current_tab, selected_index, scroll_offset, items, counts) = {
            if let Ok(graph) = self.wallet_graph.lock() {
                let items = graph.filter_modal.current_items();
                let counts = (
                    graph.filter_modal.all_wallets.len(),
                    graph.filter_modal.all_programs.len(),
                    graph.filter_modal.all_tokens.len(),
                    graph.filter_modal.selected_wallets.len(),
                    graph.filter_modal.selected_programs.len(),
                    graph.filter_modal.selected_tokens.len(),
                );
                (
                    graph.filter_modal.current_tab,
                    graph.filter_modal.selected_index,
                    graph.filter_modal.scroll_offset,
                    items,
                    counts,
                )
            } else {
                return;
            }
        };

        // Center the modal (50% width, 60% height)
        let width = (area.width * 60 / 100).max(50).min(area.width.saturating_sub(4));
        let height = (area.height * 70 / 100).max(20).min(area.height.saturating_sub(4));
        let x = (area.width.saturating_sub(width)) / 2;
        let y = (area.height.saturating_sub(height)) / 2;
        let modal_area = Rect::new(x, y, width, height);

        // Clear background
        f.render_widget(Clear, modal_area);

        // Create main block
        let block = Block::default()
            .title(" 🔍 Filter Graph ")
            .title_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Cyan))
            .style(Style::default().bg(Color::Black));

        let inner = block.inner(modal_area);
        f.render_widget(block, modal_area);

        // Split into tabs bar and content
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3), // Tabs
                Constraint::Min(0),    // Content
                Constraint::Length(3), // Footer with instructions
            ])
            .split(inner);

        // Render tabs
        let (wallet_count, program_count, token_count, sel_wallet, sel_program, sel_token) = counts;
        let tabs = vec![
            format!(" Wallets ({}/{}) ", sel_wallet, wallet_count),
            format!(" Programs ({}/{}) ", sel_program, program_count),
            format!(" Tokens ({}/{}) ", sel_token, token_count),
        ];

        let tab_idx = match current_tab {
            FilterTab::Wallets => 0,
            FilterTab::Programs => 1,
            FilterTab::Tokens => 2,
        };

        let tabs_widget = Tabs::new(tabs.iter().map(|s| s.as_str()).collect::<Vec<_>>())
            .block(Block::default().borders(Borders::BOTTOM))
            .select(tab_idx)
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD));

        f.render_widget(tabs_widget, chunks[0]);

        // Render content list
        let visible_height = chunks[1].height as usize;
        let display_items: Vec<ListItem> = items
            .iter()
            .enumerate()
            .skip(scroll_offset)
            .take(visible_height)
            .map(|(idx, (name, selected))| {
                let checkbox = if *selected { "☑" } else { "☐" };
                let style = if idx == selected_index {
                    Style::default().fg(Color::Yellow).bg(Color::DarkGray).add_modifier(Modifier::BOLD)
                } else if *selected {
                    Style::default().fg(Color::Green)
                } else {
                    Style::default().fg(Color::DarkGray)
                };

                // Truncate long addresses
                let display_name = if name.len() > 50 {
                    format!("{}...{}", &name[..20], &name[name.len()-20..])
                } else {
                    name.clone()
                };

                ListItem::new(Line::from(vec![
                    Span::styled(format!(" {} ", checkbox), style),
                    Span::styled(display_name, style),
                ]))
            })
            .collect();

        let list = List::new(display_items)
            .block(Block::default());

        f.render_widget(list, chunks[1]);

        // Render footer with instructions
        let footer_text = Line::from(vec![
            Span::styled(" ↑↓ ", Style::default().fg(Color::Yellow)),
            Span::raw("navigate  "),
            Span::styled("Space", Style::default().fg(Color::Yellow)),
            Span::raw(" toggle  "),
            Span::styled("Tab", Style::default().fg(Color::Yellow)),
            Span::raw(" switch  "),
            Span::styled("a", Style::default().fg(Color::Yellow)),
            Span::raw(" all  "),
            Span::styled("x", Style::default().fg(Color::Yellow)),
            Span::raw(" none  "),
            Span::styled("Enter/`/Esc", Style::default().fg(Color::Yellow)),
            Span::raw(" apply"),
        ]);

        let footer = Paragraph::new(footer_text)
            .style(Style::default().fg(Color::DarkGray))
            .alignment(Alignment::Center)
            .block(Block::default().borders(Borders::TOP));

        f.render_widget(footer, chunks[2]);
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
            Line::from("   0-6          Switch views (Chat/Dash/Graph/Logs/Search/BBS/Fed)"),
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
            Line::from("   T            Toggle investigation trail breadcrumb"),
            Line::from("   /            Start search (ESC to cancel)"),
            Line::from("   n/N          Next/previous search result"),
            Line::from("   y            Copy to clipboard (wallet/search results)"),
            Line::from("   Enter        Center graph on selected wallet (hop)"),
            Line::from("   Space        Expand or collapse node"),
            Line::from(""),
            Line::from(Span::styled(" ─── Chat View ───────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   i            Activate chat input mode"),
            Line::from("   Enter        Send message (when in input mode)"),
            Line::from("   Esc          Cancel input"),
            Line::from("   j/k          Scroll chat history (when not typing)"),
            Line::from(""),
            Line::from(Span::styled(" ─── Dashboard View ──────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   j/k          Scroll AI Insights panel"),
            Line::from(""),
            Line::from(Span::styled(" ─── Logs View ───────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   j/k          Scroll line by line"),
            Line::from(""),
            Line::from(Span::styled(" ─── BBS View ────────────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   i            Activate message input"),
            Line::from("   Enter        Send message (when typing)"),
            Line::from("   Esc          Cancel input"),
            Line::from("   j/k/↑/↓      Scroll posts (when not typing)"),
            Line::from("   1-9          Select board by number"),
            Line::from("   r            Refresh boards and posts"),
            Line::from(""),
            Line::from(Span::styled(" ─── Federation View ─────────────────────────────────────────", Style::default().fg(Color::Yellow))),
            Line::from("   r            Refresh peer status from disk"),
            Line::from("   a            Add new peer (enter address, then Enter)"),
            Line::from("   d            Delete selected peer"),
            Line::from("   n/p          Select next/previous peer"),
            Line::from("   s/S          Select next/previous session"),
            Line::from("   J            Show join command for selected session"),
            Line::from("   A            Toggle annotation stream display"),
            Line::from("   j/k/↑/↓      Scroll sessions list"),
            Line::from("   Esc          Cancel add peer input"),
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
        // Calculate total lines for scroll indicator
        let block = Block::default()
            .title(Span::styled(
                format!(" 💡 AI Insights {} ",
                    if self.ai_insights_scroll > 0 {
                        format!("(scroll: {})", self.ai_insights_scroll)
                    } else {
                        String::new()
                    }
                ),
                Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD)
            ))
            .borders(Borders::ALL)
            .border_type(BorderType::Rounded)
            .border_style(Style::default().fg(Color::DarkGray));

        let inner_area = block.inner(area);
        f.render_widget(block, area);

        let mut lines = Vec::new();

        // Generate real-time automated analysis with explainable insights
        if let Ok(mut graph) = self.wallet_graph.lock() {
            // Use cached explainable risk scoring system (avoids expensive recalculation on every frame)
            let risk_explanation = graph.get_cached_risk_explanation();

            // Display critical alerts first (highest priority)
            for alert in &risk_explanation.alerts {
                let color = if alert.contains("CRITICAL") || alert.contains("🚨") || alert.contains("🔴") {
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

            // Add separator after alerts if there are any
            if !risk_explanation.alerts.is_empty() {
                lines.push(Line::from(vec![
                    Span::styled(" ─────────────────────────────", Style::default().fg(Color::DarkGray)),
                ]));
            }

            // Show overall risk score with level indicator (prominent)
            let (risk_color, risk_icon) = match risk_explanation.level {
                crate::utils::tui::graph::RiskLevel::Critical => (Color::Red, "🔴"),
                crate::utils::tui::graph::RiskLevel::High => (Color::LightRed, "🟠"),
                crate::utils::tui::graph::RiskLevel::Medium => (Color::Yellow, "🟡"),
                crate::utils::tui::graph::RiskLevel::Low => (Color::Green, "🟢"),
            };

            lines.push(Line::from(vec![
                Span::styled(" ", Style::default()),
                Span::styled(
                    format!("{} RISK SCORE: {:.0}/100 ({:?})", risk_icon, risk_explanation.score, risk_explanation.level),
                    Style::default().fg(risk_color).add_modifier(Modifier::BOLD)
                ),
            ]));

            // Add actionable context for risk score interpretation
            let risk_context = match risk_explanation.level {
                crate::utils::tui::graph::RiskLevel::Critical => "  ⚠ Action: Review mixer patterns, verify source wallets, document chain",
                crate::utils::tui::graph::RiskLevel::High => "  ⚠ Action: Verify counterparties, analyze timing patterns, export data",
                crate::utils::tui::graph::RiskLevel::Medium => "  ℹ Action: Monitor for changes, track volume spikes, note patterns",
                crate::utils::tui::graph::RiskLevel::Low => "  ✓ Status: Normal patterns observed, continue routine monitoring",
            };
            lines.push(Line::from(vec![
                Span::styled(risk_context, Style::default().fg(Color::DarkGray)),
            ]));

            // Display detailed reasons (explanations) - increased to 10 for expanded panel
            if !risk_explanation.reasons.is_empty() {
                lines.push(Line::from(vec![
                    Span::styled("  Reasons:", Style::default().fg(Color::Cyan)),
                ]));
            }
            for (idx, reason) in risk_explanation.reasons.iter().take(10).enumerate() {
                let prefix = if idx == risk_explanation.reasons.len() - 1 || idx == 9 {
                    "   └─ "  // Last item
                } else {
                    "   ├─ "  // Regular item
                };
                lines.push(Line::from(vec![
                    Span::styled(prefix, Style::default().fg(Color::DarkGray)),
                    Span::styled(reason.clone(), Style::default().fg(Color::Gray)),
                ]));
            }

            // Wallet behavior classification for target
            let behavior = graph.classify_wallet_behavior(0);
            let (behavior_icon, behavior_color) = match behavior {
                crate::utils::tui::graph::WalletBehaviorType::Bot => ("🤖", Color::Yellow),
                crate::utils::tui::graph::WalletBehaviorType::Exchange => ("🏦", Color::Cyan),
                crate::utils::tui::graph::WalletBehaviorType::Trader => ("📈", Color::Green),
                crate::utils::tui::graph::WalletBehaviorType::Mixer => ("🌀", Color::Red),
                crate::utils::tui::graph::WalletBehaviorType::EOA => ("👤", Color::Blue),
                crate::utils::tui::graph::WalletBehaviorType::Contract => ("📜", Color::Magenta),
                crate::utils::tui::graph::WalletBehaviorType::Dormant => ("💤", Color::DarkGray),
            };

            lines.push(Line::from(vec![
                Span::styled(" ", Style::default()),
                Span::styled(
                    format!("{} Behavior: {:?}", behavior_icon, behavior),
                    Style::default().fg(behavior_color)
                ),
            ]));

            // Entity clustering info
            if !graph.entity_clusters.is_empty() {
                let total_clustered: usize = graph.entity_clusters.iter()
                    .map(|c| c.wallet_addresses.len())
                    .sum();

                lines.push(Line::from(vec![
                    Span::styled(" ", Style::default()),
                    Span::styled(
                        format!("🔗 {} entity clusters ({} wallets coordinated)",
                            graph.entity_clusters.len(), total_clustered),
                        Style::default().fg(Color::LightMagenta).add_modifier(Modifier::BOLD)
                    ),
                ]));

                // Show top cluster if significant
                if let Some(largest) = graph.entity_clusters.iter().max_by_key(|c| c.wallet_addresses.len()) {
                    if largest.wallet_addresses.len() >= 3 {
                        lines.push(Line::from(vec![
                            Span::styled("   ", Style::default()),
                            Span::styled(
                                format!("Largest: {} wallets, {:.0}% confidence",
                                    largest.wallet_addresses.len(), largest.confidence * 100.0),
                                Style::default().fg(Color::DarkGray)
                            ),
                        ]));
                    }
                }
            }

            // Investigation progress
            let progress_pct = if self.wallets_explored > 0 {
                (self.wallets_explored as f64 / (self.wallets_explored + 10) as f64 * 100.0).min(100.0)
            } else {
                0.0
            };
            let elapsed = self.start_time.elapsed().as_secs();
            lines.push(Line::from(vec![
                Span::styled(" ", Style::default()),
                Span::styled(
                    format!("📊 {:.0}% | {} wallets | {} nodes | {}s",
                        progress_pct, self.wallets_explored, graph.node_count(), elapsed),
                    Style::default().fg(Color::DarkGray)
                ),
            ]));
        } else {
            lines.push(Line::from(Span::styled("  Initializing graph analysis...", Style::default().fg(Color::DarkGray))));
        }

        // Apply scroll offset and handle overflow
        let available_height = inner_area.height as usize;
        let total_lines = lines.len();
        let max_scroll = total_lines.saturating_sub(available_height);
        let actual_scroll = self.ai_insights_scroll.min(max_scroll);

        // Skip lines based on scroll position
        let visible_lines: Vec<Line> = lines
            .into_iter()
            .skip(actual_scroll)
            .take(available_height)
            .collect();

        // Check if there's more content below
        let has_more = actual_scroll + visible_lines.len() < total_lines;

        let mut final_lines = visible_lines;
        if has_more {
            // Replace last line with "more content" indicator
            if !final_lines.is_empty() {
                final_lines.pop();
            }
            let remaining = total_lines - (actual_scroll + final_lines.len());
            final_lines.push(Line::from(vec![
                Span::styled("  ", Style::default()),
                Span::styled(
                    format!("⋮ {} more (j/k to scroll)", remaining),
                    Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
                ),
            ]));
        }

        let widget = Paragraph::new(final_lines).wrap(Wrap { trim: false });
        f.render_widget(widget, inner_area);
    }

    /// Export investigation to JSON file
    fn export_investigation(&self) -> Result<()> {
        use std::fs::File;
        use std::io::Write;

        let timestamp = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let filename = format!("investigation_{}_{}.json", self.target_wallet[..8].to_string(), timestamp);

        let (nodes, edges, risk_analysis, behavior) = self.wallet_graph.lock()
            .map(|mut g| {
                let risk = g.get_cached_risk_explanation();
                let behavior_type = g.classify_wallet_behavior(0);
                (g.node_count(), g.edge_count(), risk, behavior_type)
            })
            .unwrap_or((0, 0, crate::utils::tui::graph::RiskExplanation {
                score: 0.0,
                level: crate::utils::tui::graph::RiskLevel::Low,
                reasons: vec![],
                alerts: vec![],
            }, crate::utils::tui::graph::WalletBehaviorType::EOA));

        // Save to database
        if let Ok(mut db) = crate::utils::investigation_db::InvestigationDB::open() {
            let inv = crate::utils::investigation_db::Investigation {
                id: None,
                wallet_address: self.target_wallet.clone(),
                started_at: chrono::Utc::now() - chrono::Duration::seconds(self.start_time.elapsed().as_secs() as i64),
                completed_at: Some(chrono::Utc::now()),
                risk_score: risk_analysis.score,
                risk_level: format!("{:?}", risk_analysis.level),
                behavior_type: format!("{:?}", behavior),
                node_count: nodes,
                edge_count: edges,
                depth_reached: self.depth_reached,
                alerts: risk_analysis.alerts.clone(),
                reasons: risk_analysis.reasons.clone(),
                notes: Some(format!("API Calls: {}, SOL In: {:.2}, SOL Out: {:.2}",
                    self.api_calls, self.total_sol_in, self.total_sol_out)),
            };
            let _ = db.save_investigation(&inv); // Ignore errors for now
        }

        let export_data = serde_json::json!({
            "wallet": self.target_wallet,
            "timestamp": timestamp.to_string(),
            "duration_secs": self.start_time.elapsed().as_secs(),
            "risk_analysis": {
                "score": risk_analysis.score,
                "level": format!("{:?}", risk_analysis.level),
                "alerts": risk_analysis.alerts,
                "reasons": risk_analysis.reasons,
            },
            "behavior_classification": format!("{:?}", behavior),
            "stats": {
                "wallets_discovered": nodes,
                "transfers_found": edges,
                "depth_reached": self.depth_reached,
                "total_sol_in": self.total_sol_in,
                "total_sol_out": self.total_sol_out,
                "api_calls": self.api_calls,
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
        // WALLET-SPECIFIC ANALYTICS (not global network stats)
        let token_vols = self.token_volumes.lock().unwrap();
        let transfer_evts = self.transfer_events.lock().unwrap();

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

        // TARGET WALLET
        let truncated_wallet = if self.target_wallet.len() > 12 {
            format!("{}...{}", &self.target_wallet[..6], &self.target_wallet[self.target_wallet.len()-6..])
        } else {
            self.target_wallet.clone()
        };
        let wallet_text = vec![
            Line::from(Span::styled(" 🎯 TARGET WALLET ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" Addr: ", Style::default().fg(Color::DarkGray)),
                Span::styled(truncated_wallet, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" Nodes: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", self.wallets_explored), Style::default().fg(Color::Yellow)),
            ]),
        ];
        let wallet_widget = Paragraph::new(wallet_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Cyan)));
        f.render_widget(wallet_widget, chunks[0]);

        // TOKEN HOLDINGS (top 2 tokens)
        let top_tokens: Vec<_> = token_vols.iter().take(2).collect();
        let mut token_lines = vec![
            Line::from(Span::styled(" 💎 TOP TOKENS ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))),
        ];
        if top_tokens.is_empty() {
            token_lines.push(Line::from(Span::styled(" No data", Style::default().fg(Color::DarkGray))));
        } else {
            for token in top_tokens {
                token_lines.push(Line::from(vec![
                    Span::styled(format!(" {}: ", token.symbol), Style::default().fg(Color::White)),
                    Span::styled(format!("{:.2}", token.amount), Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
                ]));
            }
        }
        let token_widget = Paragraph::new(token_lines)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Yellow)));
        f.render_widget(token_widget, chunks[1]);

        // TRANSFER ACTIVITY (in/out counts)
        let inflow_count = transfer_evts.iter().filter(|e| e.direction == "IN").count();
        let outflow_count = transfer_evts.iter().filter(|e| e.direction == "OUT").count();
        let total_transfers = inflow_count + outflow_count;
        let activity_text = vec![
            Line::from(Span::styled(" 📊 TRANSFERS ", Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD))),
            Line::from(vec![
                Span::styled(" Total: ", Style::default().fg(Color::DarkGray)),
                Span::styled(format!("{}", total_transfers), Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
            ]),
            Line::from(vec![
                Span::styled(" In: ", Style::default().fg(Color::Green)),
                Span::styled(format!("{}", inflow_count), Style::default().fg(Color::Green)),
                Span::styled(" Out: ", Style::default().fg(Color::Red)),
                Span::styled(format!("{}", outflow_count), Style::default().fg(Color::Red)),
            ]),
        ];
        let activity_widget = Paragraph::new(activity_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Magenta)));
        f.render_widget(activity_widget, chunks[2]);

        // SOL FLOW (in/out balance)
        let net_flow = self.total_sol_in - self.total_sol_out;
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
                Span::styled(format!("{:.4}", self.total_sol_in), Style::default().fg(Color::Green)),
                Span::styled(" Out: ", Style::default().fg(Color::Red)),
                Span::styled(format!("{:.4}", self.total_sol_out), Style::default().fg(Color::Red)),
            ]),
        ];
        let flow_widget = Paragraph::new(flow_text)
            .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Green)));
        f.render_widget(flow_widget, chunks[3]);
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

    /// Render Federation Network Dashboard - Real-time peer visualization
    fn render_federation_dashboard(&mut self, f: &mut Frame, area: Rect) {
        let fed_state = self.federation_state.lock().unwrap().clone();

        // Main layout: [Header][Network Graph | Peers][Sessions][Status]
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),   // Header
                Constraint::Length(14),  // Network topology + peers
                Constraint::Min(8),      // Sessions list
                Constraint::Length(2),   // Status bar
            ])
            .split(area);

        // === HEADER ===
        let header_text = vec![
            Line::from(vec![
                Span::styled(" 🌐 FEDERATION NETWORK DASHBOARD ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("  ", Style::default()),
                Span::styled(format!("Node: {}", fed_state.node_id), Style::default().fg(Color::Yellow)),
            ]),
        ];
        let header = Paragraph::new(header_text)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Cyan))
                .title(Span::styled(" Federation ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))));
        f.render_widget(header, main_chunks[0]);

        // === NETWORK TOPOLOGY + PEERS (side by side) ===
        let network_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(50),  // Network topology graph
                Constraint::Percentage(50),  // Peer list
            ])
            .split(main_chunks[1]);

        // Network Topology (ASCII art visualization)
        self.render_federation_network_graph(f, network_chunks[0], &fed_state);

        // Peer List
        self.render_federation_peer_list(f, network_chunks[1], &fed_state);

        // === SESSIONS LIST ===
        self.render_federation_sessions(f, main_chunks[2], &fed_state);

        // === STATUS BAR ===
        let refresh_ago = fed_state.last_refresh
            .map(|t| format!("{}s ago", t.elapsed().as_secs()))
            .unwrap_or_else(|| "never".to_string());

        // Show different status bar based on input mode
        let status_text = if self.federation_input_active {
            Line::from(vec![
                Span::styled(" ADD PEER: ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled(&self.federation_input_buffer, Style::default().fg(Color::White)),
                Span::styled("▏", Style::default().fg(Color::White).add_modifier(Modifier::SLOW_BLINK)),
                Span::raw(" | "),
                Span::styled("Enter", Style::default().fg(Color::Green)),
                Span::raw(" confirm "),
                Span::styled("Esc", Style::default().fg(Color::Red)),
                Span::raw(" cancel"),
            ])
        } else {
            Line::from(vec![
                Span::styled(" [6] Federation ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::raw(" | "),
                Span::styled(format!("{} peers", fed_state.peers.len()), Style::default().fg(Color::Green)),
                Span::raw(" | "),
                Span::styled(format!("{} sessions", fed_state.sessions.len()), Style::default().fg(Color::Yellow)),
                Span::raw(" | "),
                Span::styled(format!("{}s", refresh_ago), Style::default().fg(Color::DarkGray)),
                Span::raw(" | "),
                Span::styled("r", Style::default().fg(Color::Green)),
                Span::raw(" refresh "),
                Span::styled("a", Style::default().fg(Color::Green)),
                Span::raw(" add "),
                Span::styled("d", Style::default().fg(Color::Red)),
                Span::raw(" del "),
                Span::styled("s/J", Style::default().fg(Color::Magenta)),
                Span::raw(" session"),
            ])
        };
        let status = Paragraph::new(status_text);
        f.render_widget(status, main_chunks[3]);
    }

    /// Render ASCII network topology graph
    fn render_federation_network_graph(&self, f: &mut Frame, area: Rect, state: &FederationDashboardState) {
        let mut lines: Vec<Line> = Vec::new();

        if state.peers.is_empty() {
            // No peers - show empty state with guidance
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                "       ┌─────────────┐",
                Style::default().fg(Color::DarkGray)
            )));
            lines.push(Line::from(vec![
                Span::styled("       │ ", Style::default().fg(Color::DarkGray)),
                Span::styled("YOU", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("       │", Style::default().fg(Color::DarkGray)),
            ]));
            lines.push(Line::from(vec![
                Span::styled("       │ ", Style::default().fg(Color::DarkGray)),
                Span::styled(&state.node_id[..8.min(state.node_id.len())], Style::default().fg(Color::Yellow)),
                Span::styled("  │", Style::default().fg(Color::DarkGray)),
            ]));
            lines.push(Line::from(Span::styled(
                "       └─────────────┘",
                Style::default().fg(Color::DarkGray)
            )));
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                "  No peers connected. Add with:",
                Style::default().fg(Color::Yellow).add_modifier(Modifier::ITALIC)
            )));
            lines.push(Line::from(Span::styled(
                "  osvm collab peers add <address>",
                Style::default().fg(Color::Green)
            )));
        } else {
            // Build network topology visualization
            lines.push(Line::from(""));

            // Center node (us)
            lines.push(Line::from(Span::styled(
                "           ┌───────────────┐",
                Style::default().fg(Color::Cyan)
            )));
            lines.push(Line::from(vec![
                Span::styled("           │ ", Style::default().fg(Color::Cyan)),
                Span::styled("★ YOU", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("        │", Style::default().fg(Color::Cyan)),
            ]));
            lines.push(Line::from(vec![
                Span::styled("           │ ", Style::default().fg(Color::Cyan)),
                Span::styled(&state.node_id[..10.min(state.node_id.len())], Style::default().fg(Color::Yellow)),
                Span::styled("    │", Style::default().fg(Color::Cyan)),
            ]));
            lines.push(Line::from(Span::styled(
                "           └───────┬───────┘",
                Style::default().fg(Color::Cyan)
            )));

            // Connection lines to peers
            let peer_count = state.peers.len();
            if peer_count > 0 {
                // Draw connection hub
                let conn_line = match peer_count {
                    1 => "                   │",
                    2 => "            ┌──────┴──────┐",
                    3 => "       ┌────┼─────────────┼────┐",
                    _ => "  ┌────┼────┼─────────────┼────┼────┐",
                };
                lines.push(Line::from(Span::styled(conn_line, Style::default().fg(Color::Green))));

                // Draw peer boxes (max 4 shown)
                let shown_peers: Vec<_> = state.peers.iter().take(4).collect();
                let mut peer_line1 = String::new();
                let mut peer_line2 = String::new();
                let mut peer_line3 = String::new();

                for peer in &shown_peers {
                    let status_icon = match peer.status {
                        PeerStatus::Online => "●",
                        PeerStatus::Offline => "○",
                        PeerStatus::Connecting => "◐",
                        PeerStatus::Unknown => "?",
                    };

                    peer_line1.push_str(&format!(" [{} ", status_icon));
                    peer_line2.push_str(&format!("  {} ", &peer.node_id[..6.min(peer.node_id.len())]));
                    peer_line3.push_str(&format!("  {}] ", peer.latency_ms.map(|l| format!("{}ms", l)).unwrap_or_else(|| "?".to_string())));
                }

                lines.push(Line::from(Span::styled(peer_line1.clone(), Style::default().fg(Color::White))));
                lines.push(Line::from(Span::styled(peer_line2.clone(), Style::default().fg(Color::Yellow))));
                lines.push(Line::from(Span::styled(peer_line3.clone(), Style::default().fg(Color::DarkGray))));

                if state.peers.len() > 4 {
                    lines.push(Line::from(Span::styled(
                        format!("  ... +{} more peers", state.peers.len() - 4),
                        Style::default().fg(Color::DarkGray).add_modifier(Modifier::ITALIC)
                    )));
                }
            }
        }

        let graph = Paragraph::new(lines)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Green))
                .title(Span::styled(" Network Topology ", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD))));
        f.render_widget(graph, area);
    }

    /// Render peer list with status and metrics
    fn render_federation_peer_list(&self, f: &mut Frame, area: Rect, state: &FederationDashboardState) {
        let mut lines: Vec<Line> = Vec::new();

        if state.peers.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                " No federation peers configured",
                Style::default().fg(Color::Yellow).add_modifier(Modifier::ITALIC)
            )));
        } else {
            // Column headers
            lines.push(Line::from(vec![
                Span::styled(" Status ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Node ID      ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Latency ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Sessions", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            ]));
            lines.push(Line::from(Span::styled(
                " ───────┼──────────────┼─────────┼─────────",
                Style::default().fg(Color::DarkGray)
            )));

            for (idx, peer) in state.peers.iter().enumerate() {
                let is_selected = self.federation_selected_peer == Some(idx);
                let (status_icon, status_color) = match peer.status {
                    PeerStatus::Online => ("● ONLINE ", Color::Green),
                    PeerStatus::Offline => ("○ OFFLINE", Color::Red),
                    PeerStatus::Connecting => ("◐ CONN.  ", Color::Yellow),
                    PeerStatus::Unknown => ("? UNKNOWN", Color::DarkGray),
                };

                let latency = peer.latency_ms
                    .map(|l| format!("{:>5}ms", l))
                    .unwrap_or_else(|| "   N/A".to_string());

                // Highlight selected peer
                let row_style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };
                let select_indicator = if is_selected { "▶" } else { " " };

                lines.push(Line::from(vec![
                    Span::styled(select_indicator, row_style.fg(Color::Cyan)),
                    Span::styled(format!("{} ", status_icon), row_style.fg(status_color)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{:<12} ", &peer.node_id[..12.min(peer.node_id.len())]), row_style.fg(Color::Yellow)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{} ", latency), row_style.fg(Color::White)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{:>4}", peer.sessions_hosted), row_style.fg(Color::Cyan)),
                ]));
            }
        }

        let peers_widget = Paragraph::new(lines)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Yellow))
                .title(Span::styled(" Peers ", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))));
        f.render_widget(peers_widget, area);
    }

    /// Render federation sessions list
    fn render_federation_sessions(&self, f: &mut Frame, area: Rect, state: &FederationDashboardState) {
        let mut lines: Vec<Line> = Vec::new();

        if state.sessions.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from(Span::styled(
                " No active sessions discovered",
                Style::default().fg(Color::Yellow).add_modifier(Modifier::ITALIC)
            )));
            lines.push(Line::from(""));
            lines.push(Line::from(vec![
                Span::styled(" Start a session: ", Style::default().fg(Color::White)),
                Span::styled("osvm collab start --name \"Investigation\"", Style::default().fg(Color::Green)),
            ]));
            lines.push(Line::from(vec![
                Span::styled(" Discover sessions: ", Style::default().fg(Color::White)),
                Span::styled("osvm collab discover", Style::default().fg(Color::Green)),
            ]));
        } else {
            // Column headers
            lines.push(Line::from(vec![
                Span::styled(" Name                    ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Host         ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Users ", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
                Span::styled("│ ", Style::default().fg(Color::DarkGray)),
                Span::styled("Status", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            ]));
            lines.push(Line::from(Span::styled(
                " ────────────────────────┼──────────────┼───────┼────────",
                Style::default().fg(Color::DarkGray)
            )));

            for (idx, session) in state.sessions.iter().enumerate() {
                let is_selected = self.federation_selected_session == Some(idx);
                let status_color = match session.status.as_str() {
                    "Active" => Color::Green,
                    "Full" => Color::Yellow,
                    "Paused" => Color::Gray,
                    _ => Color::Red,
                };

                let name = if session.name.len() > 22 {
                    format!("{}...", &session.name[..19])
                } else {
                    format!("{:<22}", session.name)
                };

                // Highlight selected session
                let row_style = if is_selected {
                    Style::default().bg(Color::DarkGray)
                } else {
                    Style::default()
                };
                let select_indicator = if is_selected { "▶" } else { " " };

                lines.push(Line::from(vec![
                    Span::styled(select_indicator, row_style.fg(Color::Cyan)),
                    Span::styled(format!("{} ", name), row_style.fg(Color::White)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{:<12} ", &session.host_node_id[..12.min(session.host_node_id.len())]), row_style.fg(Color::Yellow)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{:>5} ", session.participant_count), row_style.fg(Color::Cyan)),
                    Span::styled("│ ", row_style.fg(Color::DarkGray)),
                    Span::styled(format!("{:<6}", session.status), row_style.fg(status_color)),
                ]));
            }
        }

        let sessions_widget = Paragraph::new(lines)
            .block(Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Magenta))
                .title(Span::styled(" Federated Sessions ", Style::default().fg(Color::Magenta).add_modifier(Modifier::BOLD))))
            .scroll((self.federation_scroll as u16, 0));
        f.render_widget(sessions_widget, area);
    }
}
