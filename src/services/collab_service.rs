//! Collaborative Investigation Service
//!
//! CLI command handlers for collaborative investigation sessions.
//!
//! # Commands
//!
//! ```bash
//! osvm collab start --wallet <WALLET> --name "Investigation Name"
//! osvm collab join <INVITE_CODE>
//! osvm collab list
//! osvm collab leave
//! osvm collab annotate <TARGET> <TEXT>
//! osvm collab server --port 8080
//! ```

use crate::utils::collab::{
    presence::UserPresence, session::ClientType, session::ParticipantColor, Annotation,
    AnnotationSeverity, AnnotationType, CollabError, CollabRegistry, CollabWebSocketServer,
    CollaborativeSession, SessionConfig, SessionRole,
};

use colored::Colorize;
use std::process::Command;
use std::sync::Arc;
use tokio::sync::OnceCell;

/// Get the current username
fn get_username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("USERNAME"))
        .unwrap_or_else(|_| "User".to_string())
}

/// Global collaborative registry
static COLLAB_REGISTRY: OnceCell<Arc<CollabRegistry>> = OnceCell::const_new();

/// Get the global collab registry
pub async fn get_registry() -> Arc<CollabRegistry> {
    COLLAB_REGISTRY
        .get_or_init(|| async { Arc::new(CollabRegistry::new()) })
        .await
        .clone()
}

/// Start a new collaborative session
pub async fn start_session(
    name: Option<String>,
    wallet: Option<String>,
    max_participants: Option<usize>,
    password: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;

    let config = SessionConfig {
        name: name.unwrap_or_else(|| "Investigation".to_string()),
        target_wallet: wallet,
        max_participants: max_participants.unwrap_or(10),
        password,
        ..Default::default()
    };

    println!("{}", "Creating collaborative session...".cyan());

    let session = registry.create_session(config).await?;
    let info = session.connection_info();

    println!();
    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════╗".green()
    );
    println!(
        "{}",
        "║           COLLABORATIVE INVESTIGATION STARTED             ║".green()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!("║ {:<58}║", format!("Session: {}", info.name).white());
    println!(
        "║ {:<58}║",
        format!("ID: {}", &info.session_id[..8]).white()
    );
    println!(
        "║ {:<58}║",
        format!("Invite Code: {}", info.invite_code).yellow().bold()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "{}",
        "║ Share this command with collaborators:                    ║".cyan()
    );
    println!(
        "║ {:<58}║",
        format!("  osvm collab join {}", info.invite_code)
            .white()
            .bold()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "{}",
        "║ Or attach via tmux:                                       ║".cyan()
    );
    println!(
        "║ {:<58}║",
        format!("  tmux attach -t {}", info.tmux_session).white()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════╝".green()
    );
    println!();

    if let Some(wallet) = &info.target_wallet {
        println!("{} {}", "Target wallet:".cyan(), wallet.yellow());
    }

    // Attach to the session
    println!("{}", "Attaching to session (Ctrl+B D to detach)...".cyan());

    let status = Command::new("tmux")
        .args(["attach-session", "-t", &info.tmux_session])
        .status()?;

    if !status.success() {
        println!("{}", "Session ended or attachment failed".yellow());
    }

    Ok(())
}

/// Join an existing collaborative session
pub async fn join_session(
    invite_code: &str,
    name: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;

    // Look up the session by invite code
    let sessions = registry.list_sessions().await;

    // Try to find session locally first
    for (id, session_name) in &sessions {
        if let Some(session) = registry.get_session(id).await {
            if session.invite_code() == invite_code {
                let display_name = name.unwrap_or_else(get_username);

                println!("{}", format!("Joining session: {}", session_name).green());

                // Add ourselves as a participant
                let participant = session
                    .add_participant(display_name, SessionRole::Editor, ClientType::Tmux)
                    .await?;

                println!(
                    "{}",
                    format!(
                        "Connected as: {} ({})",
                        participant.name,
                        &participant.id[..8]
                    )
                    .cyan()
                );

                // Attach to tmux session
                let status = Command::new("tmux")
                    .args(["attach-session", "-t", session.tmux_session()])
                    .status()?;

                if !status.success() {
                    println!("{}", "Session ended or attachment failed".yellow());
                }

                return Ok(());
            }
        }
    }

    // If not found locally, try direct tmux session name
    let tmux_session = format!("osvm_collab_{}", invite_code.to_lowercase());

    println!(
        "{}",
        format!("Attempting to join tmux session: {}", tmux_session).cyan()
    );

    let status = Command::new("tmux")
        .args(["attach-session", "-t", &tmux_session])
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("{}", "Session ended".yellow());
        }
        _ => {
            eprintln!("{}", "Could not find session with that invite code.".red());
            eprintln!("{}", "Make sure:".yellow());
            eprintln!("  1. The invite code is correct");
            eprintln!("  2. The session host is still running");
            eprintln!("  3. You're on the same network as the host");
            eprintln!();
            eprintln!(
                "{}",
                "For remote sessions, ask the host for the WebSocket URL.".cyan()
            );
        }
    }

    Ok(())
}

/// List active collaborative sessions
pub async fn list_sessions() -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;
    let sessions = registry.list_sessions().await;

    if sessions.is_empty() {
        println!(
            "{}",
            "No active collaborative sessions found locally.".yellow()
        );
        println!();
        println!("{}", "You can also check for tmux sessions:".cyan());
        println!("  tmux list-sessions | grep osvm_collab");
        return Ok(());
    }

    println!("{}", "Active Collaborative Sessions:".green().bold());
    println!("{}", "═".repeat(60).green());

    for (id, name) in sessions {
        if let Some(session) = registry.get_session(&id).await {
            let info = session.connection_info();
            let count = session.participant_count().await;

            println!();
            println!("{}: {}", "Session".cyan(), name.white().bold());
            println!(
                "  {}: {}",
                "Invite Code".yellow(),
                info.invite_code.white().bold()
            );
            println!("  {}: {}", "Participants".cyan(), count);
            if let Some(wallet) = &info.target_wallet {
                println!("  {}: {}", "Target Wallet".cyan(), wallet);
            }
            println!(
                "  {}: tmux attach -t {}",
                "Attach".cyan(),
                info.tmux_session
            );
        }
    }

    println!();
    Ok(())
}

/// Add an annotation to an entity
pub async fn add_annotation(
    session_id: Option<String>,
    target: &str,
    text: &str,
    severity: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;

    // Get the session
    let session = if let Some(id) = session_id {
        registry.get_session(&id).await
    } else {
        // Get the first active session
        let sessions = registry.list_sessions().await;
        if let Some((id, _)) = sessions.first() {
            registry.get_session(id).await
        } else {
            None
        }
    };

    let session = session.ok_or("No active session found. Start or join a session first.")?;

    // Determine annotation type from target
    let annotation_type = if target.len() == 44 {
        // Likely a wallet or mint address
        AnnotationType::Wallet(target.to_string())
    } else if target.len() > 80 {
        // Likely a transaction signature
        AnnotationType::Transaction(target.to_string())
    } else {
        // General note
        AnnotationType::Note
    };

    // Parse severity
    let severity = match severity.as_deref() {
        Some("critical") | Some("crit") => AnnotationSeverity::Critical,
        Some("warning") | Some("warn") => AnnotationSeverity::Warning,
        Some("important") | Some("imp") => AnnotationSeverity::Important,
        Some("question") | Some("?") => AnnotationSeverity::Question,
        _ => AnnotationSeverity::Info,
    };

    let username = get_username();
    let annotation = Annotation::new(
        annotation_type.clone(),
        text.to_string(),
        severity,
        username.clone(),
        username.clone(),
        ParticipantColor::from_index(0),
    );

    let id = session.annotations().add(annotation).await;

    println!(
        "{} {} {}",
        severity.emoji(),
        "Annotation added:".green(),
        id[..8].to_string().cyan()
    );
    println!("  {}: {}", "Target".cyan(), annotation_type);
    println!("  {}: {}", "Text".cyan(), text);

    Ok(())
}

/// List annotations in a session
pub async fn list_annotations(
    session_id: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;

    let session = if let Some(id) = session_id {
        registry.get_session(&id).await
    } else {
        let sessions = registry.list_sessions().await;
        if let Some((id, _)) = sessions.first() {
            registry.get_session(id).await
        } else {
            None
        }
    };

    let session = session.ok_or("No active session found.")?;

    let annotations = session.annotations().get_all().await;

    if annotations.is_empty() {
        println!("{}", "No annotations in this session.".yellow());
        return Ok(());
    }

    println!("{}", "Session Annotations:".green().bold());
    println!("{}", "═".repeat(60).green());

    for annotation in annotations {
        println!();
        println!(
            "{} {} [{}]",
            annotation.severity.emoji(),
            annotation.text.white(),
            annotation.author_name.cyan()
        );
        println!("  {}: {}", "Target".bright_black(), annotation.target);
        if annotation.pinned {
            println!("  {} {}", "📌".yellow(), "Pinned".yellow());
        }
    }

    println!();
    Ok(())
}

/// Start the collaborative WebSocket server
pub async fn start_server(
    port: u16,
    host: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    use axum::Router;
    use std::net::SocketAddr;

    let host = host.unwrap_or_else(|| "0.0.0.0".to_string());
    let addr: SocketAddr = format!("{}:{}", host, port).parse()?;

    let registry = get_registry().await;
    let ws_server = Arc::new(CollabWebSocketServer::new());

    // Register any existing sessions
    for (id, _) in registry.list_sessions().await {
        if let Some(session) = registry.get_session(&id).await {
            ws_server.register_session(session).await;
        }
    }

    let router = ws_server.router();

    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════╗".green()
    );
    println!(
        "{}",
        "║          COLLABORATIVE SERVER STARTING                    ║".green()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "║ {:<58}║",
        format!("Listening on: http://{}:{}", host, port).white()
    );
    println!(
        "║ {:<58}║",
        format!("WebSocket: ws://{}:{}/collab/<session_id>", host, port).cyan()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "{}",
        "║ Endpoints:                                                ║".cyan()
    );
    println!(
        "║ {:<58}║",
        "  GET  /collab/join/<invite>  - Join page".white()
    );
    println!(
        "║ {:<58}║",
        "  GET  /collab/<session>      - WebSocket".white()
    );
    println!(
        "║ {:<58}║",
        "  GET  /api/collab/sessions   - List sessions".white()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════╝".green()
    );

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, router).await?;

    Ok(())
}

/// Handle collab subcommands from main.rs
pub async fn handle_collab_command(
    subcommand: &str,
    args: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    match subcommand {
        "start" => {
            let name = args.get_one::<String>("name").cloned();
            let wallet = args.get_one::<String>("wallet").cloned();
            let max = args.get_one::<String>("max").and_then(|s| s.parse().ok());
            let password = args.get_one::<String>("password").cloned();
            start_session(name, wallet, max, password).await?;
        }
        "join" => {
            let code = args
                .get_one::<String>("code")
                .ok_or("Invite code required")?;
            let name = args.get_one::<String>("name").cloned();
            join_session(code, name).await?;
        }
        "list" | "ls" => {
            list_sessions().await?;
        }
        "annotate" | "note" => {
            let target = args.get_one::<String>("target").ok_or("Target required")?;
            let text = args
                .get_one::<String>("text")
                .ok_or("Annotation text required")?;
            let severity = args.get_one::<String>("severity").cloned();
            add_annotation(None, target, text, severity).await?;
        }
        "annotations" => {
            list_annotations(None).await?;
        }
        "server" => {
            let port = args
                .get_one::<String>("port")
                .and_then(|s| s.parse().ok())
                .unwrap_or(8080);
            let host = args.get_one::<String>("host").cloned();
            start_server(port, host).await?;
        }
        _ => {
            eprintln!("{}", "Unknown collab subcommand".red());
            eprintln!(
                "{}",
                "Available: start, join, list, annotate, annotations, server".yellow()
            );
        }
    }

    Ok(())
}

// ============================================================================
// FEDERATION FUNCTIONS
// ============================================================================

use crate::utils::collab::{
    CollabFederationManager, FederatedSessionAnnouncement, FederatedSessionStatus,
};
use tokio::sync::OnceCell as TokioOnceCell;

/// Global federation manager
static FEDERATION_MANAGER: TokioOnceCell<Arc<CollabFederationManager>> = TokioOnceCell::const_new();

/// Get or initialize the federation manager
pub async fn get_federation_manager() -> Arc<CollabFederationManager> {
    FEDERATION_MANAGER
        .get_or_init(|| async {
            let node_id = format!("!{:08x}", std::process::id());
            let public_address = "http://localhost:8080".to_string(); // Default
            Arc::new(CollabFederationManager::new(&node_id, &public_address))
        })
        .await
        .clone()
}

/// Add a federation peer
pub async fn add_federation_peer(address: &str) -> Result<(), Box<dyn std::error::Error>> {
    let manager = get_federation_manager().await;

    // Generate node ID from address
    let node_id = format!("!{:08x}", crc32fast::hash(address.as_bytes()));

    manager.add_peer(&node_id, address).await?;

    println!(
        "{} {} {}",
        "✓".green(),
        "Added federation peer:".cyan(),
        address.white()
    );
    println!("  {}: {}", "Node ID".bright_black(), node_id);
    println!(
        "  {}",
        "(Saved to ~/.osvm/collab/federation.json)".bright_black()
    );

    Ok(())
}

/// Remove a federation peer
pub async fn remove_federation_peer(address: &str) -> Result<(), Box<dyn std::error::Error>> {
    let manager = get_federation_manager().await;

    let node_id = format!("!{:08x}", crc32fast::hash(address.as_bytes()));
    manager.remove_peer(&node_id).await?;

    println!(
        "{} {} {}",
        "✓".green(),
        "Removed federation peer:".cyan(),
        address.white()
    );
    println!(
        "  {}",
        "(Saved to ~/.osvm/collab/federation.json)".bright_black()
    );

    Ok(())
}

/// List federation peers
pub async fn list_federation_peers() -> Result<(), Box<dyn std::error::Error>> {
    let manager = get_federation_manager().await;
    let peers = manager.list_peers().await;

    if peers.is_empty() {
        println!("{}", "No federation peers configured.".yellow());
        println!();
        println!("{}", "Add peers with:".cyan());
        println!("  osvm collab peers add http://peer-address:8080");
        return Ok(());
    }

    println!("{}", "Federation Peers:".green().bold());
    println!("{}", "═".repeat(60).green());

    for (node_id, address) in peers {
        println!();
        println!("  {}: {}", "Node".cyan(), node_id);
        println!("  {}: {}", "Address".bright_black(), address);
    }

    println!();
    Ok(())
}

/// Discover sessions from federated peers
pub async fn discover_sessions() -> Result<(), Box<dyn std::error::Error>> {
    let manager = get_federation_manager().await;

    println!("{}", "Discovering sessions from federated peers...".cyan());

    let sessions = manager.discover_sessions().await?;

    if sessions.is_empty() {
        println!("{}", "No sessions found on federated peers.".yellow());
        println!();
        println!("{}", "Make sure:".cyan());
        println!("  1. Federation peers are configured (osvm collab peers list)");
        println!("  2. Peers have active sessions");
        println!("  3. Peers have their collab server running");
        return Ok(());
    }

    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════╗".green()
    );
    println!(
        "{}",
        "║          FEDERATED SESSIONS DISCOVERED                    ║".green()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );

    for session in sessions {
        let status_icon = match session.status {
            FederatedSessionStatus::Active => "🟢",
            FederatedSessionStatus::Full => "🟡",
            FederatedSessionStatus::Paused => "⏸️",
            FederatedSessionStatus::Ended => "🔴",
        };

        println!("║ {} {:<55}║", status_icon, session.name.white().bold());
        println!(
            "║   {}: {:<49}║",
            "Invite".cyan(),
            session.invite_code.yellow()
        );
        println!(
            "║   {}: {:<49}║",
            "Host".bright_black(),
            &session.host_node_id[..8]
        );
        println!(
            "║   {}: {}/{:<44}║",
            "Participants".bright_black(),
            session.participant_count,
            if session.max_participants == 0 {
                "∞".to_string()
            } else {
                session.max_participants.to_string()
            }
        );
        if let Some(wallet) = &session.target_wallet {
            println!(
                "║   {}: {}...{:<38}║",
                "Wallet".bright_black(),
                &wallet[..4],
                &wallet[wallet.len() - 4..]
            );
        }
        println!("║ {:<58}║", "─".repeat(56));
    }

    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════╝".green()
    );
    println!();
    println!("{}", "Join a session with:".cyan());
    println!("  osvm collab join <INVITE_CODE>");

    Ok(())
}

/// Publish a local session to the federation
pub async fn publish_session_to_federation(
    session_id: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let registry = get_registry().await;
    let federation = get_federation_manager().await;

    // Find the session
    let session = if let Some(id) = session_id {
        registry.get_session(&id).await
    } else {
        // Get the first active session
        let sessions = registry.list_sessions().await;
        if let Some((id, _)) = sessions.first() {
            registry.get_session(id).await
        } else {
            None
        }
    };

    let session = session.ok_or("No session to publish. Start a session first.")?;

    federation.publish_session(session.clone()).await?;

    let info = session.connection_info();
    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════╗".green()
    );
    println!(
        "{}",
        "║          SESSION PUBLISHED TO FEDERATION                  ║".green()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!("║ {:<58}║", format!("Session: {}", info.name).white());
    println!(
        "║ {:<58}║",
        format!("Invite: {}", info.invite_code).yellow().bold()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "{}",
        "║ Investigators on federated peers can now join with:       ║".cyan()
    );
    println!(
        "║ {:<58}║",
        format!("  osvm collab join {}", info.invite_code)
            .white()
            .bold()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════╝".green()
    );

    Ok(())
}

/// Show federation status
pub async fn show_federation_status() -> Result<(), Box<dyn std::error::Error>> {
    let manager = get_federation_manager().await;
    let peers = manager.list_peers().await;
    let sessions = manager.list_all_sessions().await;

    println!(
        "{}",
        "╔═══════════════════════════════════════════════════════════╗".green()
    );
    println!(
        "{}",
        "║              FEDERATION STATUS                            ║".green()
    );
    println!(
        "{}",
        "╠═══════════════════════════════════════════════════════════╣".green()
    );
    println!(
        "║ {:<58}║",
        format!("Node ID: {}", manager.node_id()).white()
    );
    println!(
        "║ {:<58}║",
        format!("Connected Peers: {}", peers.len()).cyan()
    );
    println!(
        "║ {:<58}║",
        format!("Known Sessions: {}", sessions.len()).cyan()
    );
    println!(
        "║ {:<58}║",
        format!(
            "Active Sessions: {}",
            sessions
                .iter()
                .filter(|s| s.status == FederatedSessionStatus::Active)
                .count()
        )
        .green()
    );
    println!(
        "{}",
        "╚═══════════════════════════════════════════════════════════╝".green()
    );

    Ok(())
}

/// Print help for collab commands
pub fn print_help() {
    println!("{}", "Collaborative Investigation Commands".green().bold());
    println!("{}", "═".repeat(60).green());
    println!();
    println!("{}", "LOCAL SESSION MANAGEMENT:".yellow().bold());
    println!();
    println!("{}", "Start a collaborative session:".cyan());
    println!("  osvm collab start --name \"Hack Investigation\" --wallet <ADDR>");
    println!();
    println!("{}", "Join a session:".cyan());
    println!("  osvm collab join <INVITE_CODE>");
    println!();
    println!("{}", "List active sessions:".cyan());
    println!("  osvm collab list");
    println!();
    println!("{}", "Add an annotation:".cyan());
    println!("  osvm collab annotate <WALLET|TX> \"Note text\" --severity warning");
    println!();
    println!("{}", "Start WebSocket server (for browser clients):".cyan());
    println!("  osvm collab server --port 8080");
    println!();
    println!(
        "{}",
        "FEDERATION (Cross-Node Collaboration):".yellow().bold()
    );
    println!();
    println!("{}", "Add a federation peer:".cyan());
    println!("  osvm collab peers add http://peer-ip:8080");
    println!();
    println!("{}", "List federation peers:".cyan());
    println!("  osvm collab peers list");
    println!();
    println!("{}", "Discover remote sessions:".cyan());
    println!("  osvm collab discover");
    println!();
    println!("{}", "Publish session to federation:".cyan());
    println!("  osvm collab publish");
    println!();
    println!("{}", "Show federation status:".cyan());
    println!("  osvm collab status");
    println!();
    println!("{}", "Severity levels:".yellow());
    println!("  info, important, warning, critical, question");
}

// ============================================================================
// TUI FEDERATION DASHBOARD INTEGRATION
// ============================================================================

use crate::utils::tui::app::{
    FederationDashboardState, FederationPeerInfo, FederationSessionInfo, PeerStatus,
};

/// Load federation state for TUI dashboard
pub async fn load_federation_dashboard_state() -> FederationDashboardState {
    let manager = get_federation_manager().await;
    let peers = manager.list_peers().await;
    let sessions = manager.list_all_sessions().await;

    FederationDashboardState {
        node_id: manager.node_id().to_string(),
        peers: peers
            .into_iter()
            .map(|(node_id, address)| FederationPeerInfo {
                node_id,
                address,
                status: PeerStatus::Unknown,
                latency_ms: None,
                sessions_hosted: 0,
                last_seen: None,
            })
            .collect(),
        sessions: sessions
            .into_iter()
            .map(|s| FederationSessionInfo {
                session_id: s.session_id,
                name: s.name,
                host_node_id: s.host_node_id,
                participant_count: s.participant_count,
                status: format!("{:?}", s.status),
            })
            .collect(),
        last_refresh: Some(std::time::Instant::now()),
        total_annotations: 0,
        connection_graph: Vec::new(),
        live_annotations: Vec::new(),
    }
}

/// Perform health check on a single peer and return updated info
pub async fn check_peer_health(address: &str) -> (PeerStatus, Option<u64>) {
    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(5))
        .build()
        .unwrap_or_else(|_| reqwest::Client::new());

    let health_url = format!("{}/api/health", address);
    let start = std::time::Instant::now();

    match client.get(&health_url).send().await {
        Ok(response) => {
            let latency = start.elapsed().as_millis() as u64;
            if response.status().is_success() {
                (PeerStatus::Online, Some(latency))
            } else {
                (PeerStatus::Offline, Some(latency))
            }
        }
        Err(_) => {
            // Try a simpler connection test
            let connect_url = format!("{}/", address);
            match client.get(&connect_url).send().await {
                Ok(_) => (PeerStatus::Online, Some(start.elapsed().as_millis() as u64)),
                Err(_) => (PeerStatus::Offline, None),
            }
        }
    }
}

/// Refresh federation dashboard state with health checks
pub async fn refresh_federation_dashboard_state() -> FederationDashboardState {
    let manager = get_federation_manager().await;
    let peers = manager.list_peers().await;
    let sessions = manager.list_all_sessions().await;

    // Perform health checks on all peers concurrently
    let mut peer_infos = Vec::new();
    let mut health_futures = Vec::new();

    for (node_id, address) in &peers {
        health_futures.push(async move {
            let (status, latency) = check_peer_health(address).await;
            (node_id.clone(), address.clone(), status, latency)
        });
    }

    // Execute all health checks concurrently
    let results = futures::future::join_all(health_futures).await;

    for (node_id, address, status, latency) in results {
        // Count sessions hosted by this peer
        let hosted_count = sessions
            .iter()
            .filter(|s| s.host_node_id == node_id)
            .count();

        peer_infos.push(FederationPeerInfo {
            node_id,
            address,
            status,
            latency_ms: latency,
            sessions_hosted: hosted_count,
            last_seen: if status == PeerStatus::Online {
                Some(chrono::Utc::now().format("%H:%M:%S").to_string())
            } else {
                None
            },
        });
    }

    // Build connection graph
    let mut connection_graph = Vec::new();
    let my_node_id = manager.node_id().to_string();
    for peer in &peer_infos {
        if peer.status == PeerStatus::Online {
            connection_graph.push((my_node_id.clone(), peer.node_id.clone()));
        }
    }

    FederationDashboardState {
        node_id: my_node_id,
        peers: peer_infos,
        sessions: sessions
            .into_iter()
            .map(|s| FederationSessionInfo {
                session_id: s.session_id,
                name: s.name,
                host_node_id: s.host_node_id,
                participant_count: s.participant_count,
                status: format!("{:?}", s.status),
            })
            .collect(),
        last_refresh: Some(std::time::Instant::now()),
        total_annotations: 0,
        connection_graph,
        live_annotations: Vec::new(),
    }
}

/// Add a peer via TUI (returns updated state)
pub async fn tui_add_peer(address: &str) -> Result<FederationDashboardState, String> {
    let manager = get_federation_manager().await;
    let node_id = format!("!{:08x}", crc32fast::hash(address.as_bytes()));

    manager
        .add_peer(&node_id, address)
        .await
        .map_err(|e| format!("Failed to add peer: {}", e))?;

    Ok(refresh_federation_dashboard_state().await)
}

/// Remove a peer via TUI (returns updated state)
pub async fn tui_remove_peer(node_id: &str) -> Result<FederationDashboardState, String> {
    let manager = get_federation_manager().await;

    manager
        .remove_peer(node_id)
        .await
        .map_err(|e| format!("Failed to remove peer: {}", e))?;

    Ok(refresh_federation_dashboard_state().await)
}
