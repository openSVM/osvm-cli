//! BBS (Bulletin Board System) command handler
//!
//! Handles CLI commands for the Meshtastic BBS system, providing
//! agent-human communication over off-grid radio networks.

use anyhow::{anyhow, Result};
use clap::ArgMatches;
use colored::*;
use solana_sdk::signature::Signer;  // For keypair.pubkey()
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::utils::bbs::{db, meshtastic, http_server, federation, registry, num_id_to_hex};

/// Global radio connection (lazy singleton)
static RADIO: once_cell::sync::Lazy<Arc<Mutex<Option<meshtastic::MeshtasticRadio>>>> =
    once_cell::sync::Lazy::new(|| Arc::new(Mutex::new(None)));

/// Helper to convert BBS db errors to anyhow errors
fn db_err<T>(result: db::Result<T>) -> Result<T> {
    result.map_err(|e| anyhow!("{}", e))
}

/// Handle BBS commands
pub async fn handle_bbs_command(matches: &ArgMatches) -> Result<()> {
    // Run migrations on any BBS command to ensure schema is up-to-date
    if let Ok(mut conn) = db::establish_connection() {
        let _ = db::run_migrations(&mut conn);
    }

    match matches.subcommand() {
        Some(("init", sub_m)) => handle_init(sub_m).await,
        Some(("boards", sub_m)) => handle_boards(sub_m).await,
        Some(("post", sub_m)) => handle_post(sub_m).await,
        Some(("read", sub_m)) => handle_read(sub_m).await,
        Some(("reply", sub_m)) => handle_reply(sub_m).await,
        Some(("agent", sub_m)) => handle_agent(sub_m).await,
        Some(("radio", sub_m)) => handle_radio(sub_m).await,
        Some(("peers", sub_m)) => handle_peers(sub_m).await,
        Some(("registry", sub_m)) => handle_registry(sub_m).await,
        Some(("server", sub_m)) => handle_server(sub_m).await,
        Some(("interactive", sub_m)) => handle_interactive(sub_m).await,
        Some(("tui", sub_m)) => handle_tui(sub_m).await,
        Some(("threads", sub_m)) => handle_threads(sub_m).await,
        Some(("stats", sub_m)) => handle_stats(sub_m).await,
        Some(("analytics", sub_m)) => handle_analytics(sub_m).await,
        Some(("mesh", sub_m)) => handle_mesh(sub_m).await,
        _ => {
            println!("{}", "Use 'osvm bbs --help' for available commands".yellow());
            Ok(())
        }
    }
}

/// Initialize BBS database
async fn handle_init(matches: &ArgMatches) -> Result<()> {
    let reset = matches.get_flag("reset");
    let _custom_path = matches.get_one::<String>("path");

    println!("{}", "Initializing BBS database...".cyan());

    // Get database path
    let db_path = crate::utils::bbs::db_path();

    if reset && db_path.exists() {
        println!("{}", "Resetting database (removing existing data)...".yellow());
        std::fs::remove_file(&db_path)?;
    }

    // Ensure parent directory exists
    if let Some(parent) = db_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    // Establish connection and initialize schema
    let mut conn = db_err(db::establish_connection())?;
    db_err(db::initialize_database(&mut conn))?;

    // Run migrations
    db_err(db::run_migrations(&mut conn))?;

    // Create default boards if they don't exist
    let boards = db::boards::list(&mut conn).unwrap_or_default();

    if boards.is_empty() {
        println!("{}", "Creating default boards...".cyan());

        let default_boards = [
            ("GENERAL", "General discussion and announcements"),
            ("ALERTS", "System alerts and notifications"),
            ("TRADES", "Trading signals and opportunities"),
            ("RESEARCH", "Blockchain research findings"),
            ("HELP", "Help requests and support"),
        ];

        for (name, desc) in default_boards.iter() {
            match db::boards::create(&mut conn, name, desc) {
                Ok(board) => println!("  {} Created board: {}", "✓".green(), board.name),
                Err(e) => println!("  {} Board {}: {}", "!".yellow(), name, e),
            }
        }
    }

    println!("\n{}", "BBS initialized successfully!".green().bold());
    println!("  Database: {}", db_path.display());
    println!("  Boards: {}", db::boards::count(&mut conn));
    println!("  Features: reply threading, moderator support, radio integration");

    Ok(())
}

/// Handle boards subcommands
async fn handle_boards(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    match matches.subcommand() {
        Some(("list", sub_m)) => {
            let json = sub_m.get_flag("json");
            let boards = db_err(db::boards::list(&mut conn))?;

            if json {
                println!("{}", serde_json::to_string_pretty(&boards.iter().map(|b| {
                    let mod_count = db::moderators::count_for_board(&mut conn, b.id);
                    serde_json::json!({
                        "id": b.id,
                        "name": b.name,
                        "description": b.description,
                        "creator_id": b.creator_id,
                        "moderators": mod_count,
                    })
                }).collect::<Vec<_>>())?);
            } else {
                println!("{}", "BBS Boards".cyan().bold());
                println!("{}", "─".repeat(60));

                if boards.is_empty() {
                    println!("{}", "No boards found. Run 'osvm bbs init' to create default boards.".yellow());
                } else {
                    for board in boards {
                        let post_count = db::posts::list_for_board(&mut conn, board.id, 1000)
                            .map(|p| p.len())
                            .unwrap_or(0);
                        let mod_count = db::moderators::count_for_board(&mut conn, board.id);
                        let owner_info = if board.creator_id.is_some() {
                            format!(" [owned]")
                        } else {
                            String::new()
                        };
                        println!("  {} {} - {} ({} posts, {} mods){}",
                            "◉".cyan(),
                            board.name.bold(),
                            board.description.dimmed(),
                            post_count,
                            mod_count,
                            owner_info.dimmed()
                        );
                    }
                }
            }
            Ok(())
        }
        Some(("create", sub_m)) => {
            let name = sub_m.get_one::<String>("name").unwrap().to_uppercase();
            let description = sub_m.get_one::<String>("description").unwrap();

            // Get or create CLI user to be the creator
            let timestamp = db::now_as_useconds();
            let (user, _) = db_err(db::users::observe(
                &mut conn,
                "!cliuser1",
                Some("CLI"),
                Some("CLI User"),
                timestamp,
            ))?;

            let board = db_err(db::boards::create_with_creator(&mut conn, &name, description, Some(user.id)))?;
            println!("{} Created board: {} - {}", "✓".green(), board.name.bold(), board.description);
            println!("  Owner: {} (can delete and add moderators)", user.long_name);
            Ok(())
        }
        Some(("delete", sub_m)) => {
            let name = sub_m.get_one::<String>("name").unwrap().to_uppercase();
            let force = sub_m.get_flag("force");

            // Get CLI user
            let timestamp = db::now_as_useconds();
            let (user, _) = db_err(db::users::observe(
                &mut conn,
                "!cliuser1",
                Some("CLI"),
                Some("CLI User"),
                timestamp,
            ))?;

            // Find the board
            let board = db_err(db::boards::get_by_name(&mut conn, &name))?;

            // Check permission
            let can_del = db_err(db::boards::can_delete(&mut conn, board.id, user.id))?;
            if !can_del {
                println!("{} Permission denied: only the board creator or moderators can delete.", "✗".red());
                println!("  Board '{}' creator_id: {:?}", name, board.creator_id);
                return Ok(());
            }

            if !force {
                println!("{} About to delete board '{}'", "!".yellow(), name);
                println!("  This will delete ALL posts in this board.");
                println!("  Use --force to confirm deletion.");
                return Ok(());
            }

            // Delete the board
            let deleted = db_err(db::boards::delete(&mut conn, board.id, user.id))?;
            if deleted {
                println!("{} Deleted board: {}", "✓".green(), name);
            } else {
                println!("{} Failed to delete board (permission denied)", "✗".red());
            }
            Ok(())
        }
        _ => {
            println!("{}", "Use 'osvm bbs boards --help' for available commands".yellow());
            Ok(())
        }
    }
}

/// Handle posting a message
async fn handle_post(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    let board_name = matches.get_one::<String>("board").unwrap().to_uppercase();
    let message = matches.get_one::<String>("message").unwrap();
    let _title = matches.get_one::<String>("title");
    let agent_id = matches.get_one::<String>("as-agent");

    // Find the board
    let board = db_err(db::boards::get_by_name(&mut conn, &board_name))?;

    // Get or create user (CLI user)
    let node_id = agent_id.map(|s| s.as_str()).unwrap_or("!cliuser1");
    let timestamp = db::now_as_useconds();

    let (user, _) = db_err(db::users::observe(
        &mut conn,
        node_id,
        Some("CLI"),
        Some("CLI User"),
        timestamp,
    ))?;

    // Create post
    let post = db_err(db::posts::create(&mut conn, board.id, user.id, message))?;

    println!("{} Posted to {} (ID: {})", "✓".green(), board.name.bold(), post.id);
    println!("  {}", message.dimmed());

    Ok(())
}

/// Handle reading messages from a board
async fn handle_read(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    let board_name = matches.get_one::<String>("board").unwrap().to_uppercase();
    let limit: i64 = matches.get_one::<String>("limit")
        .and_then(|s| s.parse().ok())
        .unwrap_or(20);
    let json = matches.get_flag("json");
    let _follow = matches.get_flag("follow");

    // Find the board
    let board = db_err(db::boards::get_by_name(&mut conn, &board_name))?;

    // Get posts
    let posts = db_err(db::posts::list_for_board(&mut conn, board.id, limit))?;

    if json {
        let output: Vec<_> = posts.iter().map(|p| {
            let user = db::users::get_by_user_id(&mut conn, p.user_id).ok();
            let reply_count = db::posts::reply_count(&mut conn, p.id);
            serde_json::json!({
                "id": p.id,
                "user": user.map(|u| u.short_name).unwrap_or_else(|| "???".to_string()),
                "body": p.body,
                "timestamp": p.created_at_us / 1_000_000,
                "parent_id": p.parent_id,
                "reply_count": reply_count,
            })
        }).collect();
        println!("{}", serde_json::to_string_pretty(&output)?);
    } else {
        println!("{} {}", board.name.cyan().bold(), format!("({} messages)", posts.len()).dimmed());
        println!("{}", "─".repeat(60));

        if posts.is_empty() {
            println!("{}", "No messages yet. Be the first to post!".dimmed());
        } else {
            // Reverse to show oldest first
            for post in posts.iter().rev() {
                let user = db::users::get_by_user_id(&mut conn, post.user_id)
                    .map(|u| u.short_name)
                    .unwrap_or_else(|_| "???".to_string());

                let timestamp = chrono::DateTime::from_timestamp(post.created_at_us / 1_000_000, 0)
                    .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                    .unwrap_or_else(|| "???".to_string());

                let reply_count = db::posts::reply_count(&mut conn, post.id);
                let reply_indicator = if reply_count > 0 {
                    format!(" [{} replies]", reply_count).dimmed().to_string()
                } else {
                    String::new()
                };

                let thread_indicator = if post.parent_id.is_some() {
                    format!("↳ ").cyan().to_string()
                } else {
                    String::new()
                };

                println!("{} {} {}{}",
                    format!("[{}]", post.id).dimmed(),
                    user.cyan(),
                    timestamp.dimmed(),
                    reply_indicator
                );
                println!("  {}{}", thread_indicator, post.body);
                println!();
            }
        }
    }

    Ok(())
}

/// Handle replying to a message
async fn handle_reply(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    let post_id: i32 = matches.get_one::<String>("message_id")
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| anyhow!("Invalid message ID"))?;
    let content = matches.get_one::<String>("content").unwrap();

    // Get or create user
    let timestamp = db::now_as_useconds();
    let (user, _) = db_err(db::users::observe(
        &mut conn,
        "!cliuser1",
        Some("CLI"),
        Some("CLI User"),
        timestamp,
    ))?;

    // Get the parent post to verify it exists
    let parent = db_err(db::posts::get(&mut conn, post_id))?;

    // Create reply
    let reply = db_err(db::posts::reply(&mut conn, post_id, user.id, content))?;

    println!("{} Reply to #{} created (ID: {})", "✓".green(), post_id, reply.id);
    println!("  {}", format!("↳ {}", content).dimmed());

    // Show context
    let board = db_err(db::boards::get(&mut conn, parent.board_id))?;
    println!("\n  In thread on {}", board.name.cyan());

    Ok(())
}

/// Handle agent subcommands
async fn handle_agent(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    match matches.subcommand() {
        Some(("register", sub_m)) => {
            let name = sub_m.get_one::<String>("name").unwrap();
            let capabilities = sub_m.get_one::<String>("capabilities")
                .map(|s| s.as_str())
                .unwrap_or("general");

            // Generate a unique agent node ID
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs() as u32;
            let node_id = num_id_to_hex(timestamp);

            let ts = db::now_as_useconds();
            let (user, created) = db_err(db::users::observe(
                &mut conn,
                &node_id,
                Some(&name[..std::cmp::min(4, name.len())]),
                Some(name),
                ts,
            ))?;

            if created {
                println!("{} Agent registered!", "✓".green());
            } else {
                println!("{} Agent updated!", "✓".green());
            }
            println!("  Node ID: {}", user.node_id.cyan());
            println!("  Name: {}", user.long_name);
            println!("  Capabilities: {}", capabilities.dimmed());

            Ok(())
        }
        Some(("list", _)) => {
            let (total, active) = db::users::counts(&mut conn);
            println!("{}", "Registered Agents".cyan().bold());
            println!("{}", "─".repeat(40));
            println!("  Total users: {}", total);
            println!("  Active users: {}", active);
            println!("\n{}", "Note: Full agent listing requires database query".dimmed());
            Ok(())
        }
        Some(("status", sub_m)) => {
            let agent_id = sub_m.get_one::<String>("agent_id").unwrap();

            if agent_id == "self" {
                println!("{}", "No agent context in CLI mode.".yellow());
                println!("  Use 'osvm bbs agent register <name>' to create an agent.");
            } else {
                match db::users::get(&mut conn, agent_id) {
                    Ok(user) => {
                        println!("{}", "Agent Status".cyan().bold());
                        println!("{}", "─".repeat(40));
                        println!("  Node ID: {}", user.node_id);
                        println!("  Short Name: {}", user.short_name);
                        println!("  Long Name: {}", user.long_name);
                        println!("  Blocked: {}", if user.jackass { "Yes".red() } else { "No".green() });

                        // Check moderator status
                        let mod_boards = db_err(db::moderators::list_for_user(&mut conn, user.id))?;
                        if !mod_boards.is_empty() {
                            println!("  Moderator of: {} boards", mod_boards.len());
                        }
                    }
                    Err(_) => {
                        println!("{} Agent '{}' not found", "!".yellow(), agent_id);
                    }
                }
            }
            Ok(())
        }
        _ => {
            println!("{}", "Use 'osvm bbs agent --help' for available commands".yellow());
            Ok(())
        }
    }
}

/// Handle radio subcommands (Meshtastic)
async fn handle_radio(matches: &ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("connect", sub_m)) => {
            let address = sub_m.get_one::<String>("address").unwrap();

            println!("{}", "Connecting to Meshtastic radio...".cyan());

            // Parse connection type
            let conn_type = meshtastic::ConnectionType::parse(address)
                .ok_or_else(|| anyhow!("Invalid address format"))?;

            println!("  Type: {}", match &conn_type {
                meshtastic::ConnectionType::Tcp { address, port } =>
                    format!("TCP {}:{}", address, port),
                meshtastic::ConnectionType::Serial { device, baud_rate } =>
                    format!("Serial {} @ {} baud", device, baud_rate),
            });

            // Create and connect
            let mut radio = meshtastic::MeshtasticRadio::new(conn_type);

            match radio.connect() {
                Ok(()) => {
                    println!("{} Connected!", "✓".green());
                    println!("  {}", radio.connection_info());

                    // Store in global
                    *RADIO.lock().unwrap() = Some(radio);
                }
                Err(e) => {
                    println!("{} Connection failed: {}", "✗".red(), e);
                    println!("\n{}", "Troubleshooting:".yellow());
                    println!("  • Check radio is powered on");
                    println!("  • Verify IP address/port or serial device");
                    println!("  • Ensure no other app is using the connection");
                }
            }

            Ok(())
        }
        Some(("disconnect", _)) => {
            let mut guard = RADIO.lock().unwrap();
            if let Some(radio) = guard.as_mut() {
                radio.disconnect();
                *guard = None;
                println!("{} Disconnected", "✓".green());
            } else {
                println!("{}", "No active radio connection.".yellow());
            }
            Ok(())
        }
        Some(("status", _)) => {
            println!("{}", "Radio Status".cyan().bold());
            println!("{}", "─".repeat(40));

            let guard = RADIO.lock().unwrap();
            if let Some(radio) = guard.as_ref() {
                let state = radio.state();
                let status_color = match state {
                    meshtastic::ConnectionState::Connected => "●".green(),
                    meshtastic::ConnectionState::Connecting => "●".yellow(),
                    meshtastic::ConnectionState::Disconnected => "●".red(),
                    meshtastic::ConnectionState::Error(_) => "●".red(),
                };
                println!("  Connection: {} {:?}", status_color, state);
                println!("  Info: {}", radio.connection_info());
                println!("  Node ID: {}", num_id_to_hex(radio.our_node_id()));
            } else {
                println!("  Connection: {} Disconnected", "●".red());
                println!("  Protocol: Meshtastic (not connected)");
            }

            println!("\n{}", "Use 'osvm bbs radio connect <address>' to connect.".dimmed());
            Ok(())
        }
        Some(("send", sub_m)) => {
            let message = sub_m.get_one::<String>("message").unwrap();
            let to = sub_m.get_one::<String>("to");

            let mut guard = RADIO.lock().unwrap();
            if let Some(radio) = guard.as_mut() {
                let dest = to.and_then(|s| s.parse().ok());
                match radio.send_text(message, dest) {
                    Ok(()) => {
                        println!("{} Message sent", "✓".green());
                    }
                    Err(e) => {
                        println!("{} Failed to send: {}", "✗".red(), e);
                    }
                }
            } else {
                println!("{} Cannot send: no radio connection.", "✗".red());
                println!("  Message: {}", message.dimmed());
                if let Some(recipient) = to {
                    println!("  To: {}", recipient);
                } else {
                    println!("  To: broadcast");
                }
            }
            Ok(())
        }
        _ => {
            println!("{}", "Use 'osvm bbs radio --help' for available commands".yellow());
            Ok(())
        }
    }
}

/// Global federation manager (lazy singleton)
static FEDERATION: once_cell::sync::Lazy<Arc<tokio::sync::RwLock<Option<Arc<federation::FederationManager>>>>> =
    once_cell::sync::Lazy::new(|| Arc::new(tokio::sync::RwLock::new(None)));

/// Get or create federation manager
async fn get_federation_manager() -> Arc<federation::FederationManager> {
    let mut guard = FEDERATION.write().await;
    if guard.is_none() {
        // Generate node ID from machine ID or random
        let node_id = format!("!{:08x}", std::process::id());
        *guard = Some(Arc::new(federation::FederationManager::new(&node_id)));
    }
    guard.as_ref().unwrap().clone()
}

/// Handle peers subcommands (federation)
async fn handle_peers(matches: &ArgMatches) -> Result<()> {
    let manager = get_federation_manager().await;

    match matches.subcommand() {
        Some(("add", sub_m)) => {
            let address = sub_m.get_one::<String>("address").unwrap();

            println!("{}", "Adding peer...".cyan());

            match manager.add_peer(address).await {
                Ok(peer) => {
                    let status_color = match peer.status {
                        federation::PeerStatus::Online => "●".green(),
                        federation::PeerStatus::Offline => "●".red(),
                        _ => "●".yellow(),
                    };
                    println!("{} Peer added!", "✓".green());
                    println!("  Node ID: {}", peer.node_id.cyan());
                    println!("  Address: {}", peer.address);
                    println!("  Status: {} {:?}", status_color, peer.status);
                }
                Err(e) => {
                    println!("{} Failed to add peer: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        Some(("remove", sub_m)) => {
            let node_id = sub_m.get_one::<String>("node_id").unwrap();

            if manager.remove_peer(node_id).await {
                println!("{} Peer {} removed", "✓".green(), node_id);
            } else {
                println!("{} Peer {} not found", "!".yellow(), node_id);
            }
            Ok(())
        }
        Some(("list", sub_m)) => {
            let json = sub_m.get_flag("json");
            let peers = manager.list_peers().await;

            if json {
                println!("{}", serde_json::to_string_pretty(&peers)?);
            } else {
                println!("{}", "Known Peers".cyan().bold());
                println!("{}", "─".repeat(60));

                if peers.is_empty() {
                    println!("{}", "No peers configured.".dimmed());
                    println!("  Use 'osvm bbs peers add <address>' to add a peer.");
                } else {
                    for peer in peers {
                        let status_color = match peer.status {
                            federation::PeerStatus::Online => "●".green(),
                            federation::PeerStatus::Offline => "●".red(),
                            federation::PeerStatus::Syncing => "●".yellow(),
                            federation::PeerStatus::Unknown => "●".dimmed(),
                            federation::PeerStatus::Error(_) => "●".red(),
                        };
                        let last_sync = peer.last_sync
                            .map(|ts| chrono::DateTime::from_timestamp(ts as i64, 0)
                                .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                                .unwrap_or_else(|| "?".to_string()))
                            .unwrap_or_else(|| "never".to_string());

                        println!("  {} {} {}", status_color, peer.node_id.cyan(), peer.address);
                        println!("      Last sync: {} | Failures: {}", last_sync.dimmed(), peer.failure_count);
                    }
                }

                println!("\n  Our node ID: {}", manager.node_id.cyan());
            }
            Ok(())
        }
        Some(("sync", sub_m)) => {
            let specific_peer = sub_m.get_one::<String>("peer");

            println!("{}", "Syncing with peers...".cyan());

            let peers = if let Some(node_id) = specific_peer {
                manager.list_peers().await.into_iter()
                    .filter(|p| &p.node_id == node_id)
                    .collect()
            } else {
                manager.list_peers().await
            };

            if peers.is_empty() {
                println!("{}", "No peers to sync with.".yellow());
                return Ok(());
            }

            for mut peer in peers {
                print!("  {} {}...", "↔".cyan(), peer.node_id);
                let since = peer.last_sync.unwrap_or(0);
                match manager.sync_from_peer(&mut peer, since).await {
                    Ok(messages) => {
                        println!(" {} {} messages", "✓".green(), messages.len());
                    }
                    Err(e) => {
                        println!(" {} {}", "✗".red(), e);
                    }
                }
            }
            Ok(())
        }
        Some(("discover", sub_m)) => {
            let local = sub_m.get_flag("local");
            let bootstrap = sub_m.get_flag("bootstrap");

            if !local && !bootstrap {
                println!("{}", "Specify --local or --bootstrap".yellow());
                return Ok(());
            }

            if bootstrap {
                println!("{}", "Querying bootstrap servers...".cyan());
                let discovered = manager.discover_from_bootstrap().await;
                println!("  Discovered {} peers", discovered.len());
                for peer in discovered {
                    println!("    {} {}", "→".green(), peer.address);
                }
            }

            if local {
                println!("{}", "Local network discovery (mDNS)...".cyan());
                println!("  {}", "mDNS discovery not yet implemented".yellow());
                println!("  Requires 'mdns' feature flag");
            }

            Ok(())
        }
        _ => {
            println!("{}", "Use 'osvm bbs peers --help' for available commands".yellow());
            Ok(())
        }
    }
}

/// Handle HTTP server command (internet mode)
async fn handle_server(matches: &ArgMatches) -> Result<()> {
    let host = matches.get_one::<String>("host").map(|s| s.as_str()).unwrap_or("0.0.0.0");
    let port: u16 = matches.get_one::<String>("port")
        .and_then(|s| s.parse().ok())
        .unwrap_or(8080);

    println!("{}", "Starting BBS HTTP Server...".cyan().bold());
    println!("  Mode: {} (use when radio not available)", "Internet".green());
    println!("");

    http_server::start_server(host, port)
        .await
        .map_err(|e| anyhow!("Server error: {}", e))
}

/// Handle interactive shell mode
async fn handle_interactive(matches: &ArgMatches) -> Result<()> {
    let board = matches.get_one::<String>("board").unwrap();

    println!("{}", "BBS Interactive Shell".cyan().bold());
    println!("{}", "─".repeat(40));
    println!("  Board: {}", board);
    println!("\n{} For full TUI, use: osvm bbs tui", "!".yellow());
    println!("  Or use individual commands:");
    println!("    osvm bbs read {}", board);
    println!("    osvm bbs post {} \"message\"", board);
    println!("    osvm bbs reply <id> \"reply\"", );

    Ok(())
}

/// Handle full-screen TUI mode
async fn handle_tui(matches: &ArgMatches) -> Result<()> {
    use crossterm::{
        event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers},
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    };
    use ratatui::{backend::CrosstermBackend, Terminal};
    use std::io;
    use std::time::Duration;
    use tokio::sync::mpsc;
    use crate::utils::bbs::meshtastic::{MeshtasticClient, MeshtasticPacket};

    let initial_board = matches.get_one::<String>("board").unwrap();
    let mesh_address = matches.get_one::<String>("mesh");

    // Initialize BBS state
    let mut bbs_state = crate::utils::bbs::tui_widgets::BBSTuiState::new();

    // Connect to database
    if let Err(e) = bbs_state.connect() {
        return Err(anyhow!("Failed to connect to BBS database: {}\nRun 'osvm bbs init' first.", e));
    }

    // Try to connect to Meshtastic if --mesh flag provided
    let mut mesh_rx: Option<mpsc::UnboundedReceiver<MeshtasticPacket>> = None;
    if let Some(addr) = mesh_address {
        println!("📻 Connecting to Meshtastic @ {}...", addr);

        // Try async connection
        if let Some(mut client) = MeshtasticClient::from_address(addr) {
            match client.connect_and_run().await {
                Ok(rx) => {
                    bbs_state.agent_status.meshtastic_connected = true;
                    bbs_state.agent_status.meshtastic_node_id = Some(format!("!{:08x}", client.our_node_id()));
                    bbs_state.status_message = format!("📻 Connected to mesh @ {}", addr);
                    mesh_rx = Some(rx);
                    println!("✅ Meshtastic connected!");
                }
                Err(e) => {
                    println!("⚠️  Meshtastic connection failed: {}", e);
                    // Fall back to sync wrapper for status display only
                    bbs_state.try_connect_meshtastic(Some(addr));
                }
            }
        } else {
            bbs_state.try_connect_meshtastic(Some(addr));
        }
    }

    // Find and select the initial board
    if let Some(idx) = bbs_state.boards.iter().position(|b| b.name.eq_ignore_ascii_case(initial_board)) {
        let board_id = bbs_state.boards[idx].id;
        bbs_state.current_board = Some(board_id);
        bbs_state.selected_board_index = Some(idx);
        let _ = bbs_state.load_posts();
    }

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Main event loop with async mesh support
    let result = run_bbs_tui_async(&mut terminal, &mut bbs_state, mesh_rx).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    result
}

/// BBS TUI event loop
fn run_bbs_tui<B: ratatui::backend::Backend>(
    terminal: &mut ratatui::Terminal<B>,
    state: &mut crate::utils::bbs::tui_widgets::BBSTuiState,
) -> Result<()> {
    use crossterm::event::{self, Event, KeyCode, KeyModifiers};
    use std::time::Duration;

    loop {
        // Draw UI
        terminal.draw(|f| {
            crate::utils::bbs::tui_widgets::render_bbs_tab(f, f.area(), state);
        })?;

        // Handle input
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                // Handle input mode first (when input is active, capture all chars)
                if state.input_active {
                    match key.code {
                        KeyCode::Enter => {
                            if !state.input_buffer.trim().is_empty() {
                                if let Err(e) = state.post_message(&state.input_buffer.clone()) {
                                    state.status_message = format!("Error: {}", e);
                                } else {
                                    state.status_message = "Message posted!".to_string();
                                    let _ = state.load_posts();
                                }
                                state.input_buffer.clear();
                            }
                            state.input_active = false;
                        }
                        KeyCode::Esc => {
                            state.input_buffer.clear();
                            state.input_active = false;
                        }
                        KeyCode::Backspace => {
                            state.input_buffer.pop();
                        }
                        KeyCode::Char(c) if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                            state.input_buffer.push(c);
                        }
                        _ => {}
                    }
                    continue;
                }

                // Not in input mode - handle navigation
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => {
                        return Ok(());
                    }
                    KeyCode::Char('i') => {
                        state.input_active = true;
                    }
                    KeyCode::Char('j') | KeyCode::Down => {
                        state.scroll_offset = state.scroll_offset.saturating_add(1);
                    }
                    KeyCode::Char('k') | KeyCode::Up => {
                        state.scroll_offset = state.scroll_offset.saturating_sub(1);
                    }
                    KeyCode::Char('r') => {
                        let _ = state.refresh_boards();
                        if state.current_board.is_some() {
                            let _ = state.load_posts();
                        }
                        state.status_message = "Refreshed".to_string();
                    }
                    KeyCode::Char(c @ '1'..='9') => {
                        let board_idx = c.to_digit(10).unwrap() as usize - 1;
                        if board_idx < state.boards.len() {
                            let board_id = state.boards[board_idx].id;
                            let board_name = state.boards[board_idx].name.clone();
                            state.current_board = Some(board_id);
                            state.selected_board_index = Some(board_idx);
                            let _ = state.load_posts();
                            state.scroll_offset = 0;
                            state.status_message = format!("Switched to: {}", board_name);
                        }
                    }
                    KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        return Ok(());
                    }
                    _ => {}
                }
            }
        }
    }
}

/// Async BBS TUI event loop with Meshtastic support
async fn run_bbs_tui_async<B: ratatui::backend::Backend>(
    terminal: &mut ratatui::Terminal<B>,
    state: &mut crate::utils::bbs::tui_widgets::BBSTuiState,
    mut mesh_rx: Option<tokio::sync::mpsc::UnboundedReceiver<crate::utils::bbs::meshtastic::MeshtasticPacket>>,
) -> Result<()> {
    use crossterm::event::{self, Event, KeyCode, KeyModifiers};
    use std::time::Duration;
    use crate::utils::bbs::meshtastic::{MeshtasticPacket, BBSCommandRouter};

    loop {
        // Draw UI
        terminal.draw(|f| {
            crate::utils::bbs::tui_widgets::render_bbs_tab(f, f.area(), state);
        })?;

        // Check for mesh messages (non-blocking)
        if let Some(ref mut rx) = mesh_rx {
            // Try to receive without blocking
            while let Ok(packet) = rx.try_recv() {
                match packet {
                    MeshtasticPacket::TextMessage { from, message, .. } => {
                        // Check if it's a BBS command
                        let is_command = message.trim().starts_with('/');

                        // Add to mesh messages
                        state.add_mesh_message(from, message.clone(), is_command);

                        // If it's a command, process it
                        if is_command {
                            if let Some(cmd) = BBSCommandRouter::parse_command(&message) {
                                state.status_message = format!("📻 Cmd from !{:08x}: {:?}", from, cmd);
                            }
                        } else {
                            state.status_message = format!("📻 Msg from !{:08x}: {}", from,
                                if message.len() > 30 { format!("{}...", &message[..30]) } else { message });
                        }
                    }
                    MeshtasticPacket::NodeInfo { node_id, short_name, long_name } => {
                        state.update_mesh_node(node_id, short_name.clone());
                        state.status_message = format!("📻 Node: !{:08x} = {}", node_id, short_name);
                    }
                    _ => {}
                }
            }
        }

        // Handle keyboard input (with timeout for responsiveness)
        if event::poll(Duration::from_millis(50))? {
            if let Event::Key(key) = event::read()? {
                // Handle input mode first (when input is active, capture all chars)
                if state.input_active {
                    match key.code {
                        KeyCode::Enter => {
                            if !state.input_buffer.trim().is_empty() {
                                if let Err(e) = state.post_message(&state.input_buffer.clone()) {
                                    state.status_message = format!("Error: {}", e);
                                } else {
                                    state.status_message = "Message posted!".to_string();
                                    let _ = state.load_posts();
                                }
                                state.input_buffer.clear();
                            }
                            state.input_active = false;
                        }
                        KeyCode::Esc => {
                            state.input_buffer.clear();
                            state.input_active = false;
                        }
                        KeyCode::Backspace => {
                            state.input_buffer.pop();
                        }
                        KeyCode::Char(c) if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                            state.input_buffer.push(c);
                        }
                        _ => {}
                    }
                    continue;
                }

                // Not in input mode - handle navigation
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => {
                        return Ok(());
                    }
                    KeyCode::Char('i') => {
                        state.input_active = true;
                    }
                    KeyCode::Char('j') | KeyCode::Down => {
                        state.scroll_offset = state.scroll_offset.saturating_add(1);
                    }
                    KeyCode::Char('k') | KeyCode::Up => {
                        state.scroll_offset = state.scroll_offset.saturating_sub(1);
                    }
                    KeyCode::Char('r') => {
                        let _ = state.refresh_boards();
                        let _ = state.refresh_agents();
                        if state.current_board.is_some() {
                            let _ = state.load_posts();
                        }
                        state.status_message = "Refreshed".to_string();
                    }
                    KeyCode::Char('m') => {
                        // Toggle showing mesh messages in posts view (future feature)
                        state.status_message = format!("📻 {} mesh messages", state.mesh_messages.len());
                    }
                    KeyCode::Char(c @ '1'..='9') => {
                        let board_idx = c.to_digit(10).unwrap() as usize - 1;
                        if board_idx < state.boards.len() {
                            let board_id = state.boards[board_idx].id;
                            let board_name = state.boards[board_idx].name.clone();
                            state.current_board = Some(board_id);
                            state.selected_board_index = Some(board_idx);
                            let _ = state.load_posts();
                            state.scroll_offset = 0;
                            state.status_message = format!("Switched to: {}", board_name);
                        }
                    }
                    KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        return Ok(());
                    }
                    _ => {}
                }
            }
        }

        // Small yield to allow mesh receiver to process
        tokio::task::yield_now().await;
    }
}

/// Thread display data (mirrors http_server::ThreadedPostInfo for CLI)
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
struct ThreadedPost {
    id: String,
    board: String,
    author: String,
    author_node: String,
    body: String,
    created_at: i64,
    parent_id: Option<String>,
    is_local: bool,
    reply_count: i64,
    score: i32,
    replies: Vec<ThreadedPost>,
    depth: u32,
    collapsed: bool,
}

/// Flattened thread item for display
#[derive(Clone, Debug)]
struct FlattenedThread {
    post: ThreadedPost,
    visible: bool,        // Is this visible (parent not collapsed)?
    is_collapsed: bool,   // Is this post itself collapsed?
    has_children: bool,   // Does it have children?
    sentiment: Option<ThreadSentiment>,  // Sentiment analysis result
    ai_summary: Option<String>,          // AI-generated summary of collapsed children
}

/// Thread sentiment classification
#[derive(Clone, Debug)]
enum ThreadSentiment {
    Positive,   // Constructive, helpful
    Neutral,    // Factual, informational
    Negative,   // Critical, angry
    Mixed,      // Both positive and negative
}

impl ThreadSentiment {
    fn color(&self) -> colored::Color {
        match self {
            ThreadSentiment::Positive => colored::Color::Green,
            ThreadSentiment::Neutral => colored::Color::White,
            ThreadSentiment::Negative => colored::Color::Red,
            ThreadSentiment::Mixed => colored::Color::Yellow,
        }
    }

    fn icon(&self) -> &'static str {
        match self {
            ThreadSentiment::Positive => "😊",
            ThreadSentiment::Neutral => "😐",
            ThreadSentiment::Negative => "😠",
            ThreadSentiment::Mixed => "🤔",
        }
    }
}

/// Thread viewer mode
#[derive(Clone, Debug, PartialEq)]
enum ViewerMode {
    Normal,
    Reply { target_post_id: String },
    Summary { post_id: String, loading: bool },
}

/// Thread viewer state
struct ThreadViewerState {
    flattened: Vec<FlattenedThread>,
    selected_idx: usize,
    scroll_offset: usize,
    board_name: String,
    server_url: Option<String>,
    mode: ViewerMode,
    input_buffer: String,
    status_message: Option<String>,
    ws_connected: bool,
}

/// Handle threads command - interactive collapsible thread viewer
async fn handle_threads(matches: &ArgMatches) -> Result<()> {
    use crossterm::{
        event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers},
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen, Clear, ClearType},
        cursor,
    };
    use std::io::{self, Write};

    let board_name = matches.get_one::<String>("board").unwrap().to_uppercase();
    let server_url = matches.get_one::<String>("server").cloned();
    let json_output = matches.get_flag("json");

    // Fetch threads
    let threads = if let Some(ref url) = server_url {
        // Fetch from HTTP server
        fetch_threads_from_server(url, &board_name).await?
    } else {
        // Fetch from local database (reuse http_server logic)
        fetch_threads_from_local(&board_name)?
    };

    if json_output {
        // Just output JSON and exit
        println!("{}", serde_json::to_string_pretty(&threads)?);
        return Ok(());
    }

    if threads.is_empty() {
        println!("{}", "No threads found in this board.".yellow());
        println!("Try posting first: {}", "osvm bbs post <BOARD> \"message\"".cyan());
        return Ok(());
    }

    // Create viewer state
    let mut state = ThreadViewerState {
        flattened: flatten_threads(&threads),
        selected_idx: 0,
        scroll_offset: 0,
        board_name: board_name.clone(),
        server_url,
        mode: ViewerMode::Normal,
        input_buffer: String::new(),
        status_message: None,
        ws_connected: false,
    };

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;

    let result = run_thread_viewer_async(&mut stdout, &mut state).await;

    // Restore terminal
    disable_raw_mode()?;
    execute!(stdout, LeaveAlternateScreen, DisableMouseCapture)?;

    result
}

/// Fetch threads from HTTP server
async fn fetch_threads_from_server(url: &str, board: &str) -> Result<Vec<ThreadedPost>> {
    let url = format!("{}/api/boards/{}/threads", url.trim_end_matches('/'), board);
    let response = reqwest::get(&url).await
        .map_err(|e| anyhow!("Failed to fetch from server: {}", e))?;

    if !response.status().is_success() {
        return Err(anyhow!("Server returned error: {}", response.status()));
    }

    let api_response: serde_json::Value = response.json().await
        .map_err(|e| anyhow!("Failed to parse response: {}", e))?;

    // Extract data from ApiResponse wrapper
    let data = api_response.get("data")
        .ok_or_else(|| anyhow!("Missing data field in response"))?;

    let threads: Vec<ThreadedPost> = serde_json::from_value(data.clone())
        .map_err(|e| anyhow!("Failed to parse threads: {}", e))?;

    Ok(threads)
}

/// Fetch threads from local database
fn fetch_threads_from_local(board_name: &str) -> Result<Vec<ThreadedPost>> {
    use crate::utils::bbs::http_server;

    let mut conn = db_err(db::establish_connection())?;
    let our_node_id = format!("!{:08x}", std::process::id());

    // Get board
    let board = db::boards::get_by_name(&mut conn, board_name)
        .map_err(|_| anyhow!("Board '{}' not found", board_name))?;

    // Collect posts (reusing http_server logic pattern)
    let mut all_posts: Vec<http_server::UnifiedPostInfo> = Vec::new();

    // Add local posts
    if let Ok(posts) = db::posts::list_for_board(&mut conn, board.id, 500) {
        for p in posts {
            let user_name = db::users::get_by_user_id(&mut conn, p.user_id)
                .map(|u| u.short_name)
                .unwrap_or_else(|_| "???".to_string());
            let reply_count = db::posts::reply_count(&mut conn, p.id);
            let parent_id = p.unified_parent_id();

            all_posts.push(http_server::UnifiedPostInfo {
                id: p.id.to_string(),
                board: board_name.to_string(),
                author: user_name,
                author_node: "local".to_string(),
                body: p.body.clone(),
                created_at: p.created_at_us / 1_000_000,
                parent_id,
                is_local: true,
                reply_count,
                score: p.score,
            });
        }
    }

    // Add federated posts
    if let Ok(fed_posts) = db::federated::get_messages_for_board(&mut conn, board_name, 0, 500) {
        for fp in fed_posts {
            if fp.origin_node == our_node_id {
                continue;
            }
            let reply_count = db::federated::reply_count(&mut conn, &fp.message_id);

            all_posts.push(http_server::UnifiedPostInfo {
                id: fp.message_id.clone(),
                board: fp.board.clone(),
                author: fp.author_name.clone(),
                author_node: fp.author_node.clone(),
                body: fp.body.clone(),
                created_at: fp.created_at,
                parent_id: fp.parent_id.clone(),
                is_local: false,
                reply_count,
                score: 0,
            });
        }
    }

    // Build thread tree manually here
    let threads = build_local_thread_tree(&all_posts);
    Ok(threads)
}

/// Build thread tree from flat posts (local version for CLI)
fn build_local_thread_tree(posts: &[http_server::UnifiedPostInfo]) -> Vec<ThreadedPost> {
    use std::collections::HashMap;
    use crate::utils::bbs::http_server;

    // Map id -> children
    let mut children_map: HashMap<String, Vec<&http_server::UnifiedPostInfo>> = HashMap::new();
    let mut roots: Vec<&http_server::UnifiedPostInfo> = Vec::new();

    for post in posts {
        if let Some(ref parent_id) = post.parent_id {
            children_map.entry(parent_id.clone()).or_default().push(post);
        } else {
            roots.push(post);
        }
    }

    // Sort roots by score (desc) then created_at (desc)
    roots.sort_by(|a, b| {
        match b.score.cmp(&a.score) {
            std::cmp::Ordering::Equal => b.created_at.cmp(&a.created_at),
            other => other,
        }
    });

    fn build_thread(
        post: &http_server::UnifiedPostInfo,
        children_map: &HashMap<String, Vec<&http_server::UnifiedPostInfo>>,
        depth: u32,
    ) -> ThreadedPost {
        let mut replies = Vec::new();

        if let Some(children) = children_map.get(&post.id) {
            let mut sorted: Vec<_> = children.iter().copied().collect();
            sorted.sort_by(|a, b| a.created_at.cmp(&b.created_at));

            for child in sorted {
                replies.push(build_thread(child, children_map, depth + 1));
            }
        }

        ThreadedPost {
            id: post.id.clone(),
            board: post.board.clone(),
            author: post.author.clone(),
            author_node: post.author_node.clone(),
            body: post.body.clone(),
            created_at: post.created_at,
            parent_id: post.parent_id.clone(),
            is_local: post.is_local,
            reply_count: post.reply_count,
            score: post.score,
            replies,
            depth,
            collapsed: post.score < -2,  // Auto-collapse low-score posts
        }
    }

    roots.iter().map(|r| build_thread(r, &children_map, 0)).collect()
}

/// Flatten threads for display (with visibility tracking)
fn flatten_threads(threads: &[ThreadedPost]) -> Vec<FlattenedThread> {
    let mut result = Vec::new();

    fn flatten_recursive(
        post: &ThreadedPost,
        result: &mut Vec<FlattenedThread>,
        parent_collapsed: bool,
    ) {
        let visible = !parent_collapsed;
        let has_children = !post.replies.is_empty();

        // Basic sentiment analysis from content
        let sentiment = analyze_sentiment(&post.body, post.score);

        result.push(FlattenedThread {
            post: post.clone(),
            visible,
            is_collapsed: post.collapsed,
            has_children,
            sentiment: Some(sentiment),
            ai_summary: None,  // Filled on demand
        });

        let child_hidden = parent_collapsed || post.collapsed;
        for child in &post.replies {
            flatten_recursive(child, result, child_hidden);
        }
    }

    for thread in threads {
        flatten_recursive(thread, &mut result, false);
    }

    result
}

/// Simple keyword-based sentiment analysis (runs locally, no AI needed)
fn analyze_sentiment(text: &str, score: i32) -> ThreadSentiment {
    let text_lower = text.to_lowercase();

    // Positive indicators
    let positive_words = ["thanks", "great", "awesome", "helpful", "love", "excellent",
                          "agree", "good", "nice", "perfect", "amazing", "happy", "👍", "❤️", "😊"];
    // Negative indicators
    let negative_words = ["wrong", "bad", "hate", "terrible", "awful", "disagree",
                          "stupid", "broken", "bug", "issue", "problem", "angry", "👎", "😠"];

    let positive_count: i32 = positive_words.iter()
        .filter(|w| text_lower.contains(*w))
        .count() as i32;
    let negative_count: i32 = negative_words.iter()
        .filter(|w| text_lower.contains(*w))
        .count() as i32;

    // Factor in voting score
    let score_factor = if score > 2 { 1 } else if score < -2 { -1 } else { 0 };

    let total = positive_count - negative_count + score_factor;

    if positive_count > 0 && negative_count > 0 {
        ThreadSentiment::Mixed
    } else if total > 0 {
        ThreadSentiment::Positive
    } else if total < 0 {
        ThreadSentiment::Negative
    } else {
        ThreadSentiment::Neutral
    }
}

/// Re-flatten after toggle
fn reflatten(flattened: &mut Vec<FlattenedThread>) {
    // Track collapsed state by id
    let collapsed_ids: std::collections::HashSet<String> = flattened.iter()
        .filter(|f| f.is_collapsed)
        .map(|f| f.post.id.clone())
        .collect();

    // Mark visibility
    for i in 0..flattened.len() {
        let depth = flattened[i].post.depth;

        if depth == 0 {
            flattened[i].visible = true;
        } else {
            // Check if any ancestor is collapsed
            let mut visible = true;
            let mut current_depth = depth;

            // Walk backwards to find parent
            for j in (0..i).rev() {
                if flattened[j].post.depth < current_depth {
                    if collapsed_ids.contains(&flattened[j].post.id) {
                        visible = false;
                        break;
                    }
                    current_depth = flattened[j].post.depth;
                    if current_depth == 0 {
                        break;
                    }
                }
            }
            flattened[i].visible = visible;
        }
    }
}

/// Run the interactive thread viewer (async with all features)
async fn run_thread_viewer_async(
    stdout: &mut std::io::Stdout,
    state: &mut ThreadViewerState,
) -> Result<()> {
    use crossterm::{
        event::{self, Event, KeyCode, KeyModifiers},
        execute,
        terminal::{Clear, ClearType},
        cursor,
    };
    use std::time::Duration;
    use std::io::Write;

    loop {
        // Get terminal size
        let (width, height) = crossterm::terminal::size().unwrap_or((80, 24));
        let max_visible = (height - 6) as usize;  // Header + footer + status/input

        // Get visible items
        let visible_items: Vec<(usize, &FlattenedThread)> = state.flattened.iter()
            .enumerate()
            .filter(|(_, f)| f.visible)
            .collect();

        // Clamp selection
        if !visible_items.is_empty() && state.selected_idx >= visible_items.len() {
            state.selected_idx = visible_items.len() - 1;
        }

        // Adjust scroll
        if state.selected_idx < state.scroll_offset {
            state.scroll_offset = state.selected_idx;
        }
        if state.selected_idx >= state.scroll_offset + max_visible {
            state.scroll_offset = state.selected_idx.saturating_sub(max_visible - 1);
        }

        // Draw screen
        execute!(stdout, Clear(ClearType::All), cursor::MoveTo(0, 0))?;

        // Header with mode indicator
        let mode_indicator = match &state.mode {
            ViewerMode::Normal => "",
            ViewerMode::Reply { .. } => " [REPLY MODE]",
            ViewerMode::Summary { loading, .. } => if *loading { " [LOADING SUMMARY...]" } else { " [SUMMARY]" },
        };
        let ws_indicator = if state.ws_connected { " 🔗" } else { "" };
        let header = format!(
            "📋 {} Threads{}{} | u/d=vote | r=reply | s=summary | c/e=collapse | q=quit",
            state.board_name, mode_indicator, ws_indicator
        );
        println!("{}", header.cyan().bold());
        println!("{}", "─".repeat(width as usize).dimmed());

        // Draw visible threads
        for (view_idx, (flat_idx, item)) in visible_items.iter().enumerate().skip(state.scroll_offset).take(max_visible) {
            let is_selected = view_idx == state.selected_idx;
            let depth = item.post.depth as usize;
            let indent = "  ".repeat(depth);

            // Build collapse indicator
            let collapse_icon = if item.has_children {
                if item.is_collapsed { "[+]" } else { "[-]" }
            } else {
                "   "
            };

            // Sentiment icon
            let sentiment_icon = item.sentiment.as_ref()
                .map(|s| s.icon())
                .unwrap_or("");

            // Score display with vote arrows
            let score_str = if item.post.score > 0 {
                format!("+{}", item.post.score).green().to_string()
            } else if item.post.score < 0 {
                format!("{}", item.post.score).red().to_string()
            } else {
                "0".dimmed().to_string()
            };

            // Truncate body for display
            let max_body_len = (width as usize).saturating_sub(indent.len() + 40);
            let body_preview: String = item.post.body.chars()
                .take(max_body_len)
                .collect::<String>()
                .replace('\n', " ");

            // Format the line with sentiment color
            let prefix = if is_selected { "▶ " } else { "  " };

            let line = format!(
                "{}{}{} {} {} {} {} {}",
                prefix,
                indent,
                collapse_icon,
                sentiment_icon,
                score_str,
                item.post.author.bold(),
                "•".dimmed(),
                body_preview
            );

            // Apply sentiment color to the line
            let colored_line = if let Some(ref sentiment) = item.sentiment {
                match sentiment {
                    ThreadSentiment::Positive => line.green(),
                    ThreadSentiment::Negative => line.red(),
                    ThreadSentiment::Mixed => line.yellow(),
                    ThreadSentiment::Neutral => line.normal(),
                }
            } else {
                line.normal()
            };

            if is_selected {
                println!("{}", colored_line.on_blue());
            } else {
                println!("{}", colored_line);
            }

            // Show AI summary if this post is collapsed and has one
            if item.is_collapsed && item.ai_summary.is_some() {
                let summary = item.ai_summary.as_ref().unwrap();
                let summary_line = format!("{}     📝 {}", indent, summary);
                println!("{}", summary_line.italic().dimmed());
            }
        }

        // Status bar / Input area
        execute!(stdout, cursor::MoveTo(0, height - 3))?;
        println!("{}", "─".repeat(width as usize).dimmed());

        // Mode-specific display
        match &state.mode {
            ViewerMode::Reply { target_post_id } => {
                println!("Replying to post #{}: {}", target_post_id, "(Enter to send, Esc to cancel)".dimmed());
                print!("> {}", state.input_buffer);
            }
            ViewerMode::Summary { post_id, loading } => {
                if *loading {
                    println!("Generating AI summary for post #{}...", post_id);
                } else {
                    println!("Summary mode for post #{}", post_id);
                }
            }
            ViewerMode::Normal => {
                if let Some(ref msg) = state.status_message {
                    println!("{}", msg);
                } else {
                    // Footer
                    let visible_count = visible_items.len();
                    let total_count = state.flattened.len();
                    let footer = format!(
                        " Showing {}/{} | Scroll: {}/{} | j/k=nav ↑↓ | Enter=toggle",
                        visible_count,
                        total_count,
                        state.scroll_offset + 1,
                        visible_count.saturating_sub(max_visible - 1).max(1)
                    );
                    println!("{}", footer.dimmed());
                }
            }
        }

        stdout.flush()?;

        // Handle input
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                // Mode-specific key handling
                match &state.mode {
                    ViewerMode::Reply { target_post_id } => {
                        match key.code {
                            KeyCode::Esc => {
                                state.mode = ViewerMode::Normal;
                                state.input_buffer.clear();
                            }
                            KeyCode::Enter => {
                                // Submit reply
                                if !state.input_buffer.is_empty() {
                                    let reply_text = state.input_buffer.clone();
                                    let target = target_post_id.clone();
                                    state.input_buffer.clear();
                                    state.mode = ViewerMode::Normal;

                                    // Post the reply
                                    match submit_reply(&state.board_name, &target, &reply_text, state.server_url.as_deref()).await {
                                        Ok(_) => {
                                            state.status_message = Some(format!("{} Reply posted!", "✓".green()));
                                            // Refresh threads
                                            if let Ok(threads) = refresh_threads(&state.board_name, state.server_url.as_deref()).await {
                                                state.flattened = flatten_threads(&threads);
                                                reflatten(&mut state.flattened);
                                            }
                                        }
                                        Err(e) => {
                                            state.status_message = Some(format!("{} Failed: {}", "✗".red(), e));
                                        }
                                    }
                                }
                            }
                            KeyCode::Backspace => {
                                state.input_buffer.pop();
                            }
                            KeyCode::Char(c) => {
                                state.input_buffer.push(c);
                            }
                            _ => {}
                        }
                        continue;
                    }
                    ViewerMode::Summary { .. } => {
                        if key.code == KeyCode::Esc || key.code == KeyCode::Char('s') {
                            state.mode = ViewerMode::Normal;
                        }
                        continue;
                    }
                    ViewerMode::Normal => {}
                }

                // Normal mode key handling
                match key.code {
                    KeyCode::Char('q') | KeyCode::Esc => {
                        return Ok(());
                    }
                    KeyCode::Char('j') | KeyCode::Down => {
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() - 1 {
                            state.selected_idx += 1;
                        }
                    }
                    KeyCode::Char('k') | KeyCode::Up => {
                        state.selected_idx = state.selected_idx.saturating_sub(1);
                    }
                    KeyCode::Enter | KeyCode::Char(' ') => {
                        // Toggle collapse
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() {
                            let (flat_idx, _) = visible_items[state.selected_idx];
                            if state.flattened[flat_idx].has_children {
                                state.flattened[flat_idx].is_collapsed = !state.flattened[flat_idx].is_collapsed;
                                reflatten(&mut state.flattened);
                            }
                        }
                    }
                    // FEATURE 1: Upvote/Downvote
                    KeyCode::Char('u') => {
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() {
                            let (flat_idx, _) = visible_items[state.selected_idx];
                            let post_id = &state.flattened[flat_idx].post.id;
                            match cast_vote_on_post(post_id, 1, state.server_url.as_deref()).await {
                                Ok(new_score) => {
                                    state.flattened[flat_idx].post.score = new_score;
                                    state.status_message = Some(format!("{} Upvoted! Score: {}", "▲".green(), new_score));
                                }
                                Err(e) => {
                                    state.status_message = Some(format!("{} Vote failed: {}", "✗".red(), e));
                                }
                            }
                        }
                    }
                    KeyCode::Char('d') => {
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() {
                            let (flat_idx, _) = visible_items[state.selected_idx];
                            let post_id = &state.flattened[flat_idx].post.id;
                            match cast_vote_on_post(post_id, -1, state.server_url.as_deref()).await {
                                Ok(new_score) => {
                                    state.flattened[flat_idx].post.score = new_score;
                                    state.status_message = Some(format!("{} Downvoted! Score: {}", "▼".red(), new_score));
                                }
                                Err(e) => {
                                    state.status_message = Some(format!("{} Vote failed: {}", "✗".red(), e));
                                }
                            }
                        }
                    }
                    // FEATURE 2: Reply mode
                    KeyCode::Char('r') => {
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() {
                            let (flat_idx, _) = visible_items[state.selected_idx];
                            let post_id = state.flattened[flat_idx].post.id.clone();
                            state.mode = ViewerMode::Reply { target_post_id: post_id };
                            state.input_buffer.clear();
                        }
                    }
                    // FEATURE 4: AI Summary
                    KeyCode::Char('s') if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                        if !visible_items.is_empty() && state.selected_idx < visible_items.len() {
                            let (flat_idx, _) = visible_items[state.selected_idx];
                            if state.flattened[flat_idx].has_children && state.flattened[flat_idx].is_collapsed {
                                let post_id = state.flattened[flat_idx].post.id.clone();
                                state.mode = ViewerMode::Summary { post_id: post_id.clone(), loading: true };

                                // Collect children text for summarization
                                let children_text = collect_children_text(&state.flattened, flat_idx);
                                match generate_thread_summary(&children_text).await {
                                    Ok(summary) => {
                                        state.flattened[flat_idx].ai_summary = Some(summary);
                                        state.status_message = Some(format!("{} Summary generated!", "✓".green()));
                                    }
                                    Err(e) => {
                                        state.status_message = Some(format!("{} Summary failed: {}", "✗".red(), e));
                                    }
                                }
                                state.mode = ViewerMode::Normal;
                            } else {
                                state.status_message = Some("Collapse a thread with children first (Enter), then press 's'".yellow().to_string());
                            }
                        }
                    }
                    KeyCode::Char('c') if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                        // Collapse all
                        for f in state.flattened.iter_mut() {
                            if f.has_children {
                                f.is_collapsed = true;
                            }
                        }
                        reflatten(&mut state.flattened);
                    }
                    KeyCode::Char('e') => {
                        // Expand all
                        for f in state.flattened.iter_mut() {
                            f.is_collapsed = false;
                        }
                        reflatten(&mut state.flattened);
                    }
                    KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        return Ok(());
                    }
                    KeyCode::PageDown => {
                        state.selected_idx = (state.selected_idx + max_visible).min(visible_items.len().saturating_sub(1));
                    }
                    KeyCode::PageUp => {
                        state.selected_idx = state.selected_idx.saturating_sub(max_visible);
                    }
                    KeyCode::Home => {
                        state.selected_idx = 0;
                        state.scroll_offset = 0;
                    }
                    KeyCode::End => {
                        state.selected_idx = visible_items.len().saturating_sub(1);
                    }
                    // FEATURE 3: Refresh (for now, WebSocket would be automatic)
                    KeyCode::Char('R') => {
                        state.status_message = Some("Refreshing...".cyan().to_string());
                        if let Ok(threads) = refresh_threads(&state.board_name, state.server_url.as_deref()).await {
                            state.flattened = flatten_threads(&threads);
                            reflatten(&mut state.flattened);
                            state.status_message = Some(format!("{} Refreshed!", "✓".green()));
                        }
                    }
                    _ => {}
                }

                // Clear status after a few iterations (simple timeout)
                if state.status_message.is_some() {
                    // We'll let it stay for one more loop iteration
                }
            }
        } else {
            // Clear status message on poll timeout
            state.status_message = None;
        }
    }
}

/// Cast a vote on a post (local or via HTTP)
async fn cast_vote_on_post(post_id: &str, vote_type: i32, server_url: Option<&str>) -> Result<i32> {
    if let Some(url) = server_url {
        // Vote via HTTP API
        let vote_url = format!("{}/api/posts/{}/vote", url.trim_end_matches('/'), post_id);
        let client = reqwest::Client::new();
        let response = client.post(&vote_url)
            .json(&serde_json::json!({ "vote": vote_type }))
            .send()
            .await
            .map_err(|e| anyhow!("Vote request failed: {}", e))?;

        if !response.status().is_success() {
            return Err(anyhow!("Vote failed: {}", response.status()));
        }

        let result: serde_json::Value = response.json().await?;
        let new_score = result.get("data")
            .and_then(|d| d.get("new_score"))
            .and_then(|s| s.as_i64())
            .unwrap_or(0) as i32;
        Ok(new_score)
    } else {
        // Vote locally
        let mut conn = db_err(db::establish_connection())?;
        let post_id_num: i32 = post_id.parse().map_err(|_| anyhow!("Invalid post ID"))?;

        // Use a default user ID for local voting (we'd need proper user tracking in production)
        let user_id = 1; // TODO: Get actual user from session

        match db::votes::cast_vote(&mut conn, user_id, post_id_num, vote_type) {
            Ok(result) => {
                let new_score = match result {
                    db::votes::VoteResult::Voted { new_score } => new_score,
                    db::votes::VoteResult::Changed { new_score, .. } => new_score,
                    db::votes::VoteResult::Removed { new_score } => new_score,
                };
                Ok(new_score)
            }
            Err(e) => Err(anyhow!("Vote failed: {}", e)),
        }
    }
}

/// Submit a reply to a post
async fn submit_reply(board: &str, parent_id: &str, body: &str, server_url: Option<&str>) -> Result<()> {
    if let Some(url) = server_url {
        // Reply via HTTP API
        let reply_url = format!("{}/api/boards/{}/posts", url.trim_end_matches('/'), board);
        let client = reqwest::Client::new();
        let response = client.post(&reply_url)
            .json(&serde_json::json!({
                "message": body,
                "parent_id": parent_id
            }))
            .send()
            .await
            .map_err(|e| anyhow!("Reply request failed: {}", e))?;

        if !response.status().is_success() {
            return Err(anyhow!("Reply failed: {}", response.status()));
        }
        Ok(())
    } else {
        // Reply locally
        let mut conn = db_err(db::establish_connection())?;

        // Get or create user for local posting (using observe which upserts)
        let node_id = format!("!{:08x}", std::process::id());
        let now_us = crate::utils::bbs::db::now_as_useconds();
        let (user, _) = db::users::observe(&mut conn, &node_id, Some("CLI"), Some("CLI User"), now_us)
            .map_err(|e| anyhow!("Failed to get/create user: {}", e))?;

        // Get board
        let board_record = db::boards::get_by_name(&mut conn, board)
            .map_err(|_| anyhow!("Board not found"))?;

        // Parse parent_id - if it's numeric, it's a local reply; otherwise federated
        if let Ok(local_parent) = parent_id.parse::<i32>() {
            // Local reply
            db::posts::create_with_parent(&mut conn, board_record.id, user.id, body, Some(local_parent))
                .map_err(|e| anyhow!("Failed to create reply: {}", e))?;
        } else {
            // Federated reply
            db::posts::create_with_federated_parent(&mut conn, board_record.id, user.id, body, parent_id)
                .map_err(|e| anyhow!("Failed to create reply: {}", e))?;
        }
        Ok(())
    }
}

/// Refresh threads from source
async fn refresh_threads(board: &str, server_url: Option<&str>) -> Result<Vec<ThreadedPost>> {
    if let Some(url) = server_url {
        fetch_threads_from_server(url, board).await
    } else {
        fetch_threads_from_local(board)
    }
}

/// Collect children text for AI summarization
fn collect_children_text(flattened: &[FlattenedThread], parent_idx: usize) -> String {
    let parent_depth = flattened[parent_idx].post.depth;
    let mut texts = Vec::new();

    for item in flattened.iter().skip(parent_idx + 1) {
        if item.post.depth <= parent_depth {
            break; // No longer a child
        }
        let indent = "  ".repeat((item.post.depth - parent_depth - 1) as usize);
        texts.push(format!("{}- {}: {}", indent, item.post.author, item.post.body));
    }

    texts.join("\n")
}

/// Generate AI summary of thread content
async fn generate_thread_summary(content: &str) -> Result<String> {
    // Try to use AI service if available
    let openai_url = std::env::var("OPENAI_URL").ok();
    let openai_key = std::env::var("OPENAI_KEY").ok();

    if let (Some(url), Some(key)) = (openai_url, openai_key) {
        let client = reqwest::Client::new();
        let prompt = format!(
            "Summarize this thread discussion in 1-2 sentences. Be concise and capture the main points:\n\n{}",
            content
        );

        let response = client.post(&url)
            .header("Authorization", format!("Bearer {}", key))
            .json(&serde_json::json!({
                "model": "gpt-3.5-turbo",
                "messages": [
                    {"role": "system", "content": "You are a helpful assistant that summarizes discussions concisely."},
                    {"role": "user", "content": prompt}
                ],
                "max_tokens": 100,
                "temperature": 0.3
            }))
            .send()
            .await;

        if let Ok(resp) = response {
            if let Ok(json) = resp.json::<serde_json::Value>().await {
                if let Some(choice) = json.get("choices").and_then(|c| c.get(0)) {
                    if let Some(message) = choice.get("message").and_then(|m| m.get("content")) {
                        return Ok(message.as_str().unwrap_or("Summary unavailable").to_string());
                    }
                }
            }
        }
    }

    // Fallback: simple extractive summary
    let sentences: Vec<&str> = content.split('.').collect();
    if sentences.is_empty() {
        return Ok("No content to summarize".to_string());
    }

    // Take first and last meaningful parts
    let preview = content.chars().take(150).collect::<String>();
    Ok(format!("{}...", preview.replace('\n', " ")))
}

/// Handle stats command
async fn handle_stats(matches: &ArgMatches) -> Result<()> {
    let mut conn = db_err(db::establish_connection())?;

    let json = matches.get_flag("json");

    let board_count = db::boards::count(&mut conn);
    let post_count = db::posts::count(&mut conn);
    let (user_count, active_users) = db::users::counts(&mut conn);

    // Check radio status
    let radio_connected = RADIO.lock().unwrap().as_ref()
        .map(|r| r.state() == meshtastic::ConnectionState::Connected)
        .unwrap_or(false);

    if json {
        println!("{}", serde_json::to_string_pretty(&serde_json::json!({
            "boards": board_count,
            "posts": post_count,
            "users": {
                "total": user_count,
                "active": active_users,
            },
            "radio_connected": radio_connected,
            "database": crate::utils::bbs::db_path().to_string_lossy(),
        }))?);
    } else {
        println!("{}", "BBS Statistics".cyan().bold());
        println!("{}", "─".repeat(40));
        println!("  Boards: {}", board_count);
        println!("  Posts: {}", post_count);
        println!("  Users: {} ({} active)", user_count, active_users);
        println!("  Radio: {}", if radio_connected { "Connected".green() } else { "Disconnected".dimmed() });
        println!("\n  Database: {}", crate::utils::bbs::db_path().display());
    }

    Ok(())
}

/// Handle analytics command - comprehensive board analytics
async fn handle_analytics(matches: &ArgMatches) -> Result<()> {
    use chrono::{Local, TimeZone, Timelike};

    let mut conn = db_err(db::establish_connection())?;
    let board_name = matches.get_one::<String>("board")
        .map(|s| s.to_uppercase())
        .unwrap_or_else(|| "GENERAL".to_string());
    let days: i64 = matches.get_one::<String>("days")
        .and_then(|s| s.parse().ok())
        .unwrap_or(7);
    let json = matches.get_flag("json");

    // Get board
    let board = match db::boards::get_by_name(&mut conn, &board_name) {
        Ok(b) => b,
        Err(_) => {
            println!("{}", format!("Board '{}' not found", board_name).red());
            return Ok(());
        }
    };

    // Calculate time range
    let now_us = db::now_as_useconds();
    let cutoff_us = now_us - (days * 24 * 60 * 60 * 1_000_000);

    // Get all posts for the board in the time range
    let posts = db::posts::list_for_board(&mut conn, board.id, 10000)
        .unwrap_or_default()
        .into_iter()
        .filter(|p| p.created_at_us >= cutoff_us)
        .collect::<Vec<_>>();

    // Analyze votes
    let mut total_upvotes = 0i64;
    let mut total_downvotes = 0i64;
    let mut total_score: i64 = 0;
    let mut positive_posts = 0;
    let mut negative_posts = 0;
    let mut neutral_posts = 0;

    // Sentiment analysis
    let mut sentiment_positive = 0;
    let mut sentiment_negative = 0;
    let mut sentiment_neutral = 0;
    let mut sentiment_mixed = 0;

    // Activity by hour
    let mut activity_by_hour: [u32; 24] = [0; 24];

    // Top contributors
    let mut contributor_posts: std::collections::HashMap<i32, u32> = std::collections::HashMap::new();

    for post in &posts {
        // Score analysis
        total_score += post.score as i64;
        if post.score > 0 {
            positive_posts += 1;
            total_upvotes += post.score as i64;
        } else if post.score < 0 {
            negative_posts += 1;
            total_downvotes += (-post.score) as i64;
        } else {
            neutral_posts += 1;
        }

        // Vote counts from user_votes table
        let (up, down) = db::votes::get_vote_counts(&mut conn, post.id);
        total_upvotes = total_upvotes.max(up);
        total_downvotes = total_downvotes.max(down);

        // Sentiment (reuse the analyze_sentiment function)
        let sentiment = analyze_sentiment(&post.body, post.score);
        match sentiment {
            ThreadSentiment::Positive => sentiment_positive += 1,
            ThreadSentiment::Negative => sentiment_negative += 1,
            ThreadSentiment::Neutral => sentiment_neutral += 1,
            ThreadSentiment::Mixed => sentiment_mixed += 1,
        }

        // Activity by hour
        if let chrono::MappedLocalTime::Single(dt) = Local.timestamp_micros(post.created_at_us) {
            let hour = dt.hour() as usize;
            activity_by_hour[hour] += 1;
        }

        // Track contributors
        *contributor_posts.entry(post.user_id).or_insert(0) += 1;
    }

    // Get top contributors
    let mut top_contributors: Vec<(i32, u32)> = contributor_posts.into_iter().collect();
    top_contributors.sort_by(|a, b| b.1.cmp(&a.1));
    let top_5: Vec<_> = top_contributors.into_iter().take(5).collect();

    // Peak hour
    let peak_hour = activity_by_hour.iter()
        .enumerate()
        .max_by_key(|(_, &count)| count)
        .map(|(hour, _)| hour)
        .unwrap_or(0);

    // Format output
    if json {
        let top_contributors_json: Vec<_> = top_5.iter()
            .filter_map(|(user_id, count)| {
                db::users::get_by_user_id(&mut conn, *user_id)
                    .ok()
                    .map(|u| serde_json::json!({
                        "user": u.short_name,
                        "node_id": u.node_id,
                        "posts": count,
                    }))
            })
            .collect();

        println!("{}", serde_json::to_string_pretty(&serde_json::json!({
            "board": board_name,
            "period_days": days,
            "posts": {
                "total": posts.len(),
                "with_positive_score": positive_posts,
                "with_negative_score": negative_posts,
                "neutral": neutral_posts,
            },
            "votes": {
                "total_upvotes": total_upvotes,
                "total_downvotes": total_downvotes,
                "net_score": total_score,
                "upvote_ratio": if total_upvotes + total_downvotes > 0 {
                    (total_upvotes as f64 / (total_upvotes + total_downvotes) as f64 * 100.0).round()
                } else { 0.0 },
            },
            "sentiment": {
                "positive": sentiment_positive,
                "negative": sentiment_negative,
                "neutral": sentiment_neutral,
                "mixed": sentiment_mixed,
            },
            "activity": {
                "peak_hour": peak_hour,
                "by_hour": activity_by_hour.to_vec(),
            },
            "top_contributors": top_contributors_json,
        }))?);
    } else {
        println!("{}", format!("📊 Analytics: {} (Last {} days)", board_name, days).cyan().bold());
        println!("{}", "═".repeat(50));

        // Post summary
        println!("\n{}", "📝 Posts".bold());
        println!("   Total: {}", posts.len());
        println!("   With positive score: {} {}", positive_posts, "▲".green());
        println!("   With negative score: {} {}", negative_posts, "▼".red());
        println!("   Neutral: {}", neutral_posts);

        // Vote summary
        println!("\n{}", "🗳️  Votes".bold());
        println!("   Total upvotes: {} {}", total_upvotes, "▲".green());
        println!("   Total downvotes: {} {}", total_downvotes, "▼".red());
        println!("   Net score: {}", if total_score >= 0 {
            format!("+{}", total_score).green()
        } else {
            format!("{}", total_score).red()
        });
        if total_upvotes + total_downvotes > 0 {
            let ratio = total_upvotes as f64 / (total_upvotes + total_downvotes) as f64 * 100.0;
            println!("   Upvote ratio: {:.1}%", ratio);
        }

        // Sentiment summary
        println!("\n{}", "😊 Sentiment".bold());
        let total_sentiment = sentiment_positive + sentiment_negative + sentiment_neutral + sentiment_mixed;
        if total_sentiment > 0 {
            println!("   Positive: {} ({:.1}%) {}", sentiment_positive,
                sentiment_positive as f64 / total_sentiment as f64 * 100.0, "😊".green());
            println!("   Negative: {} ({:.1}%) {}", sentiment_negative,
                sentiment_negative as f64 / total_sentiment as f64 * 100.0, "😠".red());
            println!("   Neutral:  {} ({:.1}%) {}", sentiment_neutral,
                sentiment_neutral as f64 / total_sentiment as f64 * 100.0, "😐");
            println!("   Mixed:    {} ({:.1}%) {}", sentiment_mixed,
                sentiment_mixed as f64 / total_sentiment as f64 * 100.0, "🤔".yellow());
        }

        // Activity chart (simplified sparkline)
        println!("\n{}", "⏰ Activity by Hour".bold());
        let max_activity = *activity_by_hour.iter().max().unwrap_or(&1);
        let bar_chars = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];
        print!("   ");
        for count in activity_by_hour.iter() {
            let level = if max_activity > 0 {
                (*count as f64 / max_activity as f64 * 7.0).round() as usize
            } else { 0 };
            print!("{}", bar_chars[level.min(7)]);
        }
        println!();
        println!("   0h            12h            24h");
        println!("   Peak hour: {}:00", peak_hour);

        // Top contributors
        if !top_5.is_empty() {
            println!("\n{}", "🏆 Top Contributors".bold());
            for (i, (user_id, count)) in top_5.iter().enumerate() {
                let user_name = db::users::get_by_user_id(&mut conn, *user_id)
                    .map(|u| u.short_name)
                    .unwrap_or_else(|_| "???".to_string());
                let medal = match i {
                    0 => "🥇",
                    1 => "🥈",
                    2 => "🥉",
                    _ => "  ",
                };
                println!("   {} {} - {} posts", medal, user_name, count);
            }
        }

        println!("\n{}", "─".repeat(50).dimmed());
    }

    Ok(())
}

/// Handle on-chain registry commands (Solana devnet)
async fn handle_registry(matches: &ArgMatches) -> Result<()> {
    let rpc_url = matches.get_one::<String>("rpc").map(|s| s.as_str());

    match matches.subcommand() {
        Some(("register", sub_m)) => {
            let address = sub_m.get_one::<String>("address").unwrap();
            let name = sub_m.get_one::<String>("name").unwrap();
            let keypair_path = sub_m.get_one::<String>("keypair")
                .map(|s| s.as_str())
                .unwrap_or("~/.config/solana/id.json");

            println!("{}", "Registering node on Solana devnet...".cyan());

            // Expand ~ in path
            let expanded_path = registry::expand_path(keypair_path);
            let keypair = match registry::load_keypair(&expanded_path) {
                Ok(kp) => kp,
                Err(e) => {
                    println!("{} Failed to load keypair: {}", "✗".red(), e);
                    println!("\n{}", "To create a keypair:".yellow());
                    println!("  solana-keygen new --outfile ~/.config/solana/id.json");
                    println!("  solana airdrop 1 --url devnet");
                    return Ok(());
                }
            };

            let client = match registry::RegistryClient::new(rpc_url) {
                Ok(c) => c,
                Err(e) => {
                    println!("{} Failed to connect: {}", "✗".red(), e);
                    return Ok(());
                }
            };

            // Generate node ID from address
            let node_id = registry::generate_node_id(address);

            match client.register(&keypair, node_id, address, name) {
                Ok(sig) => {
                    println!("{} Node registered on-chain!", "✓".green());
                    println!("  Node ID: {}", format!("!{:02x}{:02x}{:02x}{:02x}",
                        node_id[0], node_id[1], node_id[2], node_id[3]).cyan());
                    println!("  Address: {}", address);
                    println!("  Name: {}", name);
                    println!("  Owner: {}", keypair.pubkey());
                    println!("  Tx: {}", sig.dimmed());
                }
                Err(e) => {
                    println!("{} Registration failed: {}", "✗".red(), e);
                    println!("\n{}", "Common issues:".yellow());
                    println!("  • Insufficient SOL for rent (need ~0.002 SOL)");
                    println!("  • Node already registered (use 'update' instead)");
                    println!("  • Program not deployed (wait for deployment)");
                }
            }
            Ok(())
        }
        Some(("list", sub_m)) => {
            let json = sub_m.get_flag("json");

            println!("{}", "Querying on-chain registry...".cyan());

            let client = match registry::RegistryClient::new(rpc_url) {
                Ok(c) => c,
                Err(e) => {
                    println!("{} Failed to connect: {}", "✗".red(), e);
                    return Ok(());
                }
            };

            match client.list_nodes() {
                Ok(nodes) => {
                    if json {
                        let output: Vec<_> = nodes.iter().map(|(pda, node)| {
                            serde_json::json!({
                                "pda": pda.to_string(),
                                "node_id": node.get_node_id_string(),
                                "address": node.get_address(),
                                "name": node.get_name(),
                                "owner": node.owner.to_string(),
                                "registered_at": node.registered_at,
                                "last_heartbeat": node.last_heartbeat,
                            })
                        }).collect();
                        println!("{}", serde_json::to_string_pretty(&output)?);
                    } else {
                        println!("{}", "On-Chain BBS Nodes".cyan().bold());
                        println!("{}", "─".repeat(70));

                        if nodes.is_empty() {
                            println!("{}", "No nodes registered yet.".dimmed());
                            println!("  Use 'osvm bbs registry register' to be the first!");
                        } else {
                            for (_pda, node) in nodes {
                                let heartbeat = registry::format_timestamp(node.last_heartbeat);
                                println!("  {} {} {}",
                                    "◉".green(),
                                    node.get_node_id_string().cyan(),
                                    node.get_name().bold()
                                );
                                println!("      Address: {}", node.get_address());
                                println!("      Last seen: {}", heartbeat.dimmed());
                                println!();
                            }
                        }

                        println!("  Program: {}", registry::PROGRAM_ID.dimmed());
                    }
                }
                Err(e) => {
                    println!("{} Failed to query: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        Some(("update", sub_m)) => {
            let address = sub_m.get_one::<String>("address");
            let name = sub_m.get_one::<String>("name");
            let keypair_path = sub_m.get_one::<String>("keypair")
                .map(|s| s.as_str())
                .unwrap_or("~/.config/solana/id.json");

            if address.is_none() && name.is_none() {
                println!("{}", "Nothing to update. Specify --address or --name.".yellow());
                return Ok(());
            }

            let expanded_path = registry::expand_path(keypair_path);
            let keypair = registry::load_keypair(&expanded_path)?;

            let client = registry::RegistryClient::new(rpc_url)?;

            match client.update(&keypair, address.map(|s| s.as_str()), name.map(|s| s.as_str())) {
                Ok(sig) => {
                    println!("{} Registration updated!", "✓".green());
                    if let Some(a) = address {
                        println!("  New address: {}", a);
                    }
                    if let Some(n) = name {
                        println!("  New name: {}", n);
                    }
                    println!("  Tx: {}", sig.dimmed());
                }
                Err(e) => {
                    println!("{} Update failed: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        Some(("heartbeat", sub_m)) => {
            let keypair_path = sub_m.get_one::<String>("keypair")
                .map(|s| s.as_str())
                .unwrap_or("~/.config/solana/id.json");

            let expanded_path = registry::expand_path(keypair_path);
            let keypair = registry::load_keypair(&expanded_path)?;

            let client = registry::RegistryClient::new(rpc_url)?;

            match client.heartbeat(&keypair) {
                Ok(sig) => {
                    println!("{} Heartbeat sent!", "✓".green());
                    println!("  Tx: {}", sig.dimmed());
                }
                Err(e) => {
                    println!("{} Heartbeat failed: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        Some(("deregister", sub_m)) => {
            let keypair_path = sub_m.get_one::<String>("keypair")
                .map(|s| s.as_str())
                .unwrap_or("~/.config/solana/id.json");
            let force = sub_m.get_flag("force");

            if !force {
                println!("{} This will remove your node from the on-chain registry.", "!".yellow());
                println!("  Use --force to confirm.");
                return Ok(());
            }

            let expanded_path = registry::expand_path(keypair_path);
            let keypair = registry::load_keypair(&expanded_path)?;

            let client = registry::RegistryClient::new(rpc_url)?;

            match client.deregister(&keypair, None) {
                Ok(sig) => {
                    println!("{} Node deregistered!", "✓".green());
                    println!("  Rent returned to: {}", keypair.pubkey());
                    println!("  Tx: {}", sig.dimmed());
                }
                Err(e) => {
                    println!("{} Deregistration failed: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        Some(("discover", _sub_m)) => {
            println!("{}", "Discovering peers from on-chain registry...".cyan());

            let client = registry::RegistryClient::new(rpc_url)?;
            let manager = get_federation_manager().await;

            match client.list_nodes() {
                Ok(nodes) => {
                    println!("  Found {} registered nodes", nodes.len());

                    let mut added = 0;
                    for (_pda, node) in nodes {
                        let address = node.get_address();
                        if !address.is_empty() {
                            if let Ok(_peer) = manager.add_peer(&address).await {
                                println!("    {} {} {}", "→".green(), node.get_node_id_string().cyan(), address);
                                added += 1;
                            }
                        }
                    }

                    println!("\n{} Added {} peers from registry", "✓".green(), added);
                }
                Err(e) => {
                    println!("{} Discovery failed: {}", "✗".red(), e);
                }
            }
            Ok(())
        }
        _ => {
            println!("{}", "On-Chain Registry Commands".cyan().bold());
            println!("{}", "─".repeat(50));
            println!("  {} - Register this node on Solana devnet", "register".bold());
            println!("  {} - List all registered nodes", "list".bold());
            println!("  {} - Update registration (address/name)", "update".bold());
            println!("  {} - Send heartbeat (update last_seen)", "heartbeat".bold());
            println!("  {} - Remove registration from chain", "deregister".bold());
            println!("  {} - Discover and add peers from registry", "discover".bold());
            println!();
            println!("  Program ID: {}", registry::PROGRAM_ID.dimmed());
            println!("  Network: {}", "Solana Devnet".cyan());
            println!();
            println!("{}", "Use 'osvm bbs registry --help' for options.".dimmed());
            Ok(())
        }
    }
}

/// Handle mesh message management commands
async fn handle_mesh(matches: &ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("stats", sub_m)) => handle_mesh_stats(sub_m).await,
        Some(("recent", sub_m)) => handle_mesh_recent(sub_m).await,
        Some(("nodes", sub_m)) => handle_mesh_nodes(sub_m).await,
        Some(("prune", sub_m)) => handle_mesh_prune(sub_m).await,
        _ => {
            println!("{}", "Mesh Message Commands".cyan().bold());
            println!("{}", "─".repeat(50));
            println!("  {} - Show mesh message statistics", "stats".bold());
            println!("  {} - View recent mesh messages", "recent".bold());
            println!("  {} - List mesh nodes by activity", "nodes".bold());
            println!("  {} - Remove old messages", "prune".bold());
            println!();
            println!("{}", "Use 'osvm bbs mesh --help' for options.".dimmed());
            Ok(())
        }
    }
}

/// Show mesh statistics
async fn handle_mesh_stats(matches: &ArgMatches) -> Result<()> {
    let json_output = matches.get_flag("json");
    let show_hourly = matches.get_flag("hourly");

    let mut conn = db_err(db::establish_connection())?;

    let stats = db::mesh_messages::get_stats(&mut conn)
        .map_err(|e| anyhow::anyhow!("Failed to get mesh stats: {}", e))?;

    if json_output {
        let json = serde_json::json!({
            "total_messages": stats.total_messages,
            "total_commands": stats.total_commands,
            "total_responses": stats.total_responses,
            "unique_nodes": stats.unique_nodes,
            "messages_last_hour": stats.messages_last_hour,
            "messages_last_24h": stats.messages_last_24h,
            "oldest_message": stats.oldest_message,
            "newest_message": stats.newest_message,
            "top_nodes": stats.top_nodes.iter().map(|n| serde_json::json!({
                "node_id": format!("!{:08x}", n.node_id),
                "node_name": n.node_name,
                "message_count": n.message_count,
                "command_count": n.command_count,
            })).collect::<Vec<_>>(),
        });
        println!("{}", serde_json::to_string_pretty(&json)?);
    } else {
        println!("{}", "Mesh Message Statistics".cyan().bold());
        println!("{}", "─".repeat(50));
        println!();

        // Overview
        println!("  {} {}", "Total Messages:".bold(), stats.total_messages);
        println!("  {} {} ({:.1}%)",
            "Commands:".bold(),
            stats.total_commands,
            if stats.total_messages > 0 {
                (stats.total_commands as f64 / stats.total_messages as f64) * 100.0
            } else { 0.0 }
        );
        println!("  {} {}", "With Responses:".bold(), stats.total_responses);
        println!("  {} {}", "Unique Nodes:".bold(), stats.unique_nodes);
        println!();

        // Activity
        println!("{}", "Activity".cyan().bold());
        println!("  {} {}", "Last Hour:".bold(), stats.messages_last_hour);
        println!("  {} {}", "Last 24h:".bold(), stats.messages_last_24h);

        // Time range
        if let (Some(oldest), Some(newest)) = (stats.oldest_message, stats.newest_message) {
            let oldest_dt = chrono::DateTime::from_timestamp_micros(oldest)
                .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                .unwrap_or_else(|| "N/A".to_string());
            let newest_dt = chrono::DateTime::from_timestamp_micros(newest)
                .map(|dt| dt.format("%Y-%m-%d %H:%M").to_string())
                .unwrap_or_else(|| "N/A".to_string());

            println!();
            println!("{}", "Time Range".cyan().bold());
            println!("  {} {}", "First:".bold(), oldest_dt.dimmed());
            println!("  {} {}", "Latest:".bold(), newest_dt.dimmed());
        }

        // Top nodes
        if !stats.top_nodes.is_empty() {
            println!();
            println!("{}", "Top Nodes".cyan().bold());
            for (i, node) in stats.top_nodes.iter().take(5).enumerate() {
                let name = node.node_name.as_deref().unwrap_or("Unknown");
                let node_id = format!("!{:08x}", node.node_id);
                println!("  {}. {} {} ({} msgs, {} cmds)",
                    i + 1,
                    node_id.yellow(),
                    name,
                    node.message_count,
                    node.command_count
                );
            }
        }

        // Hourly activity
        if show_hourly {
            println!();
            println!("{}", "Hourly Activity (Last 24h)".cyan().bold());

            let hourly = db::mesh_messages::get_hourly_activity(&mut conn)
                .unwrap_or_default();

            if hourly.is_empty() {
                println!("  {} No activity in the last 24 hours", "─".dimmed());
            } else {
                // ASCII bar chart
                let max_count = hourly.iter().map(|(_, c)| *c).max().unwrap_or(1);
                for (ts, count) in hourly.iter() {
                    let dt = chrono::DateTime::from_timestamp_micros(*ts)
                        .map(|dt| dt.format("%H:00").to_string())
                        .unwrap_or_else(|| "??:??".to_string());

                    let bar_len = if max_count > 0 {
                        (*count as f64 / max_count as f64 * 30.0) as usize
                    } else { 0 };
                    let bar = "█".repeat(bar_len);

                    println!("  {} {} {} ({})", dt.dimmed(), bar.green(), " ".repeat(30 - bar_len), count);
                }
            }
        }
    }

    Ok(())
}

/// Show recent mesh messages
async fn handle_mesh_recent(matches: &ArgMatches) -> Result<()> {
    let limit: i64 = matches.get_one::<String>("limit")
        .and_then(|s| s.parse().ok())
        .unwrap_or(20);
    let commands_only = matches.get_flag("commands");
    let node_filter = matches.get_one::<String>("node");
    let json_output = matches.get_flag("json");

    let mut conn = db_err(db::establish_connection())?;

    let messages = if commands_only {
        db::mesh_messages::get_commands(&mut conn, limit)
    } else if let Some(node_id_str) = node_filter {
        // Parse node ID like "!abcd1234"
        let node_id = crate::utils::bbs::hex_id_to_num(node_id_str)
            .ok_or_else(|| anyhow::anyhow!("Invalid node ID: {}", node_id_str))?;
        db::mesh_messages::get_from_node(&mut conn, node_id, limit)
    } else {
        db::mesh_messages::get_recent(&mut conn, limit)
    }.map_err(|e| anyhow::anyhow!("Failed to get messages: {}", e))?;

    if json_output {
        let json: Vec<_> = messages.iter().map(|m| serde_json::json!({
            "id": m.id,
            "from_node_id": format!("!{:08x}", m.from_node_id as u32),
            "from_name": m.from_name,
            "body": m.body,
            "is_command": m.is_command,
            "received_at": m.received_at_us,
            "response": m.response,
        })).collect();
        println!("{}", serde_json::to_string_pretty(&json)?);
    } else {
        println!("{}", "Recent Mesh Messages".cyan().bold());
        println!("{}", "─".repeat(60));

        if messages.is_empty() {
            println!("  {} No messages found", "─".dimmed());
        } else {
            for msg in messages.iter() {
                let node_id = format!("!{:08x}", msg.from_node_id as u32);
                let name = msg.from_name.as_deref().unwrap_or("Unknown");
                let time = chrono::DateTime::from_timestamp_micros(msg.received_at_us)
                    .map(|dt| dt.format("%m-%d %H:%M").to_string())
                    .unwrap_or_else(|| "??-?? ??:??".to_string());

                let cmd_marker = if msg.is_command { "📡" } else { "  " };

                println!();
                println!("  {} {} {} ({})", cmd_marker, node_id.yellow(), name.cyan(), time.dimmed());
                println!("     {}", msg.body);

                if let Some(ref response) = msg.response {
                    println!("     {} {}", "→".green(), response.dimmed());
                }
            }
        }
    }

    Ok(())
}

/// List mesh nodes by activity
async fn handle_mesh_nodes(matches: &ArgMatches) -> Result<()> {
    let limit: i64 = matches.get_one::<String>("limit")
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let json_output = matches.get_flag("json");

    let mut conn = db_err(db::establish_connection())?;

    let nodes = db::mesh_messages::get_top_nodes(&mut conn, limit)
        .map_err(|e| anyhow::anyhow!("Failed to get nodes: {}", e))?;

    if json_output {
        let json: Vec<_> = nodes.iter().map(|n| serde_json::json!({
            "node_id": format!("!{:08x}", n.node_id),
            "node_name": n.node_name,
            "message_count": n.message_count,
            "command_count": n.command_count,
        })).collect();
        println!("{}", serde_json::to_string_pretty(&json)?);
    } else {
        println!("{}", "Mesh Nodes by Activity".cyan().bold());
        println!("{}", "─".repeat(60));
        println!();
        println!("  {:^4} {:^12} {:^20} {:>8} {:>8}", "#", "Node ID", "Name", "Msgs", "Cmds");
        println!("  {} {} {} {} {}", "─".repeat(4), "─".repeat(12), "─".repeat(20), "─".repeat(8), "─".repeat(8));

        if nodes.is_empty() {
            println!("  {} No nodes found", "─".dimmed());
        } else {
            for (i, node) in nodes.iter().enumerate() {
                let node_id = format!("!{:08x}", node.node_id);
                let name = node.node_name.as_deref().unwrap_or("Unknown");
                let name_truncated = if name.len() > 20 { format!("{}...", &name[..17]) } else { name.to_string() };

                println!("  {:>4} {} {:20} {:>8} {:>8}",
                    i + 1,
                    node_id.yellow(),
                    name_truncated.cyan(),
                    node.message_count,
                    node.command_count
                );
            }
        }
    }

    Ok(())
}

/// Prune old mesh messages
async fn handle_mesh_prune(matches: &ArgMatches) -> Result<()> {
    let keep_count: i64 = matches.get_one::<String>("keep")
        .and_then(|s| s.parse().ok())
        .unwrap_or(1000);
    let force = matches.get_flag("force");

    let mut conn = db_err(db::establish_connection())?;

    // Get current count
    let current_count = db::mesh_messages::count(&mut conn)
        .map_err(|e| anyhow::anyhow!("Failed to count messages: {}", e))?;

    if current_count <= keep_count {
        println!("{} Only {} messages in database (keeping {}), nothing to prune.",
            "ℹ".cyan(), current_count, keep_count);
        return Ok(());
    }

    let to_delete = current_count - keep_count;

    if !force {
        println!("{}", "Mesh Message Pruning".yellow().bold());
        println!("{}", "─".repeat(40));
        println!("  Current messages: {}", current_count);
        println!("  Keeping: {}", keep_count);
        println!("  {} {} messages", "Will delete:".red(), to_delete);
        println!();
        print!("Continue? [y/N] ");
        use std::io::Write;
        std::io::stdout().flush()?;

        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;
        if !input.trim().eq_ignore_ascii_case("y") {
            println!("{}", "Cancelled.".yellow());
            return Ok(());
        }
    }

    let deleted = db::mesh_messages::prune_old(&mut conn, keep_count)
        .map_err(|e| anyhow::anyhow!("Failed to prune messages: {}", e))?;

    println!("{} Deleted {} old messages. {} remaining.",
        "✓".green(), deleted, current_count - deleted as i64);

    Ok(())
}
