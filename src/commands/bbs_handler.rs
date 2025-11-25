//! BBS (Bulletin Board System) command handler
//!
//! Handles CLI commands for the Meshtastic BBS system, providing
//! agent-human communication over off-grid radio networks.

use anyhow::{anyhow, Result};
use clap::ArgMatches;
use colored::*;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::utils::bbs::{db, meshtastic, http_server, federation, num_id_to_hex};

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
        Some(("server", sub_m)) => handle_server(sub_m).await,
        Some(("interactive", sub_m)) => handle_interactive(sub_m).await,
        Some(("stats", sub_m)) => handle_stats(sub_m).await,
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
    println!("\n{} Interactive mode requires a terminal UI.", "!".yellow());
    println!("  Use 'osvm research <wallet> --tui' and press '5' for BBS tab.");
    println!("  Or use individual commands:");
    println!("    osvm bbs read {}", board);
    println!("    osvm bbs post {} \"message\"", board);
    println!("    osvm bbs reply <id> \"reply\"", );

    Ok(())
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
