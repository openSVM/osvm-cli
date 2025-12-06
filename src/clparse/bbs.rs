//! BBS (Bulletin Board System) command definitions for Meshtastic agent-human communication

use clap::{Arg, ArgAction, Command};

pub fn build_bbs_command() -> Command {
    Command::new("bbs")
        .about("Meshtastic BBS for agent-human off-grid communication")
        .long_about(
            "Meshtastic Bulletin Board System (BBS) for decentralized agent-human communication.\n\
            \n\
            The BBS provides a text-based interface for agents and humans to communicate\n\
            over Meshtastic radio networks, enabling off-grid blockchain operations and\n\
            investigations even without internet connectivity.\n\
            \n\
            Features:\n\
            • Multi-board message organization (GENERAL, ALERTS, TRADES, etc.)\n\
            • Agent registration and identity management\n\
            • Meshtastic radio integration (TCP/serial)\n\
            • Offline-first design with SQLite persistence\n\
            • Command-driven interface compatible with radio text limits\n\
            \n\
            Quick Start:\n\
            • osvm bbs init                    # Initialize BBS database\n\
            • osvm bbs boards list             # List available boards\n\
            • osvm bbs post GENERAL \"Hello\"    # Post a message\n\
            • osvm bbs read GENERAL            # Read board messages\n\
            • osvm bbs radio connect           # Connect to Meshtastic radio",
        )
        .arg_required_else_help(true)
        // Database initialization
        .subcommand(
            Command::new("init")
                .about("Initialize BBS database and default boards")
                .arg(
                    Arg::new("reset")
                        .long("reset")
                        .action(ArgAction::SetTrue)
                        .help("Reset database (WARNING: destroys all data)"),
                )
                .arg(
                    Arg::new("path")
                        .long("path")
                        .value_name("PATH")
                        .help("Custom database path (default: ~/.osvm/bbs.db)"),
                ),
        )
        // Board management
        .subcommand(
            Command::new("boards")
                .about("Manage BBS boards")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("list")
                        .about("List all available boards")
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        ),
                )
                .subcommand(
                    Command::new("create")
                        .about("Create a new board")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Board name (uppercase, no spaces)")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("description")
                                .value_name("DESCRIPTION")
                                .help("Board description")
                                .required(true)
                                .index(2),
                        )
                        .arg(
                            Arg::new("private")
                                .long("private")
                                .action(ArgAction::SetTrue)
                                .help("Create as private board"),
                        ),
                )
                .subcommand(
                    Command::new("delete")
                        .about("Delete a board")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Board name to delete")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("force")
                                .long("force")
                                .short('f')
                                .action(ArgAction::SetTrue)
                                .help("Skip confirmation prompt"),
                        ),
                ),
        )
        // Message operations
        .subcommand(
            Command::new("post")
                .about("Post a message to a board")
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Target board name")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("message")
                        .value_name("MESSAGE")
                        .help("Message content")
                        .required(true)
                        .index(2),
                )
                .arg(
                    Arg::new("title")
                        .long("title")
                        .short('t')
                        .value_name("TITLE")
                        .help("Optional message title"),
                )
                .arg(
                    Arg::new("as-agent")
                        .long("as-agent")
                        .value_name("AGENT_ID")
                        .help("Post as registered agent"),
                ),
        )
        .subcommand(
            Command::new("read")
                .about("Read messages from a board")
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Board name to read")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("limit")
                        .long("limit")
                        .short('n')
                        .value_name("COUNT")
                        .default_value("20")
                        .help("Number of messages to show"),
                )
                .arg(
                    Arg::new("since")
                        .long("since")
                        .value_name("TIMESTAMP")
                        .help("Show messages since timestamp (epoch seconds)"),
                )
                .arg(
                    Arg::new("follow")
                        .long("follow")
                        .short('f')
                        .action(ArgAction::SetTrue)
                        .help("Follow mode - wait for new messages"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        .subcommand(
            Command::new("reply")
                .about("Reply to a specific message")
                .arg(
                    Arg::new("message_id")
                        .value_name("MESSAGE_ID")
                        .help("ID of message to reply to")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("content")
                        .value_name("CONTENT")
                        .help("Reply content")
                        .required(true)
                        .index(2),
                ),
        )
        // Agent management
        .subcommand(
            Command::new("agent")
                .about("Manage BBS agents")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("register")
                        .about("Register a new agent identity")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Agent display name")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("capabilities")
                                .long("capabilities")
                                .short('c')
                                .value_name("CAPS")
                                .help("Comma-separated capabilities (e.g., research,trade,monitor)"),
                        ),
                )
                .subcommand(Command::new("list").about("List registered agents"))
                .subcommand(
                    Command::new("status")
                        .about("Show agent status")
                        .arg(
                            Arg::new("agent_id")
                                .value_name("AGENT_ID")
                                .help("Agent ID (or 'self' for current)")
                                .default_value("self")
                                .index(1),
                        ),
                ),
        )
        // Radio connection (Meshtastic)
        .subcommand(
            Command::new("radio")
                .about("Manage Meshtastic radio connection")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("connect")
                        .about("Connect to Meshtastic radio")
                        .arg(
                            Arg::new("address")
                                .value_name("ADDRESS")
                                .help("Radio address (e.g., 192.168.1.100:4403 or /dev/ttyUSB0)")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("tcp")
                                .long("tcp")
                                .action(ArgAction::SetTrue)
                                .help("Use TCP connection (default for IP addresses)"),
                        )
                        .arg(
                            Arg::new("serial")
                                .long("serial")
                                .action(ArgAction::SetTrue)
                                .help("Use serial connection (default for /dev paths)"),
                        ),
                )
                .subcommand(Command::new("disconnect").about("Disconnect from radio"))
                .subcommand(Command::new("status").about("Show radio connection status"))
                .subcommand(
                    Command::new("send")
                        .about("Send raw message via radio")
                        .arg(
                            Arg::new("message")
                                .value_name("MESSAGE")
                                .help("Message to send (max 228 bytes)")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("to")
                                .long("to")
                                .value_name("NODE_ID")
                                .help("Target node ID (broadcast if not specified)"),
                        ),
                ),
        )
        // Interactive mode
        .subcommand(
            Command::new("interactive")
                .about("Start interactive BBS shell")
                .alias("shell")
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Initial board to open")
                        .default_value("GENERAL")
                        .index(1),
                ),
        )
        // Full-screen TUI mode
        .subcommand(
            Command::new("tui")
                .about("Start full-screen TUI interface")
                .long_about(
                    "Launch a full-screen terminal user interface for the BBS.\n\
                    \n\
                    The TUI provides a rich graphical interface with:\n\
                    • Board list with quick-switch (1-9 keys)\n\
                    • Scrollable posts view (j/k or arrow keys)\n\
                    • Vim-style input mode (press 'i' to type, Enter to send, Esc to cancel)\n\
                    • Real-time status updates\n\
                    \n\
                    Keyboard shortcuts:\n\
                    • i       - Enter input mode to compose a message\n\
                    • Enter   - Send message (in input mode)\n\
                    • Esc     - Cancel input / Exit TUI\n\
                    • j/k     - Scroll posts up/down\n\
                    • 1-9     - Quick-switch boards\n\
                    • r       - Refresh posts\n\
                    • q       - Quit TUI",
                )
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Initial board to open")
                        .default_value("GENERAL")
                        .index(1),
                )
                .arg(
                    Arg::new("mesh")
                        .long("mesh")
                        .short('m')
                        .value_name("ADDRESS")
                        .help("Connect to Meshtastic radio (e.g., 192.168.1.100:4403)")
                        .num_args(0..=1)
                        .default_missing_value("localhost:4403"),
                ),
        )
        // Threaded view (collapsible)
        .subcommand(
            Command::new("threads")
                .about("View posts as collapsible threads (interactive)")
                .long_about(
                    "Launch an interactive thread viewer with collapsible threads.\n\
                    \n\
                    Features:\n\
                    • Hierarchical view of posts and replies\n\
                    • Collapsible threads (press Enter/Space to toggle)\n\
                    • Score-based auto-collapse (low-scored posts collapse by default)\n\
                    • Color-coded depth for nested replies\n\
                    • Vim-style navigation (j/k for up/down)\n\
                    \n\
                    Keyboard shortcuts:\n\
                    • j/k/↓/↑   - Move selection up/down\n\
                    • Enter/Space - Toggle expand/collapse\n\
                    • c          - Collapse all threads\n\
                    • e          - Expand all threads\n\
                    • r          - Refresh from database\n\
                    • q/Esc      - Quit",
                )
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Board name to view")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("server")
                        .long("server")
                        .short('s')
                        .value_name("URL")
                        .help("Fetch from HTTP server instead of local database (e.g., http://localhost:8080)"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output raw JSON threads (non-interactive)"),
                ),
        )
        // Stats and info
        .subcommand(
            Command::new("stats")
                .about("Show BBS statistics")
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        // Analytics dashboard
        .subcommand(
            Command::new("analytics")
                .about("Show board analytics: vote patterns, activity trends, sentiment")
                .long_about(
                    "Display comprehensive analytics for a board including:\n\
                    \n\
                    • Vote patterns (upvotes/downvotes distribution)\n\
                    • Activity trends by hour and day\n\
                    • Sentiment analysis of posts\n\
                    • Top contributors and engagement metrics\n\
                    \n\
                    Example:\n\
                    osvm bbs analytics GENERAL --days 7"
                )
                .arg(
                    Arg::new("board")
                        .value_name("BOARD")
                        .help("Board name to analyze (default: GENERAL)")
                        .index(1),
                )
                .arg(
                    Arg::new("days")
                        .long("days")
                        .short('d')
                        .value_name("DAYS")
                        .help("Number of days to analyze (default: 7)")
                        .default_value("7"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        // Peer management (federation)
        .subcommand(
            Command::new("peers")
                .about("Manage peer nodes for federation")
                .long_about(
                    "Manage connections to other BBS nodes for message federation.\n\
                    \n\
                    Federation allows multiple BBS nodes to share messages,\n\
                    creating a decentralized bulletin board network.\n\
                    \n\
                    Discovery methods:\n\
                    • Manual: Add peers by address\n\
                    • Local: Auto-discover on LAN (mDNS)\n\
                    • Bootstrap: Query known servers for peers\n\
                    • Gossip: Peers share their peer lists",
                )
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("add")
                        .about("Add a peer node")
                        .arg(
                            Arg::new("address")
                                .value_name("ADDRESS")
                                .help("Peer address (e.g., http://192.168.1.100:8080)")
                                .required(true)
                                .index(1),
                        ),
                )
                .subcommand(
                    Command::new("remove")
                        .about("Remove a peer node")
                        .arg(
                            Arg::new("node_id")
                                .value_name("NODE_ID")
                                .help("Node ID to remove (e.g., !abcd1234)")
                                .required(true)
                                .index(1),
                        ),
                )
                .subcommand(
                    Command::new("list")
                        .about("List known peers")
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        ),
                )
                .subcommand(
                    Command::new("sync")
                        .about("Sync messages from all peers")
                        .arg(
                            Arg::new("peer")
                                .value_name("NODE_ID")
                                .help("Sync from specific peer only")
                                .index(1),
                        ),
                )
                .subcommand(
                    Command::new("discover")
                        .about("Discover peers automatically")
                        .arg(
                            Arg::new("local")
                                .long("local")
                                .action(ArgAction::SetTrue)
                                .help("Discover on local network (mDNS)"),
                        )
                        .arg(
                            Arg::new("bootstrap")
                                .long("bootstrap")
                                .action(ArgAction::SetTrue)
                                .help("Query bootstrap servers"),
                        ),
                ),
        )
        // HTTP Server (internet mode)
        .subcommand(
            Command::new("server")
                .about("Start HTTP API server for internet-based BBS access")
                .long_about(
                    "Start an HTTP server that provides REST API and WebSocket endpoints.\n\
                    Use this when Meshtastic radio is not available.\n\
                    \n\
                    Endpoints:\n\
                    • GET  /api/boards              - List all boards\n\
                    • GET  /api/boards/:name        - Get board info\n\
                    • POST /api/boards              - Create board\n\
                    • GET  /api/boards/:name/posts  - List posts\n\
                    • POST /api/boards/:name/posts  - Create post\n\
                    • POST /api/posts/:id/reply     - Reply to post\n\
                    • GET  /api/stats               - Statistics\n\
                    • WS   /ws                      - Real-time updates",
                )
                .arg(
                    Arg::new("host")
                        .long("host")
                        .short('H')
                        .value_name("HOST")
                        .default_value("0.0.0.0")
                        .help("Host address to bind to"),
                )
                .arg(
                    Arg::new("port")
                        .long("port")
                        .short('P')
                        .value_name("PORT")
                        .default_value("8080")
                        .help("Port to listen on"),
                ),
        )
        // On-chain registry (Solana devnet)
        .subcommand(
            Command::new("registry")
                .about("On-chain peer registry (Solana devnet)")
                .long_about(
                    "Decentralized peer discovery using Solana blockchain.\n\
                    \n\
                    BBS nodes can register on-chain for trustless peer discovery.\n\
                    This enables fully decentralized peer finding without central servers.\n\
                    \n\
                    Commands:\n\
                    • register  - Register this node on Solana devnet\n\
                    • list      - List all registered nodes\n\
                    • update    - Update your registration\n\
                    • heartbeat - Send heartbeat (update last_seen)\n\
                    • deregister - Remove from registry\n\
                    • discover  - Fetch peers from registry\n\
                    \n\
                    Note: Uses Solana devnet (free, just need to airdrop SOL)",
                )
                .arg(
                    Arg::new("rpc")
                        .long("rpc")
                        .value_name("URL")
                        .help("Custom RPC endpoint (default: devnet)"),
                )
                .subcommand(
                    Command::new("register")
                        .about("Register this node on-chain")
                        .arg(
                            Arg::new("address")
                                .value_name("ADDRESS")
                                .help("HTTP address for this node (e.g., http://myip:8080)")
                                .required(true)
                                .index(1),
                        )
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .help("Display name for this node")
                                .required(true)
                                .index(2),
                        )
                        .arg(
                            Arg::new("keypair")
                                .long("keypair")
                                .short('k')
                                .value_name("PATH")
                                .help("Path to Solana keypair (default: ~/.config/solana/id.json)"),
                        ),
                )
                .subcommand(
                    Command::new("list")
                        .about("List all registered nodes")
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        ),
                )
                .subcommand(
                    Command::new("update")
                        .about("Update node registration")
                        .arg(
                            Arg::new("address")
                                .long("address")
                                .short('a')
                                .value_name("ADDRESS")
                                .help("New HTTP address"),
                        )
                        .arg(
                            Arg::new("name")
                                .long("name")
                                .short('n')
                                .value_name("NAME")
                                .help("New display name"),
                        )
                        .arg(
                            Arg::new("keypair")
                                .long("keypair")
                                .short('k')
                                .value_name("PATH")
                                .help("Path to Solana keypair"),
                        ),
                )
                .subcommand(
                    Command::new("heartbeat")
                        .about("Update heartbeat timestamp")
                        .arg(
                            Arg::new("keypair")
                                .long("keypair")
                                .short('k')
                                .value_name("PATH")
                                .help("Path to Solana keypair"),
                        ),
                )
                .subcommand(
                    Command::new("deregister")
                        .about("Remove node from on-chain registry")
                        .arg(
                            Arg::new("keypair")
                                .long("keypair")
                                .short('k')
                                .value_name("PATH")
                                .help("Path to Solana keypair"),
                        )
                        .arg(
                            Arg::new("force")
                                .long("force")
                                .short('f')
                                .action(ArgAction::SetTrue)
                                .help("Skip confirmation"),
                        ),
                )
                .subcommand(
                    Command::new("discover")
                        .about("Discover peers from on-chain registry"),
                ),
        )
        // Mesh message management
        .subcommand(
            Command::new("mesh")
                .about("Meshtastic mesh message management and statistics")
                .long_about(
                    "View and manage messages received over Meshtastic radio mesh.\n\
                    \n\
                    All mesh messages are stored in the BBS database for analysis and history.\n\
                    Use this command to view statistics, recent messages, and activity patterns.\n\
                    \n\
                    Commands:\n\
                    • stats   - Show comprehensive mesh statistics\n\
                    • recent  - View recent mesh messages\n\
                    • nodes   - List active mesh nodes\n\
                    • prune   - Clean up old messages",
                )
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("stats")
                        .about("Show mesh message statistics")
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        )
                        .arg(
                            Arg::new("hourly")
                                .long("hourly")
                                .action(ArgAction::SetTrue)
                                .help("Include hourly activity breakdown"),
                        ),
                )
                .subcommand(
                    Command::new("recent")
                        .about("View recent mesh messages")
                        .arg(
                            Arg::new("limit")
                                .long("limit")
                                .short('n')
                                .value_name("COUNT")
                                .default_value("20")
                                .help("Number of messages to show"),
                        )
                        .arg(
                            Arg::new("commands")
                                .long("commands")
                                .short('c')
                                .action(ArgAction::SetTrue)
                                .help("Show only command messages"),
                        )
                        .arg(
                            Arg::new("node")
                                .long("node")
                                .value_name("NODE_ID")
                                .help("Filter by node ID (e.g., !abcd1234)"),
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        ),
                )
                .subcommand(
                    Command::new("nodes")
                        .about("List mesh nodes by activity")
                        .arg(
                            Arg::new("limit")
                                .long("limit")
                                .short('n')
                                .value_name("COUNT")
                                .default_value("10")
                                .help("Number of nodes to show"),
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format"),
                        ),
                )
                .subcommand(
                    Command::new("prune")
                        .about("Remove old mesh messages")
                        .arg(
                            Arg::new("keep")
                                .long("keep")
                                .short('k')
                                .value_name("COUNT")
                                .default_value("1000")
                                .help("Number of recent messages to keep"),
                        )
                        .arg(
                            Arg::new("force")
                                .long("force")
                                .short('f')
                                .action(ArgAction::SetTrue)
                                .help("Skip confirmation"),
                        ),
                ),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm bbs init
     Initialize BBS database with default boards.
     💡 Creates: ~/.osvm/bbs.db with GENERAL, ALERTS, TRADES boards.

  2. osvm bbs tui
     Launch full-screen TUI for the BBS.
     💡 Best way to interact - vim-style keys, real-time updates.

  3. osvm bbs post GENERAL "Hello from the mesh!"
     Post a message to the GENERAL board.
     💡 Messages are 228 bytes max for Meshtastic compatibility.

  4. osvm bbs read GENERAL -f
     Follow mode - watch for new messages.
     💡 Like tail -f for the bulletin board.

  5. osvm bbs radio connect 192.168.1.100:4403
     Connect to a Meshtastic radio over TCP.
     💡 For serial: osvm bbs radio connect /dev/ttyUSB0

  6. osvm bbs server --port 8080
     Start HTTP API server for web access.
     💡 Provides REST API and WebSocket endpoints.

  7. osvm bbs registry register http://myip:8080 "My Node"
     Register on Solana devnet for peer discovery.
     💡 Decentralized BBS node registry on-chain!

  8. osvm bbs threads GENERAL
     View posts as collapsible threads.
     💡 Enter/Space to expand/collapse discussions.

  9. osvm bbs analytics GENERAL --days 30
     Show 30-day analytics for the GENERAL board.
     💡 Activity trends, sentiment, top contributors.

 10. osvm bbs tui --mesh 192.168.1.100:4403
     Launch TUI with Meshtastic radio connected.
     💡 Full off-grid communication capability!

💡 WHAT IS MESHTASTIC?
  Meshtastic is an open-source mesh networking protocol for
  LoRa radios. It enables off-grid text communication over
  distances of several miles without internet or cell towers.

  BBS + Meshtastic = Off-grid blockchain agent communication!

USE CASES:
  • Emergency coordination when internet is down
  • Remote area blockchain operations
  • Agent-to-human communication in the field
  • Decentralized message boards without servers

KEYBOARD SHORTCUTS (TUI):
  i: Enter input mode    1-9: Switch boards
  j/k: Scroll up/down    r: Refresh
  Enter: Send message    Esc/q: Quit
"#)
}
