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
}
