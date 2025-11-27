//! Collaborative Investigation command definitions
//!
//! Real-time collaborative blockchain investigation with shared TUI sessions,
//! user presence, and annotations.

use clap::{Arg, ArgAction, Command};

pub fn build_collab_command() -> Command {
    Command::new("collab")
        .about("Real-time collaborative blockchain investigation")
        .long_about(
            "Collaborative Investigation System for team-based blockchain forensics.\n\
            \n\
            Enables multiple investigators to share a TUI session in real-time,\n\
            see each other's cursors, and add annotations to wallets and transactions.\n\
            \n\
            Features:\n\
            • Shared tmux sessions - multiple terminals, same view\n\
            • WebSocket bridge - browser-based participants\n\
            • User presence - see who's connected and their cursors\n\
            • Shared annotations - notes visible to all participants\n\
            • Invite codes - easy session sharing\n\
            \n\
            Quick Start:\n\
            • osvm collab start --wallet 5Q544f...       # Start investigation\n\
            • osvm collab join ABC123                    # Join with invite code\n\
            • osvm collab annotate <WALLET> \"Note\"       # Add annotation\n\
            • osvm collab server                         # Start WebSocket server",
        )
        .arg_required_else_help(true)
        // Start a new session
        .subcommand(
            Command::new("start")
                .about("Start a new collaborative investigation session")
                .arg(
                    Arg::new("name")
                        .long("name")
                        .short('n')
                        .value_name("NAME")
                        .help("Session name (e.g., \"Hack Investigation\")"),
                )
                .arg(
                    Arg::new("wallet")
                        .long("wallet")
                        .short('w')
                        .value_name("ADDRESS")
                        .help("Target wallet address to investigate"),
                )
                .arg(
                    Arg::new("max")
                        .long("max-participants")
                        .short('m')
                        .value_name("NUM")
                        .help("Maximum number of participants (default: 10)"),
                )
                .arg(
                    Arg::new("password")
                        .long("password")
                        .short('P')
                        .value_name("PASS")
                        .help("Password to protect the session"),
                ),
        )
        // Join an existing session
        .subcommand(
            Command::new("join")
                .about("Join an existing collaborative session")
                .arg(
                    Arg::new("code")
                        .value_name("INVITE_CODE")
                        .help("6-character invite code (e.g., ABC123)")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("name")
                        .long("name")
                        .short('n')
                        .value_name("NAME")
                        .help("Your display name"),
                ),
        )
        // List active sessions
        .subcommand(
            Command::new("list")
                .about("List active collaborative sessions")
                .visible_alias("ls"),
        )
        // Add annotation
        .subcommand(
            Command::new("annotate")
                .about("Add an annotation to a wallet or transaction")
                .visible_alias("note")
                .arg(
                    Arg::new("target")
                        .value_name("TARGET")
                        .help("Wallet address or transaction signature")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("text")
                        .value_name("TEXT")
                        .help("Annotation text")
                        .required(true)
                        .index(2),
                )
                .arg(
                    Arg::new("severity")
                        .long("severity")
                        .short('s')
                        .value_name("LEVEL")
                        .value_parser(["info", "important", "warning", "critical", "question"])
                        .help("Severity level (info, important, warning, critical, question)"),
                ),
        )
        // List annotations
        .subcommand(
            Command::new("annotations")
                .about("List all annotations in the session"),
        )
        // Start WebSocket server
        .subcommand(
            Command::new("server")
                .about("Start WebSocket server for browser-based participants")
                .arg(
                    Arg::new("port")
                        .long("port")
                        .short('p')
                        .value_name("PORT")
                        .default_value("8080")
                        .help("Port to listen on"),
                )
                .arg(
                    Arg::new("host")
                        .long("host")
                        .value_name("HOST")
                        .default_value("0.0.0.0")
                        .help("Host to bind to"),
                ),
        )
        // Federation: Peer management
        .subcommand(
            Command::new("peers")
                .about("Manage federation peers")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("add")
                        .about("Add a federation peer")
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
                        .about("Remove a federation peer")
                        .arg(
                            Arg::new("address")
                                .value_name("ADDRESS")
                                .help("Peer address to remove")
                                .required(true)
                                .index(1),
                        ),
                )
                .subcommand(
                    Command::new("list")
                        .about("List configured federation peers"),
                ),
        )
        // Federation: Discover sessions
        .subcommand(
            Command::new("discover")
                .about("Discover collaborative sessions from federated peers"),
        )
        // Federation: Publish session
        .subcommand(
            Command::new("publish")
                .about("Publish local session to the federation network")
                .arg(
                    Arg::new("session_id")
                        .long("session")
                        .value_name("ID")
                        .help("Session ID to publish (default: current session)"),
                ),
        )
        // Federation: Status
        .subcommand(
            Command::new("status")
                .about("Show federation network status"),
        )
        // Help
        .subcommand(
            Command::new("help")
                .about("Show help for collab commands"),
        )
}
