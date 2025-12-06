use clap::{Arg, ArgAction, Command};

/// Build the RPC management command
pub fn build_rpc_command() -> Command {
    Command::new("rpc")
        .about("Manage RPC nodes (local/remote)")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("solana")
                .about("Deploy a Solana RPC node via SSH")
                .arg(
                    Arg::new("connection")
                        .help("SSH connection string (format: user@host[:port])")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "mainnet", "testnet", "devnet",
                        ]))
                        .default_value("mainnet")
                        .help("Network to deploy on"),
                )
                .arg(
                    Arg::new("version")
                        .long("version")
                        .short('v')
                        .value_name("VERSION")
                        .help("Solana version to install (e.g., '1.18.0', 'stable', 'beta')"),
                )
                .arg(
                    Arg::new("client-type")
                        .long("client-type")
                        .value_name("TYPE")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "agave", "firedancer", "jito"
                        ]))
                        .help("Validator client type to use (default: agave)"),
                )
                .arg(
                    Arg::new("hot-swap")
                        .long("hot-swap")
                        .action(ArgAction::SetTrue)
                        .help("Enable zero-downtime hot-swap capability"),
                ),
        )
        .subcommand(
            Command::new("sonic")
                .about("Deploy a Sonic RPC node")
                .arg(
                    Arg::new("connection")
                        .help("SSH connection string (format: user@host[:port])")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "mainnet", "testnet", "devnet",
                        ]))
                        .default_value("mainnet")
                        .help("Network to deploy on"),
                ),
        )
        .subcommand(
            Command::new("query-solana")
                .about("Query Solana RPC endpoint (info, health, monitor)")
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "mainnet", "testnet", "devnet",
                        ]))
                        .default_value("mainnet")
                        .help("Solana network to query"),
                )
                .arg(
                    Arg::new("custom-url")
                        .long("custom-url")
                        .value_name("URL")
                        .help("Custom RPC URL to query"),
                )
                .arg(
                    Arg::new("monitor")
                        .long("monitor")
                        .action(ArgAction::SetTrue)
                        .help("Monitor network activity in real-time"),
                )
                .arg(
                    Arg::new("health")
                        .long("health")
                        .action(ArgAction::SetTrue)
                        .help("Check network health"),
                )
                .arg(
                    Arg::new("info")
                        .long("info")
                        .action(ArgAction::SetTrue)
                        .help("Show network information (default if no other flag)"),
                ),
        )
        .subcommand(
            Command::new("local")
                .about("Deploy a local RPC node on localhost")
                .arg(
                    Arg::new("svm")
                        .long("svm")
                        .value_name("SVM_NAME")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "solana", "sonic", "eclipse", "soon", "opensvm",
                        ]))
                        .default_value("solana")
                        .help("SVM to deploy RPC for"),
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "mainnet", "testnet", "devnet",
                        ]))
                        .default_value("devnet")
                        .help("Network to deploy on"),
                )
                .arg(
                    Arg::new("port")
                        .long("port")
                        .value_name("PORT")
                        .default_value("8899")
                        .help("RPC port to bind to (default: 8899)"),
                )
                .arg(
                    Arg::new("faucet-port")
                        .long("faucet-port")
                        .value_name("FAUCET_PORT")
                        .default_value("9900")
                        .help("Faucet port for devnet (default: 9900)"),
                )
                .arg(
                    Arg::new("ledger-path")
                        .long("ledger-path")
                        .value_name("PATH")
                        .help("Ledger data directory path (default: ~/.config/osvm/ledgers/{network})"),
                )
                .arg(
                    Arg::new("reset")
                        .long("reset")
                        .action(ArgAction::SetTrue)
                        .help("Reset the ledger on startup"),
                )
                .arg(
                    Arg::new("background")
                        .long("background")
                        .short('d')
                        .action(ArgAction::SetTrue)
                        .help("Run in background (daemon mode)"),
                )
                .arg(
                    Arg::new("stop")
                        .long("stop")
                        .action(ArgAction::SetTrue)
                        .help("Stop running local RPC node"),
                )
                .arg(
                    Arg::new("status")
                        .long("status")
                        .action(ArgAction::SetTrue)
                        .help("Check status of local RPC node"),
                ),
        )
        .subcommand(
            Command::new("test")
                .about("Start a local test validator with RPC for development")
                .arg(
                    Arg::new("ledger-path")
                        .long("ledger-path")
                        .value_name("PATH")
                        .default_value("test-ledger")
                        .help("Ledger data directory path"),
                )
                .arg(
                    Arg::new("rpc-port")
                        .long("rpc-port")
                        .value_name("PORT")
                        .default_value("8899")
                        .help("RPC port to bind to"),
                )
                .arg(
                    Arg::new("faucet-port")
                        .long("faucet-port")
                        .value_name("PORT")
                        .default_value("9900")
                        .help("Faucet port for SOL airdrops"),
                )
                .arg(
                    Arg::new("reset")
                        .long("reset")
                        .action(ArgAction::SetTrue)
                        .help("Reset the ledger on startup"),
                )
                .arg(
                    Arg::new("background")
                        .long("background")
                        .short('d')
                        .action(ArgAction::SetTrue)
                        .help("Run in background (daemon mode)"),
                )
                .arg(
                    Arg::new("stop")
                        .long("stop")
                        .action(ArgAction::SetTrue)
                        .help("Stop running test validator"),
                )
                .arg(
                    Arg::new("status")
                        .long("status")
                        .action(ArgAction::SetTrue)
                        .help("Check status of test validator"),
                )
                .arg(
                    Arg::new("logs")
                        .long("logs")
                        .action(ArgAction::SetTrue)
                        .help("Show recent logs from test validator"),
                )
                .arg(
                    Arg::new("quiet")
                        .long("quiet")
                        .short('q')
                        .action(ArgAction::SetTrue)
                        .help("Suppress non-essential output"),
                ),
        )
        .subcommand(
            Command::new("devnet")
                .about("Start a legitimate devnet RPC node that syncs with Solana devnet")
                .arg(
                    Arg::new("ledger-path")
                        .long("ledger-path")
                        .value_name("PATH")
                        .default_value("devnet-ledger")
                        .help("Ledger data directory path for devnet sync"),
                )
                .arg(
                    Arg::new("rpc-port")
                        .long("rpc-port")
                        .value_name("PORT")
                        .default_value("8899")
                        .help("RPC port to bind to"),
                )
                .arg(
                    Arg::new("background")
                        .long("background")
                        .short('d')
                        .action(ArgAction::SetTrue)
                        .help("Run in background (daemon mode)"),
                )
                .arg(
                    Arg::new("stop")
                        .long("stop")
                        .action(ArgAction::SetTrue)
                        .help("Stop running devnet RPC node"),
                )
                .arg(
                    Arg::new("status")
                        .long("status")
                        .action(ArgAction::SetTrue)
                        .help("Check status of devnet RPC node"),
                )
                .arg(
                    Arg::new("logs")
                        .long("logs")
                        .action(ArgAction::SetTrue)
                        .help("Show recent logs from devnet RPC node"),
                )
                .arg(
                    Arg::new("lines")
                        .long("lines")
                        .short('n')
                        .value_name("LINES")
                        .default_value("50")
                        .help("Number of recent log lines to show (used with --logs)"),
                )
                .arg(
                    Arg::new("follow")
                        .long("follow")
                        .short('f')
                        .action(ArgAction::SetTrue)
                        .help("Follow log output in real-time (used with --logs)"),
                ),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm rpc test
     Start a local test validator for development.
     💡 Isolated environment - perfect for smart contract testing.

  2. osvm rpc test --reset
     Start fresh with a clean ledger.
     💡 Use when you want a clean slate for testing.

  3. osvm rpc query-solana --health
     Check mainnet RPC endpoint health.
     💡 Returns: "ok" if healthy, error otherwise.

  4. osvm rpc query-solana --monitor
     Monitor network activity in real-time.
     💡 Shows TPS, slot height, and network metrics.

  5. osvm rpc local --svm solana --network devnet
     Run local devnet RPC on your machine.
     💡 Syncs with public devnet network.

  6. osvm rpc solana user@server.com --network mainnet
     Deploy mainnet RPC to remote server via SSH.
     💡 Full mainnet node - requires significant resources.

  7. osvm rpc devnet -d
     Run devnet RPC in background (daemon mode).
     💡 Use --status to check, --stop to terminate.

  8. osvm rpc test --logs
     Show recent test validator logs.
     💡 Useful for debugging transaction failures.

  9. osvm rpc query-solana --custom-url https://my-rpc.com
     Test custom RPC endpoint.
     💡 Verify your own or third-party RPC node.

 10. osvm rpc local --port 9999 --faucet-port 9901
     Run RPC on custom ports.
     💡 Useful when default ports are in use.

💡 RPC NODE TYPES:
  Test Validator:
  • Isolated local chain (not connected to any network)
  • Instant transaction confirmation
  • Built-in faucet for unlimited devnet SOL
  • Perfect for development and testing

  Devnet RPC:
  • Syncs with Solana devnet
  • Real network behavior
  • Shares state with other devnet users
  • Good for integration testing

  Mainnet RPC:
  • Full production network
  • Requires significant storage (~2TB)
  • High bandwidth requirements
  • Use for production workloads

COMMON PORTS:
  • 8899: Default RPC port
  • 8900: Default websocket port
  • 9900: Default faucet port
"#)
}
