use clap::{Arg, ArgAction, Command};

/// Build the RPC management command
pub fn build_rpc_command() -> Command {
    Command::new("rpc")
        .about("Manage RPC nodes (local/remote)")
        .arg_required_else_help(true)
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
}
