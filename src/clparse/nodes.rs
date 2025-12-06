use clap::{Arg, ArgAction, Command};

/// Build the nodes management command
pub fn build_nodes_command() -> Command {
    Command::new("nodes")
        .about("Manage validator and RPC nodes")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("list")
                .about("List all nodes")
                .arg(
                    Arg::new("svm")
                        .long("svm")
                        .value_name("SVM_NAME")
                        .help("Filter nodes by SVM"),
                )
                .arg(
                    Arg::new("type")
                        .long("type")
                        .value_name("NODE_TYPE")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "validator",
                            "rpc",
                            "all",
                        ]))
                        .default_value("all")
                        .help("Filter nodes by type"),
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "mainnet", "testnet", "devnet", "all",
                        ]))
                        .default_value("all")
                        .help("Filter nodes by network"),
                )
                .arg(
                    Arg::new("status")
                        .long("status")
                        .value_name("STATUS")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "running", "stopped", "error", "unknown", "all",
                        ]))
                        .default_value("all")
                        .help("Filter nodes by status"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output as JSON"),
                ),
        )
        .subcommand(Command::new("dashboard").about("Launch interactive node monitoring dashboard"))
        .subcommand(
            Command::new("status")
                .about("Check status of nodes")
                .arg(
                    Arg::new("node-id")
                        .value_name("NODE_ID")
                        .index(1)
                        .required(true)
                        .help("ID of the node to check"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output as JSON"),
                ),
        )
        .subcommand(
            Command::new("get")
                .about("Get detailed information about a specific node")
                .arg(
                    Arg::new("node-id")
                        .value_name("NODE_ID")
                        .index(1)
                        .required(true)
                        .help("ID of the node to get information about"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output as JSON"),
                ),
        )
        .subcommand(
            Command::new("restart")
                .about("🚧 COMING SOON: Restart a node")
                .arg(
                    Arg::new("node-id")
                        .value_name("NODE_ID")
                        .index(1)
                        .required(true)
                        .help("ID of the node to restart"),
                ),
        )
        .subcommand(
            Command::new("stop")
                .about("🚧 COMING SOON: Stop a node")
                .arg(
                    Arg::new("node-id")
                        .value_name("NODE_ID")
                        .index(1)
                        .required(true)
                        .help("ID of the node to stop"),
                ),
        )
        .subcommand(
            Command::new("logs")
                .about("View logs from a node")
                .arg(
                    Arg::new("node-id")
                        .value_name("NODE_ID")
                        .index(1)
                        .required(true)
                        .help("ID of the node to get logs from"),
                )
                .arg(
                    Arg::new("lines")
                        .long("lines")
                        .short('n')
                        .value_name("LINES")
                        .default_value("100")
                        .help("Number of lines to show"),
                )
                .arg(
                    Arg::new("follow")
                        .long("follow")
                        .short('f')
                        .action(ArgAction::SetTrue)
                        .help("Follow log output"),
                ),
        )
        .subcommand(
            Command::new("deploy")
                .about("Deploy a new node")
                .arg(
                    Arg::new("svm")
                        .long("svm")
                        .value_name("SVM_NAME")
                        .required(true)
                        .help("SVM to deploy node for"),
                )
                .arg(
                    Arg::new("type")
                        .long("type")
                        .value_name("NODE_TYPE")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "validator",
                            "rpc",
                        ]))
                        .default_value("validator")
                        .help("Type of node to deploy"),
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
                    Arg::new("host")
                        .long("host")
                        .value_name("HOST")
                        .required(true)
                        .help("Remote host to deploy on (format: user@host[:port])"),
                )
                .arg(
                    Arg::new("name")
                        .long("name")
                        .value_name("NAME")
                        .help("Custom name for the node (default: auto-generated)"),
                ),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm nodes list
     List all configured nodes with status.
     💡 Shows: ID, type, network, status, version.

  2. osvm nodes dashboard
     Launch interactive node monitoring dashboard.
     💡 Real-time TUI with health metrics and alerts.

  3. osvm nodes list --type validator --network mainnet
     Filter to mainnet validators only.
     💡 Combine filters to narrow down results.

  4. osvm nodes status node-001
     Check detailed status of specific node.
     💡 Shows: slot, health, peers, vote account.

  5. osvm nodes get node-001 --json
     Get node info in JSON format.
     💡 Useful for scripting and automation.

  6. osvm nodes logs node-001 -f
     Follow node logs in real-time.
     💡 Like 'tail -f' for your validator.

  7. osvm nodes logs node-001 -n 500
     Show last 500 log lines.
     💡 Increase from default 100 for more context.

  8. osvm nodes deploy --svm agave --type validator \
       --network mainnet --host user@server.com
     Deploy new validator to remote server.
     💡 Automatically configures and starts the node.

  9. osvm nodes list --status running | grep mainnet
     Find all running mainnet nodes.
     💡 Chain with grep for quick filtering.

 10. watch -n 5 osvm nodes status node-001
     Monitor node status every 5 seconds.
     💡 Quick way to watch node health.

💡 NODE TYPES:
  Validator:
  • Participates in consensus
  • Earns staking rewards
  • Requires vote account
  • Higher resource requirements

  RPC:
  • Serves API requests
  • No voting/staking
  • Good for dApps backend
  • Lower resource needs

STATUS MEANINGS:
  🟢 running:  Node is operational and synced
  🟡 syncing:  Node is catching up to tip
  🔴 stopped:  Node is not running
  ⚠️ error:    Node has issues (check logs)
  ❓ unknown:  Cannot determine status

COMMON ISSUES:
  • "Behind" → Node falling behind on slots
  • "No peers" → Network connectivity issues
  • "Vote failed" → Check vote account balance
"#)
}
