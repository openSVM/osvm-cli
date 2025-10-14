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
                            "validator", "rpc", "all",
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
        .subcommand(
            Command::new("dashboard").about("Launch interactive node monitoring dashboard"),
        )
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
                            "validator", "rpc",
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
}
