use {
    clap::{command, Arg, ArgAction, Command},
    solana_clap_utils::input_validators::{is_url_or_moniker, is_valid_signer},
};

fn validate_signer(s: &str) -> Result<String, String> {
    is_valid_signer(s).map(|()| s.to_string())
}

fn validate_url_or_moniker(s: &str) -> Result<String, String> {
    is_url_or_moniker(s).map(|()| s.to_string())
}

/// Construct the cli input model and parse command line
pub fn parse_command_line() -> clap::ArgMatches {
    command!()
        .arg_required_else_help(true)
        .allow_external_subcommands(true)
        // Global arguments
        .arg({
            let mut arg = Arg::new("config_file")
                .short('C')
                .long("config")
                .value_name("PATH")
                .help("Configuration file to use")
                .global(true);
            if let Some(ref config_file) = *solana_cli_config::CONFIG_FILE {
                arg = arg.default_value(config_file.as_str());
            }
            arg
        })
        .arg(
            Arg::new("keypair")
                .long("keypair")
                .value_name("KEYPAIR")
                .value_parser(validate_signer)
                .global(true)
                .help("Filepath or URL to a keypair [default: client keypair]"),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .global(true)
                .action(ArgAction::Count)
                .help("Sets the level of verbosity (-v, -vv, -vvv)"),
        )
        .arg(
            Arg::new("no_color")
                .long("no-color")
                .action(ArgAction::SetTrue)
                .global(true)
                .help("Disable colorized output (also respects NO_COLOR environment variable)"),
        )
        .arg(
            Arg::new("json_rpc_url")
                .short('u')
                .long("url")
                .value_name("URL")
                .global(true)
                .value_parser(validate_url_or_moniker)
                .help("JSON RPC URL for the cluster [default: value from configuration file]"),
        )
        .arg(
            Arg::new("svm")
                .long("svm")
                .value_name("SVM_LIST")
                .help("Comma-separated list of SVMs to install"),
        )
        .arg(
            Arg::new("node-type")
                .long("node-type")
                .value_name("TYPE")
                .value_parser(clap::builder::PossibleValuesParser::new(["validator", "rpc"]))
                .default_value("validator")
                .help("Type of node to install (validator or RPC)"),
        )
        .arg(
            Arg::new("network")
                .long("network")
                .value_name("NETWORK")
                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                .default_value("mainnet")
                .help("Network to deploy on")
        )
        .subcommand(
            Command::new("examples")
                .about("Show usage examples for OSVM CLI commands")
                .arg(
                    Arg::new("category")
                        .long("category")
                        .short('c')
                        .value_name("CATEGORY")
                        .help("Filter examples by category (basic, svm, node, monitoring, workflow)")
                )
                .arg(
                    Arg::new("list_categories")
                        .long("list-categories")
                        .action(ArgAction::SetTrue)
                        .help("List all available example categories")
                )
        )
        .subcommand(
            Command::new("svm")
                .about("Manage Solana Virtual Machines (SVMs)")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("list")
                        .about("List all SVMs installed in the chain")
                )
                .subcommand(
                    Command::new("dashboard")
                        .about("Launch interactive SVM monitoring dashboard")
                )
                .subcommand(
                    Command::new("get")
                        .about("Get detailed information about a specific SVM")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .index(1)
                                .required(true)
                                .help("Name of the SVM to get information about")
                        )
                )
                .subcommand(
                    Command::new("install")
                        .about("Install an SVM on a remote host")
                        .arg(
                            Arg::new("name")
                                .value_name("NAME")
                                .index(1)
                                .required(true)
                                .help("Name of the SVM to install")
                        )
                        .arg(
                            Arg::new("host")
                                .long("host")
                                .value_name("HOST")
                                .required(true)
                                .help("Remote host to install on (format: user@host[:port])")
                        )
                )
        )
        // Node management commands
        .subcommand(
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
                                .help("Filter nodes by SVM")
                        )
                        .arg(
                            Arg::new("type")
                                .long("type")
                                .value_name("NODE_TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["validator", "rpc", "all"]))
                                .default_value("all")
                                .help("Filter nodes by type")
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet", "all"]))
                                .default_value("all")
                                .help("Filter nodes by network")
                        )
                        .arg(
                            Arg::new("status")
                                .long("status")
                                .value_name("STATUS")
                                .value_parser(clap::builder::PossibleValuesParser::new(["running", "stopped", "error", "unknown", "all"]))
                                .default_value("all")
                                .help("Filter nodes by status")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    Command::new("dashboard").about("Launch interactive node monitoring dashboard")
                )
                .subcommand(
                    Command::new("status")
                        .about("Check status of nodes")
                        .arg(
                            Arg::new("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to check")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    Command::new("get")
                        .about("Get detailed information about a specific node")
                        .arg(
                            Arg::new("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to get information about")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    Command::new("restart")
                        .about("Restart a node")
                        .arg(
                            Arg::new("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to restart")
                        )
                )
                .subcommand(
                    Command::new("stop")
                        .about("Stop a node")
                        .arg(
                            Arg::new("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to stop")
                        )
                )
                .subcommand(
                    Command::new("logs")
                        .about("View logs from a node")
                        .arg(
                            Arg::new("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to get logs from")
                        )
                        .arg(
                            Arg::new("lines")
                                .long("lines")
                                .short('n')
                                .value_name("LINES")
                                .default_value("100")
                                .help("Number of lines to show")
                        )
                        .arg(
                            Arg::new("follow")
                                .long("follow")
                                .short('f')
                                .action(ArgAction::SetTrue)
                                .help("Follow log output")
                        )
                )
                .subcommand(
                    Command::new("deploy")
                        .about("Deploy a new node")
                        .arg(
                            Arg::new("svm")
                                .long("svm")
                                .value_name("SVM_NAME")
                                .required(true)
                                .help("SVM to deploy node for")
                        )
                        .arg(
                            Arg::new("type")
                                .long("type")
                                .value_name("NODE_TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["validator", "rpc"]))
                                .default_value("validator")
                                .help("Type of node to deploy")
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                        .arg(
                            Arg::new("host")
                                .long("host")
                                .value_name("HOST")
                                .required(true)
                                .help("Remote host to deploy on (format: user@host[:port])")
                        )
                        .arg(
                            Arg::new("name")
                                .long("name")
                                .value_name("NAME")
                                .help("Custom name for the node (default: auto-generated)")
                        )
                )
        )
        .subcommand(
            Command::new("rpc")
                .about("Deploy an RPC node to a remote host")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("sonic")
                        .about("Deploy a Sonic RPC node")
                        .arg(
                            Arg::new("connection")
                                .help("SSH connection string (format: user@host[:port])")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                )
                .subcommand(
                    Command::new("solana")
                        .about("Deploy a Solana RPC node")
                        .arg(
                            Arg::new("connection")
                                .help("SSH connection string (format: user@host[:port])")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                )
        )
        .subcommand(
            Command::new("deploy")
                .about("Deploy eBPF binary to all available SVM networks")
                .arg(
                    Arg::new("binary")
                        .value_name("BINARY_PATH")
                        .help("Path to the eBPF binary file (.so)")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("program-id")
                        .long("program-id")
                        .value_name("PROGRAM_ID_PATH")
                        .help("Path to program keypair JSON file (for new deployments) or program address JSON file (for upgrades)")
                        .required(true)
                )
                .arg(
                    Arg::new("owner")
                        .long("owner")
                        .value_name("OWNER_PATH")
                        .help("Path to program owner keypair JSON file (must contain private key)")
                        .required(true)
                )
                .arg(
                    Arg::new("fee")
                        .long("fee")
                        .value_name("FEE_PAYER_PATH")
                        .help("Path to deployment fee payer keypair JSON file (must contain private key)")
                        .required(true)
                )
                .arg(
                    Arg::new("publish-idl")
                        .long("publish-idl")
                        .action(ArgAction::SetTrue)
                        .help("Publish IDL alongside the program deployment")
                )
                .arg(
                    Arg::new("idl-file")
                        .long("idl-file")
                        .value_name("IDL_PATH")
                        .help("Path to Anchor IDL JSON file (optional, defaults to generated IDL)")
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet", "all"]))
                        .default_value("all")
                        .help("Network to deploy on (default: deploy to all networks)")
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output results in JSON format for machine-readable processing")
                )
                .arg(
                    Arg::new("retry-attempts")
                        .long("retry-attempts")
                        .value_name("COUNT")
                        .default_value("3")
                        .help("Number of retry attempts for failed deployments (default: 3)")
                )
                .arg(
                    Arg::new("confirm-large")
                        .long("confirm-large")
                        .action(ArgAction::SetTrue)
                        .help("Require confirmation for deploying large binaries (>1MB)")
                )
        )
        .subcommand(
            Command::new("new_feature_command")
                .about("New feature for testing")
        )
        .subcommand(
            Command::new("solana")
                .about("Deploy and manage Solana validators")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("validator")
                        .about("Deploy a Solana validator node with enhanced features")
                        .arg(
                            Arg::new("connection")
                                .help("SSH connection string (format: user@host[:port])")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                        .arg(
                            Arg::new("version")
                                .long("version")
                                .value_name("VERSION")
                                .help("Solana client version (e.g., v1.16.0, v1.18.23-jito)")
                        )
                        .arg(
                            Arg::new("client-type")
                                .long("client-type")
                                .value_name("TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["standard", "jito", "agave", "firedancer", "sig"]))
                                .default_value("agave")
                                .help("Solana client type (standard, jito, agave, firedancer, sig)")
                        )
                        .arg(
                            Arg::new("hot-swap")
                                .long("hot-swap")
                                .action(ArgAction::SetTrue)
                                .help("Enable hot-swap capability for high availability")
                        )
                        .arg(
                            Arg::new("ledger-disk")
                                .long("ledger-disk")
                                .value_name("DEVICE")
                                .help("Ledger disk device path (e.g., /dev/nvme0n1)")
                        )
                        .arg(
                            Arg::new("accounts-disk")
                                .long("accounts-disk")
                                .value_name("DEVICE")
                                .help("Accounts disk device path (e.g., /dev/nvme1n1)")
                        )
                        .arg(
                            Arg::new("metrics-config")
                                .long("metrics-config")
                                .value_name("CONFIG")
                                .help("Metrics configuration string (e.g., host=https://metrics.solana.com:8086,db=mainnet-beta,u=mainnet-beta_write,p=password)")
                        )
                )
                .subcommand(
                    Command::new("rpc")
                        .about("Deploy a Solana RPC node with enhanced features")
                        .arg(
                            Arg::new("connection")
                                .help("SSH connection string (format: user@host[:port])")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                        .arg(
                            Arg::new("version")
                                .long("version")
                                .value_name("VERSION")
                                .help("Solana client version (e.g., v1.16.0)")
                        )
                        .arg(
                            Arg::new("client-type")
                                .long("client-type")
                                .value_name("TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["standard", "jito", "agave", "firedancer", "sig"]))
                                .default_value("agave")
                                .help("Solana client type (standard, jito, agave, firedancer, sig)")
                        )
                        .arg(
                            Arg::new("ledger-disk")
                                .long("ledger-disk")
                                .value_name("DEVICE")
                                .help("Ledger disk device path (e.g., /dev/nvme0n1)")
                        )
                        .arg(
                            Arg::new("accounts-disk")
                                .long("accounts-disk")
                                .value_name("DEVICE")
                                .help("Accounts disk device path (e.g., /dev/nvme1n1)")
                        )
                        .arg(
                            Arg::new("metrics-config")
                                .long("metrics-config")
                                .value_name("CONFIG")
                                .help("Metrics configuration string")
                        )
                        .arg(
                            Arg::new("enable-history")
                                .long("enable-history")
                                .action(ArgAction::SetTrue)
                                .help("Enable transaction history (increases storage requirements)")
                        )
                )
        )
        .get_matches()
}
