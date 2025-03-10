//! @brief Command line setup and parse

use {
    clap::{
        crate_description, crate_name, crate_version, App, AppSettings, Arg, ArgMatches, SubCommand,
    },
    solana_clap_utils::input_validators::{is_url_or_moniker, is_valid_pubkey, is_valid_signer},
};

/// Construct the cli input model and parse command line
pub fn parse_command_line() -> ArgMatches<'static> {
    App::new(crate_name!())
        .about(crate_description!())
        .version(crate_version!())
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::AllowExternalSubcommands)
        .arg({
            let arg = Arg::with_name("config_file")
                .short("C")
                .long("config")
                .value_name("PATH")
                .takes_value(true)
                .global(true)
                .help("Configuration file to use");
            if let Some(ref config_file) = *solana_cli_config::CONFIG_FILE {
                arg.default_value(config_file)
            } else {
                arg
            }
        })
        .arg(
            Arg::with_name("keypair")
                .long("keypair")
                .value_name("KEYPAIR")
                .validator(is_valid_signer)
                .takes_value(true)
                .global(true)
                .help("Filepath or URL to a keypair [default: client keypair]"),
        )
        .arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .global(true)
                .multiple(true)
                .number_of_values(0)
                .help("Sets the level of verbosity (-v, -vv, -vvv)"),
        )
        .arg(
            Arg::with_name("no_color")
                .long("no-color")
                .takes_value(false)
                .global(true)
                .help("Disable colorized output (also respects NO_COLOR environment variable)"),
        )
        .arg(
            Arg::with_name("json_rpc_url")
                .short("u")
                .long("url")
                .value_name("URL")
                .takes_value(true)
                .global(true)
                .validator(is_url_or_moniker)
                .help("JSON RPC URL for the cluster [default: value from configuration file]"),
        )
        .arg(
            Arg::with_name("svm")
                .long("svm")
                .value_name("SVM_LIST")
                .takes_value(true)
                .help("Comma-separated list of SVMs to install"),
        )
        .arg(
            Arg::with_name("node-type")
                .long("node-type")
                .value_name("TYPE")
                .takes_value(true)
                .possible_values(&["validator", "rpc"])
                .default_value("validator")
                .help("Type of node to install (validator or RPC)"),
        )
        .arg(
            Arg::with_name("network")
                .long("network")
                .value_name("NETWORK")
                .takes_value(true)
                .possible_values(&["mainnet", "testnet", "devnet"])
                .default_value("mainnet")
                .help("Network to deploy on"),
        )
        .subcommand(
            SubCommand::with_name("balance").about("Get balance").arg(
                Arg::with_name("address")
                    .validator(is_valid_pubkey)
                    .value_name("ADDRESS")
                    .takes_value(true)
                    .index(1)
                    .help("Address to get the balance of"),
            ),
        )
        .subcommand(
            SubCommand::with_name("mint")
                .about("Mint a new key/value pair to an account")
                .arg(
                    Arg::with_name("to-owner")
                        .display_order(1)
                        .long("to-owner")
                        .short("t")
                        .required(true)
                        .takes_value(true)
                        .help("Owner of accounts")
                        .possible_values(&["User1", "User2"]),
                )
                .arg(
                    Arg::with_name("key")
                        .display_order(2)
                        .long("key")
                        .short("k")
                        .required(true)
                        .takes_value(true)
                        .help("The key of key/value pair"),
                )
                .arg(
                    Arg::with_name("value")
                        .display_order(3)
                        .long("value")
                        .required(true)
                        .min_values(1)
                        .help("The value string of key/value pair"),
                ),
        )
        .subcommand(
            SubCommand::with_name("transfer")
                .about("Transfer a key/value pair from one account to another")
                .arg(
                    Arg::with_name("from-owner")
                        .display_order(1)
                        .long("from-owner")
                        .short("f")
                        .required(true)
                        .takes_value(true)
                        .help("Owner to transfer from")
                        .possible_values(&["User1", "User2"]),
                )
                .arg(
                    Arg::with_name("to-owner")
                        .display_order(2)
                        .long("to-owner")
                        .short("t")
                        .required(true)
                        .takes_value(true)
                        .help("Owner to transfer to")
                        .possible_values(&["User1", "User2"]),
                )
                .arg(
                    Arg::with_name("key")
                        .display_order(3)
                        .long("key")
                        .short("k")
                        .required(true)
                        .takes_value(true)
                        .help("The key of key/value pair to transfer"),
                ),
        )
        .subcommand(
            SubCommand::with_name("burn")
                .about("Burn (delete) a key/value pair from an account")
                .arg(
                    Arg::with_name("from-owner")
                        .display_order(1)
                        .long("from-owner")
                        .short("f")
                        .required(true)
                        .takes_value(true)
                        .help("Owner to burn key/value from")
                        .possible_values(&["User1", "User2"]),
                )
                .arg(
                    Arg::with_name("key")
                        .display_order(2)
                        .long("key")
                        .short("k")
                        .required(true)
                        .takes_value(true)
                        .help("The key of key/value pair to burn"),
                ),
        )
        .subcommand(SubCommand::with_name("ping").about("Send a ping transaction"))
        .subcommand(
            SubCommand::with_name("examples")
                .about("Show usage examples for OSVM CLI commands")
                .arg(
                    Arg::with_name("category")
                        .long("category")
                        .short("c")
                        .value_name("CATEGORY")
                        .takes_value(true)
                        .help("Filter examples by category (basic, svm, node, monitoring, workflow)")
                )
                .arg(
                    Arg::with_name("list_categories")
                        .long("list-categories")
                        .help("List all available example categories")
                )
        )
        .subcommand(
            SubCommand::with_name("svm")
                .about("Manage Solana Virtual Machines (SVMs)")
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .subcommand(
                    SubCommand::with_name("list")
                        .about("List all SVMs installed in the chain")
                )
                .subcommand(
                    SubCommand::with_name("dashboard")
                        .about("Launch interactive SVM monitoring dashboard")
                )
                .subcommand(
                    SubCommand::with_name("get")
                        .about("Get detailed information about a specific SVM")
                        .arg(
                            Arg::with_name("name")
                                .value_name("NAME")
                                .index(1)
                                .required(true)
                                .help("Name of the SVM to get information about")
                        )
                )
                .subcommand(
                    SubCommand::with_name("install")
                        .about("Install an SVM on a remote host")
                        .arg(
                            Arg::with_name("name")
                                .value_name("NAME")
                                .index(1)
                                .required(true)
                                .help("Name of the SVM to install")
                        )
                        .arg(
                            Arg::with_name("host")
                                .long("host")
                                .value_name("HOST")
                                .required(true)
                                .takes_value(true)
                                .help("Remote host to install on (format: user@host[:port])")
                        )
                )
        )
        .subcommand(
            SubCommand::with_name("nodes")
                .about("Manage validator and RPC nodes")
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .subcommand(
                    SubCommand::with_name("list")
                        .about("List all nodes")
                        .arg(
                            Arg::with_name("svm")
                                .long("svm")
                                .value_name("SVM_NAME")
                                .takes_value(true)
                                .help("Filter nodes by SVM")
                        )
                        .arg(
                            Arg::with_name("type")
                                .long("type")
                                .value_name("NODE_TYPE")
                                .takes_value(true)
                                .possible_values(&["validator", "rpc", "all"])
                                .default_value("all")
                                .help("Filter nodes by type")
                        )
                        .arg(
                            Arg::with_name("network")
                                .long("network")
                                .value_name("NETWORK")
                                .takes_value(true)
                                .possible_values(&["mainnet", "testnet", "devnet", "all"])
                                .default_value("all")
                                .help("Filter nodes by network")
                        )
                        .arg(
                            Arg::with_name("status")
                                .long("status")
                                .value_name("STATUS")
                                .takes_value(true)
                                .possible_values(&["running", "stopped", "error", "unknown", "all"])
                                .default_value("all")
                                .help("Filter nodes by status")
                        )
                        .arg(
                            Arg::with_name("json")
                                .long("json")
                                .takes_value(false)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    SubCommand::with_name("dashboard").about("Launch interactive node monitoring dashboard")
                )
                .subcommand(
                    SubCommand::with_name("status")
                        .about("Check status of nodes")
                        .arg(
                            Arg::with_name("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to check")
                        )
                        .arg(
                            Arg::with_name("json")
                                .long("json")
                                .takes_value(false)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    SubCommand::with_name("get")
                        .about("Get detailed information about a specific node")
                        .arg(
                            Arg::with_name("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to get information about")
                        )
                        .arg(
                            Arg::with_name("json")
                                .long("json")
                                .takes_value(false)
                                .help("Output as JSON")
                        )
                )
                .subcommand(
                    SubCommand::with_name("restart")
                        .about("Restart a node")
                        .arg(
                            Arg::with_name("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to restart")
                        )
                )
                .subcommand(
                    SubCommand::with_name("stop")
                        .about("Stop a node")
                        .arg(
                            Arg::with_name("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to stop")
                        )
                )
                .subcommand(
                    SubCommand::with_name("logs")
                        .about("View logs from a node")
                        .arg(
                            Arg::with_name("node-id")
                                .value_name("NODE_ID")
                                .index(1)
                                .required(true)
                                .help("ID of the node to get logs from")
                        )
                        .arg(
                            Arg::with_name("lines")
                                .long("lines")
                                .short("n")
                                .value_name("LINES")
                                .takes_value(true)
                                .default_value("100")
                                .help("Number of lines to show")
                        )
                        .arg(
                            Arg::with_name("follow")
                                .long("follow")
                                .short("f")
                                .takes_value(false)
                                .help("Follow log output")
                        )
                )
                .subcommand(
                    SubCommand::with_name("deploy")
                        .about("Deploy a new node")
                        .arg(
                            Arg::with_name("svm")
                                .long("svm")
                                .value_name("SVM_NAME")
                                .required(true)
                                .takes_value(true)
                                .help("SVM to deploy node for")
                        )
                        .arg(
                            Arg::with_name("type")
                                .long("type")
                                .value_name("NODE_TYPE")
                                .takes_value(true)
                                .possible_values(&["validator", "rpc"])
                                .default_value("validator")
                                .help("Type of node to deploy")
                        )
                        .arg(
                            Arg::with_name("network")
                                .long("network")
                                .value_name("NETWORK")
                                .takes_value(true)
                                .possible_values(&["mainnet", "testnet", "devnet"])
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                        .arg(
                            Arg::with_name("host")
                                .long("host")
                                .value_name("HOST")
                                .takes_value(true)
                                .help("Remote host to deploy on (format: user@host)")
                        )
                )
        )
        .subcommand(
            SubCommand::with_name("rpc")
                .about("Deploy an RPC node to a remote host")
                .setting(AppSettings::SubcommandRequiredElseHelp)
                .subcommand(
                    SubCommand::with_name("sonic")
                        .about("Deploy a Sonic RPC node")
                        .arg(
                            Arg::with_name("connection")
                                .help("SSH connection string (format: user@host[:port])")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::with_name("network")
                                .long("network")
                                .value_name("NETWORK")
                                .takes_value(true)
                                .possible_values(&["mainnet", "testnet", "devnet"])
                                .default_value("mainnet")
                                .help("Network to deploy on")
                        )
                )
        )
        .get_matches()
}