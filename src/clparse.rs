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
        .disable_version_flag(true) // Disable the auto-generated --version flag
        .arg_required_else_help(false) // Allow no args to default to advanced chat
        .after_help("
QUICK START:
  osvm                        Launch interactive AI-powered agent chat (default)
  osvm chat --advanced        Launch advanced multi-session chat interface
  osvm agent \"<prompt>\"      Execute single AI-powered command
  osvm doctor                 Check system health and dependencies
  osvm mcp setup              Set up Solana MCP server integration
  osvm --help                 Show detailed help for all commands

AI-POWERED NATURAL LANGUAGE:
  ⭐ Any unknown command is interpreted as an AI query!

  Examples:
  • osvm \"how do I deploy a validator?\"
  • osvm \"check my wallet balance\"
  • osvm \"what's the current network status?\"
  • osvm \"show me recent transactions\"

  The AI will:
  → Understand your natural language request
  → Plan and execute the appropriate tools via MCP
  → Provide intelligent responses with context

For more examples: osvm examples
For issues & feedback: https://github.com/anthropics/osvm-cli/issues")
        .before_help("░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
........███████.....█████████..█████...█████..█████...█████.....▐█░░░░░░
......███░░░░░███..███░░░░░███░░███...░░███.░░██████.██████.......▐█░░░░
.....███.....░░███░███....░░░..░███....░███..░███░█████░███.........▐█░░
....░███......░███░░█████████..░███....░███..░███░░███░░███...........▐█
....░███......░███.░░░░░░░░███.░░███...███...░███░░░░░░░███.........▐█░░
....░░███.....███..███....░███..░░░█████░....░███░░░░░░░███.......▐█░░░░
░░░░░░░░███████░░░░░█████████░░░░░░░███░░░░░░░███░░░░░░░███░░░░░▐█░░░░░░
 ███████████████████████████████████████████████████████████████████████

🚀 OSVM CLI v0.1.0 - OpenSVM Command Line Interface
   AI-Powered Solana Virtual Machine Management & Security Auditing")
        // Add version aliases as subcommands
        .subcommand(Command::new("v").about("Show version information"))
        .subcommand(Command::new("ver").about("Show version information"))
        .subcommand(Command::new("version").about("Show version information"))
        .allow_external_subcommands(true)
        // Add a single version flag with multiple aliases
        .arg(
            Arg::new("version_flag")
                .long("version")
                .short('V')
                .visible_aliases(["v", "ver"])
                .action(ArgAction::SetTrue)
                .help("Show version information")
                .global(false)
        )
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
            Arg::new("debug")
                .long("debug")
                .action(ArgAction::SetTrue)
                .global(true)
                .help("Show debug information"),
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
        .arg(
            Arg::new("theme")
                .long("theme")
                .value_name("THEME")
                .global(true)
                .help("UI theme to use")
        )
        .arg(
            Arg::new("auto_theme")
                .long("auto-theme")
                .action(ArgAction::SetTrue)
                .global(true)
                .help("Enable automatic theme switching")
        )
        // Core commands
        .subcommand(
            Command::new("balance")
                .about("Check SOL balance for an address")
                .long_about("Check the SOL balance for a Solana address.\n\
                           \n\
                           If no address is provided, shows the balance of the configured keypair.\n\
                           \n\
                           Examples:\n\
                           • osvm balance                                    # Your configured wallet\n\
                           • osvm balance 4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T # Specific address")
                .arg(
                    Arg::new("address")
                        .value_name("ADDRESS")
                        .help("Solana address to check balance for (defaults to configured keypair)")
                        .index(1)
                )
        )
        // RPC management commands
        .subcommand(
            Command::new("rpc-manager") // Renamed from "rpc" to "rpc-manager"
                .about("Manage RPC nodes (local/remote)")
                .arg_required_else_help(true)
                .subcommand( // Moved sonic to be first
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
                    Command::new("query-solana") // Renamed from "solana"
                        .about("Query Solana RPC endpoint (info, health, monitor)") // Updated about string
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("mainnet")
                                .help("Solana network to query")
                        )
                        .arg(
                            Arg::new("custom-url")
                                .long("custom-url")
                                .value_name("URL")
                                .help("Custom RPC URL to query")
                        )
                        .arg(
                            Arg::new("monitor")
                                .long("monitor")
                                .action(ArgAction::SetTrue)
                                .help("Monitor network activity in real-time")
                        )
                        .arg(
                            Arg::new("health")
                                .long("health")
                                .action(ArgAction::SetTrue)
                                .help("Check network health")
                        )
                        .arg(
                            Arg::new("info")
                                .long("info")
                                .action(ArgAction::SetTrue)
                                .help("Show network information (default if no other flag)")
                        )
                )
                .subcommand(
                    Command::new("local")
                        .about("Deploy a local RPC node on localhost")
                        .arg(
                            Arg::new("svm")
                                .long("svm")
                                .value_name("SVM_NAME")
                                .value_parser(clap::builder::PossibleValuesParser::new(["solana", "sonic", "eclipse", "soon", "opensvm"]))
                                .default_value("solana")
                                .help("SVM to deploy RPC for")
                        )
                        .arg(
                            Arg::new("network")
                                .long("network")
                                .value_name("NETWORK")
                                .value_parser(clap::builder::PossibleValuesParser::new(["mainnet", "testnet", "devnet"]))
                                .default_value("devnet")
                                .help("Network to deploy on")
                        )
                        .arg(
                            Arg::new("port")
                                .long("port")
                                .value_name("PORT")
                                .default_value("8899")
                                .help("RPC port to bind to (default: 8899)")
                        )
                        .arg(
                            Arg::new("faucet-port")
                                .long("faucet-port")
                                .value_name("FAUCET_PORT")
                                .default_value("9900")
                                .help("Faucet port for devnet (default: 9900)")
                        )
                        .arg(
                            Arg::new("ledger-path")
                                .long("ledger-path")
                                .value_name("PATH")
                                .help("Ledger data directory path (default: ~/.config/osvm/ledgers/{network})")
                        )
                        .arg(
                            Arg::new("reset")
                                .long("reset")
                                .action(ArgAction::SetTrue)
                                .help("Reset the ledger on startup")
                        )
                        .arg(
                            Arg::new("background")
                                .long("background")
                                .short('d')
                                .action(ArgAction::SetTrue)
                                .help("Run in background (daemon mode)")
                        )
                        .arg(
                            Arg::new("stop")
                                .long("stop")
                                .action(ArgAction::SetTrue)
                                .help("Stop running local RPC node")
                        )
                        .arg(
                            Arg::new("status")
                                .long("status")
                                .action(ArgAction::SetTrue)
                                .help("Check status of local RPC node")
                        )
                )
                .subcommand(
                    Command::new("test")
                        .about("Start a local test validator with RPC for development")
                        .arg(
                            Arg::new("ledger-path")
                                .long("ledger-path")
                                .value_name("PATH")
                                .default_value("test-ledger")
                                .help("Ledger data directory path")
                        )
                        .arg(
                            Arg::new("rpc-port")
                                .long("rpc-port")
                                .value_name("PORT")
                                .default_value("8899")
                                .help("RPC port to bind to")
                        )
                        .arg(
                            Arg::new("faucet-port")
                                .long("faucet-port")
                                .value_name("PORT")
                                .default_value("9900")
                                .help("Faucet port for SOL airdrops")
                        )
                        .arg(
                            Arg::new("reset")
                                .long("reset")
                                .action(ArgAction::SetTrue)
                                .help("Reset the ledger on startup")
                        )
                        .arg(
                            Arg::new("background")
                                .long("background")
                                .short('d')
                                .action(ArgAction::SetTrue)
                                .help("Run in background (daemon mode)")
                        )
                        .arg(
                            Arg::new("stop")
                                .long("stop")
                                .action(ArgAction::SetTrue)
                                .help("Stop running test validator")
                        )
                        .arg(
                            Arg::new("status")
                                .long("status")
                                .action(ArgAction::SetTrue)
                                .help("Check status of test validator")
                        )
                        .arg(
                            Arg::new("logs")
                                .long("logs")
                                .action(ArgAction::SetTrue)
                                .help("Show recent logs from test validator")
                        )
                        .arg(
                            Arg::new("quiet")
                                .long("quiet")
                                .short('q')
                                .action(ArgAction::SetTrue)
                                .help("Suppress non-essential output")
                        )
                )
                .subcommand(
                    Command::new("devnet")
                        .about("Start a legitimate devnet RPC node that syncs with Solana devnet")
                        .arg(
                            Arg::new("ledger-path")
                                .long("ledger-path")
                                .value_name("PATH")
                                .default_value("devnet-ledger")
                                .help("Ledger data directory path for devnet sync")
                        )
                        .arg(
                            Arg::new("rpc-port")
                                .long("rpc-port")
                                .value_name("PORT")
                                .default_value("8899")
                                .help("RPC port to bind to")
                        )
                        .arg(
                            Arg::new("background")
                                .long("background")
                                .short('d')
                                .action(ArgAction::SetTrue)
                                .help("Run in background (daemon mode)")
                        )
                        .arg(
                            Arg::new("stop")
                                .long("stop")
                                .action(ArgAction::SetTrue)
                                .help("Stop running devnet RPC node")
                        )
                        .arg(
                            Arg::new("status")
                                .long("status")
                                .action(ArgAction::SetTrue)
                                .help("Check status of devnet RPC node")
                        )
                        .arg(
                            Arg::new("logs")
                                .long("logs")
                                .action(ArgAction::SetTrue)
                                .help("Show recent logs from devnet RPC node")
                        )
                        .arg(
                            Arg::new("lines")
                                .long("lines")
                                .short('n')
                                .value_name("LINES")
                                .default_value("50")
                                .help("Number of recent log lines to show (used with --logs)")
                        )
                        .arg(
                            Arg::new("follow")
                                .long("follow")
                                .short('f')
                                .action(ArgAction::SetTrue)
                                .help("Follow log output in real-time (used with --logs)")
                        )
                )
        )
        // END OF MOVED RPC SUBCOMMAND
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
            Command::new("chat")
                .about("Interactive AI-powered agent chat with MCP tools and intelligent planning")
                .long_about("Launch a comprehensive chat interface with AI-powered tool planning and execution.\n\
                           \n\
                           Basic Mode:\n\
                           • Simple chat interface with MCP tool integration\n\
                           • Single chat session\n\
                           • Direct tool calling\n\
                           \n\
                           Advanced Mode (--advanced, default when no args):\n\
                           • FAR-style/Borland TUI design with dual panels\n\
                           • AI-powered input parsing and intelligent tool planning\n\
                           • Multi-session management with background agent execution\n\
                           • Session recording and agent control (run/pause/stop)\n\
                           • Professional keyboard shortcuts and vim-like navigation")
                .arg(
                    Arg::new("debug")
                        .long("debug")
                        .action(ArgAction::SetTrue)
                        .help("Enable debug mode for chat interface")
                )
                .arg(
                    Arg::new("test")
                        .long("test")
                        .action(ArgAction::SetTrue)
                        .help("Run comprehensive UI tests and show screenshots")
                )
                .arg(
                    Arg::new("advanced")
                        .long("advanced")
                        .action(ArgAction::SetTrue)
                        .help("Launch advanced FAR-style chat interface with AI planning and multi-session support")
                )
        )
        .subcommand(
            Command::new("plan")
                .about("Create an AI-powered execution plan for OSVM commands")
                .long_about("Analyze a natural language request and generate an executable OSVM command plan.\n\
                           \n\
                           This command uses AI to understand your intent and suggests the appropriate\n\
                           OSVM commands to accomplish your goal.\n\
                           \n\
                           Examples:\n\
                           • osvm plan \"show me all validators\"\n\
                           • osvm plan \"check network health\"\n\
                           • osvm plan \"list my nodes\"")
                .arg(
                    Arg::new("query")
                        .value_name("QUERY")
                        .help("Natural language query describing what you want to do")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("execute")
                        .long("execute")
                        .short('e')
                        .action(ArgAction::SetTrue)
                        .help("Execute the plan automatically (skip confirmation for safe commands)")
                )
                .arg(
                    Arg::new("yes")
                        .long("yes")
                        .short('y')
                        .action(ArgAction::SetTrue)
                        .help("Auto-confirm all commands including those requiring confirmation")
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output results in JSON format")
                )
        )
        .subcommand(
            Command::new("agent")
                .about("Execute agent commands with AI planning and MCP tool execution")
                .long_about("Execute a single agent command with AI-powered planning and tool execution.\n\
                           \n\
                           The agent will:\n\
                           • Analyze your request using AI\n\
                           • Create an execution plan with available MCP tools\n\
                           • Execute the tools in sequence\n\
                           • Provide a contextual response\n\
                           \n\
                           Examples:\n\
                           • osvm agent \"What's my wallet balance?\"\n\
                           • osvm agent \"Show recent transactions\"\n\
                           • osvm agent \"Deploy a validator node\"")
                .arg(
                    Arg::new("prompt")
                        .value_name("PROMPT")
                        .help("The prompt or command for the agent to execute")
                        .required(true)
                        .index(1)
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output results in JSON format")
                )
                .arg(
                    Arg::new("verbose")
                        .long("verbose")
                        .short('v')
                        .action(ArgAction::Count)
                        .help("Show detailed execution steps")
                )
                .arg(
                    Arg::new("no-tools")
                        .long("no-tools")
                        .action(ArgAction::SetTrue)
                        .help("Disable MCP tool execution (AI response only)")
                )
                .arg(
                    Arg::new("timeout")
                        .long("timeout")
                        .value_name("SECONDS")
                        .default_value("30")
                        .help("Maximum execution time in seconds")
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
                        .about("🚧 COMING SOON: Install an SVM on a remote host")
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
                        .about("🚧 COMING SOON: Restart a node")
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
                        .about("🚧 COMING SOON: Stop a node")
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
            Command::new("doctor")
                .about("Comprehensive system health check and repair")
                .arg(
                    Arg::new("check_all")
                        .long("check-all")
                        .action(ArgAction::SetTrue)
                        .help("Run comprehensive health check")
                )
                .arg(
                    Arg::new("fix")
                        .long("fix")
                        .action(ArgAction::SetTrue)
                        .help("Attempt to fix detected issues automatically")
                )
                .arg(
                    Arg::new("system_only")
                        .long("system-only")
                        .action(ArgAction::SetTrue)
                        .help("Check only system-level dependencies")
                )
                .arg(
                    Arg::new("user_only")
                        .long("user-only")
                        .action(ArgAction::SetTrue)
                        .help("Check only user-level dependencies")
                )
                .arg(
                    Arg::new("verbose")
                        .long("verbose")
                        .short('v')
                        .action(ArgAction::Count)
                        .help("Detailed diagnostic output")
                )
        )
        .subcommand(
            Command::new("mcp")
                .about("Manage Model Context Protocol (MCP) servers")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("add")
                        .about("Add or update an MCP server configuration")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("server_url")
                                .long("server-url")
                                .value_name("URL")
                                .required(true)
                                .help("MCP server URL (e.g., http://localhost:3000)")
                        )
                        .arg(
                            Arg::new("name")
                                .long("name")
                                .value_name("NAME")
                                .help("Human-readable name for the server")
                        )
                        .arg(
                            Arg::new("transport")
                                .long("transport")
                                .value_name("TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["http", "stdio"]))
                                .default_value("http")
                                .help("Transport type for communication (websocket not yet implemented)")
                        )
                        .arg(
                            Arg::new("auth_type")
                                .long("auth-type")
                                .value_name("TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["none", "bearer", "api_key", "basic"]))
                                .default_value("none")
                                .help("Authentication type")
                        )
                        .arg(
                            Arg::new("auth_token")
                                .long("auth-token")
                                .value_name("TOKEN")
                                .help("Authentication token (for bearer or api_key auth)")
                        )
                        .arg(
                            Arg::new("username")
                                .long("username")
                                .value_name("USERNAME")
                                .help("Username (for basic auth)")
                        )
                        .arg(
                            Arg::new("password")
                                .long("password")
                                .value_name("PASSWORD")
                                .help("Password (for basic auth)")
                        )
                        .arg(
                            Arg::new("enabled")
                                .long("enabled")
                                .action(ArgAction::SetTrue)
                                .help("Enable the server immediately after adding")
                        )
                )
                .subcommand(
                    Command::new("add-github")
                        .about("Add MCP server from GitHub repository")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("github_url")
                                .help("GitHub repository URL (e.g., https://github.com/openSVM/solana-mcp-server)")
                                .required(true)
                                .index(2)
                        )
                        .arg(
                            Arg::new("name")
                                .long("name")
                                .value_name("NAME")
                                .help("Human-readable name for the server")
                        )
                        .arg(
                            Arg::new("enabled")
                                .long("enabled")
                                .action(ArgAction::SetTrue)
                                .help("Enable the server immediately after adding")
                        )
                        .arg(
                            Arg::new("yes")
                                .long("yes")
                                .short('y')
                                .action(ArgAction::SetTrue)
                                .help("Skip interactive confirmation (for automation and CI)")
                        )
                )
                .subcommand(
                    Command::new("remove")
                        .about("Remove an MCP server configuration")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier to remove")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("list")
                        .about("List all configured MCP servers")
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                        .arg(
                            Arg::new("enabled_only")
                                .long("enabled-only")
                                .action(ArgAction::SetTrue)
                                .help("Show only enabled servers")
                        )
                )
                .subcommand(
                    Command::new("enable")
                        .about("Enable an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier to enable")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("disable")
                        .about("Disable an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier to disable")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("test")
                        .about("Test connectivity to an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier to test")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("init")
                        .about("Initialize connection with an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier to initialize")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("tools")
                        .about("List available tools from an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("call")
                        .about("Call a tool on an MCP server")
                        .arg(
                            Arg::new("server_id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("tool_name")
                                .help("Name of the tool to call")
                                .required(true)
                                .index(2)
                        )
                        .arg(
                            Arg::new("arguments")
                                .long("args")
                                .value_name("JSON")
                                .help("Tool arguments as JSON (e.g., '{\"param\":\"value\"}')")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("setup")
                        .about("Quick setup for Solana MCP server integration")
                        .arg(
                            Arg::new("mcp_url")
                                .long("mcp-url")
                                .value_name("URL")
                                .help("Solana MCP server URL (default: http://localhost:3000)")
                                .default_value("http://localhost:3000")
                        )
                        .arg(
                            Arg::new("auto_enable")
                                .long("auto-enable")
                                .action(ArgAction::SetTrue)
                                .help("Automatically enable the server after setup")
                        )
                )
                .subcommand(
                    Command::new("search")
                        .about("Search for MCP servers by name, description, or features")
                        .arg(
                            Arg::new("query")
                                .help("Search query (searches name, description, and features)")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("transport")
                                .long("transport")
                                .value_name("TYPE")
                                .value_parser(clap::builder::PossibleValuesParser::new(["http", "stdio", "any"]))
                                .default_value("any")
                                .help("Filter by transport type")
                        )
                        .arg(
                            Arg::new("enabled_only")
                                .long("enabled-only")
                                .action(ArgAction::SetTrue)
                                .help("Only show enabled servers")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output results in JSON format")
                        )
                )
                .subcommand(
                    Command::new("mount")
                        .about("Mount a folder to an MCP tool (auto-detected tool path)")
                        .arg(
                            Arg::new("tool_name")
                                .help("MCP tool name (e.g., solana-mcp)")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("host_path")
                                .help("Host folder path to mount (e.g., ~/solana-data)")
                                .required(true)
                                .index(2)
                        )
                        .arg(
                            Arg::new("readonly")
                                .long("readonly")
                                .short('r')
                                .action(ArgAction::SetTrue)
                                .help("Mount as read-only")
                        )
                )
                .subcommand(
                    Command::new("unmount")
                        .about("Unmount a folder from an MCP tool")
                        .arg(
                            Arg::new("tool_name")
                                .help("MCP tool name")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("host_path")
                                .help("Host folder path to unmount")
                                .required(true)
                                .index(2)
                        )
                )
                .subcommand(
                    Command::new("mounts")
                        .about("List mounts for MCP tools")
                        .arg(
                            Arg::new("tool_name")
                                .help("MCP tool name (optional, shows all if not specified)")
                                .index(1)
                        )
                )
        )
        .subcommand(
            Command::new("mount")
                .about("Manage folder mounts for OSVM microVMs and MCP tools")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("add")
                        .about("Mount a folder to OSVM microVM (auto-detected VM path)")
                        .arg(
                            Arg::new("host_path")
                                .help("Host folder path to mount (e.g., ~/Documents)")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("readonly")
                                .long("readonly")
                                .short('r')
                                .action(ArgAction::SetTrue)
                                .help("Mount as read-only")
                        )
                )
                .subcommand(
                    Command::new("remove")
                        .about("Unmount a folder from OSVM microVM")
                        .arg(
                            Arg::new("host_path")
                                .help("Host folder path to unmount")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("list")
                        .about("List all OSVM microVM mounts")
                )
        )
        .subcommand(
            Command::new("snapshot")
                .about("Analyze and manage Solana snapshots")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("read")
                        .about("Read and display snapshot accounts")
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory (default: ~/.config/osvm/ledgers/devnet/remote/extracted)")
                        )
                        .arg(
                            Arg::new("limit")
                                .long("limit")
                                .short('n')
                                .value_name("COUNT")
                                .help("Limit number of accounts to display")
                        )
                        .arg(
                            Arg::new("offset")
                                .long("offset")
                                .value_name("COUNT")
                                .default_value("0")
                                .help("Skip first N accounts")
                        )
                        .arg(
                            Arg::new("parallel")
                                .long("parallel")
                                .action(ArgAction::SetTrue)
                                .help("Enable parallel processing")
                        )
                        .arg(
                            Arg::new("threads")
                                .long("threads")
                                .value_name("COUNT")
                                .help("Number of threads for parallel processing (default: CPU count)")
                        )
                        .arg(
                            Arg::new("filter-owner")
                                .long("filter-owner")
                                .value_name("PUBKEY")
                                .help("Filter accounts by owner program")
                        )
                        .arg(
                            Arg::new("filter-min-balance")
                                .long("filter-min-balance")
                                .value_name("LAMPORTS")
                                .help("Filter accounts with balance >= LAMPORTS")
                        )
                        .arg(
                            Arg::new("filter-max-balance")
                                .long("filter-max-balance")
                                .value_name("LAMPORTS")
                                .help("Filter accounts with balance <= LAMPORTS")
                        )
                        .arg(
                            Arg::new("filter-min-size")
                                .long("filter-min-size")
                                .value_name("BYTES")
                                .help("Filter accounts with data size >= BYTES")
                        )
                        .arg(
                            Arg::new("filter-max-size")
                                .long("filter-max-size")
                                .value_name("BYTES")
                                .help("Filter accounts with data size <= BYTES")
                        )
                        .arg(
                            Arg::new("filter-executable")
                                .long("filter-executable")
                                .action(ArgAction::SetTrue)
                                .help("Filter only executable accounts")
                        )
                        .arg(
                            Arg::new("filter-rent-exempt")
                                .long("filter-rent-exempt")
                                .action(ArgAction::SetTrue)
                                .help("Filter only rent-exempt accounts")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                        .arg(
                            Arg::new("quiet")
                                .long("quiet")
                                .short('q')
                                .action(ArgAction::SetTrue)
                                .help("Suppress non-essential output")
                        )
                )
                .subcommand(
                    Command::new("stats")
                        .about("Show comprehensive snapshot statistics")
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("export")
                        .about("Export snapshot to various formats")
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory")
                        )
                        .arg(
                            Arg::new("output")
                                .long("output")
                                .short('o')
                                .value_name("FILE")
                                .required(true)
                                .help("Output file path")
                        )
                        .arg(
                            Arg::new("format")
                                .long("format")
                                .short('f')
                                .value_name("FORMAT")
                                .value_parser(clap::builder::PossibleValuesParser::new(["json", "csv", "parquet", "msgpack"]))
                                .default_value("json")
                                .help("Export format")
                        )
                        .arg(
                            Arg::new("filter-owner")
                                .long("filter-owner")
                                .value_name("PUBKEY")
                                .help("Filter accounts by owner program")
                        )
                        .arg(
                            Arg::new("filter-min-balance")
                                .long("filter-min-balance")
                                .value_name("LAMPORTS")
                                .help("Filter accounts with balance >= LAMPORTS")
                        )
                        .arg(
                            Arg::new("filter-max-balance")
                                .long("filter-max-balance")
                                .value_name("LAMPORTS")
                                .help("Filter accounts with balance <= LAMPORTS")
                        )
                )
                .subcommand(
                    Command::new("compare")
                        .about("Compare two snapshots and show differences")
                        .arg(
                            Arg::new("snapshot1")
                                .value_name("SNAPSHOT1")
                                .required(true)
                                .index(1)
                                .help("First snapshot directory")
                        )
                        .arg(
                            Arg::new("snapshot2")
                                .value_name("SNAPSHOT2")
                                .required(true)
                                .index(2)
                                .help("Second snapshot directory")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("validate")
                        .about("Validate snapshot integrity")
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("find")
                        .about("Find specific account by pubkey")
                        .arg(
                            Arg::new("pubkey")
                                .value_name("PUBKEY")
                                .required(true)
                                .index(1)
                                .help("Account public key to search for")
                        )
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory")
                        )
                        .arg(
                            Arg::new("json")
                                .long("json")
                                .action(ArgAction::SetTrue)
                                .help("Output in JSON format")
                        )
                )
                .subcommand(
                    Command::new("interactive")
                        .about("Launch interactive TUI for snapshot exploration")
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Path to snapshot directory")
                        )
                )
        )
        .subcommand(
            Command::new("db")
                .about("Manage ClickHouse database for blockchain data indexing")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("init")
                        .about("Initialize ClickHouse database")
                        .arg(
                            Arg::new("data-dir")
                                .long("data-dir")
                                .value_name("PATH")
                                .help("Custom data directory path")
                        )
                )
                .subcommand(
                    Command::new("start")
                        .about("Start ClickHouse database server")
                )
                .subcommand(
                    Command::new("stop")
                        .about("Stop ClickHouse database server")
                )
                .subcommand(
                    Command::new("status")
                        .about("Check ClickHouse database server status")
                )
                .subcommand(
                    Command::new("query")
                        .about("Execute SQL query on ClickHouse database")
                        .arg(
                            Arg::new("query")
                                .long("query")
                                .short('q')
                                .value_name("SQL")
                                .required(true)
                                .help("SQL query to execute")
                        )
                )
                .subcommand(
                    Command::new("activity")
                        .about("View activity logs (CLI commands and chat history)")
                        .arg(
                            Arg::new("stats")
                                .long("stats")
                                .action(ArgAction::SetTrue)
                                .help("Show activity statistics")
                        )
                        .arg(
                            Arg::new("commands")
                                .long("commands")
                                .action(ArgAction::SetTrue)
                                .help("Show CLI command history")
                        )
                        .arg(
                            Arg::new("chat")
                                .long("chat")
                                .action(ArgAction::SetTrue)
                                .help("Show chat message history")
                        )
                        .arg(
                            Arg::new("limit")
                                .long("limit")
                                .short('n')
                                .value_name("COUNT")
                                .default_value("100")
                                .help("Limit number of results")
                        )
                        .arg(
                            Arg::new("session-id")
                                .long("session-id")
                                .value_name("ID")
                                .help("Filter by session ID")
                        )
                )
                .subcommand(
                    Command::new("sync")
                        .about("Sync blockchain data from snapshots/ledger to ClickHouse")
                        .arg(
                            Arg::new("mode")
                                .long("mode")
                                .value_name("MODE")
                                .value_parser(clap::builder::PossibleValuesParser::new([
                                    "last-30-days",
                                    "full-historical",
                                    "realtime"
                                ]))
                                .default_value("last-30-days")
                                .help("Sync mode")
                        )
                        .arg(
                            Arg::new("programs")
                                .long("programs")
                                .value_name("PUBKEYS")
                                .help("Comma-separated list of program IDs to index")
                        )
                        .arg(
                            Arg::new("accounts")
                                .long("accounts")
                                .value_name("PUBKEYS")
                                .help("Comma-separated list of account pubkeys to index")
                        )
                        .arg(
                            Arg::new("pattern")
                                .long("pattern")
                                .value_name("HEX")
                                .help("Hex byte pattern to match in account data (e.g., 0x1234abcd)")
                        )
                        .arg(
                            Arg::new("ledger-path")
                                .long("ledger-path")
                                .value_name("PATH")
                                .help("Custom ledger path")
                        )
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Custom snapshot directory")
                        )
                )
        )
        .subcommand(
            Command::new("realtime")
                .about("Manage real-time blockchain data sync daemon")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("start")
                        .about("Start real-time sync daemon")
                        .arg(
                            Arg::new("programs")
                                .long("programs")
                                .value_name("PUBKEYS")
                                .help("Comma-separated list of program IDs to monitor")
                        )
                        .arg(
                            Arg::new("accounts")
                                .long("accounts")
                                .value_name("PUBKEYS")
                                .help("Comma-separated list of account pubkeys to monitor")
                        )
                        .arg(
                            Arg::new("patterns")
                                .long("patterns")
                                .value_name("HEX")
                                .help("Hex byte patterns to match (e.g., 0x1234abcd)")
                        )
                        .arg(
                            Arg::new("ledger-path")
                                .long("ledger-path")
                                .value_name("PATH")
                                .help("Custom ledger path")
                        )
                        .arg(
                            Arg::new("snapshot-dir")
                                .long("snapshot-dir")
                                .value_name("PATH")
                                .help("Custom snapshot directory")
                        )
                )
                .subcommand(
                    Command::new("stop")
                        .about("Stop real-time sync daemon")
                )
                .subcommand(
                    Command::new("status")
                        .about("Check real-time sync daemon status")
                )
        )
        .subcommand(
            Command::new("audit")
                .about("Generate comprehensive security audit report")
                .arg(
                    Arg::new("repository")
                        .help("Repository to audit (format: owner/repo or owner/repo#branch)")
                        .value_name("REPOSITORY")
                        .index(1)
                )
                .arg(
                    Arg::new("output")
                        .long("output")
                        .short('o')
                        .value_name("PATH")
                        .help("Output directory for audit report files")
                        .default_value("audit_reports")
                )
                .arg(
                    Arg::new("format")
                        .long("format")
                        .value_name("FORMAT")
                        .value_parser(clap::builder::PossibleValuesParser::new(["typst", "pdf", "both", "json", "html", "markdown"]))
                        .default_value("both")
                        .help("Output format: typst source, PDF, both, JSON, HTML, or Markdown")
                )
                .arg(
                    Arg::new("verbose")
                        .long("verbose")
                        .short('v')
                        .action(ArgAction::Count)
                        .help("Verbose audit output")
                )
                .arg(
                    Arg::new("test")
                        .long("test")
                        .action(ArgAction::SetTrue)
                        .help("Generate test audit report with sample data")
                )
                .arg(
                    Arg::new("noai")
                        .long("noai")
                        .action(ArgAction::SetTrue)
                        .help("Disable AI-powered security analysis")
                )
                .arg(
                    Arg::new("api-url")
                        .long("api-url")
                        .value_name("URL")
                        .help("Custom API URL for AI analysis (default: https://osvm.ai/api/getAnswer)")
                )
                .arg(
                    Arg::new("gh")
                        .long("gh")
                        .value_name("REPO#BRANCH")
                        .help("Git repository to audit in format: owner/repo#branch
                               
Examples:
  --gh opensvm/aeamcp#main           # Audit main branch of opensvm/aeamcp
  --gh solana-labs/solana#master     # Audit Solana Labs repository
  --gh myorg/myproject#develop       # Audit develop branch

The command will:
1. Clone the specified repository and branch
2. Create a new audit branch with timestamp
3. Run comprehensive security analysis
4. Generate audit reports (Typst/PDF)
5. Commit and push results to the new branch")
                )
                .arg(
                    Arg::new("template")
                        .long("template")
                        .value_name("PATH")
                        .help("Path to external template file to use instead of built-in templates
                               
Examples:
  --template ./templates/custom.typst    # Use custom Typst template
  --template ./templates/custom.html     # Use custom HTML template
  --template ./templates/custom.json     # Use custom JSON template
  --template ./templates/custom.md       # Use custom Markdown template

If not specified, built-in templates embedded in the binary will be used.")
                )
                .arg(
                    Arg::new("no-commit")
                        .long("no-commit")
                        .action(ArgAction::SetTrue)
                        .help("Don't commit audit results to repository. If no output directory is provided, files will be copied to the current folder.")
                )
        )
        .get_matches()
}
