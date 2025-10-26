use {
    clap::{command, Arg, ArgAction, Command},
    solana_clap_utils::input_validators::{is_url_or_moniker, is_valid_signer},
};

// Import all modularized command builders (full refactoring - 19 commands)
mod agent;
mod audit;
mod balance;
mod chat;
mod database;
mod deploy;
mod doctor;
mod examples;
mod mcp;
mod mount;
mod nodes;
mod ovsm;
mod plan;
mod qa;
mod tutorial;
mod realtime;
mod rpc;
mod snapshot;
mod svm;

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
REVOLUTIONARY ARCHITECTURE:
  🛡️ OSVM uses revolutionary microVM/unikernel isolation:
  • Unikernels (50KB) - 99.9% attack surface reduction, 10-50ms boot
  • MicroVMs (5MB overhead) - Hardware isolation, 125ms boot, SEV/SGX support
  • Zero-Trust mTLS - All connections authenticated and encrypted
  • Hardware Security - VT-x/AMD-V, SEV/SGX, TPM, control flow integrity

  📖 Learn more: Architecture.md (comprehensive 2,150-line deep dive)

COMMAND CATEGORIES:
  Core Operations:
    balance              Check SOL balance for an address
    svm                  Manage Solana Virtual Machines
    nodes                Manage validator and RPC nodes

  AI & Automation:
    [default]            Interactive AI agent (microVM isolated by default)
    chat                 Advanced multi-session chat interface
    agent                Execute single AI-powered command
    plan                 Create AI-powered execution plans

  Data Management:
    snapshot             Analyze and manage Solana snapshots
    db                   ClickHouse database for blockchain indexing
    realtime             Real-time blockchain data sync daemon

  Infrastructure:
    rpc                  Manage RPC nodes (local/remote)
    deploy               Deploy eBPF programs to SVM networks
    mount                Manage folder mounts for microVMs

  Tools & Configuration:
    mcp                  Manage MCP (Model Context Protocol) servers
    audit                Generate security audit reports
    doctor               System health check and repair
    examples             Show usage examples
    ovsm                 OVSM scripting language for automation

QUICK START:
  osvm                        Launch AI agent (microVM isolated)
  osvm chat --advanced        Advanced multi-session interface
  osvm agent \"<prompt>\"      Execute single AI command
  osvm doctor                 Check system health
  osvm mcp setup              Set up Solana MCP integration

  # Isolation control:
  OSVM_SKIP_MICROVM=1 osvm   Skip microVM for development

AI-POWERED NATURAL LANGUAGE:
  ⭐ Any unknown command is interpreted as an AI query!

  Examples:
  • osvm \"how do I deploy a validator?\"
  • osvm \"check my wallet balance\"
  • osvm \"what's the current network status?\"
  • osvm \"show me recent transactions\"

  The AI will:
  → Understand your natural language request
  → Plan and execute appropriate tools via MCP
  → Provide intelligent responses with context

For more info: osvm examples | Architecture.md
Issues & feedback: https://github.com/opensvm/osvm-cli/issues")
        .before_help(format!("░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
........███████.....█████████..█████...█████..█████...█████.....▐█░░░░░░
......███░░░░░███..███░░░░░███░░███...░░███.░░██████.██████.......▐█░░░░
.....███.....░░███░███....░░░..░███....░███..░███░█████░███.........▐█░░
....░███......░███░░█████████..░███....░███..░███░░███░░███...........▐█
....░███......░███.░░░░░░░░███.░░███...███...░███░░░░░░░███.........▐█░░
....░░███.....███..███....░███..░░░█████░....░███░░░░░░░███.......▐█░░░░
░░░░░░░░███████░░░░░█████████░░░░░░░███░░░░░░░███░░░░░░░███░░░░░▐█░░░░░░
 ███████████████████████████████████████████████████████████████████████

🚀 OSVM CLI v{} - OpenSVM Command Line Interface
   AI-Powered Solana Virtual Machine Management & Security Auditing

   🛡️ Revolutionary microVM/unikernel architecture with hardware isolation
   ⚡ 99.9% attack surface reduction | <1ms latency | 125ms boot time",
    env!("CARGO_PKG_VERSION")))
        // Add version aliases as subcommands
        .subcommand(Command::new("v").about("Show version information"))
        .subcommand(Command::new("ver").about("Show version information"))
        .subcommand(Command::new("version").about("Show version information"))
        // Short aliases for planning/agent modes (external subcommands for natural language)
        .subcommand(Command::new("p").about("AI planning mode - use OVSM to plan and execute queries").allow_external_subcommands(true))
        .subcommand(Command::new("a").about("AI agent mode - use OVSM to plan and execute queries").allow_external_subcommands(true))
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
            Arg::new("plan")
                .long("plan")
                .short('p')
                .visible_short_alias('a')
                .action(ArgAction::SetTrue)
                .global(true)
                .help("Use AI planning mode (OVSM agent) for queries"),
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
        // Core commands - using modular builders
        .subcommand(balance::build_balance_command())
        .subcommand(rpc::build_rpc_command())
        .subcommand(examples::build_examples_command())
        .subcommand(chat::build_chat_command())
        .subcommand(plan::build_plan_command())
        .subcommand(agent::build_agent_command())
        .subcommand(svm::build_svm_command())
        .subcommand(nodes::build_nodes_command())
        .subcommand(deploy::build_deploy_command())
        .subcommand(doctor::build_doctor_command())
        .subcommand(tutorial::build_tutorial_command())
        .subcommand(mcp::build_mcp_command())
        .subcommand(mount::build_mount_command())
        .subcommand(snapshot::build_snapshot_command())
        .subcommand(database::build_database_command())
        .subcommand(realtime::build_realtime_command())
        .subcommand(audit::build_audit_command())
        .subcommand(qa::build_qa_command())
        .subcommand(ovsm::build_ovsm_command())
        .get_matches()
}
