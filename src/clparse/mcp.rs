//! MCP (Model Context Protocol) server management command definition

use clap::{Arg, ArgAction, Command};

pub fn build_mcp_command() -> Command {
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
            Command::new("microvm")
                .about("Manage MCP servers running in microVMs")
                .arg_required_else_help(true)
                .subcommand(
                    Command::new("launch")
                        .about("Launch an MCP server in a microVM")
                        .arg(
                            Arg::new("server-id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                        .arg(
                            Arg::new("memory")
                                .long("memory")
                                .value_name("MB")
                                .default_value("256")
                                .help("Memory allocation in MB")
                        )
                        .arg(
                            Arg::new("vcpus")
                                .long("vcpus")
                                .value_name("COUNT")
                                .default_value("1")
                                .help("Number of virtual CPUs")
                        )
                        .arg(
                            Arg::new("command")
                                .long("command")
                                .value_name("CMD")
                                .help("Command to run in the VM")
                        )
                )
                .subcommand(
                    Command::new("launch-many")
                        .about("Launch multiple MCP servers for testing")
                        .arg(
                            Arg::new("count")
                                .help("Number of VMs to launch")
                                .index(1)
                                .default_value("10")
                        )
                        .arg(
                            Arg::new("memory")
                                .long("memory")
                                .value_name("MB")
                                .default_value("256")
                                .help("Memory per VM in MB")
                        )
                )
                .subcommand(
                    Command::new("status")
                        .about("Show status of running MCP microVMs")
                )
                .subcommand(
                    Command::new("stop")
                        .about("Stop a running MCP microVM")
                        .arg(
                            Arg::new("server-id")
                                .help("Server identifier")
                                .required(true)
                                .index(1)
                        )
                )
                .subcommand(
                    Command::new("test")
                        .about("Run microVM integration tests")
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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm mcp setup
     Quick setup for Solana MCP server integration.
     💡 Configures default localhost:3000 server automatically.

  2. osvm mcp add-github solana-mcp https://github.com/openSVM/solana-mcp-server
     Add MCP server directly from GitHub repository.
     💡 Auto-clones, builds, and configures the server.

  3. osvm mcp list
     Show all configured MCP servers with status.
     💡 Displays server ID, URL, transport, and enabled state.

  4. osvm mcp test solana-mcp
     Test connectivity to a specific MCP server.
     💡 Verifies server is running and responds correctly.

  5. osvm mcp tools solana-mcp
     List all tools available from an MCP server.
     💡 Shows tool names, descriptions, and parameter schemas.

  6. osvm mcp call solana-mcp getBalance --args '{"address":"..."}'
     Call a specific tool with arguments.
     💡 Arguments must be valid JSON matching tool schema.

  7. osvm mcp enable solana-mcp && osvm mcp disable other-mcp
     Enable/disable servers for agent tool access.
     💡 Only enabled servers are used by AI agents.

  8. osvm mcp microvm launch solana-mcp --memory 512
     Launch MCP server in isolated microVM.
     💡 Hardware-level isolation for untrusted servers.

  9. osvm mcp mount solana-mcp ~/solana-data --readonly
     Mount folder to MCP tool in microVM.
     💡 Use --readonly for security when sharing sensitive data.

 10. osvm mcp search "solana"
     Search for MCP servers by name/description.
     💡 Finds servers matching your query across all configs.

💡 WHAT IS MCP?
  Model Context Protocol (MCP) is a standard for AI tool calling.
  MCP servers expose "tools" that AI agents can invoke to:
  • Query blockchain data (getBalance, getTransaction, etc.)
  • Execute actions (sendTransaction, deployProgram, etc.)
  • Access external services (APIs, databases, file systems)

SECURITY BEST PRACTICES:
  • Use microVMs for untrusted MCP servers
  • Mount folders as read-only when possible
  • Review tool permissions before enabling servers
  • Use auth tokens for production servers

SERVER TYPES:
  • http:  HTTP-based servers (default, most common)
  • stdio: Stdio-based servers (local process communication)
"#)
}
