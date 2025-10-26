use clap::{Arg, ArgAction, Command};

/// Build the agent command
pub fn build_agent_command() -> Command {
    Command::new("agent")
        .about("Execute agent commands with AI planning and MCP tool execution")
        .long_about(
            "Execute a single agent command with AI-powered planning and tool execution.\n\
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
                           • osvm agent \"Deploy a validator node\"",
        )
        .arg(
            Arg::new("prompt")
                .value_name("PROMPT")
                .help("The prompt or command for the agent to execute")
                .required(true)
                .num_args(1..)
                .index(1),
        )
        .arg(
            Arg::new("json")
                .long("json")
                .action(ArgAction::SetTrue)
                .help("Output results in JSON format"),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .help("Show detailed execution steps"),
        )
        .arg(
            Arg::new("no-tools")
                .long("no-tools")
                .action(ArgAction::SetTrue)
                .help("Disable MCP tool execution (AI response only)"),
        )
        .arg(
            Arg::new("timeout")
                .long("timeout")
                .value_name("SECONDS")
                .default_value("30")
                .help("Maximum execution time in seconds"),
        )
}
