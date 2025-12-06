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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm agent "What's my wallet balance?"
     Natural language query - AI automatically calls balance tools.
     💡 The agent understands context and selects appropriate MCP tools.

  2. osvm agent "Show transactions for wallet ABC...XYZ in the last hour"
     Complex multi-step query with time filtering.
     💡 Agent chains getSignatures → getTransaction → format output.

  3. osvm agent "Compare gas fees between mainnet and devnet"
     Cross-network analysis query.
     💡 Agent calls RPC on multiple networks and synthesizes results.

  4. osvm agent "Find all NFTs owned by this wallet" --json
     Output structured JSON for programmatic processing.
     💡 Pipe to jq for filtering: | jq '.nfts[] | .name'

  5. osvm agent "Deploy my program to devnet" --verbose
     See step-by-step execution with detailed tool calls.
     💡 Great for debugging complex multi-tool workflows.

  6. osvm agent "Analyze this token: BONK" --timeout 60
     Extend timeout for complex analysis tasks.
     💡 Default is 30s; increase for large data queries.

  7. osvm agent "Explain what this transaction does: <sig>"
     Deep transaction analysis with human-readable output.
     💡 Agent parses instructions, accounts, and program calls.

  8. osvm agent "What's the best RPC endpoint for my region?"
     Infrastructure recommendation query.
     💡 Agent tests latency and suggests optimal endpoints.

  9. osvm agent "Send 0.1 SOL to <address>" --no-tools
     Get AI guidance without executing (safe planning mode).
     💡 Use --no-tools when you want instructions, not actions.

 10. osvm a "quick balance check"
     Short alias for agent command.
     💡 'osvm a' and 'osvm p' are shortcuts for agent/plan modes.

💡 PRO TIPS:
  • Always quote multi-word prompts: "your query here"
  • The agent has full access to all configured MCP servers
  • Use --verbose to understand the AI's reasoning
  • Combine with pipes: osvm agent "..." | grep "error"
  • For complex investigations, use 'osvm research' instead
"#)
}
