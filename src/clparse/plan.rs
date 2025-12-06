use clap::{Arg, ArgAction, Command};

/// Build the plan command
pub fn build_plan_command() -> Command {
    Command::new("plan")
        .about("Create an AI-powered execution plan for OSVM commands")
        .long_about(
            "Analyze a natural language request and generate an executable OSVM command plan.\n\
                           \n\
                           This command uses AI to understand your intent and suggests the appropriate\n\
                           OSVM commands to accomplish your goal.\n\
                           \n\
                           Examples:\n\
                           • osvm plan \"show me all validators\"\n\
                           • osvm plan \"check network health\"\n\
                           • osvm plan \"list my nodes\"",
        )
        .arg(
            Arg::new("query")
                .value_name("QUERY")
                .help("Natural language query describing what you want to do")
                .required(true)
                .num_args(1..)
                .index(1),
        )
        .arg(
            Arg::new("execute")
                .long("execute")
                .short('e')
                .action(ArgAction::SetTrue)
                .help("Execute the plan automatically (skip confirmation for safe commands)"),
        )
        .arg(
            Arg::new("yes")
                .long("yes")
                .short('y')
                .action(ArgAction::SetTrue)
                .help("Auto-confirm all commands including those requiring confirmation"),
        )
        .arg(
            Arg::new("json")
                .long("json")
                .action(ArgAction::SetTrue)
                .help("Output results in JSON format"),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm plan "show me all validators"
     AI generates a plan to query validator information.
     💡 Plan shows commands like: osvm nodes list --type validator

  2. osvm plan "check network health" --execute
     Generate AND execute the plan automatically.
     💡 Skips confirmation for read-only/safe commands.

  3. osvm plan "deploy a program to devnet" --execute
     Plan deployment steps and execute with confirmation.
     💡 Dangerous commands still require explicit approval.

  4. osvm plan "monitor my wallet for incoming transactions" --json
     Output structured plan in JSON format.
     💡 Useful for integrating with automation scripts.

  5. osvm plan "compare RPC latencies across providers"
     AI suggests multi-step benchmarking plan.
     💡 Plans can include multiple osvm commands in sequence.

  6. osvm plan "analyze the last 100 transactions on my wallet"
     Complex investigation query to analysis plan.
     💡 Suggests: osvm research <wallet> --depth 5

  7. osvm p "quick balance check"
     Short alias: 'osvm p' = 'osvm plan'
     💡 Also: 'osvm a' = 'osvm agent', 'osvm -p' = planning mode

  8. osvm plan "set up MCP for Solana" --execute --yes
     Auto-execute with auto-confirm for all steps.
     💡 --yes bypasses all confirmation prompts (use carefully!)

  9. osvm plan "install a validator on my server"
     Infrastructure planning for node deployment.
     💡 AI considers prerequisites, config, and startup commands.

 10. osvm plan "swap SOL to USDC and check balance"
     Multi-step workflow planning.
     💡 AI chains: osvm swap --from SOL --to USDC && osvm balance

💡 PLAN vs AGENT:
  • PLAN: Shows what it will do, asks for confirmation
  • AGENT: Executes immediately (use for trusted queries)

  Use PLAN when you want to review before execution.
  Use AGENT when you trust the AI to act directly.

PRO TIPS:
  • The AI understands Solana-specific terminology
  • You can reference wallet addresses, program IDs, etc.
  • Plans are generated using your configured MCP tools
  • Use --verbose for detailed AI reasoning
"#)
}
