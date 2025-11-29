use clap::{Arg, Command};

/// Build the research subcommand
pub fn build_research_command() -> Command {
    Command::new("research")
        .about("Perform intelligent wallet activity research with AI self-evaluation")
        .long_about(
            "Conducts multi-phase blockchain investigation with iterative AI evaluation.\n\
            The agent automatically:\n\
            - Executes investigation phases (initial, profiling, analysis, pattern recognition)\n\
            - Self-evaluates findings using context-aware AI prompts\n\
            - Adapts investigation strategy based on discoveries\n\
            - Generates and tests hypotheses\n\
            - Produces comprehensive investigation reports"
        )
        .arg(
            Arg::new("wallet")
                .help("The wallet address to investigate")
                .required(true)
                .value_name("ADDRESS")
                .index(1)
        )
        .arg(
            Arg::new("depth")
                .short('d')
                .long("depth")
                .help("Investigation depth (1-10, default: 5)")
                .value_name("LEVEL")
                .default_value("5")
        )
        .arg(
            Arg::new("save")
                .short('s')
                .long("save")
                .help("Save the investigation report to a file")
                .action(clap::ArgAction::SetTrue)
        )
        .arg(
            Arg::new("focus")
                .short('f')
                .long("focus")
                .help("Focus area for investigation")
                .value_name("AREA")
                .value_parser(["trading", "defi", "nft", "mev", "general"])
                .default_value("general")
        )
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .help("Show detailed investigation progress (use -vv or -vvv for more detail)")
                .action(clap::ArgAction::Count)
        )
        .arg(
            Arg::new("agent")
                .long("agent")
                .help("Use advanced multi-iteration research agent (more thorough but slower)")
                .action(clap::ArgAction::SetTrue)
        )
        .arg(
            Arg::new("stream")
                .long("stream")
                .alias("realtime")
                .help("Enable real-time streaming visualization (graph updates progressively)")
                .action(clap::ArgAction::SetTrue)
        )
        .arg(
            Arg::new("tui")
                .long("tui")
                .help("Launch interactive TUI (Terminal User Interface) with real-time graph visualization")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("stream")
        )
        .arg(
            Arg::new("auto")
                .long("auto")
                .short('a')
                .help("Run autonomous investigation in CLI mode (no TUI, no user input)")
                .long_help(
                    "Runs a fully autonomous BFS investigation in headless CLI mode.\n\
                    Prints progress with reasoning and steps to stdout.\n\
                    Generates a report file at the end.\n\n\
                    Example: osvm research <WALLET> --auto\n\
                    Example: osvm research <WALLET> --auto --query \"Is this a MEV bot?\"\n\
                    Example: osvm research <WALLET> -a -d 3 --save"
                )
                .action(clap::ArgAction::SetTrue)
                .conflicts_with_all(["tui", "stream"])
        )
        .arg(
            Arg::new("query")
                .long("query")
                .short('q')
                .help("Investigation query or hypothesis (used with --auto)")
                .value_name("QUERY")
                .requires("auto")
        )
        .arg(
            Arg::new("max-wallets")
                .long("max-wallets")
                .help("Maximum wallets to explore in autonomous mode (default: 50)")
                .value_name("COUNT")
                .default_value("50")
        )
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .help("Output file path for the report (default: ~/.osvm/reports/)")
                .value_name("PATH")
        )
        .arg(
            Arg::new("token")
                .short('t')
                .long("token")
                .help("Trace a specific token flow (mint address or symbol like USDC, SOL)")
                .long_help(
                    "Filter investigation to a specific token and trace its flow path.\n\
                    Shows how the token moved: A ─[500]→ B ─[450]→ C ─[400]→ D\n\n\
                    Example: osvm research <WALLET> --auto --token USDC\n\
                    Example: osvm research <WALLET> --auto -t So11111111111111111111111111111111111111112"
                )
                .value_name("TOKEN")
        )
        .subcommand(
            Command::new("demo")
                .about("Run a demonstration of the research agent")
        )
}