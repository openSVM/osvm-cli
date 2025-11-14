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
        .subcommand(
            Command::new("demo")
                .about("Run a demonstration of the research agent")
        )
}