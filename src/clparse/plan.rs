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
}
