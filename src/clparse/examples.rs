use clap::{Arg, ArgAction, Command};

/// Build the examples command
pub fn build_examples_command() -> Command {
    Command::new("examples")
        .about("Show usage examples for OSVM CLI commands")
        .arg(
            Arg::new("category")
                .long("category")
                .short('c')
                .value_name("CATEGORY")
                .help("Filter examples by category (basic, svm, node, monitoring, workflow)"),
        )
        .arg(
            Arg::new("list_categories")
                .long("list-categories")
                .action(ArgAction::SetTrue)
                .help("List all available example categories"),
        )
}
