//! Tutorial command builder

use clap::{Arg, ArgAction, Command};

pub fn build_tutorial_command() -> Command {
    Command::new("tutorial")
        .about("Interactive tutorial for new users")
        .long_about(
            "Start an interactive tutorial that guides you through OSVM CLI basics.\n\n\
                    This tutorial covers:\n\
                    • Installation verification\n\
                    • System health checks\n\
                    • OVSM LISP language basics\n\
                    • Examples and REPL usage\n\
                    • AI chat interface\n\n\
                    Estimated time: 5-10 minutes",
        )
        .arg(
            Arg::new("skip")
                .long("skip")
                .value_name("STEPS")
                .action(ArgAction::Append)
                .help("Skip specific tutorial steps (comma-separated numbers)"),
        )
        .arg(
            Arg::new("quick")
                .long("quick")
                .short('q')
                .action(ArgAction::SetTrue)
                .help("Quick tutorial mode (skip optional steps)"),
        )
        .after_help(
            "EXAMPLES:\n\
            osvm tutorial                 # Start full tutorial\n\
            osvm tutorial --quick         # Quick tutorial (skip optional steps)\n\
            osvm tutorial --skip 3,5      # Skip steps 3 and 5\n\n\
            This tutorial is designed for first-time users and takes 5-10 minutes.",
        )
}
