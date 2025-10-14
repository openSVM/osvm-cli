use clap::{Arg, ArgAction, Command};

/// Build the doctor command
pub fn build_doctor_command() -> Command {
    Command::new("doctor")
        .about("Comprehensive system health check and repair")
        .arg(
            Arg::new("check_all")
                .long("check-all")
                .action(ArgAction::SetTrue)
                .help("Run comprehensive health check"),
        )
        .arg(
            Arg::new("fix")
                .long("fix")
                .action(ArgAction::SetTrue)
                .help("Attempt to fix detected issues automatically"),
        )
        .arg(
            Arg::new("system_only")
                .long("system-only")
                .action(ArgAction::SetTrue)
                .help("Check only system-level dependencies"),
        )
        .arg(
            Arg::new("user_only")
                .long("user-only")
                .action(ArgAction::SetTrue)
                .help("Check only user-level dependencies"),
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .action(ArgAction::Count)
                .help("Detailed diagnostic output"),
        )
}
