use clap::{Arg, ArgAction, Command};

/// Build the realtime command
pub fn build_realtime_command() -> Command {
    Command::new("realtime")
        .about("Manage real-time blockchain data sync daemon")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("start")
                .about("Start real-time sync daemon")
                .arg(
                    Arg::new("programs")
                        .long("programs")
                        .value_name("PUBKEYS")
                        .help("Comma-separated list of program IDs to monitor"),
                )
                .arg(
                    Arg::new("accounts")
                        .long("accounts")
                        .value_name("PUBKEYS")
                        .help("Comma-separated list of account pubkeys to monitor"),
                )
                .arg(
                    Arg::new("patterns")
                        .long("patterns")
                        .value_name("HEX")
                        .help("Hex byte patterns to match (e.g., 0x1234abcd)"),
                )
                .arg(
                    Arg::new("ledger-path")
                        .long("ledger-path")
                        .value_name("PATH")
                        .help("Custom ledger path"),
                )
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Custom snapshot directory"),
                ),
        )
        .subcommand(Command::new("stop").about("Stop real-time sync daemon"))
        .subcommand(Command::new("status").about("Check real-time sync daemon status"))
}
