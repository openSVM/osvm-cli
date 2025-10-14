//! Database (ClickHouse) command definition

use clap::{Arg, ArgAction, Command};

pub fn build_database_command() -> Command {
    Command::new("db")
        .about("Manage ClickHouse database for blockchain data indexing")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("init")
                .about("Initialize ClickHouse database")
                .arg(
                    Arg::new("data-dir")
                        .long("data-dir")
                        .value_name("PATH")
                        .help("Custom data directory path"),
                ),
        )
        .subcommand(Command::new("start").about("Start ClickHouse database server"))
        .subcommand(Command::new("stop").about("Stop ClickHouse database server"))
        .subcommand(
            Command::new("status").about("Check ClickHouse database server status"),
        )
        .subcommand(
            Command::new("query")
                .about("Execute SQL query on ClickHouse database")
                .arg(
                    Arg::new("query")
                        .long("query")
                        .short('q')
                        .value_name("SQL")
                        .required(true)
                        .help("SQL query to execute"),
                ),
        )
        .subcommand(
            Command::new("activity")
                .about("View activity logs (CLI commands and chat history)")
                .arg(
                    Arg::new("stats")
                        .long("stats")
                        .action(ArgAction::SetTrue)
                        .help("Show activity statistics"),
                )
                .arg(
                    Arg::new("commands")
                        .long("commands")
                        .action(ArgAction::SetTrue)
                        .help("Show CLI command history"),
                )
                .arg(
                    Arg::new("chat")
                        .long("chat")
                        .action(ArgAction::SetTrue)
                        .help("Show chat message history"),
                )
                .arg(
                    Arg::new("limit")
                        .long("limit")
                        .short('n')
                        .value_name("COUNT")
                        .default_value("100")
                        .help("Limit number of results"),
                )
                .arg(
                    Arg::new("session-id")
                        .long("session-id")
                        .value_name("ID")
                        .help("Filter by session ID"),
                ),
        )
        .subcommand(
            Command::new("sync")
                .about("Sync blockchain data from snapshots/ledger to ClickHouse")
                .arg(
                    Arg::new("mode")
                        .long("mode")
                        .value_name("MODE")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "last-30-days",
                            "full-historical",
                            "realtime",
                        ]))
                        .default_value("last-30-days")
                        .help("Sync mode"),
                )
                .arg(
                    Arg::new("programs")
                        .long("programs")
                        .value_name("PUBKEYS")
                        .help("Comma-separated list of program IDs to index"),
                )
                .arg(
                    Arg::new("accounts")
                        .long("accounts")
                        .value_name("PUBKEYS")
                        .help("Comma-separated list of account pubkeys to index"),
                )
                .arg(
                    Arg::new("pattern")
                        .long("pattern")
                        .value_name("HEX")
                        .help("Hex byte pattern to match in account data (e.g., 0x1234abcd)"),
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
}
