//! Snapshot management command definition

use clap::{Arg, ArgAction, Command};

pub fn build_snapshot_command() -> Command {
    Command::new("snapshot")
        .about("Analyze and manage Solana snapshots")
        .arg_required_else_help(true)
        .subcommand(
            Command::new("read")
                .about("Read and display snapshot accounts")
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory (default: ~/.config/osvm/ledgers/devnet/remote/extracted)"),
                )
                .arg(
                    Arg::new("limit")
                        .long("limit")
                        .short('n')
                        .value_name("COUNT")
                        .help("Limit number of accounts to display"),
                )
                .arg(
                    Arg::new("offset")
                        .long("offset")
                        .value_name("COUNT")
                        .default_value("0")
                        .help("Skip first N accounts"),
                )
                .arg(
                    Arg::new("parallel")
                        .long("parallel")
                        .action(ArgAction::SetTrue)
                        .help("Enable parallel processing"),
                )
                .arg(
                    Arg::new("threads")
                        .long("threads")
                        .value_name("COUNT")
                        .help("Number of threads for parallel processing (default: CPU count)"),
                )
                .arg(
                    Arg::new("filter-owner")
                        .long("filter-owner")
                        .value_name("PUBKEY")
                        .help("Filter accounts by owner program"),
                )
                .arg(
                    Arg::new("filter-min-balance")
                        .long("filter-min-balance")
                        .value_name("LAMPORTS")
                        .help("Filter accounts with balance >= LAMPORTS"),
                )
                .arg(
                    Arg::new("filter-max-balance")
                        .long("filter-max-balance")
                        .value_name("LAMPORTS")
                        .help("Filter accounts with balance <= LAMPORTS"),
                )
                .arg(
                    Arg::new("filter-min-size")
                        .long("filter-min-size")
                        .value_name("BYTES")
                        .help("Filter accounts with data size >= BYTES"),
                )
                .arg(
                    Arg::new("filter-max-size")
                        .long("filter-max-size")
                        .value_name("BYTES")
                        .help("Filter accounts with data size <= BYTES"),
                )
                .arg(
                    Arg::new("filter-executable")
                        .long("filter-executable")
                        .action(ArgAction::SetTrue)
                        .help("Filter only executable accounts"),
                )
                .arg(
                    Arg::new("filter-rent-exempt")
                        .long("filter-rent-exempt")
                        .action(ArgAction::SetTrue)
                        .help("Filter only rent-exempt accounts"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                )
                .arg(
                    Arg::new("quiet")
                        .long("quiet")
                        .short('q')
                        .action(ArgAction::SetTrue)
                        .help("Suppress non-essential output"),
                ),
        )
        .subcommand(
            Command::new("stats")
                .about("Show comprehensive snapshot statistics")
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        .subcommand(
            Command::new("export")
                .about("Export snapshot to various formats")
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory"),
                )
                .arg(
                    Arg::new("output")
                        .long("output")
                        .short('o')
                        .value_name("FILE")
                        .required(true)
                        .help("Output file path"),
                )
                .arg(
                    Arg::new("format")
                        .long("format")
                        .short('f')
                        .value_name("FORMAT")
                        .value_parser(clap::builder::PossibleValuesParser::new([
                            "json", "csv", "parquet", "msgpack",
                        ]))
                        .default_value("json")
                        .help("Export format"),
                )
                .arg(
                    Arg::new("filter-owner")
                        .long("filter-owner")
                        .value_name("PUBKEY")
                        .help("Filter accounts by owner program"),
                )
                .arg(
                    Arg::new("filter-min-balance")
                        .long("filter-min-balance")
                        .value_name("LAMPORTS")
                        .help("Filter accounts with balance >= LAMPORTS"),
                )
                .arg(
                    Arg::new("filter-max-balance")
                        .long("filter-max-balance")
                        .value_name("LAMPORTS")
                        .help("Filter accounts with balance <= LAMPORTS"),
                ),
        )
        .subcommand(
            Command::new("compare")
                .about("Compare two snapshots and show differences")
                .arg(
                    Arg::new("snapshot1")
                        .value_name("SNAPSHOT1")
                        .required(true)
                        .index(1)
                        .help("First snapshot directory"),
                )
                .arg(
                    Arg::new("snapshot2")
                        .value_name("SNAPSHOT2")
                        .required(true)
                        .index(2)
                        .help("Second snapshot directory"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        .subcommand(
            Command::new("validate")
                .about("Validate snapshot integrity")
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        .subcommand(
            Command::new("find")
                .about("Find specific account by pubkey")
                .arg(
                    Arg::new("pubkey")
                        .value_name("PUBKEY")
                        .required(true)
                        .index(1)
                        .help("Account public key to search for"),
                )
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory"),
                )
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Output in JSON format"),
                ),
        )
        .subcommand(
            Command::new("interactive")
                .about("Launch interactive TUI for snapshot exploration")
                .arg(
                    Arg::new("snapshot-dir")
                        .long("snapshot-dir")
                        .value_name("PATH")
                        .help("Path to snapshot directory"),
                ),
        )
        .subcommand(
            Command::new("download")
                .about("Download mainnet snapshot to remote server via SSH")
                .arg(
                    Arg::new("connection")
                        .value_name("USER@HOST")
                        .required(true)
                        .index(1)
                        .help("SSH connection string (user@host[:port])"),
                )
                .arg(
                    Arg::new("network")
                        .long("network")
                        .value_name("NETWORK")
                        .default_value("mainnet")
                        .help("Network to download snapshot from (mainnet, devnet, testnet)"),
                )
                .arg(
                    Arg::new("ledger-dir")
                        .long("ledger-dir")
                        .value_name("PATH")
                        .help("Target ledger directory on remote server [default: /opt/osvm/solana/ledger]"),
                ),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm snapshot stats
     Show comprehensive snapshot statistics.
     💡 Account count, total lamports, program distribution.

  2. osvm snapshot read --limit 100
     Read first 100 accounts from snapshot.
     💡 Quick preview of snapshot contents.

  3. osvm snapshot find <pubkey>
     Find specific account by public key.
     💡 Returns: balance, owner, data size, rent epoch.

  4. osvm snapshot read --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
     Filter accounts by SPL Token program.
     💡 Find all token accounts in the snapshot.

  5. osvm snapshot export -o accounts.parquet --format parquet
     Export to Parquet for data analysis.
     💡 Parquet is efficient for large-scale analytics.

  6. osvm snapshot compare snap1/ snap2/
     Compare two snapshots and show differences.
     💡 Detect account changes between slots.

  7. osvm snapshot validate
     Check snapshot integrity.
     💡 Verifies hashes and account consistency.

  8. osvm snapshot interactive
     Launch TUI for exploring snapshot.
     💡 Navigate accounts with keyboard shortcuts.

  9. osvm snapshot download user@server.com --network mainnet
     Download mainnet snapshot to remote server.
     💡 Useful for bootstrapping new validators.

 10. osvm snapshot read --filter-min-balance 1000000000 --parallel
     Find accounts with ≥1 SOL using parallel processing.
     💡 1 SOL = 1,000,000,000 lamports

💡 WHAT IS A SNAPSHOT?
  A Solana snapshot is a compressed archive of the entire
  account database at a specific slot. Validators use snapshots
  to quickly bootstrap without replaying from genesis.

  Snapshot contents:
  • All account data (pubkey, lamports, owner, data)
  • Bank state (slot, epoch, fees)
  • Program accounts and BPF bytecode

COMMON FILTERS:
  --filter-owner <PUBKEY>      Filter by program owner
  --filter-min-balance <LAM>   Minimum lamports
  --filter-max-balance <LAM>   Maximum lamports
  --filter-min-size <BYTES>    Minimum data size
  --filter-executable          Only executable accounts

EXPORT FORMATS:
  • json:     Standard JSON (large files)
  • csv:      Spreadsheet compatible
  • parquet:  Columnar, best for analytics
  • msgpack:  Binary, compact size
"#)
}
