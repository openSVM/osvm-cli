use clap::{Arg, Command};

/// Build the SVM management command
pub fn build_svm_command() -> Command {
    Command::new("svm")
        .about("Manage Solana Virtual Machines (SVMs)")
        .arg_required_else_help(true)
        .subcommand(Command::new("list").about("List all SVMs installed in the chain"))
        .subcommand(Command::new("dashboard").about("Launch interactive SVM monitoring dashboard"))
        .subcommand(
            Command::new("get")
                .about("Get detailed information about a specific SVM")
                .arg(
                    Arg::new("name")
                        .value_name("NAME")
                        .index(1)
                        .required(true)
                        .help("Name of the SVM to get information about"),
                ),
        )
        .subcommand(
            Command::new("install")
                .about("🚧 COMING SOON: Install an SVM on a remote host")
                .arg(
                    Arg::new("name")
                        .value_name("NAME")
                        .index(1)
                        .required(true)
                        .help("Name of the SVM to install"),
                )
                .arg(
                    Arg::new("host")
                        .long("host")
                        .value_name("HOST")
                        .required(true)
                        .help("Remote host to install on (format: user@host[:port])"),
                ),
        )
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm svm list
     List all SVMs installed in the chain.
     💡 Shows: name, version, status, and resource usage.

  2. osvm svm dashboard
     Launch interactive SVM monitoring dashboard.
     💡 Real-time TUI with performance graphs and stats.

  3. osvm svm get solana-mainnet
     Get detailed info about a specific SVM.
     💡 Shows: validator count, slot height, TPS, health.

  4. osvm svm get firedancer-testnet
     Query Firedancer SVM instance status.
     💡 Firedancer is an alternative Solana validator client.

  5. osvm svm list | grep mainnet
     Filter SVMs by network type.
     💡 Pipe to grep for quick filtering.

  6. osvm svm install agave --host user@server.com
     [Coming Soon] Install Agave SVM on remote host.
     💡 Agave is the new name for Solana Labs client.

  7. osvm svm dashboard --refresh 5
     Dashboard with custom refresh rate (5 seconds).
     💡 Default is 10s; lower values for more responsiveness.

  8. watch osvm svm list
     Continuously monitor SVM status.
     💡 Use 'watch' command for simple auto-refresh.

  9. osvm svm get solana-devnet --json
     JSON output for programmatic parsing.
     💡 Pipe to jq: | jq '.health.status'

 10. osvm svm get mainnet && osvm svm get devnet
     Compare status across networks.
     💡 Chain commands to check multiple SVMs quickly.

💡 WHAT IS AN SVM?
  SVM = Solana Virtual Machine
  It's the execution environment where Solana smart contracts run.

  Different SVM implementations:
  • Solana Labs (Agave) - Original reference client
  • Firedancer (Jump Crypto) - High-performance alternative
  • Jito - MEV-enhanced validator client
  • Lite RPC - Lightweight RPC node

KEY METRICS TO WATCH:
  • Slot Height: Current blockchain position
  • TPS: Transactions per second
  • Health: Online, degraded, or offline
  • Version: Software version running
"#)
}
