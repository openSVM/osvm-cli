use clap::{Arg, Command};

/// Build the balance command
pub fn build_balance_command() -> Command {
    Command::new("balance")
        .about("Check SOL balance for an address")
        .long_about(
            "Check the SOL balance for a Solana address.\n\
                           \n\
                           If no address is provided, shows the balance of the configured keypair.\n\
                           \n\
                           Examples:\n\
                           • osvm balance                                    # Your configured wallet\n\
                           • osvm balance 4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T # Specific address",
        )
        .arg(
            Arg::new("address")
                .value_name("ADDRESS")
                .help("Solana address to check balance for (defaults to configured keypair)")
                .index(1),
        )
}
