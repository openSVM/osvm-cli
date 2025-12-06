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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm balance
     Check your configured wallet's SOL balance.
     💡 Uses keypair from ~/.config/solana/id.json or --keypair flag.

  2. osvm balance 4Nd1mBQtrMJVYVfKf2PJy9NZUZdTAsp7D4xWLs4gDB4T
     Check any wallet's balance by its public key.
     💡 Solana addresses are base58-encoded 32-byte public keys.

  3. osvm balance --url devnet
     Check balance on devnet instead of mainnet.
     💡 Useful for testing - devnet SOL has no real value.

  4. osvm balance TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
     Check balance of the SPL Token Program.
     💡 Program accounts hold lamports for rent-exemption.

  5. osvm balance $(solana address)
     Use shell substitution to check your current wallet.
     💡 Combines with Solana CLI for scripting workflows.

  6. osvm balance --url https://api.mainnet-beta.solana.com
     Explicitly specify the RPC endpoint.
     💡 Useful when testing different RPC providers.

  7. osvm balance --keypair /path/to/keypair.json
     Check balance using a specific keypair file.
     💡 Never use your main wallet keypair for testing!

  8. osvm balance 11111111111111111111111111111111
     Check balance of the System Program (always 0).
     💡 This is the Native Program for account creation.

  9. osvm -u testnet balance
     Shorthand for checking testnet balance.
     💡 testnet, devnet, mainnet are valid monikers.

 10. osvm balance && osvm balance --url devnet
     Check both mainnet and devnet in one command.
     💡 Compare balances across networks for debugging.

💡 PRO TIPS:
  • Addresses are case-sensitive - double-check before sending funds!
  • The first 4 characters often identify the address type/purpose.
  • Use devnet for all testing - request free SOL at https://faucet.solana.com
"#)
        .arg(
            Arg::new("address")
                .value_name("ADDRESS")
                .help("Solana address to check balance for (defaults to configured keypair)")
                .index(1),
        )
}
