use clap::{Arg, ArgAction, Command};

/// Build the invoke command
pub fn build_invoke_command() -> Command {
    Command::new("invoke")
        .about("Invoke a deployed Solana program with instruction data")
        .arg(
            Arg::new("program-id")
                .value_name("PROGRAM_ID")
                .help("Program ID to invoke (base58 encoded public key)")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("data")
                .long("data")
                .short('d')
                .value_name("HEX_DATA")
                .help("Instruction data as hex string (e.g., '0x1234abcd' or empty for no data)"),
        )
        .arg(
            Arg::new("accounts")
                .long("account")
                .short('a')
                .value_name("ACCOUNT")
                .action(ArgAction::Append)
                .help("Account in format 'pubkey:is_signer:is_writable' (e.g., '5Ys...:false:true')"),
        )
        .arg(
            Arg::new("skip-preflight")
                .long("skip-preflight")
                .action(ArgAction::SetTrue)
                .help("Skip preflight transaction simulation (useful for debugging on-chain errors)"),
        )
        .arg(
            Arg::new("show-logs")
                .long("show-logs")
                .action(ArgAction::SetTrue)
                .help("Fetch and display transaction logs after execution"),
        )
}
