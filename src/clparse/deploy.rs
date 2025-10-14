use clap::{Arg, ArgAction, Command};

/// Build the deploy command
pub fn build_deploy_command() -> Command {
    Command::new("deploy")
        .about("Deploy eBPF binary to all available SVM networks")
        .arg(
            Arg::new("binary")
                .value_name("BINARY_PATH")
                .help("Path to the eBPF binary file (.so)")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("program-id")
                .long("program-id")
                .value_name("PROGRAM_ID_PATH")
                .help("Path to program keypair JSON file (for new deployments) or program address JSON file (for upgrades)")
                .required(true),
        )
        .arg(
            Arg::new("owner")
                .long("owner")
                .value_name("OWNER_PATH")
                .help("Path to program owner keypair JSON file (must contain private key)")
                .required(true),
        )
        .arg(
            Arg::new("fee")
                .long("fee")
                .value_name("FEE_PAYER_PATH")
                .help("Path to deployment fee payer keypair JSON file (must contain private key)")
                .required(true),
        )
        .arg(
            Arg::new("publish-idl")
                .long("publish-idl")
                .action(ArgAction::SetTrue)
                .help("Publish IDL alongside the program deployment"),
        )
        .arg(
            Arg::new("idl-file")
                .long("idl-file")
                .value_name("IDL_PATH")
                .help("Path to Anchor IDL JSON file (optional, defaults to generated IDL)"),
        )
        .arg(
            Arg::new("network")
                .long("network")
                .value_name("NETWORK")
                .value_parser(clap::builder::PossibleValuesParser::new([
                    "mainnet", "testnet", "devnet", "all",
                ]))
                .default_value("all")
                .help("Network to deploy on (default: deploy to all networks)"),
        )
        .arg(
            Arg::new("json")
                .long("json")
                .action(ArgAction::SetTrue)
                .help("Output results in JSON format for machine-readable processing"),
        )
        .arg(
            Arg::new("retry-attempts")
                .long("retry-attempts")
                .value_name("COUNT")
                .default_value("3")
                .help("Number of retry attempts for failed deployments (default: 3)"),
        )
        .arg(
            Arg::new("confirm-large")
                .long("confirm-large")
                .action(ArgAction::SetTrue)
                .help("Require confirmation for deploying large binaries (>1MB)"),
        )
}
