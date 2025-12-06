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
        .after_help(r#"
TOP 10 POPULAR USAGES:
━━━━━━━━━━━━━━━━━━━━━━━━

  1. osvm deploy target/deploy/program.so --program-id ./program-keypair.json \
       --owner ./owner.json --fee ./fee-payer.json
     Deploy a new program with all required keypairs.
     💡 Program ID keypair must be NEW for first deployment.

  2. osvm deploy program.so --program-id ./program.json --owner ./owner.json \
       --fee ./payer.json --network devnet
     Deploy only to devnet (skip mainnet/testnet).
     💡 Always test on devnet first!

  3. osvm deploy program.so ... --publish-idl --idl-file ./target/idl/program.json
     Deploy with Anchor IDL for client SDK generation.
     💡 IDL enables TypeScript/Rust SDK auto-generation.

  4. osvm deploy program.so ... --json
     JSON output for CI/CD pipeline integration.
     💡 Captures: program ID, deploy slot, fees paid.

  5. osvm deploy program.so ... --retry-attempts 5
     Increase retries for unreliable networks.
     💡 Default is 3; increase for congested networks.

  6. osvm deploy program.so ... --confirm-large
     Prompt before deploying large programs.
     💡 Large programs cost more to deploy and upgrade.

  7. osvm deploy program.so ... --network all
     Deploy to mainnet, testnet, AND devnet.
     💡 Useful for multi-network protocols.

  8. cargo build-sbf && osvm deploy ./target/deploy/myprogram.so ...
     Build and deploy in one command chain.
     💡 cargo build-sbf compiles Rust to Solana BPF.

  9. osvm ovsm compile script.ovsm -o script.so && osvm deploy script.so ...
     Deploy OVSM-compiled programs.
     💡 OVSM scripts can compile to deployable BPF!

 10. osvm deploy program.so ... 2>&1 | tee deploy.log
     Log deployment output for auditing.
     💡 Keep records of all mainnet deployments.

⚠️  SECURITY BEST PRACTICES:
  • NEVER commit keypairs to git
  • Use separate fee payer for deployments
  • Test thoroughly on devnet before mainnet
  • Keep upgrade authority keypair secure offline
  • Verify program hash after deployment

KEYPAIR REQUIREMENTS:
  • --program-id: New keypair (deploy) or address JSON (upgrade)
  • --owner:      Private key required (signs upgrade auth)
  • --fee:        Private key required (pays transaction fees)

DEPLOYMENT COSTS:
  Program size affects cost. Rough estimates:
  • 100KB: ~1 SOL
  • 500KB: ~5 SOL
  • 1MB+:  ~10+ SOL
"#)
}
