use crate::config::Config;
use solana_client::rpc_client::RpcClient;
use solana_sdk::{native_token::Sol, pubkey::Pubkey};
use std::str::FromStr;

pub async fn handle_balance_command(
    matches: &clap::ArgMatches,
    rpc_client: &RpcClient,
    config: &Config,
) -> Result<(), Box<dyn std::error::Error>> {
    let address = matches
        .get_one::<String>("address")
        .and_then(|s| Pubkey::from_str(s).ok())
        .unwrap_or_else(|| config.default_signer.pubkey());

    println!(
        "{} has a balance of {}",
        address,
        Sol(rpc_client
            .get_balance_with_commitment(&address, config.commitment_config)?
            .value)
    );

    Ok(())
}
