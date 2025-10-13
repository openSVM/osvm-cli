//! Balance command implementation
//!
//! This module handles the `balance` subcommand which checks SOL balance for an address.

use crate::config::Config;
use crate::utils::arg_helpers;
use crate::utils::cli_error::CliError;
use clap::ArgMatches;
use solana_client::rpc_client::RpcClient;
use solana_sdk::native_token::Sol;

/// Execute the balance command
///
/// Checks the SOL balance for a specified address or the configured default signer.
///
/// # Arguments
///
/// * `matches` - The argument matches from clap for this subcommand
/// * `config` - The loaded configuration
/// * `rpc_client` - The RPC client to use for queries
///
/// # Returns
///
/// Result indicating success or failure
///
/// # Example
///
/// ```ignore
/// let result = execute(matches, &config, &rpc_client).await?;
/// ```
pub async fn execute(
    matches: &ArgMatches,
    config: &Config,
    rpc_client: &RpcClient,
) -> Result<(), CliError> {
    // Extract address, using default signer if not provided
    let address = match arg_helpers::get_optional_pubkey(matches, "address")? {
        Some(addr) => addr,
        None => config.default_signer.pubkey(),
    };

    // Get balance from RPC
    let balance = rpc_client
        .get_balance_with_commitment(&address, config.commitment_config)
        .map_err(|e| CliError::rpc(format!("Failed to get balance: {}", e)))?
        .value;

    // Display result
    println!("{} has a balance of {}", address, Sol(balance));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use solana_sdk::pubkey::Pubkey;
    use std::str::FromStr;

    #[test]
    fn test_pubkey_parsing() {
        // Test that we can parse a valid pubkey
        let pubkey_str = "11111111111111111111111111111111";
        let pubkey = Pubkey::from_str(pubkey_str);
        assert!(pubkey.is_ok());
    }
}
