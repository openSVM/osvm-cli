//! @brief Account state access

use {
    crate::utils::txn_utils::get_account_for,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{commitment_config::CommitmentConfig, signature::Keypair, signer::Signer},
    std::{collections::BTreeMap, error::Error},
};

/// Unpacks token state for the accumulator
pub fn unpack_account_data(
    rpc_client: &RpcClient,
    account: &Keypair,
    commitment_config: CommitmentConfig,
) -> Result<(bool, BTreeMap<String, String>), Box<dyn Error>> {
    match get_account_for(rpc_client, &account.pubkey(), commitment_config) {
        Some(_account) => {
            // Implement custom unpacking logic here instead of using sol_template_shared
            Ok((false, BTreeMap::new()))
        },
        None => Err(Box::<dyn Error>::from(format!(
            "account not found for \"{:?}\". ",
            account
        ))),
    }
}
