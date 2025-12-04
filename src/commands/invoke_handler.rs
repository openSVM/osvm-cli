use crate::config::Config;
use solana_client::rpc_client::RpcClient;
use solana_commitment_config::CommitmentConfig;
use solana_sdk::{
    instruction::{AccountMeta, Instruction},
    pubkey::Pubkey,
    signature::{read_keypair_file, Signer},
    transaction::Transaction,
};
use std::str::FromStr;

pub async fn handle_invoke_command(
    matches: &clap::ArgMatches,
    config: &Config,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 OSVM Program Invocation Tool");
    println!("================================\n");

    // Get program ID
    let program_id_str = matches
        .get_one::<String>("program-id")
        .ok_or("Program ID is required")?;
    let program_id = Pubkey::from_str(program_id_str)?;

    // Get keypair (fee payer/signer)
    let payer = if let Some(keypair_path) = matches.get_one::<String>("keypair") {
        Box::new(read_keypair_file(keypair_path)?) as Box<dyn Signer>
    } else {
        // Use config's default signer - need to create a new keypair from it
        // Since we can't clone the Box<dyn Signer>, we'll require explicit keypair path
        return Err("Keypair path is required. Use --keypair <PATH> or set SOLANA_CONFIG".into());
    };

    // Get instruction data (hex encoded, optional)
    let instruction_data = if let Some(data_hex) = matches.get_one::<String>("data") {
        hex::decode(data_hex.trim_start_matches("0x"))?
    } else {
        Vec::new() // Empty data
    };

    // Get RPC URL (use config's URL since --url is a global arg handled elsewhere)
    let rpc_url = &config.json_rpc_url;

    // Parse accounts if provided
    let mut accounts = Vec::new();
    if let Some(account_strings) = matches.get_many::<String>("accounts") {
        for account_str in account_strings {
            // Format: pubkey:is_signer:is_writable
            let parts: Vec<&str> = account_str.split(':').collect();
            if parts.len() != 3 {
                return Err(format!(
                    "Invalid account format: {}. Expected: pubkey:is_signer:is_writable",
                    account_str
                )
                .into());
            }

            let pubkey = Pubkey::from_str(parts[0])?;
            let is_signer = parts[1].parse::<bool>()?;
            let is_writable = parts[2].parse::<bool>()?;

            accounts.push(AccountMeta {
                pubkey,
                is_signer,
                is_writable,
            });
        }
    }

    // Display configuration
    println!("📋 Invocation Configuration:");
    println!("  Program ID: {}", program_id);
    println!("  Payer: {}", payer.pubkey());
    println!("  RPC URL: {}", rpc_url);
    println!("  Instruction data: {} bytes", instruction_data.len());
    println!("  Accounts: {}", accounts.len());
    if !accounts.is_empty() {
        for (i, account) in accounts.iter().enumerate() {
            println!(
                "    [{}] {} (signer: {}, writable: {})",
                i, account.pubkey, account.is_signer, account.is_writable
            );
        }
    }
    println!();

    // Create RPC client
    let client = RpcClient::new_with_commitment(rpc_url.to_string(), CommitmentConfig::confirmed());

    // Create instruction
    let instruction = Instruction {
        program_id,
        accounts,
        data: instruction_data,
    };

    // Get recent blockhash
    println!("⏳ Getting recent blockhash...");
    let recent_blockhash = client.get_latest_blockhash()?;

    // Create transaction
    let mut transaction = Transaction::new_with_payer(&[instruction], Some(&payer.pubkey()));
    transaction.sign(&[&payer], recent_blockhash);

    // Check if we should skip preflight (useful for debugging)
    let skip_preflight = matches.get_flag("skip-preflight");

    // Send transaction
    println!("📤 Sending transaction...\n");

    let signature = if skip_preflight {
        println!("⚠️  Skipping preflight checks (transaction may fail on-chain)");
        client.send_transaction(&transaction)?
    } else {
        client.send_and_confirm_transaction(&transaction)?
    };

    println!("✅ Transaction sent!");
    println!("Signature: {}\n", signature);

    // Determine cluster for explorer URL
    let cluster = if rpc_url.contains("devnet") {
        "devnet"
    } else if rpc_url.contains("testnet") {
        "testnet"
    } else if rpc_url.contains("localhost") || rpc_url.contains("127.0.0.1") {
        "custom"
    } else {
        "mainnet-beta"
    };

    println!("🔗 View on Solana Explorer:");
    if cluster == "custom" {
        println!(
            "https://explorer.solana.com/tx/{}?cluster=custom&customUrl={}",
            signature,
            urlencoding::encode(rpc_url)
        );
    } else {
        println!(
            "https://explorer.solana.com/tx/{}?cluster={}",
            signature, cluster
        );
    }

    // Fetch transaction logs if requested
    if matches.get_flag("show-logs") || skip_preflight {
        println!("\n📋 Fetching transaction logs...");
        match client.get_transaction(
            &signature,
            solana_transaction_status::UiTransactionEncoding::Json,
        ) {
            Ok(confirmed_tx) => {
                if let Some(meta) = confirmed_tx.transaction.meta {
                    // In Solana SDK 3.0, log_messages uses OptionSerializer enum
                    use solana_transaction_status::option_serializer::OptionSerializer;

                    if let OptionSerializer::Some(log_messages) = meta.log_messages {
                        println!("\n📝 Program Logs:");
                        for log in log_messages {
                            println!("  {}", log);
                        }
                    }
                    println!("\n💰 Transaction Details:");
                    println!("  Fee: {} lamports", meta.fee);
                    if let Some(err) = meta.err {
                        println!("  ❌ Error: {:?}", err);
                    } else {
                        println!("  ✅ Status: Success");
                    }
                }
            }
            Err(e) => {
                println!("⚠️  Could not fetch transaction details: {}", e);
            }
        }
    }

    Ok(())
}
