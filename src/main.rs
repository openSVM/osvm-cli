//! @brief Main entry point for CLI

use {
    crate::prelude::{
        burn_instruction, load_account, load_wallet, mint_transaction, ping_instruction,
        transfer_instruction, unpack_account_data, KEYS_DB, PROG_KEY,
    },
    clparse::parse_command_line,
    crate::utils::{svm_info, ssh_deploy, dashboard, nodes, examples},
    solana_clap_utils::{
        input_parsers::pubkey_of, input_validators::normalize_to_url_if_moniker,
        keypair::DefaultSigner,
    },
    solana_client::rpc_client::RpcClient,
    solana_remote_wallet::remote_wallet::RemoteWalletManager,
    solana_sdk::{
        commitment_config::CommitmentConfig,
        instruction::AccountMeta,
        native_token::Sol,
        signature::{Keypair, Signer},
        system_instruction,
    },
    std::{process::exit, sync::Arc, env, str::FromStr},
};
pub mod clparse;
pub mod prelude;
pub mod utils;

/// Space allocated for account state
const ACCOUNT_STATE_SPACE: usize = 1024;

struct Config {
    commitment_config: CommitmentConfig,
    default_signer: Box<dyn Signer>,
    json_rpc_url: String,
    verbose: u8, // 0=normal, 1=verbose (-v), 2=very verbose (-vv), 3=debug (-vvv)
    no_color: bool,
}

/// Wallet and account verification and load
///
/// Will search KEYS_DB for existence of the owner string and return the wallet and account keys
/// and, optionally, fund the wallet and create and initialize the account if needed
///
/// # Example
/// ```ignore
/// validate_user_account_and_load(&rpc_client, funding_source, commitment_config, "User1")?;
/// ```
fn validate_user_accounts_and_load(
    rpc_client: &RpcClient,
    funding_source: &dyn Signer,
    commitment_config: CommitmentConfig,
    owner: &str,
) -> Result<(&'static Keypair, &'static Keypair), Box<dyn std::error::Error>> {
    // Check in KEYS_DB for owner
    let (wallet, account) = KEYS_DB.wallet_and_account(owner.to_string())?;
    // Fund wallet if required
    load_wallet(rpc_client, wallet, funding_source, commitment_config)?;
    // Create and initialize account if required
    load_account(
        rpc_client,
        account,
        wallet,
        &PROG_KEY.pubkey(),
        ACCOUNT_STATE_SPACE as u64,
        commitment_config,
    )?;
    Ok((wallet, account))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app_matches = parse_command_line();
    let (sub_command, sub_matches) = app_matches.subcommand();
    let matches = sub_matches.unwrap();
    let mut wallet_manager: Option<Arc<RemoteWalletManager>> = None;

    // Check if colors should be disabled (via flag or environment variable)
    let no_color = matches.is_present("no_color") || env::var("NO_COLOR").is_ok();
    if no_color {
        // Disable colored output globally for the colored crate
        colored::control::set_override(false);
    }

    let config = {
        let cli_config = if let Some(config_file) = matches.value_of("config_file") {
            solana_cli_config::Config::load(config_file).unwrap_or_default()
        } else {
            solana_cli_config::Config::load("~/.config/osvm/config.yml").unwrap_or_default()
        };

        let default_signer = DefaultSigner::new(
            "keypair".to_string(),
            matches
                .value_of(&"keypair")
                .map(|s| s.to_string())
                .unwrap_or_else(|| cli_config.keypair_path.clone()),
        );

        Config {
            json_rpc_url: normalize_to_url_if_moniker(
                matches
                    .value_of("json_rpc_url")
                    .unwrap_or(&cli_config.json_rpc_url)
                    .to_string(),
            ),
            default_signer: default_signer
                .signer_from_path(matches, &mut wallet_manager)
                .unwrap_or_else(|err| {
                    eprintln!("error: {}", err);
                    exit(1);
                }),
            // Count occurrences of the verbose flag to determine verbosity level
            verbose: matches.occurrences_of("verbose") as u8,
            no_color,
            commitment_config: CommitmentConfig::confirmed(),
        }
    };

    // Set up logger with appropriate filter based on verbosity level
    match config.verbose {
        0 => solana_logger::setup_with_default("solana=info"),
        1 => solana_logger::setup_with_default("solana=debug"),
        2 => solana_logger::setup_with_default("solana=debug,program=trace"),
        _ => solana_logger::setup_with_default("solana=trace,program=trace"), // Level 3 or higher
    }

    // Display information based on verbosity level
    if config.verbose > 0 {
        println!("JSON RPC URL: {}", config.json_rpc_url);
        
        if config.verbose >= 2 {
            println!("Using keypair: {}", config.default_signer.pubkey());
            println!("Commitment level: {:?}", config.commitment_config.commitment);
            
            if config.verbose >= 3 {
                // Most detailed level - show configuration details
                let rpc_client = RpcClient::new(config.json_rpc_url.clone());
                let balance = rpc_client.get_balance_with_commitment(
                    &config.default_signer.pubkey(),
                    config.commitment_config
                )?;
                println!("Wallet balance: {} SOL", Sol(balance.value));
            }
        }
    }
    let rpc_client = RpcClient::new(config.json_rpc_url.clone());

    match (sub_command, sub_matches) {
        ("balance", Some(arg_matches)) => {
            let address =
                pubkey_of(arg_matches, "address").unwrap_or_else(|| config.default_signer.pubkey());
            println!(
                "{} has a balance of {}",
                address,
                Sol(rpc_client
                    .get_balance_with_commitment(&address, config.commitment_config)?
                    .value)
            );
        }
        ("mint", Some(_arg_matchs)) => {
            let owner = matches.value_of("to-owner").unwrap();
            let key = matches.value_of("key").unwrap();
            let value = {
                let value: Vec<_> = matches.values_of("value").unwrap().collect();
                value.join(" ")
            };
            // Verify the owner is a valid account
            let (wallet, account) = validate_user_accounts_and_load(
                &rpc_client,
                config.default_signer.as_ref(),
                config.commitment_config,
                owner,
            )?;
            // Execute command
            mint_transaction(
                &rpc_client,
                &[
                    AccountMeta::new(account.pubkey(), false),
                    AccountMeta::new(wallet.pubkey(), true),
                ],
                wallet,
                key,
                &value,
                config.commitment_config,
            )?;
            let (_, btree) = unpack_account_data(&rpc_client, account, config.commitment_config)?;
            println!("{} to account key/value store {:?}", owner, btree);
        }
        ("transfer", Some(_arg_matchs)) => {
            let from_owner = matches.value_of("from-owner").unwrap();
            let to_owner = matches.value_of("to-owner").unwrap();
            let key = matches.value_of("key").unwrap();
            // Verify that from and to owners are different and both are
            // valid
            let (from_wallet, from_account) = validate_user_accounts_and_load(
                &rpc_client,
                config.default_signer.as_ref(),
                config.commitment_config,
                from_owner,
            )?;
            let (_, to_account) = validate_user_accounts_and_load(
                &rpc_client,
                config.default_signer.as_ref(),
                config.commitment_config,
                to_owner,
            )?;
            // Execute command
            transfer_instruction(
                &rpc_client,
                &[
                    AccountMeta::new(from_account.pubkey(), false),
                    AccountMeta::new(to_account.pubkey(), false),
                    AccountMeta::new(from_wallet.pubkey(), true),
                ],
                from_wallet,
                key,
                config.commitment_config,
            )?;
            let (_, btree) =
                unpack_account_data(&rpc_client, from_account, config.commitment_config)?;
            println!("{} from account key/value store {:?}", from_owner, btree);
            let (_, btree) =
                unpack_account_data(&rpc_client, to_account, config.commitment_config)?;
            println!("{} to account key/value store {:?}", to_owner, btree);
        }
        ("burn", Some(_arg_matchs)) => {
            let owner = matches.value_of("from-owner").unwrap();
            let key = matches.value_of("key").unwrap();
            // Verify the owner is a valid account
            let (wallet, account) = validate_user_accounts_and_load(
                &rpc_client,
                config.default_signer.as_ref(),
                config.commitment_config,
                owner,
            )?;
            // Execute command
            burn_instruction(
                &rpc_client,
                &[
                    AccountMeta::new(account.pubkey(), false),
                    AccountMeta::new(wallet.pubkey(), true),
                ],
                wallet,
                key,
                config.commitment_config,
            )?;
            let (_, btree) = unpack_account_data(&rpc_client, account, config.commitment_config)?;
            println!("{} from account key/value store {:?}", owner, btree);
        }
        ("svm", Some(svm_matches)) => {
            let (svm_sub_command, svm_sub_matches) = svm_matches.subcommand();
            match (svm_sub_command, svm_sub_matches) {
                ("list", _) => {
                    // List all SVMs
                    let svms = svm_info::list_all_svms(&rpc_client, config.commitment_config)?;  
                    svm_info::display_svm_list(&svms);
                }
                ("dashboard", _) => {
                    // Launch the interactive dashboard
                    match dashboard::run_dashboard(&rpc_client, config.commitment_config) {
                        Ok(_) => println!("Dashboard closed"),
                        Err(e) => {
                            eprintln!("Error running dashboard: {}", e);
                            exit(1);
                        }
                    }
                }
                ("get", Some(get_matches)) => {
                    // Get details for a specific SVM
                    let name = get_matches.value_of("name").unwrap();
                    match svm_info::get_svm_info(&rpc_client, name, config.commitment_config) {
                        Ok(info) => svm_info::display_svm_info(&info),
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            exit(1);
                        }
                    }
                }
                ("install", Some(install_matches)) => {
                    // Install an SVM on a remote host
                    let svm_name = install_matches.value_of("name").unwrap();
                    let host = install_matches.value_of("host").unwrap();
                    
                    // First get SVM info to verify it exists and can be installed
                    match svm_info::get_svm_info(&rpc_client, svm_name, config.commitment_config) {
                        Ok(info) => {
                            if !info.can_install_validator && !info.can_install_rpc {
                                eprintln!("SVM '{}' cannot be installed", svm_name);
                                exit(1);
                            }
                            
                            // Default to installing as validator on mainnet
                            match ssh_deploy::deploy_node(
                                host, 
                                svm_name,
                                "validator",
                                ssh_deploy::NetworkType::Mainnet
                            ) {
                                Ok(node_id) => println!("Successfully installed {} as node {}", svm_name, node_id),
                                Err(e) => eprintln!("Installation failed: {}", e),
                            }
                        },
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            exit(1);
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        ("nodes", Some(nodes_matches)) => {
            let (node_sub_command, node_sub_matches) = nodes_matches.subcommand();
            match (node_sub_command, node_sub_matches) {
                ("list", Some(list_matches)) => {
                    // List all nodes
                    let network = list_matches.value_of("network").unwrap_or("all");
                    let node_type = list_matches.value_of("type").unwrap_or("all");
                    let status = list_matches.value_of("status").unwrap_or("all");
                    let svm = list_matches.value_of("svm");
                    let json_output = list_matches.is_present("json");
                    
                    match nodes::list_all_nodes(&rpc_client, network, svm, node_type, status, config.commitment_config, config.verbose) {
                        Ok(node_list) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&node_list).unwrap());
                            } else {
                                nodes::display_node_list(&node_list, config.verbose);
                            }
                        },
                        Err(e) => {
                            eprintln!("Error listing nodes: {}", e);
                            exit(1);
                        }
                    }
                },
                ("dashboard", _) => {
                    // Launch node monitoring dashboard
                    match nodes::run_dashboard(&rpc_client, config.commitment_config, config.verbose) {
                        Ok(_) => println!("Node dashboard closed"),
                        Err(e) => {
                            eprintln!("Error running node dashboard: {}", e);
                            exit(1);
                        }
                    }
                },
                ("status", Some(status_matches)) => {
                    // Check node status
                    let node_id = status_matches.value_of("node-id").unwrap();
                    let json_output = status_matches.is_present("json");
                    
                    match nodes::get_node_status(node_id) {
                        Ok(status) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&status).unwrap());
                            } else {
                                nodes::display_node_status(node_id, &status, config.verbose);
                            }
                        },
                        Err(e) => {
                            eprintln!("Error getting node status: {}", e);
                            exit(1);
                        }
                    }
                },
                ("get", Some(get_matches)) => {
                    // Get detailed node information
                    let node_id = get_matches.value_of("node-id").unwrap();
                    let json_output = get_matches.is_present("json");
                    
                    match nodes::get_node_info(&rpc_client, node_id, config.commitment_config) {
                        Ok(info) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&info).unwrap());
                            } else {
                                nodes::display_node_info(&info, config.verbose);
                            }
                        },
                        Err(e) => {
                            eprintln!("Error getting node info: {}", e);
                            exit(1);
                        }
                    }
                },
                ("restart", Some(restart_matches)) => {
                    // Restart a node
                    let node_id = restart_matches.value_of("node-id").unwrap();
                    match nodes::restart_node(node_id) {
                        Ok(_) => println!("Node {} restarted successfully", node_id),
                        Err(e) => {
                            eprintln!("Error restarting node: {}", e);
                            exit(1);
                        }
                    }
                },
                ("stop", Some(stop_matches)) => {
                    // Stop a node
                    let node_id = stop_matches.value_of("node-id").unwrap();
                    match nodes::stop_node(node_id) {
                        Ok(_) => println!("Node {} stopped successfully", node_id),
                        Err(e) => {
                            eprintln!("Error stopping node: {}", e);
                            exit(1);
                        }
                    }
                },
                ("logs", Some(logs_matches)) => {
                    // View node logs
                    let node_id = logs_matches.value_of("node-id").unwrap();
                    let lines = logs_matches.value_of("lines").unwrap().parse::<usize>().unwrap_or(100);
                    let follow = logs_matches.is_present("follow");
                    
                    match nodes::get_node_logs(node_id, lines, follow) {
                        Ok(_) => {
                            if follow {
                                // For follow mode, the function won't return until user interrupts
                                println!("Log streaming ended");
                            }
                        },
                        Err(e) => {
                            eprintln!("Error getting node logs: {}", e);
                            exit(1);
                        }
                    }
                },
                _ => unreachable!(),
            }
        }
        ("examples", Some(examples_matches)) => {
            // Handle the examples command
            if examples_matches.is_present("list_categories") {
                // List all available example categories
                println!("Available example categories:");
                println!("  basic       - Basic Commands");
                println!("  svm         - SVM Management");
                println!("  node        - Node Deployment");
                println!("  monitoring  - Node Monitoring and Management");
                println!("  workflow    - Common Workflows");
                println!("\nUse 'osvm examples --category <name>' to show examples for a specific category.");
            } 
            else if let Some(category) = examples_matches.value_of("category") {
                // Display examples for a specific category
                examples::display_category_by_name(category);
            } 
            else {
                // Display all examples
                examples::display_all_examples();
            }
        }
        ("ping", Some(_arg_matches)) => {
            let signature = ping_instruction(
                &rpc_client,
                config.default_signer.as_ref(),
                config.commitment_config,
            )
            .unwrap_or_else(|err| {
                eprintln!("error: send transaction: {}", err);
                exit(1);
            });
            println!("Signature: {}", signature);
        }
        // Handle SSH deployment (format: osvm user@host --svm svm1,svm2)
        (conn_str, _) if conn_str.contains('@') && matches.is_present("svm") => {
            // This is an SSH deployment command
            let svm_list = matches.value_of("svm").unwrap();
            let node_type_str = matches.value_of("node-type").unwrap();
            let network_str = matches.value_of("network").unwrap();
            
            // Parse connection string
            let connection = match ssh_deploy::ServerConfig::from_connection_string(conn_str) {
                Ok(conn) => conn,
                Err(e) => {
                    eprintln!("Error parsing SSH connection string: {}", e);
                    exit(1);
                }
            };
            
            // Parse node type
            let node_type = node_type_str.to_string();
            
            // Parse network type
            let _network = match network_str.to_lowercase().as_str() {
                "mainnet" => ssh_deploy::NetworkType::Mainnet,
                "testnet" => ssh_deploy::NetworkType::Testnet,
                "devnet" => ssh_deploy::NetworkType::Devnet,
                _ => {
                    eprintln!("Invalid network: {}", network_str);
                    exit(1);
                }
            };
            
            // Parse SVM list
            let svm_types = svm_list.split(',').map(|s| s.trim().to_string()).collect::<Vec<_>>();
            if svm_types.is_empty() {
                eprintln!("No SVMs specified");
                exit(1);
            }
            
            // Create deployment config
            let deploy_config = ssh_deploy::DeploymentConfig {
                svm_type: svm_types[0].clone(),
                node_type: node_type.to_string(),
                network: ssh_deploy::NetworkType::Mainnet,
                node_name: "default".to_string(),
                rpc_url: None,
                additional_params: std::collections::HashMap::new(),
            };

            if let Err(e) = ssh_deploy::deploy_svm_node(connection, deploy_config, None).await {
                eprintln!("Deployment error: {}", e);
                exit(1);
            }
        }
        (cmd, _) => {
            eprintln!("Unknown command: {}", cmd);
            exit(1);
        }
    };

    Ok(())
}

#[cfg(test)]
mod test {
    use borsh::{BorshDeserialize, BorshSerialize};
    use solana_sdk::pubkey::Pubkey;

    use {super::*, solana_test_validator::*};

    #[test]
    fn test_ping() {
        let (test_validator, payer) = TestValidatorGenesis::default().start();
        let rpc_client = test_validator.get_rpc_client();

        assert!(matches!(
            ping_instruction(&rpc_client, &payer, CommitmentConfig::confirmed()),
            Ok(_)
        ));
    }

    #[test]
    fn test_borsh() {
        #[repr(C)]
        #[derive(BorshSerialize, BorshDeserialize, PartialEq, Debug, Clone)]
        pub struct UpdateMetadataAccountArgs {
            pub data: Option<String>,
            pub update_authority: Option<Pubkey>,
            pub primary_sale_happened: Option<bool>,
        }
        let faux = UpdateMetadataAccountArgs {
            data: Some(String::from("This")),
            update_authority: Some(Pubkey::default()),
            primary_sale_happened: Some(true),
        };
        let bout = faux.try_to_vec().unwrap();
        println!("{:?}", bout);
        let in_faux = UpdateMetadataAccountArgs::try_from_slice(&bout).unwrap();
        println!("{:?}", in_faux);
    }
}
