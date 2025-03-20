use {
    crate::utils::{dashboard, examples, nodes, ssh_deploy, svm_info},
    clparse::parse_command_line,
    solana_clap_utils::input_validators::normalize_to_url_if_moniker,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{commitment_config::CommitmentConfig, native_token::Sol, signature::Signer},
    std::{env, process::exit, str::FromStr},
};

// Helper function to handle the type mismatch between clap v2 and v4
fn pubkey_of_checked(matches: &clap::ArgMatches, name: &str) -> Option<solana_sdk::pubkey::Pubkey> {
    matches
        .get_one::<String>(name)
        .map(|s| s.as_str())
        .and_then(|s| solana_sdk::pubkey::Pubkey::from_str(s).ok())
}

#[cfg(feature = "remote-wallet")]
use solana_remote_wallet::remote_wallet::RemoteWalletManager;
pub mod clparse;
pub mod prelude;
pub mod utils;

struct Config {
    commitment_config: CommitmentConfig,
    default_signer: Box<dyn Signer>,
    json_rpc_url: String,
    verbose: u8, // 0=normal, 1=verbose (-v), 2=very verbose (-vv), 3=debug (-vvv)
    #[allow(dead_code)]
    no_color: bool,
}

// Wrapper type for webpki::Error to implement traits
#[derive(Debug)]
struct WebPkiError(webpki::Error);

impl std::fmt::Display for WebPkiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WebPkiError: {:?}", self.0)
    }
}

impl std::error::Error for WebPkiError {}

// Convert from webpki::Error to WebPkiError
impl From<webpki::Error> for WebPkiError {
    fn from(error: webpki::Error) -> Self {
        WebPkiError(error)
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app_matches = parse_command_line();
    let Some((sub_command, sub_matches)) = app_matches.subcommand() else {
        eprintln!("No subcommand provided");
        exit(1);
    };
    let matches = sub_matches;

    let mut wallet_manager = None;

    // Check if colors should be disabled (via flag or environment variable)
    let no_color = matches.contains_id("no_color") || env::var("NO_COLOR").is_ok();
    if no_color {
        // Disable colored output globally for the colored crate
        colored::control::set_override(false);
    }

    let config = {
        let cli_config = if let Some(config_file) =
            matches.get_one::<String>("config_file").map(|s| s.as_str())
        {
            solana_cli_config::Config::load(config_file).unwrap_or_default()
        } else {
            solana_cli_config::Config::load("~/.config/osvm/config.yml").unwrap_or_default()
        };

        let keypair_path = matches
            .get_one::<String>("keypair")
            .map(|s| s.to_string())
            .unwrap_or_else(|| cli_config.keypair_path.clone());

        // Create a signer directly from the keypair path
        let signer = solana_sdk::signature::read_keypair_file(&keypair_path)
            .unwrap_or_else(|err| {
                eprintln!("Error reading keypair file {}: {}", keypair_path, err);
                exit(1);
            });

        Config {
            json_rpc_url: normalize_to_url_if_moniker(
                matches
                    .get_one::<String>("json_rpc_url")
                    .map(|s| s.as_str())
                    .unwrap_or(&cli_config.json_rpc_url),
            ),
            default_signer: Box::new(signer),
            // Count occurrences of the verbose flag to determine verbosity level
            verbose: matches.get_count("verbose") as u8,
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
            println!(
                "Commitment level: {:?}",
                config.commitment_config.commitment
            );

            if config.verbose >= 3 {
                // Most detailed level - show configuration details
                let rpc_client = RpcClient::new(config.json_rpc_url.clone());
                let balance = rpc_client.get_balance_with_commitment(
                    &config.default_signer.pubkey(),
                    config.commitment_config,
                )?;
                println!("Wallet balance: {} SOL", Sol(balance.value));
            }
        }
    }
    let rpc_client = RpcClient::new(config.json_rpc_url.clone());

    match sub_command {
        "balance" => {
            let address = pubkey_of_checked(matches, "address")
                .unwrap_or_else(|| config.default_signer.pubkey());

            println!(
                "{} has a balance of {}",
                address,
                Sol(rpc_client
                    .get_balance_with_commitment(&address, config.commitment_config)?
                    .value)
            );
        }
        "svm" => {
            let Some((svm_sub_command, svm_sub_matches)) = matches.subcommand() else {
                eprintln!("No SVM subcommand provided");
                exit(1);
            };

            match svm_sub_command {
                "list" => {
                    // List all SVMs
                    let svms = svm_info::list_all_svms(&rpc_client, config.commitment_config)?;
                    svm_info::display_svm_list(&svms);
                }
                "dashboard" => {
                    // Launch the interactive dashboard
                    match dashboard::run_dashboard(&rpc_client, config.commitment_config) {
                        Ok(_) => println!("Dashboard closed"),
                        Err(e) => {
                            eprintln!("Error running dashboard: {}", e);
                            exit(1);
                        }
                    }
                }
                "get" => {
                    // Get details for a specific SVM
                    let name = svm_sub_matches
                        .get_one::<String>("name")
                        .map(|s| s.as_str())
                        .unwrap();
                    match svm_info::get_svm_info(&rpc_client, name, config.commitment_config) {
                        Ok(info) => svm_info::display_svm_info(&info),
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            exit(1);
                        }
                    }
                }
                "install" => {
                    // Install an SVM on a remote host
                    let svm_name = svm_sub_matches
                        .get_one::<String>("name")
                        .map(|s| s.as_str())
                        .unwrap();
                    let host = svm_sub_matches
                        .get_one::<String>("host")
                        .map(|s| s.as_str())
                        .unwrap();

                    println!("Installing SVM: {}", svm_name);
                    println!("Host: {}", host);

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
                                ssh_deploy::NetworkType::Mainnet,
                            ) {
                                Ok(node_id) => {
                                    println!("Installation complete");
                                    println!(
                                        "Successfully installed {} as node {}",
                                        svm_name, node_id
                                    );
                                }
                                Err(e) => eprintln!("Installation failed: {}", e),
                            }
                        }
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            exit(1);
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        "nodes" => {
            let Some((node_sub_command, node_sub_matches)) = matches.subcommand() else {
                eprintln!("No node subcommand provided");
                exit(1);
            };
            
            match node_sub_command {
                "list" => {
                    // List all nodes
                    let network = node_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("all");
                    let node_type = node_sub_matches
                        .get_one::<String>("type")
                        .map(|s| s.as_str())
                        .unwrap_or("all");
                    let status = node_sub_matches
                        .get_one::<String>("status")
                        .map(|s| s.as_str())
                        .unwrap_or("all");
                    let svm = node_sub_matches.get_one::<String>("svm").map(|s| s.as_str());
                    let json_output = node_sub_matches.contains_id("json");

                    match nodes::list_all_nodes(
                        &rpc_client,
                        network,
                        svm,
                        node_type,
                        status,
                        config.commitment_config,
                        config.verbose,
                    ) {
                        Ok(node_list) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&node_list).unwrap());
                            } else {
                                nodes::display_node_list(&node_list, config.verbose);
                            }
                        }
                        Err(e) => {
                            eprintln!("Error listing nodes: {}", e);
                            exit(1);
                        }
                    }
                }
                "dashboard" => {
                    // Launch node monitoring dashboard
                    match nodes::run_dashboard(
                        &rpc_client,
                        config.commitment_config,
                        config.verbose,
                    ) {
                        Ok(_) => println!("Node dashboard closed"),
                        Err(e) => {
                            eprintln!("Error running node dashboard: {}", e);
                            exit(1);
                        }
                    }
                }
                "status" => {
                    // Check node status
                    let node_id = node_sub_matches
                        .get_one::<String>("node-id")
                        .map(|s| s.as_str())
                        .unwrap();
                    let json_output = node_sub_matches.contains_id("json");

                    match nodes::get_node_status(node_id) {
                        Ok(status) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&status).unwrap());
                            } else {
                                nodes::display_node_status(node_id, &status, config.verbose);
                            }
                        }
                        Err(e) => {
                            eprintln!("Error getting node status: {}", e);
                            exit(1);
                        }
                    }
                }
                "get" => {
                    // Get detailed node information
                    let node_id = node_sub_matches
                        .get_one::<String>("node-id")
                        .map(|s| s.as_str())
                        .unwrap();
                    let json_output = node_sub_matches.contains_id("json");

                    match nodes::get_node_info(&rpc_client, node_id, config.commitment_config) {
                        Ok(info) => {
                            if json_output {
                                println!("{}", serde_json::to_string_pretty(&info).unwrap());
                            } else {
                                nodes::display_node_info(&info, config.verbose);
                            }
                        }
                        Err(e) => {
                            eprintln!("Error getting node info: {}", e);
                            exit(1);
                        }
                    }
                }
                "restart" => {
                    // Restart a node
                    let node_id = node_sub_matches
                        .get_one::<String>("node-id")
                        .map(|s| s.as_str())
                        .unwrap();
                    match nodes::restart_node(node_id) {
                        Ok(_) => println!("Node {} restarted successfully", node_id),
                        Err(e) => {
                            eprintln!("Error restarting node: {}", e);
                            exit(1);
                        }
                    }
                }
                "stop" => {
                    // Stop a node
                    let node_id = node_sub_matches
                        .get_one::<String>("node-id")
                        .map(|s| s.as_str())
                        .unwrap();
                    match nodes::stop_node(node_id) {
                        Ok(_) => println!("Node {} stopped successfully", node_id),
                        Err(e) => {
                            eprintln!("Error stopping node: {}", e);
                            exit(1);
                        }
                    }
                }
                "logs" => {
                    // View node logs
                    let node_id = node_sub_matches
                        .get_one::<String>("node-id")
                        .map(|s| s.as_str())
                        .unwrap();
                    let lines = node_sub_matches
                        .get_one::<String>("lines")
                        .map(|s| s.parse::<usize>().unwrap_or(100))
                        .unwrap_or(100);
                    let follow = node_sub_matches.contains_id("follow");

                    match nodes::get_node_logs(node_id, lines, follow) {
                        Ok(_) => {
                            if follow {
                                // For follow mode, the function won't return until user interrupts
                                println!("Log streaming ended");
                            }
                        }
                        Err(e) => {
                            eprintln!("Error getting node logs: {}", e);
                            exit(1);
                        }
                    }
                }
                "deploy" => {
                    // Deploy a new node
                    let svm = node_sub_matches
                        .get_one::<String>("svm")
                        .map(|s| s.as_str())
                        .unwrap();
                    let node_type = node_sub_matches
                        .get_one::<String>("type")
                        .map(|s| s.as_str())
                        .unwrap_or("validator");
                    let network = node_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("mainnet");
                    let host = node_sub_matches
                        .get_one::<String>("host")
                        .map(|s| s.as_str())
                        .unwrap();
                    let name = node_sub_matches
                        .get_one::<String>("name")
                        .map(|s| s.as_str())
                        .unwrap_or("default");

                    // Parse network type
                    let network_type = match network.to_lowercase().as_str() {
                        "mainnet" => ssh_deploy::NetworkType::Mainnet,
                        "testnet" => ssh_deploy::NetworkType::Testnet,
                        "devnet" => ssh_deploy::NetworkType::Devnet,
                        _ => {
                            eprintln!("Invalid network: {}", network);
                            exit(1);
                        }
                    };

                    let deploy_config = nodes::DeployNodeConfig::new(svm, node_type, network_type)
                        .with_name(name)
                        .with_host(host);

                    match nodes::deploy_node(&rpc_client, deploy_config).await {
                        Ok(node_info) => {
                            println!("Node deployed successfully: {:?}", node_info);
                        }
                        Err(e) => {
                            eprintln!("Error deploying node: {}", e);
                            exit(1);
                        }
                    }
                }
                _ => {
                    eprintln!("Unknown node command: {}", node_sub_command);
                    exit(1);
                }
            }
        }
        "examples" => {
            // Handle the examples command
            if matches.contains_id("list_categories") {
                // List all available example categories
                println!("Available example categories:");
                println!("  basic       - Basic Commands");
                println!("  svm         - SVM Management");
                println!("  node        - Node Deployment");
                println!("  monitoring  - Node Monitoring and Management");
                println!("  workflow    - Common Workflows");
                println!("\nUse 'osvm examples --category <name>' to show examples for a specific category.");
            } else if let Some(category) = matches.get_one::<String>("category").map(|s| s.as_str())
            {
                // Display examples for a specific category
                examples::display_category_by_name(category);
            } else {
                // Display all examples
                examples::display_all_examples();
            }
        }
        "solana" => {
            let Some((solana_sub_command, solana_sub_matches)) = matches.subcommand() else {
                eprintln!("No solana subcommand provided");
                exit(1);
            };

            match solana_sub_command {
                "validator" => {
                    // Deploy a Solana validator with enhanced features
                    let connection_str = solana_sub_matches
                        .get_one::<String>("connection")
                        .map(|s| s.as_str())
                        .unwrap();
                    let network_str = solana_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("mainnet");
                    let version = solana_sub_matches
                        .get_one::<String>("version")
                        .map(|s| s.as_str())
                        .map(|s| s.to_string());
                    let client_type = solana_sub_matches
                        .get_one::<String>("client-type")
                        .map(|s| s.to_string());
                    let hot_swap_enabled = solana_sub_matches.contains_id("hot-swap");
                    let metrics_config = solana_sub_matches
                        .get_one::<String>("metrics-config")
                        .map(|s| s.to_string());

                    // Parse connection string
                    let connection =
                        match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
                            Ok(conn) => conn,
                            Err(e) => {
                                eprintln!("Error parsing SSH connection string: {}", e);
                                exit(1);
                            }
                        };

                    // Parse network type
                    let network = match network_str.to_lowercase().as_str() {
                        "mainnet" => ssh_deploy::NetworkType::Mainnet,
                        "testnet" => ssh_deploy::NetworkType::Testnet,
                        "devnet" => ssh_deploy::NetworkType::Devnet,
                        _ => {
                            eprintln!("Invalid network: {}", network_str);
                            exit(1);
                        }
                    };

                    // Create disk configuration if both disk params are provided
                    let disk_config = if solana_sub_matches.contains_id("ledger-disk")
                        && solana_sub_matches.contains_id("accounts-disk")
                    {
                        Some(ssh_deploy::DiskConfig {
                            ledger_disk: solana_sub_matches
                                .get_one::<String>("ledger-disk")
                                .map(|s| s.as_str())
                                .unwrap()
                                .to_string(),
                            accounts_disk: solana_sub_matches
                                .get_one::<String>("accounts-disk")
                                .map(|s| s.as_str())
                                .unwrap()
                                .to_string(),
                        })
                    } else {
                        None
                    };

                    // Create deployment config with enhanced features
                    let deploy_config = ssh_deploy::DeploymentConfig {
                        svm_type: "solana".to_string(),
                        node_type: "validator".to_string(),
                        network,
                        node_name: format!("solana-validator-{}", network_str),
                        rpc_url: None,
                        additional_params: std::collections::HashMap::new(),
                        version,
                        client_type,
                        hot_swap_enabled,
                        metrics_config,
                        disk_config,
                    };

                    println!("Deploying Solana validator node to {}...", connection_str);
                    println!("Network: {}", network_str);
                    if let Some(ver) = &deploy_config.version {
                        println!("Version: {}", ver);
                    }
                    if let Some(client) = &deploy_config.client_type {
                        println!("Client type: {}", client);
                    }
                    if deploy_config.hot_swap_enabled {
                        println!("Hot-swap capability: Enabled");
                    }
                    if let Some(disks) = &deploy_config.disk_config {
                        println!("Disk configuration:");
                        println!("  Ledger disk: {}", disks.ledger_disk);
                        println!("  Accounts disk: {}", disks.accounts_disk);
                    }

                    if let Err(e) =
                        ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
                    {
                        eprintln!("Deployment error: {}", e);
                        exit(1);
                    }

                    println!("Solana validator node deployed successfully!");
                }
                "rpc" => {
                    let solana_sub_matches = solana_sub_matches;
                    // Deploy a Solana RPC node with enhanced features
                    let connection_str = solana_sub_matches
                        .get_one::<String>("connection")
                        .map(|s| s.as_str())
                        .unwrap();
                    let network_str = solana_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("mainnet");
                    let version = solana_sub_matches
                        .get_one::<String>("version")
                        .map(|s| s.as_str())
                        .map(|s| s.to_string());
                    let client_type = solana_sub_matches
                        .get_one::<String>("client-type")
                        .map(|s| s.as_str())
                        .map(|s| s.to_string());
                    let enable_history = solana_sub_matches.contains_id("enable-history");
                    let metrics_config = solana_sub_matches
                        .get_one::<String>("metrics-config")
                        .map(|s| s.as_str())
                        .map(|s| s.to_string());

                    // Parse connection string
                    let connection =
                        match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
                            Ok(conn) => conn,
                            Err(e) => {
                                eprintln!("Error parsing SSH connection string: {}", e);
                                exit(1);
                            }
                        };

                    // Parse network type
                    let network = match network_str.to_lowercase().as_str() {
                        "mainnet" => ssh_deploy::NetworkType::Mainnet,
                        "testnet" => ssh_deploy::NetworkType::Testnet,
                        "devnet" => ssh_deploy::NetworkType::Devnet,
                        _ => {
                            eprintln!("Invalid network: {}", network_str);
                            exit(1);
                        }
                    };

                    // Create disk configuration if both disk params are provided
                    let disk_config = if solana_sub_matches.contains_id("ledger-disk")
                        && solana_sub_matches.contains_id("accounts-disk")
                    {
                        Some(ssh_deploy::DiskConfig {
                            ledger_disk: solana_sub_matches
                                .get_one::<String>("ledger-disk")
                                .map(|s| s.as_str())
                                .unwrap()
                                .to_string(),
                            accounts_disk: solana_sub_matches
                                .get_one::<String>("accounts-disk")
                                .map(|s| s.as_str())
                                .unwrap()
                                .to_string(),
                        })
                    } else {
                        None
                    };

                    // Create additional params for RPC-specific options
                    let mut additional_params = std::collections::HashMap::new();
                    if enable_history {
                        additional_params.insert("enable_history".to_string(), "true".to_string());
                    }

                    // Create deployment config with enhanced features
                    let deploy_config = ssh_deploy::DeploymentConfig {
                        svm_type: "solana".to_string(),
                        node_type: "rpc".to_string(),
                        network,
                        node_name: format!("solana-rpc-{}", network_str),
                        rpc_url: None,
                        additional_params,
                        version,
                        client_type,
                        hot_swap_enabled: false, // Not needed for RPC nodes
                        metrics_config,
                        disk_config,
                    };

                    println!("Deploying Solana RPC node to {}...", connection_str);
                    println!("Network: {}", network_str);
                    if let Some(ver) = &deploy_config.version {
                        println!("Version: {}", ver);
                    }
                    if let Some(client) = &deploy_config.client_type {
                        println!("Client type: {}", client);
                    }
                    if enable_history {
                        println!("Transaction history: Enabled");
                    }
                    if let Some(disks) = &deploy_config.disk_config {
                        println!("Disk configuration:");
                        println!("  Ledger disk: {}", disks.ledger_disk);
                        println!("  Accounts disk: {}", disks.accounts_disk);
                    }

                    if let Err(e) =
                        ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
                    {
                        eprintln!("Deployment error: {}", e);
                        exit(1);
                    }

                    println!("Solana RPC node deployed successfully!");
                }
                _ => {
                    eprintln!("Unknown Solana command: {}", solana_sub_command);
                    exit(1);
                }
            }
        }
        "rpc" => {
            let Some((rpc_sub_command, rpc_sub_matches)) = matches.subcommand() else {
                eprintln!("No RPC subcommand provided");
                exit(1);
            };

            match rpc_sub_command {
                "sonic" => {
                    // Deploy a Sonic RPC node
                    let connection_str = rpc_sub_matches
                        .get_one::<String>("connection")
                        .map(|s| s.as_str())
                        .unwrap();
                    let network_str = rpc_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap();

                    // Parse connection string
                    let connection =
                        match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
                            Ok(conn) => conn,
                            Err(e) => {
                                eprintln!("Error parsing SSH connection string: {}", e);
                                exit(1);
                            }
                        };

                    // Parse network type
                    let network = match network_str.to_lowercase().as_str() {
                        "mainnet" => ssh_deploy::NetworkType::Mainnet,
                        "testnet" => ssh_deploy::NetworkType::Testnet,
                        "devnet" => ssh_deploy::NetworkType::Devnet,
                        _ => {
                            eprintln!("Invalid network: {}", network_str);
                            exit(1);
                        }
                    };

                    // Create deployment config
                    let deploy_config = ssh_deploy::DeploymentConfig {
                        svm_type: "sonic".to_string(),
                        node_type: "rpc".to_string(),
                        network,
                        node_name: "sonic-rpc".to_string(),
                        rpc_url: None,
                        additional_params: std::collections::HashMap::new(),
                        version: None,
                        client_type: None,
                        hot_swap_enabled: false,
                        metrics_config: None,
                        disk_config: None,
                    };

                    println!("Deploying Sonic RPC node to {}...", connection_str);

                    if let Err(e) =
                        ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
                    {
                        eprintln!("Deployment error: {}", e);
                        exit(1);
                    }

                    println!("Sonic RPC node deployed successfully!");
                }
                "solana" => {
                    // Use the enhanced Solana deployment via rpc subcommand
                    let connection_str = rpc_sub_matches
                        .get_one::<String>("connection")
                        .map(|s| s.as_str())
                        .unwrap();
                    let network_str = rpc_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("mainnet");

                    // Parse connection string
                    let connection =
                        match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
                            Ok(conn) => conn,
                            Err(e) => {
                                eprintln!("Error parsing SSH connection string: {}", e);
                                exit(1);
                            }
                        };

                    // Parse network type
                    let network = match network_str.to_lowercase().as_str() {
                        "mainnet" => ssh_deploy::NetworkType::Mainnet,
                        "testnet" => ssh_deploy::NetworkType::Testnet,
                        "devnet" => ssh_deploy::NetworkType::Devnet,
                        _ => {
                            eprintln!("Invalid network: {}", network_str);
                            exit(1);
                        }
                    };

                    // Create deployment config
                    let deploy_config = ssh_deploy::DeploymentConfig {
                        svm_type: "solana".to_string(),
                        node_type: "rpc".to_string(),
                        network,
                        node_name: format!("solana-rpc-{}", network_str),
                        rpc_url: None,
                        additional_params: std::collections::HashMap::new(),
                        version: None,
                        client_type: None,
                        hot_swap_enabled: false,
                        metrics_config: None,
                        disk_config: None,
                    };

                    println!("Deploying Solana RPC node to {}...", connection_str);

                    if let Err(e) =
                        ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
                    {
                        eprintln!("Deployment error: {}", e);
                        exit(1);
                    }

                    println!("Solana RPC node deployed successfully!");
                }
                _ => {
                    eprintln!("Unknown RPC type: {}", rpc_sub_command);
                    exit(1);
                }
            }
        }
        // Handle SSH deployment (format: osvm user@host --svm svm1,svm2)
        conn_str if conn_str.contains('@') && matches.contains_id("svm") => {
            // This is an SSH deployment command
            let svm_list = matches
                .get_one::<String>("svm")
                .map(|s| s.as_str())
                .unwrap();
            let node_type_str = matches
                .get_one::<String>("node-type")
                .map(|s| s.as_str())
                .unwrap();
            let network_str = matches
                .get_one::<String>("network")
                .map(|s| s.as_str())
                .unwrap();

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
            let network = match network_str.to_lowercase().as_str() {
                "mainnet" => ssh_deploy::NetworkType::Mainnet,
                "testnet" => ssh_deploy::NetworkType::Testnet,
                "devnet" => ssh_deploy::NetworkType::Devnet,
                _ => {
                    eprintln!("Invalid network: {}", network_str);
                    exit(1);
                }
            };

            // Parse SVM list
            let svm_types = svm_list
                .split(',')
                .map(|s| s.trim().to_string())
                .collect::<Vec<_>>();
            if svm_types.is_empty() {
                eprintln!("No SVMs specified");
                exit(1);
            }

            // Create deployment config
            let deploy_config = ssh_deploy::DeploymentConfig {
                svm_type: svm_types[0].clone(),
                node_type: node_type.to_string(),
                network,
                node_name: "default".to_string(),
                rpc_url: None,
                additional_params: std::collections::HashMap::new(),
                version: None,
                client_type: None,
                hot_swap_enabled: false,
                metrics_config: None,
                disk_config: None,
            };

            if let Err(e) = ssh_deploy::deploy_svm_node(connection, deploy_config, None).await {
                eprintln!("Deployment failed: {}", e);
                exit(1);
            }
        }
        "new_feature_command" => {
            println!("Expected output for new feature");
        }
        cmd => {
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
        // With borsh 1.5.5, we need to use BorshSerialize in a different way
        let mut bout = Vec::new();
        faux.serialize(&mut bout).unwrap();
        // With borsh 1.5.5, use the BorshDeserialize trait method
        let in_faux = UpdateMetadataAccountArgs::deserialize(&mut &bout[..]).unwrap();

        // Assert that the deserialized data matches the original
        assert_eq!(faux, in_faux);
    }
}