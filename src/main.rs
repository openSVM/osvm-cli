use {
    crate::utils::{dashboard, examples, nodes, ssh_deploy, svm_info, webpki_error::WebPkiError},
    clparse::parse_command_line,
    solana_clap_utils::{
        input_parsers::pubkey_of, input_validators::normalize_to_url_if_moniker,
        keypair::DefaultSigner,
    },
    solana_client::rpc_client::RpcClient,
    solana_sdk::{commitment_config::CommitmentConfig, native_token::Sol, signature::Signer},
    std::{env, process::exit},
};

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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app_matches = parse_command_line();
    let sub_command_opt = app_matches.subcommand();
    
    // Handle the case where there's no subcommand
    if sub_command_opt.is_none() {
        eprintln!("No subcommand provided");
        exit(1);
    }
    
    let (sub_command, sub_matches) = sub_command_opt.unwrap();
    let matches = sub_matches.unwrap();

    #[cfg(feature = "remote-wallet")]
    let mut wallet_manager: Option<Arc<RemoteWalletManager>> = None;

    #[cfg(not(feature = "remote-wallet"))]
    let mut wallet_manager = None;

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
            "keypair",
            matches
                .value_of("keypair")
                .map(|s| s.to_string())
                .unwrap_or_else(|| cli_config.keypair_path.clone()),
        );

        Config {
            json_rpc_url: normalize_to_url_if_moniker(
                matches
                    .value_of("json_rpc_url")
                    .unwrap_or(&cli_config.json_rpc_url),
            ),
            #[cfg(feature = "remote-wallet")]
            default_signer: default_signer
                .signer_from_path(matches, &mut wallet_manager)
                .unwrap_or_else(|err| {
                    eprintln!("error: {}", err);
                    exit(1);
                }),
            #[cfg(not(feature = "remote-wallet"))]
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

                    println!("Installing SVM: {}", svm_name);
                    println!("Host: {}", host);

                    // First get SVM info to verify it exists and can be installed
                    match svm_info::get_svm_info(&rpc_client, svm_name, config.commitment_config) {
                        Ok(info) => {
                            if (!info.can_install_validator && !info.can_install_rpc) {
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
                            if (json_output) {
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
                ("dashboard", _) => {
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
                ("status", Some(status_matches)) => {
                    // Check node status
                    let node_id = status_matches.value_of("node-id").unwrap();
                    let json_output = status_matches.is_present("json");

                    match nodes::get_node_status(node_id) {
                        Ok(status) => {
                            if (json_output) {
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
                ("get", Some(get_matches)) => {
                    // Get detailed node information
                    let node_id = get_matches.value_of("node-id").unwrap();
                    let json_output = get_matches.is_present("json");

                    match nodes::get_node_info(&rpc_client, node_id, config.commitment_config) {
                        Ok(info) => {
                            if (json_output) {
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
                }
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
                }
                ("logs", Some(logs_matches)) => {
                    // View node logs
                    let node_id = logs_matches.value_of("node-id").unwrap();
                    let lines = logs_matches
                        .value_of("lines")
                        .unwrap()
                        .parse::<usize>()
                        .unwrap_or(100);
                    let follow = logs_matches.is_present("follow");

                    match nodes::get_node_logs(node_id, lines, follow) {
                        Ok(_) => {
                            if (follow) {
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
                ("deploy", Some(deploy_matches)) => {
                    // Deploy a new node
                    let svm = deploy_matches.value_of("svm").unwrap();
                    let node_type = deploy_matches.value_of("type").unwrap_or("validator");
                    let network = deploy_matches.value_of("network").unwrap_or("mainnet");
                    let host = deploy_matches.value_of("host").unwrap();
                    let name = deploy_matches.value_of("name").unwrap_or("default");

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
                _ => unreachable!(),
            }
        }
        ("examples", Some(examples_matches)) => {
            // Handle the examples command
            if (examples_matches.is_present("list_categories")) {
                // List all available example categories
                println!("Available example categories:");
                println!("  basic       - Basic Commands");
                println!("  svm         - SVM Management");
                println!("  node        - Node Deployment");
                println!("  monitoring  - Node Monitoring and Management");
                println!("  workflow    - Common Workflows");
                println!("\nUse 'osvm examples --category <name>' to show examples for a specific category.");
            } else if let Some(category) = examples_matches.value_of("category") {
                // Display examples for a specific category
                examples::display_category_by_name(category);
            } else {
                // Display all examples
                examples::display_all_examples();
            }
        }
        ("solana", Some(solana_matches)) => {
            let (solana_sub_command, solana_sub_matches) = solana_matches.subcommand();
            match (solana_sub_command, solana_sub_matches) {
                ("validator", Some(validator_matches)) => {
                    // Deploy a Solana validator with enhanced features
                    let connection_str = validator_matches.value_of("connection").unwrap();
                    let network_str = validator_matches.value_of("network").unwrap_or("mainnet");
                    let version = validator_matches.value_of("version").map(|s| s.to_string());
                    let client_type = validator_matches
                        .value_of("client-type")
                        .map(|s| s.to_string());
                    let hot_swap_enabled = validator_matches.is_present("hot-swap");
                    let metrics_config = validator_matches
                        .value_of("metrics-config")
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
                    let disk_config = if (validator_matches.is_present("ledger-disk")
                        && validator_matches.is_present("accounts-disk"))
                    {
                        Some(ssh_deploy::DiskConfig {
                            ledger_disk: validator_matches
                                .value_of("ledger-disk")
                                .unwrap()
                                .to_string(),
                            accounts_disk: validator_matches
                                .value_of("accounts-disk")
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
                    if (deploy_config.hot_swap_enabled) {
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
                ("rpc", Some(rpc_matches)) => {
                    // Deploy a Solana RPC node with enhanced features
                    let connection_str = rpc_matches.value_of("connection").unwrap();
                    let network_str = rpc_matches.value_of("network").unwrap_or("mainnet");
                    let version = rpc_matches.value_of("version").map(|s| s.to_string());
                    let client_type = rpc_matches.value_of("client-type").map(|s| s.to_string());
                    let enable_history = rpc_matches.is_present("enable-history");
                    let metrics_config = rpc_matches
                        .value_of("metrics-config")
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
                    let disk_config = if (rpc_matches.is_present("ledger-disk")
                        && rpc_matches.is_present("accounts-disk"))
                    {
                        Some(ssh_deploy::DiskConfig {
                            ledger_disk: rpc_matches.value_of("ledger-disk").unwrap().to_string(),
                            accounts_disk: rpc_matches
                                .value_of("accounts-disk")
                                .unwrap()
                                .to_string(),
                        })
                    } else {
                        None
                    };

                    // Create additional params for RPC-specific options
                    let mut additional_params = std::collections::HashMap::new();
                    if (enable_history) {
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
                    if (enable_history) {
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
        ("rpc", Some(rpc_matches)) => {
            let (rpc_sub_command, rpc_sub_matches) = rpc_matches.subcommand();
            match (rpc_sub_command, rpc_sub_matches) {
                ("sonic", Some(sonic_matches)) => {
                    // Deploy a Sonic RPC node
                    let connection_str = sonic_matches.value_of("connection").unwrap();
                    let network_str = sonic_matches.value_of("network").unwrap();

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
                ("solana", Some(solana_matches)) => {
                    // Use the enhanced Solana deployment via rpc subcommand
                    let connection_str = solana_matches.value_of("connection").unwrap();
                    let network_str = solana_matches.value_of("network").unwrap_or("mainnet");

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
            if (svm_types.is_empty()) {
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