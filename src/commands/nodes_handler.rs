use crate::config::Config;
use crate::utils::{nodes, ssh_deploy};
use solana_client::rpc_client::RpcClient;
use std::process::exit;

pub async fn handle_nodes_command(
    matches: &clap::ArgMatches,
    rpc_client: &RpcClient,
    config: &Config,
) -> Result<(), Box<dyn std::error::Error>> {
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
            let svm = node_sub_matches
                .get_one::<String>("svm")
                .map(|s| s.as_str());
            let json_output = node_sub_matches.contains_id("json");

            match nodes::list_all_nodes(
                rpc_client,
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
            match nodes::run_dashboard(rpc_client, config.commitment_config, config.verbose) {
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
                    return Err(format!("Error getting node status: {}", e).into());
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

            match nodes::get_node_info(rpc_client, node_id, config.commitment_config) {
                Ok(info) => {
                    if json_output {
                        println!("{}", serde_json::to_string_pretty(&info).unwrap());
                    } else {
                        nodes::display_node_info(&info, config.verbose);
                    }
                }
                Err(e) => {
                    return Err(format!("Error getting node info: {}", e).into());
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
                    return Err(format!("Error restarting node: {}", e).into());
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
                    return Err(format!("Error stopping node: {}", e).into());
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
                .and_then(|s| s.parse::<usize>().ok())
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
                    return Err(format!("Error getting node logs: {}", e).into());
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

            match nodes::deploy_node(rpc_client, deploy_config).await {
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

    Ok(())
}
