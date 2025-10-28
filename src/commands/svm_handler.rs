use crate::config::Config;
use crate::utils::{dashboard, ssh_deploy, svm_info};
use solana_client::rpc_client::RpcClient;
use std::process::exit;

pub fn handle_svm_command(
    matches: &clap::ArgMatches,
    rpc_client: &RpcClient,
    config: &Config,
) -> Result<(), Box<dyn std::error::Error>> {
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
                            println!("Successfully installed {} as node {}", svm_name, node_id);
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

    Ok(())
}
