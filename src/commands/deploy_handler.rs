use crate::config::Config;
use crate::utils::ebpf_deploy;

pub async fn handle_deploy_command(
    matches: &clap::ArgMatches,
    config: &Config,
) -> Result<(), Box<dyn std::error::Error>> {
    // Command to deploy eBPF binary to all SVM networks
            let binary_path = matches
                .get_one::<String>("binary")
                .map(|s| s.as_str())
                .unwrap();
            let program_id_path = matches
                .get_one::<String>("program-id")
                .map(|s| s.as_str())
                .unwrap();
            let owner_path = matches
                .get_one::<String>("owner")
                .map(|s| s.as_str())
                .unwrap();
            let fee_payer_path = matches
                .get_one::<String>("fee")
                .map(|s| s.as_str())
                .unwrap();
            let publish_idl = matches.get_flag("publish-idl");
            let idl_file_path = matches.get_one::<String>("idl-file").map(|s| s.to_string());
            let network_str = matches
                .get_one::<String>("network")
                .map(|s| s.as_str())
                .unwrap_or("all");
            let json_output = matches.get_flag("json");
            let retry_attempts = matches
                .get_one::<String>("retry-attempts")
                .and_then(|s| s.parse().ok())
                .unwrap_or(3);
            let confirm_large_binaries = matches.get_flag("confirm-large");

            // Create deployment configuration
            let deploy_config = ebpf_deploy::DeployConfig {
                binary_path: binary_path.to_string(),
                program_id_path: program_id_path.to_string(),
                owner_path: owner_path.to_string(),
                fee_payer_path: fee_payer_path.to_string(),
                publish_idl,
                idl_file_path,
                network_selection: network_str.to_string(),
                json_output,
                retry_attempts,
                confirm_large_binaries,
            };

            println!("🚀 OSVM eBPF Deployment Tool");
            println!("============================");
            println!("📁 Binary path: {binary_path}");
            println!("🆔 Program ID: {program_id_path}");
            println!("👤 Owner: {owner_path}");
            println!("💰 Fee payer: {fee_payer_path}");
            println!("📄 Publish IDL: {}", if publish_idl { "yes" } else { "no" });
            println!("🌐 Target network(s): {network_str}");
            println!();

            // Execute deployment
            let results = ebpf_deploy::deploy_to_all_networks(
                deploy_config.clone(),
                config.commitment_config,
            )
            .await;

            // Display results using the new display function
            if let Err(e) =
                ebpf_deploy::display_deployment_results(&results, deploy_config.json_output)
            {
                eprintln!("Error displaying results: {}", e);
            }

            // Determine exit status
            let failure_count = results
                .iter()
                .filter(|r| r.as_ref().map_or(true, |d| !d.success))
                .count();

            if failure_count > 0 {
                return Err("Some deployments failed".into());
            }

    Ok(())
}
