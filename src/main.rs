use {
    crate::config::Config, // Added
    crate::utils::diagnostics::DiagnosticCoordinator,
    crate::utils::{dashboard, ebpf_deploy, examples, nodes, ssh_deploy, svm_info},
    clparse::parse_command_line,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{native_token::Sol, pubkey::Pubkey, signature::Signer}, // Modified (removed CommitmentConfig) - bad formatting
    std::{process::exit, str::FromStr},                                 // Modified
};

// Helper function to handle the type mismatch between clap v2 and v4
// This function remains as it's used by command handlers directly with `sub_matches`
fn pubkey_of_checked(matches: &clap::ArgMatches, name: &str) -> Option<Pubkey> {
    matches
        .get_one::<String>(name)
        .map(|s| s.as_str())
        .and_then(|s| Pubkey::from_str(s).ok())
}

#[cfg(feature = "remote-wallet")]
use {solana_remote_wallet::remote_wallet::RemoteWalletManager, std::sync::Arc};
pub mod clparse;
pub mod config; // Added
pub mod prelude;
pub mod utils;

// Config struct is now in src/config.rs

/// Show recent logs from devnet RPC node
fn show_devnet_logs(lines: usize, follow: bool) -> Result<(), Box<dyn std::error::Error>> {
    use std::fs;
    use std::process::Command;

    println!("📋 Devnet RPC Node Logs");
    println!("=======================");

    // Find the most recent agave-validator log file
    let log_files = fs::read_dir(".")?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let name = entry.file_name().to_string_lossy().to_string();
            if name.starts_with("agave-validator-") && name.ends_with(".log") {
                Some((entry.path(), entry.metadata().ok()?.modified().ok()?))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if log_files.is_empty() {
        println!("⚠️  No validator log files found in current directory");
        println!("💡 Make sure you're in the correct directory where the validator was started");
        println!("💡 Log files are named like: agave-validator-*.log");
        return Ok(());
    }

    // Get the most recent log file
    let (most_recent_log, _) = log_files
        .iter()
        .max_by_key(|(_, modified_time)| *modified_time)
        .unwrap();

    println!("📄 Log file: {}", most_recent_log.display());
    println!(
        "📏 Showing last {} lines{}\n",
        lines,
        if follow { " (following)" } else { "" }
    );

    if follow {
        // Use tail -f to follow the log
        let mut child = Command::new("tail")
            .arg("-f")
            .arg("-n")
            .arg(lines.to_string())
            .arg(most_recent_log)
            .spawn()?;

        println!("📡 Following logs in real-time (Press Ctrl+C to stop)...\n");

        // Wait for the process (it will run until Ctrl+C)
        let status = child.wait()?;
        if !status.success() {
            eprintln!("❌ tail command failed");
        }
    } else {
        // Just show the last N lines
        let output = Command::new("tail")
            .arg("-n")
            .arg(lines.to_string())
            .arg(most_recent_log)
            .output()?;

        if output.status.success() {
            let log_content = String::from_utf8_lossy(&output.stdout);

            // Parse and format the logs with colors
            for line in log_content.lines() {
                if line.contains("ERROR") {
                    println!("❌ {}", line);
                } else if line.contains("WARN") {
                    println!("⚠️  {}", line);
                } else if line.contains("INFO") {
                    println!("ℹ️  {}", line);
                } else {
                    println!("   {}", line);
                }
            }
        } else {
            eprintln!(
                "❌ Failed to read log file: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    Ok(())
}

/// Handle the audit command separately to avoid triggering config loading and self-repair
async fn handle_audit_command(app_matches: &clap::ArgMatches, matches: &clap::ArgMatches) -> Result<(), Box<dyn std::error::Error>> {
    use crate::utils::audit::AuditCoordinator;
    use std::fs;
    use std::path::Path;

    println!("🔍 OSVM Security Audit");
    println!("======================");

    let output_dir = matches.get_one::<String>("output").unwrap();
    let format = matches.get_one::<String>("format").unwrap();
    let verbose = matches.get_count("verbose");
    let test_mode = matches.get_flag("test");
    let ai_analysis = matches.get_flag("ai-analysis");
    let gh_repo = matches.get_one::<String>("gh");

    if verbose > 0 {
        println!("📁 Output directory: {}", output_dir);
        println!("📄 Format: {}", format);
        if test_mode {
            println!("🧪 Test mode: generating sample audit report");
        }
        if ai_analysis {
            println!("🤖 AI analysis: enabled");
        }
        if let Some(repo) = gh_repo {
            println!("🐙 GitHub repository: {}", repo);
        }
    }

    // Check for OpenAI API key if AI analysis is enabled
    let openai_api_key = if ai_analysis {
        match std::env::var("OPENAI_API_KEY") {
            Ok(key) => {
                if key.is_empty() {
                    eprintln!("❌ OPENAI_API_KEY environment variable is empty");
                    eprintln!("   Please set your OpenAI API key to enable AI analysis");
                    exit(1);
                }
                Some(key)
            }
            Err(_) => {
                eprintln!("❌ OPENAI_API_KEY environment variable not found");
                eprintln!("   Please set your OpenAI API key to enable AI analysis");
                eprintln!("   Example: export OPENAI_API_KEY='your-api-key-here'");
                exit(1);
            }
        }
    } else {
        None
    };

    // Create output directory
    if let Err(e) = fs::create_dir_all(output_dir) {
        eprintln!("❌ Failed to create output directory: {}", e);
        exit(1);
    }

    // Generate audit report - handle GitHub mode, test mode, or regular audit
    let report = if let Some(repo_spec) = gh_repo {
        // GitHub repository audit mode
        println!("🐙 GitHub repository audit mode");
        let audit_coordinator = if let Some(api_key) = openai_api_key {
            AuditCoordinator::with_ai(api_key)
        } else {
            AuditCoordinator::new()
        };
        
        match audit_coordinator.audit_github_repository(repo_spec).await {
            Ok(_) => {
                println!("✅ GitHub repository audit completed and pushed");
                return Ok(()); // Exit early as files are already generated and committed
            },
            Err(e) => {
                eprintln!("❌ Failed to audit GitHub repository: {}", e);
                exit(1);
            }
        }
    } else if test_mode {
        println!("🧪 Generating test audit report...");
        // Create audit coordinator only for test report generation - no diagnostics
        let audit_coordinator = if let Some(api_key) = openai_api_key {
            AuditCoordinator::with_ai(api_key)
        } else {
            AuditCoordinator::new()
        };
        audit_coordinator.create_test_audit_report()
    } else {
        // Initialize audit coordinator and run full audit
        let audit_coordinator = if let Some(api_key) = openai_api_key {
            AuditCoordinator::with_ai(api_key)
        } else {
            AuditCoordinator::new()
        };
        
        // Run security audit
        match audit_coordinator.run_security_audit().await {
            Ok(report) => report,
            Err(e) => {
                eprintln!("❌ Failed to run security audit: {}", e);
                exit(1);
            }
        }
    };

    println!("✅ Security audit completed successfully");
    println!("📊 Security Score: {:.1}/100", report.summary.security_score);
    println!("🔍 Total Findings: {}", report.summary.total_findings);
    
    if report.summary.critical_findings > 0 {
        println!("🔴 Critical: {}", report.summary.critical_findings);
    }
    if report.summary.high_findings > 0 {
        println!("🟠 High: {}", report.summary.high_findings);
    }
    if report.summary.medium_findings > 0 {
        println!("🟡 Medium: {}", report.summary.medium_findings);
    }
    if report.summary.low_findings > 0 {
        println!("🔵 Low: {}", report.summary.low_findings);
    }

    // Generate timestamp for unique filenames
    let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
    
    // Generate outputs based on requested format
    let typst_path = Path::new(output_dir).join(format!("osvm_audit_report_{}.typ", timestamp));
    let pdf_path = Path::new(output_dir).join(format!("osvm_audit_report_{}.pdf", timestamp));
    
    // Create audit coordinator for document generation
    let audit_coordinator = AuditCoordinator::new();
    
    match format.as_str() {
        "typst" | "both" => {
            if let Err(e) = audit_coordinator.generate_typst_document(&report, &typst_path) {
                eprintln!("❌ Failed to generate Typst document: {}", e);
                exit(1);
            }
            println!("📄 Typst document generated: {}", typst_path.display());
            
            if format == "both" {
                if let Err(e) = audit_coordinator.compile_to_pdf(&typst_path, &pdf_path) {
                    eprintln!("❌ Failed to compile PDF: {}", e);
                    eprintln!("   Typst document is available at: {}", typst_path.display());
                } else {
                    println!("📋 PDF report generated: {}", pdf_path.display());
                }
            }
        }
        "pdf" => {
            // Generate Typst document first (temporary)
            if let Err(e) = audit_coordinator.generate_typst_document(&report, &typst_path) {
                eprintln!("❌ Failed to generate Typst document: {}", e);
                exit(1);
            }
            
            if let Err(e) = audit_coordinator.compile_to_pdf(&typst_path, &pdf_path) {
                eprintln!("❌ Failed to compile PDF: {}", e);
                exit(1);
            }
            
            // Remove temporary Typst file
            let _ = fs::remove_file(&typst_path);
            println!("📋 PDF report generated: {}", pdf_path.display());
        }
        _ => {
            eprintln!("❌ Invalid format specified");
            exit(1);
        }
    }

    if verbose > 0 {
        println!("\n📋 Audit Summary:");
        println!("  Compliance Level: {}", report.summary.compliance_level);
        println!("  System: {} {}", report.system_info.os_info, report.system_info.architecture);
        println!("  Rust Version: {}", report.system_info.rust_version);
        if let Some(ref solana_version) = report.system_info.solana_version {
            println!("  Solana Version: {}", solana_version);
        }
    }

    println!("\n💡 To view the full report, open the generated file.");
    
    if !test_mode && (report.summary.critical_findings > 0 || report.summary.high_findings > 0) {
        println!("⚠️  Critical or high-severity findings detected. Please review and address them promptly.");
        exit(1);
    }

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app_matches = parse_command_line();
    let Some((sub_command, sub_matches)) = app_matches.subcommand() else {
        // If no subcommand is provided, parse_command_line should handle it or exit.
        // This return is a fallback.
        return Err("No subcommand provided. Use --help for more information.".into());
    };
    // 'matches' will refer to the subcommand's matches, as before.
    let matches = sub_matches;

    // Handle audit command early to avoid config loading that might trigger self-repair
    if sub_command == "audit" {
        return handle_audit_command(&app_matches, sub_matches).await;
    }

    // Load configuration using the new Config module
    // Pass app_matches for global flags like 'verbose' and 'no_color',
    // and sub_matches for command-specific overrides like 'json_rpc_url', 'keypair', 'config_file'.
    let config = Config::load(&app_matches, sub_matches).await?;

    // Setup logging and display initial info using the config method
    config.setup_logging_and_display_info()?;

    #[cfg(feature = "remote-wallet")]
    #[allow(unused_variables, unused_mut)]
    let mut wallet_manager: Option<Arc<RemoteWalletManager>> = None;

    #[cfg(not(feature = "remote-wallet"))]
    let _wallet_manager: Option<()> = None;

    // The RpcClient is now created after config loading and logging setup.
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
                    let svm = node_sub_matches
                        .get_one::<String>("svm")
                        .map(|s| s.as_str());
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
        // "solana" => { // Temporarily commented out due to clparse changes
        //     let Some((solana_sub_command, solana_sub_matches)) = matches.subcommand() else {
        //         eprintln!("No solana subcommand provided");
        //         exit(1);
        //     };
        //
        //     match solana_sub_command {
        //         "validator" => {
        //             // Deploy a Solana validator with enhanced features
        //             let connection_str = solana_sub_matches
        //                 .get_one::<String>("connection")
        //                 .map(|s| s.as_str())
        //                 .unwrap();
        //             let network_str = solana_sub_matches
        //                 .get_one::<String>("network")
        //                 .map(|s| s.as_str())
        //                 .unwrap_or("mainnet");
        //             let version = solana_sub_matches
        //                 .get_one::<String>("version")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //             let client_type = solana_sub_matches
        //                 .get_one::<String>("client-type")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //             let hot_swap_enabled = solana_sub_matches.contains_id("hot-swap");
        //             let metrics_config = solana_sub_matches
        //                 .get_one::<String>("metrics-config")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //
        //             // Parse connection string
        //             let connection =
        //                 match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
        //                     Ok(conn) => conn,
        //                     Err(e) => {
        //                         eprintln!("Error parsing SSH connection string: {}", e);
        //                         exit(1);
        //                     }
        //                 };
        //
        //             // Parse network type
        //             let network = match network_str.to_lowercase().as_str() {
        //                 "mainnet" => ssh_deploy::NetworkType::Mainnet,
        //                 "testnet" => ssh_deploy::NetworkType::Testnet,
        //                 "devnet" => ssh_deploy::NetworkType::Devnet,
        //                 _ => {
        //                     eprintln!("Invalid network: {}", network_str);
        //                     exit(1);
        //                 }
        //             };
        //
        //             // Create disk configuration if both disk params are provided
        //             let disk_config = if solana_sub_matches.contains_id("ledger-disk")
        //                 && solana_sub_matches.contains_id("accounts-disk")
        //             {
        //                 Some(ssh_deploy::DiskConfig {
        //                     ledger_disk: solana_sub_matches
        //                         .get_one::<String>("ledger-disk")
        //                         .map(|s| s.as_str())
        //                         .unwrap()
        //                         .to_string(),
        //                     accounts_disk: solana_sub_matches
        //                         .get_one::<String>("accounts-disk")
        //                         .map(|s| s.as_str())
        //                         .unwrap()
        //                         .to_string(),
        //                 })
        //             } else {
        //                 None
        //             };
        //
        //             // Create deployment config with enhanced features
        //             let deploy_config = ssh_deploy::DeploymentConfig {
        //                 svm_type: "solana".to_string(),
        //                 node_type: "validator".to_string(),
        //                 network,
        //                 node_name: format!("solana-validator-{}", network_str),
        //                 rpc_url: None,
        //                 additional_params: std::collections::HashMap::new(),
        //                 version,
        //                 client_type,
        //                 hot_swap_enabled,
        //                 metrics_config,
        //                 disk_config,
        //             };
        //
        //             println!("Deploying Solana validator node to {}...", connection_str);
        //             println!("Network: {}", network_str);
        //             if let Some(ver) = &deploy_config.version {
        //                 println!("Version: {}", ver);
        //             }
        //             if let Some(client) = &deploy_config.client_type {
        //                 println!("Client type: {}", client);
        //             }
        //             if deploy_config.hot_swap_enabled {
        //                 println!("Hot-swap capability: Enabled");
        //             }
        //             if let Some(disks) = &deploy_config.disk_config {
        //                 println!("Disk configuration:");
        //                 println!("  Ledger disk: {}", disks.ledger_disk);
        //                 println!("  Accounts disk: {}", disks.accounts_disk);
        //             }
        //
        //             if let Err(e) =
        //                 ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
        //             {
        //                 eprintln!("Deployment error: {}", e);
        //                 exit(1);
        //             }
        //
        //             println!("Solana validator node deployed successfully!");
        //         }
        //         "rpc" => {
        //             // Deploy a Solana RPC node with enhanced features
        //             let connection_str = solana_sub_matches
        //                 .get_one::<String>("connection")
        //                 .map(|s| s.as_str())
        //                 .unwrap();
        //             let network_str = solana_sub_matches
        //                 .get_one::<String>("network")
        //                 .map(|s| s.as_str())
        //                 .unwrap_or("mainnet");
        //             let version = solana_sub_matches
        //                 .get_one::<String>("version")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //             let client_type = solana_sub_matches
        //                 .get_one::<String>("client-type")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //             let enable_history = solana_sub_matches.contains_id("enable-history");
        //             let metrics_config = solana_sub_matches
        //                 .get_one::<String>("metrics-config")
        //                 .map(|s| s.as_str())
        //                 .map(|s| s.to_string());
        //
        //             // Parse connection string
        //             let connection =
        //                 match ssh_deploy::ServerConfig::from_connection_string(connection_str) {
        //                     Ok(conn) => conn,
        //                     Err(e) => {
        //                         eprintln!("Error parsing SSH connection string: {}", e);
        //                         exit(1);
        //                     }
        //                 };
        //
        //             // Parse network type
        //             let network = match network_str.to_lowercase().as_str() {
        //                 "mainnet" => ssh_deploy::NetworkType::Mainnet,
        //                 "testnet" => ssh_deploy::NetworkType::Testnet,
        //                 "devnet" => ssh_deploy::NetworkType::Devnet,
        //                 _ => {
        //                     eprintln!("Invalid network: {}", network_str);
        //                     exit(1);
        //                 }
        //             };
        //
        //             // Create disk configuration if both disk params are provided
        //             let disk_config = if solana_sub_matches.contains_id("ledger-disk")
        //                 && solana_sub_matches.contains_id("accounts-disk")
        //             {
        //                 Some(ssh_deploy::DiskConfig {
        //                     ledger_disk: solana_sub_matches
        //                         .get_one::<String>("ledger-disk")
        //                         .map(|s| s.as_str())
        //                         .unwrap()
        //                         .to_string(),
        //                     accounts_disk: solana_sub_matches
        //                         .get_one::<String>("accounts-disk")
        //                         .map(|s| s.as_str())
        //                         .unwrap()
        //                         .to_string(),
        //                 })
        //             } else {
        //                 None
        //             };
        //
        //             // Create additional params for RPC-specific options
        //             let mut additional_params = std::collections::HashMap::new();
        //             if enable_history {
        //                 additional_params.insert("enable_history".to_string(), "true".to_string());
        //             }
        //
        //             // Create deployment config with enhanced features
        //             let deploy_config = ssh_deploy::DeploymentConfig {
        //                 svm_type: "solana".to_string(),
        //                 node_type: "rpc".to_string(),
        //                 network,
        //                 node_name: format!("solana-rpc-{}", network_str),
        //                 rpc_url: None,
        //                 additional_params,
        //                 version,
        //                 client_type,
        //                 hot_swap_enabled: false, // Not needed for RPC nodes
        //                 metrics_config,
        //                 disk_config,
        //             };
        //
        //             println!("Deploying Solana RPC node to {}...", connection_str);
        //             println!("Network: {}", network_str);
        //             if let Some(ver) = &deploy_config.version {
        //                 println!("Version: {}", ver);
        //             }
        //             if let Some(client) = &deploy_config.client_type {
        //                 println!("Client type: {}", client);
        //             }
        //             if enable_history {
        //                 println!("Transaction history: Enabled");
        //             }
        //             if let Some(disks) = &deploy_config.disk_config {
        //                 println!("Disk configuration:");
        //                 println!("  Ledger disk: {}", disks.ledger_disk);
        //                 println!("  Accounts disk: {}", disks.accounts_disk);
        //             }
        //
        //             if let Err(e) =
        //                 ssh_deploy::deploy_svm_node(connection, deploy_config, None).await
        //             {
        //                 eprintln!("Deployment error: {}", e);
        //                 exit(1);
        //             }
        //
        //             println!("Solana RPC node deployed successfully!");
        //         }
        //         _ => {
        //             eprintln!("Unknown Solana command: {}", solana_sub_command);
        //             exit(1);
        //         }
        //     }
        // }
        "rpc-manager" => {
            // Renamed from "rpc"
            let Some((rpc_sub_command, rpc_sub_matches)) = matches.subcommand() else {
                eprintln!("No RPC subcommand provided");
                exit(1);
            };

            match rpc_sub_command {
                "sonic" => {
                    // Moved to be first to match clparse.rs
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
                "query-solana" => {
                    // Renamed from "solana"
                    // Connect to Solana RPC endpoints
                    let network = rpc_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("mainnet");
                    let custom_url = rpc_sub_matches
                        .get_one::<String>("custom-url")
                        .map(|s| s.as_str());
                    let monitor = rpc_sub_matches.get_flag("monitor");
                    let health = rpc_sub_matches.get_flag("health");
                    let _info = rpc_sub_matches.get_flag("info");

                    if monitor {
                        // Monitor network activity
                        match crate::utils::solana_rpc::monitor_network(network, custom_url).await {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("❌ Error monitoring network: {}", e);
                                exit(1);
                            }
                        }
                    } else if health {
                        // Check network health
                        match crate::utils::solana_rpc::check_network_health(network, custom_url)
                            .await
                        {
                            Ok(health_info) => {
                                println!("🏥 Solana {} Network Health", network.to_uppercase());
                                println!("=============================");
                                println!(
                                    "Status: {}",
                                    if health_info.healthy {
                                        "✅ Healthy"
                                    } else {
                                        "❌ Unhealthy"
                                    }
                                );
                                println!("RPC URL: {}", health_info.rpc_url);
                                if let Some(response_time) = health_info.response_time_ms {
                                    println!("Response Time: {}ms", response_time);
                                }
                                if let Some(slot) = health_info.slot_height {
                                    println!("Current Slot: {}", slot);
                                }
                                if let Some(epoch) = health_info.epoch {
                                    println!("Current Epoch: {}", epoch);
                                }
                                if let Some(validators) = health_info.validator_count {
                                    println!("Total Validators: {}", validators);
                                }
                                if let Some(voting) = health_info.voting_validators {
                                    println!("Voting Validators: {}", voting);
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error checking network health: {}", e);
                                exit(1);
                            }
                        }
                    } else {
                        // Show network info (default)
                        match crate::utils::solana_rpc::show_network_info(network, custom_url).await
                        {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("❌ Error getting network info: {}", e);
                                exit(1);
                            }
                        }
                    }
                }
                "local" => {
                    // Deploy a local RPC node on localhost
                    let svm = rpc_sub_matches
                        .get_one::<String>("svm")
                        .map(|s| s.as_str())
                        .unwrap_or("solana");
                    let network = rpc_sub_matches
                        .get_one::<String>("network")
                        .map(|s| s.as_str())
                        .unwrap_or("devnet");
                    let port = rpc_sub_matches
                        .get_one::<String>("port")
                        .map(|s| s.as_str())
                        .unwrap_or("8899");
                    let faucet_port = rpc_sub_matches
                        .get_one::<String>("faucet-port")
                        .map(|s| s.as_str())
                        .unwrap_or("9900");
                    let ledger_path = rpc_sub_matches
                        .get_one::<String>("ledger-path")
                        .map(|s| s.as_str())
                        .unwrap_or("/tmp/test-ledger");
                    let reset = rpc_sub_matches.get_flag("reset");
                    let background = rpc_sub_matches.get_flag("background");
                    let stop = rpc_sub_matches.get_flag("stop");
                    let status = rpc_sub_matches.get_flag("status");

                    if stop {
                        // Stop local RPC node
                        match crate::utils::local_rpc::stop_local_rpc().await {
                            Ok(_) => println!("✅ Local RPC node stopped successfully"),
                            Err(e) => {
                                eprintln!("❌ Error stopping local RPC node: {}", e);
                                exit(1);
                            }
                        }
                    } else if status {
                        // Check status of local RPC node
                        match crate::utils::local_rpc::check_local_rpc_status().await {
                            Ok(status_info) => {
                                println!("📊 Local RPC Node Status");
                                println!("========================");
                                println!(
                                    "Status: {}",
                                    if status_info.running {
                                        "🟢 Running"
                                    } else {
                                        "🔴 Stopped"
                                    }
                                );
                                if let Some(pid) = status_info.pid {
                                    println!("PID: {}", pid);
                                }
                                if let Some(port) = status_info.port {
                                    println!("Port: {}", port);
                                    println!("RPC URL: http://localhost:{}", port);
                                }
                                if let Some(network) = status_info.network {
                                    println!("Network: {}", network);
                                }
                                if let Some(uptime) = status_info.uptime {
                                    println!("Uptime: {}", uptime);
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error checking local RPC status: {}", e);
                                exit(1);
                            }
                        }
                    } else {
                        // Start local RPC node
                        let config = crate::utils::local_rpc::LocalRpcConfig {
                            svm: svm.to_string(),
                            network: network.to_string(),
                            port: port.parse().unwrap_or(8899),
                            faucet_port: Some(faucet_port.parse().unwrap_or(9900)),
                            ledger_path: ledger_path.to_string(),
                            reset,
                            background,
                        };

                        println!(
                            "🚀 Starting local {} RPC node on localhost",
                            svm.to_uppercase()
                        );
                        println!("📋 Configuration:");
                        println!("   SVM: {}", svm);
                        println!("   Network: {}", network);
                        println!("   RPC Port: {}", config.port);
                        if let Some(faucet) = config.faucet_port {
                            println!("   Faucet Port: {}", faucet);
                        }
                        println!("   Ledger Path: {}", ledger_path);
                        if reset {
                            println!("   Reset: Yes");
                        }
                        if background {
                            println!("   Background: Yes");
                        }
                        println!();

                        match crate::utils::local_rpc::start_local_rpc(config).await {
                            Ok(node_info) => {
                                println!("✅ Local RPC node started successfully!");
                                println!("🔗 RPC URL: http://localhost:{}", node_info.port);
                                if let Some(faucet_port) = node_info.faucet_port {
                                    println!("💰 Faucet URL: http://localhost:{}", faucet_port);
                                }
                                println!("📁 Ledger Path: {}", node_info.ledger_path);
                                if background {
                                    println!("🔧 Use 'osvm rpc local --status' to check status");
                                    println!("🛑 Use 'osvm rpc local --stop' to stop the node");
                                } else {
                                    println!("ℹ️  Press Ctrl+C to stop the node");
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error starting local RPC node: {}", e);
                                exit(1);
                            }
                        }
                    }
                }
                "devnet" => {
                    // Start a legitimate devnet RPC node that syncs with real blockchain
                    let ledger_path = rpc_sub_matches
                        .get_one::<String>("ledger-path")
                        .map(|s| s.as_str())
                        .unwrap_or("devnet-ledger");
                    let rpc_port = rpc_sub_matches
                        .get_one::<String>("rpc-port")
                        .map(|s| s.as_str())
                        .unwrap_or("8899");
                    let background = rpc_sub_matches.get_flag("background");
                    let stop = rpc_sub_matches.get_flag("stop");
                    let status = rpc_sub_matches.get_flag("status");
                    let logs = rpc_sub_matches.get_flag("logs");
                    let lines = rpc_sub_matches
                        .get_one::<String>("lines")
                        .and_then(|s| s.parse().ok())
                        .unwrap_or(50);
                    let follow = rpc_sub_matches.get_flag("follow");

                    if stop {
                        // Stop devnet RPC node
                        match crate::utils::devnet_rpc::stop_devnet_rpc().await {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("❌ Error stopping devnet RPC node: {}", e);
                                exit(1);
                            }
                        }
                    } else if status {
                        // Check status of devnet RPC node
                        match crate::utils::devnet_rpc::check_devnet_rpc_status().await {
                            Ok(status_info) => {
                                println!("📊 Devnet RPC Node Status");
                                println!("=========================");
                                println!(
                                    "Status: {}",
                                    if status_info.running {
                                        "🟢 Running"
                                    } else {
                                        "🔴 Stopped"
                                    }
                                );
                                println!("Network: {} (real blockchain sync)", status_info.network);
                                if let Some(pid) = status_info.pid {
                                    println!("PID: {}", pid);
                                }
                                if let Some(port) = status_info.rpc_port {
                                    println!("RPC Port: {}", port);
                                    println!("RPC URL: http://localhost:{}", port);
                                }
                                if status_info.syncing {
                                    println!("Sync Status: 🔄 Syncing with devnet");
                                    if let Some(slot) = status_info.slot_height {
                                        println!("Current Slot: {}", slot);
                                    }
                                } else if status_info.running {
                                    println!("Sync Status: ⏳ Starting up...");
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error checking devnet RPC status: {}", e);
                                exit(1);
                            }
                        }
                    } else if logs {
                        // Show logs from devnet RPC node
                        match show_devnet_logs(lines, follow) {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("❌ Error showing devnet RPC logs: {}", e);
                                exit(1);
                            }
                        }
                    } else {
                        // Start devnet RPC node
                        let config = crate::utils::devnet_rpc::DevnetRpcConfig {
                            ledger_path: ledger_path.to_string(),
                            rpc_port: rpc_port.parse().unwrap_or(8899),
                            gossip_port: 8001,
                            background,
                        };

                        match crate::utils::devnet_rpc::start_devnet_rpc(config).await {
                            Ok(_node_info) => {
                                // _node_info was unused
                                if background {
                                    println!(
                                        "🔧 Use 'osvm rpc devnet --status' to check sync progress"
                                    );
                                    println!("🛑 Use 'osvm rpc devnet --stop' to stop the node");
                                } else {
                                    println!("ℹ️  Devnet RPC node finished");
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error starting devnet RPC node: {}", e);
                                exit(1);
                            }
                        }
                    }
                }
                "test" => {
                    // Start a local test validator with RPC for development
                    let ledger_path = rpc_sub_matches
                        .get_one::<String>("ledger-path")
                        .map(|s| s.as_str())
                        .unwrap_or("test-ledger");
                    let rpc_port = rpc_sub_matches
                        .get_one::<String>("rpc-port")
                        .map(|s| s.as_str())
                        .unwrap_or("8899");
                    let faucet_port = rpc_sub_matches
                        .get_one::<String>("faucet-port")
                        .map(|s| s.as_str())
                        .unwrap_or("9900");
                    let reset = rpc_sub_matches.get_flag("reset");
                    let background = rpc_sub_matches.get_flag("background");
                    let stop = rpc_sub_matches.get_flag("stop");
                    let status = rpc_sub_matches.get_flag("status");
                    let logs = rpc_sub_matches.get_flag("logs");
                    let quiet = rpc_sub_matches.get_flag("quiet");

                    if stop {
                        // Stop test validator
                        println!("🛑 Stopping test validator...");
                        let output = std::process::Command::new("pkill")
                            .arg("-f")
                            .arg("solana-test-validator")
                            .output();
                        match output {
                            Ok(result) => {
                                if result.status.success() {
                                    println!("✅ Test validator stopped successfully");
                                } else {
                                    println!("⚠️  No test validator process found");
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error stopping test validator: {}", e);
                                exit(1);
                            }
                        }
                    } else if status {
                        // Check status of test validator
                        println!("📊 Test Validator Status");
                        println!("========================");

                        // Check if process is running
                        let ps_output = std::process::Command::new("pgrep")
                            .arg("-f")
                            .arg("solana-test-validator")
                            .output();

                        match ps_output {
                            Ok(result) => {
                                if result.status.success() && !result.stdout.is_empty() {
                                    let pids = String::from_utf8_lossy(&result.stdout);
                                    println!("Status: 🟢 Running");
                                    println!("PID(s): {}", pids.trim());
                                    println!("RPC URL: http://localhost:{}", rpc_port);
                                    println!("Faucet URL: http://localhost:{}", faucet_port);
                                    println!("Ledger Path: {}", ledger_path);

                                    // Test RPC health
                                    let health_check = std::process::Command::new("curl")
                                        .arg("-s")
                                        .arg("-X")
                                        .arg("POST")
                                        .arg("-H")
                                        .arg("Content-Type: application/json")
                                        .arg("-d")
                                        .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#)
                                        .arg(&format!("http://localhost:{}", rpc_port))
                                        .output();

                                    if let Ok(health_result) = health_check {
                                        if health_result.status.success() {
                                            let response =
                                                String::from_utf8_lossy(&health_result.stdout);
                                            if response.contains("\"ok\"") {
                                                println!("RPC Health: ✅ Healthy");
                                            } else {
                                                println!(
                                                    "RPC Health: ⚠️  Unknown response: {}",
                                                    response
                                                );
                                            }
                                        } else {
                                            println!("RPC Health: ❌ Not responding");
                                        }
                                    } else {
                                        println!("RPC Health: ❓ Unable to check");
                                    }
                                } else {
                                    println!("Status: 🔴 Stopped");
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Error checking test validator status: {}", e);
                                exit(1);
                            }
                        }
                    } else if logs {
                        // Show logs from test validator - this would need to be implemented
                        // For now, we'll just show a message since test validator logs are typically minimal
                        println!("📋 Test Validator Logs");
                        println!("======================");
                        println!("ℹ️  Test validator runs with minimal logging.");
                        println!(
                            "💡 Check the terminal where the validator was started for output."
                        );
                        println!("🔧 Use 'osvm rpc-manager test --status' to check health.");
                    } else {
                        // Start test validator
                        println!("🚀 Starting local test validator");
                        println!("================================");
                        println!("📁 Ledger path: {}", ledger_path);
                        println!("🔗 RPC port: {}", rpc_port);
                        println!("💰 Faucet port: {}", faucet_port);
                        if reset {
                            println!("🔄 Reset: Yes");
                        }
                        if background {
                            println!("⚙️  Background: Yes");
                        }
                        if quiet {
                            println!("🤫 Quiet: Yes");
                        }
                        println!();

                        // Build command
                        let mut cmd = std::process::Command::new("solana-test-validator");
                        cmd.arg("--rpc-port").arg(rpc_port);
                        cmd.arg("--faucet-port").arg(faucet_port);
                        cmd.arg("--ledger").arg(ledger_path);

                        if reset {
                            cmd.arg("--reset");
                        }
                        if quiet {
                            cmd.arg("--quiet");
                        }

                        if background {
                            // Start in background
                            match cmd.spawn() {
                                Ok(mut child) => {
                                    // Give it a moment to start
                                    tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

                                    // Check if it's still running
                                    match child.try_wait() {
                                        Ok(Some(status)) => {
                                            eprintln!("❌ Test validator exited immediately with status: {}", status);
                                            exit(1);
                                        }
                                        Ok(None) => {
                                            println!("✅ Test validator started in background");
                                            println!("🆔 Process ID: {}", child.id());
                                            println!("🔗 RPC URL: http://localhost:{}", rpc_port);
                                            println!(
                                                "💰 Faucet URL: http://localhost:{}",
                                                faucet_port
                                            );
                                            println!();
                                            println!("🔧 Use 'osvm rpc-manager test --status' to check status");
                                            println!(
                                                "🛑 Use 'osvm rpc-manager test --stop' to stop"
                                            );

                                            // Test RPC after a moment
                                            tokio::time::sleep(tokio::time::Duration::from_secs(2))
                                                .await;
                                            let health_check = std::process::Command::new("curl")
                                                .arg("-s")
                                                .arg("-X")
                                                .arg("POST")
                                                .arg("-H")
                                                .arg("Content-Type: application/json")
                                                .arg("-d")
                                                .arg(r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#)
                                                .arg(&format!("http://localhost:{}", rpc_port))
                                                .output();

                                            if let Ok(health_result) = health_check {
                                                if health_result.status.success() {
                                                    let response = String::from_utf8_lossy(
                                                        &health_result.stdout,
                                                    );
                                                    if response.contains("\"ok\"") {
                                                        println!("🎉 Test validator is healthy and ready!");
                                                    }
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            eprintln!(
                                                "❌ Error checking test validator status: {}",
                                                e
                                            );
                                            exit(1);
                                        }
                                    }
                                }
                                Err(e) => {
                                    eprintln!("❌ Error starting test validator: {}", e);
                                    exit(1);
                                }
                            }
                        } else {
                            // Start in foreground
                            println!("🎯 Starting test validator in foreground mode...");
                            println!("ℹ️  Press Ctrl+C to stop");
                            println!();

                            match cmd.status() {
                                Ok(status) => {
                                    if status.success() {
                                        println!("✅ Test validator finished normally");
                                    } else {
                                        eprintln!(
                                            "❌ Test validator exited with status: {}",
                                            status
                                        );
                                        exit(1);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("❌ Error running test validator: {}", e);
                                    exit(1);
                                }
                            }
                        }
                    }
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
        "deploy" => {
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
        }
        "doctor" => {
            // Handle the doctor command for system diagnostics and repair
            let diagnostic_coordinator = DiagnosticCoordinator::new();

            if matches.contains_id("fix") {
                // Run diagnostics and attempt repairs
                println!("🩺 OSVM System Health Check & Repair");
                println!("===================================");

                match diagnostic_coordinator.run_detailed_diagnostics().await {
                    Ok(results) => {
                        // Display current status
                        println!("📊 System Status: {:?}", results.summary.overall_health);
                        println!(
                            "🔍 Checks: {}/{} passed",
                            results.summary.passed_checks, results.summary.total_checks
                        );

                        if results.summary.critical_issues > 0 || results.summary.warnings > 0 {
                            println!("\n🛠️  Issues detected - attempting automatic repair...");

                            // Extract repairable errors from health check
                            let health = &results.system_health;
                            let mut repairable_errors = Vec::new();

                            // Convert health issues to repairable errors
                            for issue in &health.issues {
                                match issue.category {
                                    crate::utils::diagnostics::IssueCategory::SystemDependencies => {
                                        if issue.title.contains("System tuning") {
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::SystemTuningRequired);
                                        } else if issue.title.contains("Missing dependency") {
                                            let dep_name = issue.title.replace("Missing dependency: ", "");
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::MissingSystemDependencies(vec![dep_name]));
                                        } else if issue.title.contains("Update available") {
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::OutdatedSystemPackages);
                                        }
                                    }
                                    crate::utils::diagnostics::IssueCategory::UserConfiguration => {
                                        if issue.title.contains("Solana CLI not installed") {
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::MissingSolanaCli);
                                        } else if issue.title.contains("config directory missing") {
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::MissingConfigDirectory);
                                        } else if issue.title.contains("keypair missing") {
                                            // Extract keypair path from CLI or config
                                            let cli_config = solana_cli_config::Config::load("~/.config/osvm/config.yml").unwrap_or_default();
                                            let default_keypair_path = matches
                                                .get_one::<String>("keypair")
                                                .map(|s| s.to_string())
                                                .unwrap_or_else(|| cli_config.keypair_path.clone());
                                            repairable_errors.push(crate::utils::self_repair::RepairableError::MissingKeypair(default_keypair_path));
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            if !repairable_errors.is_empty() {
                                let repair_system =
                                    crate::utils::self_repair::SelfRepairSystem::default();
                                match repair_system.repair_automatically(repairable_errors).await {
                                    Ok(crate::utils::self_repair::RepairResult::Success(msg)) => {
                                        println!("✅ {}", msg);
                                    }
                                    Ok(result) => {
                                        println!("⚠️  Repair result: {:?}", result);
                                    }
                                    Err(e) => {
                                        println!("❌ Repair failed: {}", e);
                                    }
                                }
                            } else {
                                println!("ℹ️  No automatically repairable issues found");
                            }
                        } else {
                            println!("🎉 All systems healthy! No repairs needed.");
                        }
                    }
                    Err(e) => {
                        eprintln!("Error running diagnostics: {}", e);
                        exit(1);
                    }
                }
            } else {
                // Just run diagnostics without repair
                let check_all = matches.contains_id("check_all");
                let system_only = matches.contains_id("system_only");
                let user_only = matches.contains_id("user_only");
                let verbose = matches.get_count("verbose") > 0;

                if check_all || (!system_only && !user_only) {
                    println!("🩺 OSVM Comprehensive System Health Check");
                    println!("==========================================");

                    match diagnostic_coordinator.run_detailed_diagnostics().await {
                        Ok(results) => {
                            // Display summary
                            println!("\n📊 SUMMARY");
                            println!("├── Overall Health: {:?}", results.summary.overall_health);
                            println!("├── Total Checks: {}", results.summary.total_checks);
                            println!("├── Passed: {}", results.summary.passed_checks);
                            println!("├── Failed: {}", results.summary.failed_checks);
                            println!("├── Critical Issues: {}", results.summary.critical_issues);
                            println!("└── Warnings: {}", results.summary.warnings);

                            // Display detailed results if verbose
                            if verbose {
                                println!("\n🔍 DETAILED RESULTS");
                                for (name, check) in &results.detailed_checks {
                                    let status = if check.passed { "✅" } else { "❌" };
                                    println!("  {} {}: {}", status, name, check.message);
                                    if let Some(details) = &check.details {
                                        println!("     └── {}", details);
                                    }
                                }
                            }

                            // Display issues and recommendations
                            let health = &results.system_health;
                            if !health.issues.is_empty() {
                                println!("\n⚠️  ISSUES FOUND:");
                                for issue in &health.issues {
                                    let severity_icon = match issue.severity {
                                        crate::utils::diagnostics::IssueSeverity::Critical => "🔴",
                                        crate::utils::diagnostics::IssueSeverity::Error => "🟠",
                                        crate::utils::diagnostics::IssueSeverity::Warning => "🟡",
                                        crate::utils::diagnostics::IssueSeverity::Info => "🔵",
                                    };
                                    println!(
                                        "  {} {}: {}",
                                        severity_icon, issue.title, issue.description
                                    );
                                    if let Some(fix) = &issue.suggested_fix {
                                        println!("     💡 Suggested fix: {}", fix);
                                    }
                                }
                            }

                            if !health.recommendations.is_empty() {
                                println!("\n💡 RECOMMENDATIONS:");
                                for rec in &health.recommendations {
                                    println!("  • {}", rec);
                                }
                            }

                            if health.issues.is_empty() {
                                println!("\n🎉 All systems healthy!");
                            } else {
                                println!(
                                    "\nℹ️  Use 'osvm doctor --fix' to attempt automatic repairs"
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("Error running diagnostics: {}", e);
                            exit(1);
                        }
                    }
                } else {
                    println!("🩺 OSVM Targeted Health Check");
                    println!("=============================");

                    match diagnostic_coordinator.check_system_health().await {
                        Ok(health) => {
                            if system_only {
                                println!("\n🖥️  SYSTEM DEPENDENCIES:");
                                for dep in &health.system_dependencies {
                                    let status = if dep.installed { "✅" } else { "❌" };
                                    let update_info = if dep.update_available {
                                        " (update available)"
                                    } else {
                                        ""
                                    };
                                    println!(
                                        "  {} {}: {}{}",
                                        status,
                                        dep.name,
                                        dep.version.as_deref().unwrap_or("not installed"),
                                        update_info
                                    );
                                }
                            }

                            if user_only {
                                println!("\n👤 USER CONFIGURATION:");
                                let config = &health.user_configuration;
                                println!(
                                    "  {} Solana CLI: {}",
                                    if config.cli_installed { "✅" } else { "❌" },
                                    if config.cli_installed {
                                        config.cli_version.as_deref().unwrap_or("unknown version")
                                    } else {
                                        "not installed"
                                    }
                                );
                                println!(
                                    "  {} Config directory: {}",
                                    if config.config_dir_exists {
                                        "✅"
                                    } else {
                                        "❌"
                                    },
                                    if config.config_dir_exists {
                                        "exists"
                                    } else {
                                        "missing"
                                    }
                                );
                                println!(
                                    "  {} Keypair: {}",
                                    if config.keypair_exists { "✅" } else { "❌" },
                                    if config.keypair_exists {
                                        config.keypair_path.as_deref().unwrap_or("unknown path")
                                    } else {
                                        "missing"
                                    }
                                );
                                println!(
                                    "  {} Network: {}",
                                    if config.current_network.is_some() {
                                        "✅"
                                    } else {
                                        "❌"
                                    },
                                    config
                                        .current_network
                                        .as_deref()
                                        .unwrap_or("not configured")
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("Error checking system health: {}", e);
                            exit(1);
                        }
                    }
                }
            }
        }
        "audit" => {
            // This case should not be reached as audit is handled early
            eprintln!("❌ Audit command handled early - this should not be reached");
            exit(1);
        }
        "new_feature_command" => {
            println!("Expected output for new feature");
        }
        cmd => {
            return Err(format!("Unknown command: {cmd}").into());
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
        #[derive(BorshSerialize, BorshDeserialize, PartialEq, Eq, Debug, Clone)]
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
