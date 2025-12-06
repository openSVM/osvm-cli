// Allow unused code in CLI during active development
// Most warnings are from experimental features and development scaffolding
#![allow(unused)]

use {
    crate::config::Config,
    crate::utils::diagnostics::DiagnosticCoordinator,
    crate::utils::input_sanitization,
    crate::utils::markdown_renderer::MarkdownRenderer,
    crate::utils::{dashboard, ebpf_deploy, examples, nodes, ssh_deploy, svm_info},
    clparse::parse_command_line,
    solana_client::rpc_client::RpcClient,
    solana_sdk::{native_token::Sol, pubkey::Pubkey, signature::Signer},
    std::{process::exit, str::FromStr},
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
pub mod ai_config;
pub mod clparse;
pub mod commands;
pub mod config; // Added
pub mod prelude;
pub mod services;
pub mod utils;

// Config struct is now in src/config.rs

/// Check if a command is a known OSVM command
fn is_known_command(sub_command: &str) -> bool {
    matches!(
        sub_command,
        "balance"
            | "svm"
            | "nodes"
            | "examples"
            | "rpc"
            | "deploy"
            | "invoke"
            | "doctor"
            | "tutorial"
            | "audit"
            | "qa"
            | "ovsm"
            | "mcp"
            | "bbs"
            | "collab"
            | "mount"
            | "snapshot"
            | "stream"
            | "settings"
            | "db"
            | "realtime"
            | "chat"
            | "code"
            | "swap"
            | "degen"
            | "amm"
            | "perp"
            | "agent"
            | "plan"
            | "p"     // Short alias for plan
            | "a"     // Short alias for agent
            | "v"
            | "ver"
            | "version"
    )
}

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

/// Handle QA command for automated testing and bug detection
/// Handle MCP commands using the dedicated MCP service
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Check for -version or -ver directly from args (special case for non-standard formats)
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 2 && (args[1] == "-version" || args[1] == "-ver") {
        let osvm_version = env!("CARGO_PKG_VERSION");
        let ovsm_version = ovsm::VERSION;
        println!("OSVM CLI v{}", osvm_version);
        println!("OVSM Interpreter v{}", ovsm_version);
        return Ok(());
    }

    // Initialize logging to ~/.osvm/logs/log-{timestamp}.log
    // Log all operations for debugging and audit trail
    // Use quiet mode (file-only, no console) for TUI mode to keep terminal clean
    let is_tui_mode = args.iter().any(|a| a == "--tui");
    let init_result = if is_tui_mode {
        crate::utils::logger::init_logging_quiet()
    } else {
        crate::utils::logger::init_logging()
    };
    if let Err(e) = init_result {
        if !is_tui_mode {
            eprintln!("⚠️  Warning: Failed to initialize logging: {}", e);
            eprintln!("   Continuing without file logging...");
        }
    }

    // Capture start time for command logging
    let command_start_time = std::time::Instant::now();
    let command_args: Vec<String> = std::env::args().collect();

    let app_matches = parse_command_line();

    // Check for version flag (which includes aliases)
    if app_matches.get_flag("version_flag") {
        // Show version info and exit
        let osvm_version = env!("CARGO_PKG_VERSION");
        let ovsm_version = ovsm::VERSION;
        println!("OSVM CLI v{}", osvm_version);
        println!("OVSM Interpreter v{}", ovsm_version);
        return Ok(());
    }

    // Check for subcommands (including version subcommands)
    let (sub_command, sub_matches) = match app_matches.subcommand() {
        Some((cmd, matches)) => (cmd, matches),
        None => {
            // No subcommand - bootstrap OSVM agent with microVM isolation
            use crate::services::microvm_launcher::{
                get_default_osvm_config, is_running_in_microvm, MicroVmLauncher,
            };

            // Check if we should skip microVM
            let skip_microvm = std::env::var("OSVM_SKIP_MICROVM")
                .map(|v| v == "1" || v.to_lowercase() == "true")
                .unwrap_or(false);

            if skip_microvm || is_running_in_microvm() {
                // Already in microVM or explicitly skipping - run agent directly
                if is_running_in_microvm() {
                    println!("🚀 OSVM Agent Running in microVM isolation mode");
                    std::env::set_var("OSVM_IN_MICROVM", "1");
                } else {
                    println!("🚀 OSVM Agent Starting (Direct Mode - OSVM_SKIP_MICROVM=1)");
                }
                println!("💡 Use 'osvm --help' to see all available commands\n");

                return crate::utils::agent_chat_v2::run_advanced_agent_chat()
                    .await
                    .map_err(|e| format!("Failed to start advanced chat: {}", e).into());
            }

            // On host - launch in microVM for enhanced security
            println!("🚀 Launching OSVM Agent in microVM...");
            println!();

            // Create microVM launcher
            let launcher = match MicroVmLauncher::new() {
                Ok(l) => l,
                Err(e) => {
                    eprintln!("❌ Failed to initialize microVM launcher: {}", e);
                    eprintln!("   Falling back to direct execution");
                    eprintln!("   💡 Set OSVM_SKIP_MICROVM=1 to suppress this warning\n");

                    return crate::utils::agent_chat_v2::run_advanced_agent_chat()
                        .await
                        .map_err(|e| format!("Failed to start advanced chat: {}", e).into());
                }
            };

            // Get default configuration
            let config = get_default_osvm_config();

            // Launch OSVM runtime in microVM
            match launcher.launch_osvm_runtime(config) {
                Ok(mut handle) => {
                    println!("✅ OSVM microVM launched successfully");
                    println!("   MicroVM is now running in isolated environment");
                    println!("   Press Ctrl+C to stop\n");

                    // Wait for microVM to finish
                    loop {
                        if !handle.is_running() {
                            println!("\n🛑 MicroVM terminated");
                            break;
                        }
                        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                    }

                    return Ok(());
                }
                Err(e) => {
                    eprintln!("❌ Failed to launch microVM: {}", e);
                    eprintln!("   Falling back to direct execution");
                    eprintln!();
                    eprintln!("💡 To fix this:");
                    eprintln!(
                        "   1. Ensure Firecracker is installed: ~/.osvm/bin/firecracker --version"
                    );
                    eprintln!("   2. Check kernel exists: ls -lh ~/.osvm/kernel/vmlinux.bin");
                    eprintln!("   3. Check rootfs exists: ls -lh ~/.osvm/rootfs/osvm-runtime.cpio");
                    eprintln!("   4. Or set OSVM_SKIP_MICROVM=1 to skip microVM launch\n");

                    return crate::utils::agent_chat_v2::run_advanced_agent_chat()
                        .await
                        .map_err(|e| format!("Failed to start advanced chat: {}", e).into());
                }
            }
        }
    };

    // Check for version subcommands
    if sub_command == "v" || sub_command == "ver" || sub_command == "version" {
        // Show version info and exit
        let osvm_version = env!("CARGO_PKG_VERSION");
        let ovsm_version = ovsm::VERSION;
        println!("OSVM CLI v{}", osvm_version);
        println!("OVSM Interpreter v{}", ovsm_version);
        return Ok(());
    }

    // 'matches' will refer to the subcommand's matches, as before.
    let matches = sub_matches;

    // Handle audit command early to avoid config loading that might trigger self-repair
    if sub_command == "audit" {
        return commands::audit_handler::handle_audit_command(&app_matches, sub_matches).await;
    }

    // Handle settings command early - no config loading needed
    if sub_command == "settings" {
        return commands::settings::handle_settings_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle QA command early to avoid config loading that might trigger self-repair
    if sub_command == "qa" {
        return commands::qa_handler::handle_qa_command(&app_matches, sub_matches).await;
    }

    // Handle MCP command early to avoid config loading that might trigger self-repair
    if sub_command == "mcp" {
        return commands::mcp_handler::handle_mcp_command(&app_matches, sub_matches).await;
    }

    // Handle OVSM command early to avoid config loading that might trigger self-repair
    if sub_command == "ovsm" {
        return commands::ovsm_handler::handle_ovsm_command(sub_matches).await;
    }

    // Handle BBS command early - it only needs SQLite, no Solana config
    if sub_command == "bbs" {
        return commands::bbs_handler::handle_bbs_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle collab command early - real-time collaborative investigation
    if sub_command == "collab" {
        return handle_collab_command(sub_matches).await;
    }

    // Handle snapshot command early - it doesn't need keypair or Solana config
    if sub_command == "snapshot" {
        return commands::snapshot::execute_snapshot_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle RPC early - it generates its own keypairs and doesn't need default config
    if sub_command == "rpc" {
        return commands::rpc_manager::handle_rpc_manager(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle chat command early to avoid config loading that might trigger self-repair
    if sub_command == "chat" {
        // Check if test mode is requested
        if sub_matches.get_flag("test") {
            return crate::utils::agent_chat::run_chat_ui_tests()
                .await
                .map_err(|e| e.into());
        } else if sub_matches.get_flag("microvm") {
            // Run chat with microVM isolation for maximum security
            return crate::utils::agent_chat_microvm::run_microvm_agent_chat()
                .await
                .map_err(|e| e.into());
        } else if sub_matches.get_flag("advanced") {
            return crate::utils::agent_chat_v2::run_advanced_agent_chat()
                .await
                .map_err(|e| e.into());
        } else {
            // Check for test mode
            let test_mode = sub_matches.get_flag("test_mode");
            return crate::utils::agent_chat::run_agent_chat_ui_with_mode(test_mode)
                .await
                .map_err(|e| e.into());
        }
    }

    // Handle code command - AI-powered coding assistant
    if sub_command == "code" {
        return commands::code_handler::handle_code_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle swap command - Token swaps via Jupiter
    if sub_command == "swap" {
        return commands::swap_handler::handle_swap_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle degen command - Autonomous trading agent
    if sub_command == "degen" {
        return commands::degen_handler::handle_degen_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle amm command - Liquidity management TUI
    if sub_command == "amm" {
        return commands::amm_handler::handle_amm_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle perp command - Perpetual futures trading TUI
    if sub_command == "perp" {
        return commands::perp_handler::handle_perp_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle research command early - it only needs AI and OVSM services, no Solana config
    if sub_command == "research" {
        return commands::research::handle_research_command(sub_matches)
            .await
            .map_err(|e| e.into());
    }

    // Handle stream command early - it doesn't need keypair or Solana config, just RPC access
    if sub_command == "stream" {
        // StreamCommand has its own clap Parser, so we parse from args
        // Skip "osvm stream" part and just pass the remaining args
        use crate::commands::stream::StreamCommand;
        use clap::Parser;

        let args: Vec<String> = std::env::args().collect();
        // Find "stream" and take everything after it
        let stream_args: Vec<String> = std::iter::once("stream".to_string())
            .chain(
                args.iter()
                    .skip_while(|arg| *arg != "stream")
                    .skip(1)
                    .cloned(),
            )
            .collect();

        let cmd = StreamCommand::parse_from(&stream_args);

        return match crate::commands::stream::execute(cmd).await {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("❌ Stream server failed: {}", e);
                std::process::exit(1);
            }
        };
    }

    // Handle agent command for CLI-based agent execution
    if sub_command == "agent" {
        // Get prompt args (can be multiple words)
        let prompt_parts: Vec<String> = sub_matches
            .get_many::<String>("prompt")
            .map(|values| values.map(|s| s.to_string()).collect())
            .ok_or("No prompt provided for agent command")?;

        let prompt = prompt_parts.join(" ");

        let json_output = sub_matches.get_flag("json");
        let verbose = sub_matches.get_count("verbose");
        let no_tools = sub_matches.get_flag("no-tools");
        let timeout = sub_matches
            .get_one::<String>("timeout")
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(90); // Increased from 30s to 90s for complex agent queries with parallel tool execution

        return crate::utils::agent_cli::execute_agent_command(
            &prompt,
            json_output,
            verbose,
            no_tools,
            timeout,
        )
        .await
        .map_err(|e| e.into());
    }

    // Handle plan command for AI-powered command planning
    if sub_command == "plan" {
        // Get query args (can be multiple words)
        let query_parts: Vec<String> = sub_matches
            .get_many::<String>("query")
            .map(|values| values.map(|s| s.to_string()).collect())
            .ok_or("No query provided for plan command")?;

        let query = query_parts.join(" ");

        let execute = sub_matches.get_flag("execute");
        let yes = sub_matches.get_flag("yes");
        let json_output = sub_matches.get_flag("json");
        let debug = app_matches.get_flag("debug");

        use crate::utils::osvm_command_planner::OsvmCommandPlanner;

        let planner = OsvmCommandPlanner::new(debug);

        println!("🧠 Analyzing your request: \"{}\"", query);
        println!();

        // Create the execution plan
        match planner.create_plan(&query).await {
            Ok(plan) => {
                if json_output {
                    // JSON output mode
                    println!("{}", serde_json::to_string_pretty(&plan)?);
                } else {
                    // Pretty print the plan
                    println!("📋 Execution Plan");
                    println!("{}", "━".repeat(50));
                    println!("💭 Reasoning: {}", plan.reasoning);
                    println!("🎯 Confidence: {:.0}%", plan.confidence * 100.0);
                    println!("🔧 Steps: {}", plan.steps.len());
                    println!();

                    for (i, step) in plan.steps.iter().enumerate() {
                        println!("{}. {}", i + 1, step.full_command);
                        println!("   {}", step.explanation);
                        if step.requires_confirmation {
                            println!("   ⚠️  Requires confirmation");
                        }
                        println!();
                    }

                    println!("✨ Expected outcome: {}", plan.expected_outcome);
                    println!();

                    // Execute if requested
                    if execute {
                        println!("{}", "━".repeat(50));
                        println!("⚡ Executing plan...");
                        println!();

                        match planner.execute_plan(&plan, yes).await {
                            Ok(results) => {
                                let formatted = planner.format_results(&plan, &results);
                                println!("{}", formatted);
                            }
                            Err(e) => {
                                eprintln!("❌ Execution failed: {}", e);
                                return Err(e.into());
                            }
                        }
                    } else {
                        println!("💡 Use --execute to run this plan");
                        println!("💡 Use --execute --yes to skip confirmations");
                    }
                }
            }
            Err(e) => {
                eprintln!("❌ Failed to create execution plan: {}", e);
                eprintln!();
                eprintln!("💡 Try rephrasing your request or use 'osvm examples' for help");
                return Err(e.into());
            }
        }

        return Ok(());
    }

    // Handle short planning/agent aliases early (p and a commands)
    if sub_command == "p" || sub_command == "a" {
        return commands::ai_query::handle_ai_query_with_planning(
            sub_command,
            sub_matches,
            &app_matches,
        )
        .await;
    }

    // Handle AI queries early to avoid config loading
    // Check if this is a streaming agent query (osvm {anything} where {anything} is not a known command)
    if !is_known_command(sub_command) {
        // Check if verbose flag is set
        let verbose = app_matches.get_count("verbose");
        let plan_only = app_matches.get_flag("plan_only");
        let debug = app_matches.get_flag("debug");
        let plan_flag = app_matches.get_flag("plan");

        // Use streaming agent for direct queries
        return crate::utils::streaming_agent::execute_streaming_agent(
            sub_command,
            verbose,
            plan_only,
            plan_flag,
            debug,
        )
        .await
        .map_err(|e| format!("Streaming agent failed: {}", e).into());
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
            commands::balance::handle_balance_command(matches, &rpc_client, &config).await?;
        }
        "svm" => {
            commands::svm_handler::handle_svm_command(matches, &rpc_client, &config)?;
        }
        "nodes" => {
            commands::nodes_handler::handle_nodes_command(matches, &rpc_client, &config).await?;
        }
        "examples" => {
            // Handle the examples command
            if matches.get_flag("list_categories") {
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
        "deploy" => {
            commands::deploy_handler::handle_deploy_command(matches, &config).await?;
        }
        "invoke" => {
            commands::invoke_handler::handle_invoke_command(matches, &config).await?;
        }
        "mcp" => {
            // Handle the MCP command for managing Model Context Protocol servers
            return commands::mcp_handler::handle_mcp_command(&app_matches, sub_matches).await;
        }
        "mount" => {
            // Handle folder mount management for OSVM microVMs
            let Some((mount_sub_command, mount_sub_matches)) = matches.subcommand() else {
                eprintln!("No mount subcommand provided");
                exit(1);
            };

            match mount_sub_command {
                "add" => {
                    let host_path = mount_sub_matches
                        .get_one::<String>("host_path")
                        .expect("host_path is required by clap");
                    let readonly = mount_sub_matches.get_flag("readonly");

                    match crate::commands::mount::handle_mount_add(host_path, readonly) {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("❌ Failed to add mount: {}", e);
                            exit(1);
                        }
                    }
                }
                "remove" => {
                    let host_path = mount_sub_matches
                        .get_one::<String>("host_path")
                        .expect("host_path is required by clap");

                    match crate::commands::mount::handle_mount_remove(host_path) {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("❌ Failed to remove mount: {}", e);
                            exit(1);
                        }
                    }
                }
                "list" => match crate::commands::mount::handle_mount_list() {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("❌ Failed to list mounts: {}", e);
                        exit(1);
                    }
                },
                _ => {
                    eprintln!("Unknown mount subcommand: {}", mount_sub_command);
                    exit(1);
                }
            }
        }
        "snapshot" => {
            // Handle snapshot analysis and management commands
            match crate::commands::snapshot::execute_snapshot_command(sub_matches).await {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Snapshot command failed: {}", e);
                    exit(1);
                }
            }
        }
        "db" => {
            // Handle database management commands
            let Some((db_sub_command, db_sub_matches)) = matches.subcommand() else {
                eprintln!("No database subcommand provided");
                exit(1);
            };

            use crate::commands::database::{execute_database_command, DatabaseArgs};

            let args = DatabaseArgs {
                data_dir: db_sub_matches
                    .get_one::<String>("data-dir")
                    .map(|s| s.to_string()),
                query: db_sub_matches
                    .get_one::<String>("query")
                    .map(|s| s.to_string()),
                limit: db_sub_matches
                    .get_one::<String>("limit")
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(100),
                session_id: db_sub_matches
                    .get_one::<String>("session-id")
                    .map(|s| s.to_string()),
                show_commands: db_sub_matches.get_flag("commands"),
                show_chat: db_sub_matches.get_flag("chat"),
                show_stats: db_sub_matches.get_flag("stats"),
                // Sync arguments
                sync_mode: db_sub_matches
                    .get_one::<String>("mode")
                    .map(|s| s.to_string()),
                programs: db_sub_matches
                    .get_one::<String>("programs")
                    .map(|s| s.to_string()),
                accounts: db_sub_matches
                    .get_one::<String>("accounts")
                    .map(|s| s.to_string()),
                pattern: db_sub_matches
                    .get_one::<String>("pattern")
                    .map(|s| s.to_string()),
                ledger_path: db_sub_matches
                    .get_one::<String>("ledger-path")
                    .map(|s| s.to_string()),
                snapshot_dir: db_sub_matches
                    .get_one::<String>("snapshot-dir")
                    .map(|s| s.to_string()),
            };

            match execute_database_command(db_sub_command, &args).await {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Database command failed: {}", e);
                    exit(1);
                }
            }
        }
        "realtime" => {
            // Handle real-time sync daemon commands
            let Some((realtime_sub_command, realtime_sub_matches)) = matches.subcommand() else {
                eprintln!("No realtime subcommand provided");
                exit(1);
            };

            use crate::commands::realtime::{execute_realtime_command, RealtimeArgs};

            let args = RealtimeArgs {
                pumpfun: realtime_sub_matches.get_flag("pumpfun"),
                programs: realtime_sub_matches
                    .get_one::<String>("programs")
                    .map(|s| s.to_string()),
                accounts: realtime_sub_matches
                    .get_one::<String>("accounts")
                    .map(|s| s.to_string()),
                patterns: realtime_sub_matches
                    .get_one::<String>("patterns")
                    .map(|s| s.to_string()),
                ledger_path: realtime_sub_matches
                    .get_one::<String>("ledger-path")
                    .map(|s| s.to_string()),
                snapshot_dir: realtime_sub_matches
                    .get_one::<String>("snapshot-dir")
                    .map(|s| s.to_string()),
            };

            match execute_realtime_command(realtime_sub_command, &args).await {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("❌ Realtime command failed: {}", e);
                    exit(1);
                }
            }
        }
        "doctor" => {
            commands::doctor_handler::handle_doctor_command(matches).await?;
        }
        "tutorial" => {
            commands::tutorial::run_interactive_tutorial().await?;
        }
        "audit" => {
            // This case should not be reached as audit is handled early to avoid config loading
            eprintln!("❌ Audit command should be handled before config loading");
            eprintln!("   This indicates a programming error - please report this issue.");
            exit(1);
        }
        "qa" => {
            // This case should not be reached as qa is handled early to avoid config loading
            eprintln!("❌ QA command should be handled before config loading");
            eprintln!("   This indicates a programming error - please report this issue.");
            exit(1);
        }
        "ovsm" => {
            // This case should not be reached as ovsm is handled early to avoid config loading
            eprintln!("❌ OVSM command should be handled before config loading");
            eprintln!("   This indicates a programming error - please report this issue.");
            exit(1);
        }
        "research" => {
            // This case should not be reached as research is handled early to avoid config loading
            eprintln!("❌ Research command should be handled before config loading");
            eprintln!("   This indicates a programming error - please report this issue.");
            exit(1);
        }
        // Short planning mode aliases - enable OVSM planning agent
        "p" | "a" => {
            // Extract the actual query from subcommand args
            commands::ai_query::handle_ai_query_with_planning(sub_command, matches, &app_matches)
                .await?;
        }
        cmd => {
            // Check if this is meant to be an AI query (external subcommand)
            // Route to AI query handler which will validate if it's natural language
            commands::ai_query::handle_ai_query(cmd, matches, &app_matches).await?;
        }
    };

    // Log successful command execution
    let command_duration = command_start_time.elapsed();
    let result = Ok(());
    log_command_execution(sub_command, &command_args[1..], &result, command_duration).await;

    Ok(())
}

/// Handle collaborative investigation commands
async fn handle_collab_command(
    sub_matches: &clap::ArgMatches,
) -> Result<(), Box<dyn std::error::Error>> {
    use crate::services::collab_service;

    // Get the subcommand
    let subcommand = sub_matches.subcommand();

    match subcommand {
        Some(("start", args)) => {
            let name = args.get_one::<String>("name").cloned();
            let wallet = args.get_one::<String>("wallet").cloned();
            let max = args.get_one::<String>("max").and_then(|s| s.parse().ok());
            let password = args.get_one::<String>("password").cloned();
            collab_service::start_session(name, wallet, max, password).await?;
        }
        Some(("join", args)) => {
            let code = args
                .get_one::<String>("code")
                .ok_or("Invite code required")?;
            let name = args.get_one::<String>("name").cloned();
            collab_service::join_session(code, name).await?;
        }
        Some(("list", _)) | Some(("ls", _)) => {
            collab_service::list_sessions().await?;
        }
        Some(("annotate", args)) | Some(("note", args)) => {
            let target = args.get_one::<String>("target").ok_or("Target required")?;
            let text = args
                .get_one::<String>("text")
                .ok_or("Annotation text required")?;
            let severity = args.get_one::<String>("severity").cloned();
            collab_service::add_annotation(None, target, text, severity).await?;
        }
        Some(("annotations", _)) => {
            collab_service::list_annotations(None).await?;
        }
        Some(("server", args)) => {
            let port = args
                .get_one::<String>("port")
                .and_then(|s| s.parse().ok())
                .unwrap_or(8080);
            let host = args.get_one::<String>("host").cloned();
            collab_service::start_server(port, host).await?;
        }
        // Federation commands
        Some(("peers", peer_args)) => match peer_args.subcommand() {
            Some(("add", add_args)) => {
                let address = add_args
                    .get_one::<String>("address")
                    .ok_or("Peer address required")?;
                collab_service::add_federation_peer(address).await?;
            }
            Some(("remove", rm_args)) => {
                let address = rm_args
                    .get_one::<String>("address")
                    .ok_or("Peer address required")?;
                collab_service::remove_federation_peer(address).await?;
            }
            Some(("list", _)) | None => {
                collab_service::list_federation_peers().await?;
            }
            _ => {
                collab_service::list_federation_peers().await?;
            }
        },
        Some(("discover", _)) => {
            collab_service::discover_sessions().await?;
        }
        Some(("publish", _)) => {
            collab_service::publish_session_to_federation(None).await?;
        }
        Some(("status", _)) => {
            collab_service::show_federation_status().await?;
        }
        Some(("help", _)) | None => {
            collab_service::print_help();
        }
        Some((cmd, _)) => {
            eprintln!("Unknown collab subcommand: {}", cmd);
            collab_service::print_help();
        }
    }

    Ok(())
}

/// Log command execution to ClickHouse if available
async fn log_command_execution(
    command_name: &str,
    args: &[String],
    result: &Result<(), Box<dyn std::error::Error>>,
    duration: std::time::Duration,
) {
    // Try to log to ClickHouse if it's running
    use crate::services::activity_logger::ActivityLogger;
    use crate::services::clickhouse_service::{ClickHouseService, ClickHouseStatus};

    if let Ok(service) = ClickHouseService::new() {
        if let Ok(status) = service.status().await {
            if matches!(status, ClickHouseStatus::Running) {
                let logger = ActivityLogger::new(std::sync::Arc::new(service));
                let exit_code = if result.is_ok() { 0 } else { 1 };
                let duration_ms = duration.as_millis() as u64;
                let error_message = result.as_ref().err().map(|e| e.to_string());

                let _ = logger
                    .log_command(
                        command_name,
                        args,
                        exit_code,
                        duration_ms,
                        error_message.as_deref(),
                    )
                    .await;

                // Flush immediately for CLI commands
                let _ = logger.flush_commands().await;
            }
        }
    }
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
