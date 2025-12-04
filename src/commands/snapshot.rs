//! Snapshot management and analysis commands for OSVM CLI

use anyhow::{Context, Result};
use clap::ArgMatches;
use std::path::PathBuf;

use crate::services::snapshot_service::{
    ExportFormat, FilterOptions, OutputConfig, SnapshotReader, SnapshotService, Statistics,
};

/// Execute snapshot-related commands
pub async fn execute_snapshot_command(matches: &ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("read", sub_matches)) => read_snapshot(sub_matches).await,
        Some(("stats", sub_matches)) => show_statistics(sub_matches).await,
        Some(("export", sub_matches)) => export_snapshot(sub_matches).await,
        Some(("compare", sub_matches)) => compare_snapshots(sub_matches).await,
        Some(("validate", sub_matches)) => validate_snapshot(sub_matches).await,
        Some(("find", sub_matches)) => find_account(sub_matches).await,
        Some(("interactive", sub_matches)) => interactive_mode(sub_matches).await,
        Some(("download", sub_matches)) => download_snapshot(sub_matches).await,
        _ => {
            eprintln!("No snapshot subcommand provided. Use --help for usage information.");
            std::process::exit(1);
        }
    }
}

/// Read and display snapshot accounts
async fn read_snapshot(matches: &ArgMatches) -> Result<()> {
    let snapshot_dir = get_snapshot_dir(matches)?;
    let output_config = get_output_config(matches)?;
    let filter_options = get_filter_options(matches)?;

    let mut reader = SnapshotReader::new(snapshot_dir)?;

    if matches.get_flag("parallel") {
        reader.enable_parallel_processing(
            matches
                .get_one::<String>("threads")
                .and_then(|s| s.parse().ok())
                .unwrap_or_else(num_cpus::get),
        );
    }

    let limit = matches
        .get_one::<String>("limit")
        .and_then(|s| s.parse().ok());

    let offset = matches
        .get_one::<String>("offset")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    reader
        .read_accounts(output_config, filter_options, limit, offset)
        .await
}

/// Show snapshot statistics
async fn show_statistics(matches: &ArgMatches) -> Result<()> {
    let snapshot_dir = get_snapshot_dir(matches)?;
    let service = SnapshotService::new(snapshot_dir)?;

    let stats = service.compute_statistics().await?;

    if matches.get_flag("json") {
        println!("{}", serde_json::to_string_pretty(&stats)?);
    } else {
        display_statistics(&stats);
    }

    Ok(())
}

/// Export snapshot to various formats
async fn export_snapshot(matches: &ArgMatches) -> Result<()> {
    let snapshot_dir = get_snapshot_dir(matches)?;
    let output_file = matches
        .get_one::<String>("output")
        .context("Output file is required")?;

    let format = match matches.get_one::<String>("format").map(|s| s.as_str()) {
        Some("json") => ExportFormat::Json,
        Some("csv") => ExportFormat::Csv,
        Some("parquet") => ExportFormat::Parquet,
        Some("msgpack") => ExportFormat::MessagePack,
        _ => ExportFormat::Json,
    };

    let filter_options = get_filter_options(matches)?;
    let service = SnapshotService::new(snapshot_dir)?;

    service
        .export(output_file.into(), format, filter_options)
        .await
}

/// Compare two snapshots
async fn compare_snapshots(matches: &ArgMatches) -> Result<()> {
    let snapshot1 = PathBuf::from(
        matches
            .get_one::<String>("snapshot1")
            .context("First snapshot directory is required")?,
    );

    let snapshot2 = PathBuf::from(
        matches
            .get_one::<String>("snapshot2")
            .context("Second snapshot directory is required")?,
    );

    let service = SnapshotService::new(snapshot1)?;
    let diff = service.compare_with(&snapshot2).await?;

    if matches.get_flag("json") {
        println!("{}", serde_json::to_string_pretty(&diff)?);
    } else {
        display_diff(&diff);
    }

    Ok(())
}

/// Validate snapshot integrity
async fn validate_snapshot(matches: &ArgMatches) -> Result<()> {
    let snapshot_dir = get_snapshot_dir(matches)?;
    let service = SnapshotService::new(snapshot_dir)?;

    let validation = service.validate().await?;

    if matches.get_flag("json") {
        println!("{}", serde_json::to_string_pretty(&validation)?);
    } else {
        display_validation(&validation);
    }

    Ok(())
}

/// Find specific account by pubkey
async fn find_account(matches: &ArgMatches) -> Result<()> {
    let snapshot_dir = get_snapshot_dir(matches)?;
    let pubkey = matches
        .get_one::<String>("pubkey")
        .context("Account pubkey is required")?;

    let service = SnapshotService::new(snapshot_dir)?;
    let account = service.find_account(pubkey).await?;

    if matches.get_flag("json") {
        println!("{}", serde_json::to_string_pretty(&account)?);
    } else {
        display_account(&account);
    }

    Ok(())
}

/// Launch interactive TUI mode
async fn interactive_mode(_matches: &ArgMatches) -> Result<()> {
    use crate::services::snapshot_service::SnapshotTui;

    let mut tui = SnapshotTui::new()?;
    tui.run().await
}

// Helper functions

fn get_snapshot_dir(matches: &ArgMatches) -> Result<PathBuf> {
    let dir = matches
        .get_one::<String>("snapshot-dir")
        .map(PathBuf::from)
        .or_else(|| {
            std::env::var("HOME").ok().map(|home| {
                PathBuf::from(home).join(".config/osvm/ledgers/devnet/remote/extracted")
            })
        })
        .context("Snapshot directory not specified and HOME not set")?;

    Ok(dir)
}

fn get_output_config(matches: &ArgMatches) -> Result<OutputConfig> {
    // Check both --no-color flag and NO_COLOR environment variable
    let colorized = !matches.get_flag("no-color") && std::env::var("NO_COLOR").is_err();

    Ok(OutputConfig {
        format: if matches.get_flag("json") {
            crate::services::snapshot_service::OutputFormat::Json
        } else {
            crate::services::snapshot_service::OutputFormat::Text
        },
        colorized,
        quiet: matches.get_flag("quiet"),
        show_progress: !matches.get_flag("quiet"),
        human_readable: !matches.get_flag("json"),
    })
}

fn get_filter_options(matches: &ArgMatches) -> Result<FilterOptions> {
    Ok(FilterOptions {
        owner: matches.get_one::<String>("filter-owner").cloned(),
        min_balance: matches
            .get_one::<String>("filter-min-balance")
            .and_then(|s| s.parse().ok()),
        max_balance: matches
            .get_one::<String>("filter-max-balance")
            .and_then(|s| s.parse().ok()),
        min_size: matches
            .get_one::<String>("filter-min-size")
            .and_then(|s| s.parse().ok()),
        max_size: matches
            .get_one::<String>("filter-max-size")
            .and_then(|s| s.parse().ok()),
        executable: matches.get_flag("filter-executable").then_some(true),
        rent_exempt: matches.get_flag("filter-rent-exempt").then_some(true),
    })
}

fn display_statistics(stats: &Statistics) {
    use colored::Colorize;

    println!("\n{}", "=".repeat(80).cyan());
    println!("{}", "Snapshot Statistics".bold().cyan());
    println!("{}", "=".repeat(80).cyan());
    println!();

    println!(
        "{:30} {}",
        "Total Accounts:".bold(),
        stats.total_accounts.to_string().green()
    );
    println!(
        "{:30} {}",
        "Total Lamports:".bold(),
        format_lamports(stats.total_lamports)
    );
    println!(
        "{:30} {}",
        "Average Balance:".bold(),
        format_lamports(stats.average_balance)
    );
    println!(
        "{:30} {}",
        "Median Balance:".bold(),
        format_lamports(stats.median_balance)
    );
    println!(
        "{:30} {} bytes",
        "Total Data Size:".bold(),
        format_bytes(stats.total_data_size)
    );
    println!(
        "{:30} {} bytes",
        "Average Data Size:".bold(),
        format_bytes(stats.average_data_size)
    );
    println!();

    if !stats.accounts_by_owner.is_empty() {
        println!("{}", "Top 10 Programs by Account Count:".bold());
        for (i, (owner, count)) in stats.accounts_by_owner.iter().take(10).enumerate() {
            println!(
                "  {:2}. {} ({} accounts)",
                i + 1,
                owner.bright_blue(),
                count
            );
        }
        println!();
    }

    if !stats.largest_accounts.is_empty() {
        println!("{}", "Top 10 Largest Accounts:".bold());
        for (i, (pubkey, lamports)) in stats.largest_accounts.iter().take(10).enumerate() {
            println!(
                "  {:2}. {} ({})",
                i + 1,
                pubkey.bright_blue(),
                format_lamports(*lamports)
            );
        }
        println!();
    }

    println!("{}", "=".repeat(80).cyan());
}

fn display_diff(diff: &serde_json::Value) {
    use colored::Colorize;

    println!("\n{}", "=".repeat(80).cyan());
    println!("{}", "Snapshot Comparison".bold().cyan());
    println!("{}", "=".repeat(80).cyan());
    println!();

    if let Some(snapshot1) = diff.get("snapshot1").and_then(|v| v.as_str()) {
        println!("{}: {}", "Snapshot 1".bold(), snapshot1);
    }
    if let Some(snapshot2) = diff.get("snapshot2").and_then(|v| v.as_str()) {
        println!("{}: {}", "Snapshot 2".bold(), snapshot2);
    }
    println!();

    if let Some(summary) = diff.get("summary") {
        println!("{}", "Summary".bold().yellow());
        println!(
            "  Total accounts (snapshot 1): {}",
            summary
                .get("total_accounts_snapshot1")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  Total accounts (snapshot 2): {}",
            summary
                .get("total_accounts_snapshot2")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  {} New accounts: {}",
            "✨".green(),
            summary
                .get("new_accounts")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  {} Deleted accounts: {}",
            "🗑️".red(),
            summary
                .get("deleted_accounts")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  {} Modified accounts: {}",
            "📝".yellow(),
            summary
                .get("modified_accounts")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  {} Unchanged accounts: {}",
            "✓".green(),
            summary
                .get("unchanged_accounts")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!();
    }

    if let Some(new_accounts) = diff.get("new_accounts").and_then(|v| v.as_array()) {
        if !new_accounts.is_empty() {
            println!(
                "{} {} New Accounts",
                "✨".green(),
                new_accounts.len().to_string().bold()
            );
            for (i, account) in new_accounts.iter().take(10).enumerate() {
                println!(
                    "  {}. {} ({} lamports)",
                    i + 1,
                    account
                        .get("account")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown"),
                    account
                        .get("lamports")
                        .and_then(|v| v.as_u64())
                        .unwrap_or(0)
                );
            }
            if new_accounts.len() > 10 {
                println!("  ... and {} more", new_accounts.len() - 10);
            }
            println!();
        }
    }

    if let Some(deleted) = diff.get("deleted_accounts").and_then(|v| v.as_array()) {
        if !deleted.is_empty() {
            println!(
                "{} {} Deleted Accounts",
                "🗑️".red(),
                deleted.len().to_string().bold()
            );
            for (i, account) in deleted.iter().take(10).enumerate() {
                println!(
                    "  {}. {} ({} lamports)",
                    i + 1,
                    account
                        .get("account")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown"),
                    account
                        .get("lamports")
                        .and_then(|v| v.as_u64())
                        .unwrap_or(0)
                );
            }
            if deleted.len() > 10 {
                println!("  ... and {} more", deleted.len() - 10);
            }
            println!();
        }
    }

    if let Some(modified) = diff.get("modified_accounts").and_then(|v| v.as_array()) {
        if !modified.is_empty() {
            println!(
                "{} {} Modified Accounts",
                "📝".yellow(),
                modified.len().to_string().bold()
            );
            for (i, account) in modified.iter().take(10).enumerate() {
                let account_name = account
                    .get("account")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown");
                println!("  {}. {}", i + 1, account_name);

                if let Some(changes) = account.get("changes") {
                    if let Some(lamports) = changes.get("lamports") {
                        let old = lamports.get("old").and_then(|v| v.as_u64()).unwrap_or(0);
                        let new = lamports.get("new").and_then(|v| v.as_u64()).unwrap_or(0);
                        let diff = lamports.get("diff").and_then(|v| v.as_i64()).unwrap_or(0);
                        println!("     Lamports: {} → {} ({:+})", old, new, diff);
                    }
                }
            }
            if modified.len() > 10 {
                println!("  ... and {} more", modified.len() - 10);
            }
            println!();
        }
    }

    println!("{}", "=".repeat(80).cyan());
}

fn display_validation(validation: &serde_json::Value) {
    use colored::Colorize;

    println!("\n{}", "=".repeat(80).cyan());
    println!("{}", "Snapshot Validation".bold().cyan());
    println!("{}", "=".repeat(80).cyan());
    println!();

    let is_valid = validation
        .get("valid")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    println!(
        "{}: {}",
        "Status".bold(),
        if is_valid {
            "✅ VALID".green()
        } else {
            "❌ INVALID".red()
        }
    );
    println!();

    if let Some(summary) = validation.get("summary") {
        println!("{}", "Summary".bold().yellow());
        println!(
            "  Total files: {}",
            summary
                .get("total_files")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  Successfully read: {}",
            summary
                .get("successful_reads")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  Failed reads: {}",
            summary
                .get("failed_reads")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!(
            "  Total lamports: {}",
            summary
                .get("total_lamports")
                .and_then(|v| v.as_u64())
                .unwrap_or(0)
        );
        println!();
    }

    if let Some(errors) = validation.get("errors").and_then(|v| v.as_array()) {
        if !errors.is_empty() {
            println!(
                "{} {} Errors Found",
                "❌".red(),
                errors.len().to_string().bold()
            );
            for error in errors {
                if let Some(err_msg) = error.as_str() {
                    println!("  • {}", err_msg.red());
                }
            }
            println!();
        }
    }

    if let Some(warnings) = validation.get("warnings").and_then(|v| v.as_array()) {
        if !warnings.is_empty() {
            println!(
                "{} {} Warnings",
                "⚠️".yellow(),
                warnings.len().to_string().bold()
            );
            for warning in warnings {
                if let Some(warn_msg) = warning.as_str() {
                    println!("  • {}", warn_msg.yellow());
                }
            }
            println!();
        }
    }

    if let Some(info) = validation.get("info").and_then(|v| v.as_array()) {
        if !info.is_empty() {
            println!("{}", "Information".bold().blue());
            for info_msg in info {
                if let Some(msg) = info_msg.as_str() {
                    println!("  ℹ️  {}", msg);
                }
            }
            println!();
        }
    }

    println!("{}", "=".repeat(80).cyan());
}

fn display_account(account: &serde_json::Value) {
    use colored::Colorize;

    println!("\n{}", "=".repeat(80).cyan());
    println!("{}", "Account Search Result".bold().cyan());
    println!("{}", "=".repeat(80).cyan());
    println!();

    let found = account
        .get("found")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    if found {
        println!("{}: {}", "Status".bold(), "✅ FOUND".green());

        if let Some(file) = account.get("file").and_then(|v| v.as_str()) {
            println!("{}: {}", "File".bold(), file.bright_black());
        }
        println!();

        if let Some(acc_data) = account.get("account") {
            if let Some(pubkey) = acc_data.get("pubkey").and_then(|v| v.as_str()) {
                println!("{}: {}", "Pubkey".bold(), pubkey.bright_blue());
            }
            if let Some(lamports) = acc_data.get("lamports").and_then(|v| v.as_u64()) {
                println!(
                    "{}: {}",
                    "Lamports".bold(),
                    format_lamports(lamports).green()
                );
            }
            if let Some(data_len) = acc_data.get("data_len").and_then(|v| v.as_u64()) {
                println!("{}: {} bytes", "Data Length".bold(), data_len);
            }
            if let Some(owner) = acc_data.get("owner").and_then(|v| v.as_str()) {
                println!("{}: {}", "Owner".bold(), owner.bright_blue());
            }
            if let Some(executable) = acc_data.get("executable").and_then(|v| v.as_bool()) {
                println!("{}: {}", "Executable".bold(), executable);
            }
            if let Some(rent_epoch) = acc_data.get("rent_epoch").and_then(|v| v.as_u64()) {
                println!("{}: {}", "Rent Epoch".bold(), rent_epoch);
            }
            if let Some(data_preview) = acc_data.get("data_preview").and_then(|v| v.as_str()) {
                println!("{}: {}", "Data Preview".bold(), data_preview.dimmed());
            }
        }
    } else {
        println!("{}: {}", "Status".bold(), "❌ NOT FOUND".red());

        if let Some(message) = account.get("message").and_then(|v| v.as_str()) {
            println!("{}: {}", "Message".bold(), message);
        }
        if let Some(searched) = account.get("searched_files").and_then(|v| v.as_u64()) {
            println!("{}: {} files", "Searched".bold(), searched);
        }
    }

    println!();
    println!("{}", "=".repeat(80).cyan());
}

fn format_lamports(lamports: u64) -> String {
    if lamports >= 1_000_000_000 {
        format!("{:.2} SOL", lamports as f64 / 1_000_000_000.0)
    } else if lamports >= 1_000_000 {
        format!("{:.2}M lamports", lamports as f64 / 1_000_000.0)
    } else if lamports >= 1_000 {
        format!("{:.2}K lamports", lamports as f64 / 1_000.0)
    } else {
        format!("{} lamports", lamports)
    }
}

fn format_bytes(bytes: u64) -> String {
    if bytes >= 1_073_741_824 {
        format!("{:.2} GB", bytes as f64 / 1_073_741_824.0)
    } else if bytes >= 1_048_576 {
        format!("{:.2} MB", bytes as f64 / 1_048_576.0)
    } else if bytes >= 1_024 {
        format!("{:.2} KB", bytes as f64 / 1_024.0)
    } else {
        format!("{}", bytes)
    }
}

/// Download snapshot to remote server via SSH
async fn download_snapshot(matches: &ArgMatches) -> Result<()> {
    use crate::utils::ssh_deploy::{ServerConfig, SshClient};

    let connection = matches
        .get_one::<String>("connection")
        .context("Connection string required")?;

    let network = matches
        .get_one::<String>("network")
        .map(|s| s.as_str())
        .unwrap_or("mainnet");

    let ledger_dir = matches
        .get_one::<String>("ledger-dir")
        .map(|s| s.as_str())
        .unwrap_or("/opt/osvm/solana/ledger");

    println!(
        "📥 Downloading {} snapshot via SSH to {}...",
        network, connection
    );

    // Parse connection string
    let server_config = ServerConfig::from_connection_string(connection)
        .context("Invalid connection string format (use user@host[:port])")?;

    // Create and connect SSH client
    let mut client = SshClient::new(server_config)
        .map_err(|e| anyhow::anyhow!("Failed to create SSH client: {}", e))?;

    client
        .connect()
        .map_err(|e| anyhow::anyhow!("Failed to establish SSH connection: {}", e))?;

    println!("✅ Connected to {}", connection);

    // Ensure ledger directory exists
    client
        .create_directory(ledger_dir)
        .map_err(|e| anyhow::anyhow!("Failed to create ledger directory: {}", e))?;

    println!("📂 Ledger directory: {}", ledger_dir);

    // Download snapshot using agave-validator's built-in snapshot fetcher
    println!("⏳ Downloading snapshot (this may take 10-30 minutes)...");

    // Determine RPC URL based on network
    let rpc_url = match network {
        "mainnet" => "https://api.mainnet-beta.solana.com",
        "devnet" => "https://api.devnet.solana.com",
        "testnet" => "https://api.testnet.solana.com",
        _ => "https://api.mainnet-beta.solana.com",
    };

    // Use agave-validator to fetch snapshot - it has built-in snapshot download capability
    let download_cmd = format!(
        "cd {} && ~/.local/share/solana/install/active_release/bin/agave-validator \
        --ledger {} \
        --rpc-port 9999 \
        --snapshot-fetch-url {} \
        --no-wait-for-vote-to-start-leader \
        --no-voting \
        --no-port-check \
        --expected-genesis-hash {} \
        --limit-ledger-size 50000000 > /tmp/snapshot-download.log 2>&1 & \
        sleep 2 && VALIDATOR_PID=$! && echo \"Started validator PID: $VALIDATOR_PID\" && \
        tail -f /tmp/snapshot-download.log & TAIL_PID=$! && \
        while kill -0 $VALIDATOR_PID 2>/dev/null; do \
          if grep -q 'Snapshot downloaded and unpacked' /tmp/snapshot-download.log; then \
            echo 'Snapshot download complete!' && kill $VALIDATOR_PID $TAIL_PID 2>/dev/null && break; \
          fi; \
          sleep 5; \
        done; \
        wait $VALIDATOR_PID 2>/dev/null; \
        kill $TAIL_PID 2>/dev/null; \
        echo 'Download process finished'",
        ledger_dir,
        ledger_dir,
        rpc_url,
        match network {
            "mainnet" => "5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d",
            "devnet" => "EtWTRABZaYq6iMfeYKouRu166VU2xqa1wcaWoxPkrZBG",
            "testnet" => "4uhcVJyU9pJkvQyS88uRDiswHXSCkY3zQawwpjk2NsNY",
            _ => "5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d",
        }
    );

    let result = client
        .execute_command(&download_cmd)
        .map_err(|e| anyhow::anyhow!("Snapshot download failed: {}", e))?;

    println!("✅ Snapshot downloaded successfully!");
    println!("\n📊 Download result:\n{}", result);

    Ok(())
}
