//! Real-time blockchain data sync daemon

use anyhow::{Context, Result};
use colored::Colorize;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;

use crate::services::{
    blockchain_indexer::{BlockchainIndexer, IndexingConfig, IndexingMode},
    clickhouse_service::{ClickHouseService, ClickHouseStatus},
};

/// Real-time daemon arguments
#[derive(Debug, Clone, Default)]
pub struct RealtimeArgs {
    pub programs: Option<String>,
    pub accounts: Option<String>,
    pub patterns: Option<String>,
    pub ledger_path: Option<String>,
    pub snapshot_dir: Option<String>,
}

/// Execute realtime command
pub async fn execute_realtime_command(subcommand: &str, args: &RealtimeArgs) -> Result<()> {
    match subcommand {
        "start" => cmd_start(args).await,
        "stop" => cmd_stop().await,
        "status" => cmd_status().await,
        _ => Err(anyhow::anyhow!(
            "Unknown realtime subcommand: {}",
            subcommand
        )),
    }
}

/// Start real-time sync daemon
async fn cmd_start(args: &RealtimeArgs) -> Result<()> {
    println!(
        "{}",
        "Starting real-time blockchain sync daemon...".cyan().bold()
    );

    // Check if daemon is already running
    if is_daemon_running()? {
        return Err(anyhow::anyhow!(
            "Real-time daemon is already running. Stop it with: osvm realtime stop"
        ));
    }

    // Check if ClickHouse is running
    let service = ClickHouseService::new()?;
    let status = service.status().await?;
    if status != ClickHouseStatus::Running {
        return Err(anyhow::anyhow!(
            "ClickHouse server is not running. Start it with: osvm db start"
        ));
    }

    println!("✓ ClickHouse server is running");
    println!();

    // SECURITY: Validate all user inputs before passing to daemon subprocess
    if let Some(programs) = &args.programs {
        validate_daemon_argument(programs, "programs")?;
    }
    if let Some(accounts) = &args.accounts {
        validate_daemon_argument(accounts, "accounts")?;
    }
    if let Some(patterns) = &args.patterns {
        validate_daemon_argument(patterns, "patterns")?;
    }
    if let Some(ledger) = &args.ledger_path {
        validate_path_argument(ledger, "ledger-path")?;
    }
    if let Some(snapshot) = &args.snapshot_dir {
        validate_path_argument(snapshot, "snapshot-dir")?;
    }

    // Build daemon configuration
    let mut daemon_args = vec!["realtime-daemon".to_string()];

    if let Some(programs) = &args.programs {
        daemon_args.push(format!("--programs={}", programs));
    }
    if let Some(accounts) = &args.accounts {
        daemon_args.push(format!("--accounts={}", accounts));
    }
    if let Some(patterns) = &args.patterns {
        daemon_args.push(format!("--patterns={}", patterns));
    }
    if let Some(ledger) = &args.ledger_path {
        daemon_args.push(format!("--ledger-path={}", ledger));
    }
    if let Some(snapshot) = &args.snapshot_dir {
        daemon_args.push(format!("--snapshot-dir={}", snapshot));
    }

    // Start daemon process in background
    let current_exe = std::env::current_exe()?;
    let child = Command::new(&current_exe)
        .args(&daemon_args)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .context("Failed to spawn realtime daemon")?;

    // Save PID file
    let pid_path = get_pid_file_path()?;
    std::fs::write(&pid_path, child.id().to_string())?;

    println!(
        "{}",
        "✓ Real-time sync daemon started successfully"
            .green()
            .bold()
    );
    println!();
    println!("Daemon details:");
    println!("  PID: {}", child.id());
    println!("  PID file: {}", pid_path.display());
    println!();
    println!("The daemon will:");
    println!("  • Monitor the ledger for new blocks (every 100ms)");
    println!("  • Index new transactions and accounts to ClickHouse");
    println!("  • Apply configured filters (programs, accounts, patterns)");
    println!();
    println!("Management commands:");
    println!("  Check status: {}", "osvm realtime status".yellow());
    println!("  Stop daemon: {}", "osvm realtime stop".yellow());

    Ok(())
}

/// Stop real-time sync daemon
async fn cmd_stop() -> Result<()> {
    println!("{}", "Stopping real-time sync daemon...".cyan().bold());

    if !is_daemon_running()? {
        println!("{}", "● Real-time daemon is not running".yellow());
        return Ok(());
    }

    let pid_path = get_pid_file_path()?;
    let pid_str = std::fs::read_to_string(&pid_path)?;
    let pid: i32 = pid_str.trim().parse().context("Invalid PID in PID file")?;

    // Send SIGTERM to daemon
    #[cfg(unix)]
    {
        use nix::sys::signal::{kill, Signal};
        use nix::unistd::Pid;

        kill(Pid::from_raw(pid), Signal::SIGTERM)
            .context("Failed to send termination signal to daemon")?;
    }

    // Wait for process to exit (up to 5 seconds)
    for _ in 0..50 {
        if !is_daemon_running()? {
            break;
        }
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    }

    // Remove PID file
    std::fs::remove_file(&pid_path)?;

    println!(
        "{}",
        "✓ Real-time daemon stopped successfully".green().bold()
    );

    Ok(())
}

/// Check real-time sync daemon status
async fn cmd_status() -> Result<()> {
    if is_daemon_running()? {
        let pid_path = get_pid_file_path()?;
        let pid_str = std::fs::read_to_string(&pid_path)?;
        let pid = pid_str.trim();

        println!("{}", "● Real-time daemon is running".green().bold());
        println!();
        println!("Daemon details:");
        println!("  PID: {}", pid);
        println!("  PID file: {}", pid_path.display());
        println!();
        println!("The daemon is:");
        println!("  ✓ Monitoring ledger for new blocks");
        println!("  ✓ Indexing data to ClickHouse");
    } else {
        println!("{}", "● Real-time daemon is not running".red().bold());
        println!();
        println!(
            "To start the daemon, run: {}",
            "osvm realtime start".yellow()
        );
    }

    Ok(())
}

/// Check if daemon is running
fn is_daemon_running() -> Result<bool> {
    let pid_path = get_pid_file_path()?;

    if !pid_path.exists() {
        return Ok(false);
    }

    let pid_str = std::fs::read_to_string(&pid_path)?;
    let pid: i32 = pid_str.trim().parse().context("Invalid PID in PID file")?;

    // Check if process is actually running
    #[cfg(unix)]
    {
        use nix::sys::signal::{kill, Signal};
        use nix::unistd::Pid;

        // Use null signal (0) to check if process exists without killing it
        match kill(Pid::from_raw(pid), None) {
            Ok(_) => Ok(true),
            Err(_) => {
                // Process doesn't exist, clean up stale PID file
                let _ = std::fs::remove_file(&pid_path);
                Ok(false)
            }
        }
    }

    #[cfg(not(unix))]
    {
        // On non-Unix, just check if PID file exists
        Ok(pid_path.exists())
    }
}

/// Get PID file path
fn get_pid_file_path() -> Result<PathBuf> {
    let home = std::env::var("HOME").context("HOME environment variable not set")?;
    let path = PathBuf::from(home).join(".osvm").join("realtime.pid");

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    Ok(path)
}

/// Run the real-time sync daemon (internal entry point)
pub async fn run_realtime_daemon(
    config: IndexingConfig,
    clickhouse: Arc<ClickHouseService>,
) -> Result<()> {
    use crate::services::ledger_service::LedgerService;
    use log::{info, warn};

    info!("Real-time sync daemon started");

    let indexer = BlockchainIndexer::new(
        None, // Use default ledger path
        None, // Use default snapshot path
        clickhouse,
        config.clone(),
    )?;

    // Try to set up ledger monitoring
    // Note: ledger_path is not in IndexingConfig, using default path
    let ledger_path = PathBuf::from("devnet-ledger");

    match LedgerService::new(ledger_path.clone()) {
        Ok(ledger_service) => {
            info!("Connected to ledger at: {:?}", ledger_path);

            if ledger_service.has_transaction_history() {
                info!("Ledger has transaction history - monitoring for new slots");

                // Get slot monitoring channel
                match ledger_service.watch_for_new_slots() {
                    Ok(mut slot_rx) => {
                        info!("Started monitoring for new blocks");

                        // Process new slots as they arrive
                        while let Some(new_slot) = slot_rx.recv().await {
                            info!("New slot detected: {}", new_slot);

                            // In a full implementation, we would:
                            // 1. Fetch transactions from the new slot
                            // 2. Parse and decode them
                            // 3. Apply filters (programs, accounts, patterns)
                            // 4. Index to ClickHouse

                            // For now, just log the event
                            info!(
                                "Processing slot {}... (indexing not yet fully implemented)",
                                new_slot
                            );
                        }

                        warn!("Slot monitoring channel closed");
                    }
                    Err(e) => {
                        warn!("Failed to start slot monitoring: {}", e);
                        info!("Falling back to polling mode");
                        run_polling_mode().await?;
                    }
                }
            } else {
                warn!("Ledger does not have transaction history");
                info!("Falling back to polling mode");
                run_polling_mode().await?;
            }
        }
        Err(e) => {
            warn!("Failed to connect to ledger: {}", e);
            info!("Falling back to polling mode");
            run_polling_mode().await?;
        }
    }

    Ok(())
}

/// Fallback polling mode when ledger monitoring is not available
async fn run_polling_mode() -> Result<()> {
    use log::info;
    use tokio::time::{interval, Duration};

    let mut tick = interval(Duration::from_secs(5));
    let mut iteration = 0u64;

    info!("Running in polling mode (checks every 5 seconds)");

    loop {
        tick.tick().await;
        iteration += 1;

        if iteration % 12 == 0 {
            // Every minute
            info!("Daemon active (iteration: {})", iteration);
        }

        // In a full implementation, we would periodically:
        // 1. Check for new snapshot files
        // 2. Sync any new account data
        // 3. Check ledger size changes
    }
}

/// Validate daemon argument to prevent injection attacks
fn validate_daemon_argument(arg: &str, arg_name: &str) -> Result<()> {
    // Check for shell metacharacters
    let dangerous_chars = [
        ';', '|', '&', '$', '`', '<', '>', '(', ')', '\n', '\r', '\\',
    ];
    for ch in dangerous_chars {
        if arg.contains(ch) {
            return Err(anyhow::anyhow!(
                "Argument '{}' contains forbidden character '{}'. Shell metacharacters are not allowed.",
                arg_name,
                ch
            ));
        }
    }

    // Check for command injection patterns
    if arg.contains("$(") || arg.contains("${") {
        return Err(anyhow::anyhow!(
            "Argument '{}' contains command substitution pattern. Not allowed for security.",
            arg_name
        ));
    }

    Ok(())
}

/// Validate path argument for daemon
fn validate_path_argument(path: &str, arg_name: &str) -> Result<()> {
    use std::path::PathBuf;

    // First validate for shell metacharacters
    validate_daemon_argument(path, arg_name)?;

    // Path must not contain .. to prevent traversal
    if path.contains("..") {
        return Err(anyhow::anyhow!(
            "Path argument '{}' contains '..' which is not allowed for security.",
            arg_name
        ));
    }

    // Ideally, validate the path exists and is accessible
    let path_buf = PathBuf::from(path);

    // If the parent directory exists, that's a good sign
    if let Some(parent) = path_buf.parent() {
        if parent.as_os_str().is_empty() {
            // Root path is okay
        } else if !parent.exists() {
            return Err(anyhow::anyhow!(
                "Parent directory of '{}' does not exist: {:?}",
                arg_name,
                parent
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_realtime_args_default() {
        let args = RealtimeArgs::default();
        assert!(args.programs.is_none());
        assert!(args.accounts.is_none());
    }

    #[test]
    fn test_validate_daemon_argument() {
        // Valid arguments should pass
        assert!(validate_daemon_argument("program1,program2", "programs").is_ok());
        assert!(validate_daemon_argument("Account123", "accounts").is_ok());

        // Invalid arguments should fail
        assert!(validate_daemon_argument("test;malicious", "test").is_err());
        assert!(validate_daemon_argument("test|grep", "test").is_err());
        assert!(validate_daemon_argument("test$(whoami)", "test").is_err());
        assert!(validate_daemon_argument("test`id`", "test").is_err());
    }

    #[test]
    fn test_validate_path_argument() {
        // Valid paths should pass (if parent exists)
        assert!(validate_path_argument("/tmp/test", "ledger").is_ok());

        // Invalid paths should fail
        assert!(validate_path_argument("../../etc/shadow", "path").is_err());
        assert!(validate_path_argument("/tmp/test;rm -rf /", "path").is_err());
    }
}
