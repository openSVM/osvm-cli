//! Database management commands

use anyhow::{Context, Result};
use colored::Colorize;
use std::sync::Arc;

use crate::services::{
    activity_logger::ActivityLogger,
    clickhouse_service::{ClickHouseService, ClickHouseStatus},
};

/// Execute database command
pub async fn execute_database_command(subcommand: &str, args: &DatabaseArgs) -> Result<()> {
    match subcommand {
        "init" => cmd_init(args).await,
        "start" => cmd_start(args).await,
        "stop" => cmd_stop(args).await,
        "status" => cmd_status(args).await,
        "query" => cmd_query(args).await,
        "activity" => cmd_activity(args).await,
        "sync" => cmd_sync(args).await,
        _ => Err(anyhow::anyhow!(
            "Unknown database subcommand: {}",
            subcommand
        )),
    }
}

/// Database command arguments
#[derive(Debug, Clone)]
pub struct DatabaseArgs {
    pub data_dir: Option<String>,
    pub query: Option<String>,
    pub limit: usize,
    pub session_id: Option<String>,
    pub show_commands: bool,
    pub show_chat: bool,
    pub show_stats: bool,
    // Sync-specific fields
    pub sync_mode: Option<String>,
    pub programs: Option<String>,
    pub accounts: Option<String>,
    pub pattern: Option<String>,
    pub ledger_path: Option<String>,
    pub snapshot_dir: Option<String>,
}

impl Default for DatabaseArgs {
    fn default() -> Self {
        Self {
            data_dir: None,
            query: None,
            limit: 100,
            session_id: None,
            show_commands: false,
            show_chat: false,
            show_stats: false,
            sync_mode: None,
            programs: None,
            accounts: None,
            pattern: None,
            ledger_path: None,
            snapshot_dir: None,
        }
    }
}

/// Initialize ClickHouse database
async fn cmd_init(args: &DatabaseArgs) -> Result<()> {
    println!("{}", "Initializing ClickHouse database...".cyan().bold());

    let service = if let Some(data_dir) = &args.data_dir {
        let config = crate::services::clickhouse_service::ClickHouseConfig {
            data_dir: std::path::PathBuf::from(data_dir),
            ..Default::default()
        };
        ClickHouseService::with_config(config)?
    } else {
        ClickHouseService::new()?
    };

    service
        .init()
        .await
        .context("Failed to initialize ClickHouse")?;

    println!("{}", "✓ ClickHouse initialized successfully".green().bold());
    println!();
    println!("Next steps:");
    println!("  1. Start the database: {}", "osvm db start".yellow());
    println!("  2. Check status: {}", "osvm db status".yellow());
    println!("  3. Query data: {}", "osvm db query \"SELECT 1\"".yellow());

    Ok(())
}

/// Start ClickHouse server
async fn cmd_start(_args: &DatabaseArgs) -> Result<()> {
    println!("{}", "Starting ClickHouse server...".cyan().bold());

    let service = ClickHouseService::new()?;
    service
        .start()
        .await
        .context("Failed to start ClickHouse server")?;

    println!(
        "{}",
        "✓ ClickHouse server started successfully".green().bold()
    );
    println!();
    println!("Server is running on:");
    println!("  HTTP: {}", "http://localhost:8123".yellow());
    println!("  TCP: {}", "localhost:9000".yellow());

    Ok(())
}

/// Stop ClickHouse server
async fn cmd_stop(_args: &DatabaseArgs) -> Result<()> {
    println!("{}", "Stopping ClickHouse server...".cyan().bold());

    let service = ClickHouseService::new()?;
    service
        .stop()
        .await
        .context("Failed to stop ClickHouse server")?;

    println!(
        "{}",
        "✓ ClickHouse server stopped successfully".green().bold()
    );

    Ok(())
}

/// Check ClickHouse server status
async fn cmd_status(_args: &DatabaseArgs) -> Result<()> {
    let service = ClickHouseService::new()?;
    let status = service.status().await?;

    match status {
        ClickHouseStatus::Running => {
            println!("{}", "● ClickHouse server is running".green().bold());
            println!();
            println!("Connection details:");
            println!("  HTTP endpoint: {}", "http://localhost:8123".yellow());
            println!("  TCP endpoint: {}", "localhost:9000".yellow());
            println!("  Database: {}", "osvm".yellow());
        }
        ClickHouseStatus::Stopped => {
            println!("{}", "● ClickHouse server is stopped".red().bold());
            println!();
            println!("To start the server, run: {}", "osvm db start".yellow());
        }
        ClickHouseStatus::Error(msg) => {
            println!(
                "{}",
                format!("● ClickHouse server error: {}", msg).red().bold()
            );
            println!();
            println!(
                "Try restarting: {}",
                "osvm db stop && osvm db start".yellow()
            );
        }
    }

    Ok(())
}

/// Execute a SQL query
async fn cmd_query(args: &DatabaseArgs) -> Result<()> {
    let service = ClickHouseService::new()?;

    // Check if server is running
    let status = service.status().await?;
    if status != ClickHouseStatus::Running {
        return Err(anyhow::anyhow!(
            "ClickHouse server is not running. Start it with: osvm db start"
        ));
    }

    let query = args
        .query
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("No query provided. Use --query \"SELECT ...\""))?;

    // SECURITY: Validate query to prevent SQL injection
    validate_sql_query(query)?;

    println!("{}", format!("Executing query: {}", query).cyan());
    println!();

    let result = service
        .query_json(query)
        .await
        .context("Failed to execute query")?;

    println!("{}", result);

    Ok(())
}

/// Validate SQL query to prevent injection attacks
fn validate_sql_query(query: &str) -> Result<()> {
    let query_upper = query.trim().to_uppercase();

    // Must start with SELECT
    if !query_upper.starts_with("SELECT") {
        anyhow::bail!(
            "Only SELECT queries are allowed. Other operations are forbidden for security."
        );
    }

    // Reject dangerous keywords
    let dangerous_keywords = [
        "DROP", "DELETE", "UPDATE", "ALTER", "INSERT", "CREATE", "TRUNCATE", "RENAME", "GRANT",
        "REVOKE", "EXEC", "EXECUTE",
    ];

    for keyword in dangerous_keywords {
        if query_upper.contains(keyword) {
            anyhow::bail!(
                "Query contains forbidden keyword '{}'. Only SELECT queries are allowed.",
                keyword
            );
        }
    }

    // Reject stacked queries (semicolon anywhere except in quoted strings would be complex to parse)
    // Simple check: reject if semicolon found (may have false positives but safer)
    if query.contains(';') {
        anyhow::bail!("Multiple statements (semicolons) are not allowed for security reasons.");
    }

    // Reject UNION attacks
    if query_upper.contains("UNION") {
        anyhow::bail!("UNION queries are not allowed for security reasons.");
    }

    // Whitelist allowed tables (osvm database only)
    let allowed_tables = [
        "osvm.cli_commands",
        "osvm.blockchain_accounts",
        "osvm.command_activity",
        "osvm.chat_history",
    ];
    let has_allowed_table = allowed_tables
        .iter()
        .any(|table| query_upper.contains(&table.to_uppercase()));

    if !has_allowed_table {
        anyhow::bail!(
            "Query must reference allowed tables: {}. Access to other tables is forbidden.",
            allowed_tables.join(", ")
        );
    }

    // Reject system table access
    if query_upper.contains("SYSTEM.") {
        anyhow::bail!("Access to system tables is forbidden.");
    }

    Ok(())
}

/// Show activity logs
async fn cmd_activity(args: &DatabaseArgs) -> Result<()> {
    let service = Arc::new(ClickHouseService::new()?);

    // Check if server is running
    let status = service.status().await?;
    if status != ClickHouseStatus::Running {
        return Err(anyhow::anyhow!(
            "ClickHouse server is not running. Start it with: osvm db start"
        ));
    }

    let logger = ActivityLogger::new(service);

    // Show statistics if requested
    if args.show_stats {
        println!("{}", "Activity Statistics".cyan().bold());
        println!();

        let stats = logger
            .get_activity_stats()
            .await
            .context("Failed to fetch activity statistics")?;
        println!("{}", stats);
        println!();
    }

    // Show command history if requested
    if args.show_commands {
        println!("{}", "CLI Command History".cyan().bold());
        println!();

        let history = logger
            .query_command_history(args.limit, args.session_id.as_deref())
            .await
            .context("Failed to fetch command history")?;
        println!("{}", history);
        println!();
    }

    // Show chat history if requested
    if args.show_chat {
        println!("{}", "Chat Message History".cyan().bold());
        println!();

        let history = logger
            .query_chat_history(args.limit, args.session_id.as_deref())
            .await
            .context("Failed to fetch chat history")?;
        println!("{}", history);
        println!();
    }

    // If nothing specific was requested, show summary
    if !args.show_stats && !args.show_commands && !args.show_chat {
        println!("{}", "Activity Summary".cyan().bold());
        println!();
        println!("Use the following flags to view specific data:");
        println!("  {} - Show activity statistics", "--stats".yellow());
        println!("  {} - Show CLI command history", "--commands".yellow());
        println!("  {} - Show chat message history", "--chat".yellow());
        println!();
        println!("Options:");
        println!(
            "  {} <N> - Limit results to N entries (default: 100)",
            "--limit".yellow()
        );
        println!("  {} <ID> - Filter by session ID", "--session-id".yellow());
        println!();
        println!("Examples:");
        println!("  osvm db activity --stats");
        println!("  osvm db activity --commands --limit 50");
        println!("  osvm db activity --chat --session-id abc123");
    }

    Ok(())
}

/// Sync blockchain data to ClickHouse
async fn cmd_sync(args: &DatabaseArgs) -> Result<()> {
    use crate::services::blockchain_indexer::{
        parse_sync_args, BlockchainIndexer, IndexingMode, SyncArgs,
    };

    let service = Arc::new(ClickHouseService::new()?);

    // Check if server is running
    let status = service.status().await?;
    if status != ClickHouseStatus::Running {
        return Err(anyhow::anyhow!(
            "ClickHouse server is not running. Start it with: osvm db start"
        ));
    }

    println!(
        "{}",
        "Syncing blockchain data to ClickHouse...".cyan().bold()
    );
    println!();

    // Parse sync mode
    let mode = match args.sync_mode.as_deref().unwrap_or("last-30-days") {
        "last-30-days" => IndexingMode::LastNDays(30),
        "full-historical" => IndexingMode::FullHistorical,
        "realtime" => IndexingMode::RealtimeOnly,
        _ => IndexingMode::LastNDays(30),
    };

    // Build sync arguments
    let sync_args = SyncArgs {
        mode,
        programs: args
            .programs
            .as_ref()
            .map(|p| p.split(',').map(|s| s.trim().to_string()).collect())
            .unwrap_or_default(),
        accounts: args
            .accounts
            .as_ref()
            .map(|a| a.split(',').map(|s| s.trim().to_string()).collect())
            .unwrap_or_default(),
        patterns: args
            .pattern
            .as_ref()
            .map(|p| vec![p.clone()])
            .unwrap_or_default(),
        ledger_path: args.ledger_path.clone(),
        snapshot_path: args.snapshot_dir.clone(),
    };

    // Parse into indexing config
    let config = parse_sync_args(&sync_args)?;

    // Create indexer (clone config since we need it later)
    let indexer = BlockchainIndexer::new(
        args.ledger_path.clone(),
        args.snapshot_dir.clone(),
        service,
        config.clone(),
    )?;

    // Sync accounts from snapshot
    println!("📊 Syncing account data from snapshot...");
    match indexer.sync_accounts(0).await {
        Ok(count) => {
            println!(
                "{}",
                format!("✓ Indexed {} accounts successfully", count)
                    .green()
                    .bold()
            );
        }
        Err(e) => {
            eprintln!("{}", format!("❌ Account sync failed: {}", e).red());
            return Err(e);
        }
    }

    println!();

    // Sync transactions from ledger
    println!("📊 Syncing transaction data from ledger...");

    // Determine slot range based on mode
    let current_slot = 1000000u64; // Placeholder - would query from ledger
    let slot_range = match config.mode {
        IndexingMode::LastNDays(days) => {
            // Calculate slot range for last N days
            // Assuming ~2 slots per second average
            let slots_per_day = 2 * 60 * 60 * 24;
            let start_slot = current_slot.saturating_sub(days as u64 * slots_per_day);
            start_slot..current_slot
        }
        IndexingMode::FullHistorical => 0..u64::MAX,
        IndexingMode::RealtimeOnly => current_slot..u64::MAX,
        _ => 0..current_slot,
    };

    match indexer.sync_transactions(slot_range).await {
        Ok(count) => {
            println!(
                "{}",
                format!("✓ Indexed {} transactions successfully", count)
                    .green()
                    .bold()
            );
        }
        Err(e) => {
            eprintln!("{}", format!("⚠ Transaction sync failed: {}", e).yellow());
            println!("Note: Transaction history requires ledger started with --enable-rpc-transaction-history");
            // Don't return error, continue to show summary
        }
    }

    println!();
    println!("{}", "Sync completed successfully!".green().bold());
    println!();
    println!("Query your data with:");
    println!(
        "  Accounts: {}",
        "osvm db query --query \"SELECT * FROM osvm.blockchain_accounts LIMIT 10\"".yellow()
    );
    println!(
        "  Transactions: {}",
        "osvm db query --query \"SELECT * FROM osvm.blockchain_transactions LIMIT 10\"".yellow()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_database_args_default() {
        let args = DatabaseArgs::default();
        assert_eq!(args.limit, 100);
        assert!(!args.show_stats);
    }
}
