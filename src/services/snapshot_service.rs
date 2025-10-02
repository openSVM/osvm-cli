//! Comprehensive snapshot analysis and management service

use anyhow::{anyhow, Context, Result};
use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use solana_sdk::pubkey::Pubkey;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use crate::services::account_decoders::{DecodedAccount, DecoderRegistry, format_decoded_account};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccountInfo {
    pub pubkey: Option<Pubkey>,
    pub lamports: u64,
    pub data_len: u64,
    pub owner: Pubkey,
    pub executable: bool,
    pub rent_epoch: u64,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct OutputConfig {
    pub format: OutputFormat,
    pub colorized: bool,
    pub quiet: bool,
    pub show_progress: bool,
    pub human_readable: bool,
}

#[derive(Debug, Clone)]
pub enum OutputFormat {
    Text,
    Json,
}

#[derive(Debug, Clone, Default)]
pub struct FilterOptions {
    pub owner: Option<String>,
    pub min_balance: Option<u64>,
    pub max_balance: Option<u64>,
    pub min_size: Option<u64>,
    pub max_size: Option<u64>,
    pub executable: Option<bool>,
    pub rent_exempt: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Statistics {
    pub total_accounts: usize,
    pub total_lamports: u64,
    pub average_balance: u64,
    pub median_balance: u64,
    pub total_data_size: u64,
    pub average_data_size: u64,
    pub accounts_by_owner: Vec<(String, usize)>,
    pub largest_accounts: Vec<(String, u64)>,
    pub executable_count: usize,
    pub rent_exempt_count: usize,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExportFormat {
    Json,
    Csv,
    Parquet,
    MessagePack,
}

/// Main snapshot reader with parallel processing support
pub struct SnapshotReader {
    snapshot_dir: PathBuf,
    parallel_threads: Option<usize>,
}

impl SnapshotReader {
    pub fn new(snapshot_dir: PathBuf) -> Result<Self> {
        if !snapshot_dir.exists() {
            return Err(anyhow!(
                "Snapshot directory does not exist: {:?}",
                snapshot_dir
            ));
        }

        Ok(Self {
            snapshot_dir,
            parallel_threads: None,
        })
    }

    pub fn enable_parallel_processing(&mut self, threads: usize) {
        self.parallel_threads = Some(threads);
    }

    pub async fn read_accounts(
        &self,
        output_config: OutputConfig,
        filter_options: FilterOptions,
        limit: Option<usize>,
        offset: usize,
    ) -> Result<()> {
        let accounts_dir = self.snapshot_dir.join("accounts");
        if !accounts_dir.exists() {
            return Err(anyhow!("Accounts directory not found in snapshot"));
        }

        // Read all account files
        let mut entries: Vec<_> = fs::read_dir(&accounts_dir)?
            .filter_map(|e| e.ok())
            .collect();

        entries.sort_by_key(|e| e.file_name());

        let total_accounts = entries.len();
        let entries_to_process: Vec<_> = entries
            .into_iter()
            .skip(offset)
            .take(limit.unwrap_or(usize::MAX))
            .collect();

        if !output_config.quiet {
            println!("=== Solana Snapshot Account Stream ===");
            println!("Snapshot Directory: {:?}", self.snapshot_dir);
            println!("Total accounts: {}", total_accounts);
            println!("Processing: {} accounts", entries_to_process.len());
            println!("{}", "=".repeat(80));
            println!();
        }

        let progress = if output_config.show_progress {
            let pb = ProgressBar::new(entries_to_process.len() as u64);
            pb.set_style(
                ProgressStyle::default_bar()
                    .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos}/{len} {msg}")
                    .unwrap()
                    .progress_chars("=>-"),
            );
            Some(pb)
        } else {
            None
        };

        let results = Arc::new(Mutex::new(Vec::new()));

        if let Some(threads) = self.parallel_threads {
            // Parallel processing
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(threads)
                .build()?;

            pool.install(|| {
                entries_to_process
                    .par_iter()
                    .for_each(|entry| {
                        let path = entry.path();
                        match read_account_file(&path) {
                            Ok(info) => {
                                if filter_matches(&info, &filter_options) {
                                    results.lock().unwrap().push((entry.file_name(), info));
                                }
                            }
                            Err(e) => {
                                eprintln!("Error reading {:?}: {}", path, e);
                            }
                        }
                        if let Some(ref pb) = progress {
                            pb.inc(1);
                        }
                    });
            });
        } else {
            // Sequential processing
            for entry in entries_to_process {
                let path = entry.path();
                match read_account_file(&path) {
                    Ok(info) => {
                        if filter_matches(&info, &filter_options) {
                            results.lock().unwrap().push((entry.file_name(), info));
                        }
                    }
                    Err(e) => {
                        eprintln!("Error reading {:?}: {}", path, e);
                    }
                }
                if let Some(ref pb) = progress {
                    pb.inc(1);
                }
            }
        }

        if let Some(pb) = progress {
            pb.finish_with_message("Complete");
        }

        // Display results
        let results = results.lock().unwrap();
        for (idx, (filename, info)) in results.iter().enumerate() {
            display_account(
                idx + 1,
                &filename.to_string_lossy(),
                info,
                &output_config,
            )?;
        }

        if !output_config.quiet {
            println!("{}", "=".repeat(80));
            println!("Processed {} accounts", results.len());
        }

        Ok(())
    }
}

/// Snapshot service for advanced operations
pub struct SnapshotService {
    pub snapshot_dir: PathBuf,
}

impl SnapshotService {
    pub fn new(snapshot_dir: PathBuf) -> Result<Self> {
        if !snapshot_dir.exists() {
            return Err(anyhow!(
                "Snapshot directory does not exist: {:?}",
                snapshot_dir
            ));
        }

        Ok(Self { snapshot_dir })
    }

    pub async fn compute_statistics(&self) -> Result<Statistics> {
        let accounts_dir = self.snapshot_dir.join("accounts");
        let entries: Vec<_> = fs::read_dir(&accounts_dir)?
            .filter_map(|e| e.ok())
            .collect();

        let mut total_lamports = 0u64;
        let mut total_data_size = 0u64;
        let mut balances = Vec::new();
        let mut owner_counts: HashMap<String, usize> = HashMap::new();
        let mut executable_count = 0;
        let mut rent_exempt_count = 0;
        let mut account_balances: Vec<(String, u64)> = Vec::new();

        for entry in &entries {
            if let Ok(info) = read_account_file(&entry.path()) {
                total_lamports += info.lamports;
                total_data_size += info.data_len;
                balances.push(info.lamports);

                let owner_str = info.owner.to_string();
                *owner_counts.entry(owner_str).or_insert(0) += 1;

                if info.executable {
                    executable_count += 1;
                }

                // Approximate rent exemption (> 2 years rent)
                if info.rent_epoch == u64::MAX {
                    rent_exempt_count += 1;
                }

                if let Some(pubkey) = info.pubkey {
                    account_balances.push((pubkey.to_string(), info.lamports));
                }
            }
        }

        balances.sort_unstable();
        let median_balance = if !balances.is_empty() {
            balances[balances.len() / 2]
        } else {
            0
        };

        let average_balance = if !entries.is_empty() {
            total_lamports / entries.len() as u64
        } else {
            0
        };

        let average_data_size = if !entries.is_empty() {
            total_data_size / entries.len() as u64
        } else {
            0
        };

        let mut accounts_by_owner: Vec<_> = owner_counts.into_iter().collect();
        accounts_by_owner.sort_by(|a, b| b.1.cmp(&a.1));

        account_balances.sort_by(|a, b| b.1.cmp(&a.1));
        let largest_accounts = account_balances.into_iter().take(10).collect();

        Ok(Statistics {
            total_accounts: entries.len(),
            total_lamports,
            average_balance,
            median_balance,
            total_data_size,
            average_data_size,
            accounts_by_owner,
            largest_accounts,
            executable_count,
            rent_exempt_count,
        })
    }

    pub async fn export(
        &self,
        output_file: PathBuf,
        format: ExportFormat,
        filter_options: FilterOptions,
    ) -> Result<()> {
        let accounts_dir = self.snapshot_dir.join("accounts");
        let entries: Vec<_> = fs::read_dir(&accounts_dir)?
            .filter_map(|e| e.ok())
            .collect();

        let mut accounts = Vec::new();
        for entry in entries {
            if let Ok(info) = read_account_file(&entry.path()) {
                if filter_matches(&info, &filter_options) {
                    accounts.push(info);
                }
            }
        }

        match format {
            ExportFormat::Json => {
                let json = serde_json::to_string_pretty(&accounts)?;
                fs::write(output_file, json)?;
            }
            ExportFormat::Csv => {
                let mut wtr = csv::Writer::from_path(output_file)?;
                wtr.write_record(&["pubkey", "lamports", "data_len", "owner", "executable", "rent_epoch"])?;
                for account in accounts {
                    wtr.write_record(&[
                        account.pubkey.map(|p| p.to_string()).unwrap_or_default(),
                        account.lamports.to_string(),
                        account.data_len.to_string(),
                        account.owner.to_string(),
                        account.executable.to_string(),
                        account.rent_epoch.to_string(),
                    ])?;
                }
                wtr.flush()?;
            }
            ExportFormat::Parquet => {
                return Err(anyhow!("Parquet export not yet implemented"));
            }
            ExportFormat::MessagePack => {
                let bytes = rmp_serde::to_vec(&accounts)?;
                fs::write(output_file, bytes)?;
            }
        }

        Ok(())
    }

    pub async fn compare_with(&self, other_snapshot: &Path) -> Result<serde_json::Value> {
        let accounts_dir1 = self.snapshot_dir.join("accounts");
        let accounts_dir2 = other_snapshot.join("accounts");

        if !accounts_dir2.exists() {
            return Err(anyhow!("Second snapshot accounts directory not found"));
        }

        // Read accounts from both snapshots
        let entries1: Vec<_> = fs::read_dir(&accounts_dir1)?
            .filter_map(|e| e.ok())
            .collect();
        let entries2: Vec<_> = fs::read_dir(&accounts_dir2)?
            .filter_map(|e| e.ok())
            .collect();

        let mut accounts1: HashMap<String, AccountInfo> = HashMap::new();
        let mut accounts2: HashMap<String, AccountInfo> = HashMap::new();

        // Load first snapshot accounts
        for entry in entries1 {
            if let Ok(info) = read_account_file(&entry.path()) {
                let key = entry.file_name().to_string_lossy().to_string();
                accounts1.insert(key, info);
            }
        }

        // Load second snapshot accounts
        for entry in entries2 {
            if let Ok(info) = read_account_file(&entry.path()) {
                let key = entry.file_name().to_string_lossy().to_string();
                accounts2.insert(key, info);
            }
        }

        // Compare snapshots
        let mut new_accounts = Vec::new();
        let mut deleted_accounts = Vec::new();
        let mut modified_accounts = Vec::new();

        // Find new and modified accounts
        for (key, info2) in &accounts2 {
            if let Some(info1) = accounts1.get(key) {
                // Account exists in both - check if modified
                if info1.lamports != info2.lamports 
                    || info1.data_len != info2.data_len 
                    || info1.owner != info2.owner {
                    modified_accounts.push(serde_json::json!({
                        "account": key,
                        "changes": {
                            "lamports": {
                                "old": info1.lamports,
                                "new": info2.lamports,
                                "diff": info2.lamports as i128 - info1.lamports as i128
                            },
                            "data_len": {
                                "old": info1.data_len,
                                "new": info2.data_len,
                                "diff": info2.data_len as i128 - info1.data_len as i128
                            },
                            "owner_changed": info1.owner != info2.owner
                        }
                    }));
                }
            } else {
                // New account in snapshot2
                new_accounts.push(serde_json::json!({
                    "account": key,
                    "lamports": info2.lamports,
                    "data_len": info2.data_len,
                    "owner": info2.owner.to_string()
                }));
            }
        }

        // Find deleted accounts
        for (key, info1) in &accounts1 {
            if !accounts2.contains_key(key) {
                deleted_accounts.push(serde_json::json!({
                    "account": key,
                    "lamports": info1.lamports,
                    "data_len": info1.data_len,
                    "owner": info1.owner.to_string()
                }));
            }
        }

        Ok(serde_json::json!({
            "snapshot1": self.snapshot_dir.to_string_lossy(),
            "snapshot2": other_snapshot.to_string_lossy(),
            "summary": {
                "total_accounts_snapshot1": accounts1.len(),
                "total_accounts_snapshot2": accounts2.len(),
                "new_accounts": new_accounts.len(),
                "deleted_accounts": deleted_accounts.len(),
                "modified_accounts": modified_accounts.len(),
                "unchanged_accounts": accounts1.len() - deleted_accounts.len() - modified_accounts.len()
            },
            "new_accounts": new_accounts,
            "deleted_accounts": deleted_accounts,
            "modified_accounts": modified_accounts
        }))
    }

    pub async fn validate(&self) -> Result<serde_json::Value> {
        let accounts_dir = self.snapshot_dir.join("accounts");
        
        if !accounts_dir.exists() {
            return Ok(serde_json::json!({
                "valid": false,
                "errors": ["Accounts directory not found"],
                "warnings": [],
                "info": []
            }));
        }

        let entries: Vec<_> = fs::read_dir(&accounts_dir)?
            .filter_map(|e| e.ok())
            .collect();

        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let mut info = Vec::new();

        let mut successful_reads = 0;
        let mut failed_reads = 0;
        let mut total_lamports = 0u64;
        let mut suspicious_balances = 0;
        let mut zero_balance_accounts = 0;

        for entry in &entries {
            match read_account_file(&entry.path()) {
                Ok(account_info) => {
                    successful_reads += 1;
                    total_lamports += account_info.lamports;

                    // Check for suspicious patterns
                    if account_info.lamports == 0 && account_info.data_len > 0 {
                        zero_balance_accounts += 1;
                    }

                    // Check for extremely large balances (potential corruption)
                    if account_info.lamports > 1_000_000_000_000_000_000 {
                        suspicious_balances += 1;
                        warnings.push(format!(
                            "Account {} has suspiciously large balance: {} lamports",
                            entry.file_name().to_string_lossy(),
                            account_info.lamports
                        ));
                    }

                    // Validate data length consistency
                    if account_info.data_len as usize != account_info.data.len() {
                        warnings.push(format!(
                            "Account {}: data_len mismatch (expected: {}, actual: {})",
                            entry.file_name().to_string_lossy(),
                            account_info.data_len,
                            account_info.data.len()
                        ));
                    }
                }
                Err(e) => {
                    failed_reads += 1;
                    errors.push(format!(
                        "Failed to read {}: {}",
                        entry.file_name().to_string_lossy(),
                        e
                    ));
                }
            }
        }

        // Add informational messages
        info.push(format!("Total account files: {}", entries.len()));
        info.push(format!("Successfully parsed: {}", successful_reads));
        info.push(format!("Failed to parse: {}", failed_reads));
        info.push(format!("Total lamports: {}", total_lamports));
        info.push(format!("Zero-balance accounts with data: {}", zero_balance_accounts));

        if suspicious_balances > 0 {
            warnings.push(format!("Found {} accounts with suspiciously large balances", suspicious_balances));
        }

        // Validate snapshot structure
        if !self.snapshot_dir.join("version").exists() {
            warnings.push("Version file not found".to_string());
        }

        if !self.snapshot_dir.join("snapshots").exists() {
            warnings.push("Snapshots directory not found".to_string());
        }

        let is_valid = errors.is_empty() && successful_reads > 0;

        Ok(serde_json::json!({
            "valid": is_valid,
            "summary": {
                "total_files": entries.len(),
                "successful_reads": successful_reads,
                "failed_reads": failed_reads,
                "total_lamports": total_lamports,
                "zero_balance_with_data": zero_balance_accounts,
                "suspicious_balances": suspicious_balances
            },
            "errors": errors,
            "warnings": warnings,
            "info": info
        }))
    }

    pub async fn find_account(&self, pubkey: &str) -> Result<serde_json::Value> {
        let accounts_dir = self.snapshot_dir.join("accounts");
        let entries: Vec<_> = fs::read_dir(&accounts_dir)?
            .filter_map(|e| e.ok())
            .collect();

        let total_files = entries.len();

        // Search through all accounts
        for entry in entries {
            if let Ok(info) = read_account_file(&entry.path()) {
                // Check if this is the account we're looking for
                if let Some(account_pubkey) = info.pubkey {
                    if account_pubkey.to_string() == pubkey {
                        return Ok(serde_json::json!({
                            "found": true,
                            "file": entry.file_name().to_string_lossy(),
                            "account": {
                                "pubkey": account_pubkey.to_string(),
                                "lamports": info.lamports,
                                "data_len": info.data_len,
                                "owner": info.owner.to_string(),
                                "executable": info.executable,
                                "rent_epoch": info.rent_epoch,
                                "data_preview": format!("{:?}", &info.data[..info.data.len().min(64)])
                            }
                        }));
                    }
                }
            }
        }

        Ok(serde_json::json!({
            "found": false,
            "message": format!("Account {} not found in snapshot", pubkey),
            "searched_files": total_files
        }))
    }
}

/// Interactive TUI for snapshot exploration
pub struct SnapshotTui {}

impl SnapshotTui {
    pub fn new() -> Result<Self> {
        Ok(Self {})
    }

    pub async fn run(&mut self) -> Result<()> {
        println!("Interactive TUI mode coming soon!");
        println!("This will feature:");
        println!("  - Arrow key navigation");
        println!("  - Real-time filtering");
        println!("  - Account inspection");
        println!("  - Search functionality");
        Ok(())
    }
}

// Helper functions

fn read_account_file(path: &Path) -> Result<AccountInfo> {
    let mut file = fs::File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    if buffer.len() < 100 {
        return Err(anyhow!("File too small to be a valid account"));
    }

    // Parse the account storage format
    // This is a simplified parser - in production should use solana-runtime's AppendVec
    let data_len = u64::from_le_bytes(buffer[8..16].try_into()?);
    let owner_bytes: [u8; 32] = buffer[17..49].try_into()?;
    let owner = Pubkey::new_from_array(owner_bytes);
    let lamports = u64::from_le_bytes(buffer[49..57].try_into()?);
    let rent_epoch = u64::from_le_bytes(buffer[57..65].try_into()?);

    // Try to extract pubkey if available
    let pubkey = if buffer.len() > 81 {
        if let Ok(pubkey_bytes) = buffer[65..97].try_into() {
            Some(Pubkey::new_from_array(pubkey_bytes))
        } else {
            None
        }
    } else {
        None
    };

    let executable = false; // Would need proper parsing

    let data_start = 130;
    let data = if buffer.len() > data_start {
        buffer[data_start..].to_vec()
    } else {
        Vec::new()
    };

    Ok(AccountInfo {
        pubkey,
        lamports,
        data_len,
        owner,
        executable,
        rent_epoch,
        data,
    })
}

fn filter_matches(account: &AccountInfo, filter: &FilterOptions) -> bool {
    if let Some(ref owner_filter) = filter.owner {
        let owner_str = account.owner.to_string();
        if !owner_str.contains(owner_filter) {
            return false;
        }
    }

    if let Some(min) = filter.min_balance {
        if account.lamports < min {
            return false;
        }
    }

    if let Some(max) = filter.max_balance {
        if account.lamports > max {
            return false;
        }
    }

    if let Some(min) = filter.min_size {
        if account.data_len < min {
            return false;
        }
    }

    if let Some(max) = filter.max_size {
        if account.data_len > max {
            return false;
        }
    }

    if let Some(exec) = filter.executable {
        if account.executable != exec {
            return false;
        }
    }

    if let Some(rent_exempt) = filter.rent_exempt {
        let is_rent_exempt = account.rent_epoch == u64::MAX;
        if is_rent_exempt != rent_exempt {
            return false;
        }
    }

    true
}

fn display_account(
    index: usize,
    filename: &str,
    account: &AccountInfo,
    config: &OutputConfig,
) -> Result<()> {
    if config.format == OutputFormat::Json {
        println!("{}", serde_json::to_string_pretty(account)?);
        return Ok(());
    }

    // Try to decode account data
    let decoder_registry = DecoderRegistry::new();
    let decoded = decoder_registry.decode(&account.owner, &account.data).ok();
    let program_name = decoder_registry.get_program_name(&account.owner);

    if config.colorized {
        println!("{} ({})", format!("Account #{}", index).bold().cyan(), filename.bright_black());
        
        // Show program type if known
        if let Some(name) = program_name {
            println!("  {}: {}", "Type".bold(), name.bright_magenta());
        }
        
        println!("  {}: {}", "Lamports".bold(), format_lamports(account.lamports).green());
        println!("  {}: {} bytes", "Data Length".bold(), account.data_len);
        println!("  {}: {}", "Owner".bold(), account.owner.to_string().bright_blue());
        println!("  {}: {}", "Executable".bold(), account.executable);
        println!("  {}: {}", "Rent Epoch".bold(), account.rent_epoch);
        
        // Display decoded data if available
        if let Some(DecodedAccount::Unknown) = decoded {
            // Don't show anything for unknown types
        } else if let Some(ref decoded_data) = decoded {
            let formatted = format_decoded_account(decoded_data);
            if !formatted.is_empty() {
                println!();
                println!("  {}", "Decoded Data:".bold().yellow());
                for line in formatted.lines() {
                    println!("  {}", line);
            }
            }
        }
    } else {
        println!("Account #{} ({})", index, filename);
        
        if let Some(name) = program_name {
            println!("  Type: {}", name);
        }
        
        println!("  Lamports: {}", account.lamports);
        println!("  Data Length: {} bytes", account.data_len);
        println!("  Owner: {}", account.owner);
        println!("  Executable: {}", account.executable);
        println!("  Rent Epoch: {}", account.rent_epoch);
        
        // Display decoded data
        if let Some(DecodedAccount::Unknown) = decoded {
            // Don't show anything for unknown types
        } else if let Some(ref decoded_data) = decoded {
            let formatted = format_decoded_account(decoded_data);
            if !formatted.is_empty() {
                println!();
                println!("  Decoded Data:");
                for line in formatted.lines() {
                    println!("  {}", line);
                }
            }
        }
    }

    if !account.data.is_empty() && !config.quiet {
        println!("  Data (first {} bytes):", account.data.len().min(128));
        display_hex_dump(&account.data[..account.data.len().min(128)]);
        if account.data.len() > 128 {
            println!("    ... ({} more bytes)", account.data.len() - 128);
        }
    }

    println!();
    Ok(())
}

fn display_hex_dump(data: &[u8]) {
    for (i, chunk) in data.chunks(16).enumerate() {
        print!("    {:04x}: ", i * 16);
        for byte in chunk {
            print!("{:02x} ", byte);
        }
        for _ in 0..(16 - chunk.len()) {
            print!("   ");
        }
        print!(" |");
        for byte in chunk {
            let c = if byte.is_ascii_graphic() || *byte == b' ' {
                *byte as char
            } else {
                '.'
            };
            print!("{}", c);
        }
        println!("|");
    }
}

fn format_lamports(lamports: u64) -> String {
    if lamports >= 1_000_000_000 {
        format!("{:.4} SOL", lamports as f64 / 1_000_000_000.0)
    } else {
        format!("{} lamports", lamports)
    }
}

impl PartialEq for OutputFormat {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (OutputFormat::Text, OutputFormat::Text) | (OutputFormat::Json, OutputFormat::Json)
        )
    }
}
