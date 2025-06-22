//! Continuous log monitoring and automatic repair system
//!
//! This module provides real-time log analysis and automatic fixes for various
//! types of issues that can occur during validator operation.

use crate::utils::osvm_logger::{LogCategory, OSVM_LOGGER};
use crate::{osvm_debug, osvm_error, osvm_info, osvm_warn};
use anyhow::{Context, Result};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use tokio::task;

/// Type of issue detected in logs
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IssueType {
    SystemTuning(String), // System parameter needs adjustment
    PortConflict(u16),    // Port is already in use
    DiskSpace,            // Low disk space
    MemoryPressure,       // High memory usage
    NetworkConnectivity,  // Network connectivity issues
    ExternalReachability, // External reachability issues (needs ngrok)
    SnapshotFailure,      // Snapshot download/creation failed
    AccountsDbError,      // AccountsDB corruption or issues
    LedgerCorruption,     // Ledger corruption detected
    RpcTimeout,           // RPC request timeouts
    TooManyOpenFiles,     // File descriptor limit reached
    ValidatorCrash,       // Validator process crashed
    SlowBlockProduction,  // Slow block production
    GossipFailure,        // Gossip protocol failures
    Custom(String),       // Custom issue type
}

/// Severity level of detected issues
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Detected issue with context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedIssue {
    pub issue_type: IssueType,
    pub severity: Severity,
    pub message: String,
    pub context: HashMap<String, String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Fix result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FixResult {
    pub success: bool,
    pub message: String,
    pub requires_restart: bool,
}

/// Log pattern matcher
struct LogPattern {
    regex: Regex,
    issue_type: IssueType,
    severity: Severity,
    extractor: Box<dyn Fn(&regex::Captures) -> HashMap<String, String> + Send + Sync>,
}

/// Log monitor configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogMonitorConfig {
    pub auto_fix_enabled: bool,
    pub restart_on_critical: bool,
    pub max_restart_attempts: u32,
    pub restart_cooldown_seconds: u64,
}

impl Default for LogMonitorConfig {
    fn default() -> Self {
        Self {
            auto_fix_enabled: true,
            restart_on_critical: true,
            max_restart_attempts: 3,
            restart_cooldown_seconds: 300,
        }
    }
}

/// Log monitor state
struct MonitorState {
    issues_fixed: HashMap<IssueType, u32>,
    restart_count: u32,
    last_restart: Option<chrono::DateTime<chrono::Utc>>,
}

/// Initialize log patterns for issue detection
fn init_log_patterns() -> Vec<LogPattern> {
    vec![
        // System tuning issues
        LogPattern {
            regex: Regex::new(r"(\S+):\s*recommended=(\d+),\s*current=(\d+)\s*too small").unwrap(),
            issue_type: IssueType::SystemTuning("".to_string()),
            severity: Severity::Warning,
            extractor: Box::new(|caps| {
                let mut ctx = HashMap::new();
                ctx.insert("parameter".to_string(), caps[1].to_string());
                ctx.insert("recommended".to_string(), caps[2].to_string());
                ctx.insert("current".to_string(), caps[3].to_string());
                ctx
            }),
        },
        // Port conflict
        LogPattern {
            regex: Regex::new(r"bind\(\):\s*Address already in use.*:(\d+)").unwrap(),
            issue_type: IssueType::PortConflict(0),
            severity: Severity::Error,
            extractor: Box::new(|caps| {
                let mut ctx = HashMap::new();
                ctx.insert("port".to_string(), caps[1].to_string());
                ctx
            }),
        },
        // Disk space
        LogPattern {
            regex: Regex::new(r"No space left on device|disk.*full|insufficient.*space").unwrap(),
            issue_type: IssueType::DiskSpace,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Memory pressure
        LogPattern {
            regex: Regex::new(r"Out of memory|OOM|memory exhausted|Cannot allocate memory").unwrap(),
            issue_type: IssueType::MemoryPressure,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Too many open files
        LogPattern {
            regex: Regex::new(r"Too many open files|EMFILE|ulimit.*reached").unwrap(),
            issue_type: IssueType::TooManyOpenFiles,
            severity: Severity::Error,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Local network connectivity
        LogPattern {
            regex: Regex::new(r"Connection refused|Network unreachable|No route to host|Connection timed out").unwrap(),
            issue_type: IssueType::NetworkConnectivity,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // External reachability (needs ngrok)
        LogPattern {
            regex: Regex::new(r"Received no response at tcp/(\d+).*check your port configuration.*timed out.*receive operation").unwrap(),
            issue_type: IssueType::ExternalReachability,
            severity: Severity::Critical,
            extractor: Box::new(|caps| {
                let mut ctx = HashMap::new();
                ctx.insert("port".to_string(), caps[1].to_string());
                ctx
            }),
        },
        // Snapshot failures
        LogPattern {
            regex: Regex::new(r"Snapshot download failed|Failed to download snapshot|snapshot.*error").unwrap(),
            issue_type: IssueType::SnapshotFailure,
            severity: Severity::Error,
            extractor: Box::new(|_| HashMap::new()),
        },
        // AccountsDB errors
        LogPattern {
            regex: Regex::new(r"AccountsDb.*error|accounts.*corrupt|Failed to load accounts").unwrap(),
            issue_type: IssueType::AccountsDbError,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Ledger corruption
        LogPattern {
            regex: Regex::new(r"Ledger corruption|blockstore.*corrupt|Invalid ledger").unwrap(),
            issue_type: IssueType::LedgerCorruption,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // RPC timeouts
        LogPattern {
            regex: Regex::new(r"RPC request timeout|jsonrpc.*timeout|Request timed out").unwrap(),
            issue_type: IssueType::RpcTimeout,
            severity: Severity::Warning,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Validator crash
        LogPattern {
            regex: Regex::new(r"panic|SIGKILL|SIGSEGV|Aborted|thread.*panicked").unwrap(),
            issue_type: IssueType::ValidatorCrash,
            severity: Severity::Critical,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Slow block production
        LogPattern {
            regex: Regex::new(r"Slow block|Block production.*slow|Leader slot.*skipped").unwrap(),
            issue_type: IssueType::SlowBlockProduction,
            severity: Severity::Warning,
            extractor: Box::new(|_| HashMap::new()),
        },
        // Gossip failures
        LogPattern {
            regex: Regex::new(r"Gossip.*failed|gossip.*error|Failed to gossip").unwrap(),
            issue_type: IssueType::GossipFailure,
            severity: Severity::Error,
            extractor: Box::new(|_| HashMap::new()),
        },
    ]
}

/// Check if ngrok is installed
fn is_ngrok_installed() -> bool {
    Command::new("which")
        .arg("ngrok")
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

/// Install ngrok automatically
async fn install_ngrok() -> Result<()> {
    println!("🔧 Installing ngrok...");

    // Download and install ngrok
    let output = Command::new("sh")
        .arg("-c")
        .arg("curl -s https://ngrok-agent.s3.amazonaws.com/ngrok.asc | sudo tee /etc/apt/trusted.gpg.d/ngrok.asc >/dev/null && echo 'deb https://ngrok-agent.s3.amazonaws.com buster main' | sudo tee /etc/apt/sources.list.d/ngrok.list && sudo apt update && sudo apt install ngrok")
        .output()
        .context("Failed to install ngrok")?;

    if output.status.success() {
        println!("✅ ngrok installed successfully");
        Ok(())
    } else {
        // Try alternative installation method
        println!("🔧 Trying alternative ngrok installation...");
        let alt_output = Command::new("sh")
            .arg("-c")
            .arg("wget -q https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-stable-linux-amd64.tgz -O /tmp/ngrok.tgz && sudo tar -xzf /tmp/ngrok.tgz -C /usr/local/bin && sudo chmod +x /usr/local/bin/ngrok")
            .output()
            .context("Failed to install ngrok via wget")?;

        if alt_output.status.success() {
            println!("✅ ngrok installed successfully via wget");
            Ok(())
        } else {
            anyhow::bail!(
                "Failed to install ngrok: {}",
                String::from_utf8_lossy(&output.stderr)
            )
        }
    }
}

/// Setup ngrok tunnels for Solana validator ports
async fn setup_ngrok_tunnels(ports: &[u16]) -> Result<HashMap<u16, String>> {
    osvm_info!(
        LogCategory::Ngrok,
        "setup_ngrok_tunnels",
        &format!("Setting up ngrok tunnels for ports: {:?}", ports)
    );
    println!("🌐 Setting up ngrok tunnels for Solana validator ports...");

    let mut tunnel_urls = HashMap::new();

    // Check if ngrok is installed
    if !is_ngrok_installed() {
        osvm_warn!(
            LogCategory::Ngrok,
            "setup_ngrok_tunnels",
            "ngrok not found, installing automatically"
        );
        println!("📦 ngrok not found, installing...");
        install_ngrok().await?;

        // Verify installation
        if !is_ngrok_installed() {
            anyhow::bail!("Failed to install ngrok");
        }
    }

    // Kill any existing ngrok processes
    let _ = Command::new("pkill").arg("-f").arg("ngrok").output();

    // Wait for cleanup
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    // Start ngrok tunnels for each port
    for &port in ports {
        println!("🔗 Creating ngrok tunnel for port {}", port);

        // Use HTTP tunnel with custom domain for RPC ports (8899), TCP for others
        let mut child = if port == 8899 {
            // HTTP tunnel for RPC endpoint with custom domain
            Command::new("ngrok")
                .arg("http")
                .arg(format!("localhost:{}", port))
                .arg("--domain")
                .arg("osvm.dev")
                .arg("--log")
                .arg("stdout")
                .spawn()
                .context("Failed to start ngrok")?
        } else {
            // TCP tunnel with pooling enabled to allow sharing endpoints
            Command::new("ngrok")
                .arg("tcp")
                .arg(port.to_string())
                .arg("--pooling-enabled")
                .arg("--log")
                .arg("stdout")
                .spawn()
                .context("Failed to start ngrok")?
        };

        // Give ngrok time to start
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        // Get tunnel URL from ngrok API
        let api_output = Command::new("curl")
            .arg("-s")
            .arg("http://localhost:4040/api/tunnels")
            .output()
            .context("Failed to query ngrok API")?;

        if api_output.status.success() {
            let api_response = String::from_utf8_lossy(&api_output.stdout);

            // Parse JSON to extract tunnel URL
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&api_response) {
                if let Some(tunnels) = json["tunnels"].as_array() {
                    for tunnel in tunnels {
                        if let Some(config) = tunnel["config"].as_object() {
                            if let Some(addr) = config["addr"].as_str() {
                                if addr.ends_with(&format!(":{}", port)) {
                                    if let Some(public_url) = tunnel["public_url"].as_str() {
                                        tunnel_urls.insert(port, public_url.to_string());
                                        osvm_info!(
                                            LogCategory::Ngrok,
                                            "setup_ngrok_tunnels",
                                            &format!(
                                                "Successfully created tunnel for port {}: {}",
                                                port, public_url
                                            )
                                        );
                                        println!(
                                            "✅ ngrok tunnel for port {}: {}",
                                            port, public_url
                                        );
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // If we couldn't get the URL from API, that's ok, ngrok is still running
        if !tunnel_urls.contains_key(&port) {
            println!(
                "⚠️  Created ngrok tunnel for port {} (URL not retrieved)",
                port
            );
        }
    }

    if !tunnel_urls.is_empty() {
        println!("🎉 ngrok tunnels active - external connectivity should now work!");
        println!("💡 Your Solana validator is now accessible from the internet via ngrok");
    }

    Ok(tunnel_urls)
}

/// Apply fix for detected issue
async fn apply_fix(issue: &DetectedIssue, config: &LogMonitorConfig) -> Result<FixResult> {
    osvm_info!(
        LogCategory::AutoRepair,
        "apply_fix",
        &format!("Attempting to fix issue: {:?}", issue.issue_type)
    );

    if !config.auto_fix_enabled {
        osvm_warn!(
            LogCategory::AutoRepair,
            "apply_fix",
            "Auto-fix is disabled, skipping repair"
        );
        return Ok(FixResult {
            success: false,
            message: "Auto-fix is disabled".to_string(),
            requires_restart: false,
        });
    }

    match &issue.issue_type {
        IssueType::SystemTuning(param) => {
            let param = if param.is_empty() {
                issue.context.get("parameter").cloned().unwrap_or_default()
            } else {
                param.clone()
            };

            let recommended = issue
                .context
                .get("recommended")
                .ok_or_else(|| anyhow::anyhow!("Missing recommended value"))?;

            println!("🔧 Applying system tuning: {} = {}", param, recommended);

            let output = Command::new("sudo")
                .arg("sysctl")
                .arg("-w")
                .arg(format!("{}={}", param, recommended))
                .output()
                .context("Failed to run sysctl")?;

            if output.status.success() {
                // Persist to sysctl.conf
                let _ = Command::new("sh")
                    .arg("-c")
                    .arg(format!(
                        "grep -q '^{}=' /etc/sysctl.conf || echo '{}={}' | sudo tee -a /etc/sysctl.conf > /dev/null",
                        param, param, recommended
                    ))
                    .output();

                Ok(FixResult {
                    success: true,
                    message: format!("Set {} = {}", param, recommended),
                    requires_restart: true,
                })
            } else {
                Ok(FixResult {
                    success: false,
                    message: format!(
                        "Failed to set {}: {}",
                        param,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    requires_restart: false,
                })
            }
        }

        IssueType::PortConflict(port) => {
            let port = if *port == 0 {
                issue
                    .context
                    .get("port")
                    .and_then(|p| p.parse::<u16>().ok())
                    .unwrap_or(0)
            } else {
                *port
            };

            println!("🔧 Attempting to free port {}", port);

            // Find process using the port
            let lsof_output = Command::new("sudo")
                .arg("lsof")
                .arg("-i")
                .arg(format!(":{}", port))
                .output()
                .context("Failed to run lsof")?;

            if lsof_output.status.success() {
                let output_str = String::from_utf8_lossy(&lsof_output.stdout);
                // Parse PID from lsof output (usually in second column)
                if let Some(line) = output_str.lines().nth(1) {
                    if let Some(pid) = line.split_whitespace().nth(1) {
                        println!("🔧 Killing process {} using port {}", pid, port);
                        let _ = Command::new("sudo").arg("kill").arg("-9").arg(pid).output();
                    }
                }
            }

            Ok(FixResult {
                success: true,
                message: format!("Attempted to free port {}", port),
                requires_restart: true,
            })
        }

        IssueType::DiskSpace => {
            println!("🔧 Cleaning up disk space...");

            // Clean up old logs
            let _ = Command::new("find")
                .arg("/tmp")
                .arg("-name")
                .arg("*.log")
                .arg("-mtime")
                .arg("+7")
                .arg("-delete")
                .output();

            // Clean package cache
            let _ = Command::new("sudo").arg("apt-get").arg("clean").output();

            Ok(FixResult {
                success: true,
                message: "Cleaned up disk space".to_string(),
                requires_restart: false,
            })
        }

        IssueType::TooManyOpenFiles => {
            println!("🔧 Increasing file descriptor limits...");

            // Increase ulimit for current session
            let _ = Command::new("ulimit").arg("-n").arg("65536").output();

            // Update system limits
            let limits_conf = "* soft nofile 65536\n* hard nofile 65536\n";
            let _ = Command::new("sh")
                .arg("-c")
                .arg(format!(
                    "echo '{}' | sudo tee /etc/security/limits.d/90-nofile.conf",
                    limits_conf
                ))
                .output();

            Ok(FixResult {
                success: true,
                message: "Increased file descriptor limits".to_string(),
                requires_restart: true,
            })
        }

        IssueType::MemoryPressure => {
            println!("🔧 Attempting to free memory...");

            // Clear page cache
            let _ = Command::new("sudo")
                .arg("sh")
                .arg("-c")
                .arg("echo 1 > /proc/sys/vm/drop_caches")
                .output();

            Ok(FixResult {
                success: true,
                message: "Cleared system caches".to_string(),
                requires_restart: false,
            })
        }

        IssueType::NetworkConnectivity => {
            println!("🔧 Fixing network connectivity - opening Solana validator ports...");

            // Open required Solana validator ports in firewall
            let ports = ["8001", "8899", "8900", "8002:8020"];
            let mut success_count = 0;

            for port in &ports {
                println!("🔧 Opening firewall port {}/tcp", port);
                let output = Command::new("sudo")
                    .arg("ufw")
                    .arg("allow")
                    .arg(format!("{}/tcp", port))
                    .output();

                if let Ok(result) = output {
                    if result.status.success() {
                        success_count += 1;
                        println!("✅ Opened port {}/tcp", port);
                    } else {
                        println!(
                            "⚠️  Failed to open port {}/tcp: {}",
                            port,
                            String::from_utf8_lossy(&result.stderr)
                        );
                    }
                }
            }

            // Also try to enable UFW if it's not enabled
            let _ = Command::new("sudo")
                .arg("ufw")
                .arg("--force")
                .arg("enable")
                .output();

            Ok(FixResult {
                success: success_count > 0,
                message: format!(
                    "Opened {}/{} firewall ports for Solana validator",
                    success_count,
                    ports.len()
                ),
                requires_restart: true,
            })
        }

        IssueType::ExternalReachability => {
            osvm_info!(
                LogCategory::Ngrok,
                "apply_fix",
                "External reachability issue detected - setting up ngrok tunnels"
            );
            println!("🌐 Fixing external reachability issue - setting up ngrok tunnels...");

            // Get the port from context or use default Solana ports
            let failed_port = issue
                .context
                .get("port")
                .and_then(|p| p.parse::<u16>().ok())
                .unwrap_or(8899);

            osvm_debug!(
                LogCategory::Ngrok,
                "apply_fix",
                &format!(
                    "Failed port: {}, setting up tunnels for Solana ports",
                    failed_port
                )
            );

            // Set up ngrok tunnels for all Solana validator ports
            let solana_ports = vec![8001, 8899, 8900];

            match setup_ngrok_tunnels(&solana_ports).await {
                Ok(tunnel_urls) => {
                    let mut message = "Set up ngrok tunnels for external access".to_string();
                    if !tunnel_urls.is_empty() {
                        message.push_str(&format!(" - {} tunnels active", tunnel_urls.len()));
                        for (port, url) in &tunnel_urls {
                            println!("🔗 Port {} available at: {}", port, url);
                        }
                    }

                    Ok(FixResult {
                        success: true,
                        message,
                        requires_restart: false, // ngrok works with running validator
                    })
                }
                Err(e) => {
                    println!("❌ Failed to set up ngrok tunnels: {}", e);
                    Ok(FixResult {
                        success: false,
                        message: format!("Failed to set up ngrok: {}", e),
                        requires_restart: false,
                    })
                }
            }
        }

        _ => {
            // For other issues, we might need a restart
            Ok(FixResult {
                success: false,
                message: format!("No automatic fix available for {:?}", issue.issue_type),
                requires_restart: issue.severity >= Severity::Error,
            })
        }
    }
}

/// Monitor logs continuously and apply fixes
pub async fn monitor_logs_continuous(
    log_source: impl std::io::Read + Send + 'static,
    config: LogMonitorConfig,
    restart_callback: Option<Box<dyn Fn() -> Result<()> + Send + Sync>>,
) -> Result<()> {
    let patterns = Arc::new(init_log_patterns());
    let state = Arc::new(Mutex::new(MonitorState {
        issues_fixed: HashMap::new(),
        restart_count: 0,
        last_restart: None,
    }));

    let reader = BufReader::new(log_source);
    let (tx, mut rx) = mpsc::channel::<String>(1000);

    // Spawn log reader thread
    task::spawn_blocking(move || {
        for line in reader.lines().map_while(Result::ok) {
            let _ = tx.blocking_send(line);
        }
    });

    // Process logs and apply fixes
    while let Some(line) = rx.recv().await {
        // Display the log line
        if line.contains("ERROR") || line.contains("FATAL") {
            eprintln!("❌ {}", line);
        } else if line.contains("WARN") {
            println!("⚠️  {}", line);
        } else if line.contains("INFO")
            && (line.contains("obtained shred-version")
                || line.contains("RPC service")
                || line.contains("ledger processed"))
        {
            println!("📋 {}", line);
        }

        // Check against all patterns
        for pattern in patterns.iter() {
            if let Some(captures) = pattern.regex.captures(&line) {
                let context = (pattern.extractor)(&captures);

                let issue = DetectedIssue {
                    issue_type: match &pattern.issue_type {
                        IssueType::SystemTuning(_) => IssueType::SystemTuning(
                            context.get("parameter").cloned().unwrap_or_default(),
                        ),
                        IssueType::PortConflict(_) => IssueType::PortConflict(
                            context
                                .get("port")
                                .and_then(|p| p.parse().ok())
                                .unwrap_or(0),
                        ),
                        IssueType::ExternalReachability => {
                            // Use the captured context from the regex
                            IssueType::ExternalReachability
                        }
                        other => other.clone(),
                    },
                    severity: pattern.severity.clone(),
                    message: line.clone(),
                    context,
                    timestamp: chrono::Utc::now(),
                };

                println!(
                    "🔍 Detected issue: {:?} (Severity: {:?})",
                    issue.issue_type, issue.severity
                );

                // Apply fix
                match apply_fix(&issue, &config).await {
                    Ok(fix_result) => {
                        if fix_result.success {
                            println!("✅ Fix applied: {}", fix_result.message);

                            // Update state
                            let mut state_guard = state.lock().await;
                            *state_guard
                                .issues_fixed
                                .entry(issue.issue_type.clone())
                                .or_insert(0) += 1;

                            // Handle restart if needed
                            if fix_result.requires_restart && config.restart_on_critical {
                                let now = chrono::Utc::now();
                                let should_restart = match state_guard.last_restart {
                                    Some(last) => {
                                        let elapsed =
                                            now.signed_duration_since(last).num_seconds() as u64;
                                        elapsed >= config.restart_cooldown_seconds
                                    }
                                    None => true,
                                };

                                if should_restart
                                    && state_guard.restart_count < config.max_restart_attempts
                                {
                                    println!(
                                        "🔄 Restarting validator due to fix requiring restart..."
                                    );
                                    state_guard.restart_count += 1;
                                    state_guard.last_restart = Some(now);
                                    drop(state_guard);

                                    if let Some(ref callback) = restart_callback {
                                        if let Err(e) = callback() {
                                            eprintln!("❌ Failed to restart validator: {}", e);
                                        }
                                    }
                                }
                            }
                        } else {
                            println!("⚠️  Fix not applied: {}", fix_result.message);
                        }
                    }
                    Err(e) => {
                        eprintln!("❌ Failed to apply fix: {}", e);
                    }
                }
            }
        }
    }

    Ok(())
}

/// Create a log file monitor that tails the file (like tail -f)
pub async fn monitor_log_file(
    log_path: &str,
    config: LogMonitorConfig,
    restart_callback: Option<Box<dyn Fn() -> Result<()> + Send + Sync>>,
) -> Result<()> {
    use std::fs::File;
    use std::io::{BufRead, BufReader, Seek, SeekFrom};

    osvm_info!(
        LogCategory::AutoRepair,
        "monitor_log_file",
        &format!("Starting real-time log monitoring for: {}", log_path)
    );
    println!("🔍 Starting log monitoring for: {}", log_path);

    let patterns = Arc::new(init_log_patterns());
    let state = Arc::new(Mutex::new(MonitorState {
        issues_fixed: HashMap::new(),
        restart_count: 0,
        last_restart: None,
    }));

    // Open file and start reading from current position (not end)
    // This allows us to catch existing error messages in the log
    let mut file = File::open(log_path).context("Failed to open log file")?;

    let mut reader = BufReader::new(file);
    let mut line = String::new();

    loop {
        line.clear();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // No new data, wait a bit
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                continue;
            }
            Ok(_) => {
                let line = line.trim().to_string();
                if line.is_empty() {
                    continue;
                }

                // Display the log line with colors
                if line.contains("ERROR") || line.contains("FATAL") {
                    println!("❌ {}", line);
                } else if line.contains("WARN") {
                    println!("⚠️  {}", line);
                } else if line.contains("INFO")
                    && (line.contains("obtained shred-version")
                        || line.contains("RPC service")
                        || line.contains("ledger processed")
                        || line.contains("OS network limits test passed"))
                {
                    println!("📋 {}", line);
                }

                // Check against all patterns for auto-repair
                for pattern in patterns.iter() {
                    if let Some(captures) = pattern.regex.captures(&line) {
                        let context = (pattern.extractor)(&captures);

                        let issue = DetectedIssue {
                            issue_type: match &pattern.issue_type {
                                IssueType::SystemTuning(_) => IssueType::SystemTuning(
                                    context.get("parameter").cloned().unwrap_or_default(),
                                ),
                                IssueType::PortConflict(_) => IssueType::PortConflict(
                                    context
                                        .get("port")
                                        .and_then(|p| p.parse().ok())
                                        .unwrap_or(0),
                                ),
                                IssueType::ExternalReachability => IssueType::ExternalReachability,
                                other => other.clone(),
                            },
                            severity: pattern.severity.clone(),
                            message: line.clone(),
                            context,
                            timestamp: chrono::Utc::now(),
                        };

                        osvm_info!(
                            LogCategory::AutoRepair,
                            "monitor_log_file",
                            &format!(
                                "ISSUE DETECTED: {:?} (Severity: {:?}) - Message: {}",
                                issue.issue_type, issue.severity, issue.message
                            )
                        );
                        println!(
                            "🔍 Detected issue: {:?} (Severity: {:?})",
                            issue.issue_type, issue.severity
                        );

                        // Apply fix
                        match apply_fix(&issue, &config).await {
                            Ok(fix_result) => {
                                if fix_result.success {
                                    osvm_info!(
                                        LogCategory::AutoRepair,
                                        "monitor_log_file",
                                        &format!("FIX SUCCESSFUL: {}", fix_result.message)
                                    );
                                    println!("✅ Fix applied: {}", fix_result.message);

                                    // Update state
                                    let mut state_guard = state.lock().await;
                                    *state_guard
                                        .issues_fixed
                                        .entry(issue.issue_type.clone())
                                        .or_insert(0) += 1;

                                    // Handle restart if needed
                                    if fix_result.requires_restart && config.restart_on_critical {
                                        let now = chrono::Utc::now();
                                        let should_restart = match state_guard.last_restart {
                                            Some(last) => {
                                                let elapsed =
                                                    now.signed_duration_since(last).num_seconds()
                                                        as u64;
                                                elapsed >= config.restart_cooldown_seconds
                                            }
                                            None => true,
                                        };

                                        if should_restart
                                            && state_guard.restart_count
                                                < config.max_restart_attempts
                                        {
                                            println!("🔄 Restarting validator due to fix requiring restart...");
                                            state_guard.restart_count += 1;
                                            state_guard.last_restart = Some(now);
                                            drop(state_guard);

                                            if let Some(ref callback) = restart_callback {
                                                if let Err(e) = callback() {
                                                    eprintln!(
                                                        "❌ Failed to restart validator: {}",
                                                        e
                                                    );
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    println!("⚠️  Fix not applied: {}", fix_result.message);
                                }
                            }
                            Err(e) => {
                                eprintln!("❌ Failed to apply fix: {}", e);
                            }
                        }

                        // Break after first match to avoid multiple fixes for same line
                        break;
                    }
                }
            }
            Err(e) => {
                eprintln!("❌ Error reading log file: {}", e);
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_pattern_matching() {
        let patterns = init_log_patterns();

        // Test system tuning pattern
        let line = "net.core.rmem_max: recommended=134217728, current=212992 too small";
        let mut found = false;
        for pattern in &patterns {
            if let Some(caps) = pattern.regex.captures(line) {
                let context = (pattern.extractor)(&caps);
                assert_eq!(context.get("parameter").unwrap(), "net.core.rmem_max");
                assert_eq!(context.get("recommended").unwrap(), "134217728");
                found = true;
                break;
            }
        }
        assert!(found, "System tuning pattern should match");
    }
}
