//! ClickHouse database service for storing and querying blockchain data and user activity

use anyhow::{anyhow, Context, Result};
use log::{debug, error, info, warn};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use tokio::sync::Mutex;

/// ClickHouse service status
#[derive(Debug, Clone, PartialEq)]
pub enum ClickHouseStatus {
    Running,
    Stopped,
    Error(String),
}

/// ClickHouse service configuration
#[derive(Debug, Clone)]
pub struct ClickHouseConfig {
    pub data_dir: PathBuf,
    pub config_dir: PathBuf,
    pub http_port: u16,
    pub tcp_port: u16,
    pub log_dir: PathBuf,
}

impl Default for ClickHouseConfig {
    fn default() -> Self {
        let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
        let base_dir = PathBuf::from(home).join(".osvm").join("clickhouse");

        Self {
            data_dir: base_dir.join("data"),
            config_dir: base_dir.join("config"),
            http_port: 8123,
            tcp_port: 9000,
            log_dir: base_dir.join("logs"),
        }
    }
}

/// ClickHouse service for managing the database process
pub struct ClickHouseService {
    config: ClickHouseConfig,
    process: Arc<Mutex<Option<Child>>>,
    client: clickhouse::Client,
}

impl ClickHouseService {
    /// Create a new ClickHouse service with default configuration
    pub fn new() -> Result<Self> {
        Self::with_config(ClickHouseConfig::default())
    }

    /// Create a new ClickHouse service with custom configuration
    pub fn with_config(config: ClickHouseConfig) -> Result<Self> {
        let client = clickhouse::Client::default()
            .with_url(format!("http://localhost:{}", config.http_port))
            .with_database("osvm");

        Ok(Self {
            config,
            process: Arc::new(Mutex::new(None)),
            client,
        })
    }

    /// Initialize ClickHouse: download binary if needed and set up configuration
    pub async fn init(&self) -> Result<()> {
        info!("Initializing ClickHouse database...");

        // Create necessary directories
        std::fs::create_dir_all(&self.config.data_dir)
            .context("Failed to create data directory")?;
        std::fs::create_dir_all(&self.config.config_dir)
            .context("Failed to create config directory")?;
        std::fs::create_dir_all(&self.config.log_dir).context("Failed to create log directory")?;

        // Check if ClickHouse is installed
        if !self.is_clickhouse_installed() {
            info!("ClickHouse not found, downloading...");
            self.download_clickhouse().await?;
        } else {
            info!("ClickHouse binary found");
        }

        // Generate configuration file
        self.generate_config()?;

        // Create database schema
        self.create_schema().await?;

        info!("ClickHouse initialization complete");
        Ok(())
    }

    /// Check if ClickHouse binary is installed
    fn is_clickhouse_installed(&self) -> bool {
        Command::new("clickhouse")
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    /// Download ClickHouse binary
    async fn download_clickhouse(&self) -> Result<()> {
        info!("Downloading ClickHouse binary...");

        // Determine architecture and OS
        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;

        if os != "linux" {
            return Err(anyhow!(
                "Automatic ClickHouse installation is only supported on Linux. \
                 Please install ClickHouse manually: https://clickhouse.com/docs/en/install"
            ));
        }

        let download_url = match arch {
            "x86_64" => "https://builds.clickhouse.com/master/amd64/clickhouse",
            "aarch64" => "https://builds.clickhouse.com/master/aarch64/clickhouse",
            _ => return Err(anyhow!("Unsupported architecture: {}", arch)),
        };

        let bin_dir = self.config.config_dir.join("bin");
        std::fs::create_dir_all(&bin_dir)?;
        let bin_path = bin_dir.join("clickhouse");

        // Download the binary
        let response = reqwest::get(download_url).await?;
        let bytes = response.bytes().await?;
        std::fs::write(&bin_path, bytes)?;

        // Make executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&bin_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&bin_path, perms)?;
        }

        info!("ClickHouse binary downloaded successfully");
        Ok(())
    }

    /// Generate ClickHouse configuration file
    fn generate_config(&self) -> Result<()> {
        let config_content = format!(
            r#"<?xml version="1.0"?>
<clickhouse>
    <logger>
        <level>information</level>
        <log>{}/clickhouse-server.log</log>
        <errorlog>{}/clickhouse-server.err.log</errorlog>
        <size>1000M</size>
        <count>10</count>
    </logger>

    <http_port>{}</http_port>
    <tcp_port>{}</tcp_port>

    <path>{}/</path>
    <tmp_path>{}/tmp/</tmp_path>
    <user_files_path>{}/user_files/</user_files_path>
    <format_schema_path>{}/format_schemas/</format_schema_path>

    <users>
        <default>
            <password></password>
            <networks>
                <ip>::/0</ip>
            </networks>
            <profile>default</profile>
            <quota>default</quota>
        </default>
    </users>

    <profiles>
        <default>
            <max_memory_usage>10000000000</max_memory_usage>
            <use_uncompressed_cache>0</use_uncompressed_cache>
            <load_balancing>random</load_balancing>
        </default>
    </profiles>

    <quotas>
        <default>
            <interval>
                <duration>3600</duration>
                <queries>0</queries>
                <errors>0</errors>
                <result_rows>0</result_rows>
                <read_rows>0</read_rows>
                <execution_time>0</execution_time>
            </interval>
        </default>
    </quotas>
</clickhouse>
"#,
            self.config.log_dir.display(),
            self.config.log_dir.display(),
            self.config.http_port,
            self.config.tcp_port,
            self.config.data_dir.display(),
            self.config.data_dir.display(),
            self.config.data_dir.display(),
            self.config.data_dir.display(),
        );

        let config_file = self.config.config_dir.join("config.xml");
        std::fs::write(config_file, config_content)?;

        info!("ClickHouse configuration file generated");
        Ok(())
    }

    /// Create database schema (tables)
    async fn create_schema(&self) -> Result<()> {
        info!("Creating database schema...");

        // Start ClickHouse temporarily if not running
        let was_running = self.status().await? == ClickHouseStatus::Running;
        if !was_running {
            self.start().await?;
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
        }

        // Create database
        self.execute_query("CREATE DATABASE IF NOT EXISTS osvm")
            .await?;

        // Create tables
        self.create_cli_commands_table().await?;
        self.create_chat_messages_table().await?;
        self.create_blockchain_accounts_table().await?;
        self.create_blockchain_transactions_table().await?;

        if !was_running {
            self.stop().await?;
        }

        info!("Database schema created successfully");
        Ok(())
    }

    async fn create_cli_commands_table(&self) -> Result<()> {
        let sql = r#"
        CREATE TABLE IF NOT EXISTS osvm.cli_commands (
            timestamp DateTime64(3),
            session_id String,
            command String,
            args String,
            exit_code Int32,
            duration_ms UInt64,
            error_message String
        ) ENGINE = MergeTree()
        ORDER BY (timestamp, session_id)
        "#;
        self.execute_query(sql).await
    }

    async fn create_chat_messages_table(&self) -> Result<()> {
        let sql = r#"
        CREATE TABLE IF NOT EXISTS osvm.chat_messages (
            timestamp DateTime64(3),
            session_id String,
            session_name String,
            message_type Enum8('User' = 1, 'Assistant' = 2, 'System' = 3, 'Processing' = 4, 'Error' = 5),
            content String,
            tokens_used UInt32,
            model_name String
        ) ENGINE = MergeTree()
        ORDER BY (timestamp, session_id)
        "#;
        self.execute_query(sql).await
    }

    async fn create_blockchain_accounts_table(&self) -> Result<()> {
        let sql = r#"
        CREATE TABLE IF NOT EXISTS osvm.blockchain_accounts (
            slot UInt64,
            pubkey String,
            owner String,
            lamports UInt64,
            data_len UInt32,
            data_hash String,
            executable Bool,
            rent_epoch UInt64,
            program_type String,
            decoded_data String,
            data_sample Array(UInt8)
        ) ENGINE = ReplacingMergeTree(slot)
        ORDER BY (owner, pubkey, slot)
        "#;
        self.execute_query(sql).await
    }

    async fn create_blockchain_transactions_table(&self) -> Result<()> {
        let sql = r#"
        CREATE TABLE IF NOT EXISTS osvm.blockchain_transactions (
            slot UInt64,
            signature String,
            block_time DateTime64(3),
            fee UInt64,
            success Bool,
            accounts Array(String),
            program_ids Array(String),
            instruction_data Array(String),
            logs Array(String),
            instruction_count UInt16
        ) ENGINE = MergeTree()
        ORDER BY (slot, signature)
        "#;
        self.execute_query(sql).await
    }

    /// Start the ClickHouse server process
    pub async fn start(&self) -> Result<()> {
        let mut process_guard = self.process.lock().await;

        if process_guard.is_some() {
            return Ok(()); // Already running
        }

        info!("Starting ClickHouse server...");

        let config_file = self.config.config_dir.join("config.xml");
        if !config_file.exists() {
            return Err(anyhow!(
                "ClickHouse not initialized. Run 'osvm db init' first."
            ));
        }

        let clickhouse_bin = self.get_clickhouse_binary()?;

        let child = Command::new(clickhouse_bin)
            .arg("server")
            .arg("--config-file")
            .arg(&config_file)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .context("Failed to start ClickHouse server")?;

        *process_guard = Some(child);

        // Wait for server to be ready
        for _ in 0..30 {
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
            if self.is_server_ready().await {
                info!("ClickHouse server started successfully");
                return Ok(());
            }
        }

        Err(anyhow!(
            "ClickHouse server failed to start within 3 seconds"
        ))
    }

    /// Stop the ClickHouse server process
    pub async fn stop(&self) -> Result<()> {
        let mut process_guard = self.process.lock().await;

        if let Some(mut child) = process_guard.take() {
            info!("Stopping ClickHouse server...");

            // Try graceful shutdown first
            #[cfg(unix)]
            {
                use nix::sys::signal::{kill, Signal};
                use nix::unistd::Pid;

                if let Ok(pid) = child.id().try_into() {
                    let _ = kill(Pid::from_raw(pid), Signal::SIGTERM);
                }
            }

            // Wait up to 5 seconds for graceful shutdown
            for _ in 0..50 {
                match child.try_wait() {
                    Ok(Some(_)) => {
                        info!("ClickHouse server stopped gracefully");
                        return Ok(());
                    }
                    Ok(None) => {
                        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                    }
                    Err(e) => {
                        warn!("Error checking process status: {}", e);
                        break;
                    }
                }
            }

            // Force kill if still running
            warn!("Force killing ClickHouse server");
            let _ = child.kill();
            let _ = child.wait();
        }

        Ok(())
    }

    /// Get the current status of the ClickHouse server
    pub async fn status(&self) -> Result<ClickHouseStatus> {
        let process_guard = self.process.lock().await;

        if let Some(child) = process_guard.as_ref() {
            // Check if process is still running
            let is_running = self.is_server_ready().await;
            if is_running {
                Ok(ClickHouseStatus::Running)
            } else {
                Ok(ClickHouseStatus::Error(
                    "Process exists but not responding".to_string(),
                ))
            }
        } else {
            // Check if another instance is running
            if self.is_server_ready().await {
                Ok(ClickHouseStatus::Running)
            } else {
                Ok(ClickHouseStatus::Stopped)
            }
        }
    }

    /// Check if the server is ready to accept connections
    async fn is_server_ready(&self) -> bool {
        self.execute_query("SELECT 1").await.is_ok()
    }

    /// Execute a SQL query
    pub async fn execute_query(&self, sql: &str) -> Result<()> {
        self.client
            .query(sql)
            .execute()
            .await
            .context("Failed to execute query")?;
        Ok(())
    }

    /// Execute a SQL query and return results as JSON
    pub async fn query_json(&self, sql: &str) -> Result<String> {
        // Use HTTP client directly to get JSON response
        let url = format!(
            "http://localhost:{}/?query={}&default_format=JSONEachRow",
            self.config.http_port,
            urlencoding::encode(sql)
        );

        let response = reqwest::get(&url)
            .await
            .context("Failed to execute query")?;

        let text = response.text().await.context("Failed to read response")?;

        // Parse each line as JSON and collect into an array
        let mut results = Vec::new();
        for line in text.lines() {
            if !line.trim().is_empty() {
                if let Ok(value) = serde_json::from_str::<serde_json::Value>(line) {
                    results.push(value);
                }
            }
        }

        serde_json::to_string_pretty(&results).context("Failed to serialize results")
    }

    /// Get the ClickHouse binary path
    fn get_clickhouse_binary(&self) -> Result<PathBuf> {
        // Check custom binary first
        let custom_bin = self.config.config_dir.join("bin").join("clickhouse");
        if custom_bin.exists() {
            return Ok(custom_bin);
        }

        // Check system installation
        if self.is_clickhouse_installed() {
            Ok(PathBuf::from("clickhouse"))
        } else {
            Err(anyhow!(
                "ClickHouse binary not found. Run 'osvm db init' first."
            ))
        }
    }

    /// Get the client for direct access
    pub fn client(&self) -> &clickhouse::Client {
        &self.client
    }
}

impl Drop for ClickHouseService {
    fn drop(&mut self) {
        // Clean shutdown when service is dropped
        let process = self.process.clone();
        tokio::spawn(async move {
            let mut guard = process.lock().await;
            if let Some(mut child) = guard.take() {
                let _ = child.kill();
                let _ = child.wait();
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_clickhouse_config_default() {
        let config = ClickHouseConfig::default();
        assert_eq!(config.http_port, 8123);
        assert_eq!(config.tcp_port, 9000);
    }

    #[tokio::test]
    async fn test_clickhouse_service_creation() {
        let service = ClickHouseService::new();
        assert!(service.is_ok());
    }
}
