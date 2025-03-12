//! Dashboard utilities
//! Provides functionality to generate and manage monitoring dashboards

use {
    crate::utils::nodes::NodeInfo,
    crate::utils::nodes_dashboard::generate_monitoring_dashboard,
    solana_client::rpc_client::RpcClient,
    solana_sdk::commitment_config::CommitmentConfig,
    std::{error::Error, fmt, fs, path::Path, process::Command},
};

/// Dashboard error types
#[derive(Debug)]
pub enum DashboardError {
    /// File I/O error
    IoError(String),
    /// Browser launch error
    BrowserError(String),
    /// Other error
    Other(String),
}

impl fmt::Display for DashboardError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DashboardError::IoError(msg) => write!(f, "Dashboard I/O error: {}", msg),
            DashboardError::BrowserError(msg) => write!(f, "Browser launch error: {}", msg),
            DashboardError::Other(msg) => write!(f, "Dashboard error: {}", msg),
        }
    }
}

impl Error for DashboardError {}

/// Dashboard manager
#[derive(Debug)]
pub struct DashboardManager {
    /// Output directory for dashboard files
    output_dir: String,
    /// Dashboard title
    title: String,
    /// Verbosity level
    verbosity: u8,
}

impl Default for DashboardManager {
    fn default() -> Self {
        DashboardManager {
            output_dir: ".".to_string(),
            title: "OSVM Node Monitoring Dashboard".to_string(),
            verbosity: 1, // Default to standard verbosity
        }
    }
}

impl DashboardManager {
    /// Create a new dashboard manager
    ///
    /// # Arguments
    /// * `output_dir` - Directory to save dashboard files
    /// * `verbosity` - Verbosity level (0-3)
    ///
    /// # Returns
    /// * `DashboardManager` - Dashboard manager
    pub fn new(output_dir: &str, verbosity: u8) -> Self {
        DashboardManager {
            output_dir: output_dir.to_string(),
            title: "OSVM Node Monitoring Dashboard".to_string(),
            verbosity,
        }
    }

    /// Set dashboard title
    ///
    /// # Arguments
    /// * `title` - Dashboard title
    ///
    /// # Returns
    /// * `&mut Self` - Self reference for method chaining
    pub fn with_title(&mut self, title: &str) -> &mut Self {
        self.title = title.to_string();
        self
    }

    /// Set verbosity level
    ///
    /// # Arguments
    /// * `verbosity` - Verbosity level (0-3)
    ///
    /// # Returns
    /// * `&mut Self` - Self reference for method chaining
    pub fn with_verbosity(&mut self, verbosity: u8) -> &mut Self {
        self.verbosity = verbosity;
        self
    }

    /// Generate and save dashboard HTML
    ///
    /// # Arguments
    /// * `nodes` - List of node information
    ///
    /// # Returns
    /// * `Result<String, Box<dyn Error>>` - Path to the dashboard file
    pub fn generate_dashboard(&self, nodes: &[NodeInfo]) -> Result<String, Box<dyn Error>> {
        // Ensure output directory exists
        fs::create_dir_all(&self.output_dir)?;

        // Generate HTML content
        // Pass verbosity level to the dashboard generator
        let html_content = generate_monitoring_dashboard(nodes, self.verbosity);

        // Create output file path
        let file_path = format!("{}/osvm_dashboard.html", self.output_dir);

        // Write HTML to file
        fs::write(&file_path, html_content)?;

        Ok(file_path)
    }

    /// Open dashboard in default browser
    ///
    /// # Arguments
    /// * `file_path` - Path to the dashboard HTML file
    ///
    /// # Returns
    /// * `Result<(), Box<dyn Error>>` - Result
    pub fn open_in_browser(&self, file_path: &str) -> Result<(), Box<dyn Error>> {
        // Convert to absolute path if needed
        let path = Path::new(file_path);

        // Get file URL
        let file_url = if path.is_absolute() {
            format!("file://{}", path.display())
        } else {
            let abs_path = fs::canonicalize(path)?;
            format!("file://{}", abs_path.display())
        };

        // Determine the platform and open browser accordingly
        #[cfg(target_os = "windows")]
        {
            Command::new("cmd")
                .args(&["/c", "start", "", &file_url])
                .spawn()
                .map_err(|e| {
                    Box::new(DashboardError::BrowserError(e.to_string())) as Box<dyn Error>
                })?;
        }

        #[cfg(target_os = "macos")]
        {
            Command::new("open").arg(&file_url).spawn().map_err(|e| {
                Box::new(DashboardError::BrowserError(e.to_string())) as Box<dyn Error>
            })?;
        }

        #[cfg(target_os = "linux")]
        {
            Command::new("xdg-open")
                .arg(&file_url)
                .spawn()
                .map_err(|e| {
                    Box::new(DashboardError::BrowserError(e.to_string())) as Box<dyn Error>
                })?;
        }

        Ok(())
    }

    /// Generate dashboard and open in browser
    ///
    /// # Arguments
    /// * `nodes` - List of node information
    ///
    /// # Returns
    /// * `Result<(), Box<dyn Error>>` - Result
    pub fn generate_and_open(&self, nodes: &[NodeInfo]) -> Result<(), Box<dyn Error>> {
        let file_path = self.generate_dashboard(nodes)?;
        self.open_in_browser(&file_path)?;

        Ok(())
    }

    /// Create a real-time dashboard server (this would require an actual implementation)
    ///
    /// # Arguments
    /// * `port` - Port to run the dashboard server on
    ///
    /// # Returns
    /// * `Result<(), Box<dyn Error>>` - Result
    pub fn run_dashboard_server(&self, port: u16, verbosity: u8) -> Result<(), Box<dyn Error>> {
        // This would normally start a web server that provides real-time updates
        // For this prototype, we'll just print a message with detail based on verbosity
        println!(
            "Dashboard server would be running on http://localhost:{}",
            port
        );

        // Show additional details based on verbosity level
        match verbosity {
            0 => Ok(()), // Minimal output
            1 => {
                println!("(Real implementation would start an actual web server)");
                Ok(())
            }
            2 => {
                println!(
                    "Monitoring [node_count] nodes with refresh rate of {} seconds",
                    5
                );
                Ok(())
            }
            _ => {
                println!("Starting server with detailed debug logging enabled for development");
                Ok(())
            }
        }
    }
}

/// Save a quick dashboard to the current directory and open it
///
/// # Arguments
/// * `nodes` - List of node information
/// * `verbosity` - Verbosity level (0-3)
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Result
pub fn quick_dashboard(nodes: &[NodeInfo], verbosity: u8) -> Result<(), Box<dyn Error>> {
    let mut dashboard = DashboardManager::default();
    dashboard.with_verbosity(verbosity);
    dashboard.generate_and_open(nodes)
}

/// Run the dashboard with the given RPC client and commitment config
///
/// # Arguments
/// * `client` - RPC client
/// * `commitment_config` - Commitment config
///
/// # Returns
/// * `Result<(), Box<dyn Error>>` - Result
pub fn run_dashboard(
    client: &RpcClient,
    commitment_config: CommitmentConfig,
) -> Result<(), Box<dyn Error>> {
    crate::utils::nodes::run_dashboard(client, commitment_config, 1)
}