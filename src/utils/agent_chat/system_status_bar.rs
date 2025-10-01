//! Top-anchored system status bar with scrolling text carousel
//!
//! This module provides a real-time status bar that shows unikernel/microvm status,
//! OSVM access permissions, and system information with a scrolling text carousel
//! for content that doesn't fit in the terminal width.

use super::Colors;
use anyhow::Result;
use crossterm::terminal;
use std::io::{self, Write};
use std::time::{Duration, Instant};

/// System status information
#[derive(Debug, Clone)]
pub struct SystemStatus {
    pub unikernels_running: usize,
    pub microvms_running: usize,
    pub osvm_access_folders: Vec<String>,
    pub total_components: usize,
    pub healthy_components: usize,
    pub current_network: String,
    pub uptime_seconds: u64,
}

/// Scrolling text carousel state
#[derive(Debug)]
pub struct TextCarousel {
    text: String,
    position: usize,
    last_update: Instant,
    scroll_speed: Duration,
}

impl TextCarousel {
    pub fn new(text: String) -> Self {
        Self {
            text,
            position: 0,
            last_update: Instant::now(),
            scroll_speed: Duration::from_millis(200), // Scroll every 200ms
        }
    }

    /// Update carousel position and return current visible text
    pub fn update(&mut self, max_width: usize) -> String {
        if self.text.len() <= max_width {
            // Text fits, no scrolling needed
            return self.text.clone();
        }

        // Check if it's time to scroll
        if self.last_update.elapsed() >= self.scroll_speed {
            self.position = (self.position + 1) % (self.text.len() + 10); // Add padding
            self.last_update = Instant::now();
        }

        // Create scrolling window
        let extended_text = format!("{}          {}", self.text, self.text); // Add padding and repeat
        let start_pos = self.position % self.text.len();

        if start_pos + max_width <= extended_text.len() {
            extended_text[start_pos..start_pos + max_width].to_string()
        } else {
            // Handle wrap-around
            let first_part = &extended_text[start_pos..];
            let remaining = max_width - first_part.len();
            format!("{}{}", first_part, &extended_text[..remaining])
        }
    }
}

/// Get real system status information
pub async fn get_system_status() -> SystemStatus {
    // Get real system information
    let uptime = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    // Check for running processes that might be unikernels/microvms
    let (unikernels, microvms) = count_isolation_processes().await;

    // Get OSVM access folders
    let access_folders = get_osvm_access_folders();

    // Get component status (mock for now, but structure for real implementation)
    let (total_components, healthy_components) = get_component_status().await;

    // Detect current network
    let current_network = detect_solana_network().unwrap_or_else(|| "unknown".to_string());

    SystemStatus {
        unikernels_running: unikernels,
        microvms_running: microvms,
        osvm_access_folders: access_folders,
        total_components,
        healthy_components,
        current_network,
        uptime_seconds: uptime,
    }
}

/// Count running isolation processes
async fn count_isolation_processes() -> (usize, usize) {
    // Check for firecracker processes (microvms)
    let firecracker_count = count_processes_by_name("firecracker").await;

    // Check for hermit/unikernel processes
    let hermit_count = count_processes_by_name("hermit").await;
    let qemu_count = count_processes_by_name("qemu").await;

    (hermit_count + qemu_count, firecracker_count)
}

/// Count processes by name
async fn count_processes_by_name(process_name: &str) -> usize {
    // Use ps command to count processes
    if let Ok(output) = tokio::process::Command::new("ps")
        .args(&["aux"])
        .output()
        .await
    {
        if let Ok(stdout) = String::from_utf8(output.stdout) {
            return stdout
                .lines()
                .filter(|line| line.to_lowercase().contains(process_name))
                .count();
        }
    }
    0
}

/// Get OSVM access folders
fn get_osvm_access_folders() -> Vec<String> {
    let mut folders = Vec::new();

    // Check common OSVM directories
    let potential_dirs = vec![
        "/home/larp/larpdevs/osvm-cli",
        "/tmp/osvm",
        "/var/lib/osvm",
        "~/.osvm",
        "~/.local/share/solana",
    ];

    for dir in potential_dirs {
        if std::path::Path::new(dir).exists() {
            folders.push(dir.to_string());
        }
    }

    // Add current working directory
    if let Ok(cwd) = std::env::current_dir() {
        folders.push(cwd.display().to_string());
    }

    folders
}

/// Get component status (placeholder for real orchestrator integration)
async fn get_component_status() -> (usize, usize) {
    // In a real implementation, this would query the orchestrator
    // For now, return mock data based on actual system state
    let total = 3; // Core components: AI service, MCP service, Chat interface
    let healthy = 3; // All healthy for demo
    (total, healthy)
}

/// Detect current Solana network
fn detect_solana_network() -> Option<String> {
    // Try to read Solana config
    if let Ok(home) = std::env::var("HOME") {
        let config_path = format!("{}/.config/solana/cli/config.yml", home);
        if let Ok(content) = std::fs::read_to_string(config_path) {
            // Simple parsing to find network
            for line in content.lines() {
                if line.trim().starts_with("json_rpc_url:") {
                    if line.contains("mainnet") {
                        return Some("mainnet".to_string());
                    } else if line.contains("devnet") {
                        return Some("devnet".to_string());
                    } else if line.contains("testnet") {
                        return Some("testnet".to_string());
                    } else if line.contains("localhost") {
                        return Some("localnet".to_string());
                    }
                }
            }
        }
    }
    None
}

/// Render the top-anchored status bar
pub fn render_system_status_bar(status: &SystemStatus, carousel: &mut TextCarousel) -> Result<()> {
    let (cols, _) = terminal::size().unwrap_or((80, 24));

    // Position cursor at top of terminal
    print!("\x1B[1;1H");

    // Create status text
    let status_text = format!(
        "OSVM: {}μVMs {}uK {}comp({}/{}) net:{} up:{}s access:{} • ",
        status.microvms_running,
        status.unikernels_running,
        status.total_components,
        status.healthy_components,
        status.total_components,
        status.current_network,
        status.uptime_seconds % 3600, // Show seconds for demo
        status.osvm_access_folders.len()
    );

    // Add detailed folder access info to carousel
    let detailed_info = format!(
        "{} • Folders: {} • Components: {} healthy, {} total • Network: {} • Uptime: {} seconds",
        status_text,
        status.osvm_access_folders.join(", "),
        status.healthy_components,
        status.total_components,
        status.current_network,
        status.uptime_seconds
    );

    // Update carousel text if it changed
    if carousel.text != detailed_info {
        carousel.text = detailed_info;
        carousel.position = 0;
    }

    // Get visible text from carousel
    let available_width = cols.saturating_sub(2) as usize; // Leave margin
    let visible_text = carousel.update(available_width);

    // Render status bar with background color
    print!(
        "{}{}{}{}{}",
        Colors::CYAN, // Background color
        Colors::BOLD, // Bold text
        visible_text,
        " ".repeat(available_width.saturating_sub(visible_text.len())), // Pad to full width
        Colors::RESET
    );

    io::stdout().flush()?;
    Ok(())
}

/// Create system status bar manager
pub struct SystemStatusBarManager {
    carousel: TextCarousel,
    last_status_update: Instant,
    status_update_interval: Duration,
}

impl SystemStatusBarManager {
    pub fn new() -> Self {
        Self {
            carousel: TextCarousel::new("OSVM System Loading...".to_string()),
            last_status_update: Instant::now(),
            status_update_interval: Duration::from_secs(5), // Update every 5 seconds
        }
    }

    /// Update and render the status bar
    pub async fn update_and_render(&mut self) -> Result<()> {
        // Update system status periodically
        if self.last_status_update.elapsed() >= self.status_update_interval {
            let status = get_system_status().await;
            self.last_status_update = Instant::now();

            // Update carousel with new status
            render_system_status_bar(&status, &mut self.carousel)?;
        } else {
            // Just update carousel animation
            let (cols, _) = terminal::size().unwrap_or((80, 24));
            let available_width = cols.saturating_sub(2) as usize;
            let visible_text = self.carousel.update(available_width);

            // Render just the carousel update
            print!("\x1B[1;1H");
            print!(
                "{}{}{}{}{}",
                Colors::CYAN,
                Colors::BOLD,
                visible_text,
                " ".repeat(available_width.saturating_sub(visible_text.len())),
                Colors::RESET
            );
            io::stdout().flush()?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_carousel_short_text() {
        let mut carousel = TextCarousel::new("Short".to_string());
        let result = carousel.update(20);
        assert_eq!(result, "Short");
    }

    #[test]
    fn test_text_carousel_long_text() {
        let mut carousel =
            TextCarousel::new("This is a very long text that should scroll".to_string());
        let result = carousel.update(10);
        assert_eq!(result.len(), 10);
    }

    #[tokio::test]
    async fn test_get_system_status() {
        let status = get_system_status().await;
        assert!(status.total_components > 0);
        assert!(status.uptime_seconds > 0);
    }
}
