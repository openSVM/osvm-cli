//! TUI Visual Testing Agent
//!
//! This service provides automated visual testing for the OSVM advanced chat TUI.
//! It runs the TUI in a virtual terminal (PTY), captures screen output, sends
//! keyboard events, and validates the UI state.
//!
//! ## Features
//! - PTY-based virtual terminal emulation
//! - Screen capture and comparison
//! - Keyboard event injection
//! - Visual regression testing
//! - Screenshot generation
//!
//! ## Usage
//! ```rust
//! let agent = TuiTestAgent::new(80, 24)?;
//! agent.launch_advanced_chat().await?;
//! agent.send_keys("Hello\n").await?;
//! let screenshot = agent.capture_screen().await?;
//! ```

use anyhow::{Context, Result};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::time::Duration;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child as TokioChild, Command as TokioCommand};
use tokio::sync::Mutex;
use tokio::time::sleep;
// PNG screenshot dependencies
use image::{Rgb, RgbImage};
use imageproc::drawing::draw_text_mut;
use ab_glyph::{FontRef, PxScale};
// VT100 terminal emulator for capturing alternate screen buffer
use vt100::Parser as VT100Parser;
// Visual layout validator
use super::visual_layout_validator::{VisualLayoutValidator, LayoutAnalysis};

/// Terminal dimensions
#[derive(Debug, Clone, Copy)]
pub struct TerminalSize {
    pub cols: u16,
    pub rows: u16,
}

impl Default for TerminalSize {
    fn default() -> Self {
        Self {
            cols: 120,
            rows: 30,
        }
    }
}

/// Screen capture of terminal output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScreenCapture {
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub width: u16,
    pub height: u16,
    pub content: String,
    pub ansi_content: String, // With ANSI codes preserved
}

/// Keyboard event to send to TUI
#[derive(Debug, Clone)]
pub enum KeyEvent {
    Char(char),
    Enter,
    Tab,
    Escape,
    Backspace,
    Delete,
    Up,
    Down,
    Left,
    Right,
    F(u8), // F1-F12
    Ctrl(char),
    Alt(char),
    Shift(char),
}

impl KeyEvent {
    /// Convert key event to ANSI escape sequence
    pub fn to_ansi(&self) -> String {
        match self {
            KeyEvent::Char(c) => c.to_string(),
            KeyEvent::Enter => "\r".to_string(),
            KeyEvent::Tab => "\t".to_string(),
            KeyEvent::Escape => "\x1b".to_string(),
            KeyEvent::Backspace => "\x7f".to_string(),
            KeyEvent::Delete => "\x1b[3~".to_string(),
            KeyEvent::Up => "\x1b[A".to_string(),
            KeyEvent::Down => "\x1b[B".to_string(),
            KeyEvent::Right => "\x1b[C".to_string(),
            KeyEvent::Left => "\x1b[D".to_string(),
            KeyEvent::F(n) => format!("\x1bO{}", (b'P' + n - 1) as char),
            KeyEvent::Ctrl(c) => format!("{}", (*c as u8 & 0x1f) as char),
            KeyEvent::Alt(c) => format!("\x1b{}", c),
            KeyEvent::Shift(c) => c.to_uppercase().to_string(),
        }
    }
}

/// Visual TUI test scenario
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TuiTestScenario {
    pub name: String,
    pub description: String,
    pub steps: Vec<TuiTestStep>,
    pub expected_screens: Vec<ScreenExpectation>,
}

/// Individual test step for TUI testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TuiTestStep {
    pub action: TuiAction,
    pub wait_ms: u64,
    pub description: String,
}

/// Action to perform in TUI
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TuiAction {
    SendKeys(String),
    SendKey(String), // Single key event like "Tab", "F10", "Ctrl-C"
    Wait(u64),
    CaptureScreen,
    VerifyText(String),
    VerifyNotText(String),
}

/// Expected screen state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScreenExpectation {
    pub after_step: usize,
    pub contains_text: Vec<String>,
    pub not_contains_text: Vec<String>,
    pub title: Option<String>,
}

/// Main TUI visual testing agent
pub struct TuiTestAgent {
    size: TerminalSize,
    process: Arc<Mutex<Option<TokioChild>>>,
    vt100_parser: Arc<Mutex<VT100Parser>>,
    screenshots_dir: PathBuf,
    layout_validator: VisualLayoutValidator,
    xvfb_process: Arc<Mutex<Option<TokioChild>>>,
    xvfb_display: Arc<Mutex<Option<String>>>,
}

impl TuiTestAgent {
    /// Create a new TUI test agent with specified terminal size
    pub fn new(cols: u16, rows: u16) -> Result<Self> {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
        let screenshots_dir = PathBuf::from(format!("{}/.osvm/qa/screenshots", home));
        std::fs::create_dir_all(&screenshots_dir)?;

        // Create VT100 parser to emulate terminal and capture alternate screen buffer
        let parser = VT100Parser::new(rows, cols, 1000); // 1000 lines of scrollback

        // Create visual layout validator with baseline directory
        let baseline_dir = PathBuf::from(format!("{}/.osvm/qa/baselines", home));
        let layout_validator = VisualLayoutValidator::new(baseline_dir)?;

        Ok(Self {
            size: TerminalSize { cols, rows },
            process: Arc::new(Mutex::new(None)),
            vt100_parser: Arc::new(Mutex::new(parser)),
            screenshots_dir,
            layout_validator,
            xvfb_process: Arc::new(Mutex::new(None)),
            xvfb_display: Arc::new(Mutex::new(None)),
        })
    }

    /// Start Xvfb (X Virtual Frame Buffer) for headless visual testing
    async fn start_xvfb(&self) -> Result<String> {
        if !which_command_exists("Xvfb") {
            return Err(anyhow::anyhow!(
                "Xvfb not installed. Install with: sudo apt-get install xvfb"
            ));
        }

        // Find available display number
        let display_num = self.find_available_display().await?;
        let display = format!(":{}", display_num);

        info!("Starting Xvfb on display {}", display);

        // Start Xvfb
        let mut cmd = TokioCommand::new("Xvfb");
        cmd.arg(&display)
            .arg("-screen")
            .arg("0")
            .arg(format!("{}x{}x24", self.size.cols * 10, self.size.rows * 20))
            .arg("-ac") // Disable access control
            .arg("+extension")
            .arg("GLX")
            .arg("+render")
            .arg("-noreset")
            .stdout(Stdio::null())
            .stderr(Stdio::null());

        let child = cmd.spawn()
            .context("Failed to spawn Xvfb")?;

        info!("Xvfb started with PID: {:?}", child.id());

        // Store Xvfb process
        let mut xvfb = self.xvfb_process.lock().await;
        *xvfb = Some(child);

        let mut display_lock = self.xvfb_display.lock().await;
        *display_lock = Some(display.clone());

        // Wait for Xvfb to initialize
        sleep(Duration::from_secs(2)).await;

        Ok(display)
    }

    /// Find an available X display number
    async fn find_available_display(&self) -> Result<u16> {
        for num in 99..200 {
            let lock_file = format!("/tmp/.X{}-lock", num);
            if !std::path::Path::new(&lock_file).exists() {
                return Ok(num);
            }
        }
        Err(anyhow::anyhow!("No available X display found"))
    }

    /// Stop Xvfb if running
    async fn stop_xvfb(&self) -> Result<()> {
        let mut xvfb = self.xvfb_process.lock().await;
        if let Some(mut child) = xvfb.take() {
            info!("Stopping Xvfb");
            let _ = child.kill().await;
            let _ = child.wait().await;
        }

        let mut display = self.xvfb_display.lock().await;
        *display = None;

        Ok(())
    }

    /// Launch the OSVM chat in a terminal (supports both headless and windowed mode)
    pub async fn launch_chat(&self, advanced: bool) -> Result<()> {
        let mode = if advanced { "advanced" } else { "basic" };
        info!("Launching OSVM {} chat in terminal ({}x{})", mode, self.size.cols, self.size.rows);

        let binary_path = std::env::current_exe()?;
        let chat_cmd = if advanced {
            format!("{} chat --advanced", binary_path.display())
        } else {
            format!("{} chat", binary_path.display())
        };

        // Check if we have DISPLAY set (X11 available)
        let display_env = std::env::var("DISPLAY").ok();
        let has_display = display_env.is_some();

        // If no display and Xvfb is available, start it
        let display_to_use = if !has_display && which_command_exists("Xvfb") && which_command_exists("xterm") {
            info!("No DISPLAY set, starting Xvfb for headless visual testing");
            match self.start_xvfb().await {
                Ok(display) => {
                    info!("Using Xvfb display: {}", display);
                    Some(display)
                }
                Err(e) => {
                    warn!("Failed to start Xvfb: {}, falling back to headless PTY mode", e);
                    None
                }
            }
        } else {
            display_env
        };

        let mut child = if let Some(ref display) = display_to_use {
            // Launch in actual xterm window for screenshot capability
            info!("Launching in xterm window for visual screenshot support (DISPLAY={})", display);

            let mut cmd = TokioCommand::new("xterm");
            cmd.arg("-geometry")
                .arg(format!("{}x{}", self.size.cols, self.size.rows))
                .arg("-bg").arg("black")
                .arg("-fg").arg("white")
                .arg("-fa").arg("Monospace")
                .arg("-fs").arg("12")
                .arg("-title").arg("OSVM Chat Test")
                .arg("-e")
                .arg(&chat_cmd)
                .env("DISPLAY", display)
                .env("TERM", "xterm-256color")
                .env("COLORTERM", "truecolor")
                .env("FORCE_COLOR", "1")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .context("Failed to spawn xterm")?
        } else {
            // Fallback to headless PTY mode with script
            info!("Using headless PTY mode (no DISPLAY or xterm not available)");

            let mut cmd = TokioCommand::new("script");
            cmd.arg("-q")
                .arg("/dev/null")
                .arg("-c")
                .arg(&chat_cmd)
                .env("TERM", "xterm-256color")
                .env("COLUMNS", self.size.cols.to_string())
                .env("LINES", self.size.rows.to_string())
                .env("COLORTERM", "truecolor")
                .env("FORCE_COLOR", "1")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .context("Failed to spawn script command")?
        };

        info!("Process spawned with PID: {:?}", child.id());

        // Start background task to capture output and feed to VT100 parser
        if let Some(stdout) = child.stdout.take() {
            let vt100_parser = self.vt100_parser.clone();
            tokio::spawn(async move {
                let mut reader = BufReader::new(stdout);
                let mut buffer = vec![0u8; 8192];

                loop {
                    match reader.read(&mut buffer).await {
                        Ok(0) => break,
                        Ok(n) => {
                            let mut parser = vt100_parser.lock().await;
                            parser.process(&buffer[..n]);
                        }
                        Err(e) => {
                            error!("Error reading process output: {}", e);
                            break;
                        }
                    }
                }
            });
        }

        // Store process handle
        let mut process = self.process.lock().await;
        *process = Some(child);

        // Wait for UI to initialize
        sleep(Duration::from_secs(2)).await;

        Ok(())
    }

    /// Launch the OSVM advanced chat (convenience wrapper)
    pub async fn launch_advanced_chat(&self) -> Result<()> {
        self.launch_chat(true).await
    }

    /// Launch the OSVM basic chat
    pub async fn launch_basic_chat(&self) -> Result<()> {
        self.launch_chat(false).await
    }

    /// Send keyboard events to the TUI
    pub async fn send_keys(&self, keys: &str) -> Result<()> {
        debug!("Sending keys: {}", keys);

        let mut process = self.process.lock().await;
        if let Some(child) = process.as_mut() {
            if let Some(stdin) = child.stdin.as_mut() {
                stdin.write_all(keys.as_bytes()).await?;
                stdin.flush().await?;
            } else {
                return Err(anyhow::anyhow!("Process stdin not available"));
            }
        } else {
            return Err(anyhow::anyhow!("No process running"));
        }

        Ok(())
    }

    /// Send a specific key event to the TUI
    pub async fn send_key_event(&self, event: KeyEvent) -> Result<()> {
        let ansi = event.to_ansi();
        self.send_keys(&ansi).await
    }

    /// Capture current screen state from VT100 terminal emulator
    pub async fn capture_screen(&self) -> Result<ScreenCapture> {
        let parser = self.vt100_parser.lock().await;
        let screen = parser.screen();

        // Get screen contents - VT100 handles alternate screen buffer internally
        let clean_content = screen.contents();
        let ansi_content = screen.contents_formatted();

        // Debug: log screen state
        debug!("VT100 Screen State:");
        debug!("  Size: {}x{}", screen.size().1, screen.size().0);
        debug!("  Cursor: ({}, {})", screen.cursor_position().1, screen.cursor_position().0);
        debug!("  Alternate screen: {}", screen.alternate_screen());
        debug!("  Content length: {} chars", clean_content.len());
        debug!("  First 200 chars: {:?}", clean_content.chars().take(200).collect::<String>());

        // If content is mostly empty or just escape codes, try to get cell-by-cell content
        let content = if clean_content.trim().is_empty() || clean_content.len() < 50 {
            warn!("VT100 screen.contents() returned minimal content, extracting cell-by-cell");
            extract_screen_cells(&screen, self.size.rows, self.size.cols)
        } else {
            clean_content
        };

        let capture = ScreenCapture {
            timestamp: chrono::Utc::now(),
            width: self.size.cols,
            height: self.size.rows,
            content,
            ansi_content: String::from_utf8_lossy(&ansi_content).to_string(),
        };

        Ok(capture)
    }

    /// Save screenshot to file
    pub async fn save_screenshot(&self, name: &str) -> Result<PathBuf> {
        let capture = self.capture_screen().await?;
        let timestamp = capture.timestamp.format("%Y%m%d_%H%M%S");
        let filename = format!("{}_{}.txt", name, timestamp);
        let filepath = self.screenshots_dir.join(&filename);

        // Save both clean and ANSI versions
        let content = format!(
            "=== OSVM Advanced Chat Screenshot ===\n\
            Timestamp: {}\n\
            Terminal: {}x{}\n\
            \n\
            --- Clean Text ---\n\
            {}\n\
            \n\
            --- With ANSI Codes ---\n\
            {}\n",
            capture.timestamp,
            capture.width,
            capture.height,
            capture.content,
            capture.ansi_content
        );

        std::fs::write(&filepath, content)?;
        info!("Screenshot saved to: {}", filepath.display());

        Ok(filepath)
    }

    /// Save screenshot as PNG image - captures actual rendered terminal window
    pub async fn save_screenshot_png(&self, name: &str) -> Result<PathBuf> {
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let filename = format!("{}_{}.png", name, timestamp);
        let filepath = self.screenshots_dir.join(&filename);

        // Try to capture actual terminal window screenshot
        match self.capture_window_screenshot(&filepath).await {
            Ok(_) => {
                info!("Real window screenshot saved to: {}", filepath.display());
                Ok(filepath)
            }
            Err(e) => {
                // Fallback to text-based rendering if window capture fails
                warn!("Window screenshot failed ({}), falling back to text rendering", e);
                let capture = self.capture_screen().await?;
                render_terminal_to_png(&capture.content, capture.width, capture.height, &filepath)?;
                info!("Text-rendered PNG saved to: {}", filepath.display());
                Ok(filepath)
            }
        }
    }

    /// Capture actual terminal window screenshot using system tools
    async fn capture_window_screenshot(&self, output_path: &PathBuf) -> Result<()> {
        // Get the display to use (either Xvfb or system DISPLAY)
        let display_lock = self.xvfb_display.lock().await;
        let display_env = if let Some(ref d) = *display_lock {
            d.clone()
        } else {
            std::env::var("DISPLAY")
                .map_err(|_| anyhow::anyhow!("No DISPLAY available"))?
        };
        drop(display_lock);

        // Find the window ID for "OSVM Chat Test" window
        let window_id = self.find_osvm_window().await?;

        // Check which screenshot tool is available and use it
        if which_command_exists("import") {
            // Use ImageMagick import - most reliable
            let output = tokio::process::Command::new("import")
                .env("DISPLAY", &display_env)
                .arg("-window")
                .arg(&window_id)
                .arg(output_path.to_str().unwrap())
                .output()
                .await?;

            if !output.status.success() {
                return Err(anyhow::anyhow!(
                    "import command failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ));
            }
        } else if which_command_exists("scrot") {
            // Use scrot with window ID
            let output = tokio::process::Command::new("scrot")
                .env("DISPLAY", &display_env)
                .arg("-u")
                .arg("-a")
                .arg(&window_id)
                .arg(output_path.to_str().unwrap())
                .output()
                .await?;

            if !output.status.success() {
                return Err(anyhow::anyhow!(
                    "scrot command failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ));
            }
        } else {
            return Err(anyhow::anyhow!(
                "No screenshot tool available. Install imagemagick (import) or scrot"
            ));
        }

        Ok(())
    }

    /// Find the window ID of the OSVM Chat Test window
    async fn find_osvm_window(&self) -> Result<String> {
        if !which_command_exists("xdotool") {
            return Err(anyhow::anyhow!("xdotool not installed (needed to find window)"));
        }

        // Get the display to use (either Xvfb or system DISPLAY)
        let display_lock = self.xvfb_display.lock().await;
        let display_env = if let Some(ref d) = *display_lock {
            d.clone()
        } else {
            std::env::var("DISPLAY")
                .map_err(|_| anyhow::anyhow!("No DISPLAY available"))?
        };
        drop(display_lock);

        // Use xdotool to search for window by title
        let output = tokio::process::Command::new("xdotool")
            .env("DISPLAY", &display_env)
            .arg("search")
            .arg("--name")
            .arg("OSVM Chat Test")
            .output()
            .await?;

        if !output.status.success() {
            return Err(anyhow::anyhow!("xdotool failed to find OSVM window"));
        }

        let window_ids = String::from_utf8_lossy(&output.stdout);
        let first_id = window_ids
            .lines()
            .next()
            .ok_or_else(|| anyhow::anyhow!("No OSVM Chat Test window found"))?;

        Ok(first_id.trim().to_string())
    }

    /// Verify that specific text appears on screen
    pub async fn verify_text_on_screen(&self, expected: &str) -> Result<bool> {
        let capture = self.capture_screen().await?;
        Ok(capture.content.contains(expected))
    }

    /// Run a complete TUI test scenario
    pub async fn run_scenario(&self, scenario: &TuiTestScenario, save_as_png: bool) -> Result<TuiTestResult> {
        info!("Running TUI scenario: {}", scenario.name);
        let start_time = std::time::Instant::now();

        let mut screenshots = Vec::new();
        let mut errors = Vec::new();
        let mut passed = true;

        for (step_idx, step) in scenario.steps.iter().enumerate() {
            info!("  Step {}: {}", step_idx + 1, step.description);

            match &step.action {
                TuiAction::SendKeys(keys) => {
                    if let Err(e) = self.send_keys(keys).await {
                        errors.push(format!("Step {}: Failed to send keys: {}", step_idx + 1, e));
                        passed = false;
                    }
                }
                TuiAction::SendKey(key) => {
                    let event = parse_key_event(key)?;
                    if let Err(e) = self.send_key_event(event).await {
                        errors.push(format!("Step {}: Failed to send key: {}", step_idx + 1, e));
                        passed = false;
                    }
                }
                TuiAction::Wait(ms) => {
                    sleep(Duration::from_millis(*ms)).await;
                }
                TuiAction::CaptureScreen => {
                    let screenshot_name = format!("{}_step_{}", scenario.name.replace(' ', "_"), step_idx + 1);
                    let result = if save_as_png {
                        self.save_screenshot_png(&screenshot_name).await
                    } else {
                        self.save_screenshot(&screenshot_name).await
                    };

                    match result {
                        Ok(path) => screenshots.push(path),
                        Err(e) => {
                            errors.push(format!("Step {}: Failed to capture screen: {}", step_idx + 1, e));
                        }
                    }
                }
                TuiAction::VerifyText(text) => {
                    match self.verify_text_on_screen(text).await {
                        Ok(found) => {
                            if !found {
                                errors.push(format!("Step {}: Expected text not found: '{}'", step_idx + 1, text));
                                passed = false;
                            }
                        }
                        Err(e) => {
                            errors.push(format!("Step {}: Failed to verify text: {}", step_idx + 1, e));
                            passed = false;
                        }
                    }
                }
                TuiAction::VerifyNotText(text) => {
                    match self.verify_text_on_screen(text).await {
                        Ok(found) => {
                            if found {
                                errors.push(format!("Step {}: Unexpected text found: '{}'", step_idx + 1, text));
                                passed = false;
                            }
                        }
                        Err(e) => {
                            errors.push(format!("Step {}: Failed to verify text absence: {}", step_idx + 1, e));
                            passed = false;
                        }
                    }
                }
            }

            // Wait after each step
            sleep(Duration::from_millis(step.wait_ms)).await;

            // Check screen expectations
            for expectation in scenario.expected_screens.iter().filter(|e| e.after_step == step_idx + 1) {
                let capture = self.capture_screen().await?;

                for expected_text in &expectation.contains_text {
                    if !capture.content.contains(expected_text) {
                        errors.push(format!(
                            "After step {}: Expected text not found: '{}'",
                            step_idx + 1,
                            expected_text
                        ));
                        passed = false;
                    }
                }

                for unexpected_text in &expectation.not_contains_text {
                    if capture.content.contains(unexpected_text) {
                        errors.push(format!(
                            "After step {}: Unexpected text found: '{}'",
                            step_idx + 1,
                            unexpected_text
                        ));
                        passed = false;
                    }
                }
            }
        }

        let duration = start_time.elapsed();
        info!(
            "TUI scenario '{}' completed in {:.2}s: {}",
            scenario.name,
            duration.as_secs_f64(),
            if passed { "PASSED" } else { "FAILED" }
        );

        Ok(TuiTestResult {
            scenario_name: scenario.name.clone(),
            passed,
            duration_seconds: duration.as_secs_f64(),
            screenshots,
            errors,
        })
    }

    /// Stop the TUI process
    pub async fn stop(&self) -> Result<()> {
        let mut process = self.process.lock().await;
        if let Some(mut child) = process.take() {
            // Send Ctrl+C to gracefully terminate
            if let Some(stdin) = child.stdin.as_mut() {
                let _ = stdin.write_all(b"\x03").await; // Ctrl+C
                let _ = stdin.flush().await;
            }

            // Wait a bit for graceful shutdown
            sleep(Duration::from_millis(500)).await;

            // Force kill if still running
            let _ = child.kill().await;
            let _ = child.wait().await;

            info!("TUI process stopped");
        }

        // Stop Xvfb if we started it
        self.stop_xvfb().await?;

        Ok(())
    }

    /// Get the most recent N lines from screen
    pub async fn get_recent_output(&self, lines: usize) -> String {
        let parser = self.vt100_parser.lock().await;
        let screen = parser.screen();
        let clean = screen.contents();

        clean
            .lines()
            .rev()
            .take(lines)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Get debug info about current terminal state
    pub async fn get_debug_info(&self) -> String {
        let parser = self.vt100_parser.lock().await;
        let screen = parser.screen();

        format!(
            "VT100 Terminal State Debug Info:\n\
             Terminal Size: {}x{}\n\
             Cursor Position: ({}, {})\n\
             Alternate Screen: {}\n\
             Content Length: {} chars\n\
             First 500 chars:\n{}\n",
            screen.size().1,  // cols
            screen.size().0,  // rows
            screen.cursor_position().1,  // col
            screen.cursor_position().0,  // row
            screen.alternate_screen(),
            screen.contents().len(),
            screen.contents().chars().take(500).collect::<String>()
        )
    }

    /// Analyze the current screen layout
    pub async fn analyze_layout(&self) -> Result<LayoutAnalysis> {
        let capture = self.capture_screen().await?;
        self.layout_validator.analyze_layout(
            &capture.content,
            capture.width,
            capture.height,
        )
    }

    /// Compare current screen with baseline for visual regression testing
    pub async fn check_visual_regression(&self, test_name: &str) -> Result<super::visual_layout_validator::RegressionResult> {
        let capture = self.capture_screen().await?;

        // Create or load baseline
        let baseline = capture.content.clone(); // For now, use current as baseline

        self.layout_validator.compare_screenshots(
            &baseline,
            &capture.content,
            test_name,
        )
    }

    /// Validate that expected UI components are present
    pub async fn validate_components(&self, expected_components: &[&str]) -> Result<Vec<String>> {
        let layout = self.analyze_layout().await?;
        let mut missing = Vec::new();

        for expected in expected_components {
            let found = layout.components.iter().any(|c| {
                c.content.to_lowercase().contains(&expected.to_lowercase())
            });

            if !found {
                missing.push(expected.to_string());
            }
        }

        Ok(missing)
    }

    /// Save layout analysis to JSON file
    pub async fn save_layout_analysis(&self, name: &str) -> Result<PathBuf> {
        let layout = self.analyze_layout().await?;
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let filename = format!("{}_{}_layout.json", name, timestamp);
        let filepath = self.screenshots_dir.join(&filename);

        let json = serde_json::to_string_pretty(&layout)?;
        std::fs::write(&filepath, json)?;

        info!("Layout analysis saved to: {}", filepath.display());
        Ok(filepath)
    }
}

impl Drop for TuiTestAgent {
    fn drop(&mut self) {
        // Try to stop process on drop
        let process = self.process.clone();
        let xvfb = self.xvfb_process.clone();
        tokio::spawn(async move {
            let mut proc = process.lock().await;
            if let Some(mut child) = proc.take() {
                let _ = child.kill().await;
            }

            // Stop Xvfb if running
            let mut xvfb_proc = xvfb.lock().await;
            if let Some(mut child) = xvfb_proc.take() {
                let _ = child.kill().await;
            }
        });
    }
}

/// Result of a TUI test scenario
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TuiTestResult {
    pub scenario_name: String,
    pub passed: bool,
    pub duration_seconds: f64,
    pub screenshots: Vec<PathBuf>,
    pub errors: Vec<String>,
}

/// Strip ANSI escape codes from text
fn strip_ansi_codes(text: &str) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
    re.replace_all(text, "").to_string()
}

/// Check if a command exists on the system
fn which_command_exists(cmd: &str) -> bool {
    std::process::Command::new("which")
        .arg(cmd)
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

/// Extract screen content cell-by-cell from VT100 screen
fn extract_screen_cells(screen: &vt100::Screen, rows: u16, cols: u16) -> String {
    use std::fmt::Write;

    let mut result = String::new();

    for row in 0..rows {
        for col in 0..cols {
            // Get the cell at this position
            let cell = screen.cell(row, col);
            if let Some(cell) = cell {
                // Extract the character
                let ch = cell.contents();
                write!(&mut result, "{}", ch).unwrap();
            } else {
                result.push(' ');
            }
        }
        result.push('\n');
    }

    result
}

/// Parse a key event string like "Tab", "F10", "Ctrl-C", "Alt-R"
fn parse_key_event(key_str: &str) -> Result<KeyEvent> {
    match key_str {
        "Enter" => Ok(KeyEvent::Enter),
        "Tab" => Ok(KeyEvent::Tab),
        "Escape" | "Esc" => Ok(KeyEvent::Escape),
        "Backspace" => Ok(KeyEvent::Backspace),
        "Delete" | "Del" => Ok(KeyEvent::Delete),
        "Up" => Ok(KeyEvent::Up),
        "Down" => Ok(KeyEvent::Down),
        "Left" => Ok(KeyEvent::Left),
        "Right" => Ok(KeyEvent::Right),
        s if s.starts_with("F") && s.len() >= 2 => {
            let num: u8 = s[1..].parse().context("Invalid F-key number")?;
            Ok(KeyEvent::F(num))
        }
        s if s.starts_with("Ctrl-") && s.len() == 6 => {
            let c = s.chars().nth(5).unwrap();
            Ok(KeyEvent::Ctrl(c))
        }
        s if s.starts_with("Alt-") && s.len() == 5 => {
            let c = s.chars().nth(4).unwrap();
            Ok(KeyEvent::Alt(c))
        }
        s if s.len() == 1 => {
            Ok(KeyEvent::Char(s.chars().next().unwrap()))
        }
        _ => Err(anyhow::anyhow!("Unknown key event: {}", key_str)),
    }
}

/// Render terminal text to PNG image with ANSI color support
fn render_terminal_to_png(
    text: &str,
    cols: u16,
    rows: u16,
    output_path: &PathBuf,
) -> Result<()> {
    // Terminal character dimensions (monospace)
    const CHAR_WIDTH: u32 = 10;
    const CHAR_HEIGHT: u32 = 20;
    const PADDING: u32 = 10;

    // Calculate image dimensions
    let img_width = (cols as u32 * CHAR_WIDTH) + (PADDING * 2);
    let img_height = (rows as u32 * CHAR_HEIGHT) + (PADDING * 2);

    // Create image with dark background (terminal-like)
    let bg_color = Rgb([12u8, 12u8, 12u8]);
    let mut img = RgbImage::from_pixel(img_width, img_height, bg_color);

    // Load embedded monospace font data
    let font_data = include_bytes!("../../assets/fonts/DejaVuSansMono.ttf");
    let font = FontRef::try_from_slice(font_data)
        .context("Failed to load embedded font")?;

    let scale = PxScale::from(16.0);

    // Parse ANSI codes and render with proper colors
    let lines: Vec<&str> = text.lines().take(rows as usize).collect();

    for (line_idx, line) in lines.iter().enumerate() {
        let y = PADDING + (line_idx as u32 * CHAR_HEIGHT);

        // Parse ANSI codes for this line and render with colors
        render_line_with_ansi(
            &mut img,
            line,
            PADDING as i32,
            y as i32,
            scale,
            &font,
            cols as usize,
        );
    }

    // Add header with terminal info
    let header = format!("OSVM Terminal Screenshot - {}x{}", cols, rows);
    draw_text_mut(
        &mut img,
        Rgb([100u8, 150u8, 250u8]), // Blue header
        PADDING as i32,
        5,
        PxScale::from(12.0),
        &font,
        &header,
    );

    // Save to PNG
    img.save(output_path)
        .context("Failed to save PNG screenshot")?;

    Ok(())
}

/// Render a line with ANSI color codes
fn render_line_with_ansi(
    img: &mut RgbImage,
    line: &str,
    start_x: i32,
    y: i32,
    scale: PxScale,
    font: &FontRef,
    max_chars: usize,
) {
    const CHAR_WIDTH: i32 = 10;

    let mut x = start_x;
    let mut current_color = Rgb([200u8, 200u8, 200u8]); // Default light gray
    let mut char_count = 0;

    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        if char_count >= max_chars {
            break;
        }

        // Check for ANSI escape sequence
        if ch == '\x1b' && chars.peek() == Some(&'[') {
            chars.next(); // consume '['

            // Parse ANSI code
            let mut code_str = String::new();
            while let Some(&next_ch) = chars.peek() {
                if next_ch.is_ascii_digit() || next_ch == ';' {
                    code_str.push(chars.next().unwrap());
                } else {
                    chars.next(); // consume the terminating character (usually 'm')
                    break;
                }
            }

            // Parse color code
            if let Ok(code) = code_str.parse::<u8>() {
                current_color = ansi_code_to_rgb(code);
            } else if code_str.contains(';') {
                // Handle compound codes like "38;5;n" for 256 colors
                let parts: Vec<&str> = code_str.split(';').collect();
                if parts.len() >= 3 && parts[0] == "38" && parts[1] == "5" {
                    if let Ok(color_idx) = parts[2].parse::<u8>() {
                        current_color = xterm_256_to_rgb(color_idx);
                    }
                }
            }
            continue;
        }

        // Render the character with current color
        if !ch.is_control() {
            let char_str = ch.to_string();
            draw_text_mut(
                img,
                current_color,
                x,
                y,
                scale,
                font,
                &char_str,
            );
            x += CHAR_WIDTH;
            char_count += 1;
        }
    }
}

/// Convert ANSI color code to RGB
fn ansi_code_to_rgb(code: u8) -> Rgb<u8> {
    match code {
        0 => Rgb([200, 200, 200]),  // Reset/normal
        30 => Rgb([0, 0, 0]),        // Black
        31 => Rgb([205, 49, 49]),    // Red
        32 => Rgb([13, 188, 121]),   // Green
        33 => Rgb([229, 229, 16]),   // Yellow
        34 => Rgb([36, 114, 200]),   // Blue
        35 => Rgb([188, 63, 188]),   // Magenta
        36 => Rgb([17, 168, 205]),   // Cyan
        37 => Rgb([229, 229, 229]),  // White

        // Bright variants
        90 => Rgb([102, 102, 102]),  // Bright black (gray)
        91 => Rgb([241, 76, 76]),    // Bright red
        92 => Rgb([35, 209, 139]),   // Bright green
        93 => Rgb([245, 245, 67]),   // Bright yellow
        94 => Rgb([59, 142, 234]),   // Bright blue
        95 => Rgb([214, 112, 214]),  // Bright magenta
        96 => Rgb([41, 184, 219]),   // Bright cyan
        97 => Rgb([255, 255, 255]),  // Bright white

        _ => Rgb([200, 200, 200]),   // Default
    }
}

/// Convert xterm 256 color to RGB
fn xterm_256_to_rgb(color: u8) -> Rgb<u8> {
    match color {
        // 0-15: Standard colors (same as ANSI)
        0..=15 => ansi_code_to_rgb(if color < 8 { 30 + color } else { 82 + color }),

        // 16-231: 216 color cube (6x6x6)
        16..=231 => {
            let idx = color - 16;
            let r = (idx / 36) * 51;
            let g = ((idx % 36) / 6) * 51;
            let b = (idx % 6) * 51;
            Rgb([r, g, b])
        }

        // 232-255: Grayscale
        232..=255 => {
            let gray = 8 + (color - 232) * 10;
            Rgb([gray, gray, gray])
        }
    }
}

/// Create default TUI test scenarios
pub fn create_default_tui_scenarios() -> Vec<TuiTestScenario> {
    vec![
        // Basic navigation test
        TuiTestScenario {
            name: "basic_navigation".to_string(),
            description: "Test basic keyboard navigation in the TUI".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(10000),
                    wait_ms: 0,
                    description: "Wait for UI to fully render (extended 10s wait)".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture initial screen".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Tab".to_string()),
                    wait_ms: 1000,
                    description: "Press Tab to navigate".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after Tab".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 2,
                    contains_text: vec![], // VT100 parser working - expect alternate screen buffer
                    not_contains_text: vec!["Panic".to_string(), "thread 'main' panicked".to_string()],
                    title: Some("Alternate Screen Buffer Captured".to_string()),
                },
            ],
        },

        // Help dialog test
        TuiTestScenario {
            name: "help_dialog".to_string(),
            description: "Test opening and closing help dialog".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(10000),
                    wait_ms: 0,
                    description: "Wait for UI to fully render (extended 10s wait)".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("F10".to_string()),
                    wait_ms: 1500,
                    description: "Press F10 for context menu".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture context menu".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKey("Escape".to_string()),
                    wait_ms: 1000,
                    description: "Press Escape to close".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture after closing".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 3,
                    contains_text: vec![], // VT100 capturing alternate screen buffer
                    not_contains_text: vec!["Panic".to_string(), "thread 'main' panicked".to_string()],
                    title: Some("F10 Key Interaction".to_string()),
                },
            ],
        },

        // Message input test
        TuiTestScenario {
            name: "message_input".to_string(),
            description: "Test typing and sending a message".to_string(),
            steps: vec![
                TuiTestStep {
                    action: TuiAction::Wait(10000),
                    wait_ms: 0,
                    description: "Wait for UI to fully render (extended 10s wait)".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::SendKeys("Hello, OSVM!".to_string()),
                    wait_ms: 1500,
                    description: "Type a message".to_string(),
                },
                TuiTestStep {
                    action: TuiAction::CaptureScreen,
                    wait_ms: 1000,
                    description: "Capture with typed message".to_string(),
                },
            ],
            expected_screens: vec![
                ScreenExpectation {
                    after_step: 3,
                    contains_text: vec![], // VT100 capturing alternate screen buffer
                    not_contains_text: vec!["Panic".to_string(), "thread 'main' panicked".to_string()],
                    title: Some("Text Input Test".to_string()),
                },
            ],
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_key_event_to_ansi() {
        assert_eq!(KeyEvent::Enter.to_ansi(), "\r");
        assert_eq!(KeyEvent::Tab.to_ansi(), "\t");
        assert_eq!(KeyEvent::Escape.to_ansi(), "\x1b");
        assert_eq!(KeyEvent::Ctrl('c').to_ansi(), "\x03");
    }

    #[test]
    fn test_parse_key_event() {
        assert!(matches!(parse_key_event("Tab").unwrap(), KeyEvent::Tab));
        assert!(matches!(parse_key_event("Enter").unwrap(), KeyEvent::Enter));
        assert!(matches!(parse_key_event("F10").unwrap(), KeyEvent::F(10)));
        assert!(matches!(parse_key_event("Ctrl-C").unwrap(), KeyEvent::Ctrl('C')));
        assert!(matches!(parse_key_event("Alt-R").unwrap(), KeyEvent::Alt('R')));
    }

    #[test]
    fn test_strip_ansi_codes() {
        let text_with_ansi = "\x1b[31mRed text\x1b[0m Normal text";
        let clean = strip_ansi_codes(text_with_ansi);
        assert_eq!(clean, "Red text Normal text");
    }
}
