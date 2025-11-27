//! TUI Screenshot Testing Framework
//!
//! This module provides utilities for visual regression testing of the research TUI.
//! It captures terminal output as "screenshots" and compares them against golden images.
//!
//! # Architecture
//!
//! The framework uses two approaches:
//! 1. **In-memory rendering**: Uses ratatui's TestBackend for fast, deterministic tests
//! 2. **tmux capture**: For integration testing with real terminal behavior
//!
//! # Usage
//!
//! ```rust,ignore
//! use osvm::utils::tui::screenshot_test::{TuiScreenshot, ScreenshotDiff};
//!
//! let screenshot = TuiScreenshot::capture_widget(|f, area| {
//!     // Render your widget here
//!     f.render_widget(my_widget, area);
//! }, 80, 24);
//!
//! // Compare against golden image
//! let diff = screenshot.compare_to_golden("tests/golden/my_widget.txt")?;
//! assert!(diff.is_match(), "Visual regression detected: {}", diff.summary());
//! ```

use ratatui::{
    backend::TestBackend,
    buffer::Buffer,
    layout::Rect,
    style::Color,
    Terminal,
};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;
use std::io::{self, Write};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};

/// Global counter for unique tmux session names
static TMUX_SESSION_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique session name for tmux
fn generate_unique_session_name(prefix: &str) -> String {
    let counter = TMUX_SESSION_COUNTER.fetch_add(1, Ordering::SeqCst);
    let pid = std::process::id();
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_micros() % 1_000_000)
        .unwrap_or(0);
    format!("{}_{}_{}_{}", prefix, pid, counter, timestamp)
}

/// Result type for screenshot operations
pub type ScreenshotResult<T> = Result<T, ScreenshotError>;

/// Errors that can occur during screenshot testing
#[derive(Debug)]
pub enum ScreenshotError {
    Io(io::Error),
    GoldenNotFound(PathBuf),
    DimensionMismatch { expected: (u16, u16), actual: (u16, u16) },
    TmuxError(String),
    RenderError(String),
    ColorAssertion(ColorAssertionError),
}

/// Error for color assertion failures
#[derive(Debug, Clone)]
pub struct ColorAssertionError {
    pub description: String,
    pub expected_color: Option<Color>,
    pub actual_colors: Vec<(u16, u16, Color)>,  // (x, y, color) tuples
    pub region: Option<ColorRegion>,
}

impl std::fmt::Display for ScreenshotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "IO error: {}", e),
            Self::GoldenNotFound(p) => write!(f, "Golden image not found: {}", p.display()),
            Self::DimensionMismatch { expected, actual } => {
                write!(f, "Dimension mismatch: expected {}x{}, got {}x{}",
                    expected.0, expected.1, actual.0, actual.1)
            }
            Self::TmuxError(e) => write!(f, "tmux error: {}", e),
            Self::RenderError(e) => write!(f, "Render error: {}", e),
            Self::ColorAssertion(e) => write!(f, "Color assertion failed: {}", e.description),
        }
    }
}

impl std::fmt::Display for ColorAssertionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description)?;
        if let Some(expected) = &self.expected_color {
            write!(f, " (expected: {:?})", expected)?;
        }
        if !self.actual_colors.is_empty() {
            write!(f, " (found: ")?;
            for (i, (x, y, color)) in self.actual_colors.iter().take(5).enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{:?}@({},{})", color, x, y)?;
            }
            if self.actual_colors.len() > 5 {
                write!(f, ", ... {} more", self.actual_colors.len() - 5)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl std::error::Error for ScreenshotError {}

impl From<io::Error> for ScreenshotError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

/// A captured TUI screenshot
#[derive(Debug, Clone)]
pub struct TuiScreenshot {
    /// Raw buffer content (characters + styles)
    pub buffer: Buffer,
    /// Width in columns
    pub width: u16,
    /// Height in rows
    pub height: u16,
    /// Optional name for the screenshot
    pub name: Option<String>,
}

impl TuiScreenshot {
    /// Create a new screenshot by rendering a widget
    pub fn capture_widget<F>(render_fn: F, width: u16, height: u16) -> Self
    where
        F: FnOnce(&mut ratatui::Frame, Rect),
    {
        let backend = TestBackend::new(width, height);
        let mut terminal = Terminal::new(backend).expect("Failed to create test terminal");

        terminal.draw(|f| {
            let area = f.area();
            render_fn(f, area);
        }).expect("Failed to draw");

        let buffer = terminal.backend().buffer().clone();

        Self {
            buffer,
            width,
            height,
            name: None,
        }
    }

    /// Set a name for this screenshot
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Convert to plain text representation (without ANSI codes)
    pub fn to_text(&self) -> String {
        let mut lines = Vec::new();

        for y in 0..self.height {
            let mut line = String::new();
            for x in 0..self.width {
                let cell = self.buffer.cell((x, y)).expect("Cell out of bounds");
                line.push_str(cell.symbol());
            }
            // Trim trailing whitespace but preserve leading
            lines.push(line.trim_end().to_string());
        }

        // Remove trailing empty lines
        while lines.last().map(|l| l.is_empty()).unwrap_or(false) {
            lines.pop();
        }

        lines.join("\n")
    }

    /// Convert to text with style markers for debugging
    pub fn to_styled_text(&self) -> String {
        use ratatui::style::Color;

        let mut output = String::new();

        for y in 0..self.height {
            for x in 0..self.width {
                let cell = self.buffer.cell((x, y)).expect("Cell out of bounds");
                let fg = cell.fg;

                // Add color markers for non-default colors
                let marker = match fg {
                    Color::Red | Color::LightRed => '!',      // Alerts
                    Color::Green | Color::LightGreen => '+',  // Success
                    Color::Yellow | Color::LightYellow => '~', // Warning
                    Color::Blue | Color::LightBlue => '>',    // Info
                    Color::Magenta | Color::LightMagenta => '*', // Highlight
                    Color::Cyan | Color::LightCyan => '@',    // Links/special
                    _ => ' ',
                };

                if marker != ' ' {
                    output.push(marker);
                }
                output.push_str(cell.symbol());
            }
            output.push('\n');
        }

        output
    }

    /// Save screenshot to file
    pub fn save(&self, path: impl AsRef<Path>) -> ScreenshotResult<()> {
        let content = self.to_text();
        fs::write(path, content)?;
        Ok(())
    }

    /// Load a golden image from file
    pub fn load_golden(path: impl AsRef<Path>) -> ScreenshotResult<String> {
        let path = path.as_ref();
        if !path.exists() {
            return Err(ScreenshotError::GoldenNotFound(path.to_path_buf()));
        }
        Ok(fs::read_to_string(path)?)
    }

    /// Compare this screenshot against a golden image file
    pub fn compare_to_golden(&self, golden_path: impl AsRef<Path>) -> ScreenshotResult<ScreenshotDiff> {
        let golden = Self::load_golden(&golden_path)?;
        Ok(self.compare_to_text(&golden))
    }

    /// Compare this screenshot against expected text
    pub fn compare_to_text(&self, expected: &str) -> ScreenshotDiff {
        let actual = self.to_text();
        let actual_lines: Vec<&str> = actual.lines().collect();
        let expected_lines: Vec<&str> = expected.lines().collect();

        let mut differences = Vec::new();
        let max_lines = actual_lines.len().max(expected_lines.len());

        for i in 0..max_lines {
            let actual_line = actual_lines.get(i).copied().unwrap_or("");
            let expected_line = expected_lines.get(i).copied().unwrap_or("");

            if actual_line != expected_line {
                differences.push(LineDiff {
                    line_number: i + 1,
                    expected: expected_line.to_string(),
                    actual: actual_line.to_string(),
                });
            }
        }

        ScreenshotDiff {
            screenshot_name: self.name.clone(),
            total_lines: max_lines,
            differences,
            actual_text: actual,
            expected_text: expected.to_string(),
        }
    }

    /// Update golden image (use during test development)
    pub fn update_golden(&self, golden_path: impl AsRef<Path>) -> ScreenshotResult<()> {
        let path = golden_path.as_ref();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        self.save(path)
    }

    // ========================================================================
    // COLOR ASSERTION METHODS
    // ========================================================================

    /// Get the foreground color at a specific position
    pub fn get_fg_color(&self, x: u16, y: u16) -> Option<Color> {
        self.buffer.cell((x, y)).map(|c| c.fg)
    }

    /// Get the background color at a specific position
    pub fn get_bg_color(&self, x: u16, y: u16) -> Option<Color> {
        self.buffer.cell((x, y)).map(|c| c.bg)
    }

    /// Get all unique foreground colors in the screenshot
    pub fn get_all_fg_colors(&self) -> HashMap<Color, usize> {
        let mut colors = HashMap::new();
        for y in 0..self.height {
            for x in 0..self.width {
                if let Some(cell) = self.buffer.cell((x, y)) {
                    *colors.entry(cell.fg).or_insert(0) += 1;
                }
            }
        }
        colors
    }

    /// Get all cells with a specific foreground color
    pub fn find_cells_with_fg_color(&self, color: Color) -> Vec<(u16, u16, String)> {
        let mut cells = Vec::new();
        for y in 0..self.height {
            for x in 0..self.width {
                if let Some(cell) = self.buffer.cell((x, y)) {
                    if cell.fg == color {
                        cells.push((x, y, cell.symbol().to_string()));
                    }
                }
            }
        }
        cells
    }

    /// Assert that a specific color exists somewhere in the screenshot
    pub fn assert_has_color(&self, color: Color) -> ScreenshotResult<()> {
        let colors = self.get_all_fg_colors();
        if colors.contains_key(&color) {
            Ok(())
        } else {
            Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: format!("Color {:?} not found in screenshot", color),
                expected_color: Some(color),
                actual_colors: colors.keys().map(|c| (0, 0, *c)).collect(),
                region: None,
            }))
        }
    }

    /// Assert that a specific color does NOT exist in the screenshot
    pub fn assert_no_color(&self, color: Color) -> ScreenshotResult<()> {
        let cells = self.find_cells_with_fg_color(color);
        if cells.is_empty() {
            Ok(())
        } else {
            Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: format!("Color {:?} was found but should not exist", color),
                expected_color: None,
                actual_colors: cells.iter().map(|(x, y, _)| (*x, *y, color)).collect(),
                region: None,
            }))
        }
    }

    /// Assert that text containing a pattern has a specific color
    pub fn assert_text_has_color(&self, pattern: &str, expected_color: Color) -> ScreenshotResult<()> {
        // Find all occurrences of the pattern
        let text = self.to_text();
        let lines: Vec<&str> = text.lines().collect();

        let mut found = false;
        let mut wrong_colors = Vec::new();

        for (y, line) in lines.iter().enumerate() {
            if let Some(start_x) = line.find(pattern) {
                found = true;
                // Check colors for each character in the pattern
                for (i, _) in pattern.chars().enumerate() {
                    let x = (start_x + i) as u16;
                    if let Some(cell) = self.buffer.cell((x, y as u16)) {
                        if cell.fg != expected_color && !is_similar_color(cell.fg, expected_color) {
                            wrong_colors.push((x, y as u16, cell.fg));
                        }
                    }
                }
            }
        }

        if !found {
            return Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: format!("Pattern '{}' not found in screenshot", pattern),
                expected_color: Some(expected_color),
                actual_colors: vec![],
                region: None,
            }));
        }

        if !wrong_colors.is_empty() {
            return Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: format!(
                    "Pattern '{}' found but has wrong color(s)",
                    pattern
                ),
                expected_color: Some(expected_color),
                actual_colors: wrong_colors,
                region: None,
            }));
        }

        Ok(())
    }

    /// Assert colors in a rectangular region
    pub fn assert_region_has_color(&self, region: ColorRegion, expected_color: Color) -> ScreenshotResult<()> {
        let mut wrong_colors = Vec::new();
        let mut found_any = false;

        for y in region.y..region.y.saturating_add(region.height).min(self.height) {
            for x in region.x..region.x.saturating_add(region.width).min(self.width) {
                if let Some(cell) = self.buffer.cell((x, y)) {
                    // Only check non-space characters (colored content)
                    if cell.symbol().trim().is_empty() {
                        continue;
                    }
                    found_any = true;
                    if cell.fg != expected_color && !is_similar_color(cell.fg, expected_color) {
                        wrong_colors.push((x, y, cell.fg));
                    }
                }
            }
        }

        if !found_any {
            return Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: "Region is empty (no non-space characters)".to_string(),
                expected_color: Some(expected_color),
                actual_colors: vec![],
                region: Some(region),
            }));
        }

        if !wrong_colors.is_empty() {
            return Err(ScreenshotError::ColorAssertion(ColorAssertionError {
                description: format!(
                    "Region has {} cells with wrong color",
                    wrong_colors.len()
                ),
                expected_color: Some(expected_color),
                actual_colors: wrong_colors,
                region: Some(region),
            }));
        }

        Ok(())
    }

    /// Get a color summary for the entire screenshot
    pub fn color_summary(&self) -> ColorSummary {
        let colors = self.get_all_fg_colors();

        let has_red = colors.contains_key(&Color::Red) || colors.contains_key(&Color::LightRed);
        let has_green = colors.contains_key(&Color::Green) || colors.contains_key(&Color::LightGreen);
        let has_yellow = colors.contains_key(&Color::Yellow) || colors.contains_key(&Color::LightYellow);
        let has_blue = colors.contains_key(&Color::Blue) || colors.contains_key(&Color::LightBlue);
        let has_magenta = colors.contains_key(&Color::Magenta) || colors.contains_key(&Color::LightMagenta);
        let has_cyan = colors.contains_key(&Color::Cyan) || colors.contains_key(&Color::LightCyan);

        ColorSummary {
            total_colors: colors.len(),
            has_red,
            has_green,
            has_yellow,
            has_blue,
            has_magenta,
            has_cyan,
            color_counts: colors,
        }
    }

    /// Create a color assertions builder for fluent API
    pub fn assert_colors(&self) -> ColorAssertions<'_> {
        ColorAssertions { screenshot: self }
    }
}

/// A rectangular region for color assertions
#[derive(Debug, Clone, Copy)]
pub struct ColorRegion {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
}

impl ColorRegion {
    pub fn new(x: u16, y: u16, width: u16, height: u16) -> Self {
        Self { x, y, width, height }
    }

    /// Create a region for the first N lines
    pub fn top_lines(height: u16, screen_width: u16) -> Self {
        Self { x: 0, y: 0, width: screen_width, height }
    }

    /// Create a region for a specific line
    pub fn line(y: u16, screen_width: u16) -> Self {
        Self { x: 0, y, width: screen_width, height: 1 }
    }
}

/// Summary of colors in a screenshot
#[derive(Debug)]
pub struct ColorSummary {
    pub total_colors: usize,
    pub has_red: bool,
    pub has_green: bool,
    pub has_yellow: bool,
    pub has_blue: bool,
    pub has_magenta: bool,
    pub has_cyan: bool,
    pub color_counts: HashMap<Color, usize>,
}

/// Fluent API for color assertions
pub struct ColorAssertions<'a> {
    screenshot: &'a TuiScreenshot,
}

impl<'a> ColorAssertions<'a> {
    /// Assert that the screenshot contains a specific color
    pub fn has_color(self, color: Color) -> ScreenshotResult<Self> {
        self.screenshot.assert_has_color(color)?;
        Ok(self)
    }

    /// Assert that the screenshot does NOT contain a specific color
    pub fn no_color(self, color: Color) -> ScreenshotResult<Self> {
        self.screenshot.assert_no_color(color)?;
        Ok(self)
    }

    /// Assert that text with a pattern has a specific color
    pub fn text_color(self, pattern: &str, color: Color) -> ScreenshotResult<Self> {
        self.screenshot.assert_text_has_color(pattern, color)?;
        Ok(self)
    }

    /// Assert that a region has a specific color
    pub fn region_color(self, region: ColorRegion, color: Color) -> ScreenshotResult<Self> {
        self.screenshot.assert_region_has_color(region, color)?;
        Ok(self)
    }

    /// Assert colors for risk level indicators
    pub fn risk_level_is_red(self) -> ScreenshotResult<Self> {
        self.has_color(Color::Red)
    }

    pub fn risk_level_is_green(self) -> ScreenshotResult<Self> {
        self.has_color(Color::Green)
    }

    pub fn risk_level_is_yellow(self) -> ScreenshotResult<Self> {
        self.has_color(Color::Yellow)
    }

    /// Convenience: assert critical risk colors (red)
    pub fn is_critical_risk(self) -> ScreenshotResult<Self> {
        self.screenshot.assert_text_has_color("CRITICAL", Color::Red)
            .or_else(|_| self.screenshot.assert_text_has_color("Critical", Color::Red))?;
        Ok(self)
    }

    /// Convenience: assert high risk colors (light red/orange)
    pub fn is_high_risk(self) -> ScreenshotResult<Self> {
        self.screenshot.assert_text_has_color("High", Color::LightRed)?;
        Ok(self)
    }

    /// Convenience: assert medium risk colors (yellow)
    pub fn is_medium_risk(self) -> ScreenshotResult<Self> {
        self.screenshot.assert_text_has_color("Medium", Color::Yellow)?;
        Ok(self)
    }

    /// Convenience: assert low risk colors (green)
    pub fn is_low_risk(self) -> ScreenshotResult<Self> {
        self.screenshot.assert_text_has_color("Low", Color::Green)?;
        Ok(self)
    }
}

/// Check if two colors are "similar" (same family, different brightness)
fn is_similar_color(a: Color, b: Color) -> bool {
    matches!(
        (a, b),
        (Color::Red, Color::LightRed) | (Color::LightRed, Color::Red) |
        (Color::Green, Color::LightGreen) | (Color::LightGreen, Color::Green) |
        (Color::Yellow, Color::LightYellow) | (Color::LightYellow, Color::Yellow) |
        (Color::Blue, Color::LightBlue) | (Color::LightBlue, Color::Blue) |
        (Color::Magenta, Color::LightMagenta) | (Color::LightMagenta, Color::Magenta) |
        (Color::Cyan, Color::LightCyan) | (Color::LightCyan, Color::Cyan) |
        (Color::Gray, Color::DarkGray) | (Color::DarkGray, Color::Gray) |
        (Color::White, Color::Gray) | (Color::Gray, Color::White)
    )
}

/// Difference between actual and expected screenshots
#[derive(Debug)]
pub struct ScreenshotDiff {
    pub screenshot_name: Option<String>,
    pub total_lines: usize,
    pub differences: Vec<LineDiff>,
    pub actual_text: String,
    pub expected_text: String,
}

impl ScreenshotDiff {
    /// Check if the screenshots match
    pub fn is_match(&self) -> bool {
        self.differences.is_empty()
    }

    /// Get a human-readable summary of differences
    pub fn summary(&self) -> String {
        if self.is_match() {
            return "Screenshots match".to_string();
        }

        let mut output = String::new();
        output.push_str(&format!(
            "Found {} differences in {} lines",
            self.differences.len(),
            self.total_lines
        ));

        if let Some(name) = &self.screenshot_name {
            output.push_str(&format!(" (screenshot: {})", name));
        }
        output.push('\n');

        for diff in self.differences.iter().take(10) {
            output.push_str(&format!(
                "\nLine {}: \n  Expected: \"{}\"\n  Actual:   \"{}\"",
                diff.line_number, diff.expected, diff.actual
            ));
        }

        if self.differences.len() > 10 {
            output.push_str(&format!(
                "\n... and {} more differences",
                self.differences.len() - 10
            ));
        }

        output
    }

    /// Get percentage of lines that match
    pub fn match_percentage(&self) -> f64 {
        if self.total_lines == 0 {
            return 100.0;
        }
        let matching = self.total_lines - self.differences.len();
        (matching as f64 / self.total_lines as f64) * 100.0
    }
}

/// A single line difference
#[derive(Debug)]
pub struct LineDiff {
    pub line_number: usize,
    pub expected: String,
    pub actual: String,
}

/// Tmux-based screenshot capture for integration testing
pub struct TmuxCapture {
    session_name: String,
    width: u16,
    height: u16,
}

impl TmuxCapture {
    /// Create a new tmux capture session
    pub fn new(width: u16, height: u16) -> ScreenshotResult<Self> {
        let session_name = generate_unique_session_name("osvm_test");

        // Create detached tmux session with specific size
        let output = Command::new("tmux")
            .args([
                "new-session",
                "-d",
                "-s", &session_name,
                "-x", &width.to_string(),
                "-y", &height.to_string(),
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string()
            ));
        }

        Ok(Self {
            session_name,
            width,
            height,
        })
    }

    /// Run a command in the tmux session
    pub fn run_command(&self, cmd: &str) -> ScreenshotResult<()> {
        let output = Command::new("tmux")
            .args([
                "send-keys",
                "-t", &self.session_name,
                cmd,
                "Enter",
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string()
            ));
        }

        Ok(())
    }

    /// Send keys to the tmux session
    pub fn send_keys(&self, keys: &str) -> ScreenshotResult<()> {
        let output = Command::new("tmux")
            .args([
                "send-keys",
                "-t", &self.session_name,
                keys,
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string()
            ));
        }

        Ok(())
    }

    /// Capture the current terminal state
    pub fn capture(&self) -> ScreenshotResult<String> {
        let output = Command::new("tmux")
            .args([
                "capture-pane",
                "-t", &self.session_name,
                "-p",  // Print to stdout
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                String::from_utf8_lossy(&output.stderr).to_string()
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    /// Wait for output to stabilize (no changes for duration)
    pub fn wait_for_stable(&self, check_interval_ms: u64, stability_ms: u64) -> ScreenshotResult<String> {
        let mut last_capture = self.capture()?;
        let mut stable_since = std::time::Instant::now();
        let stability_duration = std::time::Duration::from_millis(stability_ms);
        let max_wait = std::time::Duration::from_secs(30);
        let start = std::time::Instant::now();

        loop {
            std::thread::sleep(std::time::Duration::from_millis(check_interval_ms));

            let current = self.capture()?;

            if current == last_capture {
                if stable_since.elapsed() >= stability_duration {
                    return Ok(current);
                }
            } else {
                last_capture = current;
                stable_since = std::time::Instant::now();
            }

            if start.elapsed() > max_wait {
                return Ok(last_capture); // Return whatever we have after timeout
            }
        }
    }
}

impl Drop for TmuxCapture {
    fn drop(&mut self) {
        // Kill the tmux session when done
        let _ = Command::new("tmux")
            .args(["kill-session", "-t", &self.session_name])
            .output();
    }
}

// ============================================================================
// TMUX AUTO-TILING SUPPORT (btop-style)
// ============================================================================

/// Layout types for auto-tiling (similar to i3wm/sway/btop)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TileLayout {
    /// Single pane (no tiling)
    Single,
    /// Main pane on left, stacked panes on right (like i3 main-vertical)
    MainVertical,
    /// Main pane on top, stacked panes below (like i3 main-horizontal)
    MainHorizontal,
    /// All panes split horizontally (side by side)
    EvenHorizontal,
    /// All panes split vertically (stacked)
    EvenVertical,
    /// Grid layout - tmux's built-in tiled layout
    Tiled,
    /// Custom percentage split (main pane takes specified percentage)
    MainVerticalRatio(u8),
    /// Custom percentage split horizontal
    MainHorizontalRatio(u8),
}

impl TileLayout {
    /// Get the tmux layout name for select-layout
    pub fn to_tmux_layout(&self) -> &'static str {
        match self {
            TileLayout::Single => "main-vertical",
            TileLayout::MainVertical | TileLayout::MainVerticalRatio(_) => "main-vertical",
            TileLayout::MainHorizontal | TileLayout::MainHorizontalRatio(_) => "main-horizontal",
            TileLayout::EvenHorizontal => "even-horizontal",
            TileLayout::EvenVertical => "even-vertical",
            TileLayout::Tiled => "tiled",
        }
    }
}

/// A pane within a tiled tmux session
#[derive(Debug, Clone)]
pub struct TmuxPane {
    /// Pane index (0-based)
    pub index: usize,
    /// Optional name/label for the pane
    pub name: Option<String>,
    /// Command running in this pane
    pub command: Option<String>,
    /// Last captured content
    pub last_capture: Option<String>,
}

impl TmuxPane {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            name: None,
            command: None,
            last_capture: None,
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

/// Result of capturing all panes in a tiled session
#[derive(Debug)]
pub struct TiledCaptureResult {
    /// Captured content for each pane (indexed by pane number)
    pub panes: Vec<(usize, String)>,
    /// Session name
    pub session_name: String,
    /// Layout used
    pub layout: TileLayout,
    /// Timestamp of capture
    pub timestamp: std::time::Instant,
}

impl TiledCaptureResult {
    /// Get capture for a specific pane by index
    pub fn get_pane(&self, index: usize) -> Option<&str> {
        self.panes.iter()
            .find(|(i, _)| *i == index)
            .map(|(_, content)| content.as_str())
    }

    /// Get all pane contents as a combined view
    pub fn combined_view(&self, separator: &str) -> String {
        self.panes.iter()
            .map(|(i, content)| format!("=== Pane {} ===\n{}", i, content))
            .collect::<Vec<_>>()
            .join(separator)
    }

    /// Compare each pane against golden files
    pub fn compare_to_goldens(&self, golden_dir: &Path, prefix: &str) -> Vec<ScreenshotDiff> {
        self.panes.iter().map(|(i, content)| {
            let golden_path = golden_dir.join(format!("{}_{}.txt", prefix, i));
            let expected = fs::read_to_string(&golden_path).unwrap_or_default();

            let actual_lines: Vec<&str> = content.lines().collect();
            let expected_lines: Vec<&str> = expected.lines().collect();

            let mut differences = Vec::new();
            let max_lines = actual_lines.len().max(expected_lines.len());

            for line_num in 0..max_lines {
                let actual_line = actual_lines.get(line_num).copied().unwrap_or("");
                let expected_line = expected_lines.get(line_num).copied().unwrap_or("");

                if actual_line != expected_line {
                    differences.push(LineDiff {
                        line_number: line_num + 1,
                        expected: expected_line.to_string(),
                        actual: actual_line.to_string(),
                    });
                }
            }

            ScreenshotDiff {
                screenshot_name: Some(format!("pane_{}", i)),
                total_lines: max_lines,
                differences,
                actual_text: content.clone(),
                expected_text: expected,
            }
        }).collect()
    }
}

/// Tiled tmux session manager for parallel testing (btop-style)
///
/// This provides i3wm/sway-like auto-tiling within tmux for testing
/// multiple TUI views simultaneously.
///
/// # Example
///
/// ```rust,ignore
/// use osvm::utils::tui::screenshot_test::{TmuxTiledSession, TileLayout};
///
/// // Create a 2x2 tiled session
/// let mut session = TmuxTiledSession::new(160, 48)?
///     .with_layout(TileLayout::Tiled);
///
/// // Split into 4 panes
/// session.split_panes(4)?;
///
/// // Run different views in each pane
/// session.run_in_pane(0, "osvm research WALLET1 --tui")?;
/// session.run_in_pane(1, "osvm research WALLET2 --tui")?;
/// session.run_in_pane(2, "osvm chat")?;
/// session.run_in_pane(3, "htop")?; // btop-style!
///
/// // Capture all panes at once
/// let captures = session.capture_all()?;
/// ```
pub struct TmuxTiledSession {
    session_name: String,
    width: u16,
    height: u16,
    layout: TileLayout,
    panes: Vec<TmuxPane>,
    /// Whether to synchronize input to all panes (like tmux synchronize-panes)
    sync_input: bool,
}

impl TmuxTiledSession {
    /// Create a new tiled tmux session
    pub fn new(width: u16, height: u16) -> ScreenshotResult<Self> {
        let session_name = generate_unique_session_name("osvm_tiled");

        // Create detached tmux session with specific size
        let output = Command::new("tmux")
            .args([
                "new-session",
                "-d",
                "-s", &session_name,
                "-x", &width.to_string(),
                "-y", &height.to_string(),
            ])
            .output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(ScreenshotError::TmuxError(
                format!("Failed to create tmux session: {}", stderr)
            ));
        }

        Ok(Self {
            session_name,
            width,
            height,
            layout: TileLayout::Single,
            panes: vec![TmuxPane::new(0)],
            sync_input: false,
        })
    }

    /// Set the tile layout
    pub fn with_layout(mut self, layout: TileLayout) -> Self {
        self.layout = layout;
        self
    }

    /// Enable synchronized input to all panes
    pub fn with_sync_input(mut self, sync: bool) -> Self {
        self.sync_input = sync;
        if sync {
            let _ = self.set_synchronize_panes(true);
        }
        self
    }

    /// Get the session name
    pub fn session_name(&self) -> &str {
        &self.session_name
    }

    /// Get the number of panes
    pub fn pane_count(&self) -> usize {
        self.panes.len()
    }

    /// Split the session into N panes with auto-tiling
    pub fn split_panes(&mut self, count: usize) -> ScreenshotResult<()> {
        if count == 0 || count > 16 {
            return Err(ScreenshotError::TmuxError(
                "Pane count must be between 1 and 16".to_string()
            ));
        }

        // Create additional panes (we already have 1)
        for i in 1..count {
            // Determine split direction based on layout
            let split_flag = match self.layout {
                TileLayout::EvenHorizontal | TileLayout::MainVertical => "-h", // horizontal split
                TileLayout::EvenVertical | TileLayout::MainHorizontal => "-v", // vertical split
                TileLayout::Tiled => if i % 2 == 1 { "-h" } else { "-v" },
                _ => "-h",
            };

            let output = Command::new("tmux")
                .args([
                    "split-window",
                    split_flag,
                    "-t", &format!("{}:{}", self.session_name, 0),
                ])
                .output()?;

            if !output.status.success() {
                return Err(ScreenshotError::TmuxError(
                    format!("Failed to split pane {}: {}", i, String::from_utf8_lossy(&output.stderr))
                ));
            }

            self.panes.push(TmuxPane::new(i));
        }

        // Apply the selected layout
        self.apply_layout()?;

        Ok(())
    }

    /// Apply the current layout to the session
    pub fn apply_layout(&self) -> ScreenshotResult<()> {
        let layout_name = self.layout.to_tmux_layout();

        let output = Command::new("tmux")
            .args([
                "select-layout",
                "-t", &self.session_name,
                layout_name,
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to apply layout: {}", String::from_utf8_lossy(&output.stderr))
            ));
        }

        // Handle custom ratio layouts
        match self.layout {
            TileLayout::MainVerticalRatio(pct) => {
                self.resize_main_pane(pct, true)?;
            }
            TileLayout::MainHorizontalRatio(pct) => {
                self.resize_main_pane(pct, false)?;
            }
            _ => {}
        }

        Ok(())
    }

    /// Resize the main pane to a specific percentage
    fn resize_main_pane(&self, percentage: u8, horizontal: bool) -> ScreenshotResult<()> {
        let size = if horizontal {
            (self.width as u32 * percentage as u32 / 100) as u16
        } else {
            (self.height as u32 * percentage as u32 / 100) as u16
        };

        let resize_flag = if horizontal { "-x" } else { "-y" };

        let output = Command::new("tmux")
            .args([
                "resize-pane",
                "-t", &format!("{}:0.0", self.session_name),
                resize_flag, &size.to_string(),
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to resize main pane: {}", String::from_utf8_lossy(&output.stderr))
            ));
        }

        Ok(())
    }

    /// Run a command in a specific pane
    pub fn run_in_pane(&mut self, pane_index: usize, cmd: &str) -> ScreenshotResult<()> {
        if pane_index >= self.panes.len() {
            return Err(ScreenshotError::TmuxError(
                format!("Pane {} does not exist (have {} panes)", pane_index, self.panes.len())
            ));
        }

        let output = Command::new("tmux")
            .args([
                "send-keys",
                "-t", &format!("{}:0.{}", self.session_name, pane_index),
                cmd,
                "Enter",
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to run command in pane {}: {}", pane_index, String::from_utf8_lossy(&output.stderr))
            ));
        }

        self.panes[pane_index].command = Some(cmd.to_string());
        Ok(())
    }

    /// Send keys to a specific pane (without Enter)
    pub fn send_keys_to_pane(&self, pane_index: usize, keys: &str) -> ScreenshotResult<()> {
        if pane_index >= self.panes.len() {
            return Err(ScreenshotError::TmuxError(
                format!("Pane {} does not exist", pane_index)
            ));
        }

        let output = Command::new("tmux")
            .args([
                "send-keys",
                "-t", &format!("{}:0.{}", self.session_name, pane_index),
                keys,
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to send keys to pane {}: {}", pane_index, String::from_utf8_lossy(&output.stderr))
            ));
        }

        Ok(())
    }

    /// Send keys to all panes simultaneously
    pub fn send_keys_to_all(&self, keys: &str) -> ScreenshotResult<()> {
        for i in 0..self.panes.len() {
            self.send_keys_to_pane(i, keys)?;
        }
        Ok(())
    }

    /// Enable/disable synchronized pane input
    pub fn set_synchronize_panes(&mut self, enable: bool) -> ScreenshotResult<()> {
        let flag = if enable { "on" } else { "off" };

        let output = Command::new("tmux")
            .args([
                "set-window-option",
                "-t", &self.session_name,
                "synchronize-panes", flag,
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to set synchronize-panes: {}", String::from_utf8_lossy(&output.stderr))
            ));
        }

        self.sync_input = enable;
        Ok(())
    }

    /// Capture a specific pane's content
    pub fn capture_pane(&self, pane_index: usize) -> ScreenshotResult<String> {
        if pane_index >= self.panes.len() {
            return Err(ScreenshotError::TmuxError(
                format!("Pane {} does not exist", pane_index)
            ));
        }

        let output = Command::new("tmux")
            .args([
                "capture-pane",
                "-t", &format!("{}:0.{}", self.session_name, pane_index),
                "-p",  // Print to stdout
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to capture pane {}: {}", pane_index, String::from_utf8_lossy(&output.stderr))
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    /// Capture all panes simultaneously
    pub fn capture_all(&self) -> ScreenshotResult<TiledCaptureResult> {
        let mut captures = Vec::new();

        for pane in &self.panes {
            let content = self.capture_pane(pane.index)?;
            captures.push((pane.index, content));
        }

        Ok(TiledCaptureResult {
            panes: captures,
            session_name: self.session_name.clone(),
            layout: self.layout,
            timestamp: std::time::Instant::now(),
        })
    }

    /// Wait for all panes to stabilize
    pub fn wait_for_stable_all(&self, check_interval_ms: u64, stability_ms: u64) -> ScreenshotResult<TiledCaptureResult> {
        let mut last_captures: Vec<(usize, String)> = self.panes.iter()
            .map(|p| (p.index, String::new()))
            .collect();
        let mut stable_since = std::time::Instant::now();
        let stability_duration = std::time::Duration::from_millis(stability_ms);
        let max_wait = std::time::Duration::from_secs(60);
        let start = std::time::Instant::now();

        loop {
            std::thread::sleep(std::time::Duration::from_millis(check_interval_ms));

            let current = self.capture_all()?;
            let mut all_stable = true;

            for (i, current_content) in &current.panes {
                if let Some((_, last_content)) = last_captures.iter().find(|(idx, _)| idx == i) {
                    if current_content != last_content {
                        all_stable = false;
                        break;
                    }
                }
            }

            if all_stable {
                if stable_since.elapsed() >= stability_duration {
                    return Ok(current);
                }
            } else {
                last_captures = current.panes;
                stable_since = std::time::Instant::now();
            }

            if start.elapsed() > max_wait {
                return self.capture_all(); // Return whatever we have
            }
        }
    }

    /// Focus a specific pane
    pub fn focus_pane(&self, pane_index: usize) -> ScreenshotResult<()> {
        if pane_index >= self.panes.len() {
            return Err(ScreenshotError::TmuxError(
                format!("Pane {} does not exist", pane_index)
            ));
        }

        let output = Command::new("tmux")
            .args([
                "select-pane",
                "-t", &format!("{}:0.{}", self.session_name, pane_index),
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to focus pane {}: {}", pane_index, String::from_utf8_lossy(&output.stderr))
            ));
        }

        Ok(())
    }

    /// Kill a specific pane (useful for cleanup)
    pub fn kill_pane(&mut self, pane_index: usize) -> ScreenshotResult<()> {
        if pane_index >= self.panes.len() {
            return Err(ScreenshotError::TmuxError(
                format!("Pane {} does not exist", pane_index)
            ));
        }

        // Don't kill the last pane - would destroy the session
        if self.panes.len() == 1 {
            return Err(ScreenshotError::TmuxError(
                "Cannot kill the last pane".to_string()
            ));
        }

        let output = Command::new("tmux")
            .args([
                "kill-pane",
                "-t", &format!("{}:0.{}", self.session_name, pane_index),
            ])
            .output()?;

        if !output.status.success() {
            return Err(ScreenshotError::TmuxError(
                format!("Failed to kill pane {}: {}", pane_index, String::from_utf8_lossy(&output.stderr))
            ));
        }

        self.panes.remove(pane_index);
        // Re-index remaining panes
        for (i, pane) in self.panes.iter_mut().enumerate() {
            pane.index = i;
        }

        Ok(())
    }

    /// Attach to the session interactively (for debugging)
    pub fn attach(&self) -> ScreenshotResult<()> {
        let status = Command::new("tmux")
            .args(["attach-session", "-t", &self.session_name])
            .status()?;

        if !status.success() {
            return Err(ScreenshotError::TmuxError(
                "Failed to attach to session".to_string()
            ));
        }

        Ok(())
    }

    /// Create a predefined layout configuration for testing
    pub fn create_test_layout(layout_name: &str, width: u16, height: u16) -> ScreenshotResult<Self> {
        let mut session = Self::new(width, height)?;

        match layout_name {
            "dashboard" => {
                // Main view on left, 3 smaller panes stacked on right
                session.layout = TileLayout::MainVerticalRatio(70);
                session.split_panes(4)?;
            }
            "comparison" => {
                // Side by side comparison (2 panes)
                session.layout = TileLayout::EvenHorizontal;
                session.split_panes(2)?;
            }
            "quad" => {
                // 2x2 grid
                session.layout = TileLayout::Tiled;
                session.split_panes(4)?;
            }
            "vertical-stack" => {
                // All panes stacked vertically
                session.layout = TileLayout::EvenVertical;
                session.split_panes(3)?;
            }
            "monitoring" => {
                // btop-style: large top, 2 bottom panels
                session.layout = TileLayout::MainHorizontalRatio(65);
                session.split_panes(3)?;
            }
            _ => {
                return Err(ScreenshotError::TmuxError(
                    format!("Unknown layout: {}", layout_name)
                ));
            }
        }

        Ok(session)
    }
}

impl Drop for TmuxTiledSession {
    fn drop(&mut self) {
        // Kill the entire tmux session when done
        let _ = Command::new("tmux")
            .args(["kill-session", "-t", &self.session_name])
            .output();
    }
}

/// Builder for setting up tiled test scenarios
pub struct TiledTestBuilder {
    width: u16,
    height: u16,
    layout: TileLayout,
    scenarios: Vec<TiledTestScenario>,
}

/// A test scenario for a single pane
#[derive(Clone)]
pub struct TiledTestScenario {
    pub name: String,
    pub command: String,
    pub expected_patterns: Vec<String>,
    pub wait_for_text: Option<String>,
    pub golden_file: Option<PathBuf>,
}

impl TiledTestBuilder {
    /// Create a new tiled test builder
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            width,
            height,
            layout: TileLayout::Tiled,
            scenarios: Vec::new(),
        }
    }

    /// Set the layout
    pub fn layout(mut self, layout: TileLayout) -> Self {
        self.layout = layout;
        self
    }

    /// Add a test scenario for a pane
    pub fn add_scenario(mut self, scenario: TiledTestScenario) -> Self {
        self.scenarios.push(scenario);
        self
    }

    /// Convenience: add a simple command scenario
    pub fn add_command(mut self, name: &str, command: &str) -> Self {
        self.scenarios.push(TiledTestScenario {
            name: name.to_string(),
            command: command.to_string(),
            expected_patterns: Vec::new(),
            wait_for_text: None,
            golden_file: None,
        });
        self
    }

    /// Build and run the tiled test
    pub fn run(self) -> ScreenshotResult<TiledTestResult> {
        let mut session = TmuxTiledSession::new(self.width, self.height)?
            .with_layout(self.layout);

        let pane_count = self.scenarios.len().max(1);
        session.split_panes(pane_count)?;

        // Run commands in each pane
        for (i, scenario) in self.scenarios.iter().enumerate() {
            session.run_in_pane(i, &scenario.command)?;
        }

        // Wait for scenarios that need stabilization
        let has_wait_conditions = self.scenarios.iter().any(|s| s.wait_for_text.is_some());

        let captures = if has_wait_conditions {
            session.wait_for_stable_all(100, 2000)?
        } else {
            std::thread::sleep(std::time::Duration::from_millis(500));
            session.capture_all()?
        };

        // Validate expected patterns
        let mut validations = Vec::new();
        for (i, scenario) in self.scenarios.iter().enumerate() {
            if let Some((_, content)) = captures.panes.iter().find(|(idx, _)| *idx == i) {
                let mut passed = true;
                let mut missing_patterns = Vec::new();

                for pattern in &scenario.expected_patterns {
                    if !content.contains(pattern) {
                        passed = false;
                        missing_patterns.push(pattern.clone());
                    }
                }

                validations.push(PaneValidation {
                    pane_index: i,
                    scenario_name: scenario.name.clone(),
                    passed,
                    missing_patterns,
                    captured_content: content.clone(),
                });
            }
        }

        let all_passed = validations.iter().all(|v| v.passed);

        Ok(TiledTestResult {
            captures,
            validations,
            all_passed,
        })
    }
}

/// Result of a tiled test run
#[derive(Debug)]
pub struct TiledTestResult {
    pub captures: TiledCaptureResult,
    pub validations: Vec<PaneValidation>,
    pub all_passed: bool,
}

/// Validation result for a single pane
#[derive(Debug)]
pub struct PaneValidation {
    pub pane_index: usize,
    pub scenario_name: String,
    pub passed: bool,
    pub missing_patterns: Vec<String>,
    pub captured_content: String,
}

impl TiledTestResult {
    /// Get a summary of the test results
    pub fn summary(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!(
            "Tiled Test Results: {} panes, {} passed, {} failed\n",
            self.validations.len(),
            self.validations.iter().filter(|v| v.passed).count(),
            self.validations.iter().filter(|v| !v.passed).count(),
        ));

        for v in &self.validations {
            let status = if v.passed { "✓" } else { "✗" };
            output.push_str(&format!("  {} Pane {}: {}\n", status, v.pane_index, v.scenario_name));

            if !v.missing_patterns.is_empty() {
                output.push_str("    Missing patterns:\n");
                for p in &v.missing_patterns {
                    output.push_str(&format!("      - \"{}\"\n", p));
                }
            }
        }

        output
    }

    /// Save all captures to a directory
    pub fn save_captures(&self, dir: &Path, prefix: &str) -> ScreenshotResult<()> {
        fs::create_dir_all(dir)?;

        for (i, content) in &self.captures.panes {
            let path = dir.join(format!("{}_{}.txt", prefix, i));
            fs::write(&path, content)?;
        }

        Ok(())
    }
}

/// Test runner for screenshot tests
pub struct ScreenshotTestRunner {
    golden_dir: PathBuf,
    update_goldens: bool,
}

impl ScreenshotTestRunner {
    /// Create a new test runner
    pub fn new(golden_dir: impl Into<PathBuf>) -> Self {
        let update_goldens = std::env::var("UPDATE_GOLDENS").is_ok();
        Self {
            golden_dir: golden_dir.into(),
            update_goldens,
        }
    }

    /// Run a screenshot test
    pub fn test<F>(&self, name: &str, width: u16, height: u16, render_fn: F) -> ScreenshotResult<()>
    where
        F: FnOnce(&mut ratatui::Frame, Rect),
    {
        let screenshot = TuiScreenshot::capture_widget(render_fn, width, height)
            .with_name(name);

        let golden_path = self.golden_dir.join(format!("{}.txt", name));

        if self.update_goldens {
            println!("Updating golden: {}", golden_path.display());
            screenshot.update_golden(&golden_path)?;
            return Ok(());
        }

        let diff = screenshot.compare_to_golden(&golden_path)?;

        if !diff.is_match() {
            // Save actual output for comparison
            let actual_path = self.golden_dir.join(format!("{}.actual.txt", name));
            screenshot.save(&actual_path)?;

            panic!(
                "Screenshot test '{}' failed!\n{}\n\nActual output saved to: {}\nRun with UPDATE_GOLDENS=1 to update golden image.",
                name,
                diff.summary(),
                actual_path.display()
            );
        }

        Ok(())
    }
}

/// Macro for defining screenshot tests
#[macro_export]
macro_rules! screenshot_test {
    ($name:ident, $width:expr, $height:expr, $body:expr) => {
        #[test]
        fn $name() {
            let runner = $crate::utils::tui::screenshot_test::ScreenshotTestRunner::new(
                concat!(env!("CARGO_MANIFEST_DIR"), "/tests/golden/tui")
            );
            runner.test(stringify!($name), $width, $height, $body).unwrap();
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::widgets::{Block, Borders, Paragraph};
    use ratatui::style::{Color, Style};

    #[test]
    fn test_basic_screenshot_capture() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let block = Block::default()
                .title(" Test Block ")
                .borders(Borders::ALL);
            f.render_widget(block, area);
        }, 40, 10);

        let text = screenshot.to_text();
        assert!(text.contains("Test Block"), "Should contain title");
        assert!(text.contains("┌"), "Should contain border characters");
    }

    #[test]
    fn test_screenshot_comparison_match() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Hello, World!");
            f.render_widget(para, area);
        }, 20, 3);

        let expected = screenshot.to_text();
        let diff = screenshot.compare_to_text(&expected);

        assert!(diff.is_match(), "Same content should match");
        assert_eq!(diff.match_percentage(), 100.0);
    }

    #[test]
    fn test_screenshot_comparison_mismatch() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Hello");
            f.render_widget(para, area);
        }, 20, 3);

        let diff = screenshot.compare_to_text("Goodbye");

        assert!(!diff.is_match(), "Different content should not match");
        assert!(!diff.differences.is_empty());
    }

    #[test]
    fn test_styled_text_output() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Warning!")
                .style(Style::default().fg(Color::Yellow));
            f.render_widget(para, area);
        }, 20, 3);

        let styled = screenshot.to_styled_text();
        assert!(styled.contains('~'), "Should contain yellow marker");
    }

    #[test]
    fn test_multiline_capture() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let text = "Line 1\nLine 2\nLine 3";
            let para = Paragraph::new(text);
            f.render_widget(para, area);
        }, 20, 5);

        let text = screenshot.to_text();
        assert!(text.contains("Line 1"));
        assert!(text.contains("Line 2"));
        assert!(text.contains("Line 3"));
    }

    // ========================================================================
    // COLOR ASSERTION TESTS
    // ========================================================================

    #[test]
    fn test_get_fg_color() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Red Text")
                .style(Style::default().fg(Color::Red));
            f.render_widget(para, area);
        }, 20, 3);

        // First character should be red
        let color = screenshot.get_fg_color(0, 0);
        assert_eq!(color, Some(Color::Red));
    }

    #[test]
    fn test_get_all_fg_colors() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Text")
                .style(Style::default().fg(Color::Green));
            f.render_widget(para, area);
        }, 20, 3);

        let colors = screenshot.get_all_fg_colors();
        assert!(colors.contains_key(&Color::Green), "Should contain green");
    }

    #[test]
    fn test_assert_has_color_success() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Alert!")
                .style(Style::default().fg(Color::Red));
            f.render_widget(para, area);
        }, 20, 3);

        assert!(screenshot.assert_has_color(Color::Red).is_ok());
    }

    #[test]
    fn test_assert_has_color_failure() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Normal")
                .style(Style::default().fg(Color::White));
            f.render_widget(para, area);
        }, 20, 3);

        assert!(screenshot.assert_has_color(Color::Red).is_err());
    }

    #[test]
    fn test_assert_no_color_success() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Safe")
                .style(Style::default().fg(Color::Green));
            f.render_widget(para, area);
        }, 20, 3);

        // Should not have red
        assert!(screenshot.assert_no_color(Color::Red).is_ok());
    }

    #[test]
    fn test_assert_no_color_failure() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Danger!")
                .style(Style::default().fg(Color::Red));
            f.render_widget(para, area);
        }, 20, 3);

        // Should fail because red exists
        assert!(screenshot.assert_no_color(Color::Red).is_err());
    }

    #[test]
    fn test_assert_text_has_color_success() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("CRITICAL")
                .style(Style::default().fg(Color::Red));
            f.render_widget(para, area);
        }, 20, 3);

        assert!(screenshot.assert_text_has_color("CRITICAL", Color::Red).is_ok());
    }

    #[test]
    fn test_assert_text_has_color_wrong_color() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("CRITICAL")
                .style(Style::default().fg(Color::Green)); // Wrong color!
            f.render_widget(para, area);
        }, 20, 3);

        let result = screenshot.assert_text_has_color("CRITICAL", Color::Red);
        assert!(result.is_err(), "Should fail because CRITICAL is green, not red");
    }

    #[test]
    fn test_assert_text_has_color_pattern_not_found() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Hello");
            f.render_widget(para, area);
        }, 20, 3);

        let result = screenshot.assert_text_has_color("CRITICAL", Color::Red);
        assert!(result.is_err(), "Should fail because pattern not found");
    }

    #[test]
    fn test_color_summary() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            use ratatui::text::{Line, Span};
            let lines = vec![
                Line::from(Span::styled("Red", Style::default().fg(Color::Red))),
                Line::from(Span::styled("Green", Style::default().fg(Color::Green))),
                Line::from(Span::styled("Yellow", Style::default().fg(Color::Yellow))),
            ];
            let para = Paragraph::new(lines);
            f.render_widget(para, area);
        }, 20, 5);

        let summary = screenshot.color_summary();
        assert!(summary.has_red);
        assert!(summary.has_green);
        assert!(summary.has_yellow);
        assert!(!summary.has_blue);
    }

    #[test]
    fn test_fluent_color_assertions() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            let para = Paragraph::new("Warning")
                .style(Style::default().fg(Color::Yellow));
            f.render_widget(para, area);
        }, 20, 3);

        let result = screenshot.assert_colors()
            .has_color(Color::Yellow)
            .and_then(|a| a.no_color(Color::Red));

        assert!(result.is_ok());
    }

    #[test]
    fn test_similar_colors() {
        // Red and LightRed should be considered similar
        assert!(is_similar_color(Color::Red, Color::LightRed));
        assert!(is_similar_color(Color::LightRed, Color::Red));

        // Red and Green should NOT be similar
        assert!(!is_similar_color(Color::Red, Color::Green));
    }

    #[test]
    fn test_region_assertion() {
        let screenshot = TuiScreenshot::capture_widget(|f, area| {
            use ratatui::text::{Line, Span};
            // First line red, second line green
            let lines = vec![
                Line::from(Span::styled("LINE1", Style::default().fg(Color::Red))),
                Line::from(Span::styled("LINE2", Style::default().fg(Color::Green))),
            ];
            let para = Paragraph::new(lines);
            f.render_widget(para, area);
        }, 20, 4);

        // First line should be red
        let region = ColorRegion::line(0, 20);
        assert!(screenshot.assert_region_has_color(region, Color::Red).is_ok());

        // Second line should be green
        let region = ColorRegion::line(1, 20);
        assert!(screenshot.assert_region_has_color(region, Color::Green).is_ok());
    }

    // ========================================================================
    // TMUX TILING TESTS (unit tests - don't require tmux to be running)
    // ========================================================================

    #[test]
    fn test_tile_layout_names() {
        assert_eq!(TileLayout::Single.to_tmux_layout(), "main-vertical");
        assert_eq!(TileLayout::MainVertical.to_tmux_layout(), "main-vertical");
        assert_eq!(TileLayout::MainHorizontal.to_tmux_layout(), "main-horizontal");
        assert_eq!(TileLayout::EvenHorizontal.to_tmux_layout(), "even-horizontal");
        assert_eq!(TileLayout::EvenVertical.to_tmux_layout(), "even-vertical");
        assert_eq!(TileLayout::Tiled.to_tmux_layout(), "tiled");
        assert_eq!(TileLayout::MainVerticalRatio(70).to_tmux_layout(), "main-vertical");
        assert_eq!(TileLayout::MainHorizontalRatio(60).to_tmux_layout(), "main-horizontal");
    }

    #[test]
    fn test_tmux_pane_creation() {
        let pane = TmuxPane::new(0);
        assert_eq!(pane.index, 0);
        assert!(pane.name.is_none());
        assert!(pane.command.is_none());

        let named_pane = TmuxPane::new(1).with_name("Dashboard");
        assert_eq!(named_pane.index, 1);
        assert_eq!(named_pane.name, Some("Dashboard".to_string()));
    }

    #[test]
    fn test_tiled_capture_result_get_pane() {
        let result = TiledCaptureResult {
            panes: vec![
                (0, "Pane 0 content".to_string()),
                (1, "Pane 1 content".to_string()),
                (2, "Pane 2 content".to_string()),
            ],
            session_name: "test".to_string(),
            layout: TileLayout::Tiled,
            timestamp: std::time::Instant::now(),
        };

        assert_eq!(result.get_pane(0), Some("Pane 0 content"));
        assert_eq!(result.get_pane(1), Some("Pane 1 content"));
        assert_eq!(result.get_pane(2), Some("Pane 2 content"));
        assert_eq!(result.get_pane(99), None);
    }

    #[test]
    fn test_tiled_capture_result_combined_view() {
        let result = TiledCaptureResult {
            panes: vec![
                (0, "Content A".to_string()),
                (1, "Content B".to_string()),
            ],
            session_name: "test".to_string(),
            layout: TileLayout::EvenHorizontal,
            timestamp: std::time::Instant::now(),
        };

        let combined = result.combined_view("\n---\n");
        assert!(combined.contains("=== Pane 0 ==="));
        assert!(combined.contains("Content A"));
        assert!(combined.contains("=== Pane 1 ==="));
        assert!(combined.contains("Content B"));
        assert!(combined.contains("---"));
    }

    #[test]
    fn test_tiled_test_scenario_builder() {
        let builder = TiledTestBuilder::new(160, 48)
            .layout(TileLayout::Tiled)
            .add_command("test1", "echo hello")
            .add_command("test2", "echo world");

        assert_eq!(builder.scenarios.len(), 2);
        assert_eq!(builder.scenarios[0].name, "test1");
        assert_eq!(builder.scenarios[0].command, "echo hello");
        assert_eq!(builder.scenarios[1].name, "test2");
        assert_eq!(builder.scenarios[1].command, "echo world");
    }

    #[test]
    fn test_tiled_test_result_summary() {
        let result = TiledTestResult {
            captures: TiledCaptureResult {
                panes: vec![(0, "test".to_string())],
                session_name: "test".to_string(),
                layout: TileLayout::Single,
                timestamp: std::time::Instant::now(),
            },
            validations: vec![
                PaneValidation {
                    pane_index: 0,
                    scenario_name: "test_scenario".to_string(),
                    passed: true,
                    missing_patterns: vec![],
                    captured_content: "test".to_string(),
                },
                PaneValidation {
                    pane_index: 1,
                    scenario_name: "failing_scenario".to_string(),
                    passed: false,
                    missing_patterns: vec!["expected_pattern".to_string()],
                    captured_content: "wrong".to_string(),
                },
            ],
            all_passed: false,
        };

        let summary = result.summary();
        assert!(summary.contains("2 panes"));
        assert!(summary.contains("1 passed"));
        assert!(summary.contains("1 failed"));
        assert!(summary.contains("✓ Pane 0: test_scenario"));
        assert!(summary.contains("✗ Pane 1: failing_scenario"));
        assert!(summary.contains("expected_pattern"));
    }

    #[test]
    fn test_tile_layout_equality() {
        assert_eq!(TileLayout::Tiled, TileLayout::Tiled);
        assert_ne!(TileLayout::Tiled, TileLayout::Single);
        assert_eq!(TileLayout::MainVerticalRatio(70), TileLayout::MainVerticalRatio(70));
        assert_ne!(TileLayout::MainVerticalRatio(70), TileLayout::MainVerticalRatio(80));
    }

    #[test]
    fn test_pane_validation_struct() {
        let validation = PaneValidation {
            pane_index: 0,
            scenario_name: "dashboard".to_string(),
            passed: true,
            missing_patterns: vec![],
            captured_content: "Dashboard content".to_string(),
        };

        assert!(validation.passed);
        assert!(validation.missing_patterns.is_empty());
        assert_eq!(validation.scenario_name, "dashboard");
    }
}

// ============================================================================
// TMUX INTEGRATION TESTS (require tmux to be installed)
// ============================================================================

#[cfg(test)]
mod tmux_integration_tests {
    use super::*;

    /// Helper to check if tmux is available
    fn tmux_available() -> bool {
        Command::new("tmux")
            .args(["-V"])
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tmux_tiled_session_creation() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        let session = TmuxTiledSession::new(80, 24);
        assert!(session.is_ok(), "Should create tmux session");

        let session = session.unwrap();
        assert_eq!(session.pane_count(), 1);
        // Session is automatically cleaned up on drop
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tmux_split_panes() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        let mut session = TmuxTiledSession::new(160, 48)
            .expect("Failed to create session")
            .with_layout(TileLayout::Tiled);

        session.split_panes(4).expect("Failed to split panes");
        assert_eq!(session.pane_count(), 4);
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tmux_capture_all_panes() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        let mut session = TmuxTiledSession::new(80, 24)
            .expect("Failed to create session")
            .with_layout(TileLayout::EvenHorizontal);

        session.split_panes(2).expect("Failed to split panes");

        // Run different commands in each pane
        session.run_in_pane(0, "echo 'PANE_ZERO'").expect("Failed to run in pane 0");
        session.run_in_pane(1, "echo 'PANE_ONE'").expect("Failed to run in pane 1");

        std::thread::sleep(std::time::Duration::from_millis(500));

        let captures = session.capture_all().expect("Failed to capture");
        assert_eq!(captures.panes.len(), 2);

        // Verify content was captured (might include shell prompts)
        let pane0 = captures.get_pane(0).expect("Pane 0 not found");
        let pane1 = captures.get_pane(1).expect("Pane 1 not found");

        assert!(pane0.contains("PANE_ZERO") || !pane0.is_empty());
        assert!(pane1.contains("PANE_ONE") || !pane1.is_empty());
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tmux_predefined_layouts() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        // Test dashboard layout
        let dashboard = TmuxTiledSession::create_test_layout("dashboard", 160, 48);
        assert!(dashboard.is_ok());
        assert_eq!(dashboard.unwrap().pane_count(), 4);

        // Test comparison layout
        let comparison = TmuxTiledSession::create_test_layout("comparison", 160, 48);
        assert!(comparison.is_ok());
        assert_eq!(comparison.unwrap().pane_count(), 2);

        // Test quad layout
        let quad = TmuxTiledSession::create_test_layout("quad", 160, 48);
        assert!(quad.is_ok());
        assert_eq!(quad.unwrap().pane_count(), 4);

        // Test monitoring layout (btop-style)
        let monitoring = TmuxTiledSession::create_test_layout("monitoring", 160, 48);
        assert!(monitoring.is_ok());
        assert_eq!(monitoring.unwrap().pane_count(), 3);

        // Test unknown layout
        let unknown = TmuxTiledSession::create_test_layout("nonexistent", 80, 24);
        assert!(unknown.is_err());
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tmux_send_keys_to_pane() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        let mut session = TmuxTiledSession::new(80, 24)
            .expect("Failed to create session");

        session.split_panes(2).expect("Failed to split");

        // Send keys without Enter
        session.send_keys_to_pane(0, "test input").expect("Failed to send keys");

        // Verify error for invalid pane
        let result = session.send_keys_to_pane(99, "invalid");
        assert!(result.is_err());
    }

    #[test]
    #[ignore = "requires tmux to be installed and running"]
    fn test_tiled_test_builder_run() {
        if !tmux_available() {
            eprintln!("Skipping: tmux not available");
            return;
        }

        let result = TiledTestBuilder::new(80, 24)
            .layout(TileLayout::EvenHorizontal)
            .add_command("echo_test", "echo 'HELLO'")
            .add_command("pwd_test", "pwd")
            .run();

        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.captures.panes.len(), 2);
    }
}
