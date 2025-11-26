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
        let session_name = format!("osvm_test_{}", std::process::id());

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
}
