//! Visual Layout Validator for TUI Testing
//!
//! This module provides comprehensive visual layout validation for terminal UIs.
//! It can detect and validate UI components, measure layouts, and perform visual regression testing.

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Layout component detection results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayoutAnalysis {
    pub width: u16,
    pub height: u16,
    pub components: Vec<DetectedComponent>,
    pub borders: Vec<DetectedBorder>,
    pub text_regions: Vec<TextRegion>,
    pub layout_metrics: LayoutMetrics,
}

/// A detected UI component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedComponent {
    pub component_type: ComponentType,
    pub position: Position,
    pub size: Size,
    pub content: String,
}

/// Type of UI component
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComponentType {
    TitleBar,
    StatusBar,
    InputField,
    ChatHistory,
    SessionList,
    ToolPanel,
    HelpDialog,
    ContextMenu,
    Border,
    Button,
    Label,
}

/// Position in terminal (row, col)
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Position {
    pub row: u16,
    pub col: u16,
}

/// Size dimensions
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Size {
    pub width: u16,
    pub height: u16,
}

/// Detected border (box drawing characters)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedBorder {
    pub style: BorderStyle,
    pub position: Position,
    pub size: Size,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum BorderStyle {
    Single,   // ─│┌┐└┘
    Double,   // ═║╔╗╚╝
    Heavy,    // ━┃┏┓┗┛
    Rounded,  // ─│╭╮╰╯
    Mixed,
}

/// Text region with content
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextRegion {
    pub position: Position,
    pub size: Size,
    pub content: String,
    pub is_emphasized: bool, // Bold, colored, etc.
}

/// Layout metrics for validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayoutMetrics {
    pub total_borders: usize,
    pub total_text_regions: usize,
    pub max_nesting_depth: usize,
    pub symmetry_score: f32, // 0.0 - 1.0
    pub alignment_score: f32, // 0.0 - 1.0
}

/// Visual regression comparison result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegressionResult {
    pub passed: bool,
    pub similarity_score: f32, // 0.0 - 1.0
    pub differences: Vec<VisualDifference>,
    pub baseline_path: PathBuf,
    pub current_path: PathBuf,
}

/// A visual difference between two screenshots
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualDifference {
    pub diff_type: DifferenceType,
    pub position: Position,
    pub description: String,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum DifferenceType {
    MissingComponent,
    ExtraComponent,
    PositionChanged,
    SizeChanged,
    ContentChanged,
    StyleChanged,
}

/// Visual layout validator
pub struct VisualLayoutValidator {
    baseline_dir: PathBuf,
    tolerance: f32, // Similarity tolerance (0.0 - 1.0)
}

impl VisualLayoutValidator {
    /// Create a new visual layout validator
    pub fn new(baseline_dir: PathBuf) -> Result<Self> {
        std::fs::create_dir_all(&baseline_dir)?;

        Ok(Self {
            baseline_dir,
            tolerance: 0.95, // 95% similarity required
        })
    }

    /// Analyze terminal screen layout
    pub fn analyze_layout(&self, screen_content: &str, width: u16, height: u16) -> Result<LayoutAnalysis> {
        let components = self.detect_components(screen_content, width, height)?;
        let borders = self.detect_borders(screen_content, width, height)?;
        let text_regions = self.detect_text_regions(screen_content, width, height)?;
        let layout_metrics = self.calculate_metrics(&components, &borders, width, height);

        Ok(LayoutAnalysis {
            width,
            height,
            components,
            borders,
            text_regions,
            layout_metrics,
        })
    }

    /// Detect UI components in screen content
    fn detect_components(&self, content: &str, width: u16, height: u16) -> Result<Vec<DetectedComponent>> {
        let mut components = Vec::new();
        let lines: Vec<&str> = content.lines().collect();

        // Detect title bar (usually top line with centered text)
        if let Some(first_line) = lines.first() {
            if self.looks_like_title_bar(first_line) {
                components.push(DetectedComponent {
                    component_type: ComponentType::TitleBar,
                    position: Position { row: 0, col: 0 },
                    size: Size { width, height: 1 },
                    content: first_line.trim().to_string(),
                });
            }
        }

        // Detect status bar (usually bottom line)
        if let Some(last_line) = lines.last() {
            if self.looks_like_status_bar(last_line) {
                components.push(DetectedComponent {
                    component_type: ComponentType::StatusBar,
                    position: Position { row: height - 1, col: 0 },
                    size: Size { width, height: 1 },
                    content: last_line.trim().to_string(),
                });
            }
        }

        // Detect input fields (lines with prompts like ">" or ":")
        for (idx, line) in lines.iter().enumerate() {
            if self.looks_like_input_field(line) {
                components.push(DetectedComponent {
                    component_type: ComponentType::InputField,
                    position: Position { row: idx as u16, col: 0 },
                    size: Size { width, height: 1 },
                    content: line.trim().to_string(),
                });
            }
        }

        // Detect help dialogs (boxes with "Help" or "?" in content)
        if content.contains("Help") || content.contains("Commands") {
            components.push(DetectedComponent {
                component_type: ComponentType::HelpDialog,
                position: Position { row: 0, col: 0 },
                size: Size { width: 0, height: 0 }, // Will be calculated
                content: "Help dialog detected".to_string(),
            });
        }

        // Detect session list (FAR-style left panel)
        if content.contains("Sessions") || content.contains("Session") {
            components.push(DetectedComponent {
                component_type: ComponentType::SessionList,
                position: Position { row: 0, col: 0 },
                size: Size { width: 0, height: 0 },
                content: "Session list detected".to_string(),
            });
        }

        Ok(components)
    }

    /// Detect borders (box drawing characters)
    fn detect_borders(&self, content: &str, width: u16, height: u16) -> Result<Vec<DetectedBorder>> {
        let mut borders = Vec::new();
        let lines: Vec<&str> = content.lines().collect();

        for (row, line) in lines.iter().enumerate() {
            let mut current_border: Option<(usize, BorderStyle)> = None;

            for (col, ch) in line.chars().enumerate() {
                if self.is_border_char(ch) {
                    let style = self.detect_border_style(ch);

                    if let Some((start_col, existing_style)) = current_border {
                        if style != existing_style {
                            // Border style changed, save previous border
                            borders.push(DetectedBorder {
                                style: existing_style,
                                position: Position { row: row as u16, col: start_col as u16 },
                                size: Size { width: (col - start_col) as u16, height: 1 },
                            });
                            current_border = Some((col, style));
                        }
                    } else {
                        current_border = Some((col, style));
                    }
                } else if let Some((start_col, style)) = current_border {
                    // Border ended
                    borders.push(DetectedBorder {
                        style,
                        position: Position { row: row as u16, col: start_col as u16 },
                        size: Size { width: (col - start_col) as u16, height: 1 },
                    });
                    current_border = None;
                }
            }

            // Handle border that extends to end of line
            if let Some((start_col, style)) = current_border {
                borders.push(DetectedBorder {
                    style,
                    position: Position { row: row as u16, col: start_col as u16 },
                    size: Size { width: (line.len() - start_col) as u16, height: 1 },
                });
            }
        }

        Ok(borders)
    }

    /// Detect text regions
    fn detect_text_regions(&self, content: &str, width: u16, height: u16) -> Result<Vec<TextRegion>> {
        let mut regions = Vec::new();
        let lines: Vec<&str> = content.lines().collect();

        for (row, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if !trimmed.is_empty() && !self.is_all_borders(line) {
                regions.push(TextRegion {
                    position: Position { row: row as u16, col: 0 },
                    size: Size { width: trimmed.len() as u16, height: 1 },
                    content: trimmed.to_string(),
                    is_emphasized: self.has_emphasis(line),
                });
            }
        }

        Ok(regions)
    }

    /// Calculate layout metrics
    fn calculate_metrics(
        &self,
        components: &[DetectedComponent],
        borders: &[DetectedBorder],
        width: u16,
        height: u16,
    ) -> LayoutMetrics {
        let total_borders = borders.len();
        let total_text_regions = components.len();

        // Simple heuristic for nesting depth (count overlapping borders)
        let max_nesting_depth = self.calculate_nesting_depth(borders);

        // Calculate symmetry (how balanced the layout is)
        let symmetry_score = self.calculate_symmetry(components, width, height);

        // Calculate alignment (how well components align)
        let alignment_score = self.calculate_alignment(components);

        LayoutMetrics {
            total_borders,
            total_text_regions,
            max_nesting_depth,
            symmetry_score,
            alignment_score,
        }
    }

    /// Compare two screenshots for visual regression
    pub fn compare_screenshots(&self, baseline: &str, current: &str, name: &str) -> Result<RegressionResult> {
        let baseline_path = self.baseline_dir.join(format!("{}_baseline.txt", name));
        let current_path = self.baseline_dir.join(format!("{}_current.txt", name));

        // Save current for comparison
        std::fs::write(&current_path, current)?;

        // Check if baseline exists
        if !baseline_path.exists() {
            // Create baseline
            std::fs::write(&baseline_path, baseline)?;
            return Ok(RegressionResult {
                passed: true,
                similarity_score: 1.0,
                differences: vec![],
                baseline_path,
                current_path,
            });
        }

        // Load baseline
        let baseline_content = std::fs::read_to_string(&baseline_path)?;

        // Calculate similarity
        let similarity = self.calculate_similarity(&baseline_content, current);
        let passed = similarity >= self.tolerance;

        // Find differences
        let differences = self.find_differences(&baseline_content, current);

        Ok(RegressionResult {
            passed,
            similarity_score: similarity,
            differences,
            baseline_path,
            current_path,
        })
    }

    /// Calculate similarity between two screenshots (0.0 - 1.0)
    fn calculate_similarity(&self, baseline: &str, current: &str) -> f32 {
        let baseline_lines: Vec<&str> = baseline.lines().collect();
        let current_lines: Vec<&str> = current.lines().collect();

        if baseline_lines.is_empty() && current_lines.is_empty() {
            return 1.0;
        }

        let max_lines = baseline_lines.len().max(current_lines.len());
        if max_lines == 0 {
            return 1.0;
        }

        let mut matching_lines = 0;
        let mut partial_matches = 0.0;

        for i in 0..max_lines {
            match (baseline_lines.get(i), current_lines.get(i)) {
                (Some(baseline_line), Some(current_line)) => {
                    if baseline_line == current_line {
                        matching_lines += 1;
                    } else {
                        // Calculate line similarity
                        let line_similarity = self.calculate_line_similarity(baseline_line, current_line);
                        partial_matches += line_similarity;
                    }
                }
                _ => {
                    // One line is missing
                }
            }
        }

        (matching_lines as f32 + partial_matches) / max_lines as f32
    }

    /// Calculate similarity between two lines
    fn calculate_line_similarity(&self, baseline: &str, current: &str) -> f32 {
        let max_len = baseline.len().max(current.len());
        if max_len == 0 {
            return 1.0;
        }

        let matching_chars: usize = baseline
            .chars()
            .zip(current.chars())
            .filter(|(a, b)| a == b)
            .count();

        matching_chars as f32 / max_len as f32
    }

    /// Find specific differences between screenshots
    fn find_differences(&self, baseline: &str, current: &str) -> Vec<VisualDifference> {
        let mut differences = Vec::new();
        let baseline_lines: Vec<&str> = baseline.lines().collect();
        let current_lines: Vec<&str> = current.lines().collect();

        let max_lines = baseline_lines.len().max(current_lines.len());

        for i in 0..max_lines {
            match (baseline_lines.get(i), current_lines.get(i)) {
                (Some(baseline_line), Some(current_line)) => {
                    if baseline_line != current_line {
                        differences.push(VisualDifference {
                            diff_type: DifferenceType::ContentChanged,
                            position: Position { row: i as u16, col: 0 },
                            description: format!("Line {} changed", i + 1),
                        });
                    }
                }
                (Some(_), None) => {
                    differences.push(VisualDifference {
                        diff_type: DifferenceType::MissingComponent,
                        position: Position { row: i as u16, col: 0 },
                        description: format!("Line {} missing in current", i + 1),
                    });
                }
                (None, Some(_)) => {
                    differences.push(VisualDifference {
                        diff_type: DifferenceType::ExtraComponent,
                        position: Position { row: i as u16, col: 0 },
                        description: format!("Extra line {} in current", i + 1),
                    });
                }
                (None, None) => unreachable!(),
            }
        }

        differences
    }

    // Helper methods for component detection

    fn looks_like_title_bar(&self, line: &str) -> bool {
        let trimmed = line.trim();
        // Title bars usually have some centering and are not too long
        trimmed.len() > 5 && trimmed.len() < 60 && !self.is_all_borders(line)
    }

    fn looks_like_status_bar(&self, line: &str) -> bool {
        // Status bars often contain key hints or status info
        line.contains("F10") || line.contains("Help") || line.contains("Quit") || line.contains("Session")
    }

    fn looks_like_input_field(&self, line: &str) -> bool {
        let trimmed = line.trim();
        trimmed.starts_with('>') || trimmed.starts_with(':') || trimmed.contains("Type a message")
    }

    fn is_border_char(&self, ch: char) -> bool {
        matches!(ch,
            '─' | '│' | '┌' | '┐' | '└' | '┘' |  // Single
            '═' | '║' | '╔' | '╗' | '╚' | '╝' |  // Double
            '━' | '┃' | '┏' | '┓' | '┗' | '┛' |  // Heavy
            '╭' | '╮' | '╰' | '╯' |              // Rounded
            '├' | '┤' | '┬' | '┴' | '┼' |        // Connectors
            '╠' | '╣' | '╦' | '╩' | '╬'          // Double connectors
        )
    }

    fn detect_border_style(&self, ch: char) -> BorderStyle {
        match ch {
            '─' | '│' | '┌' | '┐' | '└' | '┘' | '├' | '┤' | '┬' | '┴' | '┼' => BorderStyle::Single,
            '═' | '║' | '╔' | '╗' | '╚' | '╝' | '╠' | '╣' | '╦' | '╩' | '╬' => BorderStyle::Double,
            '━' | '┃' | '┏' | '┓' | '┗' | '┛' => BorderStyle::Heavy,
            '╭' | '╮' | '╰' | '╯' => BorderStyle::Rounded,
            _ => BorderStyle::Mixed,
        }
    }

    fn is_all_borders(&self, line: &str) -> bool {
        !line.is_empty() && line.chars().all(|ch| self.is_border_char(ch) || ch.is_whitespace())
    }

    fn has_emphasis(&self, line: &str) -> bool {
        // Check for ANSI codes indicating emphasis
        line.contains("\x1b[1m") || line.contains("\x1b[4m") || line.contains("\x1b[7m")
    }

    fn calculate_nesting_depth(&self, borders: &[DetectedBorder]) -> usize {
        // Simple heuristic: count distinct rows with borders
        let mut rows = std::collections::HashSet::new();
        for border in borders {
            rows.insert(border.position.row);
        }
        rows.len().min(5) // Cap at 5 for sanity
    }

    fn calculate_symmetry(&self, components: &[DetectedComponent], width: u16, height: u16) -> f32 {
        if components.is_empty() {
            return 1.0;
        }

        // Calculate center of mass
        let center_col = width / 2;
        let mut left_weight = 0.0;
        let mut right_weight = 0.0;

        for component in components {
            let component_center = component.position.col + component.size.width / 2;
            if component_center < center_col {
                left_weight += component.size.width as f32;
            } else {
                right_weight += component.size.width as f32;
            }
        }

        let total_weight = left_weight + right_weight;
        if total_weight == 0.0 {
            return 1.0;
        }

        1.0 - ((left_weight - right_weight).abs() / total_weight)
    }

    fn calculate_alignment(&self, components: &[DetectedComponent]) -> f32 {
        if components.len() < 2 {
            return 1.0;
        }

        // Check how many components share the same column positions
        let mut col_counts: HashMap<u16, usize> = HashMap::new();
        for component in components {
            *col_counts.entry(component.position.col).or_insert(0) += 1;
        }

        let max_aligned = col_counts.values().max().copied().unwrap_or(1);
        max_aligned as f32 / components.len() as f32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_border_char_detection() {
        let validator = VisualLayoutValidator::new(PathBuf::from("/tmp")).unwrap();
        assert!(validator.is_border_char('─'));
        assert!(validator.is_border_char('│'));
        assert!(validator.is_border_char('╔'));
        assert!(!validator.is_border_char('a'));
    }

    #[test]
    fn test_similarity_calculation() {
        let validator = VisualLayoutValidator::new(PathBuf::from("/tmp")).unwrap();

        let text1 = "Hello World\nTest Line";
        let text2 = "Hello World\nTest Line";
        assert_eq!(validator.calculate_similarity(text1, text2), 1.0);

        let text3 = "Hello World\nDifferent Line";
        let similarity = validator.calculate_similarity(text1, text3);
        assert!(similarity < 1.0 && similarity > 0.5);
    }
}
