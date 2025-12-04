//! Streaming output and context tracking for Claude Code-style UX
//!
//! This module provides:
//! - Character-by-character streaming response display
//! - Context window usage indicator
//! - Multiline input support

use super::Colors;
use anyhow::Result;
use crossterm::terminal;
use std::io::{self, Write};
use std::time::Duration;
use tokio::time::sleep;

/// Context usage tracking for displaying token utilization
#[derive(Debug, Clone)]
pub struct ContextTracker {
    /// Estimated tokens used in current conversation
    pub tokens_used: usize,
    /// Maximum context window size
    pub max_tokens: usize,
    /// Number of messages in chat history
    pub message_count: usize,
    /// Estimated tokens per message (running average)
    avg_tokens_per_message: f32,
}

impl Default for ContextTracker {
    fn default() -> Self {
        Self {
            tokens_used: 0,
            max_tokens: 128_000, // Claude's typical context window
            message_count: 0,
            avg_tokens_per_message: 50.0,
        }
    }
}

impl ContextTracker {
    /// Create a new context tracker
    pub fn new() -> Self {
        Self::default()
    }

    /// Estimate tokens from text (rough approximation: ~4 chars per token)
    fn estimate_tokens(text: &str) -> usize {
        // More accurate estimation considering:
        // - Words are roughly 1.3 tokens on average
        // - Punctuation and special chars count as separate tokens
        let word_count = text.split_whitespace().count();
        let punct_count = text.chars().filter(|c| c.is_ascii_punctuation()).count();
        (word_count as f32 * 1.3 + punct_count as f32 * 0.5) as usize
    }

    /// Add a message to the context
    pub fn add_message(&mut self, text: &str) {
        let tokens = Self::estimate_tokens(text);
        self.tokens_used = self.tokens_used.saturating_add(tokens);
        self.message_count += 1;

        // Update running average
        self.avg_tokens_per_message =
            (self.avg_tokens_per_message * (self.message_count - 1) as f32 + tokens as f32)
            / self.message_count as f32;
    }

    /// Get usage percentage
    pub fn usage_percent(&self) -> f32 {
        (self.tokens_used as f32 / self.max_tokens as f32 * 100.0).min(100.0)
    }

    /// Check if context is getting full (> 80%)
    pub fn is_context_full(&self) -> bool {
        self.usage_percent() > 80.0
    }

    /// Estimate remaining messages before context is full
    pub fn estimated_remaining_messages(&self) -> usize {
        if self.avg_tokens_per_message > 0.0 {
            let remaining_tokens = self.max_tokens.saturating_sub(self.tokens_used);
            (remaining_tokens as f32 / self.avg_tokens_per_message) as usize
        } else {
            999
        }
    }

    /// Reset the tracker (e.g., after /clear)
    pub fn reset(&mut self) {
        self.tokens_used = 0;
        self.message_count = 0;
        self.avg_tokens_per_message = 50.0;
    }
}

/// Render a compact context usage bar at the bottom of the terminal
pub fn render_context_bar(tracker: &ContextTracker) -> String {
    let usage = tracker.usage_percent();
    let bar_width = 20;
    let filled = (usage / 100.0 * bar_width as f32) as usize;
    let empty = bar_width - filled;

    // Color based on usage level
    let (bar_color, status) = if usage > 90.0 {
        (Colors::RED, "⚠️  Critical")
    } else if usage > 75.0 {
        (Colors::YELLOW, "⚡ High")
    } else if usage > 50.0 {
        (Colors::CYAN, "📊 Medium")
    } else {
        (Colors::GREEN, "✓ Good")
    };

    let bar = format!(
        "{}{}{}{}",
        bar_color,
        "█".repeat(filled),
        Colors::DIM,
        "░".repeat(empty)
    );

    format!(
        "{}[{}{}{} {:.1}%{} • {}msgs • ~{}k tokens • {}]{}",
        Colors::DIM,
        bar,
        Colors::RESET,
        Colors::DIM,
        usage,
        Colors::RESET,
        tracker.message_count,
        tracker.tokens_used / 1000,
        status,
        Colors::RESET
    )
}

/// Render the context bar at the bottom of the screen
pub fn show_context_bar(tracker: &ContextTracker) {
    let (cols, rows) = terminal::size().unwrap_or((80, 24));
    let bar = render_context_bar(tracker);

    // Position at bottom row
    print!("\x1B[{};1H", rows);
    print!("\x1B[2K"); // Clear line
    print!("{}", bar);
    let _ = io::stdout().flush();
}

/// Stream text output character by character with variable speed
pub async fn stream_text(text: &str, chars_per_second: f32) {
    let delay = Duration::from_secs_f32(1.0 / chars_per_second);
    let mut stdout = io::stdout();

    for ch in text.chars() {
        print!("{}", ch);
        let _ = stdout.flush();

        // Variable speed: faster for spaces and punctuation
        let actual_delay = if ch.is_whitespace() || ch.is_ascii_punctuation() {
            delay / 2
        } else {
            delay
        };
        sleep(actual_delay).await;
    }
    let _ = stdout.flush();
}

/// Stream text with word-by-word output (more natural feel)
pub async fn stream_text_by_words(text: &str, words_per_second: f32) {
    let delay = Duration::from_secs_f32(1.0 / words_per_second);
    let mut stdout = io::stdout();

    let words: Vec<&str> = text.split_inclusive(|c: char| c.is_whitespace() || c == '\n').collect();

    for word in words {
        print!("{}", word);
        let _ = stdout.flush();
        sleep(delay).await;
    }
    let _ = stdout.flush();
}

/// Stream text with a Claude Code-style typing effect
/// Starts slow, speeds up, then slows down at the end
pub async fn stream_claude_style(text: &str) {
    let mut stdout = io::stdout();
    let total_chars = text.chars().count();

    if total_chars == 0 {
        return;
    }

    // Calculate base delay to complete in ~2-4 seconds for typical messages
    let base_time_secs = (total_chars as f32 / 100.0).clamp(0.5, 4.0);

    for (i, ch) in text.chars().enumerate() {
        print!("{}", ch);
        let _ = stdout.flush();

        // Variable speed based on position
        let progress = i as f32 / total_chars as f32;
        let speed_multiplier = if progress < 0.1 {
            0.3 // Start slow
        } else if progress > 0.9 {
            0.5 // End slow
        } else {
            1.0 + (progress - 0.5).abs() * 0.5 // Fastest in middle
        };

        let char_delay = base_time_secs / (total_chars as f32 * speed_multiplier);

        // Skip delay for spaces and certain punctuation for more natural feel
        if !ch.is_whitespace() {
            sleep(Duration::from_secs_f32(char_delay)).await;
        }
    }
    let _ = stdout.flush();
}

/// Multiline input state
#[derive(Debug, Default)]
pub struct MultilineInput {
    lines: Vec<String>,
    current_line: usize,
    cursor_col: usize,
    is_multiline_mode: bool,
}

impl MultilineInput {
    pub fn new() -> Self {
        Self {
            lines: vec![String::new()],
            current_line: 0,
            cursor_col: 0,
            is_multiline_mode: false,
        }
    }

    /// Enter multiline mode (triggered by Shift+Enter simulation: \n in input)
    pub fn enter_multiline_mode(&mut self) {
        self.is_multiline_mode = true;
    }

    /// Check if in multiline mode
    pub fn is_multiline(&self) -> bool {
        self.is_multiline_mode
    }

    /// Add a new line (Shift+Enter behavior)
    pub fn new_line(&mut self) {
        // Split current line at cursor and create new line
        let current = &self.lines[self.current_line];
        let (before, after) = current.split_at(self.cursor_col.min(current.len()));

        let after_str = after.to_string();
        self.lines[self.current_line] = before.to_string();
        self.lines.insert(self.current_line + 1, after_str);

        self.current_line += 1;
        self.cursor_col = 0;
        self.is_multiline_mode = true;
    }

    /// Insert a character at cursor position
    pub fn insert_char(&mut self, ch: char) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }

        let line = &mut self.lines[self.current_line];
        if self.cursor_col >= line.len() {
            line.push(ch);
        } else {
            line.insert(self.cursor_col, ch);
        }
        self.cursor_col += 1;
    }

    /// Delete character before cursor
    pub fn backspace(&mut self) {
        if self.cursor_col > 0 {
            let line = &mut self.lines[self.current_line];
            if self.cursor_col <= line.len() {
                line.remove(self.cursor_col - 1);
                self.cursor_col -= 1;
            }
        } else if self.current_line > 0 {
            // Merge with previous line
            let current_content = self.lines.remove(self.current_line);
            self.current_line -= 1;
            self.cursor_col = self.lines[self.current_line].len();
            self.lines[self.current_line].push_str(&current_content);
        }
    }

    /// Get the full text content
    pub fn get_content(&self) -> String {
        self.lines.join("\n")
    }

    /// Clear all content
    pub fn clear(&mut self) {
        self.lines = vec![String::new()];
        self.current_line = 0;
        self.cursor_col = 0;
        self.is_multiline_mode = false;
    }

    /// Get display representation with line numbers
    pub fn get_display(&self) -> String {
        if self.lines.len() == 1 {
            self.lines[0].clone()
        } else {
            self.lines
                .iter()
                .enumerate()
                .map(|(i, line)| {
                    if i == self.current_line {
                        format!("{}{}│{} {}", Colors::CYAN, i + 1, Colors::RESET, line)
                    } else {
                        format!("{}{}│{} {}", Colors::DIM, i + 1, Colors::RESET, line)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        }
    }

    /// Get line count
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }
}

/// Render multiline input box with visual indicator
pub fn render_multiline_input(input: &MultilineInput) -> Result<()> {
    let (cols, _) = terminal::size().unwrap_or((80, 24));
    let content_width = cols.saturating_sub(4) as usize;

    if input.is_multiline() {
        println!(
            "{}┌─ Multiline Input (Enter to submit, Esc to cancel) ───────┐{}",
            Colors::CYAN,
            Colors::RESET
        );

        for (i, line) in input.lines.iter().enumerate() {
            let line_num = format!("{:2}", i + 1);
            let indicator = if i == input.current_line { "▶" } else { " " };

            let truncated = if line.len() > content_width {
                format!("{}...", &line[..content_width.saturating_sub(3)])
            } else {
                line.clone()
            };

            println!(
                "{}│{}{}{} {}│{}",
                Colors::CYAN,
                Colors::DIM,
                line_num,
                indicator,
                truncated,
                Colors::RESET
            );
        }

        println!(
            "{}└{}┘{}",
            Colors::CYAN,
            "─".repeat(content_width + 4),
            Colors::RESET
        );
    } else {
        // Single line mode - just show the content
        let content = input.get_content();
        print!("> {}", content);
    }

    io::stdout().flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_tracker() {
        let mut tracker = ContextTracker::new();

        tracker.add_message("Hello, how are you?");
        assert!(tracker.tokens_used > 0);
        assert_eq!(tracker.message_count, 1);

        tracker.add_message("I'm doing well, thank you for asking!");
        assert_eq!(tracker.message_count, 2);

        assert!(tracker.usage_percent() < 1.0); // Should be very low
    }

    #[test]
    fn test_multiline_input() {
        let mut input = MultilineInput::new();

        input.insert_char('H');
        input.insert_char('i');
        assert_eq!(input.get_content(), "Hi");

        input.new_line();
        assert!(input.is_multiline());
        assert_eq!(input.line_count(), 2);

        input.insert_char('!');
        assert_eq!(input.get_content(), "Hi\n!");

        input.backspace();
        input.backspace(); // Should merge lines
        assert_eq!(input.line_count(), 1);
    }

    #[test]
    fn test_render_context_bar() {
        let tracker = ContextTracker::new();
        let bar = render_context_bar(&tracker);

        assert!(bar.contains("0.0%"));
        assert!(bar.contains("0msgs"));
    }
}
