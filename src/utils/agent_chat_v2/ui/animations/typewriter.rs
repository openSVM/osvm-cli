//! Typewriter effect for AI responses - creates anticipation and feels more human

use super::{Animation, QualityLevel};
use std::time::{Duration, Instant};

/// Typewriter animation for text appearing character by character
pub struct TypewriterEffect {
    full_text: String,
    displayed_text: String,
    current_index: usize,
    char_delay: Duration,
    last_char_time: Instant,
    is_complete: bool,
    speed_multiplier: f32,

    // Advanced features
    variable_speed: bool,    // Speed varies based on punctuation
    cursor_visible: bool,    // Show blinking cursor
    cursor_blink_phase: f32, // For cursor animation
    sound_enabled: bool,     // Typing sounds
}

impl TypewriterEffect {
    pub fn new(text: String) -> Self {
        Self {
            full_text: text,
            displayed_text: String::new(),
            current_index: 0,
            char_delay: Duration::from_millis(30), // ~33 chars/second
            last_char_time: Instant::now(),
            is_complete: false,
            speed_multiplier: 1.0,
            variable_speed: true,
            cursor_visible: true,
            cursor_blink_phase: 0.0,
            sound_enabled: false,
        }
    }

    /// Set typing speed (1.0 = normal, 2.0 = double speed, 0.5 = half speed)
    pub fn set_speed(&mut self, multiplier: f32) {
        self.speed_multiplier = multiplier.max(0.1).min(10.0);
    }

    /// Enable variable speed based on punctuation
    pub fn set_variable_speed(&mut self, enabled: bool) {
        self.variable_speed = enabled;
    }

    /// Get delay for next character based on context
    fn get_char_delay(&self, ch: char) -> Duration {
        if !self.variable_speed {
            return Duration::from_millis((30.0 / self.speed_multiplier) as u64);
        }

        // Variable delays for more natural typing
        let base_delay = match ch {
            '.' | '!' | '?' => 300, // Long pause after sentence
            ',' | ';' | ':' => 150, // Medium pause after clause
            ' ' => 20,              // Quick space
            '\n' => 200,            // Pause at newline
            _ => 30,                // Normal character
        };

        Duration::from_millis((base_delay as f32 / self.speed_multiplier) as u64)
    }

    /// Add next character(s) to displayed text
    fn add_next_chars(&mut self) {
        if self.current_index >= self.full_text.len() {
            self.is_complete = true;
            return;
        }

        let chars: Vec<char> = self.full_text.chars().collect();
        let current_char = chars[self.current_index];

        // Check if we should add this character
        let delay = self.get_char_delay(current_char);
        if self.last_char_time.elapsed() >= delay {
            // Add the character
            self.displayed_text.push(current_char);
            self.current_index += 1;
            self.last_char_time = Instant::now();

            // Emit typing sound if enabled
            if self.sound_enabled {
                self.emit_typing_sound(current_char);
            }
        }
    }

    /// Emit typing sound (terminal bell or custom)
    fn emit_typing_sound(&self, ch: char) {
        if !self.sound_enabled {
            return;
        }

        // Only sound for actual characters, not spaces or newlines
        if !ch.is_whitespace() {
            // This would emit a subtle terminal bell
            // In practice, you might want to use a proper sound library
            // print!("\x07");
        }
    }

    /// Get current display text with optional cursor
    pub fn get_display_text(&self) -> String {
        let mut text = self.displayed_text.clone();

        if self.cursor_visible && !self.is_complete {
            // Blinking cursor
            let cursor = if self.cursor_blink_phase < 0.5 {
                "█" // Solid block
            } else {
                "▁" // Thin underline
            };
            text.push_str(cursor);
        }

        text
    }

    /// Skip to end (instant complete)
    pub fn skip_to_end(&mut self) {
        self.displayed_text = self.full_text.clone();
        self.current_index = self.full_text.len();
        self.is_complete = true;
    }

    /// Get progress percentage
    pub fn progress(&self) -> f32 {
        if self.full_text.is_empty() {
            return 1.0;
        }
        self.current_index as f32 / self.full_text.len() as f32
    }
}

impl Animation for TypewriterEffect {
    fn update(&mut self, delta: f32, quality: QualityLevel) {
        if self.is_complete {
            return;
        }

        // Update cursor blink
        self.cursor_blink_phase += delta * 2.0; // Blink every 0.5 seconds
        if self.cursor_blink_phase > 1.0 {
            self.cursor_blink_phase = 0.0;
        }

        // Adjust speed based on quality level
        match quality {
            QualityLevel::Low => {
                // Add multiple characters at once for better performance
                for _ in 0..5 {
                    self.add_next_chars();
                    if self.is_complete {
                        break;
                    }
                }
            }
            QualityLevel::Medium => {
                // Add a couple characters
                for _ in 0..2 {
                    self.add_next_chars();
                    if self.is_complete {
                        break;
                    }
                }
            }
            QualityLevel::High => {
                // Smooth single character addition
                self.add_next_chars();
            }
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn render(&self) -> String {
        self.get_display_text()
    }
}

/// Wave text effect where characters appear in a wave pattern
pub struct WaveTextEffect {
    text: String,
    wave_position: f32,
    wave_speed: f32,
    is_complete: bool,
}

impl WaveTextEffect {
    pub fn new(text: String) -> Self {
        Self {
            text,
            wave_position: 0.0,
            wave_speed: 10.0,
            is_complete: false,
        }
    }

    pub fn get_display_text(&self) -> String {
        let mut result = String::new();
        let chars: Vec<char> = self.text.chars().collect();

        for (i, ch) in chars.iter().enumerate() {
            let distance = (i as f32 - self.wave_position).abs();

            if distance < 3.0 {
                // Character is in the wave
                let intensity = 1.0 - (distance / 3.0);

                // Use different characters based on intensity
                let display_char = if intensity > 0.8 {
                    *ch // Full character
                } else if intensity > 0.5 {
                    '▒' // Medium shade
                } else if intensity > 0.2 {
                    '░' // Light shade
                } else {
                    ' ' // Space
                };

                result.push(display_char);
            } else if (i as f32) < self.wave_position - 3.0 {
                // Character has passed through wave
                result.push(*ch);
            } else {
                // Character hasn't been reached yet
                result.push(' ');
            }
        }

        result
    }
}

impl Animation for WaveTextEffect {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.wave_position += self.wave_speed * delta;

        if self.wave_position > self.text.len() as f32 + 5.0 {
            self.is_complete = true;
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn render(&self) -> String {
        self.get_display_text()
    }
}

/// Matrix rain effect for dramatic moments
pub struct MatrixRainEffect {
    width: usize,
    height: usize,
    columns: Vec<MatrixColumn>,
}

struct MatrixColumn {
    position: f32,
    speed: f32,
    chars: Vec<char>,
}

impl MatrixRainEffect {
    pub fn new(width: usize, height: usize) -> Self {
        use rand::prelude::*;
        let mut rng = rand::rng();

        let columns: Vec<MatrixColumn> = (0..width)
            .map(|_| MatrixColumn {
                position: rng.random_range(0.0..height as f32),
                speed: rng.random_range(5.0..15.0),
                chars: Self::random_chars(height),
            })
            .collect();

        Self {
            width,
            height,
            columns,
        }
    }

    fn random_chars(count: usize) -> Vec<char> {
        use rand::Rng;
        let mut rng = rand::rng();
        let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789@#$%^&*";

        // BUG-1005 fix: Add empty check and fallback
        if chars.is_empty() {
            return (0..count).map(|_| ' ').collect();
        }

        (0..count)
            .map(|_| {
                chars
                    .chars()
                    .nth(rng.random_range(0..chars.len()))
                    .unwrap_or('█')
            })
            .collect()
    }
}

impl Animation for MatrixRainEffect {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        use rand::Rng;
        let mut rng = rand::rng();

        for col in &mut self.columns {
            col.position += col.speed * delta;

            if col.position > self.height as f32 {
                col.position = 0.0;
                col.speed = rng.random_range(5.0..15.0);
                col.chars = MatrixRainEffect::random_chars(self.height);
            }
        }
    }

    fn is_complete(&self) -> bool {
        false // Runs forever
    }

    fn render(&self) -> String {
        let mut grid = vec![vec![' '; self.width]; self.height];

        for (x, col) in self.columns.iter().enumerate() {
            let y = col.position as usize;
            if y < self.height {
                // Bright head
                grid[y][x] = col.chars[y % col.chars.len()];

                // Fading tail
                for i in 1..5 {
                    if y >= i {
                        let tail_y = y - i;
                        let fade_char = if i == 1 {
                            col.chars[tail_y % col.chars.len()]
                        } else if i == 2 {
                            '▒'
                        } else if i == 3 {
                            '░'
                        } else {
                            '·'
                        };
                        grid[tail_y][x] = fade_char;
                    }
                }
            }
        }

        grid.iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typewriter_effect() {
        let mut typewriter = TypewriterEffect::new("Hello, World!".to_string());

        // The typewriter effect uses real wall-clock time (Instant::now())
        // so we can't test it with simulated delta time. Instead, test the skip_to_end functionality.
        typewriter.skip_to_end();

        assert_eq!(typewriter.displayed_text, "Hello, World!");
        assert!(typewriter.is_complete());
    }

    #[test]
    fn test_wave_text() {
        let mut wave = WaveTextEffect::new("Test".to_string());

        wave.wave_position = 2.0;
        let display = wave.get_display_text();
        assert!(display.contains(char::is_alphabetic));
    }
}
