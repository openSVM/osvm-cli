//! Smooth progress indicators that feel responsive

use super::{easing, lerp, Animation, QualityLevel};

/// Smooth progress bar with partial fill characters
pub struct SmoothProgressBar {
    current: f32,
    target: f32,
    width: usize,
    speed: f32,
    style: ProgressStyle,
    show_percentage: bool,
    animated_percentage: f32,
}

#[derive(Clone)]
pub enum ProgressStyle {
    Classic,   // [=====>    ]
    Blocks,    // █████░░░░░
    Dots,      // ●●●●●○○○○○
    Arrows,    // ▶▶▶▶▶▷▷▷▷▷
    Gradient,  // ████▓▒░░░░
    Segmented, // [■■■□□]
    Wave,      // ～～～～～
    Neon,      // ████░░░░░░ with color
}

impl SmoothProgressBar {
    pub fn new(width: usize) -> Self {
        Self {
            current: 0.0,
            target: 0.0,
            width,
            speed: 5.0, // Smooth transition speed
            style: ProgressStyle::Blocks,
            show_percentage: true,
            animated_percentage: 0.0,
        }
    }

    pub fn set_progress(&mut self, value: f32) {
        self.target = value.clamp(0.0, 1.0);
    }

    pub fn set_style(&mut self, style: ProgressStyle) {
        self.style = style;
    }

    pub fn set_speed(&mut self, speed: f32) {
        self.speed = speed.max(0.1);
    }

    fn render_bar(&self) -> String {
        match self.style {
            ProgressStyle::Classic => self.render_classic(),
            ProgressStyle::Blocks => self.render_blocks(),
            ProgressStyle::Dots => self.render_dots(),
            ProgressStyle::Arrows => self.render_arrows(),
            ProgressStyle::Gradient => self.render_gradient(),
            ProgressStyle::Segmented => self.render_segmented(),
            ProgressStyle::Wave => self.render_wave(),
            ProgressStyle::Neon => self.render_neon(),
        }
    }

    fn render_classic(&self) -> String {
        let filled = (self.current * self.width as f32) as usize;
        let mut bar = String::from("[");

        for i in 0..self.width {
            if i < filled - 1 {
                bar.push('=');
            } else if i == filled - 1 && filled > 0 {
                bar.push('>');
            } else {
                bar.push(' ');
            }
        }

        bar.push(']');
        bar
    }

    fn render_blocks(&self) -> String {
        let filled_complete = (self.current * self.width as f32) as usize;
        let partial = ((self.current * self.width as f32) % 1.0 * 8.0) as usize;

        let mut bar = String::new();

        // Full blocks
        for _ in 0..filled_complete {
            bar.push('█');
        }

        // Partial block with 8 levels of fill
        if filled_complete < self.width {
            let partial_char = match partial {
                0 => ' ',
                1 => '▏',
                2 => '▎',
                3 => '▍',
                4 => '▌',
                5 => '▋',
                6 => '▊',
                7 => '▉',
                _ => '█',
            };
            bar.push(partial_char);

            // Empty blocks
            for _ in (filled_complete + 1)..self.width {
                bar.push('░');
            }
        }

        bar
    }

    fn render_dots(&self) -> String {
        let filled = (self.current * self.width as f32) as usize;
        let mut bar = String::new();

        for i in 0..self.width {
            if i < filled {
                bar.push('●');
            } else {
                bar.push('○');
            }
        }

        bar
    }

    fn render_arrows(&self) -> String {
        let filled = (self.current * self.width as f32) as usize;
        let mut bar = String::new();

        for i in 0..self.width {
            if i < filled {
                bar.push('▶');
            } else {
                bar.push('▷');
            }
        }

        bar
    }

    fn render_gradient(&self) -> String {
        let position = self.current * self.width as f32;
        let mut bar = String::new();

        for i in 0..self.width {
            let distance = (i as f32 - position).abs();

            let ch = if i as f32 <= position {
                '█'
            } else if distance < 1.0 {
                '▓'
            } else if distance < 2.0 {
                '▒'
            } else if distance < 3.0 {
                '░'
            } else {
                ' '
            };

            bar.push(ch);
        }

        bar
    }

    fn render_segmented(&self) -> String {
        let segments = 10;
        let filled_segments = (self.current * segments as f32) as usize;
        let mut bar = String::from("[");

        for i in 0..segments {
            if i < filled_segments {
                bar.push('■');
            } else {
                bar.push('□');
            }
        }

        bar.push(']');
        bar
    }

    fn render_wave(&self) -> String {
        let position = self.current * self.width as f32;
        let mut bar = String::new();

        for i in 0..self.width {
            if (i as f32) < position {
                // Create wave pattern
                let wave_phase = (i as f32 * 0.5 + self.animated_percentage * 10.0).sin();
                let ch = if wave_phase > 0.5 {
                    '～'
                } else if wave_phase > 0.0 {
                    '〜'
                } else {
                    '∼'
                };
                bar.push(ch);
            } else {
                bar.push('·');
            }
        }

        bar
    }

    fn render_neon(&self) -> String {
        let position = self.current * self.width as f32;
        let mut bar = String::new();

        for i in 0..self.width {
            let distance = (i as f32 - position).abs();

            let ch = if (i as f32) < position - 1.0 {
                '█' // Solid fill
            } else if distance < 1.0 {
                '▓' // Glowing edge
            } else if distance < 2.0 {
                '░' // Glow falloff
            } else {
                '·' // Empty
            };

            bar.push(ch);
        }

        // Add glow effect at the end
        if position > 0.0 && position < self.width as f32 {
            // This would be enhanced with color in a real terminal
        }

        bar
    }
}

impl Animation for SmoothProgressBar {
    fn update(&mut self, delta: f32, quality: QualityLevel) {
        // Smooth interpolation speed based on quality
        let speed = match quality {
            QualityLevel::Low => self.speed * 2.0, // Faster updates
            QualityLevel::Medium => self.speed * 1.5,
            QualityLevel::High => self.speed,
        };

        // Use easing for smooth animation
        let diff = self.target - self.current;
        let change = diff * speed * delta;

        self.current += change;

        // Clamp to avoid overshooting
        if (self.current - self.target).abs() < 0.001 {
            self.current = self.target;
        }

        // Animate percentage display
        self.animated_percentage = lerp(self.animated_percentage, self.current, delta * speed);
    }

    fn is_complete(&self) -> bool {
        (self.current - 1.0).abs() < 0.001
    }

    fn render(&self) -> String {
        let bar = self.render_bar();

        if self.show_percentage {
            format!("{} {:3.0}%", bar, self.animated_percentage * 100.0)
        } else {
            bar
        }
    }
}

/// Circular progress indicator
pub struct CircularProgress {
    progress: f32,
    target: f32,
    radius: usize,
    clockwise: bool,
}

impl CircularProgress {
    pub fn new(radius: usize) -> Self {
        Self {
            progress: 0.0,
            target: 0.0,
            radius,
            clockwise: true,
        }
    }

    pub fn set_progress(&mut self, value: f32) {
        self.target = value.clamp(0.0, 1.0);
    }

    fn get_circle_char(&self, angle: f32) -> char {
        let segment = (angle * 8.0) as usize % 8;
        match segment {
            0 => '⠁',
            1 => '⠉',
            2 => '⠋',
            3 => '⠛',
            4 => '⠟',
            5 => '⠿',
            6 => '⡿',
            7 => '⣿',
            _ => '⣿',
        }
    }
}

impl Animation for CircularProgress {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.progress = lerp(self.progress, self.target, delta * 5.0);
    }

    fn is_complete(&self) -> bool {
        (self.progress - 1.0).abs() < 0.001
    }

    fn render(&self) -> String {
        let angle = self.progress * std::f32::consts::PI * 2.0;

        // Simple ASCII circle
        let segments = 8;
        let mut display = vec!['○'; segments];

        let filled = (self.progress * segments as f32) as usize;
        for i in 0..filled {
            display[i] = '●';
        }

        // Format as circle
        format!(
            "  {} {} \n{} {:3.0}% {}\n  {} {}",
            display[0],
            display[1],
            display[7],
            self.progress * 100.0,
            display[2],
            display[6],
            display[3],
        )
    }
}

/// Step progress for multi-step operations
pub struct StepProgress {
    current_step: usize,
    total_steps: usize,
    steps: Vec<StepInfo>,
}

#[derive(Clone)]
pub struct StepInfo {
    pub name: String,
    pub status: StepStatus,
}

#[derive(Clone, PartialEq)]
pub enum StepStatus {
    Pending,
    Active,
    Complete,
    Failed,
    Skipped,
}

impl StepProgress {
    pub fn new(steps: Vec<String>) -> Self {
        let step_infos = steps
            .into_iter()
            .map(|name| StepInfo {
                name,
                status: StepStatus::Pending,
            })
            .collect();

        Self {
            current_step: 0,
            total_steps: 0,
            steps: step_infos,
        }
    }

    pub fn advance(&mut self) {
        if self.current_step < self.steps.len() {
            self.steps[self.current_step].status = StepStatus::Complete;
            self.current_step += 1;

            if self.current_step < self.steps.len() {
                self.steps[self.current_step].status = StepStatus::Active;
            }
        }
    }

    pub fn fail_current(&mut self) {
        if self.current_step < self.steps.len() {
            self.steps[self.current_step].status = StepStatus::Failed;
        }
    }
}

impl Animation for StepProgress {
    fn update(&mut self, _delta: f32, _quality: QualityLevel) {
        // Steps are manually controlled, no automatic updates
    }

    fn is_complete(&self) -> bool {
        self.current_step >= self.steps.len()
    }

    fn render(&self) -> String {
        self.steps
            .iter()
            .enumerate()
            .map(|(i, step)| {
                let icon = match step.status {
                    StepStatus::Pending => "○",
                    StepStatus::Active => "◉",
                    StepStatus::Complete => "✓",
                    StepStatus::Failed => "✗",
                    StepStatus::Skipped => "—",
                };

                format!("{} {} {}", icon, i + 1, step.name)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_smooth_progress() {
        let mut progress = SmoothProgressBar::new(10);
        progress.set_progress(0.5);

        for _ in 0..10 {
            progress.update(0.1, QualityLevel::High);
        }

        assert!((progress.current - 0.5).abs() < 0.1);
    }

    #[test]
    fn test_step_progress() {
        let mut steps = StepProgress::new(vec![
            "Step 1".to_string(),
            "Step 2".to_string(),
            "Step 3".to_string(),
        ]);

        steps.advance();
        assert_eq!(steps.current_step, 1);

        let rendered = steps.render();
        assert!(rendered.contains("✓"));
    }
}
