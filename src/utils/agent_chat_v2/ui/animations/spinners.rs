//! Various spinner animations for loading states - keeps users engaged

use super::{Animation, QualityLevel};

/// Collection of spinner animations
#[derive(Clone)]
pub enum SpinnerType {
    Dots,
    Circle,
    Blocks,
    Arrows,
    Braille,
    Pulse,
    Bounce,
    Wave,
    Clock,
    Moon,
    Weather,
    BoxBounce,
    StarWars,
    Custom(Vec<String>),
}

impl SpinnerType {
    /// Get frames for the spinner
    pub fn frames(&self) -> Vec<&str> {
        match self {
            Self::Dots => vec!["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"],
            Self::Circle => vec!["◐", "◓", "◑", "◒"],
            Self::Blocks => vec!["▖", "▘", "▝", "▗"],
            Self::Arrows => vec!["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"],
            Self::Braille => vec!["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"],
            Self::Pulse => vec!["○", "◔", "◑", "◕", "●", "◕", "◑", "◔"],
            Self::Bounce => vec!["⠁", "⠂", "⠄", "⡀", "⢀", "⠠", "⠐", "⠈"],
            Self::Wave => vec!["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃"],
            Self::Clock => vec!["🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚", "🕛"],
            Self::Moon => vec!["🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘"],
            Self::Weather => vec!["☀️", "☀️", "☁️", "⛅", "☁️", "🌧️", "⛈️", "🌧️", "☁️", "⛅"],
            Self::BoxBounce => vec!["▌", "▀", "▐", "▄"],
            Self::StarWars => vec!["|", "/", "-", "\\"],
            Self::Custom(frames) => {
                // Return references to the custom frames
                // This is a bit tricky with ownership, so we'll handle it differently
                vec!["custom"] // Placeholder
            }
        }
    }

    /// Get interval between frames in milliseconds
    pub fn interval(&self) -> u64 {
        match self {
            Self::Dots => 80,
            Self::Circle => 120,
            Self::Blocks => 120,
            Self::Arrows => 100,
            Self::Braille => 80,
            Self::Pulse => 100,
            Self::Bounce => 100,
            Self::Wave => 80,
            Self::Clock => 100,
            Self::Moon => 200,
            Self::Weather => 500,
            Self::BoxBounce => 120,
            Self::StarWars => 100,
            Self::Custom(_) => 100,
        }
    }
}

/// Basic spinner animation
pub struct Spinner {
    spinner_type: SpinnerType,
    current_frame: usize,
    frame_time: f32,
    label: String,
    is_complete: bool,
}

impl Spinner {
    pub fn new(spinner_type: SpinnerType, label: String) -> Self {
        Self {
            spinner_type,
            current_frame: 0,
            frame_time: 0.0,
            label,
            is_complete: false,
        }
    }

    pub fn stop(&mut self) {
        self.is_complete = true;
    }

    pub fn set_label(&mut self, label: String) {
        self.label = label;
    }
}

impl Animation for Spinner {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.frame_time += delta * 1000.0; // Convert to milliseconds

        if self.frame_time >= self.spinner_type.interval() as f32 {
            self.frame_time = 0.0;
            let frames = self.spinner_type.frames();
            self.current_frame = (self.current_frame + 1) % frames.len();
        }
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    fn render(&self) -> String {
        let frames = self.spinner_type.frames();
        let frame = frames[self.current_frame];
        format!("{} {}", frame, self.label)
    }
}

/// Multi-spinner for showing parallel operations
pub struct MultiSpinner {
    spinners: Vec<(Spinner, bool)>, // (spinner, is_done)
}

impl MultiSpinner {
    pub fn new() -> Self {
        Self {
            spinners: Vec::new(),
        }
    }

    pub fn add(&mut self, label: String) -> usize {
        self.spinners.push((
            Spinner::new(SpinnerType::Dots, label),
            false,
        ));
        self.spinners.len() - 1
    }

    pub fn complete(&mut self, index: usize) {
        if let Some((spinner, done)) = self.spinners.get_mut(index) {
            spinner.stop();
            *done = true;
        }
    }

    pub fn render_all(&self) -> String {
        self.spinners
            .iter()
            .map(|(spinner, done)| {
                if *done {
                    format!("✓ {}", spinner.label)
                } else {
                    spinner.render()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Animation for MultiSpinner {
    fn update(&mut self, delta: f32, quality: QualityLevel) {
        for (spinner, done) in &mut self.spinners {
            if !*done {
                spinner.update(delta, quality);
            }
        }
    }

    fn is_complete(&self) -> bool {
        self.spinners.iter().all(|(_, done)| *done)
    }

    fn render(&self) -> String {
        self.render_all()
    }
}

/// DNA helix spinner for biotech/scientific feel
pub struct HelixSpinner {
    phase: f32,
    label: String,
}

impl HelixSpinner {
    pub fn new(label: String) -> Self {
        Self { phase: 0.0, label }
    }

    fn get_helix_frame(&self) -> String {
        let frames = vec![
            "     ╔════╗",
            "    ╔╝────╚╗",
            "   ╔╝──────╚╗",
            "  ╔╝────────╚╗",
            " ╔╝──────────╚╗",
            "╔╝────────────╚╗",
            "╚╗────────────╔╝",
            " ╚╗──────────╔╝",
            "  ╚╗────────╔╝",
            "   ╚╗──────╔╝",
            "    ╚╗────╔╝",
            "     ╚════╝",
        ];

        let index = (self.phase * frames.len() as f32) as usize % frames.len();
        frames[index].to_string()
    }
}

impl Animation for HelixSpinner {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.phase += delta * 2.0; // Rotate at 2 revolutions per second
        if self.phase > 1.0 {
            self.phase -= 1.0;
        }
    }

    fn is_complete(&self) -> bool {
        false // Runs forever
    }

    fn render(&self) -> String {
        format!("{} {}", self.get_helix_frame(), self.label)
    }
}

/// Orbital spinner - electrons orbiting nucleus
pub struct OrbitalSpinner {
    electron_positions: Vec<f32>,
    label: String,
}

impl OrbitalSpinner {
    pub fn new(label: String) -> Self {
        Self {
            electron_positions: vec![0.0, 0.33, 0.66], // Three electrons
            label,
        }
    }

    fn render_orbital(&self) -> String {
        let orbit_chars = vec!['·', '•', '●', '○'];
        let mut display = vec![' '; 8];

        for &pos in &self.electron_positions {
            let index = (pos * 8.0) as usize % 8;
            display[index] = '●';
        }

        format!(
            "  {}{} \n {} ⚛ {}\n  {} {}",
            display[0],
            display[1],
            display[7],
            display[2],
            display[6],
            display[3],
        )
    }
}

impl Animation for OrbitalSpinner {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        for (i, pos) in self.electron_positions.iter_mut().enumerate() {
            let speed = 1.0 + (i as f32 * 0.3); // Different speeds for each electron
            *pos += delta * speed;
            if *pos > 1.0 {
                *pos -= 1.0;
            }
        }
    }

    fn is_complete(&self) -> bool {
        false
    }

    fn render(&self) -> String {
        format!("{}\n{}", self.render_orbital(), self.label)
    }
}

/// Pulsating text for emphasis
pub struct PulsingText {
    text: String,
    phase: f32,
    speed: f32,
}

impl PulsingText {
    pub fn new(text: String) -> Self {
        Self {
            text,
            phase: 0.0,
            speed: 2.0,
        }
    }

    fn get_intensity(&self) -> f32 {
        (self.phase * std::f32::consts::PI * 2.0).sin() * 0.5 + 0.5
    }

    fn intensity_to_char(&self, intensity: f32) -> &str {
        match (intensity * 5.0) as usize {
            0 => "░",
            1 => "▒",
            2 => "▓",
            3 => "█",
            _ => "▓",
        }
    }
}

impl Animation for PulsingText {
    fn update(&mut self, delta: f32, _quality: QualityLevel) {
        self.phase += delta * self.speed;
        if self.phase > 1.0 {
            self.phase -= 1.0;
        }
    }

    fn is_complete(&self) -> bool {
        false
    }

    fn render(&self) -> String {
        let intensity = self.get_intensity();
        let border = self.intensity_to_char(intensity);

        format!(
            "{0}{0} {1} {0}{0}",
            border,
            self.text
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spinner_animation() {
        let mut spinner = Spinner::new(SpinnerType::Dots, "Loading...".to_string());

        for _ in 0..10 {
            spinner.update(0.1, QualityLevel::High);
            let rendered = spinner.render();
            assert!(rendered.contains("Loading..."));
        }
    }

    #[test]
    fn test_multi_spinner() {
        let mut multi = MultiSpinner::new();
        let idx1 = multi.add("Task 1".to_string());
        let idx2 = multi.add("Task 2".to_string());

        multi.update(0.1, QualityLevel::High);
        let output = multi.render_all();
        assert!(output.contains("Task 1"));
        assert!(output.contains("Task 2"));

        multi.complete(idx1);
        let output = multi.render_all();
        assert!(output.contains("✓ Task 1"));
    }
}