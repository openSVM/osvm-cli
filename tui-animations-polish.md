# TUI Animations & Polish for OSVM Chat

## ✨ Micro-Animations for Terminal UI

### 1. **Smooth Color Transitions**
Instead of instant color changes, fade between colors:
```rust
pub struct ColorFader {
    start: Color,
    end: Color,
    steps: usize,
    current: usize,
}

impl ColorFader {
    pub fn new(from: Color, to: Color) -> Self {
        Self {
            start: from,
            end: to,
            steps: 10,  // 10 frames for smooth transition
            current: 0,
        }
    }

    pub fn next_frame(&mut self) -> Color {
        if self.current >= self.steps {
            return self.end;
        }

        self.current += 1;
        let progress = self.current as f32 / self.steps as f32;

        // Linear interpolation between colors
        match (self.start, self.end) {
            (Color::Rgb(r1, g1, b1), Color::Rgb(r2, g2, b2)) => {
                Color::Rgb(
                    lerp(r1, r2, progress),
                    lerp(g1, g2, progress),
                    lerp(b1, b2, progress),
                )
            },
            _ => self.end,
        }
    }
}

fn lerp(start: u8, end: u8, t: f32) -> u8 {
    (start as f32 + (end as f32 - start as f32) * t) as u8
}
```

### 2. **Typing Animation for Agent Responses**
```rust
pub struct TypewriterEffect {
    full_text: String,
    displayed: String,
    char_delay_ms: u64,
    last_update: Instant,
}

impl TypewriterEffect {
    pub fn update(&mut self) -> bool {
        if self.displayed.len() >= self.full_text.len() {
            return false;  // Animation complete
        }

        if self.last_update.elapsed().as_millis() >= self.char_delay_ms as u128 {
            // Add next character
            let next_char = self.full_text.chars()
                .nth(self.displayed.len())
                .unwrap();
            self.displayed.push(next_char);
            self.last_update = Instant::now();
            true
        } else {
            false
        }
    }
}
```

### 3. **Smooth Progress Bars**
```rust
pub struct SmoothProgress {
    current: f32,
    target: f32,
    speed: f32,  // How fast to animate (0.0 to 1.0)
}

impl SmoothProgress {
    pub fn update(&mut self) -> String {
        // Smooth interpolation
        self.current += (self.target - self.current) * self.speed;

        let width = 20;
        let filled = (self.current * width as f32) as usize;
        let partial = ((self.current * width as f32) % 1.0 * 8.0) as usize;

        let mut bar = String::new();

        // Full blocks
        for _ in 0..filled {
            bar.push('█');
        }

        // Partial block (8 levels of fill)
        if filled < width {
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
            for _ in (filled + 1)..width {
                bar.push('░');
            }
        }

        format!("[{}] {:.1}%", bar, self.current * 100.0)
    }
}
```

### 4. **Pulsing Indicators**
```rust
pub struct PulseAnimation {
    phase: f32,
    speed: f32,
}

impl PulseAnimation {
    pub fn update(&mut self) -> String {
        self.phase += self.speed;
        let brightness = (self.phase.sin() + 1.0) / 2.0;  // 0.0 to 1.0

        // Use different characters based on brightness
        match (brightness * 5.0) as usize {
            0 => "○",
            1 => "◔",
            2 => "◑",
            3 => "◕",
            4 => "●",
            _ => "●",
        }.to_string()
    }

    pub fn get_color(&self) -> Color {
        let brightness = (self.phase.sin() + 1.0) / 2.0;
        let value = (55 + (200.0 * brightness) as u8);
        Color::Rgb(value, value, value)
    }
}
```

### 5. **Wave Loading Animation**
```rust
pub struct WaveLoader {
    positions: Vec<f32>,
    time: f32,
}

impl WaveLoader {
    pub fn new(length: usize) -> Self {
        Self {
            positions: vec![0.0; length],
            time: 0.0,
        }
    }

    pub fn update(&mut self) -> String {
        self.time += 0.2;

        let mut result = String::new();
        for i in 0..self.positions.len() {
            let phase = self.time + (i as f32 * 0.5);
            let height = (phase.sin() + 1.0) / 2.0;

            let ch = match (height * 8.0) as usize {
                0 => '⠀',
                1 => '⠁',
                2 => '⠃',
                3 => '⠇',
                4 => '⠏',
                5 => '⠟',
                6 => '⠿',
                7 => '⡿',
                _ => '⣿',
            };
            result.push(ch);
        }
        result
    }
}
```

## 🎬 Transition Effects

### 1. **Panel Slide Transitions**
```rust
pub struct SlideTransition {
    from_pos: i32,
    to_pos: i32,
    current: f32,
    duration: Duration,
    start_time: Instant,
}

impl SlideTransition {
    pub fn update(&mut self) -> i32 {
        let elapsed = self.start_time.elapsed().as_secs_f32();
        let progress = (elapsed / self.duration.as_secs_f32()).min(1.0);

        // Ease-in-out curve
        let eased = ease_in_out(progress);

        let current = self.from_pos as f32 +
                     (self.to_pos - self.from_pos) as f32 * eased;

        current as i32
    }
}

fn ease_in_out(t: f32) -> f32 {
    if t < 0.5 {
        2.0 * t * t
    } else {
        -1.0 + (4.0 - 2.0 * t) * t
    }
}
```

### 2. **Fade In/Out for Messages**
```rust
pub struct FadeEffect {
    alpha: f32,
    direction: FadeDirection,
    speed: f32,
}

impl FadeEffect {
    pub fn update(&mut self) -> Option<Color> {
        match self.direction {
            FadeDirection::In => {
                self.alpha = (self.alpha + self.speed).min(1.0);
            },
            FadeDirection::Out => {
                self.alpha = (self.alpha - self.speed).max(0.0);
            }
        }

        // Convert alpha to terminal-compatible representation
        let gray_level = (self.alpha * 255.0) as u8;
        Some(Color::Rgb(gray_level, gray_level, gray_level))
    }

    pub fn apply_to_text(&self, text: &str) -> String {
        // Use Unicode combining characters for fade effect
        if self.alpha < 0.3 {
            format!("{}̶", text)  // Strikethrough for very faded
        } else if self.alpha < 0.7 {
            format!("{}̃", text)  // Tilde overlay for medium fade
        } else {
            text.to_string()
        }
    }
}
```

## 🌟 Polish Details

### 1. **Smart Cursor Parking**
```rust
// After user action, intelligently position cursor
pub fn smart_cursor_position(context: &ActionContext) -> CursorTarget {
    match context.last_action {
        Action::SendMessage => {
            if context.has_error {
                CursorTarget::ErrorDialog
            } else if context.suggestions_available {
                CursorTarget::FirstSuggestion
            } else {
                CursorTarget::InputField
            }
        },
        Action::SelectSession => CursorTarget::ChatView,
        Action::OpenMenu => CursorTarget::FirstMenuItem,
        _ => CursorTarget::InputField,
    }
}
```

### 2. **Contextual Sound Effects (Optional)**
```rust
pub struct SoundEffects {
    enabled: bool,
}

impl SoundEffects {
    pub fn play(&self, effect: SoundEffect) {
        if !self.enabled { return; }

        let sequence = match effect {
            SoundEffect::MessageSent => "\x07",     // Bell
            SoundEffect::MessageReceived => "\x07",
            SoundEffect::Error => "\x07\x07",       // Double bell
            _ => "",
        };

        print!("{}", sequence);
    }
}
```

### 3. **Smooth Scrolling**
```rust
pub struct SmoothScroller {
    current_line: f32,
    target_line: usize,
    speed: f32,
}

impl SmoothScroller {
    pub fn update(&mut self) -> usize {
        let diff = self.target_line as f32 - self.current_line;
        self.current_line += diff * self.speed;

        // Snap to target when very close
        if diff.abs() < 0.1 {
            self.current_line = self.target_line as f32;
        }

        self.current_line as usize
    }
}
```

### 4. **Attention Grabbers**
```rust
pub struct AttentionEffect {
    blink_state: bool,
    shake_offset: i32,
    glow_intensity: f32,
}

impl AttentionEffect {
    pub fn blink(&mut self) -> &str {
        self.blink_state = !self.blink_state;
        if self.blink_state { "▶" } else { " " }
    }

    pub fn shake(&mut self) -> i32 {
        // Small random offset for shake effect
        self.shake_offset = match self.shake_offset {
            0 => 1,
            1 => -1,
            _ => 0,
        };
        self.shake_offset
    }

    pub fn glow(&mut self) -> String {
        self.glow_intensity = (self.glow_intensity + 0.1).min(1.0);

        // Create glow effect with Unicode
        match (self.glow_intensity * 3.0) as usize {
            0 => "░",
            1 => "▒",
            2 => "▓",
            _ => "█",
        }.to_string()
    }
}
```

## 🎯 Performance Optimization

### 1. **Frame Rate Control**
```rust
pub struct FrameRateController {
    target_fps: u32,
    last_frame: Instant,
}

impl FrameRateController {
    pub fn should_update(&mut self) -> bool {
        let frame_duration = Duration::from_millis(1000 / self.target_fps as u64);

        if self.last_frame.elapsed() >= frame_duration {
            self.last_frame = Instant::now();
            true
        } else {
            false
        }
    }
}
```

### 2. **Dirty Rectangle Tracking**
```rust
pub struct DirtyRegion {
    regions: Vec<Rect>,
}

impl DirtyRegion {
    pub fn mark_dirty(&mut self, area: Rect) {
        self.regions.push(area);
    }

    pub fn needs_redraw(&self, area: &Rect) -> bool {
        self.regions.iter().any(|r| r.overlaps(area))
    }

    pub fn clear(&mut self) {
        self.regions.clear();
    }
}
```

## 🌈 Putting It All Together

```rust
pub struct AnimatedChat {
    color_fader: ColorFader,
    typewriter: TypewriterEffect,
    progress: SmoothProgress,
    loader: WaveLoader,
    scroller: SmoothScroller,
    frame_controller: FrameRateController,
}

impl AnimatedChat {
    pub fn update(&mut self, siv: &mut Cursive) {
        if !self.frame_controller.should_update() {
            return;  // Skip frame for performance
        }

        // Update all animations
        let new_color = self.color_fader.next_frame();
        let typing_done = self.typewriter.update();
        let progress_bar = self.progress.update();
        let loading = self.loader.update();
        let scroll_pos = self.scroller.update();

        // Apply updates to UI
        siv.call_on_name("chat_view", |view: &mut TextView| {
            view.set_content(self.typewriter.displayed.clone());
        });

        siv.call_on_name("progress", |view: &mut TextView| {
            view.set_content(progress_bar);
        });

        siv.call_on_name("loader", |view: &mut TextView| {
            view.set_content(loading);
        });

        // Request refresh
        siv.refresh();
    }
}
```

## 💎 Final Polish Checklist

- [ ] **Smooth animations** at 30 FPS minimum
- [ ] **Color transitions** for state changes
- [ ] **Typewriter effect** for AI responses
- [ ] **Progress indicators** with smooth interpolation
- [ ] **Attention effects** for important events
- [ ] **Smart cursor positioning** after actions
- [ ] **Frame rate limiting** for CPU efficiency
- [ ] **Dirty region tracking** for minimal redraws
- [ ] **Graceful degradation** on slow terminals
- [ ] **Accessibility mode** without animations

These micro-animations and polish details will make your TUI feel as smooth and responsive as a modern GUI application!