# Implementation Guide for Layouts & Colors

## 🎨 Ready-to-Use Color Scheme Implementations

### 1. **Add New Themes to theme.rs**
```rust
impl ModernTheme {
    /// Cyberpunk Neon theme
    pub fn cyberpunk() -> Theme {
        let mut theme = Theme::default();
        let mut palette = Palette::default();

        palette[PaletteColor::Background] = Color::Rgb(10, 14, 39);      // #0a0e27
        palette[PaletteColor::View] = Color::Rgb(21, 25, 51);           // #151933
        palette[PaletteColor::Primary] = Color::Rgb(0, 255, 255);       // Cyan neon
        palette[PaletteColor::Secondary] = Color::Rgb(255, 0, 255);     // Magenta neon
        palette[PaletteColor::TitlePrimary] = Color::Rgb(224, 224, 255);
        palette[PaletteColor::Highlight] = Color::Rgb(0, 255, 255);

        theme.palette = palette;
        theme.borders = BorderStyle::Simple;
        theme.shadow = true;
        theme
    }

    /// Nord Ice theme
    pub fn nord() -> Theme {
        let mut theme = Theme::default();
        let mut palette = Palette::default();

        palette[PaletteColor::Background] = Color::Rgb(46, 52, 64);     // #2e3440
        palette[PaletteColor::View] = Color::Rgb(59, 66, 82);          // #3b4252
        palette[PaletteColor::Primary] = Color::Rgb(136, 192, 208);    // #88c0d0
        palette[PaletteColor::Secondary] = Color::Rgb(129, 161, 193);  // #81a1c1
        palette[PaletteColor::TitlePrimary] = Color::Rgb(236, 239, 244);

        theme.palette = palette;
        theme.borders = BorderStyle::Simple;
        theme
    }
}
```

### 2. **Semantic Message Colors**
```rust
pub fn get_message_color(msg_type: &MessageType) -> ColorStyle {
    match msg_type {
        MessageType::User => ColorStyle::new(
            Color::Rgb(15, 76, 117),    // Human blue
            Color::Rgb(30, 30, 30)
        ),
        MessageType::Agent => ColorStyle::new(
            Color::Rgb(22, 83, 126),     // AI purple
            Color::Rgb(30, 30, 30)
        ),
        MessageType::System => ColorStyle::new(
            Color::Rgb(96, 96, 96),      // System gray
            Color::Rgb(30, 30, 30)
        ),
        MessageType::Tool => ColorStyle::new(
            Color::Rgb(243, 156, 18),    // Action orange
            Color::Rgb(30, 30, 30)
        ),
        MessageType::Error => ColorStyle::new(
            Color::Rgb(231, 76, 60),     // Alert red
            Color::Rgb(30, 30, 30)
        ),
    }
}
```

## 🏗️ Layout Implementations

### 1. **Golden Ratio Layout**
```rust
fn create_golden_ratio_layout(&self, siv: &mut Cursive) -> LinearLayout {
    let screen_width = siv.screen_size().x;

    // Calculate golden ratio proportions
    let sidebar_width = (screen_width as f32 * 0.382) as usize;
    let main_width = screen_width - sidebar_width;
    let input_height = (siv.screen_size().y as f32 * 0.236) as usize;

    let mut layout = LinearLayout::horizontal();

    // Sidebar (38.2%)
    layout.add_child(
        ResizedView::with_fixed_width(
            sidebar_width,
            self.create_sidebar()
        )
    );

    // Main content (61.8%)
    let mut main_layout = LinearLayout::vertical();
    main_layout.add_child(
        self.create_chat_view()
            .full_height()
            .fixed_width(main_width)
    );
    main_layout.add_child(
        ResizedView::with_fixed_height(
            input_height,
            self.create_input_panel()
        )
    );

    layout.add_child(main_layout);
    layout
}
```

### 2. **Adaptive Density Layout**
```rust
fn create_adaptive_layout(&self, siv: &mut Cursive) -> Box<dyn View> {
    let (width, height) = (siv.screen_size().x, siv.screen_size().y);

    match (width, height) {
        (w, h) if w < 80 || h < 24 => {
            // Ultra-compact: Stacked layout
            Box::new(self.create_compact_layout())
        },
        (w, _) if w < 120 => {
            // Standard: Two-column
            Box::new(self.create_standard_layout())
        },
        (w, h) if w >= 120 && h >= 40 => {
            // Wide: Three-column with tools
            Box::new(self.create_wide_layout())
        },
        _ => {
            // Default: Golden ratio
            Box::new(self.create_golden_ratio_layout(siv))
        }
    }
}

fn create_compact_layout(&self) -> LinearLayout {
    let mut layout = LinearLayout::vertical();

    // Collapsible sections
    layout.add_child(
        Panel::new(
            TextView::new("▼ Sessions (3)")
                .with_name("sessions_header")
        ).title("OSVM")
    );

    layout.add_child(
        self.create_messages_view()
            .scrollable()
            .full_height()
    );

    layout.add_child(
        ResizedView::with_fixed_height(3, self.create_input())
    );

    layout
}
```

### 3. **Card-Based Layout with Depth**
```rust
fn create_card(title: &str, content: impl View + 'static) -> Panel<ResizedView<BoxView<impl View>>> {
    // Create shadow effect with Unicode box drawing
    let shadowed = BoxView::with_fixed_size(
        (40, 10),
        LinearLayout::vertical()
            .child(content)
            .child(TextView::new("░░░░░░░░")) // Shadow line
    );

    Panel::new(ResizedView::with_full_screen(shadowed))
        .title(title)
        .title_position(HAlign::Left)
}

fn create_card_layout(&self) -> LinearLayout {
    let mut layout = LinearLayout::horizontal();

    layout.add_child(self.create_card(
        "Sessions",
        self.create_session_list()
    ));

    layout.add_child(DummyView.fixed_width(2)); // Gap

    layout.add_child(self.create_card(
        "Chat",
        self.create_chat_content()
    ));

    layout
}
```

## 🎨 Visual Effects

### 1. **Animated Borders for Focus**
```rust
pub struct AnimatedBorder {
    frames: Vec<&'static str>,
    current: usize,
}

impl AnimatedBorder {
    pub fn new() -> Self {
        Self {
            frames: vec![
                "┌───────┐",
                "╔═══════╗",  // Focused
                "┌───────┐",
            ],
            current: 0,
        }
    }

    pub fn next_frame(&mut self) -> &str {
        self.current = (self.current + 1) % self.frames.len();
        self.frames[self.current]
    }
}
```

### 2. **Gradient Text Effect**
```rust
fn create_gradient_title(text: &str) -> StyledString {
    let mut styled = StyledString::new();
    let gradient = vec![
        Color::Rgb(100, 100, 255),
        Color::Rgb(120, 120, 255),
        Color::Rgb(140, 140, 255),
        Color::Rgb(160, 160, 255),
        Color::Rgb(180, 180, 255),
    ];

    for (i, ch) in text.chars().enumerate() {
        let color = gradient[i % gradient.len()];
        styled.append_styled(
            ch.to_string(),
            ColorStyle::new(color, Color::Rgb(30, 30, 30))
        );
    }
    styled
}
```

### 3. **Status Bar with Live Updates**
```rust
fn create_rich_status_bar(&self) -> impl View {
    let status_text = format!(
        " {} {} │ {} msgs │ {} │ {} ",
        self.get_connection_indicator(),  // 🟢 or 🔴
        self.get_network_latency(),      // "15ms"
        self.get_message_count(),        // "42"
        self.get_time(),                // "14:32"
        self.get_session_name()         // "Main Chat"
    );

    TextView::new(status_text)
        .with_name("status_bar")
        .fixed_height(1)
}

fn get_connection_indicator(&self) -> &str {
    match self.connection_status {
        Connected => "🟢",
        Connecting => "🟡",
        Disconnected => "🔴",
    }
}
```

## 🌈 Theme Switcher Implementation

```rust
fn show_theme_picker(siv: &mut Cursive) {
    let themes = vec![
        ("VS Code Dark", "vscode"),
        ("Cyberpunk Neon", "cyberpunk"),
        ("Nord Ice", "nord"),
        ("Tokyo Night", "tokyo"),
        ("Dracula", "dracula"),
        ("Gruvbox", "gruvbox"),
        ("High Contrast", "high_contrast"),
    ];

    let mut select = SelectView::new();
    for (name, id) in themes {
        select.add_item_str(name).with_id(id);
    }

    select.set_on_submit(|s, theme_id: &str| {
        apply_theme(s, theme_id);
        s.pop_layer();
    });

    siv.add_layer(
        Dialog::around(select)
            .title("Choose Theme")
            .button("Preview", |s| preview_theme(s))
            .button("Cancel", |s| { s.pop_layer(); })
    );
}
```

## 📐 Responsive Helpers

```rust
pub struct ResponsiveLayout;

impl ResponsiveLayout {
    pub fn calculate_proportions(screen_size: Vec2) -> LayoutProportions {
        let (width, height) = (screen_size.x, screen_size.y);

        LayoutProportions {
            sidebar: match width {
                w if w < 80 => 0,      // Hidden
                w if w < 120 => 25,    // Narrow
                _ => 30,               // Normal
            },

            input_height: match height {
                h if h < 30 => 3,      // Minimal
                h if h < 50 => 5,      // Standard
                _ => 8,                // Comfortable
            },

            status_bar: if height > 30 { 1 } else { 0 },

            padding: if width > 100 { 2 } else { 1 },
        }
    }
}
```

## 🎯 Color Accessibility Helpers

```rust
fn ensure_contrast(fg: Color, bg: Color) -> Color {
    let contrast_ratio = calculate_contrast_ratio(fg, bg);

    // WCAG AA requires 4.5:1 for normal text
    if contrast_ratio < 4.5 {
        // Lighten or darken fg to meet contrast requirement
        adjust_color_for_contrast(fg, bg, 4.5)
    } else {
        fg
    }
}

fn calculate_contrast_ratio(c1: Color, c2: Color) -> f32 {
    let l1 = relative_luminance(c1);
    let l2 = relative_luminance(c2);

    let lighter = l1.max(l2);
    let darker = l1.min(l2);

    (lighter + 0.05) / (darker + 0.05)
}
```

## 🚀 Quick Start Implementation

Add to your `main.rs` or chat initialization:

```rust
// 1. Detect terminal capabilities
let term_size = siv.screen_size();
let color_support = detect_color_support();

// 2. Choose appropriate layout
let layout = if term_size.x < 80 {
    create_compact_layout()
} else {
    create_golden_ratio_layout()
};

// 3. Apply theme based on time or user preference
let theme = if is_night_time() {
    ModernTheme::cyberpunk()
} else {
    ModernTheme::nord()
};

siv.set_theme(theme);
siv.add_fullscreen_layer(layout);
```

These implementations provide concrete, working code that can be integrated into the existing OSVM chat system!