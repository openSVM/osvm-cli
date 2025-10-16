//! Dracula Enhanced theme - dark with vibrant colors

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Dracula palette with enhanced contrast
    palette[PaletteColor::Background] = Color::Rgb(40, 42, 54);       // #282a36 - Background
    palette[PaletteColor::View] = Color::Rgb(68, 71, 90);             // #44475a - Current line
    palette[PaletteColor::Primary] = Color::Rgb(189, 147, 249);       // #bd93f9 - Purple
    palette[PaletteColor::Secondary] = Color::Rgb(139, 233, 253);     // #8be9fd - Cyan
    palette[PaletteColor::Tertiary] = Color::Rgb(80, 250, 123);       // #50fa7b - Green
    palette[PaletteColor::TitlePrimary] = Color::Rgb(248, 248, 242);  // #f8f8f2 - Foreground
    palette[PaletteColor::TitleSecondary] = Color::Rgb(189, 147, 249);
    palette[PaletteColor::Highlight] = Color::Rgb(98, 114, 164);      // #6272a4 - Selection
    palette[PaletteColor::HighlightInactive] = Color::Rgb(68, 71, 90);
    palette[PaletteColor::HighlightText] = Color::Rgb(248, 248, 242);

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = true;

    theme
}