//! Cyberpunk Neon theme - high contrast with electric vibes

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Deep space background with neon accents
    palette[PaletteColor::Background] = Color::Rgb(10, 14, 39);       // #0a0e27 - Deep space
    palette[PaletteColor::View] = Color::Rgb(21, 25, 51);             // #151933 - Raised surface
    palette[PaletteColor::Primary] = Color::Rgb(0, 255, 255);         // #00ffff - Cyan neon
    palette[PaletteColor::Secondary] = Color::Rgb(255, 0, 255);       // #ff00ff - Magenta neon
    palette[PaletteColor::Tertiary] = Color::Rgb(0, 255, 0);          // #00ff00 - Green neon
    palette[PaletteColor::TitlePrimary] = Color::Rgb(224, 224, 255);  // #e0e0ff - Soft blue-white
    palette[PaletteColor::TitleSecondary] = Color::Rgb(0, 255, 255);
    palette[PaletteColor::Highlight] = Color::Rgb(255, 0, 255);       // Magenta highlight
    palette[PaletteColor::HighlightInactive] = Color::Rgb(42, 63, 95);
    palette[PaletteColor::HighlightText] = Color::Rgb(0, 0, 0);       // Black on neon

    theme.palette = palette;
    theme.borders = BorderStyle::Simple; // Could use Double for more impact
    theme.shadow = true;

    theme
}