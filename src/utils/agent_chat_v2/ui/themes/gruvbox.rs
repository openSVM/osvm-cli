//! Gruvbox Material theme - retro groove with modern refinements

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Gruvbox Material palette - warm and comfortable
    palette[PaletteColor::Background] = Color::Rgb(29, 32, 33); // #1d2021 - Hard dark
    palette[PaletteColor::View] = Color::Rgb(40, 40, 40); // #282828 - Soft dark
    palette[PaletteColor::Primary] = Color::Rgb(131, 165, 152); // #83a598 - Aqua
    palette[PaletteColor::Secondary] = Color::Rgb(211, 134, 155); // #d3869b - Purple
    palette[PaletteColor::Tertiary] = Color::Rgb(184, 187, 38); // #b8bb26 - Green
    palette[PaletteColor::TitlePrimary] = Color::Rgb(235, 219, 178); // #ebdbb2 - Light
    palette[PaletteColor::TitleSecondary] = Color::Rgb(131, 165, 152);
    palette[PaletteColor::Highlight] = Color::Rgb(80, 73, 69); // #504945 - Selection
    palette[PaletteColor::HighlightInactive] = Color::Rgb(60, 56, 54);
    palette[PaletteColor::HighlightText] = Color::Rgb(235, 219, 178);

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = false; // Clean retro look

    theme
}
