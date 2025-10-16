//! Tokyo Night Storm theme - modern Japanese-inspired aesthetics

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Tokyo Night Storm palette
    palette[PaletteColor::Background] = Color::Rgb(36, 40, 59);       // #24283b - Storm night
    palette[PaletteColor::View] = Color::Rgb(31, 35, 53);             // #1f2335 - Deep storm
    palette[PaletteColor::Primary] = Color::Rgb(122, 162, 247);       // #7aa2f7 - Sky blue
    palette[PaletteColor::Secondary] = Color::Rgb(187, 154, 247);     // #bb9af7 - Wisteria
    palette[PaletteColor::Tertiary] = Color::Rgb(158, 206, 106);      // #9ece6a - Spring green
    palette[PaletteColor::TitlePrimary] = Color::Rgb(169, 177, 214);  // #a9b1d6 - Moon white
    palette[PaletteColor::TitleSecondary] = Color::Rgb(122, 162, 247);
    palette[PaletteColor::Highlight] = Color::Rgb(41, 50, 92);        // #29325c - Selection
    palette[PaletteColor::HighlightInactive] = Color::Rgb(86, 95, 137);
    palette[PaletteColor::HighlightText] = Color::Rgb(195, 202, 255);

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = true;

    theme
}