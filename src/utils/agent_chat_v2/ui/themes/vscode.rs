//! VS Code Dark theme - familiar and professional

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // VS Code exact color palette
    palette[PaletteColor::Background] = Color::Rgb(30, 30, 30);       // #1e1e1e
    palette[PaletteColor::View] = Color::Rgb(37, 37, 38);            // #252526
    palette[PaletteColor::Primary] = Color::Rgb(0, 122, 204);        // #007acc
    palette[PaletteColor::Secondary] = Color::Rgb(86, 156, 214);     // #569cd6
    palette[PaletteColor::Tertiary] = Color::Rgb(106, 153, 85);      // #6a9955
    palette[PaletteColor::TitlePrimary] = Color::Rgb(212, 212, 212); // #d4d4d4
    palette[PaletteColor::TitleSecondary] = Color::Rgb(86, 156, 214);
    palette[PaletteColor::Highlight] = Color::Rgb(38, 79, 120);      // #264f78
    palette[PaletteColor::HighlightInactive] = Color::Rgb(42, 45, 46);
    palette[PaletteColor::HighlightText] = Color::Rgb(212, 212, 212);

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = true;

    theme
}