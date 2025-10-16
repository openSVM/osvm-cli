//! Solarized Pro theme - precision engineered colors

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Solarized Dark with enhanced contrast
    palette[PaletteColor::Background] = Color::Rgb(0, 43, 54);        // #002b36 - Base03
    palette[PaletteColor::View] = Color::Rgb(7, 54, 66);              // #073642 - Base02
    palette[PaletteColor::Primary] = Color::Rgb(38, 139, 210);        // #268bd2 - Blue
    palette[PaletteColor::Secondary] = Color::Rgb(42, 161, 152);      // #2aa198 - Cyan
    palette[PaletteColor::Tertiary] = Color::Rgb(133, 153, 0);        // #859900 - Green
    palette[PaletteColor::TitlePrimary] = Color::Rgb(131, 148, 150);  // #839496 - Base0
    palette[PaletteColor::TitleSecondary] = Color::Rgb(38, 139, 210);
    palette[PaletteColor::Highlight] = Color::Rgb(88, 110, 117);      // #586e75 - Base01
    palette[PaletteColor::HighlightInactive] = Color::Rgb(7, 54, 66);
    palette[PaletteColor::HighlightText] = Color::Rgb(253, 246, 227); // #fdf6e3 - Base3

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = false; // Clean scientific look

    theme
}