//! Nord Ice theme - cool, professional, easy on the eyes

use cursive::theme::{BaseColor, BorderStyle, Color, Palette, PaletteColor, Theme};

pub fn theme() -> Theme {
    let mut theme = Theme::default();
    let mut palette = Palette::default();

    // Nord palette - Arctic, north-bluish color palette
    palette[PaletteColor::Background] = Color::Rgb(46, 52, 64);       // #2e3440 - Polar Night
    palette[PaletteColor::View] = Color::Rgb(59, 66, 82);             // #3b4252 - Raised surface
    palette[PaletteColor::Primary] = Color::Rgb(136, 192, 208);       // #88c0d0 - Frost blue
    palette[PaletteColor::Secondary] = Color::Rgb(129, 161, 193);     // #81a1c1 - Ice blue
    palette[PaletteColor::Tertiary] = Color::Rgb(163, 190, 140);      // #a3be8c - Aurora green
    palette[PaletteColor::TitlePrimary] = Color::Rgb(236, 239, 244);  // #eceff4 - Snow white
    palette[PaletteColor::TitleSecondary] = Color::Rgb(136, 192, 208);
    palette[PaletteColor::Highlight] = Color::Rgb(94, 129, 172);      // #5e81ac - Selected
    palette[PaletteColor::HighlightInactive] = Color::Rgb(76, 86, 106);
    palette[PaletteColor::HighlightText] = Color::Rgb(236, 239, 244);

    theme.palette = palette;
    theme.borders = BorderStyle::Simple;
    theme.shadow = false; // Clean, minimal look

    theme
}