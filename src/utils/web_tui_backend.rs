//! Web TUI Backend - Streams ratatui output to browser via SSE
//!
//! This module provides a ratatui backend that captures terminal output
//! and streams it to a web browser using Server-Sent Events (SSE).
//!
//! The backend writes ANSI escape codes to a buffer, which xterm.js
//! in the browser can render natively.

use ratatui::backend::Backend;
use ratatui::buffer::Cell;
use ratatui::layout::{Position, Size};
use ratatui::style::{Color, Modifier};
use std::io::{self, Write};
use std::sync::Arc;

use crate::utils::web_terminal::WebTerminalSender;

/// A ratatui backend that streams output to a web terminal
pub struct WebTuiBackend {
    /// Buffer for building ANSI output
    buffer: Vec<u8>,
    /// Terminal size
    width: u16,
    height: u16,
    /// Web terminal sender for streaming
    sender: WebTerminalSender,
    /// Current cursor position
    cursor_pos: Position,
    /// Current style state for optimization
    current_fg: Option<Color>,
    current_bg: Option<Color>,
    current_modifier: Modifier,
}

impl WebTuiBackend {
    /// Create a new web TUI backend
    pub fn new(sender: WebTerminalSender, width: u16, height: u16) -> Self {
        Self {
            buffer: Vec::with_capacity(65536),
            width,
            height,
            sender,
            cursor_pos: Position::new(0, 0),
            current_fg: None,
            current_bg: None,
            current_modifier: Modifier::empty(),
        }
    }

    /// Write ANSI escape sequence for color
    fn write_color(&mut self, color: Color, foreground: bool) {
        let code = if foreground { "38" } else { "48" };
        match color {
            Color::Reset => {
                if foreground {
                    let _ = write!(self.buffer, "\x1b[39m");
                } else {
                    let _ = write!(self.buffer, "\x1b[49m");
                }
            }
            Color::Black => { let _ = write!(self.buffer, "\x1b[{};5;0m", code); }
            Color::Red => { let _ = write!(self.buffer, "\x1b[{};5;1m", code); }
            Color::Green => { let _ = write!(self.buffer, "\x1b[{};5;2m", code); }
            Color::Yellow => { let _ = write!(self.buffer, "\x1b[{};5;3m", code); }
            Color::Blue => { let _ = write!(self.buffer, "\x1b[{};5;4m", code); }
            Color::Magenta => { let _ = write!(self.buffer, "\x1b[{};5;5m", code); }
            Color::Cyan => { let _ = write!(self.buffer, "\x1b[{};5;6m", code); }
            Color::Gray => { let _ = write!(self.buffer, "\x1b[{};5;7m", code); }
            Color::DarkGray => { let _ = write!(self.buffer, "\x1b[{};5;8m", code); }
            Color::LightRed => { let _ = write!(self.buffer, "\x1b[{};5;9m", code); }
            Color::LightGreen => { let _ = write!(self.buffer, "\x1b[{};5;10m", code); }
            Color::LightYellow => { let _ = write!(self.buffer, "\x1b[{};5;11m", code); }
            Color::LightBlue => { let _ = write!(self.buffer, "\x1b[{};5;12m", code); }
            Color::LightMagenta => { let _ = write!(self.buffer, "\x1b[{};5;13m", code); }
            Color::LightCyan => { let _ = write!(self.buffer, "\x1b[{};5;14m", code); }
            Color::White => { let _ = write!(self.buffer, "\x1b[{};5;15m", code); }
            Color::Rgb(r, g, b) => {
                let _ = write!(self.buffer, "\x1b[{};2;{};{};{}m", code, r, g, b);
            }
            Color::Indexed(i) => { let _ = write!(self.buffer, "\x1b[{};5;{}m", code, i); }
        };
    }

    /// Write ANSI escape sequence for modifiers
    fn write_modifier(&mut self, modifier: Modifier) {
        if modifier.contains(Modifier::BOLD) {
            let _ = write!(self.buffer, "\x1b[1m");
        }
        if modifier.contains(Modifier::DIM) {
            let _ = write!(self.buffer, "\x1b[2m");
        }
        if modifier.contains(Modifier::ITALIC) {
            let _ = write!(self.buffer, "\x1b[3m");
        }
        if modifier.contains(Modifier::UNDERLINED) {
            let _ = write!(self.buffer, "\x1b[4m");
        }
        if modifier.contains(Modifier::SLOW_BLINK) || modifier.contains(Modifier::RAPID_BLINK) {
            let _ = write!(self.buffer, "\x1b[5m");
        }
        if modifier.contains(Modifier::REVERSED) {
            let _ = write!(self.buffer, "\x1b[7m");
        }
        if modifier.contains(Modifier::CROSSED_OUT) {
            let _ = write!(self.buffer, "\x1b[9m");
        }
    }

    /// Apply cell style
    fn apply_style(&mut self, cell: &Cell) {
        let fg = cell.fg;
        let bg = cell.bg;
        let modifier = cell.modifier;

        // Reset if needed
        if modifier != self.current_modifier {
            let _ = write!(self.buffer, "\x1b[0m");
            self.current_fg = None;
            self.current_bg = None;
            self.current_modifier = Modifier::empty();
            self.write_modifier(modifier);
            self.current_modifier = modifier;
        }

        // Apply foreground
        if Some(fg) != self.current_fg {
            self.write_color(fg, true);
            self.current_fg = Some(fg);
        }

        // Apply background
        if Some(bg) != self.current_bg {
            self.write_color(bg, false);
            self.current_bg = Some(bg);
        }
    }

    /// Send the buffered content to web terminal
    fn flush_to_web(&mut self) {
        if !self.buffer.is_empty() {
            if let Ok(s) = String::from_utf8(self.buffer.clone()) {
                let _ = self.sender.send(&s);
            }
            self.buffer.clear();
        }
    }
}

impl Backend for WebTuiBackend {
    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        let mut last_pos: Option<(u16, u16)> = None;

        for (x, y, cell) in content {
            // Move cursor if not at expected position
            if last_pos != Some((x.saturating_sub(1), y)) || last_pos.is_none() {
                // ANSI cursor position (1-indexed)
                let _ = write!(self.buffer, "\x1b[{};{}H", y + 1, x + 1);
            }

            // Apply style
            self.apply_style(cell);

            // Write the character
            let symbol = cell.symbol();
            let _ = write!(self.buffer, "{}", symbol);

            last_pos = Some((x, y));
        }

        Ok(())
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        let _ = write!(self.buffer, "\x1b[?25l");
        Ok(())
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        let _ = write!(self.buffer, "\x1b[?25h");
        Ok(())
    }

    fn get_cursor_position(&mut self) -> io::Result<Position> {
        Ok(self.cursor_pos)
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> io::Result<()> {
        let pos = position.into();
        self.cursor_pos = pos;
        let _ = write!(self.buffer, "\x1b[{};{}H", pos.y + 1, pos.x + 1);
        Ok(())
    }

    fn clear(&mut self) -> io::Result<()> {
        let _ = write!(self.buffer, "\x1b[2J\x1b[H");
        Ok(())
    }

    fn size(&self) -> io::Result<Size> {
        Ok(Size::new(self.width, self.height))
    }

    fn window_size(&mut self) -> io::Result<ratatui::backend::WindowSize> {
        Ok(ratatui::backend::WindowSize {
            columns_rows: Size::new(self.width, self.height),
            pixels: Size::new(self.width * 8, self.height * 16),
        })
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_to_web();
        Ok(())
    }
}

impl Write for WebTuiBackend {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_to_web();
        Ok(())
    }
}

/// Dual backend that writes to both crossterm (real terminal) and web
pub struct DualBackend<W: Write> {
    /// Real terminal backend
    crossterm: ratatui::backend::CrosstermBackend<W>,
    /// Web backend (optional)
    web: Option<WebTuiBackend>,
}

impl<W: Write> DualBackend<W> {
    /// Create a dual backend with both crossterm and web output
    pub fn new(writer: W, web_sender: WebTerminalSender, width: u16, height: u16) -> Self {
        Self {
            crossterm: ratatui::backend::CrosstermBackend::new(writer),
            web: Some(WebTuiBackend::new(web_sender, width, height)),
        }
    }

    /// Create a dual backend with only crossterm (no web)
    #[allow(dead_code)]
    pub fn crossterm_only(writer: W) -> Self {
        Self {
            crossterm: ratatui::backend::CrosstermBackend::new(writer),
            web: None,
        }
    }
}

impl<W: Write> Backend for DualBackend<W> {
    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a Cell)>,
    {
        // Collect content so we can iterate twice
        let cells: Vec<_> = content.collect();

        // Draw to crossterm
        self.crossterm.draw(cells.iter().map(|(x, y, c)| (*x, *y, *c)))?;

        // Draw to web if enabled
        if let Some(ref mut web) = self.web {
            web.draw(cells.iter().map(|(x, y, c)| (*x, *y, *c)))?;
        }

        Ok(())
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        self.crossterm.hide_cursor()?;
        if let Some(ref mut web) = self.web {
            web.hide_cursor()?;
        }
        Ok(())
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        self.crossterm.show_cursor()?;
        if let Some(ref mut web) = self.web {
            web.show_cursor()?;
        }
        Ok(())
    }

    fn get_cursor_position(&mut self) -> io::Result<Position> {
        self.crossterm.get_cursor_position()
    }

    fn set_cursor_position<P: Into<Position>>(&mut self, position: P) -> io::Result<()> {
        let pos = position.into();
        self.crossterm.set_cursor_position(pos)?;
        if let Some(ref mut web) = self.web {
            web.set_cursor_position(pos)?;
        }
        Ok(())
    }

    fn clear(&mut self) -> io::Result<()> {
        self.crossterm.clear()?;
        if let Some(ref mut web) = self.web {
            web.clear()?;
        }
        Ok(())
    }

    fn size(&self) -> io::Result<Size> {
        self.crossterm.size()
    }

    fn window_size(&mut self) -> io::Result<ratatui::backend::WindowSize> {
        self.crossterm.window_size()
    }

    fn flush(&mut self) -> io::Result<()> {
        Backend::flush(&mut self.crossterm)?;
        if let Some(ref mut web) = self.web {
            Backend::flush(web)?;
        }
        Ok(())
    }
}

impl<W: Write> Write for DualBackend<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = self.crossterm.write(buf)?;
        if let Some(ref mut web) = self.web {
            web.write(buf)?;
        }
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        std::io::Write::flush(&mut self.crossterm)?;
        if let Some(ref mut web) = self.web {
            std::io::Write::flush(web)?;
        }
        Ok(())
    }
}
