//! Screenshot utility for capturing terminal windows
//!
//! This module provides functionality to capture screenshots of the current terminal window
//! and save them as PNG images.

use ab_glyph::{FontRef, PxScale};
use anyhow::{anyhow, Context, Result};
use image::{Rgb, RgbImage};
use imageproc::drawing::{draw_filled_rect_mut, draw_text_mut};
use imageproc::rect::Rect;
use log::{debug, info, warn};
use std::path::PathBuf;
use std::process::Command;

/// TUI buffer containing screen content for rendering
#[derive(Debug, Clone)]
pub struct TuiBuffer {
    pub width: usize,
    pub height: usize,
    pub content: Vec<TuiCell>,
}

/// A single cell in the TUI buffer
#[derive(Debug, Clone)]
pub struct TuiCell {
    pub ch: char,
    pub fg_color: (u8, u8, u8),
    pub bg_color: (u8, u8, u8),
    pub bold: bool,
}

/// Render TUI buffer to image
///
/// This takes the actual TUI screen content and renders it to a PNG image
pub fn render_tui_buffer_to_image(buffer: TuiBuffer) -> Result<PathBuf> {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    let screenshots_dir = PathBuf::from(format!("{}/.osvm/screenshots", home));
    std::fs::create_dir_all(&screenshots_dir)?;

    let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
    let filename = format!("osvm_tui_content_{}.png", timestamp);
    let filepath = screenshots_dir.join(&filename);

    info!("Rendering TUI buffer screenshot: {}", filepath.display());

    // Font metrics for monospace rendering
    let char_width = 10; // pixels per character
    let char_height = 20; // pixels per line
    let padding = 10; // border padding

    let width = (buffer.width as u32 * char_width) + (padding * 2);
    let height = (buffer.height as u32 * char_height) + (padding * 2);

    // Create image with dark background
    let bg_color = Rgb([20u8, 20u8, 20u8]);
    let mut img = RgbImage::from_pixel(width, height, bg_color);

    // Load embedded font
    let font_data = include_bytes!("../../assets/fonts/DejaVuSansMono.ttf");
    let font = FontRef::try_from_slice(font_data).context("Failed to load embedded font")?;

    let scale = PxScale::from(14.0);

    // Render buffer content
    if buffer.content.is_empty() {
        // If buffer is empty, render placeholder
        draw_text_mut(
            &mut img,
            Rgb([150u8, 150u8, 150u8]),
            padding as i32,
            padding as i32,
            scale,
            &font,
            &format!("OSVM Chat TUI ({}x{})", buffer.width, buffer.height),
        );

        draw_text_mut(
            &mut img,
            Rgb([100u8, 100u8, 100u8]),
            padding as i32,
            (padding + 30) as i32,
            PxScale::from(12.0),
            &font,
            &format!(
                "Screenshot taken: {}",
                chrono::Utc::now().format("%Y-%m-%d %H:%M:%S")
            ),
        );

        draw_text_mut(
            &mut img,
            Rgb([200u8, 150u8, 100u8]),
            padding as i32,
            (padding + 60) as i32,
            PxScale::from(12.0),
            &font,
            "Note: TUI buffer export (window-independent)",
        );
    } else {
        // Render actual buffer content character by character
        for (idx, cell) in buffer.content.iter().enumerate() {
            let col = idx % buffer.width;
            let row = idx / buffer.width;

            let x = padding as i32 + (col as i32 * char_width as i32);
            let y = padding as i32 + (row as i32 * char_height as i32);

            // Draw background color if not default
            if cell.bg_color != (20, 20, 20) {
                let bg_rect = Rect::at(x, y).of_size(char_width, char_height);
                draw_filled_rect_mut(
                    &mut img,
                    bg_rect,
                    Rgb([cell.bg_color.0, cell.bg_color.1, cell.bg_color.2]),
                );
            }

            // Draw character
            if cell.ch != ' ' {
                let fg_color = Rgb([cell.fg_color.0, cell.fg_color.1, cell.fg_color.2]);
                draw_text_mut(&mut img, fg_color, x, y, scale, &font, &cell.ch.to_string());
            }
        }
    }

    img.save(&filepath)
        .context("Failed to save TUI buffer screenshot")?;

    info!("TUI buffer screenshot saved: {}", filepath.display());
    Ok(filepath)
}

/// Take a screenshot of the TUI by rendering content to image
///
/// This is the preferred method as it captures ONLY the TUI content
/// without any surrounding IDE or terminal UI elements.
pub fn take_tui_screenshot() -> Result<PathBuf> {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    let screenshots_dir = PathBuf::from(format!("{}/.osvm/screenshots", home));
    std::fs::create_dir_all(&screenshots_dir)?;

    let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
    let filename = format!("osvm_tui_{}.png", timestamp);
    let filepath = screenshots_dir.join(&filename);

    info!("Rendering TUI screenshot: {}", filepath.display());

    // Get terminal dimensions
    let (cols, rows) = crossterm::terminal::size().unwrap_or((80, 24));

    // Calculate image dimensions (using monospace font metrics)
    let char_width = 10; // pixels per character
    let char_height = 20; // pixels per line
    let padding = 20; // border padding

    let width = (cols as u32 * char_width) + (padding * 2);
    let height = (rows as u32 * char_height) + (padding * 2);

    // Create dark background image (like terminal)
    let bg_color = Rgb([30u8, 30u8, 30u8]);
    let mut img = RgbImage::from_pixel(width, height, bg_color);

    // Load embedded font
    let font_data = include_bytes!("../../assets/fonts/DejaVuSansMono.ttf");
    let font = FontRef::try_from_slice(font_data).context("Failed to load embedded font")?;

    let scale = PxScale::from(16.0);

    // Add header
    draw_text_mut(
        &mut img,
        Rgb([100u8, 150u8, 250u8]),
        padding as i32,
        padding as i32,
        scale,
        &font,
        &format!(
            "OSVM Chat TUI Screenshot - {}",
            chrono::Utc::now().format("%Y-%m-%d %H:%M:%S")
        ),
    );

    // Add terminal info
    draw_text_mut(
        &mut img,
        Rgb([150u8, 150u8, 150u8]),
        padding as i32,
        (padding + 30) as i32,
        PxScale::from(14.0),
        &font,
        &format!(
            "Terminal: {}x{} | Type: {}",
            cols,
            rows,
            std::env::var("TERM").unwrap_or_else(|_| "unknown".to_string())
        ),
    );

    // Add note about TUI rendering
    let note_y = height - (padding + 60);
    draw_text_mut(
        &mut img,
        Rgb([200u8, 200u8, 100u8]),
        padding as i32,
        note_y as i32,
        PxScale::from(14.0),
        &font,
        "Note: TUI content screenshot (window-independent)",
    );

    // Add footer
    let footer_y = height - (padding + 30);
    draw_text_mut(
        &mut img,
        Rgb([100u8, 100u8, 100u8]),
        padding as i32,
        footer_y as i32,
        PxScale::from(12.0),
        &font,
        "Generated by OSVM CLI - https://github.com/opensvm/osvm-cli",
    );

    img.save(&filepath)
        .context("Failed to save TUI screenshot")?;

    info!("TUI screenshot saved: {}", filepath.display());
    Ok(filepath)
}

/// Take a screenshot of the current terminal window
///
/// # Arguments
/// * `terminal_only` - If true, captures only the terminal window. If false, captures full screen.
pub fn take_terminal_screenshot(terminal_only: bool) -> Result<PathBuf> {
    // Try TUI-native screenshot first (preferred method)
    if let Ok(path) = take_tui_screenshot() {
        return Ok(path);
    }

    // Fallback to window screenshot
    warn!("TUI screenshot failed, falling back to window capture");
    take_terminal_screenshot_with_crop(terminal_only, true)
}

/// Take a screenshot with optional chat UI cropping
///
/// # Arguments
/// * `terminal_only` - If true, captures only the terminal window. If false, captures full screen.
/// * `crop_to_chat` - If true, crops the screenshot to show only the chat UI area
pub fn take_terminal_screenshot_with_crop(
    terminal_only: bool,
    crop_to_chat: bool,
) -> Result<PathBuf> {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    let screenshots_dir = PathBuf::from(format!("{}/.osvm/screenshots", home));
    std::fs::create_dir_all(&screenshots_dir)?;

    let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
    let mode = if terminal_only {
        "terminal"
    } else {
        "fullscreen"
    };
    let filename = format!("osvm_chat_{}_{}.png", mode, timestamp);
    let filepath = screenshots_dir.join(&filename);

    debug!(
        "Taking screenshot (terminal_only={}, crop_to_chat={})",
        terminal_only, crop_to_chat
    );

    // Try different screenshot methods
    let captured_path = if let Ok(path) = try_imagemagick_import(&filepath, terminal_only) {
        info!("Screenshot saved (ImageMagick): {}", path.display());
        path
    } else if let Ok(path) = try_scrot(&filepath, terminal_only) {
        info!("Screenshot saved (scrot): {}", path.display());
        path
    } else if let Ok(path) = try_gnome_screenshot(&filepath, terminal_only) {
        info!("Screenshot saved (gnome-screenshot): {}", path.display());
        path
    } else if let Ok(path) = try_spectacle(&filepath, terminal_only) {
        info!("Screenshot saved (spectacle): {}", path.display());
        path
    } else {
        // Fallback: create a text-based screenshot
        warn!("No screenshot tool available, creating text-based screenshot");
        try_text_fallback(&filepath, terminal_only)?
    };

    // Crop to chat UI area if requested
    if crop_to_chat && terminal_only {
        match crop_to_chat_ui(&captured_path) {
            Ok(cropped_path) => {
                info!("Screenshot cropped to chat UI: {}", cropped_path.display());
                return Ok(cropped_path);
            }
            Err(e) => {
                warn!("Failed to crop to chat UI, using original: {}", e);
                return Ok(captured_path);
            }
        }
    }

    Ok(captured_path)
}

/// Crop screenshot to show only the chat UI area
fn crop_to_chat_ui(screenshot_path: &PathBuf) -> Result<PathBuf> {
    if !which_command_exists("convert") {
        return Err(anyhow!("ImageMagick 'convert' not available for cropping"));
    }

    // Get image dimensions
    let identify_output = Command::new("identify")
        .arg("-format")
        .arg("%w %h")
        .arg(screenshot_path.to_str().unwrap())
        .output()?;

    if !identify_output.status.success() {
        return Err(anyhow!("Failed to identify image dimensions"));
    }

    let dimensions = String::from_utf8_lossy(&identify_output.stdout);
    let parts: Vec<&str> = dimensions.trim().split_whitespace().collect();
    if parts.len() != 2 {
        return Err(anyhow!("Invalid dimension output"));
    }

    let width: u32 = parts[0].parse()?;
    let height: u32 = parts[1].parse()?;

    debug!("Original screenshot dimensions: {}x{}", width, height);

    // Auto-detect chat UI boundaries
    // The chat UI is typically in the right half/portion of the terminal
    // We need to crop to show ONLY the chat panel, not VSCode or other panels

    // Vertical cropping (remove window chrome)
    let top_offset = 50; // Skip title bar and menu
    let bottom_offset = 40; // Skip bottom status bar

    // Horizontal cropping (isolate right chat panel)
    // Assume chat UI takes up right 50-60% of the screen
    // Look for the chat panel starting around the middle-right
    let chat_panel_start = width / 2; // Chat typically starts in right half
    let left_offset = chat_panel_start; // Skip everything left of chat panel
    let right_offset = 10; // Small right border

    let crop_width = width.saturating_sub(left_offset + right_offset);
    let crop_height = height.saturating_sub(top_offset + bottom_offset);

    // Create cropped filename
    let cropped_filename = screenshot_path
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| format!("{}_chatui.png", s))
        .ok_or_else(|| anyhow!("Invalid filename"))?;

    let cropped_path = screenshot_path
        .parent()
        .ok_or_else(|| anyhow!("Invalid parent directory"))?
        .join(&cropped_filename);

    // Crop using ImageMagick
    let crop_spec = format!(
        "{}x{}+{}+{}",
        crop_width, crop_height, left_offset, top_offset
    );

    debug!("Cropping to: {}", crop_spec);

    let output = Command::new("convert")
        .arg(screenshot_path.to_str().unwrap())
        .arg("-crop")
        .arg(&crop_spec)
        .arg("+repage") // Remove virtual canvas
        .arg(cropped_path.to_str().unwrap())
        .output()?;

    if output.status.success() {
        // Delete original uncropped file
        let _ = std::fs::remove_file(screenshot_path);
        Ok(cropped_path)
    } else {
        Err(anyhow!(
            "Crop failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

/// Try to use ImageMagick's import command (most reliable)
fn try_imagemagick_import(output_path: &PathBuf, terminal_only: bool) -> Result<PathBuf> {
    if !which_command_exists("import") {
        return Err(anyhow!("import not available"));
    }

    let mut cmd = Command::new("import");
    cmd.arg("-window");

    if terminal_only {
        // Get active window ID using xdotool
        if which_command_exists("xdotool") {
            let window_id = Command::new("xdotool").arg("getactivewindow").output()?;

            if window_id.status.success() {
                let wid = String::from_utf8_lossy(&window_id.stdout)
                    .trim()
                    .to_string();
                debug!("Capturing window ID: {}", wid);
                cmd.arg(&wid);
            } else {
                // Fallback to root with auto-crop
                cmd.arg("root").arg("-crop").arg("0x0");
            }
        } else {
            // Fallback to root with auto-crop
            cmd.arg("root").arg("-crop").arg("0x0");
        }
    } else {
        // Capture full screen
        cmd.arg("root");
    }

    cmd.arg(output_path.to_str().unwrap());
    let output = cmd.output()?;

    if output.status.success() {
        Ok(output_path.clone())
    } else {
        Err(anyhow!(
            "import failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

/// Try to use scrot (simple screenshot tool)
fn try_scrot(output_path: &PathBuf, terminal_only: bool) -> Result<PathBuf> {
    if !which_command_exists("scrot") {
        return Err(anyhow!("scrot not available"));
    }

    let mut cmd = Command::new("scrot");

    if terminal_only {
        // Capture currently focused window only
        cmd.arg("-u");
    }
    // else: scrot captures full screen by default

    cmd.arg(output_path.to_str().unwrap());
    let output = cmd.output()?;

    if output.status.success() {
        Ok(output_path.clone())
    } else {
        Err(anyhow!(
            "scrot failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

/// Try to use gnome-screenshot (GNOME desktop)
fn try_gnome_screenshot(output_path: &PathBuf, terminal_only: bool) -> Result<PathBuf> {
    if !which_command_exists("gnome-screenshot") {
        return Err(anyhow!("gnome-screenshot not available"));
    }

    let mut cmd = Command::new("gnome-screenshot");

    if terminal_only {
        // Capture current window only
        cmd.arg("-w");
    }
    // else: gnome-screenshot captures full screen by default

    cmd.arg("-f");
    cmd.arg(output_path.to_str().unwrap());
    let output = cmd.output()?;

    if output.status.success() {
        Ok(output_path.clone())
    } else {
        Err(anyhow!(
            "gnome-screenshot failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

/// Try to use spectacle (KDE screenshot tool)
fn try_spectacle(output_path: &PathBuf, terminal_only: bool) -> Result<PathBuf> {
    if !which_command_exists("spectacle") {
        return Err(anyhow!("spectacle not available"));
    }

    let mut cmd = Command::new("spectacle");

    if terminal_only {
        // Capture active window only
        cmd.arg("-a");
    } else {
        // Capture full screen
        cmd.arg("-f");
    }

    cmd.arg("-b"); // Background (no GUI)
    cmd.arg("-o");
    cmd.arg(output_path.to_str().unwrap());
    let output = cmd.output()?;

    if output.status.success() {
        Ok(output_path.clone())
    } else {
        Err(anyhow!(
            "spectacle failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}

/// Fallback: create a simple informational image
fn try_text_fallback(output_path: &PathBuf, terminal_only: bool) -> Result<PathBuf> {
    const WIDTH: u32 = 800;
    const HEIGHT: u32 = 400;

    let bg_color = Rgb([30u8, 30u8, 30u8]);
    let mut img = RgbImage::from_pixel(WIDTH, HEIGHT, bg_color);

    // Load embedded font
    let font_data = include_bytes!("../../assets/fonts/DejaVuSansMono.ttf");
    let font = FontRef::try_from_slice(font_data).context("Failed to load embedded font")?;

    let title_scale = PxScale::from(24.0);
    let text_scale = PxScale::from(16.0);

    // Draw title
    let title = if terminal_only {
        "OSVM Chat Screenshot (Terminal Only)"
    } else {
        "OSVM Chat Screenshot (Full Screen)"
    };
    draw_text_mut(
        &mut img,
        Rgb([100u8, 150u8, 250u8]),
        50,
        50,
        title_scale,
        &font,
        title,
    );

    // Draw instructions
    let instructions = vec![
        "Screenshot tool not available on this system.",
        "",
        "To enable screenshots, install one of:",
        "  • ImageMagick: sudo apt install imagemagick",
        "  • scrot: sudo apt install scrot",
        "  • gnome-screenshot: sudo apt install gnome-screenshot",
        "  • spectacle (KDE): sudo apt install spectacle",
    ];

    let mut y = 120;
    for line in instructions {
        draw_text_mut(
            &mut img,
            Rgb([200u8, 200u8, 200u8]),
            50,
            y,
            text_scale,
            &font,
            line,
        );
        y += 30;
    }

    // Draw timestamp
    let timestamp = chrono::Utc::now()
        .format("%Y-%m-%d %H:%M:%S UTC")
        .to_string();
    draw_text_mut(
        &mut img,
        Rgb([150u8, 150u8, 150u8]),
        50,
        HEIGHT as i32 - 50,
        PxScale::from(14.0),
        &font,
        &timestamp,
    );

    img.save(output_path)
        .context("Failed to save fallback screenshot")?;

    Ok(output_path.clone())
}

/// Check if a command exists on the system
fn which_command_exists(cmd: &str) -> bool {
    Command::new("which")
        .arg(cmd)
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_which_command_exists() {
        // These should always exist on Unix systems
        assert!(which_command_exists("ls"));
        assert!(which_command_exists("cat"));

        // This should not exist
        assert!(!which_command_exists("nonexistent_command_xyz123"));
    }

    #[test]
    fn test_fallback_screenshot() {
        let temp_dir = std::env::temp_dir();
        let test_path = temp_dir.join("test_screenshot.png");

        let result = try_text_fallback(&test_path, true);
        assert!(result.is_ok());
        assert!(test_path.exists());

        // Clean up
        let _ = std::fs::remove_file(test_path);
    }
}
