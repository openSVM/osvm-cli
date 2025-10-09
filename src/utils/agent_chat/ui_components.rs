//! Modern UI components with enhanced visual styling
//! Provides beautifully styled terminal UI elements with gradients, icons, and animations

use super::{Colors, InputMode, InputState, TaskState, TodoPriority, SPINNER_FRAMES};
use crate::utils::theme::{Gradient, ProgressBar, StatusIndicator, StyledBox, Theme};
use anyhow::Result;
use ratatui::{prelude::*, style::*, text::*, widgets::*, Frame};

/// Show modern welcome box with gradient and rounded corners
pub fn show_welcome_box() {
    // Create gradient for the title
    let title_gradient = Gradient::purple_gradient();
    let title = "OSVM Agent Chat Interface";
    let gradient_title = Gradient::apply_to_text(title, &title_gradient);

    // Use rounded corners with modern styling
    println!();
    println!(
        "{}{}╭─────────────────────────────────────────────────────────╮{}",
        Theme::PRIMARY,
        Theme::BOLD,
        Theme::RESET
    );
    println!(
        "{}│  {} {}✨ {} │{}",
        Theme::PRIMARY,
        Theme::ICON_ROCKET,
        gradient_title,
        Theme::ICON_SPARKLES,
        Theme::RESET
    );
    println!(
        "{}│                                                         │{}",
        Theme::PRIMARY_LIGHT,
        Theme::RESET
    );
    println!(
        "{}│   {} {}Real-time AI{} {} {}MCP Tools{} {} {}Smart Tasks{}   │{}",
        Theme::PRIMARY_LIGHT,
        Theme::ICON_LIGHTNING,
        Theme::SUCCESS,
        Theme::TEXT,
        Theme::ICON_GEAR,
        Theme::ACCENT_BRIGHT,
        Theme::TEXT,
        Theme::ICON_STAR,
        Theme::WARNING_LIGHT,
        Theme::TEXT,
        Theme::RESET
    );
    println!(
        "{}│   {}Type your message or {}{}/help{} for commands        │{}",
        Theme::PRIMARY_LIGHT,
        Theme::TEXT_DIM,
        Theme::ACCENT,
        Theme::BOLD,
        Theme::TEXT_DIM,
        Theme::RESET
    );
    println!(
        "{}╰─────────────────────────────────────────────────────────╯{}",
        Theme::PRIMARY,
        Theme::RESET
    );
    println!();
}

/// Show enhanced status bar using simple sequential output
pub fn show_enhanced_status_bar_in_place(
    task_state: &TaskState,
    _renderer: &mut super::responsive_layout::TerminalRenderer,
) -> Result<()> {
    show_enhanced_status_bar(task_state);
    Ok(())
}

/// Show enhanced status bar with modern styling and icons
pub fn show_enhanced_status_bar(task_state: &TaskState) {
    // Use modern spinner frames from theme
    let spinner_frames = Theme::SPINNER_DOTS;
    let spinner = spinner_frames[task_state.spinner_frame % spinner_frames.len()];

    // Mode indicator with icons
    let (mode_icon, mode_text, mode_color) = match task_state.input_mode {
        InputMode::InputField => (Theme::ICON_CHEVRON_RIGHT, "INPUT MODE", Theme::SUCCESS),
        InputMode::TaskSelection => (Theme::ICON_BULLET, "TASK MODE", Theme::WARNING),
    };

    // Create a modern status bar with gradient border
    println!();
    println!(
        "{}╭─ {} Status {} ──────────────────────────────────────╮{}",
        Theme::ACCENT,
        Theme::ICON_INFO,
        Theme::ICON_INFO,
        Theme::RESET
    );
    println!(
        "{}│ {}{} {} {}│ {}{} {}{} {}│{}",
        Theme::ACCENT,
        Theme::ACCENT_BRIGHT,
        spinner,
        Theme::TEXT,
        task_state.current_task,
        mode_color,
        mode_icon,
        Theme::BOLD,
        mode_text,
        Theme::RESET,
        Theme::RESET
    );
    println!(
        "{}╰──────────────────────────────────────────────────────╯{}",
        Theme::ACCENT,
        Theme::RESET
    );
}

/// Show task details using simple sequential rendering with boxes
pub fn show_task_details_in_place(
    task_state: &TaskState,
    _renderer: &mut super::responsive_layout::TerminalRenderer,
) -> Result<()> {
    show_task_details_below_input(task_state);
    Ok(())
}

/// Show task details with modern styling and progress indicators
pub fn show_task_details_below_input(task_state: &TaskState) {
    let complete_count = task_state.todo_items.iter().filter(|t| t.completed).count();
    let total_count = task_state.todo_items.len();
    let progress = if total_count > 0 {
        complete_count as f32 / total_count as f32
    } else {
        0.0
    };

    // Modern task header with progress bar
    println!();
    println!(
        "{}╭─ {} Tasks {} ─────────────────────────────────────────╮{}",
        Theme::WARNING,
        Theme::ICON_STAR,
        ProgressBar::render(progress, 20, false),
        Theme::RESET
    );

    for (i, item) in task_state.todo_items.iter().enumerate() {
        // Use modern icons for checkboxes
        let checkbox = if item.completed {
            format!("{}{}", Theme::SUCCESS, Theme::ICON_SUCCESS)
        } else {
            format!("{}☐", Theme::TEXT_MUTED)
        };

        // Priority with colored badges
        let (priority_icon, priority_color) = match item.priority {
            TodoPriority::High => (Theme::ICON_FIRE, Theme::ERROR),
            TodoPriority::Medium => (Theme::ICON_WARNING, Theme::WARNING),
            TodoPriority::Low => (Theme::ICON_ARROW_DOWN, Theme::SUCCESS_LIGHT),
        };

        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection
            && i == task_state.selected_todo_index
        {
            format!("{}{} ", Theme::ACCENT_BRIGHT, Theme::ICON_CHEVRON_RIGHT)
        } else {
            "  ".to_string()
        };

        // Modern task item display
        println!(
            "{}│ {}{} {} {} {}{}{}  {}│{}",
            Theme::WARNING,
            selection_indicator,
            checkbox,
            priority_icon,
            priority_color,
            Theme::BOLD,
            item.text,
            Theme::RESET,
            Theme::WARNING,
            Theme::RESET
        );

        if let Some(ref results) = item.execution_results {
            println!(
                "{}│     {} {}{}{}{}{}│{}",
                Theme::WARNING,
                Theme::ICON_ARROW_RIGHT,
                Theme::TEXT_DIM,
                Theme::ITALIC,
                results,
                Theme::RESET,
                Theme::WARNING,
                Theme::RESET
            );
        }
    }

    // Show task summary with modern styling
    println!(
        "{}├─────────────────────────────────────────────────────────┤{}",
        Theme::WARNING,
        Theme::RESET
    );
    println!(
        "{}│ {} Completed: {}{}/{}{} {} Progress: {}{}%{}         {}│{}",
        Theme::WARNING,
        Theme::ICON_SUCCESS,
        Theme::SUCCESS,
        complete_count,
        total_count,
        Theme::RESET,
        Theme::ICON_HOURGLASS,
        Theme::ACCENT_BRIGHT,
        (progress * 100.0) as u32,
        Theme::RESET,
        Theme::WARNING,
        Theme::RESET
    );
    println!(
        "{}╰─────────────────────────────────────────────────────────╯{}",
        Theme::WARNING,
        Theme::RESET
    );

    if !task_state.current_reasoning.is_empty() {
        println!();
        println!(
            "{}╭─ {} Current Reasoning ─────────────────────────────────╮{}",
            Theme::ACCENT,
            Theme::ICON_INFO,
            Theme::RESET
        );
        println!(
            "{}│ {}{}{} │{}",
            Theme::ACCENT,
            Theme::TEXT,
            task_state.current_reasoning,
            Theme::ACCENT,
            Theme::RESET
        );
        println!(
            "{}╰─────────────────────────────────────────────────────────╯{}",
            Theme::ACCENT,
            Theme::RESET
        );
    }
}

/// Render task status in TUI
pub fn render_task_status(f: &mut Frame, area: Rect, task_state: &TaskState) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(6),
            Constraint::Length(3),
        ])
        .split(area);

    // Task header with spinner
    let spinner = SPINNER_FRAMES[task_state.spinner_frame];
    let task_header = Paragraph::new(Text::from(vec![Line::from(vec![
        Span::styled(spinner, Style::default().fg(ratatui::style::Color::Cyan)),
        Span::raw(" "),
        Span::styled(
            &task_state.current_task,
            Style::default()
                .fg(ratatui::style::Color::White)
                .add_modifier(Modifier::BOLD),
        ),
    ])]))
    .block(
        Block::default()
            .borders(Borders::ALL)
            .title("Task Status")
            .border_style(Style::default().fg(ratatui::style::Color::Blue)),
    );

    f.render_widget(task_header, chunks[0]);

    // Todo list
    let todo_items: Vec<ListItem> = task_state
        .todo_items
        .iter()
        .enumerate()
        .map(|(i, item)| {
            let checkbox = if item.completed { "☑" } else { "☐" };
            let priority_color = match item.priority {
                TodoPriority::High => ratatui::style::Color::Red,
                TodoPriority::Medium => ratatui::style::Color::Yellow,
                TodoPriority::Low => ratatui::style::Color::Green,
            };

            let selection_indicator = if task_state.input_mode == InputMode::TaskSelection
                && i == task_state.selected_todo_index
            {
                "▶ "
            } else {
                "  "
            };

            let todo_text = if item.text.chars().count() > 45 {
                let truncated: String = item.text.chars().take(42).collect();
                format!("{}...", truncated)
            } else {
                item.text.clone()
            };

            ListItem::new(Line::from(vec![
                Span::raw(selection_indicator),
                Span::styled(checkbox, Style::default().fg(priority_color)),
                Span::raw(" "),
                Span::styled(todo_text, Style::default().fg(ratatui::style::Color::White)),
            ]))
        })
        .collect();

    let todo_header = if task_state.input_mode == InputMode::TaskSelection {
        "Todo List (navigable)"
    } else {
        "Todo List"
    };

    let todo_list = List::new(todo_items)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(todo_header)
                .border_style(Style::default().fg(ratatui::style::Color::Blue)),
        )
        .highlight_style(
            Style::default()
                .fg(ratatui::style::Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    f.render_widget(todo_list, chunks[1]);

    // Current reasoning
    let reasoning_text = Text::from(task_state.current_reasoning.clone());
    let reasoning_paragraph = Paragraph::new(reasoning_text)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title("Current Reasoning")
                .border_style(Style::default().fg(ratatui::style::Color::Blue)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().fg(ratatui::style::Color::Gray));

    f.render_widget(reasoning_paragraph, chunks[2]);
}

/// Render input bar in TUI
pub fn render_input_bar(
    f: &mut Frame,
    area: Rect,
    input_state: &InputState,
    task_state: &TaskState,
) {
    let input_text = if task_state.input_mode == InputMode::InputField {
        format!("> {}", input_state.input)
    } else {
        format!("[Task Mode] Press Ctrl+T to return to input")
    };

    let input = Paragraph::new(input_text)
        .style(Style::default().fg(ratatui::style::Color::Green))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title("Input")
                .border_style(Style::default().fg(ratatui::style::Color::Green)),
        );

    f.render_widget(input, area);
}

/// Wrap text to specified width
fn wrap_text(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_line = String::new();

    for word in text.split_whitespace() {
        if current_line.len() + word.len() + 1 > width {
            if !current_line.is_empty() {
                lines.push(current_line.clone());
                current_line.clear();
            }
        }

        if !current_line.is_empty() {
            current_line.push(' ');
        }
        current_line.push_str(word);
    }

    if !current_line.is_empty() {
        lines.push(current_line);
    }

    lines
}
