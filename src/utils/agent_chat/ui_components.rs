//! UI components and rendering logic for the chat interface

use super::{Colors, TaskState, InputState, TodoPriority, InputMode, SPINNER_FRAMES};
use ratatui::{
    Frame,
    prelude::*,
    widgets::*,
    style::*,
    text::*,
};
use std::io::{self, Write};

/// Show the welcome box
pub fn show_welcome_box() {
    println!("\n{}╭────────────────────────────────────────────────╮", Colors::CYAN);
    println!("│         {}OSVM Agent Chat Interface{}              │", Colors::BOLD, Colors::CYAN);
    println!("│                                                │");
    println!("│  • Claude Code-style real-time chat           │");
    println!("│  • MCP tool integration                       │");
    println!("│  • Fuzzy search & auto-complete              │");
    println!("│  • Ctrl+T: Toggle task navigation            │");
    println!("│                                                │");
    println!("│  Type 'help' for commands or 'exit' to quit  │");
    println!("╰────────────────────────────────────────────────╯{}\n", Colors::RESET);
}

/// Show enhanced status bar
pub fn show_enhanced_status_bar(task_state: &TaskState) {
    let spinner = SPINNER_FRAMES[task_state.spinner_frame];
    let mode_indicator = match task_state.input_mode {
        InputMode::InputField => "[INPUT]",
        InputMode::TaskSelection => "[TASKS]",
    };

    println!("{}{}═══════════════════════════════════════════════════════════{}",
        Colors::DIM, Colors::BOLD, Colors::RESET);

    println!("{}  {} {} {} | Mode: {}{}",
        Colors::CYAN,
        spinner,
        task_state.current_task,
        Colors::DIM,
        mode_indicator,
        Colors::RESET
    );

    // Show todos inline
    let incomplete_count = task_state.todo_items.iter().filter(|t| !t.completed).count();
    let complete_count = task_state.todo_items.iter().filter(|t| t.completed).count();

    println!("{}  Tasks: {}{}/{} complete{} | {}{}{}",
        Colors::DIM,
        Colors::GREEN,
        complete_count,
        task_state.todo_items.len(),
        Colors::DIM,
        Colors::GRAY,
        task_state.current_reasoning,
        Colors::RESET
    );

    println!("{}═══════════════════════════════════════════════════════════{}\n",
        Colors::DIM, Colors::RESET);
}

/// Show task details below input
pub fn show_task_details_below_input(task_state: &TaskState) {
    if let Some(selected_task) = task_state.get_selected_task_details() {
        println!("\n{}┌─ Task Details ─────────────────────────────┐{}", Colors::DIM, Colors::RESET);
        println!("{}│ {}{}{}", Colors::DIM, Colors::YELLOW, selected_task.text, Colors::RESET);

        // Show reasoning
        let reasoning_lines = wrap_text(&selected_task.reasoning, 44);
        for line in reasoning_lines.iter().take(2) {
            println!("{}│ {}{}", Colors::DIM, line, Colors::RESET);
        }

        // Show execution results if available
        if let Some(results) = &selected_task.execution_results {
            println!("{}│ {}{}{}", Colors::DIM, Colors::GREEN, results, Colors::RESET);
        }

        println!("{}└────────────────────────────────────────────┘{}", Colors::DIM, Colors::RESET);
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
    let task_header = Paragraph::new(Text::from(vec![
        Line::from(vec![
            Span::styled(spinner, Style::default().fg(Color::Cyan)),
            Span::raw(" "),
            Span::styled(&task_state.current_task, Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ])
    ]))
    .block(Block::default()
        .borders(Borders::ALL)
        .title("Task Status")
        .border_style(Style::default().fg(Color::Blue)));

    f.render_widget(task_header, chunks[0]);

    // Todo list
    let todo_items: Vec<ListItem> = task_state.todo_items.iter().enumerate().map(|(i, item)| {
        let checkbox = if item.completed { "☑" } else { "☐" };
        let priority_color = match item.priority {
            TodoPriority::High => Color::Red,
            TodoPriority::Medium => Color::Yellow,
            TodoPriority::Low => Color::Green,
        };

        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection &&
                                    i == task_state.selected_todo_index {
            "▶ "
        } else {
            "  "
        };

        let todo_text = if item.text.len() > 45 {
            format!("{}...", &item.text[..42])
        } else {
            item.text.clone()
        };

        ListItem::new(Line::from(vec![
            Span::raw(selection_indicator),
            Span::styled(checkbox, Style::default().fg(priority_color)),
            Span::raw(" "),
            Span::styled(todo_text, Style::default().fg(Color::White)),
        ]))
    }).collect();

    let todo_header = if task_state.input_mode == InputMode::TaskSelection {
        "Todo List (navigable)"
    } else {
        "Todo List"
    };

    let todo_list = List::new(todo_items)
        .block(Block::default()
            .borders(Borders::ALL)
            .title(todo_header)
            .border_style(Style::default().fg(Color::Blue)))
        .highlight_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD));

    f.render_widget(todo_list, chunks[1]);

    // Current reasoning
    let reasoning_text = Text::from(task_state.current_reasoning.clone());
    let reasoning_paragraph = Paragraph::new(reasoning_text)
        .block(Block::default()
            .borders(Borders::ALL)
            .title("Current Reasoning")
            .border_style(Style::default().fg(Color::Blue)))
        .wrap(Wrap { trim: true })
        .style(Style::default().fg(Color::Gray));

    f.render_widget(reasoning_paragraph, chunks[2]);
}

/// Render input bar in TUI
pub fn render_input_bar(f: &mut Frame, area: Rect, input_state: &InputState, task_state: &TaskState) {
    let input_text = if task_state.input_mode == InputMode::InputField {
        format!("> {}", input_state.input)
    } else {
        format!("[Task Mode] Press Ctrl+T to return to input")
    };

    let input = Paragraph::new(input_text)
        .style(Style::default().fg(Color::Green))
        .block(Block::default()
            .borders(Borders::ALL)
            .title("Input")
            .border_style(Style::default().fg(Color::Green)));

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