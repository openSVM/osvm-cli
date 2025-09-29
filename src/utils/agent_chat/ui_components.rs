//! UI components and rendering logic for the chat interface

use super::{Colors, TaskState, InputState, TodoPriority, InputMode, SPINNER_FRAMES};
use anyhow::Result;
use ratatui::{
    Frame,
    prelude::*,
    widgets::*,
    style::*,
    text::*,
};

/// Show the welcome box using simple borders - compact version
pub fn show_welcome_box() {
    println!("{}┌─ OSVM Agent Chat Interface ─────────────────────┐{}", Colors::CYAN, Colors::RESET);
    println!("{}│ Real-time AI chat • MCP tools • Ctrl+T tasks    │{}", Colors::CYAN, Colors::RESET);
    println!("{}│ Type your message or /help for commands         │{}", Colors::CYAN, Colors::RESET);
    println!("{}└──────────────────────────────────────────────────┘{}", Colors::CYAN, Colors::RESET);
}

/// Show enhanced status bar using simple sequential output
pub fn show_enhanced_status_bar_in_place(
    task_state: &TaskState,
    _renderer: &mut super::responsive_layout::TerminalRenderer,
) -> Result<()> {
    show_enhanced_status_bar(task_state);
    Ok(())
}

/// Show enhanced status bar - compact version
pub fn show_enhanced_status_bar(task_state: &TaskState) {
    let spinner = SPINNER_FRAMES[task_state.spinner_frame];
    let mode_indicator = match task_state.input_mode {
        InputMode::InputField => "INPUT",
        InputMode::TaskSelection => "TASKS",
    };

    println!("{}┌─ Status ─────────────────────────────────────────┐{}", Colors::CYAN, Colors::RESET);
    println!("{}│ {} {} • Mode: {} │{}", 
             Colors::CYAN, 
             spinner,
             task_state.current_task,
             mode_indicator,
             Colors::RESET);
    println!("{}└──────────────────────────────────────────────────┘{}", Colors::CYAN, Colors::RESET);
}

/// Show task details using simple sequential rendering with boxes
pub fn show_task_details_in_place(
    task_state: &TaskState,
    _renderer: &mut super::responsive_layout::TerminalRenderer,
) -> Result<()> {
    show_task_details_below_input(task_state);
    Ok(())
}

/// Show task details with simple borders
pub fn show_task_details_below_input(task_state: &TaskState) {
    let complete_count = task_state.todo_items.iter().filter(|t| t.completed).count();
    
    println!("{}+-- Tasks ({}/{}) ------------------------------+{}", 
             Colors::YELLOW,
             complete_count,
             task_state.todo_items.len(),
             Colors::RESET);
    
    for (i, item) in task_state.todo_items.iter().enumerate() {
        let checkbox = if item.completed { "[X]" } else { "[ ]" };
        let priority_color = match item.priority {
            TodoPriority::High => Colors::RED,
            TodoPriority::Medium => Colors::YELLOW,
            TodoPriority::Low => Colors::GREEN,
        };
        
        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection &&
                                     i == task_state.selected_todo_index {
            format!("{}>> {}", Colors::CYAN, Colors::RESET)
        } else {
            "   ".to_string()
        };
        
        println!("{}|{}{} {}{}{} |{}",
                 Colors::YELLOW,
                 selection_indicator,
                 checkbox,
                 priority_color,
                 item.text,
                 Colors::YELLOW,
                 Colors::RESET);
        
        if let Some(ref results) = item.execution_results {
            println!("{}|    {}{}{} |{}",
                     Colors::YELLOW,
                     Colors::DIM,
                     results,
                     Colors::YELLOW,
                     Colors::RESET);
        }
    }
    
    println!("{}+-----------------------------------------------+{}", Colors::YELLOW, Colors::RESET);
    
    if !task_state.current_reasoning.is_empty() {
        println!("{}+-- Current Reasoning ---------------------------+{}", Colors::BLUE, Colors::RESET);
        println!("{}| {} |{}", Colors::BLUE, task_state.current_reasoning, Colors::RESET);
        println!("{}+-----------------------------------------------+{}", Colors::BLUE, Colors::RESET);
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
            Span::styled(spinner, Style::default().fg(ratatui::style::Color::Cyan)),
            Span::raw(" "),
            Span::styled(&task_state.current_task, Style::default().fg(ratatui::style::Color::White).add_modifier(Modifier::BOLD)),
        ])
    ]))
    .block(Block::default()
        .borders(Borders::ALL)
        .title("Task Status")
        .border_style(Style::default().fg(ratatui::style::Color::Blue)));

    f.render_widget(task_header, chunks[0]);

    // Todo list
    let todo_items: Vec<ListItem> = task_state.todo_items.iter().enumerate().map(|(i, item)| {
        let checkbox = if item.completed { "☑" } else { "☐" };
        let priority_color = match item.priority {
            TodoPriority::High => ratatui::style::Color::Red,
            TodoPriority::Medium => ratatui::style::Color::Yellow,
            TodoPriority::Low => ratatui::style::Color::Green,
        };

        let selection_indicator = if task_state.input_mode == InputMode::TaskSelection &&
                                    i == task_state.selected_todo_index {
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
            .border_style(Style::default().fg(ratatui::style::Color::Blue)))
        .highlight_style(Style::default().fg(ratatui::style::Color::Yellow).add_modifier(Modifier::BOLD));

    f.render_widget(todo_list, chunks[1]);

    // Current reasoning
    let reasoning_text = Text::from(task_state.current_reasoning.clone());
    let reasoning_paragraph = Paragraph::new(reasoning_text)
        .block(Block::default()
            .borders(Borders::ALL)
            .title("Current Reasoning")
            .border_style(Style::default().fg(ratatui::style::Color::Blue)))
        .wrap(Wrap { trim: true })
        .style(Style::default().fg(ratatui::style::Color::Gray));

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
        .style(Style::default().fg(ratatui::style::Color::Green))
        .block(Block::default()
            .borders(Borders::ALL)
            .title("Input")
            .border_style(Style::default().fg(ratatui::style::Color::Green)));

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
