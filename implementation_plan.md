# Implementation Plan

## Overview  
Fix the OSVM agent_chat TUI layout and responsiveness issues by implementing proper in-place terminal rendering, ensuring auto-suggest functionality works correctly, and maintaining full text selection and right-click context menu capabilities while providing a professional user interface.

The current agent_chat implementation suffers from layout misalignment, poor text wrapping, inconsistent spacing, and most critically: 1) renders new frames below instead of mutating terminal in-place, 2) auto-suggestions don't work properly, and 3) text selection/right-click functionality may be impacted. This plan focuses specifically on the `src/utils/agent_chat/` module to address these problems while preserving the embedded terminal approach that allows normal text selection, copy, and right-click context menus.

## Types
Define unified layout and rendering type system for consistent TUI components.

```rust
/// Unified layout constraint system
#[derive(Debug, Clone)]
pub struct LayoutConstraints {
    pub min_width: u16,
    pub max_width: Option<u16>,
    pub min_height: u16,
    pub max_height: Option<u16>,
    pub flex_grow: f32,
}

/// Terminal-aware component sizing
#[derive(Debug, Clone)]
pub struct ResponsiveLayout {
    pub small_terminal: LayoutConstraints,  // < 80 cols
    pub medium_terminal: LayoutConstraints, // 80-120 cols
    pub large_terminal: LayoutConstraints,  // > 120 cols
}

/// Unified theme system for consistent styling
#[derive(Debug, Clone)]
pub struct TuiTheme {
    pub primary_color: Color,
    pub secondary_color: Color,
    pub accent_color: Color,
    pub background_color: Color,
    pub text_color: Color,
    pub border_style: BorderType,
    pub padding: Padding,
}

/// Component alignment and positioning
#[derive(Debug, Clone)]
pub enum ComponentAlignment {
    Start,
    Center,
    End,
    Stretch,
}

/// Dynamic component state for responsive updates
#[derive(Debug, Clone)]
pub struct ComponentState {
    pub terminal_size: (u16, u16),
    pub available_area: Rect,
    pub needs_update: bool,
    pub scroll_offset: u16,
}

/// Layout manager for coordinating component placement
pub struct LayoutManager {
    pub theme: TuiTheme,
    pub components: Vec<Box<dyn ResponsiveComponent>>,
    pub state: ComponentState,
}

/// Trait for responsive TUI components
pub trait ResponsiveComponent {
    fn calculate_size(&self, available: Rect, terminal_size: (u16, u16)) -> Rect;
    fn render(&self, frame: &mut Frame, area: Rect, state: &ComponentState);
    fn handle_resize(&mut self, new_size: (u16, u16));
    fn min_size(&self) -> (u16, u16);
}
```

## Files
Systematically refactor TUI architecture with new responsive components and unified rendering.

**New files to be created:**
- `src/utils/agent_chat/responsive_layout.rs` - Responsive layout utilities for agent_chat
- `src/utils/agent_chat/layout_manager.rs` - Layout coordination within agent_chat

**Existing files to be modified:**
- `src/utils/agent_chat/chat_application.rs` - Fix main rendering loop and terminal management
- `src/utils/agent_chat/ui_components.rs` - Fix alignment, spacing, and text wrapping
- `src/utils/agent_chat/terminal_utils.rs` - Improve terminal control and display functions  
- `src/utils/agent_chat/task_state.rs` - Add layout-aware state management
- `src/utils/agent_chat/input_handler.rs` - Improve cursor positioning and text handling

**Configuration file updates:**
- No major dependency changes needed - focus on better usage of existing ratatui/crossterm

## Functions
Implement responsive rendering and layout management functions.

**New functions:**
- `get_terminal_layout(terminal_size: (u16, u16)) -> Layout` in `src/utils/agent_chat/responsive_layout.rs`
- `calculate_component_areas(terminal_size: (u16, u16)) -> ComponentAreas` 
- `format_status_line(task_state: &TaskState, available_width: u16) -> String`
- `format_todo_list(task_state: &TaskState, available_width: u16, max_height: u16) -> Vec<String>`
- `wrap_text_smart(text: &str, width: usize) -> Vec<String>` - Proper word wrapping
- `truncate_with_ellipsis(text: &str, max_width: usize) -> String`
- `detect_terminal_resize() -> Option<(u16, u16)>` in `src/utils/agent_chat/terminal_utils.rs`
- `render_in_place(components: &ComponentAreas) -> Result<()>` - In-place terminal mutation
- `preserve_cursor_position() -> (u16, u16)` - Save cursor for text selection preservation
- `restore_cursor_position(pos: (u16, u16))` - Restore cursor after rendering
- `generate_auto_suggestions(input: &str, history: &[String]) -> Vec<String>` - Smart auto-suggestions
- `display_suggestions_overlay(suggestions: &[String], selected: usize) -> Result<()>` - Non-intrusive suggestion display

**Modified functions:**
- `run_agent_chat_ui() -> Result<()>` in `src/utils/agent_chat/chat_application.rs`
  - Implement in-place terminal rendering (no new frames below)
  - Add proper terminal state management to preserve text selection
  - Enable auto-suggestion functionality with real-time updates
  - Ensure right-click and context menu functionality is preserved

- `show_enhanced_status_bar(task_state: &TaskState)` in `src/utils/agent_chat/ui_components.rs`
  - Convert to in-place rendering with cursor position preservation
  - Fix alignment and spacing issues
  - Add proper text truncation for narrow terminals

- `show_task_details_below_input(task_state: &TaskState)`
  - Implement in-place updates without scrolling terminal content
  - Fix overflow and wrapping issues
  - Implement collapsible sections for small terminals

- `get_enhanced_input_with_status()` in `src/utils/agent_chat/chat_application.rs`
  - Fix auto-suggestion display and selection
  - Ensure text selection and right-click work during input
  - Implement proper cursor management for suggestions

**Removed functions:**
- Functions that print new content below instead of in-place updates
- Inefficient rendering functions that interfere with text selection

## Classes
Create unified component architecture replacing mixed framework usage.

**New classes:**
- `ComponentAreas` in `src/utils/agent_chat/responsive_layout.rs`
  - Defines screen areas for status bar, todo list, input field, and suggestions
  - Handles dynamic sizing based on terminal dimensions
  - Provides consistent spacing and alignment

- `LayoutState` in `src/utils/agent_chat/layout_manager.rs`
  - Tracks current terminal size and component visibility
  - Manages resize events and layout recalculation
  - Coordinates component rendering order

**Modified classes:**
- `TaskState` in `src/utils/agent_chat/task_state.rs`
  - Add terminal size awareness
  - Improve text formatting methods
  - Better component state coordination

- `App` in `src/utils/agent_chat/chat_application.rs`
  - Implement proper resize handling
  - Fix terminal initialization and cleanup
  - Improve rendering loop efficiency

- `InputState` in `src/utils/agent_chat/input_handler.rs`
  - Better cursor position management
  - Improved text wrapping for long inputs
  - Enhanced suggestion display handling

**Removed classes:**
- None - focus on improving existing agent_chat components

## Dependencies
Consolidate TUI framework dependencies for consistency and reduced complexity.

**New packages:**
- None - work with existing dependencies

**Updated packages:**
- Better utilize existing `ratatui = "0.29.0"` features
- Improve usage of existing `crossterm = "0.29.0"` capabilities

**Removed packages:**
- None - agent_chat module doesn't use cursive dependencies

**Version changes:**
- No dependency version changes needed
- Focus on better implementation patterns with existing crates

## Testing
Comprehensive testing approach for responsive TUI components and layout system.

**New test files:**
- `src/utils/agent_chat/layout_tests.rs` - Terminal layout and formatting tests
- Add tests to existing `run_chat_ui_tests()` function

**Existing test modifications:**
- Update `run_chat_ui_tests()` in `src/utils/agent_chat/chat_application.rs`
- Add layout validation for different terminal sizes
- Test text wrapping and truncation behavior

**Testing strategies:**
- Test common terminal sizes (80x24, 120x30, 160x40)
- Validate component alignment and spacing
- Test text overflow handling and wrapping
- Verify suggestion display at different sizes
- Manual testing with real terminal resize events

## Implementation Order
Sequential implementation steps to minimize conflicts and ensure successful integration.

1. **Phase 1: Terminal Rendering Foundation** (Critical Core)
   - Create `src/utils/agent_chat/responsive_layout.rs` with in-place rendering utilities
   - Implement cursor position preservation for text selection support
   - Add terminal state management that doesn't interfere with right-click functionality
   - Create `render_in_place()` function that updates terminal without scrolling

2. **Phase 2: Auto-Suggestion Implementation** (User Experience)
   - Fix auto-suggestion generation and display in `input_handler.rs`
   - Implement non-intrusive suggestion overlay that doesn't break text selection
   - Add real-time suggestion updates as user types
   - Ensure suggestion selection works with keyboard navigation

3. **Phase 3: Status Bar In-Place Updates** (Visual Foundation)  
   - Convert `show_enhanced_status_bar()` to use cursor positioning instead of new prints
   - Implement proper text truncation for narrow terminals
   - Fix spinner animation with in-place updates
   - Preserve terminal scroll buffer and text selection

4. **Phase 4: Task Panel In-Place Rendering** (Core Functionality)
   - Fix `show_task_details_below_input()` to use cursor positioning
   - Implement proper text wrapping that doesn't interfere with terminal selection
   - Add collapsible sections with smooth in-place transitions
   - Preserve existing terminal content during updates

5. **Phase 5: Input Field and Text Selection** (User Interaction)
   - Ensure input field rendering preserves right-click functionality
   - Fix cursor positioning to maintain normal terminal text selection
   - Implement proper handling for long input text without scrolling
   - Test copy/paste and context menu functionality thoroughly

6. **Phase 6: Terminal State Preservation** (System Integration)
   - Implement proper terminal initialization that preserves native functionality
   - Add resize event handling without breaking text selection
   - Ensure terminal cleanup restores original state completely
   - Validate that all native terminal features continue to work

7. **Phase 7: Integration Testing** (Quality Assurance)
   - Test auto-suggestions with different input patterns
   - Validate text selection works in all UI areas  
   - Test right-click context menus in various scenarios
   - Verify no unwanted scrolling or new content below existing text
   - Manual testing of copy/paste functionality

8. **Phase 8: Performance and Polish** (Final Optimization)
   - Optimize rendering performance to minimize terminal mutations
   - Fine-tune suggestion timing and responsiveness
   - Ensure smooth UI transitions without flickering
   - Add configuration options for auto-suggestion behavior
