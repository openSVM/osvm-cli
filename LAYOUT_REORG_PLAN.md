# Chat UI Layout Reorganization Plan

## Completed ✅
1. **Session Persistence** - Chats now save/load automatically
   - File: `src/utils/agent_chat_v2/persistence.rs`
   - Auto-saves on: message add, session create, session switch
   - Loads on startup to restore previous state
   - No more duplicate "Main Chat" entries!

2. **Duplicate Chat Fix** - Fixed by loading from disk instead of creating defaults every time

## TODO - Layout Improvements

### Current Structure (FAR-style)
```
┌─ Dialog Title (OSVM Agent - State) ──────────────────────────┐
│  ┌─ Chat List ──┬─ Chat History ─────────────────────────┐  │
│  │ Main Chat    │ User: message                          │  │
│  │ > Analysis   │ Agent: response                        │  │
│  │ + New Chat   │ ...                                    │  │
│  │              │                                        │  │
│  │ [Buttons]    │ ┌─ Input ─────────────────────────┐   │  │
│  │ Run/Pause/   │ │ Type message...                 │   │  │
│  │ Stop/Record  │ └─────────────────────────────────┘   │  │
│  │              │ [Clear] [Export] [Settings] [Help]    │  │
│  └──────────────┴────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────┘
```

### Desired Structure (Microsoft Edit style)
```
┌─────────────────────────────────────────────────────────────────┐
│ File  Edit  Session  Tools  Help         [Agent: Idle] F1=Help  │ <- Menu bar at top
├──────────┬──────────────────────────────────────────────────────┤
│ Sessions │ Main Chat                                            │
│ ──────────
│ Main     │ User: What's my balance?                             │
│ > Work   │ Agent: I'll check that for you...                    │
│          │ Result: 2.5 SOL                                      │
│ + New    │                                                      │
│          │                                                      │
│ MCP      │                                                      │
│ > osvm   │  <- Collapsible by server                           │
│   ⌄ Tools│                                                      │
│   · bal  │                                                      │
│   · send │                                                      │
│   · rpc  │                                                      │
│          │                                                      │
│          │ ┌────────────────────────────────────────────────┐  │
│          │ │ Type your message... (Ctrl+Enter to send)      │  │
│          │ └────────────────────────────────────────────────┘  │
└──────────┴──────────────────────────────────────────────────────┘
```

## Implementation Steps

### Step 1: Create Top Menu Bar
File: `src/utils/agent_chat_v2/ui/layout.rs`

Add new method to `AdvancedChatUI`:
```rust
fn create_top_menu_bar(&self) -> impl View {
    let mut menu_bar = LinearLayout::horizontal();

    // File menu
    menu_bar.add_child(Button::new_raw(" File ", |s| show_file_menu(s)));
    menu_bar.add_child(TextView::new(" "));

    // Edit menu
    menu_bar.add_child(Button::new_raw(" Edit ", |s| show_edit_menu(s)));
    menu_bar.add_child(TextView::new(" "));

    // Session menu
    menu_bar.add_child(Button::new_raw(" Session ", |s| show_session_menu(s)));
    menu_bar.add_child(TextView::new(" "));

    // Tools menu
    menu_bar.add_child(Button::new_raw(" Tools ", |s| show_tools_menu(s)));
    menu_bar.add_child(TextView::new(" "));

    // Help menu
    menu_bar.add_child(Button::new_raw(" Help ", |s| show_help_menu(s)));

    // Spacer to push status to right
    menu_bar.add_child(DummyView.full_width());

    // Agent status on right
    let status = if let Some(session) = self.state.get_active_session() {
        format!("[Agent: {}]", session.agent_state)
    } else {
        "[Agent: Idle]".to_string()
    };
    menu_bar.add_child(TextView::new(status).with_name("menu_status"));
    menu_bar.add_child(TextView::new("  F1=Help  "));

    Panel::new(menu_bar).title("").full_width()
}
```

### Step 2: Reorganize Main Layout
Modify `setup_far_ui()`:

```rust
pub fn setup_far_ui(&self, siv: &mut Cursive) {
    // Apply theme
    siv.set_theme(ModernTheme::dark());

    // Vertical layout: Menu Bar | Main Content
    let mut root_layout = LinearLayout::vertical();

    // Add top menu bar
    root_layout.add_child(self.create_top_menu_bar());

    // Horizontal layout: Left Sidebar | Chat Area
    let mut content_layout = LinearLayout::horizontal();

    // Left sidebar with both sessions and MCP tools
    let left_width = if siv.screen_size().x > 120 { 30 } else { 25 };
    content_layout.add_child(
        ResizedView::with_fixed_width(
            left_width,
            self.create_unified_sidebar() // New method!
        )
    );

    // Right: Chat history + input (no buttons underneath!)
    content_layout.add_child(
        self.create_chat_area().full_width()
    );

    root_layout.add_child(content_layout.full_height());

    // No Dialog wrapper - just fullscreen
    siv.add_fullscreen_layer(root_layout);
    siv.focus_name("input").ok();
}
```

### Step 3: Create Unified Sidebar
File: `src/utils/agent_chat_v2/ui/components.rs` (or layout.rs)

```rust
fn create_unified_sidebar(&self) -> impl View {
    let mut sidebar = LinearLayout::vertical();

    // Sessions section
    sidebar.add_child(Panel::new(
        self.create_chat_list_view()
    ).title(" Sessions ").full_height());

    // MCP Tools section (collapsible by server)
    sidebar.add_child(Panel::new(
        self.create_collapsible_mcp_tools()
    ).title(" MCP Tools "));

    sidebar
}

fn create_collapsible_mcp_tools(&self) -> impl View {
    let mut tools_list = ListView::new();

    // Get available tools grouped by server
    if let Ok(available_tools) = self.state.available_tools.read() {
        for (server_id, tools) in available_tools.iter() {
            // Server header (clickable to collapse/expand)
            tools_list.add_child(
                &format!("▼ {}", server_id), // ▼ = expanded, ▶ = collapsed
                Button::new_raw(&format!(" {} ({} tools)", server_id, tools.len()),
                    move |s| toggle_server_collapsed(s, server_id.clone()))
            );

            // Tools under this server (visible when expanded)
            for tool in tools {
                tools_list.add_child(
                    &format!("  · {}", tool.name),
                    Button::new_raw(&format!("  · {}", tool.name),
                        move |s| show_tool_info(s, tool.clone()))
                );
            }
        }
    }

    ScrollView::new(tools_list)
}
```

### Step 4: Simplify Chat Area
Remove duplicate buttons, keep only input field:

```rust
fn create_chat_area(&self) -> impl View {
    let mut layout = LinearLayout::vertical();

    // Chat history (full height)
    layout.add_child(
        ScrollView::new(
            TextView::new("").with_name("chat_history")
        ).with_name("chat_scroll")
        .full_height()
    );

    // Input area at bottom (no buttons!)
    layout.add_child(
        Panel::new(
            EditView::new()
                .on_submit(handle_send_message)
                .with_name("input")
                .full_width()
        ).title(" Input (Ctrl+Enter to send) ")
    );

    layout
}
```

## Key Improvements

1. **Single menu bar at top** - All actions (File, Edit, Session, Tools, Help) in one place
2. **Unified left sidebar** - Sessions + MCP tools together, more organized
3. **Collapsible MCP tools** - Each server can be expanded/collapsed (▼/▶)
4. **Cleaner chat area** - No duplicate buttons under input
5. **Better space usage** - More room for actual chat content
6. **Status in menu bar** - Agent state visible at top right

## Menu Implementations Needed

Add these to `src/utils/agent_chat_v2/ui/handlers.rs`:

```rust
pub fn show_file_menu(s: &mut Cursive) {
    s.add_layer(
        Dialog::new()
            .title("File")
            .button("Export Chat", export_current_chat)
            .button("Export All", export_all_chats)
            .button("Settings", show_settings)
            .button("Quit", |s| s.quit())
            .button("Cancel", |s| s.pop_layer())
    );
}

pub fn show_edit_menu(s: &mut Cursive) {
    // Copy, Delete, Clear, etc.
}

pub fn show_session_menu(s: &mut Cursive) {
    // New Session, Rename, Delete, Record, etc.
}

pub fn show_tools_menu(s: &mut Cursive) {
    // Refresh MCP Tools, Configure Servers, etc.
}

pub fn show_help_menu(s: &mut Cursive) {
    // About, Keyboard Shortcuts, Documentation
}
```

## Testing Plan

1. Build: `cargo build --release`
2. Run: `./target/release/osvm chat --advanced`
3. Verify:
   - [x] Menu bar appears at top
   - [x] Sessions load from disk (no duplicates)
   - [x] MCP tools are collapsible by server
   - [x] Input area has no duplicate buttons
   - [x] Agent status shows in menu bar
   - [x] F1 opens help from menu bar

## Files to Modify

- `src/utils/agent_chat_v2/ui/layout.rs` - Main layout changes
- `src/utils/agent_chat_v2/ui/components.rs` - Sidebar components
- `src/utils/agent_chat_v2/ui/handlers.rs` - Menu handlers
- `src/utils/agent_chat_v2/ui/display.rs` - Update refresh logic

## Estimated Impact

- Lines changed: ~200-300
- New methods: 5-7
- Build time: ~2 minutes
- Testing time: 10 minutes
