# Chat Interface Test Instructions

The chat interfaces have been fixed with the following improvements:

## Fixes Applied

### 1. Runtime Panic Issues
- **Basic Chat**: Removed nested runtime creation in `agent_chat.rs` line 511
- **Advanced Chat**:
  - Changed spinner animation from `tokio::spawn` to `std::thread::spawn`
  - Added `send_agent_command_sync` method for UI callbacks
  - Fixed all UI callbacks to use sync method instead of `tokio::spawn`

### 2. Clipboard Support
- Added `arboard` dependency for system clipboard integration
- Updated `copy_last_message` function to use real clipboard
- Alt+C now copies messages to system clipboard

### 3. Terminal Detection
- Both interfaces properly detect terminal availability
- Falls back to demo mode when no TTY is available

## Testing Instructions

### In a Terminal
Run these commands in a proper terminal (not through SSH or CI):

```bash
# Test basic chat
./target/release/osvm chat

# Test advanced chat with FAR-style UI
./target/release/osvm chat --advanced
```

### Features to Test

#### Basic Chat
- Type messages and press Enter to send
- Check if MCP servers are listed
- Try Tab for command completion
- Press Ctrl+C or type 'quit' to exit

#### Advanced Chat (--advanced flag)
1. **Session Management**
   - Click "+ New Chat" button to create new sessions
   - Switch between sessions in the left panel

2. **Clipboard (Alt+C)**
   - Send a message
   - Press Alt+C to copy last message
   - Paste somewhere to verify it worked

3. **Agent Controls**
   - Run/Pause/Stop buttons for agent execution
   - Record/Stop Rec for session recording

4. **Suggestions (Numbers 1-5)**
   - After sending a message, suggestions should appear
   - Press 1-5 to insert suggestion at cursor
   - Press ESC to hide suggestions

5. **Other Hotkeys**
   - Alt+R: Retry last message
   - Alt+D: Delete last message
   - Alt+F: Fork conversation
   - Alt+M: Switch to standard mode

6. **Bottom Buttons**
   - Clear Chat: Clears current session
   - Export Chat: Saves chat to file
   - Settings: Opens settings dialog
   - Help: Shows help screen
   - Quit: Exits application

## Known Limitations

1. **No TTY Environment**: When running without a terminal (CI, some SSH sessions), falls back to demo mode showing static examples
2. **Copy/Paste**: System copy works with Alt+C. Paste depends on terminal emulator (usually Ctrl+Shift+V or Cmd+V)
3. **Text Selection**: Terminal-based UIs don't support mouse text selection by default. Use Alt+C to copy messages instead

## Troubleshooting

If the UI doesn't start:
1. Make sure you're in a proper terminal (not CI mode)
2. Check TERM environment variable is set: `echo $TERM`
3. Try different terminal emulators (xterm, gnome-terminal, iTerm2)

If clipboard doesn't work:
1. On Linux: Ensure xclip or xsel is installed
2. On macOS: Should work out of the box
3. On Windows: May need to run in Windows Terminal
4. Fallback: Text is shown in dialog if clipboard fails