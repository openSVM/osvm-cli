#!/bin/bash
# Launch OSVM Chat with VS Code Theme in a proper terminal

echo "🚀 Launching OSVM Chat with VS Code Dark+ Theme..."
echo ""
echo "This will open a new terminal window with the advanced chat interface."
echo "The interface features:"
echo "  • VS Code Dark+ color scheme (#1e1e1e background)"
echo "  • Professional blue accents (#007acc)"
echo "  • 12 complete UX systems"
echo "  • AI-powered tool planning"
echo "  • MCP server integration"
echo ""

# Path to the OSVM binary
OSVM_BIN="/home/larp/larpdevs/osvm-cli/target/release/osvm"

# Check if binary exists
if [ ! -f "$OSVM_BIN" ]; then
    echo "❌ Binary not found at $OSVM_BIN"
    echo "Building release binary..."
    cd /home/larp/larpdevs/osvm-cli
    cargo build --release
fi

# Detect available terminal emulator
if command -v gnome-terminal &> /dev/null; then
    TERMINAL="gnome-terminal"
    TERMINAL_CMD="gnome-terminal -- bash -c '$OSVM_BIN chat --advanced; exec bash'"
elif command -v xterm &> /dev/null; then
    TERMINAL="xterm"
    TERMINAL_CMD="xterm -e '$OSVM_BIN chat --advanced; bash'"
elif command -v konsole &> /dev/null; then
    TERMINAL="konsole"
    TERMINAL_CMD="konsole -e '$OSVM_BIN chat --advanced'"
elif command -v alacritty &> /dev/null; then
    TERMINAL="alacritty"
    TERMINAL_CMD="alacritty -e bash -c '$OSVM_BIN chat --advanced; exec bash'"
elif command -v tilix &> /dev/null; then
    TERMINAL="tilix"
    TERMINAL_CMD="tilix -e '$OSVM_BIN chat --advanced'"
else
    echo "❌ No suitable terminal emulator found!"
    echo "Please install one of: gnome-terminal, xterm, konsole, alacritty, tilix"
    exit 1
fi

echo "✅ Using terminal: $TERMINAL"
echo ""
echo "📋 Quick Start Guide:"
echo "  • F1 - Comprehensive help"
echo "  • ? - Quick shortcuts"
echo "  • ↑/↓ - History navigation"
echo "  • Alt+R - New session"
echo "  • Alt+C - Clear chat"
echo "  • F12 - Take screenshot"
echo "  • Ctrl+C - Exit"
echo ""
echo "🎨 VS Code Theme Features:"
echo "  • Background: #1e1e1e (VS Code dark)"
echo "  • Primary: #007acc (VS Code blue)"
echo "  • Success: #6a9955 (VS Code green)"
echo "  • Error: #f44747 (VS Code red)"
echo "  • Text: #d4d4d4 (VS Code default text)"
echo ""
echo "Press Enter to launch..."
read -r

# Launch in new terminal window
eval "$TERMINAL_CMD" &

echo "✅ Chat interface launched in new window!"
echo ""
echo "💡 Tips:"
echo "  • Try the AI-powered reply suggestions (1-5 keys)"
echo "  • Use F1 for the full help system"
echo "  • The interface supports multiple chat sessions"
echo "  • Press F12 to take screenshots"
