#!/bin/bash

# OSVM CLI Release Installation Script
# This script builds the release binary and installs it to /usr/bin

set -e  # Exit on any error

# Ensure we can interact with the terminal
exec < /dev/tty

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BINARY_NAME="osvm"
INSTALL_PATH="/usr/bin/$BINARY_NAME"
BACKUP_PATH="/usr/bin/${BINARY_NAME}.backup"

echo "🚀 OSVM CLI Release Installation"
echo "================================"

# Check if we're in the correct directory
if [ ! -f "$SCRIPT_DIR/Cargo.toml" ]; then
    echo "❌ Error: Cargo.toml not found. Please run this script from the project root."
    exit 1
fi

# Change to project directory
cd "$SCRIPT_DIR"

echo "📦 Building release binary..."
cargo build --release

# Check if build was successful
if [ ! -f "target/release/$BINARY_NAME" ]; then
    echo "❌ Error: Release binary not found at target/release/$BINARY_NAME"
    echo "   Make sure the build completed successfully."
    exit 1
fi

echo "✅ Release binary built successfully"

# Check if binary already exists and create backup
if [ -f "$INSTALL_PATH" ]; then
    echo "📋 Existing binary found at $INSTALL_PATH"
    
    # Get version of existing binary
    EXISTING_VERSION=$($INSTALL_PATH --version 2>/dev/null || echo "unknown")
    echo "   Current version: $EXISTING_VERSION"
    
    echo "💾 Creating backup at $BACKUP_PATH..."
    sudo cp "$INSTALL_PATH" "$BACKUP_PATH"
    echo "✅ Backup created"
fi

# Get version of new binary
NEW_VERSION=$(./target/release/$BINARY_NAME --version 2>/dev/null || echo "unknown")
echo "🔄 Installing new version: $NEW_VERSION"

# Install the new binary
echo "📥 Installing binary to $INSTALL_PATH..."
sudo cp "target/release/$BINARY_NAME" "$INSTALL_PATH"

# Make sure it's executable
sudo chmod +x "$INSTALL_PATH"

# Verify installation
if [ -f "$INSTALL_PATH" ] && [ -x "$INSTALL_PATH" ]; then
    echo "✅ Installation successful!"
    
    # Test the installed binary
    echo "🧪 Testing installed binary..."
    INSTALLED_VERSION=$($INSTALL_PATH --version 2>/dev/null || echo "failed")
    
    if [ "$INSTALLED_VERSION" != "failed" ]; then
        echo "✅ Binary test successful: $INSTALLED_VERSION"
        echo ""
        echo "🎉 OSVM CLI has been successfully installed!"
        echo "   Location: $INSTALL_PATH"
        echo "   Version: $INSTALLED_VERSION"
        if [ -f "$BACKUP_PATH" ]; then
            echo "   Backup: $BACKUP_PATH"
        fi
        echo ""
        echo "💡 You can now use 'osvm' from anywhere in your system."
        echo "   Try running: osvm --help"
    else
        echo "❌ Error: Installed binary failed to run"
        
        # Restore backup if it exists
        if [ -f "$BACKUP_PATH" ]; then
            echo "🔄 Restoring backup..."
            sudo cp "$BACKUP_PATH" "$INSTALL_PATH"
            echo "✅ Backup restored"
        fi
        exit 1
    fi
else
    echo "❌ Error: Installation failed"
    exit 1
fi

echo ""
echo "📚 Additional Commands:"
echo "   osvm --help                    # Show help"
echo "   osvm rpc test --status # Check test RPC status"
echo "   osvm rpc devnet --help # Devnet RPC options"
echo "   osvm diagnostics              # Run system diagnostics"
tt