#!/bin/sh
# OSVM CLI Installer
# This script installs the OSVM CLI tool for managing Solana Virtual Machines

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "${BLUE}OSVM CLI Installer${NC}"
echo "=============================="

# Check if Rust is installed
if ! command -v rustc >/dev/null 2>&1; then
    echo "${YELLOW}Rust is not installed. Installing Rust...${NC}"
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    . "$HOME/.cargo/env"
    echo "${GREEN}Rust installed successfully!${NC}"
else
    echo "${GREEN}Rust is already installed.${NC}"
fi

# Check if Cargo is installed
if ! command -v cargo >/dev/null 2>&1; then
    echo "${RED}Cargo is not available. Please ensure Rust is properly installed.${NC}"
    exit 1
else
    echo "${GREEN}Cargo is available.${NC}"
fi

# Check for Solana CLI tools
if ! command -v solana >/dev/null 2>&1; then
    echo "${YELLOW}Solana CLI tools not found. It's recommended to install them.${NC}"
    echo "You can install Solana CLI tools with: sh -c \"\$(curl -sSfL https://release.solana.com/v1.14.29/install)\""
fi

echo "${BLUE}Installing OSVM CLI...${NC}"

# Install directly from GitHub repository
echo "${BLUE}Installing from GitHub repository...${NC}"

# Create a temporary directory
TMP_DIR=$(mktemp -d)
cd "$TMP_DIR"

# Clone the repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Build and install
cargo build --release

# Determine install location based on platform
if [ "$(uname)" = "Darwin" ]; then
    # macOS
    INSTALL_DIR="/usr/local/bin"
else
    # Linux
    INSTALL_DIR="/usr/local/bin"
fi

# Copy binary to install location
if [ -w "$INSTALL_DIR" ]; then
    cp target/release/osvm "$INSTALL_DIR/"
else
    echo "${YELLOW}Copying binary to $INSTALL_DIR requires sudo permission${NC}"
    sudo cp target/release/osvm "$INSTALL_DIR/"
fi

# Clean up
cd
rm -rf "$TMP_DIR"

# Final check
if command -v osvm >/dev/null 2>&1; then
    echo "${GREEN}OSVM CLI installed successfully!${NC}"
    echo ""
    echo "You can now use the OSVM CLI with the 'osvm' command."
    echo "Try 'osvm --help' to get started."
else
    echo "${RED}Installation failed. Please try installing manually:${NC}"
    echo "1. Clone the repository: git clone https://github.com/opensvm/osvm-cli.git"
    echo "2. Build the project: cd osvm-cli && cargo build --release"
    echo "3. Install the binary: sudo cp target/release/osvm /usr/local/bin/"
fi