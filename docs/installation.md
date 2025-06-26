# Installation

This guide provides comprehensive installation instructions for OSVM CLI across different platforms and deployment scenarios.

## Overview

OSVM CLI can be installed through multiple methods including automated installation scripts, package managers, pre-built binaries, and building from source. Choose the method that best fits your environment and requirements.

## Quick Installation

### Automated Installation (Recommended)

The fastest way to install OSVM CLI on Linux and macOS:

```bash
# Install latest stable version
curl -sSfL https://install.osvm.dev | sh

# Install specific version
curl -sSfL https://install.osvm.dev | sh -s -- --version 1.2.0

# Install to custom directory
curl -sSfL https://install.osvm.dev | sh -s -- --dir /usr/local/bin
```

### Platform-Specific Quick Install

**Linux:**
```bash
# Using curl
curl -sSfL https://install.osvm.dev | sh

# Using wget
wget -qO- https://install.osvm.dev | sh
```

**macOS:**
```bash
# Using Homebrew (coming soon)
brew install osvm-cli

# Using curl
curl -sSfL https://install.osvm.dev | sh
```

**Windows:**
```powershell
# Using PowerShell
iwr -useb https://install.osvm.dev/windows | iex

# Using Chocolatey (coming soon)
choco install osvm-cli
```

## Installation Methods

### Method 1: Automated Script Installation

The automated installation script handles dependency installation, platform detection, and environment setup.

```mermaid
flowchart TD
    subgraph "Automated Installation Process"
        START[Run Install Script]
        DETECT[Detect Platform]
        DEPS[Install Dependencies]
        DOWNLOAD[Download Binary]
        INSTALL[Install OSVM CLI]
        CONFIG[Setup Configuration]
        VERIFY[Verify Installation]
        COMPLETE[Installation Complete]
    end
    
    START --> DETECT
    DETECT --> DEPS
    DEPS --> DOWNLOAD
    DOWNLOAD --> INSTALL
    INSTALL --> CONFIG
    CONFIG --> VERIFY
    VERIFY --> COMPLETE
    
    classDef techDebt fill:#f6f6f6,stroke:#d9534f,color:#d9534f,font-family:Consolas,monospace,font-weight:bold
```

**Installation Script Features:**
- Automatic platform detection
- Dependency installation
- Binary verification
- PATH configuration
- Initial setup

**Installation Options:**
```bash
# Standard installation
curl -sSfL https://install.osvm.dev | sh

# Custom installation directory
curl -sSfL https://install.osvm.dev | sh -s -- --dir /opt/osvm

# Install specific version
curl -sSfL https://install.osvm.dev | sh -s -- --version 1.2.0

# Install pre-release version
curl -sSfL https://install.osvm.dev | sh -s -- --pre-release

# Verbose installation
curl -sSfL https://install.osvm.dev | sh -s -- --verbose

# Skip dependency installation
curl -sSfL https://install.osvm.dev | sh -s -- --no-deps
```

### Method 2: Package Manager Installation

#### Ubuntu/Debian (APT)

```bash
# Add OSVM repository
curl -fsSL https://packages.osvm.dev/gpg | sudo gpg --dearmor -o /usr/share/keyrings/osvm.gpg
echo "deb [signed-by=/usr/share/keyrings/osvm.gpg] https://packages.osvm.dev/apt stable main" | sudo tee /etc/apt/sources.list.d/osvm.list

# Update package list
sudo apt update

# Install OSVM CLI
sudo apt install osvm-cli

# Install specific version
sudo apt install osvm-cli=1.2.0
```

#### CentOS/RHEL/Fedora (YUM/DNF)

```bash
# Add OSVM repository
sudo tee /etc/yum.repos.d/osvm.repo << EOF
[osvm]
name=OSVM CLI Repository
baseurl=https://packages.osvm.dev/yum
enabled=1
gpgcheck=1
gpgkey=https://packages.osvm.dev/gpg
EOF

# Install OSVM CLI
sudo yum install osvm-cli
# or
sudo dnf install osvm-cli
```

#### macOS (Homebrew)

```bash
# Add OSVM tap
brew tap osvm/cli

# Install OSVM CLI
brew install osvm-cli

# Install specific version
brew install osvm-cli@1.2.0

# Upgrade to latest version
brew upgrade osvm-cli
```

#### Windows (Chocolatey)

```powershell
# Install OSVM CLI
choco install osvm-cli

# Install specific version
choco install osvm-cli --version 1.2.0

# Upgrade to latest version
choco upgrade osvm-cli
```

### Method 3: Pre-built Binaries

Download and install pre-built binaries for your platform.

#### Linux

```bash
# Download latest binary
wget https://github.com/openSVM/osvm-cli/releases/latest/download/osvm-linux-x64.tar.gz

# Extract and install
tar -xzf osvm-linux-x64.tar.gz
sudo mv osvm /usr/local/bin/
sudo chmod +x /usr/local/bin/osvm

# Verify installation
osvm --version
```

#### macOS

```bash
# Download latest binary
curl -LO https://github.com/openSVM/osvm-cli/releases/latest/download/osvm-macos-x64.tar.gz

# Extract and install
tar -xzf osvm-macos-x64.tar.gz
sudo mv osvm /usr/local/bin/
sudo chmod +x /usr/local/bin/osvm

# Verify installation
osvm --version
```

#### Windows

```powershell
# Download latest binary
Invoke-WebRequest -Uri "https://github.com/openSVM/osvm-cli/releases/latest/download/osvm-windows-x64.zip" -OutFile "osvm.zip"

# Extract and install
Expand-Archive -Path osvm.zip -DestinationPath C:\osvm
$env:PATH += ";C:\osvm"

# Verify installation
osvm --version
```

### Method 4: Building from Source

Build OSVM CLI from source code for custom configurations or development.

#### Prerequisites

**System Requirements:**
- Rust 1.70.0 or later
- Git
- C compiler (gcc, clang, or MSVC)
- OpenSSL development libraries
- pkg-config

#### Install Prerequisites

**Ubuntu/Debian:**
```bash
sudo apt update
sudo apt install -y build-essential git curl pkg-config libssl-dev
```

**CentOS/RHEL/Fedora:**
```bash
sudo yum groupinstall -y "Development Tools"
sudo yum install -y git curl pkg-config openssl-devel
# or
sudo dnf groupinstall -y "Development Tools"
sudo dnf install -y git curl pkg-config openssl-devel
```

**macOS:**
```bash
# Install Xcode command line tools
xcode-select --install

# Install Homebrew if not installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install dependencies
brew install git openssl pkg-config
```

**Windows:**
```powershell
# Install Visual Studio Build Tools
# Download from: https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022

# Install Git
# Download from: https://git-scm.com/download/win
```

#### Install Rust

```bash
# Install Rust using rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Source the environment
source ~/.cargo/env

# Verify Rust installation
rustc --version
cargo --version
```

#### Build and Install

```bash
# Clone the repository
git clone https://github.com/openSVM/osvm-cli.git
cd osvm-cli

# Build release version
cargo build --release

# Install to system
sudo cp target/release/osvm /usr/local/bin/

# Or install using cargo
cargo install --path .

# Verify installation
osvm --version
```

#### Build Options

```bash
# Build with specific features
cargo build --release --features "ssl-vendored"

# Build for different target
cargo build --release --target x86_64-unknown-linux-musl

# Build with optimization
RUSTFLAGS="-C target-cpu=native" cargo build --release

# Build debug version
cargo build
```

## System Requirements

### Minimum Requirements

**Hardware:**
- CPU: x86_64 or ARM64 (Apple Silicon)
- RAM: 4GB minimum, 8GB recommended
- Disk: 10GB free space for installation and data
- Network: Internet connection for downloads and operations

**Operating System:**
- **Linux:** Ubuntu 20.04+, CentOS 8+, Debian 10+, Fedora 35+
- **macOS:** macOS 10.15+ (Catalina and later)
- **Windows:** Windows 10+ or Windows Server 2019+

### Dependencies

**Required Dependencies:**
- OpenSSL 1.1.1+ or LibreSSL 3.3.0+
- Git (for source builds)
- SSH client (for remote operations)

**Optional Dependencies:**
- Docker (for containerized deployments)
- ngrok (for external accessibility features)
- jq (for JSON processing in scripts)

## Platform-Specific Installation

### Linux Installation

#### Ubuntu/Debian Detailed Setup

```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Install required dependencies
sudo apt install -y curl wget git build-essential pkg-config libssl-dev

# Install OSVM CLI
curl -sSfL https://install.osvm.dev | sh

# Add to PATH (if not automatically added)
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Verify installation
osvm --version

# Test basic functionality
osvm examples --list-categories
```

#### CentOS/RHEL/Fedora Detailed Setup

```bash
# Update system packages
sudo yum update -y
# or
sudo dnf update -y

# Install EPEL repository (CentOS/RHEL)
sudo yum install -y epel-release

# Install required dependencies
sudo yum groupinstall -y "Development Tools"
sudo yum install -y curl wget git openssl-devel pkg-config

# Install OSVM CLI
curl -sSfL https://install.osvm.dev | sh

# Add to PATH
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Verify installation
osvm --version
```

#### Arch Linux Setup

```bash
# Update system
sudo pacman -Syu

# Install dependencies
sudo pacman -S curl wget git base-devel openssl pkg-config

# Install OSVM CLI
curl -sSfL https://install.osvm.dev | sh

# Add to PATH
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Verify installation
osvm --version
```

### macOS Installation

#### Homebrew Installation (Recommended)

```bash
# Install Homebrew if not present
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Add OSVM tap
brew tap osvm/cli

# Install OSVM CLI
brew install osvm-cli

# Verify installation
osvm --version
```

#### Manual Installation on macOS

```bash
# Install Xcode command line tools
xcode-select --install

# Install OSVM CLI
curl -sSfL https://install.osvm.dev | sh

# Add to PATH
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Verify installation
osvm --version
```

### Windows Installation

#### PowerShell Installation

```powershell
# Set execution policy (if needed)
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

# Install OSVM CLI
iwr -useb https://install.osvm.dev/windows | iex

# Verify installation
osvm --version
```

#### Manual Windows Installation

```powershell
# Create installation directory
New-Item -Path "C:\osvm" -ItemType Directory -Force

# Download binary
Invoke-WebRequest -Uri "https://github.com/openSVM/osvm-cli/releases/latest/download/osvm-windows-x64.zip" -OutFile "osvm.zip"

# Extract binary
Expand-Archive -Path osvm.zip -DestinationPath C:\osvm -Force

# Add to PATH permanently
$currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
$newPath = "$currentPath;C:\osvm"
[Environment]::SetEnvironmentVariable("PATH", $newPath, "User")

# Refresh current session PATH
$env:PATH += ";C:\osvm"

# Verify installation
osvm --version
```

### Docker Installation

Run OSVM CLI in a containerized environment.

```bash
# Pull official Docker image
docker pull osvm/cli:latest

# Run OSVM CLI container
docker run -it --rm osvm/cli:latest osvm --version

# Run with volume mounting for persistence
docker run -it --rm \
  -v ~/.osvm:/root/.osvm \
  -v ~/.ssh:/root/.ssh:ro \
  osvm/cli:latest bash

# Create alias for easier usage
echo 'alias osvm="docker run -it --rm -v ~/.osvm:/root/.osvm -v ~/.ssh:/root/.ssh:ro osvm/cli:latest osvm"' >> ~/.bashrc
```

## Post-Installation Setup

### Initial Configuration

```bash
# Initialize OSVM CLI configuration
osvm config init

# Set default network
osvm config set --network mainnet

# Set default keypair (optional)
osvm config set --keypair ~/.config/solana/id.json

# Verify configuration
osvm config show
```

### Environment Setup

#### Shell Integration

**Bash/Zsh Completion:**
```bash
# Generate completion script
osvm completion bash > ~/.osvm-completion.bash
osvm completion zsh > ~/.osvm-completion.zsh

# Add to shell configuration
echo 'source ~/.osvm-completion.bash' >> ~/.bashrc
# or
echo 'source ~/.osvm-completion.zsh' >> ~/.zshrc
```

**Fish Completion:**
```bash
# Generate and install Fish completion
osvm completion fish > ~/.config/fish/completions/osvm.fish
```

#### SSH Configuration

```bash
# Generate SSH key for validator access
ssh-keygen -t ed25519 -f ~/.ssh/osvm_validator_key

# Add SSH key to agent
ssh-add ~/.ssh/osvm_validator_key

# Configure SSH for OSVM usage
cat >> ~/.ssh/config << EOF
Host validator-*
  User solana
  IdentityFile ~/.ssh/osvm_validator_key
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null
EOF
```

### Verification and Testing

#### Installation Verification

```bash
# Check OSVM CLI version
osvm --version

# Run basic health check
osvm doctor

# Test network connectivity
osvm test-network --network devnet

# List available examples
osvm examples --list-categories

# Test SVM functionality
osvm svm list
```

#### Comprehensive Test

```bash
# Run comprehensive installation test
osvm test-installation

# Expected output:
# ✅ Binary executable
# ✅ Configuration accessible
# ✅ Network connectivity
# ✅ SSH functionality
# ✅ Dependencies available
# ✅ Installation complete
```

## Troubleshooting Installation

### Common Issues

#### Permission Errors

**Linux/macOS:**
```bash
# Fix binary permissions
chmod +x ~/.osvm/bin/osvm

# Fix directory permissions
chmod 755 ~/.osvm

# Install with sudo if needed
sudo curl -sSfL https://install.osvm.dev | sh
```

#### PATH Issues

```bash
# Check if OSVM is in PATH
which osvm

# Add to PATH manually
export PATH="$HOME/.osvm/bin:$PATH"

# Make permanent
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Dependency Issues

**Ubuntu/Debian:**
```bash
# Install missing dependencies
sudo apt install -y curl wget git build-essential libssl-dev pkg-config

# Update certificates
sudo apt update && sudo apt install -y ca-certificates
```

**CentOS/RHEL:**
```bash
# Install missing dependencies
sudo yum groupinstall -y "Development Tools"
sudo yum install -y openssl-devel curl wget git

# Update certificates
sudo yum update -y ca-certificates
```

#### Network Issues

```bash
# Test network connectivity
curl -I https://github.com

# Use alternative download method
wget -qO- https://install.osvm.dev | sh

# Manual download if script fails
wget https://github.com/openSVM/osvm-cli/releases/latest/download/osvm-linux-x64.tar.gz
```

### Debug Installation

```bash
# Run installation with verbose output
curl -sSfL https://install.osvm.dev | sh -s -- --verbose

# Check installation logs
cat ~/.osvm/install.log

# Test individual components
osvm doctor --component network
osvm doctor --component dependencies
osvm doctor --component configuration
```

## Updating OSVM CLI

### Automatic Updates

```bash
# Update to latest version
osvm update

# Update with confirmation
osvm update --confirm

# Check for available updates
osvm update --check
```

### Manual Updates

```bash
# Re-run installation script for latest version
curl -sSfL https://install.osvm.dev | sh

# Update using package manager
sudo apt update && sudo apt upgrade osvm-cli  # Ubuntu/Debian
brew upgrade osvm-cli  # macOS Homebrew
```

### Version Management

```bash
# List installed versions
osvm version --list

# Switch between versions
osvm version use 1.2.0

# Remove old versions
osvm version remove 1.1.0
```

## Uninstallation

### Complete Removal

```bash
# Run uninstall script
curl -sSfL https://install.osvm.dev/uninstall | sh

# Or manual removal
rm -rf ~/.osvm
sudo rm -f /usr/local/bin/osvm

# Remove from PATH
# Edit ~/.bashrc, ~/.zshrc, etc. and remove OSVM PATH entries

# Remove package manager installations
sudo apt remove osvm-cli  # Ubuntu/Debian
brew uninstall osvm-cli   # macOS Homebrew
```

### Selective Removal

```bash
# Keep configuration, remove only binary
rm ~/.osvm/bin/osvm

# Keep binary, reset configuration
rm -rf ~/.osvm/config

# Remove specific components
osvm clean --cache
osvm clean --logs
osvm clean --temp
```

## Related Documentation

- [Configuration](configuration.md) - Configuration management
- [Examples](examples.md) - Usage examples
- [SVM Management](svm-management.md) - SVM installation and management
- [Troubleshooting](../README.md#troubleshooting) - General troubleshooting guide