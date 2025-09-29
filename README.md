# OSVM CLI

[![License: MIT](https://img.shields.io/badge/License-MIT-gray.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/Rust-1.80.0+-gray.svg)](https://www.rust-lang.org/)
[![Solana](https://img.shields.io/badge/Solana-1.14.29+-gray.svg)](https://solana.com/)

A powerful command-line interface for managing Solana Virtual Machines (SVMs) across networks. Deploy, monitor, and manage SVM infrastructure with AI assistance, MCP server integration, and extensible plugin architecture.

## 🚀 Quick Start

### Installation

**One-line install:**
```bash
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh
```

**Alternative methods:**
<details>
<summary>Windows, Docker, and Source installations</summary>

**Windows:**
```bash
powershell -Command "Invoke-WebRequest -Uri 'https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.ps1' -OutFile 'install.ps1'; .\install.ps1"
```

**Docker:**
```bash
docker pull ghcr.io/opensvm/osvm-cli:latest
docker run --rm ghcr.io/opensvm/osvm-cli:latest --version
```

**From Source:**
```bash
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli && cargo build --release
sudo cp target/release/osvm /usr/local/bin/
```
</details>

### First Steps

```bash
# Check installation
osvm --version

# List available SVMs
osvm svm list

# Get help and examples
osvm examples

# Launch interactive dashboard
osvm svm dashboard
```

## 📋 Table of Contents

### Getting Started
- [🚀 Quick Start](#-quick-start)
- [🏗️ Core Features](#️-core-features)
- [📖 Basic Commands](#-basic-commands)

### SVM Operations
- [🔧 SVM Management](#-svm-management)
- [🚀 Node Deployment](#-node-deployment)
- [📡 RPC Services](#-rpc-services)
- [💾 eBPF Deployment](#-ebpf-deployment)

### Advanced Features
- [🤖 AI Integration](#-ai-integration)
- [🔌 MCP Server Management](#-mcp-server-management)
- [🧩 Plugin System](#-plugin-system)

### Tools & Monitoring
- [📊 Dashboard](#-dashboard)
- [🔍 System Health](#-system-health)
- [⚙️ CI/CD Integration](#️-cicd-integration)

### Development
- [🛠️ Plugin Development](#️-plugin-development)
- [📚 Documentation](#-documentation)
- [🤝 Contributing](#-contributing)

## 🏗️ Core Features

### Infrastructure Management
- **SVM Deployment** - Deploy and manage Solana Virtual Machines across networks
- **Node Operations** - Validator and RPC node deployment with automated configuration
- **Remote Management** - SSH-based deployment and monitoring
- **Multi-Network Support** - Mainnet, testnet, and devnet operations

### Intelligence & Automation
- **AI Integration** - Built-in AI for security analysis and blockchain queries
- **MCP Server Support** - Connect to specialized blockchain data services
- **Plugin Architecture** - Extend functionality with custom commands and tools
- **Self-Repair Systems** - Automated maintenance and diagnostics

### Monitoring & Performance
- **Interactive Dashboard** - Real-time TUI monitoring interface
- **Performance Tracking** - TPS, latency, and resource monitoring
- **Log Management** - Centralized logging and analysis
- **Health Checks** - Automated system diagnostics

## 📖 Basic Commands

### Information & Help
```bash
# Get help and examples
osvm examples
osvm examples --category svm

# Check system health
osvm doctor
osvm doctor --fix

# Get wallet balance
osvm balance [ADDRESS]
```

### SVM Operations
```bash
# List all SVMs
osvm svm list

# Get SVM details
osvm svm get sonic

# Interactive SVM dashboard
osvm svm dashboard
```

### AI Assistance
```bash
# Ask AI questions
osvm "How do I deploy a validator?"
osvm "What are Solana security best practices?"
osvm "Explain this error message: [paste error]"
```

## 🔧 SVM Management

### Viewing SVMs
```bash
# List all available SVMs
osvm svm list

# Get detailed information about a specific SVM
osvm svm get sonic
osvm svm get solana
osvm svm get eclipse
```

### Interactive Dashboard
```bash
# Launch comprehensive SVM monitoring dashboard
osvm svm dashboard
```

**Dashboard Features:**
- Overview of all SVMs with status indicators
- Network details (mainnet, testnet, devnet)
- Performance metrics with real-time visualization
- Node status monitoring with resource usage
- Aggregated logs from all nodes

**Keyboard Controls:**
- `Tab`, `→`, `←`: Switch between tabs
- `↑`, `↓`: Navigate through items
- `n`/`p`: Select next/previous SVM
- `v`: Toggle verbosity level
- `h`: Toggle help overlay
- `q` or `Ctrl+C`: Quit

## 🚀 Node Deployment

### Single Node Deployment
```bash
# Deploy validator node (mainnet)
osvm user@host --svm sonic --node-type validator --network mainnet

# Deploy RPC node (testnet)
osvm user@host --svm sonic --node-type rpc --network testnet

# Deploy to devnet for testing
osvm user@host --svm solana --node-type validator --network devnet
```

### Multi-SVM Deployment
```bash
# Deploy multiple SVMs to a single server
osvm user@host --svm sonic,solana,eclipse,soon --node-type validator --network devnet
```

### Node Management
```bash
# List deployed nodes
osvm nodes list

# Check node status
osvm nodes status NODE-ID

# View node logs
osvm nodes logs NODE-ID
osvm nodes logs NODE-ID --follow

# Control node operations
osvm nodes restart NODE-ID
osvm nodes stop NODE-ID
```

## 📡 RPC Services

### RPC Deployment
```bash
# Deploy Sonic RPC node
osvm rpc sonic user@host --network mainnet

# Deploy with specific configuration
osvm rpc sonic user@host --network testnet --version latest
```

### Local Development
```bash
# Start local test validator (instant setup)
osvm rpc-manager test --background --reset

# Check test validator status
osvm rpc-manager test --status

# Stop test validator
osvm rpc-manager test --stop
```

### RPC Monitoring
```bash
# Monitor RPC logs
osvm rpc-manager devnet --logs
osvm rpc-manager devnet --logs --follow --lines 100

# Query Solana network
osvm rpc-manager query-solana --info
osvm rpc-manager query-solana --health
```

**RPC Features:**
- **Test Validator**: Local development with built-in faucet
- **Devnet RPC**: Real devnet validator with auto-repair
- **Network Monitoring**: Health checks and performance metrics
- **Auto-Configuration**: Handles dependencies and setup automatically

## 💾 eBPF Deployment

### Basic Deployment
```bash
# Deploy to all networks
osvm deploy ./program.so \
  --program-id ./program-id.json \
  --owner ./owner-keypair.json \
  --fee ./fee-payer.json \
  --publish-idl

# Deploy to specific network
osvm deploy ./program.so \
  --program-id ./program-id.json \
  --owner ./owner-keypair.json \
  --fee ./fee-payer.json \
  --network mainnet
```

### Advanced Deployment
```bash
# Deploy with custom IDL
osvm deploy ./program.so \
  --program-id ./program-id.json \
  --owner ./owner-keypair.json \
  --fee ./fee-payer.json \
  --publish-idl \
  --idl-file ./custom-idl.json
```

### File Requirements

**Keypair files** (for new deployments):
```json
[123,45,67,89,...,234]  // Array of 64 bytes
```

**Pubkey-only files** (for upgrades):
```json
{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}
```

**Required files:**
- eBPF binary (.so file)
- Program ID (keypair for new, pubkey for upgrades)
- Program owner keypair (private key required)
- Fee payer keypair (private key required)

## 🤖 AI Integration

### AI Configuration
```bash
# Set up OpenAI
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-openai-api-key"

# Use local AI (Ollama)
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama-key"
```

### AI Queries
```bash
# General blockchain questions
osvm "What are the best practices for Solana smart contract security?"

# Deployment guidance
osvm "Help me optimize my validator performance"

# Error analysis
osvm "Analyze this smart contract for vulnerabilities"
```

### Security Auditing
```bash
# Audit local code
osvm audit

# Audit GitHub repository
osvm audit https://github.com/user/solana-program

# Audit with specific output format
osvm audit ./contracts --format json --output ./audit-report.json
```

**AI Features:**
- Smart contract security analysis
- Automated code review
- Best practices guidance
- Multi-provider support (OpenAI, Anthropic, local models)
- Privacy-focused local AI options

## 🔌 MCP Server Management

Model Context Protocol (MCP) integration provides specialized blockchain data access and external service connections.

### Quick Setup
```bash
# Quick setup with Solana MCP server
osvm mcp setup --auto-enable

# List configured servers
osvm mcp list

# Test connectivity
osvm mcp test solana-server
```

### Adding Servers

**From GitHub:**
```bash
# Add official Solana MCP server
osvm mcp add-github solana-mcp https://github.com/openSVM/solana-mcp-server --enabled

# Add custom server
osvm mcp add-github my-server https://github.com/user/custom-mcp-server --enabled
```

**Manual Configuration:**
```bash
# HTTP server with authentication
osvm mcp add blockchain-data \
  --server-url https://api.blockchain-mcp.com \
  --transport http \
  --auth-type bearer \
  --auth-token your-token \
  --enabled

# WebSocket server
osvm mcp add realtime-data \
  --server-url wss://ws.realtime-mcp.com \
  --transport websocket \
  --auth-type api_key \
  --auth-token your-key \
  --enabled
```

### Using MCP Tools
```bash
# List available tools
osvm mcp tools
osvm mcp tools solana-server

# Execute blockchain queries
osvm mcp call solana-server getBalance \
  --args '{"pubkey":"11111111111111111111111111111112"}'

# Chain operations
osvm mcp call solana-server getStakeAccounts \
  --args '{"staker":"Your-Address"}' | \
osvm mcp call solana-server getStakeRewards \
  --args '{"accounts":"@stdin"}'
```

### Server Management
```bash
# Enable/disable servers
osvm mcp enable solana-server
osvm mcp disable old-server

# Remove server
osvm mcp remove unused-server

# Search servers
osvm mcp search "solana balance"
```

<details>
<summary>Authentication Methods</summary>

**Bearer Token:**
```bash
osvm mcp add api-server \
  --server-url https://api.example.com \
  --auth-type bearer \
  --auth-token "Bearer eyJ0eXAiOiJKV1Q..."
```

**API Key:**
```bash
osvm mcp add data-server \
  --server-url https://data.example.com \
  --auth-type api_key \
  --auth-token "your-api-key"
```

**Basic Auth:**
```bash
osvm mcp add secure-server \
  --server-url https://secure.example.com \
  --auth-type basic \
  --username your-username \
  --password your-password
```
</details>

## 🧩 Plugin System

OSVM's plugin architecture allows extending functionality with custom commands, tools, themes, and integrations.

### Plugin Management
```bash
# List installed plugins
osvm plugins list

# Install from directory
osvm plugins install ./my-plugin/

# Install from GitHub
osvm plugins install github.com/user/osvm-weather-plugin

# Enable/disable plugins
osvm plugins enable weather-plugin
osvm plugins disable old-plugin

# Get plugin info
osvm plugins info weather-plugin
```

### Plugin Types

**Command Plugins** - Add slash commands:
```bash
/echo Hello World      # Built-in example
/weather Boston        # Custom weather plugin
/deploy contracts/     # Custom deployment plugin
```

**Tool Plugins** - Provide AI-accessible tools:
```bash
# Tools become available to AI automatically
osvm chat "Get the weather in Boston"
```

**Theme Plugins** - Visual customization:
```bash
osvm chat --theme cyberpunk
osvm chat --theme minimal-dark
```

**Integration Plugins** - External services:
```bash
/github create-issue "Fix deployment bug"
/db query "SELECT * FROM transactions LIMIT 10"
```

For detailed plugin development instructions, see [Plugin Development](#️-plugin-development).

## 📊 Dashboard

Launch the interactive monitoring dashboard:

```bash
osvm svm dashboard
```

**Features:**
- Real-time SVM status monitoring
- Network performance metrics
- Node resource usage tracking
- Integrated log viewing
- Multi-tab interface

**Navigation:**
- `Tab`/`Shift+Tab`: Switch tabs
- `↑`/`↓`: Navigate lists
- `Enter`: Select/activate
- `q`: Quit dashboard

## 🔍 System Health

### Health Checks
```bash
# Run comprehensive diagnostics
osvm doctor

# Auto-fix issues
osvm doctor --fix

# Check specific components
osvm doctor --system-only
osvm doctor --network-only
```

### Self-Repair System
The built-in self-repair system automatically detects and fixes common issues:

- **System Dependencies** - Missing build tools, outdated packages
- **User Configuration** - Invalid configs, missing keypairs
- **Network Issues** - RPC endpoint failures, connectivity problems
- **Permission Problems** - File access, system tuning requirements

## ⚙️ CI/CD Integration

Deploy SVM nodes directly from GitHub workflows:

```yaml
- name: Deploy SVM Node
  uses: ./.github/actions/svm-deploy
  with:
    svm-name: 'my-svm'
    host: 'user@example.com'
    ssh-private-key: ${{ secrets.SSH_PRIVATE_KEY }}
    network: 'devnet'
    node-type: 'validator'
```

**Features:**
- Secure deployment using GitHub Secrets
- Multi-network and node type support
- Detailed deployment logging
- Reusable workflow components

[View Complete CI/CD Documentation](.github/actions/svm-deploy/README.md)

## 🛠️ Plugin Development

### Creating Your First Plugin

**1. Setup Plugin Directory:**
```bash
mkdir ~/.osvm/plugins/weather-plugin
cd ~/.osvm/plugins/weather-plugin
```

**2. Create Plugin Manifest (`plugin.json`):**
```json
{
  "name": "weather-plugin",
  "version": "1.0.0",
  "description": "Get weather information for any city",
  "author": "Your Name",
  "license": "MIT",
  "plugin_type": "Command",
  "entry_point": "weather.py",
  "dependencies": ["requests"],
  "permissions": [
    {"NetworkAccess": ["api.openweathermap.org"]},
    "EnvironmentAccess"
  ],
  "min_osvm_version": "0.8.0",
  "supported_platforms": ["linux", "macos", "windows"]
}
```

**3. Create Plugin Script (`weather.py`):**
```python
#!/usr/bin/env python3
import json
import sys
import requests
from datetime import datetime

def main():
    # Read context from stdin
    context = json.loads(sys.stdin.read())

    # Parse command
    user_input = context["user_input"]
    city = user_input.replace("/weather", "").strip()

    if not city:
        result = {
            "success": False,
            "error": "Please specify a city: /weather Boston"
        }
        print(json.dumps(result))
        return

    # Get weather data (simplified example)
    try:
        # Your weather API logic here
        result = {
            "success": True,
            "output": f"🌤️ Weather in {city}: 22°C, Partly Cloudy",
            "suggestions": [f"/weather forecast {city}"]
        }
    except Exception as e:
        result = {
            "success": False,
            "error": f"Failed to get weather: {str(e)}"
        }

    print(json.dumps(result))

if __name__ == "__main__":
    main()
```

**4. Make Executable and Test:**
```bash
chmod +x weather.py
osvm plugins install .
osvm plugins enable weather-plugin

# Test in chat
osvm chat
# Type: /weather Boston
```

### Advanced Plugin Development

<details>
<summary>Rust Tool Plugin Example</summary>

**Cargo.toml:**
```toml
[package]
name = "osvm-database-plugin"
version = "1.0.0"
edition = "2021"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
tokio = { version = "1.0", features = ["full"] }
anyhow = "1.0"
```

**src/main.rs:**
```rust
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize)]
struct PluginContext {
    user_input: String,
    session_id: String,
    config: HashMap<String, serde_json::Value>,
}

#[derive(Serialize)]
struct PluginResult {
    success: bool,
    output: String,
    error: Option<String>,
    suggestions: Vec<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    let context: PluginContext = serde_json::from_str(&input)?;

    // Your plugin logic here
    let result = PluginResult {
        success: true,
        output: "Database query executed successfully".to_string(),
        error: None,
        suggestions: vec![]
    };

    println!("{}", serde_json::to_string(&result)?);
    Ok(())
}
```
</details>

<details>
<summary>Theme Plugin Example</summary>

**theme.json:**
```json
{
  "name": "cyberpunk-theme",
  "version": "1.0.0",
  "plugin_type": "Theme",
  "colors": {
    "primary": "#00ff41",
    "secondary": "#ff00ff",
    "background": "#0d0208",
    "text": "#00ff41",
    "accent": "#00ffff"
  },
  "styles": {
    "chat_input": {
      "background": "background",
      "text": "primary",
      "border": "accent"
    },
    "message_user": {
      "text": "primary",
      "prefix": "► "
    },
    "message_assistant": {
      "text": "text",
      "prefix": "◉ "
    }
  }
}
```
</details>

### Plugin Security

**Permission System:**
- `ReadFiles(["~/data"])` - File read access
- `WriteFiles(["~/output"])` - File write access
- `ExecuteCommands` - System command execution
- `NetworkAccess(["api.example.com"])` - Network access
- `EnvironmentAccess` - Environment variables
- `MCPAccess` - MCP server access
- `AIAccess` - AI service access

**Best Practices:**
- Request minimal permissions
- Validate all user input
- Use secure connections
- Handle errors gracefully
- Store secrets in config, not code

## 📚 Documentation

### Core Guides
- [Complete Documentation Index](docs/README.md)
- [AI Integration Guide](docs/ai-endpoint-configuration.md)
- [RPC Manager Guide](docs/rpc-manager.md)
- [Examples & Patterns](docs/examples.md)

### Advanced Features
- [Agent Chat Interface](docs/agent-chat.md)
- [Security & Auditing](docs/security-audit.md)
- [Plugin API Reference](docs/plugin-api.md)
- [MCP Tool Reference](docs/mcp-tools.md)

### Development Resources
- [GitHub Actions Integration](.github/actions/svm-deploy/README.md)
- [Troubleshooting Guide](docs/troubleshooting.md)
- [CLAUDE.md Development Guide](CLAUDE.md)

For complete documentation, visit [docs.opensvm.org](https://docs.opensvm.org).

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Setup

**Install Pre-commit Hooks:**
```bash
./install-pre-commit-hook.sh
```

**Development Commands:**
```bash
# Format code
cargo fmt --all

# Run linting
cargo clippy --all-targets --all-features

# Run tests
cargo test

# Build and test
make dev
```

**Code Quality Checks:**
- `cargo fmt --all -- --check` - Formatting verification
- `cargo clippy` - Linting checks
- `cargo test` - Test suite


## Monetization Strategy & Business Model

### $OVSM Token Economy
**Tiered Token Gating System:**
- **Platinum Tier (1M+ tokens):** 1 $OVSM per AI prompt
- **Gold Tier (100k+ tokens):** 10 $OVSM per AI prompt
- **Silver Tier (<100k tokens):** 100 $OVSM per AI prompt
- **Guest Users:** 10 prompts / day free, then 200 $OVSM per AI prompt

**Premium Feature Gates:**
- **Social Features:** Minimum 100,000 $OVSM token holding required
- **Advanced Analytics:** Token consumption for complex queries
- **Unlimited Threads:** Premium tier exclusive (free users limited to 10)
- **Extended AI Reasoning:** 5 expansions for free, unlimited for premium


## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

**Need Help?**
- 📖 [Documentation](docs/README.md)
- 💬 [GitHub Discussions](https://github.com/opensvm/osvm-cli/discussions)
- 🐛 [Report Issues](https://github.com/opensvm/osvm-cli/issues)
- 🌐 [Official Website](https://opensvm.org)