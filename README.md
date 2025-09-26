# OSVM CLI

[![License: MIT](https://img.shields.io/badge/License-MIT-gray.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/Rust-1.80.0+-gray.svg)](https://www.rust-lang.org/)
[![Solana](https://img.shields.io/badge/Solana-1.14.29+-gray.svg)](https://solana.com/)

A command-line interface for managing Solana Virtual Machines (SVMs) across networks with AI and MCP server integration. Deploy, monitor, and manage SVM infrastructure with automated AI assistance and blockchain data services.

## Installation

### Quick Install

```bash
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh
```

### Alternative Methods

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

## Core Features

- **SVM Management** - List, inspect, and manage Solana Virtual Machines
- **Node Deployment** - Deploy validator and RPC nodes with automated configuration
- **AI Integration** - Built-in AI assistance for security analysis and blockchain queries
- **MCP Server Support** - Connect to Model Context Protocol servers for enhanced data access
- **Interactive Dashboard** - Real-time monitoring with terminal-based interface
- **Remote Management** - SSH deployment and management capabilities
- **Performance Monitoring** - Track TPS, latency, and system requirements
- **Automated Maintenance** - Self-repair systems and log monitoring
- **Multi-Network Support** - Configure for mainnet, testnet, or devnet
- **Command Examples** - Built-in help and workflow examples

## Basic Usage

```bash
# List all SVMs
osvm svm list

# Get SVM information  
osvm svm get sonic

# Deploy node to remote server
osvm user@host --svm sonic --node-type validator --network mainnet

# Launch interactive dashboard
osvm svm dashboard

# Get help and examples
osvm examples
```



## CI/CD Integration

Deploy SVM nodes directly from GitHub workflows with minimal configuration.

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
- Configurable for different networks and node types  
- Detailed deployment logging
- Reusable workflows for common patterns

[View Complete Documentation](.github/actions/svm-deploy/README.md)

## Command Reference

### SVM Management

```bash
# List all SVMs installed in the chain
osvm svm list

# Get detailed information about a specific SVM
osvm svm get sonic

# Launch interactive dashboard
osvm svm dashboard
```

### Command Examples and Help

```bash
# Show examples for all command categories
osvm examples

# Show examples for a specific category
osvm examples --category svm

# Available categories: basic, svm, node, monitoring, workflow

# List all available example categories
osvm examples --list-categories
```

### Node Deployment

```bash
# Deploy a validator node to a remote server (mainnet)
osvm user@host --svm sonic --node-type validator --network mainnet

# Deploy an RPC node to a remote server (testnet)
osvm user@host --svm sonic --node-type rpc --network testnet

# Deploy a Sonic RPC node to a remote server
osvm rpc sonic root@host.example.com --network mainnet

# Deploy multiple SVMs to a single server
osvm user@host --svm sonic,solana,eclipse,soon --node-type validator --network devnet

# Deploy an eBPF binary to all available SVM networks
osvm deploy ./path/to/ebpf.so --program-id ./path/to/program-address.json --owner ./path/to/program-owner.json --fee ./path/to/deployment-fee-payer.json --publish-idl
```

### RPC Node Deployment

```bash
# Deploy a Sonic RPC node to a remote server (mainnet)
osvm rpc sonic user@host --network mainnet

# Deploy a Sonic RPC node to a remote server (testnet)
osvm rpc sonic user@host --network testnet

# Deploy a Sonic RPC node to a remote server (devnet)
osvm rpc sonic user@host --network devnet

# Monitor devnet RPC node logs (automatically finds the most recent log file)
osvm rpc-manager devnet --logs

# Show specific number of recent log lines
osvm rpc-manager devnet --logs --lines 100

# Follow logs in real-time (similar to tail -f)
osvm rpc-manager devnet --logs --follow

# Start a local test validator for development (guaranteed to work)
osvm rpc-manager test --background --reset

# Check test validator status
osvm rpc-manager test --status

# Stop test validator
osvm rpc-manager test --stop
```

The `rpc-manager` command provides comprehensive RPC node management:

- **Test Validator**: Local development validator with guaranteed RPC functionality
  - Instant startup with no external dependencies
  - Built-in faucet for SOL airdrops
  - Perfect for local development and testing
  - Full Solana RPC API support
- **Devnet RPC**: Real devnet validator that syncs with live blockchain
  - Auto-repair system with real-time log monitoring
  - System parameter tuning (network limits, file descriptors, etc.)
  - External reachability fixes using ngrok tunnels
  - Port conflict resolution and memory optimization
- **Sonic RPC**: Deploys Sonic RPC nodes using Docker containers
- **Network Selection**: Choose between mainnet, testnet, or devnet environments
- **Automatic Configuration**: Handles all dependencies and configuration automatically
- **Comprehensive Logging**: Structured JSON logging to `osvm.log` for debugging

### eBPF Program Deployment

```bash
# Deploy an eBPF binary to all available SVM networks
osvm deploy ./path/to/ebpf.so --program-id ./path/to/program-address.json --owner ./path/to/program-owner.json --fee ./path/to/deployment-fee-payer.json --publish-idl

# Deploy with custom Anchor IDL file
osvm deploy ./path/to/ebpf.so --program-id ./path/to/program-address.json --owner ./path/to/program-owner.json --fee ./path/to/deployment-fee-payer.json --publish-idl --idl-file ./path/to/program.json

# Deploy to a specific network only
osvm deploy ./path/to/ebpf.so --program-id ./path/to/program-address.json --owner ./path/to/program-owner.json --fee ./path/to/deployment-fee-payer.json --network mainnet
```

The `deploy` command provides a streamlined way to deploy eBPF programs:

- **Multi-network Deployment**: Deploy to all SVM networks with one command
- **Network Selection**: Choose between mainnet, testnet, devnet, or all networks
- **IDL Publishing**: Option to publish the IDL along with the program
  - Auto-generated basic IDL (default)
  - Custom Anchor IDL JSON file support via `--idl-file` option
- **Required Files**:
  - eBPF binary (.so file)
  - **Program ID file**: 
    - For **new deployments**: Must be a keypair JSON file (contains private key)
    - For **upgrades**: Can be either a keypair file or pubkey-only JSON file
  - **Program owner keypair**: JSON file containing private key (required for all deployments)
  - **Fee payer keypair**: JSON file containing private key (pays for deployment transaction)

### File Format Requirements

**Keypair files** (generated with `solana-keygen new`):
```json
[123,45,67,89,...,234]  // Array of 64 bytes containing private key
```

**Pubkey-only files** (for upgrades only):
```json
{"programId": "HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS"}
```
or plain string:
```
HN4tEEGheziD9dqcWg4xZd29htcerjXKGoGiQXM5hxiS
```

## AI Integration

Built-in AI capabilities for blockchain analysis and assistance:

```bash
# Ask AI questions about Solana security
osvm "What are the best practices for Solana smart contract security?"

# Use custom AI models (OpenAI, Ollama, LocalAI, etc.)
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-openai-api-key"
osvm "Help me optimize my validator performance"

# Local AI models for privacy
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama-key"
osvm "Analyze this smart contract for vulnerabilities"
```

**AI Features:**
- Smart contract analysis and security auditing
- Automated code review of Solana programs
- Best practices guidance for deployment
- Privacy options with local models (Ollama, LocalAI)
- Multi-provider support (OpenAI, Anthropic, OpenAI-compatible APIs)

## MCP Server Integration

Connect to Model Context Protocol servers for enhanced blockchain data access:

```bash
# Quick setup with Solana MCP server
osvm mcp setup --auto-enable

# Add MCP server manually
osvm mcp add solana-server \
  --server-url https://api.solana-mcp.com \
  --auth-type bearer \
  --auth-token your-token \
  --enabled

# Add MCP server from GitHub repository
osvm mcp add-github solana-mcp https://github.com/openSVM/solana-mcp-server --enabled

# Discover available tools
osvm mcp tools solana-server

# Execute blockchain queries
osvm mcp call solana-server getBalance --args '{"pubkey":"11111111111111111111111111111112"}'
```

**MCP Features:**
- Multi-transport support (HTTP, WebSocket, stdio)
- Authentication with bearer tokens, API keys, basic auth
- Circuit breaker for automatic failure recovery
- Tool discovery for blockchain operations
- GitHub integration for server deployment
- AI-ready integration for enhanced analysis

## Dashboard

The interactive dashboard provides real-time monitoring:

```bash
osvm svm dashboard
```

**Features:**
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

## Documentation

- [Complete Documentation Index](docs/README.md) - Start here for all documentation
- [AI Integration Guide](docs/ai-endpoint-configuration.md) - AI-powered analysis setup
- [MCP Integration Guide](docs/mcp-integration.md) - Model Context Protocol server setup
- [RPC Manager Guide](docs/rpc-manager.md) - RPC node management
- [GitHub Actions Integration](.github/actions/svm-deploy/README.md) - CI/CD integration guide
- [Examples & Patterns](docs/examples.md) - Usage examples and workflows

For complete documentation, visit [our official documentation](https://docs.opensvm.org).

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Setup

To ensure code quality and consistency, this project uses pre-commit hooks that enforce code formatting and linting.

#### Installing Pre-commit Hooks

Run the following command from the project root to install the pre-commit hooks:

```bash
./install-pre-commit-hook.sh
```

This will install a git hook that automatically runs:
- `cargo fmt --all -- --check` - Ensures code is properly formatted
- `cargo clippy` - Runs linting checks

#### Manual Code Quality Checks

You can also run these checks manually:

```bash
# Format your code
cargo fmt --all

# Check formatting without modifying files
cargo fmt --all -- --check

# Run clippy linting
cargo clippy --all-targets --all-features
```

#### Skipping Pre-commit Hooks

If you need to skip the pre-commit hook for a specific commit (not recommended), use:

```bash
git commit --no-verify -m "your message"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
