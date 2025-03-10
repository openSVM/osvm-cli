# OSVM CLI 🚀

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/Rust-1.80.0+-orange.svg)](https://www.rust-lang.org/)
[![Solana](https://img.shields.io/badge/Solana-1.14.29+-purple.svg)](https://solana.com/)

A powerful command-line interface for managing Solana Virtual Machines (SVMs) across various networks. Monitor, deploy, and manage your SVM infrastructure with a single tool.

![OSVM CLI Dashboard](images/dashboard-screenshot.png)

## ⚡ One-Line Installation

### Linux/macOS

```bash
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh
```

### Windows

```bash
powershell -Command "Invoke-WebRequest -Uri 'https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.ps1' -OutFile 'install.ps1'; .\install.ps1"
```

## 🌟 Key Features

- **SVM Management**: List and inspect Solana Virtual Machines
- **Node Deployment**: Deploy validator or dedicated RPC nodes with a single command
- **Interactive Dashboard**: Real-time monitoring with a terminal-based UI
- **Network Configuration**: Configure nodes for mainnet, testnet, or devnet
- **Performance Metrics**: Track TPS, latency, and system requirements
- **Colorized Output**: Enhanced readability with consistent color-coding for status, commands, and data
- **Command Examples**: Built-in examples for common workflows and operations
- **SSH Deployment**: Remote deployment and management capabilities

## 📋 Command Reference

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
osvm user@host --svm sonic,solana,sei --node-type validator --network devnet
```

### RPC Node Deployment

```bash
# Deploy a Sonic RPC node to a remote server (mainnet)
osvm rpc sonic user@host --network mainnet

# Deploy a Sonic RPC node to a remote server (testnet)
osvm rpc sonic user@host --network testnet

# Deploy a Sonic RPC node to a remote server (devnet)
osvm rpc sonic user@host --network devnet
```

The `rpc` command provides a streamlined way to deploy specific RPC nodes:

- **Sonic RPC**: Deploys a Sonic RPC node using Docker containers from the official repository
- **Network Selection**: Choose between mainnet, testnet, or devnet environments
- **Automatic Configuration**: Handles all dependencies and configuration automatically

## 🔧 Detailed Installation

### Prerequisites

- Rust 1.80.0 or later
- Solana CLI tools 1.14.29 or later

### From Source

```bash
# Clone the repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Build the project
cargo build --release

# Install the binary
sudo cp target/release/osvm /usr/local/bin/
```

### From Package Managers

#### Cargo (Rust's Package Manager)

```bash
cargo install osvm-cli
```

## 📊 Dashboard Features

The interactive dashboard provides real-time monitoring of your SVM infrastructure, including:

- Overview of all installed SVMs with status indicators
- Network details for each SVM (mainnet, testnet, devnet)
- Performance metrics with real-time visualization
- Node status monitoring with resource usage
- Aggregated logs from all nodes

Launch the dashboard with:

```bash
osvm svm dashboard
```

### Keyboard Controls

- `Tab`, `Right Arrow`, `Left Arrow`: Switch between tabs
- `Up Arrow`, `Down Arrow`: Navigate through items
- `n`: Select next SVM
- `v`: Toggle verbosity level (affects displayed information detail)
- `p`: Select previous SVM
- `h`: Toggle help overlay
- `q` or `Ctrl+C`: Quit the dashboard

## 📚 Documentation

For complete documentation, visit [our official documentation](https://docs.opensvm.org).

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.