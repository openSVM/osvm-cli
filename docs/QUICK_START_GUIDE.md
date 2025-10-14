# OSVM CLI Quick Start Guide

**Get up and running with OSVM CLI in 15 minutes**

This guide will walk you through installing OSVM CLI and running your first commands, from basic setup to deploying your first node.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [First Steps](#first-steps)
4. [Your First Local RPC Node](#your-first-local-rpc-node)
5. [AI-Powered Queries](#ai-powered-queries)
6. [Deploying to Remote Server](#deploying-to-remote-server)
7. [Next Steps](#next-steps)

---

## Prerequisites

Before you begin, ensure you have:

- **Operating System**: Linux (Ubuntu 20.04+ recommended) or macOS
- **Rust**: Version 1.70 or higher
- **Git**: For cloning the repository
- **Solana CLI**: (Optional, but recommended)

### Check Your System

```bash
# Check Rust version
rustc --version
# Expected: rustc 1.70.0 or higher

# Check if Solana CLI is installed
solana --version
# Expected: solana-cli 1.17.0 or higher (optional)

# Check available disk space
df -h
# Recommended: At least 10GB free space
```

---

## Installation

### Method 1: Quick Install Script (Recommended)

```bash
# One-line installation
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh

# Add to PATH (if not already)
echo 'export PATH="$HOME/.osvm/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Method 2: Build from Source

```bash
# Clone the repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Build the project
cargo build --release

# Install the binary
sudo cp target/release/osvm /usr/bin/osvm
# Or install to user directory
cp target/release/osvm ~/.local/bin/osvm
```

### Verify Installation

```bash
# Check OSVM CLI version
osvm --version

# Expected output:
# osvm-cli 0.8.0

# View help
osvm --help

# Expected output shows all available commands:
# OSVM CLI - Revolutionary Blockchain Infrastructure
# 
# USAGE:
#     osvm [OPTIONS] [COMMAND]
# 
# COMMANDS:
#     svm              Manage Solana Virtual Machines
#     nodes            Manage validator and RPC nodes
#     rpc-manager      Manage RPC nodes
#     ...
```

---

## First Steps

### 1. System Health Check

Run a health check to ensure all dependencies are properly configured:

```bash
osvm doctor
```

**Expected output:**
```
🏥 OSVM System Health Check
===========================

✅ Operating System: Linux (Ubuntu 22.04)
✅ Rust toolchain: 1.70.0 (installed)
✅ Cargo: 1.70.0 (installed)
✅ Git: 2.34.1 (installed)
✅ SSH client: OpenSSH_8.9p1 (available)
⚠️  Solana CLI: Not installed (optional)

System Requirements:
✅ CPU: 4 cores available
✅ Memory: 16 GB available
✅ Disk space: 245 GB available

Overall Status: ✅ Healthy
```

If you see any issues, fix them with:

```bash
osvm doctor --auto-repair
```

### 2. Configure Network Settings

Set your default Solana network:

```bash
# Set network to devnet for testing
osvm config set --network devnet

# Or use mainnet
osvm config set --network mainnet

# Verify configuration
osvm config show
```

**Expected output:**
```
Current Configuration:
======================

Network: devnet
RPC URL: https://api.devnet.solana.com
Commitment: confirmed
Keypair: ~/.config/solana/id.json
```

### 3. Generate a Keypair (Optional)

If you don't have a Solana keypair yet:

```bash
# Generate a new keypair
solana-keygen new --outfile ~/.config/solana/id.json

# Or use OSVM's keygen (if available)
osvm keygen --output ~/.config/solana/id.json

# Check your address
solana address
# Or
osvm balance
```

### 4. Check Your Balance

```bash
# Check SOL balance for configured keypair
osvm balance

# Expected output:
# Address: 7xKxY1Z2A3bC4dE5fG6hI7jK8lM9nO0pQ1rS2tU3vW4x
# Balance: 0.000000000 SOL
```

To get devnet SOL:

```bash
# Request airdrop (devnet only)
solana airdrop 2

# Check balance again
osvm balance
# Balance: 2.000000000 SOL
```

---

## Your First Local RPC Node

Let's start a local test validator for development:

### 1. Start Local RPC Node

```bash
osvm rpc-manager local
```

**Expected output:**
```
🚀 Starting Local RPC Node
==========================

✅ Initializing test validator
✅ Configuring faucet
✅ Setting up ledger directory

RPC Endpoint: http://localhost:8899
WebSocket: ws://localhost:8900
Faucet: http://localhost:9900

✅ Local RPC node is running!

Press Ctrl+C to stop the node
```

The node is now running and ready to accept connections!

### 2. Test Your Local Node

Open a **new terminal** and test the RPC endpoint:

```bash
# Check connection
curl -X POST http://localhost:8899 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}'

# Expected output:
# {"jsonrpc":"2.0","result":"ok","id":1}

# Get current slot
curl -X POST http://localhost:8899 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"getSlot"}'
```

### 3. Use OSVM with Local Node

```bash
# Set Solana CLI to use local node
solana config set --url http://localhost:8899

# Request airdrop from local faucet
solana airdrop 100

# Check balance
osvm balance
# Balance: 100.000000000 SOL
```

### 4. Stop the Local Node

Return to the terminal running the RPC node and press `Ctrl+C`:

```bash
^C
Shutting down gracefully...
✅ Local RPC node stopped
```

---

## AI-Powered Queries

OSVM CLI includes AI-powered analysis capabilities for intelligent blockchain insights.

### 1. Configure AI Endpoint

```bash
# For OpenAI (GPT-4)
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-api-key-here"

# Or for local Ollama
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama"

# Add to your shell profile for persistence
echo 'export OPENAI_URL="https://api.openai.com/v1/chat/completions"' >> ~/.bashrc
echo 'export OPENAI_KEY="sk-your-api-key-here"' >> ~/.bashrc
```

### 2. Test AI Connection

```bash
osvm chat --test
```

**Expected output:**
```
🤖 Testing AI Connection
========================

Endpoint: https://api.openai.com/v1/chat/completions
Model: gpt-4

✅ Connection successful
✅ API key valid
✅ Model accessible

AI assistant is ready!
```

### 3. Ask Natural Language Questions

```bash
# Query network information
osvm "What is the current slot height on mainnet?"

# Expected output:
# 🤖 AI Analysis:
# 
# The current slot on Solana mainnet is approximately 245,678,901
# as of [timestamp]. Solana processes approximately 2.5 slots per
# second, so this number increases rapidly.
# 
# Context:
# - Epoch: 524
# - Block time: ~400ms
# - Network health: Excellent
# 
# Confidence: 95%

# Ask about best practices
osvm "What are the best practices for validator security?"

# Get deployment guidance
osvm "How do I deploy a program to mainnet safely?"
```

### 4. Interactive AI Chat

```bash
osvm chat
```

**Interactive session:**
```
🤖 OSVM AI Assistant (type 'exit' to quit)
==========================================

You: What's the difference between devnet and mainnet?

AI: Devnet and mainnet are different Solana network environments:

**Devnet (Development Network):**
- Purpose: Testing and development
- SOL tokens: No real value (free from faucet)
- Data: Periodically reset
- RPC: https://api.devnet.solana.com
- Use for: Testing programs before mainnet deployment

**Mainnet (Main Network):**
- Purpose: Production blockchain
- SOL tokens: Real value (trade on exchanges)
- Data: Permanent
- RPC: https://api.mainnet-beta.solana.com
- Use for: Production deployments

You: How do I deploy a program?

AI: To deploy a Solana program, follow these steps:
[... detailed guidance ...]

You: exit

Goodbye! 👋
```

### 5. Security Audit

Audit your Solana program for vulnerabilities:

```bash
osvm audit ./my-program
```

**Expected output:**
```
🔍 DeepLogic AI Security Audit
==============================
Target: ./my-program

Analyzing code... ✅

🔴 HIGH SEVERITY (2 issues):

[H1] Missing signer validation
     File: src/lib.rs:45
     Code: let user = &ctx.accounts.user;
     
     Issue: The user account is not validated as a signer.
     An attacker could pass any account as the user.
     
     Fix: Add signer validation:
     require!(user.is_signer, ErrorCode::Unauthorized);

[H2] Integer overflow risk
     File: src/lib.rs:78
     Code: balance += amount;
     
     Issue: Arithmetic operation without overflow protection.
     
     Fix: Use checked arithmetic:
     balance = balance.checked_add(amount)?;

🟡 MEDIUM SEVERITY (5 issues):
[... additional findings ...]

📊 Audit Summary:
Total Issues: 12
High: 2 | Medium: 5 | Low: 5
Risk Score: 7.5/10
Confidence: 95%

Recommendations:
1. Fix all HIGH severity issues immediately
2. Review MEDIUM severity issues before mainnet
3. Consider professional audit for mainnet deployment
```

---

## Deploying to Remote Server

Deploy a validator node to a remote server using SSH.

### 1. Prepare Your Server

Ensure your remote server has:
- Ubuntu 20.04+ or similar Linux distribution
- At least 16GB RAM
- At least 500GB disk space
- SSH access configured

### 2. Prepare Keypairs

```bash
# Generate validator identity keypair
solana-keygen new --outfile ~/validator-keypair.json

# Generate vote account keypair
solana-keygen new --outfile ~/vote-keypair.json

# Secure the keypairs
chmod 600 ~/validator-keypair.json ~/vote-keypair.json
```

### 3. Deploy Validator

```bash
osvm deploy-validator \
  --host validator@your-server.com \
  --keypair ~/validator-keypair.json \
  --vote-keypair ~/vote-keypair.json \
  --network testnet
```

**Expected output:**
```
🚀 OSVM Validator Deployment
============================

Target: validator@your-server.com
Network: testnet

📋 Pre-deployment checks:
✅ SSH connection established
✅ Server meets requirements
✅ Keypairs validated

📦 Installing dependencies:
✅ Rust toolchain (1.70.0)
✅ Solana CLI (1.18.0)
✅ System packages

🔧 Configuring system:
✅ System limits optimized
✅ Firewall configured
✅ Storage optimized

🚀 Deploying validator:
✅ Validator binary installed
✅ Configuration created
✅ Service registered
✅ Validator started

📊 Deployment Summary:
Node ID: validator-001
Status: ✅ Running
Identity: 7xKxY1Z2A3bC4dE5fG6hI7jK8lM9nO0pQ1rS2tU3vW4x
Vote Account: BPF1...xyz789
Network: testnet
Uptime: 5s

✅ Validator deployment complete!

Monitor with: osvm node status validator-001
View logs: osvm node logs validator-001 --follow
```

### 4. Monitor Your Validator

```bash
# Check validator status
osvm node status validator-001

# Expected output:
# Validator: validator-001
# Status: ✅ Running
# Network: testnet
# Version: 1.18.0
# Identity: 7xKxY1Z2A3bC4dE5fG6hI7jK8lM9nO0pQ1rS2tU3vW4x
# Vote Account: BPF1...xyz789
# Uptime: 15m 23s
# Slot: 245,678,901
# Health: Excellent

# View real-time logs
osvm node logs validator-001 --follow

# Launch monitoring dashboard
osvm node dashboard
```

---

## Next Steps

Congratulations! You've successfully:

✅ Installed OSVM CLI
✅ Ran health checks
✅ Started a local RPC node
✅ Used AI-powered queries
✅ Deployed a remote validator

### Continue Learning

**Essential Documentation:**
- [Full Examples](examples.md) - Comprehensive command examples
- [SVM Management](svm-management.md) - Managing Solana Virtual Machines
- [Node Management](node-management.md) - Advanced node operations
- [RPC Manager](rpc-manager.md) - RPC node configuration

**Advanced Features:**
- [MCP Integration](mcp-integration.md) - Model Context Protocol servers
- [AI Analysis](deeplogic-ai-analysis.md) - DeepLogic vulnerability detection
- [Security Audit](security-audit.md) - Comprehensive security scanning
- [Snapshot Analysis](snapshot-command-guide.md) - Account snapshot tools

**Production Deployment:**
- [Production Guide](../PRODUCTION_DEPLOYMENT_GUIDE.md) - Production best practices
- [Architecture](../Architecture.md) - System design and theory
- [SSH Deployment](ssh-deployment.md) - Remote deployment guide

### Common Next Steps

```bash
# Explore available examples
osvm examples --list-categories

# List all available commands
osvm --help

# Join the community
# GitHub: https://github.com/opensvm/osvm-cli
# Documentation: https://opensvm.github.io/osvm-cli

# Deploy to production
# See: PRODUCTION_DEPLOYMENT_GUIDE.md
```

---

## Troubleshooting

### Build Fails with Missing Dependencies

```bash
# Install required system packages (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y build-essential pkg-config libssl-dev libudev-dev

# Install required system packages (macOS)
brew install pkg-config openssl
```

### Cannot Connect to RPC Node

```bash
# Check if node is running
curl http://localhost:8899

# Check firewall
sudo ufw status

# Check logs
osvm node logs local-rpc
```

### AI Connection Fails

```bash
# Verify environment variables
echo $OPENAI_URL
echo $OPENAI_KEY

# Test connection
curl -X POST $OPENAI_URL \
  -H "Authorization: Bearer $OPENAI_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model":"gpt-4","messages":[{"role":"user","content":"test"}]}'
```

### SSH Deployment Fails

```bash
# Test SSH connection
ssh validator@your-server.com

# Use verbose mode for debugging
osvm --verbose deploy-validator --host validator@your-server.com ...

# Check server requirements
osvm doctor --remote validator@your-server.com
```

---

## Getting Help

- **Documentation**: [docs/README.md](README.md)
- **Examples**: [docs/examples.md](examples.md)
- **Issues**: [GitHub Issues](https://github.com/opensvm/osvm-cli/issues)
- **Discussions**: [GitHub Discussions](https://github.com/opensvm/osvm-cli/discussions)

---

**Ready to build on Solana with OSVM CLI? Let's go! 🚀**
