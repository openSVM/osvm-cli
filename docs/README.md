# OSVM CLI Documentation

Welcome to the comprehensive documentation for OSVM CLI - the revolutionary Solana Virtual Machine command-line interface with **hardware-isolated microVM/unikernel architecture**, integrated AI assistance, and MCP server support.

## 🚀 Quick Access

- [**🏗️ REVOLUTIONARY ARCHITECTURE**](../Architecture.md) - **Must-read 2,150-line deep dive into microVM/unikernel security**
- [**📖 Main Documentation Website**](index.html) - Interactive terminal-style documentation
- [**⚡ Getting Started**](#quick-start) - Begin your OSVM journey
- [**📚 API Reference**](#api-reference) - Complete command reference
- [**🧩 Plugin Development**](#plugin-development) - Extend OSVM functionality

## 📋 Documentation Index

### 🏗️ Revolutionary Architecture (START HERE!)

**[📖 Architecture.md - Complete Guide](../Architecture.md)** - Comprehensive 2,150-line architectural deep dive

**Essential Reading:**
- [**The Problem Space**](../Architecture.md#the-problem-space) - Why traditional approaches fail (containers, VMs, sandboxing)
- [**What is a Unikernel?**](../Architecture.md#21-what-is-a-unikernel) - From 30MB OS to 50KB (99.83% reduction)
- [**What is a MicroVM?**](../Architecture.md#22-what-is-a-microvm) - 125ms boot vs 30-60s traditional VMs
- [**Hardware Security**](../Architecture.md#23-hardware-based-security-features) - VT-x, AMD-V, SEV, SGX, TPM explained
- [**Zero-Trust Networking**](../Architecture.md#24-zero-trust-networking) - mTLS and capability-based security
- [**Attack Surface Analysis**](../Architecture.md#25-the-attack-surface-concept) - Quantifying 99.9% reduction
- [**The OSVM Innovation**](../Architecture.md#the-osvm-innovation) - How we combine it all
- [**Security Model**](../Architecture.md#security-model) - Formal guarantees, threat analysis, defense-in-depth
- [**Performance Characteristics**](../Architecture.md#performance-characteristics) - Detailed benchmarks
- [**Use Cases**](../Architecture.md#use-cases-and-applications) - Validators, RPC nodes, MCP servers, compliance

**Implementation Status:**
- [**Implementation Complete**](../IMPLEMENTATION_COMPLETE.md) - All Phases 1-3 complete
- [**Design Document**](../Design-Doc.md) - Technical implementation details
- [**Phase 2 Features**](../PHASE2_COMPLETE.md) - Hot-swap, vsock, Firecracker MicroVMs

**Key Metrics:**
- ✅ 99.83% attack surface reduction (50KB vs 30MB)
- ✅ Zero-downtime updates with automatic rollback
- ✅ <1ms inter-VM communication (vsock)
- ✅ 125ms boot time for MicroVMs (240-600x faster)
- ✅ Hardware-enforced isolation (KVM, SEV, SGX)

### 🏗️ Core Features
- [**SVM Management**](svm-management.md) - Deploy and manage Solana Virtual Machines
- [**Node Management**](node-management.md) - Validator and RPC node operations
- [**SSH Deployment**](ssh-deployment.md) - Remote deployment capabilities
- [**RPC Manager**](rpc.md) - Comprehensive RPC node management

### 🤖 AI Integration
- [**AI Endpoint Configuration**](ai-endpoint-configuration.md) - Configure AI providers and endpoints
- [**DeepLogic AI Analysis**](deeplogic-ai-analysis.md) - Advanced vulnerability detection
- [**Security Audit**](security-audit.md) - Automated security analysis and reporting

### 🧠 OVSM Language (NEW!)
- [**OVSM Overview**](ovsm/OVSM_README.md) - Introduction to OVSM planning language
- [**Core Specification**](ovsm/ovsm-spec.md) - Complete OVSM language reference (207 tools)
- [**Agent Extensions**](ovsm/ovsm-agents.md) - Multi-agent research features (15 advanced capabilities)
- [**Tool Index**](ovsm/COMPLETE_TOOL_INDEX.md) - Comprehensive tool reference
- [**Planning Guide**](ovsm/PLANNING_FORMAT.md) - How to write OVSM research plans

**What is OVSM?**
OVSM (Open Versatile Seeker Mind) is a domain-specific language for expressing AI agent research plans with conditional branching, parallel execution, and adaptive learning. It enables sophisticated multi-agent workflows for blockchain investigation, statistical analysis, and knowledge discovery.

### 🔌 MCP Integration
- [**MCP Integration Guide**](mcp-integration.md) - Model Context Protocol server setup
- [**GitHub MCP Servers**](mcp-integration.md#github-integration) - Repository-based server deployment
- [**Tool Discovery**](mcp-integration.md#tool-discovery) - Blockchain tool discovery and execution

### 🧩 Plugin System
- [**Plugin Development Guide**](#plugin-development) - Create custom plugins
- [**Plugin API Reference**](#plugin-api) - Complete API documentation
- [**Plugin Examples**](#plugin-examples) - Working plugin examples
- [**Plugin Security**](#plugin-security) - Security model and permissions

### ⚙️ Advanced Features
- [**Self-Repair System**](self-repair-system.md) - Automatic error detection and repair
- [**Log Monitoring**](log-monitoring.md) - Real-time log analysis and alerts
- [**Validator Enhancements**](validator-enhancements.md) - Hardware optimization
- [**eBPF Deployment**](ebpf-deployment.md) - Solana program deployment

### 🛠️ Tools & Interfaces
- [**Interactive Dashboard**](dashboard.md) - Real-time monitoring interface
- [**Agent Chat System**](#agent-chat) - AI-powered command interface
- [**Command Examples**](examples.md) - Usage patterns and workflows

### 📦 Setup & Configuration
- [**Installation Guide**](installation.md) - Complete installation instructions
- [**Configuration Management**](configuration.md) - System and user configuration
- [**Testing Guide**](testing.md) - Testing and validation procedures

## 🚀 Quick Start

### 1. Installation
```bash
# One-line install
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh

# Verify installation
osvm --version
```

### 2. Basic Configuration
```bash
# Check system health
osvm doctor

# Auto-fix common issues
osvm doctor --fix

# List available SVMs
osvm svm list
```

### 3. First Deployment
```bash
# Deploy to testnet for testing
osvm user@host --svm sonic --node-type validator --network testnet

# Monitor with dashboard
osvm svm dashboard
```

### 4. AI Integration
```bash
# Configure AI provider
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="your-api-key"

# Ask AI questions
osvm "How do I optimize my validator performance?"

# Run security audit
osvm audit ./contracts
```

### 5. MCP Server Setup
```bash
# Quick setup with Solana MCP server
osvm mcp setup --auto-enable

# Test connectivity
osvm mcp test solana-server

# Execute blockchain queries
osvm mcp call solana-server getBalance --args '{"pubkey":"your-address"}'
```

## 🏗️ Architecture Overview

OSVM CLI is built with a modular architecture consisting of:

### Revolutionary Isolation Layer (NEW!)
- **Firecracker MicroVMs** - ~125ms boot time, hardware isolation
- **HermitCore Unikernels** - ~50-100ms boot, 99.83% attack surface reduction
- **vsock Communication** - <1ms latency for VM-to-VM communication
- **Hot-Swap Manager** - Zero-downtime updates with automatic rollback
- **TEE Support** - Intel SGX/AMD SEV for hardware-protected keys
- **Auto-Scaler** - Intelligent metric-based capacity management
- **Orchestrator** - Central control plane for component lifecycle

### Core Components
- **Command Router** - Central command processing and routing
- **Service Layer** - High-level service implementations (AI, MCP, Audit)
- **Utility Layer** - Core functionality modules
- **Plugin System** - Extensible plugin architecture

### Key Services
- **AI Service** - Multi-provider AI integration with circuit breaker
- **MCP Service** - Model Context Protocol server management
- **Audit Service** - Security analysis and vulnerability detection
- **SSH Deploy Service** - Remote deployment and management

### Plugin Architecture
- **Command Plugins** - Custom slash commands
- **Tool Plugins** - MCP-style tools for AI integration
- **Theme Plugins** - Visual customization
- **Integration Plugins** - External service connections

## 📖 API Reference

### Core Commands

**SVM Management:**
```bash
osvm svm list                    # List all SVMs
osvm svm get <name>             # Get SVM details
osvm svm dashboard              # Launch monitoring dashboard
```

**Node Operations:**
```bash
osvm nodes list                 # List deployed nodes
osvm nodes status <id>          # Check node status
osvm nodes logs <id> [--follow] # View node logs
osvm nodes restart <id>         # Restart node
```

**AI Integration:**
```bash
osvm "query"                    # Natural language query
osvm chat                       # Interactive AI chat
osvm audit [path]               # Security audit
```

**MCP Operations:**
```bash
osvm mcp list                   # List configured servers
osvm mcp tools [server]         # List available tools
osvm mcp call <server> <tool>   # Execute tool
```

**Plugin Management:**
```bash
osvm plugins list               # List installed plugins
osvm plugins install <path>     # Install plugin
osvm plugins enable <name>      # Enable plugin
```

## 🧩 Plugin Development

### Creating a Command Plugin

**1. Plugin Structure:**
```
my-plugin/
├── plugin.json          # Plugin manifest
├── main.py             # Entry point
├── requirements.txt    # Dependencies
└── README.md          # Documentation
```

**2. Plugin Manifest (`plugin.json`):**
```json
{
  "name": "my-plugin",
  "version": "1.0.0",
  "description": "Description of plugin functionality",
  "author": "Your Name",
  "license": "MIT",
  "plugin_type": "Command",
  "entry_point": "main.py",
  "dependencies": ["requests"],
  "permissions": [
    {"NetworkAccess": ["api.example.com"]},
    "EnvironmentAccess"
  ],
  "min_osvm_version": "0.8.0",
  "supported_platforms": ["linux", "macos", "windows"]
}
```

**3. Plugin Implementation (`main.py`):**
```python
#!/usr/bin/env python3
import json
import sys

def main():
    # Read context from stdin
    context = json.loads(sys.stdin.read())

    # Parse user input
    user_input = context["user_input"]
    command = user_input.replace("/mycommand", "").strip()

    # Execute plugin logic
    try:
        result = {
            "success": True,
            "output": f"Plugin executed with: {command}",
            "suggestions": ["/mycommand help"]
        }
    except Exception as e:
        result = {
            "success": False,
            "error": str(e)
        }

    # Return result as JSON
    print(json.dumps(result))

if __name__ == "__main__":
    main()
```

### Plugin Types

**Command Plugins:** Add new slash commands to the chat interface
- Entry point: Executable script (Python, Rust, Node.js, etc.)
- Input: JSON context via stdin
- Output: JSON result via stdout

**Tool Plugins:** Provide MCP-style tools for AI integration
- Automatically available to AI queries
- Support complex parameter schemas
- Can chain with other tools

**Theme Plugins:** Visual customization
- JSON-based color and style definitions
- Support for animations and transitions
- Responsive design compatibility

**Integration Plugins:** External service connections
- Database connections
- API integrations
- Cloud service providers
- Version control systems

### Plugin Security

**Permission System:**
```json
{
  "permissions": [
    {"ReadFiles": ["~/data", "/tmp"]},
    {"WriteFiles": ["~/output"]},
    {"NetworkAccess": ["api.example.com", "*.trusted-domain.com"]},
    "ExecuteCommands",
    "EnvironmentAccess",
    "MCPAccess",
    "AIAccess"
  ]
}
```

**Security Best Practices:**
- Request minimal permissions needed
- Validate all user input
- Use secure communication (HTTPS/TLS)
- Store secrets in plugin configuration
- Implement proper error handling
- Follow principle of least privilege

## 🛠️ Development Resources

### Contributing to OSVM
- [**Development Setup**](../CLAUDE.md#development-environment-setup) - Complete setup guide
- [**Code Style Guide**](../CLAUDE.md#code-style) - Coding standards and conventions
- [**Testing Strategy**](../CLAUDE.md#testing-strategy) - Testing guidelines
- [**Architecture Guide**](../CLAUDE.md#project-structure) - Codebase architecture

### External Resources
- [**Solana Documentation**](https://docs.solana.com/) - Solana blockchain documentation
- [**Model Context Protocol**](https://modelcontextprotocol.io/) - MCP specification
- [**Rust Programming**](https://doc.rust-lang.org/) - Rust language documentation
- [**GitHub Actions**](https://docs.github.com/en/actions) - CI/CD documentation

## 📞 Support & Community

### Getting Help
- **📖 Documentation**: Complete guides and references
- **💬 GitHub Discussions**: Community Q&A and discussions
- **🐛 Issue Tracker**: Bug reports and feature requests
- **📧 Email Support**: Technical support contact

### Community Resources
- **Discord Server**: Real-time community chat
- **Twitter**: Updates and announcements
- **Blog**: Technical articles and tutorials
- **Newsletter**: Monthly updates and releases

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](../LICENSE) file for details.

---

**🔗 Quick Links:**
- [Main Website](index.html) | [GitHub Repository](https://github.com/opensvm/osvm-cli) | [Download Latest](https://github.com/opensvm/osvm-cli/releases/latest)

*Last updated: $(date)*
