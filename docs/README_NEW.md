# OSVM CLI Documentation

Welcome to the comprehensive documentation for OSVM CLI - the revolutionary Solana Virtual Machine command-line interface with **hardware-isolated microVM/unikernel architecture**, **AI-powered code execution**, integrated AI assistance, and MCP server support.

## 🚀 Quick Access

- [**🤖 AI-POWERED CHAT (NEW!)**](#-ai-powered-chat-new) - **Execute code with natural language**
- [**📝 OVSM LISP LANGUAGE**](#-ovsm-language---lisp-syntax-production-ready) - **Blockchain automation DSL**
- [**🏗️ REVOLUTIONARY ARCHITECTURE**](../Architecture.md) - **Must-read 2,150-line deep dive into microVM/unikernel security**
- [**📖 Main Documentation Website**](index.html) - Interactive terminal-style documentation
- [**⚡ Getting Started**](#quick-start) - Begin your OSVM journey
- [**📚 API Reference**](#api-reference) - Complete command reference

## 🆕 What's New in v0.9.2

### 🎉 Major Features

**🤖 AI-Powered Code Execution**
- Chat now automatically extracts and executes OVSM LISP code from AI responses
- No more copy/paste - just review, confirm, and execute!
- Enhanced safety with 30-second timeout and pre-validation

**🛡️ Security Enhancements**
- **30-second timeout** prevents infinite loops
- **Pre-validation** catches syntax errors before execution
- **View full code** option with line numbers
- Security score improved from 5/10 to 9/10 (+80%)

**✨ Better User Experience**
- View complete code before executing (type `v`)
- Clear error messages for AI failures
- Accepts LISP code with comments (improved heuristic)

See [CHANGELOG.md](../CHANGELOG.md) for complete release notes.

---

## 🤖 AI-Powered Chat (NEW!)

### Interactive Code Execution

The chat interface now **automatically detects and executes OVSM code** from AI responses!

**Quick Start:**
```bash
# Launch AI-powered chat
osvm chat

# Try it:
> Calculate the sum of 1 to 100

# AI generates code, you can:
# - View full code before executing (type 'v')
# - Execute with automatic timeout protection
# - See results immediately
```

### Example Session

```
$ osvm chat

┌─────────────────────────────────────────┐
│   OSVM Agent Chat (Enhanced)            │
│   Type /help for commands               │
└─────────────────────────────────────────┘

> Calculate factorial of 5

• Assistant: Here's OVSM LISP code to calculate factorial:

```lisp
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
```

╭─ OVSM Code Block 1 ─
│ (define (factorial n)
│   (if (<= n 1)
│ ...
╰─

Execute? ([y]es/[n]o/[v]iew full): v

Full Code (Block 1):

   1 │ (define (factorial n)
   2 │   (if (<= n 1)
   3 │       1
   4 │       (* n (factorial (- n 1)))))
   5 │
   6 │ (factorial 5)

Execute now? (y/n): y

▶ Executing OVSM code (30s timeout)...
✓ Execution successful!
Result: Number(120)
```

### Features

- ✅ **Auto-detect code blocks** - Finds LISP code in AI responses
- ✅ **Pre-validation** - Checks syntax before asking you
- ✅ **View full code** - See complete code with line numbers
- ✅ **30-second timeout** - Prevents infinite loops
- ✅ **Safe by default** - Requires confirmation before execution

### Chat Commands

```bash
/help         # Show help menu
/clear        # Clear chat history
/tools        # List available MCP tools
/status       # Show system status
/screenshot   # Take terminal screenshot
exit or quit  # Exit chat
```

### Advanced Mode

For power users with multi-session support:

```bash
osvm chat --advanced
```

---

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
- [**CHANGELOG**](../CHANGELOG.md) - Version history and release notes

**Key Metrics:**
- ✅ 99.83% attack surface reduction (50KB vs 30MB)
- ✅ Zero-downtime updates with automatic rollback
- ✅ <1ms inter-VM communication (vsock)
- ✅ 125ms boot time for MicroVMs (240-600x faster)
- ✅ Hardware-enforced isolation (KVM, SEV, SGX)
- ✅ AI-powered code execution with timeout protection (NEW!)

### 🏗️ Core Features
- [**SVM Management**](svm-management.md) - Deploy and manage Solana Virtual Machines
- [**Node Management**](node-management.md) - Validator and RPC node operations
- [**SSH Deployment**](ssh-deployment.md) - Remote deployment capabilities
- [**RPC Manager**](rpc-manager.md) - Comprehensive RPC node management

### 🤖 AI Integration
- [**AI Chat Documentation**](#-ai-powered-chat-new) - Interactive code execution (NEW!)
- [**AI Endpoint Configuration**](ai-endpoint-configuration.md) - Configure AI providers and endpoints
- [**DeepLogic AI Analysis**](deeplogic-ai-analysis.md) - Advanced vulnerability detection
- [**Security Audit**](security-audit.md) - Automated security analysis and reporting

### 🧠 OVSM Language - LISP Syntax (Production Ready!)
- [**OVSM LISP Specification**](../OVSM_LISP_SYNTAX_SPEC.md) - **Complete LISP syntax reference (ONLY supported syntax)**
- [**Implementation Report**](../FINAL_LISP_IMPLEMENTATION_REPORT.md) - Production-ready implementation details
- [**Usage Guide**](../crates/ovsm/USAGE_GUIDE.md) - How to write OVSM scripts
- [**Example Scripts**](../examples/ovsm_scripts/) - Working LISP syntax examples
- [**Legacy Docs**](ovsm/OVSM_README.md) - Historical reference (may reference deprecated Python syntax)

**What is OVSM?**
OVSM is a LISP-based domain-specific language for blockchain scripting with Solana RPC integration. Using S-expression syntax, it provides zero-ambiguity parsing, conditional branching, loops, and native blockchain operations. **Python-style syntax removed October 2025** - LISP is now the only supported syntax with 100% test coverage and zero parser bugs.

**Quick Example:**
```lisp
;; Calculate sum with logging
(define total 0)
(for (i (range 1 11))
  (set! total (+ total i)))
(log :message "Sum 1-10:" :value total)
total  ;; Returns 55
```

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
- [**Agent Chat System**](#-ai-powered-chat-new) - AI-powered command interface with code execution (NEW!)
- [**Command Examples**](examples.md) - Usage patterns and workflows

### 📦 Setup & Configuration
- [**Installation Guide**](installation.md) - Complete installation instructions
- [**Configuration Management**](configuration.md) - System and user configuration
- [**Testing Guide**](testing.md) - Testing and validation procedures

---

## 🚀 Quick Start

### 1. Installation
```bash
# One-line install
curl -sSf https://raw.githubusercontent.com/opensvm/osvm-cli/main/install.sh | sh

# Or build from source
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli
cargo build --release
sudo cp target/release/osvm /usr/bin/osvm

# Verify installation
osvm --version  # Should show v0.9.2
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

### 3. Try the AI Chat (NEW!)
```bash
# Start interactive AI chat
osvm chat

# Ask natural language questions
> Calculate the sum of 1 to 100

# AI generates code - you can:
# - View full code (type 'v')
# - Execute (type 'y')
# - Cancel (type 'n')
```

### 4. Execute OVSM Code
```bash
# Execute inline OVSM code
osvm ovsm eval '(+ 1 2 3 4 5)'

# Run OVSM script file
osvm ovsm run script.ovsm

# Start interactive REPL
osvm ovsm repl

# See examples
osvm ovsm examples
```

### 5. First Deployment
```bash
# Deploy to testnet for testing
osvm user@host --svm sonic --node-type validator --network testnet

# Monitor with dashboard
osvm svm dashboard
```

### 6. AI Integration
```bash
# Configure AI provider
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="your-api-key"

# Ask AI questions
osvm "How do I optimize my validator performance?"

# Run security audit
osvm audit ./contracts
```

### 7. MCP Server Setup
```bash
# Quick setup with Solana MCP server
osvm mcp setup --auto-enable

# Test connectivity
osvm mcp test solana-server

# Execute blockchain queries
osvm mcp call solana-server getBalance --args '{"pubkey":"your-address"}'
```

---

## 🏗️ Architecture Overview

OSVM CLI is built with a modular architecture consisting of:

### Revolutionary Isolation Layer
- **Firecracker MicroVMs** - ~125ms boot time, hardware isolation
- **HermitCore Unikernels** - ~50-100ms boot, 99.83% attack surface reduction
- **vsock Communication** - <1ms latency for VM-to-VM communication
- **Hot-Swap Manager** - Zero-downtime updates with automatic rollback
- **TEE Support** - Intel SGX/AMD SEV for hardware-protected keys
- **Auto-Scaler** - Intelligent metric-based capacity management
- **Orchestrator** - Central control plane for component lifecycle

### AI & Automation Layer (NEW!)
- **AI Chat Enhancement** - Code extraction and execution from natural language
- **OVSM Interpreter** - Production-ready LISP interpreter (100% test coverage)
- **Code Validation** - Pre-execution syntax checking
- **Timeout Protection** - 30-second execution timeout
- **Safety Controls** - User confirmation and full code transparency

### Core Components
- **Command Router** - Central command processing and routing
- **Service Layer** - High-level service implementations (AI, MCP, Audit, OVSM)
- **Utility Layer** - Core functionality modules
- **Plugin System** - Extensible plugin architecture

### Key Services
- **AI Service** - Multi-provider AI integration with circuit breaker
- **OVSM Service** - LISP interpreter for blockchain automation
- **MCP Service** - Model Context Protocol server management
- **Audit Service** - Security analysis and vulnerability detection
- **SSH Deploy Service** - Remote deployment and management

### Plugin Architecture
- **Command Plugins** - Custom slash commands
- **Tool Plugins** - MCP-style tools for AI integration
- **Theme Plugins** - Visual customization
- **Integration Plugins** - External service connections

---

## 📖 API Reference

### Core Commands

**AI & Chat:**
```bash
osvm chat                         # Start AI chat (enhanced with code execution)
osvm chat --advanced              # Advanced multi-session chat
osvm "query"                      # Natural language query
osvm audit [path]                 # Security audit with AI
```

**OVSM Language:**
```bash
osvm ovsm run script.ovsm         # Execute OVSM script
osvm ovsm eval '(+ 1 2 3)'        # Execute inline OVSM code
osvm ovsm check script.ovsm       # Syntax check without execution
osvm ovsm repl                    # Interactive REPL
osvm ovsm examples                # Show example scripts
```

**SVM Management:**
```bash
osvm svm list                     # List all SVMs
osvm svm get <name>               # Get SVM details
osvm svm dashboard                # Launch monitoring dashboard
```

**Node Operations:**
```bash
osvm nodes list                   # List deployed nodes
osvm nodes status <id>            # Check node status
osvm nodes logs <id> [--follow]   # View node logs
osvm nodes restart <id>           # Restart node
```

**MCP Operations:**
```bash
osvm mcp list                     # List configured servers
osvm mcp tools [server]           # List available tools
osvm mcp call <server> <tool>     # Execute tool
```

**Plugin Management:**
```bash
osvm plugins list                 # List installed plugins
osvm plugins install <path>       # Install plugin
osvm plugins enable <name>        # Enable plugin
```

---

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
  "min_osvm_version": "0.9.0",
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

---

## 🛠️ Development Resources

### Contributing to OSVM
- [**Development Setup**](../CLAUDE.md#development-environment-setup) - Complete setup guide
- [**Code Style Guide**](../CLAUDE.md#code-style) - Coding standards and conventions
- [**Testing Strategy**](../CLAUDE.md#testing-strategy) - Testing guidelines
- [**Architecture Guide**](../CLAUDE.md#project-structure) - Codebase architecture
- [**CHANGELOG**](../CHANGELOG.md) - Version history and release notes

### External Resources
- [**Solana Documentation**](https://docs.solana.com/) - Solana blockchain documentation
- [**Model Context Protocol**](https://modelcontextprotocol.io/) - MCP specification
- [**Rust Programming**](https://doc.rust-lang.org/) - Rust language documentation
- [**GitHub Actions**](https://docs.github.com/en/actions) - CI/CD documentation

---

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

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](../LICENSE) file for details.

---

**🔗 Quick Links:**
- [Main Website](index.html) | [GitHub Repository](https://github.com/opensvm/osvm-cli) | [Download Latest](https://github.com/opensvm/osvm-cli/releases/latest) | [CHANGELOG](../CHANGELOG.md)

---

**Version:** 0.9.2
**Last updated:** October 19, 2025
**Status:** Production Ready
