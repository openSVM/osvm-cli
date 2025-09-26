# OSVM CLI Documentation

Welcome to the comprehensive documentation for OSVM CLI - the Solana Virtual Machine command-line interface with integrated AI and MCP capabilities.

## Documentation Index

### Core Features
- [SVM Management](svm-management.md) - Manage Solana Virtual Machines
- [Node Management](node-management.md) - Deploy and monitor validator/RPC nodes
- [SSH Deployment](ssh-deployment.md) - Remote deployment capabilities
- [RPC Manager](rpc-manager.md) - Comprehensive RPC node management

### AI Integration
- [AI Endpoint Configuration](ai-endpoint-configuration.md) - Configure AI endpoints for smart contract analysis
- [DeepLogic AI Analysis](deeplogic-ai-analysis.md) - AI-powered logical vulnerability detection with code remediation
- [Security Audit](security-audit.md) - Comprehensive security analysis and reporting

### MCP Integration
- [MCP Integration](mcp-integration.md) - Model Context Protocol server integration
- [GitHub MCP Servers](mcp-integration.md#github-integration) - Clone and deploy MCP servers from repositories
- [MCP Tool Discovery](mcp-integration.md#tool-discovery) - Discover and execute blockchain tools

### Advanced Features
- [Self-Repair System](self-repair-system.md) - Automatic error detection and repair
- [Log Monitoring](log-monitoring.md) - Real-time log analysis and auto-repair
- [Validator Enhancements](validator-enhancements.md) - Hardware optimization and configuration
- [eBPF Deployment](ebpf-deployment.md) - Solana program deployment

### User Interface
- [Dashboard](dashboard.md) - Interactive monitoring dashboards
- [Examples](examples.md) - Usage examples and command patterns

### Setup & Configuration
- [Installation](installation.md) - Installation guide
- [Configuration](configuration.md) - Configuration management

## Quick Start

1. [Install OSVM CLI](installation.md)
2. [Configure your environment](configuration.md)
3. [Try basic SVM operations](svm-management.md#quick-start)
4. [Set up AI integration](ai-endpoint-configuration.md#quick-start)
5. [Configure MCP servers](mcp-integration.md#quick-start)
6. [Deploy your first node](node-management.md#quick-start)
7. [Run a security audit](security-audit.md#quick-start)

## Architecture Overview

## Architecture Overview

### Core Components
- **CLI Interface**: Command-line interface with comprehensive subcommands
- **SVM Manager**: Solana Virtual Machine lifecycle management
- **Node Manager**: Validator and RPC node deployment and monitoring
- **SSH Deployment**: Remote server deployment capabilities
- **RPC Manager**: Comprehensive RPC node management and scaling

### AI Integration
- **AI Service**: Multi-provider AI endpoint support (OpenAI, Ollama, LocalAI, etc.)
- **Smart Contract Analysis**: AI-powered security analysis and code review
- **DeepLogic Analysis**: Advanced logical vulnerability detection
- **Natural Language Queries**: Query blockchain data using natural language

### MCP Integration
- **MCP Client**: Full Model Context Protocol client implementation
- **Transport Support**: HTTP, WebSocket, and stdio transport protocols
- **GitHub Integration**: Clone and build MCP servers from GitHub repositories
- **Tool Discovery**: Automatic discovery and execution of MCP server tools
- **Circuit Breaker**: Fault-tolerant MCP server communication

### External Integrations
- **Solana Network**: Direct blockchain interaction and monitoring
- **GitHub Repositories**: Clone and deploy MCP servers from public/private repos
- **Remote Servers**: SSH-based deployment and management
- **Local Environment**: Development and testing support

## Support

For issues, questions, or contributions, please visit the [OSVM CLI repository](https://github.com/openSVM/osvm-cli).