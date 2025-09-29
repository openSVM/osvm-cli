# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Table of Contents
1. [Development Environment Setup](#development-environment-setup)
2. [Development Commands](#development-commands)
3. [Project Structure](#project-structure)
4. [Command Architecture](#command-architecture)
5. [Service Layer Details](#service-layer-details)
6. [Core Utilities](#core-utilities)
7. [SSH Deployment System](#ssh-deployment-system)
8. [AI Integration](#ai-integration)

9. [MCP Server Integration](#mcp-server-integration)
10. [Testing Strategy](#testing-strategy)
11. [Error Handling](#error-handling)
12. [Configuration Management](#configuration-management)
13. [Common Development Patterns](#common-development-patterns)
14. [Debugging and Troubleshooting](#debugging-and-troubleshooting)

## Development Environment Setup

### Prerequisites
- Rust 1.80.0+ with cargo
- Solana CLI tools (installed automatically via self-repair if missing)
- SSH client for remote deployments
- Docker (optional, for containerized deployments)
- Git for version control

### Initial Setup
```bash
# Clone repository
git clone https://github.com/opensvm/osvm-cli.git
cd osvm-cli

# Install pre-commit hooks for code quality
./install-pre-commit-hook.sh

# Build and test
cargo build
cargo test

# Install locally for development
make install-dev
```

## Development Commands

### Building
```bash
# Debug build (fast compilation, larger binary)
cargo build
# or
make build

# Release build (optimized, smaller binary)
cargo build --release
# or
make build-release

# Clean build artifacts
cargo clean
# or
make clean

# Full development cycle (clean, build, test)
make dev

# Full release cycle (clean, build-release, test, install)
make release
```

### Testing
```bash
# Run all tests
cargo test

# Run unit tests only
cargo test --lib --bins

# Run integration tests only
cargo test --test main

# Run specific test module
cargo test utils::self_repair

# Run tests with output displayed
cargo test -- --nocapture

# Run benchmarks
cargo bench

# Run specific benchmark
cargo bench deployment_benchmarks
```

### Code Quality
```bash
# Format all code
cargo fmt --all

# Check formatting without modifying files
cargo fmt --all -- --check

# Run clippy linter with warnings as errors
cargo clippy --all-targets --all-features -- -D warnings

# Run clippy allowing certain warnings (current project setting)
cargo clippy  # project allows all clippy warnings in lib.rs

# Quick syntax check without building
cargo check

# Check with all features
cargo check --all-features
```

### Installation and Distribution
```bash
# Install release binary to /usr/bin (requires sudo)
make install
# or manually:
./install-release.sh

# Install debug binary for development (requires sudo)
make install-dev
# or manually:
sudo cp target/debug/osvm /usr/bin/osvm
sudo chmod +x /usr/bin/osvm

# Uninstall
make uninstall
# or manually:
sudo rm -f /usr/bin/osvm /usr/bin/osvm.backup

# Verify installation
make verify-install
# or
osvm --version
```

## Project Structure

```
osvm-cli/
├── src/
│   ├── main.rs              # Entry point, command routing, main logic
│   ├── lib.rs               # Library root, module exports
│   ├── config.rs            # Configuration loading and management
│   ├── clparse.rs           # Command-line parsing with clap v4
│   ├── prelude.rs           # Common imports and types
│   ├── services/            # High-level service implementations
│   │   ├── mod.rs           # Service module exports
│   │   ├── ai_service.rs    # AI query processing, OpenAI/Ollama integration
│   │   ├── audit_service.rs # Security auditing with AI analysis
│   │   └── mcp_service.rs   # Model Context Protocol server management
│   └── utils/               # Core utilities and implementations
│       ├── mod.rs           # Utils module exports
│       ├── agent_chat.rs    # Basic agent chat UI
│       ├── agent_chat_v2.rs # Advanced FAR-style chat UI
│       ├── ssh_deploy/      # SSH deployment subsystem
│       ├── self_repair/     # Automatic system repair
│       ├── diagnostics/     # System health checks
│       └── [many more...]   # Various utility modules
├── tests/                   # Integration tests
├── benches/                 # Performance benchmarks
├── vendor/                  # Vendored dependencies
├── .github/                 # GitHub Actions workflows
│   ├── workflows/           # CI/CD pipelines
│   └── actions/             # Custom GitHub Actions
├── Cargo.toml              # Rust dependencies and metadata
├── Makefile                # Build shortcuts
└── install-release.sh      # Installation script
```

## Command Architecture

### Command Flow
1. **Entry Point** (`main.rs:742-2164`)
   - Parse command line with `clparse::parse_command_line()`
   - Check for version flags first (--version, -V, v, ver)
   - Handle special commands early (audit, mcp, chat) before config loading
   - Route unknown commands to AI service for natural language processing
   - Load configuration with `Config::load()`
   - Route to appropriate handler based on subcommand

2. **Command Parsing** (`clparse.rs`)
   - Uses clap v4 with builder pattern
   - Defines global arguments (config, keypair, verbose, debug, url)
   - Supports external subcommands for AI queries
   - Version handling through both flags and subcommands
   - Structured subcommand hierarchy

### Main Commands

#### `osvm balance [ADDRESS]`
- Shows SOL balance for an address
- Default: configured keypair address
- Uses RPC client with commitment config

#### `osvm svm <SUBCOMMAND>`
- **list**: Display all SVMs with status
- **get <NAME>**: Detailed info for specific SVM
- **dashboard**: Interactive TUI dashboard (ratatui)
- **install <NAME> <HOST>**: Deploy SVM to remote host

#### `osvm nodes <SUBCOMMAND>`
- **list**: List nodes with filters (network, type, status, svm)
- **dashboard**: Real-time node monitoring UI
- **status <NODE-ID>**: Check specific node status
- **get <NODE-ID>**: Detailed node information
- **restart/stop <NODE-ID>**: Node control operations
- **logs <NODE-ID>**: View node logs (with --follow option)
- **deploy**: Deploy new node to remote host

#### `osvm rpc-manager <SUBCOMMAND>`
- **sonic <CONNECTION>**: Deploy Sonic RPC via SSH
- **query-solana**: Query Solana network (info, health, monitor)
- **local**: Start local development RPC
- **devnet**: Start real devnet validator that syncs
- **test**: Start test validator for development

#### `osvm deploy <BINARY>`
- Deploy eBPF programs to SVM networks
- Required: binary, program-id, owner, fee-payer
- Optional: --publish-idl, --idl-file, --network
- Supports multi-network deployment

#### `osvm audit [REPOSITORY]`
- Security audit for Solana programs
- Local or GitHub repository analysis
- AI-powered vulnerability detection
- Multiple output formats (json, html, markdown)
- Exit code 1 on critical findings for CI/CD

#### `osvm mcp <SUBCOMMAND>`
- **add**: Add MCP server configuration
- **add-github**: Clone and configure from GitHub
- **list**: Show configured servers
- **enable/disable**: Toggle server status
- **test**: Test server connectivity
- **tools**: List available tools from server
- **call**: Execute tool on server
- **setup**: Quick setup for Solana MCP
- **search**: Search servers by query

#### `osvm chat [--advanced] [--test]`
- Interactive chat interface
- Basic mode: Simple chat UI
- Advanced mode: FAR-style multi-session UI
- Test mode: Run UI tests with screenshots

#### `osvm doctor [--fix]`
- System health diagnostics
- Check system dependencies
- Verify user configuration
- Optional automatic repair with --fix

#### `osvm examples [--category CATEGORY]`
- Show usage examples
- Categories: basic, svm, node, monitoring, workflow
- List categories with --list-categories

#### SSH Deployment Format
```bash
osvm user@host --svm svm1,svm2 --node-type validator --network mainnet
```

#### AI Query (Unknown Commands)
Any unknown command is interpreted as an AI query:
```bash
osvm "How do I deploy a validator?"  # Natural language query
```

## Service Layer Details

### AI Service (`services/ai_service.rs`)

**Architecture:**
- Supports multiple AI providers (OpenAI, Ollama, custom endpoints)
- Circuit breaker pattern for fault tolerance
- Template-based prompt management
- Debug logging with verbosity levels

**Configuration:**
```rust
// Environment variables
OPENAI_URL="https://api.openai.com/v1/chat/completions"
OPENAI_KEY="sk-..."
// Or custom endpoint
OPENAI_URL="http://localhost:11434/v1/chat/completions"  // Ollama
```

**Key Methods:**
- `query_with_debug()`: Process natural language queries
- `with_api_url()`: Use custom AI endpoint
- `with_internal_ai()`: Use OSVM's default AI service

**Integration Points:**
- Main command router for unknown commands
- Audit service for vulnerability analysis
- Agent chat for conversational interface

### Audit Service (`services/audit_service.rs`)

**Features:**
- Multi-source auditing (local files, GitHub repos)
- AI-enhanced vulnerability detection
- Multiple output formats
- CI/CD integration with exit codes

**Audit Flow:**
1. Clone/read repository
2. Parse Rust/Solana code with syn
3. Extract security patterns
4. Generate AI analysis prompts
5. Process with AI service
6. Format results (JSON/HTML/Markdown)
7. Save reports and optionally commit

**Configuration:**
```rust
AuditRequest {
    output_dir: String,
    format: String,         // json, html, markdown
    verbose: u8,
    test_mode: bool,
    ai_analysis: bool,
    gh_repo: Option<String>,  // owner/repo#branch
    template_path: Option<String>,
    no_commit: bool,
    api_url: Option<String>,
}
```

### MCP Service (`services/mcp_service.rs`)

**Architecture:**
- Multi-transport support (HTTP, WebSocket, stdio)
- Authentication mechanisms (bearer, basic, API key)
- Circuit breaker for reliability
- Server discovery and management
- Tool execution framework

**Server Configuration:**
```rust
McpServerConfig {
    name: String,
    url: String,
    transport_type: McpTransportType,
    auth: Option<McpAuthConfig>,
    enabled: bool,
    extra_config: HashMap<String, Value>,
    github_url: Option<String>,
    local_path: Option<String>,
}
```

**Key Operations:**
- Server lifecycle (add, remove, enable, disable)
- Tool discovery and execution
- GitHub repository integration
- Configuration persistence in `~/.osvm/mcp_config.json`

## Core Utilities

### SSH Deployment System (`utils/ssh_deploy/`)

**Module Structure:**
```
ssh_deploy/
├── mod.rs              # Module exports
├── client.rs           # SSH client implementation
├── dependencies.rs     # Dependency management
├── deploy.rs           # Main deployment logic
├── deployments/        # SVM-specific deployments
│   ├── mod.rs         # Deployment registry
│   ├── sonic.rs       # Sonic SVM deployment
│   ├── solana.rs      # Solana deployment
│   ├── eclipse.rs     # Eclipse deployment
│   └── s00n.rs        # Soon deployment
├── disk_management.rs  # Storage configuration
├── errors.rs          # Error types
├── hot_swap.rs        # Zero-downtime updates
├── monitoring.rs      # Deployment monitoring
├── optimizations.rs   # Performance tuning
├── services.rs        # Service management
├── types.rs           # Type definitions
└── validators.rs      # Validation logic
```

**Deployment Flow:**
1. Parse connection string (`user@host:port`)
2. Establish SSH connection
3. Check system dependencies
4. Install missing packages
5. Configure system parameters
6. Deploy SVM binaries
7. Setup services (systemd/supervisor)
8. Start and monitor services
9. Perform health checks

**Hot Swap Support:**
- Blue-green deployment pattern
- Zero-downtime updates
- Automatic rollback on failure
- State preservation across updates

### Agent Chat System (`utils/agent_chat_v2.rs`)

**Advanced Features:**
- FAR (File Manager) style UI with cursive-multiplex
- Multi-session chat management
- AI-powered tool planning
- Background agent execution
- Session recording and replay
- MCP server integration

**Architecture:**
```rust
ChatSession {
    id: Uuid,
    name: String,
    created_at: DateTime<Utc>,
    messages: Vec<ChatMessage>,
    agent_state: AgentState,
    recording: bool,
    recording_file: Option<String>,
}

AgentState {
    Idle,
    Thinking,
    Planning,
    ExecutingTool(String),
    Waiting,
    Paused,
    Error(String),
}
```

**UI Components:**
- Session list panel
- Chat history view
- Input area with multi-line support
- Status bar with agent state
- Tool execution panel
- Help overlay

### Self-Repair System (`utils/self_repair/`)

**Module Structure:**
```
self_repair/
├── mod.rs              # Main repair orchestration
├── package_managers.rs # Package manager detection
├── repair_strategies.rs # Repair implementations
├── system_deps.rs      # System dependency repairs
└── user_deps.rs        # User configuration repairs
```

**Repairable Errors:**
```rust
enum RepairableError {
    // System issues
    OutdatedSystemPackages,
    MissingBuildTools,
    OutdatedRustToolchain,
    MissingSystemDependencies(Vec<String>),

    // User issues
    MissingSolanaCli,
    OutdatedSolanaCli,
    MissingKeypair(String),
    InvalidConfig,
    MissingConfigDirectory,

    // Network issues
    ConnectivityIssues,
    RpcEndpointFailure(String),

    // Permissions
    InsufficientPermissions(String),
    SystemTuningRequired,
}
```

**Repair Flow:**
1. Detect issue through diagnostics
2. Categorize as repairable or manual
3. Apply appropriate repair strategy
4. Validate repair success
5. Report results

### Diagnostics System (`utils/diagnostics/`)

**Components:**
- `system_health.rs`: Overall health assessment
- `connectivity.rs`: Network connectivity checks
- `version_checker.rs`: Dependency version validation
- `rollback_validator.rs`: Rollback safety checks

**Health Check Categories:**
1. System dependencies (build tools, libraries)
2. User configuration (keypair, config files)
3. Network connectivity (RPC endpoints)
4. Version compatibility
5. Permission requirements

### eBPF Deployment (`utils/ebpf_deploy.rs`)

**Deployment Process:**
1. Validate binary and keypair files
2. Parse program ID (new deployment vs upgrade)
3. Connect to RPC endpoints
4. Check program ownership
5. Deploy/upgrade program
6. Optionally publish IDL
7. Verify deployment success

**Multi-Network Support:**
- Parallel deployment to multiple networks
- Network-specific RPC endpoints
- Retry logic with exponential backoff
- Comprehensive error reporting

## SSH Deployment System

### Deployment Configuration
```rust
DeploymentConfig {
    svm_type: String,        // "sonic", "solana", etc.
    node_type: String,       // "validator", "rpc"
    network: NetworkType,    // Mainnet, Testnet, Devnet
    node_name: String,
    rpc_url: Option<String>,
    additional_params: HashMap<String, String>,
    version: Option<String>,
    client_type: Option<String>,
    hot_swap_enabled: bool,
    metrics_config: Option<MetricsConfig>,
    disk_config: Option<DiskConfig>,
}
```

### System Optimizations
- File descriptor limits adjustment
- Network parameter tuning
- Memory management configuration
- CPU affinity settings
- I/O scheduler optimization

### Service Management
- Systemd service creation
- Supervisor configuration
- Process monitoring
- Automatic restart on failure
- Log rotation setup

## AI Integration

### Natural Language Processing
The CLI interprets unknown commands as AI queries:
```rust
// In main.rs
if !is_known_command(sub_command) {
    return handle_ai_query(sub_command, sub_matches, &app_matches).await;
}
```

### AI Provider Configuration
```bash
# OpenAI
export OPENAI_URL="https://api.openai.com/v1/chat/completions"
export OPENAI_KEY="sk-your-key"

# Ollama (local)
export OPENAI_URL="http://localhost:11434/v1/chat/completions"
export OPENAI_KEY="ollama-key"

# Custom endpoint
export OPENAI_URL="https://your-api.com/v1/chat"
export OPENAI_KEY="your-key"
```

### Template System
Located in `utils/prompt_templates.rs`:
- Security audit prompts
- Code analysis templates
- Deployment guidance
- Error diagnosis templates

### Circuit Breaker
Prevents cascading failures:
```rust
GranularCircuitBreaker {
    failure_threshold: 5,
    success_threshold: 2,
    timeout: Duration::from_secs(30),
    reset_timeout: Duration::from_secs(60),
}
```

## MCP Server Integration

### Server Types
1. **HTTP Servers**: REST API based
2. **WebSocket Servers**: Real-time bidirectional
3. **Stdio Servers**: Local process communication

### Authentication Methods
```rust
McpAuthConfig {
    auth_type: String,  // "bearer", "basic", "api_key"
    token: Option<String>,
    username: Option<String>,
    password: Option<String>,
}
```

### Tool Execution Flow
1. List available tools from server
2. Validate tool parameters
3. Execute tool with arguments
4. Process response
5. Handle errors with circuit breaker

### GitHub Integration
```bash
# Add server from GitHub
osvm mcp add-github my-server https://github.com/org/mcp-server --enabled

# Clones to ~/.osvm/mcp_servers/my-server/
# Reads mcp.json configuration
# Installs dependencies
# Registers server
```

## Testing Strategy

### Unit Tests
Located alongside source files:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_functionality() {
        // Test implementation
    }
}
```

### Integration Tests
In `tests/` directory:
- `main.rs`: Main integration test suite
- `e2e/`: End-to-end test modules
- `mock_server.rs`: Mock HTTP servers for testing

### Test Utilities
In `utils/test_utils.rs`:
- Mock SSH clients
- Fake RPC responses
- Test fixture management
- Assertion helpers

### Running Tests
```bash
# All tests
cargo test

# With coverage (requires cargo-tarpaulin)
cargo tarpaulin --out Html

# Specific test pattern
cargo test deployment

# Show test output
cargo test -- --show-output

# Run ignored tests
cargo test -- --ignored
```

## Error Handling

### Error Types
```rust
// SSH Deployment errors
pub enum DeploymentError {
    ConnectionFailed(String),
    AuthenticationFailed(String),
    DependencyMissing(String),
    ServiceStartFailed(String),
    ValidationFailed(String),
}

// Self-repair errors
pub enum RepairError {
    SystemDependency(String),
    UserDependency(String),
    NetworkError(String),
    PermissionError(String),
    ValidationError(String),
}
```

### Error Propagation
- Use `Result<T, Box<dyn Error>>` for main functions
- Use `anyhow::Result` in utilities for context
- Use `thiserror` for custom error types
- Always provide context with `.context()`

### Error Recovery
1. Automatic retry with backoff
2. Circuit breaker activation
3. Self-repair attempt
4. Fallback to manual intervention
5. Detailed error reporting

## Configuration Management

### Configuration Loading (`config.rs`)
```rust
Config::load(app_matches, sub_matches) -> Result<Config>
```

**Order of precedence:**
1. Command-line arguments
2. Environment variables
3. Config file (`~/.config/osvm/config.yml`)
4. Default values

### Configuration Structure
```yaml
# ~/.config/osvm/config.yml
json_rpc_url: https://api.mainnet-beta.solana.com
keypair_path: ~/.config/solana/id.json
commitment: confirmed
```

### Environment Variables
```bash
# Color output
NO_COLOR=1              # Disable colors

# Solana configuration
SOLANA_CONFIG=~/.config/solana/cli/config.yml

# AI configuration
OPENAI_URL=...
OPENAI_KEY=...

# Debug output
RUST_LOG=debug
RUST_BACKTRACE=1
```

## Common Development Patterns

### Async/Await Pattern
```rust
// All network operations are async
#[tokio::main]
async fn main() -> Result<()> {
    let result = some_async_operation().await?;
    Ok(())
}
```

### Builder Pattern
```rust
// Used for complex configurations
let config = DeploymentConfig::builder()
    .svm_type("sonic")
    .network(NetworkType::Mainnet)
    .hot_swap_enabled(true)
    .build()?;
```

### Command Pattern
```rust
// Each subcommand is handled by a dedicated function
match sub_command {
    "svm" => handle_svm_command(matches),
    "nodes" => handle_nodes_command(matches),
    _ => handle_unknown_command(sub_command),
}
```

### Repository Pattern
```rust
// Data access abstracted through repository traits
trait NodeRepository {
    async fn list_nodes(&self) -> Result<Vec<Node>>;
    async fn get_node(&self, id: &str) -> Result<Node>;
}
```

### Circuit Breaker Pattern
```rust
// Prevents cascading failures
let breaker = CircuitBreaker::new(config);
match breaker.call(async_operation).await {
    Ok(result) => process(result),
    Err(CircuitOpen) => use_fallback(),
}
```

## Debugging and Troubleshooting

### Debug Output
```bash
# Enable debug mode
osvm --debug <command>

# Verbose output levels
osvm -v      # Level 1: Basic info
osvm -vv     # Level 2: Detailed info
osvm -vvv    # Level 3: Trace level

# Rust logging
RUST_LOG=debug osvm <command>
RUST_LOG=osvm=trace,solana=debug osvm <command>
```

### Common Issues and Solutions

**1. Compilation Errors**
```bash
# Clean build
cargo clean && cargo build

# Update dependencies
cargo update

# Check for breaking changes
cargo tree -d  # Show duplicate dependencies
```

**2. SSH Deployment Failures**
```bash
# Test SSH connection
ssh user@host "echo test"

# Check SSH key permissions
chmod 600 ~/.ssh/id_rsa

# Verbose SSH debugging
osvm --debug user@host --svm sonic
```

**3. RPC Connection Issues**
```bash
# Test RPC endpoint
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"getHealth"}' \
  https://api.mainnet-beta.solana.com

# Use custom RPC
osvm --url https://your-rpc.com <command>
```

**4. Self-Repair Issues**
```bash
# Run diagnostics
osvm doctor

# Attempt automatic repair
osvm doctor --fix

# Manual repair for specific issue
osvm doctor --fix --system-only
```

### Performance Profiling
```bash
# CPU profiling with flamegraph
cargo install flamegraph
cargo flamegraph --bin osvm -- svm list

# Memory profiling with valgrind
valgrind --tool=massif target/release/osvm svm list
ms_print massif.out.<pid>

# Benchmark specific operations
cargo bench deployment_benchmarks
```

### Log Files
```
# OSVM logs
~/.osvm/logs/osvm.log        # Main application log
~/.osvm/logs/audit.log       # Audit operations
~/.osvm/logs/deployment.log  # SSH deployments
~/.osvm/logs/mcp.log         # MCP server operations

# Solana validator logs
./agave-validator-*.log      # Validator output
./solana-validator-*.log     # Legacy validator
```

## Advanced Topics

### Adding New SVM Support
1. Create deployment module in `ssh_deploy/deployments/new_svm.rs`
2. Implement `Deployment` trait
3. Register in `deployments/mod.rs`
4. Add to clparse options
5. Update documentation

### Creating Custom MCP Servers
1. Implement MCP protocol
2. Create server configuration
3. Add discovery mechanism
4. Implement tool handlers
5. Test with `osvm mcp test`

### Extending AI Capabilities
1. Add prompt templates
2. Implement analysis vectors
3. Configure circuit breaker
4. Add response parsing
5. Test with various providers

### Contributing Workflow
1. Fork repository
2. Create feature branch
3. Install pre-commit hooks
4. Implement changes
5. Add tests
6. Run full test suite
7. Update documentation
8. Submit pull request

## Performance Considerations

### Memory Management
- Message history limited to 1000 entries
- Automatic cleanup of old sessions
- Lazy loading of large datasets
- Stream processing for logs

### Network Optimization
- Connection pooling for RPC clients
- Parallel deployment operations
- Async I/O throughout
- Circuit breaker for failing endpoints

### CPU Optimization
- Tokio runtime configuration
- Thread pool sizing
- CPU affinity for validators
- Parallel compilation where possible

## Security Considerations

### Keypair Management
- Never log private keys
- Use secure storage paths
- Validate keypair permissions
- Support hardware wallets

### SSH Security
- Key-based authentication only
- Known hosts verification
- Secure command execution
- No password storage

### Audit Security
- Sandboxed code execution
- Limited filesystem access
- No network access during analysis
- Secure AI prompt construction

## Maintenance Tasks

### Dependency Updates
```bash
# Check outdated dependencies
cargo outdated

# Update dependencies
cargo update

# Update major versions (manual Cargo.toml edit required)
cargo upgrade --incompatible
```

### Code Quality Maintenance
```bash
# Run all quality checks
make dev  # clean, build, test

# Fix formatting
cargo fmt --all

# Fix clippy warnings
cargo clippy --fix

# Check for security vulnerabilities
cargo audit
```

### Documentation Updates
- Keep CLAUDE.md synchronized with code changes
- Update README.md for user-facing changes
- Document new features in code comments
- Update API documentation with cargo doc

## Important Implementation Notes

1. **Clippy Warnings**: The project currently allows all clippy warnings (`#![allow(clippy::all)]`). Consider selectively enabling clippy lints for better code quality.

2. **Error Handling**: The project uses a mix of error handling strategies. Prefer `anyhow::Result` for internal functions and custom error types for public APIs.

3. **Async Runtime**: The project uses Tokio. All async operations should use `tokio::spawn` for concurrent execution when appropriate.

4. **Configuration Priority**: Command-line args > Environment variables > Config file > Defaults

5. **Version Compatibility**: Maintain compatibility with Solana SDK 3.0.0. Test thoroughly when updating Solana dependencies.

6. **SSH Operations**: Always validate SSH connections before attempting deployments. Use timeouts to prevent hanging operations.

7. **AI Rate Limiting**: Implement rate limiting for AI service calls to prevent quota exhaustion.

8. **MCP Server Health**: Regular health checks for enabled MCP servers. Disable failing servers automatically.

9. **Test Coverage**: Aim for >80% test coverage. Use `cargo tarpaulin` for coverage reports.

10. **Documentation**: All public functions should have doc comments. Use `cargo doc` to generate API documentation.