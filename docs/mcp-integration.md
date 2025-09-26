# OSVM CLI - MCP Server Integration

## Overview

The OSVM CLI now supports Model Context Protocol (MCP) servers, allowing seamless integration with external services that provide blockchain data and functionality through the standardized MCP protocol. This enables unified VM operations across different cloud providers and enhanced workflow flexibility for teams leveraging MCP infrastructure.

## What is MCP?

Model Context Protocol (MCP) is an open standard for connecting AI assistants to data sources and tools. In the context of OSVM CLI, MCP servers provide structured access to Solana blockchain data, making it easy to query and interact with the blockchain through both programmatic calls and natural language interfaces.

## Key Features

- **MCP Server Management**: Configure, enable, disable, and test MCP server connections
- **Multiple Transport Types**: Support for HTTP, WebSocket, and stdio transports
- **Authentication Support**: Bearer tokens, API keys, and basic authentication
- **Tool Discovery**: Automatically discover and list available tools from MCP servers
- **Tool Execution**: Call MCP server tools with parameters and get formatted results
- **Circuit Breaker**: Automatic failure recovery and error handling
- **Environment Configuration**: Load servers from environment variables
- **Debug Mode**: Comprehensive debugging information for troubleshooting

## Quick Start

### 1. Set up Solana MCP Server Integration

```bash
# Quick setup with default configuration
osvm mcp setup --auto-enable

# Custom MCP server URL
osvm mcp setup --mcp-url http://your-mcp-server:8080 --auto-enable
```

### 2. List Configured Servers

```bash
# List all configured MCP servers
osvm mcp list

# Show only enabled servers
osvm mcp list --enabled-only

# JSON output
osvm mcp list --json
```

### 3. Test Server Connectivity

```bash
# Test connection to a specific server
osvm mcp test solana

# Test with debug output
osvm mcp test solana --debug
```

### 4. Discover Available Tools

```bash
# List all tools available from a server
osvm mcp tools solana

# JSON output for programmatic use
osvm mcp tools solana --json
```

### 5. Call MCP Tools

```bash
# Call a tool without arguments
osvm mcp call solana getHealth

# Call with arguments
osvm mcp call solana getAccountInfo --args '{"pubkey":"11111111111111111111111111111112"}'

# JSON output
osvm mcp call solana getBalance --args '{"pubkey":"11111111111111111111111111111112"}' --json
```

## Command Reference

### Server Management

#### Add Server
```bash
osvm mcp add <server_id> --server-url <url> [OPTIONS]

Options:
  --name <NAME>                  Human-readable name for the server
  --transport <TYPE>            Transport type: http, websocket, stdio [default: http]
  --auth-type <TYPE>           Authentication type: none, bearer, api_key, basic [default: none]
  --auth-token <TOKEN>         Authentication token (for bearer/api_key)
  --username <USERNAME>        Username (for basic auth)
  --password <PASSWORD>        Password (for basic auth)
  --enabled                    Enable server immediately
```

Examples:
```bash
# Add HTTP server with no authentication
osvm mcp add myserver --server-url http://localhost:3000 --enabled

# Add server with bearer token authentication
osvm mcp add secure-server \
  --server-url https://api.example.com \
  --auth-type bearer \
  --auth-token your-token-here \
  --enabled

# Add server with basic authentication
osvm mcp add basic-server \
  --server-url https://secure.example.com \
  --auth-type basic \
  --username myuser \
  --password mypass
```

#### Remove Server
```bash
osvm mcp remove <server_id>
```

#### Enable/Disable Server
```bash
osvm mcp enable <server_id>
osvm mcp disable <server_id>
```

### Server Operations

#### Initialize Connection
```bash
osvm mcp init <server_id>
```

#### Test Connectivity
```bash
osvm mcp test <server_id>
```

#### List Available Tools
```bash
osvm mcp tools <server_id> [--json]
```

#### Call Tools
```bash
osvm mcp call <server_id> <tool_name> [--args <JSON>] [--json]
```

## Environment Variables

The MCP service supports configuration through environment variables:

- `SOLANA_MCP_SERVER_URL`: URL for the primary Solana MCP server
- `MCP_SERVER_<NAME>_URL`: URL for custom MCP servers (e.g., `MCP_SERVER_CUSTOM_URL`)

Examples:
```bash
# Configure Solana MCP server
export SOLANA_MCP_SERVER_URL=http://localhost:3000

# Configure custom MCP servers
export MCP_SERVER_ECLIPSE_URL=https://eclipse-mcp.example.com
export MCP_SERVER_SONIC_URL=https://sonic-mcp.example.com
```

## Solana MCP Server Integration

The primary integration target is the [openSVM/solana-mcp-server](https://github.com/openSVM/solana-mcp-server), which provides comprehensive Solana blockchain access through MCP.

### Available Tools

When connected to the Solana MCP server, you'll have access to tools like:

- `getAccountInfo`: Get account information by pubkey
- `getBalance`: Get SOL balance for an account
- `getBlock`: Get block information by slot
- `getTransaction`: Get transaction details by signature
- `getTokenAccounts`: Get token accounts for an owner
- And many more Solana RPC methods...

### Usage Examples

```bash
# Check SOL balance
osvm mcp call solana getBalance --args '{"pubkey":"Gh9ZwEmdLJ8DscKNTkTqPbNwLNNBjuSzaG9Vp2KGtKJr"}'

# Get account information
osvm mcp call solana getAccountInfo --args '{"pubkey":"11111111111111111111111111111112"}'

# Get current slot
osvm mcp call solana getSlot

# Get block information
osvm mcp call solana getBlock --args '{"slot":123456789}'
```

## Authentication

### Bearer Token Authentication

For servers requiring Bearer token authentication:

```bash
osvm mcp add secure-solana \
  --server-url https://secure-solana-mcp.example.com \
  --auth-type bearer \
  --auth-token your-jwt-token-here \
  --enabled
```

### API Key Authentication

For servers using API key authentication:

```bash
osvm mcp add api-server \
  --server-url https://api-solana-mcp.example.com \
  --auth-type api_key \
  --auth-token your-api-key-here \
  --enabled
```

### Basic Authentication

For servers using username/password authentication:

```bash
osvm mcp add basic-server \
  --server-url https://basic-auth-mcp.example.com \
  --auth-type basic \
  --username myusername \
  --password mypassword \
  --enabled
```

## Transport Types

### HTTP Transport (Default)

Standard HTTP JSON-RPC communication:
```bash
osvm mcp add http-server --server-url http://localhost:3000 --transport http
```

### WebSocket Transport

Real-time communication over WebSocket:
```bash
osvm mcp add ws-server --server-url ws://localhost:8900 --transport websocket
```

### Stdio Transport

Direct process communication (future implementation):
```bash
osvm mcp add stdio-server --server-url ./path/to/mcp-server --transport stdio
```

## Error Handling and Circuit Breaker

The MCP service includes a built-in circuit breaker that:

- Tracks failed requests per endpoint
- Automatically disables failing endpoints temporarily
- Provides graceful recovery when services come back online
- Reports detailed error information in debug mode

Use `--debug` flag to see detailed error information:
```bash
osvm mcp test myserver --debug
```

## Integration with AI Features

MCP servers integrate seamlessly with OSVM's AI features. When MCP servers are configured and enabled, the AI service can automatically leverage them to answer blockchain-related queries with real-time data.

## Advanced Usage

### Batch Operations

Query multiple servers simultaneously (future feature):
```bash
# Call the same tool on all enabled servers
osvm mcp call-all getHealth --json
```

### Configuration Files

While currently using environment variables, future versions will support configuration files for persistent server management.

### Custom MCP Servers

You can integrate any MCP-compatible server by providing the appropriate configuration:

```bash
osvm mcp add custom-blockchain \
  --server-url https://my-custom-mcp.example.com \
  --name "My Custom Blockchain MCP" \
  --transport http \
  --auth-type bearer \
  --auth-token my-token \
  --enabled
```

## Troubleshooting

### Common Issues

1. **Connection refused**: Check if the MCP server is running and accessible
   ```bash
   osvm mcp test myserver --debug
   ```

2. **Authentication failures**: Verify your authentication credentials
   ```bash
   osvm mcp init myserver --debug
   ```

3. **Tool not found**: List available tools to verify names
   ```bash
   osvm mcp tools myserver
   ```

4. **Circuit breaker open**: The server has failed repeatedly and is temporarily disabled
   - Wait for automatic recovery
   - Or restart with `osvm mcp test myserver`

### Debug Mode

Enable debug mode for detailed troubleshooting:
```bash
# Add --debug to any MCP command
osvm mcp call solana getHealth --debug
```

## Future Enhancements

- Configuration file persistence
- Stdio transport implementation  
- WebSocket subscription support
- Batch operations across multiple servers
- Configuration import/export
- Server health monitoring dashboard
- Automatic server discovery
- Load balancing across multiple servers

## Security Considerations

- Authentication tokens are stored in memory only (not persisted to disk)
- All HTTPS connections verify certificates
- Debug mode may log sensitive information - use carefully in production
- Environment variables are loaded at runtime and not cached

## Contributing

The MCP integration is designed to be extensible. To add support for new MCP servers or features:

1. Extend the `McpService` with new methods
2. Add corresponding CLI commands in `clparse.rs`
3. Update the command handler in `main.rs`
4. Add comprehensive tests
5. Update this documentation

For more information about the Model Context Protocol, visit the [MCP specification](https://modelcontextprotocol.io).