#!/usr/bin/env node
/**
 * Simple Test MCP Server
 * 
 * A minimal MCP server for testing the microVM infrastructure.
 * Implements basic JSON-RPC 2.0 protocol with a few test tools.
 */

const readline = require('readline');

// Create readline interface for stdio
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Server state
const serverState = {
  initialized: false,
  clientInfo: null,
  requestCount: 0
};

// Available tools
const tools = [
  {
    name: "echo",
    description: "Echo back the input message",
    inputSchema: {
      type: "object",
      properties: {
        message: {
          type: "string",
          description: "The message to echo back"
        }
      },
      required: ["message"]
    }
  },
  {
    name: "add",
    description: "Add two numbers",
    inputSchema: {
      type: "object",
      properties: {
        a: { type: "number", description: "First number" },
        b: { type: "number", description: "Second number" }
      },
      required: ["a", "b"]
    }
  },
  {
    name: "get_server_info",
    description: "Get information about the server",
    inputSchema: {
      type: "object",
      properties: {}
    }
  }
];

/**
 * Handle JSON-RPC request
 */
function handleRequest(request) {
  serverState.requestCount++;
  
  const response = {
    jsonrpc: "2.0",
    id: request.id
  };

  try {
    // Route to appropriate handler
    switch (request.method) {
      case "initialize":
        return handleInitialize(request, response);
      
      case "tools/list":
        return handleToolsList(request, response);
      
      case "tools/call":
        return handleToolsCall(request, response);
      
      default:
        response.error = {
          code: -32601,
          message: `Method not found: ${request.method}`
        };
        return response;
    }
  } catch (error) {
    response.error = {
      code: -32603,
      message: `Internal error: ${error.message}`
    };
    return response;
  }
}

/**
 * Handle initialize request
 */
function handleInitialize(request, response) {
  if (serverState.initialized) {
    response.error = {
      code: -32600,
      message: "Server already initialized"
    };
    return response;
  }

  const params = request.params || {};
  serverState.initialized = true;
  serverState.clientInfo = params.clientInfo || {};

  response.result = {
    protocolVersion: "2024-11-05",
    capabilities: {
      tools: {}
    },
    serverInfo: {
      name: "simple-test-mcp-server",
      version: "1.0.0"
    }
  };

  console.error(`[Server] Initialized by ${serverState.clientInfo.name || 'unknown client'}`);
  return response;
}

/**
 * Handle tools/list request
 */
function handleToolsList(request, response) {
  if (!serverState.initialized) {
    response.error = {
      code: -32600,
      message: "Server not initialized"
    };
    return response;
  }

  response.result = {
    tools: tools
  };

  return response;
}

/**
 * Handle tools/call request
 */
function handleToolsCall(request, response) {
  if (!serverState.initialized) {
    response.error = {
      code: -32600,
      message: "Server not initialized"
    };
    return response;
  }

  const params = request.params || {};
  const toolName = params.name;
  const args = params.arguments || {};

  // Find the tool
  const tool = tools.find(t => t.name === toolName);
  if (!tool) {
    response.error = {
      code: -32602,
      message: `Unknown tool: ${toolName}`
    };
    return response;
  }

  // Execute the tool
  try {
    const result = executeTool(toolName, args);
    response.result = {
      content: [
        {
          type: "text",
          text: JSON.stringify(result, null, 2)
        }
      ]
    };
  } catch (error) {
    response.error = {
      code: -32603,
      message: `Tool execution failed: ${error.message}`
    };
  }

  return response;
}

/**
 * Execute a tool
 */
function executeTool(toolName, args) {
  switch (toolName) {
    case "echo":
      return {
        message: args.message,
        timestamp: new Date().toISOString()
      };
    
    case "add":
      return {
        result: args.a + args.b,
        operation: `${args.a} + ${args.b}`
      };
    
    case "get_server_info":
      return {
        serverInfo: {
          name: "simple-test-mcp-server",
          version: "1.0.0",
          initialized: serverState.initialized,
          clientInfo: serverState.clientInfo,
          requestCount: serverState.requestCount,
          uptime: process.uptime(),
          pid: process.pid,
          platform: process.platform,
          arch: process.arch,
          nodeVersion: process.version
        }
      };
    
    default:
      throw new Error(`Unknown tool: ${toolName}`);
  }
}

/**
 * Main loop - read requests from stdin, write responses to stdout
 */
console.error('[Server] Simple Test MCP Server starting...');
console.error('[Server] Protocol: JSON-RPC 2.0 over stdio');
console.error('[Server] Available tools:', tools.map(t => t.name).join(', '));

rl.on('line', (line) => {
  if (!line.trim()) {
    return; // Skip empty lines
  }

  try {
    const request = JSON.parse(line);
    const response = handleRequest(request);
    
    // Write response to stdout
    console.log(JSON.stringify(response));
  } catch (error) {
    // Invalid JSON or other error
    const errorResponse = {
      jsonrpc: "2.0",
      id: null,
      error: {
        code: -32700,
        message: `Parse error: ${error.message}`
      }
    };
    console.log(JSON.stringify(errorResponse));
  }
});

rl.on('close', () => {
  console.error('[Server] Shutting down...');
  process.exit(0);
});

// Handle signals
process.on('SIGINT', () => {
  console.error('[Server] Received SIGINT, shutting down...');
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.error('[Server] Received SIGTERM, shutting down...');
  process.exit(0);
});

console.error('[Server] Ready to accept requests');
