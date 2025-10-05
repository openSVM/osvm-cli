#!/usr/bin/env node

/**
 * Simple Echo MCP Server for Testing
 * 
 * Provides basic tools for testing MCP tool execution:
 * - echo: Returns the input message
 * - add: Adds two numbers
 * - get_timestamp: Returns current timestamp
 * - error_test: Throws an error for testing error handling
 */

const readline = require('readline');

// MCP Server configuration
const SERVER_INFO = {
  name: "echo-test-server",
  version: "1.0.0",
  capabilities: {
    tools: {}
  }
};

// Tool definitions
const TOOLS = [
  {
    name: "echo",
    description: "Echoes back the input message",
    inputSchema: {
      type: "object",
      properties: {
        message: {
          type: "string",
          description: "Message to echo back"
        }
      },
      required: ["message"]
    }
  },
  {
    name: "add",
    description: "Adds two numbers together",
    inputSchema: {
      type: "object",
      properties: {
        a: {
          type: "number",
          description: "First number"
        },
        b: {
          type: "number",
          description: "Second number"
        }
      },
      required: ["a", "b"]
    }
  },
  {
    name: "get_timestamp",
    description: "Returns the current Unix timestamp",
    inputSchema: {
      type: "object",
      properties: {}
    }
  },
  {
    name: "error_test",
    description: "Intentionally throws an error for testing error handling",
    inputSchema: {
      type: "object",
      properties: {
        error_message: {
          type: "string",
          description: "Custom error message"
        }
      }
    }
  }
];

// Tool execution functions
function executeTool(name, args) {
  switch (name) {
    case "echo":
      return {
        content: [
          {
            type: "text",
            text: `Echo: ${args.message}`
          }
        ]
      };
    
    case "add":
      const sum = args.a + args.b;
      return {
        content: [
          {
            type: "text",
            text: `${args.a} + ${args.b} = ${sum}`
          }
        ],
        result: sum
      };
    
    case "get_timestamp":
      const timestamp = Math.floor(Date.now() / 1000);
      return {
        content: [
          {
            type: "text",
            text: `Current timestamp: ${timestamp}`
          }
        ],
        timestamp: timestamp
      };
    
    case "error_test":
      throw new Error(args.error_message || "Intentional test error");
    
    default:
      throw new Error(`Unknown tool: ${name}`);
  }
}

// JSON-RPC message handler
function handleMessage(message) {
  try {
    const request = JSON.parse(message);
    
    // Handle initialize request
    if (request.method === "initialize") {
      return {
        jsonrpc: "2.0",
        id: request.id,
        result: {
          protocolVersion: "2024-11-05",
          serverInfo: SERVER_INFO,
          capabilities: SERVER_INFO.capabilities
        }
      };
    }
    
    // Handle tools/list request
    if (request.method === "tools/list") {
      return {
        jsonrpc: "2.0",
        id: request.id,
        result: {
          tools: TOOLS
        }
      };
    }
    
    // Handle tools/call request
    if (request.method === "tools/call") {
      const { name, arguments: args } = request.params;
      
      try {
        const result = executeTool(name, args || {});
        return {
          jsonrpc: "2.0",
          id: request.id,
          result: result
        };
      } catch (error) {
        return {
          jsonrpc: "2.0",
          id: request.id,
          error: {
            code: -32603,
            message: error.message
          }
        };
      }
    }
    
    // Unknown method
    return {
      jsonrpc: "2.0",
      id: request.id,
      error: {
        code: -32601,
        message: `Method not found: ${request.method}`
      }
    };
    
  } catch (error) {
    return {
      jsonrpc: "2.0",
      id: null,
      error: {
        code: -32700,
        message: `Parse error: ${error.message}`
      }
    };
  }
}

// Main stdio loop
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Log to stderr for debugging (stdout is for JSON-RPC)
function log(message) {
  console.error(`[echo-server] ${message}`);
}

log("Echo Test MCP Server starting...");
log(`Tools available: ${TOOLS.map(t => t.name).join(', ')}`);

rl.on('line', (line) => {
  if (!line.trim()) return;
  
  log(`Received: ${line.substring(0, 100)}...`);
  
  const response = handleMessage(line);
  const responseStr = JSON.stringify(response);
  
  log(`Sending: ${responseStr.substring(0, 100)}...`);
  console.log(responseStr);
});

rl.on('close', () => {
  log("Echo Test MCP Server shutting down");
  process.exit(0);
});

// Handle uncaught errors
process.on('uncaughtException', (error) => {
  log(`Uncaught exception: ${error.message}`);
  process.exit(1);
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection: ${reason}`);
  process.exit(1);
});

log("Ready for requests");
