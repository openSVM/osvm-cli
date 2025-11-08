# Dune Analytics MCP Integration Guide

Complete guide for integrating Dune Analytics as an MCP (Model Context Protocol) tool with OSVM CLI.

## Overview

Dune Analytics provides powerful blockchain analytics through SQL queries and curated APIs. This integration allows OSVM to:
- Execute custom Dune queries
- Retrieve query results
- Access real-time token data
- Get wallet balances and transactions
- Use pre-built analytics dashboards

## Prerequisites

1. **Dune Account**: Sign up at [dune.com](https://dune.com)
2. **API Key**: Generate at Settings → API → Create New API Key
3. **MCP Server**: Node.js or Python MCP server implementation
4. **OSVM CLI**: Latest version with MCP support

---

## Step 1: Get Your Dune API Key

1. Log in to [dune.com](https://dune.com)
2. Navigate to **Settings** → **API**
3. Click **Create New API Key**
4. Choose permission level:
   - **All endpoints** (recommended for OSVM): Full access to queries, executions, results
   - **Only Developer APIs**: Limited to real-time APIs only
5. **Save your API key securely** - it won't be shown again!

```bash
# Store your API key as environment variable
export DUNE_API_KEY="your_api_key_here"
```

---

## Step 2: MCP Server Implementation

### Option A: Node.js MCP Server (Recommended)

Create a new MCP server for Dune Analytics:

```bash
# Create project directory
mkdir -p ~/.osvm/mcp-servers/dune-analytics
cd ~/.osvm/mcp-servers/dune-analytics

# Initialize Node.js project
npm init -y
npm install @modelcontextprotocol/sdk axios dotenv
```

**Create `dune-mcp-server.js`:**

```javascript
#!/usr/bin/env node

const { Server } = require('@modelcontextprotocol/sdk/server');
const { StdioServerTransport } = require('@modelcontextprotocol/sdk/server/stdio');
const axios = require('axios');
require('dotenv').config();

const DUNE_API_BASE = 'https://api.dune.com/api/v1';
const API_KEY = process.env.DUNE_API_KEY;

if (!API_KEY) {
  console.error('ERROR: DUNE_API_KEY environment variable not set');
  process.exit(1);
}

// Create MCP server
const server = new Server(
  {
    name: 'dune-analytics',
    version: '1.0.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Configure axios with API key
const duneClient = axios.create({
  baseURL: DUNE_API_BASE,
  headers: {
    'x-dune-api-key': API_KEY,
    'Content-Type': 'application/json'
  }
});

// Tool 1: Execute Dune Query
server.setRequestHandler('tools/call', async (request) => {
  const { name, arguments: args } = request.params;

  try {
    switch (name) {
      case 'execute_dune_query': {
        const { query_id, parameters } = args;

        const response = await duneClient.post(`/query/${query_id}/execute`, {
          query_parameters: parameters || {}
        });

        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(response.data, null, 2)
            }
          ]
        };
      }

      case 'get_query_results': {
        const { query_id, execution_id } = args;

        let url = `/query/${query_id}/results`;
        if (execution_id) {
          url += `/${execution_id}`;
        } else {
          url += '/latest';
        }

        const response = await duneClient.get(url);

        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(response.data, null, 2)
            }
          ]
        };
      }

      case 'get_token_balances': {
        const { wallet_address, chain } = args;

        const response = await duneClient.get('/balances', {
          params: {
            wallet: wallet_address,
            chain: chain || 'solana'
          }
        });

        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(response.data, null, 2)
            }
          ]
        };
      }

      case 'get_execution_status': {
        const { execution_id } = args;

        const response = await duneClient.get(`/execution/${execution_id}/status`);

        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(response.data, null, 2)
            }
          ]
        };
      }

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            error: error.message,
            details: error.response?.data || 'No additional details'
          }, null, 2)
        }
      ],
      isError: true
    };
  }
});

// List available tools
server.setRequestHandler('tools/list', async () => {
  return {
    tools: [
      {
        name: 'execute_dune_query',
        description: 'Execute a Dune Analytics query by ID with optional parameters',
        inputSchema: {
          type: 'object',
          properties: {
            query_id: {
              type: 'string',
              description: 'Dune query ID to execute'
            },
            parameters: {
              type: 'object',
              description: 'Query parameters as key-value pairs',
              additionalProperties: true
            }
          },
          required: ['query_id']
        }
      },
      {
        name: 'get_query_results',
        description: 'Get results from a Dune query execution',
        inputSchema: {
          type: 'object',
          properties: {
            query_id: {
              type: 'string',
              description: 'Dune query ID'
            },
            execution_id: {
              type: 'string',
              description: 'Specific execution ID, or omit for latest'
            }
          },
          required: ['query_id']
        }
      },
      {
        name: 'get_token_balances',
        description: 'Get real-time token balances for a wallet address',
        inputSchema: {
          type: 'object',
          properties: {
            wallet_address: {
              type: 'string',
              description: 'Wallet address to query'
            },
            chain: {
              type: 'string',
              description: 'Blockchain (default: solana)',
              enum: ['ethereum', 'solana', 'polygon', 'arbitrum', 'optimism', 'base']
            }
          },
          required: ['wallet_address']
        }
      },
      {
        name: 'get_execution_status',
        description: 'Check the status of a query execution',
        inputSchema: {
          type: 'object',
          properties: {
            execution_id: {
              type: 'string',
              description: 'Execution ID to check status for'
            }
          },
          required: ['execution_id']
        }
      }
    ]
  };
});

// Start server
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error('Dune Analytics MCP server running on stdio');
}

main().catch((error) => {
  console.error('Server error:', error);
  process.exit(1);
});
```

**Create `.env` file:**

```bash
DUNE_API_KEY=your_actual_api_key_here
```

**Make executable:**

```bash
chmod +x dune-mcp-server.js
```

---

## Step 3: Configure OSVM MCP

Add the Dune Analytics MCP server to OSVM configuration:

```bash
# Edit or create MCP config
nano ~/.osvm/mcp_config.json
```

**Add this configuration:**

```json
{
  "servers": {
    "dune-analytics": {
      "command": "node",
      "args": ["/home/YOUR_USERNAME/.osvm/mcp-servers/dune-analytics/dune-mcp-server.js"],
      "env": {
        "DUNE_API_KEY": "your_dune_api_key_here"
      },
      "transport": "stdio"
    }
  }
}
```

**Or use environment variable:**

```json
{
  "servers": {
    "dune-analytics": {
      "command": "node",
      "args": ["/home/YOUR_USERNAME/.osvm/mcp-servers/dune-analytics/dune-mcp-server.js"],
      "env": {
        "DUNE_API_KEY": "${DUNE_API_KEY}"
      },
      "transport": "stdio"
    }
  }
}
```

---

## Step 4: Test the Integration

### Test 1: List Available Tools

```bash
osvm mcp list
```

You should see:
```
✓ Loaded 4 tools from MCP server 'dune-analytics'
  - execute_dune_query
  - get_query_results
  - get_token_balances
  - get_execution_status
```

### Test 2: Execute a Query

```bash
osvm a "get Solana DEX volume from Dune Analytics"
```

The AI will use the Dune MCP tools to fetch data!

---

## Step 5: Usage Examples

### Example 1: Token Balances

```bash
osvm a "use Dune to get token balances for wallet 5rVDMMoBQs3zJQ9DT7oxsoNZfxptgLCKhuWqdwoX9q85"
```

### Example 2: Custom Query

First, create a query on [dune.com](https://dune.com) and note the query ID (e.g., `3482190`)

```bash
osvm a "execute Dune query 3482190 for Solana NFT trading volume"
```

### Example 3: Parameterized Query

```bash
osvm a "run Dune query 1234567 with parameters: token_address='ABC...XYZ', days=7"
```

---

## Advanced Configuration

### Using HTTP Transport (Alternative)

If you prefer HTTP over stdio:

```javascript
// In dune-mcp-server.js, replace StdioServerTransport with:
const { HttpServerTransport } = require('@modelcontextprotocol/sdk/server/http');

async function main() {
  const transport = new HttpServerTransport({
    port: 3001,
    path: '/dune-mcp'
  });
  await server.connect(transport);
  console.log('Dune Analytics MCP server running on http://localhost:3001/dune-mcp');
}
```

**Update mcp_config.json:**

```json
{
  "servers": {
    "dune-analytics": {
      "url": "http://localhost:3001/dune-mcp",
      "transport": "http"
    }
  }
}
```

---

## Popular Dune Queries for Solana

Here are some useful Dune query IDs for Solana analytics:

| Query Type | Description | Example Query ID |
|------------|-------------|------------------|
| DEX Volume | Daily DEX trading volume | `1234567` |
| NFT Sales | NFT marketplace activity | `2345678` |
| Token Holders | Top token holders | `3456789` |
| Wallet Activity | Active wallet metrics | `4567890` |

**Note:** Replace with actual query IDs from your Dune account or public queries.

---

## Troubleshooting

### Issue: "DUNE_API_KEY not set"

**Solution:**
```bash
# Verify environment variable
echo $DUNE_API_KEY

# Or add to your shell profile
echo 'export DUNE_API_KEY="your_key"' >> ~/.bashrc
source ~/.bashrc
```

### Issue: "401 Unauthorized"

**Cause:** Invalid or expired API key

**Solution:** Regenerate API key at dune.com/settings/api

### Issue: "Query execution timeout"

**Cause:** Large query taking too long

**Solution:**
- Check execution status with `get_execution_status`
- Use smaller date ranges
- Optimize your Dune query SQL

### Issue: MCP server not found

**Solution:**
```bash
# Verify server path
ls -la ~/.osvm/mcp-servers/dune-analytics/

# Check Node.js installation
node --version

# Test server directly
node ~/.osvm/mcp-servers/dune-analytics/dune-mcp-server.js
```

---

## Best Practices

1. **Rate Limiting**: Dune API has rate limits. Use query caching when possible.
2. **Query Optimization**: Write efficient SQL queries on Dune to reduce execution time.
3. **Parameter Validation**: Always validate parameters before passing to queries.
4. **Error Handling**: Implement retry logic for transient failures.
5. **Security**: Never commit API keys to git. Use environment variables.

---

## Next Steps

1. **Create Custom Queries**: Build your own analytics queries on dune.com
2. **Explore Public Dashboards**: Browse community queries for inspiration
3. **Combine Tools**: Use Dune Analytics with other MCP tools for richer insights
4. **Automate Reports**: Schedule OSVM queries with Dune data

---

## Additional Resources

- [Dune API Documentation](https://docs.dune.com/api-reference/)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [OSVM MCP Integration Guide](../MCP_INTEGRATION.md)
- [Dune Community Dashboards](https://dune.com/browse/dashboards)

---

## Support

- **Dune Issues**: [Discord](https://discord.gg/dunecom)
- **OSVM Issues**: [GitHub](https://github.com/anthropics/osvm-cli/issues)
- **MCP Protocol**: [GitHub Discussions](https://github.com/modelcontextprotocol/specification/discussions)
