# OVSM MCP Integration Examples

This directory contains example OVSM scripts that demonstrate integration with the osvm-mcp server for blockchain RPC operations.

## Prerequisites

### 1. Start the osvm-mcp Server

The osvm-mcp server provides blockchain RPC tools to OVSM scripts:

```bash
# Clone and start osvm-mcp (in a separate terminal)
cd ~/projects
git clone https://github.com/openSVM/osvm-mcp
cd osvm-mcp
npm install
npm start  # Runs on http://localhost:3000
```

### 2. Configure Solana RPC

The osvm-mcp server needs a Solana RPC endpoint. Set it in `.env`:

```bash
# In osvm-mcp directory
cat > .env << EOF
SOLANA_RPC_URL=https://api.mainnet-beta.solana.com
PORT=3000
EOF
```

### 3. Configure OSVM

Add osvm-mcp as an MCP server:

```bash
osvm mcp add osvm-mcp http://localhost:3000
osvm mcp list  # Verify it's registered
```

## Available Examples

### 1. Comprehensive Demo (`mcp_comprehensive_demo.ovsm`) ⭐ START HERE

**Complete demonstration of all 8 tool categories:**
- Account tools (stats, portfolio, type checking)
- Transaction tools (details, analysis, explanations)
- Block tools (current block, stats, recent blocks)
- DeFi analytics (TVL, volume, protocols)
- Token tools (metadata, info, holders)
- Search tools (universal search)
- Direct RPC access (90+ Solana methods)
- Utility tools (program registry)

**Run:**
```bash
osvm ovsm run examples/ovsm_scripts/mcp_comprehensive_demo.ovsm --debug
```

### 2. Basic RPC Operations (`mcp_basic_rpc.ovsm`)

Demonstrates fundamental blockchain queries:
- `getSlot()` - Current slot number
- `getBlockHeight()` - Current block height
- `getVersion()` - Solana version info
- `getBalance()` - Wallet balance in lamports
- `getSignaturesForAddress()` - Recent transaction signatures

**Run:**
```bash
osvm ovsm run examples/ovsm_scripts/mcp_basic_rpc.ovsm
```

### 3. Wallet Activity Dashboard (`mcp_wallet_analysis.ovsm`)

Comprehensive wallet analysis showing:
- Transaction success/failure rates
- Time-based activity (last hour, last day)
- Hourly transaction rate
- Statistical dashboard object

**Run:**
```bash
osvm ovsm run examples/ovsm_scripts/mcp_wallet_analysis.ovsm
```

### 4. Pumpfun 1-Minute Counter (`pumpfun_1min_lisp.ovsm`)

Production example counting Pumpfun transactions in the last minute:
- Pagination with cursor-based fetching
- Time-based filtering
- Efficient batch processing

**Run:**
```bash
osvm ovsm run examples/ovsm_scripts/pumpfun_1min_lisp.ovsm
```

## Complete Tool Documentation

**📖 See [MCP_TOOLS_REFERENCE.md](./MCP_TOOLS_REFERENCE.md) for complete documentation of all 75+ tools!**

The reference includes:
- All 31 specialized osvm-mcp tools with examples
- 90+ Solana RPC methods via `solana_rpc_call`
- Parameter formats and return values
- Best practices and troubleshooting
- Complete code examples for each tool

## Quick Tool Overview

When osvm-mcp is running, these tool categories are available:

### Basic RPC
- `(getSlot)` - Get current slot
- `(getBlockHeight)` - Get block height
- `(getVersion)` - Get Solana version

### Account Queries
- `(getBalance :address "...")` - Get SOL balance
- `(getAccountInfo :address "...")` - Get account data

### Transaction Queries
- `(getSignaturesForAddress :address "..." :limit N)` - Get tx signatures
- `(getSignaturesForAddress :address "..." :before "cursor")` - Paginated fetch
- `(getTransaction :signature "...")` - Get full transaction details

### Utility
- `(now)` - Current Unix timestamp (built-in, no MCP needed)

## Response Structures

### Signature Object (from getSignaturesForAddress)

```lisp
{
  :signature "5J7c..."        ; Transaction signature (base58)
  :slot 123456789            ; Slot number
  :blockTime 1699123456      ; Unix timestamp
  :err null                  ; null if successful, object if failed
  :memo null                 ; Transaction memo (if any)
}
```

### Account Info Object (from getAccountInfo)

```lisp
{
  :lamports 1000000000       ; Balance in lamports
  :owner "11111..."          ; Program owner pubkey
  :executable false          ; Is executable program
  :rentEpoch 361            ; Rent epoch
  :data "..."               ; Account data (base64)
}
```

## Common Patterns

### 1. Filtering Successful Transactions

```lisp
(define all-sigs (getSignaturesForAddress :address wallet :limit 100))
(define successful (filter (lambda (sig) (null? (. sig err))) all-sigs))
```

### 2. Time-Based Filtering

```lisp
(define cutoff (- (now) 3600))  ; 1 hour ago
(define recent (filter
  (lambda (sig) (>= (. sig blockTime) cutoff))
  all-sigs))
```

### 3. Pagination

```lisp
(define page1 (getSignaturesForAddress :address wallet :limit 50))
(define cursor (. (last page1) signature))
(define page2 (getSignaturesForAddress :address wallet :limit 50 :before cursor))
```

### 4. Transaction Details Lookup

```lisp
(define sigs (getSignaturesForAddress :address wallet :limit 1))
(define sig-str (. (first sigs) signature))
(define tx-details (getTransaction :signature sig-str))
```

## Troubleshooting

### Error: "UndefinedTool: getSlot"

**Cause:** osvm-mcp server not running or not registered

**Fix:**
```bash
# Start osvm-mcp server
cd ~/projects/osvm-mcp
npm start

# Register with OSVM
osvm mcp add osvm-mcp http://localhost:3000
```

### Error: "Failed to initialize MCP server"

**Cause:** Wrong URL or server not responding

**Fix:**
```bash
# Test server directly
curl http://localhost:3000/health

# Re-register with correct URL
osvm mcp remove osvm-mcp
osvm mcp add osvm-mcp http://localhost:3000
```

### Error: "RPC request failed"

**Cause:** Solana RPC endpoint issues

**Fix:**
```bash
# Check .env in osvm-mcp directory
cat ~/projects/osvm-mcp/.env

# Try different RPC (Helius, Triton, etc.)
SOLANA_RPC_URL=https://mainnet.helius-rpc.com
```

## Testing Without Live RPC

For development/testing without a live blockchain connection, see:
- `tests/real_mcp_execution_tests.rs` - Mock MCP server tests
- `tests/mcp_tool_execution_tests.rs` - Unit tests with fixtures

## Architecture

```
┌─────────────────┐
│  OVSM Script    │
│  (.ovsm file)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  OVSM Evaluator │
│  (Lisp Runtime) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  McpBridgeTool  │  ◄── Converts OVSM calls to MCP protocol
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  McpService     │  ◄── Manages MCP server connections
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  osvm-mcp       │  ◄── HTTP/WebSocket MCP server
│  (Node.js)      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Solana RPC     │  ◄── Blockchain endpoint
│  (mainnet/dev)  │
└─────────────────┘
```

## Further Reading

- **OVSM Syntax:** `docs/ovsm/OVSM_LISP_SYNTAX_SPEC.md`
- **MCP Protocol:** `https://modelcontextprotocol.io`
- **osvm-mcp Repository:** `https://github.com/openSVM/osvm-mcp`
- **Solana RPC API:** `https://docs.solana.com/api`

## Contributing

Have a useful OVSM+MCP example? Submit a PR with:
1. New `.ovsm` file in `examples/ovsm_scripts/`
2. Documentation in this README
3. Test coverage if applicable
