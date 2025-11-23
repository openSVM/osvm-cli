# OSVM Streaming Server

Real-time Solana blockchain event streaming with WebSocket, SSE, and HTTP support.

## 🚀 Quick Start

```bash
# Stream all Pump.fun activity
osvm stream --programs pumpfun

# Stream multiple DEXes
osvm stream --programs "raydium,orca,jupiter"

# Stream with token filter (planned)
osvm stream --programs raydium --tokens BONK

# List available program aliases
osvm stream --list-programs

# List available token symbols
osvm stream --list-tokens
```

## 🎯 Features

### Program Aliases
No more copy-pasting long program IDs! Use friendly names:

| Alias | Category | Program ID |
|-------|----------|------------|
| `pumpfun`, `pump` | Meme Platform | `6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P` |
| `raydium` | DEX | `675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8` |
| `orca` | DEX | `9W959DqEETiGZocYWCQPaJ6sBmUzgfxXfqGeTEdp3aQP` |
| `jupiter` | Aggregator | `JUP6LkbZbjS1jKKwapdHNy74zcZ3tLUZoi5QNyVTaV4` |
| ... | ... | 40+ programs total |

### Token Symbols
Filter by familiar token names:

| Symbol | Token | Mint Address |
|--------|-------|--------------|
| `USDC` | USD Coin | `EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v` |
| `BONK` | Bonk | `DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263` |
| `SOL` | Wrapped SOL | `So11111111111111111111111111111111111111112` |
| ... | ... | 25+ tokens total |

## 📡 Streaming Protocols

The server provides three ways to consume events:

### WebSocket
Best for: Real-time bidirectional communication

```javascript
const ws = new WebSocket('ws://localhost:8080/ws');
ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    console.log('Event:', data);
};
```

### Server-Sent Events (SSE)
Best for: Server-to-client streaming, automatic reconnection

```javascript
const eventSource = new EventSource('http://localhost:8080/stream');
eventSource.onmessage = (event) => {
    const data = JSON.parse(event.data);
    console.log('Event:', data);
};
```

### HTTP Polling
Best for: Simple integration, no persistent connection

```bash
curl http://localhost:8080/events?limit=10
```

## 🏗️ Architecture

### Hybrid Streaming System

The server intelligently chooses the best streaming method:

```
┌─────────────────────────────────┐
│   osvm stream --programs X      │
└────────────┬────────────────────┘
             │
    ┌────────▼────────┐
    │ Has program     │
    │ filter?         │
    └────┬───────┬────┘
         │       │
    YES  │       │ NO
         │       │
    ┌────▼───┐  │  ┌─────▼──────┐
    │ WebSocket  │  │ HTTP       │
    │ logsSubscribe│ │ get_block()│
    │ (Real-time)│  │ (Polling)  │
    └─────────┘  │  └────────────┘
```

**WebSocket Mode** (with `--programs`):
- ✅ Real-time: <1s latency
- ✅ High throughput: 18+ events/sec tested
- ✅ Efficient: Event-driven, no polling

**HTTP Polling Mode** (without filters):
- ℹ️ General block streaming
- ℹ️ Lower throughput due to polling interval
- ℹ️ Works for unfiltered monitoring

## 🎓 Token Graduation Detection

Monitor Pump.fun for token graduations (bonding curve completions):

### Detection Patterns

```python
# Keywords that indicate graduation
GRADUATION_INDICATORS = [
    'graduate',
    'migration',
    'raydium',
    'liquidity pool',
    'bonding curve complete',
    'lp creation',
    'ammv4'
]

# Program invocations
RAYDIUM_V4 = '675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8'
```

### Example Implementation

```javascript
const eventSource = new EventSource('http://localhost:8080/stream');

eventSource.onmessage = (event) => {
    const data = JSON.parse(event.data);

    if (data.type === 'log_message') {
        const logsText = data.logs.join(' ').toLowerCase();

        // Check for graduation
        if (logsText.includes('graduate') ||
            logsText.includes('raydium')) {
            console.log('🎓 GRADUATION DETECTED!');
            console.log('Signature:', data.signature);
            console.log('Logs:', data.logs);
        }

        // Check for buy/sell
        if (logsText.includes('instruction: buy')) {
            console.log('📈 Buy detected');
        } else if (logsText.includes('instruction: sell')) {
            console.log('📉 Sell detected');
        }
    }
};
```

## 🔧 Command-Line Options

```bash
osvm stream [OPTIONS]

OPTIONS:
  --rpc-url <URL>              RPC endpoint (default: mainnet-beta)
  --host <HOST>                Server host (default: 127.0.0.1)
  -p, --port <PORT>            Server port (default: 8080)

  # Filtering
  --programs <ALIASES>         Program aliases or IDs (comma-separated)
  --accounts <ADDRESSES>       Account addresses (comma-separated)
  --tokens <SYMBOLS>           Token symbols or mints (comma-separated)
  --pools <POOLS>              Pool names (comma-separated)
  --event-types <TYPES>        Event types to include
  --success-only               Only successful transactions
  --min-fee <LAMPORTS>         Minimum transaction fee

  # Discovery
  --list-programs              List all available program aliases
  --list-tokens                List all available token symbols

  # Protocols
  --websocket <BOOL>           Enable WebSocket (default: true)
  --sse <BOOL>                 Enable SSE (default: true)
  --http <BOOL>                Enable HTTP polling (default: true)
```

## 📊 Event Types

The server emits the following event types:

### Transaction
```json
{
  "type": "transaction",
  "signature": "5Q544fKrFoe...",
  "slot": 123456789,
  "timestamp": 1700000000,
  "success": true,
  "fee": 5000,
  "signer": "9xQeWvG816...",
  "program_ids": ["6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"]
}
```

### Log Message
```json
{
  "type": "log_message",
  "signature": "3NJsrtuL8z...",
  "logs": [
    "Program 6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P invoke [1]",
    "Program log: Instruction: Buy",
    "Program 6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P success"
  ],
  "slot": 123456789
}
```

### Token Transfer
```json
{
  "type": "token_transfer",
  "signature": "2xKw4rPzX...",
  "from": "9xQeWvG816...",
  "to": "HN7cABqL...",
  "amount": 1000.5,
  "token": "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v",
  "decimals": 6
}
```

### Slot Update
```json
{
  "type": "slot_update",
  "slot": 123456789,
  "parent": 123456788,
  "timestamp": 1700000000
}
```

## 🎯 Real-World Use Cases

### 1. Monitor Pump.fun Launches
```bash
osvm stream --programs pumpfun --success-only
```

Track all successful transactions on Pump.fun in real-time.

### 2. DEX Arbitrage Bot
```bash
osvm stream --programs "raydium,orca,jupiter" --tokens "USDC,BONK"
```

Monitor prices across multiple DEXes for arbitrage opportunities.

### 3. Whale Tracker
```bash
osvm stream --tokens "SOL,mSOL,jitoSOL" --min-fee 100000
```

Detect large transactions (high fees = likely whale activity).

### 4. NFT Sales Monitor
```bash
osvm stream --programs "magiceden,tensor" --success-only
```

Track NFT sales across major marketplaces.

### 5. Stablecoin Flow Analysis
```bash
osvm stream --tokens "USDC,USDT,USDS" --event-types token_transfer
```

Monitor stablecoin transfers across the network.

## 🚦 Performance

### Tested Performance (Pump.fun)
- **Events/sec**: 18+ events per second
- **Latency**: <1 second from blockchain to client
- **Throughput**: 547 events in 30 seconds
- **Connection**: WebSocket to public Solana RPC

### Optimization Tips

1. **Use Premium RPC**: Public RPCs may have rate limits
   - Helius, QuickNode, or Triton for production
   - WebSocket subscriptions more reliable

2. **Filter Early**: Apply filters server-side
   - Use `--programs`, `--tokens`, `--success-only`
   - Reduces bandwidth and client processing

3. **Choose Right Protocol**:
   - WebSocket: Bidirectional, lowest latency
   - SSE: Unidirectional, auto-reconnect
   - HTTP: Simplest, highest latency

## ⚠️ Known Limitations

1. **Public RPC Restrictions**:
   - `api.mainnet-beta.solana.com` may rate limit
   - WebSocket subscriptions can timeout
   - Consider paid RPC for production use

2. **Token/Pool Filtering**:
   - `--tokens` and `--pools` flags partially implemented
   - Currently filters logs, not parsed token data

3. **Historical Data**:
   - Only real-time streaming, no historical backfill
   - Use RPC `getSignaturesForAddress` for history

## 📚 API Reference

### HTTP Endpoints

#### GET /health
Health check endpoint.

**Response:**
```json
{"status": "ok"}
```

#### GET /stats
Get streaming statistics.

**Response:**
```json
{
  "events_processed": 1234,
  "events_filtered": 567,
  "events_sent": 667,
  "uptime_secs": 3600,
  "connected_clients": 5,
  "last_slot": 123456789
}
```

#### GET /events
Get recent events (HTTP polling).

**Query Parameters:**
- `limit`: Max events to return (default: 10)

**Response:**
```json
[
  {"type": "transaction", ...},
  {"type": "log_message", ...}
]
```

#### POST /filter
Update event filters.

**Request Body:**
```json
{
  "program_ids": ["6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P"],
  "success_only": true,
  "min_fee": 5000,
  "event_types": ["transaction", "log_message"]
}
```

## 🔗 Links

- [Main README](../README.md)
- [Program Aliases Source](../src/utils/program_aliases.rs)
- [Stream Service Source](../src/services/stream_service.rs)
- [Stream Command Source](../src/commands/stream.rs)

## 🤝 Contributing

To add new program aliases or token symbols:

1. Edit `src/utils/program_aliases.rs`
2. Add to `PROGRAM_ALIASES` or `TOKEN_SYMBOLS` HashMap
3. Run tests: `cargo test`
4. Submit PR

## 📝 License

MIT License - See [LICENSE](../LICENSE) for details.
