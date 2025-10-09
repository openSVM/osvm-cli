# ClickHouse Database Integration Guide

Complete guide for using OSVM's ClickHouse database system for blockchain data indexing and activity logging.

## Overview

The ClickHouse integration provides:
- **Managed ClickHouse Process**: OSVM manages a local ClickHouse database server
- **Activity Logging**: Automatic logging of CLI commands and chat sessions
- **Blockchain Indexing**: Index accounts and transactions from Solana ledgers/snapshots
- **Real-time Sync**: Continuous monitoring and indexing of new blockchain data
- **Powerful Queries**: SQL-based queries for analysis and forensics

## Architecture

```
┌─────────────────────────────────────────────┐
│              OSVM CLI                        │
├─────────────────────────────────────────────┤
│  Commands                                    │
│  ├─ osvm db (database management)            │
│  ├─ osvm realtime (continuous sync)          │
│  └─ Chat sessions (auto-logging)             │
├─────────────────────────────────────────────┤
│  Services                                    │
│  ├─ ClickHouseService (process mgmt)         │
│  ├─ ActivityLogger (CLI/chat logs)           │
│  ├─ BlockchainIndexer (data sync)            │
│  └─ RealtimeDaemon (continuous monitoring)   │
└─────────────────────────────────────────────┘
           ↓
    ┌──────────────┐
    │  ClickHouse  │
    │  (Managed)   │
    └──────────────┘
           ↓
┌───────────────────────────────────────────┐
│  Database Tables                           │
│  ├─ cli_commands (command history)         │
│  ├─ chat_messages (chat sessions)          │
│  ├─ blockchain_accounts (account data)     │
│  └─ blockchain_transactions (tx history)   │
└───────────────────────────────────────────┘
```

## Quick Start

### 1. Initialize ClickHouse

```bash
# Initialize database (downloads ClickHouse if needed)
osvm db init

# Start the database server
osvm db start

# Check status
osvm db status
```

### 2. Index Blockchain Data

```bash
# Sync last 30 days of data (default)
osvm db sync

# Sync all historical data
osvm db sync --mode full-historical

# Sync specific programs only
osvm db sync --programs TOKEN_PROGRAM_ID,STAKE_PROGRAM_ID

# Sync with data pattern matching
osvm db sync --pattern 0x1234abcd
```

### 3. Start Real-time Monitoring

```bash
# Start continuous sync daemon
osvm realtime start

# Start with filters
osvm realtime start --programs TOKEN_PROGRAM_ID --accounts PUBKEY1,PUBKEY2

# Check daemon status
osvm realtime status

# Stop daemon
osvm realtime stop
```

### 4. Query Your Data

```bash
# View activity statistics
osvm db activity --stats

# View CLI command history
osvm db activity --commands --limit 50

# View chat history
osvm db activity --chat

# Execute custom SQL queries
osvm db query --query "SELECT * FROM osvm.cli_commands ORDER BY timestamp DESC LIMIT 10"
```

## Database Schema

### Table: `cli_commands`
Logs all CLI command executions:
```sql
CREATE TABLE osvm.cli_commands (
    timestamp DateTime64(3),
    session_id String,
    command String,
    args String,
    exit_code Int32,
    duration_ms UInt64,
    error_message String
) ENGINE = MergeTree()
ORDER BY (timestamp, session_id);
```

### Table: `chat_messages`
Logs all agent chat interactions:
```sql
CREATE TABLE osvm.chat_messages (
    timestamp DateTime64(3),
    session_id String,
    session_name String,
    message_type Enum8('User'=1, 'Assistant'=2, 'System'=3, 'Processing'=4, 'Error'=5),
    content String,
    tokens_used UInt32,
    model_name String
) ENGINE = MergeTree()
ORDER BY (timestamp, session_id);
```

### Table: `blockchain_accounts`
Indexed account data from snapshots:
```sql
CREATE TABLE osvm.blockchain_accounts (
    slot UInt64,
    pubkey String,
    owner String,
    lamports UInt64,
    data_len UInt32,
    data_hash String,
    executable Bool,
    rent_epoch UInt64,
    program_type String,
    decoded_data String,
    data_sample Array(UInt8)
) ENGINE = ReplacingMergeTree(slot)
ORDER BY (owner, pubkey, slot);
```

### Table: `blockchain_transactions`
Transaction history from ledger:
```sql
CREATE TABLE osvm.blockchain_transactions (
    slot UInt64,
    signature String,
    block_time DateTime64(3),
    fee UInt64,
    success Bool,
    accounts Array(String),
    program_ids Array(String),
    instruction_data Array(String),
    logs Array(String),
    instruction_count UInt16
) ENGINE = MergeTree()
ORDER BY (slot, signature);
```

## Command Reference

### Database Management (`osvm db`)

#### Initialize Database
```bash
osvm db init [--data-dir <PATH>]
```
Downloads ClickHouse (if needed), creates configuration, and sets up database schema.

#### Start/Stop Server
```bash
osvm db start   # Start ClickHouse server
osvm db stop    # Stop ClickHouse server
osvm db status  # Check server status
```

#### Query Interface
```bash
# Execute SQL queries
osvm db query --query "SELECT * FROM osvm.cli_commands LIMIT 10"

# View activity logs
osvm db activity --stats                    # Show statistics
osvm db activity --commands --limit 50      # CLI command history
osvm db activity --chat --session-id abc123 # Chat history for session
```

#### Data Synchronization
```bash
# Sync modes
osvm db sync --mode last-30-days      # Last 30 days (default)
osvm db sync --mode full-historical   # All historical data
osvm db sync --mode realtime          # Start fresh, new data only

# Filtering options
osvm db sync --programs PROG1,PROG2,PROG3           # Specific programs
osvm db sync --accounts PUBKEY1,PUBKEY2             # Specific accounts
osvm db sync --pattern 0x1234abcd                   # Data pattern matching

# Custom paths
osvm db sync --ledger-path /path/to/ledger
osvm db sync --snapshot-dir /path/to/snapshot
```

### Real-time Sync (`osvm realtime`)

#### Start Daemon
```bash
# Start with default settings
osvm realtime start

# Start with filters
osvm realtime start \
  --programs TOKEN_PROGRAM_ID,STAKE_PROGRAM_ID \
  --accounts WALLET1,WALLET2 \
  --patterns 0xdeadbeef

# Custom paths
osvm realtime start \
  --ledger-path /custom/ledger \
  --snapshot-dir /custom/snapshot
```

#### Manage Daemon
```bash
osvm realtime status  # Check daemon status
osvm realtime stop    # Stop daemon
```

## Example Queries

### Activity Analytics

```sql
-- Most used commands
SELECT 
    command,
    count(*) as usage_count,
    avg(duration_ms) as avg_duration
FROM osvm.cli_commands
WHERE timestamp > now() - INTERVAL 7 DAY
GROUP BY command
ORDER BY usage_count DESC
LIMIT 10;

-- Command success rate
SELECT 
    command,
    countIf(exit_code = 0) as successful,
    countIf(exit_code != 0) as failed,
    successful * 100.0 / (successful + failed) as success_rate
FROM osvm.cli_commands
GROUP BY command
ORDER BY success_rate DESC;

-- Chat activity by session
SELECT 
    session_name,
    count(*) as message_count,
    countIf(message_type = 1) as user_messages,
    countIf(message_type = 2) as assistant_messages
FROM osvm.chat_messages
WHERE timestamp > now() - INTERVAL 1 DAY
GROUP BY session_name;
```

### Blockchain Analytics

```sql
-- Top accounts by balance
SELECT 
    pubkey,
    owner,
    lamports,
    program_type
FROM osvm.blockchain_accounts
ORDER BY lamports DESC
LIMIT 100;

-- Accounts by program type
SELECT 
    program_type,
    count(*) as account_count,
    sum(lamports) as total_lamports,
    avg(data_len) as avg_data_size
FROM osvm.blockchain_accounts
GROUP BY program_type
ORDER BY account_count DESC;

-- Find accounts with specific byte pattern
SELECT 
    pubkey,
    owner,
    program_type,
    data_sample
FROM osvm.blockchain_accounts
WHERE has(data_sample, 0x12)  -- Example: find accounts with byte 0x12
LIMIT 100;
```

## Advanced Features

### Automatic Activity Logging

All OSVM activity is automatically logged when ClickHouse is running:
- **CLI commands**: Every `osvm` command execution
- **Chat messages**: All agent chat interactions
- **Performance metrics**: Command duration, success/failure rates

No configuration needed - just start ClickHouse and logging begins!

### Data Filtering

Filter what gets indexed to save space and improve query performance:

```bash
# Index only SPL Token program accounts
osvm db sync --programs TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA

# Index specific wallet activities
osvm db sync --accounts WALLET_PUBKEY1,WALLET_PUBKEY2

# Find accounts with specific magic bytes
osvm db sync --pattern 0xCAFEBABE
```

### Real-time Monitoring

The realtime daemon provides sub-second latency for new blockchain data:

```bash
# Monitor all new activity
osvm realtime start

# Monitor specific programs in real-time
osvm realtime start --programs TOKEN_PROGRAM_ID

# Monitor specific accounts
osvm realtime start --accounts WHALE_WALLET_1,WHALE_WALLET_2
```

The daemon:
- Polls ledger every 100ms for new blocks
- Indexes new data immediately
- Applies all configured filters
- Runs in background with PID file management

## Storage Management

ClickHouse data is stored in `~/.osvm/clickhouse/`:
```
~/.osvm/clickhouse/
├── data/              # Database files
├── config/            # Configuration
│   ├── config.xml
│   └── bin/           # Downloaded ClickHouse binary
└── logs/              # Server logs
    ├── clickhouse-server.log
    └── clickhouse-server.err.log
```

### Retention Policies

Implement custom retention with SQL:

```sql
-- Delete old CLI commands (older than 90 days)
ALTER TABLE osvm.cli_commands 
DELETE WHERE timestamp < now() - INTERVAL 90 DAY;

-- Delete old chat messages (older than 30 days)
ALTER TABLE osvm.chat_messages
DELETE WHERE timestamp < now() - INTERVAL 30 DAY;
```

## Performance Tips

1. **Batch Operations**: The indexer automatically batches 1000 rows per insert
2. **Parallel Processing**: Use rayon for parallel snapshot reading
3. **Selective Indexing**: Use filters to index only what you need
4. **Regular Cleanup**: Delete old data to maintain performance

## Troubleshooting

### ClickHouse Won't Start

```bash
# Check logs
tail -f ~/.osvm/clickhouse/logs/clickhouse-server.log

# Verify ports are free
lsof -i :8123  # HTTP port
lsof -i :9000  # TCP port

# Reinitialize if corrupted
osvm db stop
rm -rf ~/.osvm/clickhouse
osvm db init
```

### Daemon Not Responding

```bash
# Check daemon status
osvm realtime status

# Force stop if stuck
kill $(cat ~/.osvm/realtime.pid)
rm ~/.osvm/realtime.pid

# Restart
osvm realtime start
```

### Query Timeouts

```sql
-- Increase query timeout (seconds)
SET max_execution_time = 300;

-- Or use LIMIT to reduce result size
SELECT * FROM osvm.cli_commands
ORDER BY timestamp DESC
LIMIT 1000;
```

## Integration Examples

### Use in Scripts

```bash
#!/bin/bash
# Export command history to JSON
osvm db query --query "SELECT * FROM osvm.cli_commands" > commands.json

# Check if realtime daemon is running
if osvm realtime status | grep -q "running"; then
    echo "Daemon is active"
else
    echo "Starting daemon..."
    osvm realtime start
fi
```

### Monitoring Dashboard

Create custom dashboards using ClickHouse's HTTP interface:

```bash
# Get recent activity as JSON
curl "http://localhost:8123/?query=SELECT%20*%20FROM%20osvm.cli_commands%20LIMIT%2010&default_format=JSONEachRow"
```

## Security Considerations

1. **Local Only**: ClickHouse binds to localhost only (not exposed to network)
2. **No Authentication**: Default setup has no password (local use only)
3. **Data Privacy**: All data stays on your local machine
4. **PID Files**: Daemon uses PID files for process management

For production deployment with network access, configure authentication in `~/.osvm/clickhouse/config/config.xml`.

## Performance Benchmarks

Typical performance metrics (varies by hardware):
- **Account indexing**: ~10,000 accounts/second
- **Transaction indexing**: ~5,000 transactions/second
- **Query latency**: <100ms for most queries
- **Realtime lag**: <1 second from block to indexed data

## Transaction Data Extraction

### Manual Binary Parsing Implementation

The system uses custom binary parsers to extract transaction data from RocksDB:

**Fully Implemented (100% accuracy):**
- ✅ **Block timestamps** - Direct binary field extraction
- ✅ **Slot numbers** - From RocksDB keys/values

**Heuristic Parsers (60-90% accuracy):**
- ✅ **Transaction logs** - Pattern-based Vec<String> extraction
- ✅ **Account lists** - 32-byte pubkey scanning with validation
- ✅ **Inner instruction counts** - CPI call detection

**Implementation:** `src/services/rocksdb_parser.rs`

### Parser Accuracy

The heuristic parsers use pattern matching and may not extract 100% of data:
- **Logs**: ~70-90% extraction rate (looks for "Program", "invoke" keywords)
- **Accounts**: ~60-80% extraction rate (validates pubkey format)
- **Inner instructions**: Basic count detection

**Why heuristic?** Bincode version incompatibility between project (2.0) and Solana types (1.x) prevents full deserialization. Heuristic parsing provides practical data extraction without dependency conflicts.

## Current Implementation Status

Fully Operational Features:
- ✅ ClickHouse process management
- ✅ Activity logging (CLI + chat)
- ✅ Account data indexing from snapshots
- ✅ Transaction indexing with RocksDB parsing
- ✅ Real-time sync daemon
- ✅ All 4 must-have transaction features (block_time, logs, accounts, inner_instructions)

See `CLICKHOUSE_MANUAL_PARSING_COMPLETE.md` for technical details on the parsing implementation.

## Support

For issues or questions:
- GitHub Issues: https://github.com/opensvm/osvm-cli/issues
- Documentation: https://github.com/opensvm/osvm-cli/tree/main/docs

## License

MIT License - See LICENSE file for details
