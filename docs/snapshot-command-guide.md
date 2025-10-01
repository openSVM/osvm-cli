# OSVM Snapshot Command - Complete User Guide

## Overview

The `osvm snapshot` command provides comprehensive tools for analyzing, comparing, and exporting Solana blockchain snapshots. This guide covers all features and usage examples.

## Quick Start

```bash
# Show snapshot statistics
osvm snapshot stats

# Read first 10 accounts
osvm snapshot read --limit 10

# Get help
osvm snapshot --help
osvm snapshot read --help
```

## Commands

### 1. `read` - Read and Display Accounts

Display snapshot accounts with advanced filtering and formatting options.

**Basic Usage:**
```bash
# Read all accounts (with default snapshot directory)
osvm snapshot read

# Read specific snapshot
osvm snapshot read --snapshot-dir /path/to/snapshot

# Read first 100 accounts
osvm snapshot read --limit 100

# Skip first 50, read next 100
osvm snapshot read --offset 50 --limit 100
```

**Filtering:**
```bash
# Filter by owner program
osvm snapshot read --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA

# Filter by balance range
osvm snapshot read --filter-min-balance 1000000000  # Min 1 SOL
osvm snapshot read --filter-max-balance 100000000000  # Max 100 SOL

# Filter by data size
osvm snapshot read --filter-min-size 1024  # At least 1KB of data
osvm snapshot read --filter-max-size 10240  # At most 10KB

# Combine filters
osvm snapshot read \
  --filter-owner Token \
  --filter-min-balance 1000000 \
  --limit 50
```

**Performance:**
```bash
# Enable parallel processing (uses all CPU cores)
osvm snapshot read --parallel

# Custom thread count
osvm snapshot read --parallel --threads 4

# Process large snapshot efficiently
osvm snapshot read --parallel --threads 8 --quiet
```

**Output Modes:**
```bash
# Quiet mode (no headers/footers)
osvm snapshot read --quiet --limit 10

# JSON output for scripting
osvm snapshot read --json --limit 5

# Disable colors
osvm snapshot read --no-color
```

### 2. `stats` - Show Statistics

Display comprehensive snapshot statistics including totals, averages, and top accounts.

```bash
# Show statistics
osvm snapshot stats

# Custom snapshot directory
osvm snapshot stats --snapshot-dir /path/to/snapshot

# JSON output
osvm snapshot stats --json
```

**Output Includes:**
- Total accounts and lamports
- Average and median balances
- Total and average data sizes
- Top 10 programs by account count
- Top 10 largest accounts by balance
- Executable and rent-exempt account counts

### 3. `export` - Export to Various Formats

Export snapshot data to different file formats for analysis.

**Export to JSON:**
```bash
osvm snapshot export --output accounts.json

# With filtering
osvm snapshot export \
  --output token_accounts.json \
  --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
```

**Export to CSV:**
```bash
osvm snapshot export --output accounts.csv --format csv

# Whale accounts only
osvm snapshot export \
  --output whales.csv \
  --format csv \
  --filter-min-balance 100000000000
```

**Export to MessagePack:**
```bash
# Efficient binary format
osvm snapshot export --output accounts.msgpack --format msgpack
```

**Supported Formats:**
- `json` - Human-readable JSON
- `csv` - Spreadsheet-compatible CSV
- `msgpack` - Binary MessagePack (efficient)
- `parquet` - Apache Parquet (coming soon)

### 4. `compare` - Compare Two Snapshots

Compare two snapshots to identify changes between them.

```bash
# Compare two snapshots
osvm snapshot compare \
  ~/.config/osvm/ledgers/devnet/snapshot1 \
  ~/.config/osvm/ledgers/devnet/snapshot2

# JSON output for automation
osvm snapshot compare snapshot1/ snapshot2/ --json
```

**Output Shows:**
- New accounts created
- Deleted accounts
- Modified accounts with balance/data changes
- Summary statistics

### 5. `validate` - Validate Snapshot Integrity

Check snapshot for corruption, inconsistencies, and suspicious patterns.

```bash
# Validate default snapshot
osvm snapshot validate

# Validate specific snapshot
osvm snapshot validate --snapshot-dir /path/to/snapshot

# JSON output
osvm snapshot validate --json
```

**Validation Checks:**
- File readability
- Data structure consistency
- Balance reasonability checks
- Data length validation
- Snapshot directory structure

### 6. `find` - Find Specific Account

Search for a specific account by its public key.

```bash
# Find account by pubkey
osvm snapshot find 7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZRuJosgAsU

# Custom snapshot
osvm snapshot find <PUBKEY> --snapshot-dir /path/to/snapshot

# JSON output
osvm snapshot find <PUBKEY> --json
```

### 7. `interactive` - Interactive TUI Mode

Launch interactive terminal UI for snapshot exploration (coming soon).

```bash
osvm snapshot interactive
```

**Planned Features:**
- Arrow key navigation
- Real-time filtering
- Account inspection
- Search functionality
- Sort by various fields

## Advanced Examples

### Whale Hunting

Find and analyze large accounts:

```bash
# Find whales (>100 SOL)
osvm snapshot read --filter-min-balance 100000000000 --limit 20

# Export whale list to CSV
osvm snapshot export \
  --output whales.csv \
  --format csv \
  --filter-min-balance 100000000000

# Get statistics on whales only
osvm snapshot read \
  --filter-min-balance 100000000000 \
  --json | jq 'length'
```

### Token Account Analysis

Analyze all token accounts:

```bash
# Find all SPL Token accounts
osvm snapshot read \
  --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA \
  --limit 100

# Export token accounts
osvm snapshot export \
  --output token_accounts.json \
  --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
```

### Program Account Discovery

Find accounts owned by specific programs:

```bash
# Find Metaplex accounts
osvm snapshot read --filter-owner metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s

# Find System Program accounts
osvm snapshot read --filter-owner 11111111111111111111111111111111
```

### Automated Analysis Pipeline

```bash
# Export to JSON, process with jq, store results
osvm snapshot read --json --limit 1000 | \
  jq '[.[] | select(.lamports > 1000000000)]' > rich_accounts.json

# Validate and export in one go
if osvm snapshot validate --json | jq -e '.valid'; then
  osvm snapshot export --output backup.json
  echo "Snapshot validated and exported successfully"
fi
```

### Performance Benchmarking

```bash
# Benchmark sequential processing
time osvm snapshot read --quiet --limit 1000

# Benchmark parallel processing
time osvm snapshot read --quiet --limit 1000 --parallel --threads 8

# Compare performance
for threads in 1 2 4 8; do
  echo "Testing with $threads threads:"
  time osvm snapshot read --quiet --limit 5000 --parallel --threads $threads
done
```

### Snapshot Monitoring

```bash
# Monitor snapshot changes over time
while true; do
  osvm snapshot stats --json > "stats_$(date +%s).json"
  sleep 3600  # Every hour
done

# Compare consecutive snapshots
osvm snapshot compare \
  snapshot_morning/ \
  snapshot_evening/ > daily_changes.json
```

## Output Formats

### Text Output (Default)

Colorized, human-readable format:

```
=== Solana Snapshot Account Stream ===
Snapshot Directory: "/home/user/.config/osvm/ledgers/devnet/remote/extracted"
Total accounts: 7264
Processing: 10 accounts
================================================================================

Account #1 (355539392.39219)
  Lamports: 18374686479671629389 lamports
  Data Length: 82 bytes
  Owner: 8hgRQ1LcGXUgjxeXvV5To7uTJjdMjRQ7CVLKLk7x5FD
  Executable: false
  Rent Epoch: 504403158265495551
```

### JSON Output

Machine-readable format for automation:

```json
{
  "pubkey": null,
  "lamports": 18374686479671629389,
  "data_len": 82,
  "owner": "8hgRQ1LcGXUgjxeXvV5To7uTJjdMjRQ7CVLKLk7x5FD",
  "executable": false,
  "rent_epoch": 504403158265495551,
  "data": [...]
}
```

## Tips & Best Practices

### Performance Optimization

1. **Use Parallel Processing**: For large snapshots, always use `--parallel`
2. **Limit Output**: Use `--limit` and `--offset` to process in batches
3. **Quiet Mode**: Use `--quiet` when you don't need progress bars
4. **Filter Early**: Apply filters to reduce data processing

### Scripting Integration

```bash
# Check if snapshot is valid before processing
if osvm snapshot validate --json | jq -e '.valid'; then
  osvm snapshot export --output data.json
fi

# Count accounts by owner
osvm snapshot read --json | \
  jq 'group_by(.owner) | map({owner: .[0].owner, count: length})'

# Find total lamports
osvm snapshot stats --json | jq '.total_lamports'
```

### Troubleshooting

**Snapshot not found:**
```bash
# Specify custom snapshot directory
osvm snapshot read --snapshot-dir /path/to/snapshot
```

**Out of memory:**
```bash
# Process in smaller batches
osvm snapshot read --limit 1000 --offset 0
osvm snapshot read --limit 1000 --offset 1000
```

**Slow performance:**
```bash
# Enable parallel processing
osvm snapshot read --parallel --threads 8
```

## Integration with Other Tools

### With jq

```bash
# Extract owner addresses
osvm snapshot read --json --limit 100 | jq '.[].owner' | sort | uniq

# Calculate average balance
osvm snapshot stats --json | jq '.average_balance'
```

### With Python

```python
import subprocess
import json

# Get snapshot data
result = subprocess.run(
    ['osvm', 'snapshot', 'read', '--json', '--limit', '100'],
    capture_output=True, text=True
)
accounts = json.loads(result.stdout)

# Analyze
total_balance = sum(acc['lamports'] for acc in accounts)
print(f"Total balance: {total_balance} lamports")
```

### With Shell Scripts

```bash
#!/bin/bash

# Daily snapshot backup script
DATE=$(date +%Y%m%d)
SNAPSHOT_DIR=~/.config/osvm/ledgers/devnet/remote/extracted
OUTPUT_DIR=~/snapshot_backups

# Validate
if ! osvm snapshot validate --snapshot-dir "$SNAPSHOT_DIR" --json | jq -e '.valid'; then
  echo "❌ Snapshot validation failed"
  exit 1
fi

# Export
osvm snapshot export \
  --snapshot-dir "$SNAPSHOT_DIR" \
  --output "$OUTPUT_DIR/snapshot_$DATE.json"

# Get stats
osvm snapshot stats --snapshot-dir "$SNAPSHOT_DIR" --json > "$OUTPUT_DIR/stats_$DATE.json"

echo "✅ Snapshot backed up successfully"
```

## Global Flags

All snapshot commands support these global flags:

- `--no-color` - Disable colored output
- `--verbose` / `-v` - Increase verbosity (-v, -vv, -vvv)
- `--debug` - Show debug information
- `--config <PATH>` - Use custom config file
- `--keypair <PATH>` - Use custom keypair
- `--url <URL>` - Use custom RPC URL

## Default Behavior

- **Snapshot Directory**: `~/.config/osvm/ledgers/devnet/remote/extracted`
- **Output Format**: Colorized text
- **Processing**: Sequential (use `--parallel` for parallel)
- **Progress**: Enabled (use `--quiet` to disable)

## Performance Characteristics

| Operation | Sequential | Parallel (8 cores) |
|-----------|-----------|-------------------|
| Read 1000 accounts | ~2-3s | ~0.5-1s |
| Read 10000 accounts | ~20-30s | ~4-6s |
| Statistics | ~5s | ~1s |
| Export to JSON | ~6s | ~1.5s |
| Export to CSV | ~7s | ~2s |

## Error Handling

The command provides clear error messages:

```bash
# Missing snapshot
❌ Snapshot directory does not exist: "/invalid/path"

# Invalid filter
❌ Failed to parse filter value

# Permission denied
❌ Error reading account file: Permission denied
```

## Future Enhancements

Planned features marked with 🔜:

- 🔜 Interactive TUI with arrow key navigation
- 🔜 Parquet export format
- 🔜 Account data decoders (Token, Stake, Vote accounts)
- 🔜 Search index for instant lookups
- 🔜 Checksum validation
- 🔜 Integration with solana-runtime AppendVec parsers

## Support

For issues or feature requests:
- GitHub: https://github.com/openSVM/osvm-cli/issues
- Documentation: https://github.com/openSVM/osvm-cli/tree/main/docs

## See Also

- `osvm doctor` - System health check
- `osvm rpc-manager` - RPC node management
- `osvm audit` - Security auditing
- `osvm mcp` - MCP server integration
