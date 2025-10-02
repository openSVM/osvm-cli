# OSVM Snapshot Command - Quick Reference

## One-Liners for Common Tasks

### Basic Operations
```bash
osvm snapshot stats                                    # Show statistics
osvm snapshot read --limit 10                          # Preview first 10 accounts
osvm snapshot validate                                 # Check snapshot integrity
osvm snapshot export --output backup.json              # Export to JSON
```

### Filtering
```bash
osvm snapshot read --filter-owner Token --limit 50     # Token accounts
osvm snapshot read --filter-min-balance 1000000000     # Accounts with ≥1 SOL
osvm snapshot export --output big.csv --format csv --filter-min-size 10000  # Large accounts to CSV
```

### Performance
```bash
osvm snapshot read --parallel --threads 8              # Fast parallel processing
osvm snapshot stats --quiet                            # No progress bars
osvm snapshot read --json --limit 100 > data.json      # Scriptable output
```

### Analysis
```bash
osvm snapshot compare snapshot1/ snapshot2/            # Compare snapshots
osvm snapshot find 7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZRuJosgAsU  # Find account
```

## Command Matrix

| Task | Command |
|------|---------|
| Get statistics | `osvm snapshot stats` |
| Read accounts | `osvm snapshot read [OPTIONS]` |
| Export data | `osvm snapshot export --output <FILE> [OPTIONS]` |
| Compare snapshots | `osvm snapshot compare <SNAP1> <SNAP2>` |
| Validate snapshot | `osvm snapshot validate` |
| Find account | `osvm snapshot find <PUBKEY>` |
| Interactive mode | `osvm snapshot interactive` |

## Filter Options

| Filter | Flag | Example |
|--------|------|---------|
| Owner program | `--filter-owner <PUBKEY>` | `--filter-owner TokenkegQ...` |
| Min balance | `--filter-min-balance <N>` | `--filter-min-balance 1000000000` |
| Max balance | `--filter-max-balance <N>` | `--filter-max-balance 100000000000` |
| Min data size | `--filter-min-size <N>` | `--filter-min-size 1024` |
| Max data size | `--filter-max-size <N>` | `--filter-max-size 10240` |
| Executable only | `--filter-executable` | `--filter-executable` |
| Rent-exempt only | `--filter-rent-exempt` | `--filter-rent-exempt` |

## Export Formats

| Format | Extension | Use Case |
|--------|-----------|----------|
| JSON | `.json` | General purpose, human-readable |
| CSV | `.csv` | Spreadsheet analysis |
| MessagePack | `.msgpack` | Efficient storage/transfer |
| Parquet | `.parquet` | Big data tools (coming soon) |

## Performance Flags

| Flag | Description | Example |
|------|-------------|---------|
| `--parallel` | Enable multi-threading | `osvm snapshot read --parallel` |
| `--threads <N>` | Set thread count | `--parallel --threads 4` |
| `--quiet` | No progress bars | `--quiet` |
| `--limit <N>` | Limit output | `--limit 100` |
| `--offset <N>` | Skip first N | `--offset 1000` |

## Output Modes

| Flag | Description |
|------|-------------|
| `--json` | JSON format |
| `--quiet` | Minimal output |
| `--no-color` | Disable colors |

## Use Cases & Solutions

### "Find all whale accounts"
```bash
osvm snapshot read --filter-min-balance 100000000000 --limit 20
```

### "Export token accounts to CSV"
```bash
osvm snapshot export --output tokens.csv --format csv \
  --filter-owner TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
```

### "Compare two snapshots"
```bash
osvm snapshot compare \
  ~/.config/osvm/ledgers/devnet/snapshot_old \
  ~/.config/osvm/ledgers/devnet/snapshot_new
```

### "Validate before processing"
```bash
if osvm snapshot validate --json | jq -e '.valid'; then
  osvm snapshot export --output validated_data.json
fi
```

### "Fast parallel analysis"
```bash
time osvm snapshot read --parallel --threads 8 --quiet --limit 10000
```

### "Find specific account"
```bash
osvm snapshot find 7xKXtg2CW87d97TXJSDpbD5jBkheTqA83TZRuJosgAsU
```

## Scripting Examples

### Bash
```bash
#!/bin/bash
# Get total lamports
total=$(osvm snapshot stats --json | jq '.total_lamports')
echo "Total SOL: $(echo "scale=2; $total / 1000000000" | bc)"
```

### Python
```python
import subprocess, json
result = subprocess.run(['osvm', 'snapshot', 'stats', '--json'], 
                       capture_output=True, text=True)
stats = json.loads(result.stdout)
print(f"Total accounts: {stats['total_accounts']}")
```

### Node.js
```javascript
const { execSync } = require('child_process');
const stats = JSON.parse(
  execSync('osvm snapshot stats --json').toString()
);
console.log(`Total lamports: ${stats.total_lamports}`);
```

## Tips

💡 **Performance**: Use `--parallel` for snapshots with >1000 accounts  
💡 **Memory**: Use `--limit` and `--offset` for very large snapshots  
💡 **Automation**: Always use `--json` for scripting  
💡 **Quality**: Run `validate` before important operations  
💡 **Monitoring**: Use `compare` to track snapshot changes over time  

## Error Messages

| Error | Solution |
|-------|----------|
| "Snapshot directory does not exist" | Check path or use `--snapshot-dir` |
| "Accounts directory not found" | Ensure snapshot is fully extracted |
| "File too small to be a valid account" | Corrupted snapshot, re-download |
| "Permission denied" | Check file permissions |

## See Also

- Full Guide: `docs/snapshot-command-guide.md`
- Implementation: `SNAPSHOT_IMPROVEMENTS_IMPLEMENTED.md`
- Original Analysis: `SNAPSHOT_ANALYSIS.md`
