# Security Changes - 2025-10-13

## Critical Security Fix: Separated OSVM and Solana Keypair Directories

### Problem

OSVM was previously using Solana's default configuration directory (`~/.config/solana/`) for its keypairs, which created a serious security risk:
- Commands could accidentally overwrite user's main Solana keypair
- No isolation between OSVM's operational keys and user's valuable keys
- Risk of catastrophic data loss if OSVM code had bugs

### Incident

On 2025-10-13, an AI assistant accidentally ran:
```bash
solana-keygen new --force --outfile ~/.config/solana/id.json
```

This **permanently overwrote** the user's main Solana keypair with no possibility of recovery. The original file had been created on 2025-06-13 and was the **only copy**.

### Solution

**OSVM now uses a completely separate directory: `~/.config/osvm/`**

#### Changes Made

1. **Default Keypair Location Changed**
   - Old: `~/.config/solana/id.json`
   - New: `~/.config/osvm/keypair.json`

2. **Code Changes**
   - `src/config.rs`: Changed default keypair path to use `~/.config/osvm/keypair.json`
   - `src/utils/self_repair/user_deps.rs`: Added `get_osvm_config_dir()` method
   - `src/utils/self_repair/user_deps.rs`: Updated `get_default_keypair_path()` to use OSVM directory
   - `src/utils/mainnet_rpc.rs`: Changed hardcoded path to OSVM directory

3. **Configuration Files Updated**
   - `.claude_rules` - Claude Code rules
   - `.clinerules` - Cline rules
   - `.cursorrules` - Cursor rules
   - `.github/copilot-instructions.md` - GitHub Copilot rules
   - `CLAUDE.md` - Main development documentation
   - `docs/NEVER-MODIFY-SOLANA-CONFIG.md` - Dedicated security documentation

### Benefits

1. **Complete Isolation**: OSVM's keypairs are completely separate from user's main Solana keypairs
2. **No Risk of Overwrite**: Even if OSVM has bugs, it can't touch `~/.config/solana/`
3. **Clear Separation**: Users can easily distinguish between their personal Solana keys and OSVM's operational keys
4. **Better Security**: Users can have different backup strategies for different key types

### Migration

#### For Existing Users

If you have an existing OSVM setup, your old keypair in `~/.config/solana/` will still work if you explicitly specify it:

```bash
# Option 1: Use explicit --keypair flag (recommended)
osvm --keypair ~/.config/solana/id.json <command>

# Option 2: Create new OSVM-specific keypair (safer)
mkdir -p ~/.config/osvm
solana-keygen new --no-bip39-passphrase --outfile ~/.config/osvm/keypair.json
osvm <command>  # Will use new default path
```

#### For New Users

OSVM will automatically use `~/.config/osvm/keypair.json`. If it doesn't exist, you'll need to create it:

```bash
mkdir -p ~/.config/osvm
solana-keygen new --no-bip39-passphrase --outfile ~/.config/osvm/keypair.json
```

### Directory Structure

```
~/.config/
├── solana/                      # User's main Solana configuration
│   ├── id.json                  # ⚠️  NEVER TOUCH - User's main keypair
│   └── cli/
│       └── config.yml           # ⚠️  NEVER TOUCH - Solana CLI config
│
└── osvm/                        # OSVM's isolated configuration
    ├── config.yml               # OSVM configuration
    ├── keypair.json             # OSVM's default keypair
    ├── mcp_config.json          # MCP server configuration
    ├── logs/                    # OSVM logs
    └── ledgers/                 # Blockchain data
```

### AI Assistant Rules

All AI coding assistants have been configured with strict rules to:
- **NEVER** modify files in `~/.config/solana/`
- **ALWAYS** use temporary keypairs in `/tmp/` for testing
- **ALWAYS** use explicit `--keypair` flags
- **ASK** user before creating any keypair files

### Testing

```bash
# Test that OSVM uses new default path
./target/release/osvm --help | grep keypair

# Verify new path is used
mkdir -p ~/.config/osvm
touch ~/.config/osvm/keypair.json  # Create empty file
./target/release/osvm balance 2>&1 | head -5  # Should try to read from ~/.config/osvm/

# Clean up test
rm ~/.config/osvm/keypair.json
```

### Related Documentation

- `docs/NEVER-MODIFY-SOLANA-CONFIG.md` - Comprehensive security rules
- `.claude_rules` - Claude Code rules
- `.clinerules` - Cline rules
- `.cursorrules` - Cursor rules
- `.github/copilot-instructions.md` - GitHub Copilot rules
- `CLAUDE.md` - Main development guide

### Lessons Learned

1. **Defense in Depth**: Use separate directories for different security domains
2. **Never Trust Defaults**: Don't rely on user's configuration directories
3. **Document Security Rules**: Put security rules in multiple places for visibility
4. **Test with Temporary Files**: Always use `/tmp/` for testing
5. **Explicit is Better**: Use explicit `--keypair` flags instead of defaults

### Acknowledgment

This change was made in response to a real incident where an AI assistant accidentally overwrote a user's Solana keypair. The incident highlighted the need for better isolation and clearer security boundaries.

---

## RPC Configuration Updates - Anza Docs Alignment

### Problem

OSVM's devnet RPC implementation was failing to start in NAT'd environments due to strict UDP port reachability checks. The validator would:
1. Successfully connect to entrypoints
2. Obtain shred version
3. Fail UDP port reachability tests
4. Exit after max retries

### Root Cause

After reviewing the official Anza documentation (https://docs.anza.xyz/clusters/available), we discovered:
- **Missing `--no-port-check` flag**: Anza recommends this for non-voting RPC nodes
- **Incorrect port range**: Using `8002-8020` instead of Anza's recommended `8000-8020`
- **Missing WAL recovery**: Mainnet was missing the recommended `--wal-recovery-mode skip_any_corrupted_record`

### Changes Made

#### Devnet RPC (`src/utils/devnet_rpc.rs`)
1. **Added `--no-port-check` flag** (line 292)
   - Critical for NAT'd environments
   - Skips UDP port reachability tests for non-voting nodes
   - Follows Anza's recommendation for RPC-only deployments

2. **Fixed dynamic port range** (line 258)
   - Old: `8002-8020`
   - New: `8000-8020` (Anza recommended)

#### Mainnet RPC (`src/utils/mainnet_rpc.rs`)
1. **Fixed dynamic port range** (line 115)
   - Old: `8002-8020`
   - New: `8000-8020` (Anza recommended)

2. **Added WAL recovery mode** (line 158-159)
   - Added: `--wal-recovery-mode skip_any_corrupted_record`
   - Brings mainnet in line with devnet and Anza recommendations

### Anza Documentation Reference

From https://docs.anza.xyz/clusters/available:

**Common Validator Configuration Recommendations:**
- Use `--known-validator` flags ✅ (already implemented)
- Set `--rpc-port 8899` ✅ (already implemented)
- Configure `--dynamic-port-range 8000-8020` ✅ (now fixed)
- Use `--wal-recovery-mode skip_any_corrupted_record` ✅ (now fixed)
- Limit ledger size ✅ (already implemented)

**Devnet Configuration:**
- RPC URL: `https://api.devnet.solana.com` ✅
- Gossip Entrypoint: `entrypoint.devnet.solana.com:8001` ✅
- Genesis Hash: `EtWTRABZaYq6iMfeYKouRu166VU2xqa1wcaWoxPkrZBG` ✅
- Known validators: dv1Z..., dv2e..., dv4A..., dv3q... ✅

**Mainnet Beta Configuration:**
- RPC URL: `https://api.mainnet-beta.solana.com` ✅
- Gossip Entrypoint: `entrypoint.mainnet-beta.solana.com:8001` ✅
- Genesis Hash: `5eykt4UsFv8P8NJdTREpY1vzqKqZKvdpKuc147dw2N9d` ✅

### Benefits

1. **NAT Compatibility**: Devnet RPC now works behind NAT without public UDP ports
2. **Standards Compliance**: Both devnet and mainnet follow Anza's official recommendations
3. **Better Reliability**: WAL recovery mode helps handle corrupted records gracefully
4. **Correct Port Range**: Uses full recommended range for dynamic ports

### Testing Required

```bash
# Build with new changes
cargo build --release

# Test devnet RPC with --no-port-check
./target/release/osvm rpc devnet

# Verify it connects and doesn't fail on port checks
# Expected: Should successfully start syncing without UDP port errors
```

### Future Work

- Add testnet support (currently missing entirely)
- Consider adding automatic cluster selection based on network conditions
- Implement better error messages when ports are unavailable

---

**Version**: 1.1
**Date**: 2025-10-13
**Severity**: CRITICAL (Keypair), HIGH (RPC Config)
**Status**: IMPLEMENTED
**Tested**: ⏳ Pending (RPC changes need testing)
