# 🚨 CRITICAL SECURITY RULE: NEVER MODIFY SOLANA CONFIGURATION FILES

## ⚠️ THIS IS THE MOST IMPORTANT RULE IN THE PROJECT ⚠️

**READ THIS BEFORE MAKING ANY CODE CHANGES**

## The Rule

**NEVER create, modify, or delete Solana configuration files:**
- `~/.config/solana/id.json` - **NEVER TOUCH THIS FILE**
- `~/.config/solana/cli/config.yml` - **NEVER TOUCH THIS FILE**
- Any `.json` files in `~/.config/solana/` - **NEVER TOUCH THESE FILES**

## Why This Matters

These files contain **cryptographic private keys** that:

1. **Cannot be recovered if overwritten or deleted**
2. **May not have backups** - users often don't save seed phrases
3. **Control access to funds** - potentially millions of dollars
4. **Are unique and irreplaceable** - like losing the only key to a safe

### Real Consequences of Violating This Rule

- ❌ Permanent loss of blockchain identity
- ❌ Irreversible loss of all funds in associated accounts
- ❌ Loss of access to deployed programs and their upgrade authority
- ❌ Loss of validator identity and stake
- ❌ Complete destruction of user's trust in the tool
- ❌ Potential legal liability

## What Happened

On 2025-10-13, an AI assistant made the catastrophic mistake of running:

```bash
solana-keygen new --force --outfile ~/.config/solana/id.json
```

This **permanently** overwrote the user's keypair. The file had been created on 2025-06-13 and was the **only copy**. There is **NO WAY** to recover it.

## The Correct Way to Test

### Always Use Temporary Keypairs

```bash
# ✅ CORRECT APPROACH
# Step 1: Create temporary test keypair
TMP_KEYPAIR="/tmp/test-keypair-$(date +%s).json"
solana-keygen new --no-bip39-passphrase --outfile "$TMP_KEYPAIR"

# Step 2: Use explicit --keypair flag in ALL commands
osvm --keypair "$TMP_KEYPAIR" balance
osvm rpc devnet --keypair "$TMP_KEYPAIR" --background
cargo run -- --keypair "$TMP_KEYPAIR" <command>

# Step 3: Clean up when done
rm -f "$TMP_KEYPAIR"
```

### In Rust Tests

```rust
#[test]
fn test_with_keypair() {
    // Create temporary directory and keypair
    let temp_dir = tempfile::tempdir().unwrap();
    let keypair_path = temp_dir.path().join("test-keypair.json");

    // Generate test keypair
    let keypair = Keypair::new();
    write_keypair_file(&keypair, &keypair_path).unwrap();

    // Use explicit keypair path
    let config = Config {
        keypair_path: Some(keypair_path),
        ..Default::default()
    };

    // Run test
    let result = function_under_test(&config);

    // Cleanup automatic via tempfile
}
```

## What NEVER To Do

```bash
# ❌ CATASTROPHIC - Overwrites user's keypair forever
solana-keygen new --force --outfile ~/.config/solana/id.json

# ❌ DANGEROUS - Uses default location (may fail and tempt overwriting)
osvm rpc devnet

# ❌ WRONG - Modifying config without explicit permission
solana config set --keypair ~/.config/solana/id.json

# ❌ WRONG - Creating files in user's Solana directory
touch ~/.config/solana/backup.json

# ❌ WRONG - Any operation with --force on user's config
solana-keygen new --force --outfile <any-user-path>
```

## Decision Tree: What To Do When Keypair is Missing

```
Command fails due to missing keypair?
    |
    ├─> Is this a test/development scenario?
    |   ├─> YES: Create temporary keypair in /tmp/
    |   |        Use explicit --keypair /tmp/test-keypair.json
    |   |        ✅ SAFE
    |   |
    |   └─> NO: This is production/user's actual usage
    |           ├─> STOP immediately
    |           ├─> ASK user what they want to do
    |           ├─> Suggest: solana-keygen new --outfile /tmp/test-keypair.json
    |           └─> NEVER create/modify files in ~/.config/solana/
    |
    └─> User explicitly asks to create keypair?
        ├─> Confirm: "This will create a NEW keypair. Do you have your existing seed phrase backed up?"
        ├─> Suggest: Use `solana-keygen new` (without --force)
        ├─> NEVER use --force flag
        └─> Let user run the command themselves
```

## Code Review Checklist

Before committing ANY code that touches Solana configuration:

- [ ] Does this code modify `~/.config/solana/id.json`? → **BLOCK**
- [ ] Does this code modify `~/.config/solana/cli/config.yml`? → **BLOCK**
- [ ] Does this code use `--force` flag with solana-keygen? → **BLOCK**
- [ ] Does this code create keypairs without explicit user request? → **BLOCK**
- [ ] Does this code use default keypair paths in tests? → **BLOCK**
- [ ] Are all test keypairs in `/tmp/` directory? → **REQUIRED**
- [ ] Are all commands using explicit `--keypair` flag? → **REQUIRED**
- [ ] Is there user confirmation before any keypair operations? → **REQUIRED**

## For AI Assistants

If you are an AI coding assistant (Claude, Copilot, Cursor, Cline, etc.):

### DO THIS:
1. ✅ Always create test keypairs in `/tmp/` directory
2. ✅ Always use explicit `--keypair /tmp/test-keypair.json` flag
3. ✅ Ask user for permission before any keypair operations
4. ✅ Suggest temporary keypairs for testing
5. ✅ Clean up temporary files when done

### NEVER DO THIS:
1. ❌ Never modify files in `~/.config/solana/`
2. ❌ Never use `--force` flag with user's keypair location
3. ❌ Never assume it's safe to create/overwrite keypair files
4. ❌ Never use default keypair paths in examples or tests
5. ❌ Never run solana-keygen on user's actual keypair without explicit permission

## Emergency: What If I Violated This Rule?

If you have overwritten a user's keypair:

1. **Stop immediately** - don't make it worse
2. **Inform the user immediately** - be honest about what happened
3. **Check for backups**:
   - System backups (Time Machine, rsync, etc.)
   - Cloud storage backups
   - Git history (if accidentally committed)
   - File system recovery tools (may work if just overwritten)
4. **Try file recovery** (may not work):
   ```bash
   # Install recovery tools
   sudo apt install extundelete testdisk photorec

   # Check if file system supports recovery
   # This rarely works with ext4 after overwrite
   ```
5. **Ask user for seed phrase** - they may have it written down
6. **Document the incident** for future prevention

## Related Files

This rule is documented in multiple places for maximum visibility:

- `/.claude_rules` - Claude Code rules
- `/.clinerules` - Cline rules
- `/.cursorrules` - Cursor rules
- `/.github/copilot-instructions.md` - GitHub Copilot rules
- `/CLAUDE.md` - Main development documentation
- `/docs/NEVER-MODIFY-SOLANA-CONFIG.md` - This file

## Summary

**NEVER modify Solana configuration files. ALWAYS use temporary test keypairs in /tmp/. ALWAYS use explicit --keypair flags.**

This is not negotiable. This is not optional. This is the most important rule in the project.

---

**Last Updated**: 2025-10-13
**Reason**: Catastrophic user data loss incident
**Severity**: CRITICAL
**Non-Compliance**: Project termination
