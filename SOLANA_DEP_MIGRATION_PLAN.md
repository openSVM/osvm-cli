# Solana Dependency Migration Plan

## ✅ COMPLETED: 2025-11-27

### Results

| Metric | Before | After |
|--------|--------|-------|
| Security vulnerabilities | 5 | 1 (just "unmaintained" warning) |
| clap 2.x in tree | Yes | **Eliminated** |
| Direct Solana deps | 11 | 9 |
| Total crates | 3053 | 2949 |
| Solana crate references | 582 | 559 |

### What Was Done

1. **Removed `config` crate** - Was unused, pulling in json5 vulnerability
2. **Replaced `solana-clap-utils`** with `src/utils/validators.rs` (~100 lines)
3. **Replaced `solana-cli-config`** with `src/utils/config_loader.rs` (~150 lines)

### Remaining Warning

Only `paste` (RUSTSEC-2024-0436) remains - this is "unmaintained" not a security vulnerability.
Used by: ratatui, image, rmp-serde, nalgebra. Cannot remove without major rewrites.

---

## Original Analysis (for reference)

## Current State
- 11 direct Solana dependencies
- 564+ transitive dependencies from solana-sdk alone
- 4 security warnings, all from solana-clap-utils → clap 2.x → ansi_term/atty

## Dependencies Analysis

### KEEP (Essential)
| Dependency | Transitive Deps | Why Keep |
|------------|-----------------|----------|
| `solana-client` | ~200 | RPC client for blockchain queries - no replacement |
| `solana-sdk` | ~564 | Core crypto types (Pubkey, Signature, Keypair) - partially needed |
| `solana-commitment-config` | ~10 | CommitmentConfig enum - could inline but tight coupling |

### ELIMINATE (Quick Wins)
| Dependency | Lines of Replacement | Savings | Priority |
|------------|---------------------|---------|----------|
| `solana-clap-utils` | ~50 | Removes ALL vulnerabilities | **HIGH** |
| `solana-cli-config` | ~30 | Config file parsing | Medium |
| `solana-logger` | ~5 | Use env_logger directly | Medium |
| `solana-sdk-ids` | 0 | Not used directly | **HIGH** |

### CONDITIONAL (Feature-Gated)
| Dependency | Usage | Recommendation |
|------------|-------|----------------|
| `solana-program` | Audit tests only | Move to dev-dependencies |
| `solana-loader-v3-interface` | ebpf_deploy.rs only | Feature-gate |
| `solana-system-interface` | ebpf_deploy.rs only | Feature-gate |
| `solana-transaction-status` | 4 uses | Inline UiTransactionEncoding |
| `solana-remote-wallet` | Already optional | Keep as-is |

## Migration Phases

### Phase 1: Eliminate Vulnerabilities (Day 1)
**Goal:** Remove solana-clap-utils, eliminate all security warnings

1. Create `src/utils/validators.rs`:
```rust
/// URL monikers for Solana networks
pub fn normalize_to_url_if_moniker(url_or_moniker: &str) -> String {
    match url_or_moniker {
        "m" | "mainnet-beta" => "https://api.mainnet-beta.solana.com".into(),
        "t" | "testnet" => "https://api.testnet.solana.com".into(),
        "d" | "devnet" => "https://api.devnet.solana.com".into(),
        "l" | "localhost" => "http://localhost:8899".into(),
        other => other.into(),
    }
}

pub fn is_url_or_moniker(s: &str) -> Result<(), String> {
    let url = normalize_to_url_if_moniker(s);
    url::Url::parse(&url)
        .map_err(|e| format!("Invalid URL: {}", e))?
        .host()
        .ok_or_else(|| "URL has no host".to_string())?;
    Ok(())
}

pub fn is_valid_signer(s: &str) -> Result<(), String> {
    // Accept pubkey strings (base58, 32-44 chars)
    if s.len() >= 32 && s.len() <= 44 && s.chars().all(|c| c.is_alphanumeric()) {
        return Ok(());
    }
    // Accept file paths
    if std::path::Path::new(s).exists() {
        return Ok(());
    }
    Err(format!("Invalid signer: {}", s))
}
```

2. Update imports in `clparse.rs` and `config.rs`
3. Remove `solana-clap-utils` from Cargo.toml
4. Run `cargo audit` - expect 0 warnings

**Estimated time:** 30 minutes
**Risk:** Low - simple string manipulation

### Phase 2: Remove Unused Dependencies (Day 1)
**Goal:** Remove solana-sdk-ids, clean up Cargo.toml

1. Remove `solana-sdk-ids` (not used)
2. Move `solana-program` to `[dev-dependencies]`
3. Update `cargo audit` allowlist

**Estimated time:** 15 minutes
**Risk:** Very low

### Phase 3: Simplify Config Loading (Week 1)
**Goal:** Replace solana-cli-config with simple YAML/JSON

1. Create `src/config/loader.rs` to handle `~/.config/solana/cli/config.yml`
2. Only need: `json_rpc_url`, `keypair_path`, `commitment`
3. Remove `solana-cli-config` dependency

**Estimated time:** 1 hour
**Risk:** Low - well-defined config format

### Phase 4: Replace Logger (Week 1)
**Goal:** Use standard Rust logging

Replace:
```rust
solana_logger::setup_with_default("solana=info")
```

With:
```rust
env_logger::Builder::from_env(
    env_logger::Env::default().default_filter_or("info")
).init();
```

**Estimated time:** 15 minutes
**Risk:** Very low

### Phase 5: Feature-Gate Heavy Dependencies (Week 2)
**Goal:** Make BPF deployment optional

1. Create `deploy-bpf` feature
2. Gate `solana-loader-v3-interface` and `solana-system-interface`
3. Most users don't deploy programs, they query

**Estimated time:** 2 hours
**Risk:** Medium - need to test feature combinations

## Expected Results

| Metric | Before | After Phase 1 | After All Phases |
|--------|--------|---------------|------------------|
| Security warnings | 4 | 0 | 0 |
| Direct Solana deps | 11 | 10 | 7 |
| Build time (cold) | ~4 min | ~3.5 min | ~2.5 min |
| Binary size | TBD | TBD | TBD |

## Risks & Mitigations

1. **Behavior drift from Solana validators**
   - Mitigation: Copy implementation, add tests

2. **Breaking changes if Solana config format changes**
   - Mitigation: Pin to known format, document version

3. **Missing edge cases in validator reimplementation**
   - Mitigation: Port Solana's tests for these functions

## Decision Point

After Phase 1, reassess:
- If build times acceptable → stop here
- If need more reduction → continue phases 2-5
- If need maximum reduction → consider replacing solana-client with raw reqwest (major effort)

## Commands to Track Progress

```bash
# Count Solana dependencies
cargo tree | grep solana | wc -l

# Check security status
cargo audit

# Measure build time
cargo clean && time cargo build --release

# Measure binary size
ls -lh target/release/osvm
```
