# Whale Hunter Guide

## Overview

The Whale Hunter is an intelligent multi-hop wallet discovery tool that expands wallet connections until high-value targets are found.

## Versions

### 1. `whale_hunter.ovsm` - Mock Data Version

**Purpose**: Testing, development, and demonstration

**Features**:
- Mock balance generation (0-2000 SOL range)
- Mock wallet connections
- Fast execution (no RPC delays)
- 20 hops maximum
- 50 concurrent wallets per hop

**Usage**:
```bash
osvm ovsm run examples/ovsm_scripts/whale_hunter.ovsm
```

**Best For**:
- Testing async/await patterns
- Demonstrating whale hunting logic
- Performance benchmarking
- Learning OVSM concurrency

---

### 2. `whale_hunter_rpc.ovsm` - Production Template

**Purpose**: Template for real blockchain investigation with actual Solana RPC calls

**Status**: ⚠️ **Template/Reference** - RPC functions not yet implemented in OVSM runtime

**Features (When RPC Functions Available)**:
- **Real RPC Calls**:
  - `getBalance()` - Actual SOL balances
  - `getAccountInfo()` - Program ownership verification
  - `getSignaturesForAddress()` - Transaction history
  - `getTransaction()` - Detailed transaction data
- **Rate Limit Friendly**:
  - 5 hops default (adjustable)
  - 10 wallets per hop max
  - 50 signatures per wallet
- **Production Ready**:
  - Real whale detection
  - Actual DEX pool identification
  - True wallet relationships

**Usage**:
```bash
# Basic usage
osvm ovsm run examples/ovsm_scripts/whale_hunter_rpc.ovsm

# With custom RPC endpoint
osvm --url https://your-rpc-endpoint.com ovsm run examples/ovsm_scripts/whale_hunter_rpc.ovsm
```

**Best For**:
- Real blockchain investigation
- Finding actual whales (1000+ SOL)
- Discovering DEX pools
- Tracing real wallet relationships

---

## Configuration Comparison

| Setting | Mock Version | RPC Version |
|---------|-------------|-------------|
| Max Depth | 20 hops | 5 hops* |
| Wallets/Hop | 50 | 10* |
| Execution Time | ~30-60s | ~2-5 min* |
| RPC Calls | None | ~500+ |
| Cost | Free | RPC credits |

\* Adjustable based on your RPC rate limits

---

## RPC Version Configuration

### Adjust for Your RPC Provider

```lisp
;; Conservative (free tier)
(define max-depth 3)
(define max-wallets-per-hop 5)
(define signatures-limit 20)

;; Standard (paid tier)
(define max-depth 5)
(define max-wallets-per-hop 10)
(define signatures-limit 50)

;; Aggressive (premium tier)
(define max-depth 10)
(define max-wallets-per-hop 25)
(define signatures-limit 100)
```

### RPC Providers

**Free Tier** (limited):
- Solana public RPC: `https://api.mainnet-beta.solana.com`
- Limit: ~100 requests/second

**Paid Tiers** (recommended):
- Helius: High rate limits, caching
- Triton: Low latency, compression
- QuickNode: Enterprise reliability

### Usage Examples

```bash
# Using Helius
osvm --url https://mainnet.helius-rpc.com/?api-key=YOUR_KEY \
  ovsm run examples/ovsm_scripts/whale_hunter_rpc.ovsm

# Using Triton
osvm --url https://api.triton.one/YOUR_KEY \
  ovsm run examples/ovsm_scripts/whale_hunter_rpc.ovsm
```

---

## RPC Version Details

### Real Data Extraction

**Balance Checking**:
```lisp
;; Returns balance in lamports
(define balance-lamports (getBalance wallet))
(define balance-sol (/ balance-lamports 1000000000))
```

**Program Ownership**:
```lisp
;; Check if wallet is a DEX pool account
(define account-info (getAccountInfo wallet))
(define owner (get account-info "owner"))
(if (is-dex-program owner)
    ;; It's a pool!
    ...
)
```

**Connection Discovery**:
```lisp
;; Get recent transactions
(define signatures (getSignaturesForAddress wallet :limit 50))

;; Extract all unique wallets from transactions
(for (sig signatures)
  (define tx (getTransaction (get sig "signature")))
  (define account-keys (get ... "accountKeys"))
  ;; Add unique wallets to children list
)
```

---

## Performance Considerations

### RPC Version Timing

| Operation | Time | RPC Calls |
|-----------|------|-----------|
| Check 1 wallet | ~500ms | 52 calls |
| Hop with 10 wallets | ~5s | 520 calls |
| 5 hops total | ~25s | 2,600 calls |

**Breakdown per wallet**:
- 1x `getBalance` (~50ms)
- 1x `getAccountInfo` (~50ms)
- 1x `getSignaturesForAddress` (~100ms)
- ~50x `getTransaction` (~300ms total with batching)

### Optimization Tips

1. **Use Premium RPC**: Dramatic speed improvements
2. **Enable Compression**: Reduce bandwidth
3. **Batch Requests**: When possible
4. **Cache Results**: Don't re-check visited wallets
5. **Limit Depth**: Start small, expand as needed

---

## Output Comparison

### Mock Version Output
```
🔍 HOP 1: Checking 1 wallets...
  → 0 whales, 0 pools, 3 new connections
  💰 Hop balance: 44 SOL (Total: 44 SOL)
  🏆 Top 3 wallets:
    1. 5rVDMMoBQs3zJQ9DT7oxsoNZfxptgLCKhuWqdwoX9q85 - 44 SOL
```

### RPC Version Output
```
🔍 HOP 1: Checking 1 wallets...
  🐋 WHALE FOUND: 5rVDMMoBQs3zJQ9DT7oxsoNZfxptgLCKhuWqdwoX9q85 - 1,234 SOL (Hop 1)
  → 1 whales, 0 pools, 8 new connections
  💰 Hop balance: 1,234 SOL (Total: 1,234 SOL)
  🏆 Top 3 wallets:
    1. 5rVDMMoBQs3zJQ9DT7oxsoNZfxptgLCKhuWqdwoX9q85 - 1,234 SOL
    2. 9W959DqEETiGZocYWCQPaJ6sBmUzgfxXfqGeTEdp3aQP - 500 SOL
    3. DqEETiGZocYWCQPaJ6sBmUzgfxXfqGeTEdp3aQP9W959 - 250 SOL

✅ TARGET FOUND!
```

---

## Whale Detection Criteria

### What Counts as a Whale?

**Default**: 1000+ SOL (~$100k+ USD)

**Adjustable**:
```lisp
;; High-value targets only
(define target-sol-balance 5000)  ;; $500k+

;; Lower threshold for testing
(define target-sol-balance 100)   ;; $10k+

;; Mega whales only
(define target-sol-balance 10000) ;; $1M+
```

### DEX Pools Tracked

- ✅ Raydium AMM
- ✅ Orca Whirlpool v1 & v2
- ✅ Lifinity
- ✅ Phoenix

**Add More**:
```lisp
(define dex-programs [
  "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"
  "YOUR_PROGRAM_ID_HERE"  ;; Add custom program
])
```

---

## Troubleshooting

### RPC Version Issues

**"Too Many Requests"**:
- Reduce `max-wallets-per-hop`
- Reduce `signatures-limit`
- Use premium RPC provider

**"Timeout"**:
- Increase timeout in code
- Use faster RPC endpoint
- Reduce concurrent requests

**"Rate Limited"**:
- Add delays between hops
- Use dedicated RPC node
- Implement backoff strategy

**No Whales Found**:
- Lower `target-sol-balance`
- Increase `max-depth`
- Try different seed wallet
- Check seed wallet has transactions

---

## Best Practices

### Starting Your Hunt

1. **Test with Mock Version First**:
   ```bash
   osvm ovsm run examples/ovsm_scripts/whale_hunter.ovsm
   ```

2. **Try RPC Version with Conservative Settings**:
   - Start with 3 hops
   - 5 wallets per hop
   - Monitor RPC usage

3. **Gradually Increase Scale**:
   - If working well, increase to 5 hops
   - Then 10 wallets per hop
   - Watch for rate limits

4. **Use Premium RPC for Production**:
   - Free tier = testing only
   - Paid tier = real investigations
   - Track RPC costs

### Choosing a Seed Wallet

**Good Seed Wallets**:
- Known DEX traders
- High-volume wallets
- Active protocol users
- NFT collectors

**Bad Seed Wallets**:
- Empty/inactive wallets
- Single-transaction wallets
- Exchange hot wallets (too many connections)

---

## Cost Estimation

### RPC Version Costs

**Free Tier** (Solana public RPC):
- Cost: $0
- Limit: ~1-2 hops practical
- Best for: Testing only

**Helius Starter** ($50/month):
- Requests: 250k/month
- 5 hops = ~2,600 requests
- Capacity: ~90 whale hunts/month
- Best for: Regular investigation

**Helius Pro** ($200/month):
- Requests: 2M/month
- Capacity: ~770 whale hunts/month
- Best for: Heavy usage

---

## Next Steps

1. ✅ Run mock version to understand flow
2. ✅ Get premium RPC access
3. ✅ Test RPC version with 1 hop
4. ✅ Gradually scale up
5. ✅ Automate with cron/scheduler
6. ✅ Set up alerts for whale discoveries

---

## Support

- **Documentation**: `ASYNC_EXAMPLES.md`
- **Issues**: GitHub issues
- **Examples**: `examples/ovsm_scripts/`

**Happy Whale Hunting! 🐋**
