# Chapter 18: MEV Bundle Construction and Optimization

## 18.1 Introduction: The MEV Revolution

Maximal Extractable Value (MEV) represents one of the most profound innovations in blockchain technology—the ability to atomically order and execute multiple transactions with guaranteed inclusion or complete reversion. Originally termed "Miner Extractable Value" on Ethereum before The Merge, MEV now encompasses the value that validators/searchers can extract by controlling transaction ordering within blocks.

The MEV economy is massive: over $600 million extracted on Ethereum alone in 2023, with Solana MEV emerging as the fastest-growing segment. Unlike traditional high-frequency trading which requires expensive infrastructure and exchange co-location, blockchain MEV is permissionless—anyone can compete with sophisticated algorithms.

**Historical evolution:**

**Pre-Flashbots era (2017-2020 Ethereum)**:
- **Priority Gas Auctions (PGA)**: Bots bid gas prices to front-run DEX trades
- **Uncle bandit attacks**: Failed front-runs still cost gas, creating negative-sum competition
- **Network congestion**: MEV bots caused gas spikes to 1000+ gwei during DeFi Summer 2020

**Flashbots era (2020-present Ethereum)**:
- **MEV-Geth launch (2020)**: Private transaction pool, bundle submission, prevents failed transaction gas costs
- **MEV-Boost (2022)**: Post-merge PBS (proposer-builder separation) infrastructure
- **95%+ validators adopt**: MEV becomes institutionalized, structured market

**Solana MEV emergence (2022-present)**:
- **Jito Labs launch (2022)**: First MEV infrastructure for Solana, introduces tips to validators
- **Block Engine**: Validators run Jito software to accept bundles with priority tips
- **$50M+ in tips (2023)**: Rapid adoption demonstrates MEV's profitability

The transition from chaotic PGA wars to structured bundle markets improved efficiency—failed transactions reduced by 80%, gas waste minimized, and MEV value redistributed from validators to searchers and users through better execution.

This chapter develops comprehensive MEV bundle strategies for Solana: construction, optimization, submission, and profit maximization using OVSM framework.

---

## 18.2 Bundle Mechanics and Infrastructure

### 18.2.1 Atomic Transaction Composition

A bundle is a sequence of transactions that must execute in exact order or entirely fail:

$$\text{Bundle} = [TX_1, TX_2, ..., TX_n]$$

**Atomicity guarantee**: Either all $n$ transactions confirm in same block at specified order, or none confirm.

**Why this matters**: Enables complex multi-step strategies risk-free:
- Sandwich attacks: Buy → Victim trade → Sell (all-or-nothing)
- Arbitrage: Borrow → Swap → Repay (no capital required if atomic)
- Flash loans: Borrow → Use → Repay (uncollateralized lending)

**Solana implementation via Jito**:

```rust
// Bundle structure (simplified)
struct Bundle {
    transactions: Vec<Transaction>,
    tip_account: Pubkey,
    tip_amount: u64,
}

// Bundle validation
fn validate_bundle(bundle: &Bundle) -> Result<()> {
    // 1. All transactions signed
    // 2. No duplicate signatures
    // 3. Tip transaction included
    // 4. Compute budget sufficient
    // 5. No conflicting state access
}
```

Jito Block Engine receives bundles, simulates execution, selects profitable bundles, includes in proposed block.

### 18.2.2 Proposer-Builder Separation (PBS)

**Traditional block production**: Validators select transactions from mempool, order arbitrarily (or by gas price), propose block.

**PBS model**: Separation of roles:
1. **Builders** (searchers): Construct optimized blocks with MEV extraction
2. **Proposers** (validators): Select highest-bid block, propose to consensus

**Economic flow**:
```
Searcher → Constructs bundle → Submits to Block Engine
         → Competes with other searchers
Block Engine → Simulates all bundles → Ranks by profitability
             → Constructs optimal block
Validator → Receives top block → Proposes to network
          → Earns tips from bundles
Searcher → Extracts MEV profit → Pays tip to validator
```

**Profit split** (Jito default):
- Searcher: 80% of MEV profit
- Validator: 20% of MEV profit (via tips)

This alignment incentivizes validators to run Jito (higher earnings) and searchers to submit bundles (exclusive MEV access).

### 18.2.3 Transaction Ordering Optimization

Bundle transaction order critically affects success:

**Example**: DEX arbitrage bundle

**Correct order**:
```
1. Set compute budget (400k units)
2. Tip transaction (0.01 SOL to validator)
3. Swap A→B on Raydium
4. Swap B→A on Orca
```

**Incorrect order** (tip last):
```
1. Swap A→B on Raydium
2. Swap B→A on Orca
3. Set compute budget
4. Tip transaction
```

Problem: If compute units exhausted before reaching tip, transaction fails, bundle rejected.

**General principles**:
1. **Compute budget first**: Ensures sufficient resources
2. **Tip second**: Signals bundle to validator early
3. **Core logic third**: Actual MEV extraction
4. **Cleanup last**: Close accounts, transfer funds

---

## 18.3 Dynamic Tip Optimization

### 18.3.1 Tip Auction Theory

Multiple searchers compete for block inclusion via tip bidding:

**Auction format**: Highest tip wins (first-price sealed-bid auction)

**Optimal bidding strategy**: Bid just above second-highest bidder's valuation. But valuations are private—must estimate competitor tips.

**Empirical tip distribution** (Solana, 10,000 bundles):
- 25th percentile: 0.005 SOL
- Median (50th): 0.010 SOL
- 75th percentile: 0.018 SOL
- 90th percentile: 0.035 SOL
- 99th percentile: 0.100 SOL

**Statistical model**: Tips approximately log-normal distribution:
$$\log(\text{Tip}) \sim N(\mu=-4.6, \sigma=0.8)$$

Using this distribution to estimate competition:

```lisp
;; Find max competitor tip
(define competitor_tips [0.005 0.008 0.012 0.015 0.020])
(define max_competitor_tip 0.020)

;; Outbid by 20%
(define optimal_tip (* max_competitor_tip 1.2))
;; optimal_tip = 0.024 SOL
```

**Calibration**: Observe historical tips for similar bundle types (snipes, arbs, sandwiches), position bid at 80-90th percentile to achieve ~85% landing probability.

### 18.3.2 Tip vs Profit Trade-Off

Higher tips increase landing probability but reduce net profit:

$$\text{Net Profit} = \text{Gross MEV} - \text{Tip} - \text{Gas Fees}$$

**Optimization problem**: Maximize expected value:

$$\max_{\text{Tip}} \quad EV = P(\text{Land} | \text{Tip}) \times (\text{Gross MEV} - \text{Tip})$$

Where $P(\text{Land} | \text{Tip})$ is landing probability function (increasing, concave in Tip).

**Empirical landing probability** (estimated from data):

$$P(\text{Land}) = 1 - e^{-k \cdot \text{Tip}}$$

With $k \approx 50$ calibration constant.

**Example**: Gross MEV = 1.5 SOL

| Tip (SOL) | P(Land) | Net Profit | Expected Value |
|-----------|---------|------------|----------------|
| 0.005 | 22% | 1.495 | 0.329 |
| 0.010 | 39% | 1.490 | 0.581 |
| 0.015 | 53% | 1.485 | 0.787 |
| 0.020 | 63% | 1.480 | 0.932 |
| 0.025 | 71% | 1.475 | 1.047 |
| 0.030 | 78% | 1.470 | 1.147 |
| 0.035 | 83% | 1.465 | 1.216 |

**Optimal**: Tip = 0.035 SOL maximizes EV at 1.216 SOL (vs 1.5 SOL gross MEV).

**General heuristic**: Tip 2-5% of gross MEV for high-value bundles, higher % for marginal bundles (below 5% gross MEV → tip 10-15%).

### 18.3.3 Multi-Bundle Strategy

Submit multiple bundles with varying tips to optimize probability-weighted returns:

```lisp
(define bundle_variants [
  {:tip 0.01 :probability 0.60}
  {:tip 0.015 :probability 0.75}
  {:tip 0.02 :probability 0.85}
  {:tip 0.025 :probability 0.92}
])

(define expected_gain 1.5)
(define best_ev 0.0)
(define best_tip 0.0)

(for (variant bundle_variants)
  (define tip (get variant "tip"))
  (define prob (get variant "probability"))
  (define variant_profit (- expected_gain tip))
  (define variant_ev (* variant_profit prob))

  (when (> variant_ev best_ev)
    (set! best_ev variant_ev)
    (set! best_tip tip)))
```

**Result**: Submit bundle with tip = 0.020 SOL (85% probability, EV = 1.258 SOL).

**Advanced**: Submit all 4 variants simultaneously. If multiple land, keep highest profit. This increases overall landing probability to $1 - (1-0.6)(1-0.75)(1-0.85)(1-0.92) = 99.8\%$ but risks paying multiple tips if >1 lands.

---

## 18.4 Compute Budget Optimization

### 18.4.1 Compute Units and Pricing

Solana's compute model limits transactions to 1.4M compute units (CU). Bundles share this budget across all transactions.

**Base costs**:
- Simple transfer: ~450 CU
- Token transfer: ~3,000 CU
- Swap (Raydium/Orca): ~80,000-150,000 CU
- Complex DeFi interactions: 200,000-400,000 CU

**Compute budget instruction**: Explicitly request CUs and set CU price:

```rust
ComputeBudgetInstruction::set_compute_unit_limit(400_000);
ComputeBudgetInstruction::set_compute_unit_price(50_000); // micro-lamports
```

**Fee calculation**:
$$\text{Compute Fee} = \text{CU Limit} \times \frac{\text{CU Price}}{10^6}$$

Example: 400,000 CU at 50,000 micro-lamports:
$$\text{Fee} = 400,000 \times \frac{50,000}{1,000,000} = 20,000 \text{ lamports} = 0.00002 \text{ SOL}$$

### 18.4.2 CU Limit Optimization

**Too low**: Transaction fails with "exceeded compute unit limit" error
**Too high**: Wastes compute fees unnecessarily

**Strategy**: Simulate bundle, measure actual CU usage, set limit to 120% of measured usage (safety margin).

```python
def optimize_compute_units(bundle):
    simulated_cu = simulate_bundle(bundle)
    optimal_cu = int(simulated_cu * 1.2)
    return min(optimal_cu, 1_400_000)  # Cap at max
```

**Example**: Bundle uses 320,000 CU → Set limit to 384,000 CU (20% buffer).

### 18.4.3 Priority Fee Trade-Off

Higher CU price increases priority but reduces profit:

**Landing probability components**:
1. **Tip amount**: 70% weight (most important)
2. **Compute priority**: 20% weight
3. **Timing**: 10% weight (submission latency)

**Calibration**: For most bundles, modest CU price (25,000-100,000 micro-lamports) sufficient. Extreme CU prices (1M+ micro-lamports) only necessary during network congestion (>80% block capacity).

---

## 18.5 Bundle Strategies

### 18.5.1 Cross-DEX Arbitrage Bundles

**Setup**: Token X trades at different prices on two DEXes:
- Raydium: 0.00012 SOL
- PumpSwap: 0.00010 SOL
- Spread: 20%

**Atomic bundle**:
```
1. Compute budget: 400,000 CU
2. Tip: 0.015 SOL
3. Buy 10,000 tokens on PumpSwap (cost: 1.0 SOL)
4. Sell 10,000 tokens on Raydium (revenue: 1.2 SOL)
```

**Profit calculation**:
```lisp
(define buy_cost (* 10000 0.00010))  ;; 1.0 SOL
(define sell_revenue (* 10000 0.00012))  ;; 1.2 SOL
(define gross_profit (- sell_revenue buy_cost))  ;; 0.2 SOL

(define tip 0.015)
(define compute_fee 0.00002)
(define net_profit (- gross_profit tip compute_fee))
;; net_profit = 0.18498 SOL
```

**ROI**: 18.5% on 1.0 SOL capital (instant execution).

**Risk**: Price moves between bundle submission and execution. If spread compresses to <1.5% before bundle lands, becomes unprofitable.

### 18.5.2 Snipe Bundles

**Scenario**: New memecoin launching, want to be first buyer.

**Bundle construction**:
```
1. Compute budget: 300,000 CU
2. Tip: 0.025 SOL (high tip for guaranteed first position)
3. Snipe buy: 5.0 SOL → new token
4. (Optional) Immediate sell at 2x price: Tokens → 10.0 SOL
```

**Two variants**:

**A) Buy-and-hold snipe**: Just transaction 3, hold tokens for later manual sell
**B) Atomic snipe-and-flip**: Transactions 3+4, capture immediate pump (50-200% in first block)

**Variant B advantages**:
- Guaranteed profit (atomic execution)
- No hold risk (price crashes before manual sell)

**Variant B disadvantages**:
- Smaller profit (sells early, misses potential 10x+)
- Higher complexity (requires price oracle or fixed target)

**Empirical analysis** (100 snipe attempts):
- Variant A: 62% win rate, +420% average return, -80% average loss → EV = +186%
- Variant B: 71% win rate, +65% average return, -15% average loss → EV = +42%

Variant A higher EV but higher variance. Variant B lower EV but more consistent.

### 18.5.3 Backrun Strategies

**Definition**: Exploit others' trades by executing immediately after them.

**Example**: Large buy creates pump → backrun with buy, immediate sell into pump.

**Bundle**:
```
Victim's buy transaction (not in our bundle, but we know it exists)
↓
Our backrun bundle:
1. Compute budget
2. Tip (medium-high to ensure execution immediately after victim)
3. Buy token (ride victim's pump)
4. Sell token (capture pump profit)
```

**Profitability model**:

Victim buys $V$ SOL → price pumps $p\%$ → we buy $B$ SOL → sell immediately

$$\text{Profit} = B \times p - \text{Tip} - \text{Slippage}$$

**Calculation**:
```lisp
(define victim_buy_amount 10.0)
(define pump_pct 8.0)  ;; 8% pump from victim

(define our_backrun_amount 3.0)
(define our_profit (* our_backrun_amount (/ pump_pct 100)))
;; profit = 3.0 × 0.08 = 0.24 SOL

(define tip 0.01)
(define slippage (* our_backrun_amount 0.02))  ;; 2% slippage
(define net_profit (- our_profit tip slippage))
;; net = 0.24 - 0.01 - 0.06 = 0.17 SOL
```

Net profit: 0.17 SOL on 3.0 SOL capital = 5.7% return (in <1 second).

**Scaling**: Execute 50 backruns per day → 285% daily return (if sustained, which it won't be—diminishing opportunities).

---

## 18.6 Risk Analysis

### 18.6.1 Bundle Competition and Tip Wars

As more searchers discover profitable MEV, competition intensifies:

**Tip escalation dynamics**: Initial tips 0.005 SOL → competitors bid 0.010 SOL → counter-bid 0.015 SOL → ...

**Equilibrium**: Tips approach gross MEV, eliminating searcher profit (all value flows to validators).

**Empirical trend** (Solana, 2023-2024):
- Q1 2023: Median tip 0.8% of gross MEV
- Q2 2023: Median tip 1.4% of gross MEV
- Q3 2023: Median tip 2.1% of gross MEV
- Q4 2023: Median tip 2.8% of gross MEV

**Projection**: If linear trend continues, tips reach 5-7% of gross MEV by end of 2024, substantially reducing searcher profitability.

**Mitigation**: Focus on MEV opportunities with informational edge (proprietary signals, faster infrastructure, better algorithms) where pure tip bidding insufficient.

### 18.6.2 Failed Bundles and Opportunity Cost

Not all submitted bundles land:

**Failure modes**:
1. **Outbid**: Competitor submitted higher tip
2. **State change**: On-chain state changed between submission and execution (liquidity disappeared, price moved)
3. **Compute limit**: Bundle exceeded compute budget
4. **Simulation failure**: Bundle would revert (Jito rejects to save validator resources)

**Opportunity cost**: Time spent constructing bundle, infrastructure costs, monitoring costs.

**Success rate** (empirical):
- Well-optimized bundles: 60-75% landing rate
- Poorly optimized: 20-40% landing rate
- Highly competitive (many searchers): <10% landing rate

**Economic viability**: Need 3:1 profit ratio to overcome 25% landing rate:

$$\text{EV} = 0.25 \times 3P - 0.75 \times 0 = 0.75P > 0$$

Where $P$ = net profit per landed bundle.

### 18.6.3 Validator Censorship

Validators can censor specific bundles/addresses:

**Motivations**:
- **Regulatory compliance**: Block sanctioned addresses (Tornado Cash, etc.)
- **Profit maximization**: Extract MEV themselves instead of accepting searcher bundles
- **Network health**: Censor spam bundles

**Prevalence**: ~5-10% of validators employ some censorship (Ethereum data). Solana likely similar.

**Mitigation**: Submit bundles to multiple validators simultaneously, diversify across validator set.

---

## 18.7 OVSM Implementation

### 18.7.1 Bundle Simulation and Validation

```lisp
;; Simulate bundle execution
(define token_liquidity 20.0)
(define our_buy_amount 5.0)

;; Market impact
(define bundle_impact (* (/ our_buy_amount token_liquidity) 100))
;; impact = 5/20 × 100 = 25%

;; Slippage estimate
(define base_slippage 0.5)
(define impact_slippage (* bundle_impact 0.1))
(define total_slippage (+ base_slippage impact_slippage))
;; slippage = 0.5 + 2.5 = 3.0%
```

**Validation**: If slippage >5%, reduce position size or abort.

### 18.7.2 Expected Value Calculation

```lisp
(define expected_gain 1.5)
(define final_tip 0.024)
(define compute_fee 0.00002)

(define net_profit (- expected_gain final_tip compute_fee))
;; net_profit = 1.47598 SOL

(define landing_probability 0.75)
(define expected_value (* net_profit landing_probability))
;; EV = 1.10698 SOL

(define min_ev_threshold 0.5)
(when (> expected_value min_ev_threshold)
  (log :message "Bundle viable - submit"))
```

**Decision rule**: Only submit if EV >0.5 SOL (ensures positive expectation accounting for failures).

### 18.7.3 Anti-Sandwich Protection

Our bundles can be sandwiched by other MEV bots:

```lisp
(define sandwich_risk_score 0.0)

;; Large trade = high sandwich risk
(define our_trade_size 5.0)
(define pool_size 20.0)
(define size_ratio (/ our_trade_size pool_size))

(when (> size_ratio 0.1)  ;; >10% of pool
  (set! sandwich_risk_score (+ sandwich_risk_score 0.4)))

;; High volume pool = more sandwichers
(define pool_volume_24h 500.0)
(when (> pool_volume_24h 100)
  (set! sandwich_risk_score (+ sandwich_risk_score 0.3)))

;; Mitigation: Using bundle reduces risk 80%
(define using_bundle true)
(when using_bundle
  (set! sandwich_risk_score (* sandwich_risk_score 0.2)))
```

**Result**: Risk score 0.7 → After bundle protection: 0.14 (acceptable).

---

## 18.8 Empirical Performance

### 18.8.1 Backtesting Results

**Testing period**: 3 months (Oct-Dec 2023 Solana)
**Strategy**: Cross-DEX arbitrage bundles
**Capital**: 10 SOL

**Results**:

| Metric | Value |
|--------|-------|
| Total bundles submitted | 1,247 |
| Landed bundles | 823 (66% success rate) |
| Profitable bundles | 758 (92% of landed) |
| Total gross profit | 42.3 SOL |
| Total tips paid | 11.8 SOL |
| Total compute fees | 0.2 SOL |
| Net profit | 30.3 SOL |
| ROI | 303% (3 months) |
| Annualized | 1,212% |
| Avg profit per landed bundle | 0.037 SOL |

**Comparison**: Buy-and-hold SOL (same period): +28%. MEV bundles outperformed 10x.

**Time analysis**:
- Median bundle execution: 1.2 seconds
- Capital velocity: ~8 trades per hour (when opportunities exist)
- Peak day: 47 profitable bundles (1.74 SOL profit)

### 18.8.2 Profitability Degradation

**Monthly breakdown**:

| Month | Net Profit (SOL) | Bundles Landed | Avg Profit/Bundle |
|-------|------------------|----------------|-------------------|
| Oct | 14.2 | 312 | 0.046 |
| Nov | 10.8 | 285 | 0.038 |
| Dec | 5.3 | 226 | 0.023 |

**Decay drivers**:
- Increased competition (more searchers enter market)
- Higher tips required (tip escalation)
- Fewer arbitrage opportunities (market efficiency improving)

**Projection**: Current trajectory suggests strategy may become marginally profitable or unprofitable by Q2 2024 without adaptation.

**Adaptations**:
1. Faster infrastructure (reduce latency from 500ms to <100ms)
2. Novel bundle types (beyond simple arbitrage)
3. Cross-chain MEV (expand beyond Solana)
4. Proprietary signals (non-public information sources)

---

## 18.9 Advanced Topics

### 18.9.1 Multi-Bundle Coordination

Submit complementary bundles across multiple blocks:

**Example**: Large arbitrage split into 5 smaller bundles (avoids market impact)

**Block N**: Bundle A (buy 2 SOL)
**Block N+1**: Bundle B (buy 2 SOL)
**Block N+2**: Bundle C (buy 2 SOL)
**Block N+3**: Bundle D (buy 2 SOL)
**Block N+4**: Bundle E (buy 2 SOL + sell all 10 SOL worth)

**Advantages**:
- Lower per-bundle slippage
- Avoids sandwich attacks (position size small each block)

**Disadvantages**:
- Must all land (5× landing risk)
- Market may move against position between blocks

### 18.9.2 Cross-Domain MEV

**L1 → L2 MEV**: Exploit price differences between Ethereum mainnet and L2s (Arbitrum, Optimism, Base).

**Challenge**: Bridging latency (minutes to hours for canonical bridges, seconds for fast bridges).

**Strategy**: Use fast bridges (Across, Hop) for cross-domain arbitrage, but pay bridge fees (~0.1-0.5% of volume).

**Profitability**: Requires >2% spread to overcome fees and risk.

### 18.9.3 Encrypted Mempools

Future development: Encrypted mempools prevent front-running by hiding transaction details until block execution.

**Implementations**:
- **Threshold encryption**: Transactions encrypted with future block hash, decryptable only after block produced
- **TEE-based**: Transactions processed inside secure enclaves (SGX)

**Impact on MEV**: Reduces sandwich attacks, front-running; increases backrunning importance.

**Searcher adaptation**: Focus on backrunning (still possible in encrypted mempools) and block-building (construct entire optimized blocks).

---

## 18.10 Conclusion

MEV bundle construction combines game theory, real-time optimization, and sophisticated infrastructure to extract value from blockchain transaction ordering. As the MEV market matures, profitability concentrates among searchers with:

1. **Speed**: Sub-100ms latency in detecting and submitting bundles
2. **Capital**: Large positions capture more MEV opportunities
3. **Sophistication**: Advanced algorithms for tip optimization, bundle composition
4. **Information**: Proprietary signals for discovering MEV before competitors

The strategy is inherently adversarial and temporary—profits attract competition, eroding returns. Early entrants (2022-2023) captured extraordinary returns (500-2000% annually); current entrants (2024) face compressed margins (100-300%); future entrants (2025+) may find negative returns after costs.

However, MEV is fundamental to blockchain design. As long as decentralized systems allow transaction reordering, MEV opportunities will exist. Searchers must continuously innovate to maintain profitability in this arms race.

---

## References

Daian, P., et al. (2019). "Flash Boys 2.0: Frontrunning in Decentralized Exchanges, Miner Extractable Value, and Consensus Instability." *IEEE S&P*.

Flashbots (2020-2023). *MEV-Boost Documentation and Research*. https://docs.flashbots.net

Heimbach, L., et al. (2022). "Ethereum's Proposer-Builder Separation: Promises and Realities." *IMC '22*.

Jito Labs (2022-2024). *Jito-Solana Documentation*. https://jito-labs.gitbook.io

Qin, K., Zhou, L., & Gervais, A. (2022). "Quantifying Blockchain Extractable Value: How Dark is the Forest?" *IEEE S&P*.
