# Chapter 11: Statistical Arbitrage — Pairs Trading (COMPLETION OUTLINE)

**Target:** ~12,000-15,000 words total (current: 5,720 words → add ~7,000-9,000 words)

**Current Status:** Incomplete (ends at line 881 with continuation marker)

---

## What's Already Complete

✅ **Section 11.1**: Introduction and Historical Context (Morgan Stanley, 1987 crash, 2007 quant quake timeline)
✅ **Section 11.2**: Theoretical Foundations - Cointegration (Engle-Granger, Johansen methods)
✅ **Section 11.3**: Ornstein-Uhlenbeck Process (SDE formulation, parameter estimation, optimal trading rules)
✅ **Section 11.4**: Empirical Implementation (pair selection, formation/trading periods, 2007 quake analysis)
✅ **Section 11.5.1-11.5.2**: Basic OVSM implementation (spread calculation, signaling, ADF test)

**Current word count:** ~5,720 words
**Current diagrams:** 4 Mermaid diagrams (timeline, xychart, 2 flowcharts)

---

## Sections to Add (Complete the Chapter)

### 11.5.3 Ornstein-Uhlenbeck Parameter Estimation in OVSM (NEW)
**Word count target:** ~800 words
**Code:** ~100 lines

- Implement discrete-time AR(1) approximation of OU process
- Maximum likelihood estimation of theta, mu, sigma
- Half-life calculation from parameters
- Worked example: ETH/BTC pair with actual estimation

**Code functions:**
```lisp
(defun estimate-ou-parameters (spread delta-t)
  ;; Returns {:theta :mu :sigma :half-life})

(defun calculate-half-life (theta)
  ;; Returns t_1/2 = ln(2)/theta)

(defun ou-predict (current-spread theta mu delta-t)
  ;; Returns E[X_{t+Δt} | X_t])
```

### 11.5.4 Dynamic Hedge Ratio with Kalman Filter (NEW)
**Word count target:** ~1,200 words
**Code:** ~150 lines

- Problem: Static hedge ratios fail when relationship changes
- Kalman filter for time-varying beta estimation
- State-space formulation
- Implementation in OVSM with matrix operations
- Comparison: static vs. dynamic hedge ratio performance

**Diagram:** State diagram showing Kalman filter prediction-update cycle

**Code functions:**
```lisp
(defun kalman-filter-init (initial-beta variance)
  ;; Initialize state estimate and covariance)

(defun kalman-predict (state process-noise)
  ;; Prediction step: X̂_{t|t-1} = F X̂_{t-1|t-1})

(defun kalman-update (predicted observation measurement-noise)
  ;; Update step: Kalman gain, posterior estimate)

(defun rolling-hedge-ratio (prices-a prices-b window-size)
  ;; Returns time-varying hedge ratio using Kalman filter)
```

### 11.5.5 Complete Backtesting Framework (NEW)
**Word count target:** ~1,500 words
**Code:** ~200 lines

- Walk-forward pair selection
- Formation period (12 months) → Trading period (6 months)
- Event-driven execution (avoid look-ahead bias)
- Transaction cost modeling (5 components from Chapter 9)
- Performance metrics (Sharpe, max drawdown, win rate)

**Diagram:** Flowchart showing walk-forward backtesting process

**Code functions:**
```lisp
(defun backtest-pairs-trading (universe formation-days trading-days
                                :entry-z 2.0 :exit-z 0.5
                                :transaction-costs 0.0015)
  ;; Returns {:sharpe :returns :max-dd :trades})

(defun select-top-pairs (universe n-pairs method)
  ;; method: "distance", "cointegration", "correlation")

(defun execute-pairs-strategy (prices-a prices-b params)
  ;; Full strategy execution with position management)
```

### 11.5.6 Production Risk Management System (NEW)
**Word count target:** ~2,000 words
**Code:** ~250 lines

**Content:**
- Position limits (2-3% per pair, 30% aggregate exposure)
- Stop-loss rules (exit at 3-4 sigma divergence)
- Correlation monitoring (detect regime changes)
- Portfolio-level risk (VaR, CVaR)
- Circuit breakers (halt trading during extreme volatility)
- August 2007 scenario: what would have saved funds?

**Diagrams:**
1. Quadrant chart: Risk vs. Return by position sizing
2. Pie chart: Portfolio allocation across 10 pairs

**Code functions:**
```lisp
(defun create-risk-manager (:max-position-pct 0.03
                             :max-aggregate 0.30
                             :stop-loss-sigma 3.5
                             :var-limit 0.02)
  ;; Returns risk manager object with validation functions)

(defun validate-trade (pair current-positions risk-manager)
  ;; Pre-trade checks: position limits, VaR, correlation)

(defun calculate-position-size (pair-volatility capital risk-per-trade)
  ;; Kelly criterion / volatility-scaled sizing)

(defun monitor-correlation-breakdown (pairs correlation-threshold)
  ;; Detect when correlations spike (2007 scenario))

(defun circuit-breaker-check (market-volatility threshold)
  ;; Halt trading during extreme conditions)
```

### 11.5.7 Worked Example: ETH/BTC Pairs Trading (NEW)
**Word count target:** ~1,800 words
**Code:** ~300 lines (complete end-to-end example)

**Content:**
- Real data: ETH/BTC daily prices (2023-2024)
- Step-by-step execution:
  1. Formation period: estimate hedge ratio, test cointegration
  2. Calculate OU parameters, half-life
  3. Define entry/exit thresholds
  4. Trading period: execute signals with transaction costs
  5. Results: equity curve, Sharpe ratio, drawdown
- Comparison: Static vs. Kalman filter hedge ratio
- Risk analysis: What if correlation broke (2007 scenario)?

**Diagrams:**
1. XYChart: ETH/BTC spread over time with signals
2. XYChart: Equity curve showing cumulative returns
3. Timeline: Trade execution log with entry/exit points

**Complete runnable OVSM script** (~300 lines)

### 11.6 Production Deployment Considerations (NEW SECTION)
**Word count target:** ~1,200 words

**Content:**
- Data infrastructure (tick data, order book, reference data)
- Execution algorithms (minimize market impact)
- Monitoring and alerting (detect strategy decay)
- Infrastructure costs ($500-2000/month realistic estimate)
- **Disaster prevention checklist** (what Knight Capital, LTCM, 2007 funds missed)

**Diagram:** Architecture diagram showing production system components

### 11.7 Common Failure Modes and Mitigations (NEW SECTION)
**Word count target:** ~1,500 words

**Content based on real disasters:**

| Failure Mode | Example | Cost | Mitigation |
|--------------|---------|------|------------|
| **Cointegration breakdown** | August 2007 | $150B AUM destroyed | Stop-loss at 3σ, correlation monitoring |
| **Regime change** | LTCM 1998 | $4.6B | Regime detection (HMM), reduce leverage |
| **Crowding** | Gatev decline post-2000 | 50% Sharpe decay | Diversify pair selection methods |
| **Transaction costs** | Naive backtest | 3-4% annual return | Model 5-component costs (Chapter 9) |
| **Leverage cascade** | 2007 unwind | 20-30% losses | Maintain liquidity buffer, limit leverage |
| **Data quality** | Survivorship bias | Inflated backtest | Use point-in-time universe |

**Detailed analysis of each failure mode with OVSM code for detection/prevention.**

**Diagram:** Sankey diagram showing how small risks cascade into catastrophic losses

### 11.8 Chapter Summary and Key Takeaways (NEW SECTION)
**Word count target:** ~800 words

**Content:**
- ✅ **What works:** Pairs trading still viable with proper implementation
- ✅ **Critical success factors:**
  1. Cointegration testing (not just correlation)
  2. Risk management (position limits, stop-losses)
  3. Transaction cost modeling
  4. Walk-forward validation
  5. Correlation monitoring
- ⚠️ **What fails:**
  1. Static hedge ratios during regime changes
  2. Ignoring tail risk (August 2007)
  3. Excessive leverage
  4. Overcrowding in popular pairs
- 📊 **Realistic expectations (2024):**
  - Sharpe ratio: 0.8-1.2 (down from 2.0 in 1990s)
  - Annual return: 6-10% (down from 11% pre-2000)
  - Win rate: 55-60%
  - Holding period: 5-20 days

**How to avoid appearing on the disaster list:**
1. Implement ALL risk controls (cost: $0-500/month)
2. Walk-forward validate (catches overfitting)
3. Monitor correlations daily (early warning)
4. Maintain liquidity buffer (survive temporary losses)
5. Limit leverage to 1.5x max

### 11.9 Exercises (NEW SECTION)
**Word count target:** ~600 words

**Mathematical Problems:**
1. Derive the half-life formula from the OU process solution
2. Calculate optimal entry thresholds under transaction costs
3. Prove that cointegration implies error correction representation

**Coding Exercises:**
1. Implement Johansen test for 3+ asset baskets
2. Add regime detection (HMM) to detect correlation breakdowns
3. Create dashboard showing all active pairs with risk metrics

**Empirical Research:**
1. Test pairs trading on different asset classes (FX, commodities, crypto)
2. Compare distance vs. cointegration pair selection
3. Analyze strategy decay: why did Sharpe fall from 2.0 to 0.8?

### 11.10 References (EXPAND EXISTING)
**Add 15-20 new references:**

- Gatev, E., Goetzmann, W.N., & Rouwenhorst, K.G. (2006). "Pairs Trading: Performance of a Relative-Value Arbitrage Rule." *Review of Financial Studies*, 19(3), 797-827.
- Khandani, A.E., & Lo, A.W. (2007). "What Happened to the Quants in August 2007?" *Journal of Investment Management*, 5(4), 5-54.
- Do, B., & Faff, R. (2010). "Does Simple Pairs Trading Still Work?" *Financial Analysts Journal*, 66(4), 83-95.
- Elliott, R.J., Van Der Hoek, J., & Malcolm, W.P. (2005). "Pairs trading." *Quantitative Finance*, 5(3), 271-276.
- Vidyamurthy, G. (2004). *Pairs Trading: Quantitative Methods and Analysis*. Wiley.
- Engle, R.F., & Granger, C.W. (1987). "Co-integration and error correction: representation, estimation, and testing." *Econometrica*, 251-276.
- Johansen, S. (1991). "Estimation and hypothesis testing of cointegration vectors in Gaussian vector autoregressive models." *Econometrica*, 1551-1580.
- MacKinnon, J.G. (1996). "Numerical distribution functions for unit root and cointegration tests." *Journal of Applied Econometrics*, 11(6), 601-618.
- [15+ more references from research]

---

## Technical Specifications

### Code Requirements
- **Total new code:** ~1,000 lines of OVSM
- **Functions:** 25-30 fully documented functions
- **Complete example:** 300-line ETH/BTC end-to-end script
- **Comment style:** WHAT/WHY/HOW pattern (established in Chapters 8-10)

### Diagrams Requirements
- **New Mermaid diagrams:** 6-8 additional diagrams
  - QuadrantChart (risk vs. return)
  - Pie chart (portfolio allocation)
  - Sankey (risk cascade)
  - XYChart (equity curve)
  - Timeline (trade log)
  - Architecture diagram
  - Additional state/flow diagrams as needed

### Pedagogical Elements
- **60% explanation, 40% code** (maintain established ratio)
- **Disaster-driven examples** (August 2007 throughout)
- **Production-ready implementations** (not toy examples)
- **Worked examples with real numbers**
- **Failure mode analysis** (what went wrong, how to prevent)

---

## Integration with Previous Chapters

**Chapter 8 (Time Series):**
- Reference ADF test implementation
- Use cointegration theory
- Apply walk-forward validation

**Chapter 9 (Backtesting):**
- Use 5-component transaction cost model
- Apply walk-forward framework
- Reference Sharpe degradation formula

**Chapter 10 (Production):**
- Event-driven architecture
- Risk management systems
- Circuit breakers and kill switches
- Observability patterns

---

## Estimated Addition

**Words to add:** ~7,000-9,000 words
**Final total:** ~12,000-15,000 words
**New code:** ~1,000 lines OVSM
**New diagrams:** 6-8 Mermaid visualizations
**Time estimate:** 3-4 hours comprehensive writing

---

## Success Criteria

✅ Complete all OVSM implementations (production-grade, fully commented)
✅ Add worked ETH/BTC example (end-to-end, runnable)
✅ Document all 2007 quant quake lessons
✅ Provide complete risk management system
✅ Include disaster prevention checklist
✅ Maintain 60/40 explanation/code ratio
✅ Add 6-8 advanced Mermaid diagrams
✅ Cross-reference Chapters 8, 9, 10
✅ Expand bibliography to 20+ pairs trading papers
✅ Achieve ~12,000-15,000 word target

---

**Status:** Ready to execute
**Next step:** Begin writing Section 11.5.3 (OU parameter estimation)
