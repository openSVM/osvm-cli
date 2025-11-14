# Chapter 9: Backtesting Frameworks - PEDAGOGICAL OUTLINE

## Target: 12,000-15,000 words, 60% explanation, 40% implementation

---

## OPENING STORY: The Overfitting Crisis (~1,500 words)

**Hook:** 95% of quantitative strategies fail within 6 months of live trading despite stunning backtest results.

**Case Study: The $100M Lesson**

Anonymous mid-sized hedge fund, 2018:
- Backtest: 3.5 Sharpe ratio, 60% annual returns, max drawdown 8%
- Live trading (3 months): -15% return, 25% drawdown, shut down
- What went wrong? Seven deadly sins of backtesting

**The Seven Deadly Sins:**
1. **Survivorship bias**: Backtested on S&P 500 current constituents (survivors only)
2. **Look-ahead bias**: Used next-day open prices known only after close
3. **Data snooping**: Tested 347 parameter combinations, reported best
4. **Overfitting**: 15 parameters for 5 years of data (overfitted noise)
5. **Unrealistic costs**: Assumed instant fills at midpoint, ignored slippage
6. **Regime change**: 2017 bull market → 2018 volatility spike
7. **Incomplete testing**: No stress testing, no regime analysis

**The Lesson:**
> "A backtest is not a prediction. It's a diagnostic tool that reveals whether your strategy's edge might survive the transition from history to reality." — Ernest Chan

**What This Chapter Teaches:**
- How to design backtests that reveal flaws, not hide them
- Walk-forward analysis to prevent overfitting
- Event-driven architecture for realistic simulation
- Transaction cost modeling that reflects reality
- Performance metrics that matter (not just Sharpe)
- Common pitfalls and how to avoid each one

---

## SECTION 1: Why Most Backtests Lie (~2,000 words)

**Pedagogical Goal:** Build intuition for why backtests fail before diving into solutions.

### 1.1 The Fundamental Problem

**The Time Machine Paradox:**
```
Backtest: You have perfect knowledge of the future
Reality: You have uncertainty, emotions, execution delays
```

**Example:**
```
Backtest says: "Buy AAPL at $150.00"
Reality:
- You see $150.00 and decide to buy (100ms delay)
- Your order reaches exchange (5ms latency)
- Price is now $150.02 (slippage)
- You get filled at $150.025 (partial fill at worse price)
- Commission: $0.005 per share
- Actual entry: $150.03 (0.02% worse than backtest)

For a 2% move strategy, that 0.02% slippage ate 1% of your edge!
```

### 1.2 The Three Sources of Backtest Lies

**Lie 1: Perfect Information (Look-Ahead Bias)**

Classic mistake:
```python
# WRONG - Look-ahead bias!
data['returns'] = data['close'].pct_change()
data['signal'] = (data['returns'].shift(-1) > 0).astype(int)  # Future data!
```

**Why this is devastating:**
- You're using tomorrow's returns to make today's decision
- Backtest looks amazing (you have a crystal ball)
- Live trading fails immediately (no crystal ball)

**Lie 2: Perfect Execution (Unrealistic Costs)**

Naive assumption:
```
IF signal == BUY:
    buy_at_price = current_close
    # In reality: slippage, latency, partial fills, impact
```

**Reality:**
```
Your $1M order on $50M daily volume = 2% of volume
Market impact: ~0.5% (square root rule)
Plus slippage: ~0.1%
Plus commissions: ~0.05%
Total cost: ~0.65% round-trip

If your edge is 1% per trade, costs ate 65% of it!
```

**Lie 3: Perfect Data (Survivorship & Data Snooping)**

**Survivorship Bias Example:**
```
Testing a stock-picking strategy on current S&P 500 constituents:
- Current 500: These are the survivors (success bias)
- Missing: 200+ companies that were delisted, bankrupt, acquired
- Effect: Your backtest picks winners that already won

Example: Testing "buy low P/E stocks" on S&P 500 from 2000-2010
- Backtest using 2020 constituents: +15% annual return
- Backtest using point-in-time constituents: +8% annual return
- Survivorship bias added 7% (nearly doubled results!)
```

**Data Snooping Example:**
```
Researcher tests 347 parameter combinations:
- 1 combination looks amazing (Sharpe = 3.2)
- Report only that one
- Problem: At 5% significance, 347 tests give 17 false positives
- That "amazing" strategy is likely one of the false positives
```

### 1.3 The Forward-Looking Bias Taxonomy

**Complete List of Ways Backtests Cheat:**

| Bias Type | Example | Impact on Returns |
|-----------|---------|-------------------|
| **Look-ahead** | Using next-day open before close | +50-300% inflation |
| **Survivorship** | Only current index members | +5-15% annual |
| **Selection** | Cherry-picking assets that worked | +10-50% |
| **Data snooping** | Testing hundreds of parameters | +10-200% (false!) |
| **Hindsight** | Using restated financial data | +2-5% |
| **Time period** | Testing only bull markets | +50-100% |

### 1.4 The Psychology of Overfitting

**Why smart people overfit:**
1. **Confirmation bias**: We want our strategy to work
2. **Pattern recognition**: Humans see patterns in randomness
3. **Pressure**: Need to show results to investors/boss
4. **Ignorance**: Don't know they're doing it wrong

**The Monte Carlo Reality Check:**

```python
# True test: Can your strategy beat random luck?
np.random.seed(42)
random_returns = np.random.normal(0, 0.01, 1000)  # Random noise
optimized_params = optimize(random_returns)       # Optimize on noise!

# Result: Even random noise can produce "strategies" with Sharpe > 2
# if you optimize hard enough!
```

---

## SECTION 2: Walk-Forward Analysis - The Antidote to Overfitting (~2,500 words)

**Pedagogical Goal:** Teach the gold standard method to validate strategies.

### 2.1 The Core Idea

**Traditional Split:**
```
[=========== In-Sample ===========][== Out-of-Sample ==]
        Train (70%)                    Test (30%)
        Optimize here                  Validate here
        (2015-2020)                    (2021-2023)

Problem: Only one out-of-sample test
If you get lucky, you won't know it!
```

**Walk-Forward:**
```
[=== Train ===][Test][=== Train ===][Test][=== Train ===][Test]
  (2015-2016)  (2017) (2016-2018)  (2019) (2018-2020)  (2021)
     ↓          ↓         ↓          ↓         ↓          ↓
  Optimize    Test   Re-optimize   Test   Re-optimize   Test

Multiple out-of-sample tests → More robust validation
```

### 2.2 The Anchored vs. Rolling Window Debate

**Anchored (Expanding Window):**
```
Train 1: [========= 2015-2016 =========] → Test 2017
Train 2: [=============== 2015-2018 ===============] → Test 2019
Train 3: [======================= 2015-2020 =======================] → Test 2021

Pros: More data → more stable parameters
Cons: Old data may be irrelevant (regime change)
```

**Rolling (Fixed Window):**
```
Train 1: [=== 2015-2016 ===] → Test 2017
Train 2:     [=== 2017-2018 ===] → Test 2019
Train 3:         [=== 2019-2020 ===] → Test 2021

Pros: Focuses on recent data → adapts to regime changes
Cons: Less data → less stable parameters
```

**Which to use?**
- **Stable strategies (mean reversion):** Anchored
- **Adaptive strategies (momentum):** Rolling
- **When in doubt:** Test both!

### 2.3 Worked Example: ETH/BTC Pairs Trading with Walk-Forward

**Setup:**
```
Data: 2020-2023 daily (1,460 days)
Strategy: Pairs trading from Chapter 8
Parameters to optimize: Entry threshold (Z-score), exit threshold
Window: 252 days train, 63 days test (1 year train, 1 quarter test)
```

**Step 1: Traditional Backtest (The Wrong Way)**

```lisp
;; Optimize on ALL data (2020-2023)
(define all-data (get-historical-prices "ETH/USD" "BTC/USD" :days 1460))
(define best-params (optimize-pairs-trading all-data))
;; Result: entry_z = 2.1, exit_z = 0.3, Sharpe = 2.8

;; Problem: We used future data to optimize!
;; Live trading will fail.
```

**Step 2: Walk-Forward (The Right Way)**

```lisp
(define (walk-forward-backtest data :train-days 252 :test-days 63)
  (define n (length data))
  (define results (array))

  ;; Loop through time, never using future data
  (for (start (range 0 (- n train-days test-days) test-days))
    (let ((train-end (+ start train-days))
          (test-end (+ train-end test-days))
          (train-data (slice data start train-end))
          (test-data (slice data train-end test-end)))

      ;; Optimize on training window ONLY
      (define params (optimize-params train-data))

      ;; Test on out-of-sample data
      (define test-result (backtest-with-params test-data params))

      (push! results {:period (format "{}-{}" train-end test-end)
                      :params params
                      :sharpe (get test-result :sharpe)
                      :return (get test-result :total-return)})))

  results)

;; Run walk-forward
(define wf-results (walk-forward-backtest all-data))

;; Analyze results
(define avg-sharpe (mean (map (lambda (r) (get r :sharpe)) wf-results)))
(log :message (format "Walk-forward Sharpe: {:.2f}" avg-sharpe))
;; Result: Sharpe = 1.3 (much lower than 2.8!)
;; This is the REAL expectation for live trading.
```

**Step 3: Degradation Analysis**

```
In-sample Sharpe (optimized): 2.8
Out-of-sample Sharpe (walk-forward): 1.3
Degradation: 54% (typical for overfitting!)

If degradation > 30%: Strategy likely overfit
If degradation < 15%: Strategy likely robust
```

### 2.4 Parameter Stability Analysis

**Key Question:** Do optimal parameters change dramatically between windows?

```lisp
;; Extract optimal parameters from each window
(define param-history
  (map (lambda (r) (get (get r :params) :entry-z)) wf-results))

;; Compute stability metrics
(define param-mean (mean param-history))
(define param-std (std param-history))
(define param-cv (/ param-std param-mean))  ; Coefficient of variation

(log :message (format "Parameter CV: {:.2f}" param-cv))

;; Decision rule:
;; CV < 0.15: Stable → use fixed parameters
;; CV > 0.30: Unstable → need adaptive approach (Kalman filter)
```

**Example Results:**
```
Entry Z-score across windows: [2.1, 1.9, 2.3, 2.0, 2.2, 1.8]
Mean: 2.05
Std: 0.17
CV: 0.083 (8.3%)

Conclusion: Parameters stable → can use fixed threshold
```

### 2.5 OVSM Implementation: Complete Walk-Forward Framework

```lisp
;;═══════════════════════════════════════════════════════════════════════════
;; WALK-FORWARD ANALYSIS FRAMEWORK
;;═══════════════════════════════════════════════════════════════════════════
;;
;; WHAT: Validates strategy by optimizing on past, testing on future (repeatedly)
;; WHY: Single train/test split can be lucky; multiple tests more robust
;; HOW: Rolling or anchored windows with optimization → test → re-optimize
;;
;; KEY PRINCIPLE: Never use future data in past decisions!
;;═══════════════════════════════════════════════════════════════════════════

(define (walk-forward-analysis strategy data
                               :train-window 252
                               :test-window 63
                               :window-type "rolling"
                               :reoptimize true)
  (do
    ;;─── STEP 1: Validate inputs ───
    (define n (length data))
    (define min-required (+ train-window test-window))

    (if (< n min-required)
        (error (format "Insufficient data: need {} days, have {}"
                       min-required n)))

    ;;─── STEP 2: Calculate number of windows ───
    (define num-windows
      (floor (/ (- n train-window) test-window)))

    (log :message (format "Running {} walk-forward windows..." num-windows))

    ;;─── STEP 3: Loop through windows ───
    (define results (array))

    (for (window-idx (range 0 num-windows))
      (let* ((test-start (+ train-window (* window-idx test-window)))
             (test-end (+ test-start test-window))

             ;; Train window depends on anchored vs rolling
             (train-start
               (if (= window-type "anchored")
                   0                              ; Anchored: always start from beginning
                   (- test-start train-window)))  ; Rolling: fixed-size window

             (train-data (slice data train-start test-start))
             (test-data (slice data test-start test-end)))

        (log :message (format "Window {}: Train [{}-{}], Test [{}-{}]"
                              window-idx train-start test-start
                              test-start test-end))

        ;;─── STEP 4: Optimize on training data ───
        (define optimal-params
          (if reoptimize
              (optimize-strategy strategy train-data)
              (get (last results) :params)))  ; Use previous params if not reoptimizing

        ;;─── STEP 5: Test on out-of-sample data ───
        (define test-metrics
          (backtest strategy test-data optimal-params))

        ;;─── STEP 6: Store results ───
        (push! results
               {:window window-idx
                :train-period {:start train-start :end test-start}
                :test-period {:start test-start :end test-end}
                :params optimal-params
                :metrics test-metrics})))

    ;;─── STEP 7: Aggregate performance ───
    (define all-returns
      (flatten (map (lambda (r) (get (get r :metrics) :returns)) results)))

    (define aggregate
      {:sharpe (sharpe-ratio all-returns)
       :total-return (cumulative-return all-returns)
       :max-drawdown (max-drawdown all-returns)
       :win-rate (win-rate all-returns)
       :num-trades (length all-returns)})

    ;;─── STEP 8: Parameter stability analysis ───
    (define param-values
      (map (lambda (r) (get (get r :params) :primary-param)) results))

    (define param-stability
      {:mean (mean param-values)
       :std (std param-values)
       :cv (/ (std param-values) (mean param-values))
       :min (min param-values)
       :max (max param-values)})

    ;;─── RETURN COMPLETE ANALYSIS ───
    {:window-results results
     :aggregate-metrics aggregate
     :parameter-stability param-stability
     :windows-tested num-windows
     :interpretation
       (format "Walk-forward Sharpe: {:.2f} | Param CV: {:.1f}% | {} windows"
               (get aggregate :sharpe)
               (* 100 (get param-stability :cv))
               num-windows)}))
```

---

## SECTION 3: Event-Driven Architecture - Simulating Reality (~2,500 words)

**Pedagogical Goal:** Teach how to build backtests that simulate real trading.

### 3.1 Vectorized vs. Event-Driven

**Vectorized Backtest (Fast but Unrealistic):**
```python
# Compute signals for all timesteps at once
signals = (data['close'] > data['sma_50']).astype(int)
returns = signals.shift(1) * data['returns']  # Apply signals

# Problem: Assumes perfect execution at close prices!
```

**Event-Driven Backtest (Slower but Realistic):**
```python
# Process one event at a time, like real trading
for timestamp in data.index:
    market_event = get_market_data(timestamp)  # Receive data
    signal = strategy.calculate_signal(market_event)  # Make decision
    if signal:
        order = create_order(signal)  # Generate order
        fill = execute_order(order, current_market)  # Simulate execution
```

### 3.2 The Event Queue Architecture

**Core Components:**

```
┌─────────────┐
│ Event Queue │ ← Central coordinator
└─────────────┘
      ↓ ↑
┌─────────────┐
│ Data Handler│ → Generates MarketEvent (new bar/tick)
└─────────────┘
      ↓
┌─────────────┐
│  Strategy   │ → Generates SignalEvent (buy/sell decision)
└─────────────┘
      ↓
┌─────────────┐
│  Portfolio  │ → Generates OrderEvent (size, type)
└─────────────┘
      ↓
┌─────────────┐
│  Execution  │ → Generates FillEvent (actual execution)
└─────────────┘
      ↓ (back to queue)
```

### 3.3 Event Types and Their Responsibilities

**MarketEvent:**
```lisp
{:type "market"
 :timestamp "2023-11-14T09:30:00"
 :symbol "ETH/USD"
 :open 2000.00
 :high 2010.00
 :low 1995.00
 :close 2005.00
 :volume 1250000}
```

**SignalEvent:**
```lisp
{:type "signal"
 :timestamp "2023-11-14T09:30:01"  ; 1ms after market data
 :symbol "ETH/USD"
 :direction "LONG"                  ; or "SHORT", "EXIT"
 :strength 0.75                     ; Confidence 0-1
 :strategy-id "pairs-trading-eth-btc"}
```

**OrderEvent:**
```lisp
{:type "order"
 :timestamp "2023-11-14T09:30:02"  ; 1ms after signal
 :symbol "ETH/USD"
 :order-type "MARKET"               ; or "LIMIT", "STOP"
 :quantity 10                       ; Size determined by portfolio manager
 :direction "BUY"
 :limit-price null                  ; For limit orders
 :stop-price null}                  ; For stop orders
```

**FillEvent:**
```lisp
{:type "fill"
 :timestamp "2023-11-14T09:30:02.005"  ; 5ms latency
 :symbol "ETH/USD"
 :quantity 10
 :direction "BUY"
 :fill-price 2005.15                    ; Slippage: 0.15 above close
 :commission 2.01                       ; 0.10% of $2005.15 * 10
 :slippage-bps 7.5                      ; 0.075%
 :exchange "Binance"}
```

### 3.4 Slippage and Market Impact Models

**Fixed Slippage (Simple):**
```lisp
(define (apply-fixed-slippage fill-price direction :bps 10)
  ;; Add/subtract 0.10% for buy/sell
  (if (= direction "BUY")
      (* fill-price (+ 1 (/ bps 10000)))
      (* fill-price (- 1 (/ bps 10000)))))
```

**Variable Slippage (Realistic):**
```lisp
(define (apply-variable-slippage fill-price direction order-size volume
                                   :base-bps 5)
  ;;
  ;; Slippage increases with order size relative to volume
  ;; Square root rule: impact ~ sqrt(order_size / volume)
  ;;
  (define volume-pct (/ order-size volume))
  (define impact-multiplier (sqrt (* volume-pct 100)))  ; Scale factor
  (define total-slippage-bps (* base-bps impact-multiplier))

  ;; Apply directional slippage
  (if (= direction "BUY")
      (* fill-price (+ 1 (/ total-slippage-bps 10000)))
      (* fill-price (- 1 (/ total-slippage-bps 10000)))))

;; Example:
;; Order: $100k, Daily volume: $10M
;; Volume %: 1%
;; Impact multiplier: sqrt(1) = 1x
;; Slippage: 5 bps * 1 = 5 bps (0.05%)

;; Order: $1M, Daily volume: $10M
;; Volume %: 10%
;; Impact multiplier: sqrt(10) = 3.16x
;; Slippage: 5 bps * 3.16 = 15.8 bps (0.158%)
```

---

## SECTION 4: Transaction Costs - The Reality Tax (~1,500 words)

**Pedagogical Goal:** Quantify the hidden costs that kill strategies.

### 4.1 The Complete Cost Model

**Five Components:**

1. **Commission:** Fixed or percentage-based fee
   - Retail: 0.10-0.20% per side
   - Pro: 0.02-0.05% per side
   - Crypto maker: -0.01% (rebate!) to +0.05%

2. **Slippage:** Difference between decision price and execution price
   - Liquid markets: 0.01-0.05%
   - Illiquid markets: 0.10-0.50%
   - Market orders during volatility: 0.50-2.00%

3. **Spread:** Bid-ask spread you cross
   - SPY: 0.01% (1 cent on $300)
   - Mid-cap stock: 0.05-0.10%
   - Crypto: 0.02-0.20%

4. **Market Impact:** Your order moves the price
   - Small orders: ~0%
   - Large orders (>1% volume): 0.10-0.50%
   - Very large (>5% volume): 0.50-2.00%

5. **Opportunity Cost:** Delayed execution
   - Waiting for limit fill: potential missed move
   - Quantify as: (move while waiting) × (probability of missing)

**Total Realistic Cost:**
```
Typical day trade (liquid market):
Commission: 0.10%
Slippage: 0.03%
Spread: 0.02%
Impact: 0.01%
Total: 0.16% per side × 2 = 0.32% round-trip

For a strategy with 1% expected return per trade:
Net after costs: 1.00% - 0.32% = 0.68% (lost 32% of edge!)
```

### 4.2 Cost Impact by Strategy Type

| Strategy Type | Typical Holding | Trades/Year | Annual Cost Impact |
|---------------|-----------------|-------------|-------------------|
| **High-frequency** | Minutes-hours | 1000+ | 320%+ of gross (often underwater) |
| **Day trading** | Hours-days | 250+ | 80-160% |
| **Swing trading** | Days-weeks | 50-100 | 16-32% |
| **Position trading** | Weeks-months | 10-20 | 3-6% |
| **Pairs trading** | Days | 100-200 | 32-64% |

**The Lesson:** Higher frequency requires exponentially tighter edge.

---

## SECTION 5: Performance Metrics That Matter (~1,500 words)

**Pedagogical Goal:** Teach which metrics reveal strategy quality.

### 5.1 The Sharpe Ratio - The Standard

**Formula:**
$$\text{Sharpe} = \frac{E[R - R_f]}{\sigma(R)}$$

Where:
- $R$ = strategy returns
- $R_f$ = risk-free rate (usually ignored for crypto/stocks)
- $\sigma(R)$ = standard deviation of returns

**Interpretation:**
```
Sharpe < 0.5: Poor (not worth trading)
Sharpe 0.5-1.0: Decent (might be tradeable)
Sharpe 1.0-2.0: Good (solid strategy)
Sharpe 2.0-3.0: Excellent (rare, verify carefully)
Sharpe > 3.0: Suspicious (likely overfitting or data issues)
```

**Problem with Sharpe:**
- Penalizes upside volatility same as downside
- Assumes normal distribution (ignores fat tails)
- Unstable for short time periods

### 5.2 The Sortino Ratio - Downside Focus

**Formula:**
$$\text{Sortino} = \frac{E[R - R_f]}{\sigma_{\text{downside}}(R)}$$

Only penalizes downside volatility.

**When Sortino >> Sharpe:**
Your strategy has large upside moves but controlled downside → asymmetric payoff (good!)

**Example:**
```
Strategy A: Sharpe = 1.5, Sortino = 2.5
→ Lots of upside volatility, little downside
→ Momentum/trend-following profile

Strategy B: Sharpe = 1.5, Sortino = 1.6
→ Symmetric volatility
→ Mean-reversion profile
```

### 5.3 The Calmar Ratio - Drawdown Adjusted

**Formula:**
$$\text{Calmar} = \frac{\text{Annual Return}}{\text{Max Drawdown}}$$

**Why it matters:**
- Drawdown = largest peak-to-trough decline
- Measures how much you must endure to get returns
- Calmar > 1.0 is good (return exceeds worst drawdown)

**Example:**
```
Strategy: 20% annual return, 15% max drawdown
Calmar = 20 / 15 = 1.33

Investor perspective: "I risk losing 15% to make 20%"
(Better than: "I risk 15% to make 12%", Calmar = 0.80)
```

### 5.4 Maximum Drawdown - The Pain Metric

**Formula:**
$$\text{MaxDD} = \max_{t} \left( \frac{\text{Peak}_t - \text{Trough}_t}{\text{Peak}_t} \right)$$

**Why it's critical:**
- Determines position sizing (you must survive the drawdown)
- Psychological pain threshold (30%+ → most people quit)
- Regulatory (many funds have 20% drawdown limits)

**Real-World Impact:**
```
Strategy with 15% max drawdown:
- Can use 2x leverage (30% max loss = tolerable)
- Most investors will stick through it

Strategy with 40% max drawdown:
- Cannot use leverage (80% loss = wipeout)
- Investors will redeem before recovery
```

### 5.5 Win Rate vs. Win/Loss Ratio

**Two paths to profitability:**

**Path 1: High Win Rate, Low Win/Loss**
```
Win rate: 70%
Avg win: $100
Avg loss: $150
Expected value: 0.70 × $100 - 0.30 × $150 = $70 - $45 = $25
```
Example: Mean reversion, pairs trading

**Path 2: Low Win Rate, High Win/Loss**
```
Win rate: 35%
Avg win: $400
Avg loss: $100
Expected value: 0.35 × $400 - 0.65 × $100 = $140 - $65 = $75
```
Example: Trend following, momentum

**Neither is better—just different psychology:**
- High win rate: Feels good, frequent wins
- High win/loss: Painful losing streaks, rare big wins

---

## SECTION 6: Common Pitfalls and Fixes (~1,500 words)

**Pedagogical Goal:** Teach the seven deadly sins and their antidotes.

### 6.1 Survivorship Bias

**Problem:** Testing on current index members misses bankruptcies.

**Example:**
```
Strategy: "Buy stocks with P/E < 10"
Backtest on 2024 S&P 500 members: +12% annual
Backtest on point-in-time data: +7% annual

Missing: Lehman Brothers, Bear Stearns, Enron, etc. (P/E was low before collapse)
```

**Fix:**
- Use point-in-time data (expensive but necessary)
- Or: Test on current universe but add 30% penalty to returns
- Or: Focus on highly liquid assets (less prone to delisting)

### 6.2 Look-Ahead Bias

**Problem:** Using future data in past decisions.

**Sneaky Examples:**
```python
# WRONG - Uses all data to normalize
data['normalized'] = (data['price'] - data['price'].mean()) / data['price'].std()

# RIGHT - Uses only past data
data['normalized'] = data['price'].rolling(252).apply(lambda x: (x[-1] - x.mean()) / x.std())
```

**Fix:**
- Always use `.shift()` or explicit time checks
- Never use `.iloc[-1]` or `.tail()` in backtests
- Test: "Could I have known this at timestamp T?"

### 6.3 Data Snooping

**Problem:** Testing many strategies, reporting only the best.

**The Math:**
```
Test 100 strategies at 5% significance
Expected false positives: 100 × 0.05 = 5

You'll find 5 "significant" strategies that are pure luck!
```

**Fix: Bonferroni Correction**
```
Adjusted p-value = 0.05 / number_of_tests
Example: 100 tests → require p < 0.0005 (not 0.05)
```

**Or: White's Reality Check**
Test your strategy against random variations to see if it's actually better than luck.

---

## SECTION 7: Complete OVSM Backtesting Framework (~2,000 words)

**Pedagogical Goal:** Provide production-ready, event-driven backtester.

[Full OVSM implementation with event queue, slippage models, transaction costs, walk-forward analysis, and all performance metrics]

---

## SECTION 8: Summary & Deployment Checklist (~1,000 words)

**Key Takeaways:**
1. Most backtests lie—learn to spot the lies
2. Walk-forward analysis prevents overfitting
3. Event-driven architecture simulates reality
4. Transaction costs often exceed strategy edge
5. Sharpe alone is insufficient—use Sortino, Calmar, max drawdown

**Pre-Deployment Checklist:**
```
☐ Walk-forward analysis completed (5+ windows)
☐ Sharpe degradation < 30% (in-sample → out-of-sample)
☐ Transaction costs modeled realistically (0.2-0.5% round-trip)
☐ Maximum drawdown acceptable (< 20% for most investors)
☐ Tested across multiple regimes (bull, bear, sideways)
☐ Parameter stability checked (CV < 0.30)
☐ No look-ahead bias (code reviewed)
☐ Point-in-time data used (or survivorship penalty applied)
☐ Slippage model includes market impact
☐ Calmar ratio > 0.5 (returns justify drawdown)
```

**Next Chapter:** Production Systems (Chapter 10)

---

## REFERENCES

1. Bailey, D. H., Borwein, J., López de Prado, M., & Zhu, Q. J. (2014). "The Probability of Backtest Overfitting." Journal of Computational Finance.

2. Pardo, R. (2008). "The Evaluation and Optimization of Trading Strategies" (2nd ed.). Wiley.

3. Chan, E. P. (2013). "Algorithmic Trading: Winning Strategies and Their Rationale." Wiley.

4. Aronson, D. R. (2007). "Evidence-Based Technical Analysis." Wiley.

5. White, H. (2000). "A Reality Check for Data Snooping." Econometrica, 68(5), 1097-1126.

6. Harvey, C. R., & Liu, Y. (2015). "Backtesting." Journal of Portfolio Management.

7. López de Prado, M. (2018). "Advances in Financial Machine Learning." Wiley.

8. Narang, R. K. (2013). "Inside the Black Box" (2nd ed.). Wiley.

---

**Total Estimated Word Count:** ~14,500 words
