# Chapter 8: Time Series Analysis

> *"In August 1998, Long-Term Capital Management lost $4.6 billion in 4 months. Two Nobel Prize winners sat on the board. Their models showed 10-sigma events happening daily—events that, according to their statistical assumptions, should occur once in the lifetime of the universe. What went wrong? They assumed the past was stationary."*

---

## Opening: The $4.6 Billion Lesson in Non-Stationarity

### The Setup: Genius Meets Hubris

In the mid-1990s, Long-Term Capital Management (LTCM) represented the pinnacle of quantitative finance. Founded by John Meriwether (former Salomon Brothers vice-chairman) and featuring Myron Scholes and Robert Merton (who would share the 1997 Nobel Prize in Economics), LTCM's partners included some of the brightest minds in finance.

Their strategy was elegant: statistical arbitrage based on **convergence trading**. They identified pairs of bonds, currencies, or equity indices that historically moved together—relationships validated by years of data showing **cointegration**. When these spreads widened beyond historical norms, LTCM would bet on convergence, earning small but "certain" profits.

By 1998, LTCM managed $4.6 billion in capital with over $125 billion in assets (27:1 leverage) and derivative positions with notional values exceeding $1 trillion. Their models, built on sophisticated time series analysis, showed annual volatility under 10% with expected returns of 40%+. Investors paid a 2% management fee and 25% performance fee for access to these Nobel-laureate-designed "arbitrage-free" trades.

### The Collapse: When Stationarity Breaks

On August 17, 1998, Russia defaulted on its domestic debt. What LTCM's models predicted as a 3-standard-deviation event (should happen 0.3% of the time) cascaded into something unprecedented:

**Week 1 (Aug 17-21):**
- Treasury-to-Treasury spreads (on-the-run vs off-the-run) widened 300%
- LTCM's cointegration models expected mean reversion within days
- Instead, spreads kept widening
- Loss: $550 million (12% of capital)

**Week 2 (Aug 24-28):**
- Global flight to quality accelerated
- Liquidity evaporated across markets
- Historical correlations collapsed—assets that "never" moved together did
- Spreads that typically reverted in 5-10 days showed no signs of convergence
- Loss: $750 million (20% of remaining capital)

**By September 23:**
- LTCM's capital had fallen from $4.6B to $400M (91% loss)
- They faced margin calls they couldn't meet
- Federal Reserve orchestrated a $3.6B bailout by 14 banks
- Spreads that "always" converged took **months** to revert, not days

```mermaid
timeline
    title LTCM Collapse Timeline - When Stationarity Broke
    section 1994-1997
        1994 : Fund Launch : $1.25B capital : Nobel laureates join
        1995-1996 : Glory Years : 40%+ annual returns : $7B under management
        1997 : Peak Performance : 17% return : Models validated
    section 1998 Crisis
        Jan-Jul 1998 : Warning Signs : Asia crisis : Volatility rising : Models still profitable
        Aug 17 : Russian Default : 3-sigma event : Spreads widen 300%
        Aug 17-21 : Week 1 : $550M loss (12%) : Mean reversion expected
        Aug 24-28 : Week 2 : $750M loss (20%) : Correlations collapse
        Sep 1-15 : Acceleration : Daily 5-sigma events : Liquidity vanishes
        Sep 23 : Bailout : $400M remains (91% loss) : Fed orchestrates $3.6B rescue
    section Aftermath
        1999-2000 : Liquidation : Positions unwound over months : Spreads finally converge
        2000 : Lessons : Liquidity risk recognized : Tail risk management born
```

### What Went Wrong: Three Fatal Statistical Assumptions

**1. Stationarity Assumption**

LTCM assumed that historical mean and variance were stable predictors of the future. Their models were trained on data from the 1990s—a period of declining volatility and increasing globalization. They extrapolated these patterns indefinitely.

**The Reality:** Financial time series exhibit **regime changes**. The "calm 1990s" regime ended abruptly in August 1998. Volatility patterns, correlation structures, and liquidity profiles all shifted simultaneously.

**Time Series Lesson:** Before modeling any data, we must test whether it's stationary—whether past statistical properties persist. The Augmented Dickey-Fuller (ADF) test, applied to LTCM's spreads in rolling windows, would have shown deteriorating stationarity going into August 1998.

**2. Constant Correlation Assumption**

LTCM's pairs were chosen based on historical cointegration—the property that two non-stationary price series have a stationary linear combination (spread). For example, if German and Italian government bonds typically trade with a 50 basis point spread, deviations from this spread should mean-revert.

Their models showed:
```
Typical spread half-life: 7-10 days
Maximum historical spread: 80 basis points
August 1998 spread: 150+ basis points (unprecedented)
```

**The Reality:** Cointegration is not a permanent property. It exists within regimes but breaks during crises when capital flows overwhelm fundamental relationships.

**Time Series Lesson:** Cointegration tests (Engle-Granger, Johansen) must be performed in **rolling windows**, not just on full historical samples. A pair cointegrated from 1995-1997 may decouple in 1998.

**3. Gaussian Error Distribution**

LTCM's risk models assumed returns followed a normal distribution. Their Value-at-Risk (VaR) calculations, based on this assumption, showed:
- 1% daily VaR: $50 million
- Expected 3-sigma events: Once per year
- Expected 5-sigma events: Once per 14,000 years

**The Reality in August 1998:**
- Actual losses: $300M+ per week (6+ sigma events)
- 5-sigma-equivalent events: **Daily** for three weeks
- Fat-tailed distributions: Actual returns had kurtosis > 10 (normal = 3)

**Time Series Lesson:** Financial returns exhibit **volatility clustering** (GARCH effects), **fat tails**, and **autocorrelation in absolute returns**. Models must account for these stylized facts, not impose Gaussian assumptions.

### The Aftermath: Lessons for Modern Quant Trading

LTCM's collapse fundamentally changed quantitative finance:

1. **Liquidity Risk Recognized:** Models must account for execution costs during stress
2. **Tail Risk Management:** VaR alone is insufficient; focus on **Expected Shortfall** and stress testing
3. **Dynamic Modeling:** Use adaptive techniques (Kalman filters, regime-switching models) rather than static historical parameters
4. **Systematic Backtesting:** Walk-forward validation across different market regimes, not just in-sample optimization

**The Central Lesson for This Chapter:**

> **Time series analysis is not about predicting the future. It's about rigorously testing when the past stops being relevant.**

The tools we'll learn—stationarity tests, cointegration analysis, ARIMA modeling, Kalman filtering—are not crystal balls. They are diagnostic instruments that tell us:
- **When** historical patterns are stable enough to trade
- **How** to detect regime changes before catastrophic losses
- **What** to do when relationships break down (exit positions, reduce leverage, stop trading)

LTCM had brilliant models. What they lacked was humility about their models' assumptions and tools to detect when those assumptions failed. We'll build both.

---

## Section 1: Why Prices Aren't Random Walks

### 1.1 The Efficient Market Hypothesis vs Reality

The **Efficient Market Hypothesis** (EMH), popularized by Eugene Fama, claims that asset prices follow a random walk:

$$P_t = P_{t-1} + \epsilon_t$$

Where $\epsilon_t$ is white noise (independent, identically distributed random shocks). If true, yesterday's price movement tells us **nothing** about today's. Technical analysis is futile. The past is irrelevant.

**But is it true?** Let's examine real data.

### 1.2 Empirical Evidence: SPY Daily Returns (2010-2020)

Consider the SPDR S&P 500 ETF (SPY), one of the most liquid and efficient markets in the world. If EMH holds anywhere, it should hold here.

```
Data: 2,516 daily log returns
Period: Jan 2010 - Dec 2020
```

**Test 1: Autocorrelation in Returns**

If returns are truly random walk, today's return should be uncorrelated with yesterday's:
$$\text{Corr}(R_t, R_{t-1}) \approx 0$$

**Actual Result:**
```
Lag-1 autocorrelation: -0.04 (p = 0.047)
```

At first glance, this seems to confirm EMH: -0.04 is tiny. But:
1. It's statistically significant (p < 0.05)
2. At high frequency (hourly crypto), autocorrelation reaches 0.15+
3. Even 4% autocorrelation is exploitable with sufficient volume

**Test 2: Autocorrelation in Absolute Returns**

Even if returns themselves aren't autocorrelated, **volatility** might be:

$$\text{Corr}(|R_t|, |R_{t-1}|) = ?$$

**Actual Result:**
```
Lag-1 autocorrelation of |R_t|: 0.31 (p < 0.001)
Lag-5 autocorrelation: 0.18 (p < 0.001)
```

**This is massive.** High volatility today strongly predicts high volatility tomorrow. This is **volatility clustering**—a violation of the random walk assumption.

**Visual Evidence:**

```
2010-2012: Calm period (daily vol ≈ 0.5%)
2015-2016: China slowdown (vol spikes to 1.5%)
Mar 2020: COVID crash (vol explodes to 5%+)
Apr-Dec 2020: Return to calm (vol ≈ 0.8%)
```

Volatility clearly clusters in time. This is **temporal dependence**—the foundation of time series analysis.

**Test 3: Fat Tails**

Random walk assumes Gaussian errors: $\epsilon_t \sim \mathcal{N}(0, \sigma^2)$

For a normal distribution:
- Kurtosis = 3
- 3-sigma events: 0.3% of the time (once per 1.5 years daily data)
- 5-sigma events: 0.00006% (once per 14,000 years)

**Actual SPY Results:**
```
Kurtosis: 7.8
3-sigma events: 2.1% (42 occurrences in 10 years vs. 8 expected)
5-sigma events: 3 occurrences (vs. 0 expected)
```

**Returns have fat tails.** Extreme events happen 5-6 times more frequently than the normal distribution predicts. Any model assuming Gaussian errors will catastrophically underestimate tail risk (see: LTCM, 2008 crisis, COVID crash).

### 1.3 Three Patterns We Exploit

If prices aren't purely random walks, what patterns exist?

**Pattern 1: Autocorrelation (Momentum)**

**What It Is:** Today's return predicts tomorrow's in the same direction.

**Where It Appears:**
- High-frequency crypto (hourly returns: ρ ≈ 0.10-0.15)
- Post-earnings momentum (drift continues 1-5 days)
- Intraday SPY (first-hour return predicts last-hour)

**How to Model:** Autoregressive (AR) models
$$R_t = \phi R_{t-1} + \epsilon_t$$

If $\phi > 0$: Momentum exists. If today's return is +1%, expect tomorrow's to be $+\phi\%$ on average.

**Trading Implication:**
```
IF hourly_return > 0.5%:
    GO LONG (expect continuation)
IF hourly_return < -0.5%:
    GO SHORT
```

**Pattern 2: Mean Reversion (Pairs Trading)**

**What It Is:** Spreads between cointegrated assets revert to a long-run mean.

**Where It Appears:**
- ETH/BTC ratio (mean-reverts with 5-10 day half-life)
- Competing stocks (KO vs PEP, CVX vs XOM)
- Yield spreads (10Y-2Y treasuries)

**How to Model:** Cointegration + Error Correction Models (ECM)

$$\Delta Y_t = \gamma(Y_{t-1} - \beta X_{t-1}) + \epsilon_t$$

If $\gamma < 0$: Deviations correct. If spread widens, it's likely to narrow.

**Trading Implication:**
```
Spread = ETH - β×BTC
IF z-score(Spread) > 2:
    SHORT spread (sell ETH, buy BTC)
IF z-score(Spread) < -2:
    LONG spread (buy ETH, sell BTC)
```

**Pattern 3: Volatility Clustering (Options Pricing)**

**What It Is:** High volatility begets high volatility. Calm periods stay calm.

**Where It Appears:**
- Equity indices (VIX persistence)
- FX markets (crisis correlation)
- Crypto (cascading liquidations)

**How to Model:** GARCH models (covered in Chapter 12)

$$\sigma_t^2 = \omega + \alpha \epsilon_{t-1}^2 + \beta \sigma_{t-1}^2$$

If $\alpha + \beta \approx 1$: Volatility shocks are persistent.

**Trading Implication:**
```
IF realized_vol > historical_avg:
    BUY options (implied vol will rise)
    REDUCE position sizes (larger potential moves)
```

### 1.4 The Framework: Three Questions Every Trade Must Answer

Before deploying any time series trading strategy, we must answer:

**Question 1: Will This Pattern Persist?**
**Tool:** Stationarity tests (ADF, KPSS)

Non-stationary patterns are dangerous. A "momentum" strategy that worked in the 1990s bull market may fail in a sideways market. We need to test whether our statistical properties (mean, variance, autocorrelation) are stable.

**Question 2: How Strong Is the Pattern?**
**Tool:** Autocorrelation analysis, cointegration tests

Even if a pattern exists, it might be too weak to overcome transaction costs. We need to quantify:
- Autocorrelation magnitude ($\rho$ = ?)
- Cointegration strength (half-life = ?)
- Signal-to-noise ratio (Sharpe ratio)

**Question 3: When Will It Break?**
**Tool:** Structural break tests, rolling window validation

Markets change. Regulation shifts. Technology evolves. A pairs trade that worked pre-algo-trading may fail post-algo. We need early warning systems to detect regime changes.

**Example Workflow:**

```lisp
;; Pre-trade checklist (simplified)
(define (validate-strategy data)
  (do
    ;; Q1: Is data stationary?
    (define adf (adf-test data))
    (if (not (get adf :reject-null))
        (error "Non-stationary - cannot trust historical stats"))

    ;; Q2: Is autocorrelation significant?
    (define acf-lag1 (autocorr data :lag 1))
    (if (< (abs acf-lag1) 0.05)
        (error "Autocorrelation too weak to exploit"))

    ;; Q3: Is it stable in rolling windows?
    (define rolling (rolling-autocorr data :window 252 :step 21))
    (define stability (std rolling))
    (if (> stability 0.10)
        (error "Parameter instability - pattern not reliable"))

    {:approved true
     :autocorr acf-lag1
     :stability stability}))
```

**The Rest of This Chapter:**

We'll learn each of these tools in depth:
- **Section 2:** Stationarity testing (ADF, KPSS, unit roots)
- **Section 3:** ARIMA models (capturing autocorrelation)
- **Section 4:** Cointegration (pairs trading foundation)
- **Section 5:** Kalman filters (adaptive parameter tracking)
- **Section 6:** Spectral analysis (cycle detection)
- **Section 7:** Practical implementation

By the end, you'll have production-ready OVSM code to:
- Test any time series for stationarity
- Build ARIMA forecasting models
- Identify cointegrated pairs
- Track time-varying parameters
- Detect when relationships break down

Let's begin with the foundation: **stationarity**.

---

## Section 2: Stationarity - The Foundation of Time Series Analysis

### 2.1 What Stationarity Really Means (Intuitive Definition)

Before diving into mathematical definitions and tests, let's build intuition.

**Intuitive Definition:**
> "A time series is stationary if its statistical properties don't change when we shift the time axis."

**Concrete Test:**
1. Split SPY daily closing prices into two periods:
   - **Period A:** 2010-2015 (1,260 trading days)
   - **Period B:** 2015-2020 (1,256 trading days)

2. Compute statistics for each period:

**Price Levels (Non-Stationary):**
```
Period A: Mean = $130, Std Dev = $25, Min = $94, Max = $211
Period B: Mean = $250, Std Dev = $35, Min = $181, Max = $340
```

The mean doubled! Variance increased. If we trained a model on Period A ("typical price is $130 ± $25"), it would catastrophically fail in Period B when prices are $250+.

**Log Returns (Stationary):**
```
Period A: Mean = 0.052%, Std Dev = 0.95%, Skew = -0.3, Kurt = 7.2
Period B: Mean = 0.048%, Std Dev = 1.01%, Skew = -0.5, Kurt = 7.8
```

Much more stable! Mean and std dev nearly identical. Skew and kurtosis similar (both showing slight negative skew and fat tails). A model trained on Period A would remain valid in Period B.

**The Insight:**
> Price levels trend (non-stationary), but returns oscillate around zero (stationary). This is why we trade on returns, spreads, or changes—not raw prices.

**Practical Test for Stationarity:**

Imagine you have a time series but **don't know the time axis**. If you can't tell whether it's from 2010 or 2020 by looking at the statistical distribution, it's stationary.

- **Price levels:** You can immediately tell the decade (2010 = $130, 2020 = $330)
- **Returns:** They look the same (centered near 0, ~1% daily vol)

### 2.2 The Three Mathematical Conditions

Formally, a time series $\{X_t\}$ is **weakly stationary** if:

**Condition 1: Constant Mean**
$$E[X_t] = \mu \quad \forall t$$

The expected value doesn't depend on time. At any point—2010, 2015, 2025—the average value is the same.

**Why It Matters:**
- If mean = $100 in 2010 but $200 in 2020, we can't pool data
- Regression coefficients estimated on 2010 data won't apply to 2020
- Forecasting becomes impossible (which mean do we forecast toward?)

**Example Violation:**
- **GDP:** Grows over time (mean increases)
- **Stock prices:** Trend upward (mean changes)
- **Asset allocations:** Shift with risk-on/risk-off regimes

**Example Satisfaction:**
- **Log returns:** Centered around small positive value (~0.05% daily for SPY)
- **Yield spreads:** Oscillate around historical average
- **Pair spreads:** Deviations from equilibrium have stable mean (if cointegrated)

**Condition 2: Constant Variance**
$$\text{Var}(X_t) = \sigma^2 \quad \forall t$$

The variability around the mean doesn't change over time.

**Why It Matters:**
- If volatility doubles during crises, our risk models break
- Option pricing assumes volatility is predictable
- Confidence intervals widen/narrow if variance isn't constant

**Example Violation:**
- **SPY returns during COVID (Mar 2020):** Daily std dev jumped from 0.8% → 5%+
- **GARCH effects:** $\sigma_t^2$ depends on past shocks (volatility clustering)

**Example Satisfaction (approximately):**
- **Short-term interest rates:** Fairly stable variance (when not near zero bound)
- **Scaled returns:** $R_t / \sigma_t$ (if we model volatility explicitly with GARCH)

**Condition 3: Autocovariance Depends Only on Lag**
$$\text{Cov}(X_t, X_{t-k}) = \gamma_k \quad \forall t$$

The correlation between observations separated by $k$ periods is the same regardless of when you measure it.

**Why It Matters:**
- If correlation structure changes over time, AR/MA models have unstable parameters
- A momentum strategy that worked in 2010 (ρ₁ = 0.15) might fail in 2020 if ρ₁ drops to 0.02

**Example Violation:**
- **Market regimes:** Bull markets have positive autocorrelation (momentum), bear markets have negative (mean reversion)

**Example Satisfaction:**
- **White noise:** Cov(X_t, X_{t-k}) = 0 for all k ≠ 0
- **Stable AR process:** If $R_t = 0.1·R_{t-1} + \epsilon_t$ holds constantly over time

### 2.3 Strong vs Weak Stationarity

**Strong (Strict) Stationarity:**
All statistical properties (mean, variance, skewness, kurtosis, all higher moments, entire distribution) are time-invariant.

**Weak (Covariance) Stationarity:**
Only first two moments (mean, variance) and autocovariances are time-invariant.

**In Practice:**
- We almost always assume weak stationarity (it's testable and sufficient for most models)
- Strong stationarity is too strict (empirically, higher moments do vary slightly over time)
- ARIMA, GARCH, cointegration all require only weak stationarity

### 2.4 Why Stationarity Matters: Three Catastrophic Failures

**Failure 1: Spurious Regression**

**The Problem:** Regressing one non-stationary series on another can show high R² purely by chance.

**Classic Example: Ice Cream Sales vs Drowning Deaths**

```
Both variables trend upward over summer months:
- Ice cream sales: $10k (Jan) → $100k (Aug) → $10k (Dec)
- Drowning deaths: 5 (Jan) → 50 (Aug) → 5 (Dec)

Regression: Drownings = α + β·IceCreamSales
Result: R² = 0.92, β is "highly significant" (p < 0.001)

Interpretation: Ice cream causes drowning? NO!
Reality: Both are driven by temperature (omitted variable)
```

**In Finance:**
```
Regress Bitcoin on Nasdaq (both trending up 2010-2020):
Result: R² = 0.85, looks like strong relationship

Reality: Both driven by:
- Tech enthusiasm
- Low interest rates
- Risk-on sentiment

If rates rise and tech corrects, relationship breaks
```

**The Fix:** Test for stationarity. If both series are non-stationary, test for **cointegration** (Section 4) instead of naive regression.

**Failure 2: Invalid Statistical Inference**

**The Problem:** t-statistics and p-values assume independent observations. Non-stationary data violates this.

**Example:**
```
Strategy: "Buy SPY when price > 100-day MA"
Backtest 2010-2020: 10,000 daily observations

Naive test:
- 5,200 days above MA (52%)
- 4,800 days below MA (48%)
- Chi-square test: p = 0.04 (significant!)
- Conclusion: Trend following works?

Problem: Observations aren't independent!
- Once above MA, likely to stay above for weeks (trending)
- Effective sample size ≈ 500 (not 10,000)
- Actual p-value ≈ 0.4 (not significant)
```

**The Fix:** Use **Newey-West standard errors** (adjust for autocorrelation) or test on stationary transformations (returns, not prices).

**Failure 3: Forecasting Breakdown**

**The Problem:** Forecasting non-stationary data requires predicting the trend—which is unknowable.

**Example: Forecasting SPY Price in 2025**

```
Model trained on 2010-2020 prices:
ARIMA(2,0,0): P_t = 150 + 0.8·P_{t-1} - 0.3·P_{t-2} + ε

Forecast for Jan 2025:
- If 2024 ended at $400: Forecast ≈ $410 ± $50
- If 2024 ended at $300: Forecast ≈ $310 ± $50

The forecast is just "slightly above where we are now"—useless!

Why? The model learned:
- 2010-2015: Prices in $100-200 range
- 2015-2020: Prices in $200-350 range

It has no idea whether 2025 will see $500 or $200
```

**Better Approach:**

```
Model returns (stationary):
ARIMA(1,0,0): R_t = 0.0005 + 0.05·R_{t-1} + ε

Forecast for Jan 2025:
- Expected return: 0.05% daily (regardless of price level)
- This translates to: Current_Price × (1 + 0.0005)^days

Now we have a stable, reusable model
```

### 2.5 The Unit Root Problem: Why Prices Random Walk

**The Mathematical Issue:**

Consider a simple model:
$$P_t = \rho P_{t-1} + \epsilon_t$$

- If $|\rho| < 1$: The series is stationary (shocks decay)
- If $\rho = 1$: The series has a **unit root** (shocks persist forever)

**Why "Unit Root"?**

Rewrite as: $(1 - \rho L)P_t = \epsilon_t$, where $L$ is the lag operator ($LP_t = P_{t-1}$)

The characteristic equation is: $1 - \rho z = 0 \Rightarrow z = 1/\rho$

- If $\rho = 1$, the root is $z = 1$ (on the unit circle)
- Hence, "unit root"

**Economic Interpretation:**

If $\rho = 1$:
$$P_t = P_{t-1} + \epsilon_t$$

This is a **random walk**. Today's price = yesterday's price + noise. All past shocks accumulate (infinite memory). The variance grows without bound:

$$\text{Var}(P_t) = \text{Var}(P_0) + t \cdot \sigma_\epsilon^2$$

After $t$ periods, variance is proportional to $t$ (non-stationary).

**Visual Analogy:**

- **Stationary process ($\rho < 1$):** Dog on a leash. It wanders but is pulled back.
- **Unit root process ($\rho = 1$):** Unleashed dog. It wanders indefinitely, never returning.

**Why Stock Prices Have Unit Roots:**

Empirical fact: Stock prices are well-approximated by random walks.

**Intuition:**
- If prices were predictable (mean-reverting), arbitrageurs would exploit it
- E.g., if SPY always returned to $300, you'd buy at $250 and sell at $350 (free money)
- Markets are efficient enough to eliminate such patterns in price levels
- But they're not efficient enough to eliminate ALL patterns (e.g., momentum in returns, cointegration in spreads)

**The Transformation:**

If $P_t$ has a unit root, **first-differencing** makes it stationary:
$$R_t = P_t - P_{t-1} = \epsilon_t$$

Log returns are stationary (approximately), so we can model them with ARIMA.

### 2.6 Testing for Unit Roots: The Augmented Dickey-Fuller (ADF) Test

Now that we understand what stationarity means and why it matters, we need a rigorous statistical test. The **Augmented Dickey-Fuller (ADF) test** is the gold standard.

**The Core Idea:**

We want to test whether $\rho = 1$ (unit root) in:
$$P_t = \rho P_{t-1} + \epsilon_t$$

Subtract $P_{t-1}$ from both sides:
$$\Delta P_t = (\rho - 1) P_{t-1} + \epsilon_t$$

Define $\gamma = \rho - 1$:
$$\Delta P_t = \gamma P_{t-1} + \epsilon_t$$

**The Hypothesis Test:**
- **Null hypothesis ($H_0$):** $\gamma = 0$ (equivalently, $\rho = 1$, unit root exists, series is non-stationary)
- **Alternative ($H_1$):** $\gamma < 0$ (equivalently, $\rho < 1$, no unit root, series is stationary)

**Why "Augmented"?**

The basic Dickey-Fuller test assumes errors are white noise. In reality, errors are often autocorrelated. The ADF test "augments" the regression by adding lagged differences:

$$\Delta P_t = \alpha + \beta t + \gamma P_{t-1} + \sum_{i=1}^{p} \delta_i \Delta P_{t-i} + \epsilon_t$$

Where:
- $\alpha$ = constant term (drift)
- $\beta t$ = linear time trend (optional)
- $\gamma P_{t-1}$ = the key coefficient we're testing
- $\sum_{i=1}^{p} \delta_i \Delta P_{t-i}$ = lagged differences to absorb autocorrelation

**Three Model Specifications:**

1. **No constant, no trend:** $\Delta P_t = \gamma P_{t-1} + \sum \delta_i \Delta P_{t-i} + \epsilon_t$
   - Use for: Series clearly mean-zero (rare)

2. **Constant, no trend:** $\Delta P_t = \alpha + \gamma P_{t-1} + \sum \delta_i \Delta P_{t-i} + \epsilon_t$
   - Use for: Most financial returns (mean ≠ 0)

3. **Constant + trend:** $\Delta P_t = \alpha + \beta t + \gamma P_{t-1} + \sum \delta_i \Delta P_{t-i} + \epsilon_t$
   - Use for: Price levels with deterministic trend

**Critical Detail: Non-Standard Distribution**

The test statistic $t_\gamma = \gamma / SE(\gamma)$ does **NOT** follow a standard t-distribution under the null. Dickey and Fuller (1979) derived special critical values via simulation. MacKinnon (1996) refined them.

**Standard t-distribution 5% critical value:** -1.96
**ADF 5% critical value (with constant):** -2.86

The ADF critical values are more negative (harder to reject null). This makes sense: we're testing a boundary condition ($\rho = 1$), which creates distribution asymmetry.

### 2.7 Worked Example: Testing SPY Prices vs Returns

Let's apply ADF to real data with full transparency.

**Data:**
- SPY daily closing prices, January 2020 - December 2020 (252 trading days)
- Source: Yahoo Finance
- Sample: $P_1 = 323.87$ (Jan 2, 2020), $P_{252} = 373.88$ (Dec 31, 2020)

**Step 1: Visual Inspection**

```
Plot: SPY Prices 2020
- Jan: $324
- Feb: $339 (↑)
- Mar: $250 (↓ COVID crash)
- Apr-Dec: Steady climb to $374

Visual conclusion: Clear trend, likely non-stationary
```

**Step 2: Run ADF Test on Prices**

**Model:** $\Delta P_t = \alpha + \gamma P_{t-1} + \delta_1 \Delta P_{t-1} + \epsilon_t$

We'll use 1 lag (p=1) based on AIC/BIC selection (typical for daily financial data).

**Regression Setup:**
- Dependent variable: $\Delta P_t$ (first differences) – 251 observations
- Independent variables:
  - Constant: 1
  - $P_{t-1}$ (lagged level)
  - $\Delta P_{t-1}$ (lagged first difference)

**OLS Results:**

```
Coefficient estimates:
α (const) =  0.198  (SE = 0.091)  [t = 2.18]
γ (P_t-1) = -0.001  (SE = 0.003)  [t = -0.33] ← KEY STAT
δ_1 (ΔP_t-1) = -0.122  (SE = 0.063)  [t = -1.94]

Residual std error: 4.52
R² = 0.02 (low, as expected for differenced data)
```

**The ADF Test Statistic:**

$$t_\gamma = \frac{\gamma}{SE(\gamma)} = \frac{-0.001}{0.003} = -0.33$$

**Critical Values (with constant, n=251):**
```
1%: -3.43
5%: -2.86
10%: -2.57
```

**Decision Rule:** Reject $H_0$ if $t_\gamma <$ critical value

**Result:** $-0.33 > -2.86$
→ **FAIL to reject null hypothesis**
→ **Prices have a unit root (non-stationary)**

**Interpretation:**
The data cannot distinguish between $\gamma = 0$ (random walk) and $\gamma = -0.001$ (very slow mean reversion). For practical purposes, SPY prices behave as a random walk. We should **not** model them directly.

**Step 3: Transform to Log Returns**

$$R_t = \log(P_t / P_{t-1})$$

```
Sample returns (first 5 days):
R_1 = log(323.87/323.87) = 0  (first obs, no prior)
R_2 = log(325.12/323.87) = 0.0038 (+0.38%)
R_3 = log(327.45/325.12) = 0.0071 (+0.71%)
R_4 = log(326.89/327.45) = -0.0017 (-0.17%)
R_5 = log(330.23/326.89) = 0.0102 (+1.02%)
```

**Step 4: Run ADF Test on Returns**

**Model:** $\Delta R_t = \alpha + \gamma R_{t-1} + \delta_1 \Delta R_{t-1} + \epsilon_t$

Note: $\Delta R_t$ is the change in returns (second difference of prices). This might seem odd, but it's mathematically correct for the ADF specification.

**OLS Results:**

```
Coefficient estimates:
α (const) =  0.00005  (SE = 0.0003)  [t = 0.17]
γ (R_t-1) = -0.95     (SE = 0.08)    [t = -11.88] ← KEY STAT
δ_1 (ΔR_t-1) = 0.02   (SE = 0.06)    [t = 0.33]

Residual std error: 0.015
```

**The ADF Test Statistic:**

$$t_\gamma = \frac{-0.95}{0.08} = -11.88$$

**Critical Value (5%):** -2.86

**Decision Rule:** $-11.88 < -2.86$
→ **REJECT null hypothesis**
→ **Returns are stationary** ✓

**Interpretation:**
The coefficient $\gamma = -0.95$ is large and highly significant. This means returns exhibit strong mean reversion (which is expected since returns should oscillate around their mean, not wander indefinitely).

**Step 5: Trading Implications**

```
SPY Prices → Non-stationary → Cannot model directly
SPY Returns → Stationary → Safe to model with ARIMA/GARCH
```

**Strategy Design:**
```
❌ WRONG: "If SPY drops to $300, buy because it will return to $350"
           (Assumes mean reversion in prices – false!)

✓ CORRECT: "If SPY returns show +2% momentum, expect slight continuation"
           (Models returns, which are stationary)

✓ CORRECT: "SPY/QQQ spread is cointegrated, trade deviations"
           (The spread is stationary even if both prices aren't)
```

### 2.8 Choosing Lag Length (p) in ADF Test

The "Augmented" part (lagged differences) is crucial but raises a question: how many lags?

**Too Few Lags:**
- Residuals remain autocorrelated
- ADF test statistic is biased
- May incorrectly reject/accept unit root

**Too Many Lags:**
- Loss of degrees of freedom
- Reduced test power
- May fail to reject unit root when we should

**Three Selection Methods:**

**Method 1: Information Criteria (AIC/BIC)**

Run ADF with p = 0, 1, 2, ..., max_p and select the p that minimizes AIC or BIC.

$$\text{AIC} = n \log(\hat{\sigma}^2) + 2k$$
$$\text{BIC} = n \log(\hat{\sigma}^2) + k \log(n)$$

Where $k$ = number of parameters, $n$ = sample size.

BIC penalizes complexity more strongly (prefer for small samples).

**Method 2: Schwert Criterion (Rule of Thumb)**

$$p_{\max} = \text{floor}\left(12 \left(\frac{T}{100}\right)^{1/4}\right)$$

For daily data:
- T = 252 (1 year): $p_{\max} = 12 \times 1.26 = 15$
- T = 1260 (5 years): $p_{\max} = 12 \times 1.89 = 22$

Then use AIC to select optimal p ≤ $p_{\max}$.

**Method 3: Sequential t-tests**

Start with $p_{\max}$ and test whether the last lag coefficient is significant. If not, drop it and retest with $p-1$.

**Practical Recommendation:**

For financial data:
```
Daily: p = 1 to 5 (use AIC selection)
Weekly: p = 1 to 4
Monthly: p = 1 to 2
```

Higher frequencies need more lags due to microstructure noise.

### 2.9 KPSS Test: The Complementary Perspective

The ADF test has a **conservative bias**: it tends not to reject the null (unit root) even when the series is weakly stationary.

**The Problem:**

ADF null = "unit root exists"
→ Burden of proof is on stationarity
→ If test is inconclusive (low power), we default to "non-stationary"

**The Solution: KPSS Test**

The **Kwiatkowski-Phillips-Schmidt-Shin (KPSS)** test reverses the hypotheses:

- **Null ($H_0$):** Series is stationary
- **Alternative ($H_1$):** Unit root exists

**The Logic:**

By using both tests, we get four possible outcomes:

| ADF Rejects $H_0$ | KPSS Rejects $H_0$ | Interpretation |
|-------------------|--------------------|-----------------------------|
| ✓ (Stationary) | ✗ (Stationary) | **Stationary** (both agree) |
| ✗ (Unit root) | ✓ (Unit root) | **Non-stationary** (both agree) |
| ✓ (Stationary) | ✓ (Unit root) | **Trend-stationary** (detrend first) |
| ✗ (Unit root) | ✗ (Stationary) | **Inconclusive** (increase sample size) |

```mermaid
quadrantChart
    title Stationarity Test Decision Matrix
    x-axis "ADF: Non-Stationary" --> "ADF: Stationary"
    y-axis "KPSS: Stationary" --> "KPSS: Non-Stationary"
    quadrant-1 "⚠️ TREND-STATIONARY<br/>Detrend before modeling"
    quadrant-2 "✓ STATIONARY<br/>Safe to model (BEST)"
    quadrant-3 "? INCONCLUSIVE<br/>Increase sample size"
    quadrant-4 "✗ NON-STATIONARY<br/>Difference or abandon"
    SPY Returns: [0.85, 0.15]
    SPY Prices: [0.15, 0.85]
    BTC Returns: [0.80, 0.20]
    GDP: [0.20, 0.80]
```

**Worked Example: SPY Returns**

**ADF Test:**
- Test statistic: -11.88
- Critical value (5%): -2.86
- **Result:** Reject null (stationary)

**KPSS Test:**
```
Test statistic: 0.12
Critical value (5%): 0.463
```

Decision rule: Reject $H_0$ if test stat > critical value
→ 0.12 < 0.463
→ **Do not reject** → Series is stationary

**Combined Conclusion:**
Both ADF and KPSS agree: SPY returns are stationary. High confidence in result.

**Practical Usage:**

```lisp
;; Robust stationarity check using both tests
(define (is-stationary? data :alpha 0.05)
  (do
    (define adf (adf-test data))
    (define kpss (kpss-test data))

    (define adf-says-stationary (get adf :reject-null))  ; ADF rejects unit root
    (define kpss-says-stationary (not (get kpss :reject-null)))  ; KPSS doesn't reject stationarity

    (if (and adf-says-stationary kpss-says-stationary)
        {:stationary true :confidence "high"}
        (if (or adf-says-stationary kpss-says-stationary)
            {:stationary "uncertain" :confidence "medium" :recommendation "Use larger sample or detrend"}
            {:stationary false :confidence "high"}))))
```

### 2.10 OVSM Implementation: ADF Test with Full Explanation

Now let's implement the ADF test in OVSM with detailed comments explaining every step.

```lisp
;;═══════════════════════════════════════════════════════════════════════════
;; AUGMENTED DICKEY-FULLER TEST FOR UNIT ROOTS
;;═══════════════════════════════════════════════════════════════════════════
;;
;; WHAT: Tests whether a time series has a unit root (is non-stationary)
;; WHY: Non-stationary data leads to spurious regressions and invalid inference
;; HOW: Regress ΔY_t on Y_{t-1} and test if coefficient = 0 (unit root)
;;
;; REFERENCE: Dickey & Fuller (1979), MacKinnon (1996) for critical values
;;═══════════════════════════════════════════════════════════════════════════

(define (adf-test series :lags 12 :trend "c")
  (do
    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 1: Compute First Differences (ΔY_t = Y_t - Y_{t-1})
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; WHY: The ADF regression is specified in terms of differences
    ;; NOTE: This reduces sample size by 1 (lose first observation)
    ;;
    (define diffs (diff series))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 2: Create Lagged Level Variable (Y_{t-1})
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; This is the KEY variable in the ADF test
    ;; Its coefficient (γ) determines presence of unit root:
    ;;   - If γ = 0: unit root exists (non-stationary)
    ;;   - If γ < 0: no unit root (stationary, mean-reverting)
    ;;
    (define y-lag (lag series 1))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 3: Create Augmented Lags (ΔY_{t-1}, ΔY_{t-2}, ..., ΔY_{t-p})
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; WHY "Augmented"?
    ;; The basic Dickey-Fuller test assumes white noise errors.
    ;; Real data has autocorrelated errors → biased test statistic.
    ;; Solution: Add lagged differences as control variables.
    ;;
    ;; HOW MANY LAGS?
    ;; - Too few: residuals still autocorrelated (biased test)
    ;; - Too many: loss of power (reduced sample size)
    ;; - Rule of thumb (Schwert): 12 * (T/100)^0.25
    ;; - Or use AIC/BIC to select optimal lag length
    ;;
    (define lag-diffs
      (for (i (range 1 (+ lags 1)))
        (lag diffs i)))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 4: Build Regression Matrix Based on Trend Specification
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; THREE OPTIONS:
    ;;
    ;; 1. trend = "nc" (no constant, no trend)
    ;;    Model: ΔY_t = γY_{t-1} + Σδ_iΔY_{t-i} + ε_t
    ;;    Use for: Series clearly mean-zero (rare in finance)
    ;;
    ;; 2. trend = "c" (constant, no trend) ← MOST COMMON
    ;;    Model: ΔY_t = α + γY_{t-1} + Σδ_iΔY_{t-i} + ε_t
    ;;    Use for: Returns, spreads (non-zero mean but no trend)
    ;;
    ;; 3. trend = "ct" (constant + time trend)
    ;;    Model: ΔY_t = α + βt + γY_{t-1} + Σδ_iΔY_{t-i} + ε_t
    ;;    Use for: Price levels with deterministic drift
    ;;
    ;; CRITICAL: Different trend specs have different critical values!
    ;;
    (define X
      (cond
        ;;─── No constant (rarely used) ───
        ((= trend "nc")
         (hstack y-lag lag-diffs))

        ;;─── Constant only (default for returns/spreads) ───
        ((= trend "c")
         (hstack
           (ones (length diffs))  ; Intercept column (all 1s)
           y-lag                  ; Y_{t-1} (THE coefficient we test)
           lag-diffs))            ; ΔY_{t-1}, ..., ΔY_{t-p}

        ;;─── Constant + linear trend (for price levels) ───
        ((= trend "ct")
         (hstack
           (ones (length diffs))            ; Intercept
           (range 1 (+ (length diffs) 1))   ; Time trend (1, 2, 3, ...)
           y-lag                            ; Y_{t-1}
           lag-diffs))))                    ; Augmentation lags

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 5: OLS Regression (ΔY_t = X·β + ε)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; This estimates all coefficients via Ordinary Least Squares:
    ;;   β = (X'X)^{-1} X'y
    ;;
    ;; The coefficient on Y_{t-1} is γ (rho - 1 in notation)
    ;;
    (define regression (ols diffs X))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 6: Extract γ Coefficient and Compute Test Statistic
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; CRITICAL: The position of γ in the coefficient vector depends on trend spec:
    ;;   - trend="nc": γ is at index 0 (first coefficient)
    ;;   - trend="c":  γ is at index 1 (after constant)
    ;;   - trend="ct": γ is at index 2 (after constant and trend)
    ;;
    (define gamma-index
      (cond
        ((= trend "nc") 0)
        ((= trend "c") 1)
        ((= trend "ct") 2)))

    (define gamma (get regression :coef gamma-index))
    (define se-gamma (get regression :stderr gamma-index))

    ;;────  The ADF Test Statistic  ────
    ;;
    ;; This is just a t-statistic: γ / SE(γ)
    ;; BUT: It does NOT follow a t-distribution!
    ;; Under H₀ (unit root), it follows the Dickey-Fuller distribution
    ;; (More negative than standard t-distribution)
    ;;
    (define adf-stat (/ gamma se-gamma))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 7: Critical Values (MacKinnon 1996 Approximations)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; IMPORTANT: These are NOT t-distribution critical values!
    ;;
    ;; For reference, standard t-distribution 5% critical value ≈ -1.96
    ;; ADF critical values are more negative (harder to reject null)
    ;;
    ;; Why? Because we're testing a boundary condition (ρ = 1),
    ;; the distribution is asymmetric under the null.
    ;;
    ;; SOURCE: MacKinnon, J.G. (1996). "Numerical Distribution Functions
    ;;         for Unit Root and Cointegration Tests." Journal of Applied
    ;;         Econometrics, 11(6), 601-618.
    ;;
    (define crit-values
      (cond
        ;;─── No constant ───
        ((= trend "nc")
         {:1% -2.56 :5% -1.94 :10% -1.62})

        ;;─── Constant only (most common) ───
        ((= trend "c")
         {:1% -3.43 :5% -2.86 :10% -2.57})

        ;;─── Constant + trend ───
        ((= trend "ct")
         {:1% -3.96 :5% -3.41 :10% -3.12})))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 8: P-Value Approximation
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; Exact p-values require MacKinnon's response surface regression.
    ;; Here we provide a simple approximation based on critical values.
    ;;
    ;; NOTE: This is conservative. For publication-quality work, use
    ;; proper p-value calculation (available in statsmodels, urca packages)
    ;;
    (define p-value (adf-p-value adf-stat trend (length series)))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 9: Decision and Interpretation
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; DECISION RULE (at α = 0.05 significance level):
    ;;   - If adf-stat < critical-value: REJECT H₀ (series is stationary)
    ;;   - If adf-stat ≥ critical-value: FAIL TO REJECT H₀ (unit root)
    ;;
    ;; INTERPRETATION:
    ;;   - Reject H₀ → γ is significantly negative → mean reversion exists
    ;;   - Fail to reject → γ close to zero → random walk (unit root)
    ;;
    (define reject-null (< adf-stat (get crit-values :5%)))

    ;;─── Return comprehensive results ───
    {:statistic adf-stat
     :p-value p-value
     :critical-values crit-values
     :lags lags
     :trend trend
     :reject-null reject-null

     ;; Human-readable interpretation
     :interpretation
       (if reject-null
           (format "STATIONARY: Reject unit root (stat={:.2f} < crit={:.2f}). Safe to model."
                   adf-stat (get crit-values :5%))
           (format "NON-STATIONARY: Unit root detected (stat={:.2f} > crit={:.2f}). Difference series before modeling."
                   adf-stat (get crit-values :5%)))}))

;;═══════════════════════════════════════════════════════════════════════════
;; HELPER: P-VALUE APPROXIMATION FOR ADF TEST
;;═══════════════════════════════════════════════════════════════════════════
(define (adf-p-value stat trend n)
  ;;
  ;; This is a crude approximation. For exact p-values, use MacKinnon's
  ;; response surface regression:
  ;;   p-value = Φ(τ_n) where τ_n depends on (stat, trend, sample size)
  ;;
  ;; Here we provide conservative bounds based on critical values
  ;;
  (define tau-coeffs
    (cond
      ((= trend "nc") [-1.94 -1.62 -1.28])  ; 5%, 10%, 25% critical values
      ((= trend "c")  [-2.86 -2.57 -2.28])
      ((= trend "ct") [-3.41 -3.12 -2.76])))

  (cond
    ((< stat (get tau-coeffs 0)) 0.01)   ; stat more negative than 5% cv → p < 1%
    ((< stat (get tau-coeffs 1)) 0.05)   ; between 5% and 10% cv → p ≈ 5%
    ((< stat (get tau-coeffs 2)) 0.10)   ; between 10% and 25% cv → p ≈ 10%
    (true 0.15)))                         ; less negative than 25% cv → p > 10%
```

**Usage Example:**

```lisp
;; Load SPY prices
(define spy-prices (get-historical-prices "SPY" :days 252))

;; Test prices (expect non-stationary)
(define adf-prices (adf-test spy-prices :lags 1 :trend "c"))
(log :message (get adf-prices :interpretation))
;; Output: "NON-STATIONARY: Unit root detected (stat=-0.33 > crit=-2.86). Difference series before modeling."

;; Transform to returns
(define spy-returns (log-returns spy-prices))

;; Test returns (expect stationary)
(define adf-returns (adf-test spy-returns :lags 1 :trend "c"))
(log :message (get adf-returns :interpretation))
;; Output: "STATIONARY: Reject unit root (stat=-11.88 < crit=-2.86). Safe to model."
```

### 2.11 Stationarity Transformations: Making Data Stationary

When ADF test reveals non-stationarity, we must transform the data. Four common approaches:

**Transformation 1: First Differencing**

$$\Delta Y_t = Y_t - Y_{t-1}$$

**When to Use:** Price levels with unit root

**Example:**
```
SPY prices: $324, $325, $327, $327, $330
First differences: +$1, +$2, $0, +$3
```

**Log Version (for percentage changes):**
$$R_t = \log(P_t / P_{t-1}) = \log P_t - \log P_{t-1}$$

**Transformation 2: Seasonal Differencing**

$$\Delta_s Y_t = Y_t - Y_{t-s}$$

**When to Use:** Data with seasonal unit roots (e.g., monthly sales)

**Example (s=12 for monthly data):**
```
Ice cream sales: Compare Jan 2024 to Jan 2023 (remove annual seasonality)
```

**Transformation 3: Detrending**

Remove linear or polynomial trend:
$$Y_t^{\text{detrend}} = Y_t - (\hat{\alpha} + \hat{\beta} t)$$

**When to Use:** Trend-stationary series (ADF rejects, KPSS rejects)

**Transformation 4: Log Transformation**

$$Y_t^{\text{log}} = \log(Y_t)$$

**When to Use:** Exponentially growing series (GDP, tech stock prices)

**Benefit:** Converts exponential trend to linear trend

**OVSM Automatic Transformation:**

```lisp
;; Automatically make series stationary
(define (make-stationary series :max-diffs 2)
  ;;
  ;; Try up to max-diffs differencing operations
  ;; Use both ADF and KPSS to confirm stationarity
  ;;
  (define (try-transform data diffs-applied)
    (if (>= diffs-applied max-diffs)
        ;; Hit maximum diffs → return as-is with warning
        {:data data
         :transformations diffs-applied
         :stationary false
         :warning "Maximum differencing reached but series still non-stationary"}

        ;; Test current series
        (let ((adf-result (adf-test data :trend "c"))
              (kpss-result (kpss-test data :trend "c")))

          (if (and (get adf-result :reject-null)           ; ADF says stationary
                   (not (get kpss-result :reject-null)))   ; KPSS agrees
              ;; ✓ Stationary!
              {:data data
               :transformations diffs-applied
               :stationary true
               :adf-stat (get adf-result :statistic)
               :kpss-stat (get kpss-result :statistic)}

              ;; ✗ Still non-stationary → difference and retry
              (try-transform (diff data) (+ diffs-applied 1))))))

  ;; Start with original series (0 diffs)
  (try-transform series 0))

;; Usage
(define result (make-stationary spy-prices))
(if (get result :stationary)
    (log :message (format "Series stationary after {} differencing operations"
                          (get result :transformations)))
    (log :message "WARNING: Could not achieve stationarity"))
```

---

### 2.12 Summary: The Stationarity Toolkit

**Key Concepts:**
1. **Stationarity = constant mean + constant variance + time-invariant autocovariance**
2. **Unit root = random walk behavior (shocks persist forever)**
3. **ADF test = standard tool (null: unit root exists)**
4. **KPSS test = complementary (null: stationarity exists)**
5. **Transform non-stationary data via differencing or detrending**

**Decision Tree:**

```mermaid
graph TD
    A[Time Series Data] --> B{Visual Inspection}
    B -->|Clear trend| C[Run ADF with trend='ct']
    B -->|No obvious trend| D[Run ADF with trend='c']

    C --> E{ADF p-value < 0.05?}
    D --> E

    E -->|Yes - Reject H0| F[Run KPSS Test]
    E -->|No - Unit Root| G[Difference Series]

    F --> H{KPSS p-value > 0.05?}
    H -->|Yes| I[✓ STATIONARY]
    H -->|No| J[Trend-Stationary: Detrend]

    G --> K[Retest with ADF]
    K --> E

    J --> L[Retest]
    L --> I
```

**Common Pitfalls:**

| Mistake | Consequence | Fix |
|---------|-------------|-----|
| Model prices directly | Spurious patterns | Difference to returns |
| Use wrong ADF trend spec | Biased test | Match spec to data (prices→'ct', returns→'c') |
| Ignore KPSS | False confidence | Always run both tests |
| Over-difference | Introduce MA component | Use AIC/BIC to avoid |
| Assume stationarity | Invalid inference | ALWAYS test first |

**Next Section:**

Now that we can identify and create stationary data, we'll learn how to **model** it using ARIMA—capturing patterns like autocorrelation and momentum that persist in stationary returns.

---

## Section 3: ARIMA Models - Capturing Temporal Patterns

### 3.1 The Autoregressive Idea: Today Predicts Tomorrow

We've established that returns (not prices) are stationary. But are they predictable? Or completely random?

**Simple Hypothesis:**
> "Today's return contains information about tomorrow's return."

This is the **autoregressive** (AR) hypothesis. If true, we can exploit it.

**Starting Simple: AR(1)**

The simplest autoregressive model says:
$$R_t = c + \phi R_{t-1} + \epsilon_t$$

Where:
- $R_t$ = today's return
- $R_{t-1}$ = yesterday's return
- $\phi$ = autoregressive coefficient (the key parameter)
- $c$ = constant (long-run mean × (1 - φ))
- $\epsilon_t$ = white noise error (unpredictable shock)

**Interpreting φ:**

| φ Value | Interpretation | Trading Strategy |
|---------|----------------|------------------|
| φ > 0 | **Momentum**: Positive returns follow positive returns | Trend following |
| φ = 0 | **Random walk**: No predictability | EMH holds, don't trade |
| φ < 0 | **Mean reversion**: Positive returns follow negative returns | Contrarian |
| \|φ\| close to 1 | **Strong persistence**: Pattern lasts many periods | High Sharpe potential |
| \|φ\| close to 0 | **Weak pattern**: Noise dominates signal | Not exploitable |

**Stationarity Condition:**
The AR(1) model is stationary only if $|\phi| < 1$.

- If $\phi = 1$: Random walk (unit root)
- If $|\phi| > 1$: Explosive (variance grows exponentially)

### 3.2 Worked Example: Bitcoin Hourly Returns

Let's test whether cryptocurrency exhibits short-term momentum.

**Data:**
- BTC/USDT hourly returns, January 2024 (744 hours)
- Source: Binance
- Returns calculated as: $R_t = \log(P_t / P_{t-1})$

**Step 1: Visual Inspection**

```
Plot: BTC Hourly Returns (Jan 2024)
- Mean: 0.00015 (+0.015% per hour ≈ +0.36% daily)
- Std Dev: 0.0085 (0.85%)
- Range: -3.2% to +2.8%
- Outliers: 12 hours with |R| > 2% (cascading liquidations)
```

**Observation:** Returns cluster—big moves follow big moves.

**Step 2: Autocorrelation Analysis**

Compute sample autocorrelation at lag 1:
$$\hat{\rho}_1 = \frac{\sum_{t=2}^{T} (R_t - \bar{R})(R_{t-1} - \bar{R})}{\sum_{t=1}^{T} (R_t - \bar{R})^2}$$

**Result:**
```
Lag 1: ρ̂₁ = 0.148 (t-stat = 4.04, p < 0.001)
Lag 2: ρ̂₂ = 0.032 (t-stat = 0.87, p = 0.38)
Lag 3: ρ̂₃ = -0.015 (t-stat = -0.41, p = 0.68)
```

**Interpretation:**
- **Lag 1 is significant:** If last hour's return was +1%, expect this hour's return to be +0.15% on average
- **Lags 2+ not significant:** Only immediate past matters
- This suggests **AR(1)** model

**Step 3: Estimate AR(1) Model**

Using OLS regression: $R_t$ on $R_{t-1}$

**Results:**
```
Coefficient estimates:
c (const) = 0.00014  (SE = 0.00031)  [t = 0.45, p = 0.65]
φ (R_t-1) = 0.142    (SE = 0.037)    [t = 3.84, p < 0.001]

Model fit:
R² = 0.021 (2.1% of variance explained)
Residual std error: 0.0084
AIC = -5,234
```

**Interpretation:**

**The Model:**
$$R_t = 0.00014 + 0.142 \cdot R_{t-1} + \epsilon_t$$

**What It Means:**
1. **φ = 0.142:** 14.2% of last hour's return persists this hour
2. **Weak but significant:** R² = 2.1% seems small, but it's exploitable
3. **Momentum confirmed:** φ > 0 means positive autocorrelation

**Step 4: Trading Strategy**

```
Signal generation:
IF R_{t-1} > 0.5%:  BUY  (expect +0.07% continuation)
IF R_{t-1} < -0.5%: SELL (expect -0.07% continuation)

Position sizing:
- Expected return: 0.142 × |R_{t-1}|
- Risk: 0.84% (residual std error)
- Sharpe (hourly): 0.142 / 0.84 ≈ 0.17
- Annualized Sharpe: 0.17 × √(24×365) ≈ 2.3 (if pattern holds)

Reality check:
- Transaction costs: ~0.10% round-trip (Binance taker fees)
- Slippage: ~0.05% for market orders
- Total cost: 0.15% per trade

Net expected return: 0.07% - 0.15% = -0.08% (NOT PROFITABLE)
```

**The Lesson:**
Even statistically significant momentum (p < 0.001) may not be economically significant after costs. This is why high-frequency strategies need:
1. Very tight spreads (limit orders, maker rebates)
2. Large volume (fee discounts)
3. Leverage (amplify small edges)

**Step 5: Out-of-Sample Validation**

The 14.2% coefficient was estimated on January 2024 data. Does it hold in February?

```
February 2024 test:
- Predicted autocorrelation: 0.142
- Actual autocorrelation: 0.089 (t = 2.34, p = 0.02)

Degradation: 0.142 → 0.089 (37% reduction)
Strategy P&L: -0.3% (costs dominated)
```

**Reality:** Parameters are unstable. AR(1) patterns exist but are too weak and fleeting for reliable trading in liquid markets.

### 3.3 Higher-Order AR: AR(p) Models

Sometimes multiple lags matter:
$$R_t = c + \phi_1 R_{t-1} + \phi_2 R_{t-2} + \cdots + \phi_p R_{t-p} + \epsilon_t$$

**Example: Weekly Equity Returns**

SPY weekly returns show a different pattern:

```
Lag 1: ρ̂₁ = -0.05  (slight mean reversion)
Lag 2: ρ̂₂ = 0.08   (bi-weekly momentum)
Lag 4: ρ̂₄ = -0.12  (monthly mean reversion)
```

An AR(4) model captures this:
$$R_t = 0.0012 - 0.05 R_{t-1} + 0.08 R_{t-2} - 0.03 R_{t-3} - 0.12 R_{t-4} + \epsilon_t$$

**Interpretation:**
- Last week negative → this week slightly positive (reversion)
- 2 weeks ago positive → this week positive (medium-term momentum)
- 4 weeks ago negative → this week positive (monthly pattern)

### 3.4 OVSM Implementation: AR(p) Model

```lisp
;;═══════════════════════════════════════════════════════════════════════════
;; AUTOREGRESSIVE MODEL OF ORDER p
;;═══════════════════════════════════════════════════════════════════════════
;;
;; WHAT: Models time series as linear combination of past values
;; WHY: Captures momentum (φ > 0) or mean reversion (φ < 0) patterns
;; HOW: OLS regression of Y_t on Y_{t-1}, ..., Y_{t-p}
;;
;; MODEL: Y_t = c + φ₁Y_{t-1} + φ₂Y_{t-2} + ... + φ_pY_{t-p} + ε_t
;;
;; STATIONARITY: Requires roots of characteristic polynomial outside unit circle
;;               In practice: Check that Σ|φᵢ| < 1 (approximate)
;;═══════════════════════════════════════════════════════════════════════════

(define (ar-model data :order 1)
  (do
    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 1: Create Dependent Variable (Y_t)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; We lose 'order' observations at the start because we need lags.
    ;; Example: For AR(2), we need Y_{t-1} and Y_{t-2}, so first valid t is 3.
    ;;
    ;; data = [y₁, y₂, y₃, y₄, y₅]
    ;; For AR(2):
    ;;   y = [y₃, y₄, y₅]  (dependent variable)
    ;;
    (define y (slice data order (length data)))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 2: Create Lagged Independent Variables Matrix
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; Build design matrix X = [1, Y_{t-1}, Y_{t-2}, ..., Y_{t-p}]
    ;;
    ;; Example for AR(2) with data = [1, 2, 3, 4, 5]:
    ;;   Row 1: [1, y₂, y₁] = [1, 2, 1]  (for predicting y₃)
    ;;   Row 2: [1, y₃, y₂] = [1, 3, 2]  (for predicting y₄)
    ;;   Row 3: [1, y₄, y₃] = [1, 4, 3]  (for predicting y₅)
    ;;
    ;; CRITICAL: Alignment is tricky. For lag k:
    ;;   - Start at index (order - k)
    ;;   - End at index (length - k)
    ;;
    (define X
      (hstack
        (ones (length y))                    ; Intercept column
        (for (lag-num (range 1 (+ order 1)))
          ;; Y_{t-1}: slice(data, order-1, length-1)
          ;; Y_{t-2}: slice(data, order-2, length-2)
          ;; ...
          (slice data
                 (- order lag-num)            ; Start index
                 (- (length data) lag-num))))) ; End index

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 3: Ordinary Least Squares Estimation
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; Solves: β̂ = (X'X)⁻¹X'y
    ;;
    ;; This minimizes sum of squared residuals:
    ;;   SSR = Σ(yᵢ - ŷᵢ)² = Σ(yᵢ - X'β)²
    ;;
    ;; Result: β = [c, φ₁, φ₂, ..., φ_p]
    ;;
    (define regression (ols y X))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 4: Extract Parameters
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    (define const (get regression :coef 0))              ; Intercept
    (define phi-coeffs (slice (get regression :coef) 1)) ; [φ₁, φ₂, ..., φ_p]

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 5: Compute Fitted Values and Residuals
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; Fitted: ŷ = Xβ
    ;; Residuals: ε̂ = y - ŷ (our forecast errors)
    ;;
    (define fitted (matmul X (get regression :coef)))
    (define residuals (subtract y fitted))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 6: Model Selection Criteria (AIC and BIC)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; AIC (Akaike Information Criterion):
    ;;   AIC = n·log(σ²) + 2k
    ;;   Penalizes model complexity (k parameters)
    ;;   Lower is better
    ;;
    ;; BIC (Bayesian Information Criterion):
    ;;   BIC = n·log(σ²) + k·log(n)
    ;;   Stronger penalty for complexity (especially for large n)
    ;;   Lower is better
    ;;
    ;; USAGE:
    ;;   - Fit AR(1), AR(2), ..., AR(10)
    ;;   - Select p that minimizes AIC (or BIC)
    ;;   - BIC tends to select simpler models (preferred for forecasting)
    ;;
    (define n (length y))
    (define k (+ order 1))                    ; Number of parameters
    (define sigma2 (/ (sum-squares residuals) n))
    (define aic (+ (* n (log sigma2)) (* 2 k)))
    (define bic (+ (* n (log sigma2)) (* k (log n))))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 7: Stationarity Check
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; THEORY: AR(p) is stationary if roots of characteristic polynomial
    ;;         φ(z) = 1 - φ₁z - φ₂z² - ... - φ_pz^p = 0
    ;;         all lie OUTSIDE the unit circle (|z| > 1)
    ;;
    ;; PRACTICE: Approximate check—Σ|φᵢ| should be well below 1
    ;;           If close to 1, model is nearly non-stationary (unit root)
    ;;
    (define phi-sum (sum (map abs phi-coeffs)))
    (define stationary-approx (< phi-sum 0.95))  ; Rule of thumb

    ;; For exact check, we'd compute roots of characteristic polynomial
    ;; (require polynomial root-finding, which we'll implement if needed)

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 8: Forecasting Function
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; One-step-ahead forecast:
    ;;   ŷ_{T+1} = c + φ₁y_T + φ₂y_{T-1} + ... + φ_py_{T-p+1}
    ;;
    (define (forecast-next recent-values)
      ;; recent-values should be [y_T, y_{T-1}, ..., y_{T-p+1}] (length = order)
      (if (!= (length recent-values) order)
          (error "forecast-next requires 'order' most recent values")
          (+  const
              (sum (for (i (range 0 order))
                     (* (get phi-coeffs i)
                        (get recent-values i)))))))

    ;;─────────────────────────────────────────────────────────────────────
    ;; RETURN RESULTS
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    {:type "AR"
     :order order
     :constant const
     :coefficients phi-coeffs              ; [φ₁, φ₂, ..., φ_p]
     :residuals residuals
     :fitted fitted
     :sigma-squared sigma2                 ; Residual variance
     :aic aic
     :bic bic
     :r-squared (get regression :r-squared)
     :stationary stationary-approx
     :forecast forecast-next               ; Function for predictions

     ;; Human-readable interpretation
     :interpretation
       (cond
         ((> (first phi-coeffs) 0.1)
          (format "MOMENTUM: φ₁={:.3f} (positive autocorrelation)" (first phi-coeffs)))
         ((< (first phi-coeffs) -0.1)
          (format "MEAN REVERSION: φ₁={:.3f} (negative autocorrelation)" (first phi-coeffs)))
         (true
          (format "WEAK PATTERN: φ₁={:.3f} (close to random walk)" (first phi-coeffs))))}))

;;═══════════════════════════════════════════════════════════════════════════
;; HELPER: Sum of Squared Residuals
;;═══════════════════════════════════════════════════════════════════════════
(define (sum-squares vec)
  (sum (map (lambda (x) (* x x)) vec)))
```

**Usage Example:**

```lisp
;; Load Bitcoin hourly returns
(define btc-returns (get-historical-returns "BTC/USDT" :frequency "1h" :days 30))

;; Fit AR(1)
(define ar1 (ar-model btc-returns :order 1))

;; Examine results
(log :message (get ar1 :interpretation))
;; Output: "MOMENTUM: φ₁=0.142 (positive autocorrelation)"

(log :message (format "R² = {:.1%}, AIC = {:.0f}"
                      (get ar1 :r-squared)
                      (get ar1 :aic)))
;; Output: "R² = 2.1%, AIC = -5234"

;; Forecast next hour
(define recent-values [(last btc-returns)])  ; Most recent return
(define forecast ((get ar1 :forecast) recent-values))
(log :message (format "Forecast next hour: {:.2%}" forecast))
;; Output: "Forecast next hour: 0.07%" (if last hour was +0.5%)

;; Compare AR(1) vs AR(2) vs AR(3)
(define ar2 (ar-model btc-returns :order 2))
(define ar3 (ar-model btc-returns :order 3))

(log :message (format "AR(1) BIC: {:.0f}" (get ar1 :bic)))
(log :message (format "AR(2) BIC: {:.0f}" (get ar2 :bic)))
(log :message (format "AR(3) BIC: {:.0f}" (get ar3 :bic)))
;; Select the model with lowest BIC
```

### 3.5 Moving Average (MA) Models: Why Yesterday's Errors Matter

Autoregressive models say: "Today's value depends on yesterday's values."

Moving average models say: "Today's value depends on yesterday's forecast errors."

**The MA(1) Model:**
$$R_t = \mu + \epsilon_t + \theta \epsilon_{t-1}$$

Where:
- $\epsilon_t$ = today's shock (white noise)
- $\epsilon_{t-1}$ = yesterday's shock
- $\theta$ = MA coefficient

**Intuitive Explanation:**

Imagine news arrives that surprises the market:
1. **Day 0:** Fed announces unexpected rate hike (large positive shock $\epsilon_0$)
2. **Day 1:** Market overreacts to news, pushing prices too high
3. **Day 2:** Correction happens—prices drift back down

This creates an MA(1) pattern where yesterday's shock $\epsilon_{t-1}$ affects today's return.

**When MA Patterns Arise:**

| Source | Mechanism | θ Sign |
|--------|-----------|--------|
| **Overreaction** | News shock → overreaction → correction | θ < 0 (negative MA) |
| **Delayed response** | News shock → gradual incorporation | θ > 0 (positive MA) |
| **Bid-ask bounce** | Trade at ask → mean revert to mid → trade at bid | θ < 0 |
| **Noise trading** | Uninformed trades → price noise → reversion | θ < 0 |

**Financial Reality:**

High-frequency returns often exhibit **negative MA(1)** (θ ≈ -0.2 to -0.4) due to:
- Bid-ask bounce
- Price discreteness (tick sizes)
- Market microstructure noise

Daily/weekly returns rarely show strong MA patterns (θ ≈ 0) because:
- Markets efficiently incorporate news within hours
- Persistent patterns would be arbitraged away

### 3.6 The Estimation Challenge: Errors Aren't Observed

**The Problem:**

In AR models, we regress $R_t$ on observed values $R_{t-1}$. Simple OLS works.

In MA models, we need to regress $R_t$ on unobserved errors $\epsilon_{t-1}$. OLS won't work because we don't observe errors until after we've estimated the model!

**The Circular Logic:**
1. To estimate θ, we need errors $\epsilon_t$
2. To compute errors, we need θ: $\epsilon_t = R_t - \mu - \theta \epsilon_{t-1}$
3. Chicken-and-egg problem!

**Solution: Maximum Likelihood Estimation (MLE)**

We iteratively search for θ that maximizes the likelihood of observing our data.

**Algorithm (Simplified):**

```
Step 1: Initialize θ = 0, μ = mean(R)
Step 2: Compute errors recursively:
        ε₁ = R₁ - μ
        ε₂ = R₂ - μ - θε₁
        ε₃ = R₃ - μ - θε₂
        ...
Step 3: Compute log-likelihood:
        L(θ) = -n/2·log(2π) - n/2·log(σ²) - 1/(2σ²)·Σεₜ²
Step 4: Update θ to increase L(θ) (use Newton-Raphson or similar)
Step 5: Repeat Steps 2-4 until convergence
```

**In Practice:**

Modern software (statsmodels, R's arima, OVSM below) handles this automatically. But understanding the challenge explains why:
- MA estimation is slower than AR
- MA models are less common in practice (AR often sufficient)
- ARMA combines both (best of both worlds)

### 3.7 ARIMA: Putting It All Together

**The Full ARIMA(p,d,q) Model:**

$$\phi(L)(1-L)^d R_t = \theta(L)\epsilon_t$$

Where:
- **p:** Number of AR lags (order of autoregressive part)
- **d:** Number of differencing operations (integration order)
- **q:** Number of MA lags (order of moving average part)
- $\phi(L) = 1 - \phi_1 L - \phi_2 L^2 - \cdots - \phi_p L^p$ (AR polynomial)
- $\theta(L) = 1 + \theta_1 L + \theta_2 L^2 + \cdots + \theta_q L^q$ (MA polynomial)
- $L$ = lag operator ($L R_t = R_{t-1}$)

**Expanded Form:**

After differencing d times, the model becomes:
$$R_t' = c + \phi_1 R_{t-1}' + \cdots + \phi_p R_{t-p}' + \epsilon_t + \theta_1 \epsilon_{t-1} + \cdots + \theta_q \epsilon_{t-q}$$

Where $R_t'$ is the differenced series.

**Common Specifications:**

| Model | p | d | q | Use Case |
|-------|---|---|---|----------|
| **ARIMA(1,0,0)** | 1 | 0 | 0 | Stationary returns with momentum |
| **ARIMA(0,1,0)** | 0 | 1 | 0 | Random walk (benchmark) |
| **ARIMA(1,1,0)** | 1 | 1 | 0 | Prices with momentum in returns |
| **ARIMA(0,0,1)** | 0 | 0 | 1 | Stationary with MA shock |
| **ARIMA(1,1,1)** | 1 | 1 | 1 | Prices with AR+MA in returns |
| **ARIMA(2,1,2)** | 2 | 1 | 2 | Complex patterns (often overfit) |

**Interpretation Guide:**

**The d Parameter (Differencing):**
- $d=0$: Series already stationary (use for returns)
- $d=1$: Series has unit root (use for price levels)
- $d=2$: Rare; series has two unit roots (unusual in finance)

**The p Parameter (AR order):**
- Captures autocorrelation in the series itself
- $p=1$: Only yesterday matters
- $p=5$: Last 5 periods matter (weekly patterns in daily data)

**The q Parameter (MA order):**
- Captures autocorrelation in forecast errors
- $q=1$: Yesterday's shock affects today
- $q=0$: No MA component (pure AR often sufficient)

### 3.8 Box-Jenkins Methodology: A Systematic Approach

George Box and Gwilym Jenkins (1970) developed a 3-step procedure for ARIMA model building:

```mermaid
stateDiagram-v2
    [*] --> LoadData: Raw Time Series
    LoadData --> CheckStationarity: Visual Inspection

    CheckStationarity --> Difference: ADF p > 0.05<br/>(Non-stationary)
    CheckStationarity --> IdentifyOrders: ADF p < 0.05<br/>(Stationary)
    Difference --> CheckStationarity: d = d + 1

    IdentifyOrders --> PlotACF_PACF: Examine Patterns
    PlotACF_PACF --> SelectModel: ACF/PACF Analysis

    SelectModel --> EstimateAR: PACF cuts off<br/>ACF decays
    SelectModel --> EstimateMA: ACF cuts off<br/>PACF decays
    SelectModel --> EstimateARMA: Both decay

    EstimateAR --> Diagnostics: MLE/OLS
    EstimateMA --> Diagnostics: MLE
    EstimateARMA --> Diagnostics: MLE

    Diagnostics --> LjungBox: Test Residuals
    LjungBox --> PassedDiagnostics: p > 0.05<br/>(White noise)
    LjungBox --> SelectModel: p < 0.05<br/>(Try different p,q)

    PassedDiagnostics --> OutOfSample: Walk-forward test
    OutOfSample --> DeployModel: RMSE_test ≈ RMSE_train
    OutOfSample --> SelectModel: RMSE_test >> RMSE_train<br/>(Overfitting)

    DeployModel --> [*]: Production Ready

    note right of CheckStationarity
        Use ADF + KPSS
        d=0 for returns
        d=1 for prices
    end note

    note right of PlotACF_PACF
        Look for patterns:
        - Sharp cutoff
        - Exponential decay
        - Significance bounds
    end note

    note right of Diagnostics
        Check:
        1. Ljung-Box (residuals)
        2. Jarque-Bera (normality)
        3. AIC/BIC (parsimony)
    end note
```

**STEP 1: IDENTIFICATION**
*Goal: Determine orders p, d, q*

**1a. Determine d (differencing order):**

```
Algorithm:
1. Run ADF test on raw series
2. If p-value > 0.05: Difference once (d=1)
3. Run ADF test on differenced series
4. If still p > 0.05: Difference again (d=2)
5. Stop when p < 0.05 (series is stationary)

Financial reality:
- Returns (d=0): Already stationary
- Prices (d=1): One difference makes stationary
- d=2 is rare and suggests data problems
```

**1b. Examine ACF and PACF plots:**

**ACF (Autocorrelation Function):**
Correlation between $R_t$ and $R_{t-k}$ for various lags $k$:
$$\rho_k = \frac{\text{Cov}(R_t, R_{t-k})}{\text{Var}(R_t)}$$

**PACF (Partial Autocorrelation Function):**
Correlation between $R_t$ and $R_{t-k}$ after removing effects of intermediate lags.

**Pattern Recognition:**

| ACF Pattern | PACF Pattern | Suggested Model |
|-------------|--------------|-----------------|
| Cuts off sharply after lag q | Decays exponentially | **MA(q)** |
| Decays exponentially | Cuts off sharply after lag p | **AR(p)** |
| Decays exponentially | Decays exponentially | **ARMA(p,q)** - use AIC/BIC |
| All near zero | All near zero | **White noise** (no model needed) |

**Example: Bitcoin Hourly Returns**

```
ACF values:
Lag 1: 0.148 **
Lag 2: 0.032
Lag 3: -0.015
Lag 4: -0.008
Lag 5+: All < 0.05

PACF values:
Lag 1: 0.148 **
Lag 2: 0.012
Lag 3: -0.018
Lag 4: -0.010
Lag 5+: All < 0.05

Pattern:
- ACF: Exponential decay (only lag 1 significant)
- PACF: Sharp cutoff after lag 1

Conclusion: AR(1) model suggested
```

**STEP 2: ESTIMATION**
*Goal: Estimate parameters φ and θ*

```
Method: Maximum Likelihood Estimation (MLE)

For AR-only models:
- Can use OLS (equivalent to MLE for AR)
- Fast and reliable

For MA or ARMA models:
- Must use iterative MLE
- Requires numerical optimization (Newton-Raphson, BFGS)
- Software handles this automatically

Result:
- Coefficient estimates: φ̂, θ̂
- Standard errors: SE(φ̂), SE(θ̂)
- Information criteria: AIC, BIC
```

**STEP 3: DIAGNOSTIC CHECKING**
*Goal: Validate model assumptions*

**3a. Residual Autocorrelation (Ljung-Box Test):**

```
Hypothesis:
H₀: Residuals are white noise (no autocorrelation)
H₁: Residuals are autocorrelated (model inadequate)

Test statistic:
Q = n(n+2) Σ(k=1 to m) ρ̂ₖ² / (n-k)

where ρ̂ₖ = autocorrelation of residuals at lag k

Decision:
If p-value > 0.05: ✓ Residuals are white noise (model adequate)
If p-value < 0.05: ✗ Model missed patterns (try different p, d, q)
```

**3b. Normality of Residuals (Jarque-Bera Test):**

```
Tests whether residuals are normally distributed

JB = n/6 · (S² + (K-3)²/4)

where:
S = skewness
K = kurtosis

Financial reality:
- Returns have fat tails (K > 3)
- JB test almost always rejects normality
- This is OK! ARIMA forecasts still valid
- For risk management, use GARCH (Chapter 12) or t-distribution
```

**3c. Out-of-Sample Testing:**

```
Walk-forward validation:

1. Split data: Train (80%), Test (20%)
2. Estimate ARIMA on train set
3. Forecast test set one-step-ahead
4. Compute RMSE_test
5. Compare to RMSE_train:
   - If RMSE_test ≈ RMSE_train: ✓ Model generalizes
   - If RMSE_test >> RMSE_train: ✗ Overfitting
```

### 3.9 Worked Example: Ethereum Daily Returns (Complete Box-Jenkins)

Let's apply the full methodology to Ethereum.

**Data:**
- ETH/USD daily log returns, 2023 (365 days)
- Mean: 0.12% daily
- Std Dev: 3.8% daily
- Range: -15% to +12%

**STEP 1: IDENTIFICATION**

**1a. Stationarity Check:**

```lisp
(define eth-prices (get-historical-prices "ETH/USD" :days 365))
(define adf-prices (adf-test eth-prices :trend "c"))
;; Result: p-value = 0.32 → Fail to reject → NON-STATIONARY

(define eth-returns (log-returns eth-prices))
(define adf-returns (adf-test eth-returns :trend "c"))
;; Result: p-value < 0.001 → Reject → STATIONARY

;; Conclusion: d = 0 (work with returns, already stationary)
```

**1b. ACF/PACF Analysis:**

```
ACF:
Lag 1: 0.08 (p = 0.12)  [Not significant at 5%, but close]
Lag 2: -0.05
Lag 3: 0.03
Lag 4: -0.02
Lag 5+: < 0.05

PACF:
Lag 1: 0.08
Lag 2: -0.06
Lag 3: 0.04
Lag 4: -0.03
Lag 5+: < 0.05

Interpretation:
- Weak patterns (all correlations < 0.10)
- ACF: Slight lag-1, then noise
- PACF: Slight lag-1, then noise

Candidates:
- ARIMA(1,0,0): AR(1) model
- ARIMA(0,0,1): MA(1) model
- ARIMA(1,0,1): ARMA(1,1) model
- ARIMA(0,0,0): White noise (no model)
```

**STEP 2: ESTIMATION**

Fit all candidate models and compare AIC/BIC:

```lisp
(define models (list
  (arima-model eth-returns :p 0 :d 0 :q 0)  ; White noise
  (arima-model eth-returns :p 1 :d 0 :q 0)  ; AR(1)
  (arima-model eth-returns :p 0 :d 0 :q 1)  ; MA(1)
  (arima-model eth-returns :p 1 :d 0 :q 1)  ; ARMA(1,1)
  (arima-model eth-returns :p 2 :d 0 :q 0)  ; AR(2)
))

(for (model models)
  (log :message (format "{}: AIC={:.0f}, BIC={:.0f}"
                        (get model :type)
                        (get model :aic)
                        (get model :bic))))

;; Results:
;; White noise: AIC=-1842, BIC=-1838
;; AR(1):       AIC=-1845, BIC=-1837  ← Lowest AIC
;; MA(1):       AIC=-1843, BIC=-1835
;; ARMA(1,1):   AIC=-1844, BIC=-1832  ← Lowest BIC
;; AR(2):       AIC=-1844, BIC=-1832

;; Decision: AR(1) has lowest AIC, ARMA(1,1) has lowest BIC
;; BIC preferred for forecasting (penalizes complexity more)
;; Choose: ARMA(1,1)
```

**Selected Model: ARIMA(1,0,1)**

```
Coefficient estimates:
φ (AR1) = 0.15  (SE = 0.06, t = 2.50, p = 0.01)
θ (MA1) = -0.07 (SE = 0.07, t = -1.00, p = 0.32)

Model:
R_t = 0.0012 + 0.15·R_{t-1} + ε_t - 0.07·ε_{t-1}

Interpretation:
- Weak momentum (φ = 0.15)
- MA component not significant (θ p-value = 0.32)
- Model barely better than AR(1)
```

**STEP 3: DIAGNOSTIC CHECKING**

**3a. Ljung-Box Test on Residuals:**

```lisp
(define residuals (get selected-model :residuals))
(define lb-test (ljung-box-test residuals :lags 20))

;; Result:
;; Q-statistic: 18.3
;; p-value: 0.56
;; Conclusion: ✓ Residuals appear to be white noise
```

**3b. Jarque-Bera Test:**

```lisp
(define jb-test (jarque-bera-test residuals))

;; Result:
;; JB statistic: 423
;; p-value: < 0.001
;; Skewness: -0.2
;; Kurtosis: 5.8 (excess kurtosis = 2.8)

;; Conclusion: ✗ Residuals not normal (fat tails)
;; Action: For forecasting, this is OK
;;         For risk management, use GARCH or bootstrap methods
```

**3c. Out-of-Sample Forecast:**

```lisp
;; Train on first 292 days (80%), test on last 73 days (20%)
(define train-returns (slice eth-returns 0 292))
(define test-returns (slice eth-returns 292 365))

(define train-model (arima-model train-returns :p 1 :d 0 :q 1))

;; Walk-forward forecast
(define forecasts (array))
(for (t (range 0 (length test-returns)))
  (define recent-values (slice eth-returns (+ 292 t -1) (+ 292 t)))
  (define recent-errors (slice (get train-model :residuals) -1))
  (define forecast ((get train-model :forecast) recent-values recent-errors))
  (push! forecasts forecast))

;; Compute forecast errors
(define forecast-errors (subtract test-returns forecasts))
(define rmse-test (sqrt (mean (map (lambda (e) (* e e)) forecast-errors))))
(define rmse-train (sqrt (get train-model :sigma-squared)))

(log :message (format "RMSE Train: {:.4f}" rmse-train))
(log :message (format "RMSE Test:  {:.4f}" rmse-test))

;; Results:
;; RMSE Train: 0.0375 (3.75%)
;; RMSE Test:  0.0391 (3.91%)
;; Degradation: 4% (acceptable—model generalizes reasonably)
```

**STEP 4: INTERPRETATION & TRADING**

**Model Summary:**
```
ARIMA(1,0,1) for ETH daily returns

φ = 0.15: Weak positive autocorrelation (momentum)
θ = -0.07: Negligible MA component

Forecast equation:
E[R_{t+1} | R_t, ε_t] = 0.0012 + 0.15·R_t - 0.07·ε_t

One-step-ahead forecast std error: 3.75%
```

**Trading Implications:**

```
Signal generation:
IF R_t > 1%:  Forecast R_{t+1} ≈ 0.15% (positive continuation)
IF R_t < -1%: Forecast R_{t+1} ≈ -0.15% (negative continuation)

Expected Sharpe (if pattern persists):
- Signal strength: 0.15
- Noise: 3.75%
- Sharpe = 0.15 / 3.75 = 0.04 daily = 0.04·√252 = 0.63 annualized

Reality check:
- Very weak pattern (barely above noise)
- Transaction costs (~0.10%) eat most of edge
- High vol (3.75% daily) creates large drawdowns
- NOT tradeable as standalone strategy

Better use:
- Combine with other signals (volatility, sentiment, order flow)
- Use for risk management (expected return in position sizing)
- Pairs trading (combine ETH/BTC mean reversion with ETH momentum)
```

### 3.10 Summary: ARIMA Toolkit

**Key Takeaways:**

1. **AR models capture autocorrelation** (φ > 0 = momentum, φ < 0 = reversion)
2. **MA models capture shock persistence** (θ ≠ 0 means yesterday's surprise affects today)
3. **ARIMA combines AR + I (differencing) + MA** for non-stationary data
4. **Box-Jenkins methodology** provides systematic model selection
5. **Financial returns** often show weak patterns (|φ| < 0.2) due to market efficiency
6. **Transaction costs** often exceed statistical edges in liquid markets

**When ARIMA Works:**
- Illiquid assets (large spreads allow exploiting small edges)
- Higher frequencies (patterns exist intraday before arbitraged)
- Combined signals (ARIMA + fundamental + sentiment)
- Risk management (forecasting volatility for position sizing)

**When ARIMA Fails:**
- Regime changes (2020 COVID, 2022 Fed pivot)
- Structural breaks (new regulations, tech changes)
- Non-linear patterns (deep learning may help)
- Fat-tailed shocks (use GARCH, not ARIMA)

**Next Section:**

We've learned to model stationary returns. But what about **relationships between assets**? Section 4 introduces **cointegration**—the foundation of pairs trading and statistical arbitrage.

---

## Section 4: Cointegration - The Foundation of Pairs Trading

### 4.1 The Economic Intuition: Why Spreads Mean-Revert

ARIMA models individual assets. But the real money in quantitative trading often comes from **relationships between assets**.

**The Pairs Trading Insight:**

> "Two companies in the same sector may drift apart temporarily due to idiosyncratic shocks, but economic forces pull them back together."

**Classic Example: Coca-Cola (KO) vs. PepsiCo (PEP)**

Both sell sugary beverages. Both face similar:
- Input costs (sugar, aluminum, transportation)
- Regulatory environment (soda taxes, health regulations)
- Consumer trends (shift to healthier options)
- Competitive dynamics (market share battles)

**What happens if KO rises 10% while PEP is flat?**

1. **Short-term:** News-driven (KO announces new product line)
2. **Medium-term:** Arbitrageurs notice the gap
3. **Long-term:** Economic forces equalize:
   - If KO is overpriced → investors sell KO, buy PEP
   - If both deserve higher valuation → PEP catches up
   - Either way, the **spread** mean-reverts

This is **cointegration**: Two non-stationary price series have a stationary linear combination (the spread).

### 4.2 Mathematical Definition

**Formal Definition:**

Two time series $X_t$ and $Y_t$ are **cointegrated** if:

1. Both are **non-stationary** (integrated of order 1, denoted $I(1)$)
2. There exists a coefficient $\beta$ such that:
   $$Z_t = Y_t - \beta X_t \sim I(0)$$
   (the spread $Z_t$ is stationary)

**Key Insight:**

- $X_t$ wanders (unit root) ✗
- $Y_t$ wanders (unit root) ✗
- But $Y_t - \beta X_t$ is bounded ✓

**Economic Meaning of β:**

β is the **hedge ratio** (or **cointegrating vector**):
- If β = 1: One-for-one relationship (same industry, similar size)
- If β = 0.5: Y moves half as much as X (smaller company)
- If β = 2: Y moves twice as much as X (leveraged relationship)

**Crypto Example: ETH vs BTC**

```
Observation: ETH and BTC prices both trend up over time (non-stationary)
Question: Is there a stable relationship?

Historical data (2023):
- When BTC = $30,000, ETH ≈ $1,850
- When BTC = $40,000, ETH ≈ $2,467

Implied β = ΔETH / ΔBTC = (2467-1850) / (40000-30000) = 617 / 10000 = 0.0617

Cointegration hypothesis:
ETH ≈ 0.0617 × BTC (long-run equilibrium)

Spread = ETH - 0.0617 × BTC
If spread = +$100: ETH is expensive relative to BTC → SHORT spread
If spread = -$100: ETH is cheap relative to BTC → LONG spread
```

### 4.3 The Engle-Granger Two-Step Method

Robert Engle and Clive Granger won the 2003 Nobel Prize in Economics for cointegration. Their two-step test is the industry standard for pairs trading.

**STEP 1: Cointegrating Regression**

Run OLS regression:
$$Y_t = \alpha + \beta X_t + u_t$$

This estimates the hedge ratio $\beta$.

**Important:** This regression is "spurious" in the traditional sense (both variables are non-stationary). But we're not using it for causal inference—we're just finding $\beta$.

**STEP 2: Test Residuals for Stationarity**

Extract residuals (the spread):
$$\hat{u}_t = Y_t - \hat{\alpha} - \hat{\beta} X_t$$

Run ADF test on $\hat{u}_t$:
- **H₀:** Spread has unit root (NOT cointegrated)
- **H₁:** Spread is stationary (cointegrated)

**Critical Detail:**

The ADF critical values are **different** from standard ADF! Why? Because we estimated $\beta$ before testing, which biases the test.

**MacKinnon (1991) Cointegration Critical Values:**
```
Sample size = 100:
1%: -3.90
5%: -3.34
10%: -3.04

vs. Standard ADF (with constant):
1%: -3.43
5%: -2.86
10%: -2.57
```

Cointegration critical values are more negative (harder to reject). We need stronger evidence.

### 4.4 Worked Example: ETH/BTC Pairs Trading

Let's apply Engle-Granger to real data.

**Data:**
- ETH/USD and BTC/USD daily closing prices, 2023 (365 days)
- Both are non-stationary (confirmed via ADF test)

**STEP 1: Cointegrating Regression**

```lisp
(define eth-prices (get-historical-prices "ETH/USD" :days 365 :year 2023))
(define btc-prices (get-historical-prices "BTC/USD" :days 365 :year 2023))

;; Verify non-stationarity
(define adf-eth (adf-test eth-prices :trend "c"))
(define adf-btc (adf-test btc-prices :trend "c"))
;; Results: Both fail to reject unit root (as expected for prices)

;; Cointegrating regression: ETH = α + β·BTC + u
(define X-matrix (hstack (ones (length btc-prices)) btc-prices))
(define regression (ols eth-prices X-matrix))

(define alpha (get regression :coef 0))  ; Intercept
(define beta (get regression :coef 1))   ; Hedge ratio
(define residuals (get regression :residuals))  ; The spread

(log :message (format "Hedge ratio β = {:.4f}" beta))
(log :message (format "Intercept α = {:.2f}" alpha))
(log :message (format "R² = {:.3f}" (get regression :r-squared)))
```

**Results:**
```
Hedge ratio β = 0.0621
Intercept α = -206.43
R² = 0.948 (very tight relationship!)

Model: ETH = -206.43 + 0.0621 × BTC
```

**Interpretation:**
- For every $1 BTC moves, ETH moves $0.0621
- R² = 94.8% means BTC explains most of ETH's variation
- To hedge 1 ETH long, short 0.0621 BTC (or vice versa)

**STEP 2: Test Spread for Stationarity**

```lisp
;; Spread = ETH - β·BTC
(define spread residuals)

;; Run ADF test on spread (NO constant/trend in ADF equation)
(define adf-spread (adf-test spread :trend "nc"))

(log :message (format "ADF statistic: {:.3f}" (get adf-spread :statistic)))
(log :message (format "Cointegration 5%% critical value: -3.34"))
```

**Results:**
```
ADF statistic: -4.12
Critical value (5%): -3.34

Decision: -4.12 < -3.34 → REJECT H₀
Conclusion: ETH and BTC are COINTEGRATED ✓
```

**What This Means for Trading:**

The spread `ETH - 0.0621×BTC` is mean-reverting!

**STEP 3: Spread Statistics and Trading Rules**

```lisp
(define spread-mean (mean spread))
(define spread-std (std spread))
(define current-spread (last spread))
(define z-score (/ (- current-spread spread-mean) spread-std))

(log :message (format "Spread mean: ${:.2f}" spread-mean))
(log :message (format "Spread std dev: ${:.2f}" spread-std))
(log :message (format "Current Z-score: {:.2f}" z-score))
```

**Results:**
```
Spread mean: $0.82 (slightly positive—not economically meaningful)
Spread std dev: $47.23
Current Z-score: 1.85
```

**Trading Strategy:**

```
ENTRY RULES:
IF z-score > +2.0: SHORT spread
  → Sell 1 ETH, Buy 0.0621 BTC
  → Expecting spread to narrow

IF z-score < -2.0: LONG spread
  → Buy 1 ETH, Sell 0.0621 BTC
  → Expecting spread to widen

EXIT RULES:
IF z-score crosses 0: Close position (spread returned to mean)
IF z-score crosses ±4: Stop loss (relationship may have broken)

POSITION SIZING:
- Half-life = -log(2)/log(ρ) where ρ is AR(1) coefficient of spread
- Typical holding period: 1-2 × half-life
- Leverage: 2-3x (spread is stationary, bounded risk)
```

**STEP 4: Half-Life Calculation**

How fast does the spread mean-revert?

```lisp
;; Estimate AR(1) on spread
(define ar1-spread (ar-model spread :order 1))
(define rho (first (get ar1-spread :coefficients)))

;; Half-life = time for half the deviation to disappear
(define half-life (/ (log 0.5) (log rho)))

(log :message (format "Mean reversion speed: ρ = {:.3f}" rho))
(log :message (format "Half-life: {:.1f} days" half-life))
```

**Results:**
```
ρ = 0.82 (strong persistence, but < 1)
Half-life = 3.4 days

Interpretation:
- If spread deviates by $100, expect $50 reversion in 3.4 days
- Typical trade duration: 3-7 days
- High turnover → need low transaction costs
```

```mermaid
---
config:
  themeVariables:
    xyChart:
      backgroundColor: transparent
---
xychart-beta
    title "ETH/BTC Spread Mean Reversion (2023)"
    x-axis [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
    y-axis "Spread ($)" -80 --> 100
    line "Spread" [5, -15, 45, 75, 35, -10, 25, 60, 85, 40, -5, 15]
    line "Mean (0)" [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    line "+2σ (Entry)" [47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47]
    line "-2σ (Entry)" [-47, -47, -47, -47, -47, -47, -47, -47, -47, -47, -47, -47]
```

**Chart Interpretation:**
- **Blue line (Spread):** ETH - 0.0621×BTC oscillates around zero
- **Red line (Mean):** Long-run equilibrium at $0
- **Green lines (±2σ):** Entry thresholds at ±$47
- **Pattern:** Clear mean reversion - spread crosses +2σ in Mar, Aug, Sep → SHORT signals
- **Half-life (3.4 days):** Visible in rapid reversions after peaks

**STEP 5: Backtest (Simplified)**

```lisp
;; Rolling Z-score strategy
(define positions (zeros (length spread)))
(define pnl (array))

(for (t (range 50 (length spread)))  ; Start after 50 days for std calculation
  (let ((rolling-mean (mean (slice spread (- t 50) t)))
        (rolling-std (std (slice spread (- t 50) t)))
        (current-z (/ (- (get spread t) rolling-mean) rolling-std)))

    ;; Generate signal
    (define signal
      (cond
        ((> current-z 2.0) -1)   ; Short spread
        ((< current-z -2.0) 1)   ; Long spread
        ((abs current-z) < 0.5) 0)  ; Exit near zero
        (true (get positions (- t 1)))))  ; Hold existing

    (set-at! positions t signal)

    ;; P&L = position × Δspread
    (if (> t 50)
        (push! pnl (* (get positions (- t 1))
                      (- (get spread t) (get spread (- t 1))))))))

(define total-pnl (sum pnl))
(define sharpe (* (/ (mean pnl) (std pnl)) (sqrt 252)))

(log :message (format "Total P&L: ${:.2f}" total-pnl))
(log :message (format "Sharpe ratio: {:.2f}" sharpe))
```

**Results (2023):**
```
Total P&L: $823 per unit spread traded
Number of trades: 12
Win rate: 67%
Sharpe ratio: 1.82
Max drawdown: -$156

Reality check:
- Transaction costs: ~0.10% × 2 legs = 0.20% per roundtrip
- On $2000 ETH + $30000 BTC position = $64 per trade
- Total costs for 12 trades: $768
- Net P&L: $823 - $768 = $55 (costs ate most gains!)

Conclusion: Profitable, but barely. Need:
1. Higher frequency (more trades to amortize research costs)
2. Lower costs (maker fees, larger size for discounts)
3. Multiple pairs (diversification)
4. Dynamic hedge ratio (Kalman filter—next section)
```

### 4.5 OVSM Implementation: Engle-Granger Cointegration Test

```lisp
;;═══════════════════════════════════════════════════════════════════════════
;; ENGLE-GRANGER COINTEGRATION TEST
;;═══════════════════════════════════════════════════════════════════════════
;;
;; WHAT: Tests if two non-stationary series are cointegrated
;; WHY: Identifies mean-reverting spreads for pairs trading
;; HOW: (1) Regress Y on X to find β, (2) Test residuals for stationarity
;;
;; REFERENCE: Engle & Granger (1987), MacKinnon (1991) for critical values
;;
;; TRADING USE: If cointegrated, trade deviations from equilibrium:
;;              Spread = Y - βX
;;              IF Z-score(Spread) > +2: SHORT spread
;;              IF Z-score(Spread) < -2: LONG spread
;;═══════════════════════════════════════════════════════════════════════════

(define (engle-granger-test y x :alpha 0.05)
  (do
    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 1: Cointegrating Regression Y_t = α + βX_t + u_t
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; PURPOSE: Find the linear combination that might be stationary
    ;;
    ;; CRITICAL: This is NOT a causal model!
    ;;           We're just finding β, not testing if X "causes" Y
    ;;           Both variables are non-stationary (violates OLS assumptions)
    ;;           But that's OK—we're estimating a long-run equilibrium
    ;;
    (define X-matrix (hstack (ones (length x)) x))  ; [1, X]
    (define regression (ols y X-matrix))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 2: Extract Cointegrating Vector (α, β)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; β = Hedge ratio (how many units of X per 1 unit of Y)
    ;; α = Intercept (usually not economically meaningful)
    ;;
    ;; TRADING INTERPRETATION:
    ;; To hedge 1 unit of Y, take -β units of X
    ;; Example: If Y=ETH, X=BTC, β=0.062
    ;;          To hedge 1 ETH long, short 0.062 BTC
    ;;
    (define alpha (get regression :coef 0))
    (define beta (get regression :coef 1))
    (define residuals (get regression :residuals))  ; The spread u_t

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 3: ADF Test on Residuals (THE COINTEGRATION TEST)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; CRITICAL: Use trend="nc" (no constant, no trend)
    ;;
    ;; WHY NO CONSTANT?
    ;; The residuals are already "de-meaned" by the regression
    ;; Including a constant would bias the test
    ;;
    ;; WHY NO TREND?
    ;; If the spread trended, it wouldn't be cointegrated
    ;; (Cointegration requires stationary spread, trends violate this)
    ;;
    (define adf-result (adf-test residuals :trend "nc" :lags 1))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 4: Use COINTEGRATION Critical Values (Not Standard ADF!)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; SOURCE: MacKinnon, J.G. (1991). "Critical Values for Cointegration Tests"
    ;;
    ;; WHY DIFFERENT?
    ;; Standard ADF assumes we're testing a known series
    ;; Here, we estimated β first, which affects the distribution
    ;; The "pre-testing" makes it easier to reject the null by chance
    ;; So we need more negative critical values (stricter test)
    ;;
    ;; RULE OF THUMB (for n ≈ 100-500):
    ;; Cointegration 5% CV ≈ -3.34 (vs. standard ADF -2.86)
    ;;
    ;; For exact values, use MacKinnon's response surface:
    ;; CV(n, k) = c_∞ + c₁/n + c₂/n²
    ;; where k = number of variables (2 for bivariate case)
    ;;
    (define n (length residuals))
    (define crit-values-coint
      (if (< n 100)
          {:1% -3.75 :5% -3.17 :10% -2.91}   ; Small sample (n ≈ 50-100)
          (if (< n 300)
              {:1% -3.90 :5% -3.34 :10% -3.04}  ; Medium sample (n ≈ 100-300)
              {:1% -3.96 :5% -3.41 :10% -3.12})))  ; Large sample (n > 300)

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 5: Decision (Reject Unit Root = Cointegrated)
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; IF adf-stat < critical-value: Spread is stationary → Cointegrated ✓
    ;; IF adf-stat ≥ critical-value: Spread has unit root → NOT cointegrated ✗
    ;;
    (define cointegrated
      (< (get adf-result :statistic) (get crit-values-coint :5%)))

    ;;─────────────────────────────────────────────────────────────────────
    ;; STEP 6: Trading Statistics
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    ;; If cointegrated, compute trading parameters:
    ;; - Mean and std of spread (for Z-score calculation)
    ;; - Half-life of mean reversion (for position holding period)
    ;; - Current signal (entry/exit)
    ;;
    (define spread-mean (mean residuals))
    (define spread-std (std residuals))
    (define current-spread (last residuals))
    (define current-z (/ (- current-spread spread-mean) spread-std))

    ;; Estimate mean reversion speed (AR(1) on spread)
    (define ar1-spread (ar-model residuals :order 1))
    (define rho (first (get ar1-spread :coefficients)))
    (define half-life (if (and (> rho 0) (< rho 1))
                          (/ (log 0.5) (log rho))
                          null))  ; undefined if ρ ≤ 0 or ρ ≥ 1

    ;; Trading signal
    (define signal
      (cond
        ((> current-z 2.0) "SHORT_SPREAD")   ; Spread too wide → short
        ((< current-z -2.0) "LONG_SPREAD")   ; Spread too narrow → long
        ((< (abs current-z) 0.5) "EXIT")     ; Near equilibrium → exit
        (true "HOLD")))                       ; Within normal range → hold

    ;;─────────────────────────────────────────────────────────────────────
    ;; RETURN COMPREHENSIVE RESULTS
    ;;─────────────────────────────────────────────────────────────────────
    ;;
    {:method "Engle-Granger"
     :cointegrated cointegrated
     :hedge-ratio beta
     :intercept alpha
     :r-squared (get regression :r-squared)

     ;; Test statistics
     :adf-statistic (get adf-result :statistic)
     :critical-values crit-values-coint
     :p-value (if (< (get adf-result :statistic) (get crit-values-coint :1%))
                  "< 0.01"
                  (if (< (get adf-result :statistic) (get crit-values-coint :5%))
                      "< 0.05"
                      "> 0.10"))

     ;; Spread (residuals)
     :spread residuals

     ;; Trading parameters
     :spread-stats {:mean spread-mean
                    :std spread-std
                    :current current-spread
                    :z-score current-z
                    :half-life half-life
                    :mean-reversion-speed rho}

     ;; Current signal
     :signal signal

     ;; Human-readable interpretation
     :interpretation
       (if cointegrated
           (format "✓ COINTEGRATED (ADF={:.2f} < CV={:.2f}). Hedge ratio β={:.4f}. Half-life={:.1f} periods. Current signal: {}"
                   (get adf-result :statistic)
                   (get crit-values-coint :5%)
                   beta
                   (if half-life half-life 999)
                   signal)
           (format "✗ NOT COINTEGRATED (ADF={:.2f} > CV={:.2f}). Cannot trade this pair."
                   (get adf-result :statistic)
                   (get crit-values-coint :5%)))}))
```

**Usage Example:**

```lisp
;; Test ETH/BTC cointegration
(define eth-prices (get-historical-prices "ETH/USD" :days 365))
(define btc-prices (get-historical-prices "BTC/USD" :days 365))

(define result (engle-granger-test eth-prices btc-prices))

;; Check if cointegrated
(if (get result :cointegrated)
    (do
      (log :message (get result :interpretation))

      ;; Extract trading parameters
      (define beta (get result :hedge-ratio))
      (define z-score (get (get result :spread-stats) :z-score))
      (define signal (get result :signal))

      ;; Execute trade if signal
      (if (= signal "SHORT_SPREAD")
          (do
            (log :message "📉 SHORT SPREAD: Sell 1 ETH, Buy {:.4f} BTC" beta)
            ;; place-order "ETH/USD" "sell" 1
            ;; place-order "BTC/USD" "buy" beta
            ))

      (if (= signal "LONG_SPREAD")
          (do
            (log :message "📈 LONG SPREAD: Buy 1 ETH, Sell {:.4f} BTC" beta)
            ;; place-order "ETH/USD" "buy" 1
            ;; place-order "BTC/USD" "sell" beta
            )))
    (log :message "⚠️ Pair not cointegrated. Do not trade."))
```

### 4.6 Error Correction Models (ECM): Modeling the Dynamics

Cointegration tells us a long-run equilibrium exists. But how do prices adjust back?

**The Error Correction Model:**

$$\Delta Y_t = \gamma(Y_{t-1} - \beta X_{t-1}) + \text{short-run dynamics} + \epsilon_t$$

Where:
- $\Delta Y_t$ = change in $Y$
- $(Y_{t-1} - \beta X_{t-1})$ = **error correction term (ECT)**—yesterday's deviation from equilibrium
- $\gamma$ = speed of adjustment (how fast errors correct)

**Interpretation of γ:**

| γ Value | Meaning | Half-Life |
|---------|---------|-----------|
| γ = -0.50 | 50% of deviation corrects each period | 1 period |
| γ = -0.20 | 20% correction per period | 3.1 periods |
| γ = -0.10 | 10% correction per period | 6.6 periods |
| γ = 0 | No correction (not cointegrated!) | ∞ |
| γ > 0 | Explosive (errors grow!) | Invalid |

**Half-life formula:**
$$\text{Half-life} = \frac{\log(0.5)}{\log(1 + \gamma)}$$

**Worked Example: ETH/BTC ECM**

```lisp
;; From previous cointegration test
(define spread (get eg-result :spread))
(define lag-spread (lag spread 1))
(define delta-eth (diff eth-prices))

;; Regress ΔET on ECT_{t-1}
(define X-ecm (hstack (ones (length delta-eth)) lag-spread))
(define ecm-regression (ols delta-eth X-ecm))

(define const (get ecm-regression :coef 0))
(define gamma (get ecm-regression :coef 1))  ; Speed of adjustment

(log :message (format "Error correction speed γ = {:.3f}" gamma))
(log :message (format "Half-life = {:.1f} days" (/ (log 0.5) (log (+ 1 gamma)))))
```

**Results:**
```
γ = -0.18 (t = -4.2, p < 0.001)
Half-life = 3.5 days

Interpretation:
- 18% of yesterday's spread deviation corrects today
- If spread is $100 too wide, expect $18 narrowing tomorrow
- Typical trade duration: 3-7 days (1-2 × half-life)
```

**Trading Implication:**

Faster mean reversion (more negative γ) → better for pairs trading:
- Larger γ (e.g., -0.50): Quick trades, high turnover, high Sharpe
- Smaller γ (e.g., -0.05): Slow trades, long holds, more risk

### 4.7 When Cointegration Breaks: Rolling Window Analysis

**The Danger:**

Cointegration is not permanent! Relationships break during:
- Regulatory changes (one company faces new rules)
- Management shake-ups (CEO departure)
- Product failures (one company's key product flops)
- M&A rumors (one company becomes acquisition target)
- Market regime changes (risk-on vs risk-off)

**Example: KO vs PEP during COVID**

Pre-COVID (2019): Cointegrated (both benefit from restaurants, events)
COVID (Mar 2020): Relationship broke (stay-at-home benefited PEP more than KO)
Post-vaccine (2021): Re-cointegrated

**Solution: Rolling Window Tests**

Test cointegration in overlapping windows (e.g., 252 trading days, step 21 days):

```lisp
(define (rolling-cointegration y x :window 252 :step 21)
  (define results (array))

  (for (start (range 0 (- (length y) window) step))
    (let ((end (+ start window))
          (y-window (slice y start end))
          (x-window (slice x start end))
          (test-result (engle-granger-test y-window x-window)))

      (push! results
             {:date (get-date end)
              :cointegrated (get test-result :cointegrated)
              :hedge-ratio (get test-result :hedge-ratio)
              :adf-stat (get test-result :adf-statistic)
              :half-life (get (get test-result :spread-stats) :half-life)})))

  results)

;; Analyze stability
(define results (rolling-cointegration eth-prices btc-prices))
(define pct-cointegrated
  (* 100 (/ (count-where results (lambda (r) (get r :cointegrated)))
            (length results))))

(log :message (format "Cointegrated {:.0f}% of rolling windows" pct-cointegrated))

;; If < 70%, relationship is unstable → don't trade
(if (< pct-cointegrated 70)
    (log :message "⚠️ WARNING: Cointegration unstable. High risk."))
```

### 4.8 Summary: Cointegration for Pairs Trading

**Key Takeaways:**

1. **Cointegration = mean-reverting spread** between two non-stationary series
2. **Engle-Granger two-step:** (1) Regress to find β, (2) Test residuals
3. **Use cointegration critical values** (more negative than standard ADF)
4. **Half-life** determines trade duration (1-2 × half-life typical)
5. **Transaction costs** often dominate—need tight spreads or high volume
6. **Rolling window tests** essential—cointegration breaks!

**When Pairs Trading Works:**
- Stable economic relationship (same sector, similar business models)
- High cointegration persistence (>80% of rolling windows)
- Fast mean reversion (half-life < 10 days)
- Low transaction costs (<0.05% per leg)

**When It Fails:**
- Regime changes (COVID, regulation, management turnover)
- Low liquidity (wide spreads, slippage)
- Slow mean reversion (half-life > 30 days → capital tied up)
- Parameter instability (β changes frequently)

**Next Section:**

Static hedge ratios (β from Engle-Granger) assume the relationship is constant. But what if β changes over time? We'll briefly cover **Kalman filters**—a dynamic approach that tracks time-varying parameters.

---

## Section 5: Kalman Filters - Dynamic Parameter Tracking (Brief Overview)

### 5.1 The Problem with Static Hedge Ratios

From Section 4, we learned that ETH and BTC are cointegrated with β ≈ 0.062. But this assumes β is constant throughout 2023.

**Reality Check:**

```
Q1 2023: β = 0.0598
Q2 2023: β = 0.0621
Q3 2023: β = 0.0587
Q4 2023: β = 0.0634

Range: 0.0587 to 0.0634 (8% variation)
```

If we use the annual average β = 0.062:
- In Q4 (β actually = 0.063), we're under-hedged → larger drawdowns
- In Q3 (β actually = 0.059), we're over-hedged → leaving money on table

**Solution:** Track β dynamically using a **Kalman filter**.

### 5.2 The Kalman Filter Intuition

**Simple Idea:**
> "Yesterday's β estimate + today's new information = updated β estimate"

**The Math (State-Space Form):**

**State equation (how β evolves):**
$$\beta_t = \beta_{t-1} + w_t$$

β follows a random walk (changes slowly over time).

**Observation equation (what we observe):**
$$\text{ETH}_t = \beta_t \times \text{BTC}_t + v_t$$

We observe ETH and BTC prices, but β is hidden.

**Kalman Filter's Job:**
1. **Predict:** What do we think β is today, given yesterday's estimate?
2. **Update:** Given today's ETH/BTC prices, revise the estimate
3. **Balance:** Trust yesterday's estimate vs. today's data (Kalman gain determines this)

**Analogy:**

Imagine tracking someone's walking speed:
- **Prior belief:** "They were walking 3 mph yesterday"
- **New observation:** "They covered 1 mile in 18 minutes = 3.3 mph"
- **Kalman update:** "New estimate: 3.15 mph" (weighted average)

The weight depends on:
- How certain we are about prior (low uncertainty → trust prior more)
- How noisy observations are (high noise → trust prior more)

### 5.3 Benefits for Pairs Trading

**Kalman Filter vs. Static β:**

| Metric | Static β (Engle-Granger) | Dynamic β (Kalman) |
|--------|--------------------------|---------------------|
| **Sharpe Ratio** | 1.82 | 2.14 (+18%) |
| **Max Drawdown** | -$156 | -$124 (-21%) |
| **Win Rate** | 67% | 72% |
| **Avg Hold Time** | 5.2 days | 4.8 days |

**Why Better?**

1. **Adapts to regime changes** (β adjusts when market dynamics shift)
2. **Tighter spreads** (better hedge → smaller residual risk)
3. **Fewer whipsaws** (dynamic threshold based on current β uncertainty)

### 5.4 Practical Considerations

**When to Use Kalman Filters:**
- ✓ High-frequency trading (parameters change quickly)
- ✓ Multiple pairs (computational efficiency matters less)
- ✓ Volatile relationships (crypto, emerging markets)

**When Static β is Fine:**
- ✓ Stable, liquid pairs (KO/PEP, XOM/CVX)
- ✓ Low-frequency trading (weekly rebalancing)
- ✓ Simplicity matters (production systems, easier to monitor)

**OVSM Implementation Note:**

Full Kalman filter implementation requires matrix operations (covariance updates, matrix inversion). This adds ~150 lines of code. Given chapter length, we defer to Section 11 (which covers Kalman filtering for pairs trading in complete detail with OVSM code).

**Quick Concept:**

```lisp
;; Simplified Kalman update (conceptual, not production-ready)
(define (update-beta-kalman prior-beta prior-uncertainty eth-price btc-price)
  (do
    ;; Prediction step
    (define predicted-beta prior-beta)  ; Random walk: β_t = β_{t-1}
    (define predicted-uncertainty (+ prior-uncertainty process-noise))

    ;; Innovation (forecast error)
    (define predicted-eth (* predicted-beta btc-price))
    (define innovation (- eth-price predicted-eth))

    ;; Kalman gain (how much to trust new data)
    (define K (/ predicted-uncertainty
                 (+ predicted-uncertainty measurement-noise)))

    ;; Update
    (define new-beta (+ predicted-beta (* K innovation / btc-price)))
    (define new-uncertainty (* (- 1 K) predicted-uncertainty))

    {:beta new-beta :uncertainty new-uncertainty}))
```

**Key Takeaway:**

Kalman filters provide a principled way to track time-varying parameters. For pairs trading, this translates to ~15-25% Sharpe improvement in practice. See Chapter 11 for complete implementation.

---

## Summary: Time Series Analysis Toolkit

### What We've Learned

This chapter covered the mathematical and practical foundations of time series analysis for algorithmic trading:

**1. Stationarity (Section 2)**
- **Why it matters:** Non-stationary data causes spurious regressions, invalid inference
- **How to test:** ADF test (null: unit root) + KPSS test (null: stationary)
- **How to fix:** Differencing (prices → returns) or detrending
- **Key insight:** Returns are stationary, prices aren't

**2. ARIMA Models (Section 3)**
- **AR(p):** Models autocorrelation (φ > 0 = momentum, φ < 0 = reversion)
- **MA(q):** Models shock persistence (yesterday's error affects today)
- **ARIMA(p,d,q):** Combines AR + differencing + MA
- **Box-Jenkins methodology:** Systematic 3-step process (identify → estimate → diagnose)
- **Reality check:** Patterns in liquid markets are weak (|φ| < 0.2) and fleeting

**3. Cointegration (Section 4)**
- **What:** Two non-stationary series have stationary spread
- **Why:** Enables pairs trading (mean-reverting spreads)
- **How to test:** Engle-Granger (regress → test residuals with special critical values)
- **Trading application:** Z-score entry (±2), exit at mean, stop at ±4
- **Critical:** Test in rolling windows—cointegration breaks!

**4. Dynamic Parameters (Section 5)**
- **Problem:** Static β assumes constant relationships
- **Solution:** Kalman filter tracks time-varying parameters
- **Benefit:** ~20% Sharpe improvement for pairs trading
- **Trade-off:** Complexity vs. performance (worthwhile for active strategies)

### The Three Questions Framework

Before deploying any time series strategy, answer these:

**Question 1: Is the pattern stationary?**
```
Tool: ADF + KPSS tests
Decision: If non-stationary → difference or abandon
```

**Question 2: Is it strong enough to trade?**
```
For AR: |φ| > 0.10 and statistically significant (p < 0.05)
For cointegration: Half-life < 10 days, >80% rolling window persistence
Transaction cost check: Expected return > 2 × costs
```

**Question 3: Is it stable over time?**
```
Tool: Rolling window analysis (252-day windows, 21-day step)
Decision: If parameter varies >30% → use dynamic methods or skip
Out-of-sample validation: RMSE_test ≈ RMSE_train (within 10%)
```

### Common Pitfalls (Checklist)

| Mistake | Consequence | Prevention |
|---------|-------------|------------|
| **Model prices directly** | Spurious patterns, unbounded risk | ALWAYS difference to returns first |
| **Ignore transaction costs** | "Profitable" backtest → losing live | Subtract realistic costs (0.10-0.20%) |
| **Use wrong ADF critical values** | False cointegration detection | Use MacKinnon (1991) for cointegration |
| **Overfit ARIMA orders** | Great in-sample, fails out-sample | Use BIC (stronger penalty than AIC) |
| **Assume cointegration is permanent** | Blow up when relationship breaks | Rolling window tests, stop-loss at 4σ |
| **Ignore regime changes** | 2020 strategies fail in 2024 | Monitor parameters monthly, kill stale strategies |

### When Time Series Analysis Works

**Ideal Conditions:**
- ✓ Stationary data (returns, spreads) or can be made stationary
- ✓ Sufficient history (>252 observations for daily, >1000 for hourly)
- ✓ Stable relationships (cointegration in >80% of rolling windows)
- ✓ Low transaction costs (<0.05% per leg)
- ✓ Fast mean reversion (half-life < 10 periods)

```mermaid
quadrantChart
    title Time Series Strategy Selection Matrix
    x-axis "Weak Pattern (|φ| < 0.1)" --> "Strong Pattern (|φ| > 0.2)"
    y-axis "Slow Reversion (τ > 20d)" --> "Fast Reversion (τ < 5d)"
    quadrant-1 "🚀 HIGH FREQUENCY<br/>Momentum + Quick Scalp<br/>BTC hourly, limit orders"
    quadrant-2 "✓ PAIRS TRADING<br/>ETH/BTC cointegration<br/>Best risk/reward"
    quadrant-3 "✗ AVOID<br/>Weak + Slow<br/>Not tradeable"
    quadrant-4 "⚠️ POSITION TRADING<br/>Weekly equity momentum<br/>High cost risk"
    ETH/BTC Pairs: [0.75, 0.80]
    BTC Hourly AR: [0.65, 0.25]
    SPY Weekly: [0.35, 0.15]
    Random Walk: [0.10, 0.50]
```

**Success Examples:**
- **Pairs trading:** ETH/BTC, equity sector pairs (XLE/XLF)
- **Spread trading:** Futures calendar spreads, cash-futures basis
- **FX carry:** Interest rate differentials with slow mean reversion
- **Options vol trading:** Implied vs. realized vol spreads

### When It Fails

**Warning Signs:**
- ⚠️ Non-stationary after 2 differences (d > 2 suggests data quality issues)
- ⚠️ Slow mean reversion (half-life > 30 days ties up capital)
- ⚠️ Parameter instability (β changes >30% quarter-over-quarter)
- ⚠️ Recent regime change (COVID, Fed pivot, new regulation)
- ⚠️ High transaction costs (>0.20% per roundtrip eats edges)

**Failure Examples:**
- **LTCM 1998:** Cointegration broke during Russian crisis
- **Quant meltdown (Aug 2007):** All pairs strategies unwound simultaneously
- **COVID (Mar 2020):** Historical correlations collapsed
- **FTX collapse (Nov 2022):** Crypto correlations went to 1 (diversification failed)

### Production Deployment Checklist

Before going live with a time series strategy:

**1. Validation**
```
☐ Passes ADF/KPSS tests (stationary data)
☐ Ljung-Box test on residuals (p > 0.05, no autocorrelation left)
☐ Out-of-sample RMSE within 10% of in-sample
☐ Walk-forward backtest across 3+ years
☐ Profitable in recent 6 months (not just ancient history)
```

**2. Risk Management**
```
☐ Position sizing: Kelly criterion or fixed-fractional (2-5% risk per trade)
☐ Stop-loss: 4σ for pairs trading, 3× historical max drawdown
☐ Maximum holding period: 3 × half-life (exit if not converged)
☐ Correlation limits: <0.7 correlation with other strategies (diversification)
```

**3. Monitoring**
```
☐ Daily: Check if still cointegrated (rolling 60-day window)
☐ Weekly: Recompute β, update thresholds
☐ Monthly: Full re-estimation, compare to expectations
☐ Quarterly: Strategy review—kill if Sharpe < 0.5 for 90 days
```

**4. Infrastructure**
```
☐ Data quality: Handle missing data, outliers, corporate actions
☐ Execution: Account for slippage, partial fills
☐ Latency: Can execute within half-life timeframe
☐ Costs: Track actual costs vs. assumed (0.10% often becomes 0.25%)
```

### Key OVSM Functions Developed

This chapter provided production-ready implementations:

```lisp
;; Stationarity testing
(adf-test series :lags 12 :trend "c")          ; Unit root test
(kpss-test series :lags null :trend "c")       ; Stationarity test
(make-stationary series :max-diffs 2)          ; Auto-transform

;; ARIMA modeling
(ar-model data :order 1)                        ; Autoregressive model
(arima-model data :p 1 :d 1 :q 1)              ; Full ARIMA
(auto-arima data :max-p 5 :max-q 5)            ; Automatic order selection

;; Cointegration
(engle-granger-test y x :alpha 0.05)           ; Pairs trading test
(rolling-cointegration y x :window 252)        ; Stability check

;; Diagnostics
(ljung-box-test residuals :lags 20)            ; Residual autocorrelation
(jarque-bera-test residuals)                   ; Normality test
```

All functions include:
- Detailed WHAT/WHY/HOW comments
- Trading interpretation
- Error handling
- Human-readable output

### Next Steps

**Chapter 9: Backtesting Frameworks**

Now that we can model time series and identify tradeable patterns, Chapter 9 covers:
- Walk-forward validation (avoiding look-ahead bias)
- Event-driven backtesting architecture
- Transaction cost modeling (realistic slippage, fees)
- Performance metrics (Sharpe, Sortino, max drawdown, Calmar)
- Common backtest bugs (survivorship bias, data snooping)

**Chapter 10: Production Systems**

Chapter 10 takes strategies from backtest to production:
- Order management systems (limit orders, market orders, IOC)
- Risk checks (pre-trade, real-time, end-of-day)
- Data pipelines (market data ingestion, cleaning, storage)
- Monitoring dashboards (live P&L, positions, alerts)
- Failure modes (connectivity loss, exchange outages, fat fingers)

### Final Thoughts

Time series analysis is both powerful and dangerous:

**Powerful because:**
- Mathematically rigorous (ADF, cointegration tests have solid statistical foundations)
- Empirically validated (pairs trading profitable for decades)
- Computationally efficient (closed-form solutions, no deep learning needed)

**Dangerous because:**
- Past patterns don't guarantee future results
- Regime changes break relationships (LTCM, 2008, COVID)
- Easy to overfit (p-hacking, data snooping)
- Transaction costs often exceed edges in liquid markets

**The Successful Approach:**

1. **Be skeptical:** Assume patterns are spurious until proven otherwise
2. **Test rigorously:** ADF + KPSS, out-of-sample validation, rolling windows
3. **Start simple:** AR(1) often performs as well as ARIMA(5,2,3)
4. **Monitor constantly:** Relationships change—detect early, exit fast
5. **Diversify:** Never bet the farm on one pair or one model

> "Time series analysis doesn't predict the future. It identifies when the past has become irrelevant and when it might still matter—barely." — Adapted from Nassim Taleb

**You now have the tools. Use them wisely.**

---

## References

1. **Engle, R.F., & Granger, C.W.J. (1987).** "Co-integration and error correction: Representation, estimation, and testing." *Econometrica*, 55(2), 251-276.
   - Nobel Prize-winning paper on cointegration

2. **Box, G.E.P., Jenkins, G.M., & Reinsel, G.C. (2015).** *Time Series Analysis: Forecasting and Control* (5th ed.). Wiley.
   - The ARIMA bible

3. **Hamilton, J.D. (1994).** *Time Series Analysis*. Princeton University Press.
   - Graduate-level treatment, rigorous proofs

4. **Tsay, R.S. (2010).** *Analysis of Financial Time Series* (3rd ed.). Wiley.
   - Finance-specific applications

5. **MacKinnon, J.G. (1996).** "Numerical distribution functions for unit root and cointegration tests." *Journal of Applied Econometrics*, 11(6), 601-618.
   - Critical values for ADF and cointegration tests

6. **Dickey, D.A., & Fuller, W.A. (1979).** "Distribution of the estimators for autoregressive time series with a unit root." *Journal of the American Statistical Association*, 74(366a), 427-431.
   - Original ADF test

7. **Johansen, S. (1991).** "Estimation and hypothesis testing of cointegration vectors in Gaussian vector autoregressive models." *Econometrica*, 59(6), 1551-1580.
   - Multivariate cointegration

8. **Kalman, R.E. (1960).** "A new approach to linear filtering and prediction problems." *Journal of Basic Engineering*, 82(1), 35-45.
   - Original Kalman filter paper

9. **Lowenstein, R. (2000).** *When Genius Failed: The Rise and Fall of Long-Term Capital Management*. Random House.
   - LTCM case study

10. **Chan, E.P. (2013).** *Algorithmic Trading: Winning Strategies and Their Rationale*. Wiley.
    - Practical pairs trading strategies

---

**End of Chapter 8**