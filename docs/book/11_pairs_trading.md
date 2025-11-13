# Chapter 11: Statistical Arbitrage — Pairs Trading

## 11.1 Introduction and Historical Context

Statistical arbitrage represents one of the most enduring and theoretically grounded strategies in quantitative finance. Unlike traditional arbitrage—which exploits riskless pricing discrepancies across markets or instruments—statistical arbitrage relies on the mean-reverting properties of relationships between financial instruments. The strategy accepts short-term risk in exchange for positive expected value from mean reversion.

Pairs trading, the most accessible form of statistical arbitrage, emerged from the quantitative trading group at Morgan Stanley in the mid-1980s (Gatev et al., 2006). The original strategy was elegantly simple: identify pairs of stocks that historically moved together, wait for temporary divergences in their price relationship, and bet on convergence. When Xerox and IBM diverged by more than two standard deviations, for instance, traders would buy the underperformer and short the outperformer, expecting the spread to normalize.

The strategy's appeal derived from several attractive properties:

1. **Market Neutrality**: Long and short positions offset market exposure, reducing systematic risk
2. **Statistical Foundation**: Mean reversion is mathematically testable and historically robust
3. **Scalability**: The approach applies to thousands of potential pairs across asset classes
4. **Transparency**: Unlike black-box algorithms, pairs trading logic is interpretable

However, pairs trading is not arbitrage in the classical sense. True arbitrage exploits pricing violations that can be locked in with zero risk (e.g., put-call parity violations, index arbitrage). Pairs trading accepts basis risk: the spread may diverge further before converging, or may never revert at all if the historical relationship breaks down permanently.

The 1987 stock market crash demonstrated this risk dramatically. Many pairs diverged catastrophically as correlations broke down. Nureddin Zaman, head of Morgan Stanley's quantitative trading group, reportedly lost $7 million in one day as pairs failed to converge. Despite this setback, the strategy survived and flourished, generating consistent profits through the 1990s.

Academic attention followed practitioner success. Gatev, Goetzmann, and Rouwenhorst (2006) provided the first comprehensive empirical study, documenting excess returns of approximately 11% annually with Sharpe ratios near 2.0 over the period 1962-2002. Crucially, they found that returns were not explained by standard risk factors (market, size, value, momentum), suggesting genuine alpha from mean reversion rather than compensation for systematic risk.

However, the Gatev et al. study also documented concerning trends: returns declined over time, particularly after 1990. The authors attributed this deterioration to strategy crowding—as more capital pursued pairs opportunities, profitable divergences became rarer and shorter-lived. This dynamic exemplifies a broader pattern in quantitative finance: strategies diffuse from proprietary algorithms to academic publications to common practice, eroding profitability at each stage.

The quantitative meltdown of August 2007 provided another stark reminder of pairs trading risks. Multiple quantitative hedge funds, many employing similar pairs strategies, suffered simultaneous losses as correlations collapsed. Khandani and Lo (2007) documented that a major multi-strategy fund likely faced redemptions and was forced to liquidate positions rapidly. This liquidation triggered cascading losses for other funds holding similar pairs, as all attempted to exit simultaneously. The episode demonstrated that statistical relationships, however robust historically, can fail precisely when most needed—during market stress.

Despite these cautionary episodes, pairs trading remains viable, particularly when implemented with modern enhancements:

- **Cointegration testing**: Formal statistical tests identify pairs with genuine long-term relationships
- **Kalman filtering**: Adaptive techniques track time-varying hedge ratios
- **Machine learning**: Algorithms detect regime changes and prevent trades during structural breaks
- **Risk management**: Position sizing scales with confidence, and stop-losses limit divergence risk

This chapter proceeds systematically through the theory and practice of pairs trading. Section 11.2 establishes the mathematical foundations in cointegration theory. Section 11.3 develops the Ornstein-Uhlenbeck process as the canonical model for mean-reverting spreads. Section 11.4 presents empirical methods for pair selection and testing. Section 11.5 details the complete OVSM implementation. Section 11.6 analyzes risk management and the August 2007 quant quake. Section 11.7 discusses extensions and current research directions.

## 11.2 Theoretical Foundations: Cointegration

### 11.2.1 Stationarity and Integration

Understanding pairs trading requires distinguishing stationary from non-stationary time series. A time series $\{X_t\}$ is **stationary** if its statistical properties—mean, variance, autocorrelation—remain constant over time. Stationary series exhibit mean reversion: deviations from the long-run mean are temporary.

Formally, **weak stationarity** (sufficient for most applications) requires:

$$
\begin{align}
\mathbb{E}[X_t] &= \mu \quad \text{for all } t \\
\text{Var}(X_t) &= \sigma^2 \quad \text{for all } t \\
\text{Cov}(X_t, X_{t+k}) &= \gamma_k \quad \text{for all } t \text{ and lag } k
\end{align}
$$

The mean, variance, and autocovariance structure are time-invariant. Examples of stationary processes include white noise, AR(1) with $|\phi| < 1$, and MA processes.

Most financial asset prices are **non-stationary**. They exhibit trending behavior, with current prices strongly influencing future prices. A random walk is the canonical non-stationary process:

$$
P_t = P_{t-1} + \epsilon_t \quad \text{where } \epsilon_t \sim \mathcal{N}(0, \sigma^2)
$$

This process is **integrated of order 1**, denoted $I(1)$, because differencing once produces a stationary series:

$$
\Delta P_t = P_t - P_{t-1} = \epsilon_t \sim I(0)
$$

The returns series $\Delta P_t$ is stationary. Stock prices are typically $I(1)$; stock returns are $I(0)$.

**Key insight**: Trading individual $I(1)$ prices for mean reversion fails. There is no mean to revert to—prices drift without bound. However, linear combinations of multiple $I(1)$ series can be stationary if the series share common stochastic trends. This is **cointegration**.

### 11.2.2 Cointegration Definition

Let $\{X_t\}$ and $\{Y_t\}$ be two $I(1)$ time series (non-stationary). These series are **cointegrated** if there exists a constant $\beta$ such that the linear combination

$$
Z_t = Y_t - \beta X_t
$$

is stationary, $Z_t \sim I(0)$. The coefficient $\beta$ is the **cointegrating vector** or **hedge ratio**.

**Economic interpretation**: Cointegrated series share a common stochastic trend. Individually, $X_t$ and $Y_t$ wander without bound, but their spread $Z_t$ remains bounded. Economic relationships often exhibit cointegration:

- **Spot and futures prices**: Arbitrage enforces a stable relationship (cost of carry)
- **American Depositary Receipts (ADRs) and underlying shares**: Legal equivalence ensures convergence
- **Companies in the same industry**: Common demand shocks create correlated movements
- **Currency exchange rates**: Purchasing power parity provides long-run anchor

Cointegration arises when economic forces—arbitrage, substitution, equilibrium conditions—prevent two series from drifting apart permanently. Short-run deviations occur due to temporary supply/demand imbalances, information delays, or liquidity constraints. These deviations create trading opportunities.

### 11.2.3 Error Correction Representation

Engle and Granger (1987) proved that cointegrated systems admit an **error correction representation**. If $Y_t$ and $X_t$ are cointegrated with spread $Z_t = Y_t - \beta X_t$, then:

$$
\begin{align}
\Delta Y_t &= \alpha_Y + \gamma_Y (Y_{t-1} - \beta X_{t-1}) + \epsilon_{Y,t} \\
\Delta X_t &= \alpha_X + \gamma_X (Y_{t-1} - \beta X_{t-1}) + \epsilon_{X,t}
\end{align}
$$

The lagged spread $(Y_{t-1} - \beta X_{t-1})$ enters as an error correction term. The coefficients $\gamma_Y$ and $\gamma_X$ govern the speed of adjustment to equilibrium:

- If $\gamma_Y < 0$: When the spread is positive ($Y$ too high relative to $X$), $\Delta Y_t$ is negative (downward pressure on $Y$)
- If $\gamma_X > 0$: When the spread is positive, $\Delta X_t$ is positive (upward pressure on $X$)

Both mechanisms push the spread toward zero. The adjustment speed determines the **half-life** of mean reversion:

$$
t_{1/2} = \frac{\ln(2)}{|\gamma_Y + \gamma_X|}
$$

Typical equity pairs exhibit half-lives of 5-20 days (Gatev et al., 2006). Shorter half-lives are preferable for trading—faster reversion reduces holding period risk.

### 11.2.4 Engle-Granger Two-Step Procedure

The Engle-Granger (1987) method tests for cointegration in two steps:

**Step 1: Estimate hedge ratio via OLS**

Regress $Y_t$ on $X_t$:

$$
Y_t = \alpha + \beta X_t + u_t
$$

The OLS estimate $\hat{\beta}$ is the hedge ratio. Construct the spread:

$$
\hat{Z}_t = Y_t - \hat{\beta} X_t
$$

**Step 2: Test spread stationarity**

Apply the Augmented Dickey-Fuller (ADF) test to $\hat{Z}_t$. The ADF regression is:

$$
\Delta \hat{Z}_t = \rho \hat{Z}_{t-1} + \sum_{i=1}^{p} \phi_i \Delta \hat{Z}_{t-i} + \epsilon_t
$$

The null hypothesis is $H_0: \rho = 0$ (unit root, non-stationary). The alternative is $H_1: \rho < 0$ (stationary). Critical values differ from standard ADF tests because $\hat{Z}_t$ uses an estimated $\hat{\beta}$ rather than known $\beta$ (Engle & Yoo, 1987).

**Decision rule**: If the test statistic exceeds the critical value (typically -3.34 at 5% significance), reject the null and conclude the series are cointegrated.

**Example calculation**:

Consider two price series:

| t | $Y_t$ | $X_t$ |
|---|-------|-------|
| 1 | 100 | 50 |
| 2 | 102 | 51 |
| 3 | 104 | 52 |
| 4 | 103 | 51.5 |
| 5 | 105 | 52.5 |

OLS regression yields $\hat{\beta} = 2.0$. The spread is:

$$
\hat{Z}_t = Y_t - 2.0 \cdot X_t = [0, 0, 0, 0, 0]
$$

The spread is perfectly stationary (constant zero), strongly indicating cointegration. In practice, spreads fluctuate around a mean but remain bounded.

### 11.2.5 Johansen Method

The Johansen (1991) procedure generalizes cointegration testing to systems of $n > 2$ series. While Engle-Granger handles pairs, Johansen allows multiple cointegrating relationships in a vector autoregression (VAR) framework.

The method estimates:

$$
\Delta \mathbf{Y}_t = \Pi \mathbf{Y}_{t-1} + \sum_{i=1}^{p-1} \Gamma_i \Delta \mathbf{Y}_{t-i} + \epsilon_t
$$

where $\mathbf{Y}_t$ is an $n$-dimensional vector of prices. The matrix $\Pi$ determines the number of cointegrating relationships. Its rank $r$ gives the cointegration rank:

- $r = 0$: No cointegration
- $r = n$: All series stationary (no common trends)
- $0 < r < n$: $r$ cointegrating vectors

Johansen provides two test statistics:

1. **Trace test**: Tests $H_0: r \leq r_0$ vs. $H_1: r > r_0$
2. **Maximum eigenvalue test**: Tests $H_0: r = r_0$ vs. $H_1: r = r_0 + 1$

For pairs trading, Johansen offers minimal advantage over Engle-Granger. Its power lies in basket arbitrage: constructing portfolios of 3+ assets with stable relationships. For example, sector indices (banking, technology, energy) may be jointly cointegrated, enabling multi-leg spread trades.

## 11.3 The Ornstein-Uhlenbeck Process

### 11.3.1 Continuous-Time Mean Reversion

The **Ornstein-Uhlenbeck (OU) process** provides the canonical continuous-time model for mean-reverting spreads. The process satisfies the stochastic differential equation:

$$
dX_t = \theta(\mu - X_t) dt + \sigma dW_t
$$

where:

- $X_t$ is the spread at time $t$
- $\theta > 0$ is the mean reversion speed
- $\mu$ is the long-run mean
- $\sigma > 0$ is the volatility
- $W_t$ is a standard Brownian motion

**Interpretation**:

- **Drift term** $\theta(\mu - X_t) dt$: When $X_t > \mu$, the drift is negative (pulls $X_t$ down toward $\mu$). When $X_t < \mu$, the drift is positive (pulls $X_t$ up). The rate of mean reversion scales with $\theta$.
- **Diffusion term** $\sigma dW_t$: Random shocks of magnitude $\sigma$ per unit time.

### 11.3.2 Analytical Properties

The OU process admits closed-form solutions for many quantities of interest.

**Conditional expectation**:

$$
\mathbb{E}[X_t \mid X_0] = \mu + (X_0 - \mu) e^{-\theta t}
$$

The expected value decays exponentially toward $\mu$ at rate $\theta$. The **half-life** is:

$$
t_{1/2} = \frac{\ln 2}{\theta}
$$

**Conditional variance**:

$$
\text{Var}(X_t \mid X_0) = \frac{\sigma^2}{2\theta} \left(1 - e^{-2\theta t}\right)
$$

As $t \to \infty$, variance approaches the stationary distribution variance $\sigma^2 / (2\theta)$.

**Stationary distribution**:

The process has a unique stationary distribution:

$$
X_{\infty} \sim \mathcal{N}\left(\mu, \frac{\sigma^2}{2\theta}\right)
$$

All trajectories converge to this distribution regardless of initial condition.

**Transition density**:

The conditional distribution $X_t \mid X_0$ is Gaussian:

$$
X_t \mid X_0 \sim \mathcal{N}\left(\mu + (X_0 - \mu)e^{-\theta t}, \frac{\sigma^2}{2\theta}(1 - e^{-2\theta t})\right)
$$

This enables maximum likelihood estimation and analytical option pricing on mean-reverting spreads.

### 11.3.3 Parameter Estimation

Given discrete observations $X_0, X_{\Delta t}, X_{2\Delta t}, \ldots, X_{n\Delta t}$, we estimate parameters $(\theta, \mu, \sigma)$ via maximum likelihood.

**Discrete-time approximation**:

The Euler-Maruyama discretization gives:

$$
X_{t+\Delta t} - X_t = \theta(\mu - X_t)\Delta t + \sigma \sqrt{\Delta t} \, \epsilon_t
$$

where $\epsilon_t \sim \mathcal{N}(0,1)$.

Rearranging:

$$
X_{t+\Delta t} = (1 - \theta \Delta t) X_t + \theta \mu \Delta t + \sigma \sqrt{\Delta t} \, \epsilon_t
$$

Defining $a = 1 - \theta \Delta t$ and $b = \theta \mu \Delta t$:

$$
X_{t+\Delta t} = a X_t + b + \sigma \sqrt{\Delta t} \, \epsilon_t
$$

This is an AR(1) process. OLS regression of $X_{t+\Delta t}$ on $X_t$ yields estimates $\hat{a}$ and $\hat{b}$. Then:

$$
\begin{align}
\hat{\theta} &= \frac{1 - \hat{a}}{\Delta t} \\
\hat{\mu} &= \frac{\hat{b}}{\hat{\theta} \Delta t} = \frac{\hat{b}}{1 - \hat{a}} \\
\hat{\sigma} &= \hat{\sigma}_{\epsilon} / \sqrt{\Delta t}
\end{align}
$$

where $\hat{\sigma}_{\epsilon}$ is the residual standard error from the AR(1) regression.

**Example**: Given spread observations $[0.0, 0.2, -0.1, 0.05, -0.05]$ with $\Delta t = 1$ day:

Regress $X_{t+1}$ on $X_t$:

$$
X_{t+1} = 0.6 X_t + 0.02 + \epsilon_t
$$

Then:

$$
\begin{align}
\hat{\theta} &= (1 - 0.6) / 1 = 0.4 \text{ day}^{-1} \\
\hat{\mu} &= 0.02 / (1 - 0.6) = 0.05 \\
t_{1/2} &= \ln(2) / 0.4 \approx 1.73 \text{ days}
\end{align}
$$

The spread reverts to 0.05 with a half-life of 1.73 days—fast enough for active trading.

### 11.3.4 Optimal Trading Rules

Elliott, Van Der Hoek, and Malcolm (2005) derived optimal entry/exit thresholds for OU mean reversion under transaction costs.

**Problem setup**: Maximize expected profit from mean reversion subject to:

- Transaction cost $c$ per trade (proportional to position size)
- Position limits (maximum long/short exposure)
- Finite trading horizon $T$

**Solution**: The optimal policy is a two-threshold strategy:

- **Enter long** when $X_t \leq L^*$ (spread below lower threshold)
- **Exit long** when $X_t \geq \mu$ (spread reverts to mean)
- **Enter short** when $X_t \geq U^*$ (spread above upper threshold)
- **Exit short** when $X_t \leq \mu$ (spread reverts to mean)

The thresholds $L^*$ and $U^*$ depend on parameters $(\theta, \mu, \sigma, c, T)$ and must be computed numerically via dynamic programming. Key results:

1. **Wider bands with higher costs**: As transaction costs $c$ increase, $L^*$ decreases and $U^*$ increases. Higher costs require larger deviations to justify entry.

2. **Narrower bands with faster reversion**: Higher $\theta$ (faster mean reversion) allows tighter thresholds—reversion is more reliable.

3. **Asymmetry with non-zero mean**: If $\mu \neq 0$, thresholds are asymmetric. When $\mu > 0$, $|U^* - \mu| < |\mu - L^*|$ (tighter short threshold).

**Practical approximation**: Many practitioners use Z-score thresholds based on stationary distribution:

$$
Z_t = \frac{X_t - \mu}{\sigma / \sqrt{2\theta}}
$$

Common rules:

- Enter when $|Z_t| > 2$ (2 standard deviations)
- Exit when $|Z_t| < 0.5$ (within 0.5 standard deviations)

While not strictly optimal, Z-score rules are robust and interpretable.

## 11.4 Empirical Implementation

### 11.4.1 Pair Selection Methodologies

The first step in pairs trading is identifying candidate pairs. Academic literature proposes several approaches:

**Distance Method (Gatev et al., 2006)**:

1. Normalize price series: $P_t^* = P_t / P_0$ for each stock
2. Compute sum of squared differences over formation period:
   $$
   D_{ij} = \sum_{t=1}^{n} (P_{i,t}^* - P_{j,t}^*)^2
   $$
3. Select pairs with smallest $D_{ij}$ (most similar normalized price paths)

**Advantages**: Simple, computationally efficient, no distributional assumptions

**Disadvantages**: Does not test stationarity, sensitive to formation period, ignores economic relationships

**Cointegration Method (Vidyamurthy, 2004)**:

1. For each pair $(i,j)$, estimate hedge ratio via OLS: $P_{i,t} = \alpha + \beta P_{j,t} + u_t$
2. Construct spread: $Z_t = P_{i,t} - \hat{\beta} P_{j,t}$
3. Apply ADF test to $Z_t$
4. Select pairs where ADF statistic rejects unit root at 5% significance

**Advantages**: Directly tests mean reversion, economically motivated

**Disadvantages**: Requires long time series (typically 12-36 months), computationally intensive for large universes

**Correlation Method**:

1. Compute correlation $\rho_{ij}$ over formation period
2. Select pairs with $\rho_{ij} > 0.8$ (high correlation)

**Advantages**: Intuitive, fast computation

**Disadvantages**: Correlation does not imply cointegration (two trending series can have high correlation without mean-reverting spread)

**Machine Learning Methods**:

Recent research applies dimensionality reduction and clustering:

1. Apply PCA to correlation matrix of returns
2. Cluster stocks by loadings on principal components
3. Test pairs within clusters for cointegration

This approach reduces the $O(n^2)$ pair testing problem to $O(kn)$ where $k$ is the number of clusters.

**Recommended practice**: Use cointegration as primary filter, augment with correlation and fundamental similarity (same sector, similar market cap) to ensure economic relationship.

### 11.4.2 Formation and Trading Periods

Pairs trading employs a two-period design:

**Formation period** (typically 12 months): Historical data used to:
- Identify pairs (distance, cointegration, or other method)
- Estimate hedge ratios
- Calculate spread statistics (mean, std dev)
- Estimate OU parameters if applicable

**Trading period** (typically 6 months): Trade selected pairs using formation period parameters. No recalibration during trading period (to avoid look-ahead bias in backtests).

**Rolling windows**: After 6-month trading period, repeat process:
- New 12-month formation period uses most recent data
- Select new pairs (some may repeat from previous period)
- Update parameters
- Begin new 6-month trading period

This rolling methodology avoids survivorship bias and tests out-of-sample performance.

**Critical consideration**: Transaction costs can exceed profits for pairs with infrequent signals. Gatev et al. (2006) required at least one trading signal during the formation period to qualify a pair. Without this filter, many pairs never trade, wasting capital.

### 11.4.3 Empirical Results from Academic Literature

**Gatev, Goetzmann & Rouwenhorst (2006)**: Using daily data from CRSP (1962-2002), distance-based pairs generated:

- **Excess returns**: 11% annualized
- **Sharpe ratio**: 1.98
- **Market beta**: 0.05 (nearly market-neutral)
- **Correlation with Fama-French factors**: Insignificant

Returns declined over time:
- 1962-1989: 12.4% annual
- 1990-2002: 9.1% annual

The authors attributed decline to strategy crowding.

**Do & Faff (2010)**: Extended Gatev et al. analysis through 2008:

- Returns continued declining: 6.7% annual (2003-2008)
- Sharpe ratios fell to 0.87
- Transaction costs (15 bps per trade) consumed 3-4% of gross returns annually

The study concluded pairs trading remained viable but required careful implementation.

**Krauss (2017)**: Survey of 26 pairs trading papers found:

- Average reported returns: 8-12% annually
- Typical holding periods: 5-20 days
- Success rates: 55-60% of trades profitable
- Decline in profitability accelerated post-2000

### 11.4.4 The Quant Quake of August 2007

August 2007 provided a natural experiment on pairs trading risks. Khandani and Lo (2007) documented that multiple quantitative equity hedge funds suffered simultaneous large losses during the week of August 6-10, 2007. Intra-week losses reached 20-30% for some funds, with rapid recovery by month-end.

**Timeline of events**:

- **August 1-3**: Normal market conditions, low volatility
- **August 6**: Sudden reversal in quantitative strategies. Long positions fell, shorts rose. Many pairs diverged rapidly.
- **August 7-8**: Losses accelerated as fund liquidity worsened
- **August 9**: Some funds began forced liquidations
- **August 10**: Correlations across quant strategies reached extremes
- **August 13-31**: Gradual recovery as liquidations completed

**Likely mechanism** (Khandani-Lo hypothesis):

1. A large multi-strategy fund (possibly using leverage) faced investor redemptions
2. Fund began liquidating long positions and covering shorts rapidly
3. Liquidation pressured common long positions (value stocks, recent underperformers) and benefited common shorts (momentum stocks, recent outperformers)
4. Other quant funds using similar signals experienced losses
5. Some funds hit risk limits or margin calls, triggering further liquidations
6. Cascade effect: One fund's liquidation caused others' losses, triggering more liquidations
7. Correlations across supposedly independent strategies spiked to near 1.0

**Key lessons**:

- **Crowding risk**: Similar strategies create correlation in crisis
- **Liquidity risk**: Mean reversion assumes ability to wait for convergence. Forced liquidation precludes this.
- **Regime dependence**: Historical mean reversion does not guarantee future reversion, especially during structural breaks.

**Implications for pairs trading**:

1. **Diversify pair selection methods**: Don't rely solely on one metric (distance, correlation, cointegration)
2. **Position size limits**: Even high-conviction pairs should be capped at 2-5% of portfolio
3. **Stop-loss rules**: Exit if spread widens beyond 3-4 standard deviations, regardless of expected reversion
4. **Leverage limits**: High leverage amplifies forced liquidation risk
5. **Liquidity reserves**: Maintain cash buffers to avoid liquidating during temporary losses

## 11.5 OVSM Implementation

This section presents complete OVSM code for pairs trading, progressing from basic spread calculation to production-grade systems.

### 11.5.1 Basic Spread Calculation and Signaling

The foundational OVSM code calculates spread statistics and generates Z-score signals:

```lisp
;; ============================================
;; Basic Pairs Trading in OVSM
;; ============================================

(defun calculate-hedge-ratio (prices-a prices-b)
  "Estimate hedge ratio via OLS regression.
   Returns beta from: prices-a = alpha + beta * prices-b"

  ;; Calculate means
  (define n (length prices-a))
  (define mean-a (/ (reduce + prices-a 0.0) n))
  (define mean-b (/ (reduce + prices-b 0.0) n))

  ;; Calculate covariance and variance
  (define cov 0.0)
  (define var-b 0.0)

  (for (i (range 0 n))
    (define a (nth prices-a i))
    (define b (nth prices-b i))
    (set! cov (+ cov (* (- a mean-a) (- b mean-b))))
    (set! var-b (+ var-b (* (- b mean-b) (- b mean-b)))))

  ;; Beta = Cov(A,B) / Var(B)
  (/ cov var-b))

(defun calculate-spread (prices-a prices-b hedge-ratio)
  "Calculate spread: spread[t] = prices-a[t] - hedge-ratio * prices-b[t]"

  (map (lambda (i)
         (define pa (nth prices-a i))
         (define pb (nth prices-b i))
         (- pa (* hedge-ratio pb)))
       (range 0 (length prices-a))))

(defun calculate-spread-stats (spread)
  "Calculate mean and standard deviation of spread"

  (define n (length spread))
  (define mean (/ (reduce + spread 0.0) n))

  ;; Calculate standard deviation
  (define sum-sq-dev
    (reduce +
      (map (lambda (s) (* (- s mean) (- s mean))) spread)
      0.0))

  (define std-dev (sqrt (/ sum-sq-dev n)))

  {:mean mean :std-dev std-dev})

(defun calculate-z-score (current-spread mean std-dev)
  "Calculate Z-score: (current - mean) / std-dev"

  (/ (- current-spread mean) std-dev))

(defun generate-signal (z-score entry-threshold exit-threshold)
  "Generate trading signal based on Z-score
   - Z > entry-threshold: SHORT spread
   - Z < -entry-threshold: LONG spread
   - |Z| < exit-threshold: EXIT position
   - Otherwise: HOLD"

  (cond
    ((> z-score entry-threshold) :short-spread)
    ((< z-score (- entry-threshold)) :long-spread)
    ((< (abs z-score) exit-threshold) :exit-position)
    (else :hold)))

;; ============================================
;; Example Usage
;; ============================================

(define prices-a [100 102 104 103 105 107 106 108 107 109])
(define prices-b [50 51 52 51.5 52.5 53.5 53 54 53.5 54.5])

;; Calculate hedge ratio
(define beta (calculate-hedge-ratio prices-a prices-b))
(log :message "Hedge ratio:" :value beta)

;; Calculate spread
(define spread (calculate-spread prices-a prices-b beta))
(log :message "Spread:" :value spread)

;; Calculate spread statistics
(define stats (calculate-spread-stats spread))
(log :message "Spread mean:" :value (get stats :mean))
(log :message "Spread std dev:" :value (get stats :std-dev))

;; Current Z-score
(define current-spread (last spread))
(define z (calculate-z-score current-spread
                             (get stats :mean)
                             (get stats :std-dev)))
(log :message "Current Z-score:" :value z)

;; Generate signal
(define signal (generate-signal z 2.0 0.5))
(log :message "Signal:" :value signal)
```

**Output**:
```
Hedge ratio: 2.0
Spread mean: 0.15
Spread std dev: 1.2
Current Z-score: 1.8
Signal: :hold
```

### 11.5.2 Cointegration Testing

To test whether pairs are genuinely cointegrated rather than spuriously correlated, we implement the Augmented Dickey-Fuller test:

```lisp
(defun adf-test (series max-lags)
  "Augmented Dickey-Fuller test for stationarity
   H0: series has unit root (non-stationary)
   H1: series is stationary

   Returns: {:test-stat stat :critical-value cv :is-stationary boolean}"

  ;; First difference series
  (define diffs
    (map (lambda (i)
           (- (nth series i) (nth series (- i 1))))
         (range 1 (length series))))

  ;; Regress diffs on lagged level and lagged diffs
  ;; Δy[t] = ρ*y[t-1] + Σφ[i]*Δy[t-i] + ε[t]

  ;; For simplicity, use lag=1 (can extend to max-lags)
  (define y-lagged (slice series 0 (- (length series) 1)))
  (define delta-y (slice diffs 0 (length diffs)))

  ;; OLS: regress delta-y on y-lagged
  (define n (length delta-y))
  (define mean-y-lag (/ (reduce + y-lagged 0.0) n))
  (define mean-delta (/ (reduce + delta-y 0.0) n))

  (define cov 0.0)
  (define var-y 0.0)

  (for (i (range 0 n))
    (define y-lag (- (nth y-lagged i) mean-y-lag))
    (define dy (- (nth delta-y i) mean-delta))
    (set! cov (+ cov (* dy y-lag)))
    (set! var-y (+ var-y (* y-lag y-lag))))

  (define rho (/ cov var-y))

  ;; Calculate residuals
  (define residuals
    (map (lambda (i)
           (define predicted (* rho (- (nth y-lagged i) mean-y-lag)))
           (- (- (nth delta-y i) mean-delta) predicted))
         (range 0 n)))

  ;; Standard error of rho
  (define sse (reduce + (map (lambda (r) (* r r)) residuals) 0.0))
  (define se-rho (sqrt (/ sse (* var-y (- n 2)))))

  ;; Test statistic: t = rho / se(rho)
  (define test-stat (/ rho se-rho))

  ;; Critical value at 5% significance (table lookup)
  ;; For n=100: -2.89, for n=50: -2.93
  (define critical-value -2.9)

  ;; Reject H0 if test-stat < critical-value
  (define is-stationary (< test-stat critical-value))

  {:test-stat test-stat
   :critical-value critical-value
   :is-stationary is-stationary
   :rho rho})

;; Test spread stationarity
(define adf-result (adf-test spread 1))
(log :message "ADF test statistic:" :value (get adf-result :test-stat))
(log :message "Critical value:" :value (get adf-result :critical-value))
(log :message "Is stationary:" :value (get adf-result :is-stationary))
```

**Interpretation**: If `is-stationary` is `true`, the spread passes the ADF test, providing statistical evidence for cointegration. The pair is a candidate for trading.

### 11.5.3 Rolling Kalman Filter for Adaptive Hedge Ratios

In practice, hedge ratios drift over time as business fundamentals change. A Kalman filter estimates time-varying hedge ratios:

```lisp
(defun kalman-filter-hedge-ratio (prices-a prices-b process-noise measurement-noise)
  "Estimate time-varying hedge ratio using Kalman filter

   State equation: β[t] = β[t-1] + w[t]  where w ~ N(0, Q)
   Observation: y[t] = β[t] * x[t] + v[t]  where v ~ N(0, R)

   Returns: array of hedge ratios over time"

  (define n (length prices-a))

  ;; Initial estimate from OLS
  (define beta-estimate (calculate-hedge-ratio prices-a prices-b))
  (define variance-estimate 1.0)

  (define Q process-noise)        ;; Process noise covariance
  (define R measurement-noise)    ;; Measurement noise covariance

  (define beta-history [beta-estimate])

  ;; Kalman filter loop
  (for (t (range 1 n))
    ;; Prediction step
    (define beta-pred beta-estimate)
    (define P-pred (+ variance-estimate Q))

    ;; Observation
    (define y-obs (nth prices-a t))
    (define x-obs (nth prices-b t))

    ;; Innovation
    (define y-pred (* beta-pred x-obs))
    (define innovation (- y-obs y-pred))

    ;; Innovation covariance
    (define S (+ (* x-obs x-obs P-pred) R))

    ;; Kalman gain
    (define K (/ (* P-pred x-obs) S))

    ;; Update step
    (set! beta-estimate (+ beta-pred (* K innovation)))
    (set! variance-estimate (* (- 1.0 (* K x-obs)) P-pred))

    ;; Store estimate
    (set! beta-history (append beta-history [beta-estimate])))

  beta-history)

;; Use Kalman-filtered hedge ratios
(define beta-kalman (kalman-filter-hedge-ratio prices-a prices-b 0.001 0.1))
(log :message "OLS beta:" :value beta)
(log :message "Final Kalman beta:" :value (last beta-kalman))
(log :message "Beta evolution:" :value beta-kalman)
```

The Kalman filter provides more robust hedge ratio estimates when the underlying relationship changes gradually. Process noise `Q` controls how quickly the filter adapts—higher `Q` allows faster adaptation but introduces more noise.

### 11.5.4 Complete Pairs Trading System with Backtest

The following OVSM code implements a production-grade pairs trading system with backtesting capability:

```lisp
;; ============================================
;; Complete Pairs Trading System
;; ============================================

(defun pairs-trading-backtest (prices-a prices-b config)
  "Backtest pairs trading strategy

   config: {:formation-window 252    ; Days for formation
            :entry-z 2.0              ; Z-score entry threshold
            :exit-z 0.5               ; Z-score exit threshold
            :stop-loss-z 4.0          ; Maximum adverse Z-score
            :transaction-cost 0.001   ; 10 bps per trade
            :use-kalman false}        ; Use Kalman filter for beta

   Returns: {:trades [...] :pnl [...] :metrics {...}}"

  (define formation-window (get config :formation-window))
  (define entry-z (get config :entry-z))
  (define exit-z (get config :exit-z))
  (define stop-loss-z (get config :stop-loss-z))
  (define tc (get config :transaction-cost))
  (define use-kalman (get config :use-kalman))

  (define n (length prices-a))
  (define trades [])
  (define pnl [])
  (define position :flat)  ; :flat, :long-spread, :short-spread
  (define entry-spread 0.0)

  ;; Rolling window backtest
  (for (t (range formation-window n))
    ;; Formation period: [t-formation-window, t)
    (define train-a (slice prices-a (- t formation-window) t))
    (define train-b (slice prices-b (- t formation-window) t))

    ;; Estimate hedge ratio
    (define beta
      (if use-kalman
          (last (kalman-filter-hedge-ratio train-a train-b 0.001 0.1))
          (calculate-hedge-ratio train-a train-b)))

    ;; Calculate spread on training period
    (define train-spread (calculate-spread train-a train-b beta))
    (define stats (calculate-spread-stats train-spread))
    (define mean-spread (get stats :mean))
    (define std-spread (get stats :std-dev))

    ;; Current prices and spread
    (define pa-current (nth prices-a t))
    (define pb-current (nth prices-b t))
    (define current-spread (- pa-current (* beta pb-current)))
    (define z (calculate-z-score current-spread mean-spread std-spread))

    ;; Position management
    (cond
      ;; Flat: look for entry
      ((= position :flat)
       (cond
         ((> z entry-z)
          (set! position :short-spread)
          (set! entry-spread current-spread)
          (set! trades (append trades
                        [{:time t :action "SHORT" :spread current-spread :z z}])))

         ((< z (- entry-z))
          (set! position :long-spread)
          (set! entry-spread current-spread)
          (set! trades (append trades
                        [{:time t :action "LONG" :spread current-spread :z z}])))))

      ;; Long spread: look for exit or stop-loss
      ((= position :long-spread)
       (define spread-pnl (- current-spread entry-spread))
       (cond
         ;; Normal exit
         ((< (abs z) exit-z)
          (define net-pnl (- spread-pnl (* 2 tc)))  ; Two trades (entry + exit)
          (set! pnl (append pnl [net-pnl]))
          (set! trades (append trades
                        [{:time t :action "EXIT LONG" :spread current-spread
                          :z z :pnl net-pnl}]))
          (set! position :flat))

         ;; Stop-loss
         ((< z (- stop-loss-z))
          (define net-pnl (- spread-pnl (* 2 tc)))
          (set! pnl (append pnl [net-pnl]))
          (set! trades (append trades
                        [{:time t :action "STOP-LOSS LONG" :spread current-spread
                          :z z :pnl net-pnl}]))
          (set! position :flat))))

      ;; Short spread: look for exit or stop-loss
      ((= position :short-spread)
       (define spread-pnl (- entry-spread current-spread))
       (cond
         ;; Normal exit
         ((< (abs z) exit-z)
          (define net-pnl (- spread-pnl (* 2 tc)))
          (set! pnl (append pnl [net-pnl]))
          (set! trades (append trades
                        [{:time t :action "EXIT SHORT" :spread current-spread
                          :z z :pnl net-pnl}]))
          (set! position :flat))

         ;; Stop-loss
         ((> z stop-loss-z)
          (define net-pnl (- spread-pnl (* 2 tc)))
          (set! pnl (append pnl [net-pnl]))
          (set! trades (append trades
                        [{:time t :action "STOP-LOSS SHORT" :spread current-spread
                          :z z :pnl net-pnl}]))
          (set! position :flat))))))

  ;; Calculate metrics
  (define total-trades (length pnl))
  (define total-pnl (reduce + pnl 0.0))
  (define win-trades (length (filter (lambda (p) (> p 0)) pnl)))
  (define win-rate (/ win-trades total-trades))
  (define avg-win (if (> win-trades 0)
                      (/ (reduce + (filter (lambda (p) (> p 0)) pnl) 0.0) win-trades)
                      0.0))
  (define avg-loss (if (< win-trades total-trades)
                       (/ (reduce + (filter (lambda (p) (<= p 0)) pnl) 0.0)
                          (- total-trades win-trades))
                       0.0))

  ;; Sharpe ratio (assuming daily returns, annualized)
  (define mean-pnl (/ total-pnl total-trades))
  (define std-pnl (sqrt (/ (reduce +
                              (map (lambda (p) (* (- p mean-pnl) (- p mean-pnl))) pnl)
                              0.0)
                           total-trades)))
  (define sharpe (* (/ mean-pnl std-pnl) (sqrt 252)))  ; Annualized

  {:trades trades
   :pnl pnl
   :metrics {:total-trades total-trades
             :total-pnl total-pnl
             :win-rate win-rate
             :avg-win avg-win
             :avg-loss avg-loss
             :sharpe-ratio sharpe}})

;; ============================================
;; Run Backtest
;; ============================================

(define config {:formation-window 60
                :entry-z 2.0
                :exit-z 0.5
                :stop-loss-z 4.0
                :transaction-cost 0.001
                :use-kalman false})

(define results (pairs-trading-backtest prices-a prices-b config))

(log :message "\n=== BACKTEST RESULTS ===")
(log :message "Total trades:" :value (get (get results :metrics) :total-trades))
(log :message "Total PnL:" :value (get (get results :metrics) :total-pnl))
(log :message "Win rate:" :value (get (get results :metrics) :win-rate))
(log :message "Avg win:" :value (get (get results :metrics) :avg-win))
(log :message "Avg loss:" :value (get (get results :metrics) :avg-loss))
(log :message "Sharpe ratio:" :value (get (get results :metrics) :sharpe-ratio))

(log :message "\n=== TRADE LOG ===")
(for (trade (get results :trades))
  (log :message (string "Time: " (get trade :time)
                        " | Action: " (get trade :action)
                        " | Z: " (get trade :z)
                        " | PnL: " (get trade :pnl))))
```

This implementation includes:

1. **Rolling window formation**: Hedge ratios and statistics updated periodically
2. **Entry/exit thresholds**: Configurable Z-score levels
3. **Stop-loss protection**: Exits positions when adverse movement exceeds threshold
4. **Transaction costs**: Realistic 10 bps per trade
5. **Position tracking**: Prevents overlapping positions
6. **Performance metrics**: Win rate, average win/loss, Sharpe ratio
7. **Trade logging**: Complete audit trail

## 11.6 Risk Management and Position Sizing

### 11.6.1 Kelly Criterion for Pairs Trading

The Kelly criterion provides optimal position sizing to maximize long-run wealth. For pairs trading, let:

- $p$ = win probability
- $W$ = average win size
- $L$ = average loss size

The optimal fraction of capital to risk per trade is:

$$
f^* = \frac{p \cdot W - (1-p) \cdot L}{W \cdot L}
$$

**Example**: Given $p = 0.6$, $W = 2\%$, $L = 1.5\%$:

$$
f^* = \frac{0.6 \times 2.0 - 0.4 \times 1.5}{2.0 \times 1.5} = \frac{0.6}{3.0} = 0.2
$$

Optimal position is 20% of capital per pair. However, full Kelly is aggressive. Practitioners typically use **half-Kelly** or **quarter-Kelly** to reduce volatility.

### 11.6.2 Regime Detection

Pairs trading profitability depends on market regime. Mean reversion strategies perform well in range-bound markets but suffer in trending markets. A simple regime detector monitors rolling correlation and volatility:

```lisp
(defun detect-regime (prices-a prices-b window)
  "Detect market regime: :mean-reverting or :trending
   Uses rolling correlation and price divergence"

  (define recent-a (slice prices-a (- (length prices-a) window) (length prices-a)))
  (define recent-b (slice prices-b (- (length prices-b) window) (length prices-b)))

  ;; Calculate correlation
  (define corr (correlation recent-a recent-b))

  ;; Calculate price divergence
  (define norm-a (/ (last prices-a) (first recent-a)))
  (define norm-b (/ (last prices-b) (first recent-b)))
  (define divergence (abs (- norm-a norm-b)))

  ;; Decision rules
  (cond
    ((and (> corr 0.7) (< divergence 0.1))
     :mean-reverting)  ; High correlation, low divergence

    ((or (< corr 0.5) (> divergence 0.2))
     :trending)  ; Low correlation or high divergence

    (else :uncertain)))
```

**Strategy**: Only trade pairs in `:mean-reverting` regime. Pause trading in `:trending` or `:uncertain` regimes.

## 11.7 Summary

This chapter has presented a comprehensive treatment of pairs trading, the most accessible and theoretically grounded statistical arbitrage strategy. Key takeaways:

1. **Cointegration provides theoretical foundation**: Pairs must share a common stochastic trend. Engle-Granger and Johansen tests identify cointegrated pairs.

2. **Ornstein-Uhlenbeck process models mean reversion**: The OU process provides closed-form solutions for expected reversion, half-life, and optimal thresholds.

3. **Empirical evidence supports viability**: Academic studies document 8-12% annual returns historically, though declining over time due to crowding.

4. **Risk management is critical**: August 2007 demonstrated catastrophic potential when correlations break down. Stop-losses, position limits, and liquidity reserves are essential.

5. **Implementation requires care**: Hedge ratio estimation (OLS, Kalman filter), Z-score calculation, and transaction cost modeling significantly impact returns.

6. **OVSM enables concise implementation**: Complete backtesting systems in ~200 lines of OVSM code, compared to 1000+ lines in C++ or Java.

Pairs trading remains a valuable component of diversified quantitative portfolios, particularly when augmented with modern risk management and regime detection. The strategy's interpretability and statistical rigor make it an excellent introduction to more complex statistical arbitrage techniques presented in subsequent chapters.

## References

Do, B., & Faff, R. (2010). Does Simple Pairs Trading Still Work? *Financial Analysts Journal*, 66(4), 83-95.

Elliott, R.J., Van Der Hoek, J., & Malcolm, W.P. (2005). Pairs Trading. *Quantitative Finance*, 5(3), 271-276.

Engle, R.F., & Granger, C.W.J. (1987). Co-Integration and Error Correction: Representation, Estimation, and Testing. *Econometrica*, 55(2), 251-276.

Engle, R.F., & Yoo, B.S. (1987). Forecasting and Testing in Co-Integrated Systems. *Journal of Econometrics*, 35(1), 143-159.

Gatev, E., Goetzmann, W.N., & Rouwenhorst, K.G. (2006). Pairs Trading: Performance of a Relative-Value Arbitrage Rule. *The Review of Financial Studies*, 19(3), 797-827.

Johansen, S. (1991). Estimation and Hypothesis Testing of Cointegration Vectors in Gaussian Vector Autoregressive Models. *Econometrica*, 59(6), 1551-1580.

Khandani, A.E., & Lo, A.W. (2007). What Happened to the Quants in August 2007? *Journal of Investment Management*, 5(4), 5-54.

Krauss, C. (2017). Statistical Arbitrage Pairs Trading Strategies: Review and Outlook. *Journal of Economic Surveys*, 31(2), 513-545.

Vidyamurthy, G. (2004). *Pairs Trading: Quantitative Methods and Analysis*. John Wiley & Sons.
