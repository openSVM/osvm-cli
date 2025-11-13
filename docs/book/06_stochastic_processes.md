# Chapter 6: Stochastic Processes and Simulation

## Introduction: The Random Walk Home

Imagine you leave a bar at midnight, thoroughly intoxicated. Each step you take is random—sometimes forward, sometimes backward, sometimes to the left or right. Where will you be in 100 steps? In 1000 steps? This "drunkard's walk" is the foundation of stochastic processes, the mathematical machinery that powers modern quantitative finance.

The drunk walk isn't just a colorful analogy—it's precisely how particles move in liquid (Brownian motion, discovered by Einstein in 1905), and remarkably, it's the best model we have for stock prices over short time intervals. This chapter reveals why randomness, properly modeled, is more powerful than prediction.

Financial markets exhibit behaviors that deterministic models cannot capture:

- **Sudden jumps**: Tesla announces earnings—the stock gaps 15% overnight
- **Volatility clustering**: Calm markets stay calm; chaotic markets stay chaotic
- **Mean reversion**: Commodity spreads drift back to historical norms
- **Fat tails**: Market crashes happen far more often than normal distributions predict

We'll build mathematical models for each phenomenon, implement them in OVSM, and demonstrate how to use them for pricing derivatives, managing risk, and designing trading strategies.

**What you'll learn:**

1. **Brownian Motion**: The foundation—continuous random paths that model everything from stock prices to interest rates
2. **Jump-Diffusion**: Adding discontinuous shocks to capture crashes and news events
3. **GARCH Models**: Time-varying volatility—why market turbulence clusters
4. **Ornstein-Uhlenbeck Processes**: Mean reversion—the mathematics of pairs trading
5. **Monte Carlo Simulation**: Using randomness to price complex derivatives

**Pedagogical approach:** We start with intuition (the drunk walk), formalize it mathematically, then implement working code. Every equation is explained in plain language. Every code block includes inline comments describing what each line does and why.

---

## 6.1 Brownian Motion: The Foundation

### 6.1.1 From the Drunk Walk to Brownian Motion

Let's formalize our drunk walk. At each time step, you take a random step:

- Step forward (+1) with probability 50%
- Step backward (-1) with probability 50%

After $n$ steps, your position is the sum of $n$ random steps. The Central Limit Theorem tells us something remarkable: as $n$ grows large, your position approaches a **normal distribution** with mean 0 and variance $n$.

This is the essence of Brownian motion: accumulate random shocks, and you get a random walk whose variance grows linearly with time.

**Mathematical Definition:**

Standard Brownian Motion $W_t$ (also called a Wiener process) satisfies four properties:

1. **Starts at zero**: $W_0 = 0$
2. **Independent increments**: The change from time $s$ to $t$ is independent of the change from time $u$ to $v$ if the intervals don't overlap
3. **Normal increments**: $W_t - W_s \sim \mathcal{N}(0, t-s)$—the change over any interval is normally distributed with variance equal to the length of the interval
4. **Continuous paths**: $W_t$ is continuous (no jumps), though not differentiable

**Key properties:**

- **Expected value**: $\mathbb{E}[W_t] = 0$—on average, you end up where you started
- **Variance**: $\text{Var}(W_t) = t$—uncertainty grows with the square root of time
- **Standard deviation**: $\sigma(W_t) = \sqrt{t}$—this $\sqrt{t}$ scaling is fundamental to option pricing

**Intuition:** If you take twice as many steps, you don't go twice as far on average—you go $\sqrt{2}$ times as far. Randomness compounds with the square root of time, not linearly.

### 6.1.2 Simulating Brownian Motion

To simulate Brownian motion on a computer, we discretize time into small steps $\Delta t$:

$$W_{t+\Delta t} = W_t + \sqrt{\Delta t} \cdot Z$$

where $Z \sim \mathcal{N}(0,1)$ is a standard normal random variable.

**Why $\sqrt{\Delta t}$?** Because variance must scale linearly with time. If each step has variance $\Delta t$, then the standard deviation is $\sqrt{\Delta t}$.

```lisp
;; Simulate standard Brownian motion
;; Parameters:
;;   n-steps: number of time steps to simulate
;;   dt: time increment (e.g., 0.01 = 1% of a year if annual time unit)
;; Returns: array of positions [W_0, W_1, ..., W_n] where W_0 = 0
(define (brownian-motion n-steps dt)
  (let ((path [0])           ;; Start at W_0 = 0
        (current-position 0)) ;; Track current position

    ;; Generate n-steps of random increments
    (for (i (range 0 n-steps))
      ;; Generate dW = sqrt(dt) * Z, where Z ~ N(0,1)
      (let ((dW (* (sqrt dt) (standard-normal))))

        ;; Update position: W_{t+dt} = W_t + dW
        (set! current-position (+ current-position dW))

        ;; Append to path
        (set! path (append path current-position))))

    path))

;; Generate standard normal random variable using Box-Muller transform
;; This converts two uniform [0,1] random numbers into a standard normal
;; Formula: Z = sqrt(-2 log(U1)) * cos(2π U2)
(define (standard-normal)
  (let ((u1 (random))  ;; Uniform random number in [0,1]
        (u2 (random)))
    (* (sqrt (* -2 (log u1)))           ;; sqrt(-2 log(U1))
       (cos (* 2 3.14159 u2)))))        ;; cos(2π U2)

;; Example: Simulate 1000 steps with dt = 0.01 (1% time increments)
(define bm-path (brownian-motion 1000 0.01))

;; At the end (t = 1000 * 0.01 = 10), we expect:
;; - Mean position: ~0 (may vary due to randomness)
;; - Standard deviation: sqrt(10) ≈ 3.16
```

**Interpretation:**

- Each step adds a small random shock scaled by $\sqrt{\Delta t}$
- After many steps, the path wanders randomly—sometimes positive, sometimes negative
- The path is **continuous** (no jumps) but very **jagged** (not smooth or differentiable)
- Smaller $\Delta t$ gives a more accurate approximation to true Brownian motion

**Key Insight:** This simulation is the foundation of Monte Carlo methods in finance. To price a derivative, we simulate thousands of Brownian paths, calculate the payoff on each path, and average the results.

### 6.1.3 Geometric Brownian Motion: Modeling Stock Prices

**Problem:** Stock prices can't go negative, but standard Brownian motion can. Solution: Model the **logarithm** of the stock price as Brownian motion.

**Geometric Brownian Motion (GBM)** is the most famous model in quantitative finance, used by Black-Scholes-Merton for option pricing:

$$dS_t = \mu S_t dt + \sigma S_t dW_t$$

This is a **stochastic differential equation (SDE)**. Read it as:

- $dS_t$: infinitesimal change in stock price
- $\mu S_t dt$: drift term—deterministic trend (e.g., $\mu = 0.10$ = 10% annual growth)
- $\sigma S_t dW_t$: diffusion term—random fluctuations (e.g., $\sigma = 0.30$ = 30% annual volatility)

**Solution (via Itô's Lemma):**

$$S_t = S_0 \exp\left(\left(\mu - \frac{\sigma^2}{2}\right)t + \sigma W_t\right)$$

**Why the $-\frac{\sigma^2}{2}$ term?** This is the **Itô correction**, arising from the quadratic variation of Brownian motion. Without it, the expected value would be wrong. The intuition: volatility drag—high volatility reduces geometric average returns.

**Discrete Simulation:**

$$S_{t+\Delta t} = S_t \exp\left(\left(\mu - \frac{\sigma^2}{2}\right)\Delta t + \sigma \sqrt{\Delta t} Z\right)$$

```lisp
;; Simulate Geometric Brownian Motion (stock price process)
;; Parameters:
;;   initial-price: starting stock price (e.g., 100 for $100)
;;   mu: annual drift/expected return (e.g., 0.10 = 10% per year)
;;   sigma: annual volatility (e.g., 0.50 = 50% per year)
;;   n-steps: number of time steps
;;   dt: time increment in years (e.g., 1/252 = 1 trading day)
;; Returns: array of prices [S_0, S_1, ..., S_n]
(define (gbm initial-price mu sigma n-steps dt)
  (let ((prices [initial-price])
        (current-price initial-price))

    (for (i (range 0 n-steps))
      ;; Generate random shock: dW = sqrt(dt) * Z
      (let ((dW (* (sqrt dt) (standard-normal))))

        ;; Calculate drift component: (μ - σ²/2) * dt
        (let ((drift-term (* (- mu (* 0.5 sigma sigma)) dt))

              ;; Calculate diffusion component: σ * dW
              (diffusion-term (* sigma dW)))

          ;; Update price: S_{t+dt} = S_t * exp(drift + diffusion)
          ;; We multiply (not add) because returns are multiplicative
          (set! current-price
                (* current-price
                   (exp (+ drift-term diffusion-term))))

          (set! prices (append prices current-price)))))

    prices))

;; Example: Simulate SOL price for 1 year (252 trading days)
;; Starting price: $100
;; Expected annual return: 15%
;; Annual volatility: 50% (typical for crypto)
(define sol-simulation
  (gbm 100.0        ;; S_0 = $100
       0.15         ;; μ = 15% annual drift
       0.50         ;; σ = 50% annual volatility
       252          ;; 252 trading days
       (/ 1 252)))  ;; dt = 1 day = 1/252 years

;; Extract final price after 1 year
(define final-price (last sol-simulation))

;; Expected final price: S_0 * exp(μ * T) = 100 * exp(0.15 * 1) ≈ $116.18
;; Actual final price will vary due to randomness!
;; Standard deviation: S_0 * exp(μ*T) * sqrt(exp(σ²*T) - 1) ≈ $64
```

**Statistical Properties of GBM:**

| Property | Formula | Interpretation |
|----------|---------|----------------|
| Expected price | $S_0 e^{\mu t}$ | Exponential growth at rate $\mu$ |
| Price variance | $S_0^2 e^{2\mu t}(e^{\sigma^2 t} - 1)$ | Grows explosively with volatility |
| Log-return mean | $\mu - \frac{\sigma^2}{2}$ | Drift adjusted for volatility drag |
| Log-return std dev | $\sigma \sqrt{t}$ | Scales with $\sqrt{t}$ (square root of time) |

**Why this matters:**

- **Option pricing**: Black-Scholes assumes GBM—understanding it is essential
- **Risk management**: Variance grows with time—longer horizons are riskier
- **Strategy design**: Volatility drag means high-volatility assets underperform their drift

### 6.1.4 Multi-Asset GBM with Correlation

Real portfolios hold multiple assets. How do we simulate correlated assets (e.g., BTC and ETH)?

**Key idea:** Generate **correlated** Brownian motions, not independent ones.

**Method: Cholesky Decomposition**

Given a correlation matrix $\rho$, find a matrix $L$ such that $L L^T = \rho$. Then:

$$W_{\text{correlated}} = L \cdot Z_{\text{independent}}$$

transforms independent normal random variables $Z$ into correlated ones.

**2-asset example:**

Correlation matrix for two assets with correlation $\rho$:

$$\rho = \begin{bmatrix} 1 & \rho \\ \rho & 1 \end{bmatrix}$$

Cholesky decomposition:

$$L = \begin{bmatrix} 1 & 0 \\ \rho & \sqrt{1-\rho^2} \end{bmatrix}$$

```lisp
;; Cholesky decomposition for 2x2 correlation matrix
;; Input: rho (correlation coefficient between -1 and 1)
;; Output: 2x2 lower triangular matrix L such that L*L^T = [[1,rho],[rho,1]]
(define (cholesky-2x2 rho)
  ;; For correlation matrix [[1, rho], [rho, 1]]:
  ;; L = [[1, 0], [rho, sqrt(1-rho^2)]]
  [[1 0]
   [rho (sqrt (- 1 (* rho rho)))]])

;; Simulate 2 correlated Geometric Brownian Motions
;; Parameters:
;;   S0-1, S0-2: initial prices for assets 1 and 2
;;   mu1, mu2: annual drifts
;;   sigma1, sigma2: annual volatilities
;;   rho: correlation coefficient (-1 to 1)
;;   n-steps, dt: time discretization
(define (correlated-gbm-2 S0-1 S0-2 mu1 mu2 sigma1 sigma2 rho n-steps dt)
  (let ((L (cholesky-2x2 rho))           ;; Cholesky decomposition
        (prices-1 [S0-1])                ;; Price path for asset 1
        (prices-2 [S0-2])                ;; Price path for asset 2
        (current-1 S0-1)
        (current-2 S0-2))

    (for (i (range 0 n-steps))
      ;; Generate independent standard normals
      (let ((Z1 (standard-normal))
            (Z2 (standard-normal)))

        ;; Apply Cholesky: [W1, W2] = L * [Z1, Z2]
        ;; W1 = L[0,0]*Z1 + L[0,1]*Z2 = 1*Z1 + 0*Z2 = Z1
        ;; W2 = L[1,0]*Z1 + L[1,1]*Z2 = rho*Z1 + sqrt(1-rho^2)*Z2
        (let ((W1 (+ (* (nth (nth L 0) 0) Z1)
                    (* (nth (nth L 0) 1) Z2)))
              (W2 (+ (* (nth (nth L 1) 0) Z1)
                    (* (nth (nth L 1) 1) Z2))))

          ;; Now W1 and W2 are correlated normals with Corr(W1,W2) = rho

          ;; Calculate drift terms (adjusted for volatility drag)
          (let ((drift1 (- mu1 (* 0.5 sigma1 sigma1)))
                (drift2 (- mu2 (* 0.5 sigma2 sigma2))))

            ;; Update prices using correlated Brownian increments
            (set! current-1
                  (* current-1
                     (exp (+ (* drift1 dt)
                            (* sigma1 (sqrt dt) W1)))))

            (set! current-2
                  (* current-2
                     (exp (+ (* drift2 dt)
                            (* sigma2 (sqrt dt) W2)))))

            (set! prices-1 (append prices-1 current-1))
            (set! prices-2 (append prices-2 current-2))))))

    {:asset-1 prices-1 :asset-2 prices-2}))

;; Example: SOL and BTC with 70% correlation
;; SOL: $100, 15% drift, 50% vol
;; BTC: $50000, 12% drift, 40% vol
;; Correlation: 0.70 (typical for crypto assets)
(define corr-sim
  (correlated-gbm-2
    100.0 50000.0  ;; Initial prices
    0.15 0.12      ;; Drifts (annual)
    0.50 0.40      ;; Volatilities (annual)
    0.70           ;; Correlation
    252            ;; 252 trading days
    (/ 1 252)))    ;; dt = 1 day

;; Extract final prices
(define sol-final (last (corr-sim :asset-1)))
(define btc-final (last (corr-sim :asset-2)))
```

**Verifying correlation:**

To check that our simulation produces the correct correlation, calculate the correlation of log-returns:

```lisp
;; Calculate log-returns from price series
(define (log-returns prices)
  (let ((returns []))
    (for (i (range 1 (length prices)))
      (let ((r (log (/ (nth prices i) (nth prices (- i 1))))))
        (set! returns (append returns r))))
    returns))

;; Calculate correlation between two return series
(define (correlation returns-1 returns-2)
  (let ((n (length returns-1))
        (mean-1 (average returns-1))
        (mean-2 (average returns-2)))

    (let ((cov (/ (sum (map (range 0 n)
                           (lambda (i)
                             (* (- (nth returns-1 i) mean-1)
                                (- (nth returns-2 i) mean-2)))))
                  n))
          (std-1 (std-dev returns-1))
          (std-2 (std-dev returns-2)))

      (/ cov (* std-1 std-2)))))

;; Verify correlation of simulated returns
(define (verify-correlation sim)
  (let ((returns-1 (log-returns (sim :asset-1)))
        (returns-2 (log-returns (sim :asset-2))))

    (correlation returns-1 returns-2)))

;; Should return value close to 0.70
(define observed-corr (verify-correlation corr-sim))
;; Might be 0.68 or 0.72 due to sampling variation—close to 0.70 on average
```

**Why correlation matters:**

- **Diversification**: Uncorrelated assets reduce portfolio risk
- **Pairs trading**: Requires finding cointegrated (correlated) assets
- **Risk management**: Correlation breaks down in crises—all assets crash together

---

## 6.2 Jump-Diffusion Processes: Modeling Crashes

### 6.2.1 The Problem with Pure Brownian Motion

GBM assumes continuous price evolution. But real markets exhibit **discontinuous jumps**:

- **Earnings announcements**: Stock gaps 15% overnight
- **Black swan events**: COVID-19 triggers 30% crashes in days
- **Liquidation cascades**: Crypto flash crashes (e.g., May 19, 2021)

Brownian motion can't produce these jumps—even with high volatility, large moves are extremely rare under normal distributions.

**Solution: Jump-Diffusion Models**

Combine continuous diffusion (Brownian motion) with discrete jumps (Poisson process).

### 6.2.2 Merton Jump-Diffusion Model

**Robert Merton (1976)** extended GBM to include random jumps:

$$dS_t = \mu S_t dt + \sigma S_t dW_t + S_t dJ_t$$

where:
- $\mu S_t dt + \sigma S_t dW_t$ = continuous diffusion (normal GBM)
- $S_t dJ_t$ = jump component

**Jump process $J_t$:**

- Jumps arrive according to a **Poisson process** with intensity $\lambda$ (average jumps per unit time)
- Jump sizes $Y_i$ are random, typically $\log(1 + Y_i) \sim \mathcal{N}(\mu_J, \sigma_J^2)$

**Intuition:**

- Most of the time ($1 - \lambda dt$ probability), no jump occurs—price evolves via GBM
- Occasionally ($\lambda dt$ probability), a jump occurs—price multiplies by $(1 + Y_i)$

**Example parameters:**
- $\lambda = 2$: 2 jumps per year on average
- $\mu_J = -0.05$: jumps are 5% down on average (crashes more common than rallies)
- $\sigma_J = 0.10$: jump sizes vary with 10% standard deviation

```lisp
;; Merton jump-diffusion simulation
;; Parameters:
;;   S0: initial price
;;   mu: continuous drift (annual)
;;   sigma: continuous volatility (annual)
;;   lambda: jump intensity (average jumps per year)
;;   mu-jump: mean log-jump size (e.g., -0.05 = 5% down on average)
;;   sigma-jump: standard deviation of log-jump sizes
;;   n-steps, dt: time discretization
(define (merton-jump-diffusion S0 mu sigma lambda mu-jump sigma-jump n-steps dt)
  (let ((prices [S0])
        (current-price S0))

    (for (i (range 0 n-steps))
      ;; === Continuous diffusion component (GBM) ===
      (let ((dW (* (sqrt dt) (standard-normal)))
            (drift-component (* mu dt))
            (diffusion-component (* sigma dW)))

        ;; === Jump component ===
        ;; Number of jumps in interval dt follows Poisson distribution
        (let ((n-jumps (poisson-random (* lambda dt))))

          (let ((total-jump-multiplier 1))  ;; Cumulative effect of all jumps

            ;; For each jump that occurs, generate jump size and apply it
            (for (j (range 0 n-jumps))
              (let ((log-jump-size (+ mu-jump
                                     (* sigma-jump (standard-normal)))))
                ;; Convert log-jump to multiplicative jump: exp(log-jump)
                (set! total-jump-multiplier
                      (* total-jump-multiplier (exp log-jump-size)))))

            ;; Update price:
            ;; 1. Apply diffusion: S * exp((μ - σ²/2)dt + σ dW)
            ;; 2. Apply jumps: multiply by jump factor
            (set! current-price
                  (* current-price
                     (exp (+ (- drift-component (* 0.5 sigma sigma dt))
                            diffusion-component))
                     total-jump-multiplier))

            (set! prices (append prices current-price))))))

    prices))

;; Generate Poisson random variable (number of jumps in interval dt)
;; Uses Knuth's algorithm: generate exponential inter-arrival times
(define (poisson-random lambda)
  (let ((L (exp (- lambda)))  ;; Threshold
        (k 0)                 ;; Counter
        (p 1))                ;; Cumulative probability

    ;; Generate random arrivals until cumulative probability drops below L
    (while (> p L)
      (set! k (+ k 1))
      (set! p (* p (random))))  ;; Multiply by uniform random

    (- k 1)))  ;; Return number of arrivals

;; Example: SOL with crash risk
;; Normal volatility: 30% (lower than pure GBM since jumps capture extreme moves)
;; Jump intensity: 2 per year (1 jump every 6 months on average)
;; Jump size: -5% mean, 10% std dev (mostly downward jumps)
(define jump-sim
  (merton-jump-diffusion
    100.0      ;; S_0 = $100
    0.15       ;; μ = 15% drift
    0.30       ;; σ = 30% continuous volatility
    2.0        ;; λ = 2 jumps per year
    -0.05      ;; μ_J = -5% mean jump (crashes)
    0.10       ;; σ_J = 10% jump volatility
    252        ;; 252 days
    (/ 1 252))) ;; dt = 1 day
```

**Detecting jumps in simulated data:**

```lisp
;; Identify jumps in a price path
;; Jumps are defined as returns exceeding a threshold (e.g., 3 standard deviations)
(define (detect-jumps prices threshold)
  (let ((returns (log-returns prices))
        (jumps []))

    (for (i (range 0 (length returns)))
      (let ((r (nth returns i)))
        ;; If absolute return exceeds threshold, classify as jump
        (if (> (abs r) threshold)
            (set! jumps (append jumps {:index i
                                        :return r
                                        :price-before (nth prices i)
                                        :price-after (nth prices (+ i 1))}))
            null)))

    jumps))

;; Find jumps larger than 3 standard deviations
(define returns (log-returns jump-sim))
(define return-std (std-dev returns))
(define detected-jumps (detect-jumps jump-sim (* 3 return-std)))

;; Expected: ~2 jumps detected (since lambda=2, we expect 2 jumps per year)
```

### 6.2.3 Kou Double-Exponential Jump-Diffusion

**Problem with Merton:** Assumes symmetric jump distribution (normal). Real data shows **asymmetry**:

- Up-jumps are small and frequent (good news trickles in)
- Down-jumps are large and rare (crashes are sudden)

**Steven Kou (2002)** proposed a double-exponential jump model:

$$P(\text{Jump size} > x) = \begin{cases}
p \eta_1 e^{-\eta_1 x} & x > 0 \text{ (up-jump)} \\
(1-p) \eta_2 e^{\eta_2 x} & x < 0 \text{ (down-jump)}
\end{cases}$$

where:
- $p$ = probability of up-jump
- $\eta_1$ = decay rate of up-jumps (large $\eta_1$ → small jumps)
- $\eta_2$ = decay rate of down-jumps (small $\eta_2$ → large jumps)

**Typical equity parameters:**
- $p = 0.4$ (40% up-jumps, 60% down-jumps)
- $\eta_1 = 50$ (up-jumps average $1/50 = 2\%$)
- $\eta_2 = 10$ (down-jumps average $1/10 = 10\%$)

```lisp
;; Generate double-exponential jump size
;; Parameters:
;;   p: probability of up-jump
;;   eta1: decay rate for up-jumps (larger = smaller average jump)
;;   eta2: decay rate for down-jumps
(define (double-exponential-jump p eta1 eta2)
  (if (< (random) p)
      ;; Up-jump: exponential distribution with rate eta1
      ;; Formula: -log(U) / eta1, where U ~ Uniform(0,1)
      (/ (- (log (random))) eta1)

      ;; Down-jump: negative exponential with rate eta2
      (- (/ (- (log (random))) eta2))))

;; Kou jump-diffusion simulation
(define (kou-jump-diffusion S0 mu sigma lambda p eta1 eta2 n-steps dt)
  (let ((prices [S0])
        (current-price S0))

    (for (i (range 0 n-steps))
      ;; Continuous diffusion (standard GBM)
      (let ((dW (* (sqrt dt) (standard-normal)))
            (drift (* mu dt))
            (diffusion (* sigma dW)))

        ;; Jump component with double-exponential jumps
        (let ((n-jumps (poisson-random (* lambda dt))))

          (let ((total-jump-pct 0))  ;; Sum of all log-jumps
            (for (j (range 0 n-jumps))
              (set! total-jump-pct
                    (+ total-jump-pct (double-exponential-jump p eta1 eta2))))

            ;; Price update: diffusion + jumps
            (set! current-price
                  (* current-price
                     (exp (+ (- drift (* 0.5 sigma sigma dt))
                            diffusion
                            total-jump-pct))))

            (set! prices (append prices current-price))))))

    prices))

;; Example: Asymmetric jumps (small up, large down) - realistic for equities
(define kou-sim
  (kou-jump-diffusion
    100.0      ;; S_0 = $100
    0.10       ;; μ = 10% annual drift
    0.25       ;; σ = 25% continuous volatility
    3.0        ;; λ = 3 jumps per year
    0.4        ;; p = 40% chance of up-jump
    50.0       ;; η_1 = 50 (up-jumps avg 1/50 = 2%)
    10.0       ;; η_2 = 10 (down-jumps avg 1/10 = 10%)
    252
    (/ 1 252)))
```

**Jump Statistics Comparison:**

| Model | Up-Jump Prob | Avg Up-Jump | Avg Down-Jump | Use Case |
|-------|--------------|-------------|---------------|----------|
| Merton (symmetric) | 50% | $\mu_J$ | $\mu_J$ | Commodities, FX |
| Kou (equity-like) | 40% | +2% | -10% | Stock indices |
| Kou (crypto-like) | 45% | +5% | -15% | High-volatility assets |

**Why this matters:**

- **Option pricing**: Asymmetric jumps create volatility skew—out-of-the-money puts trade at higher implied volatility
- **Risk management**: Tail risk is underestimated if you assume normal jumps
- **Strategy design**: Mean-reversion strategies fail during jump events—need jump filters

---

## 6.3 GARCH Models: Volatility Clustering

### 6.3.1 The Volatility Puzzle

Look at any stock chart, and you'll notice a pattern: **volatility clusters**.

- Calm periods stay calm (low volatility persists)
- Turbulent periods stay turbulent (high volatility persists)

"*Large changes tend to be followed by large changes—of either sign—and small changes tend to be followed by small changes.*" – Benoit Mandelbrot

**Example:** During 2020:
- January–February: S&P 500 daily volatility ~12% (annualized)
- March (COVID crash): Daily volatility spiked to ~80%
- April–May: Volatility remained elevated at ~40%
- Later 2020: Gradually declined back to ~20%

This clustering violates the constant-volatility assumption of GBM. We need **time-varying volatility**.

### 6.3.2 GARCH(1,1) Model

**GARCH** = Generalized AutoRegressive Conditional Heteroskedasticity (don't memorize that—just know it models time-varying volatility)

**The model:**

Returns have conditional volatility:

$$r_t = \mu + \sigma_t \epsilon_t, \quad \epsilon_t \sim \mathcal{N}(0,1)$$

Volatility evolves according to:

$$\sigma_t^2 = \omega + \alpha r_{t-1}^2 + \beta \sigma_{t-1}^2$$

**Interpretation:**

- $\omega$ = baseline variance (long-run average)
- $\alpha r_{t-1}^2$ = yesterday's return shock increases today's volatility
- $\beta \sigma_{t-1}^2$ = yesterday's volatility persists into today (autocorrelation)

**Intuition:** If yesterday had a large return (up or down), today's volatility increases. If yesterday's volatility was high, today's volatility stays high.

**Stationarity condition:** $\alpha + \beta < 1$ (otherwise variance explodes to infinity)

**Typical equity parameters:**
- $\omega \approx 0.000005$ (very small baseline)
- $\alpha \approx 0.08$ (8% weight on yesterday's shock)
- $\beta \approx 0.90$ (90% weight on yesterday's volatility)
- $\alpha + \beta = 0.98$ (high persistence—shocks decay slowly)

```lisp
;; GARCH(1,1) simulation
;; Parameters:
;;   n-steps: number of periods to simulate
;;   mu: mean return per period
;;   omega: baseline variance
;;   alpha: weight on lagged squared return
;;   beta: weight on lagged variance
;;   initial-sigma: starting volatility (standard deviation, not variance)
;; Returns: {:returns [...], :volatilities [...]}
(define (garch-11 n-steps mu omega alpha beta initial-sigma)
  (let ((returns [])
        (volatilities [initial-sigma])
        (current-sigma initial-sigma))

    (for (i (range 0 n-steps))
      ;; Generate standardized shock: epsilon ~ N(0,1)
      (let ((epsilon (standard-normal)))

        ;; Generate return: r_t = mu + sigma_t * epsilon_t
        (let ((return (+ mu (* current-sigma epsilon))))

          (set! returns (append returns return))

          ;; Update volatility for next period
          ;; sigma_t^2 = omega + alpha * r_{t-1}^2 + beta * sigma_{t-1}^2
          (let ((prev-return (if (> i 0)
                                (nth returns (- i 1))
                                0)))  ;; Use 0 for first period

            (let ((sigma-squared (+ omega
                                   (* alpha prev-return prev-return)
                                   (* beta current-sigma current-sigma))))

              ;; Convert variance to standard deviation
              (set! current-sigma (sqrt sigma-squared))
              (set! volatilities (append volatilities current-sigma)))))))

    {:returns returns :volatilities volatilities}))

;; Example: Simulate 1000 days of equity returns with GARCH volatility
(define garch-sim
  (garch-11
    1000       ;; 1000 days
    0.0005     ;; μ = 0.05% daily mean return (≈13% annualized)
    0.000005   ;; ω = baseline variance
    0.08       ;; α = shock weight
    0.90       ;; β = persistence
    0.015))    ;; Initial σ = 1.5% daily (≈24% annualized)

;; Extract results
(define returns (garch-sim :returns))
(define vols (garch-sim :volatilities))
```

**Volatility Persistence:**

The **half-life** of a volatility shock is:

$$\text{Half-life} = \frac{\log 2}{\log(1/(\alpha + \beta))}$$

```lisp
;; Calculate half-life of volatility shocks
;; This tells us how many periods it takes for a shock to decay by 50%
(define (volatility-half-life alpha beta)
  (let ((persistence (+ alpha beta)))
    (/ (log 2) (log (/ 1 persistence)))))

;; Example: alpha=0.08, beta=0.90 → persistence=0.98
(define half-life (volatility-half-life 0.08 0.90))
;; → ≈34 periods
;; Interpretation: A volatility shock takes 34 days to decay by half
;; In other words, shocks persist for over a month!
```

**Verify volatility clustering:**

```lisp
;; Autocorrelation of squared returns (indicates volatility clustering)
;; High autocorrelation in r_t^2 suggests volatility clustering
(define (autocorr-squared-returns returns lag)
  (let ((squared-returns (map returns (lambda (r) (* r r)))))
    (let ((n (length squared-returns))
          (mean-sq (average squared-returns)))

      (let ((cov (/ (sum (map (range 0 (- n lag))
                             (lambda (i)
                               (* (- (nth squared-returns i) mean-sq)
                                  (- (nth squared-returns (+ i lag)) mean-sq)))))
                   (- n lag)))
            (var (variance squared-returns)))

        (/ cov var)))))

;; Check lag-1 autocorrelation of squared returns
(define lag1-autocorr (autocorr-squared-returns returns 1))
;; GARCH returns typically show lag1-autocorr ≈ 0.2-0.4
;; GBM returns would show ≈0 (no clustering)
```

**Why GARCH matters:**

- **Option pricing**: Volatility isn't constant—GARCH-implied options trade at different prices than Black-Scholes
- **Risk management**: VaR calculations using constant volatility underestimate risk during crises
- **Trading**: Volatility mean reversion—high volatility today predicts lower volatility in the future (sell volatility when high)

### 6.3.3 GARCH Option Pricing via Monte Carlo

Black-Scholes assumes constant volatility. GARCH allows time-varying volatility, giving more realistic option prices.

**Method:** Simulate GARCH paths, calculate option payoffs, discount and average.

```lisp
;; Price European call option under GARCH dynamics
;; Parameters:
;;   S0: current stock price
;;   K: strike price
;;   r: risk-free rate
;;   T: time to maturity (in same units as GARCH parameters)
;;   n-sims: number of Monte Carlo simulations
;;   mu, omega, alpha, beta, sigma0: GARCH parameters
(define (garch-option-price S0 K r T n-sims mu omega alpha beta sigma0)
  (let ((payoffs []))

    (for (sim (range 0 n-sims))
      ;; Simulate GARCH returns for T periods
      (let ((garch-result (garch-11 T mu omega alpha beta sigma0)))

        ;; Convert returns to price path
        ;; S_t = S_0 * exp(sum of returns)
        (let ((price-path (returns-to-prices S0 (garch-result :returns))))

          ;; Calculate call option payoff: max(S_T - K, 0)
          (let ((final-price (last price-path))
                (payoff (max 0 (- final-price K))))

            (set! payoffs (append payoffs payoff))))))

    ;; Discount expected payoff to present value
    (* (exp (- (* r T))) (average payoffs))))

;; Convert log-returns to price path
(define (returns-to-prices S0 returns)
  (let ((prices [S0])
        (current-price S0))

    (for (r returns)
      ;; Price multiplier: exp(return)
      (set! current-price (* current-price (exp r)))
      (set! prices (append prices current-price)))

    prices))

;; Example: Price 1-month ATM call with GARCH vol
(define garch-call-price
  (garch-option-price
    100.0      ;; S_0 = $100
    100.0      ;; K = $100 (at-the-money)
    0.05       ;; r = 5% risk-free rate
    21         ;; T = 21 days (1 month)
    10000      ;; 10,000 simulations
    0.0005 0.000005 0.08 0.90 0.015))  ;; GARCH params

;; Compare to Black-Scholes (constant vol):
;; GARCH price typically higher due to volatility risk premium
```

### 6.3.4 EGARCH: Asymmetric Volatility (Leverage Effect)

**Observation:** In equities, **down moves increase volatility more than up moves** of the same magnitude.

This is the **leverage effect**:
- Stock drops 5% → volatility spikes 20%
- Stock rises 5% → volatility barely changes

**EGARCH** (Exponential GARCH) captures this asymmetry:

$$\log(\sigma_t^2) = \omega + \alpha \frac{\epsilon_{t-1}}{\sigma_{t-1}} + \gamma \left(\left|\frac{\epsilon_{t-1}}{\sigma_{t-1}}\right| - \mathbb{E}\left[\left|\frac{\epsilon_{t-1}}{\sigma_{t-1}}\right|\right]\right) + \beta \log(\sigma_{t-1}^2)$$

where $\gamma < 0$ creates asymmetry (negative shocks increase volatility more).

```lisp
;; EGARCH(1,1) simulation
;; Parameters same as GARCH, plus:
;;   gamma: asymmetry parameter (negative for leverage effect)
(define (egarch-11 n-steps mu omega alpha gamma beta initial-log-sigma2)
  (let ((returns [])
        (log-sigma2s [initial-log-sigma2])
        (current-log-sigma2 initial-log-sigma2))

    (for (i (range 0 n-steps))
      ;; Current volatility: sigma = sqrt(exp(log-sigma^2))
      (let ((sigma (sqrt (exp current-log-sigma2)))
            (epsilon (standard-normal)))

        ;; Return: r_t = mu + sigma_t * epsilon_t
        (let ((return (+ mu (* sigma epsilon))))

          (set! returns (append returns return))

          ;; Update log-variance using EGARCH dynamics
          ;; Expected |Z| for Z ~ N(0,1) = sqrt(2/pi) ≈ 0.79788
          (let ((standardized-error (/ epsilon sigma))
                (expected-abs-error 0.79788))

            (let ((log-sigma2-next
                   (+ omega
                      (* alpha standardized-error)  ;; Sign effect
                      (* gamma (- (abs standardized-error)
                                 expected-abs-error))  ;; Size effect
                      (* beta current-log-sigma2))))  ;; Persistence

              (set! current-log-sigma2 log-sigma2-next)
              (set! log-sigma2s (append log-sigma2s log-sigma2-next)))))))

    {:returns returns
     :volatilities (map log-sigma2s (lambda (ls2) (sqrt (exp ls2))))}))

;; Example: Equity with leverage effect
;; gamma < 0 means negative shocks increase volatility more
(define egarch-sim
  (egarch-11 1000 0.0005 -0.2 -0.1 -0.15 0.98 (log 0.000225)))
```

---

## 6.4 Ornstein-Uhlenbeck Process: Mean Reversion

### 6.4.1 When Randomness Has Memory

Not all financial variables wander aimlessly. Some **revert to a long-term mean**:

- **Interest rates**: The Fed targets a specific rate—deviations are temporary
- **Commodity spreads**: Crack spreads (oil–gasoline) revert to refining costs
- **Pairs trading**: Price ratio of cointegrated stocks (e.g., Coke vs Pepsi)

The **Ornstein-Uhlenbeck (OU) process** models mean reversion:

$$dX_t = \theta(\mu - X_t)dt + \sigma dW_t$$

**Components:**

- $\mu$ = long-term mean (equilibrium level)
- $\theta$ = speed of mean reversion (larger = faster pull back to mean)
- $\sigma$ = volatility (random fluctuations around mean)
- $(\mu - X_t)$ = "error term"—distance from mean

**Intuition:** If $X_t > \mu$ (above mean), the drift term $\theta(\mu - X_t)$ is negative, pulling $X_t$ downward. If $X_t < \mu$, the drift is positive, pushing $X_t$ upward.

**Analogy:** A ball attached to a spring. Pull it away from equilibrium—it oscillates back, with friction (mean reversion) and random kicks (volatility).

**Solution:**

$$X_t = X_0 e^{-\theta t} + \mu(1 - e^{-\theta t}) + \sigma \int_0^t e^{-\theta(t-s)} dW_s$$

As $t \to \infty$:
- Deterministic part: $X_t \to \mu$ (converges to mean)
- Variance: $\text{Var}(X_t) \to \frac{\sigma^2}{2\theta}$ (stationary distribution)

**Half-life of mean reversion:**

$$\text{Half-life} = \frac{\log 2}{\theta}$$

Example: $\theta = 2$ → half-life = 0.35 years ≈ 4 months

```lisp
;; Ornstein-Uhlenbeck simulation
;; Parameters:
;;   X0: initial value
;;   theta: mean reversion speed
;;   mu: long-term mean
;;   sigma: volatility
;;   n-steps, dt: time discretization
(define (ornstein-uhlenbeck X0 theta mu sigma n-steps dt)
  (let ((path [X0])
        (current-X X0))

    (for (i (range 0 n-steps))
      ;; Drift term: theta * (mu - X_t) * dt
      ;; This pulls X toward mu
      (let ((drift (* theta (- mu current-X) dt))

            ;; Diffusion term: sigma * sqrt(dt) * Z
            (diffusion (* sigma (sqrt dt) (standard-normal))))

        ;; Update: X_{t+dt} = X_t + drift + diffusion
        (set! current-X (+ current-X drift diffusion))
        (set! path (append path current-X))))

    path))

;; Example: Pairs trading spread (mean-reverting)
;; Spread between two stock prices should revert to 0
(define spread-sim
  (ornstein-uhlenbeck
    0.0      ;; X_0 = 0 (start at mean)
    2.0      ;; θ = 2 (fast mean reversion: half-life ≈ 0.35 years)
    0.0      ;; μ = 0 (long-term mean)
    0.1      ;; σ = 0.1 (volatility around mean)
    252
    (/ 1 252)))

;; Calculate half-life
(define (ou-half-life theta)
  (/ (log 2) theta))

(define half-life (ou-half-life 2.0))
;; → 0.3466 years ≈ 87 trading days
;; Interpretation: After 87 days, half of any deviation from mean is eliminated
```

**Trading strategy based on OU:**

Enter positions when spread deviates significantly from mean, exit when it reverts.

```lisp
;; Mean-reversion trading strategy
;; Parameters:
;;   spread: time series of spread values
;;   threshold: number of standard deviations for entry (e.g., 2.0)
;; Returns: array of signals ("long", "short", "hold")
(define (ou-trading-strategy spread threshold)
  (let ((mean (average spread))
        (std (std-dev spread))
        (signals []))

    (for (i (range 0 (length spread)))
      (let ((value (nth spread i))
            (z-score (/ (- value mean) std)))  ;; Standardized deviation

        ;; If spread > mean + threshold*std → SHORT (expect reversion down)
        ;; If spread < mean - threshold*std → LONG (expect reversion up)
        ;; Otherwise HOLD
        (if (> z-score threshold)
            (set! signals (append signals "short"))
            (if (< z-score (- threshold))
                (set! signals (append signals "long"))
                (set! signals (append signals "hold"))))))

    signals))

;; Generate trading signals for 2-sigma threshold
(define trading-signals (ou-trading-strategy spread-sim 2.0))

;; Backtest: count how many times we'd trade
(define n-long-entries (length (filter trading-signals
                                       (lambda (s) (= s "long")))))
(define n-short-entries (length (filter trading-signals
                                        (lambda (s) (= s "short")))))
```

### 6.4.2 Vasicek Interest Rate Model

OU process is used to model **short-term interest rates**:

$$dr_t = \theta(\mu - r_t)dt + \sigma dW_t$$

where $r_t$ is the instantaneous interest rate.

**Properties:**

- Mean reversion: rates pulled toward long-term average $\mu$
- Allows negative rates (realistic post-2008, but problematic for some models)

```lisp
;; Vasicek interest rate model (just OU process for rates)
(define (vasicek r0 theta mu sigma n-steps dt)
  (ornstein-uhlenbeck r0 theta mu sigma n-steps dt))

;; Example: Simulate Fed Funds rate
(define interest-rate-sim
  (vasicek
    0.05     ;; r_0 = 5% current rate
    0.5      ;; θ = 0.5 (moderate mean reversion)
    0.04     ;; μ = 4% long-term rate
    0.01     ;; σ = 1% volatility
    252
    (/ 1 252)))
```

**Limitation:** Vasicek allows negative rates. For many applications, we need **positive rates**.

### 6.4.3 CIR Model: Non-Negative Mean Reversion

**Cox-Ingersoll-Ross (CIR)** model ensures non-negative rates via **square-root diffusion**:

$$dr_t = \theta(\mu - r_t)dt + \sigma \sqrt{r_t} dW_t$$

The $\sqrt{r_t}$ term means volatility decreases as $r_t \to 0$, preventing negative rates.

**Feller condition:** $2\theta\mu \geq \sigma^2$ ensures $r_t$ stays strictly positive.

```lisp
;; CIR simulation (square-root diffusion)
(define (cir r0 theta mu sigma n-steps dt)
  (let ((path [r0])
        (current-r r0))

    (for (i (range 0 n-steps))
      ;; Drift: theta * (mu - r_t) * dt
      (let ((drift (* theta (- mu current-r) dt))

            ;; Diffusion: sigma * sqrt(max(r_t, 0)) * sqrt(dt) * Z
            ;; The sqrt(r_t) ensures volatility → 0 as r_t → 0
            (diffusion (* sigma
                         (sqrt (max current-r 0))  ;; Prevent sqrt of negative
                         (sqrt dt)
                         (standard-normal))))

        ;; Update rate: r_{t+dt} = max(0, r_t + drift + diffusion)
        ;; Floor at 0 to prevent numerical negativity
        (set! current-r (max 0 (+ current-r drift diffusion)))
        (set! path (append path current-r))))

    path))

;; Example: Simulate positive interest rate
(define cir-sim
  (cir 0.03      ;; r_0 = 3%
       0.5       ;; θ = 0.5
       0.04      ;; μ = 4%
       0.05      ;; σ = 5%
       252
       (/ 1 252)))

;; Verify Feller condition: 2*theta*mu >= sigma^2
;; 2 * 0.5 * 0.04 = 0.04
;; sigma^2 = 0.0025
;; 0.04 >= 0.0025 ✓ (condition satisfied → strictly positive)
```

---

## 6.5 Monte Carlo Methods: Harnessing Randomness

### 6.5.1 The Monte Carlo Principle

**Core idea:** Can't solve a problem analytically? Simulate it many times and average the results.

**Example:** Pricing a complex derivative
1. Simulate 10,000 price paths (using GBM, GARCH, or jump-diffusion)
2. Calculate derivative payoff on each path
3. Average the payoffs
4. Discount to present value

**Why it works:** Law of Large Numbers—as simulations increase, the average converges to the true expectation.

**Error:** Monte Carlo error is $O(1/\sqrt{N})$, where $N$ = number of simulations.
- 100 sims → error ~10%
- 10,000 sims → error ~1%
- 1,000,000 sims → error ~0.1%

To reduce error by half, you need **4x more simulations** (expensive!).

### 6.5.2 Variance Reduction: Antithetic Variates

**Goal:** Reduce Monte Carlo error without increasing simulations.

**Antithetic Variates Technique:**

For every random path with shock $Z$, simulate a second path with shock $-Z$.

**Why this helps:** If the payoff function is monotonic in $Z$, then $f(Z)$ and $f(-Z)$ are negatively correlated, reducing variance.

**Variance reduction:** Typically 30-50% lower variance (equivalent to 1.5-2x more simulations).

```lisp
;; Standard Monte Carlo (baseline)
(define (monte-carlo-standard payoff-fn n-sims)
  (let ((payoffs []))

    (for (i (range 0 n-sims))
      (let ((Z (standard-normal))
            (payoff (payoff-fn Z)))
        (set! payoffs (append payoffs payoff))))

    (average payoffs)))

;; Antithetic variates Monte Carlo
(define (monte-carlo-antithetic payoff-fn n-sims)
  (let ((payoffs []))

    ;; Generate n-sims/2 pairs of (Z, -Z)
    (for (i (range 0 (/ n-sims 2)))
      (let ((Z (standard-normal))
            (payoff1 (payoff-fn Z))       ;; Payoff with Z
            (payoff2 (payoff-fn (- Z))))  ;; Payoff with -Z (antithetic)

        (set! payoffs (append payoffs payoff1))
        (set! payoffs (append payoffs payoff2))))

    (average payoffs)))

;; Example: Price European call option
;; S_T = S_0 * exp((r - 0.5*sigma^2)*T + sigma*sqrt(T)*Z)
;; Payoff = max(S_T - K, 0)
(define (gbm-call-payoff S0 K r sigma T Z)
  (let ((ST (* S0 (exp (+ (* (- r (* 0.5 sigma sigma)) T)
                         (* sigma (sqrt T) Z))))))
    (max 0 (- ST K))))

;; Standard MC: 10,000 simulations
(define standard-price
  (monte-carlo-standard
    (lambda (Z) (gbm-call-payoff 100 110 0.05 0.2 1 Z))
    10000))

;; Antithetic MC: 10,000 simulations (but using 5,000 pairs)
(define antithetic-price
  (monte-carlo-antithetic
    (lambda (Z) (gbm-call-payoff 100 110 0.05 0.2 1 Z))
    10000))

;; Both should give similar prices (around $6-7 for these parameters)
;; But antithetic variance is ~40% lower → more accurate with same # of sims
```

**Variance Reduction Techniques Comparison:**

| Method | Variance Reduction | Implementation Complexity | Speedup Factor |
|--------|-------------------|---------------------------|----------------|
| Standard MC | Baseline | Trivial | 1x |
| Antithetic Variates | 40% | Very Low | ~1.7x |
| Control Variates | 70% | Medium | ~3x |
| Importance Sampling | 90% | High | ~10x (for tail events) |
| Quasi-Monte Carlo | 50-80% | Medium | ~2-5x |

### 6.5.3 Control Variates: Using Known Solutions

**Idea:** If you know the exact price of a similar derivative, use it to reduce variance.

**Example:** Pricing an Asian option (payoff based on average price) using a European option (payoff based on final price) as control.

**Method:**

1. Simulate both Asian payoff $Y$ and European payoff $X$ on the same paths
2. Compute their correlation
3. Adjust the Asian estimate using the European error:

$$\hat{Y}_{\text{adjusted}} = \hat{Y} + c (E[X]_{\text{exact}} - \hat{X}_{\text{MC}})$$

where $c = -\frac{\text{Cov}(X,Y)}{\text{Var}(X)}$ is the optimal coefficient.

```lisp
;; Control variate: Price Asian option using European option as control
;; Asian option payoff: max(Avg(S) - K, 0)
;; European option payoff: max(S_T - K, 0)
(define (mc-asian-with-control S0 K r sigma T n-steps n-sims)
  (let ((asian-payoffs [])
        (european-payoffs []))

    ;; Simulate n-sims price paths
    (for (sim (range 0 n-sims))
      (let ((path (gbm S0 r sigma n-steps (/ T n-steps))))

        ;; Asian payoff: average of all prices along path
        (let ((avg-price (average path))
              (asian-pay (max 0 (- avg-price K))))
          (set! asian-payoffs (append asian-payoffs asian-pay)))

        ;; European payoff: final price only
        (let ((final-price (last path))
              (european-pay (max 0 (- final-price K))))
          (set! european-payoffs (append european-payoffs european-pay)))))

    ;; Compute Monte Carlo estimates
    (let ((asian-mean (average asian-payoffs))
          (european-mean (average european-payoffs))

          ;; Exact European option price (Black-Scholes)
          (european-exact (black-scholes-call S0 K r sigma T)))

      ;; Control variate adjustment coefficient
      (let ((c (/ (covariance asian-payoffs european-payoffs)
                 (variance european-payoffs))))

        ;; Adjusted Asian option price
        (+ asian-mean (* c (- european-exact european-mean)))))))

;; Black-Scholes call formula (analytical solution)
(define (black-scholes-call S K r sigma T)
  (let ((d1 (/ (+ (log (/ S K)) (* (+ r (* 0.5 sigma sigma)) T))
              (* sigma (sqrt T))))
        (d2 (- d1 (* sigma (sqrt T)))))

    (- (* S (normal-cdf d1))
       (* K (exp (- (* r T))) (normal-cdf d2)))))

;; Standard normal CDF approximation
(define (normal-cdf x)
  (if (< x 0)
      (- 1 (normal-cdf (- x)))
      (let ((t (/ 1 (+ 1 (* 0.2316419 x)))))
        (let ((poly (+ (* 0.319381530 t)
                      (* -0.356563782 t t)
                      (* 1.781477937 t t t)
                      (* -1.821255978 t t t t)
                      (* 1.330274429 t t t t t))))
          (- 1 (* (/ 1 (sqrt (* 2 3.14159))) (exp (* -0.5 x x)) poly))))))
```

**Variance reduction:** Control variates can reduce variance by 70% when control and target are highly correlated.

### 6.5.4 Quasi-Monte Carlo: Better Sampling

**Problem with standard MC:** Random sampling can leave gaps or clusters in the sample space.

**Solution: Quasi-Monte Carlo (QMC)** uses **low-discrepancy sequences** that fill space more uniformly.

**Examples:**
- Van der Corput sequence
- Halton sequence
- Sobol sequence (best for high dimensions)

**Convergence:** QMC achieves $O(1/N)$ error vs. standard MC's $O(1/\sqrt{N})$—much faster!

```lisp
;; Van der Corput sequence (simple low-discrepancy sequence)
;; Generates uniform [0,1] numbers more evenly distributed than random()
(define (van-der-corput n base)
  (let ((vdc 0)
        (denom 1))

    ;; Reverse base-representation of n
    (while (> n 0)
      (set! denom (* denom base))
      (set! vdc (+ vdc (/ (% n base) denom)))
      (set! n (floor (/ n base))))

    vdc))

;; Convert uniform [0,1] to standard normal using inverse CDF
;; This is the inverse transform method
(define (inverse-normal-cdf u)
  ;; Approximation (simplified for pedagogy)
  ;; For u in (0,1), map to standard normal
  (if (< u 0.5)
      (- (sqrt (* -2 (log u))))
      (sqrt (* -2 (log (- 1 u))))))

;; Quasi-Monte Carlo option pricing
(define (qmc-option-price payoff-fn n-sims)
  (let ((payoffs []))

    (for (i (range 1 (+ n-sims 1)))
      ;; Use Van der Corput instead of random()
      (let ((u (van-der-corput i 2))
            (Z (inverse-normal-cdf u))
            (payoff (payoff-fn Z)))
        (set! payoffs (append payoffs payoff))))

    (average payoffs)))

;; QMC typically converges 10-100x faster for smooth payoffs
(define qmc-price
  (qmc-option-price
    (lambda (Z) (gbm-call-payoff 100 110 0.05 0.2 1 Z))
    1000))  ;; Only 1000 sims needed (vs 10,000 for standard MC)
```

**When to use QMC:**

- ✅ Smooth payoffs (European options, vanilla swaps)
- ✅ Low-to-moderate dimensions (<50)
- ❌ Discontinuous payoffs (digital options)—QMC can be worse
- ❌ Path-dependent with early exercise (American options)—randomization needed

---

## 6.6 Calibration: Fitting Models to Market Data

### 6.6.1 Historical Volatility Estimation

Simplest method: Calculate standard deviation of historical returns.

```lisp
;; Calculate annualized historical volatility from price series
;; Parameters:
;;   prices: array of prices
;;   periods-per-year: number of periods in a year (e.g., 252 for daily)
(define (historical-volatility prices periods-per-year)
  (let ((returns (log-returns prices)))
    (let ((daily-vol (std-dev returns)))
      ;; Annualize: sigma_annual = sigma_daily * sqrt(periods per year)
      (* daily-vol (sqrt periods-per-year)))))

;; Example: Daily prices → annualized volatility
(define prices [100 102 101 103 104 102 105 107 106 108])
(define annual-vol (historical-volatility prices 252))
;; Typical result: 0.15-0.40 (15-40% annualized)
```

### 6.6.2 GARCH Parameter Estimation (Maximum Likelihood)

**Goal:** Find parameters $(\omega, \alpha, \beta)$ that maximize the likelihood of observed returns.

**Log-likelihood for GARCH(1,1):**

$$\mathcal{L}(\omega, \alpha, \beta) = -\frac{1}{2}\sum_{t=1}^T \left[\log(2\pi) + \log(\sigma_t^2) + \frac{r_t^2}{\sigma_t^2}\right]$$

**Method:** Grid search or numerical optimization (in practice, use specialized libraries).

```lisp
;; Log-likelihood for GARCH(1,1)
(define (garch-log-likelihood returns omega alpha beta)
  (let ((n (length returns))
        ;; Initial variance: unconditional variance
        (sigma-squared (/ (* omega 1) (- 1 alpha beta)))
        (log-lik 0))

    (for (i (range 0 n))
      (let ((r (nth returns i)))

        ;; Add to log-likelihood: -0.5 * (log(2π) + log(σ²) + r²/σ²)
        (set! log-lik (- log-lik
                        (* 0.5 (+ 1.8378770  ;; log(2π)
                                 (log sigma-squared)
                                 (/ (* r r) sigma-squared)))))

        ;; Update variance for next period: σ²_t = ω + α*r²_{t-1} + β*σ²_{t-1}
        (set! sigma-squared (+ omega
                              (* alpha r r)
                              (* beta sigma-squared)))))

    log-lik))

;; Simplified grid search for GARCH parameters
(define (estimate-garch returns)
  (let ((best-params null)
        (best-lik -999999))

    ;; Grid search over parameter space
    (for (omega [0.000001 0.000005 0.00001])
      (for (alpha [0.05 0.08 0.10 0.15])
        (for (beta [0.85 0.90 0.92])

          ;; Check stationarity: alpha + beta < 1
          (if (< (+ alpha beta) 1)
              (let ((lik (garch-log-likelihood returns omega alpha beta)))
                (if (> lik best-lik)
                    (do
                      (set! best-lik lik)
                      (set! best-params {:omega omega :alpha alpha :beta beta}))
                    null))
              null))))

    best-params))

;; Example usage:
;; (define estimated-params (estimate-garch historical-returns))
```

### 6.6.3 Mean Reversion Speed Calibration

**Estimate $\theta$ (mean reversion speed) from spread data using OLS regression.**

**Method:** Regress $\Delta X_t$ on $X_{t-1}$:

$$\Delta X_t = a + b X_{t-1} + \epsilon$$

Then $\theta = -b$ (negative slope indicates mean reversion).

```lisp
;; Estimate Ornstein-Uhlenbeck theta parameter via OLS
(define (estimate-ou-theta spread)
  (let ((n (length spread))
        (sum-x 0)
        (sum-y 0)
        (sum-xx 0)
        (sum-xy 0))

    ;; Build regression data: y = Delta X_t, x = X_{t-1}
    (for (i (range 1 n))
      (let ((x (nth spread (- i 1)))             ;; X_{t-1}
            (y (- (nth spread i) (nth spread (- i 1)))))  ;; Delta X_t

        (set! sum-x (+ sum-x x))
        (set! sum-y (+ sum-y y))
        (set! sum-xx (+ sum-xx (* x x)))
        (set! sum-xy (+ sum-xy (* x y)))))

    ;; OLS slope: beta = (n*sum_xy - sum_x*sum_y) / (n*sum_xx - sum_x^2)
    (let ((n-minus-1 (- n 1)))
      (let ((slope (/ (- sum-xy (/ (* sum-x sum-y) n-minus-1))
                     (- sum-xx (/ (* sum-x sum-x) n-minus-1)))))

        ;; Mean reversion speed: theta = -slope
        ;; (Assuming dt=1; otherwise divide by dt)
        (- slope)))))

;; Example:
;; (define theta-estimate (estimate-ou-theta spread-data))
;; (define half-life (/ (log 2) theta-estimate))
```

---

## 6.7 Practical Applications

### 6.7.1 Heston Stochastic Volatility Model

**Problem:** GARCH models volatility as deterministic given past returns. But volatility itself is random!

**Heston Model:** Both price and volatility follow stochastic processes:

$$\begin{aligned}
dS_t &= \mu S_t dt + \sqrt{V_t} S_t dW_t^S \\
dV_t &= \kappa(\theta - V_t) dt + \sigma_v \sqrt{V_t} dW_t^V
\end{aligned}$$

where $\text{Corr}(dW_t^S, dW_t^V) = \rho$.

**Parameters:**
- $V_t$ = variance (volatility squared)
- $\kappa$ = speed of volatility mean reversion
- $\theta$ = long-run variance
- $\sigma_v$ = volatility of volatility ("vol of vol")
- $\rho$ = correlation between price and volatility (typically negative for equities)

```lisp
;; Heston model simulation
(define (heston-simulation S0 V0 kappa theta sigma-v rho mu n-steps dt)
  (let ((prices [S0])
        (variances [V0])
        (current-S S0)
        (current-V V0))

    (for (i (range 0 n-steps))
      ;; Generate independent standard normals
      (let ((Z1 (standard-normal))
            (Z2 (standard-normal)))

        ;; Create correlated Brownian motions
        (let ((W-S Z1)
              (W-V (+ (* rho Z1)
                     (* (sqrt (- 1 (* rho rho))) Z2))))

          ;; Update variance (CIR dynamics to ensure V > 0)
          (let ((dV (+ (* kappa (- theta current-V) dt)
                      (* sigma-v (sqrt (max current-V 0)) (sqrt dt) W-V))))

            ;; Ensure variance stays positive
            (set! current-V (max 0.0001 (+ current-V dV)))

            ;; Update price using current variance
            (let ((dS (+ (* mu current-S dt)
                        (* (sqrt current-V) current-S (sqrt dt) W-S))))

              (set! current-S (+ current-S dS))
              (set! prices (append prices current-S))
              (set! variances (append variances current-V)))))))

    {:prices prices :variances variances}))

;; Example: Equity with stochastic vol
(define heston-sim
  (heston-simulation
    100.0      ;; S_0 = $100
    0.04       ;; V_0 = 0.04 (20% volatility)
    2.0        ;; κ = 2 (mean reversion speed)
    0.04       ;; θ = 0.04 (long-run variance)
    0.3        ;; σ_v = 0.3 (vol of vol)
    -0.7       ;; ρ = -0.7 (leverage effect: negative correlation)
    0.10       ;; μ = 10% drift
    252
    (/ 1 252)))
```

**Why Heston matters:**

- **Volatility smile**: Matches market-observed implied volatility patterns
- **Path-dependent options**: Exotic options depend on both price and volatility evolution
- **VIX modeling**: VIX (volatility index) is essentially $\sqrt{V_t}$ in Heston

### 6.7.2 Value at Risk (VaR) via Monte Carlo

**VaR** = "What's the maximum loss we expect 95% of the time?"

**Method:**
1. Simulate 10,000 portfolio paths
2. Calculate P&L on each path
3. Find the 5th percentile (95% of outcomes are better)

```lisp
;; Value at Risk via Monte Carlo
;; Parameters:
;;   S0: current portfolio value
;;   mu: expected return
;;   sigma: volatility
;;   T: time horizon (e.g., 10 days)
;;   confidence: confidence level (e.g., 0.95 for 95% VaR)
;;   n-sims: number of simulations
(define (portfolio-var S0 mu sigma T confidence n-sims)
  (let ((final-values []))

    ;; Simulate portfolio values at time T
    (for (sim (range 0 n-sims))
      (let ((path (gbm S0 mu sigma 1 T)))  ;; Single-step for final value
        (let ((ST (last path)))
          (set! final-values (append final-values ST)))))

    ;; Sort final values
    (let ((sorted (sort final-values)))

      ;; VaR = loss at (1 - confidence) percentile
      (let ((var-index (floor (* n-sims (- 1 confidence)))))
        (let ((var-level (nth sorted var-index)))

          {:VaR (- S0 var-level)
           :VaR-percent (/ (- S0 var-level) S0)})))))

;; Example: 10-day 95% VaR for $100k portfolio
(define var-result
  (portfolio-var
    100000     ;; Portfolio value
    0.10       ;; 10% annual expected return
    0.25       ;; 25% annual volatility
    (/ 10 252) ;; 10 trading days
    0.95       ;; 95% confidence
    10000))    ;; 10,000 simulations

;; Typical result: VaR ≈ $5,000-$8,000
;; Interpretation: 95% of the time, we won't lose more than this amount in 10 days
```

**Conditional VaR (CVaR)**: Average loss in the worst 5% of cases (more conservative).

```lisp
;; Conditional VaR (Expected Shortfall)
(define (portfolio-cvar S0 mu sigma T confidence n-sims)
  (let ((final-values []))

    ;; Simulate final values
    (for (sim (range 0 n-sims))
      (let ((ST (last (gbm S0 mu sigma 1 T))))
        (set! final-values (append final-values ST))))

    ;; Find VaR threshold
    (let ((sorted (sort final-values))
          (var-index (floor (* n-sims (- 1 confidence)))))
      (let ((var-threshold (nth sorted var-index)))

        ;; CVaR = average loss beyond VaR threshold
        (let ((tail-losses []))
          (for (val final-values)
            (if (< val var-threshold)
                (set! tail-losses (append tail-losses (- S0 val)))
                null))

          {:CVaR (average tail-losses)
           :CVaR-percent (/ (average tail-losses) S0)})))))

;; Example: 95% CVaR (expected loss in worst 5% of scenarios)
(define cvar-result (portfolio-cvar 100000 0.10 0.25 (/ 10 252) 0.95 10000))
;; Typical result: CVaR ≈ $10,000-$15,000 (worse than VaR, as expected)
```

---

## 6.8 Key Takeaways

**Model Selection Framework:**

| Market Behavior | Recommended Model | Key Parameters |
|-----------------|-------------------|----------------|
| Equity indices (normal times) | GBM + GARCH | μ≈0.10, σ≈0.20, α≈0.08, β≈0.90 |
| Equity indices (with crashes) | GARCH + Merton jumps | λ≈2, μ_J≈-0.05, σ_J≈0.10 |
| Crypto assets | Kou jump-diffusion | High σ, asymmetric jumps |
| Interest rates | CIR (positive rates) | θ≈0.5, μ≈0.04 |
| Commodity spreads | Ornstein-Uhlenbeck | Estimate θ from data |
| Options pricing (realistic) | Heston stochastic vol | Calibrate to IV surface |

**Common Pitfalls:**

- ⚠️ **Ignoring fat tails**: Normal distributions underestimate crash risk—use jump-diffusion
- ⚠️ **Constant volatility**: GARCH shows volatility clusters—use time-varying vol
- ⚠️ **Overfitting calibration**: Out-of-sample validation essential
- ⚠️ **Discretization error**: Use small $\Delta t$ (≤ 1/252 for daily data)

**Computational Efficiency:**

| Task | Method | Speed |
|------|--------|-------|
| Single path | Direct simulation | Instant |
| 10K paths | Standard MC | ~1 second |
| 10K paths | Antithetic MC | ~1 second (same time, less error) |
| High accuracy | QMC + control variates | 10x faster than standard MC |
| Path-dependent options | GPU parallelization | 100x faster |

**Next Steps:**

Chapter 7 applies these stochastic processes to optimization problems:
- Calibrating GARCH parameters via maximum likelihood
- Optimizing portfolio weights under jump-diffusion dynamics
- Walk-forward testing with stochastic simulations

The randomness you've learned to simulate here becomes the foundation for testing and refining trading strategies.

---

## Further Reading

1. **Glasserman, P. (2003)**. *Monte Carlo Methods in Financial Engineering*. Springer.
   - The definitive reference for Monte Carlo methods—comprehensive and rigorous.

2. **Cont, R., & Tankov, P. (2004)**. *Financial Modelling with Jump Processes*. Chapman & Hall.
   - Deep dive into jump-diffusion models with real-world calibration examples.

3. **Shreve, S. (2004)**. *Stochastic Calculus for Finance II: Continuous-Time Models*. Springer.
   - Mathematical foundations—rigorous treatment of Brownian motion and Itô calculus.

4. **Tsay, R. S. (2010)**. *Analysis of Financial Time Series* (3rd ed.). Wiley.
   - Practical guide to GARCH models with extensive empirical examples.

5. **Heston, S. (1993)**. "A Closed-Form Solution for Options with Stochastic Volatility". *Review of Financial Studies*, 6(2), 327-343.
   - Original paper introducing the Heston model—surprisingly readable.

---

**Navigation:**
- [← Chapter 5: Time Series Analysis](05_time_series.md)
- [→ Chapter 7: Optimization Algorithms](07_optimization.md)
