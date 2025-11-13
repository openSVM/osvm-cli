# OVSM Language Examples

This directory contains comprehensive examples demonstrating OVSM's features.

## Running the Tests

```bash
# Run all tests
./run_all_tests.sh

# Run individual example
osvm ovsm run 01_data_structures.ovsm
```

## Examples Overview

### 01_data_structures.ovsm
**Features demonstrated:**
- Basic data types (int, float, string, bool, null)
- Arrays (creation, access, operations)
- Objects (creation, access, dynamic keys with `set`)
- Nested structures
- Deep property access

**Key learnings:**
- Use `set` for dynamic object keys: `(set obj variable_key value)`
- Use `get` for safe property access: `(get obj "key")`
- Arrays and objects can be deeply nested

### 02_control_flow.ovsm
**Features demonstrated:**
- Conditionals (`if`, `when`, `unless`)
- Loops (`while`, `for`, `range`)
- Pattern matching (`case`, `typecase`)
- Nested conditionals

**Key learnings:**
- `if` always returns a value (functional style)
- `for` can iterate arrays or ranges
- `case` provides clean multi-branch logic
- `typecase` enables type-based dispatch

### 03_functional_programming.ovsm
**Features demonstrated:**
- Higher-order functions (`map`, `filter`)
- Lambda expressions (closures)
- Array operations (`take`, `drop`, `concat`)
- String operations (`toLowerCase`, `toUpperCase`, `string-contains`)

**Key learnings:**
- Function signatures: `map(array, fn)`, `filter(array, fn)`
- Lambdas capture variables from outer scope
- `concat` is variadic: `(concat arr1 arr2 arr3 ...)`

### 04_real_world_patterns.ovsm
**Features demonstrated:**
- Dynamic object construction
- Error handling with null checks
- Data aggregation (counting, summing)
- Pagination patterns
- Fallback chains with `or`

**Key learnings:**
- Build objects incrementally with `set!` and `set`
- Use `null?` for safe data access
- Chain fallbacks: `(or val1 val2 default)`
- Aggregate in loops with mutable counters

### 05_defi_analytics.ovsm
**Features demonstrated:**
- Liquidity pool calculations (constant product AMM)
- Swap output and price impact
- Arbitrage opportunity detection
- Impermanent loss calculation
- Multi-DEX price comparison

**Key learnings:**
- AMM formula: x * y = k (constant product)
- Price impact increases with swap size
- Arbitrage spread = (sell_price - buy_price) / buy_price
- Impermanent loss occurs when prices diverge from initial ratio

### 06_trading_strategies.ovsm
**Features demonstrated:**
- Moving Average Crossover (SMA)
- Relative Strength Index (RSI)
- Bollinger Bands (mean ± 2σ)
- Portfolio rebalancing algorithms
- Volume-Weighted Average Price (VWAP)

**Key learnings:**
- SMA smooths noise, crossover generates signals
- RSI < 30 = oversold, RSI > 70 = overbought
- Bollinger Bands identify volatility and extremes
- VWAP shows institutional activity levels

### 07_risk_management.ovsm
**Features demonstrated:**
- Kelly Criterion position sizing
- Stop loss & take profit calculation
- Value at Risk (VaR) estimation
- Maximum drawdown analysis
- Sharpe ratio calculation
- Correlation analysis for diversification

**Key learnings:**
- Kelly Criterion optimizes bet size based on edge
- Use Half Kelly for safer position sizing
- VaR quantifies maximum expected loss at confidence level
- Sharpe ratio measures risk-adjusted returns
- High correlation reduces diversification benefits

### 08_market_microstructure.ovsm
**Features demonstrated:**
- Order book analysis (bid/ask depth, spread)
- Volume profile and Point of Control (POC)
- Time & Sales analysis (order flow delta)
- Market impact modeling (square-root model)
- TWAP vs VWAP execution comparison
- Hidden liquidity detection (iceberg orders)
- Microstructure noise measurement

**Key learnings:**
- Order book imbalance predicts short-term pressure
- POC = price with highest traded volume
- Positive delta = aggressive buying > selling
- Impact ≈ volatility × √(order_size / daily_volume)
- Bid-ask bounce creates measurement noise

### 09_execution_algorithms.ovsm
**Features demonstrated:**
- TWAP (Time-Weighted Average Price) execution
- VWAP (Volume-Weighted Average Price) execution
- Iceberg order simulation (hidden size)
- Dark pool routing strategies
- Slippage calculation and monitoring
- Order splitting and timing optimization

**Key learnings:**
- TWAP splits orders evenly across time periods
- VWAP matches historical volume profile for minimal impact
- Iceberg orders hide large size to prevent information leakage
- Dark pools reduce market impact for large orders
- Optimal split size balances market impact vs timing risk

### 10_market_making.ovsm
**Features demonstrated:**
- Avellaneda-Stoikov market making model
- Inventory risk management and skewing
- Bid-ask spread optimization
- PnL tracking and performance metrics
- Adverse selection detection
- Maker rebate calculations

**Key learnings:**
- Optimal spread = γσ² + (2/γ)ln(1 + γ/κ)
- Inventory skew adjusts quotes to manage position risk
- High maker rebates incentivize providing liquidity
- Adverse selection occurs when informed traders pick off quotes
- Target inventory = 0 minimizes directional risk

### 11_statistical_arbitrage.ovsm
**Features demonstrated:**
- Pairs trading with cointegration
- Z-score mean reversion strategy
- Spread calculation and normalization
- Entry/exit signal generation
- Half-life estimation for mean reversion
- Kalman filter for dynamic hedge ratios

**Key learnings:**
- Cointegrated pairs revert to historical relationship
- Z-score > 2 or < -2 triggers trades
- Half-life indicates how fast spread reverts
- Kalman filter adapts to changing market conditions
- Spread = Price1 - (hedge_ratio × Price2)

### 12_options_volatility.ovsm
**Features demonstrated:**
- Implied volatility calculation (simplified Black-Scholes)
- Options Greeks (delta, gamma, theta, vega)
- Volatility smile analysis
- Straddle and strangle strategies
- Iron condor construction
- Volatility arbitrage detection

**Key learnings:**
- IV = market's expectation of future volatility
- Delta measures directional exposure
- Gamma measures convexity (delta sensitivity)
- Theta = time decay (always negative for long options)
- Vega = sensitivity to IV changes
- Volatility smile shows skew in IV across strikes

### 13_ai_sentiment_trading.ovsm
**Features demonstrated:**
- News sentiment aggregation and scoring
- Social media sentiment analysis (Twitter, Reddit, Discord)
- Sentiment momentum tracking (rate of change)
- Fear & Greed index calculation
- Volume-adjusted sentiment signals
- Sentiment-driven position sizing

**Key learnings:**
- Aggregate sentiment predicts short-term price movement
- Weight news by recency and source credibility
- Sentiment momentum confirms trend strength
- Fear & Greed index combines multiple sentiment sources
- High sentiment + high volume = strong signal
- Position size scales with sentiment confidence

### 14_ml_prediction_trading.ovsm
**Features demonstrated:**
- Linear regression for price prediction
- R-squared confidence calculation
- Exponential Moving Average (EMA) prediction
- Candlestick pattern recognition (uptrends)
- Momentum-based prediction
- Neural network simulation (perceptron)
- Ensemble model aggregation (5 models)
- Automated trading decisions based on ML scores

**Key learnings:**
- Linear regression: predicted_price = slope × time + intercept
- R-squared measures prediction quality (1.0 = perfect fit)
- EMA = α × price + (1-α) × previous_EMA
- Perceptron: output = sigmoid(Σ(feature × weight) + bias)
- Ensemble models reduce single-model bias
- Combine multiple signals for higher confidence trades

### 15_pumpswap_sniper.ovsm
**Features demonstrated:**
- New token listing detection on PumpSwap
- Liquidity analysis for sniping opportunities
- Whale transaction monitoring (frontrunning detection)
- Sandwich attack profitability calculation
- Bundle transaction strategies (atomic execution)
- Anti-rug security checks (mint authority, LP burn)
- Sniper bot decision matrix (multi-signal scoring)

**Key learnings:**
- Snipe tokens with 5-50 SOL liquidity for optimal risk/reward
- Frontrun whale buys by 10% priority fee for guaranteed execution
- Sandwich attacks profit from victim's price impact
- Check: mint authority, freeze authority, LP burned, holder distribution
- Safety score < 60 = high rug risk, avoid
- Combine liquidity, competition, whale activity for entry signal

### 16_memecoin_momentum.ovsm
**Features demonstrated:**
- Momentum detection (1-min, 5-min rate of change)
- Volume analysis and confirmation
- Holder distribution tracking (inflow/outflow)
- Whale accumulation detection
- Social media metrics (Twitter, Telegram)
- Multi-tier profit taking (2x, 5x, 10x, 20x)
- Trailing stop loss (dynamic based on volatility)
- Momentum divergence detection (bearish signal)
- FOMO protection (avoid chasing pumps)
- Risk-adjusted position sizing

**Key learnings:**
- Momentum > 50% + volume > 2x avg = strong signal
- Net holder inflow + whale entry = bullish
- Social hype score amplifies price movement
- Exit in tiers: 25% at 2x, 25% at 5x, etc.
- Trailing stop: -15% from peak protects profits
- Bearish divergence: price up + volume down = warning
- Avoid entries after +50% pump (FOMO zone)
- Position size = risk_amount / stop_loss_distance

### 17_whale_copy_trading.ovsm
**Features demonstrated:**
- Whale wallet identification and ranking
- Real-time whale transaction monitoring
- Copy trade signal generation (consensus detection)
- Smart entry timing (wait for dip after detection)
- Position sizing based on whale confidence
- Whale exit detection (sell signals)
- Anti-whale dump protection (coordinated sells)
- Whale holding duration analysis
- Wallet clustering detection (same entity, multiple wallets)

**Key learnings:**
- Rank whales by: win_rate (40%) + avg_profit (40%) + consistency (20%)
- Multiple whales buying same token = strong signal
- Wait 20-120 seconds after detection for optimal entry
- Copy size = whale_size × 2% × whale_confidence
- Exit immediately when whale sells
- Dump detected: ≥2 whales selling + volume > 20 SOL
- Average whale hold time predicts exit window
- Wallet clusters may amplify or fake signals

### 18_mev_bundle_sniping.ovsm
**Features demonstrated:**
- Jito MEV bundle construction and validation
- Dynamic tip calculation based on competition
- Bundle transaction ordering (critical for success)
- Compute budget optimization (units vs priority fee trade-off)
- Landing probability calculation
- Multi-bundle strategy (send variants with different tips)
- Bundle retry logic with tip escalation
- Cross-bundle arbitrage opportunities
- Backrun strategy (profit from other snipers)
- Anti-sandwich protection via private mempool

**Key learnings:**
- Bundle must include tip to Jito tip account (3 addresses available)
- Transaction order: tip → snipe → compute limit → approve
- Optimal tip = max(competitor_tips) × 1.2, capped at 5% of trade
- Landing probability = tip_percentile × compute_percentile × timing - congestion_penalty
- Expected value (EV) = profit × landing_probability
- Multi-bundle strategy: send 3-4 variants with different tips for max EV
- Retry with +0.005 SOL tip increase if bundle fails
- Private mempool reduces sandwich risk by 80-90%
- Backrun other snipers' buys for immediate profit
- Bundle visibility ≈ 0.02 (98% stealth) vs public mempool

### 19_flash_loan_sniping.ovsm
**Features demonstrated:**
- Flash loan provider comparison (Solend, Kamino, MarginFi)
- Leveraged snipe strategy (10x buying power)
- Flash loan execution flow (borrow → buy → pump → sell → repay)
- Profitability calculation with leverage
- Risk analysis (minimum pump required, slippage impact)
- Multi-hop flash loan arbitrage
- Flash loan sandwich attack
- Failure scenarios (revert probability, adverse price moves)
- Optimal flash loan size calculation
- Atomic transaction validation

**Key learnings:**
- Flash loan fees: 5-9 bps depending on provider
- Leverage ratio = (capital + loan) / capital (e.g., 10x with 1 SOL + 9 SOL loan)
- ROI on capital = (profit / capital) × 100 (amplified by leverage)
- Min pump required = 1 + (flash_loan_fee / total_capital) to break even
- Slippage cost = total_capital × (slippage_% / 100)
- Flash loan must be atomic: borrow and repay in same transaction
- Multi-hop arb: buy on PumpSwap, sell on Raydium in one tx
- Flash loan sandwich: frontrun with loan, backrun with instant sell
- Expected loss from failures = revert_prob × tx_fee + adverse_prob × loss
- Risk-adjusted EV = profit - slippage - expected_failures

### 20_bot_competition_defense.ovsm
**Features demonstrated:**
- Bot competition analysis (detect competing snipers)
- Latency optimization breakdown (RPC, signing, construction, send)
- Priority fee warfare (dynamic adjustment based on percentiles)
- Deceptive strategies (fake transactions, randomized timing)
- Anti-copy-trading protection (detect and counter copycats)
- Mempool monitoring defense (private mempool, Jito bundles)
- Bot fingerprinting (identify patterns to counter)
- Sybil attack detection (same entity, multiple bots)
- Smart order routing (choose best path based on congestion)
- Adaptive competition strategy (adjust based on intensity)
- Cost-benefit analysis (monthly costs vs revenue)

**Key learnings:**
- Identify fastest competitor and calculate speed gap needed
- Latency breakdown: RPC (30ms) + sig (15ms) + construct (25ms) + send (20ms)
- Optimize each component: dedicated RPC (-10ms), pre-sign (-10ms), cached ix (-8ms)
- Fee percentiles: Target 90th percentile to beat 90% of bots
- Deception: Send 3 fake txs + 1 real (competitors waste 4x resources)
- Anti-copy: Signal-to-noise ratio = real_trades / (real + decoy)
- Private mempool + Jito = 98% visibility reduction (0.02 vs 1.0)
- Bot patterns: consistent tips, fixed timing, exact amounts
- Sybil detection: shared funding sources between wallets
- Adaptive strategy: HIGH competition = max tip + Jito + private RPC
- Cost-benefit: Must achieve positive ROI after RPC + Jito + compute fees
- Competition viability score: can_beat (30%) + win_prob (30%) + ROI (30%) + stealth (10%)

### 21_ai_token_scoring.ovsm
**Features demonstrated:**
- Feature extraction (liquidity-to-age, volume-to-liquidity, holder growth)
- Multi-layer neural network scoring (3-layer with ReLU activation)
- Gradient boosting ensemble (weighted combination of weak learners)
- Random forest classification (decision tree ensemble, majority voting)
- K-means clustering (group tokens by market cap characteristics)
- Anomaly detection (z-score based scam detection)
- Sentiment integration (60% AI + 40% sentiment)
- Confidence intervals (prediction confidence based on score extremity)
- Model performance metrics (accuracy, precision, recall, F1 score)

**Key learnings:**
- Feature engineering: liq/age, vol/liq, holder_growth, normalized values
- Neural network: ReLU activation for hidden layers, sigmoid for output
- Ensemble: Combine liq-focused (40%), vol-focused (35%), holder-focused (25%)
- Random forest: 4 decision trees, majority vote determines BUY/SKIP
- Clustering: Euclidean distance to classify as Low/Mid/High-cap
- Anomaly: z-score > 2 or < -2 indicates potential scam or gem
- Combined score: 60% AI model + 40% sentiment for final ranking
- Confidence: |score - 0.5| × 200 gives prediction confidence %
- Model quality: Accuracy 78%, Precision 86.6%, Recall 88.6%, F1 87.6%

### 22_deep_learning_patterns.ovsm
**Features demonstrated:**
- Convolutional pattern detection (price candle patterns)
- LSTM sequence prediction (time series with forget/input/output gates)
- Attention mechanism (multi-head attention for feature importance)
- Transformer encoder (self-attention matrix for token classification)
- GAN anomaly detection (generative adversarial for scam detection)
- Q-Learning (reinforcement learning Q-table for buy/sell/hold)
- Policy gradient (continuous action space for position sizing)
- Actor-Critic architecture (policy network + value network)
- Deep Q-Network (DQN with neural network Q-function approximation)
- Ensemble deep learning (combine CNN, LSTM, Transformer, DQN votes)

**Key learnings:**
- Convolution: Apply filters to detect bullish/bearish price patterns
- LSTM: forget_gate × cell_state + input_gate → updated predictions
- Attention: Query-Key similarity with softmax normalization
- Transformer: Self-attention = dot_product(vec1, vec2) across all vectors
- GAN: Distance from normal patterns detects anomalies (threshold 500)
- Q-Learning: Select action with max Q-value from Q-table
- Policy gradient: Sample actions from mean ± std distribution
- Actor-Critic: Advantage = reward - value_estimate (>0 = execute)
- DQN: Neural network approximates Q(state, action) for all actions
- Ensemble: Majority vote across 4 models (≥3 votes = consensus)

### 23_ai_portfolio_optimization.ovsm
**Features demonstrated:**
- Mean-variance optimization (Markowitz portfolio theory, Sharpe ratios)
- Black-Litterman model (blend market equilibrium with investor views)
- Risk parity allocation (equal risk contribution, inverse volatility)
- Genetic algorithm (evolve optimal portfolios through generations)
- Monte Carlo simulation (10+ simulations, 30-day horizon)
- Kelly Criterion for memecoins (optimal position sizing with caps)
- Dynamic rebalancing (compare current vs target, execute trades)
- Correlation matrix analysis (identify high-correlation pairs)
- Portfolio quality scoring (Sharpe, diversification, expected return, downside)

**Key learnings:**
- Sharpe ratio = (expected_return - risk_free) / volatility
- Black-Litterman: blended_return = (1-τ×conf)×market + τ×conf×view
- Risk parity: weight = (1/volatility) / Σ(1/volatilities)
- Genetic algorithm: Evolve population by fitness (portfolio Sharpe)
- Monte Carlo: Simulate 10 paths, calculate avg/best/worst outcomes
- Kelly: kelly_fraction = (p×b - q) / b, use half-Kelly for safety, cap at 30%
- Rebalancing: Only trade if |current - target| > 1 SOL threshold
- Correlation: Pairs with avg_corr > 0.6 reduce diversification
- Quality score: Sharpe (30%) + diversification (25%) + return (25%) + downside (20%)

## Common Patterns

### Dynamic Keys
```lisp
;; Build object with variable keys
(define result {})
(define key "dynamic_key")
(define result (set result key "value"))
```

### Safe Data Access
```lisp
;; Avoid errors with null checks
(define data (get response "data"))
(define safe_data (if (null? data) {} data))
```

### Aggregation
```lisp
;; Count by grouping
(define counts {})
(for (item items)
  (define type (get item "type"))
  (define current (get counts type))
  (define count (if (null? current) 1 (+ current 1)))
  (set! counts (set counts type count)))
```

## Function Signatures Reference

```lisp
;; Arrays
(map array fn)
(filter array fn)
(take array n)
(drop array n)
(concat arr1 arr2 ...)
(flatten nested_array)

;; Objects
(get obj key)
(set obj key value)  ;; Returns new object
(keys obj)
(values obj)

;; Strings
(concat str1 str2 ...)
(toLowerCase str)
(toUpperCase str)
(string-contains str substring)

;; Control
(if condition then else)
(when condition body...)
(unless condition body...)
(for (var collection) body...)
(while condition body...)
```

## Test Results

All 43 examples pass successfully (100% success rate):

**Fundamentals (1-4)**
- ✅ 01: Data structures
- ✅ 02: Control flow
- ✅ 03: Functional programming
- ✅ 04: Real-world patterns

**DeFi & Traditional Trading (5-12)**
- ✅ 05: DeFi analytics
- ✅ 06: Trading strategies
- ✅ 07: Risk management
- ✅ 08: Market microstructure
- ✅ 09: Execution algorithms
- ✅ 10: Market making
- ✅ 11: Statistical arbitrage
- ✅ 12: Options volatility

**AI/ML Trading (13-14)**
- ✅ 13: AI sentiment trading
- ✅ 14: ML prediction trading

**PumpSwap & Memecoin Trading (15-20)**
- ✅ 15: PumpSwap sniper
- ✅ 16: Memecoin momentum
- ✅ 17: Whale copy trading
- ✅ 18: MEV bundle sniping
- ✅ 19: Flash loan sniping
- ✅ 20: Bot competition defense

**Advanced AI & Deep Learning (21-23)**
- ✅ 21: AI token scoring
- ✅ 22: Deep learning patterns
- ✅ 23: AI portfolio optimization

**Professional Trading Strategies (24-33)**
- ✅ 24: Order flow imbalance
- ✅ 25: HFT market making
- ✅ 26: Cross-exchange arbitrage
- ✅ 27: Liquidity provision
- ✅ 28: Smart order routing
- ✅ 29: Volatility trading
- ✅ 30: Pairs trading
- ✅ 31: Grid trading bot
- ✅ 32: DCA with AI timing
- ✅ 33: Multi-timeframe analysis

**Institutional Strategies (34-43)**
- ✅ 34: Iceberg order detection
- ✅ 35: Statistical arbitrage ML
- ✅ 36: Order book reconstruction
- ✅ 37: Alpha signal combination
- ✅ 38: Regime switching
- ✅ 39: Portfolio rebalancing
- ✅ 40: Slippage prediction
- ✅ 41: Market impact models
- ✅ 42: Adaptive execution
- ✅ 43: Advanced risk metrics

### 24_order_flow_imbalance.ovsm
**Core Strategy**: Trade on order book imbalances and microstructure signals
- Order Flow Imbalance (OFI) = (bid_vol - ask_vol) / total_vol
- Volume-Weighted Mid Price (VWMP) leads regular mid price
- Microprice = (best_bid×ask_size + best_ask×bid_size) / total_size
- Aggressive order detection (market orders that cross spread)
- Toxicity index (informed trading / adverse selection risk)
- Queue position advantage (top 30% = high fill probability)

### 25_hft_market_making.ovsm  
**Core Strategy**: High-frequency quotes with inventory management
- Inventory skew adjusts bid/ask (skew = inventory / limit × 0.5)
- Sub-10ms quote refresh rates (100 quotes/second)
- Adverse selection protection (widen spread at high intensity)
- Real-time PnL tracking (realized + unrealized)

### 26_cross_exchange_arbitrage.ovsm
**Core Strategy**: Exploit price differences across DEXes
- Monitor Raydium, Orca, Jupiter simultaneously
- Net spread = gross_spread - (buy_fee + sell_fee)
- Optimal size = net_spread / slippage_impact
- Execute atomic cross-DEX trades

### 27_liquidity_provision.ovsm
**Core Strategy**: Optimize LP positions considering IL and fees
- Impermanent Loss = 2×√(price_ratio) / (1 + price_ratio) - 1
- Fee earnings vs IL cost comparison
- Net PnL = fee_earned - il_cost
- Dynamic withdraw/provide decisions

### 28_smart_order_routing.ovsm
**Core Strategy**: Route orders to optimal venue
- Score venues by: impact (order/liquidity) + latency + fee
- Minimize market impact across fragmented liquidity
- Latency-aware routing (sub-30ms targets)

### 29_volatility_trading.ovsm
**Core Strategy**: Trade volatility regimes
- Realized volatility from returns (√variance × √252)
- Vol regime classification (HIGH/MEDIUM/LOW)
- Strategy: Sell options in HIGH vol, buy in LOW vol

### 30_pairs_trading_cointegration.ovsm
**Core Strategy**: Mean-reversion on cointegrated pairs
- Spread = Price_A - (hedge_ratio × Price_B)
- Z-score = (current_spread - mean_spread) / std_dev
- Signal: z > +2 SHORT spread, z < -2 LONG spread

### 31_grid_trading_bot.ovsm
**Core Strategy**: Profit from range-bound markets
- Grid levels from lower to upper bound
- Buy orders below price, sell orders above
- Profit = num_fills × grid_step × size

### 32_dca_ai_timing.ovsm
**Core Strategy**: AI-enhanced dollar-cost averaging
- Timing multiplier = 1 + (AI_score - 0.5) × 0.5
- Bonus 1.5x on oversold + bullish sentiment
- Dynamic position sizing based on market conditions

### 33_multi_timeframe_analysis.ovsm
**Core Strategy**: Confirm trends across timeframes
- Weight: 1m (1), 5m (2), 15m (3), 1h (4)
- Trend score: sum of aligned timeframes
- Signal: score ≥8 STRONG BUY, ≥5 BUY, else WAIT


### 34_iceberg_order_detection.ovsm
**Detect hidden liquidity**: Iceberg ratio = total_fills / visible_size. Ratio >2 indicates hidden orders. Track refill events.

### 35_statistical_arbitrage_ml.ovsm
**ML-enhanced pairs**: Linear model with weights (z-score 45%, momentum 35%, volume 20%). Reversion probability determines position size.

### 36_order_book_reconstruction.ovsm
**Reconstruct from trades**: Volume delta = buy_vol - sell_vol. Positive delta = bullish pressure. Trade velocity measures intensity.

### 37_alpha_signal_combination.ovsm
**Sharpe-weighted ensemble**: Combine 5 alpha signals weighted by Sharpe ratios. Combined_alpha = Σ(signal × weight).

### 38_regime_switching_strategies.ovsm
**Adaptive strategies**: TRENDING (momentum), MEAN_REVERT (fade), BREAKOUT (aggressive), VOLATILE (reduce size).

### 39_portfolio_rebalancing.ovsm
**Cost-benefit rebalancing**: Only rebalance if breakeven <30 days. Total cost = Σ(|diff| × 0.003).

### 40_slippage_prediction_model.ovsm
**Market impact prediction**: Slippage = vol × √(size/volume) + spread/2. Split orders if predicted >25bps.

### 41_market_impact_models.ovsm
**Almgren-Chriss model**: Permanent + temporary impact. Optimal duration = size / (daily_vol × target_participation).

### 42_adaptive_execution_algorithms.ovsm
**Dynamic pacing**: Schedule_ratio = %_done / %_time. Speed up if <0.8, slow down if >1.2. Adapt to volatility.

### 43_advanced_risk_metrics.ovsm
**Comprehensive risk**: VaR, CVaR, Max Drawdown, Calmar, Sortino. Risk score combines 4 metrics for overall assessment.

