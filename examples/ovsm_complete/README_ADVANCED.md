# Advanced Trading Examples for OVSM

This directory contains 12 comprehensive examples demonstrating professional trading and quantitative finance applications in OVSM.

## Running the Examples

```bash
# Run all tests
./run_all_tests.sh

# Run individual example
osvm ovsm run 09_execution_algorithms.ovsm
```

## Examples Overview

### 09_execution_algorithms.ovsm
**Features demonstrated:**
- TWAP (Time-Weighted Average Price) execution
- VWAP (Volume-Weighted Average Price) execution
- Iceberg orders (hidden liquidity)
- Implementation shortfall measurement
- POV (Percentage of Volume) adaptive execution
- Dark pool routing strategies

**Key learnings:**
- TWAP splits orders equally across time
- VWAP weights orders by expected market volume
- Iceberg orders hide true size to minimize market impact
- Implementation shortfall = decision price - execution price
- POV algorithms adapt to real-time market volume
- Dark pools offer ~60% fill rates vs 100% on lit exchanges

**Use cases:**
- Executing large institutional orders
- Minimizing market impact and information leakage
- Optimizing execution costs

### 10_market_making.ovsm
**Features demonstrated:**
- Basic bid-ask spread calculation
- Inventory risk management and quote skewing
- Avellaneda-Stoikov optimal market making model
- Adverse selection protection
- Multi-level quote layering
- PnL calculation (realized and unrealized)
- Maker-taker fee economics

**Key learnings:**
- Spread = profit per round trip (buy + sell)
- Skew quotes based on inventory to manage risk
- Avellaneda-Stoikov: reservation price = mid - γσ²qT
- Widen spreads when detecting toxic flow
- Layer quotes at multiple price levels
- Maker rebates improve profitability vs taker fees

**Use cases:**
- High-frequency market making
- Liquidity provision on DEXs
- Inventory risk management

### 11_statistical_arbitrage.ovsm
**Features demonstrated:**
- Pairs trading with z-score signals
- Mean reversion testing
- Kalman filter for spread estimation
- Basket arbitrage (index vs constituents)
- Strategy backtesting framework

**Key learnings:**
- Pairs trading: trade spread between correlated assets
- Z-score > 2: short spread, Z-score < -2: long spread
- Mean reversion rate > 60% indicates tradeable relationship
- Kalman filters smooth noisy spread estimates
- Basket arbitrage: trade index vs weighted basket of stocks

**Use cases:**
- Statistical arbitrage strategies
- Pairs/spread trading
- Index arbitrage
- Mean reversion trading

### 12_options_volatility.ovsm
**Features demonstrated:**
- Black-Scholes option pricing (approximation)
- Implied volatility (IV) calculation
- Volatility smile and skew analysis
- Realized vs implied volatility comparison
- Straddle strategy (long volatility)
- Iron Condor strategy (short volatility)
- Gamma scalping for delta-hedged positions

**Key learnings:**
- Option value = intrinsic + time value
- IV backed out from market prices using Newton-Raphson
- Volatility smile: OTM options have higher IV than ATM
- IV > RV: sell volatility, IV < RV: buy volatility
- Straddle profits from large moves in either direction
- Iron Condor profits from low volatility (range-bound)
- Gamma scalping profits from rehedging delta

**Use cases:**
- Options trading strategies
- Volatility trading (long vol, short vol)
- Delta-hedging and gamma scalping
- Implied vs realized volatility arbitrage

## Common Patterns

### Price Impact Calculation
```lisp
;; Square-root model
(define participation_rate (/ order_size daily_volume))
(define impact_guess (/ participation_rate 2.0))
(define impact_factor (/ (+ impact_guess (/ participation_rate impact_guess)) 2.0))
(define price_impact (* volatility impact_factor spot_price))
```

### Inventory Skewing
```lisp
;; Adjust quotes based on position
(define inventory_skew (/ current_position max_position))
(define skew_adjustment (* inventory_skew max_skew_pct))
(define adjusted_bid (- mid_price (* mid_price skew_adjustment)))
(define adjusted_ask (+ mid_price (* mid_price skew_adjustment)))
```

### Z-Score Calculation
```lisp
;; For pairs trading signals
(define z_score (/ (- current_value mean) std_dev))
(define signal (if (> z_score 2.0)
                   "SHORT spread"
                   (if (< z_score -2.0)
                       "LONG spread"
                       "HOLD")))
```

### PnL Tracking
```lisp
;; Realized PnL from trades
(for (trade trades)
  (if (= side :buy)
      (set! position (+ position qty))
      (do
        (define cost_basis (* qty avg_cost))
        (define proceeds (* qty price))
        (set! pnl (+ pnl (- proceeds cost_basis)))
        (set! position (- position qty)))))

;; Unrealized PnL from open position
(define unrealized_pnl (* position (- current_price avg_cost)))
```

## Technical Concepts

### Execution Algorithms
- **TWAP**: Time-Weighted Average Price - equal slices over time
- **VWAP**: Volume-Weighted Average Price - match market volume profile
- **POV**: Percentage of Volume - adaptive to market conditions
- **Implementation Shortfall**: Cost of delayed execution

### Market Making
- **Reservation Price**: Fair value adjusted for inventory risk
- **Adverse Selection**: Informed traders picking off stale quotes
- **Maker-Taker**: Fee structure favoring passive liquidity provision
- **Quote Layering**: Multiple price levels to capture more flow

### Statistical Arbitrage
- **Cointegration**: Long-term equilibrium relationship between assets
- **Mean Reversion**: Tendency of spread to return to average
- **Kalman Filter**: Optimal estimation of true spread value
- **Basket Arbitrage**: Trade index vs constituent stocks

### Options Trading
- **Greeks**: Delta, Gamma, Vega, Theta measure sensitivities
- **Implied Volatility**: Market's expectation of future volatility
- **Volatility Smile**: IV varies by strike (OTM > ATM)
- **Gamma Scalping**: Profit from rehedging delta-neutral position

## Performance Metrics

### Execution Quality
- **Average execution price** vs VWAP/TWAP benchmark
- **Slippage**: Difference from decision price
- **Market impact**: Price movement caused by order
- **Fill rate**: Percentage of order completed

### Market Making
- **Profit per round trip**: Spread captured
- **Inventory turnover**: How quickly position flips
- **Fill ratio**: Maker fills / total fills
- **Sharpe ratio**: Risk-adjusted returns

### Statistical Arbitrage
- **Z-score**: Standard deviations from mean
- **Mean reversion rate**: How often spread reverts
- **Trades per day**: Strategy frequency
- **Win rate**: Percentage of profitable trades

### Options
- **Delta**: Price sensitivity to underlying
- **Gamma**: Delta sensitivity to underlying
- **Vega**: Price sensitivity to volatility
- **Theta**: Time decay per day

## Real-World Applications

### Crypto Trading
- DEX liquidity provision (Uniswap, Raydium, Orca)
- CEX-DEX arbitrage
- Pairs trading (SOL/ETH, BTC/USD)
- Perps funding rate arbitrage

### TradFi
- Equity index arbitrage
- Fixed income spread trading
- Commodity futures pairs
- Options volatility trading

### DeFi
- Automated market making
- Liquidation strategies
- Yield farming optimization
- Cross-chain arbitrage

## Test Results

All 12 examples pass successfully:
- ✅ Data structures (01)
- ✅ Control flow (02)
- ✅ Functional programming (03)
- ✅ Real-world patterns (04)
- ✅ DeFi analytics (05)
- ✅ Trading strategies (06)
- ✅ Risk management (07)
- ✅ Market microstructure (08)
- ✅ Execution algorithms (09)
- ✅ Market making (10)
- ✅ Statistical arbitrage (11)
- ✅ Options & volatility (12)

## Next Steps

### Enhancements Needed
1. **Auto type coercion**: Automatic int→float conversion in comparisons
2. **Network calls**: HTTP/WebSocket/JSON-RPC support for live data
3. **Math functions**: Native sqrt, pow, log, exp
4. **Date/time**: Timestamp parsing and manipulation
5. **Statistics**: Native correlation, regression functions

### Advanced Topics to Add
- Machine learning models (linear regression, classification)
- Monte Carlo simulations
- Portfolio optimization (Markowitz)
- Greeks calculation (numerical derivatives)
- Real-time order book analysis

## Resources

- **Execution algorithms**: Almgren-Chriss (2000), Kissell (2013)
- **Market making**: Avellaneda & Stoikov (2008)
- **Statistical arbitrage**: Vidyamurthy (2004)
- **Options trading**: Hull (2017), Natenberg (1994)
