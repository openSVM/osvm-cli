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

All 8 examples pass successfully:
- ✅ Data structures
- ✅ Control flow
- ✅ Functional programming
- ✅ Real-world patterns
- ✅ DeFi analytics
- ✅ Trading strategies
- ✅ Risk management
- ✅ Market microstructure
