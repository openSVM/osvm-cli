# Chapter 5: Functional Programming for Trading Systems

## The $440 Million Bug That Could Have Been Prevented

On August 1, 2012, Knight Capital Group deployed new trading software. Within 45 minutes, a hidden bug executed 4 million trades, accumulating a $440 million loss—nearly wiping out the company. The root cause? **Mutable shared state** in their order routing system.

A flag variable that should have been set to "off" remained "on" from a previous test. This single piece of mutable state, shared across multiple trading algorithms, caused the system to repeatedly send duplicate orders. Each algorithm thought it was acting independently, but they were all reading and modifying the same variable without coordination.

This disaster illustrates the central problem that functional programming solves: **when state can change anywhere, bugs can hide everywhere**.

---

## Why Trading Systems Need Functional Programming

Before we dive into techniques, let's understand *why* functional programming matters for trading:

**Problem 1: Backtests that lie**

You run a backtest on Monday. Sharpe ratio: 2.3. You run the *exact same code* on Tuesday with the *exact same data*. Sharpe ratio: 1.8. What happened?

Your strategy accidentally relied on global state—perhaps a counter that wasn't reset, or a cache that accumulated stale data. The backtest isn't **reproducible** because the code has **side effects** that change hidden state.

**Problem 2: Race conditions in live trading**

Your strategy monitors SOL/USDC on two exchanges. When prices diverge, you arbitrage. Simple, right?

Thread 1 sees: Binance $100, Coinbase $102 → Buy Binance, Sell Coinbase
Thread 2 sees: Binance $100, Coinbase $102 → Buy Binance, Sell Coinbase

Both threads execute simultaneously. You buy 200 SOL on Binance (double the intended position) and sell 200 on Coinbase. The arbitrage profit evaporates because you've doubled your transaction costs and moved the market against yourself.

The problem? **Shared mutable state** (your position counter) accessed by multiple threads without proper synchronization.

**Problem 3: Debugging temporal chaos**

Your strategy makes a bad trade at 2:47 PM. You add logging to debug, but now the bug disappears. Why? Your logging code modified global state (a counter, a timestamp, a file pointer), changing the program's behavior. This is a **Heisenbug**—observation changes the outcome.

Functional programming eliminates these problems through three core principles:

1. **Pure functions**: Output depends only on inputs, never on hidden state
2. **Immutability**: Data never changes after creation
3. **Composition**: Build complex behavior from simple, reusable pieces

Let's explore each principle by solving real trading problems.

---

## 5.1 Pure Functions: Making Code Predictable

### What Makes a Function "Pure"?

A pure function is like a mathematical equation. Given the same inputs, it *always* produces the same outputs, and it doesn't change anything else in the world.

**Mathematical function (pure):**
```
f(x) = x² + 2x + 1
f(3) = 16    (always)
f(3) = 16    (always)
f(3) = 16    (always, forever)
```

**Trading calculation (pure):**
```lisp
;; Calculate profit/loss from a trade
(define (calculate-pnl entry-price exit-price quantity)
  (* quantity (- exit-price entry-price)))

;; This function is pure because:
;; 1. Same inputs → same output (always)
(calculate-pnl 100 105 10)  ;; → 50
(calculate-pnl 100 105 10)  ;; → 50 (deterministic)

;; 2. No side effects (doesn't change anything)
;; - Doesn't modify global variables
;; - Doesn't write to databases
;; - Doesn't send network requests
;; - Doesn't print to console
```

Now contrast with an **impure** version:

```lisp
;; IMPURE: Modifies global state
(define total-pnl 0)  ;; Global variable (danger!)

(define (calculate-pnl-impure entry-price exit-price quantity)
  (let ((pnl (* quantity (- exit-price entry-price))))
    (set! total-pnl (+ total-pnl pnl))  ;; Side effect!
    pnl))

;; This function has hidden behavior:
(calculate-pnl-impure 100 105 10)  ;; → 50, and total-pnl becomes 50
(calculate-pnl-impure 100 105 10)  ;; → 50, but total-pnl becomes 100!

;; Two problems:
;; 1. The function's behavior depends on when you call it
;; 2. Concurrent calls will corrupt total-pnl (race condition)
```

### Why Purity Matters: The Backtest Reproducibility Problem

Imagine backtesting a simple moving average crossover strategy:

```lisp
;; IMPURE VERSION (typical imperative style)
(define sma-fast [])  ;; Global arrays (hidden state!)
(define sma-slow [])
(define position 0)

(define (backtest-impure prices)
  ;; BUG: These arrays accumulate across multiple backtest runs!
  (set! sma-fast [])  ;; We "reset" them, but...
  (set! sma-slow [])
  (set! position 0)

  (for (i (range 10 (length prices)))
    (let ((fast (average (slice prices (- i 10) i)))
          (slow (average (slice prices (- i 20) i))))

      (set! sma-fast (append sma-fast fast))  ;; Side effect
      (set! sma-slow (append sma-slow slow))  ;; Side effect

      ;; Trading logic
      (if (and (> fast slow) (= position 0))
          (set! position 100)  ;; Buy
          (if (and (< fast slow) (> position 0))
              (set! position 0)  ;; Sell
              null))))

  {:final-position position
   :trades (length sma-fast)})  ;; Wrong! Counts SMA calculations, not trades
```

This code has subtle bugs:

1. **Non-deterministic across runs**: If you call `backtest-impure` twice, the second run might behave differently because global arrays might not be fully cleared
2. **Hard to test**: You can't test the SMA calculation independently—it's entangled with the trading logic
3. **Impossible to parallelize**: Two backtests running simultaneously will corrupt each other's global state

Now the **pure** version:

```lisp
;; PURE VERSION: All inputs explicit, all outputs explicit
(define (calculate-sma prices window)
  "Calculate simple moving average for a price series.
   Returns array of SMA values (length = prices.length - window + 1)"

  (let ((result []))  ;; Local variable (no global state)
    (for (i (range (- window 1) (length prices)))
      ;; Extract window of prices
      (let ((window-prices (slice prices (- i window -1) (+ i 1))))
        ;; Calculate average for this window
        (let ((avg (/ (sum window-prices) window)))
          (set! result (append result avg)))))
    result))

;; Test it in isolation:
(define test-prices [100 102 101 103 105])
(define sma-3 (calculate-sma test-prices 3))
;; → [101.0, 102.0, 103.0]
;;
;; Let's verify by hand:
;; Window 1: [100, 102, 101] → average = 303/3 = 101.0 ✓
;; Window 2: [102, 101, 103] → average = 306/3 = 102.0 ✓
;; Window 3: [101, 103, 105] → average = 309/3 = 103.0 ✓

(define (generate-signals fast-sma slow-sma)
  "Generate buy/sell signals from two SMA series.
   Returns array of signals: 'buy', 'sell', or 'hold'"

  (let ((signals []))
    (for (i (range 0 (min (length fast-sma) (length slow-sma))))
      (let ((fast (nth fast-sma i))
            (slow (nth slow-sma i)))

        ;; Crossover logic
        (if (> i 0)
            (let ((prev-fast (nth fast-sma (- i 1)))
                  (prev-slow (nth slow-sma (- i 1))))

              ;; Golden cross: fast crosses above slow
              (if (and (> fast slow) (<= prev-fast prev-slow))
                  (set! signals (append signals "buy"))

                  ;; Death cross: fast crosses below slow
                  (if (and (< fast slow) (>= prev-fast prev-slow))
                      (set! signals (append signals "sell"))

                      ;; No cross
                      (set! signals (append signals "hold")))))

            ;; First signal (no previous value to compare)
            (set! signals (append signals "hold")))))
    signals))

(define (simulate-trades signals prices initial-capital)
  "Simulate trades based on signals.
   Returns final portfolio state with trade history"

  (let ((capital initial-capital)
        (position 0)
        (trades []))

    (for (i (range 0 (length signals)))
      (let ((signal (nth signals i))
            (price (nth prices i)))

        ;; Execute buy
        (if (and (= signal "buy") (= position 0))
            (let ((shares (floor (/ capital price))))
              (set! position shares)
              (set! capital (- capital (* shares price)))
              (set! trades (append trades {:time i :type "buy" :price price :shares shares})))

            ;; Execute sell
            (if (and (= signal "sell") (> position 0))
                (do
                  (set! capital (+ capital (* position price)))
                  (set! trades (append trades {:time i :type "sell" :price price :shares position}))
                  (set! position 0))
                null))))

    ;; Final portfolio value
    (let ((final-value (+ capital (* position (last prices)))))
      {:capital capital
       :position position
       :trades trades
       :final-value final-value
       :pnl (- final-value initial-capital)})))

;; Pure backtest: compose pure functions
(define (backtest-pure prices fast-window slow-window initial-capital)
  "Complete backtest pipeline using pure function composition"

  (let ((fast-sma (calculate-sma prices fast-window))
        (slow-sma (calculate-sma prices slow-window)))

    ;; Align SMAs (slow-sma is shorter, starts later)
    (let ((offset (- slow-window fast-window))
          (aligned-fast (slice fast-sma offset (length fast-sma))))

      (let ((signals (generate-signals aligned-fast slow-sma)))

        ;; Align prices with signals
        (let ((aligned-prices (slice prices (+ slow-window -1) (length prices))))

          (simulate-trades signals aligned-prices initial-capital))))))
```

Now let's see why this matters:

```lisp
;; Test with concrete data
(define test-prices [100 102 101 103 105 104 106 108 107 110 112 111])

;; Run backtest once
(define result1 (backtest-pure test-prices 3 5 10000))
;; → {:final-value 11234 :pnl 1234 :trades [...]}

;; Run exact same backtest again
(define result2 (backtest-pure test-prices 3 5 10000))
;; → {:final-value 11234 :pnl 1234 :trades [...]}

;; Results are IDENTICAL because function is pure
(= result1 result2)  ;; → true (always!)

;; Run 1000 backtests in parallel (if we had parallelism)
(define results
  (map (range 0 1000)
       (lambda (_) (backtest-pure test-prices 3 5 10000))))

;; ALL 1000 results are identical
(define unique (deduplicate results))
(length unique)  ;; → 1 (only one unique result)

;; This is the power of purity:
;; - Deterministic: Same inputs → same outputs
;; - Testable: Each function can be tested in isolation
;; - Composable: Functions combine like LEGO bricks
;; - Parallelizable: No shared state = no race conditions
;; - Debuggable: No hidden state to corrupt your reasoning
```

### The Testing Advantage

Because each function is pure, we can test them independently with **concrete examples**:

```lisp
;; Test SMA calculation
(define test-sma (calculate-sma [10 20 30 40 50] 3))
;; Expected: [20, 30, 40]
;; Window 1: (10+20+30)/3 = 20 ✓
;; Window 2: (20+30+40)/3 = 30 ✓
;; Window 3: (30+40+50)/3 = 40 ✓
(assert (= test-sma [20 30 40]))

;; Test signal generation
(define test-signals (generate-signals [10 15 20] [12 14 16]))
;; Fast starts below slow (10 < 12), then crosses above
;; Expected: ["hold", "hold", "buy"]
(assert (= (nth test-signals 2) "buy"))

;; Test trade execution
(define test-trades (simulate-trades ["hold" "buy" "hold" "sell"]
                                     [100 105 110 108]
                                     1000))
;; Buy at 105: get floor(1000/105) = 9 shares, capital = 1000 - 945 = 55
;; Sell at 108: capital = 55 + 9*108 = 1027
;; PnL = 1027 - 1000 = 27
(assert (= (test-trades :pnl) 27))
```

Every function can be verified **by hand** with small inputs. This is impossible with impure functions that depend on hidden state.

---

## 5.2 Higher-Order Functions: Code That Writes Code

### The Problem: Repetitive Transformations

You're analyzing a portfolio of 100 assets. For each asset, you need to:
1. Calculate daily returns
2. Compute volatility
3. Normalize returns (z-score)
4. Detect outliers

The imperative way leads to repetitive loops:

```lisp
;; IMPERATIVE: Repetitive loops everywhere
(define returns [])
(for (i (range 1 (length prices)))
  (set! returns (append returns (/ (nth prices i) (nth prices (- i 1))))))

(define squared-returns [])
(for (i (range 0 (length returns)))
  (set! squared-returns (append squared-returns
                                (* (nth returns i) (nth returns i)))))

(define normalized [])
(let ((mean (average returns))
      (std (std-dev returns)))
  (for (i (range 0 (length returns)))
    (set! normalized (append normalized
                            (/ (- (nth returns i) mean) std)))))
```

Notice the pattern: **loop through array, transform each element, build new array**. This appears three times!

Higher-order functions eliminate this repetition by treating **functions as data**:

```lisp
;; FUNCTIONAL: Transform with map
(define returns
  (map (range 1 (length prices))
       (lambda (i) (/ (nth prices i) (nth prices (- i 1))))))

(define squared-returns
  (map returns (lambda (r) (* r r))))

(define normalized
  (let ((mean (average returns))
        (std (std-dev returns)))
    (map returns (lambda (r) (/ (- r mean) std)))))
```

The `map` function encapsulates the loop pattern. We just specify *what* to do to each element (via a `lambda` function), not *how* to loop.

### Map: Transform Every Element

`map` takes two arguments:
1. A collection (array)
2. A function to apply to each element

It returns a new array with the function applied to every element.

**By hand example:**

```lisp
;; Transform each price to a percentage change
(define prices [100 105 103 108])

;; What we want:
;; 100 → (no previous, skip)
;; 105 → (105/100 - 1) * 100 = 5%
;; 103 → (103/105 - 1) * 100 = -1.9%
;; 108 → (108/103 - 1) * 100 = 4.85%

(define pct-changes
  (map (range 1 (length prices))
       (lambda (i)
         (* 100 (- (/ (nth prices i) (nth prices (- i 1))) 1)))))

;; Result: [5.0, -1.9047, 4.8543]

;; Let's verify step by step:
;; i=1: prices[1]=105, prices[0]=100 → 100*(105/100-1) = 100*0.05 = 5.0 ✓
;; i=2: prices[2]=103, prices[1]=105 → 100*(103/105-1) = 100*(-0.019) = -1.9 ✓
;; i=3: prices[3]=108, prices[2]=103 → 100*(108/103-1) = 100*0.0485 = 4.85 ✓
```

### Filter: Select Elements That Match

`filter` takes a collection and a **predicate** (a function that returns true/false), returning only elements where the predicate is true.

**By hand example:**

```lisp
;; Find all days with returns > 2%
(define returns [0.5 2.3 -1.2 3.1 0.8 -0.5 2.7])

(define large-gains
  (filter returns (lambda (r) (> r 2.0))))

;; Process each element:
;; 0.5 > 2.0? NO → exclude
;; 2.3 > 2.0? YES → include
;; -1.2 > 2.0? NO → exclude
;; 3.1 > 2.0? YES → include
;; 0.8 > 2.0? NO → exclude
;; -0.5 > 2.0? NO → exclude
;; 2.7 > 2.0? YES → include

;; Result: [2.3, 3.1, 2.7]
```

### Reduce: Aggregate to a Single Value

`reduce` (also called "fold") collapses an array into a single value by repeatedly applying a function.

**By hand example:**

```lisp
;; Calculate total portfolio value
(define holdings [
  {:symbol "SOL" :shares 100 :price 105}
  {:symbol "BTC" :shares 2 :price 45000}
  {:symbol "ETH" :shares 10 :price 2500}
])

(define total-value
  (reduce holdings
          0  ;; Starting value (accumulator)
          (lambda (acc holding)
            (+ acc (* (holding :shares) (holding :price))))))

;; Step-by-step execution:
;; acc=0, holding=SOL → acc = 0 + 100*105 = 10500
;; acc=10500, holding=BTC → acc = 10500 + 2*45000 = 100500
;; acc=100500, holding=ETH → acc = 100500 + 10*2500 = 125500

;; Result: 125500
```

The `reduce` pattern:
1. Start with an initial value (the accumulator)
2. Process each element, updating the accumulator
3. Return final accumulator value

### Composing Higher-Order Functions: Building Pipelines

The real power emerges when we **chain** these operations:

```lisp
;; Calculate volatility of large positive returns

;; Step 1: Calculate returns
(define prices [100 105 103 108 110 107 112])

;; Step 2: Convert to returns
(define returns
  (map (range 1 (length prices))
       (lambda (i) (- (/ (nth prices i) (nth prices (- i 1))) 1))))
;; → [0.05, -0.019, 0.049, 0.019, -0.027, 0.047]

;; Step 3: Filter for large gains (> 2%)
(define large-gains
  (filter returns (lambda (r) (> r 0.02))))
;; → [0.05, 0.049, 0.047]

;; Step 4: Calculate variance (reduce)
(let ((n (length large-gains))
      (mean (/ (reduce large-gains 0 +) n)))

  ;; Sum of squared deviations
  (let ((sum-sq-dev
         (reduce large-gains 0
                 (lambda (acc r)
                   (+ acc (* (- r mean) (- r mean)))))))

    ;; Variance
    (/ sum-sq-dev n)))

;; Let's work through this by hand:
;; large-gains = [0.05, 0.049, 0.047]
;; mean = (0.05 + 0.049 + 0.047) / 3 = 0.146 / 3 = 0.04867
;;
;; Squared deviations:
;; (0.05 - 0.04867)² = 0.000133² = 0.0000000177
;; (0.049 - 0.04867)² = 0.000033² = 0.0000000011
;; (0.047 - 0.04867)² = -0.001667² = 0.0000027779
;;
;; Sum = 0.0000029967
;; Variance = 0.0000029967 / 3 = 0.000000999
```

This pipeline is:
- **Declarative**: Says *what* to compute, not *how* to loop
- **Composable**: Each step is independent and testable
- **Pure**: No side effects, fully reproducible
- **Readable**: Reads like a specification

### Real Example: Multi-Indicator Strategy

Let's build a complete trading strategy using higher-order functions:

```lisp
;; Strategy: Buy when ALL indicators bullish, sell when ALL bearish

;; Indicator 1: RSI > 30 (oversold recovery)
(define (rsi-indicator prices period threshold)
  (let ((rsi (calculate-rsi prices period)))
    (map rsi (lambda (r) (> r threshold)))))

;; Indicator 2: Price above 50-day MA
(define (ma-indicator prices period)
  (let ((ma (calculate-sma prices period)))
    (map (range 0 (length ma))
         (lambda (i)
           (> (nth prices (+ i period -1)) (nth ma i))))))

;; Indicator 3: Volume above average
(define (volume-indicator volumes period threshold-multiplier)
  (let ((avg-vol (calculate-sma volumes period)))
    (map (range 0 (length avg-vol))
         (lambda (i)
           (> (nth volumes (+ i period -1))
              (* threshold-multiplier (nth avg-vol i)))))))

;; Combine indicators: ALL must agree
(define (combine-indicators indicator-arrays)
  "Returns array of booleans: true only when ALL indicators true"

  ;; Make sure all arrays have same length
  (let ((min-len (reduce indicator-arrays
                         (length (first indicator-arrays))
                         (lambda (acc arr) (min acc (length arr))))))

    ;; For each index, check if ALL indicators are true
    (map (range 0 min-len)
         (lambda (i)
           ;; Use reduce to implement AND across all indicators
           (reduce indicator-arrays true
                   (lambda (acc indicator-arr)
                     (and acc (nth indicator-arr i))))))))

;; Generate signals from combined indicators
(define (indicators-to-signals combined-indicators)
  (map combined-indicators
       (lambda (bullish) (if bullish "buy" "hold"))))

;; Complete strategy
(define (multi-indicator-strategy prices volumes)
  (let ((rsi-signals (rsi-indicator prices 14 30))
        (ma-signals (ma-indicator prices 50))
        (vol-signals (volume-indicator volumes 20 1.5)))

    (let ((combined (combine-indicators [rsi-signals ma-signals vol-signals])))
      (indicators-to-signals combined))))
```

Let's trace through with concrete data:

```lisp
;; Simplified example with 5 data points
(define test-prices [100 105 103 108 110])
(define test-volumes [1000 1200 900 1500 1100])

;; Assume we've calculated indicators:
(define rsi-signals [false true true true false])     ;; RSI crosses 30
(define ma-signals [true true false true true])       ;; Price vs MA
(define vol-signals [false true false true false])    ;; High volume

;; Combine with AND logic:
;; Index 0: false AND true AND false = FALSE
;; Index 1: true AND true AND true = TRUE
;; Index 2: true AND false AND false = FALSE
;; Index 3: true AND true AND true = TRUE
;; Index 4: false AND true AND false = FALSE

(define combined [false true false true false])
(define signals ["hold" "buy" "hold" "buy" "hold"])
```

The beauty of this approach:
1. **Each indicator is independent** (easy to test)
2. **Combination logic is separate** (easy to modify AND vs OR)
3. **Pure functions throughout** (no hidden state)
4. **Highly composable** (add/remove indicators trivially)

---

## 5.3 Immutability: Why Never Changing Data Prevents Bugs

### The Race Condition Disaster

Imagine two trading threads sharing a portfolio:

```lisp
;; MUTABLE SHARED STATE (dangerous!)
(define global-portfolio {:cash 10000 :positions {}})

(define (execute-trade symbol quantity price)
  ;; Thread 1 might be here
  (let ((current-cash (global-portfolio :cash)))

    ;; ... while Thread 2 reads the SAME cash value

    ;; Now both threads write, LAST WRITE WINS
    (set! global-portfolio
          (assoc global-portfolio :cash
                (- current-cash (* quantity price))))))

;; Thread 1: Buy 100 SOL @ $45 → cash should be 10000 - 4500 = 5500
;; Thread 2: Buy 50 ETH @ $2500 → cash should be 10000 - 125000 = ERROR!
;; But both read cash=10000 simultaneously...

;; Possible outcomes:
;; 1. Thread 1 writes last: cash = 5500 (Thread 2's purchase lost!)
;; 2. Thread 2 writes last: cash = -115000 (negative cash, bankruptcy!)
;; 3. Corrupted data: cash = NaN (memory corruption)

;; THIS IS A HEISENBUG: Appears randomly, hard to reproduce, impossible to debug
```

The problem: **time** becomes a hidden input to your function. The output depends not just on the arguments, but on *when* you call it relative to other threads.

### Immutability: Data That Can't Change

Immutable data structures **never change after creation**. Instead of modifying, we create new versions:

```lisp
;; IMMUTABLE VERSION: Safe by construction
(define (execute-trade-immutable portfolio symbol quantity price)
  "Returns NEW portfolio, original unchanged"

  (let ((cost (* quantity price))
        (new-cash (- (portfolio :cash) cost))
        (old-positions (portfolio :positions))
        (old-quantity (get old-positions symbol 0)))

    ;; Create NEW portfolio (original untouched)
    {:cash new-cash
     :positions (assoc old-positions symbol (+ old-quantity quantity))}))

;; Usage:
(define portfolio-v0 {:cash 10000 :positions {}})

;; Thread 1 creates new portfolio
(define portfolio-v1 (execute-trade-immutable portfolio-v0 "SOL" 100 45))
;; → {:cash 5500 :positions {"SOL" 100}}

;; Thread 2 ALSO starts from portfolio-v0 (not affected by Thread 1)
(define portfolio-v2 (execute-trade-immutable portfolio-v0 "ETH" 50 2500))
;; → {:cash -115000 :positions {"ETH" 50}}  (clearly invalid!)

;; Application logic decides which to keep:
(if (>= (portfolio-v1 :cash) 0)
    portfolio-v1  ;; Valid trade
    portfolio-v0)  ;; Reject, not enough cash
```

Key insight: **No race condition is possible** because:
1. Each thread works with its own copy of data
2. The original data never changes
3. Conflicts become explicit (two different versions exist)
4. Application logic chooses which version to commit

### "But Doesn't Copying Everything Waste Memory?"

No! Modern immutable data structures use **structural sharing**:

```lisp
;; Original portfolio
(define portfolio-v0
  {:cash 10000
   :positions {"SOL" 100 "BTC" 2 "ETH" 10}})

;; Update just cash (positions unchanged)
(define portfolio-v1
  (assoc portfolio-v0 :cash 9000))

;; Memory layout (conceptual):
;; portfolio-v0 → {:cash 10000, :positions → [SOL:100, BTC:2, ETH:10]}
;;                                              ↑
;; portfolio-v1 → {:cash 9000, :positions ------┘ (SHARES the same array!)
;;
;; Only the changed field is copied, not the entire structure!
```

Immutable data structures in languages like Clojure, Haskell, and modern JavaScript achieve:
- **O(log n) updates** (almost as fast as mutation)
- **Constant-time snapshots** (entire history preserved cheaply)
- **Safe concurrent access** (no locks needed)

### Time-Travel Debugging

Immutability enables **undo/redo** and **state snapshots** trivially:

```lisp
;; Portfolio history: array of immutable snapshots
(define (create-portfolio-history initial-portfolio)
  {:states [initial-portfolio]
   :current-index 0})

(define (execute-trade-with-history history symbol quantity price)
  (let ((current (nth (history :states) (history :current-index))))

    (let ((new-portfolio (execute-trade-immutable current symbol quantity price)))

      ;; Append new state to history
      {:states (append (history :states) new-portfolio)
       :current-index (+ (history :current-index) 1)})))

(define (undo history)
  (if (> (history :current-index) 0)
      (assoc history :current-index (- (history :current-index) 1))
      history))

(define (redo history)
  (if (< (history :current-index) (- (length (history :states)) 1))
      (assoc history :current-index (+ (history :current-index) 1))
      history))

;; Usage:
(define hist (create-portfolio-history {:cash 10000 :positions {}}))

(set! hist (execute-trade-with-history hist "SOL" 100 45))
;; States: [{cash:10000}, {cash:5500, SOL:100}]

(set! hist (execute-trade-with-history hist "BTC" 2 45000))
;; States: [{cash:10000}, {cash:5500, SOL:100}, {cash:-84500, SOL:100, BTC:2}]

;; Oops, BTC trade was bad! Undo it:
(set! hist (undo hist))
;; Current index: 1 → {cash:5500, SOL:100}

;; Try different trade:
(set! hist (execute-trade-with-history hist "ETH" 10 2500))
;; States: [..., {cash:5500, SOL:100}, {cash:-19500, SOL:100, ETH:10}]
;;                ↑ can still access this!
```

This is **impossible** with mutable state—once you overwrite data, it's gone forever.

---

## 5.4 Function Composition: Building Complex from Simple

### The Unix Philosophy for Trading

Unix commands succeed because they compose:

```bash
cat trades.csv | grep "SOL" | awk '{sum+=$3} END {print sum}'
```

Each command:
1. Does one thing well
2. Accepts input from previous command
3. Produces output for next command

We can apply this to trading:

```lisp
;; Instead of one giant function:
(define (analyze-portfolio-WRONG prices)
  ;; 200 lines of tangled logic
  ...)

;; Build pipeline of small functions:
(define (analyze-portfolio prices)
  (let ((returns (calculate-returns prices)))
    (let ((filtered (filter-outliers returns)))
      (let ((normalized (normalize filtered)))
        (calculate-sharpe normalized)))))

;; Or using composition:
(define analyze-portfolio
  (compose calculate-sharpe
           normalize
           filter-outliers
           calculate-returns))
```

### Composition Operator

The `compose` function chains functions right-to-left (like mathematical composition):

```lisp
;; Manual composition:
(define (f-then-g x)
  (g (f x)))

;; Generic compose:
(define (compose f g)
  (lambda (x) (f (g x))))

;; Example: Price → Returns → Log Returns → Volatility
(define price-to-vol
  (compose std-dev          ;; 4. Standard deviation
           (compose log           ;; 3. Take log
                    (compose calculate-returns)))) ;; 2. Calculate returns
                    ;; 1. Input: prices

;; Usage:
(define prices [100 105 103 108 110])
(define vol (price-to-vol prices))

;; Step-by-step:
;; 1. calculate-returns([100,105,103,108,110]) → [1.05, 0.98, 1.05, 1.02]
;; 2. log([1.05, 0.98, 1.05, 1.02]) → [0.0488, -0.0202, 0.0488, 0.0198]
;; 3. std-dev([0.0488, -0.0202, 0.0488, 0.0198]) → 0.0314
```

### Indicator Pipelines

Let's build a complete technical analysis pipeline:

```lisp
;; Step 1: Basic transformations
(define (to-returns prices)
  (map (range 1 (length prices))
       (lambda (i) (- (/ (nth prices i) (nth prices (- i 1))) 1))))

(define (to-log-returns returns)
  (map returns log))

(define (zscore values)
  (let ((mean (average values))
        (std (std-dev values)))
    (map values (lambda (v) (/ (- v mean) std)))))

;; Step 2: Windowed operations (for indicators)
(define (windowed-operation data window-size operation)
  "Apply operation to sliding windows of data"
  (map (range (- window-size 1) (length data))
       (lambda (i)
         (operation (slice data (- i window-size -1) (+ i 1))))))

;; Step 3: Build indicators using windowed-operation
(define (sma prices window)
  (windowed-operation prices window average))

(define (bollinger-bands prices window num-std)
  (let ((middle (sma prices window))
        (rolling-std (windowed-operation prices window std-dev)))

    {:middle middle
     :upper (map (range 0 (length middle))
                (lambda (i) (+ (nth middle i) (* num-std (nth rolling-std i)))))
     :lower (map (range 0 (length middle))
                (lambda (i) (- (nth middle i) (* num-std (nth rolling-std i)))))}))

;; Step 4: Compose into complete analysis
(define (analyze-price-action prices)
  ;; Returns: {returns, volatility, bands, ...}
  (let ((returns (to-returns prices))
        (log-returns (to-log-returns returns))
        (vol (std-dev log-returns))
        (bands (bollinger-bands prices 20 2)))

    {:returns returns
     :log-returns log-returns
     :volatility vol
     :annualized-vol (* vol (sqrt 252))
     :bands bands
     :current-price (last prices)
     :current-band-position
       (let ((price (last prices))
             (upper (last (bands :upper)))
             (lower (last (bands :lower))))
         (/ (- price lower) (- upper lower)))}))  ;; 0 = lower band, 1 = upper band

;; Usage:
(define prices [/* 252 days of data */])
(define analysis (analyze-price-action prices))

;; Access results:
(analysis :annualized-vol)  ;; → 0.45 (45% annualized volatility)
(analysis :current-band-position)  ;; → 0.85 (near upper band, overbought?)
```

The pipeline is **declarative**: each step describes *what* to compute, and the composition operator handles *how* to thread data through.

---

## 5.5 Monads: Making Error Handling Composable

### The Problem: Error Handling Breaks Composition

You're fetching market data from multiple exchanges:

```lisp
;; Each fetch might fail (network error, API down, invalid data)
(define (fetch-price symbol exchange)
  ;; Returns price OR null if error
  ...)

;; UGLY: Manual null checks everywhere
(let ((price-binance (fetch-price "SOL" "binance")))
  (if (null? price-binance)
      (log :error "Binance fetch failed")

      (let ((price-coinbase (fetch-price "SOL" "coinbase")))
        (if (null? price-coinbase)
            (log :error "Coinbase fetch failed")

            (let ((arb-opportunity (- price-binance price-coinbase)))
              (if (> arb-opportunity 0.5)
                  (execute-arbitrage "SOL" arb-opportunity)
                  (log :message "No arbitrage")))))))
```

This is **pyramid of doom**—deeply nested conditionals that obscure the actual logic.

### Maybe Monad: Explicit Absence

The Maybe monad makes "might not exist" explicit in the type:

```lisp
;; Maybe type: represents value OR absence
(define (just value)
  {:type "just" :value value})

(define (nothing)
  {:type "nothing"})

;; Check if Maybe has value
(define (is-just? m)
  (= (m :type) "just"))

;; Bind: chain operations that might fail
(define (maybe-bind m f)
  "If m is Just, apply f to its value. If Nothing, short-circuit"
  (if (is-just? m)
      (f (m :value))
      (nothing)))

;; Now we can chain without null checks:
(maybe-bind (fetch-price "SOL" "binance")
  (lambda (price-binance)
    (maybe-bind (fetch-price "SOL" "coinbase")
      (lambda (price-coinbase)
        (let ((arb (- price-binance price-coinbase)))
          (if (> arb 0.5)
              (just (execute-arbitrage "SOL" arb))
              (nothing)))))))

;; If ANY step fails, the entire chain returns Nothing
;; No need for manual null checks at each step!
```

By hand example:

```lisp
;; Success case:
;; fetch-price("SOL", "binance") → Just(105.5)
;; → apply lambda, fetch-price("SOL", "coinbase") → Just(107.2)
;; → apply lambda, calculate arb: 105.5 - 107.2 = -1.7 (not > 0.5)
;; → return Nothing

;; Failure case:
;; fetch-price("SOL", "binance") → Nothing
;; → maybe-bind short-circuits, return Nothing immediately
;; → Second fetch never happens!
```

### Either Monad: Carrying Error Information

Maybe tells us something failed, but not *why*. Either carries error details:

```lisp
;; Either type: success (Right) OR error (Left)
(define (right value)
  {:type "right" :value value})

(define (left error-message)
  {:type "left" :error error-message})

(define (is-right? e)
  (= (e :type) "right"))

;; Bind for Either
(define (either-bind e f)
  (if (is-right? e)
      (f (e :value))
      e))  ;; Propagate error

;; Trade validation pipeline
(define (validate-price order)
  (if (and (> (order :price) 0) (< (order :price) 1000000))
      (right order)
      (left "Price out of range [0, 1000000]")))

(define (validate-quantity order)
  (if (and (> (order :quantity) 0) (< (order :quantity) 1000000))
      (right order)
      (left "Quantity out of range [0, 1000000]")))

(define (validate-balance order account-balance)
  (let ((cost (* (order :price) (order :quantity))))
    (if (>= account-balance cost)
        (right order)
        (left (string-concat "Insufficient balance. Need "
                            (to-string cost)
                            ", have "
                            (to-string account-balance))))))

;; Chain validations
(define (validate-order order balance)
  (either-bind (validate-price order)
    (lambda (o1)
      (either-bind (validate-quantity o1)
        (lambda (o2)
          (validate-balance o2 balance))))))

;; Test cases:
(define good-order {:price 45.5 :quantity 100})
(validate-order good-order 5000)
;; → Right({:price 45.5 :quantity 100})

(define bad-price {:price -10 :quantity 100})
(validate-order bad-price 5000)
;; → Left("Price out of range [0, 1000000]")

(define bad-balance {:price 45.5 :quantity 100})
(validate-order bad-balance 1000)
;; → Left("Insufficient balance. Need 4550, have 1000")
```

The pipeline **short-circuits** on first error:

```
validate-price → PASS
validate-quantity → PASS
validate-balance → FAIL → return Left("Insufficient...")

validate-price → FAIL → return Left("Price...")
(validate-quantity never runs)
(validate-balance never runs)
```

### Railway-Oriented Programming

Visualize Either as two tracks:

```
Input Order
    ↓
[Validate Price] → Success → [Validate Quantity] → Success → [Validate Balance] → Success → Execute Trade
    ↓ Failure                      ↓ Failure                    ↓ Failure              ↓
    Error Track ←─────────────────Error Track ←──────────────Error Track ←────────── Error Track
```

Once on the error track, you stay there until explicitly handled.

---

## 5.6 Practical Example: Complete Backtesting System

Let's build a production-quality backtesting system using all FP principles:

```lisp
;; ============================================================================
;; PURE INDICATOR FUNCTIONS
;; ============================================================================

(define (calculate-sma prices window)
  "Simple Moving Average: average of last N prices"
  (windowed-operation prices window
    (lambda (window-prices)
      (/ (sum window-prices) (length window-prices)))))

(define (calculate-ema prices alpha)
  "Exponential Moving Average: EMA[t] = α*Price[t] + (1-α)*EMA[t-1]"
  (let ((ema-values [(first prices)]))  ;; EMA[0] = Price[0]
    (for (i (range 1 (length prices)))
      (let ((prev-ema (last ema-values))
            (current-price (nth prices i)))
        (let ((new-ema (+ (* alpha current-price)
                         (* (- 1 alpha) prev-ema))))
          (set! ema-values (append ema-values new-ema)))))
    ema-values))

(define (calculate-rsi prices period)
  "Relative Strength Index: momentum indicator (0-100)"
  ;; Calculate price changes
  (let ((changes (map (range 1 (length prices))
                     (lambda (i) (- (nth prices i) (nth prices (- i 1)))))))

    ;; Separate gains and losses
    (let ((gains (map changes (lambda (c) (if (> c 0) c 0))))
          (losses (map changes (lambda (c) (if (< c 0) (abs c) 0)))))

      ;; Calculate average gains and losses
      (let ((avg-gains (calculate-sma gains period))
            (avg-losses (calculate-sma losses period)))

        ;; RSI formula: 100 - (100 / (1 + RS)), where RS = avg-gain / avg-loss
        (map (range 0 (length avg-gains))
             (lambda (i)
               (let ((ag (nth avg-gains i))
                     (al (nth avg-losses i)))
                 (if (= al 0)
                     100  ;; No losses → RSI = 100
                     (- 100 (/ 100 (+ 1 (/ ag al))))))))))))

;; ============================================================================
;; PURE STRATEGY FUNCTIONS (return signals, not side effects)
;; ============================================================================

(define (sma-crossover-strategy fast-period slow-period)
  "Returns function that generates buy/sell signals from SMA crossover"
  (lambda (prices current-index)
    (if (< current-index slow-period)
        "hold"  ;; Not enough data yet

        (let ((recent-prices (slice prices 0 (+ current-index 1))))
          (let ((fast-sma (calculate-sma recent-prices fast-period))
                (slow-sma (calculate-sma recent-prices slow-period)))

            ;; Current values
            (let ((fast-current (last fast-sma))
                  (slow-current (last slow-sma)))

              ;; Previous values
              (let ((fast-prev (nth fast-sma (- (length fast-sma) 2)))
                    (slow-prev (nth slow-sma (- (length slow-sma) 2))))

                ;; Golden cross: fast crosses above slow
                (if (and (> fast-current slow-current)
                        (<= fast-prev slow-prev))
                    "buy"

                    ;; Death cross: fast crosses below slow
                    (if (and (< fast-current slow-current)
                            (>= fast-prev slow-prev))
                        "sell"

                        "hold")))))))))

(define (rsi-mean-reversion-strategy period oversold overbought)
  "Buy when oversold, sell when overbought"
  (lambda (prices current-index)
    (if (< current-index period)
        "hold"

        (let ((recent-prices (slice prices 0 (+ current-index 1))))
          (let ((rsi-values (calculate-rsi recent-prices period)))
            (let ((current-rsi (last rsi-values)))

              (if (< current-rsi oversold)
                  "buy"   ;; Oversold → expect reversion up
                  (if (> current-rsi overbought)
                      "sell"  ;; Overbought → expect reversion down
                      "hold"))))))))

;; ============================================================================
;; PURE BACKTEST ENGINE
;; ============================================================================

(define (backtest-strategy strategy prices initial-capital)
  "Simulate strategy on historical prices. Returns complete trade history."

  (let ((capital initial-capital)
        (position 0)
        (trades [])
        (equity-curve [initial-capital]))

    (for (i (range 0 (length prices)))
      (let ((price (nth prices i))
            (signal (strategy prices i)))

        ;; Execute trades based on signal
        (if (and (= signal "buy") (= position 0))
            ;; Buy with all available capital
            (let ((shares (floor (/ capital price))))
              (if (> shares 0)
                  (do
                    (set! position shares)
                    (set! capital (- capital (* shares price)))
                    (set! trades (append trades {:time i
                                                :type "buy"
                                                :price price
                                                :shares shares
                                                :capital capital})))
                  null))

            ;; Sell entire position
            (if (and (= signal "sell") (> position 0))
                (do
                  (set! capital (+ capital (* position price)))
                  (set! trades (append trades {:time i
                                              :type "sell"
                                              :price price
                                              :shares position
                                              :capital capital}))
                  (set! position 0))
                null))

        ;; Record equity (capital + position value)
        (let ((equity (+ capital (* position price))))
          (set! equity-curve (append equity-curve equity)))))

    ;; Calculate performance metrics
    (let ((final-equity (last equity-curve))
          (total-return (/ (- final-equity initial-capital) initial-capital))
          (returns (calculate-returns equity-curve))
          (sharpe-ratio (/ (average returns) (std-dev returns))))

      {:initial-capital initial-capital
       :final-equity final-equity
       :total-return total-return
       :sharpe-ratio sharpe-ratio
       :trades trades
       :equity-curve equity-curve
       :max-drawdown (calculate-max-drawdown equity-curve)})))

(define (calculate-max-drawdown equity-curve)
  "Maximum peak-to-trough decline"
  (let ((peak (first equity-curve))
        (max-dd 0))

    (for (i (range 1 (length equity-curve)))
      (let ((equity (nth equity-curve i)))
        ;; Update peak
        (if (> equity peak)
            (set! peak equity)
            null)

        ;; Update max drawdown
        (let ((drawdown (/ (- peak equity) peak)))
          (if (> drawdown max-dd)
              (set! max-dd drawdown)
              null))))

    max-dd))

;; ============================================================================
;; USAGE EXAMPLES
;; ============================================================================

;; Load historical data
(define sol-prices [100 102 101 103 105 104 106 108 107 110 112 111 113 115])

;; Test SMA crossover strategy
(define sma-strategy (sma-crossover-strategy 3 5))
(define sma-results (backtest-strategy sma-strategy sol-prices 10000))

(log :message "SMA Strategy Results")
(log :value (sma-results :final-equity))
(log :value (sma-results :total-return))
(log :value (sma-results :sharpe-ratio))
(log :value (length (sma-results :trades)))

;; Test RSI strategy
(define rsi-strategy (rsi-mean-reversion-strategy 14 30 70))
(define rsi-results (backtest-strategy rsi-strategy sol-prices 10000))

(log :message "RSI Strategy Results")
(log :value (rsi-results :final-equity))
(log :value (rsi-results :total-return))

;; Compare strategies
(if (> (sma-results :sharpe-ratio) (rsi-results :sharpe-ratio))
    (log :message "SMA strategy wins")
    (log :message "RSI strategy wins"))
```

**Key properties of this system:**

1. **Pure indicator functions**: Same inputs → same outputs, always
2. **Testable strategies**: Each strategy is a function that can be tested in isolation
3. **Composable**: Easy to combine multiple strategies
4. **Reproducible**: Running the backtest twice gives identical results
5. **No hidden state**: All state is explicit in function parameters and return values

---

## 5.7 Key Takeaways

**Core Principles:**

1. **Pure functions eliminate non-determinism**
   - Same inputs always produce same outputs
   - No side effects mean no hidden dependencies
   - Makes testing and debugging trivial

2. **Immutability prevents race conditions**
   - Data can't change, so threads can't conflict
   - Enables time-travel debugging and undo
   - Uses structural sharing for efficiency

3. **Higher-order functions eliminate repetition**
   - map/filter/reduce replace manual loops
   - Functions compose like LEGO blocks
   - Code becomes declarative (what, not how)

4. **Monads make error handling composable**
   - Maybe/Either prevent null pointer exceptions
   - Railway-oriented programming: errors short-circuit
   - Explicit failure handling in types

**When to Use FP:**

- **Backtesting**: Reproducibility is critical
- **Risk calculations**: Bugs cost millions
- **Concurrent systems**: No locks needed with immutability
- **Complex algorithms**: Composition reduces complexity

**When to Be Pragmatic:**

- **Performance-critical tight loops**: Mutation can be faster (but profile first!)
- **Interfacing with imperative APIs**: Isolate side effects at boundaries
- **Simple scripts**: Don't over-engineer for throwaway code

**The Knight Capital lesson**: Shared mutable state killed a company. Pure functions, immutability, and explicit error handling prevent entire classes of catastrophic bugs. In production trading systems, FP isn't academic purity—it's pragmatic risk management.

**Next Steps:**

Now that we can write **correct, composable code**, we need to model the **randomness** inherent in markets. Chapter 6 introduces stochastic processes—mathematical models of price movements that capture uncertainty while remaining analytically tractable.
