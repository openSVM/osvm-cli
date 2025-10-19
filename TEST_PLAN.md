# OSVM LISP System Test Plan

## Overview
This test plan validates the end-to-end natural language → LISP execution pipeline.

**Last Updated:** 2025-10-19
**Status:** Production Ready
**Critical Fixes Applied:** For loops, RPC versioned transactions, pagination, scoping

---

## Test Categories

### 1. LISP Core Functionality Tests

#### 1.1 Basic LISP Execution
**Command:** `./target/release/osvm ovsm eval '<code>'`

**Test Cases:**
```bash
# Basic arithmetic
./target/release/osvm ovsm eval '(+ 1 2 3)'
Expected: 6

# Variables
./target/release/osvm ovsm eval '(define x 10) (+ x 5)'
Expected: 15

# For loops (critical fix)
./target/release/osvm ovsm eval '(define arr [1 2 3]) (define sum 0) (for (x arr) (set! sum (+ sum x))) sum'
Expected: 6

# While loops
./target/release/osvm ovsm eval '(define i 0) (while (< i 5) (set! i (+ i 1))) i'
Expected: 5

# Helper functions (and, or, min, max)
./target/release/osvm ovsm eval '(and true true false)'
Expected: false

./target/release/osvm ovsm eval '(or false false true)'
Expected: true

./target/release/osvm ovsm eval '(min 5 2 8 1 9)'
Expected: 1

./target/release/osvm ovsm eval '(max 5 2 8 1 9)'
Expected: 9
```

**Pass Criteria:** All expressions return expected results

---

#### 1.2 Script Execution
**Command:** `./target/release/osvm ovsm run <script.ovsm>`

**Test Script:** `/tmp/simple_count.ovsm`
```lisp
(const PUMPFUN_PROGRAM "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(define now_time (now))
(define cutoff (- now_time 60))
(define sigs (getSignaturesForAddress PUMPFUN_PROGRAM {:limit 1000}))
(define count 0)
(for (sig sigs)
  (when (>= (. sig blockTime) cutoff)
    (set! count (+ count 1))))
count
```

**Test Cases:**
```bash
# 1 minute query (no pagination needed)
./target/release/osvm ovsm run /tmp/simple_count.ovsm
Expected: Numeric result (400-800 range)
Execution time: < 2 seconds
```

**Pass Criteria:**
- Returns valid count
- No errors
- Execution time reasonable

---

### 2. Natural Language Query Tests

#### 2.1 Short Time Window Queries (< 2 minutes)
**Expected Behavior:** No pagination, single RPC call

**Test Cases:**
```bash
# Test 1: Basic 1-minute query
./target/release/osvm "how many txs had pumpfun program in last 1 minute?"
Expected Output:
  - ✅ OVSM execution completed
  - Numeric result (400-800 range)
  - Execution time: 3-8 seconds

# Test 2: Alternative phrasing
./target/release/osvm "pumpfun last minute"
Expected: Same behavior as Test 1

# Test 3: Different wording
./target/release/osvm "count pumpfun transactions 1 min"
Expected: Same behavior as Test 1
```

**Pass Criteria:**
- Code extracted successfully
- Execution completes without errors
- Result is numeric (not null, not explanation text)
- No pagination used (check debug output)

---

#### 2.2 Long Time Window Queries (>= 2 minutes)
**Expected Behavior:** Pagination required, multiple RPC calls

**Test Cases:**
```bash
# Test 1: 10-minute query
./target/release/osvm "how many txs had pumpfun program in last 10 minute?"
Expected Output:
  - ✅ OVSM execution completed
  - Numeric result (5000-8000 range)
  - Execution time: 8-15 seconds
  - Multiple RPC calls visible in debug

# Test 2: Verify pagination is working
./target/release/osvm -vv "pumpfun last 10 minutes" 2>&1 | grep "define batch"
Expected: Should see batch variable being set multiple times

# Test 3: Compare with manual script
./target/release/osvm ovsm run /tmp/pumpfun_10min_correct.ovsm
Expected: Similar count to natural language query (± 10%)
```

**Pass Criteria:**
- Uses pagination (visible in debug output)
- Result > 1000 (proving pagination worked)
- Result is NOT exactly 1000 (would indicate pagination failure)
- Execution time proportional to data volume

---

#### 2.3 Edge Cases

**Test Cases:**
```bash
# Test 1: Very short window (30 seconds)
./target/release/osvm "pumpfun last 30 seconds"
Expected: Result < 500

# Test 2: Ambiguous query
./target/release/osvm "pumpfun transactions"
Expected: Either asks for clarification OR defaults to reasonable window

# Test 3: Typo in program name
./target/release/osvm "pump fun last minute"
Expected: Still recognizes "pumpfun" OR uses correct address

# Test 4: Different program
./target/release/osvm "how many jupiter transactions last minute"
Expected: Uses Jupiter address, returns result
```

**Pass Criteria:**
- Handles variations gracefully
- No crashes
- Reasonable defaults for ambiguous inputs

---

### 3. Code Generation Quality Tests

#### 3.1 Scoping Rules Compliance
**Purpose:** Verify AI doesn't create scoping bugs

**Test:** Extract generated code and inspect
```bash
./target/release/osvm -vv "pumpfun 10 min" 2>&1 | grep -A50 "📝 Extracted OVSM code:"
```

**Check For:**
- ✅ All variables defined at the top
- ✅ Only `set!` used inside `while`/`when`/`if` blocks
- ❌ NO `define` inside `when` blocks
- ❌ NO `define` inside `if` branches
- ❌ NO `define` inside `while` loops

**Pass Criteria:** No scoping violations in generated code

---

#### 3.2 Pagination Pattern Compliance
**Purpose:** Verify AI generates correct pagination

**Test:** Inspect generated code for 10-minute query
```bash
./target/release/osvm -vv "pumpfun last 10 minutes" 2>&1 | grep -A60 "📝 Extracted"
```

**Check For:**
- ✅ `define batch []` at top
- ✅ `define batch_size 0` at top (or uses COUNT inline)
- ✅ `define before null` at top
- ✅ `define continue true` at top
- ✅ `define last_sig null` at top (if used)
- ✅ `while continue` loop
- ✅ Pagination cursor update: `(set! before (. last_sig signature))`
- ✅ Stop condition checking oldest blockTime

**Pass Criteria:** Matches pagination example from prompts

---

### 4. Performance Tests

#### 4.1 Execution Time Benchmarks

**Test Cases:**
```bash
# 1 minute query
time ./target/release/osvm "pumpfun last 1 minute"
Expected: < 8 seconds total

# 10 minute query
time ./target/release/osvm "pumpfun last 10 minutes"
Expected: < 20 seconds total

# Manual script (baseline)
time ./target/release/osvm ovsm run /tmp/simple_count.ovsm
Expected: < 2 seconds
```

**Pass Criteria:**
- Natural language overhead < 5 seconds
- Manual script execution < 2 seconds
- Pagination doesn't cause timeout

---

#### 4.2 Reliability Test (Multiple Runs)

**Test:** Run same query 5 times
```bash
for i in {1..5}; do
  echo "=== Run $i ==="
  ./target/release/osvm "pumpfun last 1 minute" 2>&1 | grep -E "✅ OVSM Plan Executed|Result:"
  sleep 2
done
```

**Pass Criteria:**
- 5/5 successful executions
- 5/5 numeric results (no nulls)
- Results within expected range (fluctuation is normal)
- No extraction failures

---

### 5. Integration Tests

#### 5.1 RPC Integration
**Purpose:** Verify RPC calls work correctly

**Test Cases:**
```bash
# Test 1: getSignaturesForAddress with limit
./target/release/osvm ovsm eval '(define sigs (getSignaturesForAddress "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P" {:limit 10})) (COUNT sigs)'
Expected: 10

# Test 2: Signature objects have blockTime
./target/release/osvm ovsm eval '(define sigs (getSignaturesForAddress "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P" {:limit 1})) (define sig ([] sigs 0)) (. sig blockTime)'
Expected: Unix timestamp (large number)

# Test 3: Pagination cursor
./target/release/osvm ovsm eval '(define sigs (getSignaturesForAddress "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P" {:limit 5})) (define last ([] sigs 4)) (. last signature)'
Expected: Signature string
```

**Pass Criteria:** All RPC calls return expected data

---

#### 5.2 AI Service Integration
**Purpose:** Verify AI generates valid code

**Test:** Check AI response structure
```bash
./target/release/osvm -vv "test query" 2>&1 | grep "📥 OSVM AI Response"
```

**Check For:**
- ✅ Response starts with `[TIME:...] [CONFIDENCE:...]`
- ✅ Contains `Main Branch:` section
- ✅ Contains triple backticks with `lisp` tag
- ✅ Code is syntactically valid LISP

**Pass Criteria:** AI response follows expected format

---

### 6. Regression Tests

#### 6.1 Critical Bug Fixes Verification

**Test Cases:**
```bash
# Test 1: For loops work (was broken before)
./target/release/osvm ovsm eval '(define arr [1 2 3]) (for (x arr) (log :message x)) null'
Expected: Logs 1, 2, 3 (no parse errors)

# Test 2: getTransaction supports versioned transactions
./target/release/osvm ovsm eval '(define sigs (getSignaturesForAddress "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P" {:limit 1})) (define sig ([] sigs 0)) (getTransaction (. sig signature))'
Expected: Transaction object (no "version not supported" error)

# Test 3: Pagination example from prompts works
./target/release/osvm ovsm run /tmp/pumpfun_10min_correct.ovsm
Expected: Numeric result > 1000

# Test 4: Helper functions work
cargo test -p ovsm --lib -- eval_and
cargo test -p ovsm --lib -- eval_or
cargo test -p ovsm --lib -- eval_min
cargo test -p ovsm --lib -- eval_max
Expected: All tests pass
```

**Pass Criteria:** All previously broken features now work

---

### 7. Test Scripts Reference

#### 7.1 Simple Count (1 minute)
**Location:** `/tmp/simple_count.ovsm`
```lisp
(const PUMPFUN_PROGRAM "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(define now_time (now))
(define cutoff (- now_time 60))
(define sigs (getSignaturesForAddress PUMPFUN_PROGRAM {:limit 1000}))
(define count 0)
(for (sig sigs)
  (when (>= (. sig blockTime) cutoff)
    (set! count (+ count 1))))
count
```

#### 7.2 Paginated Count (10 minutes)
**Location:** `/tmp/pumpfun_10min_correct.ovsm`
```lisp
(const PUMPFUN_ADDR "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P")
(define cutoff (- (now) 600))
(define all_count 0)
(define before null)
(define continue true)
(define batch [])
(define batch_size 0)

(while continue
  (set! batch
    (if (== before null)
        (getSignaturesForAddress PUMPFUN_ADDR {:limit 1000})
        (getSignaturesForAddress PUMPFUN_ADDR {:limit 1000 :before before})))

  (set! batch_size (COUNT batch))

  (for (sig batch)
    (when (>= (. sig blockTime) cutoff)
      (set! all_count (+ all_count 1))))

  (when (or (== batch_size 0)
            (and (> batch_size 0) (< (. ([] batch (- batch_size 1)) blockTime) cutoff)))
    (set! continue false))

  (when (and continue (> batch_size 0))
    (set! before (. ([] batch (- batch_size 1)) signature))))

(log :message "Total PumpFun txs in last 10 minutes (with pagination):" :value all_count)
all_count
```

---

## Success Metrics

### Overall System Health
- ✅ **90%+ Success Rate:** Natural language queries execute successfully
- ✅ **No Null Results:** Scoping bugs eliminated
- ✅ **Performance:** Queries complete in < 20 seconds
- ✅ **Accuracy:** Results match manual scripts (± 10%)

### Code Quality
- ✅ **Scoping Compliance:** No `define` in `when`/`if`/`while` blocks
- ✅ **Pagination:** Used for queries > 2 minutes
- ✅ **Efficiency:** No unnecessary `getTransaction` calls

### Test Coverage
- ✅ **Unit Tests:** 19/19 LISP core tests passing
- ✅ **Integration:** Natural language → result works
- ✅ **Regression:** All fixed bugs stay fixed

---

## Quick Smoke Test

**Run this to verify system is working:**
```bash
#!/bin/bash
echo "🧪 OSVM Quick Smoke Test"
echo ""

echo "Test 1: LISP eval..."
RESULT=$(./target/release/osvm ovsm eval '(+ 1 2 3)')
[[ "$RESULT" == *"6"* ]] && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 2: For loops..."
RESULT=$(./target/release/osvm ovsm eval '(define arr [1 2 3]) (define s 0) (for (x arr) (set! s (+ s x))) s')
[[ "$RESULT" == *"6"* ]] && echo "✅ PASS" || echo "❌ FAIL"

echo "Test 3: Natural language (1 min)..."
RESULT=$(./target/release/osvm "pumpfun last 1 minute" 2>&1 | grep -E "^[0-9]+$")
[[ ! -z "$RESULT" ]] && echo "✅ PASS (result: $RESULT)" || echo "❌ FAIL"

echo ""
echo "🎉 Smoke test complete!"
```

**Pass Criteria:** 3/3 tests pass

---

## Known Issues

### Intermittent Failures
- **Issue:** AI occasionally generates code with scoping violations
- **Mitigation:** Improved prompts with explicit examples
- **Workaround:** Re-run query or use manual script
- **Status:** Monitoring (should be < 10% failure rate)

### Variable Results
- **Issue:** Transaction counts fluctuate due to real-time data
- **Expected:** ±10% variance between runs is normal
- **Not a bug:** This reflects actual blockchain activity

---

## Maintenance

### When to Update Tests
1. **New LISP features added** → Add unit tests
2. **AI prompts changed** → Re-run code quality tests
3. **RPC endpoints changed** → Update integration tests
4. **Performance degradation** → Investigate and update benchmarks

### Test Execution Schedule
- **Pre-commit:** Quick smoke test (3 tests)
- **Pre-release:** Full test suite
- **Weekly:** Reliability test (5 runs)
- **After prompt changes:** Code quality tests

---

## Appendix: Debug Commands

```bash
# See full AI response
./target/release/osvm -vv "query" 2>&1 | less

# Check extraction
./target/release/osvm -vv "query" 2>&1 | grep -A50 "📝 Extracted OVSM code:"

# See execution trace
./target/release/osvm -vv "query" 2>&1 | grep "DEBUG:"

# Verify RPC calls
./target/release/osvm -vv "query" 2>&1 | grep "Registered RPC bridge"

# Check parse success
./target/release/osvm -vv "query" 2>&1 | grep "Found simplified format"

# Performance timing
time ./target/release/osvm "query"
```

---

**Test Plan Version:** 1.0
**Last Validated:** 2025-10-19
**Status:** ✅ Production Ready
