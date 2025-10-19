# OVSM Agent Query Catalog

**Total Queries:** 100
**Format:** OVSM-LISP (`.ovsm` files)
**Purpose:** Demonstrate OVSM language features, provide test cases, and serve as examples for AI agents

---

## 📊 Query Distribution

| Category | Count | Range | Description |
|----------|-------|-------|-------------|
| **Basic** | 25 | 001-025 | Arithmetic, variables, conditionals, helpers |
| **Loops** | 25 | 026-050 | While, for, iteration patterns, loop control |
| **Data Structures** | 25 | 051-075 | Arrays, objects, manipulation, nesting |
| **Advanced** | 25 | 076-100 | Algorithms, blockchain, finance, complex logic |

---

## 📁 Directory Structure

```
agent_queries/
├── README.md           # Usage guide
├── QUERY_CATALOG.md    # This file
├── basic/              # 001-025: Basic operations
├── loops/              # 026-050: Loop patterns
├── data_structures/    # 051-075: Data manipulation
└── advanced/           # 076-100: Advanced algorithms
```

---

## 🎯 Basic Queries (001-025)

### Arithmetic Operations
- **001** Simple addition (42 + 58)
- **002** Variadic addition (sum 1-10)
- **003** Multiplication (12 * 15)
- **004** Division (144 / 12)
- **005** Modulo (17 % 5)
- **006** Nested arithmetic ((5+3) * (10-2))
- **023** Arithmetic precedence (2 + 3*4)

### Variables & Constants
- **007** Variable definition
- **008** Variable mutation
- **009** Constant definition (PI)

### Conditionals
- **010** Simple if statement
- **011** Equality comparison
- **012** Inequality comparison
- **013** Logical NOT
- **014** Null check
- **017** Nested if (grade calculator)
- **018** When conditional
- **019** Cond multi-way conditional

### Strings & Arrays
- **015** String concatenation
- **020** Range creation
- **024** Empty check
- **025** Array length

### Helpers
- **016** Sequential execution (do)
- **021** Current timestamp
- **022** Log messages

---

## 🔄 Loop Queries (026-050)

### While Loops
- **026** Simple while (count 0-3)
- **027** While sum (1-10)
- **031** While countdown (10-0)
- **036** **While with if-then-else (THE CRITICAL FIX!)** ⭐

### For Loops
- **028** For with range
- **029** For with array
- **030** Nested for loops
- **032** For over string characters
- **033** Accumulator pattern
- **034** Conditional loop

### Loop Patterns
- **035** Early termination
- **037** Counting evens
- **038** Max finder
- **039** Min finder
- **040** Double values (map pattern)
- **041** Filter positives
- **046** Array contains check
- **047** Count occurrences

### Advanced Loop Algorithms
- **042** Sum of squares
- **043** Fibonacci sequence
- **044** Reverse array
- **045** String builder
- **048** Alternating sum
- **049** Cumulative product
- **050** Nested sum (matrix-like)

---

## 📦 Data Structure Queries (051-075)

### Arrays
- **051** Create array
- **053** Array first element
- **054** Array last element
- **055** Nested array
- **057** Array concatenation
- **058** Mixed-type array
- **059** Empty array
- **061** Array push pattern
- **063** Array size check
- **065** Array membership
- **066** Array slice
- **070** Array initialization
- **071** Range generation
- **072** Tuple-like array
- **073** Array prepend

### Objects
- **052** Create object
- **056** Object property access
- **060** Empty object
- **062** Nested object
- **064** Object keys
- **067** Merge objects
- **074** Object update

### Complex Structures
- **068** Array of objects
- **069** Multidimensional array
- **075** Deep nested structure

---

## 🚀 Advanced Queries (076-100)

### Mathematical Algorithms
- **076** Factorial calculation
- **077** Fibonacci (iterative)
- **078** Prime number check
- **079** GCD calculation (Euclidean algorithm)
- **080** Bubble sort pass
- **081** Binary search setup
- **088** Distance formula
- **089** Quadratic formula
- **090** Matrix sum

### Statistical & Financial
- **082** Average calculator
- **083** Variance calculation
- **084** Moving average
- **086** Percentage calculator
- **087** Compound interest
- **098** Loan amortization
- **099** RSI indicator

### String Processing
- **091** String reversal
- **092** Palindrome check
- **093** Word count

### Blockchain & Crypto
- **094** Blockchain timestamp
- **095** Hash simulation
- **096** Merkle root simulation
- **097** Exponential growth
- **100** Liquidity pool calculation

### Time & Growth
- **085** Time difference
- **097** Exponential growth

---

## 🌟 Featured Queries

### Query #036: The Critical Fix ⭐
**File:** `loops/036_while_with_if.ovsm`

This query demonstrates the **critical parser bug fix** - IF-THEN-ELSE inside a WHILE loop now works correctly!

```lisp
(define done false)
(define count 0)
(while (not done)
  (if (== count 0)
      (set! count 1)
      (set! count 2))
  (set! done true))  ;; ✅ THIS LINE EXECUTES!
count
```

**Expected:** `1`
**Why it matters:** This was impossible in Python-style OVSM due to parser ambiguity. LISP syntax fixes it!

### Query #077: Fibonacci
**File:** `advanced/077_fibonacci_recursive_iter.ovsm`

Demonstrates iterative Fibonacci with array manipulation:

```lisp
(define fib [0 1])
(for (i (range 2 10))
  (set! fib (+ fib [(+ (last fib) (last (init fib)))])))
(last fib)
```

**Expected:** `55` (10th Fibonacci number)

### Query #100: DeFi Calculation
**File:** `advanced/100_liquidity_pool_calc.ovsm`

Real-world DeFi liquidity pool value calculation:

```lisp
(define reserve-a 1000000)
(define reserve-b 2000000)
(define total-supply 1000)
(define user-balance 10)
(* (/ user-balance total-supply) reserve-a)
```

**Expected:** `10000` (LP token value)

---

## 🧪 Usage Examples

### Run a Single Query
```bash
osvm ovsm run agent_queries/basic/001_simple_addition.ovsm
```

### Run All Basic Queries
```bash
for file in agent_queries/basic/*.ovsm; do
    echo "Running $file..."
    osvm ovsm run "$file"
done
```

### Test a Category
```bash
# Test all loop queries
cargo test --test lisp_e2e_tests

# Or run specific tests
for query in agent_queries/loops/*.ovsm; do
    osvm ovsm run "$query" && echo "✅ PASS" || echo "❌ FAIL"
done
```

---

## 📝 Query File Format

Each query file follows this standard format:

```lisp
;; Query: [Human-readable description]
;; Category: [Basic|Loops|Data Structures|Advanced]
;; Expected: [Expected result]

[OVSM code here]
```

Example:
```lisp
;; Query: What is 42 + 58?
;; Category: Basic
;; Expected: 100

(+ 42 58)
```

---

## 🎓 Learning Path

### Beginner (Start Here)
1. Basic 001-010: Arithmetic and variables
2. Basic 011-015: Comparisons and strings
3. Basic 016-025: Conditionals and helpers

### Intermediate
1. Loops 026-035: Simple loops
2. Data Structures 051-063: Arrays and objects
3. Loops 036-045: Advanced loop patterns

### Advanced
1. Loops 046-050: Complex iterations
2. Data Structures 064-075: Nested structures
3. Advanced 076-085: Algorithms

### Expert
1. Advanced 086-095: Financial & crypto
2. Advanced 096-100: DeFi & blockchain

---

## 🔍 Query Search Index

### By Feature
- **Variadic operators:** 002, 023
- **Helper functions:** 013, 014, 020, 021, 022, 024, 025
- **Sequential execution:** 016
- **Multi-way conditionals:** 019
- **Loop control:** 035
- **The critical fix:** 036 ⭐
- **Fibonacci:** 043, 077
- **Sorting:** 080
- **Prime numbers:** 078
- **String manipulation:** 045, 091, 092, 093
- **Blockchain:** 094, 095, 096, 100
- **Financial:** 086, 087, 098, 099

### By Complexity
- **Trivial (1 line):** 001-006, 011-015, 051, 059, 060
- **Simple (2-5 lines):** 007-010, 020-025, 052-058
- **Medium (6-15 lines):** 026-050, 061-075, 076-090
- **Complex (15+ lines):** 091-100

---

## 📊 Statistics

- **Total lines of code:** ~500 lines
- **Average query length:** ~5 lines
- **Shortest query:** 1 line (001-006)
- **Longest query:** ~15 lines (099, 100)
- **Comments:** 300+ lines (3 per query)

---

## 🚀 Future Extensions

Potential additions for v2:
- Error handling examples (try/catch)
- Lambda/function examples
- Macro examples
- Recursive function examples
- Blockchain RPC integration examples
- Real Solana program interaction

---

**Generated:** October 19, 2025
**OVSM Version:** 1.0 (LISP syntax)
**Status:** ✅ Complete (100/100 queries)
**Location:** `/home/larp/larpdevs/osvm-cli/crates/ovsm/agent_queries/`
