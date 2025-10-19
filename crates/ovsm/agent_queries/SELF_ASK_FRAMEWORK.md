# Self-Ask and Refine Framework for OVSM Queries

## Overview

This framework enables iterative refinement of OVSM queries through self-questioning, validation, and improvement cycles.

## Self-Ask Methodology

### Question Categories

1. **Correctness Questions**
   - Does this query produce the expected output?
   - Are there edge cases not covered?
   - Are the types correct?
   - Does it handle null/empty inputs?

2. **Clarity Questions**
   - Is the query description accurate?
   - Are variable names descriptive?
   - Is the expected output clearly defined?
   - Would a beginner understand this?

3. **Completeness Questions**
   - Does it demonstrate all relevant features?
   - Are there missing error conditions?
   - Should it include multiple approaches?
   - Are there related queries that should exist?

4. **Optimization Questions**
   - Is this the most efficient approach?
   - Can it be simplified without losing clarity?
   - Are there unnecessary operations?
   - Could it be more idiomatic?

5. **Educational Questions**
   - What does this teach?
   - What common mistakes does it prevent?
   - Does it build on previous queries?
   - What query should come next?

## Refinement Process

### Phase 1: Self-Ask
```
For each query, ask:
1. What am I trying to demonstrate?
2. What could go wrong?
3. What might confuse a reader?
4. How could this be clearer?
5. What variations should exist?
```

### Phase 2: Validate
```
For each query:
1. Parse check: Does it parse correctly?
2. Execute check: Does it run without errors?
3. Output check: Does it match expected output?
4. Edge case check: What breaks it?
5. Style check: Does it follow conventions?
```

### Phase 3: Refine
```
For each issue found:
1. Document the problem
2. Propose 2-3 solutions
3. Evaluate trade-offs
4. Implement best solution
5. Re-validate
```

### Phase 4: Expand
```
Create related queries:
1. Simplified version (if complex)
2. Enhanced version (if basic)
3. Alternative approach
4. Error handling version
5. Edge case version
```

## Scoring System

### Query Quality Score (0-100)

**Correctness (40 points)**
- Parses correctly: 10 pts
- Executes without error: 10 pts
- Produces expected output: 10 pts
- Handles edge cases: 10 pts

**Clarity (30 points)**
- Clear description: 10 pts
- Descriptive names: 10 pts
- Good comments: 10 pts

**Educational Value (30 points)**
- Teaches a concept: 10 pts
- Shows best practice: 10 pts
- Avoids bad patterns: 10 pts

### Scoring Examples

**Low Score (40/100):**
```lisp
;; Query: Do stuff
;; Expected: number

(define x 5)
(+ x 3)
```
- ❌ Vague description (-10)
- ❌ Non-descriptive name "x" (-10)
- ❌ No clear teaching point (-20)

**High Score (95/100):**
```lisp
;; Query: Calculate factorial of 5 using accumulator pattern
;; Category: Advanced
;; Expected: 120
;; Demonstrates: Loop accumulator, range iteration, mutation

(define n 5)
(define factorial 1)

;; Multiply factorial by each number from 1 to n
(for (i (range 1 (+ n 1)))
  (set! factorial (* factorial i)))

factorial  ;; Returns 120
```
- ✅ Clear description (+10)
- ✅ Descriptive names (+10)
- ✅ Inline comments (+10)
- ✅ Teaches accumulator pattern (+10)
- ✅ Shows range iteration (+10)

## Refinement Patterns

### Pattern 1: Add Context
**Before:**
```lisp
(+ 1 2 3)
```

**After:**
```lisp
;; Demonstrate variadic addition operator
;; OVSM operators accept multiple arguments unlike Python-style
(define numbers-to-sum [1 2 3])
(+ 1 2 3)  ;; Can add any number of operands
```

### Pattern 2: Add Error Handling
**Before:**
```lisp
(/ 10 2)
```

**After:**
```lisp
;; Safe division with zero-check
(define numerator 10)
(define denominator 2)

(if (== denominator 0)
    (do
      (log :message "Error: Division by zero")
      null)
    (/ numerator denominator))
```

### Pattern 3: Add Edge Cases
**Before:**
```lisp
(define nums [1 2 3])
(length nums)
```

**After:**
```lisp
;; Test length on various inputs
(do
  (log :message (length [1 2 3]))      ;; 3
  (log :message (length []))           ;; 0
  (log :message (length [[1] [2]]))    ;; 2 (nested)
  (length [1 2 3 4 5]))                ;; Return 5
```

### Pattern 4: Add Alternatives
**Before:**
```lisp
;; Sum array
(define sum 0)
(for (n [1 2 3 4 5])
  (set! sum (+ sum n)))
sum
```

**After (showing multiple approaches):**
```lisp
;; Approach 1: Loop accumulator
(define sum 0)
(for (n [1 2 3 4 5])
  (set! sum (+ sum n)))

;; Approach 2: Reduce pattern (when available)
;; (reduce + 0 [1 2 3 4 5])

;; Approach 3: Variadic operator
(+ 1 2 3 4 5)

sum  ;; Returns 15
```

### Pattern 5: Add Teaching Comments
**Before:**
```lisp
(while (< x 10)
  (set! x (+ x 1)))
```

**After:**
```lisp
;; While loop executes as long as condition is true
(define x 0)

(while (< x 10)
  ;; Loop body: increments x by 1 each iteration
  (set! x (+ x 1)))

;; After loop: x = 10 (condition became false)
x
```

## Self-Ask Questions Template

For each query, ask yourself:

```markdown
## Query Self-Assessment

### Correctness
Q1: Does it parse without errors?
Q2: Does it execute correctly?
Q3: Does output match expected?
Q4: What edge cases exist?
Q5: What could break it?

### Clarity
Q6: Is the description precise?
Q7: Are variable names clear?
Q8: Would a beginner understand?
Q9: Are there enough comments?
Q10: Is the expected output obvious?

### Completeness
Q11: What features does it demonstrate?
Q12: What features are missing?
Q13: Should there be variations?
Q14: Are error cases handled?
Q15: Does it need more context?

### Optimization
Q16: Is there a simpler approach?
Q17: Are there redundant operations?
Q18: Is it idiomatic OVSM?
Q19: Could performance be better?
Q20: Is the code too clever?

### Educational
Q21: What does this teach?
Q22: What prerequisites exist?
Q23: What comes next?
Q24: What mistakes does it prevent?
Q25: How does it compare to alternatives?
```

## Refinement Checklist

Before finalizing a query:

- [ ] Parses correctly in OVSM
- [ ] Executes without runtime errors
- [ ] Produces expected output
- [ ] Has descriptive title
- [ ] Has accurate category
- [ ] Has clear expected result
- [ ] Uses descriptive variable names
- [ ] Includes helpful comments
- [ ] Demonstrates clear concept
- [ ] Avoids anti-patterns
- [ ] Handles common edge cases
- [ ] Follows OVSM idioms
- [ ] Has appropriate complexity for category
- [ ] Teaches transferable knowledge
- [ ] Includes "why" not just "what"

## Query Evolution Tracking

Track how queries evolve:

```
Version 1 (Initial):
- Basic implementation
- Minimal comments
- Single approach

Version 2 (Refined):
- Added edge cases
- Added descriptive names
- Added inline comments

Version 3 (Enhanced):
- Added alternative approaches
- Added error handling
- Added teaching comments
- Added related concepts

Version 4 (Optimized):
- Simplified where possible
- Removed redundancy
- Improved idioms
- Better educational flow
```

## Success Metrics

A refined query should:
- Score ≥85/100 on quality rubric
- Pass all validation checks
- Teach exactly one clear concept
- Use idiomatic OVSM patterns
- Include enough context to be self-contained
- Avoid requiring external knowledge
- Build on previous queries progressively
- Anticipate and prevent common errors

---

**Next Steps:**
1. Apply self-ask process to all 100 queries
2. Generate refinement scores
3. Create enhanced versions of low-scoring queries
4. Build progression graph showing query dependencies
5. Create auto-validation test suite
