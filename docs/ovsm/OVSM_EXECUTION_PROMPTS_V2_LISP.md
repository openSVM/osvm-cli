# OVSM Execution Prompts V2 - LISP Syntax

**Version:** 2.0
**Date:** October 19, 2025
**Syntax:** LISP/S-Expression Only

When executing an OVSM plan, you need to call the AI at different stages for decisions. This document provides prompt templates using LISP syntax.

---

## ⚠️ Breaking Change

**Python-style syntax has been removed.** All prompts and examples now use LISP/S-expression syntax exclusively.

---

## Execution Flow

```
1. Initial Planning → AI generates OVSM plan (LISP syntax)
2. Execute Main Branch → Run tools
3. Decision Point Reached → AI makes decision based on results
4. Execute Branch → Run tools (LISP syntax)
5. Another Decision Point → AI decides again
6. Final Action → Complete
```

---

## Prompt Template 1: Initial Planning

**When**: User asks a research question
**Purpose**: Generate the complete OVSM plan using LISP syntax

```
[SYSTEM PROMPT - loaded once]
Use OVSM V2 with LISP/S-expression syntax exclusively.

[USER]
Research Question: {user_question}

Context:
- Available tools: {list_of_available_tools}
- Time constraint: {max_time or "none"}
- Cost constraint: {max_cost or "none"}

Please provide an OVSM plan using LISP syntax for this research.

Requirements:
- Use (define var value) for variables
- Use (tool-name arg1 arg2) for tool calls
- Use (for (item collection) ...) for loops
- Use (if condition then else) for conditionals
- Include inline comments with ;;
```

**AI Response Example** (LISP syntax):
```lisp
;; Research Plan: Analyze recent transaction patterns

[TIME: 2-3 minutes] [COST: Low] [CONFIDENCE: 85%]

**Available Tools:**
- getSlot
- getBlock
- getSignaturesForAddress
- MAP, FILTER, AVG

**Main Branch:**
;; Get recent blockchain data
(define current-slot (getSlot))
(define recent-block (getBlock :slot current-slot))

;; Extract transaction fees
(define transactions (get recent-block :transactions))
(define fees (MAP transactions :fn (lambda (tx) (get tx :fee))))

;; Calculate average
(define avg-fee (AVG fees))
(define total-txs (length transactions))

**Decision Point:** Check if fees are unusually high
  BRANCH A (avg-fee > 5000):
    ;; High fees detected - analyze further
    (define fee-stddev (STDDEV fees))
    (log :message "High fee environment detected")
  BRANCH B (else):
    ;; Normal fees
    (log :message "Normal fee environment")

**Action:** Return fee analysis with recommendations
```

---

## Prompt Template 2: Decision Point Evaluation

**When**: Reached a DECISION point and need to choose a branch
**Purpose**: AI analyzes results and selects appropriate branch

```
[SYSTEM PROMPT - already loaded]
Continue using OVSM V2 LISP syntax.

[CONTEXT]
Original Question: {original_question}
Current OVSM Plan: {the_ovsm_plan}

[EXECUTION STATUS]
We have executed the Main Branch and reached a Decision Point.

Decision Point: {decision_description}

Available Branches:
{list_each_branch_with_conditions}

[TOOL RESULTS]
Tool calls executed so far (LISP syntax):
(getSlot) → 245123456
(getBlock :slot 245123456) → {...block_data...}
(MAP transactions ...) → [2500, 3200, 4800, ...]
(AVG fees) → 6420

Current Variables (LISP bindings):
current-slot = 245123456
recent-block = {...}
transactions = [{...}, {...}, ...]
fees = [2500, 3200, 4800, ...]
avg-fee = 6420
total-txs = 342

[QUESTION]
Based on these results, which branch should we execute?

Evaluate each branch condition using LISP semantics:
- BRANCH A condition: (> avg-fee 5000)
- BRANCH B condition: else

Respond with:
1. Selected branch (A, B, C, etc.)
2. Reasoning with LISP evaluation
3. Updated confidence level if applicable
```

**AI Response Example**:
```
Selected Branch: BRANCH A

Reasoning: The condition (> avg-fee 5000) evaluates to true because:
- avg-fee = 6420
- (> 6420 5000) = true

Therefore, high fees detected. Should execute deeper analysis.

LISP evaluation trace:
  (> avg-fee 5000)
  (> 6420 5000)
  true

Updated Confidence: 90% (increased - clear high-fee pattern)
```

---

## Prompt Template 3: Error Handling Decision

**When**: A tool call fails or returns an error
**Purpose**: AI decides which error handling path to take

```
[SYSTEM PROMPT - already loaded]
Using OVSM V2 LISP syntax for error handling.

[ERROR CONTEXT]
Original OVSM Plan: {plan}
Current Execution Point: {location_in_plan}

[ERROR DETAILS]
Failed Expression: (getBlock :slot {slot_number})
Error Type: {error_type}
Error Message: "{error_message}"

Available Error Handlers:
1. RETRY with backoff
   (retry-with-backoff :max-attempts 3 :delay 1000)

2. FALLBACK to alternative
   (getBlockWithAlternative :slot {slot_number})

3. SKIP and continue
   (continue-without-block-data)

4. ABORT execution
   (abort-with-error :message "...")

Current Context:
{current_variables_in_lisp_format}

[QUESTION]
How should we handle this error?

Consider:
- Error severity
- Alternative approaches
- Impact on final result
- Confidence implications
```

**AI Response Example**:
```
Selected Handler: RETRY with backoff

Reasoning:
- Error is likely transient (RPC timeout)
- Block data is critical for analysis
- Cost of retry is low
- Fallback options available if retry fails

LISP Error Handling:
(do
  (define result
    (retry-with-backoff
      :max-attempts 3
      :delay 1000
      :fn (lambda () (getBlock :slot {slot_number}))))
  result)

If retry fails after 3 attempts, then use FALLBACK handler.

Updated Confidence: 75% (reduced due to RPC instability)
```

---

## Prompt Template 4: Loop Iteration Decision

**When**: Inside a loop and need to decide whether to continue
**Purpose**: AI evaluates loop condition and determines next action

```
[SYSTEM PROMPT - already loaded]
Evaluating LISP loop condition.

[LOOP CONTEXT]
Loop Type: while loop
Loop Condition: (< processed total-items)
Loop Body:
  (define item (get items processed))
  (processItem item)
  (set! processed (+ processed 1))

[CURRENT STATE]
Iteration Number: {iteration_count}
Current Variables:
  processed = {current_processed_value}
  total-items = {total_items_value}
  items = [{...}, {...}, ...]

Loop Condition Evaluation:
  (< processed total-items)
  (< {current_processed_value} {total_items_value})
  {true_or_false}

[QUESTION]
Should the loop continue executing?

Evaluate:
1. Condition result (true/false)
2. Whether to BREAK early
3. Safety limits (max iterations)
```

**AI Response Example**:
```
Loop Decision: CONTINUE

Evaluation:
  (< processed total-items)
  (< 15 100)
  true

Therefore, loop condition is satisfied. Continue execution.

No early BREAK conditions detected:
- All items processing successfully
- Within reasonable iteration count (15 / 1000 max)
- No timeout concerns

Next Iteration: Process item at index 15
```

---

## Prompt Template 5: Final Action Generation

**When**: All branches executed, ready for final output
**Purpose**: AI synthesizes results into final answer

```
[SYSTEM PROMPT - already loaded]
Generating final output from OVSM execution results.

[ORIGINAL QUESTION]
{user_research_question}

[EXECUTION SUMMARY]
Main Branch Executed: ✅
Decision Points Resolved: {count}
Branches Executed: {list_of_branches}
Tool Calls Made: {count}

[COLLECTED DATA - LISP Variables]
{all_variables_with_values_in_lisp_format}

Example:
  current-slot = 245123456
  avg-fee = 6420
  fee-stddev = 1250
  total-txs = 342
  fee-pattern = "high-variance"

[INTERMEDIATE RESULTS]
{key_findings_from_execution}

[QUESTION]
Based on the complete OVSM execution, provide the final answer to the research question.

Include:
1. Direct answer to the question
2. Key findings (3-5 bullet points)
3. Supporting data with LISP variable references
4. Confidence level and caveats
5. Recommendations (if applicable)
```

**AI Response Example**:
```
# Final Answer: Transaction Fee Analysis

## Direct Answer
Current Solana network fees are experiencing high variance with an average of
6,420 lamports per transaction (avg-fee variable), which is 28% above the
30-day baseline.

## Key Findings
- Average fee: 6,420 lamports (from LISP: avg-fee)
- Standard deviation: 1,250 lamports (from LISP: fee-stddev)
- Variance ratio: 0.19 (calculated: fee-stddev / avg-fee)
- Total transactions analyzed: 342 (from LISP: total-txs)
- Pattern detected: High variance indicates congestion

## Supporting Data
LISP Variables Referenced:
  (define avg-fee 6420)
  (define fee-stddev 1250)
  (define variance-ratio (/ fee-stddev avg-fee))  ;; 0.19
  (define total-txs 342)

## Confidence: 90%
High confidence due to:
- Large sample size (342 transactions)
- Recent data (current-slot = 245123456)
- Clear statistical pattern

Caveats:
- Single block sample (consider multi-block for trends)
- Network conditions may vary by time of day

## Recommendations
1. For time-sensitive transactions: Set priority fee to 8,000+ lamports
2. For cost-sensitive: Wait for lower-variance periods
3. Monitor trend over next 100 blocks for pattern confirmation
```

---

## Common LISP Patterns in Prompts

### Pattern 1: Variable Tracking
```
Current Variables (LISP format):
  (define slot 245123456)
  (define block {...})
  (define fees [2500, 3200, ...])
```

### Pattern 2: Condition Evaluation
```
Evaluating: (> avg-fee threshold)
Substitution: (> 6420 5000)
Result: true
```

### Pattern 3: Loop State
```
Loop: (while (< i 10) ...)
Current: i = 7
Condition: (< 7 10) = true
Action: Continue
```

### Pattern 4: Error Handling
```
Failed: (getBlock :slot 123)
Handler: (retry-with-backoff ...)
Fallback: (use-cached-data)
```

---

## Migration from V1 (Python-style)

### Variable References
- V1: `$variable = value`
- V2: `(define variable value)`

### Conditionals
- V1: `IF condition THEN ... ELSE ...`
- V2: `(if condition then else)`

### Loops
- V1: `WHILE condition: body`
- V2: `(while (condition) body...)`

### Tool Calls
- V1: `toolName(param: value)`
- V2: `(toolName :param value)`

---

## Best Practices

1. **Always use LISP syntax** in prompts and examples
2. **Show variable bindings** in LISP format: `(define var value)`
3. **Evaluate conditions explicitly** with substitution steps
4. **Reference the query library** for proven patterns
5. **Include inline comments** with `;;` for clarity

---

## Query Library Reference

When generating prompts, reference these proven examples:

- **Query #001**: Simple arithmetic patterns
- **Query #036**: IF-THEN-ELSE in loops (the critical fix!)
- **Query #040**: Array transformation patterns
- **Query #076**: Accumulator pattern for iteration
- **Query #100**: Real-world DeFi calculations

Location: `/home/larp/larpdevs/osvm-cli/crates/ovsm/agent_queries/`

---

**Version:** 2.0
**Status:** Production
**Last Updated:** October 19, 2025
**Syntax:** LISP/S-Expression Only
