# OVSM Execution Prompts - Runtime Decision Making

When executing an OVSM plan, you need to call the AI at different stages for decisions. This document provides prompt templates for each execution stage.

---

## Execution Flow

```
1. Initial Planning → AI generates OVSM plan
2. Execute Main Branch → Run tools
3. Decision Point Reached → AI makes decision based on results
4. Execute Branch → Run tools
5. Another Decision Point → AI decides again
6. Final Action → Complete
```

---

## Prompt Template 1: Initial Planning

**When**: User asks a research question
**Purpose**: Generate the complete OVSM plan

```
[SYSTEM PROMPT - loaded once]

[USER]
Research Question: {user_question}

Context:
- Available tools: {list_of_available_tools}
- Time constraint: {max_time or "none"}
- Cost constraint: {max_cost or "none"}

Please provide an OVSM plan for this research.
```

**AI Response**: Complete OVSM plan with Main Branch and Decision Points

---

## Prompt Template 2: Decision Point Evaluation

**When**: Reached a DECISION point and need to choose a branch
**Purpose**: AI analyzes results and selects appropriate branch

```
[SYSTEM PROMPT - already loaded]

[CONTEXT]
Original Question: {original_question}
Current OVSM Plan: {the_ovsm_plan}

[EXECUTION STATUS]
We have executed the Main Branch and reached a Decision Point.

Decision Point: {decision_description}

Available Branches:
{list_each_branch_with_conditions}

[TOOL RESULTS]
Tool calls executed so far:
{tool_name_1}({params}) → {result_1}
{tool_name_2}({params}) → {result_2}
...

Current Variables:
$var1 = {value1}
$var2 = {value2}
...

[QUESTION]
Based on these results, which branch should we execute?

Evaluate each branch condition and select the appropriate one.
Respond with:
1. Selected branch (A, B, C, etc.)
2. Reasoning for selection
3. Updated confidence level if applicable
```

**AI Response**:
```
Selected Branch: BRANCH B

Reasoning: The condition ($stddev / $mean > 0.5) evaluates to true because:
- $stddev = 1250
- $mean = 2000
- Ratio: 0.625 > 0.5
Therefore high variance detected, should use median instead of mean.

Updated Confidence: 85% (was 90%, reduced due to high variance)
```

---

## Prompt Template 3: Error Handling Decision

**When**: An error occurred and CATCH block has multiple handlers
**Purpose**: AI determines error type and selects handler

```
[CONTEXT]
Executing tool: {tool_name}({parameters})

[ERROR OCCURRED]
Error: {error_message}
Error Type: {error_type if known}
Stack Trace: {stack_trace if available}

[ERROR HANDLING OPTIONS]
The plan defines these CATCH handlers:
- CATCH FATAL: {fatal_handler_description}
- CATCH RECOVERABLE: {recoverable_handler_description}
- CATCH WARNING: {warning_handler_description}

[QUESTION]
Classify this error and determine which handler to execute:
1. Is this error FATAL, RECOVERABLE, or WARNING?
2. Which CATCH block should we execute?
3. Should we retry, fallback, or abort?
```

**AI Response**:
```
Error Classification: RECOVERABLE

This is a network timeout, which is typically recoverable.
The service might be temporarily unavailable.

Recommended Action: Execute RECOVERABLE handler
- Attempt fallback to secondary data source
- If that fails, use cached/historical data with reduced confidence

Retry: Yes, after 2s backoff
```

---

## Prompt Template 4: Mid-Execution Adaptation

**When**: Monitoring constraints (time/cost) during execution
**Purpose**: AI decides whether to continue or switch strategies

```
[EXECUTION MONITORING]
Started: {start_time}
Elapsed: {elapsed_time}
Constraint: Max time = {max_time}
Progress: {percentage}%

Cost So Far: {actual_cost}
Constraint: Max cost = {max_cost}

[CURRENT STATE]
Completed Steps: {completed_steps}
Remaining Steps: {remaining_steps}
Estimated Time Remaining: {estimate}

[ISSUE]
We are approaching the time constraint ({elapsed_time} / {max_time}).

[OPTIONS]
1. Continue with current plan (may exceed constraint)
2. Switch to faster but less accurate approach
3. Abort and return partial results
4. Skip optional analysis steps

[QUESTION]
What should we do? Consider:
- How close are we to completion?
- Can we still get valuable results?
- What's the cost of stopping now vs continuing?
```

**AI Response**:
```
Recommendation: Switch to faster approach (Option 2)

Reasoning:
- 80% through time budget but only 50% through steps
- Current pace will exceed constraint by ~30%
- Can sacrifice detailed analysis for core findings
- Partial results still valuable

Action Plan:
- Skip optional cross-validation step
- Use sampling instead of full enumeration
- Reduce sample size from 10000 to 1000
- Estimated completion: within constraint
- Confidence reduction: 95% → 85% (acceptable)
```

---

## Prompt Template 5: Conditional Loop Exit

**When**: In a WHILE or FOR loop with BREAK IF condition
**Purpose**: AI evaluates if exit condition is met

```
[LOOP CONTEXT]
Loop Type: {WHILE or FOR}
Iteration: {current_iteration}
Max Iterations: {max_iterations if set}

Loop Code:
{show_the_loop_code}

[CURRENT STATE]
Variables:
$var1 = {value1}
$var2 = {value2}

Latest Results:
{most_recent_tool_results}

[EXIT CONDITION]
BREAK IF {condition_expression}

[QUESTION]
Should we exit the loop now?
Evaluate: {condition_expression}
Consider: Have we gathered sufficient data? Is confidence high enough?
```

**AI Response**:
```
Exit Loop: YES

Evaluation:
- Condition: $confidence > 90
- Current: $confidence = 93
- Result: 93 > 90 → TRUE

We have sufficient data with high confidence. No benefit to continuing.

Action: BREAK from loop, proceed to next section
```

---

## Prompt Template 6: Agent Result Synthesis

**When**: Multiple parallel agents complete, need to merge results
**Purpose**: AI synthesizes findings from multiple agents

```
[PARALLEL AGENTS COMPLETED]
Number of Agents: {count}

Agent Results:
Agent 1 ({agent_type_1}):
  Task: {task_1}
  Result: {result_1}
  Confidence: {conf_1}%

Agent 2 ({agent_type_2}):
  Task: {task_2}
  Result: {result_2}
  Confidence: {conf_2}%

...

[SYNTHESIS REQUIRED]
These agents investigated different aspects of: {original_question}

[QUESTION]
Synthesize these results into a comprehensive answer:
1. What are the key findings from each agent?
2. Are there contradictions or conflicts?
3. What's the combined confidence level?
4. What's the final consolidated answer?
```

**AI Response**:
```
Key Findings:
- Price Analysis: Token shows 23% increase over 7 days
- Volume Analysis: Trading volume up 150%, suggesting real demand
- Sentiment Analysis: 78% positive mentions, growing interest

Contradictions: None - all indicators align positively

Combined Confidence: 87%
- Price data: 95% confidence
- Volume data: 90% confidence
- Sentiment data: 75% confidence (social data less reliable)
- Weighted average: 87%

Final Answer:
Strong bullish signals across all modalities. Price increase supported by
volume growth and positive sentiment. High confidence this is genuine demand
rather than manipulation.
```

---

## Prompt Template 7: Hypothesis Test Conclusion

**When**: Statistical test completed, need to draw conclusion
**Purpose**: AI interprets statistical results

```
[HYPOTHESIS TESTING]
Hypothesis: {hypothesis_statement}
Null Hypothesis: {null_hypothesis}
Alternative: {alternative_hypothesis}
Significance Level: {alpha}

[TEST RESULTS]
Test Type: {t_test, chi_square, etc}
Test Statistic: {statistic_value}
P-Value: {p_value}
Effect Size: {effect_size}
Sample Sizes: Group1={n1}, Group2={n2}

[QUESTION]
Interpret these results:
1. Do we reject or fail to reject the null hypothesis?
2. Is the result statistically significant?
3. What does this mean practically?
4. What are the limitations?
```

**AI Response**:
```
Conclusion: REJECT null hypothesis

Statistical Significance: YES
- P-value (0.003) < significance level (0.05)
- We have strong evidence against the null hypothesis

Interpretation:
Higher priority fees DO cause faster confirmation times.
- Effect size: -2.4 seconds (medium to large effect)
- Group difference statistically significant and practically meaningful

Practical Meaning:
Users paying >1000 lamports priority fee confirm ~2.4s faster on average.
This is meaningful in high-frequency trading scenarios.

Limitations:
- Observational data (not randomized experiment)
- Confounders may exist (network conditions, time of day)
- Effect may vary during extreme congestion
- Sample limited to 1000 transactions per group
```

---

## Prompt Template 8: Insufficient Data Recovery

**When**: GUARD clause fails or insufficient data found
**Purpose**: AI suggests alternative approaches

```
[DATA COLLECTION FAILED]
Attempted: {what_was_attempted}
Expected: {expected_data}
Actual: {actual_result}

Guard Clause Failed: {guard_condition}
Error: {error_message}

[QUESTION]
The primary approach failed. What should we do?

Options:
1. Try alternative data source
2. Relax constraints and retry
3. Use historical/cached data with lower confidence
4. Return error to user with explanation

Consider:
- Can we still answer the user's question partially?
- Are there workarounds?
- What confidence level would alternatives provide?
```

**AI Response**:
```
Recommendation: Option 3 - Use historical data with reduced confidence

Reasoning:
- Alternative sources likely have same issue (network-wide outage)
- Relaxing constraints won't help (no data available)
- Historical data from 24h ago still relevant
- Partial answer better than no answer

Action:
1. Query historical cache for similar time period yesterday
2. Return result with clear caveats
3. Reduce confidence: 90% → 60%
4. Add caveat: "Based on historical data due to current network unavailability"

Estimated confidence: 60%
User value: High (still gets useful answer)
```

---

## Prompt Template 9: Dynamic Replanning

**When**: Original plan is failing or taking too long
**Purpose**: AI generates alternative strategy mid-execution

```
[CURRENT PLAN PERFORMANCE]
Original Plan: {summary_of_plan}
Expected Time: {expected_time}
Actual Time So Far: {elapsed_time}
Completion: {percentage}%

[ISSUE]
The plan is not performing as expected:
- Taking {X}x longer than estimated
- Already used {Y}% of cost budget
- Confidence dropping due to {reason}

[COMPLETED SO FAR]
{list_of_completed_steps_and_results}

[REMAINING]
{list_of_remaining_steps}

[QUESTION]
Generate an alternative strategy that:
1. Uses the data already collected
2. Completes within remaining time/cost budget
3. Still answers the user's question
4. Explicitly states reduced confidence level

Provide a modified OVSM plan for the remaining work.
```

**AI Response**:
```
Alternative Strategy:

**Remaining Plan:**

[TIME: ~15s] [COST: ~0.0005 SOL] [CONFIDENCE: 75%]

**Main Branch:**
// Reuse already collected data
$existing_blocks = {blocks_collected}
$existing_txs = {transactions_parsed}

// Skip detailed analysis, use statistical sampling
$sample = SLICE(array: $existing_txs, start: 0, end: 500)
$fees = MAP($sample, tx => tx.meta.fee)
$quick_avg = MEAN(data: $fees)

**Action:**
RETURN {
  average_fee: $quick_avg,
  confidence: 75,
  note: "Reduced analysis due to time constraint",
  sample_size: 500,
  caveat: "Sampled data, not exhaustive"
}

Tradeoffs:
- Faster: 15s vs 45s
- Cheaper: Uses existing data
- Less confidence: 75% vs 95%
- Still valuable: User gets answer
```

---

## General Format for All Runtime Prompts

```
[SYSTEM] - Keep loaded
[CONTEXT] - Original question & plan
[EXECUTION STATE] - What's been done, current variables
[DECISION NEEDED] - Specific choice to make
[OPTIONS] - Available choices
[QUESTION] - Clear question for AI
```

**AI should always respond with:**
1. Clear decision/selection
2. Reasoning based on data
3. Updated confidence if changed
4. Next actions to take

---

## Best Practices

### DO:
✓ Include all relevant tool results
✓ Show current variable values
✓ List available options explicitly
✓ Ask specific questions
✓ Provide context about constraints

### DON'T:
✗ Send full plan every time (just relevant Decision Point)
✗ Hide error details
✗ Omit variable values
✗ Ask vague "what should I do?" without context
✗ Forget to include confidence levels

---

## Token Optimization

For token-constrained scenarios, minimal format:

```
Decision Point: {description}
Results: {key_results_only}
Branches: A) {cond_a} B) {cond_b}
Choose: A or B?
```

**AI Response**: `BRANCH B - {one_line_reason}`

This minimal format uses ~50-100 tokens vs ~200-400 for full format.

---

## Example Integration

```python
# Initial planning
plan = ai.generate_plan(user_question)

# Execute main branch
results = execute_tools(plan.main_branch)

# Reached decision point
if plan.has_decision_point():
    decision_prompt = f"""
    Decision Point: {plan.decision.description}

    Tool Results:
    {format_results(results)}

    Variables:
    {format_variables(current_state)}

    Branches:
    {format_branches(plan.decision.branches)}

    Which branch should execute?
    """

    branch_choice = ai.decide(decision_prompt)

    # Execute selected branch
    execute_tools(plan.decision.branches[branch_choice])
```

---

**File**: OVSM_EXECUTION_PROMPTS.md
**Version**: v1.0
**Use With**: OVSM Language v1.1
