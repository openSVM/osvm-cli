# Advanced Planning Format for AI Research Agents

This document explains the sophisticated conditional branching format used in the QA test questions.

## Overview

Each question includes an **Expected Plan** that demonstrates how an AI agent should:
1. Break down complex queries into tool calls
2. Make decisions based on intermediate results
3. Branch into different strategies based on conditions
4. Synthesize results to achieve the user's goal

## Plan Structure

```markdown
## Q[NUM]: "[User Question]"
**Expected Plan:**
- **Main Branch:**
  - Tool: [first tool call]
  - Tool: [second tool call]
  - Calculate/Extract: [data processing]

- **Decision Point:** [Condition to check]
  - **Branch A ([Condition 1]):**
    - Tool: [conditional tool call]
    - Action: [what to do]
  - **Branch B ([Condition 2]):**
    - Tool: [different tool call]
    - Action: [alternative approach]
  - **Branch C ([Condition 3]):**
    - Tool: [third option]
    - Action: [another approach]

- **Secondary Analysis:**
  - Tool: [additional context gathering]
  - Calculate: [derived metrics]

- **Action:** [Final output description]
```

## Key Components

### 1. Main Branch
The primary execution path that always runs first.

**Example:**
```markdown
- **Main Branch:**
  - Tool: `getSlot` → current_slot
  - Tool: `getBlock` with current_slot
  - Extract: transactions array
```

### 2. Decision Points
Conditional logic based on data from Main Branch.

**Example:**
```markdown
- **Decision Point:** Check transaction count
  - **If count < 10:**
    - Branch A: Query previous blocks
  - **If count >= 10:**
    - Branch B: Use current block only
```

### 3. Multiple Branches
Different strategies based on conditions.

**Example:**
```markdown
- **Decision Point:** Analyze error type
  - **Branch A (Network error):**
    - Tool: `getRecentPerformanceSamples`
    - Check: Network congestion
  - **Branch B (Program error):**
    - Tool: `simulateTransaction`
    - Parse: Error logs
  - **Branch C (Account error):**
    - Tool: `getAccountInfo`
    - Verify: Account state
```

### 4. Nested Decisions
Branches can have their own decision points.

**Example:**
```markdown
- **Decision Point:** Check transaction status
  - **Branch A (Success):**
    - Extract: Result data
    - **Sub-Decision:** Check if CPI involved
      - **If CPI:** Parse inner instructions
      - **If no CPI:** Simple result
  - **Branch B (Failure):**
    - Parse error
```

### 5. Secondary Analysis
Additional context or enrichment after main analysis.

**Example:**
```markdown
- **Secondary Analysis:**
  - Tool: `getRecentPrioritizationFees` for context
  - Calculate: How this compares to network average
  - Correlate: With time of day patterns
```

### 6. AI Decision Points
When the AI needs to choose strategy based on inference.

**Example:**
```markdown
- **Decision Point:** User intent unclear
  - **AI Analysis:** Examine query context
    - Recent → last 24 hours
    - Historical → all time
    - Specific date mentioned → that period
  - Choose appropriate branch
```

## Complete Example

```markdown
## Q42: "Analyze this DEX swap for optimal execution"

**Expected Plan:**
- **Main Branch:**
  - Tool: `getTransaction` with signature
  - Extract: Swap instruction data
  - Parse: Input token, output token, amounts
  - Identify: DEX protocol (Raydium, Orca, Jupiter)

- **Decision Point:** Check if direct swap or routed
  - **Branch A (Direct swap - single pool):**
    - Tool: `getAccountInfo` for liquidity pool
    - Calculate: Pool reserves, price impact
    - Compare: Executed price vs expected price
  - **Branch B (Routed swap - multiple hops):**
    - Tool: `getTransaction` to parse inner instructions
    - Extract: Each hop in the route
    - **For each hop:**
      - Tool: `getAccountInfo` for each pool
      - Calculate: Price impact per hop
      - Identify: Total slippage
  - **Branch C (Aggregated swap):**
    - Parse: Jupiter or 1inch aggregation
    - Extract: All source pools
    - Tool: `getAccountInfo` for each pool
    - Calculate: Split percentages

- **Decision Point:** Compare actual vs optimal execution
  - Tool: Get current prices from all relevant pools
  - Calculate: What optimal route would have been
  - **If suboptimal execution:**
    - Branch A: Calculate missed opportunity cost
    - Identify: Why suboptimal (slippage, routing, timing)
  - **If optimal execution:**
    - Branch B: Confirm best route was chosen
    - Calculate: Savings vs naive route

- **Secondary Analysis:**
  - Tool: `getRecentPrioritizationFees`
  - Calculate: Total cost including gas
  - Compare: Against other DEXes at same time
  - Tool: Query oracle prices for verification
  - Identify: If MEV occurred (front-run/back-run)

- **Action:** Return comprehensive swap analysis with:
  - Execution quality score (0-100)
  - Optimal vs actual comparison
  - Cost breakdown (fees, slippage, gas)
  - MEV impact if detected
  - Recommendations for improvement
```

## Advanced Patterns

### Pattern 1: Loop with Exit Conditions

```markdown
- **Main Branch:**
  - Loop: Query blocks backwards
    - Tool: `getBlock` for slot N
    - Check: Found target transaction?
    - Exit: If found OR reached 1000 blocks
```

### Pattern 2: Parallel Tool Calls

```markdown
- **Main Branch:**
  - Parallel execution:
    - Tool A: `getAccountInfo` for address 1
    - Tool B: `getAccountInfo` for address 2
    - Tool C: `getTransaction` for signature
  - Wait: All complete
  - Synthesize: Results
```

### Pattern 3: Fallback Chain

```markdown
- **Main Branch:**
  - Tool: Primary oracle price feed
- **Decision Point:** Check if price available
  - **If unavailable:**
    - Branch A: Try secondary oracle
    - **If still unavailable:**
      - Branch B: Calculate from DEX pools
      - **If pools empty:**
        - Branch C: Use historical average
```

### Pattern 4: Aggregation with Filtering

```markdown
- **Main Branch:**
  - Tool: `getSignaturesForAddress` → all transactions
  - Filter: Only successful transactions
  - Group by: Instruction type
  - **For each group:**
    - Sample: Representative transactions
    - Tool: `getTransaction` for samples
    - Analyze: Common patterns
  - Aggregate: Findings across groups
```

### Pattern 5: Statistical Decision Making

```markdown
- **Main Branch:**
  - Collect: Data sample
  - Calculate: Mean, median, std dev
- **Decision Point:** Check distribution
  - **If normal distribution:**
    - Use: Standard statistical tests
  - **If skewed distribution:**
    - Use: Median instead of mean
  - **If outliers present (>2 std dev):**
    - Branch: Separate analysis for outliers
```

## Best Practices

### 1. **Always Start with Main Branch**
Define the core execution path before conditional logic.

### 2. **Make Decisions Explicit**
Clearly state what condition triggers each branch.

### 3. **Include Tool Parameters**
Show what parameters each tool call needs.

### 4. **Show Data Flow**
Indicate how data passes between tools:
- `Tool A → result_1`
- `Calculate: metric = f(result_1)`
- `Tool B(metric) → result_2`

### 5. **Explain Branch Selection**
When AI must choose, explain the reasoning:
```markdown
- **AI Decision:** Based on query wording
  - "recent" → last 24 hours
  - "historical" → all time
  - no qualifier → last 7 days (default)
```

### 6. **Handle Edge Cases**
Include branches for unusual situations:
```markdown
- **Branch D (Edge case - no data):**
  - Explain: Why no data available
  - Suggest: Alternative approaches
```

### 7. **Calculate Confidence**
When appropriate, include confidence scoring:
```markdown
- Calculate: Confidence score
  - 100% if direct data
  - 80% if estimated from sample
  - 60% if inferred from patterns
```

## Testing the Plan

A good plan should answer:
- ✅ What tools will be called?
- ✅ In what order?
- ✅ What conditions trigger branches?
- ✅ How is the data synthesized?
- ✅ What's the final output format?
- ✅ How are errors handled?
- ✅ What if data is missing?

## Example: Simple vs Advanced Plan

### ❌ Too Simple
```markdown
**Expected Plan:**
- Tool: `getTransaction`
- Parse: instructions
- Action: Display
```

### ✅ Properly Detailed
```markdown
**Expected Plan:**
- **Main Branch:**
  - Tool: `getTransaction` with signature
  - Extract: transaction.message.instructions

- **Decision Point:** Check instruction types
  - **For each instruction:**
    - Branch A (Known program):
      - Use instruction layouts
      - Parse to human-readable
    - Branch B (Unknown program):
      - Tool: `getAccountInfo` to fetch IDL
      - Parse using IDL or show raw

- **For CPIs:**
  - Extract: meta.innerInstructions
  - Recursively parse each level

- **Action:** Display hierarchical instruction tree
```

## Conclusion

This format ensures that AI agents:
1. Think through the complete problem
2. Handle multiple scenarios
3. Make intelligent decisions
4. Provide comprehensive analysis
5. Adapt to different data conditions

The goal is not just to answer the question, but to demonstrate **intelligent research methodology** that a human expert would follow.
