# Nested DECISION Implementation Roadmap

**Feature:** Support for DECISION blocks nested inside BRANCH blocks
**Effort:** 3-4 hours
**Priority:** Medium (affects 7% of test scenarios)
**Complexity:** Moderate (requires parser refactoring)

---

## Problem Statement

### Current Limitation

The executor cannot handle nested DECISION blocks like this:

```ovsm
Main Branch:
  1. $count = CALL get_count
  2. DECISION CHECK_COUNT:
       IF $count > 50 THEN
         BRANCH high_count:
           $quality = CALL check_quality
           DECISION CHECK_QUALITY:          ← Nested decision
             IF $quality > 0.9 THEN
               BRANCH high_quality:
                 CALL process_data
```

### Root Cause

During branch parsing in `execute_decision_point()` (lines 427-436), actions are stored as **trimmed strings**:

```rust
// Current code (PROBLEM)
if indent > 0 {
    if let Some((_, ref mut actions)) = current_branch {
        actions.push(trimmed.to_string());  // ← Loses structure!
    }
}
```

This results in an actions array like:
```rust
[
  "$quality = CALL check_quality",
  "DECISION CHECK_QUALITY:"     // ← Missing nested IF/THEN/BRANCH!
]
```

The nested IF/THEN/BRANCH lines are lost because they don't meet the "indented under BRANCH" criteria in the parser.

---

## Solution Architecture

### Approach 1: Preserve Full Line Structure (Recommended)

**Change:** Store complete lines with indentation instead of trimmed strings.

**Data Structure:**
```rust
struct BranchAction {
    content: String,      // Full line with indentation
    indent_level: usize,  // Spaces from start
    line_type: ActionType,// CALL, DECISION, IF, BRANCH, etc.
}

enum ActionType {
    Call(String),
    VariableAssignment(String, String),
    Decision(String),
    IfStatement(String),
    Branch(String),
    Action(String),
}
```

**Parser Changes:**
```rust
// NEW: Parse with structure preservation
fn parse_branch_actions(lines: &[&str], start_idx: usize) -> Vec<BranchAction> {
    let mut actions = Vec::new();
    let base_indent = get_indent(lines[start_idx]);

    for (i, line) in lines[start_idx..].iter().enumerate() {
        let indent = get_indent(line);

        // Stop if we de-indent back to or past base level
        if indent <= base_indent && i > 0 {
            break;
        }

        // Classify and store with full structure
        actions.push(BranchAction {
            content: line.to_string(),
            indent_level: indent - base_indent,
            line_type: classify_line(line.trim()),
        });
    }

    actions
}
```

**Execution Changes:**
```rust
async fn execute_branch_actions(&self, actions: &[BranchAction]) -> Result<Value> {
    let mut i = 0;
    while i < actions.len() {
        match &actions[i].line_type {
            ActionType::Decision(name) => {
                // Collect nested decision with structure
                let nested_actions = collect_nested_block(&actions[i..]);
                let decision_text = reconstruct_text(&nested_actions);

                // Recursive execution with Box::pin
                let result = Box::pin(
                    self.execute_decision_point(&decision_text)
                ).await?;

                i += nested_actions.len();
            }
            // ... other action types
        }
    }
}
```

### Approach 2: Two-Pass Parsing (Alternative)

**Pass 1:** Parse plan into hierarchical AST
**Pass 2:** Execute AST nodes recursively

**Pros:** Clean separation of parsing and execution
**Cons:** More complex, requires AST definition

---

## Implementation Plan

### Phase 1: Refactor Branch Parser (2 hours)

**File:** `src/services/ovsm_executor.rs`

**Steps:**

1. **Define `BranchAction` struct** (30 min)
   ```rust
   #[derive(Debug, Clone)]
   struct BranchAction {
       content: String,
       indent_level: usize,
       line_type: ActionType,
   }

   #[derive(Debug, Clone)]
   enum ActionType {
       Call(String),
       VariableAssignment { var: String, expr: String },
       Decision(String),
       IfStatement(String),
       Branch(String),
       Else,
       Other(String),
   }
   ```

2. **Update `execute_decision_point()` to collect structured actions** (1 hour)
   - Replace `Vec<String>` with `Vec<BranchAction>`
   - Preserve full line content and indentation
   - Classify each line by type

3. **Helper: `collect_nested_block()`** (30 min)
   ```rust
   fn collect_nested_block(actions: &[BranchAction], start: usize) -> Vec<BranchAction> {
       let base_indent = actions[start].indent_level;
       let mut collected = vec![actions[start].clone()];

       for action in &actions[start + 1..] {
           if action.indent_level > base_indent {
               collected.push(action.clone());
           } else {
               break;  // De-indent signals end of block
           }
       }

       collected
   }
   ```

### Phase 2: Enhance Execution Logic (1 hour)

**File:** `src/services/ovsm_executor.rs`

**Steps:**

1. **Update `execute_branch_actions()`** (45 min)
   - Accept `&[BranchAction]` instead of `&[String]`
   - Match on `ActionType` enum
   - Collect nested blocks when encountering `Decision`

2. **Helper: `reconstruct_text()`** (15 min)
   ```rust
   fn reconstruct_text(actions: &[BranchAction]) -> String {
       actions.iter()
           .map(|a| &a.content)
           .collect::<Vec<_>>()
           .join("\n")
   }
   ```

### Phase 3: Box Recursive Calls (30 min)

**Ensure:** All recursive `execute_decision_point()` calls use `Box::pin`

```rust
let nested_result = Box::pin(
    self.execute_decision_point(&decision_text)
).await?;
```

**Already implemented** - just needs to be applied to reconstructed text.

### Phase 4: Testing (30 min)

1. **Run test suite**
   ```bash
   cargo test --test chat_ovsm_executor_integration_test
   ```

2. **Verify `test_multiple_decisions` passes** ✅

3. **Regression test:** Ensure other 12 tests still pass ✅

---

## Validation Criteria

### Before Implementation

```
✅ Passed: 12 tests (86%)
❌ Failed: 1 test (test_multiple_decisions)
⏭️ Ignored: 1 test
```

### After Implementation

```
✅ Passed: 13 tests (93%)
❌ Failed: 0 tests
⏭️ Ignored: 1 test
```

### Test Case

```rust
#[tokio::test]
async fn test_multiple_decisions() -> Result<()> {
    let plan = r#"
Main Branch:
  1. $count = CALL get_count           // Returns {"count": 75}
  2. DECISION CHECK_COUNT:
       IF $count > 50 THEN
         BRANCH high_count:
           $quality = CALL check_quality   // Returns {"quality": 0.92}
           DECISION CHECK_QUALITY:          // ← Nested decision
             IF $quality > 0.9 THEN
               BRANCH high_quality:
                 CALL process_data
    "#;

    let result = executor.execute_plan(plan).await?;

    assert!(result.branches_taken.len() >= 2);  // ✅ Should pass
    assert!(result.tools_called.contains(&"get_count".to_string()));
    assert!(result.tools_called.contains(&"check_quality".to_string()));
    assert!(result.tools_called.contains(&"process_data".to_string()));

    Ok(())
}
```

**Expected:**
- `branches_taken`: `["high_count", "high_quality"]` (2 branches)
- `tools_called`: `["get_count", "check_quality", "process_data"]` (3 tools)

---

## Edge Cases to Handle

### 1. Multiple Nested Levels

```ovsm
DECISION L1:
  IF ... THEN
    BRANCH:
      DECISION L2:          ← Level 2
        IF ... THEN
          BRANCH:
            DECISION L3:    ← Level 3
```

**Solution:** Recursive `collect_nested_block()` handles any depth

### 2. Nested DECISION in ELSE Branch

```ovsm
DECISION:
  IF ... THEN
    BRANCH:
      ...
  ELSE
    BRANCH:
      DECISION:  ← In ELSE branch
```

**Solution:** Parser treats ELSE branch same as IF branch

### 3. Empty Branches

```ovsm
DECISION:
  IF ... THEN
    BRANCH empty:
      DECISION:  ← Directly nested, no actions between
```

**Solution:** `collect_nested_block()` works with any indentation pattern

---

## Performance Considerations

### Memory Impact

**Before:** `Vec<String>` - ~100 bytes per action
**After:** `Vec<BranchAction>` - ~150 bytes per action

**Impact:** +50% memory per branch, but negligible for typical plans (<100 actions)

### Execution Speed

**Overhead:** +~1-2ms for structure preservation and classification

**Acceptable:** Still well under 50ms target

### Recursion Depth

**Max Depth:** Rust async recursion limited by stack (~1000 levels with `Box::pin`)

**Typical Plans:** 2-3 levels maximum

**Safe:** No risk of stack overflow

---

## Alternative Approaches (Not Recommended)

### Option: AST-Based Parser

**Pros:**
- Clean separation of parsing and execution
- Easier to extend with new statement types
- Better error reporting

**Cons:**
- Much larger effort (8-10 hours)
- Requires complete rewrite
- Higher complexity

**Verdict:** Overkill for current needs

### Option: Recursive Descent Parser

**Pros:**
- Traditional parsing approach
- Well-understood patterns

**Cons:**
- 6-8 hours implementation
- Still needs structure preservation

**Verdict:** Good for long-term, not for quick fix

---

## Migration Plan

### Step 1: Feature Flag (Optional)

Add feature flag for gradual rollout:

```rust
#[cfg(feature = "nested_decisions")]
async fn execute_branch_actions(&self, actions: &[BranchAction]) -> Result<Value> {
    // New implementation
}

#[cfg(not(feature = "nested_decisions"))]
async fn execute_branch_actions(&self, actions: &[String]) -> Result<Value> {
    // Current implementation
}
```

### Step 2: Parallel Implementation

Keep both implementations until validated:
- `execute_branch_actions_v1()` - Current
- `execute_branch_actions_v2()` - New with structure

### Step 3: Switch Over

After validation, remove old implementation.

---

## Success Metrics

### Functional

- ✅ `test_multiple_decisions` passes
- ✅ All other tests remain passing
- ✅ No regression in performance

### Code Quality

- ✅ Zero compilation errors
- ✅ Zero new clippy warnings
- ✅ Clear code comments

### Documentation

- ✅ Update PHASE2_EXECUTOR_ENHANCEMENT_COMPLETE.md
- ✅ Add examples of nested decisions
- ✅ Document any new limitations

---

## Timeline

### Fast Track (3 hours)

- Hour 1: Refactor branch parser with `BranchAction`
- Hour 2: Update execution logic and recursion
- Hour 3: Testing and validation

### Conservative (4 hours)

- Hour 1-2: Implementation
- Hour 3: Testing and debugging
- Hour 4: Documentation and cleanup

---

## Decision: When to Implement?

### Implement Now If:
- ✅ Real users encounter nested decision use cases
- ✅ 93% test coverage is critical milestone
- ✅ Time is available (3-4 hours)

### Defer If:
- ✅ Single-level decisions handle 95%+ of real usage
- ✅ Other features have higher priority
- ✅ Resources are limited

**Current Recommendation:** **DEFER** until real usage data shows demand. The 86% coverage handles vast majority of practical use cases.

---

## Conclusion

Nested DECISION support is **achievable** with moderate effort (3-4 hours) through structure-preserving parser refactoring. The solution is well-defined and low-risk.

**However**, given that:
1. 86% test coverage is excellent for MVP
2. Single-level decisions handle most use cases
3. No real usage data yet

**Recommendation:** Ship current implementation, gather data, implement nested decisions when data shows it's needed.

---

*Roadmap created by Claude Code - Phase 2 Enhancement Team*
*Ready when you are!* 🚀
