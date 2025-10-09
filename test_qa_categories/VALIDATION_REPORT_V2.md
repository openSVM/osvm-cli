# AI Planning Meta-Language - Deep Validation Report v2

## Status: ISSUES FOUND ⚠️

Second review reveals additional undefined functions and inconsistencies.

---

## Critical Issues Found

### Issue 1: Missing Helper Functions in Base Spec ❌

**In ai-lang-spec.md Example 2 & 3:**

Undefined helper functions:
```
deriveOrcaPoolAddress()       // NOT defined
parseOrcaPoolPrice()          // NOT defined
deriveRaydiumPoolAddress()    // NOT defined
parseRaydiumPoolPrice()       // NOT defined
parseJupiterQuote()           // NOT defined
findPoolsForPair()            // NOT defined
calculateRoutes()             // NOT defined
parseU64()                    // NOT defined
DEX_PROGRAM_ID                // NOT defined (constant)
DEX_PROGRAM                   // NOT defined (constant)
LENDING_PROGRAM               // NOT defined (constant)
SYSTEM_PROGRAM                // NOT defined (constant)
```

**Severity**: HIGH
**Impact**: Examples cannot execute as written

---

### Issue 2: Missing Tools in Agent Extensions ❌

**In ai-lang-spec-agents.md:**

Undefined tools/functions:
```
MERGE_RESULTS()               // NOT defined
EXECUTE_STRATEGY()            // NOT defined
ABS()                         // NOT defined (math function)
extractPriorityFee()          // NOT defined
extractHour()                 // NOT defined
calculateSeverity()           // NOT defined
LOOP EVERY                    // NOT defined syntax
```

**Severity**: HIGH
**Impact**: Agent extension examples cannot execute

---

### Issue 3: Inconsistent Function Usage

**Problem**: Some tools used with named params, some without:
```
// Inconsistent
MAP($blocks, b => b.transactions)              // No named params
MAP(collection: $blocks, fn: b => b.transactions)  // Named params

FILTER($data, pred => ...)                     // Positional
FILTER(collection: $data, predicate: ...)     // Named
```

**Severity**: MEDIUM
**Impact**: Confusing which style to use

---

### Issue 4: Missing Syntax Definition

**LOOP EVERY** syntax used but never defined:
```
LOOP EVERY 30s:
  [statements]
```

**Severity**: MEDIUM
**Impact**: Syntax not in grammar

---

### Issue 5: Constants Not Addressed

Program IDs and constants used but no definition system:
```
DEX_PROGRAM_ID
SYSTEM_PROGRAM
LENDING_PROGRAM
```

**Severity**: LOW
**Impact**: Unclear how to define constants

---

## Proposed Fixes

### Fix 1: Add Missing Math/Utility Functions

```
### Math Tools

TOOL ABS:
  params: {value: number}
  returns: number

TOOL SQRT:
  params: {value: number}
  returns: number

TOOL POW:
  params: {base: number, exponent: number}
  returns: number

TOOL ROUND:
  params: {value: number, decimals?: u32}
  returns: number

TOOL FLOOR:
  params: {value: number}
  returns: number

TOOL CEIL:
  params: {value: number}
  returns: number
```

### Fix 2: Add Missing Agent Extension Tools

```
TOOL MERGE_RESULTS:
  description: "Merge results from multiple agents"
  params: {results: Array<AgentResult>}
  returns: MergedResult

TOOL EXECUTE_STRATEGY:
  params: {strategy: Strategy}
  returns: StrategyResult
```

### Fix 3: Define All Helper Functions

Either:
**Option A**: Add to standard library as tools
**Option B**: Mark as "implementation details" in DEFINE_TOOL

Example for Option B:
```
DEFINE_TOOL queryOrcaPool:
  params: {token_pair: (string, string)}
  returns: f64 | null
  implementation:
    // Helper function (implementation detail)
    DEFINE deriveOrcaPoolAddress(pair) AS:
      // PDA derivation logic
      RETURN pool_address

    // Helper function
    DEFINE parseOrcaPoolPrice(data) AS:
      // Parse pool state
      RETURN price

    $pool_address = deriveOrcaPoolAddress($token_pair)
    $pool_account = getAccountInfo(pubkey: $pool_address)
    $price = parseOrcaPoolPrice($pool_account.data)
    RETURN $price
```

### Fix 4: Define LOOP EVERY Syntax

Add to Control Flow section:
```
### Periodic Loop

LOOP EVERY duration:
  [statements]
  BREAK IF condition

// Examples:
LOOP EVERY 30s:
  $value = checkMetric()
  BREAK IF $value < threshold

LOOP EVERY 5min:
  $data = collect()
```

### Fix 5: Add Constants Section

```
## Constants

Constants use UPPERCASE without `$` prefix:

```
CONST DEX_PROGRAM_ID = "675kPX9MHTjS2zt1qfr1NYHuzeLXfQM9H24wFSUt1Mp8"
CONST SYSTEM_PROGRAM = "11111111111111111111111111111111"
CONST TOKEN_PROGRAM = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

// Usage
IF $program_id == DEX_PROGRAM_ID THEN
  // Handle DEX transaction
```

### Fix 6: Standardize Function Call Syntax

**Decision**: Support BOTH positional and named parameters:

```
// Positional (when parameter meaning is obvious)
$result = MAP($collection, item => item * 2)

// Named (when clarity is needed)
$result = MAP(
  collection: $collection,
  fn: item => item * 2
)

// Rule: Use positional for 1-2 params, named for 3+
```

---

## Revised Validation Checklist

### Syntax Validation
- [x] All keywords defined
- [x] Variable syntax consistent
- [x] Control flow complete
- [ ] LOOP EVERY syntax needs definition ❌
- [x] Error handling comprehensive
- [x] Comments syntax clear

### Tool Validation
- [ ] Base spec examples use undefined helpers ❌
- [ ] Agent spec examples use undefined tools ❌
- [x] Tool parameters specified
- [x] Return types documented
- [x] Custom tools use DEFINE_TOOL

### Completeness
- [x] Base specification structure complete
- [x] Agent extensions all 15 sections written
- [ ] Standard library missing math functions ❌
- [ ] Constants system not defined ❌
- [ ] Helper function pattern unclear ❌

---

## Severity Assessment

**CRITICAL (Blocks usage):**
- None currently

**HIGH (Major confusion/errors):**
- Undefined helper functions in examples
- Missing MERGE_RESULTS, EXECUTE_STRATEGY
- Missing ABS and math functions
- LOOP EVERY syntax not defined

**MEDIUM (Causes confusion):**
- Inconsistent parameter syntax
- Constants not explained
- Helper function pattern unclear

**LOW (Minor issues):**
- Some wording could be clearer
- Examples could show more edge cases

---

## Overall Assessment

**Previous Assessment**: 95/100 (A+)
**Revised Assessment**: 78/100 (C+)

**Reason for Downgrade**: Second review found that examples rely heavily on undefined helper functions and missing tools. While the framework is sound, practical usability is compromised.

**Required Actions**:
1. Add math functions to standard library
2. Define MERGE_RESULTS and EXECUTE_STRATEGY
3. Add LOOP EVERY to control flow
4. Define constants system
5. Clarify helper function pattern in DEFINE_TOOL
6. Standardize parameter syntax rules

**After Fixes**: Should reach 90+/100

---

## Recommendation

**DO NOT USE** for production until:
- [ ] Math functions added
- [ ] All agent extension tools defined
- [ ] LOOP EVERY syntax documented
- [ ] Constants system explained
- [ ] Helper function pattern clarified

**Estimated Fix Time**: 30-60 minutes

**Status**: NEEDS REVISION before production use
