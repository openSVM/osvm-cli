# DeFi Analysis - Analysis Questions
**Category:** 07_defi_analysis
**Focus:** DEX, lending, liquidity, farming, oracles
**Level:** Analysis
**Topic:** DeFi protocol analysis
**Questions:** Q301-Q400

---

## Q301: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q302: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q303: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q304: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q305: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q306: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q307: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q308: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q309: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q310: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q311: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q312: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q313: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q314: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q315: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q316: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q317: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q318: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q319: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q320: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q321: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q322: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q323: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q324: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q325: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q326: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q327: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q328: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q329: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q330: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q331: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q332: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q333: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q334: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q335: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q336: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q337: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q338: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q339: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q340: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q341: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q342: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q343: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q344: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q345: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q346: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q347: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q348: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q349: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q350: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q351: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q352: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q353: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q354: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q355: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q356: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q357: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q358: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q359: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q360: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q361: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q362: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q363: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q364: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q365: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q366: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q367: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q368: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q369: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q370: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q371: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q372: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q373: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q374: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q375: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q376: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q377: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q378: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q379: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q380: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q381: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q382: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q383: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q384: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q385: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q386: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q387: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q388: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q389: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q390: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q391: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q392: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q393: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q394: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q395: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mintA"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q396: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q397: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q398: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "asset"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q399: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q400: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getProgramAccounts($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing analysis-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

