# DeFi Analysis - Expert Questions
**Category:** 07_defi_analysis
**Focus:** DEX, lending, liquidity, farming, oracles
**Level:** Expert
**Topic:** Advanced DeFi analytics
**Questions:** Q901-Q1000

---

## Q901: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q902: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q903: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q904: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q905: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q906: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q907: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q908: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q909: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q910: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q911: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q912: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q913: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q914: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q915: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q916: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q917: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q918: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q919: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q920: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q921: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q922: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q923: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q924: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q925: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q926: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q927: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q928: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q929: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q930: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q931: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q932: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q933: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q934: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q935: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q936: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q937: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q938: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q939: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q940: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q941: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q942: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q943: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q944: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q945: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q946: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q947: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q948: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q949: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q950: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q951: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q952: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q953: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q954: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q955: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q956: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q957: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q958: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q959: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q960: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q961: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q962: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q963: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q964: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q965: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q966: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q967: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q968: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q969: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q970: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q971: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q972: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q973: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q974: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q975: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q976: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q977: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q978: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q979: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q980: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q981: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q982: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q983: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q984: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q985: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q986: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q987: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q988: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q989: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q990: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q991: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q992: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q993: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q994: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q995: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q996: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q997: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q998: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q999: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q1000: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing expert-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

