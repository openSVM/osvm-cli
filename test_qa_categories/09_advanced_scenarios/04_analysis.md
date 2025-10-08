# Advanced Scenarios - Analysis Questions
**Category:** 09_advanced_scenarios
**Focus:** MEV, clustering, rugpulls, governance
**Level:** Analysis
**Topic:** Security analysis
**Questions:** Q301-Q400

---

## Q301: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q302: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q303: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q304: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q305: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q306: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q307: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q308: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q309: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q310: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q311: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q312: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q313: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q314: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q315: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q316: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q317: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q318: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q319: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q320: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q321: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q322: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q323: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q324: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q325: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q326: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q327: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q328: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q329: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q330: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q331: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q332: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q333: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q334: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q335: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q336: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q337: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q338: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q339: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q340: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q341: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q342: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q343: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q344: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q345: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q346: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q347: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q348: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q349: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q350: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q351: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q352: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q353: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q354: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q355: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q356: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q357: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q358: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q359: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q360: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q361: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q362: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q363: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q364: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q365: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q366: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q367: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q368: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q369: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q370: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q371: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q372: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q373: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q374: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q375: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q376: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q377: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q378: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q379: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q380: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q381: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q382: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q383: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q384: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q385: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q386: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q387: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q388: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q389: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q390: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q391: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q392: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "n"
$result = TOOL.getTransaction($signature)

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

## Q393: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q394: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "mint"
$result = TOOL.getTransaction($signature)

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

## Q395: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "proposal"
$result = TOOL.getTransaction($signature)

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

## Q396: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q397: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "program"
$result = TOOL.getTransaction($signature)

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

## Q398: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "collection"
$result = TOOL.getTransaction($signature)

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

## Q399: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

## Q400: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getBlock, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "input"
$result = TOOL.getTransaction($signature)

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

