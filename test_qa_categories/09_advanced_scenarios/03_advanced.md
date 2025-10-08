# Advanced Scenarios - Advanced Questions
**Category:** 09_advanced_scenarios
**Focus:** MEV, clustering, rugpulls, governance
**Level:** Advanced
**Topic:** Complex attack patterns
**Questions:** Q201-Q300

---

## Q201: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q202: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q203: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q204: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q205: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q206: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q207: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q208: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q209: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q210: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q211: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q212: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q213: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q214: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q215: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q216: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q217: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q218: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q219: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q220: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q221: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q222: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q223: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q224: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q225: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q226: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q227: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q228: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q229: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q230: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q231: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q232: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q233: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q234: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q235: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q236: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q237: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q238: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q239: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q240: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q241: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q242: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q243: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q244: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q245: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q246: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q247: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q248: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q249: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q250: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q251: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q252: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q253: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q254: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q255: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q256: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q257: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q258: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q259: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q260: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q261: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q262: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q263: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q264: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q265: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q266: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q267: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q268: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q269: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q270: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q271: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q272: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q273: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q274: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q275: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q276: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q277: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q278: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q279: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q280: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q281: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q282: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q283: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q284: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q285: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q286: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q287: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q288: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q289: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q290: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q291: "Detect MEV opportunities in the mempool"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q292: "Identify sandwich attacks in the last 100 blocks"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q293: "Find accounts with suspicious token transfer patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q294: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q295: "Analyze governance proposal Prop-123 voting patterns"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q296: "Find clusters of related addresses using transaction graph analysis"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q297: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q298: "Identify wash trading patterns for NFT collection DeGods"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q299: "Find accounts with sudden large balance changes"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q300: "Detect potential Sybil attack patterns in validator network"

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
Return structured data showing advanced-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

