# Advanced Scenarios - Edge_Cases Questions
**Category:** 09_advanced_scenarios
**Focus:** MEV, clustering, rugpulls, governance
**Level:** Edge_Cases
**Topic:** Exploit edge cases
**Questions:** Q801-Q900

---

## Q801: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q802: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q803: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q804: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q805: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q806: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q807: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q808: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q809: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q810: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q811: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q812: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q813: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q814: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q815: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q816: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q817: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q818: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q819: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q820: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q821: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q822: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q823: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q824: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q825: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q826: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q827: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q828: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q829: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q830: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q831: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q832: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q833: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q834: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q835: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q836: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q837: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q838: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q839: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q840: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q841: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q842: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q843: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q844: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q845: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q846: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q847: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q848: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q849: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q850: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q851: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q852: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q853: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q854: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q855: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q856: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q857: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q858: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q859: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q860: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q861: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q862: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q863: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q864: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q865: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q866: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q867: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q868: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q869: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q870: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q871: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q872: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q873: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q874: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q875: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q876: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q877: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q878: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q879: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q880: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q881: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q882: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q883: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q884: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q885: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q886: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q887: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q888: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q889: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q890: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q891: "Detect MEV opportunities in the mempool"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q892: "Identify sandwich attacks in the last 100 blocks"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q893: "Find accounts with suspicious token transfer patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q894: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q895: "Analyze governance proposal Prop-123 voting patterns"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q896: "Find clusters of related addresses using transaction graph analysis"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q897: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q898: "Identify wash trading patterns for NFT collection DeGods"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q899: "Find accounts with sudden large balance changes"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q900: "Detect potential Sybil attack patterns in validator network"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

