# Advanced Scenarios - Historical Questions
**Category:** 09_advanced_scenarios
**Focus:** MEV, clustering, rugpulls, governance
**Level:** Historical
**Topic:** Historical exploit data
**Questions:** Q701-Q800

---

## Q701: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q702: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q703: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q704: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q705: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q706: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q707: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q708: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q709: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q710: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q711: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q712: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q713: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q714: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q715: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q716: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q717: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q718: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q719: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q720: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q721: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q722: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q723: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q724: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q725: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q726: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q727: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q728: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q729: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q730: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q731: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q732: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q733: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q734: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q735: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q736: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q737: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q738: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q739: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q740: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q741: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q742: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q743: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q744: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q745: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q746: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q747: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q748: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q749: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q750: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q751: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q752: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q753: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q754: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q755: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q756: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q757: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q758: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q759: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q760: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q761: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q762: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q763: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q764: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q765: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q766: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q767: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q768: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q769: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q770: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q771: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q772: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q773: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q774: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q775: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q776: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q777: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q778: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q779: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q780: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q781: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q782: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q783: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q784: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q785: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q786: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q787: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q788: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q789: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q790: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q791: "Detect MEV opportunities in the mempool"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q792: "Identify sandwich attacks in the last 100 blocks"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q793: "Find accounts with suspicious token transfer patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q794: "Detect potential rugpull indicators for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q795: "Analyze governance proposal Prop-123 voting patterns"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q796: "Find clusters of related addresses using transaction graph analysis"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q797: "Detect front-running transactions for DEX TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q798: "Identify wash trading patterns for NFT collection DeGods"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q799: "Find accounts with sudden large balance changes"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q800: "Detect potential Sybil attack patterns in validator network"

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
$result = getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing historical-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

