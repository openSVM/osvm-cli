# DeFi Analysis - Patterns Questions
**Category:** 07_defi_analysis
**Focus:** DEX, lending, liquidity, farming, oracles
**Level:** Patterns
**Topic:** DeFi usage patterns
**Questions:** Q401-Q500

---

## Q401: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q402: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q403: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q404: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q405: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q406: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q407: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q408: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q409: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q410: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q411: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q412: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q413: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q414: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q415: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q416: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q417: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q418: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q419: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q420: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q421: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q422: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q423: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q424: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q425: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q426: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q427: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q428: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q429: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q430: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q431: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q432: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q433: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q434: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q435: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q436: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q437: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q438: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q439: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q440: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q441: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q442: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q443: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q444: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q445: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q446: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q447: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q448: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q449: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q450: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q451: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q452: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q453: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q454: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q455: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q456: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q457: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q458: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q459: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q460: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q461: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q462: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q463: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q464: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q465: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q466: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q467: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q468: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q469: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q470: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q471: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q472: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q473: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q474: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q475: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q476: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q477: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q478: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q479: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q480: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q481: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q482: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q483: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q484: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q485: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q486: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q487: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q488: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q489: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q490: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q491: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q492: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q493: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q494: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q495: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mintA"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q496: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q497: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q498: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "asset"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q499: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q500: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, getTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getProgramAccounts($signature)

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


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
    - Parse response
    - Extract relevant fields
    - Calculate metrics
    - Format output

  BRANCH B (error/not found):
    - Log error
    - Return error message
    - Suggest alternative query

**Action:**
Return structured data showing patterns-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

