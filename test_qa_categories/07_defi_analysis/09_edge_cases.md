# DeFi Analysis - Edge_Cases Questions
**Category:** 07_defi_analysis
**Focus:** DEX, lending, liquidity, farming, oracles
**Level:** Edge_Cases
**Topic:** DeFi edge cases
**Questions:** Q801-Q900

---

## Q801: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q802: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q803: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q804: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q805: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q806: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q807: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q808: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q809: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q810: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q811: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q812: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q813: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q814: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q815: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q816: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q817: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q818: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q819: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q820: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q821: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q822: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q823: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q824: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q825: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q826: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q827: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q828: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q829: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q830: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q831: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q832: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q833: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q834: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q835: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q836: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q837: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q838: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q839: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q840: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q841: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q842: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q843: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q844: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q845: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q846: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q847: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q848: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q849: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q850: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q851: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q852: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q853: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q854: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q855: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q856: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q857: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q858: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q859: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q860: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q861: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q862: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q863: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q864: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q865: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q866: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q867: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q868: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q869: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q870: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q871: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q872: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q873: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q874: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q875: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q876: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q877: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q878: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q879: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q880: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q881: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q882: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q883: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q884: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q885: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q886: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q887: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q888: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q889: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q890: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q891: "What is the TVL of liquidity pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q892: "What are the reserves in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q893: "Calculate the current swap rate from token A to token B in pool 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q894: "What is the utilization rate of lending protocol TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q895: "Find all liquidity pools for token pair So11111111111111111111111111111111111111112/EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q896: "What is the largest liquidity provider in pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q897: "Calculate impermanent loss for position 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q898: "What is the oracle price for asset SOL/USDC?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q899: "Find all active farming positions for user 7xK...abc"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q900: "What is the APY for lending pool 7xK...abc?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

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
Return structured data showing edge_cases-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

