# Token Research - Basic Questions
**Category:** 06_token_research
**Focus:** Token supply, holders, metadata, transfers
**Level:** Basic
**Topic:** Basic token queries
**Questions:** Q1-Q100

---

## Q1: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q2: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q3: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q4: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q5: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q6: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q7: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q8: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q9: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q10: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q11: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q12: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q13: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q14: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q15: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q16: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q17: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q18: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q19: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q20: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q21: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q22: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q23: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q24: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q25: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q26: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q27: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q28: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q29: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q30: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q31: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q32: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q33: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q34: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q35: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q36: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q37: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q38: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q39: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q40: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q41: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q42: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q43: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q44: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q45: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q46: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q47: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q48: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q49: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q50: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q51: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q52: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q53: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q54: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q55: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q56: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q57: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q58: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q59: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q60: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q61: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q62: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q63: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q64: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q65: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q66: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q67: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q68: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q69: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q70: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q71: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q72: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q73: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q74: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q75: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q76: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q77: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q78: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q79: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q80: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q81: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q82: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q83: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q84: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q85: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q86: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q87: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q88: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q89: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q90: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q91: "What is the total supply of token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q92: "How many holders does token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v have?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q93: "What is the largest token account for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q94: "Find all token accounts for owner 7xK...abc"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q95: "What is the metadata for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q96: "Calculate the token distribution (Gini coefficient) for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q97: "How many token transfers occurred for EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v in the last 100 slots?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q98: "What is the token balance of account 7xK...abc for mint EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q99: "Find tokens with supply less than 10.5"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q100: "What is the mint authority for token EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v?"

**Expected Plan:**

[TIME: ~5-10s] [COST: free]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner, getTokenSupply, getTokenAccountBalance
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "mint"
$result = getTokenAccountsByOwner($signature)

DECISION: Check if result exists
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
Return structured data showing basic-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

