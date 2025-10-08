# Account State - Analysis Questions
**Category:** 02_account_state
**Focus:** Balances, ownership, PDAs, stake accounts
**Level:** Analysis
**Topic:** Account state evolution analysis
**Questions:** Q301-Q400

---

## Q301: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q302: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q303: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q304: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q305: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q306: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q307: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q308: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q309: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q310: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q311: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q312: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q313: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q314: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q315: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q316: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q317: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q318: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q319: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q320: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q321: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q322: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q323: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q324: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q325: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q326: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q327: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q328: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q329: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q330: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q331: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q332: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q333: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q334: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q335: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q336: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q337: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q338: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q339: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q340: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q341: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q342: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q343: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q344: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q345: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q346: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q347: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q348: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q349: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q350: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q351: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q352: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q353: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q354: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q355: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q356: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q357: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q358: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q359: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q360: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q361: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q362: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q363: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q364: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q365: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q366: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q367: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q368: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q369: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q370: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q371: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q372: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q373: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q374: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q375: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q376: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q377: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q378: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q379: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q380: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q381: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q382: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q383: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q384: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q385: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q386: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q387: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q388: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q389: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q390: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q391: "What is the SOL balance of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q392: "Who is the owner of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q393: "Is account 7xK...abc executable?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q394: "What is the data size of account 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q395: "Derive the PDA for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA with seeds [b'metadata', user.key().as_ref()]"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q396: "Find all token accounts owned by 7xK...abc"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q397: "What is the rent-exempt minimum for account size 1024?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "size"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q398: "List all accounts owned by program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "program"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q399: "What is the stake account balance for 7xK...abc?"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "addr"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q400: "Find accounts with balance greater than 10.5 SOL"

**Expected Plan:**

[TIME: ~10-30s] [COST: ~0.001 SOL]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getBalance, getProgramAccounts
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "amount"
$result = getAccountInfo($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    // Data is already parsed in $result
    // Extract specific metrics from $result
    RETURN {
  // Add specific fields here
}

  BRANCH B (not found):
    ERROR: "Resource not found"
    RETURN ERROR(message: "Not found")


**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

