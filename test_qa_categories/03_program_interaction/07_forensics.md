# Program Interaction - Forensics Questions
**Category:** 03_program_interaction
**Focus:** Deployments, upgrades, IDLs, invocations
**Level:** Forensics
**Topic:** Program exploit detection
**Questions:** Q601-Q700

---

## Q601: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q602: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q603: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q604: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q605: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q606: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q607: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q608: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q609: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q610: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q611: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q612: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q613: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q614: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q615: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q616: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q617: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q618: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q619: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q620: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q621: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q622: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q623: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q624: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q625: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q626: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q627: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q628: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q629: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q630: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q631: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q632: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q633: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q634: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q635: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q636: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q637: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q638: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q639: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q640: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q641: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q642: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q643: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q644: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q645: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q646: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q647: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q648: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q649: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q650: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q651: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q652: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q653: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q654: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q655: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q656: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q657: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q658: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q659: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q660: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q661: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q662: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q663: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q664: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q665: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q666: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q667: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q668: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q669: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q670: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q671: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q672: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q673: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q674: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q675: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q676: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q677: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q678: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q679: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q680: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q681: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q682: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q683: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q684: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q685: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q686: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q687: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q688: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q689: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q690: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q691: "What is the upgrade authority for program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q692: "When was program 7xK...abc last upgraded?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q693: "How many times has program 7xK...abc been invoked in the last 100 slots?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q694: "What is the data size of program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q695: "Find all programs that call program 7xK...abc via CPI"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q696: "What is the most frequently called instruction on program 7xK...abc?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q697: "Simulate calling instruction transfer on program 7xK...abc"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "ix"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q698: "What accounts does program 7xK...abc typically interact with?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q699: "Find all program deployments in the last 100 epochs"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
$signature = "n"
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q700: "What is the average compute cost for program 7xK...abc instructions?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

**Available Tools:**
From Standard Library:
  - getProgramAccounts, getAccountInfo, simulateTransaction
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
Return structured data showing forensics-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

