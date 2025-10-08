# Transaction Analysis - Edge_Cases Questions
**Category:** 01_transaction_analysis
**Focus:** Transaction queries, fees, instructions, CPIs
**Level:** Edge_Cases
**Topic:** Unusual transaction scenarios
**Questions:** Q801-Q900

---

## Q801: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q802: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q803: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q804: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q805: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q806: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q807: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q808: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q809: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q810: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q811: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q812: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q813: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q814: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q815: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q816: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q817: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q818: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q819: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q820: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q821: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q822: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q823: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q824: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q825: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q826: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q827: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q828: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q829: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q830: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q831: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q832: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q833: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q834: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q835: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q836: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q837: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q838: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q839: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q840: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q841: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q842: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q843: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q844: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q845: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q846: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q847: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q848: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q849: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q850: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q851: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q852: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q853: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q854: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q855: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q856: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q857: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q858: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q859: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q860: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q861: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q862: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q863: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q864: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q865: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q866: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q867: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q868: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q869: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q870: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q871: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q872: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q873: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q874: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q875: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q876: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q877: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q878: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q879: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q880: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q881: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q882: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q883: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q884: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q885: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q886: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q887: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q888: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q889: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q890: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q891: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q892: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q893: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q894: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q895: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q896: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "sig"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q897: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "addr"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

## Q898: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q899: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
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

## Q900: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~1-5min] [COST: ~0.01 SOL]

**Available Tools:**
From Standard Library:
  - getTransaction, getRecentBlockhash, simulateTransaction
  - JSON parser, filter, aggregator
  - Math operations (sum, avg, max, min)

**Main Branch:**
```
$signature = "slot"
$result = TOOL.getTransaction($signature)

DECISION: Check if result exists
  BRANCH A (result found):
    $data = PARSE($result)
    $metrics = ANALYZE($data)
    OUTPUT: $metrics

  BRANCH B (not found):
    ERROR: "Resource not found"
    OUTPUT: null
```

**Decision Point:** Does the query return valid data?
  BRANCH A (valid data):
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

