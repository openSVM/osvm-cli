# Transaction Analysis - Optimization Questions
**Category:** 01_transaction_analysis
**Focus:** Transaction queries, fees, instructions, CPIs
**Level:** Optimization
**Topic:** Fee optimization and batching strategies
**Questions:** Q501-Q600

---

## Q501: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q502: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q503: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q504: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q505: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q506: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q507: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q508: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q509: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q510: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q511: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q512: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q513: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q514: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q515: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q516: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q517: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q518: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q519: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q520: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q521: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q522: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q523: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q524: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q525: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q526: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q527: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q528: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q529: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q530: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q531: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q532: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q533: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q534: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q535: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q536: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q537: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q538: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q539: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q540: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q541: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q542: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q543: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q544: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q545: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q546: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q547: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q548: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q549: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q550: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q551: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q552: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q553: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q554: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q555: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q556: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q557: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q558: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q559: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q560: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q561: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q562: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q563: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q564: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q565: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q566: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q567: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q568: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q569: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q570: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q571: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q572: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q573: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q574: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q575: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q576: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q577: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q578: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q579: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q580: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q581: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q582: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q583: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q584: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q585: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q586: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q587: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q588: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q589: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q590: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q591: "What is the total fee paid for transaction signature 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q592: "How many instructions are in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q593: "Which programs were invoked in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q594: "What is the CPI depth of transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q595: "How many compute units did transaction 5J8..xyz consume?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q596: "What accounts were written to in transaction 5J8..xyz?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q597: "Find all failed transactions for address 7xK...abc in the last 100 slots"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q598: "Calculate the average transaction fee for program TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA in the last hour"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q599: "Identify transactions with CPI depth greater than 100"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

## Q600: "What is the most expensive transaction in block 155000000?"

**Expected Plan:**

[TIME: ~30-60s] [COST: ~0.005 SOL]

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
Return structured data showing optimization-level analysis of the requested blockchain resource with appropriate error handling and validation.

---

