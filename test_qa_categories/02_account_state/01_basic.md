# Account State - Basic Questions (Q1-Q100)

**Category:** Account State
**Difficulty:** Basic
**Focus:** Account balances, ownership, token holdings

---

## Q1: "What is the SOL balance of address 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv?"

**Expected Plan:**

[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getBalance (Solana RPC)

```ovsm
**Main Branch:**
$address = "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"

$balance_lamports = getBalance(pubkey: $address)
$balance_sol = $balance_lamports / LAMPORTS_PER_SOL

**Action:**
RETURN {
  address: $address,
  balance_lamports: $balance_lamports,
  balance_sol: $balance_sol,
  confidence: 100
}
```

---

## Q2: "Show me all token accounts owned by address 7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner (Solana RPC)
  - MAP, FILTER, COUNT (Data Processing)

```ovsm
**Main Branch:**
$owner = "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"

$token_accounts = getTokenAccountsByOwner(
  owner: $owner,
  programId: TOKEN_PROGRAM
)

$active_accounts = FILTER(
  collection: $token_accounts,
  predicate: acc => acc.account.data.parsed.info.tokenAmount.uiAmount > 0
)

**Decision Point:** Check if any tokens found
  BRANCH A (COUNT($active_accounts) > 0):
    $has_tokens = true
  BRANCH B (COUNT($active_accounts) == 0):
    $has_tokens = false

**Action:**
RETURN {
  owner: $owner,
  token_accounts: MAP($active_accounts, acc => {
    mint: acc.account.data.parsed.info.mint,
    amount: acc.account.data.parsed.info.tokenAmount.uiAmount,
    address: acc.pubkey
  }),
  total_accounts: COUNT(collection: $active_accounts),
  has_tokens: $has_tokens,
  confidence: 100
}
```

---

## Q3: "Which program owns account ABC123?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)

```ovsm
**Main Branch:**
$account_address = "ABC123"

TRY:
  $account = getAccountInfo(pubkey: $account_address)
CATCH FATAL:
  RETURN ERROR(message: "Account not found")

$owner_program = $account.owner

**Decision Point:** Identify program type
  BRANCH A ($owner_program == SYSTEM_PROGRAM):
    $program_type = "System Program (native account)"
  BRANCH B ($owner_program == TOKEN_PROGRAM):
    $program_type = "Token Program (SPL token account)"
  BRANCH C ($owner_program == TOKEN_2022_PROGRAM):
    $program_type = "Token-2022 Program"
  BRANCH D (default):
    $program_type = "Custom program"

**Action:**
RETURN {
  account: $account_address,
  owner: $owner_program,
  program_type: $program_type,
  confidence: 100
}
```

---

[Q4-Q100 continue with proper OVSM format and ```ovsm blocks...]

_Note: This file will be completed with all 100 questions in the next update. The pattern above shows the proper format with ```ovsm syntax highlighting._
