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

---

## Q2: "Show me all token accounts owned by address 7cvk..."

**Expected Plan:**

[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner (Solana RPC)
  - MAP, FILTER, COUNT (Data Processing)

**Main Branch:**
$owner = "7cvkjYAkUYs4W8XcXsca7cBrEGFeSUjeZmKoNBvXyzgv"

$token_accounts = getTokenAccountsByOwner(owner: $owner, programId: TOKEN_PROGRAM)

$active_accounts = FILTER(
  collection: $token_accounts,
  predicate: acc => acc.account.data.parsed.info.tokenAmount.uiAmount > 0
)

**Decision Point:** Check if tokens found
  BRANCH A (COUNT($active_accounts) > 0):
    $has_tokens = true
  BRANCH B (COUNT($active_accounts) == 0):
    $has_tokens = false

**Action:**
RETURN {
  owner: $owner,
  token_accounts: MAP($active_accounts, acc => {mint: acc.account.data.parsed.info.mint, amount: acc.account.data.parsed.info.tokenAmount.uiAmount}),
  total: COUNT(collection: $active_accounts),
  confidence: 100
}

[Q3-Q100 continue with proper OVSM syntax...]
