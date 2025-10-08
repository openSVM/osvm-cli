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

## Q4: "Is account XYZ rent exempt?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - COUNT (Data Processing)

```ovsm
**Main Branch:**
$account_address = "XYZ"

$account = getAccountInfo(pubkey: $account_address)

$balance_lamports = $account.lamports
$data_size = COUNT(collection: $account.data)

// Rent calculation: 0.00089088 SOL base + 0.00000348 SOL per byte
$rent_exempt_minimum = 890880 + ($data_size * 348)

**Decision Point:** Check rent exemption
  BRANCH A ($balance_lamports >= $rent_exempt_minimum):
    $is_rent_exempt = true
    $surplus = $balance_lamports - $rent_exempt_minimum
    $status = "rent_exempt"
  BRANCH B ($balance_lamports < $rent_exempt_minimum):
    $is_rent_exempt = false
    $deficit = $rent_exempt_minimum - $balance_lamports
    $status = "at_risk"

**Action:**
RETURN {
  account: $account_address,
  balance_lamports: $balance_lamports,
  data_size_bytes: $data_size,
  rent_exempt_minimum: $rent_exempt_minimum,
  is_rent_exempt: $is_rent_exempt,
  surplus_or_deficit: $is_rent_exempt ? $surplus : -$deficit,
  status: $status,
  confidence: 100
}
```

---

## Q5: "What is the data size of account DEF456?"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - COUNT (Data Processing)

```ovsm
**Main Branch:**
$account_address = "DEF456"

$account = getAccountInfo(pubkey: $account_address)

GUARD $account != null ELSE
  RETURN ERROR(message: "Account not found")

$data_bytes = COUNT(collection: $account.data)
$data_kb = $data_bytes / 1024

**Decision Point:** Categorize data size
  BRANCH A ($data_bytes == 0):
    $size_category = "empty"
  BRANCH B ($data_bytes < 1024):
    $size_category = "small"  
  BRANCH C ($data_bytes >= 1024 AND $data_bytes < 10240):
    $size_category = "medium"
  BRANCH D ($data_bytes >= 10240):
    $size_category = "large"

**Action:**
RETURN {
  account: $account_address,
  data_size_bytes: $data_bytes,
  data_size_kb: $data_kb,
  size_category: $size_category,
  confidence: 100
}
```

---

[Q6-Q100 to be added - file growing incrementally]

## Q6: "Does address GHI789 own any NFTs?"

**Expected Plan:**

[TIME: ~4s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getTokenAccountsByOwner (Solana RPC)
  - FILTER, COUNT, MAP (Data Processing)

```ovsm
**Main Branch:**
$owner = "GHI789"

$token_accounts = getTokenAccountsByOwner(
  owner: $owner,
  programId: TOKEN_PROGRAM
)

// NFTs have amount=1 and decimals=0
$nft_accounts = FILTER(
  collection: $token_accounts,
  predicate: acc =>
    acc.account.data.parsed.info.tokenAmount.decimals == 0 AND
    acc.account.data.parsed.info.tokenAmount.uiAmount == 1
)

**Decision Point:** Check if NFTs found
  BRANCH A (COUNT($nft_accounts) > 0):
    $owns_nfts = true
    $nft_list = MAP($nft_accounts, acc => acc.account.data.parsed.info.mint)
  BRANCH B (COUNT($nft_accounts) == 0):
    $owns_nfts = false
    $nft_list = []

**Action:**
RETURN {
  owner: $owner,
  owns_nfts: $owns_nfts,
  nft_count: COUNT(collection: $nft_accounts),
  nft_mints: $nft_list,
  confidence: 95
}
```

---

## Q7: "Show me the top 10 largest accounts by SOL balance"

**Expected Plan:**

[TIME: ~5s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getLargestAccounts (Solana RPC)
  - SLICE, MAP (Data Processing)

```ovsm
**Main Branch:**
$all_largest = getLargestAccounts(limit: 20)

$top_10 = SLICE(array: $all_largest, start: 0, end: 10)

$formatted = MAP(
  collection: $top_10,
  fn: acc => {
    address: acc.address,
    balance_lamports: acc.lamports,
    balance_sol: acc.lamports / LAMPORTS_PER_SOL
  }
)

**Action:**
RETURN {
  top_accounts: $formatted,
  count: COUNT(collection: $formatted),
  confidence: 100
}
```

---

## Q8: "List all staking accounts for wallet JKL012"

**Expected Plan:**

[TIME: ~6s] [COST: ~0.001 SOL] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts (Solana RPC)
  - FILTER, MAP, SUM (Data Processing)

```ovsm
**Main Branch:**
CONST STAKE_PROGRAM = "Stake11111111111111111111111111111111111111"

$wallet = "JKL012"

$all_stake_accounts = getProgramAccounts(
  programId: STAKE_PROGRAM,
  filters: [{dataSize: 200}]
)

// Filter for this wallet's stake accounts
$wallet_stakes = FILTER(
  collection: $all_stake_accounts,
  predicate: acc =>
    acc.account.data.parsed.info.meta.authorized.staker == $wallet OR
    acc.account.data.parsed.info.meta.authorized.withdrawer == $wallet
)

$total_staked = SUM(
  collection: MAP($wallet_stakes, acc => acc.account.lamports)
)

**Decision Point:** Check for active stakes
  BRANCH A (COUNT($wallet_stakes) > 0):
    $active_stakes = FILTER(
      $wallet_stakes,
      s => s.account.data.parsed.info.stake?.delegation?.active == true
    )
    $active_count = COUNT(collection: $active_stakes)
  BRANCH B (COUNT($wallet_stakes) == 0):
    $active_count = 0

**Action:**
RETURN {
  wallet: $wallet,
  stake_accounts: MAP($wallet_stakes, acc => {
    address: acc.pubkey,
    balance_lamports: acc.account.lamports,
    active: acc.account.data.parsed.info.stake?.delegation?.active ?? false
  }),
  total_accounts: COUNT(collection: $wallet_stakes),
  active_accounts: $active_count,
  total_staked_lamports: $total_staked,
  total_staked_sol: $total_staked / LAMPORTS_PER_SOL,
  confidence: 100
}
```

---

## Q9: "Derive the PDA for seeds ['metadata', mint_address] with TOKEN_METADATA_PROGRAM"

**Expected Plan:**

[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - derivePDA (Solana Utility)

```ovsm
**Main Branch:**
CONST TOKEN_METADATA_PROGRAM = "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s"

$mint_address = INPUT(prompt: "mint_address")

$seeds = [
  "metadata",
  TOKEN_METADATA_PROGRAM,
  $mint_address
]

$pda_address = derivePDA(
  seeds: $seeds,
  program: TOKEN_METADATA_PROGRAM
)

**Action:**
RETURN {
  pda_address: $pda_address,
  seeds: $seeds,
  program: TOKEN_METADATA_PROGRAM,
  mint: $mint_address,
  note: "This is the derived address for Metaplex NFT metadata",
  confidence: 100
}
```

---

## Q10: "Get Associated Token Account address for owner MNO345 and mint PQR678"

**Expected Plan:**

[TIME: ~1s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - deriveATA (Solana Utility)
  - getAccountInfo (Solana RPC)

```ovsm
**Main Branch:**
$owner = "MNO345"
$mint = "PQR678"

$ata_address = deriveATA(
  owner: $owner,
  mint: $mint
)

// Check if ATA exists
TRY:
  $ata_account = getAccountInfo(pubkey: $ata_address)
  $exists = true
  $balance = $ata_account.data.parsed.info.tokenAmount.uiAmount
CATCH:
  $exists = false
  $balance = 0

**Decision Point:** Check if ATA exists
  BRANCH A ($exists == true):
    $status = "exists"
    $message = "ATA found with balance: {$balance}"
  BRANCH B ($exists == false):
    $status = "not_created"
    $message = "ATA address derived but account doesn't exist yet"

**Action:**
RETURN {
  ata_address: $ata_address,
  owner: $owner,
  mint: $mint,
  exists: $exists,
  balance: $balance,
  status: $status,
  message: $message,
  confidence: 100
}
```

---

[Q11-Q100 continue...]

## Q11: "What is the transaction history for account STU901?"

**Expected Plan:**

[TIME: ~8s] [COST: free] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - SLICE, COUNT, MAP (Data Processing)

```ovsm
**Main Branch:**
$address = "STU901"

$all_signatures = getSignaturesForAddress(
  address: $address,
  limit: 1000
)

// Take most recent 50
$recent_50 = SLICE(array: $all_signatures, start: 0, end: 50)

**Decision Point:** Analyze activity level
  BRANCH A (COUNT($all_signatures) > 100):
    $activity_level = "high"
    $note = "Showing last 50 of {COUNT($all_signatures)} transactions"
  BRANCH B (COUNT($all_signatures) >= 10 AND COUNT($all_signatures) <= 100):
    $activity_level = "moderate"
    $note = "Complete history shown"
  BRANCH C (COUNT($all_signatures) < 10):
    $activity_level = "low"
    $note = "Low activity account"

**Action:**
RETURN {
  address: $address,
  total_transactions: COUNT(collection: $all_signatures),
  recent_transactions: MAP($recent_50, sig => {
    signature: sig.signature,
    slot: sig.slot,
    block_time: sig.blockTime,
    status: sig.err == null ? "success" : "failed"
  }),
  activity_level: $activity_level,
  note: $note,
  confidence: 95
}
```

---

## Q12: "Check if account VWX234 is a program (executable)"

**Expected Plan:**

[TIME: ~2s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)

```ovsm
**Main Branch:**
$account_address = "VWX234"

TRY:
  $account = getAccountInfo(pubkey: $account_address)
CATCH FATAL:
  RETURN ERROR(message: "Account not found")

$is_executable = $account.executable
$owner = $account.owner
$data_size = COUNT(collection: $account.data)

**Decision Point:** Determine account type
  BRANCH A ($is_executable == true):
    $account_type = "executable_program"
    $info = "Contains BPF bytecode ({$data_size} bytes)"
  BRANCH B ($is_executable == false):
    $account_type = "data_account"
    $info = "Non-executable data account ({$data_size} bytes)"

**Action:**
RETURN {
  account: $account_address,
  executable: $is_executable,
  account_type: $account_type,
  owner: $owner,
  data_size: $data_size,
  info: $info,
  confidence: 100
}
```

---

[Continuing with Q13-Q100 - creating batch additions to reach 100 total questions with proper OVSM logic and ```ovsm blocks]

## Q13: "How many SOL does the top holder have?"

**Expected Plan:**
[TIME: ~3s] [COST: free] [CONFIDENCE: 100%]

**Available Tools:**
From Standard Library: getLargestAccounts (Solana RPC), FIRST

```ovsm
**Main Branch:**
$largest = getLargestAccounts(limit: 1)
$top_account = FIRST(array: $largest)

**Action:**
RETURN {
  address: $top_account.address,
  balance_sol: $top_account.lamports / LAMPORTS_PER_SOL,
  confidence: 100
}
```

---

## Q14: "Find all program-owned accounts for program ID ABC"

**Expected Plan:**
[TIME: ~10s] [COST: ~0.002 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: getProgramAccounts, COUNT, SLICE

```ovsm
**Main Branch:**
$program_id = "ABC"
$accounts = getProgramAccounts(programId: $program_id)

**Decision Point:** Handle large result sets
  BRANCH A (COUNT($accounts) > 100):
    $result = SLICE(array: $accounts, start: 0, end: 100)
    $note = "Showing first 100 of {COUNT($accounts)}"
  BRANCH B (COUNT($accounts) <= 100):
    $result = $accounts
    $note = "Complete list"

**Action:**
RETURN {
  program: $program_id,
  accounts: $result,
  total: COUNT($accounts),
  showing: COUNT($result),
  note: $note,
  confidence: 90
}
```

---

[Q15-Q100 pattern continues - adding in next batch]
