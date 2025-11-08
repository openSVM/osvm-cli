# OSVM-MCP Complete Tool Reference

This document provides a complete reference for all 75+ tools available through the osvm-mcp server.

**Discovery:** Tools are **automatically discovered** at runtime - no need to manually update this list when osvm-mcp adds new tools!

```bash
# See what tools are available
osvm ovsm run script.ovsm --debug
# Output: 📦 Discovered 75 tools from MCP server 'osvm-mcp'
```

## Tool Categories

1. [Account Tools](#account-tools) (6 tools)
2. [Transaction Tools](#transaction-tools) (4 tools)
3. [Block Tools](#block-tools) (3 tools)
4. [Analytics Tools](#analytics-tools) (4 tools)
5. [Token & NFT Tools](#token--nft-tools) (4 tools)
6. [Search Tools](#search-tools) (2 tools)
7. [Utility Tools](#utility-tools) (5 tools)
8. [Monetization Tools](#monetization-tools) (3 tools)
9. [Direct RPC Access](#direct-rpc-access) (90+ Solana RPC methods)

---

## Account Tools

### 1. `get_account_stats`
**Purpose:** Get transaction statistics for an account

**Parameters:**
```lisp
{:address "wallet_address_here"}
```

**Example:**
```lisp
(define stats (get_account_stats {:address "vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"}))
(log :message "Total transactions:" :value (. stats totalTransactions))
(log :message "Token transfers:" :value (. stats tokenTransfers))
```

**Returns:**
```json
{
  "totalTransactions": 1234,
  "tokenTransfers": 567,
  "nftTransfers": 89
}
```

---

### 2. `get_account_portfolio`
**Purpose:** Complete portfolio with SOL balance, tokens, prices, and total value

**Parameters:**
```lisp
{:address "wallet_address_here"}
```

**Example:**
```lisp
(define portfolio (get_account_portfolio {:address "vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"}))
(define sol_balance (. (. portfolio native) balance))
(define sol_value (. (. portfolio native) value))
(log :message "SOL Balance:" :value sol_balance)
(log :message "SOL Value (USD):" :value sol_value)
(log :message "Total Portfolio Value:" :value (. portfolio totalValue))
```

**Returns:**
```json
{
  "native": {
    "balance": 1.5,
    "symbol": "SOL",
    "price": 150.50,
    "value": 225.75
  },
  "tokens": [
    {
      "mint": "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v",
      "symbol": "USDC",
      "balance": 1000,
      "price": 1.0,
      "value": 1000
    }
  ],
  "totalValue": 1225.75
}
```

---

### 3. `get_solana_balance`
**Purpose:** Get SOL balance and token holdings (alias for get_account_portfolio)

**Example:**
```lisp
(define balance (get_solana_balance {:address "wallet_address"}))
```

---

### 4. `get_account_transactions`
**Purpose:** Paginated transaction history for an account

**Parameters:**
```lisp
{:address "wallet_address" :limit 50 :before "cursor_signature"}
```

**Example:**
```lisp
(define txs (get_account_transactions {:address "vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg" :limit 20}))
(for (tx txs)
  (log :message "Signature:" :value (. tx signature))
  (log :message "Block Time:" :value (. tx blockTime)))
```

---

### 5. `get_account_token_stats`
**Purpose:** Token-specific statistics for an account

**Parameters:**
```lisp
{:address "wallet_address" :mint "token_mint_address"}
```

**Example:**
```lisp
(define stats (get_account_token_stats {
  :address "vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"
  :mint "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
}))
(log :message "Token transfers:" :value (. stats transfers))
```

---

### 6. `check_account_type`
**Purpose:** Identify account type (wallet, program, token mint, etc.)

**Parameters:**
```lisp
{:address "account_address"}
```

**Example:**
```lisp
(define account_type (check_account_type {:address "11111111111111111111111111111111"}))
(log :message "Account type:" :value (. account_type type))
;; Returns: "program", "wallet", "token_mint", "token_account", etc.
```

---

## Transaction Tools

### 7. `get_transaction`
**Purpose:** Get detailed transaction with instructions, accounts, balances, logs

**Parameters:**
```lisp
{:signature "transaction_signature"}
```

**Example:**
```lisp
(define tx (get_transaction {
  :signature "5j7s6NiJS3JAkvgkoc18WVAsiSaci2pxB2A6ueCJP4tprA2TFg9wSyTLeYouxPBJEMzJinENTkpA52YStRW5Dia7"
}))

(log :message "Slot:" :value (. tx slot))
(log :message "Block time:" :value (. tx blockTime))
(log :message "Fee (lamports):" :value (. tx fee))
(log :message "Success:" :value (null? (. (. tx meta) err)))

;; Iterate instructions
(for (ix (. tx instructions))
  (log :message "Program:" :value (. ix programId)))
```

---

### 8. `batch_transactions`
**Purpose:** Fetch up to 20 transactions in one call

**Parameters:**
```lisp
{:signatures ["sig1" "sig2" "sig3"]}
```

**Example:**
```lisp
(define sigs ["sig1..." "sig2..." "sig3..."])
(define transactions (batch_transactions {:signatures sigs}))
(log :message "Fetched:" :value (length transactions))
```

---

### 9. `analyze_transaction`
**Purpose:** AI-powered transaction analysis (programs, tokens, risk assessment)

**Parameters:**
```lisp
{:signature "transaction_signature"}
```

**Example:**
```lisp
(define analysis (analyze_transaction {
  :signature "5j7s6NiJS3JAkvgkoc18WVAsiSaci2pxB2A6ueCJP4tprA2TFg9wSyTLeYouxPBJEMzJinENTkpA52YStRW5Dia7"
}))

(log :message "Programs involved:" :value (. analysis programs))
(log :message "Token transfers:" :value (. analysis tokenTransfers))
(log :message "Risk score:" :value (. analysis riskScore))
```

---

### 10. `explain_transaction`
**Purpose:** Natural language explanation of what the transaction does

**Parameters:**
```lisp
{:signature "transaction_signature"}
```

**Example:**
```lisp
(define explanation (explain_transaction {
  :signature "5j7s6NiJS3JAkvgkoc18WVAsiSaci2pxB2A6ueCJP4tprA2TFg9wSyTLeYouxPBJEMzJinENTkpA52YStRW5Dia7"
}))

(log :message (. explanation text))
;; Output: "This transaction swapped 10 USDC for 0.05 SOL on Jupiter DEX..."
```

---

## Block Tools

### 11. `get_block`
**Purpose:** Get block data by slot number

**Parameters:**
```lisp
{:slot 378805000}
```

**Example:**
```lisp
(define current_slot (getSlot))
(define block (get_block {:slot current_slot}))

(log :message "Block time:" :value (. block blockTime))
(log :message "Block height:" :value (. block blockHeight))
(log :message "Parent slot:" :value (. block parentSlot))
(log :message "Transactions:" :value (length (. block transactions)))
```

---

### 12. `get_recent_blocks`
**Purpose:** List recent blocks with pagination

**Parameters:**
```lisp
{:limit 20 :before 378805000}
```

**Example:**
```lisp
(define blocks (get_recent_blocks {:limit 10}))
(for (block blocks)
  (log :message "Slot:" :value (. block slot))
  (log :message "Transactions:" :value (. block txCount)))
```

---

### 13. `get_block_stats`
**Purpose:** Network performance metrics (TPS, block time, etc.)

**Example:**
```lisp
(define stats (get_block_stats {}))
(log :message "Current TPS:" :value (. stats tps))
(log :message "Avg block time:" :value (. stats avgBlockTime))
(log :message "Blocks per second:" :value (. stats blocksPerSecond))
```

---

## Analytics Tools

### 14. `get_defi_overview`
**Purpose:** DeFi ecosystem overview (TVL, volume, top protocols)

**Example:**
```lisp
(define defi (get_defi_overview {}))

(log :message "Total TVL:" :value (. defi totalTvl))
(log :message "24h Volume:" :value (. defi totalVolume24h))
(log :message "Active DEXes:" :value (. defi activeDexes))

(for (protocol (. defi topProtocols))
  (log :message "Protocol:" :value (. protocol name))
  (log :message "TVL:" :value (. protocol tvl)))
```

**Returns:**
```json
{
  "totalTvl": 1385500000,
  "totalVolume24h": 2703866854.95,
  "activeDexes": 41,
  "topProtocols": [
    {"name": "raydium", "tvl": 950000000, "volume24h": 647361015},
    {"name": "jupiter", "tvl": 800000000, "volume24h": 450000000}
  ]
}
```

---

### 15. `get_dex_analytics`
**Purpose:** DEX-specific trading analytics

**Parameters:**
```lisp
{:dex "raydium" :timeframe "24h"}
```

**Example:**
```lisp
(define analytics (get_dex_analytics {:dex "raydium"}))
(log :message "Volume:" :value (. analytics volume24h))
(log :message "Trades:" :value (. analytics trades24h))
```

---

### 16. `get_defi_health`
**Purpose:** DeFi ecosystem health indicators

**Example:**
```lisp
(define health (get_defi_health {}))
(log :message "Health score:" :value (. health score))
(log :message "Active protocols:" :value (. health activeProtocols))
```

---

### 17. `get_validator_analytics`
**Purpose:** Validator network statistics

**Example:**
```lisp
(define validators (get_validator_analytics {}))
(log :message "Total validators:" :value (. validators total))
(log :message "Active validators:" :value (. validators active))
(log :message "Avg commission:" :value (. validators avgCommission))
```

---

## Token & NFT Tools

### 18. `get_token_info`
**Purpose:** SPL token metadata, supply, decimals, holders

**Parameters:**
```lisp
{:mint "token_mint_address"}
```

**Example:**
```lisp
(define token (get_token_info {:mint "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"}))

(log :message "Symbol:" :value (. token symbol))
(log :message "Name:" :value (. token name))
(log :message "Decimals:" :value (. token decimals))
(log :message "Supply:" :value (. token supply))
(log :message "Holders:" :value (. token holders))
```

---

### 19. `get_token_metadata`
**Purpose:** Batch fetch metadata for multiple tokens

**Parameters:**
```lisp
{:mints ["mint1" "mint2" "mint3"]}
```

**Example:**
```lisp
(define mints [
  "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
  "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB"
])

(define tokens (get_token_metadata {:mints mints}))
(for (token tokens)
  (log :message "Symbol:" :value (. token symbol)))
```

---

### 20. `get_nft_collections`
**Purpose:** NFT collection stats (floor price, volume, holders)

**Parameters:**
```lisp
{:limit 20}
```

**Example:**
```lisp
(define collections (get_nft_collections {:limit 10}))
(for (collection collections)
  (log :message "Name:" :value (. collection name))
  (log :message "Floor price:" :value (. collection floorPrice))
  (log :message "24h volume:" :value (. collection volume24h)))
```

---

### 21. `get_trending_nfts`
**Purpose:** Trending NFT collections by volume

**Example:**
```lisp
(define trending (get_trending_nfts {}))
(for (nft trending)
  (log :message "Collection:" :value (. nft name))
  (log :message "Volume:" :value (. nft volume)))
```

---

## Search Tools

### 22. `universal_search`
**Purpose:** Search across accounts, transactions, tokens, programs

**Parameters:**
```lisp
{:query "search_term" :types ["account" "transaction" "token"]}
```

**Example:**
```lisp
(define results (universal_search {:query "pump" :types ["token"]}))
(log :message "Found:" :value (length results))
```

---

### 23. `search_accounts`
**Purpose:** Advanced account search with balance/token filters

**Parameters:**
```lisp
{:minBalance 1000000000 :tokenMint "usdc_mint"}
```

**Example:**
```lisp
(define accounts (search_accounts {:minBalance 1000000000}))
(log :message "Accounts with > 1 SOL:" :value (length accounts))
```

---

## Utility Tools

### 24. `solana_rpc_call`
**Purpose:** Direct access to 90+ Solana RPC methods

**Parameters:**
```lisp
{:method "method_name" :params [param1 param2]}
```

**Example:**
```lisp
;; Get slot
(define slot_result (solana_rpc_call {:method "getSlot"}))
(define slot (. slot_result result))

;; Get block height
(define height_result (solana_rpc_call {:method "getBlockHeight"}))
(define height (. height_result result))

;; Get balance
(define balance_result (solana_rpc_call {
  :method "getBalance"
  :params ["vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"]
}))
(define balance (. (. balance_result result) value))

;; Get account info
(define account_result (solana_rpc_call {
  :method "getAccountInfo"
  :params ["vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"]
}))
```

**Available RPC methods (90+):**
- Account methods: `getAccountInfo`, `getBalance`, `getMultipleAccounts`, `getProgramAccounts`, `getTokenAccountsByOwner`
- Block methods: `getBlock`, `getBlockHeight`, `getBlockProduction`, `getBlockCommitment`, `getBlocks`, `getBlocksWithLimit`, `getBlockTime`
- Transaction methods: `getSignaturesForAddress`, `getTransaction`, `getConfirmedTransaction`, `simulateTransaction`
- Network methods: `getSlot`, `getHealth`, `getVersion`, `getEpochInfo`, `getLeaderSchedule`
- And many more...

---

### 25. `verify_wallet_signature`
**Purpose:** Verify wallet signatures for authentication

**Parameters:**
```lisp
{:address "wallet_address" :signature "signature_hex" :message "message"}
```

**Example:**
```lisp
(define verified (verify_wallet_signature {
  :address "vines1vzrYbzLMRdu58ou5XTby4qAqVRLmqo36NKPTg"
  :signature "signature_hex_string"
  :message "Login to OpenSVM"
}))

(if (. verified valid)
    (log :message "Signature valid!")
    (log :message "Signature invalid"))
```

---

### 26. `get_program_registry`
**Purpose:** List registered Solana programs

**Example:**
```lisp
(define programs (get_program_registry {}))
(for (program programs)
  (log :message "Program ID:" :value (. program programId))
  (log :message "Name:" :value (. program name)))
```

---

### 27. `get_program_info`
**Purpose:** Program metadata and verification status

**Parameters:**
```lisp
{:programId "program_address"}
```

**Example:**
```lisp
(define program (get_program_info {:programId "11111111111111111111111111111111"}))
(log :message "Name:" :value (. program name))
(log :message "Verified:" :value (. program verified))
```

---

### 28. `report_error`
**Purpose:** Report client-side errors for telemetry

**Parameters:**
```lisp
{:error "error_message" :context {:user "..." :action "..."}}
```

**Example:**
```lisp
(report_error {
  :error "Transaction failed"
  :context {:action "swap" :dex "jupiter"}
})
```

---

## Monetization Tools

**Note:** These tools require authentication (JWT token)

### 29. `get_balance`
**Purpose:** Get SVMAI token balance for API billing

**Example:**
```lisp
;; Requires: OPENSVM_JWT_TOKEN environment variable
(define balance (get_balance {}))
(log :message "SVMAI balance:" :value (. balance balance))
```

---

### 30. `get_usage_stats`
**Purpose:** API usage statistics

**Example:**
```lisp
(define usage (get_usage_stats {}))
(log :message "Requests today:" :value (. usage requestsToday))
(log :message "Tokens used:" :value (. usage tokensUsed))
```

---

### 31. `manage_api_keys`
**Purpose:** Manage Anthropic API keys

**Parameters:**
```lisp
{:action "list" | "create" | "delete" :keyId "key_id"}
```

**Example:**
```lisp
(define keys (manage_api_keys {:action "list"}))
(log :message "Active API keys:" :value (length keys))
```

---

## Direct RPC Access

The `solana_rpc_call` tool provides access to 90+ standard Solana RPC methods. Here are some common ones:

### Account RPC Methods
- `getAccountInfo` - Get account data
- `getBalance` - Get SOL balance
- `getMultipleAccounts` - Batch fetch accounts
- `getProgramAccounts` - Query accounts by program
- `getTokenAccountsByOwner` - Get SPL token accounts

### Transaction RPC Methods
- `getSignaturesForAddress` - Get transaction history
- `getTransaction` - Get transaction details
- `getConfirmedTransaction` - Get confirmed transaction
- `simulateTransaction` - Simulate transaction
- `sendTransaction` - Send signed transaction

### Block RPC Methods
- `getBlock` - Get block by slot
- `getBlockHeight` - Current block height
- `getBlocks` - Get confirmed blocks
- `getBlockTime` - Get block timestamp
- `getFirstAvailableBlock` - Earliest available block

### Network RPC Methods
- `getSlot` - Current slot
- `getHealth` - RPC health status
- `getVersion` - Solana version
- `getEpochInfo` - Current epoch info
- `getInflationRate` - Current inflation rate
- `getSupply` - Total supply information

### Validator RPC Methods
- `getVoteAccounts` - Get vote accounts
- `getLeaderSchedule` - Leader schedule
- `getClusterNodes` - Cluster node information

**Full list:** https://docs.solana.com/api

---

## Best Practices

### 1. Use Type-Specific Tools When Available

```lisp
;; ✅ GOOD - Use specialized tool
(get_account_portfolio {:address wallet})

;; ❌ LESS IDEAL - Generic RPC call (more setup needed)
(solana_rpc_call {:method "getAccountInfo" :params [wallet]})
```

### 2. Handle Errors Gracefully

```lisp
(try
  (define portfolio (get_account_portfolio {:address wallet}))
  (log :message "Portfolio value:" :value (. portfolio totalValue))
  (catch err
    (log :message "Error fetching portfolio:" :value err)))
```

### 3. Use Batch Operations

```lisp
;; ✅ GOOD - Single batch call
(define txs (batch_transactions {:signatures sig_list}))

;; ❌ SLOW - Multiple individual calls
(for (sig sig_list)
  (get_transaction {:signature sig}))
```

### 4. Leverage AI-Powered Tools

```lisp
;; Use analyze_transaction for insights
(define analysis (analyze_transaction {:signature sig}))
(log :message "Risk assessment:" :value (. analysis riskScore))

;; Use explain_transaction for human-readable output
(define explanation (explain_transaction {:signature sig}))
(log :message (. explanation text))
```

---

## Troubleshooting

### "Undefined tool" Error

**Problem:** Tool name doesn't match
**Solution:** Use exact snake_case names (e.g., `get_account_portfolio` not `getAccountPortfolio`)

### Authentication Errors

**Problem:** JWT required but not provided
**Solution:** Only affects `get_balance`, `get_usage_stats`, `manage_api_keys`
```bash
export OPENSVM_JWT_TOKEN="your_token_here"
```

### Invalid Parameters

**Problem:** Wrong parameter format
**Solution:** Use object syntax with keyword keys:
```lisp
;; ✅ Correct
(get_token_info {:mint "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"})

;; ❌ Wrong
(get_token_info "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v")
```

---

## See Also

- [MCP_INTEGRATION_README.md](./MCP_INTEGRATION_README.md) - Setup guide
- [mcp_basic_rpc.ovsm](./mcp_basic_rpc.ovsm) - Basic RPC examples
- [mcp_wallet_analysis.ovsm](./mcp_wallet_analysis.ovsm) - Wallet analysis example
- [pumpfun_1min_lisp.ovsm](./pumpfun_1min_lisp.ovsm) - Production pagination example
- osvm-mcp README: `/home/larp/.osvm/mcp/osvm-mcp/README.md`
