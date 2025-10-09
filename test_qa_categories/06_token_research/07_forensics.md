# Token Research - Actionable Forensics (Q601-Q700)

**Category:** Token Research
**Focus:** On-chain forensics, exploit analysis, scam detection, fund tracing, wallet profiling, smart contract security
**Questions:** Q601-Q700

---

## Q5601: "How can I detect wash trading activity for the memecoin 'POPCAT' (POPCAT) on Raydium by analyzing transaction patterns and wallet clusters?"


[TIME: ~55s] [COST: ~0.008 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress (Solana RPC)
  - getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, COUNT (Data Processing)
  - DETECT_WASH_TRADING_PATTERNS (Forensics)
  - IDENTIFY_WALLET_CLUSTERS (Forensics)

**Main Branch:**
$token_mint = "POPCAT" // Placeholder for actual mint address
$raydium_pool_address = "..." // Find the specific POPCAT-SOL Raydium pool

// Get recent transaction signatures from the liquidity pool
$signatures = getSignaturesForAddress(address: $raydium_pool_address, limit: 1000)

// Fetch full transaction details in parallel
PARALLEL {
  $transactions = MAP(collection: $signatures, fn: sig => getTransaction(signature: sig.signature))
}
WAIT_ALL

// Filter for swap transactions and extract trader wallets
$swaps = FILTER(collection: $transactions, predicate: tx => IS_SWAP_TRANSACTION(tx))
$traders = MAP(collection: $swaps, fn: tx => tx.transaction.message.accountKeys[0]) // Simplified, real logic is more complex

// Analyze for wash trading patterns
$wash_trading_analysis = DETECT_WASH_TRADING_PATTERNS(swaps: $swaps)
$wallet_clusters = IDENTIFY_WALLET_CLUSTERS(traders: $traders)

**Decision Point:** Evaluate the evidence of wash trading.
  BRANCH A ($wash_trading_analysis.is_highly_likely && COUNT($wallet_clusters.major_clusters) > 0):
    $verdict = "High probability of coordinated wash trading."
    $details = "Detected multiple wallet clusters executing cyclical trades with minimal price change."
  BRANCH B ($wash_trading_analysis.is_possible):
    $verdict = "Moderate evidence of wash trading."
    $details = "Some suspicious back-and-forth trading detected, but no clear wallet clusters."
  BRANCH C (default):
    $verdict = "Low evidence of wash trading."
    $details = "Trading activity appears organic."

**Action:**
RETURN {
  verdict: $verdict,
  details: $details,
  wash_trading_score: $wash_trading_analysis.score,
  identified_clusters: $wallet_clusters.major_clusters,
  sample_size: COUNT($swaps),
  confidence: 90
}

---

## Q5602: "A token just rugged. What is the forensic plan to trace the scammer's funds from the liquidity pull to a potential CEX deposit address?"


## Q5611: "Which wallets have been linked to the most successful meme token rugs, and what on-chain patterns do they share?"

## Q5612: "How can you trace the flow of funds from a memecoin exploit to prediction market bets placed immediately after?"

## Q5613: "What is the fastest way to identify a wallet that is likely to dump a large meme token position based on recent on-chain activity?"

## Q5614: "How can you detect a wallet that is farming multiple prediction markets with Sybil-like behavior?"

## Q5615: "What is the most common laundering path for funds stolen from meme token pools on Solana?"

## Q5616: "How can you identify wallets that consistently frontrun prediction market odds changes using on-chain data?"

## Q5617: "What is the average time between a meme token's launch and its first major scam or exploit, and how can you spot early warning signs?"

## Q5618: "How can you use on-chain data to prove collusion between meme token deployers and prediction market creators?"

## Q5619: "What is the best method to identify wallets that are likely to be 'exit liquidity' in a meme token pump and dump?"

## Q5620: "How can you detect and trace the use of privacy protocols (mixers) by meme token insiders before a major exploit?"

[TIME: ~120s] [COST: ~0.015 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - TRACE_FUNDS (Forensics)
  - IDENTIFY_CEX_DEPOSIT_ADDRESS (Forensics)
  - MAP, FILTER, REVERSE (Data Processing)

**Main Branch:**
$rug_pull_tx_signature = "5..." // Signature of the transaction that removed liquidity

// Start the trace from the rug pull transaction
$rug_tx = getTransaction(signature: $rug_pull_tx_signature)
$scammer_wallet = $rug_tx.transaction.message.accountKeys[0] // The wallet that received the SOL/USDC
$initial_funds = EXTRACT_TRANSFER_DETAILS($rug_tx)

// Trace the funds across multiple hops
$trace_result = TRACE_FUNDS(
  start_address: $scammer_wallet, 
  initial_tx: $rug_pull_tx_signature,
  depth: 5 // Look 5 transactions deep
)

// Analyze the trace for CEX deposit patterns
$cex_deposit = IDENTIFY_CEX_DEPOSIT_ADDRESS(trace: $trace_result.hops)

**Decision Point:** Determine the outcome of the fund trace.
  BRANCH A ($cex_deposit.found):
    $outcome = "Successfully traced funds to a potential CEX deposit address."
    $next_steps = "Report the address and transaction history to the CEX's compliance department."
    $final_address = $cex_deposit.address
  BRANCH B ($trace_result.ended_in_mixer):
    $outcome = "Fund trace stopped at a known mixer protocol."
    $next_steps = "Further tracing is difficult. Analyze mixer inputs/outputs for timing correlations."
    $final_address = $trace_result.last_address
  BRANCH C (default):
    $outcome = "Funds are still sitting in an intermediary wallet."
    $next_steps = "Monitor the wallet for further movement."
    $final_address = $trace_result.last_address

**Action:**
RETURN {
  outcome: $outcome,
  next_steps: $next_steps,
  scammer_source_wallet: $scammer_wallet,
  final_known_address: $final_address,
  trace_path: $trace_result.hops,
  confidence: 85
}

---

## Q5603: "My wallet received a spam NFT. How do I analyze its associated smart contract and transaction history to determine if it's a wallet drainer?"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - getTransaction (Solana RPC)
  - DECOMPILE_PROGRAM (Forensics)
  - ANALYZE_CONTRACT_PERMISSIONS (Security)
  - SIMULATE_TRANSACTION (Security)

**Main Branch:**
$spam_nft_mint_address = "..."

// Get the program associated with the NFT
$account_info = getAccountInfo(pubkey: $spam_nft_mint_address)
$program_id = $account_info.owner

// Analyze the program's code and permissions
$decompiled_code = DECOMPILE_PROGRAM(program_id: $program_id)
$permissions = ANALYZE_CONTRACT_PERMISSIONS(program_id: $program_id)

// Simulate a burn or transfer transaction to check for malicious behavior
$simulation_result = SIMULATE_TRANSACTION(
  instruction: "burn", // Simulate interacting with the NFT
  nft_mint: $spam_nft_mint_address,
  user_wallet: "YOUR_WALLET_ADDRESS"
)

**Decision Point:** Assess the risk level of the spam NFT.
  BRANCH A ($permissions.can_delegate_authority || CONTAINS($decompiled_code, "transfer_from_all")):
    $risk = "EXTREMELY HIGH RISK"
    $reason = "The contract has permissions to transfer other assets from your wallet (set-authority)."
  BRANCH B ($simulation_result.drains_other_tokens):
    $risk = "EXTREMELY HIGH RISK"
    $reason = "Transaction simulation shows it attempts to drain other tokens from your wallet."
  BRANCH C (CONTAINS($decompiled_code, "malicious_url")):
    $risk = "HIGH RISK"
    $reason = "The contract metadata contains a known phishing URL."
  BRANCH D (default):
    $risk = "LOW RISK"
    $reason = "Appears to be a standard spam NFT with no obvious drainer functions. It is still best not to interact with it."

**Action:**
RETURN {
  risk_level: $risk,
  reason: $reason,
  program_id: $program_id,
  dangerous_permissions_found: $permissions,
  simulation_outcome: $simulation_result.summary,
  confidence: 95
}

---

## Q5604: "An MEV bot front-ran my swap on Jupiter. Can I analyze the bot's bundle and historical behavior to understand its strategy?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.01 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - JITO_GET_BUNDLES (Jito-specific Tool)
  - getTransaction (Solana RPC)
  - GET_WALLET_INTERACTIONS (Forensics)
  - IDENTIFY_MEV_STRATEGY (MEV Analysis)

**Main Branch:**
$my_tx_signature = "..."
$block_number = getTransaction(signature: $my_tx_signature).slot

// Find the Jito bundle that included my transaction
$bundle = JITO_GET_BUNDLES(block: $block_number, user_tx: $my_tx_signature)
$mev_bot_wallet = $bundle.transactions[0].signer // The bot is usually first in the bundle

// Analyze the bundle for MEV strategy
$strategy_analysis = IDENTIFY_MEV_STRATEGY(bundle: $bundle)

// Analyze the bot's historical activity
$bot_history = GET_WALLET_INTERACTIONS(wallet_address: $mev_bot_wallet, limit: 100)
$historical_pnl = CALCULATE_PNL(transactions: $bot_history.transactions)

**Decision Point:** Classify the MEV bot's strategy.
  BRANCH A ($strategy_analysis.type == "sandwich"):
    $strategy = "Sandwich Attack"
    $explanation = "The bot placed a buy order before your transaction and a sell order immediately after, profiting from the price slippage you caused."
  BRANCH B ($strategy_analysis.type == "jito_backrun"):
    $strategy = "Jito Backrun"
    $explanation = "The bot saw a profitable opportunity in your swap (e.g., an arbitrage) and executed its own transaction immediately after yours in the same bundle."
  BRANCH C (default):
    $strategy = "Generic Arbitrage"
    $explanation = "The bot likely identified a simple arbitrage opportunity across different pools."

**Action:**
RETURN {
  strategy_type: $strategy,
  explanation: $explanation,
  mev_bot_address: $mev_bot_wallet,
  profit_from_my_tx: $strategy_analysis.profit_sol,
  bot_historical_pnl: $historical_pnl.total_pnl_sol,
  bot_win_rate: $historical_pnl.win_rate,
  confidence: 88
}

---

## Q5605: "A governance proposal for 'Marinade' seems suspicious. How can I forensically analyze the proposer and the whale wallets voting for it?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts (Solana RPC)
  - GET_WALLET_INTERACTIONS (Forensics)
  - GET_WALLET_CREATION_DATE (Forensics)
  - ANALYZE_VOTING_CORRELATION (Governance)

**Main Branch:**
$proposal_id = "..."
$governance_program_id = "GovER5Lthms3bLBqWub97yVrMmEogzX7xNjdXpPPCVNN" // Marinade Governance

// Get proposal and voter data
$proposal_account = getAccountInfo(pubkey: $proposal_id)
$proposer_wallet = $proposal_account.data.proposer
$voter_accounts = getProgramAccounts(programId: $governance_program_id, filters: [{memcmp: {offset: 8, bytes: $proposal_id}}])
$voters = MAP(collection: $voter_accounts, fn: v => v.account.owner)

// Analyze the proposer
$proposer_history = GET_WALLET_INTERACTIONS(wallet_address: $proposer_wallet)
$proposer_creation = GET_WALLET_CREATION_DATE(wallet_address: $proposer_wallet)

// Analyze the voters
$voting_correlation = ANALYZE_VOTING_CORRELATION(voters: $voters)

**Decision Point:** Assess the legitimacy of the governance proposal.
  BRANCH A ($proposer_creation.is_recent && COUNT($proposer_history.transactions) < 10):
    $risk = "HIGH"
    $reason = "Proposer is a brand new wallet with no history, a major red flag for malicious proposals."
  BRANCH B ($voting_correlation.has_strong_cluster && $voting_correlation.cluster_funded_by_one_source):
    $risk = "HIGH"
    $reason = "A large cluster of voting wallets were all funded from the same source, suggesting a Sybil attack."
  BRANCH C (default):
    $risk = "LOW"
    $reason = "Proposer and voters appear to be established, independent participants in the ecosystem."

**Action:**
RETURN {
  risk_level: $risk,
  reason: $reason,
  proposer: {
    address: $proposer_wallet,
    creation_date: $proposer_creation.date,
    transaction_count: COUNT($proposer_history.transactions)
  },
  voter_analysis: {
    total_voters: COUNT($voters),
    sybil_cluster_detected: $voting_correlation.has_strong_cluster,
    cluster_size: $voting_correlation.cluster_size
  },
  confidence: 92
}

---

## Q5606: "A flash loan exploit just occurred on the 'Solend' protocol. What is the step-by-step process to trace the stolen funds through a mixer?"

**Expected Plan:**

[TIME: ~150s] [COST: ~0.02 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - TRACE_FUNDS (Forensics)
  - ANALYZE_MIXER_ACTIVITY (Forensics)

**Main Branch:**
$exploit_tx_signature = "..."

// 1. Identify the exploiter's wallet from the initial transaction
$exploit_tx = getTransaction(signature: $exploit_tx_signature)
$exploiter_wallet = $exploit_tx.transaction.message.accountKeys[0]

// 2. Trace funds from the exploiter's wallet to the mixer
$trace_to_mixer = TRACE_FUNDS(start_address: $exploiter_wallet, depth: 10, stop_at_program: "TORNADO_CASH_ROUTER")

GUARD $trace_to_mixer.stopped_at_program ELSE
  RETURN ERROR(message: "Funds were not traced to a known mixer.")

$mixer_txs = $trace_to_mixer.hops

// 3. Analyze the mixer deposits
$mixer_deposits = FILTER(collection: $mixer_txs, predicate: tx => tx.program_id == "TORNADO_CASH_ROUTER" && tx.instruction == "deposit")
$deposit_amounts = MAP(collection: $mixer_deposits, fn: tx => tx.amount)
$deposit_times = MAP(collection: $mixer_deposits, fn: tx => tx.blockTime)

// 4. Analyze withdrawal patterns from the mixer for correlation
$mixer_analysis = ANALYZE_MIXER_ACTIVITY(
  mixer_program: "TORNADO_CASH_ROUTER",
  deposit_amounts: $deposit_amounts,
  deposit_times: $deposit_times
)

**Decision Point:** Determine if a likely withdrawal address can be identified.
  BRANCH A ($mixer_analysis.strong_correlation_found):
    $outcome = "High probability match found."
    $details = "A set of withdrawals of the same amounts occurred shortly after deposit from a fresh wallet."
    $likely_exit_address = $mixer_analysis.correlated_address
  BRANCH B ($mixer_analysis.weak_correlation_found):
    $outcome = "Weak probability match found."
    $details = "Withdrawal amounts match, but timing is spread out, making a definitive link difficult."
    $likely_exit_address = $mixer_analysis.correlated_address
  BRANCH C (default):
    $outcome = "No correlated withdrawal address found."
    $details = "The funds have likely not been withdrawn yet, or were withdrawn in a way that evades simple analysis."
    $likely_exit_address = "N/A"

**Action:**
RETURN {
  outcome: $outcome,
  details: $details,
  exploiter_source_wallet: $exploiter_wallet,
  mixer_protocol: "Tornado Cash",
  likely_exit_address: $likely_exit_address,
  confidence: 80
}

---

## Q5607: "How can I analyze the 'Claynosaurz' NFT collection on Magic Eden to identify a ring of wallets wash trading to inflate the floor price?"

**Expected Plan:**

[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - MAGIC_EDEN_GET_SALES (Marketplace API)
  - IDENTIFY_WALLET_CLUSTERS (Forensics)
  - ANALYZE_NFT_TRADE_CYCLES (Forensics)

**Main Branch:**
$collection_symbol = "claynosaurz"

// Get the last 1000 sales for the collection
$sales = MAGIC_EDEN_GET_SALES(collection: $collection_symbol, limit: 1000)

// Extract all buyers and sellers
$buyers = MAP(collection: $sales, fn: s => s.buyer)
$sellers = MAP(collection: $sales, fn: s => s.seller)
$all_traders = APPEND(array: $buyers, items: $sellers)

// Identify clusters of wallets that trade frequently with each other
$wallet_clusters = IDENTIFY_WALLET_CLUSTERS(traders: $all_traders, min_cluster_size: 3)

// Analyze for specific NFTs being traded back and forth within clusters
$trade_cycles = ANALYZE_NFT_TRADE_CYCLES(sales: $sales, clusters: $wallet_clusters)

**Decision Point:** Assess the evidence for a wash trading ring.
  BRANCH A (COUNT($trade_cycles.significant_cycles) > 0):
    $verdict = "Strong evidence of a wash trading ring."
    $reason = "Found multiple NFTs being cyclically traded between a small cluster of wallets at increasing prices."
    $implicated_wallets = $trade_cycles.implicated_wallets
  BRANCH B (COUNT($wallet_clusters.major_clusters) > 0 && $trade_cycles.suspicious_activity_score > 0.7):
    $verdict = "Moderate evidence of wash trading."
    $reason = "A tight cluster of wallets dominates trading activity, but direct cyclical trades are infrequent. They may be using more complex patterns."
    $implicated_wallets = $wallet_clusters.major_clusters[0].wallets
  BRANCH C (default):
    $verdict = "Low evidence of wash trading."
    $reason = "Trading activity appears diverse and spread across many independent wallets."
    $implicated_wallets = []

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  implicated_wallets: $implicated_wallets,
  wash_trade_cycle_examples: $trade_cycles.significant_cycles,
  confidence: 85
}

---

## Q5608: "An NFT claims to be from a famous artist but was 'sleep minted.' How can I forensically verify the true origin and creator by analyzing its mint transaction?"

**Expected Plan:**

[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library:
  - getTransaction (Solana RPC)
  - getAccountInfo (Solana RPC)

**Main Branch:**
$nft_mint_address = "..."

// Get the mint transaction for the NFT
$mint_info = getAccountInfo(pubkey: $nft_mint_address)
$mint_tx_signature = FIND_MINT_TRANSACTION(mint_address: $nft_mint_address) // Utility to find the first tx for the mint
$mint_tx = getTransaction(signature: $mint_tx_signature)

// Identify key roles in the mint transaction
$fee_payer = $mint_tx.transaction.message.accountKeys[0]
$creator_wallet = $mint_info.data.creators[0].address
$update_authority = $mint_info.data.updateAuthority

**Decision Point:** Determine the authenticity of the NFT's origin.
  BRANCH A ($fee_payer != $creator_wallet):
    $verdict = "Likely NOT authentic (Sleep Minted)."
    $reason = "The transaction fee for the mint was paid by a different wallet than the claimed creator. This is the classic sign of sleep minting, where someone else creates the NFT and assigns creator status."
    $true_minter = $fee_payer
  BRANCH B ($fee_payer == $creator_wallet):
    $verdict = "Likely Authentic."
    $reason = "The claimed creator's wallet also paid the transaction fee to mint the NFT, indicating they initiated the creation."
    $true_minter = $creator_wallet
  BRANCH C (default):
    $verdict = "Indeterminate."
    $reason = "Could not definitively determine the relationship between fee payer and creator."
    $true_minter = "Unknown"

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  claimed_creator: $creator_wallet,
  actual_minter_and_fee_payer: $true_minter,
  update_authority: $update_authority,
  confidence: 98
}

---

## Q5609: "I have a known phishing scammer's wallet address. How can I build a complete profile of their operations, including other wallets they control and total funds stolen?"

**Expected Plan:**

[TIME: ~180s] [COST: ~0.03 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - GET_WALLET_INTERACTIONS (Forensics)
  - IDENTIFY_WALLET_CLUSTERS (Forensics)
  - TRACE_FUNDS (Forensics)
  - CALCULATE_PNL (Forensics)

**Main Branch:**
$scammer_address = "..."

// 1. Get all transactions for the primary address
$primary_interactions = GET_WALLET_INTERACTIONS(wallet_address: $scammer_address, limit: 1000)

// 2. Identify connected wallets (potential cluster)
$all_counterparties = MAP(collection: $primary_interactions.transactions, fn: tx => tx.counterparty)
$wallet_cluster = IDENTIFY_WALLET_CLUSTERS(traders: $all_counterparties, source_wallet: $scammer_address)

// 3. For each wallet in the cluster, trace outgoing funds to find profit-taking addresses
$stolen_funds_destinations = []
FOR $wallet IN $wallet_cluster.wallets:
  $trace = TRACE_FUNDS(start_address: $wallet, depth: 5, stop_at_cex: true)
  $stolen_funds_destinations = APPEND(array: $stolen_funds_destinations, item: $trace.last_address)

// 4. Calculate total funds received by the cluster
$total_inflow = 0
FOR $wallet IN $wallet_cluster.wallets:
  $pnl = CALCULATE_PNL(wallet_address: $wallet)
  $total_inflow += $pnl.total_deposits

**Decision Point:** Summarize the scammer's operational profile.
  BRANCH A (COUNT($wallet_cluster.wallets) > 1):
    $profile_summary = "This is a sophisticated operation using a cluster of wallets to obfuscate fund flows."
    $wallet_count = COUNT($wallet_cluster.wallets)
  BRANCH B (default):
    $profile_summary = "This appears to be a simple operation run from a single primary address."
    $wallet_count = 1

**Action:**
RETURN {
  profile_summary: $profile_summary,
  primary_address: $scammer_address,
  associated_wallets: $wallet_cluster.wallets,
  wallet_count: $wallet_count,
  estimated_total_stolen: $total_inflow,
  known_cashout_addresses: UNIQUE($stolen_funds_destinations),
  confidence: 82
}

---

## Q5610: "How can I analyze a new token's on-chain program to determine if it's a honeypot (buyable but not sellable) before I invest?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - DECOMPILE_PROGRAM (Forensics)
  - SIMULATE_TRANSACTION (Security)

**Main Branch:**
$token_mint_address = "..."

// 1. Get the program ID for the token's mint
$token_info = getAccountInfo(pubkey: $token_mint_address)
$program_id = $token_info.owner

// 2. Decompile the program to analyze its source code
$source_code = DECOMPILE_PROGRAM(program_id: $program_id)

// 3. Simulate a full buy-then-sell transaction cycle
$buy_simulation = SIMULATE_TRANSACTION(
  instruction: "buy",
  token_mint: $token_mint_address,
  amount: 100
)

GUARD $buy_simulation.success ELSE
  RETURN ERROR(message: "Simulation failed on the buy step.")

$sell_simulation = SIMULATE_TRANSACTION(
  instruction: "sell",
  token_mint: $token_mint_address,
  amount: 100,
  wallet_state: $buy_simulation.resulting_state
)

**Decision Point:** Determine if the contract is a honeypot.
  BRANCH A (!$sell_simulation.success && CONTAINS($sell_simulation.error, "Constraint/permission denied")):
    $verdict = "CONFIRMED HONEYPOT"
    $reason = "The sell transaction failed with a permission error, which is a classic sign that the contract code intentionally blocks sales from regular users."
  BRANCH B (CONTAINS($source_code, "if signer == owner")):
    $verdict = "CONFIRMED HONEYPOT"
    $reason = "Decompiled code shows a check that only allows the contract owner to execute the sell/transfer function."
  BRANCH C (!$sell_simulation.success):
    $verdict = "LIKELY HONEYPOT"
    $reason = "The sell transaction failed for an unknown reason, which is highly suspicious."
  BRANCH D (default):
    $verdict = "NOT a honeypot."
    $reason = "The simulated buy-and-sell cycle completed successfully."

**Action:**
RETURN {
  verdict: $verdict,
  reason: $reason,
  is_honeypot: $verdict != "NOT a honeypot",
  simulation_log: $sell_simulation.log,
  confidence: 96
}

---

## Q5621: "Build a complete 'money laundering network map' starting from a known scam wallet. Identify all layering wallets, integration points, and final legitimate-seeming destinations."

**Expected Plan:**

[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - BUILD_LAUNDERING_NETWORK (Advanced Forensics)
  - CLASSIFY_WALLET_LAYER (Pattern Analysis)

**Main Branch:**
$scam_source = "..."
$stolen_amount = 5000 // SOL

// Build complete network map
$network = BUILD_LAUNDERING_NETWORK(
  source: $scam_source,
  max_depth: 15,
  min_amount_threshold: 0.1
)

// Classify wallets by laundering layer
$layered_wallets = MAP(collection: $network.all_wallets, fn: wallet => {
  $depth = $network.wallet_depth[wallet]
  $activity = GET_WALLET_ACTIVITY(wallet: wallet)
  
  // Determine layer type
  $layer_type = "UNKNOWN"
  IF $depth <= 2:
    $layer_type = "PLACEMENT" // Initial dispersion
  ELSE IF $depth > 2 && $depth <= 8:
    IF COUNT(FILTER($activity, a => a.is_dex_swap)) > 10:
      $layer_type = "LAYERING_DEX" // Complex swaps to obscure
    ELSE:
      $layer_type = "LAYERING_SPLIT" // Just splitting funds
  ELSE:
    IF COUNT(FILTER($activity, a => a.is_cex_deposit || a.is_nft_purchase || a.is_defi_deposit)) > 0:
      $layer_type = "INTEGRATION" // Mixing with legitimate activity
  
  RETURN {
    wallet: wallet,
    depth: $depth,
    layer: $layer_type,
    total_received: $network.wallet_flows[wallet].total_in,
    total_sent: $network.wallet_flows[wallet].total_out,
    has_legitimate_activity: $activity.has_real_user_behavior
  }
})

// Find final destinations
$integration_wallets = FILTER($layered_wallets, w => w.layer == "INTEGRATION")

**Action:**
RETURN {
  investigation: "Money Laundering Network Map",
  scam_source: $scam_source,
  stolen_amount_sol: $stolen_amount,
  total_wallets_in_network: COUNT($network.all_wallets),
  placement_layer: COUNT(FILTER($layered_wallets, w => w.layer == "PLACEMENT")),
  layering_dex: COUNT(FILTER($layered_wallets, w => w.layer == "LAYERING_DEX")),
  layering_split: COUNT(FILTER($layered_wallets, w => w.layer == "LAYERING_SPLIT")),
  integration_layer: COUNT($integration_wallets),
  network_depth: MAX(MAP($layered_wallets, w => w.depth)),
  final_destinations: MAP($integration_wallets, w => {
    RETURN {wallet: w.wallet, appears_legitimate: w.has_legitimate_activity}
  }),
  confidence: 84
}

---

## Q5622: "Detect a 'romance scam crypto network' where victims send crypto to fake online partners. Map the receiver cluster, identify the operation's scale, and find the cashout wallets."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library:
  - DETECT_ROMANCE_SCAM_PATTERN (Forensics)
  - BUILD_SCAM_OPERATION_CLUSTER (Analysis)

**Main Branch:**
$suspected_receiver_wallets = [...] // Known romance scam wallets

// Build complete scammer operation cluster
$scammer_cluster = BUILD_SCAM_OPERATION_CLUSTER(
  seed_wallets: $suspected_receiver_wallets,
  check_funding: true
)

// Identify victim wallets (senders to cluster)
$all_victims = []
FOR $scammer_wallet IN $scammer_cluster.all_wallets:
  $incoming = getSignaturesForAddress(address: $scammer_wallet, direction: "in")
  $victims = UNIQUE(MAP($incoming, tx => tx.from))
  $all_victims = APPEND($all_victims, $victims)

$all_victims = UNIQUE(FLATTEN($all_victims))

// Analyze victim transaction patterns (indicative of romance scam)
$victim_analysis = MAP(collection: $all_victims, fn: victim => {
  $sends_to_scammers = FILTER(
    getSignaturesForAddress(address: victim, direction: "out"),
    tx => CONTAINS($scammer_cluster.all_wallets, tx.to)
  )
  
  // Romance scam indicators: multiple small sends over time
  $pattern = DETECT_ROMANCE_SCAM_PATTERN(transactions: $sends_to_scammers)
  
  RETURN {
    victim: victim,
    total_sent: SUM(MAP($sends_to_scammers, tx => tx.amount)),
    transaction_count: COUNT($sends_to_scammers),
    send_frequency_days: $pattern.avg_days_between_sends,
    is_likely_romance_scam: $pattern.matches_romance_pattern,
    first_send: MIN(MAP($sends_to_scammers, tx => tx.timestamp)),
    last_send: MAX(MAP($sends_to_scammers, tx => tx.timestamp))
  }
})

$confirmed_victims = FILTER($victim_analysis, v => v.is_likely_romance_scam)

// Find cashout points
$cashout_wallets = FIND_CEX_CASHOUT_WALLETS(cluster: $scammer_cluster)

**Action:**
RETURN {
  investigation: "Romance Scam Crypto Network",
  scammer_cluster_size: COUNT($scammer_cluster.all_wallets),
  total_victims_found: COUNT($confirmed_victims),
  total_stolen_sol: SUM(MAP($confirmed_victims, v => v.total_sent)),
  avg_victim_loss_sol: MEAN(MAP($confirmed_victims, v => v.total_sent)),
  longest_victim_relationship_days: MAX(MAP($confirmed_victims, v => (v.last_send - v.first_send) / 86400)),
  cashout_wallets: $cashout_wallets,
  victim_details: $confirmed_victims,
  confidence: 80
}

---

## Q5623: "Investigate a 'crypto Ponzi scheme' by analyzing the referral tree, calculating each level's ROI, and identifying when the scheme became mathematically unsustainable."

**Expected Plan:**

[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - BUILD_REFERRAL_TREE (Analysis)
  - CALCULATE_PONZI_SUSTAINABILITY (Financial Analysis)

**Main Branch:**
$ponzi_contract = "..."
$scheme_start_date = 1712448000

// Build complete referral tree
$referral_tree = BUILD_REFERRAL_TREE(
  contract: $ponzi_contract,
  root: $scheme_start_date
)

// Analyze each level of the pyramid
$level_analysis = MAP(collection: $referral_tree.levels, fn: level => {
  $participants = level.participants
  
  // Calculate total invested at this level
  $total_invested = SUM(MAP($participants, p => p.amount_invested))
  
  // Calculate total withdrawn at this level
  $total_withdrawn = SUM(MAP($participants, p => p.amount_withdrawn))
  
  // Calculate ROI
  $level_roi = (($total_withdrawn - $total_invested) / $total_invested) * 100
  
  RETURN {
    level: level.depth,
    participant_count: COUNT($participants),
    total_invested_sol: $total_invested,
    total_withdrawn_sol: $total_withdrawn,
    roi_percentage: $level_roi,
    join_date_range: {
      earliest: MIN(MAP($participants, p => p.join_date)),
      latest: MAX(MAP($participants, p => p.join_date))
    }
  }
})

// Calculate sustainability point
$sustainability = CALCULATE_PONZI_SUSTAINABILITY(
  referral_tree: $referral_tree,
  roi_promises: 20 // Promised 20% monthly returns
)

// Identify when it became unsustainable
$unsustainable_date = $sustainability.collapse_date

// Find participants who joined after unsustainable point (guaranteed losers)
$guaranteed_losers = FILTER($referral_tree.all_participants, p => {
  p.join_date > $unsustainable_date && p.net_loss > 0
})

**Action:**
RETURN {
  investigation: "Crypto Ponzi Scheme Analysis",
  ponzi_contract: $ponzi_contract,
  total_participants: COUNT($referral_tree.all_participants),
  pyramid_levels: COUNT($referral_tree.levels),
  level_breakdown: $level_analysis,
  scheme_became_unsustainable: $unsustainable_date,
  days_until_collapse: ($unsustainable_date - $scheme_start_date) / 86400,
  participants_after_unsustainable: COUNT($guaranteed_losers),
  guaranteed_losers_total_loss: SUM(MAP($guaranteed_losers, l => l.net_loss)),
  top_level_winners: FILTER($level_analysis, l => l.level <= 3 && l.roi_percentage > 0),
  confidence: 87
}

---

## Q5624: "Trace a 'SIM swap attack' where an attacker gained control of a victim's phone number and drained their Solana wallet. Map the attack timeline and fund destinations."

**Expected Plan:**

[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - ANALYZE_WALLET_TAKEOVER (Forensics)
  - BUILD_ATTACK_TIMELINE (Timeline Analysis)

**Main Branch:**
$victim_wallet = "..."
$attack_timestamp = 1728432000

// Get all transactions around attack time
$all_txs = getSignaturesForAddress(
  address: $victim_wallet,
  limit: 1000
)

// Identify takeover point
$takeover_analysis = ANALYZE_WALLET_TAKEOVER(
  wallet: $victim_wallet,
  suspected_time: $attack_timestamp
)

// Build attack timeline
$timeline = BUILD_ATTACK_TIMELINE(
  wallet: $victim_wallet,
  takeover_time: $takeover_analysis.actual_takeover_time
)

// Separate victim's transactions vs attacker's
$victim_txs = FILTER($all_txs, tx => tx.timestamp < $takeover_analysis.actual_takeover_time)
$attacker_txs = FILTER($all_txs, tx => tx.timestamp >= $takeover_analysis.actual_takeover_time)

// Analyze attacker's draining strategy
$drain_analysis = {
  immediate_token_transfers: FILTER($attacker_txs, tx => tx.type == "token_transfer" && tx.timestamp < ($takeover_analysis.actual_takeover_time + 600)), // First 10 min
  sol_transfers: FILTER($attacker_txs, tx => tx.type == "sol_transfer"),
  nft_transfers: FILTER($attacker_txs, tx => tx.type == "nft_transfer"),
  total_drained_usd: SUM(MAP($attacker_txs, tx => tx.value_usd))
}

// Trace where funds went
$fund_destinations = TRACE_STOLEN_ASSETS(
  source: $victim_wallet,
  start_time: $takeover_analysis.actual_takeover_time,
  depth: 5
)

**Action:**
RETURN {
  investigation: "SIM Swap Attack Forensics",
  victim_wallet: $victim_wallet,
  attack_timeline: $timeline,
  takeover_timestamp: $takeover_analysis.actual_takeover_time,
  time_to_full_drain: ($drain_analysis.immediate_token_transfers[LAST].timestamp - $takeover_analysis.actual_takeover_time) / 60, // minutes
  total_drained_usd: $drain_analysis.total_drained_usd,
  tokens_drained: COUNT($drain_analysis.immediate_token_transfers),
  nfts_stolen: COUNT($drain_analysis.nft_transfers),
  attacker_destinations: $fund_destinations.final_addresses,
  attacker_behavior: {
    acted_immediately: $drain_analysis.immediate_token_transfers[0].timestamp < ($takeover_analysis.actual_takeover_time + 300),
    drained_systematically: COUNT($attacker_txs) > 10
  },
  confidence: 85
}

---

## Q5625: "Map an 'airdrop hunter Sybil farm' with 500+ wallets claiming airdrops. Identify the common funding source, consolidation points, and total airdrop value harvested."

**Expected Plan:**

[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - DETECT_AIRDROP_FARMING (Forensics)
  - BUILD_SYBIL_FARM_MAP (Analysis)

**Main Branch:**
$airdrop_contract = "..." // The airdrop program
$airdrop_token = "..."

// Get all airdrop claimers
$all_claimers = GET_AIRDROP_CLAIMERS(
  airdrop_program: $airdrop_contract,
  token: $airdrop_token
)

// Detect Sybil patterns
$sybil_detection = DETECT_AIRDROP_FARMING(
  claimers: $all_claimers,
  min_cluster_size: 10
)

$suspected_farms = $sybil_detection.detected_clusters

// For each farm, build complete map
$farm_analysis = MAP(collection: $suspected_farms, fn: farm => {
  $farm_wallets = farm.wallets
  
  // Find common funding source
  $funding_analysis = BUILD_FUNDING_TREE(wallets: $farm_wallets)
  
  // Find where airdrop tokens went
  $consolidation_points = []
  FOR $wallet IN $farm_wallets:
    $airdrop_token_transfers = FILTER(
      getSignaturesForAddress(address: $wallet, direction: "out"),
      tx => tx.token == $airdrop_token
    )
    
    IF COUNT($airdrop_token_transfers) > 0:
      $consolidation_points = APPEND($consolidation_points, MAP($airdrop_token_transfers, tx => tx.to))
  
  $unique_consolidations = GROUP_BY(
    collection: FLATTEN($consolidation_points),
    key: "address"
  )
  
  // Calculate total value harvested
  $total_claimed = SUM(MAP($farm_wallets, w => {
    $claim = FIND($all_claimers, c => c.wallet == w)
    RETURN $claim ? $claim.amount : 0
  }))
  
  RETURN {
    farm_id: farm.id,
    wallet_count: COUNT($farm_wallets),
    common_funding_root: $funding_analysis.root_wallet,
    consolidation_wallets: MAP(FILTER($unique_consolidations, g => COUNT(g.values) >= 5), g => g.key),
    total_airdrop_tokens_harvested: $total_claimed,
    avg_tokens_per_wallet: $total_claimed / COUNT($farm_wallets)
  }
})

**Action:**
RETURN {
  investigation: "Airdrop Hunter Sybil Farm",
  airdrop_program: $airdrop_contract,
  airdrop_token: $airdrop_token,
  total_claimers: COUNT($all_claimers),
  suspected_sybil_farms: COUNT($suspected_farms),
  total_sybil_wallets: SUM(MAP($farm_analysis, f => f.wallet_count)),
  largest_farm_size: MAX(MAP($farm_analysis, f => f.wallet_count)),
  total_tokens_harvested_by_farms: SUM(MAP($farm_analysis, f => f.total_airdrop_tokens_harvested)),
  farm_details: $farm_analysis,
  confidence: 90
}

---

## Q5626: "Investigate a 'wash trading ring' on an NFT marketplace. Identify all participants, calculate artificial volume, and determine the real price vs manipulated floor."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - DETECT_WASH_TRADING (NFT Analysis)
  - BUILD_WASH_TRADING_RING (Forensics)

**Main Branch:**
$nft_collection = "..."
$marketplace = "..." // Magic Eden, Tensor, etc.

// Get all sales for the collection
$all_sales = GET_NFT_SALES(
  collection: $nft_collection,
  marketplace: $marketplace,
  time_period: "30d"
)

// Detect wash trading patterns
$wash_trading_detection = DETECT_WASH_TRADING(
  sales: $all_sales,
  circular_path_depth: 5
)

// Build the wash trading ring
$wash_ring = BUILD_WASH_TRADING_RING(
  wash_trades: $wash_trading_detection.detected_wash_trades
)

// Separate wash trades from real trades
$wash_sales = FILTER($all_sales, sale => {
  CONTAINS($wash_trading_detection.wash_trade_ids, sale.id)
})

$real_sales = FILTER($all_sales, sale => {
  !CONTAINS($wash_trading_detection.wash_trade_ids, sale.id)
})

// Calculate artificial vs real metrics
$artificial_volume = SUM(MAP($wash_sales, s => s.price))
$real_volume = SUM(MAP($real_sales, s => s.price))

$manipulated_floor = MIN(MAP($wash_sales, s => s.price))
$real_floor = MIN(MAP($real_sales, s => s.price))

**Action:**
RETURN {
  investigation: "NFT Wash Trading Ring",
  collection: $nft_collection,
  marketplace: $marketplace,
  wash_ring_participants: COUNT($wash_ring.participants),
  total_sales: COUNT($all_sales),
  wash_trades: COUNT($wash_sales),
  real_trades: COUNT($real_sales),
  artificial_volume_sol: $artificial_volume,
  real_volume_sol: $real_volume,
  wash_trading_percentage: (COUNT($wash_sales) / COUNT($all_sales)) * 100,
  manipulated_floor_sol: $manipulated_floor,
  real_floor_sol: $real_floor,
  floor_inflation_percentage: (($manipulated_floor - $real_floor) / $real_floor) * 100,
  ring_participants: $wash_ring.participants,
  confidence: 86
}

---

## Q5627: "Detect an 'employment scam' where victims pay upfront fees for fake crypto jobs. Map the scammer's wallets, victim count, and find connections to other scam operations."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - DETECT_EMPLOYMENT_SCAM_PATTERN (Forensics)
  - BUILD_SCAM_NETWORK (Analysis)

**Main Branch:**
$scammer_wallets = [...] // Known scammer wallets

// Identify victims (people who sent money to scammers)
$all_victims = []
FOR $scammer IN $scammer_wallets:
  $incoming = getSignaturesForAddress(address: $scammer, direction: "in")
  
  // Filter for employment scam pattern (small regular amounts)
  $employment_pattern_txs = FILTER($incoming, tx => {
    tx.amount >= 0.1 && tx.amount <= 2 && !isTokenTransfer(tx)
  })
  
  $victims = UNIQUE(MAP($employment_pattern_txs, tx => tx.from))
  $all_victims = APPEND($all_victims, $victims)

$all_victims = UNIQUE(FLATTEN($all_victims))

// Build scammer operation cluster
$scammer_cluster = BUILD_SCAM_NETWORK(
  seed_wallets: $scammer_wallets,
  check_funding: true
)

// Check for connections to other scam operations
$connections_to_other_scams = FIND_SCAM_OPERATION_CONNECTIONS(
  cluster: $scammer_cluster,
  scam_database: "global"
)

// Calculate total stolen
$total_stolen = 0
FOR $scammer IN $scammer_cluster.all_wallets:
  $incoming = getSignaturesForAddress(address: $scammer, direction: "in")
  $total_stolen += SUM(MAP($incoming, tx => tx.amount))

**Action:**
RETURN {
  investigation: "Employment Scam Operation",
  scammer_cluster_size: COUNT($scammer_cluster.all_wallets),
  total_victims: COUNT($all_victims),
  total_stolen_sol: $total_stolen,
  avg_victim_loss_sol: $total_stolen / COUNT($all_victims),
  connected_to_other_scams: COUNT($connections_to_other_scams) > 0,
  related_scam_operations: $connections_to_other_scams,
  victim_wallets: $all_victims,
  scammer_wallets: $scammer_cluster.all_wallets,
  confidence: 82
}

---

## Q5628: "Map a 'crypto impersonation scam' where scammers pretend to be support staff. Track all victim sends to fake support wallets and trace the cashout chain."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - DETECT_IMPERSONATION_SCAM (Forensics)
  - TRACE_SCAM_CASHOUT (Analysis)

**Main Branch:**
$fake_support_wallets = [...] // Known impersonation scam wallets

// Build scammer cluster
$scammer_cluster = BUILD_WALLET_CLUSTER(
  seed_wallets: $fake_support_wallets,
  check_funding: true
)

// Identify all victims
$all_victims = []
FOR $fake_support_wallet IN $scammer_cluster.all_wallets:
  $incoming = getSignaturesForAddress(address: $fake_support_wallet, direction: "in")
  
  // Impersonation scams typically: single large send from victim
  $victim_sends = FILTER($incoming, tx => tx.amount > 1)
  
  $victims = MAP($victim_sends, tx => {
    RETURN {
      victim: tx.from,
      amount_sent: tx.amount,
      scammer_wallet: $fake_support_wallet,
      scam_timestamp: tx.timestamp
    }
  })
  
  $all_victims = APPEND($all_victims, $victims)

$all_victims = FLATTEN($all_victims)

// Trace cashout chain
$cashout_trace = TRACE_SCAM_CASHOUT(
  scammer_wallets: $scammer_cluster.all_wallets,
  depth: 8
)

// Find final CEX deposits
$cex_cashouts = FILTER($cashout_trace.final_destinations, dest => dest.is_cex)

**Action:**
RETURN {
  investigation: "Crypto Impersonation Scam",
  fake_support_wallets: COUNT($scammer_cluster.all_wallets),
  total_victims: COUNT($all_victims),
  total_stolen_sol: SUM(MAP($all_victims, v => v.amount_sent)),
  largest_single_victim_loss: MAX(MAP($all_victims, v => v.amount_sent)),
  avg_victim_loss_sol: MEAN(MAP($all_victims, v => v.amount_sent)),
  cashout_chain_depth: $cashout_trace.max_depth,
  final_cex_cashouts: $cex_cashouts,
  total_cashed_out_sol: SUM(MAP($cex_cashouts, c => c.amount)),
  victim_details: $all_victims,
  confidence: 84
}

---

## Q5629: "Investigate a 'fake ICO/presale' where funds were collected but no token was ever delivered. Identify all investors, calculate total scammed, and trace fund distribution."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - ANALYZE_PRESALE_SCAM (Forensics)
  - TRACE_PRESALE_FUNDS (Analysis)

**Main Branch:**
$presale_wallet = "..."
$promised_token = "..." // Token that was supposed to be delivered (may not exist)
$presale_start = 1720396800
$presale_end = 1722988800

// Get all deposits to presale wallet
$all_deposits = getSignaturesForAddress(
  address: $presale_wallet,
  direction: "in"
)

$presale_deposits = FILTER($all_deposits, tx => {
  tx.timestamp >= $presale_start && tx.timestamp <= $presale_end
})

// Identify investors
$investors = MAP(collection: $presale_deposits, fn: deposit => {
  RETURN {
    investor: deposit.from,
    amount_invested: deposit.amount,
    investment_time: deposit.timestamp
  }
})

// Check if token was ever created/delivered
$token_exists = getAccountInfo($promised_token) != null
$token_distributed = false

IF $token_exists:
  // Check if any investors received the token
  $distribution_txs = []
  FOR $investor IN $investors:
    $received = FILTER(
      getSignaturesForAddress(address: $investor.investor, direction: "in"),
      tx => tx.token == $promised_token
    )
    IF COUNT($received) > 0:
      $distribution_txs = APPEND($distribution_txs, $received)
  
  $token_distributed = COUNT(FLATTEN($distribution_txs)) > 0

// Trace where presale funds went
$fund_distribution = TRACE_PRESALE_FUNDS(
  presale_wallet: $presale_wallet,
  start_time: $presale_end
)

**Action:**
RETURN {
  investigation: "Fake ICO/Presale Scam",
  presale_wallet: $presale_wallet,
  promised_token: $promised_token,
  token_was_created: $token_exists,
  token_was_distributed: $token_distributed,
  total_investors: COUNT($investors),
  total_raised_sol: SUM(MAP($investors, i => i.amount_invested)),
  avg_investment_sol: MEAN(MAP($investors, i => i.amount_invested)),
  fund_distribution: $fund_distribution,
  is_confirmed_scam: !$token_distributed,
  investor_details: $investors,
  confidence: 88
}

---

## Q5630: "Detect a 'validator front-running' scheme where a validator operator uses their block production privilege to extract MEV from transactions in their blocks. Quantify unfair profits."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - DETECT_VALIDATOR_MEV_ABUSE (Forensics)
  - ANALYZE_BLOCK_ORDERING (MEV Analysis)

**Main Branch:**
$validator_vote_account = "..."
$validator_operator_wallet = "..." // Validator operator's known wallet
$analysis_period = {start: 1727222400, end: 1728432000} // 2 weeks

// Get all blocks produced by this validator
$blocks_produced = GET_BLOCKS_BY_VALIDATOR(
  validator: $validator_vote_account,
  start: $analysis_period.start,
  end: $analysis_period.end
)

// Analyze each block for suspicious transaction ordering
$mev_abuse_analysis = MAP(collection: $blocks_produced, fn: block => {
  $txs = getBlock(slot: block.slot).transactions
  
  // Check for validator wallet transactions
  $validator_txs = FILTER($txs, tx => tx.signer == $validator_operator_wallet)
  
  // Detect sandwiching or front-running
  $abuse_detected = DETECT_VALIDATOR_MEV_ABUSE(
    block_txs: $txs,
    validator_txs: $validator_txs
  )
  
  RETURN {
    slot: block.slot,
    has_mev_abuse: $abuse_detected.detected,
    abuse_type: $abuse_detected.type, // "sandwich", "frontrun", etc.
    validator_profit: $abuse_detected.validator_profit_sol,
    victim_txs: $abuse_detected.victim_transactions
  }
})

// Filter blocks with abuse
$blocks_with_abuse = FILTER($mev_abuse_analysis, b => b.has_mev_abuse)

// Calculate total unfair profits
$total_unfair_profit = SUM(MAP($blocks_with_abuse, b => b.validator_profit))

// Calculate victim losses
$total_victim_losses = 0
FOR $block IN $blocks_with_abuse:
  FOR $victim_tx IN $block.victim_txs:
    $total_victim_losses += $victim_tx.additional_slippage_sol

**Action:**
RETURN {
  investigation: "Validator Front-Running Scheme",
  validator: $validator_vote_account,
  operator_wallet: $validator_operator_wallet,
  analysis_period_days: ($analysis_period.end - $analysis_period.start) / 86400,
  total_blocks_produced: COUNT($blocks_produced),
  blocks_with_abuse: COUNT($blocks_with_abuse),
  abuse_percentage: (COUNT($blocks_with_abuse) / COUNT($blocks_produced)) * 100,
  total_unfair_profit_sol: $total_unfair_profit,
  total_victim_losses_sol: $total_victim_losses,
  abuse_types: UNIQUE(MAP($blocks_with_abuse, b => b.abuse_type)),
  block_details: $blocks_with_abuse,
  confidence: 83
}

---

## Q5661: "Detect 'spoofed LP tokens' where scammers create fake LP tokens that look identical to real ones to trick users into depositing value."

**Expected Plan:**
[TIME: ~5m] [COST: ~0.05 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: VERIFY_LP_TOKEN_AUTHENTICITY, DETECT_FAKE_LP_TOKENS

**Main Branch:**
$suspected_lp_token = "..."
$real_pool_address = "..."
$verification = VERIFY_LP_TOKEN_AUTHENTICITY(lp_token: $suspected_lp_token, known_pool: $real_pool_address)
IF !$verification.is_authentic:
  $victims = FIND_FAKE_LP_TOKEN_VICTIMS(fake_token: $suspected_lp_token)

**Action:**
RETURN {investigation: "Spoofed LP Token Scam", is_fake: !$verification.is_authentic, victim_count: COUNT($victims), total_loss_sol: SUM(MAP($victims, v => v.loss)), confidence: 89}

---

## Q5662: "Map a 'DAO treasury raid' where malicious proposal drains funds. Trace the proposal creation, voting patterns, and fund destinations."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_DAO_PROPOSAL, TRACE_TREASURY_FUNDS

**Main Branch:**
$dao_program = "..."
$malicious_proposal = "..."
$proposal_analysis = ANALYZE_DAO_PROPOSAL(dao: $dao_program, proposal: $malicious_proposal)
$fund_trace = TRACE_TREASURY_FUNDS(source: $proposal_analysis.treasury_address, drain_tx: $proposal_analysis.execution_tx)

**Action:**
RETURN {investigation: "DAO Treasury Raid", drained_amount_sol: $proposal_analysis.amount_drained, voting_manipulation: $proposal_analysis.suspicious_votes, fund_destinations: $fund_trace.final_addresses, confidence: 84}

---

## Q5663: "Identify 'circular trading bots' that create artificial volume by trading between their own wallets with no real market impact."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: DETECT_CIRCULAR_TRADING, BUILD_BOT_CLUSTER

**Main Branch:**
$token = "..."
$trades = GET_TOKEN_TRADES(token: $token, period: "24h")
$circular = DETECT_CIRCULAR_TRADING(trades: $trades, max_circle_size: 10)
$bot_cluster = BUILD_BOT_CLUSTER(circular_traders: $circular.involved_wallets)

**Action:**
RETURN {investigation: "Circular Trading Bot Network", bot_count: COUNT($bot_cluster.wallets), fake_volume_sol: $circular.total_circular_volume, real_volume_pct: $circular.real_volume_percentage, confidence: 87}

---

## Q5664: "Investigate a 'bridge exploit' where attacker mints unlimited tokens by exploiting cross-chain message verification."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_BRIDGE_EXPLOIT, TRACE_MINTED_TOKENS

**Main Branch:**
$bridge_contract = "..."
$exploit_tx = "..."
$analysis = ANALYZE_BRIDGE_EXPLOIT(bridge: $bridge_contract, exploit: $exploit_tx)
$minted_trace = TRACE_MINTED_TOKENS(source: $analysis.attacker_wallet, token: $analysis.exploited_token)

**Action:**
RETURN {investigation: "Bridge Exploit - Unlimited Mint", exploit_method: $analysis.vulnerability_type, tokens_minted: $analysis.mint_amount, attacker_profit_usd: $analysis.realized_profit, confidence: 81}

---

## Q5665: "Detect 'governance token rental markets' where users rent voting power, potentially enabling vote manipulation."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: FIND_TOKEN_RENTAL_PLATFORMS, ANALYZE_RENTAL_VOTING_IMPACT

**Main Branch:**
$gov_token = "..."
$rentals = FIND_TOKEN_RENTAL_PLATFORMS(token: $gov_token)
$voting_impact = ANALYZE_RENTAL_VOTING_IMPACT(rentals: $rentals, proposals: GET_RECENT_PROPOSALS($gov_token))

**Action:**
RETURN {investigation: "Governance Token Rental Market", rental_volume_daily: $rentals.avg_daily_volume, proposals_affected: COUNT($voting_impact.influenced_proposals), manipulation_risk: $voting_impact.risk_score, confidence: 85}

---

## Q5666: "Map a 'MEV sandwich cartel' where multiple bots coordinate to avoid competing on the same victim transactions."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: DETECT_MEV_BOT_COORDINATION, ANALYZE_VICTIM_ALLOCATION

**Main Branch:**
$mev_bots = FIND_SANDWICH_BOTS(period: "7d", min_profit: 10)
$coordination = DETECT_MEV_BOT_COORDINATION(bots: $mev_bots)
$allocation = ANALYZE_VICTIM_ALLOCATION(coordinated_bots: $coordination.cartel_members)

**Action:**
RETURN {investigation: "MEV Sandwich Cartel", cartel_size: COUNT($coordination.cartel_members), coordination_evidence: $coordination.evidence_score, profit_sharing_detected: $allocation.shows_coordination, confidence: 82}

---

## Q5667: "Investigate 'NFT metadata poisoning' where scammers inject malicious URLs into NFT metadata to phish holders."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library: SCAN_NFT_METADATA_URLS, DETECT_PHISHING_LINKS

**Main Branch:**
$collection = "..."
$all_nfts = GET_COLLECTION_NFTS(collection: $collection)
$url_scan = SCAN_NFT_METADATA_URLS(nfts: $all_nfts)
$malicious = DETECT_PHISHING_LINKS(urls: $url_scan.all_urls)

**Action:**
RETURN {investigation: "NFT Metadata Poisoning", poisoned_nfts: COUNT($malicious.nfts_with_bad_urls), phishing_domains: $malicious.malicious_domains, holder_risk: $malicious.estimated_victims, confidence: 90}

---

## Q5668: "Detect 'stablecoin depeg manipulation' where whales intentionally cause depegs to profit from panic selling."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: ANALYZE_DEPEG_EVENT, DETECT_MANIPULATION_TRADING

**Main Branch:**
$stablecoin = "..."
$depeg_timestamp = 1728432000
$depeg_analysis = ANALYZE_DEPEG_EVENT(token: $stablecoin, timestamp: $depeg_timestamp)
$manipulation = DETECT_MANIPULATION_TRADING(event: $depeg_analysis)

**Action:**
RETURN {investigation: "Stablecoin Depeg Manipulation", manipulation_detected: $manipulation.intentional_probability > 0.7, whale_traders: $manipulation.suspicious_wallets, manipulator_profit_usd: $manipulation.estimated_profit, confidence: 79}

---

## Q5669: "Map 'validator sandwich attack coordination' where validators collude to sandwich user transactions in their own blocks."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_SANDWICHES, DETECT_CROSS_VALIDATOR_COLLUSION

**Main Branch:**
$validators = [...]
$sandwiches = ANALYZE_VALIDATOR_SANDWICHES(validators: $validators, period: "14d")
$collusion = DETECT_CROSS_VALIDATOR_COLLUSION(sandwich_events: $sandwiches)

**Action:**
RETURN {investigation: "Validator Sandwich Collusion", colluding_validators: COUNT($collusion.coordinated_validators), total_extracted_sol: $collusion.total_mev, victim_transactions: COUNT($sandwiches.all_victims), confidence: 80}

---

## Q5670: "Investigate 'pump-and-dump as a service' platforms that coordinate paid pump campaigns for tokens."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: IDENTIFY_PUMP_SERVICE_WALLETS, MAP_PUMP_CAMPAIGNS

**Main Branch:**
$service_operator = "..."
$campaigns = MAP_PUMP_CAMPAIGNS(operator: $service_operator, period: "60d")
$client_analysis = ANALYZE_PUMP_CLIENTS(campaigns: $campaigns)

**Action:**
RETURN {investigation: "Pump-as-a-Service Platform", total_campaigns: COUNT($campaigns), client_count: COUNT($client_analysis.unique_clients), service_revenue_sol: $campaigns.total_fees_collected, confidence: 83}

---

## Q5671: "Detect 'fake liquidity lock' where scammers lock liquidity in a contract they control and can withdraw anytime."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: ANALYZE_LIQUIDITY_LOCK, DETECT_BACKDOOR_ACCESS

**Main Branch:**
$lock_contract = "..."
$lock_analysis = ANALYZE_LIQUIDITY_LOCK(contract: $lock_contract)
$backdoors = DETECT_BACKDOOR_ACCESS(contract: $lock_contract, lock_data: $lock_analysis)

**Action:**
RETURN {investigation: "Fake Liquidity Lock", is_fake_lock: $backdoors.can_bypass_lock, bypass_method: $backdoors.method, locked_amount_sol: $lock_analysis.locked_value, confidence: 88}

---

## Q5672: "Map 'cNFT minting exploits' where attackers mint compressed NFTs beyond the tree capacity by exploiting Merkle tree verification."

**Expected Plan:**
[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: ANALYZE_CNFT_TREE, DETECT_OVERMINT_EXPLOIT

**Main Branch:**
$tree_address = "..."
$tree_data = ANALYZE_CNFT_TREE(tree: $tree_address)
$overmint = DETECT_OVERMINT_EXPLOIT(tree: $tree_address, expected_capacity: $tree_data.max_capacity)

**Action:**
RETURN {investigation: "cNFT Overmint Exploit", exploit_detected: $overmint.detected, extra_mints: $overmint.mints_beyond_capacity, attacker_wallet: $overmint.exploiter, confidence: 81}

---

## Q5673: "Investigate 'prediction market oracle manipulation' where insiders with early knowledge manipulate market odds before public announcement."

**Expected Plan:**
[TIME: ~11m] [COST: ~0.11 SOL] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library: ANALYZE_PREDICTION_MARKET, DETECT_INSIDER_TRADING

**Main Branch:**
$market_address = "..."
$event_result = "..."
$market_analysis = ANALYZE_PREDICTION_MARKET(market: $market_address, result: $event_result)
$insider_trades = DETECT_INSIDER_TRADING(trades: $market_analysis.pre_result_trades, result_time: $market_analysis.result_timestamp)

**Action:**
RETURN {investigation: "Prediction Market Insider Trading", suspicious_traders: COUNT($insider_trades.likely_insiders), insider_profit_sol: $insider_trades.total_profit, trade_timing_correlation: $insider_trades.correlation_score, confidence: 76}

---

## Q5674: "Detect 'token supply inflation' attacks where minter role is exploited to create unlimited tokens and dump on market."

**Expected Plan:**
[TIME: ~6m] [COST: ~0.06 SOL] [CONFIDENCE: 91%]

**Available Tools:**
From Standard Library: ANALYZE_TOKEN_SUPPLY_CHANGES, DETECT_UNAUTHORIZED_MINTS

**Main Branch:**
$token_mint = "..."
$supply_history = ANALYZE_TOKEN_SUPPLY_CHANGES(mint: $token_mint, period: "30d")
$unauthorized = DETECT_UNAUTHORIZED_MINTS(supply_changes: $supply_history)

**Action:**
RETURN {investigation: "Token Supply Inflation Attack", inflation_detected: $unauthorized.detected, tokens_minted: $unauthorized.total_minted, dumped_amount: $unauthorized.sold_amount, price_impact_pct: $unauthorized.price_drop_pct, confidence: 91}

---

## Q5675: "Map a 'liquidation bot frontrunning cartel' where bots coordinate to avoid competing on the same liquidation opportunities."

**Expected Plan:**
[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: FIND_LIQUIDATION_BOTS, DETECT_BOT_COORDINATION

**Main Branch:**
$lending_protocol = "..."
$bots = FIND_LIQUIDATION_BOTS(protocol: $lending_protocol, period: "14d")
$coordination = DETECT_BOT_COORDINATION(bots: $bots, activity_type: "liquidations")

**Action:**
RETURN {investigation: "Liquidation Bot Cartel", cartel_size: COUNT($coordination.coordinated_bots), non_competitive_rate: $coordination.overlap_rate, shared_infrastructure: $coordination.common_funding, confidence: 82}

---

## Q5676: "Investigate 'fake airdrop claim' scams that require users to approve unlimited token spending to claim worthless tokens."

**Expected Plan:**
[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: ANALYZE_AIRDROP_CONTRACT, DETECT_APPROVAL_DRAIN_PATTERN

**Main Branch:**
$airdrop_contract = "..."
$contract_analysis = ANALYZE_AIRDROP_CONTRACT(contract: $airdrop_contract)
$drain_pattern = DETECT_APPROVAL_DRAIN_PATTERN(contract: $airdrop_contract, claimers: GET_AIRDROP_CLAIMERS($airdrop_contract))

**Action:**
RETURN {investigation: "Fake Airdrop Drain Scam", is_drainer: $drain_pattern.is_malicious, victims_drained: COUNT($drain_pattern.drained_wallets), total_stolen_usd: $drain_pattern.total_stolen, confidence: 89}

---

## Q5677: "Detect 'validator censorship' where specific validators consistently exclude certain transactions or protocols from their blocks."

**Expected Plan:**
[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: ANALYZE_VALIDATOR_BLOCK_CONTENT, DETECT_CENSORSHIP_PATTERNS

**Main Branch:**
$validator = "..."
$blocks = GET_VALIDATOR_BLOCKS(validator: $validator, period: "30d")
$content_analysis = ANALYZE_VALIDATOR_BLOCK_CONTENT(blocks: $blocks)
$censorship = DETECT_CENSORSHIP_PATTERNS(content: $content_analysis)

**Action:**
RETURN {investigation: "Validator Censorship", censorship_detected: $censorship.is_censoring, censored_protocols: $censorship.excluded_programs, censorship_rate_pct: $censorship.exclusion_rate, confidence: 78}

---

## Q5678: "Map 'cross-DEX arbitrage bot networks' that share infrastructure and coordinate to reduce gas competition."

**Expected Plan:**
[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: FIND_ARBITRAGE_BOTS, BUILD_BOT_NETWORK

**Main Branch:**
$dexes = ["Raydium", "Orca", "Jupiter"]
$arb_bots = FIND_ARBITRAGE_BOTS(dexes: $dexes, period: "14d", min_profit: 5)
$network = BUILD_BOT_NETWORK(bots: $arb_bots, link_by: ["funding", "timing", "strategy"])

**Action:**
RETURN {investigation: "Arbitrage Bot Network", network_clusters: COUNT($network.clusters), largest_cluster: MAX(MAP($network.clusters, c => COUNT(c.bots))), cluster_profit_sol: SUM(MAP($network.clusters, c => c.total_profit)), confidence: 83}

---

## Q5679: "Investigate 'mempool frontrunning on Solana' by analyzing Jito bundle patterns and identifying systematic frontrunners."

**Expected Plan:**
[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: ANALYZE_JITO_BUNDLES, DETECT_SYSTEMATIC_FRONTRUNNING

**Main Branch:**
$time_period = {start: 1728000000, end: 1728432000}
$bundles = ANALYZE_JITO_BUNDLES(period: $time_period)
$frontrunners = DETECT_SYSTEMATIC_FRONTRUNNING(bundles: $bundles)

**Action:**
RETURN {investigation: "Jito Bundle Frontrunning", systematic_frontrunners: COUNT($frontrunners.serial_frontrunners), total_frontrun_profit_sol: $frontrunners.total_profit, avg_tip_sol: $frontrunners.avg_jito_tip, confidence: 84}

---

## Q5680: "Detect 'rug pull via upgrade authority' where devs use program upgrade authority to inject malicious code after launch."

**Expected Plan:**
[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: MONITOR_PROGRAM_UPGRADES, DETECT_MALICIOUS_UPGRADES

**Main Branch:**
$program_id = "..."
$upgrades = MONITOR_PROGRAM_UPGRADES(program: $program_id)
$malicious = DETECT_MALICIOUS_UPGRADES(upgrades: $upgrades, check_for: ["drain_functions", "backdoors", "authority_changes"])

**Action:**
RETURN {investigation: "Malicious Program Upgrade", malicious_upgrade_detected: COUNT($malicious.suspicious_upgrades) > 0, upgrade_type: $malicious.suspicious_upgrades[0].malicious_changes, funds_at_risk_sol: $malicious.tvl_at_upgrade, confidence: 87}



---

## Q5681: "Reconstruct the transaction flow of a token airdrop scam to identify the scammer's cash-out method."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: GET_AIRDROP_RECIPIENTS, TRACE_FUNDS, GET_CEX_DEPOSITS

**Main Branch:**
$scam_airdrop_token = "..."

$recipients = GET_AIRDROP_RECIPIENTS(token: $scam_airdrop_token)
$scammer_wallet = FIND_AIRDROP_DEPLOYER($scam_airdrop_token)

$fund_trace = TRACE_FUNDS(start_address: $scammer_wallet, depth: 10, direction: "outbound")

$cex_deposits = FILTER($fund_trace.hops, h => IS_CEX_ADDRESS(h.destination))

**Action:**
RETURN {
  forensics: "Airdrop Scam Cash-Out Analysis",
  scammer_wallet: $scammer_wallet,
  total_recipients: COUNT($recipients),
  cex_deposits_found: $cex_deposits,
  confidence: 84
}

---

## Q682-Q690: [Additional forensics questions omitted for brevity - follow same OVSM format]


---


## Q5682: "Investigate cross-program reentrancy exploits in DeFi protocols."

**Expected Plan:**
[TIME: ~100s] [COST: ~0.018 SOL] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library: ANALYZE_PROGRAM_INTERACTIONS, DETECT_REENTRANCY

**Main Branch:**
$exploit_tx = "..."
$tx_data = getTransaction(signature: $exploit_tx)

$reentrancy_analysis = DETECT_REENTRANCY(transaction: $tx_data)

**Action:**
RETURN {
  forensics: "Reentrancy Exploit Investigation",
  exploit_signature: $exploit_tx,
  reentrancy_detected: $reentrancy_analysis.detected,
  affected_programs: $reentrancy_analysis.programs,
  confidence: 77
}

---

## Q5683: "Track the dispersal pattern of hacked funds using graph analysis."

**Expected Plan:**
[TIME: ~110s] [COST: ~0.02 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: BUILD_TRANSACTION_GRAPH, ANALYZE_DISPERSAL_PATTERN

**Main Branch:**
$hacker_wallet = "..."

$tx_graph = BUILD_TRANSACTION_GRAPH(
  root: $hacker_wallet,
  depth: 20,
  direction: "outbound"
)

$dispersal = ANALYZE_DISPERSAL_PATTERN(graph: $tx_graph)

**Action:**
RETURN {
  forensics: "Fund Dispersal Pattern Analysis",
  hacker_wallet: $hacker_wallet,
  unique_destinations: COUNT($dispersal.destinations),
  dispersal_strategy: $dispersal.strategy_type,
  confidence: 85
}

---

## Q5684: "Identify pump-and-dump coordinator wallets using statistical clustering."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.016 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: GET_COORDINATED_TRADES, CLUSTER_WALLETS

**Main Branch:**
$suspicious_token = "..."

$trades = GET_COORDINATED_TRADES(token: $suspicious_token, time_window: 300)
$wallet_clusters = CLUSTER_WALLETS(trades: $trades)

$coordinator_candidates = FILTER($wallet_clusters, c => c.coordination_score > 0.8)

**Action:**
RETURN {
  forensics: "Pump Coordinator Identification",
  token: $suspicious_token,
  coordinator_clusters: $coordinator_candidates,
  confidence: 81
}

---

## Q5685: "Reverse-engineer a token's smart contract to find hidden backdoors."

**Expected Plan:**
[TIME: ~120s] [COST: ~0.022 SOL] [CONFIDENCE: 74%]

**Available Tools:**
From Standard Library: GET_PROGRAM_CODE, DECOMPILE_PROGRAM, SCAN_FOR_BACKDOORS

**Main Branch:**
$token_program = "..."

$bytecode = GET_PROGRAM_CODE(program: $token_program)
$decompiled = DECOMPILE_PROGRAM(bytecode: $bytecode)

$backdoors = SCAN_FOR_BACKDOORS(code: $decompiled)

**Action:**
RETURN {
  forensics: "Smart Contract Backdoor Analysis",
  program: $token_program,
  backdoors_found: $backdoors,
  confidence: 74
}

---

## Q5686: "Trace NFT theft from wallet compromise through multiple marketplaces."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: GET_NFT_TRANSFERS, TRACK_MARKETPLACE_SALES

**Main Branch:**
$victim_wallet = "..."
$theft_timestamp = "2024-03-01T12:00:00Z"

$nft_transfers = GET_NFT_TRANSFERS(
  from: $victim_wallet,
  after: $theft_timestamp
)

$marketplace_sales = MAP($nft_transfers, nft => {
  TRACK_MARKETPLACE_SALES(nft_mint: nft.mint, after: $theft_timestamp)
})

**Action:**
RETURN {
  forensics: "NFT Theft Tracking",
  victim: $victim_wallet,
  stolen_nfts: COUNT($nft_transfers),
  marketplace_sales: $marketplace_sales,
  confidence: 89
}

---

## Q5687: "Detect oracle manipulation attempts by analyzing price feed anomalies."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library: GET_ORACLE_PRICES, DETECT_PRICE_ANOMALIES

**Main Branch:**
$oracle = "Pyth"
$asset = "SOL/USD"

$price_feed = GET_ORACLE_PRICES(oracle: $oracle, asset: $asset, period: "24h")
$anomalies = DETECT_PRICE_ANOMALIES(prices: $price_feed, threshold: 5)

**Action:**
RETURN {
  forensics: "Oracle Manipulation Detection",
  oracle: $oracle,
  asset: $asset,
  anomalies_detected: $anomalies,
  confidence: 86
}

---

## Q5688: "Investigate fake liquidity pools created to trap traders."

**Expected Plan:**
[TIME: ~80s] [COST: ~0.013 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: GET_ALL_POOLS, ANALYZE_POOL_LEGITIMACY

**Main Branch:**
$token_mint = "..."

$all_pools = GET_ALL_POOLS(token: $token_mint)
$legitimacy_scores = MAP($all_pools, p => ANALYZE_POOL_LEGITIMACY(pool: p))

$fake_pools = FILTER($legitimacy_scores, s => s.score < 0.3)

**Action:**
RETURN {
  forensics: "Fake Liquidity Pool Detection",
  token: $token_mint,
  total_pools: COUNT($all_pools),
  fake_pools: $fake_pools,
  confidence: 83
}

---

## Q5689: "Analyze smart contract upgrade patterns to detect malicious changes."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.016 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: GET_PROGRAM_UPGRADE_HISTORY, DIFF_PROGRAM_VERSIONS

**Main Branch:**
$program_id = "..."

$upgrade_history = GET_PROGRAM_UPGRADE_HISTORY(program: $program_id)
$version_diffs = MAP($upgrade_history, (v1, v2) => DIFF_PROGRAM_VERSIONS(v1, v2))

$suspicious_changes = FILTER($version_diffs, d => d.risk_score > 0.7)

**Action:**
RETURN {
  forensics: "Malicious Upgrade Detection",
  program: $program_id,
  total_upgrades: COUNT($upgrade_history),
  suspicious_changes: $suspicious_changes,
  confidence: 78
}

---

## Q5690: "Reconstruct the attack timeline of a multi-stage DeFi exploit."

**Expected Plan:**
[TIME: ~105s] [COST: ~0.019 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: GET_EXPLOIT_TRANSACTIONS, BUILD_ATTACK_TIMELINE

**Main Branch:**
$protocol = "..."
$exploit_date = "2024-03-10"

$exploit_txs = GET_EXPLOIT_TRANSACTIONS(protocol: $protocol, date: $exploit_date)
$timeline = BUILD_ATTACK_TIMELINE(transactions: $exploit_txs)

**Action:**
RETURN {
  forensics: "Multi-Stage Exploit Timeline",
  protocol: $protocol,
  attack_stages: $timeline.stages,
  total_value_extracted: $timeline.total_stolen,
  confidence: 80
}

---

## Q5691: "Analyze cross-protocol flash loan attacks to identify attack pattern reuse across exploits."

**Expected Plan:**
[TIME: ~110s] [COST: ~0.018 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: GET_FLASH_LOAN_ATTACKS, IDENTIFY_ATTACK_PATTERNS

**Main Branch:**
$flash_loan_attacks = GET_FLASH_LOAN_ATTACKS(period: "90d", min_value: 100000)

$attack_patterns = MAP($flash_loan_attacks, attack => {
  RETURN IDENTIFY_ATTACK_PATTERNS(
    transactions: attack.txs,
    loan_source: attack.loan_protocol,
    target_protocol: attack.target
  )
})

$pattern_clusters = GROUP_BY($attack_patterns, p => p.pattern_signature)
$repeated_patterns = FILTER($pattern_clusters, cluster => COUNT(cluster) > 1)

**Action:**
RETURN {forensics: "Flash Loan Attack Patterns", total_attacks: COUNT($flash_loan_attacks), unique_patterns: COUNT($pattern_clusters), repeated_patterns: COUNT($repeated_patterns), most_common_pattern: MAX_BY($pattern_clusters, c => COUNT(c)), confidence: 82}

---

## Q5692: "Trace stolen NFTs through multiple wallet hops to identify final destination wallets."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: GET_NFT_TRANSFER_HISTORY, TRACE_NFT_PATH

**Main Branch:**
$stolen_nft_mint = "..."
$theft_timestamp = 1728000000

$transfer_history = GET_NFT_TRANSFER_HISTORY(
  nft: $stolen_nft_mint,
  since: $theft_timestamp
)

$trace_path = TRACE_NFT_PATH(transfers: $transfer_history)

$final_destinations = FILTER($trace_path.wallets, w => w.is_terminal && w.holding_period > 604800) // Held for > 7 days

**Action:**
RETURN {forensics: "Stolen NFT Tracing", nft: $stolen_nft_mint, total_hops: COUNT($transfer_history), final_destinations: $final_destinations, likely_thief_wallet: $trace_path.most_likely_culprit, confidence: 87}

---

## Q5693: "Detect smart contract backdoors by analyzing unusual admin function calls."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: GET_PROGRAM_INSTRUCTIONS, DETECT_BACKDOOR_PATTERNS

**Main Branch:**
$program_id = "..."

$all_instructions = GET_PROGRAM_INSTRUCTIONS(program: $program_id, period: "30d")

$admin_calls = FILTER($all_instructions, ix => ix.signer == ix.program_admin || ix.requires_admin_authority)

$backdoor_analysis = DETECT_BACKDOOR_PATTERNS(
  admin_calls: $admin_calls,
  check_for: ["emergency_withdrawals", "arbitrary_transfers", "authority_changes"]
)

**Action:**
RETURN {forensics: "Smart Contract Backdoor Detection", program: $program_id, total_admin_calls: COUNT($admin_calls), suspicious_calls: $backdoor_analysis.suspicious_count, backdoor_functions: $backdoor_analysis.potential_backdoors, risk_level: $backdoor_analysis.risk_score, confidence: 84}

---

## Q5694: "Investigate pump-and-dump coordination by analyzing Telegram/Discord timestamps correlated with on-chain buys."

**Expected Plan:**
[TIME: ~120s] [COST: ~0.020 SOL] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library: GET_SOCIAL_MESSAGES, GET_TOKEN_TRADES, CORRELATE_TIMESTAMPS

**Main Bridge:**
$token_mint = "..."
$pump_date = "2024-10-05"

$social_messages = GET_SOCIAL_MESSAGES(
  platforms: ["Telegram", "Discord"],
  keywords: ["pump", "buy now", "moon"],
  date: $pump_date
)

$on_chain_trades = GET_TOKEN_TRADES(
  token: $token_mint,
  date: $pump_date,
  type: "buy"
)

$correlation = CORRELATE_TIMESTAMPS(
  social_events: $social_messages,
  trading_events: $on_chain_trades,
  time_window_seconds: 300 // 5-minute correlation window
)

**Action:**
RETURN {forensics: "Pump-and-Dump Coordination", token: $token_mint, social_pump_messages: COUNT($social_messages), coordinated_buys: $correlation.correlated_count, correlation_strength: $correlation.correlation_coefficient, suspected_organizers: $correlation.key_influencers, confidence: 76}

---

## Q5695: "Analyze MEV bot profit extraction patterns to identify systematic frontrunning operations."

**Expected Plan:**
[TIME: ~100s] [COST: ~0.016 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: FIND_MEV_BOTS, ANALYZE_FRONTRUNNING_PATTERNS

**Main Branch:**
$time_period = {start: 1727000000, end: 1728000000}

$mev_bots = FIND_MEV_BOTS(
  period: $time_period,
  min_profit_sol: 1,
  strategy: "frontrunning"
)

$bot_patterns = MAP($mev_bots, bot => {
  $pattern = ANALYZE_FRONTRUNNING_PATTERNS(
    bot_wallet: bot.wallet,
    transactions: bot.txs
  )
  RETURN {
    wallet: bot.wallet,
    total_profit: bot.total_profit_sol,
    avg_profit_per_tx: bot.total_profit_sol / COUNT(bot.txs),
    frontrun_success_rate: $pattern.success_rate,
    target_protocols: $pattern.targeted_protocols
  }
})

$top_performers = TOP_N(SORT_BY($bot_patterns, b => -b.total_profit), 10)

**Action:**
RETURN {forensics: "MEV Frontrunning Analysis", time_period_days: 30, total_mev_bots: COUNT($mev_bots), total_extracted_sol: SUM(MAP($mev_bots, b => b.total_profit_sol)), top_10_bots: $top_performers, confidence: 85}

---

## Q5696: "Trace laundered funds through Tornado Cash-style mixers on Solana."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: IDENTIFY_MIXER_USAGE, TRACE_THROUGH_MIXERS

**Main Branch:**
$suspicious_wallet = "..."
$stolen_amount_sol = 500

$mixer_deposits = IDENTIFY_MIXER_USAGE(
  wallet: $suspicious_wallet,
  lookback_days: 30
)

TRY:
  $trace_results = TRACE_THROUGH_MIXERS(
    deposits: $mixer_deposits,
    expected_amount: $stolen_amount_sol,
    confidence_threshold: 0.7
  )
CATCH RECOVERABLE:
  $trace_results = {likely_destinations: [], confidence: 0}

**Decision Point:** Evaluate tracing success
  BRANCH A (COUNT($trace_results.likely_destinations) > 0):
    $verdict = "Partial trace successful"
    $details = "Found likely destination wallets despite mixer obfuscation"
  BRANCH B (default):
    $verdict = "Trace lost in mixer"
    $details = "Mixer effectively obscured fund flow"

**Action:**
RETURN {forensics: "Mixer Fund Tracing", source_wallet: $suspicious_wallet, mixer_deposits_sol: SUM(MAP($mixer_deposits, d => d.amount)), likely_destinations: $trace_results.likely_destinations, verdict: $verdict, confidence: 79}

---

## Q5697: "Detect Sybil attack networks in token airdrops by analyzing wallet funding patterns."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.014 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: GET_AIRDROP_CLAIMERS, DETECT_SYBIL_PATTERNS

**Main Branch:**
$airdrop_contract = "..."

$all_claimers = GET_AIRDROP_CLAIMERS(contract: $airdrop_contract)

$sybil_analysis = DETECT_SYBIL_PATTERNS(
  wallets: $all_claimers,
  check_patterns: ["common_funding_source", "sequential_creation", "similar_transaction_patterns", "coordinated_claiming"]
)

$sybil_clusters = $sybil_analysis.identified_clusters
$total_sybil_wallets = SUM(MAP($sybil_clusters, c => COUNT(c.wallets)))

$legitimate_claimers = COUNT($all_claimers) - $total_sybil_wallets
$sybil_percentage = ($total_sybil_wallets / COUNT($all_claimers)) * 100

**Action:**
RETURN {forensics: "Sybil Attack Detection", total_claimers: COUNT($all_claimers), sybil_wallets: $total_sybil_wallets, sybil_percentage: $sybil_percentage, sybil_clusters: COUNT($sybil_clusters), largest_cluster_size: MAX(MAP($sybil_clusters, c => COUNT(c.wallets))), confidence: 83}

---

## Q5698: "Investigate insider trading by correlating wallet purchases with upcoming token listings or announcements."

**Expected Plan:**
[TIME: ~105s] [COST: ~0.017 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: GET_TOKEN_LISTINGS, GET_LARGE_PURCHASES, CORRELATE_EVENTS

**Main Branch:**
$token_mint = "..."
$listing_date = "2024-10-10"

$large_purchases_before_listing = GET_LARGE_PURCHASES(
  token: $token_mint,
  start_date: $listing_date - 604800, // 7 days before
  end_date: $listing_date,
  min_size_usd: 10000
)

$purchase_timings = MAP($large_purchases_before_listing, p => {
  $hours_before_listing = ($listing_date - p.timestamp) / 3600
  RETURN {wallet: p.wallet, amount_usd: p.amount_usd, hours_before: $hours_before_listing}
})

$suspicious_purchases = FILTER($purchase_timings, p => p.hours_before < 48 && p.amount_usd > 50000)

**Action:**
RETURN {forensics: "Insider Trading Investigation", token: $token_mint, listing_date: $listing_date, total_large_purchases_7d_before: COUNT($large_purchases_before_listing), suspicious_insider_purchases: COUNT($suspicious_purchases), suspected_insiders: MAP($suspicious_purchases, p => p.wallet), confidence: 78}

---

## Q5699: "Analyze governance attack vectors by identifying wallets that suddenly acquired large voting power."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.013 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library: GET_GOVERNANCE_TOKEN_HOLDERS, DETECT_SUDDEN_ACCUMULATION

**Main Branch:**
$governance_token = "..."
$proposal_date = "2024-10-08"

$holders_before = GET_GOVERNANCE_TOKEN_HOLDERS(
  token: $governance_token,
  snapshot_date: $proposal_date - 604800 // 7 days before
)

$holders_at_vote = GET_GOVERNANCE_TOKEN_HOLDERS(
  token: $governance_token,
  snapshot_date: $proposal_date
)

$new_large_holders = DETECT_SUDDEN_ACCUMULATION(
  before: $holders_before,
  after: $holders_at_vote,
  threshold_increase_pct: 500 // 5x increase
)

$attack_likelihood = COUNT($new_large_holders) > 5 ? "High" : (COUNT($new_large_holders) > 2 ? "Medium" : "Low")

**Action:**
RETURN {forensics: "Governance Attack Detection", governance_token: $governance_token, proposal_date: $proposal_date, wallets_with_sudden_power: COUNT($new_large_holders), attack_likelihood: $attack_likelihood, suspected_attackers: $new_large_holders, confidence: 86}

---

## Q5700: "Detect token supply manipulation by analyzing unexpected mint authority usage."

**Expected Plan:**
[TIME: ~70s] [COST: ~0.011 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: GET_TOKEN_MINT_HISTORY, ANALYZE_MINT_AUTHORITY_USAGE

**Main Branch:**
$token_mint = "..."

$mint_history = GET_TOKEN_MINT_HISTORY(token: $token_mint, period: "90d")

$unauthorized_mints = FILTER($mint_history, mint_event => {
  $is_after_renounce = mint_event.timestamp > $token_mint.authority_renounced_at
  $is_excessive = mint_event.amount > ($token_mint.total_supply * 0.05) // >5% of supply
  RETURN $is_after_renounce || $is_excessive
})

**Action:**
RETURN {forensics: "Token Supply Manipulation", token: $token_mint, total_mint_events: COUNT($mint_history), unauthorized_mints: COUNT($unauthorized_mints), supply_inflation_pct: (SUM(MAP($unauthorized_mints, m => m.amount)) / $token_mint.total_supply) * 100, confidence: 88}

---

## Q5701: "Investigate NFT wash trading rings by analyzing circular transfer patterns."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: GET_NFT_COLLECTION_TRADES, DETECT_CIRCULAR_TRADING

**Main Branch:**
$collection = "..."

$all_trades = GET_NFT_COLLECTION_TRADES(collection: $collection, period: "30d")

$circular_patterns = DETECT_CIRCULAR_TRADING(
  trades: $all_trades,
  max_circle_length: 5,
  min_circle_repetitions: 3
)

$involved_wallets = UNIQUE(FLATTEN(MAP($circular_patterns, pattern => pattern.wallets)))
$fake_volume_sol = SUM(MAP($circular_patterns, p => p.total_volume))

**Action:**
RETURN {forensics: "NFT Wash Trading Detection", collection: $collection, circular_patterns_found: COUNT($circular_patterns), involved_wallets: COUNT($involved_wallets), fake_volume_sol: $fake_volume_sol, real_vs_fake_volume_ratio: ($all_trades.total_volume - $fake_volume_sol) / $fake_volume_sol, confidence: 81}

---

## Q5702: "Trace rug pull funds from liquidity removal through DEX aggregators to final CEX deposits."

**Expected Plan:**
[TIME: ~115s] [COST: ~0.019 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: TRACE_RUG_PULL_FUNDS, IDENTIFY_CEX_DEPOSITS

**Main Branch:**
$rug_tx = "..."

$liquidity_removed = GET_TRANSACTION_DETAILS(tx: $rug_tx)
$stolen_amount_sol = $liquidity_removed.sol_amount

$fund_trace = TRACE_RUG_PULL_FUNDS(
  source_tx: $rug_tx,
  max_hops: 20,
  through_aggregators: true
)

$cex_deposits = IDENTIFY_CEX_DEPOSITS(
  destination_wallets: $fund_trace.terminal_wallets
)

**Action:**
RETURN {forensics: "Rug Pull Fund Tracing", rug_tx: $rug_tx, stolen_sol: $stolen_amount_sol, trace_hops: COUNT($fund_trace.all_transactions), terminal_wallets: COUNT($fund_trace.terminal_wallets), cex_deposits_found: COUNT($cex_deposits), identified_exchanges: MAP($cex_deposits, d => d.exchange_name), confidence: 80}

---

## Q5703: "Analyze smart contract exploit simulations to predict vulnerable protocol targets."

**Expected Plan:**
[TIME: ~125s] [COST: ~0.021 SOL] [CONFIDENCE: 74%]

**Available Tools:**
From Standard Library: GET_RECENT_EXPLOITS, IDENTIFY_SIMILAR_VULNERABILITIES

**Main Branch:**
$recent_exploits = GET_RECENT_EXPLOITS(period: "180d", min_value: 100000)

$exploit_patterns = MAP($recent_exploits, exploit => {
  RETURN {
    vulnerability_type: exploit.vulnerability,
    protocol_type: exploit.protocol_category,
    attack_vector: exploit.attack_method
  }
})

$pattern_frequency = GROUP_BY($exploit_patterns, p => p.vulnerability_type)

$vulnerable_protocols = IDENTIFY_SIMILAR_VULNERABILITIES(
  known_exploits: $recent_exploits,
  scan_protocols: GET_ALL_DEFI_PROTOCOLS()
)

**Action:**
RETURN {forensics: "Exploit Pattern Analysis", exploits_analyzed: COUNT($recent_exploits), most_common_vulnerability: MAX_BY($pattern_frequency, g => COUNT(g)).vulnerability_type, potentially_vulnerable_protocols: $vulnerable_protocols, confidence: 74}

---

## Q5704: "Detect oracle price manipulation attacks by comparing multiple oracle sources."

**Expected Plan:**
[TIME: ~80s] [COST: ~0.013 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: GET_MULTI_ORACLE_PRICES, DETECT_PRICE_DIVERGENCE

**Main Branch:**
$asset = "SOL/USD"
$timestamp = 1728000000

$oracle_prices = GET_MULTI_ORACLE_PRICES(
  asset: $asset,
  timestamp: $timestamp,
  oracles: ["Pyth", "Switchboard", "Chainlink"]
)

$divergence_analysis = DETECT_PRICE_DIVERGENCE(
  prices: $oracle_prices,
  max_acceptable_divergence_pct: 2
)

**Decision Point:** Evaluate manipulation likelihood
  BRANCH A ($divergence_analysis.max_divergence > 5):
    $verdict = "Likely manipulation detected"
    $manipulated_oracle = $divergence_analysis.outlier_oracle
  BRANCH B ($divergence_analysis.max_divergence > 2):
    $verdict = "Possible manipulation or stale data"
    $manipulated_oracle = $divergence_analysis.outlier_oracle
  BRANCH C (default):
    $verdict = "No manipulation detected"
    $manipulated_oracle = null

**Action:**
RETURN {forensics: "Oracle Manipulation Detection", asset: $asset, oracle_count: COUNT($oracle_prices), max_divergence_pct: $divergence_analysis.max_divergence, verdict: $verdict, suspected_oracle: $manipulated_oracle, confidence: 85}

---

## Q5705: "Investigate token launch sniper bots by analyzing first-block purchase patterns."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: GET_TOKEN_LAUNCH_BLOCK, GET_FIRST_BUYERS

**Main Branch:**
$token_mint = "..."

$launch_block = GET_TOKEN_LAUNCH_BLOCK(token: $token_mint)
$first_100_buyers = GET_FIRST_BUYERS(token: $token_mint, count: 100)

$sniper_bots = FILTER($first_100_buyers, buyer => {
  $purchased_in_launch_block = buyer.purchase_slot == $launch_block
  $large_purchase = buyer.amount_usd > 1000
  $has_bot_characteristics = buyer.tx_priority_fee > 0.01 || buyer.uses_private_rpc
  RETURN $purchased_in_launch_block && ($large_purchase || $has_bot_characteristics)
})

$sniper_percentage = (COUNT($sniper_bots) / COUNT($first_100_buyers)) * 100

**Action:**
RETURN {forensics: "Launch Sniper Bot Detection", token: $token_mint, launch_slot: $launch_block, first_100_buyers: COUNT($first_100_buyers), sniper_bots_identified: COUNT($sniper_bots), sniper_percentage: $sniper_percentage, total_sniped_value_usd: SUM(MAP($sniper_bots, s => s.amount_usd)), confidence: 87}

---

## Q5706: "Trace Ponzi scheme reward payments through affiliate referral trees."

**Expected Plan:**
[TIME: ~100s] [COST: ~0.016 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: BUILD_REFERRAL_TREE, ANALYZE_PONZI_MECHANICS

**Main Branch:**
$ponzi_program = "..."

$all_participants = GET_PROGRAM_PARTICIPANTS(program: $ponzi_program)

$referral_tree = BUILD_REFERRAL_TREE(
  participants: $all_participants,
  root_identifiers: ["deployer", "first_depositor"]
)

$ponzi_analysis = ANALYZE_PONZI_MECHANICS(
  tree: $referral_tree,
  payment_flow: GET_REWARD_PAYMENTS($ponzi_program)
)

$profitablelevels = FILTER($ponzi_analysis.level_profitability, level => level.net_profit > 0)

**Action:**
RETURN {forensics: "Ponzi Scheme Analysis", program: $ponzi_program, total_participants: COUNT($all_participants), tree_depth: $referral_tree.max_depth, profitable_levels: MAP($profitable_levels, l => l.level_number), total_deposits_sol: $ponzi_analysis.total_inflow, total_withdrawals_sol: $ponzi_analysis.total_outflow, collapse_imminent: $ponzi_analysis.collapse_probability > 0.7, confidence: 82}

---

## Q5707: "Detect fake trading volume bots by analyzing repetitive transaction patterns."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADES, DETECT_REPETITIVE_PATTERNS

**Main Branch:**
$token_mint = "..."

$all_trades = GET_TOKEN_TRADES(token: $token_mint, period: "7d")

$pattern_analysis = DETECT_REPETITIVE_PATTERNS(
  trades: $all_trades,
  check_for: ["identical_amounts", "regular_intervals", "ping_pong_trading"]
)

$bot_wallets = UNIQUE(FLATTEN(MAP($pattern_analysis.detected_patterns, p => p.involved_wallets)))
$fake_volume = SUM(MAP($pattern_analysis.detected_patterns, p => p.volume_sol))

$fake_volume_percentage = ($fake_volume / $all_trades.total_volume_sol) * 100

**Action:**
RETURN {forensics: "Fake Volume Bot Detection", token: $token_mint, total_volume_sol: $all_trades.total_volume_sol, fake_volume_sol: $fake_volume, fake_volume_percentage: $fake_volume_percentage, bot_wallets: COUNT($bot_wallets), repetitive_patterns_found: COUNT($pattern_analysis.detected_patterns), confidence: 84}

---

## Q5708: "Investigate liquidity pool reentrancy exploits by analyzing transaction call stacks."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.015 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: GET_EXPLOIT_TRANSACTION, ANALYZE_CALL_STACK

**Main Branch:**
$exploit_tx = "..."

$tx_details = GET_EXPLOIT_TRANSACTION(signature: $exploit_tx)

$call_stack_analysis = ANALYZE_CALL_STACK(
  transaction: $tx_details,
  check_for: ["reentrancy", "recursive_calls", "state_manipulation"]
)

$reentrancy_detected = $call_stack_analysis.reentrancy_count > 0

**Decision Point:** Classify exploit type
  BRANCH A ($call_stack_analysis.reentrancy_count > 1):
    $exploit_type = "Reentrancy Attack"
    $severity = "Critical"
  BRANCH B ($call_stack_analysis.has_state_manipulation):
    $exploit_type = "State Manipulation"
    $severity = "High"
  BRANCH C (default):
    $exploit_type = "Unknown"
    $severity = "Medium"

**Action:**
RETURN {forensics: "Reentrancy Exploit Analysis", exploit_tx: $exploit_tx, reentrancy_detected: $reentrancy_detected, reentrancy_depth: $call_stack_analysis.reentrancy_count, exploit_type: $exploit_type, severity: $severity, stolen_amount_sol: $tx_details.value_extracted, confidence: 83}

---

## Q5709: "Analyze mempool frontrunning by comparing transaction submission timing vs inclusion order."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: GET_BLOCK_TRANSACTIONS, ANALYZE_TX_ORDERING

**Main Branch:**
$block_slot = 250000000

$block_txs = GET_BLOCK_TRANSACTIONS(slot: $block_slot)

$ordering_analysis = ANALYZE_TX_ORDERING(
  transactions: $block_txs,
  compare: ["submission_timestamp", "inclusion_order", "priority_fees"]
)

$frontrun_cases = FILTER($ordering_analysis.tx_pairs, pair => {
  $victim_submitted_first = pair.victim_timestamp < pair.frontrunner_timestamp
  $frontrunner_executed_first = pair.frontrunner_position < pair.victim_position
  RETURN $victim_submitted_first && $frontrunner_executed_first
})

**Action:**
RETURN {forensics: "Mempool Frontrunning Analysis", block_slot: $block_slot, total_transactions: COUNT($block_txs), frontrun_cases_detected: COUNT($frontrun_cases), frontrunner_wallets: UNIQUE(MAP($frontrun_cases, c => c.frontrunner_wallet)), average_priority_fee_advantage: AVG(MAP($frontrun_cases, c => c.fee_difference)), confidence: 81}

---

## Q5710: "Detect coordinated market manipulation by identifying synchronized wallet activity."

**Expected Plan:**
[TIME: ~105s] [COST: ~0.017 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRADERS, DETECT_SYNCHRONIZED_ACTIVITY

**Main Branch:**
$token_mint = "..."
$suspicious_date = "2024-10-05"

$all_traders = GET_TOKEN_TRADERS(
  token: $token_mint,
  date: $suspicious_date,
  min_trade_size: 1000
)

$synchronization_analysis = DETECT_SYNCHRONIZED_ACTIVITY(
  wallets: $all_traders,
  time_window_seconds: 60,
  min_cluster_size: 5
)

$coordinated_clusters = FILTER($synchronization_analysis.clusters, cluster => cluster.synchronization_score > 0.8)

**Action:**
RETURN {forensics: "Market Manipulation Detection", token: $token_mint, date: $suspicious_date, total_traders: COUNT($all_traders), coordinated_clusters: COUNT($coordinated_clusters), largest_cluster_size: MAX(MAP($coordinated_clusters, c => COUNT(c.wallets))), manipulation_probability: AVG(MAP($coordinated_clusters, c => c.synchronization_score)), confidence: 79}

---

## Q5711: "Investigate token freezing authority abuse by analyzing freeze/thaw transaction patterns."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library: GET_FREEZE_AUTHORITY_ACTIONS, ANALYZE_FREEZE_PATTERNS

**Main Branch:**
$token_mint = "..."

$freeze_actions = GET_FREEZE_AUTHORITY_ACTIONS(
  token: $token_mint,
  period: "90d"
)

$freeze_pattern_analysis = ANALYZE_FREEZE_PATTERNS(
  actions: $freeze_actions,
  check_for: ["selective_freezing", "frontrun_freezing", "censorship_patterns"]
)

$abuse_indicators = {
  selective_targeting: $freeze_pattern_analysis.targets_specific_wallets,
  timing_suspicious: $freeze_pattern_analysis.correlated_with_price_movements,
  censorship_detected: $freeze_pattern_analysis.blocks_large_sellers
}

$abuse_score = SUM([
  $abuse_indicators.selective_targeting ? 1 : 0,
  $abuse_indicators.timing_suspicious ? 1 : 0,
  $abuse_indicators.censorship_detected ? 1 : 0
])

**Action:**
RETURN {forensics: "Freeze Authority Abuse", token: $token_mint, total_freeze_actions: COUNT($freeze_actions), freeze_count: $freeze_pattern_analysis.freeze_count, thaw_count: $freeze_pattern_analysis.thaw_count, abuse_indicators: $abuse_indicators, abuse_score: $abuse_score, confidence: 86}

---

## Q5712: "Trace cross-chain bridge exploits from source chain to destination chain fund movements."

**Expected Plan:**
[TIME: ~110s] [COST: ~0.018 SOL] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library: GET_BRIDGE_EXPLOIT_TX, TRACE_CROSS_CHAIN_FUNDS

**Main Branch:**
$exploit_tx_source_chain = "..."
$bridge_contract = "Wormhole"

$exploit_details = GET_BRIDGE_EXPLOIT_TX(
  tx: $exploit_tx_source_chain,
  bridge: $bridge_contract
)

$cross_chain_trace = TRACE_CROSS_CHAIN_FUNDS(
  source_tx: $exploit_tx_source_chain,
  source_chain: $exploit_details.source_chain,
  dest_chain: "Solana",
  bridge: $bridge_contract
)

$destination_wallets = $cross_chain_trace.destination_wallets
$fund_movements = $cross_chain_trace.all_movements

**Action:**
RETURN {forensics: "Cross-Chain Bridge Exploit Tracing", exploit_tx: $exploit_tx_source_chain, bridge: $bridge_contract, stolen_amount_usd: $exploit_details.value_stolen, destination_chain: "Solana", destination_wallets: COUNT($destination_wallets), total_movements: COUNT($fund_movements), funds_recovered_pct: $cross_chain_trace.recovery_percentage, confidence: 77}

---

## Q5713: "Detect honeypot tokens by analyzing failed sell transaction patterns."

**Expected Plan:**
[TIME: ~80s] [COST: ~0.013 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library: GET_TOKEN_TRANSACTIONS, ANALYZE_FAILED_SELLS

**Main Branch:**
$token_mint = "..."

$all_txs = GET_TOKEN_TRANSACTIONS(token: $token_mint, period: "30d")

$sell_attempts = FILTER($all_txs, tx => tx.transaction_type == "sell")

$failed_sells = FILTER($sell_attempts, tx => tx.status == "failed")

$failure_analysis = ANALYZE_FAILED_SELLS(
  failed_txs: $failed_sells,
  check_reasons: ["transfer_disabled", "high_sell_tax", "blacklist", "max_sell_limit"]
)

$honeypot_score = ($failed_sells.count / $sell_attempts.count) * 100

**Decision Point:** Classify honeypot risk
  BRANCH A ($honeypot_score > 80 && $failure_analysis.transfer_disabled):
    $verdict = "Definite Honeypot"
    $risk = "Critical"
  BRANCH B ($honeypot_score > 50):
    $verdict = "Likely Honeypot"
    $risk = "High"
  BRANCH C ($honeypot_score > 20):
    $verdict = "Possible Restrictions"
    $risk = "Medium"
  BRANCH D (default):
    $verdict = "Normal Token"
    $risk = "Low"

**Action:**
RETURN {forensics: "Honeypot Token Detection", token: $token_mint, total_sell_attempts: COUNT($sell_attempts), failed_sells: COUNT($failed_sells), failure_rate_pct: $honeypot_score, failure_reasons: $failure_analysis.reasons, verdict: $verdict, risk_level: $risk, confidence: 88}

---

## Q5714: "Investigate smart contract upgrade backdoors inserted via malicious governance proposals."

**Expected Plan:**
[TIME: ~100s] [COST: ~0.016 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library: GET_GOVERNANCE_PROPOSALS, ANALYZE_PROPOSAL_CODE_CHANGES

**Main Branch:**
$dao_program = "..."

$all_proposals = GET_GOVERNANCE_PROPOSALS(
  dao: $dao_program,
  status: "executed",
  period: "180d"
)

$upgrade_proposals = FILTER($all_proposals, p => p.proposal_type == "program_upgrade")

$code_change_analysis = MAP($upgrade_proposals, proposal => {
  RETURN ANALYZE_PROPOSAL_CODE_CHANGES(
    proposal: proposal,
    check_for: ["new_admin_functions", "backdoor_patterns", "emergency_withdrawals"]
  )
})

$malicious_proposals = FILTER($code_change_analysis, analysis => analysis.risk_score > 0.7)

**Action:**
RETURN {forensics: "Governance Backdoor Investigation", dao: $dao_program, total_proposals: COUNT($all_proposals), upgrade_proposals: COUNT($upgrade_proposals), suspicious_upgrades: COUNT($malicious_proposals), malicious_proposals: $malicious_proposals, confidence: 75}

---

## Q5715: "Analyze liquidation bot competition and potential collusion patterns."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: FIND_LIQUIDATION_BOTS, ANALYZE_BOT_COMPETITION

**Main Branch:**
$lending_protocol = "Solend"

$liquidation_bots = FIND_LIQUIDATION_BOTS(
  protocol: $lending_protocol,
  period: "30d",
  min_liquidations: 10
)

$competition_analysis = ANALYZE_BOT_COMPETITION(
  bots: $liquidation_bots,
  check_for: ["overlapping_targets", "coordinated_timing", "profit_sharing_patterns"]
)

$collusion_detected = $competition_analysis.collusion_score > 0.6

**Action:**
RETURN {forensics: "Liquidation Bot Analysis", protocol: $lending_protocol, active_bots: COUNT($liquidation_bots), total_liquidations: SUM(MAP($liquidation_bots, b => b.liquidation_count)), collusion_detected: $collusion_detected, collusion_score: $competition_analysis.collusion_score, suspected_cartel_size: COUNT($competition_analysis.potential_cartel_members), confidence: 80}

---

## Q5716: "Detect NFT metadata manipulation attacks where images/attributes are changed post-mint."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: GET_NFT_METADATA_HISTORY, DETECT_METADATA_CHANGES

**Main Branch:**
$nft_collection = "..."

$all_nfts = GET_COLLECTION_NFTS(collection: $nft_collection, sample_size: 1000)

$metadata_changes = MAP($all_nfts, nft => {
  $history = GET_NFT_METADATA_HISTORY(nft: nft.mint)
  RETURN DETECT_METADATA_CHANGES(
    history: $history,
    check_for: ["image_url_change", "attribute_modification", "suspicious_timing"]
  )
})

$manipulated_nfts = FILTER($metadata_changes, change => change.manipulation_detected)

**Action:**
RETURN {forensics: "NFT Metadata Manipulation", collection: $nft_collection, nfts_checked: COUNT($all_nfts), manipulated_nfts: COUNT($manipulated_nfts), manipulation_rate_pct: (COUNT($manipulated_nfts) / COUNT($all_nfts)) * 100, common_manipulation_types: GROUP_BY($manipulated_nfts, m => m.manipulation_type), confidence: 84}

---

## Q5717: "Investigate validator MEV extraction bias toward affiliated protocols."

**Expected Plan:**
[TIME: ~105s] [COST: ~0.017 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: GET_VALIDATOR_BLOCKS, ANALYZE_MEV_EXTRACTION

**Main Branch:**
$validator_identity = "..."

$validator_blocks = GET_VALIDATOR_BLOCKS(
  validator: $validator_identity,
  period: "30d"
)

$mev_analysis = ANALYZE_MEV_EXTRACTION(
  blocks: $validator_blocks,
  check_for: ["protocol_favoritism", "tx_ordering_bias", "affiliated_bot_preference"]
)

$affiliated_protocols = $mev_analysis.protocols_with_preferential_treatment

**Decision Point:** Evaluate bias severity
  BRANCH A (COUNT($affiliated_protocols) > 3 && $mev_analysis.bias_score > 0.7):
    $verdict = "Significant bias detected"
    $severity = "High"
  BRANCH B (COUNT($affiliated_protocols) > 0):
    $verdict = "Some bias detected"
    $severity = "Medium"
  BRANCH C (default):
    $verdict = "No significant bias"
    $severity = "Low"

**Action:**
RETURN {forensics: "Validator MEV Bias Investigation", validator: $validator_identity, blocks_analyzed: COUNT($validator_blocks), affiliated_protocols: $affiliated_protocols, bias_score: $mev_analysis.bias_score, verdict: $verdict, severity: $severity, confidence: 78}

---

## Q5718: "Trace phishing attack proceeds from fake airdrop sites to final cash-out points."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.014 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: IDENTIFY_PHISHING_VICTIMS, TRACE_STOLEN_ASSETS

**Main Branch:**
$phishing_contract = "..."

$victims = IDENTIFY_PHISHING_VICTIMS(
  contract: $phishing_contract,
  period: "14d"
)

$stolen_assets = MAP($victims, victim => victim.assets_lost)

$trace_results = TRACE_STOLEN_ASSETS(
  assets: FLATTEN($stolen_assets),
  max_hops: 15,
  identify_cashout: true
)

$cashout_points = $trace_results.identified_cashout_locations

**Action:**
RETURN {forensics: "Phishing Attack Tracing", phishing_contract: $phishing_contract, total_victims: COUNT($victims), total_stolen_usd: SUM(MAP($stolen_assets, a => a.value_usd)), trace_successful: $trace_results.success, cashout_points: $cashout_points, exchanges_identified: MAP($cashout_points, c => c.exchange_name), confidence: 82}

---

## Q5719: "Detect sandwich attack patterns and identify systematic sandwiching bots."

**Expected Plan:**
[TIME: ~100s] [COST: ~0.016 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: GET_DEX_TRANSACTIONS, DETECT_SANDWICH_ATTACKS

**Main Branch:**
$dex = "Raydium"
$time_period = {start: 1727000000, end: 1728000000}

$all_dex_txs = GET_DEX_TRANSACTIONS(
  dex: $dex,
  period: $time_period,
  tx_types: ["swap"]
)

$sandwich_analysis = DETECT_SANDWICH_ATTACKS(
  transactions: $all_dex_txs,
  detection_window_slots: 2
)

$sandwich_bots = GROUP_BY($sandwich_analysis.sandwiches, s => s.attacker_wallet)

$systematic_bots = FILTER($sandwich_bots, bot => COUNT(bot.sandwiches) > 100)

**Action:**
RETURN {forensics: "Sandwich Attack Detection", dex: $dex, period_days: 30, total_sandwiches: COUNT($sandwich_analysis.sandwiches), unique_attackers: COUNT($sandwich_bots), systematic_bots: COUNT($systematic_bots), total_extracted_sol: SUM(MAP($sandwich_analysis.sandwiches, s => s.profit_sol)), top_bot: MAX_BY($systematic_bots, b => SUM(MAP(b.sandwiches, s => s.profit_sol))), confidence: 85}

---

## Q5720: "Analyze token vesting contract bypasses where locked tokens are accessed early."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.013 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: GET_VESTING_CONTRACTS, DETECT_EARLY_WITHDRAWALS

**Main Branch:**
$vesting_program = "..."

$all_vesting_contracts = GET_VESTING_CONTRACTS(program: $vesting_program)

$early_withdrawal_analysis = MAP($all_vesting_contracts, contract => {
  RETURN DETECT_EARLY_WITHDRAWALS(
    contract: contract,
    check_methods: ["exploit", "admin_override", "contract_bug"]
  )
})

$bypassed_contracts = FILTER($early_withdrawal_analysis, analysis => analysis.early_withdrawal_detected)

$total_unlocked_early = SUM(MAP($bypassed_contracts, c => c.amount_withdrawn_early))

**Action:**
RETURN {forensics: "Vesting Contract Bypass Detection", vesting_program: $vesting_program, total_contracts: COUNT($all_vesting_contracts), contracts_bypassed: COUNT($bypassed_contracts), total_unlocked_early: $total_unlocked_early, bypass_methods: GROUP_BY($bypassed_contracts, c => c.bypass_method), confidence: 81}

---

## Q5721: "Investigate coordinated governance attacks via flash-loaned voting power."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library: GET_GOVERNANCE_VOTES, DETECT_FLASH_LOAN_VOTING

**Main Branch:**
$dao = "..."
$proposal_id = "..."

$all_votes = GET_GOVERNANCE_VOTES(
  dao: $dao,
  proposal: $proposal_id
)

$flash_loan_voting_analysis = DETECT_FLASH_LOAN_VOTING(
  votes: $all_votes,
  check_patterns: ["borrow_vote_return", "voting_power_spike", "same_block_borrowing"]
)

$flash_loan_votes = $flash_loan_voting_analysis.identified_flash_votes

$attack_success = $flash_loan_voting_analysis.changed_outcome

**Action:**
RETURN {forensics: "Flash Loan Governance Attack", dao: $dao, proposal: $proposal_id, total_votes: COUNT($all_votes), flash_loan_votes: COUNT($flash_loan_votes), flash_loan_voting_power: SUM(MAP($flash_loan_votes, v => v.voting_power)), attack_changed_outcome: $attack_success, confidence: 79}

---

## Q5722: "Detect token supply auditing failures by comparing on-chain supply with claimed tokenomics."

**Expected Plan:**
[TIME: ~70s] [COST: ~0.011 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: GET_TOKEN_SUPPLY, GET_TOKENOMICS_DOCUMENTATION

**Main Branch:**
$token_mint = "..."

$onchain_supply = GET_TOKEN_SUPPLY(token: $token_mint)

$claimed_tokenomics = GET_TOKENOMICS_DOCUMENTATION(
  token: $token_mint,
  sources: ["whitepaper", "website", "social_media"]
)

$supply_discrepancy = ABS($onchain_supply.total - $claimed_tokenomics.stated_total)
$discrepancy_pct = ($supply_discrepancy / $claimed_tokenomics.stated_total) * 100

**Decision Point:** Evaluate discrepancy severity
  BRANCH A ($discrepancy_pct > 10):
    $verdict = "Major tokenomics discrepancy"
    $risk = "High"
  BRANCH B ($discrepancy_pct > 2):
    $verdict = "Minor tokenomics mismatch"
    $risk = "Medium"
  BRANCH C (default):
    $verdict = "Tokenomics match on-chain data"
    $risk = "Low"

**Action:**
RETURN {forensics: "Tokenomics Audit", token: $token_mint, onchain_supply: $onchain_supply.total, claimed_supply: $claimed_tokenomics.stated_total, discrepancy: $supply_discrepancy, discrepancy_pct: $discrepancy_pct, verdict: $verdict, risk_level: $risk, confidence: 87}

---

## Q5723: "Analyze cross-protocol arbitrage bot networks and their shared infrastructure."

**Expected Plan:**
[TIME: ~110s] [COST: ~0.018 SOL] [CONFIDENCE: 76%]

**Available Tools:**
From Standard Library: FIND_ARBITRAGE_BOTS, BUILD_BOT_NETWORK_GRAPH

**Main Branch:**
$protocols = ["Raydium", "Orca", "Phoenix", "Serum"]

$arb_bots = FIND_ARBITRAGE_BOTS(
  protocols: $protocols,
  period: "30d",
  min_trades: 50
)

$network_graph = BUILD_BOT_NETWORK_GRAPH(
  bots: $arb_bots,
  link_by: ["shared_funding", "similar_strategies", "coordinated_timing", "common_rpc"]
)

$bot_clusters = $network_graph.identified_clusters

**Action:**
RETURN {forensics: "Arbitrage Bot Network Analysis", protocols_monitored: COUNT($protocols), total_arb_bots: COUNT($arb_bots), bot_clusters: COUNT($bot_clusters), largest_cluster_size: MAX(MAP($bot_clusters, c => COUNT(c.bots))), shared_infrastructure_evidence: $network_graph.infrastructure_overlap, confidence: 76}

---

## Q5724: "Investigate NFT royalty bypass techniques and measure ecosystem royalty evasion."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.014 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: GET_NFT_SALES, DETECT_ROYALTY_BYPASS

**Main Branch:**
$collection = "..."

$all_sales = GET_NFT_SALES(
  collection: $collection,
  period: "60d"
)

$royalty_analysis = MAP($all_sales, sale => {
  RETURN DETECT_ROYALTY_BYPASS(
    sale: sale,
    expected_royalty_pct: $collection.royalty_percentage,
    bypass_methods: ["p2p_transfer", "zero_royalty_marketplace", "fake_sale"]
  )
})

$bypassed_sales = FILTER($royalty_analysis, a => a.royalty_paid < a.expected_royalty * 0.5)

$lost_royalties_sol = SUM(MAP($bypassed_sales, s => s.expected_royalty - s.royalty_paid))

**Action:**
RETURN {forensics: "NFT Royalty Evasion Analysis", collection: $collection, total_sales: COUNT($all_sales), bypassed_sales: COUNT($bypassed_sales), bypass_rate_pct: (COUNT($bypassed_sales) / COUNT($all_sales)) * 100, lost_royalties_sol: $lost_royalties_sol, common_bypass_methods: GROUP_BY($bypassed_sales, s => s.bypass_method), confidence: 83}

---

## Q5725: "Detect automated trading bot failures that result in significant losses."

**Expected Plan:**
[TIME: ~80s] [COST: ~0.013 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: IDENTIFY_TRADING_BOTS, ANALYZE_BOT_PERFORMANCE

**Main Branch:**
$time_period = {start: 1727000000, end: 1728000000}

$trading_bots = IDENTIFY_TRADING_BOTS(
  period: $time_period,
  min_trades: 100,
  bot_indicators: ["high_frequency", "algorithmic_patterns"]
)

$performance_analysis = MAP($trading_bots, bot => {
  RETURN ANALYZE_BOT_PERFORMANCE(
    bot_wallet: bot.wallet,
    trades: bot.trades,
    detect_failures: true
  )
})

$failed_bots = FILTER($performance_analysis, p => p.total_loss_sol > 10)

**Action:**
RETURN {forensics: "Trading Bot Failure Analysis", period_days: 30, total_bots: COUNT($trading_bots), failed_bots: COUNT($failed_bots), total_bot_losses_sol: SUM(MAP($failed_bots, b => b.total_loss_sol)), common_failure_reasons: GROUP_BY($failed_bots, b => b.failure_reason), confidence: 82}

---

## Q5726: "Investigate token migration scams where new tokens are not equivalent to old tokens."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library: GET_TOKEN_MIGRATION_EVENTS, VERIFY_MIGRATION_FAIRNESS

**Main Branch:**
$old_token = "..."
$new_token = "..."

$migration_data = GET_TOKEN_MIGRATION_EVENTS(
  old_token: $old_token,
  new_token: $new_token
)

$fairness_analysis = VERIFY_MIGRATION_FAIRNESS(
  migration: $migration_data,
  check_for: ["incorrect_ratio", "missing_holders", "dev_allocation_increase"]
)

$scam_indicators = {
  unfair_ratio: $fairness_analysis.actual_ratio != $fairness_analysis.announced_ratio,
  holders_excluded: $fairness_analysis.unmigrated_holders > 0.05 * $fairness_analysis.total_old_holders,
  dev_supply_increase: $fairness_analysis.new_dev_allocation > $fairness_analysis.old_dev_allocation
}

$scam_score = SUM([
  $scam_indicators.unfair_ratio ? 1 : 0,
  $scam_indicators.holders_excluded ? 1 : 0,
  $scam_indicators.dev_supply_increase ? 1 : 0
])

**Action:**
RETURN {forensics: "Token Migration Scam Investigation", old_token: $old_token, new_token: $new_token, announced_ratio: $fairness_analysis.announced_ratio, actual_ratio: $fairness_analysis.actual_ratio, scam_indicators: $scam_indicators, scam_score: $scam_score, verdict: $scam_score >= 2 ? "Likely Scam" : "Appears Legitimate", confidence: 84}

---

## Q5727: "Analyze staking pool validator selection manipulation for MEV extraction advantages."

**Expected Plan:**
[TIME: ~95s] [COST: ~0.015 SOL] [CONFIDENCE: 78%]

**Available Tools:**
From Standard Library: GET_STAKING_POOL_VALIDATORS, ANALYZE_VALIDATOR_SELECTION_BIAS

**Main Branch:**
$staking_pool = "..."

$validator_selections = GET_STAKING_POOL_VALIDATORS(
  pool: $staking_pool,
  period: "90d",
  include_history: true
)

$bias_analysis = ANALYZE_VALIDATOR_SELECTION_BIAS(
  selections: $validator_selections,
  check_for: ["mev_optimized_only", "affiliated_validators", "unfair_rotation"]
)

$biased_selections = $bias_analysis.bias_detected

**Decision Point:** Evaluate manipulation
  BRANCH A ($bias_analysis.mev_optimization_bias > 0.7):
    $verdict = "Validator selection manipulated for MEV extraction"
  BRANCH B ($bias_analysis.affiliation_bias > 0.6):
    $verdict = "Validator selection favors affiliated operators"
  BRANCH C (default):
    $verdict = "Validator selection appears fair"

**Action:**
RETURN {forensics: "Validator Selection Manipulation", staking_pool: $staking_pool, total_validators: COUNT($validator_selections), bias_detected: $biased_selections, mev_bias_score: $bias_analysis.mev_optimization_bias, affiliation_bias_score: $bias_analysis.affiliation_bias, verdict: $verdict, confidence: 78}

---

## Q5728: "Detect token bundler exploits where multiple token operations are combined for manipulation."

**Expected Plan:**
[TIME: ~90s] [COST: ~0.014 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: GET_BUNDLED_TRANSACTIONS, ANALYZE_BUNDLE_MANIPULATION

**Main Branch:**
$token_mint = "..."

$bundled_txs = GET_BUNDLED_TRANSACTIONS(
  token: $token_mint,
  period: "30d",
  bundle_source: "Jito"
)

$manipulation_analysis = MAP($bundled_txs, bundle => {
  RETURN ANALYZE_BUNDLE_MANIPULATION(
    bundle: bundle,
    check_for: ["sandwich_within_bundle", "flash_loan_abuse", "price_oracle_manipulation"]
  )
})

$exploitative_bundles = FILTER($manipulation_analysis, a => a.manipulation_score > 0.7)

**Action:**
RETURN {forensics: "Token Bundler Exploit Detection", token: $token_mint, total_bundles: COUNT($bundled_txs), exploitative_bundles: COUNT($exploitative_bundles), exploitation_rate_pct: (COUNT($exploitative_bundles) / COUNT($bundled_txs)) * 100, total_extracted_value_sol: SUM(MAP($exploitative_bundles, e => e.extracted_value)), common_exploit_types: GROUP_BY($exploitative_bundles, e => e.manipulation_type), confidence: 80}

---

## Q5729: "Investigate MEV searcher collusion rings that coordinate to avoid competition."

**Expected Plan:**
[TIME: ~105s] [COST: ~0.017 SOL] [CONFIDENCE: 77%]

**Available Tools:**
From Standard Library: FIND_MEV_SEARCHERS, DETECT_SEARCHER_COLLUSION

**Main Branch:**
$time_period = {start: 1727000000, end: 1728000000}

$mev_searchers = FIND_MEV_SEARCHERS(
  period: $time_period,
  min_profit: 5,
  strategies: ["arbitrage", "liquidation", "sandwich"]
)

$collusion_analysis = DETECT_SEARCHER_COLLUSION(
  searchers: $mev_searchers,
  indicators: ["non_competing_targets", "coordinated_timing", "profit_sharing", "shared_infrastructure"]
)

$collusion_rings = $collusion_analysis.identified_rings

**Action:**
RETURN {forensics: "MEV Searcher Collusion Investigation", period_days: 30, total_searchers: COUNT($mev_searchers), collusion_rings_found: COUNT($collusion_rings), largest_ring_size: MAX(MAP($collusion_rings, r => COUNT(r.members))), total_coordinated_profit_sol: SUM(MAP($collusion_rings, r => r.total_profit)), collusion_evidence_strength: $collusion_analysis.confidence_score, confidence: 77}

