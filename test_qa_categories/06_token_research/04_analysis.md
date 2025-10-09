# Token Research - Practical Token Analysis (Q301-Q400)

**Category:** Token Research
**Difficulty:** Intermediate
**Focus:** Practical token analysis, tokenomics, liquidity, holder behavior, competitive analysis, fundamental value
**Questions:** Q301-Q400

---

## Q301: "What is the tokenomics breakdown for 'Jupiter' (JUP), including total supply, circulating supply, and the distribution between team, investors, and community?"

**Expected Plan:**

[TIME: ~30s] [COST: ~0.002 SOL] [CONFIDENCE: 98%]

**Available Tools:**
From Standard Library:
  - getTokenSupply (Solana RPC)
  - getAccountInfo (Solana RPC)
  - ANALYZE_TOKENOMICS_DISTRIBUTION (Analysis)
  - SEARCH_DOCS (Web/External)

**Main Branch:**
$token_mint = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"

// Get on-chain supply data
$total_supply = getTokenSupply(mint: $token_mint).value.uiAmount
$circulating_supply_onchain = GET_CIRCULATING_SUPPLY(mint: $token_mint) // A conceptual tool

// Find official tokenomics documentation
$docs = SEARCH_DOCS(query: "Jupiter JUP tokenomics official")
$documented_allocations = PARSE_TOKENOMICS_DOCS($docs)

// Analyze known large wallets (team, treasury)
$distribution = ANALYZE_TOKENOMICS_DISTRIBUTION(
  mint: $token_mint,
  known_allocations: $documented_allocations
)

**Decision Point:** Compare on-chain data with documented tokenomics.
  BRANCH A ($distribution.discrepancy_found):
    $verdict = "Discrepancy Found"
    $details = "On-chain holder distribution does not perfectly match the officially stated tokenomics. Further investigation of large wallets is needed."
  BRANCH B (default):
    $verdict = "Matches Documentation"
    $details = "On-chain data aligns with the documented allocation for team, investors, and community."

**Action:**
RETURN {
  verdict: $verdict,
  details: $details,
  total_supply: $total_supply,
  circulating_supply: $circulating_supply_onchain,
  allocations: $distribution.allocations,
  confidence: 98
}

---

## Q302: "How deep is the liquidity for the 'dogwifhat' (WIF) token on its primary Raydium pool, and how large of a trade can be made before experiencing 3% price impact?"

**Expected Plan:**

[TIME: ~40s] [COST: ~0.004 SOL] [CONFIDENCE: 95%]

**Available Tools:**
From Standard Library:
  - RAYDIUM_GET_POOL_INFO (Raydium API)
  - CALCULATE_TRADE_IMPACT (DeFi)

**Main Branch:**
$pool_id = "..." // The primary WIF/SOL pool on Raydium

// Get the current state of the liquidity pool
$pool_info = RAYDIUM_GET_POOL_INFO(pool_id: $pool_id)
$sol_reserves = $pool_info.reserves.sol
$wif_reserves = $pool_info.reserves.wif

// Calculate trade sizes for different price impact levels
$impact_analysis = CALCULATE_TRADE_IMPACT(
  base_reserves: $sol_reserves,
  quote_reserves: $wif_reserves,
  impact_levels_bps: [100, 200, 300, 500] // 1%, 2%, 3%, 5%
)

$trade_for_3_percent_impact = FILTER(collection: $impact_analysis, predicate: i => i.impact_bps == 300)[0]

**Decision Point:** Evaluate the liquidity depth.
  BRANCH A ($trade_for_3_percent_impact.trade_size_usd > 100000):
    $liquidity_level = "Deep"
    $assessment = "The pool is very deep. A trade of over $100,000 can be executed with less than 3% price impact."
  BRANCH B ($trade_for_3_percent_impact.trade_size_usd > 20000):
    $liquidity_level = "Moderate"
    $assessment = "The pool has moderate liquidity. Whales need to be cautious, but retail-sized trades will have minimal impact."
  BRANCH C (default):
    $liquidity_level = "Shallow"
    $assessment = "The pool is shallow. Even trades above a few thousand dollars will cause significant slippage."

**Action:**
RETURN {
  liquidity_level: $liquidity_level,
  assessment: $assessment,
  total_liquidity_usd: $pool_info.liquidity_usd,
  trade_size_for_3_percent_impact_usd: $trade_for_3_percent_impact.trade_size_usd,
  full_impact_analysis: $impact_analysis,
  confidence: 95
}

---

## Q303: "What are the primary on-chain use cases and demand drivers for the 'Pyth Network' (PYTH) token?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - getProgramAccounts (Solana RPC)
  - ANALYZE_PROGRAM_INTERACTIONS (Analysis)
  - SEARCH_DOCS (Web/External)

**Main Branch:**
$token_mint = "HZ1JovNiVvGrGNiiYvEozEVgZ58AQuUnqeEhw2jwF7ht" // PYTH
$pyth_program_id = "..." // Pyth's main oracle program

// 1. Find documented utility
$docs = SEARCH_DOCS(query: "Pyth Network PYTH token utility")
$documented_uses = PARSE_UTILITY_DOCS($docs) // e.g., Governance, Staking

// 2. Find on-chain usage by seeing which programs hold/interact with PYTH
$pyth_holders = getProgramAccounts(programId: "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", filters: [{tokenMint: $token_mint}])
$interacting_programs = ANALYZE_PROGRAM_INTERACTIONS(token_holders: $pyth_holders)

**Decision Point:** Synthesize the findings.
  BRANCH A (CONTAINS($documented_uses, "Governance") && $interacting_programs.has_governance_usage):
    $primary_use = "Governance"
    $details = "The main use case is participating in Pyth DAO governance to vote on proposals. This is confirmed by on-chain data showing large amounts of PYTH locked in the governance contract."
  BRANCH B (CONTAINS($documented_uses, "Staking") && $interacting_programs.has_staking_usage):
    $details = "The token is used for staking by data publishers. On-chain analysis shows significant flows to and from known publisher wallets and staking contracts."
  BRANCH C (default):
    $primary_use = "Speculation"
    $details = "While documentation mentions utility, the majority of on-chain volume is on DEXs, suggesting speculation is the primary driver currently."

**Action:**
RETURN {
  details: $details,
  documented_utilities: $documented_uses,
  onchain_interactions: $interacting_programs.summary,
  confidence: 90
}

---

## Q304: "What percentage of 'Marinade Staked SOL' (mSOL) holders are actively using it in DeFi protocols versus just holding it in their wallets?"

**Expected Plan:**

[TIME: ~75s] [COST: ~0.01 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - ANALYZE_HOLDER_BEHAVIOR (Analysis)
  - MAP, FILTER, SUM (Data Processing)

**Main Branch:**
$token_mint = "mSoLzYCxHdYgdzU16g5QSh3i5K3z3KZK7ytfqcJm7So"

// Get a large sample of mSOL holders
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)

// Analyze the behavior of each holder
$behavior_analysis = ANALYZE_HOLDER_BEHAVIOR(holders: $holders)

// Categorize holders
$defi_users = FILTER(collection: $behavior_analysis, predicate: h => h.is_using_in_defi)
$passive_holders = FILTER(collection: $behavior_analysis, predicate: h => !h.is_using_in_defi)

// Calculate percentages based on volume
$total_volume = SUM(MAP(collection: $holders, fn: h => h.amount))
$defi_volume = SUM(MAP(collection: $defi_users, fn: h => h.amount))
$passive_volume = SUM(MAP(collection: $passive_holders, fn: h => h.amount))

$defi_percentage = ($defi_volume / $total_volume) * 100
$passive_percentage = ($passive_volume / $total_volume) * 100

**Decision Point:** Interpret the holder behavior.
  BRANCH A ($defi_percentage > 60):
    $conclusion = "Highly active in DeFi."
    $summary = "A strong majority of mSOL is being actively used as collateral or in liquidity pools, indicating high composability and utility."
  BRANCH B ($defi_percentage > 30):
    $conclusion = "Moderately active in DeFi."
    $summary = "A significant portion of mSOL is used in DeFi, but a large amount is also held passively, suggesting users value it as both a productive asset and a store of value."
  BRANCH C (default):

    ## Q321: "Which meme tokens have the highest percentage of holders in profit, and what entry prices did they buy at?"

    ## Q322: "How can you analyze the liquidity depth of a new meme token to determine the maximum safe buy size for a trader?"

    ## Q323: "What is the best way to track the top 10 wallets accumulating a meme token before a major event?"

    ## Q324: "How can you use prediction market odds to inform entry and exit points for meme token trades?"

    ## Q325: "What is the average time between a meme token's launch and its first major whale sell-off?"

    ## Q326: "How can you identify wallets that are likely to dump a meme token based on on-chain behavioral patterns?"

    ## Q327: "What is the most profitable strategy for rotating between meme tokens and prediction market bets during high volatility?"

    ## Q328: "How can you use on-chain volume and order flow to predict the next meme token to trend on social media?"

    ## Q329: "What is the best way to hedge a meme token position using prediction markets on Solana?"

    ## Q330: "How can you detect and avoid wallets that are likely to be exit liquidity in a meme token pump and dump?"
    $summary = "The majority of mSOL is held in wallets, not actively used in other protocols. Its primary value to holders may be the underlying staking yield itself."

**Action:**
RETURN {
  conclusion: $conclusion,
  summary: $summary,
  percentage_in_defi: $defi_percentage,
  percentage_held_passively: $passive_percentage,
  top_defi_protocols_used: $behavior_analysis.top_protocols,
  confidence: 92
}

---

## Q305: "How does the on-chain development activity for 'Helium Network Token' (HNT) compare to its competitor 'Render' (RNDR) over the last 90 days?"

**Expected Plan:**

[TIME: ~50s] [COST: ~0.005 SOL] [CONFIDENCE: 90%]

**Available Tools:**
From Standard Library:
  - SEARCH_GITHUB (Web/External)
  - GET_ONCHAIN_PROGRAM_UPDATES (Analysis)

**Main Branch:**
$hnt_program_id = "..."
$rndr_program_id = "..."
$hnt_github_repo = "helium/helium-program-library"
$rndr_github_repo = "rendernetwork/render-program-library"

// 1. Get on-chain program updates
$hnt_updates = GET_ONCHAIN_PROGRAM_UPDATES(program_id: $hnt_program_id, time_period: "90d")
$rndr_updates = GET_ONCHAIN_PROGRAM_UPDATES(program_id: $rndr_program_id, time_period: "90d")

// 2. Get GitHub commit activity
$hnt_commits = SEARCH_GITHUB(repo: $hnt_github_repo, query: "commits in last 90 days")
$rndr_commits = SEARCH_GITHUB(repo: $rndr_github_repo, query: "commits in last 90 days")

**Decision Point:** Compare the development activity levels.
  BRANCH A (COUNT($hnt_commits) > COUNT($rndr_commits) && COUNT($hnt_updates) > COUNT($rndr_updates)):
    $winner = "Helium (HNT)"
    $comparison = "HNT shows significantly more development activity both on-chain and in its public GitHub repository compared to RNDR."
  BRANCH B (COUNT($rndr_commits) > COUNT($hnt_commits) && COUNT($rndr_updates) > COUNT($hnt_updates)):
    $winner = "Render (RNDR)"
    $comparison = "RNDR shows significantly more development activity both on-chain and in its public GitHub repository compared to HNT."
  BRANCH C (default):
    $winner = "Comparable"
    $comparison = "Both projects show a similar level of development activity, indicating both are actively being maintained and improved."

**Action:**
RETURN {
  winner: $winner,
  comparison_summary: $comparison,
  hnt_metrics: {
    github_commits_90d: COUNT($hnt_commits),
    onchain_updates_90d: COUNT($hnt_updates)
  },
  rndr_metrics: {
    github_commits_90d: COUNT($rndr_commits),
    onchain_updates_90d: COUNT($rndr_updates)
  },
  confidence: 90
}

---

## Q306: "What is the current social media sentiment for 'Bonk' (BONK), and how does it correlate with its trading volume over the past 7 days?"

**Expected Plan:**

[TIME: ~60s] [COST: ~0.006 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TWITTER_GET_SENTIMENT_HISTORY (Social API)
  - BIRDEYE_GET_VOLUME_HISTORY (Birdeye API)
  - CORRELATE_DATASETS (Statistical)

**Main Branch:**
$cashtag = "$BONK"
$token_address = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"

// Get 7-day history for sentiment and volume
$sentiment_history = TWITTER_GET_SENTIMENT_HISTORY(query: $cashtag, time_period: "7d", interval: "1h")
$volume_history = BIRDEYE_GET_VOLUME_HISTORY(token_address: $token_address, time_period: "7d", interval: "1h")

// Correlate the two time series
$correlation = CORRELATE_DATASETS(
  dataset_A: $sentiment_history,
  dataset_B: $volume_history,
  lag_hours: 1 // Check if sentiment leads volume by 1 hour
)

**Decision Point:** Interpret the correlation between sentiment and volume.
  BRANCH A ($correlation.score > 0.7):
    $finding = "Strong Positive Correlation"
    $interpretation = "Social media sentiment is a strong leading indicator for trading volume. Spikes in positive sentiment are quickly followed by spikes in trading activity."
  BRANCH B ($correlation.score < -0.7):
    $finding = "Strong Negative Correlation"
    $interpretation = "Spikes in negative sentiment are strongly correlated with trading volume, suggesting fear and panic selling are major drivers of activity."
  BRANCH C (default):
    $finding = "Weak Correlation"
    $interpretation = "Social media sentiment does not appear to be a significant driver of trading volume. Volume is likely driven by other factors like market structure or news."

**Action:**
RETURN {
  finding: $finding,
  interpretation: $interpretation,
  correlation_score: $correlation.score,
  sentiment_leading_volume_by_hours: $correlation.lag_hours,
  current_sentiment_score: LAST($sentiment_history).score,
  confidence: 85
}

---

## Q307: "Is the smart contract for the 'Jeo Boden' (BODEN) token immutable, or does the owner retain potentially dangerous permissions like freezing transfers or minting new tokens?"

**Expected Plan:**

[TIME: ~25s] [COST: ~0.001 SOL] [CONFIDENCE: 99%]

**Available Tools:**
From Standard Library:
  - getAccountInfo (Solana RPC)
  - ANALYZE_CONTRACT_PERMISSIONS (Security)

**Main Branch:**
$token_mint = "3psH1Mj1f7yUfaD5gh6a1sHk4S_... " // BODEN mint address

// Get the token's mint account data
$mint_info = getAccountInfo(pubkey: $token_mint)

// Analyze the permissions from the mint account
$permissions = ANALYZE_CONTRACT_PERMISSIONS(mint_account_data: $mint_info)

**Decision Point:** Assess the contract's security and immutability.
  BRANCH A ($permissions.is_mutable && $permissions.has_mint_authority):
    $risk = "EXTREMELY HIGH"
    $details = "The contract is not immutable, and a wallet still holds mint authority. This wallet can create an infinite number of new tokens at any time, destroying the token's value."
  BRANCH B ($permissions.is_mutable && $permissions.has_freeze_authority):
    $risk = "HIGH"
    $details = "The contract is not immutable, and a wallet still holds freeze authority. This wallet can freeze transfers for any holder, effectively locking their assets."
  BRANCH C (!$permissions.is_mutable):
    $risk = "LOW"
    $details = "The contract is immutable. Both the mint and freeze authorities have been revoked, which is the security standard for a decentralized token."
  BRANCH D (default):
    $risk = "MODERATE"
    $details = "The contract may have some remaining authorities, but they are not critical ones like minting new supply."

**Action:**
RETURN {
  risk_level: $risk,
  details: $details,
  is_immutable: !$permissions.is_mutable,
  mint_authority: $permissions.mint_authority,
  freeze_authority: $permissions.freeze_authority,
  confidence: 99
}

---

## Q308: "What is the distribution of 'JitoSOL' (JITOSOL) holders, and are there any red flags like an excessive concentration in a few non-protocol wallets?"

**Expected Plan:**

[TIME: ~45s] [COST: ~0.004 SOL] [CONFIDENCE: 94%]

**Available Tools:**
From Standard Library:
  - getTokenLargestAccounts (Solana RPC)
  - CALCULATE_GINI_COEFFICIENT (Statistical)
  - IDENTIFY_KNOWN_WALLETS (Analysis)

**Main Branch:**
$token_mint = "J1toso1uCk3RLmjorhTtrVwY9HJ7X8V9yYac6Y7kGCPn"

// Get the top 1000 holders
$holders = getTokenLargestAccounts(mint: $token_mint, limit: 1000)

// Identify which of these are known protocol/CEX wallets
$identified_holders = IDENTIFY_KNOWN_WALLETS(wallets: MAP($holders, h => h.address))

// Filter out known wallets to analyze the distribution among individuals/unknowns
$unknown_holders = FILTER($holders, h => !$identified_holders[h.address].is_known)

// Calculate concentration metrics for the unknown holders
$gini_coefficient = CALCULATE_GINI_COEFFICIENT(holders: $unknown_holders)
$top_10_unknown_pct = SUM(MAP(SLICE($unknown_holders, 0, 10), h => h.percentage))

**Decision Point:** Evaluate the holder concentration for red flags.
  BRANCH A ($top_10_unknown_pct > 50):
    $flag = "RED FLAG"
    $assessment = "Holder distribution is dangerously concentrated. The top 10 unknown wallets control over 50% of the supply outside of protocols, giving them immense power over the market."
  BRANCH B ($top_10_unknown_pct > 20):
    $flag = "YELLOW FLAG"
    $assessment = "Holder distribution is somewhat concentrated. While not critical, the top 10 unknown wallets have significant influence."
  BRANCH C (default):
    $flag = "GREEN FLAG"
    $assessment = "Holder distribution appears healthy and decentralized among individual holders."

**Action:**
RETURN {
  red_flag_status: $flag,
  assessment: $assessment,
  gini_coefficient_unknowns: $gini_coefficient,
  top_10_unknown_holders_own_pct: $top_10_unknown_pct,
  top_10_unknown_wallets: SLICE($unknown_holders, 0, 10),
  confidence: 94
}

---

## Q309: "Based on the vesting schedule, what is the projected inflation rate and increase in circulating supply for the 'Tensor' (TNSR) token over the next 12 months?"

**Expected Plan:**

[TIME: ~30s] [COST: ~0.001 SOL] [CONFIDENCE: 96%]

**Available Tools:**
From Standard Library:
  - SEARCH_DOCS (Web/External)
  - PARSE_VESTING_SCHEDULE (Analysis)
  - PROJECT_SUPPLY_INFLATION (Analysis)

**Main Branch:**
$token_symbol = "TNSR"

// 1. Find the official documentation for the vesting schedule
$docs = SEARCH_DOCS(query: "Tensor TNSR token vesting schedule official")
$vesting_schedule = PARSE_VESTING_SCHEDULE(docs: $docs)

// 2. Project the supply unlocks over the next 12 months
$inflation_projection = PROJECT_SUPPLY_INFLATION(
  schedule: $vesting_schedule,
  time_horizon_months: 12
)

**Decision Point:** Characterize the upcoming inflation.
  BRANCH A ($inflation_projection.next_12m_increase_pct > 100):
    $impact = "High Inflation"
    $summary = "The circulating supply is projected to more than double in the next 12 months, which could create significant sell pressure."
  BRANCH B ($inflation_projection.next_12m_increase_pct > 40):
    $impact = "Moderate Inflation"
    $summary = "A significant amount of new supply will enter circulation over the next year. Key unlock dates may cause volatility."
  BRANCH C (default):
    $impact = "Low Inflation"
    $summary = "Most of the supply is already circulating. Upcoming unlocks are minor and unlikely to have a major market impact."

**Action:**
RETURN {
  inflation_impact: $impact,
  summary: $summary,
  current_circulating_supply: $vesting_schedule.current_supply,
  projected_supply_in_12m: $inflation_projection.final_supply,
  circulating_supply_increase_pct: $inflation_projection.next_12m_increase_pct,
  major_unlock_dates: $inflation_projection.unlock_dates,
  confidence: 96
}

---

## Q310: "How does the ratio of daily active users to market cap for 'Helium Mobile' (MOBILE) compare to other DePIN projects on Solana?"

**Expected Plan:**

[TIME: ~70s] [COST: ~0.008 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - GET_DAILY_ACTIVE_USERS (Analysis)
  - BIRDEYE_GET_MARKET_CAP (Birdeye API)
  - MAP, SORT_BY (Data Processing)

**Main Branch:**
$target_project = "MOBILE"
$competitors = ["HNT", "RENDER", "NOS"] // Other DePIN projects

// Get DAUs and Market Cap for the target project
$mobile_dau = GET_DAILY_ACTIVE_USERS(project: $target_project)
$mobile_mc = BIRDEYE_GET_MARKET_CAP(token_symbol: $target_project)
$mobile_ratio = $mobile_mc / $mobile_dau

// Get DAUs and Market Cap for competitors
$competitor_ratios = MAP(collection: $competitors, fn: p => {
  project: p,
  dau: GET_DAILY_ACTIVE_USERS(project: p),
  mc: BIRDEYE_GET_MARKET_CAP(token_symbol: p),
  ratio: mc / dau
})

$all_projects = APPEND(array: $competitor_ratios, item: { project: $target_project, ratio: $mobile_ratio })
$sorted_projects = SORT_BY(collection: $all_projects, field: "ratio", direction: "asc") // Lower is better

**Decision Point:** Evaluate MOBILE's valuation relative to its user base.
  BRANCH A (FIRST($sorted_projects).project == $target_project):
    $valuation = "Potentially Undervalued"
    $conclusion = "Helium Mobile has the lowest Market Cap to Daily Active User ratio, suggesting it is valued more cheaply per user compared to its peers."
  BRANCH B (LAST($sorted_projects).project == $target_project):
    $valuation = "Potentially Overvalued"
    $conclusion = "Helium Mobile has the highest Market Cap to Daily Active User ratio, suggesting it carries a premium valuation per user compared to its peers."
  BRANCH C (default):
    $valuation = "Fairly Valued"
    $conclusion = "Helium Mobile's valuation ratio is in the middle of the pack compared to its peers."

**Action:**
RETURN {
  valuation_assessment: $valuation,
  conclusion: $conclusion,
  mobile_mc_per_dau: $mobile_ratio,
  peer_comparison: $sorted_projects,
  confidence: 88
}

---

## Q311: "Map a 'crypto laundering service' that accepts dirty funds and returns 'clean' crypto for a fee. Track all clients, calculate service volume, and find operator wallets."

**Expected Plan:**

[TIME: ~14m] [COST: ~0.14 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library:
  - DETECT_LAUNDERING_SERVICE (Advanced Forensics)
  - BUILD_SERVICE_CLIENT_MAP (Analysis)

**Main Branch:**
$suspected_service_wallet = "..."

// Detect laundering service pattern
$service_analysis = DETECT_LAUNDERING_SERVICE(
  suspected_wallet: $suspected_service_wallet,
  time_period: "180d"
)

// Build client map
$clients = BUILD_SERVICE_CLIENT_MAP(
  service_wallet: $suspected_service_wallet,
  identify_dirty_sources: true
)

// Calculate service metrics
$total_volume = SUM(MAP($clients, c => c.total_sent_sol))
$estimated_fee = $total_volume * 0.05 // Assume 5% service fee

**Action:**
RETURN {
  investigation: "Crypto Laundering Service",
  service_wallet: $suspected_service_wallet,
  total_clients: COUNT($clients),
  total_volume_sol: $total_volume,
  estimated_fees_sol: $estimated_fee,
  operator_wallets: $service_analysis.operator_cluster,
  confidence: 81
}

---

## Q312: "Detect 'insider trading' on token launches by identifying wallets that buy large amounts seconds after deployment but before public announcement."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - FIND_STEALTH_LAUNCH_INSIDERS (Forensics)

**Main Branch:**
$token_mint = "..."
$public_announcement_time = 1728432000

$launch_time = getAccountInfo($token_mint).data.created_at

// Find buyers between deployment and announcement
$insider_buyers = FIND_STEALTH_LAUNCH_INSIDERS(
  token: $token_mint,
  launch_time: $launch_time,
  announcement_time: $public_announcement_time,
  min_buy_amount_sol: 10
)

**Action:**
RETURN {
  investigation: "Insider Trading Detection",
  token: $token_mint,
  stealth_period_minutes: ($public_announcement_time - $launch_time) / 60,
  insider_buyers: COUNT($insider_buyers),
  total_insider_volume_sol: SUM(MAP($insider_buyers, i => i.buy_amount_sol)),
  insider_wallets: MAP($insider_buyers, i => i.wallet),
  confidence: 86
}

---

## Q313: "Identify 'MEV protection failures' by finding transactions that paid for MEV protection but still got sandwiched. Calculate wasted protection fees."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 84%]

**Available Tools:**
From Standard Library:
  - FIND_FAILED_MEV_PROTECTION (MEV Analysis)

**Main Branch:**
$time_period = "30d"

$failed_protections = FIND_FAILED_MEV_PROTECTION(
  period: $time_period,
  protection_services: ["Jito", "Flashbots", "Eden"]
)

$wasted_fees = SUM(MAP($failed_protections, f => f.protection_fee_sol))

**Action:**
RETURN {
  investigation: "MEV Protection Failures",
  failed_protections: COUNT($failed_protections),
  total_wasted_fees_sol: $wasted_fees,
  most_common_failure_reason: MOST_COMMON(MAP($failed_protections, f => f.failure_reason)),
  confidence: 84
}

---

## Q314: "Map a 'Ponzi token' with referral rewards. Build the referral tree, calculate each level's profitability, and identify the collapse point."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library:
  - BUILD_REFERRAL_TREE (Analysis)
  - CALCULATE_PONZI_COLLAPSE_POINT (Financial Analysis)

**Main Branch:**
$ponzi_token = "..."

$referral_tree = BUILD_REFERRAL_TREE(
  token: $ponzi_token,
  root_wallet: getAccountInfo($ponzi_token).data.authority
)

$collapse_point = CALCULATE_PONZI_COLLAPSE_POINT(
  tree: $referral_tree,
  promised_returns: 10 // 10% monthly
)

**Action:**
RETURN {
  investigation: "Ponzi Token Referral Analysis",
  token: $ponzi_token,
  total_participants: COUNT($referral_tree.all_participants),
  pyramid_levels: COUNT($referral_tree.levels),
  collapse_date: $collapse_point.estimated_collapse_date,
  winners: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit > 0)),
  losers: COUNT(FILTER($referral_tree.all_participants, p => p.net_profit < 0)),
  confidence: 87
}

---

## Q315: "Detect 'fake volume bots' by analyzing trading patterns: repetitive amounts, perfect timing intervals, circular fund flows between limited wallets."

**Expected Plan:**

[TIME: ~9m] [COST: ~0.09 SOL] [CONFIDENCE: 88%]

**Available Tools:**
From Standard Library:
  - DETECT_FAKE_VOLUME_BOTS (Pattern Analysis)

**Main Branch:**
$token_mint = "..."

$fake_volume = DETECT_FAKE_VOLUME_BOTS(
  token: $token_mint,
  detection_methods: ["repetitive_amounts", "timing_regularity", "circular_flows"]
)

$real_volume = GET_TOKEN_VOLUME(token: $token_mint, period: "24h") - $fake_volume.estimated_fake_volume_sol
$fake_percentage = ($fake_volume.estimated_fake_volume_sol / GET_TOKEN_VOLUME(token: $token_mint, period: "24h")) * 100

**Action:**
RETURN {
  investigation: "Fake Volume Bot Detection",
  token: $token_mint,
  fake_volume_bots: COUNT($fake_volume.detected_bots),
  estimated_fake_volume_sol: $fake_volume.estimated_fake_volume_sol,
  estimated_real_volume_sol: $real_volume,
  fake_volume_percentage: $fake_percentage,
  bot_wallets: $fake_volume.bot_wallets,
  confidence: 88
}

---

## Q316: "Investigate a 'liquidity migration scam' where devs drain LP from one DEX, promise to migrate to another, but never do. Track fund flow."

**Expected Plan:**

[TIME: ~8m] [COST: ~0.08 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library:
  - TRACE_LIQUIDITY_REMOVAL (Analysis)

**Main Branch:**
$token_mint = "..."
$old_pool = "..." // Raydium pool
$promised_new_pool = "..." // Orca pool (never created)

$lp_removal = TRACE_LIQUIDITY_REMOVAL(
  token: $token_mint,
  pool: $old_pool
)

// Check if new pool was ever created
$new_pool_exists = getAccountInfo($promised_new_pool) != null

// Trace removed funds
$fund_trace = TRACE_FUNDS(
  start: $lp_removal.remover_wallet,
  depth: 5
)

**Action:**
RETURN {
  investigation: "Liquidity Migration Scam",
  token: $token_mint,
  old_pool: $old_pool,
  lp_removed_sol: $lp_removal.total_removed_sol,
  new_pool_created: $new_pool_exists,
  removed_funds_destination: $fund_trace.final_addresses,
  is_confirmed_scam: !$new_pool_exists,
  confidence: 85
}

---

## Q317: "Map 'connected scammer networks' by finding common funding sources, shared infrastructure wallets, or coordinated launch timing across multiple rugs."

**Expected Plan:**

[TIME: ~13m] [COST: ~0.13 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library:
  - BUILD_SCAMMER_NETWORK_MAP (Advanced Forensics)

**Main Branch:**
$known_rugs = [...] // List of known rug pulls

$network_map = BUILD_SCAMMER_NETWORK_MAP(
  scam_projects: $known_rugs,
  link_types: ["funding_source", "infrastructure_wallets", "launch_timing"]
)

**Action:**
RETURN {
  investigation: "Connected Scammer Networks",
  rugs_analyzed: COUNT($known_rugs),
  distinct_networks: COUNT($network_map.detected_networks),
  largest_network_size: MAX(MAP($network_map.detected_networks, n => COUNT(n.projects))),
  network_details: $network_map.detected_networks,
  confidence: 83
}

---

## Q318: "Detect 'pump and dump discord groups' by correlating coordinated buying with Discord/Telegram message timestamps and member wallets."

**Expected Plan:**

[TIME: ~10m] [COST: ~0.1 SOL] [CONFIDENCE: 79%]

**Available Tools:**
From Standard Library:
  - CORRELATE_SOCIAL_WITH_ONCHAIN (Cross-Platform Analysis)

**Main Branch:**
$suspected_tokens = [...]
$discord_timestamps = [...] // Call timestamps from known pump groups

$correlation = CORRELATE_SOCIAL_WITH_ONCHAIN(
  tokens: $suspected_tokens,
  social_signals: $discord_timestamps,
  correlation_window: 300 // 5 minutes
)

**Action:**
RETURN {
  investigation: "Discord Pump Group Detection",
  tokens_analyzed: COUNT($suspected_tokens),
  strong_correlations: COUNT(FILTER($correlation, c => c.correlation_strength > 0.8)),
  pump_group_members: $correlation.suspected_member_wallets,
  confidence: 79
}

---

## Q319: "Identify 'token lockup bypass exploits' where teams find ways to access supposedly locked tokens early. Trace the exploit path."

**Expected Plan:**

[TIME: ~7m] [COST: ~0.07 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library:
  - DETECT_LOCKUP_BYPASS (Security Analysis)

**Main Branch:**
$token_mint = "..."
$lockup_contract = "..."

$bypass_detection = DETECT_LOCKUP_BYPASS(
  token: $token_mint,
  lockup_contract: $lockup_contract
)

**Action:**
RETURN {
  investigation: "Token Lockup Bypass Detection",
  token: $token_mint,
  bypass_detected: $bypass_detection.bypass_found,
  bypass_method: $bypass_detection.exploit_method,
  tokens_unlocked_early: $bypass_detection.early_unlock_amount,
  bypass_transactions: $bypass_detection.exploit_transactions,
  confidence: 86
}

---

## Q320: "Map 'validator collusion' where multiple validators coordinate to censor specific transactions or prioritize their own MEV extraction."

**Expected Plan:**

[TIME: ~12m] [COST: ~0.12 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - DETECT_VALIDATOR_COLLUSION (Advanced Forensics)

**Main Branch:**
$suspected_validators = [...]
$time_period = {start: 1727222400, end: 1728432000}

$collusion_analysis = DETECT_VALIDATOR_COLLUSION(
  validators: $suspected_validators,
  period: $time_period,
  detection_methods: ["censorship_patterns", "coordinated_mev", "shared_infrastructure"]
)

**Action:**
RETURN {
  investigation: "Validator Collusion Detection",
  validators_analyzed: COUNT($suspected_validators),
  collusion_detected: $collusion_analysis.collusion_found,
  colluding_validators: $collusion_analysis.colluding_group,
  evidence: $collusion_analysis.evidence,
  estimated_unfair_mev_sol: $collusion_analysis.total_coordinated_mev_sol,
  confidence: 82
}


---

## Q321: "Detect 'spoofed LP tokens' where scammers create fake LP tokens that look identical to real ones to trick users into depositing value."

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

## Q322: "Map a 'DAO treasury raid' where malicious proposal drains funds. Trace the proposal creation, voting patterns, and fund destinations."

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

## Q323: "Identify 'circular trading bots' that create artificial volume by trading between their own wallets with no real market impact."

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

## Q324: "Investigate a 'bridge exploit' where attacker mints unlimited tokens by exploiting cross-chain message verification."

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

## Q325: "Detect 'governance token rental markets' where users rent voting power, potentially enabling vote manipulation."

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

## Q326: "Map a 'MEV sandwich cartel' where multiple bots coordinate to avoid competing on the same victim transactions."

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

## Q327: "Investigate 'NFT metadata poisoning' where scammers inject malicious URLs into NFT metadata to phish holders."

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

## Q328: "Detect 'stablecoin depeg manipulation' where whales intentionally cause depegs to profit from panic selling."

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

## Q329: "Map 'validator sandwich attack coordination' where validators collude to sandwich user transactions in their own blocks."

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

## Q330: "Investigate 'pump-and-dump as a service' platforms that coordinate paid pump campaigns for tokens."

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

## Q331: "Detect 'fake liquidity lock' where scammers lock liquidity in a contract they control and can withdraw anytime."

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

## Q332: "Map 'cNFT minting exploits' where attackers mint compressed NFTs beyond the tree capacity by exploiting Merkle tree verification."

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

## Q333: "Investigate 'prediction market oracle manipulation' where insiders with early knowledge manipulate market odds before public announcement."

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

## Q334: "Detect 'token supply inflation' attacks where minter role is exploited to create unlimited tokens and dump on market."

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

## Q335: "Map a 'liquidation bot frontrunning cartel' where bots coordinate to avoid competing on the same liquidation opportunities."

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

## Q336: "Investigate 'fake airdrop claim' scams that require users to approve unlimited token spending to claim worthless tokens."

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

## Q337: "Detect 'validator censorship' where specific validators consistently exclude certain transactions or protocols from their blocks."

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

## Q338: "Map 'cross-DEX arbitrage bot networks' that share infrastructure and coordinate to reduce gas competition."

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

## Q339: "Investigate 'mempool frontrunning on Solana' by analyzing Jito bundle patterns and identifying systematic frontrunners."

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

## Q340: "Detect 'rug pull via upgrade authority' where devs use program upgrade authority to inject malicious code after launch."

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

## Q341: "Perform a Monte Carlo simulation to predict the 30-day price distribution for a volatile token."

**Expected Plan:**
[TIME: ~120s] [COST: ~0.02 SOL] [CONFIDENCE: 80%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, CALCULATE_VOLATILITY, MONTE_CARLO_SIMULATION

**Main Branch:**
$token_mint = "..."

$historical_prices = GET_PRICE_HISTORY(token: $token_mint, period: "90d")
$volatility = CALCULATE_VOLATILITY(data: $historical_prices)
$current_price = $historical_prices.last()

$simulation_results = MONTE_CARLO_SIMULATION(
  starting_price: $current_price,
  volatility: $volatility,
  days: 30,
  simulations: 10000
)

$price_percentiles = {
  p5: PERCENTILE($simulation_results.final_prices, 5),
  p25: PERCENTILE($simulation_results.final_prices, 25),
  p50: PERCENTILE($simulation_results.final_prices, 50),
  p75: PERCENTILE($simulation_results.final_prices, 75),
  p95: PERCENTILE($simulation_results.final_prices, 95)
}

**Action:**
RETURN {
  analysis: "30-Day Price Distribution (Monte Carlo)",
  current_price: $current_price,
  expected_price_p50: $price_percentiles.p50,
  confidence_interval_80pct: [$price_percentiles.p10, $price_percentiles.p90],
  worst_case_p5: $price_percentiles.p5,
  best_case_p95: $price_percentiles.p95,
  confidence: 80
}

---

## Q342: "Analyze the options/perpetuals open interest to gauge trader sentiment."

**Expected Plan:**
[TIME: ~55s] [COST: ~0.007 SOL] [CONFIDENCE: 85%]

**Available Tools:**
From Standard Library: GET_OPTIONS_OI, GET_PERPS_OI

**Main Branch:**
$token_symbol = "SOL"

$options_oi = GET_OPTIONS_OI(token: $token_symbol)
$perps_oi = GET_PERPS_OI(token: $token_symbol)

$call_put_ratio = $options_oi.call_oi / $options_oi.put_oi
$long_short_ratio = $perps_oi.long_oi / $perps_oi.short_oi

**Decision Point:** Interpret sentiment
  BRANCH A ($call_put_ratio > 2 && $long_short_ratio > 1.5):
    $sentiment = "Strongly Bullish"
  BRANCH B ($call_put_ratio < 0.5 && $long_short_ratio < 0.67):
    $sentiment = "Strongly Bearish"
  BRANCH C (default):
    $sentiment = "Neutral/Mixed"

**Action:**
RETURN {
  analysis: "Derivatives Market Sentiment",
  token: $token_symbol,
  call_put_ratio: $call_put_ratio,
  long_short_ratio: $long_short_ratio,
  sentiment: $sentiment,
  confidence: 85
}

---

## Q343: "Calculate the Sharpe ratio for a token to evaluate risk-adjusted returns."

**Expected Plan:**
[TIME: ~45s] [COST: ~0.005 SOL] [CONFIDENCE: 92%]

**Available Tools:**
From Standard Library: GET_PRICE_HISTORY, CALCULATE_SHARPE_RATIO

**Main Branch:**
$token_mint = "..."
$risk_free_rate = 0.04  // 4% annual risk-free rate

$price_history = GET_PRICE_HISTORY(token: $token_mint, period: "365d", interval: "1d")

$sharpe_ratio = CALCULATE_SHARPE_RATIO(
  returns: $price_history,
  risk_free_rate: $risk_free_rate
)

**Decision Point:** Interpret Sharpe ratio
  BRANCH A ($sharpe_ratio > 2):
    $rating = "Excellent risk-adjusted returns"
  BRANCH B ($sharpe_ratio > 1):
    $rating = "Good risk-adjusted returns"
  BRANCH C ($sharpe_ratio > 0):
    $rating = "Positive but below-average returns"
  BRANCH D (default):
    $rating = "Poor risk-adjusted returns"

**Action:**
RETURN {
  analysis: "Risk-Adjusted Returns (Sharpe Ratio)",
  token: $token_mint,
  sharpe_ratio: $sharpe_ratio,
  rating: $rating,
  confidence: 92
}

---

## Q344: "Identify correlation clusters among similar-category tokens (e.g., all gaming tokens)."

**Expected Plan:**
[TIME: ~85s] [COST: ~0.014 SOL] [CONFIDENCE: 83%]

**Available Tools:**
From Standard Library: GET_TOKENS_BY_CATEGORY, GET_PRICE_HISTORY, BUILD_CORRELATION_MATRIX

**Main Branch:**
$category = "gaming"
$tokens = GET_TOKENS_BY_CATEGORY(category: $category, min_mcap: 1000000)

$price_data = MAP($tokens, token => {
  token_id: token.mint,
  prices: GET_PRICE_HISTORY(token: token.mint, period: "30d")
})

$correlation_matrix = BUILD_CORRELATION_MATRIX(data: $price_data)

$high_correlation_pairs = FILTER($correlation_matrix, pair => pair.correlation > 0.75)

**Action:**
RETURN {
  analysis: "Token Correlation Clusters",
  category: $category,
  tokens_analyzed: COUNT($tokens),
  high_correlation_count: COUNT($high_correlation_pairs),
  strongest_pair: MAX_BY($high_correlation_pairs, p => p.correlation),
  confidence: 83
}

---

## Q345: "Analyze whale accumulation patterns by tracking large buy orders over time."

**Expected Plan:**
[TIME: ~70s] [COST: ~0.011 SOL] [CONFIDENCE: 86%]

**Available Tools:**
From Standard Library: GET_LARGE_TRANSACTIONS, DETECT_ACCUMULATION_PATTERN

**Main Branch:**
$token_mint = "..."
$whale_threshold = 50000  // USD

$large_buys = GET_LARGE_TRANSACTIONS(
  token: $token_mint,
  type: "buy",
  min_size_usd: $whale_threshold,
  period: "30d"
)

$accumulation_pattern = DETECT_ACCUMULATION_PATTERN(transactions: $large_buys)

$daily_whale_volume = GROUP_BY($large_buys, tx => tx.date)

**Action:**
RETURN {
  analysis: "Whale Accumulation Patterns",
  token: $token_mint,
  whale_buy_count_30d: COUNT($large_buys),
  total_whale_volume_usd: SUM(MAP($large_buys, tx => tx.size_usd)),
  accumulation_trend: $accumulation_pattern.trend,
  avg_daily_whale_buys: COUNT($large_buys) / 30,
  confidence: 86
}

---

## Q346: "Calculate the funding rate arbitrage opportunity between spot and perpetual futures."

**Expected Plan:**
[TIME: ~50s] [COST: ~0.006 SOL] [CONFIDENCE: 89%]

**Available Tools:**
From Standard Library: GET_SPOT_PRICE, GET_PERP_PRICE, GET_FUNDING_RATE

**Main Branch:**
$token_symbol = "SOL"

$spot_price = GET_SPOT_PRICE(token: $token_symbol, exchange: "Raydium")
$perp_price = GET_PERP_PRICE(token: $token_symbol, exchange: "Drift")
$funding_rate = GET_FUNDING_RATE(token: $token_symbol, exchange: "Drift")

$basis = (($perp_price - $spot_price) / $spot_price) * 100
$annualized_funding = $funding_rate * 365 * 3  // Funding paid 3x daily

**Decision Point:** Evaluate arbitrage opportunity
  BRANCH A ($annualized_funding > 10 && $basis < 0.5):
    $verdict = "Strong cash-and-carry arbitrage opportunity"
    $expected_apy = $annualized_funding
  BRANCH B ($annualized_funding < -10 && $basis > -0.5):
    $verdict = "Reverse cash-and-carry opportunity"
    $expected_apy = ABS($annualized_funding)
  BRANCH C (default):
    $verdict = "No significant arbitrage opportunity"
    $expected_apy = 0

**Action:**
RETURN {
  analysis: "Funding Rate Arbitrage Analysis",
  token: $token_symbol,
  funding_rate_pct: $funding_rate * 100,
  annualized_apy: $expected_apy,
  verdict: $verdict,
  confidence: 89
}

---

## Q347: "Analyze the token's circulating supply inflation rate and its impact on price."

**Expected Plan:**
[TIME: ~60s] [COST: ~0.008 SOL] [CONFIDENCE: 87%]

**Available Tools:**
From Standard Library: GET_SUPPLY_HISTORY, GET_PRICE_HISTORY, CORRELATE

**Main Branch:**
$token_mint = "..."

$supply_history = GET_SUPPLY_HISTORY(token: $token_mint, period: "180d")
$price_history = GET_PRICE_HISTORY(token: $token_mint, period: "180d")

$daily_inflation_rates = []
FOR $i IN 1..COUNT($supply_history)-1:
  $inflation = (($supply_history[$i] - $supply_history[$i-1]) / $supply_history[$i-1]) * 100
  $daily_inflation_rates = APPEND($daily_inflation_rates, $inflation)

$avg_daily_inflation = AVG($daily_inflation_rates)
$annualized_inflation = $avg_daily_inflation * 365

$inflation_price_correlation = CORRELATE($daily_inflation_rates, $price_history[1:])

**Action:**
RETURN {
  analysis: "Supply Inflation Impact Analysis",
  token: $token_mint,
  avg_daily_inflation_pct: $avg_daily_inflation,
  annualized_inflation_pct: $annualized_inflation,
  inflation_price_correlation: $inflation_price_correlation.coefficient,
  impact: $inflation_price_correlation.coefficient < -0.5 ? "Strong negative impact" : "Limited impact",
  confidence: 87
}

---

## Q348: "Detect unusual trading volume spikes that precede major price movements."

**Expected Plan:**
[TIME: ~65s] [COST: ~0.009 SOL] [CONFIDENCE: 81%]

**Available Tools:**
From Standard Library: GET_VOLUME_HISTORY, GET_PRICE_HISTORY, DETECT_ANOMALIES

**Main Branch:**
$token_mint = "..."

$volume_data = GET_VOLUME_HISTORY(token: $token_mint, period: "90d", interval: "1h")
$price_data = GET_PRICE_HISTORY(token: $token_mint, period: "90d", interval: "1h")

$volume_spikes = DETECT_ANOMALIES(data: $volume_data, threshold: 3)  // 3 std devs

$spike_followed_by_move = FILTER($volume_spikes, spike => {
  $spike_time = spike.timestamp
  $price_before = FIND($price_data, p => p.timestamp == $spike_time).price
  $price_24h_later = FIND($price_data, p => p.timestamp == $spike_time + 86400).price
  $price_change_pct = ABS((($price_24h_later - $price_before) / $price_before) * 100)
  
  RETURN $price_change_pct > 5  // 5% move within 24h
})

$predictive_accuracy = (COUNT($spike_followed_by_move) / COUNT($volume_spikes)) * 100

**Action:**
RETURN {
  analysis: "Volume Spike Price Prediction",
  token: $token_mint,
  total_volume_spikes: COUNT($volume_spikes),
  spikes_followed_by_move: COUNT($spike_followed_by_move),
  predictive_accuracy_pct: $predictive_accuracy,
  confidence: 81
}

---

## Q349: "Calculate the token's fair value using discounted cash flow (DCF) for yield-bearing tokens."

**Expected Plan:**
[TIME: ~55s] [COST: ~0.007 SOL] [CONFIDENCE: 75%]

**Available Tools:**
From Standard Library: GET_YIELD_DATA, CALCULATE_DCF

**Main Branch:**
$token_mint = "..."  // Yield-bearing token like stSOL, mSOL

$yield_data = GET_YIELD_DATA(token: $token_mint)
$current_apy = $yield_data.apy
$current_price = GET_CURRENT_PRICE($token_mint)

$discount_rate = 0.08  // 8% discount rate
$growth_rate = 0.02    // 2% perpetual growth

$expected_annual_yield = $current_price * $current_apy
$fair_value = $expected_annual_yield / ($discount_rate - $growth_rate)

$value_vs_price = (($fair_value - $current_price) / $current_price) * 100

**Decision Point:** Valuation assessment
  BRANCH A ($value_vs_price > 20):
    $verdict = "Significantly undervalued"
  BRANCH B ($value_vs_price > 10):
    $verdict = "Moderately undervalued"
  BRANCH C ($value_vs_price < -10):
    $verdict = "Overvalued"
  BRANCH D (default):
    $verdict = "Fairly valued"

**Action:**
RETURN {
  analysis: "DCF Fair Value Analysis (Yield Token)",
  token: $token_mint,
  current_price: $current_price,
  fair_value: $fair_value,
  upside_downside_pct: $value_vs_price,
  verdict: $verdict,
  confidence: 75
}

---

## Q350: "Analyze cross-chain bridge flows to detect capital rotation patterns."

**Expected Plan:**
[TIME: ~75s] [COST: ~0.012 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library: GET_BRIDGE_TRANSACTIONS, ANALYZE_CAPITAL_FLOWS

**Main Branch:**
$token_symbol = "USDC"

$bridge_flows = GET_BRIDGE_TRANSACTIONS(
  token: $token_symbol,
  period: "30d"
)

$inbound_flows = FILTER($bridge_flows, tx => tx.direction == "to_solana")
$outbound_flows = FILTER($bridge_flows, tx => tx.direction == "from_solana")

$net_inflow = SUM(MAP($inbound_flows, tx => tx.amount_usd)) - SUM(MAP($outbound_flows, tx => tx.amount_usd))

$source_chains = GROUP_BY($inbound_flows, tx => tx.source_chain)
$dest_chains = GROUP_BY($outbound_flows, tx => tx.dest_chain)

**Action:**
RETURN {
  analysis: "Cross-Chain Capital Flows",
  token: $token_symbol,
  net_inflow_30d_usd: $net_inflow,
  inbound_count: COUNT($inbound_flows),
  outbound_count: COUNT($outbound_flows),
  top_source_chains: TOP_N($source_chains, 3),
  top_dest_chains: TOP_N($dest_chains, 3),
  flow_direction: $net_inflow > 0 ? "Capital inflowing to Solana" : "Capital outflowing from Solana",
  confidence: 82
}
