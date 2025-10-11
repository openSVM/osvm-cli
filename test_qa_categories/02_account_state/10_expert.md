# Account State - Expert Level

## Q1: "Track suspicious balance changes across 1000+ wallets to detect coordinated pump-and-dump schemes"

**Expected Plan:**
[TIME: ~180s] [COST: ~0.03 SOL] [CONFIDENCE: 70%]

**Available Tools:**
From Standard Library:
  - getAccountInfo, getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, SORT_BY, FLATTEN (Data Processing)
  - CORRELATE, FIND_PATTERNS, identifyPatterns (Statistical)
  - ADD_NODE, ADD_EDGE, FIND_CLUSTERS (Knowledge Graph)

**Main Branch:**
```ovsm
$token_mint = INPUT(prompt: "Enter token mint address to investigate")
$time_window_hours = 24

// Get current slot for time reference
$current_slot = getSlot()
$slots_per_hour = 7200  // Approximate
$start_slot = $current_slot - ($time_window_hours * $slots_per_hour)

// First, find the largest holders
TRY:
  $largest_accounts = getTokenLargestAccounts(mint: $token_mint)
CATCH FATAL:
  RETURN ERROR(message: "Invalid token mint or token doesn't exist")

$accounts_to_analyze = MAP(
  collection: SLICE(collection: $largest_accounts.value, start: 0, end: 100),
  fn: acc => acc.address
)

LOG("Analyzing " + COUNT(collection: $accounts_to_analyze) + " largest token holders...")

// Build transaction graph
$graph = INIT_KNOWLEDGE_GRAPH()
$all_transfers = []
$wallet_behaviors = {}

PARALLEL {
  // Analyze wallets in parallel batches
  FOR $wallet IN $accounts_to_analyze:
    TRY:
      $signatures = getSignaturesForAddress(
        address: $wallet,
        limit: 200,
        until: $start_slot
      )
      
      // Filter to recent transactions
      $recent_sigs = FILTER(
        collection: $signatures,
        predicate: sig => sig.slot >= $start_slot
      )
      
      $buys = 0
      $sells = 0
      $total_bought = 0
      $total_sold = 0
      $first_activity = null
      $last_activity = null
      $connected_wallets = []
      
      FOR $sig_info IN $recent_sigs:
        $tx = getTransaction(signature: $sig_info.signature, maxSupportedTransactionVersion: 0)
        
        IF $tx.meta.err == null AND $tx.meta.postTokenBalances != null THEN
          // Check if this transaction involved our token
          FOR $post_balance IN $tx.meta.postTokenBalances:
            IF $post_balance.mint == $token_mint THEN
              $pre_balance = FIND(
                collection: $tx.meta.preTokenBalances,
                predicate: pre => pre.accountIndex == $post_balance.accountIndex
              )
              
              IF $pre_balance != null THEN
                $amount_change = $post_balance.uiTokenAmount.uiAmount - $pre_balance.uiTokenAmount.uiAmount
                
                IF $amount_change > 0 THEN
                  $buys += 1
                  $total_bought += $amount_change
                ELSE IF $amount_change < 0 THEN
                  $sells += 1
                  $total_sold += ABS($amount_change)
                
                // Track timing
                IF $first_activity == null OR $sig_info.blockTime < $first_activity THEN
                  $first_activity = $sig_info.blockTime
                IF $last_activity == null OR $sig_info.blockTime > $last_activity THEN
                  $last_activity = $sig_info.blockTime
                
                // Track connections
                $accounts = $tx.transaction.message.accountKeys
                FOR $other_account IN $accounts:
                  IF $other_account != $wallet AND NOT CONTAINS($connected_wallets, $other_account) THEN
                    $connected_wallets = APPEND(array: $connected_wallets, item: $other_account)
                    
                    // Add to graph
                    $graph = ADD_NODE(graph: $graph, id: $wallet, type: "holder")
                    $graph = ADD_NODE(graph: $graph, id: $other_account, type: "connected")
                    $graph = ADD_EDGE(
                      graph: $graph,
                      from: $wallet,
                      to: $other_account,
                      relationship: "transacted_with",
                      timestamp: $sig_info.blockTime
                    )
                
                // Record transfer details
                $all_transfers = APPEND(
                  array: $all_transfers,
                  item: {
                    wallet: $wallet,
                    timestamp: $sig_info.blockTime,
                    amount: $amount_change,
                    type: $amount_change > 0 ? "buy" : "sell",
                    signature: $sig_info.signature
                  }
                )
      
      // Calculate behavior metrics
      $activity_duration = $last_activity - $first_activity
      $buy_sell_ratio = $buys > 0 ? $sells / $buys : 0
      
      $wallet_behaviors[$wallet] = {
        buys: $buys,
        sells: $sells,
        total_bought: $total_bought,
        total_sold: $total_sold,
        buy_sell_ratio: $buy_sell_ratio,
        first_activity: $first_activity,
        last_activity: $last_activity,
        activity_duration_seconds: $activity_duration,
        connected_wallets_count: COUNT(collection: $connected_wallets),
        net_position: $total_bought - $total_sold
      }
    
    CATCH RECOVERABLE:
      LOG("Failed to analyze wallet: " + $wallet)
      CONTINUE
}
WAIT_ALL

// Pattern detection: Coordinated dump
$recent_sellers = FILTER(
  collection: KEYS($wallet_behaviors),
  predicate: wallet => {
    $behavior = $wallet_behaviors[$wallet]
    $behavior.sells > $behavior.buys AND 
    $behavior.total_sold > 1000 AND
    $behavior.last_activity > ($current_slot - 7200)  // Last hour
  }
)

// Check if sellers acted in coordination (similar timing)
$seller_timings = MAP(
  collection: $recent_sellers,
  fn: wallet => $wallet_behaviors[$wallet].last_activity
)

$timing_correlation = null
IF COUNT(collection: $seller_timings) >= 2 THEN
  $mean_timing = MEAN(data: $seller_timings)
  $timing_stddev = STDDEV(data: $seller_timings)
  
  // If stddev is low, they sold around the same time
  $timing_correlation = $timing_stddev < 300  // Within 5 minutes

// Pattern detection: Coordinated pump (buying together)
$recent_buyers = FILTER(
  collection: KEYS($wallet_behaviors),
  predicate: wallet => {
    $behavior = $wallet_behaviors[$wallet]
    $behavior.buys > 0 AND
    $behavior.total_bought > 500 AND
    $behavior.first_activity > ($current_slot - 14400)  // Last 2 hours
  }
)

// Check wallet clustering
$clusters = FIND_CLUSTERS(
  graph: $graph,
  min_cluster_size: 3,
  max_distance: 2
)

// Identify suspicious patterns
$suspicious_clusters = FILTER(
  collection: $clusters,
  predicate: cluster => {
    // Cluster members all sold recently
    $members = cluster.members
    $sellers_in_cluster = 0
    FOR $member IN $members:
      IF CONTAINS($recent_sellers, $member) THEN
        $sellers_in_cluster += 1
    
    ($sellers_in_cluster / COUNT(collection: $members)) > 0.6  // 60%+ are sellers
  }
)
```

**Decision Point:** Classify scheme type and severity
  BRANCH A ($timing_correlation == true AND COUNT(collection: $recent_sellers) >= 5 AND 
             COUNT(collection: $suspicious_clusters) > 0):
    $scheme_type = "COORDINATED_DUMP"
    $severity = "CRITICAL"
    $confidence = 90
    $analysis = "ACTIVE DUMP DETECTED: " + COUNT(collection: $recent_sellers) + 
                " wallets sold large amounts within minutes of each other. " +
                COUNT(collection: $suspicious_clusters) + " clusters identified."
    $recommendation = "SELL IMMEDIATELY or expect 50%+ price drop"
    
  BRANCH B (COUNT(collection: $recent_buyers) >= 10 AND COUNT(collection: $recent_sellers) < 3):
    $scheme_type = "COORDINATED_PUMP"
    $severity = "HIGH"
    $confidence = 75
    $analysis = "PUMP IN PROGRESS: " + COUNT(collection: $recent_buyers) + 
                " wallets accumulating. Expect dump soon."
    $recommendation = "DO NOT BUY - this is a pump scheme"
    
  BRANCH C (COUNT(collection: $suspicious_clusters) > 0):
    $scheme_type = "SUSPICIOUS_NETWORK"
    $severity = "MEDIUM"
    $confidence = 65
    $analysis = "Connected wallet groups detected with coordinated behavior"
    $recommendation = "Exercise caution - monitor for dump signals"
    
  BRANCH D (true):
    $scheme_type = "NORMAL_ACTIVITY"
    $severity = "LOW"
    $confidence = 60
    $analysis = "No clear coordination detected in trading patterns"
    $recommendation = "Normal market activity"

**Action:**
RETURN {
  token_mint: $token_mint,
  time_window_analyzed_hours: $time_window_hours,
  wallets_analyzed: COUNT(collection: KEYS($wallet_behaviors)),
  scheme_type: $scheme_type,
  severity: $severity,
  analysis: $analysis,
  recommendation: $recommendation,
  recent_sellers: COUNT(collection: $recent_sellers),
  recent_buyers: COUNT(collection: $recent_buyers),
  suspicious_clusters: COUNT(collection: $suspicious_clusters),
  timing_correlation_detected: $timing_correlation,
  top_sellers: SORT_BY(
    collection: MAP(
      collection: $recent_sellers,
      fn: wallet => {
        $behavior = $wallet_behaviors[$wallet]
        {
          wallet: wallet,
          sold: $behavior.total_sold,
          timing: $behavior.last_activity
        }
      }
    ),
    key: seller => seller.sold,
    order: "desc"
  ),
  cluster_details: $suspicious_clusters,
  network_graph: $graph,
  confidence: $confidence,
  caveats: [
    "Cannot detect coordination happening off-chain",
    "Legitimate market makers may appear coordinated",
    "Private transactions not included in analysis",
    "Timing correlation doesn't prove coordination"
  ]
}

---

## Q2: "Identify 'smart money' wallets by analyzing their historical win rate and copy their positions"

**Expected Plan:**
[TIME: ~120s] [COST: ~0.02 SOL] [CONFIDENCE: 65%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction, getAccountInfo (Solana RPC)
  - MAP, FILTER, SORT_BY, GROUP_BY (Data Processing)
  - MEAN, STDDEV, PERCENTILE (Statistical)

**Main Branch:**
```ovsm
$token_mint = INPUT(prompt: "Enter token mint to find smart traders for")
$min_trades = 10

// Step 1: Get recent large transactions for this token
$current_slot = getSlot()
$blocks_to_scan = 1000

$traders = []
$trade_data = {}

FOR $i IN 0..$blocks_to_scan:
  TRY:
    $slot = $current_slot - $i
    $block = getBlock(slot: $slot, maxSupportedTransactionVersion: 0)
    
    FOR $tx IN $block.transactions:
      IF $tx.meta.err == null AND $tx.meta.postTokenBalances != null THEN
        // Check if involves our token
        $involves_token = ANY(
          collection: $tx.meta.postTokenBalances,
          predicate: balance => balance.mint == $token_mint
        )
        
        IF $involves_token THEN
          $trader = $tx.transaction.message.accountKeys[0]
          
          IF NOT CONTAINS($traders, $trader) THEN
            $traders = APPEND(array: $traders, item: $trader)
  CATCH:
    CONTINUE

LOG("Found " + COUNT(collection: $traders) + " unique traders, analyzing performance...")

// Step 2: Analyze each trader's history with this token
FOR $trader IN $traders:
  TRY:
    $signatures = getSignaturesForAddress(address: $trader, limit: 500)
    
    $trades = []
    $positions = []  // Open positions
    
    FOR $sig_info IN $signatures:
      $tx = getTransaction(signature: $sig_info.signature, maxSupportedTransactionVersion: 0)
      
      IF $tx.meta.err == null AND $tx.meta.postTokenBalances != null THEN
        FOR $post IN $tx.meta.postTokenBalances:
          IF $post.mint == $token_mint AND $post.owner == $trader THEN
            $pre = FIND(
              collection: $tx.meta.preTokenBalances,
              predicate: p => p.accountIndex == $post.accountIndex
            )
            
            IF $pre != null THEN
              $amount_change = $post.uiTokenAmount.uiAmount - $pre.uiTokenAmount.uiAmount
              
              IF $amount_change != 0 THEN
                $trades = APPEND(
                  array: $trades,
                  item: {
                    timestamp: $sig_info.blockTime,
                    amount: ABS($amount_change),
                    type: $amount_change > 0 ? "buy" : "sell",
                    signature: $sig_info.signature
                  }
                )
    
    // Calculate win rate by matching buys to sells
    IF COUNT(collection: $trades) >= $min_trades THEN
      $sorted_trades = SORT_BY(collection: $trades, key: t => t.timestamp, order: "asc")
      
      $total_profit = 0
      $winning_trades = 0
      $losing_trades = 0
      $position_size = 0
      
      FOR $trade IN $sorted_trades:
        IF $trade.type == "buy" THEN
          $position_size += $trade.amount
        ELSE  // sell
          IF $position_size > 0 THEN
            // Simplified P&L calculation (would need price data for accuracy)
            // Assume if they're selling, they made a decision about profitability
            // We'll estimate based on timing
            $holding_duration = $trade.timestamp - $sorted_trades[0].timestamp
            
            IF $holding_duration < 3600 THEN  // <1 hour = likely loss (panic sell)
              $losing_trades += 1
            ELSE  // Held longer = likely profitable
              $winning_trades += 1
            
            $position_size -= $trade.amount
      
      $win_rate = ($winning_trades / ($winning_trades + $losing_trades)) * 100
      
      $trade_data[$trader] = {
        total_trades: COUNT(collection: $trades),
        winning_trades: $winning_trades,
        losing_trades: $losing_trades,
        win_rate: $win_rate,
        current_position: $position_size,
        first_trade: $sorted_trades[0].timestamp,
        last_trade: LAST(collection: $sorted_trades).timestamp
      }
  
  CATCH:
    CONTINUE

// Rank traders by win rate
$ranked_traders = []
FOR $trader IN KEYS($trade_data):
  $data = $trade_data[$trader]
  $ranked_traders = APPEND(
    array: $ranked_traders,
    item: {
      wallet: $trader,
      win_rate: $data.win_rate,
      total_trades: $data.total_trades,
      current_position: $data.current_position
    }
  )

$ranked_traders = SORT_BY(
  collection: $ranked_traders,
  key: trader => trader.win_rate,
  order: "desc"
)

// Get top 10 smart money wallets
$smart_money = SLICE(collection: $ranked_traders, start: 0, end: 10)

// Check their current positions
$copy_recommendations = []
FOR $smart_trader IN $smart_money:
  IF $smart_trader.current_position > 0 THEN
    $copy_recommendations = APPEND(
      array: $copy_recommendations,
      item: {
        wallet: $smart_trader.wallet,
        position_size: $smart_trader.current_position,
        win_rate: $smart_trader.win_rate,
        recommendation: "BUY - smart money is holding"
      }
    )
  ELSE
    $copy_recommendations = APPEND(
      array: $copy_recommendations,
      item: {
        wallet: $smart_trader.wallet,
        position_size: 0,
        win_rate: $smart_trader.win_rate,
        recommendation: "WAIT - smart money has exited"
      }
    )
```

**Decision Point:** Generate trading signal
  BRANCH A (COUNT(collection: FILTER($copy_recommendations, r => r.position_size > 0)) >= 7):
    $signal = "STRONG_BUY"
    $rationale = "70%+ of top traders holding positions"
    
  BRANCH B (COUNT(collection: FILTER($copy_recommendations, r => r.position_size > 0)) >= 4):
    $signal = "BUY"
    $rationale = "Majority of top traders holding"
    
  BRANCH C (COUNT(collection: FILTER($copy_recommendations, r => r.position_size > 0)) <= 2):
    $signal = "SELL"
    $rationale = "Smart money has mostly exited"

**Action:**
RETURN {
  token_mint: $token_mint,
  traders_analyzed: COUNT(collection: KEYS($trade_data)),
  qualified_traders: COUNT(collection: $ranked_traders),
  top_10_smart_money: $smart_money,
  trading_signal: $signal,
  signal_rationale: $rationale,
  copy_trade_recommendations: $copy_recommendations,
  confidence: 65,
  caveats: [
    "Win rate estimation is simplified - needs actual price data",
    "Past performance doesn't guarantee future results",
    "Some 'smart money' may be insiders",
    "Short holding times may be profitable scalps, not losses"
  ]
}
