# NFT Analysis - Forensics Level

## Q1: "Detect NFT wash trading and price manipulation"

**Expected Plan:**

[TIME: ~18s] [COST: ~0.012 SOL] [CONFIDENCE: 82%]

**Available Tools:**
From Standard Library:
  - getSignaturesForAddress, getTransaction (Solana RPC)
  - MAP, FILTER, GROUP_BY, UNIQUE (Data Processing)
  - FIND_PATTERNS, CORRELATE (Statistical)

**Main Branch:**
$nft_mint = INPUT(prompt: "Enter NFT mint address to investigate")

// Get all transfer transactions
$signatures = getSignaturesForAddress(address: $nft_mint, limit: 1000)

// Build transaction graph
$transfers = []
FOR $sig IN $signatures:
  TRY:
    $tx = getTransaction(signature: $sig.signature)
    $transfer_info = extractNFTTransfer(transaction: $tx, mint: $nft_mint)
    
    IF $transfer_info != null:
      $transfers = APPEND(array: $transfers, item: {
        signature: $sig.signature,
        timestamp: $tx.blockTime,
        from: $transfer_info.from,
        to: $transfer_info.to,
        price: $transfer_info.price,
        marketplace: $transfer_info.marketplace
      })
  CATCH RECOVERABLE:
    LOG(level: "debug", message: "Could not parse transaction")

// Identify unique traders
$sellers = UNIQUE(collection: MAP(collection: $transfers, fn: t => t.from))
$buyers = UNIQUE(collection: MAP(collection: $transfers, fn: t => t.to))

// Detect circular trading
$circular_trades = []
FOR $transfer IN $transfers:
  // Check if seller was recent buyer
  $recent_purchases = FILTER(
    collection: $transfers,
    predicate: t => t.to == $transfer.from AND 
                    t.timestamp < $transfer.timestamp AND
                    t.timestamp > ($transfer.timestamp - 86400)  // within 24h
  )
  
  IF COUNT(collection: $recent_purchases) > 0:
    $circular_trades = APPEND(array: $circular_trades, item: $transfer)

// Analyze price progression
$prices = MAP(collection: $transfers, fn: t => t.price)
$price_volatility = STDDEV(data: $prices)
$mean_price = MEAN(data: $prices)

// Check for coordinated activity
$trader_overlap = FILTER(
  collection: $sellers,
  predicate: seller => CONTAINS($buyers, seller)
)

// Calculate wash trading score
$wash_score = (COUNT(collection: $trader_overlap) / COUNT(collection: $sellers)) * 100

**Decision Point:** Assess wash trading likelihood
  BRANCH A ($wash_score > 50 AND COUNT(collection: $circular_trades) > 10):
    $verdict = "confirmed_wash_trading"
    $confidence_level = 90
    $explanation = "High overlap between buyers/sellers with many circular trades"
    
  BRANCH B ($wash_score > 30):
    $verdict = "likely_wash_trading"
    $confidence_level = 75
    $explanation = "Significant trader overlap suggests wash trading"
    
  BRANCH C (COUNT(collection: $circular_trades) > 5):
    $verdict = "suspicious_activity"
    $confidence_level = 60
    $explanation = "Multiple circular trades detected"
    
  BRANCH D ($price_volatility > $mean_price):
    $verdict = "price_manipulation"
    $confidence_level = 65
    $explanation = "Extreme price volatility suggests manipulation"
    
  BRANCH E (true):
    $verdict = "legitimate_trading"
    $confidence_level = 70
    $explanation = "No significant wash trading indicators"

**Action:**
RETURN {
  nft_mint: $nft_mint,
  total_transfers: COUNT(collection: $transfers),
  unique_sellers: COUNT(collection: $sellers),
  unique_buyers: COUNT(collection: $buyers),
  trader_overlap: COUNT(collection: $trader_overlap),
  wash_score: $wash_score,
  circular_trades: COUNT(collection: $circular_trades),
  mean_price: $mean_price,
  price_volatility: $price_volatility,
  verdict: $verdict,
  confidence: $confidence_level,
  explanation: $explanation,
  suspicious_transactions: $circular_trades
}
