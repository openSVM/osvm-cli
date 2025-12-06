//! Pump.fun API Client
//!
//! Provides access to pump.fun token data via multiple sources:
//! - ClickHouse (from osvm realtime daemon) - primary, fastest
//! - OpenSVM API (osvm.ai) - comprehensive Solana analytics
//! - Direct on-chain data via Solana RPC - fallback
//!
//! References:
//! - https://osvm.ai/llms.txt - OpenSVM API documentation
//! - https://docs.bloxroute.com/solana/trader-api/api-endpoints/pump.fun

use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

use crate::services::clickhouse_service::{ClickHouseService, ClickHouseStatus};

/// Pump.fun program ID on Solana mainnet
pub const PUMPFUN_PROGRAM_ID: &str = "6EF8rrecthR5Dkzon8Nwu78hRvfCKubJ14M5uBEwF6P";

/// Pump.fun tokens use 6 decimals (not standard 9)
pub const PUMPFUN_TOKEN_DECIMALS: u8 = 6;

/// Bonding curve graduation threshold (~$69k market cap)
pub const GRADUATION_THRESHOLD_SOL: f64 = 85.0;

// ============================================================================
// API Response Types
// ============================================================================

/// Token from pump.fun
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PumpToken {
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub description: Option<String>,
    pub image_uri: Option<String>,
    pub creator: String,
    pub created_at: DateTime<Utc>,
    pub bonding_curve: String,
    pub associated_bonding_curve: String,

    // Market data
    pub price_sol: f64,
    pub price_usd: f64,
    pub market_cap_sol: f64,
    pub market_cap_usd: f64,
    pub liquidity_sol: f64,
    pub volume_24h_sol: f64,

    // Holder data
    pub holder_count: u32,
    pub top_10_holder_pct: f64,
    pub dev_holding_pct: f64,

    // Trading data
    pub buy_count_24h: u32,
    pub sell_count_24h: u32,
    pub unique_traders_24h: u32,

    // Price changes
    pub price_change_1m: f64,
    pub price_change_5m: f64,
    pub price_change_1h: f64,
    pub price_change_24h: f64,

    // Status
    pub is_graduated: bool,  // Migrated to Raydium
    pub graduation_progress: f64,  // 0-100%

    // Metadata
    pub twitter: Option<String>,
    pub telegram: Option<String>,
    pub website: Option<String>,
}

/// New token launch event from WebSocket
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NewTokenEvent {
    pub signature: String,
    pub mint: String,
    pub symbol: String,
    pub name: String,
    pub creator: String,
    pub bonding_curve: String,
    pub associated_bonding_curve: String,
    pub timestamp: DateTime<Utc>,
    pub initial_buy_sol: Option<f64>,
}

/// Trade event from pump.fun
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TradeEvent {
    pub signature: String,
    pub mint: String,
    pub trader: String,
    pub is_buy: bool,
    pub sol_amount: f64,
    pub token_amount: f64,
    pub price_sol: f64,
    pub timestamp: DateTime<Utc>,
    pub market_cap_sol: f64,
}

/// Bonding curve state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BondingCurveState {
    pub mint: String,
    pub virtual_token_reserves: u64,
    pub virtual_sol_reserves: u64,
    pub real_token_reserves: u64,
    pub real_sol_reserves: u64,
    pub token_total_supply: u64,
    pub complete: bool,  // Graduated
}

// ============================================================================
// Client Configuration
// ============================================================================

// OpenSVM API base URL
const OPENSVM_API_URL: &str = "https://opensvm.com/api";

/// Configuration for the pump.fun client
#[derive(Debug, Clone)]
pub struct PumpFunConfig {
    /// OpenSVM API URL (osvm.ai)
    pub opensvm_api_url: String,
    /// OpenSVM JWT token (optional, for authenticated endpoints)
    pub opensvm_jwt_token: Option<String>,
    /// bloXroute auth header (optional, for streams)
    pub bloxroute_auth: Option<String>,
    /// Solana RPC URL
    pub rpc_url: String,
    /// WebSocket URL for subscriptions
    pub ws_url: Option<String>,
    /// Request timeout
    pub timeout: Duration,
    /// Cache TTL for token data
    pub cache_ttl: Duration,
}

impl Default for PumpFunConfig {
    fn default() -> Self {
        Self {
            opensvm_api_url: std::env::var("OPENSVM_API_URL")
                .unwrap_or_else(|_| OPENSVM_API_URL.to_string()),
            opensvm_jwt_token: std::env::var("OPENSVM_JWT_TOKEN").ok(),
            bloxroute_auth: std::env::var("BLOXROUTE_AUTH").ok(),
            rpc_url: std::env::var("SOLANA_RPC_URL")
                .unwrap_or_else(|_| "https://api.mainnet-beta.solana.com".to_string()),
            ws_url: std::env::var("SOLANA_WS_URL").ok(),
            timeout: Duration::from_secs(30),
            cache_ttl: Duration::from_secs(5),
        }
    }
}

// ============================================================================
// Pump.fun Client
// ============================================================================

/// Client for interacting with pump.fun data
pub struct PumpFunClient {
    config: PumpFunConfig,
    http: Client,
    cache: Arc<RwLock<TokenCache>>,
    clickhouse: Option<Arc<ClickHouseService>>,
}

struct TokenCache {
    tokens: HashMap<String, (PumpToken, std::time::Instant)>,
    new_tokens: Vec<PumpToken>,
    trending: Vec<PumpToken>,
    last_update: Option<std::time::Instant>,
    sol_price_usd: f64,
    sol_price_updated: Option<std::time::Instant>,
}

impl Default for TokenCache {
    fn default() -> Self {
        Self {
            tokens: HashMap::new(),
            new_tokens: Vec::new(),
            trending: Vec::new(),
            last_update: None,
            sol_price_usd: 200.0, // Default fallback
            sol_price_updated: None,
        }
    }
}

impl PumpFunClient {
    /// Create a new pump.fun client
    pub fn new(config: PumpFunConfig) -> Self {
        let http = Client::builder()
            .timeout(config.timeout)
            .build()
            .expect("Failed to create HTTP client");

        // Try to connect to ClickHouse if available
        let clickhouse = ClickHouseService::new()
            .ok()
            .map(Arc::new);

        Self {
            config,
            http,
            cache: Arc::new(RwLock::new(TokenCache::default())),
            clickhouse,
        }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(PumpFunConfig::default())
    }

    /// Create with explicit ClickHouse service
    pub fn with_clickhouse(config: PumpFunConfig, clickhouse: Arc<ClickHouseService>) -> Self {
        let http = Client::builder()
            .timeout(config.timeout)
            .build()
            .expect("Failed to create HTTP client");

        Self {
            config,
            http,
            cache: Arc::new(RwLock::new(TokenCache::default())),
            clickhouse: Some(clickhouse),
        }
    }

    /// Check if ClickHouse is available and running
    pub async fn is_clickhouse_available(&self) -> bool {
        if let Some(ch) = &self.clickhouse {
            if let Ok(status) = ch.status().await {
                return status == ClickHouseStatus::Running;
            }
        }
        false
    }

    /// Get current SOL price in USD (cached for 60 seconds)
    pub async fn get_sol_price_usd(&self) -> f64 {
        const CACHE_TTL_SECS: u64 = 60;

        // Check cache first
        {
            let cache = self.cache.read().await;
            if let Some(updated) = cache.sol_price_updated {
                if updated.elapsed().as_secs() < CACHE_TTL_SECS {
                    return cache.sol_price_usd;
                }
            }
        }

        // Fetch fresh price from CoinGecko (free, no auth required)
        let price = self.fetch_sol_price_from_api().await;

        // Update cache
        {
            let mut cache = self.cache.write().await;
            cache.sol_price_usd = price;
            cache.sol_price_updated = Some(std::time::Instant::now());
        }

        price
    }

    /// Fetch SOL price from CoinGecko API
    async fn fetch_sol_price_from_api(&self) -> f64 {
        // CoinGecko simple price endpoint (free, no auth)
        let url = "https://api.coingecko.com/api/v3/simple/price?ids=solana&vs_currencies=usd";

        match self.http.get(url).send().await {
            Ok(resp) => {
                if resp.status().is_success() {
                    #[derive(Deserialize)]
                    struct SolPrice {
                        usd: f64,
                    }
                    #[derive(Deserialize)]
                    struct CoinGeckoResponse {
                        solana: SolPrice,
                    }

                    if let Ok(data) = resp.json::<CoinGeckoResponse>().await {
                        log::debug!("Fetched SOL price: ${:.2}", data.solana.usd);
                        return data.solana.usd;
                    }
                }
            }
            Err(e) => {
                log::warn!("Failed to fetch SOL price: {}", e);
            }
        }

        // Fallback to cached or default
        let cache = self.cache.read().await;
        cache.sol_price_usd
    }

    // ========================================================================
    // ClickHouse Queries (Primary data source from realtime daemon)
    // ========================================================================

    /// Get recent pump.fun transactions from ClickHouse
    pub async fn get_recent_transactions_clickhouse(&self, limit: u32) -> Result<Vec<TradeEvent>> {
        let ch = self.clickhouse.as_ref()
            .ok_or_else(|| anyhow!("ClickHouse not available"))?;

        // Query pump.fun transactions indexed by realtime daemon
        let query = format!(
            r#"
            SELECT
                signature,
                program_id,
                accounts,
                data,
                timestamp
            FROM transactions
            WHERE program_id = '{}'
            ORDER BY timestamp DESC
            LIMIT {}
            "#,
            PUMPFUN_PROGRAM_ID,
            limit
        );

        // Execute query and parse results
        // Note: This is a placeholder - actual implementation depends on ClickHouse schema
        let _result = ch.execute_query(&query).await?;

        // For now, return empty vec - real implementation would parse trade events
        Ok(Vec::new())
    }

    /// Get new token launches from ClickHouse (indexed by realtime daemon)
    pub async fn get_new_tokens_clickhouse(&self, hours: u32) -> Result<Vec<PumpToken>> {
        let ch = self.clickhouse.as_ref()
            .ok_or_else(|| anyhow!("ClickHouse not available"))?;

        // Query for token creation transactions (instruction discriminator for Create)
        let query = format!(
            r#"
            SELECT DISTINCT
                accounts[2] as mint,
                timestamp
            FROM transactions
            WHERE program_id = '{}'
              AND timestamp > now() - INTERVAL {} HOUR
              AND substring(data, 1, 8) = '181ec828051c0777'  -- Create instruction discriminator
            ORDER BY timestamp DESC
            LIMIT 100
            "#,
            PUMPFUN_PROGRAM_ID,
            hours
        );

        let _result = ch.execute_query(&query).await?;

        // For now, return empty - real implementation would fetch token metadata
        Ok(Vec::new())
    }

    /// Get trading volume from ClickHouse
    pub async fn get_volume_stats_clickhouse(&self, mint: &str) -> Result<(f64, u32, u32)> {
        let ch = self.clickhouse.as_ref()
            .ok_or_else(|| anyhow!("ClickHouse not available"))?;

        let query = format!(
            r#"
            SELECT
                count(*) as trade_count,
                countIf(is_buy = 1) as buy_count,
                countIf(is_buy = 0) as sell_count
            FROM pumpfun_trades
            WHERE mint = '{}'
              AND timestamp > now() - INTERVAL 24 HOUR
            "#,
            mint
        );

        let _result = ch.execute_query(&query).await?;

        // Placeholder return
        Ok((0.0, 0, 0))
    }

    // ========================================================================
    // OpenSVM API (osvm.ai) - Primary external data source
    // Docs: https://osvm.ai/llms.txt
    // ========================================================================

    /// Get trending/top tokens from OpenSVM API
    pub async fn get_trending_tokens_opensvm(&self) -> Result<Vec<PumpToken>> {
        let url = format!("{}/trading/markets?type=trending&limit=50", self.config.opensvm_api_url);

        let mut req = self.http.get(&url);

        // Add auth if available
        if let Some(token) = &self.config.opensvm_jwt_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            return Err(anyhow!("OpenSVM API error: {}", response.status()));
        }

        // Fetch current SOL price for accurate USD->SOL conversions
        let sol_price_usd = self.get_sol_price_usd().await;
        log::debug!("Using SOL price: ${:.2} for conversions", sol_price_usd);

        let data: OpenSvmMarketsResponse = response.json().await?;
        Ok(data.markets.into_iter().map(|t| t.to_pump_token(sol_price_usd)).collect())
    }

    /// Get recent trades for a token from OpenSVM API
    pub async fn get_trades_opensvm(&self, mint: &str, limit: u32) -> Result<Vec<TradeEvent>> {
        let url = format!(
            "{}/trades?mint={}&limit={}",
            self.config.opensvm_api_url, mint, limit
        );

        let mut req = self.http.get(&url);
        if let Some(token) = &self.config.opensvm_jwt_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            return Err(anyhow!("OpenSVM API error: {}", response.status()));
        }

        let data: OpenSvmTradesResponse = response.json().await?;
        Ok(data.data.into_iter().map(|t| t.into()).collect())
    }

    /// Get token info from OpenSVM API
    pub async fn get_token_info_opensvm(&self, mint: &str) -> Result<PumpToken> {
        let url = format!("{}/token/{}", self.config.opensvm_api_url, mint);

        let mut req = self.http.get(&url);
        if let Some(token) = &self.config.opensvm_jwt_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            return Err(anyhow!("OpenSVM API error: {}", response.status()));
        }

        let data: OpenSvmTokenResponse = response.json().await?;
        Ok(data.into())
    }

    /// Get market data from OpenSVM API
    pub async fn get_market_data_opensvm(&self, mint: &str) -> Result<MarketData> {
        let url = format!("{}/market-data?mint={}", self.config.opensvm_api_url, mint);

        let response = self.http.get(&url).send().await?;

        if !response.status().is_success() {
            return Err(anyhow!("OpenSVM API error: {}", response.status()));
        }

        let data: OpenSvmMarketDataResponse = response.json().await?;
        Ok(data.into())
    }

    /// Get traders for a token from OpenSVM API
    pub async fn get_token_traders_opensvm(&self, mint: &str) -> Result<Vec<TraderInfo>> {
        let url = format!(
            "{}/token/{}/traders?limit=50&includeVolume=true",
            self.config.opensvm_api_url, mint
        );

        let mut req = self.http.get(&url);
        if let Some(token) = &self.config.opensvm_jwt_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let response = req.send().await?;

        if !response.status().is_success() {
            return Err(anyhow!("OpenSVM API error: {}", response.status()));
        }

        let data: OpenSvmTradersResponse = response.json().await?;
        Ok(data.data)
    }

    /// Check if OpenSVM API is available
    pub fn has_opensvm_api(&self) -> bool {
        true // OpenSVM API is always available (public endpoints)
    }

    // ========================================================================
    // Direct RPC Methods (fallback)
    // ========================================================================

    /// Get bonding curve state from on-chain data
    pub async fn get_bonding_curve_state(&self, bonding_curve: &str) -> Result<BondingCurveState> {
        use solana_client::nonblocking::rpc_client::RpcClient;
        use solana_sdk::pubkey::Pubkey;
        use std::str::FromStr;

        let rpc = RpcClient::new(self.config.rpc_url.clone());
        let pubkey = Pubkey::from_str(bonding_curve)?;

        let account = rpc.get_account(&pubkey).await?;

        // Parse bonding curve account data
        // Layout: discriminator(8) + virtualTokenReserves(8) + virtualSolReserves(8) +
        //         realTokenReserves(8) + realSolReserves(8) + tokenTotalSupply(8) + complete(1)
        let data = &account.data;
        if data.len() < 49 {
            return Err(anyhow!("Invalid bonding curve data"));
        }

        let virtual_token_reserves = u64::from_le_bytes(data[8..16].try_into()?);
        let virtual_sol_reserves = u64::from_le_bytes(data[16..24].try_into()?);
        let real_token_reserves = u64::from_le_bytes(data[24..32].try_into()?);
        let real_sol_reserves = u64::from_le_bytes(data[32..40].try_into()?);
        let token_total_supply = u64::from_le_bytes(data[40..48].try_into()?);
        let complete = data[48] != 0;

        Ok(BondingCurveState {
            mint: String::new(), // Would need to derive from bonding curve
            virtual_token_reserves,
            virtual_sol_reserves,
            real_token_reserves,
            real_sol_reserves,
            token_total_supply,
            complete,
        })
    }

    /// Calculate price from bonding curve state
    pub fn calculate_price_sol(state: &BondingCurveState) -> f64 {
        if state.virtual_token_reserves == 0 {
            return 0.0;
        }

        // Price = virtualSolReserves / virtualTokenReserves
        let sol_reserves = state.virtual_sol_reserves as f64 / 1e9; // lamports to SOL
        let token_reserves = state.virtual_token_reserves as f64 / 1e6; // 6 decimals

        sol_reserves / token_reserves
    }

    /// Calculate market cap from bonding curve state
    pub fn calculate_market_cap_sol(state: &BondingCurveState) -> f64 {
        let price = Self::calculate_price_sol(state);
        let total_supply = state.token_total_supply as f64 / 1e6;
        price * total_supply
    }

    /// Calculate graduation progress (0-100%)
    pub fn calculate_graduation_progress(state: &BondingCurveState) -> f64 {
        let real_sol = state.real_sol_reserves as f64 / 1e9;
        (real_sol / GRADUATION_THRESHOLD_SOL * 100.0).min(100.0)
    }

    // ========================================================================
    // Aggregated Methods (use best available source)
    // Priority: 1) ClickHouse (realtime daemon) 2) OpenSVM API 3) Cache
    // ========================================================================

    /// Get new tokens from best available source
    pub async fn get_new_tokens(&self) -> Result<Vec<PumpToken>> {
        // 1. Try ClickHouse first (realtime daemon data)
        if self.is_clickhouse_available().await {
            if let Ok(tokens) = self.get_new_tokens_clickhouse(24).await {
                if !tokens.is_empty() {
                    log::debug!("Got {} new tokens from ClickHouse", tokens.len());
                    return Ok(tokens);
                }
            }
        }

        // 2. Try OpenSVM API (osvm.ai)
        if let Ok(tokens) = self.get_trending_tokens_opensvm().await {
            log::debug!("Got {} tokens from OpenSVM API", tokens.len());
            return Ok(tokens);
        }

        // 3. Fallback: return cached data or empty
        let cache = self.cache.read().await;
        Ok(cache.new_tokens.clone())
    }

    /// Get trending tokens
    pub async fn get_trending_tokens(&self) -> Result<Vec<PumpToken>> {
        // 1. Try ClickHouse first - query tokens with highest volume
        if self.is_clickhouse_available().await {
            // Could query for tokens with most trades in last hour
            // For now, fall through to OpenSVM
        }

        // 2. Try OpenSVM API (osvm.ai)
        if let Ok(mut tokens) = self.get_trending_tokens_opensvm().await {
            // Sort by volume
            tokens.sort_by(|a, b| b.volume_24h_sol.partial_cmp(&a.volume_24h_sol).unwrap());
            log::debug!("Got {} trending tokens from OpenSVM API", tokens.len());
            return Ok(tokens.into_iter().take(50).collect());
        }

        // 3. Fallback
        let cache = self.cache.read().await;
        Ok(cache.trending.clone())
    }

    /// Get data source status for UI
    pub async fn get_data_source_status(&self) -> DataSourceStatus {
        DataSourceStatus {
            clickhouse_available: self.is_clickhouse_available().await,
            opensvm_available: true, // Always available (public API)
            bloxroute_available: self.config.bloxroute_auth.is_some(),
        }
    }
}

/// Status of available data sources
#[derive(Debug, Clone)]
pub struct DataSourceStatus {
    pub clickhouse_available: bool,
    pub opensvm_available: bool,
    pub bloxroute_available: bool,
}

impl DataSourceStatus {
    pub fn best_source(&self) -> &'static str {
        if self.clickhouse_available {
            "ClickHouse (realtime)"
        } else if self.opensvm_available {
            "OpenSVM API (osvm.ai)"
        } else {
            "Cache only"
        }
    }
}

/// Market data from OpenSVM API
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketData {
    pub price_sol: f64,
    pub price_usd: f64,
    pub volume_24h: f64,
    pub market_cap: f64,
    pub liquidity: f64,
    pub price_change_24h: f64,
}

/// Trader info from OpenSVM API
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraderInfo {
    pub address: String,
    pub volume_sol: f64,
    pub trade_count: u32,
    pub pnl_sol: f64,
}

impl PumpFunClient {
    /// Get top gainers
    pub async fn get_top_gainers(&self, timeframe: GainerTimeframe) -> Result<Vec<PumpToken>> {
        let tokens = self.get_trending_tokens().await?;

        let mut sorted: Vec<_> = tokens.into_iter()
            .filter(|t| {
                let change = match timeframe {
                    GainerTimeframe::FiveMin => t.price_change_5m,
                    GainerTimeframe::OneHour => t.price_change_1h,
                    GainerTimeframe::TwentyFourHour => t.price_change_24h,
                };
                change > 0.0
            })
            .collect();

        sorted.sort_by(|a, b| {
            let a_change = match timeframe {
                GainerTimeframe::FiveMin => a.price_change_5m,
                GainerTimeframe::OneHour => a.price_change_1h,
                GainerTimeframe::TwentyFourHour => a.price_change_24h,
            };
            let b_change = match timeframe {
                GainerTimeframe::FiveMin => b.price_change_5m,
                GainerTimeframe::OneHour => b.price_change_1h,
                GainerTimeframe::TwentyFourHour => b.price_change_24h,
            };
            b_change.partial_cmp(&a_change).unwrap()
        });

        Ok(sorted.into_iter().take(20).collect())
    }

    /// Get top losers
    pub async fn get_top_losers(&self, timeframe: GainerTimeframe) -> Result<Vec<PumpToken>> {
        let tokens = self.get_trending_tokens().await?;

        let mut sorted: Vec<_> = tokens.into_iter()
            .filter(|t| {
                let change = match timeframe {
                    GainerTimeframe::FiveMin => t.price_change_5m,
                    GainerTimeframe::OneHour => t.price_change_1h,
                    GainerTimeframe::TwentyFourHour => t.price_change_24h,
                };
                change < 0.0
            })
            .collect();

        sorted.sort_by(|a, b| {
            let a_change = match timeframe {
                GainerTimeframe::FiveMin => a.price_change_5m,
                GainerTimeframe::OneHour => a.price_change_1h,
                GainerTimeframe::TwentyFourHour => a.price_change_24h,
            };
            let b_change = match timeframe {
                GainerTimeframe::FiveMin => b.price_change_5m,
                GainerTimeframe::OneHour => b.price_change_1h,
                GainerTimeframe::TwentyFourHour => b.price_change_24h,
            };
            a_change.partial_cmp(&b_change).unwrap()
        });

        Ok(sorted.into_iter().take(20).collect())
    }

    /// Get token by mint address
    pub async fn get_token(&self, mint: &str) -> Result<PumpToken> {
        // Check cache first
        {
            let cache = self.cache.read().await;
            if let Some((token, time)) = cache.tokens.get(mint) {
                if time.elapsed() < self.config.cache_ttl {
                    return Ok(token.clone());
                }
            }
        }

        // Fetch from OpenSVM API
        if let Ok(token) = self.get_token_info_opensvm(mint).await {
            // Update cache
            let mut cache = self.cache.write().await;
            cache.tokens.insert(mint.to_string(), (token.clone(), std::time::Instant::now()));

            return Ok(token);
        }

        Err(anyhow!("Token not found: {}", mint))
    }
}

// ============================================================================
// Timeframe enum
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GainerTimeframe {
    FiveMin,
    OneHour,
    TwentyFourHour,
}

// ============================================================================
// OpenSVM API Response Types
// Docs: https://osvm.ai/llms.txt
// ============================================================================

/// Response from /api/trading/markets
#[derive(Debug, Deserialize)]
struct OpenSvmMarketsResponse {
    #[serde(default)]
    markets: Vec<OpenSvmMarketToken>,
}

/// Token from markets endpoint
#[derive(Debug, Deserialize)]
struct OpenSvmMarketToken {
    /// Token mint address
    #[serde(default)]
    mint: String,
    /// Trading pair symbol (e.g., "SOL/USDC")
    #[serde(default)]
    symbol: String,
    /// Base token name
    #[serde(rename = "baseToken", default)]
    base_token: String,
    /// Current price in USD
    #[serde(default)]
    price: f64,
    /// 24h price change percentage
    #[serde(rename = "change24h", default)]
    change_24h: f64,
    /// 24h trading volume in USD
    #[serde(rename = "volume24h", default)]
    volume_24h: f64,
    /// Market cap in USD
    #[serde(rename = "marketCap", default)]
    market_cap: f64,
    /// Liquidity in USD
    #[serde(default)]
    liquidity: f64,
}

impl OpenSvmMarketToken {
    /// Convert to PumpToken using the provided SOL price
    fn to_pump_token(self, sol_price_usd: f64) -> PumpToken {
        PumpToken {
            mint: self.mint,
            symbol: if self.symbol.is_empty() { self.base_token.clone() } else { self.symbol },
            name: if self.base_token.is_empty() { "Unknown".to_string() } else { self.base_token },
            description: None,
            image_uri: None,
            creator: String::new(),
            created_at: Utc::now(),
            bonding_curve: String::new(),
            associated_bonding_curve: String::new(),
            price_sol: self.price / sol_price_usd, // Convert USD to SOL
            price_usd: self.price,
            market_cap_sol: self.market_cap / sol_price_usd,
            market_cap_usd: self.market_cap,
            liquidity_sol: self.liquidity / sol_price_usd,
            volume_24h_sol: self.volume_24h / sol_price_usd,
            holder_count: 0, // Not available from this endpoint
            top_10_holder_pct: 0.0,
            dev_holding_pct: 0.0,
            buy_count_24h: 0,
            sell_count_24h: 0,
            unique_traders_24h: 0,
            price_change_1m: 0.0,
            price_change_5m: self.change_24h / 288.0, // Rough estimate
            price_change_1h: self.change_24h / 24.0, // Rough estimate
            price_change_24h: self.change_24h,
            is_graduated: false,
            graduation_progress: 0.0,
            twitter: None,
            telegram: None,
            website: None,
        }
    }
}

/// Response from /api/trades
#[derive(Debug, Deserialize)]
struct OpenSvmTradesResponse {
    #[serde(default)]
    data: Vec<OpenSvmTrade>,
}

/// Trade from trades endpoint
#[derive(Debug, Deserialize)]
struct OpenSvmTrade {
    #[serde(default)]
    signature: String,
    #[serde(default)]
    mint: String,
    #[serde(default)]
    symbol: String,
    #[serde(default)]
    trader: String,
    #[serde(rename = "isBuy", default)]
    is_buy: bool,
    #[serde(rename = "solAmount", default)]
    sol_amount: f64,
    #[serde(rename = "tokenAmount", default)]
    token_amount: f64,
    #[serde(default)]
    price: f64,
    #[serde(default)]
    timestamp: i64,
}

impl From<OpenSvmTrade> for TradeEvent {
    fn from(t: OpenSvmTrade) -> Self {
        TradeEvent {
            signature: t.signature,
            mint: t.mint,
            trader: t.trader,
            is_buy: t.is_buy,
            sol_amount: t.sol_amount,
            token_amount: t.token_amount,
            price_sol: t.price,
            timestamp: chrono::DateTime::from_timestamp(t.timestamp, 0)
                .unwrap_or_else(|| Utc::now()),
            market_cap_sol: 0.0,
        }
    }
}

/// Response from /api/token/:address
#[derive(Debug, Deserialize)]
struct OpenSvmTokenResponse {
    #[serde(default)]
    address: String,
    #[serde(default)]
    symbol: String,
    #[serde(default)]
    name: String,
    #[serde(default)]
    price: f64,
    #[serde(rename = "priceUsd", default)]
    price_usd: f64,
    #[serde(rename = "marketCap", default)]
    market_cap: f64,
    #[serde(default)]
    liquidity: f64,
    #[serde(rename = "volume24h", default)]
    volume_24h: f64,
    #[serde(rename = "priceChange24h", default)]
    price_change_24h: f64,
    #[serde(default)]
    holders: u32,
}

impl From<OpenSvmTokenResponse> for PumpToken {
    fn from(t: OpenSvmTokenResponse) -> Self {
        PumpToken {
            mint: t.address,
            symbol: if t.symbol.is_empty() { "???".to_string() } else { t.symbol },
            name: if t.name.is_empty() { "Unknown".to_string() } else { t.name },
            description: None,
            image_uri: None,
            creator: String::new(),
            created_at: Utc::now(),
            bonding_curve: String::new(),
            associated_bonding_curve: String::new(),
            price_sol: t.price,
            price_usd: t.price_usd,
            market_cap_sol: t.market_cap / 200.0,
            market_cap_usd: t.market_cap,
            liquidity_sol: t.liquidity / 200.0,
            volume_24h_sol: t.volume_24h / 200.0,
            holder_count: t.holders,
            top_10_holder_pct: 0.0,
            dev_holding_pct: 0.0,
            buy_count_24h: 0,
            sell_count_24h: 0,
            unique_traders_24h: 0,
            price_change_1m: 0.0,
            price_change_5m: 0.0,
            price_change_1h: 0.0,
            price_change_24h: t.price_change_24h,
            is_graduated: false,
            graduation_progress: 0.0,
            twitter: None,
            telegram: None,
            website: None,
        }
    }
}

/// Response from /api/market-data
#[derive(Debug, Deserialize)]
struct OpenSvmMarketDataResponse {
    #[serde(default)]
    price: f64,
    #[serde(rename = "priceUsd", default)]
    price_usd: f64,
    #[serde(rename = "volume24h", default)]
    volume_24h: f64,
    #[serde(rename = "marketCap", default)]
    market_cap: f64,
    #[serde(default)]
    liquidity: f64,
    #[serde(rename = "priceChange24h", default)]
    price_change_24h: f64,
}

impl From<OpenSvmMarketDataResponse> for MarketData {
    fn from(r: OpenSvmMarketDataResponse) -> Self {
        MarketData {
            price_sol: r.price,
            price_usd: r.price_usd,
            volume_24h: r.volume_24h,
            market_cap: r.market_cap,
            liquidity: r.liquidity,
            price_change_24h: r.price_change_24h,
        }
    }
}

/// Response from /api/token/:address/traders
#[derive(Debug, Deserialize)]
struct OpenSvmTradersResponse {
    #[serde(default)]
    data: Vec<TraderInfo>,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_price_calculation() {
        let state = BondingCurveState {
            mint: "test".to_string(),
            virtual_token_reserves: 1_000_000_000_000, // 1M tokens (6 decimals)
            virtual_sol_reserves: 30_000_000_000,      // 30 SOL (9 decimals)
            real_token_reserves: 0,
            real_sol_reserves: 0,
            token_total_supply: 1_000_000_000_000,
            complete: false,
        };

        let price = PumpFunClient::calculate_price_sol(&state);
        assert!((price - 0.00003).abs() < 0.00001);
    }

    #[test]
    fn test_graduation_progress() {
        let state = BondingCurveState {
            mint: "test".to_string(),
            virtual_token_reserves: 1_000_000_000_000,
            virtual_sol_reserves: 30_000_000_000,
            real_token_reserves: 0,
            real_sol_reserves: 42_500_000_000, // 42.5 SOL
            token_total_supply: 1_000_000_000_000,
            complete: false,
        };

        let progress = PumpFunClient::calculate_graduation_progress(&state);
        assert!((progress - 50.0).abs() < 1.0);
    }
}
