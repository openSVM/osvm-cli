//! Jupiter API client for token swaps
//!
//! Provides quote fetching and swap transaction building via Jupiter aggregator.

use anyhow::{anyhow, Result};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::time::Duration;

const JUPITER_QUOTE_API: &str = "https://quote-api.jup.ag/v6/quote";
const JUPITER_SWAP_API: &str = "https://quote-api.jup.ag/v6/swap";
const JUPITER_TOKENS_API: &str = "https://token.jup.ag/all";

/// Jupiter API client
pub struct JupiterClient {
    client: Client,
}

impl Default for JupiterClient {
    fn default() -> Self {
        Self::new()
    }
}

impl JupiterClient {
    /// Create a new Jupiter client
    pub fn new() -> Self {
        Self {
            client: Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .expect("Failed to create HTTP client"),
        }
    }

    /// Get a quote for a token swap
    pub async fn get_quote(&self, params: &QuoteParams) -> Result<QuoteResponse> {
        let url = format!(
            "{}?inputMint={}&outputMint={}&amount={}&slippageBps={}",
            JUPITER_QUOTE_API,
            params.input_mint,
            params.output_mint,
            params.amount,
            params.slippage_bps
        );

        let response = self.client.get(&url).send().await?;

        if !response.status().is_success() {
            let status = response.status();
            let error_text = response.text().await.unwrap_or_default();
            return Err(anyhow!("Jupiter quote error ({}): {}", status, error_text));
        }

        Ok(response.json().await?)
    }

    /// Get a swap transaction from Jupiter
    pub async fn get_swap_transaction(
        &self,
        quote: &QuoteResponse,
        user_pubkey: &str,
    ) -> Result<SwapTransaction> {
        let body = serde_json::json!({
            "quoteResponse": quote,
            "userPublicKey": user_pubkey,
            "wrapAndUnwrapSol": true,
            "dynamicComputeUnitLimit": true,
            "prioritizationFeeLamports": "auto"
        });

        let response = self
            .client
            .post(JUPITER_SWAP_API)
            .json(&body)
            .send()
            .await?;

        if !response.status().is_success() {
            let status = response.status();
            let error_text = response.text().await.unwrap_or_default();
            return Err(anyhow!("Jupiter swap error ({}): {}", status, error_text));
        }

        Ok(response.json().await?)
    }

    /// Fetch the full token list from Jupiter
    pub async fn get_token_list(&self) -> Result<Vec<TokenInfo>> {
        let response = self.client.get(JUPITER_TOKENS_API).send().await?;

        if !response.status().is_success() {
            let status = response.status();
            let error_text = response.text().await.unwrap_or_default();
            return Err(anyhow!("Jupiter tokens error ({}): {}", status, error_text));
        }

        Ok(response.json().await?)
    }
}

/// Parameters for getting a quote
#[derive(Debug, Clone, Serialize)]
pub struct QuoteParams {
    /// Input token mint address
    pub input_mint: String,
    /// Output token mint address
    pub output_mint: String,
    /// Amount in smallest units (lamports for SOL)
    pub amount: u64,
    /// Slippage tolerance in basis points (100 = 1%)
    pub slippage_bps: u16,
}

/// Quote response from Jupiter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuoteResponse {
    /// Input token mint
    #[serde(rename = "inputMint")]
    pub input_mint: String,
    /// Output token mint
    #[serde(rename = "outputMint")]
    pub output_mint: String,
    /// Input amount in smallest units
    #[serde(rename = "inAmount")]
    pub in_amount: String,
    /// Output amount in smallest units
    #[serde(rename = "outAmount")]
    pub out_amount: String,
    /// Price impact percentage
    #[serde(rename = "priceImpactPct")]
    pub price_impact_pct: String,
    /// Route plan (series of swaps)
    #[serde(rename = "routePlan")]
    pub route_plan: Vec<RoutePlanStep>,
    /// Swap mode (ExactIn or ExactOut)
    #[serde(rename = "swapMode")]
    pub swap_mode: String,
    /// Other market infos (for serialization to swap endpoint)
    #[serde(rename = "otherAmountThreshold")]
    pub other_amount_threshold: String,
    /// Context slot
    #[serde(rename = "contextSlot", default)]
    pub context_slot: Option<u64>,
    /// Time taken
    #[serde(rename = "timeTaken", default)]
    pub time_taken: Option<f64>,
}

/// A step in the route plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutePlanStep {
    /// Swap info for this step
    #[serde(rename = "swapInfo")]
    pub swap_info: SwapInfo,
    /// Percentage of the total swap going through this route
    pub percent: u8,
}

/// Information about a single swap in the route
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwapInfo {
    /// AMM/DEX key
    #[serde(rename = "ammKey")]
    pub amm_key: String,
    /// DEX label (e.g., "Raydium", "Orca")
    pub label: Option<String>,
    /// Input mint for this step
    #[serde(rename = "inputMint")]
    pub input_mint: String,
    /// Output mint for this step
    #[serde(rename = "outputMint")]
    pub output_mint: String,
    /// Input amount for this step
    #[serde(rename = "inAmount")]
    pub in_amount: String,
    /// Output amount for this step
    #[serde(rename = "outAmount")]
    pub out_amount: String,
    /// Fee amount
    #[serde(rename = "feeAmount")]
    pub fee_amount: String,
    /// Fee mint
    #[serde(rename = "feeMint")]
    pub fee_mint: String,
}

/// Swap transaction response from Jupiter
#[derive(Debug, Clone, Deserialize)]
pub struct SwapTransaction {
    /// Base64-encoded versioned transaction
    #[serde(rename = "swapTransaction")]
    pub swap_transaction: String,
    /// Last valid block height for the transaction
    #[serde(rename = "lastValidBlockHeight")]
    pub last_valid_block_height: u64,
    /// Priority fee lamports (if computed)
    #[serde(rename = "prioritizationFeeLamports", default)]
    pub prioritization_fee_lamports: Option<u64>,
}

/// Token information from Jupiter's token list
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TokenInfo {
    /// Token mint address
    pub address: String,
    /// Token symbol (e.g., "SOL", "USDC")
    pub symbol: String,
    /// Token name
    pub name: String,
    /// Token decimals
    pub decimals: u8,
    /// Logo URI
    #[serde(rename = "logoURI")]
    pub logo_uri: Option<String>,
    /// Token tags (e.g., ["verified", "community"])
    pub tags: Option<Vec<String>>,
    /// Daily volume (if available)
    #[serde(rename = "daily_volume", default)]
    pub daily_volume: Option<f64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quote_params_serialization() {
        let params = QuoteParams {
            input_mint: "So11111111111111111111111111111111111111112".to_string(),
            output_mint: "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v".to_string(),
            amount: 1_000_000_000,
            slippage_bps: 50,
        };

        let json = serde_json::to_string(&params).unwrap();
        assert!(json.contains("input_mint"));
        assert!(json.contains("1000000000"));
    }

    #[test]
    fn test_quote_response_deserialization() {
        let json = r#"{
            "inputMint": "So11111111111111111111111111111111111111112",
            "outputMint": "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v",
            "inAmount": "1000000000",
            "outAmount": "95000000",
            "priceImpactPct": "0.01",
            "routePlan": [],
            "swapMode": "ExactIn",
            "otherAmountThreshold": "94050000"
        }"#;

        let response: QuoteResponse = serde_json::from_str(json).unwrap();
        assert_eq!(response.input_mint, "So11111111111111111111111111111111111111112");
        assert_eq!(response.out_amount, "95000000");
    }
}
