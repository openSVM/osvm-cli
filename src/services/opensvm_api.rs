//! OpenSVM API Client
//!
//! Client for the OpenSVM IDL & Annotations API.
//! Base URL: https://opensvm.com/api
//!
//! Features:
//! - Fetch and cache address annotations (labels, risk, tags)
//! - Retrieve program IDLs
//! - Create annotations for discovered wallets
//! - Semantic search for addresses

use anyhow::{Context, Result};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const BASE_URL: &str = "https://opensvm.com/api";
const CACHE_TTL_SECS: u64 = 300; // 5 minutes

/// OpenSVM API client with caching
pub struct OpenSvmApi {
    client: Client,
    /// Cache for annotations by address
    annotation_cache: Arc<Mutex<HashMap<String, CachedAnnotation>>>,
}

#[derive(Clone)]
struct CachedAnnotation {
    data: Option<AddressAnnotation>,
    fetched_at: Instant,
}

// ═══════════════════════════════════════════════════════════════════════════
// API Response Types
// ═══════════════════════════════════════════════════════════════════════════

/// Annotation for a Solana address
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AddressAnnotation {
    pub id: Option<String>,
    pub address: String,
    #[serde(rename = "type")]
    pub annotation_type: Option<String>,
    pub label: String,
    pub description: Option<String>,
    pub tags: Option<Vec<String>>,
    pub category: Option<String>,
    pub risk: Option<String>,
    pub metadata: Option<serde_json::Value>,
    #[serde(rename = "createdAt")]
    pub created_at: Option<String>,
    #[serde(rename = "updatedAt")]
    pub updated_at: Option<String>,
}

/// Response from GET /api/annotations/address/:address
#[derive(Debug, Deserialize)]
pub struct AddressAnnotationsResponse {
    pub address: String,
    pub count: usize,
    pub annotations: Vec<AddressAnnotation>,
}

/// Response from GET /api/annotations
#[derive(Debug, Deserialize)]
pub struct AnnotationsListResponse {
    pub annotations: Vec<AddressAnnotation>,
    pub total: usize,
}

/// Response from POST /api/annotations
#[derive(Debug, Deserialize)]
pub struct CreateAnnotationResponse {
    pub id: String,
    pub address: String,
    pub label: String,
    #[serde(rename = "type")]
    pub annotation_type: Option<String>,
}

/// Response from GET /api/annotations/stats
#[derive(Debug, Deserialize)]
pub struct AnnotationStats {
    pub total: usize,
    #[serde(rename = "byType")]
    pub by_type: Option<HashMap<String, usize>>,
    #[serde(rename = "byRisk")]
    pub by_risk: Option<HashMap<String, usize>>,
    #[serde(rename = "uniqueAddresses")]
    pub unique_addresses: Option<usize>,
}

/// IDL response
#[derive(Debug, Deserialize)]
pub struct IdlResponse {
    #[serde(rename = "programId")]
    pub program_id: String,
    pub name: Option<String>,
    pub version: Option<String>,
    pub network: Option<String>,
    pub idl: Option<serde_json::Value>,
}

/// Request to create annotation
#[derive(Debug, Serialize)]
pub struct CreateAnnotationRequest {
    pub address: String,
    pub label: String,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub annotation_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub category: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub risk: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<serde_json::Value>,
    #[serde(rename = "createdBy", skip_serializing_if = "Option::is_none")]
    pub created_by: Option<String>,
}

// ═══════════════════════════════════════════════════════════════════════════
// Implementation
// ═══════════════════════════════════════════════════════════════════════════

impl OpenSvmApi {
    /// Create a new API client
    pub fn new() -> Self {
        Self {
            client: Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .unwrap_or_default(),
            annotation_cache: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Annotations API
    // ─────────────────────────────────────────────────────────────────────────

    /// Get annotation for a single address (with caching)
    pub async fn get_annotation(&self, address: &str) -> Result<Option<AddressAnnotation>> {
        // Check cache first
        if let Some(cached) = self.check_cache(address) {
            return Ok(cached);
        }

        // Fetch from API
        let url = format!("{}/annotations/address/{}", BASE_URL, address);
        let response = self.client.get(&url).send().await;

        match response {
            Ok(resp) if resp.status().is_success() => {
                let data: AddressAnnotationsResponse = resp.json().await
                    .context("Failed to parse annotation response")?;

                let annotation = data.annotations.into_iter().next();
                self.update_cache(address, annotation.clone());
                Ok(annotation)
            }
            Ok(resp) if resp.status() == 404 => {
                // No annotation exists - cache the negative result
                self.update_cache(address, None);
                Ok(None)
            }
            Ok(resp) => {
                let status = resp.status();
                let text = resp.text().await.unwrap_or_default();
                Err(anyhow::anyhow!("API error {}: {}", status, text))
            }
            Err(e) => Err(anyhow::anyhow!("Request failed: {}", e)),
        }
    }

    /// Get annotations for multiple addresses (batched)
    pub async fn get_annotations_batch(&self, addresses: &[String]) -> HashMap<String, AddressAnnotation> {
        let mut results = HashMap::new();

        // Check cache for all addresses first
        let mut uncached = Vec::new();
        for addr in addresses {
            if let Some(cached) = self.check_cache(addr) {
                if let Some(annotation) = cached {
                    results.insert(addr.clone(), annotation);
                }
            } else {
                uncached.push(addr.clone());
            }
        }

        // Fetch uncached addresses (in parallel batches of 10)
        for chunk in uncached.chunks(10) {
            let futures: Vec<_> = chunk.iter()
                .map(|addr| self.get_annotation(addr))
                .collect();

            let batch_results = futures::future::join_all(futures).await;

            for (addr, result) in chunk.iter().zip(batch_results) {
                if let Ok(Some(annotation)) = result {
                    results.insert(addr.clone(), annotation);
                }
            }
        }

        results
    }

    /// Search annotations
    pub async fn search_annotations(
        &self,
        query: Option<&str>,
        annotation_type: Option<&str>,
        risk: Option<&str>,
        tags: Option<&[&str]>,
        limit: Option<usize>,
    ) -> Result<Vec<AddressAnnotation>> {
        let mut url = format!("{}/annotations?", BASE_URL);

        if let Some(q) = query {
            url.push_str(&format!("q={}&", urlencoding::encode(q)));
        }
        if let Some(t) = annotation_type {
            url.push_str(&format!("type={}&", t));
        }
        if let Some(r) = risk {
            url.push_str(&format!("risk={}&", r));
        }
        if let Some(t) = tags {
            url.push_str(&format!("tags={}&", t.join(",")));
        }
        if let Some(l) = limit {
            url.push_str(&format!("limit={}&", l));
        }

        let response = self.client.get(&url).send().await
            .context("Failed to search annotations")?;

        if response.status().is_success() {
            let data: AnnotationsListResponse = response.json().await
                .context("Failed to parse search response")?;
            Ok(data.annotations)
        } else {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            Err(anyhow::anyhow!("Search failed {}: {}", status, text))
        }
    }

    /// Create a new annotation
    pub async fn create_annotation(&self, request: CreateAnnotationRequest) -> Result<CreateAnnotationResponse> {
        let url = format!("{}/annotations", BASE_URL);

        let response = self.client
            .post(&url)
            .json(&request)
            .send()
            .await
            .context("Failed to create annotation")?;

        if response.status().is_success() {
            let data: CreateAnnotationResponse = response.json().await
                .context("Failed to parse create response")?;

            // Invalidate cache for this address
            self.invalidate_cache(&request.address);

            Ok(data)
        } else {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            Err(anyhow::anyhow!("Create failed {}: {}", status, text))
        }
    }

    /// Get annotation statistics
    pub async fn get_stats(&self) -> Result<AnnotationStats> {
        let url = format!("{}/annotations/stats", BASE_URL);

        let response = self.client.get(&url).send().await
            .context("Failed to get stats")?;

        if response.status().is_success() {
            let data: AnnotationStats = response.json().await
                .context("Failed to parse stats response")?;
            Ok(data)
        } else {
            let status = response.status();
            Err(anyhow::anyhow!("Stats request failed: {}", status))
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // IDL API
    // ─────────────────────────────────────────────────────────────────────────

    /// Get IDL for a program
    pub async fn get_idl(&self, program_id: &str) -> Result<Option<IdlResponse>> {
        let url = format!("{}/idl/{}", BASE_URL, program_id);

        let response = self.client.get(&url).send().await
            .context("Failed to fetch IDL")?;

        if response.status().is_success() {
            let data: IdlResponse = response.json().await
                .context("Failed to parse IDL response")?;
            Ok(Some(data))
        } else if response.status() == 404 {
            Ok(None)
        } else {
            let status = response.status();
            Err(anyhow::anyhow!("IDL request failed: {}", status))
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Cache Management
    // ─────────────────────────────────────────────────────────────────────────

    fn check_cache(&self, address: &str) -> Option<Option<AddressAnnotation>> {
        if let Ok(cache) = self.annotation_cache.lock() {
            if let Some(cached) = cache.get(address) {
                if cached.fetched_at.elapsed() < Duration::from_secs(CACHE_TTL_SECS) {
                    return Some(cached.data.clone());
                }
            }
        }
        None
    }

    fn update_cache(&self, address: &str, annotation: Option<AddressAnnotation>) {
        if let Ok(mut cache) = self.annotation_cache.lock() {
            cache.insert(address.to_string(), CachedAnnotation {
                data: annotation,
                fetched_at: Instant::now(),
            });

            // Limit cache size
            if cache.len() > 1000 {
                // Remove oldest entries
                let mut entries: Vec<_> = cache.iter()
                    .map(|(k, v)| (k.clone(), v.fetched_at))
                    .collect();
                entries.sort_by(|a, b| a.1.cmp(&b.1));
                for (key, _) in entries.into_iter().take(100) {
                    cache.remove(&key);
                }
            }
        }
    }

    fn invalidate_cache(&self, address: &str) {
        if let Ok(mut cache) = self.annotation_cache.lock() {
            cache.remove(address);
        }
    }

    /// Clear all cached annotations
    pub fn clear_cache(&self) {
        if let Ok(mut cache) = self.annotation_cache.lock() {
            cache.clear();
        }
    }
}

impl Default for OpenSvmApi {
    fn default() -> Self {
        Self::new()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Helper Functions
// ═══════════════════════════════════════════════════════════════════════════

/// Format an address with its annotation (if available)
pub fn format_address_with_label(address: &str, annotation: Option<&AddressAnnotation>) -> String {
    match annotation {
        Some(ann) => {
            let risk_icon = match ann.risk.as_deref() {
                Some("safe") => "✅",
                Some("suspicious") => "⚠️",
                Some("malicious") => "🚨",
                _ => "❓",
            };
            format!("{} {} ({})", risk_icon, ann.label, address)
        }
        None => address.to_string(),
    }
}

/// Get risk level color for TUI
pub fn risk_color(risk: Option<&str>) -> &'static str {
    match risk {
        Some("safe") => "green",
        Some("suspicious") => "yellow",
        Some("malicious") => "red",
        _ => "gray",
    }
}

/// Determine annotation type from address characteristics
pub fn infer_annotation_type(address: &str, is_program: bool, is_token: bool) -> &'static str {
    if is_program {
        "program"
    } else if is_token {
        "token"
    } else {
        "wallet"
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_address_with_label() {
        let annotation = AddressAnnotation {
            id: None,
            address: "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN".to_string(),
            annotation_type: Some("program".to_string()),
            label: "Jupiter V6".to_string(),
            description: None,
            tags: Some(vec!["dex".to_string(), "swap".to_string()]),
            category: Some("defi".to_string()),
            risk: Some("safe".to_string()),
            metadata: None,
            created_at: None,
            updated_at: None,
        };

        let formatted = format_address_with_label(
            "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN",
            Some(&annotation)
        );
        assert!(formatted.contains("Jupiter V6"));
        assert!(formatted.contains("✅"));
    }

    #[test]
    fn test_format_address_no_label() {
        let formatted = format_address_with_label(
            "SomeAddress123456789",
            None
        );
        assert_eq!(formatted, "SomeAddress123456789");
    }

    #[test]
    fn test_risk_color() {
        assert_eq!(risk_color(Some("safe")), "green");
        assert_eq!(risk_color(Some("suspicious")), "yellow");
        assert_eq!(risk_color(Some("malicious")), "red");
        assert_eq!(risk_color(None), "gray");
    }
}
