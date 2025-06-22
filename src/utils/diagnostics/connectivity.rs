//! Network connectivity diagnostics
//!
//! This module provides network connectivity testing capabilities
//! for Solana endpoints and general internet connectivity.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tokio::time::timeout;

use super::{CheckResult, DiagnosticError, NetworkHealth};

/// Solana network endpoints
const SOLANA_ENDPOINTS: &[(&str, &str)] = &[
    ("mainnet", "https://api.mainnet-beta.solana.com"),
    ("testnet", "https://api.testnet.solana.com"),
    ("devnet", "https://api.devnet.solana.com"),
];

/// Connectivity test configuration
#[derive(Debug, Clone)]
pub struct ConnectivityConfig {
    pub timeout_seconds: u64,
    pub max_retries: u32,
    pub test_internet: bool,
    pub test_solana_endpoints: bool,
}

impl Default for ConnectivityConfig {
    fn default() -> Self {
        Self {
            timeout_seconds: 10,
            max_retries: 3,
            test_internet: true,
            test_solana_endpoints: true,
        }
    }
}

/// Network test result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkTestResult {
    pub endpoint: String,
    pub accessible: bool,
    pub response_time_ms: Option<u64>,
    pub error_message: Option<String>,
    pub status_code: Option<u16>,
}

/// Check overall network health
pub async fn check_network_health() -> Result<NetworkHealth, DiagnosticError> {
    let config = ConnectivityConfig::default();
    check_network_health_with_config(&config).await
}

/// Check network health with custom configuration
pub async fn check_network_health_with_config(
    config: &ConnectivityConfig,
) -> Result<NetworkHealth, DiagnosticError> {
    let mut response_times = HashMap::new();

    // Test general internet connectivity
    let internet_connected = if config.test_internet {
        test_internet_connectivity(config).await
    } else {
        true // Assume connected if not testing
    };

    // Test Solana endpoints
    let (mainnet_accessible, testnet_accessible, devnet_accessible) =
        if config.test_solana_endpoints {
            let results = test_solana_endpoints(config).await;

            // Store response times
            for result in &results {
                if let Some(response_time) = result.response_time_ms {
                    response_times.insert(result.endpoint.clone(), response_time);
                }
            }

            let mainnet = results
                .iter()
                .find(|r| r.endpoint.contains("mainnet"))
                .map(|r| r.accessible)
                .unwrap_or(false);
            let testnet = results
                .iter()
                .find(|r| r.endpoint.contains("testnet"))
                .map(|r| r.accessible)
                .unwrap_or(false);
            let devnet = results
                .iter()
                .find(|r| r.endpoint.contains("devnet"))
                .map(|r| r.accessible)
                .unwrap_or(false);

            (mainnet, testnet, devnet)
        } else {
            (true, true, true) // Assume accessible if not testing
        };

    Ok(NetworkHealth {
        internet_connected,
        solana_mainnet_accessible: mainnet_accessible,
        solana_testnet_accessible: testnet_accessible,
        solana_devnet_accessible: devnet_accessible,
        response_times,
    })
}

/// Test internet connectivity using reliable endpoints
async fn test_internet_connectivity(config: &ConnectivityConfig) -> bool {
    let test_endpoints = [
        "https://1.1.1.1", // Cloudflare DNS
        "https://8.8.8.8", // Google DNS
        "https://google.com",
    ];

    for endpoint in &test_endpoints {
        if test_http_endpoint(endpoint, config).await.accessible {
            return true;
        }
    }

    false
}

/// Test all Solana endpoints
async fn test_solana_endpoints(config: &ConnectivityConfig) -> Vec<NetworkTestResult> {
    let mut results = Vec::new();

    for (network, endpoint) in SOLANA_ENDPOINTS {
        let test_url = format!("{}/health", endpoint);
        let mut result = test_http_endpoint(&test_url, config).await;
        result.endpoint = format!("{} ({})", network, endpoint);
        results.push(result);
    }

    results
}

/// Test a specific HTTP endpoint
async fn test_http_endpoint(url: &str, config: &ConnectivityConfig) -> NetworkTestResult {
    let timeout_duration = Duration::from_secs(config.timeout_seconds);

    for attempt in 1..=config.max_retries {
        let start_time = Instant::now();

        match timeout(timeout_duration, make_http_request(url)).await {
            Ok(Ok(response)) => {
                let response_time = start_time.elapsed().as_millis() as u64;

                return NetworkTestResult {
                    endpoint: url.to_string(),
                    accessible: response.status.is_success(),
                    response_time_ms: Some(response_time),
                    error_message: if response.status.is_success() {
                        None
                    } else {
                        Some(format!("HTTP {}", response.status.as_u16()))
                    },
                    status_code: Some(response.status.as_u16()),
                };
            }
            Ok(Err(e)) => {
                if attempt == config.max_retries {
                    return NetworkTestResult {
                        endpoint: url.to_string(),
                        accessible: false,
                        response_time_ms: None,
                        error_message: Some(format!("Request failed: {}", e)),
                        status_code: None,
                    };
                }
                // Retry on error
            }
            Err(_) => {
                if attempt == config.max_retries {
                    return NetworkTestResult {
                        endpoint: url.to_string(),
                        accessible: false,
                        response_time_ms: None,
                        error_message: Some("Request timeout".to_string()),
                        status_code: None,
                    };
                }
                // Retry on timeout
            }
        }

        // Small delay between retries
        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    // Should not reach here due to the loop structure, but just in case
    NetworkTestResult {
        endpoint: url.to_string(),
        accessible: false,
        response_time_ms: None,
        error_message: Some("All retry attempts failed".to_string()),
        status_code: None,
    }
}

/// Make an HTTP request (using reqwest or similar)
async fn make_http_request(
    url: &str,
) -> Result<HttpResponse, Box<dyn std::error::Error + Send + Sync>> {
    // For now, we'll use a simple approach with curl since we don't want to add reqwest dependency
    // In a real implementation, you'd use reqwest or another HTTP client

    let output = tokio::process::Command::new("curl")
        .arg("-s")
        .arg("-I") // HEAD request
        .arg("--max-time")
        .arg("10")
        .arg(url)
        .output()
        .await?;

    if output.status.success() {
        let response_text = String::from_utf8_lossy(&output.stdout);

        // Parse HTTP status from curl response
        if let Some(status_line) = response_text.lines().next() {
            if let Some(status_code_str) = status_line.split_whitespace().nth(1) {
                if let Ok(status_code) = status_code_str.parse::<u16>() {
                    return Ok(HttpResponse {
                        status: HttpStatus::from_code(status_code),
                    });
                }
            }
        }

        // Default to 200 if we can't parse but curl succeeded
        Ok(HttpResponse {
            status: HttpStatus::from_code(200),
        })
    } else {
        Err(format!("Curl failed: {}", String::from_utf8_lossy(&output.stderr)).into())
    }
}

/// Simple HTTP response representation
#[derive(Debug)]
struct HttpResponse {
    status: HttpStatus,
}

/// Simple HTTP status representation
#[derive(Debug)]
struct HttpStatus {
    code: u16,
}

impl HttpStatus {
    fn from_code(code: u16) -> Self {
        Self { code }
    }

    fn as_u16(&self) -> u16 {
        self.code
    }

    fn is_success(&self) -> bool {
        self.code >= 200 && self.code < 300
    }
}

/// Check Solana endpoints for detailed diagnostics
pub async fn check_solana_endpoints() -> CheckResult {
    let start = Instant::now();
    let config = ConnectivityConfig::default();

    match test_solana_endpoints(&config).await {
        results => {
            let accessible_count = results.iter().filter(|r| r.accessible).count();
            let total_count = results.len();

            let passed = accessible_count == total_count;
            let message = if passed {
                "All Solana endpoints are accessible".to_string()
            } else {
                format!(
                    "{}/{} Solana endpoints accessible",
                    accessible_count, total_count
                )
            };

            let details = results
                .iter()
                .map(|r| {
                    format!(
                        "{}: {} ({}ms)",
                        r.endpoint,
                        if r.accessible { "✅" } else { "❌" },
                        r.response_time_ms
                            .map(|t| t.to_string())
                            .unwrap_or_else(|| "timeout".to_string())
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");

            CheckResult {
                name: "Solana Endpoints".to_string(),
                passed,
                message,
                details: Some(details),
                execution_time_ms: start.elapsed().as_millis() as u64,
            }
        }
    }
}

/// Test RPC functionality on Solana endpoints
pub async fn test_solana_rpc_functionality() -> Result<Vec<NetworkTestResult>, DiagnosticError> {
    let config = ConnectivityConfig::default();
    let mut results = Vec::new();

    for (network, endpoint) in SOLANA_ENDPOINTS {
        // Test getHealth RPC call
        let rpc_test_url = endpoint;
        let rpc_body = r#"{"jsonrpc":"2.0","id":1,"method":"getHealth"}"#;

        let start_time = Instant::now();

        // Use curl to make RPC call
        let output = tokio::process::Command::new("curl")
            .arg("-s")
            .arg("-X")
            .arg("POST")
            .arg("-H")
            .arg("Content-Type: application/json")
            .arg("-d")
            .arg(rpc_body)
            .arg("--max-time")
            .arg("10")
            .arg(rpc_test_url)
            .output()
            .await;

        let response_time = start_time.elapsed().as_millis() as u64;

        match output {
            Ok(output) if output.status.success() => {
                let response_text = String::from_utf8_lossy(&output.stdout);
                let accessible = response_text.contains("result") || response_text.contains("ok");

                results.push(NetworkTestResult {
                    endpoint: format!("{} RPC ({})", network, endpoint),
                    accessible,
                    response_time_ms: Some(response_time),
                    error_message: if accessible {
                        None
                    } else {
                        Some("RPC call failed".to_string())
                    },
                    status_code: Some(200), // Assume 200 if curl succeeded
                });
            }
            Ok(output) => {
                let error_msg = String::from_utf8_lossy(&output.stderr);
                results.push(NetworkTestResult {
                    endpoint: format!("{} RPC ({})", network, endpoint),
                    accessible: false,
                    response_time_ms: None,
                    error_message: Some(format!("Curl failed: {}", error_msg)),
                    status_code: None,
                });
            }
            Err(e) => {
                results.push(NetworkTestResult {
                    endpoint: format!("{} RPC ({})", network, endpoint),
                    accessible: false,
                    response_time_ms: None,
                    error_message: Some(format!("Command failed: {}", e)),
                    status_code: None,
                });
            }
        }
    }

    Ok(results)
}

/// Get network latency to Solana endpoints
pub async fn measure_network_latency() -> Result<HashMap<String, u64>, DiagnosticError> {
    let config = ConnectivityConfig::default();
    let results = test_solana_endpoints(&config).await;

    let latency_map = results
        .iter()
        .filter_map(|r| r.response_time_ms.map(|time| (r.endpoint.clone(), time)))
        .collect();

    Ok(latency_map)
}

/// Check if we're behind a firewall or proxy
pub async fn check_firewall_restrictions() -> CheckResult {
    let start = Instant::now();

    // Test both HTTP and HTTPS
    let http_test = test_http_endpoint(
        "http://httpbin.org/status/200",
        &ConnectivityConfig::default(),
    )
    .await;
    let https_test = test_http_endpoint(
        "https://httpbin.org/status/200",
        &ConnectivityConfig::default(),
    )
    .await;

    // Test different ports commonly blocked by firewalls
    let port_tests = vec![
        ("HTTP (80)", "http://httpbin.org/status/200"),
        ("HTTPS (443)", "https://httpbin.org/status/200"),
        (
            "Alternative HTTP (8080)",
            "http://httpbin.org:8080/status/200",
        ),
    ];

    let mut accessible_protocols = Vec::new();
    let mut blocked_protocols = Vec::new();

    for (name, url) in port_tests {
        let result = test_http_endpoint(url, &ConnectivityConfig::default()).await;
        if result.accessible {
            accessible_protocols.push(name);
        } else {
            blocked_protocols.push(name);
        }
    }

    let passed = !accessible_protocols.is_empty();
    let message = if passed {
        "Network connectivity appears normal".to_string()
    } else {
        "Possible firewall restrictions detected".to_string()
    };

    let details = format!(
        "Accessible: {:?}\nBlocked: {:?}",
        accessible_protocols, blocked_protocols
    );

    CheckResult {
        name: "Firewall/Proxy Check".to_string(),
        passed,
        message,
        details: Some(details),
        execution_time_ms: start.elapsed().as_millis() as u64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connectivity_config_default() {
        let config = ConnectivityConfig::default();
        assert_eq!(config.timeout_seconds, 10);
        assert_eq!(config.max_retries, 3);
        assert!(config.test_internet);
        assert!(config.test_solana_endpoints);
    }

    #[test]
    fn test_http_status() {
        let status = HttpStatus::from_code(200);
        assert_eq!(status.as_u16(), 200);
        assert!(status.is_success());

        let error_status = HttpStatus::from_code(404);
        assert_eq!(error_status.as_u16(), 404);
        assert!(!error_status.is_success());
    }

    #[test]
    fn test_network_test_result_creation() {
        let result = NetworkTestResult {
            endpoint: "https://example.com".to_string(),
            accessible: true,
            response_time_ms: Some(100),
            error_message: None,
            status_code: Some(200),
        };

        assert!(result.accessible);
        assert_eq!(result.response_time_ms, Some(100));
        assert_eq!(result.status_code, Some(200));
    }

    #[tokio::test]
    async fn test_solana_endpoints_list() {
        // Test that our endpoint list is not empty and contains expected networks
        assert!(!SOLANA_ENDPOINTS.is_empty());

        let networks: Vec<&str> = SOLANA_ENDPOINTS
            .iter()
            .map(|(network, _)| *network)
            .collect();
        assert!(networks.contains(&"mainnet"));
        assert!(networks.contains(&"testnet"));
        assert!(networks.contains(&"devnet"));
    }
}
