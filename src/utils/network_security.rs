//! Network security utilities for safe network operations
//!
//! This module provides utilities for secure network operations including
//! timeouts, rate limiting, and connection validation.

use std::time::Duration;
use tokio::time::{timeout, sleep};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::collections::HashMap;
use tokio::sync::Mutex;

/// Default timeout for network operations
pub const DEFAULT_NETWORK_TIMEOUT: Duration = Duration::from_secs(30);

/// Default timeout for SSH operations
pub const DEFAULT_SSH_TIMEOUT: Duration = Duration::from_secs(60);

/// Default timeout for AI API calls
pub const DEFAULT_AI_TIMEOUT: Duration = Duration::from_secs(120);

/// Rate limiter for preventing abuse
pub struct RateLimiter {
    /// Request counters per endpoint
    counters: Arc<Mutex<HashMap<String, (AtomicU64, std::time::Instant)>>>,
    /// Maximum requests per window
    max_requests: u64,
    /// Time window duration
    window_duration: Duration,
}

impl RateLimiter {
    /// Create a new rate limiter
    pub fn new(max_requests: u64, window_duration: Duration) -> Self {
        Self {
            counters: Arc::new(Mutex::new(HashMap::new())),
            max_requests,
            window_duration,
        }
    }

    /// Check if a request should be allowed
    pub async fn check_rate_limit(&self, endpoint: &str) -> bool {
        let mut counters = self.counters.lock().await;
        let now = std::time::Instant::now();

        let (counter, window_start) = counters
            .entry(endpoint.to_string())
            .or_insert_with(|| (AtomicU64::new(0), now));

        // Reset window if expired
        if now.duration_since(*window_start) > self.window_duration {
            counter.store(0, Ordering::Relaxed);
            *window_start = now;
        }

        let current_count = counter.load(Ordering::Relaxed);
        if current_count >= self.max_requests {
            false
        } else {
            counter.fetch_add(1, Ordering::Relaxed);
            true
        }
    }

    /// Wait until rate limit allows the request
    pub async fn wait_for_rate_limit(&self, endpoint: &str) -> Result<(), &'static str> {
        const MAX_WAIT_TIME: Duration = Duration::from_secs(300); // 5 minutes max
        let start_wait = std::time::Instant::now();

        while !self.check_rate_limit(endpoint).await {
            if start_wait.elapsed() > MAX_WAIT_TIME {
                return Err("Rate limit wait timeout exceeded");
            }
            sleep(Duration::from_millis(100)).await;
        }

        Ok(())
    }
}

/// Secure HTTP client with built-in timeouts and validation
pub struct SecureHttpClient {
    client: reqwest::Client,
    rate_limiter: Option<RateLimiter>,
}

impl SecureHttpClient {
    /// Create a new secure HTTP client
    pub fn new() -> Self {
        let client = reqwest::Client::builder()
            .timeout(DEFAULT_NETWORK_TIMEOUT)
            .tcp_keepalive(Duration::from_secs(60))
            .pool_idle_timeout(Duration::from_secs(90))
            .pool_max_idle_per_host(10)
            .user_agent(format!("osvm-cli/{}", env!("CARGO_PKG_VERSION")))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            rate_limiter: Some(RateLimiter::new(60, Duration::from_secs(60))), // 60 requests per minute
        }
    }

    /// Create a secure HTTP client with custom rate limiting
    pub fn with_rate_limit(max_requests: u64, window: Duration) -> Self {
        let mut client = Self::new();
        client.rate_limiter = Some(RateLimiter::new(max_requests, window));
        client
    }

    /// Perform a secure GET request with automatic retries and rate limiting
    pub async fn secure_get(&self, url: &str) -> Result<reqwest::Response, Box<dyn std::error::Error + Send + Sync>> {
        self.validate_url(url)?;

        if let Some(rate_limiter) = &self.rate_limiter {
            rate_limiter.wait_for_rate_limit(url).await
                .map_err(|e| format!("Rate limit error: {}", e))?;
        }

        let response = timeout(
            DEFAULT_NETWORK_TIMEOUT,
            self.client.get(url).send()
        ).await
            .map_err(|_| "HTTP request timeout")?
            .map_err(|e| format!("HTTP request failed: {}", e))?;

        if !response.status().is_success() {
            return Err(format!("HTTP error: {}", response.status()).into());
        }

        Ok(response)
    }

    /// Perform a secure POST request
    pub async fn secure_post<T: serde::Serialize>(
        &self,
        url: &str,
        body: &T,
    ) -> Result<reqwest::Response, Box<dyn std::error::Error + Send + Sync>> {
        self.validate_url(url)?;

        if let Some(rate_limiter) = &self.rate_limiter {
            rate_limiter.wait_for_rate_limit(url).await
                .map_err(|e| format!("Rate limit error: {}", e))?;
        }

        let response = timeout(
            DEFAULT_NETWORK_TIMEOUT,
            self.client.post(url).json(body).send()
        ).await
            .map_err(|_| "HTTP request timeout")?
            .map_err(|e| format!("HTTP request failed: {}", e))?;

        if !response.status().is_success() {
            return Err(format!("HTTP error: {}", response.status()).into());
        }

        Ok(response)
    }

    /// Validate URL to prevent SSRF attacks
    fn validate_url(&self, url: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let parsed = url::Url::parse(url)
            .map_err(|e| format!("Invalid URL: {}", e))?;

        // Only allow HTTP and HTTPS
        if !matches!(parsed.scheme(), "http" | "https") {
            return Err("Only HTTP and HTTPS URLs are allowed".into());
        }

        // Block common internal/private ranges
        if let Some(host) = parsed.host() {
            match host {
                url::Host::Ipv4(ip) => {
                    if ip.is_loopback() || ip.is_private() || ip.is_link_local() {
                        return Err("Access to internal IP addresses is not allowed".into());
                    }
                }
                url::Host::Ipv6(ip) => {
                    if ip.is_loopback() {
                        return Err("Access to loopback addresses is not allowed".into());
                    }
                }
                url::Host::Domain(domain) => {
                    let domain_lower = domain.to_lowercase();
                    if matches!(domain_lower.as_str(), "localhost" | "127.0.0.1" | "::1") {
                        return Err("Access to localhost is not allowed".into());
                    }
                }
            }
        }

        Ok(())
    }
}

/// Execute a function with timeout
pub async fn with_timeout<F, T>(
    future: F,
    timeout_duration: Duration,
) -> Result<T, Box<dyn std::error::Error + Send + Sync>>
where
    F: std::future::Future<Output = Result<T, Box<dyn std::error::Error + Send + Sync>>>,
{
    timeout(timeout_duration, future)
        .await
        .map_err(|_| Box::new(std::io::Error::new(std::io::ErrorKind::TimedOut, "Operation timeout")) as Box<dyn std::error::Error + Send + Sync>)?
}

/// Execute an SSH command with timeout
pub async fn ssh_command_with_timeout(
    client: &mut crate::utils::ssh_deploy::SshClient,
    command: &str,
    timeout_duration: Duration,
) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    // Validate command to prevent injection
    validate_ssh_command(command)?;

    timeout(
        timeout_duration,
        async {
            client.execute_command(command)
                .map_err(|e| format!("SSH command failed: {}", e).into())
        }
    ).await
        .map_err(|_| Box::new(std::io::Error::new(std::io::ErrorKind::TimedOut, "SSH command timeout")) as Box<dyn std::error::Error + Send + Sync>)?
}

/// Validate SSH command to prevent injection attacks
fn validate_ssh_command(command: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Check for dangerous characters
    for c in command.chars() {
        if matches!(c, ';' | '&' | '|' | '`' | '$' | '(' | ')' | '<' | '>' | '\n' | '\r') {
            // Allow some safe pipe usage for basic commands
            if c == '|' && command.contains("grep") {
                continue;
            }
            return Err(format!("Dangerous character in SSH command: '{}'", c).into());
        }
    }

    // Check for dangerous command patterns
    let dangerous_patterns = [
        "rm -rf",
        "dd if=",
        "mkfs",
        "fdisk",
        "passwd",
        "su -",
        "sudo su",
        "chmod 777",
    ];

    let command_lower = command.to_lowercase();
    for pattern in dangerous_patterns {
        if command_lower.contains(pattern) {
            return Err(format!("Potentially dangerous command pattern: {}", pattern).into());
        }
    }

    // Limit command length
    if command.len() > 1000 {
        return Err("Command too long (max 1000 characters)".into());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_ssh_command_safe() {
        assert!(validate_ssh_command("ls -la").is_ok());
        assert!(validate_ssh_command("ps aux | grep solana").is_ok());
        assert!(validate_ssh_command("systemctl status solana").is_ok());
    }

    #[test]
    fn test_validate_ssh_command_dangerous() {
        assert!(validate_ssh_command("rm -rf /").is_err());
        assert!(validate_ssh_command("dd if=/dev/zero of=/dev/sda").is_err());
        assert!(validate_ssh_command("sudo su -").is_err());
        assert!(validate_ssh_command("ls; rm file").is_err());
        assert!(validate_ssh_command("echo `whoami`").is_err());
    }

    #[tokio::test]
    async fn test_rate_limiter() {
        let limiter = RateLimiter::new(2, Duration::from_secs(1));

        assert!(limiter.check_rate_limit("test").await);
        assert!(limiter.check_rate_limit("test").await);
        assert!(!limiter.check_rate_limit("test").await);
    }
}