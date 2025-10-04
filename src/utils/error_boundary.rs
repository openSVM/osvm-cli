//! Error boundary utilities for secure error handling and recovery
//!
//! This module provides comprehensive error boundary handling to prevent
//! information disclosure, ensure graceful degradation, and maintain security
//! during error conditions.

use anyhow::{Context, Result};
use log::{debug, error, warn};
use std::fmt;
use std::panic;

/// Security-aware error boundary that prevents information disclosure
pub struct SecureErrorBoundary {
    debug_mode: bool,
    component_name: String,
}

impl SecureErrorBoundary {
    /// Create a new secure error boundary
    pub fn new(component_name: &str, debug_mode: bool) -> Self {
        Self {
            debug_mode,
            component_name: component_name.to_string(),
        }
    }

    /// Execute a function within an error boundary
    pub async fn execute<F, T>(&self, operation: F) -> Result<T>
    where
        F: std::future::Future<Output = Result<T>> + std::panic::UnwindSafe,
    {
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            tokio::runtime::Handle::current().block_on(async { operation.await })
        }));

        match result {
            Ok(Ok(value)) => Ok(value),
            Ok(Err(error)) => {
                self.handle_error(&error);
                Err(error)
            }
            Err(panic_info) => {
                self.handle_panic(panic_info);
                Err(anyhow::anyhow!("Internal error in {}", self.component_name))
            }
        }
    }

    /// Execute a synchronous function within an error boundary
    pub fn execute_sync<F, T>(&self, operation: F) -> Result<T>
    where
        F: FnOnce() -> Result<T> + std::panic::UnwindSafe,
    {
        let result = panic::catch_unwind(operation);

        match result {
            Ok(Ok(value)) => Ok(value),
            Ok(Err(error)) => {
                self.handle_error(&error);
                Err(error)
            }
            Err(panic_info) => {
                self.handle_panic(panic_info);
                Err(anyhow::anyhow!("Internal error in {}", self.component_name))
            }
        }
    }

    /// Handle errors securely
    fn handle_error(&self, error: &anyhow::Error) {
        if self.debug_mode {
            error!("Error in {}: {:?}", self.component_name, error);
            // In debug mode, include full error chain
            let mut current = error.source();
            while let Some(err) = current {
                debug!("Caused by: {}", err);
                current = err.source();
            }
        } else {
            // In production, log sanitized error information
            let sanitized_error = self.sanitize_error_message(&error.to_string());
            error!("Error in {}: {}", self.component_name, sanitized_error);
        }
    }

    /// Handle panics securely
    fn handle_panic(&self, panic_info: Box<dyn std::any::Any + Send>) {
        let panic_message = if let Some(s) = panic_info.downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = panic_info.downcast_ref::<String>() {
            s.clone()
        } else {
            "Unknown panic".to_string()
        };

        if self.debug_mode {
            error!("Panic in {}: {}", self.component_name, panic_message);
        } else {
            let sanitized_message = self.sanitize_error_message(&panic_message);
            error!(
                "Internal error in {}: {}",
                self.component_name, sanitized_message
            );
        }
    }

    /// Sanitize error messages to prevent information disclosure
    fn sanitize_error_message(&self, message: &str) -> String {
        let mut sanitized = message.to_string();

        // Remove file paths
        sanitized = regex::Regex::new(r"/[^\s]+")
            .unwrap()
            .replace_all(&sanitized, "[PATH_REDACTED]")
            .to_string();

        // Remove potential private keys or tokens
        sanitized = regex::Regex::new(r"[A-Za-z0-9+/=]{32,}")
            .unwrap()
            .replace_all(&sanitized, "[TOKEN_REDACTED]")
            .to_string();

        // Remove IP addresses
        sanitized = regex::Regex::new(r"\b(?:[0-9]{1,3}\.){3}[0-9]{1,3}\b")
            .unwrap()
            .replace_all(&sanitized, "[IP_REDACTED]")
            .to_string();

        // Limit length to prevent log flooding
        if sanitized.len() > 200 {
            sanitized.truncate(197);
            sanitized.push_str("...");
        }

        sanitized
    }
}

/// Circuit breaker for preventing cascading failures
pub struct CircuitBreaker {
    failure_count: std::sync::atomic::AtomicUsize,
    last_failure_time: std::sync::Mutex<Option<std::time::Instant>>,
    failure_threshold: usize,
    timeout_duration: std::time::Duration,
    name: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CircuitState {
    Closed,   // Normal operation
    Open,     // Failures exceeded threshold
    HalfOpen, // Testing if service recovered
}

impl CircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(
        name: &str,
        failure_threshold: usize,
        timeout_duration: std::time::Duration,
    ) -> Self {
        Self {
            failure_count: std::sync::atomic::AtomicUsize::new(0),
            last_failure_time: std::sync::Mutex::new(None),
            failure_threshold,
            timeout_duration,
            name: name.to_string(),
        }
    }

    /// Get current circuit state
    pub fn state(&self) -> CircuitState {
        let failure_count = self
            .failure_count
            .load(std::sync::atomic::Ordering::Relaxed);

        if failure_count < self.failure_threshold {
            return CircuitState::Closed;
        }

        let last_failure = self.last_failure_time.lock().unwrap();
        if let Some(last_time) = *last_failure {
            if last_time.elapsed() > self.timeout_duration {
                CircuitState::HalfOpen
            } else {
                CircuitState::Open
            }
        } else {
            CircuitState::Closed
        }
    }

    /// Execute an operation with circuit breaker protection
    pub async fn call<F, T>(&self, operation: F) -> Result<T>
    where
        F: std::future::Future<Output = Result<T>>,
    {
        match self.state() {
            CircuitState::Open => {
                warn!("Circuit breaker {} is open, rejecting call", self.name);
                return Err(anyhow::anyhow!(
                    "Service temporarily unavailable (circuit breaker open)"
                ));
            }
            CircuitState::HalfOpen => {
                debug!(
                    "Circuit breaker {} is half-open, testing service",
                    self.name
                );
            }
            CircuitState::Closed => {
                debug!("Circuit breaker {} is closed, allowing call", self.name);
            }
        }

        match operation.await {
            Ok(result) => {
                self.on_success();
                Ok(result)
            }
            Err(error) => {
                self.on_failure();
                Err(error)
            }
        }
    }

    /// Record a successful operation
    fn on_success(&self) {
        self.failure_count
            .store(0, std::sync::atomic::Ordering::Relaxed);
        debug!("Circuit breaker {} recorded success", self.name);
    }

    /// Record a failed operation
    fn on_failure(&self) {
        let new_count = self
            .failure_count
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            + 1;
        let mut last_failure = self.last_failure_time.lock().unwrap();
        *last_failure = Some(std::time::Instant::now());

        if new_count >= self.failure_threshold {
            warn!(
                "Circuit breaker {} tripped after {} failures",
                self.name, new_count
            );
        } else {
            debug!(
                "Circuit breaker {} recorded failure {}/{}",
                self.name, new_count, self.failure_threshold
            );
        }
    }

    /// Manually reset the circuit breaker
    pub fn reset(&self) {
        self.failure_count
            .store(0, std::sync::atomic::Ordering::Relaxed);
        let mut last_failure = self.last_failure_time.lock().unwrap();
        *last_failure = None;
        debug!("Circuit breaker {} manually reset", self.name);
    }
}

/// Global error recovery manager
pub struct ErrorRecoveryManager {
    boundaries: std::sync::Mutex<Vec<std::sync::Arc<SecureErrorBoundary>>>,
    circuit_breakers:
        std::sync::Mutex<std::collections::HashMap<String, std::sync::Arc<CircuitBreaker>>>,
}

impl ErrorRecoveryManager {
    /// Create a new error recovery manager
    pub fn new() -> Self {
        Self {
            boundaries: std::sync::Mutex::new(Vec::new()),
            circuit_breakers: std::sync::Mutex::new(std::collections::HashMap::new()),
        }
    }

    /// Register an error boundary
    pub fn register_boundary(&self, boundary: std::sync::Arc<SecureErrorBoundary>) {
        let mut boundaries = self.boundaries.lock().unwrap();
        boundaries.push(boundary);
    }

    /// Get or create a circuit breaker
    pub fn get_circuit_breaker(&self, name: &str) -> std::sync::Arc<CircuitBreaker> {
        let mut breakers = self.circuit_breakers.lock().unwrap();
        breakers
            .entry(name.to_string())
            .or_insert_with(|| {
                std::sync::Arc::new(CircuitBreaker::new(
                    name,
                    5,                                  // 5 failures
                    std::time::Duration::from_secs(60), // 60 second timeout
                ))
            })
            .clone()
    }

    /// Get health status of all circuit breakers
    pub fn health_status(&self) -> std::collections::HashMap<String, CircuitState> {
        let breakers = self.circuit_breakers.lock().unwrap();
        breakers
            .iter()
            .map(|(name, breaker)| (name.clone(), breaker.state()))
            .collect()
    }
}

/// Global error recovery manager instance
lazy_static::lazy_static! {
    static ref GLOBAL_ERROR_MANAGER: ErrorRecoveryManager = ErrorRecoveryManager::new();
}

/// Get the global error recovery manager
pub fn global_error_manager() -> &'static ErrorRecoveryManager {
    &GLOBAL_ERROR_MANAGER
}

/// Macro for creating secure error boundaries
#[macro_export]
macro_rules! secure_boundary {
    ($name:expr, $debug:expr, $operation:expr) => {{
        let boundary = $crate::utils::error_boundary::SecureErrorBoundary::new($name, $debug);
        boundary.execute(async { $operation }).await
    }};
}

/// Macro for circuit breaker protected operations
#[macro_export]
macro_rules! with_circuit_breaker {
    ($name:expr, $operation:expr) => {{
        let manager = $crate::utils::error_boundary::global_error_manager();
        let breaker = manager.get_circuit_breaker($name);
        breaker.call(async { $operation }).await
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_error_boundary_success() {
        let boundary = SecureErrorBoundary::new("test", false);
        let result = boundary
            .execute(async { Ok::<i32, anyhow::Error>(42) })
            .await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[tokio::test]
    async fn test_error_boundary_error() {
        let boundary = SecureErrorBoundary::new("test", false);
        let result = boundary
            .execute(async { Err::<i32, anyhow::Error>(anyhow::anyhow!("test error")) })
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_circuit_breaker() {
        let breaker = CircuitBreaker::new("test", 2, Duration::from_millis(100));

        // Should be closed initially
        assert_eq!(breaker.state(), CircuitState::Closed);

        // First failure
        let result = breaker
            .call(async { Err::<(), anyhow::Error>(anyhow::anyhow!("failure")) })
            .await;
        assert!(result.is_err());
        assert_eq!(breaker.state(), CircuitState::Closed);

        // Second failure - should trip the breaker
        let result = breaker
            .call(async { Err::<(), anyhow::Error>(anyhow::anyhow!("failure")) })
            .await;
        assert!(result.is_err());
        assert_eq!(breaker.state(), CircuitState::Open);

        // Should reject calls while open
        let result = breaker.call(async { Ok::<(), anyhow::Error>(()) }).await;
        assert!(result.is_err());

        // Wait for timeout
        tokio::time::sleep(Duration::from_millis(150)).await;
        assert_eq!(breaker.state(), CircuitState::HalfOpen);
    }

    #[test]
    fn test_error_message_sanitization() {
        let boundary = SecureErrorBoundary::new("test", false);

        let message =
            "Error in /home/user/.ssh/id_rsa with token sk-1234567890abcdef and IP 192.168.1.1";
        let sanitized = boundary.sanitize_error_message(message);

        assert!(!sanitized.contains("/home/user"));
        assert!(!sanitized.contains("sk-1234567890abcdef"));
        assert!(!sanitized.contains("192.168.1.1"));
    }
}
