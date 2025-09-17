//! Granular Circuit Breaker System for AI Services and Analysis Vectors
//!
//! This module provides sophisticated circuit breaker functionality that can be
//! configured per API endpoint and per analysis vector to provide fine-grained
//! resilience control.

use anyhow::Result;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

/// Circuit breaker states
#[derive(Debug, Clone, PartialEq)]
pub enum CircuitState {
    Closed,   // Normal operation
    Open,     // Failing, requests blocked
    HalfOpen, // Testing if service has recovered
}

/// Analysis vector types for granular control
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum AnalysisVector {
    StateTransition,
    EconomicExploit,
    AccessControl,
    MathematicalIntegrity,
    General,
}

/// API endpoint identifier
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct EndpointId {
    pub service: String,  // e.g., "osvm.ai", "openai", "custom"
    pub endpoint: String, // e.g., "/api/getAnswer", "/v1/chat/completions"
}

/// Circuit breaker configuration
#[derive(Debug, Clone)]
pub struct CircuitBreakerConfig {
    /// Number of failures before opening circuit
    pub failure_threshold: u32,
    /// Time window for counting failures
    pub failure_window: Duration,
    /// How long to wait before attempting recovery
    pub recovery_timeout: Duration,
    /// Number of successful requests needed to close circuit
    pub success_threshold: u32,
    /// Maximum concurrent requests in half-open state
    pub half_open_max_calls: u32,
}

/// Circuit breaker statistics
#[derive(Debug, Clone)]
pub struct CircuitStats {
    pub state: CircuitState,
    pub failure_count: u32,
    pub success_count: u32,
    pub last_failure_time: Option<Instant>,
    pub last_success_time: Option<Instant>,
    pub total_requests: u64,
    pub total_failures: u64,
    pub state_changes: u32,
}

/// Individual circuit breaker instance
#[derive(Debug)]
struct CircuitBreakerInstance {
    config: CircuitBreakerConfig,
    stats: CircuitStats,
    failure_times: Vec<Instant>,
    half_open_calls: u32,
}

/// Main circuit breaker manager with granular control
pub struct GranularCircuitBreaker {
    /// Circuit breakers per endpoint
    endpoint_breakers: Arc<RwLock<HashMap<EndpointId, CircuitBreakerInstance>>>,
    /// Circuit breakers per analysis vector
    vector_breakers: Arc<RwLock<HashMap<AnalysisVector, CircuitBreakerInstance>>>,
    /// Global circuit breaker
    global_breaker: Arc<RwLock<CircuitBreakerInstance>>,
    /// Default configuration
    default_config: CircuitBreakerConfig,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            failure_window: Duration::from_secs(5 * 60), // 5 minutes
            recovery_timeout: Duration::from_secs(60),   // 1 minute
            success_threshold: 3,
            half_open_max_calls: 3,
        }
    }
}

impl Default for CircuitStats {
    fn default() -> Self {
        Self {
            state: CircuitState::Closed,
            failure_count: 0,
            success_count: 0,
            last_failure_time: None,
            last_success_time: None,
            total_requests: 0,
            total_failures: 0,
            state_changes: 0,
        }
    }
}

impl CircuitBreakerInstance {
    fn new(config: CircuitBreakerConfig) -> Self {
        Self {
            config,
            stats: CircuitStats::default(),
            failure_times: Vec::new(),
            half_open_calls: 0,
        }
    }

    /// Check if the circuit should allow a request
    fn can_execute(&mut self) -> bool {
        self.update_state();

        match self.stats.state {
            CircuitState::Closed => true,
            CircuitState::Open => false,
            CircuitState::HalfOpen => {
                if self.half_open_calls < self.config.half_open_max_calls {
                    self.half_open_calls += 1;
                    true
                } else {
                    false
                }
            }
        }
    }

    /// Record a successful request
    fn on_success(&mut self) {
        self.stats.total_requests += 1;
        self.stats.success_count += 1;
        self.stats.last_success_time = Some(Instant::now());

        match self.stats.state {
            CircuitState::HalfOpen => {
                if self.stats.success_count >= self.config.success_threshold {
                    self.close_circuit();
                }
            }
            CircuitState::Open => {
                // This shouldn't happen, but if it does, close the circuit
                self.close_circuit();
            }
            CircuitState::Closed => {
                // Reset failure count on success
                self.stats.failure_count = 0;
                self.failure_times.clear();
            }
        }
    }

    /// Record a failed request
    fn on_failure(&mut self) {
        self.stats.total_requests += 1;
        self.stats.total_failures += 1;
        self.stats.failure_count += 1;
        let now = Instant::now();
        self.stats.last_failure_time = Some(now);
        self.failure_times.push(now);

        // Clean old failures outside the window
        self.failure_times
            .retain(|&time| now.duration_since(time) <= self.config.failure_window);

        // Check if we should open the circuit
        if self.failure_times.len() >= self.config.failure_threshold as usize {
            self.open_circuit();
        }

        // Reset success count on failure in half-open state
        if self.stats.state == CircuitState::HalfOpen {
            self.stats.success_count = 0;
        }
    }

    /// Update circuit state based on time and conditions
    fn update_state(&mut self) {
        match self.stats.state {
            CircuitState::Open => {
                if let Some(last_failure) = self.stats.last_failure_time {
                    if Instant::now().duration_since(last_failure) >= self.config.recovery_timeout {
                        self.half_open_circuit();
                    }
                }
            }
            CircuitState::Closed | CircuitState::HalfOpen => {
                // Clean old failures
                let now = Instant::now();
                self.failure_times
                    .retain(|&time| now.duration_since(time) <= self.config.failure_window);

                if self.failure_times.is_empty() {
                    self.stats.failure_count = 0;
                }
            }
        }
    }

    fn open_circuit(&mut self) {
        if self.stats.state != CircuitState::Open {
            println!("🚫 Circuit breaker OPENED - blocking requests");
            self.stats.state = CircuitState::Open;
            self.stats.state_changes += 1;
            self.stats.success_count = 0;
            self.half_open_calls = 0;
        }
    }

    fn half_open_circuit(&mut self) {
        if self.stats.state != CircuitState::HalfOpen {
            println!("🔄 Circuit breaker HALF-OPEN - testing recovery");
            self.stats.state = CircuitState::HalfOpen;
            self.stats.state_changes += 1;
            self.stats.success_count = 0;
            self.half_open_calls = 0;
        }
    }

    fn close_circuit(&mut self) {
        if self.stats.state != CircuitState::Closed {
            println!("✅ Circuit breaker CLOSED - normal operation");
            self.stats.state = CircuitState::Closed;
            self.stats.state_changes += 1;
            self.stats.failure_count = 0;
            self.stats.success_count = 0;
            self.failure_times.clear();
            self.half_open_calls = 0;
        }
    }
}

impl GranularCircuitBreaker {
    pub fn new() -> Self {
        Self {
            endpoint_breakers: Arc::new(RwLock::new(HashMap::new())),
            vector_breakers: Arc::new(RwLock::new(HashMap::new())),
            global_breaker: Arc::new(RwLock::new(CircuitBreakerInstance::new(
                CircuitBreakerConfig::default(),
            ))),
            default_config: CircuitBreakerConfig::default(),
        }
    }

    /// Create with custom default configuration
    pub fn with_config(config: CircuitBreakerConfig) -> Self {
        Self {
            endpoint_breakers: Arc::new(RwLock::new(HashMap::new())),
            vector_breakers: Arc::new(RwLock::new(HashMap::new())),
            global_breaker: Arc::new(RwLock::new(CircuitBreakerInstance::new(config.clone()))),
            default_config: config,
        }
    }

    /// Configure circuit breaker for a specific endpoint
    pub fn configure_endpoint(&self, endpoint: EndpointId, config: CircuitBreakerConfig) {
        let mut breakers = self.endpoint_breakers.write().unwrap();
        breakers.insert(endpoint, CircuitBreakerInstance::new(config));
    }

    /// Configure circuit breaker for a specific analysis vector
    pub fn configure_vector(&self, vector: AnalysisVector, config: CircuitBreakerConfig) {
        let mut breakers = self.vector_breakers.write().unwrap();
        breakers.insert(vector, CircuitBreakerInstance::new(config));
    }

    /// Check if request can execute for endpoint and vector
    pub fn can_execute_endpoint(&self, endpoint: &EndpointId) -> bool {
        // Check global circuit first
        if !self.can_execute_global() {
            return false;
        }

        let mut breakers = self.endpoint_breakers.write().unwrap();

        // Get or create circuit breaker for this endpoint
        let breaker = breakers
            .entry(endpoint.clone())
            .or_insert_with(|| CircuitBreakerInstance::new(self.default_config.clone()));

        breaker.can_execute()
    }

    /// Check if request can execute for analysis vector
    pub fn can_execute_vector(&self, vector: &AnalysisVector) -> bool {
        // Check global circuit first
        if !self.can_execute_global() {
            return false;
        }

        let mut breakers = self.vector_breakers.write().unwrap();

        // Get or create circuit breaker for this vector
        let breaker = breakers
            .entry(vector.clone())
            .or_insert_with(|| CircuitBreakerInstance::new(self.default_config.clone()));

        breaker.can_execute()
    }

    /// Check global circuit breaker
    pub fn can_execute_global(&self) -> bool {
        let mut global = self.global_breaker.write().unwrap();
        global.can_execute()
    }

    /// Record success for endpoint
    pub fn on_success_endpoint(&self, endpoint: &EndpointId) {
        self.on_success_global();

        let mut breakers = self.endpoint_breakers.write().unwrap();
        if let Some(breaker) = breakers.get_mut(endpoint) {
            breaker.on_success();
        }
    }

    /// Record success for analysis vector
    pub fn on_success_vector(&self, vector: &AnalysisVector) {
        self.on_success_global();

        let mut breakers = self.vector_breakers.write().unwrap();
        if let Some(breaker) = breakers.get_mut(vector) {
            breaker.on_success();
        }
    }

    /// Record global success
    pub fn on_success_global(&self) {
        let mut global = self.global_breaker.write().unwrap();
        global.on_success();
    }

    /// Record failure for endpoint
    pub fn on_failure_endpoint(&self, endpoint: &EndpointId) {
        self.on_failure_global();

        let mut breakers = self.endpoint_breakers.write().unwrap();
        if let Some(breaker) = breakers.get_mut(endpoint) {
            breaker.on_failure();
        }
    }

    /// Record failure for analysis vector
    pub fn on_failure_vector(&self, vector: &AnalysisVector) {
        self.on_failure_global();

        let mut breakers = self.vector_breakers.write().unwrap();
        if let Some(breaker) = breakers.get_mut(vector) {
            breaker.on_failure();
        }
    }

    /// Record global failure
    pub fn on_failure_global(&self) {
        let mut global = self.global_breaker.write().unwrap();
        global.on_failure();
    }

    /// Get statistics for endpoint
    pub fn get_endpoint_stats(&self, endpoint: &EndpointId) -> Option<CircuitStats> {
        let breakers = self.endpoint_breakers.read().unwrap();
        breakers.get(endpoint).map(|b| b.stats.clone())
    }

    /// Get statistics for analysis vector
    pub fn get_vector_stats(&self, vector: &AnalysisVector) -> Option<CircuitStats> {
        let breakers = self.vector_breakers.read().unwrap();
        breakers.get(vector).map(|b| b.stats.clone())
    }

    /// Get global statistics
    pub fn get_global_stats(&self) -> CircuitStats {
        let global = self.global_breaker.read().unwrap();
        global.stats.clone()
    }

    /// Get comprehensive status report
    pub fn get_status_report(&self) -> CircuitBreakerReport {
        let global_stats = self.get_global_stats();

        let endpoint_stats: HashMap<EndpointId, CircuitStats> = {
            let breakers = self.endpoint_breakers.read().unwrap();
            breakers
                .iter()
                .map(|(k, v)| (k.clone(), v.stats.clone()))
                .collect()
        };

        let vector_stats: HashMap<AnalysisVector, CircuitStats> = {
            let breakers = self.vector_breakers.read().unwrap();
            breakers
                .iter()
                .map(|(k, v)| (k.clone(), v.stats.clone()))
                .collect()
        };

        CircuitBreakerReport {
            global_stats,
            endpoint_stats,
            vector_stats,
        }
    }

    /// Reset all circuit breakers
    pub fn reset_all(&self) {
        // Reset global
        {
            let mut global = self.global_breaker.write().unwrap();
            *global = CircuitBreakerInstance::new(self.default_config.clone());
        }

        // Reset endpoints
        {
            let mut breakers = self.endpoint_breakers.write().unwrap();
            for breaker in breakers.values_mut() {
                *breaker = CircuitBreakerInstance::new(self.default_config.clone());
            }
        }

        // Reset vectors
        {
            let mut breakers = self.vector_breakers.write().unwrap();
            for breaker in breakers.values_mut() {
                *breaker = CircuitBreakerInstance::new(self.default_config.clone());
            }
        }

        println!("🔄 All circuit breakers have been reset");
    }
}

/// Comprehensive status report
#[derive(Debug)]
pub struct CircuitBreakerReport {
    pub global_stats: CircuitStats,
    pub endpoint_stats: HashMap<EndpointId, CircuitStats>,
    pub vector_stats: HashMap<AnalysisVector, CircuitStats>,
}

impl CircuitBreakerReport {
    pub fn print_summary(&self) {
        println!("\n📊 Circuit Breaker Status Report");
        println!("================================");

        println!("\n🌐 Global Circuit: {:?}", self.global_stats.state);
        println!("   Total Requests: {}", self.global_stats.total_requests);
        println!("   Total Failures: {}", self.global_stats.total_failures);
        println!("   State Changes: {}", self.global_stats.state_changes);

        if !self.endpoint_stats.is_empty() {
            println!("\n🔗 Endpoint Circuits:");
            for (endpoint, stats) in &self.endpoint_stats {
                println!(
                    "   {}/{}: {:?} ({}req, {}fail)",
                    endpoint.service,
                    endpoint.endpoint,
                    stats.state,
                    stats.total_requests,
                    stats.total_failures
                );
            }
        }

        if !self.vector_stats.is_empty() {
            println!("\n🧮 Analysis Vector Circuits:");
            for (vector, stats) in &self.vector_stats {
                println!(
                    "   {:?}: {:?} ({}req, {}fail)",
                    vector, stats.state, stats.total_requests, stats.total_failures
                );
            }
        }
    }
}

impl Default for GranularCircuitBreaker {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper trait for creating endpoint IDs
impl EndpointId {
    pub fn new(service: &str, endpoint: &str) -> Self {
        Self {
            service: service.to_string(),
            endpoint: endpoint.to_string(),
        }
    }

    pub fn osvm_ai() -> Self {
        Self::new("osvm.ai", "/api/getAnswer")
    }

    pub fn openai() -> Self {
        Self::new("openai", "/v1/chat/completions")
    }

    pub fn custom(service: &str, endpoint: &str) -> Self {
        Self::new(service, endpoint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_circuit_breaker_states() {
        let breaker = GranularCircuitBreaker::new();
        let endpoint = EndpointId::osvm_ai();

        // Should start closed
        assert!(breaker.can_execute_endpoint(&endpoint));

        // Simulate failures
        for _ in 0..5 {
            breaker.on_failure_endpoint(&endpoint);
        }

        // Should be open now
        assert!(!breaker.can_execute_endpoint(&endpoint));

        // Check stats
        let stats = breaker.get_endpoint_stats(&endpoint).unwrap();
        assert_eq!(stats.state, CircuitState::Open);
        assert_eq!(stats.total_failures, 5);
    }

    #[test]
    fn test_analysis_vector_circuit() {
        let breaker = GranularCircuitBreaker::new();
        let vector = AnalysisVector::EconomicExploit;

        assert!(breaker.can_execute_vector(&vector));

        // Cause failures
        for _ in 0..5 {
            breaker.on_failure_vector(&vector);
        }

        assert!(!breaker.can_execute_vector(&vector));
    }

    #[test]
    fn test_half_open_recovery() {
        let mut config = CircuitBreakerConfig::default();
        config.recovery_timeout = Duration::from_millis(100);

        let breaker = GranularCircuitBreaker::with_config(config);
        let endpoint = EndpointId::osvm_ai();

        // Open the circuit
        for _ in 0..5 {
            breaker.on_failure_endpoint(&endpoint);
        }
        assert!(!breaker.can_execute_endpoint(&endpoint));

        // Wait for recovery timeout
        thread::sleep(Duration::from_millis(150));

        // Should be half-open now and allow limited requests
        assert!(breaker.can_execute_endpoint(&endpoint));

        // Successful requests should close the circuit
        for _ in 0..3 {
            breaker.on_success_endpoint(&endpoint);
        }

        let stats = breaker.get_endpoint_stats(&endpoint).unwrap();
        assert_eq!(stats.state, CircuitState::Closed);
    }
}
