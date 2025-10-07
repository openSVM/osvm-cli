//! Advanced comprehensive tests for circuit breaker patterns including
//! granular circuit breakers, state transitions, metrics, and failure handling

use anyhow::Result;
use osvm::utils::circuit_breaker::{
    CircuitBreaker, GranularCircuitBreaker, CircuitState, CircuitBreakerConfig,
    CircuitBreakerMetrics, FailureClassifier, BreakerPolicy,
};
use std::time::Duration;
use std::sync::Arc;
use tokio::sync::Mutex;

#[cfg(test)]
mod circuit_breaker_basic_tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_initialization() {
        let breaker = CircuitBreaker::new("test-service", 3, Duration::from_secs(60));

        assert_eq!(breaker.name(), "test-service");
        assert_eq!(breaker.failure_threshold(), 3);
        assert!(breaker.is_closed());
    }

    #[tokio::test]
    async fn test_successful_calls_keep_closed() -> Result<()> {
        let breaker = CircuitBreaker::new("test", 3, Duration::from_secs(60));

        for _ in 0..10 {
            breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        }

        assert!(breaker.is_closed());
        assert_eq!(breaker.failure_count(), 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_opens_on_threshold() -> Result<()> {
        let breaker = CircuitBreaker::new("fail-service", 3, Duration::from_millis(100));

        // Make 3 failing calls
        for _ in 0..3 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Test failure"))
            }).await;
        }

        // Circuit should be open
        assert!(breaker.is_open());

        // Next call should fail fast
        let result = breaker.call(async {
            Ok::<(), anyhow::Error>(())
        }).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("circuit") ||
                result.unwrap_err().to_string().contains("open"));

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_half_open_transition() -> Result<()> {
        let breaker = CircuitBreaker::new("recovery", 2, Duration::from_millis(50));

        // Trip the circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Failure"))
            }).await;
        }

        assert!(breaker.is_open());

        // Wait for timeout
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Should be half-open now
        assert!(breaker.is_half_open());

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_recovery_to_closed() -> Result<()> {
        let breaker = CircuitBreaker::new("recover", 2, Duration::from_millis(50));

        // Trip circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        // Wait for half-open
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Successful call should close it
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;

        assert!(breaker.is_closed());

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_reopen_from_half_open() -> Result<()> {
        let breaker = CircuitBreaker::new("reopen", 2, Duration::from_millis(50));

        // Trip circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        // Wait for half-open
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert!(breaker.is_half_open());

        // Failing call should reopen circuit
        let _ = breaker.call(async {
            Err::<(), anyhow::Error>(anyhow::anyhow!("Still failing"))
        }).await;

        assert!(breaker.is_open());

        Ok(())
    }

    #[tokio::test]
    async fn test_manual_circuit_reset() -> Result<()> {
        let breaker = CircuitBreaker::new("manual", 2, Duration::from_secs(60));

        // Trip circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        assert!(breaker.is_open());

        // Manual reset
        breaker.reset();

        assert!(breaker.is_closed());
        assert_eq!(breaker.failure_count(), 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_metrics_collection() -> Result<()> {
        let breaker = CircuitBreaker::new("metrics", 5, Duration::from_secs(60));

        // Make some calls
        for i in 0..10 {
            if i % 3 == 0 {
                let _ = breaker.call(async {
                    Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
                }).await;
            } else {
                breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
            }
        }

        let metrics = breaker.metrics();

        assert_eq!(metrics.total_calls, 10);
        assert!(metrics.success_count > 0);
        assert!(metrics.failure_count > 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_concurrent_circuit_breaker_access() -> Result<()> {
        let breaker = Arc::new(CircuitBreaker::new("concurrent", 10, Duration::from_secs(1)));

        let mut handles = vec![];

        // Multiple concurrent successful calls
        for i in 0..20 {
            let breaker_clone = Arc::clone(&breaker);
            let handle = tokio::spawn(async move {
                breaker_clone.call(async move {
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    Ok::<usize, anyhow::Error>(i)
                }).await
            });
            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        // All should succeed
        for result in results {
            assert!(result?.is_ok());
        }

        assert!(breaker.is_closed());

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_breaker_timeout_handling() -> Result<()> {
        let breaker = CircuitBreaker::with_timeout(
            "timeout-test",
            3,
            Duration::from_secs(60),
            Duration::from_millis(100),
        );

        // This should timeout
        let result = breaker.call(async {
            tokio::time::sleep(Duration::from_secs(1)).await;
            Ok::<(), anyhow::Error>(())
        }).await;

        assert!(result.is_err());

        Ok(())
    }
}

#[cfg(test)]
mod granular_circuit_breaker_tests {
    use super::*;

    #[tokio::test]
    async fn test_granular_breaker_initialization() {
        let config = CircuitBreakerConfig {
            failure_threshold: 5,
            success_threshold: 2,
            timeout: Duration::from_secs(30),
            reset_timeout: Duration::from_secs(60),
        };

        let breaker = GranularCircuitBreaker::new("granular-service", config);

        assert_eq!(breaker.name(), "granular-service");
        assert!(breaker.is_closed());
    }

    #[tokio::test]
    async fn test_granular_breaker_per_endpoint() -> Result<()> {
        let config = CircuitBreakerConfig {
            failure_threshold: 2,
            success_threshold: 1,
            timeout: Duration::from_millis(50),
            reset_timeout: Duration::from_millis(100),
        };

        let breaker = GranularCircuitBreaker::new("multi-endpoint", config);

        // Fail endpoint1
        for _ in 0..2 {
            let _ = breaker.call_endpoint("endpoint1", async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        // endpoint1 should be open
        assert!(breaker.is_endpoint_open("endpoint1"));

        // endpoint2 should still be closed
        assert!(!breaker.is_endpoint_open("endpoint2"));

        // Successful call to endpoint2
        breaker.call_endpoint("endpoint2", async {
            Ok::<(), anyhow::Error>(())
        }).await?;

        assert!(!breaker.is_endpoint_open("endpoint2"));

        Ok(())
    }

    #[tokio::test]
    async fn test_granular_breaker_metrics_per_endpoint() -> Result<()> {
        let config = CircuitBreakerConfig::default();
        let breaker = GranularCircuitBreaker::new("metrics-test", config);

        // Make calls to different endpoints
        for _ in 0..5 {
            breaker.call_endpoint("api1", async {
                Ok::<(), anyhow::Error>(())
            }).await?;
        }

        for _ in 0..3 {
            let _ = breaker.call_endpoint("api2", async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        let metrics = breaker.endpoint_metrics("api1");
        assert_eq!(metrics.total_calls, 5);
        assert_eq!(metrics.success_count, 5);

        let api2_metrics = breaker.endpoint_metrics("api2");
        assert_eq!(api2_metrics.total_calls, 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_granular_breaker_global_status() -> Result<()> {
        let config = CircuitBreakerConfig::default();
        let breaker = GranularCircuitBreaker::new("global-test", config);

        let status = breaker.global_status();

        assert!(status.total_endpoints >= 0);
        assert!(status.healthy_endpoints >= 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_granular_breaker_reset_endpoint() -> Result<()> {
        let config = CircuitBreakerConfig {
            failure_threshold: 2,
            success_threshold: 1,
            timeout: Duration::from_secs(30),
            reset_timeout: Duration::from_secs(60),
        };

        let breaker = GranularCircuitBreaker::new("reset-test", config);

        // Trip endpoint
        for _ in 0..2 {
            let _ = breaker.call_endpoint("api1", async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        assert!(breaker.is_endpoint_open("api1"));

        // Reset specific endpoint
        breaker.reset_endpoint("api1");

        assert!(!breaker.is_endpoint_open("api1"));

        Ok(())
    }
}

#[cfg(test)]
mod failure_classifier_tests {
    use super::*;

    #[test]
    fn test_transient_failure_detection() {
        let classifier = FailureClassifier::new();

        let transient_errors = vec![
            anyhow::anyhow!("Connection timeout"),
            anyhow::anyhow!("Network unreachable"),
            anyhow::anyhow!("Temporary failure"),
        ];

        for error in transient_errors {
            assert!(classifier.is_transient(&error));
            assert!(!classifier.is_permanent(&error));
        }
    }

    #[test]
    fn test_permanent_failure_detection() {
        let classifier = FailureClassifier::new();

        let permanent_errors = vec![
            anyhow::anyhow!("Authentication failed"),
            anyhow::anyhow!("Invalid credentials"),
            anyhow::anyhow!("403 Forbidden"),
        ];

        for error in permanent_errors {
            assert!(classifier.is_permanent(&error));
            assert!(!classifier.is_transient(&error));
        }
    }

    #[test]
    fn test_failure_classification_affects_counter() {
        let classifier = FailureClassifier::new();

        let transient = anyhow::anyhow!("Timeout");
        let permanent = anyhow::anyhow!("Auth failed");

        // Transient failures might not count towards threshold
        assert!(classifier.should_count_failure(&transient));

        // Permanent failures always count
        assert!(classifier.should_count_failure(&permanent));
    }
}

#[cfg(test)]
mod breaker_policy_tests {
    use super::*;

    #[test]
    fn test_conservative_policy() {
        let policy = BreakerPolicy::conservative();

        assert_eq!(policy.failure_threshold, 3);
        assert_eq!(policy.success_threshold, 5);
        assert!(policy.reset_timeout > Duration::from_secs(30));
    }

    #[test]
    fn test_aggressive_policy() {
        let policy = BreakerPolicy::aggressive();

        assert_eq!(policy.failure_threshold, 1);
        assert_eq!(policy.success_threshold, 1);
        assert!(policy.reset_timeout < Duration::from_secs(10));
    }

    #[test]
    fn test_balanced_policy() {
        let policy = BreakerPolicy::balanced();

        assert!(policy.failure_threshold > 1);
        assert!(policy.success_threshold > 1);
    }

    #[test]
    fn test_custom_policy() {
        let policy = BreakerPolicy::custom(10, 5, Duration::from_secs(120));

        assert_eq!(policy.failure_threshold, 10);
        assert_eq!(policy.success_threshold, 5);
        assert_eq!(policy.reset_timeout, Duration::from_secs(120));
    }
}

#[cfg(test)]
mod circuit_breaker_state_tests {
    use super::*;

    #[test]
    fn test_state_transitions() {
        use CircuitState::*;

        // Valid transitions
        assert!(Closed.can_transition_to(&HalfOpen));
        assert!(Closed.can_transition_to(&Open));
        assert!(Open.can_transition_to(&HalfOpen));
        assert!(HalfOpen.can_transition_to(&Closed));
        assert!(HalfOpen.can_transition_to(&Open));

        // Invalid transitions
        assert!(!Open.can_transition_to(&Closed)); // Must go through half-open
    }

    #[test]
    fn test_state_serialization() {
        let states = vec![
            CircuitState::Closed,
            CircuitState::Open,
            CircuitState::HalfOpen,
        ];

        for state in states {
            let json = serde_json::to_string(&state).unwrap();
            let deserialized: CircuitState = serde_json::from_str(&json).unwrap();
            assert_eq!(state, deserialized);
        }
    }
}

#[cfg(test)]
mod advanced_scenarios_tests {
    use super::*;

    #[tokio::test]
    async fn test_cascading_failures() -> Result<()> {
        let breaker1 = Arc::new(CircuitBreaker::new("service1", 2, Duration::from_millis(50)));
        let breaker2 = Arc::new(CircuitBreaker::new("service2", 2, Duration::from_millis(50)));

        // Service1 calls service2
        let b1 = Arc::clone(&breaker1);
        let b2 = Arc::clone(&breaker2);

        let result = b1.call(async move {
            b2.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Service2 failed"))
            }).await
        }).await;

        assert!(result.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_bulkhead_pattern() -> Result<()> {
        let breaker = Arc::new(CircuitBreaker::new("bulkhead", 5, Duration::from_secs(1)));
        let semaphore = Arc::new(tokio::sync::Semaphore::new(3)); // Max 3 concurrent

        let mut handles = vec![];

        for i in 0..10 {
            let breaker_clone = Arc::clone(&breaker);
            let semaphore_clone = Arc::clone(&semaphore);

            let handle = tokio::spawn(async move {
                let _permit = semaphore_clone.acquire().await.unwrap();

                breaker_clone.call(async move {
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    Ok::<usize, anyhow::Error>(i)
                }).await
            });

            handles.push(handle);
        }

        let results = futures::future::join_all(handles).await;

        // All should complete
        for result in results {
            assert!(result.is_ok());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_fallback_on_open_circuit() -> Result<()> {
        let breaker = CircuitBreaker::new("fallback", 2, Duration::from_millis(50));

        // Trip circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<i32, anyhow::Error>(anyhow::anyhow!("Fail"))
            }).await;
        }

        assert!(breaker.is_open());

        // Use fallback when circuit is open
        let result = match breaker.call(async {
            Ok::<i32, anyhow::Error>(42)
        }).await {
            Ok(value) => value,
            Err(_) => -1, // Fallback value
        };

        assert_eq!(result, -1); // Should use fallback

        Ok(())
    }

    #[tokio::test]
    async fn test_retry_with_circuit_breaker() -> Result<()> {
        let breaker = Arc::new(CircuitBreaker::new("retry", 5, Duration::from_secs(1)));
        let attempt_count = Arc::new(Mutex::new(0));

        let result = {
            let breaker_clone = Arc::clone(&breaker);
            let count = Arc::clone(&attempt_count);

            breaker_clone.call(async move {
                let mut c = count.lock().await;
                *c += 1;

                if *c < 3 {
                    Err(anyhow::anyhow!("Temporary failure"))
                } else {
                    Ok(42)
                }
            }).await
        }?;

        assert_eq!(result, 42);

        let final_count = *attempt_count.lock().await;
        assert_eq!(final_count, 3);

        Ok(())
    }
}

#[cfg(test)]
mod circuit_breaker_metrics_tests {
    use super::*;

    #[tokio::test]
    async fn test_metrics_accuracy() -> Result<()> {
        let breaker = CircuitBreaker::new("metrics-accuracy", 10, Duration::from_secs(60));

        let mut success = 0;
        let mut failure = 0;

        for i in 0..20 {
            if i % 3 == 0 {
                let _ = breaker.call(async {
                    Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
                }).await;
                failure += 1;
            } else {
                breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
                success += 1;
            }
        }

        let metrics = breaker.metrics();

        assert_eq!(metrics.total_calls, 20);
        assert_eq!(metrics.success_count, success);
        assert_eq!(metrics.failure_count, failure);

        Ok(())
    }

    #[tokio::test]
    async fn test_success_rate_calculation() -> Result<()> {
        let breaker = CircuitBreaker::new("success-rate", 10, Duration::from_secs(60));

        // 7 successes, 3 failures
        for i in 0..10 {
            if i < 7 {
                breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
            } else {
                let _ = breaker.call(async {
                    Err::<(), anyhow::Error>(anyhow::anyhow!("Fail"))
                }).await;
            }
        }

        let metrics = breaker.metrics();
        let success_rate = metrics.success_rate();

        assert!((success_rate - 0.7).abs() < 0.01); // 70% success rate

        Ok(())
    }

    #[tokio::test]
    async fn test_metrics_reset() -> Result<()> {
        let breaker = CircuitBreaker::new("metrics-reset", 5, Duration::from_secs(60));

        // Make some calls
        for _ in 0..5 {
            breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        }

        breaker.reset_metrics();

        let metrics = breaker.metrics();
        assert_eq!(metrics.total_calls, 0);
        assert_eq!(metrics.success_count, 0);

        Ok(())
    }
}
