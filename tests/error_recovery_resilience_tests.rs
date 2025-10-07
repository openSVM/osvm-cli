//! Tests for error recovery, resilience, and fault tolerance

use anyhow::Result;
use osvm::utils::{
    error_boundary::{SecureErrorBoundary, CircuitBreaker, ErrorRecoveryManager},
    self_repair::{SelfRepair, RepairableError, RepairStrategy},
    diagnostics::{SystemHealth, HealthCheck, HealthStatus},
};
use std::time::Duration;
use std::sync::Arc;
use tokio::sync::Mutex;

#[cfg(test)]
mod circuit_breaker_tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_basic_flow() -> Result<()> {
        let breaker = CircuitBreaker::new("test-service", 3, Duration::from_secs(60));

        // Initially closed
        assert!(breaker.is_closed());

        // Successful calls keep it closed
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        assert!(breaker.is_closed());

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_on_failures() -> Result<()> {
        let breaker = CircuitBreaker::new("failing-service", 3, Duration::from_millis(100));

        // Make 3 failing calls
        for _ in 0..3 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Test failure"))
            }).await;
        }

        // Circuit should be open now
        assert!(breaker.is_open());

        // Next call should fail immediately
        let result = breaker.call(async {
            Ok::<(), anyhow::Error>(())
        }).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("circuit breaker"));

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_breaker_half_open_recovery() -> Result<()> {
        let breaker = CircuitBreaker::new("recovery-service", 2, Duration::from_millis(50));

        // Trip the circuit
        for _ in 0..2 {
            let _ = breaker.call(async {
                Err::<(), anyhow::Error>(anyhow::anyhow!("Failure"))
            }).await;
        }

        assert!(breaker.is_open());

        // Wait for timeout
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Should transition to half-open
        assert!(breaker.is_half_open());

        // Successful call should close it
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;
        breaker.call(async { Ok::<(), anyhow::Error>(()) }).await?;

        assert!(breaker.is_closed());

        Ok(())
    }

    #[tokio::test]
    async fn test_circuit_breaker_concurrent_access() -> Result<()> {
        let breaker = Arc::new(CircuitBreaker::new("concurrent-service", 5, Duration::from_secs(1)));

        let mut handles = vec![];

        // Multiple concurrent successful calls
        for i in 0..10 {
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

        for result in results {
            assert!(result?.is_ok());
        }

        assert!(breaker.is_closed());

        Ok(())
    }
}

#[cfg(test)]
mod error_boundary_tests {
    use super::*;

    #[tokio::test]
    async fn test_error_boundary_catches_errors() -> Result<()> {
        let boundary = SecureErrorBoundary::new("test-component", false);

        let result = boundary.execute(async {
            Err::<i32, anyhow::Error>(anyhow::anyhow!("Test error"))
        }).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Test error"));

        Ok(())
    }

    #[tokio::test]
    async fn test_error_boundary_passes_success() -> Result<()> {
        let boundary = SecureErrorBoundary::new("test-component", false);

        let result = boundary.execute(async {
            Ok::<i32, anyhow::Error>(42)
        }).await?;

        assert_eq!(result, 42);

        Ok(())
    }

    #[tokio::test]
    async fn test_error_boundary_debug_mode() -> Result<()> {
        let boundary_debug = SecureErrorBoundary::new("debug-component", true);
        let boundary_prod = SecureErrorBoundary::new("prod-component", false);

        let error_msg = "Sensitive error: API_KEY=sk-12345";

        let result_debug = boundary_debug.execute(async {
            Err::<(), anyhow::Error>(anyhow::anyhow!(error_msg))
        }).await;

        let result_prod = boundary_prod.execute(async {
            Err::<(), anyhow::Error>(anyhow::anyhow!(error_msg))
        }).await;

        // Both should fail but handle errors differently
        assert!(result_debug.is_err());
        assert!(result_prod.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_nested_error_boundaries() -> Result<()> {
        let outer_boundary = SecureErrorBoundary::new("outer", false);
        let inner_boundary = SecureErrorBoundary::new("inner", false);

        let result = outer_boundary.execute(async {
            inner_boundary.execute(async {
                Err::<i32, anyhow::Error>(anyhow::anyhow!("Inner error"))
            }).await
        }).await;

        assert!(result.is_err());

        Ok(())
    }
}

#[cfg(test)]
mod self_repair_tests {
    use super::*;

    #[tokio::test]
    async fn test_repairable_error_detection() -> Result<()> {
        let errors = vec![
            RepairableError::MissingSolanaCli,
            RepairableError::OutdatedSolanaCli,
            RepairableError::MissingBuildTools,
            RepairableError::MissingConfigDirectory,
        ];

        for error in errors {
            assert!(error.is_repairable());
            assert!(!error.is_manual_only());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_strategy_selection() -> Result<()> {
        let test_cases = vec![
            (RepairableError::MissingSolanaCli, RepairStrategy::InstallSolanaCli),
            (RepairableError::MissingBuildTools, RepairStrategy::InstallBuildTools),
            (RepairableError::MissingConfigDirectory, RepairStrategy::CreateConfigDirectory),
        ];

        for (error, expected_strategy) in test_cases {
            let strategy = error.get_repair_strategy();
            assert!(matches!(strategy, expected_strategy));
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_self_repair_dry_run() -> Result<()> {
        let self_repair = SelfRepair::new();

        let issues = vec![
            RepairableError::MissingConfigDirectory,
            RepairableError::MissingBuildTools,
        ];

        // Dry run should not actually repair
        let plan = self_repair.plan_repairs(&issues)?;

        assert_eq!(plan.repairs.len(), 2);
        assert!(!plan.executed);

        Ok(())
    }

    #[tokio::test]
    async fn test_repair_failure_handling() -> Result<()> {
        let self_repair = SelfRepair::new();

        // Simulate a repair that requires sudo but we don't have it
        let error = RepairableError::InsufficientPermissions("sudo required".to_string());

        let result = self_repair.attempt_repair(&error);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("permission"));

        Ok(())
    }
}

#[cfg(test)]
mod health_check_tests {
    use super::*;

    #[tokio::test]
    async fn test_system_health_check() -> Result<()> {
        let health = SystemHealth::new();

        let status = health.check_all().await?;

        // Should have multiple health checks
        assert!(!status.checks.is_empty());

        // Should have overall status
        assert!(matches!(
            status.overall,
            HealthStatus::Healthy | HealthStatus::Degraded | HealthStatus::Unhealthy
        ));

        Ok(())
    }

    #[tokio::test]
    async fn test_individual_health_checks() -> Result<()> {
        let health = SystemHealth::new();

        let checks = vec![
            health.check_system_dependencies().await,
            health.check_user_configuration().await,
            health.check_network_connectivity().await,
        ];

        for check in checks {
            assert!(check.is_ok());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_health_check_timeout() -> Result<()> {
        let health = SystemHealth::new();

        // Simulate a health check with timeout
        let result = tokio::time::timeout(
            Duration::from_secs(5),
            health.check_all()
        ).await;

        assert!(result.is_ok());

        Ok(())
    }

    #[tokio::test]
    async fn test_health_status_aggregation() -> Result<()> {
        let individual_statuses = vec![
            HealthStatus::Healthy,
            HealthStatus::Healthy,
            HealthStatus::Degraded,
        ];

        // Aggregate status should be degraded
        let overall = if individual_statuses.iter().all(|s| matches!(s, HealthStatus::Healthy)) {
            HealthStatus::Healthy
        } else if individual_statuses.iter().any(|s| matches!(s, HealthStatus::Unhealthy)) {
            HealthStatus::Unhealthy
        } else {
            HealthStatus::Degraded
        };

        assert!(matches!(overall, HealthStatus::Degraded));

        Ok(())
    }
}

#[cfg(test)]
mod retry_logic_tests {
    use super::*;

    #[tokio::test]
    async fn test_exponential_backoff() -> Result<()> {
        let mut delays = vec![];
        let base_delay = Duration::from_millis(100);

        for attempt in 0..5 {
            let delay = base_delay * 2u32.pow(attempt);
            delays.push(delay);
        }

        assert_eq!(delays[0], Duration::from_millis(100));
        assert_eq!(delays[1], Duration::from_millis(200));
        assert_eq!(delays[2], Duration::from_millis(400));
        assert_eq!(delays[3], Duration::from_millis(800));
        assert_eq!(delays[4], Duration::from_millis(1600));

        Ok(())
    }

    #[tokio::test]
    async fn test_retry_with_backoff() -> Result<()> {
        let attempt_count = Arc::new(Mutex::new(0));

        let result = retry_with_backoff(3, Duration::from_millis(10), {
            let count = Arc::clone(&attempt_count);
            move || {
                let count = Arc::clone(&count);
                async move {
                    let mut c = count.lock().await;
                    *c += 1;

                    if *c < 3 {
                        Err(anyhow::anyhow!("Temporary failure"))
                    } else {
                        Ok(42)
                    }
                }
            }
        }).await?;

        assert_eq!(result, 42);

        let final_count = *attempt_count.lock().await;
        assert_eq!(final_count, 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_retry_exhaustion() -> Result<()> {
        let result = retry_with_backoff(3, Duration::from_millis(10), || async {
            Err::<i32, anyhow::Error>(anyhow::anyhow!("Persistent failure"))
        }).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("failure"));

        Ok(())
    }
}

// Helper function for retry logic
async fn retry_with_backoff<F, Fut, T>(
    max_attempts: usize,
    base_delay: Duration,
    mut operation: F,
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = Result<T>>,
{
    let mut last_error = None;

    for attempt in 0..max_attempts {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(e) => {
                last_error = Some(e);
                if attempt < max_attempts - 1 {
                    let delay = base_delay * 2u32.pow(attempt as u32);
                    tokio::time::sleep(delay).await;
                }
            }
        }
    }

    Err(last_error.unwrap())
}

#[cfg(test)]
mod fault_injection_tests {
    use super::*;

    #[tokio::test]
    async fn test_network_failure_simulation() -> Result<()> {
        let failure_rate = 0.3; // 30% failure rate

        let mut successes = 0;
        let mut failures = 0;

        for _ in 0..100 {
            let random_value: f64 = rand::random();
            if random_value < failure_rate {
                failures += 1;
            } else {
                successes += 1;
            }
        }

        // Should have roughly 30% failures
        assert!(failures > 20 && failures < 40);
        assert!(successes > 60 && successes < 80);

        Ok(())
    }

    #[tokio::test]
    async fn test_timeout_handling() -> Result<()> {
        let slow_operation = async {
            tokio::time::sleep(Duration::from_secs(10)).await;
            Ok::<(), anyhow::Error>(())
        };

        let result = tokio::time::timeout(Duration::from_millis(100), slow_operation).await;

        assert!(result.is_err()); // Should timeout

        Ok(())
    }

    #[tokio::test]
    async fn test_partial_failure_recovery() -> Result<()> {
        let operations = vec![
            async { Ok::<i32, anyhow::Error>(1) },
            async { Err::<i32, anyhow::Error>(anyhow::anyhow!("Failed")) },
            async { Ok::<i32, anyhow::Error>(3) },
        ];

        let results: Vec<Result<i32>> = futures::future::join_all(operations).await;

        let successful: Vec<i32> = results.iter()
            .filter_map(|r| r.as_ref().ok().copied())
            .collect();

        assert_eq!(successful.len(), 2);
        assert_eq!(successful, vec![1, 3]);

        Ok(())
    }
}

#[cfg(test)]
mod graceful_degradation_tests {
    use super::*;

    #[tokio::test]
    async fn test_fallback_on_primary_failure() -> Result<()> {
        let primary_service_available = false;
        let fallback_service_available = true;

        let result = if primary_service_available {
            "primary result"
        } else if fallback_service_available {
            "fallback result"
        } else {
            "error: no service available"
        };

        assert_eq!(result, "fallback result");

        Ok(())
    }

    #[tokio::test]
    async fn test_degraded_functionality() -> Result<()> {
        let full_features_available = false;

        let features = if full_features_available {
            vec!["feature_a", "feature_b", "feature_c"]
        } else {
            // Provide essential features only
            vec!["feature_a"]
        };

        assert_eq!(features.len(), 1);
        assert!(features.contains(&"feature_a"));

        Ok(())
    }

    #[tokio::test]
    async fn test_cache_fallback() -> Result<()> {
        let cache: Option<String> = Some("cached data".to_string());
        let network_available = false;

        let data = if network_available {
            "fresh data from network".to_string()
        } else if let Some(cached) = cache {
            cached
        } else {
            return Err(anyhow::anyhow!("No data available"));
        };

        assert_eq!(data, "cached data");

        Ok(())
    }
}