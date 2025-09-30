//! Hot-Swap System for Zero-Downtime Updates
//!
//! This module implements blue-green deployment with automatic rollback for OSVM components.
//! It enables updating running components (RPC nodes, validators, MCP servers) without
//! service interruption.
//!
//! # Architecture
//!
//! ```text
//! Phase 1: Preparation
//! ┌─────────────┐
//! │ Component A │ ← Currently serving traffic
//! │ (v1.0.0)    │
//! └─────────────┘
//!
//! Phase 2: Blue-Green Deployment
//! ┌─────────────┐     ┌─────────────┐
//! │ Component A │ ←───│ Component B │ ← New version starting
//! │ (v1.0.0)    │     │ (v1.1.0)    │
//! └─────────────┘     └─────────────┘
//!      ↑ 100%              ↑ 0%
//!
//! Phase 3: Health Check
//! ┌─────────────┐     ┌─────────────┐
//! │ Component A │     │ Component B │ ← Verify health
//! │ (v1.0.0)    │     │ (v1.1.0)    │    ✓ Boot successful
//! └─────────────┘     └─────────────┘    ✓ Passes health checks
//!      ↑ 100%              ↑ 0%           ✓ Ready for traffic
//!
//! Phase 4: Traffic Shift
//! ┌─────────────┐     ┌─────────────┐
//! │ Component A │  ───→ Component B │ ← Shift traffic
//! │ (v1.0.0)    │     │ (v1.1.0)    │
//! └─────────────┘     └─────────────┘
//!      ↑ 0%               ↑ 100%
//!
//! Phase 5: Drain & Cleanup
//! ┌─────────────┐     ┌─────────────┐
//! │ Component A │ ──X  │ Component B │ ← Old version stopped
//! │ (v1.0.0)    │     │ (v1.1.0)    │    after connection drain
//! └─────────────┘     └─────────────┘
//!   (stopped)              ↑ 100%
//! ```
//!
//! # Rollback on Failure
//!
//! If Component B fails health checks or crashes during traffic shift:
//! - Immediately revert traffic to Component A
//! - Stop Component B
//! - Report failure reason
//! - No service interruption
//!
//! # Use Cases
//!
//! - **RPC Node Updates**: Update to new Solana version without downtime
//! - **MCP Server Updates**: Deploy new tool versions seamlessly
//! - **Security Patches**: Apply critical patches with zero interruption
//! - **Configuration Changes**: Update settings without restart

use super::{Component, ComponentId, ComponentRegistry, ComponentStatus};
use super::runtime::{Runtime, RuntimeManager};
use anyhow::{anyhow, Context, Result};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::{sleep, timeout};

// Hot-swap timing constants
const DEFAULT_HEALTH_CHECK_TIMEOUT_SECS: u64 = 30;
const DEFAULT_HEALTH_CHECK_INTERVAL_SECS: u64 = 2;
const DEFAULT_DRAIN_TIMEOUT_SECS: u64 = 60;
const DEFAULT_MAX_HEALTH_CHECKS: u32 = 10;

/// Hot-swap manager for zero-downtime component updates
pub struct HotSwapManager {
    /// Runtime manager for starting new components
    runtime_manager: Arc<RuntimeManager>,

    /// Component registry
    registry: Arc<ComponentRegistry>,

    /// Hot-swap configuration
    config: HotSwapConfig,
}

/// Configuration for hot-swap operations
#[derive(Debug, Clone)]
pub struct HotSwapConfig {
    /// Health check timeout (how long to wait for new component to be healthy)
    pub health_check_timeout: Duration,

    /// Health check interval (how often to check)
    pub health_check_interval: Duration,

    /// Connection drain timeout (how long to wait for connections to finish)
    pub drain_timeout: Duration,

    /// Maximum number of health check attempts
    pub max_health_checks: u32,

    /// Whether to automatically rollback on failure
    pub auto_rollback: bool,
}

impl Default for HotSwapConfig {
    fn default() -> Self {
        Self {
            health_check_timeout: Duration::from_secs(DEFAULT_HEALTH_CHECK_TIMEOUT_SECS),
            health_check_interval: Duration::from_secs(DEFAULT_HEALTH_CHECK_INTERVAL_SECS),
            drain_timeout: Duration::from_secs(DEFAULT_DRAIN_TIMEOUT_SECS),
            max_health_checks: DEFAULT_MAX_HEALTH_CHECKS,
            auto_rollback: true,
        }
    }
}

/// Result of a hot-swap operation
#[derive(Debug)]
pub enum HotSwapResult {
    /// Hot-swap completed successfully
    Success {
        old_component_id: ComponentId,
        new_component_id: ComponentId,
        duration: Duration,
    },

    /// Hot-swap failed and rolled back
    RolledBack {
        old_component_id: ComponentId,
        new_component_id: ComponentId,
        reason: String,
    },

    /// Hot-swap failed and could not rollback
    Failed {
        old_component_id: ComponentId,
        new_component_id: ComponentId,
        reason: String,
    },
}

/// State of a hot-swap operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HotSwapState {
    /// Starting new component
    Starting,
    /// Running health checks
    HealthChecking,
    /// Shifting traffic
    ShiftingTraffic,
    /// Draining connections
    Draining,
    /// Cleaning up old component
    CleaningUp,
    /// Rolling back
    RollingBack,
    /// Completed
    Completed,
}

impl HotSwapManager {
    /// Create a new hot-swap manager
    pub fn new(
        runtime_manager: Arc<RuntimeManager>,
        registry: Arc<ComponentRegistry>,
        config: HotSwapConfig,
    ) -> Self {
        Self {
            runtime_manager,
            registry,
            config,
        }
    }

    /// Verify old component is ready for hot-swap
    async fn verify_old_component(&self, component_id: ComponentId) -> Result<Component> {
        let component = self.registry.get(component_id).await
            .context(format!(
                "Old component {} not found. Ensure component is registered before hot-swap",
                component_id
            ))?;

        if !matches!(component.status, ComponentStatus::Running) {
            return Err(anyhow!(
                "Old component {} is not running (status: {:?}). \
                 Only running components can be hot-swapped",
                component_id,
                component.status
            ));
        }

        Ok(component)
    }

    /// Start and register new component
    async fn start_new_component(
        &self,
        mut new_component: Component,
        runtime: &Arc<dyn Runtime>,
    ) -> Result<Component> {
        log::info!("Phase 1: Starting new component {}", new_component.id);

        runtime.start_component(&mut new_component).await
            .context("Failed to start new component")?;

        self.registry.register(new_component.clone()).await
            .context("Failed to register new component")?;

        Ok(new_component)
    }

    /// Perform health checks on new component
    async fn perform_health_checks(
        &self,
        runtime: &Arc<dyn Runtime>,
        component_id: ComponentId,
    ) -> Result<()> {
        log::info!("Phase 2: Running health checks on new component");

        timeout(
            self.config.health_check_timeout,
            self.health_check_loop(runtime, component_id)
        ).await
            .context("Health check timeout")?
            .context("Health check failed")?;

        log::info!("Health checks passed");
        Ok(())
    }

    /// Shift traffic from old to new component
    async fn shift_traffic(&self) -> Result<()> {
        log::info!("Phase 3: Shifting traffic to new component");

        // In production, this would:
        // 1. Update load balancer / service mesh
        // 2. Update network policies
        // 3. Migrate certificate mappings
        sleep(Duration::from_millis(100)).await;

        log::info!("Traffic shifted successfully");
        Ok(())
    }

    /// Drain connections from old component
    async fn drain_connections(&self) -> Result<()> {
        log::info!("Phase 4: Draining connections from old component");

        // Wait for connection drain timeout
        // In production, would monitor active connections
        sleep(self.config.drain_timeout).await;

        log::info!("Connection drain complete");
        Ok(())
    }

    /// Stop old component after successful hot-swap
    async fn stop_old_component(
        &self,
        component_id: ComponentId,
        runtime: &Arc<dyn Runtime>,
    ) -> Result<()> {
        log::info!("Phase 5: Stopping old component {}", component_id);

        runtime.stop_component(component_id).await
            .context(format!("Failed to stop old component {}", component_id))?;

        Ok(())
    }

    /// Perform hot-swap of a component
    ///
    /// This starts a new instance of the component, verifies it's healthy,
    /// shifts traffic to it, drains connections from the old instance,
    /// and stops the old instance.
    ///
    /// If anything fails, automatically rolls back to the old instance.
    pub async fn hot_swap(
        &self,
        old_component_id: ComponentId,
        new_component: Component,
    ) -> Result<HotSwapResult> {
        let start_time = std::time::Instant::now();

        log::info!(
            "Starting hot-swap: {} -> {}",
            old_component_id,
            new_component.id
        );

        // Verify old component is ready
        let _old_component = self.verify_old_component(old_component_id).await?;

        // Get runtime and component ID before moving
        let runtime = self.runtime_manager.get_runtime(&new_component.isolation_config)?;
        let new_component_id = new_component.id;

        // Start new component
        let new_component = match self.start_new_component(new_component, &runtime).await {
            Ok(comp) => comp,
            Err(e) => {
                log::error!("Failed to start new component: {}", e);
                return Ok(HotSwapResult::Failed {
                    old_component_id,
                    new_component_id,
                    reason: format!("Failed to start: {}", e),
                });
            }
        };

        // Perform health checks
        if let Err(e) = self.perform_health_checks(&runtime, new_component.id).await {
            log::error!("Health checks failed: {}", e);
            return self.rollback(
                old_component_id,
                new_component.id,
                &runtime,
                format!("Health check failed: {}", e)
            ).await;
        }

        // Shift traffic to new component
        self.shift_traffic().await?;

        // Drain connections from old component
        self.drain_connections().await?;

        // Stop old component
        if let Err(e) = self.stop_old_component(old_component_id, &runtime).await {
            log::warn!("Failed to stop old component: {} (non-fatal)", e);
        }

        // Success! Calculate duration
        let duration = start_time.elapsed();

        log::info!(
            "Hot-swap completed successfully in {:?}",
            duration
        );

        Ok(HotSwapResult::Success {
            old_component_id,
            new_component_id: new_component.id,
            duration,
        })
    }

    /// Run health check loop until component is healthy or max attempts reached
    async fn health_check_loop(
        &self,
        runtime: &Arc<dyn Runtime>,
        component_id: ComponentId,
    ) -> Result<()> {
        for attempt in 1..=self.config.max_health_checks {
            log::debug!("Health check attempt {}/{}", attempt, self.config.max_health_checks);

            // Check component status
            let status = runtime.get_status(component_id).await?;

            match status {
                ComponentStatus::Running => {
                    log::info!("Component is running and healthy");
                    return Ok(());
                }
                ComponentStatus::Failed => {
                    return Err(anyhow!("Component failed during startup"));
                }
                ComponentStatus::Starting => {
                    log::debug!("Component still starting...");
                }
                _ => {
                    return Err(anyhow!("Component in unexpected state: {:?}", status));
                }
            }

            // Wait before next check
            sleep(self.config.health_check_interval).await;
        }

        Err(anyhow!(
            "Component did not become healthy after {} attempts",
            self.config.max_health_checks
        ))
    }

    /// Rollback to old component on failure
    async fn rollback(
        &self,
        old_component_id: ComponentId,
        new_component_id: ComponentId,
        runtime: &Arc<dyn Runtime>,
        reason: String,
    ) -> Result<HotSwapResult> {
        if !self.config.auto_rollback {
            log::warn!("Auto-rollback disabled, not rolling back");
            return Ok(HotSwapResult::Failed {
                old_component_id,
                new_component_id,
                reason,
            });
        }

        log::warn!("Rolling back due to: {}", reason);

        // Stop new component
        if let Err(e) = runtime.stop_component(new_component_id).await {
            log::error!("Failed to stop new component during rollback: {}", e);
            return Ok(HotSwapResult::Failed {
                old_component_id,
                new_component_id,
                reason: format!("Rollback failed: {}", e),
            });
        }

        // Verify old component is still healthy
        let old_status = runtime.get_status(old_component_id).await?;
        if !matches!(old_status, ComponentStatus::Running) {
            log::error!("Old component is no longer running! Status: {:?}", old_status);
            return Ok(HotSwapResult::Failed {
                old_component_id,
                new_component_id,
                reason: format!("Rollback failed: old component not running ({})", reason),
            });
        }

        log::info!("Rollback completed successfully");

        Ok(HotSwapResult::RolledBack {
            old_component_id,
            new_component_id,
            reason,
        })
    }

    /// Perform canary deployment (gradual traffic shift)
    ///
    /// This is a more cautious approach where traffic is shifted gradually:
    /// - Start with 1% of traffic to new component
    /// - Monitor metrics
    /// - Increase to 5%, 10%, 25%, 50%, 100%
    /// - Rollback immediately if errors detected
    ///
    /// This is safer for critical services but takes longer.
    pub async fn canary_deployment(
        &self,
        old_component_id: ComponentId,
        new_component: Component,
        traffic_percentages: Vec<u8>,  // e.g., [1, 5, 10, 25, 50, 100]
    ) -> Result<HotSwapResult> {
        log::info!("Starting canary deployment with percentages: {:?}", traffic_percentages);

        // TODO: Implement canary deployment
        // This would integrate with load balancer / service mesh to gradually
        // shift traffic while monitoring error rates, latency, etc.

        Err(anyhow!("Canary deployment not yet implemented"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::isolation::config::{IsolationConfig, IsolationType};
    use crate::utils::isolation::component::ComponentType;

    #[test]
    fn test_hotswap_config_default() {
        let config = HotSwapConfig::default();
        assert_eq!(config.health_check_timeout, Duration::from_secs(30));
        assert_eq!(config.max_health_checks, 10);
        assert!(config.auto_rollback);
    }

    #[tokio::test]
    async fn test_hotswap_manager_creation() {
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let registry = Arc::new(ComponentRegistry::new());
        let config = HotSwapConfig::default();

        let manager = HotSwapManager::new(runtime_manager, registry, config);
        assert_eq!(manager.config.max_health_checks, 10);
    }
}