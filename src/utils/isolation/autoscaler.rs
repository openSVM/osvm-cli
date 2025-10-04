//! Auto-Scaling System for OSVM Components
//!
//! This module implements intelligent auto-scaling of components based on load metrics.
//! It monitors resource usage and automatically adjusts the number of running instances
//! to meet demand while minimizing costs.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  Auto-Scaler                                                │
//! │  ┌────────────────────────────────────────────────────────┐ │
//! │  │  Metrics Collector                                     │ │
//! │  │  • CPU usage                                           │ │
//! │  │  • Memory usage                                        │ │
//! │  │  • Request rate                                        │ │
//! │  │  • Response latency                                    │ │
//! │  └────────────────────────────────────────────────────────┘ │
//! │                         │                                   │
//! │  ┌──────────────────────▼──────────────────────────────┐   │
//! │  │  Decision Engine                                     │   │
//! │  │  if (CPU > 80%):    scale_up()                      │   │
//! │  │  if (CPU < 20%):    scale_down()                    │   │
//! │  │  if (requests > limit): scale_up()                  │   │
//! │  └──────────────────────┬──────────────────────────────┘   │
//! │                         │                                   │
//! │  ┌──────────────────────▼──────────────────────────────┐   │
//! │  │  Scaling Actions (via Orchestrator)                 │   │
//! │  │  • Deploy new instance (hot-standby)                │   │
//! │  │  • Register with load balancer                      │   │
//! │  │  • Shift traffic gradually                          │   │
//! │  │  • Remove old instance after drain                  │   │
//! │  └──────────────────────────────────────────────────────┘   │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Scaling Policies
//!
//! 1. **Target Tracking**: Maintain metric at target (e.g., 70% CPU)
//! 2. **Step Scaling**: Scale by fixed amount when threshold crossed
//! 3. **Predictive**: Use ML to predict load and scale proactively
//!
//! # Example Use Cases
//!
//! **RPC Node Auto-Scaling**:
//! - Monitor request rate (TPS)
//! - When TPS > 80% capacity → Add new RPC instance
//! - When TPS < 20% capacity → Remove instance
//! - Result: Match capacity to demand, save costs
//!
//! **Validator Scaling** (for multiple validators):
//! - Monitor stake distribution
//! - Add validators when stake increases
//! - Decommission when stake decreases
//! - Result: Optimal validator count

use super::{Component, ComponentId, ComponentType, Orchestrator};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tokio::time::interval;

/// Auto-scaler for managing component instances
pub struct AutoScaler {
    /// Orchestrator for deploying/removing instances
    orchestrator: Arc<Orchestrator>,

    /// Scaling configurations per component type
    policies: Arc<RwLock<HashMap<String, ScalingPolicy>>>,

    /// Component metrics
    metrics: Arc<RwLock<HashMap<ComponentId, ComponentMetrics>>>,

    /// Auto-scaler configuration
    config: AutoScalerConfig,
}

/// Auto-scaler configuration
#[derive(Debug, Clone)]
pub struct AutoScalerConfig {
    /// How often to evaluate scaling decisions
    pub evaluation_interval: Duration,

    /// Cooldown period after scaling action
    pub cooldown_period: Duration,

    /// Enable predictive scaling
    pub enable_predictive: bool,
}

impl Default for AutoScalerConfig {
    fn default() -> Self {
        Self {
            evaluation_interval: Duration::from_secs(60),
            cooldown_period: Duration::from_secs(300), // 5 minutes
            enable_predictive: false,
        }
    }
}

/// Scaling policy for a component type
#[derive(Debug, Clone)]
pub struct ScalingPolicy {
    /// Component type this policy applies to
    pub component_type: String,

    /// Minimum instances
    pub min_instances: usize,

    /// Maximum instances
    pub max_instances: usize,

    /// Target CPU utilization (0.0 - 1.0)
    pub target_cpu: f64,

    /// Target memory utilization (0.0 - 1.0)
    pub target_memory: f64,

    /// Target request rate (requests/sec)
    pub target_rps: Option<f64>,

    /// Scale up threshold (how much above target triggers scale-up)
    pub scale_up_threshold: f64,

    /// Scale down threshold (how much below target triggers scale-down)
    pub scale_down_threshold: f64,

    /// How many instances to add/remove per action
    pub scale_step: usize,
}

impl Default for ScalingPolicy {
    fn default() -> Self {
        Self {
            component_type: "unknown".to_string(),
            min_instances: 1,
            max_instances: 10,
            target_cpu: 0.70,    // 70%
            target_memory: 0.80, // 80%
            target_rps: None,
            scale_up_threshold: 0.15,   // Scale up if 15% above target
            scale_down_threshold: 0.20, // Scale down if 20% below target
            scale_step: 1,
        }
    }
}

/// Component metrics for scaling decisions
#[derive(Debug, Clone)]
pub struct ComponentMetrics {
    /// Component ID
    pub component_id: ComponentId,

    /// CPU utilization (0.0 - 1.0)
    pub cpu_usage: f64,

    /// Memory utilization (0.0 - 1.0)
    pub memory_usage: f64,

    /// Request rate (requests/sec)
    pub requests_per_sec: f64,

    /// Average response latency (milliseconds)
    pub avg_latency_ms: f64,

    /// Last update timestamp
    pub timestamp: std::time::Instant,
}

/// Scaling decision
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalingDecision {
    /// Scale up by N instances
    ScaleUp(usize),

    /// Scale down by N instances
    ScaleDown(usize),

    /// No action needed
    NoChange,
}

impl AutoScaler {
    /// Create a new auto-scaler
    pub fn new(orchestrator: Arc<Orchestrator>, config: AutoScalerConfig) -> Self {
        Self {
            orchestrator,
            policies: Arc::new(RwLock::new(HashMap::new())),
            metrics: Arc::new(RwLock::new(HashMap::new())),
            config,
        }
    }

    /// Start the auto-scaling loop
    pub async fn start(&self) -> Result<()> {
        log::info!("Starting auto-scaler");

        let mut ticker = interval(self.config.evaluation_interval);

        loop {
            ticker.tick().await;

            if let Err(e) = self.evaluate_all_policies().await {
                log::error!("Auto-scaling evaluation failed: {}", e);
            }
        }
    }

    /// Add a scaling policy for a component type
    pub async fn add_policy(&self, policy: ScalingPolicy) {
        log::info!("Adding scaling policy for {}", policy.component_type);
        let mut policies = self.policies.write().await;
        policies.insert(policy.component_type.clone(), policy);
    }

    /// Update metrics for a component
    pub async fn update_metrics(&self, metrics: ComponentMetrics) {
        let mut all_metrics = self.metrics.write().await;
        all_metrics.insert(metrics.component_id, metrics);
    }

    /// Evaluate all scaling policies
    async fn evaluate_all_policies(&self) -> Result<()> {
        let policies = self.policies.read().await;

        // TODO: Get components from orchestrator (need public API)
        // For now, return early - will implement when orchestrator API is ready
        log::debug!("Auto-scaling evaluation (implementation pending)");

        // Stub: Evaluate policies but don't actually scale
        for (_type_name, _policy) in policies.iter() {
            // Future: Get instances, evaluate metrics, make scaling decisions
        }

        Ok(())

        // Future implementation when orchestrator public API is available:
        /*
        let components = self.orchestrator.list_components().await?;
        let mut by_type: HashMap<String, Vec<Component>> = HashMap::new();
        for component in components {
            let type_name = component_type_name(&component.component_type);
            by_type.entry(type_name).or_default().push(component);
        }
        for (type_name, policy) in policies.iter() {
            let instances = by_type.get(type_name).map(|v| v.as_slice()).unwrap_or(&[]);
            self.evaluate_policy(policy, instances).await?;
        }
        Ok(())
        */
    }

    /// Evaluate a single scaling policy
    async fn evaluate_policy(&self, policy: &ScalingPolicy, instances: &[Component]) -> Result<()> {
        let current_count = instances.len();

        // Get aggregated metrics
        let avg_metrics = self.aggregate_metrics(instances).await?;

        // Make scaling decision
        let decision = self.make_decision(policy, &avg_metrics, current_count);

        match decision {
            ScalingDecision::ScaleUp(count) => {
                log::info!(
                    "Scaling up {} by {} instances (current: {})",
                    policy.component_type,
                    count,
                    current_count
                );

                // Check max instances limit
                if current_count + count > policy.max_instances {
                    log::warn!(
                        "Cannot scale up: would exceed max instances ({}) ",
                        policy.max_instances
                    );
                    return Ok(());
                }

                // Scale up
                for _ in 0..count {
                    // Clone configuration from existing instance
                    if let Some(template) = instances.first() {
                        let mut new_component = template.clone();
                        new_component.id = ComponentId::new();

                        self.orchestrator.deploy_component(new_component).await?;
                    }
                }
            }

            ScalingDecision::ScaleDown(count) => {
                log::info!(
                    "Scaling down {} by {} instances (current: {})",
                    policy.component_type,
                    count,
                    current_count
                );

                // Check min instances limit
                if current_count.saturating_sub(count) < policy.min_instances {
                    log::warn!(
                        "Cannot scale down: would go below min instances ({})",
                        policy.min_instances
                    );
                    return Ok(());
                }

                // Scale down (remove least loaded instances)
                let to_remove = self.select_instances_to_remove(instances, count).await;
                for component_id in to_remove {
                    self.orchestrator.undeploy_component(component_id).await?;
                }
            }

            ScalingDecision::NoChange => {
                log::debug!("No scaling action needed for {}", policy.component_type);
            }
        }

        Ok(())
    }

    /// Make scaling decision based on metrics
    fn make_decision(
        &self,
        policy: &ScalingPolicy,
        metrics: &ComponentMetrics,
        current_count: usize,
    ) -> ScalingDecision {
        // Check CPU utilization
        let cpu_diff = metrics.cpu_usage - policy.target_cpu;

        if cpu_diff > policy.scale_up_threshold {
            log::info!(
                "CPU usage ({:.1}%) exceeds target ({:.1}%) + threshold",
                metrics.cpu_usage * 100.0,
                policy.target_cpu * 100.0
            );
            return ScalingDecision::ScaleUp(policy.scale_step);
        }

        if cpu_diff < -policy.scale_down_threshold && current_count > policy.min_instances {
            log::info!(
                "CPU usage ({:.1}%) below target ({:.1}%) - threshold",
                metrics.cpu_usage * 100.0,
                policy.target_cpu * 100.0
            );
            return ScalingDecision::ScaleDown(policy.scale_step);
        }

        // Check request rate if configured
        if let Some(target_rps) = policy.target_rps {
            let rps_ratio = metrics.requests_per_sec / target_rps;

            if rps_ratio > 1.0 + policy.scale_up_threshold {
                log::info!(
                    "Request rate ({:.1} RPS) exceeds target ({:.1} RPS)",
                    metrics.requests_per_sec,
                    target_rps
                );
                return ScalingDecision::ScaleUp(policy.scale_step);
            }

            if rps_ratio < 1.0 - policy.scale_down_threshold && current_count > policy.min_instances
            {
                log::info!(
                    "Request rate ({:.1} RPS) below target ({:.1} RPS)",
                    metrics.requests_per_sec,
                    target_rps
                );
                return ScalingDecision::ScaleDown(policy.scale_step);
            }
        }

        ScalingDecision::NoChange
    }

    /// Aggregate metrics across multiple component instances
    async fn aggregate_metrics(&self, instances: &[Component]) -> Result<ComponentMetrics> {
        let metrics = self.metrics.read().await;

        let mut total_cpu = 0.0;
        let mut total_memory = 0.0;
        let mut total_rps = 0.0;
        let mut total_latency = 0.0;
        let mut count = 0;

        for instance in instances {
            if let Some(m) = metrics.get(&instance.id) {
                total_cpu += m.cpu_usage;
                total_memory += m.memory_usage;
                total_rps += m.requests_per_sec;
                total_latency += m.avg_latency_ms;
                count += 1;
            }
        }

        if count == 0 {
            return Ok(ComponentMetrics {
                component_id: ComponentId::new(),
                cpu_usage: 0.0,
                memory_usage: 0.0,
                requests_per_sec: 0.0,
                avg_latency_ms: 0.0,
                timestamp: std::time::Instant::now(),
            });
        }

        Ok(ComponentMetrics {
            component_id: ComponentId::new(),
            cpu_usage: total_cpu / count as f64,
            memory_usage: total_memory / count as f64,
            requests_per_sec: total_rps, // Total, not average
            avg_latency_ms: total_latency / count as f64,
            timestamp: std::time::Instant::now(),
        })
    }

    /// Select instances to remove (least loaded first)
    async fn select_instances_to_remove(
        &self,
        instances: &[Component],
        count: usize,
    ) -> Vec<ComponentId> {
        let metrics = self.metrics.read().await;

        let mut scored: Vec<(ComponentId, f64)> = instances
            .iter()
            .filter_map(|instance| {
                metrics.get(&instance.id).map(|m| {
                    // Score based on utilization (lower = better candidate for removal)
                    let score = m.cpu_usage + m.memory_usage;
                    (instance.id, score)
                })
            })
            .collect();

        // Sort by score (ascending)
        scored.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        // Take lowest N
        scored.into_iter().take(count).map(|(id, _)| id).collect()
    }
}

/// Get component type name
fn component_type_name(component_type: &ComponentType) -> String {
    match component_type {
        ComponentType::OsvmCore => "OsvmCore".to_string(),
        ComponentType::Validator { .. } => "Validator".to_string(),
        ComponentType::RpcNode { .. } => "RpcNode".to_string(),
        ComponentType::McpServer { .. } => "McpServer".to_string(),
        ComponentType::Service { .. } => "Service".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scaling_policy_default() {
        let policy = ScalingPolicy::default();
        assert_eq!(policy.min_instances, 1);
        assert_eq!(policy.max_instances, 10);
        assert_eq!(policy.target_cpu, 0.70);
    }

    #[test]
    fn test_scaling_decision() {
        assert_eq!(ScalingDecision::ScaleUp(2), ScalingDecision::ScaleUp(2));
        assert_ne!(ScalingDecision::ScaleUp(1), ScalingDecision::ScaleDown(1));
    }
}
