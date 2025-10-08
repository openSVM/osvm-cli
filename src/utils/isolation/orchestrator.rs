//! OSVM Core Orchestration Layer
//!
//! This module provides the central control plane for managing all OSVM components.
//! It handles component lifecycle, health monitoring, service discovery, and policy enforcement.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  OSVM Core Orchestrator                                     │
//! │                                                             │
//! │  ┌────────────────────────────────────────────────────────┐│
//! │  │  Component Manager                                     ││
//! │  │  • Lifecycle (start, stop, restart, hot-swap)         ││
//! │  │  • Service Discovery (register, lookup)               ││
//! │  │  • Health Monitoring (periodic checks)                ││
//! │  └────────────────────────────────────────────────────────┘│
//! │                         │                                   │
//! │  ┌──────────────────────▼──────────────────────────────┐   │
//! │  │  Resource Allocator                                  │   │
//! │  │  • CID allocation (vsock)                            │   │
//! │  │  • Port allocation                                   │   │
//! │  │  • Certificate issuance                              │   │
//! │  └──────────────────────┬──────────────────────────────┘   │
//! │                         │                                   │
//! │  ┌──────────────────────▼──────────────────────────────┐   │
//! │  │  Policy Engine                                       │   │
//! │  │  • Network policies (who can talk to whom)          │   │
//! │  │  • Resource policies (CPU, memory limits)           │   │
//! │  │  • Security policies (isolation levels)             │   │
//! │  └──────────────────────┬──────────────────────────────┘   │
//! │                         │                                   │
//! │  ┌──────────────────────▼──────────────────────────────┐   │
//! │  │  Runtime Manager                                     │   │
//! │  │  • Process Runtime (dev/test)                        │   │
//! │  │  • Firecracker Runtime (production)                  │   │
//! │  │  • HermitCore Runtime (minimal services)            │   │
//! │  └──────────────────────────────────────────────────────┘   │
//! └─────────────────────────────────────────────────────────────┘
//!            │                │                │
//!       ┌────▼────┐      ┌────▼────┐     ┌────▼────┐
//!       │ RPC 1   │      │ RPC 2   │     │Validator│
//!       │ (CID 3) │      │ (CID 4) │     │ (CID 5) │
//!       └─────────┘      └─────────┘     └─────────┘
//! ```
//!
//! # Responsibilities
//!
//! 1. **Component Lifecycle**: Start, stop, restart, hot-swap components
//! 2. **Service Discovery**: Components register and find each other
//! 3. **Health Monitoring**: Periodic health checks with automatic remediation
//! 4. **Resource Allocation**: CIDs, ports, certificates, network policies
//! 5. **Policy Enforcement**: Security, networking, and resource policies
//! 6. **Automated Operations**: Auto-scaling, failover, load balancing
//!
//! # Use Cases
//!
//! - **Deploy RPC Node**: Orchestrator allocates resources, starts MicroVM, registers service
//! - **Update Validator**: Hot-swap with zero downtime
//! - **Scale RPC Fleet**: Auto-scale based on load
//! - **Handle Failure**: Detect unhealthy component, restart or replace
//! - **Enforce Policies**: Block unauthorized communication, enforce resource limits

use super::{
    certificate::CertificateAuthority, Component, ComponentId, ComponentRegistry, ComponentStatus,
    ComponentType, HotSwapManager, NetworkManager, RuntimeManager, VsockManager,
};
use anyhow::{anyhow, Context, Result};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tokio::time::{interval, sleep};

/// OSVM Core Orchestrator - central control plane
pub struct Orchestrator {
    /// Component registry
    registry: Arc<ComponentRegistry>,

    /// Runtime manager
    runtime_manager: Arc<RuntimeManager>,

    /// Network manager (mTLS)
    network_manager: Arc<NetworkManager>,

    /// vsock manager (VM-to-VM)
    vsock_manager: Arc<VsockManager>,

    /// Hot-swap manager
    hotswap_manager: Arc<HotSwapManager>,

    /// Certificate authority
    cert_authority: Option<Arc<CertificateAuthority>>,

    /// Service discovery map
    services: Arc<RwLock<HashMap<String, Vec<ServiceEndpoint>>>>,

    /// Health check state
    health_checks: Arc<RwLock<HashMap<ComponentId, HealthState>>>,

    /// Orchestrator configuration
    config: OrchestratorConfig,
}

/// Orchestrator configuration
#[derive(Debug, Clone)]
pub struct OrchestratorConfig {
    /// Health check interval
    pub health_check_interval: Duration,

    /// Component restart threshold (how many failures before giving up)
    pub max_restart_attempts: u32,

    /// Auto-restart failed components
    pub auto_restart: bool,

    /// Auto-scale based on load
    pub auto_scale: bool,

    /// Enable service mesh (automatic mTLS between all components)
    pub enable_service_mesh: bool,
}

impl Default for OrchestratorConfig {
    fn default() -> Self {
        Self {
            health_check_interval: Duration::from_secs(30),
            max_restart_attempts: 3,
            auto_restart: true,
            auto_scale: false,
            enable_service_mesh: true,
        }
    }
}

/// Service endpoint for service discovery
#[derive(Debug, Clone)]
pub struct ServiceEndpoint {
    /// Component ID
    pub component_id: ComponentId,

    /// Service name (e.g., "rpc-node")
    pub service_name: String,

    /// Network address (for external access)
    pub network_addr: Option<String>,

    /// vsock address (for VM-to-VM)
    pub vsock_cid: Option<u32>,

    /// Health status
    pub healthy: bool,

    /// Metadata
    pub metadata: HashMap<String, String>,
}

/// Health state for a component
#[derive(Debug, Clone)]
struct HealthState {
    /// Last health check time
    last_check: std::time::Instant,

    /// Health status
    healthy: bool,

    /// Consecutive failure count
    failure_count: u32,

    /// Last error message
    last_error: Option<String>,
}

impl Orchestrator {
    /// Create a new orchestrator
    pub fn new(
        registry: Arc<ComponentRegistry>,
        runtime_manager: Arc<RuntimeManager>,
        network_manager: Arc<NetworkManager>,
        vsock_manager: Arc<VsockManager>,
        hotswap_manager: Arc<HotSwapManager>,
        config: OrchestratorConfig,
    ) -> Self {
        Self {
            registry,
            runtime_manager,
            network_manager,
            vsock_manager,
            hotswap_manager,
            cert_authority: None,
            services: Arc::new(RwLock::new(HashMap::new())),
            health_checks: Arc::new(RwLock::new(HashMap::new())),
            config,
        }
    }

    /// Start the orchestrator (background tasks)
    ///
    /// Note: This method requires the orchestrator to be wrapped in an Arc.
    /// Call it as: `orchestrator_arc.start_background_tasks().await`
    pub async fn start_background_tasks(self: Arc<Self>) -> Result<()> {
        log::info!("Starting OSVM Core Orchestrator");

        // Start health check loop
        if self.config.health_check_interval > Duration::ZERO {
            let orchestrator = Arc::clone(&self);
            tokio::spawn(async move {
                orchestrator.health_check_loop().await;
            });
        }

        log::info!("Orchestrator started successfully");
        Ok(())
    }

    /// Start the orchestrator (convenience method for non-Arc usage)
    pub async fn start(&self) -> Result<()> {
        log::warn!("start() called without Arc - background tasks will not run. Use start_background_tasks() with Arc<Orchestrator>");
        log::info!("Orchestrator initialized (no background tasks)");
        Ok(())
    }

    /// Deploy a new component
    pub async fn deploy_component(&self, mut component: Component) -> Result<ComponentId> {
        log::info!("Deploying component: {:?}", component.component_type);

        let component_id = component.id;

        // 1. Allocate resources
        log::debug!("Allocating resources for component {}", component_id);

        // Allocate vsock CID
        let cid = self.vsock_manager.allocate_cid(component_id).await?;
        log::debug!("Allocated vsock CID {} for component {}", cid, component_id);

        // Issue certificate (if CA available)
        if let Some(ref ca) = self.cert_authority {
            log::debug!("Issuing certificate for component {}", component_id);
            match ca
                .issue_certificate(
                    &component_id.to_string(),
                    90, // 90 day validity
                )
                .await
            {
                Ok(cert) => {
                    log::info!(
                        "Issued certificate for {} (expires: {})",
                        component_id,
                        cert.not_after
                    );
                    // Store certificate reference in component metadata if needed
                }
                Err(e) => {
                    log::warn!(
                        "Failed to issue certificate for {}: {}. Continuing without mTLS.",
                        component_id,
                        e
                    );
                }
            }
        }

        // 2. Start component
        log::debug!("Starting component {}", component_id);
        let runtime = self
            .runtime_manager
            .get_runtime(&component.isolation_config)?;
        runtime
            .start_component(&mut component)
            .await
            .context("Failed to start component")?;

        // 3. Register component
        log::debug!("Registering component {}", component_id);
        self.registry.register(component.clone()).await?;

        // 4. Register service endpoint
        if let Some(service_name) = self.get_service_name(&component.component_type) {
            log::debug!("Registering service endpoint: {}", service_name);

            // Extract network address from component
            let network_addr = self.extract_network_address(&component);

            self.register_service(ServiceEndpoint {
                component_id,
                service_name,
                network_addr,
                vsock_cid: Some(cid),
                healthy: true,
                metadata: HashMap::new(),
            })
            .await?;
        }

        // 5. Initialize health check
        self.health_checks.write().await.insert(
            component_id,
            HealthState {
                last_check: std::time::Instant::now(),
                healthy: true,
                failure_count: 0,
                last_error: None,
            },
        );

        log::info!("Component {} deployed successfully", component_id);
        Ok(component_id)
    }

    /// Stop and remove a component
    pub async fn undeploy_component(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Undeploying component {}", component_id);

        // Get component
        let component = self.registry.get(component_id).await?;

        // Stop component
        let runtime = self
            .runtime_manager
            .get_runtime(&component.isolation_config)?;
        runtime.stop_component(component_id).await?;

        // Free resources
        self.vsock_manager.free_cid(component_id).await?;

        // Unregister service
        self.unregister_service(component_id).await?;

        // Remove health check
        self.health_checks.write().await.remove(&component_id);

        log::info!("Component {} undeployed successfully", component_id);
        Ok(())
    }

    /// Hot-swap a component (zero-downtime update)
    pub async fn update_component(
        &self,
        old_component_id: ComponentId,
        new_component: Component,
    ) -> Result<ComponentId> {
        log::info!("Updating component {} with hot-swap", old_component_id);

        // Perform hot-swap
        let result = self
            .hotswap_manager
            .hot_swap(old_component_id, new_component.clone())
            .await?;

        match result {
            super::HotSwapResult::Success {
                new_component_id, ..
            } => {
                log::info!(
                    "Component updated successfully: {} → {}",
                    old_component_id,
                    new_component_id
                );

                // Update service registry
                self.update_service_endpoint(old_component_id, new_component_id)
                    .await?;

                Ok(new_component_id)
            }
            super::HotSwapResult::RolledBack { reason, .. } => {
                log::error!("Hot-swap rolled back: {}", reason);
                Err(anyhow!("Hot-swap failed and rolled back: {}", reason))
            }
            super::HotSwapResult::Failed { reason, .. } => {
                log::error!("Hot-swap failed: {}", reason);
                Err(anyhow!("Hot-swap failed: {}", reason))
            }
        }
    }

    /// Discover services by name
    pub async fn discover_service(&self, service_name: &str) -> Result<Vec<ServiceEndpoint>> {
        let services = self.services.read().await;

        if let Some(endpoints) = services.get(service_name) {
            // Filter to only healthy endpoints
            let healthy: Vec<_> = endpoints.iter().filter(|e| e.healthy).cloned().collect();

            if healthy.is_empty() {
                return Err(anyhow!(
                    "No healthy endpoints for service '{}'",
                    service_name
                ));
            }

            Ok(healthy)
        } else {
            Err(anyhow!("Service '{}' not found", service_name))
        }
    }

    /// Register a service endpoint
    async fn register_service(&self, endpoint: ServiceEndpoint) -> Result<()> {
        let mut services = self.services.write().await;

        services
            .entry(endpoint.service_name.clone())
            .or_insert_with(Vec::new)
            .push(endpoint);

        Ok(())
    }

    /// Unregister service endpoints for a component
    async fn unregister_service(&self, component_id: ComponentId) -> Result<()> {
        let mut services = self.services.write().await;

        for endpoints in services.values_mut() {
            endpoints.retain(|e| e.component_id != component_id);
        }

        Ok(())
    }

    /// Update service endpoint (after hot-swap)
    async fn update_service_endpoint(
        &self,
        old_component_id: ComponentId,
        new_component_id: ComponentId,
    ) -> Result<()> {
        let mut services = self.services.write().await;

        for endpoints in services.values_mut() {
            for endpoint in endpoints.iter_mut() {
                if endpoint.component_id == old_component_id {
                    endpoint.component_id = new_component_id;
                }
            }
        }

        Ok(())
    }

    /// Health check loop (runs periodically)
    async fn health_check_loop(self: Arc<Self>) {
        let mut ticker = interval(self.config.health_check_interval);

        loop {
            ticker.tick().await;

            if let Err(e) = self.check_all_components().await {
                log::error!("Health check error: {}", e);
            }
        }
    }

    /// Check health of all components
    async fn check_all_components(&self) -> Result<()> {
        let components = self.registry.list().await;

        for component in components {
            if let Err(e) = self.check_component_health(component.id).await {
                log::warn!("Health check failed for component {}: {}", component.id, e);

                // Handle unhealthy component
                if self.config.auto_restart {
                    self.handle_unhealthy_component(component.id).await?;
                }
            }
        }

        Ok(())
    }

    /// Check health of a single component
    async fn check_component_health(&self, component_id: ComponentId) -> Result<()> {
        let component = self.registry.get(component_id).await?;
        let runtime = self
            .runtime_manager
            .get_runtime(&component.isolation_config)?;

        // Get component status
        let status = runtime.get_status(component_id).await?;

        let mut health_checks = self.health_checks.write().await;
        let health_state = health_checks
            .get_mut(&component_id)
            .ok_or_else(|| anyhow!("No health state for component {}", component_id))?;

        health_state.last_check = std::time::Instant::now();

        match status {
            ComponentStatus::Running => {
                health_state.healthy = true;
                health_state.failure_count = 0;
                health_state.last_error = None;
                Ok(())
            }
            ComponentStatus::Failed => {
                health_state.healthy = false;
                health_state.failure_count += 1;
                health_state.last_error = Some("Component failed".to_string());
                Err(anyhow!("Component is in failed state"))
            }
            _ => {
                // Starting, Stopping, etc. - not an error
                Ok(())
            }
        }
    }

    /// Handle unhealthy component (restart or replace)
    async fn handle_unhealthy_component(&self, component_id: ComponentId) -> Result<()> {
        let health_checks = self.health_checks.read().await;
        let health_state = health_checks
            .get(&component_id)
            .ok_or_else(|| anyhow!("No health state for component {}", component_id))?;

        if health_state.failure_count >= self.config.max_restart_attempts {
            log::error!(
                "Component {} exceeded max restart attempts ({}), giving up",
                component_id,
                self.config.max_restart_attempts
            );
            return Err(anyhow!("Max restart attempts exceeded"));
        }

        log::info!(
            "Restarting unhealthy component {} (attempt {})",
            component_id,
            health_state.failure_count
        );

        drop(health_checks); // Release lock

        // Get component and restart
        let component = self.registry.get(component_id).await?;
        let runtime = self
            .runtime_manager
            .get_runtime(&component.isolation_config)?;

        runtime.restart_component(component_id).await?;

        log::info!("Component {} restarted successfully", component_id);
        Ok(())
    }

    /// Get service name from component type
    fn get_service_name(&self, component_type: &ComponentType) -> Option<String> {
        match component_type {
            ComponentType::RpcNode { .. } => Some("rpc-node".to_string()),
            ComponentType::Validator { .. } => Some("validator".to_string()),
            ComponentType::McpServer { name, .. } => Some(format!("mcp-{}", name)),
            ComponentType::OsvmCore => Some("osvm-core".to_string()),
            _ => None,
        }
    }

    /// Extract network address from component
    fn extract_network_address(&self, component: &Component) -> Option<String> {
        match &component.component_type {
            ComponentType::RpcNode { bind_address, .. } => bind_address.clone(),
            ComponentType::McpServer { .. } => {
                // Extract from runtime handle if available
                component
                    .runtime_handle
                    .as_ref()
                    .and_then(|handle| handle.data.get("listen_addr").cloned())
            }
            ComponentType::Validator { .. } => {
                // Extract from runtime handle if available
                component
                    .runtime_handle
                    .as_ref()
                    .and_then(|handle| handle.data.get("rpc_addr").cloned())
            }
            ComponentType::Service { .. } => {
                // Extract from runtime handle
                component
                    .runtime_handle
                    .as_ref()
                    .and_then(|handle| handle.data.get("bind_addr").cloned())
            }
            ComponentType::OsvmCore => {
                // OSVM Core doesn't expose a network address
                None
            }
        }
    }

    /// Get orchestrator statistics
    pub async fn stats(&self) -> OrchestratorStats {
        let components = self.registry.list().await;
        let services = self.services.read().await;
        let health_checks = self.health_checks.read().await;

        let healthy_count = health_checks.values().filter(|h| h.healthy).count();

        OrchestratorStats {
            total_components: components.len(),
            healthy_components: healthy_count,
            unhealthy_components: components.len() - healthy_count,
            registered_services: services.len(),
            total_endpoints: services.values().map(|v| v.len()).sum(),
        }
    }

    /// List all components (for autoscaler)
    pub async fn list_components(&self) -> Vec<Component> {
        self.registry.list().await
    }

    /// Stop a component
    pub async fn stop_component(&self, component_id: ComponentId) -> Result<()> {
        log::info!("Stopping component {}", component_id);

        let component = self.registry.get(component_id).await?;
        let runtime = self
            .runtime_manager
            .get_runtime(&component.isolation_config)?;

        runtime.stop_component(component_id).await?;

        // Update status
        self.registry
            .update_status(component_id, ComponentStatus::Stopped)
            .await?;

        // Remove from health checks
        self.health_checks.write().await.remove(&component_id);

        log::info!("Component {} stopped", component_id);
        Ok(())
    }

    /// List components filtered by type name
    pub async fn list_components_by_type(&self, type_name: &str) -> Vec<Component> {
        self.registry
            .list()
            .await
            .into_iter()
            .filter(|c| {
                let component_type_name = match &c.component_type {
                    ComponentType::OsvmCore => "OsvmCore",
                    ComponentType::Validator { .. } => "Validator",
                    ComponentType::RpcNode { .. } => "RpcNode",
                    ComponentType::McpServer { .. } => "McpServer",
                    ComponentType::Service { .. } => "Service",
                };
                component_type_name == type_name
            })
            .collect()
    }

    /// Scale component type to desired count
    pub async fn scale_component_type(
        &self,
        component_type: &str,
        desired_count: usize,
    ) -> Result<()> {
        let current_components = self.list_components_by_type(component_type).await;
        let current_count = current_components.len();

        log::info!(
            "Scaling {} from {} to {} instances",
            component_type,
            current_count,
            desired_count
        );

        if desired_count > current_count {
            // Scale up
            let to_add = desired_count - current_count;
            log::info!(
                "Scaling up: adding {} instances of {}",
                to_add,
                component_type
            );

            for i in 0..to_add {
                // Create new component based on type
                // Note: This is a simplified implementation
                // Production would use proper component templates
                log::info!("Adding {} instance {}/{}", component_type, i + 1, to_add);
            }
        } else if desired_count < current_count {
            // Scale down
            let to_remove = current_count - desired_count;
            log::info!(
                "Scaling down: removing {} instances of {}",
                to_remove,
                component_type
            );

            for (i, component) in current_components.iter().take(to_remove).enumerate() {
                log::info!(
                    "Removing {} instance {}/{}: {}",
                    component_type,
                    i + 1,
                    to_remove,
                    component.id
                );
                // Stop and remove component
                self.stop_component(component.id).await?;
            }
        }

        Ok(())
    }
}

/// Orchestrator statistics
#[derive(Debug, Clone)]
pub struct OrchestratorStats {
    pub total_components: usize,
    pub healthy_components: usize,
    pub unhealthy_components: usize,
    pub registered_services: usize,
    pub total_endpoints: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::isolation::config::{IsolationConfig, IsolationType};

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let network_manager = Arc::new(NetworkManager::default());
        let temp_dir = std::env::temp_dir().join("osvm-orch-test1");
        let vsock_manager = Arc::new(VsockManager::new(temp_dir).unwrap());
        let hotswap_manager = Arc::new(HotSwapManager::new(
            runtime_manager.clone(),
            registry.clone(),
            Default::default(),
        ));

        let orchestrator = Orchestrator::new(
            registry,
            runtime_manager,
            network_manager,
            vsock_manager,
            hotswap_manager,
            OrchestratorConfig::default(),
        );

        assert_eq!(orchestrator.config.max_restart_attempts, 3);
    }

    #[tokio::test]
    async fn test_orchestrator_stats() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let network_manager = Arc::new(NetworkManager::default());
        let temp_dir = std::env::temp_dir().join("osvm-orch-test2");
        let vsock_manager = Arc::new(VsockManager::new(temp_dir).unwrap());
        let hotswap_manager = Arc::new(HotSwapManager::new(
            runtime_manager.clone(),
            registry.clone(),
            Default::default(),
        ));

        let orchestrator = Orchestrator::new(
            registry,
            runtime_manager,
            network_manager,
            vsock_manager,
            hotswap_manager,
            OrchestratorConfig::default(),
        );

        let stats = orchestrator.stats().await;
        assert_eq!(stats.total_components, 0);
    }

    #[tokio::test]
    async fn test_orchestrator_list_components() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let orchestrator = Orchestrator::new(
            registry.clone(),
            runtime_manager,
            Arc::new(NetworkManager::default()),
            Arc::new(VsockManager::new(std::env::temp_dir().join("osvm-orch-list")).unwrap()),
            Arc::new(HotSwapManager::new(
                Arc::new(RuntimeManager::with_defaults()),
                registry.clone(),
                Default::default(),
            )),
            OrchestratorConfig::default(),
        );

        // Initially empty
        let components = orchestrator.list_components().await;
        assert_eq!(components.len(), 0);

        // Register some components
        let comp1 = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "testnet".to_string(),
                bind_address: None,
            },
            status: ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(comp1).await.unwrap();

        let components = orchestrator.list_components().await;
        assert_eq!(components.len(), 1);
    }

    #[tokio::test]
    async fn test_orchestrator_list_by_type() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let orchestrator = Orchestrator::new(
            registry.clone(),
            runtime_manager,
            Arc::new(NetworkManager::default()),
            Arc::new(VsockManager::new(std::env::temp_dir().join("osvm-orch-bytype")).unwrap()),
            Arc::new(HotSwapManager::new(
                Arc::new(RuntimeManager::with_defaults()),
                registry.clone(),
                Default::default(),
            )),
            OrchestratorConfig::default(),
        );

        // Register different component types
        let rpc = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "testnet".to_string(),
                bind_address: None,
            },
            status: ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        let validator = Component {
            id: ComponentId::new(),
            component_type: ComponentType::Validator {
                network: "mainnet".to_string(),
                identity: None,
            },
            status: ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(rpc).await.unwrap();
        registry.register(validator).await.unwrap();

        // List by type
        let rpc_nodes = orchestrator.list_components_by_type("RpcNode").await;
        assert_eq!(rpc_nodes.len(), 1);

        let validators = orchestrator.list_components_by_type("Validator").await;
        assert_eq!(validators.len(), 1);

        let mcp_servers = orchestrator.list_components_by_type("McpServer").await;
        assert_eq!(mcp_servers.len(), 0);
    }

    #[tokio::test]
    async fn test_orchestrator_scale_component_type() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let orchestrator = Orchestrator::new(
            registry.clone(),
            runtime_manager,
            Arc::new(NetworkManager::default()),
            Arc::new(VsockManager::new(std::env::temp_dir().join("osvm-orch-scale")).unwrap()),
            Arc::new(HotSwapManager::new(
                Arc::new(RuntimeManager::with_defaults()),
                registry.clone(),
                Default::default(),
            )),
            OrchestratorConfig::default(),
        );

        // Register some RPC components
        for _ in 0..2 {
            let component = Component {
                id: ComponentId::new(),
                component_type: ComponentType::RpcNode {
                    network: "testnet".to_string(),
                    bind_address: None,
                },
                status: ComponentStatus::Running,
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                metadata: Default::default(),
            };
            registry.register(component).await.unwrap();
        }

        // Initially have 2 RPC nodes
        let rpc_nodes = orchestrator.list_components_by_type("RpcNode").await;
        assert_eq!(rpc_nodes.len(), 2);

        // Scale to 3 (scale up operation logs but doesn't actually deploy in this stub)
        let result = orchestrator.scale_component_type("RpcNode", 3).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_complete_deployment_workflow() {
        // Integration test: Complete component deployment workflow
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let network_manager = Arc::new(NetworkManager::default());
        let vsock_manager = Arc::new(
            VsockManager::new(std::env::temp_dir().join("osvm-integration-test")).unwrap(),
        );
        let hotswap_manager = Arc::new(HotSwapManager::new(
            runtime_manager.clone(),
            registry.clone(),
            Default::default(),
        ));

        let orchestrator = Orchestrator::new(
            registry.clone(),
            runtime_manager,
            network_manager,
            vsock_manager,
            hotswap_manager,
            OrchestratorConfig::default(),
        );

        // Step 1: Start with no components
        assert_eq!(orchestrator.list_components().await.len(), 0);

        // Step 2: Deploy a component manually (simulated)
        let component = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "testnet".to_string(),
                bind_address: Some("127.0.0.1:8899".to_string()),
            },
            status: ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        let component_id = component.id;
        registry.register(component).await.unwrap();

        // Step 3: Verify component is listed
        assert_eq!(orchestrator.list_components().await.len(), 1);

        // Step 4: Check stats
        let stats = orchestrator.stats().await;
        assert_eq!(stats.total_components, 1);

        // Step 5: List by type
        let rpc_nodes = orchestrator.list_components_by_type("RpcNode").await;
        assert_eq!(rpc_nodes.len(), 1);

        // Step 6: Update status manually (stop requires runtime tracking)
        registry
            .update_status(component_id, ComponentStatus::Stopped)
            .await
            .unwrap();

        // Step 7: Verify status updated
        let stopped = registry.get(component_id).await.unwrap();
        assert_eq!(stopped.status, ComponentStatus::Stopped);
    }

    #[tokio::test]
    async fn test_multi_component_type_deployment() {
        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let orchestrator = Orchestrator::new(
            registry.clone(),
            runtime_manager,
            Arc::new(NetworkManager::default()),
            Arc::new(VsockManager::new(std::env::temp_dir().join("osvm-multi-type")).unwrap()),
            Arc::new(HotSwapManager::new(
                Arc::new(RuntimeManager::with_defaults()),
                registry.clone(),
                Default::default(),
            )),
            OrchestratorConfig::default(),
        );

        // Deploy multiple component types
        let components = vec![
            Component {
                id: ComponentId::new(),
                component_type: ComponentType::Validator {
                    network: "mainnet".to_string(),
                    identity: None,
                },
                status: ComponentStatus::Running,
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                metadata: Default::default(),
            },
            Component {
                id: ComponentId::new(),
                component_type: ComponentType::RpcNode {
                    network: "mainnet".to_string(),
                    bind_address: None,
                },
                status: ComponentStatus::Running,
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                metadata: Default::default(),
            },
            Component {
                id: ComponentId::new(),
                component_type: ComponentType::McpServer {
                    name: "test-mcp".to_string(),
                    version: None,
                },
                status: ComponentStatus::Running,
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                metadata: Default::default(),
            },
        ];

        for comp in components {
            registry.register(comp).await.unwrap();
        }

        // Verify all types are present
        assert_eq!(
            orchestrator
                .list_components_by_type("Validator")
                .await
                .len(),
            1
        );
        assert_eq!(
            orchestrator.list_components_by_type("RpcNode").await.len(),
            1
        );
        assert_eq!(
            orchestrator
                .list_components_by_type("McpServer")
                .await
                .len(),
            1
        );
        assert_eq!(
            orchestrator.list_components_by_type("Service").await.len(),
            0
        );

        let stats = orchestrator.stats().await;
        assert_eq!(stats.total_components, 3);
    }

    #[tokio::test]
    async fn test_orchestrator_concurrent_operations() {
        use tokio::task::JoinSet;

        let registry = Arc::new(ComponentRegistry::new());
        let runtime_manager = Arc::new(RuntimeManager::with_defaults());
        let orchestrator = Arc::new(Orchestrator::new(
            registry.clone(),
            runtime_manager,
            Arc::new(NetworkManager::default()),
            Arc::new(VsockManager::new(std::env::temp_dir().join("osvm-concurrent")).unwrap()),
            Arc::new(HotSwapManager::new(
                Arc::new(RuntimeManager::with_defaults()),
                registry.clone(),
                Default::default(),
            )),
            OrchestratorConfig::default(),
        ));

        // Register 20 components concurrently
        for i in 0..20 {
            let component = Component {
                id: ComponentId::new(),
                component_type: ComponentType::Service {
                    name: format!("service-{}", i),
                },
                status: ComponentStatus::Running,
                isolation_config: IsolationConfig::default(),
                runtime_handle: None,
                metadata: Default::default(),
            };
            registry.register(component).await.unwrap();
        }

        let mut handles = JoinSet::new();

        // Perform concurrent operations
        for _ in 0..10 {
            let orch = orchestrator.clone();
            handles.spawn(async move { orch.list_components().await.len() });
        }

        for _ in 0..10 {
            let orch = orchestrator.clone();
            handles.spawn(async move { orch.stats().await.total_components });
        }

        // All operations should succeed
        let mut total = 0;
        while let Some(result) = handles.join_next().await {
            total += result.unwrap();
        }

        // Should have collected component counts
        assert!(total > 0);
    }
}
