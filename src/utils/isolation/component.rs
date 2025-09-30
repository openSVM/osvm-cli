//! Component management for isolated execution

use super::{IsolationConfig, IsolationError};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Unique identifier for a component
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ComponentId(Uuid);

impl ComponentId {
    /// Create a new random component ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    /// Create from UUID
    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    /// Get the underlying UUID
    pub fn uuid(&self) -> Uuid {
        self.0
    }
}

impl Default for ComponentId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ComponentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::str::FromStr for ComponentId {
    type Err = uuid::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(Uuid::parse_str(s)?))
    }
}

/// Type of component
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ComponentType {
    /// OSVM Core orchestration service
    OsvmCore,

    /// Solana validator
    Validator {
        #[serde(default)]
        network: String,
        #[serde(default)]
        identity: Option<String>,
    },

    /// RPC node
    RpcNode {
        #[serde(default)]
        network: String,
        #[serde(default)]
        bind_address: Option<String>,
    },

    /// MCP server
    McpServer {
        name: String,
        #[serde(default)]
        version: Option<String>,
    },

    /// Generic service
    Service {
        name: String,
    },
}

impl ComponentType {
    /// Get human-readable name
    pub fn name(&self) -> &str {
        match self {
            ComponentType::OsvmCore => "OSVM Core",
            ComponentType::Validator { .. } => "Validator",
            ComponentType::RpcNode { .. } => "RPC Node",
            ComponentType::McpServer { .. } => "MCP Server",
            ComponentType::Service { .. } => "Service",
        }
    }

    /// Check if component is security-critical
    pub fn is_security_critical(&self) -> bool {
        matches!(
            self,
            ComponentType::OsvmCore | ComponentType::Validator { .. }
        )
    }
}

/// Component status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComponentStatus {
    /// Component is starting
    Starting,
    /// Component is running
    Running,
    /// Component is stopping
    Stopping,
    /// Component is stopped
    Stopped,
    /// Component has failed
    Failed,
    /// Component is being upgraded
    Upgrading,
}

impl ComponentStatus {
    /// Check if component is operational
    pub fn is_operational(&self) -> bool {
        matches!(self, ComponentStatus::Running)
    }

    /// Check if component is transitioning
    pub fn is_transitioning(&self) -> bool {
        matches!(
            self,
            ComponentStatus::Starting | ComponentStatus::Stopping | ComponentStatus::Upgrading
        )
    }
}

/// A component running in an isolated environment
#[derive(Debug, Clone)]
pub struct Component {
    /// Unique identifier
    pub id: ComponentId,

    /// Component type
    pub component_type: ComponentType,

    /// Current status
    pub status: ComponentStatus,

    /// Isolation configuration
    pub isolation_config: IsolationConfig,

    /// Runtime handle (if running)
    pub runtime_handle: Option<RuntimeHandle>,

    /// Metadata
    pub metadata: ComponentMetadata,
}

/// Runtime handle for a running component
#[derive(Debug, Clone)]
pub struct RuntimeHandle {
    /// Process ID (if applicable)
    pub pid: Option<u32>,

    /// Socket path for communication
    pub socket_path: Option<String>,

    /// Additional runtime-specific data
    pub data: HashMap<String, String>,
}

/// Component metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentMetadata {
    /// Human-readable name
    pub name: String,

    /// Version
    #[serde(default)]
    pub version: Option<String>,

    /// Description
    #[serde(default)]
    pub description: Option<String>,

    /// Additional key-value pairs
    #[serde(default)]
    pub tags: HashMap<String, String>,
}

impl Default for ComponentMetadata {
    fn default() -> Self {
        Self {
            name: "Unknown".to_string(),
            version: None,
            description: None,
            tags: HashMap::new(),
        }
    }
}

/// Registry of all components
pub struct ComponentRegistry {
    components: Arc<RwLock<HashMap<ComponentId, Component>>>,
}

impl ComponentRegistry {
    /// Create a new component registry
    pub fn new() -> Self {
        Self {
            components: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a new component
    pub async fn register(&self, component: Component) -> Result<ComponentId> {
        let id = component.id;
        let mut components = self.components.write().await;

        if components.contains_key(&id) {
            return Err(IsolationError::OperationFailed(format!(
                "Component {} already registered",
                id
            ))
            .into());
        }

        components.insert(id, component);
        Ok(id)
    }

    /// Unregister a component
    pub async fn unregister(&self, id: ComponentId) -> Result<()> {
        let mut components = self.components.write().await;

        if components.remove(&id).is_none() {
            return Err(
                IsolationError::ComponentNotFound(format!("Component {} not found", id)).into(),
            );
        }

        Ok(())
    }

    /// Get a component
    pub async fn get(&self, id: ComponentId) -> Result<Component> {
        let components = self.components.read().await;

        components
            .get(&id)
            .cloned()
            .ok_or_else(|| {
                IsolationError::ComponentNotFound(format!("Component {} not found", id)).into()
            })
    }

    /// Update component status
    pub async fn update_status(&self, id: ComponentId, status: ComponentStatus) -> Result<()> {
        let mut components = self.components.write().await;

        let component = components
            .get_mut(&id)
            .ok_or_else(|| IsolationError::ComponentNotFound(format!("Component {} not found", id)))?;

        component.status = status;
        Ok(())
    }

    /// List all components
    pub async fn list(&self) -> Vec<Component> {
        let components = self.components.read().await;
        components.values().cloned().collect()
    }

    /// List components by type
    pub async fn list_by_type(&self, component_type: &ComponentType) -> Vec<Component> {
        let components = self.components.read().await;
        components
            .values()
            .filter(|c| &c.component_type == component_type)
            .cloned()
            .collect()
    }

    /// List components by status
    pub async fn list_by_status(&self, status: ComponentStatus) -> Vec<Component> {
        let components = self.components.read().await;
        components
            .values()
            .filter(|c| c.status == status)
            .cloned()
            .collect()
    }

    /// Count components
    pub async fn count(&self) -> usize {
        let components = self.components.read().await;
        components.len()
    }

    /// Check if component exists
    pub async fn exists(&self, id: ComponentId) -> bool {
        let components = self.components.read().await;
        components.contains_key(&id)
    }
}

impl Default for ComponentRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_component() -> Component {
        Component {
            id: ComponentId::new(),
            component_type: ComponentType::McpServer {
                name: "test-mcp".to_string(),
                version: Some("1.0.0".to_string()),
            },
            status: ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: ComponentMetadata {
                name: "Test MCP".to_string(),
                version: Some("1.0.0".to_string()),
                description: Some("Test MCP server".to_string()),
                tags: HashMap::new(),
            },
        }
    }

    #[tokio::test]
    async fn test_register_component() {
        let registry = ComponentRegistry::new();
        let component = create_test_component();
        let id = component.id;

        registry.register(component).await.unwrap();

        assert!(registry.exists(id).await);
        assert_eq!(registry.count().await, 1);
    }

    #[tokio::test]
    async fn test_unregister_component() {
        let registry = ComponentRegistry::new();
        let component = create_test_component();
        let id = component.id;

        registry.register(component).await.unwrap();
        registry.unregister(id).await.unwrap();

        assert!(!registry.exists(id).await);
        assert_eq!(registry.count().await, 0);
    }

    #[tokio::test]
    async fn test_update_status() {
        let registry = ComponentRegistry::new();
        let component = create_test_component();
        let id = component.id;

        registry.register(component).await.unwrap();
        registry
            .update_status(id, ComponentStatus::Stopped)
            .await
            .unwrap();

        let updated = registry.get(id).await.unwrap();
        assert_eq!(updated.status, ComponentStatus::Stopped);
    }

    #[tokio::test]
    async fn test_list_by_status() {
        let registry = ComponentRegistry::new();

        let component1 = create_test_component();
        let component2 = {
            let mut c = create_test_component();
            c.status = ComponentStatus::Stopped;
            c
        };

        registry.register(component1).await.unwrap();
        registry.register(component2).await.unwrap();

        let running = registry.list_by_status(ComponentStatus::Running).await;
        let stopped = registry.list_by_status(ComponentStatus::Stopped).await;

        assert_eq!(running.len(), 1);
        assert_eq!(stopped.len(), 1);
    }

    #[test]
    fn test_component_id_display() {
        let id = ComponentId::new();
        let id_str = id.to_string();

        // Should be a valid UUID string
        assert_eq!(id_str.len(), 36); // UUID with hyphens
        assert!(id_str.contains('-'));
    }

    #[test]
    fn test_component_type_name() {
        assert_eq!(ComponentType::OsvmCore.name(), "OSVM Core");
        assert_eq!(
            ComponentType::Validator {
                network: "mainnet".to_string(),
                identity: None
            }
            .name(),
            "Validator"
        );
    }

    #[test]
    fn test_component_type_security_critical() {
        assert!(ComponentType::OsvmCore.is_security_critical());
        assert!(ComponentType::Validator {
            network: "mainnet".to_string(),
            identity: None
        }
        .is_security_critical());
        assert!(!ComponentType::McpServer {
            name: "test".to_string(),
            version: None
        }
        .is_security_critical());
    }

    #[test]
    fn test_component_status_operational() {
        assert!(ComponentStatus::Running.is_operational());
        assert!(!ComponentStatus::Starting.is_operational());
        assert!(!ComponentStatus::Failed.is_operational());
    }

    #[test]
    fn test_component_status_transitioning() {
        assert!(ComponentStatus::Starting.is_transitioning());
        assert!(ComponentStatus::Stopping.is_transitioning());
        assert!(ComponentStatus::Upgrading.is_transitioning());
        assert!(!ComponentStatus::Running.is_transitioning());
        assert!(!ComponentStatus::Stopped.is_transitioning());
    }
}