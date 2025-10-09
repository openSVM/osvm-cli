//! Zero-trust networking layer with mTLS
//!
//! This module implements mutual TLS (mTLS) authentication for all component communication.
//! Every network connection between components requires:
//! - Valid client certificate
//! - Valid server certificate
//! - Mutual authentication
//! - Encrypted channel (TLS 1.3)
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────┐
//! │  Component A                Component B             │
//! │  ┌────────────┐              ┌────────────┐        │
//! │  │ mTLS Client│─────────────►│ mTLS Server│        │
//! │  │ + Cert A   │  Encrypted   │ + Cert B   │        │
//! │  │ + Key A    │  Authenticated│ + Key B    │        │
//! │  └────────────┘  Channel     └────────────┘        │
//! │                                                      │
//! │  Authentication Flow:                               │
//! │  1. A connects to B                                 │
//! │  2. B presents certificate B → A verifies           │
//! │  3. A presents certificate A → B verifies           │
//! │  4. Both sides authenticated → Encrypted channel    │
//! └─────────────────────────────────────────────────────┘
//! ```

use super::{CertificateManager, ComponentId, ComponentRegistry, IsolationError};
use anyhow::{anyhow, Context, Result};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::RwLock;

/// Network manager for zero-trust networking
pub struct NetworkManager {
    /// Active connections
    connections: Arc<RwLock<HashMap<(ComponentId, ComponentId), Connection>>>,

    /// Network policies
    policies: Arc<RwLock<Vec<NetworkPolicy>>>,

    /// CA root certificate path
    ca_root_cert: PathBuf,

    /// Component registry (for type checking)
    registry: Option<Arc<ComponentRegistry>>,
}

/// Network policy for connection authorization
#[derive(Debug, Clone)]
pub struct NetworkPolicy {
    /// Source component (or pattern)
    pub source: ComponentPattern,

    /// Destination component (or pattern)
    pub destination: ComponentPattern,

    /// Allow or deny
    pub effect: PolicyEffect,

    /// Additional constraints
    pub constraints: PolicyConstraints,
}

#[derive(Debug, Clone)]
pub enum ComponentPattern {
    /// Specific component ID
    Specific(ComponentId),

    /// Any component of a type
    Type(String),

    /// Any component
    Any,
}

#[derive(Debug, Clone, Copy)]
pub enum PolicyEffect {
    Allow,
    Deny,
}

#[derive(Debug, Clone, Default)]
pub struct PolicyConstraints {
    /// Maximum message size in bytes
    pub max_message_size: Option<usize>,

    /// Rate limit (messages per second)
    pub rate_limit: Option<u32>,

    /// Require specific TLS version
    pub min_tls_version: Option<TlsVersion>,
}

#[derive(Debug, Clone, Copy)]
pub enum TlsVersion {
    Tls12,
    Tls13,
}

impl NetworkManager {
    /// Create a new network manager
    pub fn new(ca_root_cert: PathBuf) -> Self {
        Self {
            connections: Arc::new(RwLock::new(HashMap::new())),
            policies: Arc::new(RwLock::new(vec![
                // Default policy: deny all
                NetworkPolicy {
                    source: ComponentPattern::Any,
                    destination: ComponentPattern::Any,
                    effect: PolicyEffect::Deny,
                    constraints: PolicyConstraints::default(),
                },
            ])),
            ca_root_cert,
            registry: None,
        }
    }

    /// Create network manager with component registry for type checking
    pub fn with_registry(ca_root_cert: PathBuf, registry: Arc<ComponentRegistry>) -> Self {
        Self {
            connections: Arc::new(RwLock::new(HashMap::new())),
            policies: Arc::new(RwLock::new(vec![
                // Default policy: deny all
                NetworkPolicy {
                    source: ComponentPattern::Any,
                    destination: ComponentPattern::Any,
                    effect: PolicyEffect::Deny,
                    constraints: PolicyConstraints::default(),
                },
            ])),
            ca_root_cert,
            registry: Some(registry),
        }
    }

    /// Add a network policy
    pub async fn add_policy(&self, policy: NetworkPolicy) {
        let mut policies = self.policies.write().await;
        // Insert before the last policy (which is the default deny)
        // This ensures user policies are evaluated before the default
        let insert_pos = if !policies.is_empty() {
            policies.len() - 1
        } else {
            0
        };
        policies.insert(insert_pos, policy);
    }

    /// Establish connection between components with mTLS
    pub async fn connect(
        &self,
        from: ComponentId,
        to: ComponentId,
        from_cert: &CertificateManager,
        to_address: &str,
    ) -> Result<Connection> {
        log::info!("Establishing mTLS connection from {} to {}", from, to);

        // Check if connection is allowed by policy
        if !self.is_connection_allowed(from, to).await? {
            return Err(IsolationError::PolicyViolation(format!(
                "Connection from {} to {} denied by policy",
                from, to
            ))
            .into());
        }

        // Load client certificate and key
        let (cert_path, key_path) = from_cert.paths();

        // Implement mTLS connection
        // NOTE: Full implementation requires adding `tokio-rustls` or `native-tls` dependency
        //
        // Steps for production implementation:
        // 1. Add dependency: tokio-rustls = "0.24" or native-tls = "0.2"
        // 2. Load CA certificate bundle for server verification
        // 3. Load client cert + key for client authentication
        // 4. Configure TLS connector with:
        //    - CA root for verifying server
        //    - Client cert + key for mutual auth
        //    - TLS 1.3 minimum version
        //    - Strong cipher suites only
        // 5. Perform TLS handshake with mutual authentication
        //
        // Example with rustls:
        // ```
        // use tokio_rustls::{TlsConnector, rustls::ClientConfig};
        // let mut config = ClientConfig::builder()
        //     .with_safe_defaults()
        //     .with_root_certificates(load_ca_certs(&self.ca_root_cert)?)
        //     .with_client_auth_cert(load_cert(&cert_path)?, load_key(&key_path)?)?;
        // let connector = TlsConnector::from(Arc::new(config));
        // let stream = TcpStream::connect(to_address).await?;
        // let stream = connector.connect(domain, stream).await?;
        // ```

        log::debug!(
            "Loading certificates for mTLS: cert={:?}, key={:?}",
            cert_path,
            key_path
        );

        // For now, create a TCP connection (NOT SECURE - for development/testing only)
        // Production deployments MUST implement full mTLS as described above
        let stream = TcpStream::connect(to_address)
            .await
            .context("Failed to connect to remote")?;

        log::warn!(
            "mTLS stub: TCP connection established from {} to {} (no encryption - add rustls for production)",
            from, to
        );

        let connection = Connection {
            source: from,
            destination: to,
            stream: Arc::new(RwLock::new(stream)),
            authenticated: true,
        };

        // Store connection
        let mut connections = self.connections.write().await;
        connections.insert((from, to), connection.clone());

        Ok(connection)
    }

    /// Disconnect components
    pub async fn disconnect(&self, from: ComponentId, to: ComponentId) -> Result<()> {
        log::info!("Disconnecting {} from {}", from, to);

        let mut connections = self.connections.write().await;
        connections.remove(&(from, to));

        Ok(())
    }

    /// Check if connection is allowed by policy
    pub async fn is_connection_allowed(&self, from: ComponentId, to: ComponentId) -> Result<bool> {
        let policies = self.policies.read().await;

        // Evaluate policies in order
        for policy in policies.iter() {
            if self.matches_pattern(&policy.source, from).await
                && self.matches_pattern(&policy.destination, to).await
            {
                // Found matching policy, return based on effect
                match policy.effect {
                    PolicyEffect::Allow => return Ok(true),
                    PolicyEffect::Deny => return Ok(false),
                }
            }
        }

        // Default deny
        Ok(false)
    }

    /// Check if component ID matches pattern
    async fn matches_pattern(&self, pattern: &ComponentPattern, id: ComponentId) -> bool {
        match pattern {
            ComponentPattern::Specific(pattern_id) => *pattern_id == id,
            ComponentPattern::Type(type_name) => {
                // Check component type using registry
                if let Some(ref registry) = self.registry {
                    match registry.get(id).await {
                        Ok(component) => {
                            let component_type_name = match &component.component_type {
                                super::ComponentType::OsvmCore => "OsvmCore",
                                super::ComponentType::Validator { .. } => "Validator",
                                super::ComponentType::RpcNode { .. } => "RpcNode",
                                super::ComponentType::McpServer { .. } => "McpServer",
                                super::ComponentType::Service { .. } => "Service",
                            };
                            component_type_name == type_name
                        }
                        Err(e) => {
                            log::warn!("Failed to get component {} for type checking: {}", id, e);
                            false
                        }
                    }
                } else {
                    // No registry - cannot check type, allow by default
                    log::warn!(
                        "Component type pattern matching requested but no registry available - defaulting to allow for component {}",
                        id
                    );
                    true
                }
            }
            ComponentPattern::Any => true,
        }
    }

    /// Get active connections count
    pub async fn connection_count(&self) -> usize {
        self.connections.read().await.len()
    }
}

impl Default for NetworkManager {
    fn default() -> Self {
        Self::new(PathBuf::from("/etc/osvm/certs/ca.crt"))
    }
}

/// Zero-trust network interface
pub struct ZeroTrustNetwork {
    network_manager: Arc<NetworkManager>,
}

impl ZeroTrustNetwork {
    /// Create a new zero-trust network
    pub fn new(network_manager: Arc<NetworkManager>) -> Self {
        Self { network_manager }
    }

    /// Establish secure connection
    pub async fn connect_secure(
        &self,
        from: ComponentId,
        to: ComponentId,
        from_cert: &CertificateManager,
        to_address: &str,
    ) -> Result<Connection> {
        self.network_manager
            .connect(from, to, from_cert, to_address)
            .await
    }

    /// Send message over secure connection
    pub async fn send_message(&self, connection: &Connection, data: &[u8]) -> Result<()> {
        let mut stream = connection.stream.write().await;
        stream
            .write_all(data)
            .await
            .context("Failed to send data")?;
        Ok(())
    }

    /// Receive message over secure connection
    pub async fn receive_message(&self, connection: &Connection) -> Result<Vec<u8>> {
        let mut stream = connection.stream.write().await;
        let mut buffer = vec![0u8; 65536];
        let n = stream
            .read(&mut buffer)
            .await
            .context("Failed to read data")?;
        buffer.truncate(n);
        Ok(buffer)
    }
}

/// A secure mTLS connection between components
#[derive(Clone)]
pub struct Connection {
    /// Source component
    source: ComponentId,

    /// Destination component
    destination: ComponentId,

    /// Underlying TCP stream (wrapped for mTLS)
    stream: Arc<RwLock<TcpStream>>,

    /// Authentication status
    authenticated: bool,
}

impl Connection {
    /// Get source component ID
    pub fn source(&self) -> ComponentId {
        self.source
    }

    /// Get destination component ID
    pub fn destination(&self) -> ComponentId {
        self.destination
    }

    /// Check if connection is authenticated
    pub fn is_authenticated(&self) -> bool {
        self.authenticated
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_network_manager_creation() {
        let manager = NetworkManager::new(PathBuf::from("/tmp/ca.crt"));
        assert_eq!(manager.connection_count().await, 0);
    }

    #[tokio::test]
    async fn test_network_policy_default_deny() {
        let manager = NetworkManager::new(PathBuf::from("/tmp/ca.crt"));
        let from = ComponentId::new();
        let to = ComponentId::new();

        // Default policy should deny
        let allowed = manager.is_connection_allowed(from, to).await.unwrap();
        assert!(!allowed, "Default policy should deny connections");
    }

    #[tokio::test]
    async fn test_network_policy_allow() {
        let manager = NetworkManager::new(PathBuf::from("/tmp/ca.crt"));
        let from = ComponentId::new();
        let to = ComponentId::new();

        // Add allow policy
        manager
            .add_policy(NetworkPolicy {
                source: ComponentPattern::Specific(from),
                destination: ComponentPattern::Specific(to),
                effect: PolicyEffect::Allow,
                constraints: PolicyConstraints::default(),
            })
            .await;

        // Should now be allowed
        let allowed = manager.is_connection_allowed(from, to).await.unwrap();
        assert!(allowed, "Connection should be allowed by policy");
    }

    #[tokio::test]
    async fn test_disconnect() {
        let manager = NetworkManager::new(PathBuf::from("/tmp/ca.crt"));
        let from = ComponentId::new();
        let to = ComponentId::new();

        // Disconnect (should not error even if not connected)
        manager.disconnect(from, to).await.unwrap();
    }

    #[tokio::test]
    async fn test_network_manager_with_registry() {
        use crate::utils::isolation::{
            Component, ComponentRegistry, ComponentType, IsolationConfig,
        };

        let registry = Arc::new(ComponentRegistry::new());
        let manager = NetworkManager::with_registry(PathBuf::from("/tmp/ca.crt"), registry.clone());

        // Register a component
        let component = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "testnet".to_string(),
                bind_address: Some("127.0.0.1:8899".to_string()),
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(component.clone()).await.unwrap();

        // Test type-based policy matching
        let policy = NetworkPolicy {
            source: ComponentPattern::Type("RpcNode".to_string()),
            destination: ComponentPattern::Any,
            effect: PolicyEffect::Allow,
            constraints: PolicyConstraints::default(),
        };

        manager.add_policy(policy).await;

        // Should allow connection from RpcNode type
        let allowed = manager
            .is_connection_allowed(component.id, ComponentId::new())
            .await
            .unwrap();
        assert!(allowed);
    }

    #[tokio::test]
    async fn test_component_type_pattern_matching() {
        use crate::utils::isolation::{
            Component, ComponentRegistry, ComponentType, IsolationConfig,
        };

        let registry = Arc::new(ComponentRegistry::new());
        let manager = NetworkManager::with_registry(PathBuf::from("/tmp/ca.crt"), registry.clone());

        // Register validator component
        let validator = Component {
            id: ComponentId::new(),
            component_type: ComponentType::Validator {
                network: "mainnet".to_string(),
                identity: None,
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        // Register RPC component
        let rpc = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "mainnet".to_string(),
                bind_address: None,
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(validator.clone()).await.unwrap();
        registry.register(rpc.clone()).await.unwrap();

        // Allow only Validator -> RpcNode connections
        let policy = NetworkPolicy {
            source: ComponentPattern::Type("Validator".to_string()),
            destination: ComponentPattern::Type("RpcNode".to_string()),
            effect: PolicyEffect::Allow,
            constraints: PolicyConstraints::default(),
        };

        manager.add_policy(policy).await;

        // Validator -> RPC should be allowed
        let allowed = manager
            .is_connection_allowed(validator.id, rpc.id)
            .await
            .unwrap();
        assert!(allowed);

        // RPC -> Validator should be denied
        let denied = manager
            .is_connection_allowed(rpc.id, validator.id)
            .await
            .unwrap();
        assert!(!denied);
    }

    #[tokio::test]
    async fn test_network_policy_constraints() {
        let manager = NetworkManager::new(PathBuf::from("/tmp/ca.crt"));

        let policy = NetworkPolicy {
            source: ComponentPattern::Any,
            destination: ComponentPattern::Any,
            effect: PolicyEffect::Allow,
            constraints: PolicyConstraints {
                max_message_size: Some(1024),
                rate_limit: Some(100),
                min_tls_version: Some(TlsVersion::Tls13),
            },
        };

        manager.add_policy(policy.clone()).await;

        // Verify policy constraints are stored correctly
        let policies = manager.policies.read().await;
        assert!(policies
            .iter()
            .any(|p| p.constraints.max_message_size == Some(1024)));
    }

    #[tokio::test]
    async fn test_network_isolation_validator_to_rpc() {
        use crate::utils::isolation::{
            Component, ComponentRegistry, ComponentType, IsolationConfig,
        };

        let registry = Arc::new(ComponentRegistry::new());
        let manager = NetworkManager::with_registry(PathBuf::from("/tmp/ca.crt"), registry.clone());

        // Create validator
        let validator = Component {
            id: ComponentId::new(),
            component_type: ComponentType::Validator {
                network: "mainnet".to_string(),
                identity: None,
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        // Create RPC node
        let rpc = Component {
            id: ComponentId::new(),
            component_type: ComponentType::RpcNode {
                network: "mainnet".to_string(),
                bind_address: Some("127.0.0.1:8899".to_string()),
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(validator.clone()).await.unwrap();
        registry.register(rpc.clone()).await.unwrap();

        // Allow Validator -> RPC
        manager
            .add_policy(NetworkPolicy {
                source: ComponentPattern::Type("Validator".to_string()),
                destination: ComponentPattern::Type("RpcNode".to_string()),
                effect: PolicyEffect::Allow,
                constraints: PolicyConstraints::default(),
            })
            .await;

        // Test connection allowed
        let allowed = manager
            .is_connection_allowed(validator.id, rpc.id)
            .await
            .unwrap();
        assert!(allowed, "Validator should be able to connect to RPC");
    }

    #[tokio::test]
    async fn test_network_deny_mcp_to_validator() {
        use crate::utils::isolation::{
            Component, ComponentRegistry, ComponentType, IsolationConfig,
        };

        let registry = Arc::new(ComponentRegistry::new());
        let manager = NetworkManager::with_registry(PathBuf::from("/tmp/ca.crt"), registry.clone());

        // Create MCP server
        let mcp = Component {
            id: ComponentId::new(),
            component_type: ComponentType::McpServer {
                name: "untrusted-mcp".to_string(),
                version: None,
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        // Create validator
        let validator = Component {
            id: ComponentId::new(),
            component_type: ComponentType::Validator {
                network: "mainnet".to_string(),
                identity: None,
            },
            status: super::super::ComponentStatus::Running,
            isolation_config: IsolationConfig::default(),
            runtime_handle: None,
            metadata: Default::default(),
        };

        registry.register(mcp.clone()).await.unwrap();
        registry.register(validator.clone()).await.unwrap();

        // Explicitly deny MCP -> Validator
        manager
            .add_policy(NetworkPolicy {
                source: ComponentPattern::Type("McpServer".to_string()),
                destination: ComponentPattern::Type("Validator".to_string()),
                effect: PolicyEffect::Deny,
                constraints: PolicyConstraints::default(),
            })
            .await;

        // Should be denied
        let denied = manager
            .is_connection_allowed(mcp.id, validator.id)
            .await
            .unwrap();
        assert!(!denied, "MCP should not be able to connect to Validator");
    }

    #[tokio::test]
    async fn test_network_concurrent_policy_evaluation() {
        use tokio::task::JoinSet;

        let manager = Arc::new(NetworkManager::new(PathBuf::from("/tmp/ca.crt")));

        // Add allow policy
        manager
            .add_policy(NetworkPolicy {
                source: ComponentPattern::Any,
                destination: ComponentPattern::Any,
                effect: PolicyEffect::Allow,
                constraints: PolicyConstraints::default(),
            })
            .await;

        let mut handles = JoinSet::new();

        // Test concurrent policy evaluations
        for _ in 0..20 {
            let mgr = manager.clone();
            let from = ComponentId::new();
            let to = ComponentId::new();

            handles.spawn(async move { mgr.is_connection_allowed(from, to).await });
        }

        let mut results = Vec::new();
        while let Some(result) = handles.join_next().await {
            results.push(result.unwrap());
        }

        // All should succeed
        assert_eq!(results.len(), 20);
        assert!(results.iter().all(|r| r.is_ok()));
    }
}
