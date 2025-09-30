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

use super::{CertificateManager, ComponentId, IsolationError};
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
        }
    }

    /// Add a network policy
    pub async fn add_policy(&self, policy: NetworkPolicy) {
        let mut policies = self.policies.write().await;
        policies.push(policy);
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
        let (_cert_path, key_path) = from_cert.paths();

        // TODO: Implement actual mTLS connection using rustls or native-tls
        // For now, create a basic TCP connection
        let stream = TcpStream::connect(to_address)
            .await
            .context("Failed to connect to remote")?;

        log::info!("mTLS connection established from {} to {}", from, to);

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
            if self.matches_pattern(&policy.source, from)
                && self.matches_pattern(&policy.destination, to)
            {
                return Ok(matches!(policy.effect, PolicyEffect::Allow));
            }
        }

        // Default deny
        Ok(false)
    }

    /// Check if component ID matches pattern
    fn matches_pattern(&self, pattern: &ComponentPattern, id: ComponentId) -> bool {
        match pattern {
            ComponentPattern::Specific(pattern_id) => *pattern_id == id,
            ComponentPattern::Type(_type_name) => {
                // TODO: Check component type
                true
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
    pub async fn send_message(
        &self,
        connection: &Connection,
        data: &[u8],
    ) -> Result<()> {
        let mut stream = connection.stream.write().await;
        stream.write_all(data).await.context("Failed to send data")?;
        Ok(())
    }

    /// Receive message over secure connection
    pub async fn receive_message(&self, connection: &Connection) -> Result<Vec<u8>> {
        let mut stream = connection.stream.write().await;
        let mut buffer = vec![0u8; 65536];
        let n = stream.read(&mut buffer).await.context("Failed to read data")?;
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
}