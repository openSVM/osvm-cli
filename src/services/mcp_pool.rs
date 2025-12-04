//! Dynamic auto-scaling MCP connection pool
//!
//! Provides parallel MCP access by maintaining a pool of McpService instances
//! that scales up under load and scales down after idle timeout.

use anyhow::Result;
use serde_json::Value;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{Mutex, Semaphore};

use crate::services::mcp_service::McpService;

/// Configuration for the MCP pool
#[derive(Clone)]
pub struct McpPoolConfig {
    /// Minimum instances to keep alive (default: 1)
    pub min_instances: usize,
    /// Maximum instances allowed (default: 8)
    pub max_instances: usize,
    /// Idle timeout before scaling down (default: 30s)
    pub idle_timeout: Duration,
    /// Enable debug logging
    pub debug: bool,
}

impl Default for McpPoolConfig {
    fn default() -> Self {
        Self {
            min_instances: 1,
            max_instances: 8,
            idle_timeout: Duration::from_secs(30),
            debug: false,
        }
    }
}

/// A pooled MCP instance with usage tracking
struct PooledInstance {
    /// The actual MCP service
    service: Arc<Mutex<McpService>>,
    /// Whether this instance is currently in use
    busy: AtomicBool,
    /// Last time this instance was used
    last_used: Mutex<Instant>,
    /// Instance ID for debugging
    id: usize,
    /// Cached tool→server mapping (populated on first use)
    tool_server_map: Mutex<std::collections::HashMap<String, String>>,
}

impl PooledInstance {
    async fn new(id: usize, debug: bool) -> Result<Self> {
        let mut service = McpService::new_with_debug(debug);
        service.load_config()?;

        // Initialize all servers and build tool→server map
        let servers: Vec<String> = service
            .list_servers()
            .iter()
            .map(|(id, _)| (*id).clone())
            .collect();

        let mut tool_map = std::collections::HashMap::new();

        for server_id in servers {
            if service.initialize_server(&server_id).await.is_ok() {
                // Cache all tools from this server
                if let Ok(tools) = service.list_tools(&server_id).await {
                    for tool in tools {
                        tool_map.insert(tool.name.clone(), server_id.clone());
                    }
                }
            }
        }

        Ok(Self {
            service: Arc::new(Mutex::new(service)),
            busy: AtomicBool::new(false),
            last_used: Mutex::new(Instant::now()),
            id,
            tool_server_map: Mutex::new(tool_map),
        })
    }

    fn is_busy(&self) -> bool {
        self.busy.load(Ordering::SeqCst)
    }

    fn try_acquire(&self) -> bool {
        self.busy
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
    }

    fn release(&self) {
        self.busy.store(false, Ordering::SeqCst);
    }

    async fn update_last_used(&self) {
        *self.last_used.lock().await = Instant::now();
    }

    async fn idle_duration(&self) -> Duration {
        self.last_used.lock().await.elapsed()
    }
}

/// RAII guard that releases the instance back to pool on drop
pub struct McpGuard {
    instance: Arc<PooledInstance>,
    pool: Arc<McpPoolInner>,
}

impl McpGuard {
    /// Call an MCP tool through this pooled instance (uses cached tool→server map)
    pub async fn call_tool(&self, tool_name: &str, params: Option<Value>) -> Result<Value> {
        // Look up server from cache (fast path)
        let server_id = {
            let map = self.instance.tool_server_map.lock().await;
            map.get(tool_name).cloned()
        };

        if let Some(server_id) = server_id {
            let mut service = self.instance.service.lock().await;
            let result = service.call_tool(&server_id, tool_name, params).await?;
            self.instance.update_last_used().await;
            return Ok(result);
        }

        Err(anyhow::anyhow!(
            "Tool '{}' not found in any MCP server",
            tool_name
        ))
    }

    /// Get the underlying service for direct access (use sparingly)
    pub async fn service(&self) -> tokio::sync::MutexGuard<'_, McpService> {
        self.instance.service.lock().await
    }
}

impl Drop for McpGuard {
    fn drop(&mut self) {
        self.instance.release();
        self.pool.active_count.fetch_sub(1, Ordering::SeqCst);

        if self.pool.config.debug {
            eprintln!("[McpPool] Released instance #{}", self.instance.id);
        }
    }
}

/// Inner pool state (shared via Arc)
struct McpPoolInner {
    instances: Mutex<Vec<Arc<PooledInstance>>>,
    config: McpPoolConfig,
    next_id: AtomicUsize,
    active_count: AtomicUsize,
    /// Semaphore to limit concurrent instance creation
    creation_semaphore: Semaphore,
}

/// Dynamic auto-scaling MCP connection pool
pub struct McpPool {
    inner: Arc<McpPoolInner>,
    /// Handle to the cleanup task
    _cleanup_handle: Option<tokio::task::JoinHandle<()>>,
}

impl McpPool {
    /// Create a new MCP pool with default configuration
    pub async fn new() -> Result<Self> {
        Self::with_config(McpPoolConfig::default()).await
    }

    /// Create a new MCP pool with custom configuration
    pub async fn with_config(config: McpPoolConfig) -> Result<Self> {
        let inner = Arc::new(McpPoolInner {
            instances: Mutex::new(Vec::new()),
            config: config.clone(),
            next_id: AtomicUsize::new(0),
            active_count: AtomicUsize::new(0),
            creation_semaphore: Semaphore::new(1), // Only one creation at a time
        });

        // Create minimum instances
        {
            let mut instances = inner.instances.lock().await;
            for _ in 0..config.min_instances {
                let id = inner.next_id.fetch_add(1, Ordering::SeqCst);
                if config.debug {
                    eprintln!("[McpPool] Creating initial instance #{}", id);
                }
                let instance = PooledInstance::new(id, config.debug).await?;
                instances.push(Arc::new(instance));
            }
        }

        // Start cleanup task
        let cleanup_inner = Arc::clone(&inner);
        let cleanup_handle = tokio::spawn(async move {
            Self::cleanup_loop(cleanup_inner).await;
        });

        Ok(Self {
            inner,
            _cleanup_handle: Some(cleanup_handle),
        })
    }

    /// Acquire an MCP instance from the pool
    ///
    /// If all instances are busy and we're under max_instances, spawns a new one.
    /// Returns an RAII guard that releases the instance on drop.
    pub async fn acquire(&self) -> Result<McpGuard> {
        // First, try to acquire an existing idle instance
        {
            let instances = self.inner.instances.lock().await;
            for instance in instances.iter() {
                if instance.try_acquire() {
                    self.inner.active_count.fetch_add(1, Ordering::SeqCst);
                    if self.inner.config.debug {
                        eprintln!("[McpPool] Acquired existing instance #{}", instance.id);
                    }
                    return Ok(McpGuard {
                        instance: Arc::clone(instance),
                        pool: Arc::clone(&self.inner),
                    });
                }
            }
        }

        // All instances busy - try to create a new one
        let current_count = {
            let instances = self.inner.instances.lock().await;
            instances.len()
        };

        if current_count < self.inner.config.max_instances {
            // Acquire creation semaphore to prevent thundering herd
            let _permit = self.inner.creation_semaphore.acquire().await?;

            // Double-check after acquiring semaphore
            let mut instances = self.inner.instances.lock().await;

            // Maybe another task created one while we waited
            for instance in instances.iter() {
                if instance.try_acquire() {
                    self.inner.active_count.fetch_add(1, Ordering::SeqCst);
                    if self.inner.config.debug {
                        eprintln!(
                            "[McpPool] Acquired instance #{} (created by another task)",
                            instance.id
                        );
                    }
                    return Ok(McpGuard {
                        instance: Arc::clone(instance),
                        pool: Arc::clone(&self.inner),
                    });
                }
            }

            // Still need to create one
            if instances.len() < self.inner.config.max_instances {
                let id = self.inner.next_id.fetch_add(1, Ordering::SeqCst);
                if self.inner.config.debug {
                    eprintln!(
                        "[McpPool] Scaling UP: creating instance #{} (total: {})",
                        id,
                        instances.len() + 1
                    );
                }

                let instance = Arc::new(PooledInstance::new(id, self.inner.config.debug).await?);
                instance.try_acquire(); // Mark as busy immediately
                instances.push(Arc::clone(&instance));

                self.inner.active_count.fetch_add(1, Ordering::SeqCst);

                return Ok(McpGuard {
                    instance,
                    pool: Arc::clone(&self.inner),
                });
            }
        }

        // At max capacity - wait for one to become available
        if self.inner.config.debug {
            eprintln!(
                "[McpPool] At max capacity ({}), waiting for available instance...",
                self.inner.config.max_instances
            );
        }

        loop {
            tokio::time::sleep(Duration::from_millis(50)).await;

            let instances = self.inner.instances.lock().await;
            for instance in instances.iter() {
                if instance.try_acquire() {
                    self.inner.active_count.fetch_add(1, Ordering::SeqCst);
                    if self.inner.config.debug {
                        eprintln!("[McpPool] Acquired instance #{} after waiting", instance.id);
                    }
                    return Ok(McpGuard {
                        instance: Arc::clone(instance),
                        pool: Arc::clone(&self.inner),
                    });
                }
            }
        }
    }

    /// Get current pool statistics
    pub async fn stats(&self) -> PoolStats {
        let instances = self.inner.instances.lock().await;
        let total = instances.len();
        let busy = instances.iter().filter(|i| i.is_busy()).count();

        PoolStats {
            total_instances: total,
            busy_instances: busy,
            idle_instances: total - busy,
            max_instances: self.inner.config.max_instances,
        }
    }

    /// Background cleanup loop
    async fn cleanup_loop(inner: Arc<McpPoolInner>) {
        let check_interval = Duration::from_secs(5);

        loop {
            tokio::time::sleep(check_interval).await;

            let mut instances = inner.instances.lock().await;
            let min = inner.config.min_instances;
            let timeout = inner.config.idle_timeout;

            // Find idle instances to remove
            let mut to_remove = Vec::new();

            for (idx, instance) in instances.iter().enumerate() {
                // Don't remove if we'd go below minimum
                if instances.len() - to_remove.len() <= min {
                    break;
                }

                // Don't remove busy instances
                if instance.is_busy() {
                    continue;
                }

                // Check idle duration
                if instance.idle_duration().await > timeout {
                    to_remove.push(idx);
                    if inner.config.debug {
                        eprintln!(
                            "[McpPool] Scaling DOWN: removing idle instance #{}",
                            instance.id
                        );
                    }
                }
            }

            // Remove in reverse order to preserve indices
            for idx in to_remove.into_iter().rev() {
                instances.remove(idx);
            }
        }
    }
}

/// Pool statistics
#[derive(Debug, Clone)]
pub struct PoolStats {
    pub total_instances: usize,
    pub busy_instances: usize,
    pub idle_instances: usize,
    pub max_instances: usize,
}

impl std::fmt::Display for PoolStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}/{} busy, {}/{} max",
            self.busy_instances, self.total_instances, self.total_instances, self.max_instances
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_pool_config_default() {
        let config = McpPoolConfig::default();
        assert_eq!(config.min_instances, 1);
        assert_eq!(config.max_instances, 8);
        assert_eq!(config.idle_timeout, Duration::from_secs(30));
    }
}
