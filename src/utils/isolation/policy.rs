//! Policy engine for access control

use super::{ComponentId, ComponentType};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Access control policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    pub name: String,
    pub effect: PolicyEffect,
    pub subject: PolicySubject,
    pub resource: PolicyResource,
    pub action: PolicyAction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PolicyEffect {
    Allow,
    Deny,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum PolicySubject {
    Component { id: Option<ComponentId> },
    ComponentType { component_type: ComponentType },
    Any,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PolicyResource {
    Network,
    FileSystem,
    Keys,
    Other(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PolicyAction {
    Read,
    Write,
    Execute,
    Connect,
}

/// Policy engine for evaluating access control policies
pub struct PolicyEngine {
    policies: Vec<Policy>,
}

impl PolicyEngine {
    /// Create a new policy engine
    pub fn new() -> Self {
        Self { policies: vec![] }
    }

    /// Add a policy
    pub fn add_policy(&mut self, policy: Policy) {
        self.policies.push(policy);
    }

    /// Check if action is allowed
    pub fn check_allowed(
        &self,
        subject: &PolicySubject,
        resource: &PolicyResource,
        action: &PolicyAction,
    ) -> bool {
        // Evaluate all policies
        // Default deny if no matching policy found
        let mut has_deny = false;
        let mut has_allow = false;

        for policy in &self.policies {
            // Check if policy matches the request
            if self.matches_subject(subject, &policy.subject)
                && self.matches_resource(resource, &policy.resource)
                && self.matches_action(action, &policy.action)
            {
                match policy.effect {
                    PolicyEffect::Allow => has_allow = true,
                    PolicyEffect::Deny => has_deny = true,
                }
            }
        }

        // Explicit deny always wins (deny overrides allow)
        if has_deny {
            return false;
        }

        // Allow if there's an explicit allow policy
        has_allow
    }

    /// Check if subject matches
    fn matches_subject(&self, request: &PolicySubject, policy: &PolicySubject) -> bool {
        match (request, policy) {
            (_, PolicySubject::Any) => true,
            (PolicySubject::Component { id: req_id }, PolicySubject::Component { id: pol_id }) => {
                match (req_id, pol_id) {
                    (Some(rid), Some(pid)) => rid == pid,
                    (Some(_), None) => true, // Policy matches any component
                    _ => false,
                }
            }
            (
                PolicySubject::ComponentType {
                    component_type: req_type,
                },
                PolicySubject::ComponentType {
                    component_type: pol_type,
                },
            ) => req_type == pol_type,
            _ => false,
        }
    }

    /// Check if resource matches
    fn matches_resource(&self, request: &PolicyResource, policy: &PolicyResource) -> bool {
        match (request, policy) {
            (PolicyResource::Network, PolicyResource::Network) => true,
            (PolicyResource::FileSystem, PolicyResource::FileSystem) => true,
            (PolicyResource::Keys, PolicyResource::Keys) => true,
            (PolicyResource::Other(req), PolicyResource::Other(pol)) => req == pol,
            _ => false,
        }
    }

    /// Check if action matches
    fn matches_action(&self, request: &PolicyAction, policy: &PolicyAction) -> bool {
        std::mem::discriminant(request) == std::mem::discriminant(policy)
    }
}

impl Default for PolicyEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_policy_engine_allow() {
        let mut engine = PolicyEngine::new();

        let policy = Policy {
            name: "test-policy".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Read,
        };

        engine.add_policy(policy);

        assert!(engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_engine_deny() {
        let mut engine = PolicyEngine::new();

        let deny_policy = Policy {
            name: "deny-write".to_string(),
            effect: PolicyEffect::Deny,
            subject: PolicySubject::Any,
            resource: PolicyResource::FileSystem,
            action: PolicyAction::Write,
        };

        engine.add_policy(deny_policy);

        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::FileSystem,
            &PolicyAction::Write
        ));
    }

    #[test]
    fn test_policy_engine_deny_overrides_allow() {
        let mut engine = PolicyEngine::new();

        let allow_policy = Policy {
            name: "allow-network".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Connect,
        };

        let deny_policy = Policy {
            name: "deny-network".to_string(),
            effect: PolicyEffect::Deny,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Connect,
        };

        engine.add_policy(allow_policy);
        engine.add_policy(deny_policy);

        // Deny should override allow
        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Connect
        ));
    }

    #[test]
    fn test_policy_engine_no_match() {
        let mut engine = PolicyEngine::new();

        let policy = Policy {
            name: "allow-read".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Read,
        };

        engine.add_policy(policy);

        // No matching policy for Write action - should return false
        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Write
        ));
    }

    #[test]
    fn test_policy_component_type_matching() {
        let mut engine = PolicyEngine::new();

        let policy = Policy {
            name: "validator-only".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "testnet".to_string(),
                    identity: None,
                },
            },
            resource: PolicyResource::Keys,
            action: PolicyAction::Read,
        };

        engine.add_policy(policy);

        // Validator should be allowed
        assert!(engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "testnet".to_string(),
                    identity: None,
                },
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));

        // RPC should not be allowed
        assert!(!engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::RpcNode {
                    network: "testnet".to_string(),
                    bind_address: None,
                },
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_empty_engine() {
        let engine = PolicyEngine::new();

        // No policies - should deny everything
        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_multiple_resources() {
        let mut engine = PolicyEngine::new();

        // Allow network read
        engine.add_policy(Policy {
            name: "allow-network-read".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Read,
        });

        // Allow filesystem write
        engine.add_policy(Policy {
            name: "allow-fs-write".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::FileSystem,
            action: PolicyAction::Write,
        });

        // Network read allowed
        assert!(engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Read
        ));

        // Filesystem write allowed
        assert!(engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::FileSystem,
            &PolicyAction::Write
        ));

        // Network write NOT allowed
        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Write
        ));
    }

    #[test]
    fn test_policy_component_id_matching() {
        let mut engine = PolicyEngine::new();
        let component_id = ComponentId::new();

        // Allow specific component to access keys
        engine.add_policy(Policy {
            name: "specific-component".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Component {
                id: Some(component_id),
            },
            resource: PolicyResource::Keys,
            action: PolicyAction::Read,
        });

        // Specific component allowed
        assert!(engine.check_allowed(
            &PolicySubject::Component {
                id: Some(component_id),
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));

        // Different component denied
        assert!(!engine.check_allowed(
            &PolicySubject::Component {
                id: Some(ComponentId::new()),
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_wildcard_component_id() {
        let mut engine = PolicyEngine::new();

        // Allow any component (with id: None) to connect
        engine.add_policy(Policy {
            name: "any-component".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Component { id: None },
            resource: PolicyResource::Network,
            action: PolicyAction::Connect,
        });

        // Any specific component should be allowed
        assert!(engine.check_allowed(
            &PolicySubject::Component {
                id: Some(ComponentId::new()),
            },
            &PolicyResource::Network,
            &PolicyAction::Connect
        ));
    }

    #[test]
    fn test_policy_other_resource() {
        let mut engine = PolicyEngine::new();

        engine.add_policy(Policy {
            name: "allow-database".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Other("database".to_string()),
            action: PolicyAction::Read,
        });

        assert!(engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Other("database".to_string()),
            &PolicyAction::Read
        ));

        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Other("cache".to_string()),
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_all_actions() {
        let mut engine = PolicyEngine::new();

        // Test all action types
        for action in [
            PolicyAction::Read,
            PolicyAction::Write,
            PolicyAction::Execute,
            PolicyAction::Connect,
        ] {
            engine.add_policy(Policy {
                name: format!("allow-{:?}", action),
                effect: PolicyEffect::Allow,
                subject: PolicySubject::Any,
                resource: PolicyResource::Network,
                action: action.clone(),
            });

            assert!(engine.check_allowed(&PolicySubject::Any, &PolicyResource::Network, &action));
        }
    }

    #[test]
    fn test_security_policy_validation() {
        let mut engine = PolicyEngine::new();

        // Security-critical components can access keys
        engine.add_policy(Policy {
            name: "validator-key-access".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "mainnet".to_string(),
                    identity: None,
                },
            },
            resource: PolicyResource::Keys,
            action: PolicyAction::Read,
        });

        // Non-critical components explicitly denied
        engine.add_policy(Policy {
            name: "deny-mcp-keys".to_string(),
            effect: PolicyEffect::Deny,
            subject: PolicySubject::ComponentType {
                component_type: ComponentType::McpServer {
                    name: "test".to_string(),
                    version: None,
                },
            },
            resource: PolicyResource::Keys,
            action: PolicyAction::Read,
        });

        // Validator can access keys
        assert!(engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "mainnet".to_string(),
                    identity: None,
                },
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));

        // MCP server explicitly denied
        assert!(!engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::McpServer {
                    name: "test".to_string(),
                    version: None,
                },
            },
            &PolicyResource::Keys,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_layered_policy_evaluation() {
        let mut engine = PolicyEngine::new();

        // Layer 1: Allow all network reads
        engine.add_policy(Policy {
            name: "allow-network-read".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Read,
        });

        // Layer 2: Deny specific component
        let blocked_component = ComponentId::new();
        engine.add_policy(Policy {
            name: "block-specific-component".to_string(),
            effect: PolicyEffect::Deny,
            subject: PolicySubject::Component {
                id: Some(blocked_component),
            },
            resource: PolicyResource::Network,
            action: PolicyAction::Read,
        });

        // Random component can read network
        assert!(engine.check_allowed(
            &PolicySubject::Component {
                id: Some(ComponentId::new()),
            },
            &PolicyResource::Network,
            &PolicyAction::Read
        ));

        // Blocked component cannot (deny overrides allow)
        assert!(!engine.check_allowed(
            &PolicySubject::Component {
                id: Some(blocked_component),
            },
            &PolicyResource::Network,
            &PolicyAction::Read
        ));
    }

    #[test]
    fn test_policy_resource_isolation() {
        let mut engine = PolicyEngine::new();

        // Allow network but not filesystem
        engine.add_policy(Policy {
            name: "network-only".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::Any,
            resource: PolicyResource::Network,
            action: PolicyAction::Connect,
        });

        assert!(engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Network,
            &PolicyAction::Connect
        ));

        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::FileSystem,
            &PolicyAction::Connect
        ));

        assert!(!engine.check_allowed(
            &PolicySubject::Any,
            &PolicyResource::Keys,
            &PolicyAction::Connect
        ));
    }

    #[test]
    fn test_complex_policy_scenarios() {
        let mut engine = PolicyEngine::new();

        // Scenario: Only validators on mainnet can execute operations
        engine.add_policy(Policy {
            name: "mainnet-validator-execute".to_string(),
            effect: PolicyEffect::Allow,
            subject: PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "mainnet".to_string(),
                    identity: None,
                },
            },
            resource: PolicyResource::Other("transactions".to_string()),
            action: PolicyAction::Execute,
        });

        // Testnet validators denied
        engine.add_policy(Policy {
            name: "deny-testnet-execute".to_string(),
            effect: PolicyEffect::Deny,
            subject: PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "testnet".to_string(),
                    identity: None,
                },
            },
            resource: PolicyResource::Other("transactions".to_string()),
            action: PolicyAction::Execute,
        });

        // Mainnet allowed
        assert!(engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "mainnet".to_string(),
                    identity: None,
                },
            },
            &PolicyResource::Other("transactions".to_string()),
            &PolicyAction::Execute
        ));

        // Testnet denied
        assert!(!engine.check_allowed(
            &PolicySubject::ComponentType {
                component_type: ComponentType::Validator {
                    network: "testnet".to_string(),
                    identity: None,
                },
            },
            &PolicyResource::Other("transactions".to_string()),
            &PolicyAction::Execute
        ));
    }
}
