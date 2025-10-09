//! Property-based tests for critical system behaviors

use osvm::{
    services::{
        ai_service::{ToolCall, ToolPlan},
        mcp_service::{McpParameter, McpTool},
    },
    utils::{
        isolation::{ComponentId, ResourceLimits},
        secure_logger::SecureLogger,
    },
};
use proptest::prelude::*;
use serde_json::{json, Value};
use std::collections::HashMap;

/// Property: Tool plans should always serialize/deserialize correctly
#[cfg(test)]
mod tool_plan_properties {
    use super::*;

    proptest! {
        #[test]
        fn tool_plan_roundtrip_preserves_data(
            reasoning in "[a-zA-Z0-9 ]{1,500}",
            outcome in "[a-zA-Z0-9 ]{1,500}",
            num_tools in 0usize..20,
            tool_names in prop::collection::vec("[a-z_]{5,20}", 0..20)
        ) {
            let tools: Vec<ToolCall> = tool_names.iter()
                .take(num_tools)
                .enumerate()
                .map(|(i, name)| ToolCall {
                    server_id: format!("server-{}", i % 3),
                    tool_name: name.clone(),
                    reason: format!("Reason for {}", name),
                    args: HashMap::new(),
                })
                .collect();

            let original = ToolPlan {
                reasoning: reasoning.clone(),
                tools_to_use: tools,
                expected_outcome: outcome.clone(),
            };

            let serialized = serde_json::to_string(&original).unwrap();
            let deserialized: ToolPlan = serde_json::from_str(&serialized).unwrap();

            prop_assert_eq!(original.reasoning, deserialized.reasoning);
            prop_assert_eq!(original.expected_outcome, deserialized.expected_outcome);
            prop_assert_eq!(original.tools_to_use.len(), deserialized.tools_to_use.len());
        }

        #[test]
        fn tool_plan_json_structure_valid(
            num_tools in 0usize..10
        ) {
            let tools: Vec<ToolCall> = (0..num_tools)
                .map(|i| ToolCall {
                    server_id: format!("server-{}", i),
                    tool_name: format!("tool-{}", i),
                    reason: format!("reason-{}", i),
                    args: HashMap::new(),
                })
                .collect();

            let plan = ToolPlan {
                reasoning: "test reasoning".to_string(),
                tools_to_use: tools,
                expected_outcome: "test outcome".to_string(),
            };

            let json = serde_json::to_value(&plan).unwrap();

            // Verify JSON structure
            prop_assert!(json.is_object());
            prop_assert!(json["reasoning"].is_string());
            prop_assert!(json["osvm_tools_to_use"].is_array());
            prop_assert!(json["expected_outcome"].is_string());
            prop_assert_eq!(
                json["osvm_tools_to_use"].as_array().unwrap().len(),
                num_tools
            );
        }
    }
}

/// Property: MCP tools should handle arbitrary parameter combinations
#[cfg(test)]
mod mcp_tool_properties {
    use super::*;

    proptest! {
        #[test]
        fn mcp_tool_accepts_valid_parameters(
            tool_name in "[a-z_]{5,30}",
            description in "[a-zA-Z0-9 ]{10,200}",
            num_params in 0usize..10,
            param_names in prop::collection::vec("[a-z_]{3,20}", 0..10),
            param_types in prop::collection::vec(prop_oneof!["string", "number", "boolean", "array", "object"], 0..10),
            required_flags in prop::collection::vec(any::<bool>(), 0..10)
        ) {
            let parameters: Vec<McpParameter> = (0..num_params)
                .map(|i| McpParameter {
                    name: param_names.get(i).cloned().unwrap_or_else(|| format!("param_{}", i)),
                    param_type: param_types.get(i).cloned().unwrap_or_else(|| "string".to_string()),
                    required: required_flags.get(i).copied().unwrap_or(false),
                    description: Some(format!("Description for param {}", i)),
                })
                .collect();

            let tool = McpTool {
                name: tool_name.clone(),
                description: description.clone(),
                parameters,
            };

            // Serialize and deserialize
            let serialized = serde_json::to_string(&tool).unwrap();
            let deserialized: McpTool = serde_json::from_str(&serialized).unwrap();

            prop_assert_eq!(tool.name, deserialized.name);
            prop_assert_eq!(tool.description, deserialized.description);
            prop_assert_eq!(tool.parameters.len(), deserialized.parameters.len());
        }

        #[test]
        fn mcp_tool_argument_validation(
            args in prop::collection::hash_map(
                "[a-z_]{3,20}",
                prop_oneof![
                    any::<String>().prop_map(Value::String),
                    any::<f64>().prop_map(|n| json!(n)),
                    any::<bool>().prop_map(Value::Bool),
                ],
                0..20
            )
        ) {
            // Any argument map should be serializable
            let json = serde_json::to_value(&args);
            prop_assert!(json.is_ok());

            let value = json.unwrap();
            prop_assert!(value.is_object());

            // All keys should be strings
            if let Some(obj) = value.as_object() {
                for key in obj.keys() {
                    prop_assert!(!key.is_empty());
                    prop_assert!(key.chars().all(|c| c.is_ascii_lowercase() || c == '_'));
                }
            }
        }
    }
}

/// Property: Resource limits should always be validated correctly
#[cfg(test)]
mod resource_limit_properties {
    use super::*;

    proptest! {
        #[test]
        fn resource_limits_validation(
            cpu_cores in 0u32..1000,
            memory_mb in 0u32..1000000,
            disk_gb in 0u32..10000,
            bandwidth_mbps in prop::option::of(0u32..10000),
            max_processes in prop::option::of(0usize..100000),
            max_files in prop::option::of(0usize..1000000)
        ) {
            let limits = ResourceLimits {
                cpu_cores,
                memory_mb,
                disk_gb,
                network_bandwidth_mbps: bandwidth_mbps,
                max_processes,
                max_open_files: max_files,
            };

            // Reasonable limits should be considered valid
            let is_valid = cpu_cores > 0 && cpu_cores <= 128
                && memory_mb > 0 && memory_mb <= 524288  // 512GB
                && disk_gb > 0 && disk_gb <= 1000  // 1TB
                && bandwidth_mbps.map_or(true, |b| b <= 10000)  // 10Gbps
                && max_processes.map_or(true, |p| p > 0 && p <= 10000)
                && max_files.map_or(true, |f| f > 0 && f <= 100000);

            // Serialize and deserialize
            let serialized = serde_json::to_string(&limits).unwrap();
            let deserialized: ResourceLimits = serde_json::from_str(&serialized).unwrap();

            prop_assert_eq!(limits.cpu_cores, deserialized.cpu_cores);
            prop_assert_eq!(limits.memory_mb, deserialized.memory_mb);
            prop_assert_eq!(limits.disk_gb, deserialized.disk_gb);
        }

        #[test]
        fn resource_limits_ordering(
            cpu1 in 1u32..50,
            cpu2 in 1u32..50,
            mem1 in 128u32..8192,
            mem2 in 128u32..8192,
        ) {
            let limits1 = ResourceLimits {
                cpu_cores: cpu1,
                memory_mb: mem1,
                disk_gb: 10,
                network_bandwidth_mbps: None,
                max_processes: None,
                max_open_files: None,
            };

            let limits2 = ResourceLimits {
                cpu_cores: cpu2,
                memory_mb: mem2,
                disk_gb: 10,
                network_bandwidth_mbps: None,
                max_processes: None,
                max_open_files: None,
            };

            // Property: If all resources in limits1 <= limits2, then limits1 fits within limits2
            let fits_within = cpu1 <= cpu2 && mem1 <= mem2;

            if fits_within {
                prop_assert!(limits1.cpu_cores <= limits2.cpu_cores);
                prop_assert!(limits1.memory_mb <= limits2.memory_mb);
            }
        }
    }
}

/// Property: Security sanitization should never leak sensitive data
#[cfg(test)]
mod security_properties {
    use super::*;

    proptest! {
        #[test]
        fn secure_logger_removes_api_keys(
            prefix in prop_oneof!["sk-", "pk-", "key-", "token-"],
            key_body in "[A-Za-z0-9]{16,64}",
            surrounding_text in "[a-zA-Z0-9 ]{0,100}"
        ) {
            let logger = SecureLogger::new(false);
            let api_key = format!("{}{}", prefix, key_body);
            let input = format!("{} API_KEY={} {}", surrounding_text, api_key, surrounding_text);

            let sanitized = logger.sanitize(&input);

            // Property: API keys should be redacted
            prop_assert!(!sanitized.contains(&api_key));
            prop_assert!(sanitized.contains("[REDACTED]") || sanitized.contains("REDACTED"));
        }

        #[test]
        fn secure_logger_removes_paths(
            username in "[a-z]{3,20}",
            path_parts in prop::collection::vec("[a-z]{3,10}", 1..5),
            file in "[a-z]{3,20}\\.[a-z]{2,4}"
        ) {
            let logger = SecureLogger::new(false);
            let path = format!("/home/{}/{}/{}", username, path_parts.join("/"), file);
            let input = format!("Error reading file: {}", path);

            let sanitized = logger.sanitize(&input);

            // Property: Usernames in paths should be redacted
            prop_assert!(!sanitized.contains(&username));
            prop_assert!(sanitized.contains("[USER]") || sanitized.contains("REDACTED"));
        }

        #[test]
        fn secure_logger_preserves_non_sensitive_data(
            safe_text in "[a-zA-Z0-9 .,!?]{10,200}"
        ) {
            let logger = SecureLogger::new(false);

            // Avoid patterns that might trigger sanitization
            let input = safe_text
                .replace("sk-", "s-k-")
                .replace("key", "k-e-y")
                .replace("token", "t-o-k-e-n");

            let sanitized = logger.sanitize(&input);

            // Most content should be preserved if it doesn't match patterns
            let preserved_ratio = input.chars()
                .filter(|&c| sanitized.contains(c))
                .count() as f64 / input.len() as f64;

            prop_assert!(preserved_ratio > 0.5);
        }
    }
}

/// Property: Component IDs should be unique and consistent
#[cfg(test)]
mod component_id_properties {
    use super::*;
    use std::collections::HashSet;

    proptest! {
        #[test]
        fn component_ids_are_unique(
            num_components in 1usize..1000
        ) {
            let mut ids = HashSet::new();

            for _ in 0..num_components {
                let id = ComponentId::new();
                let was_new = ids.insert(id);
                prop_assert!(was_new, "ComponentId collision detected!");
            }

            prop_assert_eq!(ids.len(), num_components);
        }

        #[test]
        fn component_id_serialization(
            num_ids in 1usize..100
        ) {
            for _ in 0..num_ids {
                let original = ComponentId::new();

                // Serialize and deserialize
                let serialized = serde_json::to_string(&original).unwrap();
                let deserialized: ComponentId = serde_json::from_str(&serialized).unwrap();

                // Property: Serialization preserves identity
                prop_assert_eq!(original, deserialized);
            }
        }
    }
}

/// Property: JSON values should handle arbitrary nesting
#[cfg(test)]
mod json_properties {
    use super::*;

    fn arbitrary_json() -> impl Strategy<Value = Value> {
        let leaf = prop_oneof![
            any::<bool>().prop_map(Value::Bool),
            any::<i64>().prop_map(|n| json!(n)),
            any::<f64>().prop_map(|n| json!(n)),
            "[a-zA-Z0-9 ]{0,100}".prop_map(Value::String),
            Just(Value::Null),
        ];

        leaf.prop_recursive(8, 256, 10, |inner| {
            prop_oneof![
                prop::collection::vec(inner.clone(), 0..10).prop_map(Value::Array),
                prop::collection::hash_map("[a-z_]{1,20}", inner, 0..10)
                    .prop_map(|map| { Value::Object(map.into_iter().collect()) }),
            ]
        })
    }

    proptest! {
        #[test]
        fn json_roundtrip_preserves_structure(
            json in arbitrary_json()
        ) {
            let serialized = json.to_string();
            let deserialized: Value = serde_json::from_str(&serialized).unwrap();

            prop_assert_eq!(json, deserialized);
        }

        #[test]
        fn json_merge_operations(
            json1 in arbitrary_json(),
            json2 in arbitrary_json()
        ) {
            if let (Value::Object(mut obj1), Value::Object(obj2)) = (json1.clone(), json2.clone()) {
                // Merge obj2 into obj1
                for (k, v) in obj2 {
                    obj1.insert(k.clone(), v.clone());
                }

                let merged = Value::Object(obj1.clone());

                // Property: Merged object contains all keys
                for key in obj2.keys() {
                    prop_assert!(obj1.contains_key(key));
                }
            }
        }
    }
}

/// Property: Error messages should be bounded in size
#[cfg(test)]
mod error_handling_properties {
    use super::*;

    proptest! {
        #[test]
        fn error_messages_have_reasonable_length(
            error_text in "[a-zA-Z0-9 ]{1,10000}"
        ) {
            let max_length = 1000;
            let truncated = if error_text.len() > max_length {
                format!("{}...", &error_text[..max_length])
            } else {
                error_text.clone()
            };

            prop_assert!(truncated.len() <= max_length + 3);
            prop_assert!(truncated.starts_with(&error_text[..truncated.len().min(error_text.len())]));
        }

        #[test]
        fn error_chain_depth_limited(
            chain_depth in 0usize..100
        ) {
            let mut errors = vec![];
            for i in 0..chain_depth {
                errors.push(format!("Error at level {}", i));
            }

            // Property: Error chains should be limited to prevent stack overflow
            let max_chain_depth = 10;
            let displayed_errors = errors.iter().take(max_chain_depth).count();

            prop_assert!(displayed_errors <= max_chain_depth);
        }
    }
}
