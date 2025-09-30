#![allow(clippy::all)]
#![allow(unused)]

//! Utility modules for the OSVM CLI
//!
//! This directory contains various utility modules that provide the core functionality
//! for the OSVM CLI, including SVM and node management, SSH deployment, and UI components.
//! Also includes utility wrappers for external dependencies.

use serde::de::DeserializeOwned;
use std::{fs::File, io, path::Path};

// UI and display utilities
/// Agent chat interface using cursive-multiplex with MCP tools
pub mod agent_chat;
#[cfg(test)]
pub mod agent_chat_tests;
pub mod agent_chat_v2;
/// CLI-based agent execution without UI
pub mod agent_cli;
/// Color formatting utilities for terminal output
pub mod color;
/// Dashboard utilities for interactive SVM monitoring
pub mod dashboard;
/// Example command utilities for displaying usage examples
pub mod examples;
/// Custom keybinding system for agent interfaces
pub mod keybindings;
/// Node dashboard utilities for interactive node monitoring
pub mod nodes_dashboard;
/// Secure logging utilities to prevent information disclosure
pub mod secure_logger;
/// Network security utilities for safe network operations
pub mod network_security;
/// Cryptographic security utilities for key validation and secure storage
pub mod crypto_security;
/// Error boundary utilities for secure error handling and recovery
pub mod error_boundary;
/// Secure system operations without privilege escalation
pub mod secure_system;
/// Real-time security monitoring and threat detection
// FIXME: Disabled due to compilation errors (async trait not dyn-compatible)
// pub mod security_monitor;
/// Secure deployment architectures for maximum isolation
// FIXME: Disabled due to compilation errors (async trait not dyn-compatible)
// pub mod secure_deployment;
/// Automated security testing framework for continuous validation
// FIXME: Disabled due to compilation errors (async trait not dyn-compatible)
// pub mod security_testing;
/// Plugin system for extending functionality
pub mod plugins;
/// Advanced theme customization system
pub mod themes;
/// Isolation runtime support for unikernels and microVMs
pub mod isolation;

// Core functionality
/// AST-based code analysis for enhanced fix suggestions
pub mod ast_analyzer;
/// Security audit system for comprehensive vulnerability analysis
pub mod audit;
/// Modular audit system with structured vulnerability checks
pub mod audit_modular;
/// Structured Rust code parser for security analysis
pub mod audit_parser;
/// Template-based report generation system
pub mod audit_templates;
/// Integration tests for audit system with AI fallback scenarios
#[cfg(test)]
pub mod audit_tests;
/// Blueprint theme system for consistent UI styling
pub mod blueprint_theme;
/// Granular circuit breaker system for AI services
pub mod circuit_breaker;
/// Code snippet extraction for DeepLogic AI Analysis
pub mod code_extractor;
/// Debug logging utilities with configurable verbosity levels
pub mod debug_logger;
/// Simple devnet proxy for development
pub mod devnet_proxy;
/// Devnet RPC node that syncs with real blockchain
pub mod devnet_rpc;
/// eBPF deployment utilities for program deployment
pub mod ebpf_deploy;
/// Enhanced Git repository management with dynamic branch detection
pub mod git_manager;
/// Local RPC node deployment and management
pub mod local_rpc;
/// Mainnet RPC connectivity
pub mod mainnet_rpc;
/// Markdown rendering for terminal output
pub mod markdown_renderer;
/// Node management utilities for monitoring and controlling nodes
pub mod nodes;
/// Configurable AI prompt templates system
pub mod prompt_templates;
/// Simple devnet proxy implementation
pub mod simple_devnet_proxy;
/// Solana RPC connectivity and monitoring
pub mod solana_rpc;
/// SSH deployment utilities for remote node deployment
pub mod ssh_deploy;
/// SVM information and management utilities
pub mod svm_info;

// Self-repair and diagnostics
/// System diagnostics and health monitoring
pub mod diagnostics;
/// Continuous log monitoring and automatic repair
pub mod log_monitor;
/// OSVM internal logging system for tracking events and decisions
pub mod osvm_logger;
/// Self-repair system for automatic dependency management
pub mod self_repair;

// External dependency wrappers

/// Loads a YAML configuration file and deserializes it into the specified type
///
/// # Arguments
///
/// * `config_file` - Path to the YAML configuration file
///
/// # Returns
///
/// * `Result<T, io::Error>` - The deserialized configuration or an error
///
/// # Examples
///
/// ```
/// use osvm::utils::load_keys_config_file;
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Config {
///     key: String,
/// }
///
/// let config: Config = load_keys_config_file("config.yml").unwrap();
/// ```
pub fn load_keys_config_file<T, P>(config_file: P) -> Result<T, io::Error>
where
    T: DeserializeOwned,
    P: AsRef<Path>,
{
    let file = File::open(config_file)?;
    let config =
        serde_yaml::from_reader(file).map_err(|err| io::Error::other(format!("{:?}", err)))?;
    Ok(config)
}
