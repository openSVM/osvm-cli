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
/// Color formatting utilities for terminal output
pub mod color;
/// Dashboard utilities for interactive SVM monitoring
pub mod dashboard;
/// Example command utilities for displaying usage examples
pub mod examples;
/// Node dashboard utilities for interactive node monitoring
pub mod nodes_dashboard;

// Core functionality
/// Security audit system for comprehensive vulnerability analysis
pub mod audit;
/// Modular audit system with structured vulnerability checks
pub mod audit_modular;
/// Structured Rust code parser for security analysis
pub mod audit_parser;
/// Template-based report generation system
pub mod audit_templates;
/// Devnet RPC node that syncs with real blockchain
pub mod devnet_rpc;
/// eBPF deployment utilities for program deployment
pub mod ebpf_deploy;
/// Local RPC node deployment and management
pub mod local_rpc;
/// Node management utilities for monitoring and controlling nodes
pub mod nodes;
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
