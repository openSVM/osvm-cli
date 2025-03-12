//! Utility modules for the OSVM CLI
//!
//! This directory contains various utility modules that provide the core functionality
//! for the OSVM CLI, including SVM and node management, SSH deployment, and UI components.

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
/// Node management utilities for monitoring and controlling nodes
pub mod nodes;
/// SSH deployment utilities for remote node deployment
pub mod ssh_deploy;
/// SVM information and management utilities
pub mod svm_info;

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
    let config = serde_yaml::from_reader(file)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{:?}", err)))?;
    Ok(config)
}
