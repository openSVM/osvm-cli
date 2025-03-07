//! Module exports for utility modules

use std::{fs::File, io, path::Path};

pub mod color;
pub mod examples;
pub mod svm_info;
pub mod ssh_deploy;
pub mod nodes;
pub mod dashboard;
pub mod nodes_dashboard;

/// Loads a yaml file
pub fn load_keys_config_file<T, P>(config_file: P) -> Result<T, io::Error>
where
    T: serde::de::DeserializeOwned,
    P: AsRef<Path>,
{
    let file = File::open(config_file)?;
    let config = serde_yaml::from_reader(file)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{:?}", err)))?;
    Ok(config)
}