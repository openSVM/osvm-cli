//! OSVM CLI Library
//! 
//! This library provides the core functionality for the OSVM CLI.
//! It includes utilities for managing SVMs, nodes, and SSH deployments.

pub mod utils;

/// Exports key capabilities for convenient use throughout the codebase
pub mod prelude {
    pub use crate::utils::{
        svm_info,
        ssh_deploy,
        nodes,
        dashboard,
        examples,
        color,
    };
}