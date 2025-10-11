//! Standard library tools for OVSM

pub mod data_processing;
pub mod math;
pub mod statistics;
pub mod utilities;

use crate::tools::ToolRegistry;

/// Register all standard library tools
pub fn register_all(registry: &mut ToolRegistry) {
    data_processing::register(registry);
    statistics::register(registry);
    math::register(registry);
    utilities::register(registry);
}
