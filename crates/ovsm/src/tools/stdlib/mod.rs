//! Standard library tools for OVSM

pub mod advanced_math;
pub mod arrays;
pub mod characters;
pub mod clos_basic;
pub mod conditions;
pub mod data_processing;
pub mod format;
pub mod hash_tables;
pub mod io_basic;
pub mod lists_advanced;
pub mod loop_utilities;
pub mod math;
pub mod numeric;
pub mod objects;
pub mod packages;
pub mod parsing;
pub mod pathnames;
pub mod sequences;
pub mod statistics;
pub mod streams;
pub mod strings;
pub mod type_predicates;
pub mod utilities;

use crate::tools::ToolRegistry;

/// Register all standard library tools
pub fn register_all(registry: &mut ToolRegistry) {
    // Core modules
    data_processing::register(registry);
    statistics::register(registry);
    math::register(registry);
    utilities::register(registry);
    objects::register(registry);
    parsing::register(registry);

    // Common Lisp compatibility modules
    type_predicates::register(registry);
    strings::register(registry);
    sequences::register(registry);
    advanced_math::register(registry);
    arrays::register(registry);
    numeric::register(registry);
    characters::register(registry);
    lists_advanced::register(registry);
    hash_tables::register(registry);
    io_basic::register(registry);
    pathnames::register(registry);
    format::register(registry);
    streams::register(registry);
    loop_utilities::register(registry);
    conditions::register(registry);
    clos_basic::register(registry);
    packages::register(registry);
}
