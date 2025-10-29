//! Standard library tools for OVSM

pub mod advanced_math;
pub mod arrays;
pub mod characters;
pub mod clos_advanced;
pub mod clos_basic;
pub mod compiler_eval;
pub mod conditions;
pub mod data_processing;
pub mod format;
pub mod hash_tables;
pub mod io_basic;
pub mod io_extended;
pub mod lists_advanced;
pub mod loop_full;
pub mod loop_utilities;
pub mod math;
pub mod numeric;
pub mod objects;
pub mod packages;
pub mod parsing;
pub mod pathnames;
pub mod reader_printer;
pub mod sequences;
pub mod statistics;
pub mod streams;
pub mod strings;
pub mod system;
pub mod type_predicates;
pub mod types_extended;
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
    io_extended::register(registry);
    pathnames::register(registry);
    format::register(registry);
    streams::register(registry);
    loop_utilities::register(registry);
    loop_full::register(registry);
    conditions::register(registry);
    clos_basic::register(registry);
    clos_advanced::register(registry);
    packages::register(registry);
    reader_printer::register(registry);
    compiler_eval::register(registry);
    system::register(registry);
    types_extended::register(registry);
}
