//! Standard library tools for OVSM

pub mod advanced_math;
pub mod arrays;
pub mod bit_operations;
pub mod characters;
pub mod clos_advanced;
pub mod clos_basic;
pub mod compiler_eval;
pub mod conditions;
pub mod control_flow_extended;
pub mod data_processing;
pub mod documentation;
pub mod environment;
pub mod format;
pub mod hash_tables;
pub mod introspection;
pub mod io_basic;
pub mod io_extended;
pub mod lists_advanced;
pub mod loop_advanced;
pub mod loop_full;
pub mod loop_utilities;
pub mod math;
pub mod method_combinations;
pub mod multiple_values;
pub mod numeric;
pub mod objects;
pub mod packages;
pub mod parsing;
pub mod pathnames;
pub mod printer_control;
pub mod random_extended;
pub mod reader_control;
pub mod reader_printer;
pub mod sequences;
pub mod sequences_advanced;
pub mod statistics;
pub mod streams;
pub mod strings;
pub mod symbols_extended;
pub mod system;
pub mod time_date;
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

    // Phase 9 modules (90% coverage)
    multiple_values::register(registry);
    control_flow_extended::register(registry);
    symbols_extended::register(registry);
    method_combinations::register(registry);
    environment::register(registry);

    // Phase 10 modules (100% coverage - COMPLETE!)
    loop_advanced::register(registry);
    printer_control::register(registry);
    reader_control::register(registry);
    time_date::register(registry);
    sequences_advanced::register(registry);
    random_extended::register(registry);
    bit_operations::register(registry);
    documentation::register(registry);
    introspection::register(registry);
}
