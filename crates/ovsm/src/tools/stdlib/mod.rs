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
pub mod llm;
pub mod loop_advanced;
pub mod loop_full;
pub mod loop_utilities;
pub mod math;
pub mod method_combinations;
pub mod multiple_values;
pub mod network;
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
    // IMPORTANT: MCP tools should ONLY be for external integrations!
    // Basic language features (math, strings, control flow, etc.)
    // should be built-in functions in the evaluator, NOT MCP tools.
    //
    // MCP tools have network overhead and are meant for:
    // - Blockchain API calls (get_account_transactions, fetch_token_metadata)
    // - External system integration (file I/O, network requests)
    // - Database queries
    //
    // They should NOT be used for:
    // - Basic math operations (add, multiply, sqrt)
    // - String manipulation (concat, substring, uppercase)
    // - Control flow (if, when, case, cond)
    // - Data structures (lists, arrays, hash tables)
    //
    // ALL of these Common Lisp stdlib implementations should be
    // moved to the lisp_evaluator as native built-in functions.

    // DISABLED - All of these should be language builtins:
    // data_processing::register(registry);  // Already disabled - COUNT, APPEND, etc
    // statistics::register(registry);       // mean, median, stddev - should be builtins
    // math::register(registry);             // sin, cos, sqrt - should be builtins
    // utilities::register(registry);        // Misc utilities - should be builtins
    // objects::register(registry);          // Object system - should be built-in
    // parsing::register(registry);          // parse-integer etc - should be builtins

    // type_predicates::register(registry);  // integerp, stringp - should be builtins
    // strings::register(registry);          // string ops - should be builtins
    // sequences::register(registry);        // sequence ops - should be builtins
    // advanced_math::register(registry);    // math functions - should be builtins
    // arrays::register(registry);           // array ops - should be builtins
    // numeric::register(registry);          // numeric ops - should be builtins
    // characters::register(registry);       // char ops - should be builtins
    // lists_advanced::register(registry);   // list ops - should be builtins
    // hash_tables::register(registry);      // hash ops - should be builtins
    // format::register(registry);           // formatting - should be builtin
    // loop_utilities::register(registry);   // loop helpers - should be builtins
    // loop_full::register(registry);        // loop macro - should be builtin
    // conditions::register(registry);       // error handling - should be builtin
    // clos_basic::register(registry);       // object system - should be builtin
    // clos_advanced::register(registry);    // object system - should be builtin
    // packages::register(registry);         // namespaces - should be builtin
    // compiler_eval::register(registry);    // eval/compile - should be builtin
    // types_extended::register(registry);   // type system - should be builtin
    // multiple_values::register(registry);  // multiple returns - should be builtin
    // control_flow_extended::register(registry); // control flow - should be builtin
    // symbols_extended::register(registry); // symbol ops - should be builtin
    // method_combinations::register(registry); // CLOS - should be builtin
    // environment::register(registry);      // env vars - might keep as tool
    // loop_advanced::register(registry);    // loop macro - should be builtin
    // printer_control::register(registry);  // printing - should be builtin
    // reader_control::register(registry);   // reading - should be builtin
    // time_date::register(registry);        // time/date - might keep as tool
    // sequences_advanced::register(registry); // sequence ops - should be builtin
    // random_extended::register(registry);  // random numbers - should be builtin
    // bit_operations::register(registry);   // bit ops - should be builtin
    // documentation::register(registry);    // docs - should be builtin
    // introspection::register(registry);    // reflection - should be builtin

    // These MIGHT be legitimate MCP tools for external I/O:
    // (But even these should probably be native with proper sandboxing)
    // io_basic::register(registry);         // File I/O - maybe keep as tool
    // io_extended::register(registry);      // Advanced I/O - maybe keep as tool
    // pathnames::register(registry);        // File paths - maybe keep as tool
    // streams::register(registry);          // I/O streams - maybe keep as tool
    // reader_printer::register(registry);   // I/O formatting - maybe keep as tool
    // system::register(registry);           // System calls - maybe keep as tool

    // For now, disable EVERYTHING to prevent conflicts with builtins.
    // Real MCP tools should have descriptive names like:
    // - get_account_transactions
    // - fetch_token_metadata
    // - query_database
    // NOT single words like COUNT, APPEND, SORT, etc.

    let _ = registry; // Suppress unused variable warning
}
