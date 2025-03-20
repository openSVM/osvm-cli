//! Compatibility layer for Clap API changes
//! This module provides functions to bridge the gap between different versions of Clap

use clap::ArgMatches;

/// Get a string value from ArgMatches (compatibility with older Clap API)
pub fn value_of<'a>(matches: &'a ArgMatches, name: &str) -> Option<&'a str> {
    matches.get_one::<String>(name).map(|s| s.as_str())
}

/// Check if an argument is present (compatibility with older Clap API)
pub fn is_present(matches: &ArgMatches, name: &str) -> bool {
    matches.contains_id(name)
}

/// Get the number of occurrences of a flag (compatibility with older Clap API)
pub fn occurrences_of(matches: &ArgMatches, name: &str) -> u8 {
    matches.get_count(name) as u8
}

/// Get a value from ArgMatches and unwrap it (compatibility with older Clap API)
pub fn value_of_unwrap<'a>(matches: &'a ArgMatches, name: &str) -> &'a str {
    value_of(matches, name).unwrap()
}

/// Get a value from ArgMatches with a default (compatibility with older Clap API)
pub fn value_of_or<'a, 'b>(matches: &'a ArgMatches, name: &str, default: &'b str) -> &'a str
where
    'b: 'a,
{
    value_of(matches, name).unwrap_or(default)
}
