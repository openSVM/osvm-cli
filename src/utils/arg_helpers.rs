//! Safe argument extraction helpers for CLI argument parsing
//!
//! This module provides safe, reusable functions to extract typed arguments
//! from clap's `ArgMatches` without panicking. All functions return
//! `Result<T, CliError>` for proper error handling.

use crate::utils::cli_error::CliError;
use clap::ArgMatches;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

/// Safely extract a required string argument
///
/// Returns an error if the argument is missing or cannot be extracted.
///
/// # Example
/// ```ignore
/// let address = get_required_str(matches, "address")?;
/// ```
pub fn get_required_str<'a>(
    matches: &'a ArgMatches,
    name: &str,
) -> Result<&'a str, CliError> {
    matches
        .get_one::<String>(name)
        .map(|s| s.as_str())
        .ok_or_else(|| CliError::MissingArgument {
            arg_name: name.to_string(),
        })
}

/// Safely extract an optional string argument
///
/// Returns `None` if the argument is not provided.
///
/// # Example
/// ```ignore
/// let optional_value = get_optional_str(matches, "optional_field");
/// ```
pub fn get_optional_str<'a>(
    matches: &'a ArgMatches,
    name: &str,
) -> Option<&'a str> {
    matches.get_one::<String>(name).map(|s| s.as_str())
}

/// Safely extract a required string argument with a default value
///
/// Returns the default value if the argument is not provided.
///
/// # Example
/// ```ignore
/// let network = get_str_with_default(matches, "network", "mainnet");
/// ```
pub fn get_str_with_default<'a>(
    matches: &'a ArgMatches,
    name: &str,
    default: &'a str,
) -> &'a str {
    matches
        .get_one::<String>(name)
        .map(|s| s.as_str())
        .unwrap_or(default)
}

/// Safely extract an optional u64 argument
///
/// Returns an error if the argument is present but cannot be parsed as u64.
///
/// # Example
/// ```ignore
/// let optional_count = get_optional_u64(matches, "count")?;
/// ```
pub fn get_optional_u64(
    matches: &ArgMatches,
    name: &str,
) -> Result<Option<u64>, CliError> {
    match matches.get_one::<String>(name) {
        Some(s) => {
            let value = s.parse::<u64>().map_err(|e| CliError::InvalidArgument {
                arg_name: name.to_string(),
                value: s.clone(),
                reason: format!("Not a valid u64: {}", e),
            })?;
            Ok(Some(value))
        }
        None => Ok(None),
    }
}

/// Safely extract a required u64 argument
///
/// Returns an error if the argument is missing or cannot be parsed as u64.
///
/// # Example
/// ```ignore
/// let port = get_required_u64(matches, "port")?;
/// ```
pub fn get_required_u64(matches: &ArgMatches, name: &str) -> Result<u64, CliError> {
    let s = get_required_str(matches, name)?;
    s.parse::<u64>().map_err(|e| CliError::InvalidArgument {
        arg_name: name.to_string(),
        value: s.to_string(),
        reason: format!("Not a valid u64: {}", e),
    })
}

/// Safely extract a u64 argument with a default value
///
/// Returns the default value if the argument is not provided.
/// Returns an error if the argument is present but cannot be parsed as u64.
///
/// # Example
/// ```ignore
/// let port = get_u64_with_default(matches, "port", 8899)?;
/// ```
pub fn get_u64_with_default(
    matches: &ArgMatches,
    name: &str,
    default: u64,
) -> Result<u64, CliError> {
    match matches.get_one::<String>(name) {
        Some(s) => s.parse::<u64>().map_err(|e| CliError::InvalidArgument {
            arg_name: name.to_string(),
            value: s.clone(),
            reason: format!("Not a valid u64: {}", e),
        }),
        None => Ok(default),
    }
}

/// Safely extract an optional usize argument
///
/// Returns an error if the argument is present but cannot be parsed as usize.
///
/// # Example
/// ```ignore
/// let optional_size = get_optional_usize(matches, "size")?;
/// ```
pub fn get_optional_usize(
    matches: &ArgMatches,
    name: &str,
) -> Result<Option<usize>, CliError> {
    match matches.get_one::<String>(name) {
        Some(s) => {
            let value = s.parse::<usize>().map_err(|e| CliError::InvalidArgument {
                arg_name: name.to_string(),
                value: s.clone(),
                reason: format!("Not a valid usize: {}", e),
            })?;
            Ok(Some(value))
        }
        None => Ok(None),
    }
}

/// Safely extract a usize argument with a default value
///
/// Returns the default value if the argument is not provided.
/// Returns an error if the argument is present but cannot be parsed as usize.
///
/// # Example
/// ```ignore
/// let lines = get_usize_with_default(matches, "lines", 100)?;
/// ```
pub fn get_usize_with_default(
    matches: &ArgMatches,
    name: &str,
    default: usize,
) -> Result<usize, CliError> {
    match matches.get_one::<String>(name) {
        Some(s) => s.parse::<usize>().map_err(|e| CliError::InvalidArgument {
            arg_name: name.to_string(),
            value: s.clone(),
            reason: format!("Not a valid usize: {}", e),
        }),
        None => Ok(default),
    }
}

/// Safely extract a boolean flag
///
/// Returns `true` if the flag is present, `false` otherwise.
///
/// # Example
/// ```ignore
/// let verbose = get_flag(matches, "verbose");
/// ```
pub fn get_flag(matches: &ArgMatches, name: &str) -> bool {
    matches.get_flag(name)
}

/// Safely extract an optional Pubkey argument
///
/// Returns an error if the argument is present but cannot be parsed as a valid Pubkey.
///
/// # Example
/// ```ignore
/// let optional_address = get_optional_pubkey(matches, "address")?;
/// ```
pub fn get_optional_pubkey(
    matches: &ArgMatches,
    name: &str,
) -> Result<Option<Pubkey>, CliError> {
    match matches.get_one::<String>(name) {
        Some(s) => {
            let pubkey = Pubkey::from_str(s).map_err(|e| CliError::InvalidArgument {
                arg_name: name.to_string(),
                value: s.clone(),
                reason: format!("Not a valid Pubkey: {}", e),
            })?;
            Ok(Some(pubkey))
        }
        None => Ok(None),
    }
}

/// Safely extract a required Pubkey argument
///
/// Returns an error if the argument is missing or cannot be parsed as a valid Pubkey.
///
/// # Example
/// ```ignore
/// let address = get_required_pubkey(matches, "address")?;
/// ```
pub fn get_required_pubkey(matches: &ArgMatches, name: &str) -> Result<Pubkey, CliError> {
    let s = get_required_str(matches, name)?;
    Pubkey::from_str(s).map_err(|e| CliError::InvalidArgument {
        arg_name: name.to_string(),
        value: s.to_string(),
        reason: format!("Not a valid Pubkey: {}", e),
    })
}

/// Safely extract a count argument (used for -v, -vv, etc.)
///
/// Returns the number of times the flag was specified.
///
/// # Example
/// ```ignore
/// let verbosity_level = get_count(matches, "verbose");
/// ```
pub fn get_count(matches: &ArgMatches, name: &str) -> u8 {
    matches.get_count(name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{Arg, ArgAction, Command};

    fn create_test_app() -> Command {
        Command::new("test")
            .arg(
                Arg::new("required_str")
                    .long("required-str")
                    .required(false),
            )
            .arg(Arg::new("optional_str").long("optional-str"))
            .arg(Arg::new("number").long("number"))
            .arg(
                Arg::new("flag")
                    .long("flag")
                    .action(ArgAction::SetTrue),
            )
            .arg(
                Arg::new("count")
                    .long("count")
                    .action(ArgAction::Count),
            )
            .arg(Arg::new("pubkey").long("pubkey"))
    }

    #[test]
    fn test_get_required_str_present() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--required-str", "value"]);
        let result = get_required_str(&matches, "required_str");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "value");
    }

    #[test]
    fn test_get_required_str_missing() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test"]);
        let result = get_required_str(&matches, "required_str");
        assert!(result.is_err());
        match result {
            Err(CliError::MissingArgument { arg_name }) => {
                assert_eq!(arg_name, "required_str");
            }
            _ => panic!("Expected MissingArgument error"),
        }
    }

    #[test]
    fn test_get_optional_str_present() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--optional-str", "value"]);
        let result = get_optional_str(&matches, "optional_str");
        assert_eq!(result, Some("value"));
    }

    #[test]
    fn test_get_optional_str_missing() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test"]);
        let result = get_optional_str(&matches, "optional_str");
        assert_eq!(result, None);
    }

    #[test]
    fn test_get_str_with_default() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test"]);
        let result = get_str_with_default(&matches, "optional_str", "default");
        assert_eq!(result, "default");
    }

    #[test]
    fn test_get_optional_u64_valid() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--number", "42"]);
        let result = get_optional_u64(&matches, "number");
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(42));
    }

    #[test]
    fn test_get_optional_u64_invalid() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--number", "invalid"]);
        let result = get_optional_u64(&matches, "number");
        assert!(result.is_err());
    }

    #[test]
    fn test_get_flag_present() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--flag"]);
        assert!(get_flag(&matches, "flag"));
    }

    #[test]
    fn test_get_flag_missing() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test"]);
        assert!(!get_flag(&matches, "flag"));
    }

    #[test]
    fn test_get_count() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--count", "--count", "--count"]);
        assert_eq!(get_count(&matches, "count"), 3);
    }

    #[test]
    fn test_get_optional_pubkey_valid() {
        let app = create_test_app();
        let pubkey_str = "11111111111111111111111111111111";
        let matches = app.get_matches_from(vec!["test", "--pubkey", pubkey_str]);
        let result = get_optional_pubkey(&matches, "pubkey");
        assert!(result.is_ok());
        assert!(result.unwrap().is_some());
    }

    #[test]
    fn test_get_optional_pubkey_invalid() {
        let app = create_test_app();
        let matches = app.get_matches_from(vec!["test", "--pubkey", "invalid"]);
        let result = get_optional_pubkey(&matches, "pubkey");
        assert!(result.is_err());
    }
}
