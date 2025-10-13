//! Centralized error type for CLI operations
//!
//! This module defines a unified error type for all CLI command operations,
//! providing consistent error messages and proper error propagation.

use thiserror::Error;

/// CLI-specific error type for command operations
///
/// This error type consolidates all CLI-related errors into a single enum,
/// making error handling consistent across all command modules.
#[derive(Error, Debug)]
pub enum CliError {
    /// A required command-line argument was not provided
    #[error("Missing required argument: {arg_name}")]
    MissingArgument {
        /// Name of the missing argument
        arg_name: String,
    },

    /// A command-line argument has an invalid value
    #[error("Invalid value '{value}' for argument '{arg_name}': {reason}")]
    InvalidArgument {
        /// Name of the argument
        arg_name: String,
        /// The invalid value provided
        value: String,
        /// Explanation of why the value is invalid
        reason: String,
    },

    /// A required subcommand was not provided
    #[error("Missing required subcommand for '{command}'")]
    MissingSubcommand {
        /// Name of the command that requires a subcommand
        command: String,
    },

    /// An unknown subcommand was provided
    #[error("Unknown subcommand '{subcommand}' for command '{command}'")]
    UnknownSubcommand {
        /// Name of the parent command
        command: String,
        /// Name of the unknown subcommand
        subcommand: String,
    },

    /// Configuration error
    #[error("Configuration error: {message}")]
    ConfigError {
        /// Error message
        message: String,
    },

    /// RPC connection error
    #[error("RPC connection error: {message}")]
    RpcError {
        /// Error message
        message: String,
    },

    /// File system error
    #[error("File system error: {message}")]
    FileSystemError {
        /// Error message
        message: String,
    },

    /// Generic command execution error
    #[error("Command execution failed: {message}")]
    ExecutionError {
        /// Error message
        message: String,
    },

    /// Network operation error
    #[error("Network operation failed: {message}")]
    NetworkError {
        /// Error message
        message: String,
    },

    /// Validation error
    #[error("Validation error: {message}")]
    ValidationError {
        /// Error message
        message: String,
    },

    /// I/O error wrapper
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Anyhow error wrapper for compatibility
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl CliError {
    /// Create a ConfigError from a message
    pub fn config<S: Into<String>>(message: S) -> Self {
        CliError::ConfigError {
            message: message.into(),
        }
    }

    /// Create an RpcError from a message
    pub fn rpc<S: Into<String>>(message: S) -> Self {
        CliError::RpcError {
            message: message.into(),
        }
    }

    /// Create an ExecutionError from a message
    pub fn execution<S: Into<String>>(message: S) -> Self {
        CliError::ExecutionError {
            message: message.into(),
        }
    }

    /// Create a NetworkError from a message
    pub fn network<S: Into<String>>(message: S) -> Self {
        CliError::NetworkError {
            message: message.into(),
        }
    }

    /// Create a ValidationError from a message
    pub fn validation<S: Into<String>>(message: S) -> Self {
        CliError::ValidationError {
            message: message.into(),
        }
    }

    /// Create a FileSystemError from a message
    pub fn filesystem<S: Into<String>>(message: S) -> Self {
        CliError::FileSystemError {
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_missing_argument_error() {
        let error = CliError::MissingArgument {
            arg_name: "address".to_string(),
        };
        let msg = format!("{}", error);
        assert!(msg.contains("Missing required argument: address"));
    }

    #[test]
    fn test_invalid_argument_error() {
        let error = CliError::InvalidArgument {
            arg_name: "port".to_string(),
            value: "invalid".to_string(),
            reason: "Not a valid number".to_string(),
        };
        let msg = format!("{}", error);
        assert!(msg.contains("Invalid value 'invalid' for argument 'port'"));
        assert!(msg.contains("Not a valid number"));
    }

    #[test]
    fn test_config_error_constructor() {
        let error = CliError::config("Failed to load config");
        let msg = format!("{}", error);
        assert!(msg.contains("Configuration error: Failed to load config"));
    }

    #[test]
    fn test_rpc_error_constructor() {
        let error = CliError::rpc("Connection timeout");
        let msg = format!("{}", error);
        assert!(msg.contains("RPC connection error: Connection timeout"));
    }

    #[test]
    fn test_execution_error_constructor() {
        let error = CliError::execution("Command failed");
        let msg = format!("{}", error);
        assert!(msg.contains("Command execution failed: Command failed"));
    }
}
