//! Error definitions for SSH deployment

use {
    std::{error::Error, fmt, io},
    ssh2,
};

/// Deployment error types
#[derive(Debug)]
pub enum DeploymentError {
    /// Connection error
    ConnectionError(String),
    /// Authentication error
    AuthError(String),
    /// Command execution error
    CommandError(String),
    /// File transfer error
    FileTransferError(String),
    /// Configuration error
    InvalidConfig(String),
    /// Configuration error
    ConfigError(String),
    /// Validation error
    ValidationError(String),
    /// SSH error
    SshError(ssh2::Error),
    /// IO error
    IoError(io::Error),
    /// Deployment error
    DeploymentError(String),
    /// Other error
    Other(String),
}

impl fmt::Display for DeploymentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeploymentError::ConnectionError(msg) => write!(f, "Connection error: {}", msg),
            DeploymentError::AuthError(msg) => write!(f, "Authentication error: {}", msg),
            DeploymentError::CommandError(msg) => write!(f, "Command execution error: {}", msg),
            DeploymentError::FileTransferError(msg) => write!(f, "File transfer error: {}", msg),
            DeploymentError::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
            DeploymentError::ConfigError(msg) => write!(f, "Configuration error: {}", msg),
            DeploymentError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            DeploymentError::SshError(e) => write!(f, "SSH error: {}", e),
            DeploymentError::IoError(e) => write!(f, "IO error: {}", e),
            DeploymentError::DeploymentError(msg) => write!(f, "Deployment error: {}", msg),
            DeploymentError::Other(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl Error for DeploymentError {}

impl From<ssh2::Error> for DeploymentError {
    fn from(e: ssh2::Error) -> Self {
        DeploymentError::SshError(e)
    }
}

impl From<io::Error> for DeploymentError {
    fn from(e: io::Error) -> Self {
        DeploymentError::IoError(e)
    }
}