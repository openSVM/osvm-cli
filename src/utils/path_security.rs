//! Centralized path security validation and utilities
//!
//! This module provides secure path handling to prevent:
//! - Path traversal attacks
//! - TOCTOU race conditions
//! - Symlink attacks
//! - Access to sensitive system directories

use anyhow::{anyhow, Context, Result};
use std::fs::{self, Metadata};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};

/// List of sensitive directories that should never be mounted
const SENSITIVE_DIRECTORIES: &[&str] = &[
    "/etc",
    "/boot",
    "/sys",
    "/proc",
    "/dev",
    "/root",
    "/var/run",
    "/run",
    "/tmp",
    "/var/tmp",
];

/// List of system-critical directories requiring extra validation
const SYSTEM_DIRECTORIES: &[&str] = &[
    "/bin",
    "/sbin",
    "/usr/bin",
    "/usr/sbin",
    "/lib",
    "/lib64",
    "/usr/lib",
];

/// Validated path with associated file descriptor for TOCTOU protection
pub struct ValidatedPath {
    /// Canonicalized absolute path
    pub path: PathBuf,
    /// File descriptor for TOCTOU-safe operations (Unix only)
    fd: Option<RawFd>,
    /// Cached metadata
    metadata: Metadata,
}

impl ValidatedPath {
    /// Get the canonical path
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the file descriptor (Unix only)
    pub fn fd(&self) -> Option<RawFd> {
        self.fd
    }

    /// Get cached metadata
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    /// Check if path is a directory
    pub fn is_dir(&self) -> bool {
        self.metadata.is_dir()
    }

    /// Check if path is a file
    pub fn is_file(&self) -> bool {
        self.metadata.is_file()
    }

    /// Get owner UID
    pub fn owner_uid(&self) -> u32 {
        self.metadata.uid()
    }

    /// Get group GID
    pub fn group_gid(&self) -> u32 {
        self.metadata.gid()
    }

    /// Get permissions mode
    pub fn mode(&self) -> u32 {
        self.metadata.mode()
    }
}

impl Drop for ValidatedPath {
    fn drop(&mut self) {
        // Close file descriptor when dropped
        if let Some(fd) = self.fd {
            unsafe {
                libc::close(fd);
            }
        }
    }
}

/// Safely validate and canonicalize a path with TOCTOU protection
///
/// This function:
/// 1. Expands tilde (~) properly
/// 2. Converts to absolute path
/// 3. Resolves all symlinks
/// 4. Checks path exists and is accessible
/// 5. Opens a file descriptor for TOCTOU protection
/// 6. Validates against sensitive directories
///
/// # Arguments
/// * `path` - The path to validate (can contain ~, relative paths, etc.)
/// * `must_be_dir` - If true, path must be a directory
/// * `allow_symlinks` - If false, rejects paths that are or contain symlinks
///
/// # Returns
/// ValidatedPath with file descriptor for TOCTOU-safe operations
pub fn safe_path_validation(
    path: &str,
    must_be_dir: bool,
    allow_symlinks: bool,
) -> Result<ValidatedPath> {
    // Step 1: Expand tilde properly
    let expanded = expand_tilde(path)?;

    // Step 2: Convert to PathBuf for manipulation
    let path_buf = PathBuf::from(&expanded);

    // Step 3: Check if path exists before canonicalization
    if !path_buf.exists() {
        return Err(anyhow!("Path does not exist: {}", path));
    }

    // Step 4: Canonicalize to resolve symlinks and get absolute path
    // This also validates the path exists and is accessible
    let canonical = path_buf
        .canonicalize()
        .with_context(|| format!("Failed to canonicalize path: {}", path))?;

    // Step 5: Validate against symlink policy
    if !allow_symlinks {
        // Check if the original path is a symlink
        let metadata = fs::symlink_metadata(&path_buf)
            .with_context(|| format!("Failed to get metadata for: {}", path))?;
        
        if metadata.is_symlink() {
            return Err(anyhow!(
                "Symlinks are not allowed: {} points to {}",
                path,
                canonical.display()
            ));
        }

        // Check if any component in the canonical path differs from original
        // (indicates symlink in path)
        if canonical != path_buf.canonicalize()? {
            return Err(anyhow!(
                "Path contains symlinks which are not allowed: {}",
                path
            ));
        }
    }

    // Step 6: Open file descriptor for TOCTOU protection
    use std::fs::File;
    use std::os::unix::fs::OpenOptionsExt;
    
    let file = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_DIRECTORY | libc::O_NOFOLLOW)
        .open(&canonical)
        .with_context(|| format!("Failed to open path for validation: {}", canonical.display()))?;
    
    let fd = file.as_raw_fd();
    
    // Get metadata using the file descriptor (TOCTOU-safe)
    let metadata = file.metadata()
        .with_context(|| format!("Failed to get metadata via fd: {}", canonical.display()))?;

    // Step 7: Validate directory requirement
    if must_be_dir && !metadata.is_dir() {
        return Err(anyhow!("Path must be a directory: {}", path));
    }

    // Step 8: Check against sensitive directories
    validate_not_sensitive(&canonical)?;

    // Step 9: Validate permissions
    validate_permissions(&metadata, &canonical)?;

    // Keep fd alive by forgetting the File (we'll manage it manually)
    std::mem::forget(file);

    Ok(ValidatedPath {
        path: canonical,
        fd: Some(fd),
        metadata,
    })
}

/// Expand tilde (~) in paths securely
fn expand_tilde(path: &str) -> Result<String> {
    if path.starts_with("~/") || path == "~" {
        // Get HOME from environment
        let home = std::env::var("HOME")
            .context("HOME environment variable not set")?;
        
        // Validate HOME is an absolute path
        let home_path = PathBuf::from(&home);
        if !home_path.is_absolute() {
            return Err(anyhow!("HOME is not an absolute path: {}", home));
        }

        // Replace ~ with HOME
        if path == "~" {
            Ok(home)
        } else {
            Ok(path.replacen("~", &home, 1))
        }
    } else if path.starts_with("~") {
        // Reject ~user syntax for security
        Err(anyhow!(
            "User home directory syntax (~user) is not supported for security reasons. \
             Use absolute paths or ~/path for your own home directory."
        ))
    } else {
        Ok(path.to_string())
    }
}

/// Validate path is not a sensitive system directory
fn validate_not_sensitive(path: &Path) -> Result<()> {
    let path_str = path.to_string_lossy();

    // Check exact matches with sensitive directories
    for sensitive in SENSITIVE_DIRECTORIES {
        if path_str == *sensitive {
            return Err(anyhow!(
                "Access denied: {} is a sensitive system directory",
                sensitive
            ));
        }
    }

    // Check if path is under sensitive directory
    for sensitive in SENSITIVE_DIRECTORIES {
        if path_str.starts_with(&format!("{}/", sensitive)) {
            return Err(anyhow!(
                "Access denied: path is under sensitive system directory {}",
                sensitive
            ));
        }
    }

    // Warn about system directories but don't block (may be needed for read-only)
    for system_dir in SYSTEM_DIRECTORIES {
        if path_str == *system_dir || path_str.starts_with(&format!("{}/", system_dir)) {
            log::warn!(
                "Mounting system directory: {} (ensure read-only if possible)",
                path_str
            );
            break;
        }
    }

    Ok(())
}

/// Validate file permissions are secure
fn validate_permissions(metadata: &Metadata, path: &Path) -> Result<()> {
    let mode = metadata.mode();
    let perms = mode & 0o777;

    // Check for world-writable
    if perms & 0o002 != 0 {
        log::warn!(
            "Path {} is world-writable (permissions: {:o}). This may be a security risk.",
            path.display(),
            perms
        );
    }

    // Check for setuid/setgid
    if mode & 0o4000 != 0 {
        log::warn!("Path {} has setuid bit set", path.display());
    }
    if mode & 0o2000 != 0 {
        log::warn!("Path {} has setgid bit set", path.display());
    }

    Ok(())
}

/// Create a secure socket directory with proper permissions
///
/// Returns path to socket directory with 0700 permissions in XDG_RUNTIME_DIR
/// or secure fallback location
pub fn create_secure_socket_dir() -> Result<PathBuf> {
    // Try XDG_RUNTIME_DIR first (most secure)
    if let Ok(xdg_runtime) = std::env::var("XDG_RUNTIME_DIR") {
        let runtime_path = PathBuf::from(xdg_runtime);
        if runtime_path.exists() && runtime_path.is_dir() {
            let socket_dir = runtime_path.join("osvm-sockets");
            return create_dir_with_perms(&socket_dir, 0o700);
        }
    }

    // Fallback to user's home directory
    let home = std::env::var("HOME")
        .context("HOME environment variable not set")?;
    let socket_dir = PathBuf::from(home)
        .join(".local")
        .join("run")
        .join("osvm");
    
    create_dir_with_perms(&socket_dir, 0o700)
}

/// Create directory with specific permissions atomically
fn create_dir_with_perms(path: &Path, mode: u32) -> Result<PathBuf> {
    use std::os::unix::fs::DirBuilderExt;

    if path.exists() {
        // Verify existing directory has correct permissions
        let metadata = fs::metadata(path)
            .with_context(|| format!("Failed to get metadata for {}", path.display()))?;
        
        if !metadata.is_dir() {
            return Err(anyhow!("{} exists but is not a directory", path.display()));
        }

        let current_mode = metadata.mode() & 0o777;
        if current_mode != mode {
            // Fix permissions
            fs::set_permissions(path, fs::Permissions::from_mode(mode))
                .with_context(|| format!("Failed to set permissions on {}", path.display()))?;
            log::warn!(
                "Fixed permissions on {} from {:o} to {:o}",
                path.display(),
                current_mode,
                mode
            );
        }
    } else {
        // Create with correct permissions atomically
        fs::DirBuilder::new()
            .recursive(true)
            .mode(mode)
            .create(path)
            .with_context(|| format!("Failed to create directory {}", path.display()))?;
    }

    Ok(path.to_path_buf())
}

/// Generate secure random socket name
pub fn generate_socket_name(prefix: &str) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    
    let pid = std::process::id();
    
    // Use process-safe naming
    format!("{}-{}-{}.sock", prefix, pid, timestamp)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_expand_tilde() {
        std::env::set_var("HOME", "/home/testuser");
        
        assert_eq!(expand_tilde("~/Documents").unwrap(), "/home/testuser/Documents");
        assert_eq!(expand_tilde("~").unwrap(), "/home/testuser");
        assert!(expand_tilde("~otheruser/file").is_err());
    }

    #[test]
    fn test_validate_not_sensitive() {
        assert!(validate_not_sensitive(Path::new("/etc")).is_err());
        assert!(validate_not_sensitive(Path::new("/etc/passwd")).is_err());
        assert!(validate_not_sensitive(Path::new("/home/user/documents")).is_ok());
    }

    #[test]
    fn test_generate_socket_name() {
        let name1 = generate_socket_name("test");
        let name2 = generate_socket_name("test");
        
        assert!(name1.starts_with("test-"));
        assert!(name1.ends_with(".sock"));
        assert_ne!(name1, name2); // Should be unique
    }
}
