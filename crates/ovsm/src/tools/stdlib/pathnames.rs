//! Pathname operations for OVSM
//!
//! This module implements Common Lisp's pathname functions including:
//! - Pathname creation (PATHNAME, MAKE-PATHNAME, PARSE-NAMESTRING)
//! - Component extraction (PATHNAME-DIRECTORY, PATHNAME-NAME, PATHNAME-TYPE, etc.)
//! - Path utilities (MERGE-PATHNAMES, NAMESTRING, TRUENAME, etc.)
//!
//! Implementation notes:
//! - Uses Rust's std::path for cross-platform path handling
//! - Pathnames are represented as strings in OVSM
//! - Components are extracted using Path methods
//! - Supports both absolute and relative paths

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::path::{Path, PathBuf};

// ============================================================================
// PATHNAME CREATION (3 functions)
// ============================================================================

/// PATHNAME - Create pathname object (returns path string)
pub struct PathnameTool;

impl Tool for PathnameTool {
    fn name(&self) -> &str {
        "PATHNAME"
    }

    fn description(&self) -> &str {
        "Create pathname from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PATHNAME".to_string(),
                reason: "Expected pathname string".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        // Simply return the path string (validated by Path)
        let path = Path::new(path_str);
        Ok(Value::String(path.display().to_string()))
    }
}

/// MAKE-PATHNAME - Construct pathname from components
pub struct MakePathnameTool;

impl Tool for MakePathnameTool {
    fn name(&self) -> &str {
        "MAKE-PATHNAME"
    }

    fn description(&self) -> &str {
        "Construct pathname from directory, name, and type components"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Arguments: directory, name, type (extension)
        let mut path = PathBuf::new();

        if !args.is_empty() {
            // First arg is directory
            let dir = args[0].as_string()?;
            path.push(dir);
        }

        if args.len() > 1 {
            // Second arg is filename
            let name = args[1].as_string()?;

            if args.len() > 2 {
                // Third arg is extension
                let ext = args[2].as_string()?;
                let filename = format!("{}.{}", name, ext);
                path.push(filename);
            } else {
                path.push(name);
            }
        }

        Ok(Value::String(path.display().to_string()))
    }
}

/// PARSE-NAMESTRING - Parse string to pathname
pub struct ParseNamestringTool;

impl Tool for ParseNamestringTool {
    fn name(&self) -> &str {
        "PARSE-NAMESTRING"
    }

    fn description(&self) -> &str {
        "Parse string into pathname"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PARSE-NAMESTRING".to_string(),
                reason: "Expected string to parse".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        // Return normalized path string
        Ok(Value::String(path.display().to_string()))
    }
}

// ============================================================================
// PATHNAME COMPONENTS (6 functions)
// ============================================================================

/// PATHNAME-DIRECTORY - Get directory component
pub struct PathnameDirectoryTool;

impl Tool for PathnameDirectoryTool {
    fn name(&self) -> &str {
        "PATHNAME-DIRECTORY"
    }

    fn description(&self) -> &str {
        "Get directory component of pathname"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PATHNAME-DIRECTORY".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        match path.parent() {
            Some(parent) => Ok(Value::String(parent.display().to_string())),
            None => Ok(Value::Null),
        }
    }
}

/// PATHNAME-NAME - Get filename component (without extension)
pub struct PathnameNameTool;

impl Tool for PathnameNameTool {
    fn name(&self) -> &str {
        "PATHNAME-NAME"
    }

    fn description(&self) -> &str {
        "Get filename component without extension"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PATHNAME-NAME".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        match path.file_stem() {
            Some(name) => Ok(Value::String(name.to_string_lossy().to_string())),
            None => Ok(Value::Null),
        }
    }
}

/// PATHNAME-TYPE - Get file extension
pub struct PathnameTypeTool;

impl Tool for PathnameTypeTool {
    fn name(&self) -> &str {
        "PATHNAME-TYPE"
    }

    fn description(&self) -> &str {
        "Get file extension"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PATHNAME-TYPE".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        match path.extension() {
            Some(ext) => Ok(Value::String(ext.to_string_lossy().to_string())),
            None => Ok(Value::Null),
        }
    }
}

/// PATHNAME-DEVICE - Get device component (not applicable on Unix)
pub struct PathnameDeviceTool;

impl Tool for PathnameDeviceTool {
    fn name(&self) -> &str {
        "PATHNAME-DEVICE"
    }

    fn description(&self) -> &str {
        "Get device component (Windows drive letter)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PATHNAME-DEVICE".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        // On Windows, this would return the drive letter (C:, D:, etc.)
        // On Unix, always returns null
        #[cfg(target_os = "windows")]
        {
            let path_str = args[0].as_string()?;
            let path = Path::new(path_str);

            // Try to extract drive letter
            if let Some(prefix) = path.components().next() {
                use std::path::Component;
                if let Component::Prefix(prefix) = prefix {
                    return Ok(Value::String(
                        prefix.as_os_str().to_string_lossy().to_string(),
                    ));
                }
            }
        }

        Ok(Value::Null)
    }
}

/// PATHNAME-HOST - Get host component (not applicable in OVSM)
pub struct PathnameHostTool;

impl Tool for PathnameHostTool {
    fn name(&self) -> &str {
        "PATHNAME-HOST"
    }

    fn description(&self) -> &str {
        "Get host component (always null in OVSM)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // OVSM doesn't support network pathnames
        Ok(Value::Null)
    }
}

/// PATHNAME-VERSION - Get version component (not applicable in OVSM)
pub struct PathnameVersionTool;

impl Tool for PathnameVersionTool {
    fn name(&self) -> &str {
        "PATHNAME-VERSION"
    }

    fn description(&self) -> &str {
        "Get version component (always null in OVSM)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // OVSM doesn't support versioned pathnames
        Ok(Value::Null)
    }
}

// ============================================================================
// PATHNAME UTILITIES (6 functions)
// ============================================================================

/// MERGE-PATHNAMES - Merge pathname with defaults
pub struct MergePathnamesTool;

impl Tool for MergePathnamesTool {
    fn name(&self) -> &str {
        "MERGE-PATHNAMES"
    }

    fn description(&self) -> &str {
        "Merge pathname with default pathname"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MERGE-PATHNAMES".to_string(),
                reason: "Expected at least one pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        if args.len() > 1 {
            // Merge with default path
            let default_str = args[1].as_string()?;
            let default_path = Path::new(default_str);

            if path.is_relative() {
                // Join relative path with default
                let merged = default_path.join(path);
                Ok(Value::String(merged.display().to_string()))
            } else {
                // Absolute path, return as-is
                Ok(Value::String(path.display().to_string()))
            }
        } else {
            // No default, return path as-is
            Ok(Value::String(path.display().to_string()))
        }
    }
}

/// NAMESTRING - Convert pathname to string
pub struct NamestringTool;

impl Tool for NamestringTool {
    fn name(&self) -> &str {
        "NAMESTRING"
    }

    fn description(&self) -> &str {
        "Convert pathname to string representation"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NAMESTRING".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        Ok(Value::String(path.display().to_string()))
    }
}

/// DIRECTORY-NAMESTRING - Get directory as string
pub struct DirectoryNamestringTool;

impl Tool for DirectoryNamestringTool {
    fn name(&self) -> &str {
        "DIRECTORY-NAMESTRING"
    }

    fn description(&self) -> &str {
        "Get directory component as string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DIRECTORY-NAMESTRING".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        match path.parent() {
            Some(parent) => {
                let mut dir_str = parent.display().to_string();
                // Ensure trailing separator
                if !dir_str.is_empty() && !dir_str.ends_with('/') && !dir_str.ends_with('\\') {
                    dir_str.push('/');
                }
                Ok(Value::String(dir_str))
            }
            None => Ok(Value::String(String::new())),
        }
    }
}

/// FILE-NAMESTRING - Get filename as string
pub struct FileNamestringTool;

impl Tool for FileNamestringTool {
    fn name(&self) -> &str {
        "FILE-NAMESTRING"
    }

    fn description(&self) -> &str {
        "Get filename component as string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FILE-NAMESTRING".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        match path.file_name() {
            Some(name) => Ok(Value::String(name.to_string_lossy().to_string())),
            None => Ok(Value::String(String::new())),
        }
    }
}

/// ENOUGH-NAMESTRING - Get relative pathname
pub struct EnoughNamestringTool;

impl Tool for EnoughNamestringTool {
    fn name(&self) -> &str {
        "ENOUGH-NAMESTRING"
    }

    fn description(&self) -> &str {
        "Get relative pathname from base"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ENOUGH-NAMESTRING".to_string(),
                reason: "Expected pathname and base pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let base_str = args[1].as_string()?;

        let path = Path::new(path_str);
        let base = Path::new(base_str);

        // Try to get relative path from base to path
        match path.strip_prefix(base) {
            Ok(relative) => Ok(Value::String(relative.display().to_string())),
            Err(_) => {
                // If strip_prefix fails, return the original path
                Ok(Value::String(path.display().to_string()))
            }
        }
    }
}

/// TRUENAME - Get canonical pathname
pub struct TruenameTool;

impl Tool for TruenameTool {
    fn name(&self) -> &str {
        "TRUENAME"
    }

    fn description(&self) -> &str {
        "Get canonical pathname (resolves symlinks)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "TRUENAME".to_string(),
                reason: "Expected pathname".to_string(),
            });
        }

        let path_str = args[0].as_string()?;
        let path = Path::new(path_str);

        // Try to canonicalize (resolve symlinks and make absolute)
        match path.canonicalize() {
            Ok(canonical) => Ok(Value::String(canonical.display().to_string())),
            Err(_) => {
                // If canonicalize fails (file doesn't exist), return absolute path
                match std::env::current_dir() {
                    Ok(cwd) => {
                        let absolute = if path.is_absolute() {
                            path.to_path_buf()
                        } else {
                            cwd.join(path)
                        };
                        Ok(Value::String(absolute.display().to_string()))
                    }
                    Err(_) => Ok(Value::String(path.display().to_string())),
                }
            }
        }
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all pathname tools
pub fn register(registry: &mut ToolRegistry) {
    // Pathname creation
    registry.register(PathnameTool);
    registry.register(MakePathnameTool);
    registry.register(ParseNamestringTool);

    // Pathname components
    registry.register(PathnameDirectoryTool);
    registry.register(PathnameNameTool);
    registry.register(PathnameTypeTool);
    registry.register(PathnameDeviceTool);
    registry.register(PathnameHostTool);
    registry.register(PathnameVersionTool);

    // Pathname utilities
    registry.register(MergePathnamesTool);
    registry.register(NamestringTool);
    registry.register(DirectoryNamestringTool);
    registry.register(FileNamestringTool);
    registry.register(EnoughNamestringTool);
    registry.register(TruenameTool);
}
